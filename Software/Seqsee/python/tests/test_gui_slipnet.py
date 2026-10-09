"""Slipnet drawing (loop0002 item 007): seqsee/gui/draw/slipnet.py.

Mirrors lib/SGUI/Slipnet.pm (Setup with config/GUI_ws3.conf's [Layout] Margin and
[SlipnetLayout], DrawIt, DrawNode with Themes/Std2.pm's Style::NetActivation), reading
SLTM::GetTopConcepts (lib/SLTM.pm) through seqsee/gui/snapshot.py. Golden:
tests/golden/gui_slipnet.json from oracle/gui_slipnet.pl.
"""
import dataclasses
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import sltm
from seqsee.gui import snapshot
from seqsee.gui.draw import ops, slipnet
from seqsee.gui.draw.theme import Style

CASES = load("gui_slipnet")
ROOT = Path(__file__).resolve().parents[2]


def _id(case):
    return "{}-{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]))


def _snap_with(*concepts):
    """A snapshot of the empty workspace whose slipnet is the given ConceptSnaps."""
    gui_recipes.build("empty")
    return dataclasses.replace(snapshot.take(), slipnet=tuple(concepts))


def draw_or_partial(snap, rect):
    """The ops on the canvas after DrawIt: all of them, or those drawn before it died."""
    try:
        return slipnet.draw(snap, *rect), None
    except ops.DrawDied as e:
        return e.ops, str(e)


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_matches_perl_canvas(case):
    gui_recipes.build(case["recipe"])
    out, died = draw_or_partial(snapshot.take(), case["rect"])
    assert died == case["died"]
    assert_ops_match(out, case["items"])


@pytest.mark.parametrize("case", [c for c in CASES if c["rect"] == [0, 0, 780, 450]],
                         ids=_id)
def test_snapshot_concepts_match_perl(case):
    gui_recipes.build(case["recipe"])
    got = snapshot.take().slipnet
    assert [(c.text, c.perl_class) for c in got] == [(t, r) for t, _, _, r in case["concepts"]]
    for c, (_, act, raw, _) in zip(got, case["concepts"]):
        assert c.activation == pytest.approx(act, rel=1e-12)
        assert c.raw_activation == raw
        assert c.raw_significance is None          # GetTopConcepts gives 3 fields


def test_concept_without_as_text_dies_after_its_oval():
    # PERL-QUIRK: Mapping::Dir has no as_text; DrawNode dies after createOval. A hidden one
    # is never asked for its text.
    gui_recipes.build("slipnet_dir")
    snap = snapshot.take()
    with pytest.raises(ops.DrawDied) as err:
        slipnet.draw(snap, 0, 0, 780, 450)
    assert str(err.value) == 'Can\'t locate object method "as_text" via package "Mapping::Dir"'
    assert [type(o) for o in err.value.ops] == [ops.Oval, ops.Text, ops.Oval, ops.Text, ops.Oval]
    hidden = dataclasses.replace(snap, slipnet=snap.slipnet[:3])
    assert len(slipnet.draw(hidden, 0, 0, 780, 450)) == 4


def test_golden_covers_the_item_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "slipnet_small", "slipnet_full", "slipnet_long_text",
            "slipnet_dir"} <= recipes
    assert any(c["died"] for c in CASES) and any(not c["died"] for c in CASES)
    assert recipes <= set(gui_recipes.RECIPES)
    assert any(not c["items"] for c in CASES) and any(c["items"] for c in CASES)
    assert any(x or y for x, y, _, _ in (c["rect"] for c in CASES))
    full = [c for c in CASES if c["recipe"] == "slipnet_full" and c["rect"][2] == 780][0]
    assert len(full["items"]) == 62                  # 31 nodes drawn (the PERL-QUIRK below)
    texts = [i["opts"].get("text") for c in CASES for i in c["items"] if i["type"] == "text"]
    assert any(len(t) == 30 for t in texts)


def test_layout_matches_gui_ws3_conf():
    conf = (ROOT / "config" / "GUI_ws3.conf").read_text()
    section = re.search(r"\[SlipnetLayout\]\n(.*?)(\n\[|\Z)", conf, re.S).group(1)
    values = dict(re.findall(r"(\w+)\s*=\s*(\S+)", section))
    lay = slipnet.LAYOUT
    assert lay.margin == float(re.search(r"Margin\s*=\s*(\S+)", conf).group(1))
    assert lay.entries_per_column == int(values["EntriesPerColumn"])
    assert lay.column_count == int(values["ColumnCount"])
    assert lay.max_oval_radius == float(values["MaxOvalRadius"])
    assert lay.max_text_width == int(values["MaxTextWidth"])
    assert lay.min_activation_for_display == float(values["MinActivationForDisplay"])


def test_node_oval_and_text():
    snap = _snap_with(snapshot.ConceptSnap("ascending", 0.5, 40, None))
    oval, text = slipnet.draw(snap, 0, 0, 780, 450)
    # left = top = Margin 20; radius 7.5 around (20 + 2 + 15, 20 + 2 + 15)
    assert oval == ops.oval(29.5, 29.5, 44.5, 44.5, **Style.NetActivation(0))
    assert text == ops.text(56, 37, anchor="w", text="ascending")


def test_threshold_is_strict_and_skipped_nodes_take_no_row():
    snap = _snap_with(snapshot.ConceptSnap("a", 0.01, 2, None),
                      snapshot.ConceptSnap("b", 0.0100001, 2, None),
                      snapshot.ConceptSnap("c", None, 2, None),
                      snapshot.ConceptSnap("d", 0.2, 2, None))
    texts = [o for o in slipnet.draw(snap, 0, 0, 780, 450) if isinstance(o, ops.Text)]
    assert [(t.text, t.coords[1]) for t in texts] == [("b", 37), ("d", 78)]


def test_columns_and_the_extra_fourth_column():
    snap = _snap_with(*(snapshot.ConceptSnap(str(i), 0.5, 2, None) for i in range(40)))
    texts = [o for o in slipnet.draw(snap, 0, 0, 780, 450) if isinstance(o, ops.Text)]
    # PERL-QUIRK: DrawIt checks the column before moving to the next one, so a 31st node
    # is drawn in a fourth column, outside the rectangle, before the loop stops.
    assert len(texts) == 31
    col_width = int((780 - 40) / 3)
    assert [t.coords[0] for t in texts[::10]] == [56 + k * col_width for k in range(4)]
    assert [t.coords[1] for t in texts[:3]] == [37, 78, 119]       # RowHeight int(410/10)


def test_text_cut_to_max_text_width():
    long = "x" * 29 + "yz"
    snap = _snap_with(snapshot.ConceptSnap(long, 0.5, 2, None))
    assert slipnet.draw(snap, 0, 0, 780, 450)[1].text == long[:30]


def test_colour_from_raw_significance():
    snap = _snap_with(snapshot.ConceptSnap("a", 0.5, 2, 50.7))
    assert slipnet.draw(snap, 0, 0, 780, 450)[0].fill == Style.NetActivation(50)["fill"]


def test_negative_geometry_truncates_toward_zero():
    snap = _snap_with(*(snapshot.ConceptSnap(str(i), 0.5, 2, None) for i in range(12)))
    texts = [o for o in slipnet.draw(snap, 0, 0, 30, 30) if isinstance(o, ops.Text)]
    # EffectiveWidth -10: ColumnWidth int(-10/3) = -3; RowHeight int(-10/10) = -1
    assert texts[10].coords == (20 + 6 + 30 - 3, 20 + 2 + 15 + 0)
    assert texts[1].coords[1] == 20 + 2 + 15 - 1


def test_snapshot_slipnet_is_frozen_when_activations_change():
    gui_recipes.build("slipnet_small")
    snap = snapshot.take()
    before = snap.slipnet
    sltm.ACTIVATIONS[1][2] = 0.123
    sltm.clear()
    assert snap.slipnet == before
    assert snapshot.take().slipnet == ()
    hash(snap)


def test_draw_is_pure_and_repeatable():
    gui_recipes.build("slipnet_full")
    snap = snapshot.take()
    assert slipnet.draw(snap, 0, 0, 780, 450) == slipnet.draw(snap, 0, 0, 780, 450)
    assert snapshot.take() == snap

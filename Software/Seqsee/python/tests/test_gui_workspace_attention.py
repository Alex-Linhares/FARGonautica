"""Workspace attention drawing (loop0002 item 006): seqsee/gui/draw/workspace_attention.py.

Mirrors lib/SGUI/Workspace_Attention.pm (Setup with config/GUI_ws3.conf's
[Workspace_AttentionLayout], DrawIt, PrepareForDrawing and DrawBlackRectangle, the empty
DrawLegend, find_element_style / find_group_style / find_group_border_style /
find_relation_style with Themes/Std2.pm's Style::*Attention, Seqsee::Element::draw_attention,
Seqsee::Anchored::draw_attention, SReln::draw_attention, DrawMetonym, DrawBarLines,
DrawLastRunnable), with the attention values of SCoderack->AttentionDistribution
(lib/SCoderack.pm) taken by seqsee/gui/snapshot.py. Golden:
tests/golden/gui_workspace_attention.json from oracle/gui_workspace_attention.pl.
"""
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import scoderack
from seqsee.gui import snapshot
from seqsee.gui.draw import ops, workspace, workspace_attention
from seqsee.gui.draw.theme import Style

CASES = load("gui_workspace_attention")
ROOT = Path(__file__).resolve().parents[2]
DIED = 'Can\'t locate object method "draw_attention" via package "SRelation"'


def _id(case):
    return "{}-{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]))


def golden_measure(metrics):
    table = {(font, text): (w, ls) for font, text, w, ls in metrics}
    return lambda text, font: table[(font, text)]


def draw_or_partial(snap, rect, **kw):
    """The ops on the canvas after DrawIt: all of them, or those drawn before it died."""
    try:
        return workspace_attention.draw(snap, *rect, **kw), None
    except ops.DrawDied as e:
        return e.ops, str(e)


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_matches_perl_canvas(case):
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    out, died = draw_or_partial(snap, case["rect"], measure=golden_measure(case["metrics"]))
    assert died == case["died"]
    spans = [g.span for g in snap.groups]
    assert_ops_match(out, case["items"], ordered=len(set(spans)) == len(spans))


def _labels(snap):
    out = {}
    for e in snap.elements:
        out["e%d" % e.index] = e.attention
    for g in snap.groups:
        out["g" + g.bounds_string + g.structure_string] = g.attention
    for r in snap.relations:
        a, b = (snap.obj(oid) for oid in r.ends)
        out["r" + a.bounds_string + "->" + b.bounds_string] = r.attention
    return out


@pytest.mark.parametrize("case", [c for c in CASES if c["rect"] == [0, 0, 780, 450]],
                         ids=_id)
def test_snapshot_attention_matches_perl(case):
    gui_recipes.build(case["recipe"])
    got = _labels(snapshot.take())
    expected = {k: v for k, v in case["attention"] if not k.startswith("scalar:")}
    assert {k for k, v in got.items() if v} == set(expected)
    for k, v in expected.items():
        assert got[k] == pytest.approx(v, rel=1e-12, abs=1e-15), k


def test_golden_covers_the_item_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "six_elements", "hilit_debug", "groups_relations", "nested_groups",
            "metonyms", "attention_elements", "attention_groups",
            "attention_relations"} <= recipes
    assert recipes <= set(gui_recipes.RECIPES)
    assert any(c["died"] for c in CASES) and any(not c["died"] for c in CASES)
    assert any(c["attention"] for c in CASES)
    assert any(x or y for x, y, _, _ in (c["rect"] for c in CASES))


def test_layout_matches_gui_ws3_conf():
    conf = (ROOT / "config" / "GUI_ws3.conf").read_text()
    section = re.search(r"\[Workspace_AttentionLayout\]\n(.*?)(\n\[|\Z)", conf, re.S).group(1)
    values = dict(re.findall(r"(\w+)\s*=\s*(\S+)", section))
    lay = workspace_attention.LAYOUT
    assert lay.margin == float(re.search(r"Margin\s*=\s*(\S+)", conf).group(1))
    assert lay.elements_y_fraction == float(values["ElementsYFraction"])
    assert lay.min_gp_height_fraction == float(values["MinGpHeightFraction"])
    assert lay.max_gp_height_fraction == float(values["MaxGpHeightFraction"])
    assert lay.reln_zenith_fraction == float(values["RelnZenithFraction"])
    assert lay.meto_y_fraction == float(values["MetoYFraction"])
    assert lay.barline_height_fraction == float(values["BarlineHeightFraction"])


def test_snapshot_attention_defaults_to_zero():
    gui_recipes.build("groups_relations")      # no codelets
    snap = snapshot.take()
    assert all(o.attention == 0 for o in snap.elements + snap.groups + snap.relations)


def test_snapshot_attention_is_frozen_when_the_coderack_changes():
    gui_recipes.build("attention_elements")
    snap = snapshot.take()
    before = [e.attention for e in snap.elements]
    scoderack.clear()
    assert [e.attention for e in snap.elements] == before
    assert [e.attention for e in snapshot.take().elements] == [0] * 6


def test_black_rectangle_first_with_offsets():
    gui_recipes.build("empty")
    out = workspace_attention.draw(snapshot.take(), 50, 30, 600, 300)
    assert out[0] == ops.rectangle(70, 50, 630, 310, fill="#000000")


def test_element_style_is_attention_not_hilit():
    gui_recipes.build("attention_elements")
    snap = snapshot.take()
    out = workspace_attention.draw(snap, 0, 0, 780, 450)
    texts = [o for o in out if isinstance(o, ops.Text) and "element" in o.tags]
    assert [t.fill for t in texts] == [Style.ElementAttention(e.attention)["fill"]
                                      for e in snap.elements]
    assert snap.elements[1].hilit and texts[1].font == texts[0].font
    assert texts[0].tags == ("obj0", "element", "0")


def test_bar_lines_thin_and_label_raw():
    gui_recipes.build("attention_elements")
    out = workspace_attention.draw(snapshot.take(), 0, 0, 780, 450)
    bars = [o for o in out if isinstance(o, ops.Line)]
    assert len(bars) == 2 and all(b.width == 1 for b in bars)
    assert out[-1].text == "Seqsee::SCF::FocusOn" and out[-1].anchor == "sw"


def test_hilit_border_raised_and_group_fill_by_attention():
    gui_recipes.build("attention_groups")
    snap = snapshot.take()
    out = workspace_attention.draw(snap, 0, 0, 780, 450)
    ovals = [o for o in out if isinstance(o, ops.Oval)]
    assert [o.tags for o in ovals[-2:]] == [("hilit",), ("hilit",)]
    border = Style.GroupBorderAttention()["outline"]
    assert all(o.outline == border for o in ovals[-2:])
    fills = {o.fill for o in ovals if o.fill}
    assert {Style.GroupAttention(g.attention)["fill"] for g in snap.groups} <= fills


def test_relations_die_after_groups_and_elements():
    gui_recipes.build("attention_relations")
    with pytest.raises(ops.DrawDied, match=re.escape(DIED)) as info:
        workspace_attention.draw(snapshot.take(), 0, 0, 780, 450)
    kinds = {type(o) for o in info.value.ops}
    assert ops.Rectangle in kinds and ops.Text in kinds
    assert not any(isinstance(o, ops.Line) and o.arrow == "last" for o in info.value.ops)


def test_relations_without_the_perl_die():
    """``relations_die=False`` runs SReln::draw_attention's body instead: the relations are
    drawn (hidden between neighbours unless highlighted), with Style::RelationAttention."""
    gui_recipes.build("attention_relations")
    snap = snapshot.take()
    out = workspace_attention.draw(snap, 0, 0, 780, 450, relations_die=False)
    arcs = [o for o in out if isinstance(o, ops.Line) and o.arrow == "last"]
    # r_in (inside asc, not hilit) is hidden; r_out and r_hi are drawn.
    assert len(arcs) == 2
    by_attention = {Style.RelationAttention(r.attention)["fill"] for r in snap.relations}
    assert {a.fill for a in arcs} <= by_attention
    assert all(a.width == 4 and a.smooth and a.arrowshape == ops.Line().arrowshape
               for a in arcs)
    assert out[-1].anchor == "sw"


def test_draw_is_pure_and_repeatable():
    gui_recipes.build("attention_groups")
    snap = snapshot.take()
    assert workspace_attention.draw(snap, 0, 0, 780, 450) == \
        workspace_attention.draw(snap, 0, 0, 780, 450)
    assert snapshot.take() == snap

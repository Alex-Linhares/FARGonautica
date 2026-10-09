"""Coderack drawing (loop0002 item 008): seqsee/gui/draw/coderack.py.

Mirrors lib/SGUI/Coderack.pm (family_to_name, Setup with config/GUI_ws3.conf's [Layout]
Margin and [CoderackLayout], DrawIt), reading @SCoderack::CODELETS, $SCoderack::URGENCIES_SUM
and %SCoderack::HistoryOfRunnable (lib/SCoderack.pm) through seqsee/gui/snapshot.py.
Golden: tests/golden/gui_coderack.json from oracle/gui_coderack.pl.
"""
import dataclasses
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import scoderack
from seqsee.gui import snapshot
from seqsee.gui.draw import coderack, ops

CASES = load("gui_coderack")
ROOT = Path(__file__).resolve().parents[2]
HEADERS = ["NAME", "#", "Urgeny %", "% OF ALL RUN"]


def _id(case):
    return "{}-{}{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]),
                            "-redraw" if case["redraw"] else "")


def _build(case):
    """The case's model state, and the families DrawIt had added to the history before."""
    gui_recipes.build(case["recipe"])
    known = ()
    if case["redraw"]:
        known = coderack.rack_families(snapshot.take())
        scoderack.CODELETS.clear()
        scoderack.URGENCIES_SUM = 0
        gui_recipes._codelet("Mid", 10)
    return snapshot.take(), known


def _in_perl_order(snap, case):
    """The snapshot with its history in the order Perl's `each` walked it (hash order)."""
    return dataclasses.replace(snap, coderack=dataclasses.replace(
        snap.coderack, history=tuple((k, n) for k, n in case["drawn"])))


def _snap_with(codelets=(), urgencies_sum=0, history=()):
    gui_recipes.build("empty")
    return dataclasses.replace(snapshot.take(), coderack=snapshot.CoderackSnap(
        tuple(codelets), urgencies_sum, tuple(history)))


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_matches_perl_canvas(case):
    snap, known = _build(case)
    assert case["died"] is None
    assert_ops_match(coderack.draw(_in_perl_order(snap, case), *case["rect"]), case["items"])


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_snapshot_coderack_matches_perl(case):
    snap, known = _build(case)
    cr = snap.coderack
    assert [list(c) for c in cr.codelets] == case["codelets"]
    assert cr.urgencies_sum == case["urgencies_sum"]
    before = dict(cr.history)
    before.update({k: 0 for k in known if k not in before})
    assert sorted(before.items()) == [tuple(h) for h in case["history"]]
    # The rows Python draws (history, then the remembered and the rack families with 0) are
    # Perl's rows; only their order is Perl's hash order.
    assert dict(coderack.rows(cr, known)) == dict(map(tuple, case["drawn"]))


def test_golden_covers_the_item_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "coderack_small", "coderack_zero_urgency", "coderack_history_only",
            "coderack_many"} <= recipes <= set(gui_recipes.RECIPES)
    assert any(c["redraw"] for c in CASES)
    assert any(x or y for x, y, _, _ in (c["rect"] for c in CASES))
    many = [c for c in CASES if c["recipe"] == "coderack_many" and c["rect"][2] == 780][0]
    assert len(many["drawn"]) == 55
    assert len(many["items"]) == 8 + 48 * 7 + 12     # 3 columns of 16 rows, 12 stripes


def test_layout_matches_gui_ws3_conf():
    conf = (ROOT / "config" / "GUI_ws3.conf").read_text()
    section = re.search(r"\[CoderackLayout\]\n(.*?)(\n\[|\Z)", conf, re.S).group(1)
    values = dict(re.findall(r"(\w+)\s*=\s*(\S+)", section))
    lay = coderack.LAYOUT
    assert lay.margin == float(re.search(r"Margin\s*=\s*(\S+)", conf).group(1))
    assert lay.max_columns == int(values["MaxColumns"])
    assert lay.max_rows == int(values["MaxRows"])
    assert lay.name_offset == float(values["NameOffset"])
    assert lay.count_offset == float(values["CountOffset"])
    assert lay.urgency_offset == float(values["UrgencyOffset"])
    assert lay.historical_fraction_offset == float(values["HistoricalFractionOffset"])


def test_headers_are_above_the_rectangle():
    # PERL-QUIRK: the headers are at YOffset - 10, outside (above) the view's rectangle.
    out = coderack.draw(_snap_with(), 50, 30, 600, 300)
    assert [(o.text, o.coords, o.anchor) for o in out] == [
        (t, (50 + 20 + col * 280 + dx, 20), "nw")
        for col in (0, 1) for t, dx in zip(HEADERS, (0, 200, 220, 290))]


def test_one_row():
    snap = _snap_with([("Reader", 30), ("Reader", 10), ("FocusOn", 60)], 100,
                      [("Seqsee::SCF::Reader", 3), ("Seqsee::SCF::FocusOn", 1)])
    out = coderack.draw(snap, 0, 0, 780, 450)
    stripe, rows = out[0], out[9:]
    # RowHeight int(410/15) = 27; the bars are 0.8 of it.
    assert rows[:7] == [
        ops.text(20, 20, anchor="nw"),                      # family_to_name: undef
        ops.text(220, 20, anchor="nw", text="2"),
        ops.rectangle(240, 20, 240 + 40 * 0.5, 20 + 0.8 * 27, fill="#0000FF"),
        ops.rectangle(289, 20, 290, 20 + 0.8 * 27),
        ops.rectangle(310, 20, 310 + 50 * 3 / 4, 20 + 0.8 * 27, fill="#FF0000"),
        ops.text(370, 20, anchor="nw", text="3"),
        ops.rectangle(359, 20, 360, 20 + 0.8 * 27),
    ]
    # PERL-QUIRK: the stripe ignores the Margin (y from YOffset + 2 rows - 3).
    assert stripe == ops.rectangle(20, 2 * 27 - 3, 20 + 370, 4 * 27 - 3, fill="#CCFFDD",
                                   outline="")
    assert stripe.outline is None


def test_zero_urgency_sum_and_no_history():
    # URGENCIES_SUM 0: the sums are '---' (0 as a number); no run so far: no red bar.
    snap = _snap_with([("Reader", 0)], 0, [])
    out = coderack.draw(snap, 0, 0, 780, 450)
    assert not [o for o in out if getattr(o, "fill", None) == "#FF0000"]
    blue = [o for o in out if getattr(o, "fill", None) == "#0000FF"]
    assert [b.coords[0] == b.coords[2] for b in blue] == [True]
    assert ops.text(370, 20, anchor="nw", text="0") in out


def test_rows_and_columns_overflow():
    # PERL-QUIRK: `$rows_displayed > $MaxRows` gives 16 rows per column, and
    # `$current_column > $MaxColumns` draws a third column (outside the rectangle).
    history = [("Seqsee::SCF::H%02d" % i, i + 1) for i in range(60)]
    out = coderack.draw(_snap_with(history=history), 0, 0, 780, 450)
    counts = [o for o in out if isinstance(o, ops.Text) and o.coords[0] in (370, 740, 1110)]
    assert len(counts) == 48
    assert [c.text for c in counts] == [str(i + 1) for i in range(48)]
    assert [c.coords[0] for c in counts[::16]] == [370, 740, 1110]
    assert [c.coords[1] for c in counts[15:17]] == [20 + 15 * 27, 20]


def test_stripes_are_lowered_in_reverse_order():
    history = [("Seqsee::SCF::H%02d" % i, 1) for i in range(9)]
    bottom, top = coderack.draw_layers(_snap_with(history=history), 0, 0, 780, 450)
    assert [o.coords[1] for o in bottom] == [8 * 27 + 2 * 27 - 3, 4 * 27 + 2 * 27 - 3, 51]
    assert coderack.draw(_snap_with(history=history), 0, 0, 780, 450) == bottom + top
    assert all(o.fill == "#CCFFDD" for o in bottom)


def test_rows_add_rack_and_known_families_with_zero():
    snap = _snap_with([("A", 1), ("B", 2), ("A", 3)], 6, [("Seqsee::SCF::C", 4)])
    assert coderack.rack_families(snap) == ("Seqsee::SCF::A", "Seqsee::SCF::B")
    assert coderack.rows(snap.coderack, ["Seqsee::SCF::D", "Seqsee::SCF::C"]) == [
        ("Seqsee::SCF::C", 4), ("Seqsee::SCF::D", 0), ("Seqsee::SCF::A", 0),
        ("Seqsee::SCF::B", 0)]


def test_draw_never_writes_the_model_history():
    # Perl's DrawIt writes $HistoryOfRunnable{$_} ||= 0; the port must not.
    gui_recipes.build("coderack_small")
    before = dict(scoderack.HistoryOfRunnable)
    snap = snapshot.take()
    coderack.draw(snap, 0, 0, 780, 450)
    assert scoderack.HistoryOfRunnable == before
    assert snapshot.take() == snap


def test_snapshot_coderack_is_frozen():
    gui_recipes.build("coderack_small")
    snap = snapshot.take()
    before = snap.coderack
    scoderack.HistoryOfRunnable["Seqsee::SCF::Reader"] += 1
    scoderack.CODELETS[0].urgency = 99
    scoderack.clear()
    assert snap.coderack == before
    assert snapshot.take().coderack == snapshot.CoderackSnap()
    hash(snap)


def test_draw_is_pure_and_repeatable():
    gui_recipes.build("coderack_many")
    snap = snapshot.take()
    assert coderack.draw(snap, 0, 0, 780, 450) == coderack.draw(snap, 0, 0, 780, 450)

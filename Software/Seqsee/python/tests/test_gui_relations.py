"""Relations pane drawing (loop0002 item 010): seqsee/gui/draw/relations.py.

Mirrors lib/SGUI/Relations.pm (Setup with config/GUI_ws3.conf's [Layout] Margin and
[RelationsLayout], DrawIt, DrawRelation), reading ``values %SWorkspace::relations``
(lib/SWorkspace.pm) and each relation's get_ends / get_strength / get_type (lib/SRelation.pm,
lib/Mapping/Numeric.pm, lib/Mapping/Structural.pm) and the ends' get_bounds_string
(lib/Seqsee/Anchored.pm) through seqsee/gui/snapshot.py.
Golden: tests/golden/gui_relations.json from oracle/gui_relations.pl.
"""
import dataclasses
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import sworkspace
from seqsee.gui import snapshot
from seqsee.gui.draw import ops, relations

CASES = load("gui_relations")
ROOT = Path(__file__).resolve().parents[2]
FONT_9 = "-adobe-helvetica-bold-r-normal--9-140-100-100-p-105-iso8859-4"


def _id(case):
    return "{}-{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]))


def _row_key(r):
    """What identifies a Perl row: (ends' bounds, type text, class)."""
    return (tuple(r["ends"]), r["type"], r["class"])


def _snap_row(rel):
    return {"ends": list(rel.end_bounds), "strength": rel.strength,
            "complexity": rel.complexity, "type": rel.type_text, "class": rel.perl_class,
            "isa_numeric": int(rel.isa_numeric), "isa_structural": int(rel.isa_structural)}


def _in_perl_order(snap, perl_rows):
    """The snapshot with its relations reordered to Perl's hash-order walk."""
    pool = list(snap.relations)
    ordered = []
    for row in perl_rows:
        i = next(i for i, rel in enumerate(pool) if _row_key(_snap_row(rel)) == _row_key(row))
        ordered.append(pool.pop(i))
    assert not pool, "relations Perl didn't draw"
    return dataclasses.replace(snap, relations=tuple(ordered))


def _rows_equal(actual, expected):
    assert actual.keys() == expected.keys()
    for k in actual:
        if k in ("strength", "complexity"):
            assert actual[k] == pytest.approx(expected[k], rel=1e-12, abs=1e-12), k
        else:
            assert actual[k] == expected[k], k


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_golden(case):
    gui_recipes.build(case["recipe"])
    snap = _in_perl_order(snapshot.take(), case["relations"])
    assert case["died"] is None
    out = relations.draw(snap, *case["rect"])
    assert_ops_match(out, case["items"])


@pytest.mark.parametrize("case", [c for c in CASES if c["rect"] == [0, 0, 780, 450]],
                         ids=_id)
def test_snapshot_matches_perl(case):
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    actual = sorted((_snap_row(r) for r in snap.relations), key=lambda r: _row_key(r))
    expected = sorted(case["relations"], key=_row_key)
    assert len(actual) == len(expected)
    for a, e in zip(actual, expected):
        _rows_equal(a, e)
    assert case["compound"] == sum(r.isa_numeric for r in snap.relations) == 0


def test_golden_covers_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "relations_pane", "relations_many", "overlapping_relations"} <= recipes
    rects = {tuple(c["rect"]) for c in CASES}
    assert (0, 0, 30, 30) in rects and (50, 30, 600, 300) in rects
    # Some case has more relations than RowCount rows; one has a structural relation.
    assert max(len(c["relations"]) for c in CASES) > relations.LAYOUT.row_count
    assert any(r["class"] == "SRelation::Structural" for c in CASES for r in c["relations"])


def test_layout_matches_config():
    text = (ROOT / "config" / "GUI_ws3.conf").read_text()
    section = re.search(r"\[RelationsLayout\]\n(.*?)(?:\n\[|\Z)", text, re.S).group(1)
    conf = dict(re.findall(r"^(\w+)\s*=\s*(\S+)", section, re.M))
    margin = re.search(r"\[Layout\][^\[]*?Margin\s*=\s*(\S+)", text).group(1)
    lay = relations.LAYOUT
    assert lay.margin == float(margin)
    assert lay.row_count == int(conf["RowCount"])
    assert lay.strength_offset == float(conf["StrengthOffset"])
    assert lay.simplicity_offset == float(conf["SimplicityOffset"])
    assert lay.end1_offset == float(conf["End1Offset"])
    assert lay.end2_offset == float(conf["End2Offset"])
    assert lay.type_offset == float(conf["TypeOffset"])
    assert lay.font == conf["Font"] == FONT_9


def _rel(oid, strength=0.5, complexity=0.1, ends=(" <0, 0> ", " <1, 1> "), text="succ",
         isa_numeric=False, isa_structural=False):
    return snapshot.RelationSnap(oid=oid, ends=(0, 1), strength=strength, type_text=text,
                                 complexity=complexity, perl_class="SRelation", hilit=0,
                                 end_bounds=ends, isa_numeric=isa_numeric,
                                 isa_structural=isa_structural)


def _snap(*rels):
    gui_recipes.build("empty")
    return dataclasses.replace(snapshot.take(), relations=tuple(rels))


def test_one_row_geometry():
    out = relations.draw(_snap(_rel(7, 1.234, 0.4, (" <2, 4> ", " <5, 5> "), "pred")),
                         10, 100, 300, 150)
    stripe, *texts = out
    m, rh = 20, (150 - 40) / 22
    assert isinstance(stripe, ops.Rectangle)
    assert stripe.coords == (30, 130, 290, 130 + rh)
    assert (stripe.fill, stripe.outline) == ("#CCFFDD", None)
    assert [(t.coords, t.text) for t in texts] == [
        ((30, 130), " 1.23"), ((80, 130), " 0.40"), ((130, 130), " <2, 4> "),
        ((200, 130), " <5, 5> "), ((290, 130), "pred")]
    assert all(t.anchor == "nw" and t.font == FONT_9 for t in texts)
    assert m == relations.LAYOUT.margin


def test_stripes_every_other_row_lowered():
    rels = [_rel(i, strength=i) for i in range(5)]
    lowered, rest = relations.draw_layers(_snap(*rels), 0, 0, 780, 450)
    rh = 410 / 22
    # Rows 0, 2, 4 get a stripe; each is lowered below everything, so the last is first.
    assert [s.coords[1] for s in lowered] == pytest.approx([30 + 4 * rh, 30 + 2 * rh, 30])
    assert len(rest) == 25 and all(isinstance(t, ops.Text) for t in rest)
    assert relations.draw(_snap(*rels), 0, 0, 780, 450) == lowered + rest


def test_rows_overflow_the_rectangle():
    """PERL-QUIRK: no row limit; rows past RowCount go below the rectangle."""
    rels = [_rel(i) for i in range(30)]
    out = relations.draw(_snap(*rels), 0, 0, 780, 450)
    last = [o for o in out if isinstance(o, ops.Text)][-1]
    assert last.coords[1] == pytest.approx(30 + 29 * 410 / 22)
    assert last.coords[1] > 450


def test_strength_format():
    assert relations.fmt(0) == " 0.00"
    assert relations.fmt(5.555) == "%5.2f" % 5.555
    assert relations.fmt(123456.789) == "123456.79"
    assert relations.fmt(-3.14159) == "-3.14"
    assert relations.fmt(None) == " 0.00"          # sprintf of undef
    assert relations.fmt("2.5") == " 2.50"


def test_compound_first_and_structural_dropped():
    """DrawIt draws Mapping::Numeric relations first, then all but Mapping::Structural ones
    (never the case for SRelations: both lists are kept for fidelity)."""
    a = _rel(1, text="a")
    n = _rel(2, text="n", isa_numeric=True)
    s = _rel(3, text="s", isa_structural=True)
    rows = relations.rows(_snap(a, n, s))
    assert [r.type_text for r in rows] == ["n", "a", "n"]


def test_no_relations_draws_nothing():
    assert relations.draw(_snap(), 0, 0, 780, 450) == []


def test_snapshot_end_bounds_outside_workspace():
    names = gui_recipes.build("relations_pane")
    snap = snapshot.take()
    bounds = {r.end_bounds for r in snap.relations}
    assert (" <7, 7> ", names["r"][6].get_ends()[1].get_bounds_string()) in bounds
    assert snap.obj(next(r for r in snap.relations
                         if r.end_bounds[0] == " <7, 7> ").ends[1]) is None
    assert {r.perl_class for r in snap.relations} == {"SRelation", "SRelation::Structural"}


def test_draw_does_not_touch_model():
    gui_recipes.build("relations_pane")
    before = [(id(r), r.get_strength()) for r in sworkspace.relations.values()]
    snap = snapshot.take()
    relations.draw(snap, 0, 0, 780, 450)
    assert before == [(id(r), r.get_strength()) for r in sworkspace.relations.values()]


def test_snapshot_frozen():
    gui_recipes.build("relations_pane")
    snap = snapshot.take()
    rel = snap.relations[0]
    with pytest.raises(dataclasses.FrozenInstanceError):
        rel.end_bounds = ()
    hash(snap)


def test_draw_is_pure_and_repeatable():
    gui_recipes.build("relations_many")
    snap = snapshot.take()
    assert relations.draw(snap, 0, 0, 780, 450) == relations.draw(snap, 0, 0, 780, 450)

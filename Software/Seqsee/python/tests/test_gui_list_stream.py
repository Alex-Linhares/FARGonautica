"""Stream list drawing (loop0002 item 012): seqsee/gui/draw/lists/stream.py.

Mirrors lib/SGUI/List/Stream.pm (new's fields; GetItemList: $Global::MainStream's
CurrentThought and OlderThoughts sorted by thought_hit_intensity with rnkeysort; DrawOneItem:
the intensity, as_text, the stored_fringe sorted by activation, each component's as_text if
it can, else "$component") on top of lib/SGUI/List.pm (including Clear on a page redraw),
reading lib/SStream2.pm and lib/SThought.pm through seqsee/gui/snapshot.py.
Golden: tests/golden/gui_list_stream.json from oracle/gui_list_stream.pl.
"""
import dataclasses
import re

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee.gui import snapshot
from seqsee.gui.draw import lists, ops
from seqsee.gui.draw.lists import stream

CASES = load("gui_list_stream")
NAME = "SGUI::List::Stream"
_REF = re.compile(r"=(?:HASH|SCALAR|ARRAY|CODE)\((?:ADDR|0x[0-9a-f]+)\)")


def _norm(text):
    return None if text is None else _REF.sub("=REF", text)


def _norm_ops(out):
    return [dataclasses.replace(o, text=_norm(o.text)) if isinstance(o, ops.Text) else o
            for o in out]


def _norm_items(items):
    return [dict(i, opts=dict(i["opts"], text=_norm(i["opts"]["text"])))
            if "text" in i["opts"] else i for i in items]


def _id(case):
    return "{}-{}-p{}{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]),
                                case["page"], "-redraw" if case["redraw"] else "")


def draw_or_partial(snap, rect, page=0):
    try:
        lowered, out, state = stream.draw_layers(snap, *rect, page=page)
        return lowered + out, None, state
    except ops.DrawDied as e:
        return e.ops, str(e), None


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_golden(case):
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    if case["redraw"]:
        # Drawn on page 0, then the page-up square: PageNumber + 1, Clear, DrawIt. Clear
        # deletes the items tagged with the list; the texts of fringe-less rows have no tags,
        # so they stay (PERL-QUIRK). The new bars are lowered below them.
        lowered, out, state = stream.draw_layers(snap, *case["rect"], page=0)
        left = lists.survivors(lowered + out, NAME)
        assert left
        lowered, out, state = stream.draw_layers(snap, *case["rect"],
                                                 page=state.page_number + 1)
        result, died = lowered + left + out, None
    else:
        result, died, state = draw_or_partial(snap, case["rect"], case["page"])
    assert died == case["died"]
    assert_ops_match(_norm_ops(result), _norm_items(case["items"]))
    if died is None:
        assert state.page_number == case["page_after"]
        assert state.shown_from == case["shown_from"]
        assert state.shown_to == case["shown_to"]
        assert state.entries_count == case["entries_count"]


def _thought_as_perl(t):
    if t is None:
        return None
    return {"text": _norm(t.text), "hit": t.hit_intensity,
            "fringe": None if t.fringe is None else [
                [_norm(c.label), c.activation] for c in t.fringe]}


def _perl_thought(t):
    if t is None:
        return None
    return dict(t, text=_norm(t["text"]), fringe=None if t["fringe"] is None else [
        [_norm(c[0]), c[1]] for c in t["fringe"]])


@pytest.mark.parametrize("case", [c for c in CASES if c["rect"] == [0, 0, 780, 450]
                                  and c["page"] == 0 and not c["redraw"]], ids=_id)
def test_snapshot_and_order_match_perl(case):
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    assert _thought_as_perl(snap.stream.current) == _perl_thought(case["stream"]["current"])
    assert [_thought_as_perl(t) for t in snap.stream.older] == [
        _perl_thought(t) for t in case["stream"]["older"]]
    assert [stream.tag_of(e) for e in stream.item_list(snap)] == case["order"]


def test_golden_covers_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "stream_small", "stream_full", "stream_list_many", "stream_hole"} <= recipes
    assert {c["died"] for c in CASES} >= {
        None, "Illegal division by zero",
        "Can't call method \"as_text\" without a package or object reference"}
    assert any(c["redraw"] for c in CASES)
    assert any(c["page_after"] != c["page"] for c in CASES)
    assert any(i["tags"] == [] for c in CASES for i in c["items"])        # untagged texts


def test_layout_matches_perl_source():
    lay = stream.LAYOUT
    assert (lay.height_per_row, lay.intensity_x, lay.name_x, lay.fringe_y, lay.fringe_x) == \
        (40, 5, 100, 20, 10)


# --- drawing ---------------------------------------------------------------------------------

def _thought(text="T", hit=None, fringe=None):
    if fringe is not None:
        fringe = tuple(None if c is None else snapshot.ComponentSnap(c[0], c[1], None, c[0])
                       for c in fringe)
    return snapshot.ThoughtSnap(text, hit, fringe)


def _snap(current, *older):
    gui_recipes.build("empty")
    return dataclasses.replace(snapshot.take(),
                               stream=snapshot.StreamSnap(current, tuple(older)))


def test_no_current_thought_no_entries():
    snap = _snap(None, _thought(hit=5))
    assert stream.item_list(snap) == ()
    _, out, st = stream.draw_layers(snap, 0, 0, 780, 450)
    assert out[0].text == "No entries to show." and st.entries_count == 0


def test_older_sorted_by_hit_stably():
    snap = _snap(_thought("cur", hit=1000), _thought("a"), _thought("b", hit=3),
                 _thought("c", hit=0), _thought("d", hit=3), None, _thought("e", hit=-1))
    entries = stream.item_list(snap)
    assert [stream.tag_of(e) for e in entries] == [
        "tht0", "tht2", "tht4", "tht1", "tht3", "", "tht6"]


def test_one_row_geometry_and_texts():
    snap = _snap(_thought("Current", hit=9, fringe=[("a", 1), ("b", 3), ("c", 2.5)]),
                 _thought("Old", hit=2.5, fringe=[]))
    lowered, out, _ = stream.draw_layers(snap, 10, 100, 400, 200)
    assert [b.coords for b in lowered] == [(30, 160, 390, 200), (30, 120, 390, 160)]
    hit, name, fringe, hit2, name2, fringe2 = out[:6]
    assert [(t.coords, t.text) for t in (hit, name, fringe)] == [
        ((35, 125), "-"), ((130, 125), "Current"), ((40, 140), "[3] b; [2.5] c; [1] a; ")]
    assert [(t.coords, t.text) for t in (hit2, name2, fringe2)] == [
        ((35, 165), "2.5"), ((130, 165), "Old"), ((40, 180), "")]
    assert all(t.anchor == "nw" and t.font == ops.DEFAULT_FONT for t in out[:6])
    assert set(hit.tags) == {NAME, "tht0", NAME + "-Clickable-Item"}
    assert set(fringe2.tags) == {NAME, "tht1", NAME + "-Clickable-Item"}


def test_fringeless_row_texts_are_untagged():
    """PERL-QUIRK: `my $fringe = $thought->stored_fringe() or return;` returns from
    DrawOneItem with no item ids, so its two texts get no tags (and survive Clear)."""
    snap = _snap(_thought("Current", fringe=None))
    lowered, out, _ = stream.draw_layers(snap, 0, 0, 780, 450)
    hit, name = out[:2]
    assert (hit.text, name.text) == ("-", "Current")
    assert hit.tags == () and name.tags == ()
    assert set(lowered[0].tags) == {NAME, "tht0", NAME + "-Clickable-Item"}
    assert lists.survivors(lowered + out, NAME) == [hit, name]


def test_undef_hit_and_activation():
    snap = _snap(_thought("C", fringe=[]), _thought("Old", hit=None, fringe=[("x", None)]))
    _, out, _ = stream.draw_layers(snap, 0, 0, 780, 450)
    assert out[3].text == "" and out[5].text == "[] x; "


def test_hole_dies_after_its_bar():
    snap = _snap(_thought("C", fringe=[]), None)
    with pytest.raises(ops.DrawDied, match='method "as_text" without a package') as e:
        stream.draw_layers(snap, 0, 0, 780, 450)
    assert sum(isinstance(o, ops.Rectangle) for o in e.value.ops) == 2
    assert len(e.value.ops) == 5


def test_component_labels():
    """as_text where Perl's UNIVERSAL::can finds it (categories, platonics, mappings,
    elements, SInt), else the stringified value."""
    gui_recipes.build("stream_small")
    cur = snapshot.take().stream.current
    assert [c.label for c in cur.fringe] == ["absolute_position_3", "4", "SInt(5)", "ascending"]

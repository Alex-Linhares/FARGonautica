"""Stream drawing (loop0002 item 009): seqsee/gui/draw/stream.py.

Mirrors lib/SGUI/Stream.pm (Setup with config/GUI_ws3.conf's [Layout] Margin and
[StreamLayout], DrawIt, DrawThought) and the Style::ThoughtBox / ThoughtHead /
ThoughtComponent functions of lib/Themes/Std2.pm, reading $Global::MainStream
(lib/SStream2.pm: CurrentThought, OlderThoughts, thought_hit_intensity, hit_intensity) and each
thought's as_text and stored_fringe (lib/SThought.pm, lib/SThought/*.pm) through
seqsee/gui/snapshot.py.
Golden: tests/golden/gui_stream.json from oracle/gui_stream.pl.
"""
import dataclasses
import re
from pathlib import Path

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import global_ as Global
from seqsee import s as S
from seqsee.gui import snapshot
from seqsee.gui.draw import ops, stream
from seqsee.gui.draw.theme import Style
from seqsee.sint import SInt

CASES = load("gui_stream")
ROOT = Path(__file__).resolve().parents[2]
FONT_14 = "-adobe-helvetica-bold-r-normal--14-140-100-100-p-105-iso8859-4"
FONT_10 = "-adobe-helvetica-bold-r-normal--10-140-100-100-p-105-iso8859-4"

# Perl draws most fringe components as stringified refs. The oracle replaces the address with
# ADDR; the port gives "Class=HASH(0x<id>)" (it doesn't know Perl's reftype: Class::Std
# objects such as SLTM::Platonic are SCALAR refs). Both sides keep only the class.
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
    return "{}-{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]))


def _thought_as_perl(t):
    if t is None:
        return None
    return {"text": _norm(t.text), "hit": t.hit_intensity,
            "fringe": None if t.fringe is None else [
                [_norm(c.text), c.activation, c.hit_intensity] for c in t.fringe]}


def _perl_thought(t):
    if t is None:
        return None
    return dict(t, text=_norm(t["text"]), fringe=None if t["fringe"] is None else [
        [_norm(c[0]), c[1], c[2]] for c in t["fringe"]])


def _snap_with(current=None, *older):
    gui_recipes.build("empty")
    return dataclasses.replace(snapshot.take(),
                               stream=snapshot.StreamSnap(current, tuple(older)))


def _thought(text="T", hit=None, fringe=None):
    if fringe is not None:
        fringe = tuple(None if c is None else snapshot.ComponentSnap(*c) for c in fringe)
    return snapshot.ThoughtSnap(text, hit, fringe)


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_matches_perl_canvas(case):
    gui_recipes.build(case["recipe"])
    assert case["died"] is None
    out = stream.draw(snapshot.take(), *case["rect"])
    assert_ops_match(_norm_ops(out), _norm_items(case["items"]))


@pytest.mark.parametrize("case", [c for c in CASES if c["rect"] == [0, 0, 780, 450]],
                         ids=_id)
def test_snapshot_stream_matches_perl(case):
    gui_recipes.build(case["recipe"])
    st = snapshot.take().stream
    assert _thought_as_perl(st.current) == _perl_thought(case["stream"]["current"])
    assert [_thought_as_perl(t) for t in st.older] == [
        _perl_thought(t) for t in case["stream"]["older"]]


def test_golden_covers_the_item_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "stream_current", "stream_small", "stream_full",
            "stream_real"} <= recipes <= set(gui_recipes.RECIPES)
    assert any(x or y for x, y, _, _ in (c["rect"] for c in CASES))
    full = [c for c in CASES if c["recipe"] == "stream_full" and c["rect"][2] == 780][0]
    assert None in full["stream"]["older"]
    # 12 thoughts: a box and a head each, plus the components (at most 3 per thought).
    assert len(full["items"]) == 12 * 2 + sum(min(i % 5, 3) for i in range(12))


def test_layout_matches_gui_ws3_conf():
    conf = (ROOT / "config" / "GUI_ws3.conf").read_text()
    section = re.search(r"\[StreamLayout\]\n(.*?)(\n\[|\Z)", conf, re.S).group(1)
    values = dict(re.findall(r"(\w+)\s*=\s*(\S+)", section))
    lay = stream.LAYOUT
    assert lay.margin == float(re.search(r"Margin\s*=\s*(\S+)", conf).group(1))
    assert lay.entries_per_column == int(values["EntriesPerColumn"])
    assert lay.column_count == int(values["ColumnCount"])


def test_one_thought():
    t = _thought("Element 3", 100, [("absolute_position_3", 80, 80), ("4", 100, None),
                                    ("SInt(5)", 30, None), ("hidden", 50, None)])
    out = stream.draw(_snap_with(t), 50, 30, 600, 300)
    # ColumnWidth int(560/4) = 140, RowHeight int(260/3) = 86; the current thought is at
    # (XOffset + Margin/2, YOffset), outside the margin.
    assert out == [
        ops.rectangle(60, 30, 200, 116, width=3, fill="#87E187"),
        ops.text(61, 31, anchor="nw", text="Element 3", font=FONT_14),
        ops.text(70, 45, anchor="nw", text="absolute_position_3", fill="#3314CC",
                 font=FONT_10),
        ops.text(70, 60, anchor="nw", text="4", fill="#3314CC", font=FONT_10),
        ops.text(70, 75, anchor="nw", text="SInt(5)", fill="#3314CC", font=FONT_10),
    ]


def test_older_thought_positions():
    # Row 0 of column 0 is left to the current thought: the older ones start at row 1.
    older = [_thought("t%d" % i) for i in range(12)]
    out = stream.draw(_snap_with(None, *older), 0, 0, 780, 450)
    boxes = [o.coords[:2] for o in out if isinstance(o, ops.Rectangle)]
    cells = [(0, 1), (0, 2)] + [(c, r) for c in (1, 2, 3) for r in (0, 1, 2)] + [(4, 0)]
    assert boxes == [(20 + c * 185, 20 + r * 136) for c, r in cells]


def test_false_older_entries_take_no_row():
    out = stream.draw(_snap_with(None, None, _thought("a"), None, _thought("b")), 0, 0, 780, 450)
    assert [o.coords[:2] for o in out if isinstance(o, ops.Rectangle)] == [(20, 156), (20, 292)]


def test_thought_box_colour():
    # thought_hit_intensity: undef is 0 (v 90), clamped at 2000 (v 50); current is hue 120.
    out = stream.draw(_snap_with(_thought(hit=None), _thought(hit=None), _thought(hit=1000),
                                 _thought(hit=2000), _thought(hit=99999)), 0, 0, 780, 450)
    boxes = [o for o in out if isinstance(o, ops.Rectangle)]
    assert [(b.fill, b.width) for b in boxes] == [
        (Style.ThoughtBox(0, 1)["fill"], 3), (Style.ThoughtBox(0, 0)["fill"], 1),
        (Style.ThoughtBox(1000, 0)["fill"], 1), (Style.ThoughtBox(2000, 0)["fill"], 1),
        (Style.ThoughtBox(2000, 0)["fill"], 1)]
    assert Style.ThoughtBox(None, 0) == Style.ThoughtBox(0, 0)


def test_fringe_cases():
    # stored_fringe undef: no components; []: none; a false entry ends the list.
    for fringe, texts in ((None, []), ((), []), ((("a", 1, None), None, ("b", 2, None)), ["a"])):
        out = stream.draw(_snap_with(_thought(fringe=fringe)), 0, 0, 780, 450)
        assert [o.text for o in out[2:]] == texts


def test_empty_texts_are_tk_defaults():
    out = stream.draw(_snap_with(_thought("", fringe=[("", 1, None)])), 0, 0, 780, 450)
    assert [o.text for o in out[1:]] == ["", ""]


def test_component_text_is_perl_stringification():
    gui_recipes.build("stream_small")
    st = snapshot.take().stream
    texts = [c.text for c in st.current.fringe]
    assert texts[:3] == ["absolute_position_3", "4", "SInt(5)"]
    assert _norm(texts[3]) == "SCategory::Ascending=REF"
    assert [c.text for c in st.older[3].fringe] == ["0.5", "x"]
    assert snapshot.perl_string(SInt(7)) == "SInt(7)"
    assert snapshot.perl_string(None) == ""
    assert snapshot.perl_string(2.0) == "2"


def test_hit_intensities_in_the_snapshot():
    gui_recipes.build("stream_small")
    st = snapshot.take().stream
    assert [c.hit_intensity for c in st.current.fringe] == [80, 100, None, None]
    assert [t.hit_intensity for t in (st.current,) + st.older] == [100, 500, 2500, None, 0]


def test_snapshot_stream_is_frozen():
    built = gui_recipes.build("stream_small")
    snap = snapshot.take()
    before = snap.stream
    built["cur"].stored_fringe().append(["new", 1])
    Global.MainStream.thought_hit_intensity[built["cur"]] = 7
    Global.MainStream.older_thoughts.pop()
    Global.MainStream.clear()
    assert snap.stream == before
    assert snapshot.take().stream == snapshot.StreamSnap()
    hash(snap)


def test_draw_does_not_touch_the_model():
    built = gui_recipes.build("stream_real")
    snap = snapshot.take()
    stream.draw(snap, 0, 0, 780, 450)
    assert snapshot.take() == snap
    assert Global.MainStream.current_thought.core() is built["asc"]


def test_draw_is_pure_and_repeatable():
    gui_recipes.build("stream_full")
    snap = snapshot.take()
    assert stream.draw(snap, 0, 0, 780, 450) == stream.draw(snap, 0, 0, 780, 450)

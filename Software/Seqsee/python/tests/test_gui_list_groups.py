"""List base + Groups list drawing (loop0002 item 011): seqsee/gui/draw/lists/.

Mirrors lib/SGUI/List.pm (Setup with config/GUI_ws3.conf's [Layout] Margin, DrawIt,
DrawBookKeeping, GetEntriesOnCurrentPage, Clear, the page-up / page-down bindings) and
lib/SGUI/List/Groups.pm (new's column offsets, font and HeightPerRow; GetItemList =
SWorkspace->GetGroups (lib/SWorkspace.pm); DrawOneItem: get_is_locked_against_deletion,
get_strength, get_bounds_string (lib/Seqsee/Anchored.pm), get_categories_as_string
(lib/Categorizable.pm)) through seqsee/gui/snapshot.py.
Golden: tests/golden/gui_list_groups.json from oracle/gui_list_groups.pl.
"""
import dataclasses
import re

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import sworkspace
from seqsee.gui import snapshot
from seqsee.gui.draw import lists, ops
from seqsee.gui.draw.lists import groups

CASES = load("gui_list_groups")
FONT_9 = "-adobe-helvetica-bold-r-normal--9-140-100-100-p-105-iso8859-4"
NAME = "SGUI::List::Groups"
REF = re.compile(r"=(HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)")


def _id(case):
    return "{}-{}-p{}{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]),
                                case["page"], "-redraw" if case["redraw"] else "")


def _cats(text):
    """A categories string without addresses, in a fixed order (Perl's is hash order)."""
    return ", ".join(sorted(REF.sub("=REF", text).split(", "))) if text else text


def _row_key(bounds, categories, locked, strength):
    return (bounds, _cats(categories), bool(locked), round(float(strength), 9))


def _perl_key(row):
    return _row_key(row["bounds"], row["categories"], row["locked"], row["strength"])


def _snap_key(g):
    return _row_key(g.bounds_string, g.categories_as_string, g.is_locked, g.strength)


def _in_perl_order(snap, perl_groups):
    """The snapshot with its groups in Perl's GetGroups order, and Perl's group tags mapped
    to the snapshot's obj<oid> tags."""
    pool = list(snap.groups)
    ordered, tag_map = [], {}
    for row in perl_groups:
        i = next(i for i, g in enumerate(pool) if _snap_key(g) == _perl_key(row))
        g = pool.pop(i)
        ordered.append(g)
        tag_map[row["tag"]] = "obj%d" % g.oid
    assert not pool, "groups Perl didn't list"
    return dataclasses.replace(snap, groups=tuple(ordered)), tag_map


def _normalise_ops(out):
    """Op texts without addresses (categories strings)."""
    return [dataclasses.replace(o, text=_cats(o.text)) if isinstance(o, ops.Text) else o
            for o in out]


def _normalise_items(items, tag_map):
    norm = []
    for item in items:
        item = dict(item, tags=[tag_map.get(t, t) for t in item["tags"]])
        if item["type"] == "text" and "text" in item["opts"]:
            item["opts"] = dict(item["opts"], text=_cats(item["opts"]["text"]))
        norm.append(item)
    return norm


def draw_or_partial(snap, rect, page=0):
    try:
        lowered, out, state = groups.draw_layers(snap, *rect, page=page)
        return lowered + out, None, state
    except ops.DrawDied as e:
        return e.ops, str(e), None


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_golden(case):
    gui_recipes.build(case["recipe"])
    snap, tag_map = _in_perl_order(snapshot.take(), case["groups"])
    if case["redraw"]:
        # Drawn on page 0, then the page-up square: PageNumber + 1, Clear, DrawIt.
        _, _, state = groups.draw_layers(snap, *case["rect"], page=0)
        page = state.page_number + 1
    else:
        page = case["page"]
    out, died, state = draw_or_partial(snap, case["rect"], page)
    assert died == case["died"]
    assert_ops_match(_normalise_ops(out), _normalise_items(case["items"], tag_map))
    if died is None:
        assert state.page_number == case["page_after"]
        assert state.shown_from == case["shown_from"]
        assert state.shown_to == case["shown_to"]
        assert state.entries_count == case["entries_count"]


@pytest.mark.parametrize("case", [c for c in CASES if c["rect"] == [0, 0, 780, 450]
                                  and c["page"] == 0 and not c["redraw"]], ids=_id)
def test_snapshot_matches_perl(case):
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    assert sorted(_snap_key(g) for g in snap.groups) == sorted(
        _perl_key(r) for r in case["groups"])


def test_golden_covers_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "groups_list", "groups_many", "large", "nested_groups"} <= recipes
    assert any(c["died"] for c in CASES) and any(c["redraw"] for c in CASES)
    assert any(c["page_after"] != c["page"] for c in CASES)        # clamped to the last page
    assert any(c["page_after"] < 0 for c in CASES)                 # negative EntriesPerPage
    assert any(r["locked"] for c in CASES for r in c["groups"])
    assert any(", " in r["categories"] for c in CASES for r in c["groups"])


def test_layout_matches_perl_source():
    lay = groups.LAYOUT
    assert (lay.lock_x, lay.strength_x, lay.ends_x, lay.categories_x) == (-8, 0, 50, 140)
    assert lay.font == FONT_9 and lay.height_per_row == 15
    assert lists.MARGIN == 20


# --- GetEntriesOnCurrentPage ---------------------------------------------------------------

def test_entries_per_page_truncates_towards_zero():
    assert lists.entries_per_page(450, 15) == 27
    assert lists.entries_per_page(200, 15) == 10
    assert lists.entries_per_page(55, 15) == 1
    assert lists.entries_per_page(40, 15) == 0
    assert lists.entries_per_page(30, 15) == 0     # -10/15 → 0, not -1
    assert lists.entries_per_page(10, 15) == -2    # -30/15


def test_paging_normal_and_clamped():
    entries = list("abcdefghij")
    st = lists.current_page(entries, 4, 1)
    assert (st.page_number, st.shown_from, st.shown_to, st.entries_count) == (1, 4, 7, 10)
    assert st.entries == tuple("efgh")
    st = lists.current_page(entries, 4, 2)
    assert (st.shown_from, st.shown_to, st.entries) == (8, 9, ("i", "j"))
    st = lists.current_page(entries, 4, 7)          # past the last page: the last page
    assert (st.page_number, st.shown_from, st.shown_to) == (2, 8, 9)


def test_paging_no_entries():
    st = lists.current_page([], 4, 0)
    assert (st.page_number, st.shown_from, st.shown_to, st.entries) == (0, "--", "--", ())
    assert lists.current_page([], 4, 3).page_number == 3   # PageNumber is left alone


def test_paging_zero_per_page_dies():
    with pytest.raises(ops.DrawDied, match="^Illegal division by zero$") as e:
        lists.current_page(["a"], 0, 0)
    assert e.value.ops == []


def test_paging_negative_per_page_quirk():
    """PERL-QUIRK: a rectangle under 2 Margin + HeightPerRow high gives a negative page size;
    Perl's int() and negative slice bounds then pick odd pages."""
    st = lists.current_page(list(range(30)), -2, 0)
    assert (st.page_number, st.shown_from, st.shown_to, st.entries) == (-14, 28, 29, (28, 29))
    st = lists.current_page(list(range(3)), -2, 0)
    assert (st.page_number, st.shown_from, st.entries) == (0, "--", ())
    st = lists.current_page([], -2, 0)
    assert (st.shown_from, st.shown_to, st.entries) == (0, -3, ())


def test_perl_slice():
    assert lists.perl_slice([1, 2, 3], 0, 1) == [1, 2]
    assert lists.perl_slice([1, 2, 3], 2, 0) == []
    assert lists.perl_slice([1, 2, 3], -2, -1) == [2, 3]
    assert lists.perl_slice([1, 2, 3], 2, 4) == [3, None, None]


def test_bookkeeping_texts():
    assert lists.bookkeeping_text(lists.ListState(0, 0, 9, 30, ())) == \
        "Page #1. Entries 1-10 of 30 Shown."
    assert lists.bookkeeping_text(lists.ListState(0, "--", "--", 0, ())) == \
        "No entries to show."
    # '--' numifies to 0 (PERL-QUIRK: a list with entries but an empty page).
    assert lists.bookkeeping_text(lists.ListState(0, "--", "--", 3, ())) == \
        "Page #1. Entries 1-1 of 3 Shown."


# --- drawing ---------------------------------------------------------------------------------

def _gsnap(**kw):
    base = dict(oid=5, is_element=False, index=None, mag=None, left=0, right=2, span=3,
                items=(), strength=12.345, group_p=False, metonym_active=False,
                structure_string="", starred_structure_string=None, categories=(),
                category_kind=None, categories_as_string="", is_locked=False,
                bounds_string=" <0, 2> ", hilit=0)
    base.update(kw)
    return snapshot.ObjectSnap(**base)


def _snap(*gs):
    gui_recipes.build("empty")
    return dataclasses.replace(snapshot.take(), groups=tuple(gs))


def test_one_row_geometry_and_tags():
    snap = _snap(_gsnap(is_locked=True, categories_as_string="X=HASH(0x1)"))
    lowered, out, st = groups.draw_layers(snap, 10, 100, 300, 150)
    bar, = lowered
    assert bar.coords == (30, 120, 290, 135)
    assert (bar.fill, bar.outline) == ("#FFFFDD", None)
    tags = {NAME, "obj5", NAME + "-Clickable-Item"}
    assert set(bar.tags) == tags
    lock, strength, ends, cats, book, down, up = out
    assert [(t.coords, t.text) for t in (lock, strength, ends, cats)] == [
        ((22, 120), "L"), ((30, 120), "12.35"), ((80, 120), " <0, 2> "),
        ((170, 120), "X=HASH(0x1)")]
    assert all(t.anchor == "nw" and t.font == FONT_9 and set(t.tags) == tags
               for t in (lock, strength, ends, cats))
    assert (book.coords, book.text, book.anchor, book.font) == (
        (30, 230), "Page #1. Entries 1-1 of 1 Shown.", "nw", ops.DEFAULT_FONT)
    assert set(book.tags) == {NAME}
    assert down.coords == (290, 120, 300, 130) and set(down.tags) == {NAME, NAME + "-pagedown"}
    assert up.coords == (290, 230, 300, 240) and set(up.tags) == {NAME, NAME + "-pageup"}
    assert down.fill == up.fill == "#FF0000"
    assert st.entries == (snap.groups[0],)


def test_bars_alternate_and_are_lowered():
    snap = _snap(*(_gsnap(oid=i) for i in range(4)))
    lowered, out, _ = groups.draw_layers(snap, 0, 0, 780, 450)
    # Each bar is lowered below the whole canvas, so the last one drawn is at the bottom.
    assert [b.coords[1] for b in lowered] == [65, 50, 35, 20]
    assert [b.fill for b in lowered] == ["#FFDDCC", "#FFFFDD", "#FFDDCC", "#FFFFDD"]
    assert groups.draw(snap, 0, 0, 780, 450) == lowered + out


def test_empty_and_unset_texts():
    snap = _snap(_gsnap(categories_as_string="", bounds_string=""))
    _, out, _ = groups.draw_layers(snap, 0, 0, 780, 450)
    strength, ends, cats = out[:3]
    assert ends.text == "" and cats.text == ""
    assert strength.text == "12.35"


def test_undef_strength_is_zero():
    _, out, _ = groups.draw_layers(_snap(_gsnap(strength=None)), 0, 0, 780, 450)
    assert out[0].text == " 0.00"


def test_undef_entry_dies_after_its_bar():
    """A slice past the end gives undef entries (only reachable through odd page sizes);
    DrawOneItem then dies calling a method on undef, after the bar is drawn."""
    def one(left, top, entry):
        return groups.draw_one(left, top, entry)
    with pytest.raises(ops.DrawDied, match="get_is_locked_against_deletion") as e:
        lists.draw_list([None], one, 0, 0, 780, 450, NAME, 15)
    assert len(e.value.ops) == 1 and isinstance(e.value.ops[0], ops.Rectangle)


def test_page_state_round_trip():
    gui_recipes.build("groups_many")
    snap = snapshot.take()
    _, _, st = groups.draw_layers(snap, 0, 0, 400, 200, page=2)
    assert (st.page_number, st.shown_from, st.shown_to) == (2, 20, 29)
    assert st.entries == snap.groups[20:30]
    _, _, st = groups.draw_layers(snap, 0, 0, 400, 200, page=st.page_number + 1)
    assert st.page_number == 2                    # clamped back to the last page


def test_draw_does_not_touch_model():
    gui_recipes.build("groups_list")
    before = [(g.get_strength(), g.get_is_locked_against_deletion())
              for g in sworkspace.get_groups()]
    snap = snapshot.take()
    assert groups.draw(snap, 0, 0, 780, 450) == groups.draw(snap, 0, 0, 780, 450)
    assert before == [(g.get_strength(), g.get_is_locked_against_deletion())
                      for g in sworkspace.get_groups()]

"""Categories list drawing (loop0002 item 012): seqsee/gui/draw/lists/categories.py.

Mirrors lib/SGUI/List/Categories.pm (new's fields; PrepareForDrawing; GetItemList: the
groups sorted by span with rikeysort (lib/SWorkspace.pm GetGroups), then the elements,
get_edges and get_categories (lib/Categorizable.pm) into %Cat2Objects; DrawOneItem: get_name,
the image rectangle, instance ovals and element squares, $SWorkspace::ElementCount;
MarkDescendentsToKeep) on top of lib/SGUI/List.pm, through seqsee/gui/snapshot.py.
Golden: tests/golden/gui_list_categories.json from oracle/gui_list_categories.pl.
"""
import dataclasses

import pytest

import gui_recipes
from golden import load
from gui_compare import assert_ops_match
from seqsee import sworkspace
from seqsee.gui import snapshot
from seqsee.gui.draw import lists, ops
from seqsee.gui.draw.lists import categories

CASES = load("gui_list_categories")
NAME = "SGUI::List::Categories"


def _id(case):
    return "{}-{}-p{}{}".format(case["recipe"], "x".join(str(v) for v in case["rect"]),
                                case["page"], "-redraw" if case["redraw"] else "")


def _snap_group_key(snap, g):
    names = sorted(snap.category(cid).name for cid in g.category_ids)
    return (g.bounds_string, tuple(names))


def _in_perl_order(snap, case):
    """The snapshot with its groups in the GetGroups order Perl's GetItemList used and its
    categories in GetItemList's order; Perl's cat<K> tags mapped to cat<cid>."""
    pool = list(snap.groups)
    groups = []
    for row in case["groups"]:
        key = (row["bounds"], tuple(row["categories"]))
        i = next(i for i, g in enumerate(pool) if _snap_group_key(snap, g) == key)
        groups.append(pool.pop(i))
    assert not pool, "groups Perl didn't list"
    by_name = {}
    for c in snap.categories:
        assert c.name not in by_name, "recipes must not share category names"
        by_name[c.name] = c
    cats = [by_name[row["name"]] for row in case["categories"]]
    tag_map = {row["tag"]: "cat%d" % by_name[row["name"]].cid for row in case["categories"]}
    return dataclasses.replace(snap, groups=tuple(groups), categories=tuple(cats)), tag_map


def _items(items, tag_map):
    return [dict(i, tags=[tag_map.get(t, t) for t in i["tags"]]) for i in items]


def draw_or_partial(snap, rect, page=0):
    try:
        lowered, out, state = categories.draw_layers(snap, *rect, page=page)
        return lowered + out, None, state
    except ops.DrawDied as e:
        return e.ops, str(e), None


@pytest.mark.parametrize("case", CASES, ids=_id)
def test_golden(case):
    gui_recipes.build(case["recipe"])
    snap, tag_map = _in_perl_order(snapshot.take(), case)
    if case["redraw"]:
        # Drawn on page 0, then the page-up square: PageNumber + 1, Clear, DrawIt. Every
        # item is tagged with the list, so Clear leaves nothing.
        lowered, out, state = categories.draw_layers(snap, *case["rect"], page=0)
        assert lists.survivors(lowered + out, NAME) == []
        page = state.page_number + 1
    else:
        page = case["page"]
    out, died, state = draw_or_partial(snap, case["rect"], page)
    assert died == case["died"]
    assert_ops_match(out, _items(case["items"], tag_map))
    if died is None:
        assert state.page_number == case["page_after"]
        assert state.shown_from == case["shown_from"]
        assert state.shown_to == case["shown_to"]
        assert state.entries_count == case["entries_count"]


@pytest.mark.parametrize("case", [c for c in CASES if c["rect"] == [0, 0, 780, 450]
                                  and c["page"] == 0 and not c["redraw"]], ids=_id)
def test_item_list_matches_perl(case):
    """GetItemList: the same categories, each with the same instances (edges), as Perl;
    in Perl's order once the snapshot's groups and categories are in Perl's order."""
    gui_recipes.build(case["recipe"])
    snap = snapshot.take()
    assert snap.element_count == case["element_count"]
    mine = {e.category.name: sorted(e.edges) for e in categories.item_list(snap)}
    assert mine == {r["name"]: sorted(tuple(x) for x in r["edges"])
                    for r in case["categories"]}
    snap, _ = _in_perl_order(snap, case)
    assert [(e.category.name, list(map(list, e.edges))) for e in categories.item_list(snap)] \
        == [(r["name"], r["edges"]) for r in case["categories"]]


def test_golden_covers_states():
    recipes = {c["recipe"] for c in CASES}
    assert {"empty", "six_elements", "groups_list", "categories_many", "large"} <= recipes
    assert any(c["died"] for c in CASES) and any(c["redraw"] for c in CASES)
    assert any(c["page_after"] != c["page"] for c in CASES)
    assert any(len(c["categories"]) > 10 for c in CASES)
    # Equal spans: Perl's hash order differs between cases, which is why each case records it.
    many = [tuple(r["name"] for r in c["categories"]) for c in CASES
            if c["recipe"] == "categories_many"]
    assert len(set(many)) > 1


def test_layout_matches_perl_source():
    lay = categories.LAYOUT
    assert (lay.height_per_row, lay.height_for_image, lay.width_for_image,
            lay.max_gp_height, lay.min_gp_height) == (40, 35, 300, 15, 5)


# --- snapshot ----------------------------------------------------------------------------------

def test_snapshot_category_identities():
    gui_recipes.build("groups_list")
    snap = snapshot.take()
    names = [c.name for c in snap.categories]
    # Groups first (GetGroups order: D (no category) then equal spans), then the elements.
    assert sorted(names) == ["ascending", "even", "number", "sameness"]
    assert names[-1] == "number"
    assert [c.cid for c in snap.categories] == list(range(4))
    asc = next(c for c in snap.categories if c.name == "ascending")
    assert sum(asc.cid in g.category_ids for g in snap.groups) == 2
    assert all(e.category_ids == (snap.categories[-1].cid,) for e in snap.elements)
    for o in snap.groups + snap.elements:
        assert tuple(snap.category(cid).name for cid in o.category_ids) == o.categories


def test_snapshot_category_names_from_the_model():
    gui_recipes.build("categories_many")
    snap = snapshot.take()
    assert {c.name for c in snap.categories} == {
        "ascending", "descending", "mountain", "sameness", "Prime", "odd", "even", "number",
        *("Interlaced_%d" % n for n in range(2, 9))}


# --- drawing ---------------------------------------------------------------------------------

def _obj(oid, left, right, cids, is_element=False):
    return snapshot.ObjectSnap(
        oid=oid, is_element=is_element, index=left if is_element else None, mag=1, left=left,
        right=right, span=right - left + 1, items=(), strength=0, group_p=False,
        metonym_active=False, structure_string="", starred_structure_string=None,
        categories=(), category_kind=None, categories_as_string="", is_locked=False,
        bounds_string="", hilit=0, category_ids=tuple(cids))


def _snap(groups=(), elements=(), cats=(), element_count=None):
    gui_recipes.build("empty")
    return dataclasses.replace(
        snapshot.take(), groups=tuple(groups), elements=tuple(elements),
        categories=tuple(snapshot.CategorySnap(i, n) for i, n in enumerate(cats)),
        element_count=len(elements) if element_count is None else element_count)


def test_one_row_geometry():
    """Three elements: SpacePerElement 300/4 = 75, GroupHtPerUnitSpan (15 - 5)/3."""
    els = [_obj(i, i, i, [1], is_element=True) for i in range(3)]
    snap = _snap([_obj(3, 0, 1, [0])], els, ["cat-a", "num"])
    lowered, out, st = categories.draw_layers(snap, 10, 100, 400, 150)
    assert [e.category.name for e in st.entries] == ["cat-a", "num"]
    assert [b.coords for b in lowered] == [(30, 160, 390, 200), (30, 120, 390, 160)]
    name, image, oval, *squares = out[:6]
    assert (name.coords, name.text, name.anchor) == ((335, 137.5), "cat-a", "w")
    assert (image.coords, image.fill, image.outline) == ((30, 120, 330, 155), "#EEEEEE", "black")
    ht = 5 + 2 * 10 / 3
    assert oval.coords == pytest.approx((30 + 75 * 0.8, 137.5 - ht, 30 + 75 * 2.2, 137.5 + ht))
    assert oval.fill == "#0000FF"
    assert [s.coords for s in out[3:6]] == pytest.approx(
        [(104, 136.5, 106, 138.5), (179, 136.5, 181, 138.5), (254, 136.5, 256, 138.5)])
    tags = {NAME, "cat0", NAME + "-Clickable-Item"}
    assert all(set(o.tags) == tags for o in out[:6])
    # The number row: three element ovals.
    assert sum(isinstance(o, ops.Oval) for o in out[6:12]) == 3


def test_no_elements():
    """ElementCount 0: GroupHtPerUnitSpan divides by 1, SpacePerElement is the whole width,
    and no squares are drawn."""
    snap = _snap([_obj(0, 0, 1, [0])], [], ["c"], element_count=0)
    _, out, _ = categories.draw_layers(snap, 0, 0, 780, 450)
    name, image, oval = out[:3]
    assert oval.coords == pytest.approx((20 + 300 * 0.8, 37.5 - 25, 20 + 300 * 2.2, 37.5 + 25))
    assert isinstance(out[3], ops.Text)            # the bookkeeping text comes next


def test_groups_sorted_by_span_stably():
    snap = _snap([_obj(5, 0, 0, [0]), _obj(6, 2, 5, [0]), _obj(7, 1, 1, [0])],
                 [_obj(0, 0, 0, [0], is_element=True)], ["c"])
    entry, = categories.item_list(snap)
    assert entry.edges == ((2, 5), (0, 0), (1, 1), (0, 0))


def test_categories_without_instances_are_not_listed():
    snap = _snap([_obj(5, 0, 1, [1])], [], ["unused", "used"])
    assert [e.category.name for e in categories.item_list(snap)] == ["used"]


def test_undef_category_dies_after_its_bar():
    """An unregistered category is undef in get_categories; its row dies on get_name."""
    snap = _snap([_obj(5, 0, 1, [0])], [], [None])
    with pytest.raises(ops.DrawDied, match='method "get_name" on an undefined value') as e:
        categories.draw_layers(snap, 0, 0, 780, 450)
    assert len(e.value.ops) == 1 and isinstance(e.value.ops[0], ops.Rectangle)


def test_empty_name_leaves_default_text():
    snap = _snap([_obj(5, 0, 1, [0])], [], [""])
    _, out, _ = categories.draw_layers(snap, 0, 0, 780, 450)
    assert out[0].text == ""


def test_draw_does_not_touch_model():
    gui_recipes.build("groups_list")
    before = [(g.get_strength(), len(g.get_categories())) for g in sworkspace.get_groups()]
    snap = snapshot.take()
    assert categories.draw(snap, 0, 0, 780, 450) == categories.draw(snap, 0, 0, 780, 450)
    assert before == [(g.get_strength(), len(g.get_categories()))
                      for g in sworkspace.get_groups()]


# --- MarkDescendentsToKeep ---------------------------------------------------------------------

def test_mark_descendents_to_keep():
    """The group, its subgroups at every depth, but no elements."""
    r = gui_recipes.build("nested_groups")
    snap = snapshot.take()
    oid = {g.bounds_string: g.oid for g in snap.groups}
    s2 = next(g for g in snap.groups if g.span == 9)
    keep = categories.mark_descendents_to_keep(snap, s2.oid)
    assert keep == {g.oid for g in snap.groups}
    b = snap.obj(oid[r["b"].get_bounds_string()])
    assert categories.mark_descendents_to_keep(snap, b.oid) == {b.oid}
    assert categories.mark_descendents_to_keep(snap, snap.elements[0].oid) == set()


def test_delete_all_other_keeps():
    """DeleteAllOther's %Keep: the groups of the category and their descendents."""
    gui_recipes.build("groups_list")
    snap = snapshot.take()
    asc = next(c for c in snap.categories if c.name == "ascending")
    keep = categories.groups_to_keep(snap, asc.cid)
    assert {snap.obj(o).bounds_string for o in keep} == {" <0, 2> ", " <3, 5> "}
    same = next(c for c in snap.categories if c.name == "sameness")
    assert {snap.obj(o).bounds_string for o in categories.groups_to_keep(snap, same.cid)} \
        == {" <6, 8> "}

"""Snapshot I (loop0002 item 001): seqsee/gui/snapshot.py, the immutable plain-data copy of
the workspace that the drawing code reads instead of live model objects.

Mirrors the model reads of lib/SGUI/Workspace.pm and lib/SGUI/Workspace_Attention.pm
(SWorkspace::ElementCount, GetElements, GetGroups, %SWorkspace::relations, GetBarLines,
get_mag/get_edges/get_span/get_strength/get_metonym_activeness/get_structure_string/
GetEffectiveObject/is_of_category_p, get_ends, %Global::Hilit, $Global::Feature{debug},
$Global::CurrentRunnableString), plus the group/relation fields read by lib/SGUI/Relations.pm,
lib/SGUI/List/Groups.pm and lib/SGUI/List/Categories.pm, and $Global::Steps_Finished
(lib/Tk/SCodeletCount.pm). No golden file: the snapshot is checked against the live Python
model, which loop0001 verified against Perl.
"""
import dataclasses
import time

import pytest

import gui_recipes
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sworkspace, util
from seqsee.gui import snapshot


def take(name):
    objs = gui_recipes.build(name)
    ids = {}
    snap = snapshot.take(object_ids=ids)
    return objs, snap, ids


def _walk(value, path="snap"):
    """Yield (path, value) for every value reachable from a snapshot."""
    yield path, value
    if dataclasses.is_dataclass(value) and not isinstance(value, type):
        for f in dataclasses.fields(value):
            yield from _walk(getattr(value, f.name), f"{path}.{f.name}")
    elif isinstance(value, tuple):
        for i, v in enumerate(value):
            yield from _walk(v, f"{path}[{i}]")


# --- matches the live state ------------------------------------------------------------------
def test_empty_workspace():
    _, snap, _ = take("empty")
    assert snap.element_count == 0
    assert snap.elements == () and snap.groups == () and snap.relations == ()
    assert snap.bar_lines == ()
    assert snap.largest_group is None
    assert snap.steps == 0
    assert snap.current_runnable == ""
    assert snap.debug is False


def test_elements_match_live_state():
    objs, snap, ids = take("six_elements")
    live = sworkspace.get_elements()
    assert snap.element_count == sworkspace.ElementCount == 6
    assert [s.mag for s in snap.elements] == [e.get_mag() for e in live] == [1, 1, 2, 1, 2, 3]
    for i, (s, e) in enumerate(zip(snap.elements, live)):
        assert s.is_element and s.index == i
        assert s.oid == ids[id(e)]
        assert (s.left, s.right) == e.get_edges() == (i, i)
        assert s.span == e.get_span() == 1
        assert s.items == (s.oid,)               # an element's @$self is (itself)
        assert s.strength == e.get_strength()
        assert s.group_p is False and s.metonym_active is False
        assert s.structure_string == util.perl_str(e.get_structure_string())
        assert s.categories == ("number",)
        assert s.bounds_string == e.get_bounds_string()
        assert s.hilit == 0
        assert snap.obj(s.oid) is s


def test_groups_match_live_state():
    o, snap, ids = take("groups_relations")
    live = sworkspace.get_groups()
    assert [g.oid for g in snap.groups] == [ids[id(g)] for g in live]
    assert snap.largest_group is snap.groups[0]
    assert snap.largest_group.oid == ids[id(o["big"])]
    for s, g in zip(snap.groups, live):
        assert not s.is_element and s.index is None and s.mag is None
        assert (s.left, s.right) == g.get_edges()
        assert s.span == g.get_span()
        assert s.items == tuple(ids[id(x)] for x in g)
        assert s.strength == g.get_strength()
        assert s.is_locked == bool(g.get_is_locked_against_deletion())
        assert s.bounds_string == g.get_bounds_string()
        assert s.categories_as_string == g.get_categories_as_string()
        assert s.categories == tuple(c.get_name() for c in g.get_categories())
    asc, same, big = (snap.obj(ids[id(o[k])]) for k in ("asc", "same", "big"))
    assert (asc.left, asc.right, asc.span) == (0, 2, 3)
    assert (big.left, big.right, big.span) == (0, 5, 6)
    assert big.items == (asc.oid, same.oid)
    assert asc.category_kind == "ascending"
    assert same.category_kind == "sameness"
    assert big.category_kind is None
    assert asc.hilit == 1 and same.hilit == 0


def test_metonym_fields():
    o, snap, ids = take("groups_relations")
    same = snap.obj(ids[id(o["same"])])
    assert same.metonym_active is True
    assert same.structure_string == o["same"].get_structure_string()
    assert same.starred_structure_string == \
        util.perl_str(o["same"].get_effective_object().get_structure_string()) == "2"
    asc = snap.obj(ids[id(o["asc"])])
    assert asc.metonym_active is False and asc.starred_structure_string is None


def test_relations_match_live_state():
    o, snap, ids = take("groups_relations")
    live = list(sworkspace.relations.values())
    assert [r.oid for r in snap.relations] == [ids[id(r)] for r in live]
    for s, r in zip(snap.relations, live):
        f, sec = r.get_ends()
        assert s.ends == (ids[id(f)], ids[id(sec)])
        assert s.strength == r.get_strength()
        assert s.type_text == r.get_type().as_text() == "succ"
        assert s.complexity == r.get_type().get_complexity()
        assert s.perl_class == util.perl_ref(r)
    hi = snap.obj(ids[id(o["r_hi"])])
    assert hi.hilit == 2
    assert snap.obj(ids[id(o["r_in"])]).hilit == 0


def test_globals_and_bar_lines():
    _, snap, _ = take("groups_relations")
    assert snap.bar_lines == (0, 6) == tuple(sworkspace.get_bar_lines())
    assert snap.steps == 42
    assert snap.current_runnable == "Seqsee::SCF::FocusOn"
    Global.Feature["debug"] = 1
    assert snapshot.take().debug is True


def test_object_ids_are_unique_and_cover_relation_ends():
    _, snap, _ = take("groups_relations")
    oids = [x.oid for x in snap.elements + snap.groups + snap.relations]
    assert len(oids) == len(set(oids))
    for r in snap.relations:
        for end in r.ends:
            assert snap.obj(end) is not None


def test_relation_end_outside_the_workspace_gets_an_id_but_no_object():
    """Perl's AnchorsForRelations lookup fails for such an end and the relation is skipped;
    the snapshot keeps a distinct id with no object behind it."""
    o = gui_recipes.build("six_elements")
    from seqsee.mapping.numeric import MappingNumeric
    from seqsee.objects.anchored import Anchored
    from seqsee.srelation import SRelation
    stray = Anchored.create(o["e"][2], o["e"][3])        # never added to the workspace
    r = SRelation({"first": o["e"][0], "second": stray,
                   "type": MappingNumeric.create("succ", S.NUMBER)})
    sworkspace.relations[r] = r
    snap = snapshot.take()
    (rs,) = snap.relations
    assert snap.obj(rs.ends[0]) is snap.elements[0]
    assert snap.obj(rs.ends[1]) is None
    assert rs.ends[1] not in [x.oid for x in snap.elements + snap.groups]


def test_group_items_and_relations_within_a_group():
    """What SGUI::Workspace's %RelationsToHide is built from: consecutive items of a group."""
    o, snap, ids = take("groups_relations")
    asc = snap.obj(ids[id(o["asc"])])
    assert asc.items == tuple(ids[id(e)] for e in o["e"][0:3])
    r_in = snap.obj(ids[id(o["r_in"])])
    assert r_in.ends == asc.items[0:2]


# --- immutable -------------------------------------------------------------------------------
def test_snapshot_is_frozen_plain_data():
    _, snap, _ = take("groups_relations")
    for path, v in _walk(snap):
        assert isinstance(v, (int, float, str, bool, type(None), tuple)) or \
            dataclasses.is_dataclass(v), (path, type(v))
        if dataclasses.is_dataclass(v):
            assert type(v).__dataclass_params__.frozen, path
    with pytest.raises(dataclasses.FrozenInstanceError):
        snap.steps = 1
    with pytest.raises(dataclasses.FrozenInstanceError):
        snap.groups[0].strength = 0
    hash(snap)                                            # hashable all the way down


def test_snapshot_does_not_change_when_the_model_changes():
    o, snap, ids = take("groups_relations")
    before = dataclasses.asdict(snap)
    # Change everything the snapshot copied.
    o["asc"].set_strength(99)
    o["e"][0].set_strength(1)
    o["r_out"].set_strength(77)
    o["same"].set_metonym_activeness(0)
    o["asc"].set_is_locked_against_deletion(1)
    Global.Hilit.clear()
    Global.hilit(2, o["same"])
    Global.Steps_Finished = 1000
    Global.CurrentRunnableString = "Seqsee::SCF::Reader"
    Global.Feature["debug"] = 1
    sworkspace.add_bar_lines(3)
    sworkspace.remove_relation(o["r_in"])
    sworkspace.delete_group(o["big"])
    sworkspace.insert_elements(9)
    assert dataclasses.asdict(snap) == before
    after = snapshot.take()
    assert after != snap
    assert after.element_count == 9 and len(after.groups) == 2 and len(after.relations) == 2
    assert after.steps == 1000 and after.bar_lines == (0, 3, 6)


def test_equal_states_give_equal_snapshots():
    gui_recipes.build("groups_relations")
    a = snapshot.take()
    b = snapshot.take()
    assert a == b


# --- fast enough -----------------------------------------------------------------------------
def test_snapshot_is_fast():
    """A 30-element workspace with 10 groups and 20 relations: well under one frame (the
    budget of item 023 is 50 ms for snapshot + render)."""
    gui_recipes.build("large")
    snap = snapshot.take()
    assert snap.element_count == 30 and len(snap.groups) == 10 and len(snap.relations) == 20
    times = []
    for _ in range(20):
        t = time.perf_counter()
        snapshot.take()
        times.append(time.perf_counter() - t)
    times.sort()
    assert times[len(times) // 2] < 0.005, times      # median under 5 ms


def test_snapshot_does_not_touch_the_model():
    """Taking a snapshot must not change model state (it runs between worker steps)."""
    o = gui_recipes.build("groups_relations")
    hist = [len(x.get_history()) if hasattr(x, "get_history") else None
            for x in o["e"] + [o["asc"], o["same"], o["big"]]]
    keys = (dict(sworkspace.SUPER_GROUPS_OF), dict(Global.Hilit), list(sworkspace.relations))
    snapshot.take()
    assert keys == (dict(sworkspace.SUPER_GROUPS_OF), dict(Global.Hilit),
                    list(sworkspace.relations))
    assert hist == [len(x.get_history()) if hasattr(x, "get_history") else None
                    for x in o["e"] + [o["asc"], o["same"], o["big"]]]

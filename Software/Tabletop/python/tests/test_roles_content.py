"""The content roles of the figure, the ground, the echo and the twin, and the type key
(roles loop, item 09; D-091, D-092, D-094): the FIGURE and GROUND detectors read a row's one
repeated type and the object standing out against it, on an object and on a sameness
subgroup; the ECHO detector reads a group's echoes across the midline by type, by category or
by an object of its type, and the TWIN detector the touched object's twin across; the
TYPE_KEY detector reads the type substitution a correspondence between two grounds has made
off the Worldview, and read-worldview-roles tags it, jolts its node and drops a stale key;
consumer C1 pairs figure with figure, ground with ground (object with object, group with
group), echo with echo and the unmapped objects a key names, and an empty pool posts nothing
(nothing is gated); consumer C5 earns the key identity on a correspondence that repeats the
key; the re-cast's third tier is the TWIN tag's; the five lesions as one patch each; whole
runs on the carriers.
"""

import role_probes as RP
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import events as EV
from tabletop import roles as R
from tabletop import worldview as WV
from tabletop.codelets import dispatch
from tabletop.codelets import scouts as SC
from tabletop.coderack import make_codelet
from tabletop.config import PROFILES
from tabletop.groups import GESTALT, SAMENESS, Group
from tabletop.problems import load_problem
from tabletop.run import run_problem
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
HENRY = ["fork1", "fork2", "small-glass1"]            # Figure/Ground 1's rows (D-091)
ELIZA = ["small-glass2", "small-glass3", "fork3"]     # fork3 is the touched object


def world_of(name="figure-ground-01", seed=1, midline=True, rows=True):
    world = World(load_problem(name), CFG, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    if midline:
        tag(world, R.TABLE, R.MIDLINE)
    if rows:
        world.workspace.add_group(Group(members=list(HENRY), kind=GESTALT))
        world.workspace.add_group(Group(members=list(ELIZA), kind=GESTALT))
    return world


def tag(world, item, role, args=(), weight=1):
    return R.propose(world, item, role, args, weight, "by hand", "test", 1.0)


def run_type(world, name, *args, a=6.0):
    return dispatch.run_codelet(world, make_codelet(name, CFG, 0, args=tuple(args)), a)


def roles_read(world, item):
    return [(role, args) for role, args, _, _ in R.detect(world, item)]


# ---- FIGURE and GROUND (D-091) ---------------------------------------------------------------


def test_the_figure_is_the_only_one_of_its_type_against_the_rows_one_repeated_type():
    world = world_of()
    assert DS.ground_type_of(world, world.workspace.groups[0]) == "fork"
    assert DS.figures_on(world, DS.HENRY_ROW) == ["small-glass1"]
    assert DS.figure_role(world, "small-glass1") == [
        (R.FIGURE, (), 1, "the only small glass on the above row, against the 2 fork of g1")]
    assert DS.figure_role(world, "fork3")[0][:3] == (R.FIGURE, (), 1)
    assert DS.figure_role(world, "fork1") == [] and DS.figure_role(world, R.TABLE) == []
    assert DS.ground_role(world, "fork1") == [
        (R.GROUND, (), 1, "one of the 2 fork of g1, the ground of the above row")]
    assert DS.ground_role(world, "small-glass1") == [] and DS.ground_role(world, "g1") == []
    assert (R.FIGURE, ()) in roles_read(world, "fork3") and (R.GROUND, ()) in roles_read(world, "small-glass2")


def test_a_composition_or_a_background_with_nothing_against_it_is_no_ground():
    world = world_of("surround-05", rows=False)
    henry = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
    world.workspace.add_group(Group(members=henry, kind=GESTALT))
    assert DS.ground_type_of(world, world.workspace.groups[0]) is None   # forks and spoons
    assert all(DS.figure_role(world, o) == [] and DS.ground_role(world, o) == [] for o in henry)
    world = world_of("blockage-06", rows=False)
    world.workspace.add_group(Group(members=["small-glass1", "small-glass3"], kind=GESTALT))
    assert DS.ground_role(world, "small-glass1") == []        # no figure on Eliza's row
    world.workspace.add_group(Group(members=["small-glass2", "small-glass4"], kind=GESTALT))
    assert DS.ground_role(world, "small-glass2")[0][:3] == (R.GROUND, (), 1)
    assert DS.figure_role(world, "cup1")[0][:3] == (R.FIGURE, (), 1)     # the loner, against them


def test_a_sameness_subgroup_of_the_ground_is_the_ground_as_a_whole():
    world = world_of()
    g1 = world.workspace.groups[0]
    sub = Group(members=["fork1", "fork2"], kind=SAMENESS, parent=g1)
    name = world.workspace.add_group(sub)
    assert DS.ground_parent(world, sub) is g1
    assert DS.ground_role(world, name) == [
        (R.GROUND, (), 2, "the 2 of the 2 fork of g1, the ground of the above row")]
    assert DS.figure_role(world, name) == []
    loose = Group(members=["fork1", "small-glass2"], kind=SAMENESS)
    assert DS.ground_role(world, world.workspace.add_group(loose)) == []   # in no gestalt group


# ---- ECHO and TWIN (D-092) -------------------------------------------------------------------


def test_echoes_are_read_on_a_group_by_type_by_category_or_by_an_objects_type():
    world = world_of("blockage-06", rows=False)
    ws = world.workspace
    a = ws.add_group(Group(members=["small-glass2", "small-glass4"], kind=GESTALT))
    b = ws.add_group(Group(members=["small-glass1", "small-glass3"], kind=GESTALT))
    assert C.echoes_of(world, a) == [(R.ECHO, (b,), 2, f"{b} echoes {a} by type")]
    assert C.echoes_of(world, "cup1") == [] and C.echoes_of(world, R.TABLE) == []
    world = world_of()
    g1, g2 = (g.name for g in world.workspace.groups)
    assert C.echoes_of(world, g1) == []      # two forks and a glass against two glasses and a fork
    world5 = world_of("surround-05", rows=False)
    r1 = world5.workspace.add_group(Group(members=["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"], kind=GESTALT))
    r2 = world5.workspace.add_group(Group(members=["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"], kind=GESTALT))
    assert C.echoes_of(world5, r1) == [(R.ECHO, (r2,), 6, f"{r2} echoes {r1} by category: liquid holder "
                                                        "silverware silverware silverware silverware silverware")]
    sub = world.workspace.add_group(Group(members=["fork1", "fork2"], kind=SAMENESS))
    assert C.echoes_of(world, sub) == [(R.ECHO, ("fork3",), 2, f"fork3 echoes {sub}'s type")]
    world = world_of(midline=False)
    assert C.echoes_of(world, "g1") == []                       # the midline first


def test_the_twin_is_the_touched_objects_type_across_the_midline():
    world = world_of()
    assert C.twin_of(world, "fork1") == [(R.TWIN, ("fork3",), 1, "a fork across from fork3")]
    assert C.twin_of(world, "fork3") == [] and C.twin_of(world, "small-glass2") == []
    assert C.twin_of(world, "small-glass1") == [] and C.twin_of(world, "g1") == []
    assert C.twin_of(world_of(midline=False), "fork1") == []
    assert (R.TWIN, ("fork3",)) in roles_read(world, "fork2")


def test_the_recasts_third_tier_is_the_twin_tag_not_the_layout():
    world = world_of()
    for o in HENRY:
        tag(world, o, R.ROW, (R.ABOVE,))
    for o in ELIZA:
        tag(world, o, R.ROW, (R.BELOW,))
    tiers = {c: t for t, c, _ in R.recast(world).tiers}
    assert tiers == {"fork1": 4, "fork2": 4, "small-glass1": 4}    # no twin read yet
    tag(world, "fork1", R.TWIN, ("fork3",))
    assert R.is_twin(world, "fork3", "fork1") and not R.is_twin(world, "fork3", "fork2")
    tiers = {c: t for t, c, _ in R.recast(world).tiers}
    assert tiers == {"fork1": 3, "fork2": 4, "small-glass1": 4}


# ---- TYPE_KEY (D-094) ------------------------------------------------------------------------


def grounds_mapped(world):
    """A Worldview with the two rows mapped as wholes (an echo pair) and a fork of the one
    ground mapped to a glass of the other."""
    rows = C.create(world, "g1", "g2")
    WV.admit(world, rows)
    return rows


def test_the_key_is_read_off_a_correspondence_between_two_grounds():
    world = world_of()
    assert WV.type_keys(world, R.WORLDVIEW_ITEM) == [] and WV.type_keys(world, "fork1") == []
    rows = grounds_mapped(world)
    assert WV.key_of(world, rows) == ("fork", "small glass")
    assert WV.type_keys(world, R.WORLDVIEW_ITEM) == [
        (R.TYPE_KEY, ("fork", "small glass"), 1, "fork for small glass, by cp1")]
    cp = C.create(world, "fork1", "small-glass2")
    assert WV.key_of(world, cp) is None                        # no GROUND tag on the ends
    tag(world, "fork1", R.GROUND)
    tag(world, "small-glass2", R.GROUND)
    assert WV.key_of(world, cp) == ("fork", "small glass")
    WV.admit(world, cp)
    assert WV.type_keys(world, R.WORLDVIEW_ITEM)[0][2] == 2        # two correspondences
    figure = C.create(world, "fork3", "small-glass1")
    assert WV.key_of(world, figure) is None                     # figures, not grounds
    assert WV.key_of(world, C.create(world, "fork2", "fork3")) is None   # an identity


def test_read_worldview_roles_tags_the_key_jolts_its_node_and_drops_a_stale_one():
    world = world_of()
    assert run_type(world, "read-worldview-roles").fizzled
    rows = grounds_mapped(world)
    before = world.slipnet.activation_of("key")
    out = run_type(world, "read-worldview-roles")
    assert out.action == "found" and "TYPE_KEY:fork small glass" in out.detail
    assert world.slipnet.activation_of("key") > before
    assert WV.keys_of(world) == [("fork", "small glass")]
    WV.expel(world, [rows])
    out = run_type(world, "read-worldview-roles")
    assert out.action == "removed" and "TYPE_KEY" in out.detail and WV.keys_of(world) == []
    assert [e.reason for e in world.workspace.events.observers[0].of_type(EV.RoleDiscarded)] \
        if world.workspace.events.observers else True


def test_c5_earns_the_key_identity_on_a_correspondence_that_repeats_the_key():
    world = world_of()
    plain = C.create(world, "fork3", "small-glass1")
    assert C.key_slippages(world, plain) == [] and C.role_points(world, plain) == 0.0
    tag(world, R.WORLDVIEW_ITEM, R.TYPE_KEY, ("fork", "small glass"))
    assert C.key_slippages(world, plain) == [C.Slippage(C.ROLE, "key", "key", ("key",))]
    cp = C.create(world, "fork1", "small-glass3")
    assert C.Slippage(C.ROLE, "key", "key", ("key",)) in cp.slippages
    assert C.role_points(world, cp) > 0.0
    assert C.key_slippages(world, C.create(world, "fork2", "fork3")) == []   # not the key's types
    used = [t for t in world.workspace.roles_of(R.WORLDVIEW_ITEM)][0].consumed
    assert (R.C5, "fork1=small-glass3 repeats the key fork-small glass") in used


# ---- consumer C1 (D-095) --------------------------------------------------------------------


def test_c1_pairs_figure_with_figure_ground_like_with_like_echo_and_the_keys_objects():
    world = world_of()
    assert SC.role_pairs(world) == []
    tag(world, "fork3", R.FIGURE)
    tag(world, "small-glass1", R.FIGURE)
    assert [(x, y) for *_, x, y, _ in SC.role_pairs(world)] == [("fork3", "small-glass1")]
    tag(world, "fork1", R.GROUND)
    tag(world, "small-glass2", R.GROUND)
    sub = world.workspace.add_group(Group(members=["fork1", "fork2"], kind=SAMENESS))
    tag(world, sub, R.GROUND, weight=2)
    pairs = {(x, y) for *_, x, y, _ in SC.role_pairs(world)}
    assert ("fork1", "small-glass2") in pairs and (sub, "small-glass2") not in pairs   # like with like
    assert SC.like_with_like(world, R.GROUND, "fork1", "small-glass2")
    assert not SC.like_with_like(world, R.GROUND, sub, "small-glass2")
    tag(world, "g1", R.ECHO, ("g2",), weight=3)
    assert ("g1", "g2") in {(x, y) for *_, x, y, _ in SC.role_pairs(world)}
    key = tag(world, R.WORLDVIEW_ITEM, R.TYPE_KEY, ("fork", "small glass"))
    key_pairs = {(x, y) for ta, *_, x, y, _ in SC.role_pairs(world) if ta is key}
    assert key_pairs == {("fork1", "small-glass2"), ("fork1", "small-glass3"), ("fork2", "small-glass2"),
                         ("fork2", "small-glass3"), ("fork3", "small-glass1")}
    WV.admit(world, C.create(world, "fork1", "small-glass2"))      # mapped: no longer the key's
    key_pairs = {(x, y) for ta, *_, x, y, _ in SC.role_pairs(world) if ta is key}
    assert key_pairs == {("fork2", "small-glass3"), ("fork3", "small-glass1")}


def test_c1_with_an_empty_pool_posts_nothing_and_the_key_pairs_need_the_midline():
    world = world_of(midline=False)
    tag(world, R.WORLDVIEW_ITEM, R.TYPE_KEY, ("fork", "small glass"))
    draws = world.rng.draws
    out = run_type(world, "propose-role-cp")
    assert out.fizzled and out.posted == [] and world.rng.draws == draws
    world = world_of()
    key = tag(world, R.WORLDVIEW_ITEM, R.TYPE_KEY, ("fork", "small glass"))
    out = run_type(world, "propose-role-cp")
    assert out.action == "proposed" and [c.type for c in out.posted] == ["create-cp", "check-roles"]
    assert (R.C1, f"create-cp {' '.join(out.posted[0].args)}") in key.consumed


# ---- the lesions (D-104) and whole runs ---------------------------------------------------


def test_each_lesion_is_one_patch_and_reads_nothing():
    world = world_of()
    grounds_mapped(world)
    sub = world.workspace.add_group(Group(members=["fork1", "fork2"], kind=SAMENESS))
    live = {"A4": lambda: DS.figure_role(world, "fork3"), "A5": lambda: DS.ground_role(world, "fork1"),
            "A6": lambda: C.echoes_of(world, sub), "A7": lambda: C.twin_of(world, "fork1"),
            "K2": lambda: WV.type_keys(world, R.WORLDVIEW_ITEM)}
    for name, read in live.items():
        assert read()
        with RP.lesioned(name):
            assert read() == []
        assert read()


def test_whole_runs_read_the_figure_the_ground_the_echo_the_twin_and_the_key():
    # Figure/Ground 1 seed 64 and Blockage 6 seed 5 (Figure/Ground 1 seed 4 through v4;
    # since v5's fix, D-116 to D-120, 64 is the first seed that reads all of these).
    log = EV.Collector()
    result = run_problem(load_problem("figure-ground-01"), CFG, 64, [log])
    tags = {(e.item, e.role, tuple(e.args)) for e in log.of_type(EV.RoleTagged)}
    assert ("fork3", R.FIGURE, ()) in tags and ("small-glass1", R.FIGURE, ()) in tags
    assert {(o, R.GROUND, ()) for o in ("fork1", "fork2", "small-glass2", "small-glass3")} <= tags
    assert (R.WORLDVIEW_ITEM, R.TYPE_KEY, ("fork", "small glass")) in tags
    used = {(e.role, e.consumer) for e in log.of_type(EV.RoleConsumed)}
    assert (R.TYPE_KEY, R.C5) in used and (R.FIGURE, R.C1) in used
    assert result.answer in load_problem("figure-ground-01").ids()
    log = EV.Collector()
    run_problem(load_problem("blockage-06"), CFG, 5, [log])
    tags = {(e.item, e.role, tuple(e.args)) for e in log.of_type(EV.RoleTagged)}
    assert ("g1", R.ECHO, ("g2",)) in tags
    assert ("ECHO", "C1") in {(e.role, e.consumer) for e in log.of_type(EV.RoleConsumed)}

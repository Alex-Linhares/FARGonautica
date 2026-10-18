"""The content roles and the role slippage, with consumers C5 and C1 (roles loop, item 05;
D-087, D-088, D-090, D-098): the MEMBER, SIDEKICK, ANCHOR and CONNECTOR detectors read on
one table from the groups and the neighbours of D-027 (a run of identical neighbours is one
sidekick of weight n; sidekicks are read beside the touched object and the objects of its
family; an anchor's category is unique in its group; a connector bridges two runs); the
ROLE slippage of two ends sharing a role, scored by the tenth static factor
`cp_static_weights["role"]` times the node's activation over 150, jolting its node on
entry and parallel through its label, and nothing without a shared role (nothing is
gated); C1's sidekick, anchor and connector pairs across the table; the C5 lesion as a
patch of one function; find-neighbor's find-roles; a whole run.
"""

import pytest

import role_probes as RP
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import roles as R
from tabletop import worldview as WV
from tabletop.codelets import dispatch
from tabletop.codelets import scouts as SC
from tabletop.coderack import make_codelet
from tabletop.config import PROFILES
from tabletop.events import Collector, RoleTagged
from tabletop.groups import GESTALT, Group
from tabletop.problems import load_problem
from tabletop.run import run_problem
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]        # Surround 5
ELIZA_ROW = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]


def world_of(name="surround-05", seed=1, rows=(HENRY_ROW, ELIZA_ROW), chain=True):
    """A world with every object in view, the two rows as gestalt groups and, with
    `chain`, every member's neighbours found (D-027), the midline perceived (D-084)."""
    world = World(load_problem(name), CFG, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    for row in rows:
        group = Group(members=list(row), kind=GESTALT)
        world.workspace.add_group(group)
        DS.notice_orientation(world, group)
        if chain:
            for m in row:
                DS.find_neighbours(world, group, m)
    R.propose(world, R.TABLE, R.MIDLINE, (), 1, "by hand", "test", 1.0)
    return world


def read(world, item, role):
    return [r for r in R.detect(world, item) if r[0] == role]


def run_type(world, type_, *args, a=1.0):
    return dispatch.run_codelet(world, make_codelet(type_, CFG, 0, tuple(args)), a)


# ---- the detectors (D-087, D-088, D-090) --------------------------------------------------------


def test_a_member_is_an_object_in_a_gestalt_group_with_its_words_as_evidence():
    world = world_of(chain=False)
    [(role, args, weight, evidence)] = read(world, "cup1", R.MEMBER)
    assert (role, args, weight) == (R.MEMBER, (), 1) and evidence == "in g1"
    DS.find_ends(world, world.workspace.group_named("g1"))
    assert read(world, "fork1", R.MEMBER)[0][3] == "in g1: end left"
    assert read(world, "cup2", R.MEMBER) == [] and read(world, "g1", R.MEMBER) == []
    assert read(world, R.TABLE, R.MEMBER) == []


def test_sidekicks_are_read_beside_the_touched_object_and_its_family_with_the_run_as_weight():
    world = world_of()
    assert read(world, "knife1", R.SIDEKICK) == [
        (R.SIDEKICK, ("cup1",), 1, "cup1's left neighbour; a run of 1 knife")]
    assert read(world, "spoon1", R.SIDEKICK) == [
        (R.SIDEKICK, ("cup1",), 2, "cup1's right neighbour; a run of 2 spoon")]
    # The glass shares the cup's nearest category (liquid holder): its sidekicks are read.
    assert [(r[1], r[2]) for r in read(world, "spoon4", R.SIDEKICK)] == [(("small-glass1",), 2)]
    assert [(r[1], r[2]) for r in read(world, "fork3", R.SIDEKICK)] == [(("small-glass1",), 2)]
    # Beside a fork, a knife or a spoon (not the cup's family) nothing is a sidekick.
    assert read(world, "fork2", R.SIDEKICK) == [] and read(world, "spoon2", R.SIDEKICK) == []
    assert read(world, "knife2", R.SIDEKICK) == [] and read(world, "cup1", R.SIDEKICK) == []
    assert DS.in_touched_family(world, "small-glass1") and not DS.in_touched_family(world, "fork3")
    assert DS.nearest_category(world, "cup1") == "liquid holder"
    assert DS.run_length(world.problem, world.workspace.group_named("g1"), "spoon1", "right") == 2
    assert DS.run_length(world.problem, world.workspace.group_named("g1"), "spoon1", "left") == 1


def test_the_buridan_sidekicks_are_the_doubled_cups_and_glasses():
    world = world_of("buridan-01", rows=(["cup1", "cup2", "spoon1"], ["spoon2", "small-glass1",
                                          "small-glass2"], ["cup3", "cup4", "fork1"]))
    assert read(world, "cup2", R.SIDEKICK) == [
        (R.SIDEKICK, ("spoon1",), 2, "spoon1's left neighbour; a run of 2 cup")]
    assert [(r[1], r[2]) for r in read(world, "small-glass1", R.SIDEKICK)] == [(("spoon2",), 2)]
    assert [(r[1], r[2]) for r in read(world, "cup4", R.SIDEKICK)] == [(("fork1",), 2)]
    assert read(world, "cup1", R.SIDEKICK) == [] and read(world, "spoon1", R.SIDEKICK) == []


def test_an_anchor_has_a_neighbour_each_side_and_a_category_no_other_member_shares():
    world = world_of()
    assert read(world, "cup1", R.ANCHOR) == [
        (R.ANCHOR, (), 1, "the one liquid holder in g1; knife1 and spoon1 either side")]
    assert read(world, "small-glass1", R.ANCHOR)[0][3].startswith("the one liquid holder in g2")
    for other in ("fork1", "knife1", "spoon1", "spoon2", "fork3", "knife2", "cup2"):
        assert read(world, other, R.ANCHOR) == [], other
    # Without its neighbours known, or at an end, nothing is read.
    assert read(world_of(chain=False), "cup1", R.ANCHOR) == []
    nine = world_of("surround-09", rows=(["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"],
                                         ["spoon3", "spoon4", "salt-shaker1", "fork3", "fork4", "knife2"]))
    assert read(nine, "salt-shaker1", R.ANCHOR)[0][3].startswith("the one closed object")


def test_a_connector_bridges_two_runs_of_different_types():
    world = world_of()
    assert read(world, "small-glass1", R.CONNECTOR) == [
        (R.CONNECTOR, (), 1, "between spoon4 (spoon3 beyond) and fork3 (fork4 beyond)")]
    assert read(world, "cup1", R.CONNECTOR) == []         # knife1's far neighbour is a fork
    for other in ("fork2", "knife1", "spoon1", "spoon4", "fork3", "fork4"):
        assert read(world, other, R.CONNECTOR) == [], other
    assert read(world_of(chain=False), "small-glass1", R.CONNECTOR) == []
    group = world.workspace.group_named("g2")
    assert DS.axis_neighbours(world.problem, group, "small-glass1") == ("spoon4", "fork3")
    assert DS.axis_neighbours(world.problem, group, "spoon3") == (None, "spoon4")


def test_the_four_detectors_are_the_registrys_and_read_nothing_on_a_group_or_the_table():
    for name, function in ((R.MEMBER, "describe.member_roles"), (R.SIDEKICK, "describe.sidekicks_of"),
                           (R.ANCHOR, "describe.anchor_of"), (R.CONNECTOR, "describe.connector_of")):
        assert R.spec(name).detector == function and R.C5 in R.spec(name).consumers
        assert not R.spec(name).pending
    assert R.spec(R.MEMBER).consumers == (R.C5, R.C3) and not R.spec(R.MEMBER).pair
    world = world_of()
    for item in ("g1", "g2", R.TABLE, R.WORLDVIEW_ITEM):
        assert all(r[0] not in (R.MEMBER, R.SIDEKICK, R.ANCHOR, R.CONNECTOR)
                   for r in R.detect(world, item)), item


# ---- consumer C5: the role slippage and the tenth factor (D-098) ------------------------------


def tag(world, item, role, args=()):
    return R.propose(world, item, role, args, 1, "by hand", "test", 1.0)


def test_without_a_shared_role_nothing_is_gated_and_the_rating_is_the_nines():
    world = world_of()
    cp = C.create(world, "cup1", "cup2")                 # the loner carries no single-table role
    assert C.slippages_of_kind(cp, C.ROLE) == [] and C.role_slippages(world, cp) == []
    draws = world.rng.draws
    assert C.role_points(world, cp) == 0.0 and world.rng.draws == draws
    names = [n for n, _ in C.static_terms(world, cp)]
    assert names[-1] == "role" and len(names) == 10
    assert C.static_points(world, cp) == pytest.approx(sum(v for _, v in C.static_terms(world, cp)))


def test_two_ends_sharing_a_role_carry_an_identity_scored_by_the_role_factor():
    world = world_of()
    for item in ("cup1", "small-glass1"):
        for role in (R.MEMBER, R.ANCHOR):
            tag(world, item, role)
    tag(world, "cup1", R.CONNECTOR)                       # the cup alone: no identity
    cp = C.create(world, "cup1", "small-glass1")
    found = C.slippages_of_kind(cp, C.ROLE)
    assert [(s.a, s.b, s.labels) for s in found] == [
        ("member", "member", ("group",)), ("anchor", "anchor", ("anchor",))]
    assert all(s.identity for s in found)
    expected = sum(C.weight(world, "role") * world.slipnet.activation_of(n) / 150.0
                   for n in ("group", "anchor"))
    assert C.role_points(world, cp) == pytest.approx(expected) and expected > 0
    assert dict(C.static_terms(world, cp))["role"] == pytest.approx(expected)
    assert C.weight(world, "role") == 10.0
    assert "anchor" in WV.parallel_labels(cp) and "group" in WV.parallel_labels(cp)   # D-043
    # On entry the labels are jolted, as every slippage's are (D-030).
    before = world.slipnet.activation_of("anchor")
    jolted = dict(WV.entry_jolts(world, cp))
    assert jolted["anchor"] == world.config.jolt_label
    assert world.slipnet.activation_of("anchor") > before
    C.describe(world, cp)                                 # build-cp-descrip's descriptors
    assert "anchor-anchor" in world.workspace.descriptions.get(cp.name, [])


def test_rival_roles_on_the_two_ends_are_a_slip_that_feeds_and_earns_nothing():
    world = world_of()
    tag(world, "cup1", R.MEMBER)
    tag(world, "cup2", R.LONER)
    tag(world, "cup1", R.COLUMN, ("left",))
    tag(world, "cup2", R.COLUMN, ("right",))     # the selector roles are not C5's (D-085)
    cp = C.create(world, "cup1", "cup2")
    [s] = C.slippages_of_kind(cp, C.ROLE)
    assert (s.a, s.b, s.labels, s.identity) == ("member", "loner", (), False)
    assert C.role_points(world, cp) == 0.0 and "member" not in WV.parallel_labels(cp)


def test_comparison_roles_are_not_c5s_and_sidekicks_of_different_objects_still_match():
    world = world_of()
    tag(world, "spoon1", R.MIRROR_IMAGE, ("spoon4",))
    tag(world, "spoon4", R.MIRROR_IMAGE, ("spoon1",))
    tag(world, "spoon1", R.SIDEKICK, ("cup1",))
    tag(world, "spoon4", R.SIDEKICK, ("small-glass1",))
    cp = C.create(world, "spoon1", "spoon4")
    assert [(s.a, s.b) for s in C.slippages_of_kind(cp, C.ROLE)] == [("sidekick", "sidekick")]


def test_the_role_row_of_the_static_weights_is_refused_at_zero():
    import dataclasses
    for value in (0, -1):
        weights = tuple((k, value if k == "role" else v) for k, v in CFG.cp_static_weights)
        with pytest.raises(ValueError):
            dataclasses.replace(CFG, cp_static_weights=weights)


# ---- consumer C1: sidekick, anchor and connector pairs (D-095 revised) -----------------------


def test_c1_pairs_the_content_roles_tag_with_tag_across_the_table():
    world = world_of()
    tag(world, "spoon1", R.SIDEKICK, ("cup1",))
    tag(world, "spoon4", R.SIDEKICK, ("small-glass1",))
    tag(world, "knife1", R.SIDEKICK, ("cup1",))            # pairs with spoon4, not spoon1
    tag(world, "cup1", R.ANCHOR)
    tag(world, "small-glass1", R.ANCHOR)
    tag(world, "small-glass1", R.CONNECTOR)
    pairs = [(ta.role, a, b) for ta, _, a, b, _ in SC.role_pairs(world)]
    assert pairs == [(R.SIDEKICK, "spoon1", "spoon4"), (R.SIDEKICK, "spoon4", "knife1"),
                     (R.ANCHOR, "cup1", "small-glass1")]
    out = run_type(world, "propose-role-cp", a=6.0)
    assert out.action == "proposed" and [c.type for c in out.posted] == ["create-cp", "check-roles"]
    assert tuple(out.posted[0].args) in {("spoon1", "spoon4"), ("spoon4", "knife1"),
                                         ("cup1", "small-glass1")}


def test_the_c5_lesion_is_one_patch_and_find_neighbor_posts_find_roles():
    world = world_of()
    tag(world, "cup1", R.ANCHOR)
    tag(world, "small-glass1", R.ANCHOR)
    with RP.lesioned("C5"):
        cp = C.create(world, "cup1", "small-glass1")
        assert C.slippages_of_kind(cp, C.ROLE) == [] and C.role_points(world, cp) == 0.0
    assert C.slippages_of_kind(C.create(world, "knife1", "knife2"), C.ROLE) == []
    assert C.role_slippages(world, world.workspace.correspondence_between("cup1", "small-glass1"))
    fresh = world_of(chain=False)
    out = run_type(fresh, "find-neighbor", "cup1")
    assert out.action == "found" and out.posted[-1].type == "find-roles"
    assert tuple(out.posted[-1].args) == ("cup1",)
    assert run_type(world, "find-roles", "small-glass1").action == "found"
    assert {t.role for t in world.workspace.roles_of("small-glass1")} >= {R.MEMBER, R.CONNECTOR}


def test_a_run_reads_members_sidekicks_and_the_glass_as_anchor_and_connector():
    log = Collector()
    run_problem(load_problem("surround-05"), CFG, 2, [log])     # seed 2 since item 08's runs
    tags = log.of_type(RoleTagged)
    roles = {e.role for e in tags}
    assert R.MEMBER in roles and roles & {R.MIRROR_IMAGE, R.POINT_IMAGE}
    assert roles <= {R.MIDLINE, R.MIRROR_IMAGE, R.POINT_IMAGE, R.SYMMETRY_AXIS, R.MEMBER,
                     R.SIDEKICK, R.ANCHOR, R.CONNECTOR, R.ROW, R.COLUMN, R.COUNTERPART,
                     R.GAP, R.CORNER, R.LONER, R.STRAY, R.FIGURE, R.GROUND, R.ECHO, R.TWIN,
                     R.TYPE_KEY}
    assert all(e.weight >= 1 for e in tags)
    found = set()
    # The glass's anchor and connector tags need a run whose chain of neighbours reaches
    # it before the answer: since item 08's runs seed 99 reads the glass as anchor (the
    # first of seeds 1-150) and seed 14 spoon1 as sidekick (seed 11 through v4; since v5's
    # fix, D-116 to D-120, 14 is the first); item 11 measures how often the chain is
    # complete in time.
    for seed in (99, 14):
        log = Collector()
        run_problem(load_problem("surround-05"), CFG, seed, [log])
        found |= {(e.item, e.role) for e in log.of_type(RoleTagged)}
    assert ("small-glass1", R.ANCHOR) in found and ("spoon1", R.SIDEKICK) in found
    assert ("cup1", R.CONNECTOR) not in found

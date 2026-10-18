"""The parts of a side and the items of neither (D-086, D-085, D-100; the roles loop's item
08): the GAP, LONER, STRAY and CORNER detectors, the midline from clusters and over a stray
group, consumer C1's loner, corner and part pairs with no pair from a stray, and consumer
C7's role term and stray scale, each with its "nothing is gated" check and its lesion."""

from pathlib import Path

import pytest

from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import events as EV
from tabletop import geometry as G
from tabletop import roles as R
from tabletop import salience as S
from tabletop.codelets import dispatch
from tabletop.codelets import scouts as SC
from tabletop.coderack import make_codelet
from tabletop.config import PROFILES
from tabletop.groups import GESTALT, Group
from tabletop.problems import load_problem
from tabletop.run import run_problem
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

import role_probes as RP

CFG = V2_PROFILES["standard"]
PYTHON_DIR = Path(__file__).resolve().parents[1]


def world_of(name="surround-05", seed=1):
    """A world with every object in view, no group, no tag (D-079: an empty pool)."""
    world = World(load_problem(name), CFG, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    return world


def group(world, members, name=None):
    """A gestalt group built by hand, as find-group would record it (D-023)."""
    g = Group(members=list(members), kind=GESTALT)
    world.workspace.add_group(g)
    return g


def midline(world):
    return R.propose(world, R.TABLE, R.MIDLINE, (), 1, "by hand", "test", 1.0)


def tag(world, item, role, args=()):
    return R.propose(world, item, role, args, 1, "by hand", "test", 1.0)


# ---- the geometry (D-085) ---------------------------------------------------------------------


def test_the_four_corners_are_the_regions_in_an_outer_row_and_an_outer_column():
    corners = [r.name for r in G.REGIONS if G.is_corner(r)]
    assert corners == ["upper-left", "upper-right", "lower-left", "lower-right"]
    assert not G.is_corner(G.region_named("middle")) and not G.is_corner(G.region_named("upper-middle"))


# ---- clusters, gaps and the loner (D-086) ------------------------------------------------------


def test_the_clusters_of_a_row_are_its_gestalt_groups_and_its_isolated_objects_in_order():
    world = world_of()
    assert DS.clusters_on(world, DS.ELIZA_ROW) == []           # nothing found yet
    g2 = group(world, ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"])
    world.workspace.mark_isolated("cup2")
    names = [name for name, _ in DS.clusters_on(world, DS.ELIZA_ROW)]
    assert names == [g2.name, "cup2"]                           # by the boxes' left edges
    assert DS.band_of(world, g2.name) == DS.ELIZA_ROW and DS.band_of(world, "cup2") == DS.ELIZA_ROW
    assert DS.clusters_on(world, DS.HENRY_ROW) == []           # cup1 is not isolated


def test_a_gap_parts_two_clusters_beyond_the_zero_gap_with_nothing_between():
    """The gap reuses D-022's `proximity_zero_gap`: no distance of its own."""
    world = world_of()
    g2 = group(world, ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"])
    assert DS.gaps_on(world, DS.ELIZA_ROW) == []               # one cluster: no gap
    world.workspace.mark_isolated("cup2")
    gaps = DS.gaps_on(world, DS.ELIZA_ROW)
    assert len(gaps) == 1 and gaps[0][:2] == (g2.name, "cup2")
    assert gaps[0][2] >= CFG.proximity_zero_gap
    assert DS.gaps_on(world, DS.HENRY_ROW) == []
    # The detector tags the table alone, with the row word and the two clusters.
    assert DS.gaps_of(world, "cup2") == []
    assert [(role, args, w) for role, args, w, _ in DS.gaps_of(world, R.TABLE)] == \
        [(R.GAP, (R.BELOW, g2.name, "cup2"), 1)]
    # Two close clusters are not parted: the two spoons and the rest of the row.
    world = world_of()
    group(world, ["spoon3", "spoon4"])
    group(world, ["small-glass1", "fork3", "fork4", "knife2"])
    assert DS.gaps_on(world, DS.ELIZA_ROW) == []


def test_an_object_between_two_clusters_closes_the_gap():
    """Blockage 2's cup and far glass are parted only while nothing stands between them:
    with the glass unnoticed by any find-group there is still one cluster, and a loner
    needs the gap read, since the glass is physically on its side."""
    world = world_of("blockage-02")
    world.workspace.mark_isolated("cup1")
    assert DS.gaps_on(world, DS.HENRY_ROW) == []
    assert DS.loner_of(world, "cup1") == []                    # not alone: the glass is there
    world.workspace.mark_isolated("small-glass2")
    assert [g[:2] for g in DS.gaps_on(world, DS.HENRY_ROW)] == [("cup1", "small-glass2")]
    assert DS.loner_of(world, "cup1")[0][0] == R.LONER and DS.loner_of(world, "small-glass2")
    # Eliza's glass is alone on its row: a loner with no gap needed.
    assert DS.loner_of(world, "small-glass1") == []            # until a find-group looked
    world.workspace.mark_isolated("small-glass1")
    assert DS.loner_of(world, "small-glass1") == [(R.LONER, (), 1, "alone on the below row")]


def test_a_loner_is_isolated_in_no_group_on_a_side_and_set_off_by_a_gap():
    world = world_of()
    group(world, ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"])
    assert DS.loner_of(world, "cup2") == []                    # no find-group looked yet
    world.workspace.mark_isolated("cup2")
    (role, args, weight, evidence), = DS.loner_of(world, "cup2")
    assert role == R.LONER and weight == 1 and "set off from g1" in evidence
    assert DS.loner_of(world, "spoon3") == []                  # a member
    assert DS.loner_of(world, R.TABLE) == []


def test_a_stray_is_an_item_of_neither_side_and_a_member_is_one_through_its_group():
    world = world_of("surround-10")
    assert DS.stray_role(world, "pepper-shaker1") == []        # not on its own yet
    world.workspace.mark_isolated("pepper-shaker1")
    assert DS.stray_role(world, "pepper-shaker1") == [(R.STRAY, (), 1, "on its own, seen in middle")]
    assert DS.loner_of(world, "pepper-shaker1") == []          # the band is no side
    junk = group(world, ["fork5", "knife3", "small-glass2"])
    assert DS.stray_group(world, junk)
    assert DS.stray_role(world, junk.name) == [(R.STRAY, (), 3, "a group of 3 lying in the middle band")]
    assert DS.stray_role(world, "fork5") == []                 # a member: through its group
    assert not DS.is_stray(world, "fork5")
    tag(world, junk.name, R.STRAY)
    assert DS.is_stray(world, "fork5") and DS.is_stray(world, junk.name) and not DS.is_stray(world, "cup2")
    row = group(world, ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"])
    assert not DS.stray_group(world, row)


def test_the_midline_is_read_from_clusters_and_tolerates_a_stray_group():
    """A table of loners has a midline (D-084 revised); a group wholly in the band does not
    block it, a group reaching into it does."""
    world = world_of("blockage-02")
    assert DS.midline_perceived(world, R.TABLE) == []
    world.workspace.mark_isolated("small-glass1")
    (role, _, _, evidence), = DS.midline_perceived(world, R.TABLE)
    assert role == R.MIDLINE and "1 cluster" in evidence
    world = world_of("surround-10")
    group(world, ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"])
    group(world, ["fork5", "knife3", "small-glass2"])
    (_, _, _, evidence), = DS.midline_perceived(world, R.TABLE)
    assert "1 stray group" in evidence
    world.workspace.regions["spoon2"] = "middle"               # the row reaches into the band
    assert DS.midline_perceived(world, R.TABLE) == []


def test_a_corner_is_an_object_on_its_own_or_a_group_never_a_member():
    world = world_of()
    assert DS.corner_role(world, "cup2") == []                 # not on its own yet
    world.workspace.mark_isolated("cup2")
    assert DS.corner_role(world, "cup2") == [(R.CORNER, (), 1, "seen in lower-right")]
    g1 = group(world, ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"])
    assert DS.corner_role(world, "fork1") == []                # a member: its group is the item
    (role, _, weight, evidence), = DS.corner_role(world, g1.name)
    assert role == R.CORNER and weight == 6 and evidence == "seen in upper-left"
    world.workspace.mark_isolated("spoon1")                    # the middle column is no corner
    assert DS.corner_role(world, "knife2") == [] and DS.corner_role(world, R.TABLE) == []


# ---- consumer C1: loner, corner and part pairs, no pair from a stray (D-095) ------------------


def test_c1_pairs_loner_with_loner_and_corner_with_corner_the_point_image_first():
    world = world_of("blockage-02")
    midline(world)
    assert SC.role_pairs(world) == []                          # nothing is gated
    tag(world, "cup1", R.LONER)
    assert SC.role_pairs(world) == []                          # one loner: no pair
    tag(world, "small-glass1", R.LONER)
    pairs = SC.role_pairs(world)
    assert [(p[2], p[3]) for p in pairs] == [("cup1", "small-glass1")]
    tag(world, "cup1", R.CORNER)
    tag(world, "small-glass2", R.CORNER)
    tag(world, "small-glass1", R.CORNER)
    corners = {(p[2], p[3]): p[4] for p in SC.role_pairs(world) if p[0].role == R.CORNER}
    assert set(corners) == {("cup1", "small-glass1"), ("small-glass2", "small-glass1")}
    point, mirror = corners[("small-glass2", "small-glass1")], corners[("cup1", "small-glass1")]
    assert SC.corner_order(world, "small-glass2", "small-glass1") == 1.0            # the point image
    assert SC.corner_order(world, "cup1", "small-glass1") == dict(CFG.region_preference)["opposite"]
    ta, tb = world.workspace.role_tag("cup1", R.CORNER), world.workspace.role_tag("small-glass1", R.CORNER)
    assert mirror == pytest.approx(SC.pair_weight(world, ta, tb) * 0.7) and point > mirror


def test_c1_pairs_the_parts_a_gap_marks_off_under_the_axis():
    world = world_of()
    midline(world)
    above = tag(world, R.TABLE, R.GAP, (R.ABOVE, "fork1", "spoon2"))
    assert SC.role_pairs(world) == []                          # a gap on one row alone
    below = tag(world, R.TABLE, R.GAP, (R.BELOW, "spoon3", "cup2"))
    pairs = {(p[2], p[3]) for p in SC.role_pairs(world)}
    assert pairs == {("fork1", "spoon3"), ("spoon2", "cup2"), ("fork1", "cup2"), ("spoon2", "spoon3")}
    assert all(p[0] is above and p[1] is below for p in SC.role_pairs(world))
    tag(world, R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.MIRROR,))
    assert {(p[2], p[3]) for p in SC.role_pairs(world)} == {("fork1", "spoon3"), ("spoon2", "cup2")}
    world.workspace.discard_role(world.workspace.roles_of(R.WORLDVIEW_ITEM)[0], "test")
    tag(world, R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.POINT,))
    assert {(p[2], p[3]) for p in SC.role_pairs(world)} == {("fork1", "cup2"), ("spoon2", "spoon3")}


def test_c1_never_proposes_a_pair_from_a_stray():
    world = world_of("surround-10")
    midline(world)
    tag(world, "cup1", R.LONER)
    tag(world, "cup2", R.LONER)
    assert [(p[2], p[3]) for p in SC.role_pairs(world)] == [("cup1", "cup2")]
    world.workspace.tag_role(R.RoleTag("cup2", R.STRAY))      # straight into the pool: no fight
    assert SC.role_pairs(world) == []
    outcome = dispatch.run_codelet(world, make_codelet("propose-role-cp", CFG, 0), 6.0)
    assert outcome.posted == [] and "no role pair" in outcome.detail


# ---- consumer C7: the role term and the stray scale (D-100) -----------------------------------


def test_c7_reads_nothing_from_an_empty_pool_and_the_terms_are_frenchs_plus_the_role_term():
    world = world_of()
    for item in world.workspace.items():
        assert S.role_reading(world, item) == (0.0, 1.0)
        assert dict(S.terms(world, item))["role"] == 0.0
    assert S.FACTORS[-1] == "role" and len(S.FACTORS) == 9
    before = S.salience(world, "small-glass1")
    tag(world, "small-glass1", R.ANCHOR)
    assert S.role_term(world, "small-glass1") == dict(CFG.salience_weights)["role"]
    assert S.salience(world, "small-glass1") == pytest.approx(before + 5.0)
    tag(world, "small-glass1", R.CONNECTOR)
    assert S.role_term(world, "small-glass1") == 10.0
    tag(world, "small-glass1", R.MEMBER)                       # not a salient role
    assert S.role_term(world, "small-glass1") == 10.0
    world.workspace.roles[0].weight = 2                        # a run of two: per unit of weight
    assert S.role_term(world, "small-glass1") == 15.0


def test_c7_halves_a_strays_region_term_through_its_own_tag_or_its_groups():
    world = world_of("surround-10")
    plain = S.region_term(world, "pepper-shaker1")
    tag(world, "pepper-shaker1", R.STRAY)
    assert S.region_term(world, "pepper-shaker1") == pytest.approx(plain * CFG.stray_salience_scale)
    assert S.role_reading(world, "pepper-shaker1") == (0.0, CFG.stray_salience_scale)
    junk = group(world, ["fork5", "knife3", "small-glass2"])
    member = S.region_term(world, "fork5")
    tag(world, junk.name, R.STRAY)
    assert S.region_term(world, "fork5") == pytest.approx(member * CFG.stray_salience_scale)
    with RP.lesioned("C7"):
        assert S.region_term(world, "fork5") == pytest.approx(member)
        assert S.region_term(world, "pepper-shaker1") == pytest.approx(plain)
        tag(world, "cup2", R.ANCHOR)
        assert S.role_term(world, "cup2") == 0.0
    assert S.role_term(world, "cup2") == 5.0


# ---- the lesions and a whole run (D-104) ------------------------------------------------------


def test_the_detector_lesions_are_one_patch_each():
    world = world_of()
    group(world, ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"])
    world.workspace.mark_isolated("cup2")
    assert DS.gaps_of(world, R.TABLE) and DS.loner_of(world, "cup2") and DS.corner_role(world, "cup2")
    for lesion, call in (("S2", lambda: DS.gaps_of(world, R.TABLE)),
                         ("L4", lambda: DS.loner_of(world, "cup2")),
                         ("L3", lambda: DS.corner_role(world, "cup2"))):
        with RP.lesioned(lesion):
            assert call() == []
        assert call()
    world = world_of("surround-10")
    world.workspace.mark_isolated("salt-shaker1")
    with RP.lesioned("L5"):
        assert DS.stray_role(world, "salt-shaker1") == []
    assert DS.stray_role(world, "salt-shaker1")
    assert C.C5_LEAVES_OUT == (R.ROW, R.COLUMN, R.CORNER, R.STRAY, R.GAP)
    assert C.read_by_c5(R.LONER) and not C.read_by_c5(R.CORNER)


def test_a_run_reads_loners_corners_and_a_gap_and_c1_proposes_a_loner_pair():
    """Surround 1 seed 2 and Blockage 2 seed 6 (item 08's runs): the loners are read on the
    tables French built of them, the gap on the table, and C1 proposes loner with loner."""
    for name, seed in (("surround-01", 2), ("blockage-02", 6)):
        log = EV.Collector()
        result = run_problem(load_problem(name), CFG, seed, [log])
        tags = {(e.item, e.role) for e in log.of_type(EV.RoleTagged)}
        assert (R.TABLE, R.GAP) in tags and (R.TABLE, R.MIDLINE) in tags
        assert any(role == R.LONER for _, role in tags) and any(role == R.CORNER for _, role in tags)
        assert (R.LONER, "C1") in {(e.role, e.consumer) for e in log.of_type(EV.RoleConsumed)}
        assert result.answer in load_problem(name).ids()
    log = EV.Collector()
    run_problem(load_problem("surround-10"), CFG, 4, [log])      # seed 4 since item 09's runs
    strays = [e for e in log.of_type(EV.RoleTagged) if e.role == R.STRAY]
    assert strays and strays[0].item.startswith("g") and strays[0].weight >= 2   # a stray group
    assert all(e.item.startswith("g") for e in strays)                        # never a member

"""The geometric family and the midline, with consumer C6 (roles loop, item 04; D-082,
D-083, D-084, D-099): `geometry.placement`, `row_of`, `column_of` and `in_midline_band`
are pure and equivariant under the mirror, the side swap and the rotation of the table
(an exact test over every pair of regions); the MIDLINE detector reads the table alone;
the placement slippages of a top-level correspondence across a perceived midline, scored
by factor 4, and nothing without one (nothing is gated); the mirror and point images;
the symmetry axis read without the touched object's correspondences and only at the top
context; the three codelets; the C6 lesion as a patch of one function.
"""

from unittest import mock

import pytest

import role_probes as RP
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import geometry as G
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
HENRY_ROW = ["fork1", "knife1", "cup1", "spoon1", "spoon2"]


def mirror(r):      # left-right, the variants bench's "mirrored"
    return G.Region(2 - r.column, r.row)


def swap(r):        # Henry's side for Eliza's, "side-swapped"
    return G.Region(r.column, 2 - r.row)


def rotate(r):      # both, "rotated"
    return G.Region(2 - r.column, 2 - r.row)


# ---- geometry (D-082) --------------------------------------------------------------------------


def test_placement_is_exact_and_equivariant_under_the_three_symmetries_of_the_table():
    seen = set()
    for a in G.REGIONS:
        for b in G.REGIONS:
            p = G.placement(a, b)
            seen.add(p)
            assert p in G.PLACEMENTS
            assert G.placement(b, a) == p
            for m in (mirror, swap, rotate):
                assert G.placement(m(a), m(b)) == p, (a, b, m.__name__)
            if p != G.NONE:
                assert {a.row, b.row} == {0, 2}
    assert seen == set(G.PLACEMENTS)
    ul, ll, lr, um, lm = (G.region_named(n) for n in
                          ("upper-left", "lower-left", "lower-right", "upper-middle", "lower-middle"))
    assert G.placement(ul, ll) == G.MIRROR and G.placement(ul, lr) == G.POINT
    assert G.placement(um, lm) == G.MIDDLE and G.placement(ul, um) == G.NONE
    assert G.placement(ul, G.region_named("middle-left")) == G.NONE


def test_row_column_and_band_are_the_regions_own():
    for r in G.REGIONS:
        assert G.row_of(r) == r.row and G.column_of(r) == r.column
        assert G.in_midline_band(r) == (r.row == 1)
        assert G.in_midline_band(swap(r)) == G.in_midline_band(r)


# ---- the MIDLINE detector (D-084) ---------------------------------------------------------------


def world_of(name="surround-05", seed=1, notice=True):
    world = World(load_problem(name), CFG, seed)
    if notice:
        for o in world.problem.objects:
            world.workspace.notice(o.id, world.rng)
    return world


def test_the_midline_is_read_on_the_table_alone_once_a_gestalt_group_stands():
    world = world_of()
    assert DS.midline_perceived(world, "cup1") == [] and DS.midline_perceived(world, R.TABLE) == []
    world.workspace.add_group(Group(members=list(HENRY_ROW), kind=GESTALT))
    [(role, args, weight, evidence)] = DS.midline_perceived(world, R.TABLE)
    assert (role, args, weight) == (R.MIDLINE, (), 1) and "no grouped object" in evidence
    # A grouped object seen in the band blocks the reading; an ungrouped one does not.
    world.workspace.regions["cup1"] = "middle-left"
    assert DS.midline_perceived(world, R.TABLE) == []
    world.workspace.regions["cup1"] = "upper-left"
    world.workspace.regions["cup2"] = "middle-right"       # the loner, in no group
    assert DS.midline_perceived(world, R.TABLE)
    assert R.detect(world, R.TABLE)[0][0] == R.MIDLINE
    assert [r for r, *_ in R.detect(world, "cup2")] == [R.COLUMN]      # item 07: the column


def midline(world):
    return R.propose(world, R.TABLE, R.MIDLINE, (), 1, "by hand", "test", 1.0)


# ---- consumer C6: the placement slippages (D-082) ----------------------------------------------


def test_without_a_midline_nothing_is_gated_and_the_rating_is_v1s():
    world = world_of()
    cp = C.create(world, "cup1", "cup2")
    assert C.slippages_of_kind(cp, C.PLACEMENT) == [] and C.placement_slippages(world, cp) == []
    draws = world.rng.draws
    assert C.position_points(world, cp) == 0.0 and world.rng.draws == draws


def test_a_top_level_pair_across_the_midline_carries_a_placement_scored_by_factor_4():
    world = world_of()
    midline(world)
    cp = C.create(world, "cup1", "cup2")                   # upper-left against lower-right
    kinds = {s.labels for s in C.slippages_of_kind(cp, C.PLACEMENT)}
    assert kinds and kinds <= {C.IDENTITY_LABELS, C.FLIP_LABELS}
    [s] = C.slippages_of_kind(cp, C.PLACEMENT)
    assert s.a in G.COLUMNS and s.b in G.COLUMNS
    expected = C.weight(world, "position") * C.symmetry_scale(world, s)
    assert C.position_points(world, cp) == pytest.approx(expected) and expected > 0
    assert dict(C.static_terms(world, cp))["position"] == pytest.approx(expected)
    assert s.labels[0] in WV.parallel_labels(cp)           # parallel through the labels (D-043)
    # Inside a group correspondence the group frame rules: no placement (D-029).
    inner = C.Correspondence("cup1", "cup2", parent=cp)
    assert C.placement_slippages(world, inner) == []


def test_two_middle_column_ends_feed_symmetry_alone():
    world = world_of("figure-ground-01")
    midline(world)
    a, b = world.problem.ids()[:2]
    with mock.patch.object(DS, "seen_region", lambda w, item: G.region_named(
            "upper-middle" if item == a else "lower-middle")):
        cp = C.create(world, a, b)
    [s] = C.slippages_of_kind(cp, C.PLACEMENT)
    assert (s.a, s.b, s.labels) == ("middle", "middle", ("symmetry",))


# ---- the images (D-099) -------------------------------------------------------------------------


def test_images_need_the_midline_and_name_their_partner_with_the_items_objects_as_weight():
    world = world_of()
    assert C.mirror_and_point_images(world, "cup1") == []
    midline(world)
    found = C.mirror_and_point_images(world, "cup1")
    assert found and all(role in (R.MIRROR_IMAGE, R.POINT_IMAGE) and w == 1 and len(args) == 1
                         for role, args, w, _ in found)
    assert all(args[0] != "cup1" and " against " in ev for _, args, _, ev in found)
    assert C.mirror_and_point_images(world, R.TABLE) == []
    g = world.workspace.groups[world.workspace.add_group(Group(members=list(HENRY_ROW), kind=GESTALT)) and 0]
    weights = {w for _, _, w, _ in C.mirror_and_point_images(world, g.name)}
    assert weights <= {len(HENRY_ROW)}


# ---- the symmetry axis (D-083) -----------------------------------------------------------------


def placed(world, a, b, labels):
    cp = C.create(world, a, b)
    cp.slippages = [s for s in cp.slippages if s.kind != C.PLACEMENT] + \
        [C.Slippage(C.PLACEMENT, "left", "left" if labels == C.IDENTITY_LABELS else "right", labels)]
    cp.in_worldview = True
    return cp


def test_the_axis_leaves_the_touched_objects_correspondences_out_and_needs_a_majority():
    world = world_of()
    midline(world)
    assert WV.symmetry_axis(world, R.WORLDVIEW_ITEM) == [] and WV.axis_of(world) is None
    placed(world, "cup1", "cup2", C.IDENTITY_LABELS)       # the touched cup: left out
    assert WV.symmetry_axis(world, R.WORLDVIEW_ITEM) == []
    placed(world, "fork1", "fork3", C.IDENTITY_LABELS)
    [(role, args, weight, _)] = WV.symmetry_axis(world, R.WORLDVIEW_ITEM)
    assert (role, args, weight) == (R.SYMMETRY_AXIS, (G.MIRROR,), 1)
    placed(world, "knife1", "knife2", C.FLIP_LABELS)        # a tie: no axis
    assert WV.symmetry_axis(world, R.WORLDVIEW_ITEM) == []
    placed(world, "spoon1", "spoon3", C.FLIP_LABELS)
    assert WV.symmetry_axis(world, R.WORLDVIEW_ITEM)[0][1] == (G.POINT,)
    assert WV.symmetry_axis(world, "cup1") == []
    world.context = object()                                # inside a subworld: nothing
    assert WV.symmetry_axis(world, R.WORLDVIEW_ITEM) == []


def run_type(world, name, *args, a=6.0):
    return dispatch.run_codelet(world, make_codelet(name, CFG, 0, args=args), a)


def test_read_worldview_roles_tags_the_axis_jolts_its_node_and_drops_a_stale_one():
    world = world_of()
    midline(world)
    assert run_type(world, "read-worldview-roles").fizzled
    placed(world, "fork1", "fork3", C.IDENTITY_LABELS)
    before = world.slipnet.activation_of("mirror symmetry")
    out = run_type(world, "read-worldview-roles")
    assert out.action == "found" and WV.axis_of(world) == G.MIRROR
    assert world.slipnet.activation_of("mirror symmetry") > before
    for cp in list(world.workspace.correspondences):
        if cp.henry == "fork1":
            cp.in_worldview = False
    placed(world, "knife1", "knife2", C.FLIP_LABELS)
    placed(world, "spoon1", "spoon3", C.FLIP_LABELS)
    out = run_type(world, "read-worldview-roles")
    assert WV.axis_of(world) == G.POINT and not world.workspace.role_tag(R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.MIRROR,))


def test_c1_proposes_the_images_under_the_axis_alone():
    world = world_of()
    midline(world)
    R.propose(world, "fork1", R.MIRROR_IMAGE, ("fork3",), 1, "", "test", 1.0)
    world2 = world_of(seed=2)
    midline(world2)
    R.propose(world2, "fork1", R.POINT_IMAGE, ("fork4",), 1, "", "test", 1.0)
    assert [(a, b) for _, _, a, b, _ in SC.role_pairs(world)] == [("fork1", "fork3")]
    R.propose(world, R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.POINT,), 1, "", "test", 1.0)
    assert SC.role_pairs(world) == []
    R.propose(world2, R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.POINT,), 1, "", "test", 1.0)
    assert [(a, b) for _, _, a, b, _ in SC.role_pairs(world2)] == [("fork1", "fork4")]


# ---- the codelets (D-081) ----------------------------------------------------------------------


def test_find_roles_reads_the_table_and_one_item_and_fizzles_with_nothing_to_read():
    world = world_of(notice=False)
    world.workspace.noticed.clear()          # nothing in view at all (the touched object is
    out = run_type(world, "find-roles")      # noticed at the start, and has a column, item 07)
    assert out.fizzled and out.posted == [] and world.workspace.roles == []
    world = world_of()
    world.workspace.add_group(Group(members=list(HENRY_ROW), kind=GESTALT))
    out = run_type(world, "find-roles")
    assert out.action == "found" and world.workspace.has_role(R.TABLE, R.MIDLINE)
    assert [c.type for c in out.posted][0] == "compare-roles" and out.posted[-1].type == "propose-role-cp"


def test_compare_roles_needs_the_midline_and_posts_c1_on_a_find():
    world = world_of()
    assert "no midline" in run_type(world, "compare-roles", "cup1").detail
    midline(world)
    out = run_type(world, "compare-roles", "cup1")
    assert out.action == "found" and [c.type for c in out.posted] == ["propose-role-cp"]
    assert world.workspace.roles_of("cup1")
    assert run_type(world, "compare-roles", "nothing").fizzled


def test_a_run_reads_the_midline_the_images_and_sometimes_an_axis_and_the_lesion_is_one_patch():
    log = Collector()
    run_problem(load_problem("surround-05"), CFG, 2, [log])     # seed 2 since item 08's runs
    roles = {e.role for e in log.of_type(RoleTagged)}
    assert R.MIDLINE in roles and roles & {R.MIRROR_IMAGE, R.POINT_IMAGE}
    assert roles <= {R.MIDLINE, R.MIRROR_IMAGE, R.POINT_IMAGE, R.SYMMETRY_AXIS, R.MEMBER,
                     R.SIDEKICK, R.ANCHOR, R.CONNECTOR, R.ROW, R.COLUMN, R.COUNTERPART,
                     R.GAP, R.CORNER, R.LONER, R.STRAY, R.FIGURE, R.GROUND, R.ECHO, R.TWIN,
                     R.TYPE_KEY}                     # items 05 to 08
    world = world_of()
    midline(world)
    with RP.lesioned("C6"):
        cp = C.create(world, "cup1", "cup2")
        assert C.slippages_of_kind(cp, C.PLACEMENT) == []
    assert C.slippages_of_kind(C.create(world, "fork1", "fork3"), C.PLACEMENT)

"""The selector roles ROW and COLUMN, the re-cast (COUNTERPART) and consumer C2 (roles loop,
item 07; D-085, D-093, D-096): the two detectors read the table frame (ROW once a midline is
perceived, COLUMN always); the signature is read from standing tags, so a discarded role is
never re-cast; the four tiers and their a priori weights, ties published and unresolved;
the recast-role codelet, which fizzles with nothing read (nothing is gated), publishes the
re-cast, tags the counterparts and posts create-cp for the first tier; pick-salient-obj's
weighting by the re-cast, exactly French's draw with no tag standing; the placement reading
a tagged column (C6) and `across` reading the tagged rows (C1); C5 leaving the selector
roles out; the three lesions as one patch each; a whole run.
"""

import dataclasses
import json

import pytest

import role_probes as RP
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import events as EV
from tabletop import geometry as G
from tabletop import roles as R
from tabletop import snapshot as SN
from tabletop.codelets import dispatch
from tabletop.codelets import scouts as SC
from tabletop.coderack import make_codelet
from tabletop.config import PROFILES, Config
from tabletop.groups import GESTALT, Group
from tabletop.problems import load_problem
from tabletop.run import run_problem, trace_lines
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
ELIZA_ROW = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]


def world_of(name="surround-05", seed=1, midline=True):
    world = World(load_problem(name), CFG, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    if midline:
        R.propose(world, R.TABLE, R.MIDLINE, (), 1, "by hand", "test", 1.0)
    return world


def tag(world, item, role, args=()):
    return R.propose(world, item, role, args, 1, "by hand", "test", 1.0)


def rows_of_surround_5(world):
    """Henry's row above, Eliza's row and the loner below, every object in its exact
    column, as the detectors would read them without the blur (D-085)."""
    for o in world.problem.objects:
        region = G.exact_region(o.x, o.y)
        tag(world, o.id, R.ROW, (R.ABOVE if region.row == 0 else R.BELOW,))
        tag(world, o.id, R.COLUMN, (G.COLUMNS[region.column],))


def run_type(world, name, *args, a=6.0):
    return dispatch.run_codelet(world, make_codelet(name, CFG, 0, args=tuple(args)), a)


# ---- ROW and COLUMN (D-085) -----------------------------------------------------------------


def test_row_needs_the_midline_and_the_outer_bands_and_column_is_read_always():
    world = world_of(midline=False)
    assert DS.row_role(world, "cup1") == []                       # no midline: no side
    [(role, args, weight, evidence)] = DS.column_role(world, "cup1")
    assert (role, weight) == (R.COLUMN, 1) and args[0] in G.COLUMNS and "region" in evidence
    R.propose(world, R.TABLE, R.MIDLINE, (), 1, "", "test", 1.0)
    found = {args[0] for _ in range(20) for _, args, _, _ in DS.row_role(world, "cup1")}
    assert found <= set(R.ROW_WORDS) and R.ABOVE in found       # Henry's row, through the blur
    assert DS.row_role(world, R.TABLE) == [] and DS.column_role(world, "nothing") == []
    world.workspace.add_group(Group(members=list(HENRY_ROW), kind=GESTALT))
    assert [w for _, _, w, _ in DS.column_role(world, "g1")] == [6]      # a group's objects
    assert [r for r, *_ in R.detect(world, "cup1")] == [R.COLUMN] or R.ROW in [
        r for r, *_ in R.detect(world, "cup1")]


def test_an_item_seen_in_the_middle_band_is_of_neither_side():
    world = world_of("surround-10")                      # the junk lies in the middle band
    middle = [o.id for o in world.problem.objects if G.exact_region(o.x, o.y).row == 1]
    assert middle
    for item in middle:
        for _ in range(10):
            assert all(args[0] in R.ROW_WORDS for _, args, _, _ in DS.row_role(world, item))


def test_c5_leaves_the_selector_roles_out_and_c6_reads_a_tagged_column():
    world = world_of()
    tag(world, "cup1", R.COLUMN, ("right",))             # cup1 is in the left column
    tag(world, "cup2", R.COLUMN, ("right",))
    assert not C.read_by_c5(R.ROW) and not C.read_by_c5(R.COLUMN) and C.read_by_c5(R.ANCHOR)
    log = EV.Collector()
    world.events.observers.append(log)
    cp = C.create(world, "cup1", "cup2")
    assert C.slippages_of_kind(cp, C.ROLE) == []
    [placement] = C.slippages_of_kind(cp, C.PLACEMENT)
    assert (placement.a, placement.b, placement.labels) == ("right", "right", C.IDENTITY_LABELS)
    used = [(e.item, e.consumer) for e in log.of_type(EV.RoleConsumed)]
    assert used == [("cup1", "C6"), ("cup2", "C6")]
    drawn = G.Region(0, 0)
    assert C.tagged_column(world, "cup1", drawn) == G.Region(2, 0)
    assert C.tagged_column(world, "fork1", drawn) == drawn          # no tag: as drawn


def test_c1s_across_reads_the_tagged_rows_first():
    world = world_of()
    assert SC.row_band(world, "cup1") == 0 and SC.row_band(world, "cup2") == 2
    tag(world, "cup1", R.ROW, (R.BELOW,))                # the perceived row wins
    assert SC.row_band(world, "cup1") == 2 and not SC.across(world, "cup1", "cup2")
    tag(world, "cup2", R.ROW, (R.ABOVE,))
    assert SC.across(world, "cup1", "cup2")


# ---- the signature and the re-cast (D-093) ------------------------------------------------


def test_the_signature_is_read_from_standing_tags_and_a_discarded_role_is_never_recast():
    world = world_of()
    g = Group(members=list(HENRY_ROW), kind=GESTALT)
    g.positions = {"cup1": ["middle", "right"]}
    world.workspace.add_group(g)
    assert R.signature_of(world, "cup1") == ()
    tag(world, "cup1", R.MEMBER)
    anchor = tag(world, "cup1", R.ANCHOR)
    tag(world, "cup1", R.ROW, (R.ABOVE,))                # the tiers' criteria, not the signature
    tag(world, "knife1", R.SIDEKICK, ("cup1",))
    assert R.signature_of(world, "cup1") == (
        "anchor", "group:liquid holder,silverware,silverware,silverware,silverware,silverware",
        "member", "middle", "sidekick:silverware")
    world.workspace.discard_role(anchor, R.BROKEN, "test")
    assert "anchor" not in R.signature_of(world, "cup1")
    assert R.plays(("anchor", "member"), ("member",)) and not R.plays(("member",), ("anchor", "member"))
    assert R.plays((), ()) and R.plays(("x",), ())


def test_the_recast_needs_both_rows_and_places_every_candidate_in_its_best_tier():
    world = world_of()
    assert R.recast(world) is None                                  # the touched row unread
    tag(world, "cup1", R.ROW, (R.ABOVE,))
    assert R.recast(world) is None                                  # the other row unread
    rows_of_surround_5(world)
    tag(world, "cup2", R.TWIN, ("cup1",))                           # item 09: the third tier's tag
    tag(world, "cup1", R.ANCHOR)
    tag(world, "small-glass1", R.ANCHOR)
    tag(world, "small-glass1", R.CONNECTOR)                         # more roles still play
    rc = R.recast(world)
    tiers = {c: t for t, c, _ in rc.tiers}
    assert tiers["small-glass1"] == 2                               # the signature, left column
    assert tiers["cup2"] == 3                                       # the twin (D-092)
    assert {tiers[c] for c in ELIZA_ROW if c != "small-glass1"} == {4}
    assert set(tiers) == set(ELIZA_ROW) | {"cup2"} and "cup1" not in tiers
    weights = {c: w for _, c, w in rc.tiers}
    assert weights["small-glass1"] == 4.0 and weights["cup2"] == 2.0 and weights["fork3"] == 1.0
    assert rc.weight_of("fork3") == 1.0 and rc.weight_of("cup1") == 0.0
    assert rc.first_tier() == ["small-glass1"] and not rc.ambiguous
    assert [t for t, _, _ in rc.tiers] == sorted(t for t, _, _ in rc.tiers)
    # Tier 1: the image under the axis that plays the signature.
    tag(world, R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.MIRROR,))
    tag(world, "cup1", R.MIRROR_IMAGE, ("small-glass1",))
    rc = R.recast(world)
    assert rc.first_tier() == ["small-glass1"] and {c: t for t, c, _ in rc.tiers}["small-glass1"] == 1
    assert rc.weight_of("small-glass1") == 8.0
    # Under a point axis the mirror image is no image of the axis's kind.
    for t in world.workspace.roles_of(R.WORLDVIEW_ITEM):
        world.workspace.discard_role(t, R.BROKEN, "test")
    tag(world, R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.POINT,))
    assert {c: t for t, c, _ in R.recast(world).tiers}["small-glass1"] == 2


def test_a_tie_is_published_with_equal_weights_and_never_resolved():
    world = world_of("buridan-01")
    for o in world.problem.objects:
        region = G.exact_region(o.x, o.y)
        tag(world, o.id, R.ROW, (R.ABOVE if region.row == 0 else R.BELOW,))
    tag(world, "spoon2", R.TWIN, ("spoon1",))             # item 09: the third tier's tag
    rc = R.recast(world)                                 # spoon1: no signature, middle column
    tiers = {c: t for t, c, _ in rc.tiers}
    assert tiers == {"spoon2": 3, "small-glass1": 4, "small-glass2": 4, "cup3": 4, "cup4": 4, "fork1": 4}
    assert rc.first_tier() == ["spoon2"] and not rc.ambiguous
    tag(world, "spoon2", R.ANCHOR)                       # now spoon2 plays more than spoon1 asks:
    assert not R.recast(world).ambiguous                 # still the same type, tier 3
    world2 = world_of("buridan-01")
    for o in world2.problem.objects:
        region = G.exact_region(o.x, o.y)
        tag(world2, o.id, R.ROW, (R.ABOVE if region.row == 0 else R.BELOW,))
    for o in ("spoon2",):
        world2.workspace.discard_role(world2.workspace.role_tag(o, R.ROW, (R.BELOW,)), R.BROKEN, "test")
    rc2 = R.recast(world2)                               # five candidates, all in tier 4: a tie
    assert rc2.ambiguous and len(rc2.first_tier()) == 5
    assert len({w for _, _, w in rc2.tiers}) == 1


# ---- the recast-role codelet (D-081, D-093) -----------------------------------------------


def test_recast_role_fizzles_with_nothing_read_and_publishes_tags_and_proposals_otherwise():
    world = world_of()
    log = EV.Collector()
    world.events.observers.append(log)
    before = (world.rng.draws, world.slipnet.activations())
    out = run_type(world, "recast-role")
    assert out.fizzled and out.posted == [] and world.workspace.recast is None
    assert before == (world.rng.draws, world.slipnet.activations()) and not log.of_type(EV.RoleRecast)
    rows_of_surround_5(world)
    tag(world, R.WORLDVIEW_ITEM, R.SYMMETRY_AXIS, (G.MIRROR,))
    tag(world, "cup1", R.MIRROR_IMAGE, ("small-glass1",))
    tag(world, "cup2", R.TWIN, ("cup1",))                 # item 09: the third tier's tag
    out = run_type(world, "recast-role")
    assert out.action == "proposed" and [c.type for c in out.posted] == ["create-cp", "check-roles"]
    assert tuple(out.posted[0].args) == ("cup1", "small-glass1")
    [published] = log.of_type(EV.RoleRecast)
    assert published.touched == "cup1" and not published.ambiguous
    assert world.workspace.recast.tiers == published.tiers and world.workspace.recast.clock == world.clock
    counterparts = {t.item: t.args for t in world.workspace.roles if t.role == R.COUNTERPART}
    assert counterparts["small-glass1"] == ("cup1", "tier1") and counterparts["cup2"] == ("cup1", "tier3")
    assert counterparts["fork3"] == ("cup1", "tier2")      # an empty signature: the column decides
    assert counterparts["knife2"] == ("cup1", "tier4") and "cup1" not in counterparts
    used = {(e.item, e.role, e.consumer) for e in log.of_type(EV.RoleConsumed)}
    assert ("cup1", R.ROW, "C2") in used and ("cup1", R.COLUMN, "C2") in used
    assert ("small-glass1", R.COUNTERPART, "C1") in used
    s = SN.snapshot(world)
    assert s.recast["touched"] == "cup1" and s.recast["tiers"] == published.tiers
    assert json.dumps(dataclasses.asdict(s))
    # The axis falls: the glass drops to tier 2 and its tier-1 tag is broken, not kept.
    for t in world.workspace.roles_of(R.WORLDVIEW_ITEM):
        world.workspace.discard_role(t, R.BROKEN, "test")
    out = run_type(world, "recast-role")
    assert out.action == "found" and out.posted == []
    glass = [t for t in world.workspace.roles_of("small-glass1") if t.role == R.COUNTERPART]
    assert [t.args for t in glass] == [("cup1", "tier2")]
    dropped = [e for e in log.of_type(EV.RoleDiscarded) if e.role == R.COUNTERPART]
    assert [(e.item, e.reason) for e in dropped] == [("small-glass1", R.BROKEN)]
    assert "RL recast spoon1 [" in " ".join(trace_lines(load_problem("buridan-01"), CFG, 1))


# ---- consumer C2 (D-096) ---------------------------------------------------------------------


def test_c2_with_no_counterpart_tag_is_exactly_the_choice_by_salience():
    picks = []
    for lesion in (False, True):
        world = world_of()
        if lesion:
            with RP.lesioned("C2"):
                out = run_type(world, "pick-salient-obj", a=1.0)
        else:
            out = run_type(world, "pick-salient-obj", a=1.0)
        picks.append((out.detail, world.rng.draws))
    assert picks[0] == picks[1]
    world = world_of()
    assert SC.recast_weight(world, "cup2") == 0.0


def test_c2_weights_the_pick_by_the_tier_and_records_the_consumption():
    counts = {True: 0, False: 0}
    for seed in range(40):
        for tagged in (False, True):
            world = world_of(seed=seed)
            if tagged:
                tag(world, "small-glass1", R.COUNTERPART, ("cup1", "tier1"))
                assert SC.recast_weight(world, "small-glass1") == 8.0
            log = EV.Collector()
            world.events.observers.append(log)
            out = run_type(world, "pick-salient-obj", a=1.0)
            if out.detail.split()[0] == "small-glass1":
                counts[tagged] += 1
                used = [(e.role, e.consumer) for e in log.of_type(EV.RoleConsumed)]
                assert used == ([(R.COUNTERPART, "C2")] if tagged else [])
    assert counts[True] >= 2 * max(1, counts[False]) and counts[True] >= 8   # nine times the weight
    with pytest.raises(ValueError):
        dataclasses.replace(V2, role_recast_tiers=(8.0, 4.0, 0.0, 1.0))
    with pytest.raises(ValueError):
        dataclasses.replace(V2, role_recast_tiers=(-8.0, 4.0, 2.0, 1.0))
    assert len(CFG.role_recast_tiers) == len(R.TIERS)


# ---- the lesions (D-104) and a whole run ------------------------------------------------------


def test_the_lesions_are_one_patch_each():
    world = world_of()
    rows_of_surround_5(world)
    with RP.lesioned("K1"):
        assert R.recast(world) is None and run_type(world, "recast-role").fizzled
    assert R.recast(world) is not None
    with RP.lesioned("L2"):
        assert DS.column_role(world, "cup1") == [] and R.COLUMN not in [r for r, *_ in R.detect(world, "cup1")]
    assert R.COLUMN in [r for r, *_ in R.detect(world, "cup1")]


def test_a_run_reads_rows_columns_and_counterparts_and_the_recast_is_in_the_trace():
    log = EV.Collector()
    # Seed 1 since item 09's runs (seed 4 reads no row of the touched spoon before the answer).
    result = run_problem(load_problem("buridan-01"), CFG, 1, [log])
    roles = {e.role for e in log.of_type(EV.RoleTagged)}
    assert {R.ROW, R.COLUMN, R.COUNTERPART} <= roles
    recasts = log.of_type(EV.RoleRecast)
    assert recasts and all(e.touched == "spoon1" for e in recasts)
    for e in recasts:
        record = json.loads(json.dumps(EV.record_of(e)))
        assert record["tiers"] and all(len(t) == 3 for t in record["tiers"])
        assert all(t[0] in R.TIERS for t in record["tiers"])
    assert any(e.consumer == "C2" for e in log.of_type(EV.RoleConsumed))     # the rows re-cast
    assert result.answer in load_problem("buridan-01").ids()

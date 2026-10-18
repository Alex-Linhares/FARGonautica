"""BARRIER and CLAIMED, the Worldview roles of French's Blockage family, with consumer C4
(roles loop, item 10; D-089, D-101): the obvious counterparts read off the re-cast and the
Slipnet's distances; the two roles read off a Worldview correspondence that claims one;
read-worldview-roles tagging the barrier first, the claimed object only while the barrier
stands, jolting `barrier` and dropping stale tags; check-worldview posting one
pick-strong-cp per barrier's correspondence and nothing with an empty pool (nothing is
gated); pick-strong-cp with an argument fighting the challengers or coming back; the
`blocked` record of the default answer and its consumption at find-answer; the re-cast's
last tier; the two lesions as one patch each; whole runs.
"""

from unittest import mock

import role_probes as RP
from tabletop import answer as AN
from tabletop import correspondences as C
from tabletop import events as EV
from tabletop import geometry as G
from tabletop import invariants as IV
from tabletop import roles as R
from tabletop import worldview as WV
from tabletop.codelets import dispatch
from tabletop.codelets import judges as J
from tabletop.coderack import make_codelet
from tabletop.config import PROFILES
from tabletop.groups import GESTALT, Group
from tabletop.problems import load_problem
from tabletop.run import run_problem, trace_lines
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]


def tag(world, item, role, args=(), weight=1):
    return R.propose(world, item, role, args, weight, "by hand", "test", 1.0)


def world_of(name="blockage-06", rows=True):
    """Every object noticed, the midline perceived, and, with `rows`, every object tagged
    with its exact row and column, as the detectors read them without the blur."""
    world = World(load_problem(name), CFG, 1)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    tag(world, R.TABLE, R.MIDLINE)
    if rows:
        for o in world.problem.objects:
            region = G.exact_region(o.x, o.y)
            tag(world, o.id, R.ROW, (R.ABOVE if region.row == 0 else R.BELOW,))
            tag(world, o.id, R.COLUMN, (G.COLUMNS[region.column],))
    return world


def run_type(world, name, *args, a=6.0):
    return dispatch.run_codelet(world, make_codelet(name, CFG, 0, args=tuple(args)), a)


def claim(world, a="small-glass2", b="small-glass1"):
    """A glass-to-glass correspondence in the Worldview, French's blocker [F95 p. 33]."""
    cp = C.create(world, a, b)
    WV.admit(world, cp)
    return cp


def roles_on(world, item):
    return [(t.role, tuple(t.args)) for t in world.workspace.roles_of(item)]


# ---- the detector (D-089) ----------------------------------------------------------------


def test_the_obvious_counterparts_are_the_recasts_nearest_candidates_and_need_the_rows():
    assert WV.obvious_counterparts(world_of(rows=False)) == []        # no re-cast: nothing
    assert sorted(WV.obvious_counterparts(world_of())) == ["small-glass1", "small-glass3"]
    world = world_of("blockage-07")                                     # the silverware is far
    assert sorted(WV.obvious_counterparts(world)) == ["small-glass1", "small-glass3"]
    assert "fork2" in R.recast(world).first_tier()                      # in the tier, not nearest


def test_blockers_and_the_detector_read_barrier_and_claimed_off_the_worldview():
    world = world_of()
    assert WV.blockers(world) == [] and WV.barrier_and_claimed(world, "small-glass2") == []
    own = claim(world, "cup1", "small-glass3")                         # the touched object's own
    assert WV.blockers(world) == []
    cp = claim(world)
    assert WV.blockers(world) == [("small-glass2", "small-glass1", cp)]
    assert WV.barrier_and_claimed(world, "small-glass2") == [
        (R.BARRIER, ("small-glass1", cp.name), 1,
         f"{cp.label()} claims small-glass1, a counterpart of cup1")]
    assert WV.barrier_and_claimed(world, "small-glass1") == []        # the barrier not yet tagged
    tag(world, "small-glass2", R.BARRIER, ("small-glass1", cp.name))
    assert WV.barrier_and_claimed(world, "small-glass1") == [
        (R.CLAIMED, ("small-glass2", cp.name), 1, f"claimed by small-glass2 through {cp.label()}")]
    assert [r for r, *_ in R.detect(world, "small-glass1") if r == R.CLAIMED] == [R.CLAIMED]
    for item in (R.TABLE, R.WORLDVIEW_ITEM, "cup1", "small-glass3", "small-glass4"):
        assert WV.barrier_and_claimed(world, item) == []
    assert WV.blocked_items(world) == ["small-glass2", "small-glass1"]
    assert [r for r, *_ in R.detect(world, "small-glass2") if r == R.BARRIER] == [R.BARRIER]
    world.context = object()                                          # inside a subworld: nothing
    assert WV.blockers(world) == [] and WV.barrier_and_claimed(world, "small-glass2") == []
    world.context = None
    WV.expel(world, [cp, own])
    assert WV.blockers(world) == []


def test_a_group_mapped_as_a_whole_claims_the_counterpart_inside_it_with_its_weight():
    world = world_of()
    henry = world.workspace.add_group(Group(members=["small-glass2", "small-glass4"], kind=GESTALT))
    eliza = world.workspace.add_group(Group(members=["small-glass1", "small-glass3"], kind=GESTALT))
    cp = claim(world, henry, eliza)
    assert WV.blockers(world) == [(henry, "small-glass1", cp), (henry, "small-glass3", cp)]
    read = WV.barrier_and_claimed(world, henry)
    assert [(r, a, w) for r, a, w, _ in read] == [(R.BARRIER, ("small-glass1", cp.name), 2),
                                                  (R.BARRIER, ("small-glass3", cp.name), 2)]
    tag(world, henry, R.BARRIER, ("small-glass3", cp.name), 2)
    assert WV.barrier_and_claimed(world, "small-glass3") == [
        (R.CLAIMED, (henry, cp.name), 1, f"claimed by {henry} through {cp.label()}")]
    assert WV.barrier_and_claimed(world, "small-glass1") == []        # its own barrier tag unread
    assert WV.blocked_items(world) == [henry, "small-glass1", "small-glass3"]


# ---- read-worldview-roles (D-081, D-089) ---------------------------------------------------


def test_read_worldview_roles_tags_the_barrier_first_jolts_its_node_and_drops_stale_tags():
    world = world_of()
    assert run_type(world, "read-worldview-roles").fizzled           # nothing to read
    cp = claim(world)
    before = world.slipnet.activation_of("barrier")
    out = run_type(world, "read-worldview-roles")
    assert out.action == "found" and "BARRIER:small-glass1 cp1" in out.detail
    assert [(c.type, tuple(c.args)) for c in out.posted] == [("pick-strong-cp", (cp.name,))]
    assert world.slipnet.activation_of("barrier") > before
    tags = [t for t in world.workspace.roles if t.role in (R.BARRIER, R.CLAIMED)]
    assert [(t.item, t.role) for t in tags] == [("small-glass2", R.BARRIER), ("small-glass1", R.CLAIMED)]
    assert tags[0].weight == 1 and tags[0].evidence.startswith(cp.label())
    out = run_type(world, "read-worldview-roles")                     # the same reading: re-rated
    assert out.action == "found" and len(world.workspace.roles_of("small-glass1")) == 3
    WV.expel(world, [cp])
    out = run_type(world, "read-worldview-roles")
    assert out.action == "removed" and "BARRIER" in out.detail and "CLAIMED" in out.detail
    assert not [t for t in world.workspace.roles if t.role in (R.BARRIER, R.CLAIMED)]


def test_the_claimed_tag_is_proposed_only_while_the_barriers_tag_stands():
    world = world_of()
    claim(world)
    engine_propose = R.propose

    def barrier_loses(world_, item, role, *rest):
        return None if role == R.BARRIER else engine_propose(world_, item, role, *rest)

    with mock.patch("tabletop.roles.propose", barrier_loses):
        out = run_type(world, "read-worldview-roles")
    assert out.action == "found" and "BARRIER" not in out.detail and "CLAIMED" not in out.detail
    assert not [t for t in world.workspace.roles if t.role in (R.BARRIER, R.CLAIMED)]


# ---- consumer C4 (D-101) -------------------------------------------------------------------


def test_check_worldview_posts_one_pick_strong_cp_per_barrier_and_nothing_with_an_empty_pool():
    world = world_of()
    draws = world.rng.draws
    assert run_type(world, "check-worldview").fizzled                 # nothing is gated
    assert J.barrier_fights(world, make_codelet("check-worldview", CFG, 0)) == []
    cp = claim(world)
    run_type(world, "read-worldview-roles")
    assert world.rng.draws == draws
    log = EV.Collector()
    world.events.observers.append(log)
    out = run_type(world, "check-worldview")
    assert out.action == "found" and out.detail == f"barrier fights {cp.name}"
    assert [(c.type, tuple(c.args)) for c in out.posted] == [("pick-strong-cp", (cp.name,))]
    used = log.of_type(EV.RoleConsumed)
    assert [(e.role, e.consumer, e.item, e.effect) for e in used] == [
        (R.BARRIER, R.C4, "small-glass2", f"pick-strong-cp {cp.name}")]
    tag(world, "small-glass4", R.BARRIER, ("small-glass3", cp.name))    # the same correspondence
    assert len(J.barrier_fights(world, make_codelet("check-worldview", CFG, 0))) == 1
    with RP.lesioned("C4"):
        assert run_type(world, "check-worldview").fizzled


def test_pick_strong_cp_with_an_argument_fights_the_challengers_or_comes_back():
    world = world_of()
    cp = claim(world)
    assert J.challengers_of(world, cp) == []
    assert run_type(world, "pick-strong-cp", cp.name).detail == f"no challenger of {cp.name}"
    assert run_type(world, "pick-strong-cp", "cp9").detail == "cp9 gone"
    rival = C.create(world, "cup1", "small-glass1")                   # the same end: a rival
    other = C.create(world, "cup1", "small-glass3")                   # no conflict with cp
    assert J.challengers_of(world, cp) == [rival]
    out = run_type(world, "pick-strong-cp", cp.name)
    assert out.action in ("passed", "failed") and rival.name in out.detail
    WV.expel(world, [cp])
    assert J.challengers_of(world, cp) == [cp]                        # back to fight
    out = run_type(world, "pick-strong-cp", cp.name)
    assert cp.name in out.detail and other.name not in out.detail


def test_the_default_answer_is_recorded_blocked_only_while_a_claimed_tag_stands():
    world = world_of()
    assert AN.claimed_counterpart(world) is None
    assert AN.answer_for(world).through == AN.DEFAULT
    cp = claim(world)
    tag(world, "small-glass1", R.CLAIMED, ("small-glass2", cp.name))
    answer = AN.answer_for(world)
    assert answer.through == AN.BLOCKED and answer.object == "cup1" and answer.literal
    assert answer.label == "E3" and answer.note == f"small-glass1 is claimed by small-glass2 through {cp.name}"
    log = EV.Collector()
    world.events.observers.append(log)
    out = run_type(world, "find-answer")
    assert out.answer.through == AN.BLOCKED and "claimed by small-glass2" in out.detail
    assert [(e.role, e.consumer, e.item) for e in log.of_type(EV.RoleConsumed)] == [
        (R.CLAIMED, R.C4, "small-glass1")]
    own = claim(world, "cup1", "small-glass3")
    assert AN.answer_for(world).through == AN.CORRESPONDENCE         # the record alone differs
    WV.expel(world, [own])
    world.workspace.discard_role(world.workspace.role_tag("small-glass1", R.CLAIMED,
                                                          ("small-glass2", cp.name)), R.BROKEN)
    assert AN.answer_for(world).through == AN.DEFAULT


# ---- the re-cast's last tier (D-093 revised, D-101) ------------------------------------------


def test_the_recast_puts_a_claimed_candidate_last():
    world = world_of()
    assert R.TIERS == (1, 2, 3, 4, 5) and R.TIER_WORDS[5] == "tier5" and R.CLAIMED_TIER == 5
    assert len(CFG.role_recast_tiers) == 5
    before = {c: t for t, c, _ in R.recast(world).tiers}
    assert before == {"small-glass1": 2, "small-glass3": 2}          # the empty signature, left column
    tag(world, "small-glass1", R.CLAIMED, ("small-glass2", "cp1"))
    rc = R.recast(world)
    assert {c: t for t, c, _ in rc.tiers} == {"small-glass3": 2, "small-glass1": 5}
    assert rc.weight_of("small-glass1") == 0.5 and rc.first_tier() == ["small-glass3"]
    assert R.natural_first_tier(world) == ["small-glass1", "small-glass3"]   # the claim left out
    assert sorted(WV.obvious_counterparts(world)) == ["small-glass1", "small-glass3"]   # still obvious


# ---- the lesions (D-104) -------------------------------------------------------------------


def test_the_lesions_are_one_patch_each():
    world = world_of()
    claim(world)
    assert WV.barrier_and_claimed(world, "small-glass2")
    with RP.lesioned("S5"):
        assert WV.barrier_and_claimed(world, "small-glass2") == []
        assert "BARRIER" not in run_type(world, "read-worldview-roles").detail
    assert WV.barrier_and_claimed(world, "small-glass2")


# ---- whole runs -----------------------------------------------------------------------------


def test_discarding_a_barrier_discards_the_claimed_tag_that_names_it():
    """D-119 (v5): a CLAIMED tag stands only while its BARRIER does (D-089), so the pool
    cascades the discard, whatever discards the barrier (check-roles breaking it, a
    SIDEKICK beating it, read-worldview-roles dropping it): no CLAIMED is left orphaned,
    the invariant `check_roles` reads. A CLAIMED naming another barrier stays."""
    world = world_of()
    cp = claim(world)                                   # small-glass2=small-glass1, cp1
    barrier = tag(world, "small-glass2", R.BARRIER, ("small-glass1", cp.name), 1)
    claimed = tag(world, "small-glass1", R.CLAIMED, ("small-glass2", cp.name), 1)
    other_cp = claim(world, "small-glass4", "small-glass3")
    other_barrier = tag(world, "small-glass4", R.BARRIER, ("small-glass3", other_cp.name), 1)
    other_claimed = tag(world, "small-glass3", R.CLAIMED, ("small-glass4", other_cp.name), 1)
    assert all(t is not None for t in (barrier, claimed, other_barrier, other_claimed))
    log = EV.Collector()
    world.events.observers.append(log)
    world.workspace.discard_role(barrier, R.BROKEN, "check-roles")
    assert not world.workspace.has_role("small-glass1", R.CLAIMED)
    assert world.workspace.has_role("small-glass3", R.CLAIMED)           # the other claim stands
    assert world.workspace.has_role("small-glass4", R.BARRIER)
    dropped = [(e.tag, e.role, e.reason, e.by) for e in log.of_type(EV.RoleDiscarded)]
    assert dropped == [(barrier.name, R.BARRIER, R.BROKEN, "check-roles"),
                       (claimed.name, R.CLAIMED, R.BROKEN, "check-roles")]
    assert IV.check_roles(world) == []
    # The other way round nothing cascades: a CLAIMED discarded alone leaves its barrier,
    # which read-worldview-roles proposes the claim from again (D-089).
    world.workspace.discard_role(other_claimed, R.BROKEN, "check-roles")
    assert world.workspace.has_role("small-glass4", R.BARRIER)
    assert IV.check_roles(world) == []
    # A barrier whose correspondence left the Worldview is not broken by the expulsion
    # itself (D-101: it keeps posting the fight for the correspondence that lost its
    # place, until read-worldview-roles re-reads); the cascade is the discard's alone.
    WV.expel(world, [other_cp])
    assert world.workspace.has_role("small-glass4", R.BARRIER)


def test_blockage_2_seed_2_shows_no_bonus_ratchet_under_the_default():
    """The replay the study of v5 asked for (RESULTS.md, v5; D-116, D-117): Blockage 2
    seed 2 under `standard`, where v4's bonus ratchet took the cup's correspondence from
    -12 to -66 over ten rematches of one rivalry and the chance of its return to 0.000.
    Now every bonus stays within one step of each outcome per rivalry, and no
    correspondence's bonus falls below two penalties over the whole run. Re-pin after an
    engine change; the answer itself is a coin and is not asserted."""
    world = World(load_problem("blockage-02"), PROFILES["standard"], 2)
    cfg = world.config
    lowest, highest = 0.0, 0.0
    while not world.finished:
        world.step()
        for cp in world.workspace.correspondences:
            lowest, highest = min(lowest, cp.bonus), max(highest, cp.bonus)
    assert lowest >= -2 * cfg.loss_penalty and highest <= 2 * cfg.win_bonus
    rematches = len([c for c in world.workspace.competitions if c.rivals])
    assert rematches >= 3                                   # the same pair fought again and again
    assert len(world.workspace.awarded) <= 4                # one rivalry: two sides x two outcomes at most
    assert not world.capped


def test_a_run_reads_a_barrier_keeps_it_fighting_and_may_record_a_blocked_answer():
    # blockage-07 seed 104 since v5's fix (D-116 to D-120; seed 16 through v4): the far
    # glass claims Eliza's near glass through cp6 and the answer is blocked.
    log = EV.Collector()
    result = run_problem(load_problem("blockage-07"), CFG, 104, [log])
    tagged = [(e.item, e.role, tuple(e.args)) for e in log.of_type(EV.RoleTagged)
              if e.role in (R.BARRIER, R.CLAIMED)]
    assert tagged[:2] == [("small-glass2", R.BARRIER, ("small-glass1", "cp6")),
                          ("small-glass1", R.CLAIMED, ("small-glass2", "cp6"))]
    used = [(e.role, e.consumer, e.effect) for e in log.of_type(EV.RoleConsumed)
            if e.role in (R.BARRIER, R.CLAIMED)]
    assert (R.CLAIMED, R.C4, "the answer recorded as blocked") in used
    given = log.of_type(EV.AnswerGiven)[0]
    assert given.through == AN.BLOCKED and given.answer == "cup1" and result.label == "E3"
    lines = trace_lines(load_problem("blockage-07"), CFG, 104)
    assert any("RL tag" in line and "BARRIER:small-glass1 cp6" in line for line in lines)
    assert any("FND-ANS" in line and "claimed by small-glass2" in line for line in lines)
    assert (R.BARRIER, R.C4, "pick-strong-cp cp6") in used            # C4's posting, at the read
    log = EV.Collector()
    result = run_problem(load_problem("blockage-06"), CFG, 21, [log])
    used = [(e.role, e.consumer, e.effect) for e in log.of_type(EV.RoleConsumed)]
    assert used.count((R.BARRIER, R.C4, "pick-strong-cp cp5")) == 2   # at the read and by check-worldview
    assert result.answer == "small-glass1"                            # the claim did not hold

"""The invariants of `tabletop.invariants` (D-077): a checked run is the same run, the
engine keeps every invariant on the catalogue and on random tables, and a planted fault
is caught."""

import re
import pytest

from tabletop import correspondences as C
from tabletop import events as EV
from tabletop import invariants as IV
from tabletop import observe as OB
from tabletop import roles as RL
from tabletop import tables as T
from tabletop.coderack import make_codelet
from tabletop.config import PROFILES
from tabletop.correspondences import Correspondence
from tabletop.groups import Group
from tabletop.problems import catalogue, load_problem
from tabletop.run import fingerprint, run_problem
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
FAST_PROBLEMS = ("surround-05", "surround-10", "blockage-06", "blockage-10", "buridan-01",
                 "buridan-12", "figure-ground-01", "ch2-fig11")


# The one defect v3's defaults expose (RESULTS.md §12, reported, not fixed): check-roles
# breaks a BARRIER and leaves the CLAIMED tag naming it standing until it is re-read.
KNOWN = re.compile(r"r\d+ is CLAIMED without a BARRIER")


def no_faults(problem, config, seed, known=False):
    """A checked run with no violation; with `known`, the orphan CLAIMED tags of RESULTS.md
    §12 are let through and every other violation still fails."""
    result, faults = IV.checked_run(problem, config, seed)
    if known:
        faults = [f for f in faults if not KNOWN.fullmatch(f[1])]
    assert faults == [], (problem.name, config.profile, seed, faults[:3])
    return result


# ---- the check changes nothing ----------------------------------------------------------

def test_a_fresh_world_keeps_every_invariant_and_the_check_draws_nothing():
    world = World(load_problem("surround-05"), CFG, 1)
    draws = world.rng.draws
    assert IV.check(world) == []
    assert world.rng.draws == draws


def test_a_checked_run_is_the_same_run_with_the_same_fingerprint():
    for name, seed in (("surround-05", 1), ("blockage-06", 2), ("buridan-01", 3)):
        problem = load_problem(name)
        hasher = OB.Hasher()
        result, faults = IV.checked_run(problem, PROFILES["standard"], seed, [hasher])
        assert faults == []
        assert result == run_problem(problem, PROFILES["standard"], seed)
        assert (hasher.digest(), result.draws) == fingerprint(name, seed)


# ---- the engine keeps them: a fast sample, and a slow survey ----------------------------

def test_the_catalogue_sample_keeps_every_invariant_under_every_profile():
    for i, name in enumerate(FAST_PROBLEMS):
        problem = load_problem(name)
        for profile in PROFILES.values():
            no_faults(problem, profile, i + 1)


def test_random_tables_keep_every_invariant():
    # five tables here; twenty and five under french-faults in tests_slow (ANSWERS.md D2)
    for seed in range(1, 6):
        result = no_faults(T.random_table(seed), CFG, seed)
        assert not result.capped
    no_faults(T.random_table(21), V2_PROFILES["french-faults"], 21)


# ---- planted faults are caught ----------------------------------------------------------

def finished_world(seed: int = 1) -> World:
    world = World(load_problem("surround-05"), CFG, seed)
    while not world.finished:
        world.step()
    assert IV.check(world) == []
    return world


def test_an_activation_out_of_range_is_caught():
    world = World(load_problem("surround-05"), CFG, 1)
    world.slipnet.activation[0] = 200
    assert any("outside [1, 150]" in f for f in IV.check_slipnet(world))


def test_a_coderack_over_capacity_or_with_a_stale_codelet_is_caught():
    world = World(load_problem("surround-05"), CFG, 1)
    for _ in range(CFG.coderack_capacity + 1 - len(world.coderack)):
        world.coderack.codelets.append(make_codelet("find-group", CFG, 0))
    assert any("over its capacity" in f for f in IV.check_coderack(world))
    world = World(load_problem("surround-05"), CFG, 1)
    world.coderack.codelets[0].posted_at = -100
    assert any("codelets old" in f for f in IV.check_coderack(world))


def test_a_temperature_out_of_range_is_caught():
    world = World(load_problem("surround-05"), CFG, 1)
    world.temperature.value = 150.0
    assert IV.check_temperature(world) == ["T is 150.0"]


def test_ghost_descriptions_groups_and_correspondences_are_caught():
    world = finished_world()
    ws = world.workspace
    ws.descriptions["g99"] = ["end"]
    assert any("g99 is described" in f for f in IV.check_objects(world))
    del ws.descriptions["g99"]
    ws.groups.append(Group(members=["cup1"], name="g98"))
    assert any("g98 has members" in f for f in IV.check_groups(world))
    ws.groups.pop()
    ws.correspondences.append(Correspondence("cup1", "g99", name="cp98"))
    assert any("cp98 ends on g99" in f for f in IV.check_correspondences(world))
    ws.correspondences.pop()
    assert IV.check(world) == []


def test_two_worldview_correspondences_on_one_end_are_caught():
    world = World(load_problem("surround-05"), CFG, 1)
    for other in ("cup2", "small-glass1"):
        world.workspace.notice(other, world.rng)
        cp = C.create(world, world.problem.touched, other)
        assert cp is not None
        cp.in_worldview = True
    assert any("conflicts with" in f for f in IV.check_worldview(world))


# ---- the role pool's invariants (D-077 revised; D-079, D-089) ----------------------------

def test_role_tags_on_gone_structures_rivals_and_a_claim_without_its_barrier_are_caught():
    world = finished_world()
    ws, touched = world.workspace, world.problem.touched
    assert IV.check_roles(world) == []
    gone = RL.RoleTag("g99", RL.CORNER, name="r900", strength=50.0)
    ws.roles.append(gone)
    assert any("stands on g99, which does not exist" in f for f in IV.check_roles(world))
    ws.roles.remove(gone)
    naming = RL.RoleTag(touched, RL.BARRIER, ("cup2", "cp99"), name="r901", strength=50.0)
    ws.roles.append(naming)
    assert any("names cp99, which does not stand" in f for f in IV.check_roles(world))
    ws.roles.remove(naming)
    claimed = RL.RoleTag("cup2", RL.CLAIMED, ("cup3", "cp1"), name="r902", strength=50.0)
    ws.roles.append(claimed)
    faults = IV.check_roles(world)
    assert any("CLAIMED without a BARRIER" in f for f in faults)
    assert not any("cp1" in f for f in faults) or ws.correspondences == []
    ws.roles.remove(claimed)
    a = RL.RoleTag("cup2", RL.LONER, name="r903", strength=50.0)
    b = RL.RoleTag("cup2", RL.MEMBER, name="r904", strength=50.0)
    ws.roles += [a, b]
    assert any("are rivals on cup2" in f for f in IV.check_roles(world))
    ws.roles.remove(b)
    twin = RL.RoleTag("cup2", RL.LONER, name="r905", strength=50.0)
    ws.roles.append(twin)
    assert any("are one reading" in f for f in IV.check_roles(world))
    ws.roles.remove(twin)
    a.strength = 200.0
    assert any("has strength 200.0" in f for f in IV.check_roles(world))
    ws.roles.remove(a)
    other = RL.RoleTag("cup2", RL.COUNTERPART, ("cup2", "tier1"), name="r906", strength=50.0)
    ws.roles.append(other)
    assert any("not of the touched object" in f for f in IV.check_roles(world))
    ws.roles.remove(other)
    assert IV.check(world) == []


def test_a_run_that_reads_a_barrier_and_a_claim_keeps_the_role_invariants():
    # blockage-07 seed 104 reads a BARRIER and a CLAIMED tag (D-089; re-pin after an engine
    # change: seed 16 through v4, 104 since v5's fix, D-116 to D-120)
    log = EV.Collector()
    result, faults = IV.checked_run(load_problem("blockage-07"), CFG, 104, [log])
    assert faults == []
    roles = {e.role for e in log.of_type(EV.RoleTagged)}
    assert RL.BARRIER in roles and RL.CLAIMED in roles


def test_a_wrong_end_of_run_is_caught():
    world = finished_world()
    world.capped = True
    assert any("did not run to the cap" in f for f in IV.check_end(world))
    world.capped = False
    world.clock = CFG.max_codelets + 1
    assert any("over the cap" in f for f in IV.check_end(world))


def test_summary_counts_sentences():
    assert IV.summary([(1, "a"), (2, "a"), (3, "b")]) == {"a": 2, "b": 1}

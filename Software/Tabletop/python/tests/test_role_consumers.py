"""Consumer C1, role-guided proposals (D-095), and the test-only probes (D-104): an empty
pool posts nothing and changes no rating; a pair C1 names goes through create-cp, the test
and the competition unchanged (D-035, D-039 to D-041); the oracle probe hands in the
hand-read roles and the run is deterministic in two processes; the lesion patches one
function; the text trace prints `RL` lines.
"""

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import correspondences as C
from tabletop import events as EV
from tabletop import roles as R
from tabletop import worldview as WV
from tabletop.codelets import dispatch
from tabletop.codelets import scouts as SC
from tabletop.coderack import make_codelet
from tabletop.config import PROFILES
from tabletop.problems import load_problem
from tabletop.run import fingerprint, run_problem, trace_lines
from tabletop.world import World

import role_probes as RP
from v2_defaults import V2  # the mechanism at its decision's values (D-114, D-115)

CFG = PROFILES["standard"]
PYTHON_DIR = Path(__file__).resolve().parents[1]


def world_of(name="surround-05", seed=1):
    """A world with every object in view and the MIDLINE perceived (D-084: C1 proposes
    across a perceived midline only, D-095), tagged by hand so that the pool holds that
    one tag and nothing else."""
    world = World(load_problem(name), CFG, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    R.propose(world, R.TABLE, R.MIDLINE, (), 1, "by hand", "test", 1.0)
    return world


def c1(world, a=6.0):
    return dispatch.run_codelet(world, make_codelet("propose-role-cp", CFG, 0), a)


# ---- nothing is gated (D-095) -----------------------------------------------------------------


def test_c1_with_an_empty_pool_posts_nothing_and_changes_no_rating():
    world = world_of()
    cp = C.create(world, "cup1", "cup2")
    before = (cp.rating, world.rng.draws, len(world.workspace.correspondences),
              world.slipnet.activations())
    out = c1(world)
    assert out.fizzled and out.posted == [] and "no role pair" in out.detail
    assert before == (cp.rating, world.rng.draws, len(world.workspace.correspondences),
                      world.slipnet.activations())


def test_c1_needs_a_pair_across_the_table_that_a_correspondence_may_join():
    world = world_of()
    R.propose(world, "fork1", "CORNER", (), 1, "", "test", 1.0)     # Henry's row only
    assert c1(world).fizzled
    R.propose(world, "fork2", "CORNER", (), 1, "", "test", 1.0)     # same row: no pair
    assert c1(world).fizzled and SC.role_pairs(world) == []
    R.propose(world, "cup2", "CORNER", (), 1, "", "test", 1.0)      # Eliza's row
    pairs = SC.role_pairs(world)
    assert sorted((a, b) for _, _, a, b, _ in pairs) == [("fork1", "cup2"), ("fork2", "cup2")]
    assert all(w >= CFG.role_pair_floor for *_, w in pairs)
    C.create(world, "fork1", "cup2")
    assert [(a, b) for _, _, a, b, _ in SC.role_pairs(world)] == [("fork2", "cup2")]   # D-035: exists


# ---- the pair goes through the existing chain unchanged (D-035, D-039 to D-041) -------------


def test_c1_posts_one_create_cp_and_records_the_consumption():
    world = world_of()
    log = EV.Collector()
    world.events.observers.append(log)
    cup1 = R.propose(world, "cup1", "ANCHOR", (), 1, "", "test", 1.0)
    glass = R.propose(world, "small-glass1", "ANCHOR", (), 1, "", "test", 1.0)
    out = c1(world)
    assert out.action == "proposed" and [c.type for c in out.posted] == ["create-cp", "check-roles"]
    assert tuple(out.posted[0].args) == ("cup1", "small-glass1")
    used = log.of_type(EV.RoleConsumed)
    assert [(e.item, e.consumer) for e in used] == [("cup1", "C1"), ("small-glass1", "C1")]
    assert cup1.consumed == glass.consumed == [("C1", "create-cp cup1 small-glass1")]
    # The chain: create-cp creates and tests it as any pair (D-035, D-039), cp-compete fights it.
    created = dispatch.run_codelet(world, out.posted[0], 6.0)
    assert created.action in ("passed", "failed")
    cp = world.workspace.correspondence_between("cup1", "small-glass1")
    if created.action == "passed":
        assert cp is not None and cp.by == "create-cp" and cp.tested
        assert [c.type for c in created.posted] == ["cp-compete"]
        assert cp.static == C.static_points(world, cp)         # no bonus for its origin
        held = WV.compete(world, cp, 6.0)
        assert held is not None and cp.in_worldview
    else:
        assert cp is None       # removed by the failed test (D-039, `failed_test_removes`)


def test_c1_chooses_by_role_weight_under_the_temperature_law():
    world = world_of()
    R.propose(world, "cup1", "ANCHOR", (), 1, "", "test", 1.0)
    heavy = R.propose(world, "small-glass1", "ANCHOR", (), 1, "", "test", 1.0)
    light = R.propose(world, "knife2", "ANCHOR", (), 1, "", "test", 1.0)
    heavy.strength, light.strength = 90.0, 10.0
    choices = {}
    for seed in range(30):
        w = world_of(seed=seed)
        R.propose(w, "cup1", "ANCHOR", (), 1, "", "test", 1.0)
        h = R.propose(w, "small-glass1", "ANCHOR", (), 1, "", "test", 1.0)
        l = R.propose(w, "knife2", "ANCHOR", (), 1, "", "test", 1.0)
        h.strength, l.strength = 90.0, 10.0
        out = c1(w, a=6.0)
        choices[out.posted[0].args[1]] = choices.get(out.posted[0].args[1], 0) + 1
    assert choices.get("small-glass1", 0) > choices.get("knife2", 0)


def test_an_image_tag_names_its_partner():
    world = world_of("blockage-02")
    R.propose(world, "small-glass1", "MIRROR_IMAGE", ("cup1",), 1, "", "test", 1.0)
    pairs = SC.role_pairs(world)
    assert [(a, b) for _, _, a, b, _ in pairs] == [("small-glass1", "cup1")]
    out = c1(world)
    assert tuple(out.posted[0].args) == ("small-glass1", "cup1")


# ---- the probes (D-104) -------------------------------------------------------------------------


def test_the_oracle_hands_in_the_hand_read_roles_and_c1_reads_them():
    # On the v2 defaults (D-115): under v4's the plain run of seed 1 reads a CONTAINER (a
    # setting of six around small-glass1, item 06's detector) late in the run.
    problem = load_problem("surround-05")
    log = EV.Collector()
    with RP.oracle_roles("surround-05"):
        result = run_problem(problem, V2, 1, [log])
    tagged = log.of_type(EV.RoleTagged)
    assert tagged and all(e.by == "perceived" or e.by for e in tagged)
    assert {e.item for e in tagged} <= set(problem.ids()) | {R.TABLE, R.WORLDVIEW_ITEM}
    assert any(e.role == "LONER" and e.item == "cup2" for e in tagged)
    assert log.of_type(EV.RoleConsumed), "C1 read a tag in this run"
    assert result.answer in problem.ids()
    # Without the oracle the pool holds the engine's own readings alone: the roles whose
    # detectors items 04 to 08 built; the content roles of items 09 and 10 come later.
    plain = EV.Collector()
    run_problem(problem, V2, 1, [plain])
    own = {e.role for e in plain.of_type(EV.RoleTagged)}
    assert own <= {"MIDLINE", "MIRROR_IMAGE", "POINT_IMAGE", "SYMMETRY_AXIS", "MEMBER", "SIDEKICK",
                   "ANCHOR", "CONNECTOR", "ROW", "COLUMN", "COUNTERPART", "GAP", "CORNER", "LONER",
                   "STRAY", "FIGURE", "GROUND", "ECHO", "TWIN", "TYPE_KEY"}
    assert not any(e.role in ("CONTAINER", "BARRIER", "CLAIMED") for e in plain.of_type(EV.RoleTagged))


def test_the_oracle_file_names_registered_roles_on_real_objects():
    data = json.loads(RP.ORACLE.read_text())
    assert RP.oracle_problems() == ["surround-05", "blockage-06", "buridan-01", "blockage-02"]
    for name in RP.oracle_problems():
        ids = load_problem(name).ids()
        for row in data[name]:
            assert (row["item"] in ids or (row["item"], row["role"]) == (R.TABLE, R.MIDLINE))
            assert row["role"] in R.NAMES and row["weight"] >= 1
            assert all(a in ids or a in ("above", "below", "left", "middle", "right")
                       for a in row["args"])
            assert R.spec(row["role"]).reading != R.WORLDVIEW


def test_the_oracle_run_is_deterministic_in_two_processes():
    with RP.oracle_roles("blockage-02"):
        here = fingerprint("blockage-02", 2)
    code = ("import sys; sys.path.insert(0, 'tests'); import role_probes as RP\n"
            "from tabletop.run import fingerprint\n"
            "with RP.oracle_roles('blockage-02'):\n"
            "    print(*fingerprint('blockage-02', 2))\n")
    for hashseed in ("0", "97"):
        proc = subprocess.run([sys.executable, "-c", code], cwd=PYTHON_DIR, capture_output=True,
                              text=True, env={"PYTHONHASHSEED": hashseed, "PATH": "/usr/bin:/bin",
                                              **{k: v for k, v in os.environ.items()
                                                 if k == "TABLETOP_ENGINE"}})
        assert proc.returncode == 0, proc.stderr
        digest, draws = proc.stdout.split()
        assert (digest, int(draws)) == here


def test_the_lesion_patches_one_function_and_leaves_the_engine_switchless():
    assert set(RP.LESIONS) == {"C1", "C5", "C6", "C2", "K1", "L2", "C7", "S2", "L3", "L4", "L5",
                               "S5", "C4", "D1", "A2", "C3",
                               "A4", "A5", "A6", "A7", "K2"}
    world = world_of()
    R.propose(world, "cup1", "ANCHOR", (), 1, "", "test", 1.0)
    R.propose(world, "small-glass1", "ANCHOR", (), 1, "", "test", 1.0)
    with RP.lesioned("C1"):
        assert c1(world).fizzled
    assert c1(world).action == "proposed"
    with pytest.raises(KeyError):
        RP.lesioned("C9").__enter__()


def test_the_text_trace_prints_rl_lines_under_the_oracle():
    with RP.oracle_roles("blockage-02"):
        lines = trace_lines(load_problem("blockage-02"), CFG, 1)
    rl = [l.strip() for l in lines if l.strip().startswith("RL ")]
    assert rl and any(l.startswith("RL tag r1 ") for l in rl)
    assert all(l.split()[1] in ("tag", "drop", "used", "recast") for l in rl)

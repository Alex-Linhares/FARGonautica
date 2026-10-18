"""Loop0006 item 01: the tap and the snapshot history (`tabletop/gui/models/tap.py`,
`snapshot.py`), Qt-free (G-006, G-007).

- A run with the tap gives the same event records, draws and report as a plain run, on
  the four golden cases, and takes a snapshot at the end of the start, after every step
  and at the end.
- `state_at` agrees with a snapshot taken at that point: after every step, the snapshot
  in force at the step's last event equals `World.snapshot()` taken then.
- The snapshot is the engine's (`tabletop.snapshot.Snapshot`), and the tap keeps no
  engine object once the run has finished.
- The keyframe rule (G-007), forced on a short run by lowering the dense prefix.
- Known facts: `surround-05` seed 3 ends with `spoon3` (none), 59 codelets, T 43.6;
  seed 1 has 61 codelets and 779 events (TASK.md's facts as of v1, re-pinned at every
  engine change, last at v5's fix, D-116 to D-120; `cup2` (E2), 44 codelets, T 41.0 and
  62 codelets, 901 events at item 09 of the roles loop).
- The forced capped run of G-007's measurement (`slow`): 3000 codelets, far fewer
  snapshots than steps, the same run as the engine alone.
"""

import dataclasses

import pytest

from tabletop.config import PROFILES
from tabletop.events import Collector
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.snapshot import DENSE_CODELETS, KEYFRAME_STEPS, SnapshotHistory
from tabletop.gui.models.tap import Tap
from tabletop.problems import load_problem
from tabletop.run import report_line, result_of
from tabletop.snapshot import Snapshot
from tabletop.world import World

from golden import V2, golden_cases, plain_run

# The v2 defaults (D-114): short runs, one snapshot after every step, the facts below.
STANDARD = V2


def tapped(problem, seed, config=STANDARD, history=None):
    """The run as the controller's worker drives it: the tap as the world's observer,
    `attach` after the world is built, `after_step` after every step. Returns the log,
    the result, the index of the last event of the start and of each step, and the
    snapshot taken after each step by the test itself."""
    log = EventLog(history)
    tap = Tap(log)
    collector = Collector()
    world = World(load_problem(problem), config, seed, observers=[collector, tap])
    tap.attach(world)
    boundaries = [len(collector.events) - 1]
    taken = []
    while not world.finished:
        world.step()
        tap.after_step(world)
        boundaries.append(len(collector.events) - 1)
        taken.append(world.snapshot())
    return log, result_of(world), boundaries, taken, tap


@pytest.mark.parametrize("problem,seed", golden_cases())
def test_a_run_with_the_tap_is_the_same_run(problem, seed):
    ref = plain_run(problem, seed, STANDARD)
    log, result, boundaries, _, tap = tapped(problem, seed)
    assert tuple(log.records) == ref.records
    assert tuple(log.events) == ref.events
    assert result.draws == ref.draws
    assert report_line(result) == ref.report
    assert tap.world is None, "the tap keeps no engine object once the run has finished"
    # One snapshot at the end of the start, one after every step (these runs are short).
    assert log.snapshots.indexes == boundaries
    assert len(log.snapshots) == result.codelets + 1
    assert all(isinstance(s, Snapshot) for s in log.snapshots.by_index.values())


@pytest.mark.parametrize("problem,seed", golden_cases()[:2])
def test_state_at_agrees_with_a_snapshot_taken_at_that_point(problem, seed):
    log, result, boundaries, taken, _ = tapped(problem, seed)
    start = log.snapshots.by_index[boundaries[0]]
    assert start.clock == 0 and not start.finished
    assert start.coderack["codelets"], "the starting rack is loaded at the start's snapshot"
    assert log.state_at(boundaries[0]) is start
    for k, index in enumerate(boundaries[1:]):
        assert log.state_at(index) == taken[k]
        assert log.state_at(index).clock == k + 1
        assert log.snapshot_index(index) == index
    # Between two boundaries, the earlier snapshot is in force.
    for index in range(boundaries[1] + 1, boundaries[2]):
        assert log.snapshot_index(index) == boundaries[1]
    assert log.snapshot_index(boundaries[2] + 1) == boundaries[2]
    # Before the start's snapshot there is none.
    assert log.state_at(0) is None and log.snapshot_index(0) is None
    last = log.last_snapshot
    assert last.finished and last.clock == result.codelets
    assert last.answer["object"] == result.answer and last.answer["label"] == result.label


def test_the_terms_beside_each_snapshot_are_the_accessors(problem="surround-05", seed=3):
    """Beside each snapshot the tap records every correspondence's static and dynamic
    terms (G-020): they equal `correspondences.static_terms` and `dynamic_terms` on a
    plain world at the same step, are plain tuples, and sum to the snapshot's points."""
    from tabletop import correspondences as C
    from tabletop.gui.models.tap import correspondence_terms
    log, result, boundaries, taken, _ = tapped(problem, seed)
    world = World(load_problem(problem), STANDARD, seed, observers=[])
    expected = [correspondence_terms(world)]
    while not world.finished:
        world.step()
        expected.append({cp.name: {"static": tuple(C.static_terms(world, cp)),
                                   "dynamic": tuple(C.dynamic_terms(world, cp))}
                         for cp in world.workspace.correspondences})
    assert result.draws == world.rng.draws, "reading the terms draws nothing"
    assert len(expected) == len(boundaries)
    seen = 0
    for k, index in enumerate(boundaries):
        terms = log.terms_at(index)
        assert terms == expected[k]
        snapshot = log.state_at(index)
        for cp in snapshot.correspondences:
            static, dynamic = terms[cp["name"]]["static"], terms[cp["name"]]["dynamic"]
            assert [n for n, _ in static] == ["salience", "kind", "end_middle", "position",
                                               "type", "number", "isa", "same_members",
                                               "orientation", "role"]   # ten since item 05: French's nine, then the role factor
            assert [n for n, _ in dynamic] == ["hobj", "hobj_neighbor", "worldview_neighbor"]
            assert sum(v for _, v in static) == pytest.approx(cp["static"])
            assert sum(v for _, v in dynamic) == pytest.approx(cp["dynamic"])
            assert all(isinstance(v, float) for _, v in static + dynamic)
            seen += 1
    assert seen > 20
    assert log.terms_at(0) == {} and log.terms_at(boundaries[1] + 1) == log.terms_at(boundaries[1])


def test_the_keyframe_rule_forced_on_a_short_run():
    """Dense while the clock is at most `dense_codelets`, then every `keyframe_steps`
    steps, and always at the end (G-007)."""
    history = SnapshotHistory(dense_codelets=10, keyframe_steps=5)
    log, result, boundaries, _, _ = tapped("surround-05", 1, history=history)
    assert result.codelets == 61                         # surround-05 seed 1 since v5's fix, D-116 to D-120 (62 since item 09, 89 at item 08, 55 at item 07, 39 at item 05, 59 before)
    want = boundaries[:11]                               # the start and steps 1..10
    want += [boundaries[k] for k in range(15, 61, 5)]    # then every fifth step
    if boundaries[-1] not in want:
        want.append(boundaries[-1])                      # and the end
    assert log.snapshots.indexes == want
    assert log.last_snapshot.finished
    # The defaults: nearly every run is exact at every codelet (RESULTS.md, 44 to 67
    # codelets on average).
    assert DENSE_CODELETS == 300 and KEYFRAME_STEPS == 16


def test_known_facts_of_the_front_page_runs():
    log, result, _, _, _ = tapped("surround-05", 3)
    assert report_line(result) == "Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6."   # since v5's fix, D-116 to D-120 (cup2 (E2), structure 75, 44 codelets, T 41.0 since item 08)
    last = log.last_snapshot
    assert last.answer["object"] == "spoon3" and not last.answer["label"]   # an object French prints no bar for ("none")
    assert last.clock == 59 and round(last.temperature["value"], 1) == 43.6
    log, result, _, _, _ = tapped("surround-05", 1)
    assert result.codelets == 61 and len(log.events) == 779     # since v5's fix (62 and 901 since item 09; 89 and 1204 at item 08)
    assert log.codelets() == (1, 61)


def test_the_history_alone():
    h = SnapshotHistory(dense_codelets=2, keyframe_steps=3)
    assert len(h) == 0 and h.last is None and h.state_at(5) is None
    h.record(4, "start")
    h.record(9, "one")
    assert h.indexes == [4, 9] and h.last == "one"
    assert h.state_at(3) is None and h.state_at(4) == "start" and h.state_at(8) == "start"
    assert h.state_at(9) == "one" and h.state_at(100) == "one"
    h.record(9, "one again")                 # the later snapshot at an index wins
    assert h.indexes == [4, 9] and h.state_at(9) == "one again"
    # The rule: dense for clocks 1 and 2, then every third step, always at the end.
    assert h.wants_after_step(1, False) and h.wants_after_step(2, False)
    h.record(10, "two")
    assert not h.wants_after_step(3, False) and not h.wants_after_step(4, False)
    assert h.wants_after_step(5, False)
    h.record(20, "five")
    assert h.wants_after_step(6, True)
    h.clear()
    assert len(h) == 0 and h.indexes == []

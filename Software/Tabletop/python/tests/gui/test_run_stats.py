"""Loop0006 item 12: the stats pane's model (`tabletop/gui/models/run_stats.py`; G-045),
Qt-free, after the Seek-Whence GUI's `test_run_stats.py`.

- On the four golden runs the report line at the end equals `report_line(result)`, and
  the rows equal the `Result` and the event log's counts.
- The invariants are empty at every snapshot of the golden runs, equal `invariants.check`
  on a plain world stepped to the same clock, and the reading drew nothing.
- At every event the numbers are the events' up to it, so a seek shows the past.
- The run and configuration lines under the four profiles and an edited configuration;
  a stopped run and an error through `finish`; a violated invariant is reported.
"""

import dataclasses

import pytest

from golden import V2, golden_cases, plain_run
from tabletop import events as ev
from tabletop import invariants as INV
from tabletop.config import PROFILES
from tabletop.gui.models import run_stats as rs
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.run_controller import RunController, RunOutcome
from tabletop.gui.models.tap import Tap
from tabletop.problems import load_problem
from tabletop.run import report_line, run_problem
from tabletop.world import World

QUICK = ("surround-05", 3)


def fed(events):
    model = rs.RunStats()
    for event in events:
        model.on_event(event)
    return model


def tapped(problem, seed, profile="standard"):
    """The run through the tap, with its snapshots and the readings beside them."""
    log = EventLog()
    tap = Tap(log)
    controller = RunController(observers=[tap], on_world=tap.attach, on_step=tap.after_step)
    controller.start_run(problem, PROFILES[profile], seed)
    controller.run_to_end()
    controller.wait()
    assert controller.outcome.error is None
    return log, controller.outcome


def test_at_the_end_the_report_line_equals_report_line_of_the_result():
    # The golden cases on the v2 defaults (D-114), whose Worldview facts are read below.
    for problem, seed in golden_cases():
        run = plain_run(problem, seed, V2)
        model = fed(run.events)
        texts = model.texts("finished", "live run")
        assert texts["report"] == report_line(run.result) == run.report, (problem, seed)
        assert texts["codelets"] == str(run.result.codelets)
        assert texts["draws"] == str(run.result.draws)
        assert texts["temperature"].startswith(f"{run.result.temperature:.1f} (")
        assert texts["competitions"] == str(run.result.competitions)
        assert texts["changes"] == str(run.result.changes)
        names = tuple(name for name, _, _ in run.result.worldview)
        if (problem, seed) == ("surround-05", 2):
            # Three in the Worldview since item 09: cp3 entered at codelet 50, cp5 at 76
            # and cp6 at 87, all without a rival and in the order built, so the model's
            # list, as entered (G-045), equals `Result.worldview`'s, as built
            # (`worldview.summary`). At item 08 this run had two, entered in the other
            # order (cp4 at codelet 74, cp3 at 79); at item 05 two as well (cp13 entered at
            # codelet 103, won over cp8, and cp12 at 115), at item 07 one (cp3 entered at
            # codelet 68).
            assert model.worldview == names == ("cp3", "cp5", "cp6")
        else:
            assert model.worldview == names
        changes = [e for e in run.events if isinstance(e, ev.WorldviewChanged)]
        assert model.rating == (changes[-1].rating if changes else None)
        # Every golden run changes its Worldview since item 08 (at item 07 blockage-06 seed
        # 1 answered literally, cup1 E3, with no competition, and the row said "empty",
        # D-040; it answers small-glass3 E2 through cp3 now, and surround-05 seed 1, whose
        # literal answer, cup1, came with cp3 in the Worldview at item 08, answers cup2 E2
        # through cp1 since item 09).
        assert changes and "at the change" in texts["worldview"]
        assert texts["outcome"] == ("capped" if run.result.capped else "answered")
        assert texts["run"] == rs.run_text(problem, "standard", seed) == f"{problem}, seed {seed}"
        # the v2 defaults are edits of v3's (D-114)
        assert texts["config"].startswith("standard: ") and "rho=3.0" in texts["config"]
        assert texts["events"] == str(len(run.events))
        assert f"answer {run.result.answer} ({run.result.label or 'none'})" in texts["result"]
        assert f"through {run.result.through}" in texts["result"]
        assert f"structure {run.result.structure_value:.0f}" in texts["result"]
        log = EventLog()
        for event in run.events:
            log.record(event)
        counts = log.counts
        assert texts["fizzles"] == str(counts.get("CodeletFizzled", 0))
        assert model.groups_built == counts.get("GroupBuilt", 0)
        assert model.groups_removed == counts.get("GroupRemoved", 0)
        assert model.correspondences_built == counts.get("CorrespondenceBuilt", 0)
        assert model.correspondences_removed == counts.get("CorrespondenceRemoved", 0)
        assert texts["state"] == "finished" and texts["source"] == "live run"


def test_the_quick_starts_numbers():
    run = plain_run(*QUICK, V2)
    texts = fed(run.events).texts()
    assert texts["report"] == "Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6."   # since v5's fix, D-116 to D-120 (cup2 (E2), structure 75, 44 codelets, T 41.0 since item 08)
    assert texts["codelets"] == "59" and texts["events"] == "720"       # 44 and 507 since item 08
    assert texts["worldview"] == "1 correspondence: cp2; rating 76.0 at the change"   # cp1, 74.9 since item 08
    assert texts["codelet"] == "FND-ANS()"
    # The run's last temperature reading is the schedule's at codelet 55: two changes
    # since v5's fix (D-116 to D-120), cp1 entering an empty Worldview at codelet 18,
    # which took T from 80.0 to 97.9, and cp2 replacing it at codelet 40 after winning
    # their fight, which took T from 49.0 to 70.2; the schedule brought it down to 43.6
    # (since item 08 the run ran 44 codelets, the one change, cp1 entering at codelet 18,
    # took T from 80.0 to 96.8 and the schedule brought it to 41.0 at codelet 40; until
    # item 08 the run ran 77 codelets, the change at codelet 45 took T to 96.4 and the
    # schedule brought it to 34.6; at item 05 the change at codelet 65, where cp4 beat
    # cp2, was the last reading; before that the schedule's, at codelet 81).
    assert texts["temperature"] == "43.6 (by schedule)" and texts["q"] == "0.760"
    assert texts["structures"] == ("groups 2 built, 0 removed; correspondences 3 built, "
                                   "1 removed")                       # 2 built since item 08


def test_the_invariants_are_empty_on_the_golden_runs_and_the_reading_draws_nothing(golden_runs):
    for (problem, seed), run in golden_runs.items():
        log, outcome = tapped(problem, seed)
        assert outcome.result == run.result               # the reading changed nothing
        indexes = log.snapshots.indexes
        assert indexes
        for index in indexes:
            assert log.invariants_at(index) == {"faults": ()}, (problem, seed, index)
        assert log.invariants_at(0) is None                # before the start's snapshot
        end = len(log.events) - 1
        model = fed(log.events)
        model.update(log.state_at(end), log.invariants_at(end))
        assert model.current == run.result.structure_value  # the rating as it stands (D-062)
        # Every golden run ends with a Worldview since item 08 (at item 07 blockage-06 seed
        # 1 answered literally with structure 0 and the row said "empty").
        assert model.texts()["worldview"].endswith(
            f"rating {run.result.structure_value:.1f} (as of codelet {run.result.codelets})")
        model = rs.RunStats()
        for i, event in enumerate(log.events):
            model.on_event(event)
            model.update(log.state_at(i), log.invariants_at(i))
            if log.state_at(i) is None:
                assert model.texts()["invariants"] == "not checked yet"
            else:
                assert model.texts()["invariants"] == (
                    f"none violated (as of codelet {log.state_at(i).clock})")
                assert not model.faulty
    # the reading equals `invariants.check` on a plain world stepped to the same clock
    log, _ = tapped(*QUICK)
    world = World(load_problem(QUICK[0]), PROFILES["standard"], QUICK[1])
    for index in log.snapshots.indexes:
        snapshot = log.state_at(index)
        while world.clock < snapshot.clock and not world.finished:
            world.step()
        draws = world.rng.draws
        assert tuple(INV.check(world)) == log.invariants_at(index)["faults"] == ()
        assert world.rng.draws == draws


def test_at_every_event_the_numbers_are_the_events_up_to_it():
    run = plain_run(*QUICK, V2)
    model = rs.RunStats()
    worldview, rating, temperature, fizzles = (), None, None, 0
    for i, event in enumerate(run.events):
        model.on_event(event)
        if isinstance(event, ev.WorldviewChanged):
            worldview = tuple(n for n in worldview if n not in event.removed) + event.added
            rating = event.rating
        if isinstance(event, ev.TemperatureUpdated):
            temperature = event.temperature
        if isinstance(event, ev.CodeletFizzled):
            fizzles += 1
        assert model.events == i + 1 and model.clock == event.clock
        assert model.draws == event.draws
        assert model.worldview == worldview and model.rating == rating
        assert model.temperature == temperature and model.fizzles == fizzles
        texts = model.texts()
        assert (texts["report"] == "") == (not isinstance(event, ev.RunEnded))
        assert (texts["outcome"] == "") == (model.ended is None)
    assert model.held is False
    # the held readings before the first Worldview change
    early = fed(run.events[:100])
    assert early.held and early.texts()["temperature"].startswith("80.0 (held, by ")
    assert fed(run.events[:30]).texts()["temperature"] == "80.0 (held, by start)"


def test_the_run_and_configuration_lines_under_the_profiles():
    problem = load_problem("surround-05")
    for name, config in PROFILES.items():
        events = []
        run_problem(problem, config, 1, [type("O", (), {"on_event": staticmethod(events.append)})()])
        model = fed(events)
        texts = model.texts()
        if name == "standard":
            assert texts["run"] == "surround-05, seed 1"
            assert texts["config"] == "standard (the defaults)"
        else:
            assert texts["run"] == f"surround-05, seed 1 ({name})"
            changed = [f for f, _ in model.changed if f != "profile"]
            if changed:
                assert texts["config"].startswith(f"{name}: ")
                for field in changed:
                    assert f"{field}=" in texts["config"]
            else:                         # buridan's knob at the default (until v3, D-114)
                assert texts["config"] == name
    edited = dataclasses.replace(PROFILES["standard"], temperature_alpha=100.0)
    events = []
    run_problem(problem, edited, 1, [type("O", (), {"on_event": staticmethod(events.append)})()])
    texts = fed(events).texts()
    assert texts["run"] == "surround-05, seed 1" and texts["config"] == "standard: temperature_alpha=100.0"
    assert rs.config_note("human", (("profile", "'human'"), ("type_weight", "50"))) == \
        "human: type_weight=50"
    assert rs.config_note("buridan", (("profile", "'buridan'"),)) == "buridan"
    assert rs.config_note(None, ()) == "" and rs.run_text(None, None, None) == ""


def test_a_stopped_run_and_an_error_through_finish():
    run = plain_run(*QUICK, V2)
    model = fed(run.events[:200])
    assert model.texts()["outcome"] == ""
    model.finish(RunOutcome("run", True, None, 200, 1, None))
    assert model.texts()["outcome"] == "stopped after 200 events"
    model.finish(RunOutcome("run", False, None, 200, 1, RuntimeError("boom")))
    assert model.texts()["outcome"] == "error: RuntimeError('boom')"
    # a `RunStarted` forgets the outcome, as it forgets everything else
    model.on_event(run.events[0])
    assert model.texts()["outcome"] == "" and model.events == 1


def test_a_fresh_model_and_a_violated_invariant():
    model = rs.RunStats()
    texts = model.texts("idle", "live run")
    assert set(texts) == {field for field, _ in rs.FIELDS}
    assert texts["state"] == "idle" and texts["source"] == "live run"
    assert all(texts[f] == "" for f, _ in rs.FIELDS if f not in ("state", "source"))
    run = plain_run(*QUICK, V2)
    model = fed(run.events)
    snapshot = type("S", (), {"clock": 46, "worldview": {"total": 75.0}})()
    model.update(snapshot, {"faults": ("T is 120", "cp9 ends on nothing")})
    assert model.faulty
    assert model.texts()["invariants"] == ("2 violated (as of codelet 46): T is 120; "
                                           "cp9 ends on nothing")
    model.update(None, None)
    assert not model.faulty and model.texts()["invariants"] == "not checked yet"


def test_number_text():
    assert rs.number_text(None) == "" and rs.number_text(3) == "3"
    assert rs.number_text(34.84) == "34.8" and rs.number_text(74.95) == "75.0"


@pytest.mark.parametrize("seed", [1, 2])
def test_seeds_differ_in_their_numbers(seed):
    run = plain_run("surround-05", seed)
    texts = fed(run.events).texts()
    assert texts["report"] == run.report and texts["codelets"] == str(run.result.codelets)

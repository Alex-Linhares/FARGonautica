"""Loop0004 item 03: the stats pane's model (`seekwhence/gui/models/run_stats.py`),
Qt-free (G-016).

- At the end of run 1 the fields are the run's numbers and, given the controller's
  outcome, the summary is `Session.summary()` [M86 p. 166].
- The snapshot gives the validity, the tasks waiting, the reigning class and the
  threshold (run 3: `S-group`, 10.0 [M86 pp. 174-179]).
- A stopped run says so; the configuration note says `default` or which fields differ.
"""

import dataclasses
import io

from seekwhence.config import Config
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.run_controller import RunOutcome
from seekwhence.gui.models.run_stats import FIELDS, RunStats, config_note, number_text
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def tapped(ref):
    """The run again, with a tap: the log and the session."""
    log, tap = EventLog(), None
    tap = Tap(log)
    session = Session(ref.config, ref.seed, out=io.StringIO(), lines=list(ref.lines),
                      answers=list(ref.answers), observers=[tap])
    tap.attach_session(session)
    session.run()
    return log, session


def stats_of(events, snapshot=None, state="finished"):
    stats = RunStats()
    for event in events:
        stats.on_event(event)
    stats.update(snapshot)
    return stats, stats.texts(state)


def test_the_fields_are_the_panes(appendix_runs):
    assert [f for f, _ in FIELDS] == ["state", "run", "config", "events", "clock", "task",
                                      "terms", "waiting", "reigning", "threshold",
                                      "hypothesis", "validity", "outcome", "summary"]
    assert number_text(None) == "" and number_text(10.0) == "10.00" and number_text(3) == "3"


def test_at_the_end_of_run_1_the_stats_show_the_summary(appendix_runs):
    ref = appendix_runs[RUN1]
    stats, texts = stats_of(ref.events)
    assert texts["events"] == "174" and texts["clock"] == "52" and texts["terms"] == "4"
    assert texts["hypothesis"] == "(Cycle 3 2 (3 7))" and texts["outcome"] == "ok"
    assert texts["task"] == "Hfilter" and texts["config"] == "default"
    assert texts["run"] == "seed 54" and texts["summary"] == ""
    outcome = RunOutcome("session", False, {"summary": ref.summary, "outcome": "ok"},
                         None, len(ref.events))
    stats.finish(outcome)
    assert stats.texts("finished")["summary"] == ref.summary
    assert ref.summary == "outcome: ok after 4 terms, 52 tasks (seed 54): (Cycle 3 2 (3 7))"
    stats.set_title("run 1")
    assert stats.run_text() == "run 1, seed 54"


def test_the_snapshot_gives_validity_rack_reigning_and_threshold(appendix_runs):
    ref = appendix_runs[RUN3]
    log, session = tapped(ref)
    stats, texts = stats_of(ref.events, log.last_snapshot)
    assert texts["validity"] == "1" and texts["reigning"] == "S-group"     # [M86 p. 179]
    assert texts["threshold"] == "10.00"
    assert texts["waiting"] == str(len(session.world.taskrack.tasks))
    assert texts["hypothesis"] == "(S-group 1 (Countup 2))"
    assert texts["clock"] == "189" and texts["terms"] == "6"
    stats.update(None)
    assert stats.validity is None and stats.waiting is None


def test_a_stopped_run_says_so(appendix_runs):
    ref = appendix_runs[RUN1]
    stats, _ = stats_of(ref.events[:20])
    stats.finish(RunOutcome("session", True, None, None, 20))
    texts = stats.texts("stopped")
    assert texts["outcome"] == "stopped" and texts["summary"] == "stopped after 20 events"
    stats.finish(RunOutcome("session", False, None, None, 20, error=ValueError("x")))
    assert stats.texts()["summary"].startswith("error:")


def test_a_start_event_resets_the_model(appendix_runs):
    stats, _ = stats_of(appendix_runs[RUN1].events)
    stats.on_event(appendix_runs[RUN3].events[0])
    assert stats.events == 1 and stats.seed == 30 and stats.clock == 0 and stats.terms == 0
    assert stats.hypothesis is None and stats.summary == "" and not stats.stopped


def config_pairs(config):
    return tuple((f.name, getattr(config, f.name)) for f in dataclasses.fields(config))


def test_config_note_says_default_or_the_fields_that_differ(appendix_runs):
    assert config_note(config_pairs(Config())) == "default"
    assert config_note(appendix_runs[RUN1].events[0].config) == "default"
    edited = dataclasses.replace(Config(), max_tasks=10, guess_min_hits=4)
    assert config_note(config_pairs(edited)) == "edited: max_tasks, guess_min_hits"
    terse = appendix_runs[RUN2].events[0].config                 # run 2 is terse
    assert config_note(terse) == "edited: verbose"

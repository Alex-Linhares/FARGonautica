"""Loop0004 item 03: the stats dock (`seekwhence/gui/stats_view.py`) in the window,
offscreen, pytest-qt (G-016; Numbo GUI, `test_gui_shell.py`'s stats checks).

- At the end of run 1 the Stats dock shows `Session.summary()` [M86 p. 166], the state,
  the outcome, the run's numbers and the validity from the snapshot.
- A stopped run shows `stopped`; seeking shows the numbers of the past.
"""

import io

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from seekwhence.gui.models import presets                     # noqa: E402
from seekwhence.gui.models.run_stats import FIELDS            # noqa: E402
from seekwhence.session import Session                        # noqa: E402

from golden import APPENDIX                                   # noqa: E402

TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def summary_of(inputs):
    session = Session(inputs.config, inputs.seed, out=io.StringIO(), lines=list(inputs.lines),
                      answers=list(inputs.answers))
    session.run()
    return session.summary()


def test_the_stats_dock_holds_the_view(window):
    stats = window.stats_view
    assert window.stats_dock.widget() is stats
    assert stats.pane_id == "P12" and stats.text("state") == "idle"
    assert set(stats.texts()) == {field for field, _ in FIELDS}
    assert "run 1" in stats.text("run")


def test_at_the_end_the_stats_show_the_summary(window, qtbot):
    inputs = presets.appendix_inputs(RUN1)
    outcome = run_to_end(qtbot, window, inputs)
    stats = window.stats_view
    assert stats.text("summary") == summary_of(inputs) == outcome.result["summary"]
    assert stats.text("summary") == (
        "outcome: ok after 4 terms, 52 tasks (seed 54): (Cycle 3 2 (3 7))")   # [M86 p. 166]
    assert stats.text("state") == "finished" and stats.text("outcome") == "ok"
    assert stats.text("events") == "174" and stats.text("clock") == "52"
    assert stats.text("terms") == "4" and stats.text("hypothesis") == "(Cycle 3 2 (3 7))"
    assert stats.text("validity") == "1" and stats.text("config") == "default"
    assert stats.text("run") == "run 1: 3 7 3 7 [M86 pp. 164-166], seed 54"
    assert stats.paints > 0 and stats.events == 174


def test_run_3s_reigning_class_and_threshold(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    stats = window.stats_view
    assert stats.text("reigning") == "S-group" and stats.text("threshold") == "10.00"
    assert stats.text("hypothesis") == "(S-group 1 (Countup 2))"          # [M86 p. 179]
    assert stats.text("config") == "default"


def test_a_stopped_run_says_stopped(window, qtbot):
    window.start_session(presets.appendix_inputs(RUN3), paused=True)
    for n in range(1, 6):
        window.step()
        # The step's `paused` state arrives on a queued signal after its delivery.
        qtbot.waitUntil(lambda: window.position == n - 1 and window.state == "paused",
                        timeout=TIMEOUT)
    assert window.stats_view.text("state") == "paused"
    assert window.stats_view.text("events") == "5"
    window.stop()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    stats = window.stats_view
    assert stats.text("state") == "stopped" and stats.text("outcome") == "stopped"
    assert stats.text("summary") == "stopped after 5 events"


def test_seeking_shows_the_numbers_of_the_past(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    stats = window.stats_view
    assert window.seek(0)
    assert stats.text("events") == "1" and stats.text("clock") == "0"
    assert stats.text("terms") == "0" and stats.text("hypothesis") == ""
    assert stats.text("validity") == ""                 # no snapshot at the start event
    assert window.seek(len(window.history) - 1)
    assert stats.text("events") == "174" and stats.text("validity") == "1"

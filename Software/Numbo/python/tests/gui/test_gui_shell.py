"""loop0003 item 7: the GUI shell (numbo/gui/): main window, controls, stats.

pytest-qt, offscreen (conftest.py sets QT_QPA_PLATFORM).  The run controller
runs Numbo on its worker; each event crosses to the GUI thread through a
queued signal, the GUI-thread views handle it, and only then is it
acknowledged.  Skipped when PySide6 is not installed.
"""

import io
import os
import subprocess
import sys
import threading

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import Qt                    # noqa: E402
from PySide6.QtWidgets import QDockWidget        # noqa: E402

import full_runs                                 # noqa: E402
from numbo import harness, observe              # noqa: E402
from numbo.gui import controls as controls_module  # noqa: E402
from numbo.gui.main_window import MainWindow     # noqa: E402

TIMEOUT = 30000   # ms


@pytest.fixture
def thread_errors(monkeypatch):
    errors = []
    monkeypatch.setattr(threading, "excepthook", lambda args: errors.append(args))
    yield errors
    assert errors == []


@pytest.fixture
def window(qtbot, thread_errors):
    w = MainWindow()
    qtbot.addWidget(w)
    w.show()
    yield w
    w.close()
    assert not w.controller.is_alive()


class Recorder:
    """A GUI-thread view: records each event, the thread it came on, and how
    many events the worker had published by then."""

    def __init__(self, window):
        self.window = window
        self.events = []
        self.published = []
        self.threads = set()

    def on_event(self, event):
        self.events.append(event)
        self.published.append(self.window.controller.events_published)
        self.threads.add(threading.current_thread() is threading.main_thread())


def choose(window, puzzle=1, seed=1, cap="20000", speed=None):
    c = window.controls
    if isinstance(puzzle, int):
        c.puzzle_combo.setCurrentIndex(puzzle - 1)
    else:
        c.puzzle_combo.setCurrentIndex(c.puzzle_combo.count() - 1)
        c.custom_edit.setText(puzzle)
    c.seed_edit.setText(str(seed))
    c.cap_edit.setText(cap)
    if speed is not None:
        c.speed_slider.setValue(speed)


def plain_run(problem, seed, cap=20000):
    events = []

    class R:
        def on_event(self, event):
            events.append(event)

    out = io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap,
                                trace=io.StringIO(), out=out, observers=[R()])
    return result, out.getvalue(), events


FASTEST = len(controls_module.DELAYS) - 1


# -- the window -----------------------------------------------------------------

def test_the_window_has_dockable_controls_and_stats(window):
    docks = {d.objectName(): d for d in window.findChildren(QDockWidget)}
    assert {"controls-dock", "stats-dock"} <= set(docks)
    for dock in docks.values():
        assert dock.features() & QDockWidget.DockWidgetFeature.DockWidgetMovable
        assert dock.isVisible()
    c = window.controls
    assert c.puzzle_combo.count() == 12             # 11 chapter puzzles + custom
    assert c.puzzle_combo.itemText(0) == "1: 114 from 11 20 7 1 6"
    assert c.puzzle_combo.itemText(11) == "Custom"
    assert not c.custom_edit.isEnabled()
    c.puzzle_combo.setCurrentIndex(11)
    assert c.custom_edit.isEnabled()
    for name in ("play", "pause", "step", "step_iteration", "run_to_end", "stop"):
        assert getattr(c, name + "_button").text()
    assert window.controller.state == "idle"
    assert window.stats.text("state") == "idle"


def test_the_speed_slider_sets_the_delay(window):
    c = window.controls
    for i, delay in enumerate(controls_module.DELAYS):
        c.speed_slider.setValue(i)
        assert window.controller.delay == delay
        assert c.speed_label.text()
    assert controls_module.DELAYS[-1] == 0
    assert c.speed_label.text() == "max"


# -- play -----------------------------------------------------------------------

def test_play_runs_puzzle_1_seed_1_to_a_valid_solution(window, qtbot):
    recorder = Recorder(window)
    window.add_view(recorder)
    choose(window, 1, 1, speed=FASTEST)
    qtbot.mouseClick(window.controls.play_button, Qt.MouseButton.LeftButton)
    qtbot.waitUntil(lambda: window.stats.model.check is not None, timeout=TIMEOUT)

    s = window.stats
    assert s.text("outcome") == "solved, 45 iterations"
    assert s.text("check") == "valid: 114 = (6 x 20) - (7 - 1)"
    assert s.text("iteration") == "44"
    assert s.text("problem") == "114 from 11 20 7 1 6, seed 1"
    assert s.text("state") == "finished"
    assert s.text("x") and s.text("temperature")

    # The same run without the GUI: the same events, printed text and result.
    result, output, events = plain_run(full_runs.PUZZLES[0], 1)
    assert recorder.events == events
    assert window.outcome.output == output
    assert window.outcome.result == result
    assert s.text("events") == str(len(events)) == "286"
    # Every event came on the GUI thread, while the worker waited on it:
    # nothing more was published before the views had handled it.
    assert recorder.threads == {True}
    assert recorder.published == list(range(1, len(events) + 1))


def test_a_restart_after_stop_shows_only_the_new_run(window, qtbot):
    recorder = Recorder(window)
    window.add_view(recorder)
    choose(window, 3, 1, speed=0)        # slowest: it can't get far
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.stats.model.events >= 2, timeout=TIMEOUT)
    window.controls.stop_button.click()
    assert not window.controller.is_alive()
    qtbot.waitUntil(lambda: window.stats.text("state") == "stopped", timeout=TIMEOUT)
    choose(window, 3, 8, speed=FASTEST)
    recorder.events.clear()
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.stats.model.check is not None, timeout=TIMEOUT)
    _, output, events = plain_run(full_runs.PUZZLES[2], 8)
    assert recorder.events == events
    assert window.stats.text("outcome") == f"solved, {events[-1].iterations} iterations"
    assert window.stats.text("check").startswith("invalid: ")


def test_an_error_outcome_is_shown_cleanly(window, qtbot):
    choose(window, 1, 40, speed=FASTEST)
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.stats.model.check is not None, timeout=TIMEOUT)
    result, _, _ = plain_run(full_runs.PUZZLES[0], 40)
    assert result["outcome"] == "error"
    assert window.stats.text("outcome") == (
        f"error, {result['iterations']} iterations: {result['error']}")
    assert window.stats.text("check") == 'invalid: no "Done :" in the output'


# -- pause and step -------------------------------------------------------------

def test_step_starts_paused_and_publishes_one_event_at_a_time(window, qtbot):
    recorder = Recorder(window)
    window.add_view(recorder)
    choose(window, 1, 1, speed=FASTEST)
    c = window.controls
    c.step_button.click()
    qtbot.waitUntil(lambda: len(recorder.events) == 1, timeout=TIMEOUT)
    assert recorder.events[0].kind == "start"
    qtbot.wait(200)
    assert len(recorder.events) == 1
    assert window.stats.text("state") == "paused"
    for n in range(2, 5):
        c.step_button.click()
        qtbot.waitUntil(lambda: len(recorder.events) == n, timeout=TIMEOUT)
        qtbot.wait(50)
        assert len(recorder.events) == n
    assert window.stats.text("events") == "4"

    # Step-iteration: on through the next iteration event, then paused.
    c.step_iteration_button.click()
    qtbot.waitUntil(lambda: recorder.events[-1].kind == "iteration", timeout=TIMEOUT)
    qtbot.wait(100)
    assert recorder.events[-1].kind == "iteration"
    assert window.stats.text("iteration") == "0"
    c.step_iteration_button.click()
    qtbot.waitUntil(lambda: window.stats.text("iteration") == "1", timeout=TIMEOUT)
    qtbot.wait(100)
    assert isinstance(recorder.events[-1], observe.IterationBegan)
    assert recorder.events[-1].n == 1

    c.run_to_end_button.click()
    qtbot.waitUntil(lambda: window.stats.model.check is not None, timeout=TIMEOUT)
    assert window.stats.text("outcome") == "solved, 45 iterations"
    assert len(recorder.events) == 286


def test_pause_holds_a_playing_run_and_play_resumes_it(window, qtbot):
    recorder = Recorder(window)
    window.add_view(recorder)
    choose(window, 3, 1, speed=FASTEST)
    c = window.controls
    c.play_button.click()
    qtbot.waitUntil(lambda: len(recorder.events) > 50, timeout=TIMEOUT)
    c.pause_button.click()
    qtbot.waitUntil(lambda: window.stats.text("state") == "paused", timeout=TIMEOUT)
    held = len(recorder.events)
    qtbot.wait(300)
    assert len(recorder.events) == held
    assert window.controller.events_published == held
    c.step_button.click()
    qtbot.waitUntil(lambda: len(recorder.events) == held + 1, timeout=TIMEOUT)
    c.play_button.click()
    qtbot.waitUntil(lambda: len(recorder.events) > held + 50, timeout=TIMEOUT)
    assert window.stats.text("state") == "running"
    # The buttons follow the state.
    assert c.pause_button.isEnabled() and c.stop_button.isEnabled()
    c.stop_button.click()


# -- stop and close -------------------------------------------------------------

def test_stop_mid_run_ends_the_worker_promptly(window, qtbot, capsys):
    choose(window, 3, 1, speed=FASTEST)
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.stats.model.events > 200, timeout=TIMEOUT)
    window.controls.stop_button.click()
    assert window.controller.wait(2.0)
    # The finished signal comes after the state's.
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.stats.text("state") == "stopped"
    assert window.stats.text("outcome") == "stopped"
    assert window.stats.text("check") == ""
    assert window.outcome.stopped and window.outcome.error is None
    assert not window.controls.stop_button.isEnabled()
    assert "Traceback" not in capsys.readouterr().err


@pytest.mark.parametrize("paused", [False, True])
def test_closing_mid_run_does_not_hang(qtbot, thread_errors, capsys, paused):
    w = MainWindow()
    qtbot.addWidget(w)
    w.show()
    choose(w, 3, 1, speed=FASTEST)
    w.controls.play_button.click()
    qtbot.waitUntil(lambda: w.stats.model.events > 100, timeout=TIMEOUT)
    if paused:
        w.controls.pause_button.click()
    assert w.controller.is_alive()
    assert w.close()
    assert not w.controller.is_alive()
    assert w.controller.state == "stopped"
    qtbot.wait(100)       # any queued deliveries arrive at a closed window
    assert "Traceback" not in capsys.readouterr().err


# -- input errors ---------------------------------------------------------------

@pytest.mark.parametrize("puzzle, seed, cap, message", [
    ("1 2 3", 1, "20000", "got 3 numbers"),
    ("114 11 20 x 1 6", 1, "20000", "'x' is not an integer"),
    (1, "abc", "20000", "seed: 'abc' is not an integer"),
    (1, 1, "0", "at least 1"),
])
def test_invalid_input_shows_an_error_not_a_traceback(window, qtbot, capsys,
                                                      puzzle, seed, cap, message):
    choose(window, puzzle, seed, cap)
    for button in ("play", "step", "step_iteration", "run_to_end"):
        getattr(window.controls, button + "_button").click()
        assert message in window.controls.error_label.text()
        assert window.controls.error_label.isVisible()
        assert message in window.statusBar().currentMessage()
        assert window.controller.state == "idle"
    assert "Traceback" not in capsys.readouterr().err
    # A valid input clears the error.
    choose(window, 1, 1, speed=FASTEST)
    window.controls.run_to_end_button.click()
    assert window.controls.error_label.text() == ""
    assert not window.controls.error_label.isVisible()
    qtbot.waitUntil(lambda: window.stats.model.check is not None, timeout=TIMEOUT)


def test_a_custom_problem_runs(window, qtbot):
    choose(window, "6 1 1 1 1 1", 2, "300", speed=FASTEST)
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.stats.model.check is not None, timeout=TIMEOUT)
    result, _, _ = plain_run((6, 1, 1, 1, 1, 1), 2, 300)
    assert window.stats.text("problem") == "6 from 1 1 1 1 1, seed 2"
    assert window.stats.text("outcome") == f"{result['outcome']}, {result['iterations']} iterations"


# -- the entry point ------------------------------------------------------------

def test_python_m_numbo_gui_smoke_runs_offscreen():
    env = {**os.environ, "QT_QPA_PLATFORM": "offscreen"}
    proc = subprocess.run([sys.executable, "-m", "numbo.gui", "--smoke", "--puzzle", "1",
                           "--seed", "1"], capture_output=True, text=True, timeout=120,
                          cwd=full_runs.PYTHON_DIR, env=env)
    assert proc.returncode == 0, proc.stderr
    assert proc.stdout.splitlines()[-2:] == [
        "outcome: solved, 45 iterations (seed 1)",
        "check: valid: 114 = (6 x 20) - (7 - 1)"]
    assert "Traceback" not in proc.stderr


def test_python_m_numbo_gui_rejects_bad_arguments():
    proc = subprocess.run([sys.executable, "-m", "numbo.gui", "--smoke", "--puzzle", "12"],
                          capture_output=True, text=True, timeout=60,
                          cwd=full_runs.PYTHON_DIR,
                          env={**os.environ, "QT_QPA_PLATFORM": "offscreen"})
    assert proc.returncode == 2
    assert "puzzle" in proc.stderr and "Traceback" not in proc.stderr

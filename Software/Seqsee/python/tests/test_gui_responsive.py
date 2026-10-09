"""Responsiveness (loop0002 item 023): the window stays usable while the model runs.

Mirrors Seqsee.pl with Tk, where the model runs inside Tk's event loop: Interaction_continue
(GUI_sparse.conf's c binding) calls Seqsee::Interaction_step_n, which redraws through
UI/Graphical.pm's update_display every ``update_after`` steps (lib/Tk/Seqsee.pm's Update), and
GUI_sparse.conf's Pause sets ``$Global::Break_Loop``, read after every step. Perl's window
only reacts between steps; here the model runs on the runner's worker thread
(seqsee/gui/runner.py) and the window (seqsee/gui/qt/mainwindow.py) draws the snapshots it
sends, so we measure:

- frame cost: snapshot + compose + render of every view for a 30-term workspace (a real run,
  with groups and relations) within ``mainwindow.FRAME_BUDGET`` (50 ms);
- frame coalescing: snapshots that arrive faster than the window draws are dropped, only the
  newest is drawn, and a question or the end of a command draws the pending one first;
- event-loop latency while ``continue`` runs (a 10 ms probe timer and posted events);
- Pause: the run stops within one batch (``update_interval`` steps) of the click.
"""
import gc
import statistics
import time

import pytest

from seqsee import cli
from seqsee import global_ as Global
from seqsee.gui import app, snapshot
from seqsee.gui.draw import views

qt = pytest.mark.gui

SEQ30 = "1 1 2 1 2 3 1 2 3 4 1 2 3 4 5 1 2 3 4 5 6 1 2 3 4 5 6 7 1 2"
SEED = 3
LATENCY_BOUND = 0.150       # the longest the event loop may stall during a run (seconds)
LATENCY_P95 = 0.050         # 95% of probe ticks run within this lateness
PAUSE_BOUND = 1.0           # seconds from the Pause click to the "Paused" state


def _thirty_term_run(steps=300):
    """A real run on 30 terms (answers "no"), left in the model; returns its snapshot."""
    result = cli.run_headless(["--seq", SEQ30, "--seed", str(SEED), "--max-steps", str(steps),
                               "--answer", "no"])
    assert result["elements"][:30] == [int(x) for x in SEQ30.split()]
    return snapshot.take()


def _window(qtbot, **kwargs):
    from seqsee.gui.qt.mainwindow import MainWindow
    window = MainWindow(**kwargs)
    qtbot.addWidget(window)
    window.show()
    return window


@pytest.fixture
def run_window(qtbot):
    made = []

    def make(argv):
        from seqsee.gui.runner import Runner
        runner = Runner()
        done = []
        runner.command_done.connect(lambda n, r: done.append(n))
        window = app.build(app.parse_args(argv), runner=runner)
        qtbot.addWidget(window)
        window.show()
        made.append(window)
        qtbot.waitUntil(lambda: "new_sequence" in done, timeout=20000)
        return window, runner, done

    yield make
    for w in made:
        w.close()
        if w.runner is not None:
            w.runner.quit(timeout=5)


# ---- frame cost ---------------------------------------------------------------------------------
@qt
def test_frame_within_budget_for_30_terms(qtbot):
    """Snapshot + compose + render of each of the 11 views (window and visible panes) for a
    30-term workspace: the median of 5 frames is within FRAME_BUDGET, and the window's own
    frame timing (``frame_times``) records them."""
    from seqsee.gui.qt import mainwindow
    assert mainwindow.FRAME_BUDGET == 0.050
    snap = _thirty_term_run()
    assert snap.element_count >= 30 and snap.groups and snap.relations
    window = _window(qtbot)
    report = {}
    for v in range(len(views.VIEW_OPTIONS)):
        window.set_view(v)
        costs, takes = [], []
        for _ in range(5):
            t0 = time.perf_counter()
            snap = snapshot.take()
            t1 = time.perf_counter()
            window.show_snapshot(snap)
            costs.append(time.perf_counter() - t0)
            takes.append(t1 - t0)
        assert window.composition is not None
        report[v] = (statistics.median(takes), statistics.median(costs))
        assert window.frame_times[-1] <= costs[-1]
        assert len(window.frame_times) >= 5
    slow = {v: c for v, (_, c) in report.items() if c > mainwindow.FRAME_BUDGET}
    assert not slow, report
    # The heaviest window: view 0 and every drawing module's pane shown as well.
    window.set_view(0)
    for dock in window.pane_docks.values():
        dock.show()
    qtbot.waitUntil(lambda: all(d.isVisible() for d in window.pane_docks.values()))
    costs = []
    for _ in range(5):
        t0 = time.perf_counter()
        window.show_snapshot(snapshot.take())
        costs.append(time.perf_counter() - t0)
    assert all(d.composition is not None for d in window.pane_docks.values())
    assert statistics.median(costs) <= mainwindow.FRAME_BUDGET, costs


@qt
def test_snapshot_cost_recorded_by_the_runner(qtbot, run_window):
    """The runner times each snapshot it takes on the worker (``snapshot_times``); for the
    30-term run they stay within the frame budget."""
    from seqsee.gui.qt import mainwindow
    window, runner, done = run_window(["--seq", SEQ30, "--seed", str(SEED),
                                       "--max-steps", "200"])
    window.run_command("step_n", 200)
    qtbot.waitUntil(lambda: done.count("step_n") == 1 or runner.pending_question is not None,
                    timeout=30000)
    if runner.pending_question is not None:
        runner.answer(runner.pending_question, 0)
        qtbot.waitUntil(lambda: done.count("step_n") == 1, timeout=30000)
    assert runner.snapshot_times
    assert statistics.median(runner.snapshot_times) <= mainwindow.FRAME_BUDGET


# ---- frame coalescing ---------------------------------------------------------------------------
@qt
def test_snapshots_are_coalesced(qtbot):
    """Snapshots queued before the window gets to draw: only the newest is drawn."""
    snaps = [snapshot.take() for _ in range(3)]
    window = _window(qtbot)
    qtbot.waitExposed(window)
    for s in snaps:
        window.queue_snapshot(s)
    assert window.snapshot is None          # nothing drawn yet: the event loop draws
    qtbot.waitUntil(lambda: window.snapshot is snaps[-1], timeout=2000)
    qtbot.wait(20)
    assert len(window.frame_times) == 1     # one frame drawn
    assert window.frames_dropped == 2
    # show_snapshot draws at once and drops a queued older snapshot.
    window.queue_snapshot(snaps[0])
    window.show_snapshot(snaps[1])
    qtbot.wait(20)
    assert window.snapshot is snaps[1]
    assert window.frames_dropped == 3 and len(window.frame_times) == 2


@qt
def test_question_draws_the_pending_snapshot_first(qtbot):
    """The runner sends a snapshot before each question (the hilit objects): the window
    draws it before it shows the question, even if the frame was not drawn yet."""
    from seqsee.gui.runner import Question
    snap = snapshot.take()
    window = _window(qtbot)
    window.queue_snapshot(snap)
    window.show_question(Question("boolean", "Is the next term 4?"))
    assert window.snapshot is snap
    assert window.commentary.pending is not None


@qt
def test_end_of_command_draws_the_pending_snapshot(qtbot):
    """The closing snapshot of a command is drawn by the time the state changes."""
    snap = snapshot.take()
    window = _window(qtbot)
    window.queue_snapshot(snap)
    window._state_changed("idle")
    assert window.snapshot is snap


@qt
def test_runner_snapshots_go_through_the_queue(qtbot, run_window):
    """attach_runner connects the runner's snapshots to ``queue_snapshot``; after a command
    the window shows its last snapshot."""
    window, runner, done = run_window(["--seq", "1 1 2 1 2 3", "--seed", "7",
                                       "--max-steps", "100"])
    window.run_command("step_n", 20)
    qtbot.waitUntil(lambda: done.count("step_n") == 1, timeout=20000)
    qtbot.waitUntil(lambda: window.snapshot is not None and window.snapshot.steps == 20,
                    timeout=2000)


# ---- event-loop latency -------------------------------------------------------------------------
class _Probe:
    """A 10 ms timer: how late each tick runs while the model works."""

    def __init__(self, interval_ms=10):
        from PySide6.QtCore import QTimer
        self.interval = interval_ms / 1000
        self.gaps = []
        self._last = None
        self.timer = QTimer()
        self.timer.setInterval(interval_ms)
        self.timer.timeout.connect(self._tick)

    def start(self):
        self._last = time.perf_counter()
        self.timer.start()

    def stop(self):
        self.timer.stop()

    def _tick(self):
        now = time.perf_counter()
        self.gaps.append(now - self._last)
        self._last = now

    def lateness(self):
        return [max(0.0, g - self.interval) for g in self.gaps]


@qt
def test_event_loop_responsive_during_continue(qtbot, run_window):
    """``continue`` on 30 terms, a display every step (update_interval 1, the heaviest
    load: ~30 snapshots a second): the event loop never stalls more than LATENCY_BOUND, 95%
    of the probe ticks are on time within LATENCY_P95, and a posted event runs promptly."""
    from PySide6.QtCore import QTimer
    from seqsee.gui.qt import mainwindow
    from seqsee.gui.qt.autoanswer import AutoAnswerer
    window, runner, done = run_window(["--seq", SEQ30, "--seed", str(SEED),
                                       "--max-steps", "1500", "--update-interval", "1"])
    answerer = AutoAnswerer(window, SEQ30, keep_going=True)
    probe = _Probe()
    probe.start()
    posted = []

    def post():
        t0 = time.perf_counter()
        QTimer.singleShot(0, lambda: posted.append(time.perf_counter() - t0))

    poster = QTimer()
    poster.setInterval(50)
    poster.timeout.connect(post)
    poster.start()
    shown = window.redraw_count
    # The heap left by the earlier tests of a full pytest run would make a full garbage
    # collection (which holds the GIL) a stall of its own: set it aside.
    gc.collect()
    gc.freeze()
    t0 = time.perf_counter()
    try:
        window.run_command("continue")
        qtbot.waitUntil(lambda: answerer.errors or window.run_state == "idle" and (
            window.snapshot.steps >= 1500), timeout=60000)
    finally:
        gc.unfreeze()
    elapsed = time.perf_counter() - t0
    probe.stop()
    poster.stop()
    assert answerer.errors == []
    assert Global.Steps_Finished > 100
    late = sorted(probe.lateness())
    assert len(late) >= 0.5 * elapsed / probe.interval, (len(late), elapsed)
    assert late[-1] <= LATENCY_BOUND, late[-5:]
    assert late[int(0.95 * (len(late) - 1))] <= LATENCY_P95, late[-20:]
    assert posted and max(posted) <= LATENCY_BOUND, sorted(posted)[-5:]
    # The window kept drawing during the run (not only at the end), each frame in budget.
    assert window.redraw_count - shown >= min(10, elapsed * 5)
    assert statistics.median(window.frame_times) <= mainwindow.FRAME_BUDGET
    # The runner waited for each frame (pace_frames), so few or none were dropped.
    assert runner.pace_frames
    assert window.frames_dropped <= len(window.frame_times)


# ---- Pause ----------------------------------------------------------------------------------
@qt
@pytest.mark.parametrize("update_interval", [1, 25])
def test_pause_within_one_batch(qtbot, run_window, update_interval):
    """Pause during ``continue`` (displays every ``update_interval`` steps): the run stops
    within one batch of the click (in fact after the step that was running), the window
    says "Paused" within PAUSE_BOUND, and it shows the step where the run stopped."""
    from seqsee.gui.qt.autoanswer import AutoAnswerer
    window, runner, done = run_window(["--seq", SEQ30, "--seed", str(SEED),
                                       "--max-steps", "3000",
                                       "--update-interval", str(update_interval)])
    AutoAnswerer(window, SEQ30, keep_going=True)
    window.run_command("continue")
    qtbot.waitUntil(lambda: window.snapshot is not None and window.snapshot.steps >= 60,
                    timeout=30000)
    at_click = Global.Steps_Finished     # read only, to measure
    t0 = time.perf_counter()
    window.run_command("pause")
    qtbot.waitUntil(lambda: window.run_state == "paused", timeout=10000)
    took = time.perf_counter() - t0
    stopped = Global.Steps_Finished
    # The step running at the click (and one that may end between the read and the click):
    # well within the batch of update_interval steps.
    assert stopped - at_click <= 2
    assert stopped - at_click <= update_interval + 1
    assert took <= PAUSE_BOUND
    qtbot.waitUntil(lambda: window.snapshot.steps == stopped, timeout=2000)
    assert window.status_labels["state"].text() == "Paused"
    # Nothing runs any more.
    qtbot.wait(100)
    assert Global.Steps_Finished == stopped

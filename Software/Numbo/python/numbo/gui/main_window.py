"""The main window: dockable panes over a run controller.

Threading and the redraw handshake:
  - The RunController (numbo.models.run_controller) runs Numbo on its worker
    thread.  Its deliver, on_state and on_finished callbacks run on the
    worker; they only emit the Bridge's signals, which are connected
    queued, so the slots run on the GUI thread, in emission order.
  - The GUI-thread views (the stats pane, the tree canvas, the Pnet and
    coderack docks, and every view given to add_view) are observers of a
    GUI-side Subject.  For each delivery the window publishes the event to
    them, calls each view's redraw(now=False) (it marks itself dirty; a
    view's redraw() without NOW paints at once), paints the window once
    (gui/paint.py: each view exactly once, now), and only then
    acknowledges the delivery.  The worker
    publishes nothing more until then, so every event is drawn, none is
    skipped or coalesced.
  - The models live on the GUI thread: they are views' state, never
    controller observers (those would run on the worker).
  - Each run has a generation number, carried by its signals, so a queued
    signal from a stopped run never reaches the next run's views.
  - Closing the window stops the worker (stop() wakes it from any wait,
    including one for an acknowledgment that will never come) and joins it.

A live run writes its oracle trace to os.devnull, as the CLI does, so an
error outcome's iteration count is the CLI's.

Polish (loop0003 item 11):
  - the outcome banner over the canvas (TreeCanvas.banner) shows
    run_stats.RunStats.verdict() once the run (or the replay, at its last
    event) has ended: green for a valid solution, amber for an invalid one
    or none, red for an error, naming the 1987 code's reactivate-cyto race
    or kill-block gap when a run hits them;
  - the Run menu's shortcuts: Space play/pause, Right step (one event
    forward in the history when no run is live), Shift+Right step an
    iteration, Esc stop, Left one event back.  A line edit or spin box with
    the focus keeps its keys; the sliders take no keyboard focus;
  - SETTINGS (a QSettings, or None for none: the tests' windows) keeps the
    window's geometry and docks and the controls' inputs and speed, read
    when the window is made and written when it closes.

History, replay and scrubbing (loop0003 item 10):
  - HISTORY, an event_log.EventLog, holds every event of the run drawn (a
    live run's or an oracle trace's, as they are delivered; a session
    file's, when it is opened).  The event log dock shows it, and POSITION
    is the index of the event the views show now.
  - Session > Save writes the history as a session file (part way through a
    live run too: the file then has no RunEnded event).  Session > Open
    loads a session file, or re-runs an oracle trace, verifying it as it
    goes (through the controller like a live run, every event drawn).  The
    window is then in replay mode (REPLAY_SOURCE): the run buttons play the
    history on from POSITION (RunController.start_events, through the same
    gate, so every event is drawn, as live).  Session > Close replay goes
    back to live runs.
  - seek(index), from the timeline (the slider, back and forward, go to an
    iteration) or a log row, whenever no run is live: the views are fed the
    history's events (on from POSITION when going forward, else from the
    start: the models alone take about 1 µs an event) and drawn once, at
    INDEX.  Every view is a function of the events up to INDEX except the
    tree canvas's layout, which depends on the layout before it (its
    stability).  The window's LayoutHistory gives the layout the canvas had
    at INDEX - 1 when it drew the run: from snapshots kept every 64 events
    while drawing, or made by walking the history (in idle time after a
    session is opened).  So a seek shows exactly what the live run showed
    at that event (tested), and playing on from there goes on as it did.
"""

import functools
import inspect
import os

from PySide6.QtCore import QByteArray, QObject, QSize, Qt, QTimer, Signal
from PySide6.QtGui import QKeySequence
from PySide6.QtWidgets import QDockWidget, QFileDialog, QMainWindow, QScrollArea, QToolBar

from numbo import observe, session
from numbo.gui import paint
from numbo.gui.coderack_view import CoderackView
from numbo.gui.controls import ControlsPanel
from numbo.gui.log_view import LogView
from numbo.gui.pnet_view import PnetView
from numbo.gui.stats_view import StatsPanel
from numbo.gui.timeline import TimelineBar
from numbo.gui.tree_view import GHOST_WINDOW, TreeCanvas
from numbo.models import run_controller
from numbo.models.event_log import EventLog
from numbo.models.layout_history import LayoutHistory
from numbo.models.run_stats import InputError

__all__ = ["Bridge", "MainWindow"]

DOCK_WIDTH = 360
CODERACK_WIDTH = 380
PNET_HEIGHT = 300
LOG_WIDTH = 520
# Events the idle walk lays out per tick after a session is opened (about
# 20 ms), so that a seek anywhere finds a snapshot near it.
WALK_CHUNK = 200


DEFAULT_SIZE = (1500, 950)


def default_size(screen):
    """DEFAULT_SIZE, or less to fit SCREEN's available area."""
    w, h = DEFAULT_SIZE
    if screen is not None:
        room = screen.availableGeometry()
        if room.width() > 0 and room.height() > 0:
            w, h = min(w, room.width() - 40), min(h, room.height() - 60)
    return QSize(w, h)


def _int(value):
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


class Bridge(QObject):
    """Carries the worker's callbacks to the GUI thread (queued)."""
    delivered = Signal(int, object)     # generation, Delivery
    state_changed = Signal(int, str)    # generation, state
    finished = Signal(int, object)      # generation, RunOutcome


class MainWindow(QMainWindow):
    """run_finished(outcome) once a run's outcome has been shown;
    event_drawn(index) when every view has drawn event INDEX of the history
    (a delivery's, before it is acknowledged, or a seek's)."""

    run_finished = Signal(object)
    event_drawn = Signal(int)

    def __init__(self, parent=None, settings=None):
        super().__init__(parent)
        self.settings = settings
        self.setWindowTitle("Numbo")
        self.resize(default_size(self.screen()))
        self.outcome = None
        self._generation = 0
        self._closed = False
        self._trace = open(os.devnull, "w", encoding="utf-8")
        self._views = observe.Subject(self._view_error)
        self._redraws = []
        self.history = EventLog()
        self.position = -1
        self.replay_source = None       # None, or (format, path) of the file replayed
        self._recording = True          # the deliveries add to the history

        self.bridge = Bridge(self)
        queued = Qt.ConnectionType.QueuedConnection
        self.bridge.delivered.connect(self._on_delivered, queued)
        self.bridge.state_changed.connect(self._on_state, queued)
        self.bridge.finished.connect(self._on_finished, queued)
        self.controller = run_controller.RunController()

        self.canvas = TreeCanvas()
        self.tree_view = self.canvas.view
        self.setCentralWidget(self.canvas)
        # The timeline runs along the bottom of the window, under the docks.
        self.timeline = TimelineBar()
        self.timeline_bar = QToolBar("Timeline")
        self.timeline_bar.setObjectName("timeline-toolbar")
        self.timeline_bar.addWidget(self.timeline)
        self.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self.timeline_bar)
        self.layouts = LayoutHistory(self.tree_view.make_layouter, GHOST_WINDOW)

        self.controls = ControlsPanel()
        self.stats = StatsPanel()
        self.controls_dock = self._dock("Controls", "controls-dock", self.controls)
        # A long error message or check reason wraps; it scrolls, never clips.
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QScrollArea.Shape.NoFrame)
        scroll.setWidget(self.stats)
        self.stats_dock = self._dock("Stats", "stats-dock", scroll)
        self.resizeDocks([self.controls_dock, self.stats_dock], [DOCK_WIDTH, DOCK_WIDTH],
                         Qt.Orientation.Horizontal)
        self.coderack_view = CoderackView()
        self.coderack_dock = self._dock("Coderack", "coderack-dock", self.coderack_view,
                                        Qt.DockWidgetArea.RightDockWidgetArea)
        self.pnet_view = PnetView()
        self.pnet_dock = self._dock("Pnet", "pnet-dock", self.pnet_view,
                                    Qt.DockWidgetArea.BottomDockWidgetArea)
        # The log goes beside the Pnet: the Pnet's grid is wide and short, so
        # at the dock's height it leaves room to its right, and log rows
        # need width.
        self.log_view = LogView(self.history)
        self.log_dock = self._dock("Event log", "log-dock", self.log_view,
                                   Qt.DockWidgetArea.BottomDockWidgetArea)
        self.splitDockWidget(self.pnet_dock, self.log_dock, Qt.Orientation.Horizontal)
        self.resizeDocks([self.coderack_dock], [CODERACK_WIDTH], Qt.Orientation.Horizontal)
        # At most 30% of the height: on a small screen the left column
        # (Controls, Stats) and the canvas need the rest.
        self.resizeDocks([self.pnet_dock], [min(PNET_HEIGHT, round(0.3 * self.height()))],
                         Qt.Orientation.Vertical)
        self.resizeDocks([self.log_dock], [LOG_WIDTH], Qt.Orientation.Horizontal)
        # Controls at their own height; Stats get the rest of the column.
        self._controls_height = self.controls.sizeHint().height()
        self._sized = self._state_restored = False
        self.add_view(self.stats)
        self.add_view(self.tree_view)
        self.add_view(self.pnet_view)
        self.add_view(self.coderack_view)
        self._scenes = (self.tree_view.scene(), self.pnet_view.scene())

        view_menu = self.menuBar().addMenu("&View")
        for dock in (self.controls_dock, self.stats_dock, self.coderack_dock, self.log_dock,
                     self.pnet_dock):
            view_menu.addAction(dock.toggleViewAction())
        view_menu.addAction(self.timeline_bar.toggleViewAction())
        session_menu = self.menuBar().addMenu("&Session")
        self.open_action = session_menu.addAction("&Open session or oracle trace…")
        self.open_action.setShortcut(QKeySequence.StandardKey.Open)
        self.open_action.triggered.connect(self._open_dialog)
        self.save_action = session_menu.addAction("&Save session…")
        self.save_action.setShortcut(QKeySequence.StandardKey.Save)
        self.save_action.triggered.connect(self._save_dialog)
        self.close_replay_action = session_menu.addAction("&Close replay")
        self.close_replay_action.triggered.connect(self.close_replay)
        self.run_menu = self.menuBar().addMenu("&Run")
        for text, keys, slot in (
                ("Play / Pause", "Space", self.play_pause),
                ("Step", "Right", self.step_or_forward),
                ("Step iteration", "Shift+Right", self.step_iteration),
                ("Run to end", "", self.run_to_end),
                ("Stop", "Esc", self.stop),
                (None, None, None),
                ("Back one event", "Left", self.back_one)):
            if text is None:
                self.run_menu.addSeparator()
                continue
            action = self.run_menu.addAction(text)
            if keys:
                action.setShortcut(QKeySequence(keys))
            action.triggered.connect(slot)

        c = self.controls
        c.play_button.clicked.connect(self.play)
        c.pause_button.clicked.connect(self.controller.pause)
        c.step_button.clicked.connect(self.step)
        c.step_iteration_button.clicked.connect(self.step_iteration)
        c.run_to_end_button.clicked.connect(self.run_to_end)
        c.stop_button.clicked.connect(self.stop)
        c.delay_changed.connect(self.controller.set_delay)
        self.controller.set_delay(c.delay)

        self.timeline.seek_requested.connect(self.seek)
        self.timeline.iteration_requested.connect(self.jump_to_iteration)
        self.log_view.event_activated.connect(self.seek)
        self._walk_timer = QTimer(self)
        self._walk_timer.setInterval(0)
        self._walk_timer.timeout.connect(self._walk_layouts)
        self._update_actions()
        self._restore_settings()

    def _dock(self, title, name, widget, area=Qt.DockWidgetArea.LeftDockWidgetArea):
        dock = QDockWidget(title, self)
        dock.setObjectName(name)
        dock.setWidget(widget)
        self.addDockWidget(area, dock)
        return dock

    # -- views ------------------------------------------------------------------

    def add_view(self, view):
        """VIEW gets every event on the GUI thread (on_event), then, if it
        has one, its redraw() is called before the event is acknowledged."""
        self._views.subscribe(view)
        redraw = getattr(view, "redraw", None)
        if redraw is not None:
            if "now" in inspect.signature(redraw).parameters:
                redraw = functools.partial(redraw, now=False)
            self._redraws.append(redraw)
        return view

    def _view_error(self, error):
        observe.report_to_stderr(error)
        self.statusBar().showMessage(f"a view failed: {error.exception!r}")

    # -- the controls -------------------------------------------------------------

    def _start(self, mode):
        """Start a run of the controls' inputs, paused if MODE is a step;
        False (and the error shown) if an input is not valid."""
        try:
            problem, seed, cap = self.controls.inputs()
        except InputError as error:
            self._error(str(error))
            return False
        self.controls.show_error("")
        self.statusBar().clearMessage()
        self._connect(recording=True)
        self.stats.set_source("live run")
        try:
            self.controller.start_run(problem, seed, cap, paused=mode != "play",
                                      trace=self._trace)
        except (ValueError, run_controller.EngineBusy) as error:
            self._error(str(error))
            return False
        return True

    def _connect(self, recording):
        """A new generation of the controller's callbacks.  RECORDING: its
        deliveries add to the history (a live run, an oracle replay), else
        they move through it (a replay's playback)."""
        # The old run is stopped and joined before the generation moves on,
        # so none of its callbacks can carry the new generation.
        self.controller.stop()
        self._walk_timer.stop()
        self._generation += 1
        generation = self._generation
        bridge = self.bridge
        self.controller.deliver = lambda d: bridge.delivered.emit(generation, d)
        self.controller.on_state = lambda s: bridge.state_changed.emit(generation, s)
        self.controller.on_finished = lambda o: bridge.finished.emit(generation, o)
        self._recording = recording
        self.outcome = None

    def _error(self, message):
        self.controls.show_error(message)
        self.statusBar().showMessage(message)

    def _live(self):
        return self.controller.is_alive() and self.controller.state in ("running", "paused")

    def _start_replay(self, mode):
        """Play the history on from POSITION, paused if MODE is a step; at
        the end, from the start (but a step there does nothing)."""
        events = self.history.events
        if not events:
            return False
        if self.position >= len(events) - 1:
            if mode == "step":
                self.statusBar().showMessage("at the end of the replay")
                return False
            self.seek(0)
        self._connect(recording=False)
        self.controller.start_events(events, self.position + 1, paused=mode != "play")
        return True

    def _begin(self, mode):
        if self.replay_source is not None:
            return self._start_replay(mode)
        return self._start(mode)

    def play(self):
        if self._live():
            self.controller.play()
        else:
            self._begin("play")

    def step(self):
        if self._live() or self._begin("step"):
            self.controller.step()

    def step_iteration(self):
        if self._live() or self._begin("step"):
            self.controller.step_iteration()

    def run_to_end(self):
        if self._live() or self._begin("run-to-end"):
            self.controller.run_to_end()

    def stop(self):
        self.controller.stop()          # joins the worker
        self._update_actions()

    def play_pause(self):
        """Space: pause a playing run, else play."""
        if self._live() and self.controller.state == "running":
            self.controller.pause()
        else:
            self.play()

    def step_or_forward(self):
        """The right arrow: step a run; with no run live and the views back
        in the history, one event forward (as the timeline's ▶)."""
        if not self._live() and 0 <= self.position < len(self.history) - 1:
            self.seek(self.position + 1)
        else:
            self.step()

    def back_one(self):
        """The left arrow: one event back in the history (no run live)."""
        if self.position > 0:
            self.seek(self.position - 1)

    # -- the worker's signals, on the GUI thread ----------------------------------

    def _on_delivered(self, generation, delivery):
        if self._closed or generation != self._generation:
            return
        event = delivery.event
        if self._recording:
            if event.kind == "start":
                self.layouts.clear()
            self.log_view.appended(self.history.on_event(event))
            if len(self.history) == 1:
                self._update_actions()
        self.position = delivery.index
        self._views.publish(event)
        self._draw(event)
        self.controller.acknowledge(delivery.ticket)

    def _draw(self, event):
        """Every view draws the event at POSITION, now: each marks itself
        dirty, then the window is painted once (paint.paint_all)."""
        for redraw in self._redraws:
            try:
                redraw()
            except Exception as exception:
                self._view_error(observe.ObserverError(redraw, event, exception))
        index = self.position
        self.layouts.record(index, self.tree_view.layout)
        self.log_view.set_position(index)
        self.log_view.redraw(now=False)
        h = self.history
        self.timeline.set_position(index, len(h), h.iteration_at(index), h.iterations())
        self.canvas.banner.show_verdict(self.stats.model.verdict())
        paint.paint_all(self._scenes)
        self.event_drawn.emit(index)

    def _on_state(self, generation, state):
        if self._closed or generation != self._generation:
            return
        self.controls.set_state(state)
        self.stats.set_state(state)
        self._update_actions()

    def _on_finished(self, generation, outcome):
        if self._closed or generation != self._generation:
            return
        self.outcome = outcome
        if outcome.source == "events":
            self.stats.replayed()
            self.stats.redraw()
        else:
            self.stats.finish(outcome)
        self.canvas.banner.show_verdict(self.stats.model.verdict())
        verdict = self.stats.model.verdict()
        if verdict is not None and verdict.level == "error" and not outcome.stopped:
            self.statusBar().showMessage(f"the run ended with an error: "
                                         f"{self.stats.model.ended.message}")
        if outcome.source == "oracle":
            self.stats.set_source(self._oracle_text(outcome))
        if outcome.error is not None:
            self.statusBar().showMessage(f"the run ended with an error: {outcome.error}")
        self._update_actions()
        self.run_finished.emit(outcome)

    # -- the history: scrubbing ------------------------------------------------------

    def seek(self, index):
        """Show event INDEX of the history (see the module docstring); False
        if a run is live or there is no such event."""
        events = self.history.events
        if self._live() or not 0 <= index < len(events):
            return False
        start = self.position + 1 if 0 <= self.position < index else 0
        publish = self._views.publish
        for i in range(start, index + 1):
            publish(events[i])
        # The canvas lays out from the layout it had at the event before.
        self.tree_view.layouter.previous = self.layouts.layout_at(events, index - 1)
        self.stats.replayed()
        self.position = index
        self._draw(events[index])
        return True

    def jump_to_iteration(self, n):
        index = self.history.iteration_index(n)
        return index is not None and self.seek(index)

    def _walk_layouts(self):
        """Idle time after a session is opened: lay out the history ahead,
        so that a seek finds a snapshot near it."""
        events = self.history.events
        if self._live() or self.layouts.advance(events, WALK_CHUNK) >= len(events) - 1:
            self._walk_timer.stop()

    def _update_actions(self):
        self.save_action.setEnabled(bool(self.history.events))
        self.close_replay_action.setEnabled(self.replay_source is not None)
        self.timeline.set_scrubbable(bool(self.history.events) and not self._live())

    # -- sessions ------------------------------------------------------------------

    def save_session(self, path):
        """Write the history to PATH as a session file; False (and the error
        shown) if that fails."""
        try:
            session.write_session(str(path), list(self.history.events))
        except OSError as error:
            self.statusBar().showMessage(f"cannot save the session: {error}")
            return False
        self.statusBar().showMessage(f"saved {len(self.history)} events to {path}")
        return True

    def open_file(self, path):
        """Replay the session file or oracle trace PATH; False (and the
        error shown) if it can't be read."""
        path = str(path)
        name = os.path.basename(path)
        try:
            kind = session.detect_format(path)
            loaded = session.load_session(path) if kind == "session" else None
        except (session.SessionError, OSError) as error:
            self.statusBar().showMessage(f"cannot open {name}: {error}")
            return False
        self.statusBar().clearMessage()
        self.controls.show_error("")
        self.replay_source = (kind, path)
        self.controls.set_replay(name)
        self.setWindowTitle(f"Numbo — replaying {name}")
        self.position = -1
        if kind == "session":
            self._connect(recording=False)
            self.layouts.clear()
            self.history.load(loaded.events)
            self.log_view.reload()
            ended = "" if loaded.ended else " (it ends part way)"
            self.stats.set_source(f"session {name}: {len(loaded.events)} events{ended}")
            self.stats.set_state("replay")
            self.seek(0)
            self._walk_timer.start()
        else:
            self._connect(recording=True)
            self.layouts.clear()
            self.history.clear()
            self.log_view.reload()
            self.stats.set_source(f"oracle trace {name}: verifying…")
            try:
                self.controller.start_oracle(path, paused=True)
            except run_controller.EngineBusy as error:
                self._error(str(error))
                return False
            self.controller.step()      # draw its start event
        self._update_actions()
        return True

    def _oracle_text(self, outcome):
        name = os.path.basename(self.replay_source[1]) if self.replay_source else "?"
        error = outcome.error
        if outcome.stopped:
            return f"oracle trace {name}: stopped, not verified"
        if isinstance(error, session.SessionError):
            where = f" at line {error.line}" if error.line is not None else ""
            what = "differs" if isinstance(error, session.TraceMismatch) else "error"
            return f"oracle trace {name}: {what}{where}: {error.reason}"
        if error is not None:
            return f"oracle trace {name}: error: {error}"
        return f"oracle trace {name}: verified, {outcome.oracle.lines} lines"

    def close_replay(self):
        """Back to live runs (the history stays, to scrub)."""
        if self.replay_source is None:
            return
        self.controller.stop()
        self._walk_timer.stop()
        self.replay_source = None
        self.controls.set_replay(None)
        self.setWindowTitle("Numbo")
        self.stats.set_source("live run")
        self._update_actions()

    def _open_dialog(self):
        path, _ = QFileDialog.getOpenFileName(
            self, "Open a session or an oracle trace", "",
            "JSON lines (*.jsonl);;All files (*)")
        if path:
            self.open_file(path)

    def _save_dialog(self):
        path, _ = QFileDialog.getSaveFileName(self, "Save the session", "session.jsonl",
                                              "JSON lines (*.jsonl);;All files (*)")
        if path:
            self.save_session(path)

    # -- closing --------------------------------------------------------------------

    # -- remembered state (QSettings) ------------------------------------------------

    def _restore_settings(self):
        """The window's geometry, docks and inputs as they were left.  A
        value that doesn't read leaves the default."""
        s = self.settings
        if s is None:
            return
        geometry, state = s.value("window/geometry"), s.value("window/state")
        if isinstance(geometry, (QByteArray, bytes)):
            self.restoreGeometry(QByteArray(geometry))
        if isinstance(state, (QByteArray, bytes)):
            self._state_restored = self.restoreState(QByteArray(state))
        c = self.controls
        puzzle = _int(s.value("controls/puzzle"))
        if puzzle is not None and 0 <= puzzle < c.puzzle_combo.count():
            c.puzzle_combo.setCurrentIndex(puzzle)
        for key, edit in (("custom", c.custom_edit), ("seed", c.seed_edit),
                          ("cap", c.cap_edit)):
            value = s.value(f"controls/{key}")
            if isinstance(value, (str, int)):
                edit.setText(str(value))
        speed = _int(s.value("controls/speed"))
        if speed is not None and 0 <= speed <= c.speed_slider.maximum():
            c.speed_slider.setValue(speed)

    def _save_settings(self):
        s = self.settings
        if s is None:
            return
        c = self.controls
        s.setValue("window/geometry", self.saveGeometry())
        s.setValue("window/state", self.saveState())
        s.setValue("controls/puzzle", c.puzzle_combo.currentIndex())
        s.setValue("controls/custom", c.custom_edit.text())
        s.setValue("controls/seed", c.seed_edit.text())
        s.setValue("controls/cap", c.cap_edit.text())
        s.setValue("controls/speed", c.speed_slider.value())
        s.sync()

    def shutdown(self):
        """Stop the worker and wait for it (the window's state is saved
        first, once)."""
        if not self._closed:
            self._save_settings()
        self._closed = True
        self._walk_timer.stop()
        stopped = self.controller.stop()
        if stopped and not self._trace.closed:
            self._trace.close()
        return stopped

    def showEvent(self, event):
        super().showEvent(event)
        if not self._sized:
            # Docks are only sized once the window is laid out: Controls at
            # their own height, Stats the rest of the column (unless the
            # docks were restored from the settings).
            self._sized = True
            if not self._state_restored:
                self.resizeDocks([self.controls_dock], [self._controls_height],
                                 Qt.Orientation.Vertical)

    def closeEvent(self, event):
        self.shutdown()
        super().closeEvent(event)

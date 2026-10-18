"""The main window: dockable panes over a run controller (Numbo GUI, `main_window.py`,
adopted; ANSWERS.md 3, 7, 8; PANES.md sections 1, 3 and 4).

Threading and the redraw handshake (Numbo GUI):
  - The `RunController` (`models/run_controller.py`) runs the engine on its worker
    thread. Its `deliver`, `on_state` and `on_finished` callbacks run on the worker; they
    only emit the `Bridge`'s signals, which are connected queued, so the slots run on the
    GUI thread, in emission order.
  - The views are observers of a GUI-side `Subject`. For each delivery the window
    publishes the event to them, calls each view's `redraw(now=False)` (it marks itself
    dirty), paints the window once (`paint.py`: each view exactly once, now), and only
    then acknowledges the delivery. The worker publishes nothing more until then, so
    every event is drawn, none is skipped or coalesced.
  - The tap (`models/tap.py`) is the one observer on the worker: it must read the
    `World` at the moment of the event (ENGINE-API.md section 2), so it is subscribed to
    the controller, which publishes to it before delivering; the history it fills is
    read on the GUI thread only up to the event delivered (G-008).
  - Each run has a generation number, carried by its signals, so a queued signal from a
    stopped run never reaches the next run's views.
  - Closing the window stops the worker (a pending guess is cancelled first, so the
    worker is not left waiting on a click; `stop()` wakes it from any other wait) and
    joins it.

The guess (G-008): in an interactive run the engine's `answerer` hook is an
`InteractiveAnswerer` (`models/answerer.py`). On the worker it emits `guess_asked` and
blocks; the window shows the guess panel, already drawn up to the guess by the gate, and
a click on OK or No releases the worker with the answer [M86 pp. 166, 175].

The layout (ANSWERS.md 3, 8; PANES.md section 3): the central widget is a vertical
splitter, the sequence pane over the cytoplasm; every other pane is a `QDockWidget` with
an `objectName`; the timeline is a bottom toolbar. When the window opens, the user's own
layout (`QSettings("seekwhence", "seekwhence-gui")`, `window/state`) is restored if there
is one; otherwise the repository default, `seekwhence/gui/default_layout.json`
(`models/layout_file.py`); otherwise the built-in arrangement, `_arrange_builtin`. A
layout that fails to restore falls back to the next level and says so in the status
bar. The built-in arrangement is made once the window is shown (`showEvent`): Qt 6
leaves stale tab bars behind, one drawn over the menu bar and a second "Console | Event
log" bar, when docks are tabified in two groups before the window has been laid out
(ANSWERS.md 10.1, 10.2; G-052). View > Reset layout restores the repository default (else the built-in) and clears
the saved state; Reset to built-in layout does what it says; Save layout as default for
this code… writes the file after a confirmation. `MainWindow(settings=None)` reads and
writes no settings (the tests and `--smoke`). The layout bytes carry `STATE_VERSION`
(`saveState(version)`): a `window/state` saved by an older or a newer arrangement of
the docks is refused by Qt, said so in the status bar, and the next layout of the
precedence is used; a setting that is not bytes at all is ignored the same way
(item 18; G-059). The window fits `SMALLEST_SIZE`, 1280 x 800, with every dock shown:
each pane's minimum is a few rows, the long labels clip, the controls' form scrolls
(G-058).

The configuration (item 15; `config_view.py`, `models/config_table.py`, G-050, G-051;
ANSWERS.md 5): `config_table` holds the user's edits of `Config`; the configuration dock
(hidden by default, Edit > Configuration) is its view. An edit applies to the next run
only: `_config_edited` sets the controls' `base_config` and rebuilds `inputs`, and
`set_inputs` replaces the edits into any inputs given (the caps and the trace stay the
controls'). The title is `title_for(inputs)`, marked `[edited: ...]` as the stats pane's
note when the configuration differs from the defaults. A run's configuration is in its
session file (the run line); a replay shows the file's configuration read-only in the
pane. The edits are saved under `config/edits` with the settings.

History and seeking (Numbo GUI): `history` is the `EventLog` the tap fills; `position`
is the index of the event the views show now. `seek(index)` is allowed whenever no run
is live: the views are fed the history's events on from `position` when going forward,
else from the start, and drawn once. A view that reads the snapshot in force has a
`set_position(index)`, called before each event it is given (G-016). The timeline
toolbar (item 12; `timeline.py`, G-042, G-043) shows the position over the history
with its markers and the parse strip, and asks to seek (the slider, back and forward,
a click on a marker or a parse cell) or to jump (`jump_to`: a term, a clock or an
event); the event log dock (`log_view.py`, G-044, G-045) asks to seek on a double-click
or Enter. Both are refused while a run is live, and the timeline's controls are
disabled then (`set_scrubbable`). Session > Save events (JSON lines)… writes the
history as `python3 -m seekwhence --json` does (`save_events`).

Sessions and replay (item 13; `models/session_file.py`, G-046, G-047; Numbo GUI,
`main_window.py`'s sessions): Session > Save session… writes the history so far with the
run line it was started from (`run_line`: the inputs, and for an interactive run the
lines typed and the answers given), also part way through a run. Session > Open
session… reads the file and re-executes its run line through the window's own
controller, as a scripted run, with a `ReplayChecker` subscribed on the worker that
compares every event with the stored one; the window is then in replay mode
(`replay_source`): Play, Step and Run to end drive the re-run, every event drawn as
live, snapshots included, and when it ends Play re-executes it again. At the first
difference (the engine has changed since the file was saved) the checker stops the run,
the window warns, and the stored events are shown instead (`_show_stored`): they are
loaded into the history without snapshots, so the log, the console and the timeline
show them whole and the structure panes show only what the events alone give. Session >
Close replay goes back to live runs; the history stays, to scrub.

The terms (item 03; G-015): an interactive run (`presets.interactive_inputs`) has no
scripted lines; its `Session` reads them from a `TermFeed` (`models/term_feed.py`), a
stream the sequence pane's box fills (`enter_terms`) and the worker's `Console.ask`
blocks on, so "a user presents to the system numbers" [M86 p. 37] one per prompt. A
stop or a close cancels the feed as it cancels the answerer.

The controls (item 14; `controls.py`, `models/controls.py`, G-048, G-049; Numbo GUI,
`controls.py`): the controls dock holds the `ControlsPanel`, which takes no events. Its
buttons are connected to `play`, `pause`, `step`, `step_task`, `step_term`, `run_to_end`
and `stop`, its slider to the controller's delay, and every change of its inputs to
`_inputs_from_controls`, which keeps `inputs`, the next run's `RunInputs`, in step (an
invalid text is shown in the panel and refuses the next start). `set_inputs` shows the
inputs given (a preset action, a setting) in the panel. A run judged against a
sequence's continuation gets the bench's `ContinuationAnswerer` through the `answerer`
hook (DECISION D-070). The panel's values are saved under `controls/*` (G-049).
Whichever way a run is started (the Presets menu, the controls, `start_session`, a
session file replayed, the command line), the controls' preset, the window title and the
stats pane's Run row show the inputs actually used (`_show_started`; ANSWERS.md 10.3;
G-054).

The command line and help (item 16; `app.py`, `models/guide.py`; G-055): `app.main`
sets the controls from the arguments and may play or smoke-run; Help > What am I looking
at? is `show_guide`, the pane-to-pages table of `docs/gui/panes.md` as a page, with the
platoplasm network and the taskrack drawing marked GUI design, not Meredith's
(ANSWERS.md 4).

The console (item 04; G-018, G-019): the console pane (`console_view.py`) shows the
transcript as the CLI prints it, in both modes, from `models/console.py`, which plays
the console protocol on the run's input lines at each ask. The window tells it the
lines before a run starts (`begin`), and every line the user types, in the sequence
pane's box or at the console's command line, goes to the feed of the live interactive
run and to the console model alike (`_put_inputs`), so that both read the same lines in
the same order and a command typed live is a session input. A command with no such run
live (the buttons, or the command line) prints the state at the cursor from the
snapshot and is not an input (`show_command`; `models/commands.py`, G-017). A click on
a console line seeks to its event when no run is live; Session > Save transcript… and
Edit > Copy take the console's text in the mode shown.
"""

import dataclasses
import functools
import inspect
import json
import os
from pathlib import Path

from PySide6.QtCore import QByteArray, QObject, QSize, Qt, Signal, qVersion
from PySide6.QtGui import QAction, QKeySequence
from PySide6.QtWidgets import (QApplication, QDialog, QDialogButtonBox, QDockWidget,
                               QFileDialog, QMainWindow, QMessageBox, QSplitter,
                               QTextBrowser, QToolBar, QVBoxLayout)

from seekwhence import bench
from seekwhence.events import ObserverError, Subject
from seekwhence.gui import paint
from seekwhence.gui.config_view import ConfigView
from seekwhence.gui.console_view import ConsoleView
from seekwhence.gui.controls import ControlsPanel
from seekwhence.gui.cytoplasm_view import CytoplasmCanvas
from seekwhence.gui.models import commands, guide, layout_file, presets, run_controller
from seekwhence.gui.models.run_stats import config_note
from seekwhence.gui.models import controls as cm
from seekwhence.gui.models import session_file as sf
from seekwhence.gui.models import config_table as ct
from seekwhence.gui.models.config_table import ConfigTable
from seekwhence.gui.models.answerer import InteractiveAnswerer
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.selection import Selection
from seekwhence.gui.models.tap import Tap
from seekwhence.gui.models.term_feed import TermFeed, split_words
from seekwhence.gui.hypothesis_view import HypothesisView
from seekwhence.gui.log_view import LogView
from seekwhence.gui.models import timeline as tm
from seekwhence.gui.placeholder import PanePlaceholder
from seekwhence.gui.reformulation_view import ReformulationView
from seekwhence.gui.platoplasm_view import PlatoplasmView
from seekwhence.gui.sequence_view import SequenceView
from seekwhence.gui.socratoplasm_view import SocratoplasmView
from seekwhence.gui.stats_view import StatsView
from seekwhence.gui.taskrack_view import TaskrackView
from seekwhence.gui.timeline import TimelineBar
from seekwhence.session import parse_line
from seekwhence.trace import event_record

__all__ = ["AREAS", "PANES", "SMALLEST_SIZE", "STATE_VERSION", "TITLE", "Bridge", "MainWindow",
           "about_text", "default_size", "title_for"]

TITLE = "Seek-Whence"
DEFAULT_SIZE = (1500, 950)
# The window fits this with every dock shown: no pane's minimum is larger than a few
# rows, so a small screen clips or scrolls a pane rather than widening the window
# (item 18; G-058).
SMALLEST_SIZE = (1280, 800)
# The version `saveState` stamps on the layout bytes (QSettings `window/state` and the
# default layout file): a state saved by another version of the window's docks is
# refused by `restoreState` and the next layout in the precedence is used (G-059).
STATE_VERSION = 1
# The built-in sizes (PANES.md section 3; Numbo GUI's DOCK_WIDTH and friends).
LEFT_WIDTH = 300
RIGHT_WIDTH = 400
CONTROLS_HEIGHT = 260
BOTTOM_HEIGHT = 280
PLATOPLASM_WIDTH = 520
CENTRAL_SIZES = (380, 420)      # item 03: room for the sequence pane's three rows (G-014)

AREAS = {"left": Qt.DockWidgetArea.LeftDockWidgetArea,
         "right": Qt.DockWidgetArea.RightDockWidgetArea,
         "top": Qt.DockWidgetArea.TopDockWidgetArea,
         "bottom": Qt.DockWidgetArea.BottomDockWidgetArea}
AREA_NAMES = {area: name for name, area in AREAS.items()}

# The docks of PANES.md section 3, in the order they are added: attribute, objectName,
# title, pane id, the pages or precedent behind the pane, area, the dock it is tabbed
# under (or None) and whether it is shown by default. Two panes have no figure in the
# sources and say so (ANSWERS.md 4).
PANES = (
    ("controls", "controls-dock", "Controls", "P11", "Numbo GUI, controls.py", "left", None, True),
    ("stats", "stats-dock", "Stats", "P12", "Numbo GUI, stats_view.py", "left", None, True),
    ("taskrack", "taskrack-dock", "Taskrack", "P6",
     "[M86 pp. 41-42]. GUI design, not Meredith's: no figure of the taskrack exists.",
     "right", None, True),
    ("socratoplasm", "socratoplasm-dock", "Socratoplasm", "P3", "[M86 pp. 46, 102, 106]",
     "right", None, True),
    ("hypothesis", "hypothesis-dock", "Hypothesis", "P4", "[M86 pp. 81, 83]", "right", None, True),
    ("reformulation", "reformulation-dock", "Reformulation", "P7", "[M86 pp. 99-120]",
     "right", "hypothesis-dock", True),
    ("platoplasm", "platoplasm-dock", "Platoplasm", "P5",
     "[M86 pp. 90-91]. GUI design, not Meredith's: no figure of the network exists.",
     "bottom", None, True),
    ("console", "console-dock", "Console", "P8", "[M86 pp. 164-179]", "bottom", None, True),
    ("log", "log-dock", "Event log", "P9", "Numbo GUI, log_view.py", "bottom", "console-dock", True),
    ("config", "config-dock", "Configuration", "P13", "ANSWERS.md 5 (Edit > Configuration)",
     "right", "hypothesis-dock", False),
)
TIMELINE_NAME = "timeline-toolbar"


def default_size(screen):
    """DEFAULT_SIZE, or less to fit the screen's available area (Numbo GUI,
    `main_window.py`)."""
    w, h = DEFAULT_SIZE
    if screen is not None:
        room = screen.availableGeometry()
        if room.width() > 0 and room.height() > 0:
            w, h = min(w, room.width() - 40), min(h, room.height() - 60)
    return QSize(w, h)


def title_for(inputs) -> str:
    """The window's title for the next run: `Seek-Whence — run 1: ...`, and
    `[edited: guess_min_hits]` when the run's configuration differs from the defaults,
    the stats pane's note (ANSWERS.md 5; G-016, G-051)."""
    title = f"{TITLE} — {inputs.title}" if inputs.title else TITLE
    note = config_note(sf.config_pairs(inputs.config))
    return title if note == "default" else f"{title} [{note}]"


def about_text() -> str:
    """Help > About: what the window is, the engine's version, the sources (Numbo GUI;
    G-055)."""
    return (f"<b>Seek-Whence</b> — a PySide6 window on the Python reconstruction "
            f"(seekwhence {sf.__version__}) of Marsha Meredith's Seek-Whence (1986) "
            f"[M86], described in <i>Fluid Concepts and Creative Analogies</i>, ch. 1 "
            f"[FCCA].<br><br>The GUI only observes a run: the same seed gives the same "
            f"run with or without it. Every pane says which pages of the thesis it draws; "
            f"the platoplasm network and the taskrack drawing are GUI design, not "
            f"Meredith's (Help &gt; What am I looking at?).<br><br>Built by Ralph loop "
            f"0004 after the Numbo GUI's conventions; see docs/gui/panes.md.")


def _int(value):
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


class Replay:
    """The session file open for replay (G-047): the `file` read, its `path` and `name`,
    the `inputs` that re-execute its run line, the `checker` of the re-run under way
    (None before the first) and the `divergence` found, if any."""

    def __init__(self, file, path, name, inputs):
        self.file = file
        self.path = path
        self.name = name
        self.inputs = inputs
        self.checker = None
        self.divergence = None


class Bridge(QObject):
    """Carries the worker's callbacks to the GUI thread, queued (Numbo GUI,
    `main_window.py`): a delivery, a state, an outcome, and (G-008) a guess waiting for
    the user; each with the generation of the run it belongs to."""
    delivered = Signal(int, object)     # generation, Delivery
    state_changed = Signal(int, str)    # generation, state
    finished = Signal(int, object)      # generation, RunOutcome
    guess_asked = Signal(int, object)   # generation, GuessPending


class MainWindow(QMainWindow):
    """The one window of the GUI (Numbo GUI, `main_window.py`; see the module docstring).
    `settings`: a `QSettings`, or None to read and write nothing. `layout_path`: the
    repository default layout file (ANSWERS.md 8), the package's by default; None for
    none. Signals: `run_finished(outcome)` once a run's outcome has been shown;
    `event_drawn(index)` when every view has drawn event `index` of the history;
    `run_state_changed(state)`; `guess_shown(pending)` when the guess panel asks."""

    run_finished = Signal(object)
    event_drawn = Signal(int)
    run_state_changed = Signal(str)
    guess_shown = Signal(object)

    def __init__(self, settings=None, layout_path=layout_file.DEFAULT_PATH, parent=None):
        super().__init__(parent)
        self.settings = settings
        self.layout_path = None if layout_path is None else Path(layout_path)
        self.layout_source = "built-in"
        self._shown = False                 # the first showEvent has come (G-052)
        self._arrange_pending = False       # the built-in arrangement waits for it
        self.setWindowTitle(TITLE)
        self.resize(default_size(self.screen()))
        self.outcome = None
        self.state = "idle"
        self._generation = 0
        self._closed = False
        self._views = Subject(self._view_error)
        self._redraws = []
        self._positioners = []
        self._scenes = ()
        self.selection = Selection()      # shared by every pane (G-025)
        self._hover_saved = None          # the selection before a storyboard hover (G-041)
        self.feed = None                # the TermFeed of an interactive run (G-015)
        self.history = EventLog()
        self.tap = Tap(self.history)
        self._tap_subscribed = False
        self.position = -1
        self.replay_source = None       # None, or the path of the session file replayed
        self._replay = None             # the Replay of the file opened (G-047)
        self._recording = True          # the deliveries are the tap's new events
        self._answerer = None           # the interactive answerer of the run, if any
        self._run_observers = []        # observers subscribed for the current run only
        self.config_table = ConfigTable()   # the user's edits of the configuration (G-050)
        self.inputs = presets.appendix_inputs(presets.DEFAULT_PRESET)
        self._started = self.inputs     # the inputs the run in the history started from
        self._live_inputs = None        # the inputs chosen before a replay (G-054)

        self.bridge = Bridge(self)
        queued = Qt.ConnectionType.QueuedConnection
        self.bridge.delivered.connect(self._on_delivered, queued)
        self.bridge.state_changed.connect(self._on_state, queued)
        self.bridge.finished.connect(self._on_finished, queued)
        self.bridge.guess_asked.connect(self._on_guess_asked, queued)
        self.controller = run_controller.RunController(on_session=self.tap.attach_session)

        self.docks = {}
        self._build_central()
        self._build_timeline()
        self._build_docks()
        self._build_menus()
        self._arrange_builtin()
        self._update_actions()
        self.stats_view.set_title(self.inputs.title or self.inputs.preset)
        self._restore_settings()

    # -- building -------------------------------------------------------------------

    def _build_central(self):
        """The central widget: a vertical splitter, the sequence pane (P1,
        `sequence_view.py`) over the cytoplasm (P2, `cytoplasm_view.py`, item 05)
        (PANES.md section 3). The sequence pane's box feeds `enter_terms`, its guess
        panel `answer_guess` (G-008, G-015), and a click on a term cell highlights the
        term's glint in the cytoplasm and selects it in the shared `selection`
        (G-023, G-025; item 06). The cytoplasm's scene is the one `paint_all` settles
        (Numbo GUI, `paint.py`)."""
        self.sequence_view = SequenceView()
        self.sequence_view.term_typed.connect(self.enter_terms)
        self.sequence_view.answered.connect(self.answer_guess)
        self.guess_panel = self.sequence_view.guess_panel
        self.cytoplasm_view = CytoplasmCanvas(self.history, selection=self.selection)
        self.sequence_view.cell_clicked.connect(self.cytoplasm_view.highlight_term)
        self._scenes = (self.cytoplasm_view.scene(),)
        self.splitter = QSplitter(Qt.Orientation.Vertical)
        self.splitter.setObjectName("central-splitter")
        self.splitter.addWidget(self.sequence_view)
        self.splitter.addWidget(self.cytoplasm_view)
        self.splitter.setStretchFactor(0, 1)
        self.splitter.setStretchFactor(1, 3)
        self.setCentralWidget(self.splitter)
        self.add_view(self.sequence_view)
        self.add_view(self.cytoplasm_view)

    def _build_timeline(self):
        """The timeline toolbar along the bottom (P10, `timeline.py`, item 12; Numbo
        GUI, `main_window.py`): its seeks and jumps come to the window, which refuses
        them while a run is live (G-042, G-043)."""
        self.timeline = TimelineBar(self.history)
        self.timeline.seek_requested.connect(self._seek_from_timeline)
        self.timeline.goto_requested.connect(self.jump_to)
        self.timeline_bar = QToolBar("Timeline")
        self.timeline_bar.setObjectName(TIMELINE_NAME)
        self.timeline_bar.addWidget(self.timeline)
        self.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self.timeline_bar)

    def _build_docks(self):
        """One dock per pane of PANES: the stats dock holds its view (`stats_view.py`,
        item 03; G-016), the console dock its (`console_view.py`, item 04; G-018,
        G-019), the taskrack dock its (`taskrack_view.py`, item 07; G-027, G-028), the
        socratoplasm dock its (`socratoplasm_view.py`, item 08; G-030, G-031: a hovered
        bond lights its arc in the cytoplasm), the hypothesis dock its
        (`hypothesis_view.py`, item 09; G-033 to G-035), the platoplasm dock its
        (`platoplasm_view.py`, item 10; G-036 to G-038), the reformulation dock its
        (`reformulation_view.py`, item 11; G-039 to G-041: a click on a card seeks, a
        hovered card selects what it names), the log dock its (`log_view.py`, item 12;
        G-044, G-045: a double-click seeks), the others a placeholder until their item
        (G-011)."""
        for attr, name, title, pane, pages, area, _under, _shown in PANES:
            if attr == "controls":
                self.controls = ControlsPanel()
                dock = self._dock(title, name, self.controls, AREAS[area])
                dock.setToolTip(f"{pane} {title}: {self.controls.pages}")
                self.controls_view = self.controls
                self.controls_dock = dock
                self._wire_controls()
                continue            # the panel takes no events (G-049)
            if attr == "stats":
                view = StatsView(self.history)
            elif attr == "taskrack":
                view = TaskrackView(self.history, selection=self.selection)
            elif attr == "socratoplasm":
                view = SocratoplasmView(self.history, selection=self.selection)
                view.bond_hovered.connect(self.cytoplasm_view.highlight)
            elif attr == "hypothesis":
                view = HypothesisView(self.history)
            elif attr == "platoplasm":
                view = PlatoplasmView(self.history, selection=self.selection)
            elif attr == "reformulation":
                view = ReformulationView(self.history)
                view.card_clicked.connect(self._seek_from_card)
                view.names_hovered.connect(self._hover_names)
            elif attr == "console":
                view = ConsoleView(self.history)
                view.line_typed.connect(self.enter_console_line)
                view.command_requested.connect(self.show_command)
                view.line_activated.connect(self._seek_from_console)
            elif attr == "log":
                view = LogView(self.history)
                view.event_activated.connect(self._seek_from_log)
            elif attr == "config":
                view = ConfigView(self.config_table)
                view.edits_changed.connect(self._config_edited)
            else:
                view = PanePlaceholder(title, pane, pages)
            dock = self._dock(title, name, view, AREAS[area])
            dock.setToolTip(f"{pane} {title}: {pages}")
            setattr(self, f"{attr}_view", view)
            setattr(self, f"{attr}_dock", dock)
            self.add_view(view)

    def _wire_controls(self):
        """The controls pane's buttons, slider and inputs (Numbo GUI, `main_window.py`;
        G-049)."""
        c = self.controls
        c.play_button.clicked.connect(self.play)
        c.pause_button.clicked.connect(self.pause)
        c.step_button.clicked.connect(self.step)
        c.step_task_button.clicked.connect(self.step_task)
        c.step_term_button.clicked.connect(self.step_term)
        c.run_to_end_button.clicked.connect(self.run_to_end)
        c.stop_button.clicked.connect(self.stop)
        c.delay_changed.connect(self.controller.set_delay)
        self.controller.set_delay(c.delay)
        c.inputs_changed.connect(self._inputs_from_controls)
        c.base_config = self.config_table.config()
        self._input_error = ""
        c.show_inputs(self.inputs)

    def _dock(self, title, name, widget, area=Qt.DockWidgetArea.LeftDockWidgetArea):
        """A dock with an objectName, so that the window state can be saved (Numbo
        GUI, `main_window.py`)."""
        dock = QDockWidget(title, self)
        dock.setObjectName(name)
        dock.setWidget(widget)
        self.addDockWidget(area, dock)
        self.docks[name] = dock
        return dock

    def _build_menus(self):
        """Session, Edit, Run, View, Presets and Help (PANES.md section 4; Numbo GUI).
        Actions whose item has not come yet are disabled and say which item."""
        bar = self.menuBar()
        session = bar.addMenu("&Session")
        self.open_action = self._action(session, "&Open session…", self._open_dialog,
                                        QKeySequence.StandardKey.Open)
        self.save_action = self._action(session, "&Save session…", self._save_dialog,
                                        QKeySequence.StandardKey.Save, enabled=False)
        self.close_replay_action = self._action(session, "&Close replay", self.close_replay)
        session.addSeparator()
        self.save_transcript_action = self._action(session, "Save &transcript…",
                                                   self._save_transcript_dialog)
        self.save_events_action = self._action(session, "Save &events (JSON lines)…",
                                               self._save_events_dialog, enabled=False)
        session.addSeparator()
        self._action(session, "&Quit", self.close, QKeySequence.StandardKey.Quit)

        edit = bar.addMenu("&Edit")
        self.configuration_action = self._action(edit, "&Configuration", self.show_configuration)
        self.copy_action = self._action(edit, "Cop&y the transcript", self.copy_transcript,
                                        QKeySequence.StandardKey.Copy)

        self.run_menu = bar.addMenu("&Run")
        for text, keys, slot in (("Play / Pause", "Space", self.play_pause),
                                 ("Step", "Right", self.step_or_forward),
                                 ("Step task", "Ctrl+Right", self.step_task),
                                 ("Step term", "Shift+Right", self.step_term),
                                 ("Run to end", "", self.run_to_end),
                                 ("Stop", "Esc", self.stop),
                                 (None, None, None),
                                 ("Back one event", "Left", self.back_one),
                                 (None, None, None),
                                 ("New run", "Ctrl+N", self.new_run)):
            if text is None:
                self.run_menu.addSeparator()
            else:
                self._action(self.run_menu, text, slot, QKeySequence(keys) if keys else None)

        self.view_menu = bar.addMenu("&View")
        for _attr, name, *_ in PANES:
            self.view_menu.addAction(self.docks[name].toggleViewAction())
        self.view_menu.addAction(self.timeline_bar.toggleViewAction())
        self.view_menu.addSeparator()
        self.layers_menu = self.view_menu.addMenu("&Cytoplasm layers")
        for action in self.cytoplasm_view.layer_actions.values():
            self.layers_menu.addAction(action)
        self.view_menu.addSeparator()
        self.reset_layout_action = self._action(self.view_menu, "&Reset layout", self.reset_layout)
        self.reset_builtin_action = self._action(self.view_menu, "Reset to &built-in layout",
                                                 self.reset_builtin_layout)
        self.save_layout_action = self._action(self.view_menu,
                                               "Save layout as &default for this code…",
                                               self._save_default_layout_dialog)

        self.presets_menu = bar.addMenu("&Presets")
        for run_id in presets.appendix_ids():
            inputs = presets.appendix_inputs(run_id)
            self._action(self.presets_menu, inputs.title,
                         functools.partial(self.set_inputs, inputs))
        self.presets_menu.addSeparator()
        self.interactive_action = self._action(
            self.presets_menu, "Type the terms yourself (interactive)", self.choose_interactive)
        self.presets_menu.addSeparator()
        self.group_menus = {}
        for entry in self.controls.catalogue:
            if entry.group in ("appendix", "custom"):
                continue
            menu = self.group_menus.get(entry.group)
            if menu is None:
                menu = self.presets_menu.addMenu(cm.GROUP_TITLES.get(entry.group, entry.group))
                self.group_menus[entry.group] = menu
            action = self._action(menu, entry.label,
                                  functools.partial(self.choose_preset, entry.id))
            action.setToolTip(entry.tooltip)
            action.setStatusTip(entry.result_text)

        help_menu = bar.addMenu("&Help")
        self._action(help_menu, "&What am I looking at?", self.show_guide)
        self._action(help_menu, "&About", self.show_about)

    def _action(self, menu, text, slot, shortcut=None, enabled=True):
        action = QAction(text, self)
        if shortcut is not None:
            action.setShortcut(shortcut)
        action.triggered.connect(slot)
        action.setEnabled(enabled)
        menu.addAction(action)
        return action

    # -- the layout (ANSWERS.md 3, 8) ---------------------------------------------------

    def _arrange_builtin(self):
        """The built-in arrangement of PANES.md section 3: every dock back in its area,
        the tab groups, the default visibility, the sizes (G-010). Before the window is
        shown only the visibility is set and the arrangement waits for the first
        `showEvent` (ANSWERS.md 10.1, 10.2; G-052)."""
        self.layout_source = "built-in"
        if not self._shown:
            for _attr, name, _title, _pane, _pages, _area, _under, shown in PANES:
                self.docks[name].setVisible(shown)
            self._arrange_pending = True
            return
        self._arrange_pending = False
        docks = [self.docks[name] for _a, name, *_ in PANES]
        for dock in docks:
            dock.setFloating(False)
            self.removeDockWidget(dock)
        self.removeToolBar(self.timeline_bar)
        for _attr, name, _title, _pane, _pages, area, under, shown in PANES:
            dock = self.docks[name]
            self.addDockWidget(AREAS[area], dock)
            if under is not None:
                self.tabifyDockWidget(self.docks[under], dock)
            dock.setVisible(shown)
        self.splitDockWidget(self.platoplasm_dock, self.console_dock, Qt.Orientation.Horizontal)
        self.tabifyDockWidget(self.console_dock, self.log_dock)
        self.hypothesis_dock.raise_()
        self.console_dock.raise_()
        self.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self.timeline_bar)
        self.timeline_bar.show()
        horizontal, vertical = Qt.Orientation.Horizontal, Qt.Orientation.Vertical
        self.resizeDocks([self.controls_dock, self.stats_dock], [LEFT_WIDTH] * 2, horizontal)
        self.resizeDocks([self.taskrack_dock, self.socratoplasm_dock, self.hypothesis_dock],
                         [RIGHT_WIDTH] * 3, horizontal)
        self.resizeDocks([self.controls_dock], [CONTROLS_HEIGHT], vertical)
        self.resizeDocks([self.platoplasm_dock], [BOTTOM_HEIGHT], vertical)
        self.resizeDocks([self.platoplasm_dock, self.console_dock],
                         [PLATOPLASM_WIDTH, PLATOPLASM_WIDTH], horizontal)
        self.splitter.setSizes(list(CENTRAL_SIZES))

    def showEvent(self, event):
        """The first show makes the built-in arrangement if no restored layout replaced
        it (G-052); Numbo GUI's `showEvent` sizes its docks here for the same reason."""
        super().showEvent(event)
        if not self._shown:
            self._shown = True
            if self._arrange_pending:
                self._arrange_builtin()

    def apply_layout(self, record) -> bool:
        """Restore a `LayoutRecord`'s arrangement; False (and the built-in arrangement
        back) if it names a dock the window lacks or Qt cannot restore it (G-010)."""
        missing = layout_file.missing_docks(record, self.docks)
        if missing:
            self._say(f"the layout names docks this window lacks ({', '.join(missing)}); "
                      f"using the built-in layout")
            return False
        if not self.restoreState(QByteArray(record.state), STATE_VERSION):
            self._arrange_builtin()
            self._say("the layout could not be restored (saved by another version of the "
                      "window?); using the built-in layout")
            return False
        self._arrange_pending = False       # the restored state replaces the built-in
        return True

    def _apply_default_layout(self) -> bool:
        """The repository default, `layout_path`, if it is there and usable (G-010)."""
        if self.layout_path is None:
            return False
        try:
            record = layout_file.read_layout(self.layout_path)
        except layout_file.LayoutError as error:
            self._say(f"the default layout file cannot be used ({error}); "
                      f"using the built-in layout")
            return False
        if record is None or not self.apply_layout(record):
            return False
        self.layout_source = "default file"
        return True

    def layout_record(self):
        """The arrangement now, as a `LayoutRecord` for the default layout file: the
        state bytes, each dock's visibility, area and tab group, the timeline, the Qt
        version; no geometry (ANSWERS.md 8; G-010)."""
        docks = []
        for _attr, name, *_ in PANES:
            dock = self.docks[name]
            area = "floating" if dock.isFloating() else AREA_NAMES.get(
                self.dockWidgetArea(dock), "floating")
            docks.append(layout_file.DockPlacement(
                name, dock.toggleViewAction().isChecked(), area,
                tuple(d.objectName() for d in self.tabifiedDockWidgets(dock))))
        toolbars = ((TIMELINE_NAME, self.timeline_bar.toggleViewAction().isChecked()),)
        return layout_file.LayoutRecord(layout_file.VERSION, qVersion(),
                                        bytes(self.saveState(STATE_VERSION)), tuple(docks),
                                        toolbars)

    def save_layout_as_default(self, path=None):
        """Write the arrangement now as the default layout file at `path` (the
        repository's by default); returns the path. `OSError` if it is not writable
        (ANSWERS.md 8; G-010)."""
        path = Path(path) if path is not None else self.layout_path
        if path is None:
            raise OSError("this window has no default layout file")
        layout_file.write_layout(path, self.layout_record())
        self._say(f"layout saved as the default for this code: {path}")
        return path

    def reset_layout(self):
        """View > Reset layout: the repository default if there is one, else the
        built-in; the saved state is cleared (ANSWERS.md 3, 8; G-010)."""
        self._arrange_builtin()
        self._apply_default_layout()
        self._forget_saved_state()
        self._say(f"layout reset to the {self.layout_source} layout")

    def reset_builtin_layout(self):
        """View > Reset to built-in layout (ANSWERS.md 8; G-010)."""
        self._arrange_builtin()
        self._forget_saved_state()
        self._say("layout reset to the built-in layout")

    def _forget_saved_state(self):
        if self.settings is not None:
            self.settings.remove("window/state")
            self.settings.sync()

    def _save_default_layout_dialog(self):
        """Confirm, naming the file and what saving it means; on a read-only package
        directory, say so and offer another place (ANSWERS.md 8)."""
        path = self.layout_path
        if path is None:
            return
        answer = QMessageBox.question(
            self, "Save layout as default",
            f"Write the current arrangement of the docks to\n\n{path}\n\nIt becomes the "
            f"default layout for everyone who uses this copy of the code once the file is "
            f"committed. Your own layout (saved when the window closes) still takes "
            f"precedence on this machine.")
        if answer != QMessageBox.StandardButton.Yes:
            return
        try:
            self.save_layout_as_default(path)
        except OSError as error:
            QMessageBox.warning(self, "Cannot write the default layout",
                                f"{path} is not writable ({error}); the package may be "
                                f"installed read-only. Choose another file.")
            other, _ = QFileDialog.getSaveFileName(self, "Save the layout", "default_layout.json",
                                                   "JSON (*.json);;All files (*)")
            if other:
                try:
                    self.save_layout_as_default(other)
                except OSError as error2:
                    self._say(f"cannot save the layout: {error2}")

    # -- views (Numbo GUI) ------------------------------------------------------------

    def add_view(self, view):
        """The view gets every event on the GUI thread (`on_event`), then, if it has
        one, its `redraw()` is called before the event is acknowledged (Numbo GUI,
        `main_window.py`)."""
        self._views.subscribe(view)
        redraw = getattr(view, "redraw", None)
        if redraw is not None:
            if "now" in inspect.signature(redraw).parameters:
                redraw = functools.partial(redraw, now=False)
            self._redraws.append(redraw)
        positioner = getattr(view, "set_position", None)
        if positioner is not None:
            self._positioners.append(positioner)
        return view

    def _position_views(self, index):
        """Tell the views that read the snapshot in force which event comes next
        (G-016)."""
        for positioner in self._positioners:
            positioner(index)

    def _view_error(self, error):
        run_controller.report_to_stderr(error)
        self._say(f"a view failed: {error.exception!r}")

    def _say(self, message):
        self.statusBar().showMessage(message)

    # -- starting runs ------------------------------------------------------------------

    def set_inputs(self, inputs) -> None:
        """The inputs of the next run (`models/presets.py`), with the configuration
        pane's edits applied to their configuration (the caps and the trace kept,
        G-051); shown in the title, the controls pane and the configuration pane
        (G-009, G-049)."""
        if self.config_table.edits:
            inputs = dataclasses.replace(inputs, config=self.config_table.config(inputs.config))
        self.inputs = inputs
        self._input_error = ""
        self.controls.show_inputs(inputs)
        self.controls.show_error("")
        self._show_next_run(inputs)

    def _show_next_run(self, inputs):
        """The next run in the title (marked when its configuration differs from the
        defaults, ANSWERS.md 5; G-051), the stats pane, the configuration pane and the
        status bar."""
        self.setWindowTitle(title_for(inputs))
        self.stats_view.set_title(inputs.title or inputs.preset)
        self.config_view.show_config(inputs.config)
        self._say(f"next run: {inputs.title or inputs.preset}, seed {inputs.seed}")

    def _config_edited(self):
        """The configuration pane's edits changed: the next run's configuration is the
        edits over the defaults, with the controls' caps and trace (ANSWERS.md 5;
        G-051). A run in progress is not touched."""
        self.controls.base_config = self.config_table.config()
        if self.replay_source is not None:
            return
        if not self._inputs_from_controls():
            # the controls do not parse: keep the inputs, with the new configuration
            self.inputs = dataclasses.replace(
                self.inputs, config=self.config_table.config(self.inputs.config))
            self._show_next_run(self.inputs)

    def choose_preset(self, preset) -> None:
        """A preset of the Presets menu or the list, by id: the controls show it with its
        default seed and trace, and the next run is its (G-048)."""
        self.controls.choose_preset(preset)

    def _inputs_from_controls(self) -> bool:
        """The controls changed: the next run's inputs are theirs; an invalid text is
        shown in the panel and refuses the next start (Numbo GUI; G-049)."""
        try:
            inputs = self.controls.inputs()
        except cm.InputError as error:
            self._input_error = str(error)
            self.controls.show_error(self._input_error)
            return False
        self._input_error = ""
        self.inputs = inputs
        self.controls.show_error("")
        self._show_next_run(inputs)
        return True

    def choose_interactive(self):
        """Presets > Type the terms yourself: the next run reads its terms from the
        sequence pane's box, on the seed of the inputs now (G-015)."""
        self.set_inputs(presets.interactive_inputs(self.inputs.seed))

    def start_session(self, inputs, paused=False, observers=()) -> None:
        """Start a run of `inputs` through the controller: scripted lines and answers, or
        (`inputs.interactive`) guesses answered in the guess panel (G-008, G-009), and,
        when `inputs.lines` is None, terms typed in the sequence pane through a
        `TermFeed` (G-015). `observers` are subscribed to the controller for this run
        only, after the tap (a replay's checker, G-047). Raises `EngineBusy` if the
        previous run did not stop."""
        self._connect(recording=True)
        for observer in observers:
            self._run_observers.append(self.controller.subscribe(observer))
        self._started = inputs
        self._show_started(inputs)
        self.console_view.begin(inputs.lines)
        self.feed = TermFeed() if inputs.lines is None else None
        self.sequence_view.set_queue(())
        answerer = None
        if inputs.interactive:
            generation, bridge = self._generation, self.bridge

            def make(console):
                made = InteractiveAnswerer(
                    console, lambda pending: bridge.guess_asked.emit(generation, pending))
                self._answerer = made       # on the worker, before any guess
                return made
            answerer = make
        elif inputs.continuation is not None:
            continuation = tuple(inputs.continuation)

            def make_judge(console):
                return bench.ContinuationAnswerer(console, continuation)
            answerer = make_judge
        self.controller.start_session(inputs.config, inputs.seed, lines=inputs.lines,
                                      answers=inputs.answers, answerer=answerer,
                                      inp=self.feed, paused=paused)

    def _show_started(self, inputs):
        """The inputs a run was started with, in the controls (the preset, or `custom`),
        the title and the stats pane's Run row, whichever way it was started (ANSWERS.md
        10.3; G-054). Outside a replay they become the window's `inputs`, exactly as
        used (the configuration pane's edits are not reapplied)."""
        self.controls.show_inputs(inputs)
        self.stats_view.set_title(inputs.title or inputs.preset)
        if self.replay_source is not None:
            self.setWindowTitle(f"{TITLE} — replaying {self._replay.name}: "
                                f"{inputs.preset}, seed {inputs.seed}")
            return
        self.inputs = inputs
        self._input_error = ""
        self.controls.show_error("")
        self.setWindowTitle(title_for(inputs))
        self.config_view.show_config(inputs.config)

    def _start(self, mode):
        """Start a run of the window's inputs, paused if `mode` is a step; False (and the
        error shown) if it cannot start (Numbo GUI)."""
        self._say("")
        if self._input_error:
            self.controls.show_error(self._input_error)
            self._say(self._input_error)
            return False
        try:
            self.start_session(self.inputs, paused=mode != "play")
        except (ValueError, run_controller.EngineBusy) as error:
            self._say(str(error))
            self.controls.show_error(str(error))
            return False
        self.controls.show_error("")
        return True

    def _connect(self, recording):
        """A new generation of the controller's callbacks. `recording`: the deliveries
        are the tap's new events (a live run); else they move through the history (a
        replay's playback, item 13) (Numbo GUI)."""
        # The old run is stopped and joined before the generation moves on, so none of
        # its callbacks can carry the new generation.
        self._cancel_inputs()
        self.controller.stop()
        self._generation += 1
        generation = self._generation
        bridge = self.bridge
        self.controller.deliver = lambda d: bridge.delivered.emit(generation, d)
        self.controller.on_state = lambda s: bridge.state_changed.emit(generation, s)
        self.controller.on_finished = lambda o: bridge.finished.emit(generation, o)
        if recording and not self._tap_subscribed:
            self.controller.subscribe(self.tap)
        elif not recording and self._tap_subscribed:
            self.controller.unsubscribe(self.tap)
        self._tap_subscribed = recording
        for observer in self._run_observers:
            self.controller.unsubscribe(observer)
        self._run_observers = []
        self._recording = recording
        self._answerer = None
        self.outcome = None

    def _live(self):
        return self.controller.is_alive() and self.controller.state in ("running", "paused")

    def _begin(self, mode):
        if self.replay_source is not None:
            return self._start_replay(mode)
        return self._start(mode)

    def play(self):
        """Play: on with the delay, starting a run if none is live (Numbo GUI)."""
        if self._live():
            self.controller.play()
        else:
            self._begin("play")

    def pause(self):
        """Pause a playing run (Numbo GUI)."""
        if self._live():
            self.controller.pause()

    def step(self):
        """One event (Numbo GUI)."""
        if self._live() or self._begin("step"):
            self.controller.step()

    def step_task(self):
        """On through the next `chosen` event [M86 p. 41] (G-005)."""
        if self._live() or self._begin("step"):
            self.controller.step_task()

    def step_term(self):
        """On through the next `prompt` event [M86 p. 165] (G-005)."""
        if self._live() or self._begin("step"):
            self.controller.step_term()

    def run_to_end(self):
        """On with no delay (Numbo GUI)."""
        if self._live() or self._begin("run-to-end"):
            self.controller.run_to_end()

    def stop(self):
        """Stop the run and join the worker; a pending guess and the term feed are
        cancelled first (Numbo GUI; G-008, G-015)."""
        self._cancel_inputs()
        self.controller.stop()
        self._update_actions()

    def new_run(self):
        """Ctrl+N: a fresh run of the inputs, paused at its first event (PANES.md
        section 4; G-013; Numbo GUI's step starts a run paused)."""
        self.stop()
        if self._begin("step"):
            self.controller.step()

    def play_pause(self):
        """Space: pause a playing run, else play (Numbo GUI)."""
        if self._live() and self.controller.state == "running":
            self.controller.pause()
        else:
            self.play()

    def step_or_forward(self):
        """The right arrow: step a run; with no run live and the views back in the
        history, one event forward (Numbo GUI)."""
        if not self._live() and 0 <= self.position < len(self.history) - 1:
            self.seek(self.position + 1)
        else:
            self.step()

    def back_one(self):
        """The left arrow: one event back in the history, when no run is live (Numbo
        GUI)."""
        if self.position > 0:
            self.seek(self.position - 1)

    # -- the guess (G-008) ----------------------------------------------------------------

    def answer_guess(self, answer) -> bool:
        """The user's answer to the guess shown, `ok` or `no` [M86 pp. 166, 175]: releases
        the worker; False if no guess waits (G-008)."""
        answerer = self._answerer
        if answerer is None or not answerer.give(answer):
            self._say("no guess is waiting for an answer")
            return False
        self.guess_panel.clear()
        return True

    def _cancel_guess(self):
        answerer = self._answerer
        if answerer is not None:
            answerer.cancel()
        self.guess_panel.clear()

    def _cancel_inputs(self):
        """Cancel a pending guess and the term feed, so that a worker waiting on either
        unwinds with `RunStopped` (G-008, G-015)."""
        self._cancel_guess()
        feed = self.feed
        if feed is not None:
            feed.cancel()
        self.sequence_view.set_accepting(False)

    # -- the terms and the commands (G-015, G-017, G-019) ----------------------------------------

    def _reads_the_gui(self) -> bool:
        """Whether a live run reads its console lines from the GUI (an interactive run's
        feed) (G-015, G-019)."""
        return self.feed is not None and self._live()

    def _put_inputs(self, words) -> int:
        """Console lines for the live interactive run: each goes to the console model
        and to the feed, in that order, so both read the same lines in the same order;
        returns how many (G-015, G-019)."""
        feed = self.feed
        for word in words:
            self.console_view.model.put(word)
            feed.put(word)
        self.console_view.redraw(now=True)
        self.sequence_view.set_queue(feed.queued)
        if words:
            self._say(f"queued: {' '.join(feed.queued) or ' '.join(words)}")
        return len(words)

    def enter_terms(self, text) -> int:
        """Terms typed in the sequence pane: queued on the feed of the live interactive
        run, one per prompt; returns how many were queued, 0 (and a status line) when no
        such run is live [M86 p. 37] (G-015)."""
        if not self._reads_the_gui():
            self._say("no interactive run is waiting for terms: Presets > Type the terms "
                      "yourself, then Play")
            return 0
        return self._put_inputs(split_words(text))

    def enter_console_line(self, text) -> int:
        """A line typed at the console's command line, what the CLI takes at its prompt
        (DECISION D-054): with an interactive run live every word is a session input,
        read at the prompts in order; otherwise a command prints the state at the cursor
        and is not an input, and a term is refused with a status line. Returns how many
        words became inputs (G-019)."""
        words = split_words(text)
        if self._reads_the_gui():
            return self._put_inputs(words)
        for word in words:
            kind, value = parse_line(word)
            if kind == "command":
                self.show_command(value)
            else:
                self._say(f"{word!r} needs an interactive run: Presets > Type the terms "
                          f"yourself, then Play (a command prints the state at the cursor)")
        return 0

    def end_input(self) -> None:
        """End the interactive input: the session reads the end of the stream and ends
        with the outcome `eof` (DECISION D-069; G-015)."""
        if self.feed is not None:
            self.feed.close()

    def show_command(self, name) -> list:
        """A show command, from a button or the command line [M86 pp. 166-179]: with an
        interactive run live, a session input, echoed with its output in the console
        when the session reads it; otherwise printed in the console at once for the
        snapshot in force at `position` (`models/commands.py`; G-017, G-019). Returns
        the command's lines at `position`."""
        if self._reads_the_gui():
            self._put_inputs([name])
            snapshot = self.history.state_at(self.position) if self.position >= 0 else None
            return commands.command_lines(snapshot, name)
        return self.console_view.print_command(name)

    def _seek_from_console(self, index) -> None:
        """A click on a console line: seek to its event when no run is live (PANES.md
        P8; G-020)."""
        if self._live():
            self._say("stop the run to seek from the console")
        elif not self.seek(index):
            self._say(f"no event {index} in the history")

    def _seek_from_card(self, index) -> None:
        """A click on a storyboard card: seek to its event when no run is live
        (PANES.md P7; G-041)."""
        if self._live():
            self._say("stop the run to seek from the storyboard")
        elif not self.seek(index):
            self._say(f"no event {index} in the history")

    def _hover_names(self, names) -> None:
        """A hovered storyboard card selects the gnoths and gloms it names in the
        shared selection, so the socratoplasm and the cytoplasm show them; leaving it
        restores what was selected before (PANES.md P7; G-025, G-041)."""
        names = tuple(names)
        if names:
            if self._hover_saved is None:
                sel = self.selection
                self._hover_saved = (sel.kind, sel.name, tuple(sel.members))
            self.selection.select("operation", names[0], names)
        elif self._hover_saved is not None:
            kind, name, members = self._hover_saved
            self._hover_saved = None
            if name is None:
                self.selection.clear()
            else:
                self.selection.select(kind, name, members)

    def save_transcript(self, path, mode=None) -> str:
        """Write the console's transcript, in the mode shown or `mode`, to the file;
        returns the text written (PANES.md P8; G-019)."""
        text = self.console_view.text(mode)
        with open(path, "w", encoding="utf-8") as out:
            out.write(text)
        self._say(f"transcript saved: {path}")
        return text

    def _save_transcript_dialog(self):
        """Session > Save transcript…: a file dialog, then `save_transcript` (Numbo GUI's
        save dialogs; G-019)."""
        path, _ = QFileDialog.getSaveFileName(self, "Save the transcript", "transcript.txt",
                                              "Text (*.txt);;All files (*)")
        if path:
            try:
                self.save_transcript(path)
            except OSError as error:
                self._say(f"cannot save the transcript: {error}")

    def copy_transcript(self) -> str:
        """Edit > Copy: the console's transcript, in the mode shown, to the clipboard;
        returns it (PANES.md P8; G-019)."""
        text = self.console_view.text()
        QApplication.clipboard().setText(text)
        self._say(f"transcript copied: {self.console_view.list.row_count()} lines")
        return text

    def _on_guess_asked(self, generation, pending):
        if self._closed or generation != self._generation:
            return
        self.guess_panel.ask(pending)
        self._say(f"I have a guess! hypothesis: {pending.form}  {pending.block}  "
                  f"(enter no if wrong, ok if right)")
        self.guess_shown.emit(pending)

    # -- the worker's signals, on the GUI thread (Numbo GUI) -------------------------------

    def _on_delivered(self, generation, delivery):
        if self._closed or generation != self._generation:
            return
        event = delivery.event
        self.position = delivery.index
        if self._recording and len(self.history) == 1:
            self._update_actions()
        self.sequence_view.set_accepting(self.feed is not None)
        self._position_views(delivery.index)
        self._views.publish(event)
        if event.kind == "refuted" and self.reformulation_view.model.first_refutation == delivery.index:
            self.reformulation_dock.raise_()       # PANES.md P7; G-041
        self._draw(event)
        self.controller.acknowledge(delivery.ticket)

    def _draw(self, event):
        """Every view draws the event at `position`, now: each marks itself dirty, then
        the window is painted once (`paint.paint_all`) (Numbo GUI)."""
        feed = self.feed
        self.sequence_view.set_queue(feed.queued if feed is not None else ())
        for redraw in self._redraws:
            try:
                redraw()
            except Exception as exception:
                self._view_error(ObserverError(redraw, event, exception))
        index = self.position
        self.timeline.set_position(index)
        paint.paint_all(self._scenes)
        self.event_drawn.emit(index)

    def _on_state(self, generation, state):
        if self._closed or generation != self._generation:
            return
        self.state = state
        self.stats_view.set_state(state)
        self.controls.set_state(state)
        self._update_actions()
        self.run_state_changed.emit(state)

    def _on_finished(self, generation, outcome):
        if self._closed or generation != self._generation:
            return
        self.outcome = outcome
        self._answerer = None
        self.guess_panel.clear()
        self.sequence_view.set_accepting(False)
        self.stats_view.finish(outcome)
        self.console_view.finish(outcome)
        if outcome.error is not None:
            self._say(f"the run ended with an error: {outcome.error}")
        elif outcome.stopped:
            self._say(f"stopped after {outcome.events} events")
        elif outcome.result is not None:
            self._say(outcome.result["summary"])
        replay = self._replay
        if replay is not None and replay.checker is not None:
            self._finish_replay(replay, outcome)
        self._update_actions()
        self.run_finished.emit(outcome)

    # -- the history: seeking (Numbo GUI) ---------------------------------------------------

    def seek(self, index) -> bool:
        """Show event `index` of the history; False if a run is live or there is no such
        event (Numbo GUI)."""
        events = self.history.events
        if self._live() or not 0 <= index < len(events):
            return False
        start = self.position + 1 if 0 <= self.position < index else 0
        publish = self._views.publish
        for i in range(start, index + 1):
            self._position_views(i)
            publish(events[i])
        self.position = index
        self._draw(events[index])
        return True

    def _update_actions(self):
        self.close_replay_action.setEnabled(self.replay_source is not None)
        has_history = bool(self.history.events)
        self.save_events_action.setEnabled(has_history)
        self.save_action.setEnabled(has_history)
        self.timeline.set_scrubbable(has_history and not self._live())

    def _seek_from_timeline(self, index) -> None:
        """The timeline's slider, buttons, markers and parse cells: seek when no run is
        live (Numbo GUI, `main_window.py`; G-043)."""
        if self._live():
            self._say("stop the run to move in the timeline")
        elif not self.seek(index):
            self._say(f"no event {index + 1} in the history")

    def _seek_from_log(self, index) -> None:
        """A double-click or Enter on a log row: seek to its event when no run is live
        (PANES.md P9; Numbo GUI, `main_window.py`; G-045)."""
        if self._live():
            self._say("stop the run to seek from the event log")
        elif not self.seek(index):
            self._say(f"no event {index + 1} in the history")

    def jump_to(self, kind, n) -> bool:
        """The timeline's go-to box: seek to term `n`'s entry, to the task with clock
        `n` or to event number `n` (PANES.md P10; Numbo GUI's `jump_to_iteration`;
        G-043). False (and a status line) when there is no such event or a run is
        live."""
        if self._live():
            self._say("stop the run to move in the timeline")
            return False
        index = tm.goto_index(self.history, kind, n)
        if index is None:
            self._say(f"no {kind} {n} in the history")
            return False
        return self.seek(index)

    def save_events(self, path) -> int:
        """Write the history's events as JSON lines, one `event_record` per line, the
        format of `python3 -m seekwhence --json` (DECISION D-069; PANES.md P9); returns
        how many were written (G-045)."""
        events = list(self.history.events)
        with open(path, "w", encoding="utf-8") as out:
            for event in events:
                out.write(json.dumps(event_record(event), separators=(",", ":")) + "\n")
        self._say(f"{len(events)} events saved: {path}")
        return len(events)

    def _save_events_dialog(self):
        """Session > Save events (JSON lines)…: a file dialog, then `save_events` (Numbo
        GUI's save dialogs; G-045)."""
        path, _ = QFileDialog.getSaveFileName(self, "Save the events", "events.jsonl",
                                              "JSON lines (*.jsonl);;All files (*)")
        if path:
            try:
                self.save_events(path)
            except OSError as error:
                self._say(f"cannot save the events: {error}")

    # -- sessions and replay (item 13; G-046, G-047; Numbo GUI) -------------------------------

    def run_line(self) -> sf.RunLine:
        """The run line of the run in the history: the inputs it was started from
        (`_started`), with the lines typed and the answers given for an interactive run
        (`models/session_file.py`'s `run_line_of`; G-046)."""
        typed = self.console_view.model.inputs
        answered = [e.answer for e in self.history.events if e.kind == "answered"]
        return sf.run_line_of(self._started, typed, answered)

    def save_session(self, path) -> bool:
        """Write the history with its run line to `path` as a session file, also part way
        through a run; False (and the error shown) if that fails (PANES.md section 5;
        Numbo GUI, `main_window.py`; G-046)."""
        events = list(self.history.events)
        if not events:
            self._say("nothing to save: no run yet")
            return False
        try:
            n = sf.write_session(str(path), self.run_line(), events)
        except OSError as error:
            self._say(f"cannot save the session: {error}")
            return False
        self._say(f"saved {n} events to {path}")
        return True

    def open_session(self, path) -> bool:
        """Open the session file `path` for replay: read and check it, then re-execute
        its run line through the controller, paused at its first event (Numbo GUI's
        `open_file` for an oracle trace); False (and the error shown) if it cannot be
        read (PANES.md section 5; G-047)."""
        path = str(path)
        name = os.path.basename(path)
        try:
            file = sf.read_session(path)
        except (sf.SessionError, OSError) as error:
            self._say(f"cannot open {name}: {error}")
            return False
        self._say("")
        self.replay_source = path
        self._replay = Replay(file, path, name, sf.run_inputs(file.run, f"replay of {name}"))
        self.setWindowTitle(f"{TITLE} — replaying {name}")
        self.controls.set_replay(name)
        self._live_inputs = self.inputs     # the inputs chosen before, for Close replay
        self.controls.show_error("")
        self.config_view.set_replay(file.run.config)    # the file's configuration (G-051)
        if file.run.engine != sf.__version__:
            self._say(f"{name} was saved by seekwhence {file.run.engine}; this is "
                      f"{sf.__version__}")
        if not self._start_replay("step"):
            return False
        self.controller.step()      # draw its start event
        self._update_actions()
        return True

    def _start_replay(self, mode) -> bool:
        """Re-execute the opened file's run line, paused if `mode` is a step, with a
        fresh `ReplayChecker` that stops the run at the first difference (G-047); False
        (and the error shown) if it cannot start."""
        replay = self._replay
        controller = self.controller
        replay.checker = sf.ReplayChecker(replay.file,
                                          on_divergence=lambda d: controller.stop(wait=False))
        try:
            self.start_session(replay.inputs, paused=mode != "play",
                               observers=[replay.checker])
        except (ValueError, run_controller.EngineBusy) as error:
            self._say(str(error))
            return False
        return True

    def _finish_replay(self, replay, outcome):
        """The re-run ended: verified, stopped by the user, or different from the file,
        in which case the stored events are shown (PANES.md section 5; G-047)."""
        checker = replay.checker
        divergence = checker.finish(stopped=outcome.stopped)
        name = replay.name
        if divergence is not None:
            replay.divergence = divergence
            self._show_stored(replay)
            return
        if outcome.stopped:
            self._say(f"replay of {name}: stopped after {outcome.events} events, "
                      f"{checker.seen} verified")
            self.stats_view.set_title(f"replay of {name}: stopped")
        elif checker.beyond:
            self._say(f"replay of {name}: the file's {checker.seen} events verified; it "
                      f"was saved part way, and the run went on for {checker.beyond} more")
            self.stats_view.set_title(f"replay of {name}: verified, saved part way")
        else:
            self._say(f"replay of {name}: verified, {checker.seen} events")
            self.stats_view.set_title(f"replay of {name}: verified")

    def _show_stored(self, replay):
        """The re-run differs from the file: warn, load the stored events into the
        history (no snapshots: the log, the console and the timeline show them whole,
        the structure panes only what the events alone give) and seek to the first
        difference (PANES.md section 5; G-047)."""
        divergence = replay.divergence
        file = replay.file
        self.history.load(file.events)
        self.console_view.begin(file.run.inputs)
        self.log_view.reload()
        self.position = -1
        if file.events:
            self.seek(min(divergence.index, len(file.events) - 1))
        message = (f"{replay.name} differs from the engine at event {divergence.index + 1} "
                   f"(line {divergence.line}): {divergence.message}. Showing the stored "
                   f"events; the structure panes are not available for this file.")
        self.stats_view.set_title(f"stored events of {replay.name} (differs at event "
                                  f"{divergence.index + 1})")
        self.stats_view.set_state("stored")
        self._say(message)
        QMessageBox.warning(self, "The replay differs from the file", message)

    def close_replay(self):
        """Back to live runs: the replay stops, the inputs are the ones chosen before,
        the history stays, to scrub (Numbo GUI, `main_window.py`; G-047)."""
        if self.replay_source is None:
            return
        self._cancel_inputs()
        self.controller.stop()
        self.replay_source = None
        self._replay = None
        self.controls.set_replay(None)
        self.config_view.set_replay(None)
        self.set_inputs(self._live_inputs or self.inputs)
        self._live_inputs = None
        self._say("replay closed: the next run is a live run")
        self._update_actions()

    def _open_dialog(self):
        """Session > Open session…: a file dialog, then `open_session` (Numbo GUI)."""
        path, _ = QFileDialog.getOpenFileName(self, "Open a session", "",
                                              "JSON lines (*.jsonl);;All files (*)")
        if path:
            self.open_session(path)

    def _save_dialog(self):
        """Session > Save session…: a file dialog, then `save_session` (Numbo GUI)."""
        path, _ = QFileDialog.getSaveFileName(self, "Save the session", "session.jsonl",
                                              "JSON lines (*.jsonl);;All files (*)")
        if path:
            self.save_session(path)

    # -- Edit and Help ----------------------------------------------------------------------

    def show_configuration(self):
        """Edit > Configuration: show and raise the Configuration dock (ANSWERS.md 5;
        PANES.md P13; the dock is hidden by default, G-010)."""
        self.config_dock.show()
        self.config_dock.raise_()

    def guide_html(self) -> str:
        """The page of Help > What am I looking at?: the pane-to-pages table of
        `docs/gui/panes.md` (`models/guide.py`), every pane with the pages behind it,
        the platoplasm network (P5) and the taskrack drawing (P6) marked GUI design, not
        Meredith's (PANES.md section 4; ANSWERS.md 4; G-055)."""
        try:
            rows = guide.read_guide()
        except (OSError, ValueError):       # installed without docs/: the panes' own pages
            rows = tuple(guide.GuideRow(pane, title, "", pages,
                                        guide.GUI_DESIGN if guide.GUI_DESIGN in pages
                                        else "")
                         for _a, _n, title, pane, pages, *_ in PANES)
        return guide.guide_html(rows)

    def show_guide(self):
        """Help > What am I looking at?: `guide_html` in a dialog (G-055)."""
        dialog = QDialog(self)
        dialog.setWindowTitle("What am I looking at?")
        dialog.setObjectName("guide-dialog")
        browser = QTextBrowser(dialog)
        browser.setObjectName("guide-text")
        browser.setOpenExternalLinks(False)
        browser.setHtml(self.guide_html())
        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Close, dialog)
        buttons.rejected.connect(dialog.reject)
        buttons.accepted.connect(dialog.accept)
        layout = QVBoxLayout(dialog)
        layout.addWidget(browser)
        layout.addWidget(buttons)
        dialog.resize(760, 560)
        self.guide_dialog = dialog
        dialog.show()
        return dialog

    def show_about(self):
        """Help > About (Numbo GUI)."""
        QMessageBox.about(self, "About Seek-Whence", about_text())

    # -- remembered state (QSettings; Numbo GUI) ----------------------------------------------

    def _restore_settings(self):
        """The window's geometry, layout and inputs as they were left, then the layout
        precedence of ANSWERS.md 8: the user's state, the repository default, the
        built-in. A value that does not read leaves the default."""
        s = self.settings
        restored = False
        note = ""
        if s is not None:
            geometry, state = s.value("window/geometry"), s.value("window/state")
            if isinstance(geometry, (QByteArray, bytes)):
                self.restoreGeometry(QByteArray(geometry))
            if isinstance(state, (QByteArray, bytes)):
                restored = self.restoreState(QByteArray(state), STATE_VERSION)
                if restored:
                    self.layout_source = "settings"
                    self._arrange_pending = False
                else:
                    self._arrange_builtin()
                    note = ("the saved layout is from another version of the window and "
                            "was ignored")
            self.config_table.from_settings({ct.SETTINGS_KEY: s.value(ct.SETTINGS_KEY)})
            self.controls.base_config = self.config_table.config()
            values = cm.ControlValues.from_settings(
                {f"controls/{key}": s.value(f"controls/{key}") for key in cm.SETTINGS_KEYS})
            self.controls.set_values(values)
            self.controller.set_delay(self.controls.delay)
            if not self._inputs_from_controls():
                self.set_inputs(presets.appendix_inputs(presets.DEFAULT_PRESET))
            self._say(note)
        if not restored:
            self._apply_default_layout()

    def _save_settings(self):
        s = self.settings
        if s is None:
            return
        s.setValue("window/geometry", self.saveGeometry())
        s.setValue("window/state", self.saveState(STATE_VERSION))
        for key, value in self.controls.values().to_settings().items():
            s.setValue(key, value)
        for key, value in self.config_table.to_settings().items():
            s.setValue(key, value)
        s.sync()

    def shutdown(self):
        """Save the settings (once), cancel a pending guess, stop the worker and wait for
        it (Numbo GUI; G-008). Returns whether the worker has ended."""
        if not self._closed:
            self._save_settings()
        self._closed = True
        self._cancel_inputs()
        return self.controller.stop()

    def closeEvent(self, event):
        """Closing the window is `shutdown` (Numbo GUI, `main_window.py`)."""
        self.shutdown()
        super().closeEvent(event)

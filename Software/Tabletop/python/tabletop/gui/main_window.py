"""The main window: dockable panes over a run controller (Numbo GUI, `main_window.py`,
adopted through the Seek-Whence GUI's; ANSWERS.md 2, 5; PANES.md sections 1, 3 and 4).

Threading and the redraw handshake (Numbo GUI):
  - The `RunController` (`models/run_controller.py`) runs the engine on its worker
    thread. Its `deliver`, `on_state` and `on_finished` callbacks run on the worker; they
    only emit the `Bridge`'s signals, which are connected queued, so the slots run on the
    GUI thread, in emission order.
  - The views are observers of a GUI-side `Subject`. For each delivery the window
    publishes the event to them, calls each view's `redraw(now=False)` (it marks itself
    dirty), paints the window once (`paint.py`: each view exactly once, now), and only
    then acknowledges the delivery. The worker publishes nothing more until then, so
    every event is drawn, none is skipped or coalesced (ANSWERS.md 5).
  - The tap (`models/tap.py`) is the one observer on the worker: it reads the `World`
    only through `World.snapshot()` at the moments the controller names, so it is
    subscribed to the controller, which publishes to it before delivering (G-006); the
    history it fills is read on the GUI thread only up to the event delivered.
  - Each run has a generation number, carried by its signals, so a queued signal from a
    stopped run never reaches the next run's views.
  - Closing the window stops the worker (`stop()` wakes it from any wait) and joins it.

The layout (ANSWERS.md 2; PANES.md section 3): French's screen as the starting point
[F95 pp. 95-97]: the palette to the left of the table, the thermometer to its right, the
Slipnet grid to the right of the thermometer, the Coderack below. The table is the
central widget; every other pane is a `QDockWidget` with an `objectName`; the timeline is
a bottom toolbar. When the window opens, the user's own layout (`QSettings("tabletop",
"tabletop-gui")`, `window/state`) is restored if there is one; otherwise the repository
default, `tabletop/gui/default_layout.json` (`models/layout_file.py`); otherwise the
built-in arrangement, `_arrange_builtin` (G-010). A layout that fails to restore falls
back to the next level and says so in the status bar. The built-in arrangement is made
once the window is shown (`showEvent`): Qt 6 leaves stale tab bars behind, one drawn over
the menu bar and a tab group shown twice, when docks are tabified in groups before the
window has been laid out (G-012, the Seek-Whence GUI's lesson). View > Reset layout
restores the repository default (else the built-in) and clears the saved state; Reset to
built-in layout does what it says; Save layout as default for this code… writes the file
after a confirmation. `MainWindow(settings=None)` reads and writes no settings (the tests
and `--smoke`). The layout bytes carry `STATE_VERSION` (`saveState(version)`): a
`window/state` saved by another arrangement of the docks is refused by Qt, said so in the
status bar, and the next layout of the precedence is used (G-013). The window fits
`SMALLEST_SIZE`, 1280 x 800, with every dock shown: every pane has a small explicit
minimum (G-014).

Until their items come, the panes are `PanePlaceholder`s (`placeholder.py`, G-011) and
the menu actions whose item has not come are disabled and say which item (G-015). The
next run's inputs (`problem`, `profile`, `seed`) are the window's (G-015), set by
`set_next_run` from the Problem menu, the controls pane (P9, `controls.py`, G-044: its
`inputs_changed` is answered by `_inputs_from_controls`, an invalid seed shown in the
pane and refusing the next start) and the command line; whichever way a run is started
(`start_run`), the controls, the title and the stats pane's Run line show the run
actually started (PANES.md P9). The next run's `Config` is the chosen profile's with the
configuration pane's edits (P13, `config_view.py`, G-046, G-047: `next_config()`,
`dataclasses.replace(PROFILES[profile], **edits)`), read only when a run starts, so an
edit never touches a run in progress; the title and the stats pane mark a run whose
configuration differs from its profile (ANSWERS.md 3). The stats pane (P12, `stats_view.py`, G-045) takes the
controller's state and outcome. History and seeking (Numbo GUI): `history` is the `EventLog` the tap fills;
`position` is the index of the event the views show; `seek(index)` replays the history
to the views when no run is live, refused otherwise: the timeline (P11, `timeline.py`,
G-043), the trace and event log (P10, `log_view.py`, G-042) and the Back and Step keys
ask for seeks, and a seek back re-feeds every view from the first event, so that every
pane model is rebuilt from the history and drawn once. Session > Save trace… and Save
events as JSON lines… write what the log pane holds, as the CLI prints it (G-042).

Sessions and replay (PANES.md section 5; `models/session_file.py`, G-048, G-049; Numbo
GUI, `main_window.py`'s sessions): Session > Save session… writes the history so far with
the header of the run it was started from (`session_header`: the problem by name, or as
its dict for an edited table, the profile, the edits and the seed), also part way through
a run (then with no end line). Session > Open session… reads the file and re-executes its
header through the window's own controller with a `ReplayChecker` subscribed on the
worker, after the tap, that compares every event's JSON line with the stored one; the
window is then in replay mode (`replay_source`): the controls' inputs are off and the
configuration pane shows the file's configuration read-only, Play, Step, Step codelet and
Run to end drive the re-run, every event drawn as live, snapshots included, and when it
ends Play re-executes it again. At the first difference (the engine has changed since the
file was saved, or the file was edited and re-digested) the checker stops the run, the
window warns, and the stored events are shown instead (`_show_stored`): loaded into the
history without snapshots, so the trace, the event log and the timeline show them whole
and the panes that read the snapshot show only what the events alone give. Session >
Close replay goes back to live runs with the inputs chosen before; the history stays, to
scrub. A problem chosen from the Problem menu during a replay closes it first.
"""

import dataclasses
import functools
import inspect
import os
from pathlib import Path

from PySide6.QtCore import QByteArray, QObject, QSettings, QSize, Qt, Signal, qVersion
from PySide6.QtGui import QAction, QActionGroup, QKeySequence
from PySide6.QtWidgets import (QDialog, QDialogButtonBox, QDockWidget, QFileDialog, QLabel,
                               QMainWindow, QMessageBox, QTextBrowser, QToolBar, QVBoxLayout,
                               QWidget)

import tabletop
from tabletop import accel
from tabletop.config import PROFILES
from tabletop.gui import paint
from tabletop.gui.models import layout_file, run_controller
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.run_controller import ObserverError, Subject
from tabletop.gui.models.selection import Selection
from tabletop.gui.models.tap import Tap
from tabletop.gui.coderack_view import CoderackView
from tabletop.gui.competitions_view import CompetitionsView
from tabletop.gui.roles_view import RolesView
from tabletop.gui.config_view import ConfigView
from tabletop.gui.controls import ControlsPanel
from tabletop.gui.log_view import LogView
from tabletop.gui.models import config_table as ct
from tabletop.gui.models import controls as cm
from tabletop.gui.models import guide as gm
from tabletop.gui.models import palette as pm
from tabletop.gui.models import session_file as sf
from tabletop.gui.models.timeline import codelet_index
from tabletop.gui.palette_view import PaletteView
from tabletop.gui.placeholder import PanePlaceholder
from tabletop.gui.rack_view import RackView
from tabletop.gui.results_view import ResultsView
from tabletop.gui.slipnet_grid_view import SlipnetGridView
from tabletop.gui.slipnet_network_view import SlipnetNetworkView
from tabletop.gui.stats_view import StatsView
from tabletop.gui.table_view import TableCanvas
from tabletop.gui.temperature_view import TemperatureView
from tabletop.gui.thermometer_view import ThermometerView
from tabletop.gui.timeline import TimelineBar
from tabletop.gui.worldview_view import WorldviewView
from tabletop.problems import FAMILIES, by_family, load_problem
from tabletop.run import report_line

__all__ = ["AREAS", "DELAYS", "DEFAULT_DELAY", "DEFAULT_PROBLEM", "PANES", "SMALLEST_SIZE",
           "STATE_VERSION", "TIMELINE_NAME", "TITLE", "Bridge", "MainWindow", "about_text",
           "default_size", "engine_details", "engine_note", "title_for"]

TITLE = "Tabletop"
DEFAULT_SIZE = (1500, 950)
# The window fits this with every dock shown: no pane's minimum is larger than a few
# rows (G-014; the Seek-Whence GUI's lesson).
SMALLEST_SIZE = (1280, 800)
# The version `saveState` stamps on the layout bytes (QSettings `window/state` and the
# default layout file): a state saved by another version of the window's docks is
# refused by `restoreState` and the next layout in the precedence is used (G-013).
STATE_VERSION = 1
# The chapter 4 configuration is the default problem (PANES.md P9; [F95 p. 118]); the
# speeds are the Numbo GUI's `controls.py` (ANSWERS.md 5): seconds after each event while
# playing. All four live in the controls' model (G-044).
DEFAULT_PROBLEM = cm.DEFAULT_PROBLEM
DEFAULT_PROFILE = cm.DEFAULT_PROFILE
DEFAULT_SEED = cm.DEFAULT_SEED
DELAYS = cm.DELAYS
DEFAULT_DELAY = cm.DEFAULT_DELAY

# The built-in sizes (PANES.md section 3; Numbo GUI's DOCK_WIDTH and friends).
LEFT_WIDTH = 270
THERMOMETER_WIDTH = 90
RIGHT_WIDTH = 400
LEFT_HEIGHTS = (200, 300, 200)      # Controls, Palette and editor, Stats
RIGHT_HEIGHTS = (330, 300)          # Slipnet grid group, Worldview group
BOTTOM_HEIGHTS = (170, 180)         # Coderack group, Trace and event log group: each with its tab bar (G-037)

AREAS = {"left": Qt.DockWidgetArea.LeftDockWidgetArea,
         "right": Qt.DockWidgetArea.RightDockWidgetArea,
         "top": Qt.DockWidgetArea.TopDockWidgetArea,
         "bottom": Qt.DockWidgetArea.BottomDockWidgetArea}
AREA_NAMES = {area: name for name, area in AREAS.items()}

NOT_FRENCHS = "GUI design, not French's"

# The docks of PANES.md section 3, in the order they are added: attribute, objectName,
# title, pane id, the pages or precedent behind the pane (the guide model's words, which
# the pane, its tooltip and Help > What am I looking at? share, G-053), area, the dock it
# is tabbed under (or None) and whether it is shown by default. The panes with no figure
# in the sources say so (ANSWERS.md 4).
PANES = (
    ("controls", "controls-dock", "Controls", "P9", gm.pages_of("P9"), "left", None, True),
    ("palette", "palette-dock", "Palette and editor", "P7", gm.pages_of("P7"), "left", None, True),
    ("stats", "stats-dock", "Stats", "P12", gm.pages_of("P12"), "left", None, True),
    ("thermometer", "thermometer-dock", "Thermometer", "P2", gm.pages_of("P2"), "right", None, True),
    ("slipnet_grid", "slipnet-grid-dock", "Slipnet grid", "P3", gm.pages_of("P3"), "right", None, True),
    ("slipnet_network", "slipnet-network-dock", "Slipnet network", "P3b", gm.pages_of("P3b"),
     "right", "slipnet-grid-dock", True),
    ("worldview", "worldview-dock", "Worldview", "P5", gm.pages_of("P5"), "right", None, True),
    ("competitions", "competitions-dock", "Competitions", "P6", gm.pages_of("P6"),
     "right", "worldview-dock", True),
    ("temperature", "temperature-dock", "Temperature plot", "P2b", gm.pages_of("P2b"),
     "right", "worldview-dock", True),
    ("roles", "roles-dock", "Roles", "P14", gm.pages_of("P14"), "right", "worldview-dock", True),
    ("config", "config-dock", "Configuration", "P13", gm.pages_of("P13"), "right",
     "worldview-dock", False),
    ("coderack", "coderack-dock", "Coderack bars", "P4", gm.pages_of("P4"), "bottom", None, True),
    ("rack", "rack-dock", "Rack list", "P4b", gm.pages_of("P4b"), "bottom", "coderack-dock", True),
    ("log", "log-dock", "Trace and event log", "P10", gm.pages_of("P10"), "bottom", None, True),
    ("results", "results-dock", "Results", "P8", gm.pages_of("P8"), "bottom", "log-dock", True),
)
CENTRAL = ("table", "Table", "P1", gm.pages_of("P1"))
TIMELINE_NAME = "timeline-toolbar"
OVERLAY_KEYS = ("regions", "salience", "ranks", "roles", "eliza")   # `controls/overlay-*` (G-025, G-061)
PLOT_KEYS = ("terms", "french")                            # `controls/plot-*` (G-028)
GRID_KEYS = ("french",)                                    # `controls/grid-*` (G-032)
CODERACK_KEYS = ("legend",)                                # `controls/coderack-*` (G-037)
# The panes built so far, by attribute: each takes (title, pane_id, pages); one with
# `attach` gets the window's history and selection (G-011, G-032).
BUILT = {"controls": ControlsPanel, "stats": StatsView, "config": ConfigView,
         "palette": PaletteView,
         "thermometer": ThermometerView, "temperature": TemperatureView,
         "slipnet_grid": SlipnetGridView, "slipnet_network": SlipnetNetworkView,
         "coderack": CoderackView, "rack": RackView,
         "worldview": WorldviewView, "competitions": CompetitionsView, "log": LogView,
         "results": ResultsView, "roles": RolesView}
TIMELINE_PAGES = gm.pages_of("P11")
TRACE_FILTER = "Text (*.txt);;All files (*)"
EVENTS_FILTER = "JSON lines (*.jsonl);;All files (*)"
SESSION_FILTER = "Session (*.jsonl);;All files (*)"
PROBLEM_FILTER = "Problem (*.json);;All files (*)"


def default_size(screen):
    """DEFAULT_SIZE, or less to fit the screen's available area (Numbo GUI,
    `main_window.py`)."""
    w, h = DEFAULT_SIZE
    if screen is not None:
        room = screen.availableGeometry()
        if room.width() > 0 and room.height() > 0:
            w, h = min(w, room.width() - 40), min(h, room.height() - 60)
    return QSize(w, h)


def title_for(problem, profile, seed, edited=(), replaying=None) -> str:
    """The window's title for a run: `Tabletop — surround-05, seed 3`, with the profile
    when it is not `standard` and the fields `edited` from the profile when there are
    any: `(human)`, `(edited: max_codelets)`, `(human; edited: max_codelets)`; during a
    replay, `Tabletop — replaying s.jsonl: surround-05, seed 3` (Numbo GUI,
    `main_window.py`; ANSWERS.md 3; G-015, G-047, G-049)."""
    what = f"replaying {replaying}: " if replaying else ""
    title = f"{TITLE} — {what}{problem}, seed {seed}"
    parts = [] if profile == DEFAULT_PROFILE else [profile]
    if edited:
        parts.append(f"edited: {', '.join(edited)}")
    return title if not parts else f"{title} ({'; '.join(parts)})"


def about_text() -> str:
    """Help > About: what the window is, the engine's version, the sources (Numbo GUI;
    G-015)."""
    return (f"<b>Tabletop</b> — a PySide6 window on the Python reconstruction "
            f"(tabletop {tabletop.__version__}) of Robert French's Tabletop (1992), "
            f"described in <i>The Subtlety of Sameness</i> (1995) [F95] and in <i>Fluid "
            f"Concepts and Creative Analogies</i>, chs. 8-9 [FCCA].<br><br>The GUI only "
            f"observes a run: the same seed gives the same run with or without it. Every "
            f"pane says which pages of French's book it draws; the panes with no figure "
            f"in the sources are marked {NOT_FRENCHS}, and the icons are new drawings "
            f"(Help &gt; What am I looking at?).<br><br>The engine running: "
            f"{engine_details()} (G-063).<br><br>Built by Ralph loop 0006 after the "
            f"Numbo GUI's conventions; see docs/gui/panes.md.")


def engine_note() -> str:
    """The status bar's permanent note on the engine (G-063), the GUI's part of the rule
    that every output names its engine (D-107): `engine: compiled` for the reference
    engine, `engine: python (explicit request)` for the pure engine, which runs only
    when `tabletop.accel` was asked for it. The title stays the run's (G-015)."""
    engine = accel.info()["engine"]
    return f"engine: {engine}" + (" (explicit request)" if engine == "python" else "")


def engine_details() -> str:
    """The note's tooltip and the About box's line (G-063): for the compiled engine its
    build stamp, compiler, Cython version and pinned flags as `accel.info()` records them
    (D-110); for the pure engine, that it is the specification, run by explicit request."""
    info = accel.info()
    if info["engine"] == "compiled":
        return (f"compiled, the reference (build stamp {info['build_stamp'][:12]}, "
                f"{info['compiler']}, Cython {info['cython']}, flags {info['cflags']})")
    return "python, the pure engine (the readable specification), by explicit request"


def _bool(value):
    """A settings value as a boolean, or None when it does not read (G-025): QSettings
    gives a bool, an int or the strings `true` and `false`, by its backend."""
    if isinstance(value, bool):
        return value
    if isinstance(value, int):
        return bool(value)
    if isinstance(value, str) and value.lower() in ("true", "false"):
        return value.lower() == "true"
    return None


class Replay:
    """The session file open for replay (G-049; Seek-Whence GUI, `main_window.py`): the
    `file` read, its `path` and `name`, the `problem` and `config` that re-execute its
    header, the `checker` of the re-run under way (None before the first) and the
    `divergence` found, if any."""

    def __init__(self, file, path, name):
        self.file = file
        self.path = path
        self.name = name
        self.problem = sf.problem_of(file.header)
        self.config = sf.config_of(file.header)
        self.checker = None
        self.divergence = None


class Bridge(QObject):
    """Carries the worker's callbacks to the GUI thread, queued (Numbo GUI,
    `main_window.py`): a delivery, a state and an outcome, each with the generation of
    the run it belongs to."""
    delivered = Signal(int, object)     # generation, Delivery
    state_changed = Signal(int, str)    # generation, state
    finished = Signal(int, object)      # generation, RunOutcome


class MainWindow(QMainWindow):
    """The one window of the GUI (Numbo GUI, `main_window.py`; see the module docstring).
    `settings`: a `QSettings`, or None to read and write nothing. `layout_path`: the
    repository default layout file (ANSWERS.md 2), the package's by default; None for
    none. Signals: `run_finished(outcome)` once a run's outcome has been shown;
    `event_drawn(index)` when every view has drawn event `index` of the history;
    `run_state_changed(state)`."""

    run_finished = Signal(object)
    event_drawn = Signal(int)
    run_state_changed = Signal(str)

    def __init__(self, settings=None, layout_path=layout_file.DEFAULT_PATH, parent=None):
        super().__init__(parent)
        self.settings = settings
        self.layout_path = None if layout_path is None else Path(layout_path)
        self.layout_source = "built-in"
        self._shown = False                 # the first showEvent has come (G-012)
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
        self.history = EventLog()
        self.tap = Tap(self.history)
        self.selection = Selection()        # shared by every pane (G-022)
        self.position = -1
        self.problem = DEFAULT_PROBLEM      # the next run's inputs (G-015)
        self.profile = DEFAULT_PROFILE
        self.seed = DEFAULT_SEED
        self.started = None                 # (problem name, profile, seed) of the run shown
        self._started_problem = None        # what the run shown was started from (G-048)
        self._started_config = None
        self._input_error = ""              # the controls' text that does not parse (G-044)
        self.replay_source = None           # None, or the path of the session file replayed
        self._replay = None                 # the Replay of the file opened (G-049)
        self._run_observers = []            # observers subscribed for the current run only
        self._live_inputs = None            # the inputs chosen before a replay (G-049)

        self.bridge = Bridge(self)
        queued = Qt.ConnectionType.QueuedConnection
        self.bridge.delivered.connect(self._on_delivered, queued)
        self.bridge.state_changed.connect(self._on_state, queued)
        self.bridge.finished.connect(self._on_finished, queued)
        self.controller = run_controller.RunController(
            observers=[self.tap], on_world=self.tap.attach, on_step=self.tap.after_step,
            delay=DEFAULT_DELAY)

        self.docks = {}
        self._build_central()
        self._build_timeline()
        self._build_docks()
        self._build_menus()
        self._build_engine_note()
        self._arrange_builtin()
        self._update_actions()
        self._restore_settings()

    # -- building -------------------------------------------------------------------

    def _build_central(self):
        """The central widget: the table (P1, `table_view.py`), which reads the history's
        snapshots at the position the window gives it and shares the window's selection;
        its scene is settled before each paint (PANES.md section 3; Numbo GUI,
        `paint.py`; G-017, G-022)."""
        self.table_view = TableCanvas(self.history, selection=self.selection)
        self.table_view.setObjectName("table-view")
        self.table_view.selected.connect(self._on_selected)
        self.setCentralWidget(self.table_view)
        self.add_view(self.table_view)
        self._scenes = (self.table_view.scene(),)

    def _build_engine_note(self):
        """The permanent label at the right of the status bar naming the engine that
        runs (`engine_note`, G-063), with the build's details as its tooltip; the status
        bar's messages (the run's outcome, the selection) keep the left."""
        self.engine_label = QLabel(engine_note())
        self.engine_label.setObjectName("engine-note")
        self.engine_label.setToolTip(engine_details())
        self.statusBar().addPermanentWidget(self.engine_label)

    def _on_selected(self, name: str) -> None:
        """The shared selection changed: the status bar names it (a node by the Slipnet
        grid's hover text, G-030; anything else by the table's), or clears the note
        (G-022)."""
        if name:
            if self.selection.kind == "node":
                text = self.slipnet_grid_view.model.tooltip(name)
            else:
                text = self.table_view.view.model.tooltip(name)
            self._say(f"selected {name}: {text.splitlines()[0]}")
        elif self.statusBar().currentMessage().startswith("selected "):
            self.statusBar().clearMessage()

    def _build_timeline(self):
        """The timeline toolbar along the bottom (P11, `timeline.py`; Numbo GUI,
        `main_window.py`; G-043): its slider, buttons, strip and go-to box ask the window
        to seek, which it refuses while a run is live."""
        self.timeline = TimelineBar(self.history)
        self.timeline.seek_requested.connect(self.seek)
        self.timeline.codelet_requested.connect(self.jump_to_codelet)
        self.timeline_bar = QToolBar("Timeline")
        self.timeline_bar.setObjectName(TIMELINE_NAME)
        self.timeline_bar.addWidget(self.timeline)
        self.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self.timeline_bar)
        self.add_view(self.timeline)

    def _build_docks(self):
        """One dock per pane of PANES: the view of a pane that is built (`BUILT`: the
        thermometer, G-027, the temperature plot, G-028, the Slipnet grid, G-030, the
        Slipnet network, G-033, the Coderack bars, G-037, the rack list, G-038, the
        Worldview, G-039, the competitions, G-041, the trace and event log, G-042, the
        controls, G-044, the stats, G-045, the palette and editor, G-051, the
        results, G-052, and the roles, G-060), else a
        placeholder until its item (G-011); a built view with `attach` takes the
        window's history and shared selection (G-022, G-032); the thermometer's is
        narrow, as on French's screen [F95 p. 96 (img)]. The log pane's clicks ask the
        window to seek, and its Save buttons open the Session menu's file dialogs; the
        configuration pane's edits and its profile selector set the next run (G-047);
        the palette's Do this! and Run it again start a run of the edited table and its
        Load config and Save config open file dialogs (G-050); the results pane's Run
        batch asks the window for a batch of the next run's inputs (G-052); the
        controls' buttons, slider and inputs are wired by `_wire_controls`."""
        for attr, name, title, pane, pages, area, _under, _shown in PANES:
            if attr in BUILT:
                view = BUILT[attr](title, pane, pages)
                if hasattr(view, "attach"):
                    view.attach(self.history, self.selection)
            else:
                view = PanePlaceholder(title, pane, pages, narrow=attr == "thermometer")
            dock = self._dock(title, name, view, AREAS[area])
            dock.setToolTip(f"{pane} {title}: {pages}")
            setattr(self, f"{attr}_view", view)
            setattr(self, f"{attr}_dock", dock)
            self.add_view(view)
        self.log_view.event_activated.connect(self.seek)
        self.log_view.save_trace_requested.connect(self.save_trace_dialog)
        self.log_view.save_events_requested.connect(self.save_events_dialog)
        self.config_view.edits_changed.connect(self._edits_changed)
        self.config_view.profile_changed.connect(self._profile_from_config)
        self.palette_view.run_requested.connect(self.run_edited_table)
        self.palette_view.load_requested.connect(self.load_config_dialog)
        self.palette_view.save_requested.connect(self.save_config_dialog)
        self.results_view.batch_requested.connect(self.run_batch)
        self._wire_controls()

    def _wire_controls(self):
        """The controls pane's buttons, slider and inputs (PANES.md P9; Numbo GUI,
        `main_window.py`; G-044): the buttons are the Run menu's actions, the slider is
        the speed, and a change of the problem, the profile or the seed sets the next
        run's inputs."""
        c = self.controls_view
        c.play_button.clicked.connect(self.play)
        c.pause_button.clicked.connect(self.pause)
        c.step_button.clicked.connect(self.step)
        c.step_codelet_button.clicked.connect(self.step_codelet)
        c.run_to_end_button.clicked.connect(self.run_to_end)
        c.stop_button.clicked.connect(self.stop)
        c.back_button.clicked.connect(self.back_one)
        c.delay_changed.connect(self.set_delay)
        c.inputs_changed.connect(self._inputs_from_controls)

    def _dock(self, title, name, widget, area=Qt.DockWidgetArea.LeftDockWidgetArea):
        """A dock with an objectName, so that the window state can be saved (Numbo
        GUI, `main_window.py`)."""
        dock = QDockWidget(title, self)
        dock.setObjectName(name)
        dock.setWidget(widget)
        self.addDockWidget(area, dock)
        self.docks[name] = dock
        return dock

    # -- the menus (PANES.md section 4) ----------------------------------------------

    def _build_menus(self):
        """Session, Edit, Run, Problem, View and Help (PANES.md section 4; Numbo GUI).
        Actions whose item has not come yet are disabled and say which item (G-015)."""
        bar = self.menuBar()
        self._build_session_menu(bar.addMenu("&Session"))
        self._build_edit_menu(bar.addMenu("&Edit"))
        self._build_run_menu(bar.addMenu("&Run"))
        self._build_problem_menu(bar.addMenu("&Problem"))
        self._build_view_menu(bar.addMenu("&View"))
        help_menu = bar.addMenu("&Help")
        self._action(help_menu, "&What am I looking at?", self.show_guide)
        self._action(help_menu, "&About", self.show_about)

    def _build_session_menu(self, session):
        """New run, Open session… (Ctrl+O), Save session… (Ctrl+S, enabled once there
        is a history), Close replay (enabled in replay mode) (G-049), Save trace… and
        Save events as JSON lines… (the log pane's files, enabled once there is a
        history; G-042) and Quit (PANES.md section 4; G-015)."""
        self.session_menu = session
        self._action(session, "&New run", self.new_run, QKeySequence("Ctrl+N"))
        session.addSeparator()
        self.open_action = self._action(session, "&Open session…", self._open_dialog,
                                        QKeySequence.StandardKey.Open)
        self.open_action.setStatusTip("replay a saved session, checking it against the engine")
        self.save_action = self._action(session, "&Save session…", self._save_dialog,
                                        QKeySequence.StandardKey.Save, enabled=False)
        self.save_action.setStatusTip("the run so far as a session file, to replay")
        self.close_replay_action = self._action(session, "&Close replay", self.close_replay,
                                                enabled=False)
        self.close_replay_action.setStatusTip("back to live runs; the history stays")
        session.addSeparator()
        self.save_trace_action = self._action(session, "Save &trace…", self.save_trace_dialog,
                                              enabled=False)
        self.save_trace_action.setStatusTip("the text trace as the CLI prints it (the log pane's Trace tab)")
        self.save_events_action = self._action(session, "Save &events as JSON lines…",
                                               self.save_events_dialog, enabled=False)
        self.save_events_action.setStatusTip("every event as the CLI's --json output")
        session.addSeparator()
        self._action(session, "&Quit", self.close, QKeySequence.StandardKey.Quit)

    def _build_edit_menu(self, edit):
        """Configuration (opens P13) and Edit table (raises P7) (PANES.md section 4)."""
        self.edit_menu = edit
        self.configuration_action = self._action(edit, "&Configuration", self.show_configuration)
        self.edit_table_action = self._action(edit, "Edit &table", self.edit_table)

    def _build_run_menu(self, run):
        """Play/Pause, Step, Step codelet, Back, Run to end, Stop, Speed and Batch…
        (PANES.md section 4; ANSWERS.md 5: Numbo's keys)."""
        self.run_menu = run
        for text, keys, slot in (("Play / Pause", "Space", self.play_pause),
                                 ("Step", "Right", self.step_or_forward),
                                 ("Step codelet", "Shift+Right", self.step_codelet),
                                 ("Back", "Left", self.back_one),
                                 ("Run to end", "", self.run_to_end),
                                 ("Stop", "Esc", self.stop)):
            self._action(run, text, slot, QKeySequence(keys) if keys else None)
        run.addSeparator()
        self.speed_menu = run.addMenu("&Speed")
        self.speed_actions = QActionGroup(self)
        self.speed_actions.setExclusive(True)
        for delay in DELAYS:
            action = QAction(self._delay_text(delay), self)
            action.setCheckable(True)
            action.setChecked(delay == DEFAULT_DELAY)
            action.triggered.connect(functools.partial(self.set_delay, delay))
            self.speed_actions.addAction(action)
            self.speed_menu.addAction(action)
        run.addSeparator()
        self.batch_action = self._action(run, "&Batch…", self.batch_from_menu)

    @staticmethod
    def _delay_text(delay) -> str:
        return "no delay" if delay == 0 else f"{delay:g} s per event"

    def _build_problem_menu(self, menu):
        """The 53 problems by family, each with its title and page, setting the next run's
        problem (PANES.md section 4; [F95 p. 114]; G-015)."""
        self.problem_menu = menu
        self.problem_actions = {}
        for family in FAMILIES:
            sub = menu.addMenu(family)
            for name in by_family(family):
                problem = load_problem(name)
                page = problem.source.get("page")
                text = f"{problem.title} ({name}, p. {page})" if page else f"{problem.title} ({name})"
                action = self._action(sub, text, functools.partial(self.set_next_run, name))
                action.setCheckable(True)
                self.problem_actions[name] = action
        self._check_problem_action()

    def _build_view_menu(self, view):
        """Every dock's toggle, the timeline's, the P1 overlays (the table view's own
        checkable actions, so the bar and the menu agree; G-025), Eliza's view
        (G-026), the temperature plot's options (the plot's own actions; G-028), the
        Slipnet grid's (G-032) and the three layout actions (PANES.md section 4;
        ANSWERS.md 2)."""
        self.view_menu = view
        for _attr, name, *_ in PANES:
            view.addAction(self.docks[name].toggleViewAction())
        view.addAction(self.timeline_bar.toggleViewAction())
        view.addSeparator()
        self.overlays_menu = view.addMenu("Table &overlays")
        self.overlays_menu.setToolTipsVisible(True)
        for action in self.table_view.view.layer_actions.values():
            self.overlays_menu.addAction(action)
        self.eliza_view_action = self.table_view.view.eliza_action
        view.addAction(self.eliza_view_action)
        self.plot_menu = view.addMenu("Temperature &plot")
        self.plot_menu.setToolTipsVisible(True)
        self.plot_menu.addAction(self.temperature_view.terms_action)
        self.plot_menu.addAction(self.temperature_view.french_action)
        self.grid_menu = view.addMenu("Slipnet &grid")
        self.grid_menu.setToolTipsVisible(True)
        self.grid_menu.addAction(self.slipnet_grid_view.french_action)
        self.coderack_menu = view.addMenu("&Coderack")
        self.coderack_menu.setToolTipsVisible(True)
        self.coderack_menu.addAction(self.coderack_view.legend_action)
        view.addSeparator()
        self.save_layout_action = self._action(view, "Save layout as &default for this code…",
                                               self._save_default_layout_dialog)
        self.reset_layout_action = self._action(view, "&Reset layout", self.reset_layout)
        self.reset_builtin_action = self._action(view, "Reset to &built-in layout",
                                                 self.reset_builtin_layout)

    def _action(self, menu, text, slot, shortcut=None, enabled=True):
        action = QAction(text, self)
        if shortcut is not None:
            action.setShortcut(shortcut)
        action.triggered.connect(slot)
        action.setEnabled(enabled)
        menu.addAction(action)
        return action

    def _later(self, menu, text, item, shortcut=None):
        """An action whose item has not come: disabled, its status tip saying which (G-015)."""
        action = self._action(menu, text, lambda: None, shortcut, enabled=False)
        action.setStatusTip(f"comes with item {item:02d} of the GUI loop")
        return action

    # -- the layout (ANSWERS.md 2; G-010, G-012) --------------------------------------

    def _arrange_builtin(self):
        """The built-in arrangement of PANES.md section 3: every dock back in its area,
        the splits of French's screen, the tab groups, the default visibility, the sizes
        (G-010). Before the window is shown only the visibility is set and the arrangement
        waits for the first `showEvent` (G-012)."""
        self.layout_source = "built-in"
        if not self._shown:
            for _attr, name, _title, _pane, _pages, _area, _under, shown in PANES:
                self.docks[name].setVisible(shown)
            self._arrange_pending = True
            return
        self._arrange_pending = False
        for _a, name, *_ in PANES:
            dock = self.docks[name]
            dock.setFloating(False)
            self.removeDockWidget(dock)
        self.removeToolBar(self.timeline_bar)
        for _attr, name, _title, _pane, _pages, area, under, _shown in PANES:
            if under is None:
                self.addDockWidget(AREAS[area], self.docks[name])
        horizontal, vertical = Qt.Orientation.Horizontal, Qt.Orientation.Vertical
        # The thermometer to the right of the table, the Slipnet to the right of the
        # thermometer [F95 p. 95], the Worldview under the Slipnet; the Coderack below the
        # table [F95 p. 97], the trace under it.
        self.splitDockWidget(self.thermometer_dock, self.slipnet_grid_dock, horizontal)
        self.splitDockWidget(self.slipnet_grid_dock, self.worldview_dock, vertical)
        self.splitDockWidget(self.coderack_dock, self.log_dock, vertical)
        for _attr, name, _title, _pane, _pages, area, under, _shown in PANES:
            if under is not None:
                self.addDockWidget(AREAS[area], self.docks[name])
                self.tabifyDockWidget(self.docks[under], self.docks[name])
        for _attr, name, _title, _pane, _pages, _area, _under, shown in PANES:
            self.docks[name].setVisible(shown)
        for name in ("slipnet-grid-dock", "worldview-dock", "coderack-dock", "log-dock"):
            self.docks[name].raise_()
        self.addToolBar(Qt.ToolBarArea.BottomToolBarArea, self.timeline_bar)
        self.timeline_bar.show()
        self._size_builtin()

    def _size_builtin(self):
        """The built-in sizes of the columns and rows (PANES.md section 3; G-010)."""
        horizontal, vertical = Qt.Orientation.Horizontal, Qt.Orientation.Vertical
        left = [self.controls_dock, self.palette_dock, self.stats_dock]
        self.resizeDocks(left, [LEFT_WIDTH] * 3, horizontal)
        self.resizeDocks(left, list(LEFT_HEIGHTS), vertical)
        self.resizeDocks([self.thermometer_dock, self.slipnet_grid_dock],
                         [THERMOMETER_WIDTH, RIGHT_WIDTH], horizontal)
        self.resizeDocks([self.slipnet_grid_dock, self.worldview_dock], list(RIGHT_HEIGHTS),
                         vertical)
        self.resizeDocks([self.coderack_dock, self.log_dock], list(BOTTOM_HEIGHTS), vertical)

    def showEvent(self, event):
        """The first show makes the built-in arrangement if no restored layout replaced
        it (G-012); Numbo GUI's `showEvent` sizes its docks here for the same reason."""
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
        version; no geometry (ANSWERS.md 2; G-010)."""
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
        (ANSWERS.md 2; G-010)."""
        path = Path(path) if path is not None else self.layout_path
        if path is None:
            raise OSError("this window has no default layout file")
        layout_file.write_layout(path, self.layout_record())
        self._say(f"layout saved as the default for this code: {path}")
        return path

    def reset_layout(self):
        """View > Reset layout: the repository default if there is one, else the
        built-in; the saved state is cleared (ANSWERS.md 2; G-010)."""
        self._arrange_builtin()
        self._apply_default_layout()
        self._forget_saved_state()
        self._say(f"layout reset to the {self.layout_source} layout")

    def reset_builtin_layout(self):
        """View > Reset to built-in layout (ANSWERS.md 2; G-010)."""
        self._arrange_builtin()
        self._forget_saved_state()
        self._say("layout reset to the built-in layout")

    def _forget_saved_state(self):
        if self.settings is not None:
            self.settings.remove("window/state")
            self.settings.sync()

    def _save_default_layout_dialog(self):
        """Confirm, naming the file and what saving it means; on a read-only package
        directory, say so and offer another place (ANSWERS.md 2)."""
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
        (Seek-Whence GUI, `main_window.py`; G-017)."""
        for positioner in self._positioners:
            positioner(index)

    def _view_error(self, error):
        run_controller.report_to_stderr(error)
        self._say(f"a view failed: {error.exception!r}")

    def _say(self, message):
        self.statusBar().showMessage(message)

    # -- the next run's inputs (G-015) --------------------------------------------------

    def set_next_run(self, problem=None, profile=None, seed=None) -> None:
        """The inputs of the next run: a problem name, a profile name and a seed; each
        left as it was when None. Shown in the title and checked in the Problem menu
        (PANES.md P9: whichever way a run is started, the title agrees; G-015). During a
        replay the replay is closed first: the next run is a live run (G-049)."""
        if self.replay_source is not None:
            self.close_replay()
        if problem is not None:
            if problem not in self.problem_actions:
                raise ValueError(f"no problem named {problem!r}")
            self.problem = problem
        if profile is not None:
            if profile not in PROFILES:
                raise ValueError(f"no profile named {profile!r}")
            self.profile = profile
        if seed is not None:
            self.seed = int(seed)
        self._check_problem_action()
        self._show_next_run()

    def _show_next_run(self):
        """The next run's inputs in the controls (the selector, the profile, the seed,
        the cap line of its configuration), in the configuration pane (the profile's
        values with the edits, G-047) and, before any run, in the title (PANES.md P9;
        G-044)."""
        self.config_view.show_next_run(self.profile)
        self.results_view.show_next_run(self.problem, self.profile, self.seed)
        c = self.controls_view
        c.show_run(self.problem, self.profile, self.seed)
        c.set_config(self.next_config())
        c.show_error("")
        self._input_error = ""
        if self.started is None:
            self.setWindowTitle(title_for(self.problem, self.profile, self.seed,
                                          self.config_view.model.edited_names))
        self._preview_problem()

    def _preview_problem(self):
        """The next run's problem on the table before Play, bare: the objects and Henry's
        touch, no marks (G-064). Drawn when the table shows another problem (or none) and
        no run is live; a problem the library lacks leaves the table as it is and is said
        when the run is started (G-044)."""
        if self._live() or self.table_view.view.model.problem == self.problem:
            return
        try:
            problem = load_problem(self.problem)
        except (OSError, ValueError):
            return
        self.table_view.set_problem(problem)

    def _edits_changed(self):
        """The configuration pane's edits changed: the next run's configuration, the cap
        line and the title follow (ANSWERS.md 3; G-047)."""
        self._show_next_run()

    def _profile_from_config(self, name):
        """The configuration pane's selector chose a profile: the next run's, shown in
        the controls too (ANSWERS.md 3: one state; G-047)."""
        self.set_next_run(profile=name)

    def _inputs_from_controls(self) -> bool:
        """The controls changed: the next run's inputs are theirs; a text that does not
        parse is shown in the pane and refuses the next start (Numbo GUI; G-044)."""
        c = self.controls_view
        try:
            problem, profile, seed = c.inputs()
        except cm.InputError as error:
            self._input_error = str(error)
            c.show_error(self._input_error)
            return False
        self.set_next_run(problem, profile, seed)
        return True

    def next_config(self):
        """The next run's `Config`: `dataclasses.replace(PROFILES[profile], **edits)`, the
        chosen profile's with the configuration pane's edits (ANSWERS.md 3; G-015,
        G-047)."""
        model = self.config_view.model
        model.set_profile(self.profile)
        return model.config()

    def set_next_config(self, config) -> None:
        """The next run's configuration given whole (the command line's `--set`, G-054):
        its profile becomes the next run's and its other differences from that profile
        the configuration pane's edits (G-047); a `ValueError` for a configuration whose
        profile is none of `PROFILES`."""
        if config.profile not in PROFILES:
            raise ValueError(f"no profile named {config.profile!r}")
        self.config_view.model.set_config(config)
        self.set_next_run(profile=config.profile)

    def _check_problem_action(self):
        for name, action in self.problem_actions.items():
            action.setChecked(name == self.problem)

    def set_delay(self, seconds) -> None:
        """The speed: `seconds` after each event while playing (Run > Speed; Numbo GUI)."""
        self.controller.set_delay(seconds)
        for action in self.speed_actions.actions():
            action.setChecked(action.text() == self._delay_text(seconds))
        if seconds in DELAYS and self.controls_view.speed_slider.value() != DELAYS.index(seconds):
            self.controls_view.set_values(dataclasses.replace(
                self.controls_view.values(), speed=DELAYS.index(seconds)))

    # -- starting runs ------------------------------------------------------------------

    def start_run(self, problem, config, seed, paused=False, observers=()) -> None:
        """Start a run of `problem` (a `Problem` or a problem name) under `config` with
        `seed` through the controller, a new generation; the title and `started` say
        what runs (Numbo GUI; G-008, G-015). `observers` are subscribed to the
        controller for this run only, after the tap (a replay's checker, G-049). Raises
        `EngineBusy` if the previous run did not stop, and the controller's errors for
        bad inputs."""
        loaded = run_controller.check_inputs(problem, config, seed)
        self._connect()
        for observer in observers:
            self._run_observers.append(self.controller.subscribe(observer))
        profile = config.profile if config.profile in PROFILES else "custom"
        self.started = (loaded.name, profile, seed)
        self._started_problem = problem if isinstance(problem, str) else loaded
        self._started_config = config
        self._show_started(loaded.name, profile, seed, ct.edited_fields(config))
        self.table_view.set_problem(loaded)
        self.controller.start_run(loaded, config, seed, paused=paused)

    def _show_started(self, name, profile, seed, edited=()):
        """The run actually started, in the title (with the fields `edited` from its
        profile, ANSWERS.md 3) and the controls, whichever way it was started; its
        inputs become the next run's when they are a problem of the library and a
        profile (PANES.md P9; loop0004 ANSWERS 10.3; G-044, G-047). The stats pane's Run
        and Configuration lines come from the `RunStarted` event itself. During a
        replay the title says which file is replayed (G-049)."""
        replaying = None if self._replay is None else self._replay.name
        self.setWindowTitle(title_for(name, profile, seed, edited, replaying))
        if name in self.problem_actions:
            self.problem = name
        if profile in PROFILES:
            self.profile = profile
        self.seed = seed
        self._check_problem_action()
        c = self.controls_view
        c.show_run(name, profile, seed)
        c.show_error("")
        self._input_error = ""

    def _connect(self):
        """A new generation of the controller's callbacks (Numbo GUI). The old run is
        stopped and joined before the generation moves on, so none of its callbacks can
        carry the new generation."""
        self.controller.stop()
        for observer in self._run_observers:
            self.controller.unsubscribe(observer)
        self._run_observers = []
        self._generation += 1
        generation = self._generation
        bridge = self.bridge
        self.controller.deliver = lambda d: bridge.delivered.emit(generation, d)
        self.controller.on_state = lambda s: bridge.state_changed.emit(generation, s)
        self.controller.on_finished = lambda o: bridge.finished.emit(generation, o)
        self.outcome = None

    def _start(self, mode) -> bool:
        """Start a run of the window's next-run inputs, paused if `mode` is a step;
        False (and the error said, in the status bar and the controls) if it cannot
        start (Numbo GUI; G-044)."""
        self._say("")
        if self._input_error:
            self.controls_view.show_error(self._input_error)
            self._say(self._input_error)
            return False
        try:
            self.start_run(self.problem, self.next_config(), self.seed, paused=mode != "play")
        except (ValueError, TypeError, OSError, run_controller.EngineBusy) as error:
            self._say(str(error))
            self.controls_view.show_error(str(error))
            return False
        self.controls_view.show_error("")
        return True

    def _live(self):
        return self.controller.is_alive() and self.controller.state in ("running", "paused")

    def _begin(self, mode) -> bool:
        """Start what the run actions start when no run is live: the replay of the
        session file opened, else a live run of the next-run inputs (G-049; Seek-Whence
        GUI, `main_window.py`)."""
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
        """One event, one visible change (ANSWERS.md 5; Numbo GUI)."""
        if self._live() or self._begin("step"):
            self.controller.step()

    def step_codelet(self):
        """To the end of the current `World.step()` (ANSWERS.md 5; G-008)."""
        if self._live() or self._begin("step"):
            self.controller.step_codelet()

    def run_to_end(self):
        """On with no delay (Numbo GUI)."""
        if self._live() or self._begin("run-to-end"):
            self.controller.run_to_end()

    def stop(self):
        """Stop the run and join the worker (Numbo GUI)."""
        self.controller.stop()
        self._update_actions()

    def new_run(self):
        """Ctrl+N: a fresh run of the next-run inputs, paused at its first event
        (PANES.md section 4; Numbo GUI's step starts a run paused)."""
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
        """The left arrow: one event back in the history, when no run is live
        (Numbo GUI; ANSWERS.md 5)."""
        if self.position > 0:
            self.seek(self.position - 1)

    # -- the handshake (Numbo GUI) ------------------------------------------------------

    def _on_delivered(self, generation, delivery):
        if self._closed or generation != self._generation:
            return
        self.position = delivery.index
        if len(self.history) == 1:
            self._update_actions()
        self._position_views(delivery.index)
        self._views.publish(delivery.event)
        self._draw(delivery.event)
        self.controller.acknowledge(delivery.ticket)

    def _draw(self, event):
        """Every view draws the event at `position`, now: each marks itself dirty, then
        the window is painted once (`paint.paint_all`) (Numbo GUI)."""
        self._settle(event)
        self.event_drawn.emit(self.position)

    def _settle(self, event=None):
        """Every view redraws and the window is painted once, without an event being
        drawn (Numbo GUI; G-024): after the run has ended, so that the views show the
        last step's snapshot, which the tap records after the step's events were
        delivered."""
        for redraw in self._redraws:
            try:
                redraw()
            except Exception as exception:
                self._view_error(ObserverError(redraw, event, exception))
        paint.paint_all(self._scenes)

    def _on_state(self, generation, state):
        if self._closed or generation != self._generation:
            return
        self.state = state
        self.stats_view.set_state(state)
        self.controls_view.set_state(state)
        self.palette_view.set_state(state)
        self.results_view.set_state(state)
        self._update_actions()
        self.run_state_changed.emit(state)

    def _on_finished(self, generation, outcome):
        if self._closed or generation != self._generation:
            return
        self.outcome = outcome
        self.stats_view.finish(outcome)
        if outcome.error is not None:
            self._say(f"the run ended with an error: {outcome.error}")
        elif outcome.stopped:
            self._say(f"stopped after {outcome.events} events")
        elif outcome.result is not None:
            self._say(report_line(outcome.result))
        if self.history.events:
            self._settle()
        replay = self._replay
        if replay is not None and replay.checker is not None:
            self._finish_replay(replay, outcome)
        self._update_actions()
        self.run_finished.emit(outcome)

    # -- the history: seeking (Numbo GUI) -----------------------------------------------

    def seek(self, index) -> bool:
        """Show event `index` of the history; False if a run is live or there is no such
        event (Numbo GUI; ANSWERS.md 5)."""
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

    def jump_to_codelet(self, n) -> bool:
        """Go to codelet `n`: seek to its `CodeletChosen` event; False when there is
        none or a run is live (PANES.md P11; Numbo GUI's `jump_to_iteration`; G-043)."""
        index = codelet_index(self.history, int(n))
        return index is not None and self.seek(index)

    def _update_actions(self):
        """What the state allows: the timeline scrubs and the trace and events can be
        saved once there is a history and no run is live (Numbo GUI, `main_window.py`;
        G-042, G-043); the session can be saved once there is a history, and the replay
        closed in replay mode (G-049)."""
        has_history = bool(self.history.events)
        self.timeline.set_scrubbable(has_history and not self._live())
        self.save_trace_action.setEnabled(has_history)
        self.save_events_action.setEnabled(has_history)
        self.save_action.setEnabled(has_history)
        self.close_replay_action.setEnabled(self.replay_source is not None)

    # -- the log pane's files (PANES.md P10; G-042) ----------------------------------------

    def save_trace(self, path) -> Path:
        """Write the trace as the log pane shows it (normal or quiet), as the CLI prints
        it; returns the path (G-042)."""
        path = self.log_view.save_trace(path)
        self._say(f"trace saved: {path}")
        return path

    def save_events(self, path) -> Path:
        """Write every event as JSON lines, byte for byte the CLI's `--json` output;
        returns the path (G-042; D-065)."""
        path = self.log_view.save_events(path)
        self._say(f"events saved: {path}")
        return path

    def save_trace_dialog(self):
        """Session > Save trace… (PANES.md section 4; G-042)."""
        path, _ = QFileDialog.getSaveFileName(self, "Save the trace", "trace.txt", TRACE_FILTER)
        if path:
            self._write(self.save_trace, path)

    def save_events_dialog(self):
        """Session > Save events as JSON lines… (PANES.md section 4; G-042)."""
        path, _ = QFileDialog.getSaveFileName(self, "Save the events", "events.jsonl",
                                              EVENTS_FILTER)
        if path:
            self._write(self.save_events, path)

    def _write(self, writer, path):
        """Write a file through `writer`, saying an `OSError` in the status bar instead of
        raising (G-042)."""
        try:
            writer(path)
        except OSError as error:
            self._say(f"cannot save {path}: {error}")

    # -- sessions and replay (PANES.md section 5; G-048, G-049; Numbo GUI) ----------------

    def session_header(self) -> sf.SessionHeader:
        """The header of the run in the history: what it was started from (the problem
        by name, or as its dict for a table that is not the library's; the profile, its
        edits and the seed; `models/session_file.py`'s `header_of`; G-048)."""
        if self._started_problem is None:
            raise ValueError("no run yet")
        return sf.header_of(self._started_problem, self._started_config, self.started[2])

    def save_session(self, path) -> bool:
        """Write the history with its header to `path` as a session file, also part way
        through a run (then with no end line); False (and the error said) if that fails
        (PANES.md section 5; Numbo GUI, `main_window.py`; G-048)."""
        events = list(self.history.events)
        if not events or self._started_problem is None:
            self._say("nothing to save: no run yet")
            return False
        try:
            n = sf.write_session(str(path), self.session_header(), events)
        except (OSError, ValueError) as error:
            self._say(f"cannot save the session: {error}")
            return False
        self._say(f"saved {n} events to {path}")
        return True

    def open_session(self, path) -> bool:
        """Open the session file `path` for replay: read and check it, then re-execute
        its header through the controller, paused at its first event (Numbo GUI's
        `open_file` for an oracle trace); False (and the error said) if it cannot be
        read or started (PANES.md section 5; G-049)."""
        path = str(path)
        name = os.path.basename(path)
        try:
            file = sf.read_session(path)
            replay = Replay(file, path, name)
        except sf.SessionError as error:
            where = f", line {error.line}" if error.line is not None else ""
            self._say(f"cannot open {name}{where}: {error.reason}")
            return False
        except (OSError, ValueError) as error:
            self._say(f"cannot open {name}: {error}")
            return False
        self._say("")
        if self.replay_source is None:
            self._live_inputs = (self.problem, self.profile, self.seed)
        self.replay_source = path
        self._replay = replay
        self.controls_view.set_replay(name)
        self.config_view.set_replay(replay.config)      # the file's configuration (G-047)
        self.stats_view.set_source(f"replay of {name}")
        if file.header.version != tabletop.__version__:
            self._say(f"{name} was saved by tabletop {file.header.version}; this is "
                      f"{tabletop.__version__}")
        if not self._start_replay("step"):
            self.close_replay()
            return False
        self.controller.step()      # draw its RunStarted event
        self._update_actions()
        return True

    def _start_replay(self, mode) -> bool:
        """Re-execute the opened file's header, paused if `mode` is a step, with a fresh
        `ReplayChecker` that stops the run at the first difference (G-049); False (and
        the error said) if it cannot start."""
        replay = self._replay
        controller = self.controller
        replay.checker = sf.ReplayChecker(replay.file,
                                          on_divergence=lambda d: controller.stop(wait=False))
        self._say("")
        try:
            self.start_run(replay.problem, replay.config, replay.file.header.seed,
                           paused=mode != "play", observers=[replay.checker])
        except (ValueError, TypeError, OSError, run_controller.EngineBusy) as error:
            self._say(f"cannot replay {replay.name}: {error}")
            return False
        return True

    def _finish_replay(self, replay, outcome):
        """The re-run ended: verified, stopped by the user, or different from the file,
        in which case the stored events are shown (PANES.md section 5; G-049)."""
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
            self.stats_view.set_source(f"replay of {name}: stopped")
        elif checker.beyond:
            self._say(f"replay of {name}: the file's {checker.seen} events verified; it "
                      f"was saved part way, and the run went on for {checker.beyond} more")
            self.stats_view.set_source(f"replay of {name}: verified, saved part way")
        else:
            self._say(f"replay of {name}: verified, {checker.seen} events")
            self.stats_view.set_source(f"replay of {name}: verified")

    def _show_stored(self, replay):
        """The re-run differs from the file: warn, load the stored events into the
        history (no snapshots: the trace, the event log and the timeline show them whole,
        the panes that read the snapshot only what the events alone give) and seek to
        the first difference (PANES.md section 5; G-049)."""
        divergence = replay.divergence
        file = replay.file
        self.history.load(file.events)
        self.log_view.reload()
        self.position = -1
        if file.events:
            self.seek(min(divergence.index, len(file.events) - 1))
        message = (f"{replay.name} differs from the engine at event {divergence.index + 1} "
                   f"(line {divergence.line}): {divergence.message}. Showing the stored "
                   f"events; the panes that read the snapshot are not available for this "
                   f"file.")
        self.stats_view.set_source(f"stored events of {replay.name} (differs at event "
                                   f"{divergence.index + 1})")
        self.stats_view.set_state("stored")
        self._say(message)
        self._update_actions()
        QMessageBox.warning(self, "The replay differs from the file", message)

    def close_replay(self):
        """Back to live runs: the replay stops, the next run's inputs are the ones chosen
        before, the history stays, to scrub (Numbo GUI, `main_window.py`; G-049)."""
        if self.replay_source is None:
            return
        self.controller.stop()
        self.replay_source = None
        self._replay = None
        self.controls_view.set_replay(None)
        self.config_view.set_replay(None)
        self.stats_view.set_source("live run")
        if self.started is not None:
            name, profile, seed = self.started
            self.setWindowTitle(title_for(name, profile, seed,
                                          ct.edited_fields(self._started_config)))
        live, self._live_inputs = self._live_inputs, None
        if live is not None:
            self.set_next_run(*live)
        self._say("replay closed: the next run is a live run")
        self._update_actions()

    def _open_dialog(self):
        """Session > Open session…: a file dialog, then `open_session` (Numbo GUI)."""
        path, _ = QFileDialog.getOpenFileName(self, "Open a session", "", SESSION_FILTER)
        if path:
            self.open_session(path)

    def _save_dialog(self):
        """Session > Save session…: a file dialog, then `save_session` (Numbo GUI)."""
        path, _ = QFileDialog.getSaveFileName(self, "Save the session", "session.jsonl",
                                              SESSION_FILTER)
        if path:
            self.save_session(path)

    # -- the batch (PANES.md P8; G-052) ---------------------------------------------------

    def run_batch(self, count=None) -> bool:
        """A batch of the next run's problem, configuration and seed on the results
        pane's worker (PANES.md P8; G-052): `count` runs, or the pane's count. Refused,
        with the reason in the status bar, while a run is live, in replay mode, when the
        inputs do not parse, or when the engine is busy."""
        self._say("")
        if self._live():
            self._say("a batch is refused while a run is live: stop it first")
            return False
        if self.replay_source is not None:
            self._say("a batch is refused during a replay: close the replay first")
            return False
        if self._input_error:
            self._say(self._input_error)
            return False
        try:
            problem = load_problem(self.problem)
        except (OSError, ValueError) as error:
            self._say(str(error))
            return False
        if not self.results_view.start_batch(problem, self.next_config(), self.seed, count):
            self._say(self.results_view.error)
            return False
        self._say(f"batch of {self.results_view.model.count} runs: "
                  f"{self.results_view.model.title()}")
        return True

    def batch_from_menu(self) -> bool:
        """Run > Batch…: show and raise the results dock and start a batch of the pane's
        count (PANES.md section 4; G-052)."""
        self.results_dock.show()
        self.results_dock.raise_()
        return self.run_batch()

    # -- the Edit and Help menus ----------------------------------------------------------

    def show_configuration(self):
        """Edit > Configuration: show and raise the hidden configuration dock (ANSWERS.md
        3; PANES.md section 4; G-015)."""
        self.config_dock.show()
        self.config_dock.raise_()

    def edit_table(self):
        """Edit > Edit table: show and raise the palette and editor dock (PANES.md
        section 4; G-015)."""
        self.palette_dock.show()
        self.palette_dock.raise_()

    # -- the palette and editor (PANES.md P7; G-050, G-051) --------------------------------

    def run_edited_table(self, again=False) -> bool:
        """Do this!, or Run it again with `again` [F95 p. 96 (img)]: a run of the
        editor's table (a `Problem` through `problems.problem_from_dict` after
        `problems.validate` passes it, G-050) under the next run's configuration, with
        the next run's seed, or the next seed for Run it again, playing; refused while
        a run is live (PANES.md P7), and a replay is closed first (G-049). False, with
        the reason in the status bar, when it cannot start."""
        if self._live():
            self._say("editing is refused while a run is live: stop it first")
            return False
        if self.replay_source is not None:
            self.close_replay()
        try:
            problem = self.palette_view.editor.problem()
            seed = self.seed + 1 if again else self.seed
            self.start_run(problem, self.next_config(), seed)
        except (ValueError, TypeError, run_controller.EngineBusy) as error:
            self._say(str(error))
            return False
        self._say(f"running the edited table {problem.name!r} with seed {seed}")
        return True

    def load_config(self, path) -> bool:
        """Load config [F95 p. 96 (img)]: a problem file into the editor (the library's
        files included; G-050); False, with the reason in the status bar, when the file
        does not validate or cannot be read."""
        try:
            self.palette_view.editor.load_file(path)
        except (OSError, ValueError) as error:
            self._say(f"cannot load {path}: {error}")
            return False
        self._say(f"table loaded: {path}")
        return True

    def save_config(self, path) -> Path:
        """Save config [F95 p. 96 (img)]: the editor's table as a problem file at
        `path`, never into `problems/` (G-050); returns the path."""
        path = self.palette_view.editor.save(path)
        self._say(f"table saved: {path}")
        return path

    def load_config_dialog(self):
        """The palette's Load config: a file dialog opening in the library's folder,
        then `load_config` (G-050)."""
        path, _ = QFileDialog.getOpenFileName(self, "Load a table", pm.problem_dir_of(None),
                                              PROBLEM_FILTER)
        if path:
            self.load_config(path)

    def save_config_dialog(self):
        """The palette's Save config: a file dialog, then `save_config`; a path in
        `problems/` is refused and said (G-050)."""
        name = self.palette_view.editor.name
        path, _ = QFileDialog.getSaveFileName(self, "Save the table", f"{name}.json",
                                              PROBLEM_FILTER)
        if path:
            try:
                self.save_config(path)
            except (OSError, ValueError) as error:
                self._say(f"cannot save {path}: {error}")

    def guide_text(self) -> str:
        """Help > What am I looking at?: the pane-to-pages table of the guide model (the
        user guide's own table when `docs/gui/panes.md` is there, item 17), each pane
        with the pages behind it, the new designs marked, the icons' caption and what
        to expect from the dynamics (ANSWERS.md 4, 11; G-053)."""
        return gm.guide_html(gm.guide_rows())

    def show_guide(self):
        """Help > What am I looking at?: the guide's HTML in a non-modal dialog with a
        Close button (Seek-Whence GUI, `main_window.py`; G-053). Returns the dialog."""
        dialog = QDialog(self)
        dialog.setWindowTitle("What am I looking at?")
        dialog.setObjectName("guide-dialog")
        browser = QTextBrowser(dialog)
        browser.setObjectName("guide-text")
        browser.setOpenExternalLinks(False)
        browser.setHtml(self.guide_text())
        buttons = QDialogButtonBox(QDialogButtonBox.StandardButton.Close, dialog)
        buttons.rejected.connect(dialog.reject)
        buttons.accepted.connect(dialog.accept)
        layout = QVBoxLayout(dialog)
        layout.addWidget(browser)
        layout.addWidget(buttons)
        dialog.resize(760, 560)
        dialog.setAttribute(Qt.WidgetAttribute.WA_DeleteOnClose, True)
        dialog.show()
        self.guide_dialog = dialog
        return dialog

    def show_about(self):
        """Help > About (Numbo GUI; G-015)."""
        QMessageBox.about(self, "About Tabletop", about_text())

    # -- settings (Numbo GUI; ANSWERS.md 2) ---------------------------------------------

    def _restore_settings(self):
        """The window's geometry, layout and next-run inputs as they were left, then the
        layout precedence of ANSWERS.md 2: the user's state, the repository default, the
        built-in. A value that does not read leaves the default (G-013)."""
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
            self._restore_inputs(s)
            self._say(note)
        if not restored:
            self._apply_default_layout()
        self.set_next_run()

    def _restore_inputs(self, s):
        """The next-run inputs and the speed (the controls' model's round trip, G-044),
        the configuration pane's edits (`config/edits`, G-046), the overlays, the plot's,
        the grid's and the Coderack's options under `controls/*` (PANES.md section 3;
        G-025, G-028, G-032, G-037; a value that does not read leaves the default)."""
        values = cm.ControlValues.from_settings(
            {f"controls/{key}": s.value(f"controls/{key}") for key in cm.SETTINGS_KEYS})
        self.problem, self.profile = values.problem, values.profile
        self.seed = cm.parse_seed(values.seed)
        self.set_delay(DELAYS[values.speed])
        # the configuration pane's edits under `config/edits` (G-046)
        self.config_view.model.from_settings({ct.SETTINGS_KEY: s.value(ct.SETTINGS_KEY)})
        state = {}
        for name in OVERLAY_KEYS:
            on = _bool(s.value(f"controls/overlay-{name}"))
            if on is not None:
                state[name] = on
        self.table_view.view.set_overlay_state(state)
        plot = {}
        for name in PLOT_KEYS:
            on = _bool(s.value(f"controls/plot-{name}"))
            if on is not None:
                plot[name] = on
        self.temperature_view.set_plot_state(plot)
        grid = {}
        for name in GRID_KEYS:
            on = _bool(s.value(f"controls/grid-{name}"))
            if on is not None:
                grid[name] = on
        self.slipnet_grid_view.set_grid_state(grid)
        rack = {}
        for name in CODERACK_KEYS:
            on = _bool(s.value(f"controls/coderack-{name}"))
            if on is not None:
                rack[name] = on
        self.coderack_view.set_coderack_state(rack)

    def _save_settings(self):
        s = self.settings
        if s is None:
            return
        s.setValue("window/geometry", self.saveGeometry())
        s.setValue("window/state", self.saveState(STATE_VERSION))
        delay = self.controller.delay
        values = cm.ControlValues(self.problem, self.profile, str(self.seed),
                                  DELAYS.index(delay) if delay in DELAYS
                                  else DELAYS.index(DEFAULT_DELAY))
        for key, value in values.to_settings().items():
            s.setValue(key, value)
        for key, value in self.config_view.model.to_settings().items():
            s.setValue(key, value)
        for name, on in self.table_view.view.overlay_state().items():
            s.setValue(f"controls/overlay-{name}", bool(on))
        for name, on in self.temperature_view.plot_state().items():
            s.setValue(f"controls/plot-{name}", bool(on))
        for name, on in self.slipnet_grid_view.grid_state().items():
            s.setValue(f"controls/grid-{name}", bool(on))
        for name, on in self.coderack_view.coderack_state().items():
            s.setValue(f"controls/coderack-{name}", bool(on))
        s.sync()

    def shutdown(self):
        """Save the settings (once), stop the worker and wait for it (Numbo GUI).
        Returns whether the worker has ended."""
        if not self._closed:
            self._save_settings()
        self._closed = True
        self.results_view.shutdown()
        return self.controller.stop()

    def closeEvent(self, event):
        """Closing the window is `shutdown` (Numbo GUI, `main_window.py`)."""
        self.shutdown()
        super().closeEvent(event)

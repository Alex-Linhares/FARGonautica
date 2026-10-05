"""The main window: one QMainWindow for every panel of Metacat.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).  Item 01 of loop0003 made the window;
item 04 the splitter tree of docs/qt-gui-plan.md 2.2, which holds the panes of
the Qt hosts (metacat/qt/hosts.py):

    rows (vertical)       60% / 31% / 9% of the height
    ├── top               Temperature | Workspace | Coderack | Vertical Themes | Commentary
    ├── middle            Slipnet | themes (vertical: Top / Bottom Themes) | Episodic Memory
    └── bottom (vertical) Temporal Trace | EEG (hidden)

Fixed splitters, not docks (the owner's decision): the panes stay where they
are; the handles resize them.  Until the user drags a handle, the default sizes
follow the window's size (`default_sizes`).  A 50 ms timer brings every pane's
scene up to date with its canvas (`sync`).

Item 08 saves the layout in a QSettings (docs/qt-gui-plan.md 2.3): the window's
geometry, the hidden panes, and each splitter's state once the user has dragged
a handle, 500 ms after the last drag and at close; `restore_geometry` and
`restore_layout` bring them back at the next start, and View > Reset layout
removes them.  A window made without a QSettings saves nothing (the tests'
windows).  The window has a minimum size (MINIMUM_SIZE, and the control
strip's width).
"""
from __future__ import annotations

from PySide6.QtCore import QByteArray, Qt, QTimer
from PySide6.QtWidgets import QMainWindow, QSplitter, QToolBar

TITLE = "Metacat"
DEFAULT_SIZE = (1920, 1010)     # a maximised window on a 1920x1080 screen (the minimum)
HANDLE = 4                      # the splitter handles' width
SYNC_INTERVAL = 50              # ms between scene updates (racket/gui's refresh)
MINIMUM_SIZE = (1600, 800)      # widened to the control strip (1684 px at 1080p); rows of 430/220/65 or more
SETTINGS_VERSION = 1            # bump whenever TREE changes: an older saved layout is ignored
SAVE_DELAY = 500                # ms after the last handle drag

# Tk's aspect ratios of the unscrollable windows: (w+2):(h+2) of their default
# canvas sizes, as make-resizable sets them (docs/qt-gui-plan.md 1.1)
ASPECT = {"workspace": (802, 602), "coderack": (232, 600), "vertical-themes": (162, 592),
          "temperature": (72, 177), "slipnet": (652, 311), "themes": (602, 142)}

# the panes, in the Windows menu's order (docs/qt-gui-plan.md 2.3)
PANES = ["workspace", "slipnet", "coderack", "temperature", "trace", "commentary", "memory",
         "top-themes", "bottom-themes", "vertical-themes", "EEG"]

# the panes of each splitter, in order (a name that is not a pane is a splitter)
TREE = {
    "rows": ["top", "middle", "bottom"],
    "top": ["temperature", "workspace", "coderack", "vertical-themes", "commentary"],
    "middle": ["slipnet", "themes", "memory"],
    "themes": ["top-themes", "bottom-themes"],
    "bottom": ["trace", "EEG"],
}
VERTICAL = ("rows", "themes", "bottom")
STRETCHY = ("commentary", "memory", "trace")     # they take the extra space
HIDDEN_AT_START = ("EEG",)
MIN_REST = 200                  # the Commentary's and the Memory's default minimum


def _shrink(fixed, room):
    """fixed widths scaled down to add up to room (the first one takes the
    rounding)"""
    total = sum(fixed)
    scaled = [w * room // total for w in fixed]
    scaled[1 if len(scaled) > 1 else 0] += room - sum(scaled)
    return scaled


def _row(fixed, width):
    """a row of fixed-aspect widths and one rest pane, in width pixels"""
    if width - sum(fixed) < MIN_REST:
        fixed = _shrink(fixed, max(0, width - MIN_REST))
    return fixed + [width - sum(fixed)]


def default_sizes(width, height, handle=HANDLE, eeg=False):
    """docs/qt-gui-plan.md 2.2: each splitter's sizes for a central area of
    width x height pixels.  With the EEG shown (eeg), the bottom row is twice
    as high, shared by the Trace and the EEG, and the other rows keep their
    proportions in the rest."""
    def ratio(name, h):
        a, b = ASPECT[name]
        return round(h * a / b)
    avail = height - 2 * handle
    if eeg:
        bottom = max(100 + handle, round(0.18 * avail))
        top = round((avail - bottom) * 60 / 91)
    else:
        bottom = max(50, round(0.09 * avail))
        top = round(0.60 * avail)
    middle = avail - top - bottom
    temperature = max(60, round(0.06 * width))
    top_row = _row([temperature, ratio("workspace", top), ratio("coderack", top),
                    ratio("vertical-themes", top)], width - 4 * handle)
    theme_h = (middle - handle) // 2
    middle_row = _row([ratio("slipnet", middle), ratio("themes", (middle - handle) / 2)],
                      width - 2 * handle)
    return {"rows": [top, middle, bottom],
            "top": top_row,
            "middle": middle_row,
            "themes": [theme_h, middle - handle - theme_h],
            "bottom": ([(bottom - handle) // 2, bottom - handle - (bottom - handle) // 2]
                       if eeg else [bottom, bottom])}


class MainWindow(QMainWindow):
    """port: Metacat's windows, in one"""

    def __init__(self, settings=None):
        super().__init__()
        self.setWindowTitle(TITLE)
        self.setMinimumSize(*MINIMUM_SIZE)
        self.settings = settings      # a QSettings, or None: nothing saved
        self.splitters = {}
        for name in TREE:
            sp = QSplitter(Qt.Vertical if name in VERTICAL else Qt.Horizontal)
            sp.setObjectName(name)
            sp.setHandleWidth(HANDLE)
            sp.setChildrenCollapsible(False)
            sp.splitterMoved.connect(self._handle_dragged)
            self.splitters[name] = sp
        for name in ("top", "middle", "bottom"):
            self.splitters["rows"].addWidget(self.splitters[name])
        self.setCentralWidget(self.splitters["rows"])
        self.panes = {}
        self.hosts = {}
        self.control_panel = None
        self.bridge = None
        self.default_layout = True    # until the user drags a handle
        self.sync_timer = QTimer(self)
        self.sync_timer.setInterval(SYNC_INTERVAL)
        self.sync_timer.timeout.connect(self.sync)
        self.sync_timer.start()
        self.save_timer = QTimer(self)
        self.save_timer.setSingleShot(True)
        self.save_timer.setInterval(SAVE_DELAY)
        self.save_timer.timeout.connect(self.save_layout)
        self.resize(*DEFAULT_SIZE)

    def place_windows(self, windows):
        """the graphics windows (views.attach_views's dict, by name) as panes"""
        from metacat.objects import tell
        self.place_hosts({name: tell(w, "get-toplevel") for name, w in windows.items()})

    def place_hosts(self, hosts):
        """the Qt hosts (by window name) in the splitter tree"""
        self.hosts = {name: hosts[name] for name in PANES}
        self.panes = {name: host.pane for name, host in self.hosts.items()}
        for parent, children in TREE.items():
            if parent == "rows":
                continue
            sp = self.splitters[parent]
            for child in children:
                widget = self.splitters.get(child) or self.panes[child]
                sp.addWidget(widget)
                sp.setStretchFactor(sp.indexOf(widget), 1 if child in STRETCHY else 0)
        self.splitters["middle"].setStretchFactor(1, 0)
        for name, pane in self.panes.items():
            pane.setMinimumSize(120 if name in ("commentary", "memory") else 40, 40)
        self.panes["temperature"].v_align = "top"
        for name in HIDDEN_AT_START:
            self.hosts[name].hide_window()
            self.panes[name].hide()
        for name, pane in self.panes.items():
            if name not in HIDDEN_AT_START:
                pane.show()
        self.apply_default_layout()

    def place_control_panel(self, control_panel, bridge):
        """the control panel's strip above the panes (a fixed tool bar, so the
        central widget stays the splitter tree), its menus in the menu bar;
        when the engine parks, every pane is brought up to date at once"""
        from metacat.objects import tell
        self.control_panel, self.bridge = control_panel, bridge
        widgets = tell(control_panel, "get-widgets")
        bar = QToolBar("Control panel")
        bar.setObjectName("control-strip-bar")
        bar.setMovable(False)
        bar.setFloatable(False)
        bar.toggleViewAction().setEnabled(False)
        bar.addWidget(widgets["frame"])
        self.addToolBar(Qt.TopToolBarArea, bar)
        # a tool bar hides what doesn't fit behind an extension button: the
        # window's minimum width keeps the whole strip shown
        frame = widgets["frame"]
        margins = bar.sizeHint().width() - frame.sizeHint().width()
        self.setMinimumWidth(max(MINIMUM_SIZE[0], frame.minimumSizeHint().width() + margins))
        control_panel.attach_windows(self)
        from metacat.qt import controls
        bar = self.menuBar()
        bar.setFont(controls.qfont(controls.p_gui_menubar_font))
        bar.setProperty("swl-font", list(controls.swl.tcl_word(controls.p_gui_menubar_font)))
        for menu in widgets["menu-bar"]:
            bar.addMenu(menu)
        control_panel.parked_hook = self.sync

    def hidden_at_start(self):
        return HIDDEN_AT_START

    def set_pane_visible(self, name, on):
        """show or hide a pane (the View menu's window controllers; GUI
        thread): its host as SWL's show/hide, the pane, and the splitters that
        hold it, so that an empty splitter leaves no gap"""
        host = self.hosts[name]
        if on:
            host.show_window()
        else:
            host.hide_window()
        self.panes[name].setVisible(on)
        self.update_splitters()
        if name == "EEG" and self.default_layout:
            self.apply_default_layout()       # the bottom row's height follows the EEG
        if self.settings is not None:
            self.save_timer.start()

    def update_splitters(self):
        """a nested splitter is shown when one of its children is"""
        for name in ("themes", "top", "middle", "bottom"):
            sp = self.splitters[name]
            on = any(not sp.widget(i).isHidden() for i in range(sp.count()))
            if sp.isHidden() == on:
                sp.setVisible(on)

    def reset_layout(self):
        """View > Reset layout: the default sizes of docs/qt-gui-plan.md 2.2,
        following the window again; the saved layout is removed"""
        self.default_layout = True
        self.apply_default_layout()
        if self.settings is not None:
            self.save_timer.stop()
            for name in TREE:
                self.settings.remove("layout/" + name)
            for key in ("layout/custom", "view/hidden"):
                self.settings.remove(key)
            self.settings.sync()

    def apply_default_layout(self):
        c = self.centralWidget()
        sizes = default_sizes(c.width(), c.height(), HANDLE,
                              eeg=bool(self.panes) and not self.panes["EEG"].isHidden())
        for name, s in sizes.items():
            self.splitters[name].setSizes(s)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        if self.default_layout and self.panes:
            self.apply_default_layout()

    def _handle_dragged(self, pos, index):
        self.default_layout = False
        if self.settings is not None:
            self.save_timer.start()

    # --- the saved layout (docs/qt-gui-plan.md 2.3) ------------------------------

    def _saved(self, key, default=None):
        """a saved value, or default when there is none or it is an older
        layout version's"""
        s = self.settings
        if s is None:
            return default
        try:
            version = int(s.value("layout/version", 0))
        except (TypeError, ValueError):
            version = 0
        if version != SETTINGS_VERSION:
            return default
        value = s.value(key)
        return default if value is None else value

    def save_layout(self):
        """the geometry, the hidden panes and (once a handle has been
        dragged) each splitter's state, in the settings"""
        s = self.settings
        if s is None or not self.panes:
            return
        self.save_timer.stop()
        s.setValue("layout/version", SETTINGS_VERSION)
        s.setValue("window/geometry", self.saveGeometry())
        s.setValue("view/hidden", " ".join(n for n in PANES if self.panes[n].isHidden()))
        s.setValue("layout/custom", 0 if self.default_layout else 1)
        for name, sp in self.splitters.items():
            if self.default_layout:
                s.remove("layout/" + name)
            else:
                s.setValue("layout/" + name, sp.saveState())
        s.sync()

    def restore_geometry(self):
        """the saved window geometry (before the window is shown); False
        when there is none"""
        geometry = self._saved("window/geometry")
        return geometry is not None and self.restoreGeometry(QByteArray(geometry))

    def restore_layout(self):
        """the saved hidden panes (through the View menu's window
        controllers, so that its checkmarks follow) and splitter states (once
        the window is shown, so that the splitters have their real sizes);
        False when nothing was saved"""
        hidden = self._saved("view/hidden")
        if hidden is None or not self.panes:
            return False
        hidden = str(hidden).split()
        controllers = {c.name: c for c in (self.control_panel.window_controllers
                                           if self.control_panel is not None else [])}
        from metacat.objects import tell
        for name in PANES:
            on = name not in hidden
            if self.panes[name].isHidden() != (not on):
                if name in controllers:
                    tell(controllers[name], "show" if on else "hide")
                else:
                    self.set_pane_visible(name, on)
        if str(self._saved("layout/custom", 0)) in ("1", "true"):
            states = {name: self._saved("layout/" + name) for name in self.splitters}
            if all(state is not None for state in states.values()):
                for name, state in states.items():
                    self.splitters[name].restoreState(QByteArray(state))
                self.default_layout = False
        self.save_timer.stop()
        return True

    def closeEvent(self, event):
        self.save_layout()
        super().closeEvent(event)

    def splitter_sizes(self):
        return {name: sp.sizes() for name, sp in self.splitters.items()}

    def sync(self):
        """every pane's scene and view up to date with its canvas (GUI thread),
        the canvas commands held at the paint gate meanwhile"""
        from metacat.qt.canvas import PAINT_GATE
        with PAINT_GATE:
            shown = False
            for host in self.hosts.values():
                shown = host.sync() or shown
        if shown:
            self.update_splitters()

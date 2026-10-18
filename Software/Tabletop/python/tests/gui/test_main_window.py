"""Loop0006 item 02: the main window (`tabletop/gui/main_window.py`), after the Numbo
GUI's `tests/gui/test_gui_shell.py` and `test_polish.py` through the Seek-Whence GUI's
`test_main_window.py` and `test_answers_10.py`. pytest-qt, offscreen.

- The window is laid out as PANES.md section 3: the docks, their areas and tab groups,
  the hidden Configuration dock, the timeline toolbar, the six menus of section 4.
- A run through the window equals the plain run (events, draws, French's report line);
  every event comes on the GUI thread while the worker waits; so does
  `headless.run(window=True)`, the gate's MainWindow comparison.
- Stop mid-run leaves no live thread, and the next run is unchanged.
- Each view is painted exactly once per event (a counting probe, as Numbo's).
- Settings, the default layout file and both resets round-trip through a temporary INI
  and a temporary default file; a corrupt or old-version file falls back without error;
  the file holds no geometry and stays under 20 KB (ANSWERS.md 2).
- Nothing but the menu bar occupies the menu bar's rectangle; every tab group appears
  exactly once in the built-in layout, the default file and a restored state (the
  Seek-Whence GUI's screenshot defects, G-012); the window fits 1280 x 800 (G-014).
"""

import json
import threading

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, QRect, QSettings, Qt                    # noqa: E402
from PySide6.QtGui import QImage                                           # noqa: E402
from PySide6.QtTest import QTest                                           # noqa: E402
from PySide6.QtWidgets import QDockWidget, QMenuBar, QTabBar               # noqa: E402

from tabletop.config import PROFILES                                       # noqa: E402
from tabletop.gui import headless                                          # noqa: E402
from tabletop.gui.main_window import (AREAS, CENTRAL, DEFAULT_SIZE, DELAYS, PANES,  # noqa: E402
                                      SMALLEST_SIZE, STATE_VERSION, TIMELINE_NAME,
                                      TIMELINE_PAGES, MainWindow, about_text,
                                      engine_details, engine_note, title_for)
from tabletop.gui.models import layout_file                                # noqa: E402

from golden import V2, V2_PROFILES, plain_run                             # noqa: E402
from tabletop.problems import load_problem                                 # noqa: E402

TIMEOUT = 90000   # ms (30 s until v3, D-114: its runs are three times longer)
QUICK = ("surround-05", 3)      # the front page's quick start: on the v2 defaults 59 codelets, T 43.6, spoon3 (none) since v5's fix (D-116 to D-120); 44 and T 41.0 since item 08 (77 and 34.6 at item 07; 67 and 77.5 at item 05; 81 and 44.8 before); on the standard profile 384 codelets, 3861 events since v5
SHORT = ("buridan-03", 2)       # a short library run: 69 codelets, 738 events on the v2 defaults since v5 (D-116 to D-120); 84 and 913 since item 09 (46 and 495 at items 07 and 08; 66 and 687 at item 05; 36 and 386 at item 04; 79 and 722 after item 03; 15 and 190 in v1; G-059)


class Recorder:
    """A GUI-thread view: records each event, the thread it came on, and how many events
    the worker had published by then (Numbo GUI, `test_gui_shell.py`)."""

    def __init__(self, window):
        self.window = window
        self.events = []
        self.published = []
        self.threads = set()

    def on_event(self, event):
        self.events.append(event)
        self.published.append(self.window.controller.events_published)
        self.threads.add(threading.current_thread() is threading.main_thread())


def run_to_end(qtbot, window, problem, seed, profile="standard", v2=True):
    """A run through the window, on the v2 defaults (D-114) unless `v2` is False."""
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_run(problem, (V2_PROFILES if v2 else PROFILES)[profile], seed)
        window.controller.run_to_end()
    return window.outcome


def make_window(qtbot, settings=None, layout_path=None, size=None):
    w = MainWindow(settings=settings, layout_path=layout_path)
    qtbot.addWidget(w)
    if size is not None:
        w.resize(*size)
    w.show()
    qtbot.waitExposed(w)
    if size is not None:
        w.resize(*size)
        qtbot.waitUntil(lambda: (w.width(), w.height()) == tuple(size), timeout=TIMEOUT)
    qtbot.wait(20)          # the dock layout's pass after the first show (G-012)
    return w


def on_screen(widget):
    """Whether the widget has pixels on the window: a dock tabbed under another is
    `isVisible()` in Qt 6 but has an empty visible region (G-011)."""
    return not widget.visibleRegion().isEmpty()


def views(window):
    """Every view that takes events: the table, the timeline and the docks' placeholders."""
    return [window.table_view, window.timeline] + [getattr(window, f"{attr}_view")
                                                   for attr, *_ in PANES]


def visible_tab_bars(window):
    """The tab bars drawn on the window: visible, with tabs, inside the window's rect."""
    bars = []
    for bar in window.findChildren(QTabBar):
        if bar.parent() is window and bar.isVisible() and bar.count() > 0 \
                and window.rect().intersects(bar.geometry()):
            bars.append(tuple(bar.tabText(i) for i in range(bar.count())))
    return bars


TAB_GROUPS = (("Slipnet grid", "Slipnet network"),
              ("Worldview", "Competitions", "Temperature plot", "Roles"),
              ("Coderack bars", "Rack list"),
              ("Trace and event log", "Results"))


def one_group_each(window):
    bars = visible_tab_bars(window)
    for group in TAB_GROUPS:
        assert bars.count(group) == 1, (group, bars)
    assert len(bars) == len(TAB_GROUPS), bars


# -- the layout (PANES.md section 3) -------------------------------------------------------------

def test_the_window_is_laid_out_as_panes_md(window):
    docks = {d.objectName(): d for d in window.findChildren(QDockWidget)}
    assert set(docks) == {name for _a, name, *_ in PANES}
    for _attr, name, title, _pane, _pages, area, under, shown in PANES:
        dock = docks[name]
        assert dock.windowTitle() == title
        assert dock.features() & QDockWidget.DockWidgetFeature.DockWidgetMovable
        assert window.dockWidgetArea(dock) == AREAS[area], name
        assert dock.toggleViewAction().isChecked() == shown, name
        if under is not None:
            if not shown:       # Qt lists the tab siblings of visible docks only
                dock.show()
            assert docks[under] in window.tabifiedDockWidgets(dock), name
            if not shown:
                dock.hide()
    assert window.config_dock.isHidden()
    # The first of each tab group is on top; the thermometer stands between the table
    # and the Slipnet grid, the Coderack above the trace [F95 pp. 95-97].
    for first, others in ((window.slipnet_grid_view, [window.slipnet_network_view]),
                          (window.worldview_view, [window.competitions_view,
                                                   window.temperature_view]),
                          (window.coderack_view, [window.rack_view]),
                          (window.log_view, [window.results_view])):
        assert on_screen(first)
        assert not any(on_screen(v) for v in others)
    table, thermo = window.table_view.geometry(), window.thermometer_dock.geometry()
    grid, coderack = window.slipnet_grid_dock.geometry(), window.coderack_dock.geometry()
    assert table.right() < thermo.left() < thermo.right() < grid.left()
    assert window.palette_dock.geometry().right() < table.left()
    assert table.bottom() < coderack.top() < window.log_dock.geometry().top()
    assert window.timeline_bar.objectName() == TIMELINE_NAME
    assert window.toolBarArea(window.timeline_bar) == Qt.ToolBarArea.BottomToolBarArea
    assert window.centralWidget() is window.table_view
    assert window.controller.state == "idle" and window.state == "idle"
    assert window.layout_source == "built-in"
    assert window.windowTitle() == title_for("surround-05", "standard", 1)


def test_every_pane_without_a_figure_says_so(window):
    """ANSWERS.md 4: the new designs are labelled in the pane and its tooltip."""
    new = ("slipnet_network", "worldview", "competitions", "temperature", "rack", "roles")
    for attr, _name, _title, _pane, pages, *_ in PANES:
        view, dock = getattr(window, f"{attr}_view"), getattr(window, f"{attr}_dock")
        assert ("GUI design, not French's" in pages) == (attr in new or attr == "palette"), attr
        assert pages in view.toolTip() and pages in dock.toolTip()
        assert pages in view.pages


def test_the_menus_are_session_edit_run_problem_view_help(window):
    titles = [a.text() for a in window.menuBar().actions()]
    assert titles == ["&Session", "&Edit", "&Run", "&Problem", "&View", "&Help"]
    session = {a.text(): a for a in window.session_menu.actions() if a.text()}
    assert session["&New run"].shortcut().toString() == "Ctrl+N"
    assert session["&Open session…"].shortcut().toString() == "Ctrl+O"
    assert session["&Save session…"].shortcut().toString() == "Ctrl+S"
    # The session file actions (item 14, G-049): Open always, Save once there is a
    # history, Close replay in replay mode.
    assert session["&Open session…"].isEnabled()
    for text in ("&Save session…", "&Close replay"):
        assert not session[text].isEnabled() and "item" not in session[text].statusTip()
    # The log pane's files (item 11, G-042): there until a run has given a history.
    for text in ("Save &trace…", "Save &events as JSON lines…"):
        assert not session[text].isEnabled() and "item" not in session[text].statusTip()
    assert "&Quit" in session
    assert [a.text() for a in window.edit_menu.actions()] == ["&Configuration", "Edit &table"]
    run = {a.text(): a for a in window.run_menu.actions() if a.text()}
    keys = {text: run[text].shortcut().toString() for text in run}
    assert keys["Play / Pause"] == "Space" and keys["Step"] == "Right"
    assert keys["Step codelet"] == "Shift+Right" and keys["Back"] == "Left"
    assert keys["Stop"] == "Esc" and "Run to end" in run
    assert run["&Batch…"].isEnabled()        # item 16, G-052: the results pane's batch
    speeds = [a.text() for a in window.speed_menu.actions()]
    assert len(speeds) == len(DELAYS) and speeds[-1] == "no delay"
    assert [a.text() for a in window.problem_menu.actions()] == \
        ["surround", "blockage", "buridan", "figure-ground", "ch2"]
    assert len(window.problem_actions) == 53
    assert window.problem_actions["surround-05"].isChecked()
    assert window.problem_actions["surround-05"].text() == "Surround 5 (surround-05, p. 118)"
    view = [a.text() for a in window.view_menu.actions() if a.text()]
    for _attr, _name, title, *_ in PANES:
        assert title in view
    assert "Timeline" in view and "Table &overlays" in view and "&Eliza's view" in view
    assert "&Reset layout" in view and "Reset to &built-in layout" in view
    assert "Save layout as &default for this code…" in view
    assert [a.text() for a in window.menuBar().actions()[-1].menu().actions()] == \
        ["&What am I looking at?", "&About"]


def test_edit_configuration_shows_the_hidden_dock_and_edit_table_raises_the_palette(window):
    assert window.config_dock.isHidden()
    window.configuration_action.trigger()
    assert window.config_dock.isVisible() and on_screen(window.config_view)
    assert not on_screen(window.worldview_view)
    window.stats_dock.raise_()
    window.edit_table_action.trigger()
    assert on_screen(window.palette_view)


def test_the_problem_menu_sets_the_next_run_and_the_title(window):
    window.problem_actions["buridan-01"].trigger()
    assert window.problem == "buridan-01" and window.problem_actions["buridan-01"].isChecked()
    assert not window.problem_actions["surround-05"].isChecked()
    assert window.windowTitle() == "Tabletop — buridan-01, seed 1"
    window.set_next_run(profile="human", seed=7)
    assert window.windowTitle() == "Tabletop — buridan-01, seed 7 (human)"
    assert window.next_config() == PROFILES["human"]
    with pytest.raises(ValueError):
        window.set_next_run(problem="no-such-problem")
    with pytest.raises(ValueError):
        window.set_next_run(profile="no-such-profile")
    assert window.problem == "buridan-01" and window.profile == "human"


def test_the_table_shows_the_next_runs_problem_before_play(window, qtbot):
    """The problem chosen is on the table at once, bare; a live run keeps its own problem
    on the table; after the run, another choice swaps the table alone (G-064)."""
    model = window.table_view.view.model
    assert window.problem == "surround-05" == model.problem
    assert set(model.objects) == {o.id for o in load_problem("surround-05").objects}
    assert model.touched == load_problem("surround-05").touched and model.answer is None
    window.problem_actions["buridan-01"].trigger()
    assert model.problem == "buridan-01"
    assert set(model.objects) == {o.id for o in load_problem("buridan-01").objects}
    window.set_next_run(profile="human", seed=7)          # the same problem: untouched
    assert model.problem == "buridan-01"
    problem, seed = QUICK
    window.start_run(problem, V2_PROFILES["standard"], seed, paused=True)
    window.step()
    qtbot.waitUntil(lambda: window.position == 0, timeout=TIMEOUT)
    window.set_next_run(problem="buridan-01")             # live: the table keeps its run
    assert model.problem == problem and window.problem == "buridan-01"
    window.stop()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert model.problem == problem
    window.set_next_run(problem="buridan-02")             # stopped: the table alone changes
    assert model.problem == "buridan-02" and len(window.history) == 1
    assert window.started == (problem, "standard", seed)   # the title and panes keep the run


def test_the_speed_menu_sets_the_delay(window):
    assert window.controller.delay == 0.01
    window.speed_menu.actions()[-1].trigger()
    assert window.controller.delay == 0.0
    window.set_delay(0.5)
    assert window.controller.delay == 0.5
    assert [a.text() for a in window.speed_actions.actions() if a.isChecked()] == ["0.5 s per event"]


def test_the_guide_and_about_texts(window):
    """Help > What am I looking at? lists every pane with its pages (item 16; G-053):
    the docks', the table's and the timeline's, the words of the pane table."""
    import html
    text = window.guide_text()
    for _attr, _name, title, pane, pages, *_ in PANES:
        assert f"<b>{pane}</b> {title}</td>" in text, pane
        assert html.escape(pages, quote=False) in text, pane
    assert "<b>P1</b> Table" in text and html.escape(CENTRAL[3], quote=False) in text
    assert "<b>P11</b> Timeline" in text and TIMELINE_PAGES in text
    assert "new drawings, not French's" in text and "What to expect" in text
    assert "no context push in this run" in text
    # Help's page is generated from the user guide (item 17; G-055): the window shows
    # exactly the HTML of the file's table, and the guide names every dock.
    from tabletop.gui.models import guide as gm
    assert gm.GUIDE_PATH.is_file()
    assert text == gm.guide_html(gm.read_guide())
    guide = gm.GUIDE_PATH.read_text(encoding="utf-8")
    for dock in window.docks.values():
        assert dock.windowTitle() in guide, dock.windowTitle()
    about = about_text()
    assert "Subtlety of Sameness" in about and "What am I looking at?" in about


def test_the_status_bar_names_the_engine_and_about_says_which(window):
    """The engine in the window (G-063; D-107): a permanent label at the right of the
    status bar says `engine: compiled`, or `engine: python (explicit request)` when the
    pure engine was asked for; its tooltip and Help > About give the build's details
    (the stamp, the compiler, the Cython version and the pinned flags, D-110) or say
    that the pure engine is the specification; the status messages keep the left and
    the title stays the run's."""
    from tabletop import accel
    engine = accel.info()["engine"]
    label = window.engine_label
    assert label.text() == engine_note() == f"engine: {engine}" + (
        " (explicit request)" if engine == "python" else "")
    assert label.toolTip() == engine_details()
    if engine == "compiled":
        assert accel.info()["build_stamp"][:12] in label.toolTip()
        assert accel.info()["cflags"] in label.toolTip() and "reference" in label.toolTip()
    else:
        assert "specification" in label.toolTip() and "explicit request" in label.toolTip()
    assert engine_details() in about_text() and "G-063" in about_text()
    assert label.parent() is window.statusBar()
    window.statusBar().showMessage("a message")
    assert window.statusBar().currentMessage() == "a message" and label.text() == engine_note()
    assert window.windowTitle() == title_for("surround-05", "standard", 1)


# -- runs through the window -------------------------------------------------------------------

def test_a_run_through_the_window_equals_the_plain_run(window, qtbot):
    recorder = Recorder(window)
    window.add_view(recorder)
    drawn = []
    window.event_drawn.connect(drawn.append)
    problem, seed = QUICK
    outcome = run_to_end(qtbot, window, problem, seed, v2=False)
    plain = plain_run(problem, seed)
    assert list(window.history.records) == list(plain.records)
    assert tuple(recorder.events) == plain.events
    assert outcome.result.draws == plain.draws
    assert outcome.result == plain.result
    assert window.statusBar().currentMessage() == plain.report == \
        "Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3."   # v5's fix, D-116 to D-120 (structure 88, 305 codelets, T 17.8 in v4, D-115; small-glass1 (E1), 144 codelets, T 31.0 in v3)
    n = len(plain.records)
    # Every event came on the GUI thread, while the worker waited on it.
    assert recorder.threads == {True}
    assert recorder.published == list(range(1, n + 1))
    assert drawn == list(range(n))
    assert window.position == n - 1
    for view in views(window):
        assert view.events == n
    assert window.state == "finished" and not window.controller.is_alive()
    assert window.started == (problem, "standard", seed)
    assert window.windowTitle() == title_for(problem, "standard", seed)
    assert window.history.state_at(n - 1) is not None


def test_stop_mid_run_leaves_no_live_thread(window, qtbot):
    problem, seed = QUICK
    window.start_run(problem, V2_PROFILES["standard"], seed, paused=True)
    for n in range(1, 4):
        window.step()
        qtbot.waitUntil(lambda: window.position == n - 1, timeout=TIMEOUT)
    assert window.controller.is_alive()
    window.stop()
    assert not window.controller.is_alive()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.outcome.stopped and window.state == "stopped"
    assert len(window.history) == 3
    assert "stopped after 3 events" in window.statusBar().currentMessage()
    # A run after a stop is unchanged.
    run_to_end(qtbot, window, problem, seed)
    assert list(window.history.records) == list(plain_run(problem, seed, V2).records)


def test_a_bad_problem_is_said_not_raised(window):
    window.problem = "no-such-problem"
    window.play()
    assert not window.controller.is_alive()
    assert "no-such-problem" in window.statusBar().currentMessage()


# -- the history: seeking ---------------------------------------------------------------------

def test_seeking_after_a_run(window, qtbot):
    problem, seed = QUICK
    run_to_end(qtbot, window, problem, seed)
    n = len(window.history)
    drawn = []
    window.event_drawn.connect(drawn.append)
    window.back_one()
    assert window.position == n - 2 and drawn == [n - 2]
    window.step_or_forward()
    assert window.position == n - 1
    assert window.seek(0) and window.position == 0
    assert all(view.events == 1 for view in views(window))
    assert window.seek(10) and all(view.events == 11 for view in views(window))
    assert not window.seek(n) and not window.seek(-1)


# -- the every-event redraw (Numbo GUI, test_polish.py) -------------------------------------------

class PaintProbe:
    """At each `event_drawn`: every view on screen was painted exactly once since the
    probe's `on_event`, and no hidden one at all (Numbo GUI, `test_polish.py`)."""

    def __init__(self, window):
        self.window = window
        self.views = views(window)
        self.before = {}
        self.anomalies = []
        self.checked = 0
        window.add_view(self)
        window.event_drawn.connect(self.drawn)

    def on_event(self, event):
        self.before = {v: v.paints for v in self.views}

    def drawn(self, index):
        self.checked += 1
        for view in self.views:
            paints = view.paints - self.before.get(view, view.paints)
            expected = 1 if on_screen(view) else 0
            if paints != expected:
                self.anomalies.append((index, view.pane_id, paints, expected))


def test_each_view_is_painted_exactly_once_per_event(window, qtbot):
    probe = PaintProbe(window)
    problem, seed = QUICK
    run_to_end(qtbot, window, problem, seed)
    assert probe.checked == len(window.history) > 100
    assert probe.anomalies == []


# -- QSettings, the default layout file and the resets (ANSWERS.md 2) ---------------------------

def ini_settings(ini):
    return QSettings(ini, QSettings.Format.IniFormat)


def test_the_window_state_and_inputs_are_remembered(qtbot, ini, thread_errors):
    w = make_window(qtbot, ini_settings(ini))
    w.worldview_dock.hide()
    w.set_next_run(problem="buridan-02", profile="buridan", seed=7)
    w.set_delay(0.0)
    w.close()
    w2 = make_window(qtbot, ini_settings(ini))
    try:
        assert w2.layout_source == "settings"
        assert w2.worldview_dock.isHidden() and not w2.stats_dock.isHidden()
        assert (w2.problem, w2.profile, w2.seed) == ("buridan-02", "buridan", 7)
        assert w2.controller.delay == 0.0
        assert w2.windowTitle() == title_for("buridan-02", "buridan", 7)
        # Reset layout: the built-in (there is no default file here); the saved state
        # is cleared.
        w2.reset_layout()
        assert not w2.worldview_dock.isHidden()
        assert w2.layout_source == "built-in"
        assert not ini_settings(ini).contains("window/state")
    finally:
        w2.close()


def test_bad_settings_give_the_defaults(qtbot, ini, thread_errors, capfd):
    s = ini_settings(ini)
    s.setValue("window/geometry", b"not a geometry")
    s.setValue("window/state", "junk")
    s.setValue("controls/problem", "eleventy")
    s.setValue("controls/profile", "nope")
    s.setValue("controls/seed", "many")
    s.setValue("controls/speed", 99)
    s.sync()
    w = make_window(qtbot, ini_settings(ini))
    try:
        assert w.layout_source == "built-in"
        assert (w.problem, w.profile, w.seed) == ("surround-05", "standard", 1)
        assert w.controller.delay == 0.01
        assert not w.worldview_dock.isHidden()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


def test_a_state_of_another_version_is_ignored_and_said(qtbot, ini, thread_errors):
    w = make_window(qtbot)
    w.worldview_dock.hide()
    old = w.saveState(STATE_VERSION + 1)
    w.close()
    s = ini_settings(ini)
    s.setValue("window/state", old)
    s.sync()
    w2 = make_window(qtbot, ini_settings(ini))
    try:
        assert w2.layout_source == "built-in"
        assert not w2.worldview_dock.isHidden()
        assert "another version" in w2.statusBar().currentMessage()
        one_group_each(w2)
    finally:
        w2.close()
    assert ini_settings(ini).value("window/state") != old      # the close saved the current version


def test_a_window_without_settings_writes_none(qtbot, tmp_path, thread_errors):
    QSettings.setPath(QSettings.Format.IniFormat, QSettings.Scope.UserScope, str(tmp_path))
    w = make_window(qtbot, layout_path=tmp_path / "default_layout.json")
    w.close()
    assert list(tmp_path.iterdir()) == []


def test_save_layout_as_default_and_a_fresh_window_opens_with_it(qtbot, tmp_path, thread_errors):
    path = tmp_path / "default_layout.json"
    w = make_window(qtbot, layout_path=path)
    assert w.layout_source == "built-in"           # no file yet
    w.stats_dock.hide()
    w.competitions_dock.raise_()
    assert w.save_layout_as_default() == path
    w.close()
    text = path.read_text()
    data = json.loads(text)
    assert data["format"] == layout_file.FORMAT and data["version"] == layout_file.VERSION
    assert "geometry" not in text and len(text.encode()) < layout_file.MAX_BYTES
    placements = {d["name"]: d for d in data["docks"]}
    assert placements["stats-dock"]["visible"] is False
    assert set(placements["worldview-dock"]["tabbed_with"]) == {"competitions-dock",
                                                                "temperature-dock", "roles-dock"}
    assert data["toolbars"] == [{"name": TIMELINE_NAME, "visible": True}]

    w2 = make_window(qtbot, layout_path=path)
    try:
        assert w2.layout_source == "default file"
        assert w2.stats_dock.isHidden()
        assert on_screen(w2.competitions_view) and not on_screen(w2.worldview_view)
        one_group_each(w2)
        w2.reset_builtin_layout()
        assert w2.layout_source == "built-in" and not w2.stats_dock.isHidden()
        assert on_screen(w2.worldview_view) and not on_screen(w2.competitions_view)
        w2.reset_layout()
        assert w2.layout_source == "default file" and w2.stats_dock.isHidden()
    finally:
        w2.close()


def test_the_users_settings_override_the_default_file(qtbot, ini, tmp_path, thread_errors):
    path = tmp_path / "default_layout.json"
    w = make_window(qtbot, layout_path=path)
    w.stats_dock.hide()
    w.save_layout_as_default()
    w.close()
    w2 = make_window(qtbot, ini_settings(ini), layout_path=path)
    assert w2.layout_source == "default file" and w2.stats_dock.isHidden()
    w2.stats_dock.show()
    w2.controls_dock.hide()
    w2.close()
    w3 = make_window(qtbot, ini_settings(ini), layout_path=path)
    try:
        assert w3.layout_source == "settings"
        assert w3.controls_dock.isHidden() and not w3.stats_dock.isHidden()
        w3.reset_layout()                           # back to the file's arrangement
        assert w3.layout_source == "default file"
        assert w3.stats_dock.isHidden() and not w3.controls_dock.isHidden()
    finally:
        w3.close()


@pytest.mark.parametrize("text", [
    "not json at all",
    json.dumps({"format": "something-else", "version": 1}),
    json.dumps({"format": layout_file.FORMAT, "version": 99, "docks": [], "state": "AAAA"}),
    json.dumps({"format": layout_file.FORMAT, "version": 1, "state": "AAAA",
                "docks": [{"name": "no-such-dock", "visible": True, "area": "left"}]}),
    json.dumps({"format": layout_file.FORMAT, "version": 1, "state": "AAAA", "docks": []}),
])
def test_a_bad_default_file_falls_back_to_the_built_in(qtbot, tmp_path, thread_errors, capfd, text):
    path = tmp_path / "default_layout.json"
    path.write_text(text)
    w = make_window(qtbot, layout_path=path)
    try:
        assert w.layout_source == "built-in"
        assert "built-in layout" in w.statusBar().currentMessage()
        assert not w.stats_dock.isHidden() and w.config_dock.isHidden()
        one_group_each(w)
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


def test_an_unwritable_default_file_raises_os_error(qtbot, tmp_path, thread_errors):
    w = make_window(qtbot, layout_path=tmp_path / "no-such-dir" / "default_layout.json")
    try:
        with pytest.raises(OSError):
            w.save_layout_as_default()
    finally:
        w.close()


def test_the_shipped_default_layout_equals_the_built_in(qtbot, thread_errors):
    record = layout_file.read_layout(layout_file.DEFAULT_PATH)
    assert record is not None and record.version == layout_file.VERSION
    assert layout_file.DEFAULT_PATH.stat().st_size < layout_file.MAX_BYTES
    assert "geometry" not in layout_file.DEFAULT_PATH.read_text()
    by_name = {d.name: d for d in record.docks}
    assert set(by_name) == {name for _a, name, *_ in PANES}
    for _attr, name, _title, _pane, _pages, area, under, shown in PANES:
        assert by_name[name].visible == shown and by_name[name].area == area, name
        if under is not None and shown:     # a hidden dock records no tab siblings
            assert under in by_name[name].tabbed_with
    assert record.toolbars == ((TIMELINE_NAME, True),)
    w = make_window(qtbot, layout_path=layout_file.DEFAULT_PATH)
    try:
        assert w.layout_source == "default file"
        for _attr, name, *_rest, shown in PANES:
            assert w.docks[name].toggleViewAction().isChecked() == shown, name
        for view in (w.slipnet_grid_view, w.worldview_view, w.coderack_view, w.log_view):
            assert on_screen(view)
        for view in (w.slipnet_network_view, w.competitions_view, w.temperature_view,
                     w.roles_view, w.rack_view, w.results_view):
            assert not on_screen(view)
        one_group_each(w)
    finally:
        w.close()


# -- the Seek-Whence GUI's screenshot defects (G-012) and the smallest size (G-014) -------------

SIZE = (1920, 1080)


def test_nothing_but_the_menu_bar_occupies_the_menu_bars_rectangle(qtbot, window):
    window.resize(*SIZE)
    qtbot.wait(20)
    bar = window.menuBar()
    rect = bar.geometry()
    for widget in window.findChildren(QTabBar):
        # Every tab bar, a dock group's or a pane's own, in the window's coordinates.
        where = QRect(widget.mapTo(window, QPoint(0, 0)), widget.size())
        assert not (widget.isVisible() and where.intersects(rect)), widget.tabText(0)
    for child in window.children():
        if hasattr(child, "isVisible") and child.isVisible() and child is not bar \
                and hasattr(child, "geometry") and not isinstance(child, QMenuBar):
            assert not child.geometry().intersects(rect), (type(child).__name__, child.objectName())
    session = bar.actions()[0]
    assert session.text() == "&Session"
    centre = bar.actionGeometry(session).center()
    assert window.childAt(bar.mapTo(window, centre)) is bar
    fmt = QImage.Format.Format_RGB32
    assert window.grab(rect).toImage().convertToFormat(fmt) == \
        bar.grab().toImage().convertToFormat(fmt)                 # the pixels are the bar's own
    QTest.mouseClick(bar, Qt.MouseButton.LeftButton, Qt.KeyboardModifier.NoModifier, centre)
    qtbot.waitUntil(lambda: bar.activeAction() is session, timeout=2000)
    menu = session.menu()
    assert menu is not None and menu.isVisible()
    menu.close()


def test_every_tab_group_appears_exactly_once(qtbot, ini, thread_errors):
    w = make_window(qtbot, size=SIZE)                      # no default file: built-in
    assert w.layout_source == "built-in"
    one_group_each(w)
    w.reset_builtin_layout()
    qtbot.wait(20)
    one_group_each(w)
    w.close()
    w = make_window(qtbot, layout_path=layout_file.DEFAULT_PATH, size=SIZE)
    assert w.layout_source == "default file"
    one_group_each(w)
    w.reset_layout()
    qtbot.wait(20)
    one_group_each(w)
    w.close()
    w = make_window(qtbot, ini_settings(ini), size=SIZE)
    w.stats_dock.hide()
    w.close()
    w = make_window(qtbot, ini_settings(ini), size=SIZE)
    try:
        assert w.layout_source == "settings" and w.stats_dock.isHidden()
        one_group_each(w)
    finally:
        w.close()


def test_the_window_fits_1280_by_800_with_every_dock_shown(qtbot, window):
    for dock in window.docks.values():
        dock.show()
    qtbot.waitUntil(lambda: not any(d.isHidden() for d in window.docks.values()))
    window.resize(*SMALLEST_SIZE)
    qtbot.waitUntil(lambda: (window.width(), window.height()) == SMALLEST_SIZE, timeout=TIMEOUT)
    hint = window.minimumSizeHint()
    assert (hint.width(), hint.height()) <= SMALLEST_SIZE, (hint.width(), hint.height())
    for _attr, name, title, *_ in PANES:
        dock = window.docks[name]
        assert dock.isVisible() and not dock.isFloating(), title
        if dock.visibleRegion().isEmpty():      # under another tab: parked off the window
            assert window.tabifiedDockWidgets(dock), title
        else:
            assert window.rect().contains(dock.geometry()), title
    assert window.config_dock in window.tabifiedDockWidgets(window.worldview_dock)
    assert DEFAULT_SIZE >= SMALLEST_SIZE

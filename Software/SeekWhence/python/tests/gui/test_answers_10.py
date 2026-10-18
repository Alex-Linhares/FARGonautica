"""Loop0004, ANSWERS.md 10: the four defects seen in a screenshot of run 3, fixed in
iteration 17 (G-052, G-053, G-054), offscreen at 1920x1080 as the author's assistant
grabbed it.

1. Nothing but the menu bar occupies the menu bar's rectangle once the window is shown,
   and the Session menu is clickable: Qt's stale "Console" tab bar is gone (G-052).
2. The built-in layout, the repository default and a restored state each give exactly one
   visible tab bar for the console and event log docks (and one for the hypothesis and
   reformulation docks), also after Reset layout.
3. Whichever way a run is started (the Presets menu, the controls, `start_session`, a
   session file replayed, the command line), the controls' preset, the window title and
   the Stats "Run" row agree with the inputs used, or show `custom` (G-054).
4. After run 3 the cytoplasm draws the pseudo-gloms over terms 1-2 and 3-5, matching
   `show-parse` `((1 2) (1 2 3))` [M86 p. 178], in the grey pen and brush, distinct from a
   real glom's; the toggle hides and shows them (G-053).
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, QSettings, Qt                       # noqa: E402
from PySide6.QtGui import QColor, QImage                               # noqa: E402
from PySide6.QtTest import QTest                                       # noqa: E402
from PySide6.QtWidgets import QMenuBar, QTabBar                        # noqa: E402

from seekwhence.gui import app as gui_app                              # noqa: E402
from seekwhence.gui.main_window import MainWindow, title_for           # noqa: E402
from seekwhence.gui.models import cytoplasm as cy                      # noqa: E402
from seekwhence.gui.models import layout_file, presets                 # noqa: E402

from test_main_window import RUN1, RUN3, make_window, run_to_end       # noqa: E402

SIZE = (1920, 1080)


def shown(qtbot, settings=None, layout_path=None):
    w = make_window(qtbot, settings, layout_path)
    w.resize(*SIZE)
    qtbot.wait(20)
    return w


def visible_tab_bars(window):
    """The tab bars drawn on the window: visible, with tabs, inside the window's rect."""
    bars = []
    for bar in window.findChildren(QTabBar):
        if bar.parent() is window and bar.isVisible() and bar.count() > 0 \
                and window.rect().intersects(bar.geometry()):
            bars.append(tuple(bar.tabText(i) for i in range(bar.count())))
    return bars


def one_group_each(window):
    bars = visible_tab_bars(window)
    assert bars.count(("Console", "Event log")) == 1, bars
    assert bars.count(("Hypothesis", "Reformulation")) == 1, bars
    assert len(bars) == 2, bars


# -- 1 and 2: the stray "Console" label and the double tab bar ------------------------------------

def test_nothing_but_the_menu_bar_occupies_the_menu_bars_rectangle(qtbot, window):
    window.resize(*SIZE)
    qtbot.wait(20)
    bar = window.menuBar()
    rect = bar.geometry()
    for widget in window.findChildren(QTabBar):
        assert not (widget.isVisible() and widget.geometry().intersects(rect)), widget.tabText(0)
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


def test_the_built_in_layout_has_one_tab_group_for_the_console_and_the_log(qtbot, thread_errors):
    w = shown(qtbot)                                               # no default file: built-in
    try:
        assert w.layout_source == "built-in"
        one_group_each(w)
        assert set(d.objectName() for d in w.tabifiedDockWidgets(w.console_dock)) == {"log-dock"}
        w.reset_builtin_layout()
        qtbot.wait(20)
        one_group_each(w)
    finally:
        w.close()


def test_the_repository_default_has_one_tab_group(qtbot, thread_errors):
    w = shown(qtbot, layout_path=layout_file.DEFAULT_PATH)
    try:
        assert w.layout_source == "default file"
        one_group_each(w)
        w.reset_layout()
        qtbot.wait(20)
        one_group_each(w)
        run_to_end(qtbot, w, presets.appendix_inputs(RUN3))     # the screenshot's run
        one_group_each(w)
    finally:
        w.close()


def test_a_restored_state_has_one_tab_group(qtbot, ini, thread_errors):
    w = shown(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    w.platoplasm_dock.hide()
    w.close()
    w2 = shown(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w2.layout_source == "settings" and w2.platoplasm_dock.isHidden()
        one_group_each(w2)
    finally:
        w2.close()


# -- 3: the preset shown agrees with the run ---------------------------------------------------

def agree(window, inputs, replaying=None):
    expected = inputs.preset
    assert window.controls.preset == expected, (window.controls.preset, expected)
    assert window.stats_view.model.title == (inputs.title or inputs.preset)
    if replaying is None:
        assert window.windowTitle() == title_for(inputs)
    else:
        assert window.windowTitle() == (f"Seek-Whence — replaying {replaying}: "
                                        f"{inputs.preset}, seed {inputs.seed}")


def test_start_session_shows_its_inputs(qtbot, window):
    assert window.controls.preset == RUN1
    inputs = presets.appendix_inputs(RUN3)
    run_to_end(qtbot, window, inputs)
    agree(window, inputs)
    custom = presets.custom_inputs(inputs.config, 5, ("1", "2", "3"))
    run_to_end(qtbot, window, custom)
    agree(window, custom)
    assert window.controls.preset == "custom" and window.controls.values().terms == "1 2 3"


def test_the_presets_menu_and_the_controls_show_their_run(qtbot, window):
    action = next(a for a in window.presets_menu.actions() if a.text().startswith("run 3"))
    action.trigger()
    agree(window, window.inputs)
    assert window.inputs.preset == RUN3
    window.choose_preset("blackburn-3")
    assert window.inputs.preset == "blackburn-3"
    agree(window, window.inputs)
    window.choose_preset("run2_373373")                 # the controls' combo, then Play
    window.run_to_end()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=60000)
    assert window.controls.preset == "run2_373373"
    agree(window, window.inputs)


def test_a_replayed_session_file_shows_its_run(qtbot, window, tmp_path):
    inputs = presets.appendix_inputs(RUN3)
    run_to_end(qtbot, window, inputs)
    path = tmp_path / "s.jsonl"
    assert window.save_session(path)
    window.choose_preset("blackburn-1")
    assert window.open_session(path)
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=10000)
    replay = window.inputs if window.replay_source is None else window._replay.inputs
    agree(window, replay, replaying="s.jsonl")
    assert replay.preset == RUN3
    window.close_replay()
    assert window.inputs.preset == "blackburn-1"                    # the inputs chosen before
    agree(window, window.inputs)


def test_the_command_line_shows_its_run(window):
    args = gui_app.parser().parse_args(["--preset", "blackburn-2", "--seed", "3"])
    inputs = gui_app.apply_arguments(window, args)
    agree(window, inputs)


# -- 4: the pseudo-gloms in grey, by default ----------------------------------------------------

def test_after_run_3_the_pseudo_gloms_show_the_parse_in_grey(qtbot, window):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    canvas = window.cytoplasm_view
    view = canvas.view
    assert canvas.layer_actions["top-down"].isChecked() and view.model.layers["top-down"]
    pseudo = {name: item for name, item in view.hull_items.items() if item.shape_.pseudo}
    spans = sorted((item.shape_.name, view.model.gloms[name].start, view.model.gloms[name].end)
                   for name, item in pseudo.items() if name in view.model.gloms)
    assert {(s, e) for _n, s, e in spans} >= {(1, 2), (3, 5)}       # show-parse ((1 2) (1 2 3))
    real = [item for name, item in view.hull_items.items() if not item.shape_.pseudo]
    for item in pseudo.values():
        assert item.style.dotted and item.style.border == cy.PSEUDO_BORDER
        assert item.style.fill == cy.PSEUDO_FILL
        assert QColor(item.style.fill).lightness() > QColor(cy.HULL_FILL).lightness()
        assert QColor(item.style.border).saturation() == 0           # grey
    for item in real:
        assert item.style.border != cy.PSEUDO_BORDER and item.style.fill != cy.PSEUDO_FILL
    assert "pseudo-glom (grey)" in canvas.legend.text()
    canvas.layer_actions["top-down"].setChecked(False)
    assert not any(item.shape_.pseudo for item in view.hull_items.values())
    canvas.layer_actions["top-down"].setChecked(True)
    assert set(pseudo) <= set(view.hull_items)

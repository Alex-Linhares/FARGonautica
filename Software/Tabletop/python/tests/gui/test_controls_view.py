"""Loop0006 item 12: the controls pane (`tabletop/gui/controls.py`; G-044) in the window,
offscreen, pytest-qt, after the Seek-Whence GUI's `test_controls_view.py` and the Numbo
GUI's `test_gui_shell.py`.

- The dock holds the panel with every problem by family, the profiles, the cap.
- Every entry point agrees (PANES.md P9; loop0004 ANSWERS 10.3): the panel, the Problem
  menu, `set_next_run` (the command line's path) and `start_run` called directly show
  the same problem, profile and seed in the panel, the title and the stats Run line.
- The shortcuts drive the controller; so do the buttons; the same seed repeats the run.
- An invalid seed shows an error, refuses the start and prints no traceback.
- The speed slider, the Run > Speed menu and the controller agree.
- The inputs round-trip through a temporary INI; bad settings give the defaults.
- The run line is on screen and says the state (`test_main_window.py`'s paint probe
  counts its paints); the minimum.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings, Qt                                  # noqa: E402
from PySide6.QtTest import QTest                                          # noqa: E402

from tabletop.config import PROFILES                                      # noqa: E402
from tabletop.gui import controls as cv                                   # noqa: E402
from tabletop.gui.main_window import MainWindow, title_for                # noqa: E402
from tabletop.gui.models import controls as cm                            # noqa: E402
from tabletop.gui.models.run_stats import run_text                        # noqa: E402

TIMEOUT = 90000   # ms (30 s until v3, D-114: its runs are three times longer)
QUICK = ("surround-05", 3)
SHORT = ("buridan-03", 2)       # a short library run: 69 codelets, 738 events on the v2 defaults since v5 (D-116 to D-120), 162 and 1521 on the standard profile; 84 and 913 since item 09 (46 and 495 at items 07 and 08; 36 and 386 at item 04; 79 and 722 after item 03; 15 and 190 in v1; G-059)


def wait_finished(qtbot, window):
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    qtbot.waitUntil(lambda: not window.controller.is_alive(), timeout=TIMEOUT)
    return window.outcome


def key(qtbot, w, k, modifier=Qt.KeyboardModifier.NoModifier):
    """A key on the window, as a user would press it (the table view has the focus)."""
    w.activateWindow()
    qtbot.waitActive(w)
    w.table_view.view.setFocus()
    QTest.keyClick(w.table_view.view, k, modifier)


@pytest.fixture
def window(module_window):
    """The module's shared window with the defaults back (ANSWERS.md 10): the next-run
    inputs, the speed, no error."""
    w = module_window
    w.stop()
    w.set_next_run(problem="surround-05", profile="standard", seed=1)
    w.set_delay(cm.DEFAULT_DELAY)
    w.statusBar().clearMessage()
    return w


def make_window(qtbot, settings=None):
    w = MainWindow(settings=settings, layout_path=None)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    qtbot.wait(20)          # the dock layout's pass after the first show (G-012)
    return w


def agree(window, problem, profile, seed):
    """The panel, the window's inputs and the title show the same run (PANES.md P9)."""
    c = window.controls_view
    assert c.values().problem == problem and c.values().profile == profile
    assert c.values().seed == str(seed)
    assert (window.problem, window.profile, window.seed) == (problem, profile, seed)
    assert window.windowTitle() == title_for(problem, profile, seed)
    assert window.problem_actions[problem].isChecked()


# -- the dock and the panel (G-044) --------------------------------------------------------------

def test_the_dock_holds_the_panel_with_every_problem(window):
    c = window.controls_view
    assert window.controls_dock.widget() is c and isinstance(c, cv.ControlsPanel)
    assert c.pane_id == "P9" and "Numbo GUI" in c.pages
    names = [c.problem_combo.itemData(i) for i in range(c.problem_combo.count())]
    names = [n for n in names if n is not None]
    assert names == [e.name for e in cm.catalogue()] and names[0] == "surround-05"
    assert len(names) == 53
    headers = [c.problem_combo.itemText(i) for i in range(c.problem_combo.count())
               if c.problem_combo.itemData(i) is None and c.problem_combo.itemText(i)]
    assert len(headers) == 5 and all(h.startswith("— ") for h in headers)
    assert c.problem_combo.currentData() == "surround-05"
    assert "Surround 5" in c.problem_label.text() and "p. 118" in c.problem_label.text()
    assert [c.profile_combo.itemData(i) for i in range(c.profile_combo.count())] == list(PROFILES)
    assert c.cap_label.text().startswith("3000 codelets")
    assert c.seed_edit.text() == "1"
    assert c.run_line.text() == "idle: no run yet"
    assert [b.text() for b in c.buttons] == ["Play", "Pause", "Step", "Step codelet",
                                              "Run to end", "Stop", "Back"]
    assert not c.pause_button.isEnabled() and not c.stop_button.isEnabled()
    assert c.back_button.isEnabled()


# -- every entry point agrees (PANES.md P9) ----------------------------------------------------------

def test_the_panel_sets_the_next_run(window):
    c = window.controls_view
    c.problem_combo.setCurrentIndex(c.problem_combo.findData("buridan-01"))
    agree(window, "buridan-01", "standard", 1)
    assert "Buridan 1" in c.problem_label.text()
    c.profile_combo.setCurrentIndex(c.profile_combo.findData("human"))
    agree(window, "buridan-01", "human", 1)
    assert window.next_config() == PROFILES["human"]
    c.seed_edit.setText("12")
    c.seed_edit.editingFinished.emit()
    agree(window, "buridan-01", "human", 12)


def test_the_problem_menu_and_set_next_run_show_in_the_panel(window):
    window.problem_actions["blockage-06"].trigger()
    agree(window, "blockage-06", "standard", 1)
    # the command line's path (app.py)
    window.set_next_run(problem="surround-02", profile="french-faults", seed=5)
    agree(window, "surround-02", "french-faults", 5)
    assert window.controls_view.cap_label.text().startswith("3000 codelets")


def test_a_run_started_directly_shows_in_the_panel_the_title_and_the_stats(window, qtbot):
    window.set_next_run(problem="buridan-02", profile="human", seed=9)
    window.start_run("blockage-06", PROFILES["buridan"], 2, paused=True)
    window.controller.step_codelet()                   # to the end of the start
    qtbot.waitUntil(lambda: window.state == "paused" and window.position > 0, timeout=TIMEOUT)
    agree(window, "blockage-06", "buridan", 2)
    assert window.started == ("blockage-06", "buridan", 2)
    assert window.stats_view.text("run") == run_text("blockage-06", "buridan", 2)
    assert window.windowTitle() == "Tabletop — blockage-06, seed 2 (buridan)"
    assert window.stats_view.text("run") == window.windowTitle().split(" — ")[1]
    assert window.controls_view.run_line.text().startswith("paused: ")
    assert window.controls_view.events == len(window.history)
    window.stop()
    wait_finished(qtbot, window)
    assert window.controls_view.run_line.text() == f"stopped: {len(window.history)} events"


# -- the shortcuts and the buttons (ANSWERS.md 5) --------------------------------------------------------

def test_the_shortcuts_drive_the_controller(window, qtbot):
    window.set_next_run(problem=QUICK[0], seed=QUICK[1])
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        key(qtbot, window, Qt.Key.Key_Right)                   # Step: a run, paused at 0
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    assert window.position == 0 and window.started == (QUICK[0], "standard", QUICK[1])
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        key(qtbot, window, Qt.Key.Key_Right)
    assert window.position == 1
    key(qtbot, window, Qt.Key.Key_Right, Qt.KeyboardModifier.ShiftModifier)   # Step codelet
    qtbot.waitUntil(lambda: window.state == "paused" and window.position > 1,
                    timeout=TIMEOUT)
    assert window.controls_view.run_line.text().startswith("paused: ")
    window.set_delay(0.005)
    key(qtbot, window, Qt.Key.Key_Space)                       # Play
    qtbot.waitUntil(lambda: window.controller.state == "running", timeout=TIMEOUT)
    assert window.controls_view.pause_button.isEnabled()
    key(qtbot, window, Qt.Key.Key_Space)                       # Pause
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    key(qtbot, window, Qt.Key.Key_Escape)                      # Stop
    wait_finished(qtbot, window)
    assert window.outcome.stopped
    assert window.stats_view.text("outcome").startswith("stopped after ")
    end = window.position
    key(qtbot, window, Qt.Key.Key_Left)                        # Back, not live
    assert window.position == end - 1
    key(qtbot, window, Qt.Key.Key_Right)                       # forward, not live
    assert window.position == end
    key(qtbot, window, Qt.Key.Key_N, Qt.KeyboardModifier.ControlModifier)   # New run
    qtbot.waitUntil(lambda: window.position == 0 and window.state == "paused",
                    timeout=TIMEOUT)
    assert len(window.history) == 1
    window.stop()
    wait_finished(qtbot, window)


def test_the_buttons_drive_the_controller(window, qtbot):
    c = window.controls_view
    window.set_next_run(problem=QUICK[0], seed=QUICK[1])
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        c.step_button.click()
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
    assert window.position == 0 and not c.back_button.isEnabled()
    c.step_codelet_button.click()
    qtbot.waitUntil(lambda: window.controller.state == "paused" and window.position > 0,
                    timeout=TIMEOUT)
    window.set_delay(0.005)
    c.play_button.click()
    qtbot.waitUntil(lambda: window.controller.state == "running", timeout=TIMEOUT)
    c.pause_button.click()
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
    c.stop_button.click()
    wait_finished(qtbot, window)
    assert window.outcome.stopped and c.back_button.isEnabled()
    end = window.position
    c.back_button.click()
    assert window.position == end - 1
    c.run_to_end_button.click()
    wait_finished(qtbot, window)
    assert not window.outcome.stopped and window.stats_view.text("outcome") == "answered"
    assert c.run_line.text() == f"finished: {len(window.history)} events"


# -- errors ---------------------------------------------------------------------------------------------

def test_an_invalid_seed_shows_an_error_and_refuses_the_start(window, qtbot, capfd):
    c = window.controls_view
    c.seed_edit.setText("many")
    c.seed_edit.editingFinished.emit()
    assert c.error_label.isVisible() and "seed" in c.error_label.text()
    assert window.seed == 1                           # the last valid inputs stand
    started = window.started
    c.play_button.click()
    qtbot.wait(50)
    assert not window.controller.is_alive() and window.started == started
    assert "seed" in window.statusBar().currentMessage()
    c.seed_edit.setText("4")
    c.seed_edit.editingFinished.emit()
    assert not c.error_label.isVisible() and window.seed == 4
    assert "Traceback" not in capfd.readouterr().err


def test_a_bad_problem_is_said_in_the_panel(window):
    window.problem = "no-such-problem"
    window.play()
    assert not window.controller.is_alive()
    assert "no-such-problem" in window.controls_view.error_label.text()
    window.set_next_run(problem="surround-05")
    assert window.controls_view.error_label.text() == ""


# -- the speed (Numbo GUI) -------------------------------------------------------------------------------

def test_the_speed_slider_the_menu_and_the_controller_agree(window):
    c = window.controls_view
    assert c.delay == 0.01 == window.controller.delay
    c.speed_slider.setValue(len(cm.DELAYS) - 1)
    assert window.controller.delay == 0.0 and c.speed_label.text() == "max"
    assert [a.text() for a in window.speed_actions.actions() if a.isChecked()] == ["no delay"]
    window.speed_menu.actions()[0].trigger()
    assert window.controller.delay == 1.0 and c.speed_slider.value() == 0
    assert c.speed_label.text() == "1 s"
    window.set_delay(0.2)
    assert c.delay == 0.2 and c.speed_slider.value() == cm.DELAYS.index(0.2)


# -- QSettings (G-044) ------------------------------------------------------------------------------------

def test_the_inputs_round_trip_through_a_temporary_ini(qtbot, ini, thread_errors):
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    c = w.controls_view
    c.problem_combo.setCurrentIndex(c.problem_combo.findData("buridan-03"))
    c.profile_combo.setCurrentIndex(c.profile_combo.findData("buridan"))
    c.seed_edit.setText("12")
    c.seed_edit.editingFinished.emit()
    c.speed_slider.setValue(3)
    values = c.values()
    w.close()
    s = QSettings(ini, QSettings.Format.IniFormat)
    assert s.value("controls/problem") == "buridan-03" and str(s.value("controls/seed")) == "12"
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w2.controls_view.values() == values
        agree(w2, "buridan-03", "buridan", 12)
        assert w2.controller.delay == cm.DELAYS[3]
    finally:
        w2.close()
        assert not w2.controller.is_alive()


def test_bad_settings_give_the_defaults(qtbot, ini, thread_errors, capfd):
    s = QSettings(ini, QSettings.Format.IniFormat)
    s.setValue("controls/problem", "eleventy")
    s.setValue("controls/profile", "nope")
    s.setValue("controls/seed", "many")
    s.setValue("controls/speed", 99)
    s.sync()
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w.controls_view.values() == cm.ControlValues()
        agree(w, "surround-05", "standard", 1)
        assert not w.controls_view.error_label.isVisible()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


# -- the view protocol and the minimum (G-011, G-014) -----------------------------------------------------

def test_the_minimum_and_the_run_line(window):
    c = window.controls_view
    assert (c.minimumSize().width(), c.minimumSize().height()) == cv.MIN_SIZE
    assert c.minimumSizeHint().width() <= 200
    assert not c.run_line.visibleRegion().isEmpty() and c.paints == c.run_line.paints
    assert c.run_line.height() == 16 and c.run_line.y() == 0

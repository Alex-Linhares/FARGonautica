"""Loop0004 item 14: the controls pane (`seekwhence/gui/controls.py`) in the window,
offscreen, pytest-qt (G-048, G-049; Numbo GUI, `test_gui_shell.py` and `test_polish.py`).

- The dock holds the pane: the preset list with the three appendix runs, Custom and every
  sequence of the catalogue, each with its citation and reported result under the list
  [M86 pp. 145-147]; the Presets menu has one submenu per group.
- Choosing a preset sets the window's inputs, the title and the preset's seed and trace;
  the appendix presets end `ok` on their seeds through the Play button, and one preset of
  each group runs to its end at max speed; a judged run's answers come from the
  continuation judge (DECISION D-070) and are saved in the session file.
- The speed slider sets the controller's delay over `DELAYS`; the buttons drive the
  controller (Play, Pause, Step, Step task, Step term, Run to end, Stop); the Run menu's
  shortcuts (Space, Right, Shift+Right, Ctrl+Right, Left, Esc, Ctrl+N) drive it too;
  typing in a field does not trigger them.
- The same seed repeats the run, event for event; Random gives another seed.
- An invalid text shows an error under the buttons, not a traceback, and refuses the
  start; the inputs are off while a replay is open.
- The inputs round-trip through a temporary INI; bad settings give the defaults.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings, Qt                              # noqa: E402
from PySide6.QtTest import QTest                                      # noqa: E402

from seekwhence import sequences                                      # noqa: E402
from seekwhence.gui.main_window import MainWindow                     # noqa: E402
from seekwhence.gui.models import controls as cm                      # noqa: E402
from seekwhence.gui.models import presets, session_file as sf         # noqa: E402
from seekwhence.session import OUTCOMES                               # noqa: E402
from seekwhence.trace import event_record                             # noqa: E402

from golden import APPENDIX                                           # noqa: E402

# The longest preset run through the window (fcca-9, about 5,000 events at the 120-task
# cap) takes 40 to 90 s on a loaded machine (loop0004 iteration 16).
TIMEOUT = 240000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)


def records_of(window):
    return [event_record(e) for e in window.history.events]


def wait_finished(qtbot, window):
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    qtbot.waitUntil(lambda: not window.controller.is_alive(), timeout=TIMEOUT)
    return window.outcome


def key(qtbot, w, k, modifier=Qt.KeyboardModifier.NoModifier):
    w.activateWindow()
    qtbot.waitActive(w)
    w.cytoplasm_view.setFocus()
    QTest.keyClick(w.cytoplasm_view, k, modifier)


def make_window(qtbot, settings=None, tmp_path=None):
    w = MainWindow(settings=settings, layout_path=None)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


# -- the dock and the list (G-048) ---------------------------------------------------------------

def test_the_dock_holds_the_pane_with_every_preset(window):
    c = window.controls
    assert window.controls_dock.widget() is c and c.pane_id == "P11"
    ids = [c.preset_combo.itemData(i) for i in range(c.preset_combo.count())]
    ids = [i for i in ids if i is not None]
    assert ids == [e.id for e in cm.catalogue()]
    assert ids[:4] == [RUN1, RUN2, RUN3, cm.CUSTOM] and "blackburn-12" in ids
    assert c.preset == RUN1 and c.seed_edit.text() == "54" and c.verbose_radio.isChecked()
    assert c.result_label.text() == '[M86 pp. 164-166]  reported: solved'
    assert not c.terms_edit.isEnabled()
    assert c.answering == "scripted" and c.delay == cm.DEFAULT_DELAY
    assert c.speed_label.text() == "10 ms" and window.controller.delay == cm.DEFAULT_DELAY
    assert [a.text() for a in c.buttons] == ["Play", "Pause", "Step", "Step task", "Step term",
                                            "Run to end", "Stop"]
    assert not c.pause_button.isEnabled() and not c.stop_button.isEnabled()
    assert set(window.group_menus) == set(cm.GROUP_TITLES)
    blackburn = [a.text() for a in window.group_menus["blackburn"].actions()]
    assert len(blackburn) == 12 and blackburn[1] == "blackburn-2: 1 2 3 4 5 6 7"


def test_choosing_a_preset_sets_the_inputs_the_seed_and_the_trace(window):
    c = window.controls
    c.choose_preset(RUN2)
    assert window.inputs.preset == RUN2 and window.inputs.seed == 54
    assert not window.inputs.config.verbose and c.terse_radio.isChecked()
    assert "run 2" in window.windowTitle()
    window.group_menus["blackburn"].actions()[1].trigger()
    assert c.preset == "blackburn-2" and window.inputs.preset == "blackburn-2"
    assert c.seed_edit.text() == "1" and window.inputs.seed == 1
    assert c.result_label.text() == '[M86 pp. 6-7, 145]  reported: solved'
    assert window.inputs.lines == tuple(str(n) for n in range(1, 17))
    assert window.inputs.continuation is None and window.inputs.answers == ()
    c.answering_combo.setCurrentIndex(c.answering_combo.findData("judged"))
    assert window.inputs.continuation == tuple(sequences.find("blackburn-2").terms)
    c.choose_preset(cm.CUSTOM)
    assert c.terms_edit.isEnabled()
    c.terms_edit.setText("3 7 3 7")
    c.terms_edit.editingFinished.emit()
    assert window.inputs.preset == cm.CUSTOM and window.inputs.lines == ("3", "7", "3", "7")
    assert window.windowTitle().endswith("custom: 3 7 3 7")
    # a preset action of item 02 shows in the pane
    window.set_inputs(presets.appendix_inputs(RUN3, seed=7))
    assert c.preset == RUN3 and c.seed_edit.text() == "7" and c.answering == "scripted"
    window.choose_interactive()
    assert c.preset == cm.CUSTOM and c.answering == "interactive"


# -- runs through the buttons (G-048, G-049) ----------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_the_appendix_presets_end_ok_through_play(window, qtbot, appendix_runs, run_id):
    c = window.controls
    c.choose_preset(run_id)
    c.speed_slider.setValue(len(cm.DELAYS) - 1)          # max
    assert window.controller.delay == 0
    c.play_button.click()
    outcome = wait_finished(qtbot, window)
    assert outcome.result["outcome"] == "ok" and window.inputs.seed == SEEDS[run_id]
    assert records_of(window) == appendix_runs[run_id].records


@pytest.mark.parametrize("preset", ["blackburn-2", pytest.param("p146-2", marks=pytest.mark.slow),
                                    "p88-5", "p38-1", "p150-3",
                                    pytest.param("fcca-9", marks=pytest.mark.slow)])
def test_one_preset_of_each_group_runs_to_its_end(window, qtbot, preset):
    c = window.controls
    c.choose_preset(preset)
    c.cap_term_edit.setText("120")
    c.cap_term_edit.editingFinished.emit()
    c.speed_slider.setValue(len(cm.DELAYS) - 1)
    assert window.inputs.config.max_tasks_per_term == 120
    assert window.inputs.continuation is None            # scripted by default
    c.run_to_end_button.click()
    outcome = wait_finished(qtbot, window)
    assert outcome.error is None and outcome.result["outcome"] in OUTCOMES
    assert window.history.events[0].kind == "start" and window.position == len(window.history) - 1


def test_a_judged_run_saves_its_answers(window, qtbot, tmp_path):
    c = window.controls
    c.choose_preset("p88-5")                 # 3 7 3 7 3 7, solved [M86 p. 88]
    c.answering_combo.setCurrentIndex(c.answering_combo.findData("judged"))
    c.speed_slider.setValue(len(cm.DELAYS) - 1)
    c.run_to_end_button.click()
    outcome = wait_finished(qtbot, window)
    assert outcome.result["outcome"] == "ok"
    answered = [e.answer for e in window.history.events if e.kind == "answered"]
    assert answered and answered[-1] == "ok"
    line = window.run_line()
    assert line.mode == "judged" and line.answers == tuple(answered)
    path = tmp_path / "judged.jsonl"
    assert window.save_session(path)
    assert sf.read_session(str(path)).run.mode == "judged"
    # replays as a scripted run with the answers given
    assert window.open_session(path)
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.run_to_end()
    assert "verified" in window.statusBar().currentMessage()
    assert not c.preset_combo.isEnabled() and c.replaying is not None
    window.close_replay()
    assert c.preset_combo.isEnabled() and c.replaying is None


def test_the_buttons_drive_the_controller(window, qtbot):
    c = window.controls
    c.choose_preset(RUN3)
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        c.step_button.click()
    assert window.position == 0 and window.controller.state == "paused"
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)   # the queued state
    assert c.stop_button.isEnabled() and not c.pause_button.isEnabled()
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        c.step_task_button.click()
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
    assert window.history.events[window.position].kind == "chosen"
    c.step_term_button.click()
    qtbot.waitUntil(lambda: window.controller.state == "paused"
                    and window.history.events[window.position].kind == "prompt", timeout=TIMEOUT)
    c.speed_slider.setValue(cm.DELAYS.index(0.005))
    c.play_button.click()
    qtbot.waitUntil(lambda: window.state == "running", timeout=TIMEOUT)
    assert c.pause_button.isEnabled()
    c.pause_button.click()
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
    held = window.position
    qtbot.wait(100)
    assert window.position == held
    c.stop_button.click()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.outcome.stopped and not window.controller.is_alive()
    qtbot.waitUntil(lambda: window.state == "stopped", timeout=TIMEOUT)
    assert not c.stop_button.isEnabled()


def test_the_speed_slider_sets_the_delay(window):
    c = window.controls
    for i, delay in enumerate(cm.DELAYS):
        c.speed_slider.setValue(i)
        assert window.controller.delay == delay and c.speed_label.text() == cm.delay_text(delay)
    assert c.speed_label.text() == "max"
    assert c.speed_slider.focusPolicy() == Qt.FocusPolicy.NoFocus
    assert all(b.focusPolicy() == Qt.FocusPolicy.NoFocus for b in c.buttons)
    assert c.seed_edit.focusPolicy() != Qt.FocusPolicy.NoFocus


# -- the shortcuts (Numbo GUI, test_polish.py) ---------------------------------------------------

def test_the_shortcuts_drive_the_controller(window, qtbot):
    window.controls.choose_preset(RUN3)
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        key(qtbot, window, Qt.Key.Key_Right)
    # The event is drawn before the worker reports the step over; under load the state can
    # still read "running" for a moment, so wait for it rather than read it at once.
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
    assert window.position == 0
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        key(qtbot, window, Qt.Key.Key_Right)
    assert window.position == 1
    key(qtbot, window, Qt.Key.Key_Right, Qt.KeyboardModifier.ControlModifier)
    qtbot.waitUntil(lambda: window.controller.state == "paused" and window.position > 1,
                    timeout=TIMEOUT)
    assert window.history.events[window.position].kind == "chosen"
    key(qtbot, window, Qt.Key.Key_Right, Qt.KeyboardModifier.ShiftModifier)
    qtbot.waitUntil(lambda: window.controller.state == "paused"
                    and window.history.events[window.position].kind == "prompt", timeout=TIMEOUT)
    window.controls.speed_slider.setValue(cm.DELAYS.index(0.005))
    key(qtbot, window, Qt.Key.Key_Space)
    qtbot.waitUntil(lambda: window.controller.state == "running", timeout=TIMEOUT)
    key(qtbot, window, Qt.Key.Key_Space)
    qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
    key(qtbot, window, Qt.Key.Key_Escape)
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.outcome.stopped and not window.controller.is_alive()
    end = window.position
    key(qtbot, window, Qt.Key.Key_Left)
    assert window.position == end - 1
    key(qtbot, window, Qt.Key.Key_Right)         # not live: forward one event
    assert window.position == end
    with qtbot.waitSignal(window.event_drawn, timeout=TIMEOUT):
        key(qtbot, window, Qt.Key.Key_N, Qt.KeyboardModifier.ControlModifier)
    assert window.position == 0 and window.controller.state == "paused"
    window.stop()


def test_typing_in_a_field_does_not_trigger_the_shortcuts(window, qtbot):
    window.activateWindow()
    qtbot.waitActive(window)
    c = window.controls
    c.choose_preset(cm.CUSTOM)
    c.terms_edit.setFocus()
    QTest.keyClicks(c.terms_edit, "3 7 3")
    QTest.keyClick(c.terms_edit, Qt.Key.Key_Right)
    QTest.keyClick(c.terms_edit, Qt.Key.Key_Space)
    qtbot.wait(100)
    assert c.terms_edit.text() == "3 7 3 "
    assert window.controller.state == "idle" and window.position == -1


# -- the same seed repeats the run; Random (G-049) -------------------------------------------------

def test_the_same_seed_repeats_the_run_and_random_gives_another(window, qtbot):
    c = window.controls
    c.choose_preset("p88-4")
    c.seed_edit.setText("9")
    c.seed_edit.editingFinished.emit()
    c.cap_term_edit.setText("200")
    c.cap_term_edit.editingFinished.emit()
    c.speed_slider.setValue(len(cm.DELAYS) - 1)
    runs = []
    for _ in range(2):
        c.run_to_end_button.click()
        wait_finished(qtbot, window)
        runs.append((records_of(window), window.outcome.result["draws"]))
    assert runs[0] == runs[1] and window.inputs.seed == 9
    seed = c.new_random_seed()
    assert c.seed_edit.text() == str(seed) and window.inputs.seed == seed
    assert seed != 9


# -- errors and replay mode -------------------------------------------------------------------------

def test_an_invalid_input_shows_an_error_not_a_traceback(window, qtbot, capfd):
    c = window.controls
    c.seed_edit.setText("many")
    c.seed_edit.editingFinished.emit()
    assert c.error_label.isVisible() and "seed" in c.error_label.text()
    assert window.inputs.seed == 54                  # the last valid inputs stand
    c.play_button.click()
    qtbot.wait(100)
    assert window.controller.state == "idle" and window.position == -1
    assert "seed" in window.statusBar().currentMessage()
    c.seed_edit.setText("54")
    c.seed_edit.editingFinished.emit()
    assert not c.error_label.isVisible()
    c.choose_preset(cm.CUSTOM)
    c.answering_combo.setCurrentIndex(c.answering_combo.findData("judged"))
    assert c.error_label.isVisible() and "continuation judge" in c.error_label.text()
    assert "Traceback" not in capfd.readouterr().err


# -- QSettings (G-049) -------------------------------------------------------------------------------

def test_the_inputs_round_trip_through_a_temporary_ini(qtbot, ini, thread_errors):
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    c = w.controls
    c.choose_preset("blackburn-5")
    c.seed_edit.setText("12")
    c.seed_edit.editingFinished.emit()
    c.cap_term_edit.setText("500")
    c.cap_term_edit.editingFinished.emit()
    c.answering_combo.setCurrentIndex(c.answering_combo.findData("judged"))
    c.terse_radio.setChecked(True)
    c.speed_slider.setValue(3)
    values = c.values()
    w.close()
    s = QSettings(ini, QSettings.Format.IniFormat)
    assert s.value("controls/preset") == "blackburn-5" and s.value("controls/seed") == "12"
    w2 = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w2.controls.values() == values
        assert w2.inputs.preset == "blackburn-5" and w2.inputs.seed == 12
        assert w2.inputs.config.max_tasks_per_term == 500 and not w2.inputs.config.verbose
        assert w2.inputs.continuation is not None and w2.controller.delay == cm.DELAYS[3]
        assert "blackburn-5" in w2.windowTitle()
    finally:
        w2.close()
        assert not w2.controller.is_alive()


def test_bad_settings_give_the_defaults(qtbot, ini, thread_errors, capfd):
    s = QSettings(ini, QSettings.Format.IniFormat)
    s.setValue("controls/preset", "eleventy")
    s.setValue("controls/seed", "many")
    s.setValue("controls/speed", 99)
    s.setValue("controls/answering", "telepathy")
    s.sync()
    w = make_window(qtbot, QSettings(ini, QSettings.Format.IniFormat))
    try:
        assert w.controls.values() == cm.ControlValues()
        assert w.inputs.preset == presets.DEFAULT_PRESET and w.inputs.seed == 54
        assert not w.controls.error_label.isVisible()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err

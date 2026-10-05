"""The engine thread and run control in the Qt GUI (loop0003 item 05).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

- the bridge (metacat/qt/engine_bridge.py): `post` runs a callable on the GUI
  thread later, `call` runs it there and waits for its value (directly when
  already on the GUI thread, so the GUI thread never waits on itself);
- the control panel (metacat/qt/controls.py) answers gui.ss's messages: the
  run, input and disabled modes enable the widgets as gui.py's _enable_all
  does; a message from the engine thread changes no widget until the GUI
  thread runs it; Enter, Go, Step, Stop and Reset run gui.py's own button
  actions; the speed slider sets the engine's pauses as gui.py's slider does;
  the breakpoint and step interval dialogs read their input as gui.ss's
  input-dialog does; an error in the model puts the panel back in input mode;
- painting while the engine thread runs: no Python boundingRect in the scene
  items, and the views' paint pass entered from Python, so the GUI thread
  paints while another thread holds the GIL (anomalies: "Qt paints the panes
  slowly while the engine runs in another thread");
- drive_qt_gui.py (a fresh process) drives the whole window: a full run, step
  mode, a demo stopped and restarted, a breakpoint, Reset, a justify run, 50
  rapid Go/Stop toggles and run 7's timing, each trace equal to its golden.
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
import threading
import time
from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")

HERE = Path(__file__).resolve().parent
OUT = HERE / "screenshots-qt" / "engine"
SCRIPT = HERE / "drive_qt_gui.py"


def pump(qapp, cond, secs=5):
    t = time.time()
    while not cond():
        qapp.processEvents()
        time.sleep(0.002)
        if time.time() - t > secs:
            return False
    return True


# --- the bridge ------------------------------------------------------------------

def test_post_runs_on_the_gui_thread_later(qapp):
    from metacat.qt.engine_bridge import GuiInvoker
    inv = GuiInvoker()
    seen = []
    t = threading.Thread(target=lambda: inv.post(lambda: seen.append(threading.get_ident())))
    t.start()
    t.join()
    assert seen == []
    assert pump(qapp, lambda: seen)
    assert seen == [threading.get_ident()]


def test_call_waits_for_the_gui_threads_answer(qapp):
    from metacat.qt.engine_bridge import GuiInvoker
    inv = GuiInvoker()
    box = []

    def worker():
        box.append(inv.call(lambda: (threading.get_ident(), 42)))
        try:
            inv.call(lambda: 1 / 0)
        except ZeroDivisionError:
            box.append("raised")
    t = threading.Thread(target=worker)
    t.start()
    assert pump(qapp, lambda: len(box) == 2)
    t.join()
    assert box == [(threading.get_ident(), 42), "raised"]


def test_call_on_the_gui_thread_runs_directly(qapp):
    from metacat.qt.engine_bridge import GuiInvoker
    inv = GuiInvoker()
    assert inv.call(lambda: 7) == 7
    seen = []
    inv.post(lambda: seen.append(1))      # on the GUI thread too: at once
    assert seen == [1]


def test_a_call_times_out_instead_of_hanging(qapp):
    from metacat.qt.engine_bridge import GuiInvoker
    inv = GuiInvoker()
    box = []

    def worker():
        try:
            inv.call(lambda: None, timeout=0.2)
        except TimeoutError:
            box.append("timeout")
    t = threading.Thread(target=worker)
    t.start()
    t.join()                    # the GUI thread processes no events meanwhile
    assert box == ["timeout"]
    qapp.processEvents()        # the late call runs, and nobody waits for it


# --- the control panel -------------------------------------------------------------

@pytest.fixture
def panel(qapp, monkeypatch):
    from metacat import run, setup, view_globals
    from metacat.qt import controls, fonts
    fonts.install()
    for name in ("p_num_of_flashes", "p_flash_pause", "p_snag_pause", "p_text_scroll_pause",
                 "p_codelet_highlight_pause"):
        monkeypatch.setattr(view_globals, name, getattr(view_globals, name))
    monkeypatch.setattr(run, "g_interrupt_p", run.g_interrupt_p)
    monkeypatch.setattr(run, "g_break_time", run.g_break_time)
    monkeypatch.setattr(run, "p_step_cycles", run.p_step_cycles)
    cp = controls.make_control_panel()
    monkeypatch.setattr(setup, "g_control_panel", cp)
    yield cp
    from metacat.objects import tell
    for w in QtWidgets.QApplication.topLevelWidgets():
        if w.windowTitle() == "Input":
            w.close()
    tell(cp, "get-widgets")["frame"].deleteLater()
    qapp.processEvents()


def enabled(cp):
    from metacat.objects import tell
    W = tell(cp, "get-widgets")
    return tuple(W[n].isEnabled() for n in ("command-line", "step-button", "go-button",
                                             "stop-button", "reset-button")) + (
        W["options-menu"].menuAction().isEnabled(),)


def test_the_panel_starts_disabled_with_its_prompt(panel):
    from metacat.objects import tell
    W = tell(panel, "get-widgets")
    assert W["info-label"].text() == "Please enter a problem:"
    assert enabled(panel)[1:5] == (False, False, False, False)
    assert W["breakpoint-label"].text() == ""
    assert not W["self-watching-warning-label"].isVisibleTo(W["frame"])
    assert tell(panel, "object-type") == "control-panel"


def test_the_modes_enable_the_widgets_as_gui_py_does(panel):
    from metacat.objects import tell
    W = tell(panel, "get-widgets")
    tell(panel, "switch-to-input-mode")
    assert enabled(panel) == (True, True, True, False, True, True)
    assert W["command-line"].text() == ""
    tell(panel, "switch-to-run-mode")
    assert enabled(panel) == (False, False, False, True, False, False)
    assert W["command-line"].text() == "running..."
    tell(panel, "switch-to-disabled-mode")
    assert enabled(panel) == (False, False, False, False, False, False)
    tell(panel, "switch-to-input-mode")
    assert enabled(panel) == (True, True, True, False, True, True)


def test_a_message_from_another_thread_waits_for_the_gui_thread(panel, qapp):
    from metacat.objects import tell
    tell(panel, "switch-to-input-mode")
    t = threading.Thread(target=lambda: tell(panel, "switch-to-run-mode"))
    t.start()
    t.join()
    assert enabled(panel) == (True, True, True, False, True, True)
    assert pump(qapp, lambda: enabled(panel)[3])
    assert enabled(panel) == (False, False, False, True, False, False)


def test_invalid_input_shows_for_700_ms(panel, qapp):
    from metacat.objects import tell
    W = tell(panel, "get-widgets")
    tell(panel, "switch-to-input-mode")
    W["command-line"].setText("abc 12x")
    from PySide6.QtCore import Qt
    from PySide6.QtTest import QTest
    QTest.keyClick(W["command-line"], Qt.Key_Return)
    assert W["info-label"].text() == "Invalid input!"
    t = time.time()
    assert pump(qapp, lambda: W["info-label"].text() == "Please enter a problem:", 3)
    assert 0.6 < time.time() - t < 1.5


def test_the_speed_slider_sets_the_pauses_as_gui_py_does(panel):
    from metacat import view_globals
    from metacat.gui import gui
    from metacat.objects import tell
    slider = tell(panel, "get-widgets")["speed-slider"]
    assert (slider.minimum(), slider.maximum(), slider.value()) == (0, 100, gui.p_initial_speed)

    def pauses():
        return (view_globals.p_num_of_flashes, view_globals.p_flash_pause,
                view_globals.p_snag_pause, view_globals.p_text_scroll_pause)
    for value in (0, 1, 37, 50, 99, 100):
        gui.speed_slider_action(None, value)
        want = pauses()
        slider.setValue(value)
        assert pauses() == want, value
    assert pauses() == (1, 1, 1, 1)


def test_stop_sets_the_interrupt_flag(panel):
    from metacat import run
    from metacat.objects import tell
    tell(panel, "switch-to-run-mode")
    run.g_interrupt_p = False
    tell(panel, "get-widgets")["stop-button"].click()
    assert run.g_interrupt_p is True


def _dialog():
    found = [w for w in QtWidgets.QApplication.topLevelWidgets()
             if w.isVisible() and w.windowTitle() == "Input"]
    return found[0] if len(found) == 1 else None


def _type(dialog, text):
    from PySide6.QtCore import Qt
    from PySide6.QtTest import QTest
    entry = dialog.findChild(QtWidgets.QLineEdit)
    entry.setText(text)
    QTest.keyClick(entry, Qt.Key_Return)


def _trigger(cp, label):
    from metacat.objects import tell
    for action in tell(cp, "get-widgets")["options-menu"].actions():
        if action.text() == label:
            return action.trigger()
    raise KeyError(label)


def test_the_breakpoint_dialog_reads_its_input_as_gui_ss_does(panel, qapp):
    from metacat import run
    from metacat.objects import tell
    W = tell(panel, "get-widgets")
    run.g_break_time = False
    _trigger(panel, "Set breakpoint")
    dialog = _dialog()
    assert dialog is not None
    label = [w for w in dialog.findChildren(QtWidgets.QLabel)][0]
    assert label.text() == "Enter new breakpoint:"
    _trigger(panel, "Set breakpoint")          # raised again, not a second one
    assert _dialog() is dialog
    for bad in ("abc", "0", "-3"):
        _type(dialog, bad)
        assert label.text() == "Invalid input!" and _dialog() is dialog, bad
        assert run.g_break_time is False
    assert pump(qapp, lambda: label.text() == "Enter new breakpoint:", 3)
    _type(dialog, "100")
    assert run.g_break_time == 100
    assert W["breakpoint-label"].text() == "Breakpoint set for time step 100"
    assert pump(qapp, lambda: _dialog() is None)
    # the default is the current breakpoint, selected; empty input changes nothing
    _trigger(panel, "Set breakpoint")
    dialog = _dialog()
    entry = dialog.findChild(QtWidgets.QLineEdit)
    assert entry.text() == "100" and entry.selectedText() == "100"
    _type(dialog, "")
    assert pump(qapp, lambda: _dialog() is None)
    assert run.g_break_time == 100
    _trigger(panel, "Clear breakpoint")
    assert run.g_break_time is False and W["breakpoint-label"].text() == ""


def test_the_step_interval_dialog(panel, qapp):
    from metacat import run
    _trigger(panel, "Step mode interval")
    dialog = _dialog()
    entry = dialog.findChild(QtWidgets.QLineEdit)
    assert entry.text() == str(run.p_step_cycles)
    _type(dialog, "40")
    assert run.p_step_cycles == 40
    assert pump(qapp, lambda: _dialog() is None)


def test_an_engine_error_puts_the_panel_back_in_input_mode(panel, qapp, capsys):
    from metacat.gui import app as gui_app
    from metacat.objects import tell
    tell(panel, "switch-to-run-mode")
    thread = gui_app.EngineThread()

    def boom():
        raise RuntimeError("boom")
    thread.send(boom)
    assert pump(qapp, lambda: not thread.busy_p() and enabled(panel)[2])
    assert tell(panel, "get-widgets")["info-label"].text() == "Error: boom"
    assert "boom" in capsys.readouterr().err


# --- painting while another thread holds the GIL ------------------------------------

def test_scene_items_have_no_python_bounding_rect():
    """Qt asks every dirty item its boundingRect from C++; a Python override
    waits for the GIL each time (up to 5 ms while the engine runs)"""
    from metacat.qt.canvas import TkItem
    from metacat.qt.hosts import Pane, PaneView
    for klass in TkItem.__mro__:
        if klass.__module__.startswith("metacat"):
            assert "boundingRect" not in vars(klass), klass
            assert "shape" not in vars(klass), klass
    assert "paintEvent" in vars(PaneView)
    assert "paintEvent" not in vars(Pane)     # the margins are painted by Qt itself


def test_the_panes_repaint_while_another_thread_runs_python():
    """qt_paint_probe.py, in a fresh process (the suite's other windows and
    timers would share the GIL): a QGraphicsView of 600 canvas items, changed
    and synced every 50 ms (under the paint gate, as MainWindow.sync does) for
    2 s, while a thread runs Python and draws now and then, as the engine
    does.  The paint pass happens about as often as the timer fires.  With
    --without-fix (no paint gate, no Python paintEvent, a Python
    boundingRect): 2 ticks and no paint."""
    env = dict(os.environ, QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run([sys.executable, str(HERE / "qt_paint_probe.py")],
                          capture_output=True, text=True, timeout=120, env=env,
                          cwd=str(HERE.parent))
    assert proc.returncode == 0, proc.stderr[-2000:]
    got = json.loads(proc.stdout.splitlines()[-1])
    assert got["ticks"] >= 20 and got["painted"] >= 15, got


# --- the whole window, driven ---------------------------------------------------

def drive(scenarios, outdir):
    outdir.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run([sys.executable, str(SCRIPT), str(outdir), *scenarios],
                          capture_output=True, text=True, timeout=900, env=env,
                          cwd=str(HERE.parent))
    lines = proc.stdout.splitlines()
    assert proc.returncode == 0, proc.stdout[-3000:] + proc.stderr[-3000:]
    oks = [line.split()[1] for line in lines if line.startswith("ok ")]
    return oks, json.loads(lines[-1])


def test_a_full_run_in_the_qt_gui_gives_its_golden():
    oks, _ = drive(["full_run"], OUT / "fast")
    assert oks == ["start", "full-run"]


@pytest.mark.slow
def test_every_gui_scenario_in_the_qt_gui_gives_its_golden():
    oks, results = drive([], OUT / "all")
    assert oks == ["start", "full-run", "step-mode", "responsive", "demo-stop-go",
                   "breakpoint", "reset", "justify", "toggles", "timing"]
    assert results["responsive_max_s"] < 0.5
    assert results["paints_while_running"] >= 10
    assert results["run7_s"] > 0
    (OUT / "all" / "results.json").write_text(json.dumps(results, indent=1) + "\n")

"""Mouse and keyboard parity between the Qt GUI and the tkinter GUI (loop0003 item 07).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The inventory (data/tk-gui-inventory.json) lists every binding of the tkinter
GUI: <ButtonPress-1>, <Shift-ButtonPress-1> and <ButtonPress-3> on each
graphics window's canvas (gui/hosts.py's TkHost), and <Key-Return> on the
command line and the input dialog's entry.  Here:

- the Qt panes give Viewport.mouse_press the modifiers Tk's binding rules give
  (the most specific binding wins, a binding ignores the modifiers it doesn't
  name), at the press's pixel; a double click is two presses, as in Tk; other
  buttons and the wheel do nothing to the panel;
- the Qt entries take Return with any modifiers, and not the keypad's Enter;
- click_scenario.py, driven through QTest's real events in the Qt GUI
  (drive_qt_gui.py clicks), gives the same model states and the same traces
  as through Tk's events in the tkinter GUI (drive_gui.py clicks, under
  xvfb-run), whose result is data/tk-clicks.json: Enter, the Workspace click
  that resumes a run, the Trace and Memory selections and the answer
  comparison, the clicks that do nothing, and the theme-pattern clamp made by
  left, right and shift clicks on the Themes panes, with the run that follows.
"""
from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")

from PySide6.QtCore import QPoint, Qt  # noqa: E402
from PySide6.QtTest import QTest  # noqa: E402

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
REFERENCE = HERE / "data" / "tk-clicks.json"
OUT = HERE / "screenshots-qt" / "clicks"


# --- the presses ------------------------------------------------------------------

def test_presses_get_the_modifiers_of_tks_bindings():
    from metacat.qt.hosts import press_modifiers
    left, right, middle = Qt.LeftButton, Qt.RightButton, Qt.MiddleButton
    shift, ctrl, alt = Qt.ShiftModifier, Qt.ControlModifier, Qt.AltModifier
    none = Qt.NoModifier
    assert press_modifiers(left, none) == ("left-button",)
    assert press_modifiers(left, shift) == ("shift", "left-button")
    assert press_modifiers(left, ctrl) == ("left-button",)
    assert press_modifiers(left, alt) == ("left-button",)
    assert press_modifiers(left, shift | ctrl) == ("shift", "left-button")
    assert press_modifiers(right, none) == ("right-button",)
    assert press_modifiers(right, shift) == ("right-button",)
    assert press_modifiers(right, ctrl) == ("right-button",)
    assert press_modifiers(middle, none) is None
    assert press_modifiers(Qt.BackButton, none) is None


def test_the_inventory_has_exactly_these_bindings():
    inventory = json.loads((HERE / "data" / "tk-gui-inventory.json").read_text())
    canvas = {b["sequence"] for b in inventory["bindings"] if b["class"] == "Canvas"}
    assert canvas == {"<Button-1>", "<Shift-Button-1>", "<Button-3>", "<Configure>"}
    keys = {(b["class"], b["sequence"]) for b in inventory["bindings"]
            if b["class"] != "Canvas"}
    assert keys == {("Entry", "<Key-Return>")}


class RecordingViewport:
    def __init__(self):
        self.presses = []

    def configure(self, w, h):
        pass

    def mouse_press(self, i, j, mods):
        self.presses.append((i, j, tuple(mods)))


_HOSTS = {}


def shown_host(qapp, scrolling="none", w=300, h=200):
    """one host per kind, kept for the whole session: collecting fake hosts
    one after another crashed the cycle collector (anomalies: "Collecting
    QtHosts one after another can crash")"""
    from metacat.gui.colors import c_white
    from metacat.qt.hosts import QtHost
    if scrolling not in _HOSTS:
        host = QtHost(scrolling, lambda top: None)
        host.make_canvas(w, h, c_white)
        host.show_viewport(RecordingViewport())
        _HOSTS[scrolling] = host
    host = _HOSTS[scrolling]
    vp = host.viewport = RecordingViewport()
    host.pane.resize(w, h)
    host.pane.show()
    qapp.processEvents()
    return host, vp


def window_click(widget, button, modifiers, at, double=False):
    """a press as the platform delivers it: through the window"""
    handle = widget.window().windowHandle()
    pos = widget.mapTo(widget.window(), at)
    if double:
        QTest.mouseDClick(handle, button, modifiers, pos)
    else:
        QTest.mouseClick(handle, button, modifiers, pos)


def test_a_pane_hands_its_presses_to_the_viewport(qapp):
    host, vp = shown_host(qapp)
    viewport = host.pane.view.viewport()
    window_click(viewport, Qt.LeftButton, Qt.NoModifier, QPoint(10, 12))
    window_click(viewport, Qt.LeftButton, Qt.ShiftModifier, QPoint(20, 22))
    window_click(viewport, Qt.LeftButton, Qt.ControlModifier, QPoint(30, 32))
    window_click(viewport, Qt.RightButton, Qt.NoModifier, QPoint(40, 42))
    window_click(viewport, Qt.RightButton, Qt.ShiftModifier, QPoint(50, 52))
    window_click(viewport, Qt.MiddleButton, Qt.NoModifier, QPoint(60, 62))
    assert vp.presses == [(10, 12, ("left-button",)),
                          (20, 22, ("shift", "left-button")),
                          (30, 32, ("left-button",)),
                          (40, 42, ("right-button",)),
                          (50, 52, ("right-button",))]


def test_a_double_click_is_two_presses_as_in_tk(qapp):
    host, vp = shown_host(qapp)
    window_click(host.pane.view.viewport(), Qt.LeftButton, Qt.NoModifier, QPoint(7, 9),
                 double=True)
    assert vp.presses == [(7, 9, ("left-button",))] * 2


def test_the_press_is_in_view_pixels_and_canvasx_adds_the_scroll(qapp):
    """Tk's event x is the widget's; the panels add canvasx's scroll offset"""
    host, vp = shown_host(qapp, "horizontal", 300, 100)
    host.set_scroll_region_bang(0, 0, 3000, 100)
    host.sync()
    qapp.processEvents()
    host.pane.view.horizontalScrollBar().setValue(700)
    qapp.processEvents()
    window_click(host.pane.view.viewport(), Qt.LeftButton, Qt.NoModifier, QPoint(10, 20))
    assert vp.presses == [(10, 20, ("left-button",))]
    assert float(str(host.canvas.tcl("canvasx", 10))) == 710


def test_a_letterboxed_pane_counts_from_the_views_corner(qapp):
    host, vp = shown_host(qapp, "none", 300, 200)
    host.pane.resize(600, 200)          # wider than the ratio: margins left and right
    qapp.processEvents()
    view = host.pane.view
    assert view.x() > 0
    window_click(view.viewport(), Qt.LeftButton, Qt.NoModifier, QPoint(5, 6))
    assert vp.presses == [(5, 6, ("left-button",))]
    # a press in the margin is outside the canvas: Tk had no margin, nothing happens
    window_click(host.pane, Qt.LeftButton, Qt.NoModifier, QPoint(2, 100))
    assert len(vp.presses) == 1


def test_a_handler_error_is_reported_and_the_gui_goes_on(qapp, capsys):
    host, vp = shown_host(qapp)

    def broken(i, j, mods):
        raise RuntimeError("handler broke")
    vp.mouse_press = broken
    window_click(host.pane.view.viewport(), Qt.LeftButton, Qt.NoModifier, QPoint(5, 5))
    assert "handler broke" in capsys.readouterr().err


def test_the_wheel_does_not_scroll_the_canvas(qapp):
    """Tk's canvas has no wheel binding (TkHost adds none)"""
    host, vp = shown_host(qapp, "vertical", 300, 100)
    host.set_scroll_region_bang(0, 0, 300, 1000)
    host.sync()
    qapp.processEvents()
    bar = host.pane.view.verticalScrollBar()
    bar.setValue(100)
    viewport = host.pane.view.viewport()
    from PySide6.QtCore import QPointF
    from PySide6.QtGui import QWheelEvent
    event = QWheelEvent(QPointF(10, 10), QPointF(viewport.mapToGlobal(QPoint(10, 10))),
                        QPoint(0, 0), QPoint(0, -120), Qt.NoButton, Qt.NoModifier,
                        Qt.NoScrollPhase, False)
    QtWidgets.QApplication.sendEvent(viewport, event)
    qapp.processEvents()
    assert bar.value() == 100 and vp.presses == []


# --- the keys ------------------------------------------------------------------------

def test_entries_take_return_with_any_modifiers_but_not_the_keypads_enter(qapp):
    from metacat.qt.controls import bind_return
    entry = QtWidgets.QLineEdit()
    calls = []
    bind_return(entry, lambda: calls.append(entry.text()))
    entry.show()
    QTest.keyClicks(entry, "abc abd")
    QTest.keyClick(entry, Qt.Key_Return)
    assert calls == ["abc abd"]
    QTest.keyClick(entry, Qt.Key_Return, Qt.ShiftModifier)
    QTest.keyClick(entry, Qt.Key_Return, Qt.ControlModifier)
    assert len(calls) == 3
    QTest.keyClick(entry, Qt.Key_Enter, Qt.KeypadModifier)
    assert len(calls) == 3
    QTest.keyClicks(entry, " xyz")
    assert entry.text() == "abc abd xyz"
    entry.close()


def test_the_command_line_and_the_input_dialog_use_it():
    source = (HERE.parent / "metacat" / "qt" / "controls.py").read_text()
    assert "returnPressed.connect" not in source
    assert source.count("bind_return(") == 3    # the definition and the two entries


# --- the scenario, Qt against tkinter --------------------------------------------------

def drive_qt(outdir):
    outdir.mkdir(parents=True, exist_ok=True)
    env = dict(os.environ, QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run([sys.executable, str(HERE / "drive_qt_gui.py"), str(outdir),
                           "clicks"], capture_output=True, text=True, timeout=600,
                          env=env, cwd=str(HERE.parent))
    assert proc.returncode == 0, proc.stdout[-3000:] + proc.stderr[-3000:]
    return json.loads((outdir / "clicks.json").read_text())


def drive_tk(outdir):
    outdir.mkdir(parents=True, exist_ok=True)
    proc = subprocess.run(
        ["xvfb-run", "-a", "-s", "-screen 0 2560x1600x24", sys.executable,
         str(HERE / "drive_gui.py"), str(outdir), "invalid_input", "clicks"],
        capture_output=True, text=True, timeout=600, cwd=str(HERE.parent),
        env={k: v for k, v in os.environ.items() if k != "WAYLAND_DISPLAY"})
    assert proc.returncode == 0, proc.stdout[-3000:] + proc.stderr[-3000:]
    return json.loads((outdir / "clicks.json").read_text())


def reference(result):
    import click_scenario
    out = click_scenario.comparable(result)
    out["generated_by"] = ("python/tests/drive_gui.py OUT invalid_input clicks "
                           "(the tkinter GUI, under xvfb-run)")
    return out


def assert_same(got, want):
    for (name, a), (name2, b) in zip(got["steps"], want["steps"]):
        assert name == name2
        assert a == b, "%s:\n Qt %s\n Tk %s" % (name, json.dumps(a), json.dumps(b))
    assert len(got["steps"]) == len(want["steps"])
    assert got["traces"] == want["traces"]


@pytest.mark.slow
def test_the_clicks_in_the_qt_gui_do_what_they_do_in_the_tkinter_gui():
    result = drive_qt(OUT)
    got = reference(result)
    want = json.loads(REFERENCE.read_text())
    assert_same(got, want)
    steps = dict((name, snap) for name, snap in got["steps"])
    assert steps["keypad Enter on the command line"] == "Please enter a problem:"
    assert steps["at the breakpoint"]["t"] == 100
    assert steps["workspace left click resumes"]["t"] == 395
    assert steps["memory left click on a second answer compares them"]["highlighted_answers"]
    assert steps["Clamp Themes"]["last_clamp"][0] == "manual-clamp"
    assert steps["the run goes on with the clamp"]["t"] == steps["Clamp Themes"]["t"] + 150
    # every press of the scenario went to a pane that was on screen (the EEG is hidden)
    assert set(result["pixels"]) == {"workspace", "slipnet", "coderack", "temperature",
                                     "trace", "memory", "commentary", "eeg", "top",
                                     "vertical", "bottom"}


@pytest.mark.slow
def test_the_tkinter_reference_is_up_to_date(tmp_path):
    assert reference(drive_tk(tmp_path)) == json.loads(REFERENCE.read_text())

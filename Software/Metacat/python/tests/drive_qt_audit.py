"""Drive the Qt GUI for the final audit, offscreen (loop0003 item 11).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  QT_QPA_PLATFORM=offscreen python3 python/tests/drive_qt_audit.py OUTDIR

The window is drive_qt_menus.py's (imported: `python3 -m metacat.qt`'s window
on a screen of the inventory's size).  This driver reads the parts of
python/tests/data/tk-gui-inventory.json that the item 05-08 drivers don't
compare entry by entry, as tk_gui_inventory.py read them from the tkinter GUI:

- every graphics window: its title, panel class, drawing module, default and
  canvas sizes, scrolling and scroll bars, aspect, resize method, visibility at
  start, press handlers and background, plus the pane and the View item that
  hold it; the Logo as the window icon;
- the speed slider: its range, initial value, labels, and the speed settings
  each value of the inventory's "sets" gives;
- both input dialogs (Set breakpoint, Step mode interval): title, place,
  message, Enter on bad input, on an empty field and on a number, and that a
  second trigger raises the open dialog instead of making another;
- the Step, Go, Stop and Reset buttons' actions, and what closing the window
  does.

It writes OUTDIR/audit.json, which test_qt_audit.py compares with the
inventory, and exits 0 when every step ran (a watchdog ends it after 5 min).
"""
from __future__ import annotations

import json
import os
import sys
import threading
import time
import traceback
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import drive_qt_menus as M  # noqa: E402  (makes the window)

from PySide6.QtCore import Qt  # noqa: E402
from PySide6.QtTest import QTest  # noqa: E402
from PySide6.QtWidgets import QApplication, QLabel, QLineEdit, QSplitter  # noqa: E402

from metacat import run, setup, view_globals  # noqa: E402
from metacat.objects import tell  # noqa: E402

OUT = M.OUT
REPORT = {}
STATUS = [1]
WINDOW_GLOBALS = [
    ("Workspace", "g_workspace_window"), ("Slipnet", "g_slipnet_window"),
    ("Coderack", "g_coderack_window"), ("Temperature", "g_temperature_window"),
    ("Temporal Trace", "g_trace_window"), ("Commentary", "g_comment_window"),
    ("Episodic Memory", "g_memory_window"), ("Top Themes", "g_top_themes_window"),
    ("Bottom Themes", "g_bottom_themes_window"),
    ("Vertical Themes", "g_vertical_themes_window"), ("EEG", "g_EEG_window")]
SPEED_NAMES = ["p_codelet_highlight_pause", "p_flash_pause", "p_num_of_flashes",
               "p_snag_pause", "p_text_scroll_pause"]


# --- tk_gui_inventory.py's readers, for the same panel objects --------------------------

def fn_name(f):
    if f is None or f is False:
        return None
    return "%s.%s" % (getattr(f, "__module__", "?"), getattr(f, "__qualname__", repr(f)))


def closure(f):
    code = getattr(f, "__code__", None)
    if code is None or not f.__closure__:
        return {}
    out = {}
    for name, cell in zip(code.co_freevars, f.__closure__):
        try:
            out[name] = cell.cell_contents
        except ValueError:
            pass
    return out


def graphics_window(window):
    seen = set()
    w = window
    while id(w) not in seen:
        seen.add(id(w))
        if type(w).__name__ == "GraphicsWindow":
            return w
        w = getattr(w, "graphics_window", None) or getattr(w, "text_window", None)
        if w is None:
            break
    raise TypeError("no graphics window in %r" % (window,))


def resize_method(panel):
    w = panel
    while w is not None:
        if "resize" in type(w).__dict__:
            return fn_name(type(w).__dict__["resize"])
        if type(w).__name__ == "GraphicsWindow":
            return None
        w = getattr(w, "graphics_window", None) or getattr(w, "text_window", None)
    return None


def plain(x):
    if isinstance(x, (list, tuple)):
        return [plain(a) for a in x]
    if isinstance(x, (bool, int, float)) or x is None:
        return x
    return str(x)


def press_name(vp, attr):
    h = getattr(vp, attr)
    name = fn_name(h)
    if name == "metacat.gui.sgl.nop_event_handler":
        return None
    cl = closure(h)
    if "theme_type" in cl and name:
        name += "(%s)" % cl["theme_type"]
    return name


# --- the scenarios -------------------------------------------------------------------

def windows():
    controllers = {tell(c, "get-text"): c
                   for c in tell(setup.g_control_panel, "get-widgets")["window-controllers"]}

    def read():
        out = []
        by_pane = {id(p): n for n, p in M.WINDOW.panes.items()}
        for name, g in WINDOW_GLOBALS:
            panel = getattr(setup, g)
            gw = graphics_window(panel)
            host = gw.top
            view = host.pane.view
            bars = []
            if view.horizontalScrollBarPolicy() == Qt.ScrollBarAlwaysOn:
                bars.append("horizontal")
            if view.verticalScrollBarPolicy() == Qt.ScrollBarAlwaysOn:
                bars.append("vertical")
            vp = gw.vp
            out.append({
                "name": name, "global": g, "title": host.get_title(),
                "panel_class": "%s.%s" % (type(panel).__module__, type(panel).__name__),
                "draws_in": type(panel).__module__,
                "default_size": plain([gw.visible_w, gw.visible_h]),
                "canvas_size": plain([gw.canvas_w, gw.canvas_h]),
                "scrolling": gw.scrolling, "scrollbars": bars,
                "aspect": [host.aspect.numerator, host.aspect.denominator]
                if gw.scrolling == "none" else None,
                "resize_method": resize_method(panel),
                "visible_at_start": host.pane.isVisibleTo(M.WINDOW),
                "left_press": press_name(vp, "left_press_handler"),
                "right_press": press_name(vp, "right_press_handler"),
                "shift_left_press": press_name(vp, "right_press_handler"),
                "background": plain(host.canvas.get_background_color()),
                "pane": by_pane.get(id(host.pane)),
                "in_splitter": isinstance(host.pane.parentWidget(), QSplitter),
                "view_item": name in controllers,
            })
        return out
    REPORT["windows"] = M.on_main(read)
    REPORT["icon"] = M.on_main(lambda: {
        "window": not M.WINDOW.windowIcon().isNull(),
        "sizes": sorted(s.width() for s in M.WINDOW.windowIcon().availableSizes())})
    print("ok windows", flush=True)


def speed_slider():
    slider = M.W["speed-slider"]

    def read():
        labels = [lb.text() for lb in M.W["frame"].findChildren(QLabel)]
        return {"from": slider.minimum(), "to": slider.maximum(), "value": slider.value(),
                "labels": [t for t in ("Speed", "Slow", "Fast") if t in labels]}
    out = M.on_main(read)
    out["start"] = {n: getattr(view_globals, n) for n in SPEED_NAMES}
    sets = {}
    for value in M.INVENTORY["speed_slider"]["sets"]:
        M.on_main(slider.setValue, int(value))
        sets[value] = {n: getattr(view_globals, n) for n in SPEED_NAMES}
    M.on_main(slider.setValue, 100)
    out["sets"] = sets
    REPORT["speed_slider"] = out
    print("ok speed-slider", flush=True)


def input_dialog(path, variable, message):
    """Enter on bad input, an empty field, a number; one dialog at a time"""
    module, attr = variable
    before = getattr(module, attr)
    out = {}
    M.trigger(path)
    (dialog,) = M.dialogs("Input")
    field = M.on_main(lambda: dialog.findChildren(QLineEdit)[0])
    out["default"] = M.on_main(field.text)
    out["message"] = M.labels(dialog)
    frame = M.W["frame"]
    out["offset"] = M.on_main(lambda: [dialog.x() - frame.mapToGlobal(frame.rect().topLeft()).x(),
                                       dialog.y() - frame.mapToGlobal(frame.rect().topLeft()).y()])
    M.trigger(path)
    out["dialogs_after_second_trigger"] = len(M.dialogs("Input"))

    def enter(text):
        def do():
            field.setText(text)
            QTest.keyClick(field, Qt.Key_Return)
        M.on_main(do)
        M.settle()
    bad = []
    for text in ("x", "0", "-3"):
        enter(text)
        bad.append({"text": text, "message": M.labels(dialog),
                    "colour": M.on_main(lambda: dialog.findChildren(QLabel)[0].styleSheet()),
                    "open": len(M.dialogs("Input")), "value": getattr(module, attr)})
        time.sleep(0.8)
    out["bad"] = bad
    out["message_after_700ms"] = M.labels(dialog)
    enter("")
    out["empty"] = {"open": len(M.dialogs("Input")), "value_unchanged": getattr(module, attr) == before}
    M.trigger(path)
    (dialog,) = M.dialogs("Input")
    field = M.on_main(lambda: dialog.findChildren(QLineEdit)[0])
    enter("123")
    out["number"] = {"open": len(M.dialogs("Input")), "value": getattr(module, attr)}
    out["breakpoint_label"] = M.on_main(M.W["breakpoint-label"].text)
    setattr(module, attr, before)
    tell(setup.g_control_panel, "clear-breakpoint-message")
    M.settle()
    return out


def input_dialogs():
    REPORT["dialogs"] = {
        "Options > Set breakpoint": input_dialog("Options > Set breakpoint",
                                                 (run, "g_break_time"), "Enter new breakpoint:"),
        "Options > Step mode interval": input_dialog("Options > Step mode interval",
                                                     (run, "p_step_cycles"),
                                                     "Enter new step interval:")}
    print("ok input-dialogs", flush=True)


def buttons_and_close():
    """each button runs gui.py's action, Enter is Go in input mode, and closing
    the window ends the application"""
    cp = setup.g_control_panel
    REPORT["buttons"] = {}
    for label, key in (("Step", "step-button"), ("Go", "go-button"), ("Stop", "stop-button"),
                       ("Reset", "reset-button")):
        b = M.W[key]
        REPORT["buttons"][label] = M.on_main(lambda: {"text": b.text(),
                                                      "action": fn_name(b.action)})
    REPORT["command_line_action"] = fn_name(cp.command_line_action)
    REPORT["quit_on_last_window_closed"] = M.on_main(QApplication.quitOnLastWindowClosed)
    print("ok buttons", flush=True)


SCENARIOS = [windows, speed_slider, input_dialogs, buttons_and_close]


def driver():
    try:
        time.sleep(0.3)
        for scenario in SCENARIOS:
            scenario()
        STATUS[0] = 0
    except BaseException:   # noqa: BLE001
        traceback.print_exc()
        sys.stdout.flush()
    finally:
        (OUT / "audit.json").write_text(json.dumps(REPORT, indent=1, default=str) + "\n")
        print("done", flush=True)
        M.BRIDGE.invoker.post(M.QAPP.quit)


def watchdog():
    time.sleep(300)
    print("watchdog: the driver did not finish", flush=True)
    os._exit(2)


if __name__ == "__main__":
    threading.Thread(target=watchdog, daemon=True).start()
    threading.Thread(target=driver, daemon=True).start()
    M.QAPP.exec()
    sys.stdout.flush()
    os._exit(STATUS[0])

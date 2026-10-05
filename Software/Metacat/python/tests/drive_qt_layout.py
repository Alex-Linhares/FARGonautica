"""Drive the Qt main window's layout, offscreen (loop0003 item 08).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  python3 python/tests/drive_qt_layout.py OUTDIR SCENARIO --screen WxH[@DPR] [--settings INI]

The window is opened as `python3 -m metacat.qt` opens it (metacat.qt.app:
make_application and open_window, with the QSettings file INI, or none), on an
offscreen screen of WxH logical pixels at device pixel ratio DPR (Qt's
offscreen plugin reads its screens from a JSON file).  The GUI thread runs
QApplication.exec(); a driver thread acts through the bridge's blocking call
onto the GUI thread, as a user's events would arrive.  Scenarios:

  save      drag two splitter handles, wait for the delayed save, hide the
            Slipnet and show the EEG from the View menu, close the window
  restore   report what was restored, then View > Reset layout, then grow
            the window (the default sizes follow it again), close
  minimum   the window's minimum size, and the window shrunk below it
  start     the start screen, grabbed (high-DPI: DPR 2)
  run       View > Show all panes, then `abc abd ijk abd 1` (a justify run,
            so the Bottom Themes draw too) typed into the command line, Go,
            to the answer; every pane grabbed
  run7      the same in the default layout with Run 7, `abc abd xyz
            3852097033` (the README's screenshots, loop0003 item 09)

The last line of stdout is a JSON object with the measurements; screenshots go
to OUTDIR.  The script exits by itself (a watchdog ends it after 5 minutes).
"""
from __future__ import annotations

import argparse
import json
import os
import sys
import threading
import time
import traceback
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent))
GOLDEN = HERE.parent.parent / "tests" / "golden"

parser = argparse.ArgumentParser()
parser.add_argument("outdir")
parser.add_argument("scenario")
parser.add_argument("--screen", default="1920x1080")
parser.add_argument("--settings", default=None)
ARGS = parser.parse_args()
OUT = Path(ARGS.outdir)
OUT.mkdir(parents=True, exist_ok=True)
size, _, dpr = ARGS.screen.partition("@")
SW, SH = (int(v) for v in size.split("x"))
DPR = float(dpr or 1)
# high DPI: a screen of WxH times DPR device pixels, scaled by Qt (the
# offscreen plugin's own "dpr" key changes the screen but not its windows)
_screens = OUT / ("screens-%s-%s.json" % (ARGS.scenario, ARGS.screen))
_screens.write_text(json.dumps({"synchronousWindowSystemEvents": False, "windowFrameMargins": False,
                                "screens": [{"name": "offscreen", "x": 0, "y": 0,
                                             "width": round(SW * DPR), "height": round(SH * DPR),
                                             "logicalDpi": 96, "logicalBaseDpi": 96, "dpr": 1}]}))
if DPR != 1:
    os.environ["QT_SCALE_FACTOR"] = "%g" % DPR
os.environ["QT_QPA_PLATFORM"] = "offscreen:configfile=%s" % _screens
os.environ.pop("WAYLAND_DISPLAY", None)

from PySide6.QtCore import QEvent, QPoint, QPointF, QSettings, Qt  # noqa: E402
from PySide6.QtGui import QMouseEvent  # noqa: E402
from PySide6.QtTest import QTest  # noqa: E402
from PySide6.QtWidgets import QApplication  # noqa: E402

from metacat import chez, setup  # noqa: E402
from metacat.objects import tell  # noqa: E402
from metacat.qt import app as qt_app  # noqa: E402
from metacat.qt.grab import grab_png  # noqa: E402
from metacat.qt.mainwindow import PANES, default_sizes  # noqa: E402

RESULT = {}
STATUS = [1]

try:
    QAPP = qt_app.make_application()
    SETTINGS = (QSettings(ARGS.settings, QSettings.IniFormat) if ARGS.settings else None)
    WINDOW = qt_app.open_window(SETTINGS)
    BRIDGE = WINDOW.bridge
    CP = setup.g_control_panel
    W = tell(CP, "get-widgets")
except BaseException:   # noqa: BLE001 - the resize listener would keep the process up
    traceback.print_exc()
    sys.stdout.flush()
    os._exit(1)


def on_main(fn, *args):
    return BRIDGE.invoker.call(lambda: fn(*args), timeout=60)


def wait_for(cond, what, secs=10):
    t = time.time()
    while not cond():
        time.sleep(0.02)
        if time.time() - t > secs:
            raise AssertionError(what)


def sizes():
    return on_main(WINDOW.splitter_sizes)


def defaults():
    def do():
        c = WINDOW.centralWidget()
        return default_sizes(c.width(), c.height(), eeg=not WINDOW.panes["EEG"].isHidden())
    return on_main(do)


def hidden():
    return on_main(lambda: [n for n in PANES if WINDOW.panes[n].isHidden()])


def geometry():
    return on_main(lambda: [WINDOW.width(), WINDOW.height()])


def ini_keys():
    if not ARGS.settings:
        return []
    return sorted(QSettings(ARGS.settings, QSettings.IniFormat).allKeys())


def view(label):
    def do():
        for action in W["view-menu"].actions():
            if action.text() == label:
                action.trigger()
                return
        raise KeyError(label)
    on_main(do)


def view_checked():
    return on_main(lambda: {c.name: c.menu_item.isChecked() for c in CP.window_controllers})


def drag(splitter, index, delta):
    """a press on the handle, a move by delta pixels with the left button
    held, and a release (QSplitterHandle follows the global position)"""
    def do():
        sp = WINDOW.splitters[splitter]
        handle = sp.handle(index)
        local = QPointF(handle.rect().center())
        d = QPointF(delta, 0) if sp.orientation() == Qt.Horizontal else QPointF(0, delta)

        origin = QPointF(handle.mapToGlobal(QPoint(0, 0)))     # before the handle moves

        def send(kind, pos, button, buttons):
            QApplication.sendEvent(handle, QMouseEvent(kind, pos, origin + pos, button, buttons,
                                                       Qt.NoModifier))
        send(QEvent.MouseButtonPress, local, Qt.LeftButton, Qt.LeftButton)
        for k in range(1, 5):
            send(QEvent.MouseMove, local + d * (k / 4), Qt.NoButton, Qt.LeftButton)
        send(QEvent.MouseButtonRelease, local + d, Qt.LeftButton, Qt.NoButton)
    on_main(do)


def settle(secs=0.5):
    time.sleep(secs)
    on_main(WINDOW.sync)
    on_main(QAPP.processEvents)


def common():
    def do():
        screen = WINDOW.screen()
        avail = screen.availableGeometry()
        return {"screen": [avail.width(), avail.height()], "dpr": WINDOW.devicePixelRatioF(),
                "window": [WINDOW.width(), WINDOW.height()],
                "maximized": WINDOW.isMaximized(),
                "minimum": [WINDOW.minimumWidth(), WINDOW.minimumHeight()],
                "icon_sizes": sorted({(s.width(), s.height())
                                      for s in WINDOW.windowIcon().availableSizes()}),
                "app_icon": not QApplication.windowIcon().isNull()}
    RESULT.update(on_main(do))


def pane_report(tag):
    from PySide6.QtGui import QImage
    panes = {}
    for name in PANES:
        def info(name=name):
            pane = WINDOW.panes[name]
            host = pane.host
            return {"visible": pane.isVisibleTo(WINDOW),
                    "size": [pane.width(), pane.height()],
                    "minimum": [pane.minimumWidth(), pane.minimumHeight()],
                    "items": len(host.canvas.display_list.items),
                    "scene_items": len(host.canvas.scene.items())}
        p = on_main(info)
        if p["visible"]:
            path = OUT / ("%s-%s.png" % (name, tag))
            on_main(lambda name=name, path=path: grab_png(WINDOW.panes[name], path))
            image = QImage(str(path))
            colours = set()
            step = max(1, image.width() * image.height() // 20000)
            for k in range(0, image.width() * image.height(), step):
                colours.add(image.pixel(k % image.width(), k // image.width()) & 0xFFFFFF)
            p["colours"] = len(colours)
            p["image"] = [image.width(), image.height()]
        panes[name] = p
    return panes


def scenario_save():
    common()
    RESULT["initial"] = sizes()
    RESULT["initial_default"] = defaults()
    drag("top", 2, -120)          # the handle between the Workspace and the Coderack
    drag("rows", 1, 40)           # the handle between the top and middle rows
    RESULT["dragged"] = sizes()
    RESULT["saved_at_once"] = "layout/top" in ini_keys()
    time.sleep(1.0)
    RESULT["saved_after_delay"] = "layout/top" in ini_keys()
    view("Slipnet")
    view("EEG")
    settle()
    RESULT["hidden"] = hidden()
    RESULT["at_close"] = sizes()
    RESULT["geometry"] = geometry()
    on_main(WINDOW.close)
    RESULT["ini"] = ini_keys()


def scenario_restore():
    common()
    RESULT["sizes"] = sizes()
    RESULT["default"] = defaults()
    RESULT["hidden"] = hidden()
    RESULT["view_checked"] = view_checked()
    RESULT["geometry"] = geometry()
    view("Reset layout")
    settle()
    RESULT["reset_sizes"] = sizes()
    RESULT["reset_default"] = defaults()
    RESULT["reset_hidden"] = hidden()
    RESULT["reset_view_checked"] = view_checked()
    RESULT["ini_after_reset"] = ini_keys()
    on_main(lambda: WINDOW.resize(WINDOW.width() + 200, WINDOW.height() - 60))
    settle()
    RESULT["grown_sizes"] = sizes()
    RESULT["grown_default"] = defaults()
    on_main(WINDOW.close)
    RESULT["ini_after_close"] = ini_keys()


def scenario_minimum():
    common()
    on_main(lambda: WINDOW.resize(1000, 500))
    settle()
    from metacat.qt import hosts
    on_main(lambda: hosts.settle_resizes(QAPP.processEvents, timeout=30))
    settle()
    RESULT["shrunk"] = geometry()

    def strip():
        frame = W["frame"]
        return [frame.width(), frame.sizeHint().width(), frame.minimumSizeHint().width()]
    RESULT["strip"] = on_main(strip)
    RESULT["panes"] = pane_report("minimum")
    on_main(lambda: grab_png(WINDOW, OUT / "window-minimum.png"))


def scenario_start():
    common()
    settle(2.0)
    path = OUT / ("window-start-%dx%d@%g.png" % (SW, SH, DPR))
    on_main(lambda: grab_png(WINDOW, path))
    from PySide6.QtGui import QImage
    image = QImage(str(path))
    RESULT["image"] = [image.width(), image.height()]
    RESULT["panes"] = pane_report("start-%dx%d@%g" % (SW, SH, DPR))


def golden_end(name):
    last = json.loads((GOLDEN / name).read_text().splitlines()[-1])
    return [last["t"], last["rng"]]


def scenario_run():
    view("Show all panes")
    run_to_answer("abc abd ijk abd 1", "abc-abd-ijk-abd_1.jsonl", "run")


def scenario_run7():
    run_to_answer("abc abd xyz 3852097033", "abc-abd-xyz_3852097033.jsonl", "run7")


def run_to_answer(problem, golden, name):
    common()
    on_main(lambda: W["speed-slider"].setValue(100))      # the slider's fast end

    def enter():
        e = W["command-line"]
        e.setText(problem)
        QTest.keyClick(e, Qt.Key_Return)
    on_main(enter)
    wait_for(lambda: on_main(lambda: W["go-button"].isEnabled()), "input mode")
    t = time.time()
    on_main(lambda: W["go-button"].click())
    wait_for(lambda: BRIDGE.busy() or on_main(lambda: W["stop-button"].isEnabled()),
             "the run starts")
    wait_for(lambda: not BRIDGE.busy() and on_main(
        lambda: W["go-button"].isEnabled() and not W["stop-button"].isEnabled()),
        "the run ends", secs=240)
    RESULT["run_seconds"] = round(time.time() - t, 2)
    RESULT["end"] = [setup.g_codelet_count, chez.random_seed()]
    RESULT["golden_end"] = golden_end(golden)
    from metacat.qt import hosts
    on_main(lambda: hosts.settle_resizes(QAPP.processEvents, timeout=30))
    settle()
    tag = "%s-%dx%d" % (name, SW, SH)
    on_main(lambda: grab_png(WINDOW, OUT / ("window-%s.png" % tag)))
    RESULT["panes"] = pane_report(tag)


def driver():
    try:
        on_main(lambda: None)
        globals()["scenario_" + ARGS.scenario]()
        STATUS[0] = 0
    except BaseException:   # noqa: BLE001
        traceback.print_exc()
    print(json.dumps(RESULT), flush=True)
    sys.stdout.flush()
    os._exit(STATUS[0])


def watchdog():
    time.sleep(300)
    print("watchdog: the scenario did not finish", file=sys.stderr, flush=True)
    os._exit(2)


if __name__ == "__main__":
    threading.Thread(target=watchdog, daemon=True).start()
    DRIVER = threading.Thread(target=driver, daemon=True)
    DRIVER.start()
    QAPP.exec()
    # closing the window ends the event loop (the last window closed) while the
    # driver may still be finishing its scenario: let it print and set the status
    # (it ends the process itself; the watchdog bounds the wait)
    DRIVER.join()
    sys.stdout.flush()
    os._exit(STATUS[0])

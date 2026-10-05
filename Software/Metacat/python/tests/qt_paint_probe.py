"""Does a pane repaint while another thread runs Python?  (loop0003 item 05)

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  QT_QPA_PLATFORM=offscreen python3 python/tests/qt_paint_probe.py [--without-fix]

A QGraphicsView (metacat.qt.hosts.PaneView) of a Qt canvas with 600
rectangles; a 50 ms timer moves them and syncs the scene under the paint gate,
as MainWindow.sync does; a second thread runs Python and draws on another
canvas now and then, as the engine thread does.  Qt's own event loop runs for
2 s.  Prints {"ticks": timer ticks, "painted": paint passes (null with
--without-fix)}.  With
--without-fix, the three changes of anomalies "Qt paints the panes slowly
while the engine runs in another thread" are undone (no paint gate, no Python
paintEvent, a Python boundingRect).  Runs in a fresh process: other windows
and timers in the process would share the GIL.

Seen on 2026-10-04: 38 to 42 ticks and paints; 2 ticks with --without-fix (but
28 without the fix and with a Python event filter counting the paints:
entering Python once per paint event is itself the cure); 8 ticks with the fix when
the other thread never draws (it never waits at the paint gate).
"""
from __future__ import annotations

import contextlib
import json
import os
import sys
import threading
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
os.environ.pop("WAYLAND_DISPLAY", None)
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from PySide6.QtCore import QEventLoop, QTimer  # noqa: E402
from PySide6.QtWidgets import QApplication  # noqa: E402


def main(without_fix=False):
    app = QApplication.instance() or QApplication(["metacat-probe"])
    from metacat.gui.colors import c_white
    from metacat.qt import canvas as qcanvas
    from metacat.qt import hosts
    if without_fix:
        del hosts.PaneView.paintEvent
        qcanvas.TkItem.boundingRect = lambda self: self._rect
        qcanvas.PAINT_GATE = contextlib.nullcontext()
    canvas = qcanvas.QtCanvas(c_white)
    other = qcanvas.QtCanvas(c_white)
    for i in range(600):
        canvas.tcl("create", "rectangle", i % 30 * 20, i // 30 * 20, i % 30 * 20 + 15,
                   i // 30 * 20 + 15, "-fill", "red", "-tags", "r")
    canvas.sync()
    view = hosts.PaneView(None, canvas.scene)
    view.resize(640, 440)
    view.show()
    app.processEvents()
    stop = []

    def busy():
        n = 0
        while not stop:
            for _ in range(500):
                n += 1
            other.tcl("create", "line", 0, 0, n % 100, 10)
            other.tcl("delete", "all")
    ticks = [0]

    def tick():
        ticks[0] += 1
        canvas.tcl("move", "r", 1 if ticks[0] % 2 else -1, 0)
        with qcanvas.PAINT_GATE:
            canvas.sync()
    timer = QTimer()
    timer.timeout.connect(tick)
    timer.start(50)
    worker = threading.Thread(target=busy, daemon=True)
    worker.start()
    loop = QEventLoop()
    before = getattr(view, "paints", 0)
    QTimer.singleShot(2000, loop.quit)
    loop.exec()
    stop.append(1)
    worker.join()
    timer.stop()
    # without the fix nothing counts paints: a Python paintEvent or event
    # filter on the view is itself what lets the paint pass hold the GIL
    painted = None if without_fix else view.paints - before
    view.close()
    print(json.dumps({"ticks": ticks[0], "painted": painted}), flush=True)


if __name__ == "__main__":
    main("--without-fix" in sys.argv[1:])

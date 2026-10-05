"""The Coderack's small labels drawn by Tk and by Qt (loop0003 item 03).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

``xvfb-run -a python3 render_small_text.py tk OUT.png`` draws the Coderack's
codelet-type labels in Helvetica at 5 to 11 pixels on a tkinter Canvas and grabs
it; ``python3 render_small_text.py qt OUT.png`` (QT_QPA_PLATFORM=offscreen)
draws the same Tk commands on the Qt canvas.  The pictures are
docs/screenshots/panels/small-text-tk.png and small-text-qt.png, the evidence of
the "lose their i's and l's" entry of docs/anomalies_and_quirks.md.
"""
from __future__ import annotations

import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
sys.path.insert(0, str(HERE))

W, H = 480, 150
LABELS = "Bond builders  Bond evaluators  Whole-string  Description  illil"


def commands():
    y = 4
    for size in range(5, 12):
        yield ("create", "text", 4, y, "-text", "%d: %s" % (size, LABELS), "-anchor", "nw",
               "-font", ("helvetica", -size, "normal"))
        y += size + 8


def tk(out):
    import tkinter
    import render_sgl_fixture as r
    root = tkinter.Tk()
    root.tk.call("tk", "scaling", 96 / 72)
    c = tkinter.Canvas(root, width=W, height=H, background="white", highlightthickness=0,
                       borderwidth=0)
    c.pack()
    for command in commands():
        c.tk.call(c._w, *command)
    root.update()
    root.after(300)
    root.update()
    r.write_png(Path(out), W, H, r.grab(c.winfo_id(), W, H))
    root.destroy()


def qt(out):
    from PySide6.QtGui import QColor, QImage, QPainter
    from PySide6.QtWidgets import QApplication
    app = QApplication.instance() or QApplication([])
    from metacat.qt.canvas import QtCanvas
    c = QtCanvas()
    for command in commands():
        c.tcl(*command)
    c.sync()
    image = QImage(W, H, QImage.Format_RGB32)
    image.fill(QColor("white"))
    painter = QPainter(image)
    c.scene.render(painter, image.rect().toRectF(), image.rect().toRectF())
    painter.end()
    image.save(str(out))
    app.quit()


if __name__ == "__main__":
    {"tk": tk, "qt": qt}[sys.argv[1]](sys.argv[2])
    print("wrote", sys.argv[2])

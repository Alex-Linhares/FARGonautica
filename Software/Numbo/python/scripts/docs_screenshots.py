"""Regenerate the README's screenshots in python/docs/ (offscreen).

    python3 python/scripts/docs_screenshots.py

- gui-window.png: the whole window at 1500 x 950 after puzzle 1 seed 1,
  scaled to 1000 px wide;
- gui-gap-run.png: the tree canvas's scene at 1.5x at the end of puzzle 3
  seed 8, the kill-block gap run.

Each must stay under 200 KB (tests/test_readme.py checks it).
"""
import os
import pathlib
import sys

PYTHON_DIR = pathlib.Path(__file__).resolve().parent.parent
sys.path.insert(0, str(PYTHON_DIR))
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

from PySide6.QtCore import QEventLoop, Qt, QTimer    # noqa: E402
from PySide6.QtWidgets import QApplication           # noqa: E402

DOCS = PYTHON_DIR / "docs"
LIMIT = 200 * 1024


def spin(ms):
    loop = QEventLoop()
    QTimer.singleShot(ms, loop.quit)
    loop.exec()


def run_to_end(window, puzzle, seed):
    c = window.controls
    c.puzzle_combo.setCurrentIndex(puzzle - 1)
    c.seed_edit.setText(str(seed))
    c.cap_edit.setText("20000")
    window.outcome = None
    window.run_to_end()
    while window.outcome is None:
        spin(10)
    spin(200)


def save(image, name):
    path = DOCS / name
    if not image.save(str(path), "PNG", 0):
        raise OSError(f"could not write {path}")
    size = path.stat().st_size
    print(f"{path.relative_to(PYTHON_DIR)}: {image.width()} x {image.height()}, {size} bytes")
    if size >= LIMIT:
        raise SystemExit(f"{path} is {size} bytes, over {LIMIT}")


def main():
    app = QApplication.instance() or QApplication(sys.argv[:1])
    from numbo.gui import controls
    from numbo.gui.main_window import MainWindow

    DOCS.mkdir(exist_ok=True)
    window = MainWindow()
    window.resize(1500, 950)
    window.show()
    spin(200)
    window.controls.speed_slider.setValue(len(controls.DELAYS) - 1)

    run_to_end(window, 1, 1)
    image = window.grab().toImage()
    save(image.scaledToWidth(1000, Qt.TransformationMode.SmoothTransformation),
         "gui-window.png")

    run_to_end(window, 3, 8)
    canvas = window.tree_view.render_png(str(DOCS / "gui-gap-run.png"), 1.5)
    save(canvas, "gui-gap-run.png")
    window.close()
    app.processEvents()


if __name__ == "__main__":
    main()

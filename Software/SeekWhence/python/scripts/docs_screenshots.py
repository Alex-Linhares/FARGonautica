"""Regenerate the GUI's screenshots in python/docs/, offscreen and deterministically
(Numbo GUI, `scripts/docs_screenshots.py`; item 17; G-056).

    python3 scripts/docs_screenshots.py [--out DIR]

- gui-window.png: the whole window at the guess of run 1 (seed 54), the moment the
  guess panel asks `I have a guess!` for `3 7 3 7` [M86 p. 166], `window.grab()` scaled
  to 1000 px wide;
- gui-cytoplasm.png: the cytoplasm at the end of run 2 (seed 54), the scene at 1.5x: the
  glints, the bonds, the dubbed `(Cycle 3 3 (3 7 3))` glom and the pseudo-gloms of the
  parse [M86 pp. 167-169];
- gui-reformulation.png: run 3's storyboard (seed 30) at its first candidate,
  `new hypoth candidate (S-group 1 2)` [M86 p. 174], the dock floated at a fixed size so
  the whole episode is in view;
- gui-platoplasm.png: run 3's slip, `Countup --> S-group` [M86 p. 174], the crown
  half-way along the s-link (`CROWN_MOVE` events, G-037), the dock floated wide.

Every run is the appendix preset on its golden seed, so the pictures are the same at
every regeneration (the fonts of the machine apart). Each must stay under 200 KB
(`tests/gui/test_readme.py` checks it, as Numbo's `test_readme.py` does).
"""
import argparse
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
WINDOW_SIZE = (1500, 950)           # the window grows to its docks' minimum if larger
STORYBOARD_SIZE = (760, 980)        # the reformulation dock, floated
PLATOPLASM_SIZE = (1300, 300)       # the platoplasm dock, floated
WAIT_MS = 120_000                   # the longest wait for a run or a guess


def spin(ms):
    """Turn the Qt loop for `ms` milliseconds (Numbo GUI, `docs_screenshots.py`)."""
    loop = QEventLoop()
    QTimer.singleShot(ms, loop.quit)
    loop.exec()


def wait_until(condition, what):
    """Spin until `condition()` holds, or fail after WAIT_MS."""
    waited = 0
    while not condition():
        if waited >= WAIT_MS:
            raise SystemExit(f"timed out waiting for {what}")
        spin(10)
        waited += 10


def run_to_end(window, run_id):
    """Run an appendix preset through the window to its end, at full speed (G-054: the
    controls, the title and the stats show the run started)."""
    from seekwhence.gui.models import presets
    window.outcome = None
    window.start_session(presets.appendix_inputs(run_id))
    window.controller.run_to_end()
    wait_until(lambda: window.outcome is not None, f"the end of {run_id}")
    spin(100)


def run_to_guess(window, run_id):
    """Run an appendix preset interactively up to its guess: the worker waits in the
    answerer until the guess panel is answered (G-008) [M86 p. 166]."""
    from seekwhence.gui.models import presets
    window.outcome = None
    window.start_session(presets.appendix_inputs(run_id, interactive=True))
    window.controller.run_to_end()
    wait_until(lambda: window.guess_panel.pending is not None, f"the guess of {run_id}")
    spin(100)


def answer(window, text):
    """Answer the guess shown and wait for the run to end."""
    if not window.answer_guess(text):
        raise SystemExit("no guess was waiting")
    wait_until(lambda: window.outcome is not None, "the end of the run")


def index_of(window, kind, nth=0):
    """The index of the `nth` event of `kind` in the window's history."""
    found = [i for i, e in enumerate(window.history.events) if e.kind == kind]
    return found[nth]


def seek(window, index):
    """Seek the window to `index` (no run is live) and let it draw."""
    if not window.seek(index):
        raise SystemExit(f"could not seek to event {index}")
    spin(50)


def floated(dock, size):
    """Float a dock at a fixed size for its picture; `floated(dock, None)` docks it back."""
    if size is None:
        dock.setFloating(False)
    else:
        dock.setFloating(True)
        dock.resize(*size)
    spin(150)


def save(image, path):
    """Write `image` as a PNG at `path`; the size on the console; fail over LIMIT."""
    if not image.save(str(path), "PNG", 0):
        raise OSError(f"could not write {path}")
    size = path.stat().st_size
    print(f"{path}: {image.width()} x {image.height()}, {size} bytes")
    if size >= LIMIT:
        raise SystemExit(f"{path} is {size} bytes, over {LIMIT}")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--out", default=str(DOCS), help="the folder (default: python/docs)")
    args = parser.parse_args(argv)
    out = pathlib.Path(args.out)
    out.mkdir(parents=True, exist_ok=True)

    from seekwhence.gui import app as gui_app
    gui_app.use_pyside_plugins()
    gui_app.quiet_offscreen_warnings()
    app = QApplication.instance() or QApplication(sys.argv[:1])
    from seekwhence.gui.main_window import MainWindow
    from seekwhence.gui.models import controls, platoplasm

    window = MainWindow(settings=None)
    window.resize(*WINDOW_SIZE)
    window.show()
    spin(200)
    window.controls.speed_slider.setValue(len(controls.DELAYS) - 1)

    run_to_guess(window, "run1_3737")                               # [M86 p. 166]
    image = window.grab().toImage()
    save(image.scaledToWidth(1000, Qt.TransformationMode.SmoothTransformation),
         out / "gui-window.png")
    answer(window, "ok")

    run_to_end(window, "run2_373373")                               # [M86 pp. 167-169]
    save(window.cytoplasm_view.render_png(str(out / "gui-cytoplasm.png"), 1.5),
         out / "gui-cytoplasm.png")

    run_to_end(window, "run3_121231234")                            # [M86 pp. 170-179]
    seek(window, index_of(window, "candidate"))                     # [M86 p. 174]
    floated(window.reformulation_dock, STORYBOARD_SIZE)
    save(window.reformulation_view.render_png(str(out / "gui-reformulation.png")),
         out / "gui-reformulation.png")
    floated(window.reformulation_dock, None)

    seek(window, index_of(window, "class-changed") + platoplasm.CROWN_MOVE // 2)
    floated(window.platoplasm_dock, PLATOPLASM_SIZE)
    save(window.platoplasm_view.render_png(str(out / "gui-platoplasm.png")),
         out / "gui-platoplasm.png")
    floated(window.platoplasm_dock, None)

    window.close()
    app.processEvents()
    return 0


if __name__ == "__main__":
    sys.exit(main())

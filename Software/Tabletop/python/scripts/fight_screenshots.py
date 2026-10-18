"""Draw the pictures of docs/gui/worldview-fights.md, offscreen and deterministically: the
window and the table at three fights for the Worldview in a run of Figure 2.11 (seed 6,
the `standard` profile), where the glass in Eliza's row and the lone cup take the
Worldview from each other (Numbo GUI, `scripts/docs_screenshots.py`; G-056).

    cd python
    python3 -m scripts.fight_screenshots [--out DIR]

Run as a module from `python/`, as `scripts/docs_screenshots.py` is: no `sys.path` tricks
(ANSWERS.md 9). It needs PySide6 (`pip install -e '.[gui]'`); without it, it says so and
exits 1. It renders with `QT_QPA_PLATFORM=offscreen`, so no window opens. The run is the
engine's: the same problem, seed and profile give the same events on any machine (D-064),
so the pictures come out the same at every regeneration (the fonts of the machine apart).

Three moments, each as the whole window (`fig-2-11-s6-<codelet>-window.png`, scaled to
1000 px wide) and as the table alone (`fig-2-11-s6-<codelet>-table.png`), the window
sought to the `CompetitionHeld` event of the codelet:

- codelet 198: the cup-to-cup correspondence beats the glass-to-cup one, 76 against 73;
- codelet 219: the glass challenges the cup and the glass-to-knife pair, and loses;
- codelet 321: the glass wins the Worldview back by one point, 75 against 74.
"""
from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

PYTHON_DIR = Path(__file__).resolve().parents[1]
OUT = PYTHON_DIR / "docs" / "gui"
PROBLEM, PROFILE, SEED = "ch2-fig11", "standard", 6
WINDOW_SIZE = (1500, 950)
WINDOW_WIDTH = 1000
TABLE_SCALE = 1.6
FIGHTS = (198, 219, 321)            # the codelets whose CompetitionHeld event is drawn


def save(image, path):
    """Write `image` as a PNG at `path`; the size on the console."""
    if not image.save(str(path), "PNG", 0):
        raise OSError(f"could not write {path}")
    print(f"{path}: {image.width()} x {image.height()}, {path.stat().st_size} bytes")


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out", type=Path, default=OUT, help="where to write (docs/gui/)")
    args = parser.parse_args(argv)
    try:
        import PySide6  # noqa: F401
    except ImportError:
        print("fight_screenshots.py draws with PySide6, which is not installed: "
              "pip install -e '.[gui]'", file=sys.stderr)
        return 1
    try:
        from scripts import docs_screenshots as ds
        from tabletop.gui import app as gui_app
    except ImportError:
        print("run it from python/ as `python3 -m scripts.fight_screenshots`",
              file=sys.stderr)
        return 1
    args.out.mkdir(parents=True, exist_ok=True)
    gui_app.use_pyside_plugins()
    gui_app.quiet_offscreen_warnings()
    from PySide6.QtCore import Qt
    from PySide6.QtWidgets import QApplication
    from tabletop.gui.main_window import MainWindow

    app = QApplication.instance() or QApplication(sys.argv[:1])
    window = MainWindow(settings=None)
    window.resize(*WINDOW_SIZE)
    window.show()
    ds.spin(200)
    window.outcome = None
    window.set_next_run(problem=PROBLEM, profile=PROFILE, seed=SEED)
    window.run_to_end()
    ds.wait_until(lambda: window.outcome is not None, f"the end of {PROBLEM} seed {SEED}")
    ds.spin(100)
    print(window.statusBar().currentMessage())
    wanted = set(FIGHTS)
    for index, event in enumerate(window.history.events):
        if event.name == "CompetitionHeld" and event.clock in wanted:
            wanted.discard(event.clock)
            print(f"codelet {event.clock}: event {index}, {event.newcomer} against "
                  f"{' '.join(event.rivals)}, old {event.old_rating:.0f} rival "
                  f"{event.rival_rating:.0f}, {'rival wins' if event.rival_wins else 'old holds'}")
            ds.seek(window, index)
            image = ds.rgb(window.grab().toImage())
            save(image.scaledToWidth(WINDOW_WIDTH, Qt.TransformationMode.SmoothTransformation),
                 args.out / f"fig-2-11-s6-{event.clock}-window.png")
            path = args.out / f"fig-2-11-s6-{event.clock}-table.png"
            save(ds.rgb(window.table_view.render_png(path, TABLE_SCALE)), path)
    if wanted:
        raise SystemExit(f"no CompetitionHeld event at codelets {sorted(wanted)}")
    window.close()
    app.processEvents()
    return 0


if __name__ == "__main__":
    sys.exit(main())

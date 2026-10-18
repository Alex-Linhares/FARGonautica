"""Regenerate the GUI's screenshots in python/docs/, offscreen and deterministically
(Numbo GUI, `scripts/docs_screenshots.py`, through the Seek-Whence GUI's; G-056).

    cd python
    python3 -m scripts.docs_screenshots [--out DIR]

Run as a module from `python/`, as `scripts/docs_figures.py` is: no `sys.path` tricks
(ANSWERS.md 9). It needs PySide6 (`pip install -e '.[gui]'`); without it, it says so and
exits 1. It renders with `QT_QPA_PLATFORM=offscreen`, so no window opens.

The pictures, four from the quick start's run, `surround-05` seed 3 under `standard`
(since v5's fix, D-116 to D-120: Eliza touches cup2 (E2), 384 codelets, T 28.3), and the
fifth from a run that reads every family of roles, `blockage-07` seed 104 (Eliza touches her
own cup, E3, the answer recorded as blocked, 299 codelets), so that they come out the same at every regeneration (the fonts
of the machine apart):

- gui-window.png: the whole window, the built-in layout, at the choice of the last
  codelet, find-answer (the structures complete, the Worldview holding cp13, the
  thermometer down to 17.8, the answer not yet given), `window.grab()` scaled to 1000 px
  wide;
- gui-table.png: the P1 table at the answer: the objects as the GUI's icons, H on cup1,
  E on cup2, the groups and the correspondences as French's screen drew them
  [F95 pp. 96-107 (img)];
- gui-slipnet.png: the P3 Slipnet grid at the jump (the temperature update that follows
  the first Worldview change, codelet 197; since v3 it is a drop, on v4 to 40.7), with "French's ch. 4" on: his nearest dump's
  values in red in the lower right corners, ours in black [F95 p. 95] (G-032);
- gui-temperature.png: the P2 thermometer at the jump (T 40.7 by the change, held no
  more; G-027) beside the P2b plot at the end of the run, with the three terms stacked
  and French's chapter 4 thermometer as labelled points (G-028, G-029); the plot is GUI
  design, not French's;
- gui-roles.png: the P1 table with the roles layer on (G-061) beside the P14 Roles pane
  (G-060), both at the end of the rich run: the badges with the standing roles' codes, the
  midline band, the point-symmetry axis, the touched cup's images and the barrier's claim
  on the table; in the pane, scrolled to the structural family, the axis read off the
  Worldview, the midline, the extra glass tagged BARRIER with the glass it claims tagged
  CLAIMED, and the consumers that read them (the fight C4 posted, the answer recorded as
  blocked). Both are GUI design, not French's: a re-interpretation (D-078).

Each must stay under 200 KB (`tests/gui/test_gui_readme.py` checks it, as Numbo's
`test_readme.py` does).
"""
from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

PYTHON_DIR = Path(__file__).resolve().parents[1]
DOCS = PYTHON_DIR / "docs"
LIMIT = 200 * 1024
PROBLEM, PROFILE, SEED = "surround-05", "standard", 3     # the quick start's run
RICH_PROBLEM, RICH_SEED = "blockage-07", 104        # every family of roles read, the barrier, its claim and the axis (gui-roles.png; seed 104 since v5's fix, D-116 to D-120; 1 in v4, D-115; 27 in v3, D-114; 16 before)
WINDOW_SIZE = (1500, 950)           # the window grows to its docks' minimum if larger
WINDOW_WIDTH = 1000                 # gui-window.png is scaled to this width
TABLE_SCALE = 1.6                   # the table's scene, in px per scene unit
GRID_SIZE = (1000, 660)             # the Slipnet grid dock, floated
PLOT_SIZE = (900, 420)              # the temperature plot dock, floated
THERMOMETER_SIZE = (150, 420)       # the thermometer dock, floated
GAP = 12                            # between the thermometer and the plot
ROLES_SIZE = (700, 480)             # the roles dock, floated (528 high until v3, D-114)
ROLES_TABLE_SCALE = 0.72            # the table beside it, in px per scene unit (0.8 until v3)
WAIT_MS = 120_000                   # the longest wait for a run
PICTURES = ("gui-window.png", "gui-table.png", "gui-slipnet.png", "gui-temperature.png",
            "gui-roles.png")


def spin(ms):
    """Turn the Qt loop for `ms` milliseconds (Numbo GUI, `docs_screenshots.py`)."""
    from PySide6.QtCore import QEventLoop, QTimer
    loop = QEventLoop()
    QTimer.singleShot(ms, loop.quit)
    loop.exec()


def wait_until(condition, what):
    """Spin until `condition()` holds, or fail after WAIT_MS (Seek-Whence GUI,
    `docs_screenshots.py`)."""
    waited = 0
    while not condition():
        if waited >= WAIT_MS:
            raise SystemExit(f"timed out waiting for {what}")
        spin(10)
        waited += 10


def run_to_end(window, problem=PROBLEM, seed=SEED):
    """Run `problem` with `seed` under the quick start's profile through the window to its
    end, every event drawn (G-044: the controls, the title and the stats show the run
    started)."""
    window.outcome = None
    window.set_next_run(problem=problem, profile=PROFILE, seed=seed)
    window.run_to_end()
    wait_until(lambda: window.outcome is not None, f"the end of {problem} seed {seed}")
    if window.outcome.result is None:
        raise SystemExit(f"the run did not end: {window.outcome.error}")
    spin(100)


def index_of(window, name, nth=0, after=-1):
    """The index of the `nth` event of class `name` past index `after` in the window's
    history; `nth` -1 is the last."""
    found = [i for i, e in enumerate(window.history.events)
             if i > after and type(e).__name__ == name]
    return found[nth]


def jump_index(window):
    """The index of the temperature update that follows the first Worldview change: the
    jump to about 96 (RESULTS.md section 6.4; ANSWERS.md 11)."""
    return index_of(window, "TemperatureUpdated", after=index_of(window, "WorldviewChanged"))


def seek(window, index):
    """Seek the window to `index` (no run is live) and let it draw."""
    if not window.seek(index):
        raise SystemExit(f"could not seek to event {index}")
    spin(50)


def floated(dock, size):
    """Float a dock at a fixed size for its picture; `floated(dock, None)` docks it back
    (Seek-Whence GUI, `docs_screenshots.py`)."""
    if size is None:
        dock.setFloating(False)
    else:
        dock.setFloating(True)
        dock.resize(*size)
    spin(150)


def rgb(image):
    """The image without its alpha channel, so that a grab inside a window and a bare
    render save alike (G-027)."""
    from PySide6.QtGui import QImage
    return image.convertToFormat(QImage.Format.Format_RGB32)


def save(image, path):
    """Write `image` as a PNG at `path`; the size on the console; fail over LIMIT
    (Seek-Whence GUI, `docs_screenshots.py`)."""
    if not image.save(str(path), "PNG", 0):
        raise OSError(f"could not write {path}")
    size = path.stat().st_size
    print(f"{path}: {image.width()} x {image.height()}, {size} bytes")
    if size >= LIMIT:
        raise SystemExit(f"{path} is {size} bytes, over {LIMIT}")


def side_by_side(left, right, gap=GAP):
    """One image with `left` and `right` beside each other on a white ground, their tops
    aligned (G-056)."""
    from PySide6.QtCore import QPoint
    from PySide6.QtGui import QColor, QImage, QPainter
    width = left.width() + gap + right.width()
    height = max(left.height(), right.height())
    image = QImage(width, height, QImage.Format.Format_RGB32)
    image.fill(QColor("white"))
    painter = QPainter(image)
    painter.drawImage(QPoint(0, 0), left)
    painter.drawImage(QPoint(left.width() + gap, 0), right)
    painter.end()
    return image


def shoot_window(window, out):
    """gui-window.png: the whole window at the choice of the last codelet."""
    from PySide6.QtCore import Qt
    seek(window, index_of(window, "CodeletChosen", nth=-1))
    image = rgb(window.grab().toImage())
    save(image.scaledToWidth(WINDOW_WIDTH, Qt.TransformationMode.SmoothTransformation),
         out / "gui-window.png")


def shoot_table(window, out):
    """gui-table.png: the table at the answer [F95 pp. 96-107 (img)]."""
    seek(window, len(window.history.events) - 1)
    path = out / "gui-table.png"
    image = rgb(window.table_view.render_png(path, TABLE_SCALE))
    save(image, path)


def shoot_slipnet(window, out):
    """gui-slipnet.png: the grid at the jump beside French's nearest dump [F95 p. 95]
    (G-032)."""
    seek(window, jump_index(window))
    view = window.slipnet_grid_view
    view.french_action.setChecked(True)
    floated(window.slipnet_grid_dock, GRID_SIZE)
    path = out / "gui-slipnet.png"
    view.render_png(path)
    from PySide6.QtGui import QImage
    save(QImage(str(path)), path)
    view.french_action.setChecked(False)
    floated(window.slipnet_grid_dock, None)


def shoot_temperature(window, out):
    """gui-temperature.png: the thermometer at the jump beside the plot at the end of the
    run, with the terms and French's chapter 4 thermometer (G-027 to G-029)."""
    seek(window, jump_index(window))
    floated(window.thermometer_dock, THERMOMETER_SIZE)
    thermometer = rgb(window.thermometer_view.grab().toImage())
    floated(window.thermometer_dock, None)
    seek(window, len(window.history.events) - 1)
    view = window.temperature_view
    view.terms_action.setChecked(True)
    view.french_action.setChecked(True)
    floated(window.temperature_dock, PLOT_SIZE)
    plot = rgb(view.plot.grab().toImage())
    floated(window.temperature_dock, None)
    view.terms_action.setChecked(False)
    view.french_action.setChecked(False)
    save(side_by_side(thermometer, plot), out / "gui-temperature.png")


def scroll_to_family(widget, family):
    """Scroll the roles list so that the line before the `family` heading is at the top
    (the pane's `LinesWidget`, G-060), and let it draw."""
    k = next(i for i, line in enumerate(widget.lines) if line.text.startswith(family))
    widget.verticalScrollBar().setValue(0)
    widget.verticalScrollBar().setValue(max(0, int(widget.line_top(max(k - 1, 0)) - 4)))
    widget.viewport().update()
    spin(50)


def shoot_roles(window, out):
    """gui-roles.png: the table with the roles layer on beside the roles pane at the end
    of the rich run (G-060, G-061; D-078)."""
    run_to_end(window, RICH_PROBLEM, RICH_SEED)
    seek(window, len(window.history.events) - 1)
    layer = window.table_view.view.layer_actions["roles"]
    layer.setChecked(True)
    spin(100)
    path = out / "gui-roles.png"
    table = rgb(window.table_view.render_png(path, ROLES_TABLE_SCALE))
    layer.setChecked(False)
    floated(window.roles_dock, ROLES_SIZE)
    window.roles_view.redraw(now=True)
    scroll_to_family(window.roles_view.list, "Structural")
    pane = rgb(window.roles_view.grab().toImage())
    floated(window.roles_dock, None)
    save(side_by_side(table, pane), path)


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--out", type=Path, default=DOCS, help="where to write (python/docs/)")
    args = parser.parse_args(argv)
    try:
        import PySide6  # noqa: F401
    except ImportError:
        print("docs_screenshots.py draws with PySide6, which is not installed: "
              "pip install -e '.[gui]' (the tabletop package itself needs nothing).",
              file=sys.stderr)
        return 1
    try:
        from tabletop.gui import app as gui_app
    except ImportError:
        print("run it from python/ as `python3 -m scripts.docs_screenshots`, or install the "
              "package first (pip install -e .)", file=sys.stderr)
        return 1
    args.out.mkdir(parents=True, exist_ok=True)
    gui_app.use_pyside_plugins()
    gui_app.quiet_offscreen_warnings()
    from PySide6.QtWidgets import QApplication
    from tabletop.gui.main_window import MainWindow

    app = QApplication.instance() or QApplication(sys.argv[:1])
    window = MainWindow(settings=None)
    window.resize(*WINDOW_SIZE)
    window.show()
    spin(200)
    run_to_end(window)
    shoot_window(window, args.out)
    shoot_table(window, args.out)
    shoot_slipnet(window, args.out)
    shoot_temperature(window, args.out)
    shoot_roles(window, args.out)
    window.close()
    app.processEvents()
    return 0


if __name__ == "__main__":
    sys.exit(main())

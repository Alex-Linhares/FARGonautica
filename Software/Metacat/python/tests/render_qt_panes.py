"""Golden runs drawn in the Qt main window's panes, offscreen, grabbed to PNGs.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  QT_QPA_PLATFORM=offscreen python3 python/tests/render_qt_panes.py OUTDIR SCENE [WxH] [--resize]

The Qt counterpart of render_views.py (loop0003 item 04).  The scene's golden
run is driven directly (metacat.headless.run_problem, in this thread, which is
also the GUI thread) with every window attached on Qt hosts
(metacat.qt.hosts): each graphics window is a pane of one MainWindow, laid out
by its default splitter tree at WxH (default 1920x1010, a maximised window on
a 1920x1080 screen), made resizable as setup.ss's enable-resizing does, and
redrawn at its pane's size by its own resize method before the run starts.
After the run the panes are synchronised and grabbed: OUTDIR/window-SCENE.png
(the whole window) and OUTDIR/PANE-SCENE.png (each visible pane, named as
render_views.py names the windows).  The trace goes to OUTDIR/SCENE.jsonl.
The last line of stdout is a JSON object: reason, codelets, the panes' items,
visibility and geometry, the settling and run times.  Each scene runs in its
own process (a run changes the engine for good); the script quits its
QApplication by itself.

With --resize, the window is resized among three sizes every RESIZE_EVERY
codelets during the run (at the run's update-everything, the seam run.py
leaves to drivers), and the events are processed there: the panes' configures
reach the panels, whose resize methods redraw on the resize listener thread
while the run goes on, as in the tkinter GUI.  At the end the window goes back
to WxH and settles before the grab.  Resizing redraws pictures only: the
trace must stay the golden.
"""
from __future__ import annotations

import io
import json
import os
import sys
import time
from contextlib import redirect_stdout
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent))

RUN7 = ["abc", "abd", "xyz"], 3852097033
# name: (strings, seed, cap, keep-going?)
SCENES = {
    "run7-300": (*RUN7, 300, False),
    "run7-800": (*RUN7, 800, False),
    "run7-answer": (*RUN7, 10000, False),
    "abz-1000": (["a", "b", "z"], 1, 1000, True),
}


RESIZES = [(1600, 900), (2200, 1200), (1650, 1000)]   # the window is at least 1600 wide
RESIZE_EVERY = 150      # codelets


def render(outdir, scene, size=(1920, 1010), resize_during=False):
    os.environ["QT_QPA_PLATFORM"] = "offscreen"
    os.environ.pop("WAYLAND_DISPLAY", None)
    from PySide6.QtWidgets import QApplication
    from metacat import headless, setup
    from metacat.gui import app as gui_app
    from metacat.gui import views
    from metacat.qt import hosts
    from metacat.qt.grab import grab_png
    from metacat.qt.mainwindow import MainWindow

    strings, seed, cap, keep = SCENES[scene]
    app = QApplication.instance() or QApplication(["metacat-tests"])
    hosts.install()
    headless.prepare()
    views.load_views()
    window = MainWindow()
    window.resize(*size)
    info = {}

    def attach():
        windows = views.attach_views()
        window.place_windows(windows)
        gui_app.enable_resizing()
        window.show()
        t = time.monotonic()
        settled = hosts.settle_resizes(app.processEvents, timeout=60)
        info["settle_seconds"] = round(time.monotonic() - t, 2)
        info["settled"] = settled

    trace = io.StringIO()
    out = io.StringIO()
    t = time.monotonic()
    if resize_during:
        # the driver's seam (run.py reads update_everything through the package)
        import metacat.run as mrun
        update_everything = mrun.update_everything
        state = {"k": 0, "last": 0}

        def update_and_resize():
            update_everything()
            if setup.g_codelet_count - state["last"] >= RESIZE_EVERY:
                state["last"] = setup.g_codelet_count
                window.resize(*RESIZES[state["k"] % len(RESIZES)])
                state["k"] += 1
                app.processEvents()
        mrun.update_everything = update_and_resize
    with redirect_stdout(out):
        reason, answers = headless.run_problem(strings, seed, cap, keep, trace, views=attach)
    if resize_during:
        mrun.update_everything = update_everything
        info["resizes"] = state["k"]
        window.resize(*size)
        info["settled_after"] = hosts.settle_resizes(app.processEvents, timeout=60)
    info["run_seconds"] = round(time.monotonic() - t, 2)
    window.sync()
    app.processEvents()
    outdir = Path(outdir)
    outdir.mkdir(parents=True, exist_ok=True)
    (outdir / ("%s.jsonl" % scene)).write_text(trace.getvalue())
    grab_png(window, outdir / ("window-%s.png" % scene))
    panes = {}
    for name, pane in window.panes.items():
        host = pane.host
        visible = pane.isVisibleTo(window)
        geometry = pane.geometry()
        view = pane.view.geometry()
        panes[name] = {
            "items": len(host.canvas.display_list.items),
            "created": host.canvas.display_list.next_id - 1,
            "scene_items": len(host.canvas.scene.items()),
            "visible": visible,
            "pane": [geometry.width(), geometry.height()],
            "view": [view.x(), view.y(), view.width(), view.height()],
            "canvas": [host.width, host.height],
            "scene_rect": [host.pane.view.sceneRect().height()],
            "vscroll": [host.pane.view.verticalScrollBar().value(),
                        host.pane.view.verticalScrollBar().maximum()],
        }
        if visible:
            grab_png(pane, outdir / ("%s-%s.png" % (name, scene)))
    from metacat.gui import fonts as gfonts
    info.update(scrollbar=gfonts.g_scrollbar_width, reason=reason, answers=answers, codelets=setup.g_codelet_count,
                panes=panes, window=[window.width(), window.height()],
                sizes=window.splitter_sizes())
    window.close()
    app.processEvents()
    app.quit()
    print(json.dumps(info), flush=True)
    return 0


def main(argv):
    resize_during = "--resize" in argv
    argv = [a for a in argv if a != "--resize"]
    size = (1920, 1010)
    if len(argv) > 2:
        w, h = argv[2].split("x")
        size = (int(w), int(h))
    return render(argv[0], argv[1], size, resize_during)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

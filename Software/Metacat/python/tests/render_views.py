"""Draw Metacat's windows at points of golden runs on real Tk canvases, as PNGs.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  xvfb-run -a -s "-screen 0 3000x2000x24" python3 python/tests/render_views.py OUTDIR [SCENE ...]

Never on the owner's screen: run it under xvfb-run (tests/test_views.py does).
The counterpart of racket/tests/views-harness.rkt's scenes.  Each scene is a
golden run with every window attached (metacat.gui.views.attach_views) on Tk
hosts (metacat.gui.hosts.TkHost), fonts measured by Tk on the logo window's
hidden canvas (fonts.ss's create-mcat-logo) at 96 dpi; then an action (a click
in the Trace or Memory window, a snag event's or an answer description's
display); then each pictured window is raised and grabbed from the X server
into OUTDIR/WINDOW-SCENE.png.  Prints one line per picture:
"WINDOW-SCENE.png W H COLOURS ITEMS".  Each scene runs in its own process (a
run changes the engine for good).  The script exits by itself.
"""
from __future__ import annotations

import io
import os
import subprocess
import sys
from contextlib import redirect_stdout
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE.parent))

RUN7 = ["abc", "abd", "xyz"], 3852097033
ALL = ["workspace", "slipnet", "coderack", "top-themes", "bottom-themes", "vertical-themes",
       "memory", "commentary", "trace", "temperature", "EEG"]


def all_but(*names):
    return [w for w in ALL if w not in names]


# name: (strings, seed, cap, keep-going?, action, windows pictured).  Windows a
# scene leaves blank are not pictured (as in views-harness.rkt).
SCENES = {
    "run7-300": (*RUN7, 300, False, "window", all_but("bottom-themes", "memory")),
    "run7-800": (*RUN7, 800, False, "window", all_but("bottom-themes", "memory")),
    "run7-snag-event": (*RUN7, 800, False, "snag-event", ["workspace"]),
    "run7-answer": (*RUN7, 10000, False, "window", all_but("bottom-themes")),
    "run7-answer-description": (*RUN7, 10000, False, "answer-description", ["workspace"]),
    "run7-clamp-click": (*RUN7, 10000, False, ("click-event", "clamp"),
                         ["workspace", "slipnet", "coderack", "vertical-themes", "trace",
                          "temperature"]),
    "xyd-justify": (["abc", "abd", "xyz", "xyd"], 1760747975, 10000, False, "window", ALL),
    "glz-compare": (["abc", "abd", "glz"], 1108779034, 1800, True, ("click-answers", 1, 2),
                    all_but("trace", "EEG")),
}


def click_point(window, find, thing):
    """views-harness.rkt: click-point, a point inside thing's box, by asking find(x, y)
    over a grid of the window's coordinates"""
    from metacat import chez
    from metacat.objects import tell
    xmax = tell(window, "get-x-max")
    ymax = tell(window, "get-y-max")
    for i in range(2000):
        for j in range(200):
            x, y = chez.mul(i, chez.div(xmax, 2000)), chez.mul(j, chez.div(ymax, 200))
            if find(x, y) is thing:
                return x, y
    raise RuntimeError("click-point: not in the window")


def render_scene(name, outdir):
    import tkinter
    import golden_harness as g
    from metacat import headless, setup
    from metacat.gui import fonts, hosts, swl, views
    from metacat.objects import tell
    import render_sgl_fixture as r
    import metacat as _metacat

    strings, seed, cap, keep, action, pictured = SCENES[name]
    root = tkinter.Tk()
    root.withdraw()
    root.tk.call("tk", "scaling", 96 / 72)
    swl.g_tk_root = root
    fonts.load()
    fonts.create_mcat_logo(root)
    fonts.g_mcat_logo.widget.winfo_toplevel().geometry("+2800+1800")
    hosts.set_window_host_maker(hosts.tk_host_maker(root))
    headless.prepare()
    views.load_views()
    windows = {}

    def attach():
        windows.update(views.attach_views())

    out = io.StringIO()
    try:
        with redirect_stdout(out):
            headless.run_problem(strings, seed, cap, keep, views=attach)
            if action == "snag-event":
                tell(windows["workspace"], "clear")
                tell(tell(_metacat.trace.g_trace, "get-last-event", "snag"), "display-workspace")
            elif action == "answer-description":
                tell(tell(_metacat.memory.g_memory, "get-answers")[0], "display-workspace")
            elif action != "window" and action[0] == "click-event":
                from metacat.gui import trace_graphics as tg
                event = tell(_metacat.trace.g_trace, "get-last-event", action[1])
                x, y = click_point(setup.g_trace_window,
                                   lambda x, y: tell(_metacat.trace.g_trace,
                                                     "get-mouse-selected-event", x, y),
                                   event)
                tg.trace_window_press_handler(setup.g_trace_window, x, y)
            elif action != "window" and action[0] == "click-answers":
                from metacat.gui import memory_graphics as mg
                answers = list(reversed(tell(_metacat.memory.g_memory, "get-all-descriptions")))
                for i in action[1:]:
                    x, y = click_point(setup.g_memory_window,
                                       lambda x, y: tell(_metacat.memory.g_memory,
                                                         "get-mouse-selected-answer", x, y),
                                       answers[i])
                    mg.memory_window_press_handler(setup.g_memory_window, x, y)
    finally:
        sys.stderr.write(out.getvalue()[-2000:] if os.environ.get("RENDER_VERBOSE") else "")
    for _ in range(3):
        root.update()
    # The scenes run in parallel processes on one X display, and XGetImage reads
    # the screen: a window another scene raises at +0+0 between our lift and our
    # grab would be pictured instead of ours.  So raise-and-grab holds a lock.
    import fcntl
    lock = open(Path(outdir) / ".grab-lock", "w")
    fcntl.flock(lock, fcntl.LOCK_EX)
    try:
        grab_windows(name, outdir, root, windows, pictured, r)
    finally:
        fcntl.flock(lock, fcntl.LOCK_UN)
        lock.close()
    root.destroy()


def grab_windows(name, outdir, root, windows, pictured, r):
    """raise each pictured window at +0+0 and grab it into OUTDIR/WINDOW-SCENE.png"""
    from metacat.objects import tell
    for wname in pictured:
        window = windows[wname]
        widget = tell(window, "get-vp").canvas.widget
        top = widget.winfo_toplevel()
        top.geometry("+0+0")
        top.lift()
        for _ in range(3):
            root.update()
        w, h = widget.winfo_width(), widget.winfo_height()
        pixels = r.grab(widget.winfo_id(), w, h)
        file = Path(outdir) / ("%s-%s.png" % (wname, name))
        r.write_png(file, w, h, pixels)
        items = len(widget.find_all())
        print("%s %d %d %d %d" % (file.name, w, h, len(set(pixels)), items), flush=True)


def main(argv):
    outdir = argv[0]
    os.makedirs(outdir, exist_ok=True)
    names = argv[1:] or list(SCENES)
    if len(names) == 1:
        render_scene(names[0], outdir)
        return 0
    # one process per scene, in parallel
    procs = [subprocess.Popen([sys.executable, __file__, outdir, n], stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE, text=True) for n in names]
    status = 0
    for n, p in zip(names, procs):
        o, e = p.communicate()
        sys.stdout.write(o)
        if p.returncode != 0:
            sys.stdout.write("FAILED %s\n%s\n" % (n, e[-3000:]))
            status = 1
    return status


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

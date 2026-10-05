"""The GUI program: setup.ss's setup and enable-resizing, the engine thread, and
the window layout.  `python3 -m metacat.gui [SCALE]` runs `main`.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Metacat is based on Copycat, which was originally written in Common Lisp
by Melanie Mitchell.  Translated to Python (2026) from setup.ss's setup and
enable-resizing (the rest of setup.ss is the engine's metacat/setup.py), with
racket/gui/setup.rktl and racket/gui/gui.rkt as a worked translation.

What SWL and the Chez REPL gave the original is here, marked "port:":

- the Tk root, created withdrawn (SWL had one), with 96 dpi scaling so that
  pictures do not depend on the display (python/tests/render_views.py);
- `EngineThread`, the counterpart of the original's REPL thread.  The control
  panel hands it thunks with SWL's thread-break (gui.thread_break): init-mcat
  and run-mcat, go, ...  It runs them one at a time, each through
  run.toplevel, until run.ss's break or quiet-break parks the run (run.py's
  break/go) and control returns to it.  So the engine runs in its own thread
  and Tk's main thread only answers events: the GUI stays responsive.  The
  engine's drawing reaches Tk through tkinter, which hands each Tk call from
  the engine thread to the main thread (Tcl is threaded).  Why a thread and
  not `after`-driven stepping: suspend can break from inside a codelet
  (answers.ss), and only a parked thread can resume there (item 11); stepping
  from `after` would have to split codelets.
- the window layout (`arrange_windows`), after racket/gui/gui.rkt's: the
  original left placement to the window manager.

This module imports tkinter only inside functions.
"""
from __future__ import annotations

import sys
import threading
import traceback

from metacat import chez, engine, run
from metacat import setup as S   # setup.ss's globals (this module's setup is setup.ss's setup)
from metacat.objects import tell

g_root = False


class EngineThread:
    """port: the REPL thread.  send(thunk) queues thunk (SWL's thread-break); the
    thread runs each thunk through run.toplevel, so that a break parks the run
    and the next thunk (go) can resume it.  An error in the model ends the thunk
    and is reported to the control panel."""

    def __init__(self):
        import queue
        self.queue = queue.SimpleQueue()
        self.lock = threading.Lock()
        self.pending = 0
        self.thread = threading.Thread(target=self.loop, name="metacat-repl", daemon=True)
        self.thread.start()

    def send(self, thunk):
        with self.lock:
            self.pending += 1
        self.queue.put(thunk)

    def busy_p(self):
        with self.lock:
            return self.pending > 0

    def loop(self):
        while True:
            thunk = self.queue.get()
            try:
                run.toplevel(thunk)
            except Exception as e:   # noqa: BLE001 - the original's REPL printed it
                traceback.print_exc()
                message = str(e)
                run.g_running_p = False
                try:
                    tell(S.g_control_panel, "engine-error", message)
                except Exception:   # noqa: BLE001
                    traceback.print_exc()
            finally:
                sys.stdout.flush()
                with self.lock:
                    self.pending -= 1


def engine_busy_p():
    """port: is the engine thread running a thunk (or has one waiting)?"""
    thread = S.g_repl_thread
    return bool(thread) and thread.busy_p()


# --------------------------------------------------------------------------------
# port: where the windows go.  The original left placement to the window
# manager; the port tiles them in three rows (racket/gui/gui.rkt): the control
# panel with the Temperature under it, then the Workspace, Coderack and
# Commentary; the Slipnet, the Top and Bottom Themes (stacked), the Vertical
# Themes and the Memory; the Temporal Trace (and the EEG, hidden at first)
# under the Slipnet.

def window_frames():
    """port: the graphics windows by name"""
    return [("workspace", S.g_workspace_window), ("slipnet", S.g_slipnet_window),
            ("coderack", S.g_coderack_window), ("temperature", S.g_temperature_window),
            ("trace", S.g_trace_window), ("commentary", S.g_comment_window),
            ("memory", S.g_memory_window), ("top-themes", S.g_top_themes_window),
            ("bottom-themes", S.g_bottom_themes_window),
            ("vertical-themes", S.g_vertical_themes_window), ("EEG", S.g_EEG_window)]


p_window_gap = 8
p_title_bar = 28


def arrange_windows(control_panel_size):
    """port: racket/gui/gui.rkt's arrange-windows!"""
    def wd(w):
        return 2 + tell(w, "get-size")[0]

    def ht(w):
        return 2 + tell(w, "get-size")[1] + p_title_bar

    def place(w, x, y):
        tell(w, "set-position", x, y)
    cp_w = control_panel_size[0]
    cp_h = control_panel_size[1] + p_title_bar
    g = p_window_gap
    s = S
    # row 1
    x1 = max(cp_w, wd(s.g_temperature_window)) + g
    x2 = x1 + wd(s.g_workspace_window) + g
    x3 = x2 + wd(s.g_coderack_window) + g
    row1_h = max(cp_h + g + ht(s.g_temperature_window), ht(s.g_workspace_window),
                 ht(s.g_coderack_window), ht(s.g_comment_window))
    y2 = row1_h + g
    place(s.g_temperature_window, 0, cp_h + g)
    place(s.g_workspace_window, x1, 0)
    place(s.g_coderack_window, x2, 0)
    place(s.g_comment_window, x3, 0)
    # row 2
    xb = wd(s.g_slipnet_window) + g
    xc = xb + max(wd(s.g_top_themes_window), wd(s.g_bottom_themes_window)) + g
    xd = xc + wd(s.g_vertical_themes_window) + g
    y3 = y2 + ht(s.g_slipnet_window) + g
    place(s.g_slipnet_window, 0, y2)
    place(s.g_top_themes_window, xb, y2)
    place(s.g_bottom_themes_window, xb, y2 + ht(s.g_top_themes_window) + g)
    place(s.g_vertical_themes_window, xc, y2)
    place(s.g_memory_window, xd, y2)
    # row 3
    place(s.g_trace_window, 0, y3)
    place(s.g_EEG_window, 0, y3 + ht(s.g_trace_window) + g)
    return "done"


# --------------------------------------------------------------------------------

def make_root():
    """port: SWL's Tk application: the root window, withdrawn, at 96 dpi"""
    global g_root
    import tkinter
    from metacat.gui import swl
    root = tkinter.Tk(className="Metacat")
    root.tk = swl.ThreadSafeTk(root.tk)   # before any widget: they share it
    root.withdraw()
    root.tk.call("tk", "scaling", 96 / 72)
    swl.g_tk_root = root
    g_root = root
    return root


def setup(*args):
    """setup.ss: setup"""
    from metacat.gui import constants as K
    from metacat.gui import fonts, gui, hosts, views
    scale = 1 if not args else args[0]
    chez.printf("Initializing windows...")
    sys.stdout.flush()
    root = g_root or make_root()
    engine.load()
    # port: the graphics files are loaded here (metacat.ss loaded them with the
    # model): fonts.ss chooses its faces among Tk's families, constants.ss makes
    # %logo-font%, and the views install what the model reads
    fonts.load()
    K.load()
    hosts.set_window_host_maker(hosts.tk_host_maker(root))
    K.set_window_size_defaults(scale)
    fonts.create_mcat_logo(root)
    views.load_views()
    gui.load()
    sg = views._gui_module("slipnet_graphics")
    tg = views._gui_module("theme_graphics")
    S.g_workspace_window = views._gui_module("workspace_graphics").make_workspace_window()
    S.g_slipnet_window = sg.make_slipnet_window(sg.g_13x5_layout_table)
    S.g_coderack_window = views._gui_module("coderack_graphics").make_coderack_window()
    S.g_themespace_window = tg.make_themespace_window(tg.g_themespace_window_layout)
    S.g_top_themes_window = tell(S.g_themespace_window, "get-window", "top-bridge")
    S.g_bottom_themes_window = tell(S.g_themespace_window, "get-window",
                                        "bottom-bridge")
    S.g_vertical_themes_window = tell(S.g_themespace_window, "get-window",
                                          "vertical-bridge")
    S.g_memory_window = views._gui_module("memory_graphics").make_memory_window()
    S.g_comment_window = views._gui_module("commentary_graphics").make_comment_window()
    S.g_trace_window = views._gui_module("trace_graphics").make_trace_window()
    S.g_temperature_window = views._gui_module("temperature_graphics").make_temperature_window()
    S.g_EEG_window = views._gui_module("eeg_graphics").make_EEG_window()
    # port: the windows' places (the window manager chose them), first for an
    # estimated control panel size
    arrange_windows((400, 230))
    S.g_control_panel = gui.make_control_panel()
    # port: again, now that the control panel's size is known
    frame = tell(S.g_control_panel, "get-widgets")["frame"]
    frame.geometry("+0+0")
    root.update_idletasks()
    arrange_windows((frame.winfo_reqwidth(), frame.winfo_reqheight()))
    # port: the engine thread stands for the REPL thread
    S.g_repl_thread = EngineThread()
    views.set_thread_break_handler(gui.thread_break)
    enable_resizing()
    # port: SWL 0.9x's waiter prompt workaround is left out
    chez.printf("done~%")
    sys.stdout.flush()


def enable_resizing():
    """setup.ss: enable-resizing"""
    from metacat.gui import general_graphics
    tell(S.g_workspace_window, "make-resizable", "workspace")
    tell(S.g_slipnet_window, "make-resizable", "slipnet")
    tell(S.g_coderack_window, "make-resizable", "coderack")
    tell(S.g_temperature_window, "make-resizable", "temperature")
    tell(S.g_themespace_window, "make-resizable", "theme")
    tell(S.g_trace_window, "make-resizable", "trace")
    tell(S.g_memory_window, "make-resizable", "memory")
    tell(S.g_EEG_window, "make-resizable", "EEG")
    tell(S.g_comment_window, "make-resizable", "comment")
    general_graphics.start_resize_listener()


def main(argv=None):
    """port: python3 -m metacat.gui [SCALE] (or metacat-gui [SCALE] once installed):
    setup, then Tk's main loop, as typing (setup) after loading metacat.ss did"""
    if argv is None:
        argv = sys.argv[1:]
    scale = 1
    if argv:
        n = chez.string_to_number(argv[0])
        scale = n if n is not False else 1
    setup(scale)
    g_root.mainloop()
    return 0

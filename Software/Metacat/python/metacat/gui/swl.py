"""The SWL stand-ins the graphics files use: Tcl evaluation on a Tk canvas.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).

SWL (the Scheme Widget Library) gave the original ``swl:tcl-eval``, which sends one
Tcl command to a widget, ``swl:tcl->scheme`` and ``swl:sync-display``.  Here a
"window" is any object with a ``tcl(*args)`` method, which takes the arguments as
the original passes them (symbols, numbers, Scheme strings, colours, fonts).
``TkCanvas`` is the real one: it turns each argument into a Tcl word
(``tcl_word``) and calls the tkinter Canvas's widget command, so the original's
canvas commands reach Tk unchanged.  The tests use a recording window and compare
its commands with the oracle's (python/oracle/sgl-tcl.ss).  This module does not
import tkinter; ``TkCanvas`` is given a tkinter widget.
"""
from __future__ import annotations

from fractions import Fraction

from metacat import chez

# set by the GUI to its tkinter root, for swl:sync-display
g_tk_root = False


class ThreadSafeTk:
    """port: the Tcl interpreter of the GUI, callable from any thread.

    SWL serialized every Tk call through its own event thread.  Here the engine
    runs in its own thread (gui/app.py) and draws, and Tcl may only be touched
    from the thread that created it: a call made elsewhere is queued, the main
    thread is woken through a pipe that Tk's event loop watches, runs the call,
    and hands back the result, made of plain Python values (tkinter's own
    cross-thread calls let Tcl objects die in the calling thread, which crashed
    under Xvfb: docs/anomalies_and_quirks.md).  In the main thread a call goes
    straight to Tcl.  Installed as the root's .tk before any widget exists, so
    every widget shares it."""

    def __init__(self, tkapp):
        import os
        import queue
        import threading
        import tkinter
        self._tkapp = tkapp
        self._main = threading.get_ident()
        self._queue = queue.SimpleQueue()
        self._r, self._w = os.pipe()
        os.set_blocking(self._r, False)
        tkapp.createfilehandler(self._r, tkinter.READABLE, self._serve)

    def _serve(self, fd, mask):
        import os
        try:
            os.read(self._r, 4096)
        except BlockingIOError:
            pass
        while True:
            try:
                fn, args, box, done = self._queue.get_nowait()
            except Exception:   # noqa: BLE001 - queue.Empty
                return
            try:
                box.append((True, _plain(fn(*args))))
            except BaseException as e:   # noqa: BLE001 - re-raised in the caller
                box.append((False, e))
            done.set()

    def _marshal(self, name):
        import os
        import threading
        method = getattr(self._tkapp, name)

        def call(*args):
            if threading.get_ident() == self._main:
                return method(*args)
            box = []
            done = threading.Event()
            self._queue.put((method, args, box, done))
            os.write(self._w, b"x")
            done.wait()
            ok, value = box[0]
            if ok:
                return value
            raise value
        return call

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        value = getattr(self._tkapp, name)
        if callable(value) and name not in ("mainloop", "dooneevent", "quit"):
            value = self._marshal(name)
        setattr(self, name, value)
        return value


def _plain(x):
    """port: a Tcl result as plain Python values (no Tcl objects)"""
    if isinstance(x, tuple):
        return tuple(_plain(a) for a in x)
    if isinstance(x, (str, int, float, bool, bytes)) or x is None:
        return x
    return str(x)


def swl_tcl_eval(win, *args):
    """port: SWL's swl:tcl-eval (one Tcl command to a widget)"""
    return win.tcl(*args)


def swl_tcl_to_scheme(x):
    """port: SWL's swl:tcl->scheme.  Windows already answer Scheme values (lists,
    numbers, Scheme strings), so this is the identity."""
    return x


def swl_sync_display():
    """port: SWL's swl:sync-display (flush Tk's pending display updates)"""
    if g_tk_root:
        g_tk_root.update_idletasks()


def tcl_word(x):
    """port: one argument of a Tcl command, as tkinter passes it to Tk.

    A colour becomes #rrggbb, an SWL font Tk's font description (face size style...),
    a list a Tcl list.  An exact ratio becomes a float: the original's text items
    sit at half pixels (draw-text's (* 1/2 width)), and Tk reads coordinates as
    doubles (docs/anomalies_and_quirks.md, "Text items at half pixels")."""
    if isinstance(x, Fraction):
        return float(x)
    if isinstance(x, (list, tuple)):
        return tuple(tcl_word(a) for a in x)
    if isinstance(x, str):
        return str(x)
    if isinstance(x, (int, float)):
        return x
    to_tcl = getattr(x, "tcl_word", None)
    if to_tcl is not None:
        return to_tcl()
    from metacat.gui.colors import Rgb
    if isinstance(x, Rgb):
        return "#%02x%02x%02x" % (x.r, x.g, x.b)
    raise TypeError("no Tcl word for %r" % (x,))


def _from_tcl(x):
    if isinstance(x, tuple):
        return [_from_tcl(a) for a in x]
    if isinstance(x, str):
        return chez.String(x)
    return x


class TkCanvas:
    """port: an SWL <canvas>: a tkinter Canvas that takes the original's commands."""

    def __init__(self, widget, background=None):
        from metacat.gui.colors import c_white
        self.widget = widget
        self.background = background if background is not None else c_white
        widget.configure(background=tcl_word(self.background))

    def tcl(self, *args):
        """port: the canvas's widget command"""
        return _from_tcl(self.widget.tk.call(self.widget._w, *[tcl_word(a) for a in args]))

    def get_background_color(self):
        """port: SWL <canvas> get-background-color"""
        return self.background

    def set_background_color_bang(self, color):
        """port: SWL <canvas> set-background-color!"""
        self.background = color
        self.widget.configure(background=tcl_word(color))

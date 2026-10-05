"""The engine thread and the GUI thread of the Qt GUI.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) for loop0003 item 05, after
docs/qt-gui-plan.md 2.5 and metacat/gui/app.py's EngineThread and
metacat/gui/swl.py's ThreadSafeTk.

The threads are the tkinter GUI's, with Qt's GUI thread for Tk's main thread:

- the GUI thread runs QApplication.exec() and owns every widget and scene;
- the engine thread is metacat.gui.app.EngineThread, reused as is: the
  control panel hands it thunks with gui.thread_break (SWL's thread-break),
  and run.toplevel parks the run in it so that Go resumes it, even inside a
  codelet;
- the resize listener runs the panels' resize methods.

Canvas commands need no bridge: the Qt canvas's display list takes them in
the calling thread and answers bbox and measurement there, and the main
window's 50 ms timer brings the scenes up to date (metacat/qt/canvas.py).
What does need one is the control panel's widgets: `GuiInvoker.post` runs a
callable on the GUI thread later (a queued signal) and returns at once, which
is how a message from the engine reaches a widget; `call` runs it there and
waits for its value, for drivers and tests.  The GUI thread never waits for
another thread: on it, post and call run their callable directly.
"""
from __future__ import annotations

import threading
import traceback

from PySide6.QtCore import QObject, Qt, Signal

from metacat.qt.hosts import on_gui_thread


class GuiInvoker(QObject):
    """callables run on the GUI thread (make it on the GUI thread)"""

    _queued = Signal(object)

    def __init__(self):
        super().__init__()
        self._queued.connect(self._run, Qt.QueuedConnection)

    @staticmethod
    def _run(fn):
        try:
            fn()
        except Exception:   # noqa: BLE001 - as Tk reports an error in a callback
            traceback.print_exc()

    def post(self, fn):
        """run fn on the GUI thread: at once there, later from another thread"""
        if on_gui_thread():
            fn()
        else:
            self._queued.emit(fn)

    def call(self, fn, timeout=None):
        """fn's value, computed on the GUI thread (its exception re-raised);
        TimeoutError if the GUI thread has not run it after timeout seconds"""
        if on_gui_thread():
            return fn()
        box = []
        done = threading.Event()

        def run():
            try:
                box.append((True, fn()))
            except BaseException as e:   # noqa: BLE001 - re-raised in the caller
                box.append((False, e))
            done.set()
        self._queued.emit(run)
        if not done.wait(timeout):
            raise TimeoutError("the GUI thread did not answer")
        ok, value = box[0]
        if not ok:
            raise value
        return value


class EngineBridge:
    """the engine thread of the Qt GUI (setup.ss's REPL thread), and the
    invoker its messages reach the widgets with"""

    def __init__(self):
        from metacat import setup
        from metacat.gui import app as gui_app
        from metacat.gui import gui, views
        self.invoker = GuiInvoker()
        self.thread = gui_app.EngineThread()
        setup.g_repl_thread = self.thread
        # the Workspace's click handler resumes the run through it
        views.set_thread_break_handler(gui.thread_break)

    def busy(self):
        """is the engine running a thunk (or has one waiting)?"""
        return self.thread.busy_p()

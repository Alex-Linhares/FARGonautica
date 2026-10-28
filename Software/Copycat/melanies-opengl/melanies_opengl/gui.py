"""Hager's pyglet window, imported unchanged from ../copycat-opengl, over a TraceRun."""

import sys

from . import COPYCAT_OPENGL

if COPYCAT_OPENGL not in sys.path:
    sys.path.insert(0, COPYCAT_OPENGL)     # for `clients` (and the `copycat` it imports)

import pyglet                              # noqa: E402
from clients.opengl import Window          # noqa: E402  Hager's window, GPL v2

CAPTION = "Copycat: Melanie Mitchell's original (SBCL), seen through Hager's GUI"


class MelaniesWindow(Window):
    """Hager's Window with a caption saying what it shows. Everything drawn is his code
    reading our adapter objects; Space runs and pauses, Escape quits."""

    def __init__(self, run, speed=None):
        Window.__init__(self, run)
        self.set_caption(CAPTION)
        if speed:
            self.speed = speed

    def on_close(self):
        lisp = getattr(self.run, "lisp", None)
        if lisp is not None:
            lisp.close()
        return Window.on_close(self)


def show(run, speed=None):
    window = MelaniesWindow(run, speed)
    pyglet.app.run()
    return window

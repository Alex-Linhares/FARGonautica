"""Painting now: the every-event redraw, at one window paint per event.

In Qt 6 a widget's repaint() paints every dirty widget of its window and
flushes the window, so the views calling repaint() one after another
painted the window five times an event; and a QGraphicsScene finds its
dirty items in a queued call, so a view repainted at once was marked dirty
again by it, and painted a second time at the next repaint (measured:
the tree view 2.5 paints an event).

So the window asks each view to redraw(now=False), which only marks it
dirty (update()), then calls paint_all(scenes): the scenes' queued dirty
passes run first, then every pending window update is painted, at once.
Each view is painted exactly once, before the event is acknowledged.  A
view's own redraw() (now=True) does the same for itself.  (The scene's
dirty pass sometimes paints the view itself, synchronously, and sometimes
only marks it; either way the flush after it leaves exactly one paint.)
"""

from PySide6.QtCore import QCoreApplication, QEvent

__all__ = ["settle", "paint_all"]


def settle(scene):
    """Run SCENE's queued dirty-item pass now (it marks the views' dirty
    regions), so that it doesn't mark them again after they are painted."""
    QCoreApplication.sendPostedEvents(scene, QEvent.Type.MetaCall)


def paint_all(scenes=()):
    """Paint every dirty widget now, in one pass per window."""
    for scene in scenes:
        settle(scene)
    QCoreApplication.sendPostedEvents(None, QEvent.Type.UpdateRequest)

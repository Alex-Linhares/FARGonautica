"""Painting now: the every-event redraw, at one window paint per event (Numbo GUI,
`paint.py`, adopted whole through the Seek-Whence GUI's; ANSWERS.md 5).

In Qt 6 a widget's `repaint()` paints every dirty widget of its window and flushes the
window, so views calling `repaint()` one after another would paint the window once per
view per event; and a `QGraphicsScene` finds its dirty items in a queued call, so a view
repainted at once is marked dirty again by it and painted a second time at the next
repaint. So the window asks each view to `redraw(now=False)`, which only marks it dirty
(`update()`), then calls `paint_all(scenes)`: the scenes' queued dirty passes run first,
then every pending window update is painted, at once. Each view is painted exactly once,
before the event is acknowledged (tested with a counting probe, as the Numbo GUI's).
"""

from PySide6.QtCore import QCoreApplication, QEvent

__all__ = ["settle", "paint_all"]


def settle(scene):
    """Run the scene's queued dirty-item pass now (it marks the views' dirty regions), so
    that it does not mark them again after they are painted (Numbo GUI, `paint.py`)."""
    QCoreApplication.sendPostedEvents(scene, QEvent.Type.MetaCall)


def paint_all(scenes=()):
    """Paint every dirty widget now, in one pass per window (Numbo GUI, `paint.py`)."""
    for scene in scenes:
        settle(scene)
    QCoreApplication.sendPostedEvents(None, QEvent.Type.UpdateRequest)

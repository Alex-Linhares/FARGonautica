"""Render a QGraphicsView's whole scene to a PNG."""

from PySide6.QtCore import QRectF
from PySide6.QtGui import QImage, QPainter
from PySide6.QtWidgets import QGraphicsItem

__all__ = ["render_scene_png"]


def render_scene_png(view, path, scale=1.0):
    """Render VIEW's scene rect (at SCALE) to a PNG at PATH; return the
    QImage.  Item caches are off while it renders: a DeviceCoordinateCache
    pixmap kept for a QImage device is not invalidated by update(), so a
    second render would show the first one's items."""
    scene = view.scene()
    rect = scene.sceneRect()
    image = QImage(max(1, round(rect.width() * scale)), max(1, round(rect.height() * scale)),
                   QImage.Format.Format_ARGB32)
    image.fill(view.backgroundBrush().color())
    cached = [(item, item.cacheMode()) for item in scene.items()
              if item.cacheMode() != QGraphicsItem.CacheMode.NoCache]
    for item, _ in cached:
        item.setCacheMode(QGraphicsItem.CacheMode.NoCache)
    try:
        painter = QPainter(image)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.TextAntialiasing)
        scene.render(painter, QRectF(image.rect()), rect)
        painter.end()
    finally:
        for item, mode in cached:
            item.setCacheMode(mode)
    if not image.save(path):
        raise OSError(f"could not write {path}")
    return image

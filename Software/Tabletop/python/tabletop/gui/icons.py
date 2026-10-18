"""Painting the object icons with QPainter, in the four orientations (PANES.md section
6; ANSWERS.md 4; G-016).

The shapes are the Qt-free primitives of `models/icon_shapes.py`, new drawings in the
spirit of French's palette, Figure 2.1 [F95 p. 26 (img)], never his bitmaps. This module
turns them into QPainter calls: `paint_icon` draws one icon centred on the painter's
origin, turned to its orientation by `ROTATION` (the four of `objects.ORIENTATIONS`
[F95 p. 55]), in a box of the type's footprint (D-002) at the caller's scale;
`icon_pixmap` renders one offscreen, for a palette, a legend or the guide. The GUI's
table (`table_view.py`) and the README's figures (`scripts/docs_figures.py`) both paint
through `paint_icon`, so there is one set of icons (ANSWERS.md 4). The colours are the
README's figures': a metal grey, white china, a pale blue glass, a dark handle and a
darker pepper shaker, with a dark outline whose width `icon_shapes.pen_width` gives.
"""

from PySide6.QtCore import QPointF, QRectF, Qt
from PySide6.QtGui import QBrush, QColor, QPainter, QPainterPath, QPen, QPixmap, QPolygonF

from tabletop.gui.models import icon_shapes
from tabletop.gui.models.icon_shapes import ROTATION
from tabletop.objects import footprint

__all__ = ["EDGE", "FILL_COLORS", "NOT_FRENCHS", "ROTATION", "icon_pixmap", "paint_icon",
           "paint_primitives"]

NOT_FRENCHS = "icons: new drawings, not French's"       # the legend's words (ANSWERS.md 4)

EDGE = "#3b3a37"
# One colour per fill role of `icon_shapes.FILL_ROLES` (G-016).
FILL_COLORS = {"metal": "#d9d7d0", "white": "#ffffff", "glass": "#e3eef9", "dark": "#3b3a37",
               "pepper": "#8f8a80"}
assert set(FILL_COLORS) == set(icon_shapes.FILL_ROLES)


def _point(p) -> QPointF:
    return QPointF(p[0], p[1])


def _rect(points) -> QRectF:
    (left, top), (right, bottom) = points
    return QRectF(left, top, right - left, bottom - top)


def _path(segments) -> QPainterPath:
    path = QPainterPath()
    for segment in segments:
        kind = segment[0]
        if kind == "move":
            path.moveTo(_point(segment[1]))
        elif kind == "line":
            path.lineTo(_point(segment[1]))
        elif kind == "quad":
            path.quadTo(_point(segment[1]), _point(segment[2]))
        elif kind == "close":
            path.closeSubpath()
    return path


def paint_primitives(painter: QPainter, prims, pen: QPen, fills=FILL_COLORS) -> None:
    """Draw primitives (`icon_shapes.Primitive`) with the painter as it stands: `pen` for
    the outlines, `fills` colour by role (G-016)."""
    for p in prims:
        painter.setPen(pen if p.stroked else Qt.PenStyle.NoPen)
        painter.setBrush(QBrush(QColor(fills[p.fill])) if p.fill else Qt.BrushStyle.NoBrush)
        if p.kind == "rect":
            painter.drawRoundedRect(_rect(p.points), p.radius, p.radius)
        elif p.kind == "ellipse":
            painter.drawEllipse(_rect(p.points))
        elif p.kind == "polygon":
            painter.drawPolygon(QPolygonF([_point(q) for q in p.points]))
        elif p.kind == "path":
            painter.drawPath(_path(p.segments))
        elif p.kind == "line":
            painter.drawLine(_point(p.points[0]), _point(p.points[1]))
        elif p.kind == "dot":
            painter.drawEllipse(_point(p.points[0]), p.radius, p.radius)


def paint_icon(painter: QPainter, type_: str, w: float, h: float, orientation=None,
               edge: str = EDGE, fills=FILL_COLORS) -> None:
    """Draw the icon of `type_` centred on the painter's origin in a box `w` by `h` as
    drawn pointing up, turned to `orientation` (`ROTATION`; the box then stands `h` by
    `w` for `left` and `right`) [F95 p. 26 (img)] (D-002, G-016). The painter's
    transform, pen and brush are restored."""
    pen = QPen(QColor(edge), icon_shapes.pen_width(w, h))
    pen.setJoinStyle(Qt.PenJoinStyle.RoundJoin)
    pen.setCapStyle(Qt.PenCapStyle.RoundCap)
    painter.save()
    painter.rotate(ROTATION[orientation])
    paint_primitives(painter, icon_shapes.primitives(type_, w, h), pen, fills)
    painter.restore()


def icon_pixmap(type_: str, orientation=None, height: int = 40, margin: int = 3) -> QPixmap:
    """The icon of `type_` rendered offscreen into a transparent pixmap `height` pixels
    tall and at most as wide (the box as it lies, `objects.footprint`, scaled to fit a
    square of that side; G-016), for a palette or a legend captioned "new drawings, not
    French's" (ANSWERS.md 4)."""
    fw, fh = footprint(type_)                       # the box pointing up, in units
    lw, lh = footprint(type_, orientation)          # as it lies
    scale = (height - 2 * margin) / max(lw, lh)
    pixmap = QPixmap(max(1, round(lw * scale)) + 2 * margin, height)
    pixmap.fill(Qt.GlobalColor.transparent)
    painter = QPainter(pixmap)
    painter.setRenderHint(QPainter.RenderHint.Antialiasing)
    painter.translate(pixmap.width() / 2, pixmap.height() / 2)
    paint_icon(painter, type_, fw * scale, fh * scale, orientation)
    painter.end()
    return pixmap

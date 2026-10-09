"""Draw ops (seqsee/gui/draw/ops.py) → QGraphicsScene items, drawn the way Tk 804 draws its
canvas items.

One top-level item per op, stacked in op order (``zValue`` = index): lines and smooth
polygons and arcs are QGraphicsPathItems, rectangles QGraphicsRectItems, ovals
QGraphicsEllipseItems, plain polygons QGraphicsPolygonItems and texts ``TextItem``s. A line's
arrowheads are child polygon items. Each item carries its op (``item.data(OP_ROLE)``) and its
tags (``item.data(TAGS_ROLE)``), so a widget can map a click back to a tagged object.

The Tk geometry is reproduced by small pure functions: ``arrow_geometry`` (tkCanvLine.c's
ConfigureArrows: the arrowhead polygon, and the line end moved back to its neck),
``bezier_segments`` (tkTrig.c's TkMakeBezierCurve, for ``-smooth``), ``dash_lengths``
(tkCanvUtil.c's DashConvert, for ``-dash``) and ``text_origin`` (tkCanvText.c's
ComputeTextBbox, for ``-anchor``).

Colours go through ``colors.to_hex`` (X11 names; never ``QColor(name)``, whose SVG names
differ). Fonts: X11 names (``-adobe-helvetica-bold-r-normal--20-…``) and Tk font strings
("Helvetica -12", "{Times} 12 italic") map to the nearest QFont, sized in pixels; Tk point
sizes are taken at 96 dpi (as ``workspace.approx_measure`` does). Stipples are Tk's built-in
gray12/25/50/75 bitmaps as texture brushes. ``QtMeasure`` gives the drawing code
``(width, linespace)`` from QFontMetrics, so measured layouts (the workspace's metonym cross)
fit the text Qt draws.
"""
import dataclasses
import functools
import math
import re

from PySide6.QtCore import QPointF, QRectF, Qt
from PySide6.QtGui import (QBrush, QColor, QFont, QFontMetrics, QImage, QPainter,
                           QPainterPath, QPen, QPolygonF)
from PySide6.QtWidgets import (QGraphicsEllipseItem, QGraphicsItem, QGraphicsPathItem,
                               QGraphicsPolygonItem, QGraphicsRectItem, QGraphicsScene)

from seqsee.gui.draw import colors, ops

OP_ROLE = 0
TAGS_ROLE = 1

POINTS_TO_PIXELS = 96 / 72


# --- colours, pens, brushes ----------------------------------------------------------------

def qcolor(spec):
    """The QColor of a Tk colour spec, or None if unset."""
    hexed = colors.to_hex(spec)
    return QColor(hexed) if hexed else None


# Tk's built-in stipple bitmaps (X11 gray*.xbm), one period each; 1 = drawn.
_STIPPLES = {
    "gray75": ("1011", "1110"),
    "gray50": ("1010", "0101"),
    "gray25": ("0001", "0100"),
    "gray12": ("0001", "0000", "0100", "0000"),
}


def _brush(colour, stipple=None):
    c = qcolor(colour)
    if c is None:
        return QBrush(Qt.NoBrush)
    rows = _STIPPLES.get(stipple) if stipple else None
    if not rows:
        return QBrush(c)
    img = QImage(len(rows[0]), len(rows), QImage.Format_ARGB32)
    img.fill(Qt.transparent)
    for y, row in enumerate(rows):
        for x, bit in enumerate(row):
            if bit == "1":
                img.setPixelColor(x, y, c)
    return QBrush(img)


_CAPS = {"butt": Qt.FlatCap, "round": Qt.RoundCap, "projecting": Qt.SquareCap}
_JOINS = {"round": Qt.RoundJoin, "miter": Qt.MiterJoin, "bevel": Qt.BevelJoin}


def _pen(colour, width, dash=None, capstyle="butt", joinstyle="miter", stipple=None):
    if qcolor(colour) is None:
        return QPen(Qt.NoPen)
    pen_width = max(float(width or 0), 1.0)     # Tk draws a width-0 outline 1 px wide
    pen = QPen(_brush(colour, stipple), pen_width)
    pen.setCapStyle(_CAPS.get(capstyle, Qt.FlatCap))
    pen.setJoinStyle(_JOINS.get(joinstyle, Qt.MiterJoin))
    lengths = dash_lengths(dash, width)
    if lengths:
        pen.setDashPattern([v / pen_width for v in lengths])    # Qt counts in pen widths
    return pen


def dash_lengths(dash, width):
    """The on/off pixel lengths of a Tk ``-dash`` value, or None for a solid line.

    A string is DashConvert's: '_' 8, '-' 6, ',' 4, '.' 2 dash units, each followed by a
    4-unit gap, where a unit is the line width rounded (at least 1); a space widens the
    previous gap by width + 1. A list gives pixel lengths; an odd list repeats."""
    if dash is None or dash == "" or dash == ():
        return None
    if isinstance(dash, str) and not re.fullmatch(r"[\d\s]+", dash):
        unit = max(int(float(width or 0) + 0.5), 1)
        sizes = {"_": 8, "-": 6, ",": 4, ".": 2}
        out = []
        for ch in dash:
            if ch == " ":
                if out:
                    out[-1] += unit + 1
                continue
            if ch not in sizes:
                return None
            out += [sizes[ch] * unit, 4 * unit]
        return out or None
    values = dash.split() if isinstance(dash, str) else dash
    out = [int(v) for v in values]
    if len(out) % 2:
        out += out
    return out or None


# --- geometry ---------------------------------------------------------------------------

def arrow_geometry(prev, tip, width, shape):
    """ConfigureArrows for one end: ``(new_end, polygon)``. ``polygon`` is Tk's five
    arrowhead points (tip, wing, neck, neck, wing); the line itself ends at ``new_end``,
    backed off from the tip so that its cap hides inside the head."""
    a, b, c = (float(v) for v in shape)
    frac = (float(width or 0) / 2.0) / c if c else 0.0
    backup = frac * b + a * (1.0 - frac) / 2.0
    dx, dy = tip[0] - prev[0], tip[1] - prev[1]
    length = math.hypot(dx, dy)
    if length == 0:
        sin_t = cos_t = 0.0
    else:
        sin_t, cos_t = dy / length, dx / length
    vx, vy = tip[0] - a * cos_t, tip[1] - a * sin_t
    temp = c * sin_t
    p2x = tip[0] - b * cos_t + temp
    p8x = p2x - 2 * temp
    temp = c * cos_t
    p3y = tip[1] - b * sin_t - temp
    p9y = p3y + 2 * temp
    polygon = [
        (tip[0], tip[1]),
        (p2x, p3y),
        (p2x * frac + vx * (1 - frac), p3y * frac + vy * (1 - frac)),
        (p8x * frac + vx * (1 - frac), p9y * frac + vy * (1 - frac)),
        (p8x, p9y),
    ]
    new_end = (tip[0] - backup * cos_t, tip[1] - backup * sin_t)
    return new_end, polygon


def _mix(p, q, wp):
    return (p[0] * wp + q[0] * (1 - wp), p[1] * wp + q[1] * (1 - wp))


def bezier_segments(points):
    """TkMakeBezierCurve as cubic segments ``(start, control1, control2, end)``: a
    parabolic spline through the midpoints of the segments, starting and ending at the end
    points of an open curve. A curve whose first point equals its last is closed. Fewer
    than three points give no curve."""
    pts = [tuple(p) for p in points]
    n = len(pts)
    if n < 3:
        return []
    closed = pts[0] == pts[-1]
    triples = [(pts[i], pts[i + 1], pts[i + 2], i == 0 and not closed,
                i == n - 3 and not closed) for i in range(n - 2)]
    if closed:
        triples.insert(0, (pts[n - 2], pts[0], pts[1], False, False))
    segs = []
    for a, b, c, first, last in triples:
        start = a if first else _mix(a, b, 0.5)
        c1 = _mix(a, b, 1 / 3) if first else _mix(a, b, 1 / 6)
        c2 = _mix(b, c, 2 / 3) if last else _mix(b, c, 5 / 6)
        end = c if last else _mix(b, c, 0.5)
        segs.append((start, c1, c2, end))
    return segs


def _tk_round(v):
    return int(math.floor(v + 0.5))


def text_origin(anchor, x, y, width, height):
    """ComputeTextBbox: the top-left corner of a ``width`` × ``height`` text placed at
    (x, y) with ``anchor`` (Tk rounds the point, then takes integer halves)."""
    left, top = _tk_round(x), _tk_round(y)
    width, height = int(width), int(height)
    if anchor in ("w", "center", "e"):
        top -= height // 2
    elif anchor in ("sw", "s", "se"):
        top -= height
    if anchor in ("n", "center", "s"):
        left -= width // 2
    elif anchor in ("ne", "e", "se"):
        left -= width
    return left, top


def _pairs(coords):
    return [(float(coords[i]), float(coords[i + 1])) for i in range(0, len(coords) - 1, 2)]


def _path(points, smooth):
    path = QPainterPath()
    if not points:
        return path
    segs = bezier_segments(points) if smooth else []
    if segs:
        path.moveTo(*segs[0][0])
        for _, c1, c2, end in segs:
            path.cubicTo(QPointF(*c1), QPointF(*c2), QPointF(*end))
        return path
    path.moveTo(*points[0])
    for p in points[1:]:
        path.lineTo(*p)
    return path


# --- fonts ----------------------------------------------------------------------------

@dataclasses.dataclass(frozen=True)
class FontSpec:
    """A parsed Tk font: ``size`` in pixels if ``pixels`` else in points."""
    family: str
    size: float
    pixels: bool
    bold: bool = False
    italic: bool = False
    underline: bool = False
    overstrike: bool = False


def parse_font(spec):
    """Parse an X11 font name or a Tk font string (family, size: negative = pixels,
    positive = points, styles). None is Tk's default, "Helvetica -12"."""
    spec = (spec or ops.DEFAULT_FONT).strip()
    if spec.startswith("-"):
        f = spec.split("-")
        if len(f) >= 9:
            family = f[2].lower() or "helvetica"
            bold = f[3].lower() in ("bold", "demibold", "black", "heavy")
            italic = f[4].lower() in ("i", "o")
            if f[7].isdigit() and int(f[7]) > 0:
                return FontSpec(family, int(f[7]), True, bold, italic)
            if f[8].isdigit() and int(f[8]) > 0:
                return FontSpec(family, int(f[8]) / 10, False, bold, italic)
            return FontSpec(family, 12, True, bold, italic)
    tokens = re.findall(r"\{[^}]*\}|\S+", spec)
    family = tokens[0].strip("{}").lower() if tokens else "helvetica"
    size, pixels = 12, True
    styles = set()
    for tok in tokens[1:]:
        if re.fullmatch(r"-?\d+(\.\d+)?", tok):
            v = float(tok)
            v = int(v) if v == int(v) else v
            if v < 0:
                size, pixels = -v, True
            elif v > 0:
                size, pixels = v, False
        else:
            styles.add(tok.lower())
    return FontSpec(family, size, pixels, "bold" in styles, "italic" in styles,
                    "underline" in styles, "overstrike" in styles)


_SERIF = ("times", "serif", "georgia", "new century schoolbook", "palatino")
_MONO = ("courier", "fixed", "mono", "monospace", "typewriter", "terminal")


@functools.lru_cache(maxsize=None)
def qfont(spec):
    """The QFont nearest to a Tk font (cached per spec: don't modify the result)."""
    fs = parse_font(spec)
    font = QFont()
    if any(w in fs.family for w in _MONO):
        font.setStyleHint(QFont.TypeWriter)
        font.setFamily("Monospace" if fs.family == "fixed" else fs.family.title())
    elif any(w in fs.family for w in _SERIF):
        font.setStyleHint(QFont.Serif)
        font.setFamily(fs.family.title())
    else:
        font.setStyleHint(QFont.SansSerif)
        font.setFamily(fs.family.title())
    px = fs.size if fs.pixels else fs.size * POINTS_TO_PIXELS
    font.setPixelSize(max(1, _tk_round(px)))
    font.setBold(fs.bold)
    font.setItalic(fs.italic)
    font.setUnderline(fs.underline)
    font.setStrikeOut(fs.overstrike)
    return font


@functools.lru_cache(maxsize=None)
def _metrics(spec):
    return QFontMetrics(qfont(spec))


class QtMeasure:
    """``measure(text, font) -> (width, linespace)`` in whole pixels from QFontMetrics, for
    the drawing functions' ``measure`` argument (multi-line text: the widest line, and one
    linespace per line). Needs a QGuiApplication."""

    def __call__(self, text, font):
        fm = _metrics(font)
        lines = ("" if text is None else str(text)).split("\n")
        return max(fm.horizontalAdvance(line) for line in lines), fm.height() * len(lines)


# --- items ----------------------------------------------------------------------------

class TextItem(QGraphicsItem):
    """A Tk text item: lines of ``text`` in one font and colour, placed by ``anchor`` and
    aligned by ``justify``; ``width`` > 0 wraps lines at that many pixels."""

    def __init__(self, op, parent=None):
        super().__init__(parent)
        self._op = op
        self._text = "" if op.text is None else str(op.text)
        self._font = qfont(op.font)
        self._color = qcolor(op.fill)
        fm = _metrics(op.font)
        lines = self._text.split("\n")
        if op.width and float(op.width) > 0:
            lines = _wrap(lines, fm, float(op.width))
        self._lines = lines
        self._widths = [fm.horizontalAdvance(line) for line in lines]
        self._linespace = fm.height()
        self._ascent = fm.ascent()
        w = max(self._widths)
        h = self._linespace * len(lines)
        left, top = text_origin(op.anchor, op.coords[0], op.coords[1], w, h)
        self._rect = QRectF(left, top, w, h)

    def text(self):
        return self._text

    def lines(self):
        return list(self._lines)

    def font(self):
        return self._font

    def color(self):
        return self._color

    def anchor(self):
        return self._op.anchor

    def justify(self):
        return self._op.justify

    def line_positions(self):
        """The top-left corner of each line."""
        out = []
        for k, lw in enumerate(self._widths):
            free = self._rect.width() - lw
            dx = {"center": free / 2, "right": free}.get(self._op.justify, 0)
            out.append((self._rect.left() + dx, self._rect.top() + k * self._linespace))
        return out

    def boundingRect(self):
        return QRectF(self._rect)

    def paint(self, painter, option, widget=None):
        if self._color is None:
            return
        painter.setFont(self._font)
        painter.setPen(QPen(_brush(self._op.fill, self._op.stipple), 1))
        for line, (x, y) in zip(self._lines, self.line_positions()):
            painter.drawText(QPointF(x, y + self._ascent), line)


def _wrap(lines, fm, width):
    out = []
    for line in lines:
        cur = ""
        for word in re.findall(r"\S+\s*", line) or [""]:
            if cur and fm.horizontalAdvance((cur + word).rstrip()) > width:
                out.append(cur.rstrip())
                cur = word
            else:
                cur += word
        out.append(cur.rstrip())
    return out


def _line_item(op):
    points = _pairs(op.coords)
    heads = []
    if len(points) >= 2 and op.arrow in ("first", "both"):
        points[0], head = arrow_geometry(points[1], points[0], op.width, op.arrowshape)
        heads.append(head)
    if len(points) >= 2 and op.arrow in ("last", "both"):
        points[-1], head = arrow_geometry(points[-2], points[-1], op.width, op.arrowshape)
        heads.append(head)
    item = QGraphicsPathItem(_path(points, op.smooth))
    item.setPen(_pen(op.fill, op.width, op.dash, op.capstyle, op.joinstyle, op.stipple))
    item.setBrush(QBrush(Qt.NoBrush))
    for head in heads:
        child = QGraphicsPolygonItem(QPolygonF([QPointF(*p) for p in head]), item)
        child.setPen(QPen(Qt.NoPen))
        child.setBrush(_brush(op.fill, op.stipple))
    return item


def _box(op):
    x1, y1, x2, y2 = (float(v) for v in op.coords[:4])
    return QRectF(x1, y1, x2 - x1, y2 - y1)


def _outline_pen(op, joinstyle="miter"):
    return _pen(op.outline, op.width, op.dash, "butt", joinstyle, op.outlinestipple)


def _rect_item(op):
    item = QGraphicsRectItem(_box(op))
    item.setPen(_outline_pen(op))
    item.setBrush(_brush(op.fill, op.stipple))
    return item


def _oval_item(op):
    item = QGraphicsEllipseItem(_box(op))
    item.setPen(_outline_pen(op))
    item.setBrush(_brush(op.fill, op.stipple))
    return item


def _polygon_item(op):
    points = _pairs(op.coords)
    if op.smooth and len(points) >= 3:
        if points[0] != points[-1]:
            points.append(points[0])              # Tk closes a polygon's outline
        item = QGraphicsPathItem(_path(points, True))
    else:
        item = QGraphicsPolygonItem(QPolygonF([QPointF(*p) for p in points]))
    item.setPen(_outline_pen(op, op.joinstyle))
    item.setBrush(_brush(op.fill, op.stipple))
    return item


def _arc_item(op):
    rect = _box(op)
    start, extent = float(op.start), float(op.extent)
    path = QPainterPath()
    if op.style == "pieslice":
        path.moveTo(rect.center())
        path.arcTo(rect, start, extent)
        path.closeSubpath()
    else:
        path.arcMoveTo(rect, start)
        path.arcTo(rect, start, extent)
        if op.style == "chord":
            path.closeSubpath()
    item = QGraphicsPathItem(path)
    item.setPen(_outline_pen(op))
    item.setBrush(QBrush(Qt.NoBrush) if op.style == "arc" else _brush(op.fill, op.stipple))
    return item


_MAKERS = {"line": _line_item, "rectangle": _rect_item, "oval": _oval_item,
           "polygon": _polygon_item, "arc": _arc_item, "text": TextItem}


def make_item(op):
    """The (parentless) QGraphicsItem for one op, carrying the op and its tags."""
    item = _MAKERS[op.TYPE](op)
    item.setData(OP_ROLE, op)
    item.setData(TAGS_ROLE, list(op.tags))
    return item


def add_ops(scene, draw_ops, z0=0):
    """Add one item per op to ``scene``, stacked in op order from ``z0``; return them."""
    items = []
    for k, op in enumerate(draw_ops):
        item = make_item(op)
        item.setZValue(z0 + k)
        scene.addItem(item)
        items.append(item)
    return items


def op_items(scene):
    """The items made from ops (not their arrowhead children), in stacking order.

    Tells them apart by their op data: in PySide6 6.11, calling ``parentItem()`` on a
    top-level item removes it from its scene."""
    items = [i for i in scene.items() if i.data(OP_ROLE) is not None]
    return sorted(items, key=lambda i: i.zValue())


def render(draw_ops, scene=None, width=None, height=None, background=None):
    """A scene showing ``draw_ops`` (a given scene is cleared first). ``width`` × ``height``
    sets the scene rectangle (the Tk canvas's size); ``background`` a Tk colour."""
    if scene is None:
        scene = QGraphicsScene()
    else:
        scene.clear()
    if width is not None and height is not None:
        scene.setSceneRect(0, 0, width, height)
    if background is not None:
        scene.setBackgroundBrush(_brush(background))
    add_ops(scene, draw_ops)
    return scene


def render_image(draw_ops, width, height, background="#FFFFFF"):
    """A ``width`` × ``height`` QImage of ``draw_ops`` on ``background`` (antialiased)."""
    scene = render(draw_ops, width=width, height=height)
    img = QImage(int(width), int(height), QImage.Format_ARGB32)
    img.fill(qcolor(background) or QColor(Qt.white))
    painter = QPainter(img)
    painter.setRenderHints(QPainter.Antialiasing | QPainter.TextAntialiasing)
    scene.render(painter, QRectF(0, 0, width, height), QRectF(0, 0, width, height))
    painter.end()
    scene.clear()
    return img


def save_png(draw_ops, path, width, height, background="#FFFFFF"):
    """Render ``draw_ops`` and save them as a PNG at ``path``."""
    if not render_image(draw_ops, width, height, background).save(str(path), "PNG"):
        raise OSError(f"could not write {path}")

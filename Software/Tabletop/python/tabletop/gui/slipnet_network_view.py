"""P3b Slipnet network, tabbed under the grid: the 63 nodes (D-080) in three bands by level, the
most abstract on top, drawn in Fig. 3.6's style [F95 p. 61 (img)]: circles for the nodes
with a dark disc for the activation, ISA links as thin arrows to the category, labeled
links as lines whose label nodes are tied to them by dashed stems, the identity links as
loops; the labeled links at their current effective lengths (the number beside the link,
the stroke thicker as the link shrinks; D-011, D-012), the label nodes highlighted while
they shorten links; the context push and pop drawn as their events say (the reset nodes
ringed while inside the subworld, the restored nodes ringed through the pop's codelet,
the banner along the top; [F95 pp. 87-89], D-021, D-070), and the banner "no context push
in this run" while there has been none (ANSWERS.md 11). GUI design, not French's (G-033
to G-035).

`SlipnetNetworkWidget` is a `QWidget` that paints `models/slipnet_network.py`'s
`network_layout`, a pure function of the model, the size and the selection, through a
cached pixmap rendered again only when the layout, the size or the pixel ratio differ
from the ones it was rendered for (Seek-Whence GUI, `platoplasm_view.py`; G-034).
Hovering a node or a link shows its hover text; a left click toggles the node in the
window's shared selection (G-022), so the grid's selection and this pane's are one.
`SlipnetNetworkView` is the pane, keeping the view protocol the window expects (G-011)
and taking the window's history and selection through `attach`.
"""

import math

from PySide6.QtCore import QPointF, QRectF, QSize, Qt
from PySide6.QtGui import QColor, QFont, QImage, QPainter, QPen, QPixmap, QPolygonF
from PySide6.QtWidgets import QHBoxLayout, QLabel, QSizePolicy, QVBoxLayout, QWidget

from tabletop import slipnet_def as sd
from tabletop.gui import paint
from tabletop.gui.models import slipnet_network as sn

__all__ = ["BACKGROUND", "DISC", "ISA", "LABEL", "LABELED", "MIN_SIZE", "POP", "PUSH",
           "SELECTED", "SHORTENED", "SlipnetNetworkView", "SlipnetNetworkWidget"]

MIN_SIZE = (150, 150)                # the smallest the pane may be (G-014)
BACKGROUND = QColor("#fbfbf8")
TEXT, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
NODE, DISC = QColor("#1a1a1a"), QColor("#000000")   # the circle and its dark disc
ISA = QColor("#b8b8b0")              # ISA links, thin, with an arrowhead at the category
LABELED = QColor("#3a3a3a")          # labeled and identity links at the floor
SHORTENED = QColor("#1f5fbf")        # a labeled link shorter than at the floor (D-011)
LABEL = QColor("#2a7f2a")            # a label node; bold while it shortens a link
ROLE = QColor("#5b2a86")             # the role nodes' rings and the roles' links (D-080; G-062)
PUSH = QColor("#e6007e")             # the nodes a push reset to 1 (D-070)
POP = QColor("#1f5fbf")              # the nodes a pop restored (D-021)
SELECTED = QColor("#e07b00")         # the selection highlight (G-022)
ARROW = 4.0                          # the ISA arrowhead's length in pixels


class SlipnetNetworkWidget(QWidget):
    """The network itself (G-034): paints the `NetworkLayout` of its model for its size
    and the selection through a cached pixmap; `drawn` is the last layout painted,
    `paints` the count for the window's probe (G-011), `renders` how many pixmaps were
    rendered; a click toggles the node in the `selection` (G-022)."""

    def __init__(self, model, selection=None, parent=None):
        super().__init__(parent)
        self.model = model
        self.selection = selection
        self.drawn = sn.network_layout(model, 10, 10)
        self.paints = 0
        self.renders = 0
        self._picture = None             # the picture last rendered (G-034)
        self._picture_key = None         # (layout, size, ratio) it was rendered for
        self.setMinimumSize(MIN_SIZE[0], MIN_SIZE[1] - 24)
        self.setMouseTracking(True)
        self.setToolTip(sn.CAPTION)

    def current_layout(self):
        """The layout for the current size and selection (G-034)."""
        selected = self.selection.name if self.selection is not None \
            and self.selection.kind == "node" else None
        return sn.network_layout(self.model, self.width(), self.height(), selected)

    def mouseMoveEvent(self, event):
        """The node, else the link, under the mouse as the tooltip; else the caption
        and the banner (G-033, G-035)."""
        x, y = event.position().x(), event.position().y()
        dot = sn.dot_at(self.drawn, x, y)
        if dot is not None:
            self.setToolTip(self.model.tooltip(dot.name))
            return
        line = sn.line_at(self.drawn, x, y)
        if line is not None:
            self.setToolTip(self.model.link_tooltip(line.name))
        else:
            self.setToolTip(f"{self.drawn.banner}\n{sn.CAPTION}")

    def mousePressEvent(self, event):
        """A left click toggles the node under the mouse in the shared selection; on no
        node, clears it (G-022, G-030)."""
        if event.button() != Qt.MouseButton.LeftButton or self.selection is None:
            return
        dot = sn.dot_at(self.drawn, event.position().x(), event.position().y())
        if dot is None:
            self.selection.clear()
        else:
            self.selection.toggle(*self.model.selection_of(dot.name))

    def paintEvent(self, event):
        """Paint the picture of the layout at this size: the one rendered last when the
        layout, the size and the pixel ratio are the ones it was rendered for, else a
        fresh `render_picture` (Seek-Whence GUI, `platoplasm_view.py`; G-034); counts
        the paints (G-011)."""
        self.paints += 1
        self.drawn = self.current_layout()
        key = (self.drawn, self.size(), self.devicePixelRatioF())
        if self._picture is None or self._picture_key != key:
            self._picture = self.render_picture(self.drawn)
            self._picture_key = key
        painter = QPainter(self)
        painter.drawPixmap(0, 0, self._picture)
        painter.end()

    def render_picture(self, lay) -> QPixmap:
        """The lines, the stems, the dots and the texts, on a pixmap the widget's size
        (G-034)."""
        self.renders += 1
        ratio = self.devicePixelRatioF()
        picture = QPixmap(QSize(max(1, round(self.width() * ratio)),
                                max(1, round(self.height() * ratio))))
        picture.setDevicePixelRatio(ratio)
        painter = QPainter(picture)
        painter.fillRect(self.rect(), BACKGROUND)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, True)
        small = QFont(painter.font())
        small.setPointSizeF(max(5.0, min(7.5, lay.dots[0].radius * 0.9)))
        painter.setFont(small)
        for line in lay.lines:
            self._paint_line(painter, line)
        for stem in lay.stems:
            self._paint_stem(painter, stem)
        for dot in lay.dots:
            self._paint_dot(painter, dot, lay)
        self._paint_texts(painter, lay)
        painter.end()
        return picture

    def _paint_line(self, painter, line) -> None:
        """One link: an ISA link thin with an arrowhead at the category, a labeled link
        thicker as it shrinks and blue once shorter than at the floor, with its
        effective length beside it; an identity link as a loop above its node (G-034)."""
        painter.setBrush(Qt.BrushStyle.NoBrush)
        if line.kind == sd.ISA:
            painter.setPen(QPen(SELECTED if line.selected else ROLE if line.role else ISA, 1.0))
            painter.drawLine(QPointF(line.x1, line.y1), QPointF(line.x2, line.y2))
            self._paint_arrowhead(painter, line)
            return
        colour = SELECTED if line.selected else (SHORTENED if line.shortened
                                                 else ROLE if line.role else LABELED)
        painter.setPen(QPen(colour, 1.0 + 3.0 * line.shrink))
        if line.loop:
            r = 0.6 * self.drawn.dots[0].radius
            painter.drawEllipse(QPointF(line.x1, line.y1 - 2.2 * r), r, r)
            return
        painter.drawLine(QPointF(line.x1, line.y1), QPointF(line.x2, line.y2))
        mx, my = line.midpoint
        painter.setPen(QPen(colour, 1.0))
        painter.drawText(QRectF(mx - 20, my - 12, 40, 10), Qt.AlignmentFlag.AlignCenter, line.text)

    def _paint_arrowhead(self, painter, line) -> None:
        """The ISA arrowhead at the category's edge, as Fig. 3.6 draws ISA [F95 p. 61
        (img)] (G-034)."""
        dx, dy = line.x2 - line.x1, line.y2 - line.y1
        length = math.hypot(dx, dy)
        if length == 0:
            return
        ux, uy = dx / length, dy / length
        radius = self.drawn.dots[0].radius
        tip = QPointF(line.x2 - ux * radius, line.y2 - uy * radius)
        base = QPointF(tip.x() - ux * ARROW, tip.y() - uy * ARROW)
        side = QPointF(-uy * ARROW * 0.5, ux * ARROW * 0.5)
        painter.setBrush(painter.pen().color())
        painter.drawPolygon(QPolygonF([tip, base + side, base - side]))
        painter.setBrush(Qt.BrushStyle.NoBrush)

    def _paint_stem(self, painter, stem) -> None:
        """The dashed stem from a label node to its link's midpoint, green while the
        label shortens the link (G-034)."""
        pen = QPen(LABEL if stem.active else ISA, 1.2 if stem.active else 0.8,
                   Qt.PenStyle.DashLine)
        painter.setPen(pen)
        painter.drawLine(QPointF(stem.x1, stem.y1), QPointF(stem.x2, stem.y2))

    def _paint_dot(self, painter, dot, lay) -> None:
        """One node: the circle (green for a label node, bold while it shortens), the
        dark disc for the activation, the push and pop rings, the selection ring and the
        name above or below (G-034, G-035)."""
        centre = QPointF(dot.x, dot.y)
        painter.setBrush(BACKGROUND)
        if dot.label_node:
            painter.setPen(QPen(LABEL, 2.0 if dot.shortening else 1.0))
        elif dot.role:
            painter.setPen(QPen(ROLE, 1.0, Qt.PenStyle.DashLine))     # a role node (G-062)
        else:
            painter.setPen(QPen(NODE, 1.0))
        painter.drawEllipse(centre, dot.radius, dot.radius)
        if dot.fill > 0:
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(DISC)
            painter.drawEllipse(centre, dot.fill, dot.fill)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        if dot.reset:
            painter.setPen(QPen(PUSH, 2.0, Qt.PenStyle.DashLine))
            painter.drawEllipse(centre, dot.radius + 3, dot.radius + 3)
        if dot.restored:
            painter.setPen(QPen(POP, 2.0))
            painter.drawEllipse(centre, dot.radius + 3, dot.radius + 3)
        if dot.selected:
            painter.setPen(QPen(SELECTED, 2.0))
            painter.drawEllipse(centre, dot.radius + 5, dot.radius + 5)
        above = dot.y < lay.band_y[sn.LEVEL_ROWS[dot.level]]
        top = dot.y - dot.radius - 11 if above else dot.y + dot.radius + 1
        painter.setPen(QPen(TEXT, 1.0))
        left = max(0.0, min(lay.width - 60, dot.x - 30))     # the name stays inside the widget
        painter.drawText(QRectF(left, top, 60, 10), Qt.AlignmentFlag.AlignCenter, dot.label)

    def _paint_texts(self, painter, lay) -> None:
        """The banner along the top (magenta while inside a subworld, blue after a pop,
        muted while there has been no push) and the caption along the bottom (G-035)."""
        colour = MUTED if lay.banner == sn.NO_PUSH else (PUSH if lay.banner.startswith("inside")
                                                         else POP)
        painter.setPen(QPen(colour, 1.0))
        flags = Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter
        painter.drawText(QRectF(2, 0, lay.width - 4, sn.BANNER_HEIGHT), flags, lay.banner)
        painter.setPen(QPen(MUTED, 1.0))
        painter.drawText(QRectF(2, lay.height - sn.CAPTION_HEIGHT, lay.width - 4,
                                sn.CAPTION_HEIGHT), flags, lay.caption)


class SlipnetNetworkView(QWidget):
    """The Slipnet network pane (G-033 to G-035; G-011's view protocol): the bar's label
    over the `SlipnetNetworkWidget`; `attach(history, selection)` takes the window's;
    `set_position`, `on_event`, `redraw(now=False)`, `paints`, `events`, `pane_id`,
    `title`, `pages`."""

    def __init__(self, title="Slipnet network", pane_id="P3b", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = sn.SlipnetNetworkModel()
        self.history = None
        self.position = -1
        self.network = SlipnetNetworkWidget(self.model)
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self.bar = QWidget()
        row = QHBoxLayout(self.bar)
        row.setContentsMargins(0, 0, 0, 0)
        self.label = QLabel(sn.NOT_FRENCHS + ', after Fig. 3.6 [F95 p. 61 (img)]; links at '
                            'their effective lengths (D-011); push and pop (D-021)')
        self.label.setToolTip(self.toolTip())
        self.label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        row.addWidget(self.label, 1)
        column = QVBoxLayout(self)
        column.setContentsMargins(2, 2, 2, 2)
        column.setSpacing(2)
        column.addWidget(self.bar)
        column.addWidget(self.network, 1)

    def attach(self, history, selection) -> None:
        """Take the window's history and its shared selection (G-022, G-033)."""
        self.history = history
        self.network.selection = selection
        selection.subscribe(self._on_selection)

    @property
    def paints(self) -> int:
        """The network's paint count (the window's probe; G-011)."""
        return self.network.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    @property
    def layout_now(self):
        """The network's geometry for its current size and the selection (G-034)."""
        return self.network.current_layout()

    def _on_selection(self, _selection) -> None:
        """The shared selection changed, from this pane or another: redraw now (G-022)."""
        self.redraw(now=True)

    def set_position(self, index: int) -> None:
        """Which event comes next; the model is a function of the events, so nothing is
        read from the history (G-017's protocol; G-033)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model (G-033, G-035)."""
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Mark the network dirty and, with `now`, paint it at once (Numbo GUI,
        `paint.py`)."""
        self.network.update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the network to a PNG file (RGB), for the tests (G-034)."""
        self.network.grab().toImage().convertToFormat(QImage.Format.Format_RGB32).save(
            str(path), "PNG")
        return path.read_bytes()

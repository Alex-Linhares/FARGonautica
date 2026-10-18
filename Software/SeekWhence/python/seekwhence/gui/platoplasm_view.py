"""The platoplasm pane (P5): the eight ideal types as a network with their s-links, the
ideal atoms' strip, the reigning class with its crown and threshold, the slip-check as it
happens and the links to the structures below, a view of `models/platoplasm.py`'s
`PlatoplasmModel` (PANES.md P5; Numbo GUI, `pnet_view.py`; G-036, G-037, G-038).

**GUI design, not Meredith's** (ANSWERS.md 4): the thesis describes the platoplasm in
prose, the ideal types in "a hierarchy of categories" [M86 p. 54], the s-links that "have
direction and 'slipperiness'" [M86 p. 90], the pull and push bonds [M86 p. 91], the
reigning class and the slip-or-stay arithmetic [M86 pp. 99-105], but has no figure of this
network, and FCCA has none. The drawing follows the Numbo GUI's Pnet view: a fixed
arrangement, here one column per category from the strictest on the left [M86 pp. 54-55,
59], each type a box with its format [M86 pp. 49-53] and the counts of its manifestations
and actualizations [M86 p. 90]; the s-links as arrows whose width and label give the
slipperiness [M86 p. 90]; under the network the ideal atoms met so far, each with its glint
count [M86 pp. 48-49]. The reigning class wears a crown [M86 p. 99] that travels along the
s-link at a change of class, `Countup --> S-group` [M86 p. 174]; while a slip-check is in
force the links out of the class checked carry their pressures and the best pretender is
marked [M86 pp. 103-105]. The list beside the network gives the reigning class and the
threshold [M86 pp. 99, 105], the check as the transcript prints it, `slip-check: stayval:
15.0`, `best: (Y-group 4.0)` [M86 p. 176], and the reigning class's bonds [M86 p. 91]; it
is a `LineList` (Numbo GUI, `log_view.py`'s `LogList`, through `taskrack_view.py`; G-028).
A click on a type selects its manifestations and actualizations in the shared selection
(G-025), so the cytoplasm and the socratoplasm show them; the "Links" button draws them as
tags beside the type, Ch. 2 Figure 2's manifestation and actualization links [M86 p. 45],
off by default (PANES.md section 3).
"""

import math

from PySide6.QtCore import QLineF, QPointF, QRect, QRectF, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QFontMetrics, QPainter, QPen, QPixmap, QPolygonF
from PySide6.QtWidgets import (
    QHBoxLayout, QLabel, QSplitter, QToolButton, QVBoxLayout, QWidget)

from seekwhence.gui import paint
from seekwhence.gui.models import platoplasm as pm
from seekwhence.gui.taskrack_view import LineList

__all__ = ["NetworkWidget", "PlatoplasmList", "PlatoplasmView", "NOT_MEREDITHS"]

PAGES = ("[M86 pp. 45, 48-55, 59, 90-91, 99-105, 126, 168, 174-176]. GUI design, not "
         "Meredith's: no figure of the network exists.")
NOT_MEREDITHS = "GUI design, not Meredith's"
BACKGROUND = "#fbfbf8"
TEXT, MUTED, GREY = "#1a1a1a", "#6b6b6b", "#9a9a9a"
# One colour per category [M86 pp. 54-55] (G-036).
CATEGORY_COLORS = {"repeater": "#6a4fb3", "generate": "#2f5597", "process": "#2e7d32",
                   "fence": "#8a6d3b"}
CATEGORY_FILLS = {"repeater": "#efeaff", "generate": "#eef3ff", "process": "#e9f5e9",
                  "fence": "#f6efe3"}
REIGNING_FILL, REIGNING_BORDER = "#fff3c4", "#c9a200"       # the crown's gold
SELECTED_BORDER = "#e07b00"
BEST_BORDER = "#e07b00"
LINK, LINK_LIT, LINK_BEST = "#9a9a9a", "#2f5597", "#e07b00"
ATOM_FILL, ATOM_BORDER = "#ffffff", "#4a4a4a"
TAG_FILL = "#f3f3f3"
HEAD_TINT = "#fff3c4"
CHANGE_COLOR = "#8a6d00"
MARGIN = 8
PAD = 4
INDENT = 16
ARROW = 7.0
HIT_DISTANCE = 5.0
CROWN = "♛"        # ♛


class NetworkWidget(QWidget):
    """The network as `layout()` gives it [M86 pp. 48-55, 90] (G-036): the headings, the
    boxes, the arrows, the atoms' strip, the tags and the crown, scaled to fit the widget;
    `set_layout(layout, tooltips)` gives a new picture and the tooltip of each type; a
    click emits `clicked(name)` (None for empty space); `paints` counts the paint calls
    (Numbo GUI, `pnet_view.py`'s `paints`)."""

    clicked = Signal(object)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.lay = None
        self.tooltips = {}
        self.paints = 0
        self._picture = None            # the picture last rendered (G-060)
        self._picture_key = None        # (layout, size, ratio) it was rendered for
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(9)
        self.bold = QFont(self.font_)
        self.bold.setBold(True)
        self.small = QFont(self.font_)
        self.small.setPointSizeF(7.5)
        self.metrics = QFontMetrics(self.font_)
        self.setMinimumSize(80, 48)          # a short dock scales the picture down (G-058)
        self.setAutoFillBackground(True)
        self.setToolTip(NOT_MEREDITHS + " (ANSWERS.md 4): no figure of the network is in "
                        "the thesis or FCCA; " + PAGES)

    def sizeHint(self):
        """Room for the four columns and the strip (G-036)."""
        return QSize(560, 220)

    def set_layout(self, lay, tooltips) -> None:
        """A new picture and the types' tooltips; the widget is marked dirty (G-036)."""
        self.lay = lay
        self.tooltips = dict(tooltips)
        self.update()

    def _scale(self) -> float:
        if self.lay is None or self.lay.width <= 0 or self.lay.height <= 0:
            return 1.0
        return min(1.4, (self.width() - 2 * MARGIN) / self.lay.width,
                   (self.height() - 2 * MARGIN) / self.lay.height)

    def point(self, x, y) -> QPointF:
        """A layout point on the widget (G-036)."""
        s = self._scale()
        return QPointF(MARGIN + x * s, MARGIN + y * s)

    def rect_of(self, shape) -> QRectF:
        """A shape's box on the widget (G-036)."""
        s = self._scale()
        return QRectF(MARGIN + shape.x * s, MARGIN + shape.y * s, shape.w * s, shape.h * s)

    def node_at(self, pos) -> pm.NodeShape | None:
        """The type under a point, or None (G-036)."""
        if self.lay is None:
            return None
        for node in self.lay.nodes:
            if self.rect_of(node).contains(pos):
                return node
        return None

    def atom_at(self, pos) -> pm.AtomShape | None:
        """The atom under a point, or None (G-036)."""
        if self.lay is None:
            return None
        for atom in self.lay.atoms:
            if self.rect_of(atom).contains(pos):
                return atom
        return None

    def link_at(self, pos) -> pm.LinkShape | None:
        """The nearest s-link within `HIT_DISTANCE` of a point, or None (G-036)."""
        if self.lay is None:
            return None
        best, distance = None, HIT_DISTANCE
        for link in self.lay.links:
            d = _distance(pos, self.point(link.x1, link.y1), self.point(link.x2, link.y2))
            if d < distance:
                best, distance = link, d
        return best

    def mousePressEvent(self, event):
        """A left click emits the type's name, or None on empty space (G-038)."""
        if event.button() == Qt.MouseButton.LeftButton:
            node = self.node_at(event.position())
            self.clicked.emit(node.name if node is not None else None)

    def event(self, e):
        """The tooltip is the type's, the s-link's or the atom's, else the pane's label
        (G-038)."""
        if e.type() == e.Type.ToolTip:
            pos = QPointF(e.pos())
            node = self.node_at(pos)
            if node is not None:
                self.setToolTip(self.tooltips.get(node.name, node.name))
            else:
                link = self.link_at(pos)
                atom = self.atom_at(pos) if link is None else None
                if link is not None:
                    self.setToolTip(pm.link_tooltip(link))
                elif atom is not None:
                    self.setToolTip(self.tooltips.get(("atom", atom.value), str(atom.value)))
                else:
                    self.setToolTip(NOT_MEREDITHS + " (ANSWERS.md 4). " + PAGES)
        return super().event(e)

    def paintEvent(self, event):
        """Paint the picture of the layout at this size: the one rendered last, when
        the layout, the size and the pixel ratio are the ones it was rendered for, else
        a fresh `render_picture` (G-060). The platoplasm changes at a slip, a dubbing or
        a slip-check and stays the same over thousands of events between, so the
        every-event redraw costs one blit then."""
        self.paints += 1
        key = (self.lay, self.size(), self.devicePixelRatioF())
        if self._picture is None or self._picture_key != key:
            self._picture = self.render_picture()
            self._picture_key = key
        p = QPainter(self)
        p.drawPixmap(0, 0, self._picture)
        p.end()

    def render_picture(self) -> QPixmap:
        """The headings, the arrows under the boxes, the tags, the boxes, the atoms and
        the crown last, on a pixmap the widget's size (G-036, G-060)."""
        ratio = self.devicePixelRatioF()
        picture = QPixmap(QSize(max(1, round(self.width() * ratio)),
                                max(1, round(self.height() * ratio))))
        picture.setDevicePixelRatio(ratio)
        p = QPainter(picture)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.fillRect(self.rect(), QColor(BACKGROUND))
        lay = self.lay
        if lay is None:
            p.end()
            return picture
        s = self._scale()
        p.setFont(self.small)
        for heading in lay.headings:
            p.setPen(QColor(CATEGORY_COLORS[heading.text]))
            p.drawText(self.rect_of(heading).toRect(),
                       Qt.AlignmentFlag.AlignCenter, heading.text + " type")
        for link in lay.links:
            self._paint_link(p, link, s)
        for tag in lay.tags:
            self._paint_tag(p, tag)
        for node in lay.nodes:
            self._paint_node(p, node, s)
        for atom in lay.atoms:
            self._paint_atom(p, atom, s)
        if lay.crown is not None:
            crown = QFont(self.bold)
            crown.setPointSizeF(max(8.0, 13 * s))
            p.setFont(crown)
            p.setPen(QColor(REIGNING_BORDER))
            at = self.point(lay.crown.x, lay.crown.y)
            p.drawText(QRectF(at.x() - 10, at.y() - 16, 20, 18), Qt.AlignmentFlag.AlignCenter,
                       CROWN)
        p.end()
        return picture

    def _paint_link(self, p, link, s):
        a, b = self.point(link.x1, link.y1), self.point(link.x2, link.y2)
        color = LINK_BEST if link.best else LINK_LIT if link.lit else LINK
        width = max(0.8, link.width * s) * (1.6 if link.best else 1.0)
        p.setPen(QPen(QColor(color), width, Qt.PenStyle.SolidLine, Qt.PenCapStyle.RoundCap))
        p.drawLine(QLineF(a, b))
        angle = math.atan2(b.y() - a.y(), b.x() - a.x())
        size = ARROW * max(0.6, s)
        head = QPolygonF([
            b,
            QPointF(b.x() - size * math.cos(angle - 0.45), b.y() - size * math.sin(angle - 0.45)),
            QPointF(b.x() - size * math.cos(angle + 0.45), b.y() - size * math.sin(angle + 0.45))])
        p.setBrush(QColor(color))
        p.setPen(Qt.PenStyle.NoPen)
        p.drawPolygon(head)
        label = QFont(self.bold if link.pressure is not None else self.small)
        label.setPointSizeF(max(6.0, (8.0 if link.pressure is not None else 7.0) * s))
        p.setFont(label)
        p.setPen(QColor(color if link.lit else MUTED))
        at = self.point(link.lx, link.ly)
        p.drawText(QRectF(at.x() - 40, at.y() - 7, 80, 14), Qt.AlignmentFlag.AlignCenter,
                   link.label)

    def _paint_tag(self, p, tag):
        rect = self.rect_of(tag)
        color = QColor(GREY if tag.kind == "more" else CATEGORY_COLORS[pm.category_of(tag.owner)])
        style = Qt.PenStyle.DotLine if tag.kind == "manifestation" else Qt.PenStyle.SolidLine
        p.setPen(QPen(color, 1.0, style))
        p.drawLine(QLineF(self.point(tag.x1, tag.y1), self.point(tag.x2, tag.y2)))
        p.setBrush(QColor(TAG_FILL))
        p.setPen(QPen(color, 0.8))
        p.drawRoundedRect(rect, 3, 3)
        p.setFont(self.small)
        p.setPen(QColor(TEXT))
        p.drawText(rect.toRect(), Qt.AlignmentFlag.AlignCenter,
                   self.metrics.elidedText(tag.name, Qt.TextElideMode.ElideRight,
                                           int(rect.width()) - 2))

    def _paint_node(self, p, node, s):
        rect = self.rect_of(node)
        border = QColor(CATEGORY_COLORS[node.category])
        fill = QColor(REIGNING_FILL if node.reigning else CATEGORY_FILLS[node.category])
        width = 1.2
        style = Qt.PenStyle.SolidLine
        if node.reigning:
            border, width = QColor(REIGNING_BORDER), 2.2
        if node.best:
            border, width, style = QColor(BEST_BORDER), 1.8, Qt.PenStyle.DashLine
        if node.selected:
            border, width = QColor(SELECTED_BORDER), 2.4
        p.setPen(QPen(border, width, style))
        p.setBrush(fill)
        p.drawRoundedRect(rect, 5, 5)
        name = QFont(self.bold)
        name.setPointSizeF(max(7.0, 9.5 * s))
        p.setFont(name)
        p.setPen(QColor(TEXT))
        top = QRectF(rect.x(), rect.y() + 1, rect.width(), rect.height() * 0.5)
        p.drawText(top.toRect(), Qt.AlignmentFlag.AlignCenter, node.name)
        small = QFont(self.small)
        small.setPointSizeF(max(6.0, 7.5 * s))
        p.setFont(small)
        p.setPen(QColor(MUTED))
        bottom = QRectF(rect.x() + 2, rect.y() + rect.height() * 0.5, rect.width() - 4,
                        rect.height() * 0.5 - 2)
        metrics = QFontMetrics(small)
        text = ("strict \u00b7 " if node.strict else "") + node.counts   # the format is in the tooltip
        p.drawText(bottom.toRect(), Qt.AlignmentFlag.AlignCenter,
                   metrics.elidedText(text, Qt.TextElideMode.ElideRight, int(bottom.width())))

    def _paint_atom(self, p, atom, s):
        rect = self.rect_of(atom)
        p.setPen(QPen(QColor(ATOM_BORDER), 1.0))
        p.setBrush(QColor(ATOM_FILL))
        p.drawRoundedRect(rect, 3, 3)
        value = QFont(self.bold)
        value.setPointSizeF(max(7.0, 9 * s))
        p.setFont(value)
        p.setPen(QColor(TEXT))
        p.drawText(rect.toRect(), Qt.AlignmentFlag.AlignCenter, str(atom.value))
        if atom.count:
            small = QFont(self.small)
            small.setPointSizeF(max(6.0, 7 * s))
            p.setFont(small)
            p.setPen(QColor(MUTED))
            p.drawText(QRectF(rect.right() - 12, rect.top() - 9, 16, 10),
                       Qt.AlignmentFlag.AlignCenter, str(atom.count))


def _distance(point, a, b) -> float:
    """The distance from a point to the segment a-b (G-036)."""
    ax, ay, bx, by = a.x(), a.y(), b.x(), b.y()
    px, py = point.x(), point.y()
    dx, dy = bx - ax, by - ay
    length2 = dx * dx + dy * dy
    if length2 == 0:
        return math.hypot(px - ax, py - ay)
    t = max(0.0, min(1.0, ((px - ax) * dx + (py - ay) * dy) / length2))
    return math.hypot(px - (ax + t * dx), py - (ay + t * dy))


class PlatoplasmList(LineList):
    """The lines of `pane_lines(model)`, painted straight from the model (Numbo GUI,
    `log_view.py`'s `LogList`; G-028, G-038): headings bold on a gold band, pressures
    indented with the best in orange, a class change in gold, notes grey."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.lines = ()

    def set_lines(self, lines) -> None:
        """New lines; the scroll range follows and the viewport is marked dirty (G-028)."""
        self.lines = tuple(lines)
        self.update_rows()
        self.viewport().update()

    def row_count(self) -> int:
        """How many lines (G-028)."""
        return len(self.lines)

    def line(self, row: int) -> pm.PLine:
        """The row's line (G-038)."""
        return self.lines[row]

    def _paint_line(self, p, line, box):
        h = box.height()
        if line.kind == "head":
            p.fillRect(box, QColor(HEAD_TINT))
        color = QColor(GREY if line.grey else MUTED if line.kind in ("note", "scout")
                       else CHANGE_COLOR if line.kind in ("head", "change") else TEXT)
        if line.best:
            color = QColor(LINK_BEST)
        x = PAD + (INDENT if line.kind in ("pressure", "bond", "note") else 0)
        p.setFont(self.bold if line.kind == "head" or line.best else self.font_)
        p.setPen(color)
        room = max(10, box.width() - x - PAD)
        p.drawText(QRect(x, box.y(), room, h),
                   Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                   self.metrics.elidedText(line.text, Qt.TextElideMode.ElideRight, room))


class PlatoplasmView(QWidget):
    """The platoplasm dock's view (see the module docstring; G-036, G-037, G-038).
    `history` is the window's event log; `selection` the shared `Selection` (G-025).
    `paints` is the network's count; `set_position` and `on_event` are the view
    protocol (G-011, G-016); `selected(name)` is emitted after a click on a type."""

    pane_id = "P5"
    title = "Platoplasm"
    pages = PAGES
    selected = Signal(object)

    def __init__(self, history, selection=None, parent=None):
        super().__init__(parent)
        self.history = history
        self.model = pm.PlatoplasmModel(selection)
        self.position = -1
        self.events = 0
        self.label = QLabel(NOT_MEREDITHS)
        self.label.setStyleSheet(f"color: {MUTED}; font-style: italic;")
        self.label.setMinimumWidth(1)        # clipped, not widening the dock (G-058)
        self.label.setToolTip(NOT_MEREDITHS + " (ANSWERS.md 4): the thesis describes the "
                              "platoplasm, the s-links and the slip-or-stay check in prose "
                              "[M86 pp. 48-55, 90-91, 99-105] but draws no network; the "
                              "layout is the GUI's own, after the Numbo GUI's Pnet view.")
        self.reigning_label = QLabel("no reigning class")
        font = QFont(self.reigning_label.font())
        font.setBold(True)
        self.reigning_label.setFont(font)
        self.reigning_label.setToolTip("the reigning class [M86 p. 99] and the Reformulator's "
                                       "threshold [M86 p. 105]")
        self.links_button = QToolButton()
        self.links_button.setText("Links")
        self.links_button.setCheckable(True)
        self.links_button.setChecked(False)
        self.links_button.setToolTip("Draw each type's manifestation and actualization links, "
                                     "as Ch. 2 Figure 2 [M86 p. 45]; off by default")
        self.links_button.toggled.connect(self.set_show_links)
        self.network = NetworkWidget()
        self.network.clicked.connect(self.select_type)
        self.list = PlatoplasmList()
        self.list.line_clicked.connect(self._line_clicked)
        head = QHBoxLayout()
        head.setContentsMargins(MARGIN, 2, MARGIN, 0)
        head.addWidget(self.reigning_label, 1)
        head.addWidget(self.label)
        head.addWidget(self.links_button)
        self.splitter = QSplitter(Qt.Orientation.Horizontal)
        self.splitter.addWidget(self.network)
        self.splitter.addWidget(self.list)
        self.splitter.setStretchFactor(0, 3)
        self.splitter.setStretchFactor(1, 2)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)
        layout.addLayout(head)
        layout.addWidget(self.splitter, 1)
        if self.model.selection is not None:
            self.model.selection.subscribe(self._on_selection)
        self.redraw()

    def sizeHint(self):
        """Room for the network and a few lines (G-011)."""
        return QSize(760, 240)

    @property
    def paints(self) -> int:
        """The network's paint count (G-011)."""
        return self.network.paints

    def nodes(self) -> tuple:
        """The types' boxes as last drawn (G-036)."""
        return self.network.lay.nodes if self.network.lay is not None else ()

    def links(self) -> tuple:
        """The s-links as last drawn (G-036)."""
        return self.network.lay.links if self.network.lay is not None else ()

    def atoms(self) -> tuple:
        """The atoms' cells as last drawn [M86 pp. 48-49] (G-036)."""
        return self.network.lay.atoms if self.network.lay is not None else ()

    def crown(self) -> pm.CrownShape | None:
        """Where the crown is, as last drawn [M86 p. 99] (G-037)."""
        return self.network.lay.crown if self.network.lay is not None else None

    def lines(self) -> tuple:
        """The list's lines as last drawn (G-038)."""
        return self.list.lines

    def node_rect(self, name: str) -> QRectF:
        """A type's box on the network widget (G-036)."""
        node = next(n for n in self.nodes() if n.name == name)
        return self.network.rect_of(node)

    # -- the view protocol (G-011, G-016) ----------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-016)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model and take the snapshot in force at `position` (G-037); a
        `start` begins a new run (G-011)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.model.on_event(event)
        self.model.update(self.history.state_at(self.position))

    def redraw(self, now=False) -> None:
        """Rebuild the picture, the lines and the labels and mark the widgets dirty; with
        `now`, paint at once (Numbo GUI, `paint.py`; G-036)."""
        model = self.model
        tooltips = {name: pm.node_tooltip(node, model.reigning)
                    for name, node in model.types.items()}
        for value, atom in model.atoms.items():
            tooltips[("atom", value)] = pm.atom_tooltip(atom)
        self.network.set_layout(pm.layout(model), tooltips)
        self.list.set_lines(pm.pane_lines(model))
        if model.reigning is None:
            text = "no reigning class"
        else:
            threshold = ("" if model.threshold is None
                         else f"   threshold {pm.pressure_text(model.threshold)}")
            text = f"reigning: {model.reigning}{threshold}"
        if self.reigning_label.text() != text:
            self.reigning_label.setText(text)
        if now:
            paint.paint_all()

    # -- interactions (G-038) ------------------------------------------------------------------

    def select_type(self, name) -> tuple:
        """Select a type's manifestations and actualizations in the shared selection
        (a second click clears; None clears) and emit `selected`; returns the names
        (PANES.md P5; G-025, G-038)."""
        members = self.model.select(name)
        self.selected.emit(self.model.selected)
        self.redraw(now=True)
        return members

    def set_show_links(self, on: bool) -> None:
        """The "Links" button: draw the manifestation and actualization links
        [M86 p. 45] (G-038)."""
        self.model.set_show_links(on)
        if self.links_button.isChecked() != bool(on):
            self.links_button.setChecked(bool(on))
        self.redraw(now=True)

    def _line_clicked(self, line):
        if line.name is not None and line.name in self.model.types:
            self.select_type(line.name)

    def _on_selection(self, _selection):
        self.redraw(now=True)

    def render_png(self, path):
        """Grab the pane to a PNG at `path` (Numbo GUI, `coderack_view.py`)."""
        image = self.grab()
        if not image.save(path):
            raise OSError(f"could not write {path}")
        return image

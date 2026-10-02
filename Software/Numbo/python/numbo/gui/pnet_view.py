"""The Pnet view: the 88 pnodes on pnet_model's fixed grid, shaded by activation.

PnetView is a QGraphicsView and a GUI-thread observer (MainWindow.add_view).
It holds a pnet_model.PnetModel:
  - on_event(event) updates the model, nothing else;
  - redraw() restyles the pnodes whose style changed (the grid never moves)
    and paints the viewport now, so the run waits for the picture.

Styling (pnode_style, a pure function, so the tests can check every item
against the model):
  - the fill is the activation on a heat ramp, white to dark red, on a
    square-root scale out of MAX_ACTIVATION (activation_level): most
    activations are a few units, and on a linear scale nearly every pnode
    would be white;
  - a stripe on the left gives the pnode's kind (number, sum, product,
    operation, concept, link type);
  - a pnode with cyto-node instances has a thick dark border, and the count
    if there are several.  config's pseudo-instances (the operations' and
    link types', from the start) are not marked;
  - a magenta halo on the pnodes the latest event changed, unless it
    changed all of them (a spread or the initialization: the shading shows
    that).
The edges are drawn once (one cached layer), faint, colored by link type.
The view always fits the whole grid.  redraw() restyles only the pnodes the
events changed and those losing their highlight.
"""

import dataclasses
import functools
import math

from PySide6.QtCore import QEvent, QLineF, QRectF, Qt
from PySide6.QtGui import QBrush, QColor, QFont, QPainter, QPen
from PySide6.QtWidgets import (QGraphicsItem, QGraphicsScene, QGraphicsView,
                               QToolTip)

from numbo.gui.paint import paint_all
from numbo.gui.render import render_scene_png
from numbo.models.pnet_model import PnetModel, is_pseudo_instance, pnet_layout

__all__ = ["MAX_ACTIVATION", "KIND_COLORS", "LINK_COLORS", "PnodeStyle", "activation_level",
           "pnode_style", "PnodeItem", "PnetView"]

MAX_ACTIVATION = 200       # the largest activations a run reaches
SHADES = 64
# Flat cells and a large font: the dock is wide and short, and the grid is
# scaled down to fit it (at 0.42 with pnet_layout's default cells and a
# 12 px font, the labels could not be read).
CELL = (64, 36)
FONT_PX, BADGE_PX = 15, 10
# The heat ramp: (level, color) stops.
HEAT = ((0.0, (255, 255, 255)), (0.25, (253, 212, 158)), (0.5, (252, 141, 89)),
        (0.75, (215, 48, 31)), (1.0, (127, 0, 0)))
DARK_TEXT, LIGHT_TEXT = "#1a1a1a", "#ffffff"
LIGHT_TEXT_FROM = 0.62     # the level from which the text is white
KIND_COLORS = {"number": "#4472c4", "plus": "#5a9e4b", "times": "#d08a3c",
               "operation": "#9a5bb5", "concept": "#b8a030", "link-type": "#7f7f7f"}
KIND_NAMES = {"number": "number", "plus": "sum", "times": "product",
              "operation": "operation", "concept": "concept", "link-type": "link type"}
LINK_COLORS = {"RESULT+": "#5a9e4b", "OPERAND": "#d08a3c", "RESULTX": "#b5562c",
               "SIMILAR": "#8a8ac8", "OPERATION": "#9a5bb5", "INSTANCE": "#a09030"}
EDGE_ALPHA = 110
BORDER, INSTANCE_BORDER = "#9a9a9a", "#1a1a1a"
HIGHLIGHT = "#e6007e"
STRIPE = 4
HALO = 4
BADGE_R = 7                # the instance count's disc
MARGIN = 8


def activation_level(activation):
    """ACTIVATION as a shade, 0 to 1 in SHADES steps: sqrt(a /
    MAX_ACTIVATION), clamped (None and non-numbers are 0).  The steps mean a
    change too small to see restyles nothing (a spread changes all 88)."""
    if not isinstance(activation, (int, float)) or activation <= 0:
        return 0.0
    return round(min(1.0, math.sqrt(activation / MAX_ACTIVATION)) * SHADES) / SHADES


@functools.lru_cache(maxsize=None)
def heat_color(level):
    for (l0, c0), (l1, c1) in zip(HEAT, HEAT[1:]):
        if level <= l1:
            t = (level - l0) / (l1 - l0)
            return "#%02x%02x%02x" % tuple(round(a + (b - a) * t) for a, b in zip(c0, c1))
    return "#%02x%02x%02x" % HEAT[-1][1]


@dataclasses.dataclass(frozen=True)
class PnodeStyle:
    """How a pnode is drawn.  LEVEL: activation_level; INSTANCES: how many
    cyto-node instances it has."""
    fill: str
    text: str
    level: float
    stripe: str
    border: str
    border_width: float
    instances: int
    highlight: bool


def pnode_style(spec, activation, instances, highlight):
    """The style of the pnode SPEC (a PnodeSpec) with ACTIVATION and
    INSTANCES (the model's), HIGHLIGHT if the latest event changed it."""
    level = activation_level(activation)
    cyto = sum(1 for i in instances or () if not is_pseudo_instance(i))
    return PnodeStyle(fill=heat_color(level),
                      text=LIGHT_TEXT if level >= LIGHT_TEXT_FROM else DARK_TEXT,
                      level=level, stripe=KIND_COLORS[spec.kind],
                      border=INSTANCE_BORDER if cyto else BORDER,
                      border_width=3.0 if cyto else 1.0, instances=cyto,
                      highlight=bool(highlight))


class PnodeItem(QGraphicsItem):
    """A pnode's box: RECT is (0, 0, width, height), at its grid cell."""

    def __init__(self, spec, size, font, badge_font):
        super().__init__()
        self.name = spec.name
        self.spec = spec
        self.font, self.badge_font = font, badge_font
        self.rect = QRectF(0, 0, *size)
        self.style = None
        self.setZValue(1)
        self.setCacheMode(QGraphicsItem.CacheMode.DeviceCoordinateCache)

    def set_style(self, style):
        if style != self.style:
            self.style = style
            self.update()

    def boundingRect(self):
        m = max(HALO, BADGE_R)
        return self.rect.adjusted(-m, -m, m, m)

    def paint(self, painter, option, widget=None):
        s, r = self.style, self.rect
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if s.highlight:
            painter.setPen(QPen(QColor(HIGHLIGHT), 3))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRoundedRect(r.adjusted(-HALO / 2, -HALO / 2, HALO / 2, HALO / 2), 6, 6)
        w = s.border_width
        inner = r.adjusted(w / 2, w / 2, -w / 2, -w / 2)
        painter.setPen(QPen(QColor(s.border), w))
        painter.setBrush(QBrush(QColor(s.fill)))
        painter.drawRoundedRect(inner, 4, 4)
        painter.setPen(Qt.PenStyle.NoPen)
        painter.setBrush(QColor(s.stripe))
        painter.drawRect(QRectF(inner.x(), inner.y() + 2, STRIPE, inner.height() - 4))
        painter.setFont(self.font)
        painter.setPen(QColor(s.text))
        painter.drawText(inner.adjusted(STRIPE, 0, 0, 0), Qt.AlignmentFlag.AlignCenter,
                         str(self.spec.short_name))
        if s.instances > 1:
            # A disc on the top-right corner, clear of the label.
            disc = QRectF(r.right() - BADGE_R, r.top() - BADGE_R, 2 * BADGE_R, 2 * BADGE_R)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QColor(INSTANCE_BORDER))
            painter.drawEllipse(disc)
            painter.setFont(self.badge_font)
            painter.setPen(QColor(LIGHT_TEXT))
            painter.drawText(disc, Qt.AlignmentFlag.AlignCenter, str(s.instances))


class EdgeLayer(QGraphicsItem):
    """Every edge, under the pnodes, in one item painted once and cached:
    LINES is (a, b, link type) -> QLineF.  (One line item per edge made the
    view repaint 175 antialiased lines at every event: 500 events/s.)"""

    def __init__(self, lines, rect):
        super().__init__()
        self.lines = lines
        self.rect = rect
        self.setZValue(0)
        self.setCacheMode(QGraphicsItem.CacheMode.DeviceCoordinateCache)

    def boundingRect(self):
        return self.rect.adjusted(-1, -1, 1, 1)

    def paint(self, painter, option, widget=None):
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        for (_, _, t), line in self.lines.items():
            color = QColor(LINK_COLORS.get(t, "#999999"))
            color.setAlpha(EDGE_ALPHA)
            painter.setPen(QPen(color, 1))
            painter.drawLine(line)


class PnetView(QGraphicsView):
    """The Pnet (see the module docstring).  NODE_ITEMS: name -> PnodeItem;
    EDGE_LAYER: the edges; PAINTS: how many times the viewport has been
    painted."""

    def __init__(self, parent=None):
        super().__init__(parent)
        scene = QGraphicsScene(self)
        scene.setItemIndexMethod(QGraphicsScene.ItemIndexMethod.NoIndex)
        self.setScene(scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setBackgroundBrush(QColor("#fbfbf8"))
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.model = PnetModel()
        self.paints = 0
        font = QFont(self.font())
        font.setPixelSize(FONT_PX)
        badge = QFont(font)
        badge.setPixelSize(BADGE_PX)
        badge.setBold(True)
        lay = self.grid = pnet_layout(CELL)
        s = self.model.structure
        self.node_items = {}
        w, h = lay.node_size
        for spec in s.nodes:
            item = self.node_items[spec.name] = PnodeItem(spec, (w, h), font, badge)
            x, y = lay.positions[spec.name]
            item.setPos(x - w / 2, y - h / 2)
            scene.addItem(item)
        self.edge_layer = EdgeLayer({(a, b, t): QLineF(*lay.positions[a], *lay.positions[b])
                                     for a, b, t in s.edges()},
                                    QRectF(0, 0, lay.width, lay.height))
        scene.addItem(self.edge_layer)
        scene.setSceneRect(-MARGIN, -MARGIN, lay.width + 2 * MARGIN, lay.height + 2 * MARGIN)
        self._dirty = set(self.node_items)     # to restyle at the next redraw
        self._lit = set()                      # highlighted at the last redraw
        self.redraw()

    def on_event(self, event):
        self.model.on_event(event)
        if event.kind == "start":
            self._dirty.update(self.node_items)
        else:
            self._dirty |= self.model.changed

    def highlighted(self, name):
        changed = self.model.changed
        return name in changed and len(changed) < len(self.node_items)

    def redraw(self, now=True):
        """Restyle the pnodes that changed (or lose their highlight), paint
        now (NOW false: see below)."""
        m = self.model
        names, self._dirty = self._dirty | self._lit, set()
        lit = set()
        for name in names:
            item = self.node_items[name]
            highlight = self.highlighted(name)
            if highlight:
                lit.add(name)
            item.set_style(pnode_style(item.spec, m.activation(name), m.instances(name),
                                       highlight))
        self._lit = lit
        if now and self.isVisible():
            self.viewport().update()
            paint_all((self.scene(),))
        # NOW false: the restyled items marked their own regions (the
        # scene's dirty pass, which paint_all runs), and nothing else
        # changed: most events touch no pnode.

    def tooltip(self, name):
        """NAME's kind, activation and instances, as the model has them now."""
        m, spec = self.model, self.node_items[name].spec
        a = m.activation(name)
        a = round(a, 2) if isinstance(a, float) else a
        text = f"{name} ({KIND_NAMES[spec.kind]})\nactivation {a}"
        instances = m.instances(name)
        if instances:
            parts = []
            for t, n in instances:
                parts.append(f"{n} ({t}, config)" if is_pseudo_instance((t, n)) else f"{n} ({t})")
            text += "\ninstances: " + ", ".join(parts)
        return text

    def viewportEvent(self, event):
        # Tooltips are built when asked for, so they are always current and
        # cost nothing per event.
        if event.type() == QEvent.Type.ToolTip:
            item = self.itemAt(event.pos())
            if isinstance(item, PnodeItem):
                QToolTip.showText(event.globalPos(), self.tooltip(item.name), self)
            else:
                QToolTip.hideText()
            return True
        return super().viewportEvent(event)

    def paintEvent(self, event):
        self.paints += 1
        super().paintEvent(event)

    def fit(self):
        self.fitInView(self.sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        self.fit()

    def showEvent(self, event):
        super().showEvent(event)
        self.fit()

    def render_png(self, path, scale=1.0):
        """Render the whole grid (at SCALE) to a PNG at PATH."""
        return render_scene_png(self, path, scale)

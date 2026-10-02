"""The AST canvas: the cytoplasm's arithmetic trees, redrawn after every event.

TreeView is a QGraphicsView and a GUI-thread observer (MainWindow.add_view).
It holds a tree_model.TreeModel and a tree_layout.TreeLayout:
  - on_event(event) updates the model, nothing else;
  - redraw() lays out the model's forest (ghosts from the last GHOST_WINDOW
    events), brings the scene's items in line with the layout (an item per
    box, NodeItem, and per edge, EdgeItem, kept by name and changed only
    where they differ), fits the view if auto-fit is on, and paints the
    viewport now, so the run waits for the picture.  redraw(now=False), the
    window's call, only marks the viewport dirty: the window then paints
    every view at once (gui/paint.py).  Either way the whole viewport is
    painted at every event.
  - On most events no tree changes: the model keeps its forest object and
    the layout keeps its layout (a layout is a fixed point of its forest),
    and redraw() only restyles the dead nodes (they fade) and the
    highlighted ones (loop0003 item 11: the canvas's Python cost per event
    fell from about 0.54 to 0.11 ms on puzzle 6 seed 1, profiled).

Styling (node_style, a pure function of a TreeNode, so the tests can check
every item against the model): the fill by type (TYPE_COLORS); the status
in the border (thin when free, thick when linked; dashed and faded, more
with age, for a ghost or a dead node); activation as a bar along the
bottom (out of MAX_ACTIVATION); a halo on what the latest change touched
(strong when it is the latest event, light when events that changed no tree
came since); a dark orange border on the current target.  An operation node
is a circle holding its symbol.  A label is the value, with the expression
under it for a node that has a subtree (view_label).

Zoom with the wheel or + / -, 0 for 1:1, F to fit; drag to pan.  Auto-fit
(on at first, and again after Fit) fits the forest to the view after each
event, never zooming in past MAX_FIT_SCALE; any zoom by hand turns it off.
"""

import dataclasses
import html
from typing import NamedTuple

from PySide6.QtCore import QPointF, QRectF, Qt
from PySide6.QtGui import (QBrush, QColor, QFont, QFontMetricsF, QPainter, QPainterPath, QPen,
                           QTransform)
from PySide6.QtWidgets import (QFrame, QGraphicsItem, QGraphicsPathItem, QGraphicsScene,
                               QGraphicsView, QHBoxLayout, QLabel, QPushButton, QToolButton,
                               QVBoxLayout, QWidget)

from numbo.gui.paint import paint_all
from numbo.gui.render import render_scene_png
from numbo.models.tree_layout import Spacing, TreeLayout, node_label
from numbo.models.tree_model import OP_TYPE, TreeModel

__all__ = ["GHOST_WINDOW", "TYPE_COLORS", "Label", "Style", "view_label", "node_style",
           "NodeItem", "EdgeItem", "TreeView", "OutcomeBanner", "TreeCanvas"]

GHOST_WINDOW = 50          # events a ghost stays on the canvas
MAX_ACTIVATION = 300       # a full bar (const-bl+ gives a new block 300)
MAX_FIT_SCALE = 1.5
ZOOM_STEP = 1.25
MARGIN = 16                # around the layout, in scene units

TYPE_COLORS = {"1t": "#f4b183", "3dt": "#e8141c", "4bl": "#9dc3e6", "2b": "#c5e0b4",
               OP_TYPE: "#e4e4e4"}
TYPE_NAMES = {"1t": "target", "3dt": "derived target", "4bl": "block", "2b": "brick",
              OP_TYPE: "operation"}
OTHER_COLOR = "#ffffff"
BORDER = "#333333"
CURRENT_BORDER = "#c55a11"
TEXT = "#1a1a1a"
# Derived targets (what Numbo still needs to build) stand out: bright red,
# with bold white text.
BOLD_TYPES = {"3dt"}
TYPE_TEXT = {"3dt": "#ffffff"}
BAR = "#4a4a4a"
BAR_TRACK = "#22000000"    # #AARRGGBB: a faint track under the bar
EDGE = "#8c8c8c"
HIGHLIGHTS = {"fresh": ("#e6007e", 3.5), "recent": ("#f2a6cf", 2.5)}
# An operation's symbol as drawn in its circle (the expressions keep
# check_solution's "-" and "x").
DISPLAY_SYMBOLS = {"-": "−", "x": "×", "/": "÷"}

PAD_X, PAD_Y = 8, 3        # 6 let a long expression touch the border
BAR_HEIGHT = 3
HALO = 4                   # room outside the box for the halo


class Label(NamedTuple):
    """A node's text, whether it is an operation's (drawn round, with no
    activation bar), and whether it is drawn bold (which makes it wider)."""
    text: str
    op: bool
    bold: bool = False


def view_label(node):
    """The value, and under it the expression if the node has a subtree;
    an operation's symbol (its operation for a ghost)."""
    if node.type == OP_TYPE:
        text = node_label(node)
        return Label(DISPLAY_SYMBOLS.get(text, text), True)
    text = node_label(node)
    if node.children and node.expression:
        text += "\n" + node.expression
    return Label(text, False, node.type in BOLD_TYPES)


@dataclasses.dataclass(frozen=True)
class Style:
    """How a node is drawn.  BAR: the activation, 0 to 1 (None: no bar);
    HIGHLIGHT: None, "fresh" or "recent"."""
    fill: str
    border: str
    border_width: float
    dashed: bool
    opacity: float
    bar: object
    highlight: object = None
    text: str = TEXT
    bold: bool = False


def node_style(node, ghost_age, highlight):
    """NODE's Style.  GHOST_AGE: how many events ago it left the cytoplasm
    (None if it hasn't, or for a dead node in a live tree); HIGHLIGHT: what
    the latest change makes it (None, "fresh", "recent")."""
    border, width = BORDER, (2.0 if node.status == "linked" else 1.0)
    if node.current:
        border, width = CURRENT_BORDER, 3.0
    opacity, dashed = 1.0, False
    if not node.alive:
        dashed = True
        if ghost_age is None:
            opacity = 0.6
        else:
            opacity = 0.15 + 0.6 * max(0.0, 1 - ghost_age / GHOST_WINDOW)
    bar = None
    if node.type != OP_TYPE:
        a = node.activation
        bar = 0 if not isinstance(a, (int, float)) else min(1, max(0, a / MAX_ACTIVATION))
    return Style(fill=TYPE_COLORS.get(node.type, OTHER_COLOR), border=border,
                 border_width=width, dashed=dashed, opacity=opacity, bar=bar,
                 highlight=highlight, text=TYPE_TEXT.get(node.type, TEXT),
                 bold=node.type in BOLD_TYPES)


class NodeItem(QGraphicsItem):
    """A node's box at its layout position: RECT is (0, 0, width, height)."""

    def __init__(self, name, font, bold_font=None):
        super().__init__()
        self.name = name
        self.font = font
        self.bold_font = bold_font or font
        self.node = None
        self.label = None
        self.style = None
        self.rect = QRectF()
        self.setZValue(1)
        # Painted once into a pixmap, again only when it changes (update())
        # or the zoom does.
        self.setCacheMode(QGraphicsItem.CacheMode.DeviceCoordinateCache)

    def set_node(self, node, label, style, box):
        rect = QRectF(0, 0, box.width, box.height)
        if rect != self.rect:
            self.prepareGeometryChange()
            self.rect = rect
        pos = QPointF(box.x, box.y)
        if self.pos() != pos:
            self.setPos(pos)
        if (label, style) != (self.label, self.style):
            self.label, self.style = label, style
            self.setOpacity(style.opacity)
            self.update()
        self.node = node

    def boundingRect(self):
        return self.rect.adjusted(-HALO, -HALO, HALO, HALO)

    def paint(self, painter, option, widget=None):
        s, r = self.style, self.rect
        op = self.node.type == OP_TYPE
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if s.highlight in HIGHLIGHTS:
            color, width = HIGHLIGHTS[s.highlight]
            painter.setPen(QPen(QColor(color), width))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            halo = r.adjusted(-HALO / 2, -HALO / 2, HALO / 2, HALO / 2)
            if op:
                painter.drawEllipse(halo)
            else:
                painter.drawRoundedRect(halo, 6, 6)
        pen = QPen(QColor(s.border), s.border_width)
        if s.dashed:
            pen.setStyle(Qt.PenStyle.DashLine)
        inner = r.adjusted(s.border_width / 2, s.border_width / 2,
                           -s.border_width / 2, -s.border_width / 2)
        painter.setPen(pen)
        painter.setBrush(QBrush(QColor(s.fill)))
        if op:
            painter.drawEllipse(inner)
        else:
            painter.drawRoundedRect(inner, 4, 4)
        text_rect = r
        if s.bar is not None:
            text_rect = r.adjusted(0, 0, 0, -BAR_HEIGHT - 2)
            b = inner.adjusted(3, 0, -3, -2)
            track = QRectF(b.x(), b.bottom() - BAR_HEIGHT, b.width(), BAR_HEIGHT)
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QColor(BAR_TRACK))
            painter.drawRect(track)
            if s.bar > 0:
                painter.setBrush(QColor(BAR))
                painter.drawRect(QRectF(track.x(), track.y(), track.width() * s.bar,
                                        BAR_HEIGHT))
        painter.setFont(self.bold_font if s.bold else self.font)
        painter.setPen(QColor(s.text))
        painter.drawText(text_rect, Qt.AlignmentFlag.AlignCenter, self.label)


class EdgeItem(QGraphicsPathItem):
    """A parent-child edge, from the parent's bottom center to the child's
    top center."""

    def __init__(self, parent, child):
        super().__init__()
        self.parent_name, self.child_name = parent, child
        self.ends = None
        self.setPen(QPen(QColor(EDGE), 1.2))
        self.setZValue(0)

    def set_ends(self, a, b, opacity):
        if (a, b) != self.ends:
            self.ends = (a, b)
            path = QPainterPath(a)
            mid = (a.y() + b.y()) / 2
            path.cubicTo(QPointF(a.x(), mid), QPointF(b.x(), mid), b)
            self.setPath(path)
        if self.opacity() != opacity:
            self.setOpacity(opacity)


class TreeView(QGraphicsView):
    """The canvas (see the module docstring).  NODE_ITEMS: name -> NodeItem;
    EDGE_ITEMS: (parent, child) -> EdgeItem; LAYOUT: the latest Layout;
    PAINTS: how many times the viewport has been painted."""

    def __init__(self, parent=None):
        super().__init__(parent)
        scene = QGraphicsScene(self)
        # A few dozen items that move at every event: no index to keep up.
        scene.setItemIndexMethod(QGraphicsScene.ItemIndexMethod.NoIndex)
        self.setScene(scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
        self.setTransformationAnchor(QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.ViewportAnchor.AnchorViewCenter)
        self.setBackgroundBrush(QColor("#fbfbf8"))
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.node_font = QFont(self.font())
        self.node_font.setPointSizeF(9)
        self._metrics = QFontMetricsF(self.node_font)
        self.bold_font = QFont(self.node_font)
        self.bold_font.setBold(True)
        self._bold_metrics = QFontMetricsF(self.bold_font)
        self.model = TreeModel()
        self.layouter = self.make_layouter()
        self.layout = None
        self.node_items = {}
        self.edge_items = {}
        self._lit = set()           # the names highlighted at the last redraw
        self._dead = set()          # the dead nodes drawn (they fade)
        self.paints = 0
        self.auto_fit = True
        self._apply_scroll_policy()
        self.redraw()

    # -- the run ----------------------------------------------------------------

    def measure(self, label):
        lines = label.text.split("\n")
        metrics = self._bold_metrics if label.bold else self._metrics
        w = max(metrics.horizontalAdvance(line) for line in lines) + 2 * PAD_X
        h = metrics.height() * len(lines) + 2 * PAD_Y
        if label.op:
            return (max(w, h), h)
        return (w, h + BAR_HEIGHT + 2)

    def make_layouter(self):
        """A TreeLayout like the view's (the window's LayoutHistory makes
        the view's layouts at any event with it)."""
        return TreeLayout(self.measure, label=view_label, spacing=Spacing())

    def on_event(self, event):
        self.model.on_event(event)
        if event.kind == "start":
            self.layouter.reset()
            self.clear()

    def clear(self):
        scene = self.scene()
        for item in list(self.node_items.values()) + list(self.edge_items.values()):
            scene.removeItem(item)
        self.node_items.clear()
        self.edge_items.clear()
        self._lit = set()
        self.layout = None

    def highlight(self, name):
        m = self.model
        if name not in m.last_change:
            return None
        return "fresh" if m.last_change_at == m.events_seen else "recent"

    def redraw(self, now=True):
        """Lay out the model's forest, update the scene, paint now (NOW
        false: mark the view dirty, for the window's paint_all)."""
        model, scene = self.model, self.scene()
        forest = model.forest(ghost_window=GHOST_WINDOW)
        layout = self.layouter.layout(forest)
        if layout is self.layout and self.node_items:
            self._restyle(forest)
        else:
            self.layout = layout
            self._rebuild(forest)
        if self.isVisible():
            self.viewport().update()
            if now:
                paint_all((scene,))

    def _restyle(self, forest):
        """The forest and its layout are those drawn (the same objects: no
        tree changed and no ghost left the window), so only styles can
        differ: the dead nodes' (ghosts, and a killed node left in a live
        tree: they fade with age) and the highlights' (fresh
        turns recent; the latest change is highlighted)."""
        model = self.model
        names = self._dead | model.last_change | self._lit
        lit = set()
        for name in names:
            item = self.node_items.get(name)
            if item is None:
                continue
            highlight = self.highlight(name)
            if highlight:
                lit.add(name)
            item.set_node(item.node, item.label,
                          node_style(item.node, model.ghost_age(name), highlight),
                          self.layout.boxes[name])
        self._lit = lit

    def _rebuild(self, forest):
        """Bring the scene in line with a new layout."""
        model, scene, layout = self.model, self.scene(), self.layout
        nodes = {n.name: n for n in forest.walk()}
        self._dead = {name for name, n in nodes.items() if not n.alive}
        lit = set()
        for name in [n for n in self.node_items if n not in layout.boxes]:
            scene.removeItem(self.node_items.pop(name))
        for name, box in layout.boxes.items():
            node = nodes[name]
            item = self.node_items.get(name)
            if item is None:
                item = self.node_items[name] = NodeItem(name, self.node_font, self.bold_font)
                scene.addItem(item)
            highlight = self.highlight(name)
            if highlight:
                lit.add(name)
            item.set_node(node, view_label(node).text,
                          node_style(node, model.ghost_age(name), highlight), box)
        self._lit = lit
        edges = set(layout.edges)
        for key in [k for k in self.edge_items if k not in edges]:
            scene.removeItem(self.edge_items.pop(key))
        for key in layout.edges:
            edge = self.edge_items.get(key)
            if edge is None:
                edge = self.edge_items[key] = EdgeItem(*key)
                scene.addItem(edge)
            a, b = layout.boxes[key[0]], layout.boxes[key[1]]
            edge.set_ends(QPointF(a.cx, a.bottom), QPointF(b.cx, b.y),
                          self.node_items[key[1]].style.opacity)
        scene.setSceneRect(self.content_rect())
        if self.auto_fit:
            self._fit()

    def content_rect(self):
        lay = self.layout
        return QRectF(-MARGIN, lay.top - MARGIN, lay.width + 2 * MARGIN,
                      lay.height + 2 * MARGIN)

    def paintEvent(self, event):
        self.paints += 1
        super().paintEvent(event)

    # -- zoom, pan and fit ----------------------------------------------------------

    def scale_factor(self):
        return self.transform().m11()

    def _apply_scroll_policy(self):
        policy = (Qt.ScrollBarPolicy.ScrollBarAlwaysOff if self.auto_fit
                  else Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(policy)
        self.setVerticalScrollBarPolicy(policy)

    def _set_auto_fit(self, on):
        if on != self.auto_fit:
            self.auto_fit = on
            self._apply_scroll_policy()

    def _fit(self):
        rect = self.sceneRect()
        port = self.viewport().rect()
        if rect.width() <= 0 or rect.height() <= 0 or port.width() <= 0:
            return
        s = min(port.width() / rect.width(), port.height() / rect.height(), MAX_FIT_SCALE)
        if abs(s - self.scale_factor()) > 1e-9:
            self.setTransform(QTransform.fromScale(s, s))
        self.centerOn(rect.center())

    def fit_to_view(self):
        """Fit the forest to the view, and keep doing so (auto-fit)."""
        self._set_auto_fit(True)
        self._fit()

    def zoom_by(self, factor):
        self._set_auto_fit(False)
        self.scale(factor, factor)

    def zoom_in(self):
        self.zoom_by(ZOOM_STEP)

    def zoom_out(self):
        self.zoom_by(1 / ZOOM_STEP)

    def reset_zoom(self):
        self._set_auto_fit(False)
        self.setTransform(QTransform())

    def wheelEvent(self, event):
        steps = event.angleDelta().y() / 120
        if steps:
            self.zoom_by(ZOOM_STEP ** steps)
        event.accept()

    def keyPressEvent(self, event):
        key = event.key()
        if key in (Qt.Key.Key_Plus, Qt.Key.Key_Equal):
            self.zoom_in()
        elif key == Qt.Key.Key_Minus:
            self.zoom_out()
        elif key == Qt.Key.Key_0:
            self.reset_zoom()
        elif key == Qt.Key.Key_F:
            self.fit_to_view()
        else:
            super().keyPressEvent(event)

    def resizeEvent(self, event):
        super().resizeEvent(event)
        if self.auto_fit:
            self._fit()

    # -- pictures --------------------------------------------------------------------

    def render_png(self, path, scale=1.0):
        """Render the whole scene (at SCALE) to a PNG at PATH."""
        return render_scene_png(self, path, scale)


def legend_html():
    swatch = ('<span style="background-color:{}; border:1px solid #333">'
              '&nbsp;&nbsp;&nbsp;</span>&nbsp;{}')
    parts = [swatch.format(TYPE_COLORS[t], TYPE_NAMES[t]) for t in TYPE_NAMES]
    parts.append(f'<span style="color:{CURRENT_BORDER}"><b>&#9634;</b></span>&nbsp;current target')
    parts.append(f'<span style="color:{HIGHLIGHTS["fresh"][0]}"><b>&#9634;</b></span>'
                 '&nbsp;latest change')
    return "&nbsp;&nbsp; ".join(parts)


BANNER_COLORS = {"good": ("#e3f4e8", "#1b7f3b"), "warn": ("#fff4d6", "#8a5a00"),
                 "error": ("#fde4e7", "#b00020")}


class OutcomeBanner(QFrame):
    """How the run ended (a run_stats.Verdict), over the canvas: green for
    a valid solution, amber for an invalid one or no solution, red for an
    error.  Hidden while there is no verdict, and after its close button
    until the verdict changes."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.setObjectName("outcome-banner")
        self.verdict = None
        self.dismissed = None
        self.label = QLabel()
        self.label.setWordWrap(True)
        self.label.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        self.close_button = QToolButton()
        self.close_button.setText("×")
        self.close_button.setToolTip("hide until the next outcome")
        self.close_button.setAutoRaise(True)
        self.close_button.clicked.connect(self._dismiss)
        box = QHBoxLayout(self)
        box.setContentsMargins(8, 4, 4, 4)
        box.addWidget(self.label, 1)
        box.addWidget(self.close_button, 0, Qt.AlignmentFlag.AlignTop)
        self.hide()

    @property
    def level(self):
        return self.verdict.level if self.verdict is not None else None

    def text(self):
        v = self.verdict
        if v is None:
            return ""
        return v.headline + ("\n" + v.detail if v.detail else "")

    def show_verdict(self, verdict):
        """Show VERDICT (None: hide).  Costs nothing if it is unchanged."""
        if verdict == self.verdict:
            return
        self.verdict = verdict
        if verdict is None:
            self.hide()
            return
        fill, ink = BANNER_COLORS[verdict.level]
        self.setStyleSheet(f"#outcome-banner {{ background: {fill}; border-bottom: 2px solid "
                           f"{ink}; }} QLabel {{ color: {ink}; }}")
        detail = html.escape(verdict.detail).replace("\n", "<br>")
        self.label.setText(f"<b>{html.escape(verdict.headline)}</b>"
                           + (f"<br>{detail}" if detail else ""))
        self.setVisible(verdict != self.dismissed)

    def _dismiss(self):
        self.dismissed = self.verdict
        self.hide()


class TreeCanvas(QWidget):
    """The main window's central widget: a bar (Fit, zoom buttons, the
    legend) over the outcome banner (BANNER) and the TreeView (VIEW)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.view = TreeView()
        self.view.setObjectName("tree-view")
        self.fit_button = QPushButton("Fit")
        self.zoom_in_button = QPushButton("+")
        self.zoom_out_button = QPushButton("−")
        self.reset_zoom_button = QPushButton("1:1")
        self.fit_button.clicked.connect(self.view.fit_to_view)
        self.zoom_in_button.clicked.connect(self.view.zoom_in)
        self.zoom_out_button.clicked.connect(self.view.zoom_out)
        self.reset_zoom_button.clicked.connect(self.view.reset_zoom)
        bar = QHBoxLayout()
        bar.setContentsMargins(4, 2, 4, 2)
        for button in (self.fit_button, self.zoom_in_button, self.zoom_out_button,
                       self.reset_zoom_button):
            button.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            button.setMaximumWidth(48)
            bar.addWidget(button)
        bar.addSpacing(12)
        self.legend = QLabel(legend_html())
        self.legend.setObjectName("tree-legend")
        bar.addWidget(self.legend)
        bar.addStretch(1)
        box = QVBoxLayout(self)
        box.setContentsMargins(0, 0, 0, 0)
        box.setSpacing(0)
        box.addLayout(bar)
        self.banner = OutcomeBanner()
        box.addWidget(self.banner)
        box.addWidget(self.view, 1)

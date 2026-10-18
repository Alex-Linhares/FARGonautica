"""The cytoplasm pane (P2, part 1): glints, sparks and bonds on a canvas, a view of
`models/cytoplasm.py` (PANES.md P2; Numbo GUI, `tree_view.py`; G-021, G-022, G-023).

The drawing follows Ch. 3 Figure 1 [M86 p. 102]: the glints are teardrops standing on
a baseline in sequence order, the value above the name; the bonds are arcs below the
baseline between their members, adjacent ones short and remote ones tall, coloured by
type as Hofstadter's sameness, successorship and predecessorship glue [FCCA pp. 58-60],
their width by strength, and labelled with their value in the reigning class's
environment once the gnoths list it (`+10`, `-5` [M86 p. 102]). The sparks, which have no
figure in the thesis, are faint dashed arcs that flash when made and fade when used
(G-022). Hovering an element shows its fields: a glint's as the thesis lists them
[M86 p. 61], a bond's as `show` prints it [M86 p. 167], a spark's as the trace does
[M86 p. 164].

Part 2 (item 06) draws the level above the baseline: the gloms as nested rounded hulls
over their members, one band per nesting depth, Hofstadter's islands of order
[FCCA pp. 58-60], each labelled with its type and, once dubbed, its plato-class, form
and purity [M86 p. 69]; the pseudo-gloms (the template glom, the gnoth-gloms, the frame
gloms) as dotted hulls on the top-down layer [M86 pp. 73, 77]; a Glom-scout's proposal
as a marker on its target while the Glomtester waits and a bracket over the members
while the Glommer does [M86 pp. 66-67]; a glom burst or dissolved [M86 pp. 69-70] or
blasted away [M86 p. 92] fading as a ghost. A click selects an element in the selection
shared by every pane: a glom with its glints (G-024, G-025). The four layers (sparks,
top-down, bond values, names; PANES.md P2) are checkable actions, `layer_actions`, that
the window puts in its View menu (G-026).

`CytoplasmView` is a `QGraphicsView` and a GUI-thread observer (the window's
`add_view`), after the Numbo GUI's `TreeView`: `on_event` feeds the model and takes the
snapshot in force at `position` (`set_position`, G-016); `redraw(now=False)` lays the
model out, brings the scene's items in line with the layout (one item per glint, per
arc, per hull and per marker, kept by name and changed only where they differ) and
marks the viewport dirty for the window's `paint_all`, so each event costs one paint;
`redraw()` paints now. Zoom
with the wheel or + / -, 0 for 1:1, F to fit; drag to pan; auto-fit (on at first, and
again after Fit) fits the drawing to the view after each event. `CytoplasmCanvas` is
the central widget's lower half: a bar (Fit, zoom, the legend) over the view.
"""

from PySide6.QtCore import QPointF, QRectF, Qt, Signal
from PySide6.QtGui import (QAction, QBrush, QColor, QFont, QFontMetricsF, QPainter,
                           QPainterPath, QPainterPathStroker, QPen, QPolygonF, QTransform)
from PySide6.QtWidgets import (QGraphicsItem, QGraphicsScene, QGraphicsView, QHBoxLayout,
                               QLabel, QPushButton, QToolButton, QVBoxLayout, QWidget)

from seekwhence.gui.models import cytoplasm as cy
from seekwhence.gui.paint import paint_all
from seekwhence.gui.render import render_scene_png

__all__ = ["MAX_FIT_SCALE", "ZOOM_STEP", "MARGIN", "LAYER_TITLES", "GlintItem", "ArcItem",
           "HullItem", "MarkerItem", "CytoplasmView", "CytoplasmCanvas", "legend_html"]

MAX_FIT_SCALE = 1.5
ZOOM_STEP = 1.25
MARGIN = 24            # around the layout, in scene units
HALO = 5               # room outside a shape for the flash halo
MARKER_LABEL_WIDTH = 200   # a proposal label's room, to the right of its marker
BACKGROUND = "#fbfbf8"
BASELINE = "#b0b0b0"
TEXT = "#1a1a1a"
PAGES = "[M86 pp. 44, 61-70, 73, 77, 92, 102, 106]; [FCCA pp. 58-60]"
LAYER_TITLES = {"sparks": "Sparks", "top-down": "Top-down layer (pseudo-gloms)",
                "values": "Bond values", "names": "Names"}


class GlintItem(QGraphicsItem):
    """A glint's teardrop at its layout position, the value above the name
    [M86 p. 102] (G-022, G-023); the tooltip lists its fields [M86 p. 61]."""

    def __init__(self, name, font, small_font):
        super().__init__()
        self.name = name
        self.font = font
        self.small_font = small_font
        self.box = None
        self.style = None
        self.rect = QRectF()
        self.setZValue(2)
        self.setCacheMode(QGraphicsItem.CacheMode.DeviceCoordinateCache)

    def set_glint(self, box, style, tooltip):
        """Take the box, the style and the tooltip, changing only what differs (Numbo
        GUI, `tree_view.py`'s `set_node`)."""
        rect = QRectF(0, 0, box.width, box.height)
        if rect != self.rect:
            self.prepareGeometryChange()
            self.rect = rect
        pos = QPointF(box.left, box.top)
        if self.pos() != pos:
            self.setPos(pos)
        if (box, style) != (self.box, self.style):
            self.box, self.style = box, style
            self.setOpacity(style.opacity)
            self.update()
        if tooltip != self.toolTip():
            self.setToolTip(tooltip)

    def boundingRect(self):
        """The box with room for the halo (Numbo GUI, `tree_view.py`)."""
        return self.rect.adjusted(-HALO, -HALO, HALO, HALO)

    def shape_path(self, rect):
        """The teardrop: a point at the top, a round bottom on the baseline
        [M86 p. 102] (G-022)."""
        w, h = rect.width(), rect.height()
        path = QPainterPath(QPointF(rect.x() + w / 2, rect.y()))
        path.cubicTo(QPointF(rect.x() + w, rect.y() + h * 0.45),
                     QPointF(rect.x() + w, rect.y() + h), QPointF(rect.x() + w / 2, rect.y() + h))
        path.cubicTo(QPointF(rect.x(), rect.y() + h), QPointF(rect.x(), rect.y() + h * 0.45),
                     QPointF(rect.x() + w / 2, rect.y()))
        return path

    def paint(self, painter, option, widget=None):
        """The halo when flashing, the teardrop, the value over the name [M86 p. 102]
        (G-023)."""
        s, r = self.style, self.rect
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if s.flash:
            painter.setPen(QPen(QColor(cy.FLASH_COLOR), 3.0))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawPath(self.shape_path(r.adjusted(-HALO / 2, -HALO / 2, HALO / 2, HALO / 2)))
        painter.setPen(QPen(QColor(s.border), s.border_width))
        painter.setBrush(QBrush(QColor(s.fill)))
        painter.drawPath(self.shape_path(r))
        painter.setPen(QColor(TEXT))
        painter.setFont(self.font)
        upper = QRectF(r.x(), r.y() + r.height() * 0.3, r.width(), r.height() * 0.32)
        painter.drawText(upper, Qt.AlignmentFlag.AlignCenter, str(self.box.value))
        painter.setFont(self.small_font)
        lower = QRectF(r.x(), r.y() + r.height() * 0.6, r.width(), r.height() * 0.3)
        painter.drawText(lower, Qt.AlignmentFlag.AlignCenter, self.box.label)


class ArcItem(QGraphicsItem):
    """An arc below the baseline, a bond or a spark, with the bond's value label at its
    lowest point [M86 p. 102] (G-022, G-023); the tooltip is `show`'s line for a bond
    [M86 p. 167] and the trace's for a spark [M86 p. 164]."""

    def __init__(self, name, font):
        super().__init__()
        self.name = name
        self.font = font
        self.shape_ = None
        self.style = None
        self.path = QPainterPath()
        self.rect = QRectF()
        self.setZValue(1)

    def set_arc(self, shape, style, tooltip):
        """Take the shape, the style and the tooltip, changing only what differs (Numbo
        GUI, `tree_view.py`'s `set_ends`)."""
        if shape != self.shape_:
            self.prepareGeometryChange()
            self.shape_ = shape
            a, b = QPointF(shape.x1, 0), QPointF(shape.x2, 0)
            self.path = QPainterPath(a)
            self.path.cubicTo(QPointF(shape.x1, shape.depth * 1.35),
                              QPointF(shape.x2, shape.depth * 1.35), b)
            self.rect = QRectF(shape.x1 - HALO, -HALO, shape.x2 - shape.x1 + 2 * HALO,
                               shape.depth + cy.LABEL_HEIGHT + 2 * HALO)
            self.update()
        if style != self.style:
            self.style = style
            self.setOpacity(style.opacity)
            self.setZValue(1 if style.dashed else 1.5)
            self.update()
        if tooltip != self.toolTip():
            self.setToolTip(tooltip)

    def boundingRect(self):
        """The arc's box, with its label and the halo (Numbo GUI, `tree_view.py`)."""
        return self.rect

    def shape(self):
        """A hover hits the stroke, not the whole box under the arc (G-022)."""
        stroker = QPainterPathStroker()
        stroker.setWidth(max(6.0, self.style.width + 4 if self.style else 6.0))
        return stroker.createStroke(self.path)

    def paint(self, painter, option, widget=None):
        """The halo when flashing, the arc (dashed for a spark) and the value label
        [M86 p. 102] (G-023)."""
        s = self.style
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if s.flash:
            painter.setPen(QPen(QColor(cy.FLASH_COLOR), s.width + 4))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawPath(self.path)
        pen = QPen(QColor(s.color), s.width)
        if s.dashed:
            pen.setStyle(Qt.PenStyle.DashLine)
        painter.setPen(pen)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawPath(self.path)
        label = self.shape_.label
        if label:
            painter.setFont(self.font)
            painter.setPen(QColor(s.color))
            rect = QRectF(self.shape_.mid_x - 24, self.shape_.depth - 2, 48, cy.LABEL_HEIGHT)
            painter.drawText(rect, Qt.AlignmentFlag.AlignCenter, label)


class HullItem(QGraphicsItem):
    """A glom's hull above the baseline, a rounded rectangle over its members with its
    label at the top left: an island of order [FCCA pp. 58-60]; the label is the type
    or, once dubbed, the class, form and purity [M86 p. 69], elided to the hull's width
    (the tooltip has it whole); a pseudo-glom's is dotted [M86 p. 73] (G-024). The
    tooltip is `show`'s glom line [M86 p. 167]."""

    def __init__(self, name, font, bold_font):
        super().__init__()
        self.name = name
        self.font = font
        self.bold_font = bold_font
        self.shape_ = None
        self.style = None
        self.rect = QRectF()
        self.setZValue(0.5)

    def set_hull(self, shape, style, tooltip):
        """Take the shape, the style and the tooltip, changing only what differs (Numbo
        GUI, `tree_view.py`'s `set_node`); an inner hull sits above an outer one."""
        if shape != self.shape_:
            self.prepareGeometryChange()
            self.shape_ = shape
            self.rect = QRectF(0, 0, shape.width, shape.height)
            self.setPos(QPointF(shape.left, shape.top))
            self.setZValue(0.9 - 0.05 * shape.depth)
            self.update()
        if style != self.style:
            self.style = style
            self.setOpacity(style.opacity)
            self.update()
        if tooltip != self.toolTip():
            self.setToolTip(tooltip)

    def boundingRect(self):
        """The hull with room for the halo (Numbo GUI, `tree_view.py`)."""
        return self.rect.adjusted(-HALO, -HALO, HALO, HALO)

    def shape(self):
        """A click or hover hits the hull's outline and its label band, not the whole
        inside, so that the glints and inner hulls under it stay reachable (G-025)."""
        path = QPainterPath()
        path.addRoundedRect(self.rect, cy.HULL_RADIUS, cy.HULL_RADIUS)
        stroker = QPainterPathStroker()
        stroker.setWidth(8.0)
        outline = stroker.createStroke(path)
        band = QPainterPath()
        band.addRect(QRectF(0, 0, self.rect.width(), cy.HULL_BAND))
        return outline.united(band)

    def paint(self, painter, option, widget=None):
        """The halo when flashing, the rounded hull (dotted for a pseudo-glom), and the
        label [M86 p. 69] (G-024)."""
        s, r = self.style, self.rect
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if s.flash:
            painter.setPen(QPen(QColor(cy.FLASH_COLOR), s.border_width + 3))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRoundedRect(r, cy.HULL_RADIUS, cy.HULL_RADIUS)
        pen = QPen(QColor(s.border), s.border_width)
        if s.dotted:
            pen.setStyle(Qt.PenStyle.DotLine)
        painter.setPen(pen)
        painter.setBrush(Qt.BrushStyle.NoBrush if s.fill == "transparent"
                         else QBrush(QColor(s.fill)))
        painter.drawRoundedRect(r, cy.HULL_RADIUS, cy.HULL_RADIUS)
        painter.setPen(QColor(s.border if s.dotted else TEXT))
        font = self.bold_font if s.bold else self.font
        painter.setFont(font)
        label_rect = QRectF(r.x() + 6, r.y() + 2, r.width() - 12, cy.HULL_BAND - 4)
        label = QFontMetricsF(font).elidedText(self.shape_.label, Qt.TextElideMode.ElideRight,
                                               label_rect.width())
        painter.drawText(label_rect, Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter,
                         label)


class MarkerItem(QGraphicsItem):
    """A proposal's marker [M86 p. 66]: a small triangle pointing at its target's tip,
    hollow while the Glomtester waits and filled once the Glommer does, with a bracket
    over the Glommer's members, in the bond family's colour (G-024). The tooltip is the
    trace's line, `Same -cover proposed --> glint1` [M86 p. 165]."""

    def __init__(self, number, font):
        super().__init__()
        self.number = number
        self.font = font
        self.shape_ = None
        self.style = None
        self.rect = QRectF()
        self.setZValue(2.5)

    def set_marker(self, shape, style, tooltip):
        """Take the shape, the style and the tooltip, changing only what differs (Numbo
        GUI, `tree_view.py`'s `set_node`)."""
        if shape != self.shape_:
            self.prepareGeometryChange()
            self.shape_ = shape
            left = min(shape.x, shape.x1) - cy.MARKER_SIZE
            right = max(shape.x, shape.x2) + cy.MARKER_SIZE
            if shape.label:
                right = max(right, shape.x + MARKER_LABEL_WIDTH)
            self.rect = QRectF(left, shape.y - cy.MARKER_SIZE - cy.LABEL_HEIGHT,
                               right - left, cy.MARKER_SIZE + cy.LABEL_HEIGHT + 2)
            self.update()
        if style != self.style:
            self.style = style
            self.update()
        if tooltip != self.toolTip():
            self.setToolTip(tooltip)

    def boundingRect(self):
        """The marker, its bracket and its label, with the halo (Numbo GUI,
        `tree_view.py`)."""
        return self.rect.adjusted(-HALO, -HALO, HALO, HALO)

    def paint(self, painter, option, widget=None):
        """The triangle (filled for a Glommer), the bracket over the members and the
        family and test, `Same -cover` [M86 p. 165] (G-024)."""
        s, m = self.style, self.shape_
        size = cy.MARKER_SIZE
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        color = QColor(s.color)
        triangle = QPolygonF([QPointF(m.x, m.y), QPointF(m.x - size / 2, m.y - size),
                              QPointF(m.x + size / 2, m.y - size)])
        if s.flash:
            painter.setPen(QPen(QColor(cy.FLASH_COLOR), 3.0))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawPolygon(triangle)
        painter.setPen(QPen(color, 1.2))
        painter.setBrush(QBrush(color) if s.filled else Qt.BrushStyle.NoBrush)
        painter.drawPolygon(triangle)
        if m.x2 > m.x1:
            painter.setPen(QPen(color, 1.5))
            y = m.y - size - 1
            painter.drawLine(QPointF(m.x1, y), QPointF(m.x2, y))
            painter.drawLine(QPointF(m.x1, y), QPointF(m.x1, y + 4))
            painter.drawLine(QPointF(m.x2, y), QPointF(m.x2, y + 4))
        if m.label:
            painter.setFont(self.font)
            painter.setPen(QColor(cy.MARKER_LABEL))
            label_rect = QRectF(m.x - size, m.y - size - cy.LABEL_HEIGHT - 1,
                                MARKER_LABEL_WIDTH, cy.LABEL_HEIGHT)
            painter.drawText(label_rect, Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter,
                             m.label)


class CytoplasmView(QGraphicsView):
    """The canvas (see the module docstring; Numbo GUI, `tree_view.py`; G-022, G-023,
    G-024). `history` is the window's `EventLog`, read at `position` (G-016);
    `selection` the window's shared `Selection` (one of the view's own when None,
    G-025). `glint_items`, `arc_items`, `hull_items` and `marker_items` are the scene's
    items by name (markers by task number); `layout` the latest `Layout`; `paints` how
    many times the viewport has been painted; `layer_actions` the four checkable
    layer actions (G-026). `selected(name)` is emitted after a click selects an element
    (an empty name: cleared)."""

    pane_id = "P2"
    title = "Cytoplasm"
    pages = PAGES
    selected = Signal(str)

    def __init__(self, history, parent=None, selection=None):
        super().__init__(parent)
        scene = QGraphicsScene(self)
        scene.setItemIndexMethod(QGraphicsScene.ItemIndexMethod.NoIndex)
        self.setScene(scene)
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
        self.setTransformationAnchor(QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.ViewportAnchor.AnchorViewCenter)
        self.setBackgroundBrush(QColor(BACKGROUND))
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.value_font = QFont(self.font())
        self.value_font.setPointSizeF(11)
        self.value_font.setBold(True)
        self.name_font = QFont(self.font())
        self.name_font.setPointSizeF(7)
        self.label_font = QFont(self.font())
        self.label_font.setPointSizeF(8)
        self.hull_font = QFont(self.font())
        self.hull_font.setPointSizeF(8)
        self.hull_bold_font = QFont(self.hull_font)
        self.hull_bold_font.setBold(True)
        self.history = history
        self.model = cy.CytoplasmModel(selection)
        self.model.selection.subscribe(self._on_selection)
        self.position = -1
        self.events = 0
        self.layout = None
        self.glint_items = {}
        self.arc_items = {}
        self.hull_items = {}
        self.marker_items = {}
        self.baseline = scene.addLine(0, 0, cy.SPACING, 0, QPen(QColor(BASELINE), 1.0))
        self.baseline.setZValue(0)
        self.paints = 0
        self.auto_fit = True
        self.layer_actions = {}
        for name, title in LAYER_TITLES.items():
            action = QAction(title, self)
            action.setCheckable(True)
            action.setChecked(self.model.layers[name])
            action.toggled.connect(lambda on, name=name: self.set_layer(name, on))
            self.layer_actions[name] = action
        self.layer_actions["top-down"].setToolTip(
            "The pseudo-gloms, in grey: the template glom [M86 p. 73], the gnoth-gloms "
            "[M86 p. 77], the frame gloms; shown by default, so the current parse is "
            "visible (ANSWERS.md 10.4)")
        self._apply_scroll_policy()
        self.redraw()

    # -- the layers and the selection (G-025, G-026) --------------------------------------------

    def set_layer(self, name: str, on: bool) -> None:
        """Switch a layer on or off and paint now (PANES.md P2; G-026); the action
        follows."""
        self.model.set_layer(name, on)
        action = self.layer_actions.get(name)
        if action is not None and action.isChecked() != bool(on):
            action.setChecked(bool(on))
        self.redraw(now=True)

    def select(self, name: str | None) -> None:
        """Select an element by name in the shared selection (None clears) and paint
        now (G-025)."""
        self.model.select(name)

    def _on_selection(self, selection) -> None:
        """The shared selection changed (from this pane or another): repaint, and say
        so (G-025)."""
        if self.layout is not None:
            self.redraw(now=True)
        self.selected.emit(selection.name or "")

    def element_at(self, pos) -> str | None:
        """The name of the element drawn at a viewport position (a glint, a hull, an
        arc; a marker gives its target), or None (G-025)."""
        for item in self.items(pos):
            if isinstance(item, (GlintItem, ArcItem, HullItem)):
                return item.name
            if isinstance(item, MarkerItem):
                return self.model.proposals[item.number].target if (
                    item.number in self.model.proposals) else None
        return None

    def mousePressEvent(self, event):
        """A left click selects the element under the mouse, or clears the selection on
        empty canvas; then the drag-to-pan goes on (PANES.md P2 "Click to select";
        G-025)."""
        if event.button() == Qt.MouseButton.LeftButton:
            name = self.element_at(event.position().toPoint())
            if name is None:
                self.model.select(None)
            else:
                self.model.selection.toggle(*self._selection_of(name))
        super().mousePressEvent(event)

    def _selection_of(self, name) -> tuple:
        """What selecting the element means: a glom with everything under it, an arc
        with its ends, a glint alone (G-025)."""
        model = self.model
        if name in model.gloms or name in model.hull_ghosts:
            return ("glom", name, model.under(name))
        if name in model.glints:
            return ("glint", name, ())
        arc = {m.name: m for m in model.arcs()}.get(name)
        return (arc.kind, name, (arc.a, arc.b)) if arc is not None else ("glint", name, ())

    # -- the view protocol (G-011, G-016) --------------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-016)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model and take the snapshot in force at `position` (G-021); a
        `start` clears the scene."""
        if event.kind == "start":
            self.events = 0
            self.clear()
        self.events += 1
        self.model.on_event(event)
        self.model.update(self.history.state_at(self.position))

    def clear(self) -> None:
        """Remove every glint and arc from the scene (Numbo GUI, `tree_view.py`)."""
        scene = self.scene()
        for items in (self.glint_items, self.arc_items, self.hull_items, self.marker_items):
            for item in items.values():
                scene.removeItem(item)
            items.clear()
        self.layout = None

    def highlight_term(self, n: int) -> str | None:
        """Highlight the glint of term `n` (the sequence pane's click, PANES.md P1),
        select it in the shared selection (G-025) and paint now; returns the glint's
        name, or None when there is none (the selection is cleared) (G-023)."""
        glint = self.model.glint_at(n)
        name = glint.name if glint is not None else None
        self.model.highlight(name)
        self.model.select(name)
        self.redraw(now=True)
        return name

    def highlight(self, name: str | None) -> None:
        """Highlight the element of that name, a glint or an arc (the socratoplasm pane's
        hover over a bond, PANES.md P3; an empty name or None clears), and paint now
        (G-031)."""
        self.model.highlight(name or None)
        self.redraw(now=True)

    def redraw(self, now=True) -> None:
        """Lay the model out, bring the scene in line and paint now (`now` false: mark
        the viewport dirty for the window's `paint_all`) (Numbo GUI, `tree_view.py`)."""
        self.layout = cy.layout(self.model)
        self._rebuild()
        if self.isVisible():
            self.viewport().update()
            if now:
                paint_all((self.scene(),))

    def _rebuild(self):
        """Bring the scene's items in line with the layout: one per glint, one per arc,
        by name (Numbo GUI, `tree_view.py`'s `_rebuild`)."""
        model, scene, lay, index = self.model, self.scene(), self.layout, self.model.index
        wanted = {box.name for box in lay.glints}
        for name in [n for n in self.glint_items if n not in wanted]:
            scene.removeItem(self.glint_items.pop(name))
        for box in lay.glints:
            item = self.glint_items.get(box.name)
            if item is None:
                item = self.glint_items[box.name] = GlintItem(box.name, self.value_font,
                                                              self.name_font)
                scene.addItem(item)
            mark = model.glints[box.name]
            lit = model.highlighted == box.name or model.is_selected(box.name)
            item.set_glint(box, cy.glint_style(mark, index, lit), cy.glint_tooltip(mark))
        marks = {m.name: m for m in model.arcs()}
        wanted = {arc.name for arc in lay.arcs}
        for name in [n for n in self.arc_items if n not in wanted]:
            scene.removeItem(self.arc_items.pop(name))
        for arc in lay.arcs:
            item = self.arc_items.get(arc.name)
            if item is None:
                item = self.arc_items[arc.name] = ArcItem(arc.name, self.label_font)
                scene.addItem(item)
            mark = marks[arc.name]
            lit = model.highlighted == arc.name or model.is_selected(arc.name)
            style = (cy.spark_style(mark, index, lit) if mark.kind == "spark"
                     else cy.bond_style(mark, index, lit))
            item.set_arc(arc, style, cy.arc_tooltip(mark, model.reigning))
        hulls = {m.name: m for m in model.hulls()}
        wanted = {hull.name for hull in lay.hulls}
        for name in [n for n in self.hull_items if n not in wanted]:
            scene.removeItem(self.hull_items.pop(name))
        for hull in lay.hulls:
            item = self.hull_items.get(hull.name)
            if item is None:
                item = self.hull_items[hull.name] = HullItem(hull.name, self.hull_font,
                                                             self.hull_bold_font)
                scene.addItem(item)
            mark = hulls[hull.name]
            item.set_hull(hull, cy.hull_style(mark, index, model.is_selected(hull.name)),
                          cy.hull_tooltip(mark))
        wanted = {marker.number for marker in lay.markers}
        for number in [n for n in self.marker_items if n not in wanted]:
            scene.removeItem(self.marker_items.pop(number))
        for marker in lay.markers:
            item = self.marker_items.get(marker.number)
            if item is None:
                item = self.marker_items[marker.number] = MarkerItem(marker.number,
                                                                     self.label_font)
                scene.addItem(item)
            mark = model.proposals[marker.number]
            item.set_marker(marker, cy.marker_style(mark, index), cy.marker_tooltip(mark))
        self.baseline.setLine(lay.left - MARGIN / 2, 0, lay.right + MARGIN / 2, 0)
        scene.setSceneRect(self.content_rect())
        if self.auto_fit:
            self._fit()

    def content_rect(self):
        """The layout with a margin around it (Numbo GUI, `tree_view.py`)."""
        lay = self.layout
        return QRectF(lay.left - MARGIN, lay.top - MARGIN, lay.width + 2 * MARGIN,
                      lay.height + 2 * MARGIN)

    def paintEvent(self, event):
        """Count the paints, for the tests (Numbo GUI, `tree_view.py`)."""
        self.paints += 1
        super().paintEvent(event)

    # -- zoom, pan and fit (Numbo GUI, `tree_view.py`) ------------------------------------------

    def scale_factor(self):
        """The current zoom (Numbo GUI, `tree_view.py`)."""
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
        """Fit the drawing to the view, and keep doing so (auto-fit) (Numbo GUI,
        `tree_view.py`)."""
        self._set_auto_fit(True)
        self._fit()

    def zoom_by(self, factor):
        """Zoom by a factor; turns auto-fit off (Numbo GUI, `tree_view.py`)."""
        self._set_auto_fit(False)
        self.scale(factor, factor)

    def zoom_in(self):
        """Zoom in one step (Numbo GUI, `tree_view.py`)."""
        self.zoom_by(ZOOM_STEP)

    def zoom_out(self):
        """Zoom out one step (Numbo GUI, `tree_view.py`)."""
        self.zoom_by(1 / ZOOM_STEP)

    def reset_zoom(self):
        """Back to 1:1; turns auto-fit off (Numbo GUI, `tree_view.py`)."""
        self._set_auto_fit(False)
        self.setTransform(QTransform())

    def wheelEvent(self, event):
        """Zoom with the wheel (Numbo GUI, `tree_view.py`)."""
        steps = event.angleDelta().y() / 120
        if steps:
            self.zoom_by(ZOOM_STEP ** steps)
        event.accept()

    def keyPressEvent(self, event):
        """+ / - zoom, 0 is 1:1, F fits (Numbo GUI, `tree_view.py`)."""
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
        """Keep the fit on resize (Numbo GUI, `tree_view.py`)."""
        super().resizeEvent(event)
        if self.auto_fit:
            self._fit()

    # -- pictures ------------------------------------------------------------------------------

    def render_png(self, path, scale=1.0):
        """Render the whole scene (at `scale`) to a PNG at `path` (Numbo GUI,
        `render.py`)."""
        return render_scene_png(self, path, scale)


def legend_html():
    """The legend: the three bond colours, the spark, the flash, and the pages behind
    the drawing [M86 p. 102]; [FCCA pp. 58-60] (Numbo GUI, `tree_view.py`'s
    `legend_html`)."""
    parts = [f'<span style="color:{cy.BOND_COLORS[t]}"><b>&#9644;</b></span>&nbsp;{t}'
             for t in ("Same", "Succ", "Pred")]
    parts.append(f'<span style="color:{cy.SPARK_COLOR}"><b>&#9476;</b></span>&nbsp;spark')
    parts.append(f'<span style="color:{cy.FLASH_COLOR}"><b>&#9711;</b></span>&nbsp;just made')
    parts.append(f'<span style="color:{cy.HULL_BORDER}"><b>&#9645;</b></span>&nbsp;glom '
                 '[M86 p. 62]')
    parts.append(f'<span style="color:{cy.PSEUDO_BORDER}"><b>&#9645;</b></span>&nbsp;pseudo-glom '
                 '(grey)')
    parts.append('<span style="color:#666">&#9663; proposal [M86 p. 66]</span>')
    parts.append('<span style="color:#666">value: in the reigning class [M86 p. 102]</span>')
    return "&nbsp;&nbsp; ".join(parts)


class CytoplasmCanvas(QWidget):
    """The central widget's lower half: a bar (Fit, zoom buttons, the four layer
    toggles, the legend) over the `CytoplasmView` (`view`); the view protocol is
    delegated to the view (Numbo GUI, `tree_view.py`'s `TreeCanvas`; G-011, G-026)."""

    pane_id = "P2"
    title = "Cytoplasm"
    pages = PAGES

    def __init__(self, history, parent=None, selection=None):
        super().__init__(parent)
        self.view = CytoplasmView(history, selection=selection)
        self.view.setObjectName("cytoplasm-view")
        self.setToolTip(f"P2 Cytoplasm: {PAGES}")
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
        self.layer_buttons = {}
        for name, action in self.view.layer_actions.items():
            button = QToolButton()
            button.setDefaultAction(action)
            button.setCheckable(True)
            button.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            button.setText({"sparks": "sparks", "top-down": "top-down", "values": "values",
                            "names": "names"}[name])
            bar.addWidget(button)
            self.layer_buttons[name] = button
        bar.addSpacing(12)
        self.legend = QLabel(legend_html())
        self.legend.setObjectName("cytoplasm-legend")
        self.legend.setMinimumWidth(1)       # clipped when narrow, not widening the window (G-058)
        self.legend.setToolTip("Same, Succ and Pred are Hofstadter's sameness, successorship "
                               "and predecessorship glue [FCCA pp. 58-60]; the arcs and "
                               "values follow Ch. 3 Figure 1 [M86 p. 102]; the hulls are "
                               "the gloms [M86 p. 62], nested as islands of order "
                               "[FCCA pp. 58-60], dubbed [M86 p. 69]; a dotted hull is a "
                               "pseudo-glom [M86 pp. 73, 77]; a triangle a proposal "
                               "[M86 p. 66]")
        bar.addWidget(self.legend)
        bar.addStretch(1)
        box = QVBoxLayout(self)
        box.setContentsMargins(0, 0, 0, 0)
        box.setSpacing(0)
        box.addLayout(bar)
        box.addWidget(self.view, 1)

    @property
    def model(self):
        """The view's `CytoplasmModel` (G-021)."""
        return self.view.model

    @property
    def paints(self) -> int:
        """The view's paint count (Numbo GUI, `tree_view.py`'s `paints`)."""
        return self.view.paints

    @property
    def events(self) -> int:
        """The events the view has seen in this run (G-011)."""
        return self.view.events

    def scene(self):
        """The view's scene, for the window's `paint_all` (Numbo GUI, `paint.py`)."""
        return self.view.scene()

    def set_position(self, index: int) -> None:
        """The view protocol, delegated (G-016)."""
        self.view.set_position(index)

    def on_event(self, event) -> None:
        """The view protocol, delegated (G-011)."""
        self.view.on_event(event)

    def redraw(self, now=False) -> None:
        """The view protocol, delegated (Numbo GUI, `paint.py`)."""
        self.view.redraw(now=now)

    def highlight_term(self, n: int) -> str | None:
        """Highlight term `n`'s glint (PANES.md P1; G-023)."""
        return self.view.highlight_term(n)

    def highlight(self, name: str | None) -> None:
        """Highlight a glint or an arc by name; empty or None clears (PANES.md P3;
        G-031)."""
        self.view.highlight(name)

    @property
    def layer_actions(self) -> dict:
        """The view's four layer actions, for the View menu (G-026)."""
        return self.view.layer_actions

    @property
    def selection(self):
        """The shared `Selection` the view's model uses (G-025)."""
        return self.view.model.selection

    def select(self, name: str | None) -> None:
        """Select an element by name (None clears) (G-025)."""
        self.view.select(name)

    def render_png(self, path, scale=1.0):
        """Render the view's scene to a PNG (Numbo GUI, `render.py`)."""
        return self.view.render_png(path, scale)

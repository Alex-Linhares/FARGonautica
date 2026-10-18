"""The table pane (P1): the table from above with its objects, the touch, the answer,
the groups, the proposals and the correspondences, with its overlays and Eliza's view, a
`QGraphicsView` over `models/table.py`'s `TableModel` (PANES.md P1 and "P1 overlays";
Numbo GUI, `tree_view.py`; G-017 to G-026).

The picture is French's screen's: the table as a rectangle with "Henry" above it and
"Eliza" below, each object drawn at its place and size, the touched object with an arrow
and an **H**, and Eliza's answer with an arrow and an **E**, Figure 4.1 and the dumps of
the chapter 4 run [F95 pp. 96-107 (img)] (SPEC.md 6c.14); a gestalt group as a box with a
wavy border, a subgroup as a dashed rectangle captioned "Build new subgroup" when just
built [F95 p. 102 (img)]; a correspondence as a line between its ends' centres, dashed and
flashing while tentative or competing [F95 p. 105], solid in the Worldview, thicker
between groups [F95 p. 104 (img)], its ends flashing while new [F95 p. 101]. The icons
are **new drawings, not French's** (`icons.py`; ANSWERS.md 4), and the legend on the bar
says so; the proposals' dotted outlines, the fading of what was removed and the noticed
ring are the GUI's own (G-018, G-021). The overlays are **GUI design, not French's**
(`models/overlays.py`; G-025, G-026): `RegionsItem` paints the nine regions of D-004 as
cells with soft bands along their blurry boundaries [F95 p. 65], their engine names, the
noticed objects' last drawn regions and, with the ranks layer on, each region's rank
from the touched object (D-032); `SalienceItem` tints every item in view by its
salience (D-031, D-100), whose nine terms the hover text lists; Eliza's view turns the whole
layout round (D-005). The three layers and Eliza's view are checkable actions
(`layer_actions`, `eliza_action`), shown as buttons on the bar and in the window's View
menu, and remembered with the window's settings (Seek-Whence GUI, `cytoplasm_view.py`'s
layers).

`TableView` is a GUI-thread observer (the window's `add_view`), after the Numbo GUI's
`TreeView`: `set_problem` gives it the run's problem before the run starts (G-017);
`set_position` and `on_event` feed the model and take the snapshot in force with the terms
recorded beside it; `redraw(now=False)` lays the model out, brings the scene's items in
line with the layout (one item per object, arrow, group, proposal and correspondence, kept
by name and changed only where they differ) and marks the viewport dirty for the window's
`paint_all`, so each event costs one paint; `redraw()` paints now. Hovering shows the
model's hover text (G-018, G-023). A left click toggles the element under the mouse in
the window's shared selection, a click on empty table clears it, and the selected
elements are drawn with a highlight (G-022). Zoom with the wheel or + / -, 0 for 1:1, F
to fit; drag to pan; auto-fit (on at first, and again after Fit) fits the table to the
view after each event and resize, never zooming in past `MAX_FIT_SCALE` (G-019).
`TableCanvas` is the central widget: a bar (Fit, zoom, the legend) over the view.
"""

from PySide6.QtCore import QLineF, QPointF, QRectF, QSize, Qt, Signal
from PySide6.QtGui import (QAction, QBrush, QColor, QFont, QFontMetricsF, QPainter,
                           QPainterPath, QPainterPathStroker, QPen, QPolygonF, QTransform)
from PySide6.QtWidgets import (QGraphicsItem, QGraphicsLineItem, QGraphicsPathItem,
                               QGraphicsScene, QGraphicsSimpleTextItem, QGraphicsView,
                               QHBoxLayout, QLabel, QPushButton, QSizePolicy, QToolButton,
                               QVBoxLayout, QWidget)

from tabletop.gui import icons
from tabletop.gui.models import overlays as ovl
from tabletop.gui.models import structures as st
from tabletop.gui.models.selection import Selection
from tabletop.gui.models.table import TableModel
from tabletop.gui.paint import paint_all
from tabletop.gui.render import render_scene_png

__all__ = ["MAX_FIT_SCALE", "OVERLAY_PAGES", "PAGES", "SCALE", "ZOOM_STEP", "ArrowItem",
           "CorrespondenceItem", "GroupItem", "ObjectItem", "ProposalItem", "RegionsItem",
           "RolesItem", "SalienceItem", "TableCanvas", "TableView", "legend_text",
           "overlay_tooltips"]

PAGES = "Fig. 4.1 and the dumps [F95 pp. 96-107 (img)]; the palette [F95 p. 26 (img)]"
OVERLAY_PAGES = (f"overlays and Eliza's view: {ovl.NOT_FRENCHS}, from the prose "
                 "[F95 pp. 65-67] (D-004, D-005, D-031, D-032)")
SCALE = 6.0            # scene units per table unit (the README's hero figure's scale)
MAX_FIT_SCALE = 1.5
ZOOM_STEP = 1.25
MARGIN = 30            # around the table, in scene units: room for the names
NAME_GAP = 10          # between the table's edge and a name's baseline
BACKGROUND = "#fcfcfb"
TABLE_FILL = "#f4efe4"
TABLE_EDGE = "#a89f8c"
INK = "#0b0b0b"
INK2 = "#52514e"
NOTICED = "#2a78d6"          # the light ring of a noticed object (G-018)
SELECTED = "#e07b00"         # the highlight of the selected elements (G-022)
GROUP_INK = "#2c2c2c"        # the groups' borders (G-021)
PROPOSAL_INK = "#6a6a6a"     # the proposals' dotted outlines (G-021)
LINE_INK = "#161616"         # the correspondence lines (G-021)
ARROW_COLORS = {"H": "#1a1a1a", "E": "#1a1a1a"}
ARROW_WIDTH = 2.0
ARROW_HEAD = 7.0             # the head's length, in scene units
LETTER_SIZE = 15
NAME_SIZE = 15
CAPTION_SIZE = 11
STAGE_DOT = 3.0              # a proposal's stage dots, in scene units (G-021)
HIT_PAD = 2.0
BAND = 7.0                   # the width of a border's click band, in scene units (G-022)
DASH = [6.0, 4.0]            # the dashed lines' pattern, in pen widths (G-021)
Z_GESTALT, Z_SUBGROUP, Z_PROPOSAL, Z_OBJECT, Z_LINE, Z_ARROW = 0.0, 0.3, 0.6, 1.0, 1.5, 2.0
Z_REGIONS, Z_HEAT = -2.5, -1.5                  # between the table's fill and the structures (G-025)
Z_ROLES = 1.8                                   # the role marks over the lines, under the arrows (G-061)
ROLE_INK = "#5b2a86"         # the role badges and marks (G-061)
ROLE_FILL = "#efe6f7"        # a badge's background (G-061)
BARRIER_INK = "#b3261e"      # a barrier's bar and the claim it makes (G-061)
MIDLINE_FILL = "#5b2a86"     # the midline band's hatch, faint (G-061)
BADGE_SIZE = 9
MARK_SIZE = 8
ROLE_MARK = 3.0              # the radius of a role-proposed line's mark, in scene units (G-061)
REGION_INK = "#6b5f4e"       # the regions' boundaries and names (G-025)
DRAWN_FILL = "#2a78d6"       # a noticed object's last drawn region, tinted (G-025)
RANK_INK = "#8a5a00"         # the region ranks (G-025)
HEAT = "#e3421b"             # the salience tint, by share (G-025)
HEAT_ALPHA = (20, 110)       # the tint's alpha at share 0 and 1 (G-025)
REGION_NAME_SIZE = 9
RANK_SIZE = 26
HEAT_VALUE_SIZE = 9
ELIZA_TITLE = "Eliza's view"


def legend_text(layout=None) -> str:
    """The bar's legend: the pane, its pages and the icons' caption (ANSWERS.md 4;
    G-018), and, given a layout with overlays on or Eliza's view, their note saying they
    are GUI design, not French's (G-025, G-026)."""
    text = f"{icons.NOT_FRENCHS} · P1 Table: {PAGES}"
    if layout is None:
        return text
    notes = []
    if layout.eliza:
        notes.append(f"{ELIZA_TITLE} (the table turned round; {ovl.NOT_FRENCHS})")
    if layout.overlays.note:
        notes.append(layout.overlays.note)
    return " · ".join([text] + notes)


def overlay_tooltips() -> dict:
    """The tooltips of the three layers and Eliza's view, each saying what it shows,
    its decision and that it is GUI design, not French's (ANSWERS.md 4; G-025,
    G-026)."""
    return {
        "regions": ("Regions: the nine regions of the tic-tac-toe board with their blurry "
                    "boundaries (D-004) [F95 p. 65], their names, and each noticed object's "
                    f"last drawn region. {ovl.NOT_FRENCHS}."),
        "salience": ("Salience: every item in view tinted by its salience (D-031) [F95 "
                     "pp. 66-67], the nine terms on hover; dated by the snapshot in force. "
                     f"{ovl.NOT_FRENCHS}."),
        "ranks": ("Region ranks: the nine regions ranked from the touched object, the "
                  "diagonal first, the opposite next (D-032) [F95 p. 66], with their "
                  f"preference weights. {ovl.NOT_FRENCHS}."),
        "eliza": (f"{ELIZA_TITLE}: the whole table turned by 180 degrees, as Eliza sees it "
                  "(the engine has no sides beyond the regions, D-005). "
                  f"{ovl.NOT_FRENCHS}."),
        "roles": ("Roles: the standing role tags as badges on the objects and groups (D-079; "
                  "the codes of the registry), the midline band (S1) and the gaps (S2), the "
                  "mirror and point images (G1, G2) of the selected object (the touched "
                  "one while nothing is selected), the re-cast's candidates by tier (K1), "
                  "the barriers with the counterparts they claim (S5, S6) and the axis "
                  f"(G3); exact at every event. {ovl.NOT_FRENCHS}."),
    }


def _font(size: int, bold: bool = False) -> QFont:
    font = QFont()
    font.setPixelSize(size)
    font.setBold(bold)
    return font


def _rect(box) -> QRectF:
    """A table-unit box as a scene rectangle."""
    left, top, right, bottom = box
    return QRectF(left * SCALE, top * SCALE, (right - left) * SCALE, (bottom - top) * SCALE)


def _with_alpha(colour: str, alpha: int) -> QColor:
    c = QColor(colour)
    c.setAlpha(alpha)
    return c


def _band(path: QPainterPath, width: float = BAND) -> QPainterPath:
    """The click band along a path's stroke (G-022)."""
    stroker = QPainterPathStroker()
    stroker.setWidth(width)
    return stroker.createStroke(path)


class ObjectItem(QGraphicsItem):
    """One object's icon at its place (D-001 to D-003), with a light ring when noticed
    (G-018) and a highlight when selected (G-022); its tooltip is the model's hover
    text."""

    def __init__(self, obj):
        super().__init__()
        self.obj = None
        self.selected = False
        self.setAcceptHoverEvents(True)
        self.set_form(obj)

    @property
    def name(self) -> str:
        """The object's id (G-022)."""
        return self.obj.id

    def set_form(self, obj) -> None:
        """A new state of the object: moved, turned or marked; costs nothing when
        unchanged (G-018)."""
        if obj == self.obj:
            return
        self.prepareGeometryChange()
        self.obj = obj
        self.setPos(obj.x * SCALE, obj.y * SCALE)
        self.update()


    def set_selected(self, selected: bool) -> None:
        """Highlight the object when it is in the shared selection (G-022)."""
        if selected != self.selected:
            self.selected = selected
            self.update()

    def _ring(self) -> QRectF:
        w, h = self.obj.width * SCALE, self.obj.height * SCALE
        pad = HIT_PAD * SCALE / 2
        return QRectF(-w / 2 - pad, -h / 2 - pad, w + 2 * pad, h + 2 * pad)

    def boundingRect(self) -> QRectF:
        """The footprint box as it lies, plus the ring's room (D-002)."""
        return self._ring().adjusted(-2, -2, 2, 2)

    def paint(self, painter, option, widget=None):
        """The ring, then the icon turned to its orientation (`icons.paint_icon`;
        G-016, G-018); the selection's ring over it (G-022)."""
        obj = self.obj
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if obj.noticed:
            painter.setPen(QPen(_with_alpha(NOTICED, 80), 1.2))
            painter.setBrush(QBrush(_with_alpha(NOTICED, 14)))
            painter.drawRoundedRect(self._ring(), 4, 4)
        up_w, up_h = ((obj.height, obj.width) if obj.orientation in ("left", "right")
                      else (obj.width, obj.height))
        icons.paint_icon(painter, obj.type, up_w * SCALE, up_h * SCALE, obj.orientation)
        if self.selected:
            painter.setPen(QPen(QColor(SELECTED), 2.0))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRoundedRect(self._ring(), 4, 4)


class ArrowItem(QGraphicsItem):
    """An arrow with its letter at the tail: **H** for the touch, **E** for the answer
    [F95 pp. 98-107 (img)] (SPEC.md 6c.14; G-018)."""

    def __init__(self, arrow):
        super().__init__()
        self.arrow = None
        self.font = _font(LETTER_SIZE, bold=True)
        self.set_form(arrow)

    def set_form(self, arrow) -> None:
        """A new arrow; costs nothing when unchanged (G-018)."""
        if arrow == self.arrow:
            return
        self.prepareGeometryChange()
        self.arrow = arrow
        self.setToolTip("Henry touches this object (H)" if arrow.letter == "H"
                        else "Eliza's answer (E)")
        self.update()


    def _points(self):
        a = self.arrow
        return (QPointF(a.tail[0] * SCALE, a.tail[1] * SCALE),
                QPointF(a.head[0] * SCALE, a.head[1] * SCALE))

    def _letter_rect(self, tail) -> QRectF:
        metrics = QFontMetricsF(self.font)
        w, h = metrics.horizontalAdvance(self.arrow.letter) + 4, metrics.height()
        above = self.arrow.tail[1] < self.arrow.head[1]
        y = tail.y() - h if above else tail.y()
        return QRectF(tail.x() - w / 2, y, w, h)

    def boundingRect(self) -> QRectF:
        """The line, the head and the letter (G-018)."""
        tail, head = self._points()
        rect = QRectF(tail, head).normalized().united(self._letter_rect(tail))
        return rect.adjusted(-ARROW_HEAD, -ARROW_HEAD, ARROW_HEAD, ARROW_HEAD)

    def paint(self, painter, option, widget=None):
        """The shaft, a filled head at the object and the letter at the tail (G-018)."""
        tail, head = self._points()
        colour = QColor(ARROW_COLORS[self.arrow.letter])
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setPen(QPen(colour, ARROW_WIDTH, Qt.PenStyle.SolidLine, Qt.PenCapStyle.RoundCap))
        line = QLineF(tail, head)
        painter.drawLine(line)
        tip = QLineF(head, tail)
        tip.setLength(ARROW_HEAD)
        left, right = QLineF(tip), QLineF(tip)
        left.setAngle(tip.angle() + 25)
        right.setAngle(tip.angle() - 25)
        painter.setBrush(QBrush(colour))
        painter.setPen(Qt.PenStyle.NoPen)
        painter.drawPolygon(QPolygonF([head, left.p2(), right.p2()]))
        painter.setPen(QPen(colour))
        painter.setFont(self.font)
        painter.drawText(self._letter_rect(tail), Qt.AlignmentFlag.AlignCenter, self.arrow.letter)


class GroupItem(QGraphicsItem):
    """A group's box: a wavy border for a gestalt group, a dashed rectangle for a
    subgroup, captioned "Build new subgroup" while new, fading once removed (SPEC.md
    6c.14; [F95 p. 102 (img)]; G-021); its click band is the border (G-022)."""

    def __init__(self, form):
        super().__init__()
        self.form = None
        self.selected = False
        self.path = QPainterPath()
        self.font = _font(CAPTION_SIZE)
        self.setAcceptHoverEvents(True)
        self.set_form(form)

    @property
    def name(self) -> str:
        """The group's name (G-022)."""
        return self.form.name

    def set_form(self, form) -> None:
        """A new state of the group; costs nothing when unchanged (G-021)."""
        if form == self.form:
            return
        self.prepareGeometryChange()
        self.form = form
        self.path = QPainterPath()
        if form.wavy:
            points = [QPointF(x * SCALE, y * SCALE) for x, y in st.wavy_outline(form.box)]
            self.path.addPolygon(QPolygonF(points))
            self.path.closeSubpath()
        else:
            self.path.addRect(_rect(form.box))
        self.update()

    def set_selected(self, selected: bool) -> None:
        """Highlight the box when the group is in the shared selection (G-022)."""
        if selected != self.selected:
            self.selected = selected
            self.update()

    def _caption_rect(self) -> QRectF:
        rect = _rect(self.form.box)
        metrics = QFontMetricsF(self.font)
        w = metrics.horizontalAdvance(self.form.caption) + 6
        return QRectF(rect.center().x() - w / 2, rect.bottom() + 2, w, metrics.height())

    def shape(self) -> QPainterPath:
        """The border's band, and the caption (G-022)."""
        band = _band(self.path)
        if self.form.caption:
            band.addRect(self._caption_rect())
        return band

    def boundingRect(self) -> QRectF:
        """The box with its wave and its caption (G-021)."""
        rect = self.path.boundingRect().adjusted(-BAND, -BAND, BAND, BAND)
        return rect.united(self._caption_rect()) if self.form.caption else rect

    def paint(self, painter, option, widget=None):
        """The border by `box_style`, the selection's halo under it, the caption below
        (G-021, G-022)."""
        style = st.box_style(self.form)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        if self.selected:
            painter.setPen(QPen(_with_alpha(SELECTED, 150), style.width + 4))
            painter.drawPath(self.path)
        pen = QPen(_with_alpha(GROUP_INK, style.alpha), style.width)
        pen.setDashPattern(DASH if style.border == "dashed" else [5.0, 3.0])
        painter.setPen(pen)
        painter.drawPath(self.path)
        if self.form.caption:
            painter.setPen(QPen(_with_alpha(INK, style.alpha)))
            painter.setFont(self.font)
            painter.drawText(self._caption_rect(), Qt.AlignmentFlag.AlignCenter, self.form.caption)


class ProposalItem(QGraphicsItem):
    """A proposed group's faint dotted outline with its stage as three small dots above
    its corner, one filled per stage passed, fading when failed (D-024; G-021: GUI
    design, not French's); its click band is the outline (G-022). The stage in words is
    the hover text's."""

    def __init__(self, form):
        super().__init__()
        self.form = None
        self.selected = False
        self.setAcceptHoverEvents(True)
        self.set_form(form)

    @property
    def name(self) -> str:
        """The proposal's name (G-022)."""
        return self.form.name

    def set_form(self, form) -> None:
        """A new state of the proposal; costs nothing when unchanged (G-021)."""
        if form == self.form:
            return
        self.prepareGeometryChange()
        self.form = form
        self.update()

    def set_selected(self, selected: bool) -> None:
        """Highlight the outline when the proposal is in the shared selection (G-022)."""
        if selected != self.selected:
            self.selected = selected
            self.update()

    def _dots_rect(self) -> QRectF:
        rect = _rect(self.form.box)
        return QRectF(rect.left(), rect.top() - STAGE_DOT * 2.5, STAGE_DOT * 2 * st.STAGES,
                      STAGE_DOT * 2)

    def shape(self) -> QPainterPath:
        """The outline's band and the stage dots (G-022)."""
        path = QPainterPath()
        path.addRect(_rect(self.form.box))
        band = _band(path)
        band.addRect(self._dots_rect())
        return band

    def boundingRect(self) -> QRectF:
        """The box and its stage dots (G-021)."""
        return _rect(self.form.box).adjusted(-BAND, -BAND, BAND, BAND).united(self._dots_rect())

    def paint(self, painter, option, widget=None):
        """The dotted outline by `box_style`, the stage dots above its corner (G-021,
        G-022)."""
        style = st.box_style(self.form)
        rect = _rect(self.form.box)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setBrush(Qt.BrushStyle.NoBrush)
        if self.selected:
            painter.setPen(QPen(_with_alpha(SELECTED, 150), style.width + 4))
            painter.drawRect(rect)
        ink = _with_alpha(PROPOSAL_INK, style.alpha)
        painter.setPen(QPen(ink, style.width, Qt.PenStyle.DotLine))
        painter.drawRect(rect)
        dots = self._dots_rect()
        painter.setPen(QPen(ink, 0.8))
        for k in range(st.STAGES):
            painter.setBrush(QBrush(ink) if k < self.form.stage else Qt.BrushStyle.NoBrush)
            painter.drawEllipse(QRectF(dots.left() + k * STAGE_DOT * 2 + 1, dots.top() + 1,
                                       STAGE_DOT, STAGE_DOT))


class CorrespondenceItem(QGraphicsItem):
    """A correspondence's line between its ends' centres, by `line_style`: dashed and
    flashing while tentative or competing [F95 p. 105], solid in the Worldview, thicker
    between groups, faint when it lost or was removed, its ends boxed and flashing while
    new [F95 p. 101] (SPEC.md 6c.14; G-021); its click band is the line (G-022)."""

    def __init__(self, form):
        super().__init__()
        self.form = None
        self.selected = False
        self.setAcceptHoverEvents(True)
        self.set_form(form)

    @property
    def name(self) -> str:
        """The correspondence's name (G-022)."""
        return self.form.name

    def set_form(self, form) -> None:
        """A new state of the correspondence, or a new phase; costs nothing when
        unchanged (G-021)."""
        if form == self.form:
            return
        self.prepareGeometryChange()
        self.form = form
        self.update()

    def set_selected(self, selected: bool) -> None:
        """Highlight the line when the correspondence is in the shared selection
        (G-022)."""
        if selected != self.selected:
            self.selected = selected
            self.update()

    def _line(self) -> QLineF:
        a, b = self.form.a, self.form.b
        return QLineF(a[0] * SCALE, a[1] * SCALE, b[0] * SCALE, b[1] * SCALE)

    def _path(self) -> QPainterPath:
        path = QPainterPath(self._line().p1())
        path.lineTo(self._line().p2())
        return path

    def shape(self) -> QPainterPath:
        """The line's band (G-022)."""
        return _band(self._path(), BAND + 2)

    def boundingRect(self) -> QRectF:
        """The line and, while new, its end boxes (G-021)."""
        rect = QRectF(self._line().p1(), self._line().p2()).normalized()
        if self.form.new:
            for box in self.form.end_boxes:
                rect = rect.united(_rect(box))
        return rect.adjusted(-BAND, -BAND, BAND, BAND)

    def paint(self, painter, option, widget=None):
        """The selection's halo, the line with its dash offset by the phase, and the
        flashing end boxes (filled on one phase, outlined on the other) (G-021,
        G-022)."""
        style = st.line_style(self.form)
        line = self._line()
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        if self.selected:
            painter.setPen(QPen(_with_alpha(SELECTED, 150), style.width + 5,
                                Qt.PenStyle.SolidLine, Qt.PenCapStyle.RoundCap))
            painter.drawLine(line)
        pen = QPen(_with_alpha(LINE_INK, style.alpha), style.width, Qt.PenStyle.SolidLine,
                   Qt.PenCapStyle.FlatCap)
        if style.dashed:
            pen.setDashPattern(DASH)
            pen.setDashOffset(style.phase * (DASH[0] + DASH[1]) / 2)
        painter.setPen(pen)
        painter.drawLine(line)
        if self.form.role_proposed:
            # A correspondence a role consumer proposed (D-095): a small ring at the middle
            # of the line (G-061).
            painter.setPen(QPen(_with_alpha(ROLE_INK, style.alpha), 1.2))
            painter.setBrush(QBrush(_with_alpha(ROLE_FILL, style.alpha)))
            painter.drawEllipse(line.center(), ROLE_MARK, ROLE_MARK)
        if self.form.new:
            painter.setPen(QPen(_with_alpha(LINE_INK, style.alpha), 1.2))
            fill = _with_alpha(LINE_INK, 50 if self.form.phase == 0 else 0)
            painter.setBrush(QBrush(fill))
            for box in self.form.end_boxes:
                painter.drawRect(_rect(box).adjusted(-2, -2, 2, 2))


class RegionsItem(QGraphicsItem):
    """The nine regions (D-004) as cells: soft bands along the blurry boundaries, the
    engine's names in the corners, a tint and the ids where a noticed object's last
    drawn region is, and the ranks from the touched object (D-032) when the ranks layer
    is on (G-025: GUI design, not French's). It takes the layout's `Overlays` and
    paints nothing when they hold no regions."""

    def __init__(self):
        super().__init__()
        self.form = ovl.Overlays()
        self.name_font = _font(REGION_NAME_SIZE)
        self.rank_font = _font(RANK_SIZE, bold=True)
        self.setAcceptedMouseButtons(Qt.MouseButton.NoButton)

    def set_form(self, form) -> None:
        """New overlays; costs nothing when unchanged (G-025)."""
        if form == self.form:
            return
        self.prepareGeometryChange()
        self.form = form
        self.setVisible(bool(form.regions))
        self.update()

    def boundingRect(self) -> QRectF:
        """The union of the cells (the table) (G-025)."""
        rect = QRectF()
        for cell in self.form.regions:
            rect = rect.united(_rect(cell.box))
        return rect

    def _paint_bands(self, painter, rect: QRectF):
        """The boundaries as dashed lines inside soft bands as wide as the blur (D-004;
        G-025)."""
        half = self.form.blur * SCALE
        band = QBrush(_with_alpha(REGION_INK, 22))
        painter.setPen(Qt.PenStyle.NoPen)
        for k in (1, 2):
            x = rect.left() + rect.width() * k / 3
            y = rect.top() + rect.height() * k / 3
            painter.fillRect(QRectF(x - half, rect.top(), 2 * half, rect.height()), band)
            painter.fillRect(QRectF(rect.left(), y - half, rect.width(), 2 * half), band)
        pen = QPen(_with_alpha(REGION_INK, 130), 1.0, Qt.PenStyle.DashLine)
        painter.setPen(pen)
        for k in (1, 2):
            x = rect.left() + rect.width() * k / 3
            y = rect.top() + rect.height() * k / 3
            painter.drawLine(QLineF(x, rect.top(), x, rect.bottom()))
            painter.drawLine(QLineF(rect.left(), y, rect.right(), y))

    def _paint_cell(self, painter, cell):
        """One cell: its drawn objects' tint and ids, its name, its rank (G-025)."""
        rect = _rect(cell.box)
        if cell.drawn:
            painter.fillRect(rect.adjusted(1, 1, -1, -1), QBrush(_with_alpha(DRAWN_FILL, 26)))
        if self.form.names_shown:
            painter.setFont(self.name_font)
            painter.setPen(QPen(_with_alpha(REGION_INK, 170)))
            painter.drawText(rect.adjusted(4, 2, -4, -2),
                             Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop, cell.name)
            if cell.drawn:
                painter.setPen(QPen(_with_alpha(DRAWN_FILL, 200)))
                painter.drawText(rect.adjusted(4, 2, -4, -3),
                                 Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignBottom,
                                 "drawn: " + ", ".join(cell.drawn))
        if self.form.ranks_shown and cell.rank is not None:
            painter.setFont(self.rank_font)
            painter.setPen(QPen(_with_alpha(RANK_INK, 110)))
            painter.drawText(rect, Qt.AlignmentFlag.AlignCenter, str(cell.rank))
            painter.setFont(self.name_font)
            painter.setPen(QPen(_with_alpha(RANK_INK, 170)))
            painter.drawText(rect.adjusted(4, 2, -4, -2),
                             Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignTop,
                             f"{cell.weight:g}")

    def paint(self, painter, option, widget=None):
        """The cells, then the bands and the boundaries over them (G-025)."""
        if not self.form.regions:
            return
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.TextAntialiasing)
        for cell in self.form.regions:
            self._paint_cell(painter, cell)
        self._paint_bands(painter, self.boundingRect())


class SalienceItem(QGraphicsItem):
    """The salience heat map (D-031): every item in view tinted over its box by its share
    of the largest salience, the value written at its corner (G-025: GUI design, not
    French's). It takes the layout's `Overlays` and paints nothing without heat cells."""

    def __init__(self):
        super().__init__()
        self.form = ovl.Overlays()
        self.font = _font(HEAT_VALUE_SIZE, bold=True)
        self.setAcceptedMouseButtons(Qt.MouseButton.NoButton)

    def set_form(self, form) -> None:
        """New overlays; costs nothing when unchanged (G-025)."""
        if form == self.form:
            return
        self.prepareGeometryChange()
        self.form = form
        self.setVisible(bool(form.heat))
        self.update()

    def _rect(self, cell) -> QRectF:
        pad = HIT_PAD * SCALE / 2 + 2
        return _rect(cell.box).adjusted(-pad, -pad, pad, pad)

    def boundingRect(self) -> QRectF:
        """The union of the tinted boxes and their values (G-025)."""
        rect = QRectF()
        for cell in self.form.heat:
            rect = rect.united(self._rect(cell).adjusted(0, -12, 0, 0))
        return rect

    def paint(self, painter, option, widget=None):
        """A tint per item, warmer with its share, and its value above the box
        (G-025)."""
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setFont(self.font)
        low, high = HEAT_ALPHA
        for cell in self.form.heat:
            rect = self._rect(cell)
            alpha = int(low + (high - low) * max(0.0, min(1.0, cell.share)))
            painter.setPen(Qt.PenStyle.NoPen)
            painter.setBrush(QBrush(_with_alpha(HEAT, alpha)))
            painter.drawRoundedRect(rect, 5, 5)
            painter.setPen(QPen(_with_alpha(HEAT, 230)))
            painter.drawText(QRectF(rect.left(), rect.top() - 12, rect.width() + 20, 12),
                             Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignBottom,
                             f"{cell.value:.0f}")


class RolesItem(QGraphicsItem):
    """The roles layer (D-079; G-061: GUI design, not French's): a badge with the codes of
    an item's standing roles at the top-right corner of its box, a faint hatched band for
    the midline (S1) with the axis word, a bracket across each gap (S2), an arrow from
    the focused object to its mirror or point image (G1, G2), the re-cast's tier on each
    candidate (K1), and a bar on a barrier with a dashed line to the counterpart it
    claims (S5, S6). It takes the layout's `Overlays` and paints nothing when the layer
    is off."""

    def __init__(self):
        super().__init__()
        self.form = ovl.Overlays()
        self.table = QRectF(0, 0, 110 * SCALE, 100 * SCALE)
        self.badge_font = _font(BADGE_SIZE, bold=True)
        self.mark_font = _font(MARK_SIZE)
        self.setAcceptedMouseButtons(Qt.MouseButton.NoButton)

    def set_table(self, width: float, height: float) -> None:
        """The table's size, which fixes the bounding rectangle (G-061)."""
        rect = QRectF(0, 0, width * SCALE, height * SCALE)
        if rect != self.table:
            self.prepareGeometryChange()
            self.table = rect

    def set_form(self, form) -> None:
        """New overlays; costs nothing when unchanged, else the whole item repaints, as
        the regions and salience items do: a repaint of the changed marks' rectangles
        alone left an antialiased edge of a structure under a rectangle's border a step
        further from a fresh render than the probe allows (G-025, G-061)."""
        if form == self.form:
            return
        self.form = form
        self.setVisible(form.roles_shown)
        self.update()

    def boundingRect(self) -> QRectF:
        """The table with room around it for the badges and the labels, constant for a
        table: a changing rectangle would dirty the whole view at every new tag (G-061)."""
        return self.table.adjusted(-90, -30, 90, 20)

    def _paint_midline(self, painter) -> None:
        """The middle band tinted, faint, with "midline" and the axis word (G-061)."""
        rect = _rect(self.form.midline)
        # A plain translucent fill, not a pattern brush: a pattern's phase can differ
        # between a partial repaint and a fresh render by a pixel (the probe; G-061).
        painter.setPen(QPen(_with_alpha(MIDLINE_FILL, 90), 1.0, Qt.PenStyle.DashLine))
        painter.setBrush(QBrush(_with_alpha(MIDLINE_FILL, 14)))
        painter.drawRect(rect)
        painter.setFont(self.mark_font)
        painter.setPen(QPen(_with_alpha(ROLE_INK, 170)))
        text = "midline (S1)" + (f"; axis: {self.form.axis} (G3)" if self.form.axis else "")
        painter.drawText(rect.adjusted(4, 2, -4, -2),
                         Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop, text)

    def _paint_gap(self, painter, gap) -> None:
        """A bracket across the gap's white space, its row word under it (G-061)."""
        rect = _rect(gap.box)
        y = rect.center().y()
        painter.setPen(QPen(_with_alpha(ROLE_INK, 150), 1.2, Qt.PenStyle.DotLine))
        painter.drawLine(QLineF(rect.left(), y, rect.right(), y))
        painter.setPen(QPen(_with_alpha(ROLE_INK, 150), 1.2))
        for x in (rect.left(), rect.right()):
            painter.drawLine(QLineF(x, y - 5, x, y + 5))
        painter.setFont(self.mark_font)
        painter.drawText(QRectF(rect.left(), y + 4, rect.width(), 12),
                         Qt.AlignmentFlag.AlignCenter, f"gap (S2) {gap.row}")

    def _paint_image(self, painter, image) -> None:
        """An arrow from the focused object to its image, labelled mirror or point
        (G-061)."""
        a, b = _rect(image.source_box).center(), _rect(image.image_box).center()
        pen = QPen(_with_alpha(ROLE_INK, 160), 1.4, Qt.PenStyle.DashLine)
        painter.setPen(pen)
        painter.drawLine(QLineF(a, b))
        painter.setFont(self.mark_font)
        mid = QPointF((a.x() + b.x()) / 2, (a.y() + b.y()) / 2)
        painter.drawText(QRectF(mid.x() - 40, mid.y() - 14, 80, 12), Qt.AlignmentFlag.AlignCenter,
                         f"{image.kind} image of {image.source}")

    def _paint_badge(self, painter, badge) -> None:
        """The codes of an item's standing roles in a small box at its top-right corner
        (G-061)."""
        rect = _rect(badge.box)
        text = " ".join(code for code, _role, _args in badge.codes)
        painter.setFont(self.badge_font)
        width = painter.fontMetrics().horizontalAdvance(text) + 6
        box = QRectF(rect.right() - width / 2, rect.top() - 13, width, 12)
        # Opaque: a translucent box over an antialiased border and a tint rounds
        # differently in the backing store than in a grab (the probe; G-061).
        painter.setPen(QPen(QColor(ROLE_INK), 1.0))
        painter.setBrush(QBrush(QColor(ROLE_FILL)))
        painter.drawRoundedRect(box, 2, 2)
        painter.drawText(box, Qt.AlignmentFlag.AlignCenter, text)

    def _paint_tier(self, painter, tier) -> None:
        """The re-cast's tier word at the bottom-left corner of a candidate (G-061)."""
        rect = _rect(tier.box)
        painter.setFont(self.mark_font)
        painter.setPen(QPen(_with_alpha(ROLE_INK, 200)))
        painter.drawText(QRectF(rect.left() - 20, rect.bottom() - 2, rect.width() + 40, 12),
                         Qt.AlignmentFlag.AlignCenter, f"{tier.tier} ({tier.weight:g})")

    def _paint_block(self, painter, block) -> None:
        """A barrier's bar along its box and a dashed line to the counterpart it claims
        (G-061)."""
        barrier, claimed = _rect(block.barrier_box), _rect(block.claimed_box)
        painter.setPen(QPen(_with_alpha(BARRIER_INK, 200), 3.0))
        painter.drawLine(QLineF(barrier.left(), barrier.top() - 3, barrier.right(), barrier.top() - 3))
        painter.setPen(QPen(_with_alpha(BARRIER_INK, 160), 1.2, Qt.PenStyle.DashLine))
        painter.drawLine(QLineF(barrier.center(), claimed.center()))
        painter.setFont(self.mark_font)
        painter.setPen(QPen(_with_alpha(BARRIER_INK, 220)))
        painter.drawText(QRectF(claimed.left() - 20, claimed.top() - 24, claimed.width() + 40, 12),
                         Qt.AlignmentFlag.AlignCenter, f"claimed by {block.barrier} ({block.correspondence})")

    def paint(self, painter, option, widget=None):
        """The midline, the gaps, the images, the blocks, the tiers, then the badges over
        everything (G-061)."""
        f = self.form
        if not f.roles_shown:
            return
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.setRenderHint(QPainter.RenderHint.TextAntialiasing)
        if f.midline is not None:
            self._paint_midline(painter)
        for gap in f.gaps:
            self._paint_gap(painter, gap)
        for image in f.images:
            self._paint_image(painter, image)
        for block in f.blocks:
            self._paint_block(painter, block)
        for tier in f.tiers:
            self._paint_tier(painter, tier)
        for badge in f.badges:
            self._paint_badge(painter, badge)


class TableView(QGraphicsView):
    """The table's canvas (see the module docstring; Numbo GUI, `tree_view.py`; G-017
    to G-026). `history` is the window's `EventLog`, read at `position`; `selection` the
    window's shared `Selection` (one of its own when None); `paints` counts the paint
    calls (the window's probe); `object_items`, `arrow_items`, `group_items`,
    `proposal_items` and `line_items` are the scene's items by id, letter or name;
    `regions_item`, `salience_item` and `roles_item` the overlays' (G-025, G-061);
    `layer_actions` (by layer name) and `eliza_action` the checkable actions of the
    overlays and Eliza's view (G-025, G-026); `layout` the model's last layout; `selected(name)` is emitted after
    every change of the selection (an empty name for none), `overlays_changed()` after
    a layer or Eliza's view is toggled."""

    selected = Signal(str)
    overlays_changed = Signal()

    def __init__(self, history=None, parent=None, selection=None):
        super().__init__(parent)
        self.history = history
        self.position = -1
        self.model = TableModel()
        self.selection = selection if selection is not None else Selection()
        self.selection.subscribe(self._on_selection)
        self.layout = None
        self.paints = 0
        self.auto_fit = True
        self.object_items = {}
        self.arrow_items = {}
        self.group_items = {}
        self.proposal_items = {}
        self.line_items = {}
        self.layer_actions = {}
        self.eliza_action = None
        self.setScene(QGraphicsScene(self))
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setRenderHint(QPainter.RenderHint.TextAntialiasing)
        self.setBackgroundBrush(QBrush(QColor(BACKGROUND)))
        self.setDragMode(QGraphicsView.DragMode.ScrollHandDrag)
        self.setTransformationAnchor(QGraphicsView.ViewportAnchor.AnchorUnderMouse)
        self.setResizeAnchor(QGraphicsView.ViewportAnchor.AnchorViewCenter)
        self.setMinimumSize(160, 120)                 # the window fits 1280 x 800 (G-014)
        self.setToolTip(f"P1 Table: {PAGES}. {icons.NOT_FRENCHS}.")
        self._build_table()
        self._build_overlays()
        self._apply_scroll_policy()
        self.redraw(now=False)

    def sizeHint(self):
        """Room for the table at its scale (G-019)."""
        return QSize(720, 660)

    # -- the fixed scenery: the table, the names, the centre line ---------------------------

    def _build_table(self):
        scene = self.scene()
        self.table_item = QGraphicsPathItem()
        self.table_item.setPen(QPen(QColor(TABLE_EDGE), 2))
        self.table_item.setBrush(QBrush(QColor(TABLE_FILL)))
        self.table_item.setZValue(-3)
        scene.addItem(self.table_item)
        edge = QColor(TABLE_EDGE)
        edge.setAlpha(140)
        self.centre_item = QGraphicsLineItem()
        self.centre_item.setPen(QPen(edge, 1, Qt.PenStyle.DashLine))
        self.centre_item.setZValue(-2)
        scene.addItem(self.centre_item)
        self.name_items = {}
        for name in ("Henry", "Eliza"):
            item = QGraphicsSimpleTextItem(name)
            item.setFont(_font(NAME_SIZE, bold=True))
            item.setBrush(QBrush(QColor(INK2)))
            item.setZValue(-1)
            scene.addItem(item)
            self.name_items[name] = item
        self._table_size = None
        self._placed_names = None

    def _place_table(self, lay):
        if self._table_size == (lay.width, lay.height) and self._placed_names == lay.names:
            return
        self._table_size = (lay.width, lay.height)
        self._placed_names = lay.names
        w, h = lay.width * SCALE, lay.height * SCALE
        path = QPainterPath()
        path.addRoundedRect(QRectF(0, 0, w, h), 6, 6)
        self.table_item.setPath(path)
        (x1, y1), (x2, y2) = lay.centre
        self.centre_item.setLine(x1 * SCALE + 6, y1 * SCALE, x2 * SCALE - 6, y2 * SCALE)
        self.roles_item.set_table(lay.width, lay.height)
        for name in lay.names:
            item = self.name_items[name.text]
            rect = item.boundingRect()
            above = name.y < lay.height / 2          # the name at y = 0 is written above (G-026)
            y = (name.y * SCALE - NAME_GAP - rect.height() if above
                 else name.y * SCALE + NAME_GAP)
            item.setPos(name.x * SCALE - rect.width() / 2, y)
        self.scene().setSceneRect(self.content_rect())

    # -- the overlays and Eliza's view (G-025, G-026) -----------------------------------------

    def _build_overlays(self):
        """The two overlay items, hidden until a layer is on, and the checkable actions
        of the three layers and Eliza's view (Seek-Whence GUI, `cytoplasm_view.py`'s
        layers; G-025, G-026)."""
        self.regions_item = RegionsItem()
        self.regions_item.setZValue(Z_REGIONS)
        self.regions_item.setVisible(False)
        self.scene().addItem(self.regions_item)
        self.salience_item = SalienceItem()
        self.salience_item.setZValue(Z_HEAT)
        self.salience_item.setVisible(False)
        self.scene().addItem(self.salience_item)
        self.roles_item = RolesItem()
        self.roles_item.setZValue(Z_ROLES)
        self.roles_item.setVisible(False)
        self.scene().addItem(self.roles_item)
        tips = overlay_tooltips()
        for name in ovl.LAYERS:
            action = QAction(ovl.LAYER_TITLES[name], self)
            action.setCheckable(True)
            action.setToolTip(tips[name])
            action.setStatusTip(tips[name])
            action.toggled.connect(lambda on, name=name: self.set_layer(name, on))
            self.layer_actions[name] = action
        self.eliza_action = QAction("&" + ELIZA_TITLE, self)
        self.eliza_action.setCheckable(True)
        self.eliza_action.setToolTip(tips["eliza"])
        self.eliza_action.setStatusTip(tips["eliza"])
        self.eliza_action.toggled.connect(self.set_eliza)

    def set_layer(self, name: str, on: bool) -> None:
        """Turn one overlay layer on or off and redraw now (G-025)."""
        on = bool(on)
        if self.model.overlays.layers[name] != on:
            self.model.overlays.layers[name] = on
            self.redraw(now=True)
            self.overlays_changed.emit()
        action = self.layer_actions[name]
        if action.isChecked() != on:
            action.setChecked(on)

    def set_eliza(self, on: bool) -> None:
        """Show the table as Eliza sees it, or as Henry does, and redraw now (G-026)."""
        on = bool(on)
        if self.model.eliza != on:
            self.model.eliza = on
            self.redraw(now=True)
            self.overlays_changed.emit()
        if self.eliza_action.isChecked() != on:
            self.eliza_action.setChecked(on)

    def overlay_state(self) -> dict:
        """The layers and Eliza's view as booleans by name, for the settings (G-025)."""
        state = dict(self.model.overlays.layers)
        state["eliza"] = self.model.eliza
        return state

    def set_overlay_state(self, state: dict) -> None:
        """Set the layers and Eliza's view from booleans by name; names not given are
        left as they are (G-025)."""
        for name in ovl.LAYERS:
            if name in state:
                self.set_layer(name, state[name])
        if "eliza" in state:
            self.set_eliza(state["eliza"])

    def content_rect(self) -> QRectF:
        """The table with its margin, in scene units (G-019)."""
        w, h = self._table_size or (self.model.width, self.model.height)
        return QRectF(-MARGIN, -MARGIN, w * SCALE + 2 * MARGIN, h * SCALE + 2 * MARGIN)

    # -- the view protocol (G-011, G-017) -----------------------------------------------------

    def set_problem(self, problem) -> None:
        """The run's problem, before it starts: the table is drawn at once (G-017)."""
        self.model.set_problem(problem)
        self.redraw(now=True)

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-017)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model, then the snapshot in force at `position` with the terms and
        the salience readings recorded beside it (G-017, G-020, G-024)."""
        self.model.on_event(event)
        self._apply_state()

    def _apply_state(self) -> None:
        """Give the model the snapshot in force at `position` with the readings beside
        it (G-017, G-024). Done again at every redraw, because the snapshot of a step is
        recorded after the step's events were delivered: the window's settle after the
        run, and any later redraw, then show the final state."""
        if self.history is not None and self.position >= 0:
            self.model.update(self.history.state_at(self.position),
                              self.history.terms_at(self.position),
                              self.history.salience_at(self.position))

    def redraw(self, now=True) -> None:
        """Lay the model out, bring the scene in line and paint now (`now` false: mark
        the viewport dirty for the window's `paint_all`) (Numbo GUI, `tree_view.py`)."""
        self._apply_state()
        self.layout = self.model.layout()
        self._rebuild()
        if self.isVisible():
            self.viewport().update()
            if now:
                paint_all((self.scene(),))

    def _sync(self, items: dict, forms: dict, make, z: float) -> None:
        """Bring one family of items in line with its forms: one item per key, made or
        changed only where it differs, removed when gone, highlighted when selected, with
        the model's hover text (Numbo GUI, `tree_view.py`'s `_rebuild`; G-018, G-022)."""
        scene = self.scene()
        for key in [k for k in items if k not in forms]:
            scene.removeItem(items.pop(key))
        for key, form in forms.items():
            item = items.get(key)
            if item is None:
                item = make(form)
                item.setZValue(z)
                scene.addItem(item)
                items[key] = item
            else:
                item.set_form(form)
            if hasattr(item, "set_selected"):
                item.set_selected(self.selection.includes(key))
            item.setToolTip(self.model.tooltip(key) or item.toolTip())

    def _rebuild(self):
        """Bring the scene's items in line with the layout, family by family (G-018,
        G-021)."""
        lay = self.layout
        self._place_table(lay)
        self._sync(self.object_items, {o.id: o for o in lay.objects}, ObjectItem, Z_OBJECT)
        arrows = {a.letter: a for a in (lay.touch, lay.answer) if a is not None}
        self._sync(self.arrow_items, arrows, ArrowItem, Z_ARROW)
        gestalt = {g.name: g for g in lay.groups if g.wavy}
        subgroups = {g.name: g for g in lay.groups if not g.wavy}
        self._sync(self.group_items, gestalt | subgroups, GroupItem, Z_GESTALT)
        for name in subgroups:
            self.group_items[name].setZValue(Z_SUBGROUP)
        self._sync(self.proposal_items, {p.name: p for p in lay.proposals}, ProposalItem,
                   Z_PROPOSAL)
        self._sync(self.line_items, {c.name: c for c in lay.correspondences},
                   CorrespondenceItem, Z_LINE)
        self.regions_item.set_form(lay.overlays)
        self.salience_item.set_form(lay.overlays)
        self.roles_item.set_form(lay.overlays)
        if self.auto_fit:
            self._fit()

    # -- hit-testing and the selection (G-018, G-022) ------------------------------------------

    def element_at(self, point) -> str | None:
        """The name of the element drawn at a viewport point: an object first (a line
        ends at an object's centre, and the object is what the eye sees there), else the
        topmost correspondence, proposal or group, or None (G-022)."""
        items = self.items(point)
        for item in items:
            if isinstance(item, ObjectItem):
                return item.name
        for item in items:
            if isinstance(item, (CorrespondenceItem, ProposalItem, GroupItem)):
                return item.name
        return None

    def object_at(self, point) -> str | None:
        """The id of the object drawn at a viewport point, or None (G-018)."""
        for item in self.items(point):
            if isinstance(item, ObjectItem):
                return item.obj.id
        return None

    def mousePressEvent(self, event):
        """A left click toggles the element under the mouse in the shared selection, or
        clears it on empty table; then the drag-to-pan goes on (PANES.md P1 "click to
        select"; G-022)."""
        if event.button() == Qt.MouseButton.LeftButton:
            name = self.element_at(event.position().toPoint())
            what = self.model.selection_of(name) if name is not None else None
            if what is None:
                self.selection.clear()
            else:
                self.selection.toggle(*what)
        super().mousePressEvent(event)

    def _on_selection(self, selection) -> None:
        """The shared selection changed, from this pane or another: the roles layer's
        images follow the selected object (G-061); redraw now and say so (G-022)."""
        self.model.overlays.focus = selection.name if selection.kind == "object" else None
        if self.layout is not None:
            self.redraw(now=True)
        self.selected.emit(selection.name or "")

    def paintEvent(self, event):
        """Paint, and count (Numbo GUI, `tree_view.py`'s `paints`)."""
        self.paints += 1
        super().paintEvent(event)

    # -- zoom, pan and fit (Numbo GUI, `tree_view.py`; G-019) ---------------------------------

    def scale_factor(self) -> float:
        """The current zoom (G-019)."""
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
        """Fit the table to the view, and keep doing so (auto-fit) (G-019)."""
        self._set_auto_fit(True)
        self._fit()

    def zoom_by(self, factor):
        """Zoom by a factor; auto-fit goes off (G-019)."""
        self._set_auto_fit(False)
        self.scale(factor, factor)

    def zoom_in(self):
        """One step in (G-019)."""
        self.zoom_by(ZOOM_STEP)

    def zoom_out(self):
        """One step out (G-019)."""
        self.zoom_by(1 / ZOOM_STEP)

    def reset_zoom(self):
        """1:1 (G-019)."""
        self._set_auto_fit(False)
        self.setTransform(QTransform())

    def wheelEvent(self, event):
        """The wheel zooms (G-019)."""
        steps = event.angleDelta().y() / 120
        if steps:
            self.zoom_by(ZOOM_STEP ** steps)
        event.accept()

    def keyPressEvent(self, event):
        """+ / - zoom, 0 is 1:1, F fits (Numbo GUI, `tree_view.py`; G-019)."""
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
        """Keep the fit on a resize (G-019)."""
        super().resizeEvent(event)
        if self.auto_fit:
            self._fit()

    def render_png(self, path, scale=1.0):
        """Render the whole scene (at `scale`) to a PNG at `path` (Numbo GUI, `render.py`)."""
        return render_scene_png(self, path, scale)


class TableCanvas(QWidget):
    """The central widget: a bar (Fit, zoom buttons, the overlay toggles, the legend)
    over the `TableView` (Numbo GUI, `tree_view.py`'s `TreeCanvas`; G-019, G-025). It
    keeps the view protocol the window expects (`on_event`, `set_position`, `redraw`,
    `paints`, `pane_id`; G-011), shares the window's `selection` and re-emits the view's
    `selected` (G-022). `overlay_buttons` (by layer name and `eliza`) are the bar's
    toggles, each over the view's action, so the bar and the View menu agree."""

    selected = Signal(str)

    def __init__(self, history=None, parent=None, selection=None):
        super().__init__(parent)
        self.pane_id = "P1"
        self.title = "Table"
        self.pages = PAGES
        self.view = TableView(history, selection=selection)
        self.view.setObjectName("table-graphics-view")
        self.view.selected.connect(self.selected)
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
            button.setMaximumWidth(40)
            button.setMinimumWidth(24)
            bar.addWidget(button)
        bar.addSpacing(8)
        self.overlay_buttons = {}
        actions = dict(self.view.layer_actions)
        actions["eliza"] = self.view.eliza_action
        for name, action in actions.items():
            button = QToolButton()
            button.setDefaultAction(action)
            button.setCheckable(True)
            button.setText(ELIZA_TITLE if name == "eliza" else ovl.LAYER_TITLES[name].lower())
            button.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            button.setMinimumWidth(24)               # the window fits 1280 x 800 (G-014)
            button.setMaximumWidth(80)
            bar.addWidget(button)
            self.overlay_buttons[name] = button
        bar.addSpacing(8)
        self.legend = QLabel(legend_text())
        self.legend.setObjectName("table-legend")
        self.legend.setToolTip(f"{legend_text()}. {OVERLAY_PAGES}")
        self.legend.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        bar.addWidget(self.legend, 1)
        self.view.overlays_changed.connect(self._update_legend)
        box = QVBoxLayout(self)
        box.setContentsMargins(0, 0, 0, 0)
        box.setSpacing(0)
        box.addLayout(bar)
        box.addWidget(self.view, 1)
        self.setToolTip(f"{self.view.toolTip()} {OVERLAY_PAGES}.")

    def _update_legend(self) -> None:
        """The legend names the overlays on and Eliza's view, as GUI design (G-025,
        G-026)."""
        text = legend_text(self.view.layout)
        if text != self.legend.text():
            self.legend.setText(text)

    @property
    def paints(self) -> int:
        """The view's paint count (the window's probe; G-011)."""
        return self.view.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.view.model.events

    @property
    def selection(self):
        """The shared `Selection` the view uses (G-022)."""
        return self.view.selection

    def scene(self):
        """The view's scene, for the window's `paint_all` (Numbo GUI, `paint.py`)."""
        return self.view.scene()

    def set_problem(self, problem) -> None:
        """The run's problem, before it starts (G-017)."""
        self.view.set_problem(problem)

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-017)."""
        self.view.set_position(index)

    def on_event(self, event) -> None:
        """Feed the view (G-011)."""
        self.view.on_event(event)

    def redraw(self, now=False) -> None:
        """Redraw the view and bring the legend up to date (G-011, G-025)."""
        self.view.redraw(now=now)
        self._update_legend()

    def render_png(self, path, scale=1.0):
        """Render the view's scene to a PNG (Numbo GUI, `render.py`)."""
        return self.view.render_png(path, scale)

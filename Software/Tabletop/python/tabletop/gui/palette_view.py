"""P7 Palette and editor: French's palette with the counts and the buttons of his screen,
and an editor for setting a table, a view of `models/palette.py` (PANES.md P7; G-050,
G-051).

The palette is the column of Figure 2.1 [F95 p. 26 (img)]: the eleven types in
`objects.TYPES`' order, the silverware in its four orientations [F95 p. 55], drawn with
the shared icons (`icons.py`; new drawings, not French's, ANSWERS.md 4), and, as on the
screen of Figure 4.1, a box beside each cell with the count of that type on the table,
and the buttons "Load config", "Clear table", "Save config", "Do this!" and "Run it
again" [F95 p. 96 (img)]. The editor is GUI design, not French's: a canvas of the table
being set, onto which an icon is dragged from the palette (or added by a click on its
cell); an object is moved by dragging, touched (H), marked as an expected answer (E),
turned (silverware) or removed by its context menu or the keys H, E, R and Delete. The
table is validated as it changes (`problems.validate`) and `tables.realism_faults` says
whether French would set it [F95 p. 38]; the messages are shown under the canvas. Do
this! and Run it again ask the window to start a run of the table (`run_requested`);
Load config and Save config ask for the window's file dialogs. Editing is refused
while a run is live (`set_state`). The pane repaints one thing per event, the edit
line along its top (G-011's view protocol: `on_event`, `redraw`, `paints`).
"""

from PySide6.QtCore import QMimeData, QPointF, QRect, QRectF, QSize, Qt, Signal
from PySide6.QtGui import QBrush, QColor, QDrag, QFont, QPainter, QPainterPath, QPen
from PySide6.QtWidgets import (QComboBox, QGraphicsItem, QGraphicsPathItem, QGraphicsScene,
                               QGraphicsSimpleTextItem, QGraphicsView, QGridLayout, QHBoxLayout,
                               QLabel, QLineEdit, QMenu, QPushButton, QScrollArea, QSizePolicy,
                               QVBoxLayout, QWidget)

from tabletop import events as ev
from tabletop import objects as O
from tabletop.geometry import TABLE_HEIGHT, TABLE_WIDTH
from tabletop.gui import icons, paint
from tabletop.gui.models import palette as pm
from tabletop.gui.table_view import (BACKGROUND, INK2, SCALE, SELECTED, TABLE_EDGE, TABLE_FILL,
                                     ArrowItem)

__all__ = ["MIME_TYPE", "MIN_SIZE", "EditLine", "EditorCanvas", "EditorItem", "PaletteStrip",
           "PaletteView"]

MIME_TYPE = "application/x-tabletop-type"     # a palette drag carries the type's name
MIN_SIZE = (120, 60)                          # the pane's smallest size (G-014)
CELL_W, CELL_H, COUNT_W = 64, 30, 22          # the palette's cells and count boxes (G-051)
ICON_H = 22                                   # an icon's height in a cell
INK, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
LIVE = QColor("#1b7f3b")
ERROR, WARNING, GOOD = "#b00020", "#a63d00", "#1b7f3b"
NAME_SIZE = 10
BUTTONS = (("Load config", "load"), ("Clear table", "clear"), ("Save config", "save"),
           ("Do this!", "run"), ("Run it again", "again"))    # the screen's [F95 p. 96 (img)]
NOT_FRENCHS = "GUI design, not French's"


def _font(size: int, bold: bool = False) -> QFont:
    font = QFont()
    font.setPointSize(size)
    font.setBold(bold)
    return font


class EditLine(QWidget):
    """The thin strip along the top of the pane: whether editing is on, and the
    table's status in words; repainted once per event (G-011, G-051)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.state = "idle"
        self.events = 0
        self.paints = 0
        self.status = ""
        self.setMinimumHeight(16)
        self.setMaximumHeight(16)
        self.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)

    def text(self) -> str:
        """`editing: 3 objects; can run; ...` or `editing off while the run is live
        (running: 23 events)` (G-051)."""
        if self.state in ("running", "paused"):
            return f"editing off while the run is live ({self.state}: {self.events} events)"
        return f"editing: {self.status}"

    def paintEvent(self, event):
        """Paint the line; `paints` counts the calls for the window's probe (G-011)."""
        self.paints += 1
        painter = QPainter(self)
        font = QFont(painter.font())
        font.setPointSizeF(8.0)
        painter.setFont(font)
        painter.setPen(LIVE if self.state in ("running", "paused") else MUTED)
        painter.drawText(QRect(2, 0, self.width() - 4, self.height()),
                         Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                         painter.fontMetrics().elidedText(self.text(), Qt.TextElideMode.ElideRight,
                                                          self.width() - 4))
        painter.end()


class PaletteStrip(QWidget):
    """The palette as a column of cells in Figure 2.1's order [F95 p. 26 (img)], each
    with its icon (the silverware in the four orientations [F95 p. 55]) and the count
    of that type on the table in a box beside it [F95 p. 96 (img)] (G-051). Press and
    drag a cell to carry its type onto the editor's canvas (`MIME_TYPE`); a click emits
    `type_clicked(type)`. `counts` is what the boxes show."""

    type_clicked = Signal(str)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.counts = {t: 0 for t in O.TYPES}
        self.enabled = True
        self._pressed = None
        self.setFixedSize(CELL_W + COUNT_W + 2, CELL_H * len(O.TYPES) + 2)
        self.setToolTip(f"The palette [F95 p. 26 (img)] with the counts [F95 p. 96 (img)]; "
                        f"{icons.NOT_FRENCHS}. Drag a cell onto the table, or click it.")

    def set_counts(self, counts: dict) -> None:
        """The counts beside the cells (G-051)."""
        if counts != self.counts:
            self.counts = dict(counts)
            self.update()

    def cell_rect(self, index: int) -> QRect:
        """The cell of the type at `index` of `objects.TYPES` (G-051)."""
        return QRect(1, 1 + index * CELL_H, CELL_W, CELL_H)

    def type_at(self, point) -> str | None:
        """The type whose cell holds the point, or None (G-051)."""
        for i, type_ in enumerate(O.TYPES):
            if self.cell_rect(i).adjusted(0, 0, COUNT_W, 0).contains(point):
                return type_
        return None

    def _paint_icons(self, painter, type_, rect):
        """The icon in the cell; silverware four times, in `ORIENTATIONS`' order."""
        fw, fh = O.footprint(type_)
        if type_ in O.SILVERWARE:
            scale = (ICON_H - 4) / max(fw, fh)
            for i, orientation in enumerate(O.ORIENTATIONS):
                painter.save()
                painter.translate(rect.left() + (i + 0.5) * rect.width() / 4, rect.center().y())
                icons.paint_icon(painter, type_, fw * scale, fh * scale, orientation)
                painter.restore()
        else:
            scale = ICON_H / max(fw, fh)
            painter.save()
            painter.translate(rect.center().x(), rect.center().y())
            icons.paint_icon(painter, type_, fw * scale, fh * scale, None)
            painter.restore()

    def paintEvent(self, event):
        """The cells with their icons and the count boxes (G-051)."""
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        painter.fillRect(self.rect(), QColor(BACKGROUND))
        painter.setFont(_font(8, bold=True))
        for i, type_ in enumerate(O.TYPES):
            cell = self.cell_rect(i)
            painter.setPen(QPen(QColor(TABLE_EDGE), 1))
            painter.setBrush(QBrush(QColor("#ffffff" if self.enabled else "#f0f0ee")))
            painter.drawRect(cell)
            self._paint_icons(painter, type_, cell.adjusted(2, 2, -2, -2))
            box = QRect(cell.right() + 1, cell.top(), COUNT_W, CELL_H)
            painter.setPen(QPen(QColor(TABLE_EDGE), 1))
            painter.setBrush(QBrush(QColor("#ffffff")))
            painter.drawRect(box)
            painter.setPen(INK)
            painter.drawText(box, Qt.AlignmentFlag.AlignCenter, str(self.counts[type_]))
        painter.end()

    def mousePressEvent(self, event):
        """Remember the cell pressed, for a drag or a click (G-051)."""
        if event.button() == Qt.MouseButton.LeftButton and self.enabled:
            self._pressed = (self.type_at(event.position().toPoint()), event.position())
        super().mousePressEvent(event)

    def mouseMoveEvent(self, event):
        """Past the drag distance, carry the type as a drag (G-051)."""
        if self._pressed and self._pressed[0] and event.buttons() & Qt.MouseButton.LeftButton:
            if (event.position() - self._pressed[1]).manhattanLength() > 8:
                type_ = self._pressed[0]
                self._pressed = None
                drag = QDrag(self)
                mime = QMimeData()
                mime.setData(MIME_TYPE, type_.encode())
                mime.setText(type_)
                drag.setMimeData(mime)
                drag.setPixmap(icons.icon_pixmap(type_, None, 32))
                drag.exec(Qt.DropAction.CopyAction)
        super().mouseMoveEvent(event)

    def mouseReleaseEvent(self, event):
        """A click on a cell (no drag) adds one of its type (G-051)."""
        if self._pressed and self._pressed[0] and event.button() == Qt.MouseButton.LeftButton:
            self.type_clicked.emit(self._pressed[0])
        self._pressed = None
        super().mouseReleaseEvent(event)

    def event(self, e):
        """The tooltip names the cell under the mouse and its count (G-051)."""
        if e.type() == e.Type.ToolTip:
            type_ = self.type_at(e.pos())
            if type_:
                self.setToolTip(f"{type_}: {self.counts[type_]} on the table; drag onto the "
                                f"table or click to add one. {icons.NOT_FRENCHS}.")
        return super().event(e)


class EditorItem(QGraphicsItem):
    """One object on the editor's canvas: the icon, movable by dragging; its id under
    it; a ring when selected (G-051)."""

    def __init__(self, obj, canvas):
        super().__init__()
        self.obj = None
        self.canvas = canvas
        self.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsMovable, True)
        self.setFlag(QGraphicsItem.GraphicsItemFlag.ItemIsSelectable, True)
        self.setAcceptHoverEvents(True)
        self.set_form(obj)

    @property
    def name(self) -> str:
        """The object's id (G-051)."""
        return self.obj.id

    def set_form(self, obj) -> None:
        """A new state of the object; costs nothing when unchanged (G-051)."""
        if obj == self.obj:
            return
        self.prepareGeometryChange()
        self.obj = obj
        self.setPos(obj.x * SCALE, obj.y * SCALE)
        self.setToolTip(self.canvas.editor.hover_text(obj.id))
        self.update()

    def _box(self) -> QRectF:
        w, h = self.obj.width * SCALE, self.obj.height * SCALE
        return QRectF(-w / 2 - 2, -h / 2 - 2, w + 4, h + 4)

    def boundingRect(self) -> QRectF:
        """The footprint box as it lies, with room for the ring and the id (D-002)."""
        return self._box().adjusted(-2, -2, 2, 12)

    def paint(self, painter, option, widget=None):
        """The icon turned to its orientation (`icons.paint_icon`; G-016) and the id;
        an orange ring when selected (G-051)."""
        obj = self.obj
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        up_w, up_h = ((obj.height, obj.width) if obj.orientation in ("left", "right")
                      else (obj.width, obj.height))
        icons.paint_icon(painter, obj.type, up_w * SCALE, up_h * SCALE, obj.orientation)
        if self.isSelected():
            painter.setPen(QPen(QColor(SELECTED), 2.0))
            painter.setBrush(Qt.BrushStyle.NoBrush)
            painter.drawRoundedRect(self._box(), 4, 4)

    def mouseReleaseEvent(self, event):
        """The drag's end: the model moves the object (clamped, to a tenth) and the
        item snaps to the model's position (G-050, G-051)."""
        super().mouseReleaseEvent(event)
        if event.button() == Qt.MouseButton.LeftButton:
            self.canvas.moved(self.obj.id, self.pos().x() / SCALE, self.pos().y() / SCALE)

    def mouseDoubleClickEvent(self, event):
        """A double click touches the object (H) (G-051)."""
        self.canvas.touch(self.obj.id)


class EditorCanvas(QGraphicsView):
    """The table being set (GUI design, not French's; G-051): the table, the names, one
    `EditorItem` per object and the H and E arrows as the table pane draws them
    (`table_view.ArrowItem`, G-018); drops from the palette add an object at the drop
    point; keys and the context menu edit the selected object; the table always fits
    the view. `editor` is the model; `items_by_id` the objects' items."""

    def __init__(self, editor: pm.TableEditor, parent=None):
        super().__init__(parent)
        self.editor = editor
        self.items_by_id = {}
        self.arrow_items = []
        self.setScene(QGraphicsScene(self))
        self.setRenderHint(QPainter.RenderHint.Antialiasing)
        self.setRenderHint(QPainter.RenderHint.TextAntialiasing)
        self.setBackgroundBrush(QBrush(QColor(BACKGROUND)))
        self.setAcceptDrops(True)
        self.setMinimumSize(120, 90)
        self.setToolTip(f"The editor: {NOT_FRENCHS}. Drop an icon from the palette; drag an "
                        "object to move it; double-click or H to touch it, E to mark it an "
                        "expected answer, R to turn silverware, Delete to remove it.")
        self._build_table()
        self.editor.listeners.append(self.rebuild)
        self.rebuild()

    def sizeHint(self):
        """Room for the table (G-051)."""
        return QSize(330, 300)

    def _build_table(self):
        scene = self.scene()
        w, h = TABLE_WIDTH * SCALE, TABLE_HEIGHT * SCALE
        self.table_item = QGraphicsPathItem()
        path = QPainterPath()
        path.addRoundedRect(QRectF(0, 0, w, h), 6, 6)
        self.table_item.setPath(path)
        self.table_item.setPen(QPen(QColor(TABLE_EDGE), 2))
        self.table_item.setBrush(QBrush(QColor(TABLE_FILL)))
        self.table_item.setZValue(-3)
        scene.addItem(self.table_item)
        for text, y in (("Henry", -4), ("Eliza", h + 4)):
            item = QGraphicsSimpleTextItem(text)
            item.setFont(_font(NAME_SIZE, bold=True))
            item.setBrush(QBrush(QColor(INK2)))
            rect = item.boundingRect()
            item.setPos(w / 2 - rect.width() / 2, y - rect.height() if y < 0 else y)
            scene.addItem(item)
        scene.setSceneRect(QRectF(-10, -30, w + 20, h + 60))

    def rebuild(self) -> None:
        """The items follow the model: one per object, the arrows, the counts' source
        (G-051)."""
        scene = self.scene()
        forms = {o.id: o for o in self.editor.placed()}
        for id_ in list(self.items_by_id):
            if id_ not in forms:
                scene.removeItem(self.items_by_id.pop(id_))
        for id_, form in forms.items():
            if id_ in self.items_by_id:
                self.items_by_id[id_].set_form(form)
            else:
                item = EditorItem(form, self)
                item.setZValue(1)
                scene.addItem(item)
                self.items_by_id[id_] = item
        for item in self.arrow_items:
            scene.removeItem(item)
        self.arrow_items = []
        for arrow in self.editor.arrows():
            item = ArrowItem(arrow)
            item.setZValue(2)
            scene.addItem(item)
            self.arrow_items.append(item)

    def fit(self) -> None:
        """The whole table in view (G-051)."""
        self.fitInView(self.scene().sceneRect(), Qt.AspectRatioMode.KeepAspectRatio)

    def resizeEvent(self, event):
        """Keep the table fitted (G-051)."""
        super().resizeEvent(event)
        self.fit()

    def showEvent(self, event):
        """Fit the table when first shown (G-051)."""
        super().showEvent(event)
        self.fit()

    def set_enabled(self, enabled: bool) -> None:
        """Editing on or off: drops, drags and keys (PANES.md P7: refused while a run
        is live; G-051)."""
        self.setInteractive(enabled)
        self.setAcceptDrops(enabled)

    def selected_id(self) -> str | None:
        """The id of the selected object, or None (G-051)."""
        for item in self.scene().selectedItems():
            if isinstance(item, EditorItem):
                return item.name
        return None

    def select(self, id_) -> None:
        """Select the object with this id (none for None) (G-051)."""
        for item_id, item in self.items_by_id.items():
            item.setSelected(item_id == id_)

    # -- edits through the model (every one refused while a run is live) ---------------

    def _edit(self, action) -> bool:
        try:
            action()
        except (ValueError, KeyError):
            return False
        return True

    def moved(self, id_: str, x: float, y: float) -> None:
        """An item was dragged: move it in the model (clamped), or snap it back
        (G-051)."""
        if not self._edit(lambda: self.editor.move(id_, x, y)):
            self.rebuild()
        item = self.items_by_id.get(id_)
        if item is not None:
            item.setPos(item.obj.x * SCALE, item.obj.y * SCALE)

    def touch(self, id_: str) -> None:
        """Henry touches the object (G-051)."""
        self._edit(lambda: self.editor.touch(id_))

    def toggle_answer(self, id_: str) -> None:
        """Mark or unmark an expected answer (G-051)."""
        self._edit(lambda: self.editor.toggle_answer(id_))

    def rotate(self, id_: str) -> None:
        """Turn silverware (G-051)."""
        self._edit(lambda: self.editor.rotate(id_))

    def delete(self, id_: str) -> None:
        """Remove the object (G-051)."""
        self._edit(lambda: self.editor.delete(id_))

    def add_at(self, type_: str, scene_point) -> str | None:
        """Add an object of `type_` at a scene point (table units are scene units over
        `SCALE`); None when refused (G-051)."""
        ids = []
        if self._edit(lambda: ids.append(self.editor.add(type_, scene_point.x() / SCALE,
                                                         scene_point.y() / SCALE))):
            self.select(ids[0])
            return ids[0]
        return None

    # -- drops, keys and the context menu ---------------------------------------------

    def dragEnterEvent(self, event):
        """Accept a palette drag while editing is on (G-051)."""
        if event.mimeData().hasFormat(MIME_TYPE) and self.editor.enabled:
            event.acceptProposedAction()

    def dragMoveEvent(self, event):
        """Accept the drag over the whole canvas (G-051)."""
        if event.mimeData().hasFormat(MIME_TYPE) and self.editor.enabled:
            event.acceptProposedAction()

    def dropEvent(self, event):
        """A palette drag dropped: add the type at the point (G-051)."""
        if event.mimeData().hasFormat(MIME_TYPE) and self.editor.enabled:
            type_ = bytes(event.mimeData().data(MIME_TYPE)).decode()
            self.add_at(type_, self.mapToScene(event.position().toPoint()))
            event.acceptProposedAction()

    def keyPressEvent(self, event):
        """H touch, E answer, R rotate, Delete remove, on the selected object (G-051)."""
        id_ = self.selected_id()
        actions = {Qt.Key.Key_H: self.touch, Qt.Key.Key_E: self.toggle_answer,
                   Qt.Key.Key_R: self.rotate, Qt.Key.Key_Delete: self.delete,
                   Qt.Key.Key_Backspace: self.delete}
        if id_ is not None and event.key() in actions and self.editor.enabled:
            actions[event.key()](id_)
            event.accept()
            return
        super().keyPressEvent(event)

    def context_actions(self, id_: str) -> tuple:
        """The context menu's entries for an object: (text, callable) pairs (G-051)."""
        obj = self.editor.object(id_)
        entries = [("Touch (H)", lambda: self.touch(id_)),
                   (("Unmark" if self.editor.label_of(id_) else "Mark as") + " expected answer (E)",
                    lambda: self.toggle_answer(id_))]
        if obj is not None and obj.type in O.SILVERWARE:
            entries.append(("Rotate (R)", lambda: self.rotate(id_)))
        entries.append(("Delete", lambda: self.delete(id_)))
        return tuple(entries)

    def contextMenuEvent(self, event):
        """The context menu of the object under the mouse (G-051)."""
        item = self.itemAt(event.pos())
        if not isinstance(item, EditorItem) or not self.editor.enabled:
            return
        self.select(item.name)
        menu = QMenu(self)
        for text, action in self.context_actions(item.name):
            menu.addAction(text, action)
        menu.exec(event.globalPos())


class PaletteView(QWidget):
    """The palette and editor pane (PANES.md P7; G-050, G-051; G-011's view protocol).
    The edit line along the top, then, in a scroll area (G-014): the screen's buttons
    in two rows [F95 p. 96 (img)], the file's name, title and family, the palette
    strip beside the editor's canvas, and the messages. Signals: `run_requested(bool)`
    (Do this!, or Run it again with True); `load_requested()`, `save_requested()` for
    the window's dialogs. `pane_id`, `title` and `pages` are the window's labels."""

    run_requested = Signal(bool)
    load_requested = Signal()
    save_requested = Signal()

    def __init__(self, title="Palette and editor", pane_id="P7", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.editor = pm.TableEditor()
        self.state = "idle"
        self.edit_line = EditLine()
        self.strip = PaletteStrip()
        self.canvas = EditorCanvas(self.editor)
        self.buttons = {}
        self._build_form()
        self._lay_out()
        self.editor.listeners.append(self._refresh)
        self.strip.type_clicked.connect(self._type_clicked)
        self._refresh()
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")

    def _build_form(self):
        for text, key in BUTTONS:
            button = QPushButton(text)
            button.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            button.setMinimumWidth(40)
            self.buttons[key] = button
        self.buttons["load"].clicked.connect(self.load_requested)
        self.buttons["clear"].clicked.connect(self.clear)
        self.buttons["save"].clicked.connect(self.save_requested)
        self.buttons["run"].clicked.connect(lambda: self.run_requested.emit(False))
        self.buttons["again"].clicked.connect(lambda: self.run_requested.emit(True))
        self.buttons["run"].setToolTip("Do this!: run the table with the next run's seed and "
                                       "configuration [F95 p. 96 (img)]")
        self.buttons["again"].setToolTip("Run it again: the table once more, with the next seed")
        self.buttons["save"].setToolTip("Save config: a problem file wherever you choose, never "
                                        "into problems/")
        self.buttons["load"].setToolTip("Load config: a problem file, the library's included")
        self.name_edit, self.title_edit = QLineEdit(), QLineEdit()
        self.name_edit.setToolTip("The file's name (its stem)")
        self.title_edit.setToolTip("The file's title")
        self.family_combo = QComboBox()
        self.family_combo.addItems(list(pm.FAMILIES))
        self.family_combo.setToolTip("The file's family: one of the chapter 5 families "
                                     "[F95 p. 114], which the file format needs")
        for w in (self.name_edit, self.title_edit, self.family_combo):
            w.setMinimumWidth(40)
        self.name_edit.editingFinished.connect(self._names_changed)
        self.title_edit.editingFinished.connect(self._names_changed)
        self.family_combo.currentIndexChanged.connect(self._names_changed)
        self.messages_label = QLabel()
        self.messages_label.setObjectName("editor-messages")
        self.messages_label.setWordWrap(True)
        self.messages_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Minimum)
        self.legend = QLabel(f"{icons.NOT_FRENCHS}; the editor: {NOT_FRENCHS}")
        self.legend.setStyleSheet("color: #6b6b6b; font-size: 8pt;")
        self.legend.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)

    def _lay_out(self):
        body = QWidget()
        column = QVBoxLayout(body)
        column.setContentsMargins(4, 2, 4, 2)
        column.setSpacing(3)
        grid = QGridLayout()
        grid.setSpacing(2)
        for i, (_text, key) in enumerate(BUTTONS):
            grid.addWidget(self.buttons[key], i // 3, i % 3)
        column.addLayout(grid)
        names = QHBoxLayout()
        names.setSpacing(3)
        for label, w in (("Name", self.name_edit), ("Title", self.title_edit),
                         ("Family", self.family_combo)):
            names.addWidget(QLabel(label))
            names.addWidget(w, 1)
        column.addLayout(names)
        row = QHBoxLayout()
        row.setSpacing(4)
        row.addWidget(self.strip, 0, Qt.AlignmentFlag.AlignTop)
        row.addWidget(self.canvas, 1)
        column.addLayout(row, 1)
        column.addWidget(self.messages_label)
        column.addWidget(self.legend)
        scroll = QScrollArea()
        scroll.setWidgetResizable(True)
        scroll.setFrameShape(QScrollArea.Shape.NoFrame)
        scroll.setWidget(body)
        outer = QVBoxLayout(self)
        outer.setContentsMargins(0, 0, 0, 0)
        outer.setSpacing(0)
        outer.addWidget(self.edit_line)
        outer.addWidget(scroll, 1)

    def sizeHint(self):
        """Room for the strip and the canvas side by side (G-051)."""
        return QSize(300, 420)

    # -- the model's changes -------------------------------------------------------------

    def _refresh(self):
        """After every change of the editor: the counts, the names, the messages and the
        edit line (G-051)."""
        e = self.editor
        self.strip.set_counts(e.counts())
        for widget, text in ((self.name_edit, e.name), (self.title_edit, e.title)):
            if widget.text() != text:
                widget.setText(text)
        if self.family_combo.currentText() != e.family:
            self.family_combo.blockSignals(True)
            self.family_combo.setCurrentText(e.family)
            self.family_combo.blockSignals(False)
        status = e.status()
        self.status = status
        self.messages_label.setText(self.messages_html(status))
        self.buttons["run"].setEnabled(status.runnable and e.enabled)
        self.buttons["again"].setEnabled(status.runnable and e.enabled)
        self.edit_line.status = status.text
        self.edit_line.update()

    @staticmethod
    def messages_html(status: pm.EditorStatus) -> str:
        """The messages in red, the realism faults in orange, else a green line
        (G-051)."""
        if status.messages:
            lines = [f'<span style="color: {ERROR}">cannot run: {m}</span>'
                     for m in status.messages]
        else:
            lines = [f'<span style="color: {GOOD}">can run</span>']
        lines += [f'<span style="color: {WARNING}">not a table French would set: {f} '
                  f'[F95 p. 38]</span>' for f in status.faults]
        if not status.messages and not status.faults:
            lines.append(f'<span style="color: {GOOD}">a table French would set [F95 p. 38]</span>')
        return "<br>".join(lines)

    def _names_changed(self):
        try:
            self.editor.set_names(self.name_edit.text(), self.title_edit.text(),
                                  self.family_combo.currentText())
        except ValueError:
            self._refresh()

    def _type_clicked(self, type_):
        """A click on a palette cell: one of the type at the centre of Henry's row, to
        be dragged into place (G-051)."""
        self.canvas.add_at(type_, QPointF(TABLE_WIDTH / 2 * SCALE, 15 * SCALE))

    def clear(self) -> None:
        """Clear table [F95 p. 96 (img)] (G-050)."""
        if self.editor.enabled:
            self.editor.clear()

    def load_problem(self, problem) -> None:
        """A `Problem` into the editor (the library's, through the Problem menu or a
        file; G-050)."""
        if self.editor.enabled:
            self.editor.load(problem)

    # -- the run's state ----------------------------------------------------------------------

    def set_state(self, state) -> None:
        """Editing is on when no run is live (PANES.md P7): the model refuses changes,
        the strip, the canvas and the buttons are off meanwhile; the edit line says so
        (G-051)."""
        self.state = state
        live = state in ("running", "paused")
        self.editor.enabled = not live
        self.strip.enabled = not live
        self.strip.update()
        self.canvas.set_enabled(not live)
        for key in ("load", "clear", "save"):
            self.buttons[key].setEnabled(not live)
        for w in (self.name_edit, self.title_edit, self.family_combo):
            w.setEnabled(not live)
        self.edit_line.state = state
        self._refresh()

    # -- the view protocol (G-011) ------------------------------------------------------------

    @property
    def events(self) -> int:
        """How many events of the run the pane has seen (G-011)."""
        return self.edit_line.events

    @property
    def paints(self) -> int:
        """The edit line's paints, the one thing the pane repaints per event (G-011)."""
        return self.edit_line.paints

    def on_event(self, event) -> None:
        """Count the event for the edit line (G-051)."""
        if isinstance(event, ev.RunStarted):
            self.edit_line.events = 0
        self.edit_line.events += 1

    def redraw(self, now=False) -> None:
        """Mark the edit line dirty; with `now`, paint it at once (Numbo GUI, `paint.py`)."""
        self.edit_line.update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> None:
        """The pane as a PNG, for the guide (G-051)."""
        self.grab().toImage().convertToFormat(self.grab().toImage().Format.Format_RGB32).save(str(path))

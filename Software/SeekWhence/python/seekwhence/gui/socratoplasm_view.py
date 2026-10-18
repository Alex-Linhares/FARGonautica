"""The socratoplasm pane (P3): the parse in bracket notation, the gnoths as rectangles
over the cytoplasm's columns, one card per gnoth with the fields `show` prints, the
catchall and the template, a view of `models/socratoplasm.py`'s `SocratoplasmModel`
(PANES.md P3; G-030, G-031, G-032).

The gnoths "capture the current 'view' -- parse or parenthesization -- of the sequence"
[M86 p. 46]. The parse is shown large, as `show-parse` prints it, `((1 2) (1))`
[M86 p. 174]. Under it the gnoths stand in a row of rounded rectangles over the columns
of the cytoplasm pane, as the top row of Ch. 3 Figure 1, "gnoth1 gnoth2 gnoth3" over
their gloms and glints [M86 p. 102]; the catchall, "a repository for non-troublesome
terms" [M86 p. 98], is dotted; an uncapped gnoth is dashed [M86 p. 113]; a gnoth that has
gone fades [M86 p. 108]. Below, a list of cards: the template's four fields, check line
and fate [M86 pp. 73-76, 167], then each gnoth's fields exactly as `show` prints them
[M86 pp. 78, 168, 175], with one line per bond under the bond fields. A click on a
rectangle or a card selects the gnoth with its gnoth-glom and everything under it in the
shared selection (G-025), so the cytoplasm shows them; hovering a bond line highlights
its arc there; "Copy as `show`" puts the `gnoths:` block on the clipboard. The list is a
`LineList` (Numbo GUI, `log_view.py`'s `LogList`, through `taskrack_view.py`; G-028).
"""

from PySide6.QtCore import QPoint, QRect, QRectF, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QFontMetrics, QPainter, QPen
from PySide6.QtWidgets import QApplication, QHBoxLayout, QLabel, QPushButton, QVBoxLayout, QWidget

from seekwhence.gui import paint
from seekwhence.gui.models import socratoplasm as so
from seekwhence.gui.taskrack_view import LineList

__all__ = ["GnothStrip", "CardList", "SocratoplasmView"]

PAGES = "[M86 pp. 46, 78, 98, 102, 167-168, 174-175]"
BACKGROUND = "#fbfbf8"
TEXT, MUTED, GREY = "#1a1a1a", "#6b6b6b", "#9a9a9a"
BOX_FILL, BOX_BORDER = "#f3efff", "#6a3d9a"          # the socratoplasm's purple (G-029)
CATCHALL_BORDER = "#8a8a8a"
SELECTED_FILL, SELECTED_BORDER = "#ffe9cc", "#e07b00"
UNCONFIRMED = "#b08a00"
HEAD_TINT, TEMPLATE_TINT = "#efe9f7", "#eef3ff"
MARGIN = 8
PAD = 4
INDENT = 16


class GnothStrip(QWidget):
    """The gnoths as rounded rectangles over the cytoplasm's columns, Figure 1's top row
    [M86 p. 102] (G-031): the boxes of the pure `gnoth_boxes`, scaled to fit the width;
    a click emits `clicked(name)` (an empty name for empty space); `paints` counts the
    paint calls (Numbo GUI, `tree_view.py`'s `paints`)."""

    clicked = Signal(str)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.boxes = ()
        self.extent = 0.0
        self.paints = 0
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(9)
        self.bold = QFont(self.font_)
        self.bold.setBold(True)
        self.metrics = QFontMetrics(self.font_)
        self.setMinimumHeight(36)            # a short dock clips the second row (G-058)
        self.setAutoFillBackground(True)

    def sizeHint(self):
        """Room for two rows of boxes (G-031)."""
        return QSize(400, 2 * so.ROW_STEP + 2 * MARGIN)

    def set_boxes(self, boxes, extent: float) -> None:
        """New boxes, with the width of the cytoplasm's columns they are laid over, and
        mark the strip dirty (G-031)."""
        self.boxes = tuple(boxes)
        self.extent = extent
        self.update()

    def _scale(self) -> float:
        room = max(10.0, self.width() - 2 * MARGIN)
        return min(1.0, room / self.extent) if self.extent > 0 else 1.0

    def _origin(self) -> float:
        """The x, in the cytoplasm's units, drawn at the left margin: the left edge of a
        box at position 1 (G-031)."""
        return so.SPACING - so.GLINT_WIDTH / 2 - so.BOX_PAD_X

    def rect_of(self, box: so.GnothBox) -> QRectF:
        """The box's rectangle on the strip (G-031)."""
        scale, origin = self._scale(), self._origin()
        x = MARGIN + (box.left - origin) * scale
        return QRectF(x, MARGIN + box.row * so.ROW_STEP, box.width * scale, so.BOX_HEIGHT)

    def box_at(self, pos) -> so.GnothBox | None:
        """The box under a point, or None (G-031)."""
        for box in reversed(self.boxes):
            if self.rect_of(box).contains(pos):
                return box
        return None

    def mousePressEvent(self, event):
        """A left click emits the box's name, or an empty name on empty space (G-031)."""
        if event.button() == Qt.MouseButton.LeftButton:
            box = self.box_at(event.position())
            self.clicked.emit(box.name if box is not None else "")

    def event(self, e):
        """The tooltip is the box's label and terms (G-031)."""
        if e.type() == e.Type.ToolTip:
            box = self.box_at(e.pos())
            self.setToolTip(f"{box.label}: {box.sub or 'no terms'}\n[M86 p. 102]"
                            if box is not None else "")
        return super().event(e)

    def paintEvent(self, event):
        """Paint the boxes: a rounded rectangle with the name and the terms under it,
        selected ones in the selection's colour, the catchall dotted, an uncapped one
        dashed, ghosts faded, unconfirmed ones with an amber border (G-031)."""
        self.paints += 1
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.fillRect(self.rect(), QColor(BACKGROUND))
        if not self.boxes:
            p.setPen(QColor(MUTED))
            p.setFont(self.font_)
            p.drawText(self.rect().adjusted(MARGIN, 0, -MARGIN, 0),
                       Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                       "no gnoths yet")
            p.end()
            return
        for box in self.boxes:
            rect = self.rect_of(box)
            p.setOpacity(box.ghost if box.ghost > 0 else 1.0)
            fill = QColor(SELECTED_FILL if box.selected else BOX_FILL)
            border = QColor(SELECTED_BORDER if box.selected
                            else UNCONFIRMED if not box.confirmed
                            else CATCHALL_BORDER if box.kind == so.CATCHALL_KIND
                            else BOX_BORDER)
            pen = QPen(border, 1.6 if box.selected else 1.2)
            if box.kind == so.CATCHALL_KIND:
                pen.setStyle(Qt.PenStyle.DotLine)
            elif box.uncapped:
                pen.setStyle(Qt.PenStyle.DashLine)
            p.setPen(pen)
            p.setBrush(fill)
            p.drawRoundedRect(rect, 8, 8)
            p.setPen(QColor(TEXT))
            p.setFont(self.bold)
            inner = rect.adjusted(PAD, 1, -PAD, 0).toRect()
            p.drawText(inner, Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignHCenter,
                       self.metrics.elidedText(box.label, Qt.TextElideMode.ElideRight,
                                               inner.width()))
            p.setFont(self.font_)
            p.setPen(QColor(MUTED))
            p.drawText(inner, Qt.AlignmentFlag.AlignBottom | Qt.AlignmentFlag.AlignHCenter,
                       self.metrics.elidedText(box.sub, Qt.TextElideMode.ElideRight,
                                               inner.width()))
        p.end()


class CardList(LineList):
    """The cards as lines, `pane_lines(model)`, painted straight from the model (Numbo
    GUI, `log_view.py`'s `LogList`; G-028, G-031): headings bold on a tinted band,
    fields indented, bond lines further in, ghosts grey, a selected card outlined;
    hovering a bond line emits `bond_hovered(name)`, leaving it an empty name."""

    bond_hovered = Signal(str)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.lines = ()
        self.hovered = None
        self.setMouseTracking(True)
        self.viewport().setMouseTracking(True)

    def set_lines(self, lines) -> None:
        """New lines; the scroll range follows and the viewport is marked dirty (G-028)."""
        self.lines = tuple(lines)
        self.update_rows()
        self.viewport().update()

    def row_count(self) -> int:
        """How many lines (G-028)."""
        return len(self.lines)

    def line(self, row: int) -> so.CardLine:
        """The row's line (G-031)."""
        return self.lines[row]

    def mouseMoveEvent(self, event):
        """Hovering a bond line emits its name once; leaving it emits an empty name
        (PANES.md P3: "Hover a bond in a card's bond list to highlight the arc";
        G-031)."""
        row = self.row_at(event.position().y())
        bond = self.lines[row].bond if row is not None else None
        if bond != self.hovered:
            self.hovered = bond
            self.bond_hovered.emit(bond or "")

    def leaveEvent(self, event):
        """Leaving the list clears the hover (G-031)."""
        if self.hovered is not None:
            self.hovered = None
            self.bond_hovered.emit("")
        super().leaveEvent(event)

    def _paint_line(self, p, line, box):
        h = box.height()
        if line.kind == "head":
            p.fillRect(box, QColor(HEAD_TINT))
        elif line.kind == "template":
            p.fillRect(box, QColor(TEMPLATE_TINT))
        color = QColor(GREY if line.grey else MUTED if line.kind == "note" else TEXT)
        if line.kind in ("head", "template") and not line.grey:
            color = QColor(BOX_BORDER)
        if line.unconfirmed and line.kind == "head":
            color = QColor(UNCONFIRMED)
        x = PAD + (INDENT if line.kind in ("field", "note") else 2 * INDENT
                   if line.kind == "bond" else 0)
        p.setFont(self.bold if line.kind in ("head", "template") else self.font_)
        p.setPen(color)
        room = max(10, box.width() - x - PAD)
        flags = Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft
        p.drawText(QRect(x, box.y(), room, h), flags,
                   self.metrics.elidedText(line.text, Qt.TextElideMode.ElideRight, room))
        if line.kind == "bond" and line.bond == self.hovered:
            p.setPen(QPen(QColor(SELECTED_BORDER), 1))
            p.drawRect(QRect(x - 2, box.y(), room + 2, h - 1))
        if line.selected and line.kind == "head":
            p.setPen(QPen(QColor(SELECTED_BORDER), 1, Qt.PenStyle.DotLine))
            p.drawRect(box.adjusted(0, 0, -1, -1))


class SocratoplasmView(QWidget):
    """The socratoplasm dock's view (see the module docstring; G-030, G-031, G-032).
    `history` is the window's `EventLog`, read at `position`; `selection` the shared
    `Selection` (G-025). Signals: `selected(name)` after a click on a gnoth,
    `bond_hovered(name)` while the mouse is over a bond line (empty when it leaves)."""

    pane_id = "P3"
    title = "Socratoplasm"
    pages = PAGES
    selected = Signal(str)
    bond_hovered = Signal(str)

    def __init__(self, history, parent=None, selection=None):
        super().__init__(parent)
        self.history = history
        self.model = so.SocratoplasmModel(selection)
        self.position = -1
        self.events = 0
        self.parse_label = QLabel("()")
        self.parse_label.setMinimumWidth(1)      # clipped when narrow (G-058)
        font = QFont("monospace")
        font.setStyleHint(QFont.StyleHint.Monospace)
        font.setPointSizeF(13)
        font.setBold(True)
        self.parse_label.setFont(font)
        self.parse_label.setToolTip("The parse, as show-parse prints it [M86 pp. 174, 178]")
        self.parse_label.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        self.count_label = QLabel("")
        self.count_label.setStyleSheet(f"color: {MUTED};")
        self.copy_button = QPushButton("Copy as show")
        self.copy_button.setToolTip("Copy the gnoths: block as show prints it [M86 p. 168]")
        self.copy_button.clicked.connect(self.copy_show)
        self.strip = GnothStrip()
        self.strip.clicked.connect(self._strip_clicked)
        self.list = CardList()
        self.list.line_clicked.connect(self._line_clicked)
        self.list.bond_hovered.connect(self.bond_hovered)
        head = QHBoxLayout()
        head.setContentsMargins(MARGIN, 2, MARGIN, 0)
        head.addWidget(QLabel("parse:"))
        head.addWidget(self.parse_label, 1)
        head.addWidget(self.count_label)
        head.addWidget(self.copy_button)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)
        layout.addLayout(head)
        layout.addWidget(self.strip)
        layout.addWidget(self.list, 1)
        self.model.selection.subscribe(self._on_selection)
        self.redraw()

    def sizeHint(self):
        """Room for the parse, the strip and a few cards (G-011)."""
        return QSize(420, 360)

    @property
    def paints(self) -> int:
        """The strip's paint count (G-011)."""
        return self.strip.paints

    def parse(self) -> str:
        """The parse shown [M86 p. 174]."""
        return self.parse_label.text()

    def card_lines(self, name: str) -> list:
        """A card's field lines as shown: the gnoth's block of `show` [M86 p. 168]."""
        return [line.text for line in self.list.lines
                if line.name == name and line.kind == "field"]

    def lines(self) -> tuple:
        """The list's lines as last drawn (G-031)."""
        return self.list.lines

    def boxes(self) -> tuple:
        """The strip's boxes as last drawn (G-031)."""
        return self.strip.boxes

    # -- the view protocol (G-011, G-016) ----------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-016)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model and take the snapshot in force at `position` (G-030); a
        `start` begins a new run (G-011)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.model.on_event(event)
        self.model.update(self.history.state_at(self.position))

    def redraw(self, now=False) -> None:
        """Rebuild the parse, the boxes and the lines and mark the widgets dirty; with
        `now`, paint at once (Numbo GUI, `paint.py`; G-031)."""
        model = self.model
        self._set_text(self.parse_label, model.parse)
        n = len(model.gnoths())
        self._set_text(self.count_label, f"{n} gnoth{'s' if n != 1 else ''}"
                       + (" + catchall" if model.catchall is not None else ""))
        self.strip.set_boxes(so.gnoth_boxes(model), so.strip_extent(model))
        self.list.set_lines(so.pane_lines(model))
        if now:
            paint.paint_all()

    @staticmethod
    def _set_text(label, text):
        if label.text() != text:
            label.setText(text)

    # -- interactions (PANES.md P3) ---------------------------------------------------------

    def select_gnoth(self, name: str | None) -> tuple:
        """Select a gnoth with its glom and members in the shared selection (a second
        click clears; None or an empty name clears), paint now and emit `selected`;
        returns the names selected (G-025, G-031)."""
        names = self.model.select_gnoth(name or None)
        self.redraw(now=True)
        self.selected.emit(name or "")
        return names

    def copy_show(self) -> str:
        """Put the `gnoths:` block of `show` for the cursor on the clipboard and return
        it (PANES.md P3: "Copy as `show`"; G-030)."""
        text = "\n".join(self.model.show_block())
        QApplication.clipboard().setText(text)
        return text

    def _strip_clicked(self, name):
        self.select_gnoth(name)

    def _line_clicked(self, line):
        if line.name is not None:
            self.select_gnoth(line.name)

    def _on_selection(self, _selection):
        self.redraw(now=True)

    def render_png(self, path):
        """Grab the pane to a PNG at `path` (Numbo GUI, `coderack_view.py`)."""
        image = self.grab()
        if not image.save(path):
            raise OSError(f"could not write {path}")
        return image

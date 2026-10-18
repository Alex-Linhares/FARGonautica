"""P3 Slipnet grid: French's grid of square boxes to the right of the thermometer, each
node's activation as "the number in the upper right-hand corner" and as "the black
square within the box" [F95 p. 95], the labels along the bottom edge as the dumps print
them [F95 pp. 96-107 (img)]; a seventh row for the nodes the screen lacks, labelled "GUI
design, not French's" (D-007; ANSWERS.md 4); a jolt outlines its box for one event; French's
nearest chapter 4 grid beside the run's on request (G-030 to G-032).

`SlipnetGridWidget` is a `QWidget` that paints `models/slipnet_grid.py`'s `grid_layout`,
a pure function of the model, the size and the options (Numbo GUI, `coderack_view.py`:
a widget that paints its model); hovering a box shows the node's hover text, a left
click toggles the node in the window's shared selection (G-022). `SlipnetGridView` is
the pane: a bar with the French option as a checkable action (`french_action`, enabled
only while the run is `surround-05`) over the grid, keeping the view protocol the window
expects (G-011) and taking the window's history and selection through `attach`.
"""

from PySide6.QtCore import QRectF, Qt
from PySide6.QtGui import QAction, QColor, QFont, QImage, QPainter, QPen
from PySide6.QtWidgets import QHBoxLayout, QLabel, QSizePolicy, QToolButton, QVBoxLayout, QWidget

from tabletop.gui import paint
from tabletop.gui.models import slipnet_grid as sg

__all__ = ["BACKGROUND", "BOX", "EXTRA_BOX", "FRENCH", "JOLT", "MIN_SIZE", "OPTION_KEYS",
           "SELECTED", "SQUARE", "SlipnetGridView", "SlipnetGridWidget"]

# The smallest the pane may be (G-014): seven columns of 20 px, with the bar.
MIN_SIZE = (150, 150)
OPTION_KEYS = ("french",)            # `controls/grid-*` in the settings (G-032)
BACKGROUND = QColor("#fbfbf8")
BOX, EXTRA_BOX = QColor("#1a1a1a"), QColor("#8a8a8a")       # the borders: French's rows, the seventh
ROLE_BOX = QColor("#5b2a86")         # the roles' band's borders and caption (G-062)
SQUARE = QColor("#000000")           # "the black square within the box" [F95 p. 95]
TEXT, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
JOLT = QColor("#e6007e")             # the jolt outline (G-030)
SELECTED = QColor("#e07b00")         # the selection highlight (G-022)
FRENCH = QColor("#b00020")           # French's numbers (G-032)


class SlipnetGridWidget(QWidget):
    """The grid itself (G-031): paints the `GridLayout` of its model for its size and the
    `french` option; `drawn` is the last one painted, `paints` the count for the
    window's probe (G-011); a click toggles the node in the `selection` (G-022)."""

    def __init__(self, model, selection=None, parent=None):
        super().__init__(parent)
        self.model = model
        self.selection = selection
        self.french = False
        self.drawn = sg.grid_layout(model, 10, 10)
        self.paints = 0
        self.setMinimumSize(MIN_SIZE[0], MIN_SIZE[1] - 24)
        self.setMouseTracking(True)
        self.setToolTip("hover a box for its node")

    def current_layout(self):
        """The layout for the current size, option and selection (G-031)."""
        selected = self.selection.name if self.selection is not None \
            and self.selection.kind == "node" else None
        return sg.grid_layout(self.model, self.width(), self.height(), self.french, selected)

    def mouseMoveEvent(self, event):
        """The node under the mouse as the tooltip (G-032)."""
        box = sg.box_at(self.drawn, event.position().x(), event.position().y())
        if box is None:
            self.setToolTip(self.drawn.caption if self.drawn.note == "" else
                            f"{self.drawn.caption}\n{self.drawn.note}")
        else:
            self.setToolTip(self.model.tooltip(box.name))

    def mousePressEvent(self, event):
        """A left click toggles the node under the mouse in the shared selection; on no
        box, clears it (G-022, G-030)."""
        if event.button() != Qt.MouseButton.LeftButton or self.selection is None:
            return
        box = sg.box_at(self.drawn, event.position().x(), event.position().y())
        if box is None:
            self.selection.clear()
        else:
            self.selection.toggle(*self.model.selection_of(box.name))

    def paintEvent(self, event):
        """Paint the boxes, their labels, numbers and squares, the outlines, French's
        numbers and the caption (G-031); counts the paints (G-011)."""
        self.paints += 1
        self.drawn = lay = self.current_layout()
        painter = QPainter(self)
        painter.fillRect(self.rect(), BACKGROUND)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing, False)
        small = QFont(painter.font())
        small.setPointSizeF(max(5.0, min(8.0, lay.side / 5.5)))
        painter.setFont(small)
        for box in lay.boxes:
            self._paint_box(painter, box)
        self._paint_caption(painter, lay)
        painter.end()

    def _paint_box(self, painter, box) -> None:
        """One box: the border, the label along the bottom, the number in the upper
        right-hand corner, the black square in the middle [F95 p. 95], the jolt and
        selection outlines, and French's number in the lower right when on (G-031)."""
        rect = QRectF(box.x, box.y, box.side, box.side)
        painter.setPen(QPen(BOX if box.on_screen else ROLE_BOX if box.role else EXTRA_BOX, 1,
                            Qt.PenStyle.SolidLine if box.on_screen else Qt.PenStyle.DashLine))
        painter.setBrush(Qt.BrushStyle.NoBrush)
        painter.drawRect(rect)
        if box.jolted:
            painter.setPen(QPen(JOLT, 2))
            painter.drawRect(rect.adjusted(2, 2, -2, -2))
        if box.selected:
            painter.setPen(QPen(SELECTED, 2))
            painter.drawRect(rect.adjusted(1, 1, -1, -1))
        if box.square > 0:
            half = box.square / 2
            centre = rect.center()
            painter.fillRect(QRectF(centre.x() - half, centre.y() - half, box.square, box.square),
                             SQUARE)
        painter.setPen(QPen(TEXT, 1))
        painter.drawText(rect.adjusted(1, 1, -2, 0), Qt.AlignmentFlag.AlignRight
                         | Qt.AlignmentFlag.AlignTop, box.number)
        painter.drawText(rect.adjusted(2, 0, -1, -1), Qt.AlignmentFlag.AlignLeft
                         | Qt.AlignmentFlag.AlignBottom, box.label)
        if box.french is not None:
            painter.setPen(QPen(FRENCH, 1))
            painter.drawText(rect.adjusted(1, 0, -2, -1), Qt.AlignmentFlag.AlignRight
                             | Qt.AlignmentFlag.AlignBottom, str(box.french))

    def _paint_caption(self, painter, lay) -> None:
        """The caption under the seventh row, the roles' band's under the band (G-062),
        and the note on French's dump on the line under it (G-031)."""
        painter.setPen(QPen(MUTED, 1))
        flags = Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter
        painter.drawText(QRectF(2, lay.caption_y, self.width() - 4, sg.CAPTION_HEIGHT), flags,
                         lay.caption)
        painter.setPen(QPen(ROLE_BOX, 1))
        painter.drawText(QRectF(2, lay.role_caption_y, self.width() - 4, sg.CAPTION_HEIGHT),
                         flags, lay.role_caption)
        if lay.note:
            painter.setPen(QPen(FRENCH, 1))
            painter.drawText(QRectF(2, lay.role_caption_y + sg.CAPTION_HEIGHT, self.width() - 4,
                                    sg.CAPTION_HEIGHT), flags, lay.note)


class SlipnetGridView(QWidget):
    """The Slipnet grid pane (G-030 to G-032; G-011's view protocol): the bar with the
    French option over the `SlipnetGridWidget`; `attach(history, selection)` takes the
    window's; `set_position`, `on_event`, `redraw(now=False)`, `paints`, `events`,
    `pane_id`, `title`, `pages`; `grid_state` and `set_grid_state` for the settings."""

    def __init__(self, title="Slipnet grid", pane_id="P3", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = sg.SlipnetGridModel()
        self.history = None
        self.position = -1
        self.grid = SlipnetGridWidget(self.model)
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self._build_bar()
        column = QVBoxLayout(self)
        column.setContentsMargins(2, 2, 2, 2)
        column.setSpacing(2)
        column.addWidget(self.bar)
        column.addWidget(self.grid, 1)

    def _build_bar(self) -> None:
        """The French option as a tool button, and the label (G-032)."""
        self.french_action = QAction("French's ch. 4", self)
        self.french_action.setCheckable(True)
        self.french_action.setEnabled(False)
        self.french_action.setToolTip("the activations of French's chapter 4 dump nearest in "
                                      "codelets, in red beside the run's [F95 pp. 96-107 (img)]; "
                                      f"offered for {sg.CH4_PROBLEM} only; " + sg.NOT_FRENCHS)
        self.french_action.toggled.connect(self._on_option)
        self.bar = QWidget()
        row = QHBoxLayout(self.bar)
        row.setContentsMargins(0, 0, 0, 0)
        row.setSpacing(4)
        self.buttons = {}
        button = QToolButton()
        button.setDefaultAction(self.french_action)
        button.setMinimumWidth(24)
        self.buttons["french"] = button
        row.addWidget(button)
        self.label = QLabel('41 boxes [F95 p. 95]; seventh row: ' + sg.NOT_FRENCHS)
        self.label.setToolTip(self.toolTip())
        self.label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        row.addWidget(self.label, 1)

    def attach(self, history, selection) -> None:
        """Take the window's history (for the snapshot in force) and its shared
        selection (G-022, G-032)."""
        self.history = history
        self.grid.selection = selection
        selection.subscribe(self._on_selection)

    @property
    def paints(self) -> int:
        """The grid's paint count (the window's probe; G-011)."""
        return self.grid.paints

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    @property
    def layout_now(self):
        """The grid's geometry for its current size and option (G-031)."""
        return self.grid.current_layout()

    def grid_state(self) -> dict:
        """The options, by `OPTION_KEYS` (G-032)."""
        return {"french": self.french_action.isChecked()}

    def set_grid_state(self, state) -> None:
        """Set the options named in `state`; the others stay (G-032)."""
        if "french" in state:
            self.french_action.setChecked(bool(state["french"]))

    def _on_option(self, _checked=False) -> None:
        """The option changed: the grid takes it and redraws now (G-032)."""
        self.grid.french = self.french_action.isChecked()
        self.redraw(now=True)

    def _on_selection(self, _selection) -> None:
        """The shared selection changed, from this pane or another: redraw now (G-022)."""
        self.redraw(now=True)

    def set_position(self, index: int) -> None:
        """Which event comes next: the snapshot in force is read at `redraw` (G-017's
        protocol; G-032)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model; the French option is offered only while the run is
        `surround-05` (G-030, G-032)."""
        self.model.on_event(event)
        self.french_action.setEnabled(self.model.french_offered)

    def redraw(self, now=False) -> None:
        """Apply the snapshot in force to the hover text, mark the grid dirty and, with
        `now`, paint it at once (Numbo GUI, `paint.py`; G-032)."""
        if self.history is not None and self.position >= 0:
            self.model.update(self.history.state_at(self.position))
        self.grid.update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the grid to a PNG file (RGB), for the tests (G-031)."""
        self.grid.grab().toImage().convertToFormat(QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()

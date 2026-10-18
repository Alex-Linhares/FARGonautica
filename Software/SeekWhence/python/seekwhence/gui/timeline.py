"""The timeline (P10): where the views are in the run's history, and the controls to
move there (PANES.md P10; Numbo GUI, `timeline.py`, adopted; G-042, G-043).

A slider over the events (it seeks when released, or at each click or key: a jump on a
long run re-feeds the models, so it does not seek at every pixel of a drag), one event
back and forward, a go-to box (`term 5`, `clock 812`, `event 402`), and under the
slider two strips drawn from `models/timeline.py`'s `TimelineModel`:
  - the marker strip: a tick per term [M86 p. 164], a diamond per guess, green once
    `ok` and red once `no` [M86 pp. 166, 175], a triangle per refutation [M86 p. 168], a
    diamond per slip of the reigning class [M86 pp. 99, 174], a bar per blast
    [M86 pp. 75, 92], and a line at the event shown; a click seeks to the marker under
    the mouse, or to the nearest event;
  - the parse strip: the parse at each term boundary, as `show-parse` prints it
    [M86 p. 174], each cell reaching to the next boundary: the time-lapse of
    Hofstadter's Figure 1-1 [FCCA p. 41] and of the mountain chain solved by successive
    bracketing [FCCA pp. 64-65]; a click seeks to the boundary.

It only asks: `seek_requested(index)` and `goto_requested(kind, n)` go to the window,
which can refuse (a run is live: `set_scrubbable(False)` disables the controls then).
"""

from PySide6.QtCore import QPointF, QRectF, Qt, Signal
from PySide6.QtGui import QColor, QFontMetrics, QPainter, QPen, QPolygonF
from PySide6.QtWidgets import (QGridLayout, QLabel, QLineEdit, QPushButton, QSlider, QSizePolicy,
                               QWidget)

from seekwhence.gui.models import timeline as tm

__all__ = ["MarkerStrip", "ParseStrip", "TimelineBar"]

MARKER_HEIGHT = 26      # two rows: the term ticks with their numbers, then the marks
PARSE_HEIGHT = 18
ROW_HEIGHT = 12
CURSOR = QColor("#e6007e")
PARSE_FILL = QColor("#f3f3f3")
PARSE_EDGE = QColor("#b0b0b0")
PARSE_INK = QColor("#1a1a1a")
PARSE_FUTURE = QColor("#9a9a9a")
STRIP_BACKGROUND = QColor("#ffffff")


class MarkerStrip(QWidget):
    """The marks under the slider, painted from the model's `markers` (see the module
    docstring; G-043). `paints` counts its paints."""

    seek_requested = Signal(int)

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.paints = 0
        self.scrubbable = False
        self.setFixedHeight(MARKER_HEIGHT)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setMouseTracking(True)
        self.setAutoFillBackground(True)
        self.setToolTip("terms, guesses (green ok, red no), refutations, slips and blasts "
                        "[M86 pp. 164-179]")

    def marker_at(self, x) -> tm.Marker | None:
        """The marker nearest to the viewport x within four pixels, or None (G-043)."""
        count, width = self.model.count, self.width()
        best, best_d = None, 5.0
        for marker in self.model.markers:
            d = abs(tm.marker_x(marker.index, count, width) - x)
            if d < best_d:
                best, best_d = marker, d
        return best

    def paintEvent(self, event):
        """Paint the strip from the model (G-043)."""
        self.paints += 1
        p = QPainter(self)
        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.fillRect(self.rect(), STRIP_BACKGROUND)
        model = self.model
        count, width = model.count, self.width()
        p.setFont(self.font())
        small = p.font()
        small.setPointSizeF(max(6.0, small.pointSizeF() - 2))
        p.setFont(small)
        for marker in model.markers:
            style = tm.marker_style(marker)
            x = tm.marker_x(marker.index, count, width)
            y = style.row * ROW_HEIGHT
            color = QColor(style.color)
            p.setPen(QPen(color, 1))
            p.setBrush(color)
            if style.shape == "tick":
                p.drawLine(QPointF(x, y + 1), QPointF(x, y + ROW_HEIGHT - 1))
                p.drawText(QPointF(x + 2, y + ROW_HEIGHT - 3), marker.label)
            elif style.shape == "diamond":
                r = 4.0
                p.drawPolygon(QPolygonF([QPointF(x, y + 2), QPointF(x + r, y + 2 + r),
                                         QPointF(x, y + 2 + 2 * r), QPointF(x - r, y + 2 + r)]))
            elif style.shape == "triangle":
                r = 4.5
                p.drawPolygon(QPolygonF([QPointF(x - r, y + 2 + 2 * r), QPointF(x + r, y + 2 + 2 * r),
                                         QPointF(x, y + 2)]))
            else:   # bar
                p.drawRect(QRectF(x - 1.5, y + 1, 3, ROW_HEIGHT - 2))
        if 0 <= model.position < count:
            x = tm.marker_x(model.position, count, width)
            p.setPen(QPen(CURSOR, 1.5))
            p.drawLine(QPointF(x, 0), QPointF(x, self.height()))
        p.end()

    def mousePressEvent(self, event):
        """A click seeks, when scrubbable (G-043)."""
        if not self.scrubbable or self.model.count == 0:
            return
        x = event.position().x()
        marker = self.marker_at(x)
        index = marker.index if marker is not None else tm.index_at(x, self.model.count, self.width())
        self.seek_requested.emit(index)

    def event(self, e):
        """The tooltip names what is under the mouse (G-043)."""
        if e.type() == e.Type.ToolTip:
            marker = self.marker_at(e.pos().x())
            if marker is not None:
                self.setToolTip(tm.marker_tooltip(marker))
            else:
                self.setToolTip("terms, guesses (green ok, red no), refutations, slips and "
                                "blasts [M86 pp. 164-179]")
        return super().event(e)


class ParseStrip(QWidget):
    """The parse history under the markers, painted from the model's `parses` (see the
    module docstring) [FCCA pp. 41, 64-65] (G-043). `paints` counts its paints."""

    seek_requested = Signal(int)

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.paints = 0
        self.scrubbable = False
        self.setFixedHeight(PARSE_HEIGHT)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setAutoFillBackground(True)
        self.setToolTip("the parse at each term boundary [M86 p. 174]")

    def cells(self) -> list:
        """The strip's cells, `(x0, x1, entry)` (G-043)."""
        return tm.parse_cells(self.model.parses, self.model.count, self.width())

    def cell_at(self, x) -> tuple | None:
        """The cell under the viewport x, or None (G-043)."""
        for cell in self.cells():
            if cell[0] <= x <= cell[1]:
                return cell
        return None

    def paintEvent(self, event):
        """Paint the strip from the model (G-043)."""
        self.paints += 1
        p = QPainter(self)
        p.fillRect(self.rect(), STRIP_BACKGROUND)
        metrics = QFontMetrics(self.font())
        h = self.height()
        position = self.model.position
        for x0, x1, entry in self.cells():
            rect = QRectF(x0, 1, max(1.0, x1 - x0), h - 2)
            p.fillRect(rect, PARSE_FILL)
            p.setPen(QPen(PARSE_EDGE, 1))
            p.drawLine(QPointF(x0, 1), QPointF(x0, h - 1))
            p.setPen(PARSE_FUTURE if entry.index > position else PARSE_INK)
            text = metrics.elidedText(entry.parse or "()", Qt.TextElideMode.ElideRight,
                                      int(rect.width()) - 4)
            p.drawText(rect.adjusted(2, 0, -2, 0), Qt.AlignmentFlag.AlignVCenter, text)
        p.end()

    def mousePressEvent(self, event):
        """A click seeks, when scrubbable (G-043)."""
        if not self.scrubbable:
            return
        cell = self.cell_at(event.position().x())
        if cell is not None:
            self.seek_requested.emit(cell[2].index)

    def event(self, e):
        """The tooltip names what is under the mouse (G-043)."""
        if e.type() == e.Type.ToolTip:
            cell = self.cell_at(e.pos().x())
            if cell is not None:
                entry = cell[2]
                self.setToolTip(f"after {entry.terms} term{'s' if entry.terms != 1 else ''}: "
                                f"{entry.parse or '()'}  (event {entry.index + 1})")
            else:
                self.setToolTip("the parse at each term boundary [M86 p. 174]")
        return super().event(e)


class TimelineBar(QWidget):
    """The timeline toolbar's widget (see the module docstring; Numbo GUI,
    `timeline.py`). `history` is the window's `EventLog`; `model` its
    `TimelineModel`."""

    seek_requested = Signal(int)
    goto_requested = Signal(str, int)

    pane_id = "P10"
    title = "Timeline"
    pages = "Numbo GUI, timeline.py; [FCCA pp. 41, 64-65]"

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.model = tm.TimelineModel(history)
        self.count = 0
        self.position = -1
        self.back_button = QPushButton("◀")
        self.back_button.setToolTip("one event back")
        self.forward_button = QPushButton("▶")
        self.forward_button.setToolTip("one event forward (drawn as a step)")
        for b in (self.back_button, self.forward_button):
            b.setFixedWidth(32)
        self.slider = QSlider(Qt.Orientation.Horizontal)
        self.slider.setTracking(False)
        self.slider.setRange(0, 0)
        # The mouse's only: the window's left and right arrows move one event.
        self.slider.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.label = QLabel()
        # A fixed width: the text changes at every event, and a label whose size can
        # change lays out its parents again each time.
        self.label.setFixedWidth(self.label.fontMetrics().horizontalAdvance(
            "event 000000 of 000000: template-abandoned   term 000   clock 000000 …") + 8)
        self.label.setFixedHeight(self.label.sizeHint().height())
        self.goto_edit = QLineEdit()
        self.goto_edit.setPlaceholderText("term 5 / clock 812 / event 402")
        self.goto_edit.setToolTip("go to a term, a clock (task) or an event")
        self.goto_edit.setFixedWidth(self.goto_edit.fontMetrics().horizontalAdvance(
            "term 5 / clock 812 / event 402") + 24)
        self.go_button = QPushButton("Go")
        self.go_button.setFixedWidth(40)
        self.markers = MarkerStrip(self.model)
        self.parses = ParseStrip(self.model)

        layout = QGridLayout(self)
        layout.setContentsMargins(6, 2, 6, 2)
        layout.setHorizontalSpacing(6)
        layout.setVerticalSpacing(1)
        layout.addWidget(self.back_button, 0, 0)
        layout.addWidget(self.forward_button, 0, 1)
        layout.addWidget(self.slider, 0, 2)
        layout.addWidget(self.label, 0, 3)
        layout.addWidget(self.goto_edit, 0, 4)
        layout.addWidget(self.go_button, 0, 5)
        layout.addWidget(self.markers, 1, 2)
        layout.addWidget(self.parses, 2, 2)
        layout.setColumnStretch(2, 1)

        self.slider.valueChanged.connect(self.seek_requested)
        self.slider.sliderMoved.connect(self._preview)
        self.back_button.clicked.connect(lambda: self.seek_requested.emit(self.position - 1))
        self.forward_button.clicked.connect(lambda: self.seek_requested.emit(self.position + 1))
        self.go_button.clicked.connect(self.go)
        self.goto_edit.returnPressed.connect(self.go)
        self.markers.seek_requested.connect(self.seek_requested)
        self.parses.seek_requested.connect(self.seek_requested)
        self.set_position(-1)

    def _preview(self, value):
        self.label.setText(self.model.text(value) + " …")

    def go(self) -> bool:
        """The go-to box: emit `goto_requested(kind, n)` for a text that reads; False
        (and the box marked) for one that does not (G-043)."""
        target = tm.parse_goto(self.goto_edit.text())
        if target is None:
            self.goto_edit.setStyleSheet("background: #fde2e2")
            return False
        self.goto_edit.setStyleSheet("")
        self.goto_requested.emit(*target)
        return True

    def set_position(self, index: int) -> None:
        """The views show event `index` of the history: the slider, the label and the
        strips follow (Numbo GUI, `timeline.py`; G-042)."""
        model = self.model
        model.sync()
        model.set_position(index)
        self.position, self.count = index, model.count
        s = self.slider
        s.blockSignals(True)
        if s.maximum() != max(self.count - 1, 0):
            s.setMaximum(max(self.count - 1, 0))
        if s.value() != max(index, 0):
            s.setValue(max(index, 0))
        s.blockSignals(False)
        text = model.text(index)
        if self.label.text() != text:
            self.label.setText(text)
        self.markers.update()
        self.parses.update()

    def text(self) -> str:
        """The position text shown (Numbo GUI, `timeline.py`)."""
        return self.label.text()

    def set_scrubbable(self, on: bool) -> None:
        """Whether the controls move the views (not while a run is live) (Numbo GUI,
        `timeline.py`)."""
        for w in (self.slider, self.back_button, self.forward_button, self.goto_edit,
                  self.go_button):
            w.setEnabled(on)
        self.markers.scrubbable = on
        self.parses.scrubbable = on

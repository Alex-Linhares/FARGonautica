"""P11 Timeline (PANES.md P11; Numbo GUI, `timeline.py`, adopted through the Seek-Whence
GUI's; G-043): where the views are in the run's history, and the controls to move there.

A slider over the events (it seeks when released, or at each click or key: a jump on a
long run re-feeds the models, so it does not seek at every pixel of a drag), one event
back and forward, a go-to-codelet box, and under the slider a strip drawn from
`models/timeline.py`'s `TimelineModel`:
  - the markers of PANES.md P11: the first object noticed (a grey tick), each
    correspondence built (a blue diamond), each competition (a magenta bar), each
    Worldview change (a green diamond), the context pushes and pops (purple triangles, up
    and down; rare, ANSWERS.md 11), the first find-answer posting (an orange tick) and
    the answer (a black bar); a line at the event shown; a click seeks to the marker
    under the mouse, or to the nearest event;
  - the thin strip of T (the temperature plot's series, G-028), 0 at the bottom and 100
    at the top, grey while held (D-049).

It only asks: `seek_requested(index)` and `codelet_requested(n)` go to the window, which
can refuse (a run is live: `set_scrubbable(False)` disables the controls then; Numbo's
rule). It keeps the view protocol the window expects (G-011): `on_event`, `set_position`,
`redraw(now=False)`, `paints` (the strip's), `events`, `pane_id`, `title`, `pages`.
"""

from PySide6.QtCore import QPointF, QRectF, Qt, Signal
from PySide6.QtGui import QBrush, QColor, QPainter, QPen, QPolygonF
from PySide6.QtWidgets import (QGridLayout, QLabel, QPushButton, QSizePolicy, QSlider, QSpinBox,
                               QWidget)

from tabletop import events as ev
from tabletop.gui import paint
from tabletop.gui.models import timeline as tm

__all__ = ["PAGES", "TimelineStrip", "TimelineBar"]

PAGES = "Numbo GUI, timeline.py"
ROW_HEIGHT = 11             # the two marker rows
T_HEIGHT = 14               # the strip of T under them
STRIP_HEIGHT = 2 * ROW_HEIGHT + T_HEIGHT
CURSOR = QColor("#e6007e")
STRIP_BACKGROUND = QColor("#f7f7f7")
T_LINE = QColor("#1a1a1a")
T_HELD = QColor("#9a9a9a")
T_FLOOR = QColor("#dcdcdc")
# The pens and brushes of the markers' colours and of the strip of T, made once: the
# strip paints every marker and reading at every event, and a long run has hundreds of
# markers and thousands of readings (G-058).
_MARKER_PENS = {}
_T_PENS = {True: QPen(T_HELD, 1.2), False: QPen(T_LINE, 1.2)}


def _marker_pen(color: str) -> tuple:
    """The (pen, brush) of a marker colour, cached (G-058)."""
    pair = _MARKER_PENS.get(color)
    if pair is None:
        qcolor = QColor(color)
        pair = _MARKER_PENS[color] = (QPen(qcolor, 1), QBrush(qcolor))
    return pair
STRIP_TIP = ("markers: first notice, correspondences, competitions, Worldview changes, "
             "pushes and pops, the first find-answer posting, the answer; under them the "
             "temperature, 0 to 100")


class TimelineStrip(QWidget):
    """The markers and the strip of T under the slider, painted from the model (see the
    module docstring; G-043). `paints` counts its paints."""

    seek_requested = Signal(int)

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.paints = 0
        self.scrubbable = False
        self.setFixedHeight(STRIP_HEIGHT)
        self.setSizePolicy(QSizePolicy.Policy.Expanding, QSizePolicy.Policy.Fixed)
        self.setMouseTracking(True)
        self.setAutoFillBackground(True)
        self.setToolTip(STRIP_TIP)

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
        self._paint_temperature(p, count, width)
        small = p.font()
        small.setPointSizeF(max(6.0, small.pointSizeF() - 2))
        p.setFont(small)
        for marker in model.markers:
            self._paint_marker(p, marker, tm.marker_x(marker.index, count, width))
        if 0 <= model.position < count:
            x = tm.marker_x(model.position, count, width)
            p.setPen(QPen(CURSOR, 1.5))
            p.drawLine(QPointF(x, 0), QPointF(x, self.height()))
        p.end()

    def _paint_temperature(self, p, count, width):
        """The thin strip of T: a line from reading to reading, grey while held, over a
        floor line at T 0 (G-028, G-043; D-049)."""
        top = 2 * ROW_HEIGHT
        p.setPen(QPen(T_FLOOR, 1))
        p.drawLine(QPointF(tm.INSET, top + T_HEIGHT - 1), QPointF(width - tm.INSET, top + T_HEIGHT - 1))
        points = tm.strip_points(self.model.readings, count, width, T_HEIGHT)
        # One polyline per run of readings alike in `held` (each segment takes the colour
        # of its later reading), rather than a line per segment (G-058).
        run, held = [], None
        for k in range(1, len(points)):
            x0, y0, _held0 = points[k - 1]
            x1, y1, held1 = points[k]
            if held1 != held and run:
                p.setPen(_T_PENS[held])
                p.drawPolyline(QPolygonF(run))
                run = []
            if not run:
                run.append(QPointF(x0, top + y0))
            run.append(QPointF(x1, top + y1))
            held = held1
        if run:
            p.setPen(_T_PENS[held])
            p.drawPolyline(QPolygonF(run))
        if len(points) == 1:
            x, y, held = points[0]
            p.setPen(_T_PENS[held])
            p.drawPoint(QPointF(x, top + y))

    @staticmethod
    def _paint_marker(p, marker, x):
        """One marker by its style (G-043)."""
        style = tm.marker_style(marker)
        y = style.row * ROW_HEIGHT
        pen, brush = _marker_pen(style.color)
        p.setPen(pen)
        p.setBrush(brush)
        if style.shape == "tick":
            p.drawLine(QPointF(x, y + 1), QPointF(x, y + ROW_HEIGHT - 1))
            p.drawText(QPointF(x + 2, y + ROW_HEIGHT - 2), marker.label)
        elif style.shape == "diamond":
            r = 4.0
            p.drawPolygon(QPolygonF([QPointF(x, y + 1), QPointF(x + r, y + 1 + r),
                                     QPointF(x, y + 1 + 2 * r), QPointF(x - r, y + 1 + r)]))
        elif style.shape == "triangle":
            r = 4.0
            p.drawPolygon(QPolygonF([QPointF(x - r, y + 1 + 2 * r), QPointF(x + r, y + 1 + 2 * r),
                                     QPointF(x, y + 1)]))
        elif style.shape == "triangle-down":
            r = 4.0
            p.drawPolygon(QPolygonF([QPointF(x - r, y + 1), QPointF(x + r, y + 1),
                                     QPointF(x, y + 1 + 2 * r)]))
        elif style.shape == "dot":           # a role change (G-062)
            p.drawEllipse(QPointF(x, y + ROW_HEIGHT / 2), 2.5, 2.5)
        else:   # bar
            p.drawRect(QRectF(x - 1.5, y + 1, 3, ROW_HEIGHT - 2))

    def mousePressEvent(self, event):
        """A click seeks, when scrubbable (G-043)."""
        if not self.scrubbable or self.model.count == 0:
            return
        x = event.position().x()
        marker = self.marker_at(x)
        index = marker.index if marker is not None else tm.index_at(x, self.model.count, self.width())
        self.seek_requested.emit(index)

    def event(self, e):
        """The tooltip names the marker under the mouse (G-043)."""
        if e.type() == e.Type.ToolTip:
            marker = self.marker_at(e.pos().x())
            self.setToolTip(tm.marker_tooltip(marker) if marker is not None else STRIP_TIP)
        return super().event(e)


class TimelineBar(QWidget):
    """The timeline toolbar's widget (see the module docstring; Numbo GUI,
    `timeline.py`; G-043). `history` is the window's `EventLog`; `model` its
    `TimelineModel`."""

    seek_requested = Signal(int)
    codelet_requested = Signal(int)

    pane_id = "P11"
    title = "Timeline"
    pages = PAGES

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.model = tm.TimelineModel(history)
        self.history = history
        self.count = 0
        self.position = -1
        self.events = 0
        self.setToolTip(f"{self.pane_id} {self.title}: {self.pages}")
        self._build_controls()
        self.strip = TimelineStrip(self.model)
        layout = QGridLayout(self)
        layout.setContentsMargins(6, 1, 6, 1)
        layout.setHorizontalSpacing(6)
        layout.setVerticalSpacing(1)
        layout.addWidget(self.back_button, 0, 0)
        layout.addWidget(self.forward_button, 0, 1)
        layout.addWidget(self.slider, 0, 2)
        layout.addWidget(self.label, 0, 3)
        layout.addWidget(self.codelet_spin, 0, 4)
        layout.addWidget(self.go_button, 0, 5)
        layout.addWidget(self.strip, 1, 2)
        layout.setColumnStretch(2, 1)
        self.setMinimumWidth(400)
        self.slider.valueChanged.connect(self.seek_requested)
        self.slider.sliderMoved.connect(self._preview)
        self.back_button.clicked.connect(lambda: self.seek_requested.emit(self.position - 1))
        self.forward_button.clicked.connect(lambda: self.seek_requested.emit(self.position + 1))
        self.go_button.clicked.connect(lambda: self.codelet_requested.emit(self.codelet_spin.value()))
        self.strip.seek_requested.connect(self.seek_requested)
        self.set_position(-1)
        self.set_scrubbable(False)

    def _build_controls(self):
        """The buttons, the slider, the label and the go-to box (Numbo GUI,
        `timeline.py`)."""
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
            "event 000000 of 000000: CorrespondenceProposed   codelet 000000 …") + 8)
        self.label.setFixedHeight(self.label.sizeHint().height())
        self.codelet_spin = QSpinBox()
        self.codelet_spin.setRange(0, 0)
        self.codelet_spin.setToolTip("a codelet of the run (its ordinal)")
        self.go_button = QPushButton("Go to codelet")

    def _preview(self, value):
        self.label.setText(self.model.text(value) + " …")

    # -- the view protocol (G-011) ------------------------------------------------------

    @property
    def paints(self) -> int:
        """The strip's paint count (the window's probe; G-011)."""
        return self.strip.paints

    def on_event(self, event) -> None:
        """Count the event; a `RunStarted` begins a new run (G-011)."""
        if isinstance(event, ev.RunStarted):
            self.events = 0
        self.events += 1

    def set_position(self, index: int) -> None:
        """The views show event `index` of the history: the slider, the label, the
        go-to box's range and the strip follow (Numbo GUI, `timeline.py`; G-043)."""
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
        lo, hi = self.history.codelets() or (0, 0)
        if (self.codelet_spin.minimum(), self.codelet_spin.maximum()) != (lo, hi):
            self.codelet_spin.setRange(lo, hi)
        text = model.text(index)
        if self.label.text() != text:
            self.label.setText(text)

    def redraw(self, now=False) -> None:
        """Mark the strip dirty and, with `now`, paint it at once (Numbo GUI,
        `paint.py`; G-043)."""
        self.strip.update()
        if now:
            paint.paint_all()

    def text(self) -> str:
        """The position text shown (Numbo GUI, `timeline.py`)."""
        return self.label.text()

    def set_scrubbable(self, on: bool) -> None:
        """Whether the controls move the views (not while a run is live) (Numbo GUI,
        `timeline.py`)."""
        for w in (self.slider, self.back_button, self.forward_button, self.codelet_spin,
                  self.go_button):
            w.setEnabled(on)
        self.strip.scrubbable = on

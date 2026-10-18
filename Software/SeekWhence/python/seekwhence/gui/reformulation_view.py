"""The reformulation pane (P7): stage two as a storyboard, one episode per refutation with
its cards in the order of pp. 87-126, the freeze-dried list and the threshold's course, a
view of `models/reformulation.py`'s `ReformulationModel` (PANES.md P7; G-039, G-040,
G-041).

The storyboard is the thesis's own way of showing reformulation: each gnoth operation is
drawn as its examples are, the gnoths' brackets before and after with the glom names
under the groups, `gnoth2: [ (1 1) (2 2) ]` over `glom3 glom5` [M86 p. 110], and the
operations in series stack the successive parses, `[] (disappears)`, `(emerges)`
[M86 p. 120]. The other cards repeat the transcript's lines: `set-out -- validity: 0`
[M86 p. 168], `gnoths: (gnoth1 gnoth2 gnoth3)` [M86 p. 168], the slip-check as the
platoplasm pane prints it [M86 pp. 103-105, 176], the PROGRAM [M86 p. 174], `doing task
Bond-assessor on (S-group 8.0)`, `casts: ((S-group 1 2) (S-group 1 1))` and `new hypoth
candidate (S-group 1 2)` [M86 p. 174], the failure of the effort [M86 p. 126] and the
blast [M86 p. 92]. Beside the storyboard the freeze-dried list, "kept on a list of old
hypotheses, along with the number of terms of the sequence it explained" [M86 p. 91],
newest first, and a plot of the threshold against the clock within the episode
[M86 pp. 105, 124]. The storyboard is a `LineList` (Numbo GUI, `log_view.py`'s `LogList`,
through `taskrack_view.py`; G-028): a click on a card seeks to its event when no run is
live, a click on an episode's title folds or unfolds it, and hovering a card that names
gnoths and gloms selects them in the shared selection so that the socratoplasm and the
cytoplasm show them (G-025, G-041). The pane raises itself at the run's first refutation
(PANES.md P7; G-041).
"""

from PySide6.QtCore import QPointF, QRect, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QFontMetrics, QPainter, QPen
from PySide6.QtWidgets import (
    QHBoxLayout, QLabel, QSplitter, QToolButton, QVBoxLayout, QWidget)

from seekwhence.gui import paint
from seekwhence.gui.models import reformulation as rf
from seekwhence.gui.taskrack_view import LineList

__all__ = ["StoryList", "ThresholdPlot", "ReformulationView"]

PAGES = "[M86 pp. 87-126, 168, 174-176]"
BACKGROUND = "#fbfbf8"
TEXT, MUTED, GREY = "#1a1a1a", "#6b6b6b", "#9a9a9a"
EPISODE_TINT, EPISODE_COLOR = "#e4e9f7", "#2f5597"      # the hypothesis's blue
HEAD_TINT, HEAD_COLOR = "#f1f1ee", "#4a4a4a"
HOVER_TINT = "#fff3c4"
GOOD, BAD = "#2e7d32", "#c62828"
PLOT_LINE, PLOT_FILL, PLOT_AXIS = "#2f5597", "#eef3ff", "#b0b0b0"
MARGIN = 8
PAD = 4
INDENT = 14
PLOT_H = 64


class StoryList(LineList):
    """The storyboard's lines (`pane_lines`), painted straight from the model (Numbo GUI,
    `log_view.py`'s `LogList`; G-028, G-041): an episode's title bold on a blue band with
    a fold mark, a card's heading on a grey band, the pictures and the forms in
    monospace, notes grey, good lines green and bad ones red; the hovered card's lines
    tinted. Hovering a line that names gnoths or gloms emits `names_hovered(names)`,
    leaving it emits an empty tuple."""

    names_hovered = Signal(object)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.lines = ()
        self.hovered = None             # (episode, card) under the mouse
        self.mono = QFont("monospace")
        self.mono.setStyleHint(QFont.StyleHint.Monospace)
        self.mono.setPointSizeF(9)
        self.mono_metrics = QFontMetrics(self.mono)
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

    def line(self, row: int) -> rf.RLine:
        """The row's line (G-041)."""
        return self.lines[row]

    def mouseMoveEvent(self, event):
        """Hovering a card emits the names it shows, once per card; leaving a card emits
        an empty tuple (G-041)."""
        row = self.row_at(event.position().y())
        line = self.line(row) if row is not None else None
        key = (line.episode, line.card) if line is not None and line.card is not None else None
        if key != self.hovered:
            self.hovered = key
            self.names_hovered.emit(tuple(line.names) if key is not None else ())
            self.viewport().update()

    def leaveEvent(self, event):
        """Leaving the list clears the hover (G-041)."""
        if self.hovered is not None:
            self.hovered = None
            self.names_hovered.emit(())
            self.viewport().update()
        super().leaveEvent(event)

    def _paint_line(self, p, line, box):
        h = box.height()
        kind = line.kind
        if kind == "episode":
            p.fillRect(box, QColor(EPISODE_TINT))
        elif kind == "head":
            p.fillRect(box, QColor(HEAD_TINT))
        elif self.hovered is not None and (line.episode, line.card) == self.hovered:
            p.fillRect(box, QColor(HOVER_TINT))
        color = QColor(GREY if line.grey else EPISODE_COLOR if kind == "episode"
                       else HEAD_COLOR if kind == "head" else MUTED if kind == "note"
                       else GOOD if kind == "ok" else BAD if kind == "bad" else TEXT)
        mono = kind in ("pic", "series")
        p.setFont(self.mono if mono else self.bold if kind in ("episode", "head")
                  else self.font_)
        p.setPen(color)
        x = PAD + (0 if kind == "episode" else INDENT if kind == "head" else 2 * INDENT)
        text = line.text
        if kind == "episode":
            text = ("▸ " if line.collapsed else "▾ ") + text
        room = max(10, box.width() - x - PAD)
        metrics = self.mono_metrics if mono else self.metrics
        p.drawText(QRect(x, box.y(), room, h),
                   Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                   metrics.elidedText(text, Qt.TextElideMode.ElideRight, room))


class ThresholdPlot(QWidget):
    """The threshold against the clock within the episode, as a step line: up by 1 at
    each Reformulator, down by 1 at each Gnoth-operator that acts [M86 pp. 105, 124]
    (DECISION D-046; G-040). `points` are `(clock, value)` pairs, `end` the last clock
    read; `paints` counts the paint calls (Numbo GUI, `tree_view.py`'s `paints`)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.points = ()
        self.end = None
        self.paints = 0
        self.setMinimumHeight(PLOT_H)
        self.setToolTip("the threshold bond strength against the clock within the "
                        "episode [M86 pp. 105, 124]")

    def sizeHint(self):
        """A short strip (G-040)."""
        return QSize(160, PLOT_H)

    def set_points(self, points, end) -> None:
        """New points; the widget is marked dirty (G-040)."""
        points, end = tuple(points), end
        if points != self.points or end != self.end:
            self.points, self.end = points, end
            self.update()

    def paintEvent(self, event):
        """The step line over a faint axis, the values at its ends (G-040)."""
        self.paints += 1
        p = QPainter(self)
        p.fillRect(self.rect(), QColor(BACKGROUND))
        w, h = self.width(), self.height()
        p.setPen(QPen(QColor(PLOT_AXIS), 1))
        p.drawLine(MARGIN, h - MARGIN, w - MARGIN, h - MARGIN)
        if not self.points:
            p.setPen(QColor(GREY))
            p.drawText(self.rect(), Qt.AlignmentFlag.AlignCenter, "no threshold")
            p.end()
            return
        clocks = [c for c, _ in self.points] + ([self.end] if self.end is not None else [])
        values = [v for _, v in self.points]
        c0, c1 = min(clocks), max(clocks)
        v0, v1 = max(0.0, min(values) - 1.0), max(values) + 1.0
        span_c, span_v = max(1, c1 - c0), max(1e-9, v1 - v0)
        top, bottom = MARGIN, h - MARGIN

        def at(clock, value):
            return QPointF(MARGIN + (clock - c0) / span_c * (w - 2 * MARGIN),
                           bottom - (value - v0) / span_v * (bottom - top))

        p.setRenderHint(QPainter.RenderHint.Antialiasing)
        p.setPen(QPen(QColor(PLOT_LINE), 2))
        last = None
        for clock, value in self.points:
            point = at(clock, value)
            if last is not None:
                p.drawLine(last, QPointF(point.x(), last.y()))
                p.drawLine(QPointF(point.x(), last.y()), point)
            last = point
        if last is not None and self.end is not None:
            p.drawLine(last, QPointF(at(self.end, 0).x(), last.y()))
        p.setPen(QColor(MUTED))
        p.setFont(QFont(self.font().family(), 7))
        p.drawText(QRect(MARGIN, 0, w - 2 * MARGIN, MARGIN + 6),
                   Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignTop,
                   f"threshold {values[-1]:g}  (clock {c0}–{c1})")
        p.end()


class ReformulationView(QWidget):
    """The pane (G-039, G-040, G-041): a title with the episode count, the reigning class
    and the threshold; the freeze-dried list and the threshold plot side by side; the
    "In series" toggle; the storyboard. Signals: `card_clicked(index)` after a click on a
    card (the window seeks when no run is live), `names_hovered(names)` while a card
    naming gnoths and gloms is hovered (the window selects them)."""

    pane_id = "P7"
    title = "Reformulation"
    pages = PAGES
    card_clicked = Signal(int)
    names_hovered = Signal(object)

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.history = history
        self.model = rf.ReformulationModel()
        self.position = -1
        self.events = 0
        self.head_label = QLabel("no refutation yet")
        self.head_label.setStyleSheet("font-weight: bold;")
        self.head_label.setToolTip("stage two: from a refutation to a candidate, a failure "
                                   "or the blast [M86 pp. 87-126]")
        self.state_label = QLabel("")
        self.state_label.setStyleSheet(f"color: {MUTED};")
        self.state_label.setToolTip("the reigning class and the threshold [M86 pp. 99, 105]")
        self.freeze_label = QLabel("freeze-dried: none")
        self.freeze_label.setToolTip("the old hypotheses with the terms each explained, "
                                     "newest first [M86 p. 91]")
        self.freeze_label.setAlignment(Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft)
        self.freeze_label.setWordWrap(True)
        self.freeze_label.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
        self.plot = ThresholdPlot()
        self.series_button = QToolButton()
        self.series_button.setText("In series")
        self.series_button.setCheckable(True)
        self.series_button.setChecked(False)
        self.series_button.setToolTip("Stack the episode's parses as p. 120 does, "
                                      "\"operations in series\" [M86 p. 120]; off by default")
        self.series_button.toggled.connect(self.set_show_series)
        self.list = StoryList()
        self.list.line_clicked.connect(self._line_clicked)
        self.list.names_hovered.connect(self.names_hovered)
        head = QHBoxLayout()
        head.setContentsMargins(MARGIN, 2, MARGIN, 0)
        head.addWidget(self.head_label, 1)
        head.addWidget(self.state_label)
        head.addWidget(self.series_button)
        side = QHBoxLayout()
        side.setContentsMargins(MARGIN, 0, MARGIN, 0)
        side.addWidget(self.freeze_label, 1)
        side.addWidget(self.plot, 1)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)
        layout.addLayout(head)
        layout.addLayout(side)
        layout.addWidget(self.list, 1)
        self.redraw()

    def sizeHint(self):
        """Room for the lists and the plot (G-011)."""
        return QSize(420, 420)

    @property
    def paints(self) -> int:
        """The storyboard's paint count (G-011)."""
        return self.list.paints

    def lines(self) -> tuple:
        """The storyboard's lines as last drawn (G-041)."""
        return self.list.lines

    def episodes(self) -> list:
        """The model's episodes (G-039)."""
        return self.model.episodes

    def freeze_dried_text(self) -> str:
        """The freeze-dried list as shown (G-040)."""
        return self.freeze_label.text()

    # -- the view protocol (G-011, G-016) ----------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-016)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model with the event's index and take the snapshot in force at
        `position` (G-039, G-040); a `start` begins a new run (G-011)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.model.on_event(event, self.position if self.position >= 0 else None)
        self.model.update(self.history.state_at(self.position))

    def redraw(self, now=False) -> None:
        """Rebuild the labels, the plot and the lines and mark the widgets dirty; with
        `now`, paint at once (Numbo GUI, `paint.py`; G-041)."""
        model = self.model
        n = len(model.episodes)
        self._set_text(self.head_label, f"{n} episode{'s' if n != 1 else ''}"
                       if n else "no refutation yet")
        state = ""
        if model.reigning is not None:
            state = f"reigning {model.reigning}"
            if model.threshold is not None:
                state += f"  threshold {model.threshold}"
        self._set_text(self.state_label, state)
        old = rf.freeze_dried_lines(model)
        self._set_text(self.freeze_label,
                       "freeze-dried:\n" + "\n".join(old) if old else "freeze-dried: none")
        episode = model.episode
        self.plot.set_points(rf.threshold_points(episode),
                             episode.clock_end if episode is not None else None)
        self.list.set_lines(rf.pane_lines(model))
        if now:
            paint.paint_all()

    @staticmethod
    def _set_text(label, text):
        if label.text() != text:
            label.setText(text)

    # -- interactions (PANES.md P7) ---------------------------------------------------------

    def set_show_series(self, on: bool) -> None:
        """Show or hide the operations in series (off by default; G-041)."""
        self.model.show_series = bool(on)
        if self.series_button.isChecked() != bool(on):
            self.series_button.setChecked(bool(on))
        self.redraw(now=True)

    def toggle_episode(self, number: int) -> None:
        """Fold or unfold an episode and paint now (G-041)."""
        self.model.toggle_fold(number)
        self.redraw(now=True)

    def _line_clicked(self, line):
        if line.kind == "episode":
            self.toggle_episode(line.episode)
        elif line.index is not None:
            self.card_clicked.emit(line.index)

    def render_png(self, path):
        """Grab the pane to a PNG at `path` (Numbo GUI, `coderack_view.py`)."""
        image = self.grab()
        if not image.save(path):
            raise OSError(f"could not write {path}")
        return image

"""The stats pane (P12): the run's numbers, a view of `models/run_stats.py`'s `RunStats`
(PANES.md P12; Numbo GUI, `stats_view.py`; G-016).

It shows the state, the run (preset and seed), the configuration (default or edited,
ANSWERS.md 5), the events, the clock (tasks run [M86 p. 41]), the task last chosen, the
terms entered [M86 p. 164], the tasks waiting on the rack, the reigning class
[M86 p. 99], the threshold [M86 p. 105], the hypothesis's form and validity [M86 p. 81],
the outcome and, at the end, `Session.summary()`, the line the CLI prints (DECISION
D-069). The Numbo GUI's stats panel is a form of QLabels; this one paints its rows
itself, so that each event costs exactly one paint (G-011, G-016). The snapshot in force
is read at each event through `set_position` (G-016).
"""

from PySide6.QtCore import QRect, QSize, Qt
from PySide6.QtGui import QColor, QPainter
from PySide6.QtWidgets import QWidget

from seekwhence.gui import paint
from seekwhence.gui.models.run_stats import FIELDS, RunStats

__all__ = ["StatsView"]

ROW = 18
LABEL_WIDTH = 120
MARGIN = 8
INK = QColor(40, 40, 40)
DIM = QColor(110, 110, 110)
EDITED = QColor(176, 0, 32)


class StatsView(QWidget):
    """The stats dock's view (see the module docstring; Numbo GUI, `stats_view.py`;
    G-016). `history` is the window's `EventLog`, read at `position`."""

    pane_id = "P12"
    title = "Stats"
    pages = "Numbo GUI, stats_view.py"

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.history = history
        self.model = RunStats()
        self.state = "idle"
        self.position = -1
        self.events = 0
        self.paints = 0
        self._texts = self.model.texts(self.state)
        self.setMinimumSize(120, 4 * ROW + 2 * MARGIN)      # a short dock clips rows (G-058)
        self.setAutoFillBackground(True)

    def sizeHint(self):
        """Enough rows for every field (G-011)."""
        return QSize(300, ROW * (len(FIELDS) + 1) + 2 * MARGIN)

    def text(self, field: str) -> str:
        """The text of a field as last drawn (Numbo GUI, `stats_view.py`)."""
        return self._texts[field]

    def texts(self) -> dict:
        """Every field's text as last drawn (Numbo GUI, `stats_view.py`)."""
        return dict(self._texts)

    # ---- the view protocol (G-011, G-016) -------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (the window calls it before
        `on_event`; G-016)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model and read the snapshot in force at `position` (G-016)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.model.on_event(event)
        self.model.update(self.history.state_at(self.position))

    def set_state(self, state: str) -> None:
        """The run controller's state (Numbo GUI, `stats_view.py`)."""
        self.state = state
        self.redraw(now=True)

    def set_title(self, title: str) -> None:
        """The run's title, with the seed in the Run row (G-016)."""
        self.model.set_title(title)
        self.redraw(now=True)

    def finish(self, outcome) -> None:
        """The run controller's `RunOutcome`: the summary line (Numbo GUI,
        `stats_view.py`)."""
        self.model.finish(outcome)
        self.redraw(now=True)

    def redraw(self, now=False) -> None:
        """Take the model's texts and mark the view dirty; with `now`, paint at once
        (Numbo GUI, `paint.py`)."""
        self._texts = self.model.texts(self.state)
        self.update()
        if now:
            paint.paint_all()

    def paintEvent(self, event):
        """Paint one row per field; `paints` counts the calls (G-011, G-016)."""
        self.paints += 1
        painter = QPainter(self)
        y = MARGIN
        width = self.width() - 2 * MARGIN
        flags = Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft
        for field, title in FIELDS:
            painter.setPen(DIM)
            painter.drawText(QRect(MARGIN, y, LABEL_WIDTH, ROW), flags, title)
            text = self._texts.get(field, "")
            painter.setPen(EDITED if field == "config" and text.startswith("edited") else INK)
            rect = QRect(MARGIN + LABEL_WIDTH, y, max(10, width - LABEL_WIDTH), ROW)
            painter.drawText(rect, flags, painter.fontMetrics().elidedText(
                text, Qt.TextElideMode.ElideRight, rect.width()))
            y += ROW
        painter.end()

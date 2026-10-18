"""P12 Stats: the run's numbers, a view of `models/run_stats.py`'s `RunStats` (PANES.md
P12; Numbo GUI, `stats_view.py`, through the Seek-Whence GUI's `stats_view.py`; G-045).

It shows the state, the source of the events, the run (problem, seed and profile, the
Run line that agrees with the window's title and the controls, PANES.md P9), the
configuration (the profile and the fields changed from `standard`, ANSWERS.md 3), the
events, the codelets run, the draws (D-064), the codelet last chosen, T and Q [F95
p. 91], the Worldview's size and rating (D-040), the structures built and removed, the
competitions, the changes and the fizzles, and at the end the outcome, `Result`'s
fields and French's report line [F95 p. 145]; and the invariants (D-077) the tap checked
beside the snapshot in force, `none violated (as of codelet N)` or the violations in red,
one line each, an engine finding never hidden. The Numbo GUI's stats panel is a form of
QLabels; this one paints its rows on the viewport of a scroll area (the rack list's way,
G-038), so that each event costs exactly one paint and a short dock scrolls (G-011,
G-014). The snapshot in force is read at each event through `set_position` (G-017).
"""

from PySide6.QtCore import QRectF, QSize, Qt
from PySide6.QtGui import QColor, QFont, QFontMetricsF, QImage, QPainter, QPen
from PySide6.QtWidgets import QAbstractScrollArea

from tabletop import events as ev
from tabletop.gui import paint
from tabletop.gui.models.run_stats import FIELDS, RunStats

__all__ = ["BACKGROUND", "FAULT", "INK", "MIN_SIZE", "StatsView"]

# The smallest the pane may be (G-014): a few rows; the rest scroll.
MIN_SIZE = (120, 60)
LABEL_WIDTH = 86.0
PAD = 4.0
BACKGROUND = QColor("#fbfbf8")
INK, DIM = QColor("#1a1a1a"), QColor("#6b6b6b")
FAULT = QColor("#b00020")           # an invariant violated (PANES.md P12)
EDITED = QColor("#a63d00")          # a configuration changed from its profile (ANSWERS.md 3)


class StatsView(QAbstractScrollArea):
    """The stats pane (G-045; G-011's view protocol): `attach(history, selection)` takes
    the window's history for the snapshot in force and the invariants beside it;
    `set_position`, `on_event`, `redraw(now=False)`, `paints`, `events`, `pane_id`,
    `title`, `pages`; `set_state`, `set_source` and `finish(outcome)` from the window
    (Numbo GUI, `stats_view.py`)."""

    def __init__(self, title="Stats", pane_id="P12", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = RunStats()
        self.history = None
        self.position = -1
        self.state = "idle"
        self.source = "live run"
        self.outcome = None
        self.paints = 0
        self._texts = self.model.texts(self.state, self.source)
        self._rows = ()
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(8.5)
        # Greyscale antialiasing, so that a row's colour can be read off its pixels (G-038).
        self.font_.setStyleStrategy(QFont.StyleStrategy.NoSubpixelAntialias)
        self.metrics = QFontMetricsF(self.font_)
        self.line = self.metrics.height() + 3
        self.setMinimumSize(*MIN_SIZE)
        self.setVerticalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAsNeeded)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self._refresh()

    def sizeHint(self):
        """Enough rows for every field (G-011)."""
        return QSize(270, int(self.line * (len(FIELDS) + 1) + 2 * PAD))

    @property
    def events(self) -> int:
        """How many events of the run the view has seen (G-011)."""
        return self.model.events

    def attach(self, history, selection=None) -> None:
        """The window's history, read at `position` for the snapshot in force and the
        invariants beside it (G-045); the selection is not used."""
        self.history = history

    # -- the texts (Numbo GUI, `stats_view.py`) -------------------------------------------------

    def text(self, field: str) -> str:
        """The text of a field as last drawn (Numbo GUI, `stats_view.py`)."""
        return self._texts[field]

    def texts(self) -> dict:
        """Every field's text as last drawn (Numbo GUI, `stats_view.py`)."""
        return dict(self._texts)

    def rows(self) -> tuple:
        """The rows drawn, (title, text, colour role) triples: one per field, then one
        per invariant violated, in red (G-045)."""
        return self._rows

    def outcome_text(self) -> str:
        """The outcome: the events' (`answered`, `capped`), else the controller's
        outcome for a stopped run or an error, else nothing (G-045)."""
        text = self.model.outcome_text()
        if text or self.outcome is None:
            return text
        if self.outcome.error is not None:
            return f"error: {self.outcome.error!r}"
        if self.outcome.stopped:
            return f"stopped after {self.outcome.events} events"
        return ""

    def _refresh(self) -> None:
        texts = self.model.texts(self.state, self.source)
        texts["outcome"] = self.outcome_text()
        self._texts = texts
        rows = []
        for field, title in FIELDS:
            role = "ink"
            if field == "config" and ":" in texts[field]:
                role = "edited"
            elif field == "invariants" and self.model.faulty:
                role = "fault"
            elif field == "outcome" and texts[field].startswith(("error", "stopped")):
                role = "edited"
            rows.append((title, texts[field], role))
        if self.model.faulty:
            rows += [("", sentence, "fault") for sentence in self.model.faults]
        self._rows = tuple(rows)
        content = self.line * len(rows) + 2 * PAD
        self.verticalScrollBar().setRange(0, max(0, int(content - self.viewport().height())))
        self.verticalScrollBar().setPageStep(max(1, self.viewport().height()))
        self.verticalScrollBar().setSingleStep(int(self.line))

    def resizeEvent(self, event):
        """The scroll range follows the viewport's height (G-045)."""
        super().resizeEvent(event)
        self._refresh()

    # -- the view protocol (G-011, G-017) ---------------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (the window calls it before
        `on_event`; G-017)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model and read the snapshot in force and the invariants beside it
        (G-045)."""
        self.model.on_event(event)
        if isinstance(event, ev.RunStarted) and self.state in ("running", "paused"):
            self.outcome = None
        self._read_state()

    def _read_state(self) -> None:
        if self.history is None:
            return
        self.model.update(self.history.state_at(self.position),
                          self.history.invariants_at(self.position))

    def set_state(self, state: str) -> None:
        """The run controller's state; a live state forgets the last outcome (Numbo GUI,
        `stats_view.py`; G-045)."""
        self.state = state
        if state in ("running", "paused"):
            self.outcome = None
        self.redraw(now=True)

    def set_source(self, text: str) -> None:
        """Where the events come from: `live run`, or what a replay says (Numbo GUI,
        `stats_view.py`)."""
        self.source = text
        self.redraw(now=True)

    def finish(self, outcome) -> None:
        """The run controller's `RunOutcome`: a stopped run or an error, which no event
        says (Numbo GUI, `stats_view.py`; G-045)."""
        self.outcome = outcome
        self.model.finish(outcome)
        self.redraw(now=True)

    def redraw(self, now=False) -> None:
        """Take the model's texts, re-read the state in force and mark the viewport
        dirty; with `now`, paint at once (Numbo GUI, `paint.py`)."""
        self._read_state()
        self._refresh()
        self.viewport().update()
        if now:
            paint.paint_all()

    def render_png(self, path) -> bytes:
        """A fresh render of the viewport to a PNG file (RGB), for the tests (G-045)."""
        self.viewport().grab().toImage().convertToFormat(
            QImage.Format.Format_RGB32).save(str(path), "PNG")
        return path.read_bytes()

    def row_top(self, k: int) -> float:
        """The viewport y of the k-th row's top (G-045)."""
        return PAD + k * self.line - self.verticalScrollBar().value()

    def paintEvent(self, event):
        """Paint one row per field, the invariants' sentences in red (G-045); `paints`
        counts the calls (G-011)."""
        self.paints += 1
        painter = QPainter(self.viewport())
        painter.fillRect(self.viewport().rect(), BACKGROUND)
        painter.setFont(self.font_)
        width = self.viewport().width()
        bottom = self.viewport().height()
        flags = Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft
        for k, (title, text, role) in enumerate(self._rows):
            y = self.row_top(k)
            if y + self.line < 0 or y > bottom:
                continue
            painter.setPen(QPen(DIM, 1))
            painter.drawText(QRectF(PAD, y, LABEL_WIDTH, self.line), flags, title)
            painter.setPen(QPen(FAULT if role == "fault" else EDITED if role == "edited"
                                else INK, 1))
            room = max(10.0, width - LABEL_WIDTH - 2 * PAD)
            painter.drawText(QRectF(PAD + LABEL_WIDTH, y, room, self.line), flags,
                             self.metrics.elidedText(text, Qt.TextElideMode.ElideRight, room))
        painter.end()

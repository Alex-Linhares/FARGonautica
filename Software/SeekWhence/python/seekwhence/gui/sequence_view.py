"""The sequence pane (P1): the terms with their fate, the hypothesis's predicted block
as ghost cells, the input box, the guess panel with its OK and No and the answer badges
(PANES.md P1; G-014, G-015; Numbo GUI's view protocol, G-011).

What it draws, and why (PANES.md P1):
  - The terms "presented one term at a time" [MHN p. 3] as large numbered cells, the
    position above and the value inside; the cell's colour is its fate under the
    hypothesis: green when the Hfilter found it predicted (`new term being
    hypothesis-filtered through (Cycle 3 2 (3 7))` [M86 p. 166]), red when it refuted
    the hypothesis (`set-out -- validity: 0` [M86 p. 168]), grey when not filtered.
  - Under them, the block a fresh hypothesis gives, grouped into its hits with a bracket
    under each: `3 7 3 7` [M86 p. 166], `(1 2)(1 2 3)(1 2 3 4)` [M86 p. 179]; the cells
    beyond the last term entered are dashed ghosts, the predicted continuation.
  - The input row: `please enter a term:` [M86 p. 164] with a box that takes one term or
    several (`3 7 3 7`, queued one per prompt; `models/term_feed.py`), enabled while an
    interactive run waits at a prompt.
  - The guess panel: `I have a guess!`, the form and the block, OK and No
    [M86 pp. 166, 175] (`GuessPanel`, G-008), and under the cells a badge per answered
    guess, `3 7 3 7 ok` [M86 p. 166], `(1 2)(1 2)(1 2) nope` [M86 p. 175].
  - The show commands (`show`, `show-hypothesis`, `show-seq`, `show-parse`
    [M86 pp. 166-179]) are the console pane's (`console_view.py`, item 04; G-019): a
    line typed in the box here is a console line too, so a command works as well.

The strip (`SequenceStrip`) paints itself in one `paintEvent` per event (counted in
`paints`, the probe of G-011); the widgets around it are ordinary Qt widgets. Click a
cell and `cell_clicked(n)` is emitted (the cytoplasm and socratoplasm panes will
highlight the term's glint and gnoth); hover a cell for its fate, or a ghost for
`predicted by (Cycle 3 2 (3 7)), hit 2`.
"""

from PySide6.QtCore import QEvent, QRect, QSize, Qt, Signal
from PySide6.QtGui import QColor, QPainter, QPen
from PySide6.QtWidgets import (QHBoxLayout, QLabel, QLineEdit, QPushButton, QToolTip,
                               QVBoxLayout, QWidget)

from seekwhence.gui import paint
from seekwhence.gui.models.sequence import SequenceModel
from seekwhence.session import PROMPT

__all__ = ["FATE_COLORS", "GuessPanel", "SequenceStrip", "SequenceView"]

# The texts of the guess exchange, as printed [M86 p. 166].
GUESS_LINE = "I have a guess!"
IDLE_LINE = "(no guess waiting)"

# The fates' colours (PANES.md P1: green predicted, red refuting, grey unfiltered).
FATE_COLORS = {"predicted": QColor(205, 232, 207), "refuting": QColor(242, 194, 194),
               "unfiltered": QColor(228, 228, 228)}
GHOST_FILL = QColor(248, 248, 248)
GHOST_DIFFERS = QColor(250, 226, 226)
BADGE_OK = QColor(205, 232, 207)
BADGE_NO = QColor(242, 194, 194)
BADGE_WAITING = QColor(255, 243, 205)
INK = QColor(40, 40, 40)
DIM = QColor(120, 120, 120)

MARGIN = 10
MAX_CELL = 44
MIN_CELL = 16
BADGE_HEIGHT = 18


class SequenceStrip(QWidget):
    """The painted part of the pane: the term cells, the ghost block and the badges
    (PANES.md P1; G-014). `paints` counts the paint events (G-011)."""

    cell_clicked = Signal(int)

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.paints = 0
        self.setMinimumHeight(170)
        self.setAutoFillBackground(True)
        self.setMouseTracking(True)

    def sizeHint(self):
        """Wide, and tall enough for the two rows of cells and the badges at the largest
        cell size (G-011, G-014)."""
        return QSize(600, 2 * MAX_CELL + 2 * MARGIN + 14 + 14 + 16 + BADGE_HEIGHT + 4)

    # ---- geometry --------------------------------------------------------------------------

    def cell_size(self) -> int:
        """The side of a cell, so that every position fits the width and the two rows
        and the badges fit the height (G-014)."""
        model = self.model
        positions = max(len(model.terms), max((c.position for c in model.ghosts()), default=0))
        room = self.width() - 2 * MARGIN
        rows = (self.height() - (2 * MARGIN + 14 + 14 + 16 + BADGE_HEIGHT + 4)) // 2
        return max(MIN_CELL, min(MAX_CELL, room // max(1, positions + 1), rows))

    def term_rects(self) -> list:
        """`(cell, rect)` for each term cell, left to right (G-014)."""
        size = self.cell_size()
        top = MARGIN + 14
        return [(cell, QRect(MARGIN + (cell.n - 1) * (size + 4), top, size, size))
                for cell in self.model.terms]

    def ghost_rects(self) -> list:
        """`(ghost, rect)` for each ghost cell, aligned under its position (G-014)."""
        size = self.cell_size()
        top = MARGIN + 14 + size + 14
        return [(ghost, QRect(MARGIN + (ghost.position - 1) * (size + 4), top, size, size))
                for ghost in self.model.ghosts()]

    def badge_top(self) -> int:
        """The y of the badges' first row, under the ghost block (G-014)."""
        size = self.cell_size()
        return MARGIN + 14 + size + 14 + size + 16

    # ---- painting ------------------------------------------------------------------------------

    def paintEvent(self, event):
        """Paint the cells, the ghost block with its brackets and the badges; `paints`
        counts the calls (G-011, G-014)."""
        self.paints += 1
        painter = QPainter(self)
        painter.setRenderHint(QPainter.RenderHint.Antialiasing)
        model = self.model
        size = self.cell_size()
        font = painter.font()
        small = painter.font()
        small.setPointSizeF(max(6.0, font.pointSizeF() - 2))
        big = painter.font()
        big.setPointSizeF(max(7.0, min(16.0, size / 2.6)))
        big.setBold(True)
        center = Qt.AlignmentFlag.AlignCenter
        if not model.terms and model.form is None:
            painter.setPen(DIM)
            painter.drawText(self.rect().adjusted(MARGIN, MARGIN, -MARGIN, -MARGIN),
                             Qt.AlignmentFlag.AlignTop | Qt.AlignmentFlag.AlignLeft,
                             "no terms yet: type a term below, or play a preset")
            painter.end()
            return
        self._paint_cells(painter, small, big, center)
        self._paint_ghosts(painter, small, big, center)
        self._paint_badges(painter, small, center)
        painter.end()

    def _paint_cells(self, painter, small, big, center) -> None:
        """The term cells, the position above each, the fill by fate [M86 p. 164]
        (G-014)."""
        for cell, rect in self.term_rects():
            painter.setFont(small)
            painter.setPen(DIM)
            painter.drawText(QRect(rect.x(), rect.y() - 14, rect.width(), 12), center, str(cell.n))
            painter.setPen(QPen(INK, 1))
            painter.setBrush(FATE_COLORS.get(cell.fate, FATE_COLORS["unfiltered"]))
            painter.drawRoundedRect(rect, 4, 4)
            painter.setFont(big)
            painter.drawText(rect, center, str(cell.value))

    def _paint_ghosts(self, painter, small, big, center) -> None:
        """The ghost block under its positions: the form's line above it, a bracket
        under each hit, dashed cells beyond the terms [M86 pp. 166, 179] (DECISION
        D-033; G-014)."""
        ghosts = self.ghost_rects()
        if not ghosts:
            return
        model = self.model
        painter.setFont(small)
        painter.setPen(DIM)
        label_y = ghosts[0][1].y() - 13
        painter.drawText(QRect(MARGIN, label_y, self.width() - 2 * MARGIN, 12),
                         Qt.AlignmentFlag.AlignLeft | Qt.AlignmentFlag.AlignVCenter,
                         f"{'refuted ' if model.refuted else ''}{model.form}: {model.ghost_text()}")
        for ghost, rect in ghosts:
            pen = QPen(DIM if ghost.beyond else INK, 1)
            if ghost.beyond:
                pen.setStyle(Qt.PenStyle.DashLine)
            painter.setPen(pen)
            fill = GHOST_FILL if ghost.beyond or ghost.agrees else GHOST_DIFFERS
            painter.setBrush(fill)
            painter.drawRoundedRect(rect, 4, 4)
            painter.setFont(big)
            painter.setPen(DIM if ghost.beyond else INK)
            painter.drawText(rect, center, str(ghost.value))
        painter.setPen(QPen(INK, 1))
        for block in self._blocks(ghosts):
            first, last = block[0][1], block[-1][1]
            y = last.bottom() + 4
            painter.drawLine(first.left(), y, last.right(), y)
            painter.drawLine(first.left(), y - 3, first.left(), y)
            painter.drawLine(last.right(), y - 3, last.right(), y)

    def _paint_badges(self, painter, small, center) -> None:
        """The badges: each guess and its answer, wrapped to the width [M86 pp. 166, 175]
        (G-014)."""
        painter.setFont(small)
        x = MARGIN
        y = self.badge_top()
        for badge in self._badges():
            text = f"{badge.form}  {badge.block}  {badge.answer if badge.answer is not None else '?'}"
            width = painter.fontMetrics().horizontalAdvance(text) + 12
            if x + width > self.width() - MARGIN and x > MARGIN:
                x, y = MARGIN, y + BADGE_HEIGHT + 4
            rect = QRect(x, y, width, BADGE_HEIGHT)
            fill = {"ok": BADGE_OK, "no": BADGE_NO}.get(badge.verdict, BADGE_WAITING)
            painter.setPen(QPen(INK, 1))
            painter.setBrush(fill)
            painter.drawRoundedRect(rect, 8, 8)
            painter.drawText(rect, center, text)
            x += width + 6

    def _blocks(self, ghosts):
        blocks: list = []
        for ghost, rect in ghosts:
            if not blocks or blocks[-1][0][0].hit != ghost.hit:
                blocks.append([(ghost, rect)])
            else:
                blocks[-1].append((ghost, rect))
        return blocks

    def _badges(self):
        badges = list(self.model.badges)
        if self.model.pending is not None:
            badges.append(self.model.pending)
        return badges

    # ---- the mouse ---------------------------------------------------------------------------

    def cell_at(self, point):
        """The term cell under the point, or None (G-014)."""
        for cell, rect in self.term_rects():
            if rect.contains(point):
                return cell
        return None

    def ghost_at(self, point):
        """The ghost cell under the point, or None (G-014)."""
        for ghost, rect in self.ghost_rects():
            if rect.contains(point):
                return ghost
        return None

    def tooltip_at(self, point) -> str:
        """What hovering the point says: the cell's fate or the ghost's prediction
        (PANES.md P1; G-014)."""
        cell = self.cell_at(point)
        if cell is not None:
            return self.model.cell_tooltip(cell)
        ghost = self.ghost_at(point)
        if ghost is not None:
            return self.model.ghost_tooltip(ghost)
        return ""

    def event(self, event):
        """Tooltips for the cells (Numbo GUI's canvases; G-014)."""
        if event.type() == QEvent.Type.ToolTip:
            text = self.tooltip_at(event.pos())
            if text:
                QToolTip.showText(event.globalPos(), text, self)
            else:
                QToolTip.hideText()
            return True
        return super().event(event)

    def mousePressEvent(self, event):
        """A click on a term cell emits `cell_clicked(n)` (PANES.md P1; G-014)."""
        cell = self.cell_at(event.position().toPoint())
        if cell is not None:
            self.cell_clicked.emit(cell.n)
        super().mousePressEvent(event)


class GuessPanel(QWidget):
    """The guess exchange: the form and the block the program predicts, and OK / No,
    enabled while a guess waits [M86 pp. 166, 175] (G-008; PANES.md P1). `answered(text)`
    is emitted with `ok` or `no` when a button is clicked."""

    answered = Signal(str)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.pending = None
        self.message = QLabel(IDLE_LINE)
        self.ok_button = QPushButton("OK")
        self.no_button = QPushButton("No")
        self.ok_button.setToolTip("enter no if wrong, ok if right [M86 p. 166]")
        self.no_button.setToolTip("enter no if wrong, ok if right [M86 p. 166]")
        self.ok_button.clicked.connect(lambda: self.answered.emit("ok"))
        self.no_button.clicked.connect(lambda: self.answered.emit("no"))
        layout = QHBoxLayout(self)
        layout.setContentsMargins(8, 2, 8, 2)
        layout.addWidget(self.message, 1)
        layout.addWidget(self.ok_button)
        layout.addWidget(self.no_button)
        self.setFixedHeight(self.sizeHint().height())
        self.clear()

    def ask(self, pending) -> None:
        """Show the guess (`GuessPending`) and enable the buttons [M86 p. 166]
        (G-008)."""
        self.pending = pending
        self.message.setText(f"{GUESS_LINE}  hypothesis: {pending.form}   {pending.block}")
        self.ok_button.setEnabled(True)
        self.no_button.setEnabled(True)

    def clear(self) -> None:
        """No guess waits: disable the buttons (G-008)."""
        self.pending = None
        self.message.setText(IDLE_LINE)
        self.ok_button.setEnabled(False)
        self.no_button.setEnabled(False)

    @property
    def waiting(self) -> bool:
        """Whether a guess is shown and waits for a click (G-008)."""
        return self.pending is not None


class SequenceView(QWidget):
    """The sequence pane: the strip over the input row and the guess panel (see the
    module docstring; PANES.md P1; G-014, G-015). Signals: `term_typed(text)` when terms
    are entered in the box, `answered(text)` from the guess panel, `cell_clicked(n)`."""

    term_typed = Signal(str)
    answered = Signal(str)
    cell_clicked = Signal(int)

    pane_id = "P1"
    title = "Sequence and input"
    pages = "[M86 pp. 7, 37, 164-166, 175-176]; [FCCA p. 52]"

    def __init__(self, parent=None):
        super().__init__(parent)
        self.model = SequenceModel()
        self.events = 0
        self.accepting = False
        self.queued = ()
        self.strip = SequenceStrip(self.model)
        self.strip.cell_clicked.connect(self.cell_clicked)
        self.prompt_label = QLabel(PROMPT.strip())
        self.prompt_label.setToolTip("the prompt, as printed [M86 p. 164]")
        self.input_edit = QLineEdit()
        self.input_edit.setPlaceholderText("a term, or several: 3 7 3 7")
        self.input_edit.setToolTip("one term per prompt; several at once are queued "
                                   "[M86 p. 37]")
        self.input_edit.returnPressed.connect(self.submit)
        self.enter_button = QPushButton("Enter")
        self.enter_button.clicked.connect(self.submit)
        self.queue_label = QLabel("")
        self.queue_label.setFixedHeight(self.queue_label.sizeHint().height())
        self.guess_panel = GuessPanel()
        self.guess_panel.answered.connect(self.answered)
        inputs = QHBoxLayout()
        inputs.setContentsMargins(8, 0, 8, 0)
        inputs.addWidget(self.prompt_label)
        inputs.addWidget(self.input_edit, 1)
        inputs.addWidget(self.enter_button)
        inputs.addWidget(self.queue_label)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)
        layout.addWidget(self.strip, 1)
        layout.addLayout(inputs)
        layout.addWidget(self.guess_panel)
        self._apply_input_state()

    @property
    def paints(self) -> int:
        """The strip's paint count (G-011)."""
        return self.strip.paints

    # ---- the view protocol (G-011) ---------------------------------------------------------

    def on_event(self, event) -> None:
        """Feed the model; a `start` event begins a new run (G-014)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Mark the strip dirty and set the input row's state; with `now`, paint at
        once (Numbo GUI, `paint.py`)."""
        self._apply_input_state()
        self.strip.update()
        if now:
            paint.paint_all()

    # ---- the input row (G-015) ---------------------------------------------------------------

    def set_accepting(self, accepting: bool) -> None:
        """Whether an interactive run is live and reads terms from the GUI (G-015)."""
        self.accepting = bool(accepting)
        self._apply_input_state()

    def set_queue(self, lines) -> None:
        """The terms typed ahead and not yet read, shown beside the box (G-015)."""
        self.queued = tuple(lines)
        self.queue_label.setText(f"queued: {' '.join(self.queued)}" if self.queued else "")

    @property
    def input_enabled(self) -> bool:
        """The box takes terms when an interactive run waits at a prompt (PANES.md P1;
        G-015)."""
        return self.accepting and self.model.waiting

    def _apply_input_state(self):
        enabled = self.input_enabled
        if self.input_edit.isEnabled() != enabled:
            self.input_edit.setEnabled(enabled)
            self.enter_button.setEnabled(enabled)
        self.prompt_label.setStyleSheet("font-weight: bold;" if enabled else "color: #777;")

    def submit(self) -> None:
        """Enter: emit the box's text as `term_typed` and clear it (G-015)."""
        text = self.input_edit.text().strip()
        if not text:
            return
        self.input_edit.clear()
        self.term_typed.emit(text)

"""The console pane (P8): the transcript exactly as the CLI prints it, a view of
`models/console.py`'s `ConsoleModel` (PANES.md P8; [M86 pp. 164-179]; Numbo GUI,
`log_view.py`; G-018, G-019, G-020).

What it shows, and why: Meredith's only display is the console transcript
[M86 pp. 164-179]: `-> (startup)`, `please enter a term: 3`, the `doing task` lines, the
happenings, the show-commands' output, the guess and `bye`. The pane shows the lines of
the model in one of two modes, Verbose (the `doing task` lines, as runs 1 and 3
[M86 p. 164]) or Terse (the happenings only, as run 2 [M86 p. 167]; DECISION D-055),
switched instantly since the model keeps both. The lines after the cursor are greyed (a
seek into the past; PANES.md P8). Under the lines, a command line takes what the CLI
takes, a term or `show`, `show-hypothesis`, `show-seq`, `show-parse` and `bye`
(DECISION D-054), and the four commands as buttons (moved here from the sequence pane,
G-017, G-019).

`ConsoleList`, the rows, is Numbo's `LogList`: a `QAbstractScrollArea` that paints only
the rows in view, straight from the model, so an event costs one paint however long the
transcript is (`paints` counts them, the probe of G-011). A click on a row emits
`activated(index)`, the index of the row's event, and the window seeks there when no run
is live (PANES.md P8); the arrow keys, Page Up/Down, Home and End move a cursor row and
Enter activates it. Find (the box above the rows; Enter or the button finds the next
match, wrapping round) highlights the row found. Follow keeps the cursor's line in view.
"""

from PySide6.QtCore import QRect, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFontDatabase, QFontMetrics, QPainter, QPen
from PySide6.QtWidgets import (QAbstractScrollArea, QCheckBox, QComboBox, QHBoxLayout, QLabel,
                               QLineEdit, QPushButton, QVBoxLayout, QWidget)

from seekwhence.gui import paint
from seekwhence.gui.models.commands import COMMANDS
from seekwhence.gui.taskrack_view import LIST_MIN
from seekwhence.gui.models.console import MODES, ConsoleModel
from seekwhence.session import PROMPT

__all__ = ["ConsoleList", "ConsoleView"]

BACKGROUND = "#ffffff"
TEXT = "#1a1a1a"
FUTURE = "#9a9a9a"      # the lines after the cursor
NOW = "#fbe3f0"         # the cursor's line
FOUND = "#fff3c4"       # the row found or chosen with the keys
CURSOR = "#e6007e"
PAD = 4


class ConsoleList(QAbstractScrollArea):
    """The rows of the transcript in `mode` (Numbo GUI, `log_view.py`'s `LogList`; G-020).
    `position`: the cursor, the event drawn now; `cursor_row`: the keyboard's or find's
    row (None); `paints`: how many times it has been painted."""

    activated = Signal(int)     # an event index

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.mode = "verbose"
        self.position = -1
        self.cursor_row = None
        self.paints = 0
        self.setMinimumSize(*LIST_MIN)       # a dock can be short (G-058)
        self.setFont(QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont))
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.viewport().setAttribute(Qt.WidgetAttribute.WA_OpaquePaintEvent)
        self._colors = {k: QColor(v) for k, v in
                        (("bg", BACKGROUND), ("text", TEXT), ("future", FUTURE), ("now", NOW),
                         ("found", FOUND))}
        self.metrics = QFontMetrics(self.font())
        self.line = self.metrics.height() + 2

    # -- rows ---------------------------------------------------------------------

    @property
    def lines(self) -> list:
        """The model's lines in the mode shown (G-018)."""
        return self.model.lines[self.mode]

    def row_count(self) -> int:
        """How many rows there are (Numbo GUI, `log_view.py`)."""
        return len(self.lines)

    def row_text(self, row: int) -> str:
        """The row's text (G-018)."""
        return self.lines[row].text

    def row_index(self, row: int) -> int:
        """The event index of the row's line (G-018)."""
        return self.lines[row].index

    def is_future(self, row: int) -> bool:
        """Whether the row's line comes after the cursor (PANES.md P8; G-018)."""
        return self.model.is_future(self.mode, row, self.position)

    def now_row(self) -> int | None:
        """The row of the cursor's last line (G-018)."""
        return self.model.cursor_row(self.mode, self.position)

    def rows_in_view(self) -> int:
        """How many whole rows the viewport holds (Numbo GUI, `log_view.py`)."""
        return max(1, self.viewport().height() // self.line)

    def first_row(self) -> int:
        """The row at the top of the viewport (Numbo GUI, `log_view.py`)."""
        return self.verticalScrollBar().value()

    def row_at(self, y) -> int | None:
        """The row at the viewport's y, or None (Numbo GUI, `log_view.py`)."""
        row = self.first_row() + int(y // self.line)
        return row if 0 <= row < self.row_count() else None

    def row_rect(self, row: int) -> QRect:
        """The row's rectangle in the viewport (Numbo GUI, `log_view.py`)."""
        return QRect(0, (row - self.first_row()) * self.line, self.viewport().width(), self.line)

    def update_rows(self) -> None:
        """The number of rows changed: the scroll bar's range (Numbo GUI,
        `log_view.py`)."""
        bar = self.verticalScrollBar()
        top = max(0, self.row_count() - self.rows_in_view())
        if bar.maximum() != top:
            bar.setRange(0, top)
        bar.setPageStep(self.rows_in_view())
        if self.cursor_row is not None and self.cursor_row >= self.row_count():
            self.cursor_row = None

    def scroll_to(self, row: int, center=False) -> None:
        """Bring the row into view: the least scrolling, or (`center`) to the middle
        (Numbo GUI, `log_view.py`)."""
        bar, first, n = self.verticalScrollBar(), self.first_row(), self.rows_in_view()
        if first <= row < first + n:
            return
        if center:
            bar.setValue(row - n // 2)
        elif row < first:
            bar.setValue(row)
        else:
            bar.setValue(row - n + 1)

    def set_position(self, position: int, follow=True) -> None:
        """The cursor moved: keep its line in view when following and mark the rows
        dirty (Numbo GUI, `log_view.py`; G-018)."""
        old, self.position = self.position, position
        self.update_rows()
        row = self.now_row()
        if follow and row is not None:
            self.scroll_to(row, center=position != old + 1)
        self.viewport().update()

    def set_mode(self, mode: str) -> None:
        """Show the other transcript: the rows change, the cursor's line stays in view
        (DECISION D-055; G-018)."""
        if mode not in MODES:
            raise ValueError(f"unknown mode {mode!r}")
        self.mode = mode
        self.cursor_row = None
        self.update_rows()
        row = self.now_row()
        if row is not None:
            self.scroll_to(row, center=True)
        self.viewport().update()

    def find(self, needle: str) -> int | None:
        """The next row after the cursor row (or the found row) whose text contains the
        needle, wrapping round; it becomes the cursor row, in view (G-019)."""
        start = 0 if self.cursor_row is None else self.cursor_row + 1
        row = self.model.find(self.mode, needle, start)
        if row is not None:
            self.cursor_row = row
            self.scroll_to(row, center=True)
            self.viewport().update()
        return row

    # -- painting -------------------------------------------------------------------

    def paintEvent(self, event):
        """Paint the rows in the dirty rectangle: the cursor's line tinted, the lines
        after it greyed, the cursor row outlined (Numbo GUI, `log_view.py`; G-011)."""
        self.paints += 1
        p = QPainter(self.viewport())
        c = self._colors
        rect = event.rect()
        p.fillRect(rect, c["bg"])
        first = self.first_row()
        top = first + max(0, rect.top()) // self.line
        bottom = first + rect.bottom() // self.line + 1
        ascent = self.metrics.ascent() + 1
        now = self.now_row()
        width = self.viewport().width()
        for row in range(top, min(bottom, self.row_count())):
            y = (row - first) * self.line
            if row == now:
                p.fillRect(0, y, width, self.line, c["now"])
            elif row == self.cursor_row:
                p.fillRect(0, y, width, self.line, c["found"])
            p.setPen(c["future"] if self.is_future(row) else c["text"])
            p.drawText(PAD, y + ascent, self.row_text(row))
            if row == self.cursor_row:
                p.setPen(QPen(QColor(CURSOR), 1, Qt.PenStyle.DotLine))
                p.drawRect(0, y, width - 1, self.line - 1)
        p.end()

    def resizeEvent(self, event):
        """The scroll range follows the height (Numbo GUI, `log_view.py`)."""
        super().resizeEvent(event)
        self.update_rows()

    # -- the mouse and the keys -------------------------------------------------------

    def activate(self, row) -> None:
        """A click on the row, or Enter on it: `activated(index)` with its event's index
        (PANES.md P8: click a line to seek; G-020)."""
        if row is not None and 0 <= row < self.row_count():
            self.cursor_row = row
            self.viewport().update()
            self.activated.emit(self.row_index(row))

    def mousePressEvent(self, event):
        """A click activates the row (G-020)."""
        self.activate(self.row_at(event.position().y()))

    def event(self, e):
        """The tooltip is the whole line (a long one is clipped) (Numbo GUI,
        `log_view.py`)."""
        if e.type() == e.Type.ToolTip:
            row = self.row_at(e.pos().y())
            self.setToolTip(self.row_text(row) if row is not None else "")
        return super().event(e)

    def keyPressEvent(self, event):
        """The arrows, Page Up/Down, Home and End move the cursor row; Enter activates it
        (Numbo GUI, `log_view.py`)."""
        n = self.row_count()
        if not n:
            return super().keyPressEvent(event)
        key = event.key()
        row = self.cursor_row
        if row is None:
            row = self.now_row() or 0
        moves = {Qt.Key.Key_Up: -1, Qt.Key.Key_Down: 1, Qt.Key.Key_PageUp: -self.rows_in_view(),
                 Qt.Key.Key_PageDown: self.rows_in_view()}
        if key in moves:
            row += moves[key]
        elif key == Qt.Key.Key_Home:
            row = 0
        elif key == Qt.Key.Key_End:
            row = n - 1
        elif key in (Qt.Key.Key_Return, Qt.Key.Key_Enter):
            self.activate(row)
            return
        else:
            return super().keyPressEvent(event)
        self.cursor_row = max(0, min(n - 1, row))
        self.scroll_to(self.cursor_row)
        self.viewport().update()


class ConsoleView(QWidget):
    """The console pane (see the module docstring; PANES.md P8; G-018, G-019, G-020).
    Signals: `line_typed(text)` for a line entered at the command line,
    `command_requested(name)` for a button, `line_activated(index)` for a click on a
    row. `history` is the window's `EventLog`, read by the model at each ask."""

    line_typed = Signal(str)
    command_requested = Signal(str)
    line_activated = Signal(int)

    pane_id = "P8"
    title = "Console"
    pages = "[M86 pp. 164-179]"

    def __init__(self, history=None, parent=None):
        super().__init__(parent)
        self.model = ConsoleModel(history)
        self.events = 0
        self.position = -1
        self.list = ConsoleList(self.model)
        self.list.activated.connect(self.line_activated)

        self.mode_box = QComboBox()
        for mode in MODES:
            self.mode_box.addItem(mode.capitalize(), mode)
        self.mode_box.setToolTip("Verbose prints the `doing task` lines as runs 1 and 3 do "
                                 "[M86 p. 164]; Terse only the happenings, as run 2 "
                                 "[M86 p. 167]")
        self.mode_box.currentIndexChanged.connect(self._mode_changed)
        self.find_edit = QLineEdit()
        self.find_edit.setPlaceholderText("find")
        self.find_edit.setToolTip("the next line containing this text (Enter: again)")
        self.find_edit.returnPressed.connect(self.find_next)
        self.find_button = QPushButton("Find")
        self.find_button.clicked.connect(self.find_next)
        self.follow_box = QCheckBox("Follow")
        self.follow_box.setToolTip("keep the cursor's line in view")
        self.follow_box.setChecked(True)
        self.count_label = QLabel()
        self.count_label.setFixedWidth(self.count_label.fontMetrics().horizontalAdvance(
            "000000 lines") + 8)
        self.count_label.setFixedHeight(self.count_label.sizeHint().height())
        self.count_label.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)

        self.prompt_label = QLabel(PROMPT.strip())
        self.prompt_label.setToolTip("the prompt, as printed [M86 p. 164]: a term, show, "
                                     "show-hypothesis, show-seq, show-parse or bye")
        self.command_edit = QLineEdit()
        self.command_edit.setPlaceholderText("a term, or show, show-hypothesis, show-seq, "
                                             "show-parse, bye")
        self.command_edit.setToolTip("what the CLI takes at its prompt [M86 pp. 164-179]; "
                                     "a command with no interactive run live prints the "
                                     "state at the cursor")
        self.command_edit.returnPressed.connect(self.submit)
        self.command_buttons = {}
        buttons = QHBoxLayout()
        buttons.setContentsMargins(0, 0, 0, 0)
        for name in COMMANDS:
            button = QPushButton(name)
            button.setToolTip(f"what `{name}` prints [M86 pp. 166-179]: at the cursor, or, "
                              f"in an interactive run, as a line typed at the prompt")
            button.clicked.connect(lambda _checked=False, n=name: self.command_requested.emit(n))
            self.command_buttons[name] = button
            buttons.addWidget(button)
        buttons.addStretch(1)

        bar = QHBoxLayout()
        bar.setContentsMargins(0, 0, 0, 0)
        bar.addWidget(self.mode_box)
        bar.addWidget(self.find_edit, 1)
        bar.addWidget(self.find_button)
        bar.addWidget(self.follow_box)
        bar.addWidget(self.count_label)
        command_row = QHBoxLayout()
        command_row.setContentsMargins(0, 0, 0, 0)
        command_row.addWidget(self.prompt_label)
        command_row.addWidget(self.command_edit, 1)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.addLayout(bar)
        layout.addWidget(self.list, 1)
        layout.addLayout(command_row)
        layout.addLayout(buttons)
        self._update_count()

    def sizeHint(self):
        """Wide enough for the 72-column printout [M86 p. 164] (G-011)."""
        return QSize(640, 280)

    @property
    def paints(self) -> int:
        """The list's paint count (G-011)."""
        return self.list.paints

    @property
    def mode(self) -> str:
        """The transcript shown, `verbose` or `terse` (DECISION D-055)."""
        return self.list.mode

    # -- the view protocol (G-011, G-016, G-018) ----------------------------------------------

    def begin(self, lines=None) -> None:
        """A run is about to start, with these scripted lines or none (G-018)."""
        self.model.begin(lines)

    def set_position(self, index: int) -> None:
        """The index of the event about to be given, or sought (G-016, G-018)."""
        self.position = index
        self.model.set_position(index)

    def on_event(self, event) -> None:
        """Feed the model (G-018)."""
        if event.kind == "start":
            self.events = 0
        self.events += 1
        self.model.on_event(event)

    def redraw(self, now=False) -> None:
        """Move the list's cursor and mark it dirty; with `now`, paint at once (Numbo GUI,
        `paint.py`)."""
        self.list.set_position(self.position, self.follow_box.isChecked())
        self._update_count()
        if now:
            paint.paint_all()

    def finish(self, outcome) -> None:
        """The run controller's `RunOutcome`: the model writes the run's last lines
        (G-018)."""
        self.model.finish(outcome)
        self.redraw(now=True)

    def _update_count(self):
        text = f"{self.list.row_count()} lines"
        if self.count_label.text() != text:
            self.count_label.setText(text)

    # -- the switch, find, the text -----------------------------------------------------------

    def set_mode(self, mode: str) -> None:
        """Verbose or Terse (DECISION D-055; G-018)."""
        index = self.mode_box.findData(mode)
        if index < 0:
            raise ValueError(f"unknown mode {mode!r}")
        self.mode_box.setCurrentIndex(index)

    def _mode_changed(self, _index):
        self.list.set_mode(self.mode_box.currentData())
        self._update_count()

    def find_next(self) -> int | None:
        """Find the next row containing the find box's text (G-019)."""
        return self.list.find(self.find_edit.text())

    def text(self, mode=None) -> str:
        """The transcript in the mode shown (or given), as the CLI prints it
        (G-018)."""
        return self.model.text(self.mode if mode is None else mode)

    # -- the command line (G-019) ---------------------------------------------------------------

    def submit(self) -> None:
        """Enter at the command line: emit the text as `line_typed` and clear the box
        (G-019)."""
        text = self.command_edit.text().strip()
        if not text:
            return
        self.command_edit.clear()
        self.line_typed.emit(text)

    def print_command(self, name: str) -> list:
        """Print a command at the cursor, not as an input (G-019); the lines printed."""
        lines = self.model.print_command(name)
        self.redraw(now=True)
        return lines

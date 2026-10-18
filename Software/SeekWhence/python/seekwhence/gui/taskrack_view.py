"""The taskrack pane (P6): the tasks waiting on the rack by type, the task just chosen, the
cleanings and the history of chosen tasks, a view of `models/taskrack.py`'s
`TaskrackModel` (PANES.md P6; Numbo GUI, `coderack_view.py`; G-027, G-028, G-029).

**GUI design, not Meredith's** (ANSWERS.md 4): the thesis describes the taskrack in prose,
tasks that "run in simulated parallel" and wait on the rack "until chosen and run", with
"urgencies" that weight the draw, and cleanings of old tasks [M86 pp. 41-42], but has no
figure of it, and FCCA has none. The drawing follows the Numbo GUI's coderack view: the
chosen task on a band at the top, the waiting tasks as rows with count bars. Here the rows
are the task types (the Numbo GUI's are urgency bins), each coloured by its arena
(DECISION D-057) with a bar of its share of the rack's total weight, the chance the next
draw falls on it (DECISION D-007); a click on a row lists its tasks. The band shows the
task as the trace prints it [M86 p. 164], its chance at the draw, and `stale` in grey when
it found the world changed [M86 p. 42] (DECISION D-009). A cleaning shows as a banner
(DECISION D-010). Under the rack, the history of every task chosen this run, a virtualised
list (Numbo GUI, `log_view.py`'s `LogList`), with the per-type tallies since the last term
above it. A click on a task selects, in the shared selection, the structures its arguments
name (G-025); tasks naming something selected are outlined. The arena filter applies to the
rows and the history.
"""

from PySide6.QtCore import QRect, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QFontMetrics, QPainter, QPen
from PySide6.QtWidgets import (QAbstractScrollArea, QCheckBox, QComboBox, QHBoxLayout, QLabel,
                               QSizePolicy, QSplitter, QVBoxLayout, QWidget)

from seekwhence.gui import paint
from seekwhence.gui.models import taskrack as tr

# The smallest a list may be (width, height): Qt's own minimum for a scroll area is
# about 70 x 70, which, over nine panes, kept the window from 1280 x 800 (G-058).
LIST_MIN = (40, 24)

__all__ = ["LineList", "RackList", "HistoryList", "TaskrackView"]

PAGES = "[M86 pp. 41-42]. GUI design, not Meredith's: no figure of the taskrack exists."
BACKGROUND = "#fbfbf8"
TEXT, MUTED, GREY = "#1a1a1a", "#6b6b6b", "#9a9a9a"
BAR_TRACK = "#e6e9ef"
CHOSEN_TINT, STALE_TINT, BANNER_TINT = "#fbe3f0", "#ededed", "#fff4c2"
SELECTED = "#e07b00"
PAD = 4
MARK_W, TYPE_W, BAR_W = 14, 140, 90
INDENT = 22
ARENA_CHOICES = (("all arenas", None),) + tuple((arena, arena) for arena in tr.ARENAS)


class LineList(QAbstractScrollArea):
    """A virtualised list of `Line`s painted straight from the model, one row per line
    (Numbo GUI, `log_view.py`'s `LogList`; G-028): `paints` counts the paint calls. The
    subclasses give `row_count` and `line(row)`; a click emits `line_clicked(line)`."""

    line_clicked = Signal(object)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.paints = 0
        self.setMinimumSize(*LIST_MIN)       # a dock can be short (G-058)
        self.font_ = QFont(self.font())
        self.font_.setPointSizeF(9)
        self.bold = QFont(self.font_)
        self.bold.setBold(True)
        self.metrics = QFontMetrics(self.font_)
        self.row_height = self.metrics.height() + 4
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.viewport().setAttribute(Qt.WidgetAttribute.WA_OpaquePaintEvent)
        self._colors = {name: QColor(value) for name, value in tr.ARENA_COLORS.items()}

    def row_count(self) -> int:
        """How many rows there are (Numbo GUI, `log_view.py`)."""
        raise NotImplementedError

    def line(self, row: int) -> tr.Line:
        """The row's `Line` (G-028)."""
        raise NotImplementedError

    def rows_in_view(self) -> int:
        """How many whole rows the viewport holds (Numbo GUI, `log_view.py`)."""
        return max(1, self.viewport().height() // self.row_height)

    def first_row(self) -> int:
        """The row at the top of the viewport (Numbo GUI, `log_view.py`)."""
        return self.verticalScrollBar().value()

    def row_at(self, y) -> int | None:
        """The row at the viewport's y, or None (Numbo GUI, `log_view.py`)."""
        row = self.first_row() + int(y // self.row_height)
        return row if 0 <= row < self.row_count() else None

    def update_rows(self) -> None:
        """The number of rows changed: the scroll bar's range (Numbo GUI,
        `log_view.py`)."""
        bar = self.verticalScrollBar()
        top = max(0, self.row_count() - self.rows_in_view())
        if bar.maximum() != top:
            bar.setRange(0, top)
        bar.setPageStep(self.rows_in_view())

    def scroll_to_end(self) -> None:
        """Bring the last row into view (Numbo GUI, `log_view.py`)."""
        self.verticalScrollBar().setValue(self.verticalScrollBar().maximum())

    def resizeEvent(self, event):
        """The scroll range follows the height (Numbo GUI, `log_view.py`)."""
        super().resizeEvent(event)
        self.update_rows()

    def mousePressEvent(self, event):
        """A click emits the row's line (G-028)."""
        row = self.row_at(event.position().y())
        if row is not None:
            self.line_clicked.emit(self.line(row))

    def event(self, e):
        """The tooltip is the line's (Numbo GUI, `log_view.py`)."""
        if e.type() == e.Type.ToolTip:
            row = self.row_at(e.pos().y())
            self.setToolTip(self.line(row).tooltip or self.line(row).text
                            if row is not None else "")
        return super().event(e)

    def paintEvent(self, event):
        """Paint the rows in the dirty rectangle, each by its line's kind: the band
        tinted, a stale task grey, a banner yellow, a row with its bar in the arena's
        colour, a selected task outlined (Numbo GUI, `coderack_view.py`; G-028)."""
        self.paints += 1
        p = QPainter(self.viewport())
        p.fillRect(event.rect(), QColor(BACKGROUND))
        first = self.first_row()
        rect = event.rect()
        top = first + max(0, rect.top()) // self.row_height
        bottom = first + rect.bottom() // self.row_height + 1
        width = self.viewport().width()
        for row in range(top, min(bottom, self.row_count())):
            y = (row - first) * self.row_height
            self._paint_line(p, self.line(row), QRect(0, y, width, self.row_height))
        p.end()

    def _paint_line(self, p, line, box):
        h = box.height()
        if line.tint:
            p.fillRect(box, QColor(STALE_TINT if line.grey else CHOSEN_TINT))
        elif line.kind == "banner":
            p.fillRect(box, QColor(BANNER_TINT))
        color = QColor(GREY if line.grey else MUTED if line.kind == "note" else TEXT)
        if line.arena is not None and not line.grey and line.kind != "note":
            color = self._colors.get(line.arena, color)
        flags = Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft
        if line.kind == "row":
            p.setFont(self.bold)
            p.setPen(color)
            p.drawText(QRect(PAD, box.y(), MARK_W, h), flags,
                       "▾" if line.type in getattr(self, "expanded", ()) else "▸")
            type_box = QRect(PAD + MARK_W, box.y(), TYPE_W, h)
            p.drawText(type_box, flags, self.metrics.elidedText(
                line.text, Qt.TextElideMode.ElideRight, TYPE_W - PAD))
            track = QRect(PAD + MARK_W + TYPE_W, box.y() + 3, BAR_W, h - 6)
            p.fillRect(track, QColor(BAR_TRACK))
            bar = round(BAR_W * (line.bar or 0.0))
            if bar:
                p.fillRect(QRect(track.x(), track.y(), bar, track.height()), color)
            p.setFont(self.font_)
            if bar >= self.metrics.horizontalAdvance(line.count) + 8:
                p.setPen(QColor("#ffffff"))
                p.drawText(track.adjusted(4, 0, 0, 0), flags, line.count)
            else:
                p.setPen(QColor(TEXT))
                p.drawText(track.adjusted(bar + 4, 0, 0, 0), flags, line.count)
            p.setPen(QColor(MUTED))
            share = tr.chance_text(line.bar)
            p.drawText(QRect(track.right() + 8, box.y(), box.width() - track.right() - 8, h),
                       flags, share)
        else:
            x = PAD + (INDENT if line.kind == "task" else 0)
            p.setFont(self.bold if line.kind == "chosen" else self.font_)
            p.setPen(color)
            room = max(10, box.width() - x - PAD)
            p.drawText(QRect(x, box.y(), room, h), flags,
                       self.metrics.elidedText(line.text, Qt.TextElideMode.ElideRight, room))
        if line.selected:
            p.setPen(QPen(QColor(SELECTED), 1, Qt.PenStyle.DotLine))
            p.drawRect(box.adjusted(0, 0, -1, -1))


class RackList(LineList):
    """The rack's lines, `rack_lines(model, expanded, arena)`, set at each redraw;
    `expanded` is the set of types whose tasks are listed (G-028)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.lines = ()
        self.expanded = set()           # the types listed, as last drawn

    def set_lines(self, lines, expanded=frozenset()) -> None:
        """New lines, with the types listed; the scroll range follows and the viewport
        is marked dirty (G-028)."""
        self.lines = tuple(lines)
        self.expanded = set(expanded)
        self.update_rows()
        self.viewport().update()

    def row_count(self) -> int:
        """How many lines (G-028)."""
        return len(self.lines)

    def line(self, row: int) -> tr.Line:
        """The row's line (G-028)."""
        return self.lines[row]


class HistoryList(LineList):
    """The history of chosen tasks: one row per `ChosenTask` of the model's `history`
    that passes the arena filter, built into a `Line` only when painted, so a run of
    26,000 tasks costs one integer per row (PANES.md P6; Numbo GUI, `log_view.py`;
    G-028)."""

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model
        self.arena = None
        self.rows: list = []       # indexes into model.history
        self.seen = 0

    def clear(self) -> None:
        """A new run (G-028)."""
        self.rows = []
        self.seen = 0
        self.update_rows()
        self.viewport().update()

    def set_arena(self, arena) -> None:
        """Filter the rows by arena (None: all) and rebuild them once (G-028)."""
        self.arena = arena
        self.rows = []
        self.seen = 0
        self.sync()

    def sync(self) -> None:
        """Append the rows of the tasks chosen since the last sync (G-028)."""
        history = self.model.history
        for i in range(self.seen, len(history)):
            if self.arena is None or history[i].arena == self.arena:
                self.rows.append(i)
        self.seen = len(history)
        self.update_rows()
        self.viewport().update()

    def row_count(self) -> int:
        """How many rows pass the filter (G-028)."""
        return len(self.rows)

    def line(self, row: int) -> tr.Line:
        """The row's line, built from its `ChosenTask` (G-028)."""
        c = self.model.history[self.rows[row]]
        return tr.Line("history", tr.history_text(c), c.arena, number=c.number, grey=c.stale,
                       selected=self.model.names_selected(c.args),
                       tooltip=f"{tr.chosen_text(c)}\nclock {c.clock}, urgency {c.urgency}, "
                               f"arena {c.arena}, chance {tr.chance_text(c.chance)}"
                               + (f"\nstale: {c.reason}" if c.stale else ""))


class TaskrackView(QWidget):
    """The taskrack dock's view (see the module docstring; G-027, G-028, G-029).
    `history` is the window's `EventLog`, read at `position`; `selection` the shared
    `Selection` (G-025). Signals: `selected(name)` after a click on a task."""

    pane_id = "P6"
    title = "Taskrack"
    pages = PAGES
    selected = Signal(str)

    def __init__(self, history, parent=None, selection=None):
        super().__init__(parent)
        self.history = history
        self.model = tr.TaskrackModel(selection)
        self.position = -1
        self.events = 0
        self.arena = None
        self.opened = set()             # the types opened by a click (G-028)
        self.rack = RackList()
        self.rack.line_clicked.connect(self._rack_clicked)
        self.list = HistoryList(self.model)
        self.list.line_clicked.connect(self._history_clicked)
        self.label = QLabel("GUI design, not Meredith's")
        self.label.setToolTip("No figure of the taskrack exists in the thesis or in FCCA; "
                              "the drawing follows the Numbo GUI's coderack view "
                              "(ANSWERS.md 4). The rack itself: [M86 pp. 41-42].")
        self.label.setStyleSheet(f"color: {MUTED}; font-style: italic;")
        self.label.setMinimumWidth(1)        # clipped, not widening the dock (G-058)
        self.arena_box = QComboBox()
        for text, arena in ARENA_CHOICES:
            self.arena_box.addItem(text, arena)
        self.arena_box.setSizeAdjustPolicy(
            QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
        self.arena_box.setMinimumContentsLength(8)
        self.arena_box.setToolTip("Show one arena's rows and history (DECISION D-057)")
        self.arena_box.currentIndexChanged.connect(self._arena_changed)
        self.expand_box = QCheckBox("every task")
        self.expand_box.setToolTip("List the tasks of every type (click a row for one type)")
        self.expand_box.toggled.connect(self._expand_toggled)
        self.follow_box = QCheckBox("Follow")
        self.follow_box.setChecked(True)
        self.follow_box.setToolTip("Keep the last chosen task in view")
        bar = QHBoxLayout()
        bar.setContentsMargins(4, 2, 4, 0)
        bar.addWidget(self.label)
        bar.addStretch(1)
        bar.addWidget(self.expand_box)
        bar.addWidget(self.arena_box)
        self.tally_label = QLabel(tr.tally_text(self.model))
        self.tally_label.setToolTip("How many tasks of each type were chosen since the last "
                                    "term (PANES.md P6)")
        self.tally_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)
        self.count_label = QLabel("0 chosen")
        head = QHBoxLayout()
        head.setContentsMargins(4, 2, 4, 0)
        head.addWidget(self.tally_label, 1)
        head.addWidget(self.count_label)
        head.addWidget(self.follow_box)
        below = QWidget()
        lower = QVBoxLayout(below)
        lower.setContentsMargins(0, 0, 0, 0)
        lower.setSpacing(2)
        lower.addLayout(head)
        lower.addWidget(self.list, 1)
        self.splitter = QSplitter(Qt.Orientation.Vertical)
        self.splitter.addWidget(self.rack)
        self.splitter.addWidget(below)
        self.splitter.setChildrenCollapsible(False)
        self.splitter.setStretchFactor(0, 3)
        self.splitter.setStretchFactor(1, 2)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)
        layout.addLayout(bar)
        layout.addWidget(self.splitter, 1)
        self.model.selection.subscribe(self._on_selection)
        self.redraw()

    def sizeHint(self):
        """Room for the band, a dozen rows and some history (G-011)."""
        return QSize(420, 360)

    @property
    def paints(self) -> int:
        """The rack list's paint count (G-011)."""
        return self.rack.paints

    @property
    def expanded(self) -> frozenset:
        """The types whose tasks are listed: every waiting type when "every task" is on,
        else the ones opened by a click (G-028)."""
        if self.expand_box.isChecked():
            return frozenset(t.type for t in self.model.waiting.values())
        return frozenset(self.opened)

    def lines(self) -> tuple:
        """The rack list's lines as last drawn (G-028)."""
        return self.rack.lines

    def history_rows(self) -> list:
        """The history list's rows, as `ChosenTask`s (G-028)."""
        return [self.model.history[i] for i in self.list.rows]

    # -- the view protocol (G-011, G-016) ----------------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given (G-016)."""
        self.position = index

    def on_event(self, event) -> None:
        """Feed the model and take the snapshot in force at `position` (G-027); a
        `start` clears the history list."""
        if event.kind == "start":
            self.events = 0
            self.list.clear()
        self.events += 1
        self.model.on_event(event)
        self.model.update(self.history.state_at(self.position))

    def redraw(self, now=False) -> None:
        """Rebuild the rack's lines, add the new history rows, refresh the tallies and
        mark both lists dirty; with `now`, paint at once (Numbo GUI, `paint.py`;
        G-028)."""
        expanded = self.expanded
        self.rack.set_lines(tr.rack_lines(self.model, expanded, self.arena), expanded)
        self.list.sync()
        if self.follow_box.isChecked():
            self.list.scroll_to_end()
        self._set_text(self.tally_label, tr.tally_text(self.model))
        self._set_text(self.count_label, f"{len(self.model.history)} chosen")
        if now:
            paint.paint_all()

    @staticmethod
    def _set_text(label, text):
        if label.text() != text:
            label.setText(text)

    # -- interactions (PANES.md P6) ---------------------------------------------------------

    def set_arena(self, arena) -> None:
        """Filter the rows and the history by arena (None: all) (DECISION D-057;
        G-028)."""
        self.arena = arena
        index = self.arena_box.findData(arena)
        if index >= 0 and self.arena_box.currentIndex() != index:
            self.arena_box.setCurrentIndex(index)
        self.list.set_arena(arena)
        self.redraw(now=True)

    def _arena_changed(self, index):
        self.set_arena(self.arena_box.itemData(index))

    def toggle_row(self, type: str) -> bool:
        """List a type's tasks, or fold them; returns whether it is listed now (PANES.md
        P6; G-028)."""
        if type in self.opened:
            self.opened.discard(type)
        else:
            self.opened.add(type)
        self.redraw(now=True)
        return type in self.expanded

    def set_expand_all(self, on: bool) -> None:
        """List every type's tasks ("every task"), or only the opened ones (G-028)."""
        self.expand_box.setChecked(on)

    def _expand_toggled(self, _on):
        self.redraw(now=True)

    def select_task(self, number: int | None) -> tuple:
        """Select the structures a task's arguments name (a second click clears), paint
        now and emit `selected`; returns the names (G-025, G-028)."""
        if number is None:
            return ()
        names = self.model.select_task(number)
        self.redraw(now=True)
        self.selected.emit(f"task{number}")
        return names

    def _rack_clicked(self, line):
        if line.kind == "row":
            self.toggle_row(line.type)
        elif line.number is not None:
            self.select_task(line.number)

    def _history_clicked(self, line):
        self.select_task(line.number)

    def _on_selection(self, _selection):
        self.redraw(now=True)

    def render_png(self, path):
        """Grab the pane to a PNG at `path` (Numbo GUI, `coderack_view.py`)."""
        image = self.grab()
        if not image.save(path):
            raise OSError(f"could not write {path}")
        return image

"""The event log pane (P9): every event of the run, one row each, filterable by kind and
by group (PANES.md P9; Numbo GUI, `log_view.py`, adopted; G-044, G-045).

`LogView` shows the window's `EventLog` (`models/event_log.py`), the history the tap
fills on the worker (G-012); the view itself is an observer only to count events and
keep the scroll range in step, as the other panes are (G-011):
  - `set_position(index)` for the event drawn now, then `redraw()`, which paints now
    (the run waits for it, as for the other views; the window calls `redraw(now=False)`
    and paints every view at once, `paint.py`);
  - the run's next event scrolls the rows by blitting them in the backing store, so only
    the new row and the old row of the event drawn now are painted;
  - a row is `index  clock  kind  text` (`row_text`, `event_log.event_text`): the clock
    is the task under way [M86 p. 41];
  - the event drawn now has a tinted row and is kept in view (unless Follow is off); the
    events after it (a seek that went back) are greyed;
  - double-clicking a row, or Enter on the cursor row (moved with the arrow keys, Page
    Up/Down, Home and End), emits `event_activated(index)`: the window seeks there;
  - the Kinds menu hides or shows each kind of event, and each group of kinds at once
    (`GROUPS`).

`LogList`, the rows, is a `QAbstractScrollArea` that paints only the rows in view,
straight from the history: nothing is kept per row, and adding an event costs the same
at the 100,000th event as at the first (Numbo GUI, `log_view.py`: a `QListView` over a
model lays out all its rows again when one is added at the end).
"""

from PySide6.QtCore import QRect, Qt, Signal
from PySide6.QtGui import QAction, QColor, QFontDatabase, QFontMetrics, QPainter, QPen
from PySide6.QtWidgets import (QAbstractScrollArea, QCheckBox, QHBoxLayout, QLabel, QMenu,
                               QToolButton, QVBoxLayout, QWidget)

from seekwhence.gui.models.event_log import GROUPS, KINDS, EventLog, event_text
from seekwhence.gui.taskrack_view import LIST_MIN

__all__ = ["row_text", "LogList", "LogView"]

BACKGROUND = "#ffffff"
TEXT = "#1a1a1a"
FUTURE = "#9a9a9a"      # the rows after the event drawn now
NOW = "#fbe3f0"         # the row of the event drawn now
CURSOR = "#e6007e"
PAD = 4
PAGES = "Numbo GUI, log_view.py"


def row_text(index, clock, event):
    """One row: the event's index, the clock in force (blank before the first task), its
    kind and `event_text` (Numbo GUI, `log_view.py`; PANES.md P9)."""
    clock_text = "" if clock is None else str(clock)
    return f"{index:>6} {clock_text:>6}  {event.kind:<18} {event_text(event)}"


class LogList(QAbstractScrollArea):
    """The rows of `history`'s shown events (see the module docstring; Numbo GUI,
    `log_view.py`). `position`: the event drawn now; `cursor_row`: the keyboard's row
    (None); `paints`: how many times it has been painted."""

    activated = Signal(int)     # an event index

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.history = history
        self.position = -1
        self.cursor_row = None
        self.paints = 0
        self.setMinimumSize(*LIST_MIN)       # a dock can be short (G-058)
        self.painted_rect = QRect()     # the rect of the latest paint
        self.setFont(QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont))
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        # paintEvent fills what it paints: with this, scroll() blits the rows in the
        # backing store instead of repainting them all.
        self.viewport().setAttribute(Qt.WidgetAttribute.WA_OpaquePaintEvent)
        self._colors = {k: QColor(v) for k, v in
                        (("bg", BACKGROUND), ("text", TEXT), ("future", FUTURE), ("now", NOW))}
        self._update_metrics()

    def _update_metrics(self):
        self.metrics = QFontMetrics(self.font())
        self.line = self.metrics.height() + 2

    # -- rows ---------------------------------------------------------------------

    def row_count(self) -> int:
        """How many rows are shown (Numbo GUI, `log_view.py`)."""
        return len(self.history.visible)

    def row_index(self, row: int) -> int:
        """The event index of the row (Numbo GUI, `log_view.py`)."""
        return self.history.visible[row]

    def row_display(self, row: int) -> str:
        """The row's text (Numbo GUI, `log_view.py`)."""
        i = self.history.visible[row]
        return row_text(i, self.history.clock_at(i), self.history.events[i])

    def is_future(self, row: int) -> bool:
        """Whether the row's event comes after the one drawn now (Numbo GUI,
        `log_view.py`)."""
        return self.history.visible[row] > self.position

    def rows_in_view(self) -> int:
        """How many whole rows fit (Numbo GUI, `log_view.py`)."""
        return max(1, self.viewport().height() // self.line)

    def first_row(self) -> int:
        """The row at the top (Numbo GUI, `log_view.py`)."""
        return self.verticalScrollBar().value()

    def row_is_visible(self, row: int) -> bool:
        """Whether the row is in view (Numbo GUI, `log_view.py`)."""
        first = self.first_row()
        return first <= row < first + self.rows_in_view()

    def row_at(self, y) -> int | None:
        """The row at a viewport y, or None (Numbo GUI, `log_view.py`)."""
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
        """Bring the row into view: the least scrolling, or (`center`) to the middle of
        the view (Numbo GUI, `log_view.py`)."""
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
        """The event drawn now: scroll it into view and mark what changed dirty (Numbo
        GUI, `log_view.py`)."""
        old, self.position = self.position, position
        row = self.history.row_of(position)
        if follow and row is not None:
            # A run's next event comes in at the bottom; a jump is centred.
            self.scroll_to(row, center=row != self.first_row() + self.rows_in_view())
        if position == old + 1:
            # The run's next event: only the old and new rows of the event drawn now
            # change (the scroll above blitted the rest).
            self.update_event_row(old)
            self.update_event_row(position)
        elif position != old:
            self.viewport().update()

    def update_event_row(self, index: int) -> None:
        """Mark the row of the event (if it has one, in view) dirty (Numbo GUI,
        `log_view.py`)."""
        row = self.history.row_of(index) if index >= 0 else None
        first = self.first_row()
        # rows_in_view() whole rows, then a part of one at the bottom.
        if row is not None and first <= row <= first + self.rows_in_view():
            self.viewport().update(self.row_rect(row))

    # -- painting -------------------------------------------------------------------

    def paintEvent(self, event):
        """Paint the rows in the dirty rectangle (Numbo GUI, `log_view.py`)."""
        self.paints += 1
        self.painted_rect = event.rect()
        p = QPainter(self.viewport())
        c = self._colors
        rect = event.rect()
        p.fillRect(rect, c["bg"])
        h = self.history
        first = self.first_row()
        # Only the rows in the dirty rect: a run's next event exposes one row (the
        # scroll blits the others) and retints one more.
        top = first + max(0, rect.top()) // self.line
        bottom = first + rect.bottom() // self.line + 1
        ascent = self.metrics.ascent() + 1
        for row in range(top, min(bottom, self.row_count())):
            i = h.visible[row]
            y = (row - first) * self.line
            if i == self.position:
                p.fillRect(0, y, self.viewport().width(), self.line, c["now"])
            p.setPen(c["future"] if i > self.position else c["text"])
            # Not elided (a quarter of the cost): the viewport clips a long row, and its
            # tooltip has all of it.
            p.drawText(PAD, y + ascent, row_text(i, h.clock_at(i), h.events[i]))
            if row == self.cursor_row:
                p.setPen(QPen(QColor(CURSOR), 1, Qt.PenStyle.DotLine))
                p.drawRect(0, y, self.viewport().width() - 1, self.line - 1)
        p.end()

    def resizeEvent(self, event):
        """The rows in view changed (Numbo GUI, `log_view.py`)."""
        super().resizeEvent(event)
        self.update_rows()

    def scrollContentsBy(self, dx, dy):
        """Blit the rows that stay in view (Numbo GUI, `log_view.py`)."""
        # Blit the rows that stay in view; Qt marks the strip exposed.
        self.viewport().scroll(0, dy * self.line)

    # -- the mouse and the keys -------------------------------------------------------

    def activate(self, row) -> None:
        """What a double-click on the row, or Enter on it, does (Numbo GUI,
        `log_view.py`)."""
        if row is not None and 0 <= row < self.row_count():
            self.cursor_row = row
            self.activated.emit(self.row_index(row))

    def mousePressEvent(self, event):
        """A click puts the cursor on the row (Numbo GUI, `log_view.py`)."""
        row = self.row_at(event.position().y())
        if row is not None:
            self.cursor_row = row
            self.viewport().update()

    def mouseDoubleClickEvent(self, event):
        """A double-click activates the row (Numbo GUI, `log_view.py`)."""
        self.activate(self.row_at(event.position().y()))

    def event(self, e):
        """The tooltip is the row's text (Numbo GUI, `log_view.py`)."""
        if e.type() == e.Type.ToolTip:
            row = self.row_at(e.pos().y())
            self.setToolTip(self.row_display(row) if row is not None else "")
        return super().event(e)

    def keyPressEvent(self, event):
        """The arrows, Page Up/Down, Home, End move the cursor; Enter activates (Numbo GUI, `log_view.py`)."""
        n = self.row_count()
        if not n:
            return super().keyPressEvent(event)
        key = event.key()
        row = self.cursor_row
        if row is None:
            row = self.history.row_of(self.position) or 0
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


class LogView(QWidget):
    """The event log pane (see the module docstring; Numbo GUI, `log_view.py`; G-045).
    `history` is the window's `EventLog`."""

    event_activated = Signal(int)

    pane_id = "P9"
    title = "Event log"
    pages = PAGES

    def __init__(self, history=None, parent=None):
        super().__init__(parent)
        self.history = history if history is not None else EventLog()
        self.position = -1
        self.events = 0
        self.list = LogList(self.history)
        self.list.activated.connect(self.event_activated)

        self.kinds_button = QToolButton()
        self.kinds_button.setText("Kinds")
        self.kinds_button.setToolTip("the kinds of event shown, one by one or by group")
        self.kinds_button.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        menu = QMenu(self.kinds_button)
        self.show_all_action = menu.addAction("Show all")
        self.hide_all_action = menu.addAction("Hide all")
        menu.addSeparator()
        self.group_actions = {}
        for group, _kinds in GROUPS:
            action = QAction(group, menu)
            action.setCheckable(True)
            action.setChecked(True)
            action.toggled.connect(lambda on, g=group: self._group_toggled(g, on))
            menu.addAction(action)
            self.group_actions[group] = action
        menu.addSeparator()
        self.kind_actions = {}
        for kind in KINDS:
            action = QAction(kind, menu)
            action.setCheckable(True)
            action.setChecked(kind not in self.history.hidden)
            action.toggled.connect(self._filter_changed)
            menu.addAction(action)
            self.kind_actions[kind] = action
        self.show_all_action.triggered.connect(lambda: self._check_all(True))
        self.hide_all_action.triggered.connect(lambda: self._check_all(False))
        self.kinds_button.setMenu(menu)
        self.follow_box = QCheckBox("Follow")
        self.follow_box.setToolTip("keep the event drawn now in view")
        self.follow_box.setChecked(True)
        self.count_label = QLabel()
        # Fixed: its text changes at every event (Numbo GUI, `timeline.py`'s label).
        self.count_label.setFixedWidth(self.count_label.fontMetrics().horizontalAdvance(
            "000000 of 000000 events") + 8)
        self.count_label.setFixedHeight(self.count_label.sizeHint().height())
        self.count_label.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)

        bar = QHBoxLayout()
        bar.setContentsMargins(0, 0, 0, 0)
        bar.addWidget(self.kinds_button)
        bar.addWidget(self.follow_box)
        bar.addStretch(1)
        bar.addWidget(self.count_label)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(4, 4, 4, 4)
        layout.addLayout(bar)
        layout.addWidget(self.list, 1)
        self._bulk = False
        self.reload()

    @property
    def paints(self) -> int:
        """How many times the rows have been painted (Numbo GUI, `log_view.py`)."""
        return self.list.paints

    # -- the view protocol (G-011, G-016) --------------------------------------------

    def set_position(self, index: int) -> None:
        """The index of the event about to be given: the scroll range takes in its row
        (the tap recorded it already, G-012) and the list follows it (Numbo GUI,
        `log_view.py`; G-016)."""
        self.position = index
        self.list.update_rows()
        self.list.set_position(index, self.follow_box.isChecked())

    def on_event(self, event) -> None:
        """Count the event and keep the scroll range in step with the history (the tap
        added the event's row already, G-012); a `start` begins a new run (G-011)."""
        if event.kind == "start":
            self.events = 0
            self.list.cursor_row = None
        self.events += 1
        self.list.update_rows()

    def redraw(self, now=True) -> None:
        """Paint the rows now. `now` false (the window's `paint_all` paints them):
        `set_position` has marked what changed, the rows the scroll exposed and the two
        tinted rows, and nothing more is marked (Numbo GUI, `log_view.py`)."""
        self._update_count()
        if now and self.isVisible():
            self.list.viewport().repaint()

    # -- the history -------------------------------------------------------------

    def reload(self) -> None:
        """The history was replaced, cleared or refiltered (Numbo GUI,
        `log_view.py`)."""
        self.list.cursor_row = None
        self.list.update_rows()
        self._update_count()
        self.list.viewport().update()

    def _update_count(self):
        text = f"{len(self.history.visible)} of {len(self.history)} events"
        if self.count_label.text() != text:
            self.count_label.setText(text)

    def render_png(self, path):
        """The pane as it is, to a PNG file; returns the image (Numbo GUI,
        `render.py`)."""
        image = self.grab().toImage()
        image.save(str(path))
        return image

    # -- the filter ----------------------------------------------------------------

    def hidden_kinds(self) -> frozenset:
        """The kinds whose rows are hidden (Numbo GUI, `log_view.py`)."""
        return frozenset(kind for kind, action in self.kind_actions.items()
                         if not action.isChecked())

    def _check_all(self, on):
        self._bulk = True
        try:
            for action in self.kind_actions.values():
                action.setChecked(on)
        finally:
            self._bulk = False
        self._filter_changed()

    def _group_toggled(self, group, on):
        """A group's action checks or unchecks every kind of the group at once
        (G-045)."""
        if self._bulk:
            return
        kinds = dict(GROUPS)[group]
        self._bulk = True
        try:
            for kind in kinds:
                self.kind_actions[kind].setChecked(on)
        finally:
            self._bulk = False
        self._filter_changed()

    def _filter_changed(self):
        if self._bulk:
            return
        hidden = self.hidden_kinds()
        self._bulk = True
        try:
            for group, kinds in GROUPS:
                self.group_actions[group].setChecked(not all(k in hidden for k in kinds))
        finally:
            self._bulk = False
        self.history.set_hidden(hidden)
        self.reload()
        self.list.set_position(self.list.position, self.follow_box.isChecked())

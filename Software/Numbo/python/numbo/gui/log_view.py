"""The event log: every event of the run, one row each, filterable by kind.

LogView shows an event_log.EventLog, the window's history of the run (the
window adds each event to it; the log view doesn't observe the run itself,
because a replay moves through the history without adding to it):
  - set_position(index) for the event drawn now, then redraw(), which
    paints now (the run waits for it, as for the other views; the window
    calls redraw(now=False) and paints every view at once);
  - the run's next event scrolls the rows by blitting them in the backing
    store, so only the new row and the old row of the event drawn now are
    painted (2 rows an event, measured, instead of the ~16 in view);
  - a row is "index  kind  text" (row_text, event_log.event_text);
  - the event drawn now has a tinted row and is kept in view (unless
    Follow is off); the events after it (a replay that went back) are
    greyed;
  - double-clicking a row, or Enter on the cursor row (moved with the
    arrow keys, Page Up/Down, Home and End), emits event_activated(index):
    the window seeks there;
  - the Kinds menu hides or shows each kind of event.

LogList, the rows, is a QAbstractScrollArea that paints only the rows in
view, straight from the history: nothing is kept per row, and adding an
event costs the same at the 100,000th event as at the first.  (A
QListView or QTreeView over a model lays out all its rows again when one
is added at the end: 2 ms an event after 1,000 events, 5 ms after 6,000,
measured.)
"""

from PySide6.QtCore import QRect, Qt, Signal
from PySide6.QtGui import QAction, QColor, QFontDatabase, QFontMetrics, QPainter, QPen
from PySide6.QtWidgets import (QAbstractScrollArea, QCheckBox, QHBoxLayout, QLabel, QMenu,
                               QToolButton, QVBoxLayout, QWidget)

from numbo.models.event_log import KINDS, EventLog, event_text

__all__ = ["row_text", "LogList", "LogView"]

BACKGROUND = "#ffffff"
TEXT = "#1a1a1a"
FUTURE = "#9a9a9a"      # the rows after the event drawn now
NOW = "#fbe3f0"         # the row of the event drawn now
CURSOR = "#e6007e"
PAD = 4


def row_text(index, event):
    return f"{index:>6}  {event.kind:<16} {event_text(event)}"


class LogList(QAbstractScrollArea):
    """The rows of HISTORY's shown events (see the module docstring).
    POSITION: the event drawn now; CURSOR_ROW: the keyboard's row (None);
    PAINTS: how many times it has been painted."""

    activated = Signal(int)     # an event index

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.history = history
        self.position = -1
        self.cursor_row = None
        self.paints = 0
        self.painted_rect = QRect()     # the rect of the latest paint
        self.setFont(QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont))
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.viewport().setBackgroundRole(self.viewport().backgroundRole())
        # paintEvent fills what it paints: with this, scroll() blits the
        # rows in the backing store instead of repainting them all.
        self.viewport().setAttribute(Qt.WidgetAttribute.WA_OpaquePaintEvent)
        self._colors = {k: QColor(v) for k, v in
                        (("bg", BACKGROUND), ("text", TEXT), ("future", FUTURE), ("now", NOW))}
        self._update_metrics()

    def _update_metrics(self):
        self.metrics = QFontMetrics(self.font())
        self.line = self.metrics.height() + 2

    # -- rows ---------------------------------------------------------------------

    def row_count(self):
        return len(self.history.visible)

    def row_index(self, row):
        """The event index of ROW."""
        return self.history.visible[row]

    def row_display(self, row):
        i = self.history.visible[row]
        return row_text(i, self.history.events[i])

    def is_future(self, row):
        return self.history.visible[row] > self.position

    def rows_in_view(self):
        return max(1, self.viewport().height() // self.line)

    def first_row(self):
        return self.verticalScrollBar().value()

    def row_is_visible(self, row):
        first = self.first_row()
        return first <= row < first + self.rows_in_view()

    def row_at(self, y):
        row = self.first_row() + int(y // self.line)
        return row if 0 <= row < self.row_count() else None

    def row_rect(self, row):
        return QRect(0, (row - self.first_row()) * self.line, self.viewport().width(), self.line)

    def update_rows(self):
        """The number of rows changed: the scroll bar's range."""
        bar = self.verticalScrollBar()
        top = max(0, self.row_count() - self.rows_in_view())
        if bar.maximum() != top:
            bar.setRange(0, top)
        bar.setPageStep(self.rows_in_view())
        if self.cursor_row is not None and self.cursor_row >= self.row_count():
            self.cursor_row = None

    def scroll_to(self, row, center=False):
        """Bring ROW into view: the least scrolling, or (CENTER) to the
        middle of the view."""
        bar, first, n = self.verticalScrollBar(), self.first_row(), self.rows_in_view()
        if first <= row < first + n:
            return
        if center:
            bar.setValue(row - n // 2)
        elif row < first:
            bar.setValue(row)
        else:
            bar.setValue(row - n + 1)

    def set_position(self, position, follow=True):
        old, self.position = self.position, position
        row = self.history.row_of(position)
        if follow and row is not None:
            # A run's next event comes in at the bottom; a jump is centred.
            self.scroll_to(row, center=row != self.first_row() + self.rows_in_view())
        if position == old + 1:
            # The run's next event: only the old and new rows of the event
            # drawn now change (the scroll above blitted the rest).
            self.update_event_row(old)
            self.update_event_row(position)
        elif position != old:
            self.viewport().update()

    def update_event_row(self, index):
        """Mark the row of event INDEX (if it has one, in view) dirty."""
        row = self.history.row_of(index) if index >= 0 else None
        first = self.first_row()
        # rows_in_view() whole rows, then a part of one at the bottom.
        if row is not None and first <= row <= first + self.rows_in_view():
            self.viewport().update(self.row_rect(row))

    # -- painting -------------------------------------------------------------------

    def paintEvent(self, event):
        self.paints += 1
        self.painted_rect = event.rect()
        p = QPainter(self.viewport())
        c = self._colors
        rect = event.rect()
        p.fillRect(rect, c["bg"])
        h = self.history
        first = self.first_row()
        # Only the rows in the dirty rect: a run's next event exposes one
        # row (the scroll blits the others) and retints one more.
        top = first + max(0, rect.top()) // self.line
        bottom = first + rect.bottom() // self.line + 1
        ascent = self.metrics.ascent() + 1
        for row in range(top, min(bottom, self.row_count())):
            i = h.visible[row]
            y = (row - first) * self.line
            if i == self.position:
                p.fillRect(0, y, self.viewport().width(), self.line, c["now"])
            p.setPen(c["future"] if i > self.position else c["text"])
            # Not elided (a quarter of the cost): the viewport clips a long
            # row, and its tooltip has all of it.
            p.drawText(PAD, y + ascent, row_text(i, h.events[i]))
            if row == self.cursor_row:
                p.setPen(QPen(QColor(CURSOR), 1, Qt.PenStyle.DotLine))
                p.drawRect(0, y, self.viewport().width() - 1, self.line - 1)
        p.end()

    def resizeEvent(self, event):
        super().resizeEvent(event)
        self.update_rows()

    def scrollContentsBy(self, dx, dy):
        # Blit the rows that stay in view; Qt marks the strip exposed.
        self.viewport().scroll(0, dy * self.line)

    # -- the mouse and the keys -------------------------------------------------------

    def activate(self, row):
        """What a double-click on ROW, or Enter on it, does."""
        if row is not None and 0 <= row < self.row_count():
            self.cursor_row = row
            self.activated.emit(self.row_index(row))

    def mousePressEvent(self, event):
        row = self.row_at(event.position().y())
        if row is not None:
            self.cursor_row = row
            self.viewport().update()

    def mouseDoubleClickEvent(self, event):
        self.activate(self.row_at(event.position().y()))

    def event(self, e):
        if e.type() == e.Type.ToolTip:
            row = self.row_at(e.pos().y())
            self.setToolTip(self.row_display(row) if row is not None else "")
        return super().event(e)

    def keyPressEvent(self, event):
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
    """The event log (see the module docstring)."""

    event_activated = Signal(int)

    def __init__(self, history=None, parent=None):
        super().__init__(parent)
        self.history = history if history is not None else EventLog()
        self.list = LogList(self.history)
        self.list.activated.connect(self.event_activated)

        self.kinds_button = QToolButton()
        self.kinds_button.setText("Kinds")
        self.kinds_button.setToolTip("the kinds of event shown")
        self.kinds_button.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        menu = QMenu(self.kinds_button)
        self.show_all_action = menu.addAction("Show all")
        self.hide_all_action = menu.addAction("Hide all")
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
        # Fixed: its text changes at every event (see TimelineBar.label).
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
    def paints(self):
        return self.list.paints

    # -- the history -------------------------------------------------------------

    def appended(self, shown=True):
        """An event was added to the history (SHOWN: it has a row)."""
        if shown:
            self.list.update_rows()

    def reload(self):
        """The history was replaced, cleared or refiltered."""
        self.list.cursor_row = None
        self.list.update_rows()
        self._update_count()
        self.list.viewport().update()

    def set_position(self, position):
        self.list.set_position(position, self.follow_box.isChecked())

    def redraw(self, now=True):
        """Paint the rows now.  NOW false (the window's paint_all paints
        them): set_position has marked what changed, the rows the scroll
        exposed and the two tinted rows, and nothing more is marked."""
        self._update_count()
        if now and self.isVisible():
            self.list.viewport().repaint()

    def _update_count(self):
        text = f"{len(self.history.visible)} of {len(self.history)} events"
        if self.count_label.text() != text:
            self.count_label.setText(text)

    # -- the filter ----------------------------------------------------------------

    def _check_all(self, on):
        self._bulk = True
        try:
            for action in self.kind_actions.values():
                action.setChecked(on)
        finally:
            self._bulk = False
        self._filter_changed()

    def _filter_changed(self):
        if self._bulk:
            return
        hidden = {kind for kind, action in self.kind_actions.items() if not action.isChecked()}
        self.history.set_hidden(hidden)
        self.reload()
        self.set_position(self.list.position)

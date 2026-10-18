"""P10 Trace and event log (PANES.md P10; Numbo GUI, `log_view.py`; G-042): two tabs over
the window's history.

  - The **Trace** tab shows the text trace exactly as the CLI prints it, from
    `models/trace.py`'s `TraceModel` (the engine's own `observe.TextTrace`, D-065): one
    line per codelet with its follow-ups, the first and the last lines; a Quiet switch
    shows the `--quiet` form. Every line is tagged with the index of the event its block
    starts at: the lines whose block starts after the position shown are greyed (so,
    when the views are back in the history), and the block the position falls in is
    tinted; a click on a line seeks to its block's start. Save trace… writes the lines
    shown to a file, byte for byte the CLI's standard output.
  - The **Events** tab is the Numbo GUI's event log: every event one row, `index  name
    clock  field=value ...` (`models/event_log.py`'s `row_text`), painted only for the
    rows in view straight from the history; the Kinds menu hides or shows each name;
    the row of the event drawn now is tinted and kept in view (unless Follow is off), the
    rows after it are greyed; a double-click, or Enter on the cursor row (moved with the
    arrow keys, Page Up/Down, Home and End), seeks there. Save as JSON lines… writes the
    history as the CLI's `--json` output, byte for byte (`docs/trace-format.md`).

Both lists are `RowList`s, a `QAbstractScrollArea` painting only the rows in view straight
from their source: nothing is kept per row, so the 20,000th event of a capped run costs
what the first did (Numbo GUI, `log_view.py`). The rows in view are painted whole at every
event (one paint, G-042) rather than the two rows the Numbo GUI retints, so that the
window's paint-once rule holds whatever is in view. The pane only asks: `event_activated
(index)`, `save_trace_requested()` and `save_events_requested()` go to the window, which
seeks (refused while a run is live) or opens the file dialog. It keeps the view protocol
the window expects (G-011): `on_event`, `set_position`, `redraw(now=False)`, `paints`
(both lists', the hidden tab painting nothing), `events`, `pane_id`, `title`, `pages`.
"""

from pathlib import Path

from PySide6.QtCore import QRect, Qt, Signal
from PySide6.QtGui import (QAction, QColor, QFont, QFontDatabase, QFontMetrics, QImage, QPainter,
                           QPen)
from PySide6.QtWidgets import (QAbstractScrollArea, QCheckBox, QHBoxLayout, QLabel, QMenu,
                               QPushButton, QTabWidget, QToolButton, QVBoxLayout, QWidget)

from tabletop import events as ev
from tabletop.gui import paint
from tabletop.gui.models.event_log import NAMES, EventLog, json_lines, row_text
from tabletop.gui.models.trace import TraceModel

__all__ = ["BACKGROUND", "TEXT", "FUTURE", "NOW", "PAGES", "RowList", "EventList", "TraceList",
           "TraceTab", "EventsTab", "LogView"]

BACKGROUND = "#ffffff"
TEXT = "#1a1a1a"
FUTURE = "#9a9a9a"      # the rows after the event drawn now (Numbo GUI, `log_view.py`)
NOW = "#fbe3f0"         # the row of the event drawn now, the trace's block
CURSOR = "#e6007e"
PAD = 4
PAGES = "Numbo GUI, log_view.py"
MIN_SIZE = (200, 70)    # the window fits 1280 x 800 with every dock shown (G-014)


class RowList(QAbstractScrollArea):
    """Rows painted straight from a source (Numbo GUI, `log_view.py`'s `LogList`; G-042).
    A subclass gives `row_count`, `row_display(row)`, `is_future(row)`, `now_rows()`,
    `row_for(position)` and `index_of(row)`. `position` is the event drawn now,
    `cursor_row` the keyboard's row (None), `paints` how many times it was painted."""

    activated = Signal(int)     # an event index

    def __init__(self, parent=None):
        super().__init__(parent)
        self.position = -1
        self.cursor_row = None
        self.paints = 0
        self.painted_rect = QRect()
        font = QFontDatabase.systemFont(QFontDatabase.SystemFont.FixedFont)
        # Greyscale antialiasing: a greyed row reads as grey pixel by pixel, and a bare
        # list renders as the window's (G-038's lesson).
        font.setStyleStrategy(QFont.StyleStrategy.NoSubpixelAntialias)
        self.setFont(font)
        self.setHorizontalScrollBarPolicy(Qt.ScrollBarPolicy.ScrollBarAlwaysOff)
        self.setFocusPolicy(Qt.FocusPolicy.StrongFocus)
        self.viewport().setAttribute(Qt.WidgetAttribute.WA_OpaquePaintEvent)
        self._colors = {k: QColor(v) for k, v in
                        (("bg", BACKGROUND), ("text", TEXT), ("future", FUTURE), ("now", NOW))}
        self.metrics = QFontMetrics(self.font())
        self.line = self.metrics.height() + 2

    # -- the source (a subclass's) ------------------------------------------------------------

    def row_count(self) -> int:
        """How many rows there are (G-042)."""
        return 0

    def row_display(self, row: int) -> str:
        """The row's text (G-042)."""
        return ""

    def is_future(self, row: int) -> bool:
        """Whether the row is after the event drawn now (greyed) (G-042)."""
        return False

    def now_rows(self) -> set:
        """The rows tinted as the event drawn now (G-042)."""
        return set()

    def row_for(self, position: int) -> int | None:
        """The row the position falls in, kept in view (G-042)."""
        return None

    def index_of(self, row: int) -> int:
        """The event index a row seeks to (G-042)."""
        return -1

    # -- rows (Numbo GUI, `log_view.py`) -----------------------------------------------------

    def rows_in_view(self) -> int:
        """How many whole rows the viewport holds (Numbo GUI, `log_view.py`)."""
        return max(1, self.viewport().height() // self.line)

    def first_row(self) -> int:
        """The row at the top of the viewport (Numbo GUI, `log_view.py`)."""
        return self.verticalScrollBar().value()

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
        """The event drawn now: its row is brought into view (`follow`) and the rows in
        view are marked dirty, painted once by the window (Numbo GUI, `log_view.py`;
        G-042)."""
        self.position = position
        self.update_rows()
        row = self.row_for(position)
        if follow and row is not None:
            # A run's next event comes in at the bottom; a jump is centred.
            self.scroll_to(row, center=row != self.first_row() + self.rows_in_view())
        self.viewport().update()

    # -- painting (Numbo GUI, `log_view.py`) ------------------------------------------------

    def paintEvent(self, event):
        """Paint the rows of the dirty rectangle (Numbo GUI, `log_view.py`)."""
        self.paints += 1
        self.painted_rect = event.rect()
        p = QPainter(self.viewport())
        c = self._colors
        rect = event.rect()
        p.fillRect(rect, c["bg"])
        first = self.first_row()
        top = first + max(0, rect.top()) // self.line
        bottom = first + rect.bottom() // self.line + 1
        ascent = self.metrics.ascent() + 1
        now = self.now_rows()
        width = self.viewport().width()
        for row in range(top, min(bottom, self.row_count())):
            y = (row - first) * self.line
            if row in now:
                p.fillRect(0, y, width, self.line, c["now"])
            p.setPen(c["future"] if self.is_future(row) else c["text"])
            # Not elided: the viewport clips a long row, and its tooltip has all of it.
            p.drawText(PAD, y + ascent, self.row_display(row))
            if row == self.cursor_row:
                p.setPen(QPen(QColor(CURSOR), 1, Qt.PenStyle.DotLine))
                p.drawRect(0, y, width - 1, self.line - 1)
        p.end()

    def resizeEvent(self, event):
        """The rows in view changed: the scroll range (Numbo GUI, `log_view.py`)."""
        super().resizeEvent(event)
        self.update_rows()

    def scrollContentsBy(self, dx, dy):
        """Blit the rows that stay in view; Qt marks the strip exposed (Numbo GUI,
        `log_view.py`)."""
        self.viewport().scroll(0, dy * self.line)

    # -- the mouse and the keys (Numbo GUI, `log_view.py`) ----------------------------------

    def activate(self, row) -> None:
        """What a double-click on the row, or Enter on it, does: ask for a seek
        (Numbo GUI, `log_view.py`)."""
        if row is not None and 0 <= row < self.row_count():
            self.cursor_row = row
            self.activated.emit(self.index_of(row))

    def mousePressEvent(self, event):
        """A click moves the cursor row (Numbo GUI, `log_view.py`)."""
        row = self.row_at(event.position().y())
        if row is not None:
            self.cursor_row = row
            self.viewport().update()

    def mouseDoubleClickEvent(self, event):
        """A double-click activates the row (Numbo GUI, `log_view.py`)."""
        self.activate(self.row_at(event.position().y()))

    def event(self, e):
        """The tooltip is the row's whole text (Numbo GUI, `log_view.py`)."""
        if e.type() == e.Type.ToolTip:
            row = self.row_at(e.pos().y())
            self.setToolTip(self.row_display(row) if row is not None else "")
        return super().event(e)

    def keyPressEvent(self, event):
        """The arrow keys, Page Up/Down, Home and End move the cursor row; Enter
        activates it (Numbo GUI, `log_view.py`)."""
        n = self.row_count()
        if not n:
            return super().keyPressEvent(event)
        key = event.key()
        row = self.cursor_row
        if row is None:
            row = self.row_for(self.position) or 0
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


class EventList(RowList):
    """The Events tab's rows: the history's visible events (Numbo GUI, `log_view.py`'s
    `LogList`; G-042)."""

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.history = history

    def row_count(self) -> int:
        """The visible events (Numbo GUI, `log_view.py`)."""
        return len(self.history.visible)

    def row_display(self, row: int) -> str:
        """`index  name  clock  fields` (G-042)."""
        i = self.history.visible[row]
        return row_text(i, self.history.records[i])

    def is_future(self, row: int) -> bool:
        """After the event drawn now (Numbo GUI, `log_view.py`)."""
        return self.history.visible[row] > self.position

    def now_rows(self) -> set:
        """The row of the event drawn now, if it is shown (Numbo GUI, `log_view.py`)."""
        row = self.history.row_of(self.position)
        if row is not None and self.history.visible[row] == self.position:
            return {row}
        return set()

    def row_for(self, position: int) -> int | None:
        """The last shown event at or before the position (Numbo GUI, `log_view.py`)."""
        return self.history.row_of(position)

    def index_of(self, row: int) -> int:
        """The row's event (Numbo GUI, `log_view.py`)."""
        return self.history.visible[row]


class TraceList(RowList):
    """The Trace tab's rows: the `TraceModel`'s lines; a single click seeks to a line's
    block (PANES.md P10; G-042)."""

    def __init__(self, model, parent=None):
        super().__init__(parent)
        self.model = model

    def row_count(self) -> int:
        """The lines of the form chosen (G-042)."""
        return self.model.count

    def row_display(self, row: int) -> str:
        """The line as the CLI printed it (G-042)."""
        return self.model.lines[row].text

    def is_future(self, row: int) -> bool:
        """The line's block starts after the position (G-042)."""
        return self.model.is_future(row, self.position)

    def now_rows(self) -> set:
        """The lines of the block the position falls in (G-042)."""
        return set(self.model.block_rows(self.position))

    def row_for(self, position: int) -> int | None:
        """The last line whose block starts at or before the position (G-042)."""
        return self.model.line_of(position)

    def index_of(self, row: int) -> int:
        """The start of the line's block (G-042)."""
        return self.model.lines[row].start

    def mousePressEvent(self, event):
        """A click seeks to the line's block (PANES.md P10; G-042)."""
        row = self.row_at(event.position().y())
        if row is not None:
            self.activate(row)
            self.viewport().update()


class TraceTab(QWidget):
    """The Trace tab: the bar (Quiet, Follow, Save trace…, the line count) over the
    `TraceList` (G-042)."""

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.model = TraceModel(history)
        self.list = TraceList(self.model)
        self.quiet_box = QCheckBox("Quiet")
        self.quiet_box.setToolTip("the --quiet form: the first and the last lines only")
        self.quiet_box.toggled.connect(self.set_quiet)
        self.follow_box = QCheckBox("Follow")
        self.follow_box.setToolTip("keep the line of the event drawn now in view")
        self.follow_box.setChecked(True)
        self.save_button = QPushButton("Save trace…")
        self.save_button.setToolTip("write the lines shown to a file, as the CLI prints them")
        self.count_label = QLabel()
        self.count_label.setFixedWidth(self.count_label.fontMetrics().horizontalAdvance(
            "000000 lines") + 8)
        self.count_label.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        bar = QHBoxLayout()
        bar.setContentsMargins(0, 0, 0, 0)
        bar.addWidget(self.quiet_box)
        bar.addWidget(self.follow_box)
        bar.addWidget(self.save_button)
        bar.addStretch(1)
        bar.addWidget(self.count_label)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.addLayout(bar)
        layout.addWidget(self.list, 1)
        self.refresh()

    def set_quiet(self, quiet: bool) -> None:
        """Show the `--quiet` form or the normal one (G-042)."""
        self.model.set_quiet(quiet)
        if self.quiet_box.isChecked() != bool(quiet):
            self.quiet_box.setChecked(bool(quiet))
        self.list.cursor_row = None
        self.list.set_position(self.list.position, self.follow_box.isChecked())
        self.refresh()

    def set_position(self, index: int) -> None:
        """Sync the lines with the history and show the position (G-042)."""
        self.model.sync()
        self.model.set_position(index)
        self.list.set_position(index, self.follow_box.isChecked())

    def refresh(self) -> None:
        """The count label and the scroll range (G-042)."""
        self.list.update_rows()
        text = f"{self.model.count} lines"
        if self.count_label.text() != text:
            self.count_label.setText(text)


class EventsTab(QWidget):
    """The Events tab: the bar (Kinds, Follow, Save as JSON lines…, the counts) over the
    `EventList` (Numbo GUI, `log_view.py`'s `LogView`; G-042)."""

    def __init__(self, history, parent=None):
        super().__init__(parent)
        self.history = history
        self.list = EventList(history)
        self.kinds_button = QToolButton()
        self.kinds_button.setText("Kinds")
        self.kinds_button.setToolTip("the kinds of event shown")
        self.kinds_button.setPopupMode(QToolButton.ToolButtonPopupMode.InstantPopup)
        menu = QMenu(self.kinds_button)
        self.show_all_action = menu.addAction("Show all")
        self.hide_all_action = menu.addAction("Hide all")
        menu.addSeparator()
        self.kind_actions = {}
        for kind in NAMES:
            action = QAction(kind, menu)
            action.setCheckable(True)
            action.setChecked(kind not in history.hidden)
            action.toggled.connect(self._filter_changed)
            menu.addAction(action)
            self.kind_actions[kind] = action
        self.show_all_action.triggered.connect(lambda: self._check_all(True))
        self.hide_all_action.triggered.connect(lambda: self._check_all(False))
        self.kinds_button.setMenu(menu)
        self.follow_box = QCheckBox("Follow")
        self.follow_box.setToolTip("keep the event drawn now in view")
        self.follow_box.setChecked(True)
        self.save_button = QPushButton("Save as JSON lines…")
        self.save_button.setToolTip("write every event as the CLI's --json output")
        self.count_label = QLabel()
        self.count_label.setFixedWidth(self.count_label.fontMetrics().horizontalAdvance(
            "000000 of 000000 events") + 8)
        self.count_label.setAlignment(Qt.AlignmentFlag.AlignRight | Qt.AlignmentFlag.AlignVCenter)
        bar = QHBoxLayout()
        bar.setContentsMargins(0, 0, 0, 0)
        bar.addWidget(self.kinds_button)
        bar.addWidget(self.follow_box)
        bar.addWidget(self.save_button)
        bar.addStretch(1)
        bar.addWidget(self.count_label)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.addLayout(bar)
        layout.addWidget(self.list, 1)
        self._bulk = False
        self.refresh()

    def set_position(self, index: int) -> None:
        """Show the position (Numbo GUI, `log_view.py`)."""
        self.list.set_position(index, self.follow_box.isChecked())

    def refresh(self) -> None:
        """The count label and the scroll range (Numbo GUI, `log_view.py`)."""
        self.list.update_rows()
        text = f"{len(self.history.visible)} of {len(self.history)} events"
        if self.count_label.text() != text:
            self.count_label.setText(text)

    def hidden_kinds(self) -> frozenset:
        """The names whose rows are hidden (Numbo GUI, `log_view.py`)."""
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

    def _filter_changed(self):
        if self._bulk:
            return
        self.history.set_hidden(self.hidden_kinds())
        self.list.cursor_row = None
        self.refresh()
        self.list.set_position(self.list.position, self.follow_box.isChecked())


class LogView(QWidget):
    """The pane (see the module docstring; Numbo GUI, `log_view.py`; G-042): the Trace
    and the Events tabs; `attach(history, selection)`; the view protocol (G-011)."""

    event_activated = Signal(int)
    save_trace_requested = Signal()
    save_events_requested = Signal()

    def __init__(self, title="Trace and event log", pane_id="P10", pages=PAGES, parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.history = EventLog()
        self.position = -1
        self.events = 0
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")
        self.tabs = QTabWidget()
        self.tabs.setDocumentMode(True)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(2, 2, 2, 2)
        layout.addWidget(self.tabs)
        self._build_tabs()

    def _build_tabs(self):
        """The two tabs over the history (G-042)."""
        while self.tabs.count():
            widget = self.tabs.widget(0)
            self.tabs.removeTab(0)
            widget.deleteLater()
        self.trace = TraceTab(self.history)
        self.events_tab = EventsTab(self.history)
        self.tabs.addTab(self.trace, "Trace")
        self.tabs.addTab(self.events_tab, "Events")
        self.tabs.setTabToolTip(0, "the text trace as the CLI prints it; click a line to seek")
        self.tabs.setTabToolTip(1, "every event; double-click a row to seek")
        self.trace.list.activated.connect(self.event_activated)
        self.events_tab.list.activated.connect(self.event_activated)
        self.trace.save_button.clicked.connect(self.save_trace_requested)
        self.events_tab.save_button.clicked.connect(self.save_events_requested)

    def attach(self, history, selection=None) -> None:
        """Take the window's history: the tabs are built again over it (G-042). The
        selection is not used: the log has no element to select."""
        self.history = history
        self._build_tabs()

    @property
    def paints(self) -> int:
        """Both lists' paint counts (the hidden tab paints nothing; G-011)."""
        return self.trace.list.paints + self.events_tab.list.paints

    def set_position(self, index: int) -> None:
        """The index of the event about to be given: both tabs follow it (the tap
        recorded it already, G-006) (Numbo GUI, `log_view.py`)."""
        self.position = index
        self.trace.set_position(index)
        self.events_tab.set_position(index)

    def on_event(self, event) -> None:
        """Count the event; a `RunStarted` begins a new run, and when it is a new run's
        (not a seek's re-feed: the history then holds one event) the cursor rows are
        forgotten (G-011, G-042)."""
        if isinstance(event, ev.RunStarted):
            self.events = 0
            if len(self.history) <= 1:
                self.trace.list.cursor_row = None
                self.events_tab.list.cursor_row = None
        self.events += 1

    def redraw(self, now=False) -> None:
        """Refresh the counts; the lists were marked dirty by `set_position`, and with
        `now` they are painted at once (Numbo GUI, `log_view.py`)."""
        self.trace.refresh()
        self.events_tab.refresh()
        if now:
            paint.paint_all()

    def reload(self) -> None:
        """The history was replaced or cleared: both tabs start afresh (Numbo GUI,
        `log_view.py`)."""
        self.trace.model.clear()
        self.trace.list.cursor_row = None
        self.events_tab.list.cursor_row = None
        self.set_position(self.position)
        self.redraw(now=False)

    # -- the files (PANES.md P10) ---------------------------------------------------------

    def trace_text(self) -> str:
        """The trace shown, as the CLI prints it (G-042)."""
        self.trace.model.sync()
        return self.trace.model.text()

    def save_trace(self, path) -> Path:
        """Write the trace shown (normal or quiet) to the file; returns the path (PANES.md
        P10; G-042)."""
        path = Path(path)
        path.write_text(self.trace_text(), encoding="utf-8")
        return path

    def save_events(self, path) -> Path:
        """Write every event as JSON lines, byte for byte the CLI's `--json` output;
        returns the path (PANES.md P10; D-065; G-042)."""
        path = Path(path)
        path.write_text(json_lines(self.history), encoding="utf-8")
        return path

    def render_png(self, path, tab=0) -> bytes:
        """A fresh render of a tab's list viewport to a PNG file (RGB), for the tests
        (G-042)."""
        widget = (self.trace, self.events_tab)[tab].list.viewport()
        image = widget.grab().toImage().convertToFormat(QImage.Format.Format_RGB32)
        image.save(str(path))
        return Path(path).read_bytes()

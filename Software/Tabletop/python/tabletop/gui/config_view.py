"""P13 Configuration: the dock hidden by default that Edit > Configuration opens, every
field of `Config` as an editable table with the profile selector, a view of
`models/config_table.py` (PANES.md P13; ANSWERS.md 3; Seek-Whence GUI, `config_view.py`;
G-046, G-047).

A run is its problem, its seed and its `Config` (D-064, D-066), so the pane says along
its top that the values shown are the chosen profile's and that an edit applies to the
next run, never to the one in progress. One row per field: the field, its value in a
widget suited to its kind (a spin box for an integer, a decimal spin box for a number, a
check box for a flag, a line of text for a name or a table's literal, with an editor
under the table for the long ones), the profile's own value, the source (the page tags
and decision numbers of the field's comment in `config.py`, the decisions' headings from
DECISIONS.md on hover), the marks (French's faults, `FAULT_KNOBS`, fixed in `standard`
and restored in `french-faults`; the calibrated knobs, D-076; where the profile differs
from `standard`), the comment, and a Reset button; Reset all beside the filter. Edited
rows are tinted. The `profile` field is set by the selector, which is one state with the
controls pane's (the window keeps it, G-044). During a replay the pane shows the file's
configuration read-only (`set_replay`, the window's replay of a session file, G-049). The pane is a view of the
window's protocol: at `RunStarted` it notes the configuration the run in progress uses,
as the stats pane says it (G-045), and the status line along the top is what it repaints
per event (G-011).
"""

from PySide6.QtCore import QRect, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QPainter
from PySide6.QtWidgets import (QAbstractItemView, QCheckBox, QComboBox, QDoubleSpinBox,
                               QHBoxLayout, QHeaderView, QLabel, QLineEdit, QPlainTextEdit,
                               QPushButton, QSizePolicy, QSpinBox, QTableWidget,
                               QTableWidgetItem, QVBoxLayout, QWidget)

from tabletop import events as ev
from tabletop.config import PROFILES
from tabletop.gui import paint
from tabletop.gui.models import config_table as ct
from tabletop.gui.models.run_stats import config_note

__all__ = ["COLUMNS", "MIN_SIZE", "ConfigView", "StatusLine", "elided"]

COLUMNS = ("Field", "Value", "Profile", "Source", "Marks", "Comment", "")
FIELD_COL, VALUE_COL, PROFILE_COL, SOURCE_COL, MARKS_COL, COMMENT_COL, RESET_COL = range(7)
EDITED_TINT = QColor(255, 236, 179)        # an edited row (ANSWERS.md 3)
DIFFERS_TINT = QColor(222, 235, 250)       # the profile differs from standard here
FAULT_TINT = QColor(250, 226, 226)         # one of French's faults (FAULT_KNOBS)
FIXED_TINT = QColor(232, 232, 232)         # the profile field, set by the selector
INK, MUTED, EDITED = QColor("#1a1a1a"), QColor("#6b6b6b"), QColor("#a63d00")
MIN_SIZE = (120, 60)                       # the smallest the pane may be (G-014)
INT_RANGE = 1_000_000_000
FLOAT_RANGE = 1_000_000.0
DECIMALS = 4
ELIDE = 60


def elided(text: str, width: int = ELIDE) -> str:
    """One line of at most `width` characters, `…` at the end when cut (G-047)."""
    line = " ".join(text.split())
    return line if len(line) <= width else line[:width - 1] + "…"


class StatusLine(QWidget):
    """The thin strip along the top of the pane: the next run's configuration as the
    stats pane will say it, the edits, and the run in progress; repainted once per
    event (G-011, G-047)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.next_text = ""
        self.run_note = ""
        self.paints = 0
        self.setMinimumHeight(16)
        self.setMaximumHeight(16)
        self.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)

    def text(self) -> str:
        """`next run: standard (the defaults) · run in progress: human: type_weight=50`
        (G-047)."""
        parts = [f"next run: {self.next_text}"] if self.next_text else []
        if self.run_note:
            parts.append(f"run in progress: {self.run_note}")
        return " · ".join(parts)

    def paintEvent(self, event):
        """Paint the line; `paints` counts the calls for the window's probe (G-011)."""
        self.paints += 1
        painter = QPainter(self)
        font = QFont(painter.font())
        font.setPointSizeF(8.0)
        painter.setFont(font)
        painter.setPen(EDITED if "edited:" in self.next_text else MUTED)
        painter.drawText(QRect(2, 0, self.width() - 4, self.height()),
                         Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                         painter.fontMetrics().elidedText(self.text(), Qt.TextElideMode.ElideRight,
                                                          self.width() - 4))
        painter.end()


class ConfigView(QWidget):
    """The configuration dock's view (see the module docstring; G-046, G-047). Signals:
    `edits_changed()` after the user changed, reset or applied an edit (the window
    rebuilds the next run's configuration then); `profile_changed(name)` when the
    selector is used (the window answers through `set_next_run`, so the controls agree).
    `show_next_run(profile)` shows the chosen profile's values with the edits;
    `set_replay(config)` a replay's, read-only, None for live runs."""

    pane_id = "P13"
    edits_changed = Signal()
    profile_changed = Signal(str)

    def __init__(self, title="Configuration", pane_id="P13", pages="", parent=None,
                 model=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.model = model if model is not None else ct.ConfigTable()
        self.replay = None              # a replay's Config, or None
        self.events = 0
        self.rows = ()
        self._widgets = {}              # field name -> its value widget
        self._resets = {}               # field name -> its Reset button
        self._items = {}                # field name -> its QTableWidgetItems by column
        self._selected = None
        self.status = StatusLine()
        self._build_top()
        self._build_table()
        self._build_editor()
        self._lay_out()
        self._build_rows()
        self.refresh()
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")

    # -- building (G-047) ---------------------------------------------------------------------

    def _build_top(self):
        """The note, the profile selector, the filter and Reset all (ANSWERS.md 3)."""
        self.note_label = QLabel("The values are the chosen profile's (D-066); edits apply to "
                                 "the next run, never to a run in progress.")
        self.note_label.setWordWrap(True)
        self.note_label.setObjectName("config-note")
        self.note_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self.profile_combo = QComboBox()
        for name in PROFILES:
            self.profile_combo.addItem(name, name)
        self.profile_combo.setToolTip("the profile whose values are shown and edited (D-066); "
                                      "one state with the Controls pane's selector")
        self.profile_combo.setMinimumContentsLength(6)
        self.profile_combo.setSizeAdjustPolicy(
            QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
        self.profile_combo.currentIndexChanged.connect(self._profile_chosen)
        self.filter_edit = QLineEdit()
        self.filter_edit.setPlaceholderText("filter by name, section, source, marks or comment")
        self.filter_edit.setClearButtonEnabled(True)
        self.filter_edit.setMinimumWidth(40)
        self.filter_edit.textChanged.connect(self._filter)
        self.reset_all_button = QPushButton("Reset all")
        self.reset_all_button.setToolTip("every field back to the profile's value")
        self.reset_all_button.clicked.connect(self.reset_all)
        self.top = QHBoxLayout()
        self.top.addWidget(QLabel("Profile"))
        self.top.addWidget(self.profile_combo)
        self.top.addWidget(self.filter_edit, 1)
        self.top.addWidget(self.reset_all_button)

    def _build_table(self):
        """The table: one row per field, the columns of `COLUMNS` (PANES.md P13)."""
        self.table = QTableWidget(0, len(COLUMNS))
        self.table.setMinimumSize(*MIN_SIZE)
        self.table.setHorizontalHeaderLabels(COLUMNS)
        self.table.verticalHeader().setVisible(False)
        self.table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.table.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        header = self.table.horizontalHeader()
        header.setMinimumSectionSize(20)
        for col in (FIELD_COL, SOURCE_COL, RESET_COL):
            header.setSectionResizeMode(col, QHeaderView.ResizeMode.ResizeToContents)
        for col in (VALUE_COL, PROFILE_COL, MARKS_COL):
            header.setSectionResizeMode(col, QHeaderView.ResizeMode.Interactive)
        header.setSectionResizeMode(COMMENT_COL, QHeaderView.ResizeMode.Stretch)
        self.table.setColumnWidth(VALUE_COL, 130)
        self.table.setColumnWidth(PROFILE_COL, 90)
        self.table.setColumnWidth(MARKS_COL, 150)
        self.table.itemSelectionChanged.connect(self._selection_changed)

    def _build_editor(self):
        """The editor under the table for the selected field, Apply and Reset, and the
        error label (Seek-Whence GUI, `config_view.py`)."""
        self.editor_label = QLabel()
        self.editor_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self.editor = QPlainTextEdit()
        self.editor.setMinimumSize(40, 24)
        self.editor.setMaximumHeight(90)
        self.editor.setPlaceholderText("select a field: its value, a table one entry per line")
        self.apply_button = QPushButton("Apply")
        self.apply_button.setToolTip("the text as the field's value, checked against its type")
        self.apply_button.clicked.connect(self.apply_editor)
        self.editor_reset_button = QPushButton("Reset")
        self.editor_reset_button.clicked.connect(self._reset_selected)
        self.editor_buttons = QHBoxLayout()
        self.editor_buttons.addWidget(self.apply_button)
        self.editor_buttons.addWidget(self.editor_reset_button)
        self.editor_buttons.addStretch(1)
        self.error_label = QLabel()
        self.error_label.setObjectName("config-error")
        self.error_label.setWordWrap(True)
        self.error_label.setStyleSheet("color: #b00020;")
        self.error_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self.error_label.hide()

    def _lay_out(self):
        """The status line along the top, then the note, the top row, the table, the
        editor and the error (G-014: small minimums everywhere)."""
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(2)
        layout.addWidget(self.status)
        layout.addWidget(self.note_label)
        layout.addLayout(self.top)
        layout.addWidget(self.table, 1)
        layout.addWidget(self.editor_label)
        layout.addWidget(self.editor)
        layout.addLayout(self.editor_buttons)
        layout.addWidget(self.error_label)

    def _build_rows(self):
        """One row per field with its widgets, built once (G-047)."""
        rows = self.model.rows()
        self.table.setRowCount(len(rows))
        for r, row in enumerate(rows):
            name = row.name
            items = {}
            for c in (FIELD_COL, PROFILE_COL, SOURCE_COL, MARKS_COL, COMMENT_COL):
                item = QTableWidgetItem()
                item.setFlags(Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled)
                self.table.setItem(r, c, item)
                items[c] = item
            items[FIELD_COL].setText(name)
            items[SOURCE_COL].setText(row.source_text)
            items[MARKS_COL].setText(row.marks_text)
            items[COMMENT_COL].setText(elided(row.note.comment, 200))
            self._items[name] = items
            widget = self._value_widget(row)
            self._widgets[name] = widget
            self.table.setCellWidget(r, VALUE_COL, widget)
            reset = QPushButton("Reset")
            reset.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            reset.clicked.connect(lambda _checked=False, n=name: self.reset(n))
            self._resets[name] = reset
            self.table.setCellWidget(r, RESET_COL, reset)

    def _value_widget(self, row):
        """The widget for the field's kind (ANSWERS.md 3: widgets by type; G-047)."""
        kind, name = row.note.kind, row.name
        if kind == "int":
            w = QSpinBox()
            w.setRange(-INT_RANGE, INT_RANGE)
            w.editingFinished.connect(lambda n=name: self._edit(n, str(self._widgets[n].value())))
        elif kind == "float":
            w = QDoubleSpinBox()
            w.setRange(-FLOAT_RANGE, FLOAT_RANGE)
            w.setDecimals(DECIMALS)
            w.setSingleStep(0.1)
            w.editingFinished.connect(lambda n=name: self._edit(n, repr(self._widgets[n].value())))
        elif kind == "bool":
            w = QCheckBox()
            w.toggled.connect(lambda checked, n=name: self._edit(n, "true" if checked else "false"))
        else:
            w = QLineEdit()
            w.editingFinished.connect(lambda n=name: self._edit(n, self._widgets[n].text()))
        w.setFocusPolicy(Qt.FocusPolicy.ClickFocus)
        return w

    def sizeHint(self):
        """Enough for the table's first columns (G-011)."""
        return QSize(400, 300)

    # -- showing (G-047) ----------------------------------------------------------------------

    def show_next_run(self, profile: str) -> None:
        """Show the profile the window chose, with the edits over it (ANSWERS.md 3:
        the selector is one state with the controls'; G-047); emits nothing."""
        self.model.set_profile(profile)
        self.refresh()

    def set_replay(self, config) -> None:
        """A replay's configuration, shown read-only (its session file's, restored;
        G-049); None goes back to the edits and live runs (G-047)."""
        self.replay = config
        self.refresh()

    def refresh(self) -> None:
        """Take the rows from the model and show them, no signal emitted (G-047)."""
        self.rows = self.model.rows(self.replay)
        editable = self.replay is None
        self.profile_combo.blockSignals(True)
        try:
            profile = self.replay.profile if self.replay is not None else self.model.profile
            self.profile_combo.setCurrentIndex(max(0, self.profile_combo.findData(profile)))
        finally:
            self.profile_combo.blockSignals(False)
        self.profile_combo.setEnabled(editable)
        for row in self.rows:
            self._refresh_row(row, editable)
        self.reset_all_button.setEnabled(editable and bool(self.model.edited_names))
        self.apply_button.setEnabled(editable)
        self.editor_reset_button.setEnabled(editable)
        self._show_status()
        self._show_editor()
        self._filter(self.filter_edit.text())
        self.update()

    def _refresh_row(self, row, editable):
        """One row's widget, Reset button, texts, tooltips and tints (G-047)."""
        name = row.name
        widget, items = self._widgets[name], self._items[name]
        widget.blockSignals(True)
        try:
            self._show_value(widget, row)
        finally:
            widget.blockSignals(False)
        widget.setEnabled(editable and not row.fixed)
        self._resets[name].setEnabled(editable and row.edited and not row.fixed)
        items[PROFILE_COL].setText(elided(ct.value_text(row.profile_value)))
        tip = ct.row_tooltip(row, self.model.titles)
        for item in items.values():
            item.setToolTip(tip)
        widget.setToolTip(tip if not row.fixed else f"{name}: set by the profile selector")
        items[SOURCE_COL].setToolTip("\n".join(
            [*row.note.tags, *(self.model.title_of(d) for d in row.note.decisions)]))
        base = FIXED_TINT if row.fixed else QColor(Qt.GlobalColor.transparent)
        for col, item in items.items():
            tint = base
            if row.edited:
                tint = EDITED_TINT
            elif col == PROFILE_COL and row.differs:
                tint = DIFFERS_TINT
            elif col == MARKS_COL and row.fault is not None:
                tint = FAULT_TINT
            item.setBackground(tint)
        font = items[FIELD_COL].font()
        font.setBold(row.edited)
        items[FIELD_COL].setFont(font)

    def _show_value(self, widget, row):
        if isinstance(widget, QSpinBox):
            widget.setValue(int(row.value))
        elif isinstance(widget, QDoubleSpinBox):
            widget.setValue(float(row.value))
        elif isinstance(widget, QCheckBox):
            widget.setChecked(bool(row.value))
            widget.setText("true" if row.value else "false")
        else:
            widget.setText(" ".join(ct.value_text(row.value).split()))

    def _show_status(self):
        if self.replay is not None:
            self.status.next_text = (f"replay: the file's configuration, "
                                     f"{ct.config_note_of(self.replay)} (read-only)")
        else:
            text = ct.config_note_of(self.model.config())
            edited = self.model.edited_text()
            self.status.next_text = f"{text} · {edited}" if edited else text
        self.status.update()

    def _show_editor(self):
        row = self.row_of(self._selected) if self._selected else None
        if row is None:
            self.editor.setPlainText("")
            self.editor_label.setText("Value of the selected field:")
            self.editor.setReadOnly(True)
            return
        self.editor_label.setText(f"{row.name}: {elided(row.note.comment, 120)}")
        self.editor.setPlainText(ct.value_text(row.value))
        self.editor.setReadOnly(self.replay is not None or row.fixed)

    def _filter(self, text):
        """Show the rows whose name, section, source, marks or comment contains the
        text (PANES.md P13)."""
        kept = {row.name for row in ct.filter_rows(self.rows, text)}
        for r, row in enumerate(self.rows):
            self.table.setRowHidden(r, row.name not in kept)

    def _selection_changed(self):
        rows = self.table.selectionModel().selectedRows()
        self._selected = self.rows[rows[0].row()].name if rows else None
        self.error_label.hide()
        self._show_editor()

    def _profile_chosen(self):
        """The selector: tell the window, which sets the next run's profile and shows
        it back here (ANSWERS.md 3; G-044)."""
        name = self.profile_combo.currentData()
        if name and self.replay is None:
            self.profile_changed.emit(name)

    # -- editing (ANSWERS.md 3) ---------------------------------------------------------------

    def row_of(self, name: str):
        """The `FieldRow` of a field as last shown, or None (G-047)."""
        for row in self.rows:
            if row.name == name:
                return row
        return None

    def widget_of(self, name: str):
        """The value widget of a field (the tests type into it; G-047)."""
        return self._widgets[name]

    def visible_names(self) -> tuple:
        """The fields whose rows pass the filter (G-047)."""
        return tuple(row.name for r, row in enumerate(self.rows) if not self.table.isRowHidden(r))

    def edit(self, name: str, text: str) -> bool:
        """Edit a field from a text, as typing it in its widget does: the model checks it
        (`ConfigError` shown under the editor, nothing changed) and `edits_changed` is
        emitted when the edits changed (ANSWERS.md 3; G-046)."""
        return self._edit(name, text)

    def _edit(self, name, text) -> bool:
        if self.replay is not None:
            self.show_error("replaying: the configuration is the file's")
            self.refresh()
            return False
        try:
            changed = self.model.set_text(name, text)
        except ct.ConfigError as error:
            self.show_error(f"{name}: {error}")
            self.refresh()
            return False
        self.show_error("")
        if changed:
            self.edits_changed.emit()
        self.refresh()
        return changed

    def apply_editor(self) -> bool:
        """The Apply button: the editor's text as the selected field's value (G-047)."""
        if self._selected is None:
            self.show_error("select a field first")
            return False
        return self._edit(self._selected, self.editor.toPlainText())

    def select(self, name: str) -> None:
        """Select a field's row: its value fills the editor (G-047)."""
        for r, row in enumerate(self.rows):
            if row.name == name:
                self.table.selectRow(r)
                return

    def reset(self, name: str) -> bool:
        """A row's Reset: the field back to the profile's value (ANSWERS.md 3; G-047)."""
        if self.replay is not None:
            return False
        changed = self.model.reset(name)
        self.show_error("")
        if changed:
            self.edits_changed.emit()
        self.refresh()
        return changed

    def _reset_selected(self):
        if self._selected is not None:
            self.reset(self._selected)

    def reset_all(self) -> bool:
        """Reset all: every field back to the profile's value (ANSWERS.md 3; G-047)."""
        if self.replay is not None:
            return False
        changed = self.model.reset_all()
        self.show_error("")
        if changed:
            self.edits_changed.emit()
        self.refresh()
        return changed

    def show_error(self, message: str) -> None:
        """`message` under the editor ("" hides it) (Numbo GUI, `controls.py`)."""
        self.error_label.setText(message)
        self.error_label.setVisible(bool(message))

    # -- the view protocol (G-011) ------------------------------------------------------------

    @property
    def paints(self) -> int:
        """The status line's paints, the one thing the pane repaints per event (G-011)."""
        return self.status.paints

    def on_event(self, event) -> None:
        """At `RunStarted`, note the configuration the run in progress uses, as the stats
        pane says it (`config_note`, G-045); count the events (G-011)."""
        if isinstance(event, ev.RunStarted):
            self.events = 0
            self.status.run_note = config_note(event.profile, event.changed)
        self.events += 1

    def redraw(self, now=False) -> None:
        """Mark the status line dirty; with `now`, paint at once (Numbo GUI, `paint.py`)."""
        self.status.update()
        if now:
            paint.paint_all()

    def render_png(self, path):
        """Grab the pane to a PNG at `path` (Numbo GUI, `coderack_view.py`)."""
        image = self.grab()
        if not image.save(str(path)):
            raise OSError(f"could not write {path}")
        return image

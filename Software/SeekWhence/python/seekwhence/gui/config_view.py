"""The configuration pane (P13), a dock hidden by default and opened by Edit >
Configuration: every field of `Config` as an editable table, a view of
`models/config_table.py` (PANES.md P13; ANSWERS.md 5; G-050, G-051).

A run is its seed and its configuration (DECISION D-069), so the pane says at its
top that an edit applies to the next run and never to the one in progress. One row per
field: the field, its value in a widget suited to its type (a spin box for an integer, a
decimal spin box for a number, a check box for a flag, a line of Python literal for a
table, with the editor under the table for the long ones), its default, its source (the
page tags and decision numbers of the field's comment in `config.py`, the decisions'
headings from DECISIONS.md on hover), the comment, and a Reset button; Reset all under
the filter. Edited fields are tinted. The three fields the controls pane sets (the caps,
DECISION D-011, and the trace, DECISION D-055) are shown and not editable here (G-051).
During a replay the pane shows the file's configuration, read-only (G-047). The pane is
a view of the window's protocol: at a `start` event it notes the configuration the run
in progress uses, `default` or `edited: ...` as the stats pane says it (G-016).
"""

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QColor
from PySide6.QtWidgets import (QAbstractItemView, QCheckBox, QDoubleSpinBox, QHBoxLayout,
                               QHeaderView, QLabel, QLineEdit, QPlainTextEdit, QPushButton,
                               QSpinBox, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget)

from seekwhence.gui import paint
from seekwhence.gui.taskrack_view import LIST_MIN
from seekwhence.gui.models import config_table as ct
from seekwhence.gui.models.run_stats import config_note
from seekwhence.gui.models.session_file import config_pairs

__all__ = ["ConfigView", "COLUMNS"]

PAGES = "ANSWERS.md 5; PANES.md P13; D-069"
COLUMNS = ("Field", "Value", "Default", "Source", "Comment", "")
EDITED_TINT = QColor(255, 236, 179)
CONTROLLED_TINT = QColor(232, 232, 232)
INT_RANGE = 1_000_000_000
FLOAT_RANGE = 1_000_000.0
DECIMALS = 4
ELIDE = 60


def elided(text: str, width: int = ELIDE) -> str:
    """One line of at most `width` characters, `…` at the end when cut (G-051)."""
    line = " ".join(text.split())
    return line if len(line) <= width else line[:width - 1] + "…"


class ConfigView(QWidget):
    """The configuration dock's view (see the module docstring; G-050, G-051). Signals:
    `edits_changed()` after the user changed, reset or applied an edit (the window
    rebuilds the next run's inputs then). `show_config(config)` shows the next run's
    configuration; `set_replay(config)` a replay's, read-only, None for live runs."""

    pane_id = "P13"
    title = "Configuration"
    pages = PAGES
    edits_changed = Signal()

    def __init__(self, model=None, parent=None):
        super().__init__(parent)
        self.model = model if model is not None else ct.ConfigTable()
        self.shown = None           # the next run's Config, or None for the edits alone
        self.replay = None          # a replay's Config, or None
        self.run_note = ""          # the run in progress: default or edited: ...
        self.paints = 0
        self.events = 0
        self.rows = ()
        self._widgets = {}          # field name -> its value widget
        self._resets = {}           # field name -> its Reset button
        self._items = {}            # field name -> its QTableWidgetItems by column
        self._selected = None

        self.note_label = QLabel("Edits apply to the next run; a run in progress is never "
                                 "changed: a run is its seed and its configuration.")
        self.note_label.setWordWrap(True)
        self.note_label.setObjectName("config-note")
        self.status_label = QLabel()
        self.status_label.setObjectName("config-status")
        self.status_label.setWordWrap(True)
        self.filter_edit = QLineEdit()
        self.filter_edit.setPlaceholderText("filter by name, section, source or comment")
        self.filter_edit.setClearButtonEnabled(True)
        self.reset_all_button = QPushButton("Reset all")
        self.reset_all_button.setToolTip("every field back to its default: Config()")
        top = QHBoxLayout()
        top.addWidget(self.filter_edit, 1)
        top.addWidget(self.reset_all_button)

        self.table = QTableWidget(0, len(COLUMNS))
        self.table.setMinimumSize(*LIST_MIN)         # a dock can be short (G-058)
        self.table.setHorizontalHeaderLabels(COLUMNS)
        self.table.verticalHeader().setVisible(False)
        self.table.setSelectionBehavior(QAbstractItemView.SelectionBehavior.SelectRows)
        self.table.setSelectionMode(QAbstractItemView.SelectionMode.SingleSelection)
        self.table.setEditTriggers(QAbstractItemView.EditTrigger.NoEditTriggers)
        header = self.table.horizontalHeader()
        header.setSectionResizeMode(0, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(1, QHeaderView.ResizeMode.Interactive)
        header.setSectionResizeMode(2, QHeaderView.ResizeMode.Interactive)
        header.setSectionResizeMode(3, QHeaderView.ResizeMode.ResizeToContents)
        header.setSectionResizeMode(4, QHeaderView.ResizeMode.Stretch)
        header.setSectionResizeMode(5, QHeaderView.ResizeMode.ResizeToContents)
        self.table.setColumnWidth(1, 150)
        self.table.setColumnWidth(2, 110)

        self.editor_label = QLabel("Value of the selected field (a table one entry per line):")
        self.editor_label.setMinimumWidth(1)         # clipped when narrow (G-058)
        self.editor = QPlainTextEdit()
        self.editor.setMinimumSize(*LIST_MIN)        # a dock can be short (G-058)
        self.editor.setPlaceholderText("select a field")
        self.editor.setMaximumHeight(110)
        self.apply_button = QPushButton("Apply")
        self.apply_button.setToolTip("the text as the field's value, checked against its type")
        self.editor_reset_button = QPushButton("Reset")
        editor_buttons = QHBoxLayout()
        editor_buttons.addWidget(self.apply_button)
        editor_buttons.addWidget(self.editor_reset_button)
        editor_buttons.addStretch(1)
        self.error_label = QLabel()
        self.error_label.setObjectName("config-error")
        self.error_label.setWordWrap(True)
        self.error_label.setStyleSheet("color: #b00020;")
        self.error_label.hide()

        layout = QVBoxLayout(self)
        layout.addWidget(self.note_label)
        layout.addWidget(self.status_label)
        layout.addLayout(top)
        layout.addWidget(self.table, 1)
        layout.addWidget(self.editor_label)
        layout.addWidget(self.editor)
        layout.addLayout(editor_buttons)
        layout.addWidget(self.error_label)

        self.filter_edit.textChanged.connect(self._filter)
        self.reset_all_button.clicked.connect(self.reset_all)
        self.apply_button.clicked.connect(self.apply_editor)
        self.editor_reset_button.clicked.connect(self._reset_selected)
        self.table.itemSelectionChanged.connect(self._selection_changed)
        self._build_rows()
        self.refresh()

    # -- building ----------------------------------------------------------------------------

    def _build_rows(self):
        """One row per field with its widgets, built once (G-051)."""
        rows = self.model.rows()
        self.table.setRowCount(len(rows))
        for r, row in enumerate(rows):
            name = row.name
            items = {}
            for c in (0, 2, 3, 4):
                item = QTableWidgetItem()
                item.setFlags(Qt.ItemFlag.ItemIsSelectable | Qt.ItemFlag.ItemIsEnabled)
                self.table.setItem(r, c, item)
                items[c] = item
            items[0].setText(name)
            items[3].setText(row.source_text)
            items[4].setText(elided(row.note.comment, 200))
            self._items[name] = items
            widget = self._value_widget(row)
            self._widgets[name] = widget
            self.table.setCellWidget(r, 1, widget)
            reset = QPushButton("Reset")
            reset.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            reset.clicked.connect(lambda _checked=False, n=name: self.reset(n))
            self._resets[name] = reset
            self.table.setCellWidget(r, 5, reset)

    def _value_widget(self, row):
        """The widget for the field's kind (ANSWERS.md 5: a widget suited to each
        type; G-051)."""
        kind = row.note.kind
        name = row.name
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

    # -- showing -----------------------------------------------------------------------------

    def show_config(self, config) -> None:
        """Show the next run's configuration: the values, which rows are edited, the
        status line (ANSWERS.md 5; G-051)."""
        self.shown = config
        self.refresh()

    def set_replay(self, config) -> None:
        """A replay's configuration, shown read-only (its session file's, restored;
        G-047); None goes back to the edits and live runs."""
        self.replay = config
        self.refresh()

    def refresh(self) -> None:
        """Take the rows from the model and show them, no signal emitted (G-051)."""
        config = self.replay if self.replay is not None else self.shown
        self.rows = self.model.rows(config)
        editable = self.replay is None
        for r, row in enumerate(self.rows):
            name = row.name
            widget = self._widgets[name]
            items = self._items[name]
            widget.blockSignals(True)
            try:
                self._show_value(widget, row)
            finally:
                widget.blockSignals(False)
            widget.setEnabled(editable and not row.controlled)
            self._resets[name].setEnabled(editable and row.edited and not row.controlled)
            items[2].setText(elided(ct.value_text(row.note.default)))
            tip = ct.row_tooltip(row, self.model.titles)
            for item in items.values():
                item.setToolTip(tip)
            widget.setToolTip(tip if not row.controlled else
                              f"{name}: set in the Controls pane (P11)")
            items[3].setToolTip("\n".join(
                [*row.note.tags, *(self.model.title_of(d) for d in row.note.decisions)]))
            tint = (EDITED_TINT if row.edited else CONTROLLED_TINT if row.controlled
                    else QColor(Qt.GlobalColor.transparent))
            for item in items.values():
                item.setBackground(tint)
            font = items[0].font()
            font.setBold(row.edited)
            items[0].setFont(font)
        self.reset_all_button.setEnabled(editable and bool(self.model.edits))
        self.apply_button.setEnabled(editable)
        self.editor_reset_button.setEnabled(editable)
        self._show_status()
        self._show_editor()
        self._filter(self.filter_edit.text())
        self.update()

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
            text = f"replay: the file's configuration, {self._note_of(self.replay)} (read-only)"
        else:
            config = self.shown if self.shown is not None else self.model.config()
            text = f"next run: {self._note_of(config)}"
        if self.run_note:
            text += f"\nrun in progress: {self.run_note}"
        self.status_label.setText(text)

    @staticmethod
    def _note_of(config) -> str:
        return config_note(config_pairs(config))

    def _show_editor(self):
        row = self.row_of(self._selected) if self._selected else None
        if row is None:
            self.editor.setPlainText("")
            self.editor_label.setText("Value of the selected field (a table one entry per line):")
            return
        self.editor_label.setText(f"{row.name}: {elided(row.note.comment, 120)}")
        self.editor.setPlainText(ct.value_text(row.value))
        self.editor.setReadOnly(self.replay is not None or row.controlled)

    def _filter(self, text):
        """Show the rows whose name, section, source or comment contains the text
        (PANES.md P13)."""
        kept = {row.name for row in ct.filter_rows(self.rows, text)}
        for r, row in enumerate(self.rows):
            self.table.setRowHidden(r, row.name not in kept)

    def _selection_changed(self):
        rows = self.table.selectionModel().selectedRows()
        self._selected = self.rows[rows[0].row()].name if rows else None
        self.error_label.hide()
        self._show_editor()

    # -- editing (ANSWERS.md 5) --------------------------------------------------------------

    def row_of(self, name: str):
        """The `FieldRow` of a field as last shown, or None (G-051)."""
        for row in self.rows:
            if row.name == name:
                return row
        return None

    def widget_of(self, name: str):
        """The value widget of a field (the tests type into it; G-051)."""
        return self._widgets[name]

    def visible_names(self) -> tuple:
        """The fields whose rows pass the filter (G-051)."""
        return tuple(row.name for r, row in enumerate(self.rows) if not self.table.isRowHidden(r))

    def edit(self, name: str, text: str) -> bool:
        """Edit a field from a text, as typing it in its widget does: the model checks
        it (`ConfigError` shown under the editor, nothing changed) and `edits_changed`
        is emitted when the edits changed (ANSWERS.md 5; G-050)."""
        return self._edit(name, text)

    def _edit(self, name, text) -> bool:
        if self.replay is not None:
            self.show_error(f"replaying: the configuration is the file's")
            self.refresh()
            return False
        try:
            changed = self.model.set_text(name, text)
        except ct.ConfigError as error:
            self.show_error(f"{name}: {error}")
            return False
        self.show_error("")
        if changed:
            self.edits_changed.emit()
        self.refresh()
        return changed

    def apply_editor(self) -> bool:
        """The Apply button: the editor's text as the selected field's value (G-051)."""
        if self._selected is None:
            self.show_error("select a field first")
            return False
        return self._edit(self._selected, self.editor.toPlainText())

    def select(self, name: str) -> None:
        """Select a field's row: its value fills the editor (G-051)."""
        for r, row in enumerate(self.rows):
            if row.name == name:
                self.table.selectRow(r)
                return

    def reset(self, name: str) -> bool:
        """A row's Reset: the field back to its default (ANSWERS.md 5; G-051)."""
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
        """Reset all: every field back to its default, `Config()` (ANSWERS.md 5; G-051)."""
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

    # -- the view protocol (G-011) ----------------------------------------------------------

    def on_event(self, event) -> None:
        """At a `start` event, note the configuration the run in progress uses
        (`RunStarted.config`, PANES.md P13); count the events (G-011)."""
        if event.kind == "start":
            self.events = 0
            self.run_note = config_note(event.config)
            self._show_status()
        self.events += 1

    def redraw(self, now=False) -> None:
        """Mark the view dirty; with `now`, paint at once (Numbo GUI, `paint.py`)."""
        self.update()
        if now:
            paint.paint_all()

    def paintEvent(self, event):
        """Count the paints (Numbo GUI, `tree_view.py`'s `paints`; G-011)."""
        self.paints += 1
        super().paintEvent(event)

    def render_png(self, path):
        """Grab the pane to a PNG at `path` (Numbo GUI, `coderack_view.py`)."""
        image = self.grab()
        if not image.save(path):
            raise OSError(f"could not write {path}")
        return image

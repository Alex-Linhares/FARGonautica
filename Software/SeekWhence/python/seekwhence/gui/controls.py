"""The controls pane (P11): the preset list with each entry's citation and Meredith's
reported result, the free terms, the seed, the two caps, the answering mode, the trace,
the run buttons and the speed slider, a view of `models/controls.py` (PANES.md P11;
Numbo GUI, `controls.py`, adopted; G-048, G-049).

As Numbo's, the pane only holds the widgets, reads the inputs (`inputs()`, through the
model's `run_inputs`) and shows input errors; the window connects the buttons and the
slider. The presets are the three appendix runs with Meredith's console inputs, commands
and answers [M86 pp. 164-179], `Custom` for terms of one's own ("a user presents to the
system numbers" [M86 p. 37]), the Blackburn dozen [M86 pp. 6-7, 145-147] and the other
lists of `seekwhence.sequences` (DECISION D-070), each with its citation and reported
result under the list. The answering modes are PANES.md P11's three (G-048); the caps are
DECISION D-011's; the trace is DECISION D-055's verbose or terse. The speed slider runs
over Numbo's `DELAYS`. `set_state` enables the buttons that make sense in the controller's
state; `set_replay` turns the inputs off while a session file is replayed (G-047).
"""

from PySide6.QtCore import Qt, Signal
from PySide6.QtWidgets import (QButtonGroup, QComboBox, QFormLayout, QGridLayout, QHBoxLayout,
                               QLabel, QLineEdit, QPushButton, QRadioButton, QScrollArea, QSlider,
                               QVBoxLayout, QWidget)

from seekwhence.gui.models import controls as cm

__all__ = ["ControlsPanel"]

PAGES = "[M86 pp. 6-7, 37, 145-147, 164-179; FCCA pp. 53-54]. Numbo GUI, controls.py"


class ControlsPanel(QWidget):
    """The controls (Numbo GUI, `controls.py`; G-049). Signals: `delay_changed(seconds)`
    when the slider moves; `inputs_changed()` when any input changes. `pane_id`, `title`
    and `pages` are the view protocol's labels (G-011); the pane takes no events."""

    pane_id = "P11"
    title = "Controls"
    pages = PAGES
    delay_changed = Signal(float)
    inputs_changed = Signal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self.base_config = None         # item 15: the edited configuration, or None
        self.catalogue = cm.catalogue()
        self.preset_combo = QComboBox()
        self.preset_combo.setToolTip("the run's inputs: an appendix run [M86 pp. 164-179], "
                                     "your own terms, or a sequence of the sources")
        group = None
        for entry in self.catalogue:
            if entry.group != group and entry.group not in ("appendix", "custom"):
                self.preset_combo.insertSeparator(self.preset_combo.count())
                title = cm.GROUP_TITLES.get(entry.group, entry.group)
                self.preset_combo.addItem(f"— {title}")
                index = self.preset_combo.count() - 1
                item = self.preset_combo.model().item(index)
                item.setEnabled(False)
            group = entry.group
            self.preset_combo.addItem(entry.label, entry.id)
            self.preset_combo.setItemData(self.preset_combo.count() - 1, entry.tooltip,
                                          Qt.ItemDataRole.ToolTipRole)
        self.preset_combo.setSizeAdjustPolicy(
            QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
        self.preset_combo.setMinimumContentsLength(18)       # not the longest label (G-058)
        self.result_label = QLabel()
        self.result_label.setWordWrap(True)
        self.result_label.setObjectName("preset-result")
        self.result_label.setToolTip("the citation and Meredith's reported result "
                                     "[M86 pp. 145-147]")
        self.terms_edit = QLineEdit()
        self.terms_edit.setPlaceholderText("terms and commands: 3 7 3 7 show")
        self.terms_edit.setToolTip("the console lines fed in order: terms, show commands "
                                   "and bye [M86 p. 164]; for Custom only")
        self.seed_edit = QLineEdit()
        self.seed_edit.setToolTip("the run's seed: the same seed repeats the run")
        self.random_button = QPushButton("Random")
        self.random_button.setToolTip("a new seed, from Python's random, never the engine's")
        self.random_button.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        seed_row = QHBoxLayout()
        seed_row.addWidget(self.seed_edit, 1)
        seed_row.addWidget(self.random_button)
        self.cap_term_edit = QLineEdit()
        self.cap_term_edit.setToolTip("max_tasks_per_term: tasks run between two terms "
                                      "before the system gives up on the term (DECISION D-011)")
        self.cap_total_edit = QLineEdit()
        self.cap_total_edit.setToolTip("max_tasks: tasks run in the whole run (DECISION D-011)")
        self.answering_combo = QComboBox()
        for mode in cm.ANSWERING:
            self.answering_combo.addItem(cm.ANSWERING_TEXT[mode], mode)
        self.answering_combo.setToolTip(
            "interactive: you type the terms and answer the guesses [M86 pp. 37, 166];\n"
            "scripted: the preset's lines fed on request, guesses answered from the script, "
            "then judged against the terms fed (DECISIONS D-033, D-053);\n"
            "continuation judge: the bench's knowing user judges a guess against the true "
            "continuation (DECISION D-070)")
        self.answering_combo.setSizeAdjustPolicy(
            QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
        self.answering_combo.setMinimumContentsLength(18)
        self.verbose_radio = QRadioButton("verbose")
        self.verbose_radio.setToolTip("the doing task lines, as runs 1 and 3 print them "
                                      "[M86 pp. 164, 170] (DECISION D-055)")
        self.terse_radio = QRadioButton("terse")
        self.terse_radio.setToolTip("no doing task lines, as run 2 prints [M86 p. 167]")
        self.trace_group = QButtonGroup(self)
        self.trace_group.addButton(self.verbose_radio)
        self.trace_group.addButton(self.terse_radio)
        trace_row = QHBoxLayout()
        trace_row.addWidget(self.verbose_radio)
        trace_row.addWidget(self.terse_radio)
        trace_row.addStretch(1)

        form = QFormLayout()
        form.addRow("Preset", self.preset_combo)
        form.addRow("", self.result_label)
        form.addRow("Terms", self.terms_edit)
        form.addRow("Seed", seed_row)
        form.addRow("Cap per term", self.cap_term_edit)
        form.addRow("Cap for the run", self.cap_total_edit)
        form.addRow("Answering", self.answering_combo)
        form.addRow("Trace", trace_row)

        self.play_button = QPushButton("Play")
        self.pause_button = QPushButton("Pause")
        self.step_button = QPushButton("Step")
        self.step_button.setToolTip("one event")
        self.step_task_button = QPushButton("Step task")
        self.step_task_button.setToolTip("on through the next task chosen [M86 p. 41]")
        self.step_term_button = QPushButton("Step term")
        self.step_term_button.setToolTip("on through the next prompt for a term [M86 p. 165]")
        self.run_to_end_button = QPushButton("Run to end")
        self.run_to_end_button.setToolTip("no delay (every event is still drawn)")
        self.stop_button = QPushButton("Stop")
        self.buttons = (self.play_button, self.pause_button, self.step_button,
                        self.step_task_button, self.step_term_button, self.run_to_end_button,
                        self.stop_button)
        buttons = QGridLayout()
        for i, button in enumerate(self.buttons):
            button.setFocusPolicy(Qt.FocusPolicy.NoFocus)    # the shortcuts keep the keys
            buttons.addWidget(button, i // 2, i % 2)

        self.speed_slider = QSlider(Qt.Orientation.Horizontal)
        self.speed_slider.setRange(0, len(cm.DELAYS) - 1)
        self.speed_slider.setPageStep(1)
        self.speed_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        # The mouse's only: the window's Space and arrow keys run the run (Numbo GUI).
        self.speed_slider.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.speed_label = QLabel()
        self.speed_label.setMinimumWidth(48)
        speed = QHBoxLayout()
        speed.addWidget(QLabel("Speed"))
        speed.addWidget(self.speed_slider, 1)
        speed.addWidget(self.speed_label)

        self.error_label = QLabel()
        self.error_label.setObjectName("input-error")
        self.error_label.setWordWrap(True)
        self.error_label.setStyleSheet("color: #b00020;")
        self.error_label.hide()

        # The form in a scroll area, so a short dock scrolls instead of widening the
        # window's minimum (G-058).
        self.form_widget = QWidget()
        inner = QVBoxLayout(self.form_widget)
        inner.setContentsMargins(4, 4, 4, 4)
        inner.addLayout(form)
        inner.addLayout(buttons)
        inner.addLayout(speed)
        inner.addWidget(self.error_label)
        inner.addStretch(1)
        self.scroll = QScrollArea()
        self.scroll.setWidget(self.form_widget)
        self.scroll.setWidgetResizable(True)
        self.scroll.setFrameShape(QScrollArea.Shape.NoFrame)
        self.scroll.setMinimumSize(40, 24)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.addWidget(self.scroll)

        self.replaying = None
        self._tips = {}
        self.preset_combo.currentIndexChanged.connect(self._preset_changed)
        self.random_button.clicked.connect(self.new_random_seed)
        self.speed_slider.valueChanged.connect(self._speed_changed)
        for edit in (self.terms_edit, self.seed_edit, self.cap_term_edit, self.cap_total_edit):
            edit.editingFinished.connect(self.inputs_changed)
        self.answering_combo.currentIndexChanged.connect(self._answering_changed)
        self.verbose_radio.toggled.connect(self._trace_changed)
        self.set_values(cm.ControlValues())
        self._speed_changed()
        self.set_state("idle")

    # -- reading and showing (G-049) --------------------------------------------------------

    @property
    def delay(self) -> float:
        """The slider's delay in seconds (Numbo GUI)."""
        return cm.DELAYS[self.speed_slider.value()]

    @property
    def preset(self) -> str:
        """The preset id chosen (G-048)."""
        return self.preset_combo.currentData()

    @property
    def answering(self) -> str:
        """The answering mode chosen, one of `ANSWERING` (G-048)."""
        return self.answering_combo.currentData()

    def values(self) -> cm.ControlValues:
        """What the controls show, as text (G-049)."""
        return cm.ControlValues(preset=self.preset, terms=self.terms_edit.text(),
                                seed=self.seed_edit.text(), cap_term=self.cap_term_edit.text(),
                                cap_total=self.cap_total_edit.text(), answering=self.answering,
                                verbose=self.verbose_radio.isChecked(),
                                speed=self.speed_slider.value())

    def set_values(self, values: cm.ControlValues) -> None:
        """Show the values; emits nothing (G-049)."""
        widgets = (self.preset_combo, self.terms_edit, self.seed_edit, self.cap_term_edit,
                   self.cap_total_edit, self.answering_combo, self.verbose_radio,
                   self.terse_radio, self.speed_slider)
        for w in widgets:
            w.blockSignals(True)
        try:
            index = self.preset_combo.findData(values.preset)
            if index < 0:
                index = self.preset_combo.findData(cm.CUSTOM)
            self.preset_combo.setCurrentIndex(index)
            self.terms_edit.setText(values.terms)
            self.seed_edit.setText(values.seed)
            self.cap_term_edit.setText(values.cap_term)
            self.cap_total_edit.setText(values.cap_total)
            self.answering_combo.setCurrentIndex(
                max(0, self.answering_combo.findData(values.answering)))
            (self.verbose_radio if values.verbose else self.terse_radio).setChecked(True)
            self.speed_slider.setValue(values.speed)
        finally:
            for w in widgets:
                w.blockSignals(False)
        self._show_preset()
        self.speed_label.setText(cm.delay_text(self.delay))

    def inputs(self):
        """The `RunInputs` of the controls; `InputError` if a text is not valid
        (Numbo GUI's `inputs`; G-048)."""
        return cm.run_inputs(self.values(), self.base_config)

    def show_inputs(self, inputs) -> None:
        """Show the inputs a preset action, a setting or a session file gave (G-049)."""
        self.set_values(cm.values_of(inputs, self.values()))

    def choose_preset(self, preset: str) -> None:
        """Choose a preset by id, with its default seed and trace (G-048)."""
        index = self.preset_combo.findData(preset)
        if index >= 0:
            self.preset_combo.setCurrentIndex(index)

    def new_random_seed(self) -> int:
        """The "Random" button: a fresh seed in the field (G-049)."""
        seed = cm.random_seed()
        self.seed_edit.setText(str(seed))
        self.inputs_changed.emit()
        return seed

    # -- changes -----------------------------------------------------------------------------

    def _show_preset(self):
        entry = cm.entry_of(self.preset) if self.preset is not None else None
        text = entry.result_text if entry is not None else ""
        self.result_label.setText(text)
        self.result_label.setToolTip(entry.tooltip if entry is not None else "")
        self.terms_edit.setEnabled(self.replaying is None and self.preset == cm.CUSTOM)

    def _preset_changed(self):
        """A new preset: its default seed and trace, then `inputs_changed` (G-048)."""
        if self.preset is None:
            return
        entry = cm.entry_of(self.preset)
        self.seed_edit.setText(str(entry.seed))
        for radio in (self.verbose_radio, self.terse_radio):
            radio.blockSignals(True)
        (self.verbose_radio if entry.verbose else self.terse_radio).setChecked(True)
        for radio in (self.verbose_radio, self.terse_radio):
            radio.blockSignals(False)
        self._show_preset()
        self.inputs_changed.emit()

    def _answering_changed(self):
        self.inputs_changed.emit()

    def _trace_changed(self, _checked):
        self.inputs_changed.emit()

    def _speed_changed(self):
        self.speed_label.setText(cm.delay_text(self.delay))
        self.delay_changed.emit(self.delay)

    def set_replay(self, text) -> None:
        """Replay mode, `text` saying what is replayed: the run buttons play the replay,
        and the run's inputs are off (their tooltips say why). None: live runs (Numbo
        GUI; G-047)."""
        self.replaying = text
        for w in (self.preset_combo, self.terms_edit, self.seed_edit, self.random_button,
                  self.cap_term_edit, self.cap_total_edit, self.answering_combo,
                  self.verbose_radio, self.terse_radio):
            tip = self._tips.setdefault(w, w.toolTip())
            w.setEnabled(text is None)
            w.setToolTip(tip if text is None else
                         f"replaying {text}: Session > Close replay for live runs")
        self._show_preset()

    def show_error(self, message) -> None:
        """`message` under the buttons ("" hides it) (Numbo GUI)."""
        self.error_label.setText(message)
        self.error_label.setVisible(bool(message))

    def set_state(self, state) -> None:
        """Enable the buttons that make sense in the run controller's `state` (Numbo
        GUI)."""
        live = state in ("running", "paused")
        self.pause_button.setEnabled(state == "running")
        self.stop_button.setEnabled(live)

"""The controls pane: the puzzle (the chapter's 11, or a custom target and 5
bricks), the seed and the iteration cap, the run buttons and the speed
slider.  The window connects the buttons; this pane only holds the widgets,
reads the inputs (run_stats' parsers) and shows input errors."""

from PySide6.QtCore import Signal
from PySide6.QtWidgets import (QComboBox, QFormLayout, QGridLayout, QHBoxLayout, QLabel,
                               QLineEdit, QPushButton, QSlider, QVBoxLayout, QWidget)
from PySide6.QtCore import Qt

from numbo.models import run_stats

# The speed slider's stops: the delay after each event while playing, in
# seconds, slowest first.  0 is "as fast as redrawing allows".
DELAYS = (1.0, 0.5, 0.2, 0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0)
DEFAULT_DELAY = 0.01

CUSTOM = "Custom"


def delay_text(seconds):
    if seconds == 0:
        return "max"
    if seconds >= 1:
        return f"{seconds:g} s"
    return f"{seconds * 1000:g} ms"


class ControlsPanel(QWidget):
    """The controls.  delay_changed(seconds) when the slider moves."""

    delay_changed = Signal(float)

    def __init__(self, parent=None):
        super().__init__(parent)
        self.puzzle_combo = QComboBox()
        for n in range(1, len(run_stats.CHAPTER_PUZZLES) + 1):
            self.puzzle_combo.addItem(run_stats.puzzle_label(n))
        self.puzzle_combo.addItem(CUSTOM)
        self.custom_edit = QLineEdit()
        self.custom_edit.setPlaceholderText("target, 5 bricks: 114 11 20 7 1 6")
        self.seed_edit = QLineEdit("1")
        self.cap_edit = QLineEdit("20000")
        self.cap_edit.setToolTip("stop after this many iterations, or 'none'")

        form = QFormLayout()
        form.addRow("Puzzle", self.puzzle_combo)
        form.addRow("Custom", self.custom_edit)
        form.addRow("Seed", self.seed_edit)
        form.addRow("Max iterations", self.cap_edit)

        self.play_button = QPushButton("Play")
        self.pause_button = QPushButton("Pause")
        self.step_button = QPushButton("Step")
        self.step_button.setToolTip("one event")
        self.step_iteration_button = QPushButton("Step iteration")
        self.step_iteration_button.setToolTip("on through the next iteration")
        self.run_to_end_button = QPushButton("Run to end")
        self.run_to_end_button.setToolTip("no delay (every event is still drawn)")
        self.stop_button = QPushButton("Stop")
        buttons = QGridLayout()
        for i, button in enumerate((self.play_button, self.pause_button, self.step_button,
                                    self.step_iteration_button, self.run_to_end_button,
                                    self.stop_button)):
            buttons.addWidget(button, i // 2, i % 2)

        self.speed_slider = QSlider(Qt.Orientation.Horizontal)
        self.speed_slider.setRange(0, len(DELAYS) - 1)
        self.speed_slider.setPageStep(1)
        self.speed_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        # The mouse's only: the window's Space and arrow keys run the run.
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

        layout = QVBoxLayout(self)
        layout.addLayout(form)
        layout.addLayout(buttons)
        layout.addLayout(speed)
        layout.addWidget(self.error_label)
        layout.addStretch(1)

        self.replaying = None
        self._tips = {}
        self.puzzle_combo.currentIndexChanged.connect(self._puzzle_changed)
        self.speed_slider.valueChanged.connect(self._speed_changed)
        self._puzzle_changed()
        self.speed_slider.setValue(DELAYS.index(DEFAULT_DELAY))
        self._speed_changed()
        self.set_state("idle")

    @property
    def delay(self):
        return DELAYS[self.speed_slider.value()]

    def _puzzle_changed(self):
        self.custom_edit.setEnabled(self.replaying is None
                                    and self.puzzle_combo.currentText() == CUSTOM)

    def set_replay(self, text):
        """Replay mode, TEXT saying what is replayed: the run buttons play
        the replay, and the run's inputs are off (their tooltips say why).
        None: live runs."""
        self.replaying = text
        for w in (self.puzzle_combo, self.custom_edit, self.seed_edit, self.cap_edit):
            tip = self._tips.setdefault(w, w.toolTip())
            w.setEnabled(text is None)
            w.setToolTip(tip if text is None else
                         f"replaying {text}: Session > Close replay for live runs")
        self._puzzle_changed()

    def _speed_changed(self):
        self.speed_label.setText(delay_text(self.delay))
        self.delay_changed.emit(self.delay)

    def inputs(self):
        """(problem, seed, max_iterations) from the controls; InputError if
        one is not valid."""
        index = self.puzzle_combo.currentIndex()
        if self.puzzle_combo.currentText() == CUSTOM:
            problem = run_stats.parse_problem(self.custom_edit.text())
        else:
            problem = run_stats.CHAPTER_PUZZLES[index]
        return (problem, run_stats.parse_seed(self.seed_edit.text()),
                run_stats.parse_max_iterations(self.cap_edit.text()))

    def show_error(self, message):
        """MESSAGE under the buttons ("" hides it)."""
        self.error_label.setText(message)
        self.error_label.setVisible(bool(message))

    def set_state(self, state):
        """Enable the buttons that make sense in the run controller's STATE."""
        live = state in ("running", "paused")
        self.pause_button.setEnabled(state == "running")
        self.stop_button.setEnabled(live)

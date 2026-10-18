"""P9 Controls: the problem selector, the profile, the seed, the read-only cap, the run
buttons and the speed slider, a view of `models/controls.py` (PANES.md P9; Numbo GUI,
`controls.py`, adopted through the Seek-Whence GUI's `controls.py`; G-044).

As Numbo's, the pane only holds the widgets, reads the inputs (`inputs()`, through the
model's `run_inputs`) and shows input errors; the window connects the buttons and the
slider and keeps the next run's inputs. The problems are the 53 digitized configurations
by family, the chapter 4 configuration first [F95 p. 118], each with its title and page
[F95 p. 114]; the profiles are `config.PROFILES` (D-066); the cap is `max_codelets` of the
next run's configuration (D-061), read-only here and editable in the configuration pane
(ANSWERS.md 3). The buttons are Numbo's with Back (ANSWERS.md 5), and the speed slider
runs over Numbo's `DELAYS`. `set_values` shows any run's inputs back, so the selector
agrees with the run actually started whichever way it was started (PANES.md P9). The
panel takes every event only to count it for its run line, a thin strip along the
top of the pane that says the controller's state and the events shown, and that is what it
repaints at each event (G-011's view protocol: `on_event`, `redraw`, `paints`).
"""

from PySide6.QtCore import QRect, QSize, Qt, Signal
from PySide6.QtGui import QColor, QFont, QPainter
from PySide6.QtWidgets import (QComboBox, QFormLayout, QGridLayout, QHBoxLayout, QLabel,
                               QLineEdit, QPushButton, QScrollArea, QSizePolicy, QSlider,
                               QVBoxLayout, QWidget)

from tabletop import events as ev
from tabletop.config import PROFILES
from tabletop.gui import paint
from tabletop.gui.models import controls as cm

__all__ = ["MIN_SIZE", "ControlsPanel", "RunLine"]

# The smallest the pane may be (G-014): the form scrolls below it.
MIN_SIZE = (120, 60)
INK, MUTED = QColor("#1a1a1a"), QColor("#6b6b6b")
LIVE = QColor("#1b7f3b")


class RunLine(QWidget):
    """The thin strip along the top of the pane: the run controller's state and the
    events shown, repainted once per event (G-011, G-044)."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.state = "idle"
        self.events = 0
        self.paints = 0
        self.setMinimumHeight(16)
        self.setMaximumHeight(16)
        self.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Fixed)

    def text(self) -> str:
        """`idle`, `running: 123 events shown`, `finished: 496 events` (G-044)."""
        if self.state == "idle" and self.events == 0:
            return "idle: no run yet"
        word = "events shown" if self.state in ("running", "paused") else "events"
        return f"{self.state}: {self.events} {word}"

    def paintEvent(self, event):
        """Paint the line; `paints` counts the calls for the window's probe (G-011)."""
        self.paints += 1
        painter = QPainter(self)
        font = QFont(painter.font())
        font.setPointSizeF(8.0)
        painter.setFont(font)
        painter.setPen(LIVE if self.state in ("running", "paused") else MUTED)
        painter.drawText(QRect(2, 0, self.width() - 4, self.height()),
                         Qt.AlignmentFlag.AlignVCenter | Qt.AlignmentFlag.AlignLeft,
                         painter.fontMetrics().elidedText(self.text(), Qt.TextElideMode.ElideRight,
                                                          self.width() - 4))
        painter.end()


class ControlsPanel(QWidget):
    """The controls pane (Numbo GUI, `controls.py`; G-044; G-011's view protocol).
    Signals: `delay_changed(seconds)` when the slider moves; `inputs_changed()` when the
    problem, the profile or the seed changes. `pane_id`, `title` and `pages` are the
    window's labels."""

    delay_changed = Signal(float)
    inputs_changed = Signal()

    def __init__(self, title="Controls", pane_id="P9", pages="", parent=None):
        super().__init__(parent)
        self.title, self.pane_id, self.pages = title, pane_id, pages
        self.config = PROFILES[cm.DEFAULT_PROFILE]     # the next run's, for the cap line
        self.replaying = None
        self._tips = {}
        self._build_form()
        self._build_buttons()
        self._build_speed()
        self.error_label = QLabel()
        self.error_label.setObjectName("input-error")
        self.error_label.setWordWrap(True)
        self.error_label.setStyleSheet("color: #b00020;")
        self.error_label.hide()
        self.run_line = RunLine()
        self._lay_out()
        self.problem_combo.currentIndexChanged.connect(self._problem_changed)
        self.profile_combo.currentIndexChanged.connect(self._profile_changed)
        self.seed_edit.editingFinished.connect(self.inputs_changed)
        self.speed_slider.valueChanged.connect(self._speed_changed)
        self.set_values(cm.ControlValues())
        self._speed_changed()
        self.set_state("idle")
        self.setMinimumSize(*MIN_SIZE)
        self.setToolTip(f"{pane_id} {title}: {pages}")

    # -- building ---------------------------------------------------------------------------

    def _build_form(self):
        """The problem selector by family with the chapter 4 configuration first, the
        line under it with the problem's title, family and page, the profile, the seed
        and the read-only cap (PANES.md P9; G-044)."""
        self.problem_combo = QComboBox()
        self.problem_combo.setToolTip('the problem: one of the 53 digitized configurations '
                                      '[F95 p. 114], by family')
        family = None
        for entry in cm.catalogue():
            if entry.family != family and entry.name != cm.DEFAULT_PROBLEM:
                self.problem_combo.insertSeparator(self.problem_combo.count())
                self.problem_combo.addItem(f"— {cm.FAMILY_TITLES.get(entry.family, entry.family)}")
                self.problem_combo.model().item(self.problem_combo.count() - 1).setEnabled(False)
                family = entry.family
            self.problem_combo.addItem(entry.label, entry.name)
            self.problem_combo.setItemData(self.problem_combo.count() - 1, entry.tooltip,
                                           Qt.ItemDataRole.ToolTipRole)
        self.problem_combo.setSizeAdjustPolicy(
            QComboBox.SizeAdjustPolicy.AdjustToMinimumContentsLengthWithIcon)
        self.problem_combo.setMinimumContentsLength(14)        # not the longest label (G-014)
        self.problem_label = QLabel()
        self.problem_label.setWordWrap(True)
        self.problem_label.setObjectName("problem-info")
        self.problem_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self.profile_combo = QComboBox()
        for name in PROFILES:
            self.profile_combo.addItem(name, name)
        self.profile_combo.setToolTip("the configuration profile (D-066): standard, buridan, "
                                      "human or french-faults; the fields are in Edit > "
                                      "Configuration")
        self.seed_edit = QLineEdit()
        self.seed_edit.setToolTip("the run's seed: the same seed repeats the run, with or "
                                  "without the GUI (D-064)")
        self.cap_label = QLabel()
        self.cap_label.setObjectName("cap")
        self.cap_label.setSizePolicy(QSizePolicy.Policy.Ignored, QSizePolicy.Policy.Preferred)
        self.cap_label.setToolTip("the run cap, max_codelets (D-061): a capped run answers "
                                  "as it stands; editable in Edit > Configuration")
        self.form = QFormLayout()
        self.form.addRow("Problem", self.problem_combo)
        self.form.addRow("", self.problem_label)
        self.form.addRow("Profile", self.profile_combo)
        self.form.addRow("Seed", self.seed_edit)
        self.form.addRow("Cap", self.cap_label)

    def _build_buttons(self):
        """Play, Pause, Step, Step codelet, Run to end, Stop and Back (ANSWERS.md 5; Numbo
        GUI, `controls.py`); the buttons take no keyboard focus, so the arrows and Space
        stay the window's shortcuts (G-044)."""
        self.play_button = QPushButton("Play")
        self.play_button.setToolTip("play with the speed's delay (Space)")
        self.pause_button = QPushButton("Pause")
        self.pause_button.setToolTip("pause a playing run (Space)")
        self.step_button = QPushButton("Step")
        self.step_button.setToolTip("one event, one visible change (Right)")
        self.step_codelet_button = QPushButton("Step codelet")
        self.step_codelet_button.setToolTip("on to the end of the current codelet (Shift+Right)")
        self.run_to_end_button = QPushButton("Run to end")
        self.run_to_end_button.setToolTip("no delay (every event is still drawn)")
        self.stop_button = QPushButton("Stop")
        self.stop_button.setToolTip("stop the run (Esc)")
        self.back_button = QPushButton("Back")
        self.back_button.setToolTip("one event back in the history, when no run is live (Left)")
        self.buttons = (self.play_button, self.pause_button, self.step_button,
                        self.step_codelet_button, self.run_to_end_button, self.stop_button,
                        self.back_button)
        self.buttons_layout = QGridLayout()
        for i, button in enumerate(self.buttons):
            button.setFocusPolicy(Qt.FocusPolicy.NoFocus)
            button.setMinimumWidth(40)
            self.buttons_layout.addWidget(button, i // 2, i % 2)

    def _build_speed(self):
        """The speed slider over Numbo's `DELAYS`, the mouse's only (Numbo GUI,
        `controls.py`)."""
        self.speed_slider = QSlider(Qt.Orientation.Horizontal)
        self.speed_slider.setRange(0, len(cm.DELAYS) - 1)
        self.speed_slider.setPageStep(1)
        self.speed_slider.setTickPosition(QSlider.TickPosition.TicksBelow)
        self.speed_slider.setFocusPolicy(Qt.FocusPolicy.NoFocus)
        self.speed_slider.setToolTip("the delay after each event while playing")
        self.speed_label = QLabel()
        self.speed_label.setMinimumWidth(40)
        self.speed_layout = QHBoxLayout()
        self.speed_layout.addWidget(QLabel("Speed"))
        self.speed_layout.addWidget(self.speed_slider, 1)
        self.speed_layout.addWidget(self.speed_label)

    def _lay_out(self):
        """The run line along the top, always in view, then the form, the buttons, the
        speed and the error in a scroll area, so that a short dock scrolls instead of
        widening the window's minimum (G-014; Seek-Whence GUI, `controls.py`)."""
        self.form_widget = QWidget()
        inner = QVBoxLayout(self.form_widget)
        inner.setContentsMargins(4, 4, 4, 4)
        inner.addLayout(self.form)
        inner.addLayout(self.buttons_layout)
        inner.addLayout(self.speed_layout)
        inner.addWidget(self.error_label)
        inner.addStretch(1)
        self.scroll = QScrollArea()
        self.scroll.setWidget(self.form_widget)
        self.scroll.setWidgetResizable(True)
        self.scroll.setFrameShape(QScrollArea.Shape.NoFrame)
        self.scroll.setMinimumSize(40, 24)
        layout = QVBoxLayout(self)
        layout.setContentsMargins(0, 0, 0, 0)
        layout.setSpacing(0)
        layout.addWidget(self.run_line)
        layout.addWidget(self.scroll, 1)

    def sizeHint(self):
        """Enough for the form and the buttons (G-011)."""
        return QSize(270, 300)

    # -- reading and showing (G-044) ----------------------------------------------------------

    @property
    def delay(self) -> float:
        """The slider's delay in seconds (Numbo GUI)."""
        return cm.DELAYS[self.speed_slider.value()]

    @property
    def problem(self) -> str:
        """The problem name chosen (G-044)."""
        return self.problem_combo.currentData()

    @property
    def profile(self) -> str:
        """The profile name chosen (G-044)."""
        return self.profile_combo.currentData()

    def values(self) -> cm.ControlValues:
        """What the controls show, as text (G-044)."""
        return cm.ControlValues(problem=self.problem or cm.DEFAULT_PROBLEM,
                                profile=self.profile or cm.DEFAULT_PROFILE,
                                seed=self.seed_edit.text(), speed=self.speed_slider.value())

    def set_values(self, values: cm.ControlValues) -> None:
        """Show the values; emits nothing (G-044)."""
        widgets = (self.problem_combo, self.profile_combo, self.seed_edit, self.speed_slider)
        for w in widgets:
            w.blockSignals(True)
        try:
            self.problem_combo.setCurrentIndex(max(0, self.problem_combo.findData(values.problem)))
            self.profile_combo.setCurrentIndex(max(0, self.profile_combo.findData(values.profile)))
            self.seed_edit.setText(values.seed)
            self.speed_slider.setValue(values.speed)
        finally:
            for w in widgets:
                w.blockSignals(False)
        self._show_problem()
        self.speed_label.setText(cm.delay_text(self.delay))

    def inputs(self) -> tuple:
        """The (problem name, profile name, seed) of the controls; `InputError` if a text
        is not valid (Numbo GUI's `inputs`; G-044)."""
        return cm.run_inputs(self.values())

    def show_run(self, problem: str, profile: str, seed) -> None:
        """Show the inputs of a run started any other way (the Problem menu, the command
        line, a session file): the selector agrees with the run (PANES.md P9; G-044)."""
        self.set_values(cm.values_of(problem, profile, seed, self.values()))

    def set_config(self, config) -> None:
        """The next run's `Config`, for the read-only cap line (PANES.md P9; D-061)."""
        self.config = config
        self.cap_label.setText(cm.cap_text(config))

    # -- changes --------------------------------------------------------------------------------

    def _show_problem(self):
        try:
            entry = cm.entry_of(self.problem)
        except KeyError:
            entry = None
        self.problem_label.setText("" if entry is None else
                                   f"{entry.title}, {entry.family} {entry.citation}".rstrip())
        self.problem_label.setToolTip("" if entry is None else entry.tooltip)

    def _problem_changed(self):
        if self.problem is None:
            return
        self._show_problem()
        self.inputs_changed.emit()

    def _profile_changed(self):
        self.inputs_changed.emit()

    def _speed_changed(self):
        self.speed_label.setText(cm.delay_text(self.delay))
        self.delay_changed.emit(self.delay)

    def set_replay(self, text) -> None:
        """Replay mode, `text` saying what is replayed: the run buttons play the replay,
        and the run's inputs are off (their tooltips say why). None: live runs
        (Numbo GUI, `controls.py`)."""
        self.replaying = text
        for w in (self.problem_combo, self.profile_combo, self.seed_edit):
            tip = self._tips.setdefault(w, w.toolTip())
            w.setEnabled(text is None)
            w.setToolTip(tip if text is None else
                         f"replaying {text}: Session > Close replay for live runs")

    def show_error(self, message) -> None:
        """`message` under the buttons ("" hides it) (Numbo GUI)."""
        self.error_label.setText(message)
        self.error_label.setVisible(bool(message))

    def set_state(self, state) -> None:
        """Enable the buttons that make sense in the run controller's `state`, and say
        it on the run line (Numbo GUI)."""
        live = state in ("running", "paused")
        self.pause_button.setEnabled(state == "running")
        self.stop_button.setEnabled(live)
        self.back_button.setEnabled(not live)
        self.run_line.state = state
        self.run_line.update()

    # -- the view protocol (G-011) ---------------------------------------------------------------

    @property
    def events(self) -> int:
        """How many events of the run the pane has seen (G-011)."""
        return self.run_line.events

    @property
    def paints(self) -> int:
        """The run line's paints, the one thing the pane repaints per event (G-011)."""
        return self.run_line.paints

    def on_event(self, event) -> None:
        """Count the event for the run line (G-044)."""
        if isinstance(event, ev.RunStarted):
            self.run_line.events = 0
        self.run_line.events += 1

    def redraw(self, now=False) -> None:
        """Mark the run line dirty; with `now`, paint it at once (Numbo GUI, `paint.py`)."""
        self.run_line.update()
        if now:
            paint.paint_all()

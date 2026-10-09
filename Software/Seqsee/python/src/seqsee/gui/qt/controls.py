"""The controls: config/GUI_sparse.conf's buttons and key bindings, an InterstepSleep slider,
the codelet count (lib/Tk/SCodeletCount.pm) and the status bar's run widgets.

Perl's GUI_sparse.conf packs a button frame above the canvas: Start
(``main::Interaction_continue()``), Pause (``$Global::Break_Loop = 1``) and Quit on the
left, the SCodeletCount label (``$Global::Steps_Finished || 0``, Helvetica bold 20 px) on
the right. Its [bindings] put single keys on the main window:

    s step   h/j/k/l step 5/25/50/100   d/f/g crawl 30/300/700 ms   c continue   p pause
    x new sequence (SGUI::ask_seq)   q quit   m debugMAX = 1 - debugMAX

Here the buttons and the bindings are QActions: the bindings in a Run menu (with their keys
as shortcuts) and on the toolbar, after Start / Pause / Quit; the count sits at the right of
the toolbar. GUI_sparse.conf's [Scale] (0–40 ms, ticks every 10, on
``$Global::InterstepSleep``) is configured but packed in no frame, so Perl shows no slider;
this one is added, in the menu bar's right corner (so that the toolbar, count included, fits
the 780 px canvas's width). The status bar gets permanent labels for the steps, elements, groups and
the run state (the transient messages — errors, a died part — stay on its left).

Each action calls ``MainWindow.run_command``, which drives a ``runner.Runner``.
``RunSettings`` holds Seqsee.pl's ``--seed`` and ``--max_steps`` (no Perl widget).
"""
import dataclasses
from typing import Optional

from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QAction, QIntValidator, QKeySequence
from PySide6.QtWidgets import QHBoxLayout, QLabel, QLineEdit, QMenu, QSizePolicy, QSlider, \
    QSpinBox, QToolButton, QWidget

from seqsee import util

from . import render

COUNT_FONT = "-adobe-helvetica-bold-r-normal--20-140-100-100-p-105-iso8859-4"  # [SCodeletCount]
SLEEP_MIN, SLEEP_MAX, SLEEP_TICK = 0, 40, 10       # [Scale] -from, -to, -tickinterval
SEED_MAX = 2 ** 31 - 1
MAX_STEPS_MAX = 2 ** 31 - 1


@dataclasses.dataclass(frozen=True)
class Binding:
    key: str                    # the KeyPress key (also the shortcut)
    command: str                # MainWindow.run_command's command
    arg: Optional[int]          # its argument (steps, crawl ms)
    label: str                  # menu text
    short: str                  # toolbar text
    tip: str                    # tool tip: the Perl command


# GUI_sparse.conf [bindings], grouped as in the Run menu.
BINDINGS = (
    Binding("s", "step", None, "Step", "Step", "main::Interaction_step()"),
    Binding("h", "step_n", 5, "Step 5", "+5", "main::Interaction_step_n({n=>5})"),
    Binding("j", "step_n", 25, "Step 25", "+25", "main::Interaction_step_n({n=>25})"),
    Binding("k", "step_n", 50, "Step 50", "+50", "main::Interaction_step_n({n=>50})"),
    Binding("l", "step_n", 100, "Step 100", "+100", "main::Interaction_step_n({n=>100})"),
    Binding("d", "crawl", 30, "Crawl 30 ms", "Crawl", "main::Interaction_crawl(30)"),
    Binding("f", "crawl", 300, "Crawl 300 ms", "Crawl 300", "main::Interaction_crawl(300)"),
    Binding("g", "crawl", 700, "Crawl 700 ms", "Crawl 700", "main::Interaction_crawl(700)"),
    Binding("c", "continue", None, "Continue", "Continue", "main::Interaction_continue()"),
    Binding("p", "pause", None, "Pause", "Pause", "$Global::Break_Loop = 1"),
    Binding("x", "new_sequence", None, "New sequence...", "New...", "SGUI::ask_seq()"),
    Binding("m", "debug_max", None, "debugMAX", "debugMAX",
            "$Global::debugMAX = 1 - $Global::debugMAX"),
    Binding("q", "quit", None, "Quit", "Quit", "exit"),
)
# Where the Run menu puts a separator (after these keys).
_MENU_BREAKS = ("l", "g", "p", "m")
# The toolbar shows these bindings after the buttons (c, p and q are the buttons); a tuple is
# one drop-down button (its first action is the click).
_TOOLBAR_KEYS = ("s", "h", "j", "k", "l", None, ("d", "f", "g"), None, "x", "m")

# GUI_sparse.conf [buttons] in button_order: (text, command).
BUTTONS = (("Start", "continue"), ("Pause", "pause"), ("Quit", "quit"))

STATE_TEXT = {"idle": "Idle", "running": "Running", "waiting": "Waiting for an answer",
              "stopped": "Stopped", "paused": "Paused", None: "No model"}


def toggled_debug_max(value):
    """``1 - $Global::debugMAX`` (undef counts as 0)."""
    return 1 - (1 if util.perl_true(value) else 0)


def count_text(steps):
    """Tk::SCodeletCount's Update: ``$Global::Steps_Finished || 0``."""
    return util.perl_str(steps) if util.perl_true(steps) else "0"


class CodeletCount(QLabel):
    """lib/Tk/SCodeletCount.pm: a label showing the steps finished."""

    def __init__(self, parent=None):
        super().__init__(count_text(None), parent)
        self.setFont(render.qfont(COUNT_FONT))
        self.setContentsMargins(6, 0, 6, 0)
        self.setToolTip("Steps finished (codelets and thoughts run)")

    def update_from(self, snap):
        self.setText(count_text(snap.steps if snap is not None else None))


class SleepSlider(QWidget):
    """[Scale] on ``$Global::InterstepSleep``: 'Sleep', a 0–40 ms slider and the value.
    ``moved(ms)`` is emitted for the user's changes only; ``show_value`` doesn't emit."""

    moved = Signal(int)

    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QHBoxLayout(self)
        layout.setContentsMargins(6, 0, 6, 0)
        layout.addWidget(QLabel("Sleep"))
        self.slider = QSlider(Qt.Horizontal)
        self.slider.setRange(SLEEP_MIN, SLEEP_MAX)
        self.slider.setTickInterval(SLEEP_TICK)
        self.slider.setTickPosition(QSlider.TicksBelow)
        self.slider.setSingleStep(1)
        self.slider.setPageStep(SLEEP_TICK)
        self.slider.setFixedWidth(100)
        self.slider.setToolTip("InterstepSleep: milliseconds between steps")
        layout.addWidget(self.slider)
        self.value_label = QLabel()
        self.value_label.setMinimumWidth(self.value_label.fontMetrics().horizontalAdvance(
            "0000 ms"))
        layout.addWidget(self.value_label)
        self._set_label(self.slider.value())
        self.slider.valueChanged.connect(self._changed)

    def _set_label(self, ms):
        self.value_label.setText(f"{ms} ms")

    def _changed(self, ms):
        self._set_label(ms)
        self.moved.emit(ms)

    def show_value(self, ms):
        """Show ``ms`` (a crawl's sleep may be beyond the slider: it shows the end)."""
        self.slider.blockSignals(True)
        self.slider.setValue(max(SLEEP_MIN, min(SLEEP_MAX, ms)))
        self.slider.blockSignals(False)
        self._set_label(ms)


def default_max_steps():
    """config/seqsee.conf's ``max_steps`` (what read_config gives a run without the option)."""
    from seqsee import scoderack, seqsee_main
    section = scoderack._read_config(seqsee_main._SEQSEE_CONF).get("seqsee", {})
    return int(util.perl_num(section.get("max_steps", 100000)))


class RunSettings(QWidget):
    """Seqsee.pl's ``--seed`` and ``--max_steps``: a seed field (empty: Seqsee.pl's random
    default) and a max-steps box. ``max_steps_changed(n)`` is emitted for the user's changes."""

    max_steps_changed = Signal(int)

    def __init__(self, parent=None):
        super().__init__(parent)
        layout = QHBoxLayout(self)
        layout.setContentsMargins(6, 0, 6, 0)
        layout.addWidget(QLabel("Seed"))
        self.seed_edit = QLineEdit()
        self.seed_edit.setPlaceholderText("random")
        self.seed_edit.setValidator(QIntValidator(0, SEED_MAX, self.seed_edit))
        self.seed_edit.setFixedWidth(self.seed_edit.fontMetrics().horizontalAdvance(
            "0000000000") + 12)
        self.seed_edit.setToolTip("--seed: the random seed of a fresh run (empty: random)")
        layout.addWidget(self.seed_edit)
        layout.addWidget(QLabel("Max steps"))
        self.max_steps_box = QSpinBox()
        self.max_steps_box.setRange(1, MAX_STEPS_MAX)
        self.max_steps_box.setSingleStep(100)
        self.max_steps_box.setValue(default_max_steps())
        self.max_steps_box.setToolTip("--max_steps: a run stops after this many steps")
        self.max_steps_box.setFixedWidth(self.max_steps_box.fontMetrics().horizontalAdvance(
            "0000000000") + 40)
        layout.addWidget(self.max_steps_box)
        self.setSizePolicy(QSizePolicy.Fixed, QSizePolicy.Preferred)
        self.max_steps_box.valueChanged.connect(self.max_steps_changed)

    def seed(self):
        text = self.seed_edit.text().strip()
        return int(text) if text.isdigit() else None

    def set_seed(self, seed):
        self.seed_edit.setText("" if seed is None else str(int(seed)))

    def max_steps(self):
        return self.max_steps_box.value()

    def set_max_steps(self, n):
        self.max_steps_box.blockSignals(True)
        self.max_steps_box.setValue(n)
        self.max_steps_box.blockSignals(False)


class RunStatus:
    """The status bar's permanent labels: steps, elements, groups, run state."""

    def __init__(self, status_bar):
        self.labels = {}
        for name in ("steps", "elements", "groups", "state"):
            label = QLabel()
            label.setContentsMargins(4, 0, 4, 0)
            status_bar.addPermanentWidget(label)
            self.labels[name] = label
        self.show_snapshot(None)
        self.show_state(None)

    def show_snapshot(self, snap):
        steps = snap.steps if snap is not None else 0
        self.labels["steps"].setText(f"Steps: {count_text(steps)}")
        self.labels["elements"].setText(
            f"Elements: {snap.element_count if snap is not None else 0}")
        self.labels["groups"].setText(f"Groups: {len(snap.groups) if snap is not None else 0}")

    def show_state(self, state):
        self.labels["state"].setText(STATE_TEXT.get(state, str(state)))


def make_actions(window, run_command):
    """The run actions (key → QAction, with the key as shortcut) and the button actions."""
    run_actions = {}
    for b in BINDINGS:
        action = QAction(b.label, window)
        action.setIconText(b.short)
        action.setShortcut(QKeySequence(b.key.upper()))
        action.setToolTip(f"{b.label} ({b.key}): {b.tip}")
        if b.command == "debug_max":
            action.setCheckable(True)
            action.triggered.connect(lambda checked, b=b: run_command(b.command, int(checked)))
        else:
            action.triggered.connect(lambda _=False, b=b: run_command(b.command, b.arg))
        run_actions[b.key] = action
    button_actions = []
    for text, command in BUTTONS:
        action = QAction(text, window)
        key = next(b.key for b in BINDINGS if b.command == command)
        action.setToolTip(f"{text} ({key})")
        action.triggered.connect(lambda _=False, c=command: run_command(c, None))
        button_actions.append(action)
    return run_actions, button_actions


def fill_run_menu(menu, run_actions):
    for b in BINDINGS:
        menu.addAction(run_actions[b.key])
        if b.key in _MENU_BREAKS:
            menu.addSeparator()


def fill_toolbar(toolbar, run_actions, button_actions, count_label):
    """Start / Pause / Quit, the step / crawl / sequence / debugMAX actions, and the count at
    the right (Perl packs it ``-side => 'right'``). Returns the count's action."""
    for a in button_actions:
        toolbar.addAction(a)
    toolbar.addSeparator()
    for key in _TOOLBAR_KEYS:
        if key is None:
            toolbar.addSeparator()
        elif isinstance(key, tuple):
            button = QToolButton()
            button.setPopupMode(QToolButton.MenuButtonPopup)
            menu = QMenu(button)
            for k in key:
                menu.addAction(run_actions[k])
            button.setMenu(menu)
            button.setDefaultAction(run_actions[key[0]])
            toolbar.addWidget(button)
        else:
            toolbar.addAction(run_actions[key])
    spacer = QWidget()
    spacer.setSizePolicy(QSizePolicy.Expanding, QSizePolicy.Preferred)
    toolbar.addWidget(spacer)
    return toolbar.addWidget(count_label)

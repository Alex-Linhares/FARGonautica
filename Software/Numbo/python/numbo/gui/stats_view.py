"""The stats pane: a view of run_stats.RunStats (state, source, problem,
events, iteration, x, temperature, rack, codelet, outcome and the solution
check).  The source says what the events come from: a live run, a session
file, or an oracle trace and how its verification went (set by the
window)."""

from PySide6.QtWidgets import QFormLayout, QLabel, QWidget
from PySide6.QtCore import Qt

from numbo.models import run_stats

FIELDS = (("state", "State"), ("source", "Source"), ("problem", "Problem"), ("events", "Events"),
          ("iteration", "Iteration"), ("x", "x"), ("temperature", "Temperature"),
          ("rack", "Codelets waiting"), ("codelet", "Codelet"), ("outcome", "Outcome"),
          ("check", "Check"))

VALID_COLOR = "#1b7f3b"
INVALID_COLOR = "#b00020"


def number_text(value):
    if value is None:
        return ""
    if isinstance(value, float):
        return f"{value:.2f}"
    return str(value)


class StatsPanel(QWidget):
    """A GUI-thread view: on_event updates the model, redraw() the labels."""

    def __init__(self, parent=None):
        super().__init__(parent)
        self.model = run_stats.RunStats()
        self.state = "idle"
        self.source = "live run"
        self.stopped = False
        self.labels = {}
        form = QFormLayout(self)
        form.setLabelAlignment(Qt.AlignmentFlag.AlignRight)
        for field, title in FIELDS:
            label = QLabel()
            label.setObjectName(f"stat-{field}")
            label.setTextInteractionFlags(Qt.TextInteractionFlag.TextSelectableByMouse)
            if field in ("check", "outcome", "problem", "source"):
                label.setWordWrap(True)
            self.labels[field] = label
            form.addRow(title, label)
        self.redraw()

    def text(self, field):
        return self.labels[field].text()

    # -- the run --------------------------------------------------------------

    def on_event(self, event):
        if event.kind == "start":
            self.stopped = False
        self.model.on_event(event)

    def set_state(self, state):
        self.state = state
        self.redraw()

    def set_source(self, text):
        self.source = text
        self.redraw()

    def replayed(self):
        """The model was fed a replay's events up to some event: check the
        solution from the events (a replay has no printed text)."""
        self.stopped = False
        self.model.finish(None)

    def finish(self, outcome):
        """The run controller's RunOutcome: check the printed solution."""
        self.stopped = outcome.stopped
        if not outcome.stopped:
            self.model.finish(outcome.output)
        self.redraw()

    def texts(self):
        m = self.model
        problem = ""
        if m.problem is not None:
            problem = f"{run_stats.problem_text(m.problem)}, seed {m.seed}"
        return {"state": self.state, "source": self.source, "problem": problem, "events": str(m.events),
                "iteration": number_text(m.iteration), "x": number_text(m.x),
                "temperature": number_text(m.temperature),
                "rack": number_text(m.rack_total), "codelet": m.codelet or "",
                "outcome": "stopped" if self.stopped else m.outcome_text(),
                "check": m.check_text()}

    def redraw(self, now=True):
        """Put the model into the labels, and paint them now if they
        changed (the run waits for this: every event is drawn)."""
        changed = False
        for field, text in self.texts().items():
            label = self.labels[field]
            if label.text() != text:
                label.setText(text)
                changed = True
        style = ""
        if self.model.check is not None:
            style = f"color: {VALID_COLOR if self.model.check[0] else INVALID_COLOR};"
        if self.labels["check"].styleSheet() != style:
            self.labels["check"].setStyleSheet(style)
            changed = True
        if changed and self.isVisible():
            self.repaint() if now else self.update()

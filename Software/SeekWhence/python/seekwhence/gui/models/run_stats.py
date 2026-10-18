"""The stats pane's model (PANES.md P12; Numbo GUI, `run_stats.py`'s `RunStats`; G-016).
Qt-free.

`RunStats` observes a run's events and reads the snapshot in force: it keeps the seed
and the configuration note (from `start`), the number of events, the clock (tasks run,
from `chosen` [M86 p. 41]), the task last chosen, the terms entered (`term`
[M86 p. 164]), and from the snapshot (`update`) the tasks waiting on the rack, the
reigning class [M86 p. 99], the threshold [M86 p. 105], the hypothesis's form and its
validity [M86 p. 81]; then the outcome (`ended`, `capped`; DECISION D-069) and, once
the run has ended, `Session.summary()`, the line the CLI prints (`finish`, given the run
controller's `RunOutcome`). `texts()` gives every field as text, as the Numbo GUI's stats
panel does.
"""

from seekwhence.config import Config
from seekwhence.trace import task_line

__all__ = ["FIELDS", "RunStats", "config_note", "number_text"]

# The fields and their titles, in the order the pane shows them (PANES.md P12; Numbo
# GUI, `stats_view.py`'s FIELDS).
FIELDS = (("state", "State"), ("run", "Run"), ("config", "Configuration"),
          ("events", "Events"), ("clock", "Clock (tasks run)"), ("task", "Task"),
          ("terms", "Terms"), ("waiting", "Tasks waiting"), ("reigning", "Reigning class"),
          ("threshold", "Threshold"), ("hypothesis", "Hypothesis"), ("validity", "Validity"),
          ("outcome", "Outcome"), ("summary", "Summary"))


def number_text(value) -> str:
    """A number as the pane prints it: floats with two decimals, None as nothing (Numbo
    GUI, `stats_view.py`)."""
    if value is None:
        return ""
    if isinstance(value, float):
        return f"{value:.2f}"
    return str(value)


def config_note(config_pairs) -> str:
    """`default` when the run's configuration (the `start` event's field-value pairs)
    equals `Config()`, else which fields differ: the stats pane shows when a run uses a
    non-default configuration (ANSWERS.md 5; G-016)."""
    defaults = Config()
    changed = [name for name, value in config_pairs
               if hasattr(defaults, name) and _plain(getattr(defaults, name)) != _plain(value)]
    if not changed:
        return "default"
    return f"edited: {', '.join(changed)}"


def _plain(value):
    if isinstance(value, dict):
        return tuple((k, _plain(v)) for k, v in value.items())
    if isinstance(value, (list, tuple)):
        return tuple(_plain(v) for v in value)
    return value


class RunStats:
    """The stats of the run whose events it observes (see the module docstring; Numbo
    GUI, `run_stats.py`; G-016). A `start` event resets it."""

    def __init__(self):
        self.title = ""
        self.reset()

    def reset(self) -> None:
        """Forget the run (Numbo GUI, `run_stats.py`)."""
        self.seed = None
        self.config = ""
        self.events = 0
        self.clock = 0
        self.task = ""
        self.terms = 0
        self.waiting = None
        self.reigning = None
        self.threshold = None
        self.hypothesis = None
        self.validity = None
        self.ended = None            # the `ended` or `capped` event
        self.summary = ""
        self.stopped = False

    def set_title(self, title: str) -> None:
        """The run's title (its preset), shown with the seed (G-016)."""
        self.title = title

    def on_event(self, event) -> None:
        """Apply one event (the observer protocol, DECISION D-055) (G-016)."""
        kind = event.kind
        if kind == "start":
            self.reset()
            self.seed = event.seed
            self.config = config_note(event.config)
        self.events += 1
        if kind == "chosen":
            self.clock = event.clock
            self.task = task_line(event.type, event.args)[len("doing task "):]
        elif kind == "term":
            self.terms = event.n
        elif kind in ("hypothesis", "candidate"):
            self.hypothesis = event.form
        elif kind in ("ended", "capped"):
            self.ended = event

    def update(self, snapshot) -> None:
        """Read what the events do not carry from the snapshot in force: the tasks
        waiting, the reigning class, the threshold, the hypothesis and its validity
        (ENGINE-API.md section 2; G-016). None leaves them unknown."""
        if snapshot is None:
            self.waiting = self.reigning = self.threshold = self.validity = None
            return
        self.waiting = len(snapshot.rack.tasks)
        self.reigning = snapshot.reformulation.reigning
        self.threshold = snapshot.reformulation.threshold
        hypothesis = snapshot.hypothesis
        if hypothesis is None:
            self.validity = None
        else:
            self.hypothesis = hypothesis.form
            self.validity = hypothesis.validity

    def finish(self, outcome) -> None:
        """The run controller's `RunOutcome`: the summary line of a session that ended,
        or `stopped` (Numbo GUI, `stats_view.py`'s `finish`; G-016)."""
        self.stopped = outcome.stopped
        result = outcome.result
        if outcome.stopped:
            self.summary = f"stopped after {outcome.events} events"
        elif outcome.error is not None:
            self.summary = f"error: {outcome.error!r}"
        elif isinstance(result, dict) and result.get("summary"):
            self.summary = result["summary"]

    def outcome_text(self) -> str:
        """`ok` (a guess accepted), `capped`, `stopped`, or nothing while the run goes on
        (DECISION D-069; G-016)."""
        if self.stopped:
            return "stopped"
        if self.ended is None:
            return ""
        if self.ended.kind == "capped":
            return f"capped at {self.ended.clock} tasks"
        return self.ended.reason

    def run_text(self) -> str:
        """The title and the seed, `run 1: 3 7 3 7, seed 54` (G-016)."""
        if self.seed is None:
            return self.title
        return f"{self.title}, seed {self.seed}" if self.title else f"seed {self.seed}"

    def texts(self, state: str = "") -> dict:
        """Every field of `FIELDS` as text (Numbo GUI, `stats_view.py`'s `texts`)."""
        return {"state": state, "run": self.run_text(), "config": self.config,
                "events": str(self.events), "clock": str(self.clock), "task": self.task,
                "terms": str(self.terms), "waiting": number_text(self.waiting),
                "reigning": self.reigning or "", "threshold": number_text(self.threshold),
                "hypothesis": self.hypothesis or "", "validity": number_text(self.validity),
                "outcome": self.outcome_text(), "summary": self.summary}

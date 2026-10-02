"""The event log's model (not 1987 source; loop0003).  Qt-free.

EventLog is an observer that keeps every event of a run (a start event
begins a new log), so it is also the run's history: what a replay plays and
what the timeline scrubs over.  It keeps, as events come:
  - EVENTS, the events in order (an event's index is its place here);
  - COUNTS, the number of events of each kind;
  - the index of each iteration's event (iteration_index, iteration_at);
  - VISIBLE, the indexes of the events whose kind is not HIDDEN (the log
    view's rows), kept up as events come and rebuilt by set_hidden.
Each costs O(1) per event, so a long run's log stays cheap.

event_text(event) says in one line what an event is about; the view
computes it only for the rows it paints.
"""

import bisect

from numbo import observe
from numbo.models.coderack_model import _arg_text, form_text
from numbo.models.run_stats import problem_text

__all__ = ["KINDS", "MAX_TEXT", "event_text", "EventLog"]

# Every event kind, in observe.py's order.
KINDS = tuple(observe.EVENT_TYPES)

MAX_TEXT = 200

TYPE_NAMES = {"1t": "target", "2b": "brick", "3dt": "derived target", "4bl": "block",
              "5g": "operation"}


def _number(x):
    return f"{x:.2f}" if isinstance(x, float) else _arg_text(x)


def _plural(n, word):
    return f"{n} {word}" if n == 1 else f"{n} {word}s"


def _value_text(field, value):
    if value is None:
        return "none"
    if field == "neighbors":
        return ", ".join(f"{name} ({link})" for name, link in value)
    if field == "instances":
        return " ".join(name for _, name in value)
    if field == "plinks":
        return " ".join(map(str, value))
    return _arg_text(value)


def _choice(when, codelet, args, urgency):
    if codelet is None:
        return f"{when}: nothing (the rack was empty)"
    return f"{when}: {form_text(codelet, args)} (urgency {_arg_text(urgency)})"


def _step(s):
    return f"{s.result} = {s.a} {s.op} {s.b}"


def _text(e):
    k = e.kind
    if k == "start":
        cap = ("no cap" if e.max_iterations is None
               else f"at most {e.max_iterations} iterations")
        return f"{problem_text(e.problem)}, seed {e.seed}, {cap}"
    if k == "rng":
        return f"draw {e.n}: {e.value}"
    if k == "setup-choose":
        return _choice("set-up", e.codelet, e.args, e.urgency)
    if k == "iteration":
        waiting = sum(count for _, count in e.rack)
        return (f"iteration {e.n}: x {_number(e.x)}, temperature {_number(e.temperature)}, "
                f"{_plural(waiting, 'codelet')} waiting")
    if k == "codelet-chosen":
        return _choice(f"iteration {e.n}", e.codelet, e.args, e.urgency)
    if k == "post":
        return f"{form_text(e.codelet, e.args)} (urgency {_arg_text(e.urgency)})"
    if k == "node-created":
        kind = TYPE_NAMES.get(e.type, e.type)
        return (f"{e.name}: {kind} {_arg_text(e.value)}, {_arg_text(e.status)}, "
                f"activation {_arg_text(e.activation)}")
    if k == "op-node-created":
        return f"{e.name}: {e.result} = {e.op} of {' '.join(e.operands)}"
    if k == "node-changed":
        return f"{e.name} {e.field}: {_value_text(e.field, e.value)}"
    if k == "current-target":
        if e.name is None:
            return "none"
        return f"{e.name} (interest {_arg_text(e.interest)})"
    if k == "disconnect":
        kind = TYPE_NAMES.get(e.node_type, e.node_type)
        return f"{e.node} ({kind} {_arg_text(e.node_value)}) and {e.op}"
    if k == "node-killed":
        return f"{e.name} ({_plural(len(e.cytoplasm), 'node')} left)"
    if k == "target-replaced":
        if e.rebound:
            return f"{e.block} is the target (Obvious.)"
        return f"{e.block} replaces {e.target}"
    if k == "pnet":
        active = sum(1 for a in e.activations if a)
        return f"spread: {active} of {len(e.activations)} pnodes active"
    if k == "pnet-initialized":
        return f"{len(e.activations)} pnodes reset"
    if k == "pnodes-changed":
        values = ", ".join(f"{name} ({_value_text('instances', v)})" if e.field == "instances"
                           else f"{name} {_arg_text(v)}" for name, v in e.values)
        return f"{e.field}: {values}"
    if k == "coderack-created":
        return f"{e.name}, urgencies {' '.join(map(_arg_text, e.levels))}"
    if k == "rack-emptied":
        return "the coderack is empty"
    if k == "decomposition":
        return f"{_plural(len(e.steps), 'step')}: " + "; ".join(map(_step, e.steps))
    if k == "done":
        return f"solved after {e.iterations} iterations"
    if k == "gave-up":
        return f"gave up after {e.iterations} iterations"
    if k == "capped":
        return f"capped at {e.iterations} iterations"
    if k == "error":
        return f"error after {e.iterations} iterations: {e.message}"
    return k


def event_text(event):
    """EVENT in one line (at most MAX_TEXT characters)."""
    text = " ".join(_text(event).split())
    if len(text) > MAX_TEXT:
        text = text[:MAX_TEXT - 1] + "…"
    return text


class EventLog:
    """A run's events (see the module docstring)."""

    def __init__(self):
        self.hidden = frozenset()
        self.clear()

    def clear(self):
        self.events = []
        self.visible = []
        self.counts = {}
        self._iteration_ns = []         # iteration numbers, in event order
        self._iteration_at = []         # their events' indexes
        self._iteration_index = {}      # n -> index

    def __len__(self):
        return len(self.events)

    def on_event(self, event):
        """Add EVENT (a start event begins a new log); returns whether its
        row is shown."""
        if event.kind == "start" and self.events:
            self.clear()
        index = len(self.events)
        self.events.append(event)
        kind = event.kind
        self.counts[kind] = self.counts.get(kind, 0) + 1
        if kind == "iteration":
            self._iteration_ns.append(event.n)
            self._iteration_at.append(index)
            self._iteration_index.setdefault(event.n, index)
        if kind in self.hidden:
            return False
        self.visible.append(index)
        return True

    def load(self, events):
        """The log of EVENTS (a whole run)."""
        self.clear()
        for event in events:
            self.on_event(event)

    def set_hidden(self, kinds):
        """Hide the events of KINDS (and show the others)."""
        self.hidden = frozenset(kinds)
        hidden = self.hidden
        self.visible = [i for i, e in enumerate(self.events) if e.kind not in hidden]

    def row_of(self, index):
        """The row of the last shown event at or before INDEX (None if
        there is none)."""
        row = bisect.bisect_right(self.visible, index) - 1
        return row if row >= 0 else None

    def iteration_index(self, n):
        """The index of iteration N's event (None if there is none)."""
        return self._iteration_index.get(n)

    def iteration_at(self, index):
        """The iteration under way at event INDEX (None before the first)."""
        k = bisect.bisect_right(self._iteration_at, index) - 1
        return self._iteration_ns[k] if k >= 0 else None

    def iterations(self):
        """(first, last) iteration numbers (None if there is none yet)."""
        if not self._iteration_ns:
            return None
        return (self._iteration_ns[0], self._iteration_ns[-1])

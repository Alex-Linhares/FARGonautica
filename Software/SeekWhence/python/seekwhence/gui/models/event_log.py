"""The event log: a run's history, as the Numbo GUI's `EventLog` (`numbo/models/
event_log.py`), with snapshots beside the events (G-003, G-005). Qt-free.

`EventLog` keeps, as events come (each costs O(1) per event, so a Blackburn run's
100,000 events stay cheap):
  - `events`, the events in order (an event's index is its place here); a `start` event
    begins a new log;
  - `counts`, the number of events of each kind;
  - the index of each term's `term` event (`term_index`, `term_at`), in place of Numbo's
    iteration (the Seek-Whence run is paced by terms [MHN p. 3] and by tasks [M86 p. 41]);
  - the index of each clock's `chosen` event (`clock_index`, `clock_at`);
  - `snapshots`, the `Snapshot` recorded at some events (`record`), and `state_at(index)`,
    the snapshot in force at an event: the last one taken at or before it;
  - `visible`, the indexes of the events whose kind is not hidden (the log view's rows).
    The tap appends to it on the worker thread while the log view's Kinds filter rebuilds
    it on the GUI thread, so both go through one lock (G-044).

`GROUPS` names the kinds by the sections of `seekwhence/events.py`, for the log view's
filter (PANES.md P9: run, taskrack, cytoplasm, templates, gnoths, filtering,
reformulation).

It is also an observer (`on_event`), so it can be fed directly, without snapshots.
`event_text(event)` says in one line what an event is about; the log view computes it only
for the rows it paints (Numbo GUI, `log_view.py`).
"""

import bisect
import threading

from seekwhence import events as ev
from seekwhence.trace import event_record, item_text, task_line

__all__ = ["KINDS", "GROUPS", "MAX_TEXT", "event_text", "group_of", "EventLog"]

# Every event kind, in events.py's order (DECISION D-055).
KINDS = tuple(ev.EVENT_TYPES)

# The kinds by group, the sections of events.py (PANES.md P9; G-044).
GROUPS = (
    ("run", ("start", "term", "prompt", "capped", "message", "ended")),
    ("taskrack", ("posted", "chosen", "stale", "cleaned")),
    ("cytoplasm", ("glint", "spark", "bond", "dissolved", "proposed", "glomtester-failed",
                   "glom", "glom-failed", "dubbed", "burst", "top-down")),
    ("templates", ("template-glom", "template", "template-check", "template-accepted",
                   "template-abandoned", "template-filtered", "template-modified", "blast")),
    ("gnoths", ("gnoth", "catchall", "gnoths-constructed")),
    ("filtering", ("hypothesis", "filtered", "refuted", "guess", "answered")),
    ("reformulation", ("freeze-dried", "gnoth-filled", "gnoths-set", "slip-check",
                       "class-changed", "operation-glom", "gnoth-gone", "gnoth-uncapped",
                       "gnoth-capped", "operation", "operated", "move-found", "reformulated",
                       "bonds-assessed", "casts", "candidate", "cast-failed", "slip-scouted",
                       "blasted")),
)


def group_of(kind: str) -> str | None:
    """The group a kind belongs to, or None for a kind `GROUPS` does not name (G-044)."""
    for group, kinds in GROUPS:
        if kind in kinds:
            return group
    return None

MAX_TEXT = 200


def _field_text(value) -> str:
    """A field's value in one piece: forms and tuples as the trace prints arguments
    (DECISION D-055)."""
    if isinstance(value, float):
        return f"{value:g}"
    if isinstance(value, str):
        return value
    return item_text(value)


def event_text(event) -> str:
    """The event in one line (at most `MAX_TEXT` characters): the `doing task` line for a
    `chosen` event [M86 p. 164], otherwise its fields as `name value` pairs in the order
    of `event_record` (Numbo GUI, `event_log.py`)."""
    if event.kind == "chosen":
        text = f"{task_line(event.type, event.args)} (clock {event.clock})"
    else:
        record = event_record(event)
        parts = [f"{name} {_field_text(getattr(event, name))}"
                 for name in record if name != "kind"]
        text = ", ".join(parts) if parts else event.kind
    text = " ".join(text.split())
    if len(text) > MAX_TEXT:
        text = text[:MAX_TEXT - 1] + "…"
    return text


class EventLog:
    """A run's events and snapshots (see the module docstring; Numbo GUI, `event_log.py`;
    G-005)."""

    def __init__(self):
        self.hidden = frozenset()
        self._visible_lock = threading.Lock()      # G-044
        self.clear()

    def clear(self) -> None:
        """Forget the run (Numbo GUI, `event_log.py`)."""
        self.events = []
        self.visible = []
        self.counts = {}
        self.snapshots = {}             # event index -> Snapshot
        self._snapshot_at = []          # the indexes with a snapshot, ascending
        self._term_ns = []              # term numbers, in event order
        self._term_at = []              # their events' indexes
        self._term_index = {}           # n -> index
        self._clocks = []               # clocks of the chosen events, in event order
        self._clock_at = []             # their events' indexes
        self._clock_index = {}          # clock -> index

    def __len__(self) -> int:
        return len(self.events)

    def on_event(self, event) -> bool:
        """Add the event without a snapshot (the observer protocol, DECISION D-055);
        returns whether its row is shown."""
        return self.record(event, None)

    def record(self, event, snapshot=None) -> bool:
        """Add the event, with the snapshot taken at it if any (a `start` event begins a
        new log); returns whether its row is shown (Numbo GUI, `event_log.py`)."""
        if event.kind == "start" and self.events:
            self.clear()
        index = len(self.events)
        self.events.append(event)
        kind = event.kind
        self.counts[kind] = self.counts.get(kind, 0) + 1
        if kind == "term":
            self._term_ns.append(event.n)
            self._term_at.append(index)
            self._term_index.setdefault(event.n, index)
        elif kind == "chosen":
            self._clocks.append(event.clock)
            self._clock_at.append(index)
            self._clock_index.setdefault(event.clock, index)
        if snapshot is not None:
            self.snapshots[index] = snapshot
            self._snapshot_at.append(index)
        with self._visible_lock:
            if kind in self.hidden:
                return False
            self.visible.append(index)
        return True

    def load(self, events) -> None:
        """The log of a whole run's events, without snapshots (Numbo GUI,
        `event_log.py`)."""
        self.clear()
        for event in events:
            self.record(event)

    # ---- rows ----------------------------------------------------------------------------

    def set_hidden(self, kinds) -> None:
        """Hide the events of these kinds and show the others (Numbo GUI,
        `event_log.py`); safe against the tap recording meanwhile (G-044)."""
        with self._visible_lock:
            self.hidden = frozenset(kinds)
            hidden = self.hidden
            self.visible = [i for i, e in enumerate(self.events) if e.kind not in hidden]

    def row_of(self, index: int) -> int | None:
        """The row of the last shown event at or before the index, or None (Numbo GUI,
        `event_log.py`)."""
        row = bisect.bisect_right(self.visible, index) - 1
        return row if row >= 0 else None

    # ---- terms and clocks ------------------------------------------------------------------

    def term_index(self, n: int) -> int | None:
        """The index of term n's `term` event, or None [MHN p. 3]."""
        return self._term_index.get(n)

    def term_at(self, index: int) -> int | None:
        """The number of the last term entered at or before the index, or None before the
        first [MHN p. 3]."""
        k = bisect.bisect_right(self._term_at, index) - 1
        return self._term_ns[k] if k >= 0 else None

    def terms(self) -> tuple | None:
        """`(first, last)` term numbers entered, or None when none yet (Numbo GUI,
        `event_log.py`'s `iterations`)."""
        if not self._term_ns:
            return None
        return (self._term_ns[0], self._term_ns[-1])

    def clock_index(self, clock: int) -> int | None:
        """The index of the `chosen` event of the task with this clock, or None
        [M86 p. 41]."""
        return self._clock_index.get(clock)

    def clock_at(self, index: int) -> int | None:
        """The clock of the task under way at the index (the last `chosen` at or before
        it), or None before the first [M86 p. 41]."""
        k = bisect.bisect_right(self._clock_at, index) - 1
        return self._clocks[k] if k >= 0 else None

    def clocks(self) -> tuple | None:
        """`(first, last)` clocks of the tasks chosen, or None when none yet [M86 p. 41]."""
        if not self._clocks:
            return None
        return (self._clocks[0], self._clocks[-1])

    # ---- snapshots -------------------------------------------------------------------------

    @property
    def last_snapshot(self):
        """The most recent snapshot, or None (G-003)."""
        return self.snapshots[self._snapshot_at[-1]] if self._snapshot_at else None

    def snapshot_index(self, index: int) -> int | None:
        """The index of the last event at or before the index that has a snapshot, or
        None (G-003)."""
        k = bisect.bisect_right(self._snapshot_at, index) - 1
        return self._snapshot_at[k] if k >= 0 else None

    def state_at(self, index: int):
        """The snapshot in force at the event: the last one taken at or before it, or None
        before the first (G-003; the timeline's seek, Numbo GUI, `timeline.py`)."""
        at = self.snapshot_index(index)
        return None if at is None else self.snapshots[at]

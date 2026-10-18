"""The event log: a run's history, as the Numbo GUI's `EventLog` (`numbo/models/
event_log.py`) through the Seek-Whence GUI's, with the snapshots beside the events
(G-006). Qt-free.

`EventLog` keeps, as events come (each costs O(1) per event, so a capped run's 20,000
events stay cheap):
  - `events`, the engine's events in order (an event's index is its place here); a
    `RunStarted` event begins a new log;
  - `records`, the JSON record of each event (`tabletop.events.record_of`), kept beside
    it so that a session file and the determinism contract read what the log holds;
  - `counts`, the number of events of each name;
  - the index of each codelet's `CodeletChosen` event (`codelet_index`, `codelet_at`), in
    place of Numbo's iteration: a Tabletop run is paced by codelets [F95 p. 77], and
    `CodeletChosen.clock` is the codelet's ordinal (`docs/engine-api.md`);
  - `snapshots`, a `SnapshotHistory` of the engine's `World.snapshot()`s at some events,
    and `state_at(index)`, the snapshot in force at an event;
  - `visible`, the indexes of the events whose name is not hidden (the log view's rows).
    The tap appends to it on the worker thread while a view's filter rebuilds it on the
    GUI thread, so both go through one lock (Seek-Whence GUI, `event_log.py`).

It is also an observer (`on_event`), so it can be fed directly, without snapshots.

The log pane's rows (PANES.md P10; G-042): `row_text(index, record)` is one event's row,
`event_text(record)` its fields after the three every record has, and `json_lines(history)`
the whole history as the CLI's `--json` output, byte for byte (`observe.json_line` per
event, each line ending in a newline; D-065).
"""

import bisect
import json
import threading

from tabletop import events as ev
from tabletop.gui.models.snapshot import SnapshotHistory
from tabletop.observe import json_line

__all__ = ["NAMES", "EventLog", "event_text", "row_text", "json_lines"]

# Every event name, in events.py's order (D-065).
NAMES = tuple(cls.__name__ for cls in ev.EVENT_TYPES)


class EventLog:
    """A run's events, their records and its snapshots (see the module docstring; Numbo
    GUI, `event_log.py`; G-006)."""

    def __init__(self, snapshots=None):
        self.hidden = frozenset()
        self._visible_lock = threading.Lock()
        self.snapshots = SnapshotHistory() if snapshots is None else snapshots
        self.clear()

    def clear(self) -> None:
        """Forget the run (Numbo GUI, `event_log.py`)."""
        self.events = []
        self.records = []
        self.visible = []
        self.counts = {}
        self.snapshots.clear()
        self._clocks = []               # the clocks of the CodeletChosen events, in order
        self._clock_at = []             # their events' indexes
        self._clock_index = {}          # clock -> index

    def __len__(self) -> int:
        return len(self.events)

    def on_event(self, event) -> bool:
        """Add the event without a snapshot (the observer protocol, D-065); returns
        whether its row is shown."""
        return self.record(event)

    def record(self, event) -> bool:
        """Add the event and its JSON record (a `RunStarted` event begins a new log);
        returns whether its row is shown (Numbo GUI, `event_log.py`)."""
        name = event.name
        if name == "RunStarted" and self.events:
            self.clear()
        index = len(self.events)
        self.events.append(event)
        self.records.append(ev.record_of(event))
        self.counts[name] = self.counts.get(name, 0) + 1
        if name == "CodeletChosen":
            self._clocks.append(event.clock)
            self._clock_at.append(index)
            self._clock_index.setdefault(event.clock, index)
        with self._visible_lock:
            if name in self.hidden:
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

    def set_hidden(self, names) -> None:
        """Hide the events of these names and show the others (Numbo GUI,
        `event_log.py`); safe against the tap recording meanwhile (Seek-Whence GUI,
        `event_log.py`)."""
        with self._visible_lock:
            self.hidden = frozenset(names)
            hidden = self.hidden
            self.visible = [i for i, e in enumerate(self.events) if e.name not in hidden]

    def row_of(self, index: int) -> int | None:
        """The row of the last shown event at or before the index, or None (Numbo GUI,
        `event_log.py`)."""
        row = bisect.bisect_right(self.visible, index) - 1
        return row if row >= 0 else None

    # ---- codelets ------------------------------------------------------------------------

    def codelet_index(self, clock: int) -> int | None:
        """The index of the `CodeletChosen` event of the codelet with this ordinal, or
        None [F95 p. 77] (Numbo GUI, `event_log.py`'s `iteration_index`)."""
        return self._clock_index.get(clock)

    def codelet_at(self, index: int) -> int:
        """The ordinal of the codelet under way at the index (the last `CodeletChosen` at
        or before it), 0 during the start, before the first [F95 p. 77] (Numbo GUI,
        `event_log.py`'s `iteration_at`)."""
        k = bisect.bisect_right(self._clock_at, index) - 1
        return self._clocks[k] if k >= 0 else 0

    def codelets(self) -> tuple | None:
        """`(first, last)` ordinals of the codelets chosen, or None when none yet
        (Numbo GUI, `event_log.py`'s `iterations`)."""
        if not self._clocks:
            return None
        return (self._clocks[0], self._clocks[-1])

    # ---- snapshots -----------------------------------------------------------------------

    @property
    def last_snapshot(self):
        """The most recent snapshot, or None (G-006)."""
        return self.snapshots.last

    def snapshot_index(self, index: int) -> int | None:
        """The index of the last event at or before the index that has a snapshot, or
        None (G-006)."""
        return self.snapshots.index_at(index)

    def state_at(self, index: int):
        """The snapshot in force at the event: the last one taken at or before it, or None
        before the first (G-006; the timeline's seek, Numbo GUI, `timeline.py`)."""
        return self.snapshots.state_at(index)

    def terms_at(self, index: int) -> dict:
        """The correspondences' terms recorded beside the snapshot in force at the event
        (G-020); empty before the first."""
        return self.snapshots.terms_at(index)

    def salience_at(self, index: int) -> dict:
        """The salience readings recorded beside the snapshot in force at the event
        (G-024): the terms by item and the region order; empty before the first."""
        return self.snapshots.salience_at(index)

    def worldview_at(self, index: int) -> dict:
        """The Worldview readings recorded beside the snapshot in force at the event
        (G-039): the rivals of every correspondence; empty before the first."""
        return self.snapshots.worldview_at(index)

    def invariants_at(self, index: int):
        """The invariants' verdict recorded beside the snapshot in force at the event
        (G-045): `faults`, the sentences of `invariants.check`; None before the first."""
        return self.snapshots.invariants_at(index)


# ---- the rows (PANES.md P10; Numbo GUI, `log_view.py`'s `row_text`) -------------------------


def _value_text(value) -> str:
    if isinstance(value, (list, tuple, dict)):
        return json.dumps(value, separators=(",", ":"))
    if isinstance(value, float):
        return f"{value:g}"
    return str(value)


def event_text(record: dict) -> str:
    """An event's fields after `event`, `clock` and `draws`, as `field=value` in the
    record's order; compound values as compact JSON (G-042; D-065)."""
    return "  ".join(f"{k}={_value_text(v)}" for k, v in record.items()
                     if k not in ("event", "clock", "draws"))


def row_text(index: int, record: dict) -> str:
    """One row of the Events tab: the event's index, its name, its clock and its fields
    (Numbo GUI, `log_view.py`'s `row_text`; G-042)."""
    return f"{index:>6}  {record['event']:<22} {record['clock']:>5}  {event_text(record)}"


def json_lines(history) -> str:
    """The history as the CLI's `--json` output: every event's JSON line followed by a
    newline, in order (`docs/trace-format.md`; D-065; G-042)."""
    return "".join(json_line(event) + "\n" for event in history.events)

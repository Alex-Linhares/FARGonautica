"""The tap: the GUI's observer of a run, which records every event in an `EventLog` and a
read-only `Snapshot` of the `World` at task boundaries (ENGINE-API.md section 2; G-003,
G-004).

Observers are called synchronously, inside the engine, so at the moment the tap receives
`TaskChosen` (published before the task runs, `World.run_task`), `TermRequested`,
`GuessMade`, `GuessAnswered` or `RunEnded` the world is exactly the state the event
describes. The tap reads it then (`snapshot`) and nothing else: it never draws from the
`Rng`, never runs a task and keeps no engine object past the call, so a run with the tap
gives the same events, trace and draws as one without (tested).

When the tap reads (G-003, measured in loop0004 item 01): a snapshot costs about a
quarter of a millisecond and a task about thirty microseconds, so a snapshot at every
task boundary would make a long run many times slower. The tap therefore takes one at
every `chosen` event while the clock is at most `DENSE_TASKS` (every appendix run has
under 200 tasks [M86 pp. 164-179], so those are exact at every task), and after that a
keyframe at the first `chosen` event `KEYFRAME` or more events after the last snapshot,
the Numbo GUI's `LayoutHistory` rule; the `prompt`, `guess`, `answered` and `ended`
events always get one. A pane model shown between two snapshots starts from the earlier
one and applies the events after it (`EventLog.state_at`).

A `Session` makes the `World` itself, so the tap is given it after the session is built
(`attach`, or `attach_session` as the run controller's `on_session` callback): the `start`
event, published while the session is being made, is recorded without a snapshot (G-004).
The hypothesis's source, which the object does not carry, is tracked from the `hypothesis`
and `candidate` events (G-006).
"""

from seekwhence.gui.models.snapshot import SnapshotCache, snapshot

__all__ = ["ALWAYS", "DENSE_TASKS", "KEYFRAME", "Tap"]

# The events that always get a snapshot: the prompt, the guess, its answer, the end
# (ENGINE-API.md section 2; G-003).
ALWAYS = ("prompt", "guess", "answered", "ended")

# Every `chosen` event gets a snapshot while the clock is at most this (G-003).
DENSE_TASKS = 500

# Afterwards, a keyframe at the first `chosen` event this many events after the last
# snapshot (Numbo GUI, `layout_history.py`'s EVERY; G-003).
KEYFRAME = 64


class Tap:
    """An observer that feeds an `EventLog` (`seekwhence.gui.models.event_log`): every
    event, and a snapshot of `world` at task boundaries as the module docstring says
    (G-003, G-004). `dense_tasks` and `keyframe` are the module's defaults unless given
    (tests set them)."""

    def __init__(self, log, world=None, dense_tasks=DENSE_TASKS, keyframe=KEYFRAME):
        self.log = log
        self.world = world
        self.dense_tasks = dense_tasks
        self.keyframe = keyframe
        self.source = None
        self.cache = SnapshotCache()
        self._last = None          # the index of the last snapshot in this run

    def attach(self, world) -> None:
        """Give the tap the `World` to read; events before this are recorded without a
        snapshot (G-004)."""
        self.world = world

    def attach_session(self, session) -> None:
        """`attach` the session's world: the run controller's `on_session` callback, called
        on the worker right after the `Session` is built (G-004)."""
        self.attach(session.world)

    def wants_snapshot(self, event, index: int) -> bool:
        """Whether the event at this index gets a snapshot (G-003): always for `ALWAYS`;
        for a `chosen` event while the clock is at most `dense_tasks`, and afterwards
        when `keyframe` or more events have passed since the last snapshot."""
        kind = event.kind
        if kind in ALWAYS:
            return True
        if kind != "chosen":
            return False
        if event.clock <= self.dense_tasks:
            return True
        return self._last is None or index - self._last >= self.keyframe

    def on_event(self, event) -> None:
        """Record the event, with a snapshot of the world when `wants_snapshot` says so
        and the world is attached (G-003)."""
        kind = event.kind
        if kind == "start":
            self.source = None
            self.cache = SnapshotCache()
            self._last = None
        elif kind == "hypothesis":
            self.source = event.source
        elif kind == "candidate":
            self.source = "candidate"
        index = len(self.log.events) if kind != "start" else 0
        snap = None
        if self.world is not None and self.wants_snapshot(event, index):
            snap = snapshot(self.world, self.log.last_snapshot, self.source, self.cache)
            self._last = index
        self.log.record(event, snap)

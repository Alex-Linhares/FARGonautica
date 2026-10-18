"""The history of a run's snapshots: the engine's own `World.snapshot()`, read-only, kept
at the events where the tap takes one, with the rule that says when (G-006, G-007).
Qt-free.

The snapshot itself is the engine's (`tabletop.snapshot.Snapshot`, a frozen dataclass of
plain data that draws nothing and stores nothing; `docs/engine-api.md`, "The read-only
state"): this module copies nothing else out of the `World`, and keeps no engine object.
It keeps the snapshots under the index of the event they describe, and answers
`state_at(index)`, the snapshot in force at an event: the last one taken at or before it
(the Numbo GUI's `layout_history.py` answers the same question for its tree layouts).

When a snapshot is taken (G-007, measured in loop0006 item 01 on a run forced to the cap
of 3000 codelets, D-061): one at the end of the start, before the first codelet; one after
every `World.step()` while the clock is at most `DENSE_CODELETS` (the runs of RESULTS.md
run 44 to 67 codelets on average and none of 5,000 under `standard` was capped, so nearly
every run is exact at every codelet); after that a keyframe every `KEYFRAME_STEPS` steps,
the Numbo GUI's `LayoutHistory` rule; and always one at the end of the run. A pane shown
between two keyframes starts from the earlier one and applies the events after it.

Beside each snapshot the tap may record the correspondences' `terms` (G-020): the nine
static and three dynamic factors of every correspondence, as (name, value) pairs under the
correspondence's name, read through `correspondences.static_terms` and `dynamic_terms`,
which the snapshot does not carry; `terms_at(index)` answers as `state_at` does. It may
also record the `salience` readings of the table's overlays (G-024): the nine terms of
every item in view and the nine regions ranked from the touched object, read through
`salience.terms` and `salience.region_order`; `salience_at(index)` answers likewise. And the
`invariants` verdict (G-045): the sentences of `invariants.check` (D-077), empty when every
invariant holds; `invariants_at(index)` answers likewise.
"""

import bisect

__all__ = ["DENSE_CODELETS", "KEYFRAME_STEPS", "SnapshotHistory"]

# A snapshot after every step while the clock is at most this (G-007).
DENSE_CODELETS = 300

# Afterwards, a keyframe every this many steps (Numbo GUI, `layout_history.py`; G-007).
KEYFRAME_STEPS = 16


class SnapshotHistory:
    """The snapshots of one run under the indexes of the events they describe, with the
    keyframe rule (G-006, G-007). `dense_codelets` and `keyframe_steps` are the module's
    defaults unless given (tests lower them to force keyframes on a short run)."""

    def __init__(self, dense_codelets=DENSE_CODELETS, keyframe_steps=KEYFRAME_STEPS):
        self.dense_codelets = dense_codelets
        self.keyframe_steps = keyframe_steps
        self.clear()

    def clear(self) -> None:
        """Forget the run (Numbo GUI, `event_log.py`'s `clear`)."""
        self.by_index = {}          # event index -> Snapshot
        self.terms_by_index = {}    # event index -> {cp name: {"static": ..., "dynamic": ...}}
        self.salience_by_index = {} # event index -> {"terms": {item: pairs}, "region_order": pairs}
        self.worldview_by_index = {}  # event index -> {"rivals": {name: names}} (G-039)
        self.invariants_by_index = {}  # event index -> {"faults": sentences} (G-045)
        self._indexes = []          # the indexes with a snapshot, ascending
        self._steps_since = 0       # steps since the last snapshot

    def __len__(self) -> int:
        return len(self.by_index)

    @property
    def indexes(self) -> list:
        """The indexes of the events with a snapshot, ascending (G-006)."""
        return list(self._indexes)

    @property
    def last(self):
        """The most recent snapshot, or None (G-006)."""
        return self.by_index[self._indexes[-1]] if self._indexes else None

    def wants_after_step(self, clock: int, finished: bool) -> bool:
        """Whether the step that just ended gets a snapshot (G-007): always while the
        clock is at most `dense_codelets` and at the end of the run, afterwards every
        `keyframe_steps` steps."""
        self._steps_since += 1
        if finished or clock <= self.dense_codelets:
            return True
        return self._steps_since >= self.keyframe_steps

    def record(self, index: int, snapshot, terms=None, salience=None, worldview=None,
               invariants=None) -> None:
        """Keep the snapshot describing the state after the event at this index, with the
        correspondences' terms (G-020), the salience readings (G-024), the Worldview
        readings (G-039) and the invariants' verdict (G-045) read beside it; an index
        recorded twice keeps the later snapshot (G-006)."""
        if index not in self.by_index:
            bisect.insort(self._indexes, index)
        self.by_index[index] = snapshot
        self.terms_by_index[index] = {} if terms is None else terms
        self.salience_by_index[index] = ({"terms": {}, "region_order": ()} if salience is None
                                         else salience)
        self.worldview_by_index[index] = {"rivals": {}} if worldview is None else worldview
        self.invariants_by_index[index] = {"faults": ()} if invariants is None else invariants
        self._steps_since = 0

    def index_at(self, index: int) -> int | None:
        """The index of the last event at or before `index` that has a snapshot, or None
        before the first (G-006)."""
        k = bisect.bisect_right(self._indexes, index) - 1
        return self._indexes[k] if k >= 0 else None

    def state_at(self, index: int):
        """The snapshot in force at the event: the last one taken at or before it, or None
        before the first (G-006; Numbo GUI, `layout_history.py`'s `layout_at`)."""
        at = self.index_at(index)
        return None if at is None else self.by_index[at]

    def terms_at(self, index: int) -> dict:
        """The correspondences' terms recorded beside the snapshot in force at the event
        (G-020); empty before the first snapshot."""
        at = self.index_at(index)
        return {} if at is None else self.terms_by_index[at]

    def salience_at(self, index: int) -> dict:
        """The salience readings recorded beside the snapshot in force at the event
        (G-024): `terms` by item and the `region_order`; empty before the first
        snapshot."""
        at = self.index_at(index)
        return {"terms": {}, "region_order": ()} if at is None else self.salience_by_index[at]

    def worldview_at(self, index: int) -> dict:
        """The Worldview readings recorded beside the snapshot in force at the event
        (G-039): the `rivals` of every correspondence; empty before the first snapshot."""
        at = self.index_at(index)
        return {"rivals": {}} if at is None else self.worldview_by_index[at]

    def invariants_at(self, index: int):
        """The invariants' verdict recorded beside the snapshot in force at the event
        (G-045): `faults`, the sentences of `invariants.check`, empty when every invariant
        holds; None before the first snapshot (not checked yet)."""
        at = self.index_at(index)
        return None if at is None else self.invariants_by_index[at]

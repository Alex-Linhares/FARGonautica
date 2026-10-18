"""The tap: the GUI's observer of a run, which records every event in an `EventLog` and
takes the engine's `World.snapshot()` at the moments the design names: at the end of the
start, after each `World.step()` and at the end of the run (G-006, G-007; Seek-Whence
GUI, `tap.py`). Qt-free.

The tap is subscribed to the run controller, behind its gate (G-006), so the log holds
exactly the events the GUI has been handed, never the event the gate is holding while
paused. It reads the `World` only through `World.snapshot()`, which draws nothing and
changes nothing (`docs/engine-api.md`, "The read-only state"), and only when the
controller's worker tells it the world stands still: `attach(world)` right after the
`World` is built (the start of the run, before the first codelet) and `after_step(world)`
right after each `World.step()` returns. It keeps no engine object past the run: the
world is let go when the run has finished. A run with the tap gives the same events, draws
and report as one without (tested; the loop's gate checks it on every iteration).

Beside each snapshot the tap records the nine static and three dynamic terms of every
correspondence in the Workspace (`correspondence_terms`; G-020), through
`correspondences.static_terms` and `dynamic_terms`, which `docs/engine-api.md` lists as
drawing nothing, copied into plain tuples: the table's hover text shows them (PANES.md
P1), and the snapshot itself carries only their sums. In the same way it records the
salience readings of the table's overlays (`salience_readings`; G-024): the nine terms of
every item in view (`salience.terms`, D-031) and the nine regions ranked from the touched
object (`salience.region_order`, D-032), both listed by `docs/engine-api.md` as drawing
nothing; the snapshot carries the saliences' sums (`Snapshot.saliences`) but not the
terms, and no region order. And beside each snapshot it records the Worldview readings
of the Worldview pane (`worldview_readings`; G-039): the rivals of every correspondence in
the Workspace (`worldview.rivals_of`, D-044), which `docs/engine-api.md` lists as drawing
nothing; the snapshot carries the rating's pairs but not whom a correspondence outside the
Worldview would have to fight. And it records the invariants' verdict (`invariant_readings`;
G-045): `invariants.check` (D-077) on the state, which `docs/engine-api.md` names as the
GUI's own consistency check and which reads the state and draws nothing; the stats pane
shows its sentences, empty when every invariant holds.

The `RunStarted` event is published inside `World.__init__`, before the Coderack exists,
so the state at the start is read when the constructor has returned, and recorded under
the last event of the start (the starting rack, the Slipnet's first update, the touched
object noticed and the first temperature): `state_at` is None for the events before it
(G-006).
"""

from tabletop import correspondences as C
from tabletop import invariants as INV
from tabletop import salience as S
from tabletop import worldview as WV
from tabletop.gui.models.event_log import EventLog

__all__ = ["Tap", "correspondence_terms", "invariant_readings", "salience_readings",
           "worldview_readings"]


def correspondence_terms(world) -> dict:
    """The static and dynamic terms of every correspondence in the Workspace, as plain
    tuples of (name, value) pairs under the correspondence's name (G-020; D-036, D-037).
    Reads `correspondences.static_terms` and `dynamic_terms`, which draw nothing."""
    return {cp.name: {"static": tuple(C.static_terms(world, cp)),
                      "dynamic": tuple(C.dynamic_terms(world, cp))}
            for cp in world.workspace.correspondences}


def salience_readings(world, items) -> dict:
    """The salience readings of the P1 overlays beside a snapshot (G-024; D-031, D-032):
    `terms`, the nine factors of every item in view (French's eight and the role term, D-100) as a tuple of (name, value) pairs
    under the item's name, in French's order [F95 pp. 66-67] (`salience.terms`), and
    `region_order`, the nine regions from the most to the least preferred relative to
    the touched object, as (name, weight) pairs (`salience.region_order`). `items` are
    the names of the snapshot's `saliences`, so nothing is read twice. Both accessors
    draw nothing."""
    terms = {item: tuple((name, float(value)) for name, value in S.terms(world, item))
             for item in items}
    order = tuple((region.name, float(weight)) for region, weight in S.region_order(world))
    return {"terms": terms, "region_order": order}


def worldview_readings(world) -> dict:
    """The Worldview readings of the P5 pane beside a snapshot (G-039; D-044): `rivals`,
    the names of the Worldview correspondences each correspondence in the Workspace
    conflicts with, as a tuple under the correspondence's name, in the order built
    (`worldview.rivals_of`, which draws nothing): empty for one that "is not a rival to
    any correspondence already in the Worldview" [F95 p. 101]."""
    return {"rivals": {cp.name: tuple(r.name for r in WV.rivals_of(world, cp))
                       for cp in world.workspace.correspondences}}


def invariant_readings(world) -> dict:
    """The invariants' verdict beside a snapshot (G-045; D-077): `faults`, the sentences
    `invariants.check` returns for the state as it stands, as a tuple, empty when every
    invariant holds. The check reads the state and draws nothing (`tests/test_invariants.py`
    asserts the fingerprint), so a checked run is the same run."""
    return {"faults": tuple(INV.check(world))}


class Tap:
    """An observer that feeds an `EventLog` (`tabletop.gui.models.event_log`): every
    event, and a snapshot of the attached world at the moments the module docstring
    names (G-006, G-007)."""

    def __init__(self, log=None):
        self.log = EventLog() if log is None else log
        self.world = None

    def on_event(self, event) -> None:
        """Record the event (D-065); a `RunStarted` begins a new log."""
        self.log.record(event)

    def attach(self, world) -> None:
        """Give the tap the `World` to read, and take the snapshot of the start under the
        last event published so far (G-006), with the terms, the salience readings, the
        Worldview readings and the invariants' verdict beside it (G-020, G-024, G-039,
        G-045). The run controller's `on_world` callback, called on the worker right
        after the `World` is built."""
        self.world = world
        self._take()

    def after_step(self, world) -> None:
        """A `World.step()` has returned: take a snapshot when the rule says so (G-007),
        always at the end of the run, and let the world go once it has finished. The run
        controller's `on_step` callback, called on the worker."""
        if self.world is None:
            self.world = world
        if self.log.snapshots.wants_after_step(world.clock, world.finished):
            self._take()
        if world.finished:
            self.world = None

    def _take(self) -> None:
        if self.world is None or not self.log.events:
            return
        snapshot = self.world.snapshot()
        self.log.snapshots.record(len(self.log.events) - 1, snapshot,
                                  correspondence_terms(self.world),
                                  salience_readings(self.world,
                                                    [item for item, _ in snapshot.saliences]),
                                  worldview_readings(self.world),
                                  invariant_readings(self.world))

"""The observer core: a Subject, the Observer protocol, and the typed events
of a run (not 1987 source; loop0003).

A run publishes typed events through a Subject (events.py has the hooks
that publish them); everything else subscribes: the oracle JSON-lines trace
(trace.OracleTraceWriter), session recorders, models and views.  Observers
watch the run and never change its course: an observer that raises is
reported (Subject.errors, and on_error, by default a traceback on stderr)
and the run, and the other observers, go on as if nothing happened.

The events are frozen dataclasses of plain data (str, numbers, None, True,
tuples), one `kind` each, never engine objects: a cyto-node or a pnode is
its name, a Lisp symbol its name (a keyword ":NAME"), a Lisp string itself.
A codelet's arguments are in the oracle trace's Lisp-data encoding
(events.encode_data), as the trace has them.  They are a superset of the
oracle trace's events: every trace line is made from one or two of them.
"""

import dataclasses
import sys
import traceback
from typing import ClassVar, Protocol, runtime_checkable


@runtime_checkable
class Observer(Protocol):
    """Anything with on_event(event)."""

    def on_event(self, event: "Event") -> None: ...


@dataclasses.dataclass(frozen=True)
class ObserverError:
    """OBSERVER raised EXCEPTION on EVENT."""
    observer: object
    event: "Event"
    exception: BaseException


def report_to_stderr(error):
    """The default on_error: say which observer failed on which event, with
    the traceback, on stderr.  (A run controller's callbacks are reported
    with no event.)"""
    where = f"on a {error.event.kind} event" if error.event is not None else "outside an event"
    sys.stderr.write(f"numbo: observer {error.observer!r} failed {where}:\n")
    traceback.print_exception(error.exception, file=sys.stderr)


class Subject:
    """Publishes events to its observers, in subscription order."""

    def __init__(self, on_error=report_to_stderr):
        self._observers = []
        self.on_error = on_error
        self.errors = []

    @property
    def observers(self):
        return tuple(self._observers)

    def subscribe(self, observer):
        """Add OBSERVER (last); returns it."""
        if not isinstance(observer, Observer):
            raise TypeError(f"{observer!r} has no on_event method")
        self._observers.append(observer)
        return observer

    def unsubscribe(self, observer):
        """Remove OBSERVER (a ValueError if it is not subscribed)."""
        self._observers.remove(observer)

    def publish(self, event):
        """Give EVENT to every observer subscribed now.  An observer's
        Exception is recorded in `errors` and passed to on_error; it stops
        nothing."""
        for observer in tuple(self._observers):
            try:
                observer.on_event(event)
            except Exception as exception:
                error = ObserverError(observer, event, exception)
                self.errors.append(error)
                if self.on_error is not None:
                    self.on_error(error)

    # A Subject is itself an observer: subscribed to another, it passes the
    # events on to its own observers.
    on_event = publish


# ---------------------------------------------------------------------------
# Events

EVENT_TYPES = {}


def _event(cls):
    cls = dataclasses.dataclass(frozen=True)(cls)
    if "kind" in cls.__dict__:
        assert cls.kind not in EVENT_TYPES, cls.kind
        EVENT_TYPES[cls.kind] = cls
    return cls


@_event
class Event:
    """An event of a run."""
    kind: ClassVar[str]


@_event
class RunStarted(Event):
    """run_config began: PROBLEM (target b1 ... b5), SEED, MAX_ITERATIONS
    (None: no cap), the RNG's name, and the names of *pnet*'s pnodes."""
    kind = "start"
    problem: tuple
    seed: int
    max_iterations: object
    rng: str
    pnet: tuple


@_event
class RngDraw(Event):
    """The shared RNG's draw number N gave VALUE (outside a cr-choose; a
    choice's own draws are in its event)."""
    kind = "rng"
    n: int
    value: object


@_event
class SetupChoose(Event):
    """One of config's 13 set-up (cr-choose *coderack*): the CODELET chosen
    (a name, or None if the rack was empty) with its ARGS and URGENCY, the
    RACK before the choice ((urgency, count) per bin) and the DRAWS it made
    ((n, value) pairs)."""
    kind = "setup-choose"
    codelet: object
    args: object
    urgency: object
    rack: tuple
    draws: tuple


@_event
class IterationBegan(Event):
    """Main-loop iteration N began: X, the TEMPERATURE, and the RACK."""
    kind = "iteration"
    n: int
    x: object
    temperature: object
    rack: tuple


@_event
class CodeletChosen(Event):
    """Iteration N's cr-choose: the CODELET (None if the rack was empty),
    ARGS, URGENCY and the DRAWS made."""
    kind = "codelet-chosen"
    n: object
    codelet: object
    args: object
    urgency: object
    draws: tuple


@_event
class CodeletPosted(Event):
    """cr-hang posted CODELET with ARGS at URGENCY."""
    kind = "post"
    codelet: str
    args: tuple
    urgency: object


@_event
class NodeCreated(Event):
    """create-cyto-node is making the cyto-node NAME (published as the call
    begins, the oracle's order: the node is in the World, with these fields,
    by the next event).  TYPE "1t" target, "2b" brick, "3dt" derived target,
    "4bl" block."""
    kind = "node-created"
    name: str
    type: str
    value: object
    status: object
    level: object
    activation: object
    success: object


@_event
class OpNodeCreated(Event):
    """create-op-node is making the operation node NAME ("5g"; published as
    the call begins, like NodeCreated): OP ("PLUS", "TIMES", ...), RESULT
    and the two OPERANDS (node names), at LEVEL.  For a decomposition the
    result is the target side and an operand the new derived target."""
    kind = "op-node-created"
    name: str
    op: str
    result: str
    operands: tuple
    level: object


@_event
class NodeChanged(Event):
    """The cyto-node NAME's FIELD (type, value, status, level, activation,
    success, listed, neighbors or plinks) is now VALUE.  Neighbors are
    ((node name, link type), ...), plinks pnode names; an empty list is
    None."""
    kind = "node-changed"
    name: str
    field: str
    value: object


@_event
class CurrentTargetChanged(Event):
    """*current-target* is now NAME (a node name, the symbol CYTO-TARGET's
    name, or None) with INTEREST."""
    kind = "current-target"
    name: object
    interest: object


@_event
class Disconnect(Event):
    """disconnect is killing the operation node OP and then NODE (published
    as the call begins; each one's NodeKilled follows)."""
    kind = "disconnect"
    node: str
    node_type: object
    node_value: object
    op: str
    op_type: object
    op_value: object


@_event
class NodeKilled(Event):
    """The cyto-node NAME has left the cytoplasm (killed by a disconnect, or
    a derived target replaced by a block).  CYTOPLASM: the names of the
    nodes left, in the cytoplasm's order ((cytoplasm :suppress-node)
    reverses it)."""
    kind = "node-killed"
    name: str
    cytoplasm: tuple = ()


@_event
class TargetReplaced(Event):
    """replace-target put the block BLOCK in TARGET's place.  With REBOUND
    ("Obvious.": the block has the target's value) the global CYTO-TARGET
    now holds BLOCK, and TARGET is that symbol's name; else BLOCK took the
    current derived target TARGET's neighbors and TARGET left the cytoplasm
    (its node-killed event came first)."""
    kind = "target-replaced"
    target: str
    block: str
    rebound: bool


@_event
class PnetActivations(Event):
    """After spread-activation-in-pnet: the activation of every pnode of
    *pnet*, in its order."""
    kind = "pnet"
    activations: tuple


@_event
class PnetInitialized(Event):
    """initialize-pnet reset every pnode of *pnet*: ACTIVATIONS in its
    order, and no instances."""
    kind = "pnet-initialized"
    activations: tuple


@_event
class PnodesChanged(Event):
    """The pnodes' FIELD ("activation", or "instances": ((type, cyto-node
    name), ...) or None) changed outside a spread: VALUES is ((pnode name,
    new value), ...).  A pnode's own message (set-activation,
    :add-activation, :subtract-activation, :update-instances,
    :suppress-instances) changes one, repump's set-up-activations six at
    once."""
    kind = "pnodes-changed"
    field: str
    values: tuple


@_event
class CoderackCreated(Event):
    """create-coderack made the empty coderack NAME (*coderack*'s value)
    with one bin per urgency in LEVELS, in bin order."""
    kind = "coderack-created"
    name: str
    levels: tuple


@_event
class RackEmptied(Event):
    """cr-empty-coderack emptied the coderack."""
    kind = "rack-emptied"


@dataclasses.dataclass(frozen=True)
class DecompositionStep:
    """One paragraph of decompose's output: operation OP applied to A
    (value VA) and B (value VB) to get RESULT."""
    op: str
    a: str
    va: object
    b: str
    vb: object
    result: str


@_event
class Decomposition(Event):
    """The top-level decompose printed STEPS (DecompositionSteps)."""
    kind = "decomposition"
    steps: tuple


@_event
class RunEnded(Event):
    """The run's last event, after ITERATIONS main-loop iterations."""
    iterations: int

    outcome: ClassVar[str]


@_event
class Solved(RunEnded):
    """The run printed "Done :" and DECOMPOSITION (the last decompose's)."""
    kind = "done"
    outcome = "solved"
    decomposition: tuple = ()


@_event
class GaveUp(RunEnded):
    kind = "gave-up"
    outcome = "gave-up"


@_event
class Capped(RunEnded):
    kind = "capped"
    outcome = "capped"


@_event
class RunError(RunEnded):
    """The run ended in a Lisp error with MESSAGE."""
    kind = "error"
    outcome = "error"
    message: str = ""

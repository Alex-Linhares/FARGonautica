"""Loop0003 item 1: the observer core (observe.py) and the engine's typed
events (events.py).

The run publishes typed events through a Subject; the oracle JSON-lines
trace (trace.py) is one observer of them.  These tests check:
  - the Subject: subscription order, unsubscribe, an observer that raises is
    reported (never swallowed silently) and changes nothing for the others;
  - the events are frozen dataclasses, one kind each;
  - the oracle trace writer is a pure observer: fed a run's recorded events,
    it writes the run's trace again, byte for byte, with or without the RNG
    draws (the events are a superset of both traces);
  - observers change nothing: a run with N extra observers attached (one of
    them raising) has the same outcome, printed text and trace bytes;
  - the events agree with the World: a shadow of the cytoplasm kept from the
    events alone equals the World's cytoplasm at every event.
The oracle equivalence itself (the trace against SBCL's) is
test_full_runs.py's and test_main_loop.py's.
"""

import dataclasses
import io

import pytest

from full_runs import PUZZLES
from numbo import events, harness, observe, trace
from numbo.cyto_def import CytoNode
from numbo.franz import intern
from numbo.pnet_def import Pnode

_CYTOPLASM = intern("*CYTOPLASM*")
_CURRENT_TARGET = intern("*CURRENT-TARGET*")


class Recorder:
    def __init__(self):
        self.events = []

    def on_event(self, event):
        self.events.append(event)


def run(problem, seed, cap=20000, observers=(), rng_events=False):
    out, buf = io.StringIO(), io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap, trace=buf,
                                rng_events=rng_events, out=out, observers=list(observers))
    return result, out.getvalue(), buf.getvalue()


# ---------------------------------------------------------------------------
# Subject

def test_publish_in_subscription_order_and_unsubscribe():
    seen = []

    class Tagged:
        def __init__(self, tag):
            self.tag = tag

        def on_event(self, event):
            seen.append((self.tag, event))

    subject = observe.Subject()
    a, b, c = Tagged("a"), Tagged("b"), Tagged("c")
    assert subject.subscribe(a) is a
    subject.subscribe(b)
    subject.subscribe(c)
    assert subject.observers == (a, b, c)
    e1 = observe.RackEmptied()
    subject.publish(e1)
    assert seen == [("a", e1), ("b", e1), ("c", e1)]
    subject.unsubscribe(b)
    seen.clear()
    e2 = observe.NodeKilled(name="CYTO-BRICK1")
    subject.publish(e2)
    assert seen == [("a", e2), ("c", e2)]
    with pytest.raises(ValueError):
        subject.unsubscribe(b)


def test_an_observer_may_unsubscribe_while_an_event_is_published():
    subject = observe.Subject()
    seen = []

    class Once:
        def on_event(self, event):
            seen.append("once")
            subject.unsubscribe(self)

    class Always:
        def on_event(self, event):
            seen.append("always")

    subject.subscribe(Once())
    subject.subscribe(Always())
    subject.publish(observe.RackEmptied())
    subject.publish(observe.RackEmptied())
    assert seen == ["once", "always", "always"]


def test_an_observer_that_raises_is_reported_and_the_others_still_see_the_event():
    reported = []
    subject = observe.Subject(on_error=lambda err: reported.append(err))
    rec = Recorder()

    class Boom:
        def on_event(self, event):
            raise RuntimeError("boom")

    boom = Boom()
    subject.subscribe(boom)
    subject.subscribe(rec)
    event = observe.RackEmptied()
    subject.publish(event)  # does not raise
    assert rec.events == [event]
    assert len(reported) == 1 and subject.errors == reported
    err = reported[0]
    assert err.observer is boom and err.event is event
    assert isinstance(err.exception, RuntimeError) and str(err.exception) == "boom"


def test_by_default_an_observer_error_goes_to_stderr(capsys):
    subject = observe.Subject()

    class Boom:
        def on_event(self, event):
            raise KeyError("missing")

    subject.subscribe(Boom())
    subject.publish(observe.NodeKilled(name="X"))
    err = capsys.readouterr().err
    assert "observer" in err and "node-killed" in err and "KeyError" in err
    assert len(subject.errors) == 1


def test_a_non_observer_is_refused():
    with pytest.raises(TypeError):
        observe.Subject().subscribe(object())


# ---------------------------------------------------------------------------
# The event kinds

def test_events_are_frozen_dataclasses_with_one_kind_each():
    kinds = {}
    for cls in observe.EVENT_TYPES.values():
        assert dataclasses.is_dataclass(cls) and issubclass(cls, observe.Event)
        assert cls.__dataclass_params__.frozen, cls
        assert cls.kind not in kinds, cls.kind
        kinds[cls.kind] = cls
    assert kinds == observe.EVENT_TYPES
    expected = {"start", "rng", "setup-choose", "iteration", "codelet-chosen", "post",
                "node-created", "op-node-created", "node-changed", "current-target",
                "disconnect", "node-killed", "target-replaced", "pnet", "rack-emptied",
                "decomposition", "done", "gave-up", "capped", "error",
                # loop0003 item 4: the Pnet's and the coderack's other changes
                "pnet-initialized", "pnodes-changed", "coderack-created"}
    assert set(kinds) == expected
    e = observe.NodeKilled(name="CYTO-BRICK1")
    with pytest.raises(dataclasses.FrozenInstanceError):
        e.name = "other"
    for kind in ("done", "gave-up", "capped", "error"):
        assert issubclass(kinds[kind], observe.RunEnded)


# ---------------------------------------------------------------------------
# The oracle trace is an observer of the events

@pytest.mark.parametrize("puzzle,seed", [(1, 1), (3, 8), (1, 40), (2, 1)])
def test_the_oracle_writer_rewrites_the_trace_from_the_events(puzzle, seed):
    rec = Recorder()
    _, _, with_rng = run(PUZZLES[puzzle - 1], seed, observers=[rec], rng_events=True)
    _, _, without_rng = run(PUZZLES[puzzle - 1], seed)
    for rng_events, expected in ((True, with_rng), (False, without_rng)):
        buf = io.StringIO()
        writer = trace.OracleTraceWriter(buf, rng_events=rng_events)
        for event in rec.events:
            writer.on_event(event)
        assert buf.getvalue() == expected
    assert isinstance(rec.events[0], observe.RunStarted)
    assert isinstance(rec.events[-1], observe.RunEnded)


def test_the_trace_is_written_without_the_engine_hooks_knowing_of_it():
    """trace.py has no hooks of its own any more: events.py has them all."""
    for name in ("install", "HOOKS", "current", "Trace", "tracing"):
        assert not hasattr(trace, name), name
    assert hasattr(events, "install") and hasattr(events, "HOOKS")


# ---------------------------------------------------------------------------
# Observers change nothing

class Boom:
    """Raises on the first 3 events it sees (then is quiet)."""

    def __init__(self):
        self.n = 0

    def on_event(self, event):
        self.n += 1
        if self.n <= 3:
            raise RuntimeError("observer failure")


class WorldReader:
    """Reads the World (no changes) at every event."""

    def __init__(self):
        self.reads = 0

    def on_event(self, event):
        world = events.current.world
        nodes = world[_CYTOPLASM].nodes if _CYTOPLASM in world else None
        self.reads += len(nodes or ())


@pytest.mark.parametrize("puzzle,seed,cap", [(1, 1, 20000), (3, 8, 20000), (1, 40, 20000),
                                             (1, 323, 20000), (4, 2, 20000), (2, 1, 30)])
def test_extra_observers_change_nothing(puzzle, seed, cap, capsys):
    plain = run(PUZZLES[puzzle - 1], seed, cap, rng_events=True)
    observers = [Recorder(), Boom(), WorldReader(), Recorder()]
    watched = run(PUZZLES[puzzle - 1], seed, cap, observers=observers, rng_events=True)
    assert watched == plain
    assert "observer failure" in capsys.readouterr().err
    assert observers[0].events == observers[3].events
    assert len(observers[0].events) > len(plain[2].splitlines())


def test_a_run_with_no_trace_publishes_to_its_observers():
    rec = Recorder()
    out = io.StringIO()
    result = harness.run_config(list(PUZZLES[0]), seed=1, out=out, observers=[rec])
    assert result["outcome"] == "solved"
    assert isinstance(rec.events[-1], observe.Solved)
    assert rec.events[-1].iterations == result["iterations"]
    assert events.current is None


# ---------------------------------------------------------------------------
# The events agree with the World

def _plain(x):
    """A cyto-node ivar's value as the events carry it."""
    if isinstance(x, (CytoNode, Pnode)):
        return x.name.name
    if isinstance(x, list):
        return tuple(_plain(v) for v in x) or None
    return events.plain(x)


FIELDS = ("type", "value", "status", "level", "activation", "success", "listed",
          "neighbors", "plinks")


class ShadowChecker:
    """Keeps the cytoplasm from the events alone, and checks it against the
    World at every event.  A node is found by its name once (at the event
    after its node-created) and then followed as an object: replace-target
    can rebind the global its name holds (TargetReplaced)."""

    def __init__(self):
        self.nodes = {}          # name -> {field: value}
        self.objects = {}        # name -> the World's CytoNode
        self.in_cytoplasm = []   # names, newest first (the cytoplasm's order)
        self.created = None      # the node of a node-created being published
        self.current_target = None
        self.checked = 0
        self.kinds = set()
        self.changed_fields = set()
        self.replaced = []

    def on_event(self, event):
        self.kinds.add(event.kind)
        world = events.current.world
        if self.created is not None:
            node = world[intern(self.created)]
            assert isinstance(node, CytoNode)
            assert all(node is not o for o in self.objects.values())
            self.objects[self.created] = node
            self.check_node(self.created)
            self.created = None
        if isinstance(event, (observe.NodeCreated, observe.OpNodeCreated)):
            assert event.name not in self.nodes
            if isinstance(event, observe.NodeCreated):
                fields = dict(type=event.type, value=event.value, status=event.status,
                              level=event.level, activation=event.activation,
                              success=event.success, listed=None, neighbors=None, plinks=None)
            else:
                assert event.op in ("PLUS", "TIMES") and event.name.startswith(event.op)
                assert len(event.operands) == 2
                fields = dict(type="5g", value=None, status=None, level=event.level,
                              activation=None, success=None, listed=None, plinks=None,
                              neighbors=((event.result, "RESULT"),)
                              + tuple((o, "OPERAND") for o in event.operands))
            self.nodes[event.name] = fields
            self.in_cytoplasm.insert(0, event.name)
            self.created = event.name
            return
        if isinstance(event, observe.NodeChanged):
            assert event.field in FIELDS
            self.nodes[event.name][event.field] = event.value
            self.changed_fields.add(event.field)
        elif isinstance(event, observe.NodeKilled):
            self.in_cytoplasm.remove(event.name)
            # (cytoplasm :suppress-node) leaves the others reversed.
            assert sorted(event.cytoplasm) == sorted(self.in_cytoplasm)
            self.in_cytoplasm = list(event.cytoplasm)
        elif isinstance(event, observe.CurrentTargetChanged):
            self.current_target = (event.name, event.interest)
        elif isinstance(event, observe.Disconnect):
            assert event.node in self.in_cytoplasm and event.op in self.in_cytoplasm
        elif isinstance(event, observe.TargetReplaced):
            self.replaced.append(event)
            if event.rebound:
                assert world[intern(event.target)] is self.objects[event.block]
            else:
                assert event.target not in self.in_cytoplasm
        self.check_all(world, event)

    def check_node(self, name):
        node = self.objects[name]
        got = {f: _plain(getattr(node, f)) for f in FIELDS}
        assert got == self.nodes[name], name

    def check_all(self, world, event):
        if _CYTOPLASM not in world:
            return
        cytoplasm = world[_CYTOPLASM]
        assert [n.name.name for n in cytoplasm.nodes or ()] == self.in_cytoplasm, event
        assert all(n is self.objects[n.name.name] for n in cytoplasm.nodes or ())
        for name in self.in_cytoplasm:
            self.check_node(name)
        if (isinstance(event, observe.RunEnded)
                or isinstance(event, observe.IterationBegan) and event.n % 50 == 0):
            # Now and then, every node ever made (killed ones too).
            for name in self.objects:
                self.check_node(name)
        if self.current_target is not None and _CURRENT_TARGET in world:
            ct = world[_CURRENT_TARGET]
            assert (_plain(ct.name), ct.interest) == self.current_target
        self.checked += 1


def strict(*observers):
    """A Subject passing the events on to OBSERVERS, and the list of their
    failures (which the run's own Subject would only report)."""
    errors = []
    subject = observe.Subject(on_error=errors.append)
    for o in observers:
        subject.subscribe(o)
    return subject, errors


def checked_run(problem, seed):
    checker, rec = ShadowChecker(), Recorder()
    subject, errors = strict(checker, rec)
    result, _, _ = run(problem, seed, observers=[subject])
    assert not errors, [(e.event, e.exception) for e in errors[:3]]
    return result, checker, rec.events


@pytest.mark.parametrize("puzzle", range(1, 12))
def test_the_events_agree_with_the_world(puzzle):
    result, checker, evs = checked_run(PUZZLES[puzzle - 1], 1)
    assert checker.checked > 50
    assert {"node-created", "op-node-created", "node-changed", "current-target",
            "iteration", "codelet-chosen", "post", "pnet"} <= checker.kinds
    assert {"status", "neighbors", "activation", "plinks"} <= checker.changed_fields
    iterations = [e for e in evs if isinstance(e, observe.IterationBegan)]
    assert [e.n for e in iterations] == list(range(result["iterations"]))
    assert evs[-1].outcome == result["outcome"]


def test_the_checker_fails_on_a_missed_change():
    """The shadow check catches a change made behind the events' back."""

    class Tamper:
        def on_event(self, event):
            if isinstance(event, observe.IterationBegan) and event.n == 5:
                events.current.world[_CYTOPLASM].nodes[0].level = -7

    subject, errors = strict(ShadowChecker())
    harness.run_config(list(PUZZLES[0]), seed=1, out=io.StringIO(),
                       observers=[Tamper(), subject])
    assert errors and isinstance(errors[0].exception, AssertionError)


def test_the_kill_block_gap_run_agrees_with_the_world():
    """Puzzle 3 seed 8: kills, and kill-block's orphaned operation node."""
    _, checker, evs = checked_run(PUZZLES[2], 8)
    assert {"disconnect", "node-killed", "done"} <= checker.kinds
    statuses = {e.value for e in evs if isinstance(e, observe.NodeChanged) and e.field == "status"}
    assert {"linked", "free", "killed"} <= statuses
    # Every disconnect kills its operation node and then its node.
    for i, e in enumerate(evs):
        if isinstance(e, observe.Disconnect):
            killed = [x.name for x in evs[i:] if isinstance(x, observe.NodeKilled)][:2]
            assert killed == [e.op, e.node]


def test_replace_target_is_an_event():
    """Puzzle 4 seed 1 ends "Obvious.": CYTO-TARGET is rebound to a block."""
    _, checker, evs = checked_run(PUZZLES[3], 1)
    assert [e.rebound for e in checker.replaced][-1:] == [True]
    assert evs[-1].outcome == "solved"


def test_the_iteration_events_carry_the_codelet_chosen():
    rec = Recorder()
    run(PUZZLES[0], 1, observers=[rec], rng_events=True)
    evs = rec.events
    chosen = 0
    for i, e in enumerate(evs):
        if isinstance(e, observe.IterationBegan):
            assert isinstance(e.temperature, (int, float)) and e.rack
        if isinstance(e, observe.CodeletChosen):
            # config's cr-choose comes right after the iteration begins (and
            # only when x is not a multiple of 5).
            prev = evs[i - 1]
            assert isinstance(prev, observe.IterationBegan) and prev.n == e.n
            assert prev.x % 5 != 0
            assert e.codelet is not None and e.draws
            chosen += 1
    assert chosen > 20
    setup = [e for e in evs if isinstance(e, observe.SetupChoose)]
    assert len(setup) == events.SETUP_CHOOSES
    done = evs[-1]
    assert isinstance(done, observe.Solved) and done.outcome == "solved"
    decomposition = [e for e in evs if isinstance(e, observe.Decomposition)]
    assert len(decomposition) == 1 and decomposition[0].steps == done.decomposition
    assert all(isinstance(s, observe.DecompositionStep) for s in done.decomposition)
    assert isinstance(evs[0], observe.RunStarted) and evs[0].seed == 1 and len(evs[0].pnet) == 88

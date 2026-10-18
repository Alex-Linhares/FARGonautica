"""Loop0006 item 01: the run controller (`tabletop/gui/models/run_controller.py`), the
engine on a worker thread behind a per-event gate, after the Numbo GUI's
`tests/test_run_controller.py` through the Seek-Whence GUI's. Threading tests, no Qt
(G-008, G-009).

- A run through the gate, with the tap, gives the same event records, draws and report as
  a plain run, on the four golden cases, and so does `headless.run(window=False)`, the
  determinism contract, which imports no Qt.
- No event is published before the previous one is acknowledged; the worker blocks until
  each acknowledgment.
- The modes: paused and step (one event); step codelet (to the end of the current
  `World.step()`, exactly at a step boundary; from the start, to the end of the start);
  play with a delay; run to end.
- Stop, from any wait, leaves no live thread, and a run after a stop is unchanged; one run
  at a time; generations; a raising observer or callback is reported and changes nothing;
  `start_events` replays recorded events; bad inputs raise before the worker starts.
"""

import ast
import queue
import random
import threading
import time
from pathlib import Path

import pytest

from tabletop.config import PROFILES
from tabletop.events import Collector
from tabletop.gui import headless
from tabletop.gui.models import run_controller
from tabletop.gui.models.event_log import EventLog
from tabletop.gui.models.run_controller import (Delivery, EngineBusy, RunController,
                                                RunOutcome)
from tabletop.gui.models.tap import Tap
from tabletop.problems import load_problem
from tabletop.world import World

from golden import golden_cases

STANDARD = PROFILES["standard"]
FIRST = golden_cases()[0]                      # surround-05 seed 1: 62 codelets, 901 events since item 09 (89 and 1204 at item 08; 55 and 647 at item 07; 39 and 549 at item 05)

# How long "nothing happens" is watched for, and the bound on prompt ends.
QUIET = 0.2
PROMPT = 10.0


class Recorder:
    def __init__(self):
        self.events = []

    def on_event(self, event):
        self.events.append(event)


def wait_until(predicate, timeout=PROMPT):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return True
        time.sleep(0.002)
    return predicate()


def step_boundaries(problem, seed, config=STANDARD):
    """The index of the last event of the start and of each `World.step()`, from a plain
    run (the controller's step codelet must pause exactly there)."""
    log = Collector()
    world = World(load_problem(problem), config, seed, observers=[log])
    bounds = [len(log.events) - 1]
    while not world.finished:
        world.step()
        bounds.append(len(log.events) - 1)
    return bounds


@pytest.fixture(autouse=True)
def stop_every_controller(monkeypatch):
    """A failed test must not leave a worker holding the engine (Numbo GUI)."""
    made = []
    init = RunController.__init__

    def tracking_init(self, *args, **kw):
        init(self, *args, **kw)
        made.append(self)

    monkeypatch.setattr(RunController, "__init__", tracking_init)
    yield
    for c in made:
        c.stop()
        assert not c.is_alive()


class Consumer:
    """A deliver callback with a consumer thread that acknowledges each delivery after a
    short random pause (a redraw), checking that no delivery comes while another is
    unacknowledged (Numbo GUI)."""

    def __init__(self, controller_box, seed=0, max_pause=0.0003, auto=True):
        self.box = controller_box
        self.queue = queue.Queue()
        self.delivered = []
        self.outstanding = None
        self.violations = []
        self.lock = threading.Lock()
        self.rng = random.Random(seed)
        self.max_pause = max_pause
        self.thread = None
        if auto:
            self.thread = threading.Thread(target=self._loop, daemon=True)
            self.thread.start()

    def __call__(self, delivery):
        with self.lock:
            if self.outstanding is not None:
                self.violations.append((self.outstanding.index, delivery.index))
            self.outstanding = delivery
            self.delivered.append(delivery)
        self.queue.put(delivery)

    def ack(self, delivery):
        with self.lock:
            if self.outstanding is delivery:
                self.outstanding = None
        return self.box[0].acknowledge(delivery.ticket)

    def ack_next(self, timeout=PROMPT):
        return self.ack(self.queue.get(timeout=timeout))

    def _loop(self):
        while True:
            delivery = self.queue.get()
            if delivery is None:
                return
            if self.max_pause:
                time.sleep(self.rng.uniform(0, self.max_pause))
            self.ack(delivery)

    def close(self):
        if self.thread is not None:
            self.queue.put(None)
            self.thread.join(PROMPT)


def make(deliver_auto=None, tap=False, max_pause=0.0003, **kw):
    """A controller with a recorder (and a tap feeding a log), and optionally a deliver
    Consumer: (controller, recorder, consumer, log)."""
    box = [None]
    rec = Recorder()
    consumer = None
    log = EventLog()
    observers = [rec]
    if tap:
        the_tap = Tap(log)
        observers.append(the_tap)
        kw["on_world"] = the_tap.attach
        kw["on_step"] = the_tap.after_step
    if deliver_auto is not None:
        consumer = Consumer(box, auto=deliver_auto, max_pause=max_pause)
        kw["deliver"] = consumer
    c = RunController(observers=observers, **kw)
    box[0] = c
    return c, rec, consumer, log


def no_worker_alive() -> bool:
    return not any(t.name.startswith("tabletop-") and t.is_alive()
                   for t in threading.enumerate())


# ---------------------------------------------------------------------------
# Event order and results

@pytest.mark.parametrize("problem,seed", golden_cases())
def test_run_to_end_gives_the_exact_events_and_results(golden_runs, problem, seed,
                                                       thread_errors):
    ref = golden_runs[(problem, seed)]
    c, rec, consumer, log = make(deliver_auto=True, tap=True)
    try:
        c.start_run(problem, STANDARD, seed)
        c.run_to_end()
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    o = c.outcome
    assert isinstance(o, RunOutcome)
    assert o.source == "run" and not o.stopped and o.error is None
    assert o.result == ref.result
    assert o.result.draws == ref.draws
    assert tuple(rec.events) == ref.events
    assert tuple(log.records) == ref.records
    assert log.snapshots and log.last_snapshot.finished
    assert o.events == len(ref.events) == c.events_published
    assert c.state == "finished" and not c.is_alive()
    assert consumer.violations == []
    assert [d.event for d in consumer.delivered] == list(ref.events)
    assert all(d.generation == o.generation == c.generation for d in consumer.delivered)
    assert c.errors == []


@pytest.mark.parametrize("problem,seed", golden_cases())
def test_headless_run_equals_a_plain_run(golden_runs, problem, seed, thread_errors):
    ref = golden_runs[(problem, seed)]
    got = headless.run(problem, STANDARD, seed, window=False)
    assert sorted(got) == ["draws", "records", "report"]
    assert tuple(got["records"]) == ref.records
    assert got["draws"] == ref.draws and got["report"] == ref.report
    assert no_worker_alive()


def test_headless_accepts_a_problem_object_and_imports_no_qt(golden_runs):
    ref = golden_runs[FIRST]
    got = headless.run(load_problem(FIRST[0]), STANDARD, FIRST[1])
    assert got["draws"] == ref.draws and got["report"] == ref.report
    here = Path(headless.__file__)
    for path in [here] + sorted((here.parent / "models").glob("*.py")):
        for node in ast.parse(path.read_text()).body:
            names = ([a.name for a in node.names] if isinstance(node, ast.Import)
                     else [node.module or ""] if isinstance(node, ast.ImportFrom) else [])
            assert not any(n.split(".")[0] in ("PySide6", "shiboken6") for n in names), path


def test_no_event_is_published_before_the_previous_is_acknowledged(golden_runs,
                                                                   thread_errors):
    ref = golden_runs[FIRST]
    box = [None]
    consumer = Consumer(box, seed=7, max_pause=0.0004)
    seen_by_observer = []

    class Observer:
        # Runs on the worker before the delivery: the consumer must have acknowledged
        # everything delivered so far.
        def on_event(self, event):
            seen_by_observer.append((event, consumer.outstanding))

    c = RunController(observers=[Observer()], deliver=consumer)
    box[0] = c
    try:
        c.start_run(FIRST[0], STANDARD, FIRST[1])
        assert c.wait(PROMPT * 10)
    finally:
        consumer.close()
    assert consumer.violations == []
    assert all(outstanding is None for _, outstanding in seen_by_observer)
    assert tuple(e for e, _ in seen_by_observer) == ref.events
    assert [d.event for d in consumer.delivered] == list(ref.events)
    assert [d.index for d in consumer.delivered] == list(range(len(ref.events)))
    tickets = [d.ticket for d in consumer.delivered]
    assert len(set(tickets)) == len(tickets)
    assert c.outcome.result.draws == ref.draws


def test_the_worker_blocks_until_each_acknowledgment(golden_runs, thread_errors):
    ref = golden_runs[FIRST]
    c, rec, consumer, _ = make(deliver_auto=False)
    c.start_run(FIRST[0], STANDARD, FIRST[1])
    try:
        assert wait_until(lambda: len(consumer.delivered) == 1)
        time.sleep(QUIET)
        assert len(consumer.delivered) == 1 and len(rec.events) == 1
        assert consumer.delivered[0].event.name == "RunStarted"
        # A wrong or stale ticket acknowledges nothing.
        assert c.acknowledge(consumer.delivered[0].ticket + 1000) is False
        time.sleep(QUIET / 2)
        assert len(consumer.delivered) == 1
        for i in range(1, 5):
            assert consumer.ack_next() is True
            assert wait_until(lambda: len(consumer.delivered) == i + 1)
            time.sleep(0.02)
            assert len(consumer.delivered) == i + 1
        # An acknowledgment counts once.
        assert c.acknowledge(consumer.delivered[0].ticket) is False
        assert [d.event for d in consumer.delivered] == list(ref.events[:5])
    finally:
        c.stop()
        consumer.close()
    assert c.outcome.stopped and not c.is_alive()


# ---------------------------------------------------------------------------
# Pause and step

def test_start_paused_then_step_events(golden_runs, thread_errors):
    ref = golden_runs[FIRST]
    c, rec, _, _ = make()
    c.start_run(FIRST[0], STANDARD, FIRST[1], paused=True)
    try:
        assert wait_until(lambda: c.state == "paused")
        time.sleep(QUIET)
        assert rec.events == []
        for n in range(1, 4):
            c.step()
            assert wait_until(lambda: len(rec.events) == n and c.state == "paused")
            time.sleep(0.05)
            assert len(rec.events) == n
        # Steps requested together are all taken.
        c.step()
        c.step()
        assert wait_until(lambda: len(rec.events) == 5 and c.state == "paused")
        time.sleep(0.05)
        assert tuple(rec.events) == ref.events[:5]
    finally:
        c.stop()
    assert c.outcome.stopped and c.outcome.events == 5 and c.outcome.result is None


def test_step_codelet_pauses_exactly_at_a_step_boundary(golden_runs, thread_errors):
    ref = golden_runs[FIRST]
    bounds = step_boundaries(*FIRST)
    assert len(bounds) == ref.result.codelets + 1
    c, rec, _, log = make(tap=True)
    c.start_run(FIRST[0], STANDARD, FIRST[1], paused=True)
    try:
        assert wait_until(lambda: c.state == "paused")
        # From the start: to the end of the start, before the first codelet.
        for k in range(4):
            c.step_codelet()
            want = bounds[k] + 1
            assert wait_until(lambda: len(rec.events) == want and c.state == "paused"), \
                (k, len(rec.events), want)
            time.sleep(0.03)
            assert len(rec.events) == want
            assert ref.events[want].name in ("CodeletChosen", "CodeletRemoved")
            assert log.snapshots.indexes[-1] == bounds[k], "the step's snapshot is taken"
            assert log.state_at(bounds[k]).clock == k
        # A step in the middle of a codelet, then the rest of it.
        c.step()
        assert wait_until(lambda: len(rec.events) == bounds[3] + 2 and c.state == "paused")
        c.step_codelet()
        assert wait_until(lambda: len(rec.events) == bounds[4] + 1 and c.state == "paused")
        time.sleep(0.03)
        assert tuple(rec.events) == ref.events[:bounds[4] + 1]
        assert log.snapshots.indexes == bounds[:5]
    finally:
        c.stop()
    assert c.outcome.stopped


def test_step_codelet_on_the_last_codelet_ends_the_run(golden_runs, thread_errors):
    ref = golden_runs[FIRST]
    bounds = step_boundaries(*FIRST)
    c, rec, _, _ = make()
    c.start_run(FIRST[0], STANDARD, FIRST[1], paused=True)
    for _ in range(bounds[-2] + 1):
        c.step()
    assert wait_until(lambda: len(rec.events) == bounds[-2] + 1 and c.state == "paused")
    c.step_codelet()
    assert c.wait(PROMPT)
    assert tuple(rec.events) == ref.events and not c.outcome.stopped
    assert c.state == "finished" and c.mode == "paused"


def test_pause_and_play(golden_runs, thread_errors):
    ref = golden_runs[FIRST]
    c, rec, consumer, _ = make(deliver_auto=False)
    c.start_run(FIRST[0], STANDARD, FIRST[1])
    try:
        for _ in range(10):
            consumer.ack_next()
        assert wait_until(lambda: len(consumer.delivered) == 11)
        c.pause()
        # The delivery in hand still gets its acknowledgment; then nothing.
        consumer.ack_next()
        assert wait_until(lambda: c.state == "paused")
        n = len(rec.events)
        time.sleep(QUIET)
        assert len(rec.events) == n == len(consumer.delivered)
        assert consumer.queue.empty()
        c.play()
        for _ in range(5):
            consumer.ack_next()
        assert wait_until(lambda: len(rec.events) == n + 5)
        assert tuple(rec.events) == ref.events[:n + 5]
    finally:
        c.stop()
        consumer.close()


def test_mode_reports(thread_errors):
    c, rec, _, _ = make()
    assert c.state == "idle" and c.mode == "paused" and c.generation == 0
    c.start_run(FIRST[0], STANDARD, FIRST[1], paused=True)
    assert wait_until(lambda: c.state == "paused")
    assert c.generation >= 1
    c.step()
    assert c.mode in ("step", "paused")
    c.step_codelet()
    assert c.mode in ("step-codelet", "paused")
    c.play()
    assert c.mode == "playing"
    c.run_to_end()
    assert c.mode == "run-to-end"
    assert c.wait(PROMPT)
    assert c.state == "finished"
    assert set(run_controller.MODES) == {"paused", "playing", "step", "step-codelet",
                                         "run-to-end"}
    assert set(run_controller.STATES) == {"idle", "running", "paused", "finished", "stopped"}
    with pytest.raises(ValueError):
        c.set_delay(-1)


# ---------------------------------------------------------------------------
# Speed

def test_the_delay_paces_play_but_not_run_to_end(thread_errors):
    c, rec, _, _ = make(delay=0.04)
    c.start_run(FIRST[0], STANDARD, FIRST[1])
    time.sleep(0.4)
    n = len(rec.events)
    # About 10 events in 0.4 s (each is followed by a 0.04 s delay).
    assert 2 <= n <= 12, n
    c.set_delay(0.0)
    assert c.delay == 0.0
    assert c.wait(PROMPT)
    assert not c.outcome.stopped

    c, rec, _, _ = make(delay=10.0)
    t = time.monotonic()
    c.start_run(FIRST[0], STANDARD, FIRST[1])
    assert wait_until(lambda: len(rec.events) == 1)
    c.run_to_end()                  # cuts the delay short
    assert c.wait(PROMPT)
    assert time.monotonic() - t < 5.0
    assert len(rec.events) == 2690           # v5's fix, D-116 to D-120 (3174 in v4, D-115; 1355 in v3, D-114; 901 at v2 since item 09; 1204, 647 before)


# ---------------------------------------------------------------------------
# Stop, one at a time, errors, replay, inputs

def test_stop_from_every_wait_leaves_no_live_thread_and_the_next_run_is_unchanged(
        golden_runs, thread_errors):
    ref = golden_runs[FIRST]
    # Stopped while paused.
    c, rec, _, _ = make()
    c.start_run(FIRST[0], STANDARD, FIRST[1], paused=True)
    assert wait_until(lambda: c.state == "paused")
    assert c.stop() and not c.is_alive() and c.outcome.stopped and c.state == "stopped"
    first_generation = c.outcome.generation
    # Stopped while waiting for an acknowledgment.
    c, rec, consumer, _ = make(deliver_auto=False)
    c.start_run(FIRST[0], STANDARD, FIRST[1])
    consumer.ack_next()
    consumer.ack_next()
    assert wait_until(lambda: len(consumer.delivered) == 3)
    assert c.stop() and not c.is_alive() and c.outcome.stopped
    assert c.outcome.events == 3 and c.outcome.generation > first_generation
    # Stopped during a delay.
    c, rec, _, _ = make(delay=10.0)
    c.start_run(FIRST[0], STANDARD, FIRST[1])
    assert wait_until(lambda: len(rec.events) == 1)
    assert c.stop() and c.outcome.stopped
    # A late acknowledgment of the stopped run is refused, and the next run is unchanged.
    assert c.acknowledge(consumer.delivered[-1].ticket) is False
    c, rec, consumer, log = make(deliver_auto=True, tap=True)
    try:
        c.start_run(FIRST[0], STANDARD, FIRST[1])
        c.run_to_end()
        assert c.wait(PROMPT)
    finally:
        consumer.close()
    assert tuple(log.records) == ref.records and c.outcome.result == ref.result
    assert no_worker_alive()


def test_one_run_at_a_time(thread_errors):
    a, _, _, _ = make()
    b, _, _, _ = make()
    a.start_run(FIRST[0], STANDARD, FIRST[1], paused=True)
    assert wait_until(lambda: a.state == "paused")
    with pytest.raises(EngineBusy):
        b.start_run(FIRST[0], STANDARD, FIRST[1])
    assert b.state == "idle" and not b.is_alive()
    # A new run on the same controller stops and joins its old one first.
    a.start_run(FIRST[0], STANDARD, 2, paused=True)
    assert wait_until(lambda: a.state == "paused")
    assert a.stop()
    # Then the other controller may run.
    b.start_run(FIRST[0], STANDARD, FIRST[1])
    b.run_to_end()
    assert b.wait(PROMPT) and b.state == "finished"


def test_a_raising_observer_or_callback_is_reported_and_changes_nothing(golden_runs):
    ref = golden_runs[FIRST]
    reported = []

    class Bad:
        def on_event(self, event):
            if event.name == "CodeletChosen" and event.clock == 2:
                raise RuntimeError("an observer's bug")

    def bad_step(world):
        if world.clock == 3:
            raise RuntimeError("a callback's bug")

    c, rec, consumer, _ = make(deliver_auto=True, on_error=reported.append, on_step=bad_step)
    c.subscribe(Bad())
    try:
        c.start_run(FIRST[0], STANDARD, FIRST[1])
        c.run_to_end()
        assert c.wait(PROMPT)
    finally:
        consumer.close()
    assert tuple(rec.events) == ref.events and c.outcome.result == ref.result
    assert len(reported) == 2 and reported == c.errors
    assert isinstance(reported[0].observer, Bad) and reported[0].event.name == "CodeletChosen"
    assert reported[1].observer is bad_step and reported[1].event is None
    assert str(reported[1].exception) == "a callback's bug"
    with pytest.raises(TypeError):
        c.subscribe(object())


def test_start_events_replays_recorded_events(golden_runs, thread_errors):
    ref = golden_runs[FIRST]
    c, rec, consumer, _ = make(deliver_auto=True)
    try:
        c.start_events(list(ref.events), start=100)
        c.run_to_end()
        assert c.wait(PROMPT)
    finally:
        consumer.close()
    assert tuple(rec.events) == ref.events[100:]
    assert [d.index for d in consumer.delivered] == list(range(100, len(ref.events)))
    o = c.outcome
    assert o.source == "events" and o.result is None and o.events == len(ref.events)
    with pytest.raises(ValueError):
        c.start_events(list(ref.events), start=len(ref.events) + 1)


def test_bad_inputs_raise_before_the_worker_starts():
    c, _, _, _ = make()
    with pytest.raises(FileNotFoundError):
        c.start_run("no-such-problem", STANDARD, 1)
    with pytest.raises(TypeError):
        c.start_run(FIRST[0], {"profile": "standard"}, 1)
    with pytest.raises(TypeError):
        c.start_run(FIRST[0], STANDARD, "1")
    with pytest.raises(TypeError):
        c.start_run(42, STANDARD, 1)
    assert c.state == "idle" and not c.is_alive()
    assert Delivery(1, 0, None).generation == 0

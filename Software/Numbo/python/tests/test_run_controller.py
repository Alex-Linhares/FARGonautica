"""The run controller (numbo/models/run_controller.py): Numbo on a worker
thread behind a per-event gate.  Threading tests, no Qt.

Every check that the controller changes nothing compares with a plain
harness.run_config run made in this thread, before or after the worker
(the engine's run state is per process, so two runs never overlap)."""

import io
import queue
import random
import sys
import threading
import time

import pytest

from numbo import events, harness, observe
from numbo.models import run_controller
from numbo.models.run_controller import Delivery, RunController, RunOutcome
from numbo.session import SessionRecorder

P1 = [114, 11, 20, 7, 1, 6]
P3 = [31, 3, 5, 24, 3, 14]

# How long "nothing happens" is watched for, and the bound on prompt ends.
QUIET = 0.25
PROMPT = 2.0


class Recorder:
    def __init__(self):
        self.events = []

    def on_event(self, event):
        self.events.append(event)


def reference(problem, seed, cap=500, rng_events=False):
    """A plain run: its events, result, printed text and oracle trace."""
    rec, out, trace = Recorder(), io.StringIO(), io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap, out=out,
                                trace=trace, rng_events=rng_events, observers=[rec])
    return rec.events, result, out.getvalue(), trace.getvalue()


def plain(problem, seed, cap=500):
    """An unobserved run (no trace, no observers): result and printed text."""
    out = io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap, out=out)
    return result, out.getvalue()


@pytest.fixture(scope="module")
def ref_p1():
    return reference(P1, 1)


def wait_until(predicate, timeout=PROMPT):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if predicate():
            return True
        time.sleep(0.002)
    return predicate()


@pytest.fixture
def no_thread_errors(monkeypatch):
    """Exceptions escaping any thread, collected (none are expected)."""
    errors = []
    monkeypatch.setattr(threading, "excepthook", lambda args: errors.append(args))
    yield errors
    assert errors == []


@pytest.fixture(autouse=True)
def stop_every_controller(monkeypatch):
    """A failed test must not leave a worker holding the engine."""
    made = []
    init = RunController.__init__

    def tracking_init(self, *args, **kw):
        init(self, *args, **kw)
        made.append(self)

    monkeypatch.setattr(RunController, "__init__", tracking_init)
    yield
    for c in made:
        c.stop()


def engine_is_idle():
    return events.current is None and harness._iteration_cap is None


class Consumer:
    """A deliver callback with a consumer thread that acknowledges each
    delivery after a short random pause (a redraw), checking that no
    delivery comes while another is unacknowledged."""

    def __init__(self, controller_box, seed=0, max_pause=0.0005, auto=True):
        self.box = controller_box
        self.queue = queue.Queue()
        self.delivered = []
        self.outstanding = None
        self.violations = []
        self.lock = threading.Lock()
        self.rng = random.Random(seed)
        self.max_pause = max_pause
        self.auto = auto
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


def make(deliver_auto=None, **kw):
    """A controller with a recorder, and optionally a deliver Consumer."""
    box = [None]
    rec = Recorder()
    consumer = None
    if deliver_auto is not None:
        consumer = Consumer(box, auto=deliver_auto)
        kw["deliver"] = consumer
    c = RunController(observers=[rec], **kw)
    box[0] = c
    return c, rec, consumer


# ---------------------------------------------------------------------------
# Event order and results

def test_run_to_end_gives_the_exact_events_and_results(ref_p1, no_thread_errors):
    events_ref, result_ref, out_ref, trace_ref = ref_p1
    c, rec, _ = make()
    trace = io.StringIO()
    c.start_run(P1, seed=1, trace=trace)
    assert c.wait(PROMPT * 5)
    o = c.outcome
    assert isinstance(o, RunOutcome)
    assert o.source == "run" and not o.stopped and o.error is None
    assert o.result == result_ref
    assert o.result["outcome"] == "solved" and o.result["iterations"] == 45
    assert o.output == out_ref
    assert trace.getvalue() == trace_ref
    assert rec.events == events_ref
    assert o.events == len(events_ref) == c.events_published
    assert c.state == "finished"
    assert engine_is_idle()


@pytest.mark.parametrize("problem,seed,cap,outcome", [
    (P1, 40, 500, "error"),         # the reactivate-cyto race
    (P3, 8, 500, "solved"),         # the kill-block gap
    (P3, 1, 300, "capped"),
    ([6, 1, 1, 1, 1, 1], 1, 500, None),
])
def test_results_equal_an_unobserved_run(problem, seed, cap, outcome, no_thread_errors):
    before = plain(problem, seed, cap)
    c, rec, consumer = make(deliver_auto=True)
    try:
        c.start_run(problem, seed=seed, max_iterations=cap)
        assert c.wait(PROMPT * 10)
    finally:
        consumer.close()
    o = c.outcome
    if outcome is not None:
        assert o.result["outcome"] == outcome
    # The error outcome's iteration count is 0 without a trace in both.
    assert (o.result, o.output) == before
    assert (o.result, o.output) == plain(problem, seed, cap)
    assert consumer.violations == []
    assert [d.event for d in consumer.delivered] == rec.events
    assert isinstance(rec.events[-1], observe.RunEnded)


def test_no_event_is_published_before_the_previous_is_acknowledged(ref_p1, no_thread_errors):
    events_ref = ref_p1[0]
    box = [None]
    consumer = Consumer(box, seed=7, max_pause=0.0004)
    seen_by_observer = []

    class Observer:
        # Runs on the worker before the delivery: the consumer must have
        # acknowledged everything delivered so far.
        def on_event(self, event):
            seen_by_observer.append((event, consumer.outstanding))

    c = RunController(observers=[Observer()], deliver=consumer)
    box[0] = c
    try:
        c.start_run(P1, seed=1)
        assert c.wait(PROMPT * 10)
    finally:
        consumer.close()
    assert consumer.violations == []
    assert all(outstanding is None for _, outstanding in seen_by_observer)
    assert [e for e, _ in seen_by_observer] == events_ref
    assert [d.event for d in consumer.delivered] == events_ref
    assert [d.index for d in consumer.delivered] == list(range(len(events_ref)))
    tickets = [d.ticket for d in consumer.delivered]
    assert len(set(tickets)) == len(tickets)
    assert c.outcome.result == ref_p1[1]


def test_the_worker_blocks_until_each_acknowledgment(ref_p1, no_thread_errors):
    events_ref = ref_p1[0]
    c, rec, consumer = make(deliver_auto=False)
    c.start_run(P1, seed=1)
    try:
        assert wait_until(lambda: len(consumer.delivered) == 1)
        time.sleep(QUIET)
        assert len(consumer.delivered) == 1 and len(rec.events) == 1
        assert isinstance(consumer.delivered[0].event, observe.RunStarted)
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
        assert [d.event for d in consumer.delivered] == events_ref[:5]
    finally:
        c.stop()
        consumer.close()
    assert c.outcome.stopped


# ---------------------------------------------------------------------------
# Pause and step

def test_start_paused_then_step_events(ref_p1, no_thread_errors):
    events_ref = ref_p1[0]
    c, rec, _ = make()
    c.start_run(P1, seed=1, paused=True)
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
        assert rec.events == events_ref[:5]
    finally:
        c.stop()
    assert c.outcome.stopped and c.outcome.events == 5


def test_step_iteration_stops_after_the_next_iteration_event(ref_p1, no_thread_errors):
    events_ref = ref_p1[0]
    iteration_at = [i for i, e in enumerate(events_ref) if isinstance(e, observe.IterationBegan)]
    c, rec, _ = make()
    c.start_run(P1, seed=1, paused=True)
    try:
        for k in range(4):
            c.step_iteration()
            want = iteration_at[k] + 1
            assert wait_until(lambda: len(rec.events) == want and c.state == "paused"), \
                (k, len(rec.events), want)
            time.sleep(0.03)
            assert len(rec.events) == want
            assert isinstance(rec.events[-1], observe.IterationBegan)
            assert rec.events[-1].n == events_ref[iteration_at[k]].n
        # A step in the middle of an iteration, then to the next one.
        c.step()
        assert wait_until(lambda: len(rec.events) == iteration_at[3] + 2)
        c.step_iteration()
        assert wait_until(lambda: len(rec.events) == iteration_at[4] + 1
                          and c.state == "paused")
        assert rec.events == events_ref[:iteration_at[4] + 1]
    finally:
        c.stop()


def test_step_iteration_at_the_last_iteration_runs_to_the_end(ref_p1, no_thread_errors):
    events_ref = ref_p1[0]
    last = max(i for i, e in enumerate(events_ref) if isinstance(e, observe.IterationBegan))
    c, rec, _ = make()
    c.start_run(P1, seed=1, paused=True)
    for _ in range(last + 1):
        c.step()
    assert wait_until(lambda: len(rec.events) == last + 1 and c.state == "paused")
    c.step_iteration()
    assert c.wait(PROMPT)
    assert rec.events == events_ref and not c.outcome.stopped


def test_pause_and_play(ref_p1, no_thread_errors):
    events_ref = ref_p1[0]
    c, rec, consumer = make(deliver_auto=False)
    c.start_run(P1, seed=1)
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
        assert rec.events == events_ref[:n + 5]
    finally:
        c.stop()
        consumer.close()


def test_pause_while_running_stops_within_an_event(no_thread_errors):
    c, rec, _ = make()
    c.start_run(P3, seed=1, max_iterations=20000)
    assert wait_until(lambda: len(rec.events) > 200)
    c.pause()
    assert wait_until(lambda: c.state == "paused")
    n = len(rec.events)
    time.sleep(QUIET)
    assert len(rec.events) == n
    c.run_to_end()
    assert wait_until(lambda: len(rec.events) > n + 100)
    c.stop()
    assert c.outcome.stopped


def test_mode_reports(no_thread_errors):
    c, rec, _ = make()
    assert c.state == "idle" and c.mode == "paused"
    c.start_run(P1, seed=1, paused=True)
    assert wait_until(lambda: c.state == "paused")
    c.step()
    assert c.mode in ("step", "paused")
    c.step_iteration()
    assert c.mode == "step-iteration"
    c.play()
    assert c.mode == "playing"
    c.run_to_end()
    assert c.mode == "run-to-end"
    assert c.wait(PROMPT)
    assert c.state == "finished"
    with pytest.raises(ValueError):
        c.set_delay(-1)


# ---------------------------------------------------------------------------
# Speed

def test_the_delay_paces_play_but_not_run_to_end(no_thread_errors):
    c, rec, _ = make(delay=0.04)
    c.start_run(P1, seed=1)
    time.sleep(0.4)
    n = len(rec.events)
    # About 10 events in 0.4 s (each is followed by a 0.04 s delay).
    assert 2 <= n <= 12, n
    c.set_delay(0.0)
    assert c.delay == 0.0
    assert c.wait(PROMPT)
    assert not c.outcome.stopped

    c, rec, _ = make(delay=10.0)
    t = time.monotonic()
    c.start_run(P1, seed=1)
    assert wait_until(lambda: len(rec.events) == 1)
    time.sleep(0.1)
    assert len(rec.events) == 1
    # run-to-end cuts the delay short and ignores it from then on.
    c.run_to_end()
    assert c.wait(PROMPT)
    assert time.monotonic() - t < PROMPT
    assert len(rec.events) > 200


def test_steps_are_not_delayed(no_thread_errors):
    c, rec, _ = make(delay=10.0)
    c.start_run(P1, seed=1, paused=True)
    t = time.monotonic()
    for n in range(1, 4):
        c.step()
        assert wait_until(lambda: len(rec.events) == n and c.state == "paused")
    c.step_iteration()
    assert wait_until(lambda: isinstance(rec.events[-1], observe.IterationBegan)
                      and c.state == "paused")
    assert time.monotonic() - t < PROMPT
    c.stop()


def test_pause_cuts_the_delay_short(no_thread_errors):
    c, rec, _ = make(delay=10.0)
    c.start_run(P1, seed=1)
    assert wait_until(lambda: len(rec.events) == 1)
    c.pause()
    assert wait_until(lambda: c.state == "paused")
    c.step()
    assert wait_until(lambda: len(rec.events) == 2 and c.state == "paused")
    c.stop()


# ---------------------------------------------------------------------------
# Stop and restart

def _stop_promptly(c):
    t = time.monotonic()
    c.stop()
    assert time.monotonic() - t < PROMPT
    assert not c.is_alive()
    assert c.outcome.stopped and c.outcome.error is None
    assert c.state == "stopped"
    assert engine_is_idle()


def test_stop_while_paused(no_thread_errors, capsys):
    c, rec, _ = make()
    c.start_run(P1, seed=1, paused=True)
    assert wait_until(lambda: c.state == "paused")
    _stop_promptly(c)
    assert rec.events == []
    assert capsys.readouterr().err == ""


def test_stop_while_waiting_for_an_acknowledgment(no_thread_errors, capsys):
    c, rec, consumer = make(deliver_auto=False)
    c.start_run(P1, seed=1)
    assert wait_until(lambda: len(consumer.delivered) == 1)
    _stop_promptly(c)
    assert len(rec.events) == 1
    # A late acknowledgment of the stopped run's delivery does nothing.
    assert consumer.ack_next() is False
    assert capsys.readouterr().err == ""


def test_stop_in_a_delay(no_thread_errors):
    c, rec, _ = make(delay=10.0)
    c.start_run(P1, seed=1)
    assert wait_until(lambda: len(rec.events) == 1)
    _stop_promptly(c)


def test_stop_while_running_flat_out(no_thread_errors, capsys):
    c, rec, consumer = make(deliver_auto=True)
    try:
        c.start_run(P3, seed=1, max_iterations=20000)
        assert wait_until(lambda: len(rec.events) > 500)
        _stop_promptly(c)
    finally:
        consumer.close()
    assert not isinstance(rec.events[-1], observe.RunEnded)
    assert c.outcome.result is None
    assert c.outcome.events == len(rec.events)
    assert consumer.violations == []
    assert capsys.readouterr().err == ""


def test_a_run_after_a_stop_is_unchanged(ref_p1, no_thread_errors):
    c, rec, _ = make()
    c.start_run(P1, seed=1, paused=True)
    c.step()
    c.step()
    assert wait_until(lambda: len(rec.events) == 2)
    c.stop()
    events_ref, result_ref, out_ref, trace_ref = ref_p1
    assert reference(P1, 1) == ref_p1
    assert plain(P1, 1) == (result_ref, out_ref)


def test_stop_from_an_observer_on_the_worker(no_thread_errors):
    box = [None]

    class Stopper:
        def __init__(self):
            self.n = 0

        def on_event(self, event):
            self.n += 1
            if self.n == 30:
                box[0].stop()   # on the worker: must not join itself

    stopper = Stopper()
    c = RunController(observers=[stopper])
    box[0] = c
    c.start_run(P1, seed=1)
    assert c.wait(PROMPT)
    assert c.outcome.stopped and stopper.n == 30 and c.outcome.events == 30


def test_stop_and_wait_are_harmless_when_idle_or_finished(no_thread_errors):
    c, rec, _ = make()
    c.stop()
    assert c.wait(0.01)
    assert c.outcome is None and c.state == "idle"
    c.start_run(P1, seed=1)
    assert c.wait(PROMPT)
    c.stop()
    assert c.state == "finished" and not c.outcome.stopped
    c.play()
    c.step()
    c.pause()


def test_restart_with_a_new_problem_and_seed(no_thread_errors):
    ref3 = reference(P3, 8)
    finished = []
    c, rec, consumer = make(deliver_auto=False, on_finished=finished.append)
    c.start_run(P1, seed=1)
    try:
        for _ in range(20):
            consumer.ack_next()
        assert wait_until(lambda: len(consumer.delivered) == 21)
        old = list(consumer.delivered)
        consumer.auto = True
        consumer.thread = threading.Thread(target=consumer._loop, daemon=True)
        rec.events.clear()
        c.start_run(P3, seed=8)      # stops the old run first
        consumer.thread.start()
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    assert len(finished) == 2
    assert finished[0].stopped and finished[0].events == 21
    assert not finished[1].stopped and finished[1].result == ref3[1]
    assert finished[1].output == ref3[2]
    assert rec.events == ref3[0]
    # The new run's tickets are new: an old acknowledgment does nothing.
    assert all(d.ticket < consumer.delivered[21].ticket for d in old)
    assert [d.index for d in consumer.delivered[21:]] == list(range(len(ref3[0])))


def test_only_one_run_at_a_time(no_thread_errors):
    a, rec_a, _ = make()
    b, rec_b, _ = make()
    a.start_run(P1, seed=1, paused=True)
    assert wait_until(lambda: a.state == "paused")
    with pytest.raises(run_controller.EngineBusy):
        b.start_run(P1, seed=2)
    a.stop()
    b.start_run(P1, seed=2)
    assert b.wait(PROMPT)
    assert b.outcome.result == plain(P1, 2)[0]


def test_bad_input_is_refused_before_a_thread_starts(no_thread_errors):
    c, rec, _ = make()
    with pytest.raises(ValueError):
        c.start_run(P1, seed=1, max_iterations=0)
    with pytest.raises(ValueError):
        c.start_run([1, 2, 3], seed=1)
    assert c.state == "idle" and not c.is_alive()


# ---------------------------------------------------------------------------
# Observers, callbacks

def test_a_raising_observer_is_reported_and_changes_nothing(ref_p1, no_thread_errors):
    errors = []

    class Bad:
        def on_event(self, event):
            if isinstance(event, observe.IterationBegan):
                raise RuntimeError("boom")

    rec = Recorder()
    c = RunController(observers=[Bad(), rec], on_error=errors.append)
    c.start_run(P1, seed=1)
    assert c.wait(PROMPT)
    assert rec.events == ref_p1[0]
    assert c.outcome.result == ref_p1[1]
    assert len(errors) == 45 and all(isinstance(e.exception, RuntimeError) for e in errors)


def test_raising_callbacks_are_reported_and_change_nothing(ref_p1, no_thread_errors):
    errors = []

    def boom(*args):
        raise RuntimeError("boom")

    rec = Recorder()
    c = RunController(observers=[rec], deliver=boom, on_state=boom, on_finished=boom,
                      on_error=errors.append)
    c.start_run(P1, seed=1)
    assert c.wait(PROMPT)
    assert rec.events == ref_p1[0] and c.outcome.result == ref_p1[1]
    by = {}
    for e in errors:
        by.setdefault(e.event is None, []).append(e)
    # deliver failed on every event (and was not waited for); on_state on
    # "running" and "finished", on_finished once: three with no event.
    assert len(by[False]) == len(ref_p1[0])
    assert len(by[True]) == 3


def test_a_callback_error_goes_to_stderr_by_default(capsys, no_thread_errors):
    def boom(outcome):
        raise RuntimeError("boom")

    c = RunController(on_finished=boom)
    c.start_run(P1, seed=1, max_iterations=3)
    assert c.wait(PROMPT)
    err = capsys.readouterr().err
    assert "failed outside an event" in err and "RuntimeError: boom" in err


def test_subscribe_and_state_callbacks(ref_p1, no_thread_errors):
    states = []
    c = RunController(on_state=states.append)
    rec = c.subscribe(Recorder())
    c.start_run(P1, seed=1, paused=True)
    assert wait_until(lambda: c.state == "paused")
    c.play()
    assert c.wait(PROMPT)
    assert rec.events == ref_p1[0]
    assert states[0] == "paused" and states[-1] == "finished"
    assert "running" in states
    assert all(a != b for a, b in zip(states, states[1:]))


def test_session_recorder_on_a_stopped_run_is_complete(tmp_path, no_thread_errors):
    from numbo.session import load_session
    path = tmp_path / "s.jsonl"
    recorder = SessionRecorder(path)
    c = RunController(observers=[recorder])
    c.start_run(P1, seed=1, paused=True)
    for _ in range(7):
        c.step()
    assert wait_until(lambda: c.events_published == 7 and c.state == "paused")
    c.stop()
    recorder.close()
    s = load_session(path)
    assert len(s.events) == 7 and not s.ended


# ---------------------------------------------------------------------------
# Replays

def test_a_session_replay_through_the_gate(tmp_path, ref_p1, no_thread_errors):
    from numbo.session import write_session
    path = tmp_path / "p1.jsonl"
    write_session(path, ref_p1[0])
    c, rec, consumer = make(deliver_auto=False)
    c.start_session(path, paused=True)
    try:
        assert wait_until(lambda: c.state == "paused")
        assert rec.events == []
        c.step_iteration()
        while not (rec.events and c.state == "paused" and consumer.queue.empty()):
            try:
                consumer.ack(consumer.queue.get(timeout=0.05))
            except queue.Empty:
                pass
        assert isinstance(rec.events[-1], observe.IterationBegan)
        n = len(rec.events)
        consumer.auto = True
        consumer.thread = threading.Thread(target=consumer._loop, daemon=True)
        consumer.thread.start()
        c.run_to_end()
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    assert rec.events == ref_p1[0] and n < len(rec.events)
    o = c.outcome
    assert o.source == "session" and not o.stopped and o.error is None
    assert o.events == len(ref_p1[0])
    assert consumer.violations == []


def test_a_broken_session_ends_with_its_error(tmp_path, ref_p1, no_thread_errors, capsys):
    from numbo.session import SessionError, write_session
    path = tmp_path / "p1.jsonl"
    write_session(path, ref_p1[0])
    lines = path.read_text().splitlines(keepends=True)
    path.write_text("".join(lines[:50]))
    c, rec, _ = make()
    c.start_session(path)
    assert c.wait(PROMPT)
    assert isinstance(c.outcome.error, SessionError)
    assert "truncated" in str(c.outcome.error)
    assert rec.events == ref_p1[0][:49] and not c.outcome.stopped
    assert c.state == "finished"
    assert capsys.readouterr().err == ""


def test_an_oracle_replay_through_the_gate(tmp_path, ref_p1, no_thread_errors):
    events_ref, result_ref, out_ref, trace_ref = reference(P1, 1, rng_events=True)
    path = tmp_path / "trace.jsonl"
    path.write_text(trace_ref)
    c, rec, consumer = make(deliver_auto=True)
    try:
        c.start_oracle(path)
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    o = c.outcome
    assert o.source == "oracle" and not o.stopped and o.error is None
    assert o.oracle.divergence is None and o.oracle.rng_events
    assert o.oracle.lines == len(trace_ref.splitlines())
    assert o.result == result_ref and o.output == out_ref
    assert rec.events == events_ref
    assert consumer.violations == []


def test_an_oracle_replay_that_diverges(tmp_path, ref_p1, no_thread_errors):
    import json
    from numbo.session import TraceMismatch
    lines = ref_p1[3].splitlines()
    i = next(k for k, line in enumerate(lines) if line.startswith('{"ev": "iteration", "n": 20,'))
    obj = json.loads(lines[i])
    obj["temperature"] += 1
    lines[i] = json.dumps(obj)
    path = tmp_path / "bad.jsonl"
    path.write_text("\n".join(lines) + "\n")
    c, rec, _ = make()
    c.start_oracle(path)
    assert c.wait(PROMPT * 5)
    assert isinstance(c.outcome.error, TraceMismatch)
    assert c.outcome.oracle is not None and c.outcome.oracle.divergence is not None


def test_stop_an_oracle_replay(tmp_path, ref_p1, no_thread_errors):
    path = tmp_path / "trace.jsonl"
    path.write_text(ref_p1[3])
    c, rec, _ = make()
    c.start_oracle(path, paused=True)
    for _ in range(12):
        c.step()
    assert wait_until(lambda: len(rec.events) == 12 and c.state == "paused")
    _stop_promptly(c)
    assert rec.events == ref_p1[0][:12]


# ---------------------------------------------------------------------------
# Replaying an event list (item 10: a replay's playback from any event)

@pytest.mark.parametrize("start", [0, 1, 100, 285])
def test_events_replay_from_an_index_through_the_gate(ref_p1, start, no_thread_errors):
    events = ref_p1[0]
    c, rec, consumer = make(deliver_auto=True)
    try:
        c.start_events(events, start=start)
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    assert rec.events == events[start:]
    assert [d.index for d in consumer.delivered] == list(range(start, len(events)))
    assert [d.event for d in consumer.delivered] == events[start:]
    o = c.outcome
    assert o.source == "events" and not o.stopped and o.error is None
    assert o.result is None and o.output is None and o.oracle is None
    assert o.events == len(events)            # the index after the last one published
    assert consumer.violations == []
    assert engine_is_idle()


def test_events_replay_steps_and_steps_by_iteration(ref_p1, no_thread_errors):
    events = ref_p1[0]
    c, rec, consumer = make(deliver_auto=True)
    try:
        c.start_events(events, start=10, paused=True)
        assert wait_until(lambda: c.state == "paused")
        time.sleep(QUIET)
        assert rec.events == []
        c.step()
        assert wait_until(lambda: len(rec.events) == 1 and c.state == "paused")
        assert rec.events == [events[10]]
        c.step_iteration()
        assert wait_until(lambda: c.state == "paused" and consumer.queue.empty()
                          and isinstance(rec.events[-1], observe.IterationBegan))
        k = 11 + len(rec.events) - 1
        assert rec.events == events[10:k]
        assert not any(isinstance(e, observe.IterationBegan) for e in events[11:k - 1])
        c.run_to_end()
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    assert rec.events == events[10:]


def test_events_replay_stops_promptly(ref_p1, no_thread_errors, capsys):
    c, rec, consumer = make(deliver_auto=False)
    c.start_events(ref_p1[0], start=5)
    assert wait_until(lambda: consumer.outstanding is not None)
    _stop_promptly(c)
    assert c.outcome.stopped and c.outcome.source == "events"
    assert c.outcome.events == 6
    assert capsys.readouterr().err == ""


def test_events_replay_checks_its_start(ref_p1, no_thread_errors):
    c, rec, _ = make()
    for bad in (-1, len(ref_p1[0]) + 1, 1.5):
        with pytest.raises(ValueError):
            c.start_events(ref_p1[0], start=bad)
    assert not c.is_alive() and c.state == "idle"


def test_delivery_is_a_frozen_record():
    d = Delivery(ticket=3, index=0, event=observe.RunStarted(
        problem=(1,), seed=1, max_iterations=5, rng="splitmix64", pnet=()))
    with pytest.raises(Exception):
        d.index = 2

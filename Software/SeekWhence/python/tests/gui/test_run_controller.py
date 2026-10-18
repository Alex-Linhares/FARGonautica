"""Loop0004 item 01: the run controller (`seekwhence/gui/models/run_controller.py`), the
engine on a worker thread behind a per-event gate, after the Numbo GUI's
`tests/test_run_controller.py`. Threading tests, no Qt.

- A run through the gate, with the tap, gives the same event records, trace text, draws
  and outcome as a plain run, for the three appendix runs [M86 pp. 164-179], and so does
  `headless.run(window=False)`, the determinism contract.
- No event is published before the previous one is acknowledged; the worker blocks until
  each acknowledgment.
- The modes: paused and step (one event), step task (through the next `chosen`), step
  term (through the next `prompt`), play with a delay, run to end.
- Stop, from any wait, leaves no live thread, and a run after a stop is unchanged; one
  run at a time; a raising observer or callback is reported and changes nothing.
"""

import io
import queue
import random
import threading
import time

import pytest

from seekwhence import bench
from seekwhence import sequences as sq
from seekwhence.config import Config
from seekwhence.gui import headless
from seekwhence.gui.models import run_controller
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.run_controller import Delivery, RunController, RunOutcome
from seekwhence.gui.models.tap import Tap
from seekwhence.trace import event_record

from golden import APPENDIX, appendix_inputs

RUN_IDS = [run_id for run_id, _ in APPENDIX]

# How long "nothing happens" is watched for, and the bound on prompt ends.
QUIET = 0.25
PROMPT = 5.0

# A long run for the flat-out tests: the Blackburn sequence 11 [M86 pp. 6-7], 16 terms
# fed as the bench feeds them, every guess judged against the true continuation
# (`bench.ContinuationAnswerer`, DECISION D-070): about 10,000 tasks on seed 1.
LONG = sq.find("blackburn-11")
LONG_TERMS = list(LONG.prefix(16))


def long_answerer(console):
    return bench.ContinuationAnswerer(console, LONG.terms)


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


def make(deliver_auto=None, tap=False, max_pause=0.0005, **kw):
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
        kw["on_session"] = the_tap.attach_session
    if deliver_auto is not None:
        consumer = Consumer(box, auto=deliver_auto, max_pause=max_pause)
        kw["deliver"] = consumer
    c = RunController(observers=observers, **kw)
    box[0] = c
    return c, rec, consumer, log


def start_appendix(c, run_id, seed, **kw):
    config, lines, answers = appendix_inputs(run_id)
    c.start_session(config, seed, lines, answers, **kw)


def start_long(c, **kw):
    c.start_session(Config(verbose=False), 1, LONG_TERMS, [], answerer=long_answerer, **kw)


# ---------------------------------------------------------------------------
# Event order and results

@pytest.mark.parametrize("run_id,seed", APPENDIX)
def test_run_to_end_gives_the_exact_events_and_results(appendix_runs, run_id, seed,
                                                       thread_errors):
    ref = appendix_runs[run_id]
    c, rec, consumer, log = make(deliver_auto=True, tap=True)
    try:
        start_appendix(c, run_id, seed)
        c.run_to_end()
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    o = c.outcome
    assert isinstance(o, RunOutcome)
    assert o.source == "session" and not o.stopped and o.error is None
    assert o.result["outcome"] == ref.outcome == "ok"
    assert o.result["summary"] == ref.summary
    assert o.result["draws"] == ref.draws
    assert o.output == ref.text
    assert rec.events == ref.events
    assert [event_record(e) for e in log.events] == ref.records
    assert log.snapshots and log.last_snapshot.done
    assert o.events == len(ref.events) == c.events_published
    assert c.state == "finished" and not c.is_alive()
    assert consumer.violations == []
    assert [d.event for d in consumer.delivered] == ref.events
    assert c.errors == []


@pytest.mark.parametrize("run_id,seed", APPENDIX)
def test_headless_run_equals_a_plain_session(appendix_runs, run_id, seed, thread_errors):
    ref = appendix_runs[run_id]
    got = headless.run(ref.config, seed, ref.lines, ref.answers, window=False)
    assert got["records"] == ref.records
    assert got["draws"] == ref.draws and got["outcome"] == ref.outcome
    assert threading.active_count() == 1 or not any(
        t.name.startswith("seekwhence-") and t.is_alive() for t in threading.enumerate())


def test_a_long_run_judged_by_the_continuation_is_unchanged_by_the_gate(thread_errors):
    out = io.StringIO()
    from seekwhence.session import Session
    plain = Session(Config(verbose=False), 1, out=out, lines=list(LONG_TERMS),
                    answerer=long_answerer)
    plain.run()
    c, rec, consumer, log = make(deliver_auto=True, tap=True, max_pause=0)
    try:
        start_long(c)
        c.run_to_end()
        assert c.wait(PROMPT * 10)
    finally:
        consumer.close()
    assert c.outcome.result["outcome"] == plain.outcome
    assert c.outcome.result["draws"] == plain.world.rng.draws
    assert c.outcome.output == out.getvalue()
    assert [event_record(e) for e in log.events] == \
        [event_record(e) for e in plain.recorder.events]
    assert plain.outcome == "no-terms" and plain.world.clock > 5000
    assert len(log.events) > 10000 and consumer.violations == []
    assert len(log.snapshots) < len(log.events) / 10      # the keyframe rule (G-003)


def test_no_event_is_published_before_the_previous_is_acknowledged(appendix_runs,
                                                                   thread_errors):
    ref = appendix_runs["run1_3737"]
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
        start_appendix(c, ref.run_id, ref.seed)
        assert c.wait(PROMPT * 10)
    finally:
        consumer.close()
    assert consumer.violations == []
    assert all(outstanding is None for _, outstanding in seen_by_observer)
    assert [e for e, _ in seen_by_observer] == ref.events
    assert [d.event for d in consumer.delivered] == ref.events
    assert [d.index for d in consumer.delivered] == list(range(len(ref.events)))
    tickets = [d.ticket for d in consumer.delivered]
    assert len(set(tickets)) == len(tickets)
    assert c.outcome.result["draws"] == ref.draws


def test_the_worker_blocks_until_each_acknowledgment(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, consumer, _ = make(deliver_auto=False)
    start_appendix(c, ref.run_id, ref.seed)
    try:
        assert wait_until(lambda: len(consumer.delivered) == 1)
        time.sleep(QUIET)
        assert len(consumer.delivered) == 1 and len(rec.events) == 1
        assert consumer.delivered[0].event.kind == "start"
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
        assert [d.event for d in consumer.delivered] == ref.events[:5]
    finally:
        c.stop()
        consumer.close()
    assert c.outcome.stopped and not c.is_alive()


# ---------------------------------------------------------------------------
# Pause and step

def test_start_paused_then_step_events(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make()
    start_appendix(c, ref.run_id, ref.seed, paused=True)
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
        assert rec.events == ref.events[:5]
    finally:
        c.stop()
    assert c.outcome.stopped and c.outcome.events == 5 and c.outcome.result is None


def test_step_task_stops_after_the_next_chosen_event(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    chosen_at = [i for i, e in enumerate(ref.events) if e.kind == "chosen"]
    c, rec, _, _ = make()
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    try:
        for k in range(4):
            c.step_task()
            want = chosen_at[k] + 1
            assert wait_until(lambda: len(rec.events) == want and c.state == "paused"), \
                (k, len(rec.events), want)
            time.sleep(0.03)
            assert len(rec.events) == want
            assert rec.events[-1].kind == "chosen"
            assert rec.events[-1].clock == ref.events[chosen_at[k]].clock == k + 1
        # A step in the middle of a task, then to the next one.
        c.step()
        assert wait_until(lambda: len(rec.events) == chosen_at[3] + 2)
        c.step_task()
        assert wait_until(lambda: len(rec.events) == chosen_at[4] + 1
                          and c.state == "paused")
        assert rec.events == ref.events[:chosen_at[4] + 1]
    finally:
        c.stop()


def test_step_term_stops_after_the_next_prompt_event(appendix_runs, thread_errors):
    ref = appendix_runs["run3_121231234"]
    prompt_at = [i for i, e in enumerate(ref.events) if e.kind == "prompt"]
    assert len(prompt_at) >= 5
    c, rec, _, _ = make()
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    try:
        for k in range(3):
            c.step_term()
            want = prompt_at[k] + 1
            assert wait_until(lambda: len(rec.events) == want and c.state == "paused"), \
                (k, len(rec.events), want)
            time.sleep(0.03)
            assert len(rec.events) == want
            assert rec.events[-1].kind == "prompt"
            # The next event is the next term entered: the display is waiting for a term.
            assert ref.events[want].kind == "term"
        assert rec.events == ref.events[:prompt_at[2] + 1]
    finally:
        c.stop()


def test_step_term_at_the_last_prompt_runs_to_the_end(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    last = max(i for i, e in enumerate(ref.events) if e.kind == "prompt")
    c, rec, _, _ = make()
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    for _ in range(last + 1):
        c.step()
    assert wait_until(lambda: len(rec.events) == last + 1 and c.state == "paused")
    c.step_term()
    assert c.wait(PROMPT)
    assert rec.events == ref.events and not c.outcome.stopped


def test_pause_and_play(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, consumer, _ = make(deliver_auto=False)
    start_appendix(c, ref.run_id, ref.seed)
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
        assert rec.events == ref.events[:n + 5]
    finally:
        c.stop()
        consumer.close()


def test_pause_while_running_stops_within_an_event(thread_errors):
    c, rec, _, _ = make()
    start_long(c)
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


def test_mode_reports(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make()
    assert c.state == "idle" and c.mode == "paused"
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    assert wait_until(lambda: c.state == "paused")
    c.step()
    assert c.mode in ("step", "paused")
    c.step_task()
    assert c.mode == "step-task"
    c.step_term()
    assert c.mode == "step-term"
    c.play()
    assert c.mode == "playing"
    c.run_to_end()
    assert c.mode == "run-to-end"
    assert c.wait(PROMPT)
    assert c.state == "finished"
    assert set(run_controller.MODES) >= {"paused", "playing", "step", "step-task",
                                         "step-term", "run-to-end"}
    with pytest.raises(ValueError):
        c.set_delay(-1)


# ---------------------------------------------------------------------------
# Speed

def test_the_delay_paces_play_but_not_run_to_end(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make(delay=0.04)
    start_appendix(c, ref.run_id, ref.seed)
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
    start_appendix(c, ref.run_id, ref.seed)
    assert wait_until(lambda: len(rec.events) == 1)
    time.sleep(0.1)
    assert len(rec.events) == 1
    # run-to-end cuts the delay short and ignores it from then on.
    c.run_to_end()
    assert c.wait(PROMPT)
    assert time.monotonic() - t < PROMPT
    assert len(rec.events) == len(ref.events)


def test_steps_are_not_delayed(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make(delay=10.0)
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    t = time.monotonic()
    for n in range(1, 4):
        c.step()
        assert wait_until(lambda: len(rec.events) == n and c.state == "paused")
    c.step_task()
    assert wait_until(lambda: rec.events[-1].kind == "chosen" and c.state == "paused")
    assert time.monotonic() - t < PROMPT
    c.stop()


def test_pause_cuts_the_delay_short(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make(delay=10.0)
    start_appendix(c, ref.run_id, ref.seed)
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


def test_stop_while_paused(appendix_runs, thread_errors, capsys):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make()
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    assert wait_until(lambda: c.state == "paused")
    _stop_promptly(c)
    assert rec.events == []
    assert capsys.readouterr().err == ""


def test_stop_while_waiting_for_an_acknowledgment(appendix_runs, thread_errors, capsys):
    ref = appendix_runs["run1_3737"]
    c, rec, consumer, _ = make(deliver_auto=False)
    start_appendix(c, ref.run_id, ref.seed)
    assert wait_until(lambda: len(consumer.delivered) == 1)
    _stop_promptly(c)
    assert len(rec.events) == 1
    # A late acknowledgment of the stopped run's delivery does nothing.
    assert consumer.ack_next() is False
    assert capsys.readouterr().err == ""


def test_stop_in_a_delay(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make(delay=10.0)
    start_appendix(c, ref.run_id, ref.seed)
    assert wait_until(lambda: len(rec.events) == 1)
    _stop_promptly(c)


def test_stop_while_running_flat_out(thread_errors, capsys):
    c, rec, consumer, log = make(deliver_auto=True, tap=True)
    try:
        start_long(c)
        assert wait_until(lambda: len(rec.events) > 500)
        _stop_promptly(c)
    finally:
        consumer.close()
    assert rec.events[-1].kind != "ended"
    assert c.outcome.result is None
    assert c.outcome.events == len(rec.events) == len(log.events)
    assert consumer.violations == []
    assert capsys.readouterr().err == ""


def test_a_run_after_a_stop_is_unchanged(appendix_runs, thread_errors):
    from golden import run_plain
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make()
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    c.step()
    c.step()
    assert wait_until(lambda: len(rec.events) == 2)
    c.stop()
    again = run_plain(ref.run_id, ref.seed)
    assert (again.records, again.text, again.draws) == (ref.records, ref.text, ref.draws)


def test_stop_from_an_observer_on_the_worker(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
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
    start_appendix(c, ref.run_id, ref.seed)
    assert c.wait(PROMPT)
    assert c.outcome.stopped and stopper.n == 30 and c.outcome.events == 30


def test_stop_and_wait_are_harmless_when_idle_or_finished(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, _, _ = make()
    c.stop()
    assert c.wait(0.01)
    assert c.outcome is None and c.state == "idle"
    start_appendix(c, ref.run_id, ref.seed)
    assert c.wait(PROMPT)
    c.stop()
    assert c.state == "finished" and not c.outcome.stopped
    c.play()
    c.step()
    c.pause()


def test_restart_with_a_new_run_and_seed(appendix_runs, thread_errors):
    r1, r3 = appendix_runs["run1_3737"], appendix_runs["run3_121231234"]
    finished = []
    c, rec, consumer, log = make(deliver_auto=False, tap=True, on_finished=finished.append)
    start_appendix(c, r1.run_id, r1.seed)
    try:
        for _ in range(20):
            consumer.ack_next()
        assert wait_until(lambda: len(consumer.delivered) == 21)
        old = list(consumer.delivered)
        consumer.auto = True
        consumer.thread = threading.Thread(target=consumer._loop, daemon=True)
        rec.events.clear()
        start_appendix(c, r3.run_id, r3.seed)      # stops the old run first
        consumer.thread.start()
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    assert len(finished) == 2
    assert finished[0].stopped and finished[0].events == 21
    assert not finished[1].stopped and finished[1].result["draws"] == r3.draws
    assert finished[1].output == r3.text
    assert rec.events == r3.events
    # The log began anew at the second run's start event, snapshots included.
    assert log.events == r3.events
    assert log.last_snapshot.hypothesis.form == "(S-group 1 (Countup 2))"
    # The new run's tickets are new: an old acknowledgment does nothing.
    assert all(d.ticket < consumer.delivered[21].ticket for d in old)
    assert [d.index for d in consumer.delivered[21:]] == list(range(len(r3.events)))


def test_only_one_run_at_a_time(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    a, rec_a, _, _ = make()
    b, rec_b, _, _ = make()
    start_appendix(a, ref.run_id, ref.seed, paused=True)
    assert wait_until(lambda: a.state == "paused")
    with pytest.raises(run_controller.EngineBusy):
        start_appendix(b, ref.run_id, ref.seed)
    a.stop()
    start_appendix(b, ref.run_id, ref.seed)
    assert b.wait(PROMPT)
    assert b.outcome.result["draws"] == ref.draws


def test_bad_input_is_refused_before_a_thread_starts(thread_errors):
    c, rec, _, _ = make()
    with pytest.raises(ValueError):
        c.start_events([], start=1)
    assert c.state == "idle" and not c.is_alive()


def test_a_broken_session_ends_with_the_error(thread_errors):
    c, rec, _, _ = make()
    c.start_session(Config(), -1, [3, 7], [])        # a negative seed is refused by Rng
    assert c.wait(PROMPT)
    assert c.state == "finished" and not c.outcome.stopped
    assert isinstance(c.outcome.error, ValueError) and c.outcome.result is None


# ---------------------------------------------------------------------------
# Observers, callbacks

def test_a_raising_observer_is_reported_and_changes_nothing(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    errors = []

    class Bad:
        def on_event(self, event):
            if event.kind == "chosen":
                raise RuntimeError("boom")

    rec = Recorder()
    c = RunController(observers=[Bad(), rec], on_error=errors.append)
    start_appendix(c, ref.run_id, ref.seed)
    assert c.wait(PROMPT)
    assert rec.events == ref.events
    assert c.outcome.result["draws"] == ref.draws
    chosen = sum(1 for e in ref.events if e.kind == "chosen")
    assert len(errors) == chosen and all(isinstance(e.exception, RuntimeError) for e in errors)
    assert c.errors == errors


def test_raising_callbacks_are_reported_and_change_nothing(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    errors = []

    def boom(*args):
        raise RuntimeError("boom")

    rec = Recorder()
    c = RunController(observers=[rec], deliver=boom, on_state=boom, on_finished=boom,
                      on_session=boom, on_error=errors.append)
    start_appendix(c, ref.run_id, ref.seed)
    assert c.wait(PROMPT)
    # on_session raised on the worker: a broken run, reported as its error, nothing run.
    assert isinstance(c.outcome.error, RuntimeError) and c.outcome.result is None
    assert len(rec.events) == 1 and c.outcome.events == 1

    errors.clear()
    rec = Recorder()
    c = RunController(observers=[rec], deliver=boom, on_state=boom, on_finished=boom,
                      on_error=errors.append)
    start_appendix(c, ref.run_id, ref.seed)
    assert c.wait(PROMPT)
    assert rec.events == ref.events and c.outcome.result["draws"] == ref.draws
    by = {}
    for e in errors:
        by.setdefault(e.event is None, []).append(e)
    # deliver failed on every event (and was not waited for); on_state on "running" and
    # "finished", on_finished once: three with no event.
    assert len(by[False]) == len(ref.events)
    assert len(by[True]) == 3


def test_a_callback_error_goes_to_stderr_by_default(appendix_runs, capsys, thread_errors):
    ref = appendix_runs["run1_3737"]

    def boom(outcome):
        raise RuntimeError("boom")

    c = RunController(on_finished=boom)
    start_appendix(c, ref.run_id, ref.seed)
    assert c.wait(PROMPT)
    err = capsys.readouterr().err
    assert "failed outside an event" in err and "RuntimeError: boom" in err


def test_subscribe_and_state_callbacks(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    states = []
    sessions = []
    c = RunController(on_state=states.append, on_session=sessions.append)
    rec = c.subscribe(Recorder())
    start_appendix(c, ref.run_id, ref.seed, paused=True)
    assert wait_until(lambda: c.state == "paused")
    assert sessions == []           # the session is built at the first event, the start
    c.play()
    assert c.wait(PROMPT)
    assert rec.events == ref.events
    assert states[0] == "paused" and states[-1] == "finished"
    assert "running" in states
    assert all(a != b for a, b in zip(states, states[1:]))
    assert len(sessions) == 1 and sessions[0].outcome == "ok"
    c.unsubscribe(rec)


# ---------------------------------------------------------------------------
# Replays of recorded events

def test_an_events_replay_through_the_gate(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, consumer, _ = make(deliver_auto=False)
    c.start_events(ref.events, paused=True)
    try:
        assert wait_until(lambda: c.state == "paused")
        assert rec.events == []
        c.step_task()
        while not (rec.events and c.state == "paused" and consumer.queue.empty()):
            try:
                consumer.ack(consumer.queue.get(timeout=0.05))
            except queue.Empty:
                pass
        assert rec.events[-1].kind == "chosen"
        n = len(rec.events)
        consumer.auto = True
        consumer.thread = threading.Thread(target=consumer._loop, daemon=True)
        consumer.thread.start()
        c.run_to_end()
        assert c.wait(PROMPT * 5)
    finally:
        consumer.close()
    assert rec.events == ref.events and n > 1
    o = c.outcome
    assert o.source == "events" and o.result is None and o.output is None
    assert o.events == len(ref.events)
    assert [d.index for d in consumer.delivered] == list(range(len(ref.events)))


def test_an_events_replay_from_the_middle(appendix_runs, thread_errors):
    ref = appendix_runs["run1_3737"]
    c, rec, consumer, _ = make(deliver_auto=True)
    try:
        c.start_events(ref.events, start=100)
        assert c.wait(PROMPT)
    finally:
        consumer.close()
    assert rec.events == ref.events[100:]
    assert [d.index for d in consumer.delivered] == list(range(100, len(ref.events)))
    assert c.outcome.events == len(ref.events)

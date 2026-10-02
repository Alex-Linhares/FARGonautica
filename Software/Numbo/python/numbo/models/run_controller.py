"""The run controller: Numbo on a worker thread behind a per-event gate
(not 1987 source; loop0003).  Qt-free.

A RunController runs one thing at a time on its worker thread: a live run
(harness.run_config), a session file replay (session.read_session), an
oracle trace replay (session.replay_oracle_trace, which re-runs and checks
the trace), or a list of events from a given index (start_events: the GUI's
replay playback, from wherever the timeline is).  Every event goes through the gate, an observer subscribed last
to the run's Subject (so the oracle trace writer has already written it):

  1. it waits while the controller is paused;
  2. it publishes the event to the controller's observers (on the worker
     thread, through a Subject: an observer's exception is reported to
     on_error and changes nothing);
  3. if there is a DELIVER callback, it calls deliver(Delivery(ticket,
     index, event)) and blocks until acknowledge(ticket) (for the GUI: the
     redraw is done), so no event is published before the previous one is
     acknowledged;
  4. when playing, it waits the per-event delay (0: no wait).

The modes: "paused"; "playing" (with the delay); "step" (one event per
step(), then paused); "step-iteration" (on through the next iteration
event, then paused; to the end if there is none); "run-to-end" (no delay
and no pause).  A change of mode cuts a delay short.  Events are never
skipped or coalesced.

stop() wakes every wait, and the gate raises RunStopped, a BaseException:
Subject and run_config catch only Exception, so it unwinds the run
(run_config's and events.publishing's `finally` clauses restore the engine's
state) and the worker ends with outcome.stopped.  Stopping changes nothing
that a later run can see.

The gate draws no random numbers, touches no World state and prints
nothing, so a run under the controller has the result, printed text and
oracle trace of the same run without it (tested).

The engine's run state (events.current, the iteration cap) is per process,
so only one controller's worker runs at a time (EngineBusy), and nothing
else may call harness.run_config while it does.  Starting a new run on a
controller stops and joins its old one first.

Callbacks: on_state(state) and on_finished(outcome) are called on the worker
thread.  The states: "idle" (never started), "running", "paused",
"finished", "stopped".
"""

import contextlib
import dataclasses
import io
import itertools
import threading
import time

from numbo import observe

__all__ = ["MODES", "STATES", "Delivery", "RunOutcome", "RunStopped", "EngineBusy",
           "RunController"]

MODES = ("paused", "playing", "step", "step-iteration", "run-to-end")
STATES = ("idle", "running", "paused", "finished", "stopped")

# How long stop() and a restart wait for the worker to end, in seconds.
JOIN_TIMEOUT = 5.0

# Delivery tickets are unique across runs and controllers, so a late
# acknowledgment from a stopped run never acknowledges a new one.
_tickets = itertools.count(1)

# Held by the worker that is running the engine.
_engine = threading.Lock()


class RunStopped(BaseException):
    """Raised by the gate on the worker to unwind a stopped run.  Not an
    Exception, so that no error handler in the run catches it."""


class EngineBusy(RuntimeError):
    """Another worker is running the engine."""


@dataclasses.dataclass(frozen=True)
class Delivery:
    """An event handed to the consumer: acknowledge(TICKET) releases the
    worker.  INDEX counts the run's events from 0 (for start_events, it is
    the event's index in its list)."""
    ticket: int
    index: int
    event: object


@dataclasses.dataclass(frozen=True)
class RunOutcome:
    """How the worker ended.  SOURCE: "run", "session", "oracle" or
    "events".  STOPPED:
    stop() ended it.  RESULT: run_config's result (a live run or an oracle
    re-run that ended; None for a session replay, whose end is its RunEnded
    event).  OUTPUT: the printed text, captured when no `out` was given
    (else None).  EVENTS: the index after the last event published (how
    many were published, but for start_events, which starts at START).  ERROR: the
    exception that ended it (a SessionError for a broken session file, a
    TraceMismatch for an oracle trace the re-run differs from), else None.
    ORACLE: an oracle replay's session.OracleReplay (None if stopped)."""
    source: str
    stopped: bool
    result: object
    output: object
    events: int
    error: object = None
    oracle: object = None


class _Run:
    """One worker's run: its thread and its gate's state."""

    def __init__(self, source, first=0):
        self.source = source
        self.thread = None
        self.stopping = False
        self.pending = None     # the ticket waiting for its acknowledgment
        self.index = first      # the next event's index


class _Gate:
    """The observer the run publishes to (on the worker thread)."""

    def __init__(self, controller, run):
        self.controller = controller
        self.run = run

    def on_event(self, event):
        self.controller._pass(self.run, event)


def _check_problem(problem, max_iterations):
    problem = list(problem)
    if len(problem) != 6 or not all(type(x) is int for x in problem):
        raise ValueError(f"a problem is a target and 5 bricks (6 integers), got {problem!r}")
    if max_iterations is not None and (type(max_iterations) is not int or max_iterations < 1):
        raise ValueError(f"max_iterations must be None or at least 1, got {max_iterations!r}")
    return problem


class RunController:
    """Runs Numbo on a worker thread, publishing each event to OBSERVERS and
    to DELIVER (see the module docstring), with DELAY seconds after each
    event while playing.  ON_ERROR gets an observer's (or deliver's)
    exception as an observe.ObserverError."""

    def __init__(self, observers=(), deliver=None, delay=0.0,
                 on_error=observe.report_to_stderr, on_state=None, on_finished=None):
        self._subject = observe.Subject(on_error)
        for observer in observers:
            self._subject.subscribe(observer)
        self.deliver = deliver
        self.on_state = on_state
        self.on_finished = on_finished
        self._cond = threading.Condition()
        self._mode = "paused"
        self._steps = 0
        self._delay = 0.0
        self.set_delay(delay)
        self._state = "idle"
        self._run = None
        self.outcome = None

    # -- observers -----------------------------------------------------------

    def subscribe(self, observer):
        """Add OBSERVER (last); returns it."""
        return self._subject.subscribe(observer)

    def unsubscribe(self, observer):
        self._subject.unsubscribe(observer)

    @property
    def errors(self):
        """The observers' errors so far (observe.ObserverError)."""
        return self._subject.errors

    # -- status --------------------------------------------------------------

    @property
    def mode(self):
        return self._mode

    @property
    def state(self):
        return self._state

    @property
    def delay(self):
        return self._delay

    @property
    def events_published(self):
        run = self._run
        return run.index if run is not None else 0

    def is_alive(self):
        """Whether the worker thread is running."""
        run = self._run
        return run is not None and run.thread is not None and run.thread.is_alive()

    def wait(self, timeout=None):
        """Wait for the worker to end; returns whether it has."""
        run = self._run
        if run is None or run.thread is None:
            return True
        if run.thread is not threading.current_thread():
            run.thread.join(timeout)
        return not run.thread.is_alive()

    # -- controls ------------------------------------------------------------

    def _set_mode(self, mode, steps=0):
        with self._cond:
            self._mode = mode
            self._steps = steps
            self._cond.notify_all()

    def play(self):
        """Run on, with the delay after each event."""
        self._set_mode("playing")

    def pause(self):
        """Pause before the next event (the event in hand is still
        acknowledged)."""
        self._set_mode("paused")

    def step(self):
        """Publish one more event, then pause.  Steps add up."""
        with self._cond:
            steps = self._steps + 1 if self._mode == "step" else 1
        self._set_mode("step", steps)

    def step_iteration(self):
        """Run on through the next iteration event, then pause."""
        self._set_mode("step-iteration")

    def run_to_end(self):
        """Run on with no delay (every event is still acknowledged)."""
        self._set_mode("run-to-end")

    def set_delay(self, seconds):
        """The pause after each event while playing (0: none)."""
        seconds = float(seconds)
        if not seconds >= 0:
            raise ValueError(f"the delay must be at least 0, got {seconds!r}")
        with self._cond:
            self._delay = seconds
            self._cond.notify_all()

    def acknowledge(self, ticket):
        """The consumer is done with Delivery TICKET.  Returns False if that
        is not the delivery the worker is waiting on (stale or repeated)."""
        with self._cond:
            run = self._run
            if (run is None or run.stopping or run.pending is None
                    or run.pending != ticket):
                return False
            run.pending = None
            self._cond.notify_all()
            return True

    def stop(self, wait=True, timeout=JOIN_TIMEOUT):
        """Stop the worker (it ends at its next event or wait), and unless
        called on the worker, wait up to TIMEOUT for it.  Returns whether it
        has ended."""
        with self._cond:
            run = self._run
            if run is None or run.thread is None:
                return True
            run.stopping = True
            self._cond.notify_all()
        if wait and run.thread is not threading.current_thread():
            run.thread.join(timeout)
        return not run.thread.is_alive()

    # -- starting ------------------------------------------------------------

    def start_run(self, problem, seed=1, max_iterations=500, *, paused=False, verbose=False,
                  trace=None, rng_events=False, out=None):
        """Start a live run of PROBLEM (target and 5 bricks) with SEED and
        MAX_ITERATIONS (harness.run_config's arguments).  PAUSED: wait
        before the first event.  OUT: the stream config prints to (by
        default captured into the outcome)."""
        problem = _check_problem(problem, max_iterations)

        def work(run, gate, captured):
            from numbo import harness
            result = harness.run_config(problem, seed=seed, max_iterations=max_iterations,
                                        verbose=verbose, trace=trace, rng_events=rng_events,
                                        out=captured if out is None else out,
                                        observers=[gate])
            return {"result": result}

        self._start("run", work, paused, out is None)

    def start_session(self, source, *, paused=False):
        """Start replaying the session file SOURCE (a path or a text
        stream)."""
        def work(run, gate, captured):
            from numbo import session
            with contextlib.closing(session.read_session(source)) as events:
                for event in events:
                    gate.on_event(event)
            return {}

        self._start("session", work, paused, False)

    def start_oracle(self, source, *, paused=False, out=None):
        """Start replaying the oracle trace SOURCE: a re-run of its problem
        and seed, checked against it as it goes (session.replay_oracle_trace).
        A divergence ends it with a TraceMismatch in outcome.error."""
        def work(run, gate, captured):
            from numbo import session
            replay = session.replay_oracle_trace(source, observers=[gate],
                                                 out=captured if out is None else out,
                                                 check=False)
            error = replay.divergence.error if replay.divergence is not None else None
            return {"result": replay.result, "oracle": replay, "error": error}

        self._start("oracle", work, paused, out is None)

    def start_events(self, events, start=0, *, paused=False):
        """Start publishing EVENTS[START:] (a run's events, e.g. a loaded
        session), as a replay does.  Each delivery's index is the event's
        index in EVENTS.  Nothing is run: the result and output are None."""
        if type(start) is not int or not 0 <= start <= len(events):
            raise ValueError(f"start must be an index into the {len(events)} events, "
                             f"got {start!r}")

        def work(run, gate, captured):
            for i in range(start, len(events)):
                gate.on_event(events[i])
            return {}

        self._start("events", work, paused, False, first=start)

    def _start(self, source, work, paused, capture, first=0):
        if not self.stop(timeout=JOIN_TIMEOUT):
            raise EngineBusy("the previous run did not stop")
        if not _engine.acquire(blocking=False):
            raise EngineBusy("another run controller is running the engine")
        try:
            run = _Run(source, first)
            with self._cond:
                self._mode = "paused" if paused else "playing"
                self._steps = 0
                self._run = run
                self._state = "paused" if paused else "running"
                self.outcome = None
            captured = io.StringIO() if capture else None
            run.thread = threading.Thread(target=self._work, name=f"numbo-{source}",
                                          args=(run, work, captured), daemon=True)
            run.thread.start()
        except BaseException:
            _engine.release()
            raise

    # -- the worker ----------------------------------------------------------

    def _notify_state(self, state):
        if self.on_state is not None:
            try:
                self.on_state(state)
            except Exception as exception:
                self._report(self.on_state, None, exception)

    def _set_state(self, state):
        """On the worker: the state is now STATE."""
        with self._cond:
            changed = self._state != state
            self._state = state
        if changed:
            self._notify_state(state)

    def _work(self, run, work, captured):
        stopped = False
        details = {}
        try:
            self._notify_state(self._state)
            try:
                details = work(run, _Gate(self, run), captured)
            except RunStopped:
                stopped = True
            except Exception as error:   # a broken file, or a bug
                details = {"error": error}
        finally:
            with self._cond:
                run.pending = None
            _engine.release()
        self.outcome = RunOutcome(source=run.source, stopped=stopped,
                                  result=details.get("result"),
                                  output=captured.getvalue() if captured is not None else None,
                                  events=run.index, error=details.get("error"),
                                  oracle=details.get("oracle"))
        self._set_state("stopped" if stopped else "finished")
        if self.on_finished is not None:
            try:
                self.on_finished(self.outcome)
            except Exception as exception:
                self._report(self.on_finished, None, exception)

    def _check_stop(self, run):
        if run.stopping:
            raise RunStopped()

    def _pass(self, run, event):
        """The gate, on the worker: wait, publish, deliver, wait for the
        acknowledgment, then the delay."""
        cond = self._cond
        while True:
            with cond:
                self._check_stop(run)
                if self._mode != "paused":
                    if self._mode == "step":
                        self._steps -= 1
                        if self._steps <= 0:
                            self._mode = "paused"
                    index = run.index
                    run.index += 1
                    break
            self._set_state("paused")
            with cond:
                while self._mode == "paused" and not run.stopping:
                    cond.wait()
        self._set_state("running")

        self._subject.publish(event)
        self._check_stop(run)
        if self.deliver is not None:
            ticket = next(_tickets)
            with cond:
                run.pending = ticket
            try:
                self.deliver(Delivery(ticket, index, event))
            except Exception as exception:
                with cond:
                    run.pending = None
                self._report(self.deliver, event, exception)
            with cond:
                while run.pending == ticket:
                    self._check_stop(run)
                    cond.wait()
                self._check_stop(run)

        with cond:
            if (self._mode == "step-iteration" and isinstance(event, observe.IterationBegan)):
                self._mode = "paused"
            start = None
            while self._mode == "playing" and self._delay > 0:
                now = time.monotonic()
                start = now if start is None else start
                remaining = start + self._delay - now
                if remaining <= 0:
                    break
                cond.wait(remaining)
                self._check_stop(run)

    def _report(self, callback, event, exception):
        """CALLBACK (deliver or on_state) raised EXCEPTION: report it as an
        observer's error is reported."""
        error = observe.ObserverError(callback, event, exception)
        self._subject.errors.append(error)
        if self._subject.on_error is not None:
            self._subject.on_error(error)

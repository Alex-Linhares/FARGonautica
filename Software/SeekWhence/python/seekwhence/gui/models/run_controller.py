"""The run controller: the engine on a worker thread behind a per-event gate, the Numbo
GUI's `numbo/models/run_controller.py` adopted whole (ANSWERS.md 7; ENGINE-API.md
section 3; G-005). Qt-free.

A `RunController` runs one thing at a time on its worker thread: a live session
(`start_session`: a `seekwhence.session.Session` with scripted or interactive inputs) or
a list of recorded events from a given index (`start_events`: the replay playback of a
loaded session file). Every event goes through the gate, an observer subscribed last to
the session's `Subject` (so the session's `Recorder` and `Trace` have already seen it):

  1. it waits while the controller is paused;
  2. it publishes the event to the controller's observers (on the worker thread, through
     a `Subject`: an observer's exception is reported to `on_error` and changes nothing,
     DECISION D-055);
  3. if there is a `deliver` callback, it calls `deliver(Delivery(ticket, index, event))`
     and blocks until `acknowledge(ticket)` (for the GUI: the redraw is done), so no event
     is published before the previous one is acknowledged;
  4. when playing, it waits the per-event delay (0: no wait).

The modes: "paused"; "playing" (with the delay); "step" (one event per `step()`, then
paused); "step-task" (on through the next `chosen` event, then paused: one task
[M86 p. 41]); "step-term" (on through the next `prompt` event, then paused: the program
asks for a term [M86 p. 165]; to the end if there is none); "run-to-end" (no delay and no
pause). A change of mode cuts a delay short. Events are never skipped or coalesced.

`stop()` wakes every wait, and the gate raises `RunStopped`, a `BaseException`: `Subject`
and the engine catch only `Exception`, so it unwinds the run and the worker ends with
`outcome.stopped`. Stopping changes nothing that a later run can see: every run has its
own `World`.

The gate draws no random numbers, touches no `World` state and prints nothing, so a run
under the controller has the events, printed text and `rng.draws` of the same run without
it (tested; the loop's gate checks it on every iteration). Only one controller's worker
runs at a time (`EngineBusy`), as in Numbo; starting a new run on a controller stops and
joins its old one first.

Callbacks: `on_session(session)` is called on the worker right after the `Session` is
built and before it runs (the tap attaches the world here, G-004); `on_state(state)` and
`on_finished(outcome)` are called on the worker thread. The states: "idle" (never
started), "running", "paused", "finished", "stopped".
"""

import dataclasses
import io
import itertools
import sys
import threading
import time
import traceback

from seekwhence.events import ObserverError, Subject

__all__ = ["MODES", "STATES", "Delivery", "RunOutcome", "RunStopped", "EngineBusy",
           "RunController", "report_to_stderr"]

MODES = ("paused", "playing", "step", "step-task", "step-term", "run-to-end")
STATES = ("idle", "running", "paused", "finished", "stopped")

# How long stop() and a restart wait for the worker to end, in seconds (Numbo GUI).
JOIN_TIMEOUT = 5.0

# Delivery tickets are unique across runs and controllers, so a late acknowledgment from
# a stopped run never acknowledges a new one (Numbo GUI).
_tickets = itertools.count(1)

# Held by the worker that is running the engine (Numbo GUI).
_engine = threading.Lock()


def report_to_stderr(error: ObserverError) -> None:
    """The default `on_error`: say which observer or callback failed, on which event if
    any, with the traceback (Numbo GUI, `observe.report_to_stderr`; the engine's own
    reporter, DECISION D-055, expects an event, and a callback's error has none)."""
    where = (f"on a {error.event.kind} event" if error.event is not None
             else "outside an event")
    sys.stderr.write(f"seekwhence: observer {error.observer!r} failed {where}:\n")
    traceback.print_exception(error.exception, file=sys.stderr)


class RunStopped(BaseException):
    """Raised by the gate on the worker to unwind a stopped run. Not an `Exception`, so
    that no error handler in the run catches it (Numbo GUI, `run_controller.py`)."""


class EngineBusy(RuntimeError):
    """Another worker is running the engine (Numbo GUI, `run_controller.py`)."""


@dataclasses.dataclass(frozen=True)
class Delivery:
    """An event handed to the consumer: `acknowledge(ticket)` releases the worker. `index`
    counts the run's events from 0 (for `start_events`, it is the event's index in its
    list) (Numbo GUI, `run_controller.py`)."""
    ticket: int
    index: int
    event: object


@dataclasses.dataclass(frozen=True)
class RunOutcome:
    """How the worker ended (Numbo GUI, `run_controller.py`; G-005). `source`: "session"
    or "events". `stopped`: `stop()` ended it. `result`: for a session that ended, a dict
    with the session's `outcome`, its `summary` line, the `draws` of its `Rng`, the
    `tasks` run, the `terms` entered and the final `hypothesis` form (or None); None when
    stopped or for an events replay. `output`: the printed text, captured when no `out`
    was given (else None). `events`: the index after the last event published. `error`:
    the exception that ended it, else None."""
    source: str
    stopped: bool
    result: object
    output: object
    events: int
    error: object = None


class _Run:
    """One worker's run: its thread and its gate's state (Numbo GUI)."""

    def __init__(self, source, first=0):
        self.source = source
        self.thread = None
        self.stopping = False
        self.pending = None     # the ticket waiting for its acknowledgment
        self.index = first      # the next event's index


class _Gate:
    """The observer the run publishes to, on the worker thread (Numbo GUI)."""

    def __init__(self, controller, run):
        self.controller = controller
        self.run = run

    def on_event(self, event):
        self.controller._pass(self.run, event)


class RunController:
    """Runs a session on a worker thread, publishing each event to `observers` and to
    `deliver` (see the module docstring), with `delay` seconds after each event while
    playing. `on_error` gets an observer's (or a callback's) exception as an
    `ObserverError` (Numbo GUI, `run_controller.py`; G-005)."""

    def __init__(self, observers=(), deliver=None, delay=0.0, on_error=report_to_stderr,
                 on_state=None, on_finished=None, on_session=None):
        self._subject = Subject(on_error)
        for observer in observers:
            self._subject.subscribe(observer)
        self.deliver = deliver
        self.on_state = on_state
        self.on_finished = on_finished
        self.on_session = on_session
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
        """Add an observer, last; returns it (DECISION D-055)."""
        return self._subject.subscribe(observer)

    def unsubscribe(self, observer) -> None:
        """Remove an observer (DECISION D-055)."""
        self._subject.unsubscribe(observer)

    @property
    def errors(self) -> list:
        """The observers' and callbacks' errors so far, as `ObserverError`s
        (DECISION D-055)."""
        return self._subject.errors

    # -- status --------------------------------------------------------------

    @property
    def mode(self) -> str:
        """One of `MODES` (Numbo GUI)."""
        return self._mode

    @property
    def state(self) -> str:
        """One of `STATES` (Numbo GUI)."""
        return self._state

    @property
    def delay(self) -> float:
        """The pause after each event while playing, in seconds (Numbo GUI)."""
        return self._delay

    @property
    def events_published(self) -> int:
        """How many events the current run has published (Numbo GUI)."""
        run = self._run
        return run.index if run is not None else 0

    def is_alive(self) -> bool:
        """Whether the worker thread is running (Numbo GUI)."""
        run = self._run
        return run is not None and run.thread is not None and run.thread.is_alive()

    def wait(self, timeout=None) -> bool:
        """Wait for the worker to end; returns whether it has (Numbo GUI)."""
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

    def play(self) -> None:
        """Run on, with the delay after each event (Numbo GUI)."""
        self._set_mode("playing")

    def pause(self) -> None:
        """Pause before the next event; the event in hand is still acknowledged (Numbo
        GUI)."""
        self._set_mode("paused")

    def step(self) -> None:
        """Publish one more event, then pause. Steps add up (Numbo GUI)."""
        with self._cond:
            steps = self._steps + 1 if self._mode == "step" else 1
        self._set_mode("step", steps)

    def step_task(self) -> None:
        """Run on through the next `chosen` event, then pause: the next task is about to
        run [M86 p. 41] (G-005; Numbo GUI's `step_iteration`)."""
        self._set_mode("step-task")

    def step_term(self) -> None:
        """Run on through the next `prompt` event, then pause: the program is asking for
        a term [M86 p. 165] (G-005; Numbo GUI's `step_iteration`)."""
        self._set_mode("step-term")

    def run_to_end(self) -> None:
        """Run on with no delay; every event is still acknowledged (Numbo GUI)."""
        self._set_mode("run-to-end")

    def set_delay(self, seconds) -> None:
        """The pause after each event while playing (0: none) (Numbo GUI)."""
        seconds = float(seconds)
        if not seconds >= 0:
            raise ValueError(f"the delay must be at least 0, got {seconds!r}")
        with self._cond:
            self._delay = seconds
            self._cond.notify_all()

    def acknowledge(self, ticket) -> bool:
        """The consumer is done with the delivery of this ticket. Returns False if that
        is not the delivery the worker is waiting on (stale or repeated) (Numbo GUI)."""
        with self._cond:
            run = self._run
            if (run is None or run.stopping or run.pending is None
                    or run.pending != ticket):
                return False
            run.pending = None
            self._cond.notify_all()
            return True

    def stop(self, wait=True, timeout=JOIN_TIMEOUT) -> bool:
        """Stop the worker (it ends at its next event or wait) and, unless called on the
        worker, wait up to `timeout` for it. Returns whether it has ended (Numbo GUI)."""
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

    def start_session(self, config, seed, lines=None, answers=(), *, answerer=None,
                      inp=None, paused=False, out=None):
        """Start a live session (`seekwhence.session.Session`): `config`, `seed`, the
        scripted console `lines` and `answers` (or `inp`, a stream, for a user at a
        console) and the `answerer` hook (DECISIONS D-064, D-069). `paused`: wait before
        the first event. `out`: the stream the transcript goes to (by default captured
        into the outcome) (Numbo GUI's `start_run`; G-005)."""
        from seekwhence.session import Session

        def work(run, gate, captured):
            session = Session(config, seed, out=captured if out is None else out,
                              lines=None if lines is None else list(lines),
                              answers=list(answers), inp=inp, observers=[gate],
                              answerer=answerer)
            if self.on_session is not None:
                self.on_session(session)
            session.run()
            world = session.world
            form = session.show_hypothesis()
            return {"result": {"outcome": session.outcome, "summary": session.summary(),
                               "draws": world.rng.draws, "tasks": world.clock,
                               "terms": len(world.terms),
                               "hypothesis": None if form == "nil" else form}}

        self._start("session", work, paused, out is None)

    def start_events(self, events, start=0, *, paused=False):
        """Start publishing `events[start:]` (a run's recorded events, e.g. a loaded
        session) as a replay does. Each delivery's index is the event's index in the
        list. Nothing is run: the result and output are None (Numbo GUI)."""
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
            run.thread = threading.Thread(target=self._work, name=f"seekwhence-{source}",
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
        """On the worker: the state is now `state`."""
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
            except Exception as error:   # a broken input, or a bug
                details = {"error": error}
        finally:
            with self._cond:
                run.pending = None
            _engine.release()
        self.outcome = RunOutcome(source=run.source, stopped=stopped,
                                  result=details.get("result"),
                                  output=captured.getvalue() if captured is not None else None,
                                  events=run.index, error=details.get("error"))
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
        """The gate, on the worker: wait, publish, deliver, wait for the acknowledgment,
        then the delay (Numbo GUI)."""
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
            if ((self._mode == "step-task" and event.kind == "chosen")
                    or (self._mode == "step-term" and event.kind == "prompt")):
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
        """A callback (deliver, on_state, on_finished) raised: report it as an observer's
        error is reported (DECISION D-055)."""
        error = ObserverError(callback, event, exception)
        self._subject.errors.append(error)
        if self._subject.on_error is not None:
            self._subject.on_error(error)

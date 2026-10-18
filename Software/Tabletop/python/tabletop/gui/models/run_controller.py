"""The run controller: the engine on a worker thread behind a per-event gate, the Numbo
GUI's `numbo/models/run_controller.py` adopted whole through the Seek-Whence GUI's
(ANSWERS.md 5; `docs/engine-api.md`, "For loop0006"; G-008). Qt-free.

A `RunController` runs one thing at a time on its worker thread: a live run
(`start_run`: a `World(problem, config, seed, observers=[gate])` advanced with
`World.step()` until it has finished, the engine's own loop of `run_problem`) or a list
of recorded events from a given index (`start_events`: the replay playback of a loaded
session file). Every event goes through the gate, the one observer the `World` is given:

  1. it waits while the controller is paused;
  2. it publishes the event to the controller's observers (on the worker thread, through
     a `Subject`: an observer's exception is reported to `on_error` and changes nothing,
     as the engine's own publisher does, D-065);
  3. if there is a `deliver` callback, it calls `deliver(Delivery(ticket, index, event,
     generation))` and blocks until `acknowledge(ticket)` (for the GUI: the redraw is
     done), so no event is published before the previous one is acknowledged;
  4. when playing, it waits the per-event delay (0: no wait).

The modes: "paused"; "playing" (with the delay); "step" (one event per `step()`, then
paused: one visible change); "step-codelet" (on to the end of the current `World.step()`,
then paused before the first event of the next; from the very start, to the end of the
start, before the first codelet); "run-to-end" (no delay and no pause). A change of mode
cuts a delay short. Events are never skipped or coalesced.

`stop()` wakes every wait, and the gate raises `RunStopped`, a `BaseException`: the
engine's publisher and `Subject` catch only `Exception`, so it unwinds the run and the
worker ends with `outcome.stopped`. Stopping changes nothing that a later run can see:
every run has its own `World`. Each start has a new `generation`, so that a consumer can
drop a stopped run's late deliveries (Numbo GUI, `main_window.py`).

The gate draws no random numbers, touches no `World` state and prints nothing, so a run
under the controller has the events, draws and report of the same run without it (tested;
the loop's gate checks it on every iteration). Only one controller's worker runs at a time
(`EngineBusy`), as in Numbo; starting a new run on a controller stops and joins its old
one first.

Callbacks, all called on the worker thread: `on_world(world)` right after the `World` is
built and before its first step (the tap takes the start's snapshot there, G-006);
`on_step(world)` right after each `World.step()` returns (the tap's snapshots, G-007);
`on_state(state)` and `on_finished(outcome)`. The states: "idle" (never started),
"running", "paused", "finished", "stopped".
"""

import dataclasses
import itertools
import sys
import threading
import time
import traceback

from tabletop.config import Config
from tabletop.problems import Problem, load_problem
from tabletop.run import result_of
from tabletop.world import World

__all__ = ["MODES", "STATES", "Delivery", "RunOutcome", "RunStopped", "EngineBusy",
           "ObserverError", "Subject", "RunController", "report_to_stderr", "engine_lock"]

MODES = ("paused", "playing", "step", "step-codelet", "run-to-end")
STATES = ("idle", "running", "paused", "finished", "stopped")

# How long stop() and a restart wait for the worker to end, in seconds (Numbo GUI).
JOIN_TIMEOUT = 5.0

# Delivery tickets are unique across runs and controllers, so a late acknowledgment from
# a stopped run never acknowledges a new one (Numbo GUI).
_tickets = itertools.count(1)
_generations = itertools.count(1)

# Held by the worker that is running the engine (Numbo GUI): a run controller's, or
# the results pane's batch worker (G-052), so that a batch never runs during a live run
# and a live run never starts during a batch.
_engine = threading.Lock()


def engine_lock():
    """The lock held by whichever worker is running the engine: the run controller's
    worker for a live run, the batch worker for a batch (PANES.md P8; G-052). Taken
    without blocking; a holder refuses with `EngineBusy`."""
    return _engine


@dataclasses.dataclass(frozen=True)
class ObserverError:
    """`observer` raised `exception` on `event` (None for a callback's error) (Numbo GUI,
    `observe.py`)."""
    observer: object
    event: object
    exception: BaseException


def report_to_stderr(error: ObserverError) -> None:
    """The default `on_error`: say which observer or callback failed, on which event if
    any, with the traceback (Numbo GUI, `observe.report_to_stderr`)."""
    where = (f"on a {error.event.name} event" if error.event is not None
             else "outside an event")
    sys.stderr.write(f"tabletop-gui: observer {error.observer!r} failed {where}:\n")
    traceback.print_exception(error.exception, file=sys.stderr)


class Subject:
    """Publishes events to its observers in subscription order; an observer's `Exception`
    is recorded in `errors`, passed to `on_error` and stops nothing (Numbo GUI,
    `observe.Subject`; the engine's publisher keeps the same rule, D-065)."""

    def __init__(self, on_error=report_to_stderr):
        self._observers = []
        self.on_error = on_error
        self.errors = []

    @property
    def observers(self) -> tuple:
        """The observers, in subscription order (Numbo GUI, `observe.Subject`)."""
        return tuple(self._observers)

    def subscribe(self, observer):
        """Add an observer, last; returns it (Numbo GUI, `observe.Subject`)."""
        if not callable(getattr(observer, "on_event", None)):
            raise TypeError(f"{observer!r} has no on_event method")
        self._observers.append(observer)
        return observer

    def unsubscribe(self, observer) -> None:
        """Remove an observer (a ValueError if it is not subscribed) (Numbo GUI,
        `observe.Subject`)."""
        self._observers.remove(observer)

    def publish(self, event) -> None:
        """Give the event to every observer subscribed now (Numbo GUI,
        `observe.Subject`)."""
        for observer in tuple(self._observers):
            try:
                observer.on_event(event)
            except Exception as exception:
                self.report(observer, event, exception)

    def report(self, observer, event, exception) -> None:
        """Record an observer's (or a callback's) error and pass it to `on_error`
        (Numbo GUI, `observe.Subject`)."""
        error = ObserverError(observer, event, exception)
        self.errors.append(error)
        if self.on_error is not None:
            self.on_error(error)

    on_event = publish


class RunStopped(BaseException):
    """Raised by the gate on the worker to unwind a stopped run. Not an `Exception`, so
    that no error handler in the run catches it (Numbo GUI, `run_controller.py`)."""


class EngineBusy(RuntimeError):
    """Another worker is running the engine (Numbo GUI, `run_controller.py`)."""


@dataclasses.dataclass(frozen=True)
class Delivery:
    """An event handed to the consumer: `acknowledge(ticket)` releases the worker. `index`
    counts the run's events from 0 (for `start_events`, it is the event's index in its
    list); `generation` is the start's number, so that a stopped run's late deliveries
    can be dropped (Numbo GUI, `run_controller.py`; G-008)."""
    ticket: int
    index: int
    event: object
    generation: int = 0


@dataclasses.dataclass(frozen=True)
class RunOutcome:
    """How the worker ended (Numbo GUI, `run_controller.py`; G-008). `source`: "run" or
    "events". `stopped`: `stop()` ended it. `result`: the engine's `Result`
    (`tabletop.run.result_of`, plain data) of a run that ended; None when stopped or for
    an events replay. `events`: the index after the last event published. `generation`:
    the start's number. `error`: the exception that ended it, else None."""
    source: str
    stopped: bool
    result: object
    events: int
    generation: int
    error: object = None


class _Run:
    """One worker's run: its thread and its gate's state (Numbo GUI)."""

    def __init__(self, source, generation, first=0):
        self.source = source
        self.generation = generation
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


def check_inputs(problem, config, seed) -> Problem:
    """The run's inputs checked before the worker starts (Numbo GUI,
    `run_controller.py`'s `_check_problem`): `problem` is a `Problem` or the name of one
    in the library (loaded here, so that a bad name raises at once); `config` is a
    `Config`; `seed` an integer. Returns the `Problem`."""
    if isinstance(problem, str):
        problem = load_problem(problem)
    if not isinstance(problem, Problem):
        raise TypeError(f"a problem is a Problem or a problem name, got {problem!r}")
    if not isinstance(config, Config):
        raise TypeError(f"config must be a Config, got {config!r}")
    if type(seed) is not int:
        raise TypeError(f"seed must be an integer, got {seed!r}")
    return problem


class RunController:
    """Runs a problem on a worker thread, publishing each event to `observers` and to
    `deliver` (see the module docstring), with `delay` seconds after each event while
    playing. `on_error` gets an observer's (or a callback's) exception as an
    `ObserverError` (Numbo GUI, `run_controller.py`; G-008)."""

    def __init__(self, observers=(), deliver=None, delay=0.0, on_error=report_to_stderr,
                 on_state=None, on_finished=None, on_world=None, on_step=None):
        self._subject = Subject(on_error)
        for observer in observers:
            self._subject.subscribe(observer)
        self.deliver = deliver
        self.on_state = on_state
        self.on_finished = on_finished
        self.on_world = on_world
        self.on_step = on_step
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
        """Add an observer, last; returns it (Numbo GUI)."""
        return self._subject.subscribe(observer)

    def unsubscribe(self, observer) -> None:
        """Remove an observer (Numbo GUI)."""
        self._subject.unsubscribe(observer)

    @property
    def errors(self) -> list:
        """The observers' and callbacks' errors so far, as `ObserverError`s
        (Numbo GUI)."""
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
    def generation(self) -> int:
        """The number of the current (or last) start, 0 before any (G-008)."""
        run = self._run
        return run.generation if run is not None else 0

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
        """Pause before the next event; the event in hand is still acknowledged
        (Numbo GUI)."""
        self._set_mode("paused")

    def step(self) -> None:
        """Publish one more event, then pause: one visible change (ANSWERS.md 5). Steps
        add up (Numbo GUI)."""
        with self._cond:
            steps = self._steps + 1 if self._mode == "step" else 1
        self._set_mode("step", steps)

    def step_codelet(self) -> None:
        """Run on to the end of the current `World.step()`, then pause before the next
        codelet's first event; from the start, to the end of the start (ANSWERS.md 5;
        G-008; Numbo GUI's `step_iteration`)."""
        self._set_mode("step-codelet")

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

    def start_run(self, problem, config, seed, *, paused=False):
        """Start a live run of `problem` (a `Problem` or a problem name) under `config`
        (a `Config`) with `seed`: the worker builds `World(problem, config, seed,
        observers=[gate])` and calls `World.step()` until the run has finished, the
        engine's own loop (`run_problem`, D-066). `paused`: wait before the first event
        (Numbo GUI's `start_run`; G-008)."""
        problem = check_inputs(problem, config, seed)

        def work(run, gate):
            world = World(problem, config, seed, observers=[gate])
            self._call(self.on_world, world)
            self._end_of_step(run)
            while not world.finished:
                self._check_stop(run)
                world.step()
                self._call(self.on_step, world)
                self._end_of_step(run)
            return {"result": result_of(world)}

        self._start("run", work, paused)

    def start_events(self, events, start=0, *, paused=False):
        """Start publishing `events[start:]` (a run's recorded events, e.g. a loaded
        session) as a replay does. Each delivery's index is the event's index in the
        list. Nothing is run: the result is None (Numbo GUI)."""
        if type(start) is not int or not 0 <= start <= len(events):
            raise ValueError(f"start must be an index into the {len(events)} events, "
                             f"got {start!r}")

        def work(run, gate):
            for i in range(start, len(events)):
                gate.on_event(events[i])
            return {}

        self._start("events", work, paused, first=start)

    def _start(self, source, work, paused, first=0):
        if not self.stop(timeout=JOIN_TIMEOUT):
            raise EngineBusy("the previous run did not stop")
        if not _engine.acquire(blocking=False):
            raise EngineBusy("the engine is busy: another run or a batch is running")
        try:
            run = _Run(source, next(_generations), first)
            with self._cond:
                self._mode = "paused" if paused else "playing"
                self._steps = 0
                self._run = run
                self._state = "paused" if paused else "running"
                self.outcome = None
            run.thread = threading.Thread(target=self._work, name=f"tabletop-{source}",
                                          args=(run, work), daemon=True)
            run.thread.start()
        except BaseException:
            _engine.release()
            raise

    # -- the worker ----------------------------------------------------------

    def _call(self, callback, argument):
        """A callback on the worker: its error is reported and changes nothing."""
        if callback is not None:
            try:
                callback(argument)
            except Exception as exception:
                self._subject.report(callback, None, exception)

    def _set_state(self, state):
        """On the worker: the state is now `state`."""
        with self._cond:
            changed = self._state != state
            self._state = state
        if changed:
            self._call(self.on_state, state)

    def _work(self, run, work):
        stopped = False
        details = {}
        try:
            self._call(self.on_state, self._state)
            try:
                details = work(run, _Gate(self, run))
            except RunStopped:
                stopped = True
            except Exception as error:   # a broken input, or a bug
                details = {"error": error}
        finally:
            with self._cond:
                run.pending = None
            _engine.release()
        self.outcome = RunOutcome(source=run.source, stopped=stopped,
                                  result=details.get("result"), events=run.index,
                                  generation=run.generation, error=details.get("error"))
        self._set_state("stopped" if stopped else "finished")
        self._call(self.on_finished, self.outcome)

    def _check_stop(self, run):
        if run.stopping:
            raise RunStopped()

    def _end_of_step(self, run):
        """On the worker, between two steps (and after the start): a step codelet ends
        here, so the gate pauses before the next codelet's first event (G-008)."""
        with self._cond:
            if self._mode == "step-codelet":
                self._mode = "paused"

    def _wait_to_pass(self, run):
        """The gate's first wait: block while paused, then take the event's index."""
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
                    return index
            self._set_state("paused")
            with cond:
                while self._mode == "paused" and not run.stopping:
                    cond.wait()

    def _pass(self, run, event):
        """The gate, on the worker: wait, publish, deliver, wait for the acknowledgment,
        then the delay (Numbo GUI)."""
        cond = self._cond
        index = self._wait_to_pass(run)
        self._set_state("running")

        self._subject.publish(event)
        self._check_stop(run)
        if self.deliver is not None:
            ticket = next(_tickets)
            with cond:
                run.pending = ticket
            try:
                self.deliver(Delivery(ticket, index, event, run.generation))
            except Exception as exception:
                with cond:
                    run.pending = None
                self._subject.report(self.deliver, event, exception)
            with cond:
                while run.pending == ticket:
                    self._check_stop(run)
                    cond.wait()
                self._check_stop(run)

        with cond:
            start = None
            while self._mode == "playing" and self._delay > 0:
                now = time.monotonic()
                start = now if start is None else start
                remaining = start + self._delay - now
                if remaining <= 0:
                    break
                cond.wait(remaining)
                self._check_stop(run)

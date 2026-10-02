"""Run config headless, with a seed and an iteration cap.

Port of lisp/src/harness.lisp (not 1987 source): iteration-cap-check,
install-iteration-cap and run-config.  In Python every run is an oracle-mode
run, so run_config also does what lisp/src/oracle.lisp's oracle-run-config adds
around it: it seeds the shared RNG, writes the JSON-lines trace (trace.py)
when asked to, and turns a Lisp error into the outcome "error".

config's main loop ends only when the problem is solved or the coderack is
empty twice.  As in the Lisp, the cap stops it without editing it: mod and
cr-empty-coderack, which the loop body calls first in every iteration right
after (setq *iteration* y), are encapsulated, and the first call that sees
*iteration* = N throws out of config, so iterations 0 .. N-1 have run.

`encapsulate` is the Python sb-int:encapsulate: it replaces a module's
function by a hook that gets the original and the arguments.  The ported
modules call each other through module attributes (franz.mod,
codelets.disconnect, ...), so every call sees the hook.  Hooks call straight
through when no run is under way.

`load_world` is a fresh World as a fresh SBCL process has it after
lisp/src/load.lisp: the load-time forms of pnet-def.lisp (init-pnet, *pnet*),
init.lisp (its DEFVARs) and start.lisp ((defvar *iteration* 0)).
"""

import functools
import os
import sys

from numbo import coderack, franz, init, pnet_def, start
from numbo.franz import intern as _S
from numbo.world import OutputStream, World

_ITERATION = _S("*ITERATION*")
_PNET = _S("*PNET*")
_PROBLEM_SOLVED = _S("*PROBLEM-SOLVED*")
_VERBOSE = _S("%VERBOSE%")

__all__ = ["OutputStream", "encapsulate", "iteration_cap_check", "install_iteration_cap",
           "load_world", "run_config"]

# *iteration-cap*: when not None, config is stopped at the start of
# iteration *iteration-cap*.  _cap_world is the World whose *iteration* it
# reads (the Lisp reads the one global *iteration*).
_iteration_cap = None
_cap_world = None


class IterationCap(BaseException):
    """(throw 'iteration-cap *iteration*).  Not an Exception: no handler of
    errors catches it, as no Lisp error handler catches a throw."""


def encapsulate(module, name, tag, hook):
    """sb-int:encapsulate: MODULE.NAME becomes a function calling
    HOOK(original, *args).  Once per TAG; returns whether it was installed."""
    original = getattr(module, name)
    tags = getattr(original, "encapsulations", ())
    if tag in tags:
        return False

    def encapsulation(*args):
        return hook(original, *args)

    functools.update_wrapper(encapsulation, original)
    encapsulation.encapsulations = tags + (tag,)
    setattr(module, name, encapsulation)
    return True


def load_world():
    """A World as lisp/src/load.lisp leaves a fresh SBCL process (oracle mode):
    the 91 pnodes and *pnet* (pnet-def.lisp), init.lisp's DEFVARs, and
    *iteration* 0 (start.lisp)."""
    world = World()
    pnet_def.init_pnet(world)
    world[_PNET] = pnet_def.pnet_list(world)
    init.load_init(world)
    start.load_start(world)
    return world


def iteration_cap_check():
    """harness.lisp: iteration-cap-check."""
    if _iteration_cap is not None and _cap_world[_ITERATION] >= _iteration_cap:
        raise IterationCap(_cap_world[_ITERATION])


def _iteration_cap_hook(fn, *args):
    iteration_cap_check()
    return fn(*args)


def install_iteration_cap():
    """harness.lisp: install-iteration-cap.  Encapsulate mod and
    cr-empty-coderack with the cap check."""
    encapsulate(franz, "mod", "iteration-cap", _iteration_cap_hook)
    encapsulate(coderack, "cr_empty_coderack", "iteration-cap", _iteration_cap_hook)


def _run_config(world, problem, seed, max_iterations, verbose):
    """harness.lisp's run-config proper, on WORLD."""
    global _iteration_cap, _cap_world
    install_iteration_cap()
    # (setq *random-state* (sb-ext:seed-random-state seed)) seeds CL's RANDOM,
    # which oracle mode does not use: the shared RNG was seeded by the caller.
    world[_ITERATION] = 0
    _cap_world = world
    _iteration_cap = None
    init.init_chiffre(world)
    if verbose:
        world[_VERBOSE] = True
    capped = True
    _iteration_cap = max_iterations
    try:
        start.config(world, *problem)
        capped = False
    except IterationCap:
        pass
    finally:
        _iteration_cap = None
    if capped:
        outcome = "capped"
    elif franz._eql(world[_PROBLEM_SOLVED], 1):
        outcome = "solved"
    else:
        outcome = "gave-up"
    return {"outcome": outcome,
            "iterations": max_iterations if capped else 1 + world[_ITERATION],
            "seed": seed,
            "problem-solved": world[_PROBLEM_SOLVED],
            "error": None}


def error_message(condition):
    """(princ-to-string condition) for the error that ended a run."""
    return str(condition)


def run_config(problem, seed=1, max_iterations=500, verbose=False, trace=None,
               rng_events=False, out=None, observers=()):
    """harness.lisp: run-config, in oracle mode (oracle.lisp:
    oracle-run-config).  Run (init-chiffre) then (config . PROBLEM) in a
    fresh World, with the shared RNG seeded with SEED, stopping after
    MAX-ITERATIONS main-loop iterations (None: no cap; it must be at least 1,
    since the set-up phase already calls mod).

    TRACE: a text stream or a file name for the JSON-lines trace (trace.py),
    or None.  RNG_EVENTS adds the RNG draws to it.  OUT: the stream config
    prints to (sys.stdout by default).  VERBOSE sets %verbose% after
    init-chiffre.  OBSERVERS (observe.Observer) get the run's typed events
    (events.py), after the trace writer; they change nothing in the run.

    Returns {"outcome": "solved" | "gave-up" | "capped" | "error",
    "iterations", "seed", "problem-solved", "error": the message or None}."""
    from numbo import events, observe
    from numbo import trace as trace_module

    if max_iterations is not None and max_iterations < 1:
        raise ValueError(f"max_iterations must be None or at least 1, got {max_iterations!r}")
    events.install()
    world = load_world()
    world.out = OutputStream(sys.stdout if out is None else out)
    opened = isinstance(trace, (str, os.PathLike))
    stream = open(trace, "w", encoding="utf-8") if opened else trace
    try:
        subject = None
        if stream is not None or observers:
            subject = observe.Subject()
            if stream is not None:
                subject.subscribe(trace_module.OracleTraceWriter(stream, rng_events))
            for observer in observers:
                subject.subscribe(observer)
        with events.publishing(world, subject) as p:
            world.rng.seed(seed)
            if p is not None:
                p.run_started(problem, seed, max_iterations)
            try:
                result = _run_config(world, problem, seed, max_iterations, verbose)
            except Exception as condition:  # a Lisp error
                result = {"outcome": "error",
                          # As oracle-run-config does: the count comes from
                          # the trace's last iteration, so it is 0 without
                          # a trace (an oracle-hook quirk, not a 1987 one;
                          # observers alone do not change it).
                          "iterations": (p.last_iteration + 1
                                         if stream is not None and p.last_iteration is not None
                                         else 0),
                          "seed": seed,
                          "problem-solved": world.values.get(_PROBLEM_SOLVED),
                          "error": error_message(condition)}
            if p is not None:
                p.run_ended(result)
        return result
    finally:
        if opened:
            stream.close()

"""break/go scenarios for test_run.py, each run in a fresh Python process (test
helper, loop0002 item 11).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The counterpart of racket/tests/run-test.rkt: the engine's own break (not the
headless driver's), whose (reset) returns to the caller of `run.toplevel`, and
go, which resumes the run where it stopped.  `python3 run_scenarios.py NAME`
prints the scenario's result as JSON.
"""
from __future__ import annotations

import io
import json
import os
import sys
import types
from contextlib import redirect_stdout

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [os.path.dirname(HERE)]

import metacat as _metacat  # noqa: E402
from metacat import chez, engine, headless, run, setup  # noqa: E402
from metacat.objects import Lambda  # noqa: E402

MODES = []


def fresh_engine():
    """A loaded engine with headless windows and a control panel that accepts the
    messages break and go send; break is run.ss's own."""
    engine.load()
    headless.install_headless_windows()
    eeg = types.ModuleType("metacat.eeg_graphics")
    eeg.g_EEG = headless.make_null_window("EEG", ("initialize",))
    sys.modules["metacat.eeg_graphics"] = eeg
    _metacat.eeg_graphics = eeg

    def control_panel(self, msg, *args):
        if msg == "set-verbose-step-mode":
            setup.p_verbose = args[0]
            return "done"
        if msg in ("switch-to-input-mode", "switch-to-run-mode"):
            MODES.append(msg)
            return "done"
        raise chez.SchemeError("control-panel", "unexpected message ~s", msg)
    setup.g_control_panel = Lambda(control_panel)


def output(thunk):
    out = io.StringIO()
    with redirect_stdout(out):
        thunk()
    return out.getvalue()


def until_reset(thunk):
    """thunk run as a REPL command, until it returns or (reset); its output."""
    return output(lambda: run.toplevel(thunk))


def state():
    return [chez.random_seed(), setup.g_codelet_count, chez.number_to_string(setup.g_temperature)]


def stopped_three_times():
    fresh_engine()
    output(lambda: run.init_mcat("abc", "abd", "xyz", False, 7))
    run.g_break_time = 150
    out1 = until_reset(run.run_mcat)
    r = {"out1": out1, "count1": setup.g_codelet_count, "running1": run.g_running_p,
         "breakpoint1": isinstance(run.g_breakpoint_continuation, run.Breakpoint)}
    # *running?* as the resumed run sees it (update-everything every 15 codelets)
    seen = []
    original_update = run.update_everything

    def update_everything():
        seen.append(run.g_running_p)
        return original_update()
    run.update_everything = update_everything
    run.g_break_time = 300
    r["out2"] = until_reset(run.go)
    run.update_everything = original_update
    r["running_during_go"] = sorted(set(seen))
    r["count2"] = setup.g_codelet_count
    run.g_break_time = 450
    r["out3"] = until_reset(run.go)
    r["count3"] = setup.g_codelet_count
    r["modes"] = MODES
    r["state"] = state()
    return r


def straight():
    fresh_engine()
    output(lambda: run.init_mcat("abc", "abd", "xyz", False, 7))
    run.g_break_time = 450
    out = until_reset(run.run_mcat)
    return {"out": out, "state": state()}


def step_mode():
    fresh_engine()
    output(lambda: run.init_mcat("abc", "abd", "xyz", False, 7))
    ss_out = output(lambda: run.ss(40))
    r = {"ss": ss_out, "step_mode": run.g_step_mode_p}
    r["out1"] = until_reset(run.run_mcat)
    r["count1"] = setup.g_codelet_count
    r["out2"] = until_reset(run.go)
    r["count2"] = setup.g_codelet_count
    r["ss_off"] = output(lambda: run.ss(0))
    return r


def go_without_break():
    fresh_engine()
    return {"out": output(run.go)}


def break_in_a_codelet():
    """abc abd xyz seed 3: answer-finder reports yyz during codelet 2428 and calls
    suspend, so break stops the run inside the codelet; go finishes the codelet and
    goes on, through the later answers' breaks, to the breakpoint at 3000."""
    fresh_engine()
    output(lambda: run.init_mcat("abc", "abd", "xyz", False, 3))
    out1 = until_reset(run.run_mcat)
    r = {"out1": out1, "count1": setup.g_codelet_count, "stops": []}
    run.g_break_time = 3000
    while setup.g_codelet_count < 3000:
        out = until_reset(run.go)
        r["stops"].append([setup.g_codelet_count, out.splitlines()[-2:]])
    r["state"] = state()
    r["modes"] = MODES
    return r


def keep_going_to_3000():
    """The same problem, headless with --keep-going (the oracle's break returns at
    once, as go makes it return), to the cap at 3000."""
    out = io.StringIO()
    with redirect_stdout(out):
        reason, answers = headless.run_problem(["abc", "abd", "xyz"], 3, 3000, True)
    return {"reason": reason, "answers": answers, "state": state()}


def new_run_after_a_break():
    """A break left parked, then a new run (init-mcat drops the old breakpoint):
    the new run is the same as one in a fresh engine, apart from the Memory."""
    fresh_engine()
    output(lambda: run.init_mcat("abc", "abd", "xyz", False, 7))
    run.g_break_time = 100
    until_reset(run.run_mcat)
    old = run.g_breakpoint_continuation
    output(lambda: run.init_mcat("abc", "abd", "xyz", False, 7))
    r = {"dropped": run.g_breakpoint_continuation is False and old.abandoned}
    run.g_break_time = 450
    r["out"] = until_reset(run.run_mcat)
    r["state"] = state()
    return r


SCENARIOS = {f.__name__: f for f in [stopped_three_times, straight, step_mode,
                                     go_without_break, break_in_a_codelet,
                                     keep_going_to_3000, new_run_after_a_break]}

if __name__ == "__main__":
    print(json.dumps(SCENARIOS[sys.argv[1]]()))

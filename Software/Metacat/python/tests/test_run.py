"""run.ss (metacat/run.py): break, go and step mode, and the file's definitions
(loop0002 item 11).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The counterpart of racket/tests/run-test.rkt.  Each scenario of run_scenarios.py
runs in a fresh Python process with the engine's own break: its (reset) returns
to the caller of run.toplevel (the REPL), and go resumes the run where it stopped.
A run stopped and resumed must be the same run as one never stopped: same codelet
count, generator state and temperature.  The 109 goldens (test_golden.py) run
run.py through the headless driver.
"""
from __future__ import annotations

import ast
import inspect
import json
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import pytest

from metacat import headless, run, trace_writer
from metacat.names import scheme_to_python

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
SCENARIOS = ["stopped_three_times", "straight", "step_mode", "go_without_break",
             "break_in_a_codelet", "keep_going_to_3000", "new_run_after_a_break"]


def _scenario(name):
    proc = subprocess.run([sys.executable, str(HERE / "run_scenarios.py"), name],
                          capture_output=True, text=True, timeout=600)
    assert proc.returncode == 0, proc.stderr
    return json.loads(proc.stdout)


@pytest.fixture(scope="module")
def results():
    with ThreadPoolExecutor(len(SCENARIOS)) as pool:
        return dict(zip(SCENARIOS, pool.map(_scenario, SCENARIOS)))


def test_a_run_stopped_three_times(results):
    r = results["stopped_three_times"]
    assert r["out1"] == "Codelets run: 150\nstopped\n"
    assert r["count1"] == 150
    assert r["running1"] is False
    assert r["breakpoint1"] is True
    assert (r["out2"], r["count2"]) == ("Codelets run: 300\nstopped\n", 300)
    assert r["running_during_go"] == [True]
    assert (r["out3"], r["count3"]) == ("Codelets run: 450\nstopped\n", 450)
    assert r["modes"] == ["switch-to-input-mode", "switch-to-run-mode", "switch-to-input-mode",
                          "switch-to-run-mode", "switch-to-input-mode"]


def test_a_stopped_and_resumed_run_is_the_same_run(results):
    assert results["straight"]["out"] == "Codelets run: 450\nstopped\n"
    assert results["straight"]["state"] == results["stopped_three_times"]["state"]


def test_step_mode(results):
    r = results["step_mode"]
    assert r["ss"] == "step mode on, step size 40\n"
    assert r["step_mode"] is True
    assert (r["out1"], r["count1"]) == ("Codelets run: 40\nstopped\n", 40)
    assert (r["out2"], r["count2"]) == ("Codelets run: 80\nstopped\n", 80)
    assert r["ss_off"] == "step mode off\n"


def test_go_without_a_break(results):
    assert results["go_without_break"]["out"] == "No previous break.\n"


def test_a_break_inside_a_codelet_resumes_that_codelet(results):
    """suspend breaks during codelet 2428 (the codelet count is still 2427, the
    oracle's "codelet 2427" on its Answer line); go finishes that codelet.  Resumed
    through every break to 3000, the run is the oracle's --keep-going run (whose
    break returns at once, as go makes it return)."""
    r = results["break_in_a_codelet"]
    assert r["count1"] == 2427
    assert r["out1"].endswith('Comment: The answer "yyz" occurs to me.  I think this answer '
                              "is very good!\nType (go) or click on the Workspace to "
                              "continue...\nstopped\n")
    assert [s[0] for s in r["stops"]] == [2429, 2659, 3000]
    assert r["stops"][-1][1] == ["Codelets run: 3000", "stopped"]
    k = results["keep_going_to_3000"]
    assert (k["reason"], k["answers"]) == ("cap", ["yyz", "wyz", "xyd"])
    assert r["state"] == k["state"]


def test_a_new_run_drops_the_parked_break(results):
    r = results["new_run_after_a_break"]
    assert r["dropped"] is True
    assert r["out"] == "Codelets run: 450\nstopped\n"
    assert r["state"] == results["straight"]["state"]


def test_a_breakpoint_resumes_once():
    point = run.Breakpoint(None)
    with pytest.raises(Exception, match="can no longer be resumed"):
        point("ignore")


def test_break_outside_toplevel_resets():
    """Outside run.toplevel (a script), break's (reset) raises Reset, as Chez's
    reset ends a scheme --script run."""
    from metacat import objects, setup
    from metacat.objects import Lambda
    saved = setup.g_control_panel, run.g_breakpoint_continuation, run.g_running_p
    setup.g_control_panel = Lambda(lambda self, msg, *args: "done")
    try:
        with pytest.raises(objects.Reset):
            run.quiet_break()
        assert run.g_running_p is False
    finally:
        setup.g_control_panel, run.g_breakpoint_continuation, run.g_running_p = saved


# run.ss's definitions -----------------------------------------------------------------------

def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


def test_every_definition_has_its_python_name():
    names = defines(ROOT / "chez_scheme" / "original" / "run.ss")
    assert len(names) == 33
    missing = [n for n in names if not hasattr(run, scheme_to_python(n))]
    assert missing == []


def test_docstrings_name_their_origin():
    for fname, fn in vars(run).items():
        if (inspect.isfunction(fn) and fn.__module__ == run.__name__
                and not fname.startswith("_") and fname != "toplevel"):
            assert fn.__doc__ and fn.__doc__.split(":")[0] in ("run.ss", "Chez"), fname


@pytest.mark.parametrize("module", [run, headless, trace_writer])
def test_engine_modules_import_no_gui(module):
    tree = ast.parse(inspect.getsource(module))
    found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found)


def test_run_is_loaded_in_metacat_ss_order():
    from metacat import engine
    assert run in engine.translated_modules()
    assert os.path.exists(ROOT / "python" / "metacat" / "run.py")

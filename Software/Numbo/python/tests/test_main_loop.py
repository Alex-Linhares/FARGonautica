"""Loop0002 item 11: the main loop (init.py, start.py, harness.py, trace.py).

python/fixtures/main_loop.json (lisp/tests/oracle/main-loop.lisp) holds full
oracle runs, each made in a fresh SBCL process: the run's plist, what it
printed, and its JSON-lines trace (lisp/src/oracle.lisp, oracle-run-config).  The
Python runs the same problem with the same seed in a fresh World and must
give the same outcome, the same printed text and the same event stream.

Events are compared as parsed JSON, through json.dumps, so 3 and 3.0 differ
(the Lisp prints doubles its own way, 1.0e7, which parses to the same float
as Python's 10000000.0).  On a mismatch the test reports the first differing
event with the events before it.
"""

import io
import json
import re

import pytest

from conftest import LISP_DIR, REPO_DIR, load_fixture
from numbo import codelets, coderack, franz, harness, init, pnet_functions, start, trace
from numbo import events as engine_events
from numbo.cyto_def import CytoNode
from numbo.franz import intern
from numbo.pnet_def import Pnode

MAIN_LOOP = load_fixture("main_loop.json")
PNET = load_fixture("pnet.json")
RUNS = MAIN_LOOP["runs"]


def run_id(run):
    return "{}-seed{}-cap{}{}".format(run["problem"][0], run["seed"], run["max-iterations"],
                                      "-rng" if run["rng-events"] else "")


def canonical(event):
    return json.dumps(event)


def python_run(run, with_trace=True):
    out = io.StringIO()
    buf = io.StringIO() if with_trace else None
    result = harness.run_config(run["problem"], seed=run["seed"],
                                max_iterations=run["max-iterations"], trace=buf,
                                rng_events=run["rng-events"], out=out)
    events = [json.loads(line) for line in buf.getvalue().splitlines()] if with_trace else None
    return result, out.getvalue(), events


def first_divergence(expected, got, context=4):
    """None if the event lists are equal, else a report of the first
    differing event and the events before it."""
    for i, (e, g) in enumerate(zip(expected, got)):
        if canonical(e) != canonical(g):
            before = "\n".join(f"  [{j}] {canonical(expected[j])[:300]}"
                               for j in range(max(0, i - context), i))
            return (f"first divergent event #{i}:\n{before}\n"
                    f"  oracle: {canonical(e)[:600]}\n  python: {canonical(g)[:600]}")
    if len(expected) != len(got):
        i = min(len(expected), len(got))
        extra = (expected if len(expected) > i else got)[i]
        who = "oracle" if len(expected) > i else "python"
        return (f"event counts differ: oracle {len(expected)}, python {len(got)}; "
                f"first extra ({who}) #{i}: {canonical(extra)[:300]}")
    return None


# ---------------------------------------------------------------------------
# The fixture

def test_fixture_shape():
    assert [r["problem"] for r in RUNS[:2]] == [[114, 11, 20, 7, 1, 6], [6, 3, 3, 17, 11, 22]]
    assert [r["seed"] for r in RUNS[:2]] == [1, 1]
    outcomes = {r["result"]["outcome"] for r in RUNS}
    assert outcomes == {"solved", "capped", "gave-up"}
    for r in RUNS:
        assert r["trace"][0]["ev"] == "start"
        assert r["trace"][-1]["ev"] in ("done", "gave-up", "capped", "error")
    capped = next(r for r in RUNS if r["result"]["outcome"] == "capped")
    # The capped run goes past x = 400, where config checks the temperature.
    assert max(e["x"] for e in capped["trace"] if e["ev"] == "iteration") > 400
    assert any(e["ev"] == "rng" for r in RUNS if r["rng-events"] for e in r["trace"])


# ---------------------------------------------------------------------------
# Full runs

@pytest.mark.parametrize("run", RUNS, ids=run_id)
def test_outcome(run):
    result, _, _ = python_run(run, with_trace=False)
    expected = dict(run["result"], seed=run["seed"])
    assert {k: result[k] for k in expected} == expected


@pytest.mark.parametrize("run", RUNS, ids=run_id)
def test_event_stream(run):
    result, _, events = python_run(run)
    report = first_divergence(run["trace"], events)
    assert report is None, report
    assert result["outcome"] == run["result"]["outcome"]


@pytest.mark.parametrize("run", RUNS, ids=run_id)
def test_printed_output(run):
    _, output, _ = python_run(run)
    assert output == run["output"]


@pytest.mark.parametrize("run", RUNS[:2], ids=run_id)
def test_trace_only_observes(run):
    """As in the oracle, the trace hooks change nothing: the run prints the
    same text and ends the same way without a trace."""
    with_trace = python_run(run)
    without = python_run(run, with_trace=False)
    assert with_trace[:2] == without[:2]


def test_runs_are_independent():
    """Each run_config starts from a fresh World (a fresh oracle process)."""
    run = RUNS[1]
    harness.run_config(RUNS[3]["problem"], seed=2, max_iterations=300, out=io.StringIO())
    _, _, events = python_run(run)
    assert first_divergence(run["trace"], events) is None


def test_iteration_events_cover_the_run():
    """One iteration event per main-loop iteration, numbered 0 .. n-1, for
    every run (written by the Python trace; checked against the oracle's
    count)."""
    for run in RUNS:
        _, _, events = python_run(run)
        ns = [e["n"] for e in events if e["ev"] == "iteration"]
        assert ns == list(range(run["result"]["iterations"]))
        setup = [e for e in events if e["ev"] == "setup-choose"]
        assert len(setup) == 13


def test_cap_must_be_positive():
    with pytest.raises(ValueError):
        harness.run_config(RUNS[0]["problem"], seed=1, max_iterations=0, out=io.StringIO())


def test_uncapped_run():
    run = RUNS[1]
    result = harness.run_config(run["problem"], seed=1, max_iterations=None, out=io.StringIO())
    assert result["outcome"] == "solved"
    assert result["iterations"] == run["result"]["iterations"]


def test_error_outcome():
    """A Lisp error ends the run with outcome "error" and its message (the
    reactivate-cyto race, PORTING_NOTES.md item 10, is SEND to nil).  Made
    here by breaking a brick's plinks just before the first reactivate-cyto."""
    out, buf = io.StringIO(), io.StringIO()
    original = init.reactivate_cyto

    def broken(world):
        world[intern("CYTO-BRICK4")].plinks = None
        return original(world)

    init.reactivate_cyto = broken
    try:
        result = harness.run_config(RUNS[3]["problem"], seed=1, max_iterations=100,
                                    trace=buf, out=out)
    finally:
        init.reactivate_cyto = original
    assert result["outcome"] == "error"
    assert result["error"] == "SEND: NIL does not handle the message :SET-ACTIVATION"
    events = [json.loads(line) for line in buf.getvalue().splitlines()]
    assert events[-1] == {"ev": "error", "iterations": result["iterations"],
                          "message": result["error"]}
    # x = 40 is main-loop iteration 28 (PORTING_NOTES.md item 10).
    assert result["iterations"] == 29


# ---------------------------------------------------------------------------
# init.lisp and start.lisp

def test_load_time_parameters():
    """The values the init.lisp DEFVARs give (pnet.json, "defvar")."""
    world = harness.load_world()
    for name, value in PNET["parameters"]["defvar"].items():
        assert canonical(world[intern(name)]) == canonical(value), name
    assert world[intern("*ITERATION*")] == 0
    assert world[intern("*NAME-COUNTER*")] == 1
    assert len(world[intern("*PNET*")]) == 88


def test_init_chiffre():
    world = harness.load_world()
    world.out = io.StringIO()
    init.init_chiffre(world)
    for name, value in PNET["parameters"]["init-chiffre"].items():
        assert canonical(world[intern(name)]) == canonical(value), name
    assert world[intern("*CODERACK*")] is intern("MY-CODERACK")
    rack = coderack.cr_get(world, intern("MY-CODERACK"))
    assert [b[0] for b in rack.bins] == [600, 300, 150, 7, 4, 1, 0]
    assert world.out.getvalue() == "Graphics is OFF.\n"
    assert world[intern("%VERBOSE%")] is None


def test_fresh_line():
    """~& writes a newline only when the stream is not at a line start."""
    world = harness.load_world()
    world.out = harness.OutputStream(io.StringIO())
    world.out.write("abc")
    init.init_chiffre(world)
    assert world.out.stream.getvalue() == "abc\nGraphics is OFF.\n"


def test_eval_codelet_forms():
    """(eval form) of a codelet form: the arguments are evaluated (a symbol
    is its value, (quote x) is x, an object is itself) and the codelet is
    called on them."""
    world = harness.load_world()
    world.out = io.StringIO()
    init.init_chiffre(world)
    calls = []
    original = codelets.look_for_bl_plus
    codelets.look_for_bl_plus = lambda w, *args: calls.append(args) or "result"
    try:
        node = CytoNode(name=intern("X"))
        world[intern("FOO")] = 7
        form = [intern("LOOK-FOR-BL+"), intern("FOO"), [intern("QUOTE"), intern("BAR")], node]
        assert start.eval_form(world, form) == "result"
    finally:
        codelets.look_for_bl_plus = original
    assert calls == [(7, intern("BAR"), node)]
    assert start.eval_form(world, None) is None
    assert isinstance(start.eval_form(world, intern("NODE-5")), Pnode)


def test_every_codelets_function_can_be_evaluated():
    """Every codelets.lisp defun is reachable from an evaluated form."""
    defuns = re.findall(r"^\(defun\s+([^\s()]+)",
                        (LISP_DIR / "src" / "codelets.lisp").read_text(), re.M)
    assert len(set(defuns)) == 53
    for name in defuns:
        assert intern(name.upper()) in codelets.LISP_FUNCTIONS, name
        assert callable(getattr(codelets, codelets.LISP_FUNCTIONS[intern(name.upper())]))


@pytest.mark.parametrize("module, lisp_file", [(init, "init.lisp"), (start, "start.lisp"),
                                               (harness, "harness.lisp")])
def test_census(module, lisp_file):
    """One Python function per Lisp defun, each naming its origin."""
    text = (LISP_DIR / "src" / lisp_file).read_text()
    defuns = re.findall(r"^\((?:cl:)?defun\s+([^\s()]+)", text, re.M)
    assert defuns
    for name in defuns:
        py_name = name.replace("-", "_")
        fn = getattr(module, py_name, None)
        assert callable(fn), f"{module.__name__}.{py_name}"
        assert f"{lisp_file}: {name}" in (fn.__doc__ or ""), py_name


def test_trace_hooks_name_their_oracle_origin():
    """The oracle's 10 hooks are events.py's (loop0003 item 1: they publish
    typed events, and the trace is an observer of them)."""
    text = (LISP_DIR / "src" / "oracle.lisp").read_text()
    hooks = re.findall(r"^\(cl:defun (oracle-[a-z-]+-hook) ", text, re.M)
    assert len(hooks) == 10
    for name in hooks:
        fn = getattr(engine_events, name.replace("-", "_"))
        assert f"oracle.lisp: {name}" in fn.__doc__


# ---------------------------------------------------------------------------
# The trace's Lisp-data encoding (oracle.lisp: oracle-write-data)

def test_encode_lisp_data():
    node = CytoNode(name=intern("CYTO-BRICK1"))
    cases = [
        (None, None), ([], None), (True, True), (3, 3), (3.0, 3.0), (-0.5, -0.5),
        (intern("NODE-5"), "NODE-5"), (intern("BRICK1", "KEYWORD"), ":BRICK1"),
        ("free", {"str": "free"}), ([1, [intern("QUOTE"), intern("A")]], [1, ["QUOTE", "A"]]),
        (node, {"obj": "cyto-node", "name": "CYTO-BRICK1"}),
    ]
    for value, expected in cases:
        assert canonical(trace.encode_data(value)) == canonical(expected)
    with pytest.raises(TypeError):
        trace.encode_data(object())


def test_parse_decomposition():
    text = ("Operation PLUS6-1-V3 has been applied \n"
            "to CYTO-BRICK3 ( 7) and to CYTO-BRICK4 ( -1)\n"
            "to get CYTO-TARGET-6-V2\n")
    assert trace.oracle_parse_decomposition(text) == [
        {"op": "PLUS6-1-V3", "a": "CYTO-BRICK3", "va": 7, "b": "CYTO-BRICK4", "vb": -1,
         "result": "CYTO-TARGET-6-V2"}]
    with pytest.raises(ValueError):
        trace.oracle_parse_decomposition("Operation X has been\n")

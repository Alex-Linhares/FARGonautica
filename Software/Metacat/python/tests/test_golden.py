"""Golden traces and the files of loop0002 item 10 (themes, justify, trace,
jootsing, memory).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every golden of tests/problems.txt is run by the package's headless driver
(metacat/headless.py, metacat/run.py, metacat/trace_writer.py; golden_harness.py
runs them in parallel) in a fresh fork of a fresh process, and its trace must equal the golden byte
for byte; a failure names the first differing line.  The fast tier runs one short
golden (a b z, seed 1: 1000 codelets with keep-going); the slow tier runs all
109, and the original's crash on abc ccbbaa ijk seed 3 against the live oracle.
"""
from __future__ import annotations

import ast
import importlib
import inspect
import os
import re
import shutil
import subprocess
import tempfile
from pathlib import Path

import pytest

import golden_harness as g
from metacat import chez, coderack, engine
from metacat.names import scheme_to_python

ROOT = Path(g.ROOT)
ORIGINAL = ROOT / "chez_scheme" / "original"
MODULES = ("themes", "justify", "trace", "jootsing", "memory")
ORIGINS = {m: m + ".ss" for m in MODULES}


def M(name):
    return importlib.import_module("metacat." + name)


def assert_same_trace(file_name, got):
    expected = g.golden_text(file_name)
    if got != expected:
        line, e, a = g.first_difference(expected, got)
        pytest.fail(f"{file_name}: first difference at line {line}\n"
                    f"  golden: {e}\n  python: {a}", pytrace=False)


def test_problems_list_the_goldens():
    runs = g.golden_runs()
    assert len(runs) == 109
    assert sorted(r[0] for r in runs) == sorted(os.listdir(g.GOLDEN_DIR))


def test_a_short_golden():
    """a b z, seed 1, 1000 codelets with keep-going (misc4's problem)."""
    (status, reason, text, stdout), = g.run_in_fresh_process([(["a", "b", "z"], 1, 1000, True)], 1)
    assert status == "ok", reason
    assert_same_trace("a-b-z_1.jsonl", text)
    assert reason == "cap"


@pytest.fixture(scope="module")
def golden_results():
    runs = g.golden_runs()
    # the longest first, so that the pool ends evenly
    runs.sort(key=lambda r: -os.path.getsize(os.path.join(g.GOLDEN_DIR, r[0])))
    results = g.run_in_fresh_process([(s, seed, cap, keep) for _, s, seed, cap, keep in runs])
    return {r[0]: res for r, res in zip(runs, results)}


@pytest.mark.slow
@pytest.mark.parametrize("file_name", [r[0] for r in g.golden_runs()])
def test_golden(file_name, golden_results):
    status, reason, text, stdout = golden_results[file_name]
    if status != "ok":
        expected = g.golden_text(file_name)
        where = g.first_difference(expected, text or "")
        pytest.fail(f"{file_name}: {reason}\nfirst difference before it: {where}",
                    pytrace=False)
    assert_same_trace(file_name, text)
    # run.ss prints one Comment line per commentary paragraph and one Answer line
    # per answer
    assert stdout.count("\nComment: ") == text.count('"ev":"comment"')
    assert stdout.count("\nAnswer: ") == text.count('"ev":"answer"')


def _scheme():
    return shutil.which("scheme") or shutil.which("chezscheme")


@pytest.mark.slow
def test_the_original_crash_on_abc_ccbbaa_ijk_seed_3():
    """The original raises a Chez error (caddr of #f, transcribe-to-english) on abc
    ccbbaa ijk seed 3, which is why the golden set leaves it out.  The port raises
    the same error after the same trace lines (racket/tests/golden-test.rkt's
    check, against the live oracle)."""
    with tempfile.TemporaryDirectory() as tmp:
        chez_file = os.path.join(tmp, "crash.jsonl")
        proc = subprocess.run([_scheme(), "--script", "chez_scheme/oracle/run.ss", "abc",
                               "ccbbaa", "ijk", "--seed", "3", "--max-codelets", "10000",
                               "--trace", chez_file], cwd=ROOT, capture_output=True, text=True)
        assert proc.returncode == 1
        assert "Exception in caddr: incorrect list structure #f" in proc.stderr
        with open(chez_file) as f:
            expected = f.read()
    (status, reason, text, stdout), = g.run_in_fresh_process(
        [(["abc", "ccbbaa", "ijk"], 3, 10000, False)], 1)
    assert status == "error"
    assert reason.startswith("SchemeError") and "caddr" in reason, reason
    assert text == expected, g.first_difference(expected, text)
    assert len(expected.splitlines()) > 1000


def test_the_goldens_reach_every_event_and_the_new_codelets():
    """The goldens contain every event type and every Temporal Trace event type, and
    the thematic, justification and jootsing codelets run (racket/tests/golden-test.rkt's
    check).  Reads the goldens only."""
    text = "".join(g.golden_text(r[0]) for r in g.golden_runs())
    for ev in ["start", "codelet", "build", "break", "temperature", "slipnet", "themes",
               "event", "answer", "comment", "halt", "end"]:
        assert f'"ev":"{ev}"' in text, ev
    for type_ in ["answer", "snag", "clamp", "rule", "group", "concept-mapping",
                  "concept-activation"]:
        assert f'"ev":"event","type":"{type_}"' in text, type_
    for codelet in ["thematic-bridge-scout", "answer-justifier", "jootser", "progress-watcher"]:
        assert f'"type":"{codelet}"' in text, codelet


# The five modules -------------------------------------------------------------------------

def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


@pytest.mark.parametrize("name", MODULES)
def test_every_definition_has_its_python_function(name):
    mod = M(name)
    missing = [n for n in defines(ORIGINAL / ORIGINS[name])
               if not hasattr(mod, scheme_to_python(n))]
    assert missing == [], (name, missing)


@pytest.mark.parametrize("name", MODULES)
def test_docstrings_name_their_origin(name):
    mod = M(name)
    for fname, fn in vars(mod).items():
        if (callable(fn) and getattr(fn, "__module__", None) == mod.__name__
                and not fname.startswith("_") and inspect.isfunction(fn)):
            assert fn.__doc__ and fn.__doc__.split(":")[0] == ORIGINS[name], (name, fname)


@pytest.mark.parametrize("name", MODULES + ("theme_graphics", "trace_graphics"))
def test_engine_modules_import_no_gui(name):
    tree = ast.parse(inspect.getsource(M(name)))
    found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), name


def test_the_top_level_objects_exist_after_load():
    engine.load()
    assert engine.get_global("*themespace*") is not False
    assert engine.get_global("*trace*") is not False
    assert engine.get_global("*memory*") is not False
    from metacat.objects import tell
    assert tell(engine.get_global("*themespace*"), "object-type") == "themespace"


def test_codelet_procedures_are_installed():
    engine.load()
    for name in ["thematic-bridge-scout", "answer-justifier", "jootser", "progress-watcher"]:
        assert getattr(coderack, scheme_to_python(name)).codelet_proc is not False, name


def test_modules_are_loaded_in_metacat_ss_order():
    loaded = engine.translated_modules()
    for name in MODULES:
        assert M(name) in loaded
    order = engine.LOAD_ORDER
    assert ([order.index(n) for n in ("answers",) + MODULES]
            == sorted(order.index(n) for n in ("answers",) + MODULES))


def test_complement_codelet_pattern_is_never_defined():
    """trace.ss's get-complement-codelet-pattern returns complement-codelet-pattern,
    which the original never defines (anomalies_and_quirks.md)."""
    with pytest.raises(chez.UnboundVariable):
        M("trace").complement_codelet_pattern()


# python/oracle/batteries/trace-extra-battery.scm (local, written after the code, to
# reach what the goldens do not: see its header) -------------------------------------------

def _extra_battery():
    """The battery's three tests, in order, in one engine (as diff-eval runs them),
    in a fork of this process: they change the engine for good."""
    import sys
    import types

    from scheme_canon import canon
    from test_workspace import init_problem

    import metacat as _metacat
    from metacat import concept_mappings, groups, setup, slipnet, workspace
    from metacat.objects import Lambda, tell

    engine.load()
    eeg = types.ModuleType("metacat.eeg_graphics")
    eeg.g_EEG = Lambda(lambda self, msg, *args: "done")
    sys.modules["metacat.eeg_graphics"] = eeg
    _metacat.eeg_graphics = eeg
    trace, themes = M("trace"), M("themes")
    setup.p_workspace_graphics = False
    setup.g_themespace_window = Lambda(lambda self, msg, *args: "done")
    events = []

    def trace_window(self, msg, *args):
        if msg == "add-event":
            events.insert(0, tell(args[0], "print-name"))
        return "done"

    setup.g_trace_window = Lambda(trace_window)

    def new_events(thunk):
        events.clear()
        thunk()
        return list(reversed(events))

    def start(strings):
        init_problem(strings, 1)
        tell(themes.g_themespace, "initialize")
        tell(trace.g_trace, "initialize")

    def bridge(spanning_p, object1, object2):
        answers = {"get-theme-type": "vertical-bridge", "get-bridge-type": "vertical",
                   "spanning-bridge?": spanning_p, "get-object1": object1,
                   "get-object2": object2}

        def fn(self, msg, *args):
            if msg in answers:
                return answers[msg]
            raise chez.SchemeError("x:bridge", "unexpected message ~s", msg)
        return Lambda(fn)

    s = slipnet
    cm_cases = [
        (s.plato_letter_category, s.plato_a, s.plato_z),
        (s.plato_letter_category, s.plato_a, s.plato_b),
        (s.plato_letter_category, s.plato_c, s.plato_x),
        (s.plato_string_position_category, s.plato_leftmost, s.plato_rightmost),
        (s.plato_string_position_category, s.plato_leftmost, s.plato_middle),
        (s.plato_string_position_category, s.plato_single, s.plato_whole),
        (s.plato_alphabetic_position_category, s.plato_alphabetic_first,
         s.plato_alphabetic_last),
        (s.plato_object_category, s.plato_letter, s.plato_group),
        (s.plato_length, s.plato_one, s.plato_two),
        (s.plato_length, s.plato_two, s.plato_five),
        (s.plato_direction_category, s.plato_left, s.plato_right),
        (s.plato_bond_category, s.plato_successor, s.plato_predecessor),
        (s.plato_group_category, s.plato_succgrp, s.plato_predgrp),
        (s.plato_group_category, s.plato_samegrp, s.plato_succgrp),
        (s.plato_letter_category, s.plato_a, s.plato_a)]
    results = {}

    # concept-mapping-importance
    start(["abc", "abd", "xyz"])
    a = tell(workspace.g_initial_string, "get-letter", 0)
    x = tell(workspace.g_target_string, "get-letter", 0)

    def cm_case(spanning_p, c):
        cm = concept_mappings.make_concept_mapping(a, c[0], c[1], x, c[0], c[2])
        b = bridge(spanning_p, a, x)
        importance = trace.concept_mapping_importance(cm, b)
        evs = new_events(lambda: trace.monitor_new_concept_mappings([cm], b))
        return [tell(cm, "print-name"), importance, evs]
    results["concept-mapping-importance"] = chez.map_(
        lambda spanning_p: chez.map_(lambda c: cm_case(spanning_p, c), cm_cases),
        [False, True])

    # group-importance
    start(["abc", "abd", "xyz"])
    a = tell(workspace.g_initial_string, "get-letter", 0)
    b = tell(workspace.g_initial_string, "get-letter", 1)
    group = groups.make_group(workspace.g_initial_string, s.plato_succgrp,
                              s.plato_letter_category, s.plato_right, a, b, [a, b], [])

    def group_as(strength, spans_p):
        def fn(self, msg, *args):
            if msg == "get-strength":
                return strength
            if msg == "spans-whole-string?":
                return spans_p
            return group(group, msg, *args)
        return Lambda(fn)

    def group_case(c):
        gr = group_as(c[0], c[1])
        flipped_p = c[2]
        importance = trace.group_importance(gr, flipped_p)
        evs = new_events(lambda: trace.monitor_new_groups(gr, flipped_p))
        return [list(c), importance, evs]
    results["group-importance"] = chez.map_(group_case, [
        (97, False, False), (98, False, False), (99, False, False), (100, False, False),
        (50, True, False), (50, False, True)])

    # theme-spread-to-slipnet
    start(["abc", "abd", "xyz"])
    theme = themes.make_bridge_theme("vertical-bridge", s.plato_letter_category,
                                     s.plato_successor)

    def spread_case(activation, seed):
        tell(s.plato_letter_category, "set-activation", 0)
        tell(s.plato_successor, "set-activation", 0)
        tell(theme, "set-activation", activation)
        chez.random_seed(seed)
        tell(theme, "spread-activation-to-slipnet")
        tell(s.plato_letter_category, "flush-activation-buffer")
        tell(s.plato_successor, "flush-activation-buffer")
        r = [tell(s.plato_letter_category, "get-activation"),
             tell(s.plato_successor, "get-activation")]
        return [r, chez.random_seed()]
    results["theme-spread-to-slipnet"] = chez.map_(
        lambda activation: chez.map_(lambda seed: spread_case(activation, seed),
                                     [1, 2, 3, 7, 42, 1000]),
        [30, 50, 80, -60, 100])
    return {name: canon(value) for name, value in results.items()}


@pytest.fixture(scope="module")
def extra_results():
    import multiprocessing
    with multiprocessing.get_context("fork").Pool(1) as pool:
        return pool.apply(_extra_battery)


def test_every_extra_test_is_translated(extra_results):
    from chez_fixtures import manifest
    assert list(extra_results) == list(manifest("trace-extra"))


@pytest.mark.parametrize("name", ["concept-mapping-importance", "group-importance",
                                  "theme-spread-to-slipnet"])
def test_trace_extra_battery(name, extra_results):
    from chez_fixtures import chez as fixture
    assert extra_results[name] == fixture("trace-extra", name)

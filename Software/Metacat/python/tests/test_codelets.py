"""Bonds, groups and concept mappings, codelet by codelet, against Chez (loop0002 item 07).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/codelet-battery.scm, through codelet_harness.py (the
Python codelet-harness.scm).  For every problem of tests/problems.txt and each of
its seeds, the trace of the first 400 codelets of a run with only the bond and
group codelet types enabled: per codelet its type, urgency, time stamp, the
generator state after it, the structures built and broken, the window and monitor
calls and the proposal counts; every 15 codelets the temperature, all activations,
the Coderack's size and (every 60) every Workspace object.  Seven runs of 2000
codelets reach group-builder's consolidation of sameness groups.  The
concept-mapping tests check every message of every mapping between instances of
each slipnet category, and the mappings between real descriptions after 600
codelets.  The b:canon text of each must equal the frozen Chez output in
python/fixtures/codelet/; a failure names the first differing trace line
(problem, seed, codelet).

Tiers: the fast tier runs the first seed of problem 0 (400 codelets) against the
start of its fixture.  The full battery is marked slow; its 59 cases run in a
pool of processes forked after the engine and the harness are set up (each
case starts from that state: the oracle runs the cases one after another in one
process, and the traces show that none depends on the ones before it).
"""
from __future__ import annotations

import ast
import importlib
import inspect
import multiprocessing
import os
import re
import traceback
from contextlib import ExitStack

import pytest

from chez_fixtures import chez as fixture, manifest
from engine_stubs import engine_module
from name_mapping import ORIGINAL
from scheme_canon import canon
from metacat import chez, coderack, engine, setup, slipnet
from metacat.names import scheme_to_python
from metacat.objects import tell

import codelet_harness as h

MODULES = ("bonds", "groups", "concept_mappings")
ORIGINS = {"bonds": "bonds.ss", "groups": "groups.ss", "concept_mappings": "concept-mappings.ss"}


def M(name):
    return importlib.import_module("metacat." + name)


def _module_globals(mod):
    return {k: v for k, v in vars(mod).items() if k.startswith(("g_", "p_"))}


@pytest.fixture(scope="module", autouse=True)
def harness_engine():
    engine.load()
    with ExitStack() as stack:
        for mod, attrs in h.STAND_INS.items():
            stack.enter_context(engine_module(mod, **attrs))
        saved = []
        for name in ("setup", "slipnet", "coderack", "workspace", "formulas") + MODULES:
            try:
                mod = M(name)
            except ImportError:
                continue
            saved.append((mod, _module_globals(mod)))
        saved_top_level = dict(chez.TOP_LEVEL)
        h.install()
        yield
        for mod, values in saved:
            for k, v in values.items():
                setattr(mod, k, v)
        chez.TOP_LEVEL.clear()
        chez.TOP_LEVEL.update(saved_top_level)
        for type_ in coderack.g_codelet_types:
            tell(type_, "set-graphics-parameters", *([False] * 9))
        for node in slipnet.g_slipnet_nodes:
            tell(node, "reset")
        tell(coderack.g_coderack, "initialize")


def lines_of(text):
    """The trace lines of a fixture or a result (b:canon of a string: "...")."""
    assert text.startswith('"') and text.endswith('"')
    return text[1:-1].split("\n")


def first_line_difference(got, expected):
    g, e = lines_of(got), lines_of(expected)
    for n, (a, b) in enumerate(zip(g, e), 1):
        if a != b:
            i = next(k for k, (x, y) in enumerate(zip(a, b)) if x != y) if a[:len(b)] != b[:len(a)] \
                else min(len(a), len(b))
            return (f"traces differ at line {n}, character {i}:\n"
                    f"  python: ...{a[max(0, i - 300):i + 300]}\n"
                    f"  chez:   ...{b[max(0, i - 300):i + 300]}")
    return f"python has {len(g)} lines, chez {len(e)}"


def compare(name, got):
    expected = fixture("codelet", name)
    same = got == expected      # not `assert got == expected`: pytest's diff of long strings is slow
    if not same and expected.startswith('"'):
        pytest.fail(f"{name}: " + first_line_difference(got, expected))
    assert same, (name, got[:2000], expected[:2000])


# The fast tier -------------------------------------------------------------------------

def test_first_seed_of_problem_0():
    strings, seeds = h.PROBLEMS[0]
    got = h.trace_lines(strings, seeds[0], h.CODELET_CAP)
    expected = lines_of(fixture("codelet", "codelets-00"))
    prefix = h.compact(strings) + " " + str(seeds[0]) + " "
    expected = [l for l in expected if l.startswith(prefix)]
    got = got.split("\n")[:-1]
    for n, (a, b) in enumerate(zip(got, expected), 1):
        assert a == b, first_line_difference('"' + a + '"', '"' + b + '"') + f" (line {n})"
    assert len(got) == len(expected)


def test_first_category_cms():
    compare("cm-category-count", canon(h.run_case("cm-category-count")))
    compare("cm-categories-02", canon(h.run_case("cm-categories-02")))


# The whole battery, in parallel --------------------------------------------------------

def _run(name):
    try:
        return name, canon(h.run_case(name))
    except BaseException:
        return name, "EXCEPTION\n" + traceback.format_exc()


@pytest.fixture(scope="module")
def battery_results():
    names = list(manifest("codelet"))
    # the long runs first, so that the pool ends evenly
    names.sort(key=lambda n: (not n.startswith(("codelets-long", "cm-workspace")),))
    ctx = multiprocessing.get_context("fork")
    with ctx.Pool(min(len(names), os.cpu_count() or 1)) as pool:
        return dict(pool.imap_unordered(_run, names))


def test_every_battery_test_is_run():
    assert len(manifest("codelet")) == 59
    assert fixture("codelet", "codelets-problem-count") == str(len(h.PROBLEMS))


@pytest.mark.slow
@pytest.mark.parametrize("name", manifest("codelet"))
def test_codelet_battery(name, battery_results):
    got = battery_results[name]
    assert not got.startswith("EXCEPTION"), got
    compare(name, got)


@pytest.mark.slow
def test_what_the_runs_reach():
    """codelet-diff-test.rkt's checks that the traces reach every enabled codelet type,
    bonds and groups built and broken, and the ungated group-graphics call."""
    text = "\n".join(fixture("codelet", n) for n in manifest("codelet"))
    for type_ in ["bottom-up-bond-scout", "top-down-bond-scout:category",
                  "top-down-bond-scout:direction", "bond-evaluator", "bond-builder",
                  "top-down-group-scout:category", "top-down-group-scout:direction",
                  "group-scout:whole-string", "group-evaluator", "group-builder"]:
        assert re.search(r"\([0-9]+ " + re.escape(type_) + " ", text), type_
    for pattern in [r"\(built \(\(\(bond ", r"\(built \(\(\(group ", r"\(broken \(\(\(bond ",
                    r"\(broken \(\(\(group ", r"\(window caching-on\)"]:
        assert re.search(pattern, text), pattern


# Beyond the battery -----------------------------------------------------------------------

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
        if callable(fn) and getattr(fn, "__module__", None) == mod.__name__ and not fname.startswith("_"):
            assert fn.__doc__ and fn.__doc__.split(":")[0] == ORIGINS[name], (name, fname)


@pytest.mark.parametrize("name", MODULES + ("group_graphics",))
def test_engine_modules_import_no_gui(name):
    tree = ast.parse(inspect.getsource(M(name)))
    found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), name


def test_codelet_procedures_are_installed():
    for name in ["bottom-up-bond-scout", "top-down-bond-scout:category",
                 "top-down-bond-scout:direction", "bond-evaluator", "bond-builder",
                 "top-down-group-scout:category", "top-down-group-scout:direction",
                 "group-scout:whole-string", "group-evaluator", "group-builder"]:
        assert getattr(coderack, scheme_to_python(name)).codelet_proc is not False, name


def test_modules_are_loaded_in_metacat_ss_order():
    loaded = engine.translated_modules()
    for name in MODULES:
        assert M(name) in loaded
    order = engine.LOAD_ORDER
    assert order.index("bonds") < order.index("groups") < order.index("concept_mappings")


# python/oracle/batteries/codelet-extra-battery.scm (local, written after the code) ---------

def x_density_entry(s):
    """codelet-extra-battery.scm: x:density-entry"""
    density = tell(s, "get-local-density")
    support = tell(s, "get-local-support")
    return [density, support]


def x_densities(strings, seed, k):
    """codelet-extra-battery.scm: x:densities"""
    h.run_codelets(strings, seed, k)
    chez.random_seed(77)
    structures = []
    for s in h.strings():
        structures += tell(s, "get-bonds") + tell(s, "get-groups")
    acc = [[x_density_entry(x) for x in structures] for _ in range(3)]
    return [len(structures), acc, chez.random_seed()]


def x_group_builder_flips(seed):
    """codelet-extra-battery.scm: x:group-builder-flips"""
    bonds, groups = M("bonds"), M("groups")
    h.init_problem(["abc", "abd", "ijkl"], seed)
    tell(coderack.g_coderack, "initialize")
    s = h.get_global("*target-string*")
    ls = list(tell(s, "get-letters"))[:4]
    for i in range(3):
        l1, l2 = ls[i], ls[i + 1]
        bonds.build_bond(bonds.make_bond(l1, l2, slipnet.plato_successor,
                                         slipnet.plato_letter_category,
                                         tell(l1, "get-letter-category"),
                                         tell(l2, "get-letter-category")))
    flipped = [bonds.make_bond(ls[i + 1], ls[i], slipnet.plato_predecessor,
                               slipnet.plato_letter_category,
                               tell(ls[i + 1], "get-letter-category"),
                               tell(ls[i], "get-letter-category")) for i in range(3)]
    group = groups.make_group(s, slipnet.plato_predgrp, slipnet.plato_letter_category,
                              slipnet.plato_left, ls[0], ls[3], ls, flipped)
    codelet = tell(coderack.group_builder, "make-codelet", 50, group)
    h.EVENTS.clear()
    tell(codelet, "run")
    return [[h.bond_data(b) for b in tell(s, "get-bonds")],
            [h.group_data(g) for g in tell(s, "get-groups")],
            list(reversed(h.EVENTS)), chez.random_seed()]


EXTRA = {
    "local-densities-iijjkk": lambda: x_densities(["abc", "abd", "iijjkk"], 2, 600),
    "local-densities-mrrjjj": lambda: x_densities(["abc", "abd", "mrrjjj"], 1, 600),
    "local-densities-mrrkkk": lambda: x_densities(["xqc", "xqd", "mrrjjj", "mrrkkk"], 1, 800),
    "local-densities-abbbc": lambda: x_densities(["eqe", "qeq", "abbbc"], 3, 600),
    "local-densities-kkjjii": lambda: x_densities(["abc", "aabbcc", "kkjjii"], 1, 800),
    "local-densities-abcdxyzz": lambda: x_densities(["abc", "abd", "abcdxyzz"], 1, 800),
    "local-densities-aabbbcd": lambda: x_densities(["abc", "abd", "aabbbcd"], 1, 800),
}
for _seed in range(1, 7):
    EXTRA[f"group-builder-flips-{_seed}"] = (lambda seed: lambda: x_group_builder_flips(seed))(_seed)


def test_every_extra_test_is_translated():
    assert list(EXTRA) == list(manifest("codelet-extra"))


@pytest.mark.parametrize("name", [
    pytest.param(n, marks=pytest.mark.slow) if n.startswith("local-densities") else n
    for n in manifest("codelet-extra")])
def test_codelet_extra_battery(name):
    expected = fixture("codelet-extra", name)
    got = canon(EXTRA[name]())
    assert got == expected, (got[:1500], expected[:1500])

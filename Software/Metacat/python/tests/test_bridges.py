"""Bridges and breakers, codelet by codelet, against Chez (loop0002 item 08).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/bridge-battery.scm, through codelet_harness.py with the
bridges setting on (b:enable-bridges! #t): run.ss's initial bond and bridge
scouts, every bottom-up codelet type but those of rules, answers and
self-watching (so bridge scouts, description scouts and the breaker too), and
all of *top-down-slipnodes*.  For every problem of tests/problems.txt and each
of its seeds, the trace of the first 1000 codelets: per codelet its type,
urgency, time stamp, the generator state after it, the bonds, groups and bridges
built and broken (bridges with their concept mappings), the proposed bridges,
the descriptions per object, the window, monitor and Themespace calls; every 15
codelets the temperature, all activations and (every 60) every Workspace object.
Then nine bridge matrices: after 1500 codelets, fresh bridges between every pair
of objects with their concept mappings, strengths, incompatible bridges and
bonds, direction-incompatible bridges of every pair of groups, and every pair of
built bridges compared.  The b:canon text of each must equal the frozen Chez
output in python/fixtures/bridge/; a failure names the first differing trace
line (problem, seed, codelet).

Tiers: the fast tier runs the first seed of problem 0 for 300 codelets against
the start of its fixture.  The full battery is marked slow; its 46 cases run in a
pool of processes forked after the engine and the harness are set up, as in
test_codelets.py.
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
from metacat import chez, coderack, engine, slipnet
from metacat.names import scheme_to_python
from metacat.objects import tell

import codelet_harness as h
from test_codelets import first_line_difference, lines_of

MODULES = ("bridges", "breakers")
ORIGINS = {"bridges": "bridges.ss", "breakers": "breakers.ss"}
BATTERY = "bridge"
CODELET_CAP = 1000          # b:codelet-cap


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
        for name in ("setup", "slipnet", "coderack", "workspace", "formulas", "bonds",
                     "groups", "concept_mappings", "descriptions") + MODULES:
            try:
                mod = M(name)
            except ImportError:
                continue
            saved.append((mod, _module_globals(mod)))
        saved_top_level = dict(chez.TOP_LEVEL)
        h.install()
        h.enable_bridges(True)
        yield
        h.enable_bridges(False)
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


def compare(name, got):
    expected = fixture(BATTERY, name)
    same = got == expected      # not `assert got == expected`: pytest's diff of long strings is slow
    if not same and expected.startswith('"'):
        pytest.fail(f"{name}: " + first_line_difference(got, expected))
    assert same, (name, got[:2000], expected[:2000])


# bridge-battery.scm ------------------------------------------------------------------------

def problem_trace(i):
    """bridge-battery.scm: b:problem-trace"""
    strings, seeds = h.PROBLEMS[i]
    return chez.String("".join(h.trace_lines(strings, seed, CODELET_CAP) for seed in seeds))


def pairs(l1, l2):
    """bridge-battery.scm: b:pairs"""
    return [(x, y) for x in l1 for y in l2]


def fresh_bridge_data(orientation, o1, o2):
    """bridge-battery.scm: b:fresh-bridge-data (nothing here draws)"""
    br = M("bridges")
    cms = br.all_possible_bridge_CMs(orientation, o1, tell(o1, "get-relevant-descriptions"),
                                     o2, tell(o2, "get-relevant-descriptions"))
    if orientation == "horizontal":
        b = br.make_horizontal_bridge(o1, o2, cms)
    else:
        b = br.make_vertical_bridge(o1, o2, cms)
    bond = tell(b, "get-incompatible-bond") if orientation == "horizontal" else False
    return [h.obj_id(o1), h.obj_id(o2), h.cm_names(cms),
            [tell(cm, "get-strength") for cm in cms],
            tell(b, "internally-coherent?"),
            tell(b, "calculate-internal-strength"),
            tell(b, "calculate-external-strength"),
            [h.bridge_data(x) for x in tell(b, "get-incompatible-bridges")],
            h.bond_data(bond) if bond is not False else False,
            br.reverse_direction_orientation_p(cms),
            br.letter_category_mappable_objects_p(o1, o2),
            br.singleton_letter_factor(o1, o2)]


def direction_cm_data(orientation, g1, g2):
    """bridge-battery.scm: b:direction-cm-data"""
    br, cmm, s = M("bridges"), M("concept_mappings"), slipnet
    d1 = tell(g1, "get-descriptor-for", s.plato_direction_category)
    d2 = tell(g2, "get-descriptor-for", s.plato_direction_category)
    if d1 is False or d2 is False:
        return []
    cms = [cmm.make_concept_mapping(g1, s.plato_direction_category, d1,
                                    g2, s.plato_direction_category, d2),
           cmm.make_concept_mapping(g1, s.plato_direction_category, d1,
                                    g2, s.plato_direction_category,
                                    s.plato_right if d2 is s.plato_left else s.plato_left)]
    return [[h.obj_id(g1), h.obj_id(g2), tell(cm, "print-name"),
             [h.bridge_data(x) for x in br.direction_incompatible_bridges(orientation, g1, g2, cm)]]
            for cm in cms]


def bridge_pair_data(b1, b2):
    """bridge-battery.scm: b:bridge-pair-data"""
    br = M("bridges")
    cms1, cms2 = tell(b1, "get-all-concept-mappings"), tell(b2, "get-all-concept-mappings")
    if tell(b1, "get-orientation") == "horizontal":
        tests = [br.incompatible_horizontal_bridges_p(b1, b2),
                 br.supporting_horizontal_bridges_p(b1, b2),
                 br.incompatible_horizontal_CM_lists_p(cms1, cms2)]
    else:
        tests = [br.incompatible_vertical_bridges_p(b1, b2),
                 br.supporting_vertical_bridges_p(b1, b2),
                 br.incompatible_vertical_CM_lists_p(cms1, cms2)]
    return [h.bridge_data(b1), h.bridge_data(b2), tests, br.enclosing_bridge_p(b1, b2)]


def bridge_matrix(strings, seed, k):
    """bridge-battery.scm: b:bridge-matrix"""
    from metacat.utilities import group_p
    h.run_codelets(strings, seed, k)
    initial = tell(h.get_global("*initial-string*"), "get-objects")
    modified = tell(h.get_global("*modified-string*"), "get-objects")
    target = tell(h.get_global("*target-string*"), "get-objects")
    ws = h.get_global("*workspace*")

    def groups(l):
        return [x for x in l if group_p(x) is not False]

    top = tell(ws, "get-bridges", "top")
    vertical = tell(ws, "get-bridges", "vertical")
    return [[fresh_bridge_data("horizontal", a, b) for a, b in pairs(initial, modified)],
            [fresh_bridge_data("vertical", a, b) for a, b in pairs(initial, target)],
            [direction_cm_data("horizontal", a, b)
             for a, b in pairs(groups(initial), groups(modified))],
            [direction_cm_data("vertical", a, b)
             for a, b in pairs(groups(initial), groups(target))],
            [bridge_pair_data(a, b) for a, b in pairs(top, top)],
            [bridge_pair_data(a, b) for a, b in pairs(vertical, vertical)]]


MATRICES = {
    "bridge-matrix-abc-abd-xyz": (["abc", "abd", "xyz"], 1, 1500),
    "bridge-matrix-abc-abd-mrrjjj": (["abc", "abd", "mrrjjj"], 2, 1500),
    "bridge-matrix-abc-abd-kji": (["abc", "abd", "kji"], 1, 1500),
    "bridge-matrix-abc-abd-kji-3": (["abc", "abd", "kji"], 3, 1500),
    "bridge-matrix-aabc-aabd-ijkk": (["aabc", "aabd", "ijkk"], 2, 1500),
    "bridge-matrix-abc-aabbcc-kkjjii": (["abc", "aabbcc", "kkjjii"], 1, 1500),
    "bridge-matrix-eqe-qeq-abbbc": (["eqe", "qeq", "abbbc"], 3, 1500),
    "bridge-matrix-xqc-xqd-mrrjjj": (["xqc", "xqd", "mrrjjj", "mrrjjjj"], 2, 1500),
    "bridge-matrix-rst-rsu-xyz": (["rst", "rsu", "xyz", "uyz"], 1, 1500),
}


def run_case(name):
    """The value of bridge-battery.scm's test `name`."""
    if name == "bridges-problem-count":
        return len(h.PROBLEMS)
    if name in MATRICES:
        return bridge_matrix(*MATRICES[name])
    if name.startswith("bridges-"):
        return problem_trace(int(name[len("bridges-"):]))
    raise KeyError(name)


# The fast tier -------------------------------------------------------------------------

def test_first_seed_of_problem_0():
    strings, seeds = h.PROBLEMS[0]
    k = 300
    got = h.trace_lines(strings, seeds[0], k).split("\n")[:-1]
    prefix = h.compact(strings) + " " + str(seeds[0]) + " "
    expected = [l for l in lines_of(fixture(BATTERY, "bridges-00")) if l.startswith(prefix)]
    # the start and the first k codelets with their updates; the end entry differs
    for n, (a, b) in enumerate(zip(got[:-1], expected), 1):
        assert a == b, first_line_difference('"' + a + '"', '"' + b + '"') + f" (line {n})"
    assert len(got) - 1 == 1 + k + k // 15


def test_every_battery_test_is_translated():
    names = list(manifest(BATTERY))
    assert len(names) == 46
    for name in names:
        assert name == "bridges-problem-count" or name in MATRICES or \
            re.fullmatch(r"bridges-\d\d", name), name
    assert fixture(BATTERY, "bridges-problem-count") == str(len(h.PROBLEMS))


# The whole battery, in parallel --------------------------------------------------------

def _run(name):
    try:
        return name, canon(run_case(name))
    except BaseException:
        return name, "EXCEPTION\n" + traceback.format_exc()


@pytest.fixture(scope="module")
def battery_results():
    names = list(manifest(BATTERY))
    # the longest first, so that the pool ends evenly: problems with many seeds, then
    # the matrices
    weight = {n: len(h.PROBLEMS[int(n[len("bridges-"):])][1]) * 1000
              for n in names if re.fullmatch(r"bridges-\d\d", n)}
    names.sort(key=lambda n: -weight.get(n, 1500 if n in MATRICES else 0))
    ctx = multiprocessing.get_context("fork")
    with ctx.Pool(min(len(names), os.cpu_count() or 1)) as pool:
        return dict(pool.imap_unordered(_run, names))


@pytest.mark.slow
@pytest.mark.parametrize("name", manifest(BATTERY))
def test_bridge_battery(name, battery_results):
    got = battery_results[name]
    assert not got.startswith("EXCEPTION"), got
    compare(name, got)


@pytest.mark.slow
def test_what_the_runs_reach():
    """The traces reach every bridge, description and breaker codelet type, bridges
    built and broken, bridges with flipped groups, and the Themespace calls."""
    text = "\n".join(fixture(BATTERY, n) for n in manifest(BATTERY))
    for type_ in ["bottom-up-bridge-scout", "important-object-bridge-scout",
                  "bridge-evaluator", "bridge-builder", "bottom-up-description-scout",
                  "top-down-description-scout", "description-evaluator",
                  "description-builder", "breaker"]:
        assert re.search(r"\([0-9]+ " + re.escape(type_) + " ", text), type_
    for pattern in [r"\(built \(.*\(\(bridge ", r"\(broken \(.*\(\(bridge ",
                    r"\(bridge (top|vertical) .* #t", r"\(add-theme ",
                    r"\(new-cms "]:
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


@pytest.mark.parametrize("name", MODULES)
def test_engine_modules_import_no_gui(name):
    tree = ast.parse(inspect.getsource(M(name)))
    found = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in found), name


def test_codelet_procedures_are_installed():
    for name in ["bottom-up-bridge-scout", "important-object-bridge-scout",
                 "bridge-evaluator", "bridge-builder", "breaker"]:
        assert getattr(coderack, scheme_to_python(name)).codelet_proc is not False, name


def test_modules_are_loaded_in_metacat_ss_order():
    loaded = engine.translated_modules()
    for name in MODULES:
        assert M(name) in loaded
    order = engine.LOAD_ORDER
    assert order.index("groups") < order.index("bridges") < order.index("breakers")


# python/oracle/batteries/bridge-extra-battery.scm (local, written after the code) -------

def x_fresh_vertical(o1, o2):
    """bridge-extra-battery.scm: x:fresh-vertical"""
    br = M("bridges")
    cms = br.all_possible_bridge_CMs("vertical", o1, tell(o1, "get-relevant-descriptions"),
                                     o2, tell(o2, "get-relevant-descriptions"))
    b = br.make_vertical_bridge(o1, o2, cms)
    rd = tell(b, "get-relevant-distinguishing-CMs")
    return [h.obj_id(o1), h.obj_id(o2), h.cm_names(rd),
            [tell(cm, "get-strength") for cm in rd],
            tell(b, "internally-coherent?"), tell(b, "calculate-internal-strength")]


def x_fresh_bridges(strings, seed, k):
    """bridge-extra-battery.scm: x:fresh-bridges"""
    h.run_codelets(strings, seed, k)
    initial = tell(h.get_global("*initial-string*"), "get-objects")
    target = tell(h.get_global("*target-string*"), "get-objects")
    return [x_fresh_vertical(o1, o2) for o1 in initial for o2 in target]


EXTRA = {
    "fresh-bridges-abc-abd-glz-2": lambda: x_fresh_bridges(["abc", "abd", "glz"], 2, 1500),
}


def test_every_extra_test_is_translated():
    assert list(EXTRA) == list(manifest("bridge-extra"))


@pytest.mark.slow
@pytest.mark.parametrize("name", manifest("bridge-extra"))
def test_bridge_extra_battery(name):
    expected = fixture("bridge-extra", name)
    got = canon(EXTRA[name]())
    assert got == expected, (got[:1500], expected[:1500])

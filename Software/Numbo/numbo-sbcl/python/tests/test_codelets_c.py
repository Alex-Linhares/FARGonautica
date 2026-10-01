"""Tests for the last codelets of numbo.codelets (loop0002 item 10):
decomp+, decompi, decompx, const-bl+, const-blx, replace-target,
propagate-success, create-op-node, update-success, temperature,
collect-misfortune, check-temperature, decrease-interest, decompose,
create-coderack, misfortune, mean and diff300; and kill-block and repump
again, now inside the runs that reach the kill-block gap.

Every case comes from python/fixtures/codelets_c.json
(tests/oracle/codelets-c.lisp): a call in a real oracle run (the first of
each coderack or main-loop codelet in every chapter puzzle with seed 1, and
the first with each new outcome signature in seeds 1-3, nested calls
included), every kill/replace/propagate/decompose call of the gap run, or a
call made up on a real state for a branch the runs don't reach.  Each is
replayed on the Python function and must match exactly
(codelet_cases.run_case).  misfortune, mean and diff300 are checked on the
fixture's tables.

The coverage test at the end lists all 53 codelets.lisp functions: items 8,
9 and 10 together port every one.
"""

import inspect
import io
import re
from pathlib import Path

import pytest

import test_codelets_a
import test_codelets_b
from codelet_cases import case_ids, dumps, run_case
from conftest import load_fixture
from numbo import codelets, franz
from numbo.world import World
from world_state import load_world

CC = load_fixture("codelets_c.json")
REPO = Path(__file__).resolve().parents[2]

CODELETS = {
    "DECOMP+": "decomp_plus",
    "DECOMPI": "decompi",
    "DECOMPX": "decompx",
    "CONST-BL+": "const_bl_plus",
    "CONST-BLX": "const_blx",
    "REPLACE-TARGET": "replace_target",
    "PROPAGATE-SUCCESS": "propagate_success",
    "CREATE-OP-NODE": "create_op_node",
    "UPDATE-SUCCESS": "update_success",
    "TEMPERATURE": "temperature",
    "COLLECT-MISFORTUNE": "collect_misfortune",
    "CHECK-TEMPERATURE": "check_temperature",
    "DECREASE-INTEREST": "decrease_interest",
    "DECOMPOSE": "decompose",
    "CREATE-CODERACK": "create_coderack",
    # ported by item 8, replayed here in the gap run
    "KILL-BLOCK": "kill_block",
    "KILL-NODE": "kill_node",
    "REPUMP": "repump",
}

# The Lisp functions this item ports (Python name -> codelets.lisp name).
PORTED = {
    "check_temperature": "check-temperature", "collect_misfortune": "collect-misfortune",
    "const_bl_plus": "const-bl+", "const_blx": "const-blx",
    "create_coderack": "create-coderack", "create_op_node": "create-op-node",
    "decomp_plus": "decomp+", "decompx": "decompx", "decompi": "decompi",
    "decompose": "decompose", "decrease_interest": "decrease-interest",
    "diff300": "diff300", "mean": "mean", "misfortune": "misfortune",
    "propagate_success": "propagate-success", "replace_target": "replace-target",
    "temperature": "temperature", "update_success": "update-success",
}

CASES = CC["cases"]
IDS = case_ids(CASES)
GAP = CC["gap-run"]


def _fn(name):
    return getattr(codelets, CODELETS[name])


def test_fixture_shape():
    assert len(CC["holders"]) == 91
    assert set(CODELETS) == {c["codelet"] for c in CASES}
    for case in CASES:
        assert set(case) == {"source", "codelet", "args", "before", "arg-nodes", "result",
                             "error", "output", "draws", "after", "changed"}
        assert list(case["before"]) == ["globals", "cytoplasm", "current-target", "context",
                                        "pnodes", "coderack", "rng", "nodes"]


@pytest.mark.parametrize("case", CASES, ids=IDS)
def test_case(case):
    run_case(case, CC["parameters"], _fn(case["codelet"]))


# ---------------------------------------------------------------------------
# Coverage of the cases


def _runs(name):
    return {tuple(c["source"][1]) for c in CASES
            if c["codelet"] == name and c["source"][0] == ":RUN"}


def _posts(case):
    """The forms a case posted (in the after rack, not in the before one)."""
    before = {dumps(f) for b in case["before"]["coderack"] for f in b[1:]}
    return [f for b in case["after"]["coderack"] for f in b[1:] if dumps(f) not in before]


def test_coderack_codelets_have_run_cases():
    """Real-run cases for each codelet the coderack or the main loop runs,
    from several puzzles; each made-up-only function has a non-error case."""
    for name in ("DECOMP+", "DECOMPI", "CONST-BL+", "CONST-BLX", "REPLACE-TARGET",
                 "CHECK-TEMPERATURE", "DECREASE-INTEREST"):
        assert len(_runs(name)) >= 3, (name, _runs(name))
    for name in ("PROPAGATE-SUCCESS", "CREATE-OP-NODE", "UPDATE-SUCCESS", "TEMPERATURE",
                 "COLLECT-MISFORTUNE", "DECOMPOSE", "KILL-BLOCK", "CREATE-CODERACK"):
        assert _runs(name), name
    for name in CODELETS:
        if name != "DECOMPX":     # always an error (its let rebinds cyto-block)
            assert any(not c["error"] for c in CASES if c["codelet"] == name), name


def test_posts_cover_the_branches():
    expected = {
        "DECOMP+": {"REPLACE-TARGET", "LINK-TO-PNET", "COMPARE-B-TO-T", None},
        "DECOMPI": {"LOOK-FOR-BLX", None},
        "CONST-BL+": {"LINK-TO-PNET", "COMPARE-B-TO-T", None},
        "CONST-BLX": {"LINK-TO-PNET", "COMPARE-B-TO-T", None},
        "CHECK-TEMPERATURE": {"KILL-NODE", None},
    }
    for name, kinds in expected.items():
        seen = set()
        for case in CASES:
            if case["codelet"] == name and not case["error"]:
                posts = _posts(case)
                seen |= {f[0] for f in posts} if posts else {None}
        assert kinds <= seen, (name, kinds - seen)


def test_replace_target_and_propagate_success_solve():
    """replace-target solves directly ("Obvious."), through propagate-success,
    or does nothing (a current target that is no longer *current-target*'s)."""
    rt = [c for c in CASES if c["codelet"] == "REPLACE-TARGET" and not c["error"]]
    assert any(c["output"] == "Obvious. " for c in rt)
    assert any(c["before"]["globals"]["*problem-solved*"] == 0
               and c["after"]["globals"]["*problem-solved*"] == 1
               and c["output"] == "" for c in rt)
    assert any(c["after"] == c["before"] for c in rt) or \
        any(dumps(c["after"]["nodes"]) == dumps(c["before"]["nodes"]) for c in rt)
    ps = [c for c in CASES if c["codelet"] == "PROPAGATE-SUCCESS"]
    assert any(c["after"]["globals"]["*problem-solved*"] == 1 for c in ps)


def test_temperature_branches():
    """Both temperature formulas, integer and float results, (max) = 0 with
    no secondary nodes, and the errors."""
    kinds = {type(c["result"]).__name__ for c in CASES
             if c["codelet"] == "TEMPERATURE" and not c["error"]}
    assert {"int", "float"} <= kinds, kinds
    assert any(c["error"] for c in CASES if c["codelet"] == "TEMPERATURE")
    assert any(c["result"] == 0 for c in CASES if c["codelet"] == "COLLECT-MISFORTUNE") \
        or any(c["result"] in (None, []) for c in CASES if c["codelet"] == "COLLECT-MISFORTUNE")
    assert any(c["draws"] > 0 for c in CASES if c["codelet"] == "CHECK-TEMPERATURE")


def test_update_success_branches():
    results = {dumps(c["result"]) for c in CASES if c["codelet"] == "UPDATE-SUCCESS"}
    assert "null" in results
    assert len(results) >= 4, results


def test_check_temperature_is_a_lexpr():
    """(defun check-temperature function () ...) is a Franz lexpr: any number
    of arguments (PORTING_NOTES.md, census: lexpr)."""
    sig = inspect.signature(codelets.check_temperature)
    assert any(p.kind is p.VAR_POSITIONAL for p in sig.parameters.values())


# ---------------------------------------------------------------------------
# The kill-block gap (PORTING_NOTES.md, item 10)


def _dangling(world):
    """An operation node of the cytoplasm with a killed neighbor."""
    cytoplasm = world[franz.intern("*CYTOPLASM*")]
    for node in cytoplasm.nodes or ():
        if franz.equal("5g", node.type):
            if any(franz.equal("killed", pair[0].status) for pair in node.neighbors or ()):
                return True
    return False


def _gap_cases():
    return [c for c in CASES
            if c["source"] == [":RUN", GAP["problem"], GAP["seed"]]]


def test_gap_run_is_an_invalid_done():
    """The oracle run of puzzle 3 seed 8 prints "Done :", and the solution
    checker rejects it: a killed block is used but never derived."""
    assert GAP["outcome"] == ":SOLVED"
    assert GAP["valid"] is None
    assert "is used but never derived" in GAP["reason"]
    assert "Done : " in GAP["output"]


def test_gap_is_reproduced():
    """Python's kill-block / kill-node, on the gap run's states, leave an
    operation node with a killed neighbor, as the oracle does; and Python's
    decompose prints the oracle's invalid decomposition."""
    gap = _gap_cases()
    names = {c["codelet"] for c in gap}
    assert {"KILL-NODE", "KILL-BLOCK", "REPLACE-TARGET", "PROPAGATE-SUCCESS",
            "DECOMPOSE"} <= names, names
    left = 0
    for case in gap:
        if case["codelet"] not in ("KILL-NODE", "KILL-BLOCK"):
            continue
        world, decode = load_world(case["before"], CC["parameters"], case["arg-nodes"])
        world.out = io.StringIO()
        before = _dangling(world)
        _fn(case["codelet"])(world, *decode(case["args"]))
        if not before and _dangling(world):
            left += 1
    assert left >= 1

    killed = re.findall(r"^(.*) \((\d+)\) is used", GAP["reason"])[0][0]
    top = [c for c in gap if c["codelet"] == "DECOMPOSE" and c["args"] == ["CYTO-TARGET"]
           and c["before"]["globals"]["*problem-solved*"] == 1]
    assert top
    world, decode = load_world(top[0]["before"], CC["parameters"], top[0]["arg-nodes"])
    world.out = io.StringIO()
    codelets.decompose(world, *decode(top[0]["args"]))
    printed = world.out.getvalue()
    assert printed and killed in printed
    assert ("Done : " + printed) in GAP["output"]
    assert f"Node {killed} killed" in GAP["output"].split("Done : ")[0]


# ---------------------------------------------------------------------------
# The pure helpers, on the oracle's tables


def _check_table(rows, fn):
    for *args, (kind, *value) in rows:
        if kind == ":ERROR":
            with pytest.raises(Exception):
                fn(*args)
        else:
            assert dumps(fn(*args)) == dumps(value[0]), args


def test_misfortune():
    rows = [[i, lv, s["str"] if isinstance(s, dict) else s, r]
            for i, lv, s, r in CC["helpers"]["misfortune"]]
    _check_table(rows, codelets.misfortune)


def test_mean():
    _check_table(CC["helpers"]["mean"], lambda lst: codelets.mean(lst or None))


def test_diff300():
    for v, expected in CC["helpers"]["diff300"]:
        assert dumps(codelets.diff300(v)) == dumps(expected), v


# ---------------------------------------------------------------------------
# Every codelets.lisp function


def test_every_codelets_lisp_function_is_ported():
    """All 53 defuns of src/codelets.lisp have a Python counterpart (items 8,
    9 and 10), each with a docstring naming its Lisp origin."""
    source = (REPO / "src" / "codelets.lisp").read_text()
    defuns = re.findall(r"^\(defun ([^\s()]+)", source, re.M)
    assert len(defuns) == len(set(defuns)) == 53
    ported = {}
    for table in (test_codelets_a.PORTED, test_codelets_b.PORTED, PORTED):
        assert not set(table) & set(ported)
        ported.update(table)
    assert sorted(ported.values()) == sorted(defuns)
    for py_name, lisp_name in ported.items():
        fn = getattr(codelets, py_name)
        assert callable(fn)
        assert inspect.getdoc(fn).startswith(f"codelets.lisp: {lisp_name}"), py_name

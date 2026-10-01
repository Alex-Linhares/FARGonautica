"""Tests for the block-search codelets of numbo.codelets (loop0002 item 9):
look-for-new-block, look-for-blx, look-for-bl+, look-for-approx-blx,
look-for-approx-bl+, look-for-diff, compare-b-to-t,
test-if-possible-and-desirable, and the helpers they call.

Every case comes from python/fixtures/codelets_b.json
(tests/oracle/codelets-b.lisp): a call of one of the 8 codelets in a real
oracle run (the first of each in every chapter puzzle with seed 1, and the
first with each new outcome signature in seeds 1-3), or made up on a real
state for a branch the runs don't reach.  Each is replayed on the Python
function and must match exactly (codelet_cases.run_case).  The pure helpers
(sim, digits-in-common, multiple, compare, eliminate, remove-dd, randlist,
find-node) are checked on the fixture's tables of oracle values.
"""

import inspect
import re
from pathlib import Path

import pytest

from codelet_cases import case_ids, dumps, run_case
from conftest import load_fixture
from numbo import codelets
from numbo.franz import intern
from numbo.world import World

CB = load_fixture("codelets_b.json")
REPO = Path(__file__).resolve().parents[2]

CODELETS = {
    "LOOK-FOR-NEW-BLOCK": "look_for_new_block",
    "LOOK-FOR-BLX": "look_for_blx",
    "LOOK-FOR-BL+": "look_for_bl_plus",
    "LOOK-FOR-APPROX-BLX": "look_for_approx_blx",
    "LOOK-FOR-APPROX-BL+": "look_for_approx_bl_plus",
    "LOOK-FOR-DIFF": "look_for_diff",
    "COMPARE-B-TO-T": "compare_b_to_t",
    "TEST-IF-POSSIBLE-AND-DESIRABLE": "test_if_possible_and_desirable",
}

# The Lisp functions this item ports (Python name -> codelets.lisp name).
PORTED = {py: lisp.lower() for lisp, py in CODELETS.items()}
PORTED.update({
    "compare": "compare", "digits_in_common": "digits-in-common",
    "eliminate": "eliminate", "find_activations": "find-activations",
    "find_in": "find-in", "find_interest_in_pnet": "find-interest-in-pnet",
    "find_node": "find-node", "is_linked_to": "is-linked-to", "multiple": "multiple",
    "randlist": "randlist", "remove_dd": "remove-dd", "sim": "sim",
})

CASES = CB["cases"]
IDS = case_ids(CASES)
DIFF, DIFFREL, MIN, ADDRESS, DIV = (intern(n) for n in ("DIFF", "DIFFREL", "MIN", "ADDRESS",
                                                         "DIV"))


def _fn(name):
    return getattr(codelets, CODELETS[name])


def test_fixture_shape():
    assert len(CB["holders"]) == 91
    assert set(CODELETS) == {c["codelet"] for c in CASES}
    for case in CASES:
        assert set(case) == {"source", "codelet", "args", "before", "arg-nodes", "result",
                             "error", "output", "draws", "after", "changed"}
        assert list(case["before"]) == ["globals", "cytoplasm", "current-target", "context",
                                        "pnodes", "coderack", "rng", "nodes"]


@pytest.mark.parametrize("case", CASES, ids=IDS)
def test_case(case):
    run_case(case, CB["parameters"], _fn(case["codelet"]))


# ---------------------------------------------------------------------------
# Coverage of the cases


def _posts(case):
    """The forms a case posted (in the after rack, not in the before one)."""
    before = [f for b in case["before"]["coderack"] for f in b[1:]]
    return [f for b in case["after"]["coderack"] for f in b[1:] if f not in before]


def test_every_codelet_has_run_cases():
    """Real-run cases for each codelet, from several puzzles, and RNG draws
    where the codelet draws."""
    for name in CODELETS:
        runs = {tuple(c["source"][1]) for c in CASES
                if c["codelet"] == name and c["source"][0] == ":RUN"}
        assert len(runs) >= 3, (name, runs)
    for name in ("LOOK-FOR-NEW-BLOCK", "LOOK-FOR-APPROX-BL+", "LOOK-FOR-APPROX-BLX",
                 "LOOK-FOR-DIFF"):
        assert any(c["draws"] > 0 for c in CASES if c["codelet"] == name), name


def test_posts_cover_the_branches():
    """Each codelet reaches each codelet it can post."""
    expected = {
        "LOOK-FOR-NEW-BLOCK": {"TEST-IF-POSSIBLE-AND-DESIRABLE"},
        "LOOK-FOR-BLX": {"LOOK-FOR-APPROX-BLX", "TEST-IF-POSSIBLE-AND-DESIRABLE"},
        "LOOK-FOR-BL+": {"LOOK-FOR-APPROX-BL+", "TEST-IF-POSSIBLE-AND-DESIRABLE"},
        "LOOK-FOR-APPROX-BLX": {"TEST-IF-POSSIBLE-AND-DESIRABLE", None},
        "LOOK-FOR-APPROX-BL+": {"DECOMP+", "TEST-IF-POSSIBLE-AND-DESIRABLE", None},
        "LOOK-FOR-DIFF": {"LOOK-FOR-DIFF", "TEST-IF-POSSIBLE-AND-DESIRABLE", None},
        "COMPARE-B-TO-T": {"REPLACE-TARGET", "DECOMP+", "DECOMPI", None},
        "TEST-IF-POSSIBLE-AND-DESIRABLE": {"CONST-BL+", "CONST-BLX", None},
    }
    for name, kinds in expected.items():
        seen = set()
        for case in CASES:
            if case["codelet"] == name and not case["error"]:
                posts = _posts(case)
                seen |= {f[0] for f in posts} if posts else {None}
        assert kinds <= seen, (name, kinds - seen)


def test_compare_b_to_t_urgencies():
    """decomp+ at each of its three urgencies (sim 1, 2, 3)."""
    p = CB["parameters"]
    urgencies = set()
    for case in CASES:
        if case["codelet"] == "COMPARE-B-TO-T":
            before = {dumps(f) for b in case["before"]["coderack"] for f in b[1:]}
            for b in case["after"]["coderack"]:
                urgencies |= {b[0] for f in b[1:] if f[0] == "DECOMP+" and dumps(f) not in before}
    assert {p["%FIRST-URGENCY%"], p["%SECOND-URGENCY%"], p["%FOURTH-URGENCY%"]} <= urgencies


def test_test_if_possible_kills_and_errors():
    """test-if-possible-and-desirable frees a linked block by killing the
    node above it (output "killed"), and the oracle's errors are covered."""
    kills = [c for c in CASES
             if c["codelet"] == "TEST-IF-POSSIBLE-AND-DESIRABLE" and "killed" in c["output"]]
    assert kills
    assert any(c["error"] for c in CASES if c["codelet"] == "LOOK-FOR-NEW-BLOCK")


def test_look_for_diff_covers_every_trial():
    trials = {c["args"][0] for c in CASES if c["codelet"] == "LOOK-FOR-DIFF"}
    assert {0, 1, 2, 3, 4} <= trials, trials


# ---------------------------------------------------------------------------
# The pure helpers, on the oracle's tables


def test_sim():
    for a, b, (kind, *value), diffrel in CB["helpers"]["sim"]:
        world = World()
        if kind == ":ERROR":
            with pytest.raises(Exception):
                codelets.sim(world, a, b)
            continue
        assert dumps(codelets.sim(world, a, b)) == dumps(value[0]), (a, b)
        assert dumps(world[DIFFREL]) == dumps(diffrel), (a, b)
        assert world[DIFF] == abs(a - b)


def test_digits_in_common():
    for a, b, expected in CB["helpers"]["digits-in-common"]:
        assert dumps(codelets.digits_in_common(a, b)) == dumps(expected), (a, b)


def test_digits_in_common_examples():
    """The examples of its comment."""
    assert codelets.digits_in_common(7, 71) == [0, 1, 0]
    assert codelets.digits_in_common(7, 607) == [0, 0, 1]
    assert codelets.digits_in_common(7, 707) == [1, 0, 1]
    assert codelets.digits_in_common(22, 222) == [0, 1, 1]


def test_multiple():
    for a, b, expected in CB["helpers"]["multiple"]:
        assert dumps(codelets.multiple(a, b)) == dumps(expected), (a, b)


def test_compare():
    for e, lst, expected in CB["helpers"]["compare"]:
        assert dumps(codelets.compare(World(), e, lst)) == dumps(expected), (e, lst)


def test_eliminate():
    """Including the 1987 globals: diff and min stay set, and with no
    difference under 999 the old min is removed (or, absent, the list comes
    back reversed)."""
    world = World()
    for m, e, lst, expected, diff, min_ in CB["helpers"]["eliminate"]:
        if m is not None:
            world[MIN] = m
        assert dumps(codelets.eliminate(world, e, lst)) == dumps(expected), (m, e, lst)
        assert dumps(world[DIFF] if DIFF in world else ":UNBOUND") == dumps(diff)
        assert dumps(world[MIN]) == dumps(min_)


def test_remove_dd():
    for x, lst, expected in CB["helpers"]["remove-dd"]:
        assert dumps(codelets.remove_dd(x, lst)) == dumps(expected), (x, lst)


def test_randlist():
    for seed, lst, expected, draws, state in CB["helpers"]["randlist"]:
        world = World()
        world.rng.state = _seed_state(seed)
        before = world.rng.draws
        assert dumps(codelets.randlist(world, lst)) == dumps(expected), (seed, lst)
        assert world.rng.draws - before == draws
        assert world.rng.state == state


def _seed_state(seed):
    from numbo.rng import Rng
    return Rng(seed).state


def test_find_node():
    from numbo import pnet_def
    world = World()
    pnet_def.init_pnet(world)
    for v, holder, address, div in CB["helpers"]["find-node"]:
        p = codelets.find_node(world, v)
        assert p is world[intern(holder)], v
        assert world[ADDRESS] is intern(address)
        if address != f"NODE-{v}" and v != 0:   # round ran: it set div
            assert world[DIV] == div, v


def test_ported_functions_exist_with_lisp_docstrings():
    source = (REPO / "src" / "codelets.lisp").read_text()
    defuns = set(re.findall(r"^\(defun ([^\s()]+)", source, re.M))
    for py_name, lisp_name in PORTED.items():
        assert lisp_name in defuns, lisp_name
        fn = getattr(codelets, py_name)
        assert inspect.getdoc(fn).startswith(f"codelets.lisp: {lisp_name}"), py_name

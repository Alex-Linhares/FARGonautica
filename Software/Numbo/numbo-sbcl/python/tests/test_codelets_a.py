"""Tests for the first codelets of numbo.codelets (loop0002 item 8):
create-cyto-node, link-to-pnet, activate, kill-node, free-from-pnet,
read-target, read-brick, and the helpers they call.

Every case comes from python/fixtures/codelets_a.json
(tests/oracle/codelets-a.lisp): a call of one of the 7 functions in a real
oracle run (the first of each in every chapter puzzle with seeds 1-3, and the
first with each new signature), or made up on a real state for a branch the
runs don't reach.  For each, the World state before the call is rebuilt
(world_state.py), the Python function is called on the same arguments, and
its result (or error), output, RNG draws, the World state after it and the
globals it changed must all match (codelet_cases.run_case).
"""

import inspect
import re
from pathlib import Path

import pytest

from codelet_cases import case_ids, dumps, run_case
from conftest import load_fixture
from numbo import codelets
from numbo.franz import intern

CA = load_fixture("codelets_a.json")
REPO = Path(__file__).resolve().parents[2]

CODELETS = {
    "CREATE-CYTO-NODE": codelets.create_cyto_node,
    "LINK-TO-PNET": codelets.link_to_pnet,
    "ACTIVATE": codelets.activate,
    "KILL-NODE": codelets.kill_node,
    "FREE-FROM-PNET": codelets.free_from_pnet,
    "READ-TARGET": codelets.read_target,
    "READ-BRICK": codelets.read_brick,
}

# The Lisp functions this item ports: the 7 codelets and the helpers they
# call (Python name -> codelets.lisp name).
PORTED = {
    "activate": "activate", "create_cyto_node": "create-cyto-node",
    "disconnect": "disconnect", "free_from_pnet": "free-from-pnet",
    "is_linked_to_target": "is-linked-to-target", "kill_block": "kill-block",
    "kill_dtarget": "kill-dtarget", "kill_node": "kill-node",
    "link_to_pnet": "link-to-pnet", "ratio": "ratio", "read_brick": "read-brick",
    "read_target": "read-target", "repump": "repump", "round_": "round", "sup": "sup",
}


CASES = CA["cases"]
IDS = case_ids(CASES)


def test_fixture_shape():
    assert len(CA["holders"]) == 91
    assert set(CODELETS) == {c["codelet"] for c in CASES}
    for case in CASES:
        assert set(case) == {"source", "codelet", "args", "before", "arg-nodes", "result",
                             "error", "output", "draws", "after", "changed"}
        assert list(case["before"]) == ["globals", "cytoplasm", "current-target", "context",
                                        "pnodes", "coderack", "rng", "nodes"]


def test_every_codelet_has_run_cases_from_each_puzzle():
    """The first call of each codelet in each chapter puzzle, seed 1 (the
    fixture's minimum), is there."""
    puzzles = {tuple(c["source"][1]) for c in CASES if c["source"][0] == ":RUN"}
    assert len(puzzles) == 11
    for name in CODELETS:
        seen = {tuple(c["source"][1]) for c in CASES
                if c["codelet"] == name and c["source"][0] == ":RUN" and c["source"][2] == 1}
        # kill-node and free-from-pnet happen only in runs that kill a node
        if name in ("KILL-NODE", "FREE-FROM-PNET"):
            assert len(seen) >= 3, (name, seen)
        else:
            assert seen == puzzles, (name, puzzles - seen)


@pytest.mark.parametrize("case", CASES, ids=IDS)
def test_case(case):
    run_case(case, CA["parameters"], CODELETS[case["codelet"]])


def test_kill_node_cases_cover_the_branches():
    """kill-node on a free and a linked block, a derived target, a brick, and
    a node no longer in the cytoplasm; some posting activate, some
    compare-b-to-t."""
    kinds = set()
    for case in CASES:
        if case["codelet"] != "KILL-NODE":
            continue
        node = case["args"][0]["node"]
        nodes = case["before"]["nodes"] + case["arg-nodes"]
        in_cyto = {"node": node} in (case["before"]["cytoplasm"]["nodes"] or [])
        kinds.add((nodes[node]["type"]["str"], nodes[node]["status"]["str"], in_cyto))
    assert {("4bl", "free", True), ("4bl", "linked", True), ("3dt", "free", True),
            ("2b", "free", True), ("4bl", "free", False)} <= kinds, kinds
    kills = [c for c in CASES if c["codelet"] == "KILL-NODE" and c["output"].count("killed") > 2]
    assert kills, "no kill-node case cascades to more than one disconnect"


def _upper_neighbor(nodes, i):
    """(cyto-node :upper-neighbor) on a state's encoded nodes."""
    res, lev = None, nodes[i]["level"]
    for pair in nodes[i]["neighbors"] or ():
        if nodes[pair[0]["node"]]["level"] > lev:
            res, lev = pair[0]["node"], nodes[pair[0]["node"]]["level"]
    return res


def test_a_kill_node_case_reaches_the_kill_block_gap():
    """kill-node on a linked block whose operation node is under a derived
    target: kill-block has no "3dt" clause (PORTING_NOTES.md item 10), so the
    operation node is left behind."""
    gaps = []
    for case in CASES:
        if case["codelet"] != "KILL-NODE":
            continue
        nodes = case["before"]["nodes"] + case["arg-nodes"]
        node = nodes[case["args"][0]["node"]]
        if node["type"] != {"str": "4bl"} or node["status"] != {"str": "linked"}:
            continue
        upper = _upper_neighbor(nodes, node["neighbors"][0][0]["node"])
        if upper is not None and nodes[upper]["type"] == {"str": "3dt"}:
            gaps.append(case)
    assert gaps


def test_link_to_pnet_cases_cover_the_branches():
    """Each address branch (a pnode of that value, value 0, rounded, node-150)
    and the killed node."""
    seen = set()
    for case in CASES:
        if case["codelet"] != "LINK-TO-PNET":
            continue
        value = case["args"][2]
        posts = [form for bin_ in case["after"]["coderack"] for form in bin_[1:]]
        before = [form for bin_ in case["before"]["coderack"] for form in bin_[1:]]
        new = [f for f in posts if f not in before]
        if not new:
            seen.add("killed")
            continue
        address = new[0][2]
        if address == f"NODE-{value}":
            seen.add("bound")
        elif value == 0:
            seen.add("zero")
        elif address == "NODE-150":
            seen.add("big")
        else:
            seen.add("rounded")
    assert seen == {"bound", "zero", "rounded", "big", "killed"}, seen


@pytest.mark.parametrize("value, expected", CA["helpers"]["round"])
def test_round(value, expected):
    assert codelets.round_(_world(), value) == expected


def test_round_sets_div():
    world = _world()
    codelets.round_(world, 31)
    assert world[intern("DIV")] == 10


@pytest.mark.parametrize("value, expected", CA["helpers"]["ratio"])
def test_ratio(value, expected):
    assert dumps(codelets.ratio(_world(), value)) == dumps(expected)


def test_expt():
    """activate's (expt 0.9 val): SBCL's float-to-integer expt."""
    for value, expected in CA["helpers"]["expt"]:
        assert dumps(codelets._expt(0.9, value)) == dumps(expected), value


def _world():
    from numbo.world import World
    return World()


def test_sup():
    """codelets.lisp: sup on a list of pairs (used by disconnect)."""
    a, b, c = intern("A"), intern("B"), intern("C")
    assert codelets.sup([[a, 1], [b, 2], [c, 3]], b) == [c, a]
    assert codelets.sup(None, a) is None


def test_ported_functions_exist_with_lisp_docstrings():
    source = (REPO / "src" / "codelets.lisp").read_text()
    defuns = set(re.findall(r"^\(defun ([^\s()]+)", source, re.M))
    for py_name, lisp_name in PORTED.items():
        assert lisp_name in defuns, lisp_name
        fn = getattr(codelets, py_name)
        assert inspect.getdoc(fn).startswith(f"codelets.lisp: {lisp_name}"), py_name

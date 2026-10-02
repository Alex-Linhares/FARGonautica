"""Loop0002 item 13: the solution checker, python/numbo/solution_checker.py.

A port of lisp/src/solution-checker.lisp (not 1987 source), checked against
fixtures/solution_checker.json, which lisp/tests/oracle/solution-checker.lisp
writes from the oracle's check-solution: the hand-written decompositions of
lisp/tests/solution-tests.lisp and its mutations, one text per way of being
invalid, the arithmetic forms, case and spacing variants, and one text per
value token (check-solution reads values with READ-FROM-STRING).

The checker's verdict on every full run (the 220 chapter runs and the error
runs) is compared with the oracle's in test_full_runs.py.  The runs at the
end of this file are lisp/tests/solution-tests.lisp's parts 2-4, with the seeds
remapped to oracle mode (the shared RNG gives other runs than SBCL's).
"""

import io
import re

import pytest

from conftest import load_fixture
from full_runs import LISP_DIR, REPO_DIR

from numbo import harness, solution_checker, trace
from numbo.solution_checker import check_solution, solution_tokens

FIXTURE = load_fixture("solution_checker.json")
CASES = FIXTURE["cases"]


def test_fixture_shape():
    assert len(CASES) >= 120
    labels = [c["label"] for c in CASES]
    assert len(set(labels)) == len(labels)
    assert {True, False} == {c["valid"] for c in CASES}
    # every FAIL format string of check-solution is reached
    reasons = " | ".join(c["reason"] for c in CASES if c["reason"])
    for text in ['no "Done :" in the output', "truncated operation paragraph",
                 'expected "', "not an integer value", "is derived twice",
                 "printed as both", 'no operation after "Done :"', "cycle through",
                 "there are only", "but brick", "is used twice",
                 "is used but never derived", "unknown operation", "cannot give"]:
        assert text in reasons, text


@pytest.mark.parametrize("case", CASES, ids=[c["label"] for c in CASES])
def test_check_solution_matches_oracle(case):
    got = check_solution(case["text"], case["problem"])
    assert got == (case["valid"], case["reason"], case["expression"])


@pytest.mark.parametrize("case", FIXTURE["tokens"], ids=range(len(FIXTURE["tokens"])))
def test_solution_tokens(case):
    assert solution_tokens(case["text"]) == case["tokens"]


def test_one_solution_tokens():
    """oracle.lisp's decomposition parser uses the checker's tokenizer, as
    the Lisp does (one definition)."""
    assert trace.solution_tokens is solution_checker.solution_tokens


def test_brick_index():
    assert solution_checker.brick_index("CYTO-BRICK3") == 3
    assert solution_checker.brick_index("cyto-brick12") == 12
    assert solution_checker.brick_index("CYTO-BRICK0") == 0
    for name in ["CYTO-BRICK", "CYTO-BRICK+1", "CYTO-BRICK1A", "CYTO-BLOCK3-V1", "BRICK1", ""]:
        assert solution_checker.brick_index(name) is None, name


# ---------------------------------------------------------------------------
# lisp/tests/solution-tests.lisp, part 1, with its literal expectations

SEED_18 = """Node PLUS28-3-V30 created
Done : Operation PLUS28-3-V30 has been applied
to CYTO-BLOCK28-V29 ( 28) and to CYTO-BRICK1 ( 3)
to get CYTO-TARGET
Operation TIMES2-14-V29 has been applied
to CYTO-BRICK5 ( 14) and to CYTO-BLOCK2-V28 ( 2)
to get CYTO-BLOCK28-V29
Operation PLUS2-3-V28 has been applied
to CYTO-BRICK4 ( 3) and to CYTO-BRICK2 ( 5)
to get CYTO-BLOCK2-V28
"""
P3 = [31, 3, 5, 24, 3, 14]


def reason_has(text, problem, part):
    valid, reason, expression = check_solution(text, problem)
    return not valid and expression is None and part in reason


def test_solution_tests_lisp_part_1():
    assert check_solution(SEED_18, P3) == (True, None, "31 = (14 x (5 - 3)) + 3")
    assert reason_has(SEED_18.replace("CYTO-BRICK4 ( 3)", "CYTO-BRICK1 ( 3)", 1), P3,
                      "CYTO-BRICK1 is used twice")
    assert reason_has(SEED_18.replace("CYTO-BRICK2 ( 5)", "CYTO-BRICK3 ( 5)", 1), P3,
                      "brick 3 is 24")
    assert reason_has(SEED_18.replace("CYTO-BLOCK28-V29 ( 28)", "CYTO-BLOCK28-V29 ( 27)", 1)
                      .replace("PLUS28-3", "PLUS27-3", 1), P3, "cannot give")
    assert reason_has(SEED_18, [32, 3, 5, 24, 3, 14], "cannot give")
    assert reason_has("Node CYTO-TARGET created", P3, 'no "Done :"')
    assert reason_has("Done : ", P3, "no operation")
    assert reason_has("Done : Operation PLUS2-3-V1 has been applied to", P3, "truncated")


# ---------------------------------------------------------------------------
# Census

def test_census():
    """One Python function per defun of lisp/src/solution-checker.lisp, each
    naming its Lisp origin."""
    source = (LISP_DIR / "src" / "solution-checker.lisp").read_text()
    defuns = re.findall(r"^\(cl:defun ([a-z-]+)", source, re.M)
    assert defuns == ["solution-tokens", "brick-index", "check-solution"]
    for name in defuns:
        fn = getattr(solution_checker, name.replace("-", "_"))
        assert f"solution-checker.lisp: {name}" in fn.__doc__


# ---------------------------------------------------------------------------
# lisp/tests/solution-tests.lisp, parts 2-4, on Python runs.  The expected
# outcomes are the oracle's (fresh-process oracle-mode runs; the full-run
# test compares every chapter run's output and verdict with the oracle).

def run(problem, seed, cap=20000):
    out = io.StringIO()
    result = harness.run_config(problem, seed=seed, max_iterations=cap, out=out)
    return result, out.getvalue()


def test_puzzle_3_solved():
    """Part 2: puzzle 3 seed 2 is the one valid oracle-mode solve of seeds
    1-20 (seed 18 in default mode)."""
    result, text = run(P3, 2)
    assert (result["outcome"], result["iterations"]) == ("solved", 7813)
    assert check_solution(text, P3) == (True, None, "31 = ((5 - 3) x 14) + 3")


def test_puzzle_3_kill_block_gap():
    """Part 3: a "Done :" the checker rejects (the kill-block gap,
    PORTING_NOTES item 10): seed 8 in oracle mode (seed 93 in default mode)."""
    result, text = run(P3, 8)
    assert (result["outcome"], result["iterations"]) == ("solved", 225)
    assert check_solution(text, P3) == (
        False, "CYTO-BLOCK11-V5 (11) is used but never derived, and is not a brick", None)
    assert "Node CYTO-BLOCK11-V5 killed" in text


@pytest.mark.parametrize("problem,expression", [
    ([6, 3, 3, 17, 11, 22], "6 = 3 + 3"),
    ([11, 2, 5, 1, 25, 23], "11 = (5 x 2) + 1"),
    ([114, 11, 20, 7, 1, 6], "114 = (6 x 20) - (7 - 1)"),
], ids=["7", "8", "1"])
def test_easy_puzzles(problem, expression):
    """Part 4: the chapter's easy puzzles, seed 1, cap 500."""
    result, text = run(problem, 1, 500)
    assert result["outcome"] == "solved"
    assert check_solution(text, problem) == (True, None, expression)

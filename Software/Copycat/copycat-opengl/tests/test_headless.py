"""Headless runs of Hager's Copycat engine: every classic problem finishes with an answer on
every seed, the same seed gives the same run, and the answers look like Copycat's.

Standard-library Python; run with `python3 -m pytest tests/test_headless.py` from this folder.
"""
import os
import subprocess
import sys

import pytest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, ROOT)

from copycat.run import Run  # noqa: E402

PROBLEMS = ["abc abd ijk", "abc abd xyz", "abc abd iijjkk", "abc abd kji",
            "abc abd mrrjjj", "abc abd xyd", "abc abd rst", "abc abd cba"]
SEEDS = range(1, 11)
CAP = 40000     # codelets; the longest run seen in 650 sweeps took 23,213


def solve(problem, seed, cap=CAP):
    a, b, c = problem.split()
    run = Run(a, b, c, seed)
    while not run.workspace.answer_string and run.coderack.time < cap:
        run.step()
    answer = run.workspace.answer_string.name if run.workspace.answer_string else None
    return answer, run.coderack.time, run.workspace.temperature


@pytest.mark.parametrize("problem", PROBLEMS)
def test_every_seed_finishes(problem):
    """The two Python 3 crashes (a dict view in flatten, a lazy map in the description scout)
    and the two upstream bugs (a typo, an empty average) made runs die; none may now."""
    for seed in SEEDS:
        answer, steps, _ = solve(problem, seed)
        assert answer, "%s seed %d: no answer in %d codelets" % (problem, seed, steps)
        assert len(answer) == len(problem.split()[2])


def test_deterministic():
    """DETERMINISM-FIX: the same seed gives the same answer, step count and temperature."""
    for problem, seed in [("abc abd xyz", 8), ("abc abd iijjkk", 4), ("abc abd kji", 2)]:
        assert solve(problem, seed) == solve(problem, seed)


def test_deterministic_across_processes():
    """The nondeterminism came from id()-ordered sets, which only show between processes."""
    cmd = [sys.executable, os.path.join(ROOT, "Copycat.py"), "--quiet", "abc", "abd", "xyz",
           "--seed", "8"]
    env = dict(os.environ, PYTHONWARNINGS="ignore")
    outs = [subprocess.run(cmd, cwd=ROOT, env=env, capture_output=True, text=True,
                           check=True).stdout for _ in range(2)]
    assert outs[0] == outs[1]
    assert "Answer: " in outs[0] and "Steps: " in outs[0]


def test_answers_look_like_copycat():
    """Coarse sanity on the distributions over 10 seeds, with wide margins: this is Hager's
    engine as he left it, not a replica of Mitchell's statistics."""
    answers = {p: [solve(p, s)[0] for s in SEEDS] for p in
               ["abc abd ijk", "abc abd xyz", "abc abd rst", "abc abd xyd"]}
    assert answers["abc abd ijk"].count("ijl") >= 9
    assert answers["abc abd rst"].count("rsu") >= 9
    assert answers["abc abd xyd"].count("xye") >= 9
    assert set(answers["abc abd xyz"]) <= {"xyd", "wyz", "yyz", "dyz", "xyy", "xyzz", "yyz"}
    assert "xyd" in answers["abc abd xyz"]


def test_snag_is_handled():
    """abc abd xyz always snags (z has no successor); the run must get past the snag, and
    the engine must have recorded it."""
    run = Run("abc", "abd", "xyz", 1)
    while not run.workspace.answer_string and run.coderack.time < CAP:
        run.step()
    assert run.workspace.answer_string
    assert run.workspace.snag_count >= 1


def test_rule_text():
    answer, _, _ = solve("abc abd ijk", 1)
    run = Run("abc", "abd", "ijk", 1)
    while not run.workspace.answer_string:
        run.step()
    text = run.workspace.rule.to_string()
    assert text.startswith("Replace ") and text.endswith(".")
    assert run.workspace.translated_rule.to_string().startswith("Replace ")

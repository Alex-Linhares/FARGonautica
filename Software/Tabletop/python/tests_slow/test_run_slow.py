"""The slow tests of `tests/test_run.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_run import (CFG, P, R, problems, pytest, runs_to_an_answer)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_many_seeds_on_three_problems_end_with_an_answer(problems):
    for name in ("surround-05", "buridan-01", "figure-ground-01"):
        for r in R.run_batch(problems[name], CFG, batch_seed=1, count=100):
            assert r.answer in problems[name].ids() and not r.capped


@pytest.mark.slow
def test_every_problem_runs_to_an_answer_with_one_seed(problems):
    """Seeded whole runs on all 53 digitized configurations (D-061), Figure 2.2 included:
    the touched cup alone reached the cap under v3's and v4's defaults (RESULTS.md §12, §13:
    403 and 484 of 500 runs), and since v5's time pressure (D-120) answers around 1,400
    codelets; no run may cap."""
    runs_to_an_answer(problems, sorted(problems))

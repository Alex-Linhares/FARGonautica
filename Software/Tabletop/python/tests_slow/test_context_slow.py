"""The slow tests of `tests/test_context.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_context import (problems, pushes_and_pops, pytest)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_every_problems_whole_run_pushes_and_pops_in_pairs_and_draws_nothing_for_it(problems):
    """Over the 53 problems with seed 1 (D-021)."""
    pushes_and_pops(list(problems.values()), list(problems.values())[:5])

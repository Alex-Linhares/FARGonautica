"""The slow tests of `tests/test_invariants.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_invariants import (CFG, PROFILES, T, catalogue, load_problem, no_faults, pytest)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_a_thousand_checked_runs_keep_every_invariant_and_none_is_capped():
    """The catalogue under every profile and random tables under the v2 defaults (`CFG`).
    Under v3 and v4 the orphan CLAIMED tags of RESULTS.md §12 were let through (`known`) and
    Figure 2.2, the touched cup alone, could reach the cap; since v5's fix (D-119, D-120)
    neither is tolerated: any violation or capped run fails."""
    profiles = list(PROFILES.values())
    names = catalogue()
    capped = 0
    for i in range(500):                       # the catalogue, every profile in turn
        problem = load_problem(names[i % len(names)])
        result = no_faults(problem, profiles[i % len(profiles)], 1000 + i)
        capped += result.capped
    for seed in range(1, 501):                 # random tables under the v2 defaults
        result = no_faults(T.random_table(seed), CFG, seed)
        capped += result.capped
    assert capped == 0


@pytest.mark.slow
def test_twenty_random_tables_keep_every_invariant_under_standard_and_five_under_french_faults():
    for seed in range(1, 21):
        result = no_faults(T.random_table(seed), CFG, seed)
        assert not result.capped
    for seed in range(21, 26):
        no_faults(T.random_table(seed), PROFILES["french-faults"], seed)

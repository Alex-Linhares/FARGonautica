"""The slow tests of `tests/test_calibrate.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_calibrate import (CA)

pytestmark = pytest.mark.slow


def test_the_human_scan_is_the_same_with_and_without_workers():
    grid = (30, 50)
    one = CA.human_scan(seed=5, runs=2, grid=grid, workers=1)
    two = CA.human_scan(seed=5, runs=2, grid=grid, workers=2)
    assert one == two
    assert one["grid"] == [30, 50] and one["field"] == "type_weight" and one["before"] == 34   # v4 (D-115; 26 in v3)
    assert [s["value"] for s in one["scan"]] == [30, 50]
    assert one["chosen"] in grid and len(one["scan"][0]["rows"]) == 12
    assert set(one["scan"][0]["surveys"]) == set(one["fit_problems"])
    lines = CA.report(one)
    assert lines[0].startswith("## The `human` fit") and any("| 30 |" in l for l in lines)

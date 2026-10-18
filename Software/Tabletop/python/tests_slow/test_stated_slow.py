"""The slow tests of `tests/test_stated.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_stated import (ST, STANDARD, json, pytest)
from v2_defaults import V2

pytestmark = pytest.mark.slow


def test_the_bench_at_a_few_runs_reads_every_statement():
    """Three runs per sample: 25 numbered checks, each with a verdict or nothing to read,
    the report's table and tally, the chapter 2 series, and JSON-able records (D-074)."""
    result = ST.run(STANDARD, 1, 3, progress=lambda sv: None)
    assert [c.number for c in result.checks] == list(range(1, 26))
    assert all(c.verdict in (True, False, None) for c in result.checks)
    assert all("[F95" in c.statement for c in result.checks)
    assert {c.profile for c in result.checks} == {"standard", "french-faults"}
    assert len(result.series) == 19 and result.runs == 3
    lines = ST.report(result)
    assert lines[0].startswith("## French's statements")
    assert sum(1 for l in lines if l.startswith("| ") and l.split("|")[1].strip().isdigit()) == 25
    yes, no, none = ST.tallied(result.checks)
    assert yes + no + none == 25
    assert any(l.endswith("statements agree with the runs.") or "statements agree with the runs" in l
               for l in lines)
    assert any(l.startswith("### Chapter 2's series") for l in lines)
    records = ST.records(result)
    json.dumps(records)
    assert sum(1 for r in records if "number" in r) == 25
    assert sum(1 for r in records if "sample" in r) == len(ST.SETS)
    assert sum(1 for r in records if r["bench"] == "ch2") == 19


@pytest.mark.slow
def test_the_statements_the_mechanisms_guarantee_hold_over_many_runs():
    """At 60 runs per sample the statements that the mechanisms settle (the first group a
    gestalt one, the run ending by find-answer, the focus nodes active, the milestones in
    French's order, Blockage 10's glasses never grouped, no nested same-type run under
    `standard`) hold; the others are findings the bench prints (D-074). On the v2 defaults
    (D-114): under v3's `proximity_zero_gap` of 4.9 glass widths Blockage 10's glasses can
    group (statement 20 then fails, a v3 finding the `stated` block of RESULTS.md shows)."""
    result = ST.run(V2, 1, 60, progress=lambda sv: None)
    by_number = {c.number: c for c in result.checks}
    for n in (1, 6, 10, 14, 20, 24):
        assert by_number[n].verdict is True, (n, by_number[n].numbers)
    yes, no, none = ST.tallied(result.checks)
    assert yes >= 10

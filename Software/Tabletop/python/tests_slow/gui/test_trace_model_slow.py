"""The slow tests of `tests/gui/test_trace_model.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_trace_model import (
    EventLog,
    PROFILES,
    TraceModel,
    dataclasses,
    load_problem,
    pytest,
    run_problem,
    time,
    tl,
)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_the_capped_run_is_fed_in_bounded_time():
    """G-007's forced capped run: the trace and the timeline models follow 30,692 events (since v5's fix, D-116 to D-120; 25,263 in v4; 24,774 recounted at the fast-engine loop's item 06, 24,744 being the roles loop's stale count)
    within a bound (PANES.md P10: the log stays responsive)."""
    forced = dataclasses.replace(PROFILES["standard"], answer_post_rate=0.0)
    history = EventLog()
    run_problem(load_problem("surround-05"), forced, 1, observers=[history])
    assert len(history) > 20000
    trace = TraceModel(history)
    timeline = tl.TimelineModel(history)
    start = time.perf_counter()
    trace.sync()
    timeline.sync()
    elapsed = time.perf_counter() - start
    assert elapsed < 20.0, elapsed
    assert trace.count > 3000 and "Codelets 3000 (capped)." in trace.lines[-1].text
    start = time.perf_counter()
    for position in range(0, len(history), 997):
        trace.line_of(position)
        trace.block_rows(position)
    assert time.perf_counter() - start < 5.0

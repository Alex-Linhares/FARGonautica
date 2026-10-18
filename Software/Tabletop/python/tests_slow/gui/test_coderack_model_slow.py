"""The slow tests of `tests/gui/test_coderack_model.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_coderack_model import (PROFILES, cr, dataclasses, pytest, tapped, time)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_a_capped_runs_history_stays_responsive():
    config = dataclasses.replace(PROFILES["standard"], answer_post_rate=0.0)
    log = tapped("surround-05", 1, config)
    assert log.events[-1].capped and log.events[-1].codelets == 3000
    model = cr.CoderackModel()
    start = time.monotonic()
    for i, event in enumerate(log.events):
        if i > 0:
            model.rebase(log, i - 1)
        model.on_event(event)
    fed_in = time.monotonic() - start
    start = time.monotonic()
    for _ in range(20):
        model.layout(900, 90)
        model.rows()
    drawn = (time.monotonic() - start) / 20
    assert model.mismatches == 0 and len(log.events) > 20000
    assert fed_in < 20.0, f"{fed_in:.1f} s to feed {len(log.events)} events"
    assert drawn < 0.05, f"{drawn * 1000:.1f} ms per layout and rows"

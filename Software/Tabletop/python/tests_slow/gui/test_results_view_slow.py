"""The slow tests of `tests/gui/test_results_view.py`, moved here so that the default suite
stays fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_results_view import (LineProbe, pytest, run_to_end, shown)

pytestmark = pytest.mark.slow


def test_the_line_is_painted_once_per_event_over_the_quick_start(shown, qapp):
    """The whole quick start (`surround-05` seed 3 on the v2 defaults, 720 events since
    v5's fix, D-116 to D-120; 507 since loop0007's item 08)
    through the window with the results dock floated: the line painted exactly once per
    event and saying the run at the end (the capped form is the fast suite's; the batch
    part of the line is that file's business, so only the run part is read here)."""
    w = shown
    probe = LineProbe(w)
    outcome = run_to_end(w, qapp, "surround-05", 3)
    w.event_drawn.disconnect(probe.drawn)
    assert outcome.result is not None and probe.checked == len(w.history) == 720    # surround-05 seed 3 since v5's fix, D-116 to D-120 (507 since item 08; 856 at item 07; 782 at item 05)
    assert probe.anomalies == []
    assert w.results_view.events == 720 and w.results_view.line.state == "finished"
    assert w.results_view.line.text().endswith(" · finished: 720 events")

"""The slow tests of `tests/gui/test_log_view.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_log_view import (
    EVERY,
    PROBES,
    Probe,
    QUICK,
    SAMPLED,
    at_end,
    dated_models,
    event_models,
    pytest,
    qtbot_wait,
    ran,
    run_to_end,
    trace_lines,
)

pytestmark = pytest.mark.slow


def test_seeking_is_refused_while_a_run_is_live(ran, module_window, qapp, qtbot):
    """Numbo's rule: a live run (paused after one step) refuses seeks and disables the
    timeline; the log's clicks go through `seek` too. Last in the module: it starts a new
    run of the same problem and seed, to the end."""
    w = module_window
    at_end(w, qapp)
    # Dock the log dock again first: the probes are off, and a second run with the dock
    # floating at 1500 by 420 paints its lists in a window of their own at every event,
    # which costs ten seconds under load for nothing this test reads.
    w.log_dock.setFloating(False)
    qapp.processEvents()
    w.step()
    qtbot.waitUntil(lambda: w.position == 0 and w.controller.state == "paused", timeout=5000)
    assert w.controller.is_alive()
    assert not w.seek(0) and not w.timeline.slider.isEnabled() and not w.timeline.strip.scrubbable
    assert not w.jump_to_codelet(1)
    w.timeline.back_button.click()
    assert w.position == 0
    assert not w.save_trace_action.isEnabled() or w.history.events
    with qtbot.waitSignal(w.run_finished, timeout=20000):
        w.run_to_end()
    assert w.controller.wait(5000) and not w.controller.is_alive()
    assert w.timeline.slider.isEnabled() and w.timeline.strip.scrubbable
    assert w.seek(5) and w.position == 5
    at_end(w, qapp)

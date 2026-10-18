"""The slow tests of `tests/gui/test_main_window.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_main_window import (PROFILES, SHORT, TIMEOUT, headless)

pytestmark = pytest.mark.slow


def test_headless_through_the_window_equals_the_plain_run(qtbot, thread_errors, golden_runs):
    plain = golden_runs[("blockage-06", 2)]
    got = headless.run("blockage-06", PROFILES["standard"], 2, window=True)
    assert got == {"records": list(plain.records), "draws": plain.draws, "report": plain.report}


def test_the_run_actions_drive_the_controller(window, qtbot):
    window.set_next_run(problem=SHORT[0], seed=SHORT[1])
    window.set_delay(0.2)
    window.play_pause()                       # starts a run of the next-run inputs, playing
    qtbot.waitUntil(lambda: window.state in ("running", "finished"), timeout=TIMEOUT)
    assert window.started == (SHORT[0], "standard", SHORT[1])
    window.play_pause()                       # pauses it, if it is still going
    qtbot.waitUntil(lambda: window.state in ("paused", "finished"), timeout=TIMEOUT)
    position = window.position
    window.step_codelet()                     # to the end of the start, or of a codelet
    qtbot.waitUntil(lambda: window.position > position or window.state == "finished",
                    timeout=TIMEOUT)
    window.stop()
    assert not window.controller.is_alive()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    window.new_run()                          # Ctrl+N: a fresh run, paused at its first event
    # The "paused" state follows the first event's delivery (the gate pauses at its next
    # wait), so wait for the state as well as the position (G-058).
    qtbot.waitUntil(lambda: window.position == 0 and window.state == "paused", timeout=TIMEOUT)
    assert len(window.history) == 1
    window.run_to_end()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert len(window.history) == 1521           # buridan-03 seed 2 since v5's fix, D-116 to D-120 (1571 in v4, D-115; 1144 in v3, D-114; 913 since item 09; every run changed at items 03, 04, 05, 07 and 09; 495 at items 07 and 08; 687 at item 05)

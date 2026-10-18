"""The slow tests of `tests/gui/test_config_view.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_config_view import (
    Config,
    PROFILES,
    QColor,
    QUICK,
    TIMEOUT,
    dataclasses,
    hidden_for_a_run,
    is_reddish,
    plain_run,
    pytest,
    qtbot_wait,
    row_of,
    row_pixels,
    run_plain,
    run_to_end,
    shown,
    shown_again,
    spin,
    sv,
    title_for,
    window,
)

pytestmark = pytest.mark.slow


def test_an_edit_during_a_run_leaves_its_records_unchanged(qtbot, window):
    w, view = window, window.config_view
    hidden_for_a_run(w, qtbot)
    w.step()                                        # a run paused at its first event
    qtbot.waitUntil(lambda: w.state == "paused", timeout=TIMEOUT)
    assert w.history.events and w.controller.is_alive()
    spin(view, "max_codelets", 20)
    assert w.next_config().max_codelets == 20
    with qtbot.waitSignal(w.run_finished, timeout=TIMEOUT):
        w.controller.run_to_end()
    qtbot.waitUntil(lambda: not w.controller.is_alive(), timeout=TIMEOUT)
    run = plain_run(*QUICK)
    assert list(w.history.records) == list(run.records)
    assert w.outcome.result == run.result and w.outcome.result.codelets == 384   # v5's fix, D-116 to D-120 (305 in v4, D-115; 144 in v3, D-114; 44 since item 08; 77 at item 07; 67 at item 05)
    assert w.windowTitle() == title_for("surround-05", "standard", 3)
    assert w.stats_view.text("config") == "standard (the defaults)"
    assert view.status.text() == ("next run: standard: max_codelets=20 · edited: max_codelets "
                                  "· run in progress: standard (the defaults)")
    shown_again(w, qtbot)


def test_the_next_run_uses_the_edit_and_the_stats_pane_and_the_title_say_so(qtbot, window):
    w, view = window, window.config_view
    spin(view, "max_codelets", 20)
    assert not view.status.visibleRegion().isEmpty()
    counts = []
    w.event_drawn.connect(lambda _index: counts.append(view.paints))
    try:
        outcome = run_to_end(qtbot, w)
    finally:
        w.event_drawn.disconnect()
    assert outcome.result.capped and outcome.result.codelets == 20
    # the status line is painted once per event while the dock is on screen (G-011)
    assert len(counts) == len(w.history) > 100
    assert all(b - a == 1 for a, b in zip(counts, counts[1:]))
    assert view.events == len(w.history)
    start = w.history.records[0]
    assert start["event"] == "RunStarted" and start["changed"] == (("max_codelets", "20"),)
    expected = run_plain(dataclasses.replace(Config(), max_codelets=20))
    assert (list(w.history.records), w.history.records[-1]["draws"], outcome.result) == expected
    assert w.windowTitle() == title_for("surround-05", "standard", 3, ("max_codelets",))
    assert w.started == ("surround-05", "standard", 3)
    stats = w.stats_view
    assert stats.text("config") == "standard: max_codelets=20"
    assert stats.text("run") == "surround-05, seed 3"
    assert view.status.text() == ("next run: standard: max_codelets=20 · edited: max_codelets "
                                  "· run in progress: standard: max_codelets=20")
    w.stats_dock.setFloating(True)
    w.stats_dock.resize(520, 420)
    qtbot.wait(40)
    try:
        colour = row_pixels(stats, row_of(stats, "config"))
        edited = QColor(sv.EDITED)
        assert abs(colour.red() - edited.red()) < 12 and abs(colour.green() - edited.green()) < 12
        assert abs(colour.blue() - edited.blue()) < 12       # the edited colour, not ink or red
        assert not is_reddish(row_pixels(stats, row_of(stats, "run")))
    finally:
        w.stats_dock.setFloating(False)
    # a run under a profile with an edit, started with the profile's own name kept
    view.profile_combo.setCurrentIndex(view.profile_combo.findData("human"))
    hidden_for_a_run(w, qtbot)
    outcome = run_to_end(qtbot, w)
    shown_again(w, qtbot)
    assert w.history.records[0]["profile"] == "human"
    assert w.history.records[0]["changed"] == (("max_codelets", "20"), ("profile", "'human'"),
                                               ("type_weight", "50"))
    assert w.windowTitle() == title_for("surround-05", "human", 3, ("max_codelets",))
    assert w.windowTitle().endswith("(human; edited: max_codelets)")
    assert stats.text("config") == "human: max_codelets=20, type_weight=50"
    assert (list(w.history.records), outcome.result) == run_plain(
        dataclasses.replace(PROFILES["human"], max_codelets=20))[::2]

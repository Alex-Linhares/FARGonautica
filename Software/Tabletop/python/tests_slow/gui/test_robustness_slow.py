"""The slow tests of `tests/gui/test_robustness.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from golden import V2
from test_robustness import (
    CAPPED_BOUND_S,
    CAPPED_EVENTS,
    CAPPED_SLOWDOWN,
    CAPPED_WINDOW,
    Collector,
    PANES,
    PROFILES,
    PixelProbe,
    QDockWidget,
    QLabel,
    QMainWindow,
    QTest,
    QUICK,
    Qt,
    SHORT,
    SHORT_EVENTS,
    SIZE,
    SMALLEST_SIZE,
    STATE_VERSION,
    TIMEOUT,
    dataclasses,
    gc,
    ini_settings,
    load_problem,
    make_window,
    plain_run,
    pytest,
    raise_config,
    raise_first_tabs,
    raise_fourth_tabs,
    raise_roles,
    raise_second_tabs,
    raise_third_tabs,
    record_of,
    run_problem,
    run_to_end,
    sys,
    time,
    w,
)

pytestmark = pytest.mark.slow


def test_every_views_pixels_are_the_events_at_sampled_events(w, qtbot):
    """The quick start's run at full speed, checked every 50th event (fifteen samples of
    its 720 events since v5's fix, D-116 to D-120; eleven of 507 since item 08; eighteen
    of 856 at item 07; sixteen of 782 at item 05; nineteen of 946 at item 04): what
    the window shows of every on-screen painting widget equals a fresh render of it.
    The hidden tabs are never checked."""
    probe = PixelProbe(w, every=50)
    try:
        run_to_end(qtbot, w, *QUICK)
    finally:
        probe.detach()
    assert probe.checked == 15              # every 50th of 720 events since v5's fix, D-116 to D-120 (11 of 507 since item 08; 18 of 856 at item 07; 16 of 782 at item 05; 19 of 946 at item 04)
    assert probe.mismatches == []
    assert {"slipnet-network", "competitions", "rack-list", "results-graph",
            "temperature-plot", "config-status-line", "events"} <= probe.skipped
    assert "table" not in probe.skipped and "slipnet-grid" not in probe.skipped


PROBED_TIMEOUT = 4 * TIMEOUT   # ms: a run probed at every event renders every widget afresh
                               # 3861 times since v5's fix, D-116 to D-120 (3231 in v4; 507 since item 08; 856 at item 07; 782 at item 05), over 30 s on a loaded machine


def run_to_end_probed(qtbot, window, problem, seed):
    """`run_to_end` with a wait sized to a run probed at every event (G-058)."""
    with qtbot.waitSignal(window.run_finished, timeout=PROBED_TIMEOUT):
        window.start_run(problem, PROFILES["standard"], seed)
        window.controller.run_to_end()
    return window.outcome


@pytest.mark.slow
def test_every_views_pixels_are_the_events_after_each_event(w, qtbot):
    """The quick start's run after every one of its 3861 events (v5's fix, D-116 to D-120; 3231 in v4; 1520 in v3; 507 at v2), under the built-in
    tabs and then under the second tabs; then the temperature plot, the configuration
    pane, the Proposals tab and the Roles pane (tabbed under the Worldview's, G-060), each
    at ten seeks."""
    probe = PixelProbe(w)
    try:
        run_to_end_probed(qtbot, w, *QUICK)
        assert probe.checked == 3861 and probe.mismatches == []     # v5's quick start, D-116 to D-120 (3231 in v4, D-115; 1520 in v3; 507 since item 08)
        first = set(probe.widgets) - probe.skipped
        raise_second_tabs(w)
        qtbot.waitUntil(lambda: not w.rack_view.list.viewport().visibleRegion().isEmpty())
        probe.checked, probe.skipped = 0, set()
        run_to_end_probed(qtbot, w, *QUICK)
        assert probe.checked == 3861 and probe.mismatches == []     # v5's quick start, D-116 to D-120 (3231 in v4, D-115; 1520 in v3; 507 since item 08)
        second = set(probe.widgets) - probe.skipped
        assert {"slipnet-network", "competitions", "rack-list", "results-graph"} <= second
        missing = set(probe.widgets) - (first | second) - {
            "temperature-plot", "temperature-view", "config-view", "config-status-line",
            "events", "worldview-proposals", "roles-lines", "roles-view"}
        assert not missing, missing
        n = len(w.history)
        for arrange, name in ((raise_third_tabs, "temperature-plot"),
                              (raise_config, "config-status-line"),
                              (raise_fourth_tabs, "worldview-proposals"),
                              (raise_roles, "roles-lines")):
            arrange(w)
            qtbot.waitUntil(lambda: not probe.widgets[name].visibleRegion().isEmpty())
            probe.skipped = set()
            for k in range(10):
                assert w.seek(n * k // 10 + 7)
                probe.drawn(0)
            assert probe.mismatches == []
            assert name not in probe.skipped
    finally:
        probe.detach()
        raise_first_tabs(w)
        w.config_dock.hide()


def test_no_memory_growth_from_run_to_run(w, qtbot):
    """A short library run (buridan-03 seed 2; v1's shortest) five times in one window. The history and every model
    are per run, so after the first run (the caches warm up) the memory held must stay
    flat: a leak per event or per run would show in the blocks Python's allocator holds
    and in the objects alive (`tracemalloc` would slow the engine five-fold, G-007, so the
    allocator's own count is read instead; G-058 records the measurement)."""
    blocks, objects, items = [], [], []
    for _ in range(5):
        run_to_end(qtbot, w, *SHORT)
        qtbot.wait(20)
        gc.collect()
        blocks.append(sys.getallocatedblocks())
        objects.append(len(gc.get_objects()))
        items.append(len(w.table_view.scene().items()))
    print(f"five runs: objects {objects}, blocks {blocks}, scene items {items}")
    assert len(w.history) == SHORT_EVENTS
    assert len(set(items)) == 1
    assert max(objects[1:]) - min(objects[1:]) < 200, objects
    growth = (blocks[-1] - blocks[1]) / (len(blocks) - 2)
    assert growth < 500, blocks          # blocks a run, once warm


@pytest.mark.slow
def test_the_capped_run_goes_through_the_window_at_max_speed_within_the_bound(window, qtbot):
    """`surround-05` seed 1 with the answer switched off (G-007's measurement run) reaches
    the cap of 3000 codelets: every one of its 30,692 events (since v5's fix, D-116 to D-120; 25,263 in v4; 24,774 recounted at the fast-engine loop's item 06) drawn, within
    CAPPED_BOUND_S, equal to the plain run, and the GUI thread's CPU time for the last
    CAPPED_WINDOW events at most CAPPED_SLOWDOWN times the first window's (no cost that
    grows with the run)."""
    forced = dataclasses.replace(PROFILES["standard"], answer_post_rate=0.0)
    stamps = []
    window.event_drawn.connect(lambda index: stamps.append(time.thread_time()))
    started = time.perf_counter()
    with qtbot.waitSignal(window.run_finished, timeout=int(CAPPED_BOUND_S * 1000) + 60000):
        window.start_run("surround-05", forced, 1)
        window.controller.run_to_end()
    elapsed = time.perf_counter() - started
    assert len(window.history) == CAPPED_EVENTS
    result = window.outcome.result
    assert result.codelets == 3000 and result.capped
    plain = Collector()
    assert run_problem(load_problem("surround-05"), forced, 1, [plain]) == result
    assert list(window.history.records) == [record_of(e) for e in plain.events]
    assert elapsed < CAPPED_BOUND_S, f"{elapsed:.1f} s for {CAPPED_EVENTS} events"
    assert len(stamps) == CAPPED_EVENTS
    first = stamps[CAPPED_WINDOW] - stamps[0]
    last = stamps[-1] - stamps[-1 - CAPPED_WINDOW]
    print(f"capped run through the window: {elapsed:.1f} s wall, {1000 * elapsed / CAPPED_EVENTS:.1f} "
          f"ms an event; GUI-thread CPU time of the first {CAPPED_WINDOW} events {first:.1f} s, "
          f"of the last {last:.1f} s")
    assert last <= CAPPED_SLOWDOWN * first, (first, last, elapsed)


def test_a_run_goes_through_at_1280_by_800_with_every_dock_shown(w, qtbot):
    """Every dock shown (the configuration pane in its tab group), the window at
    SMALLEST_SIZE: its minimum fits, every dock is on the window or in a tab, and the
    short library run (buridan-03 seed 2) goes through at that size, drawn as always."""
    for dock in w.docks.values():
        dock.show()
    qtbot.waitUntil(lambda: not any(d.isHidden() for d in w.docks.values()))
    w.resize(*SMALLEST_SIZE)
    qtbot.waitUntil(lambda: (w.width(), w.height()) == SMALLEST_SIZE, timeout=TIMEOUT)
    hint = w.minimumSizeHint()
    assert (hint.width(), hint.height()) <= SMALLEST_SIZE, (hint.width(), hint.height())
    run_to_end(qtbot, w, *SHORT)
    assert list(w.history.records) == list(plain_run(*SHORT, V2).records)
    assert (w.width(), w.height()) == SMALLEST_SIZE
    for _attr, name, title, *_ in PANES:
        dock = w.docks[name]
        assert dock.isVisible() and not dock.isFloating(), title
        if dock.visibleRegion().isEmpty():
            assert w.tabifiedDockWidgets(dock), title
        else:
            assert w.rect().contains(dock.geometry()), title
    w.config_dock.hide()


def test_a_state_naming_other_docks_leaves_every_pane_reachable(qtbot, ini, thread_errors, capfd):
    """A state of this version from a window with other docks (one of ours missing, one
    unknown): whatever Qt makes of it, every pane is on the window or in a tab, its
    View action enabled, the menu bar is clear, and a run goes through."""
    other = QMainWindow()
    for name in ("controls-dock", "old-dock", "log-dock"):
        dock = QDockWidget(name, other)
        dock.setObjectName(name)
        dock.setWidget(QLabel(name))
        other.addDockWidget(Qt.DockWidgetArea.LeftDockWidgetArea, dock)
    other.show()
    qtbot.addWidget(other)
    state = other.saveState(STATE_VERSION)
    other.close()
    s = ini_settings(ini)
    s.setValue("window/state", state)
    s.sync()
    w = make_window(qtbot, ini_settings(ini))
    try:
        for _attr, name, title, _pane, _pages, _area, _under, shown_ in PANES:
            dock = w.docks[name]
            if shown_:
                assert dock.isVisible(), title
                assert not dock.visibleRegion().isEmpty() or w.tabifiedDockWidgets(dock), title
            assert dock.toggleViewAction().isEnabled(), title
        bar = w.menuBar()
        centre = bar.actionGeometry(bar.actions()[0]).center()
        assert w.childAt(bar.mapTo(w, centre)) is bar
        run_to_end(qtbot, w, *SHORT)
        assert list(w.history.records) == list(plain_run(*SHORT, V2).records)
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err

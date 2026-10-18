"""Loop0004 item 18: robustness, after the Numbo GUI's `tests/gui/test_polish.py`.
pytest-qt, offscreen.

- The pixel probe: after each event of run 1, what the window shows of every painting
  widget of every on-screen view equals a fresh render of it (the incremental paints,
  the blitted scrolls and the cached pictures leave nothing stale) (G-060, G-061).
- Memory over six consecutive runs of run 1 in one window stays flat once warm (G-061).
- Closing the window mid-run (a run playing, an interactive run waiting for a term, a
  guess pending) stops and joins the worker (G-008, G-015).
- `blackburn-11` (16 terms, 10,110 tasks, 28,959 events) runs to the end at max speed
  within a time bound, with no slowdown over the run (G-061).
- The window fits 1280 x 800 with every dock shown, and a run goes through at that size
  (G-058).
- A `window/state` saved by another version of the window's docks is ignored, the
  built-in layout used, and a state naming other docks leaves every pane reachable
  (G-059).
"""

import gc
import os
import threading
import time
import tracemalloc

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings, Qt                         # noqa: E402
from PySide6.QtGui import QImage                                 # noqa: E402
from PySide6.QtWidgets import QDockWidget, QLabel, QMainWindow   # noqa: E402

from seekwhence.gui.main_window import DEFAULT_SIZE, PANES, SMALLEST_SIZE, STATE_VERSION  # noqa: E402
from seekwhence.gui.models import presets                        # noqa: E402

from golden import APPENDIX                                      # noqa: E402
from test_main_window import plain, records_of, run_to_end, make_window  # noqa: E402

TIMEOUT = 30000           # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)

# blackburn-11 through the window at max speed: 28,959 events, every one drawn.
# Measured 129 s (4.4 ms an event) on an idle machine after G-060; the bound is 12 ms an
# event, room for a loaded machine (iteration 16 saw 2x under load) (G-061).
BB11_EVENTS = 28959
BB11_BOUND_S = 0.012 * BB11_EVENTS
BB11_WINDOW = 5000        # events: the first window against the last, CPU time
BB11_SLOWDOWN = 2.0       # the last window may cost this much more than the first


# -- the pixel probe (Numbo GUI, test_polish.py) -------------------------------------------------

def shown(w, widget):
    """What the window's backing store holds for the visible part of `widget` (what was
    painted, not a fresh render: the offscreen screen grabs the backing store)."""
    r = widget.visibleRegion().boundingRect()
    top = widget.mapTo(w, r.topLeft())
    img = w.screen().grabWindow(w.winId(), top.x(), top.y(), r.width(), r.height()).toImage()
    return img.convertToFormat(QImage.Format.Format_RGB32)


def fresh(widget):
    """A fresh render of the visible part of `widget` (its children included)."""
    r = widget.visibleRegion().boundingRect()
    return widget.grab(r).toImage().convertToFormat(QImage.Format.Format_RGB32)


def same_picture(a, b, tolerance=2):
    """Equal, or no channel of any pixel more than `tolerance` apart (an antialiased
    edge can round one step differently in the backing store than in a grab)."""
    if a == b:
        return True
    if a.size() != b.size():
        return False
    da = bytes(a.constBits())[:a.sizeInBytes()]
    db = bytes(b.constBits())[:b.sizeInBytes()]
    return all(abs(x - y) <= tolerance for x, y in zip(da, db))


def painting_widgets(w):
    """Every widget that paints a model, by name: the views' own painters and lists,
    the timeline's strips, and each whole view (its labels and buttons with it)."""
    widgets = {
        "sequence-strip": w.sequence_view.strip,
        "cytoplasm": w.cytoplasm_view.view.viewport(),
        "stats": w.stats_view,
        "taskrack-rack": w.taskrack_view.rack.viewport(),
        "taskrack-history": w.taskrack_view.list.viewport(),
        "socratoplasm-strip": w.socratoplasm_view.strip,
        "socratoplasm-cards": w.socratoplasm_view.list.viewport(),
        "hypothesis-tree": w.hypothesis_view.tree,
        "hypothesis-validity": w.hypothesis_view.validity_bar,
        "hypothesis-predictions": w.hypothesis_view.strip,
        "hypothesis-list": w.hypothesis_view.list.viewport(),
        "platoplasm-network": w.platoplasm_view.network,
        "platoplasm-list": w.platoplasm_view.list.viewport(),
        "reformulation-plot": w.reformulation_view.plot,
        "reformulation-story": w.reformulation_view.list.viewport(),
        "console": w.console_view.list.viewport(),
        "log": w.log_view.list.viewport(),
        "timeline-markers": w.timeline.markers,
        "timeline-parses": w.timeline.parses,
    }
    for attr, *_ in PANES:
        if attr != "controls":
            widgets[f"{attr}-view"] = getattr(w, f"{attr}_view")
    widgets["sequence-view"] = w.sequence_view
    widgets["cytoplasm-view"] = w.cytoplasm_view
    return widgets


class PixelProbe:
    """At each `event_drawn`: every on-screen painting widget's painted pixels equal a
    fresh render of it. Set SEEKWHENCE_PIXEL_DEBUG to save the first mismatch's pair
    under /tmp."""

    def __init__(self, w, every=1):
        self.w, self.every = w, every
        self.widgets = painting_widgets(w)
        self.mismatches = []
        self.checked = 0
        self.skipped = set()
        w.event_drawn.connect(self.drawn)

    def drawn(self, index):
        if index % self.every:
            return
        self.checked += 1
        for name, widget in self.widgets.items():
            if widget.visibleRegion().isEmpty():
                self.skipped.add(name)
                continue
            a, b = shown(self.w, widget), fresh(widget)
            if not same_picture(a, b):
                if not self.mismatches and os.environ.get("SEEKWHENCE_PIXEL_DEBUG"):
                    a.save(f"/tmp/swgui-mismatch-{name}-shown.png")
                    b.save(f"/tmp/swgui-mismatch-{name}-fresh.png")
                self.mismatches.append((index, name))


def test_every_views_pixels_are_the_events_after_each_event_of_run_1(window, qtbot):
    """Run 1 at full speed: after each of its 174 events (before its acknowledgment),
    what the window shows of every on-screen painting widget equals a fresh render of
    it [M86 pp. 164-166]."""
    probe = PixelProbe(window)
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert probe.checked == 174
    assert probe.mismatches == []
    # The log and the reformulation are tabbed away by default and never checked.
    assert {"log", "log-view", "reformulation-story"} <= probe.skipped
    assert "console" not in probe.skipped and "cytoplasm" not in probe.skipped


def test_the_pixels_stay_right_with_the_tabs_switched_and_a_seek(window, qtbot):
    """Run 3 with the event log and the reformulation raised, checked every fifth
    event, then every pane at ten seeks."""
    window.log_dock.raise_()
    window.reformulation_dock.raise_()
    qtbot.waitUntil(lambda: not window.log_view.list.viewport().visibleRegion().isEmpty())
    probe = PixelProbe(window, every=5)
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    assert probe.checked == 121 and probe.mismatches == []
    assert "log" not in probe.skipped and "reformulation-story" not in probe.skipped
    n = len(window.history)
    for k in range(10):
        assert window.seek(n * k // 10 + 7)
        probe.drawn(0)
    assert probe.mismatches == []


# -- memory (Numbo GUI, test_polish.py) ----------------------------------------------------------

def test_no_memory_growth_from_run_to_run(qtbot, window):
    """Run 1 six times in one window. The history and every model are per run, so
    after the first run (caches warm up) the memory held must stay flat: a leak per
    event (174 events a run) or per run would show. Measured: 4 objects and 2.7 KB a
    run (G-061)."""
    sizes, objects, items = [], [], []
    tracemalloc.start()
    try:
        for _ in range(6):
            run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
            qtbot.wait(20)
            gc.collect()
            sizes.append(tracemalloc.get_traced_memory()[0])
            objects.append(len(gc.get_objects()))
            items.append(len(window.cytoplasm_view.scene().items()))
    finally:
        tracemalloc.stop()
    assert len(window.history) == 174
    assert len(set(items)) == 1
    assert max(objects[1:]) - min(objects[1:]) < 50, objects
    growth = (sizes[-1] - sizes[1]) / (len(sizes) - 2)
    assert growth < 24 * 1024, sizes


# -- closing mid-run (G-008, G-015) --------------------------------------------------------------

def test_closing_while_a_run_plays_stops_and_joins_the_worker(qtbot, thread_errors):
    w = make_window(qtbot)
    w.controller.set_delay(0.02)
    w.start_session(presets.appendix_inputs(RUN3))
    w.controller.play()
    qtbot.waitUntil(lambda: w.position >= 5, timeout=TIMEOUT)
    assert w.controller.is_alive()
    before = threading.active_count()
    w.close()
    assert not w.controller.is_alive()
    assert threading.active_count() < before
    assert w.controller.state == "stopped"


def test_closing_while_an_interactive_run_waits_for_a_term_joins_the_worker(qtbot, thread_errors):
    w = make_window(qtbot)
    w.start_session(presets.interactive_inputs(54))
    w.controller.run_to_end()
    qtbot.waitUntil(lambda: w.sequence_view.input_edit.isEnabled(), timeout=TIMEOUT)
    assert w.controller.is_alive() and w.feed is not None
    w.close()
    assert not w.controller.is_alive()
    assert w.controller.state == "stopped"


def test_closing_with_a_guess_pending_joins_the_worker(qtbot, thread_errors):
    w = make_window(qtbot)
    w.start_session(presets.appendix_inputs(RUN1, interactive=True))
    w.controller.run_to_end()
    qtbot.waitUntil(lambda: w.guess_panel.waiting, timeout=TIMEOUT)
    assert w.controller.is_alive()
    w.close()
    assert not w.controller.is_alive()
    assert not w.guess_panel.waiting


# -- a long run: blackburn-11 at max speed (G-061) --------------------------------------------------

@pytest.mark.slow
def test_blackburn_11_runs_to_the_end_at_max_speed_within_the_bound(window, qtbot):
    """The longest of the Blackburn dozen as the bench runs it [M86 pp. 6-7, 145]: every
    event drawn, within BB11_BOUND_S, and the GUI thread's CPU time for the last
    BB11_WINDOW events at most BB11_SLOWDOWN times the first window's (no cost that
    grows with the run)."""
    inputs = presets.sequence_inputs("blackburn-11", 1, judged=True)
    stamps = []
    window.event_drawn.connect(lambda index: stamps.append(time.thread_time()))
    started = time.perf_counter()
    with qtbot.waitSignal(window.run_finished, timeout=int(BB11_BOUND_S * 1000) + 60000):
        window.start_session(inputs)
        window.controller.run_to_end()
    elapsed = time.perf_counter() - started
    assert len(window.history) == BB11_EVENTS
    assert window.outcome.result["summary"] == (
        "outcome: no-terms after 16 terms, 10110 tasks (seed 1): no hypothesis")
    assert records_of(window) == plain(inputs, answers=[
        e.answer for e in window.history.events if e.kind == "answered"])[0]
    assert elapsed < BB11_BOUND_S, f"{elapsed:.1f} s for {BB11_EVENTS} events"
    assert len(stamps) == BB11_EVENTS
    first = stamps[BB11_WINDOW] - stamps[0]
    last = stamps[-1] - stamps[-1 - BB11_WINDOW]
    assert last <= BB11_SLOWDOWN * first, (first, last, elapsed)


# -- the smallest window (G-058) ------------------------------------------------------------------

def test_the_window_fits_1280_by_800_with_every_dock_shown(qtbot, window):
    for dock in window.docks.values():
        dock.show()
    qtbot.waitUntil(lambda: not any(d.isHidden() for d in window.docks.values()))
    window.resize(*SMALLEST_SIZE)
    qtbot.waitUntil(lambda: (window.width(), window.height()) == SMALLEST_SIZE,
                    timeout=TIMEOUT)
    hint = window.minimumSizeHint()
    assert (hint.width(), hint.height()) <= SMALLEST_SIZE, (hint.width(), hint.height())
    # Every dock is on the window, tabbed ones through their tab; the hidden one
    # (Configuration) came up in its tab group.
    for _attr, name, title, *_ in PANES:
        dock = window.docks[name]
        assert dock.isVisible() and not dock.isFloating(), title
        if dock.visibleRegion().isEmpty():      # under another tab: parked off the window
            assert window.tabifiedDockWidgets(dock), title
        else:
            assert window.rect().contains(dock.geometry()), title
    assert window.config_dock in window.tabifiedDockWidgets(window.hypothesis_dock)
    # The run goes through at that size, drawn as always.
    inputs = presets.appendix_inputs(RUN1)
    run_to_end(qtbot, window, inputs)
    assert records_of(window) == plain(inputs)[0]
    assert (window.width(), window.height()) == SMALLEST_SIZE


def test_the_default_size_is_not_smaller_than_the_smallest():
    """(The offscreen screen is smaller than both, so the window opens at its minimum
    there.)"""
    assert DEFAULT_SIZE >= SMALLEST_SIZE


# -- settings from another version of the layout (G-059) --------------------------------------------

def ini_settings(ini):
    return QSettings(ini, QSettings.Format.IniFormat)


@pytest.mark.parametrize("version", [STATE_VERSION - 1, STATE_VERSION + 1])
def test_a_state_saved_by_another_version_is_ignored(qtbot, ini, thread_errors, capfd, version):
    """`window/state` as a window of another dock arrangement wrote it: Qt refuses it,
    the status bar says so, the built-in layout is used, and closing saves the current
    version over it."""
    w = make_window(qtbot, ini_settings(ini))
    w.platoplasm_dock.hide()
    other = w.saveState(version)
    w.close()
    s = ini_settings(ini)
    s.setValue("window/state", other)
    s.sync()
    w2 = make_window(qtbot, ini_settings(ini))
    try:
        assert w2.layout_source == "built-in"
        assert not w2.platoplasm_dock.isHidden()
        assert "another version" in w2.statusBar().currentMessage()
    finally:
        w2.close()
    assert "Traceback" not in capfd.readouterr().err
    w3 = make_window(qtbot, ini_settings(ini))
    try:
        assert w3.layout_source == "settings"
    finally:
        w3.close()


def test_a_state_naming_other_docks_leaves_every_pane_reachable(qtbot, ini, thread_errors, capfd):
    """A state of this version from a window with other docks (one of ours missing,
    one unknown): whatever Qt makes of it, every pane is on the window or in a tab,
    the menu bar is clear, and a run goes through."""
    other = QMainWindow()
    for name in ("controls-dock", "old-dock", "console-dock"):
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
                tabbed = w.tabifiedDockWidgets(dock)
                assert not dock.visibleRegion().isEmpty() or tabbed, title
            assert dock.toggleViewAction().isEnabled(), title
        bar = w.menuBar()
        centre = bar.actionGeometry(bar.actions()[0]).center()
        assert w.childAt(bar.mapTo(w, centre)) is bar
        inputs = presets.appendix_inputs(RUN1)
        run_to_end(qtbot, w, inputs)
        assert records_of(w) == plain(inputs)[0]
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err


def test_a_state_that_is_not_bytes_is_ignored(qtbot, ini, thread_errors, capfd):
    s = ini_settings(ini)
    s.setValue("window/state", "an old text layout")
    s.setValue("window/geometry", 12)
    s.sync()
    w = make_window(qtbot, ini_settings(ini))
    try:
        assert w.layout_source == "built-in"
        assert not w.platoplasm_dock.isHidden() and not w.controls_dock.isHidden()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err

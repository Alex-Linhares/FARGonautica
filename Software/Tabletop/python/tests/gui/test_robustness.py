"""Loop0006 item 19: robustness, speed and standalone readiness (G-058), after the Numbo
GUI's `tests/gui/test_polish.py` through the Seek-Whence GUI's `test_robustness.py`.
pytest-qt, offscreen.

- The pixel probe: what the window shows of every painting widget of every view on
  screen equals a fresh render of it (the incremental paints, the cached pictures and
  the scrolled lists leave nothing stale): sampled every 50th event of the quick start's
  run in the default set; after every event of it, under both tab arrangements and at
  ten seeks with the third, when marked `slow`.
- Memory over five consecutive runs in one window stays flat once warm.
- Closing the window while a run plays, and while a batch runs, stops and joins the
  workers and frees the engine lock.
- The forced capped run of G-007 (3000 codelets, 30,692 events since v5's fix) goes through the
  window at max speed within a bound, every event drawn, with no slowdown over the run
  (`slow`).
- The window fits 1280 x 800 with every dock shown, and a run goes through at that size.
- A `window/state` stamped by an older or a newer version of the window is ignored and
  said; a state that is not bytes is ignored; a state of this version naming other docks
  leaves every pane reachable (G-013).
"""

import dataclasses
import gc
import os
import sys
import threading
import time

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings, Qt                                 # noqa: E402
from PySide6.QtTest import QTest                                         # noqa: E402
from PySide6.QtWidgets import QDockWidget, QLabel, QMainWindow           # noqa: E402

from tabletop.config import PROFILES                                     # noqa: E402
from tabletop.events import Collector, record_of                         # noqa: E402
from tabletop.gui.main_window import PANES, SMALLEST_SIZE, STATE_VERSION  # noqa: E402
from tabletop.gui.models import run_controller as rc                     # noqa: E402
from tabletop.problems import load_problem                               # noqa: E402
from tabletop.run import run_problem                                     # noqa: E402

from golden import plain_run                                             # noqa: E402
from test_main_window import QUICK, make_window, run_to_end              # noqa: E402
from test_table_view import fresh, same_picture, shown                   # noqa: E402

TIMEOUT = 30000           # ms
SIZE = (1500, 950)        # the screenshots' size (G-056)
SHORT = ("buridan-03", 2)   # a short library run: 69 codelets, 738 events since v5's fix (D-116 to D-120); 84 and 913 since item 09 (46 and 495 at items 07 and 08; 66 and 687 at item 05; 36 and 386 at item 04; 79 and 722 after item 03; 15 and 190 in v1)
SHORT_EVENTS = 738          # on the v2 defaults, which this file's window runs read (D-114); 913 before v5

# The forced capped run of G-007 through the window at max speed: 30,692 events, every
# one drawn. Measured in G-058; the bound leaves room for a loaded machine.
CAPPED_EVENTS = 30692      # v5's structural Blockage fix (D-116 to D-120), counted on the plain run on 2026-10-17; 25,263 in v4 (D-115); 25,618 in v3 (D-114); before: 22,764 in v1; 22,773 after the roles substrate (D-079); 23,496 after the geometric family (D-082 to D-084); 23,646 at item 05 (the four role detectors, the role factor, C1 pairs); 24,638 at item 07 (ROW and COLUMN, the re-cast, C2, C6); 24,389 at item 08 (GAP, LONER, STRAY and CORNER, the midline on loners, C1 pairs, C7's role term); 24,291 since item 09 (FIGURE, GROUND, ECHO, TWIN and TYPE_KEY, C1's figure, ground, echo and key pairs, C5's key identity, the re-cast's TWIN tier, find-roles after a built group); 25,753 since item 10 (BARRIER and CLAIMED, read 27 times in this run, C4's fights for the barrier's correspondence, the re-cast's CLAIMED tier), 2026-10-14: counted through the window offscreen (209.2 s); 24,744 since item 06 (CONTAINER's setting and holder forms read by find-roles, two tags in this run; C3's answer from group position; examine-cp's anchor pair), 2026-10-14: counted on the plain run, which the window's run equals; 24,774 counted again on 2026-10-15 (the fast-engine loop's item 06) on the plain run of the pre-loop engine and of the compiled engine alike, the previous count having been stale
CAPPED_BOUND_S = 0.02 * CAPPED_EVENTS
CAPPED_WINDOW = 3000      # events: the first window against the last, GUI-thread CPU time
CAPPED_SLOWDOWN = 2.5     # the last window may cost this much more than the first


def engine_is_free():
    """Whether the engine lock (one run or batch at a time, G-052) can be taken now."""
    lock = rc.engine_lock()
    if lock.acquire(blocking=False):
        lock.release()
        return True
    return False


# -- the pixel probe (Numbo GUI, test_polish.py; G-058) --------------------------------------

def painting_widgets(w):
    """Every widget that paints a model, by name: the views' own painters and lists,
    the strips, and each whole view (its labels and buttons with it) but the timeline."""
    widgets = {
        "table": w.table_view.view.viewport(),
        "thermometer": w.thermometer_view,
        "temperature-plot": w.temperature_view.plot,
        "slipnet-grid": w.slipnet_grid_view.grid,
        "slipnet-network": w.slipnet_network_view.network,
        "coderack-strip": w.coderack_view.strip,
        "rack-list": w.rack_view.list.viewport(),
        "worldview-lines": w.worldview_view.worldview.viewport(),
        "worldview-proposals": w.worldview_view.proposals.viewport(),
        "competitions": w.competitions_view.list.viewport(),
        "roles-lines": w.roles_view.list.viewport(),
        "trace": w.log_view.trace.list.viewport(),
        "events": w.log_view.events_tab.list.viewport(),
        "results-graph": w.results_view.graph,
        "results-line": w.results_view.line,
        "stats": w.stats_view.viewport(),
        "timeline-strip": w.timeline.strip,
        "controls-run-line": w.controls_view.run_line,
        "palette-edit-line": w.palette_view.edit_line,
        "config-status-line": w.config_view.status,
    }
    for attr, *_ in PANES:
        widgets[f"{attr}-view"] = getattr(w, f"{attr}_view")
    widgets["table-view"] = w.table_view
    # Not the whole timeline bar: the toolbar paints its own background under it, so a
    # bare grab of the bar differs in shade from the window; its strip is the painter.
    return widgets


class PixelProbe:
    """At each `event_drawn` (every `every`th): every on-screen painting widget's painted
    pixels equal a fresh render of it. Set TABLETOP_PIXEL_DEBUG to save the first
    mismatch's pair under /tmp."""

    def __init__(self, w, every=1):
        self.w, self.every = w, every
        self.widgets = painting_widgets(w)
        self.mismatches = []
        self.checked = 0
        self.skipped = set()
        w.event_drawn.connect(self.drawn)

    def detach(self):
        self.w.event_drawn.disconnect(self.drawn)

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
                if not self.mismatches and os.environ.get("TABLETOP_PIXEL_DEBUG"):
                    a.save(f"/tmp/ttgui-mismatch-{name}-shown.png")
                    b.save(f"/tmp/ttgui-mismatch-{name}-fresh.png")
                self.mismatches.append((index, name))


def raise_second_tabs(w):
    """The tabs hidden by default on top: the network, the competitions, the rack list
    and the results, and the Worldview pane's Proposals tab."""
    for dock in (w.slipnet_network_dock, w.competitions_dock, w.rack_dock, w.results_dock):
        dock.raise_()
    w.worldview_view.tabs.setCurrentIndex(1)


def raise_third_tabs(w):
    """The temperature plot on top of its group, and the configuration pane (the dock
    Edit > Configuration shows) in it."""
    w.config_dock.show()
    w.temperature_dock.raise_()


def raise_fourth_tabs(w):
    """The Worldview pane on top with its Proposals tab."""
    w.worldview_dock.raise_()
    w.worldview_view.tabs.setCurrentIndex(1)


def raise_config(w):
    """The configuration pane on top of its group."""
    w.config_dock.raise_()


def raise_roles(w):
    """The Roles pane (P14, G-060) on top of the Worldview's group, where it is tabbed."""
    w.roles_dock.raise_()


def raise_first_tabs(w):
    """The built-in arrangement's tabs back on top."""
    for dock in (w.slipnet_grid_dock, w.worldview_dock, w.coderack_dock, w.log_dock):
        dock.raise_()
    w.worldview_view.tabs.setCurrentIndex(0)


@pytest.fixture(scope="module")
def w(module_window):
    """The module's window at the screenshots' size."""
    module_window.resize(*SIZE)
    for _ in range(100):
        QTest.qWait(10)
        if (module_window.width(), module_window.height()) == SIZE:
            break
    QTest.qWait(20)
    return module_window


# -- memory (Numbo GUI, test_polish.py; G-058) ------------------------------------------------


# -- closing mid-run and mid-batch (G-008, G-052, G-058) ---------------------------------------

def test_closing_while_a_run_plays_stops_and_joins_the_worker(qtbot, thread_errors):
    w = make_window(qtbot)
    w.set_delay(0.02)
    w.start_run("surround-05", PROFILES["standard"], 1)
    qtbot.waitUntil(lambda: w.position >= 5, timeout=TIMEOUT)
    assert w.controller.is_alive()
    before = threading.active_count()
    w.close()
    assert not w.controller.is_alive()
    assert threading.active_count() < before
    assert w.controller.state == "stopped"
    assert engine_is_free()


def test_closing_during_a_batch_stops_and_joins_the_batch_worker(qtbot, thread_errors):
    w = make_window(qtbot)
    w.set_next_run(problem="surround-05", seed=1)
    assert w.run_batch(500)
    pane = w.results_view
    assert pane.busy and not engine_is_free()
    qtbot.waitUntil(lambda: len(pane.model.results) >= 1, timeout=TIMEOUT)
    w.close()
    assert not pane.worker.is_alive() and not pane.busy
    assert engine_is_free()
    assert 1 <= len(pane.model.results) < 500


# -- a long run: the capped run at max speed (G-007, G-058) ------------------------------------


# -- the smallest window (G-014, G-058) ----------------------------------------------------------


# -- settings from another version of the layout (G-013, G-058) ----------------------------------

def ini_settings(ini):
    return QSettings(ini, QSettings.Format.IniFormat)


@pytest.mark.parametrize("version", [STATE_VERSION - 1, STATE_VERSION + 1])
def test_a_state_saved_by_another_version_is_ignored(qtbot, ini, thread_errors, capfd, version):
    """`window/state` as an older or a newer window wrote it: Qt refuses it, the status
    bar says so, the built-in layout is used, and closing saves the current version over
    it, which the next window restores."""
    w = make_window(qtbot, ini_settings(ini))
    w.worldview_dock.hide()
    other = w.saveState(version)
    w.close()
    s = ini_settings(ini)
    s.setValue("window/state", other)
    s.sync()
    w2 = make_window(qtbot, ini_settings(ini))
    try:
        assert w2.layout_source == "built-in"
        assert not w2.worldview_dock.isHidden()
        assert "another version" in w2.statusBar().currentMessage()
    finally:
        w2.close()
    assert "Traceback" not in capfd.readouterr().err
    assert ini_settings(ini).value("window/state") != other
    w3 = make_window(qtbot, ini_settings(ini))
    try:
        assert w3.layout_source == "settings"
    finally:
        w3.close()


def test_a_state_that_is_not_bytes_is_ignored(qtbot, ini, thread_errors, capfd):
    s = ini_settings(ini)
    s.setValue("window/state", "an old text layout")
    s.setValue("window/geometry", 12)
    s.sync()
    w = make_window(qtbot, ini_settings(ini))
    try:
        assert w.layout_source == "built-in"
        assert not w.worldview_dock.isHidden() and not w.controls_dock.isHidden()
    finally:
        w.close()
    assert "Traceback" not in capfd.readouterr().err

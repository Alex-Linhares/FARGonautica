"""Loop0006 item 12: the stats pane (`tabletop/gui/stats_view.py`; G-045) in the window,
offscreen, pytest-qt, one shared window and one run of `surround-05` seed 3, after the
Seek-Whence GUI's `test_stats_view.py` and the Numbo GUI's `test_gui_shell.py`.

- The run through the window equals the plain run, and the pane's backing store equals a
  fresh render at sampled events.
- At the end the rows show French's report line, equal to `report_line(result)` and to
  the status bar's, the Run line equal to the title's, `none violated`, the result's
  fields; the invariants row's pixels are not red.
- A fresh view fed the history renders byte-identically at sampled indexes; seeking
  shows the numbers of the past.
- A stopped run says `stopped after N events` and keeps saying so after a seek.
- A bare view given a violated invariant paints it red; the minimum.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtGui import QColor                                          # noqa: E402

from golden import V2, V2_PROFILES, plain_run  # noqa: E402
from tabletop import events as ev                                         # noqa: E402
from tabletop.config import PROFILES                                      # noqa: E402
from tabletop.gui import stats_view as sv                                 # noqa: E402
from tabletop.gui.models.run_stats import FIELDS, RunStats                # noqa: E402
from tabletop.run import report_line                                      # noqa: E402
from test_coderack_view import Probe, qtbot_wait                          # noqa: E402
from test_table_view import run_to_end                                    # noqa: E402
from golden import V2, V2_PROFILES                                        # noqa: E402

QUICK = ("surround-05", 3)
PROBES = {}
TIMEOUT = 30000


@pytest.fixture(scope="module")
def ran(module_window, qapp):
    """The quick run through the shared window with the stats pane on screen (its dock
    floated, as the rack dock is in `test_coderack_view.py`) and probed."""
    w = module_window
    w.resize(1600, 1000)
    w.stats_dock.setFloating(True)
    w.stats_dock.resize(520, 420)
    w.stats_dock.move(40, 40)
    qapp.processEvents()
    qtbot_wait(qapp)
    assert w.stats_dock.isFloating()
    assert not w.stats_view.viewport().visibleRegion().isEmpty()
    PROBES["stats"] = Probe(w, w.stats_view.viewport(), top=w.stats_dock)
    outcome = run_to_end(w, qapp, *QUICK)
    w.event_drawn.disconnect(PROBES["stats"].drawn)
    yield outcome
    # Dock the floating dock again: left floating, its window stays on the offscreen
    # screen after the module's window closes and takes the next module's mouse moves.
    w.stats_dock.setFloating(False)


def row_of(view, field):
    titles = dict(FIELDS)
    for k, (title, _text, _role) in enumerate(view.rows()):
        if title == titles[field]:
            return k
    raise KeyError(field)


def row_pixels(view, k):
    """The darkest pixel of a row's text area (RGB), scanning the text's left end."""
    image = view.viewport().grab().toImage()
    top = int(view.row_top(k))
    best = None
    for y in range(top + 1, top + int(view.line) - 1):
        for x in range(int(sv.LABEL_WIDTH) + 4, min(image.width(), int(sv.LABEL_WIDTH) + 160)):
            c = QColor(image.pixel(x, y))
            if best is None or c.lightness() < best.lightness():
                best = c
    return best


def is_reddish(colour):
    return colour.red() > 120 and colour.green() < 80 and colour.blue() < 80


# -- the run through the window (G-045) ----------------------------------------------------------

def test_the_run_through_the_window_equals_the_plain_run_and_the_probe(module_window, ran):
    run = plain_run(*QUICK, V2)
    assert list(module_window.history.records) == list(run.records)
    assert ran.result == run.result
    probe = PROBES["stats"]
    assert len(probe.checked) >= 9 and probe.mismatches == []


def test_at_the_end_the_rows_show_the_report_line_and_the_run(module_window, ran):
    w = module_window
    view = w.stats_view
    assert view.pane_id == "P12" and "Numbo GUI" in view.pages
    assert view.text("report") == report_line(ran.result) == w.statusBar().currentMessage()
    assert view.text("report") == "Eliza touches spoon3 (none). Structure 76. Codelets 59. T 43.6."   # surround-05 seed 3 since v5's fix, D-116 to D-120 (cup2 (E2), structure 75, 44 codelets, T 41.0 since item 08)
    assert view.text("run") == "surround-05, seed 3"
    # the run is on the v2 defaults, an edit of v3's (D-114), which the title names
    assert w.windowTitle().split(" — ")[1].startswith("surround-05, seed 3 (edited: ")
    assert view.text("state") == "finished" and view.text("source") == "live run"
    assert view.text("outcome") == "answered" and view.text("codelets") == "59"    # 44 since item 08
    assert view.text("events") == str(len(w.history)) == "720"                     # 507 since item 08
    assert view.text("draws") == str(ran.result.draws)
    assert view.text("invariants") == "none violated (as of codelet 59)"
    assert view.text("config").startswith("standard: ")       # the v2 defaults (D-114)
    assert view.text("worldview") == "1 correspondence: cp2; rating 76.0 (as of codelet 59)"   # the end snapshot's total (D-062): 76.0119, the same as the change at codelet 40 (cp2 replacing cp1, won); the report rounds it to 76 (since item 08: cp1, 74.8848 from the change at codelet 18, no rival)
    assert "answer spoon3 (none), through correspondence (cp2); structure 76" in view.text("result")
    assert set(view.texts()) == {field for field, _ in FIELDS}
    assert len(view.rows()) == len(FIELDS)         # no fault row
    assert view.events == 720 and view.paints >= 720


def test_the_invariants_row_is_not_red_and_the_report_is_ink(module_window, ran):
    view = module_window.stats_view
    view.verticalScrollBar().setValue(view.verticalScrollBar().maximum())
    view.viewport().repaint()
    for field in ("invariants", "report"):
        k = row_of(view, field)
        assert 0 <= view.row_top(k) < view.viewport().height() - view.line
        darkest = row_pixels(view, k)
        assert darkest is not None and darkest.lightness() < 100
        assert not is_reddish(darkest), (field, darkest.name())
    view.verticalScrollBar().setValue(0)


def test_a_fresh_view_fed_the_history_renders_the_windows_picture(module_window, ran, tmp_path,
                                                                 qapp):
    w = module_window
    size = w.stats_view.viewport().size()
    for index in (40, 211, len(w.history) - 1):
        assert w.seek(index)
        fresh = sv.StatsView("Stats", "P12", w.stats_view.pages)
        fresh.attach(w.history)
        fresh.resize(w.stats_view.size())
        fresh.show()
        fresh.viewport().resize(size)
        qapp.processEvents()
        fresh.set_state(w.state)
        for i, event in enumerate(w.history.events[:index + 1]):
            fresh.set_position(i)
            fresh.on_event(event)
        fresh.redraw(now=True)
        assert fresh.texts() == w.stats_view.texts(), index
        assert fresh.render_png(tmp_path / "fresh.png") == \
            w.stats_view.render_png(tmp_path / "shown.png"), index
        fresh.close()
    w.seek(len(w.history) - 1)


def test_seeking_shows_the_numbers_of_the_past(module_window, ran):
    w = module_window
    view = w.stats_view
    events = w.history.events
    for index in (0, 25, 100, 300):
        assert w.seek(index)
        assert view.text("events") == str(index + 1)
        assert view.text("codelets") == str(events[index].clock)
        assert view.text("draws") == str(events[index].draws)
        assert view.text("report") == "" and view.text("outcome") == ""
        if index < 211:                         # before the first Worldview change
            assert view.text("worldview").startswith("empty") or "held" in view.text("temperature")
        state = w.history.state_at(index)
        if state is None:
            assert view.text("invariants") == "not checked yet"
        else:
            assert view.text("invariants") == f"none violated (as of codelet {state.clock})"
    w.seek(len(events) - 1)
    assert view.text("report") == report_line(ran.result)


def test_a_stopped_run_says_so_and_keeps_saying_so_after_a_seek(window, qtbot):
    window.set_delay(0.0)
    window.start_run("surround-05", V2_PROFILES["standard"], 1, paused=True)
    for _ in range(3):                      # the start, then two codelets
        position = window.position
        window.controller.step_codelet()
        # the window's state comes by a queued signal: wait for it, not the controller's
        qtbot.waitUntil(lambda: window.state == "paused" and window.position > position,
                        timeout=TIMEOUT)
    assert window.position > 10
    assert window.stats_view.text("state") == "paused"
    window.stop()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert not window.controller.is_alive()
    view = window.stats_view
    n = len(window.history)
    assert view.text("outcome") == f"stopped after {n} events"
    assert view.text("report") == "" and view.text("state") == "stopped"
    assert window.seek(3)
    assert view.text("outcome") == f"stopped after {n} events" and view.text("events") == "4"
    # a new run forgets the stopped outcome
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_run(*QUICK[:1], V2_PROFILES["standard"], QUICK[1])
        window.controller.run_to_end()
    qtbot.waitUntil(lambda: not window.controller.is_alive(), timeout=TIMEOUT)
    assert view.text("outcome") == "answered"


def test_a_bare_view_paints_a_violated_invariant_in_red(qapp):
    run = plain_run(*QUICK, V2)
    view = sv.StatsView("Stats", "P12", "Numbo GUI, stats_view.py")
    view.resize(520, 440)
    view.show()
    qapp.processEvents()
    for event in run.events:
        view.on_event(event)
    snapshot = type("S", (), {"clock": 46, "worldview": {"total": 75.0}})()
    view.model.update(snapshot, {"faults": ("T is 120", "cp9 ends on nothing")})
    view.redraw(now=True)
    assert view.text("invariants") == ("2 violated (as of codelet 46): T is 120; "
                                       "cp9 ends on nothing")
    rows = view.rows()
    assert len(rows) == len(FIELDS) + 2
    assert rows[-2] == ("", "T is 120", "fault") and rows[-1][2] == "fault"
    view.verticalScrollBar().setValue(view.verticalScrollBar().maximum())
    view.viewport().repaint()
    for k in (row_of(view, "invariants"), len(rows) - 1):
        darkest = row_pixels(view, k)
        assert darkest is not None and is_reddish(darkest), (k, darkest.name())
    view.model.update(snapshot, {"faults": ()})
    view.redraw(now=True)
    assert len(view.rows()) == len(FIELDS)
    view.close()


def test_a_bare_view_before_any_run(qapp):
    view = sv.StatsView()
    assert view.text("state") == "idle" and view.text("run") == ""
    assert view.rows()[0] == ("State", "idle", "ink")
    model = RunStats()
    assert view.texts() == model.texts("idle", "live run")
    assert (view.minimumSize().width(), view.minimumSize().height()) == sv.MIN_SIZE
    assert isinstance(ev.RunStarted, type)

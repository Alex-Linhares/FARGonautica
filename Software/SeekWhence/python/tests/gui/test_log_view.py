"""Loop0004 item 12: the event log pane (`seekwhence/gui/log_view.py`) in the window,
after the Numbo GUI's `tests/gui/test_log_replay.py`. pytest-qt, offscreen.

- The dock is tabbed under the console; the rows list every event of a live run with
  its clock [M86 p. 41] and `event_text`.
- The Kinds menu filters by kind and by group; Show all, Hide all.
- A double-click or Enter seeks; seeking is refused while a run is live.
- The rows are painted once per event while the dock is on screen.
- Save events as JSON lines equals `python3 -m seekwhence --json` for the same run.
- A standalone view fed blackburn-11's events stays responsive (a timing bound).
"""

import io
import json
import subprocess
import sys
import time
from pathlib import Path

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QPoint, Qt                                # noqa: E402

from seekwhence import bench                                          # noqa: E402
from seekwhence import sequences as sq                                # noqa: E402
from seekwhence.config import Config                                  # noqa: E402
from seekwhence.events import Recorder                                # noqa: E402
from seekwhence.gui import log_view as lv                             # noqa: E402
from seekwhence.gui.models import presets                             # noqa: E402
from seekwhence.gui.models.event_log import GROUPS, KINDS, event_text  # noqa: E402
from seekwhence.session import Session                                # noqa: E402

from golden import APPENDIX                                           # noqa: E402

TIMEOUT = 30000
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
PYTHON_DIR = Path(__file__).resolve().parents[2]


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def on_screen(widget):
    return not widget.visibleRegion().isEmpty()


# -- the rows ---------------------------------------------------------------------------------------

def test_the_dock_lists_every_event_of_a_live_run(window, qtbot, appendix_runs):
    log = window.log_view
    assert window.log_dock.widget() is log and log.pane_id == "P9"
    assert window.console_dock in window.tabifiedDockWidgets(window.log_dock)
    assert not on_screen(log)
    assert log.count_label.text() == "0 of 0 events"
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    events = appendix_runs[RUN1].events
    rows = log.list
    assert window.history.events == events
    assert rows.row_count() == len(events) == 174
    h = window.history
    for row in (0, 1, 100, 173):
        assert rows.row_index(row) == row
        text = rows.row_display(row)
        assert text == lv.row_text(row, h.clock_at(row), events[row])
        assert event_text(events[row]) in text and events[row].kind in text
    assert rows.row_display(0).split()[:2] == ["0", "start"]
    assert rows.row_display(173).split()[:3] == ["173", "52", "ended"]
    assert "doing task" in rows.row_display(h.clock_index(1))
    assert rows.position == 173
    assert not any(rows.is_future(r) for r in range(174))
    assert log.count_label.text() == "174 of 174 events"
    assert log.events == 174


def test_the_kinds_menu_filters_by_kind_and_by_group(window, qtbot, appendix_runs):
    events = appendix_runs[RUN1].events
    log = window.log_view
    rows = log.list
    assert set(log.kind_actions) == set(KINDS)
    assert set(log.group_actions) == {group for group, _ in GROUPS}
    log.kind_actions["posted"].setChecked(False)       # hidden before the run
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    shown = [i for i, e in enumerate(events) if e.kind != "posted"]
    assert rows.row_count() == len(shown)
    assert [rows.row_index(r) for r in range(rows.row_count())] == shown
    assert log.count_label.text() == f"{len(shown)} of 174 events"
    assert log.group_actions["taskrack"].isChecked()      # some of its kinds still show
    log.group_actions["taskrack"].setChecked(False)
    rack = dict(GROUPS)["taskrack"]
    shown = [i for i, e in enumerate(events) if e.kind not in rack]
    assert [rows.row_index(r) for r in range(rows.row_count())] == shown
    assert all(not log.kind_actions[k].isChecked() for k in rack)
    log.group_actions["taskrack"].setChecked(True)
    assert rows.row_count() == 174 and all(log.kind_actions[k].isChecked() for k in rack)
    log.kind_actions["chosen"].setChecked(False)
    assert log.group_actions["taskrack"].isChecked()
    log.hide_all_action.trigger()
    assert rows.row_count() == 0 and log.count_label.text() == "0 of 174 events"
    assert not any(a.isChecked() for a in log.group_actions.values())
    log.show_all_action.trigger()
    assert rows.row_count() == 174 and log.hidden_kinds() == frozenset()
    assert all(a.isChecked() for a in log.group_actions.values())


# -- seeking (PANES.md P9) -------------------------------------------------------------------------------

def test_a_double_click_or_enter_seeks_and_a_live_run_refuses(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    window.log_dock.raise_()
    qtbot.waitUntil(lambda: on_screen(window.log_view), timeout=TIMEOUT)
    rows = window.log_view.list
    window.seek(120)
    assert rows.position == 120 and rows.row_is_visible(120)
    assert rows.is_future(121) and not rows.is_future(120)
    rows.scroll_to(40)
    y = rows.row_rect(40).center().y()
    qtbot.mouseDClick(rows.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(20, y))
    assert window.position == 40 and window.timeline.slider.value() == 40
    assert window.stats_view.text("events") == "41"
    rows.setFocus()
    qtbot.keyClick(rows, Qt.Key.Key_Down)
    qtbot.keyClick(rows, Qt.Key.Key_Down)
    qtbot.keyClick(rows, Qt.Key.Key_Return)
    assert window.position == 42
    # Refused while a run is live.
    window.step()
    qtbot.waitUntil(lambda: window.position == 0 and window.state == "paused", timeout=TIMEOUT)
    window._seek_from_log(0)
    assert "stop the run" in window.statusBar().currentMessage()
    rows.activate(0)
    assert window.position == 0 and window.controller.is_alive()
    window.step()
    qtbot.waitUntil(lambda: window.position == 1 and window.state == "paused", timeout=TIMEOUT)
    window.step()
    qtbot.waitUntil(lambda: window.position == 2 and window.state == "paused", timeout=TIMEOUT)
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.stop()
    assert not window.controller.is_alive()
    assert rows.row_count() == len(window.history) == 3
    rows.activate(1)
    assert window.position == 1


def test_the_rows_are_painted_once_per_event_when_on_screen(window, qtbot):
    window.log_dock.raise_()
    qtbot.waitUntil(lambda: on_screen(window.log_view), timeout=TIMEOUT)
    paints = []
    window.event_drawn.connect(lambda i: paints.append(window.log_view.paints))
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert len(paints) == 174
    # The rows painted between one event's drawing and the next's (Numbo GUI,
    # `test_log_replay.py`), and all but a few events exactly once: a scroll whose
    # range changes at the same time can flush the exposed strip apart.
    assert all(b > a for a, b in zip(paints, paints[1:]))
    assert paints[-1] - paints[0] <= 173 + 5
    assert window.log_view.list.row_is_visible(173)


# -- Save events as JSON lines (DECISION D-069) --------------------------------------------------------

def cli_json(seed, lines, answers, mode, path):
    command = [sys.executable, "-m", "seekwhence", "--seed", str(seed), "--terms",
               *map(str, lines), "--trace", mode, "--json", str(path)]
    if answers:
        command += ["--answers", *answers]
    subprocess.run(command, cwd=PYTHON_DIR, capture_output=True, text=True, check=True)
    return path.read_text(encoding="utf-8")


@pytest.mark.parametrize("run_id", [RUN1, RUN2])
def test_save_events_equals_the_cli_s_json_lines(window, qtbot, tmp_path, run_id):
    inputs = presets.appendix_inputs(run_id)
    assert not window.save_events_action.isEnabled()
    run_to_end(qtbot, window, inputs)
    assert window.save_events_action.isEnabled()
    saved = tmp_path / "events.jsonl"
    n = window.save_events(saved)
    assert n == len(window.history) and str(saved) in window.statusBar().currentMessage()
    mode = "verbose" if inputs.config.verbose else "terse"
    expected = cli_json(inputs.seed, inputs.lines, inputs.answers, mode, tmp_path / "cli.jsonl")
    assert saved.read_text(encoding="utf-8") == expected
    lines = saved.read_text(encoding="utf-8").splitlines()
    assert len(lines) == n and json.loads(lines[0])["kind"] == "start"
    assert json.loads(lines[-1]) == {"kind": "ended", "reason": "ok"}


# -- the timing bound (PANES.md P9) --------------------------------------------------------------------

@pytest.mark.slow
def test_the_log_stays_fast_on_blackburn_11(qtbot):
    seq = sq.find("blackburn-11")
    rec = Recorder()
    session = Session(Config(), 1, out=io.StringIO(), lines=list(seq.prefix(16)),
                      answerer=lambda console: bench.ContinuationAnswerer(console, seq.terms),
                      observers=[rec])
    session.run()
    events = rec.events
    assert len(events) > 20000
    log = lv.LogView()
    qtbot.addWidget(log)
    log.resize(400, 500)
    log.show()
    qtbot.waitExposed(log)
    qtbot.wait(20)      # offscreen, nothing paints before the loop has run once
    history = log.history
    times, cpu = [], []
    for i, e in enumerate(events):
        t, c = time.perf_counter(), time.thread_time()
        history.on_event(e)
        log.set_position(i)
        log.on_event(e)
        log.redraw()
        times.append(time.perf_counter() - t)
        cpu.append(time.thread_time() - c)
    assert log.list.row_count() == len(events)
    assert log.paints >= len(events)
    per_event = sum(times) / len(times)
    assert per_event < 1e-3, per_event
    # It does not grow with the run: the last 5000 events cost about what the first
    # 5000 did. Compared on this thread's CPU time, not the clock: the two windows are
    # ten seconds apart, and the clock also counts the time another process (an
    # unrelated job on the gate's machine) held the core meanwhile, which once made the
    # last window read 1.6 times the first with the code unchanged. CPU time is what
    # grows when a per-event cost does.
    first, last = sum(cpu[:5000]), sum(cpu[-5000:])
    assert last < 1.5 * first, (first, last)
    # Filtering the whole run is quick.
    t = time.perf_counter()
    log.kind_actions["posted"].setChecked(False)
    assert time.perf_counter() - t < 0.3
    assert log.list.row_count() == sum(1 for e in events if e.kind != "posted")

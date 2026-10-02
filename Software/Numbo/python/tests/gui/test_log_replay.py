"""loop0003 item 10: the event log (numbo/gui/log_view.py) and the replay UI
(Session > Open / Save, the timeline: scrubbing, jumping to an iteration).

pytest-qt, offscreen.  The central check: a run saved, reopened and
scrubbed (backwards and forwards, in any order) shows, at each event, what
the live run showed at that event, in every view: the tree canvas (items,
labels, styles, positions, edges), the Pnet, the coderack and the stats.
Skipped when PySide6 is not installed.
"""

import io
import json
import random
import threading
import time

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import Qt                    # noqa: E402

import full_runs                                 # noqa: E402
from conftest import requires_sbcl               # noqa: E402
from numbo import harness, observe, session     # noqa: E402
from numbo.gui import controls as controls_module  # noqa: E402
from numbo.gui import log_view as lv             # noqa: E402
from numbo.gui.main_window import MainWindow     # noqa: E402
from numbo.models.event_log import event_text   # noqa: E402

TIMEOUT = 30000   # ms
FASTEST = len(controls_module.DELAYS) - 1


@pytest.fixture
def thread_errors(monkeypatch):
    errors = []
    monkeypatch.setattr(threading, "excepthook", lambda args: errors.append(args))
    yield errors
    assert errors == []


def make_window(qtbot):
    w = MainWindow()
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    return w


@pytest.fixture
def window(qtbot, thread_errors):
    w = make_window(qtbot)
    yield w
    w.close()
    assert not w.controller.is_alive()


def choose(window, puzzle=1, seed=1, cap="20000", speed=FASTEST):
    c = window.controls
    c.puzzle_combo.setCurrentIndex(puzzle - 1)
    c.seed_edit.setText(str(seed))
    c.cap_edit.setText(cap)
    c.speed_slider.setValue(speed)


def plain_events(problem, seed, cap=20000, rng_events=False):
    events = []

    class R:
        def on_event(self, event):
            events.append(event)

    out, trace = io.StringIO(), io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap, trace=trace,
                                rng_events=rng_events, out=out, observers=[R()])
    return events, result, out.getvalue(), trace.getvalue()


def pt(p):
    return (p.x(), p.y())


def digest(w):
    """What every view shows now (the stats' state and check aside: they
    are about the controller and the printed text, compared on their own)."""
    tv = w.tree_view
    tree = {n: (it.label, it.style, pt(it.pos()), (it.rect.width(), it.rect.height()), it.node)
            for n, it in tv.node_items.items()}
    edges = {k: (pt(e.path().pointAtPercent(0)), pt(e.path().pointAtPercent(1)), e.opacity())
             for k, e in tv.edge_items.items()}
    pnet = {n: it.style for n, it in w.pnet_view.node_items.items()}
    cv = w.coderack_view
    rack = (cv.rows, cv.chosen_text, cv.total, cv.scale, cv.chosen_fresh)
    stats = {k: v for k, v in w.stats.texts().items() if k not in ("state", "check", "source")}
    rect = tv.scene().sceneRect()
    return dict(tree=tree, edges=edges, pnet=pnet, rack=rack, stats=stats,
                layout=tv.layout, scene=(rect.x(), rect.y(), rect.width(), rect.height()))


def assert_same(got, want, where):
    for key in want:
        assert got[key] == want[key], f"{key} differs at event {where}"


def play_live(window, qtbot, puzzle, seed, cap="20000"):
    """Play a run at full speed; the digest after each event is drawn."""
    digests = {}
    window.event_drawn.connect(lambda i: digests.__setitem__(i, digest(window)))
    choose(window, puzzle, seed, cap)
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    window.event_drawn.disconnect()
    return digests


@pytest.fixture(scope="module")
def p1s1():
    return plain_events(full_runs.PUZZLES[0], 1)


# -- the log --------------------------------------------------------------------

def test_the_log_lists_every_event_of_a_live_run(window, qtbot, p1s1):
    play_live(window, qtbot, 1, 1)
    events = p1s1[0]
    rows = window.log_view.list
    assert window.history.events == events
    assert rows.row_count() == len(events) == 286
    for row in (0, 1, 100, 285):
        assert rows.row_index(row) == row
        text = rows.row_display(row)
        assert text == lv.row_text(row, events[row])
        assert event_text(events[row]) in text and events[row].kind in text
    assert lv.row_text(285, events[285]).endswith("solved after 45 iterations")
    # It followed the run: the last event is the one drawn now, in view.
    assert rows.position == 285 and rows.row_is_visible(285)
    assert not any(rows.is_future(r) for r in range(286))
    assert window.log_view.count_label.text() == "286 of 286 events"


def test_the_log_filters_by_kind(window, qtbot, p1s1):
    events = p1s1[0]
    log = window.log_view
    rows = log.list
    assert set(log.kind_actions) == set(observe.EVENT_TYPES)
    log.kind_actions["post"].setChecked(False)       # hidden before the run
    play_live(window, qtbot, 1, 1)
    shown = [i for i, e in enumerate(events) if e.kind != "post"]
    assert rows.row_count() == len(shown)
    assert [rows.row_index(r) for r in range(rows.row_count())] == shown
    assert log.count_label.text() == f"{len(shown)} of 286 events"
    log.kind_actions["node-changed"].setChecked(False)
    shown = [i for i in shown if events[i].kind != "node-changed"]
    assert [rows.row_index(r) for r in range(rows.row_count())] == shown
    assert rows.row_is_visible(rows.row_count() - 1)
    log.show_all_action.trigger()
    assert rows.row_count() == 286
    log.hide_all_action.trigger()
    assert rows.row_count() == 0
    assert log.count_label.text() == "0 of 286 events"


def test_the_log_is_drawn_at_every_event(window, qtbot):
    paints = []
    window.event_drawn.connect(lambda i: paints.append(window.log_view.paints))
    choose(window, 1, 1)
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    # The log painted between one event's drawing and the next's.
    assert len(paints) == 286
    assert all(b > a for a, b in zip(paints, paints[1:]))


def test_the_log_stays_fast_on_a_long_run(qtbot):
    events = plain_events(full_runs.PUZZLES[5], 1)[0][:8000]
    log = lv.LogView()
    qtbot.addWidget(log)
    log.resize(400, 500)
    log.show()
    qtbot.waitExposed(log)
    qtbot.wait(20)      # offscreen, nothing paints before the loop has run once
    history = log.history
    times = []
    for i, e in enumerate(events):
        t = time.perf_counter()
        log.appended(history.on_event(e))
        log.set_position(i)
        log.redraw()
        times.append(time.perf_counter() - t)
    assert log.list.row_count() == len(events)
    assert log.paints >= len(events)
    per_event = sum(times) / len(times)
    assert per_event < 1e-3, per_event
    # It does not grow with the run: the last 2500 events cost about what
    # the first 2500 did.
    first, last = sum(times[:2500]), sum(times[-2500:])
    assert last < 1.5 * first, (first, last)
    # Filtering 8000 events is quick.
    t = time.perf_counter()
    log.kind_actions["post"].setChecked(False)
    assert time.perf_counter() - t < 0.2


# -- save, reopen, scrub -----------------------------------------------------------

def test_views_drawn_once_after_a_jump_equal_views_drawn_at_every_event(qtbot):
    """What a seek relies on: but for the tree's layout (LayoutHistory's
    job), a view fed events without drawing them, then drawn once, draws
    what it would have drawn had it drawn every event.  A long run, whose
    coderack bins outgrow the bar scale's minimum."""
    from numbo.gui.coderack_view import CoderackView
    from numbo.gui.pnet_view import PnetView
    from numbo.gui.stats_view import StatsPanel
    events = plain_events(full_runs.PUZZLES[5], 1)[0][:6000]
    every = (PnetView(), CoderackView(), StatsPanel())
    jumps = (PnetView(), CoderackView(), StatsPanel())
    marks = {500, 2222, 4000, 5999}
    for i, e in enumerate(events):
        for v in every + jumps:
            v.on_event(e)
        for v in every:
            v.redraw()
        if i in marks:
            for v in jumps:
                v.redraw()
            assert ({n: it.style for n, it in every[0].node_items.items()}
                    == {n: it.style for n, it in jumps[0].node_items.items()})
            a, b = every[1], jumps[1]
            assert (a.rows, a.chosen_text, a.total, a.scale) == (b.rows, b.chosen_text, b.total,
                                                                 b.scale)
            assert every[2].texts() == jumps[2].texts()
    assert every[1].scale > 10




def test_save_reopen_and_scrub_shows_the_live_run_at_each_event(qtbot, thread_errors,
                                                                tmp_path, p1s1):
    w = make_window(qtbot)
    live = play_live(w, qtbot, 1, 1)
    assert sorted(live) == list(range(286))
    final_check = w.stats.text("check")
    path = tmp_path / "p1s1.numbo.jsonl"
    assert w.save_session(path)
    saved = session.load_session(path)
    assert list(saved.events) == p1s1[0] and saved.ended
    w.close()

    # A fresh window opens it: the start event is drawn.
    w = make_window(qtbot)
    assert w.open_file(path)
    assert w.replay_source == ("session", str(path))
    assert w.position == 0
    assert w.history.events == p1s1[0]
    assert_same(digest(w), live[0], 0)
    assert w.timeline.slider.maximum() == 285 and w.timeline.slider.isEnabled()

    # Scrub: the end, backwards one by one for a while, jumps both ways.
    rng = random.Random(10)
    order = [285, 284, 283, 200, 0, 150, 149, 151, 285, 1, 2, 3, 100, 99, 98]
    order += [rng.randrange(286) for _ in range(40)]
    for index in order:
        assert w.seek(index)
        assert w.position == index
        assert_same(digest(w), live[index], index)
    w.seek(285)
    assert w.stats.text("check") == final_check
    assert w.stats.text("outcome") == "solved, 45 iterations"
    w.close()


def test_the_slider_and_the_log_seek(window, qtbot, tmp_path, p1s1):
    live = play_live(window, qtbot, 1, 1)
    slider = window.timeline.slider
    assert slider.isEnabled() and slider.value() == 285
    slider.setValue(120)
    assert window.position == 120
    assert_same(digest(window), live[120], 120)
    assert window.timeline.label.text().startswith("event 120 of 286")
    # The log marks the position and greys the events after it.
    rows = window.log_view.list
    assert rows.position == 120 and rows.row_is_visible(120)
    assert rows.is_future(121) and not rows.is_future(120)
    # A double-click on a row seeks to its event.
    rows.scroll_to(40)
    y = rows.row_rect(40).center().y()
    from PySide6.QtCore import QPoint
    qtbot.mouseDClick(rows.viewport(), Qt.MouseButton.LeftButton, pos=QPoint(20, y))
    assert window.position == 40 and slider.value() == 40
    assert_same(digest(window), live[40], 40)
    # So does Enter, on the row the arrow keys moved to.
    rows.setFocus()
    qtbot.keyClick(rows, Qt.Key.Key_Down)
    qtbot.keyClick(rows, Qt.Key.Key_Down)
    qtbot.keyClick(rows, Qt.Key.Key_Return)
    assert window.position == 42
    assert_same(digest(window), live[42], 42)
    # Back and forward by one event.
    window.timeline.back_button.click()
    assert window.position == 41
    window.timeline.forward_button.click()
    window.timeline.forward_button.click()
    assert window.position == 43
    assert_same(digest(window), live[43], 43)


def test_jump_to_an_iteration(window, qtbot, p1s1):
    live = play_live(window, qtbot, 1, 1)
    events = p1s1[0]
    t = window.timeline
    assert (t.iteration_spin.minimum(), t.iteration_spin.maximum()) == (0, 44)
    for n in (30, 0, 44, 7):
        t.iteration_spin.setValue(n)
        t.go_button.click()
        i = window.position
        assert isinstance(events[i], observe.IterationBegan) and events[i].n == n
        assert window.stats.text("iteration") == str(n)
        assert_same(digest(window), live[i], i)


def test_scrubbing_is_off_while_a_run_is_live(window, qtbot):
    choose(window, 3, 1, speed=0)
    window.controls.step_button.click()
    qtbot.waitUntil(lambda: window.position == 0, timeout=TIMEOUT)
    assert window.controller.is_alive()
    assert not window.timeline.slider.isEnabled()
    assert not window.seek(0)
    window.controls.stop_button.click()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.timeline.slider.isEnabled()
    assert window.seek(0)


# -- replay playback --------------------------------------------------------------

def test_playing_a_replay_draws_every_event_as_the_live_run_did(qtbot, thread_errors,
                                                               tmp_path, p1s1):
    w = make_window(qtbot)
    live = play_live(w, qtbot, 1, 1)
    path = tmp_path / "p1s1.jsonl"
    w.save_session(path)
    w.close()

    w = make_window(qtbot)
    w.open_file(path)
    w.seek(150)
    seen = {}
    w.event_drawn.connect(lambda i: seen.__setitem__(i, digest(w)))
    w.controls.speed_slider.setValue(FASTEST)
    w.controls.play_button.click()
    qtbot.waitUntil(lambda: w.position == 285 and not w.controller.is_alive(),
                    timeout=TIMEOUT)
    assert sorted(seen) == list(range(151, 286))
    for i, d in seen.items():
        assert_same(d, live[i], i)
    assert w.history.events == p1s1[0]           # a replay never adds to the history
    # Step and step-iteration from the middle of the replay.
    w.seek(10)
    w.controls.step_button.click()
    qtbot.waitUntil(lambda: w.position == 11 and w.controller.state == "paused",
                    timeout=TIMEOUT)
    assert_same(digest(w), live[11], 11)
    w.controls.step_iteration_button.click()
    qtbot.waitUntil(lambda: w.controller.state == "paused" and w.position > 11
                    and isinstance(w.history.events[w.position], observe.IterationBegan),
                    timeout=TIMEOUT)
    assert_same(digest(w), live[w.position], w.position)
    w.controls.stop_button.click()
    qtbot.waitUntil(lambda: not w.controller.is_alive(), timeout=TIMEOUT)
    assert w.timeline.slider.isEnabled()
    # Play at the end replays from the start.
    w.seek(285)
    w.controls.run_to_end_button.click()
    qtbot.waitUntil(lambda: w.position == 285 and not w.controller.is_alive(),
                    timeout=TIMEOUT)
    w.close()


def test_closing_a_replay_goes_back_to_live_runs(window, qtbot, tmp_path, p1s1):
    path = tmp_path / "s.jsonl"
    session.write_session(path, p1s1[0])
    assert window.open_file(path)
    assert not window.controls.puzzle_combo.isEnabled()
    assert "s.jsonl" in window.stats.text("source")
    window.close_replay_action.trigger()
    assert window.replay_source is None
    assert window.controls.puzzle_combo.isEnabled()
    choose(window, 2, 1)
    window.controls.play_button.click()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    assert window.stats.text("problem") == "87 from 8 3 9 10 7, seed 1"
    assert window.stats.text("source") == "live run"


def test_save_part_way_through_a_live_run(window, qtbot, tmp_path):
    choose(window, 1, 1, speed=0)
    window.controls.step_button.click()
    for _ in range(9):
        qtbot.waitUntil(lambda: window.controller.state == "paused", timeout=TIMEOUT)
        window.controls.step_button.click()
    qtbot.waitUntil(lambda: window.position == 9 and window.controller.state == "paused",
                    timeout=TIMEOUT)
    path = tmp_path / "part.jsonl"
    assert window.save_session(path)
    saved = session.load_session(path)
    assert list(saved.events) == window.history.events and len(saved.events) == 10
    assert not saved.ended


def test_the_session_menu(window):
    titles = [a.text() for a in window.menuBar().actions()]
    assert "&View" in titles and "&Session" in titles
    assert window.save_action.text().startswith("&Save")
    assert window.open_action.text().startswith("&Open")
    assert not window.save_action.isEnabled()        # nothing to save yet
    assert not window.close_replay_action.isEnabled()


def test_a_broken_file_shows_an_error_not_a_traceback(window, qtbot, tmp_path, p1s1,
                                                      capsys):
    path = tmp_path / "cut.jsonl"
    session.write_session(path, p1s1[0])
    lines = path.read_text().splitlines()
    path.write_text("\n".join(lines[:100]) + "\n")
    assert not window.open_file(path)
    assert "truncated" in window.statusBar().currentMessage()
    assert window.replay_source is None and window.history.events == []
    bad = tmp_path / "bad.jsonl"
    bad.write_text("hello\n")
    assert not window.open_file(bad)
    assert "bad.jsonl" in window.statusBar().currentMessage()
    assert not window.open_file(tmp_path / "missing.jsonl")
    assert "missing.jsonl" in window.statusBar().currentMessage()
    assert capsys.readouterr().err == ""


# -- oracle traces ----------------------------------------------------------------

@requires_sbcl
def test_an_oracle_trace_opens_and_verifies(qtbot, thread_errors, tmp_path):
    problem = full_runs.PUZZLES[0]
    base = str(tmp_path / "p1s1")
    oracle = full_runs.oracle_run(problem, 1, 20000, base, rng_events=False)
    trace = base + ".jsonl"
    events, result, output, _ = plain_events(problem, 1)

    w = make_window(qtbot)
    live = play_live(w, qtbot, 1, 1)
    w.close()

    w = make_window(qtbot)
    assert w.open_file(trace)
    assert w.replay_source == ("oracle", trace)
    qtbot.waitUntil(lambda: w.position == 0 and w.controller.state == "paused",
                    timeout=TIMEOUT)
    assert "verifying" in w.stats.text("source")
    w.controls.run_to_end_button.click()
    qtbot.waitUntil(lambda: w.outcome is not None, timeout=TIMEOUT)
    n_lines = len(open(trace, encoding="utf-8").read().splitlines())
    assert w.outcome.oracle.divergence is None and w.outcome.oracle.lines == n_lines
    assert w.stats.text("source") == f"oracle trace p1s1.jsonl: verified, {n_lines} lines"
    assert w.stats.text("check") == "valid: 114 = (6 x 20) - (7 - 1)"
    assert oracle["outcome"] == "solved"
    assert w.history.events == events
    # Drawn as the live run was, at the end and scrubbing back.
    assert_same(digest(w), live[285], 285)
    for index in (0, 120, 60, 285, 284):
        assert w.seek(index)
        assert_same(digest(w), live[index], index)
    w.close()


def test_a_changed_oracle_trace_shows_where_it_differs(window, qtbot, tmp_path, capsys):
    _, _, _, trace = plain_events(full_runs.PUZZLES[0], 1)
    lines = trace.splitlines()
    i = next(k for k, line in enumerate(lines) if line.startswith('{"ev": "iteration", "n": 20,'))
    obj = json.loads(lines[i])
    obj["temperature"] += 1
    lines[i] = json.dumps(obj)
    path = tmp_path / "changed.jsonl"
    path.write_text("\n".join(lines) + "\n")
    assert window.open_file(path)
    window.controls.run_to_end_button.click()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=TIMEOUT)
    text = window.stats.text("source")
    assert text.startswith(f"oracle trace changed.jsonl: differs at line {i + 1}: ")
    assert "temperature" in text
    assert "differs" in window.statusBar().currentMessage()
    assert capsys.readouterr().err == ""

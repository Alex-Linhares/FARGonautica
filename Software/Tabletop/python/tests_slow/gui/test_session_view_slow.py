"""The slow tests of `tests/gui/test_session_view.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_session_view import (
    PROFILES,
    QFileDialog,
    QMessageBox,
    SHORT,
    SHORT_CODELETS,
    SHORT_EVENTS,
    TIMEOUT,
    json,
    no_warning_box,
    opened,
    pytest,
    records_of,
    redigested,
    replay_to_end,
    run_to_end,
    sf,
    stopped_replay,
)

pytestmark = pytest.mark.slow


def test_the_session_menu_actions_and_dialogs(window, qtbot, tmp_path, monkeypatch):
    assert window.open_action.isEnabled()
    assert not window.save_action.isEnabled()
    assert not window.close_replay_action.isEnabled()
    assert not window.save_session(tmp_path / "none.jsonl")
    assert "nothing to save" in window.statusBar().currentMessage()
    run_to_end(qtbot, window, *SHORT)
    assert window.save_action.isEnabled()
    assert not window.close_replay_action.isEnabled()
    path = tmp_path / "s.jsonl"
    monkeypatch.setattr(QFileDialog, "getSaveFileName", lambda *a, **k: (str(path), ""))
    window.save_action.trigger()
    assert window.statusBar().currentMessage() == f"saved {SHORT_EVENTS} events to {path}"
    assert sf.read_session(str(path)).ended
    monkeypatch.setattr(QFileDialog, "getOpenFileName", lambda *a, **k: (str(path), ""))
    window.open_action.trigger()
    qtbot.waitUntil(lambda: window.state == "paused" and window.position == 0, timeout=TIMEOUT)
    assert window.replay_source == str(path)
    assert window.close_replay_action.isEnabled()
    assert window.windowTitle() == f"Tabletop — replaying s.jsonl: {SHORT[0]}, seed {SHORT[1]}"
    assert window.controls_view.replaying == "s.jsonl"
    assert not window.controls_view.problem_combo.isEnabled()
    assert window.stats_view.texts()["source"] == "replay of s.jsonl"
    # A cancelled dialog does nothing.
    monkeypatch.setattr(QFileDialog, "getOpenFileName", lambda *a, **k: ("", ""))
    window.open_action.trigger()
    assert window.replay_source == str(path)
    window.close_replay_action.trigger()
    assert not window.close_replay_action.isEnabled() and window.replay_source is None
    assert not window.controller.is_alive()
    assert window.controls_view.replaying is None and window.controls_view.problem_combo.isEnabled()
    assert window.stats_view.texts()["source"] == "live run"
    assert window.windowTitle() == f"Tabletop — {SHORT[0]}, seed {SHORT[1]}"


def test_save_then_open_replays_the_live_run(window, qtbot, tmp_path):
    case = SHORT
    live = run_to_end(qtbot, window, *case)
    ref = records_of(window)
    window.set_next_run(problem="blockage-07", seed=9)      # the inputs chosen before
    path = tmp_path / "live.jsonl"
    assert window.save_session(path)
    file = sf.read_session(str(path))
    assert list(file.records) == ref and file.ended
    assert file.header.problem == case[0] and file.header.seed == case[1]
    opened(qtbot, window, path)
    assert window.stats_view.texts()["source"] == "replay of live.jsonl"
    assert window.controls_view.values().problem == case[0]
    outcome = replay_to_end(qtbot, window)
    assert not outcome.stopped and outcome.error is None
    assert window.statusBar().currentMessage() == f"replay of live.jsonl: verified, {len(ref)} events"
    assert window.stats_view.texts()["source"] == "replay of live.jsonl: verified"
    assert records_of(window) == ref
    assert outcome.result == live.result
    assert len(window.history.snapshots) > 0       # a replay has snapshots, as live
    assert window._views.errors == []
    # The replayed history saved again is the same file.
    again = tmp_path / "again.jsonl"
    assert window.save_session(again)
    assert again.read_text(encoding="utf-8") == path.read_text(encoding="utf-8")
    # Play after the end re-executes the replay.
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.play()
        qtbot.waitUntil(lambda: window.state == "running", timeout=TIMEOUT)
        window.controller.run_to_end()
    assert window.statusBar().currentMessage().endswith(f"verified, {len(ref)} events")
    # Close replay: back to live runs, with the inputs chosen before.
    window.close_replay()
    assert window.replay_source is None and not window.controller.is_alive()
    assert "replaying" not in window.windowTitle()
    assert (window.problem, window.seed) == ("blockage-07", 9)
    assert window.controls_view.values().problem == "blockage-07"
    assert records_of(window) == ref        # the history stays, to scrub
    assert window.seek(3)
    # A problem chosen from the menu during a replay closes the replay first.
    opened(qtbot, window, path)
    window.problem_actions["surround-05"].trigger()
    assert window.replay_source is None and not window.controller.is_alive()
    assert window.problem == "surround-05"


def test_save_part_way_then_open_goes_on_to_the_end(window, qtbot, tmp_path):
    window.start_run(*SHORT[:1], PROFILES["standard"], SHORT[1], paused=True)
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    for _ in range(3):                  # three codelets in: a step boundary (G-008)
        before = window.position
        window.controller.step_codelet()
        qtbot.waitUntil(lambda: window.state == "paused" and window.position > before,
                        timeout=TIMEOUT)
    saved_at = window.position
    path = tmp_path / "part.jsonl"
    assert window.save_session(path)
    assert window.statusBar().currentMessage() == f"saved {saved_at + 1} events to {path}"
    file = sf.read_session(str(path))
    assert len(file.events) == saved_at + 1 and not file.ended
    assert "end" not in path.read_text(encoding="utf-8").splitlines()[-1][:8]
    window.stop()
    qtbot.waitUntil(lambda: not window.controller.is_alive(), timeout=TIMEOUT)
    opened(qtbot, window, path)
    replay_to_end(qtbot, window)
    message = window.statusBar().currentMessage()
    assert message.startswith(f"replay of part.jsonl: the file's {saved_at + 1} events verified")
    assert "saved part way" in message
    assert f"went on for {SHORT_EVENTS - saved_at - 1} more" in message
    assert window.stats_view.texts()["source"] == "replay of part.jsonl: verified, saved part way"
    assert len(records_of(window)) == SHORT_EVENTS and window.outcome.result.codelets == SHORT_CODELETS
    window.close_replay()


def test_a_tampered_file_shows_the_stored_events(window, qtbot, tmp_path, no_warning_box):
    run_to_end(qtbot, window, *SHORT)
    ref = records_of(window)
    live_markers = len(window.timeline.model.markers)
    live_lines = window.log_view.trace_text().splitlines()
    path = tmp_path / "tampered.jsonl"
    assert window.save_session(path)
    lines = path.read_text(encoding="utf-8").splitlines()
    k = next(i for i in range(60, 150) if json.loads(lines[i + 1])["event"] == "CodeletPosted")
    obj = json.loads(lines[k + 1])
    obj["urgency"] = 99
    lines[k + 1] = json.dumps(obj, separators=(",", ":"))
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    # Edited in place, the digest gives it away.
    assert not window.open_session(str(path))
    assert window.statusBar().currentMessage().startswith(
        f"cannot open tampered.jsonl, line {len(lines)}: the digest is not the body's")
    assert window.replay_source is None
    # Re-digested, the replay finds the difference and shows the stored events.
    path.write_text("\n".join(redigested(path)) + "\n", encoding="utf-8")
    file = sf.read_session(str(path))
    opened(qtbot, window, path)
    outcome = replay_to_end(qtbot, window)
    assert outcome.stopped                       # the checker stopped the re-run
    assert len(no_warning_box) == 1
    title, text = no_warning_box[0]
    assert title == "The replay differs from the file"
    assert text.startswith(f"tampered.jsonl differs from the engine at event {k + 1} (line {k + 2})")
    assert "urgency: file 99, re-run " in text
    assert "not available for this file" in text
    assert window.statusBar().currentMessage() == text
    # The stored events are shown: the history is the file's, without snapshots.
    assert records_of(window) == list(file.records)
    assert len(window.history.snapshots) == 0
    assert window.position == k
    assert window.stats_view.texts()["source"] == (
        f"stored events of tampered.jsonl (differs at event {k + 1})")
    assert window.stats_view.texts()["state"] == "stored"
    assert window._replay.divergence.index == k
    assert window._views.errors == []
    # The trace, the event log and the timeline show them whole; every pane survives.
    assert window.seek(len(file.events) - 1)
    assert window._views.errors == []
    assert window.log_view.trace_text().splitlines() == live_lines
    assert len(window.timeline.model.markers) == live_markers
    assert window.timeline.slider.maximum() == len(file.events) - 1
    assert window.log_view.events_tab.count_label.text() == f"{len(file.events)} of {len(file.events)} events"
    assert window.stats_view.texts()["events"] == str(len(file.events))
    assert window.stats_view.texts()["invariants"] == "not checked yet"
    assert not window.controller.is_alive()
    # Close replay: a live run again.
    window.close_replay()
    assert window.replay_source is None
    run_to_end(qtbot, window, *SHORT)
    assert records_of(window) == ref
    assert len(window.history.snapshots) > 0


def test_an_edited_table_session_replays(window, qtbot, tmp_path):
    """Item 14's pending case, with item 15's editor: a table set in the palette and
    run with Do this! is saved with the problem as its dict (G-048) and replays exactly
    (G-049, G-050)."""
    editor = window.palette_view.editor
    editor.add("cup", 30, 15)
    editor.add("fork", 20, 15)
    editor.add("cup", 80, 82)
    editor.add("spoon", 90, 82)
    editor.touch("cup1")
    editor.toggle_answer("cup2")
    editor.set_names("mine", "My table")
    window.set_next_run(seed=5)
    assert window.run_edited_table()
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.controller.run_to_end()
    assert window.controller.wait(TIMEOUT / 1000)
    live = window.outcome
    ref = records_of(window)
    assert ref[0]["problem"] == "mine" and live.result.problem == "mine"
    path = tmp_path / "edited-table.jsonl"
    assert window.save_session(path)
    file = sf.read_session(str(path))
    assert isinstance(file.header.problem, dict) and file.header.problem["name"] == "mine"
    assert file.header.problem["objects"][1]["type"] == "fork" and file.header.seed == 5
    assert file.header.problem == editor.to_dict()
    # Qt-free: the file replays with no difference (G-049).
    result = sf.replay(file)
    assert result.divergence is None and result.records == ref
    # Through the window, after the editor was cleared: the file alone gives the table.
    editor.clear()
    opened(qtbot, window, path)
    assert "replaying edited-table.jsonl" in window.windowTitle()
    assert window.started[0] == "mine"
    outcome = replay_to_end(qtbot, window)
    assert not outcome.stopped and outcome.error is None
    assert window.statusBar().currentMessage() == \
        f"replay of edited-table.jsonl: verified, {len(ref)} events"
    assert records_of(window) == ref and outcome.result == live.result
    assert window.table_view.view.model.layout().objects[0].id == "cup1"
    window.close_replay()
    assert window.replay_source is None and not window.controller.is_alive()


def test_stop_during_a_replay_then_play_again_starts_over(window, qtbot, tmp_path, golden_runs):
    """Stop during a replay leaves no live thread (the fast suite's check, in
    `stopped_replay`); then Play again starts the replay over, from its first event, and
    goes to the end, verified."""
    ref = stopped_replay(qtbot, window, tmp_path / "s.jsonl", golden_runs)
    window.controller.set_delay(0.0)
    replay_to_end(qtbot, window)
    assert records_of(window) == ref
    assert window.statusBar().currentMessage() == f"replay of s.jsonl: verified, {len(ref)} events"
    window.close_replay()

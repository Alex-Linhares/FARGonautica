"""Loop0004 item 13: sessions and replay in the window (`seekwhence/gui/main_window.py`),
after the Numbo GUI's `tests/gui/test_gui_shell.py`'s session tests (PANES.md sections 4
and 5; G-046, G-047). pytest-qt, offscreen.

- The Session menu: Open always, Save once there is a history, Close replay in replay mode.
- Save then Open re-executes the run through the window's controller and equals the live
  run, verified, for the three appendix runs [M86 pp. 164-179]; the replayed history
  saved again gives the same file; Play after the end re-executes; Close replay goes
  back to live runs.
- Save part way through a run gives a file with no `ended` event, whose replay goes on
  to the end.
- A session typed in the sequence pane, with a command and answers clicked, replays from
  its file as a scripted run and equals the live one.
- A tampered file: the window warns and shows the stored events in the log, the console
  and the timeline, with no snapshot; a truncated file and a file that is not a session
  are refused with the error.
- Stop during a replay leaves no live thread.
"""

import json

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import Qt                                    # noqa: E402
from PySide6.QtWidgets import QMessageBox                        # noqa: E402

from seekwhence import bench                                     # noqa: E402
from seekwhence.gui.models import presets                        # noqa: E402
from seekwhence.gui.models import session_file as sf             # noqa: E402
from seekwhence.trace import event_record                        # noqa: E402

from golden import APPENDIX                                      # noqa: E402

TIMEOUT = 60000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def records_of(window):
    return [event_record(e) for e in window.history.events]


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def replay_to_end(qtbot, window):
    """The opened replay, run to its end; returns the outcome."""
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.run_to_end()
    return window.outcome


def opened(qtbot, window, path):
    """Open the file: the replay is paused at its start event."""
    assert window.open_session(str(path))
    qtbot.waitUntil(lambda: window.state == "paused" and window.position == 0, timeout=TIMEOUT)
    assert window.replay_source == str(path)
    assert window.history.events[0].kind == "start"


def click(qtbot, button):
    assert button.isEnabled()
    qtbot.mouseClick(button, Qt.MouseButton.LeftButton)


def type_words(qtbot, window, text):
    edit = window.sequence_view.input_edit
    assert edit.isEnabled()
    qtbot.keyClicks(edit, text)
    qtbot.keyClick(edit, Qt.Key.Key_Return)


@pytest.fixture
def no_warning_box(monkeypatch):
    """`QMessageBox.warning` recorded, not shown (the test runs offscreen and unattended)."""
    shown = []
    monkeypatch.setattr(QMessageBox, "warning",
                        lambda parent, title, text, *a, **k: shown.append((title, text)))
    return shown


# -- the menu --------------------------------------------------------------------------------------

def test_the_session_menu_actions(window, qtbot, tmp_path):
    assert window.open_action.isEnabled()
    assert not window.save_action.isEnabled()
    assert not window.close_replay_action.isEnabled()
    assert not window.save_session(tmp_path / "none.jsonl")
    assert "nothing to save" in window.statusBar().currentMessage()
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert window.save_action.isEnabled()
    assert not window.close_replay_action.isEnabled()
    path = tmp_path / "s.jsonl"
    assert window.save_session(path)
    assert window.statusBar().currentMessage() == f"saved 174 events to {path}"
    opened(qtbot, window, path)
    assert window.close_replay_action.isEnabled()
    assert window.windowTitle() == "Seek-Whence — replaying s.jsonl: run1_3737, seed 54"   # G-054
    window.close_replay()
    assert not window.close_replay_action.isEnabled() and window.replay_source is None
    assert not window.controller.is_alive()


# -- save then open (G-047) ------------------------------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_save_then_open_replays_the_live_run(window, qtbot, tmp_path, appendix_runs, run_id):
    ref = appendix_runs[run_id]
    live = run_to_end(qtbot, window, presets.appendix_inputs(run_id))
    assert records_of(window) == ref.records
    path = tmp_path / "live.jsonl"
    assert window.save_session(path)
    file = sf.read_session(str(path))
    assert list(file.records) == ref.records and file.ended and file.run.mode == "scripted"
    opened(qtbot, window, path)
    assert window.stats_view.texts()["run"].startswith("replay of live.jsonl")
    outcome = replay_to_end(qtbot, window)
    assert not outcome.stopped and outcome.error is None
    assert window.statusBar().currentMessage() == (
        f"replay of live.jsonl: verified, {len(ref.records)} events")
    assert window.stats_view.texts()["run"] == f"replay of live.jsonl: verified, seed {ref.seed}"
    assert records_of(window) == ref.records
    assert outcome.result["draws"] == live.result["draws"] == ref.draws
    assert outcome.result["outcome"] == "ok"
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
    assert window.statusBar().currentMessage().endswith(f"verified, {len(ref.records)} events")
    # Close replay: back to live runs, with the inputs chosen before.
    window.close_replay()
    assert window.replay_source is None and not window.controller.is_alive()
    assert "replaying" not in window.windowTitle()
    assert records_of(window) == ref.records        # the history stays, to scrub
    assert window.seek(3)
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert records_of(window) == appendix_runs[RUN1].records


def test_save_part_way_then_open_goes_on_to_the_end(window, qtbot, tmp_path, appendix_runs):
    ref = appendix_runs[RUN2]
    window.start_session(presets.appendix_inputs(RUN2), paused=True)
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    window.controller.step_term()
    qtbot.waitUntil(lambda: window.state == "paused" and window.position > 0
                    and window.history.events[window.position].kind == "prompt",
                    timeout=TIMEOUT)
    saved_at = window.position
    path = tmp_path / "part.jsonl"
    assert window.save_session(path)
    file = sf.read_session(str(path))
    assert len(file.events) == saved_at + 1 and not file.ended
    assert list(file.records) == ref.records[:saved_at + 1]
    window.stop()
    qtbot.waitUntil(lambda: not window.controller.is_alive(), timeout=TIMEOUT)
    opened(qtbot, window, path)
    replay_to_end(qtbot, window)
    message = window.statusBar().currentMessage()
    assert message.startswith(f"replay of part.jsonl: the file's {saved_at + 1} events verified")
    assert "saved part way" in message
    assert f"went on for {len(ref.records) - saved_at - 1} more" in message
    assert records_of(window) == ref.records
    assert window.outcome.result["draws"] == ref.draws
    window.close_replay()


def test_a_typed_session_with_a_command_and_answers_replays(window, qtbot, tmp_path, appendix_runs):
    """Run 3's terms, `show` commands and answers typed and clicked [M86 pp. 170-179],
    saved, then replayed as a scripted run."""
    spec = bench.run_spec(bench.load_expectations(), RUN3)
    words = [str(x) for x in bench.inputs_of(spec)]
    assert "show" in words
    inputs = presets.interactive_inputs(30)
    view = window.sequence_view
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
        qtbot.waitUntil(lambda: view.input_enabled, timeout=TIMEOUT)
        type_words(qtbot, window, " ".join(words))
        qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
        assert window.guess_panel.pending.block == "(1 2)(1 2)(1 2)"          # [M86 p. 175]
        click(qtbot, window.guess_panel.no_button)
        qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
        click(qtbot, window.guess_panel.ok_button)
    live = records_of(window)
    assert len(live) == len(appendix_runs[RUN3].records)
    path = tmp_path / "typed.jsonl"
    assert window.save_session(path)
    file = sf.read_session(str(path))
    assert file.run.mode == "interactive" and file.run.preset == presets.INTERACTIVE
    assert list(file.run.inputs) == words and file.run.answers == ("no", "ok")
    assert file.run.seed == 30 and file.ended
    opened(qtbot, window, path)
    assert window.feed is None                  # a replay reads the file, not the GUI
    assert not view.input_enabled
    replay_to_end(qtbot, window)
    assert window.statusBar().currentMessage() == f"replay of typed.jsonl: verified, {len(live)} events"
    assert records_of(window) == live
    assert window.console_view.text("verbose").startswith(window.outcome.output.split("\n")[0])
    window.close_replay()


# -- a file the engine no longer reproduces (PANES.md section 5) ---------------------------------

def test_a_tampered_file_shows_the_stored_events(window, qtbot, tmp_path, appendix_runs,
                                                 no_warning_box):
    ref = appendix_runs[RUN3]
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    live_console = window.console_view.text("verbose").splitlines()
    assert live_console[-1].startswith("outcome: ok")
    live_markers = len(window.timeline.model.markers)
    path = tmp_path / "tampered.jsonl"
    assert window.save_session(path)
    lines = path.read_text(encoding="utf-8").splitlines()
    k = 198
    obj = json.loads(lines[k + 2])
    assert obj["kind"] == "posted" and obj["type"] == "Tester"
    obj["type"] = "Bonder"
    lines[k + 2] = json.dumps(obj, separators=(",", ":"))
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    file = sf.read_session(str(path))
    opened(qtbot, window, path)
    outcome = replay_to_end(qtbot, window)
    assert outcome.stopped                       # the checker stopped the re-run
    assert len(no_warning_box) == 1
    title, text = no_warning_box[0]
    assert title == "The replay differs from the file"
    assert text.startswith(f"tampered.jsonl differs from the engine at event {k + 1} (line {k + 3})")
    assert 'type: file "Bonder", re-run "Tester"' in text
    assert "not available for this file" in text
    assert window.statusBar().currentMessage() == text
    # The stored events are shown: the history is the file's, without snapshots.
    assert records_of(window) == list(file.records)
    assert window.history.snapshots == {}
    assert window.position == k
    assert window.stats_view.texts()["run"].startswith(
        f"stored events of tampered.jsonl (differs at event {k + 1})")
    assert window.stats_view.texts()["state"] == "stored"
    assert window._replay.divergence.index == k
    assert window._views.errors == []
    # The console, the timeline and the log show them whole.
    assert window.seek(len(file.events) - 1)
    assert window._views.errors == []
    stored_console = window.console_view.text("verbose").splitlines()
    # The transcript's event lines are the live run's; a `show` command's output at an
    # ask is rebuilt from the snapshot (G-017), and the stored events have none.
    doing = [line for line in stored_console if line.startswith("doing task")]
    assert doing == [line for line in live_console if line.startswith("doing task")]
    assert stored_console.count("I have a guess!") == live_console.count("I have a guess!")
    assert stored_console[-1] == "bye" and live_console[-2] == "bye"
    assert "please enter a term: show" in stored_console
    assert len(stored_console) < len(live_console)
    assert len(window.timeline.model.markers) == live_markers
    assert window.log_view.count_label.text() == f"{len(file.events)} of {len(file.events)} events"
    assert window.timeline.slider.maximum() == len(file.events) - 1
    assert not window.controller.is_alive()
    window.close_replay()
    assert window.replay_source is None
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert records_of(window) == appendix_runs[RUN1].records


def test_a_truncated_file_is_refused_with_the_line(window, qtbot, tmp_path):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    path = tmp_path / "whole.jsonl"
    assert window.save_session(path)
    lines = path.read_text(encoding="utf-8").splitlines()
    cut = tmp_path / "cut.jsonl"
    cut.write_text("\n".join(lines[:50]) + "\n", encoding="utf-8")
    assert not window.open_session(str(cut))
    message = window.statusBar().currentMessage()
    assert message.startswith("cannot open cut.jsonl: ")
    assert "truncated after line 50: no end line" in message
    assert window.replay_source is None and not window.controller.is_alive()
    assert "replaying" not in window.windowTitle()
    torn = tmp_path / "torn.jsonl"
    torn.write_text("\n".join(lines[:50]) + "\n" + lines[50][:30], encoding="utf-8")
    assert not window.open_session(str(torn))
    assert "line 51: not valid JSON (truncated?)" in window.statusBar().currentMessage()
    events = tmp_path / "events.jsonl"
    window.save_events(str(events))
    assert not window.open_session(str(events))
    assert "not a seekwhence session file" in window.statusBar().currentMessage()
    assert window.open_session(str(path))
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    window.close_replay()


def test_stop_during_a_replay_leaves_no_live_thread(window, qtbot, tmp_path, appendix_runs):
    ref = appendix_runs[RUN3]
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    path = tmp_path / "s.jsonl"
    assert window.save_session(path)
    opened(qtbot, window, path)
    window.controller.set_delay(0.001)
    window.play()
    qtbot.waitUntil(lambda: window.position > 20, timeout=TIMEOUT)
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.stop()
    assert not window.controller.is_alive() and window.outcome.stopped
    message = window.statusBar().currentMessage()
    assert message.startswith("replay of s.jsonl: stopped after") and "verified" in message
    assert window.stats_view.texts()["run"].startswith("replay of s.jsonl: stopped")
    assert records_of(window) == ref.records[:len(window.history.events)]
    # Play again starts the replay over, from its first event.
    replay_to_end(qtbot, window)
    assert records_of(window) == ref.records
    window.close_replay()

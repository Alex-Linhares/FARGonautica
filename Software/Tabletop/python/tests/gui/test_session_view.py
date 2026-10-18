"""Loop0006 item 14: sessions and replay in the window (`tabletop/gui/main_window.py`;
G-048, G-049), after the Seek-Whence GUI's `test_session_view.py` and the Numbo GUI's
`test_gui_shell.py`'s session tests (PANES.md sections 4 and 5). pytest-qt, offscreen.

- The Session menu: Open always, Save once there is a history, Close replay in replay
  mode; the dialogs (patched) reach `open_session` and `save_session`.
- Save then Open re-executes the run through the window's controller and equals the live
  run, verified, with snapshots, on the quick start (`surround-05` seed 3; two problems and
  an edited configuration are replayed Qt-free in `test_session_file.py`); the replayed
  history saved again gives the same file; Play after the end re-executes; Close replay
  goes back to live runs with the inputs chosen before.
- An edited configuration replays exactly: the configuration pane shows the file's
  configuration read-only, the title says the edits, and the user's edits are back after.
- Save part way through a run gives a file with no end line, whose replay goes on to the
  end.
- A tampered file (re-digested): the window warns and shows the stored events in the
  trace, the event log and the timeline, with no snapshot, and every pane survives a
  seek over them; a truncated file, an edited file and an events file are refused with
  the error in the status bar.
- Stop during a replay leaves no live thread; Play again re-executes.
"""

import dataclasses
import json

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtWidgets import QFileDialog, QMessageBox                     # noqa: E402

from tabletop.config import PROFILES                                       # noqa: E402
from tabletop.gui.models import session_file as sf                         # noqa: E402

TIMEOUT = 30000   # ms
QUICK = ("surround-05", 3)      # the front page's quick start on the standard profile, which this file's runs read: 384 codelets, 3861 events since v5's fix (D-116 to D-120); 44 and 507 since item 08, on the v2 defaults of the time (46 and 496 in v1)
GOLDEN = ("surround-05", 1)     # a golden run on the standard profile: 228 codelets, 2690 events since v5 (D-116 to D-120); 62 and 901 since item 09 (81 and 806 in v1)
SHORT = ("buridan-03", 2)       # a short library run (v1's shortest on seeds 1 to 5: 15 codelets,
SHORT_EVENTS, SHORT_CODELETS = 1521, 162   # buridan-03 seed 2 on the standard profile since v5's fix (D-116 to D-120), the window's history equal to the plain run's; 1571 and 171 in v4, D-115 (1144 and 125 in v3, D-114; 913 and 84 since item 09; 495 and 46 at items 07 and 08; 687 and 66 at item 05; 386 and 36 at item 04; 722 and 79 after item 03; 190 and 15 in v1; G-058, G-059)


def golden_file(golden_runs, path, events=None):
    """The golden run written as a session file at `path`, with no run through the window."""
    ref = golden_runs[GOLDEN]
    sf.write_session(str(path), sf.header_of(GOLDEN[0], PROFILES["standard"], GOLDEN[1]),
                     ref.events if events is None else events)
    return ref


def records_of(window):
    return list(window.history.records)


def run_to_end(qtbot, window, problem, seed, config=None):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_run(problem, PROFILES["standard"] if config is None else config, seed)
        window.controller.run_to_end()
    assert window.controller.wait(TIMEOUT / 1000)
    return window.outcome


def replay_to_end(qtbot, window):
    """The opened replay, run to its end; returns the outcome."""
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.run_to_end()
    assert window.controller.wait(TIMEOUT / 1000)
    return window.outcome


def opened(qtbot, window, path):
    """Open the file: the replay is paused at its RunStarted event."""
    assert window.open_session(str(path))
    qtbot.waitUntil(lambda: window.state == "paused" and window.position == 0, timeout=TIMEOUT)
    assert window.replay_source == str(path)
    assert window.history.events[0].name == "RunStarted"


def redigested(path):
    """The file's lines with a fresh end line (the engine, not the file, then differs)."""
    lines = path.read_text(encoding="utf-8").splitlines()
    body = lines[1:-1]
    return lines[:-1] + [json.dumps({"end": len(body), "digest": sf.digest_of(body)},
                                    separators=(",", ":"))]


@pytest.fixture
def no_warning_box(monkeypatch):
    """`QMessageBox.warning` recorded, not shown (the test runs offscreen and unattended)."""
    shown = []
    monkeypatch.setattr(QMessageBox, "warning",
                        lambda parent, title, text, *a, **k: shown.append((title, text)))
    return shown


# -- the menu and the dialogs ------------------------------------------------------------------------


# -- save then open (G-049) --------------------------------------------------------------------------


def test_an_edited_configuration_replays_exactly(window, qtbot, tmp_path):
    view = window.config_view
    assert view.model.set_value("win_bonus", 7)             # the user's own edit, kept
    config = dataclasses.replace(PROFILES["human"], max_codelets=20)
    live = run_to_end(qtbot, window, "surround-05", 1, config)
    assert live.result.capped and live.result.codelets == 20
    ref = records_of(window)
    assert window.windowTitle() == "Tabletop — surround-05, seed 1 (human; edited: max_codelets)"
    path = tmp_path / "edited.jsonl"
    assert window.save_session(path)
    file = sf.read_session(str(path))
    assert file.header.profile == "human" and file.header.edits == {"max_codelets": 20}
    opened(qtbot, window, path)
    assert window.windowTitle() == ("Tabletop — replaying edited.jsonl: surround-05, seed 1 "
                                    "(human; edited: max_codelets)")
    assert view.replay == config and not view.profile_combo.isEnabled()
    assert view.row_of("max_codelets").value == 20 and view.row_of("max_codelets").edited
    assert not view.row_of("win_bonus").edited
    assert window.controls_view.values().profile == "human"
    outcome = replay_to_end(qtbot, window)
    assert outcome.result == live.result and records_of(window) == ref
    assert window.statusBar().currentMessage() == f"replay of edited.jsonl: verified, {len(ref)} events"
    assert window.stats_view.texts()["config"] == "human: max_codelets=20, type_weight=50"
    window.close_replay()
    assert view.replay is None and view.model.edits == {"win_bonus": 7}
    assert view.row_of("win_bonus").edited and view.profile_combo.isEnabled()


# -- a file the engine no longer reproduces (PANES.md section 5) ---------------------------------


def test_a_truncated_file_is_refused_with_the_line(window, qtbot, tmp_path, golden_runs):
    path = tmp_path / "whole.jsonl"
    golden_file(golden_runs, path)
    lines = path.read_text(encoding="utf-8").splitlines()
    torn = tmp_path / "torn.jsonl"
    torn.write_text("\n".join(lines[:50]) + "\n" + lines[50][:30], encoding="utf-8")
    assert not window.open_session(str(torn))
    message = window.statusBar().currentMessage()
    assert message.startswith("cannot open torn.jsonl, line 51: not valid JSON (truncated?)")
    assert window.replay_source is None and not window.controller.is_alive()
    assert "replaying" not in window.windowTitle()
    cut = tmp_path / "cut.jsonl"
    cut.write_text("\n".join(lines[:-1]) + "\n", encoding="utf-8")
    assert not window.open_session(str(cut))
    assert "the run ended but the end line is missing" in window.statusBar().currentMessage()
    events = tmp_path / "events.jsonl"
    events.write_text("\n".join(lines[1:-1]) + "\n", encoding="utf-8")     # the CLI's --json
    assert not window.open_session(str(events))
    assert window.statusBar().currentMessage().startswith(
        "cannot open events.jsonl, line 1: not a tabletop session file")
    assert not window.open_session(str(tmp_path / "missing.jsonl"))
    assert window.statusBar().currentMessage().startswith("cannot open missing.jsonl: ")
    assert window.open_session(str(path))
    qtbot.waitUntil(lambda: window.state == "paused", timeout=TIMEOUT)
    window.close_replay()


def stopped_replay(qtbot, window, path, golden_runs):
    """The golden run's file opened, played past its twentieth event and stopped: no live
    thread, the status and the stats say so, and the history is the file's prefix."""
    ref = list(golden_file(golden_runs, path).records)
    opened(qtbot, window, path)
    window.controller.set_delay(0.001)
    window.play()
    qtbot.waitUntil(lambda: window.position > 20, timeout=TIMEOUT)
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.stop()
    assert not window.controller.is_alive() and window.outcome.stopped
    message = window.statusBar().currentMessage()
    assert message.startswith("replay of s.jsonl: stopped after") and "verified" in message
    assert window.stats_view.texts()["source"] == "replay of s.jsonl: stopped"
    assert records_of(window) == ref[:len(window.history.events)]
    return ref


def test_stop_during_a_replay_leaves_no_live_thread(window, qtbot, tmp_path, golden_runs):
    """The quick part; Play again after the stop, a whole replay through the window, is the
    slow suite's `test_stop_during_a_replay_then_play_again_starts_over`."""
    stopped_replay(qtbot, window, tmp_path / "s.jsonl", golden_runs)
    window.close_replay()
    assert window.replay_source is None and not window.controller.is_alive()

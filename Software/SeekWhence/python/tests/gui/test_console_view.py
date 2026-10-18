"""Loop0004 item 04: the console pane (`seekwhence/gui/console_view.py`) in the window,
offscreen, pytest-qt (G-018, G-019, G-020).

- The console's text of each appendix run equals the CLI's output for the same seed and
  inputs, in the mode shown and after the Verbose/Terse switch [M86 pp. 164-179].
- Lines after the cursor are greyed when seeking; a click on a line seeks to its event
  when no run is live, and is refused while one is.
- `show` at a past term equals what the CLI printed then [M86 pp. 171, 174, 178].
- The command line: a command with no interactive run live prints at the cursor; in an
  interactive run every word typed, commands included, is a session input and the console
  equals the session's own transcript; the buttons do the same.
- Find, Save transcript, Copy; a new run clears.
"""

import io
import subprocess
import sys
from pathlib import Path

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import Qt                                # noqa: E402

from seekwhence.events import Recorder                        # noqa: E402
from seekwhence.gui.models import presets                     # noqa: E402
from seekwhence.gui.models.commands import command_lines      # noqa: E402
from seekwhence.session import PROMPT, Session                # noqa: E402
from seekwhence.trace import event_record                     # noqa: E402

from golden import APPENDIX                                   # noqa: E402

PYTHON_DIR = Path(__file__).resolve().parents[2]
TIMEOUT = 30000   # ms
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def cli_output(inputs, mode):
    command = [sys.executable, "-m", "seekwhence", "--seed", str(inputs.seed), "--terms",
               *map(str, inputs.lines), "--trace", mode]
    if inputs.answers:
        command += ["--answers", *inputs.answers]
    return subprocess.run(command, cwd=PYTHON_DIR, capture_output=True, text=True,
                          check=True).stdout


def plain(config, seed, lines, answers):
    rec, out = Recorder(), io.StringIO()
    session = Session(config, seed, out=out, lines=list(lines), answers=list(answers),
                      observers=[rec])
    session.run()
    return session, [event_record(e) for e in rec.events], out.getvalue()


def run_to_end(qtbot, window, inputs):
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
    return window.outcome


def type_line(qtbot, window, text):
    edit = window.console_view.command_edit
    qtbot.keyClicks(edit, text)
    qtbot.keyClick(edit, Qt.Key.Key_Return)


def click(qtbot, button):
    assert button.isEnabled()
    qtbot.mouseClick(button, Qt.MouseButton.LeftButton)


# -- the transcript (G-018) ---------------------------------------------------------------------------

def test_the_console_dock_holds_the_view(window):
    view = window.console_view
    assert window.console_dock.widget() is view and view.pane_id == "P8"
    assert view.mode == "verbose" and view.text() == "" and view.list.row_count() == 0
    assert set(view.command_buttons) == {"show", "show-hypothesis", "show-seq", "show-parse"}
    assert view.count_label.text() == "0 lines"


@pytest.mark.parametrize("run_id", [RUN1, RUN2])
def test_the_text_equals_the_cli_output_in_both_modes(window, qtbot, run_id):
    inputs = presets.appendix_inputs(run_id)
    outcome = run_to_end(qtbot, window, inputs)
    view = window.console_view
    native = "verbose" if inputs.config.verbose else "terse"
    assert view.mode == "verbose"
    assert view.text(native) == outcome.output + outcome.result["summary"] + "\n"
    assert view.text() == cli_output(inputs, "verbose")
    view.set_mode("terse")
    assert view.mode == "terse" and view.text() == cli_output(inputs, "terse")
    assert view.count_label.text() == f"{view.list.row_count()} lines"
    assert view.paints > 0 and view.events == len(window.history)
    assert view.list.now_row() == view.list.row_count() - 1


def test_lines_after_the_cursor_are_greyed_and_a_click_seeks(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view, lst = window.console_view, window.console_view.list
    events = window.history.events
    guess = next(i for i, e in enumerate(events) if e.kind == "guess")
    assert window.seek(guess)
    now = lst.now_row()
    assert lst.row_text(now) == "3 7 3 7" and lst.row_index(now) == guess     # [M86 p. 166]
    assert not lst.is_future(now) and lst.is_future(now + 1)
    assert lst.is_future(lst.row_count() - 1)
    # A click on a row seeks to its event.
    row = 10
    lst.scroll_to(row)
    y = lst.row_rect(row).center().y()
    qtbot.mouseClick(lst.viewport(), Qt.MouseButton.LeftButton, pos=lst.row_rect(row).center())
    assert lst.row_at(y) == row
    assert window.position == lst.row_index(row) and lst.cursor_row == row
    assert not lst.is_future(row) and lst.is_future(row + 1) == (
        lst.row_index(row + 1) > window.position)
    # Enter on the keyboard's row seeks too.
    lst.cursor_row = 20
    qtbot.keyClick(lst, Qt.Key.Key_Return)
    assert window.position == lst.row_index(20)
    qtbot.keyClick(lst, Qt.Key.Key_End)
    assert lst.cursor_row == lst.row_count() - 1


def test_a_click_is_refused_while_a_run_is_live(window, qtbot):
    window.start_session(presets.interactive_inputs(54))
    window.controller.run_to_end()
    qtbot.waitUntil(lambda: window.feed is not None and window.feed.waiting, timeout=TIMEOUT)
    window.console_view.list.activate(0)
    assert "stop the run" in window.statusBar().currentMessage()
    window.stop()
    assert not window.controller.is_alive()


def test_show_at_a_past_term_equals_what_the_cli_printed_then(window, qtbot):
    inputs = presets.appendix_inputs(RUN3)
    run_to_end(qtbot, window, inputs)
    view = window.console_view
    cli = cli_output(inputs, "verbose").split("\n")
    starts = [i for i, t in enumerate(cli) if t == "please enter a term: show"]
    shows = [index for index, line in view.model.reads if line == "show"]
    assert len(starts) == len(shows) == 3                                     # [M86 pp. 171, 174, 178]
    for index, start in zip(shows, starts):
        assert window.seek(index)
        lines = window.show_command("show")
        assert cli[start + 1:start + 1 + len(lines)] == lines
        assert view.text().endswith("".join(t + "\n" for t in ["", PROMPT + "show", *lines]))
    assert window.seek(len(window.history) - 1)
    assert window.show_command("show-hypothesis") == ["(S-group 1 (Countup 2))"]  # [M86 p. 179]


# -- the command line (G-019) -------------------------------------------------------------------------

def test_a_command_with_no_run_live_prints_at_the_cursor(window, qtbot):
    view = window.console_view
    type_line(qtbot, window, "show-hypothesis")
    assert view.text() == "\n" + PROMPT + "show-hypothesis\nnil\n"
    assert view.command_edit.text() == ""
    type_line(qtbot, window, "3")
    assert "needs an interactive run" in window.statusBar().currentMessage()
    assert view.text().count("\n") == 3
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    n = view.list.row_count()
    click(qtbot, view.command_buttons["show-seq"])
    assert [view.list.row_text(r) for r in range(n, n + 3)] == ["", PROMPT + "show-seq", "3 7 3 7"]
    assert view.model.inputs == [str(x) for x in presets.appendix_inputs(RUN1).lines]


def test_lines_typed_live_are_session_inputs_and_the_console_equals_the_session(window, qtbot):
    inputs = presets.interactive_inputs(54)
    view = window.console_view
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(inputs)
        window.controller.run_to_end()
        qtbot.waitUntil(lambda: window.feed is not None and window.feed.waiting, timeout=TIMEOUT)
        type_line(qtbot, window, "3 7")
        qtbot.waitUntil(lambda: window.feed.read == ["3", "7"] and window.feed.waiting
                        and view.model.at_ask, timeout=TIMEOUT)
        assert window.enter_console_line("show-seq") == 1
        assert view.text().endswith("\n" + PROMPT + "show-seq\n3 7\n")
        qtbot.waitUntil(lambda: window.feed.read == ["3", "7", "show-seq"] and window.feed.waiting,
                        timeout=TIMEOUT)
        click(qtbot, view.command_buttons["show-hypothesis"])
        assert view.text().endswith("\n" + PROMPT + "show-hypothesis\nnil\n")
        type_line(qtbot, window, "3")
        type_line(qtbot, window, "7")
        qtbot.waitUntil(lambda: window.guess_panel.waiting, timeout=TIMEOUT)
        click(qtbot, window.guess_panel.ok_button)
    assert window.feed.read == ["3", "7", "show-seq", "show-hypothesis", "3", "7"]
    assert view.model.inputs == window.feed.read
    assert view.text() == window.outcome.output          # the session's own transcript
    session, records, text = plain(inputs.config, 54, [3, 7, "show-seq", "show-hypothesis", 3, 7],
                                   ["ok"])
    assert [event_record(e) for e in window.history.events] == records
    assert view.text() == text and window.outcome.result["draws"] == session.world.rng.draws


def test_a_command_typed_before_the_prompt_waits_for_it(window, qtbot):
    view = window.console_view
    with qtbot.waitSignal(window.run_finished, timeout=TIMEOUT):
        window.start_session(presets.interactive_inputs(54))
        window.controller.run_to_end()
        qtbot.waitUntil(lambda: window.feed is not None and window.feed.waiting, timeout=TIMEOUT)
        # Three lines at once: the command is read at the second prompt, in order.
        assert window.enter_console_line("3 show-seq 7") == 3
        qtbot.waitUntil(lambda: window.feed.read == ["3", "show-seq", "7"] and window.feed.waiting,
                        timeout=TIMEOUT)
        texts = [line.text for line in view.model.lines["verbose"]]
        i = texts.index(PROMPT + "show-seq")
        assert texts[i + 1] == "3" and texts[i - 1] == ""
        assert PROMPT + "3" in texts[:i] and PROMPT + "7" in texts[i:]
        window.end_input()
    assert window.outcome.result["outcome"] == "eof"
    assert view.text() == window.outcome.output and view.text().endswith(PROMPT + "\n")


# -- find, save, copy (G-019) ---------------------------------------------------------------------------

def test_find_save_and_copy(window, qtbot, tmp_path):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    view, lst = window.console_view, window.console_view.list
    view.find_edit.setText("i have a guess")
    qtbot.keyClick(view.find_edit, Qt.Key.Key_Return)
    assert lst.cursor_row is not None and lst.row_text(lst.cursor_row) == "I have a guess!"
    first = lst.cursor_row
    assert view.find_next() == first                       # the only match: wraps to itself
    view.find_edit.setText("sparkler")
    row = view.find_next()
    assert row is not None and "Sparkler" in lst.row_text(row)
    view.find_edit.setText("no such text")
    assert view.find_next() is None and lst.cursor_row == row
    path = tmp_path / "transcript.txt"
    text = window.save_transcript(path)
    assert path.read_text(encoding="utf-8") == text == view.text()
    assert window.save_transcript(tmp_path / "terse.txt", "terse") == view.text("terse")
    assert window.copy_transcript() == view.text()
    assert window.save_transcript_action.isEnabled() and window.copy_action.isEnabled()


def test_a_new_run_clears_and_tooltips_show_the_line(window, qtbot):
    run_to_end(qtbot, window, presets.appendix_inputs(RUN3))
    view = window.console_view
    assert view.list.row_count() > 400
    run_to_end(qtbot, window, presets.appendix_inputs(RUN1))
    assert view.text() == cli_output(presets.appendix_inputs(RUN1), "verbose")
    assert view.list.row_count() == 101
    assert view.list.row_at(10 ** 6) is None

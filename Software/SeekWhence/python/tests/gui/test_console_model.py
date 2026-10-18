"""Loop0004 item 04: the console model (`seekwhence/gui/models/console.py`), Qt-free
(G-018, G-019).

- The console text of each appendix run equals the CLI's output for the same seed and
  inputs (`python3 -m seekwhence --seed ... --terms ...` in a subprocess), verbose and
  terse [M86 pp. 164-179], and the session's own transcript.
- `show` at a past term equals what the CLI printed then [M86 pp. 171, 174-175, 178].
- An interactive run whose lines are put as they are typed, commands, an unknown word,
  `bye` and the end of the input included, equals the session's transcript.
- The lines are tagged with their events; a seek adds nothing; a command printed at the
  cursor is not an input; find.
"""

import io
import subprocess
import sys
from pathlib import Path

import pytest

from seekwhence.gui.models.commands import command_lines
from seekwhence.gui.models.console import MODES, ConsoleModel
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.tap import Tap
from seekwhence.session import HINT, PROMPT, Session

from golden import APPENDIX, appendix_inputs

PYTHON_DIR = Path(__file__).resolve().parents[2]
RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


class Feed:
    """An observer that gives the model each event with its index, after the tap has
    taken its snapshot, as the window does."""

    def __init__(self, model, tap):
        self.model, self.tap, self.index = model, tap, -1

    def on_event(self, event):
        self.index += 1
        self.tap.on_event(event)
        self.model.set_position(self.index)
        self.model.on_event(event)


class Outcome:
    """A `RunOutcome` as `ConsoleModel.finish` reads it."""

    def __init__(self, session):
        self.result = {"outcome": session.outcome, "summary": session.summary()}


def run_with_model(run_id, seed=None, lines=None, answers=None, config=None):
    """An appendix run (or given inputs) with the model observing; returns the session,
    the model, its log and the session's own transcript."""
    cfg, run_lines, run_answers = appendix_inputs(run_id) if run_id else (config, lines, answers)
    seed = dict(APPENDIX)[run_id] if seed is None else seed
    log, out = EventLog(), io.StringIO()
    tap = Tap(log)
    model = ConsoleModel(log)
    model.begin(run_lines)
    session = Session(cfg, seed, out=out, lines=list(run_lines), answers=list(run_answers),
                      observers=[Feed(model, tap)])
    tap.attach(session.world)
    session.run()
    model.finish(Outcome(session))
    return session, model, log, out.getvalue()


def cli_output(seed, lines, answers, mode):
    """What `python3 -m seekwhence` prints for the inputs in the mode."""
    command = [sys.executable, "-m", "seekwhence", "--seed", str(seed), "--terms",
               *map(str, lines), "--trace", mode]
    if answers:
        command += ["--answers", *answers]
    done = subprocess.run(command, cwd=PYTHON_DIR, capture_output=True, text=True, check=True)
    return done.stdout


# -- the appendix runs against the CLI (DECISION D-055) ---------------------------------------------

@pytest.mark.parametrize("run_id,seed", APPENDIX)
def test_the_text_equals_the_cli_output_in_both_modes(run_id, seed):
    session, model, _log, text = run_with_model(run_id)
    native = "verbose" if session.config.verbose else "terse"
    assert model.text(native) == text + session.summary() + "\n"
    _config, lines, answers = appendix_inputs(run_id)
    for mode in MODES:
        assert model.text(mode) == cli_output(seed, lines, answers, mode)
    assert model.lines["verbose"] != model.lines["terse"]
    assert model.outcome == session.outcome and model.scripted


def test_run_1s_lines_as_meredith_printed_them():
    _session, model, _log, _text = run_with_model(RUN1)
    texts = [line.text for line in model.lines["verbose"]]
    assert texts[:3] == ["-> (startup)", "", "please enter a term: 3"]           # [M86 p. 164]
    assert "doing task Sparkler-plus on (glint1 glint1 2)" in texts              # [M86 p. 164]
    i = texts.index("please enter a term: show-hypothesis")
    assert texts[i + 1] == "(Cycle 3 2 (3 7))"                                   # [M86 p. 166]
    assert texts[-6:] == ["hypothesis: (Cycle 3 2 (3 7))", "3 7 3 7", "",
                          "enter no if wrong, ok if right ok", "bye",
                          "outcome: ok after 4 terms, 52 tasks (seed 54): (Cycle 3 2 (3 7))"]
    terse = [line.text for line in model.lines["terse"]]
    assert not any(t.startswith("doing task") for t in terse)                    # [M86 p. 167]
    assert "please enter a term: show-hypothesis" in terse


def test_show_at_a_past_term_equals_what_the_cli_printed_then():
    session, model, log, _text = run_with_model(RUN3)
    cli = cli_output(30, appendix_inputs(RUN3)[1], appendix_inputs(RUN3)[2], "verbose").split("\n")
    shows = [(index, line) for index, line in model.reads if line == "show"]
    assert len(shows) == 3                                                       # [M86 pp. 171, 174, 178]
    starts = [i for i, t in enumerate(cli) if t == "please enter a term: show"]
    assert len(starts) == 3
    for (index, _line), start in zip(shows, starts):
        assert log.events[index].kind == "prompt"
        expected = command_lines(log.state_at(index), "show")
        assert cli[start + 1:start + 1 + len(expected)] == expected
    # The last `show` comes before the sixth term [M86 p. 178]: five terms then, six now.
    assert expected[1] == "1 2 1 2 3" and session.show_seq() == "1 2 1 2 3 1"


# -- the lines and their events (G-018) ---------------------------------------------------------------

def test_the_lines_are_tagged_with_their_events():
    _session, model, log, _text = run_with_model(RUN1)
    for line in model.lines["verbose"]:
        event = log.events[line.index]
        if line.text.startswith("doing task"):
            assert event.kind == "chosen" and line.source == "trace"
        elif line.text.startswith("please enter a term: ") and line.text[21:].isdigit():
            assert event.kind == "term" and line.source == "trace"
        elif line.text.startswith("please enter a term"):
            assert event.kind in ("start", "prompt") and line.source == "ask"
        elif line.text.startswith("outcome:"):
            assert line.source == "summary" and line.index == len(log.events) - 1
    assert model.lines["verbose"][0].index == 0 and model.lines["verbose"][0].source == "trace"
    assert model.cursor_row("verbose", len(log.events) - 1) == len(model.lines["verbose"]) - 1
    prompt = next(i for i, e in enumerate(log.events) if e.kind == "prompt")
    row = model.cursor_row("verbose", prompt)         # the prompt itself prints nothing
    assert model.lines["verbose"][row].index <= prompt
    assert not model.is_future("verbose", row, prompt)
    assert model.is_future("verbose", row + 1, prompt)
    assert model.cursor_row("verbose", -1) is None and ConsoleModel().cursor_row("terse") is None


def test_a_seek_adds_nothing_and_a_new_run_clears():
    _session, model, log, _text = run_with_model(RUN1)
    before = {mode: list(model.lines[mode]) for mode in MODES}
    for index in list(range(len(log.events))) + [5, 0]:
        model.set_position(index)
        model.on_event(log.events[index])
    assert model.lines == before and model.seen == len(log.events) - 1
    # A new run, announced by `begin`, clears at its `start`.
    model.begin([3, 7])
    model.set_position(0)
    model.on_event(log.events[0])
    assert [line.text for line in model.lines["verbose"]] == ["-> (startup)"]
    assert model.queue == ["7"] and model.inputs == ["3", "7"] and model.reads == [(0, "3")]
    assert not model.at_ask and model.closed and model.scripted


def test_a_command_printed_at_the_cursor_is_not_an_input():
    _session, model, log, _text = run_with_model(RUN1)
    inputs = list(model.inputs)
    n = len(model.lines["verbose"])
    prompt = next(i for i, e in enumerate(log.events) if e.kind == "prompt")
    model.set_position(prompt)
    assert model.print_command("show-seq") == ["3"]
    added = model.lines["verbose"][n:]
    assert [line.text for line in added] == ["", PROMPT + "show-seq", "3"]
    assert all(line.index == prompt and line.source == "command" for line in added)
    assert model.lines["terse"][-3:] == added
    assert model.inputs == inputs and model.cursor_row("verbose", prompt) < n
    model.set_position(-1)
    assert ConsoleModel().print_command("show-hypothesis") == ["nil"]


def test_find():
    _session, model, _log, _text = run_with_model(RUN1)
    row = model.find("verbose", "i have a guess")
    assert row is not None and model.lines["verbose"][row].text == "I have a guess!"
    assert model.find("verbose", "I HAVE A GUESS", row + 1) == row          # wraps round
    assert model.find("verbose", "no such text") is None
    assert model.find("verbose", "") is None and ConsoleModel().find("terse", "x") is None


# -- an interactive run (G-019) ------------------------------------------------------------------------

class Typist:
    """The stream of an interactive session: each console line is put in the model as the
    session asks for it (the user typing at the prompt); an answer is read without that."""

    def __init__(self, model, items):
        self.model, self.items = model, list(items)

    def readline(self):
        if not self.items:
            return ""
        kind, text = self.items.pop(0)
        if kind == "line":
            self.model.put(text)
        return text + "\n"


def interactive(seed, items):
    log, out = EventLog(), io.StringIO()
    tap = Tap(log)
    model = ConsoleModel(log)
    model.begin(None)
    session = Session(None, seed, out=out, inp=Typist(model, items),
                      observers=[Feed(model, tap)])
    tap.attach(session.world)
    session.run()
    model.finish(Outcome(session))
    return session, model, out.getvalue()


def test_an_interactive_run_typed_as_it_goes_equals_the_session():
    items = [("line", "3"), ("line", "7"), ("line", "show"), ("line", "foo"), ("line", "3"),
             ("line", "7"), ("answer", "ok")]
    session, model, text = interactive(54, items)
    assert session.outcome == "ok"
    assert model.text("verbose") == text                      # no summary: not scripted
    assert not model.scripted and model.inputs == ["3", "7", "show", "foo", "3", "7"]
    texts = [line.text for line in model.lines["verbose"]]
    assert "please enter a term: show" in texts and HINT in texts
    assert texts[texts.index("please enter a term: foo") + 1] == HINT
    assert texts[-1] == "bye"


def test_bye_and_the_end_of_the_input():
    session, model, text = interactive(54, [("line", "3"), ("line", "bye")])
    assert session.outcome == "bye" and model.text("verbose") == text and model.ended
    assert model.text("verbose").endswith("please enter a term: bye\n")
    session, model, text = interactive(54, [("line", "3"), ("line", "7")])
    assert session.outcome == "eof" and model.text("verbose") == text
    assert text.endswith("\nplease enter a term: \n") and model.ended and model.closed


def test_lines_put_before_the_ask_wait_for_it():
    model = ConsoleModel()
    model.begin(None)
    model.put("show-seq")
    model.put("3")
    assert model.queue == ["show-seq", "3"] and model.lines["verbose"] == []
    _session, _m, log, _text = run_with_model(RUN1)
    model.set_position(0)
    model.on_event(log.events[0])
    texts = [line.text for line in model.lines["verbose"]]
    assert texts == ["-> (startup)", "", "please enter a term: show-seq", ""]
    assert model.queue == [] and not model.at_ask and model.reads == [(0, "show-seq"), (0, "3")]

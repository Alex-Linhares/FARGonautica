"""The session: the console protocol of the transcripts [M86 pp. 164-179] (DECISIONS D-053,
D-054, D-064, D-069)."""

import io

import pytest

from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.filtering import ScriptedAnswerer
from seekwhence.run import enter_term, new_world, run_until_prompt
from seekwhence.session import (ANSWER_PROMPT, COMMANDS, HINT, OUTCOMES, PROMPT, Console,
                                ConsoleAnswerer, Session, glom_line, parse_line)
from seekwhence.trace import JsonLines, Trace, event_record

FENCE_SEED = 4      # `3 7 3` gloms the fence `(3 7)`, as runs 1 and 2 did (test_filtering)


def scripted(lines, seed=FENCE_SEED, answers=(), **knobs):
    out = io.StringIO()
    session = Session(Config(**knobs), seed, out=out, lines=lines, answers=answers)
    outcome = session.run()
    return session, out.getvalue().splitlines(), outcome


def interactive(text, seed=FENCE_SEED, answers=(), **knobs):
    out = io.StringIO()
    session = Session(Config(**knobs), seed, out=out, inp=io.StringIO(text), answers=answers)
    outcome = session.run()
    return session, out.getvalue().splitlines(), outcome


# ---- parsing a console line (DECISION D-054) ----------------------------------------------------

def test_parse_line_terms_commands_bye_and_the_rest():
    """A term is a nonnegative integer; the four commands and `bye` [M86 pp. 166-179]
    (DECISION D-054)."""
    assert parse_line("3") == ("term", 3)
    assert parse_line(" 12 ") == ("term", 12)
    assert parse_line("0") == ("term", 0)
    for name in COMMANDS:
        assert parse_line(name) == ("command", name)
    assert parse_line("SHOW") == ("command", "show")
    assert parse_line("bye") == ("bye", None)
    assert parse_line("-1") == ("unknown", "-1")
    assert parse_line("nope") == ("unknown", "nope")
    assert parse_line("") == ("unknown", "")
    assert parse_line("3 7") == ("unknown", "3 7")


# ---- the console (DECISION D-069) --------------------------------------------------------------

def test_console_reads_a_script_and_echoes_it_after_the_prompt():
    """`please enter a term: 3` with the input on the prompt line [M86 p. 164]."""
    out = io.StringIO()
    console = Console(out, lines=[3, "show"])
    assert console.scripted and console.echo
    assert console.ask(PROMPT) == "3"
    assert console.ask(PROMPT) == "show"
    assert console.ask(PROMPT) is None
    assert out.getvalue() == f"{PROMPT}3\n{PROMPT}show\n{PROMPT}\n"
    assert console.asked == ["3", "show"]


def test_console_reads_a_stream_and_echoes_when_it_is_not_a_terminal():
    out = io.StringIO()
    console = Console(out, inp=io.StringIO("7\nbye\n"))
    assert not console.scripted and console.echo
    assert console.ask(PROMPT) == "7"
    assert console.ask(PROMPT) == "bye"
    assert console.ask(PROMPT) is None
    assert out.getvalue() == f"{PROMPT}7\n{PROMPT}bye\n{PROMPT}\n"


def test_console_echo_can_be_turned_off_as_for_a_terminal():
    out = io.StringIO()
    console = Console(out, inp=io.StringIO("7\n"), echo=False)
    assert console.ask(PROMPT) == "7"
    assert out.getvalue() == PROMPT


def test_console_tell_writes_a_prompt_with_an_answer():
    """`enter no if wrong, ok if right ok` [M86 p. 166]."""
    out = io.StringIO()
    Console(out, lines=[]).tell(ANSWER_PROMPT, "ok")
    assert out.getvalue() == ANSWER_PROMPT + "ok\n"


# ---- the answerer (DECISIONS D-053, D-064) -----------------------------------------------------

class TestConsoleAnswerer:
    def test_scripted_answers_first_then_the_judgement_against_the_terms(self):
        """DECISION D-053: the scripted answers in order, then the guess judged by D-033."""
        out = io.StringIO()
        answerer = ConsoleAnswerer(Console(out, lines=[]), answers=["nope"], terms=[3, 7, 3, 7])
        assert answerer.answer(None, "f", "b", (3, 7, 3, 7)) == "nope"
        assert answerer.answer(None, "f", "b", (3, 7, 3, 7)) == "ok"
        assert answerer.answer(None, "f", "b", (3, 7, 3, 3)) == "no"
        assert answerer.given == ["nope", "ok", "no"]
        assert out.getvalue().splitlines() == ["", ANSWER_PROMPT + "nope", "",
                                               ANSWER_PROMPT + "ok", "", ANSWER_PROMPT + "no"]

    def test_the_user_is_asked_when_nothing_is_scripted(self):
        """`enter no if wrong, ok if right` answered on the same line [M86 p. 166]."""
        out = io.StringIO()
        console = Console(out, inp=io.StringIO("ok\n"))
        answerer = ConsoleAnswerer(console)
        assert answerer.answer(None, "f", "b", (3,)) == "ok"
        assert out.getvalue() == "\n" + ANSWER_PROMPT + "ok\n"

    def test_the_end_of_the_input_is_a_refusal(self):
        out = io.StringIO()
        answerer = ConsoleAnswerer(Console(out, inp=io.StringIO("")))
        assert answerer.answer(None, "f", "b", (3,)) == ""


# ---- the console commands [M86 pp. 166-179] (DECISION D-054) ---------------------------------------

class TestCommands:
    def test_show_on_the_fence_seed_prints_the_block_of_pp_167_168(self):
        """`show` after `3 7 3`: the terms, the one bond, `glom1` and the gnoth-glom `glom3`,
        and gnoth1's nine fields [M86 pp. 167-168], line for line (the printout's page
        break falls between the gloms and `gnoths:`)."""
        session, lines, outcome = scripted([3, 7, 3, "show"], verbose=False)
        at = lines.index("please enter a term: show")
        assert lines[at + 1:at + 22] == [
            "terms of the sequence:", "3 7 3", "",
            "bonds:", "bond1 Same print-value (remote) -- (glint1 glint3)", "",
            "gloms:",
            "glom1 (Same print-value fence) --> (3 7) terms 1 to 2",
            "glom3 pseudo --> ((3 7))  terms 1 to 2", "",
            "gnoths:", "",
            "class: Gnoths", "name: gnoth1", "frame: 0", "plato-class: Cycle", "glom: glom3",
            "notes: nil", "form: (((Cycle 3 2 (3 7)) pure))", "state: stable", "range: (1 2)"]
        assert lines[at + 22:at + 24] == ["", "please enter a term: "]
        assert outcome == "no-terms"

    def test_show_hypothesis_show_seq_and_show_parse(self):
        """`(Cycle 3 2 (3 7))` [M86 p. 166]; `3 7 3` as `show-seq` prints the terms
        [M86 p. 169]; the parse in one pair of parentheses [M86 p. 174]."""
        session, lines, _ = scripted([3, 7, 3, "show-hypothesis", "show-seq", "show-parse"],
                                     verbose=False)
        assert session.show_hypothesis() == "(Cycle 3 2 (3 7))"
        assert session.show_seq() == "3 7 3"
        assert session.show_parse() == "(((3 7)))"
        for command, answer in [("show-hypothesis", "(Cycle 3 2 (3 7))"), ("show-seq", "3 7 3"),
                                ("show-parse", "(((3 7)))")]:
            at = lines.index(f"please enter a term: {command}")
            assert lines[at + 1] == answer and lines[at + 2] == ""

    def test_show_parse_after_reformulation_prints_the_gnoths(self):
        """`((1 2) (1))` [M86 p. 174] after `1 2 1` on the seeds of test_casting."""
        session, lines, _ = scripted([1, 2, 1, "show-parse"], seed=1, verbose=False)
        at = lines.index("please enter a term: show-parse")
        assert lines[at + 1] == "((1 2) (1))"

    def test_show_before_any_term_keeps_the_headings(self):
        session = Session(Config(), 1, out=io.StringIO(), lines=[])
        assert session.show_lines() == ["terms of the sequence:", "", "", "bonds:", "",
                                        "gloms:", "", "gnoths:", ""]
        assert session.show_hypothesis() == "nil"
        assert session.show_seq() == ""
        assert session.show_parse() == "()"

    def test_glom_line_spacing_follows_the_transcripts(self):
        """One space before `terms` for a bottom-up glom, two for a pseudo-glom
        [M86 pp. 167, 175]."""
        world = new_world(Config(), 1)
        for term in (1, 2):
            enter_term(world, term)
        from seekwhence import cytoplasm as cy
        glints = cy.glints(world)
        glom = cy.make_glom(world, ("Pred", "print-value", "group"), glints)
        assert glom_line(glom) == "glom1 (Pred print-value group) --> (1 2) terms 1 to 2"
        pseudo = cy.make_glom(world, ("gnoth",), [glom], pseudo=True)
        assert glom_line(pseudo) == "glom2 pseudo --> ((1 2))  terms 1 to 2"

    def test_an_unknown_command_raises(self):
        session = Session(Config(), 1, out=io.StringIO(), lines=[])
        with pytest.raises(ValueError):
            session.command_lines("help")


# ---- whole sessions (DECISION D-069) --------------------------------------------------------------

class TestSession:
    def test_run_1_scripted_from_startup_to_bye(self):
        """Run 1 as a script: `show-hypothesis` before the fourth term, the guess, `ok`
        and `bye` [M86 pp. 164-166]; the outcome is `ok`."""
        session, lines, outcome = scripted([3, 7, 3, "show-hypothesis", 7], answers=["ok"])
        assert outcome == "ok" and session.outcome == "ok"
        assert lines[:2] == ["-> (startup)", ""]
        assert lines[2].startswith("please enter a term: 3")
        at = lines.index("please enter a term: show-hypothesis")
        assert lines[at + 1] == "(Cycle 3 2 (3 7))"
        assert lines[-5:] == ["hypothesis: (Cycle 3 2 (3 7))", "3 7 3 7", "",
                        "enter no if wrong, ok if right ok", "bye"]
        assert session.reasons == ["empty", "empty", "call-term", "done"]
        assert session.summary() == ("outcome: ok after 4 terms, "
                                     f"{session.world.clock} tasks (seed {FENCE_SEED}): "
                                     "(Cycle 3 2 (3 7))")

    def test_a_guess_with_no_scripted_answer_is_judged_against_the_terms(self):
        """DECISION D-033: `ok` when the guess agrees with the scripted sequence, `no`
        when it does not (run 3's `nope` [M86 p. 175])."""
        session, lines, outcome = scripted([3, 7, 3, 7, 3, 7])
        assert outcome == "ok"
        assert "enter no if wrong, ok if right ok" in lines
        session, lines, outcome = scripted([1, 2, 1, 2, 3, 1, 2, 3, 4], seed=1)
        assert "enter no if wrong, ok if right no" in lines
        assert session.answerer.given[0] == "no"

    def test_the_script_running_out_ends_with_no_terms_and_a_bare_prompt(self):
        session, lines, outcome = scripted([3, 7], verbose=False)
        assert outcome == "no-terms"
        assert lines[-1] == "please enter a term: "
        assert session.summary().startswith("outcome: no-terms after 2 terms, ")
        assert session.summary().endswith(f"(seed {FENCE_SEED}): no hypothesis")

    def test_the_whole_run_cap_ends_with_capped(self):
        """DECISION D-011: `max_tasks` tasks in all."""
        session, lines, outcome = scripted([3, 7, 3, 7], max_tasks=15, verbose=False)
        assert outcome == "capped" and session.world.clock == 15
        assert session.reasons[-1] == "capped"
        assert not session.world.done

    def test_a_per_term_cap_prompts_again(self):
        """DECISION D-011: the per-term cap asks for the next term as a Call-term would."""
        session, lines, outcome = scripted([3, 7, 3], max_tasks_per_term=3, verbose=False)
        assert "term-cap" in session.reasons and outcome == "no-terms"

    def test_bye_quits_and_nothing_runs_after(self):
        session, lines, outcome = scripted([3, 7, "bye", 3], verbose=False)
        assert outcome == "bye" and session.world.terms == [3, 7]
        assert lines[-1] == "please enter a term: bye"

    def test_an_unknown_line_prints_the_hint_and_prompts_again(self):
        """DECISION D-054: re-prompted."""
        session, lines, outcome = scripted([3, "seven", 7], verbose=False)
        at = lines.index("please enter a term: seven")
        assert lines[at + 1] == HINT and lines[at + 2] == ""
        assert session.world.terms == [3, 7]

    def test_entering_after_the_end_is_an_error(self):
        session, _, _ = scripted([3, "bye"])
        with pytest.raises(RuntimeError):
            session.enter(7)

    def test_interactive_input_asks_the_answer_and_ends_at_eof(self):
        """The user types the terms and `ok` [M86 p. 166]; the end of the input is `eof`."""
        session, lines, outcome = interactive("3\n7\n3\n7\nok\n")
        assert outcome == "ok"
        assert lines[-2:] == ["enter no if wrong, ok if right ok", "bye"]
        session, lines, outcome = interactive("3\n7\n")
        assert outcome == "eof" and lines[-1] == "please enter a term: "
        session, lines, outcome = interactive("3\n7\n3\n7\nno\n3\n")
        assert outcome == "eof"
        assert "enter no if wrong, ok if right no" in lines
        assert session.world.terms == [3, 7, 3, 7, 3]

    def test_scripted_answers_are_used_before_asking(self):
        session, lines, outcome = interactive("3\n7\n3\n7\n", answers=["ok"])
        assert outcome == "ok" and "enter no if wrong, ok if right ok" in lines

    def test_the_session_gives_the_same_trace_as_the_engine_alone(self):
        """A driver only observes: the session's lines are the
        engine's trace from the same seed, with the prompts printed by the session."""
        session, lines, outcome = scripted([3, 7, 3, 7], answers=["ok"])
        recorder, trace = Recorder(), Trace(io.StringIO())
        world = new_world(Config(), FENCE_SEED, observers=[recorder, trace],
                          answerer=ScriptedAnswerer(["ok"]))
        for term in (3, 7, 3, 7):
            enter_term(world, term)
            run_until_prompt(world)
        assert lines == trace.lines
        assert [e.kind for e in session.recorder.events] == [e.kind for e in recorder.events]

    def test_same_seed_same_session_and_different_seeds_differ(self):
        a = scripted([3, 7, 3, 7, 3, 7])[1]
        b = scripted([3, 7, 3, 7, 3, 7])[1]
        c = scripted([3, 7, 3, 7, 3, 7], seed=FENCE_SEED + 1)[1]
        assert a == b and a != c

    def test_outcomes_are_the_listed_ones(self):
        assert OUTCOMES == ("ok", "bye", "eof", "no-terms", "capped")
        for lines, answers, expected in [([3, 7, 3, 7], ["ok"], "ok"), (["bye"], (), "bye"),
                                         ([3], (), "no-terms")]:
            assert scripted(lines, answers=answers)[2] == expected

    def test_terse_trace_prints_no_task_lines(self):
        """`Config.verbose` False is run 2's printout [M86 p. 167] (DECISION D-055)."""
        _, lines, _ = scripted([3, 7, 3], verbose=False)
        assert not any(line.startswith("doing task") for line in lines)
        _, lines, _ = scripted([3, 7, 3], verbose=True)
        assert any(line.startswith("doing task") for line in lines)


# ---- the JSON-lines trace (DECISION D-069) -------------------------------------------------------

class TestJsonLines:
    def test_event_record_has_the_kind_first_and_plain_values(self):
        from seekwhence.events import TaskChosen, TermEntered
        assert event_record(TermEntered(1, 3, 0)) == {"kind": "term", "n": 1, "term": 3,
                                                     "cleaned": 0}
        record = event_record(TaskChosen(5, 2, "Bonder", ("Same", "print-value", ("remote",),
                                                          "glint1", "glint3"), 10, "cyto"))
        assert list(record)[0] == "kind" and record["kind"] == "chosen"
        assert record["args"] == ["Same", "print-value", ["remote"], "glint1", "glint3"]

    def test_forms_and_structures_become_their_printed_text(self):
        from seekwhence.events import HypothesisMade
        from seekwhence.forms import parse
        from seekwhence.trace import json_value
        assert json_value(parse("(Cycle 3 2 (3 7))")) == "(Cycle 3 2 (3 7))"
        assert json_value((1, None, True, "a", 2.5)) == [1, None, True, "a", 2.5]
        record = event_record(HypothesisMade("(Cycle 3 2 (3 7))", "template"))
        assert record["kind"] == "hypothesis"

    def test_a_session_writes_one_line_per_event(self):
        import json
        out, sink = io.StringIO(), io.StringIO()
        json_lines = JsonLines(sink)
        session = Session(Config(verbose=False), FENCE_SEED, out=out, lines=[3, 7, 3, 7],
                          answers=["ok"], observers=[json_lines])
        session.run()
        records = [json.loads(line) for line in sink.getvalue().splitlines()]
        assert len(records) == json_lines.count == len(session.recorder.events)
        assert records[0]["kind"] == "start" and records[0]["seed"] == FENCE_SEED
        assert [r["kind"] for r in records] == [e.kind for e in session.recorder.events]
        assert records[-1]["kind"] == "ended"

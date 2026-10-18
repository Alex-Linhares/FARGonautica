"""The text trace imitates the transcripts [M86 pp. 164-179] (DECISION D-055)."""

import io

from seekwhence.config import Config
from seekwhence.events import Message, RunStarted, TaskChosen, TermEntered
from seekwhence.forms import parse
from seekwhence.run import new_world, run_until_prompt
from seekwhence.trace import Trace, args_text, task_line


def test_args_text_prints_the_transcripts_argument_lists():
    """`(glint2 glint1 10)` [M86 p. 164]; `(Same print-value (remote) glint1 glint3)`
    [M86 p. 164]; `(glom1 (Cycle 3 2 (3 7)))` [M86 p. 165]; `((S-group P-group) glom1)`
    [M86 p. 170]; `(S-group 8.0)` [M86 p. 174]."""
    assert args_text(("glint2", "glint1", 10)) == "(glint2 glint1 10)"
    assert args_text(("Same", "print-value", ("remote",), "glint1", "glint3")) \
        == "(Same print-value (remote) glint1 glint3)"
    assert args_text(("glom1", parse("(Cycle 3 2 (3 7))"))) == "(glom1 (Cycle 3 2 (3 7)))"
    assert args_text((("S-group", "P-group"), "glom1")) == "((S-group P-group) glom1)"
    assert args_text(("S-group", 8.0)) == "(S-group 8.0)"
    assert args_text(()) == "()"


def test_args_text_prints_structures_by_name_and_nil():
    """`Template-evaluator on nil` [M86 p. 171]."""
    class Glom:
        name = "glom7"
    assert args_text((Glom(), None, True)) == "(glom7 nil t)"


def test_task_lines():
    """`doing task Tester` and `doing task Sparkler-plus on (glint1 glint1 2)` [M86 p. 164]."""
    assert task_line("Tester", ()) == "doing task Tester"
    assert task_line("Sparkler-plus", ("glint1", "glint1", 2)) \
        == "doing task Sparkler-plus on (glint1 glint1 2)"
    assert task_line("Bonder", ("Same", "print-value", ("remote",), "glint1", "glint3")) \
        == "doing task Bonder on (Same print-value (remote) glint1 glint3)"


def test_the_trace_prints_the_opening_of_run_1():
    """`-> (startup)`, a blank line, `please enter a term: 3`, `doing task Sparkler-plus on
    (glint1 glint1 2)`, `doing task Dissolver` [M86 p. 164]."""
    out = io.StringIO()
    trace = Trace(out)
    trace.on_event(RunStarted(1, ()))
    trace.on_event(TermEntered(1, 3, 0))
    trace.on_event(TaskChosen(1, 1, "Sparkler-plus", ("glint1", "glint1", 2), 2, "cytoplasm"))
    trace.on_event(TaskChosen(2, 2, "Dissolver", (), 1, "cytoplasm"))
    trace.on_event(Message("gnoths constructed"))
    assert trace.lines == ["-> (startup)", "", "please enter a term: 3",
                           "doing task Sparkler-plus on (glint1 glint1 2)",
                           "doing task Dissolver", "gnoths constructed"]
    assert out.getvalue() == "\n".join(trace.lines) + "\n"


def test_the_terse_trace_prints_no_task_lines():
    """Run 2 is printed without `doing task` lines [M86 p. 167] (DECISION D-055)."""
    trace = Trace(io.StringIO(), verbose=False)
    trace.on_event(TaskChosen(1, 1, "Tester", (), 10, "cytoplasm"))
    trace.on_event(Message("bond2 --- between glint3glint4"))
    assert trace.lines == ["bond2 --- between glint3glint4"]


def test_the_trace_ignores_the_bookkeeping_events():
    from seekwhence.events import RunCapped, TaskPosted, TasksCleaned, TaskStale, TermRequested
    trace = Trace(io.StringIO())
    for event in (TaskPosted(1, "Tester", (), 10, "cytoplasm", 0), TaskStale("Tester", (), "gone"),
                  TasksCleaned("term", ()), TermRequested("empty", 1), RunCapped(5)):
        trace.on_event(event)
    assert trace.lines == []


def test_a_traced_run_prints_the_loops_lines():
    """`doing task Call-term` then the prompt [M86 p. 165]; `doing task empty-task`
    [M86 p. 170]."""
    out = io.StringIO()
    world = new_world(Config(), 3, observers=[Trace(out)])
    world.enter_term(1)
    world.post("Call-term")
    run_until_prompt(world)
    world.enter_term(2)
    run_until_prompt(world)
    assert out.getvalue().splitlines() == ["-> (startup)", "", "please enter a term: 1",
                                           "doing task Call-term", "", "please enter a term: 2",
                                           "doing task empty-task"]


def test_the_trace_defaults_to_stdout(capsys):
    Trace().on_event(Message("hello"))
    assert capsys.readouterr().out == "hello\n"


def test_a_lone_nil_argument_prints_as_on_nil():
    """`doing task Template-evaluator on nil` [M86 p. 171]: the evaluator the
    Template-scout posts when it applies a form itself carries a nil argument
    (DECISION D-060)."""
    assert task_line("Template-evaluator", (None,)) == "doing task Template-evaluator on nil"
    assert task_line("Template-evaluator", ()) == "doing task Template-evaluator"


def test_set_out_is_printed_by_the_terse_trace_only():
    """`set-out -- validity: 0` follows the filtering line in the terse run 2 [M86 p. 168]
    and is absent at both refutations of the verbose run 3 [M86 pp. 171-172, 176]
    (DECISION D-055, item 21)."""
    from seekwhence.events import HypothesisRefuted
    terse, verbose = Trace(io.StringIO(), verbose=False), Trace(io.StringIO(), verbose=True)
    event = HypothesisRefuted("(Cycle 3 2 (3 7))", 4, 3, 7)
    terse.on_event(event)
    verbose.on_event(event)
    assert terse.lines == ["set-out -- validity: 0"]
    assert verbose.lines == []

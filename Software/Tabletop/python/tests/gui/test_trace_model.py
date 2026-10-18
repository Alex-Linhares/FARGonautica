"""The trace, the event rows and the timeline models (item 11; `models/trace.py`,
`models/event_log.py`'s rows, `models/timeline.py`; G-042, G-043), Qt-free:

- the trace text of `surround-05` seed 3 equals `python3 -m tabletop surround-05 --seed 3`'s
  standard output (a subprocess), and the quiet form equals `--quiet`'s; on the four golden
  runs the lines equal the engine's own `TextTrace` fed directly;
- the JSON lines equal `--json -`'s output (a subprocess) and `observe.json_line` per event;
- the lines' tags: every codelet line starts at the `CodeletChosen` of its ordinal, the
  notes under it share its start, the first line starts at 0 and the report line at the
  `RunEnded`; `line_of`, `block_rows` and `is_future`; feeding event by event equals feeding
  all at once; a new run forgets;
- the rows: `row_text` and `event_text`;
- the timeline's markers equal the events (the first notice, every correspondence built,
  competition, Worldview change, the first find-answer posting, the answer; the push and
  the pop on the push run of G-035), `marker_x` and `index_at` are inverses, the strip's
  points equal the temperature updates, the position text, the go-to-codelet lookup;
- `slow`: the capped 3000-codelet run of G-007 (`CAPPED_EVENTS` of `test_robustness.py`) is fed
  in a bounded time.
"""

import dataclasses
import subprocess
import sys
import time
from pathlib import Path

import pytest

from golden import plain_run, V2, V2_PROFILES
from tabletop import events as ev
from tabletop.config import PROFILES
from tabletop.gui.models import timeline as tl
from tabletop.gui.models.event_log import EventLog, event_text, json_lines, row_text
from tabletop.gui.models.trace import TraceModel
from tabletop.observe import TextTrace, json_line
from tabletop.problems import load_problem
from tabletop.run import run_problem

PYTHON_DIR = Path(__file__).resolve().parents[2]
QUICK = ("surround-05", 3)
PUSH = ("ch2-fig11", 3)             # G-035: the first by seed of the 82 runs with a push found under
                                    # `standard` in a bounded search of 15,900 runs: every problem,
                                    # seeds 1-300 (since item 09's runs: the FIGURE, GROUND, ECHO, TWIN
                                    # and TYPE_KEY detectors; `ch2-fig08` seed 2 pushes no more). Since
                                    # v5's fix (D-116 to D-120): one push and one pop of cp4 (four of
                                    # each, all of cp4, from item 09's runs to v4).


@pytest.fixture(scope="module")
def quick():
    """The quick-start run, `surround-05` seed 3, run once per module (G-009)."""
    return plain_run(*QUICK, V2)


@pytest.fixture(scope="module")
def quick_standard():
    """The quick-start run on the defaults themselves, which the CLI makes (D-114: the
    other tests here read the v2 defaults' run)."""
    return plain_run(*QUICK)


@pytest.fixture(scope="module")
def push_run():
    """The push run of G-035, `ch2-fig11` seed 3, run once per module (the push and the
    pop markers; the texts of the three kinds of competition)."""
    return plain_run(*PUSH, V2)


def run_cli(*args):
    """The CLI as a subprocess, as the engine's `tests/test_cli.py` runs it."""
    proc = subprocess.run([sys.executable, "-m", "tabletop", *args], cwd=PYTHON_DIR,
                          capture_output=True, text=True, timeout=120)
    assert proc.returncode == 0, proc.stderr
    return proc.stdout


def fed(events, step=False):
    """A history and a trace model fed the events: all at once, or (`step`) one at a time
    with a `sync` after each."""
    history = EventLog()
    model = TraceModel(history)
    if step:
        for event in events:
            history.on_event(event)
            model.sync()
    else:
        history.load(events)
        model.sync()
    return history, model


class Sink:
    def write(self, text):
        pass

    def flush(self):
        pass


def engine_lines(events, quiet=False):
    trace = TextTrace(Sink(), quiet=quiet)
    for event in events:
        trace.on_event(event)
    return trace.lines


# -- the trace equals the CLI's ----------------------------------------------------------------

def test_the_trace_equals_the_cli_output_normal_and_quiet(quick_standard):
    problem, seed = QUICK
    _history, model = fed(quick_standard.events)
    assert model.text() == run_cli(problem, "--seed", str(seed))
    model.set_quiet(True)
    assert model.text() == run_cli(problem, "--seed", str(seed), "--quiet")
    assert model.count == 2


def test_the_lines_equal_the_engines_trace_on_the_golden_runs(golden_runs):
    for run in golden_runs.values():
        _history, model = fed(run.events)
        assert [line.text for line in model.lines] == engine_lines(run.events)
        model.set_quiet(True)
        assert [line.text for line in model.lines] == engine_lines(run.events, quiet=True)
        assert model.lines[-1].text == run.report


def test_the_json_lines_equal_the_cli_json_output(quick_standard):
    problem, seed = QUICK
    history, _model = fed(quick_standard.events)
    text = json_lines(history)
    assert text == run_cli(problem, "--seed", str(seed), "--json", "-")
    assert text == "".join(json_line(e) + "\n" for e in quick_standard.events)
    assert text.count("\n") == len(history)


# -- the tags ----------------------------------------------------------------------------------

def test_the_lines_are_tagged_with_their_blocks(quick):
    events = quick.events
    history, model = fed(events)
    lines = model.lines
    assert lines[0].start == lines[0].end == 0
    assert lines[-1].start == lines[-1].end == len(events) - 1
    assert isinstance(events[lines[-1].start], ev.RunEnded)
    starts = [line.start for line in lines]
    assert starts == sorted(starts)
    codelet_lines = [line for line in lines if not line.text.startswith(" " * 6)
                     and line.text[:4].strip().isdigit()]
    assert len(codelet_lines) == 59          # since v5's fix (D-116 to D-120); 44 from item 08's runs to v2, 77 at item 07
    for line in codelet_lines:
        chosen = events[line.start]
        assert isinstance(chosen, ev.CodeletChosen)
        assert int(line.text[:4]) == chosen.clock
        assert line.start == history.codelet_index(chosen.clock)
        # Printed when the next codelet was chosen, or the run ended.
        assert isinstance(events[line.end], (ev.CodeletChosen, ev.RunEnded))
        assert line.end > line.start
    for k, line in enumerate(lines):
        if line.text.startswith(" " * 6):       # a note shares its codelet's block
            assert line.start == lines[k - 1].start
    # The start's notes (before the first codelet) begin at 0 with the first line.
    first_codelet = min(line.start for line in codelet_lines)
    assert all(line.start == 0 for line in lines if line.start < first_codelet)


def test_line_of_block_rows_and_is_future(quick):
    events = quick.events
    history, model = fed(events)
    assert model.line_of(-1) is None and model.block_rows(-1) == ()
    assert model.line_of(0) == 0
    n = len(events)
    for position in range(0, n, 37):
        row = model.line_of(position)
        assert model.lines[row].start <= position
        assert row == len(model.lines) - 1 or model.lines[row + 1].start > position
        rows = model.block_rows(position)
        assert row in rows and len({model.lines[r].start for r in rows}) == 1
        assert all(model.is_future(r, position) == (model.lines[r].start > position)
                   for r in range(len(model.lines)))
    # At the change of codelet 18 (cp1 enters the Worldview without a fight; the
    # CodeletChosen is index 274, since item 08's runs and still since v5's fix, D-116 to
    # D-120; codelet 45 at index 509 before item 08) the block is codelet 18's three lines:
    # the codelet's, its slipnet postings and the temperature's (the jump to 97.9 since v5's
    # fix, 96.8 from item 08 to v2).
    index = history.codelet_index(18)
    assert index == 274
    rows = model.block_rows(index + 3)
    assert len(rows) == 3
    assert [model.lines[r].text[:6].strip() for r in rows][0] == "18"
    assert any("T 80.0 -> 97.9" in model.lines[r].text for r in rows)
    assert model.is_future(rows[-1] + 1, index + 3)


def test_feeding_event_by_event_equals_feeding_all_at_once(quick):
    events = quick.events
    _h1, whole = fed(events)
    _h2, stepped = fed(events, step=True)
    assert whole.lines == stepped.lines
    stepped.set_quiet(True)
    whole.set_quiet(True)
    assert whole.lines == stepped.lines


def test_a_new_run_forgets_and_a_replaced_history_is_rescanned(quick, golden_runs):
    quick, other = quick.events, golden_runs[("blockage-06", 2)].events
    history, model = fed(quick, step=True)
    assert model.count > 2
    history.load(other)                   # a RunStarted begins a new log
    model.sync()
    assert [line.text for line in model.lines] == engine_lines(other)
    history.load(quick[:50])              # fewer events than scanned
    model.sync()
    assert [line.text for line in model.lines] == engine_lines(quick[:50])
    history.clear()
    model.sync()
    assert model.count == 0 and model.text() == ""


# -- the rows ----------------------------------------------------------------------------------

def test_row_text_and_event_text(quick):
    history, _model = fed(quick.events)
    record = history.records[0]
    row = row_text(0, record)
    assert row.startswith("     0  RunStarted                 0  problem=surround-05  seed=3")
    assert "touched=cup1" in row and 'objects=["fork1"' in row
    chosen = history.codelet_index(1)
    text = event_text(history.records[chosen])
    assert text.startswith("codelet=find-group  args=[]  generation=0")
    assert "event=" not in text and "clock=" not in text and "draws=" not in text
    assert row_text(chosen, history.records[chosen]).split()[:3] == [str(chosen), "CodeletChosen", "1"]


# -- the timeline ------------------------------------------------------------------------------

def timeline_of(events, step=False):
    history = EventLog()
    model = tl.TimelineModel(history)
    if step:
        for event in events:
            history.on_event(event)
            model.sync()
    else:
        history.load(events)
        model.sync()
    return history, model


def expected_markers(events):
    """The marker indexes PANES.md P11 asks for, read off the events."""
    first = {}
    marks = []
    for i, e in enumerate(events):
        if isinstance(e, ev.ObjectNoticed) and "noticed" not in first:
            first["noticed"] = i
            marks.append((i, "noticed"))
        elif isinstance(e, ev.CorrespondenceBuilt):
            marks.append((i, "correspondence"))
        elif isinstance(e, ev.CompetitionHeld):
            marks.append((i, "competition"))
        elif isinstance(e, ev.WorldviewChanged):
            marks.append((i, "change"))
        elif isinstance(e, ev.ContextPushed):
            marks.append((i, "push"))
        elif isinstance(e, ev.ContextPopped):
            marks.append((i, "pop"))
        elif isinstance(e, (ev.RoleDiscarded, ev.RoleRecast)):      # a role change (G-062)
            marks.append((i, "role"))
        elif isinstance(e, ev.AnswerCodeletPosted) and "answer-posted" not in first:
            first["answer-posted"] = i
            marks.append((i, "answer-posted"))
        elif isinstance(e, ev.AnswerGiven):
            marks.append((i, "answer"))
    return marks


def test_the_markers_equal_the_events(quick, golden_runs, push_run):
    for events in (quick.events, golden_runs[("blockage-06", 2)].events):
        _history, model = timeline_of(events, step=True)
        assert [(m.index, m.kind) for m in model.markers] == expected_markers(events)
        assert {m.kind for m in model.markers} >= {"noticed", "correspondence", "change",
                                                   "competition", "answer-posted", "answer"}
        assert all(m.kind in tl.MARKER_KINDS for m in model.markers)
    # The texts of the three kinds on the push run, `ch2-fig11` seed 3 (since item 09's runs;
    # `ch2-fig08` seed 2 had five before). Since v5's fix (D-116 to D-120): four
    # `CompetitionHeld`, one entry without a fight, one fight the rival won and two the old
    # Worldview held (G-041); from item 09 to v4 nine, three entries, two wins and four holds.
    events = push_run.events
    _history, model = timeline_of(events)
    fights = model.markers_of("competition")
    assert len(fights) == 4 and len(model.markers_of("noticed")) == 1
    assert [f.text for f in fights] == [
        "cp4 entered the Worldview without a fight at codelet 69",
        "competition at codelet 81: cp1 against cp4: the Rival Worldview won",
        "competition at codelet 94: cp4 against cp1: the Old Worldview held",
        "competition at codelet 103: cp3 against cp1: the Old Worldview held"]
    answer = model.markers_of("answer")[0]
    assert answer.text.startswith("answer: Eliza touches ")
    assert tl.marker_tooltip(answer).endswith(f"(event {answer.index + 1})")


def test_the_push_run_has_a_push_and_a_pop_marker(push_run):
    run = push_run
    _history, model = timeline_of(run.events)
    push, pop = model.markers_of("push"), model.markers_of("pop")
    # One push and one pop of cp4 (examine-cp(cp4) at codelet 73, popped by the next codelet)
    # since v5's fix (D-116 to D-120); four of each (at codelets 73, 87, 94 and 109, indexes
    # 851/859, 994/1001, 1047/1054, 1228/1235) from item 09's runs to v4; `ch2-fig08` seed 2
    # had two of cp1 before item 09.
    assert len(push) == 1 and len(pop) == 1
    assert push[0].index < pop[0].index
    assert [m.index for m in push] == [849]
    assert [m.index for m in pop] == [857]
    assert all(m.text == "context push: cp4" for m in push)
    assert all(m.text == "context pop: cp4" for m in pop)
    assert tl.marker_style(push[0]).shape == "triangle"
    assert tl.marker_style(pop[0]).shape == "triangle-down"
    assert [(m.index, m.kind) for m in model.markers] == expected_markers(run.events)


def test_the_strip_points_equal_the_temperature_updates(quick):
    events = quick.events
    _history, model = timeline_of(events)
    updates = [(i, e.temperature, e.held) for i, e in enumerate(events)
               if isinstance(e, ev.TemperatureUpdated)]
    assert [(r.index, r.temperature, r.held) for r in model.readings] == updates
    points = tl.strip_points(model.readings, model.count, 700, 20)
    assert len(points) == len(updates)
    for (x, y, held), (index, t, h) in zip(points, updates):
        assert x == tl.marker_x(index, model.count, 700) and held == h
        assert abs(y - 19 * (1 - t / 100)) < 1e-9
    assert tl.strip_points([], 10, 700, 20) == []


def test_marker_x_and_index_at_are_inverses():
    for count in (1, 2, 806):
        for index in range(count):
            x = tl.marker_x(index, count, 700)
            assert tl.INSET <= x <= 700 - tl.INSET
            assert tl.index_at(x, count, 700) == index
    assert tl.index_at(-100, 806, 700) == 0 and tl.index_at(10000, 806, 700) == 805
    assert tl.marker_x(0, 1, 700) == tl.INSET and tl.index_at(300, 1, 700) == 0


def test_the_position_text_and_the_codelet_lookup(quick):
    events = quick.events
    history, model = timeline_of(events)
    assert tl.TimelineModel(EventLog()).text() == "no events"
    assert model.text(-1) == "no events"
    assert model.text(0) == f"event 1 of {len(events)}: RunStarted   start"
    chosen = history.codelet_index(30)
    assert model.text(chosen) == f"event {chosen + 1} of {len(events)}: CodeletChosen   codelet 30"
    assert model.text(len(events) - 1).endswith("RunEnded   codelet 59")      # since v5's fix (D-116 to D-120); 44 from item 08's runs to v2
    model.set_position(chosen + 1)
    assert model.text().endswith("codelet 30")
    assert tl.codelet_index(history, 30) == chosen
    assert tl.codelet_index(history, 0) is None and tl.codelet_index(history, 99) is None


def test_a_new_run_forgets_the_markers(quick, golden_runs):
    quick, other = quick.events, golden_runs[("blockage-06", 2)].events
    history, model = timeline_of(quick, step=True)
    history.load(other)
    model.sync()
    assert [(m.index, m.kind) for m in model.markers] == expected_markers(other)
    history.clear()
    model.sync()
    assert model.markers == [] and model.readings == [] and model.count == 0

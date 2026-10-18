"""Loop0006 item 16: the results pane's model (`tabletop/gui/models/results.py`; G-052),
Qt-free, after the Numbo GUI's `test_run_controller.py` and the bench's `test_harness.py`.

- A batch of 5 on `surround-05` from batch seed 1 through the worker equals `run_batch`
  and `python3 -m tabletop surround-05 --batch 5 --seed 1` run for run (PANES.md P8;
  TASK.md's contract 4); its rows equal `harness.compare`'s, with `other` as RESULTS.md
  groups it, and its cells `markdown_table`'s.
- French's column equals `reported.py` for every problem he prints a set for.
- A cancelled batch keeps its runs and leaves no thread; a batch holds the engine lock
  against a run controller, and the reverse.
- The bar layout is a pure function; the status, the title, the footer; a problem without
  a set; bad inputs. A 50-run batch is `slow`.
"""

import dataclasses
import json
import subprocess
import sys
import threading
from pathlib import Path

import pytest

from tabletop.bench.harness import OTHER, compare, markdown_table
from tabletop.config import PROFILES
from tabletop.gui.models import results as rm
from tabletop.gui.models import run_controller as rc
from tabletop.gui.models.controls import catalogue
from tabletop.problems import load_problem
from tabletop.reported import result_sets
from tabletop.run import Result, run_batch

PYTHON_DIR = Path(__file__).resolve().parents[2]
STANDARD = PROFILES["standard"]


def batch_through_worker(model, problem, config, seed, count, cancel_after=None):
    """Run a batch through the worker, feeding the model as the view would (G-052);
    `cancel_after` cancels once that many runs have come in."""
    done = []

    def on_result(result):
        model.add(result)
        if cancel_after is not None and model.done >= cancel_after:
            worker.cancel()

    worker = rm.BatchWorker(on_result=on_result,
                            on_done=lambda cancelled, error: done.append((cancelled, error)))
    model.start(problem, config, seed, count)
    worker.start(problem, config, seed, count)
    assert worker.wait(120)
    assert not worker.is_alive() and len(done) == 1
    model.finish(*done[0])
    return worker


@pytest.fixture(scope="module")
def batch5():
    problem = load_problem("surround-05")
    model = rm.BatchModel()
    batch_through_worker(model, problem, STANDARD, 1, 5)
    return problem, model


def test_a_batch_of_5_equals_run_batch_and_the_cli(batch5):
    problem, model = batch5
    plain = run_batch(problem, STANDARD, 1, 5)
    assert model.results == plain and model.done == 5 and model.state == "finished"
    assert model.error is None and not model.running
    p = subprocess.run([sys.executable, "-m", "tabletop", "surround-05", "--batch", "5",
                        "--seed", "1", "--json", "-"], cwd=PYTHON_DIR, capture_output=True,
                       text=True, timeout=120)
    assert p.returncode == 0, p.stderr
    records = [json.loads(line) for line in p.stdout.splitlines()]
    assert [r["answer"] for r in records] == [r.answer for r in model.results]
    assert [r["seed"] for r in records] == [r.seed for r in model.results]
    assert records == [json.loads(json.dumps(dataclasses.asdict(r))) for r in plain]


def test_the_rows_equal_the_benchs_comparison_with_other_grouped(batch5):
    problem, model = batch5
    survey = model.survey()
    assert survey.problem == "surround-05" and survey.runs == 5 and survey.seed == 1
    assert survey.batch_seed == 1 and survey.profile == "standard"
    rs = model.french()
    assert rs is not None and rs.problem == "surround-05" and rs.page == 118 and rs.source == "F95"
    comparison = compare(survey, rs)
    rows = model.rows()
    assert [r.label for r in rows] == [r.label for r in comparison.rows]
    for ours, theirs in zip(rows, comparison.rows):
        for field in ("label", "names", "french_frequency", "french_structure", "french_codelets",
                      "count", "frequency", "structure", "codelets", "answers", "z"):
            assert getattr(ours, field) == getattr(theirs, field), field
    # French prints E1 and E2 for Surround 5 [F95 p. 118]; the runs that gave neither
    # fall under `other`, as RESULTS.md's tables have it.
    assert [r.label for r in rows][:2] == ["E1", "E2"]
    assert sum(r.count for r in rows) == 5
    if any(r.label == OTHER for r in rows):
        other = rows[-1]
        assert other.label == OTHER and other.french_frequency is None and other.answers
    chi = model.chi_square()
    assert chi == (comparison.chi_square, comparison.df, comparison.p)


def test_the_cells_are_formatted_as_the_benchs_table(batch5):
    problem, model = batch5
    lines = markdown_table(compare(model.survey(), model.french()))
    table_rows = [line for line in lines if line.startswith("| ") and "---" not in line][1:]
    expected = tuple(tuple(c.strip() for c in line.strip("|").split("|")) for line in table_rows)
    assert model.cells() == expected
    assert len(rm.HEADINGS) == len(expected[0]) == 8
    assert "chi-square" in model.footer() and "French's n = 50" in model.footer()
    assert model.title() == "surround-05 under standard, batch seed 1"
    assert model.french_title() == "Surround 5 [F95 p. 118 (img)]"
    assert model.status().startswith("5 runs, 0 capped, mean codelets ")
    text = rm.row_text(model.rows()[0])
    assert text.startswith("E1: ") and "frequency: French 86 %" in text and "z " in text


def fake_result(problem, answer):
    """A `Result` of a run that touched `answer`, for a table of French's numbers."""
    label = next((k for k, v in problem.answers.items() if v == answer), "")
    return Result(problem.name, 1, "standard", "compiled", (), answer, label, False, "correspondence",
                  "cp1", False, 50.0, 40, 30.0, (), 0, 1, 0, 1, 100)


def test_frenchs_column_equals_reported_py_for_every_problem_with_a_set():
    seen = 0
    for entry in catalogue():
        sets = result_sets(entry.name)
        if not sets:
            continue
        problem = load_problem(entry.name)
        model = rm.BatchModel()
        model.start(problem, STANDARD, 1, 1)
        model.add(fake_result(problem, next(iter(problem.answers.values()))))
        model.finish()
        assert model.sets() == sets
        for index, rs in enumerate(sets):
            model.choose_set(index)
            assert model.french() is rs
            rows = model.rows()
            printed = [r for r in rows if r.label != OTHER]
            assert [r.label for r in printed] == [a.label for a in rs.answers]
            for row, answer in zip(printed, rs.answers):
                assert (row.french_frequency, row.french_structure, row.french_codelets) == \
                    (answer.frequency, answer.structure, answer.codelets)
                assert row.names == answer.names
            assert sum(r.count for r in rows) == 1
            assert model.french_title() == rm.set_title(rs) and f"p. {rs.page}" in model.french_title()
            seen += 1
    assert seen >= 34


def test_a_problem_without_a_set_shows_our_rows_alone():
    name = next(e.name for e in catalogue() if not result_sets(e.name))
    problem = load_problem(name)
    model = rm.BatchModel()
    model.start(problem, STANDARD, 1, 1)
    model.add(fake_result(problem, problem.objects[-1].id))   # a chapter 2 table: no answers
    model.finish()
    assert model.french() is None and model.sets() == [] and model.chi_square() is None
    (row,) = model.rows()
    assert row.french_frequency is None and row.z is None and row.count == 1
    assert row.frequency == 100.0 and row.label == problem.objects[-1].id
    assert model.cells()[0][3] == "" and model.cells()[0][1] == "-"
    assert "French prints no result set" in model.footer()
    assert model.french_title() == "French prints no result set for this problem"


def test_a_cancelled_batch_keeps_its_runs_and_leaves_no_thread():
    problem = load_problem("surround-05")
    model = rm.BatchModel()
    worker = batch_through_worker(model, problem, STANDARD, 2, 500, cancel_after=1)
    assert model.state == "cancelled" and model.done == 1 and model.count == 500
    assert model.status() == "cancelled after 1 of 500 runs, 0 capped, mean codelets " \
        f"{model.results[0].codelets:.1f}"
    assert not worker.is_alive()
    assert not any(t.name == "tabletop-batch" for t in threading.enumerate())
    assert model.results == run_batch(problem, STANDARD, 2, 1)
    # The engine lock was released: a controller can run.
    assert rc.engine_lock().acquire(blocking=False)
    rc.engine_lock().release()
    model.clear()
    assert model.state == "idle" and model.rows() == () and model.survey() is None
    assert model.status().startswith("no batch yet")


def test_a_batch_and_a_run_controller_exclude_each_other():
    problem = load_problem("surround-05")
    started = threading.Event()
    release = threading.Event()

    def slow_result(result):
        started.set()
        release.wait(30)

    worker = rm.BatchWorker(on_result=slow_result)
    worker.start(problem, STANDARD, 3, 3)
    assert started.wait(30)
    controller = rc.RunController()
    with pytest.raises(rc.EngineBusy, match="busy"):
        controller.start_run(problem, STANDARD, 1)
    with pytest.raises(rc.EngineBusy, match="busy"):
        rm.BatchWorker().start(problem, STANDARD, 1, 1)
    worker.cancel()
    release.set()
    assert worker.wait(30) and not worker.is_alive()
    # The reverse: a paused run holds the lock against a batch.
    controller.start_run(problem, STANDARD, 1, paused=True)
    try:
        with pytest.raises(rc.EngineBusy, match="busy"):
            rm.BatchWorker().start(problem, STANDARD, 1, 1)
    finally:
        assert controller.stop()
    assert not controller.is_alive()
    worker2 = rm.BatchWorker()
    worker2.start(problem, STANDARD, 1, 1)
    assert worker2.wait(30)


def test_bad_inputs_are_refused_before_any_thread_starts():
    problem = load_problem("surround-05")
    model = rm.BatchModel()
    with pytest.raises(ValueError):
        model.start(problem, STANDARD, 1, 0)
    with pytest.raises(ValueError):
        model.start(problem, STANDARD, 1, rm.MAX_COUNT + 1)
    with pytest.raises(TypeError):
        rm.BatchWorker().start("surround-05", STANDARD, 1, 1)
    with pytest.raises(TypeError):
        rm.BatchWorker().start(problem, "standard", 1, 1)
    assert not any(t.name == "tabletop-batch" for t in threading.enumerate())
    assert rc.engine_lock().acquire(blocking=False)
    rc.engine_lock().release()


def test_the_bar_layout_is_a_pure_function(batch5):
    problem, model = batch5
    rows = model.rows()
    layout = rm.bars_layout(rows, 400, 200, "Surround 5")
    assert layout == rm.bars_layout(rows, 400, 200, "Surround 5")
    assert layout.title == "Surround 5" and len(layout.bars) == 2 * len(rows)
    bx, by, bw, bh = layout.box
    assert (bx, by) == (44, 18) and bw == 400 - 44 - 10 and bh == 200 - 18 - 34
    for k, row in enumerate(rows):
        french, ours = layout.bars[2 * k], layout.bars[2 * k + 1]
        assert (french.who, ours.who) == ("French", "ours") and french.label == ours.label == row.label
        assert french.x + french.width == pytest.approx(ours.x)
        if row.french_frequency is None:
            assert french.height == 0 and french.text == "-" and french.value is None
        else:
            assert french.height == pytest.approx(bh * row.french_frequency / 100)
            assert french.text == f"{row.french_frequency:.0f}"
        assert ours.height == pytest.approx(bh * row.frequency / 100)
        assert ours.y + ours.height == pytest.approx(by + bh) and ours.text == f"{row.frequency:.1f}"
        label, x, y = layout.labels[k]
        assert label == row.label and french.x < x < ours.x + ours.width and y > by + bh
    assert [pct for pct, _y in layout.ticks] == [0, 25, 50, 75, 100]
    assert layout.ticks[0][1] == pytest.approx(by + bh) and layout.ticks[-1][1] == pytest.approx(by)
    empty = rm.bars_layout((), 100, 50)
    assert empty.bars == () and empty.labels == () and empty.box[2] >= 1
    assert rm.number(None) == "-" and rm.number(12.34, 1) == "12.3" and rm.number(86) == "86"

"""loop0003 item 7: the Qt-free half of the GUI shell (numbo/models/run_stats.py).

The run's inputs (the chapter's puzzles, the custom problem, seed and cap as
the controls' text) and the stats panel's model (iteration, x, temperature,
outcome and the solution check), built from the run's events.  The stats
must say what the CLI's two summary lines say for the same run.
"""

import io
import subprocess
import sys

import pytest

import full_runs
from numbo import harness, observe
from numbo.models import run_stats
from numbo.models.run_stats import InputError, RunStats


# -- inputs ---------------------------------------------------------------------

def test_the_chapter_puzzles_are_the_eleven_of_the_oracle_tests():
    assert run_stats.CHAPTER_PUZZLES == tuple(tuple(p) for p in full_runs.PUZZLES)
    assert len(run_stats.CHAPTER_PUZZLES) == 11


def test_puzzle_labels_read_as_target_and_bricks():
    assert run_stats.puzzle_label(1) == "1: 114 from 11 20 7 1 6"
    assert run_stats.puzzle_label(11) == "11: 41 from 5 16 22 25 1"


@pytest.mark.parametrize("text, problem", [
    ("114 11 20 7 1 6", (114, 11, 20, 7, 1, 6)),
    ("  31, 3,5 24 3  14 ", (31, 3, 5, 24, 3, 14)),
    ("6 1 1 1 1 1", (6, 1, 1, 1, 1, 1)),
    ("-5 1 2 3 4 5", (-5, 1, 2, 3, 4, 5)),   # as the CLI: any integers
])
def test_parse_problem(text, problem):
    assert run_stats.parse_problem(text) == problem


@pytest.mark.parametrize("text, message", [
    ("", "a target and 5 bricks"),
    ("1 2 3", "got 3 numbers"),
    ("1 2 3 4 5 6 7", "got 7 numbers"),
    ("114 11 20 seven 1 6", "'seven' is not an integer"),
    ("114 11 20 7.5 1 6", "'7.5' is not an integer"),
])
def test_parse_problem_rejects(text, message):
    with pytest.raises(InputError, match=message):
        run_stats.parse_problem(text)


def test_parse_seed_and_max_iterations():
    assert run_stats.parse_seed(" 8 ") == 8
    assert run_stats.parse_max_iterations("20000") == 20000
    assert run_stats.parse_max_iterations(" None ") is None
    for bad, message in [("", "seed: '' is not an integer"),
                         ("abc", "seed: 'abc' is not an integer"),
                         ("1.5", "seed: '1.5' is not an integer")]:
        with pytest.raises(InputError, match=message):
            run_stats.parse_seed(bad)
    for bad, message in [("0", "at least 1"), ("-3", "at least 1"),
                         ("lots", "'lots' is not an integer or 'none'")]:
        with pytest.raises(InputError, match=message):
            run_stats.parse_max_iterations(bad)


def test_input_error_is_a_value_error():
    assert issubclass(InputError, ValueError)


# -- stats ----------------------------------------------------------------------

def _run(problem, seed, cap=20000):
    stats = RunStats()
    seen = []

    class Check:
        def on_event(self, event):
            stats.on_event(event)
            seen.append((event.kind, stats.iteration, stats.events))

    out = io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap,
                                trace=io.StringIO(), out=out, observers=[Check()])
    return stats, result, out.getvalue(), seen


def _cli(problem, seed, cap=20000):
    """The CLI's two summary lines for the same run."""
    args = [sys.executable, "-m", "numbo", "--quiet", "--seed", str(seed),
            "--max-iterations", str(cap), "--", *map(str, problem)]
    proc = subprocess.run(args, capture_output=True, text=True, cwd=full_runs.PYTHON_DIR)
    return proc.stdout.splitlines()[-2:]


def test_a_fresh_stats_model_is_blank():
    stats = RunStats()
    assert stats.problem is None and stats.iteration is None and stats.outcome is None
    assert stats.events == 0
    assert stats.outcome_text() == "" and stats.check_text() == ""


def test_stats_follow_a_solved_run_and_agree_with_the_cli():
    stats, result, output, seen = _run(full_runs.PUZZLES[0], 1)
    assert stats.problem == (114, 11, 20, 7, 1, 6) and stats.seed == 1
    assert stats.max_iterations == 20000
    assert stats.outcome == "solved" and stats.iterations == 45 == result["iterations"]
    # The last iteration event is iteration 44, with its x and temperature.
    assert stats.iteration == 44
    assert isinstance(stats.x, int) and stats.temperature is not None
    assert stats.events == len(seen)
    assert stats.check is None          # the check needs the printed text
    stats.finish(output)
    assert stats.check == (True, None, "114 = (6 x 20) - (7 - 1)")
    lines = [f"outcome: {stats.outcome_text()} (seed 1)", f"check: {stats.check_text()}"]
    assert lines == _cli(full_runs.PUZZLES[0], 1)
    assert stats.outcome_text() == "solved, 45 iterations"
    assert stats.check_text() == "valid: 114 = (6 x 20) - (7 - 1)"


def test_iteration_follows_every_iteration_event():
    stats, _, _, seen = _run(full_runs.PUZZLES[0], 1)
    iterations = [n for kind, n, _ in seen if kind == "iteration"]
    assert iterations == list(range(45))
    # Before the first iteration (the set-up phase) there is none.
    assert seen[0] == ("start", None, 1)
    assert [count for _, _, count in seen] == list(range(1, len(seen) + 1))


@pytest.mark.parametrize("puzzle, seed, cap", [
    (3, 8, 20000),     # solved, but the decomposition is invalid (the kill-block gap)
    (1, 40, 20000),    # error: the reactivate-cyto race
    (3, 1, 300),       # capped
])
def test_other_outcomes_agree_with_the_cli(puzzle, seed, cap):
    problem = full_runs.PUZZLES[puzzle - 1]
    stats, result, output, _ = _run(problem, seed, cap)
    stats.finish(output)
    assert stats.outcome == result["outcome"]
    # The CLI says "(seed N)" before an error's message.
    outcome, check = _cli(problem, seed, cap)
    assert f"outcome: {stats.outcome_text()}" == outcome.replace(f" (seed {seed})", "", 1)
    assert f"check: {stats.check_text()}" == check
    assert stats.check[0] is False


def test_a_new_start_resets_the_stats():
    stats = RunStats()
    stats.on_event(observe.RunStarted(problem=(6, 1, 1, 1, 1, 1), seed=3,
                                      max_iterations=None, rng="splitmix64", pnet=()))
    stats.on_event(observe.IterationBegan(n=0, x=12, temperature=0, rack=((600, 4), (300, 1))))
    stats.on_event(observe.CodeletChosen(n=0, codelet="ACTIVATE", args=(), urgency=600,
                                         draws=()))
    assert stats.rack_total == 5 and stats.codelet == "ACTIVATE"
    stats.on_event(observe.GaveUp(iterations=1))
    stats.finish("nothing")
    assert stats.outcome_text() == "gave-up, 1 iterations"
    assert stats.check_text().startswith("invalid: ")
    stats.on_event(observe.RunStarted(problem=(7, 1, 1, 1, 1, 1), seed=4,
                                      max_iterations=10, rng="splitmix64", pnet=()))
    assert (stats.problem, stats.seed, stats.max_iterations) == ((7, 1, 1, 1, 1, 1), 4, 10)
    assert stats.iteration is None and stats.outcome is None and stats.check is None
    assert stats.codelet is None and stats.rack_total is None and stats.events == 1


def test_finish_without_an_end_or_a_problem_checks_nothing():
    stats = RunStats()
    stats.finish("Done :")
    assert stats.check is None
    stats.on_event(observe.RunStarted(problem=(6, 1, 1, 1, 1, 1), seed=3,
                                      max_iterations=None, rng="splitmix64", pnet=()))
    stats.finish("Done :")       # a stopped run: no outcome, no check
    assert stats.check is None and stats.check_text() == ""
    stats.finish(None)
    assert stats.check is None


def test_an_error_outcome_names_the_error():
    stats = RunStats()
    stats.on_event(observe.RunError(iterations=27, message="float division by zero"))
    assert stats.outcome_text() == "error, 27 iterations: float division by zero"


# -- the verdict: what the canvas's banner says (loop0003 item 11) ---------------

def _verdict(puzzle, seed, cap=20000):
    stats, _, output, _ = _run(full_runs.PUZZLES[puzzle - 1], seed, cap)
    stats.finish(output)
    return stats.verdict()


def test_there_is_no_verdict_before_the_end():
    stats = RunStats()
    assert stats.verdict() is None
    stats.on_event(observe.RunStarted(problem=(6, 1, 1, 1, 1, 1), seed=3,
                                      max_iterations=None, rng="splitmix64", pnet=()))
    assert stats.verdict() is None


def test_a_valid_solution_is_good_news():
    v = _verdict(1, 1)
    assert v.level == "good"
    assert v.headline == "Solved in 45 iterations: 114 = (6 x 20) - (7 - 1)"
    assert v.detail == ""


def test_the_kill_block_gap_is_named():
    v = _verdict(3, 8)
    assert v.level == "warn"
    assert v.headline.startswith("Solved in ") and "but the solution is invalid" in v.headline
    assert "CYTO-BLOCK11-V5 (11) is used but never derived" in v.detail
    assert "kill-block gap" in v.detail and "PORTING_NOTES" in v.detail


@pytest.mark.parametrize("seed, message", [
    (40, "SEND: NIL does not handle the message :SET-ACTIVATION"),
    (323, "The variable CYTO-BRICK3 is unbound."),
])
def test_the_reactivate_cyto_race_is_explained(seed, message):
    v = _verdict(1, seed)
    assert v.level == "error"
    assert v.headline == "Error after 29 iterations"
    assert message in v.detail
    assert "reactivate-cyto race" in v.detail and "PORTING_NOTES" in v.detail


def test_another_error_is_shown_without_the_race_note():
    stats = RunStats()
    stats.on_event(observe.RunError(iterations=27, message="float division by zero"))
    v = stats.verdict()
    assert v.level == "error" and v.headline == "Error after 27 iterations"
    assert v.detail == "float division by zero"


def test_capped_and_gave_up_runs_are_warnings():
    v = _verdict(3, 1, 300)
    assert v.level == "warn" and v.headline == "Stopped at the cap, 300 iterations: not solved"
    stats = RunStats()
    stats.on_event(observe.GaveUp(iterations=12))
    v = stats.verdict()
    assert v.level == "warn" and v.headline == "Gave up after 12 iterations"

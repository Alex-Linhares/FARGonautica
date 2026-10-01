"""Loop0002 item 12: full-run equivalence, Python vs. the SBCL oracle.

The chapter's 11 puzzles x seeds 1-20 (cap 20000, tests/chapter-runs.lisp's),
plus runs chosen for the 1987 flaws they reach:
  - puzzle 1, oracle seed 40: the reactivate-cyto race (PORTING_NOTES.md,
    item 10.2), "SEND: NIL does not handle the message :SET-ACTIVATION";
  - puzzle 1, oracle seed 323: the same race one step earlier, when brick 3
    has not been read yet, "The variable CYTO-BRICK3 is unbound.";
  - the kill-block gap (item 10.1) is in the 220 already: puzzle 3 seed 8
    and puzzle 6 seed 9 print an invalid "Done :".
(Oracle seeds 1-400 of the 11 puzzles give 4 error runs: these two, and
puzzle 3 seeds 162 and 272, both the SEND case.)

Each run is made by both sides (full_runs.py): the oracle in a fresh SBCL
process (tests/oracle/lib/full-run.lisp), the Python in a fresh World, both
with the RNG draws in the trace.  The event streams must be equal event by
event (parsed JSON through json.dumps, so 3 and 3.0 differ), and so must the
outcome, the iteration count, *problem-solved*, the error message, the
printed text, and the solution checker's verdict on it (item 13).  The
oracle's records also regenerate python/RESULTS.md's tables, which must be
the committed ones (test_results_md_matches_the_oracle).  The traces are made in a temporary directory (too big to
commit); the runs are spread over a process pool (about 10 s for all).
"""

import json
import tempfile

import pytest

import full_runs
from conftest import requires_sbcl
from full_runs import CAP, PUZZLES, SEEDS, first_divergence
from scripts import chapter_runs

REACTIVATE_SEND = (PUZZLES[0], 40)
REACTIVATE_UNBOUND = (PUZZLES[0], 323)
GAP_RUNS = [(PUZZLES[2], 8), (PUZZLES[5], 9)]

SPECS = ([(p, s) for p in PUZZLES for s in SEEDS]
         + [REACTIVATE_SEND, REACTIVATE_UNBOUND])


def spec_id(spec):
    return f"{spec[0][0]}-seed{spec[1]}"


@pytest.fixture(scope="module")
def results():
    with tempfile.TemporaryDirectory(prefix="numbo-full-runs-") as workdir:
        runs = full_runs.sweep([(p, s, CAP) for p, s in SPECS], workdir)
    return {(tuple(r["problem"]), r["seed"]): r for r in runs}


@requires_sbcl
@pytest.mark.parametrize("spec", SPECS, ids=spec_id)
def test_run_matches_oracle(results, spec):
    r = results[spec]
    assert r["report"] is None, f"{spec_id(spec)} ({r['outcome']}):\n{r['report']}"


@requires_sbcl
def test_chapter_sweep_outcomes(results):
    """The 220 chapter runs are all there, and they end each way the oracle
    harness knows except "error" (no seed 1-20 run errors in oracle mode)."""
    chapter = [results[(p, s)] for p in PUZZLES for s in SEEDS]
    assert len(chapter) == 220
    outcomes = {r["outcome"] for r in chapter}
    assert {"solved", "gave-up"} <= outcomes <= {"solved", "gave-up", "capped"}
    assert sum(r["iterations"] for r in chapter) > 100000


@requires_sbcl
def test_results_md_matches_the_oracle(results):
    """python/RESULTS.md's tables are what the oracle's fresh-process runs
    give (python/scripts/chapter_runs.py makes them from the Python's runs;
    every run above matches the oracle's, verdict included)."""
    assert [(t, *b) for _, t, b, _ in chapter_runs.PUZZLES] == PUZZLES
    assert (chapter_runs.SEEDS, chapter_runs.CAP) == (len(SEEDS), CAP)
    records = [results[(p, s)] for p in PUZZLES for s in SEEDS]
    committed = chapter_runs.generated_section(chapter_runs.RESULTS_MD.read_text())
    assert chapter_runs.report(records) == committed


@requires_sbcl
def test_checker_verdicts(results):
    """184 solved runs: 182 valid, and the two kill-block-gap runs invalid."""
    chapter = [results[(p, s)] for p in PUZZLES for s in SEEDS]
    solved = [r for r in chapter if r["outcome"] == "solved"]
    invalid = [(r["problem"][0], r["seed"]) for r in solved if not r["check"][0]]
    assert (len(solved), sorted(invalid)) == (184, [(31, 8), (146, 9)])
    assert all(r["check"] == [False, 'no "Done :" in the output', None]
               for r in chapter if r["outcome"] != "solved")


@requires_sbcl
def test_reactivate_cyto_race_runs(results):
    send, unbound = results[REACTIVATE_SEND], results[REACTIVATE_UNBOUND]
    # x = 40, the first reactivate-cyto, is main-loop iteration 28.
    assert (send["outcome"], send["iterations"], send["error"]) == (
        "error", 29, "SEND: NIL does not handle the message :SET-ACTIVATION")
    assert (unbound["outcome"], unbound["iterations"], unbound["error"]) == (
        "error", 29, "The variable CYTO-BRICK3 is unbound.")


@requires_sbcl
@pytest.mark.parametrize("spec", GAP_RUNS, ids=spec_id)
def test_kill_block_gap_runs(spec, tmp_path):
    """The gap runs solve, but a block is killed before "Done :" and still
    appears in the decomposition (the checker rejects them; see
    test_codelets_c.test_gap_run_is_an_invalid_done).  Both sides print it."""
    problem, seed = spec
    expected = full_runs.oracle_run(problem, seed, CAP, str(tmp_path / "oracle"))
    got = full_runs.python_run(problem, seed, CAP, str(tmp_path / "python.jsonl"))
    assert expected["outcome"] == got["outcome"] == "solved"
    assert expected["output"] == got["output"]
    head, _, decomposition = expected["output"].partition("Done : ")
    killed = {line.split()[1] for line in head.splitlines()
              if line.startswith("Node CYTO-BLOCK") and line.endswith(" killed")}
    assert any(f"to {name} (" in decomposition for name in killed)


# ---------------------------------------------------------------------------
# The comparison itself

def _lines(events):
    return [json.dumps(e) for e in events]


def test_divergence_report():
    """first_divergence names the first differing event, with the events
    before it; 3 and 3.0 differ; extra events at either end are reported;
    the oracle's float spelling (1.0e7) is the same number as Python's."""
    a = [{"ev": "start"}, {"ev": "iteration", "n": 0, "x": 3}, {"ev": "done"}]
    assert first_divergence(_lines(a), _lines(a)) is None
    assert first_divergence(['{"t": 1.0e7}'], ['{"t": 10000000.0}']) is None
    b = [{"ev": "start"}, {"ev": "iteration", "n": 0, "x": 3.0}, {"ev": "done"}]
    report = first_divergence(_lines(a), _lines(b))
    assert report.startswith("first divergent event #1:")
    assert '[0] {"ev": "start"}' in report
    assert '"x": 3}' in report and '"x": 3.0}' in report
    report = first_divergence(_lines(a), _lines(a[:2]))
    assert "first extra event #2 (oracle)" in report
    report = first_divergence(_lines(a[:2]), _lines(a))
    assert "first extra event #2 (python)" in report

"""End-to-end parity of the Python port with the Perl baseline (item 050).

Mirrors: config/sequence_list_for_testing run through lib/Test/Seqsee.pm (RunSeqsee) by
oracle/e2e_run.perl, whose results oracle/baseline.py stored in
tests/golden/e2e_baseline.json (seeds 1..10, MAX_STEPS 10000, max_false 3,
min_extension 3). The Python side is seqsee/testing/e2e.py.

Python can't follow Perl's trajectory draw for draw (hash order and addresses differ), so
whole runs are compared statistically, per sequence:
- the success count is within ``SUCCESS_TOLERANCE`` of Perl's;
- a sequence Perl never solves is never solved, and one Perl always solves is solved in
  at least 8 of 10 runs;
- the median steps of successful runs is within a factor of 2 of Perl's, and pooled over
  all sequences within ±25%;
- no run crashes unless Perl's runs crash too.
Over seeds 1..40 (oracle/e2e_compare.py) the success counts differed by at most 3 of 40;
see PROGRESS.md, iteration 51.
"""
import json
import statistics

import pytest

import golden
from seqsee.testing import e2e

BASELINE = golden.load("e2e_baseline")
SEEDS = BASELINE[0]["seeds"]
MAX_STEPS = BASELINE[0]["max_steps"]
SUCCESS_TOLERANCE = 4          # of 10 runs
MEDIAN_FACTOR = 2.0
POOLED_MEDIAN_TOLERANCE = 0.25

# Sequences where Python is known to differ, as {seq: reason} (strict xfail). Empty since
# item 050b gave the main loop recursion headroom for Alternating 1 1 1 2 2 3 3 3 ….
KNOWN_GAPS = {}


def seq_id(entry):
    return (entry["label"] + ":" if entry["label"] else "") + entry["seq"].replace(" ", "_")


def known_gap(entry):
    reason = KNOWN_GAPS.get(entry["seq"])
    return pytest.param(entry, id=seq_id(entry),
                        marks=[pytest.mark.xfail(reason=reason, strict=True)] if reason else [])


@pytest.fixture(scope="module")
def python_runs():
    """The baseline grid run by the Python port, keyed by sequence."""
    data = e2e.run_all(SEEDS, MAX_STEPS)
    return {d["seq"]: d for d in data}


# ------------------------------------------------------------------ fast: the runner itself
def test_parse_sequences_matches_the_baseline_order():
    assert [(lab, seq, cont) for lab, seq, cont in e2e.parse_sequences()] == \
        [(d["label"], d["seq"], d["continuation"]) for d in BASELINE]


def test_run_one_reports_what_e2e_run_perl_prints():
    r = e2e.run_one(1, "1 2 3 4 5", "6 7 8", 5)
    assert set(r) == {"seed", "seq", "status", "steps", "error"}
    assert r["seed"] == 1 and r["seq"] == "1 2 3 4 5"
    assert isinstance(r["steps"], int)
    json.dumps(r)


@pytest.mark.parametrize("c", [c for c in golden.load("harness") if c["case"] == "real"],
                         ids=lambda c: f"seed{c['seed']}")
def test_run_one_matches_perl_exactly_on_short_runs(c):
    """The harness golden's genuine short Perl runs (oracle/harness.pl --real) match the
    run_one sequence (reset, load, srand, RunSeqsee) step for step."""
    r = e2e.run_one(c["seed"], c["seq"], c["continuation"], c["max_steps"],
                    c["max_false"], c["min_extension"])
    assert (r["status"], r["steps"], r["error"]) == (c["status"], c["steps"], c["error"])


def test_run_one_starts_from_fresh_state():
    """Each run resets all global state first, so earlier runs don't leak into it."""
    first = e2e.run_one(3, "1 1 2 1 2 3", "1 2 3 4", 300)
    e2e.run_one(7, "1 1 2 2 3 3", "4 4 5 5", 300)
    assert e2e.run_one(3, "1 1 2 1 2 3", "1 2 3 4", 300) == first


def test_run_one_swallows_seqsee_prints(capsys):
    e2e.run_one(1, "1 2 3", "4 5", 20)
    assert capsys.readouterr().out == ""


def test_summarize_has_the_baseline_shape():
    runs = [{"seed": 1, "seq": "1 2", "status": "Successful", "steps": 10, "error": None},
            {"seed": 2, "seq": "1 2", "status": "Crashed", "steps": 4, "error": "x"}]
    d = e2e.summarize("L", "1 2", "3", 100, [1, 2], runs)
    assert set(d) == set(BASELINE[0])
    assert d["successes"] == 1 and d["success_rate"] == 0.5
    assert d["statuses"] == ["Successful", "Crashed"] and d["errors"] == [None, "x"]
    assert e2e.success_steps(d) == [10]
    assert e2e.median_or_none([]) is None and e2e.median_or_none([1, 3]) == 2


def test_run_all_matches_run_one():
    seqs = [("", "1 2 3", "4 5 6")]
    (d,) = e2e.run_all([1, 2], 30, seqs, jobs=2)
    assert d["steps"] == [e2e.run_one(s, "1 2 3", "4 5 6", 30)["steps"] for s in (1, 2)]


# ------------------------------------------------------------------ slow: the parity grid
@pytest.mark.slow
@pytest.mark.parametrize("perl", BASELINE, ids=seq_id)
def test_success_count_within_tolerance(perl, python_runs):
    py = python_runs[perl["seq"]]
    assert abs(py["successes"] - perl["successes"]) <= SUCCESS_TOLERANCE, (perl, py)
    if perl["successes"] == 0:
        assert py["successes"] == 0
    if perl["successes"] == len(SEEDS):
        assert py["successes"] >= 8


@pytest.mark.slow
@pytest.mark.parametrize("perl", [d for d in BASELINE if d["successes"]], ids=seq_id)
def test_median_steps_of_successes_within_factor(perl, python_runs):
    py = python_runs[perl["seq"]]
    pm = statistics.median(e2e.success_steps(perl))
    qm = statistics.median(e2e.success_steps(py))
    assert pm / MEDIAN_FACTOR <= qm <= pm * MEDIAN_FACTOR, (pm, qm)


@pytest.mark.slow
def test_pooled_median_steps_of_successes(python_runs):
    perl = [st for d in BASELINE for st in e2e.success_steps(d)]
    py = [st for d in python_runs.values() for st in e2e.success_steps(d)]
    pm, qm = statistics.median(perl), statistics.median(py)
    assert abs(qm - pm) <= POOLED_MEDIAN_TOLERANCE * pm, (pm, qm)


@pytest.mark.slow
@pytest.mark.parametrize("perl", [known_gap(d) for d in BASELINE])
def test_no_crash_unless_perl_crashes(perl, python_runs):
    """Python crashes at most as often as Perl. (Other statuses vary with sampling: over
    40 seeds Perl ends some solvable sequences NotEvenExtended too.) Every step count is
    within MAX_STEPS, and only crashed runs carry an error."""
    py = python_runs[perl["seq"]]
    assert py["statuses"].count("Crashed") <= perl["statuses"].count("Crashed"), py["errors"]
    assert all(0 < st <= MAX_STEPS for st in py["steps"])
    assert all((err is not None) == (s == "Crashed") for s, err in zip(py["statuses"], py["errors"]))

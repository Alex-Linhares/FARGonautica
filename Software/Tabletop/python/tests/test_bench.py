"""The bench harness, its statistics and the `families` and `fcca` benches (item 22; D-072),
at a fast sample size: a few runs on two problems. The statistics are checked on known
inputs, the surveys against `run_batch` with the same seeds, the formats line by line, and
the command line through `subprocess`. No bench is run at its full size here (ANSWERS.md 1:
before the freeze the benches are run only in these small format checks)."""

import json
import math
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel

from tabletop import reported as RP
from tabletop.bench import __main__ as CLI
from tabletop.bench import families, fcca
from tabletop.bench import harness as H
from tabletop.bench import stats as ST
from tabletop.config import PROFILES
from tabletop.problems import load_problem
from tabletop.rng import run_seeds
from tabletop.run import run_problem

PYTHON_DIR = Path(__file__).resolve().parent.parent
CFG = PROFILES["standard"]
RUNS = 4


@pytest.fixture(scope="module")
def surveys():
    """Two problems, four runs each, shared by the tests of this file (D-072)."""
    return {name: H.survey(load_problem(name), CFG, 1, RUNS)
            for name in ("surround-05", "blockage-06")}


# --- the statistics on known inputs -----------------------------------------------------

def test_z_score_of_the_expected_count_is_zero_and_grows_with_the_gap():
    assert ST.z_score(30, 100, 0.3) == 0.0
    assert ST.z_score(40, 100, 0.3) == pytest.approx(10 / math.sqrt(21))
    assert ST.z_score(20, 100, 0.3) == pytest.approx(-10 / math.sqrt(21))
    with pytest.raises(ValueError):
        ST.z_score(0, 10, 0.0)


def test_two_sample_z_on_known_proportions():
    assert ST.two_sample_z(43, 50, 430, 500) == 0.0           # equal proportions
    assert ST.two_sample_z(0, 50, 0, 500) == 0.0              # both empty
    assert ST.two_sample_z(50, 50, 500, 500) == 0.0           # both full
    # 40 of 50 against 43 of 50: pooled 0.83, error sqrt(0.83 * 0.17 * 0.04)
    expected = (0.8 - 0.86) / math.sqrt(0.83 * 0.17 * (1 / 50 + 1 / 50))
    assert ST.two_sample_z(40, 50, 43, 50) == pytest.approx(expected)
    assert ST.two_sample_z(43, 50, 40, 50) == pytest.approx(-expected)
    # a larger sample on our side sharpens the test
    assert abs(ST.two_sample_z(400, 500, 43, 50)) > abs(ST.two_sample_z(40, 50, 43, 50))


def test_french_count_is_his_percentage_of_fifty_runs():
    assert ST.FRENCH_RUNS == RP.RUNS_PER_PROBLEM == 50
    assert ST.french_count(86) == 43.0
    assert ST.french_count(73) == 36.5          # the odd percentages are fractional runs
    assert ST.french_count(0) == 0.0


def test_normal_tail_and_false_alarm():
    assert ST.normal_tail(0.0) == pytest.approx(0.5)
    assert ST.normal_tail(1.96) == pytest.approx(0.025, abs=0.0005)
    assert ST.false_alarm(2) == pytest.approx(0.046, abs=0.001)
    assert ST.false_alarm(4) == pytest.approx(6.3e-5, rel=0.01)


def test_chi_square_homogeneity_on_known_tables():
    assert ST.chi_square_homogeneity([10, 10], [10, 10]) == (0.0, 1)
    assert ST.chi_square_homogeneity([20, 0], [0, 20]) == (40.0, 1)
    # a cell empty in both samples is dropped, one less degree of freedom
    assert ST.chi_square_homogeneity([20, 0, 0], [0, 20, 0]) == (40.0, 1)
    # a 2 x 3 table by hand: ours 30/10/10, his 20/20/10 (totals 50 each)
    ours, his = [30, 10, 10], [20, 20, 10]
    expected = [25, 15, 10]
    by_hand = sum((o - e) ** 2 / e + (h - e) ** 2 / e for o, h, e in zip(ours, his, expected))
    stat, df = ST.chi_square_homogeneity(ours, his)
    assert stat == pytest.approx(by_hand) and df == 2
    assert ST.chi_square_homogeneity([], []) == (0.0, 0)
    assert ST.chi_square_homogeneity([0, 0], [0, 0]) == (0.0, 0)
    with pytest.raises(ValueError):
        ST.chi_square_homogeneity([1, 2], [1])


def test_chi_square_limit_and_p_by_wilson_hilferty():
    # the exact 5-percent limits are 3.84 (df 1), 5.99 (df 2), 11.07 (df 5); the approximation
    # is within 3 percent at df 1 and closer above, as D-072 says
    assert ST.chi_square_limit(1, 0.05) == pytest.approx(3.84, rel=0.03)
    assert ST.chi_square_limit(2, 0.05) == pytest.approx(5.99, rel=0.02)
    assert ST.chi_square_limit(5, 0.05) == pytest.approx(11.07, rel=0.01)
    assert ST.chi_square_limit(0) == 0.0
    for df in (1, 2, 5, 10):
        limit = ST.chi_square_limit(df, 0.05)
        assert ST.chi_square_p(limit, df) == pytest.approx(0.05, abs=0.003)
    assert ST.chi_square_p(0.0, 3) == 1.0 and ST.chi_square_p(5.0, 0) == 1.0
    assert ST.chi_square_p(1.0, 1) > ST.chi_square_p(4.0, 1) > ST.chi_square_p(16.0, 1)


# --- the seeds and the survey --------------------------------------------------------------

def test_batch_seeds_depend_on_the_bench_seed_and_the_problem_only():
    a = H.batch_seed_for(1, "surround-05")
    assert a == H.batch_seed_for(1, "surround-05")
    assert a != H.batch_seed_for(2, "surround-05")
    assert a != H.batch_seed_for(1, "surround-06")
    assert 0 <= a < 2 ** 64


def test_a_survey_tallies_the_same_runs_as_run_batch(surveys):
    sv = surveys["surround-05"]
    problem = load_problem("surround-05")
    seeds = run_seeds(sv.batch_seed, RUNS)
    results = [run_problem(problem, CFG, s) for s in seeds]
    assert [r[0] for r in sv.results] == seeds
    assert [r[1] for r in sv.results] == [r.answer for r in results]
    assert sv.runs == RUNS and sv.profile == "standard" and sv.seed == 1
    assert sum(t.count for t in sv.tallies) == RUNS
    assert sum(t.frequency for t in sv.tallies) == pytest.approx(100.0)
    assert sv.codelets == pytest.approx(sum(r.codelets for r in results) / RUNS)
    for t in sv.tallies:
        mine = [r for r in results if r.answer == t.answer]
        assert t.count == len(mine)
        assert t.structure == pytest.approx(sum(r.structure_value for r in mine) / len(mine))
        assert t.codelets == pytest.approx(sum(r.codelets for r in mine) / len(mine))
        assert t.label == next((k for k, v in problem.answers.items() if v == t.answer), "")
    assert sv.tally("no-such-object") is None
    assert sv.tally(sv.tallies[0].answer) is sv.tallies[0]


def test_a_survey_is_reproducible_and_independent_of_the_other_problems(surveys):
    again = H.survey(load_problem("surround-05"), CFG, 1, RUNS)
    assert again == surveys["surround-05"]
    other = H.survey(load_problem("surround-05"), CFG, 2, RUNS)
    assert other.batch_seed != again.batch_seed


# --- the comparison ------------------------------------------------------------------------

def test_a_comparison_puts_french_numbers_beside_ours(surveys):
    rs = RP.result_sets("surround-05")[0]
    c = H.compare(surveys["surround-05"], rs)
    assert c.set is rs and c.survey is surveys["surround-05"]
    labels = [r.label for r in c.rows]
    assert labels[:2] == ["E1", "E2"]
    assert set(labels[2:]) <= {H.OTHER}
    for r, a in zip(c.rows, rs.answers):
        assert (r.french_frequency, r.french_structure, r.french_codelets) == \
            (a.frequency, a.structure, a.codelets)
        assert r.names == a.names
        assert r.z == pytest.approx(ST.two_sample_z(r.count, RUNS, ST.french_count(a.frequency), 50))
    assert sum(r.count for r in c.rows) == RUNS
    assert c.title == 'Surround 5 [F95 p. 118]'
    assert c.df == len([r for r in c.rows if r.count or r.french_frequency]) - 1
    assert 0.0 <= c.p <= 1.0


def test_a_joint_label_takes_both_of_our_labels(surveys):
    rs = RP.result_sets("blockage-06")[0]
    sv = surveys["blockage-06"]
    c = H.compare(sv, rs)
    joint = c.rows[0]
    assert joint.label == "E1+E2"
    glasses = [t for t in sv.tallies if t.label in ("E1", "E2")]
    assert joint.count == sum(t.count for t in glasses)
    assert dict(joint.answers) == {t.answer: t.count for t in glasses}
    cup = c.rows[1]
    assert cup.label == "E3"
    assert cup.count == sum(t.count for t in sv.tallies if t.label == "E3")
    if joint.count:
        assert joint.structure == pytest.approx(
            sum(t.structure * t.count for t in glasses) / joint.count)
    else:
        assert joint.structure is None and joint.codelets is None


def test_unlabelled_answers_fall_under_other():
    problem = load_problem("surround-05")
    results = [run_problem(problem, CFG, s) for s in run_seeds(1, 3)]
    fake = [H.Tally("fork3", "", 2, 50.0, 10.0, 20.0), H.Tally("small-glass1", "E1", 2, 50.0, 30.0, 40.0)]
    sv = H.Survey("surround-05", "standard", 1, 1, 4, tuple(fake), 0, 30.0, ())
    c = H.compare(sv, RP.result_sets("surround-05")[0])
    assert [r.label for r in c.rows] == ["E1", "E2", H.OTHER]
    other = c.rows[-1]
    assert other.count == 2 and other.frequency == 50.0 and other.answers == (("fork3", 2),)
    assert other.french_frequency is None and other.z > 0
    assert c.rows[1].count == 0 and c.rows[1].structure is None
    assert c.df == 2
    del results


# --- the formats ---------------------------------------------------------------------------

def test_the_table_is_in_french_format(surveys):
    c = H.compare(surveys["surround-05"], RP.result_sets("surround-05")[0])
    lines = H.markdown_table(c)
    assert lines[0] == '### Surround 5 [F95 p. 118]'
    header = lines[2]
    for heading in ("Object Eliza touches", "Average structure", "No. of codelets run"):
        assert heading in header                        # French's headings [F95 p. 115]
    assert "Frequency, French | Frequency, ours | z |" in header
    rows = [l for l in lines[4:] if l.startswith("| ")]
    assert [r.split(" | ")[0][2:] for r in rows][:2] == ["E1", "E2"]
    assert rows[0].split(" | ")[1] == "86" and rows[1].split(" | ")[1] == "14"
    assert rows[0].split(" | ")[4] == "248" and rows[0].split(" | ")[6] == "101"
    tail = lines[-2]
    assert tail.startswith(f"{RUNS} runs of surround-05 under standard (bench seed 1, batch seed ")
    assert "chi-square" in tail and "French's n = 50." in tail
    assert lines[-1] == ""


def test_a_dash_where_french_prints_one(surveys):
    c = H.compare(surveys["surround-05"], RP.result_sets("surround-06")[0].__class__(
        **{**RP.result_sets("surround-06")[0].__dict__, "problem": "surround-05"}))
    row = [l for l in H.markdown_table(c) if l.startswith("| E2")][0]
    assert row.split(" | ")[1] == "0" and row.split(" | ")[4] == "-" and row.split(" | ")[6] == "-"


def test_the_summary_row_names_french_main_answer(surveys):
    c = H.compare(surveys["blockage-06"], RP.result_sets("blockage-06")[0])
    row = H.summary_row(c)
    assert row.startswith("| Blockage 6 (F95) | 126 | E3 | 78 | ")
    assert len(H.summary_header()) == 2
    c2 = H.compare(surveys["blockage-06"], RP.result_sets("blockage-06")[1])
    assert H.summary_row(c2).startswith("| Blockage 6 (IX-2e FCCA) | 397 | E3 | 78 | ")


def test_the_json_record_replays(surveys):
    c = H.compare(surveys["blockage-06"], RP.result_sets("blockage-06")[0])
    rec = json.loads(json.dumps(H.record_of(c)))
    assert rec["problem"] == "blockage-06" and rec["page"] == 126 and rec["runs"] == RUNS
    assert [r["label"] for r in rec["rows"]][:2] == ["E1+E2", "E3"]
    assert rec["rows"][0]["french"] == {"frequency": 22, "structure": 57, "codelets": 66}
    seed, answer, structure, codelets, capped = rec["results"][0]
    r = run_problem(load_problem("blockage-06"), CFG, seed)
    assert (r.answer, r.structure_value, r.codelets, r.capped) == (answer, structure, codelets, capped)
    assert run_seeds(rec["batch_seed"], RUNS)[0] == seed


# --- the two benches ------------------------------------------------------------------------

def test_families_covers_the_35_sets_and_buridan_1_twice():
    names = families.problems_of()
    assert len(names) == 34 and len(families.SETS) == 35
    assert names.count("buridan-01") == 1
    assert families.problems_of(only=["blockage-06", "surround-05"]) == ["surround-05", "blockage-06"]


def test_families_at_sample_size():
    cs = families.run(CFG, 1, 2, only=["surround-05", "buridan-01"])
    assert [c.set.title for c in cs] == ["Surround 5", "Buridan 1 (original program)",
                                         "Buridan 1 (modified program)"]
    assert cs[1].survey is cs[2].survey                   # one survey, two sets
    assert cs[0].survey == H.survey(load_problem("surround-05"), CFG, 1, 2)
    lines = families.report(cs)
    assert lines[0] == '## Surround [F95 pp. 115-121]'
    assert '## Buridan [F95 pp. 129-139]' in lines and '## Blockage [F95 pp. 122-129]' not in lines
    assert sum(l.startswith("### ") for l in lines) == 3
    assert '### Buridan 1 (modified program) [F95 p. 130], modified program' in lines
    assert lines[lines.index("## Summary") + 2:lines.index("## Summary") + 4] == H.summary_header()
    assert lines[lines.index("## Summary") + 4:] == [H.summary_row(c) for c in cs]


def test_fcca_covers_18_figures_and_shows_the_divergent_two_against_both():
    assert len(fcca.SETS) == 18
    assert [rs.figure for rs in fcca.divergent()] == ["IX-1d", "IX-1f"]
    assert fcca.book_version(fcca.SETS[14]).title == "Buridan 1 (modified program)"
    cs = fcca.run(CFG, 1, 2, only=["surround-10", "blockage-06"])
    assert [(c.set.source, c.set.figure, c.set.page) for c in cs] == \
        [("FCCA", "IX-1f", 395), ("F95", None, 121), ("FCCA", "IX-2e", 397)]
    assert cs[0].survey is cs[1].survey
    assert cs[0].rows[0].french_frequency == 82 and cs[1].rows[0].french_frequency == 73
    assert cs[0].rows[0].count == cs[1].rows[0].count
    lines = fcca.report(cs)
    assert lines[0].startswith("## FCCA's Figures IX-1 to IX-3 [FCCA")
    assert "(the same runs against the book's numbers, which differ [F95 p. 121]:)" in lines
    assert '### Surround 10 [FCCA IX-1f p. 395]' in lines and '### Surround 10 [F95 p. 121]' in lines


# --- the command line ----------------------------------------------------------------------

def run_cli(*args, timeout=180):
    return subprocess.run([sys.executable, "-m", "tabletop.bench", *args], cwd=PYTHON_DIR,
                          capture_output=True, text=True, timeout=timeout)


def test_cli_lists_the_benches_and_refuses_bad_arguments():
    proc = run_cli("--list")
    names = [l.split()[0] for l in proc.stdout.splitlines()]
    assert proc.returncode == 0 and names == list(CLI.BENCHES) + ["all"]
    assert names[:2] == ["families", "fcca"]
    assert run_cli().returncode == CLI.USAGE_ERROR
    assert run_cli("families", "--runs", "0").returncode == CLI.USAGE_ERROR
    proc = run_cli("families", "--problems", "nowhere-01", "--runs", "1")
    assert proc.returncode == CLI.USAGE_ERROR and "no such problem" in proc.stderr
    proc = run_cli("fcca", "--problems", "surround-03", "--runs", "1")
    assert proc.returncode == CLI.USAGE_ERROR and "not in the fcca bench" in proc.stderr
    assert run_cli("nowhere").returncode == 2        # argparse's own usage error
    assert CLI.DEFAULT_RUNS == 500


def test_cli_prints_the_tables_with_progress_and_is_reproducible(tmp_path):
    args = ("families", "--problems", "surround-05", "blockage-06", "--runs", "3", "--seed", "5")
    a = run_cli(*args, "--json", str(tmp_path / "a.json"))
    b = run_cli(*args, "--json", "-", "--quiet")
    assert a.returncode == 0 and b.returncode == 0
    assert a.stderr.splitlines()[0].startswith("[1/2] surround-05 (standard): 3 runs, ")
    assert b.stderr == f"engine: {accel.info()['engine']}\n"     # the JSON took the standard output
    expected = families.report(families.run(CFG, 5, 3, ["surround-05", "blockage-06"]))
    assert a.stdout.splitlines() == [f"engine: {accel.info()['engine']}"] + expected
    records = [json.loads(l) for l in b.stdout.splitlines()]
    assert [r["problem"] for r in records] == ["surround-05", "blockage-06"]
    assert (tmp_path / "a.json").read_text() == b.stdout
    assert all(r["seed"] == 5 and r["runs"] == 3 for r in records)


def test_cli_profile_reaches_the_runs():
    a = run_cli("families", "--problems", "buridan-12", "--runs", "8", "--json", "-", "--quiet")
    b = run_cli("families", "--problems", "buridan-12", "--runs", "8", "--json", "-", "--quiet",
                "--profile", "french-faults")
    ra, rb = json.loads(a.stdout.splitlines()[0]), json.loads(b.stdout.splitlines()[0])
    assert ra["profile"] == "standard" and rb["profile"] == "french-faults"
    assert ra["batch_seed"] == rb["batch_seed"]
    assert ra["results"] != rb["results"]

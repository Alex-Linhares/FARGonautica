"""French's two calibrations: the split, the objectives, the scans and the command line
(D-076; ANSWERS.md 7) [F95 p. 130; F95 pp. 142-144]."""

import dataclasses
import json
import os
import subprocess
import sys
from pathlib import Path

from tabletop import reported as RP
from tabletop.bench import calibrate as CA
from tabletop.bench import harness as H
from tabletop.bench import results as RS
from tabletop.config import PROFILES, V2_DEFAULTS
from tabletop.problems import load_problem

PYTHON_DIR = Path(__file__).resolve().parents[1]
RESULTS_DIR = PYTHON_DIR / "results"


# ---- the split ---------------------------------------------------------------------------

def test_the_split_is_a_recorded_half_in_the_tables_order():
    fit, held = CA.split(CA.SPLIT_SEED)
    names = CA.table_problems()
    assert len(names) == 23 and len(fit) == 12 and len(held) == 11
    assert sorted(fit + held) == sorted(names) and not set(fit) & set(held)
    assert fit == [n for n in names if n in fit] and held == [n for n in names if n in held]
    assert CA.split(CA.SPLIT_SEED) == (fit, held)              # reproducible
    assert CA.split(CA.SPLIT_SEED + 1) != (fit, held)          # and seed-dependent


def test_the_recorded_split_is_the_one_results_md_reports():
    fit, held = CA.split(CA.SPLIT_SEED)
    assert fit == ["buridan-02", "buridan-03", "buridan-07", "buridan-10", "buridan-11",
                   "buridan-12", "buridan-13", "surround-01", "surround-04", "surround-05",
                   "blockage-06", "blockage-07"]
    assert "buridan-01" in held and "surround-02" in held and "blockage-05" in held


# ---- the objectives ------------------------------------------------------------------------

def test_least_squares_mean_gap_and_within_ten_on_known_rows():
    rows = [{"problem": "a", "subjects": [75, 25], "ours": [65, 35], "fit": 10.0},
            {"problem": "b", "subjects": [40, 60], "ours": [40, 30], "fit": 30.0}]
    assert CA.sum_of_squares(rows) == 100 + 100 + 0 + 900
    assert CA.mean_gap(rows) == 20.0 and CA.within_ten(rows) == 1
    assert CA.sum_of_squares([]) == 0 and CA.mean_gap([]) == 0.0


def test_human_rows_read_a_record_through_the_tables_columns():
    row = [r for r in RP.SUBJECT_ROWS if r.problem == "surround-05"][0]
    pb = load_problem("surround-05")
    record = {"tallies": [{"answer": pb.answers["E1"], "frequency": 60.0},
                          {"answer": pb.answers["E2"], "frequency": 30.0},
                          {"answer": pb.touched, "frequency": 10.0}]}
    rows = CA.human_rows({"surround-05": record})
    assert len(rows) == 1 and rows[0]["ours"] == [60.0, 30.0]
    assert rows[0]["subjects"] == list(row.subjects) and rows[0]["tabletop"] == list(row.tabletop)
    assert rows[0]["fit"] == abs(row.subjects[row.most_frequent_for_subjects()] - 30.0)


def test_buridan_reading_is_the_balance_between_e1_and_e2():
    pb = load_problem("buridan-01")
    record = {"tallies": [{"answer": pb.answers["E1"], "frequency": 45.0},
                          {"answer": pb.answers["E2"], "frequency": 35.0},
                          {"answer": pb.touched, "frequency": 20.0}]}
    r = CA.buridan_reading(record, pb)
    assert r["E1"] == 45.0 and r["E2"] == 35.0 and r["other"] == 20.0
    assert r["imbalance"] == 10.0 and r["distance_from_50_50"] == 20.0


def test_choose_takes_the_least_objective_and_the_smallest_value_among_ties():
    scan = [{"value": 60, "o": 5.0}, {"value": 40, "o": 5.0}, {"value": 80, "o": 7.0}]
    assert CA.choose(scan, "o") == 40
    assert CA.choose([{"value": 30, "o": 1.0}] + scan, "o") == 30


def test_the_grids_start_at_the_default():
    """The v2 default, which the grids were fixed around (D-076; D-114 keeps them)."""
    v2 = dict(V2_DEFAULTS)
    assert CA.HUMAN_GRID[0] == v2["type_weight"]
    assert CA.BURIDAN_GRID[0] == v2["incoherence_penalty"]
    assert list(CA.HUMAN_GRID) == sorted(set(CA.HUMAN_GRID))
    assert list(CA.BURIDAN_GRID) == sorted(set(CA.BURIDAN_GRID))


# ---- the scans ---------------------------------------------------------------------------

def test_a_survey_task_is_the_harness_survey_under_the_changed_knob():
    rec = CA.survey_task("human", "type_weight", 80, "surround-01", 7, 3)
    config = dataclasses.replace(PROFILES["human"], type_weight=80)
    sv = H.survey(load_problem("surround-01"), config, 7, 3)
    assert rec["results"] == [list(r) for r in sv.results] and rec["profile"] == "human"
    assert rec["results"] != [list(r) for r in H.survey(load_problem("surround-01"),
                                                        PROFILES["standard"], 7, 3).results]


def test_the_buridan_scan_reads_buridan_1_under_the_modified_labels():
    grid = (40, 100)
    rec = CA.buridan_scan(seed=5, runs=3, grid=grid, workers=1)
    assert rec["problem"] == "buridan-01" and rec["program"] == "modified"
    assert rec["french"] == {"E1": 50, "E2": 50} and rec["page"] == 130
    assert [s["value"] for s in rec["scan"]] == [40, 100]
    for s in rec["scan"]:
        assert abs(s["E1"] + s["E2"] + s["other"] - 100) < 1e-9
        assert s["imbalance"] == abs(s["E1"] - s["E2"])
    assert rec["chosen"] in grid
    lines = CA.report(rec)
    assert lines[0].startswith("## The `buridan` fit") and "Chosen:" in lines[-2]


# ---- the records behind RESULTS.md §9 ---------------------------------------------------------

def test_the_profiles_carry_the_values_the_v1_scans_chose():
    """The profiles keep the values the calibrations chose on the engine before roles
    (`results/v1/`, the Calibration log); the scans re-run on the roles engine (the roles
    loop's item 12) and again on the compiled engine (`results/`, the fast-engine loop's
    item 07) are a measurement only, and a re-fit needs the author's answer (ANSWERS.md
    C5). The re-run records name their engine (D-107)."""
    v1_human = json.loads((RESULTS_DIR / "v1" / "calibrate-human.json").read_text())
    v1_buridan = json.loads((RESULTS_DIR / "v1" / "calibrate-buridan.json").read_text())
    assert v1_human["chosen"] == PROFILES["human"].type_weight == 50
    assert v1_buridan["chosen"] == PROFILES["buridan"].incoherence_penalty == 40
    human = json.loads((RESULTS_DIR / "calibrate-human.json").read_text())
    buridan = json.loads((RESULTS_DIR / "calibrate-buridan.json").read_text())
    for rec, v1 in ((human, v1_human), (buridan, v1_buridan)):
        assert rec["grid"] == v1["grid"] and rec["seed"] == v1["seed"] and rec["runs"] == v1["runs"]
        assert rec["chosen"] in rec["grid"]
    # the scans start from the default of the day: v1's 30 and 40, v3's since D-114, v4's
    # since D-115
    assert human["before"] == PROFILES["standard"].type_weight
    assert buridan["before"] == PROFILES["standard"].incoherence_penalty
    assert human["grid"] == list(CA.HUMAN_GRID) and buridan["grid"] == list(CA.BURIDAN_GRID)
    assert human["split_seed"] == CA.SPLIT_SEED and list(CA.split()) == [
        human["fit_problems"], human["held_out_problems"]]
    assert "results" not in next(iter(human["scan"][0]["surveys"].values()))
    assert human["engine"] == buridan["engine"] == {"engine": "compiled"}
    assert "engine" not in v1_human and "engine" not in v1_buridan


def test_the_penalty_scan_was_flat_on_v1_and_the_default_is_kept():
    # RESULTS.md §9.2: on v1 every penalty gave the same runs, so the Worldview never rated
    # a pair; on the roles engine the readings move with the penalty (the Worldview rates
    # pairs now), and the profile's 40 is kept until the author re-fits (ANSWERS.md C5).
    v1 = json.loads((RESULTS_DIR / "v1" / "calibrate-buridan.json").read_text())
    readings = {(s["E1"], s["E2"]) for s in v1["scan"]}
    assert len(readings) == 1 and v1["chosen"] == v1["before"] == 40
    buridan = json.loads((RESULTS_DIR / "calibrate-buridan.json").read_text())
    # since v4 (D-115) the default is 24 (27 in v3, D-114) and the profile keeps its 40 on
    # top of it
    assert buridan["before"] == PROFILES["standard"].incoherence_penalty == 24
    assert PROFILES["buridan"].incoherence_penalty == 40
    assert len({(s["E1"], s["E2"]) for s in buridan["scan"]}) > 1


def test_the_calibrated_families_records_are_at_full_size():
    res = RS.Results(RESULTS_DIR)
    for name, profile in (("families-human", "human"), ("families-buridan", "buridan")):
        records = res[name]
        assert len(records) == 35 and {r["profile"] for r in records} == {profile}
        assert all(r["runs"] == 500 and r["seed"] == 1 for r in records)
    # buridan's 40 is v2's default, v4's is 24 (v3's 27, D-114, D-115): the two differ somewhere
    assert any([row["ours"] for row in res.comparison("families-buridan", r["problem"],
                                                       r["program"])["rows"]]
               != [row["ours"] for row in r["rows"]] for r in res["families"])
    humans = res["humans"]
    assert all(set(r["ours"]) == {"standard", "human"} for r in humans)


# ---- the command line ----------------------------------------------------------------------

def test_the_command_line(tmp_path):
    out = tmp_path / "b.json"
    proc = subprocess.run([sys.executable, "-m", "tabletop.bench.calibrate", "buridan", "--runs",
                           "2", "--quiet", "--json", str(out)], cwd=PYTHON_DIR,
                          capture_output=True, text=True)
    assert proc.returncode == 0 and "## The `buridan` fit" in proc.stdout
    # the first line names the engine, and the record carries `accel.info()` (D-107)
    engine = os.environ.get("TABLETOP_ENGINE") or "compiled"
    assert proc.stdout.splitlines()[0] == f"engine: {engine}"
    rec = json.loads(out.read_text())
    assert rec["profile"] == "buridan" and len(rec["scan"]) == len(CA.BURIDAN_GRID)
    assert rec["engine"]["engine"] == engine and "rng" in rec["engine"]
    proc = subprocess.run([sys.executable, "-m", "tabletop.bench.calibrate", "buridan", "--runs",
                           "1", "--quiet", "--json", "-"], cwd=PYTHON_DIR,
                          capture_output=True, text=True)
    assert proc.returncode == 0 and proc.stderr.strip() == f"engine: {engine}"
    assert json.loads(proc.stdout)["engine"]["engine"] == engine
    proc = subprocess.run([sys.executable, "-m", "tabletop.bench.calibrate", "human", "--runs",
                           "0"], cwd=PYTHON_DIR, capture_output=True, text=True)
    assert proc.returncode == 2
    proc = subprocess.run([sys.executable, "-m", "tabletop.bench.calibrate", "other"],
                          cwd=PYTHON_DIR, capture_output=True, text=True)
    assert proc.returncode == 2

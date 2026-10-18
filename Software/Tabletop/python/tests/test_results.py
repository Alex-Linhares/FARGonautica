"""The freeze and the results file (D-075): `config_defaults.lock` equals every profile's
`Config`; `results/NAME.json` holds every bench's stripped records at the full size; every
marked block of RESULTS.md is what the builder generates from those records; the builder's
own readings (the fault marks, the stripping, the direction pairs) on known inputs."""

import dataclasses
import json
import re
from pathlib import Path

import pytest

from tabletop.bench import families
from tabletop.bench import probes as PB
from tabletop.bench import results as RS
from tabletop.config import PROFILES
from tabletop.problems import catalogue, load_problem

PYTHON_DIR = Path(__file__).resolve().parents[1]
LOCK = PYTHON_DIR / "config_defaults.lock"
RESULTS_DIR = PYTHON_DIR / "results"
RESULTS_MD = PYTHON_DIR / "RESULTS.md"
FULL_RUNS = 500
BENCH_SEED = 1


# ---- the lock (TASK.md's contract; the gate's check 13) ------------------------------------

def lock_of_profiles() -> dict:
    return {name: {f.name: repr(getattr(cfg, f.name)) for f in dataclasses.fields(cfg)}
            for name, cfg in PROFILES.items()}


def test_the_lock_equals_the_profiles():
    locked = json.loads(LOCK.read_text(encoding="utf-8"))
    assert locked == lock_of_profiles()


def test_the_lock_covers_every_profile_and_field():
    locked = json.loads(LOCK.read_text(encoding="utf-8"))
    assert list(locked) == sorted(PROFILES)
    fields = {f.name for f in dataclasses.fields(PROFILES["standard"])}
    for name, values in locked.items():
        assert set(values) == fields, name
        assert values["profile"] == repr(name)


# ---- the stripped records ----------------------------------------------------------------

@pytest.fixture(scope="module")
def results() -> RS.Results:
    return RS.Results(RESULTS_DIR)


def test_every_bench_has_stripped_records_at_full_size(results):
    for name in RS.BENCHES:
        if name in RS.V1_FIRST:      # recorded on v1 first; the roles run comes with the roles
            assert (RESULTS_DIR / RS.V1_DIR / f"{name}.json").exists(), name
            assert not (RESULTS_DIR / f"{name}.json").exists(), name
            continue
        assert (RESULTS_DIR / f"{name}.json").exists(), name
        assert results[name], name
    for r in results["families"]:
        assert r["runs"] == FULL_RUNS and r["seed"] == BENCH_SEED
        assert r["results_count"] == FULL_RUNS and "results" not in r
        assert r["median_codelets"] > 0 and len(r["codelets_quartiles"]) == 2
    assert len(results["families"]) == 35 and len(results["fcca"]) == 20
    assert len(results["families-french-faults"]) == 35
    assert {r["profile"] for r in results["families-french-faults"]} == {"french-faults"}
    assert len(results["potelbat"]) == len(catalogue())
    assert len(results["humans"]) == 23
    assert len([r for r in results["stated"] if "number" in r]) == 25
    assert len(results["families-human"]) == 35 and len(results["families-buridan"]) == 35
    assert len(results["calibrate-human"]) == 1 and len(results["calibrate-buridan"]) == 1
    assert len(results["temperature"]) == 12 and all(r["paths_count"] == FULL_RUNS
                                                      and "paths" not in r
                                                      for r in results["temperature"])
    assert len(results["lesions"]) == 32 and len(results["rho"]) == 28
    tables, speed = results["robustness"]
    assert tables["kind"] == "tables" and tables["tables"] == FULL_RUNS
    assert tables["results_count"] == FULL_RUNS and len(tables["per_table"]) == FULL_RUNS
    # v3 (D-114): one capped run, and orphan CLAIMED tags on 6 tables, the defect RESULTS.md
    # §12 reports (a BARRIER broken by check-roles leaves its CLAIMED standing); any other
    # violation fails here
    assert tables["capped"] <= 1 and tables["tables_with_faults"] <= 6
    assert all(re.fullmatch(r"r\d+ is CLAIMED without a BARRIER", kind)
               for kind in tables["fault_kinds"]), tables["fault_kinds"]
    assert tables["roles"]["tags_per_run"] > 0 and all(len(row) == 6 for row in tables["per_table"])
    assert speed["kind"] == "speed" and len(speed["rows"]) == 4
    assert [r["problem"] for r in results["churn"]] == list(PB.DEFAULT_PROBLEMS)
    assert all(r["runs"] == FULL_RUNS and r["results_count"] == FULL_RUNS
               for r in results["churn"])
    assert len(results["faults"]) == 45 and len(results["focus"]) == 9


def test_no_record_keeps_its_runs(results):
    def runs_in(value) -> bool:
        if isinstance(value, dict):
            return any(k == "results" or runs_in(v) for k, v in value.items())
        if isinstance(value, list):
            return any(runs_in(v) for v in value)
        return False
    for name in RS.BENCHES:
        for r in results[name]:
            assert not runs_in(r), name
            assert "paths" not in r


def test_stripping_a_small_bench_output(tmp_path):
    cfg = PROFILES["standard"]
    comparisons = families.run(cfg, 7, 3, ["blockage-06"])
    src, dst = tmp_path / "bench-results", tmp_path / "results"
    src.mkdir()
    RS.write_records(families.records(comparisons), src / "families.json")
    written = RS.strip(src, dst)
    assert written == [dst / "families.json"]
    [r] = RS.read_records(dst / "families.json")
    assert "results" not in r and r["results_count"] == 3 and r["runs"] == 3
    codelets = sorted(x[3] for x in families.records(comparisons)[0]["results"])
    assert r["median_codelets"] == codelets[1]
    assert r["codelets_quartiles"] == [codelets[0], codelets[2]]


def test_stripping_merges_parts(tmp_path):
    src, dst = tmp_path / "bench-results", tmp_path / "results"
    src.mkdir()
    for part in ("lesions-a", "lesions-b"):
        RS.write_records([{"bench": "lesions", "variant": part,
                           "survey": {"results": [[1, "x", 2.0, 10, False]]}}],
                         src / f"{part}.json")
    RS.strip(src, dst)
    merged = RS.read_records(dst / "lesions.json")
    assert [r["variant"] for r in merged] == ["lesions-a", "lesions-b"]
    assert merged[0]["survey"] == {"results_count": 1, "median_codelets": 10,
                                   "codelets_quartiles": [10, 10]}


# ---- the v1 column source (D-078) --------------------------------------------------------

V1_DIR = RESULTS_DIR / RS.V1_DIR


def test_v1_holds_every_bench_as_the_baseline_left_it(results):
    assert (V1_DIR / "README.md").exists()
    assert results.v1 is not None and results.v1.directory == V1_DIR
    assert results.v1.v1 is None, "the v1 folder has no v1 of its own"
    for name in RS.BENCHES:
        if name in RS.ROLES_ONLY:    # reads the role pool, which v1 had not (D-106)
            assert not (V1_DIR / f"{name}.json").exists(), name
            continue
        assert (V1_DIR / f"{name}.json").exists(), name
        assert results.v1[name], name
    assert len(results.v1["families"]) == 35 and len(results.v1["fcca"]) == 20
    for r in results.v1["families"]:
        assert r["runs"] == FULL_RUNS and r["seed"] == BENCH_SEED and "results" not in r


def test_v1_row_reads_the_baseline_column(results):
    row = RS.v1_row(results, "families", "surround-05", "E1")
    assert row is not None and {"label", "french", "ours", "z"} <= set(row)
    assert row["label"] == "E1" and 0 <= row["ours"]["frequency"] <= 100
    assert RS.v1_row(results, "families", "surround-05", "E9") is None
    assert RS.v1_row(results, "families", "no-such-problem", "E1") is None
    assert RS.v1_row(RS.Results(RESULTS_DIR, with_v1=False), "families", "surround-05", "E1") is None
    assert RS.v1_row(RS.Results(V1_DIR), "families", "surround-05", "E1") is None


def test_the_three_column_helpers():
    """z against v1 (D-078): the two-sample z at n = 500 each, None without a v1 value, 0
    where there is no error to divide by; a v1 record by nested field; a dash without one."""
    assert RS.z_against_v1(60.0, 500, 50.0, 500) == pytest.approx(3.19, abs=0.01)
    assert RS.z_against_v1(50.0, 500, 60.0, 500) == pytest.approx(-3.19, abs=0.01)
    assert RS.z_against_v1(50.0, 500, None, 0) is None and RS.z_against_v1(None, 500, 50.0, 500) is None
    assert RS.z_against_v1(0.0, 500, 0.0, 500) == 0.0
    assert RS.signed(None) == "-" and RS.signed(1.234) == "+1.23" and RS.v1_pct(None) == "-"
    res = RS.Results(RESULTS_DIR)
    r = RS.v1_record(res, "rho", variant="rho 3", survey__problem="surround-05")
    assert r is not None and r["survey"]["problem"] == "surround-05" and r["variant"] == "rho 3"
    assert RS.v1_record(res, "rho", variant="rho 99") is None
    assert RS.v1_record(RS.Results(RESULTS_DIR, with_v1=False), "rho", variant="rho 3") is None
    assert RS.v1_top(res, "rho", variant="rho 3", survey__problem="surround-05").count("|") == 1
    assert RS.v1_top(RS.Results(RESULTS_DIR, with_v1=False), "rho", variant="rho 3") == "- | -"


def test_the_roles_families_block_reads_gained_and_lost_apart(results):
    """RESULTS.md §11.1 (D-078): one row per result set with French | v1 | roles and the z
    against v1; the moved answers counted as gained and lost, never netted."""
    lines = RS.roles_families_tables(results)
    rows = [l for l in lines if l.startswith("| ") and not l.startswith("| Result set")]
    assert len(rows) == 35
    assert lines[0].startswith("| Result set | French's main answer | French % | v1 % | roles % |")
    summary = next(l for l in lines if l.startswith("**Answers moved beyond z = 2"))
    assert re.search(r"gained \d+, lost \d+", summary)
    assert RS.roles_families_tables(RS.Results(RESULTS_DIR, with_v1=False)) == [
        "(no families records with a v1 source)"]


def test_the_objective_of_each_engine_and_the_v3_block(results):
    """D-114, D-115, D-116 to D-120: J = (2F + H) / 3 from the families records, as the
    Bayesian optimizations measured it (v2 0.4637, v3 0.2629, v4 0.2113 and v5 0.2039 at 500
    runs, bench seed 1; v1 0.4730), and the v3 block's two tables, French | v1 | v2 | v3, its
    v3 column read from `results/v3/` since v4; the v4 block's from `results/v4/` since v5."""
    scores = {name: RS.objective(r) for name, r in (("v1", results.v1), ("v2", results.v2),
                                                    ("v3", results.v3), ("v4", results.v4),
                                                    ("v5", results))}
    assert {s["french"] for s in scores.values()} == {37}
    assert {s["human"] for s in scores.values()} == {28}
    assert scores["v1"]["J"] == pytest.approx(0.4730, abs=5e-5)
    assert scores["v2"]["J"] == pytest.approx(0.4637, abs=5e-5)
    assert scores["v3"]["J"] == pytest.approx(0.2629, abs=5e-5)
    assert scores["v4"]["J"] == pytest.approx(0.2113, abs=5e-5)
    assert scores["v5"]["J"] == pytest.approx(0.2039, abs=5e-5)
    for s in scores.values():
        assert s["J"] == pytest.approx((2 * s["F"] + s["H"]) / 3)
    lines = RS.v3_tables(results)
    assert lines[0].startswith("| Engine | J | F (French, mean TV) | H (human, mean TV) |")
    assert sum(l.startswith("| v") for l in lines) == 3
    assert len([l for l in lines if l.startswith("| ") and "Result set" not in l
                and not l.startswith("| v") and not l.startswith("| Engine")]) == 35
    assert RS.v3_tables(RS.Results(RESULTS_DIR, with_v1=False)) == [
        "(no families records with v1 and v2 sources)"]
    lines = RS.v4_tables(results)
    assert sum(l.startswith("| v") for l in lines) == 4
    assert lines[2].startswith("| v1 |") and lines[5].startswith("| v4 | 0.2113 |")
    assert len([l for l in lines if l.startswith("| ") and "Result set" not in l
                and not l.startswith("| v") and not l.startswith("| Engine")]) == 35
    assert RS.v4_tables(RS.Results(RESULTS_DIR, with_v1=False)) == [
        "(no families records with v1, v2 and v3 sources)"]
    lines = RS.v5_tables(results)
    assert sum(l.startswith("| v") for l in lines) == 5
    assert lines[5].startswith("| v4 | 0.2113 |") and lines[6].startswith("| v5 | 0.2039 |")
    assert len([l for l in lines if l.startswith("| ") and "Result set" not in l
                and not l.startswith("| v") and not l.startswith("| Engine")]) == 35
    assert RS.v5_tables(RS.Results(RESULTS_DIR, with_v1=False)) == [
        "(no families records with v1, v2, v3 and v4 sources)"]


def test_the_v1_tables_render_as_before(results):
    without = RS.Results(RESULTS_DIR, with_v1=False)
    assert without.v1 is None
    for name in RS.BLOCKS:
        if name in RS.V1_BLOCKS:     # the v1 column is the block's point (D-102)
            assert RS.render(without, name) != RS.render(results, name), name
            continue
        assert RS.render(without, name) == RS.render(results, name), name


# ---- RESULTS.md against the records (D-075) ---------------------------------------------

def test_every_block_of_results_md_is_generated_from_the_records(results):
    text = RESULTS_MD.read_text(encoding="utf-8")
    blocks = RS.blocks_in(text)
    assert set(blocks) == set(RS.BLOCKS), "every block is in the file"
    for name, body in blocks.items():
        assert body == RS.render(results, name), name


def test_results_md_has_the_sections_the_protocol_needs():
    text = RESULTS_MD.read_text(encoding="utf-8")
    assert "## Calibration log" in text
    assert "## Re-interpretation log" in text and "## 11. Roles" in text and "D-078" in text
    assert text.index("## 11. Roles") < text.index("## Calibration log") \
        < text.index("## Re-interpretation log")
    assert re.search(r"^### 20\d\d-\d\d-\d\d", text[text.index("## Calibration log"):], re.M)
    assert "## 9. Calibrated profiles" in text and "D-076" in text
    assert "## 1. The measures" in text and "`french-faults`" in text
    assert "**M7 Lesions.**" in text and "## 6. Probes" in text
    assert "FCCA p. 365" in text and "FCCA p. 364" in text
    for d in ("D-022", "D-036", "D-039", "D-052", "D-056", "D-062"):
        assert d in text, d


# ---- the builder's readings ---------------------------------------------------------------

def test_fault_marks_follow_spec_6b():
    # no chapter 5 layout has three identical adjacent objects [F95 p. 140], so fault 1
    # marks none; the rule itself is tested on a synthetic row of three cups below
    assert all(1 not in RS.fault_marks(p) for p in catalogue())
    assert 2 in RS.fault_marks("blockage-03") and 2 in RS.fault_marks("blockage-04")
    assert RS.fault_marks("surround-05")[:] == [2, 3, 4, 5]
    assert RS.fault_marks("figure-ground-01") == [6]
    assert all(7 not in RS.fault_marks(p) for p in catalogue())


def test_m7_reads_every_lesion_on_every_problem(results):
    lines = RS.m7_lines(results)
    table = [l for l in lines if l.startswith("| ") and not l.startswith("| Lesion")]
    assert len(table) == 7
    for line in table:
        cells = line.split(" | ")[2:]
        assert len(cells) == 4
        assert all(c.rstrip(" |") in ("as predicted", "no change", "against")
                   or c.startswith("moved") for c in cells)
    assert lines[-1].startswith("Of 28 lesion-problem cells")


def test_three_adjacent_cups_make_a_run_of_three():
    import dataclasses
    pb = load_problem("blockage-01")
    cup = pb.touched_object()
    more = [dataclasses.replace(cup, id=f"cup{n}", x=cup.x + 10 * n) for n in (1, 2)]
    far = dataclasses.replace(cup, id="cup7", x=cup.x + 60)
    pb3 = dataclasses.replace(pb, objects=pb.objects + tuple(more) + (far,))
    comps = RS.components_by_type(pb3)["cup"]
    assert sorted(len(c) for c in comps) == [1, 3]
    assert not RS.is_isolated(more[0], pb3) and RS.is_isolated(far, pb3)


def test_answer_kinds():
    assert RS.answer_kind("the touched cup itself, on Henry's side") == "the touched cup"
    assert RS.answer_kind("Eliza's cup, lower right, diagonally opposite the touched cup") == "a cup"
    assert RS.answer_kind("either of Eliza's two small glasses, lower left") == "a small glass"
    assert RS.answer_kind("Eliza's saucer in place of the glass") == "a saucer"
    assert RS.answer_kind("either of Henry's two forks") == "a fork"
    assert RS.answer_kind("something else") == "something else"


def test_direction_pairs_are_within_families_and_significant(results):
    pairs = RS.direction_pairs(results)
    assert pairs
    for p in pairs:
        assert p["a"].startswith(p["family"]) and p["b"].startswith(p["family"])
        fa, fb = p["french"]
        se = RS.standard_error(fa, RS.FRENCH_RUNS, fb, RS.FRENCH_RUNS)
        assert abs(fa - fb) > RS.SIGMAS * se
        assert p["same"] == ((p["ours"][0] - p["ours"][1]) * (fa - fb) > 0)
        if p["reversed"]:
            assert not p["same"]
    # Surround 5's glass against Surround 1's is a pair French's numbers settle [F95 p. 118]
    assert any(p["a"] == "surround-01" and p["b"] == "surround-05"
               and p["names"] == "a small glass" for p in pairs)
    # Blockage 1's glass (E1) is matched with Blockage 6's glasses (E1+E2) by kind
    assert any(p["a"] == "blockage-01" and p["b"] == "blockage-06"
               and p["names"] == "a small glass" for p in pairs)


def test_every_record_names_the_compiled_engine_and_every_block_says_so(results):
    """Every stripped record of `results/` carries `engine` with the engine's name (the
    whole `accel.info()` stays in the bench's JSON), the compiled engine's since the
    fast-engine loop's item 07, and every generated block begins with an engine line; the
    v1 records predate the field and read as the python engine (D-107, D-075)."""
    for name in RS.BENCHES:
        if name in RS.ROLES_ONLY or not results[name]:
            continue
        assert all(r["engine"] == {"engine": "compiled"} for r in results[name]), name
    assert RS.engines_of(results["families"]) == ["compiled"]
    assert RS.engines_of(results.v1["families"]) == ["python"]
    assert RS.engines_of([{"engine": {"engine": "python", "rng": "splitmix64"}},
                          {"engine": {"engine": "compiled"}}, {}]) == ["python", "compiled"]
    assert RS.engine_line(results, "ch2") == "*Engine: compiled; the v1 columns: python, before roles.*"
    assert RS.engine_line(results, "churn") == "*Engine: compiled.*"
    assert RS.engine_line(RS.Results(RESULTS_DIR, with_v1=False), "ch2") == "*Engine: compiled.*"
    assert RS.engine_line(RS.Results(RESULTS_DIR / "nowhere"), "ch2") == ""
    for name in RS.BLOCKS:
        assert RS.render(results, name).startswith("*Engine: compiled"), name
    text = RESULTS_MD.read_text(encoding="utf-8")
    for name in RS.BLOCKS:
        start = text.index(RS.begin_marker(name)) + len(RS.begin_marker(name))
        assert text[start:].lstrip("\n").startswith("*Engine: compiled"), name


def test_quartiles():
    assert RS.quartiles([]) == (None, None, None)
    assert RS.quartiles([5]) == (5, 5, 5)
    assert RS.quartiles([1, 2, 3, 4]) == (1, 2, 3)
    assert RS.quartiles([3, 1, 2]) == (1, 2, 3)


def test_the_command_line(tmp_path):
    import subprocess
    import sys
    proc = subprocess.run([sys.executable, "-m", "tabletop.bench.results", "tables",
                           str(RESULTS_DIR)], cwd=PYTHON_DIR, capture_output=True, text=True)
    assert proc.returncode == 0
    for name in RS.BLOCKS:
        assert RS.begin_marker(name) in proc.stdout and RS.end_marker(name) in proc.stdout
    proc = subprocess.run([sys.executable, "-m", "tabletop.bench.results"], cwd=PYTHON_DIR,
                          capture_output=True, text=True)
    assert proc.returncode == 2


def test_capped_runs_are_read_over_every_bench(results):
    surveys = RS.survey_records(results)
    benches = {b for b, _, _ in surveys}
    assert {"families", "lesions", "humans", "robustness", "temperature"} <= benches
    capped = {(b, str(label)): sv["capped"] for b, label, sv in surveys if sv["capped"]}
    # v3 (D-114) and v4 (D-115): the lesion that clamps T high caps every run, as on v2; the
    # target benches cap none; the long runs reach the cap on junk-laden tables
    assert capped[("lesions", "temperature-high")] == 500       # on each of the four problems
    assert not {b for b, _ in capped} & {"families", "fcca", "humans", "temperature", "churn"}
    lines = RS.capped_table(results)
    assert lines[0].startswith("| Bench |") and any(l.startswith("| all |") for l in lines)
    assert any(l.startswith("| lesions | 32 | 16000 | 2000 |") for l in lines)    # v4 (v3: 2000 + 3)
    assert any(l.startswith("| families | 35 | 17500 | 0 |") for l in lines)

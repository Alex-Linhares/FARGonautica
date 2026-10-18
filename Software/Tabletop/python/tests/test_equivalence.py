"""The statistical equivalence of the pure engine, the fast version (D-107, D-113): a few
families problems x 30 runs on the compiled engine against the pure engine's recorded
200-run distributions (`tests/data/equivalence.json`, written by `tools/equivalence.py`),
each within the total-variation tolerance that 30 against 200 allows (the null's 99.9th
percentile, simulated from the recorded distribution); the tool's statistics; the record's
provenance (engine, commit, runs); and the tool itself, small, as a subprocess. The whole
families table at 200 runs per engine is `tests_slow/test_equivalence_slow.py`. The tests
that run the compiled engine skip, with a reason, only when the python engine was asked for
explicitly; the statistics and the record need no engine.
"""

import importlib.util
import json
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel

PYTHON_DIR = Path(__file__).resolve().parents[1]
TOOL = PYTHON_DIR / "tools" / "equivalence.py"
RECORD = PYTHON_DIR / "tests" / "data" / "equivalence.json"
PROBLEMS = ("surround-05", "blockage-06", "buridan-01")   # the goldens' two, and the widest null
RUNS = 30

compiled_only = pytest.mark.skipif(
    accel.info()["engine"] != "compiled",
    reason="the compiled engine is not the one running (TABLETOP_ENGINE=python was requested "
           "explicitly)")


def load_tool():
    spec = importlib.util.spec_from_file_location("equivalence", TOOL)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def tool():
    return load_tool()


@pytest.fixture(scope="module")
def record() -> dict:
    return json.loads(RECORD.read_text(encoding="utf-8"))


# -- the statistics ---------------------------------------------------------------------

def test_counts_keep_the_order_first_seen(tool):
    assert tool.counts_of(["b", "a", "b", "c", "a", "b"]) == {"b": 3, "a": 2, "c": 1}
    assert list(tool.counts_of(["b", "a"])) == ["b", "a"]


def test_total_variation_is_a_distance_between_frequencies(tool):
    assert tool.total_variation({"x": 3, "y": 1}, {"x": 3, "y": 1}) == 0.0
    assert tool.total_variation({"x": 4}, {"y": 2}) == 1.0
    assert tool.total_variation({"x": 3, "y": 1}, {"x": 1, "y": 3}) == pytest.approx(0.5)
    # frequencies, not counts: the same distribution at two sizes is at distance 0
    assert tool.total_variation({"x": 30, "y": 10}, {"x": 3, "y": 1}) == 0.0


def test_the_null_is_seeded_and_widens_as_the_sample_shrinks(tool):
    pooled = {"a": 120, "b": 50, "c": 20, "d": 10}
    q200 = tool.null_quantiles(pooled, 200, 200, draws=500)
    assert q200 == tool.null_quantiles(pooled, 200, 200, draws=500)
    assert 0.0 < q200[0.99] <= q200[0.999] < 1.0
    q30 = tool.null_quantiles(pooled, 30, 200, draws=500)
    assert q30[0.999] > q200[0.999]
    assert tool.null_quantiles(pooled, 200, 200, draws=500, seed=1) != q200


def test_the_tolerance_covers_the_null_at_the_stated_runs(tool, record):
    """D-113: the tolerance was derived from the null at RUNS; the recorded distributions'
    simulated 99.9th percentile stays below it (a cheaper simulation than the tool's)."""
    assert tool.RUNS == 200 and tool.TOLERANCE == 0.25
    worst = max(tool.null_quantiles(p["counts"], tool.RUNS, tool.RUNS, draws=300)[0.999]
                for p in record["problems"].values())
    assert worst < tool.TOLERANCE


# -- the record ---------------------------------------------------------------------------

def test_the_record_names_its_engine_commit_and_runs(tool, record):
    assert record["engine"] == "python" and record["info"]["engine"] == "python"
    assert record["info"]["rng"] == "splitmix64"
    assert re.fullmatch(r"[0-9a-f]{40}", record["commit"])
    assert record["runs"] == tool.RUNS and record["tolerance"] == tool.TOLERANCE
    assert record["profile"] == "standard" and record["seed"] == tool.BENCH_SEED
    assert len(record["problems"]) == 34
    for name, p in record["problems"].items():
        assert sum(p["counts"].values()) == tool.RUNS, name
        assert isinstance(p["batch_seed"], int)


def test_the_record_holds_every_families_problem(record):
    from tabletop.bench import families
    assert list(record["problems"]) == families.problems_of()


# -- the fast comparison, compiled against the recorded pure distributions ----------------

@compiled_only
@pytest.mark.parametrize("name", PROBLEMS)
def test_the_compiled_engine_is_within_the_recorded_pure_distribution(tool, record, name):
    from tabletop.bench.harness import batch_seed_for
    from tabletop.problems import load_problem
    from tabletop.run import run_problem
    from tabletop.config import PROFILES
    recorded = record["problems"][name]
    batch = batch_seed_for(tool.BENCH_SEED, name)
    assert batch == recorded["batch_seed"]
    problem, config = load_problem(name), PROFILES["standard"]
    results = [run_problem(problem, config, s) for s in accel.run_seeds(batch, RUNS)]
    assert all(r.engine == "compiled" for r in results)
    mine = tool.counts_of([r.answer for r in results])
    allowed = tool.null_quantiles(recorded["counts"], RUNS, tool.RUNS)[0.999]
    tv = tool.total_variation(mine, recorded["counts"])
    assert tv <= allowed, (name, tv, allowed, mine, recorded["counts"])


# -- the tool, small ----------------------------------------------------------------------

def run_tool(*args, env=None) -> subprocess.CompletedProcess:
    return subprocess.run([sys.executable, "-I", str(TOOL), *args], capture_output=True,
                          text=True, cwd=str(PYTHON_DIR.parent),
                          env=env if env is not None else dict(os.environ))


def test_help_names_the_two_knobs():
    proc = run_tool("--help")
    assert proc.returncode == 0
    assert "--runs" in proc.stdout and "--tolerance" in proc.stdout
    assert "--null-only" in proc.stdout and "--worker" not in proc.stdout


@compiled_only
def test_the_tool_compares_the_two_engines_in_two_processes(tmp_path):
    out, rec = tmp_path / "out.json", tmp_path / "record.json"
    env = {k: v for k, v in os.environ.items() if k != "TABLETOP_ENGINE"}
    proc = run_tool("--only", "1", "--runs", "3", "--out", str(out), "--record", str(rec),
                    env=env)
    assert proc.returncode == 0, proc.stderr
    lines = proc.stdout.splitlines()
    assert lines[0] == "engine: compiled against python"
    assert "runs 3 per problem per engine" in lines[1]
    assert lines[-1].startswith("1 problems: mean TV ")
    full = json.loads(out.read_text())
    assert set(full["meta"]["engines"]) == {"compiled", "python"}
    assert full["meta"]["engines"]["compiled"]["info"]["engine"] == "compiled"
    assert full["meta"]["engines"]["python"]["info"]["engine"] == "python"
    assert full["meta"]["engines"]["compiled"]["runs_per_problem"] == 6   # the null's second sample
    assert full["meta"]["engines"]["python"]["runs_per_problem"] == 3
    [row] = full["rows"]
    assert row["problem"] == "surround-01" and row["runs"] == 3
    assert sum(row["compiled"].values()) == sum(row["python"].values()) == 3
    assert sum(row["compiled_second"].values()) == 3
    assert 0.0 <= row["tv"] <= 1.0 and 0.0 <= row["null_observed"] <= 1.0
    assert row["within"] is True and full["summary"]["beyond_tolerance"] == []
    compact = json.loads(rec.read_text())
    assert compact["engine"] == "python" and compact["runs"] == 3
    assert compact["problems"]["surround-01"]["counts"] == row["python"]


@compiled_only
def test_a_problem_beyond_the_tolerance_fails_the_tool():
    env = {k: v for k, v in os.environ.items() if k != "TABLETOP_ENGINE"}
    proc = run_tool("--only", "1", "--runs", "2", "--tolerance", "-1", env=env)
    assert proc.returncode == 1, proc.stderr
    assert "| **no** |" in proc.stdout


@compiled_only
def test_the_null_alone_runs_the_compiled_engine_only(tmp_path):
    out = tmp_path / "null.json"
    env = {k: v for k, v in os.environ.items() if k != "TABLETOP_ENGINE"}
    proc = run_tool("--only", "1", "--runs", "2", "--null-only", "--out", str(out), env=env)
    assert proc.returncode == 0, proc.stderr
    assert proc.stdout.splitlines()[0] == "engine: compiled"
    full = json.loads(out.read_text())
    assert list(full["meta"]["engines"]) == ["compiled"]
    assert "tv" not in full["rows"][0] and "tv_mean" not in full["summary"]


def test_a_misnamed_engine_stops_the_tool():
    """No fallback (D-107): the pure worker started with a junk value fails at start, and the
    tool relays the error instead of carrying on."""
    env = dict(os.environ, TABLETOP_ENGINE="python")
    # the tool unsets the variable for the compiled worker and sets `python` for the pure
    # one, whatever the caller's environment: so a junk value cannot reach a worker; check
    # instead that a worker asked for the wrong engine refuses
    proc = subprocess.run([sys.executable, "-I", str(TOOL), "--worker", "compiled", "--only",
                           "1", "--runs", "1"], capture_output=True, text=True,
                          cwd=str(PYTHON_DIR.parent), env=env)
    assert proc.returncode != 0
    assert "worker for the compiled engine got the python engine" in proc.stderr

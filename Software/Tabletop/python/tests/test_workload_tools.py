"""The timing tools (D-107): `tools/workload.py` runs the families workload on the package
of a given folder and prints one JSON record with a digest of the rows; `tools/paired_timing.py`
runs a candidate beside the baseline on two cores and reports the medians. Both are tested
small (a problem or two, one run each) and in fresh processes, as they are used.
"""

import importlib.util
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

PYTHON_DIR = Path(__file__).resolve().parents[1]
TOOLS = PYTHON_DIR / "tools"
WORKLOAD = TOOLS / "workload.py"
PAIRED = TOOLS / "paired_timing.py"
KEYS = {"interpreter", "engine_dir", "engine", "seam", "profile", "set", "seed", "problems",
        "runs", "count", "codelets", "cpu", "wall", "maxrss_mb", "load1", "date", "digest"}


def load(name: str):
    spec = importlib.util.spec_from_file_location(name, TOOLS / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def workload(*args, hashseed="1", python=sys.executable) -> subprocess.CompletedProcess:
    env = dict(os.environ, PYTHONHASHSEED=hashseed)
    return subprocess.run([python, "-I", str(WORKLOAD), *args], capture_output=True, text=True,
                          env=env, cwd=str(PYTHON_DIR.parent))


def record_of(proc: subprocess.CompletedProcess) -> dict:
    assert proc.returncode == 0, proc.stderr
    lines = proc.stdout.strip().splitlines()
    assert len(lines) == 1, proc.stdout
    return json.loads(lines[0])


def test_the_workload_prints_one_record_with_the_same_digest_in_any_process():
    a = record_of(workload("--only", "2", "--runs", "1", hashseed="1"))
    b = record_of(workload("--only", "2", "--runs", "1", hashseed="2"))
    assert KEYS <= set(a)
    assert a["engine"] in ("python", "compiled") and a["problems"] == 2 and a["runs"] == 1
    assert a["count"] == 2 and a["codelets"] > 0 and a["cpu"] >= 0 and a["wall"] > 0
    assert len(a["digest"]) == 64 and a["digest"] == b["digest"]
    assert a["engine_dir"] == str(PYTHON_DIR)


def test_a_changed_field_or_a_different_problem_set_changes_the_digest():
    plain = record_of(workload("--only", "2", "--runs", "1"))
    changed = record_of(workload("--only", "2", "--runs", "1", "--set", "slipnet_update_every=5"))
    assert changed["set"] == ["slipnet_update_every=5"] and changed["digest"] != plain["digest"]
    named = record_of(workload("--problems", "surround-05", "--runs", "1"))
    assert named["problems"] == 1 and named["digest"] != plain["digest"]
    bad = workload("--only", "1", "--runs", "1", "--set", "no_such_field=1")
    assert bad.returncode != 0 and "no_such_field" in bad.stderr


def test_the_workload_runs_the_package_of_the_folder_it_is_given(tmp_path):
    copy = tmp_path / "engine-copy"
    ignore = shutil.ignore_patterns("__pycache__", "gui")
    shutil.copytree(PYTHON_DIR / "tabletop", copy / "tabletop", ignore=ignore)
    shutil.copytree(PYTHON_DIR / "problems", copy / "problems")
    here = record_of(workload("--only", "1", "--runs", "1"))
    there = record_of(workload("--engine", str(copy), "--only", "1", "--runs", "1"))
    assert there["engine_dir"] == str(copy) and there["digest"] == here["digest"]
    missing = workload("--engine", str(tmp_path / "nowhere"), "--only", "1", "--runs", "1")
    assert missing.returncode != 0 and "no tabletop package" in missing.stderr


def test_paired_timing_parses_its_specs_and_cores():
    pt = load("paired_timing")
    spec = pt.parse_spec("python3:/some/dir@TABLETOP_ENGINE=python@X=1")
    assert spec["python"] == "python3" and spec["engine"].endswith("/some/dir")
    assert spec["env"] == {"TABLETOP_ENGINE": "python", "X": "1"}
    assert pt.parse_cores(" 2, 3") == ("2", "3")
    with pytest.raises(SystemExit):
        pt.parse_spec("python3")
    with pytest.raises(SystemExit):
        pt.parse_cores("2")


def test_paired_timing_pairs_a_candidate_with_the_baseline_and_swaps_the_cores(tmp_path):
    out = tmp_path / "paired.json"
    me = f"{sys.executable}:{PYTHON_DIR}"
    proc = subprocess.run([sys.executable, str(PAIRED), "--baseline", me, "--candidate",
                           f"same={me}", "--cores", "0,1", "--only", "1", "--runs", "1",
                           "--repeats", "2", "--no-pin", "--out", str(out)],
                          capture_output=True, text=True, cwd=str(PYTHON_DIR.parent))
    assert proc.returncode == 0, proc.stderr
    record = json.loads(out.read_text())
    assert record["runs"] == 1 and record["repeats"] == 2 and record["pinned"] is False
    (d,) = record["pairs"]
    assert d["name"] == "same" and d["all_identical"] and len(d["rows"]) == 2
    assert [r["cores"] for r in d["rows"]] == [["0", "1"], ["1", "0"]]
    assert d["cpu_range"][0] <= d["cpu_median"] <= d["cpu_range"][1]
    assert proc.stdout.strip().splitlines()[-1].startswith("same ")
    assert "identical=True" in proc.stdout


def test_paired_timing_refuses_a_malformed_candidate():
    proc = subprocess.run([sys.executable, str(PAIRED), "--baseline", f"python3:{PYTHON_DIR}",
                           "--candidate", f"python3:{PYTHON_DIR}", "--cores", "0,1", "--no-pin"],
                          capture_output=True, text=True)
    assert proc.returncode == 2 and "NAME=PYTHON:DIR" in proc.stderr

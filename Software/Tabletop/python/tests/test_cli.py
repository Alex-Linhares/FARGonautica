"""The command line, `python3 -m tabletop`, through `subprocess` (item 21; D-064 to D-066)."""

import json
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel

from tabletop import __main__ as CLI
from tabletop.config import PROFILES, Config
from tabletop.problems import catalogue
from tabletop.run import fingerprint

PYTHON_DIR = Path(__file__).resolve().parent.parent


def run_cli(*args, timeout=120):
    return subprocess.run([sys.executable, "-m", "tabletop", *args], cwd=PYTHON_DIR,
                          capture_output=True, text=True, timeout=timeout)


def test_list_names_every_problem():
    proc = run_cli("--list")
    assert proc.returncode == 0
    lines = proc.stdout.splitlines()
    assert [l.split()[0] for l in lines] == catalogue()
    row = next(l for l in lines if l.startswith("surround-05 "))
    assert "13 objects" in row and "touched cup1" in row and "E1=small-glass1" in row
    assert "Surround 5, p. 118" in row
    ch2 = next(l for l in lines if l.startswith("ch2-fig11 "))
    assert "expected=" in ch2


def test_a_quiet_run_prints_the_first_and_last_lines():
    proc = run_cli("surround-05", "--seed", "3", "--quiet")
    assert proc.returncode == 0 and proc.stderr == ""
    lines = proc.stdout.splitlines()
    assert lines[0] == ("tabletop surround-05  seed 3  profile standard  engine "
                        f"{accel.info()['engine']}  Henry touches cup1")
    assert len(lines) == 2 and lines[1].startswith("Eliza touches ") and lines[1].endswith(".")


def test_a_full_run_prints_one_line_per_codelet_and_is_reproducible():
    a = run_cli("surround-05", "--seed", "3")
    b = run_cli("surround-05", "--seed", "3")
    assert a.returncode == 0 and a.stdout == b.stdout
    lines = a.stdout.splitlines()
    assert lines[1].startswith("   1  T=81.9  a=1.5  ") and "->" in lines[1]   # v4 (D-115; T=70.7 a=2.1 in v3)
    codelets = int(lines[-1].split("Codelets ")[1].split(".")[0].split()[0])
    assert sum(l[:4].strip().isdigit() for l in lines) == codelets
    assert run_cli("surround-05", "--seed", "4").stdout != a.stdout


def test_the_default_seed_is_one():
    assert run_cli("surround-05", "--quiet").stdout == run_cli("surround-05", "--seed", "1", "--quiet").stdout


def test_json_to_stdout_carries_the_trace_alone(tmp_path):
    proc = run_cli("surround-05", "--seed", "3", "--json", "-")
    assert proc.returncode == 0
    records = [json.loads(l) for l in proc.stdout.splitlines()]
    assert records[0]["event"] == "RunStarted" and records[-1]["event"] == "RunEnded"
    assert all(set(list(r)[:3]) == {"event", "clock", "draws"} for r in records)
    # The file form holds the same lines, beside the text trace on stdout.
    target = tmp_path / "run.jsonl"
    proc2 = run_cli("surround-05", "--seed", "3", "--json", str(target), "--quiet")
    assert proc2.returncode == 0 and target.read_text() == proc.stdout
    assert proc2.stdout.splitlines()[0].startswith("tabletop surround-05")


def test_the_json_trace_hashes_to_the_fingerprint():
    import hashlib
    proc = run_cli("surround-05", "--seed", "3", "--json", "-")
    digest = hashlib.sha256(proc.stdout.encode("utf-8")).hexdigest()
    assert digest == fingerprint("surround-05", 3)[0]


def test_profile_and_set_change_the_run():
    standard = run_cli("surround-05", "--seed", "3", "--quiet").stdout
    faults = run_cli("surround-05", "--seed", "3", "--quiet", "--profile", "french-faults")
    assert faults.returncode == 0 and "profile french-faults" in faults.stdout.splitlines()[0]
    assert faults.stdout != standard
    changed = run_cli("surround-05", "--seed", "3", "--quiet", "--set", "rho=2",
                      "--set", "initial_temperature=90")
    assert changed.returncode == 0
    head = changed.stdout.splitlines()[0]
    assert "rho=2.0" in head and "initial_temperature=90" in head
    assert changed.stdout != standard


@pytest.mark.parametrize("setting, message", [
    ("rhoo=2", "no Config field"),
    ("rho", "FIELD=VALUE"),
    ("rho=abc", "wants a float"),
    ("profile=x", "no Config field"),
    ("keep_all_subgroups=1", "wants a bool"),
])
def test_bad_settings_are_usage_errors(setting, message):
    proc = run_cli("surround-05", "--set", setting)
    assert proc.returncode == 2 and message in proc.stderr and proc.stdout == ""


def test_configure_applies_settings_without_touching_the_profiles():
    config = CLI.configure("standard", ["rho=2", "keep_all_subgroups=True"])
    assert config.rho == 2.0 and config.keep_all_subgroups is True
    assert PROFILES["standard"].rho == Config().rho != 2.0
    assert not PROFILES["standard"].keep_all_subgroups
    assert CLI.configure("buridan", []) is PROFILES["buridan"]
    assert CLI.parse_value("(1, 2)") == (1, 2) and CLI.parse_value("spoon") == "spoon"


def test_unknown_problem_and_no_problem_are_usage_errors():
    proc = run_cli("nowhere-99")
    assert proc.returncode == 2 and "cannot load" in proc.stderr
    proc = run_cli()
    assert proc.returncode == 2 and "usage" in proc.stderr


def test_batch_prints_a_summary_table(tmp_path):
    proc = run_cli("surround-05", "--batch", "4", "--seed", "1")
    assert proc.returncode == 0
    lines = proc.stdout.splitlines()
    assert lines[0] == f"engine: {accel.info()['engine']}"      # every output names its engine
    lines = lines[1:]
    assert lines[0].split() == ["answer", "label", "runs", "%", "structure", "codelets"]
    assert lines[-1].startswith("4 runs of surround-05, ")
    rows = lines[1:-1]
    assert sum(int(r.split()[2]) for r in rows) == 4
    assert abs(sum(float(r.split()[3]) for r in rows) - 100.0) < 0.2
    target = tmp_path / "batch.jsonl"
    proc2 = run_cli("surround-05", "--batch", "4", "--seed", "1", "--json", str(target))
    results = [json.loads(l) for l in target.read_text().splitlines()]
    assert len(results) == 4 and {r["problem"] for r in results} == {"surround-05"}
    assert proc2.stdout == proc.stdout
    assert len(set(r["seed"] for r in results)) == 4
    assert run_cli("surround-05", "--batch", "0").returncode == 2


def test_table_prints_the_layout_before_the_run():
    proc = run_cli("surround-05", "--seed", "3", "--table", "--quiet")
    assert proc.returncode == 0
    lines = proc.stdout.splitlines()
    assert lines[0].strip() == "Henry" and "Eliza" in lines[23]
    assert sum(l.startswith("|") for l in lines) == 20
    legend = [l for l in lines if l.strip().startswith(tuple("abcdefghijklm")) and "(" in l]
    assert len(legend) == 13
    assert any("cup1" in l and "*touched" in l and "upper-left" in l for l in legend)
    assert any("small-glass1" in l and " E1" in l for l in legend)
    assert any("cup2" in l and " E2" in l and "lower-right" in l for l in legend)
    assert lines[-2].startswith("tabletop surround-05") and lines[-1].startswith("Eliza touches")


def test_help_names_every_option():
    proc = run_cli("--help")
    assert proc.returncode == 0
    for option in ("--list", "--seed", "--quiet", "--json", "--profile", "--set", "--batch",
                   "--table"):
        assert option in proc.stdout

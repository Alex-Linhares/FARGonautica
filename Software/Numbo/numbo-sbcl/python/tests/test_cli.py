"""Loop0002 item 13: the command line, python -m numbo (numbo/__main__.py).

    python -m numbo TARGET B1 B2 B3 B4 B5 [--seed N] [--max-iterations N|none]
                    [--verbose] [--trace FILE] [--rng-events] [--quiet]

It runs harness.run_config (always oracle mode: the shared RNG) and prints
what config prints, then two summary lines:

    outcome: solved, 45 iterations (seed 1)
    check: valid: 114 = (6 x 20) - (7 - 1)

("check: invalid: <reason>" when solution_checker rejects the output, and
"outcome: error, ... : <message>" for a Lisp error).  The exit status is 0
for a valid solution and 1 otherwise; usage errors exit with 2.

The run's text and trace are compared with the oracle's (main_loop.json, made
by tests/oracle/main-loop.lisp); the outcomes of the other runs are the
oracle's (main_loop.json, and the fresh-process runs of test_full_runs.py).
"""

import json
import os
import subprocess
import sys

import pytest

from conftest import load_fixture
from full_runs import PYTHON_DIR

RUNS = load_fixture("main_loop.json")["runs"]


def cli(*args, cwd=PYTHON_DIR):
    env = {k: v for k, v in os.environ.items() if k not in ("WINDOW_GFX", "PYTHONPATH")}
    return subprocess.run([sys.executable, "-m", "numbo", *map(str, args)], cwd=cwd,
                          capture_output=True, text=True, timeout=300, env=env)


def oracle_run(problem, seed, rng_events=False):
    return next(r for r in RUNS if r["problem"] == problem and r["seed"] == seed
                and r["rng-events"] == rng_events)


def test_solved_run_prints_the_oracle_text():
    proc = cli(114, 11, 20, 7, 1, 6, "--seed", 1)
    expected = oracle_run([114, 11, 20, 7, 1, 6], 1)["output"]
    assert proc.returncode == 0, proc.stderr
    assert proc.stderr == ""
    assert proc.stdout == (expected + "outcome: solved, 45 iterations (seed 1)\n"
                           "check: valid: 114 = (6 x 20) - (7 - 1)\n")


def test_quiet():
    proc = cli(6, 3, 3, 17, 11, 22, "--seed", 1, "--quiet")
    assert proc.returncode == 0
    assert proc.stdout == "outcome: solved, 30 iterations (seed 1)\ncheck: valid: 6 = 3 + 3\n"


def test_default_seed_is_1():
    assert cli(6, 3, 3, 17, 11, 22, "--quiet").stdout == cli(
        6, 3, 3, 17, 11, 22, "--quiet", "--seed", 1).stdout


@pytest.mark.parametrize("rng_events", [False, True])
def test_trace_file(tmp_path, rng_events):
    path = tmp_path / "trace.jsonl"
    args = [114, 11, 20, 7, 1, 6, "--seed", 1, "--quiet", "--trace", path]
    if rng_events:
        args.append("--rng-events")
    assert cli(*args).returncode == 0
    events = [json.loads(line) for line in path.read_text().splitlines()]
    expected = oracle_run([114, 11, 20, 7, 1, 6], 1, rng_events)["trace"]
    assert [json.dumps(e) for e in events] == [json.dumps(e) for e in expected]


def test_gave_up():
    proc = cli(146, 12, 2, 5, 7, 18, "--seed", 18, "--quiet")
    assert proc.returncode == 1
    assert proc.stdout == ('outcome: gave-up, 520 iterations (seed 18)\n'
                           'check: invalid: no "Done :" in the output\n')


def test_capped():
    proc = cli(31, 3, 5, 24, 3, 14, "--seed", 1, "--max-iterations", 450)
    expected = oracle_run([31, 3, 5, 24, 3, 14], 1, True)["output"]
    assert proc.returncode == 1
    assert proc.stdout == expected + ('outcome: capped, 450 iterations (seed 1)\n'
                                      'check: invalid: no "Done :" in the output\n')


def test_kill_block_gap_is_reported():
    proc = cli(31, 3, 5, 24, 3, 14, "--seed", 8, "--quiet")
    assert proc.returncode == 1
    assert proc.stdout == (
        "outcome: solved, 225 iterations (seed 8)\n"
        "check: invalid: CYTO-BLOCK11-V5 (11) is used but never derived, and is not a brick\n")


def test_error_run():
    """The reactivate-cyto race, oracle seed 40 (test_full_runs.py)."""
    proc = cli(114, 11, 20, 7, 1, 6, "--seed", 40, "--quiet")
    assert proc.returncode == 1
    # As oracle-run-config: an error's iteration count comes from the trace,
    # so the CLI always traces (to nowhere when --trace is not given).
    assert proc.stdout == (
        "outcome: error, 29 iterations (seed 40): "
        "SEND: NIL does not handle the message :SET-ACTIVATION\n"
        'check: invalid: no "Done :" in the output\n')


def test_no_cap():
    proc = cli(6, 3, 3, 17, 11, 22, "--max-iterations", "none", "--quiet")
    assert proc.returncode == 0
    assert proc.stdout.startswith("outcome: solved, 30 iterations")


def test_verbose():
    quiet = cli(6, 3, 3, 17, 11, 22).stdout
    verbose = cli(6, 3, 3, 17, 11, 22, "--verbose").stdout
    assert "About to post codelet" in verbose and "About to post codelet" not in quiet


@pytest.mark.parametrize("args", [
    [114, 11, 20, 7, 1],
    [114, 11, 20, 7, 1, 6, 9],
    [114, 11, 20, 7, 1, "six"],
    [114, 11, 20, 7, 1, 6, "--max-iterations", 0],
    [114, 11, 20, 7, 1, 6, "--seed", "x"],
])
def test_usage_errors(args):
    proc = cli(*args)
    assert proc.returncode == 2
    assert proc.stdout == ""
    assert "usage:" in proc.stderr


def test_help():
    proc = cli("--help")
    assert proc.returncode == 0
    assert "TARGET" in proc.stdout and "--seed" in proc.stdout


def test_runs_from_the_repo_root_with_pythonpath(tmp_path):
    env = {k: v for k, v in os.environ.items() if k != "WINDOW_GFX"}
    env["PYTHONPATH"] = str(PYTHON_DIR)
    proc = subprocess.run([sys.executable, "-m", "numbo", "6", "3", "3", "17", "11", "22",
                           "--quiet"], cwd=tmp_path, capture_output=True, text=True,
                          timeout=300, env=env)
    assert proc.returncode == 0, proc.stderr

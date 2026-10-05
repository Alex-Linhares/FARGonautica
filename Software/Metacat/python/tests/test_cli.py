"""python3 -m metacat against the live oracle, chez_scheme/oracle/run.ss (loop0002
item 11).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The counterpart of racket/tests/cli-test.rkt, with the same cases: the CLI's
stdout and exit code must equal the oracle's, byte for byte, on an answer, no
cap, a cap, a justify run, keep-going, verbose mode, the halt run
(report-error-and-halt), the crash run (abc ccbbaa ijk seed 3) and bad
arguments; --trace writes the golden; a run without --seed prints a seed that
replays it in the oracle.  The comparisons run the two programs in parallel.
The usage errors are also checked without the oracle in the fast tier.
"""
from __future__ import annotations

import os
import re
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

import pytest

PYTHON_DIR = Path(__file__).resolve().parent.parent
ROOT = PYTHON_DIR.parent
GOLDEN = ROOT / "tests" / "golden"

CASES = {
    "answer": ["abc", "abd", "xyz", "--seed", "3", "--max-codelets", "10000"],
    "no-cap": ["abc", "abd", "xyz", "--seed", "3"],
    "cap": ["abc", "abd", "ijk", "--seed", "2", "--max-codelets", "300"],
    "justify": ["abc", "abd", "mrrjjj", "mrrjjjj", "--seed", "1", "--max-codelets", "10000"],
    "keep-going": ["a", "b", "z", "--seed", "1", "--max-codelets", "1000", "--keep-going"],
    "halt": ["eqe", "qeq", "abbba", "aaabaaa", "--seed", "3", "--max-codelets", "17000"],
    "verbose": ["a", "b", "z", "--seed", "1", "--max-codelets", "1000", "--keep-going",
                "--verbose"],
    "crash": ["abc", "ccbbaa", "ijk", "--seed", "3", "--max-codelets", "10000"],
}
BAD_ARGS = [[], ["abc", "abd"], ["abc", "abd", "xyz", "--seed"],
            ["abc", "abd", "xyz", "--seed", "0"], ["abc", "abd", "xyz", "--seed", "x"],
            ["abc", "abd", "xyz", "--max-codelets", "-3"], ["abc", "abd", "xyz", "--bogus"],
            ["abc", "abd", "xyz", "--seed", "4294967296"], ["a", "b", "c", "d", "e"],
            ["abc", "abd", "xyz", "--seed", "1/2"], ["abc", "abd", "1xyz"],
            ["abc", "abd", "xyz", "--trace"]]


def port_run(args):
    """(exit code, stdout, stderr) of python3 -m metacat"""
    p = subprocess.run([sys.executable, "-m", "metacat", *args], cwd=PYTHON_DIR,
                       capture_output=True, text=True, stdin=subprocess.DEVNULL, timeout=900)
    return p.returncode, p.stdout, p.stderr


def oracle_run(args):
    scheme = shutil.which("scheme") or shutil.which("chezscheme")
    p = subprocess.run([scheme, "--script", "chez_scheme/oracle/run.ss", *args], cwd=ROOT,
                       capture_output=True, text=True, stdin=subprocess.DEVNULL, timeout=900)
    return p.returncode, p.stdout, p.stderr


@pytest.fixture(scope="module")
def runs():
    """Every case and bad argument list, in the port and in the oracle, plus the
    --trace run and the clock-seeded run."""
    tmp = tempfile.mkdtemp()
    trace_file = os.path.join(tmp, "trace.jsonl")
    verbose_trace = os.path.join(tmp, "verbose.jsonl")
    jobs = {}
    for name, args in CASES.items():
        jobs[("port", name)] = (port_run, args)
        jobs[("oracle", name)] = (oracle_run, args)
    for i, args in enumerate(BAD_ARGS):
        jobs[("port", "bad", i)] = (port_run, args)
        jobs[("oracle", "bad", i)] = (oracle_run, args)
    traced = ["abc", "abd", "xyz", "--seed", "3852097033", "--max-codelets", "10000"]
    jobs[("port", "trace")] = (port_run, traced + ["--trace", trace_file])
    jobs[("oracle", "trace")] = (oracle_run, traced)
    vtraced = ["abc", "abd", "xyz", "--seed", "3009318743", "--max-codelets", "10000",
               "--verbose"]
    jobs[("port", "verbose-trace")] = (port_run, vtraced + ["--trace", verbose_trace])
    jobs[("oracle", "verbose-trace")] = (oracle_run, vtraced)
    jobs[("port", "clock")] = (port_run, ["abc", "abd", "xyz", "--max-codelets", "500"])
    with ThreadPoolExecutor(32) as pool:
        futures = {k: pool.submit(f, a) for k, (f, a) in jobs.items()}
        results = {k: f.result() for k, f in futures.items()}
    with open(trace_file) as f:
        results["trace-file"] = f.read()
    with open(verbose_trace) as f:
        results["verbose-trace-file"] = f.read()
    m = re.match(r"Problem: abc -> abd; xyz -> \?  seed ([0-9]+)\n", results[("port", "clock")][1])
    assert m, results[("port", "clock")]
    results["clock-seed"] = m.group(1)
    results[("oracle", "clock")] = oracle_run(["abc", "abd", "xyz", "--seed", m.group(1),
                                               "--max-codelets", "500"])
    shutil.rmtree(tmp)
    return results


def same_as_oracle(runs, key):
    p, o = runs[("port",) + key], runs[("oracle",) + key]
    assert p[0] == o[0], (key, p[2][-2000:])
    assert p[1] == o[1], key
    return p, o


@pytest.mark.slow
@pytest.mark.parametrize("name", [n for n in CASES if n != "crash"])
def test_same_output_and_exit_code_as_the_oracle(runs, name):
    p, o = same_as_oracle(runs, (name,))
    assert p[0] == 0
    assert p[2] == ""


@pytest.mark.slow
def test_what_the_cases_reach(runs):
    out = {name: runs[("port", name)][1] for name in CASES}
    assert "\nAnswer: yyz  quality 88  codelet 2427" in out["answer"]
    assert "\nCodelets run: 300\nStopped: cap\n" in out["cap"]
    assert out["justify"].startswith('Problem: abc -> abd; mrrjjj -> mrrjjjj  seed 1\n'
                                     'Comment: Let\'s see... "abc" changes to "abd", and')
    assert "\nStopped: cap\n" in out["keep-going"]
    assert re.search(r"\nOoops: bad message .*\nStopped: halt\n", out["halt"])
    assert len(out["verbose"].split("\n")) > 5000
    assert re.search(r"\n[(]<[a-z]+> <[a-z]+>[)] entry: overlap = ", out["verbose"])


@pytest.mark.slow
def test_the_crash_run(runs):
    """The original raises caddr of #f on abc ccbbaa ijk seed 3: both exit 1, with
    the same output up to the crash and the same first line of the error."""
    p, o = same_as_oracle(runs, ("crash",))
    assert p[0] == 1
    assert p[2].splitlines()[0] == o[2].splitlines()[0] == \
        "Error: Exception in caddr: incorrect list structure #f"


@pytest.mark.slow
@pytest.mark.parametrize("i", range(len(BAD_ARGS)))
def test_bad_arguments_as_the_oracle(runs, i):
    p, o = same_as_oracle(runs, ("bad", i))
    assert p[0] == 2
    assert p[1] == ""
    assert p[2] != ""


@pytest.mark.slow
def test_trace_writes_the_golden(runs):
    assert runs[("port", "trace")][0] == 0
    assert runs["trace-file"] == (GOLDEN / "abc-abd-xyz_3852097033.jsonl").read_text()
    assert runs[("port", "trace")][1] == runs[("oracle", "trace")][1]


@pytest.mark.slow
def test_verbose_does_not_change_the_trace(runs):
    same_as_oracle(runs, ("verbose-trace",))
    assert "entry: overlap = " in runs[("port", "verbose-trace")][1]
    assert runs["verbose-trace-file"] == (GOLDEN / "abc-abd-xyz_3009318743.jsonl").read_text()


@pytest.mark.slow
def test_a_clock_seed_replays_in_the_oracle(runs):
    assert int(runs["clock-seed"]) > 0
    assert runs[("port", "clock")][0] == 0
    assert runs[("port", "clock")][1] == runs[("oracle", "clock")][1]


@pytest.mark.parametrize("args", BAD_ARGS[:6])
def test_usage_errors(args):
    code, out, err = port_run(args)
    assert (code, out) == (2, "")
    assert err.startswith(("usage: ", "metacat: the seed"))

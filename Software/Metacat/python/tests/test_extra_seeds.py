"""The 720 extra-seed runs, Python against the oracle (loop0002 item 12).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

tests/extra-seeds.py audited the Racket port on 20 non-golden seeds per problem
line of tests/problems.txt.  python/oracle/capture_extra_seeds.py ran the same 720
runs in the unedited oracle and froze, per run, the exit code, the whole stdout,
stderr's first line and the trace's sha256 and line count
(python/fixtures/extra-seeds/runs.jsonl).  Here every run goes through the
package's headless driver, as `python3 -m metacat ... --trace FILE` runs it, each
in a fresh fork of a fresh process, and must give the same five things: the
trace byte for byte (by its hash), the output byte for byte, the same exit code.

Slow tier: all 720 runs (about 2 min on 32 cores), and the oracle's 720 again,
whose capture must be byte-identical (about 1.5 min).  Fast tier: the sources of
the fixture and the job list.  A failure names the run; to see where the traces
part, run both with --trace (the command is in the message) and diff them.
"""
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import pytest

import capture_extra_seeds as cx
import golden_harness as g

FIXTURE = Path(g.ROOT) / "python" / "fixtures" / "extra-seeds"


def oracle_runs():
    with open(FIXTURE / "runs.jsonl") as f:
        return [json.loads(line) for line in f]


def job_of(r):
    return (r["strings"], r["seed"], r["cap"], r["keep"])


def test_sources_unchanged():
    """No input of the oracle capture changed since the fixture was made."""
    recorded = (FIXTURE / "SOURCES").read_text()
    assert recorded.splitlines()[0] == "chez " + cx.capture.chez_version()
    assert cx.sources() == recorded


def test_the_jobs():
    """The fixture holds tests/extra-seeds.py's 720 runs, none of them a golden."""
    runs = oracle_runs()
    assert [job_of(r) for r in runs] == [tuple(j) for j in cx.jobs(20)]
    assert len(runs) == 720 == 20 * len(cx.problem_lines())
    golden = {(tuple(s), seed) for _, s, seed, _, _ in g.golden_runs()}
    assert not any((tuple(r["strings"]), r["seed"]) in golden for r in runs)
    assert all(r["trace_lines"] > 0 for r in runs)


def test_what_the_runs_reach():
    """The 720 runs end in each of the ways a golden run can, but crash."""
    out = [r["stdout"] for r in oracle_runs()]
    assert sum("Stopped: suspend" in o for o in out) > 600
    assert sum("Stopped: cap" in o for o in out) > 50
    assert sum("Stopped: halt" in o for o in out) >= 1


@pytest.fixture(scope="module")
def python_results():
    runs = oracle_runs()
    # the longest caps first, so that the pool ends evenly
    order = sorted(range(len(runs)), key=lambda i: -runs[i]["cap"])
    results = g.run_in_fresh_process([job_of(runs[i]) for i in order], digest=True)
    return {i: res for i, res in zip(order, results)}


@pytest.mark.slow
def test_every_extra_seed_run_matches_the_oracle(python_results):
    bad = []
    for i, r in enumerate(oracle_runs()):
        code, stdout, error, sha, lines = python_results[i]
        problems = []
        if code != r["exit"]:
            problems.append("exit code %s (oracle %s)" % (code, r["exit"]))
        if (sha, lines) != (r["trace_sha256"], r["trace_lines"]):
            problems.append("trace differs (%d lines; oracle %d)" % (lines, r["trace_lines"]))
        if stdout != r["stdout"]:
            o, p = r["stdout"].splitlines(), stdout.splitlines()
            k = next((k for k, (x, y) in enumerate(zip(o, p)) if x != y), min(len(o), len(p)))
            problems.append("stdout differs at line %d: oracle %r, python %r"
                            % (k + 1, o[k] if k < len(o) else None, p[k] if k < len(p) else None))
        if r["exit"] != 0 and error != r["error"]:
            problems.append("error %r (oracle %r)" % (error, r["error"]))
        if problems:
            bad.append("python3 -m metacat %s: %s" % (" ".join(cx.cli_args(*job_of(r))),
                                                      "; ".join(problems)))
    assert not bad, "%d of 720 runs differ:\n%s" % (len(bad), "\n".join(bad[:20]))


@pytest.mark.slow
def test_recapture_is_byte_identical(tmp_path):
    """Freshness: running the 720 again in the oracle gives the same files."""
    subprocess.run([sys.executable, str(Path(cx.__file__)), "--out", str(tmp_path)],
                   check=True, capture_output=True)
    for name in ("runs.jsonl", "SOURCES"):
        assert (tmp_path / name).read_bytes() == (FIXTURE / name).read_bytes(), name

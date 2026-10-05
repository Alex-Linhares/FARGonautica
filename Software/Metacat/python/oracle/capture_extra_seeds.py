"""Capture the oracle's extra-seed runs (loop0002 item 12).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  python3 python/oracle/capture_extra_seeds.py [--seeds N] [--jobs J] [--out DIR]

The jobs are tests/extra-seeds.py's (loop0001's final audit of the Racket port):
for every problem line of tests/problems.txt, the same strings, cap and keep-going
flag, with N (default 20) non-golden seeds drawn from Python's
random.Random(20261003) in the same way, so that the 720 runs are the ones the
Racket port was audited on.  Each runs once in the unedited oracle,
`scheme --script chez_scheme/oracle/run.ss ... --trace FILE`, in a fresh process.

Writes DIR (default python/fixtures/extra-seeds/):
  runs.jsonl   one JSON object per run, in job order: strings, seed, cap,
               keep (keep-going?), exit (the exit code), stdout (all of it),
               error (stderr's first line, "" when stderr is empty),
               trace_sha256 and trace_lines (of the trace file, which is not kept:
               the 720 traces are about 300 MB)
  SOURCES      sha256 of every input of the capture, and Chez's version, for the
               freshness check in test_extra_seeds.py
Nothing here is hand-edited; a rerun writes the same files.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import random
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
sys.path.insert(0, str(HERE))

import capture  # noqa: E402

OUT = REPO / "python" / "fixtures" / "extra-seeds"
RNG_SEED = 20261003
SOURCE_FILES = ["chez_scheme/oracle/run.ss", "chez_scheme/oracle/trace.ss",
                "chez_scheme/oracle/prelude.ss", "tests/problems.txt",
                "tests/extra-seeds.py", "python/oracle/capture_extra_seeds.py"]


def problem_lines():
    """(strings, golden seeds, cap, keep-going?) for each line of problems.txt,
    as tests/extra-seeds.py reads them."""
    out = []
    for line in (REPO / "tests/problems.txt").read_text().splitlines():
        line = line.split("#", 1)[0]
        if not line.strip():
            continue
        fields = [f.split() for f in line.split("|")]
        out.append((fields[0], [int(s) for s in fields[1]], int(fields[2][0]),
                    len(fields) == 4 and fields[3] == ["keep-going"]))
    return out


def jobs(n_seeds=20):
    """tests/extra-seeds.py's jobs: (strings, seed, cap, keep-going?)."""
    rng = random.Random(RNG_SEED)
    lines = problem_lines()
    golden = {(tuple(s), seed) for s, seeds, _, _ in lines for seed in seeds}
    out = []
    for strings, _, cap, keep in lines:
        seeds = []
        while len(seeds) < n_seeds:
            seed = rng.randrange(1, 2**32)
            if (tuple(strings), seed) not in golden and seed not in seeds:
                seeds.append(seed)
        out.extend((strings, seed, cap, keep) for seed in seeds)
    return out


def cli_args(strings, seed, cap, keep):
    return list(strings) + ["--seed", str(seed), "--max-codelets", str(cap)] + (
        ["--keep-going"] if keep else [])


def sources():
    lines = ["chez " + capture.chez_version()]
    for f in SOURCE_FILES:
        lines.append("%s  %s" % (hashlib.sha256((REPO / f).read_bytes()).hexdigest(), f))
    return "\n".join(lines) + "\n"


def oracle_run(job, tmp):
    strings, seed, cap, keep = job
    trace = Path(tmp) / ("%s_%d.jsonl" % ("-".join(strings), seed))
    proc = subprocess.run([capture.scheme(), "--script", "chez_scheme/oracle/run.ss",
                           *cli_args(*job), "--trace", str(trace)],
                          cwd=REPO, capture_output=True, timeout=1200,
                          stdin=subprocess.DEVNULL)
    data = trace.read_bytes() if trace.exists() else b""
    trace.unlink(missing_ok=True)
    err = proc.stderr.decode()
    return {"strings": strings, "seed": seed, "cap": cap, "keep": keep,
            "exit": proc.returncode, "stdout": proc.stdout.decode(),
            "error": err.splitlines()[0] if err else "",
            "trace_sha256": hashlib.sha256(data).hexdigest(),
            "trace_lines": data.count(b"\n")}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seeds", type=int, default=20)
    ap.add_argument("--jobs", type=int, default=os.cpu_count() or 4)
    ap.add_argument("--out", default=str(OUT))
    args = ap.parse_args()
    todo = jobs(args.seeds)
    with tempfile.TemporaryDirectory(prefix="metacat-extra-") as tmp, \
            ThreadPoolExecutor(max_workers=args.jobs) as pool:
        # the longest problems first, so that the pool ends evenly
        order = sorted(range(len(todo)), key=lambda i: -todo[i][2])
        done = dict(zip(order, pool.map(lambda i: oracle_run(todo[i], tmp), order)))
    results = [done[i] for i in range(len(todo))]
    for r in results:
        if not r["trace_lines"]:
            raise SystemExit("empty oracle trace: %s" % (cli_args(r["strings"], r["seed"],
                                                                    r["cap"], r["keep"]),))
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    with open(out / "runs.jsonl", "w", newline="\n") as f:
        for r in results:
            f.write(json.dumps(r, sort_keys=True) + "\n")
    (out / "SOURCES").write_text(sources())
    print("%d runs: %d exit 0, %d crashed" % (
        len(results), sum(r["exit"] == 0 for r in results),
        sum(r["exit"] != 0 for r in results)))


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Extra-seed equivalence runs (loop0001 item 17, the final audit).

    python3 tests/extra-seeds.py [--seeds N] [--jobs J] [--keep DIR]

Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).

For every problem line of tests/problems.txt (same strings, cap and
keep-going flag), runs N extra seeds (default 20) that are not golden seeds,
in both the oracle (chez_scheme/oracle/run.ss --trace) and the port
(racket/cli.rkt --trace), each in a fresh process, and compares the two
traces byte for byte, the printed output byte for byte and the exit codes.
The seeds are drawn from a fixed Python generator, so a rerun repeats the
same runs.  Nothing is written to tests/golden/: these traces are compared
with each other, not kept.  Not part of tests/run-tests.sh (it takes a few
minutes on 32 cores); its results are in docs/extra-seeds.md.

Exit 0 when every run agrees.
"""
from __future__ import annotations

import argparse
import os
import random
import shutil
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
SCHEME = shutil.which("scheme") or shutil.which("chezscheme")


def problem_lines():
    """(strings, golden seeds, cap, keep-going?) for each line of problems.txt"""
    out = []
    for line in (REPO / "tests/problems.txt").read_text().splitlines():
        line = line.split("#", 1)[0]
        if not line.strip():
            continue
        fields = [f.split() for f in line.split("|")]
        out.append((fields[0], [int(s) for s in fields[1]], int(fields[2][0]),
                    len(fields) == 4 and fields[3] == ["keep-going"]))
    return out


def run(cmd, trace):
    proc = subprocess.run(cmd + ["--trace", str(trace)], cwd=REPO,
                          capture_output=True, timeout=600)
    data = trace.read_bytes() if trace.exists() else b""
    return proc.returncode, proc.stdout, proc.stderr, data


def first_diff(a: bytes, b: bytes):
    la, lb = a.split(b"\n"), b.split(b"\n")
    for i, (x, y) in enumerate(zip(la, lb)):
        if x != y:
            return i + 1, x[:300], y[:300]
    return min(len(la), len(lb)) + 1, b"<end>", b"<end>"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seeds", type=int, default=20)
    ap.add_argument("--jobs", type=int, default=os.cpu_count() or 4)
    ap.add_argument("--keep", help="directory to keep the traces in")
    args = ap.parse_args()

    # compile the port once, so that parallel racket processes don't race
    subprocess.run(["raco", "make", "racket/cli.rkt"], cwd=REPO, check=True)

    rng = random.Random(20261003)
    lines = problem_lines()
    golden = {(tuple(s), seed) for s, seeds, _, _ in lines for seed in seeds}
    jobs = []
    for strings, _, cap, keep in lines:
        seeds = []
        while len(seeds) < args.seeds:
            seed = rng.randrange(1, 2**32)
            if (tuple(strings), seed) not in golden and seed not in seeds:
                seeds.append(seed)
        for seed in seeds:
            jobs.append((strings, seed, cap, keep))

    tmp = Path(args.keep) if args.keep else Path(tempfile.mkdtemp(prefix="metacat-extra-"))
    tmp.mkdir(parents=True, exist_ok=True)

    def one(job):
        strings, seed, cap, keep = job
        common = strings + ["--seed", str(seed), "--max-codelets", str(cap)]
        if keep:
            common.append("--keep-going")
        name = f"{'-'.join(strings)}_{seed}"
        chez = run([SCHEME, "--script", "chez_scheme/oracle/run.ss"] + common,
                   tmp / f"{name}.chez.jsonl")
        rkt = run(["racket", "racket/cli.rkt"] + common, tmp / f"{name}.rkt.jsonl")
        return job, chez, rkt

    with ThreadPoolExecutor(max_workers=max(1, args.jobs // 2)) as pool:
        results = list(pool.map(one, jobs))

    bad = 0
    stats = {"codelets": 0, "answers": 0, "halt": 0, "crash": 0, "cap": 0, "suspend": 0}
    for (strings, seed, cap, keep), chez, rkt in results:
        problems = []
        if chez[0] != rkt[0]:
            problems.append(f"exit codes {chez[0]} (oracle) vs {rkt[0]} (port)")
        if chez[1] != rkt[1]:
            problems.append("stdout differs at line %d:\n    oracle: %r\n    port:   %r"
                            % first_diff(chez[1], rkt[1]))
        if chez[3] != rkt[3]:
            problems.append("trace differs at line %d:\n    oracle: %r\n    port:   %r"
                            % first_diff(chez[3], rkt[3]))
        if not chez[3]:
            problems.append("empty oracle trace")
        if problems:
            bad += 1
            print(f"DIFFERS: {' '.join(strings)} --seed {seed}")
            for p in problems:
                print("  " + p)
        out = chez[1].decode()
        stats["answers"] += out.count("\nAnswer:") + out.startswith("Answer:")
        if chez[0] != 0:
            stats["crash"] += 1
        for kind in ("halt", "cap", "suspend"):
            if f"Stopped: {kind}" in out:
                stats[kind] += 1
        stats["codelets"] += chez[3].count(b'"ev":"codelet"')

    print(f"extra seeds: {len(results)} runs over {len(lines)} problem lines, "
          f"{len(results) - bad} identical, {bad} differ")
    print("oracle runs: " + ", ".join(f"{k} {v}" for k, v in stats.items()))
    if not args.keep:
        shutil.rmtree(tmp)
    sys.exit(1 if bad else 0)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""Perl vs Python end-to-end comparison over seeds 1..N (diagnostics; writes no golden).

Runs oracle/e2e_run.perl and seqsee.testing.e2e.run_one for every sequence of
config/sequence_list_for_testing x seeds 1..N (MAX_STEPS as in item 000) and prints, per
sequence, the success counts, the median steps of successful runs and the status counts.

Usage: python3 oracle/e2e_compare.py [--seeds N] [--max-steps N] [--jobs N] [--out FILE]
"""
import argparse
import collections
import json
import os
import statistics
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent / "src"))
from seqsee.testing import e2e  # noqa: E402


def run_perl(seed, seq, cont, max_steps):
    proc = subprocess.run([str(HERE / "run_perl.sh"), str(HERE / "e2e_run.perl"),
                           str(seed), seq, cont, str(max_steps)], capture_output=True, text=True,
                          errors="replace")  # Seqsee prints some Latin-1 bytes
    lines = proc.stdout.strip().splitlines()
    if proc.returncode != 0 or not lines:
        sys.exit(f"e2e_run failed (seed {seed}, {seq!r}):\n{proc.stderr}")
    return json.loads(lines[-1])


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--seeds", type=int, default=40)
    ap.add_argument("--max-steps", type=int, default=10000)
    ap.add_argument("--jobs", type=int, default=os.cpu_count())
    ap.add_argument("--out")
    args = ap.parse_args()
    seqs = e2e.parse_sequences()
    seeds = range(1, args.seeds + 1)
    jobs = [(sd, seq, cont) for _, seq, cont in seqs for sd in seeds]
    with ThreadPoolExecutor(args.jobs) as pool:
        perl = list(pool.map(lambda j: run_perl(*j, args.max_steps), jobs))
    by_key = {(r["seq"], r["seed"]): r for r in perl}
    py = e2e.run_all(seeds, args.max_steps, seqs, args.jobs)
    rows = []
    for (label, seq, cont), d in zip(seqs, py):
        p = e2e.summarize(label, seq, cont, args.max_steps, seeds,
                          [by_key[(seq, sd)] for sd in seeds])
        ps, qs = e2e.success_steps(p), e2e.success_steps(d)
        rows.append({
            "label": label, "seq": seq,
            "perl": p["successes"], "python": d["successes"],
            "perl_median": e2e.median_or_none(ps), "python_median": e2e.median_or_none(qs),
            "perl_statuses": dict(collections.Counter(p["statuses"])),
            "python_statuses": dict(collections.Counter(d["statuses"])),
        })
    for r in rows:
        print(f"{(r['label'] + ': ' if r['label'] else '') + r['seq']:44.44s} "
              f"{r['perl']:3d} {r['python']:3d}  {str(r['perl_median']):>7s} "
              f"{str(r['python_median']):>7s}  {r['perl_statuses']} {r['python_statuses']}")
    if args.out:
        Path(args.out).write_text(json.dumps(rows, indent=1) + "\n")


if __name__ == "__main__":
    main()

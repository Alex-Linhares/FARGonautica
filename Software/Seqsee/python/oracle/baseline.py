#!/usr/bin/env python3
"""Perl end-to-end baseline: config/sequence_list_for_testing x seeds -> golden.

Runs oracle/e2e_run.perl (headless Test::Seqsee::RunSeqsee) for every sequence
in the list and every seed, in parallel, and writes
tests/golden/e2e_baseline.json: a list with one entry per sequence (in file
order) holding per-seed statuses and step counts plus the success rate.

Usage: python3 oracle/baseline.py [--seeds N] [--max-steps N] [--jobs N]
"""
import argparse
import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parent.parent
SEQ_LIST = REPO / "config" / "sequence_list_for_testing"
GOLDEN = HERE.parent / "tests" / "golden" / "e2e_baseline.json"


def parse_sequences(path=SEQ_LIST):
    """Return [(label, seq, continuation)]; `Label:` prefixes are stripped."""
    out = []
    for line in path.read_text().splitlines():
        if not line.strip():
            continue
        label, _, rest = line.rpartition(":")
        seq, cont = rest.split("|")
        out.append((label, " ".join(seq.split()), " ".join(cont.split())))
    return out


def run_one(seed, seq, cont, max_steps):
    proc = subprocess.run(
        [str(HERE / "run_perl.sh"), str(HERE / "e2e_run.perl"),
         str(seed), seq, cont, str(max_steps)],
        capture_output=True, text=True,
    )
    lines = proc.stdout.strip().splitlines()
    if proc.returncode != 0 or not lines:
        sys.exit(f"e2e_run failed (seed {seed}, {seq!r}):\n{proc.stderr}")
    return json.loads(lines[-1])


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--seeds", type=int, default=10)
    ap.add_argument("--max-steps", type=int, default=10000)
    ap.add_argument("--jobs", type=int, default=os.cpu_count())
    args = ap.parse_args()

    seqs = parse_sequences()
    seeds = list(range(1, args.seeds + 1))
    jobs = [(s, seq, cont) for _, seq, cont in seqs for s in seeds]
    # Each job is a separate perl process, so threads give real parallelism.
    with ThreadPoolExecutor(args.jobs) as pool:
        results = list(pool.map(
            lambda j: run_one(*j, args.max_steps), jobs))
    by_key = {(r["seq"], r["seed"]): r for r in results}

    data = []
    for label, seq, cont in seqs:
        runs = [by_key[(seq, s)] for s in seeds]
        statuses = [r["status"] for r in runs]
        successes = statuses.count("Successful")
        data.append({
            "label": label,
            "seq": seq,
            "continuation": cont,
            "max_steps": args.max_steps,
            "seeds": seeds,
            "statuses": statuses,
            "steps": [r["steps"] for r in runs],
            "errors": [r["error"] for r in runs],
            "successes": successes,
            "success_rate": successes / len(seeds),
        })
    GOLDEN.write_text(json.dumps(data, indent=1, sort_keys=True) + "\n")
    print(f"-> {GOLDEN.relative_to(HERE.parent)} ({len(data)} sequences)")
    for d in data:
        print(f"{d['successes']:2d}/{len(seeds)}  {d['label'] or '-':15s} {d['seq']}")


if __name__ == "__main__":
    main()

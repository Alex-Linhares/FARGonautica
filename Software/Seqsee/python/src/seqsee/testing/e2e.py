"""End-to-end runs of the Python port, the counterpart of oracle/e2e_run.perl and
oracle/baseline.py (item 050).

Each run follows e2e_run.perl in a fresh state: ``s.reset_all()``, ``s.load()``,
``harness.load()`` (``use Test::Seqsee``), ``util.srand(seed)`` and then
``harness.run_seqsee(seq, continuation, max_steps, max_false, min_extension)`` (RunSeqsee).
Seqsee's own prints are swallowed.

``run_all`` runs config/sequence_list_for_testing × seeds in a process pool and returns
entries shaped like tests/golden/e2e_baseline.json. Python cannot follow Perl's trajectory
draw for draw (hash order and addresses differ), so the two are compared statistically.

Usage: ``python3 -m seqsee.testing.e2e [--seeds N] [--max-steps N] [--jobs N] [--json]``
prints a Perl-vs-Python table (success counts and median steps of successful runs).
"""
import argparse
import contextlib
import io
import json
import os
import statistics
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

REPO = Path(__file__).resolve().parents[4]
SEQ_LIST = REPO / "config" / "sequence_list_for_testing"
BASELINE = REPO / "python" / "tests" / "golden" / "e2e_baseline.json"


def parse_sequences(path=SEQ_LIST):
    """Return [(label, seq, continuation)] as oracle/baseline.py does (labels stripped,
    whitespace normalised)."""
    out = []
    for line in Path(path).read_text().splitlines():
        if not line.strip():
            continue
        label, _, rest = line.rpartition(":")
        seq, cont = rest.split("|")
        out.append((label, " ".join(seq.split()), " ".join(cont.split())))
    return out


def run_one(seed, seq, cont, max_steps, max_false=3, min_extension=3):
    """One e2e_run.perl run → {seed, seq, status, steps, error}."""
    from seqsee import s, util
    from seqsee.testing import harness

    with contextlib.redirect_stdout(io.StringIO()):
        s.reset_all()
        s.load()
        harness.load()
        util.srand(seed)
        r = harness.run_seqsee(seq.split(), cont.split(), max_steps, max_false, min_extension)
    err = r.get_error()
    return {
        "seed": seed,
        "seq": seq,
        "status": r.get_status().get_status_string(),
        "steps": int(r.get_steps()),
        "error": None if err is None else str(err),
    }


def _run_job(job):
    return run_one(*job)


def summarize(label, seq, cont, max_steps, seeds, runs):
    """One baseline-shaped entry from the runs of a sequence (in seed order)."""
    statuses = [r["status"] for r in runs]
    successes = statuses.count("Successful")
    return {
        "label": label,
        "seq": seq,
        "continuation": cont,
        "max_steps": max_steps,
        "seeds": list(seeds),
        "statuses": statuses,
        "steps": [r["steps"] for r in runs],
        "errors": [r["error"] for r in runs],
        "successes": successes,
        "success_rate": successes / len(seeds),
    }


def run_all(seeds=range(1, 11), max_steps=10000, sequences=None, jobs=None):
    """Run every sequence × seed in a process pool; entries in sequence order."""
    seeds = list(seeds)
    sequences = parse_sequences() if sequences is None else sequences
    work = [(sd, seq, cont, max_steps) for _, seq, cont in sequences for sd in seeds]
    # max_tasks_per_child=1: every run starts in a fresh process, as each e2e_run.perl does.
    with ProcessPoolExecutor(jobs or os.cpu_count(), max_tasks_per_child=1) as pool:
        results = list(pool.map(_run_job, work))
    by_key = {(r["seq"], r["seed"]): r for r in results}
    return [summarize(label, seq, cont, max_steps, seeds, [by_key[(seq, sd)] for sd in seeds])
            for label, seq, cont in sequences]


def success_steps(entry):
    return [st for st, s in zip(entry["steps"], entry["statuses"]) if s == "Successful"]


def median_or_none(xs):
    return statistics.median(xs) if xs else None


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--seeds", type=int, default=10)
    ap.add_argument("--max-steps", type=int, default=10000)
    ap.add_argument("--jobs", type=int, default=os.cpu_count())
    ap.add_argument("--json", action="store_true")
    args = ap.parse_args(argv)
    data = run_all(range(1, args.seeds + 1), args.max_steps, jobs=args.jobs)
    if args.json:
        print(json.dumps(data, indent=1, sort_keys=True))
        return
    perl = {d["seq"]: d for d in json.loads(BASELINE.read_text())}
    print(f"{'sequence':48s} {'Perl':>6s} {'Py':>6s} {'Perl med':>9s} {'Py med':>9s}")
    for d in data:
        p = perl.get(d["seq"])
        pm = median_or_none(success_steps(p)) if p else None
        print(f"{(d['label'] + ': ' if d['label'] else '') + d['seq']:48.48s} "
              f"{(str(p['successes']) if p else '-'):>6s} {d['successes']:>6d} "
              f"{str(pm):>9s} {str(median_or_none(success_steps(d))):>9s}")


if __name__ == "__main__":
    main()

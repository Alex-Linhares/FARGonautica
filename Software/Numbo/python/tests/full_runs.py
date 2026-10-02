"""Full-run differential testing, Python vs. the SBCL oracle (loop0002 item 12).

A run is (problem, seed, max-iterations).  The oracle side runs
lisp/tests/oracle/lib/full-run.lisp in a fresh SBCL process (oracle mode), which
writes the run's JSON-lines trace and a JSON object with its outcome and
printed output.  The Python side runs harness.run_config in a fresh World.
The two traces are then compared event by event, as parsed JSON through
json.dumps (so 3 and 3.0 differ), with the RNG draws in the trace.  On a
mismatch the report names the first divergent event with the events before
it.  The outcome plist and the printed text must be equal too, and so must
the solution checker's verdict on the printed text (check-solution on the
oracle side, solution_checker.check_solution on the Python side).

Full traces are too big to commit (up to several MB each), so they are made
on the fly in a temporary directory.  Runs are independent and are spread
over a process pool.

Also a script, for sweeps:
    python3 python/tests/full_runs.py [--seeds 1-20] [--puzzles 1,3] [--cap N]
"""

import collections
import concurrent.futures
import io
import json
import os
import pathlib
import subprocess
import sys
import tempfile

PYTHON_DIR = pathlib.Path(__file__).resolve().parent.parent
REPO_DIR = PYTHON_DIR.parent
LISP_DIR = REPO_DIR / "lisp"   # the SBCL port, the tests' oracle
FULL_RUN_LISP = LISP_DIR / "tests" / "oracle" / "lib" / "full-run.lisp"

if str(PYTHON_DIR) not in sys.path:
    sys.path.insert(0, str(PYTHON_DIR))

# The chapter's 11 puzzles (lisp/src/RESULTS.md, lisp/tests/chapter-runs.lisp), as
# (target b1 ... b5).
PUZZLES = [
    (114, 11, 20, 7, 1, 6),
    (87, 8, 3, 9, 10, 7),
    (31, 3, 5, 24, 3, 14),
    (25, 8, 5, 5, 11, 2),
    (102, 6, 17, 2, 4, 1),
    (146, 12, 2, 5, 7, 18),
    (6, 3, 3, 17, 11, 22),
    (11, 2, 5, 1, 25, 23),
    (116, 20, 2, 16, 14, 6),
    (127, 6, 4, 22, 5, 7),
    (41, 5, 16, 22, 25, 1),
]
SEEDS = range(1, 21)
CAP = 20000  # lisp/tests/chapter-runs.lisp's cap

CONTEXT = 4


def canonical(event):
    return json.dumps(event)


def lisp_list(xs):
    return "(" + " ".join(str(x) for x in xs) + ")"


def oracle_run(problem, seed, cap, base, rng_events=True):
    """Run the oracle in a fresh SBCL process; BASE.jsonl gets the trace and
    BASE.json the result.  Returns the result object."""
    spec = '(defparameter cl-user::*full-run* (quote ({} {} {} {} "{}")))'.format(
        lisp_list(problem), seed, "nil" if cap is None else cap, "t" if rng_events else "nil",
        base)
    proc = subprocess.run(
        ["sbcl", "--noinform", "--non-interactive", "--no-userinit", "--no-sysinit",
         "--eval", spec, "--load", str(FULL_RUN_LISP)],
        cwd=REPO_DIR, capture_output=True, text=True, timeout=600,
        env={k: v for k, v in os.environ.items() if k != "WINDOW_GFX"})
    if proc.returncode != 0:
        raise RuntimeError(f"oracle run {problem} seed {seed} failed:\n{proc.stdout}\n{proc.stderr}")
    with open(base + ".json", encoding="utf-8") as f:
        return json.load(f)


def python_run(problem, seed, cap, trace_path, rng_events=True):
    """Run the Python in a fresh World; the trace goes to TRACE_PATH."""
    from numbo import harness, solution_checker

    out = io.StringIO()
    result = harness.run_config(list(problem), seed=seed, max_iterations=cap,
                                trace=trace_path, rng_events=rng_events, out=out)
    text = out.getvalue()
    return dict(result, output=text,
                check=list(solution_checker.check_solution(text, list(problem))))


def first_divergence(oracle_lines, python_lines, context=CONTEXT):
    """None if the two event streams (iterables of JSON lines) are equal,
    else a report of the first differing event and the events before it."""
    before = collections.deque(maxlen=context)
    n = 0
    for n, (o, p) in enumerate(zip_longest_lines(oracle_lines, python_lines)):
        eo = None if o is None else canonical(json.loads(o))
        ep = None if p is None else canonical(json.loads(p))
        if eo != ep:
            shown = "\n".join(f"  [{i}] {e[:300]}" for i, e in before)
            if eo is None or ep is None:
                who, extra = ("python", ep) if eo is None else ("oracle", eo)
                return (f"event streams differ in length: first extra event #{n} ({who}):\n"
                        f"{shown}\n  {who}: {extra[:600]}")
            return (f"first divergent event #{n}:\n{shown}\n"
                    f"  oracle: {eo[:600]}\n  python: {ep[:600]}")
        before.append((n, eo))
    return None


def zip_longest_lines(a, b):
    a, b = iter(a), iter(b)
    while True:
        x, y = next(a, None), next(b, None)
        if x is None and y is None:
            return
        yield x, y


def compare_run(problem, seed, cap=CAP, workdir=None, rng_events=True, keep=False):
    """Run PROBLEM with SEED in both and compare.  Returns a dict: problem,
    seed, outcome, iterations (the oracle's), events (the oracle's count),
    and report (None when everything matches)."""
    tmp = None
    if workdir is None:
        tmp = tempfile.TemporaryDirectory(prefix="numbo-full-run-")
        workdir = tmp.name
    try:
        tag = f"{problem[0]}-seed{seed}"
        base = os.path.join(workdir, f"oracle-{tag}")
        py_trace = os.path.join(workdir, f"python-{tag}.jsonl")
        expected = oracle_run(problem, seed, cap, base, rng_events)
        got = python_run(problem, seed, cap, py_trace, rng_events)
        reports = []
        with open(base + ".jsonl", encoding="utf-8") as fo, \
                open(py_trace, encoding="utf-8") as fp:
            divergence = first_divergence(fo, fp)
        if divergence:
            reports.append(divergence)
        for key in ("outcome", "iterations", "problem-solved", "error", "check"):
            if canonical(expected[key]) != canonical(got[key]):
                reports.append(f"{key}: oracle {expected[key]!r}, python {got[key]!r}")
        if expected["output"] != got["output"]:
            o, p = expected["output"], got["output"]
            i = next((i for i, (x, y) in enumerate(zip(o, p)) if x != y), min(len(o), len(p)))
            reports.append(f"printed output differs at character {i}:\n"
                           f"  oracle: {o[max(0, i - 200):i + 200]!r}\n"
                           f"  python: {p[max(0, i - 200):i + 200]!r}")
        with open(base + ".jsonl", encoding="utf-8") as fo:
            events = sum(1 for _ in fo)
        if not keep:
            for path in (base + ".json", base + ".jsonl", py_trace):
                os.remove(path)
        return {"problem": list(problem), "seed": seed, "cap": cap,
                "outcome": expected["outcome"], "iterations": expected["iterations"],
                "error": expected["error"], "events": events,
                "check": expected["check"],
                "report": "\n".join(reports) if reports else None}
    finally:
        if tmp is not None:
            tmp.cleanup()


def _compare_star(args):
    return compare_run(*args)


def sweep(specs, workdir, jobs=None):
    """compare_run over SPECS, a list of (problem, seed, cap), in a process
    pool.  Returns the results in SPECS' order."""
    jobs = jobs or min(len(specs), os.cpu_count() or 1)
    with concurrent.futures.ProcessPoolExecutor(max_workers=jobs) as pool:
        return list(pool.map(_compare_star, [(p, s, c, workdir) for p, s, c in specs]))


def _parse_range(text):
    out = []
    for part in text.split(","):
        lo, _, hi = part.partition("-")
        out.extend(range(int(lo), int(hi or lo) + 1))
    return out


def main(argv):
    import argparse

    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--puzzles", default="1-11")
    parser.add_argument("--seeds", default="1-20")
    parser.add_argument("--cap", type=int, default=CAP)
    parser.add_argument("--jobs", type=int, default=None)
    args = parser.parse_args(argv)
    specs = [(PUZZLES[p - 1], s, args.cap)
             for p in _parse_range(args.puzzles) for s in _parse_range(args.seeds)]
    with tempfile.TemporaryDirectory(prefix="numbo-full-runs-") as workdir:
        results = sweep(specs, workdir, args.jobs)
    bad = 0
    for r in results:
        status = "ok" if r["report"] is None else "DIVERGES"
        print(f"{r['problem'][0]:>4} seed {r['seed']:>3}: {r['outcome']:<8} "
              f"{r['iterations']:>6} it {r['events']:>7} ev  {status}")
        if r["report"]:
            bad += 1
            print(r["report"])
    print(f"{len(results) - bad}/{len(results)} runs match")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

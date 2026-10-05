"""Run times of the Python CLI against the oracle (loop0002 item 11).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  python3 python/oracle/bench_runs.py [OUT.md] [--jobs N]

Runs every run of tests/problems.txt once as a process, `python3 -m metacat ...`
and `scheme --script chez_scheme/oracle/run.ss ...` with the same arguments, N
processes at a time (default 8, on a 32-core machine, so that they do not compete),
checks that the two print the same output, and writes the per-problem table of
docs/python-run-times.md (wall-clock seconds per process, summed over a problem's
seeds, startup included).  The counterpart of racket/tests/bench-runs.rkt.
"""
from __future__ import annotations

import os
import re
import shutil
import statistics
import subprocess
import sys
import time
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
PYTHON_DIR = os.path.dirname(HERE)
ROOT = os.path.dirname(PYTHON_DIR)
sys.path[:0] = [os.path.join(PYTHON_DIR, "tests"), PYTHON_DIR]

import golden_harness  # noqa: E402

SCHEME = shutil.which("scheme") or shutil.which("chezscheme")


def timed(cmd, cwd):
    t = time.perf_counter()
    p = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, stdin=subprocess.DEVNULL)
    return time.perf_counter() - t, p.returncode, p.stdout


def args_of(strings, seed, cap, keep):
    return list(strings) + ["--seed", str(seed), "--max-codelets", str(cap)] + (
        ["--keep-going"] if keep else [])


def one(job):
    strings, seed, cap, keep = job
    args = args_of(strings, seed, cap, keep)
    ct, cc, co = timed([SCHEME, "--script", "chez_scheme/oracle/run.ss", *args], ROOT)
    pt, pc, po = timed([sys.executable, "-m", "metacat", *args], PYTHON_DIR)
    if (cc, co) != (pc, po):
        raise SystemExit("different output for %s" % args)
    codelets = int(re.search(r"\nCodelets: (\d+)\n", po).group(1))
    return ct, pt, codelets


def startup():
    """Load, set up a problem, run 1 codelet: median of 5."""
    c = statistics.median(timed([SCHEME, "--script", "chez_scheme/oracle/run.ss", "abc",
                                 "abd", "xyz", "--seed", "1", "--max-codelets", "1"], ROOT)[0]
                          for _ in range(5))
    p = statistics.median(timed([sys.executable, "-m", "metacat", "abc", "abd", "xyz",
                                 "--seed", "1", "--max-codelets", "1"], PYTHON_DIR)[0]
                          for _ in range(5))
    return c, p


def main():
    args = sys.argv[1:]
    jobs_n = 8
    if "--jobs" in args:
        i = args.index("--jobs")
        jobs_n = int(args[i + 1])
        del args[i:i + 2]
    out = args[0] if args else None
    c0, p0 = startup()
    runs = golden_harness.golden_runs()
    with ThreadPoolExecutor(jobs_n) as pool:
        results = list(pool.map(one, [(s, seed, cap, keep) for _, s, seed, cap, keep in runs]))
    table = {}
    for (_, s, *_rest), (ct, pt, n) in zip(runs, results):
        row = table.setdefault(" ".join(s), [0, 0, 0.0, 0.0, 0.0, 0.0])
        row[0] += 1
        row[1] += n
        row[2] += ct
        row[3] += pt
        row[4] += max(0.0, ct - c0)
        row[5] += max(0.0, pt - p0)
    lines = ["Startup (load, set up a problem, run 1 codelet), median of 5: "
             "Chez %.2f s, Python %.2f s." % (c0, p0), "",
             "| Problem | Runs | Codelets | Chez s | Python s | Python / Chez "
             "| Chez ms/codelet | Python ms/codelet |",
             "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |"]
    tot = [0, 0, 0.0, 0.0, 0.0, 0.0]
    for problem, row in table.items():
        tot = [a + b for a, b in zip(tot, row)]
        lines.append("| `%s` | %d | %d | %.2f | %.2f | %.2f | %.2f | %.2f |" % (
            problem, row[0], row[1], row[2], row[3], row[3] / row[2],
            1000 * row[4] / row[1], 1000 * row[5] / row[1]))
    lines.append("| **all** | %d | %d | %.2f | %.2f | %.2f | %.2f | %.2f |" % (
        tot[0], tot[1], tot[2], tot[3], tot[3] / tot[2], 1000 * tot[4] / tot[1],
        1000 * tot[5] / tot[1]))
    text = "\n".join(lines) + "\n"
    if out:
        with open(out, "w") as f:
            f.write(text)
    sys.stdout.write(text)


if __name__ == "__main__":
    main()

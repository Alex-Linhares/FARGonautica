"""CPU time of a fixed set of Python runs, for measuring speed-ups (loop0002 item 12).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  python3 python/oracle/bench_speed.py [--repeat R] [--jobs J] [DIR ...]

Runs each run of RUNS as `python3 -m metacat ...` from each DIR (a copy of
python/, default python/ itself), R times (default 3), J processes at a time
(default 1), and prints each run's best user+system CPU time and the total of the
bests, with the run's codelet count.  The repetitions are interleaved across the
DIRs, so that a machine whose load changes during the benchmark slows every
variant alike; that is how the gains of item 12's speed-ups were measured, each
DIR being the package with the speed-ups up to one of them.  The runs were picked to
cover a short run, a long run whose Workspace and Memory grow (the halt problem),
a justify run and a keep-going run.  The outputs are not checked here: the
goldens and the extra seeds (test_golden.py, test_extra_seeds.py) do that.
"""
from __future__ import annotations

import argparse
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

HERE = os.path.dirname(os.path.abspath(__file__))
PYTHON_DIR = os.path.dirname(HERE)

RUNS = [
    "abc abd xyz --seed 3 --max-codelets 3000",
    "abc abd mrrjjj mrrjjjj --seed 1 --max-codelets 10000",
    "eqe qeq abbba aaabaaa --seed 7 --max-codelets 17000",
    "abc aabbcc kkjjii --seed 1 --max-codelets 1500 --keep-going",
    "rst rsu xyz --seed 2 --max-codelets 10000",
]


def cpu_of(job):
    """(CPU seconds, codelets) of one run: the waited child's own rusage (wait4),
    which is exact even when several runs go at once."""
    directory, args = job
    proc = subprocess.Popen([sys.executable, "-m", "metacat", *args.split()], cwd=directory,
                            stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                            stdin=subprocess.DEVNULL, text=True)
    out = proc.stdout.read()
    _, _, usage = os.wait4(proc.pid, 0)
    codelets = int(re.search(r"\nCodelets: (\d+)\n", out).group(1))
    return usage.ru_utime + usage.ru_stime, codelets


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repeat", type=int, default=3)
    ap.add_argument("--jobs", type=int, default=1)
    ap.add_argument("dirs", nargs="*", default=[PYTHON_DIR])
    args = ap.parse_args()
    jobs = [(d, run) for _ in range(args.repeat) for d in args.dirs for run in RUNS]
    with ThreadPoolExecutor(args.jobs) as pool:
        results = list(pool.map(cpu_of, jobs))
    best = {}
    for job, (t, codelets) in zip(jobs, results):
        best[job] = min(best.get(job, (t, codelets)), (t, codelets))
    for d in args.dirs:
        print(d)
        total = 0.0
        for run in RUNS:
            t, codelets = best[(d, run)]
            total += t
            print("%7.2f s  %6d codelets  %5.3f ms/codelet  %s"
                  % (t, codelets, 1000 * t / codelets, run))
        print("%7.2f s  total" % total, flush=True)


if __name__ == "__main__":
    main()

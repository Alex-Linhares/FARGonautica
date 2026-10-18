#!/usr/bin/env python3
"""The statistical equivalence of the pure engine (D-107, D-113): for every families
problem, the answer distribution of N runs on the compiled engine and of N runs on the pure
engine (a fresh process started with `TABLETOP_ENGINE=python`), the total variation (TV)
between the two per problem and its mean; and the null, which calibrates the tolerance: the
TV between two independent samples of N runs of one engine (the compiled engine's first N
seeds against its next N, observed), and the 99th and 99.9th percentiles of the TV between
two independent multinomial samples of N drawn from the problem's pooled distribution
(simulated with a seeded generator, NULL_DRAWS pairs).

    python3 -I tools/equivalence.py [--runs N] [--tolerance T] [--profile NAME]
        [--problems NAME ...] [--only N] [--seed S] [--null-only] [--out FILE]
        [--record FILE]

Each engine runs in its own process, since one process runs one engine (D-107): the tool
itself never imports `tabletop`, never sets the variable for the compiled engine (unset is
the compiled engine, required) and never catches the build error to carry on: a missing
build fails the compiled process with its message, and the tool stops. `--null-only` runs
the compiled engine alone (the two independent samples) and reports the null without the
comparison; it is how the tolerance was derived before the comparison was run (D-113).
`--out FILE` writes the full JSON record (every count, every TV, the null, the engines'
`accel.info()`, the commit); `--record FILE` writes the compact record the fast test reads
(`tests/data/equivalence.json`: the pure engine's counts per problem, the engine, the
commit and the runs). The exit status is 1 when any problem's TV exceeds the tolerance.

The seeds are the benches' (D-072, D-064): bench seed 1001, the problem's batch seed from
it, the run seeds from the batch seed, so that the N runs compared are the families bench's
own first N on each engine, and the null's second sample is the next N.
"""

import argparse
import importlib
import importlib.util
import json
import os
import random
import subprocess
import sys
import time
from pathlib import Path

PYTHON_DIR = Path(__file__).resolve().parents[1]
TOOLS = Path(__file__).resolve().parent
BENCH_SEED = 1001
RUNS = 200              # runs per problem per engine (D-113)
TOLERANCE = 0.25        # the TV any problem may reach, at RUNS (D-113)
NULL_DRAWS = 2000       # simulated pairs of samples per problem for the null's percentiles
NULL_SEED = 20261015    # the seed of the null's simulation (deterministic)
PERCENTILES = (0.99, 0.999)
ENGINES = ("compiled", "python")


def parser() -> argparse.ArgumentParser:
    """The options (D-113): `--runs` and `--tolerance` default to the module's constants."""
    p = argparse.ArgumentParser(prog="python3 -I tools/equivalence.py",
                                description="The answer distributions of the two engines, "
                                            "their total variation per problem, and the null.")
    p.add_argument("--runs", type=int, default=RUNS,
                   help=f"runs per problem on each engine (default {RUNS})")
    p.add_argument("--tolerance", type=float, default=TOLERANCE,
                   help=f"the TV a problem may reach (default {TOLERANCE})")
    p.add_argument("--profile", default="standard", help="the configuration profile")
    p.add_argument("--problems", nargs="+", metavar="NAME", help="only these problems")
    p.add_argument("--only", type=int, metavar="N", help="only the first N problems")
    p.add_argument("--seed", type=int, default=BENCH_SEED,
                   help=f"the bench seed the run seeds derive from (default {BENCH_SEED})")
    p.add_argument("--null-only", action="store_true",
                   help="run the compiled engine alone and report the null, no comparison")
    p.add_argument("--out", metavar="FILE", help="write the full JSON record here")
    p.add_argument("--record", metavar="FILE",
                   help="write the compact record the fast test reads here")
    p.add_argument("--engine-dir", metavar="DIR", default=str(PYTHON_DIR),
                   help="the folder holding the `tabletop` package (default: this one)")
    p.add_argument("--worker", choices=ENGINES, help=argparse.SUPPRESS)
    return p


# -- the statistics ------------------------------------------------------------------------

def counts_of(answers: list) -> dict:
    """The answer counts of a sample, keyed by answer id, in the order first seen (D-113)."""
    out = {}
    for a in answers:
        out[a] = out.get(a, 0) + 1
    return out


def total_variation(counts_a: dict, counts_b: dict) -> float:
    """The total variation distance between two samples' answer distributions (D-113):
    half the sum over answers of the absolute difference of the frequencies."""
    n_a, n_b = sum(counts_a.values()), sum(counts_b.values())
    keys = set(counts_a) | set(counts_b)
    return 0.5 * sum(abs(counts_a.get(k, 0) / n_a - counts_b.get(k, 0) / n_b) for k in keys)


def null_quantiles(pooled: dict, n_a: int, n_b: int, quantiles=PERCENTILES,
                   draws: int = NULL_DRAWS, seed: int = NULL_SEED) -> dict:
    """The null's percentiles (D-113): the TV between two independent multinomial samples
    of `n_a` and `n_b` runs drawn from the frequencies of `pooled`, over `draws` simulated
    pairs with a seeded generator. Returns {quantile: TV}."""
    rng = random.Random(seed)
    population = list(pooled)
    weights = [pooled[k] for k in population]
    tvs = []
    for _ in range(draws):
        a = counts_of(rng.choices(population, weights, k=n_a))
        b = counts_of(rng.choices(population, weights, k=n_b))
        tvs.append(total_variation(a, b))
    tvs.sort()
    return {q: tvs[min(len(tvs) - 1, int(q * len(tvs)))] for q in quantiles}


# -- the worker: one engine, one process ----------------------------------------------------

def load_workload():
    """`tools/workload.py`, loaded by file (the tools run under `-I`, with no path set up)."""
    spec = importlib.util.spec_from_file_location("workload", TOOLS / "workload.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def worker(args) -> int:
    """Run every problem `2 * runs` times (the compiled engine: the comparison's sample and
    the null's second, independent sample) or `runs` times (the pure engine) and print one
    JSON record: the engine, `accel.info()`, and per problem the batch seed and every run's
    (seed, answer, codelets, capped) in seed order (D-113)."""
    workload = load_workload()
    workload.load_package(Path(args.engine_dir).resolve())
    accel = importlib.import_module("tabletop.accel")
    if accel.info()["engine"] != args.worker:
        raise SystemExit(f"the worker for the {args.worker} engine got the "
                         f"{accel.info()['engine']} engine")
    families = importlib.import_module("tabletop.bench.families")
    harness = importlib.import_module("tabletop.bench.harness")
    run = importlib.import_module("tabletop.run")
    load_problem = importlib.import_module("tabletop.problems").load_problem
    problems = families.problems_of(only=args.problems)
    if args.only is not None:
        problems = problems[:args.only]
    config = workload.configured(args.profile, [])
    runs = 2 * args.runs if args.worker == "compiled" else args.runs
    cpu0 = time.process_time()
    out = {}
    for name in problems:
        problem = load_problem(name)
        batch = harness.batch_seed_for(args.seed, name)
        rows = []
        for s in accel.run_seeds(batch, runs):
            r = run.run_problem(problem, config, s)
            rows.append([s, r.answer, r.codelets, bool(r.capped)])
        out[name] = {"batch_seed": batch, "rows": rows}
    print(json.dumps({"engine": args.worker, "info": accel.info(), "runs": runs,
                      "cpu": round(time.process_time() - cpu0, 3), "problems": out}))
    return 0


def start_worker(engine: str, args, argv: list) -> subprocess.Popen:
    """A fresh process for one engine (D-107): the variable unset for the compiled engine,
    `python` for the pure one; the error of a missing build is the process's and is
    relayed, never caught."""
    env = {k: v for k, v in os.environ.items() if k != "TABLETOP_ENGINE"}
    if engine == "python":
        env["TABLETOP_ENGINE"] = "python"
    cmd = [sys.executable, "-I", str(Path(__file__).resolve()), "--worker", engine,
           "--engine-dir", args.engine_dir, "--runs", str(args.runs),
           "--profile", args.profile, "--seed", str(args.seed)]
    if args.problems:
        cmd += ["--problems", *args.problems]
    if args.only is not None:
        cmd += ["--only", str(args.only)]
    return subprocess.Popen(cmd, env=env, cwd=str(PYTHON_DIR), text=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)


def finish(proc: subprocess.Popen, engine: str) -> dict:
    out, err = proc.communicate()
    if proc.returncode != 0:
        sys.stderr.write(err)
        raise SystemExit(f"the {engine} engine's process failed (exit {proc.returncode}); "
                         "see its message above")
    return json.loads(out.strip().splitlines()[-1])


def commit_of(folder: Path) -> str:
    """The commit of the folder's checkout, or 'unknown' outside one (D-113)."""
    try:
        proc = subprocess.run(["git", "-C", str(folder), "rev-parse", "HEAD"],
                              capture_output=True, text=True, check=False)
    except OSError:
        return "unknown"
    return proc.stdout.strip() if proc.returncode == 0 else "unknown"


# -- the comparison ---------------------------------------------------------------------

def compare(compiled: dict, python: dict | None, runs: int, tolerance: float) -> dict:
    """Per problem (D-113): the compiled engine's counts over its first `runs` seeds, the
    null's second sample's counts, the pure engine's counts, the TV compiled against pure,
    the observed null TV (first against second compiled sample) and the simulated null's
    percentiles from the pooled compiled counts. Then the mean TV and the verdict."""
    rows = []
    for name, data in compiled["problems"].items():
        first = counts_of([r[1] for r in data["rows"][:runs]])
        second = counts_of([r[1] for r in data["rows"][runs:2 * runs]])
        pooled = counts_of([r[1] for r in data["rows"][:2 * runs]])
        n_pure = runs
        quantiles = null_quantiles(pooled, runs, n_pure)
        row = {"problem": name, "batch_seed": data["batch_seed"], "runs": runs,
               "compiled": first, "compiled_second": second,
               "null_observed": round(total_variation(first, second), 4),
               "null_q99": round(quantiles[0.99], 4),
               "null_q999": round(quantiles[0.999], 4),
               "answers": len(pooled)}
        if python is not None:
            pure = counts_of([r[1] for r in python["problems"][name]["rows"][:runs]])
            row["python"] = pure
            row["tv"] = round(total_variation(first, pure), 4)
            row["within"] = row["tv"] <= tolerance
            same_seed = sum(1 for a, b in zip(data["rows"][:runs],
                                              python["problems"][name]["rows"][:runs])
                            if a[1] == b[1])
            row["same_answer_on_same_seed"] = same_seed
        rows.append(row)
    summary = {"problems": len(rows),
               "null_observed_mean": round(sum(r["null_observed"] for r in rows) / len(rows), 4),
               "null_observed_max": max(r["null_observed"] for r in rows),
               "null_q99_max": max(r["null_q99"] for r in rows),
               "null_q999_max": max(r["null_q999"] for r in rows)}
    if python is not None:
        summary["tv_mean"] = round(sum(r["tv"] for r in rows) / len(rows), 4)
        summary["tv_max"] = max(r["tv"] for r in rows)
        summary["beyond_tolerance"] = [r["problem"] for r in rows if not r["within"]]
    return {"rows": rows, "summary": summary}


def report(result: dict, tolerance: float) -> list:
    """The table: problem, answers, TV (compiled against pure), the observed null, the
    simulated null's 99th and 99.9th percentiles, the verdict (D-113)."""
    has_pure = "tv" in result["rows"][0]
    lines = ["| problem | answers | TV compiled vs python | null, observed | null, 99th | null, 99.9th | within |",
             "|---|---:|---:|---:|---:|---:|---|"]
    for r in result["rows"]:
        tv = f"{r['tv']:.3f}" if has_pure else "—"
        within = ("yes" if r["within"] else "**no**") if has_pure else "—"
        lines.append(f"| {r['problem']} | {r['answers']} | {tv} | {r['null_observed']:.3f} | "
                     f"{r['null_q99']:.3f} | {r['null_q999']:.3f} | {within} |")
    s = result["summary"]
    tail = (f"mean TV {s['tv_mean']:.3f}, max {s['tv_max']:.3f}; " if has_pure else "")
    lines.append("")
    lines.append(f"{s['problems']} problems: {tail}null observed mean "
                 f"{s['null_observed_mean']:.3f}, max {s['null_observed_max']:.3f}; null 99th "
                 f"max {s['null_q99_max']:.3f}, 99.9th max {s['null_q999_max']:.3f}; "
                 f"tolerance {tolerance}")
    return lines


def compact_record(result: dict, python: dict, args, meta: dict) -> dict:
    """The record the fast test reads (D-113): the pure engine's counts per problem, with
    the engine, its `accel.info()`, the commit, the runs, the profile and the bench seed."""
    return {"engine": "python", "info": python["info"], "runs": args.runs,
            "profile": args.profile, "seed": args.seed, "tolerance": args.tolerance,
            "commit": meta["commit"], "date": meta["date"],
            "problems": {r["problem"]: {"batch_seed": r["batch_seed"], "counts": r["python"]}
                         for r in result["rows"]}}


def main(argv=None) -> int:
    args = parser().parse_args(argv)
    if args.worker:
        return worker(args)
    try:
        load1 = os.getloadavg()[0]
    except OSError:
        load1 = None
    date = time.strftime("%Y-%m-%d %H:%M:%S")
    wall0 = time.perf_counter()
    procs = {"compiled": start_worker("compiled", args, argv)}
    if not args.null_only:
        procs["python"] = start_worker("python", args, argv)
    records = {engine: finish(proc, engine) for engine, proc in procs.items()}
    wall = round(time.perf_counter() - wall0, 1)
    result = compare(records["compiled"], records.get("python"), args.runs, args.tolerance)
    meta = {"commit": commit_of(PYTHON_DIR), "date": date, "load1": load1, "wall": wall,
            "runs": args.runs, "tolerance": args.tolerance, "profile": args.profile,
            "seed": args.seed, "null_draws": NULL_DRAWS, "null_seed": NULL_SEED,
            "engines": {e: {"info": r["info"], "cpu": r["cpu"], "runs_per_problem": r["runs"]}
                        for e, r in records.items()}}
    print("engine: compiled" + ("" if args.null_only else " against python"))
    print(f"runs {args.runs} per problem per engine, bench seed {args.seed}, profile "
          f"{args.profile}, commit {meta['commit'][:12]}, load {load1}, {date}, {wall} s wall; "
          + ", ".join(f"{e} {r['cpu']} s CPU" for e, r in records.items()))
    print("\n".join(report(result, args.tolerance)))
    if args.out:
        Path(args.out).write_text(json.dumps({"meta": meta, **result}, indent=1) + "\n")
    if args.record and not args.null_only:
        Path(args.record).write_text(
            json.dumps(compact_record(result, records["python"], args, meta), indent=1) + "\n")
    beyond = result["summary"].get("beyond_tolerance", [])
    return 1 if beyond else 0


if __name__ == "__main__":
    sys.exit(main())

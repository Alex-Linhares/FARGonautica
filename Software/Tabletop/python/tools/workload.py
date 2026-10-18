#!/usr/bin/env python3
"""The timed workload: the families bench's problems, each run RUNS times under a profile,
bench seed 1001, the seeds derived as the benches derive them (D-072: the problem's batch
seed from the bench seed and its name, the run seeds from the batch seed, D-064). Prints
one JSON line with the CPU and wall seconds, the engine that ran, and a digest of the rows,
so that every timed run doubles as an identity check between two engines or two commits
(D-107: the pure engine's own goldens and the compiled engine's self-determinism).

    python3 -I tools/workload.py [--engine DIR] [--runs N] [--profile NAME] [--set FIELD=VALUE]
        [--problems NAME ...] [--only N] [--seed S] [--cprofile FILE]

`--engine DIR` is the folder that holds the `tabletop` package to time (another commit's
copy, say); the default is this folder. The package is loaded from that path by file, so
the interpreter needs no path set up and the tool runs under any Python (CPython, PyPy)
given on the command line. The engine's name is `tabletop.accel.info()["engine"]` where the
package has the engine seam, else `python` (the plain package before the seam).

The digest is the SHA-256 of every run's (problem, seed, answer, structure value, codelets,
capped, draws), in the problems' order: the same engine on the same seeds gives the same
digest in any process, and a change of any run changes it.
"""

import argparse
import hashlib
import importlib
import importlib.util
import json
import os
import platform
import resource
import sys
import time
from pathlib import Path

PYTHON_DIR = Path(__file__).resolve().parents[1]
BENCH_SEED = 1001


def parser() -> argparse.ArgumentParser:
    """The options (D-107)."""
    p = argparse.ArgumentParser(prog="python3 -I tools/workload.py",
                                description="Time the families workload on one engine and "
                                            "print its rows digest.")
    p.add_argument("--engine", metavar="DIR", default=str(PYTHON_DIR),
                   help="the folder holding the `tabletop` package to run (default: this one)")
    p.add_argument("--runs", type=int, default=10, help="runs per problem (default 10)")
    p.add_argument("--profile", default="standard", help="the configuration profile")
    p.add_argument("--set", action="append", default=[], metavar="FIELD=VALUE",
                   help="a Config field to change, as the command line's --set")
    p.add_argument("--problems", nargs="+", metavar="NAME", help="only these problems")
    p.add_argument("--only", type=int, metavar="N", help="only the first N problems")
    p.add_argument("--seed", type=int, default=BENCH_SEED,
                   help=f"the bench seed the run seeds derive from (default {BENCH_SEED})")
    p.add_argument("--cprofile", metavar="FILE", help="profile the runs with cProfile into FILE")
    return p


def load_package(engine_dir: Path):
    """Load the `tabletop` package found under `engine_dir` as `tabletop`, by file, so the
    engine timed is the one named and no other (D-107)."""
    init = Path(engine_dir).resolve() / "tabletop" / "__init__.py"
    if not init.is_file():
        raise SystemExit(f"no tabletop package under {engine_dir}: {init} is missing")
    spec = importlib.util.spec_from_file_location("tabletop", init,
                                                  submodule_search_locations=[str(init.parent)])
    package = importlib.util.module_from_spec(spec)
    sys.modules["tabletop"] = package
    spec.loader.exec_module(package)
    return package


def engine_name() -> dict:
    """The engine that ran: `accel.info()` where the package has the seam, else the plain
    package, named `python` (D-107)."""
    if importlib.util.find_spec("tabletop.accel") is None:
        return {"engine": "python", "seam": False}
    info = importlib.import_module("tabletop.accel").info()
    return {"engine": info.get("engine"), "seam": True, "info": info}


def configured(profile: str, settings: list):
    """The profile's Config with the `--set` fields applied, through the command line's own
    parser (D-066)."""
    cli = importlib.import_module("tabletop.__main__")
    return cli.configure(profile, settings)


def work(problems: list, config, runs: int, seed: int) -> tuple:
    """Run every problem `runs` times and digest the rows (D-107). Returns the digest, the
    number of runs and the codelets run in all."""
    harness = importlib.import_module("tabletop.bench.harness")
    # the batch seeds through the seam where it exists (the chosen generator, D-109), else the
    # plain package's generator
    rng = importlib.import_module("tabletop.accel" if importlib.util.find_spec("tabletop.accel")
                                  else "tabletop.rng")
    run = importlib.import_module("tabletop.run")
    load_problem = importlib.import_module("tabletop.problems").load_problem
    digest = hashlib.sha256()
    count = codelets = 0
    for name in problems:
        problem = load_problem(name)
        for s in rng.run_seeds(harness.batch_seed_for(seed, name), runs):
            r = run.run_problem(problem, config, s)
            digest.update(repr((name, s, r.answer, r.structure_value, r.codelets,
                                bool(r.capped), r.draws)).encode())
            count += 1
            codelets += r.codelets
    return digest.hexdigest(), count, codelets


def main(argv=None) -> int:
    args = parser().parse_args(argv)
    engine_dir = Path(args.engine).resolve()
    load_package(engine_dir)
    families = importlib.import_module("tabletop.bench.families")
    problems = families.problems_of(only=args.problems)
    if args.only is not None:
        problems = problems[:args.only]
    config = configured(args.profile, args.set)
    try:
        load1 = os.getloadavg()[0]
    except OSError:
        load1 = None
    cpu0, wall0 = time.process_time(), time.perf_counter()
    if args.cprofile:
        import cProfile
        profiler = cProfile.Profile()
        profiler.enable()
        digest, count, codelets = work(problems, config, args.runs, args.seed)
        profiler.disable()
        profiler.dump_stats(args.cprofile)
    else:
        digest, count, codelets = work(problems, config, args.runs, args.seed)
    cpu, wall = time.process_time() - cpu0, time.perf_counter() - wall0
    record = {"interpreter": f"{platform.python_implementation()} {platform.python_version()}",
              "engine_dir": str(engine_dir), **engine_name(),
              "profile": args.profile, "set": list(args.set), "seed": args.seed,
              "problems": len(problems), "runs": args.runs, "count": count,
              "codelets": codelets, "cpu": round(cpu, 3), "wall": round(wall, 3),
              "maxrss_mb": round(resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1024, 1),
              "load1": load1, "date": time.strftime("%Y-%m-%d %H:%M:%S"), "digest": digest}
    print(json.dumps(record))
    return 0


if __name__ == "__main__":
    sys.exit(main())

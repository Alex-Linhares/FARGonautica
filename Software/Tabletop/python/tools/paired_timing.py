#!/usr/bin/env python3
"""Paired timing of engines (D-107): every candidate runs `tools/workload.py` at the same
time as the baseline, each in a fresh process pinned to one of two cores (`taskset`), the
two cores swapped between repeats, at `nice -n 10`. The ratio baseline / candidate of CPU
time (`process_time`) and of wall time is taken per repeat; the medians and ranges over the
repeats are the result. Every run also reports its rows digest, so the output says whether
the candidate's runs are identical to the baseline's.

    python3 tools/paired_timing.py --baseline PYTHON:DIR --candidate NAME=PYTHON:DIR ...
        --cores A,B [--runs N] [--repeats R] [--profile NAME] [--set FIELD=VALUE] [--only N]
        [--problems NAME ...] [--out FILE]

A baseline or candidate is `PYTHON:DIR`, an interpreter and the folder holding the
`tabletop` package it runs, with any number of `@KEY=VALUE` suffixes setting environment
variables for that process alone (`@TABLETOP_ENGINE=python` asks the pure engine of a
package that has the seam). Both interpreters and both folders are arguments: nothing here
names a path outside this folder. The workload script is this folder's, whichever package
it times. Exit status 0 when every pair ran, 1 when a process failed, 2 on a usage error.
"""

import argparse
import json
import os
import shutil
import statistics
import subprocess
import sys
import time
from pathlib import Path

WORKLOAD = Path(__file__).resolve().with_name("workload.py")
USAGE_ERROR = 2


def parser() -> argparse.ArgumentParser:
    """The options (D-107)."""
    p = argparse.ArgumentParser(prog="python3 tools/paired_timing.py",
                                description="Paired CPU and wall timing of engines on two cores.")
    p.add_argument("--baseline", required=True, metavar="PYTHON:DIR[@KEY=VALUE...]",
                   help="the interpreter and package folder every candidate is paired with")
    p.add_argument("--candidate", action="append", default=[], required=True,
                   metavar="NAME=PYTHON:DIR[@KEY=VALUE...]", help="a candidate (repeatable)")
    p.add_argument("--cores", required=True, metavar="A,B",
                   help="the two cores the pair runs on (taskset), swapped between repeats")
    p.add_argument("--runs", type=int, default=20, help="runs per problem (default 20)")
    p.add_argument("--repeats", type=int, default=3, help="repeats per candidate (default 3)")
    p.add_argument("--profile", default="standard", help="the configuration profile")
    p.add_argument("--set", action="append", default=[], metavar="FIELD=VALUE",
                   help="a Config field to change, passed to the workload")
    p.add_argument("--only", type=int, metavar="N", help="only the first N problems")
    p.add_argument("--problems", nargs="+", metavar="NAME", help="only these problems")
    p.add_argument("--out", metavar="FILE", help="write the JSON record here")
    p.add_argument("--no-pin", action="store_true",
                   help="run without taskset and nice (for a quick test, never for a number)")
    return p


def parse_spec(text: str) -> dict:
    """`PYTHON:DIR[@KEY=VALUE...]` as a dict: python, engine, env (D-107)."""
    head, *suffixes = text.split("@")
    if ":" not in head:
        raise SystemExit(f"{text!r}: wanted PYTHON:DIR, an interpreter and a package folder")
    python, engine = head.split(":", 1)
    env = {}
    for suffix in suffixes:
        if "=" not in suffix:
            raise SystemExit(f"{text!r}: an environment suffix wants @KEY=VALUE")
        key, value = suffix.split("=", 1)
        env[key] = value
    return {"python": python, "engine": str(Path(engine).resolve()), "env": env}


def parse_cores(text: str) -> tuple:
    """`A,B` as two core numbers (D-107)."""
    parts = text.split(",")
    if len(parts) != 2 or not all(p.strip().isdigit() for p in parts):
        raise SystemExit(f"--cores wants two core numbers, A,B, not {text!r}")
    return parts[0].strip(), parts[1].strip()


def command(spec: dict, core: str, args) -> list:
    """The command of one timed process (D-107): `nice -n 10 taskset -c CORE PYTHON -I
    workload.py ...` (isolated mode: no site, no PYTHONPATH, so the folder given is the
    engine timed; CPython and PyPy both take `-I`)."""
    cmd = [spec["python"], "-I", str(WORKLOAD), "--engine", spec["engine"],
           "--runs", str(args.runs), "--profile", args.profile]
    for setting in args.set:
        cmd += ["--set", setting]
    if args.only is not None:
        cmd += ["--only", str(args.only)]
    if args.problems:
        cmd += ["--problems", *args.problems]
    if not args.no_pin:
        cmd = ["nice", "-n", "10", "taskset", "-c", core, *cmd]
    return cmd


def launch(spec: dict, core: str, args) -> subprocess.Popen:
    """Start one timed process, its environment the parent's plus the spec's (D-107)."""
    env = dict(os.environ, **spec["env"])
    return subprocess.Popen(command(spec, core, args), stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE, text=True, env=env, cwd=str(Path.home()))


def finish(proc: subprocess.Popen, what: str) -> dict:
    """The JSON line a timed process printed, or a SystemExit naming what failed (D-107)."""
    out, err = proc.communicate()
    if proc.returncode != 0:
        raise SystemExit(f"{what} failed (exit {proc.returncode}):\n{err.strip()[-2000:]}")
    return json.loads(out.strip().splitlines()[-1])


def pair(name: str, base: dict, cand: dict, cores: tuple, args, say=print) -> dict:
    """Time one candidate against the baseline over the repeats (D-107)."""
    rows = []
    for r in range(args.repeats):
        core_b, core_c = cores if r % 2 == 0 else cores[::-1]
        try:
            load1 = os.getloadavg()[0]
        except OSError:
            load1 = None
        pb = launch(base, core_b, args)
        pc = launch(cand, core_c, args)
        ob = finish(pb, "the baseline")
        oc = finish(pc, f"candidate {name}")
        row = {"repeat": r, "cores": [core_b, core_c], "load1": load1, "base": ob, "cand": oc,
               "cpu_ratio": ob["cpu"] / oc["cpu"] if oc["cpu"] else None,
               "wall_ratio": ob["wall"] / oc["wall"] if oc["wall"] else None,
               "identical": ob["digest"] == oc["digest"]}
        rows.append(row)
        say(f"{name} repeat {r}: base cpu {ob['cpu']:.1f} s, cand cpu {oc['cpu']:.1f} s, "
            f"x{row['cpu_ratio']:.2f} cpu, x{row['wall_ratio']:.2f} wall, "
            f"identical={row['identical']}, load {load1}")
    cpu = [x["cpu_ratio"] for x in rows]
    wall = [x["wall_ratio"] for x in rows]
    return {"name": name, "python": cand["python"], "engine": cand["engine"], "env": cand["env"],
            "candidate_engine": rows[0]["cand"].get("engine"),
            "baseline_engine": rows[0]["base"].get("engine"), "rows": rows,
            "cpu_median": statistics.median(cpu), "cpu_range": [min(cpu), max(cpu)],
            "wall_median": statistics.median(wall), "wall_range": [min(wall), max(wall)],
            "all_identical": all(x["identical"] for x in rows)}


def summary_line(d: dict) -> str:
    """One line per candidate: the medians and ranges and the identity verdict (D-107)."""
    return (f"{d['name']:12s} cpu x{d['cpu_median']:.2f} [{d['cpu_range'][0]:.2f}-"
            f"{d['cpu_range'][1]:.2f}]  wall x{d['wall_median']:.2f} [{d['wall_range'][0]:.2f}-"
            f"{d['wall_range'][1]:.2f}]  identical={d['all_identical']}")


def main(argv=None) -> int:
    args = parser().parse_args(argv)
    if not args.no_pin and not (shutil.which("taskset") and shutil.which("nice")):
        print("paired_timing: taskset and nice are needed to pin the pair (or --no-pin for "
              "a test that is not a number)", file=sys.stderr)
        return USAGE_ERROR
    cores = parse_cores(args.cores)
    base = parse_spec(args.baseline)
    cands = []
    for text in args.candidate:
        if "=" not in text.split("@")[0]:
            print(f"paired_timing: --candidate wants NAME=PYTHON:DIR, not {text!r}", file=sys.stderr)
            return USAGE_ERROR
        name, spec = text.split("=", 1)
        cands.append((name, parse_spec(spec)))
    record = {"date": time.strftime("%Y-%m-%d %H:%M:%S"), "runs": args.runs,
              "repeats": args.repeats, "cores": list(cores), "profile": args.profile,
              "set": list(args.set), "only": args.only, "problems": args.problems,
              "pinned": not args.no_pin, "baseline": base, "pairs": []}
    try:
        for name, cand in cands:
            record["pairs"].append(pair(name, base, cand, cores, args))
            if args.out:
                Path(args.out).write_text(json.dumps(record, indent=1) + "\n")
    except SystemExit as e:
        print(f"paired_timing: {e}", file=sys.stderr)
        return 1
    for d in record["pairs"]:
        print(summary_line(d))
    return 0


if __name__ == "__main__":
    sys.exit(main())

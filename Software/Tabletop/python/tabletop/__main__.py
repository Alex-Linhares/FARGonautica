"""The command line, `python3 -m tabletop`: one run with its text trace, a batch, the
JSON-lines trace, the problem list and the table (D-064 to D-066).

    python3 -m tabletop surround-05 --seed 3                 # one run, one line per codelet
    python3 -m tabletop surround-05 --seed 3 --quiet         # the first and last lines only
    python3 -m tabletop surround-05 --seed 3 --json run.jsonl   # the JSON-lines trace too
    python3 -m tabletop surround-05 --seed 3 --json -        # the JSON lines on stdout, alone
    python3 -m tabletop surround-05 --profile french-faults  # another profile
    python3 -m tabletop surround-05 --set rho=2 --set initial_temperature=100
    python3 -m tabletop surround-05 --batch 50 --seed 1      # fifty runs, a summary table
    python3 -m tabletop surround-05 --table                  # the layout, then the run
    python3 -m tabletop --list                               # the problems

A run is reproducible from its problem, seed and profile: the generator is seeded once
(D-064), and "Almost all of the configurations were run fifty times." [F95 p. 114] is a
batch of fifty seeds drawn from the one `--seed`. The problem names are the file stems of
`problems/` (`surround-05`, `blockage-06`, `buridan-01`, `figure-ground-01`, `ch2-fig11`).
"""

import argparse
import ast
import dataclasses
import json
import sys

from tabletop import geometry as G
from tabletop import observe as OB
from tabletop.config import PROFILES, Config
from tabletop.problems import catalogue, load_problem
from tabletop.accel import ENGINE, run_seeds
from tabletop.run import report_line, run_problem

USAGE_ERROR = 2


def parser() -> argparse.ArgumentParser:
    """The options (D-064 to D-066)."""
    p = argparse.ArgumentParser(prog="python3 -m tabletop",
                                description="Run French's Tabletop (a reconstruction) on a "
                                            "digitized configuration.")
    p.add_argument("problem", nargs="?", help="a problem name, e.g. surround-05 (see --list)")
    p.add_argument("--list", action="store_true", help="list the problems and exit")
    p.add_argument("--seed", type=int, default=1, help="the run's seed, or the batch's (default 1)")
    p.add_argument("--profile", default="standard", choices=sorted(PROFILES),
                   help="the configuration profile (default standard)")
    p.add_argument("--set", action="append", default=[], metavar="FIELD=VALUE",
                   help="override one Config field (repeatable); the run reports it as changed")
    p.add_argument("--quiet", action="store_true", help="print only the first and last lines")
    p.add_argument("--json", metavar="FILE", help="write the JSON-lines trace to FILE ('-' for "
                   "stdout, which then carries nothing else); in a batch, one Result per line")
    p.add_argument("--batch", type=int, metavar="N", help="run N seeds drawn from --seed and "
                   "print a summary table instead of traces")
    p.add_argument("--table", action="store_true", help="print the table's layout first")
    return p


def parse_value(text: str):
    """A `--set` value: a Python literal (`2`, `0.5`, `True`, `'x'`, `(1, 2)`) or, failing
    that, the string itself (D-066)."""
    try:
        return ast.literal_eval(text)
    except (ValueError, SyntaxError):
        return text


def configure(profile: str, settings: list) -> Config:
    """The profile's `Config` with every `--set FIELD=VALUE` applied (D-066); a `ValueError`
    names an unknown field, a malformed setting or a value of the wrong type."""
    config = PROFILES[profile]
    names = {f.name for f in dataclasses.fields(Config)}
    changes = {}
    for setting in settings:
        if "=" not in setting:
            raise ValueError(f"--set wants FIELD=VALUE, not {setting!r}")
        name, text = setting.split("=", 1)
        name = name.strip()
        if name not in names or name == "profile":
            raise ValueError(f"--set: no Config field named {name!r}")
        value = parse_value(text.strip())
        current = getattr(config, name)
        if isinstance(current, bool) or isinstance(value, bool):
            ok = isinstance(value, bool)
        elif isinstance(current, (int, float)):
            ok = isinstance(value, (int, float))
            value = type(current)(value) if ok and not isinstance(value, type(current)) else value
        else:
            ok = isinstance(value, type(current))
        if not ok:
            raise ValueError(f"--set {name}: wants a {type(current).__name__}, not {value!r}")
        changes[name] = value
    return dataclasses.replace(config, **changes) if changes else config


def list_problems(out) -> None:
    """One line per problem: name, family, objects, the touched object, the labelled answers
    or French's expected answer, and the figure with its page (D-003)."""
    for name in catalogue():
        problem = load_problem(name)
        first = problem.objects[0].provenance
        answers = " ".join(f"{k}={v}" for k, v in problem.answers.items())
        if not answers and problem.expected:
            answers = "expected=" + str(problem.expected.get("answer") or "(open)")
        print(f"{name:16} {problem.family:13} {len(problem.objects):2d} objects  "
              f"touched {problem.touched:12} {answers:28} {first.figure}, p. {first.page}",
              file=out)


def table_text(problem) -> list:
    """The layout as text (D-001): a map of the table, Henry's side at the top, each object
    a letter in file order, then a legend with id, type, position, orientation and the
    exact region; the touched object is marked with `*` and the labelled answers with
    their labels [F95 p. 114]."""
    columns, rows = 55, 20
    grid = [[" "] * columns for _ in range(rows)]
    letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    lines = ["Henry".center(columns + 2), "+" + "-" * columns + "+"]
    for i, obj in enumerate(problem.objects):
        col = min(columns - 1, int(obj.x / G.TABLE_WIDTH * columns))
        row = min(rows - 1, int(obj.y / G.TABLE_HEIGHT * rows))
        grid[row][col] = letters[i % len(letters)]
    lines += ["|" + "".join(r) + "|" for r in grid]
    lines += ["+" + "-" * columns + "+", "Eliza".center(columns + 2)]
    labels = {v: k for k, v in problem.answers.items()}
    for i, obj in enumerate(problem.objects):
        mark = " *touched" if obj.id == problem.touched else ""
        mark += f" {labels[obj.id]}" if obj.id in labels else ""
        orientation = f" {obj.orientation}" if obj.orientation else ""
        lines.append(f"  {letters[i % len(letters)]}  {obj.id:14} {obj.type:12}"
                     f"({obj.x:5.1f}, {obj.y:5.1f}){orientation:7} "
                     f"{G.exact_region(obj.x, obj.y).name}{mark}")
    return lines


def run_one(problem, config, seed: int, quiet: bool, json_target, out) -> int:
    """One run with the text trace on `out` and, if asked, the JSON-lines trace (D-065)."""
    observers, handle = [], None
    if json_target == "-":
        observers.append(OB.JsonLines(out))
    else:
        if json_target:
            handle = open(json_target, "w", encoding="utf-8")
            observers.append(OB.JsonLines(handle))
        observers.append(OB.TextTrace(out, quiet=quiet))
    try:
        result = run_problem(problem, config, seed, observers)
    finally:
        if handle is not None:
            handle.close()
    return 1 if result.observer_errors else 0


def summarize(results, problem) -> list:
    """A batch's summary [F95 p. 114]: for each answer, in the order first given, its
    label, the runs, the percentage, the mean structure value and the mean codelets; then
    the totals."""
    order, by_answer = [], {}
    for r in results:
        if r.answer not in by_answer:
            order.append(r.answer)
            by_answer[r.answer] = []
        by_answer[r.answer].append(r)
    lines = [f"{'answer':16} {'label':6} {'runs':>5} {'%':>6} {'structure':>10} {'codelets':>9}"]
    for answer in order:
        rs = by_answer[answer]
        label = rs[0].label or "-"
        lines.append(f"{answer:16} {label:6} {len(rs):5d} {100.0 * len(rs) / len(results):6.1f} "
                     f"{sum(r.structure_value for r in rs) / len(rs):10.1f} "
                     f"{sum(r.codelets for r in rs) / len(rs):9.1f}")
    capped = sum(r.capped for r in results)
    lines.append(f"{len(results)} runs of {problem.name}, {capped} capped, "
                 f"mean codelets {sum(r.codelets for r in results) / len(results):.1f}")
    return lines


def run_batch_cli(problem, config, seed: int, count: int, json_target, out) -> int:
    """`count` runs from the seeds of the batch (D-064), the summary table on `out`, and,
    if asked, one `Result` record per run as JSON lines."""
    if json_target != "-":
        print(f"engine: {ENGINE}", file=out)      # every output names its engine (D-107)
    results = [run_problem(problem, config, s) for s in run_seeds(seed, count)]
    if json_target:
        handle = out if json_target == "-" else open(json_target, "w", encoding="utf-8")
        try:
            for r in results:
                handle.write(json.dumps(dataclasses.asdict(r), separators=(",", ":")) + "\n")
        finally:
            if handle is not out:
                handle.close()
    if json_target != "-":
        for line in summarize(results, problem):
            print(line, file=out)
    return 0


def main(argv=None) -> int:
    """Parse the arguments and run; 0 on success, 2 on a usage error (D-064)."""
    args = parser().parse_args(sys.argv[1:] if argv is None else list(argv))
    out = sys.stdout
    if args.list:
        list_problems(out)
        return 0
    if not args.problem:
        parser().print_usage(sys.stderr)
        print("tabletop: a problem name is needed (or --list)", file=sys.stderr)
        return USAGE_ERROR
    try:
        problem = load_problem(args.problem)
    except (FileNotFoundError, ValueError) as error:
        print(f"tabletop: cannot load {args.problem!r}: {error}", file=sys.stderr)
        return USAGE_ERROR
    try:
        config = configure(args.profile, args.set)
    except ValueError as error:
        print(f"tabletop: {error}", file=sys.stderr)
        return USAGE_ERROR
    if args.batch is not None and args.batch < 1:
        print("tabletop: --batch wants a positive count", file=sys.stderr)
        return USAGE_ERROR
    if args.table and args.json != "-":
        for line in table_text(problem):
            print(line, file=out)
    if args.batch:
        return run_batch_cli(problem, config, args.seed, args.batch, args.json, out)
    return run_one(problem, config, args.seed, args.quiet, args.json, out)


if __name__ == "__main__":
    status = main()
    try:
        sys.stdout.flush()
    except BrokenPipeError:   # the reader went away (`| head`): leave quietly
        import os
        os.dup2(os.open(os.devnull, os.O_WRONLY), sys.stdout.fileno())
    sys.exit(status)

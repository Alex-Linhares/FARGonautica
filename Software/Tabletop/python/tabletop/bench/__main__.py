"""The benches' command line, `python3 -m tabletop.bench NAME` (D-072, D-073):

    python3 -m tabletop.bench families                      # the 35 chapter 5 sets, 500 runs each
    python3 -m tabletop.bench families --runs 50            # French's fifty [F95 p. 114]
    python3 -m tabletop.bench fcca --profile french-faults  # FCCA's 18 sets, another profile
    python3 -m tabletop.bench families --problems surround-05 blockage-06 --runs 20
    python3 -m tabletop.bench families --json families.json --quiet   # the records, no progress
    python3 -m tabletop.bench humans | ch2 | ch4 | focus | rho | lesions | faults | potelbat | stated | temperature | robustness
    python3 -m tabletop.bench variants --runs 100             # eight variants of every problem, paired with the original (D-102)
    python3 -m tabletop.bench churn --runs 200                # roles over time and the role codelets before the answer (D-106)
    python3 -m tabletop.bench all --out bench-results       # every bench: NAME.md and NAME.json
    python3 -m tabletop.bench --list                        # the benches

Every bench is deterministic for its `--seed` (D-064): each problem's batch seed comes
from the bench seed and the problem's name, and `--json` records every run's seed. The
Markdown tables go to stdout, one progress line per survey to stderr (unless `--quiet`),
and the JSON records to the file named by `--json` (`-` for stdout, which then carries
the JSON alone). `all` writes every bench's tables and records under `--out` (default
`bench-results/`), the files RESULTS.md is built from. Usage errors exit 2.

Each bench module offers the same five names: `SETS`, `problems_of(sets, only)`,
`planned(sets, only)`, `run(config, seed, runs, only, progress)`, `report(results)` and
`records(results)` (D-073).
"""

import argparse
import json
import sys
from pathlib import Path

from tabletop import accel
from tabletop.bench import ch2, ch4, churn, families, faults, fcca, focus, humans, lesions
from tabletop.bench import potelbat, rho, robustness, stated, temperature, variants
from tabletop.config import PROFILES
from tabletop.problems import catalogue

USAGE_ERROR = 2
DEFAULT_RUNS = 500
DEFAULT_OUT = "bench-results"

BENCHES = {
    "families": (families, "the 35 chapter 5 result sets [F95 pp. 115-139]"),
    "fcca": (fcca, "FCCA's 18 reprinted sets, the two divergent ones against both versions "
                   "[FCCA pp. 394-399]"),
    "humans": (humans, "Tables 5.1-5.3 and Figure 5.4: the subjects, Tabletop's second runs, "
                       "ours under two profiles [F95 pp. 143-144]"),
    "ch2": (ch2, "chapter 2's nineteen configurations against French's words [F95 pp. 26-35]"),
    "ch4": (ch4, "the chapter 4 run many times: temperature, rack shares and milestones "
                 "against the dumps [F95 pp. 96-107]"),
    "focus": (focus, "Surround 5 against Surround 10, and junk scaling [F95 p. 121]"),
    "rho": (rho, "the urgency parameter rho from 0.5 to 6 [F95 p. 82]"),
    "lesions": (lesions, "FCCA's criterion 11: seven lesions of the architecture [FCCA p. 365]"),
    "faults": (faults, "French's seven faults restored one at a time and together "
                       "(ANSWERS.md 2)"),
    "potelbat": (potelbat, "the brute-force baseline's answers on every problem [FCCA p. 328]"),
    "stated": (stated, "French's statements of what the program does as yes-or-no checks "
                       "over the runs, the chapter 4 order and chapter 2's series (M2)"),
    "temperature": (temperature, "the course of T over a run: the curve, the first change, "
                                 "the end, the three terms; two alternative readings "
                                 "[F95 pp. 90-93, 96-107]"),
    "robustness": (robustness, "random realistic tables with every invariant checked after "
                               "every codelet, the capped runs and a speed profile "
                               "(D-068, D-077)"),
    "variants": (variants, "eight variants of every problem (mirrored, side-swapped, rotated, "
                           "translated, type-substituted, junk-added, decoupled, on-saucer) "
                           "run with the original's seeds: the distance between the "
                           "relabelled answer distributions (D-102)"),
    "churn": (churn, "roles over time on the four carriers: role changes per run, by reason "
                     "and by the temperature at the change, and the role codelets run before "
                     "the answer against the find-answer flood (D-106)"),
}
ALL = "all"


def parser() -> argparse.ArgumentParser:
    """The options (D-072, D-073)."""
    p = argparse.ArgumentParser(prog="python3 -m tabletop.bench",
                                description="Replay French's experiments and print our numbers "
                                            "beside his.")
    p.add_argument("bench", nargs="?", choices=sorted(BENCHES) + [ALL],
                   help="which bench (see --list), or 'all'")
    p.add_argument("--list", action="store_true", help="list the benches and exit")
    p.add_argument("--runs", type=int, default=DEFAULT_RUNS,
                   help=f"runs per problem (default {DEFAULT_RUNS}; French ran about 50)")
    p.add_argument("--seed", type=int, default=1, help="the bench's seed (default 1)")
    p.add_argument("--profile", default="standard", choices=sorted(PROFILES),
                   help="the configuration profile (default standard)")
    p.add_argument("--problems", nargs="+", metavar="NAME",
                   help="only these problems (file stems of problems/)")
    p.add_argument("--json", metavar="FILE",
                   help="write one JSON record per comparison to FILE ('-' for stdout)")
    p.add_argument("--out", metavar="DIR", default=DEFAULT_OUT,
                   help=f"for 'all': the directory for NAME.md and NAME.json (default "
                        f"{DEFAULT_OUT}/)")
    p.add_argument("--quiet", action="store_true", help="no progress lines on stderr")
    return p


def progress_printer(total: int, quiet: bool):
    """One line per finished survey on stderr, unless quiet (D-072)."""
    done = [0]

    def show(sv) -> None:
        done[0] += 1
        if not quiet:
            top = max(sv.tallies, key=lambda t: t.count)
            print(f"[{done[0]}/{total}] {sv.problem} ({sv.profile}): {sv.runs} runs, "
                  f"{top.answer} {top.frequency:.0f}%, mean codelets {sv.codelets:.1f}",
                  file=sys.stderr)
    return show


def write_json(records: list, handle) -> None:
    """One compact JSON record per line (D-072)."""
    for r in records:
        handle.write(json.dumps(r, separators=(",", ":")) + "\n")


def run_bench(name: str, args) -> tuple:
    """Run one bench under the parsed arguments: (report lines, records) (D-073)."""
    module = BENCHES[name][0]
    only = args.problems
    if only is not None and name == ALL:
        only = None
    total = module.planned(module.SETS, only)
    results = module.run(PROFILES[args.profile], args.seed, args.runs, only,
                         progress_printer(total, args.quiet))
    records = module.records(results)
    for r in records:                 # every record names the engine that ran it (D-107)
        r["engine"] = accel.info()
    return module.report(results), records


def run_all(args) -> int:
    """Every bench in turn, each written as NAME.md and NAME.json under `--out`
    (D-073). `--problems` is ignored, since the benches' problem sets differ."""
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    for name in BENCHES:
        if not args.quiet:
            print(f"== {name}", file=sys.stderr)
        lines, records = run_bench(name, args)
        (out / f"{name}.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
        with open(out / f"{name}.json", "w", encoding="utf-8") as handle:
            write_json(records, handle)
        print(f"{name}: {out / f'{name}.md'}, {out / f'{name}.json'}")
    return 0


def main(argv=None) -> int:
    """Parse the arguments and run one bench, or all; 0 on success, 2 on a usage error
    (D-072)."""
    args = parser().parse_args(sys.argv[1:] if argv is None else list(argv))
    if args.list:
        for name, (_, what) in BENCHES.items():
            print(f"{name:10} {what}")
        print(f"{ALL:10} every bench, written under --out")
        return 0
    if not args.bench:
        parser().print_usage(sys.stderr)
        print("tabletop.bench: a bench name is needed (or --list)", file=sys.stderr)
        return USAGE_ERROR
    if args.runs < 1:
        print("tabletop.bench: --runs wants a positive count", file=sys.stderr)
        return USAGE_ERROR
    # The first line of every bench's output names the engine (D-107); on stderr when the
    # standard output is the JSON.
    print(f"engine: {accel.info()['engine']}", file=sys.stderr if args.json == "-" else sys.stdout)
    if args.bench == ALL:
        return run_all(args)
    module = BENCHES[args.bench][0]
    if args.problems:
        unknown = [n for n in args.problems if n not in catalogue()]
        if unknown:
            print(f"tabletop.bench: no such problem: {' '.join(unknown)}", file=sys.stderr)
            return USAGE_ERROR
        missing = [n for n in args.problems if n not in module.problems_of(module.SETS)]
        if missing:
            print(f"tabletop.bench: not in the {args.bench} bench: {' '.join(missing)}",
                  file=sys.stderr)
            return USAGE_ERROR
    lines, records = run_bench(args.bench, args)
    if args.json != "-":
        for line in lines:
            print(line)
    if args.json:
        handle = sys.stdout if args.json == "-" else open(args.json, "w", encoding="utf-8")
        try:
            write_json(records, handle)
        finally:
            if handle is not sys.stdout:
                handle.close()
    return 0


if __name__ == "__main__":
    sys.exit(main())

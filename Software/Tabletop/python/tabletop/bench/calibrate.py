"""French's two calibrations, done after the freeze and reported apart (ANSWERS.md 7; D-076).

    python3 -m tabletop.bench.calibrate human --runs 200 --seed 1 --split-seed 1 --json FILE
    python3 -m tabletop.bench.calibrate buridan --runs 500 --seed 1 --json FILE

French calibrated his program twice, and only twice. For the human comparison he "slightly
increased the relative importance that the program attached to object-type matches compared
to relational structure in the evaluation of good correspondences" [F95 p. 143] before the
second runs of Tables 5.1-5.3; for Buridan 2-13 he raised the penalty for incoherent
mappings, "when the value place on good, coherent structure is increased", until Eliza's
choice on Buridan 1 was "precisely poised, like Buridan's ass between two equidistant bales
of hay, between the spoon and the fork on her side" [F95 p. 130]. The `human` profile
changes `Config.type_weight` (D-036) and the `buridan` profile `Config.incoherence_penalty`
(D-042), nothing else; `standard` is never touched.

Each fit is a scan over a grid of values fixed before any run (D-076), one survey per
problem and value with the bench's per-problem batch seeds (D-072), and the value that
minimizes the objective is chosen, the smallest value among ties. `human` is fitted by
least squares to the subjects' two columns on a random half of the 23 problems of Tables
5.1-5.3, the half drawn from a recorded seed, and is evaluated on the other half and
against Tabletop's second-run columns (`humans` bench, RESULTS.md §9); `buridan` is fitted
to Buridan 1 under the modified program, the balance between E1 and E2, and is evaluated on
Buridan 2-13 (`families` bench under the profile). The records this module writes hold the
whole scan, so that RESULTS.md shows every value tried, not only the one chosen.
"""

import argparse
import dataclasses
import json
import multiprocessing
import sys
from concurrent.futures import ProcessPoolExecutor

from tabletop import reported as RP
from tabletop.bench import harness as H
from tabletop.bench import humans as HM
from tabletop.bench import probes as P
from tabletop.config import PROFILES, Config
from tabletop.problems import load_problem
from tabletop import accel
from tabletop.accel import Rng

# DECISION D-076: the grids, fixed before any run. The default is the first value of each,
# so that "no change" is a possible outcome of the fit: the v2 default; since v3's defaults
# (D-114) the grids stand as fixed and the scans are a measurement, never a re-fit.
HUMAN_GRID = (30, 40, 50, 60, 80, 100, 150)          # `type_weight` (D-036)
BURIDAN_GRID = (40, 60, 80, 100, 150, 200, 300)      # `incoherence_penalty` (D-042)
BURIDAN_PROBLEM = "buridan-01"                       # "Buridan 1 (modified program)" [F95 p. 130]
BURIDAN_PROGRAM = "modified"
SPLIT_SEED = 1                                       # the recorded seed of the human split
FIELDS = {"human": "type_weight", "buridan": "incoherence_penalty"}
GRIDS = {"human": HUMAN_GRID, "buridan": BURIDAN_GRID}


# ---- the split ---------------------------------------------------------------------------

def table_problems(rows=RP.SUBJECT_ROWS) -> list:
    """The 23 problems of Tables 5.1-5.3 in the tables' order [F95 pp. 143-144] (D-076)."""
    return HM.problems_of(rows)


def split(seed: int = SPLIT_SEED, rows=RP.SUBJECT_ROWS) -> tuple:
    """A random half of the tables' problems for fitting and the rest for evaluation
    (ANSWERS.md 7; D-076): the problems shuffled by a Fisher-Yates shuffle through the
    generator seeded with `seed` (D-064), the first twelve of the 23 fitted, the eleven
    others held out. Both halves are returned in the tables' order."""
    names = table_problems(rows)
    rng = Rng(seed)
    order = list(names)
    for i in range(len(order) - 1, 0, -1):
        j = rng.below(i + 1)
        order[i], order[j] = order[j], order[i]
    count = (len(order) + 1) // 2
    chosen = order[:count]
    fit = [n for n in names if n in chosen]
    held = [n for n in names if n not in chosen]
    return fit, held


# ---- one survey, run in a worker -----------------------------------------------------------

def survey_task(profile: str, field: str, value, problem: str, seed: int, runs: int) -> dict:
    """One survey record of `problem` under the profile's configuration with `field` set to
    `value` (D-076); a module-level function so that a process pool can run it."""
    config = dataclasses.replace(PROFILES[profile], **{field: value})
    return H.survey_record(H.survey(load_problem(problem), config, seed, runs))


def start_context():
    """The multiprocessing context of the scans' workers: a fork server where there
    is one, else spawning (D-076, as built in item 30)."""
    method = "forkserver" if "forkserver" in multiprocessing.get_all_start_methods() \
        else "spawn"
    return multiprocessing.get_context(method)


def surveys(profile: str, field: str, grid, problems: list, seed: int, runs: int,
            workers: int = 1, progress=None) -> dict:
    """Every (value, problem) survey of a scan, as records keyed by value then problem, in
    the grid's and the problems' order whatever the number of workers (D-076)."""
    jobs = [(value, name) for value in grid for name in problems]
    out = {value: {} for value in grid}
    if workers <= 1:
        for value, name in jobs:
            out[value][name] = survey_task(profile, field, value, name, seed, runs)
            if progress is not None:
                progress(value, name)
        return out
    # The workers are started by a fork server, not by forking this process: under
    # pytest-xdist the process is multi-threaded and Python warns that forking it may
    # deadlock. The results are the same whatever the start method (tested).
    with ProcessPoolExecutor(max_workers=workers, mp_context=start_context()) as pool:
        futures = [(value, name, pool.submit(survey_task, profile, field, value, name, seed, runs))
                   for value, name in jobs]
        for value, name, future in futures:
            out[value][name] = future.result()
            if progress is not None:
                progress(value, name)
    return out


def frequency(record: dict, ids) -> float:
    """The share of a survey record's runs, in percent, whose answer is one of `ids`
    (D-073, on the record rather than the `Survey`)."""
    return sum(t["frequency"] for t in record["tallies"] if t["answer"] in ids)


# ---- human: least squares against the subjects ---------------------------------------------

def human_rows(records_by_problem: dict, rows=RP.SUBJECT_ROWS) -> list:
    """Each table row among the surveyed problems beside our two percentages (D-076):
    `subjects`, `tabletop` (French's second runs), `ours`, and the Figure 5.4 gap."""
    out = []
    for row in rows:
        if row.problem not in records_by_problem:
            continue
        pb = load_problem(row.problem)
        ids = HM.answer_ids(row, pb)
        rec = records_by_problem[row.problem]
        ours = (frequency(rec, ids[0]), frequency(rec, ids[1]))
        out.append({"problem": row.problem, "table": row.table, "subjects": list(row.subjects),
                    "tabletop": list(row.tabletop), "ours": list(ours),
                    "fit": HM.our_fit(row, ours)})
    return out


def sum_of_squares(rows: list) -> float:
    """The least-squares objective of the `human` fit (D-076): the sum over the rows of
    the squared differences between our two percentages and the subjects' [F95 pp. 143-144]."""
    return sum((r["ours"][i] - r["subjects"][i]) ** 2 for r in rows for i in (0, 1))


def mean_gap(rows: list) -> float:
    """The mean Figure 5.4 gap over the rows [F95 p. 144 (img)] (D-076)."""
    return sum(r["fit"] for r in rows) / len(rows) if rows else 0.0


def within_ten(rows: list) -> int:
    """How many rows lie within ten points of the subjects [F95 p. 144] (D-076)."""
    return sum(r["fit"] <= HM.TEN_POINTS for r in rows)


def human_scan(seed: int, runs: int, grid=HUMAN_GRID, split_seed: int = SPLIT_SEED,
               workers: int = 1, progress=None) -> dict:
    """The `human` fit (D-076): the split, the scan over the grid on the fit half, the
    objective of every value, the value chosen and the survey records."""
    fit, held = split(split_seed)
    recs = surveys("human", "type_weight", grid, fit, seed, runs, workers, progress)
    scan = []
    for value in grid:
        rows = human_rows(recs[value])
        scan.append({"value": value, "sum_of_squares": sum_of_squares(rows),
                     "mean_gap": mean_gap(rows), "within_ten": within_ten(rows),
                     "rows": rows, "surveys": recs[value]})
    return {"bench": "calibrate", "profile": "human", "field": "type_weight",
            "before": PROFILES["standard"].type_weight, "grid": list(grid), "seed": seed,
            "runs": runs, "split_seed": split_seed, "fit_problems": fit,
            "held_out_problems": held, "scan": scan, "chosen": choose(scan, "sum_of_squares")}


def choose(scan: list, objective: str):
    """The grid value with the smallest objective, the smallest value among ties (D-076)."""
    best = min(scan, key=lambda s: (s[objective], s["value"]))
    return best["value"]


# ---- buridan: the balance on Buridan 1 -----------------------------------------------------

def buridan_reading(record: dict, problem) -> dict:
    """Buridan 1's two answers under a penalty (D-076): E1 the spoon, E2 the fork [F95
    p. 130], the rest, the imbalance |E1 - E2| (the objective: "precisely poised ...
    between the spoon and the fork") and the distance from French's 50-50 as the sum of
    the two absolute differences (reported, not fitted)."""
    e1 = frequency(record, [problem.answers["E1"]])
    e2 = frequency(record, [problem.answers["E2"]])
    return {"E1": e1, "E2": e2, "other": 100.0 - e1 - e2, "imbalance": abs(e1 - e2),
            "distance_from_50_50": abs(e1 - 50.0) + abs(e2 - 50.0)}


def buridan_scan(seed: int, runs: int, grid=BURIDAN_GRID, workers: int = 1,
                 progress=None) -> dict:
    """The `buridan` fit (D-076): Buridan 1 surveyed under every penalty of the grid, the
    readings, the penalty chosen (the least imbalance) and the survey records."""
    problem = load_problem(BURIDAN_PROBLEM)
    recs = surveys("buridan", "incoherence_penalty", grid, [BURIDAN_PROBLEM], seed, runs,
                   workers, progress)
    scan = []
    for value in grid:
        rec = recs[value][BURIDAN_PROBLEM]
        reading = buridan_reading(rec, problem)
        reading.update({"value": value, "mean_codelets": rec["mean_codelets"],
                        "surveys": recs[value]})
        scan.append(reading)
    french = RP.result_sets(BURIDAN_PROBLEM)
    target = [rs for rs in french if rs.program == BURIDAN_PROGRAM][0]
    return {"bench": "calibrate", "profile": "buridan", "field": "incoherence_penalty",
            "before": PROFILES["standard"].incoherence_penalty, "grid": list(grid),
            "seed": seed, "runs": runs, "problem": BURIDAN_PROBLEM, "program": BURIDAN_PROGRAM,
            "page": target.page,
            "french": {a.label: a.frequency for a in target.answers},
            "scan": scan, "chosen": choose(scan, "imbalance")}


# ---- the report and the command line -------------------------------------------------------

def report(record: dict) -> list:
    """The scan as a Markdown table with the value chosen (D-076)."""
    if record["profile"] == "human":
        lines = [f"## The `human` fit: `type_weight` on {len(record['fit_problems'])} of "
                 f"{len(record['fit_problems']) + len(record['held_out_problems'])} problems "
                 f"(split seed {record['split_seed']}) [F95 pp. 142-144]", "",
                 "Fitted on: " + ", ".join(record["fit_problems"]) + ".", "",
                 "Held out: " + ", ".join(record["held_out_problems"]) + ".", "",
                 "| `type_weight` | Sum of squares | Mean gap | Within ten | Chosen |",
                 "|---:|---:|---:|---:|---|"]
        for s in record["scan"]:
            lines.append(f"| {s['value']} | {s['sum_of_squares']:.0f} | {s['mean_gap']:.1f} "
                         f"| {s['within_ten']} of {len(s['rows'])} "
                         f"| {'yes' if s['value'] == record['chosen'] else ''} |")
    else:
        fr = record["french"]
        lines = [f"## The `buridan` fit: `incoherence_penalty` on Buridan 1 (modified program) "
                 f"[F95 p. {record['page']}]", "",
                 f"French: E1 {fr['E1']}, E2 {fr['E2']}.", "",
                 "| `incoherence_penalty` | E1 % | E2 % | other % | abs(E1 - E2) | "
                 "Distance from 50-50 | Mean codelets | Chosen |",
                 "|---:|---:|---:|---:|---:|---:|---:|---|"]
        for s in record["scan"]:
            lines.append(f"| {s['value']} | {s['E1']:.1f} | {s['E2']:.1f} | {s['other']:.1f} "
                         f"| {s['imbalance']:.1f} | {s['distance_from_50_50']:.1f} "
                         f"| {s['mean_codelets']:.0f} "
                         f"| {'yes' if s['value'] == record['chosen'] else ''} |")
    lines += ["", f"Chosen: `{record['field']} = {record['chosen']}` (before: "
                  f"{record['before']}); {record['runs']} runs per survey, bench seed "
                  f"{record['seed']}.", ""]
    return lines


def parser() -> argparse.ArgumentParser:
    """(D-076)."""
    p = argparse.ArgumentParser(prog="python3 -m tabletop.bench.calibrate",
                                description=__doc__.split("\n\n")[0])
    p.add_argument("profile", choices=sorted(FIELDS))
    p.add_argument("--runs", type=int, default=200)
    p.add_argument("--seed", type=int, default=1)
    p.add_argument("--split-seed", type=int, default=SPLIT_SEED)
    p.add_argument("--workers", type=int, default=1)
    p.add_argument("--json", default=None, help="write the record as one JSON line (- for stdout)")
    p.add_argument("--quiet", action="store_true")
    return p


def main(argv=None) -> int:
    """Run one fit and print its report; 0 on success, 2 on a usage error (D-076)."""
    args = parser().parse_args(sys.argv[1:] if argv is None else list(argv))
    if args.runs < 1 or args.workers < 1:
        print("--runs and --workers must be at least 1", file=sys.stderr)
        return 2

    def progress(value, name):
        if not args.quiet:
            print(f"{FIELDS[args.profile]} = {value}: {name}", file=sys.stderr)

    # The first line of the output names the engine (D-107); on stderr when the standard
    # output is the JSON.
    print(f"engine: {accel.info()['engine']}", file=sys.stderr if args.json == "-" else sys.stdout)
    if args.profile == "human":
        record = human_scan(args.seed, args.runs, split_seed=args.split_seed,
                            workers=args.workers, progress=progress)
    else:
        record = buridan_scan(args.seed, args.runs, workers=args.workers, progress=progress)
    record["engine"] = accel.info()          # the record names the engine that ran it (D-107)
    text = json.dumps(record, separators=(",", ":"))
    if args.json == "-":
        print(text)
    else:
        print("\n".join(report(record)))
        if args.json:
            with open(args.json, "w", encoding="utf-8") as handle:
                handle.write(text + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())

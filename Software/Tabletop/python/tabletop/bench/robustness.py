"""The `robustness` bench: random realistic tables run with every invariant checked after
every codelet, the share of capped runs, and a speed profile (D-068, D-077).

French ran his program on tables he set by hand [F95 pp. 115-141]; nothing says how it
fared on others. This bench generates `--runs` realistic random tables from the bench
seed (`tabletop.tables`, D-068), runs each once under the profile with `invariants.check`
after every codelet (D-077, the role pool's invariants included), and reports what the
runs did: the tables' sizes and types, the touched types, the answers (literal, on the
other side of the table, of the touched type, by route), the run lengths, the capped runs
(D-061), every violation found, and the role statistics (D-106): the tags read per role,
the tags standing at the end, the changes by reason, the re-casts and their ties, and
consumer C1's proposals (the create-cp codelets posted by propose-role-cp and
recast-role, D-095, D-093).
Then it times plain runs and checked runs of the probe problems on one core, in runs per
second and milliseconds per codelet: a machine-dependent number, recorded for the record,
never compared with French's.
"""

from __future__ import annotations

import platform
import time

from tabletop import events as EV
from tabletop import tables as TB
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.bench.results import quartiles
from tabletop.config import Config
from tabletop.invariants import checked_run, summary
from tabletop.problems import load_problem
from tabletop.accel import run_seeds
from tabletop.run import run_problem

SETS = P.DEFAULT_PROBLEMS          # the speed profile's problems
problems_of = P.problems_of
TABLES_SEED_NAME = "random tables"  # what the tables' batch seed is derived from (D-072)
SPEED_RUNS = 50                    # runs per problem, plain and checked, in the speed profile


C1_CODELETS = ("propose-role-cp", "recast-role")   # the codelets that post C1's create-cp (D-095, D-093)
CREATE_CP = "create-cp"


class RoleStats:
    """An observer that counts a run's role events (D-106): the tags read by role, the
    discards by reason, the re-casts and their ties, and C1's proposals."""

    def __init__(self):
        self.tags: dict = {}
        self.reasons: dict = {}
        self.recasts = 0
        self.ties = 0
        self.proposals = 0

    def on_event(self, e: EV.Event) -> None:
        """Count the event (D-065)."""
        if isinstance(e, EV.RoleTagged):
            self.tags[e.role] = self.tags.get(e.role, 0) + 1
        elif isinstance(e, EV.RoleDiscarded):
            reason = "lost" if e.reason.startswith("lost") else e.reason
            self.reasons[reason] = self.reasons.get(reason, 0) + 1
        elif isinstance(e, EV.RoleRecast):
            self.recasts += 1
            self.ties += e.ambiguous
        elif isinstance(e, EV.CodeletRan) and e.codelet in C1_CODELETS:
            self.proposals += sum(1 for c in e.posted if c == CREATE_CP)


class _NoAnswers:
    """What `harness.tallies_of` needs of a problem for a set of random tables: no
    labelled answers (D-068)."""
    answers: dict = {}


def planned(sets=SETS, only=None) -> int:
    """How many progress lines the bench prints: one for the tables and one per speed
    problem (D-073)."""
    return 1 + len(problems_of(sets, only))


def table_seeds(seed: int, runs: int) -> list:
    """The seeds of the random tables, from the bench seed as a batch (D-072)."""
    return run_seeds(H.batch_seed_for(seed, TABLES_SEED_NAME), runs)


def run_tables(config: Config, seed: int, runs: int, progress=None) -> dict:
    """`runs` random tables, each run once with the invariants checked (D-068, D-077):
    the per-table facts and the results."""
    results, per_table, faults, stats = [], [], {}, []
    seeds = table_seeds(seed, runs)
    for s in seeds:
        table = TB.random_table(s)
        roles = RoleStats()
        result, found = checked_run(table, config, s, [roles])
        results.append(result)
        stats.append(roles)
        counts = summary(found)
        for sentence, n in counts.items():
            faults[sentence] = faults.get(sentence, 0) + n
        touched = table.touched_object()
        answer = table.object(result.answer)
        per_table.append({"seed": s, "objects": len(table.objects), "touched": touched.type,
                          "answer": result.answer, "answer_type": answer.type,
                          "other_side": TB.band_of(answer.y) != TB.band_of(touched.y),
                          "faults": sum(counts.values()), "tags": sum(roles.tags.values())})
    if progress is not None:
        progress(H.Survey(f"{runs} random tables", config.profile, seed,
                          H.batch_seed_for(seed, TABLES_SEED_NAME), runs,
                          H.tallies_of(results, _NoAnswers()), sum(r.capped for r in results),
                          sum(r.codelets for r in results) / runs, ()))
    return {"seeds": seeds, "results": results, "per_table": per_table, "faults": faults,
            "roles": roles_summary(stats)}


def roles_summary(stats: list) -> dict:
    """The role statistics over the tables (D-106), per run: the tags read by role and in
    all, the tags standing at the end (read less discarded), the discards by reason, the
    runs with a re-cast and with a tie, and C1's proposals."""
    n = len(stats) or 1
    by_role: dict = {}
    reasons: dict = {}
    for s in stats:
        for role, count in s.tags.items():
            by_role[role] = by_role.get(role, 0) + count
        for reason, count in s.reasons.items():
            reasons[reason] = reasons.get(reason, 0) + count
    read = sum(by_role.values())
    return {"tags_per_run": round(read / n, 2),
            "standing_per_run": round((read - sum(reasons.values())) / n, 2),
            "by_role": {k: round(v / n, 2) for k, v in sorted(by_role.items())},
            "runs_without_a_tag": sum(not s.tags for s in stats),
            "changes_per_run": round(sum(reasons.values()) / n, 2),
            "reasons": {k: round(v / n, 2) for k, v in sorted(reasons.items())},
            "runs_with_a_change": sum(bool(s.reasons) for s in stats),
            "recast_runs": sum(s.recasts > 0 for s in stats),
            "tie_runs": sum(s.ties > 0 for s in stats),
            "c1_proposals_per_run": round(sum(s.proposals for s in stats) / n, 2),
            "runs_with_a_proposal": sum(s.proposals > 0 for s in stats)}


def _count(items) -> dict:
    out: dict = {}
    for it in items:
        out[it] = out.get(it, 0) + 1
    return out


def tables_summary(config: Config, seed: int, tables: dict) -> dict:
    """The random tables' runs summed up (D-068, D-077)."""
    results, per = tables["results"], tables["per_table"]
    n = len(results)
    sizes = [t["objects"] for t in per]
    codelets = [r.codelets for r in results]
    types: dict = {}
    for s in tables["seeds"]:
        for o in TB.random_table(s).objects:
            types[o.type] = types.get(o.type, 0) + 1
    low, median, high = quartiles(codelets)
    return {"bench": "robustness", "kind": "tables", "profile": config.profile, "seed": seed,
            "batch_seed": H.batch_seed_for(seed, TABLES_SEED_NAME), "tables": n,
            "objects": {"min": min(sizes), "mean": sum(sizes) / n, "max": max(sizes)},
            "types": types, "touched": _count(t["touched"] for t in per),
            "answers": {"literal": sum(r.literal for r in results),
                        "other_side": sum(t["other_side"] for t in per),
                        "same_type": sum(t["answer_type"] == t["touched"] for t in per),
                        "through": _count(r.through for r in results)},
            "codelets": {"min": min(codelets), "quartiles": [low, median, high],
                         "mean": sum(codelets) / n, "max": max(codelets)},
            "capped": sum(r.capped for r in results),
            "groups_mean": sum(r.groups for r in results) / n,
            "correspondences_mean": sum(r.correspondences for r in results) / n,
            "worldview_mean": sum(len(r.worldview) for r in results) / n,
            "faults": sum(tables["faults"].values()), "fault_kinds": tables["faults"],
            "tables_with_faults": sum(t["faults"] > 0 for t in per),
            "roles": tables["roles"],
            "per_table": [[t["seed"], t["objects"], t["touched"], t["answer"], t["faults"],
                           t["tags"]] for t in per],
            "results": [[r.seed, r.answer, r.structure_value, r.codelets, r.capped]
                        for r in results]}


def time_runs(problem, config: Config, seed: int, runs: int, checked: bool) -> dict:
    """`runs` runs of a problem on one core, plain or with the invariants checked after
    every codelet; the wall-clock seconds, the codelets run, and the rates (D-077)."""
    seeds = run_seeds(H.batch_seed_for(seed, problem.name), runs)
    codelets = 0
    start = time.perf_counter()
    for s in seeds:
        result = checked_run(problem, config, s)[0] if checked else run_problem(problem, config, s)
        codelets += result.codelets
    seconds = time.perf_counter() - start
    return {"runs": runs, "seconds": seconds, "codelets": codelets,
            "runs_per_second": runs / seconds if seconds else None,
            "ms_per_codelet": 1000 * seconds / codelets if codelets else None}


def run_speed(config: Config, seed: int, names: list, runs: int, progress=None) -> list:
    """The speed profile: every problem timed plain and checked (D-077)."""
    out = []
    for name in names:
        problem = load_problem(name)
        plain = time_runs(problem, config, seed, runs, checked=False)
        checked = time_runs(problem, config, seed, runs, checked=True)
        out.append({"problem": name, "plain": plain, "checked": checked,
                    "overhead": checked["seconds"] / plain["seconds"] if plain["seconds"]
                    else None})
        if progress is not None:
            progress(H.survey(problem, config, seed, min(runs, 5)))
    return out


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """`runs` random tables checked, then the speed profile on the probe problems (D-068,
    D-077)."""
    tables = run_tables(config, seed, runs, progress)
    names = problems_of(sets, only)
    speed = run_speed(config, seed, names, min(runs, SPEED_RUNS), progress)
    return {"summary": tables_summary(config, seed, tables), "speed": speed,
            "speed_runs": min(runs, SPEED_RUNS), "profile": config.profile, "seed": seed}


def _pct(part, whole) -> str:
    return f"{100.0 * part / whole:.1f}%" if whole else "-"


def report(results: dict) -> list:
    """The bench's text: the random tables, their runs and the violations, then the speed
    table (D-068, D-077)."""
    s = results["summary"]
    n = s["tables"]
    lines = [f"## Robustness: {n} random tables under `{s['profile']}` (D-068), every "
             f"invariant checked after every codelet (D-077)", "",
             f"Bench seed {s['seed']}, batch seed {s['batch_seed']}; tables of "
             f"{s['objects']['min']} to {s['objects']['max']} objects, {s['objects']['mean']:.1f} "
             f"on average.", "",
             "| Reading | Value |", "|---|---|",
             f"| Runs capped (D-061) | {s['capped']} of {n} ({_pct(s['capped'], n)}) |",
             f"| Invariant violations | {s['faults']} in {s['tables_with_faults']} tables |",
             f"| Literal answers (D-063) | {s['answers']['literal']} "
             f"({_pct(s['answers']['literal'], n)}) |",
             f"| Answers on the other side of the table | {s['answers']['other_side']} "
             f"({_pct(s['answers']['other_side'], n)}) |",
             f"| Answers of the touched object's type | {s['answers']['same_type']} "
             f"({_pct(s['answers']['same_type'], n)}) |",
             "| Answers by route | " + ", ".join(f"{k} {v}" for k, v in
                                               sorted(s['answers']['through'].items())) + " |",
             f"| Codelets per run | min {s['codelets']['min']}, quartiles "
             f"{s['codelets']['quartiles']}, mean {s['codelets']['mean']:.1f}, "
             f"max {s['codelets']['max']} |",
             f"| Groups, correspondences, Worldview size at the end (means) | "
             f"{s['groups_mean']:.1f}, {s['correspondences_mean']:.1f}, "
             f"{s['worldview_mean']:.2f} |",
             "| Touched types | " + ", ".join(f"{k} {v}" for k, v in
                                             sorted(s['touched'].items(),
                                                    key=lambda kv: -kv[1])) + " |",
             "| Objects by type | " + ", ".join(f"{k} {v}" for k, v in
                                               sorted(s['types'].items(),
                                                      key=lambda kv: -kv[1])) + " |",
             ""]
    if s["fault_kinds"]:
        lines += ["Violations found:", ""]
        lines += [f"- {k}: {v}" for k, v in sorted(s["fault_kinds"].items(), key=lambda kv: -kv[1])]
        lines.append("")
    if "roles" in s:
        lines += roles_lines(s)
    lines += [f"## Speed on one core, {results['speed_runs']} runs per problem "
              f"(Python {platform.python_version()}; machine-dependent)", "",
              "| Problem | Plain runs/s | Plain ms/codelet | Checked runs/s | "
              "Checked ms/codelet | Overhead |", "|---|---|---|---|---|---|"]
    for row in results["speed"]:
        p, c = row["plain"], row["checked"]
        lines.append(f"| {row['problem']} | {p['runs_per_second']:.1f} | "
                     f"{p['ms_per_codelet']:.2f} | {c['runs_per_second']:.1f} | "
                     f"{c['ms_per_codelet']:.2f} | x{row['overhead']:.2f} |")
    lines.append("")
    return lines


def roles_lines(s: dict) -> list:
    """The role statistics of a tables summary as a table (D-106); shared with the
    RESULTS.md block."""
    r, n = s["roles"], s["tables"]
    by_role = ", ".join(f"{k} {v}" for k, v in sorted(r["by_role"].items(),
                                                    key=lambda kv: (-kv[1], kv[0])))
    reasons = ", ".join(f"{k} {v}" for k, v in r["reasons"].items()) or "none"
    return ["| Roles (D-106) | Per run |", "|---|---|",
            f"| Tags read | {r['tags_per_run']:.2f} (none in {r['runs_without_a_tag']} of {n} runs) |",
            f"| Tags standing at the end | {r['standing_per_run']:.2f} |",
            f"| Changes (a tag lost, broken or gone with its structure) | {r['changes_per_run']:.2f} "
            f"({reasons}; in {r['runs_with_a_change']} of {n} runs) |",
            f"| Re-casts (D-093) | in {r['recast_runs']} of {n} runs, a tie in {r['tie_runs']} |",
            f"| C1 proposals (create-cp posted by propose-role-cp or recast-role, D-095) | "
            f"{r['c1_proposals_per_run']:.2f} (in {r['runs_with_a_proposal']} of {n} runs) |",
            f"| Tags read by role | {by_role or '-'} |", ""]


def records(results: dict) -> list:
    """The JSON records: the tables' summary (with every run, stripped by the results
    builder) and the speed profile (D-068, D-077)."""
    return [results["summary"],
            {"bench": "robustness", "kind": "speed", "profile": results["profile"],
             "seed": results["seed"], "runs": results["speed_runs"],
             "python": platform.python_version(), "rows": results["speed"]}]

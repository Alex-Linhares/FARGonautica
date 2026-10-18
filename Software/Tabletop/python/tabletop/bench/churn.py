"""The `churn` bench: roles over time, and the role codelets against the find-answer flood
(D-106; the roles loop's answers D1 and C8).

A role tag is a competing, revisable structure (D-079): it can lose a fight with a rival
reading, be broken by the role checker, or go with the structure it stood on. This bench
runs each carrier `--runs` times from the bench seed (D-072) with an observer on the
events and reads, per run, every change of a role (`RoleDiscarded`, by reason and role),
whether a change touched the answer or the touched object, whether the last change came
before the last change of the Worldview, how the changes are spread over the run (per ten
codelets in the first and the second half, and by the temperature at the change: hot, warm
and cold thirds of T), and which codelets ran before the answer: the six role codelets
(D-081) against the find-answer codelets hung by the schedule (D-056). The tags read and
standing per role, the re-casts and their ties (D-093) are counted too. A carrier on
which no role ever changes is a finding to report, not a pass; no urgency is changed by
the bench, which measures and prints.
"""

from __future__ import annotations

from tabletop import events as EV
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.codelets import catalogue as CAT
from tabletop.config import Config
from tabletop.problems import load_problem
from tabletop.accel import run_seeds
from tabletop.run import result_of
from tabletop.world import World

SETS = P.DEFAULT_PROBLEMS          # the four carriers of the oracle probe (D-104)
problems_of = P.problems_of
ROLE_CODELETS = tuple(CAT.role_codelets())
FIND_ANSWER = "find-answer"
REASONS = ("lost", "broken", "structure gone")        # `RoleDiscarded.reason` (D-079)
# The thirds of the temperature at a change (D-106): hot at or above 2/3 of 100, cold
# below 1/3, warm between.
BANDS = (("hot", 200.0 / 3.0), ("warm", 100.0 / 3.0), ("cold", 0.0))


def planned(sets=SETS, only=None) -> int:
    """One progress line per carrier (D-073)."""
    return len(problems_of(sets, only))


def reason_of(text: str) -> str:
    """The kind of a discard from its reason text: `lost to r7` is `lost` (D-079)."""
    for reason in REASONS:
        if text.startswith(reason):
            return reason
    return text


def band_of(temperature: float) -> str:
    """The third of T a reading falls in (D-106)."""
    for name, floor in BANDS:
        if temperature >= floor:
            return name
    return BANDS[-1][0]


class Timeline:
    """An observer that keeps what the bench reads of a run (D-106): the codelets chosen
    with the temperature at the choice, the tags read, the discards with the temperature
    at the change, the clocks of the Worldview's changes, the re-casts and the find-answer
    codelets hung."""

    def __init__(self):
        self.temperature = 100.0
        self.chosen: list = []        # (clock, temperature, codelet type)
        self.tags: list = []          # (clock, role)
        self.discards: list = []      # (clock, temperature, role, item, reason)
        self.worldview: list = []     # clocks
        self.recasts = 0
        self.ties = 0
        self.hung = 0

    def on_event(self, e: EV.Event) -> None:
        """Keep the event's reading (D-065)."""
        if isinstance(e, EV.CodeletChosen):
            self.temperature = e.temperature
            self.chosen.append((e.clock, e.temperature, e.codelet))
        elif isinstance(e, EV.RoleTagged):
            self.tags.append((e.clock, e.role))
        elif isinstance(e, EV.RoleDiscarded):
            self.discards.append((e.clock, self.temperature, e.role, e.item, reason_of(e.reason)))
        elif isinstance(e, EV.WorldviewChanged):
            self.worldview.append(e.clock)
        elif isinstance(e, EV.RoleRecast):
            self.recasts += 1
            self.ties += e.ambiguous
        elif isinstance(e, EV.AnswerCodeletPosted):
            self.hung += 1


def run_facts(problem, config: Config, seed: int) -> dict:
    """One run with the timeline observed (D-106): the result and the readings listed in
    the module's docstring, as plain data."""
    line = Timeline()
    world = World(problem, config, seed, [line])
    while not world.finished:
        world.step()
    result = result_of(world)
    answer, touched, n = result.answer, problem.touched, result.codelets
    half = n / 2.0
    first = sum(1 for d in line.discards if d[0] <= half)
    bands = {name: [0, 0] for name, _ in BANDS}          # changes, codelets
    for clock, t, _ in line.chosen:
        bands[band_of(t)][1] += 1
    for clock, t, *_ in line.discards:
        bands[band_of(t)][0] += 1
    roles_chosen = {name: 0 for name in ROLE_CODELETS}
    for _, _, codelet in line.chosen:
        if codelet in roles_chosen:
            roles_chosen[codelet] += 1
    first_role = next((c for c, _, codelet in line.chosen if codelet in roles_chosen), None)
    standing: dict = {}
    for tag in world.workspace.roles:
        standing[tag.role] = standing.get(tag.role, 0) + 1
    return {"result": result,
            "changes": len(line.discards),
            "reasons": {r: sum(1 for d in line.discards if d[4] == r) for r in REASONS},
            "by_role": _count(d[2] for d in line.discards),
            "touching_answer": any(d[3] == answer for d in line.discards),
            "touching_touched": any(d[3] == touched for d in line.discards),
            "last_before_worldview": bool(line.discards and line.worldview
                                          and line.discards[-1][0] < line.worldview[-1]),
            "halves": (first, len(line.discards) - first),
            "bands": bands,
            "roles_chosen": roles_chosen,
            "find_answer_chosen": sum(1 for _, _, c in line.chosen if c == FIND_ANSWER),
            "find_answer_hung": line.hung,
            "first_role": first_role,
            "tags": _count(role for _, role in line.tags),
            "standing": standing,
            "recasts": line.recasts, "ties": line.ties}


def _count(items) -> dict:
    out: dict = {}
    for it in items:
        out[it] = out.get(it, 0) + 1
    return out


def _mean(xs) -> float:
    xs = list(xs)
    return sum(xs) / len(xs) if xs else 0.0


def _sum_dicts(dicts) -> dict:
    out: dict = {}
    for d in dicts:
        for k, v in d.items():
            out[k] = out.get(k, 0) + v
    return out


def summarize(problem, config: Config, seed: int, facts: list) -> dict:
    """A carrier's record from its runs' facts (D-106): counts over the batch, means per
    run, and every run's (seed, answer, structure value, codelets, capped) for replay."""
    n = len(facts)
    results = [f["result"] for f in facts]
    changed = [f for f in facts if f["changes"]]
    codelets = sum(r.codelets for r in results)
    halves = [sum(f["halves"][0] for f in facts), sum(f["halves"][1] for f in facts)]
    bands = {name: [sum(f["bands"][name][0] for f in facts), sum(f["bands"][name][1] for f in facts)]
             for name, _ in BANDS}
    labels = lambda fs: _count(f["result"].label or f["result"].answer for f in fs)
    roles_chosen = _sum_dicts(f["roles_chosen"] for f in facts)
    return {"bench": "churn", "problem": problem.name, "profile": config.profile, "seed": seed,
            "batch_seed": H.batch_seed_for(seed, problem.name), "runs": n,
            "capped": sum(r.capped for r in results), "mean_codelets": round(codelets / n, 1),
            "changed_runs": len(changed),
            "changes": sum(f["changes"] for f in facts),
            "reasons": _sum_dicts(f["reasons"] for f in facts),
            "changes_by_role": _sum_dicts(f["by_role"] for f in facts),
            "runs_touching_answer": sum(f["touching_answer"] for f in facts),
            "runs_touching_touched": sum(f["touching_touched"] for f in facts),
            "last_before_worldview": sum(f["last_before_worldview"] for f in changed),
            "answers_touching": labels(f for f in facts if f["touching_answer"]),
            "answers_untouched": labels(f for f in facts if not f["touching_answer"]),
            "per_ten_halves": [round(20.0 * halves[0] / codelets, 3) if codelets else 0.0,
                               round(20.0 * halves[1] / codelets, 3) if codelets else 0.0],
            "bands": {name: {"changes": c, "codelets": k,
                             "per_ten": round(10.0 * c / k, 3) if k else 0.0}
                      for name, (c, k) in bands.items()},
            "role_codelets": {name: round(count / n, 2) for name, count in roles_chosen.items()},
            "role_share": round(100.0 * sum(roles_chosen.values()) / codelets, 1) if codelets else 0.0,
            "find_answer_chosen": round(_mean(f["find_answer_chosen"] for f in facts), 2),
            "find_answer_hung": round(_mean(f["find_answer_hung"] for f in facts), 2),
            "first_role_codelet": round(_mean(f["first_role"] for f in facts
                                              if f["first_role"] is not None), 1),
            "runs_with_role_codelet": sum(f["first_role"] is not None for f in facts),
            "tags_read": round(sum(sum(f["tags"].values()) for f in facts) / n, 2),
            "tags_by_role": {k: round(v / n, 2) for k, v in _sum_dicts(f["tags"] for f in facts).items()},
            "standing": round(sum(sum(f["standing"].values()) for f in facts) / n, 2),
            "standing_by_role": {k: round(v / n, 2)
                                 for k, v in _sum_dicts(f["standing"] for f in facts).items()},
            "recast_runs": sum(f["recasts"] > 0 for f in facts),
            "tie_runs": sum(f["ties"] > 0 for f in facts),
            "results": [[r.seed, r.answer, r.structure_value, r.codelets, r.capped]
                        for r in results]}


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """Every carrier `runs` times from the bench seed (D-072), observed (D-106)."""
    records = []
    for name in problems_of(sets, only):
        problem = load_problem(name)
        seeds = run_seeds(H.batch_seed_for(seed, name), runs)
        facts = [run_facts(problem, config, s) for s in seeds]
        records.append(summarize(problem, config, seed, facts))
        if progress is not None:
            progress(H.survey_of(problem, config, seed, [f["result"] for f in facts]))
    return {"records": records, "profile": config.profile, "seed": seed, "runs": runs}


def _pct(part, whole) -> str:
    return f"{100.0 * part / whole:.1f}%" if whole else "-"


def tables(records: list) -> list:
    """The bench's four tables from its records (D-106): the changes, their course over
    the run, the codelets before the answer, and the tags; shared by `report` and the
    RESULTS.md block."""
    if not records:
        return ["(no churn records)"]
    lines = ["**Role changes per run** (a tag lost to a rival, broken by the checker, or "
             "gone with its structure; D-079):", "",
             "| Carrier | Runs | Runs with a change | Changes per run | Lost | Broken | "
             "Structure gone | Last change before the last Worldview change (of the changed "
             "runs) | Runs with a change on the answer | On the touched object |",
             "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for r in records:
        n, rs = r["runs"], r["reasons"]
        lines.append(f"| {r['problem']} | {n} | {r['changed_runs']} ({_pct(r['changed_runs'], n)}) "
                     f"| {r['changes'] / n:.2f} | {rs.get('lost', 0)} | {rs.get('broken', 0)} | "
                     f"{rs.get('structure gone', 0)} | {r['last_before_worldview']} "
                     f"({_pct(r['last_before_worldview'], r['changed_runs'])}) | "
                     f"{r['runs_touching_answer']} | {r['runs_touching_touched']} |")
    lines += ["", "**Does the churn settle?** Changes per ten codelets in the first and the "
              "second half of a run, and by the temperature at the change (hot: T at or "
              "above 66.7; warm: 33.3 to 66.7; cold: below 33.3), with the codelets run in "
              "each band:", "",
              "| Carrier | First half | Second half | Hot | Warm | Cold | Codelets hot / warm / "
              "cold |", "|---|---:|---:|---:|---:|---:|---|"]
    for r in records:
        b = r["bands"]
        lines.append(f"| {r['problem']} | {r['per_ten_halves'][0]:.3f} | "
                     f"{r['per_ten_halves'][1]:.3f} | {b['hot']['per_ten']:.3f} | "
                     f"{b['warm']['per_ten']:.3f} | {b['cold']['per_ten']:.3f} | "
                     f"{b['hot']['codelets']} / {b['warm']['codelets']} / {b['cold']['codelets']} |")
    lines += ["", "**Role codelets before the answer** (chosen per run, D-081) against the "
              "find-answer codelets hung by the schedule and chosen (D-056):", "",
              "| Carrier | Codelets per run | " + " | ".join(ROLE_CODELETS) +
              " | Role share | find-answer chosen | find-answer hung | First role codelet at "
              "| Runs with one |",
              "|---|---:|" + "---:|" * len(ROLE_CODELETS) + "---:|---:|---:|---:|---:|"]
    for r in records:
        cells = " | ".join(f"{r['role_codelets'].get(name, 0.0):.2f}" for name in ROLE_CODELETS)
        lines.append(f"| {r['problem']} | {r['mean_codelets']:.1f} | {cells} | "
                     f"{r['role_share']:.1f}% | {r['find_answer_chosen']:.2f} | "
                     f"{r['find_answer_hung']:.2f} | {r['first_role_codelet']:.1f} | "
                     f"{r['runs_with_role_codelet']} |")
    lines += ["", "**Tags read and standing** (per run), the re-casts and their ties "
              "(D-093), and the roles changed most:", "",
              "| Carrier | Tags read | Standing at the end | Runs with a re-cast | Runs with "
              "a tie | Changes by role (top five) | Tags read by role (top five) |",
              "|---|---:|---:|---:|---:|---|---|"]
    for r in records:
        top = lambda d: ", ".join(f"{k} {v}" for k, v in
                                  sorted(d.items(), key=lambda kv: (-kv[1], kv[0]))[:5]) or "-"
        lines.append(f"| {r['problem']} | {r['tags_read']:.2f} | {r['standing']:.2f} | "
                     f"{r['recast_runs']} | {r['tie_runs']} | {top(r['changes_by_role'])} | "
                     f"{top(r['tags_by_role'])} |")
    lines.append("")
    return lines


def report(results: dict) -> list:
    """The bench's text (D-106)."""
    records = results["records"]
    return [f"## Roles over time: {results['runs']} runs per carrier under "
            f"`{results['profile']}`, bench seed {results['seed']} (D-106)", ""] + tables(records)


def records(results: dict) -> list:
    """The JSON records, one per carrier (D-106); the runs are stripped by the results
    builder (D-075)."""
    return list(results["records"])

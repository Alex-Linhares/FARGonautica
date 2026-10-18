"""The `ch4` bench: the chapter 4 run replayed many times, against the screen dumps
(D-073).

Chapter 4 shows one run on the central configuration, Surround 5, in nine screen dumps
at 0, 2, 12, 22, 33, 36, 47, 47 and 55 codelets [F95 pp. 96-107], each with the
thermometer, the Coderack's bar graph and the structures drawn; `tests/data/ch4.json` is
their transcription (SPEC.md §6c; `docs/ch4-run.md`). French shows one run, so the dumps
give one path, not rates: this bench runs the configuration many times from the bench
seed, stepping each run, and reports at each dump's codelet count the mean temperature
and the mean rack shares of the runs still going, beside his readings; and for each
milestone of his run (the first gestalt group, the first bottom-up group, the first
correspondence, the first Worldview entry, the first group correspondence in the
Worldview, the first fight, the answer) the share of our runs that reach it and the mean
codelet at which they do, beside the codelet count of the first dump that shows it.
Nothing here sets a default (ANSWERS.md 1: the dumps may be read directly, never fitted).
"""

import json
from dataclasses import dataclass
from pathlib import Path

from tabletop import events as EV
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import Config
from tabletop.problems import load_problem
from tabletop.accel import run_seeds
from tabletop.run import result_of
from tabletop.world import World

PROBLEM = "surround-05"   # the chapter 4 configuration [F95 p. 96; F95 p. 118]
SETS = (PROBLEM,)
DATA = Path(__file__).resolve().parents[2] / "tests" / "data" / "ch4.json"
problems_of = P.problems_of

# (milestone, how the dumps show it, how our events show it)
MILESTONES = (
    ("first gestalt group", "gestalt", "gestalt"),
    ("first bottom-up group", "subgroup", "sameness"),
    ("first correspondence", "correspondence", "correspondence"),
    ("first Worldview entry", "in the Worldview", "entry"),
    ("first group correspondence in the Worldview", "group in the Worldview", "group-entry"),
    ("first fight", "competing", "fight"),
    ("answer", "answer", "answer"),
)


def french_data() -> dict:
    """The transcription of the dumps (SPEC.md §6c) (D-073)."""
    with open(DATA, encoding="utf-8") as f:
        return json.load(f)


def french_dumps(data: dict) -> list:
    """The dumps with a thermometer or a bar graph: (figure, page, codelets, temperature,
    {label: percent}) [F95 pp. 96-107] (D-073)."""
    out = []
    for d in data["dumps"]:
        bars = {b["label"]: b["percent"] for b in d["coderack"]}
        out.append((d["figure"], d["page"], d["codelets_run"], d["temperature"], bars))
    return out


def french_milestones(data: dict) -> dict:
    """The codelet count of the first dump showing each milestone, read from the
    structures drawn [F95 pp. 96-107] (D-073); None when no dump shows it."""
    out = {name: None for name, _, _ in MILESTONES}
    for d in data["dumps"]:
        s, n = d["structures"], d["codelets_run"]
        seen = {
            "first gestalt group": any(g["kind"] == "gestalt" for g in s["groups"]),
            "first bottom-up group": any("subgroup" in g["kind"] for g in s["groups"]),
            "first correspondence": bool(s["correspondences"]),
            "first Worldview entry": any("in the Worldview" in c["status"]
                                         for c in s["correspondences"]),
            "first group correspondence in the Worldview": any(
                "in the Worldview" in c["status"] and "-" in c["from"]
                for c in s["correspondences"]),
            "first fight": any("competing" in c["status"] for c in s["correspondences"]),
            "answer": bool(s.get("answer")),
        }
        for name, hit in seen.items():
            if hit and out[name] is None:
                out[name] = n
    return out


@dataclass(frozen=True)
class RunPath:
    """One run's path (D-073): the `seed`, the `dumps` as {codelets: (temperature,
    {label: percent})} for the dump counts the run reached, the `milestones` as
    {name: codelet or None}, and the `result`."""

    seed: int
    dumps: dict
    milestones: dict
    result: object


def milestones_of(collector: EV.Collector, ids: list) -> dict:
    """The codelet of each milestone's first occurrence in a run's events (D-073)."""
    out = {name: None for name, _, _ in MILESTONES}
    ends = {}

    def first(name, clock):
        if out[name] is None:
            out[name] = clock

    for e in collector.events:
        if isinstance(e, EV.GroupBuilt):
            if e.kind == "gestalt":
                first("first gestalt group", e.clock)
            elif e.kind == "sameness":
                first("first bottom-up group", e.clock)
        elif isinstance(e, EV.CorrespondenceBuilt):
            ends[e.correspondence] = (e.henry, e.eliza)
            first("first correspondence", e.clock)
        elif isinstance(e, EV.WorldviewChanged) and e.added:
            first("first Worldview entry", e.clock)
            if any(end not in ids for name in e.added for end in ends.get(name, ()) if name in ends):
                first("first group correspondence in the Worldview", e.clock)
        elif isinstance(e, EV.CompetitionHeld) and e.rivals:
            first("first fight", e.clock)
        elif isinstance(e, EV.AnswerGiven):
            first("answer", e.clock)
    return out


def run_path(problem, config: Config, seed: int, counts: tuple) -> RunPath:
    """One run, stepped, with the thermometer and the bars read at each dump count the
    run reaches [F95 pp. 96-107] (D-073)."""
    collector = EV.Collector()
    world = World(problem, config, seed, [collector])
    dumps = {}
    while not world.finished:
        world.step()
        if world.clock in counts and not world.finished:
            dumps[world.clock] = (world.temperature.value, dict(world.coderack.shares()))
    return RunPath(seed, dumps, milestones_of(collector, problem.ids()), result_of(world))


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """Run the chapter 4 configuration `runs` times from the bench seed and gather the
    paths, the survey and French's readings (D-073)."""
    if PROBLEM not in problems_of(sets, only):
        return {"data": french_data(), "paths": [], "survey": None}
    data = french_data()
    counts = tuple(d[2] for d in french_dumps(data) if d[2] > 0)
    problem = load_problem(PROBLEM)
    batch_seed = H.batch_seed_for(seed, PROBLEM)
    paths = [run_path(problem, config, s, counts) for s in run_seeds(batch_seed, runs)]
    survey = H.survey_of(problem, config, seed, [p.result for p in paths])
    if progress is not None:
        progress(survey)
    return {"data": data, "paths": paths, "survey": survey}


def _mean(values) -> float | None:
    values = list(values)
    return sum(values) / len(values) if values else None


def dump_rows(results: dict) -> list:
    """Per dump: (figure, page, codelets, French T, our mean T, runs still going, French's
    bars, our mean shares of the same labels, our other labels at 5 percent or more)
    (D-073)."""
    out = []
    for figure, page, n, t, bars in french_dumps(results["data"]):
        if n == 0:
            continue
        alive = [p.dumps[n] for p in results["paths"] if n in p.dumps]
        ours_t = _mean(temp for temp, _ in alive)
        labels = list(bars)
        ours = {lab: _mean(shares.get(lab, 0.0) for _, shares in alive) for lab in labels}
        others = {}
        for _, shares in alive:
            for lab, pct in shares.items():
                if lab not in bars:
                    others[lab] = others.get(lab, 0.0) + pct / len(alive)
        others = {lab: pct for lab, pct in others.items() if pct >= 5.0}
        out.append((figure, page, n, t, ours_t, len(alive), bars, ours, others))
    return out


def milestone_rows(results: dict) -> list:
    """Per milestone: (name, French's first dump count, our share of runs reaching it,
    our mean codelet) (D-073)."""
    french = french_milestones(results["data"])
    paths = results["paths"]
    out = []
    for name, _, _ in MILESTONES:
        reached = [p.milestones[name] for p in paths if p.milestones[name] is not None]
        share = 100.0 * len(reached) / len(paths) if paths else None
        out.append((name, french[name], share, _mean(reached)))
    return out


def _fmt(v, digits=1) -> str:
    return "-" if v is None else f"{v:.{digits}f}"


def report(results: dict) -> list:
    """The bench's text: the dumps table, the milestones, the answers (D-073)."""
    lines = ["## The chapter 4 run against the screen dumps [F95 pp. 96-107]", ""]
    if not results["paths"]:
        return lines + ["(not run)", ""]
    lines += ["| Dump | Page | Codelets | T, French | T, ours (mean) | Runs still going | "
              "Bars, French | Same labels, ours (mean) | Our other bars >= 5 |",
              "|---|---:|---:|---:|---:|---:|---|---|---|"]
    for figure, page, n, t, ours_t, alive, bars, ours, others in dump_rows(results):
        his = " ".join(f"{lab} {pct}" for lab, pct in bars.items()) or "-"
        mine = " ".join(f"{lab} {_fmt(ours[lab], 0)}" for lab in bars) or "-"
        rest = " ".join(f"{lab} {pct:.0f}" for lab, pct in others.items()) or "-"
        lines.append(f"| Fig. {figure} | {page} | {n} | {_fmt(t)} | {_fmt(ours_t)} | {alive} | "
                     f"{his} | {mine} | {rest} |")
    lines += ["", "### Milestones", "",
              "| Milestone | French's run: first dump showing it (codelets) | Our runs "
              "reaching it, % | Mean codelet, ours |", "|---|---:|---:|---:|"]
    for name, his, share, mean in milestone_rows(results):
        lines.append(f"| {name} | {_fmt(his, 0)} | {_fmt(share)} | {_fmt(mean)} |")
    sv = results["survey"]
    data = results["data"]
    his_answer = data["dumps"][-1]["structures"].get("answer")
    others = [f"{r['answer'] or '(no answer shown)'} (Fig. {r['figure']})" for r in data["other_runs"]]
    lines += ["", "### Answers", "",
              f"French's run: {his_answer} at {data['dumps'][-1]['codelets_run']} codelets "
              f"[F95 p. {data['dumps'][-1]['page']}]; his five other runs [F95 pp. 108-110]: "
              + ", ".join(others) + ".", "",
              "| Answer | Label | Runs | % | Mean structure | Mean codelets |",
              "|---|---|---:|---:|---:|---:|"]
    for t in sv.tallies:
        lines.append(f"| {t.answer} | {t.label or '-'} | {t.count} | {t.frequency:.1f} | "
                     f"{_fmt(t.structure)} | {_fmt(t.codelets)} |")
    lines += ["", f"{sv.runs} runs under {sv.profile} (bench seed {sv.seed}, batch seed "
                  f"{sv.batch_seed}), {sv.capped} capped, mean codelets {sv.codelets:.1f}.", ""]
    return lines


def records(results: dict) -> list:
    """The JSON records: the survey, the dump rows, the milestone rows and every path
    (D-073)."""
    if not results["paths"]:
        return []
    return [{"bench": "ch4", "problem": PROBLEM, "survey": H.survey_record(results["survey"]),
             "dumps": [{"figure": f, "page": pg, "codelets": n, "french_t": t, "ours_t": ot,
                        "alive": alive, "french_bars": bars, "ours_bars": ours, "others": oth}
                       for f, pg, n, t, ot, alive, bars, ours, oth in dump_rows(results)],
             "milestones": [{"name": n, "french": h, "share": s, "mean": m}
                            for n, h, s, m in milestone_rows(results)],
             "paths": [{"seed": p.seed, "milestones": p.milestones,
                        "dumps": {str(k): [t, s] for k, (t, s) in p.dumps.items()}}
                       for p in results["paths"]]}]

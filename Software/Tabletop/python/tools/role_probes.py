#!/usr/bin/env python3
"""The oracle and lesion probes of the roles, for RESULTS.md §11.3 and §11.4 (D-104): a
measurement, never a gate. Test-only: it imports the probe helper of `tests/role_probes.py`
and patches one module-level function per probe; the engine has no switch.

    python3 -m tools.role_probes [--runs 200] [--seed 1] [--out results/role-probes.json]
                                 [--problems surround-05 ...] [--no-lesions] [--quiet]

(from `python/`, as a module, so that the package resolves without touching `sys.path`).

For each carrier of `tests/data/oracle_roles.json` it runs the engine as it stands
(`perceived`), with the hand-read roles in place of perception (`oracle`) and with the role
checker never breaking a tag (`no_break`, the D1 probe of D-106, paired run for run so that
the runs whose answer differs are those a broken role changed), and records the
mechanism measures of the variants bench (the Worldview's size, its coherent and incoherent
pairs, the role tags per run; D-102), the answers, how the answer was reached, the codelets
run and the capped runs; on the Buridan carrier it also scans the incoherence penalty at 40
and 300 (D-042, D-076's `buridan` scan) under both perceptions. The cost of perception
(`roles_read`): per carrier, the share of runs in which each role stands at the end, and
the hand-read tags the engine's own perception misses or adds (a group's tag counted for
its members). The lesions (`lesions`; D-104, `docs/predictions.md` §5): every probe of
`tests/role_probes.py` but D1, on the four carriers and on the result sets its registered
expectation names (`LESION_EXTRA`), each paired run for run with the plain batch of the
same seeds. The JSON goes to `results/role-probes.json`, from which RESULTS.md §11.3 and
§11.4 are written by hand.

The work is a list of independent jobs (`plan`, `job`, `assemble`), so that a driver may
farm them over processes and still write byte for byte what the serial run writes.
"""

import argparse
import dataclasses
import importlib.util
import json
import sys
import time
from pathlib import Path

from tabletop.bench import harness as H
from tabletop.bench import variants as V
from tabletop.config import PROFILES
from tabletop.problems import load_problem
from tabletop.rng import run_seeds
from tabletop.world import World

PYTHON_DIR = Path(__file__).resolve().parents[1]
HELPER = PYTHON_DIR / "tests" / "role_probes.py"


def load_helper():
    """The probe helper of `tests/role_probes.py`, loaded by file (test-only, D-104)."""
    spec = importlib.util.spec_from_file_location("role_probes", HELPER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


RP = load_helper()

SCAN_PROBLEM = "buridan-01"
PENALTIES = (40, 300)
DEFAULT_OUT = PYTHON_DIR / "results" / "role-probes.json"
NO_BREAK = "D1"

# The result sets, beyond the four carriers, on which a lesion's registered expectation is
# read (`docs/predictions.md` §5 and §8): the Surround E1 sets for the proposers and the
# geometry, Blockage 6-8 for the barrier, Blockage 1-5 and the carriers for the
# COLUMN-against-MIRROR_IMAGE question, Figure/Ground 1 for the content family.
SURROUND_E1 = ("surround-06", "surround-07", "surround-10")
SURROUND_E2 = ("surround-01", "surround-02", "surround-03", "surround-04")
BLOCKAGE_E3 = ("blockage-07", "blockage-08")
LESION_EXTRA = {
    "C1": SURROUND_E1,
    "C3": SURROUND_E1,
    "C6": SURROUND_E2 + SURROUND_E1,
    "S5": BLOCKAGE_E3,
    "C4": BLOCKAGE_E3,
    "A2": (),
    "C2": (),
    "C5": (),
    "C7": ("surround-10",),
    "K1": SURROUND_E1,
    "L2": ("blockage-01", "blockage-03", "blockage-04", "blockage-05"),
    "S2": SURROUND_E1,
    "L3": BLOCKAGE_E3,
    "L4": ("surround-01",),
    "L5": ("surround-10",),
    "A4": ("figure-ground-01",),
    "A5": ("figure-ground-01",),
    "A6": BLOCKAGE_E3,
    "A7": SURROUND_E2,
    "K2": ("figure-ground-01",),
}


def lesion_names() -> list:
    """Every lesion of the helper but the no-break probe, in the helper's order (D-104)."""
    return [name for name in RP.LESIONS if name != NO_BREAK]


def batch(problem, config, seeds) -> dict:
    """A batch's record: the mechanism measures, the answers by label (or id), how the
    answers were reached, the mean codelets, the capped and the literal runs; `labels`
    (one per run, in seed order) is kept for the pairing and dropped by `assemble`."""
    runs = [V.run_facts(problem, config, s) for s in seeds]
    labels = [r.result.label or r.result.answer for r in runs]
    return {"runs": len(runs), "mechanism": V.mechanism(runs), "answers": V.answer_counts(labels),
            "through": V.answer_counts([r.result.through for r in runs]),
            "codelets": round(sum(r.result.codelets for r in runs) / len(runs), 1),
            "capped": sum(r.result.capped for r in runs),
            "literal": sum(r.result.literal for r in runs),
            "labels": labels}


def paired(record: dict, plain: dict) -> dict:
    """The record with its runs paired against the plain batch of the same seeds: how many
    answers differ and which way they moved (D-106's reading of a probe)."""
    differ = [(a, b) for a, b in zip(plain["labels"], record["labels"]) if a != b]
    record["paired_differ"] = len(differ)
    record["moved"] = V.answer_counts([f"{a} to {b}" for a, b in differ])
    return record


def oracle_scope(problem: str) -> tuple:
    """The hand-read tags of a carrier as (item, role) pairs, and the roles the file reads
    at all, which is the scope the engine's perception is compared on (RESULTS.md §11.3)."""
    data = json.loads(RP.ORACLE.read_text())
    pairs = {(row["item"], row["role"]) for row in data[problem]}
    roles = sorted({row["role"] for key, rows in data.items() if not key.startswith("_")
                    for row in rows})
    return pairs, roles


def standing_pairs(world) -> set:
    """The (item, role) pairs standing at the end of a run, a group's tag counted for each
    of its members (the oracle reads objects only; a member is a corner or a stray through
    its group, D-085, D-086)."""
    pairs = set()
    for tag in world.workspace.roles:
        if world.workspace.has_group(tag.item):
            pairs.update((m, tag.role) for m in world.workspace.group_named(tag.item).members)
        else:
            pairs.add((tag.item, tag.role))
    return pairs


def roles_read(problem, seeds) -> dict:
    """The cost of perception on a carrier: per role, the share of runs in which it stands
    at the end; over the hand-read tags, the mean share the engine's perception misses
    (`missed`) and, over the engine's tags of the same roles, the mean share the hand
    reading does not have (`extra`); and the hand-read tags missed most often."""
    oracle, scope = oracle_scope(problem.name)
    present: dict = {}
    missed_by: dict = {}
    missed, extra = [], []
    for seed in seeds:
        world = World(problem, PROFILES["standard"], seed)
        while not world.finished:
            world.step()
        pairs = standing_pairs(world)
        for role in {r for _, r in pairs}:
            present[role] = present.get(role, 0) + 1
        in_scope = {p for p in pairs if p[1] in scope}
        gone = oracle - in_scope
        for p in gone:
            missed_by[p] = missed_by.get(p, 0) + 1
        missed.append(len(gone) / len(oracle))
        extra.append(len(in_scope - oracle) / len(in_scope) if in_scope else 0.0)
    n = len(seeds)
    top = sorted(missed_by.items(), key=lambda kv: (-kv[1], kv[0]))[:8]
    return {"runs": n, "hand_read": len(oracle),
            "present": {r: round(100.0 * c / n, 1) for r, c in sorted(present.items())},
            "missed": round(sum(missed) / n, 3), "extra": round(sum(extra) / n, 3),
            "missed_most": [f"{item} {role} {c}" for (item, role), c in top]}


def plan(names: list, runs: int, seed: int, lesions: bool) -> list:
    """The jobs, each a dict a worker can take: the carriers' plain, oracle, no-break and
    penalty-scan batches and their roles-read survey, then every lesion on the carriers
    and on the extra sets of `LESION_EXTRA` (with a plain batch for each extra set)."""
    jobs = []
    for name in names:
        jobs.append({"kind": "plain", "problem": name})
        jobs.append({"kind": "oracle", "problem": name})
        jobs.append({"kind": "no_break", "problem": name})
        jobs.append({"kind": "roles_read", "problem": name})
        if name == SCAN_PROBLEM:
            for penalty in PENALTIES:
                jobs.append({"kind": "scan", "problem": name, "penalty": penalty})
                jobs.append({"kind": "scan_oracle", "problem": name, "penalty": penalty})
    if lesions:
        extra = sorted({p for ps in LESION_EXTRA.values() for p in ps if p not in names})
        for name in extra:
            jobs.append({"kind": "plain", "problem": name})
        for lesion in lesion_names():
            for name in list(names) + [p for p in extra if p in LESION_EXTRA.get(lesion, ())]:
                jobs.append({"kind": "lesion", "problem": name, "lesion": lesion})
    for j in jobs:
        j["runs"], j["seed"] = runs, seed
    return jobs


def job(spec: dict) -> dict:
    """One job's record (a batch, or the roles-read survey), from its spec alone."""
    name = spec["problem"]
    problem = load_problem(name)
    seeds = run_seeds(H.batch_seed_for(spec["seed"], name), spec["runs"])
    kind = spec["kind"]
    if kind == "roles_read":
        return roles_read(problem, seeds)
    config = PROFILES["standard"]
    if kind in ("scan", "scan_oracle"):
        config = dataclasses.replace(config, incoherence_penalty=spec["penalty"])
    if kind in ("oracle", "scan_oracle"):
        with RP.oracle_roles(name):
            return batch(problem, config, seeds)
    if kind == "no_break":
        with RP.lesioned(NO_BREAK):
            return batch(problem, config, seeds)
    if kind == "lesion":
        with RP.lesioned(spec["lesion"]):
            return batch(problem, config, seeds)
    return batch(problem, config, seeds)


def assemble(jobs: list, outputs: list, names: list, runs: int, seed: int,
             seconds: float) -> dict:
    """The JSON from the jobs' outputs: one record per carrier (as the tool has always
    written it, plus `roles_read`, the answers' total variation between the perceptions
    and the lesions), one per extra set (its plain batch and its lesions), every lesioned
    batch paired with the plain one; the per-run labels dropped."""
    by = {}
    for spec, out in zip(jobs, outputs):
        by[(spec["kind"], spec["problem"], spec.get("penalty"), spec.get("lesion"))] = out
    records, extra_records = [], []
    extra = sorted({j["problem"] for j in jobs} - set(names))
    for name in [n for n in names if ("plain", n, None, None) in by] + extra:
        plain = by[("plain", name, None, None)]
        record = {"problem": name, "perceived": plain}
        if name in names:
            record["oracle"] = by[("oracle", name, None, None)]
            record["oracle_tv"] = round(V.total_variation(plain["answers"],
                                                          record["oracle"]["answers"]), 3)
            record["roles_read"] = by[("roles_read", name, None, None)]
            record["no_break"] = paired(by[("no_break", name, None, None)], plain)
            if name == SCAN_PROBLEM:
                record["penalty_scan"] = {
                    str(p): {"perceived": by[("scan", name, p, None)],
                             "oracle": by[("scan_oracle", name, p, None)]} for p in PENALTIES}
        lesions = {lesion: paired(by[("lesion", name, None, lesion)], plain)
                   for lesion in lesion_names() if ("lesion", name, None, lesion) in by}
        if lesions:
            record["lesions"] = lesions
        (records if name in names else extra_records).append(record)
    for spec, out in zip(jobs, outputs):
        out.pop("labels", None)
    return {"tool": "role_probes", "runs": runs, "seed": seed, "profile": "standard",
            "penalty_scan_problem": SCAN_PROBLEM, "penalties": list(PENALTIES),
            "lesions": [l for l in lesion_names() if any(j.get("lesion") == l for j in jobs)],
            "lesion_extra": {k: list(v) for k, v in LESION_EXTRA.items()},
            "seconds": round(seconds, 1), "records": records, "extra_records": extra_records}


def say(out: dict) -> None:
    """One line per carrier."""
    for record in out["records"]:
        p, o = record["perceived"], record["oracle"]
        print(f"{record['problem']}: perceived wv {p['mechanism']['wv_mean']} tags "
              f"{p['mechanism']['tags']} {p['answers']} | oracle wv {o['mechanism']['wv_mean']} "
              f"tags {o['mechanism']['tags']} {o['answers']} | missed {record['roles_read']['missed']}")


def main(argv=None) -> int:
    p = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    p.add_argument("--runs", type=int, default=200)
    p.add_argument("--seed", type=int, default=1)
    p.add_argument("--out", default=str(DEFAULT_OUT))
    p.add_argument("--problems", nargs="*", default=None)
    p.add_argument("--no-lesions", action="store_true", help="the carriers' probes only")
    p.add_argument("--quiet", action="store_true")
    args = p.parse_args(argv)
    names = args.problems or RP.oracle_problems()
    started = time.time()
    jobs = plan(names, args.runs, args.seed, not args.no_lesions)
    outputs = [job(spec) for spec in jobs]
    out = assemble(jobs, outputs, names, args.runs, args.seed, time.time() - started)
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    Path(args.out).write_text(json.dumps(out, indent=1) + "\n")
    if not args.quiet:
        say(out)
        print(f"wrote {args.out} in {out['seconds']} s")
    return 0


if __name__ == "__main__":
    sys.exit(main())

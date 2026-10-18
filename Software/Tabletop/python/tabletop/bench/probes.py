"""What the probe benches share (D-073): a `Variant` of a configuration (a lesion, a
value of rho, one of French's faults restored), the surveys of several variants over a
few problems, their one table per problem, their JSON records, a few readings of a survey
(the frequency of a set of objects, the literal answer's share, the main answer) and the
rank correlation the `humans` bench reports.

A probe bench changes one thing about the engine and measures what moves [FCCA p. 365];
nothing it prints is ever used to set a default (ANSWERS.md 1; D-072).
"""

import dataclasses
from dataclasses import dataclass

from tabletop import reported as RP
from tabletop.bench import harness as H
from tabletop.config import PROFILES, Config
from tabletop.problems import Problem, load_problem

# The problems the probe benches run by default (D-073): one of each family French
# reports, the chapter 4 configuration first [F95 pp. 96, 118, 123, 128, 129].
DEFAULT_PROBLEMS = ("surround-05", "blockage-02", "blockage-06", "buridan-01")


@dataclass(frozen=True)
class Variant:
    """One configuration a probe bench runs (D-073): its `name`, the `config`, the
    `changes` from the base profile as (field, repr) pairs, and a `note` saying what it
    is for, with its page where a source names it."""

    name: str
    config: Config
    changes: tuple
    note: str = ""


def variant(base: Config, name: str, note: str = "", **changes) -> Variant:
    """A variant of `base` with these fields changed (D-073); the profile's name is
    kept, so that every run records the base profile and the differences from it."""
    config = dataclasses.replace(base, **changes) if changes else base
    return Variant(name, config, tuple((k, repr(v)) for k, v in changes.items()), note)


def intact(base: Config) -> Variant:
    """The base configuration itself, the first row of every probe table (D-073)."""
    return variant(base, "intact", "the profile as it is")


def problems_of(sets, only=None) -> list:
    """The problem names of a tuple of names, kept to `only` when given (D-073)."""
    return [name for name in sets if only is None or name in only]


def frequency(sv: H.Survey, ids) -> float:
    """The share of a survey's runs, in percent, whose answer is one of `ids` (D-073)."""
    return sum(t.frequency for t in sv.tallies if t.answer in ids)


def label_frequency(sv: H.Survey, problem: Problem, label: str) -> float:
    """The share of runs that gave the object French labels `label` (`E1+E2` takes both)
    [F95 p. 123] (D-073); 0 when the problem has no such label."""
    ids = [problem.answers[part] for part in label.split("+") if part in problem.answers]
    return frequency(sv, ids)


def literal_frequency(sv: H.Survey, problem: Problem) -> float:
    """The share of runs that gave the touched object itself (D-063, D-073)."""
    return frequency(sv, [problem.touched])


def main_tally(sv: H.Survey) -> H.Tally:
    """The answer most runs gave, the earliest among ties (D-073)."""
    return max(sv.tallies, key=lambda t: t.count)


def french_main(problem: str):
    """French's most frequent answer for a problem (its label and frequency) from the
    first of its chapter 5 result sets [F95 pp. 115-139], or None (D-073)."""
    sets = RP.result_sets(problem)
    if not sets:
        return None
    best = RP.most_frequent(sets[0])
    return best


def mean_structure(sv: H.Survey) -> float:
    """The mean structure value over every run of a survey (D-062, D-073)."""
    return sum(r[2] for r in sv.results) / sv.runs


def ranks(values) -> list:
    """The ranks of values from the smallest up, ties sharing their average rank
    (D-073)."""
    order = sorted(range(len(values)), key=lambda i: values[i])
    out = [0.0] * len(values)
    i = 0
    while i < len(order):
        j = i
        while j + 1 < len(order) and values[order[j + 1]] == values[order[i]]:
            j += 1
        for k in range(i, j + 1):
            out[order[k]] = (i + j) / 2.0 + 1.0
        i = j + 1
    return out


def spearman(xs, ys) -> float | None:
    """Spearman's rank correlation of two equal-length sequences, by Pearson's formula
    on their ranks (ties averaged); None with fewer than two pairs or a constant side
    (D-073)."""
    if len(xs) != len(ys):
        raise ValueError("the two sequences need the same length")
    n = len(xs)
    if n < 2:
        return None
    rx, ry = ranks(xs), ranks(ys)
    mx, my = sum(rx) / n, sum(ry) / n
    sxy = sum((a - mx) * (b - my) for a, b in zip(rx, ry))
    sxx = sum((a - mx) ** 2 for a in rx)
    syy = sum((b - my) ** 2 for b in ry)
    if sxx == 0.0 or syy == 0.0:
        return None
    return sxy / (sxx * syy) ** 0.5


# ---- the surveys of several variants over several problems (D-073) ---------------------

def survey_variants(variants: list, problems: list, seed: int, runs: int,
                    progress=None) -> dict:
    """One survey per (variant name, problem name), in that order, each problem's runs
    from the same batch seed under every variant (D-072, D-073), so that a variant's
    rows differ from the intact ones by the configuration alone."""
    out = {}
    for v in variants:
        for name in problems:
            sv = H.survey(load_problem(name), v.config, seed, runs)
            out[(v.name, name)] = sv
            if progress is not None:
                progress(sv)
    return out


def _num(value, digits: int = 1) -> str:
    return "-" if value is None else f"{value:.{digits}f}"


def variant_table(problem: str, variants: list, surveys: dict) -> list:
    """One problem's table: a row per variant with the main answer and its share,
    French's main answer for comparison [F95 pp. 115-139], the share of the literal
    answer, the share of the answer French found most often, the mean codelets, the mean
    structure value and the capped runs (D-073)."""
    pb = load_problem(problem)
    french = french_main(problem)
    heading = f"### {problem}"
    if french is not None:
        heading += (f" (French's main answer {french.label}, {french.frequency} percent "
                    f"[F95 p. {RP.result_sets(problem)[0].page}])")
    lines = [heading, "",
             "| Variant | Main answer | % | French's main, % | Literal, % | Mean codelets "
             "| Mean structure | Capped |", "|---|---|---:|---:|---:|---:|---:|---:|"]
    for v in variants:
        sv = surveys[(v.name, problem)]
        top = main_tally(sv)
        who = f"{top.answer} ({top.label})" if top.label else top.answer
        his = french.label if french is not None else None
        his_share = label_frequency(sv, pb, his) if his else None
        lines.append(f"| {v.name} | {who} | {top.frequency:.1f} | {_num(his_share)} | "
                     f"{literal_frequency(sv, pb):.1f} | {sv.codelets:.1f} | "
                     f"{mean_structure(sv):.1f} | {sv.capped} |")
    return lines + [""]


def variants_table(variants: list) -> list:
    """The variants themselves: name, the fields changed and the note (D-073)."""
    lines = ["| Variant | Changes | What it is |", "|---|---|---|"]
    for v in variants:
        changes = ", ".join(f"{k} = {val}" for k, val in v.changes) or "none"
        lines.append(f"| {v.name} | {changes} | {v.note} |")
    return lines + [""]


def variant_records(bench: str, variants: list, problems: list, surveys: dict) -> list:
    """The JSON records of a probe bench: one per (variant, problem) with the survey
    (D-073)."""
    out = []
    for v in variants:
        for name in problems:
            out.append({"bench": bench, "variant": v.name,
                        "changes": [list(c) for c in v.changes],
                        "note": v.note, "survey": H.survey_record(surveys[(v.name, name)])})
    return out


def base_profile(config: Config) -> Config:
    """The profile a configuration came from, by its name (D-066, D-073)."""
    return PROFILES.get(config.profile, config)

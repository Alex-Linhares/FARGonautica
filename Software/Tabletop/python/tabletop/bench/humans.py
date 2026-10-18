"""The `humans` bench: Tables 5.1-5.3 and Figure 5.4, the subjects against the program
(D-073; ANSWERS.md 1 and 7).

French gave 23 of the problems to "thirty-four students" [F95 p. 142] and prints, in
Tables 5.1-5.3 [F95 pp. 143-144], the subjects' percentages beside those of a second set
of runs made after he "slightly increased the relative importance that the program
attached to object-type matches" [F95 p. 143]; Figure 5.4 orders the problems by the gap
between the subjects' most frequent answer and the program's frequency for it [F95
p. 143 (img)], and he finds that on over half of the problems the two differ by at most
ten points [F95 p. 144]. This bench surveys the 23 problems under two profiles, the one
given (`standard` by default) and `human` (the profile that is to carry French's raised
weight, item 26; equal to `standard` until then), and prints the four columns side by
side: the subjects, Tabletop's second runs, ours under each profile; then Figure 5.4's
order with his fit and ours, the rank correlations, the within-ten-points counts, and the
other human data French gives [F95 pp. 118, 122; FCCA pp. 396-397; FH91 p. 181]: every
numbered one is about the touched cup, and the two unnumbered ones about Surround 5 are
about the isolated cup. The `human` profile's calibration is item 26's and is
reported apart (ANSWERS.md 7); nothing here fits anything.
"""

from dataclasses import dataclass

from tabletop import reported as RP
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import PROFILES, Config
from tabletop.problems import Problem, catalogue, load_problem

SETS = RP.SUBJECT_ROWS
TEN_POINTS = 10   # "no more than 10 percentage points" [F95 p. 144]


def problems_of(sets=SETS, only=None) -> list:
    """The 23 problems of the tables, in the tables' order (D-073)."""
    out = []
    for row in sets:
        if row.problem not in out and (only is None or row.problem in only):
            out.append(row.problem)
    return out


def answer_ids(row: RP.SubjectRow, problem: Problem) -> tuple:
    """The objects behind the row's two columns (D-073): for Tables 5.1 and 5.2 the
    objects labelled E1 and E2 [F95 p. 143]; for Table 5.3 "Either small glass" is every
    labelled small glass and "Originally touched cup" the touched object [F95 p. 144]."""
    if row.table == "5.3":
        glasses = [o.id for o in problem.objects
                   if o.type == "small glass" and o.id in problem.answers.values()]
        return (tuple(glasses), (problem.touched,))
    return ((problem.answers[row.labels[0]],), (problem.answers[row.labels[1]],))


def our_fit(row: RP.SubjectRow, ours: tuple) -> float:
    """Figure 5.4's gap for our percentages, as `reported.fit` computes French's: the
    subjects' most frequent answer against ours for it; for Blockage 5 the remainder of
    both columns [F95 p. 143] (SPEC.md §7)."""
    if row.problem == "blockage-05":
        return abs((100 - sum(row.subjects)) - (100 - sum(ours)))
    i = row.most_frequent_for_subjects()
    return abs(row.subjects[i] - ours[i])


@dataclass(frozen=True)
class HumanComparison:
    """One table row beside our surveys (D-073): the `row`, the `surveys` by profile,
    `ours` by profile as the two percentages, our `fits` by profile and French's
    `french_fit`."""

    row: RP.SubjectRow
    surveys: dict
    ours: dict
    fits: dict
    french_fit: int


def profiles_for(config: Config) -> list:
    """The two configurations the bench runs: the one given and `human`, or `standard`
    when `human` itself is given (D-073)."""
    other = PROFILES["human"] if config.profile != "human" else PROFILES["standard"]
    return [config, other]


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return 2 * len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> list:
    """Survey the problems of the tables under both profiles and put each row beside
    ours (D-073)."""
    names = problems_of(sets, only)
    surveys = {c.profile: H.surveys_for(names, load_problem, c, seed, runs, progress)
               for c in profiles_for(config)}
    out = []
    for row in sets:
        if row.problem not in names:
            continue
        pb = load_problem(row.problem)
        ids = answer_ids(row, pb)
        ours = {prof: (P.frequency(sv[row.problem], ids[0]), P.frequency(sv[row.problem], ids[1]))
                for prof, sv in surveys.items()}
        out.append(HumanComparison(row, {prof: sv[row.problem] for prof, sv in surveys.items()},
                                   ours, {prof: our_fit(row, o) for prof, o in ours.items()},
                                   RP.fit(row)))
    return out


def correlations(comparisons: list, profile: str) -> dict:
    """Spearman's rank correlation, over the problems run, of our percentage for the
    subjects' most frequent answer with the subjects' own, and with Tabletop's second
    run's; and of our Figure 5.4 fit with French's (D-073, measure M8)."""
    subjects, tabletop, ours, his_fit, our_fit_ = [], [], [], [], []
    for c in comparisons:
        i = c.row.most_frequent_for_subjects()
        subjects.append(c.row.subjects[i])
        tabletop.append(c.row.tabletop[i])
        ours.append(c.ours[profile][i])
        his_fit.append(c.french_fit)
        our_fit_.append(c.fits[profile])
    return {"with_subjects": P.spearman(subjects, ours),
            "with_tabletop": P.spearman(tabletop, ours),
            "fit_with_french": P.spearman(his_fit, our_fit_)}


def within_ten(comparisons: list, profile: str | None) -> int:
    """How many problems lie within ten points of the subjects [F95 p. 144]: French's
    fits when `profile` is None, ours under the profile otherwise (D-073)."""
    if profile is None:
        return sum(c.french_fit <= TEN_POINTS for c in comparisons)
    return sum(c.fits[profile] <= TEN_POINTS for c in comparisons)


def _r(value) -> str:
    return "-" if value is None else f"{value:+.2f}"


def report(comparisons: list) -> list:
    """The bench's text: the three tables with our two profiles beside the subjects and
    Tabletop, Figure 5.4's order, the correlations and the other human data (D-073)."""
    if not comparisons:
        return ["## Tables 5.1-5.3 [F95 pp. 143-144]", "", "(not run)", ""]
    profiles = list(comparisons[0].surveys)
    a, b = profiles
    lines = []
    for table, title in (("5.1", "Table 5.1, Buridan [F95 p. 143]"),
                         ("5.2", "Table 5.2, Surround [F95 p. 143]"),
                         ("5.3", "Table 5.3, Blockage [F95 p. 144]")):
        mine = [c for c in comparisons if c.row.table == table]
        if not mine:
            continue
        l0, l1 = mine[0].row.labels
        lines += [f"## {title}", "",
                  f"| Problem | {l0}: subjects | TT | {a} | {b} | {l1}: subjects | TT | {a} | {b} "
                  f"| Footnote |", "|---|---:|---:|---:|---:|---:|---:|---:|---:|---|"]
        for c in mine:
            r = c.row
            lines.append(f"| {r.problem} | {r.subjects[0]} | {r.tabletop[0]} | "
                         f"{c.ours[a][0]:.1f} | {c.ours[b][0]:.1f} | {r.subjects[1]} | "
                         f"{r.tabletop[1]} | {c.ours[a][1]:.1f} | {c.ours[b][1]:.1f} | "
                         f"{r.footnote or '-'} |")
        lines.append("")
    lines += ["## Figure 5.4: goodness of fit [F95 pp. 143-144]", "",
              "French's order, best fit first, with his gap and ours (the subjects' most "
              "frequent answer against the program's frequency for it):", "",
              f"| Label | Problem | Gap, French | Gap, {a} | Gap, {b} |", "|---|---|---:|---:|---:|"]
    by_name = {c.row.problem: c for c in comparisons}
    for label, name in RP.FIGURE_5_4:
        if name in by_name:
            c = by_name[name]
            lines.append(f"| {label} | {name} | {c.french_fit} | {c.fits[a]:.1f} | "
                         f"{c.fits[b]:.1f} |")
    n = len(comparisons)
    lines += ["", f"Within ten points of the subjects: French {within_ten(comparisons, None)} "
                  f"of {n} ('over half' [F95 p. 144]); {a} {within_ten(comparisons, a)} of {n}; "
                  f"{b} {within_ten(comparisons, b)} of {n}.", "",
              "| Profile | Spearman with the subjects' % | with Tabletop's second runs | "
              "of our gap with French's |", "|---|---:|---:|---:|"]
    for prof in profiles:
        co = correlations(comparisons, prof)
        lines.append(f"| {prof} | {_r(co['with_subjects'])} | {_r(co['with_tabletop'])} | "
                     f"{_r(co['fit_with_french'])} |")
    lines += ["", "## The other human data [F95 pp. 118, 122; FCCA pp. 396-397; FH91 p. 181]",
              "", f"| Problem | Source | Page | What | Subjects, % | Object | {a}, % | {b}, % |",
              "|---|---|---:|---|---:|---|---:|---:|"]
    for d in RP.HUMAN_DATA:
        if d.problem not in by_name:
            continue
        c = by_name[d.problem]
        obj = datum_object(d, load_problem(d.problem))
        ours = {p: P.frequency(c.surveys[p], [obj]) for p in profiles}
        lines.append(f"| {d.problem} | {d.source} | {d.page} | {d.what} | "
                     f"{'-' if d.percent is None else d.percent} | {obj} | {ours[a]:.1f} | "
                     f"{ours[b]:.1f} |")
    return lines + [""]


def datum_object(datum: RP.HumanDatum, problem: Problem) -> str:
    """The object a human datum is about (D-073): the isolated cup, E2, for the two
    statements about Surround 5 [F95 p. 118]; the touched cup for every other [F95
    p. 122; FCCA pp. 396-397; FH91 p. 181]."""
    if "isolated cup" in datum.what:
        return problem.answers["E2"]
    return problem.touched


def records(comparisons: list) -> list:
    """The JSON records (D-073)."""
    return [{"bench": "humans", "problem": c.row.problem, "table": c.row.table,
             "page": c.row.page, "labels": list(c.row.labels),
             "subjects": list(c.row.subjects), "tabletop": list(c.row.tabletop),
             "french_fit": c.french_fit,
             "ours": {p: list(v) for p, v in c.ours.items()}, "fits": c.fits,
             "surveys": {p: H.survey_record(sv) for p, sv in c.surveys.items()}}
            for c in comparisons]


def human_data_problems() -> list:
    """The problems the other human data name that the catalogue holds (D-073)."""
    return [d.problem for d in RP.HUMAN_DATA if d.problem in catalogue()]

"""The bench harness (D-072): a `Survey` of one problem over a batch of runs, tallied per
answer as French tallies his [F95 p. 115]; a `Comparison` of a survey with one of French's
`ResultSet`s, answer by answer, with the two-sample z of each frequency and a chi-square
for the whole problem (`stats.py`); the tables in French's format, and the JSON record.

French prints, for each configuration, a bar graph of the frequencies and a table headed
"Object Eliza touches", "Average structure", "No. of codelets run" [F95 p. 115]; the
comparison's table keeps his three headings and puts our column beside each of his. The
survey's runs come from one batch seed derived from the bench's seed and the problem's
name, so that any run can be replayed alone from its own seed (D-064, D-072).
"""

import hashlib
from dataclasses import dataclass

from tabletop.bench import stats as ST
from tabletop.config import Config
from tabletop.problems import Problem
from tabletop.reported import ResultSet
from tabletop.accel import Rng, run_seeds
from tabletop.run import Result, run_problem

OTHER = "other"   # the row for answers French prints no bar for [F95 p. 115] (D-072)


def batch_seed_for(seed: int, problem: str) -> int:
    """The batch seed of a problem's survey under a bench seed (D-072): the first output
    of the generator seeded with the bench seed combined with the problem's name (the
    first eight bytes of its SHA-256), so that a survey does not depend on which other
    problems the bench runs, and every run is replayable from `run_seeds(batch_seed, n)`
    (D-064)."""
    name_hash = int.from_bytes(hashlib.sha256(problem.encode("utf-8")).digest()[:8], "big")
    return Rng((seed ^ name_hash) & 0xFFFFFFFFFFFFFFFF).next_u64()


@dataclass(frozen=True)
class Tally:
    """One answer's numbers over a survey, as French reports them [F95 p. 115]: the object
    `answer` (its id), its `label` in the problem file (`E1`; empty when none), the
    `count` of runs, the `frequency` in percent, and the mean `structure` value and mean
    `codelets` of the runs that gave it (None when none did) (D-072)."""

    answer: str
    label: str
    count: int
    frequency: float
    structure: float | None
    codelets: float | None


@dataclass(frozen=True)
class Survey:
    """One problem run over a batch (D-072): the `problem`, `profile`, bench `seed`,
    `batch_seed` and number of `runs`; the `tallies` in the order the answers first
    came up; the `capped` count (D-061); the mean `codelets` over all runs; and
    `results`, every run's (seed, answer, structure value, codelets, capped), so that a
    run can be replayed from its seed (D-064)."""

    problem: str
    profile: str
    seed: int
    batch_seed: int
    runs: int
    tallies: tuple
    capped: int
    codelets: float
    results: tuple

    def tally(self, answer: str) -> Tally | None:
        """The tally of an object, or None when no run gave it (D-072)."""
        for t in self.tallies:
            if t.answer == answer:
                return t
        return None


def tallies_of(results: list, problem: Problem) -> tuple:
    """French's three numbers for every answer of a batch [F95 p. 115], in the order the
    answers first came up (D-072)."""
    order, groups = [], {}
    for r in results:
        if r.answer not in groups:
            order.append(r.answer)
            groups[r.answer] = []
        groups[r.answer].append(r)
    out = []
    for answer in order:
        rs = groups[answer]
        label = next((k for k, v in problem.answers.items() if v == answer), "")
        out.append(Tally(answer, label, len(rs), 100.0 * len(rs) / len(results),
                         sum(r.structure_value for r in rs) / len(rs),
                         sum(r.codelets for r in rs) / len(rs)))
    return tuple(out)


def survey_of(problem: Problem, config: Config, seed: int, results: list) -> Survey:
    """The survey of runs already made from the problem's batch seed under the bench
    `seed` (D-072): what `survey` builds, for a bench that steps its runs itself."""
    return Survey(problem.name, config.profile, seed, batch_seed_for(seed, problem.name),
                  len(results), tallies_of(results, problem), sum(r.capped for r in results),
                  sum(r.codelets for r in results) / len(results),
                  tuple((r.seed, r.answer, r.structure_value, r.codelets, r.capped)
                        for r in results))


def survey(problem: Problem, config: Config, seed: int, runs: int) -> Survey:
    """Run a problem `runs` times from the bench `seed` and tally the answers as French
    does [F95 pp. 114-115] (D-072)."""
    batch_seed = batch_seed_for(seed, problem.name)
    results = [run_problem(problem, config, s) for s in run_seeds(batch_seed, runs)]
    return survey_of(problem, config, seed, results)


def survey_record(sv: Survey) -> dict:
    """A survey as plain data (D-072): its identity, the tallies and every run."""
    return {"problem": sv.problem, "profile": sv.profile, "seed": sv.seed,
            "batch_seed": sv.batch_seed, "runs": sv.runs, "capped": sv.capped,
            "mean_codelets": sv.codelets,
            "tallies": [{"answer": t.answer, "label": t.label, "count": t.count,
                         "frequency": t.frequency, "structure": t.structure,
                         "codelets": t.codelets} for t in sv.tallies],
            "results": [list(r) for r in sv.results]}


@dataclass(frozen=True)
class Row:
    """One answer of a comparison (D-072): French's `label` (`E1`, `E1+E2`, or `other`)
    and what it `names`; his `french_frequency`, `french_structure`, `french_codelets`
    [F95 p. 115]; our `count`, `frequency`, `structure` and `codelets` over the runs whose
    label is one of his (`answers` lists the objects behind them with their counts); and
    the two-sample `z` of the two frequencies against his fifty runs [F95 p. 114]."""

    label: str
    names: str
    french_frequency: float | None
    french_structure: float | None
    french_codelets: float | None
    count: int
    frequency: float
    structure: float | None
    codelets: float | None
    answers: tuple
    z: float


@dataclass(frozen=True)
class Comparison:
    """A survey beside one of French's result sets (D-072): the `set` (its problem,
    source, page, program and figure), the `survey`, one `Row` per answer he prints plus
    `other` when a run gave an unlabelled object, and the chi-square of homogeneity
    between our counts and his over those rows (`chi_square`, `df`, `p`)."""

    set: ResultSet
    survey: Survey
    rows: tuple
    chi_square: float
    df: int
    p: float

    @property
    def title(self) -> str:
        """French's heading with its source and page [F95 p. 115] (D-072)."""
        figure = f" {self.set.figure}" if self.set.figure else ""
        return f"{self.set.title} [{self.set.source}{figure} p. {self.set.page}]"


def _merge(tallies: list) -> tuple:
    """The count, frequency and means of several tallies taken together (D-072)."""
    count = sum(t.count for t in tallies)
    if count == 0:
        return 0, 0.0, None, None
    structure = sum(t.structure * t.count for t in tallies) / count
    codelets = sum(t.codelets * t.count for t in tallies) / count
    return count, sum(t.frequency for t in tallies), structure, codelets


def compare(sv: Survey, rs: ResultSet, french_runs: int = ST.FRENCH_RUNS) -> Comparison:
    """Put a survey beside a result set, answer by answer (D-072): a run counts under
    French's label when its own label is one of the labels joined in his (`E1+E2` takes
    `E1` and `E2` [F95 p. 123]); runs with no label of his fall under `other`, for which
    he prints no bar (his frequencies sum to 100). Each frequency gets the two-sample z
    against his `french_runs` (D-072), the problem a chi-square over the rows."""
    rows, ours, his = [], [], []
    taken = []
    for a in rs.answers:
        labels = a.label.split("+")
        mine = [t for t in sv.tallies if t.label and t.label in labels]
        taken += mine
        count, frequency, structure, codelets = _merge(mine)
        french_count = ST.french_count(a.frequency, french_runs)
        z = ST.two_sample_z(count, sv.runs, french_count, french_runs)
        rows.append(Row(a.label, a.names, a.frequency, a.structure, a.codelets, count,
                        frequency, structure, codelets,
                        tuple((t.answer, t.count) for t in mine), z))
        ours.append(count)
        his.append(french_count)
    rest = [t for t in sv.tallies if t not in taken]
    if rest:
        count, frequency, structure, codelets = _merge(rest)
        french_rest = french_runs - sum(his)
        z = ST.two_sample_z(count, sv.runs, max(0.0, french_rest), french_runs)
        rows.append(Row(OTHER, "answers French prints no bar for", None, None, None, count,
                        frequency, structure, codelets,
                        tuple((t.answer, t.count) for t in rest), z))
        ours.append(count)
        his.append(max(0.0, french_rest))
    statistic, df = ST.chi_square_homogeneity(ours, his)
    return Comparison(rs, sv, tuple(rows), statistic, df, ST.chi_square_p(statistic, df))


def _num(value, digits: int = 0) -> str:
    """A number for a table cell; a dash where French prints one [F95 p. 119] (D-072)."""
    if value is None:
        return "-"
    return f"{value:.{digits}f}"


def markdown_table(c: Comparison) -> list:
    """The comparison in French's format [F95 p. 115]: his three headings, each with our
    column beside his, then the z of each frequency; a line with the runs, the batch seed,
    the capped runs and the chi-square (D-072)."""
    sv = c.survey
    program = f", {c.set.program} program" if c.set.program != "original" else ""
    lines = [f"### {c.title}{program}", "",
             "| Object Eliza touches | Frequency, French | Frequency, ours | z | "
             "Average structure, French | ours | No. of codelets run, French | ours |",
             "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for r in c.rows:
        who = r.label if r.label != OTHER else "other (" + " ".join(
            f"{a} {n}" for a, n in r.answers) + ")"
        lines.append(f"| {who} | {_num(r.french_frequency)} | {r.frequency:.1f} | {r.z:+.2f} | "
                     f"{_num(r.french_structure)} | {_num(r.structure, 1)} | "
                     f"{_num(r.french_codelets)} | {_num(r.codelets, 1)} |")
    lines += ["", f"{sv.runs} runs of {sv.problem} under {sv.profile} (bench seed {sv.seed}, "
                  f"batch seed {sv.batch_seed}), {sv.capped} capped, mean codelets "
                  f"{sv.codelets:.1f}; chi-square {c.chi_square:.2f} on {c.df} df, "
                  f"p = {c.p:.3f}; French's n = {ST.FRENCH_RUNS}.", ""]
    return lines


def summary_header() -> list:
    """The heading of the one-line-per-set summary table (D-072)."""
    return ["| Result set | Page | Main answer | French % | Ours % | z | chi-square p |",
            "|---|---:|---|---:|---:|---:|---:|"]


def summary_row(c: Comparison) -> str:
    """One line of the summary: the set, its page, French's most frequent answer, his and
    our frequency for it, the z and the chi-square p (D-072)."""
    main = max((r for r in c.rows if r.french_frequency is not None),
               key=lambda r: r.french_frequency)
    figure = f"{c.set.figure} " if c.set.figure else ""
    return (f"| {c.set.title} ({figure}{c.set.source}) | {c.set.page} | {main.label} | "
            f"{main.french_frequency:.0f} | {main.frequency:.1f} | {main.z:+.2f} | {c.p:.3f} |")


def record_of(c: Comparison) -> dict:
    """The comparison as plain data for `--json` (D-072): the set's identity, the survey
    with every run's (seed, answer, structure, codelets, capped), the rows and the
    chi-square."""
    sv = c.survey
    return {"problem": c.set.problem, "title": c.set.title, "source": c.set.source,
            "page": c.set.page, "figure": c.set.figure, "program": c.set.program,
            "profile": sv.profile, "seed": sv.seed, "batch_seed": sv.batch_seed,
            "runs": sv.runs, "capped": sv.capped, "mean_codelets": sv.codelets,
            "rows": [{"label": r.label, "names": r.names,
                      "french": {"frequency": r.french_frequency, "structure": r.french_structure,
                                 "codelets": r.french_codelets},
                      "ours": {"count": r.count, "frequency": r.frequency,
                               "structure": r.structure, "codelets": r.codelets,
                               "answers": list(r.answers)},
                      "z": r.z} for r in c.rows],
            "chi_square": c.chi_square, "df": c.df, "p": c.p,
            "results": [list(r) for r in sv.results]}


def surveys_for(problems: list, loader, config: Config, seed: int, runs: int,
                progress=None) -> dict:
    """One survey per problem name, in the order given (D-072); `loader` turns a name
    into a `Problem`, `progress`, if given, is called with each finished survey."""
    out = {}
    for name in problems:
        sv = survey(loader(name), config, seed, runs)
        out[name] = sv
        if progress is not None:
            progress(sv)
    return out

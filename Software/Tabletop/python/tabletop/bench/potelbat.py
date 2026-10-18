"""The `potelbat` bench: the brute-force baseline's answers on every problem, beside
French's and Tabletop's (D-067, D-073; measure M9).

FCCA's Potelbat, a weighted sum of terms, "performed reasonably well on simple Tabletop
problems, although on some trickier ones" it "did poorly" [FCCA p. 328]; it cannot see
that an object is already taken, as in a blockage problem [FCCA p. 355], and "sees no
relationships between objects, makes no groupings, builds no bridges" [FCCA p. 356]. The
bench prints, for every problem of the catalogue, the Potelbat's answer and its margin
(`tabletop/potelbat.py`), French's answer (the most frequent one of chapter 5, or the one
chapter 2 names) and Tabletop's most frequent answer over a survey, and counts where each
agrees with French.
"""

from dataclasses import dataclass

from tabletop import potelbat as PB
from tabletop import reported as RP
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import Config
from tabletop.problems import catalogue, load_problem

SETS = tuple(catalogue())
problems_of = P.problems_of


@dataclass(frozen=True)
class PotelbatComparison:
    """One problem (D-073): its `name`, French's answer as an object id (`french`, with
    its `french_label` and source `page`; None where he names none), the baseline's
    `score` and `margin`, and Tabletop's `survey` and `top` tally."""

    name: str
    french: str | None
    french_label: str
    page: int | None
    score: PB.Score
    margin: float
    survey: H.Survey
    top: H.Tally


def french_answer(problem) -> tuple:
    """French's answer for a problem as (object id, label, page): the most frequent of its
    first chapter 5 result set [F95 pp. 115-139] (a joint label's first object), or the
    one its chapter 2 text names [F95 pp. 26-35]; (None, "", None) otherwise (D-073)."""
    sets = RP.result_sets(problem.name)
    if sets:
        best = RP.most_frequent(sets[0])
        first = best.label.split("+")[0]
        return problem.answers.get(first), best.label, sets[0].page
    exp = problem.expected or {}
    if exp.get("answer"):
        return exp["answer"], exp["answer"], problem.source["page"]
    return None, "", None


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> list:
    """The baseline on every problem, and a survey of Tabletop on each (D-073)."""
    names = problems_of(sets, only)
    surveys = H.surveys_for(names, load_problem, config, seed, runs, progress)
    out = []
    for name in names:
        pb = load_problem(name)
        french, label, page = french_answer(pb)
        out.append(PotelbatComparison(name, french, label, page, PB.answer(pb, config),
                                      PB.margin(pb, config), surveys[name],
                                      P.main_tally(surveys[name])))
    return out


def agreements(comparisons: list) -> tuple:
    """(problems with a French answer, Potelbat agreements, Tabletop agreements) (D-073)."""
    with_answer = [c for c in comparisons if c.french is not None]
    return (len(with_answer), sum(c.score.id == c.french for c in with_answer),
            sum(c.top.answer == c.french for c in with_answer))


def _label(problem, id_: str) -> str:
    return next((k for k, v in problem.answers.items() if v == id_), "")


def report(comparisons: list) -> list:
    """The bench's text: one row per problem, then the counts (D-073)."""
    lines = ["## The Potelbat baseline against French and Tabletop [FCCA pp. 327-328]", "",
             "| Problem | French's answer | Page | Potelbat | Margin | Potelbat agrees | "
             "Tabletop's most frequent | % | Tabletop agrees |",
             "|---|---|---:|---|---:|---|---|---:|---|"]
    for c in comparisons:
        pb = load_problem(c.name)
        his = f"{c.french} ({c.french_label})" if c.french else "-"
        mine = c.score.id + (f" ({_label(pb, c.score.id)})" if _label(pb, c.score.id) else "")
        top = c.top.answer + (f" ({c.top.label})" if c.top.label else "")
        yes = lambda ok: "-" if c.french is None else ("yes" if ok else "no")
        lines.append(f"| {c.name} | {his} | {c.page or '-'} | {mine} | {c.margin:.1f} | "
                     f"{yes(c.score.id == c.french)} | {top} | {c.top.frequency:.1f} | "
                     f"{yes(c.top.answer == c.french)} |")
    n, pot, tab = agreements(comparisons)
    lines += ["", f"Of {n} problems with an answer of French's, the Potelbat gives it on {pot} "
                  f"and Tabletop's most frequent answer is it on {tab}.", ""]
    return lines


def records(comparisons: list) -> list:
    """The JSON records (D-073)."""
    return [{"bench": "potelbat", "problem": c.name, "french": c.french,
             "french_label": c.french_label, "page": c.page,
             "potelbat": {"answer": c.score.id, "type": c.score.type,
                          "distance": c.score.distance, "type_term": c.score.type_term,
                          "place_term": c.score.place_term, "total": c.score.total,
                          "margin": c.margin},
             "survey": H.survey_record(c.survey)} for c in comparisons]

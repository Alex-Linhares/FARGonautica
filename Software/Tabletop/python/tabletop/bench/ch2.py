"""The `ch2` bench: chapter 2's nineteen configurations against French's words (D-073).

Chapter 2 presents the domain through a series of tables, Figures 2.2 to 2.20 [F95
pp. 26-35], each with French's expectation in words: the most likely answer where he names
one, the plausible answers, the unlikely ones, and how a variant shifts the balance from
the figure before it ("toward" an object, more or less than another figure). The problem
files record these in their `expected` block (D-003; `docs/problems.md` 9). This bench
surveys every configuration and checks each statement against our frequencies, as a
yes-or-no reading (measure M2): French's named answer is our most frequent; every plausible
answer appears; every unlikely answer is rarer than the most frequent; and a shift toward
an object is a higher frequency for it than in the figure compared with. None of these
is a number of his; a miss is a finding (ANSWERS.md 1).
"""

from dataclasses import dataclass

from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import Config
from tabletop.problems import by_family, load_problem

SETS = tuple(by_family("ch2"))
problems_of = P.problems_of


@dataclass(frozen=True)
class Check:
    """One statement of French's read against a survey (D-073): its `text`, our
    `numbers` as a string, and the `verdict` (True, False, or None when the figure it
    compares with was not run)."""

    text: str
    numbers: str
    verdict: bool | None


@dataclass(frozen=True)
class Ch2Comparison:
    """One configuration (D-073): the problem `name`, its `page`, French's `expected`
    block, the `survey`, our `top` tally and the `checks`."""

    name: str
    page: int
    expected: dict
    survey: H.Survey
    top: H.Tally
    checks: tuple


def _pct(sv: H.Survey, id_: str) -> float:
    return P.frequency(sv, [id_])


def checks_of(name: str, surveys: dict) -> list:
    """French's statements about one configuration, each read against the surveys
    (D-073)."""
    pb, sv = load_problem(name), surveys[name]
    exp = pb.expected or {}
    top = P.main_tally(sv)
    out = []
    if exp.get("answer"):
        a = exp["answer"]
        out.append(Check(f"French's answer {a} is our most frequent",
                         f"{a} {_pct(sv, a):.1f}%, top {top.answer} {top.frequency:.1f}%",
                         top.answer == a))
    for p in exp.get("plausible", []):
        out.append(Check(f"plausible {p} appears", f"{_pct(sv, p):.1f}%", _pct(sv, p) > 0.0))
    for u in exp.get("unlikely", []):
        out.append(Check(f"unlikely {u} is rarer than the most frequent",
                         f"{u} {_pct(sv, u):.1f}% against {top.frequency:.1f}%",
                         _pct(sv, u) < top.frequency))
    for c in exp.get("comparisons", []):
        other = c.get("more_than") or c.get("less_than")
        toward = c["toward"]
        if other not in surveys:
            out.append(Check(f"toward {toward} {'more' if 'more_than' in c else 'less'} "
                             f"than {other}", "not run", None))
            continue
        here, there = _pct(sv, toward), _pct(surveys[other], toward)
        if "more_than" in c:
            out.append(Check(f"toward {toward} more than {other}",
                             f"{here:.1f}% against {there:.1f}%", here > there))
        else:
            out.append(Check(f"toward {toward} less than {other}",
                             f"{here:.1f}% against {there:.1f}%", here < there))
    return out


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> list:
    """Survey the configurations and read French's statements against them (D-073)."""
    names = problems_of(sets, only)
    surveys = H.surveys_for(names, load_problem, config, seed, runs, progress)
    out = []
    for name in names:
        pb = load_problem(name)
        out.append(Ch2Comparison(name, pb.source["page"], pb.expected or {}, surveys[name],
                                 P.main_tally(surveys[name]), tuple(checks_of(name, surveys))))
    return out


def tallied(comparisons: list) -> tuple:
    """(agreements, disagreements, not run) over every check (D-073)."""
    verdicts = [c.verdict for comp in comparisons for c in comp.checks]
    return (sum(v is True for v in verdicts), sum(v is False for v in verdicts),
            sum(v is None for v in verdicts))


def report(comparisons: list) -> list:
    """The bench's text: one table of the configurations with our answers and French's
    words, then every check with its verdict, then the tally (D-073)."""
    lines = ["## Chapter 2's configurations against French's words [F95 pp. 26-35]", "",
             "| Figure | Page | French's answer | Plausible | Unlikely | Our most frequent | % | "
             "Mean codelets | Runs |", "|---|---:|---|---|---|---|---:|---:|---:|"]
    for c in comparisons:
        e = c.expected
        plausible = " ".join(f"{p} {_pct(c.survey, p):.0f}%" for p in e.get("plausible", []))
        unlikely = " ".join(f"{u} {_pct(c.survey, u):.0f}%" for u in e.get("unlikely", []))
        lines.append(f"| {c.name} | {c.page} | {e.get('answer') or '-'} | {plausible or '-'} | "
                     f"{unlikely or '-'} | {c.top.answer} | {c.top.frequency:.1f} | "
                     f"{c.survey.codelets:.1f} | {c.survey.runs} |")
    lines += ["", "### The checks", "", "| Figure | Statement | Numbers | Verdict |",
              "|---|---|---|---|"]
    for c in comparisons:
        for ch in c.checks:
            verdict = {True: "yes", False: "no", None: "not run"}[ch.verdict]
            lines.append(f"| {c.name} | {ch.text} | {ch.numbers} | {verdict} |")
    yes, no, skipped = tallied(comparisons)
    lines += ["", f"{yes} of {yes + no} checks agree with French's words"
                  + (f" ({skipped} not run)" if skipped else "") + ".", ""]
    return lines


def records(comparisons: list) -> list:
    """The JSON records (D-073)."""
    return [{"bench": "ch2", "problem": c.name, "page": c.page, "expected": c.expected,
             "survey": H.survey_record(c.survey),
             "checks": [{"text": ch.text, "numbers": ch.numbers, "verdict": ch.verdict}
                        for ch in c.checks]} for c in comparisons]

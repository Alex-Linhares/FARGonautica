"""The `fcca` bench: the eighteen result sets FCCA reprints as Figures IX-1 to IX-3
[FCCA pp. 394-399], each surveyed and put beside FCCA's own numbers; the two sets whose
numbers differ from the book's, Surround 5 and Surround 10 (IX-1d and IX-1f), are shown
against both versions (D-072; ANSWERS.md 1; SPEC.md §7.5).

"Each problem was run a total of 50 times." [FCCA p. 392]; the book says "Almost all of
the configurations were run fifty times." [F95 p. 114], so both versions are compared with
French's fifty.
"""

from tabletop.bench import harness as H
from tabletop.bench.families import problems_of
from tabletop.config import Config
from tabletop.problems import load_problem
from tabletop.reported import CHAPTER5, FCCA, ResultSet

SETS = FCCA


def book_version(rs: ResultSet) -> ResultSet:
    """The book's set behind an FCCA figure: the chapter 5 set of the same problem and
    program [F95 pp. 115-139] (D-072)."""
    for book in CHAPTER5:
        if book.problem == rs.problem and book.program == rs.program:
            return book
    raise KeyError(rs.problem)


def divergent(sets=SETS) -> list:
    """The FCCA sets whose numbers differ from the book's: IX-1d (Surround 5) and IX-1f
    (Surround 10) [FCCA p. 395; F95 pp. 118, 121] (D-072)."""
    return [rs for rs in sets if rs.answers != book_version(rs).answers]


def planned(sets=SETS, only: list | None = None) -> int:
    """How many surveys the bench will run (D-072)."""
    return len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only: list | None = None, progress=None,
        sets=SETS) -> list:
    """Survey every problem FCCA reprints (or those of `only`) and compare each set with
    FCCA's numbers, then, for a divergent set, with the book's as well [FCCA pp. 394-399]
    (D-072). Returns the comparisons in the figures' order, a divergent set's two together."""
    surveys = H.surveys_for(problems_of(sets, only), load_problem, config, seed, runs, progress)
    out = []
    for rs in sets:
        if rs.problem not in surveys:
            continue
        out.append(H.compare(surveys[rs.problem], rs))
        if rs in divergent(sets):
            out.append(H.compare(surveys[rs.problem], book_version(rs)))
    return out


def report(comparisons: list) -> list:
    """The bench's text: one table per comparison, figure by figure, the book's version
    following FCCA's where they differ, then the summary (D-072)."""
    lines = ["## FCCA's Figures IX-1 to IX-3 [FCCA pp. 394-399]", ""]
    for c in comparisons:
        if c.set.source == "F95":
            lines += ["(the same runs against the book's numbers, which differ "
                      "[F95 p. %d]:)" % c.set.page, ""]
        lines += H.markdown_table(c)
    lines += ["## Summary", ""] + H.summary_header()
    lines += [H.summary_row(c) for c in comparisons]
    return lines


def records(comparisons: list) -> list:
    """The JSON records, one per comparison (D-072)."""
    return [H.record_of(c) for c in comparisons]

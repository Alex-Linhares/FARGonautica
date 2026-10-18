"""The `families` bench: the 35 result sets of chapter 5 [F95 pp. 115-139], Surround 1-10,
Blockage 1-10, Buridan 1-13 (Buridan 1 under both programs [F95 pp. 129-130]) and
Figure/Ground 1, each surveyed and put beside French's numbers (D-072; ANSWERS.md 1).

Buridan 1's two sets share one problem file, so the problem is surveyed once and shown
beside each; Buridan 2-13 were run by French with the modified program [F95 p. 130], and
the bench marks those sets so that RESULTS.md can read them against the profile it ran.
"""

from tabletop.bench import harness as H
from tabletop.config import Config
from tabletop.problems import load_problem
from tabletop.reported import CHAPTER5

SETS = CHAPTER5
FAMILY_TITLES = (("surround", "Surround [F95 pp. 115-121]"),
                 ("blockage", "Blockage [F95 pp. 122-129]"),
                 ("buridan", "Buridan [F95 pp. 129-139]"),
                 ("figure-ground", "Figure/Ground [F95 pp. 139-142]"))


def problems_of(sets=SETS, only: list | None = None) -> list:
    """The problem names of the sets, once each in the sets' order, kept to `only` when
    given (D-072)."""
    out = []
    for rs in sets:
        if rs.problem not in out and (only is None or rs.problem in only):
            out.append(rs.problem)
    return out


def planned(sets=SETS, only: list | None = None) -> int:
    """How many surveys the bench will run (D-072)."""
    return len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only: list | None = None, progress=None,
        sets=SETS) -> list:
    """Survey every problem of the sets (or of `only`) and compare each set with its
    problem's survey [F95 p. 115] (D-072). Returns the comparisons in the sets' order."""
    surveys = H.surveys_for(problems_of(sets, only), load_problem, config, seed, runs, progress)
    return [H.compare(surveys[rs.problem], rs) for rs in sets if rs.problem in surveys]


def report(comparisons: list) -> list:
    """The bench's text: one table per set in French's format under its family's heading,
    then the one-line summary of every set (D-072)."""
    lines = []
    for family, title in FAMILY_TITLES:
        mine = [c for c in comparisons if c.set.family == family]
        if not mine:
            continue
        lines += [f"## {title}", ""]
        for c in mine:
            lines += H.markdown_table(c)
    lines += ["## Summary", ""] + H.summary_header()
    lines += [H.summary_row(c) for c in comparisons]
    return lines


def records(comparisons: list) -> list:
    """The JSON records, one per comparison (D-072)."""
    return [H.record_of(c) for c in comparisons]

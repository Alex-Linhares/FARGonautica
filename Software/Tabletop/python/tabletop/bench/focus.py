"""The `focus` bench: Surround 5 against Surround 10, and junk scaling (D-073).

"The specific purpose of Surround 10 is to examine the effect of adding superfluous
'junk' to a particular table configuration, in this case to Surround 5." [F95 p. 121].
French's finding: "Eliza touches the small glass only 13 percent less often than in the
Surround 5 configuration (the quality of the structure is essentially the same)." [F95
p. 121] and "the program took approximately the same amount of time to decide on an
answer here as it did in Surround 5" [F95 p. 121]. The bench surveys both problems and
prints his numbers [F95 pp. 118, 121] beside ours; then it scales the junk: Surround 5
with 0, 2, ... 12 extra objects placed in the band between the two rows, where Surround
10's junk lies [F95 p. 121 (img)], as a probe of the focusing mechanisms (D-073). The
scaled tables are the bench's own, not French's.
"""

import dataclasses

from tabletop import objects as O
from tabletop import reported as RP
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import Config
from tabletop.problems import Problem, Provenance, TableObject, load_problem
from tabletop.accel import Rng

BASE = "surround-05"      # [F95 p. 118]
JUNKED = "surround-10"    # Surround 5 plus five junk objects [F95 p. 121]
SETS = (BASE, JUNKED)
JUNK_COUNTS = (0, 2, 4, 6, 8, 10, 12)
# Surround 10's junk types first, in the figure's order [F95 p. 121 (img)], then the rest
# of the palette's small objects [F95 p. 26 (img)]; silverware upright (D-073).
JUNK_TYPES = ("pepper shaker", "salt shaker", "fork", "knife", "small glass", "spoon",
              "saucer", "big glass")
BAND = (40.0, 60.0)       # the middle band of the 100-unit table, between the rows (D-073)
MARGIN = 2.0              # the free space kept between a junk object and anything (D-073)
JITTER = 2.0              # the seeded jitter of a junk object's place, in units (D-073)
problems_of = P.problems_of


def _free(candidate: TableObject, problem: Problem, placed: list) -> bool:
    """Whether a candidate object overlaps nothing on the table (D-073)."""
    return all(O.edge_gap(candidate, other) >= MARGIN
               for other in list(problem.objects) + placed)


def junk_variant(problem: Problem, count: int, seed: int) -> Problem:
    """`problem` with `count` junk objects added in the band between the rows (D-073):
    types cycling through `JUNK_TYPES`, places on an even grid across the band's width
    with a seeded jitter of at most `JITTER` units, each kept clear of everything by
    `MARGIN`; a candidate that would overlap is moved along the grid until it is free.
    The variant is named `<problem>+junk<count>`; its objects' provenance says they are
    synthetic."""
    if count == 0:
        return problem
    rng = Rng(seed)
    placed = []
    cols = count
    ys = [BAND[0], (BAND[0] + BAND[1]) / 2, BAND[1]]
    step = problem.width / (cols + 1)
    k = 0
    for i in range(count):
        type_ = JUNK_TYPES[i % len(JUNK_TYPES)]
        orientation = "up" if type_ in O.SILVERWARE else None
        placed_one = False
        for attempt in range(3 * cols * len(ys)):
            slot = (i + attempt) % cols
            y = ys[(i + attempt // cols) % len(ys)]
            x = step * (slot + 1) + (rng.random() * 2 - 1) * JITTER
            y = y + (rng.random() * 2 - 1) * JITTER
            k += 1
            candidate = TableObject(f"junk{i + 1}", type_, round(x, 1), round(y, 1),
                                    orientation, (0, 0, 0, 0),
                                    Provenance("synthetic", 0, "junk (D-073)"))
            if _free(candidate, problem, placed):
                placed.append(candidate)
                placed_one = True
                break
        if not placed_one:
            raise ValueError(f"no room for junk object {i + 1} on {problem.name}")
    return dataclasses.replace(problem, name=f"{problem.name}+junk{count}",
                               objects=problem.objects + tuple(placed),
                               notes=problem.notes + (f"{count} synthetic junk objects in "
                                                      f"the band between the rows (D-073)",))


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    names = problems_of(sets, only)
    return len(names) + (len(JUNK_COUNTS) if BASE in names else 0)


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """Survey Surround 5 and Surround 10, then Surround 5 with 0 to 12 junk objects
    (D-073); the scaling is skipped when `only` leaves Surround 5 out."""
    names = problems_of(sets, only)
    surveys = H.surveys_for(names, load_problem, config, seed, runs, progress)
    scaling = {}
    if BASE in names:
        base = load_problem(BASE)
        for count in JUNK_COUNTS:
            variant = junk_variant(base, count, seed)
            sv = H.survey(variant, config, seed, runs)
            scaling[count] = sv
            if progress is not None:
                progress(sv)
    return {"problems": names, "surveys": surveys, "scaling": scaling}


def _french_row(name: str) -> dict:
    rs = RP.result_sets(name)[0]
    mean = sum(a.frequency * a.codelets for a in rs.answers if a.codelets) / 100.0
    return {"set": rs, "e1": rs.answer("E1"), "e2": rs.answer("E2"), "codelets": mean}


def report(results: dict) -> list:
    """The bench's text: Surround 5 beside Surround 10 with French's numbers [F95 pp. 118,
    121], the ratios he states, then the junk scaling table (D-073)."""
    lines = ["## Surround 5 against Surround 10: the junk [F95 p. 121]", ""]
    sv = results["surveys"]
    if BASE in sv and JUNKED in sv:
        lines += ["| Problem | E1 %, French | E1 %, ours | E2 %, French | E2 %, ours | "
                  "Codelets, French (weighted mean) | Mean codelets, ours | Structure E1, "
                  "French | ours | Capped |", "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|"]
        rows = {}
        for name in (BASE, JUNKED):
            pb, f, s = load_problem(name), _french_row(name), sv[name]
            e1 = s.tally(pb.answers["E1"])
            rows[name] = (P.label_frequency(s, pb, "E1"), s.codelets)
            lines.append(f"| {f['set'].title} [F95 p. {f['set'].page}] | {f['e1'].frequency} | "
                         f"{rows[name][0]:.1f} | {f['e2'].frequency} | "
                         f"{P.label_frequency(s, pb, 'E2'):.1f} | {f['codelets']:.1f} | "
                         f"{s.codelets:.1f} | {f['e1'].structure} | "
                         f"{'-' if e1 is None else f'{e1.structure:.1f}'} | {s.capped} |")
        f5, f10 = _french_row(BASE), _french_row(JUNKED)
        lines += ["", f"E1 drop from Surround 5 to 10: French {f5['e1'].frequency - f10['e1'].frequency} "
                      f"points ('13 percent less often' [F95 p. 121]); ours "
                      f"{rows[BASE][0] - rows[JUNKED][0]:.1f} points. Run length ratio, "
                      f"Surround 10 over 5: French {f10['codelets'] / f5['codelets']:.2f} "
                      f"('approximately the same amount of time' [F95 p. 121]); ours "
                      f"{rows[JUNKED][1] / rows[BASE][1]:.2f}.", ""]
    if results["scaling"]:
        pb = load_problem(BASE)
        lines += ["## Junk scaling: Surround 5 with 0 to 12 extra objects (D-073)", "",
                  "| Junk objects | Objects on the table | E1 % | E2 % | Literal % | Other % | "
                  "Mean codelets | Mean structure | Capped |",
                  "|---:|---:|---:|---:|---:|---:|---:|---:|---:|"]
        for count, s in results["scaling"].items():
            e1, e2 = P.label_frequency(s, pb, "E1"), P.label_frequency(s, pb, "E2")
            lit = P.literal_frequency(s, pb)
            lines.append(f"| {count} | {len(pb.objects) + count} | {e1:.1f} | {e2:.1f} | "
                         f"{lit:.1f} | {100 - e1 - e2 - lit:.1f} | {s.codelets:.1f} | "
                         f"{P.mean_structure(s):.1f} | {s.capped} |")
        lines.append("")
    return lines


def records(results: dict) -> list:
    """The JSON records: the two surveys and one per junk count (D-073)."""
    out = [{"bench": "focus", "problem": name, "survey": H.survey_record(s)}
           for name, s in results["surveys"].items()]
    out += [{"bench": "focus", "problem": BASE, "junk": count, "survey": H.survey_record(s)}
            for count, s in results["scaling"].items()]
    return out

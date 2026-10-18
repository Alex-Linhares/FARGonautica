"""The `temperature` bench: the course of T over a run, many runs at a time (D-073).

French shows the thermometer at nine moments of one run, from 80.0 at the start to 2.0 at
the answer [F95 pp. 96-107 (img)], and says what moved it at each step: "when structure
improves, this tends to cause the temperature to decrease" [F95 p. 105], and "if very few
changes are occurring in the quality of the Worldview, the program has probably found a
good representation of the table, and the temperature drops" [F95 p. 90] (SPEC.md §6).
It is computed from three terms (D-047 to D-050) and drives the end of the run through
find-answer (D-056). This bench runs a few problems many times, reads every temperature
update off the events (`TemperatureUpdated`), and reports the curve of T against the
codelets run (the mean and quartiles over the runs still going at each count), the T at
the first Worldview change and at the answer, the lowest T reached, how many updates came
by schedule, competition or change, and the mean size of the three terms; for Surround 5,
beside the dumps' thermometer. Beside the intact profile it runs two variants of the
formula's reading that the decisions record as alternatives: the signed rate of change of
Q (D-048's `dq_signed`: a loss of quality then lowers the first term instead of raising
it; an improvement raises it either way, so this is not the reading of p. 105 in which an
improvement "pushed the temperature down" [F95 p. 105], which no knob gives), and
alpha = 100 (D-047's alternative). Nothing here sets a default (ANSWERS.md 1); a variant
is a probe.
"""

from dataclasses import dataclass

from tabletop import events as EV
from tabletop.bench import ch4
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import Config
from tabletop.problems import load_problem
from tabletop.accel import run_seeds
from tabletop.run import result_of
from tabletop.world import World

SETS = P.DEFAULT_PROBLEMS
CLOCKS = (0, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)   # the curve's columns (D-073)
DUMP_PROBLEM = ch4.PROBLEM                                 # Surround 5 [F95 p. 96]
problems_of = P.problems_of


def variants(config: Config) -> list:
    """The intact profile, then the two alternative readings of the formula (D-073)."""
    return [P.intact(config),
            P.variant(config, "dq-signed", "the signed rate of change of Q: a loss lowers "
                      "the first term instead of raising it (D-048's alternative)",
                      dq_signed=True),
            P.variant(config, "alpha-100", "alpha = 100 in T = alpha dQ/Q + ... [F95 p. 91 "
                      "(img)] (D-047's alternative)", temperature_alpha=100.0)]


@dataclass(frozen=True)
class Path:
    """One run's temperature path (D-073): the `seed`; `updates`, every update as
    (clock, T, held, by, q, dq, dwv, tcp) in order; the `result`."""

    seed: int
    updates: tuple
    result: object

    @property
    def codelets(self) -> int:
        """The codelets the run took, the answer coming at the last (D-056, D-073)."""
        return self.result.codelets

    def at(self, clock: int) -> float | None:
        """T at a codelet count: the latest update at or before it; None once the run has
        ended (the answer comes at the last codelet) (D-050, D-073)."""
        if clock >= self.codelets:
            return None
        t = None
        for c, temperature, *_ in self.updates:
            if c > clock:
                break
            t = temperature
        return t

    def first_change(self) -> tuple | None:
        """The first update not held at the initial value (D-049): (clock, T), or None."""
        for c, t, held, *_ in self.updates:
            if not held:
                return (c, t)
        return None

    def end(self) -> float:
        """T when the answer was given: the last update's value (D-073)."""
        return self.updates[-1][1]

    def lowest(self) -> float:
        """The lowest T the run reached (D-073)."""
        return min(t for _, t, *_ in self.updates)

    def counts(self) -> dict:
        """How many updates came by each reason (`start`, `schedule`, `competition`,
        `change`) (D-050)."""
        out = {}
        for _, _, _, by, *_ in self.updates:
            out[by] = out.get(by, 0) + 1
        return out

    def terms(self, config: Config) -> list:
        """The three terms of the formula at every update not held [F95 p. 91 (img)]:
        (alpha dQ/Q, beta dWV, gamma Tcp) (D-047)."""
        out = []
        for _, _, held, _, q, dq, dwv, tcp in self.updates:
            if not held:
                out.append((config.temperature_alpha * dq / max(config.q_floor, q),
                            config.temperature_beta * dwv, config.temperature_gamma * tcp))
        return out


def run_path(problem, config: Config, seed: int) -> Path:
    """One run, its temperature updates read off the events (D-073)."""
    collector = EV.Collector()
    world = World(problem, config, seed, [collector])
    while not world.finished:
        world.step()
    updates = tuple((e.clock, e.temperature, e.held, e.by, e.q, e.dq, e.dwv, e.tcp)
                    for e in collector.events if isinstance(e, EV.TemperatureUpdated))
    return Path(seed, updates, result_of(world))


def paths_of(problem, config: Config, seed: int, runs: int) -> list:
    """The paths of a problem's survey, from the benches' batch seed (D-072, D-073)."""
    batch_seed = H.batch_seed_for(seed, problem.name)
    return [run_path(problem, config, s) for s in run_seeds(batch_seed, runs)]


def _quartiles(values: list) -> tuple:
    """(lower quartile, median, upper quartile) by nearest rank; Nones when empty."""
    if not values:
        return (None, None, None)
    v = sorted(values)
    n = len(v)
    return tuple(v[min(n - 1, max(0, int(round(f * n)) - 1))] for f in (0.25, 0.5, 0.75))


def curve(paths: list, clocks=CLOCKS) -> list:
    """Per codelet count: (clock, mean T, lower quartile, median, upper quartile, the share
    of runs still going in percent) over the runs still going (D-073)."""
    out = []
    for clock in clocks:
        ts = [t for t in (p.at(clock) for p in paths) if t is not None]
        low, median, high = _quartiles(ts)
        mean = sum(ts) / len(ts) if ts else None
        out.append((clock, mean, low, median, high, 100.0 * len(ts) / len(paths)))
    return out


def summary(paths: list, config: Config) -> dict:
    """The numbers of a set of paths (D-073): the mean and median T at the end, the mean
    lowest T, the median codelet of the first Worldview change and the share of runs with
    none, the mean T after that change, the mean updates per run by reason, and the mean of
    each term over the updates not held."""
    n = len(paths)
    ends = [p.end() for p in paths]
    firsts = [p.first_change() for p in paths]
    changed = [f for f in firsts if f is not None]
    counts = {}
    for p in paths:
        for by, k in p.counts().items():
            counts[by] = counts.get(by, 0) + k
    terms = [t for p in paths for t in p.terms(config)]
    return {
        "end_mean": sum(ends) / n, "end_median": _quartiles(ends)[1],
        "lowest_mean": sum(p.lowest() for p in paths) / n,
        "first_change_median": _quartiles([c for c, _ in changed])[1],
        "no_change_share": 100.0 * (n - len(changed)) / n,
        "after_first_mean": sum(t for _, t in changed) / len(changed) if changed else None,
        "updates_by": {by: counts[by] / n for by in ("start", "schedule", "competition",
                                                      "change") if by in counts},
        "terms_mean": [sum(t[i] for t in terms) / len(terms) if terms else None
                       for i in range(3)],
    }


def planned(sets=SETS, only=None) -> int:
    """How many surveys the bench will run (D-073)."""
    return 3 * len(problems_of(sets, only))


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> dict:
    """Run the problems under the three variants, every run stepped with its temperature
    updates collected (D-073)."""
    vs = variants(config)
    names = problems_of(sets, only)
    paths, surveys = {}, {}
    for v in vs:
        for name in names:
            problem = load_problem(name)
            ps = paths_of(problem, v.config, seed, runs)
            paths[(v.name, name)] = ps
            surveys[(v.name, name)] = H.survey_of(problem, v.config, seed,
                                                  [p.result for p in ps])
            if progress is not None:
                progress(surveys[(v.name, name)])
    return {"variants": vs, "problems": names, "paths": paths, "surveys": surveys,
            "dumps": [(n, t) for _, _, n, t, _ in ch4.french_dumps(ch4.french_data())
                      if n > 0] if DUMP_PROBLEM in names else []}


def _f(v, digits=1) -> str:
    return "-" if v is None else f"{v:.{digits}f}"


def curve_lines(rows: list, first_column: str, french=None) -> list:
    """A curve as a table: one column per codelet count, rows for French's reading when
    given, the mean, the quartiles and the runs still going (D-073)."""
    clocks = [r[0] for r in rows]
    lines = [f"| {first_column} | " + " | ".join(str(c) for c in clocks) + " |",
             "|---|" + "---:|" * len(clocks)]
    if french is not None:
        lines.append("| T, French's run [F95 pp. 96-107 (img)] | "
                     + " | ".join(_f(french.get(c)) for c in clocks) + " |")
    for label, i in (("T, mean", 1), ("lower quartile", 2), ("median", 3),
                     ("upper quartile", 4), ("runs still going, %", 5)):
        lines.append(f"| {label} | " + " | ".join(_f(r[i]) for r in rows) + " |")
    return lines


def summary_lines(results: dict, problem: str) -> list:
    """One problem's summary table, a row per variant (D-073)."""
    pb = load_problem(problem)
    lines = ["| Variant | T at the end, mean | median | Lowest T, mean | First change at "
             "codelet (median) | Runs with no change, % | T after the first change, mean | "
             "Updates per run: schedule / competition / change | Mean terms: alpha dQ/Q, "
             "beta dWV, gamma Tcp | Main answer | % | Literal, % | Mean codelets |",
             "|---|---:|---:|---:|---:|---:|---:|---|---|---|---:|---:|---:|"]
    for v in results["variants"]:
        s = summary(results["paths"][(v.name, problem)], v.config)
        sv = results["surveys"][(v.name, problem)]
        top = P.main_tally(sv)
        who = f"{top.answer} ({top.label})" if top.label else top.answer
        u = s["updates_by"]
        lines.append(
            f"| {v.name} | {_f(s['end_mean'])} | {_f(s['end_median'])} | {_f(s['lowest_mean'])} "
            f"| {_f(s['first_change_median'], 0)} | {_f(s['no_change_share'])} "
            f"| {_f(s['after_first_mean'])} | {_f(u.get('schedule'))} / "
            f"{_f(u.get('competition'))} / {_f(u.get('change'))} | "
            + ", ".join(_f(t) for t in s["terms_mean"])
            + f" | {who} | {top.frequency:.1f} | {P.literal_frequency(sv, pb):.1f} "
            f"| {sv.codelets:.1f} |")
    return lines


def report(results: dict) -> list:
    """The bench's text: the variants; per problem the intact curve (Surround 5 also at the
    dumps' counts beside French's thermometer) and the summary table (D-073)."""
    lines = ["## The course of the temperature [F95 pp. 90-93; F95 pp. 96-107 (img)]", ""]
    lines += P.variants_table(results["variants"])
    for name in results["problems"]:
        lines += [f"### {name}", "", "Under the intact profile, T against the codelets run "
                  "(the runs still going at each count):", ""]
        intact = results["paths"][(results["variants"][0].name, name)]
        lines += curve_lines(curve(intact), "Codelets run") + [""]
        if name == DUMP_PROBLEM and results["dumps"]:
            french = dict(results["dumps"])
            lines += ["At the chapter 4 dumps' codelet counts, beside French's thermometer:", ""]
            lines += curve_lines(curve(intact, tuple(french)), "Codelets run", french) + [""]
        lines += ["Per variant:", ""] + summary_lines(results, name) + [""]
    return lines


def records(results: dict) -> list:
    """The JSON records: one per (variant, problem) with the survey, the curve, the summary
    and every path's updates (D-073)."""
    out = []
    french = dict(results["dumps"])
    for v in results["variants"]:
        for name in results["problems"]:
            ps = results["paths"][(v.name, name)]
            rec = {"bench": "temperature", "variant": v.name,
                   "changes": [list(c) for c in v.changes], "note": v.note,
                   "survey": H.survey_record(results["surveys"][(v.name, name)]),
                   "curve": [list(r) for r in curve(ps)],
                   "summary": summary(ps, v.config),
                   "paths": [{"seed": p.seed, "updates": [list(u) for u in p.updates]}
                             for p in ps]}
            if name == DUMP_PROBLEM and french:
                rec["dumps"] = [{"codelets": c, "french_t": t, "ours": list(r)}
                                for (c, t), r in zip(results["dumps"],
                                                     curve(ps, tuple(french)))]
            out.append(rec)
    return out

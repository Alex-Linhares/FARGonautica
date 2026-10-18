"""The `stated` bench: French's statements of what the program does, read as yes-or-no
checks over many runs (measure M2; D-074).

French describes his program's behaviour in words throughout the book: what it notices first
[F95 p. 97], how the Worldview grows and fights [F95 pp. 101-105], when the run ends [F95
p. 105], what it does with junk, gaps and identical objects [F95 pp. 128-129; F95 p. 140], and
the actions he calls faults [F95 pp. 140-142]. None of these is a number of a result set
(ANSWERS.md 1): they are qualitative claims, the development evidence of the reconstruction.
This bench surveys a handful of configurations under `standard` (and `french-faults` where a
statement is about a fault), reads each statement against the runs, and prints every verdict
with the numbers behind it; then the chapter 4 milestones in French's order and chapter 2's
series by his words (the `ch4` and `ch2` benches' readings). A miss is a finding to trace to a
decision, never a reason to move a default.
"""

from dataclasses import dataclass

from tabletop import events as EV
from tabletop.bench import ch2, ch4
from tabletop.bench import harness as H
from tabletop.bench import probes as P
from tabletop.config import PROFILES, Config
from tabletop.geometry import exact_region, half
from tabletop.problems import load_problem
from tabletop.accel import run_seeds
from tabletop.run import result_of
from tabletop.world import World

CH4 = "surround-05"            # the chapter 4 configuration [F95 p. 96; F95 p. 118]
JUNK = "surround-10"           # the same with junk [F95 p. 121]
BLOCKAGE = ("blockage-06", "blockage-08", "blockage-09", "blockage-10")   # the gaps [F95 p. 128]
FIGURE_GROUND = "figure-ground-01"   # the fork-glass group [F95 pp. 141-142]
FOUR_CUPS = "buridan-12"       # four identical objects in a row [F95 p. 140]
FAULTS = "french-faults"
SETS = ((CH4, "standard"), (JUNK, "standard")) + tuple((b, "standard") for b in BLOCKAGE) + (
    (FIGURE_GROUND, "standard"), (FIGURE_GROUND, FAULTS), (FOUR_CUPS, "standard"),
    (FOUR_CUPS, FAULTS))


@dataclass(frozen=True)
class Facts:
    """What one run did, read off its events (D-074): the `answer`, `literal`, `codelets`,
    `capped`; `groups` as (kind, members, temperature at the build) for the groups built;
    `final_groups`, the names and members of the groups alive at the end; `first_group_kind`;
    `pairs`, correspondence name to its two ends; `first_pair`; `entries`, the ends of each
    Worldview entry in order; `fights`, (newcomer ends, rival ends, rival wins); `pushes`;
    `scheduled`, the (before, after) temperatures of the scheduled updates; `last_codelet`;
    `focus`, the last activation seen of the touched type and of "opposite"; and
    `milestones` as the `ch4` bench reads them."""

    answer: str
    literal: bool
    codelets: int
    capped: bool
    groups: tuple
    final_groups: tuple
    first_group_kind: str | None
    pairs: dict
    first_pair: tuple | None
    entries: tuple
    fights: tuple
    pushes: int
    scheduled: tuple
    last_codelet: str | None
    focus: dict
    milestones: dict


def facts_of(problem, collector: EV.Collector, result) -> Facts:
    """Read one run's facts off its events (D-074)."""
    ids = list(problem.ids())
    groups, alive, pairs, entries, fights, scheduled, focus = [], {}, {}, [], [], [], {}
    last_codelet, temperature, previous, pushes = None, None, None, 0
    touched_type = problem.touched_object().type
    for e in collector.events:
        if isinstance(e, EV.CodeletChosen):
            last_codelet, temperature = e.codelet, e.temperature
        elif isinstance(e, EV.GroupBuilt):
            groups.append((e.kind, tuple(e.members), temperature))
            alive[e.group] = tuple(e.members)
        elif isinstance(e, EV.GroupRemoved):
            alive.pop(e.group, None)
        elif isinstance(e, EV.CorrespondenceBuilt):
            pairs[e.correspondence] = (e.henry, e.eliza)
        elif isinstance(e, EV.WorldviewChanged):
            entries.extend(pairs[n] for n in e.added if n in pairs)
        elif isinstance(e, EV.CompetitionHeld) and e.rivals:
            fights.append((pairs.get(e.newcomer), tuple(pairs.get(r) for r in e.rivals),
                           e.rival_wins))
        elif isinstance(e, EV.ContextPushed):
            pushes += 1
        elif isinstance(e, EV.TemperatureUpdated):
            if e.by == "schedule" and previous is not None:
                scheduled.append((previous, e.temperature))
            previous = e.temperature
        elif isinstance(e, EV.NodeJolted) and e.node in (touched_type, "opposite"):
            focus[e.node] = e.activation
    return Facts(result.answer, result.literal, result.codelets, result.capped, tuple(groups),
                 tuple(alive.items()), groups[0][0] if groups else None, pairs,
                 next(iter(pairs.values()), None), tuple(entries), tuple(fights), pushes,
                 tuple(scheduled), last_codelet, focus, ch4.milestones_of(collector, ids))


@dataclass(frozen=True)
class Sample:
    """One problem under one profile (D-074): the `survey` and the `facts` of every run."""

    problem: str
    profile: str
    survey: H.Survey
    facts: tuple


def sample(name: str, config: Config, seed: int, runs: int) -> Sample:
    """Run a problem `runs` times from the bench seed, keeping every run's facts (D-072,
    D-074); the runs are the ones `harness.survey` would make."""
    problem = load_problem(name)
    results, facts = [], []
    for s in run_seeds(H.batch_seed_for(seed, name), runs):
        collector = EV.Collector()
        world = World(problem, config, s, [collector])
        while world.step() is not None:
            pass
        result = result_of(world)
        results.append(result)
        facts.append(facts_of(problem, collector, result))
    return Sample(name, config.profile, H.survey_of(problem, config, seed, results), tuple(facts))


@dataclass(frozen=True)
class Check:
    """One statement read against the runs (D-074): its `number`, French's `statement`
    with its page, the `problem` and `profile` it is read on, our `numbers`, the `verdict`
    (True, False, or None when the runs show nothing to read), and `source` (`French` for
    his words, `bench` for an expectation of the bench's own)."""

    number: int
    statement: str
    problem: str
    profile: str
    numbers: str
    verdict: bool | None
    source: str = "French"


# ---- the readings (D-074) --------------------------------------------------------------

def share(facts, predicate) -> float:
    """The share of runs, in percent, for which the predicate holds (D-074)."""
    return 100.0 * sum(1 for f in facts if predicate(f)) / len(facts) if facts else 0.0


def unordered(pair) -> frozenset:
    """A pair of ends without their order (D-074)."""
    return frozenset(pair) if pair else frozenset()


def henrys_row(problem) -> frozenset:
    """The objects on Henry's half of the table (D-005)."""
    return frozenset(o.id for o in problem.objects if half(exact_region(o.x, o.y)) == "henry")


def grouped_together(f: Facts, a: str, b: str) -> bool:
    """Whether some group built in the run held both objects (D-074)."""
    return any(a in members and b in members for _, members, _ in f.groups)


def end_kind(problem, end: str) -> str:
    """`object` or `group`, for an end of a correspondence (D-074)."""
    return "object" if end in problem.ids() else "group"


def pair_kinds(problem, pair) -> tuple:
    """The two ends' kinds, sorted (D-074)."""
    return tuple(sorted(end_kind(problem, e) for e in pair))


def nested_sameness_run(problem, f: Facts) -> bool:
    """Whether, at the end, a sameness group of one type lies strictly inside another of
    the same type: French's "cup1-cup2, cup2-cup3, cup3-cup4, etc." [F95 p. 140]."""
    kinds = {members: kind for kind, members, _ in f.groups}
    final = [set(m) for _, m in f.final_groups if kinds.get(tuple(m)) == "sameness"]
    for a in final:
        for b in final:
            if a < b and len({problem.object(i).type for i in b}) == 1:
                return True
    return False


def fork_glass_group(problem, f: Facts) -> bool:
    """A bottom-up group holding the touched fork and a glass [F95 pp. 141-142] (a gestalt
    bunch may hold both, D-023; the fault is the built group, D-069)."""
    touched = problem.touched
    glasses = {o.id for o in problem.objects if o.type == "small glass"}
    return any(k != "gestalt" and touched in m and glasses & set(m) for k, m, _ in f.groups)


def milestones_in_order(f: Facts) -> bool:
    """Whether the milestones the run reached come in French's order [F95 pp. 96-107]."""
    reached = [f.milestones[name] for name, _, _ in ch4.MILESTONES if f.milestones[name] is not None]
    return reached == sorted(reached)


def mean(xs) -> float | None:
    """The mean, or None of nothing (D-074)."""
    xs = list(xs)
    return sum(xs) / len(xs) if xs else None


def _pct(x: float) -> str:
    return f"{x:.1f}%"


# ---- the statements (D-074) ------------------------------------------------------------

class _Ch4:
    """The names of the chapter 4 configuration a sample's checks share (D-074): the problem,
    the facts, Henry's row, the touched cup, the isolated cup (E2), the glass (E1) and the
    two unordered pairs of the fight of [F95 p. 105]."""

    def __init__(self, s: Sample):
        self.sample = s
        self.problem = load_problem(s.problem)
        self.fs = s.facts
        self.row = henrys_row(self.problem)
        self.touched = self.problem.touched
        self.isolated, self.glass = self.problem.answers.get("E2"), self.problem.answers.get("E1")
        self.cup_cup = frozenset((self.touched, self.isolated))
        self.cup_glass = frozenset((self.touched, self.glass))

    def check(self, number, statement, reading, verdict) -> Check:
        return Check(number, statement, self.sample.problem, self.sample.profile, reading, verdict)


def ch4_checks(s: Sample) -> list:
    """The statements about the chapter 4 configuration [F95 pp. 97-118] (D-074), numbered
    1-16 in the order of the report, in four groups: the groups found (1-2), the first
    correspondence and the fight (3-5), the end of the run and the temperature (6-10), and
    the structures French's run built that ours should (11-16)."""
    c = _Ch4(s)
    return _ch4_groups(c) + _ch4_first_correspondence(c) + _ch4_end_and_temperature(c) + _ch4_structures(c)


def _ch4_groups(c: _Ch4) -> list:
    """Statements 1-2: the first group is a gestalt one and Henry's row is found whole."""
    fs, row = c.fs, c.row
    gestalt_first = share(fs, lambda f: f.first_group_kind == "gestalt")
    whole = share(fs, lambda f: any(k == "gestalt" and set(m) >= row for k, m, _ in f.groups))
    return [
        c.check(1, "The first group found is a gestalt group, a bunch of objects close "
                   "together, before any neighbour is known [F95 p. 97]",
                f"first group gestalt in {_pct(gestalt_first)} of runs", gestalt_first >= 90.0),
        c.check(2, "Henry's row of six is found whole as one gestalt group [F95 p. 97]",
                f"whole row in {_pct(whole)} of runs", whole >= 50.0),
    ]


def _ch4_first_correspondence(c: _Ch4) -> list:
    """Statements 3-5: the first correspondence is cup-cup, enters without a fight, and
    loses to cup-glass on coherence when the two fight [F95 pp. 97-105]."""
    fs, cup_cup, cup_glass = c.fs, c.cup_cup, c.cup_glass
    firsts = {}
    for f in fs:
        if f.first_pair:
            firsts[unordered(f.first_pair)] = firsts.get(unordered(f.first_pair), 0) + 1
    top = max(firsts, key=firsts.get) if firsts else None
    no_fight = share([f for f in fs if f.entries],
                     lambda f: not f.fights or f.fights[0][0] != f.entries[0])
    fights = [(n, r, w) for f in fs for n, r, w in f.fights
              if {unordered(n)} | {unordered(x) for x in r} >= {cup_cup, cup_glass}]
    glass_wins = sum(1 for n, r, w in fights if (unordered(n) == cup_glass) == w)
    return [
        c.check(3, "The first correspondence joins the touched cup to the isolated cup, "
                   "one of the most salient objects on the table [F95 p. 97]",
                f"most common first pair {sorted(top) if top else None}, "
                f"{_pct(100.0 * firsts.get(top, 0) / len(fs))} of runs; "
                f"cup-cup first in {_pct(100.0 * firsts.get(cup_cup, 0) / len(fs))}",
                top == cup_cup if top else None),
        c.check(4, "The first correspondence is put in the Worldview without a fight "
                   "[F95 p. 101]",
                f"first entry uncontested in {_pct(no_fight)} of runs with an entry",
                no_fight >= 90.0),
        c.check(5, "When cup-glass and cup-cup fight, cup-glass wins, the coherent "
                   "Worldview rating higher though cup-cup is a priori stronger [F95 "
                   "p. 105]", f"{glass_wins} of {len(fights)} such fights won by cup-glass",
                (glass_wins * 2 > len(fights)) if fights else None),
    ]


def _ch4_end_and_temperature(c: _Ch4) -> list:
    """Statements 6-10: the run ends by find-answer, the literal answer is unlikely, the
    answers are E1 or E2, T falls while the Worldview holds, the focus nodes stay active."""
    fs, problem, glass, isolated = c.fs, c.problem, c.glass, c.isolated
    ended = [f for f in fs if not f.capped]
    by_answer = share(ended, lambda f: f.last_codelet == "find-answer")
    literal = share(fs, lambda f: f.literal)
    others = [f for f in fs if not f.literal]
    two = share(others, lambda f: f.answer in (glass, isolated))
    quiet = [pair for f in fs for pair in f.scheduled]
    falls = 100.0 * sum(1 for b, a in quiet if a <= b) / len(quiet) if quiet else 0.0
    hot = share(fs, lambda f: all(f.focus.get(n, 0) >= 100 for n in
                                  (problem.touched_object().type, "opposite")))
    return [
        c.check(6, "\"The run ends only when a find-answer codelet is run.\" [F95 p. 105]",
                f"{_pct(by_answer)} of uncapped runs", by_answer == 100.0),
        c.check(7, "The default answer, the touched object itself, comes only from a "
                   "find-answer very early on, \"unlikely, but theoretically possible\" "
                   "[F95 p. 105]", f"literal answers in {_pct(literal)} of runs", literal < 50.0),
        c.check(8, "The program's answers on this configuration are the small glass "
                   "and the isolated cup [F95 p. 118]",
                f"{_pct(two)} of non-literal answers are E1 or E2", two >= 80.0 if others else None),
        c.check(9, "While the Worldview does not change, the temperature falls "
                   "[F95 p. 101]", f"{_pct(falls)} of scheduled updates do not raise T", falls >= 90.0),
        c.check(10, "The touched object's category and \"opposite\" stay active for "
                    "the whole run, near the ceiling in every dump [F95 pp. 98-107 "
                    "(img)]", f"both at or above 100 at their last jolt in {_pct(hot)} of runs",
                hot == 100.0),
    ]


def _ch4_structures(c: _Ch4) -> list:
    """Statements 11-16: cup-spoon groups rare, group-group over group-object, subworlds
    entered, the milestones in order, cup-cup before cup-glass, a group correspondence in
    the Worldview [F95 pp. 85-125]."""
    fs, problem, cup_cup, cup_glass = c.fs, c.problem, c.cup_cup, c.cup_glass
    sameness = [t for f in fs for k, _, t in f.groups if k == "sameness"]
    mixed = [t for f in fs for k, _, t in f.groups if k == "mixed"]
    t_mixed = round(mean(mixed), 1) if mixed else None
    t_same = round(mean(sameness), 1) if sameness else None
    gg = sum(1 for f in fs for p in f.entries if pair_kinds(problem, p) == ("group", "group"))
    go = sum(1 for f in fs for p in f.entries if pair_kinds(problem, p) == ("group", "object"))
    inside = share(fs, lambda f: f.pushes > 0)
    ordered = share(fs, milestones_in_order)
    both = [f for f in fs if cup_cup in map(unordered, f.entries) and cup_glass in map(unordered, f.entries)]
    cup_first = share(both, lambda f: [unordered(p) for p in f.entries].index(cup_cup)
                      < [unordered(p) for p in f.entries].index(cup_glass))
    group_entry = share(fs, lambda f: any(pair_kinds(problem, p) == ("group", "group") for p in f.entries))
    return [
        c.check(11, "A cup-spoon grouping is rare, a structure of high temperature "
                    "[F95 p. 111]",
                f"{len(mixed)} mixed against {len(sameness)} sameness groups, built at "
                f"a mean T of {t_mixed} against {t_same}",
                (len(mixed) < len(sameness) and (not mixed or t_mixed > t_same))
                if sameness else None),
        c.check(12, "The program favours group-to-group mappings over group-to-object "
                    "ones [F95 p. 125]", f"{gg} group-group and {go} group-object Worldview entries",
                gg >= go if gg or go else None),
        c.check(13, "Correspondences are built inside a group correspondence, between "
                    "the objects making up the connected groups [F95 p. 85; F95 p. 108]",
                f"a subworld entered in {_pct(inside)} of runs", inside > 0.0),
        c.check(14, "The milestones of the run come in French's order: gestalt group, "
                    "bottom-up group, correspondence, Worldview entry, group "
                    "correspondence, fight, answer [F95 pp. 96-107 (img)]",
                f"in order in {_pct(ordered)} of runs", ordered >= 90.0),
        c.check(15, "The cup-cup correspondence enters the Worldview before the "
                    "cup-glass one, which arrives as a rival later [F95 pp. 101-105]",
                f"cup-cup first in {_pct(cup_first)} of the {len(both)} runs with both",
                cup_first >= 50.0 if both else None),
        c.check(16, "A correspondence between two groups (the spoon-spoon groups) "
                    "enters the Worldview [F95 pp. 103-105]",
                f"in {_pct(group_entry)} of runs", group_entry > 0.0),
    ]


def junk_checks(base: Sample, junk: Sample) -> list:
    """Surround 10 beside Surround 5 [F95 p. 121] (D-074)."""
    out = [Check(17, "With junk between the rows the program needs more codelets (the "
                     "bench's reading of focus [F95 p. 121])", junk.problem, junk.profile,
                 f"mean codelets {junk.survey.codelets:.1f} against {base.survey.codelets:.1f}",
                 junk.survey.codelets > base.survey.codelets, "bench")]
    glass = load_problem(base.problem).answers.get("E1")
    a, b = P.frequency(base.survey, [glass]), P.frequency(junk.survey, [glass])
    out.append(Check(18, "The junk hardly changes the answer: the glass is still given "
                         "[F95 p. 121]", junk.problem, junk.profile,
                     f"glass {_pct(b)} with junk against {_pct(a)} without",
                     (b > 0.0) if a > 0.0 else None))
    return out


def blockage_checks(samples: dict) -> list:
    """The gaps of Blockage 6, 8, 9 and 10 [F95 pp. 128-129] (D-074)."""
    shares = {}
    for name in BLOCKAGE:
        a, b = _gap_ids(name)
        shares[name] = share(samples[(name, "standard")].facts,
                             lambda f, a=a, b=b: grouped_together(f, a, b))
    out = [Check(19, "Blockage 6's two glasses, a glass width apart, are grouped [F95 p. 128]",
                 "blockage-06", "standard", f"grouped in {_pct(shares['blockage-06'])} of runs",
                 shares["blockage-06"] >= 50.0),
           Check(20, "Blockage 10's glasses, three widths apart, are no longer grouped "
                     "[F95 p. 129]", "blockage-10", "standard",
                 f"grouped in {_pct(shares['blockage-10'])} of runs", shares["blockage-10"] < 5.0),
           Check(21, "The grouping falls as the glasses are pulled apart: 6, 8, 9, 10 "
                     "[F95 pp. 128-129]", "blockage-06..10", "standard",
                 ", ".join(f"{n[-2:]}: {_pct(shares[n])}" for n in BLOCKAGE),
                 all(shares[x] >= shares[y] for x, y in zip(BLOCKAGE, BLOCKAGE[1:]))
                 and shares[BLOCKAGE[0]] > shares[BLOCKAGE[-1]])]
    return out


def _gap_ids(name: str) -> tuple:
    """The two glasses of a Blockage file's `gap` block (D-003)."""
    import json
    from tabletop.problems import problem_path
    with open(problem_path(name), encoding="utf-8") as f:
        return tuple(json.load(f)["gap"]["between"])


def fault_checks(samples: dict) -> list:
    """The two faults whose fix the runs can show [F95 pp. 140-142] (D-074)."""
    fg, fg_faults = samples[(FIGURE_GROUND, "standard")], samples[(FIGURE_GROUND, FAULTS)]
    problem = load_problem(FIGURE_GROUND)
    a = share(fg.facts, lambda f: fork_glass_group(problem, f))
    b = share(fg_faults.facts, lambda f: fork_glass_group(problem, f))
    out = [Check(22, "The touched fork is not grouped with the glasses beside it, \"the "
                     "rather unhumanlike action\" of French's program [F95 pp. 141-142] "
                     "(FIXED, D-069)", FIGURE_GROUND, "standard",
                 f"fork-glass group in {_pct(a)} of runs", a < 5.0),
           Check(23, "Under french-faults the fork-glass group forms more often than under "
                     "standard [F95 pp. 141-142] (D-069)", FIGURE_GROUND, FAULTS,
                 f"{_pct(b)} against {_pct(a)}", b > a if (a or b) else None)]
    four, four_faults = samples[(FOUR_CUPS, "standard")], samples[(FOUR_CUPS, FAULTS)]
    problem = load_problem(FOUR_CUPS)
    c = share(four.facts, lambda f: nested_sameness_run(problem, f))
    d = share(four_faults.facts, lambda f: nested_sameness_run(problem, f))
    out.append(Check(24, "Four cups in a row make one group, not every run of two or more "
                         "inside it [F95 p. 140] (FIXED, D-026)", FOUR_CUPS, "standard",
                     f"a nested same-type run at the end in {_pct(c)} of runs", c == 0.0))
    out.append(Check(25, "Under french-faults the intermediate subgroups are kept [F95 "
                         "p. 140] (D-026)", FOUR_CUPS, FAULTS, f"{_pct(d)} against {_pct(c)}",
                     d > c if (c or d) else None))
    return out


# ---- the bench's protocol (D-073, D-074) -----------------------------------------------

def pairs_of(sets=SETS, only=None) -> list:
    """The (problem, profile) pairs, kept to the problems in `only` when given (D-074)."""
    return [pair for pair in sets if only is None or pair[0] in only]


def problems_of(sets=SETS, only=None) -> list:
    """The problem names the bench runs, the chapter 2 configurations included, each once
    (D-074)."""
    names = []
    for name, _ in pairs_of(sets, only):
        if name not in names:
            names.append(name)
    return names + [n for n in ch2.problems_of(ch2.SETS, only) if n not in names]


def planned(sets=SETS, only=None) -> int:
    """How many samples the bench will run, with the chapter 2 series' surveys (D-074)."""
    return len(pairs_of(sets, only)) + ch2.planned(ch2.SETS, only)


@dataclass(frozen=True)
class Stated:
    """The bench's result (D-074): the `samples` by (problem, profile), the `checks`, the
    chapter 2 `series` (`ch2.Ch2Comparison`s) and the `runs` per sample."""

    samples: dict
    checks: tuple
    series: tuple
    runs: int


def run(config: Config, seed: int, runs: int, only=None, progress=None, sets=SETS) -> Stated:
    """Sample every configuration the statements need, under `standard` and
    `french-faults` as each statement asks (the profile given is used for the `standard`
    rows), read the statements, and run the chapter 2 series (D-074)."""
    samples = {}
    for name, profile in pairs_of(sets, only):
        cfg = config if profile == "standard" else PROFILES[profile]
        s = sample(name, cfg, seed, runs)
        samples[(name, profile)] = s
        if progress is not None:
            progress(s.survey)
    checks = []
    if (CH4, "standard") in samples:
        checks += ch4_checks(samples[(CH4, "standard")])
    if (CH4, "standard") in samples and (JUNK, "standard") in samples:
        checks += junk_checks(samples[(CH4, "standard")], samples[(JUNK, "standard")])
    if all((b, "standard") in samples for b in BLOCKAGE):
        checks += blockage_checks(samples)
    if all(k in samples for k in ((FIGURE_GROUND, "standard"), (FIGURE_GROUND, FAULTS),
                                  (FOUR_CUPS, "standard"), (FOUR_CUPS, FAULTS))):
        checks += fault_checks(samples)
    series = ch2.run(config, seed, runs, only, progress)
    return Stated(samples, tuple(checks), tuple(series), runs)


def tallied(checks) -> tuple:
    """(agreements, disagreements, nothing to read) (D-074)."""
    verdicts = [c.verdict for c in checks]
    return (sum(v is True for v in verdicts), sum(v is False for v in verdicts),
            sum(v is None for v in verdicts))


def report(result: Stated) -> list:
    """The bench's text: the statements with their verdicts, the tally, the chapter 4
    milestones' order, and the chapter 2 series' tally (D-074)."""
    lines = ["## French's statements of what the program does, against the runs (M2)", "",
             f"{result.runs} runs per sample under each profile; a verdict of `-` means the "
             "runs showed nothing to read.", "",
             "| No. | Statement | Problem | Profile | Our numbers | Verdict |",
             "|---:|---|---|---|---|---|"]
    for c in result.checks:
        verdict = {True: "yes", False: "no", None: "-"}[c.verdict]
        lines.append(f"| {c.number} | {c.statement} | {c.problem} | {c.profile} | {c.numbers} | "
                     f"{verdict} |")
    yes, no, none = tallied(result.checks)
    lines += ["", f"{yes} of {yes + no} statements agree with the runs"
                  + (f" ({none} with nothing to read)" if none else "") + ".", ""]
    if result.series:
        y2, n2, s2 = ch2.tallied(result.series)
        lines += ["### Chapter 2's series against French's words [F95 pp. 26-35]", "",
                  f"{y2} of {y2 + n2} checks agree" + (f" ({s2} not run)" if s2 else "") + ".",
                  ""]
        lines += ch2.report(result.series)[2:]
    return lines


def records(result: Stated) -> list:
    """The JSON records: one per check, with the samples' surveys, then the series' (D-074)."""
    out = [{"bench": "stated", "number": c.number, "statement": c.statement,
            "problem": c.problem, "profile": c.profile, "numbers": c.numbers,
            "verdict": c.verdict, "source": c.source} for c in result.checks]
    out += [{"bench": "stated", "sample": [name, profile], "survey": H.survey_record(s.survey)}
            for (name, profile), s in result.samples.items()]
    out += ch2.records(result.series)
    return out

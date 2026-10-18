"""Multi-seed runs of the appendix sequences against the transcripts' milestones
[M86 pp. 164-179] (DECISION D-055: statistical tests of a stochastic program over many
seeds, each run reproducible from its seed).

    python3 -m seekwhence.bench appendix                      # the three runs, 100 seeds
    python3 -m seekwhence.bench appendix --runs run1_3737 --seeds 50 --first-seed 100
    python3 -m seekwhence.bench appendix --side-by-side 4     # one seed against the printout
    python3 -m seekwhence.bench appendix --markdown           # the tables for docs/
    python3 -m seekwhence.bench appendix --runs run1_3737 --golden 54 > tests/data/golden_run1_seed54.txt

For each run of `tests/data/expectations.json` the session is replayed with the console
inputs Meredith typed (`inputs_in_order`) and the answers she gave to the guesses, once per
seed; every milestone is matched in order (`seekwhence.milestones`) in its exact reading
(the line as printed) and its reached reading (structure numbers as wildcards), and the
first-appearance order of task types is compared with the transcript's. The result is the
per-milestone success rate over the seeds, the tally of outcomes, and, for one seed, the
transcript's line beside ours. Nothing here changes a run: the bench only drives sessions
and reads what they wrote (drivers only observe). Each run is
replayed in its printout's mode, verbose or terse (D-055).

    python3 -m seekwhence.bench dozen                         # every sequence of `sequences`, 20 seeds
    python3 -m seekwhence.bench dozen --groups blackburn --seeds 50 --terms 16
    python3 -m seekwhence.bench dozen --names blackburn-2 blackburn-8 --markdown
    python3 -m seekwhence.bench dozen --names blackburn-12 --trace 3   # one seed's session

The `dozen` command (item 22; DECISION D-070) runs the sequences the sources name
(`seekwhence.sequences`: the Blackburn dozen [M86 pp. 6-7], the sequences the thesis
reports solved [M86 pp. 88, 145-146], its typical problems [M86 p. 38], its unconquered
sequences [M86 p. 150] and Hofstadter's sampler [FCCA pp. 53-54]) over many seeds each,
feeding up to `--terms` terms of the true continuation one at a time and answering every
guess from the continuation itself: `ok` when the hypothesis's own terms agree with the
continuation as far as it is known, `no` otherwise (a knowing user, not D-033's judgement
of the printed block, which a wrong hypothesis that fits the terms so far can pass). It
reports, per sequence, the seeds solved (an accepted guess), the terms and tasks a solution
took, the forms accepted and the wrong guesses, the final hypotheses of the unsolved seeds
and the blasts, beside Meredith's reported result; `RESULTS.md` keeps the tables.
"""

import argparse
import io
import json
import sys
from collections import Counter
from pathlib import Path

from seekwhence import milestones as ms
from seekwhence import sequences as sq
from seekwhence.config import Config
from seekwhence.filtering import OK, judge
from seekwhence.session import ANSWER_PROMPT, Session

# Where the transcriptions and expectations live, relative to the package (SPEC 15.1).
DATA = Path(__file__).resolve().parent.parent / "tests" / "data"


def load_expectations(data: Path = DATA) -> dict:
    """The transcribed runs' expectations (`tests/data/expectations.json`, SPEC 15.1;
    [M86 pp. 164-179])."""
    return json.loads((data / "expectations.json").read_text())


def run_spec(expectations: dict, run_id: str) -> dict:
    """The expectations of one run by its id (`run1_3737`, `run2_373373`,
    `run3_121231234`; SPEC 15.1; [M86 pp. 164-179])."""
    for spec in expectations["runs"]:
        if spec["id"] == run_id:
            return spec
    raise KeyError(f"no run {run_id!r} in expectations.json")


def inputs_of(spec: dict) -> list:
    """The console lines Meredith typed, terms as integers [M86 pp. 164-179]."""
    return [int(x) if x.isdigit() else x for x in spec["inputs_in_order"]]


def answers_of(spec: dict) -> list:
    """The answers Meredith gave to the guesses, in order (`ok` [M86 p. 166], `no`
    [M86 p. 175])."""
    return [m["answer"] for m in spec["milestones"] if m["kind"] == "guess"]


def replay(spec: dict, seed: int, config: Config | None = None) -> tuple:
    """One session replaying a transcript's console inputs and answers from `seed`: returns
    the output lines and the session (DECISION D-069). Without a `config` the trace is in
    the printout's own mode: verbose for runs 1 and 3, terse for run 2, which prints no task
    lines but does print `set-out -- validity: 0` [M86 pp. 164, 167, 168, 170] (DECISION
    D-055, item 21)."""
    config = config or Config(verbose=spec["prints_task_lines"])
    out = io.StringIO()
    session = Session(config, seed, out=out, lines=inputs_of(spec), answers=answers_of(spec))
    session.run()
    return out.getvalue().splitlines(), session


def survey(spec: dict, seeds, config: Config | None = None) -> dict:
    """Replay one run over `seeds` and count, per milestone, the seeds on which it was found
    in order (exact and reached), the outcomes, the hypotheses of the guesses made, and the
    task-order agreement (DECISION D-055). Returns a dictionary of
    plain data: `seeds`, `exact` and `reached` (lists of counts in milestone order),
    `outcomes`, `guesses`, `order` (missing/extra types and the mean pair agreement),
    `first_exact_seed` (the first seed on which every milestone was found exactly, or
    None), `per_seed` (the number of exact milestones found on each seed), `judged` (the
    guesses made, counted as `right` or `wrong` by the judgement of DECISION D-033 against
    the whole sequence, since the replay gives Meredith's answers blindly: her `ok` to a
    second guess is given to whatever form was guessed second). The `doing task` needles
    are dropped when the trace is terse. One loop over the seeds feeding eight counters;
    kept as one function so that the survey's fields are defined where they are filled
    (`docs/citation-audit.md`, item 24)."""
    seeds = list(seeds)
    milestones = spec["milestones"]
    exact = [0] * len(milestones)
    reached = [0] * len(milestones)
    outcomes = Counter()
    guesses = Counter()
    judged = Counter()
    missing = Counter()
    extra = Counter()
    pairs = agreeing = 0
    first_exact_seed = None
    per_seed = []
    their_order = (spec.get("task_types") or {}).get("first_appearance_order")
    task_lines = (config or Config(verbose=spec["prints_task_lines"])).verbose
    for seed in seeds:
        lines, session = replay(spec, seed, config)
        found_exact = ms.match(lines, milestones, loose=False, ordered=True, task_lines=task_lines)
        found_reached = ms.match(lines, milestones, loose=True, ordered=False, task_lines=task_lines)
        for i, (a, b) in enumerate(zip(found_exact, found_reached)):
            exact[i] += a.found
            reached[i] += b.found
        n_exact = sum(a.found for a in found_exact)
        per_seed.append(n_exact)
        if n_exact == len(milestones) and first_exact_seed is None:
            first_exact_seed = seed
        outcomes[session.outcome] += 1
        for event in session.recorder.of_kind("guess"):
            guesses[event.form] += 1
            judged["right" if judge(event.terms, spec["sequence"]) else "wrong"] += 1
        if their_order and task_lines:
            agreement = ms.task_order_agreement(ms.task_types(lines), their_order)
            missing.update(agreement["missing"])
            extra.update(agreement["extra"])
            pairs += agreement["pairs"]
            agreeing += agreement["agreeing"]
    return {
        "id": spec["id"],
        "seeds": seeds,
        "exact": exact,
        "reached": reached,
        "outcomes": dict(outcomes),
        "guesses": dict(guesses),
        "judged": dict(judged),
        "order": None if not (their_order and task_lines) else {
            "missing": dict(missing), "extra": dict(extra),
            "pairs": pairs, "agreeing": agreeing,
            "agreement": (agreeing / pairs) if pairs else None,
        },
        "first_exact_seed": first_exact_seed,
        "per_seed": per_seed,
    }


def side_by_side(spec: dict, seed: int, config: Config | None = None, data: Path = DATA) -> list:
    """One seed against the printout, milestone by milestone: for each milestone its label,
    page, the transcript's line(s) and ours (or what was not found), for the side-by-side
    of `docs/appendix-runs.md` (SPEC 15.2-15.4; [M86 pp. 164-179]; DECISION D-055).
    Returns rows of plain data."""
    lines, session = replay(spec, seed, config)
    task_lines = spec["prints_task_lines"]
    theirs = ms.match(_console_lines(data / spec["file"]), spec["milestones"],
                      task_lines=task_lines)
    found = ms.match(lines, spec["milestones"], loose=False, ordered=True, task_lines=task_lines)
    reached = ms.match(lines, spec["milestones"], loose=True, ordered=False, task_lines=task_lines)
    rows = []
    for m, t, a, b in zip(spec["milestones"], theirs, found, reached):
        rows.append({
            "label": ms.label(m), "page": m["page"],
            "theirs": t.lines_found,
            "ours": a.lines_found if a.found else b.lines_found,
            "exact": a.found, "reached": b.found,
            "missing": None if a.found else (a.missing.text if a.missing else None),
        })
    return rows


GOLDEN_HEADER = (
    "# Golden trace of the reconstruction: {id}, seed {seed}, the console inputs of",
    "# Meredith's printout [M86 pp. {first}-{last}] and her answers (item 20).",
    "# Written by `python3 -m seekwhence.bench appendix --runs {id} --golden {seed}`; the test",
    "# `tests/test_appendix.py` replays the same session and requires this text exactly.",
    "# Never edited by hand and never regenerated to make a failing test pass: a change",
    "# in the engine that changes this run is",
    "# judged on its merits and, if it is meant, the file is rewritten with the reason in",
    "# PROGRESS.md. Lines beginning with '#' are this header, not program output.",
    "",
)


def golden_text(spec: dict, seed: int, config: Config | None = None) -> str:
    """The golden regression file of one run on one seed: a header, then the session's
    output line for line (golden files; DECISION D-055)."""
    lines, session = replay(spec, seed, config)
    header = [line.format(id=spec["id"], seed=seed, first=spec["pages"][0], last=spec["pages"][1])
              for line in GOLDEN_HEADER]
    return "\n".join(header + lines) + "\n"


def _console_lines(path: Path) -> list:
    """The console lines of a transcription (comments, page markers and notes dropped)."""
    return [raw for raw in path.read_text().splitlines()
            if not raw.startswith("#")]


# ---- reporting ---------------------------------------------------------------------------------------

def report(spec: dict, result: dict, markdown: bool = False) -> str:
    """The survey of one run as text: the outcomes, the guesses, the task-order agreement,
    and one row per milestone with its exact and reached counts (DECISION D-055)."""
    n = len(result["seeds"])
    lines = []
    title = f"{spec['id']} ({spec['title_as_printed'].strip()}), pp. {spec['pages'][0]}-{spec['pages'][1]}, {n} seeds"
    lines.append(f"## {title}" if markdown else title)
    lines.append("")
    lines.append(f"outcomes: {_counts(result['outcomes'])}")
    lines.append(f"guesses: {_counts(result['guesses'])}")
    lines.append(f"guesses judged against the sequence: {_counts(result['judged']) or 'none'}")
    seeds = result["seeds"]
    full = sum(1 for k in result["per_seed"] if k == len(spec["milestones"]))
    lines.append(f"seeds with every milestone found exactly: {full} of {n}"
                 + (f" (first: {result['first_exact_seed']})" if result["first_exact_seed"] is not None else ""))
    order = result["order"]
    if order:
        agreement = "n/a" if order["agreement"] is None else f"{100 * order['agreement']:.0f}%"
        lines.append(f"task-type first-appearance order: {agreement} of the transcript's "
                     f"consecutive pairs hold ({order['agreeing']} of {order['pairs']} over the seeds); "
                     f"types never run: {_counts(order['missing']) or 'none'}; "
                     f"types run that the transcript never printed: {_counts(order['extra']) or 'none'}")
    else:
        lines.append("task-type order: the printout has no task lines")
    lines.append("")
    if markdown:
        lines.append("| # | milestone | p. | exact | reached |")
        lines.append("|---|---|---|---|---|")
    for i, m in enumerate(spec["milestones"]):
        s, l = result["exact"][i], result["reached"][i]
        if markdown:
            lines.append(f"| {i + 1} | {ms.label(m).replace('|', '/')} | {m['page']} | {s}/{n} | {l}/{n} |")
        else:
            lines.append(f"{i + 1:3d}  {ms.label(m):60s} p. {m['page']}  exact {s:3d}/{n}  reached {l:3d}/{n}")
    del seeds
    return "\n".join(lines)


def report_side_by_side(spec: dict, seed: int, rows: list, markdown: bool = False) -> str:
    """The side-by-side of one seed as text or a markdown table (SPEC 15.2-15.4;
    [M86 pp. 164-179]; DECISION D-055)."""
    lines = [f"### {spec['id']}, seed {seed}: the transcript beside the reconstruction" if markdown
             else f"{spec['id']}, seed {seed}"]
    lines.append("")
    if markdown:
        lines.append("| # | milestone (p.) | Meredith's printout | seed %d | found |" % seed)
        lines.append("|---|---|---|---|---|")
    for i, row in enumerate(rows):
        theirs = "<br>".join(f"`{t}`" for t in row["theirs"]) if markdown else " / ".join(row["theirs"])
        ours = "<br>".join(f"`{t}`" for t in row["ours"]) if markdown else " / ".join(row["ours"])
        found = "yes" if row["exact"] else ("reached" if row["reached"] else "no")
        if not row["exact"] and not row["reached"] and row["missing"]:
            ours = (ours + "<br>" if ours else "") + f"not found: `{row['missing']}`" if markdown \
                else (ours + " / " if ours else "") + f"not found: {row['missing']}"
        if markdown:
            lines.append(f"| {i + 1} | {row['label'].replace('|', '/')} ({row['page']}) | {theirs} | {ours} | {found} |")
        else:
            lines.append(f"{i + 1:3d} [{found:5s}] {row['label']} (p. {row['page']})")
            lines.append(f"       theirs: {theirs}")
            lines.append(f"       ours:   {ours}")
    return "\n".join(lines)


def _counts(counter: dict) -> str:
    return ", ".join(f"{k}: {v}" for k, v in sorted(counter.items(), key=lambda kv: (-kv[1], str(kv[0]))))


# ---- the dozen and the other sequences (item 22; DECISION D-070) -------------------------------------

def predicts(hypothesis, continuation) -> bool:
    """Whether a hypothesis's own terms agree with a known continuation over its whole
    length (DECISION D-070): the box is fired afresh ("asked to list a number of terms"
    [M86 p. 82]); a form whose box cannot give the terms (a P-group below 0, DECISION
    D-006) predicts nothing."""
    continuation = list(continuation)
    if hypothesis is None or not continuation:
        return False
    try:
        return hypothesis.list_terms(len(continuation)) == continuation
    except ValueError:
        return False


class ContinuationAnswerer:
    """The bench's `Answerer` (DECISIONS D-064, D-070): a user who knows the rule. A guess
    is answered `ok` when the hypothesis's terms agree with the true continuation as far
    as it is known (`predicts`), `no` otherwise; the answer is written after the prompt as
    the session's answerer does (D-069). `given` keeps, per guess, the form, the answer,
    and whether D-033's judgement of the printed block alone would have accepted it."""

    def __init__(self, console, continuation):
        self.console = console
        self.continuation = tuple(continuation)
        self.given: list = []

    def answer(self, world, form: str, block: str, terms: tuple) -> str:
        """The answer to one guess (DECISION D-070)."""
        del block
        right = predicts(world.hypothesis, self.continuation)
        answer = OK if right else "no"
        self.console.tell("\n" + ANSWER_PROMPT, answer)
        self.given.append({"form": form, "answer": answer,
                           "block_right": judge(terms, self.continuation)})
        return answer


def run_sequence(seq: sq.Sequence, seed: int, n_terms: int, config: Config | None = None,
                 out=None) -> dict:
    """One session on a sequence of the catalogue from `seed`: up to `n_terms` terms of its
    continuation fed one at a time, every guess judged by the continuation (DECISION
    D-070). Returns plain data: `seed`, `outcome`, `terms` (entered), `tasks`,
    `hypothesis` (the final form or None), `solved` (a guess accepted), `guesses` (the
    answerer's records), `hypotheses` (every form made, stage one's and the candidates, in
    order), `refutations`, `blasts`, and `session` (for a trace)."""
    config = config or Config(verbose=False)
    out = out if out is not None else io.StringIO()
    holder = {}

    def make_answerer(console):
        holder["answerer"] = ContinuationAnswerer(console, seq.terms)
        return holder["answerer"]

    session = Session(config, seed, out=out, lines=list(seq.prefix(n_terms)),
                      answerer=make_answerer)
    session.run()
    recorder = session.recorder
    world = session.world
    form = session.show_hypothesis()
    return {
        "seed": seed,
        "outcome": session.outcome,
        "terms": len(world.terms),
        "tasks": world.clock,
        "hypothesis": None if form == "nil" else form,
        "solved": session.outcome == "ok",
        "guesses": holder["answerer"].given,
        "hypotheses": [e.form for e in recorder.events if e.kind in ("hypothesis", "candidate")],
        "refutations": len(recorder.of_kind("refuted")),
        "blasts": len(recorder.of_kind("blasted")),
        "session": session,
    }


def survey_sequence(seq: sq.Sequence, seeds, n_terms: int, config: Config | None = None) -> dict:
    """One sequence over `seeds` (DECISION D-070): the seeds solved,
    the terms and tasks the solutions took (min, median, max), the forms accepted and
    refused, the final hypotheses of the unsolved seeds, the outcomes, the refutations and
    blasts per seed, and the number of guesses the printed block alone would have passed
    (D-033) but the continuation refused."""
    seeds = list(seeds)
    solved_terms, solved_tasks = [], []
    accepted, refused, unsolved_final = Counter(), Counter(), Counter()
    outcomes = Counter()
    refutations = blasts = block_only = 0
    per_seed = []
    for seed in seeds:
        r = run_sequence(seq, seed, n_terms, config)
        per_seed.append({k: v for k, v in r.items() if k != "session"})
        outcomes[r["outcome"]] += 1
        refutations += r["refutations"]
        blasts += r["blasts"]
        for g in r["guesses"]:
            if g["answer"] == OK:
                accepted[g["form"]] += 1
            else:
                refused[g["form"]] += 1
                if g["block_right"]:
                    block_only += 1
        if r["solved"]:
            solved_terms.append(r["terms"])
            solved_tasks.append(r["tasks"])
        else:
            unsolved_final[r["hypothesis"] or "none"] += 1
    return {
        "name": seq.name,
        "seeds": seeds,
        "n_terms": n_terms,
        "solved": len(solved_terms),
        "terms": _spread(solved_terms),
        "tasks": _spread(solved_tasks),
        "accepted": dict(accepted),
        "refused": dict(refused),
        "unsolved_final": dict(unsolved_final),
        "outcomes": dict(outcomes),
        "refutations": refutations,
        "blasts": blasts,
        "block_only": block_only,
        "per_seed": per_seed,
    }


def _spread(values) -> dict | None:
    if not values:
        return None
    ordered = sorted(values)
    return {"min": ordered[0], "median": ordered[len(ordered) // 2], "max": ordered[-1]}


def survey_sequences(seqs, seeds, n_terms: int, config: Config | None = None) -> list:
    """`survey_sequence` over a list of sequences (DECISION D-070)."""
    return [survey_sequence(seq, seeds, n_terms, config) for seq in seqs]


def report_dozen(seqs, results, markdown: bool = False) -> str:
    """The surveys as text or as a markdown table (DECISION D-070): one row per sequence
    with the terms the source prints, Meredith's reported result, the seeds solved, the
    median terms and tasks of a solution, the forms accepted, the guesses refused, the
    final hypotheses of the unsolved seeds and the blasts."""
    lines = []
    if markdown:
        lines.append("| sequence | terms as printed | Meredith | solved | terms | tasks "
                     "| forms accepted | guesses refused | unsolved seeds end with | blasts |")
        lines.append("|---|---|---|---|---|---|---|---|---|---|")
    for seq, r in zip(seqs, results):
        n = len(r["seeds"])
        reported = seq.reported or "no verdict"
        terms = "-" if r["terms"] is None else f"{r['terms']['median']} ({r['terms']['min']}-{r['terms']['max']})"
        tasks = "-" if r["tasks"] is None else f"{r['tasks']['median']}"
        if markdown:
            lines.append(f"| {seq.name} | `{seq.printed}` | {reported} | {r['solved']}/{n} | {terms} | {tasks} "
                         f"| {_forms(r['accepted'], markdown)} | {_forms(r['refused'], markdown)} "
                         f"| {_forms(r['unsolved_final'], markdown)} | {r['blasts']} |")
        else:
            lines.append(f"{seq.name}: {seq.printed} ...   {seq.tag}   Meredith: {reported}")
            lines.append(f"    solved {r['solved']}/{n} seeds; terms {terms}; tasks (median) {tasks}; "
                         f"outcomes {_counts(r['outcomes'])}; refutations {r['refutations']}, "
                         f"blasts {r['blasts']}, refused guesses the block alone would pass {r['block_only']}")
            lines.append(f"    accepted: {_forms(r['accepted']) or 'none'}")
            lines.append(f"    refused: {_forms(r['refused']) or 'none'}")
            lines.append(f"    unsolved seeds end with: {_forms(r['unsolved_final']) or 'none'}")
    return "\n".join(lines)


def _forms(counter: dict, markdown: bool = False) -> str:
    items = sorted(counter.items(), key=lambda kv: (-kv[1], str(kv[0])))
    if markdown:
        return ", ".join(f"`{k}` {v}" for k, v in items) or "none"
    return ", ".join(f"{k} x{v}" for k, v in items)


def dozen_summary(seqs, results) -> dict:
    """The agreement with the thesis's verdicts (DECISION D-070): for each sequence with a
    verdict, whether the reconstruction solves it on at least half the seeds; the counts of
    agreements and disagreements by kind."""
    rows = []
    for seq, r in zip(seqs, results):
        n = len(r["seeds"])
        ours = "solved" if r["solved"] * 2 >= n and r["solved"] > 0 else "unsolved"
        theirs = seq.reported or None
        if theirs is None:
            verdict = "no verdict"
        elif theirs == "solved":
            verdict = "agree" if ours == "solved" else "she solved, we do not"
        else:
            verdict = "agree" if ours == "unsolved" else "we solve, she did not"
        rows.append({"name": seq.name, "theirs": theirs, "ours": ours,
                     "rate": f"{r['solved']}/{n}", "verdict": verdict})
    return {"rows": rows, "counts": dict(Counter(row["verdict"] for row in rows))}


# ---- the command line ---------------------------------------------------------------------------------------

def main(argv=None) -> int:
    """`python3 -m seekwhence.bench appendix ...` and `... dozen ...` (the module docstring
    lists the options; DECISIONS D-055, D-070)."""
    parser = argparse.ArgumentParser(prog="python3 -m seekwhence.bench",
                                     description="multi-seed runs of the appendix sequences, "
                                                 "the Blackburn dozen and the other sequences")
    sub = parser.add_subparsers(dest="command", required=True)
    d = sub.add_parser("dozen", help="run the sequences the sources name over many seeds")
    d.add_argument("--groups", nargs="+", default=list(sq.GROUPS),
                   help=f"among {', '.join(sq.GROUPS)}")
    d.add_argument("--names", nargs="+", default=None, help="sequences by name, e.g. blackburn-2")
    d.add_argument("--seeds", type=int, default=20)
    d.add_argument("--first-seed", type=int, default=0)
    d.add_argument("--terms", type=int, default=16, help="terms fed, at most")
    d.add_argument("--per-term-cap", type=int, default=None,
                   help="Config.max_tasks_per_term (D-011)")
    d.add_argument("--trace", type=int, default=None, metavar="SEED",
                   help="print one seed's session on each sequence instead of a survey")
    d.add_argument("--markdown", action="store_true")
    d.add_argument("--json", type=Path, default=None, help="write the surveys as JSON")
    p = sub.add_parser("appendix", help="replay the appendix runs over many seeds")
    p.add_argument("--runs", nargs="+", default=["run1_3737", "run2_373373", "run3_121231234"])
    p.add_argument("--seeds", type=int, default=100)
    p.add_argument("--first-seed", type=int, default=0)
    p.add_argument("--data", type=Path, default=DATA)
    p.add_argument("--side-by-side", type=int, default=None, metavar="SEED")
    p.add_argument("--golden", type=int, default=None, metavar="SEED",
                   help="print the golden trace of one seed (one run at a time)")
    p.add_argument("--markdown", action="store_true")
    args = parser.parse_args(argv)
    if args.command == "dozen":
        return main_dozen(args)
    expectations = load_expectations(args.data)
    out = []
    for run_id in args.runs:
        spec = run_spec(expectations, run_id)
        if args.golden is not None:
            sys.stdout.write(golden_text(spec, args.golden))
            return 0
        if args.side_by_side is None:
            result = survey(spec, range(args.first_seed, args.first_seed + args.seeds))
            out.append(report(spec, result, args.markdown))
        else:
            rows = side_by_side(spec, args.side_by_side, data=args.data)
            out.append(report_side_by_side(spec, args.side_by_side, rows, args.markdown))
        out.append("")
    sys.stdout.write("\n".join(out))
    return 0


def main_dozen(args) -> int:
    """The `dozen` command (DECISION D-070)."""
    seqs = [sq.find(name) for name in args.names] if args.names else sq.of_groups(args.groups)
    config = Config(verbose=False) if args.per_term_cap is None \
        else Config(verbose=False, max_tasks_per_term=args.per_term_cap)
    if args.trace is not None:
        for seq in seqs:
            sys.stdout.write(f"==== {seq.name}: {seq.printed} ... {seq.tag}; seed {args.trace}\n")
            r = run_sequence(seq, args.trace, args.terms, config, out=sys.stdout)
            sys.stdout.write(r["session"].summary() + "\n\n")
        return 0
    seeds = range(args.first_seed, args.first_seed + args.seeds)
    results = survey_sequences(seqs, seeds, args.terms, config)
    sys.stdout.write(report_dozen(seqs, results, args.markdown) + "\n")
    summary = dozen_summary(seqs, results)
    sys.stdout.write("\nagainst the thesis's verdicts: " + _counts(summary["counts"]) + "\n")
    if args.json is not None:
        args.json.write_text(json.dumps({"terms": args.terms, "results": results,
                                         "summary": summary}, indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main())

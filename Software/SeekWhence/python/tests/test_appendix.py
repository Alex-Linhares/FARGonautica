"""The three appendix runs replayed against the transcripts [M86 pp. 164-179] (DECISION
D-055: golden files and seeded statistical tests of a stochastic program; the thresholds below are set from the rates measured over seeds 0-99 by
`python3 -m seekwhence.bench appendix` and recorded in `docs/appendix-runs.md`)."""

from pathlib import Path

import pytest

from seekwhence import bench
from seekwhence import milestones as ms
from seekwhence.config import Config

DATA = Path(__file__).parent / "data"
GOLDEN_SEED = 54        # the representative seed of runs 1 and 2 in docs/appendix-runs.md
RUN3_SEED = 30          # the representative seed of run 3
SAMPLE = range(0, 100)  # the seeds of the rate tests (a few seconds for the three runs)
RUNS = ("run1_3737", "run2_373373", "run3_121231234")


@pytest.fixture(scope="module")
def expectations():
    return bench.load_expectations(DATA)


@pytest.fixture(scope="module")
def surveys(expectations):
    """Both runs surveyed once over the sample; the tests below read the counts."""
    return {run_id: bench.survey(bench.run_spec(expectations, run_id), SAMPLE)
            for run_id in RUNS}


def count(survey, spec, kind, reading="reached", **fields):
    """The seeds on which the first milestone of `kind` with the given fields was found."""
    for i, m in enumerate(spec["milestones"]):
        if m["kind"] == kind and all(m.get(k) == v for k, v in fields.items()):
            return survey[reading][i]
    raise KeyError((kind, fields))


# ---- the matcher against the transcriptions themselves ----------------------------------------

def transcript_lines(path: Path) -> list:
    return [raw for raw in path.read_text().splitlines() if not raw.startswith("#")]


@pytest.mark.parametrize("run_id", ["run1_3737", "run2_373373", "run3_121231234"])
def test_every_milestone_is_found_exactly_in_its_own_transcript(expectations, run_id):
    """The needles of every milestone kind, applied to Meredith's own printout, find every
    milestone in order (the transcription is the printout line for line, SPEC 15.1); the
    `doing task` needles are dropped for run 2, which prints no task lines [M86 p. 167]."""
    spec = bench.run_spec(expectations, run_id)
    lines = transcript_lines(DATA / spec["file"])
    # the reigning-class change is printed by the reconstruction only (DECISION D-042);
    # the printout has Meredith's annotation
    milestones = [m for m in spec["milestones"] if m.get("printed_by_program", True)]
    found = ms.match(lines, milestones, loose=False, ordered=True,
                     task_lines=spec["prints_task_lines"])
    missing = [(ms.label(m.milestone), m.missing.text) for m in found if not m.found]
    assert missing == []


def test_loose_needles_treat_structure_numbers_but_not_glints_as_wildcards():
    """`glom4`, `spark6`, `bond1` are not milestones (DECISION D-055); a glint's number is
    its term's position [M86 p. 61] and stays; a spark names its elements in either
    order (DECISION D-025)."""
    n = ms.Needle("top-down glom glom4")
    assert n.found_in("top-down glom glom4", loose=False)
    assert not n.found_in("top-down glom glom9", loose=False)
    assert n.found_in("top-down glom glom9", loose=True)
    b = ms.Needle("bond3 --- between glint1glint4")
    assert b.found_in("bond2 --- between glint1glint4", loose=True)
    assert not b.found_in("bond3 --- between glint2glint4", loose=True)
    s = ms.needles({"kind": "spark", "name": "spark1", "between": ["glint1", "glint2"]})[0]
    assert s.found_in("spark1 --- between glint2glint1", loose=True)
    assert not s.found_in("spark1 --- between glint2glint1", loose=False)


def test_milestones_are_matched_within_their_turn():
    """A line printed after a later term does not count for an earlier milestone, and a
    milestone's turn is its input's (DECISION D-055)."""
    milestones = [{"kind": "term", "index": 1, "value": 3, "page": 1},
                  {"kind": "bond", "name": "bond1", "between": ["glint1", "glint3"], "page": 1},
                  {"kind": "term", "index": 2, "value": 7, "page": 1}]
    lines = ["please enter a term: 3", "please enter a term: 7", "bond1 --- between glint1glint3"]
    found = ms.exact(lines, milestones)
    assert [m.found for m in found] == [True, False, True]
    lines = ["please enter a term: 3", "bond1 --- between glint1glint3", "please enter a term: 7"]
    assert all(m.found for m in ms.exact(lines, milestones))
    assert [m.found for m in ms.exact(lines[:1], milestones)] == [True, False, False]


def test_task_order_agreement():
    """Consecutive first-appearance pairs of the transcript that hold in ours
    (DECISION D-055)."""
    theirs = ["Sparkler-plus", "Dissolver", "Sparkler", "Tester"]
    ours = ["Sparkler-plus", "Sparkler", "Dissolver", "Tester", "Sparkler"]
    a = ms.task_order_agreement(ours, theirs)
    assert (a["pairs"], a["agreeing"]) == (3, 2)
    assert a["missing"] == [] and a["extra"] == []
    a = ms.task_order_agreement(["Tester", "Bonder"], theirs)
    assert a["missing"] == ["Sparkler-plus", "Dissolver", "Sparkler"] and a["extra"] == ["Bonder"]
    assert a["pairs"] == 0


# ---- the golden traces --------------------------------------------------------------------

@pytest.mark.parametrize("run_id, seed, name", [("run1_3737", 54, "golden_run1_seed54.txt"),
                                                ("run2_373373", 54, "golden_run2_seed54.txt"),
                                                ("run3_121231234", 30, "golden_run3_seed30.txt")])
def test_the_golden_traces(expectations, run_id, seed, name):
    """The session on the representative seed with Meredith's inputs and answers prints
    exactly the golden file, header included, each run in its printout's mode (run 2
    terse); the file is never regenerated to make this pass (Metacat's rule for its golden
    traces). Run 2's file was rewritten in item 21 (the terse
    mode, the previous frame's cap dissolved at the refutation; PROGRESS.md)."""
    spec = bench.run_spec(expectations, run_id)
    assert bench.golden_text(spec, seed) == (DATA / name).read_text()


def test_seed_54_reaches_every_milestone_of_both_runs(expectations):
    """The representative seed of `docs/appendix-runs.md`: every milestone of run 1 and of
    run 2 is reached, and all but the Plato-scout's place among the tasks of the third
    term (run 1) and the number of the frame glom (run 2, `glom9` against ours) are
    exact [M86 pp. 164-169]."""
    for run_id, inexact in (("run1_3737", {"plato-scout"}), ("run2_373373", {"hfilter"})):
        spec = bench.run_spec(expectations, run_id)
        lines, session = bench.replay(spec, GOLDEN_SEED)
        task_lines = spec["prints_task_lines"]
        assert session.outcome == "ok"
        assert all(m.found for m in ms.match(lines, spec["milestones"], loose=True,
                                             ordered=False, task_lines=task_lines))
        found = ms.match(lines, spec["milestones"], loose=False, ordered=True, task_lines=task_lines)
        not_exact = {m.milestone["kind"] for m in found if not m.found}
        assert not_exact == inexact


# The milestones of run 3 (numbered as in expectations.json and docs/appendix-runs.md) that
# the reconstruction never reaches, each a recorded divergence (docs/appendix-runs.md):
NEVER_REACHED_IN_RUN_3 = {
    25: "a Plato-scout on one of the Gnoth-setter's gloms (D-036: its caps get no scout)",
    28: "the PROGRAM wrapper `(ENCLOSE gnoth1 nil)` round a top-level shift (D-044)",
    43: "`(1 2)1` printed for the hypothesis after the refused guess (unexplained)",
    62: "p. 178's `show`: its bonds are not all made, and in another order",
}


def test_seed_30_reaches_59_of_run_3s_67_milestones(expectations):
    """The representative seed of run 3 [M86 pp. 170-179]: every milestone is reached but
    eight, each a recorded divergence (docs/appendix-runs.md): the four never reached
    (`NEVER_REACHED_IN_RUN_3`), the second Glomtester failing on glint1 where hers failed on
    glint2 (8), `bond4` between glom1 and glint3 (23, made before the Gnoth-setter on 14
    seeds), p. 174's `show` (37, the bonds numbered the other way round on this seed) and the
    bare `(SHIFT-RIGHT gnoth3 gnoth5 (glint3))` (51, the 3 is moved left instead, the same
    parse). The outcome is Meredith's: the wrong guess refused, the right one accepted."""
    spec = bench.run_spec(expectations, "run3_121231234")
    lines, session = bench.replay(spec, RUN3_SEED)
    assert session.outcome == "ok"
    assert [e.form for e in session.recorder.of_kind("guess")] == ["(S-group 1 2)", "(S-group 1 (Countup 2))"]
    not_reached = {i + 1 for i, m in enumerate(ms.reached(lines, spec["milestones"])) if not m.found}
    assert not_reached == set(NEVER_REACHED_IN_RUN_3) | {8, 23, 37, 51}
    exact = sum(m.found for m in ms.exact(lines, spec["milestones"]))
    assert exact >= 40


# ---- the rates over the sample (thresholds from the measured rates) ----------------------------

def test_run_1_rates(expectations, surveys):
    """Run 1 over 100 seeds, measured (docs/appendix-runs.md): every term entered and
    `bond1` made on 100; the fence `(3 7)` glommed, the Cycle template and hypothesis, the
    confirming Hfilter and the accepted guess `3 7 3 7` on 35 (the cover `(3 7 3)` wins
    the rest, DECISION D-020); `gnoths constructed` on 100. Thresholds: 100, 100, 25, 25,
    100."""
    spec = bench.run_spec(expectations, "run1_3737")
    s = surveys["run1_3737"]
    n = len(SAMPLE)
    for index in (1, 2, 3, 4):
        assert count(s, spec, "term", index=index) == n
    assert count(s, spec, "bond", name="bond1") == n
    assert count(s, spec, "gnoths-constructed") == n
    fence = count(s, spec, "glom", name="glom1")
    assert fence >= 0.25 * n
    assert count(s, spec, "hypothesis", ordinal=1) >= 0.25 * n
    assert count(s, spec, "guess") >= 0.25 * n
    assert count(s, spec, "guess") == s["outcomes"].get("ok", 0)
    assert set(s["guesses"]) == {"(Cycle 3 2 (3 7))"}
    assert s["outcomes"].get("ok", 0) + s["outcomes"].get("no-terms", 0) == n


def test_run_1_task_order(expectations, surveys):
    """Run 1's first-appearance order of task types [M86 pp. 164-166]: 90% of its
    consecutive pairs hold over the seeds (threshold 80%); every type it prints runs on
    some seed."""
    s = surveys["run1_3737"]["order"]
    assert s["agreement"] >= 0.8
    assert set(s["missing"]) <= {"Template-applier"}     # the cover seeds' template needs none


def test_run_2_rates(expectations, surveys):
    """Run 2 over 100 seeds, measured (docs/appendix-runs.md): the fence, `show` and the
    refutation of `(Cycle 3 2 (3 7))` on 35; `slip-check: stayval: 0.0 / best: nil` on 30
    (5 fence seeds see `bond2` before the Gnoth-setter and slip to C-group); the casts, the
    candidate `(Cycle 3 3 (3 7 3))` and its accepted guess `3 7 3 3 7` on 30; the outcome
    `ok` on 97 (the cover seeds guess `(Y-group 3 3 (3 7 3))` at the fourth term, which
    predicts the same terms). Thresholds: 25, 20, 20, 90."""
    spec = bench.run_spec(expectations, "run2_373373")
    s = surveys["run2_373373"]
    n = len(SAMPLE)
    for index in (1, 2, 3, 4):
        assert count(s, spec, "term", index=index) == n
    fence = count(s, spec, "glom", name="glom1")
    refuted = count(s, spec, "refutation")
    assert fence >= 0.25 * n and refuted == count(s, spec, "command", name="show") == fence
    stayed = count(s, spec, "slip-check")
    assert stayed >= 0.2 * n and stayed >= 0.7 * fence
    candidate = count(s, spec, "new-hypothesis-candidate")
    assert candidate >= 0.2 * n and candidate <= stayed
    assert count(s, spec, "guess") == candidate
    assert s["outcomes"].get("ok", 0) >= 0.9 * n
    assert set(s["guesses"]) <= {"(Cycle 3 3 (3 7 3))", "(Y-group 3 3 (3 7 3))"}
    assert s["guesses"]["(Cycle 3 3 (3 7 3))"] == candidate


def test_the_cover_seeds_of_run_2_guess_the_same_terms_one_term_earlier(expectations):
    """A cover seed makes `(Y-group 3 3 (3 7 3))` at the third term, which the fourth term
    `3` confirms, so the guess `(3 7 3)(3 7 3)(3 7 3)` (a group per hit, as `(1 2)(1 2)(1 2)`
    [M86 p. 175]) comes at the fourth term and is accepted: the Y-group and Meredith's
    `(Cycle 3 3 (3 7 3))` predict the same terms [M86 pp. 168-169] (a finding, RESULTS.md)."""
    spec = bench.run_spec(expectations, "run2_373373")
    seen = 0
    for seed in SAMPLE:
        lines, session = bench.replay(spec, seed)
        guesses = session.recorder.of_kind("guess")
        if guesses and guesses[0].form == "(Y-group 3 3 (3 7 3))":
            seen += 1
            assert guesses[0].block == "(3 7 3)(3 7 3)(3 7 3)"
            assert session.outcome == "ok" and len(session.world.terms) == 4
            if seen == 5:
                break
    assert seen == 5


def test_run_3_rates(expectations, surveys):
    """Run 3 over 100 seeds, measured (docs/appendix-runs.md): `(Countup 1)`, its refutation
    by the third term, `gnoths: (gnoth1 gnoth2 gnoth3)`, `Countup --> S-group`, the
    Bond-assessor at 8.0 and 10.0, the casts, `(S-group 1 2)` and its wrong guess `(1 2)(1 2)
    (1 2)` refused on 100; `glom3 is not in cytoplasm` before the Gnoth-setter on 91; the two
    Dissolvers of p. 176 finding their gloms gone on 98; `gnoths: (gnoth2 gnoth3 gnoth5)` on
    100; `slip-check: stayval: 15.0 / best: (Y-group 4.0)` on 31 (the other seeds see more
    bonds and stay with larger numbers); `(S-group 1 (Countup 2))` and its accepted guess on
    95 (4 seeds blast and guess a Tuple, 1 runs out of terms). Thresholds: 100, 80, 90, 100,
    20, 85. The task-type order agrees 91% (threshold 80%)."""
    spec = bench.run_spec(expectations, "run3_121231234")
    s = surveys["run3_121231234"]
    n = len(SAMPLE)
    for index in range(1, 7):
        assert count(s, spec, "term", index=index) == n
    assert count(s, spec, "hypothesis", form="(Countup 1)") == n
    assert count(s, spec, "hfilter", through="(Countup 1)") == n
    assert count(s, spec, "glom-dissolved", glom="glom3") >= 0.8 * n
    assert count(s, spec, "gnoth-setter", page=173) == n
    assert count(s, spec, "reigning-class-change") == n
    assert count(s, spec, "bond-assessor", page=174, threshold=8.0) == n
    assert count(s, spec, "bond-assessor", page=174, threshold=10.0) == n
    assert count(s, spec, "casts", generalised="(S-group 1 2)") == n
    assert count(s, spec, "new-hypothesis-candidate", form="(S-group 1 2)") == n
    assert count(s, spec, "guess", hypothesis="(S-group 1 2)") == n
    assert count(s, spec, "glom-dissolved", glom="glom8") >= 0.9 * n
    assert count(s, spec, "glom-dissolved", glom="glom10") >= 0.9 * n
    assert count(s, spec, "gnoth-setter", page=176) == n
    assert count(s, spec, "slip-check") >= 0.2 * n
    final = count(s, spec, "new-hypothesis-candidate", form="(S-group 1 (Countup 2))")
    assert final >= 0.8 * n
    assert count(s, spec, "guess", hypothesis="(S-group 1 (Countup 2))") >= 0.85 * n
    assert s["guesses"]["(S-group 1 2)"] == n
    assert set(s["guesses"]) <= {"(S-group 1 2)", "(S-group 1 (Countup 2))", "(Tuple 1 5 (1 2 1 2 3))"}
    assert s["judged"]["right"] == s["guesses"]["(S-group 1 (Countup 2))"] >= 0.85 * n
    assert s["judged"]["wrong"] == n + s["guesses"].get("(Tuple 1 5 (1 2 1 2 3))", 0)
    assert s["outcomes"].get("ok", 0) >= 0.95 * n
    for number in NEVER_REACHED_IN_RUN_3:
        assert s["reached"][number - 1] == 0, number     # a recorded divergence; if this changes, say so in docs/appendix-runs.md
    assert s["order"]["agreement"] >= 0.8 and s["order"]["missing"] == {}


def test_the_second_episode_reaches_p178s_parse(expectations):
    """When the third candidate `(S-group 1 (Countup 2))` is made, `show-parse` prints
    `((1 2) (1 2 3))` [M86 p. 178]: the 3 has been moved in beside the second `(1 2)`, by
    `(SHIFT-LEFT gnoth3 gnoth5 (glint5))` on most seeds, by two SHIFT-RIGHTs ending in p. 177's
    own `(SHIFT-RIGHT gnoth3 gnoth5 (glint3))` on a few (RESULTS.md; D-043's finding)."""
    spec = bench.run_spec(expectations, "run3_121231234")
    parses, operations = 0, set()
    for seed in SAMPLE:
        lines, session = bench.replay(spec, seed)
        if "new hypoth candidate (S-group 1 (Countup 2))" not in lines:
            continue
        i = lines.index("new hypoth candidate (S-group 1 (Countup 2))")
        rest = lines[i:]
        j = rest.index("please enter a term: show-parse")
        parses += rest[j + 1] == "((1 2) (1 2 3))"
        operations.update(l.split(" on ", 1)[1] for l in lines[:i] if "Gnoth-operator on ((SHIFT" in l
                          and "gnoth1" not in l)
    assert parses >= 0.8 * len(SAMPLE)
    assert "((SHIFT-LEFT gnoth3 gnoth5 (glint5)))" in operations


def test_the_survey_is_reproducible(expectations):
    """Same seeds, same counts (the runs are reproducible from their seeds, D-007); a terse
    replay of a verbose run drops the `doing task` needles and keeps the outcomes."""
    spec = bench.run_spec(expectations, "run1_3737")
    a = bench.survey(spec, range(5))
    b = bench.survey(spec, range(5))
    assert (a["exact"], a["reached"], a["outcomes"]) == (b["exact"], b["reached"], b["outcomes"])
    terse = bench.survey(spec, range(5), Config(verbose=False))
    assert terse["outcomes"] == a["outcomes"] and terse["order"] is None


def test_the_bench_command_line(capsys, expectations):
    """`python3 -m seekwhence.bench appendix` prints a report per run and the side-by-side
    of a seed (DECISION D-055)."""
    assert bench.main(["appendix", "--runs", "run1_3737", "--seeds", "3"]) == 0
    out = capsys.readouterr().out
    assert "run1_3737" in out and "exact" in out and "outcomes:" in out
    assert bench.main(["appendix", "--runs", "run2_373373", "--side-by-side", "54", "--markdown"]) == 0
    out = capsys.readouterr().out
    assert "| 29 | bye (169) |" in out
    assert bench.main(["appendix", "--runs", "run1_3737", "--golden", "54"]) == 0
    assert capsys.readouterr().out == (DATA / "golden_run1_seed54.txt").read_text()
    assert bench.main(["appendix", "--runs", "run3_121231234", "--side-by-side", "30"]) == 0
    assert "67 [yes  ] bye" in capsys.readouterr().out

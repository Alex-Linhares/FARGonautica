"""The Blackburn dozen and the other sequences the sources name, through the bench's
`dozen` command [M86 pp. 6-7, 38, 88, 145-147, 150; FCCA pp. 53-54] (DECISION D-070:
a stochastic program is tested over many seeds, asserting what is stable). The fast sample here asserts what the full survey of item 22 (RESULTS.md, 20 seeds
and 16 terms per sequence) found stable: the sequences the thesis reports solved [M86
pp. 88, 145-147] are solved on every seed tried, and the disagreements with her verdicts are
recorded as they are, held at their measured rate so that a change is noticed, never
asserted away."""

import io
import json
import subprocess
import sys
from pathlib import Path

import pytest

from seekwhence import bench
from seekwhence import sequences as sq
from seekwhence.config import Config
from seekwhence.forms import parse
from seekwhence.hypothesis import Hypothesis
from seekwhence.session import Console, Session

HERE = Path(__file__).resolve().parent
PYTHON_DIR = HERE.parent
EXPECTATIONS = json.loads((HERE / "data" / "expectations.json").read_text())


# ---- the catalogue [M86 pp. 6-7, 38, 88, 145-147, 150; FCCA pp. 53-54] ---------------------------

def test_the_dozen_as_printed():
    """The twelve sequences and their reported results are those transcribed from pp. 6-7
    and 145-147 in item 04 [M86 pp. 6-7, 145-147]."""
    for d in EXPECTATIONS["blackburn_dozen"]:
        seq = sq.find(f"blackburn-{d['number']}")
        assert seq.prefix(len(d["sequence"])) == tuple(d["sequence"])
        assert seq.shown == len(d["sequence"])
        assert seq.reported == d["result"]
        assert seq.group == "blackburn"
    assert [s.name for s in sq.BLACKBURN] == [f"blackburn-{k}" for k in range(1, 13)]


def test_the_dozen_verdicts_summarised():
    """"the current Seek-Whence program can solve only three of the Blackburn dozen --
    problems 2, 8, and 12" [M86 p. 147]."""
    solved = [int(s.name.split("-")[1]) for s in sq.BLACKBURN if s.reported == "solved"]
    assert solved == [2, 8, 12] == EXPECTATIONS["blackburn_summary"]["solved"]
    assert sq.find("blackburn-3").reported == "not attempted"


def test_the_p88_list_as_printed():
    """The nine sequences of p. 88 in order, with the terms printed there [M86 p. 88]."""
    printed = [d["sequence"] for d in EXPECTATIONS["solved_sequences_p88"]]
    assert [list(s.prefix(s.shown)) for s in sq.P88] == printed
    assert all(s.reported == "solved" for s in sq.P88)


def test_the_continuations_follow_the_parsed_forms():
    """The rules continue the parsed forms of p. 7 beyond the printed terms [M86 p. 7]:
    the next template of each is what the parse says."""
    assert sq.find("blackburn-1").prefix(21) == (1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5,
                                                 1, 2, 3, 4, 5, 6)
    assert sq.find("blackburn-3").prefix(18) == (2, 1, 2, 2, 2, 2, 2, 3, 2, 2, 4, 2, 2, 5, 2,
                                                 2, 6, 2)
    assert sq.find("blackburn-7").prefix(25)[18:] == (2, 3, 5, 5, 5, 5, 5)
    assert sq.find("blackburn-9").prefix(16)[12:] == (7, 7, 7, 7)
    assert sq.find("blackburn-11").prefix(34)[24:] == (5, 4, 3, 2, 1, 8, 1, 2, 3, 4)
    assert sq.find("p146-2").prefix(14) == (1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4)
    assert sq.find("p38-7").prefix(18) == (1, 2, 8, 3, 4, 5, 8, 6, 7, 8, 9, 8, 10, 11, 12, 13,
                                           14, 8)


def test_the_fcca_sampler_as_printed():
    """Hofstadter's sampler, including the terms FCCA prints beyond the ones SPEC 3.4 lists
    [FCCA pp. 53-54]."""
    assert sq.find("fcca-3").prefix(28) == (1, 0, 0, 1, 5, 1, 0, 0, 0, 5, 1, 1, 1, 1, 5, 0, 0, 0,
                                            0, 5, 0, 1, 1, 1, 5, 1, 1, 1)
    assert sq.find("fcca-4").prefix(30) == (1, 2, 0, 2, 0, 2, 1, 3, 1, 3, 1, 2, 0, 2, 0, 2, 0, 3,
                                            0, 3, 1, 2, 1, 2, 1, 2, 1, 3, 1, 3)
    assert sq.find("fcca-10").prefix(27) == (0, 0, 0, 1, 1, 2, 4, 5, 5, 6, 6, 6, 7, 7, 8, 10,
                                             11, 11, 12, 12, 12, 13, 13, 14, 16, 17, 17)
    assert sq.find("fcca-14").prefix(30) == (1, 2, 2, 3, 3, 1, 1, 2, 3, 3, 1, 1, 2, 2, 3, 1, 2,
                                             2, 3, 3, 1, 1, 2, 3, 3, 1, 1, 2, 2, 3)
    assert sq.find("fcca-13").prefix(24) == (1, 1, 2, 3, 1, 2, 2, 3, 1, 2, 3, 3, 1, 2, 2, 3, 1,
                                             1, 2, 3, 1, 2, 2, 3)


def test_the_finite_pattern_ends_at_zero():
    """`16 15 14 15 14 13 ...` "as well as possible, given a non-infinite pattern"
    [M86 p. 88]: the continuation sinks to 0 and stops, the domain having no negative
    integers [M86 p. 38]."""
    terms = sq.find("p88-8").terms
    assert len(terms) == sq.LENGTH
    whole = sq.continuation(sq.triples_down(16), 1000)
    assert whole[-3:] == (2, 1, 0) and len(whole) == 45


def test_lookups():
    """`find`, `of_groups`, the groups and the names (DECISION D-070)."""
    assert len(sq.CATALOGUE) == 45
    assert [s.group for s in sq.of_groups(["blackburn", "p88"])].count("p88") == 9
    assert sq.of_groups(["fcca"])[0].name == "fcca-1"
    with pytest.raises(KeyError):
        sq.find("blackburn-13")
    with pytest.raises(KeyError):
        sq.of_groups(["nowhere"])
    assert len({s.name for s in sq.CATALOGUE}) == len(sq.CATALOGUE)
    assert sq.find("blackburn-5").printed == "1 8 5 8 1 8 5 8 1 8 5 8"


# ---- the judge against the continuation (DECISION D-070) --------------------------------------------

def test_predicts_requires_the_whole_continuation():
    """A hypothesis is right when its own terms agree with the continuation over its whole
    known length (DECISION D-070), not merely as far as the guess block goes (D-033)."""
    seq = sq.find("blackburn-9")  # 1 2 3 3 4 4 ...
    assert not bench.predicts(Hypothesis(parse("(Countup 1)")), seq.terms)
    assert bench.predicts(Hypothesis(parse("(Countup 1)")), sq.find("blackburn-2").terms)
    assert bench.predicts(Hypothesis(parse("(S-group (Countup 1) 2)")), sq.find("blackburn-8").terms)
    assert bench.predicts(Hypothesis(parse("(Cycle 1 4 (1 8 5 8))")), sq.find("blackburn-5").terms)
    assert not bench.predicts(Hypothesis(parse("(Cycle 1 4 (1 8 1 2))")), sq.find("blackburn-11").terms)
    assert not bench.predicts(None, seq.terms)
    assert not bench.predicts(Hypothesis(parse("(Countup 1)")), ())


def test_predicts_is_false_when_the_box_cannot_give_the_terms():
    """A P-group that would run below 0 raises on hit (DECISION D-006); the judge counts
    it as wrong rather than failing (DECISION D-070)."""
    assert not bench.predicts(Hypothesis(parse("(P-group 2 4)")), (2, 1, 0, 0))


def test_the_continuation_answerer():
    """The bench's answerer judges by the continuation, writes the answer after the
    prompt, and records whether the printed block alone would have passed (DECISIONS
    D-033, D-064, D-070)."""
    out = io.StringIO()
    answerer = bench.ContinuationAnswerer(Console(out, lines=[]), sq.find("blackburn-9").terms)

    class World:
        hypothesis = Hypothesis(parse("(Countup 1)"))

    answer = answerer.answer(World(), "(Countup 1)", "1 2 3", (1, 2, 3))
    assert answer == "no"
    assert answerer.given == [{"form": "(Countup 1)", "answer": "no", "block_right": True}]
    assert out.getvalue() == "\nenter no if wrong, ok if right no\n"
    World.hypothesis = Hypothesis(parse("(Countup 1)"))
    right = bench.ContinuationAnswerer(Console(io.StringIO(), lines=[]), sq.find("blackburn-2").terms)
    assert right.answer(World(), "(Countup 1)", "1 2 3", (1, 2, 3)) == "ok"


def test_session_takes_an_answerer():
    """`Session(answerer=...)` replaces the console's answerer (DECISIONS D-064, D-069,
    D-070): the knowing user refuses `(Countup 1)` on `1 2 3 3 ...` where the judgement of
    the printed block would accept it."""
    seq = sq.find("blackburn-9")
    made = []

    def make(console):
        made.append(bench.ContinuationAnswerer(console, seq.terms))
        return made[-1]

    out = io.StringIO()
    session = Session(Config(verbose=False), 0, out=out, lines=list(seq.prefix(3)), answerer=make)
    session.run()
    assert session.answerer is made[0]
    assert made[0].given[0]["form"] == "(Countup 1)" and made[0].given[0]["answer"] == "no"
    assert made[0].given[0]["block_right"]
    assert "enter no if wrong, ok if right no" in out.getvalue()
    plain = Session(Config(verbose=False), 0, out=io.StringIO(), lines=list(seq.prefix(3)))
    plain.run()
    assert plain.outcome == "ok"  # D-033 alone accepts the block `1 2 3`


# ---- one run and one survey (DECISION D-070) ------------------------------------------------------

def test_run_sequence_blackburn_2():
    """Blackburn 2, "solved ... quite readily" [M86 p. 145]: `(Countup 1)` at the third
    term on seed 0, the record's fields, and the same seed the same run."""
    r = bench.run_sequence(sq.find("blackburn-2"), 0, 7)
    assert r["solved"] and r["outcome"] == "ok" and r["terms"] == 3
    assert r["hypothesis"] == "(Countup 1)" and r["hypotheses"] == ["(Countup 1)"]
    assert r["guesses"] == [{"form": "(Countup 1)", "answer": "ok", "block_right": True}]
    assert r["refutations"] == 0 and r["blasts"] == 0 and r["tasks"] > 0
    again = bench.run_sequence(sq.find("blackburn-2"), 0, 7)
    assert again["tasks"] == r["tasks"] and again["session"].summary() == r["session"].summary()


def test_run_sequence_feeds_at_most_n_terms():
    """The terms fed are capped (DECISION D-070): an unsolved sequence ends with
    `no-terms` after exactly the terms allowed."""
    r = bench.run_sequence(sq.find("blackburn-9"), 0, 6)
    assert r["outcome"] == "no-terms" and r["terms"] == 6 and not r["solved"]


def test_survey_sequence_fields():
    """The survey's counts and spreads (DECISION D-070)."""
    r = bench.survey_sequence(sq.find("blackburn-2"), range(3), 7)
    assert r["solved"] == 3 and r["terms"] == {"min": 3, "median": 3, "max": 3}
    assert r["accepted"] == {"(Countup 1)": 3} and r["refused"] == {} and r["unsolved_final"] == {}
    assert r["outcomes"] == {"ok": 3} and r["block_only"] == 0
    assert len(r["per_seed"]) == 3 and "session" not in r["per_seed"][0]
    assert bench._spread([]) is None and bench._spread([5, 3, 9]) == {"min": 3, "median": 5, "max": 9}


# ---- the fast sample: what the full survey found stable (RESULTS.md; DECISION D-070) ----------------

SEEDS = range(5)


def solved(name: str, n_terms: int, seeds=SEEDS) -> int:
    return bench.survey_sequence(sq.find(name), seeds, n_terms)["solved"]


def test_blackburn_2_and_8_and_12_solved():
    """The three she solved, "problems 2, 8, and 12" [M86 p. 147], are solved on every
    seed of the sample (20 of 20 seeds each in the full survey)."""
    assert solved("blackburn-2", 7) == 5
    assert solved("blackburn-8", 10) == 5
    assert solved("blackburn-12", 14) == 5


def test_the_forms_the_solutions_take():
    """Blackburn 8 as `(S-group (Countup 1) 2)` (its parse `1 2 * 2 3 * 3 4` [M86 p. 7]);
    Blackburn 12 as the Y-group over the whole template, not her "ugly" nested one
    [M86 p. 147] (18 of 20 seeds in the full survey; here every seed's accepted form is a
    Y-group that predicts the sequence)."""
    r8 = bench.survey_sequence(sq.find("blackburn-8"), SEEDS, 10)
    assert set(r8["accepted"]) == {"(S-group (Countup 1) 2)"}
    r12 = bench.survey_sequence(sq.find("blackburn-12"), SEEDS, 14)
    assert all(form.startswith("(Y-group ") for form in r12["accepted"])
    assert r12["accepted"].get("(Y-group 1 6 (1 8 5 5 8 1))", 0) >= 3


def test_the_other_solved_sequences_of_the_thesis():
    """The sequences of p. 88 and p. 146 that the sample covers are solved on every seed
    [M86 pp. 88, 146] (every seed in the full survey)."""
    assert solved("p88-1", 4) == 5          # 1 1 1
    assert solved("p88-4", 6) == 5          # 3 4 3 4
    assert solved("p88-6", 9, range(3)) == 3   # 3 7 3 3 7 3
    assert solved("p146-3", 10, range(3)) == 3  # 1 8 5 8 1 1 8 5 8 1


def test_disagreement_blackburn_1_solved_here():
    """A finding, not a bug (TASK.md): Blackburn 1 "thrashes hopelessly" on her program
    [M86 p. 145] but is solved here, through the blast and the Tuple fall-back (RESULTS.md;
    DECISIONS D-031, D-050): 20 of 20 seeds in the full survey. Held at the sample's rate
    so that a change is noticed."""
    r = bench.survey_sequence(sq.find("blackburn-1"), range(3), 16)
    assert r["solved"] == 3
    assert set(r["accepted"]) == {"(S-group 1 (Countup 1))"}
    assert r["blasts"] >= 3


def test_disagreement_blackburn_5_solved_here():
    """A finding: her program "finds the Y-group '1 8 5 8 1' and doggedly clings to it"
    [M86 p. 146]; the reconstruction reaches `(Cycle 1 4 (1 8 5 8))` on 19 of 20 seeds
    (RESULTS.md). Held at the sample's rate."""
    r = bench.survey_sequence(sq.find("blackburn-5"), range(3), 16)
    assert r["solved"] >= 2
    assert set(r["accepted"]) <= {"(Cycle 1 4 (1 8 5 8))"}


def test_disagreement_p88_8_not_solved_here():
    """A finding: `16 15 14 15 14 13 14 13 12 ...`, solved "as well as possible, given a
    non-infinite pattern" [M86 p. 88], is never solved here: no form of the eight types
    gives a start that falls by one per hit (RESULTS.md). Held at 0 so that a change is
    noticed."""
    r = bench.survey_sequence(sq.find("p88-8"), range(2), 9)
    assert r["solved"] == 0 and r["accepted"] == {}


def test_agreement_blackburn_9_unsolved_with_a_wrong_guess_the_block_would_pass():
    """Blackburn 9 is unsolved on both programs [M86 pp. 146-147]; here every seed guesses
    `(Countup 1)` after `1 2 3`, a guess the printed block passes (D-033) and the knowing
    user refuses (D-070)."""
    r = bench.survey_sequence(sq.find("blackburn-9"), range(3), 8)
    assert r["solved"] == 0 and r["refused"] == {"(Countup 1)": 3} and r["block_only"] == 3


def test_agreement_blackburn_10_grows_a_cycle_of_everything():
    """Blackburn 10 is unsolved on both programs [M86 p. 147]; here the fence class casts
    the whole sequence as one lengthening Cycle (DECISION D-037's reading), refused each
    time."""
    r = bench.survey_sequence(sq.find("blackburn-10"), range(2), 8)
    assert r["solved"] == 0
    assert all(form.startswith("(Cycle 9 ") for form in r["refused"])


# ---- the reports and the command line (DECISION D-070) --------------------------------------------

def test_report_and_summary():
    """The text and markdown reports have a row per sequence; the summary compares with
    the thesis's verdicts (DECISION D-070)."""
    seqs = [sq.find("blackburn-2"), sq.find("blackburn-9")]
    results = bench.survey_sequences(seqs, range(2), 7)
    text = bench.report_dozen(seqs, results)
    assert "blackburn-2: 1 2 3 4 5 6 7 ...   [M86 pp. 6-7, 145]   Meredith: solved" in text
    assert "solved 2/2 seeds" in text and "solved 0/2 seeds" in text
    md = bench.report_dozen(seqs, results, markdown=True)
    assert md.count("\n| blackburn-") == 2 and "| `1 2 3 4 5 6 7` | solved | 2/2 |" in md
    summary = bench.dozen_summary(seqs, results)
    assert [row["verdict"] for row in summary["rows"]] == ["agree", "agree"]
    assert summary["counts"] == {"agree": 2}
    unsolved = bench.dozen_summary([sq.find("blackburn-1")], [{"seeds": [0], "solved": 1}])
    assert unsolved["rows"][0]["verdict"] == "we solve, she did not"
    none = bench.dozen_summary([sq.find("p38-1")], [{"seeds": [0], "solved": 0}])
    assert none["rows"][0]["verdict"] == "no verdict"


def test_command_line(tmp_path):
    """`python3 -m seekwhence.bench dozen --names ... --seeds 2 --terms 5 --json FILE` and
    `--trace SEED` (DECISION D-070)."""
    out = tmp_path / "dozen.json"
    proc = subprocess.run([sys.executable, "-m", "seekwhence.bench", "dozen", "--names",
                           "blackburn-2", "--seeds", "2", "--terms", "5", "--json", str(out)],
                          cwd=PYTHON_DIR, capture_output=True, text=True, timeout=120)
    assert proc.returncode == 0, proc.stderr
    assert "blackburn-2: 1 2 3 4 5 6 7 ..." in proc.stdout
    assert "against the thesis's verdicts: agree: 1" in proc.stdout
    data = json.loads(out.read_text())
    assert data["terms"] == 5 and data["results"][0]["solved"] == 2
    assert data["summary"]["counts"] == {"agree": 1}
    trace = subprocess.run([sys.executable, "-m", "seekwhence.bench", "dozen", "--names",
                            "blackburn-2", "--trace", "0", "--terms", "5"],
                           cwd=PYTHON_DIR, capture_output=True, text=True, timeout=120)
    assert trace.returncode == 0, trace.stderr
    assert "==== blackburn-2: 1 2 3 4 5 6 7 ... [M86 pp. 6-7, 145]; seed 0" in trace.stdout
    assert "I have a guess!" in trace.stdout and "outcome: ok after 3 terms" in trace.stdout
    bad = subprocess.run([sys.executable, "-m", "seekwhence.bench", "dozen", "--groups", "nowhere"],
                         cwd=PYTHON_DIR, capture_output=True, text=True, timeout=60)
    assert bad.returncode != 0

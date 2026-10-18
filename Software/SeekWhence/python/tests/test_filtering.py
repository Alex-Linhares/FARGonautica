"""The Hfilter, validity, the catchall's frame gloms, the guess and the user's answer
[M86 pp. 39, 81-82, 98, 166, 168-169, 171-172, 175-176] (DECISIONS D-012, D-017, D-032,
D-033, D-053, D-063, D-064)."""

import io
from collections import Counter

import pytest

from seekwhence import bonds
from seekwhence import cytoplasm as cy
from seekwhence import gnoths
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.filtering import (FRAME_GLOM, ScriptedAnswerer, accepted, dissolve_previous_frame,
                                  frame_gloms, judge,
                                  make_frame_glom)
from seekwhence.forms import parse
from seekwhence.hypothesis import Hypothesis
from seekwhence.run import NOT_BUILT, enter_term, new_world, run_until_prompt
from seekwhence.trace import Trace

FENCE_SEED = 4     # a seed on which `3 7 3` gloms the fence `(3 7)`, as runs 1 and 2 did


def run_to_prompt(world) -> str:
    """Run until the program asks for a term, or the run is done."""
    while True:
        result = run_until_prompt(world)
        if result != "term-cap":
            return result


def full_run(terms, seed, answers=(), answerer=None, verbose=True, config=None, cast=True):
    """A run from scratch, every task live, the terms entered as the user would enter them
    and the guesses answered from the script; stops when the user accepts a guess. With
    `cast` False the Gnoth-caster is a no-op, so that the state a refutation leaves can be
    looked at before a new candidate changes it."""
    recorder, trace = Recorder(), Trace(io.StringIO(), verbose)
    if answerer is None:
        answerer = ScriptedAnswerer(answers)
    world = new_world(config, seed=seed, observers=[recorder, trace], answerer=answerer)
    if not cast:
        world.handlers["Gnoth-caster"] = lambda w, *args: None
    results = []
    for term in terms:
        enter_term(world, term)
        results.append(run_to_prompt(world))
        if results[-1] == "done":
            break
    return world, recorder, trace, results


def happenings(trace) -> list:
    """The trace without the `doing task` lines, blank lines included."""
    return [line for line in trace.lines if not line.startswith("doing task")]


# ---- the user's answer (DECISIONS D-053, D-064) ------------------------------------------------

class TestAnswerer:

    def test_scripted_answers_are_given_in_order(self):
        """The scripted answers come in order; `given` keeps them (DECISION D-064)."""
        answerer = ScriptedAnswerer(["no", "ok"])
        assert answerer.answer(None, "(Cycle 3 2 (3 7))", "3 7 3 7", (3, 7, 3, 7)) == "no"
        assert answerer.answer(None, "(Cycle 3 2 (3 7))", "3 7 3 7 3", (3, 7, 3, 7, 3)) == "ok"
        assert answerer.given == ["no", "ok"]

    def test_without_a_script_the_guess_is_judged_against_the_known_sequence(self):
        """When no answer is scripted the guess is judged against the known terms "as far
        as both go" (DECISION D-033): `3 7 3 7` is right for `3 7 3 7 3 7` [M86 p. 166]
        and wrong for `3 7 3 3 7 3` [M86 p. 168]."""
        right = ScriptedAnswerer(terms=(3, 7, 3, 7, 3, 7))
        wrong = ScriptedAnswerer(terms=(3, 7, 3, 3, 7, 3))
        assert right.answer(None, "", "3 7 3 7", (3, 7, 3, 7)) == "ok"
        assert wrong.answer(None, "", "3 7 3 7", (3, 7, 3, 7)) == "no"

    def test_the_script_comes_before_the_judgement_and_the_default_last(self):
        """Scripted answers first, then the judgement, then the default (DECISION D-064)."""
        answerer = ScriptedAnswerer(["no"], terms=(3, 7, 3, 7))
        assert answerer.answer(None, "", "3 7 3 7", (3, 7, 3, 7)) == "no"
        assert answerer.answer(None, "", "3 7 3 7", (3, 7, 3, 7)) == "ok"
        assert ScriptedAnswerer().answer(None, "", "", ()) == "ok"
        assert ScriptedAnswerer(default="no").answer(None, "", "", ()) == "no"

    def test_judge(self):
        """A guess agrees with the sequence as far as both go (DECISION D-033): the guess
        may be longer or shorter than the known sequence; an empty guess is wrong."""
        assert judge((3, 7, 3, 7), (3, 7, 3, 7, 3, 7))
        assert judge((3, 7, 3, 7, 3, 7, 3, 7), (3, 7, 3, 7))
        assert judge(((1, 2), (1, 2, 3)), (1, 2, 1, 2, 3, 1)) is False   # not flattened
        assert judge((1, 2, 1, 2, 3), (1, 2, 1, 2, 3, 1))
        assert not judge((1, 2, 1, 2), (1, 2, 1, 3))
        assert not judge((), (1, 2))

    def test_only_ok_accepts(self):
        """`ok` accepts, in any case and spacing; `no`, `nope` and anything else refuse
        [M86 pp. 166, 175] (DECISION D-053)."""
        assert accepted("ok") and accepted(" OK ") and accepted("Ok")
        assert not accepted("no") and not accepted("nope") and not accepted("") \
            and not accepted("okay")


# ---- the frames of a hypothesis (DECISIONS D-004, D-063) ---------------------------------------

class TestFrameBounds:

    @pytest.mark.parametrize("form, bounds", [
        ("(Cycle 3 2 (3 7))", [(1, 2), (1, 2), (3, 4), (3, 4), (5, 6)]),
        ("(Cycle 3 3 (3 7 3))", [(1, 3), (1, 3), (1, 3), (4, 6), (4, 6), (4, 6), (7, 9)]),
        ("(S-group 1 2)", [(1, 2), (1, 2), (3, 4), (3, 4), (5, 6)]),
        ("(S-group 1 (Countup 2))", [(1, 2), (1, 2), (3, 5), (3, 5), (3, 5), (6, 9), (6, 9)]),
        ("(Countup 1)", [(1, 1), (2, 2), (3, 3)]),
        ("(C-group (Countup 1) shared)", [(1, 1), (2, 3), (2, 3), (4, 6)]),
    ])
    def test_the_frame_holding_each_position(self, form, bounds):
        """A frame is one hit, or one full turn of a Cycle (DECISION D-004): the frames of
        the transcripts' hypotheses [M86 pp. 166, 169, 175, 179] and of `(C-group (Countup
        1) shared)` [M86 p. 161], position by position."""
        hypothesis = Hypothesis(parse(form))
        assert [hypothesis.frame_bounds(i + 1) for i in range(len(bounds))] == bounds

    def test_the_hypothesis_box_is_untouched_and_positions_count_from_one(self):
        """`frame_bounds` reads a fresh box (DECISION D-063)."""
        hypothesis = Hypothesis(parse("(Cycle 3 2 (3 7))"))
        hypothesis.postdict([3, 7, 3])
        hypothesis.frame_bounds(7)
        assert hypothesis.next_term() == 7
        with pytest.raises(ValueError):
            hypothesis.frame_bounds(0)


# ---- run 1: `3 7 3 7` to the guess [M86 pp. 164-166] -----------------------------------------

class TestRunOne:

    def test_the_fence_seed_prints_run_1_from_the_fourth_term_to_bye(self):
        """After the fourth term, run 1 prints `doing task Hfilter` first, `new term being
        hypothesis-filtered through (Cycle 3 2 (3 7))`, `top-down glom glom4`, a blank line,
        `I have a guess!`, `hypothesis: (Cycle 3 2 (3 7))`, `3 7 3 7`, `enter no if wrong,
        ok if right ok` and `bye` [M86 p. 166]; the run is then done, and nothing else runs
        (the Hfilter is run as soon as the term is entered, DECISION D-012)."""
        world, recorder, trace, results = full_run([3, 7, 3, 7], FENCE_SEED)
        assert results == ["empty", "empty", "call-term", "done"]
        start = len(trace.lines) - 1 - trace.lines[::-1].index("please enter a term: 7")
        after = trace.lines[start + 1:]
        assert after == [
            "doing task Hfilter",
            "new term being hypothesis-filtered through (Cycle 3 2 (3 7))",
            "top-down glom glom4",
            "",
            "I have a guess!",
            "",
            "hypothesis: (Cycle 3 2 (3 7))",
            "3 7 3 7",
            "",
            "enter no if wrong, ok if right ok",
            "bye",
        ]

    def test_the_events_of_the_guess_in_order(self):
        """Filtered, the frame glom, the guess, the answer, the end [M86 p. 166]."""
        world, recorder, trace, results = full_run([3, 7, 3, 7], FENCE_SEED)
        kinds = [e.kind for e in recorder.events]
        tail = [k for k in kinds if k in ("filtered", "top-down", "guess", "answered", "ended")]
        assert tail == ["top-down", "top-down", "filtered", "top-down", "guess", "answered",
                        "ended"]
        filtered = recorder.of_kind("filtered")[0]
        assert (filtered.n, filtered.term, filtered.form, filtered.ok, filtered.predicted) == \
            (4, 7, "(Cycle 3 2 (3 7))", True, 7)
        guess = recorder.of_kind("guess")[0]
        assert (guess.form, guess.block, guess.terms) == ("(Cycle 3 2 (3 7))", "3 7 3 7",
                                                          (3, 7, 3, 7))
        answered = recorder.of_kind("answered")[0]
        assert (answered.answer, answered.accepted) == ("ok", True)
        assert recorder.of_kind("ended")[0].reason == "ok"

    def test_the_confirmed_frame_gets_a_top_down_glom_in_the_catchall(self):
        """The fourth term completes the turn `(3 7)` at terms 3-4 (DECISION D-004); the
        Hfilter puts `glom4`, a pseudo-glom, over `glint3` and `glint4` and both glints
        are in the catchall, whose collection is then the one glom (DECISION D-063;
        "The system stores hypothesis-confirming terms in a catchall gnoth" [M86 p. 98])."""
        world, recorder, trace, results = full_run([3, 7, 3, 7], FENCE_SEED)
        glom4 = world.find("glom4")
        assert glom4.pseudo and glom4.type == ("frame",)
        assert [x.name for x in glom4.subgloms] == ["glint3", "glint4"]
        assert glom4.print_value == (3, 7) and glom4.positions_covered == (3, 4)
        catchall = gnoths.catchall(world)
        assert [g.name for g in catchall.glints] == ["glint3", "glint4"]
        assert catchall.collection == (glom4,) and catchall.parse_print == ((3, 7),)
        assert catchall.terms == (3, 7) and catchall.range == (3, 4)
        assert frame_gloms(world) == [glom4]
        assert catchall not in gnoths.gnoths(world)
        assert world.hypothesis.validity == 1 and world.done

    def test_after_ok_nothing_more_runs(self):
        """After `bye` the run is done: no Call-term is set out, the loop runs nothing, and
        a term entered afterwards sets out no Hfilter (DECISION D-053)."""
        world, recorder, trace, results = full_run([3, 7, 3, 7], FENCE_SEED)
        assert world.taskrack.count_by_type().get("Call-term", 0) == 0
        clock = world.clock
        assert run_until_prompt(world) == "done" and world.clock == clock
        enter_term(world, 3)
        assert world.taskrack.count_by_type().get("Hfilter", 0) == 0
        assert run_until_prompt(world) == "done"

    def test_the_rate_over_a_hundred_seeds(self):
        """The whole `3 7 3 7` run reaches `I have a guess!`, `hypothesis: (Cycle 3 2
        (3 7))`, `3 7 3 7` and stops on `ok` on 35 of the seeds 0-99 (measured when this
        test was written); on every other seed the cover `(3 7 3)` won the terraced scan
        (RESULTS.md), the hypothesis is `(Y-group 3 3 (3 7 3))` and the fourth term refutes
        it, which sets out the Gnoth-setter of item 15 and the reformulation of items 16-18
        (which ends in a candidate's Call-term, or in the blast and an empty rack). The
        item asked for "most seeds"; the share is the fence's share of the scan, not a
        property of the filter."""
        outcomes = Counter()
        for seed in range(100):
            world, recorder, trace, results = full_run([3, 7, 3, 7], seed)
            guesses = recorder.of_kind("guess")
            if guesses:
                assert results[-1] == "done"
                assert (guesses[0].form, guesses[0].block) == ("(Cycle 3 2 (3 7))", "3 7 3 7")
                assert "I have a guess!" in trace.lines and trace.lines[-1] == "bye"
                outcomes["guessed"] += 1
            else:
                refuted = recorder.of_kind("refuted")
                assert len(refuted) == 1 and refuted[0].form == "(Y-group 3 3 (3 7 3))"
                assert results[-1] in ("empty", "call-term")
                outcomes["refuted"] += 1
        assert outcomes["guessed"] >= 25
        assert outcomes["guessed"] + outcomes["refuted"] == 100

    def test_same_seed_same_trace(self):
        """Reproducible from the seed."""
        a = full_run([3, 7, 3, 7], FENCE_SEED)[2].lines
        b = full_run([3, 7, 3, 7], FENCE_SEED)[2].lines
        assert a == b

    def test_the_terse_run_prints_the_guess_without_task_lines(self):
        """Run 2 prints the Hfilter's line, the top-down glom and the guess with no `doing
        task` lines [M86 p. 169] (`verbose=False`, DECISION D-055)."""
        world, recorder, trace, results = full_run([3, 7, 3, 7], FENCE_SEED, verbose=False)
        assert not any(line.startswith("doing task") for line in trace.lines)
        assert trace.lines[-10:] == [
            "new term being hypothesis-filtered through (Cycle 3 2 (3 7))",
            "top-down glom glom4", "", "I have a guess!", "", "hypothesis: (Cycle 3 2 (3 7))",
            "3 7 3 7", "", "enter no if wrong, ok if right ok", "bye"]


# ---- a refused guess [M86 pp. 175-176] (DECISIONS D-032, D-053) --------------------------------

class TestRefusal:

    def test_a_refusal_resets_the_validity_and_asks_for_the_next_term(self):
        """After `nope` the program asks for the next term [M86 pp. 175-176]: the validity
        is 0, the hypothesis stays, a Call-term is run (DECISIONS D-032, D-053)."""
        world, recorder, trace, results = full_run([3, 7, 3, 7], FENCE_SEED, answers=["nope"])
        assert results == ["empty", "empty", "call-term", "call-term"]
        assert "enter no if wrong, ok if right nope" in trace.lines
        assert trace.lines[-1] == "doing task Call-term"
        assert trace.lines.index("enter no if wrong, ok if right nope") < len(trace.lines) - 1
        assert world.hypothesis.validity == 0 and not world.done
        assert world.hypothesis.show_form() == "(Cycle 3 2 (3 7))"
        assert not recorder.of_kind("ended")

    def test_the_next_confirming_term_guesses_again_with_a_longer_block(self):
        """One confirming term after the refusal makes another guess (DECISION D-032); the
        block covers every term entered, `3 7 3 7 3` for five terms (DECISION D-033)."""
        world, recorder, trace, results = full_run([3, 7, 3, 7, 3], FENCE_SEED,
                                                   answers=["no", "no"])
        guesses = recorder.of_kind("guess")
        assert [g.block for g in guesses] == ["3 7 3 7", "3 7 3 7 3"]
        assert results[-1] == "call-term" and world.hypothesis.validity == 0

    def test_a_partial_frame_gets_a_glom_that_grows_with_the_frame(self):
        """The fifth term `3` begins the turn at terms 5-6: its frame glom covers `glint5`
        alone; the sixth term `7` completes it and the glom over the prefix is replaced by
        one over `glint5 glint6` (DECISION D-063; a one-term pseudo-glom is printed on
        p. 175, `glom8 pseudo --> (1)  terms 3 to 3`)."""
        world, recorder, trace, results = full_run([3, 7, 3, 7, 3, 7], FENCE_SEED,
                                                   answers=["no", "no", "no"])
        made = [(e.name, e.members) for e in recorder.of_kind("top-down")]
        assert made[-3:] == [("glom4", ("glint3", "glint4")), ("glom5", ("glint5",)),
                             ("glom6", ("glint5", "glint6"))]
        assert world.find("glom5") is None
        assert [g.name for g in frame_gloms(world)] == ["glom4", "glom6"]
        catchall = gnoths.catchall(world)
        assert catchall.parse_print == ((3, 7), (3, 7)) and catchall.range == (3, 6)
        assert [g.block for g in recorder.of_kind("guess")] == \
            ["3 7 3 7", "3 7 3 7 3", "3 7 3 7 3 7"]


# ---- refutation [M86 pp. 98, 168, 171-172, 176] (DECISIONS D-017, D-063) ------------------------

class TestRefutation:

    def test_run_2s_fourth_term_refutes_the_cycle(self):
        """`3 7 3` then `3`: `new term being hypothesis-filtered through (Cycle 3 2 (3 7))`
        [M86 p. 168] (the terse trace adds `set-out -- validity: 0`, `test_trace.py`; the
        verbose one prints none, as run 3 [M86 pp. 172, 176]); the validity is 0, Sparklers and a
        Gnoth-setter are set out [M86 p. 98] (DECISION D-017); no catchall exists yet (no
        term confirmed the hypothesis), so the troublesome glint stays free (DECISION
        D-063); the Gnoth-setter freeze-dries the hypothesis (item 15), its Reformulator
        finds nothing under Cycle and the Bond-assessor's Gnoth-caster, turned off here,
        does nothing, so the rack runs empty (with it on, run 2's candidate follows:
        `test_casting.py`)."""
        world, recorder, trace, results = full_run([3, 7, 3, 3], FENCE_SEED, cast=False)
        assert results == ["empty", "empty", "call-term", "empty"]
        start = trace.lines.index("please enter a term: 3", 10)
        lines = [l for l in trace.lines[start:] if "filtered" in l or l.startswith("set-out")]
        assert lines == ["new term being hypothesis-filtered through (Cycle 3 2 (3 7))"]
        refuted = recorder.of_kind("refuted")[0]
        assert (refuted.form, refuted.n, refuted.term, refuted.predicted) == \
            ("(Cycle 3 2 (3 7))", 4, 3, 7)
        assert not recorder.of_kind("catchall")
        assert frame_gloms(world) == []
        posted = [e for e in recorder.of_kind("posted")]
        refuted_at = [e for e in recorder.events].index(refuted)
        after = [e.type for e in recorder.events[refuted_at:] if e.kind == "posted"]
        assert after[:4] == ["Sparkler", "Sparkler", "Sparkler", "Gnoth-setter"]
        assert world.hypothesis is None and world.freeze_dried[0].form == parse("(Cycle 3 2 (3 7))")
        assert "Gnoth-caster" not in NOT_BUILT
        assert [e.type for e in recorder.of_kind("chosen")].count("Gnoth-caster") == 1
        assert not recorder.of_kind("guess")
        assert posted     # the rack had tasks

    def test_the_sparklers_knob(self):
        """`Config.sparklers_on_refutation` sets how many Sparklers a refutation releases
        (DECISION D-017)."""
        config = Config(sparklers_on_refutation=5)
        world, recorder, trace, results = full_run([3, 7, 3, 3], FENCE_SEED, config=config)
        refuted = recorder.of_kind("refuted")[0]
        at = recorder.events.index(refuted)
        after = [e.type for e in recorder.events[at:] if e.kind == "posted"]
        assert after[:6] == ["Sparkler"] * 5 + ["Gnoth-setter"]

    @pytest.mark.parametrize("seed", range(10))
    def test_run_3s_third_term_refutes_countup(self, seed):
        """`1 2` makes `(Countup 1)` and the third term `1` is filtered through it and
        refutes it [M86 pp. 171-172], for every seed; the reformulation then makes run 3's
        second hypothesis and asks for the next term [M86 p. 174] (item 18)."""
        world, recorder, trace, results = full_run([1, 2, 1], seed)
        assert "new term being hypothesis-filtered through (Countup 1)" in trace.lines
        assert recorder.of_kind("refuted")[0].form == "(Countup 1)"
        assert [str(f) for f in world.freeze_dried] == ["(Countup 1) (2 terms)"]
        assert world.hypothesis.show_form() == "(S-group 1 2)" and results[-1] == "call-term"

    def test_a_refutation_after_confirmations_dissolves_the_frame_gloms(self):
        """`3 7 3 7 3 7` confirmed (guesses refused) and then `5`: the frame gloms `glom4`
        and `glom6` are dissolved before the Gnoth-setter runs (as `glom10 is not in
        cytoplasm` on p. 176; DECISION D-063); the catchall, which held every confirmed
        glint and the troublesome one [M86 p. 98], is then split by the Gnoth-setter into
        gnoths over the three turns and a final gnoth over the `5` and destroyed
        [M86 p. 98] (item 15). The Gnoth-caster is turned off so that the Gnoth-setter's
        state can be looked at."""
        world, recorder, trace, results = full_run([3, 7, 3, 7, 3, 7, 5], FENCE_SEED,
                                                   answers=["no", "no", "no"], cast=False)
        assert results[-1] == "empty"
        assert world.find("glom4") is None and world.find("glom6") is None
        assert frame_gloms(world) == []
        assert gnoths.catchall(world) is None
        assert [g.range for g in gnoths.gnoths(world)] == [(1, 2), (3, 4), (5, 6), (7, 7)]
        assert [g.terms for g in gnoths.gnoths(world)] == [(3, 7), (3, 7), (3, 7), (5,)]
        assert len(recorder.of_kind("refuted")) == 1

    def test_after_a_refutation_the_hfilter_waits_for_reformulation(self):
        """A term entered while the hypothesis awaits reformulation sets out an Hfilter that
        does nothing (DECISION D-009). The Gnoth-setter is held back here (a no-op handler)
        so that the hypothesis is still there, refuted."""
        recorder = Recorder()
        world = new_world(seed=FENCE_SEED, observers=[recorder])
        world.handlers["Gnoth-setter"] = lambda w, *args: None
        for term in (3, 7, 3, 3):
            enter_term(world, term)
            run_to_prompt(world)
        assert world.hypothesis.refuted
        enter_term(world, 7)
        assert run_to_prompt(world) == "empty"
        stale = [(e.type, e.reason) for e in recorder.of_kind("stale")]
        assert ("Hfilter", "the hypothesis awaits reformulation") in stale
        assert len(recorder.of_kind("filtered")) == 1


# ---- the task's stale cases and more than one term pending (DECISIONS D-009, D-063) -----------

class TestTask:

    def test_an_hfilter_without_a_hypothesis_does_nothing(self):
        """No hypothesis, nothing to filter (DECISION D-009)."""
        recorder = Recorder()
        world = new_world(seed=1, observers=[recorder])
        enter_term(world, 3)
        world.post("Hfilter")
        run_to_prompt(world)
        assert ("Hfilter", "no hypothesis") in [(e.type, e.reason)
                                                for e in recorder.of_kind("stale")]

    def test_a_second_hfilter_finds_nothing_to_filter(self):
        """Once the term is filtered, another Hfilter reports itself stale
        (DECISION D-009)."""
        world, recorder, trace, results = full_run([3, 7, 3, 7], FENCE_SEED, answers=["no"])
        world.post("Hfilter")
        run_to_prompt(world)
        assert ("Hfilter", "nothing to filter") in [(e.type, e.reason)
                                                    for e in recorder.of_kind("stale")]
        assert len(recorder.of_kind("filtered")) == 1

    def test_two_terms_pending_are_filtered_in_order(self):
        """When two terms are pending before an Hfilter runs, one Hfilter filters both in
        order and the other finds nothing (DECISION D-063); both guesses cover the five
        terms entered (DECISION D-033). Since the Hfilter of an entered term runs at once
        (DECISION D-012), the pending state is made by entering the terms through the
        cytoplasm alone and setting out two Hfilters by hand."""
        world, recorder, trace, results = full_run([3, 7, 3], FENCE_SEED)
        world.answerer = ScriptedAnswerer(["no", "no"])
        bonds.new_term(world, 7)
        bonds.new_term(world, 3)
        world.post("Hfilter")
        world.post("Hfilter")
        assert world.taskrack.count_by_type()["Hfilter"] == 2
        run_to_prompt(world)
        filtered = recorder.of_kind("filtered")
        assert [(e.n, e.term, e.ok) for e in filtered] == [(4, 7, True), (5, 3, True)]
        assert [g.block for g in recorder.of_kind("guess")] == ["3 7 3 7 3", "3 7 3 7 3"]
        for _ in range(3):      # the two refusals each posted a Call-term
            if world.taskrack.count_by_type().get("Hfilter", 0) == 0:
                break
            run_to_prompt(world)
        assert ("Hfilter", "nothing to filter") in [(e.type, e.reason)
                                                    for e in recorder.of_kind("stale")]

    def test_the_frame_glom_covers_only_what_is_free(self):
        """A frame whose first terms sit under a glom that straddles its start gets a frame
        glom over the rest; a glom lying inside the frame is taken as a member; when the
        frame's last term is not free there is no glom (DECISION D-063)."""
        recorder = Recorder()
        world = new_world(seed=1, observers=[recorder])
        for term in (3, 7, 3, 7, 3, 7):
            world.enter_term(term)
            cy.make_glint(world, term)
        glints = cy.glints(world)
        cy.make_glom(world, ("Same", "print-value", "fence"), glints[1:3])   # terms 2-3
        glom = make_frame_glom(world, 3, 4)
        assert [x.name for x in glom.subgloms] == ["glint4"]
        assert make_frame_glom(world, 1, 2) is None          # term 2 is not free
        inner = cy.make_glom(world, ("Same", "print-value", "fence"), glints[4:6])
        glom = make_frame_glom(world, 5, 6)
        assert glom.subgloms == (inner,)
        assert make_frame_glom(world, 7, 7) is None


# ---- what a refutation dissolves [M86 pp. 172-173, 176, 178] (DECISION D-063, item 21) ---------

class TestPreviousFrame:

    def test_run_3s_first_refutation_frees_glom1(self):
        """`1 2` then `1`: before the Gnoth-setter runs, the gnoth-glom glom3 over `(1 2)` is
        gone and the bottom-up glom1 is free again, which is what the printout shows
        (`glom3 is not in cytoplasm`, `Sparkler-plus on (glom1 glint3 10)`, `bond4 ---
        between glom1glint3` [M86 pp. 172-173]): the hypothesis `(Countup 1)` has one-term
        frames, and the frame before the failing one holds glint2."""
        recorder, trace = Recorder(), Trace(io.StringIO())
        world = new_world(seed=3, observers=[recorder, trace])
        for term in (1, 2):
            enter_term(world, term)
            run_until_prompt(world)
        assert world.hypothesis.show_form() == "(Countup 1)"
        glom1, glom3 = world.find("glom1"), world.find("glom3")
        assert glom1.parent is glom3 and glom3.pseudo
        enter_term(world, 1)                      # the Hfilter runs at once (D-012)
        assert world.hypothesis is None or world.hypothesis.refuted
        assert world.find("glom3") is None and world.find("glom1") is glom1
        assert glom1.parent is None and glom1.dubbed
        assert posted_dissolver(recorder) == ("glom3",)
        run_until_prompt(world)
        assert "glom3 is not in cytoplasm" in trace.lines

    def test_p176s_refutation_dissolves_glom8_and_glom10_and_keeps_glom9(self):
        """The state of p. 175 built by hand: gnoth2 capped over `(1 2)` (her glom9), gnoth3
        capped over the `1` of term 3 (glom8), the frame glom of term 4 over the `2` (glom10),
        and the hypothesis `(S-group 1 2)` after four terms; the `3` of term 5 refutes it:
        the caps over the previous frame, terms 3-4, go (`glom8 is not in cytoplasm`,
        `glom10 is not in cytoplasm` [M86 p. 176]) and the cap over terms 1-2 is still
        listed after the next Gnoth-setter [M86 p. 178]."""
        recorder = Recorder()
        world = new_world(seed=0, observers=[recorder])
        for term in (1, 2, 1, 2):
            bonds.new_term(world, term)
        world.taskrack.clean(tuple(world.urgencies))
        glom9 = gnoths.make_gnoth(world, "S-group", [world.find("glint1"), world.find("glint2")], 1).glom
        glom8 = gnoths.make_gnoth(world, "S-group", [world.find("glint3")], 2).glom
        glom10 = cy.make_glom(world, FRAME_GLOM, [world.find("glint4")], pseudo=True)
        world.hypothesis = Hypothesis(parse("(S-group 1 2)"))
        assert world.hypothesis.postdict(world.terms)
        world.reigning = "S-group"
        enter_term(world, 3)
        assert world.find(glom8.name) is None and world.find(glom10.name) is None
        assert world.find(glom9.name) is glom9
        assert all(world.find(f"glint{i}").parent is None for i in (3, 4, 5))
        assert world.find("glint1").parent is glom9

    def test_nothing_before_the_first_frame(self):
        """A hypothesis refuted inside its first frame has no previous frame: nothing but
        the frame gloms is dissolved."""
        recorder = Recorder()
        world = new_world(seed=0, observers=[recorder])
        bonds.new_term(world, 3)
        world.taskrack.clean(tuple(world.urgencies))
        cap = gnoths.make_gnoth(world, "Cycle", [world.find("glint1")], 0).glom
        world.hypothesis = Hypothesis(parse("(Cycle 3 2 (3 7))"))
        assert world.hypothesis.postdict(world.terms)
        assert dissolve_previous_frame(world, world.hypothesis, 2) == []
        assert world.find(cap.name) is cap


def posted_dissolver(recorder) -> tuple:
    return [e.args for e in recorder.of_kind("posted") if e.type == "Dissolver"][-1]

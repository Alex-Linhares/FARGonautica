"""The Gnoth-caster, the candidate, the Slip-scout and the blast [M86 pp. 39, 91-92,
124-126, 168-169, 174, 178] (DECISIONS D-034, D-035, D-037, D-048, D-049, D-050)."""

import io
from collections import Counter

import pytest

from seekwhence import bonds, casting, cytoplasm as cy, gnoths, slipping
from seekwhence.casting import (Casting, adopt_candidate, blast, cast_gnoths, cast_terms, fail,
                                generalise_casts, gnoth_caster, promise_frame,
                                repeats_freeze_dried, slip_scout)
from seekwhence.config import TASK_TYPES, Config
from seekwhence.events import Recorder
from seekwhence.forms import Form, form_text, parse
from seekwhence.hypothesis import FreezeDried
from seekwhence.run import NOT_BUILT, TASKS, enter_term, new_world, run_until_prompt
from seekwhence.trace import Trace
from test_filtering import FENCE_SEED
from test_filtering import full_run as answered_run
from test_reforms import post_and_run
from test_slipping import bond, gnoth_over, page_176, still_world


# ---- helpers -------------------------------------------------------------------------------

def run_to_prompt(world) -> str:
    while True:
        result = run_until_prompt(world)
        if result != "term-cap":
            return result


def full_run(terms, seed, config=None, verbose=True):
    recorder, trace = Recorder(), Trace(io.StringIO(), verbose)
    world = new_world(config, seed=seed, observers=[recorder, trace])
    results = []
    for term in terms:
        enter_term(world, term)
        results.append(run_to_prompt(world))
        if results[-1] == "done":
            break
    return world, recorder, trace, results


def parse_of(world, kind, *groups):
    """Gnoths over runs of glints, `parse_of(w, "S-group", (1, 2), (3,))` for gnoths over
    glint1-glint2 and glint3, frames from 0."""
    made = []
    for frame, group in enumerate(groups):
        made.append(gnoth_over(world, kind, frame, *(f"glint{i}" for i in group)))
    world.reigning = kind
    return made


def texts(casts) -> list:
    return [form_text(c) if not isinstance(c, int) else str(c) for c in casts]


def lines_from(trace, prefix):
    start = max(i for i, l in enumerate(trace.lines) if l.startswith(prefix))
    return trace.lines[start:]


# ---- casting one gnoth [M86 p. 124] (DECISIONS D-048, D-037) ------------------------------

class TestCastTerms:

    def test_group_classes_cast_a_run_that_passes_the_process(self):
        """`1 2` under S-group is `(S-group 1 2)` and `1` is `(S-group 1 1)` [M86 p. 174];
        `1 2 3` is `(S-group 1 3)` [M86 p. 178] (DECISION D-048)."""
        assert texts(cast_terms("S-group", (1, 2))) == ["(S-group 1 2)"]
        assert texts(cast_terms("S-group", (1,))) == ["(S-group 1 1)"]
        assert texts(cast_terms("S-group", (1, 2, 3))) == ["(S-group 1 3)"]
        assert texts(cast_terms("C-group", (4, 4, 4))) == ["(C-group 4 3)"]
        assert texts(cast_terms("P-group", (3, 2, 1))) == ["(P-group 3 3)"]

    def test_a_run_that_fails_the_process_cannot_be_cast(self):
        """"If the Gnoth-caster is unable to cast all the gnoths in terms of the reigning
        class" [M86 p. 126]: `3 7 3` is no S-group, C-group or P-group."""
        for kind in ("S-group", "C-group", "P-group"):
            assert cast_terms(kind, (3, 7, 3)) == []
        assert cast_terms("S-group", (2, 1)) == [] and cast_terms("P-group", (1, 2)) == []

    def test_y_group_casts_a_symmetric_run_with_its_elements(self):
        """A Y-group's form carries its elements [M86 p. 52]; a one-term run is trivially
        symmetric (DECISION D-048)."""
        assert texts(cast_terms("Y-group", (3, 7, 3))) == ["(Y-group 3 3 (3 7 3))"]
        assert texts(cast_terms("Y-group", (7,))) == ["(Y-group 7 1 (7))"]
        assert cast_terms("Y-group", (1, 2)) == []

    def test_repeater_classes_cast_a_single_term(self):
        assert cast_terms("Constant", (3,)) == [3] and cast_terms("Countup", (5,)) == [5]
        assert cast_terms("Constant", (3, 3)) == [] and cast_terms("Countup", (1, 2)) == []

    def test_fence_classes_cast_term_by_term(self):
        """Under Cycle every term is a one-element Cycle, `(Cycle 3 1 (3))` [M86 p. 168]
        (DECISION D-037)."""
        assert texts(cast_terms("Cycle", (3, 7))) == ["(Cycle 3 1 (3))", "(Cycle 7 1 (7))"]
        assert texts(cast_terms("Tuple", (5,))) == ["(Tuple 5 1 (5))"]

    def test_no_terms_no_cast_and_an_unknown_class_is_an_error(self):
        assert cast_terms("S-group", ()) == []
        with pytest.raises(ValueError):
            cast_terms("Z-group", (1,))


# ---- generalising the casts [M86 p. 125] (DECISIONS D-048, D-037) -------------------------

class TestGeneralise:

    def test_run_3s_first_candidate(self):
        """`((S-group 1 2) (S-group 1 1))` gives `(S-group 1 2)` [M86 p. 174]: the casts
        themselves do not generalise (lengths 2, 1), so the head `(S-group 1 2)` stands and
        the last gnoth `(1)` "shows promise" [M86 p. 125], beginning the frame `(1 2)`."""
        casts = [parse("(S-group 1 2)"), parse("(S-group 1 1)")]
        assert generalise_casts("S-group", casts) is None
        assert form_text(generalise_casts("S-group", casts[:1])) == "(S-group 1 2)"
        assert promise_frame(parse("(S-group 1 2)"), 1) == (1, 2)

    def test_run_3s_second_candidate(self):
        """`((S-group 1 2) (S-group 1 3))` gives `(S-group 1 (Countup 2))` [M86 pp. 125,
        178]: the start 1 stays, the lengths 2, 3 count up (DECISION D-048)."""
        casts = [parse("(S-group 1 2)"), parse("(S-group 1 3)")]
        assert form_text(generalise_casts("S-group", casts)) == "(S-group 1 (Countup 2))"

    def test_run_2s_cycle(self):
        """`((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))` concatenate into
        `(Cycle 3 3 (3 7 3))` [M86 p. 168] (DECISION D-037)."""
        casts = [parse(t) for t in ("(Cycle 3 1 (3))", "(Cycle 7 1 (7))", "(Cycle 3 1 (3))")]
        assert form_text(generalise_casts("Cycle", casts)) == "(Cycle 3 3 (3 7 3))"

    def test_the_rules_the_four_examples_show(self):
        """Equal parameters stay, parameters counting up by one become a Countup, anything
        else fails (DECISION D-048): a start counting up, both parameters counting up, and
        starts 1, 3 that neither repeat nor count."""
        g = generalise_casts
        assert form_text(g("S-group", [parse("(S-group 1 2)"), parse("(S-group 2 2)")])) == \
            "(S-group (Countup 1) 2)"
        assert form_text(g("C-group", [parse("(C-group 1 1)"), parse("(C-group 2 2)")])) == \
            "(C-group (Countup 1) (Countup 1))"
        assert g("S-group", [parse("(S-group 1 2)"), parse("(S-group 3 2)")]) is None
        assert g("S-group", [parse("(S-group 1 2)"), None]) is None
        assert g("S-group", []) is None

    def test_y_group_elements_position_by_position(self):
        """`(Y-group 1 3 (1 2 1))` and `(Y-group 1 3 (1 3 1))` give `(Y-group 1 3 (1
        (Countup 2) 1))`; casts of different lengths do not generalise (DECISION D-048)."""
        a, b = parse("(Y-group 1 3 (1 2 1))"), parse("(Y-group 1 3 (1 3 1))")
        assert form_text(generalise_casts("Y-group", [a, b])) == "(Y-group 1 3 (1 (Countup 2) 1))"
        assert form_text(generalise_casts("Y-group", [a, a])) == "(Y-group 1 3 (1 2 1))"
        assert generalise_casts("Y-group", [a, parse("(Y-group 1 5 (1 2 3 2 1))")]) is None

    def test_repeater_casts(self):
        assert form_text(generalise_casts("Constant", [3, 3, 3])) == "(Constant 3)"
        assert form_text(generalise_casts("Countup", [1, 2, 3])) == "(Countup 1)"
        assert generalise_casts("Constant", [1, 3]) is None
        assert generalise_casts("Constant", [parse("(Constant 1)")]) is None

    def test_casts_of_another_class_do_not_generalise(self):
        assert generalise_casts("S-group", [parse("(C-group 1 2)")]) is None

    def test_promise_frame(self):
        """The k-th frame of a form, flattened, for the "shows promise" test [M86 p. 125]:
        `(S-group 3 2)` gives `(3 4)` every frame; `(S-group 1 (Countup 2))` gives `(1 2)`,
        `(1 2 3)`, `(1 2 3 4)` [M86 p. 56]; a Cycle's frame is one turn (DECISION D-004)."""
        assert promise_frame(parse("(S-group 3 2)"), 2) == (3, 4)
        assert promise_frame(parse("(S-group 1 (Countup 2))"), 2) == (1, 2, 3, 4)
        assert promise_frame(parse("(Cycle 3 2 (3 7))"), 0) == (3, 7)
        assert promise_frame(parse("(P-group 2 4)"), 0) == ()


# ---- casting the parse [M86 pp. 124-125] -----------------------------------------------------

class TestCastGnoths:

    def test_p125_three_four_three_four_three(self):
        """"if the term groupings generated by the gnoths are: (3 4) (3 4) (3), then the
        form '(S-group 3 2)' would be generated" [M86 p. 125]: the last gnoth is left out
        of the generalisation as the one that shows promise, its cast stays in the list."""
        w, _, _ = still_world(3, 4, 3, 4, 3)
        parse_of(w, "S-group", (1, 2), (3, 4), (5,))
        c = cast_gnoths("S-group", gnoths.gnoths(w))
        assert form_text(c.form) == "(S-group 3 2)" and c.last is True
        assert c.casts_text() == "((S-group 3 2) (S-group 3 2) (S-group 3 1))"
        assert c.reason is None

    def test_p125_one_two_one_two_three(self):
        """"given (1 2) (1 2 3), the form '(S-group 1 (Countup 2))' is generated"
        [M86 p. 125]: every gnoth cast, the casts generalised [M86 p. 178]."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        parse_of(w, "S-group", (1, 2), (3, 4, 5))
        c = cast_gnoths("S-group", gnoths.gnoths(w))
        assert form_text(c.form) == "(S-group 1 (Countup 2))" and c.last is False
        assert c.casts_text() == "((S-group 1 2) (S-group 1 3))"

    def test_p174_one_two_one(self):
        """`((1 2) (1))` gives `casts: ((S-group 1 2) (S-group 1 1))` and `(S-group 1 2)`
        [M86 p. 174]."""
        w, _, _ = still_world(1, 2, 1)
        parse_of(w, "S-group", (1, 2), (3,))
        c = cast_gnoths("S-group", gnoths.gnoths(w))
        assert (c.casts_text(), form_text(c.form), c.last) == \
            ("((S-group 1 2) (S-group 1 1))", "(S-group 1 2)", True)

    def test_run_2s_gnoths_under_cycle(self):
        """`(3 7) (3) (3)` under Cycle: the first two gnoths' terms are cast one by one and
        the last, whose `3` begins `3 7 3`, is left uncast, so three casts are printed and
        the form is `(Cycle 3 3 (3 7 3))` [M86 p. 168] (DECISION D-037)."""
        w, _, _ = still_world(3, 7, 3, 3)
        parse_of(w, "Cycle", (1, 2), (3,), (4,))
        c = cast_gnoths("Cycle", gnoths.gnoths(w))
        assert c.casts_text() == "((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))"
        assert form_text(c.form) == "(Cycle 3 3 (3 7 3))" and c.last is True

    def test_a_cycle_whose_last_gnoth_shows_no_promise_takes_it_in(self):
        """The consequence of DECISION D-037 recorded there: a last gnoth that does not begin
        the concatenation is cast too, so the Cycle covers every term, `(Cycle 3 7 (3 7 3 7
        3 7 5))` for `(3 7) (3 7) (3 7) (5)`, and postdicts trivially; a last gnoth equal to
        the whole head is promising, `(3 7) (3 7)` giving `(Cycle 3 2 (3 7))`."""
        w, _, _ = still_world(3, 7, 3, 7, 3, 7, 5)
        parse_of(w, "Cycle", (1, 2), (3, 4), (5, 6), (7,))
        c = cast_gnoths("Cycle", gnoths.gnoths(w))
        assert form_text(c.form) == "(Cycle 3 7 (3 7 3 7 3 7 5))" and c.last is False
        w, _, _ = still_world(3, 7, 3, 7)
        parse_of(w, "Cycle", (1, 2), (3, 4))
        c = cast_gnoths("Cycle", gnoths.gnoths(w))
        assert form_text(c.form) == "(Cycle 3 2 (3 7))" and c.last is True

    def test_a_gnoth_that_cannot_be_cast(self):
        """"unable to cast all the gnoths in terms of the reigning class" [M86 p. 126]: the
        reason is recorded, the uncastable gnoth prints as `nil`, no form comes out."""
        w, _, _ = still_world(3, 7, 3, 7)
        parse_of(w, "S-group", (1, 2, 3), (4,))
        c = cast_gnoths("S-group", gnoths.gnoths(w))
        assert c.form is None and c.reason == "a gnoth cannot be cast"
        assert c.casts_text() == "(nil (S-group 7 1))"

    def test_casts_that_do_not_generalise_and_a_last_without_promise(self):
        """"if it cannot generalize the casts to come up with a candidate" [M86 p. 126]:
        `(1 2) (3 4)` (starts 1, 3) and `(1 2) (1 2) (3)` (the `3` does not begin `(1 2)`)."""
        w, _, _ = still_world(1, 2, 3, 4)
        parse_of(w, "S-group", (1, 2), (3, 4))
        c = cast_gnoths("S-group", gnoths.gnoths(w))
        assert c.form is None and c.reason == "the casts do not generalise"
        w, _, _ = still_world(1, 2, 1, 2, 3)
        parse_of(w, "S-group", (1, 2), (3, 4), (5,))
        c = cast_gnoths("S-group", gnoths.gnoths(w))
        assert c.form is None and c.reason == "the casts do not generalise" and c.last is False

    def test_no_gnoths(self):
        c = cast_gnoths("S-group", [])
        assert c.form is None and c.reason == "no gnoths" and c.casts_text() == "()"

    def test_the_future_forms_of_p125_are_not_attempted(self):
        """"(C-group (S-group m n) (Cycle (1 4)))" is future notation [M86 p. 125]: a gnoth
        whose terms are no flat run of the class is simply uncastable (DECISION D-048)."""
        w, _, _ = still_world(1, 2, 1, 2, 1, 2)
        parse_of(w, "C-group", (1, 2, 3, 4, 5, 6))
        assert cast_gnoths("C-group", gnoths.gnoths(w)).form is None


# ---- the candidate [M86 pp. 91, 125] (DECISIONS D-034, D-035) ------------------------------

class TestCandidate:

    def test_a_freeze_dried_repeat_is_rejected(self):
        """A candidate equal to an old hypothesis that explained at least as many terms as
        are known is rejected, "a check against cycling" [M86 p. 91] (DECISION D-035); one
        that could explain more is allowed; the knob turns the check off."""
        w, _, _ = still_world(1, 2, 1)
        form = parse("(S-group 1 2)")
        w.freeze_dried = [FreezeDried(form, 3)]
        assert repeats_freeze_dried(w, form)
        w.freeze_dried = [FreezeDried(form, 2)]
        assert not repeats_freeze_dried(w, form)
        w.freeze_dried = [FreezeDried(parse("(S-group 1 3)"), 3)]
        assert not repeats_freeze_dried(w, form)
        w2, _, _ = still_world(1, 2, 1, config=Config(reject_freeze_dried_repeats=False))
        w2.freeze_dried = [FreezeDried(form, 3)]
        assert not repeats_freeze_dried(w2, form)

    def test_the_task_rejects_a_repeat_and_sets_out_a_slip_scout(self):
        """`(1 2) (1)` under S-group with `(S-group 1 2)` freeze-dried over 3 terms: the
        casts are printed, no candidate is made, a Slip-scout follows [M86 p. 126]."""
        w, recorder, trace = still_world(1, 2, 1)
        parse_of(w, "S-group", (1, 2), (3,))
        w.freeze_dried = [FreezeDried(parse("(S-group 1 2)"), 3)]
        post_and_run(w, "Gnoth-caster")
        assert w.hypothesis is None
        failed = recorder.of_kind("cast-failed")[0]
        assert (failed.reason, failed.failures, failed.blast) == \
            ("(S-group 1 2) repeats a freeze-dried hypothesis", 1, False)
        assert [e.type for e in recorder.of_kind("posted")][-1] == "Slip-scout"
        assert "casts: ((S-group 1 2) (S-group 1 1))" in trace.lines
        assert not any(l.startswith("new hypoth") for l in trace.lines)

    def test_a_candidate_that_does_not_postdict_fails(self):
        """"if the candidate fails to postdict the sequence properly, the reformulation
        effort has failed" [M86 p. 126]: gnoths `(1 2)`, `(2 3)` over `1 2 3 2 3` (the
        third term in no gnoth) give `(S-group (Countup 1) 2)`, which predicts `1 2 2 3`."""
        w, recorder, trace = still_world(1, 2, 3, 2, 3)
        parse_of(w, "S-group", (1, 2), (4, 5))
        post_and_run(w, "Gnoth-caster")
        assert w.hypothesis is None
        failed = recorder.of_kind("cast-failed")[0]
        assert failed.reason == "(S-group (Countup 1) 2) does not postdict the terms"
        assert trace.lines[-2:] == ["casts: ((S-group 1 2) (S-group 2 2))", "(S-group (Countup 1) 2)"]

    def test_adopting_a_candidate(self):
        """"the candidate is instantiated as the new hypothesis for the system" [M86 p. 125]:
        its box is left after the known terms (DECISION D-034), its validity 0, the line
        `new hypoth candidate (S-group 1 2)` and a Call-term follow [M86 p. 174] (DECISION
        D-012), the failure count is reset (DECISION D-050) and the gnoths keep their
        frames [M86 p. 175]."""
        w, recorder, trace = still_world(1, 2, 1)
        g1, g2 = parse_of(w, "S-group", (1, 2), (3,))
        g2.frame = 1
        w.cast_failures = 2
        h = adopt_candidate(w, parse("(S-group 1 2)"))
        assert w.hypothesis is h and h.validity == 0 and h.covered == 3 and not h.refuted
        assert h.next_term() == 2
        assert trace.lines[-1] == "new hypoth candidate (S-group 1 2)"
        assert recorder.of_kind("posted")[-1].type == "Call-term"
        assert w.cast_failures == 0 and (g1.frame, g2.frame) == (0, 1)
        assert recorder.of_kind("candidate")[0].form == "(S-group 1 2)"

    def test_the_task_on_p176s_state_with_bond8(self):
        """The gnoths of the transcript's second episode after the Gnoth-operator,
        `(1 2) (1 2 3)`, cast as `casts: ((S-group 1 2) (S-group 1 3))`, `(S-group 1
        (Countup 2))`, `new hypoth candidate (S-group 1 (Countup 2))`, then `doing task
        Call-term` [M86 p. 178]."""
        w, recorder, trace = still_world(1, 2, 1, 2, 3)
        gnoth_over(w, "S-group", 1, "glint1", "glint2")
        gnoth_over(w, "S-group", 2, "glint3", "glint4", "glint5")
        w.reigning = "S-group"
        w.freeze_dried = [FreezeDried(parse("(Countup 1)"), 2),
                          FreezeDried(parse("(S-group 1 2)"), 4)]
        w.taskrack.clean(TASK_TYPES)
        post_and_run(w, "Gnoth-caster")
        assert w.taskrack.count_by_type() == {"Call-term": 1}
        run_to_prompt(w)
        lines = lines_from(trace, "doing task Gnoth-caster")
        assert lines == ["doing task Gnoth-caster",
                         "casts: ((S-group 1 2) (S-group 1 3))",
                         "(S-group 1 (Countup 2))",
                         "new hypoth candidate (S-group 1 (Countup 2))",
                         "doing task Call-term"]
        assert w.hypothesis.show_form() == "(S-group 1 (Countup 2))"
        assert gnoths.parse_text(w) == "((1 2) (1 2 3))"

    def test_stale_cases(self):
        """No reigning class, or a hypothesis already: nothing is done (DECISION D-009)."""
        w, recorder, trace = still_world(1, 2, 1)
        post_and_run(w, "Gnoth-caster")
        assert recorder.of_kind("stale")[-1].reason == "no reigning class"
        parse_of(w, "S-group", (1, 2), (3,))
        post_and_run(w, "Gnoth-caster")
        assert w.hypothesis is not None
        post_and_run(w, "Gnoth-caster")
        assert recorder.of_kind("stale")[-1].reason == "a hypothesis exists"
        assert len(recorder.of_kind("candidate")) == 1


# ---- the Slip-scout [M86 p. 126] (DECISION D-049) -------------------------------------------

class TestSlipScout:

    def test_the_check_again_with_the_staying_pressure_halved(self):
        """On p. 176's state the check gives `stayval: 15.0` against `(Y-group 4.0)`
        [M86 p. 176]; a Slip-scout checks again with the staying pressure times
        `Config.slip_scout_factor`, 7.5, prints it, stays, and sets out a Reformulator
        (DECISION D-049)."""
        w, recorder, trace = still_world(1, 2, 1, 2, 3)
        page_176(w)
        slipping.assess_bonds(w, "S-group")
        post_and_run(w, "Slip-scout")
        check = recorder.of_kind("slip-check")[-1]
        assert (check.stayval, check.best, check.pressure, check.slipped) == (7.5, "Y-group", 4.0, False)
        assert trace.lines[-2:] == ["slip-check: stayval: 7.5", "        best: (Y-group 4.0)"]
        scouted = recorder.of_kind("slip-scouted")[0]
        assert (scouted.before, scouted.after, scouted.factor, scouted.stayval) == \
            ("S-group", "S-group", 0.5, 7.5)
        assert [e.type for e in recorder.of_kind("posted")][-1] == "Reformulator"

    def test_the_factor_can_make_the_class_change(self):
        """With a factor of 0.2 the staying pressure 3.0 is below Y-group's 4.0: "the
        probability of a class change will increase" [M86 p. 126]; the slip is announced."""
        w, recorder, trace = still_world(1, 2, 1, 2, 3, config=Config(slip_scout_factor=0.2))
        page_176(w)
        slipping.assess_bonds(w, "S-group")
        post_and_run(w, "Slip-scout")
        assert w.reigning == "Y-group"
        assert "Changing to a new reigning class. S-group --> Y-group" in trace.lines
        assert recorder.of_kind("slip-scouted")[0].after == "Y-group"

    def test_a_strict_class_slips_at_once(self):
        """Under a strict class the Slip-scout's check is not printed: the class slips to
        its most slippery neighbour (DECISION D-040)."""
        w, recorder, trace = still_world(1, 2)
        parse_of(w, "Countup", (1,), (2,))
        post_and_run(w, "Slip-scout")
        assert w.reigning == "S-group" and recorder.of_kind("slip-scouted")[0].stayval is None
        assert not any(l.startswith("slip-check") for l in trace.lines)

    def test_stale_cases(self):
        w, recorder, trace = still_world(1, 2, 1)
        post_and_run(w, "Slip-scout")
        assert recorder.of_kind("stale")[-1].reason == "no reigning class"
        parse_of(w, "S-group", (1, 2), (3,))
        adopt_candidate(w, parse("(S-group 1 2)"))
        post_and_run(w, "Slip-scout")
        assert recorder.of_kind("stale")[-1].reason == "a hypothesis exists"


# ---- failure and the blast [M86 pp. 39, 92] (DECISION D-050) ------------------------------------

class TestBlast:

    def blasted_world(self):
        """`1 2 1 2 3` with a bottom-up glom `(1 2)` under a gnoth, a second gnoth, a
        catchall, a frame glom, bonds between glints and a bond to the glom."""
        w, recorder, trace = still_world(1, 2, 1, 2, 3)
        glom = cy.make_glom(w, ("Succ", "print-value", "group"), [w.find("glint1"), w.find("glint2")])
        g1 = gnoths.make_gnoth(w, "S-group", [glom], 0)
        g2 = gnoth_over(w, "S-group", 1, "glint3", "glint4")
        gnoths.make_catchall(w, [w.find("glint5")])
        b1 = bond(w, "Succ", "adjacent", "glint1", "glint2")
        b2 = bond(w, "Same", "remote", "glint1", "glint3")
        b3 = bond(w, "Same", "remote", glom.name, "glint3")
        w.reigning, w.threshold = "S-group", 9.0
        w.template = object()
        w.post("Reformulator")
        w.post("Bond-assessor", "S-group", 9.0)
        return w, recorder, trace, (glom, g1, g2, b1, b2, b3)

    def test_the_blast_leaves_only_glints_and_their_bonds(self):
        """"blast away all gnoths and gloms, leaving only the glints and their bonds"
        [M86 p. 92], "accumulated knowledge of term samenesses and other primitive
        relations maintained" [M86 p. 39]: the gnoths, the catchall, every glom (the
        gnoth-gloms, the frame glom, the bottom-up glom) and the bond to the glom go; the
        glints and the bonds between them stay; the reigning class, threshold, template and
        failure count are cleared and the reformulation tasks removed (DECISION D-050)."""
        w, recorder, trace, (glom, g1, g2, b1, b2, b3) = self.blasted_world()
        w.cast_failures = 2
        blast(w, "the casts do not generalise")
        assert w.cytoplasm.names == ("glint1", "glint2", "glint3", "glint4", "glint5",
                                     "bond1", "bond2")
        assert len(w.socratoplasm) == 0 and gnoths.gnoths(w) == [] and gnoths.catchall(w) is None
        assert all(g.parent is None for g in cy.glints(w))
        assert (w.reigning, w.threshold, w.template, w.cast_failures, w.hypothesis) == \
            (None, None, None, 0, None)
        assert w.taskrack.count_by_type().get("Reformulator", 0) == 0
        assert w.taskrack.count_by_type().get("Bond-assessor", 0) == 0
        event = recorder.of_kind("blasted")[0]
        assert set(event.gnoths) == {g1.name, g2.name, "gnoth3"}
        assert set(event.gloms) >= {glom.name, g1.glom.name, g2.glom.name}
        assert event.bonds == ("bond1", "bond2")
        assert trace.lines[-1] == "blasting away all gnoths and gloms"
        assert w.platoplasm.type("S-group").actualizations == []

    def test_the_surviving_bonds_push_up_an_inspiration(self):
        """"leaving only the glints and their bonds to push up an inspiration" [M86 p. 92]:
        each surviving bond releases what a new bond releases, Sparklers and Glom-scouts
        [M86 p. 65] (DECISIONS D-016, D-050); stage one then runs again."""
        w, recorder, trace, _ = self.blasted_world()
        w.taskrack.clean(tuple(w.urgencies))
        blast(w, "test")
        counts = w.taskrack.count_by_type()
        assert counts["Sparkler"] == 2 * w.config.sparklers_per_bond
        assert counts["Glom-scout"] == 2 * w.config.glom_scouts_per_bond

    def test_failures_are_counted_and_the_third_blasts(self):
        """Two failures set out Slip-scouts; the `Config.max_cast_failures`th blasts
        (DECISION D-050); a candidate or a Gnoth-setter resets the count."""
        w, recorder, trace, _ = self.blasted_world()
        fail(w, "one")
        fail(w, "two")
        assert w.cast_failures == 2 and w.reigning == "S-group"
        assert [e.type for e in recorder.of_kind("posted")][-2:] == ["Slip-scout", "Slip-scout"]
        fail(w, "three")
        assert w.reigning is None and recorder.of_kind("blasted")[0].reason == "three"
        assert [e.blast for e in recorder.of_kind("cast-failed")] == [False, False, True]

    def test_the_knob(self):
        w, recorder, trace, _ = self.blasted_world()
        w.config = Config(max_cast_failures=1)
        fail(w, "once")
        assert recorder.of_kind("blasted")

    def test_the_gnoth_setter_resets_the_count(self):
        """A new episode starts at 0 failures (DECISION D-050)."""
        w, recorder, trace, results = answered_run([3, 7, 3], FENCE_SEED)
        w.cast_failures = 2
        enter_term(w, 3)
        run_to_prompt(w)
        assert recorder.of_kind("freeze-dried") and not recorder.of_kind("blasted")
        assert w.hypothesis.show_form() == "(Cycle 3 3 (3 7 3))"


# ---- whole runs [M86 pp. 168-169, 174-175] -----------------------------------------------------

class TestRuns:

    def test_run_2_on_the_fence_seed(self):
        """`3 7 3 3` then `7` on the fence seed, in the terse mode: after `best: nil` the
        caster prints `casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))`, `(Cycle 3
        3 (3 7 3))` and `new hypoth candidate (Cycle 3 3 (3 7 3))` [M86 pp. 168-169]; the
        fifth term confirms it and the guess is `3 7 3 3 7`, accepted [M86 p. 169]."""
        w, recorder, trace, results = full_run([3, 7, 3, 3, 7], FENCE_SEED, verbose=False)
        after = lines_from(trace, "        best: nil")
        casts = [l for l in after if l.startswith("casts:")]
        assert casts == ["casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))"]
        i = after.index(casts[0])
        assert after[i + 1] == "(Cycle 3 3 (3 7 3))"
        assert "new hypoth candidate (Cycle 3 3 (3 7 3))" in after
        assert results == ["empty", "empty", "call-term", "call-term", "done"]
        guess = recorder.of_kind("guess")[0]
        assert (guess.form, guess.block) == ("(Cycle 3 3 (3 7 3))", "3 7 3 3 7")
        assert trace.lines[-1] == "bye"
        assert [str(f) for f in w.freeze_dried] == ["(Cycle 3 2 (3 7)) (3 terms)"]

    @pytest.mark.parametrize("seed", range(10))
    def test_run_3s_first_reformulation_ends_in_the_second_hypothesis(self, seed):
        """`1 2 1` for ten seeds: the Gnoth-caster prints `casts: ((S-group 1 2) (S-group 1
        1))`, `(S-group 1 2)`, `new hypoth candidate (S-group 1 2)`, `doing task Call-term`
        [M86 p. 174]; the parse is `((1 2) (1))`; the next term `2` is filtered through the
        new hypothesis and produces the guess `(1 2)(1 2)(1 2)` [M86 pp. 174-175]."""
        w, recorder, trace, results = answered_run([1, 2, 1, 2], seed, answers=["nope"])
        i = trace.lines.index("doing task Gnoth-caster")
        assert trace.lines[i:i + 4] == ["doing task Gnoth-caster",
                                        "casts: ((S-group 1 2) (S-group 1 1))",
                                        "(S-group 1 2)",
                                        "new hypoth candidate (S-group 1 2)"]
        # the Call-term follows, at once on p. 174 and after a spark on p. 169
        assert "doing task Call-term" in trace.lines[i + 4:]
        assert results[2] == "call-term"
        assert "new term being hypothesis-filtered through (S-group 1 2)" in trace.lines
        guess = recorder.of_kind("guess")[0]
        assert (guess.form, guess.block) == ("(S-group 1 2)", "(1 2)(1 2)(1 2)")
        assert not recorder.of_kind("cast-failed")

    def test_a_cover_seed_of_3_7_3_7_fails_three_times_and_blasts(self):
        """On a seed where `3 7 3` became `(Y-group 3 3 (3 7 3))`, the fourth term refutes
        it and the gnoths `(3 7 3)` and `(7)` cannot be generalised under Y-group; a
        Slip-scout follows each failure [M86 p. 126] and the third failure blasts
        [M86 p. 92] (DECISION D-050); the glints and their bonds remain and stage one
        starts again."""
        for seed in range(30):
            w, recorder, trace, results = full_run([3, 7, 3, 7], seed)
            if not recorder.of_kind("refuted"):
                continue
            failed = recorder.of_kind("cast-failed")
            assert [e.blast for e in failed] == [False, False, True]
            assert [e.type for e in recorder.of_kind("chosen")].count("Slip-scout") == 2
            blasted = recorder.of_kind("blasted")[0]
            assert blasted.gnoths and blasted.gloms
            assert "blasting away all gnoths and gloms" in trace.lines
            after = recorder.events.index(blasted)
            assert any(e.kind == "chosen" and e.type == "Glom-scout" for e in recorder.events[after:])
            assert w.reigning is None
            break
        else:
            pytest.fail("no cover seed among 0-29")

    def test_3_7_3_7_continued_reaches_the_cycle_on_most_seeds(self):
        """`3 7 3 7 3 7 3 7` over the seeds 0-99: the fence seeds guess `(Cycle 3 2 (3 7))`
        at the fourth term as before, and most cover seeds, after the blast, glom the fence
        in the renewed stage one and guess it later; at least 80 of 100 guess that Cycle
        (92 when this test was written: 35 fence seeds, 57 after a blast; RESULTS.md)."""
        outcomes = Counter()
        for seed in range(100):
            w, recorder, trace, results = answered_run([3, 7, 3, 7, 3, 7, 3, 7], seed)
            guesses = recorder.of_kind("guess")
            blasted = bool(recorder.of_kind("blasted"))
            if guesses and guesses[0].form == "(Cycle 3 2 (3 7))":
                outcomes["blast then cycle" if blasted else "cycle"] += 1
                assert results[-1] == "done"
            else:
                outcomes["other"] += 1
        assert outcomes["cycle"] + outcomes["blast then cycle"] >= 80
        assert outcomes["cycle"] >= 25 and outcomes["blast then cycle"] >= 25

    def test_same_seed_same_trace(self):
        for terms in ([3, 7, 3, 3, 7], [1, 2, 1, 2, 3], [3, 7, 3, 7, 3, 7]):
            traces = [full_run(terms, 5)[2].lines for _ in range(2)]
            assert traces[0] == traces[1]

    def test_the_module_is_installed(self):
        assert TASKS["Gnoth-caster"] is gnoth_caster and TASKS["Slip-scout"] is slip_scout
        assert NOT_BUILT == ()
        assert isinstance(cast_gnoths("S-group", []), Casting)


# ---- the casts written into the gnoths' `form` fields [M86 pp. 175, 179] (item 21) ----------------

class TestRecordCasts:

    def test_p174s_casts_become_the_forms_of_p175(self):
        """`casts: ((S-group 1 2) (S-group 1 1))` [M86 p. 174] and then `show` prints
        `form: (S-group 1 2)` for gnoth2 and `form: (S-group 1 1)` for gnoth3 [M86 p. 175]
        (DECISION D-048 as amended in item 21)."""
        w, _, _ = still_world(1, 2, 1)
        g2, g3 = parse_of(w, "S-group", (1, 2), (3,))
        c = cast_gnoths("S-group", gnoths.gnoths(w))
        casting.record_casts(c)
        assert form_text(g2.form) == "(S-group 1 2)" and form_text(g3.form) == "(S-group 1 1)"
        assert "form: (S-group 1 2)" in g2.show_lines() and "form: (S-group 1 1)" in g3.show_lines()

    def test_p178s_casts_become_the_forms_of_p179(self):
        """`casts: ((S-group 1 2) (S-group 1 3))` [M86 p. 178]; `form: (S-group 1 2)` and
        `form: (S-group 1 3)` [M86 p. 179]."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        g2, g5 = parse_of(w, "S-group", (1, 2), (3, 4, 5))
        casting.record_casts(cast_gnoths("S-group", gnoths.gnoths(w)))
        assert [form_text(g.form) for g in (g2, g5)] == ["(S-group 1 2)", "(S-group 1 3)"]

    def test_a_gnoth_with_several_or_no_casts_keeps_its_field(self):
        """Under a fence class every term is cast (DECISION D-037), so a gnoth over `(3 7)`
        has two casts and its `form` is left alone; an uncastable gnoth's too."""
        w, _, _ = still_world(3, 7, 3, 3)
        g1, g2, g3 = parse_of(w, "Cycle", (1, 2), (3,), (4,))
        g1.form = "kept"
        casting.record_casts(cast_gnoths("Cycle", gnoths.gnoths(w)))
        assert g1.form == "kept" and form_text(g2.form) == "(Cycle 3 1 (3))"
        w, _, _ = still_world(1, 3, 2)
        (g,) = parse_of(w, "S-group", (1, 2, 3))
        g.form = "kept"
        casting.record_casts(cast_gnoths("S-group", gnoths.gnoths(w)))
        assert g.form == "kept"

    def test_the_gnoth_caster_records_the_casts_in_a_run(self):
        """`1 2 1` run to the second hypothesis: the gnoths' `form` fields are the casts
        printed [M86 pp. 174-175]."""
        world, recorder, trace, results = answered_run([1, 2, 1], 3)
        assert world.hypothesis.show_form() == "(S-group 1 2)"
        forms = [form_text(g.form) for g in gnoths.gnoths(world)]
        assert forms == ["(S-group 1 2)", "(S-group 1 1)"]

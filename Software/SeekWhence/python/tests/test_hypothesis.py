"""The hypothesis object [M86 pp. 79-85, 91, 125]: form, box, validity, prediction, reset,
postdiction and the guess block, on the forms of the three appendix runs."""

import pytest

from seekwhence.config import Config
from seekwhence.forms import parse
from seekwhence.hypothesis import FreezeDried, Hypothesis, block_text, postdicts


def hyp(text: str) -> Hypothesis:
    return Hypothesis(parse(text))


# ---- fields and operations [M86 pp. 81-82] --------------------------------------------------

class TestFields:
    def test_form_box_and_validity(self):
        """A hypothesis has its form, its box and a validity that starts at 0
        [M86 p. 81]; it "can display its form" as `hypothesis: (Cycle 3 2 (3 7))`
        [M86 p. 166]."""
        h = hyp("(Cycle 3 2 (3 7))")
        assert h.show_form() == "(Cycle 3 2 (3 7))"
        assert h.validity == 0
        assert h.box.top.name == "box1" and h.box.form is h.form

    def test_hit_gives_the_next_run_of_terms(self):
        """"The box is the structure which, when 'hit', produces the next run of terms
        predicted by the hypothesis" [M86 p. 81]."""
        h = hyp("(S-group 1 (Countup 2))")
        assert [h.hit() for _ in range(3)] == [(1, 2), (1, 2, 3), (1, 2, 3, 4)]
        assert h.covered == 9

    def test_predict_the_next_term(self):
        """"It can also predict the next term to be expected following that form"
        [M86 p. 81], term by term: `(Cycle 3 2 (3 7))` predicts 3, 7, 3, 7 and
        `(S-group 1 (Countup 2))` predicts 1 2 1 2 3 1 2 3 4 [M86 p. 56]."""
        assert [hyp("(Cycle 3 2 (3 7))").next_term() for _ in range(1)] == [3]
        h = hyp("(Cycle 3 2 (3 7))")
        assert [h.next_term() for _ in range(4)] == [3, 7, 3, 7]
        h = hyp("(S-group 1 (Countup 2))")
        assert [h.next_term() for _ in range(9)] == [1, 2, 1, 2, 3, 1, 2, 3, 4]
        assert h.covered == 9

    def test_reset_starts_again(self):
        """"The box can be reset to start again" [M86 p. 81]; the validity is not the
        box's and stays."""
        h = hyp("(Countup 1)")
        assert [h.next_term() for _ in range(3)] == [1, 2, 3]
        h.validity = 2
        h.reset()
        assert h.covered == 0 and h.validity == 2
        assert h.next_term() == 1

    def test_rebuild_scraps_the_box_tree(self):
        """"the current technique, which is to scrap the old box tree and make a new one"
        [M86 p. 85]."""
        h = hyp("(Countup 1)")
        old = h.box
        h.next_term()
        h.rebuild()
        assert h.box is not old and h.next_term() == 1

    def test_list_terms(self):
        """"asked to list a number of terms" [M86 p. 82], from the beginning, without
        moving the hypothesis's own box."""
        h = hyp("(Cycle (8 (Countup 1) shared))")
        assert h.next_term() == 8
        assert h.list_terms(9) == [8, 1, 2, 8, 3, 4, 8, 5, 6]
        assert h.next_term() == 1

    def test_frame_is_one_hit_or_one_turn_of_a_cycle(self):
        """One frame of a group hypothesis is one hit; one frame of a Cycle is one full turn
        (DECISION D-004: p. 81 groups the Cycle's hits one turn per parenthesis, and the
        Cycle gnoth of run 1 has `range: (1 2)` [M86 p. 168])."""
        assert hyp("(S-group 1 (Countup 2))").frame() == (1, 2)
        h = hyp("(Cycle 3 2 (3 7))")
        assert h.frame() == (3, 7) and h.frame() == (3, 7)
        assert hyp("(Cycle ((Countup 1) (Tuple (5 8 4))))").frame() == (1, (5, 8, 4))


# ---- validity [M86 pp. 81, 98, 168] (DECISION D-032) ------------------------------------------

class TestValidity:
    def test_validity_grows_with_correctly_predicted_terms(self):
        """"a number that grows as new, correctly-predicted terms are encountered"
        [M86 p. 81]: one per term (DECISION D-032)."""
        h = hyp("(Cycle 3 2 (3 7))")
        assert [h.check(t) for t in (3, 7, 3)] == [True, True, True]
        assert h.validity == 3

    def test_a_wrong_term_sets_validity_to_0(self):
        """"the system immediately sets the hypothesis' validity to 0" [M86 p. 98], printed
        `set-out -- validity: 0` when the term 3 denies `(Cycle 3 2 (3 7))` [M86 p. 168]."""
        h = hyp("(Cycle 3 2 (3 7))")
        assert [h.check(t) for t in (3, 7, 3)] == [True, True, True]
        assert h.check(3) is False
        assert h.validity == 0

    def test_the_guess_threshold_is_one_confirming_term(self):
        """Every transcript guesses after exactly one confirming term [M86 pp. 166, 169, 175,
        179] (DECISION D-032): after postdicting the known terms a hypothesis has validity 0
        and reaches `Config.guess_validity` on the next correct term."""
        h = hyp("(Cycle 3 2 (3 7))")
        assert h.postdict([3, 7, 3]) and h.validity == 0
        assert h.check(7) and h.validity == Config().guess_validity


# ---- postdiction [M86 p. 125] (DECISION D-034) -----------------------------------------------

class TestPostdiction:
    @pytest.mark.parametrize("text,known", [
        ("(Cycle 3 2 (3 7))", [3, 7, 3]),                   # run 1, p. 165
        ("(Cycle 3 3 (3 7 3))", [3, 7, 3, 3]),              # run 2, p. 169
        ("(Countup 1)", [1, 2]),                            # run 3, p. 171
        ("(S-group 1 2)", [1, 2, 1]),                       # run 3, p. 174
        ("(S-group 1 (Countup 2))", [1, 2, 1, 2, 3, 1]),    # run 3, p. 178: last hit cut short
        ("(Tuple 1 3 ((Countup 1) 8 shared))", [1, 8, 1, 2, 8]),
        ("(Cycle (8 (Countup 1) shared))", [8, 1, 2, 8, 3]),
    ])
    def test_forms_that_postdict_their_runs_terms(self, text, known):
        """A candidate's box "is tested to see whether it can accurately 'postdict' the
        known terms of the sequence" [M86 p. 125]: the hypotheses of the three runs
        postdict the terms entered when each was made [M86 pp. 165, 169, 171, 174, 178]."""
        assert postdicts(parse(text), known)
        assert hyp(text).postdict(known)

    @pytest.mark.parametrize("text,known", [
        ("(Cycle 3 2 (3 7))", [3, 7, 3, 3]),          # the term that refuted run 1's hypothesis
        ("(Countup 1)", [1, 2, 1]),                   # the term that refuted run 3's first
        ("(S-group 1 2)", [1, 2, 1, 2, 3]),           # and its second
        ("(S-group 1 (Countup 2))", [1, 2, 1, 2, 4]),
        ("(C-group 1 3)", [1, 1, 2]),
        ("(P-group 2 4)", [2, 1, 0]),                 # undefined form, D-006
    ])
    def test_forms_that_do_not_postdict(self, text, known):
        """A form that does not reproduce the known terms fails the test [M86 p. 125]; the
        terms that refuted the runs' hypotheses [M86 pp. 168, 173, 176] are such cases, and
        so is an undefined P-group (DECISION D-006)."""
        assert not postdicts(parse(text), known)
        h = hyp(text)
        assert not h.postdict(known)
        assert h.covered == 0 and h.validity == 0        # left reset

    def test_postdiction_leaves_the_box_after_the_known_terms(self):
        """"asked to predict the next term, given the sequence's currently known terms"
        [M86 p. 82]: after postdicting `1 2 1 2 3 1`, `(S-group 1 (Countup 2))` predicts 2,
        the second term of its cut-short third hit [M86 pp. 178-179]."""
        h = hyp("(S-group 1 (Countup 2))")
        assert h.postdict([1, 2, 1, 2, 3, 1])
        assert h.covered == 6
        assert [h.next_term() for _ in range(3)] == [2, 3, 4]
        h = hyp("(Cycle 3 2 (3 7))")
        assert h.postdict([3, 7, 3]) and h.next_term() == 7

    def test_postdicting_no_terms_is_trivially_true(self):
        """With nothing known every form postdicts, as the fall-back Tuple must [M86 p. 79]."""
        assert hyp("(Tuple (1 1 2))").postdict([])
        assert hyp("(Tuple (1 1 2))").postdict([1, 1, 2])


# ---- the guess block [M86 pp. 166, 169, 175, 179] (DECISION D-033) ---------------------------

class TestGuessBlock:
    @pytest.mark.parametrize("text,n_entered,printed,page", [
        ("(Cycle 3 2 (3 7))", 4, "3 7 3 7", 166),
        ("(Cycle 3 3 (3 7 3))", 5, "3 7 3 3 7", 169),
        ("(S-group 1 2)", 4, "(1 2)(1 2)(1 2)", 175),
        ("(S-group 1 (Countup 2))", 6, "(1 2)(1 2 3)(1 2 3 4)", 179),
    ], ids=["run1", "run2", "run3a", "run3b"])
    def test_the_four_printed_guesses(self, text, n_entered, printed, page):
        """The guess block of each transcript, from the first term for at least three hits
        and until every entered term is covered (DECISION D-033): `3 7 3 7` after four terms
        [M86 p. 166], `3 7 3 3 7` after five [M86 p. 169], `(1 2)(1 2)(1 2)` after four
        [M86 p. 175], `(1 2)(1 2 3)(1 2 3 4)` after six [M86 p. 179]."""
        h = hyp(text)
        block = h.guess_block(n_entered, Config().guess_min_hits)
        assert block_text(block) == printed

    def test_the_guess_does_not_move_the_hypothesis_box(self):
        """The guess fires a fresh box (DECISION D-033); the hypothesis's own box still
        predicts the term after the known ones."""
        h = hyp("(Cycle 3 2 (3 7))")
        assert h.postdict([3, 7, 3, 7])
        h.guess_block(4)
        assert h.covered == 4 and h.next_term() == 3

    def test_at_least_min_hits_even_when_fewer_cover_the_terms(self):
        """`(S-group 1 2)` covers four terms in two hits but the guess shows three
        [M86 p. 175]; with `min_hits` 1 it would show two."""
        h = hyp("(S-group 1 2)")
        assert len(h.guess_block(4, 3)) == 3
        assert len(h.guess_block(4, 1)) == 2
        assert len(h.guess_block(7, 3)) == 4

    def test_block_text_formats(self):
        """Single terms are separated by spaces, lists run together [M86 pp. 166, 175]."""
        assert block_text([3, 7, 3, 7]) == "3 7 3 7"
        assert block_text([(1, 2), (1, 2)]) == "(1 2)(1 2)"
        assert block_text([1, (5, 8, 4), 2]) == "1 (5 8 4) 2"
        assert block_text([]) == ""


# ---- freeze-dried forms [M86 p. 91] (DECISION D-035) ----------------------------------------

def test_freeze_dried_form_keeps_the_form_and_the_terms_explained():
    """A failed hypothesis's form "is kept on a list of old hypotheses, along with the number
    of terms of the sequence it explained" [M86 p. 91]: `(Cycle 3 2 (3 7))` explained
    four terms when the fifth refuted it [M86 p. 168]."""
    old = FreezeDried(parse("(Cycle 3 2 (3 7))"), 4)
    assert str(old) == "(Cycle 3 2 (3 7)) (4 terms)"
    assert old == FreezeDried(parse("(Cycle (3 7))"), 4)
    with pytest.raises(AttributeError):
        old.terms_explained = 5


def test_only_forms_make_hypotheses():
    with pytest.raises(TypeError):
        Hypothesis("(Countup 1)")

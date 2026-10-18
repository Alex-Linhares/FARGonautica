"""Loop0004 item 03: the sequence pane's model (`seekwhence/gui/models/sequence.py`),
Qt-free (G-014).

- At the guess of run 1 the model shows the block `3 7 3 7` [M86 p. 166], the fourth
  term predicted, the guess pending; answered `ok`, the badge is accepted.
- Run 3 shows the refused badge on `(1 2)(1 2)(1 2)` (answered `nope`) and then the
  accepted `(1 2)(1 2 3)(1 2 3 4)` [M86 pp. 175, 179]; the refuting term is red until the
  rebuild, whose candidate clears the fates.
- The ghost cells are the fresh hypothesis's block, aligned by position, the ones beyond
  the last term the continuation; the input waits at the start and at each prompt.
- The model is an observer that changes nothing: a run with it is the same run.
"""

import io

import pytest

from seekwhence.events import Recorder
from seekwhence.gui.models.sequence import FATES, AnswerBadge, SequenceModel, TermCell
from seekwhence.session import Session
from seekwhence.trace import event_record

from golden import APPENDIX

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)


def model_at(events, index):
    """The model after the events up to `index`."""
    model = SequenceModel()
    for event in events[:index + 1]:
        model.on_event(event)
    return model


def indexes(events, kind):
    return [i for i, e in enumerate(events) if e.kind == kind]


# -- run 1 [M86 pp. 164-166] ------------------------------------------------------------------------

def test_at_the_guess_of_run_1_the_block_is_3_7_3_7(appendix_runs):
    events = appendix_runs[RUN1].events
    at_guess = indexes(events, "guess")[0]
    model = model_at(events, at_guess)
    assert model.ghost_text() == "3 7 3 7"                               # [M86 p. 166]
    assert model.form == "(Cycle 3 2 (3 7))" and not model.refuted
    assert [c.value for c in model.terms] == [3, 7, 3, 7]
    assert [c.fate for c in model.terms] == ["unfiltered"] * 3 + ["predicted"]
    assert model.terms[3] == TermCell(4, 7, "predicted", 7)
    assert model.pending == AnswerBadge("(Cycle 3 2 (3 7))", "3 7 3 7", (3, 7, 3, 7))
    assert model.pending.verdict == "waiting" and model.badges == []
    blocks = model.ghost_blocks()
    assert [[c.value for c in block] for block in blocks] == [[3], [7], [3], [7]]
    assert all(not c.beyond and c.agrees for c in model.ghosts())
    assert model.continuation() == ()


def test_run_1_answered_ok_ends_with_an_accepted_badge(appendix_runs):
    events = appendix_runs[RUN1].events
    model = model_at(events, len(events) - 1)
    assert model.done and not model.waiting and model.pending is None
    assert [(b.block, b.answer, b.accepted, b.verdict) for b in model.badges] == [
        ("3 7 3 7", "ok", True, "ok")]                                     # [M86 p. 166]


# -- run 3 [M86 pp. 170-179] ------------------------------------------------------------------------

def test_run_3_shows_the_refused_badge_then_the_accepted_one(appendix_runs):
    events = appendix_runs[RUN3].events
    first, second = indexes(events, "answered")
    model = model_at(events, first)
    assert [(b.block, b.answer, b.verdict) for b in model.badges] == [
        ("(1 2)(1 2)(1 2)", "nope", "no")]                                 # [M86 p. 175]
    assert model.form == "(S-group 1 2)" and not model.refuted
    model = model_at(events, second)
    assert [(b.block, b.verdict) for b in model.badges] == [
        ("(1 2)(1 2)(1 2)", "no"), ("(1 2)(1 2 3)(1 2 3 4)", "ok")]          # [M86 p. 179]
    assert model.ghost_text() == "(1 2)(1 2 3)(1 2 3 4)"
    assert model.form == "(S-group 1 (Countup 2))"


def test_run_3_ghosts_are_aligned_by_position_with_the_continuation_beyond(appendix_runs):
    events = appendix_runs[RUN3].events
    model = model_at(events, len(events) - 1)
    ghosts = model.ghosts()
    assert [g.position for g in ghosts] == list(range(1, 10))
    assert [g.hit for g in ghosts] == [1, 1, 2, 2, 2, 3, 3, 3, 3]
    assert [g.value for g in ghosts] == [1, 2, 1, 2, 3, 1, 2, 3, 4]
    assert [g.beyond for g in ghosts] == [False] * 6 + [True] * 3
    assert [g.agrees for g in ghosts] == [True] * 6 + [None] * 3
    assert [g.value for g in model.continuation()] == [2, 3, 4]
    assert [[c.value for c in b] for b in model.ghost_blocks()] == [[1, 2], [1, 2, 3], [1, 2, 3, 4]]


def test_a_refuting_term_is_red_until_the_candidate_clears_the_fates(appendix_runs):
    events = appendix_runs[RUN3].events
    refuted = indexes(events, "refuted")[0]
    model = model_at(events, refuted)
    assert model.terms[2] == TermCell(3, 1, "refuting", 3)       # 3 predicted, 1 entered
    assert model.refuted and model.form == "(Countup 1)"
    assert model.ghost_text() == "1 2 3"
    candidate = indexes(events, "candidate")[0]
    model = model_at(events, candidate)
    assert all(c.fate == "unfiltered" and c.predicted is None for c in model.terms)
    assert model.form == "(S-group 1 2)" and not model.refuted
    # The second refutation: term 5 (3) where 1 was predicted [M86 p. 176].
    model = model_at(events, indexes(events, "refuted")[1])
    assert model.terms[4] == TermCell(5, 3, "refuting", 1)
    assert model.terms[3].fate == "predicted"


def test_a_refused_guess_keeps_the_form_and_the_fates(appendix_runs):
    events = appendix_runs[RUN3].events
    first = indexes(events, "answered")[0]
    before, after = model_at(events, first - 1), model_at(events, first)
    assert after.form == before.form == "(S-group 1 2)"
    assert [c.fate for c in after.terms] == [c.fate for c in before.terms]
    assert after.pending is None and before.pending is not None


# -- the input (PANES.md P1) ------------------------------------------------------------------------

def test_the_model_waits_at_the_start_and_at_each_prompt(appendix_runs):
    events = appendix_runs[RUN1].events
    waiting = [model_at(events, i).waiting for i in range(len(events))]
    assert waiting[0]                                   # start: the console asks first
    for i in indexes(events, "term"):
        assert not waiting[i]
    for i in indexes(events, "prompt"):
        assert waiting[i]
    assert not waiting[-1]


def test_without_a_hypothesis_there_are_no_ghosts(appendix_runs):
    events = appendix_runs[RUN1].events
    model = model_at(events, indexes(events, "term")[2])
    assert model.form is None and model.ghosts() == () and model.ghost_text() == ""
    assert [c.value for c in model.terms] == [3, 7, 3]
    assert "no hypothesis" in model.cell_tooltip(model.terms[0])


def test_the_tooltips_name_the_form_and_the_fate(appendix_runs):
    events = appendix_runs[RUN3].events
    model = model_at(events, len(events) - 1)
    assert model.cell_tooltip(model.terms[5]).startswith("term 6: 1, predicted")
    beyond = model.continuation()[0]
    assert model.ghost_tooltip(beyond) == (
        "predicted by (S-group 1 (Countup 2)), hit 3: 2 at position 7 (not entered yet)")
    assert model.ghost_tooltip(model.ghosts()[0]).endswith("(agrees)")


def test_the_fates_are_the_three_of_panes_md():
    assert FATES == ("unfiltered", "predicted", "refuting")


def test_a_start_event_begins_a_new_run(appendix_runs):
    events = appendix_runs[RUN1].events
    model = model_at(events, len(events) - 1)
    model.on_event(appendix_runs[RUN3].events[0])
    assert model.terms == [] and model.badges == [] and model.form is None
    assert model.waiting and model.seed == 30 and model.events == 1


# -- the model observes and changes nothing -----------------------------------------------------------

@pytest.mark.parametrize("run_id,seed", APPENDIX)
def test_a_run_observed_by_the_model_is_the_same_run(appendix_runs, run_id, seed):
    ref = appendix_runs[run_id]
    model, rec, out = SequenceModel(), Recorder(), io.StringIO()
    session = Session(ref.config, seed, out=out, lines=list(ref.lines),
                      answers=list(ref.answers), observers=[rec, model])
    session.run()
    assert [event_record(e) for e in rec.events] == ref.records
    assert out.getvalue() == ref.text and session.world.rng.draws == ref.draws
    assert model.events == len(ref.events) and model.done
    # The ghosts came from a fresh hypothesis: the live box is where the run left it.
    assert session.world.hypothesis.covered == len(session.world.terms)
    model.ghosts()
    assert session.world.hypothesis.covered == len(session.world.terms)

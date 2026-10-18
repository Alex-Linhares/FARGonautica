"""Loop0004 item 11: the reformulation pane's model (`seekwhence/gui/models/reformulation.py`),
Qt-free (G-039, G-040, G-041).

- Every reformulation event of the three appendix runs, and of two Blackburn runs, lands
  on exactly one card; no other event lands on any.
- Run 3's first episode shows `Countup --> S-group`, a PROGRAM with `SHIFT-RIGHT`, the
  casts `((S-group 1 2) (S-group 1 1))` and the candidate `(S-group 1 2)`, as p. 174 has
  them [M86 p. 174]; its cards come in the storyboard's order.
- The operation's before and after pictures in the style of p. 110, with the glom names
  under the groups, and the operations in series as p. 120 stacks them, `[] (disappears)`
  [M86 pp. 110, 120].
- Run 2's one episode: the refutation at term 4, the freeze-dried `(Cycle 3 2 (3 7))`
  with 3 terms, `gnoths: (gnoth1 gnoth2 gnoth3)`, `best: nil`, the candidate
  `(Cycle 3 3 (3 7 3))` [M86 pp. 168-170].
- The freeze-dried list at run 3's end, `(Countup 1)` (2 terms) and `(S-group 1 2)`
  (4 terms), newest first, equal to every snapshot's [M86 p. 179]; the threshold's course
  within an episode, up by 1 at each Reformulator and down by 1 at an operator that acts
  [M86 pp. 105, 124].
- A failure with its Slip-scout, a refused operation and the blast on blackburn-8; a
  late Capper's card on blackburn-5 [M86 pp. 92, 113, 126].
- Synthetic events: an event with no episode open begins one without a refutation; a
  start resets; folding; the texts and the pages of every card.
"""

import io

import pytest

from seekwhence import bench, sequences
from seekwhence.config import Config
from seekwhence.events import (
    BondsAssessed, CastFailed, CastsMade, GnothOperation, HypothesisFreezeDried,
    HypothesisRefuted, Reformulated, RunStarted, SlipScouted)
from seekwhence.gui.models import reformulation as rf
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)


def logged(config, seed, lines, answers=()):
    """A run's `EventLog` with the tap's snapshots."""
    log = EventLog()
    tap = Tap(log)
    session = Session(config, seed, out=io.StringIO(), lines=list(lines),
                      answers=list(answers), observers=[tap])
    tap.attach_session(session)
    session.run()
    return log


def appendix_log(run_id):
    spec = bench.run_spec(bench.load_expectations(), run_id)
    return logged(Config(verbose=spec["prints_task_lines"]), SEEDS[run_id],
                  bench.inputs_of(spec), bench.answers_of(spec))


def blackburn_log(name, seed, n_terms=12):
    """A Blackburn sequence's first terms, scripted, no answers (the gate's cases)."""
    seq = sequences.find(name)
    return logged(Config(), seed, [str(t) for t in seq.terms[:n_terms]])


@pytest.fixture(scope="module")
def logs():
    return {RUN1: appendix_log(RUN1), RUN2: appendix_log(RUN2), RUN3: appendix_log(RUN3),
            "blackburn-8": blackburn_log("blackburn-8", 1),
            "blackburn-5": blackburn_log("blackburn-5", 3)}


def model_at(log, index=None):
    """The model fed the events up to `index` (the end by default) with the snapshots in
    force (G-039)."""
    model = rf.ReformulationModel()
    last = len(log.events) - 1 if index is None else index
    for i in range(last + 1):
        model.on_event(log.events[i], i)
        model.update(log.state_at(i))
    return model


def texts(card):
    return [text for _kind, text, _tip in card.lines]


def kinds(episode):
    return [card.kind for card in episode.cards]


# -- every event on exactly one card ----------------------------------------------------------------

@pytest.mark.parametrize("name", [RUN1, RUN2, RUN3, "blackburn-8", "blackburn-5"])
def test_every_reformulation_event_lands_on_exactly_one_card(logs, name):
    log = logs[name]
    model = model_at(log)
    cards = model.cards()
    for i, event in enumerate(log.events):
        n = sum(i in card.indices for card in cards)
        assert n == (1 if event.kind in rf.KINDS else 0), (i, event.kind, n)
    for card in cards:
        assert card.indices and card.indices[0] == card.index
        assert card.kind in rf.CARD_KINDS and not card.open


def test_the_cards_are_built_the_same_live_and_from_a_seek(logs):
    log = logs[RUN3]
    whole = model_at(log)
    for index in (118, 185, 222, 279, 355, 551, 576, len(log.events) - 1):
        part = model_at(log, index)
        assert [kinds(e) for e in part.episodes] == \
            [kinds(e) for e in whole.episodes if e.index <= index][:len(part.episodes)] \
            or part.episodes[-1].index <= index
        for a, b in zip(part.cards(), whole.cards()):
            assert a.kind == b.kind and a.index == b.index
            assert a.indices == [i for i in b.indices if i <= index]


# -- run 3's first episode as p. 174 ----------------------------------------------------------------

def test_run_3_first_episode_as_page_174(logs):
    model = model_at(logs[RUN3])
    assert len(model.episodes) == 2
    first = model.episodes[0]
    assert (first.form, first.n, first.term, first.predicted) == ("(Countup 1)", 3, 1, 3)
    assert kinds(first)[:7] == ["refutation", "freeze-dried", "gnoths-set", "slip-check",
                                "moves", "operation", "assessor"]
    assert kinds(first)[-2:] == ["casts", "candidate"]
    by_kind = {}
    for card in first.cards:
        by_kind.setdefault(card.kind, card)
    assert texts(by_kind["refutation"]) == [
        "(Countup 1) predicted 3 at term 3; the term was 1", "set-out -- validity: 0"]
    assert texts(by_kind["freeze-dried"]) == [
        "(Countup 1) (2 terms) kept on the list of old hypotheses", "reigning class: Countup"]
    assert "gnoths: (gnoth1 gnoth2 gnoth3)" in texts(by_kind["gnoths-set"])     # [M86 p. 173]
    assert any("Countup --> S-group" in t for t in texts(by_kind["slip-check"]))  # [M86 p. 174]
    moves = texts(by_kind["moves"])
    assert moves[0] == "threshold 8.0 in S-group"
    assert any(t.startswith("PROGRAM: (SHIFT-RIGHT") for t in moves)
    assert by_kind["operation"].operation == "(SHIFT-RIGHT gnoth1 gnoth2 (glint1))"
    assert by_kind["operation"].reason is None
    assert texts(by_kind["assessor"])[0] == "doing task Bond-assessor on (S-group 8.0)"
    assert texts(by_kind["casts"])[:2] == ["casts: ((S-group 1 2) (S-group 1 1))",
                                           "(S-group 1 2)"]
    assert texts(by_kind["candidate"])[0] == "new hypoth candidate (S-group 1 2)"
    assert first.outcome == "candidate" and first.outcome_form == "(S-group 1 2)"
    assert first.closed and first.rounds == 4
    assert rf.outcome_text(first) == "candidate (S-group 1 2)"
    assert rf.episode_title(first) == \
        "episode 1: (Countup 1) refuted at term 3 (1 where 3 was predicted)"


def test_the_operation_pictures_as_page_110(logs):
    model = model_at(logs[RUN3])
    card = next(c for c in model.episodes[0].cards if c.kind == "operation")
    assert [(g.name, [(x.text, x.name) for x in g.groups], g.state) for g in card.before] == [
        ("gnoth1", [("1", "glint1")], "live"), ("gnoth2", [("2", "glint2")], "live"),
        ("gnoth3", [("1", "glint3")], "live")]
    assert [(g.name, [(x.text, x.name) for x in g.groups], g.state) for g in card.after] == [
        ("gnoth1", [], "gone"), ("gnoth2", [("1", "glint1"), ("2", "glint2")], "live"),
        ("gnoth3", [("1", "glint3")], "live")]
    assert rf.picture_lines(card.before)[:2] == ["gnoth1: [ 1 ]", "          glint1"]
    assert rf.picture_lines(card.after)[:3] == [
        "gnoth1: [] (disappears)", "gnoth2: [ 1      2 ]", "          glint1 glint2"]
    assert rf.bracket_text(card.after[1]) == "[ 1 2 ]"
    assert rf.bracket_text(card.after[0]) == "[] (disappears)"
    assert "gnoth1 gone: emptied by a gnoth operation" in texts(card)
    assert "carried out" in texts(card)
    assert any(t.startswith("Gnoth-operator: 1 step of (SHIFT-RIGHT") and
               t.endswith("acted, threshold 7.0") for t in texts(card))
    assert set(card.names) >= {"gnoth1", "gnoth2"}


def test_operations_in_series_as_page_120(logs):
    model = model_at(logs[RUN3])
    columns, rows = rf.series_table(model.episodes[0])
    assert columns == ("gnoth1", "gnoth2", "gnoth3")
    assert rows == (("initial state", ("[ 1 ]", "[ 2 ]", "[ 1 ]")),
                    ("(SHIFT-RIGHT gnoth1 gnoth2 (glint1))",
                     ("[] (disappears)", "[ 1 2 ]", "[ 1 ]")))
    lines = rf.series_lines(model.episodes[0])
    assert lines[0].split() == ["gnoth1", "gnoth2", "gnoth3"]
    assert lines[1] == "initial state" and lines[3] == "(SHIFT-RIGHT gnoth1 gnoth2 (glint1))"
    assert lines[4].split("  ") [0] == "[] (disappears)"
    model.show_series = True
    model.folded.clear()
    pane = rf.pane_lines(model)
    assert [l.text for l in pane if l.kind == "series"][0].split() == ["gnoth1", "gnoth2", "gnoth3"]
    model.show_series = False
    assert all(l.kind != "series" for l in rf.pane_lines(model))


def test_a_picture_with_gloms_writes_the_glom_names_under_the_groups():
    pic = (rf.GnothPicture("gnoth2", (rf.Group("(1 1)", "glom3"), rf.Group("(2 2)", "glom5"))),
           rf.GnothPicture("gnoth3", (rf.Group("(3 3)", "glom7"),), "emerged"))
    lines = rf.picture_lines(pic)
    assert lines[0] == "gnoth2: [ (1 1) (2 2) ]"                            # [M86 p. 110]
    assert lines[1] == "          glom3 glom5"
    assert lines[1].index("glom3") == lines[0].index("(1 1)")
    assert lines[1].index("glom5") == lines[0].index("(2 2)")
    assert lines[2] == "gnoth3: [ (3 3) ] (emerges)"                        # [M86 p. 120]
    assert rf.bracket_text(pic[1]) == "[ (3 3) ] (emerges)"
    assert rf.bracket_text(rf.GnothPicture("gnoth4", (), "uncapped")) == "[ ] (uncapped)"


# -- run 2 [M86 pp. 168-170] ---------------------------------------------------------------------------

def test_run_2_has_one_episode_as_pages_168_to_170(logs):
    model = model_at(logs[RUN2])
    assert len(model.episodes) == 1
    episode = model.episodes[0]
    assert (episode.form, episode.n, episode.term, episode.predicted) == \
        ("(Cycle 3 2 (3 7))", 4, 3, 7)
    assert kinds(episode) == ["refutation", "freeze-dried", "gnoths-set", "slip-check",
                              "moves", "assessor", "casts", "candidate"]
    cards = dict(zip(kinds(episode), episode.cards))
    assert texts(cards["freeze-dried"])[0] == \
        "(Cycle 3 2 (3 7)) (3 terms) kept on the list of old hypotheses"
    assert "gnoths: (gnoth1 gnoth2 gnoth3)" in texts(cards["gnoths-set"])         # [M86 p. 168]
    assert texts(cards["gnoths-set"])[:3] == [
        "gnoth1  frame 0  range (1 2)  glom glom4  kept",
        "gnoth2  frame 1  range (3 3)  glom glom5  new",
        "gnoth3  frame 2  range (4 4)  glom glom6  new"]
    check = texts(cards["slip-check"])
    assert check[0] == "slip-check: stayval: 0.0" and "        best: nil" in check   # [M86 p. 168]
    assert check[-1] == "We will stay with the reigning class -- Cycle."
    assert texts(cards["moves"]) == ["threshold 0.0 in Cycle",
                                     "no actions to be taken: a Bond-assessor follows"]
    assert texts(cards["casts"])[0] == \
        "casts: ((Cycle 3 1 (3)) (Cycle 7 1 (7)) (Cycle 3 1 (3)))"
    assert texts(cards["candidate"])[0] == "new hypoth candidate (Cycle 3 3 (3 7 3))"
    assert model.episodes[0].outcome == "candidate"
    assert rf.series_table(episode) == ((), ()) and rf.series_lines(episode) == []


def test_run_1_has_no_episode(logs):
    model = model_at(logs[RUN1])
    assert model.episodes == [] and model.cards() == [] and model.episode is None
    assert model.first_refutation is None
    assert [l.text for l in rf.pane_lines(model)] == ["no refutation yet"]
    assert rf.freeze_dried_lines(model) == [] and rf.threshold_points(None) == ()


# -- the freeze-dried list and the threshold [M86 pp. 91, 105, 179] ------------------------------------

def test_the_freeze_dried_list_equals_the_snapshots(logs):
    log = logs[RUN3]
    model = model_at(log)
    assert model.freeze_dried_list() == (("(S-group 1 2)", 4), ("(Countup 1)", 2))    # [M86 p. 179]
    assert rf.freeze_dried_lines(model) == ["(S-group 1 2) (4 terms)", "(Countup 1) (2 terms)"]
    model = rf.ReformulationModel()
    seen = None
    for i, event in enumerate(log.events):
        model.on_event(event, i)
        snapshot = log.state_at(i)
        if event.kind == "freeze-dried":
            assert model.freeze_dried[-1] == (event.form, event.explained)
        model.update(snapshot)
        if snapshot is not None and snapshot is not seen:     # a new task boundary
            seen = snapshot
            assert model.freeze_dried == \
                [(r.form, r.terms_explained) for r in snapshot.reformulation.freeze_dried]
            assert model.reigning == snapshot.reformulation.reigning
            assert model.threshold == snapshot.reformulation.threshold
    assert model.first_refutation == 118


def test_the_threshold_course_within_an_episode(logs):
    model = model_at(logs[RUN3])
    first, second = model.episodes
    assert [v for _, v in rf.threshold_points(first)] == [8.0, 7.0, 8.0, 9.0, 10.0]
    assert [v for _, v in rf.threshold_points(second)] == [8.0, 9.0, 8.0, 9.0, 10.0]
    clocks = [c for c, _ in rf.threshold_points(first)]
    assert clocks == sorted(clocks) and first.clock_end >= clocks[-1]
    assert second.thresholds[0][0] > first.clock_end
    assert first.armed and second.armed


# -- the failure, the Slip-scout, a refused operation, the blast, a late Capper -------------------------

def test_failures_a_slip_scout_a_refused_operation_and_the_blast_on_blackburn_8(logs):
    model = model_at(logs["blackburn-8"])
    assert [e.outcome for e in model.episodes] == ["blast", "candidate"]
    first = model.episodes[0]
    failures = [c for c in first.cards if c.kind == "failure"]
    assert failures and texts(failures[0])[0].startswith("the reformulation effort has failed: ")
    assert texts(failures[0])[1].endswith("a Slip-scout is set out")
    assert texts(failures[-1])[1].endswith("the blast follows")
    after = first.cards[first.cards.index(failures[0]) + 1]
    assert after.kind == "slip-check" and texts(after)[-1].startswith("Slip-scout: x0.5")
    refused = [c for c in model.cards() if c.kind == "operation" and c.reason]
    assert refused and any(t.startswith("not carried out: ") for t in texts(refused[0]))
    assert refused[0].after is not None
    blast = first.cards[-1]
    assert blast.kind == "blast" and texts(blast)[0] == "blasting away all gnoths and gloms"
    assert texts(blast)[1].startswith("gnoths destroyed: gnoth")
    assert texts(blast)[2].startswith("gloms burst: glom")
    assert rf.outcome_text(first) == "the blast"
    assert model.reigning is not None or model.episodes[-1].closed


def test_a_late_capper_gets_a_card_of_its_own_on_blackburn_5(logs):
    model = model_at(logs["blackburn-5"])
    late = [c for c in model.cards() if c.operation and c.operation.startswith("(CAPPER ")]
    assert late and late[0].kind == "operation" and late[0].after is not None
    assert texts(late[0])[-1].startswith("Capper: ") and "recapped with glom" in texts(late[0])[-1]
    uncapped = [c for c in model.cards() if any(" uncapped over (" in t for t in texts(c))]
    assert uncapped and uncapped[0].kind == "operation"


# -- synthetic events --------------------------------------------------------------------------------

def start():
    return RunStarted(seed=1, config=tuple((f, getattr(Config(), f)) for f in
                                           ("s_links", "threshold_fraction")))


def test_an_event_with_no_episode_begins_one_without_a_refutation():
    model = rf.ReformulationModel()
    model.on_event(start())
    assert model.slipperiness("S-group", "Y-group") == 0.4           # DECISION D-038
    model.on_event(Reformulated(kind_="S-group", threshold=8.0, program=None))
    assert len(model.episodes) == 1 and model.episodes[0].form is None
    assert rf.episode_title(model.episodes[0]) == "episode 1: begun without a refutation"
    assert kinds(model.episodes[0]) == ["moves"]
    model.on_event(BondsAssessed(kind_="S-group", threshold=8.0, strongest=10, more=False))
    model.on_event(CastsMade(kind_="S-group", gnoths=("gnoth1",), casts="((S-group 1 2))",
                             generalised=None, last=False, reason="no generalisation"))
    model.on_event(CastFailed(reason="the casts did not generalise", failures=1, blast=False))
    model.on_event(SlipScouted(before="S-group", after="S-group", factor=0.5, stayval=5.0))
    assert kinds(model.episodes[0]) == ["moves", "assessor", "casts", "failure", "slip-check"]
    assert texts(model.episodes[0].cards[2])[1] == "no form: no generalisation"
    assert model.episodes[0].outcome == "failure" and not model.episodes[0].closed
    assert rf.outcome_text(model.episodes[0]) == "failed, a Slip-scout"
    model.on_event(HypothesisRefuted(form="(S-group 1 2)", n=5, term=3, predicted=1))
    assert len(model.episodes) == 2 and not model.is_open(1) and model.is_open(2)
    model.on_event(HypothesisFreezeDried(form="(S-group 1 2)", explained=4, reigning="S-group"))
    assert model.freeze_dried == [("(S-group 1 2)", 4)] and model.threshold is None
    model.on_event(GnothOperation(operation="(NO-OP gnoth2)", gnoths=("gnoth2",), gone=(),
                                  created=("gnoth9",), made=(), uncapped=("gnoth2",),
                                  reason="gnoth2 is gone"))
    card = model.episodes[1].last()
    assert card.kind == "operation" and card.reason == "gnoth2 is gone"
    assert texts(card) == ["not carried out: gnoth2 is gone", "gnoth9 emerges"]
    assert card.before == () and card.after == ()
    model.toggle_fold(1)
    assert model.is_open(1)
    model.toggle_fold(1)
    assert not model.is_open(1)
    lines = rf.pane_lines(model)
    assert lines[0].kind == "episode" and lines[0].collapsed and lines[0].episode == 1
    assert lines[0].text.endswith("—  failed, a Slip-scout")
    assert lines[1].kind == "episode" and not lines[1].collapsed
    assert lines[-1].text == "outcome: in progress"
    assert model.card_of(1) is model.episodes[0].cards[0] and model.card_of(99) is None
    model.on_event(start())
    assert model.episodes == [] and model.freeze_dried == [] and model.folded == set()


def test_every_card_cites_its_pages_and_has_a_title():
    for kind in rf.CARD_KINDS:
        assert kind in rf.CARD_PAGES and kind in rf.CARD_TITLES
        assert rf.CARD_PAGES[kind].startswith("[M86 p")
    assert rf.CARD_PAGES["blast"] == "[M86 p. 92]"
    assert rf.CARD_PAGES["operation"] == "[M86 pp. 109-120, 124]"
    assert len(rf.KINDS) == 20 and len(set(rf.KINDS)) == 20

"""Loop0004 item 09: the hypothesis pane's model (`seekwhence/gui/models/hypothesis.py`),
Qt-free (G-033, G-034, G-035).

- The box tree of `(C-group (S-group 2 3) 2)` matches Figure 3's structure: five boxes,
  `pstruc1` a C-group fed by box1 with n-val box2 and k-val box5, `pstruc2` an S-group
  with n-val box3 and k-val box4, three Constants [M86 p. 83].
- `build-box` of p. 161 hit four times on the private box gives `(1)`, `(2 2)`,
  `(3 3 3)`, `(4 4 4 4)` [M86 p. 161]; reset starts over.
- The run's own box is untouched: `covered`, the box values and the draws are the same
  with the model observing, predicting, drawing the tree and hitting a private box.
- Validity goes 0 -> 1 -> 0 at run 3's refutations [M86 pp. 174-176], and agrees with
  the snapshot at every event of the three appendix runs (no corrections).
- The form and its source [M86 pp. 79, 125, 166]; the predictions table with the
  refuting term [M86 pp. 168, 176]; the oval tree of Figures 1-2 with its first hits
  and its boxes' real names [M86 pp. 16-17, 83]; the positions an oval gives; the
  candidates and their postdiction [M86 pp. 125, 174]; the lines; a start resets.
"""

import io

import pytest

from seekwhence import bench
from seekwhence.boxes import BoxTree, build_box
from seekwhence.config import Config
from seekwhence.events import (
    CandidateMade, CastFailed, CastsMade, GuessAnswered, GuessMade, HypothesisMade,
    HypothesisRefuted, RunStarted, TermEntered, TermFiltered)
from seekwhence.forms import parse
from seekwhence.gui.models import hypothesis as hy
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.snapshot import tree_records
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX, run_plain

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)


def observed(run_id, seed):
    """The run with the tap; returns (log, session)."""
    log = EventLog()
    tap = Tap(log)
    spec = bench.run_spec(bench.load_expectations(), run_id)
    session = Session(Config(verbose=spec["prints_task_lines"]), seed, out=io.StringIO(),
                      lines=bench.inputs_of(spec), answers=bench.answers_of(spec),
                      observers=[tap])
    tap.attach_session(session)
    session.run()
    return log, session


def model_at(log, index):
    """The model fed the events up to `index` with the snapshots in force (G-033)."""
    model = hy.HypothesisModel()
    for i in range(index + 1):
        model.on_event(log.events[i])
        model.update(log.state_at(i))
    return model


def indexes(events, kind):
    return [i for i, e in enumerate(events) if e.kind == kind]


@pytest.fixture(scope="module")
def runs():
    return {run_id: observed(run_id, seed) for run_id, seed in APPENDIX}


# ---- the box tree [M86 p. 83] -----------------------------------------------------------------

def test_the_box_tree_of_figure_3():
    boxes, pstrucs = hy.fresh_records("(C-group (S-group 2 3) 2)")
    assert [hy.box_line(b) for b in boxes] == [
        f"box{i} printstruc: pstruc{i} pstruc-val: - ready: false" for i in range(1, 6)]
    assert [hy.pstruc_line(p) for p in pstrucs] == [
        "pstruc1 type: C-group boxes: (box1) n-val: box2 k-val: box5",
        "pstruc2 type: S-group boxes: (box2) n-val: box3 k-val: box4",
        "pstruc3 type: Constant boxes: (box3) value: 2",
        "pstruc4 type: Constant boxes: (box4) value: 3",
        "pstruc5 type: Constant boxes: (box5) value: 2",
    ]
    assert (boxes, pstrucs) == tree_records(BoxTree(parse("(C-group (S-group 2 3) 2)")))
    assert "pstruc-val: (empty)" in hy.box_tooltip(boxes[0])
    assert hy.pstruc_tooltip(pstrucs[0]).split("\n")[:5] == [
        "pstruc1", "type: C-group", "boxes: (box1)", "n-val: box2", "k-val: box5"]
    assert hy.fresh_records(None) == ((), ()) and hy.fresh_records("(nonsense") == ((), ())


def test_the_records_after_a_hit_carry_the_values():
    tree = BoxTree(parse("(Cycle 3 2 (3 7))"))
    tree.hit()
    boxes, pstrucs = tree_records(tree)
    assert hy.box_line(boxes[0]) == "box1 printstruc: pstruc1 pstruc-val: 3 ready: true"
    assert hy.box_line(boxes[2]) == "box3 printstruc: pstruc3 pstruc-val: - ready: false"
    assert hy.pstruc_line(pstrucs[0]) == "pstruc1 type: Cycle boxes: (box1) elements: (box2 box3)"
    assert "turn: 1" in hy.pstruc_tooltip(pstrucs[0])


# ---- the private box [M86 p. 161] --------------------------------------------------------------

def test_build_box_of_p161_hit_four_times_on_the_private_box():
    box = hy.PrivateBox("(C-group (same pstruc1) (same pstruc1))", {"pstruc1": "(Countup 1)"})
    assert box.text == "(C-group (Countup 1) shared)"
    assert [box.hit() for _ in range(4)] == ["(1)", "(2 2)", "(3 3 3)", "(4 4 4 4)"]
    assert box.hits == ["(1)", "(2 2)", "(3 3 3)", "(4 4 4 4)"]
    boxes, pstrucs = box.records()
    assert [b.name for b in boxes] == ["box1", "box2", "box3", "box4"]
    assert hy.pstruc_line(pstrucs[1]) == "pstruc2 type: Countup boxes: (box2 box4) n-val: box3"
    box.reset()
    assert box.hits == [] and box.hit() == "(1)"
    # the same form in the prose spelling, through the model
    model = hy.HypothesisModel()
    model.open_private("(C-group (Countup 1) shared)")
    assert [model.private_hit() for _ in range(4)] == ["(1)", "(2 2)", "(3 3 3)", "(4 4 4 4)"]
    model.private_reset()
    assert model.private.hits == []
    with pytest.raises(ValueError):
        model.open_private()            # no form in force, nothing given


def test_the_runs_own_box_is_untouched(runs):
    log, session = runs[RUN3]
    hypothesis = session.world.hypothesis
    before = (hypothesis.covered, hypothesis.validity, list(hypothesis.pending),
              [(b.name, b.pstruc_val, b.ready) for b in hypothesis.box.boxes],
              session.world.rng.draws)
    model = model_at(log, len(log.events) - 1)
    model.predictions()
    model.tree()
    model.select_node(())
    model.highlighted()
    assert model.private_hit() == "(1 2)" and model.private_hit() == "(1 2 3)"
    assert hy.postdicts_terms(model.form, model.terms) is True
    assert hy.pane_lines(model)
    after = (hypothesis.covered, hypothesis.validity, list(hypothesis.pending),
             [(b.name, b.pstruc_val, b.ready) for b in hypothesis.box.boxes],
             session.world.rng.draws)
    assert before == after
    # and the observed run is the plain run
    plain = run_plain(RUN3, SEEDS[RUN3])
    assert [e for e in log.events] == plain.events and session.world.rng.draws == plain.draws


# ---- validity [M86 pp. 81, 168, 174-176] ---------------------------------------------------------

def test_validity_goes_0_1_0_at_run_3s_refutations(runs):
    log, _ = runs[RUN3]
    events = log.events
    made, candidates = indexes(events, "hypothesis"), indexes(events, "candidate")
    refuted = indexes(events, "refuted")
    filtered_ok = [i for i in indexes(events, "filtered") if events[i].ok]
    assert model_at(log, made[0]).validity == 0                   # (Countup 1) [M86 p. 171]
    assert model_at(log, refuted[0]).validity == 0                # term 3 refutes it
    model = model_at(log, candidates[0])
    assert model.validity == 0 and model.form == "(S-group 1 2)"  # [M86 p. 174]
    assert model_at(log, filtered_ok[0]).validity == 1            # term 4 predicted
    model = model_at(log, refuted[1])
    assert model.validity == 0 and model.refuted                  # term 5 (3) [M86 p. 176]
    assert model.refutation == (5, 3, 1)
    assert model_at(log, candidates[1]).validity == 0
    model = model_at(log, len(events) - 1)
    assert model.validity == 1 and model.form == "(S-group 1 (Countup 2))"
    assert model.threshold == 1
    # a refused guess zeroes the validity at the next event [M86 pp. 175-176]
    answered = indexes(events, "answered")[0]
    assert not events[answered].accepted
    assert model_at(log, answered).validity == 1
    assert model_at(log, answered + 1).validity == 0


def test_validity_agrees_with_every_snapshot(runs):
    for run_id, (log, _) in runs.items():
        model = hy.HypothesisModel()
        for i, event in enumerate(log.events):
            model.on_event(event)
            model.update(log.state_at(i))
            snapshot = log.state_at(i)
            if log.snapshot_index(i) == i and snapshot.hypothesis is not None:
                assert snapshot.hypothesis.form == model.form, (run_id, i)
                assert snapshot.hypothesis.validity == model.validity, (run_id, i)
                assert model.box_confirmed
                assert model.structure_text() == snapshot.hypothesis.structure_text
        assert model.corrections == 0, run_id


def test_the_bar_fractions():
    assert hy.validity_fraction(0, 1) == 0.0 and hy.threshold_fraction(0, 1) == 1.0
    assert hy.validity_fraction(1, 1) == 1.0
    assert hy.validity_fraction(1, 2) == 0.5 and hy.threshold_fraction(1, 2) == 1.0
    assert hy.validity_fraction(4, 2) == 1.0 and hy.threshold_fraction(4, 2) == 0.5
    assert hy.validity_fraction(0, 0) == 0.0


# ---- the form, its source and the box's names [M86 pp. 79, 83, 125, 166] -------------------------

def test_run_1s_form_comes_from_the_template(runs):
    log, _ = runs[RUN1]
    made = indexes(log.events, "hypothesis")[0]
    model = model_at(log, made)
    assert model.form == "(Cycle 3 2 (3 7))" and model.source == "template"    # [M86 p. 166]
    assert hy.source_text(model.source) == "from the template [M86 p. 79]"
    assert not model.box_confirmed                   # a fresh tree until the next snapshot
    assert [b.name for b in model.boxes] == ["box1", "box2", "box3"]
    assert hy.pane_lines(hy.HypothesisModel()) == ()
    model.show_box = True
    lines = hy.pane_lines(model)
    assert lines[0].text == "box tree [M86 p. 83] (provisional names until the next task)"
    assert lines[0].grey and lines[1].grey
    # the next snapshot brings the run's own names
    after = model_at(log, log.snapshot_index(len(log.events) - 1))
    assert after.box_confirmed and after.boxes[0].name != "box1"
    assert after.structure_text() == log.last_snapshot.hypothesis.structure_text
    assert hy.source_text("fallback") == "the Tuple fall-back [M86 p. 79]"
    assert hy.source_text("candidate", "S-group") == "a candidate cast under S-group [M86 p. 125]"
    assert hy.source_text(None) == ""


def test_run_3_ends_on_a_candidate_with_the_real_box_names(runs):
    log, _ = runs[RUN3]
    model = model_at(log, len(log.events) - 1)
    assert model.form == "(S-group 1 (Countup 2))" and model.source == "candidate"
    assert model.kind_ == "S-group"
    assert model.structure_text().split("\n")[0] == "box15 printstruc: pstruc15"
    assert hy.pstruc_line(model.pstrucs[0]) == (
        "pstruc15 type: S-group boxes: (box15) n-val: box16 k-val: box17")
    nodes = model.tree()
    assert [(n.label, model.box_name(n)) for n in nodes] == [
        ("S-group", "box15"), ("1", "box16"), ("Countup", "box17"), ("2", "box18")]
    assert hy.node_tooltip(nodes[0], "box15").split("\n") == [
        "(S-group 1 (Countup 2))", "gives (1 2) (1 2 3) (1 2 3 4)", "box: box15 [M86 p. 83]"]


# ---- the predictions table [M86 pp. 168, 176] --------------------------------------------------

def test_the_predictions_at_run_1s_end(runs):
    log, _ = runs[RUN1]
    model = model_at(log, len(log.events) - 1)
    cells = model.predictions()
    assert [c.predicted for c in cells] == [3, 7, 3, 7, 3, 7, 3]
    assert [c.term for c in cells] == [3, 7, 3, 7, None, None, None]
    assert [c.state for c in cells] == ["agrees"] * 4 + ["beyond"] * 3


def test_the_refuting_term_and_its_prediction(runs):
    log, _ = runs[RUN3]
    refuted = indexes(log.events, "refuted")
    cells = model_at(log, refuted[1]).predictions()
    assert cells[4] == hy.PredictionCell(5, 1, 3, "refuting")          # [M86 p. 176]
    assert [c.state for c in cells[:4]] == ["agrees"] * 4
    cells = model_at(log, refuted[0]).predictions()
    assert cells[2] == hy.PredictionCell(3, 3, 1, "refuting")          # (Countup 1) [M86 p. 172]
    # the candidate that follows clears the refutation: every term agrees again
    candidate = next(i for i in indexes(log.events, "candidate") if i > refuted[1])
    cells = model_at(log, candidate).predictions()
    assert [c.state for c in cells[:5]] == ["agrees"] * 5 and cells[5].state == "beyond"
    assert hy.predictions(None, [1, 2]) == (
        hy.PredictionCell(1, None, 1, "unknown"), hy.PredictionCell(2, None, 2, "unknown"))
    assert hy.predictions(None, []) == ()


# ---- the oval tree [M86 pp. 16-17, 50-53] --------------------------------------------------------

def test_the_oval_tree_of_figure_1():
    nodes = hy.form_tree("(S-group 2 3)")
    assert [(n.kind, n.label, n.edge, n.box) for n in nodes] == [
        ("form", "S-group", None, 0), ("int", "2", "start", 1), ("int", "3", "length", 2)]
    assert nodes[0].hits == "(2 3 4) (2 3 4) (2 3 4)"                 # [M86 p. 16]
    assert nodes[0].children == ((0,), (1,))
    assert nodes[1].y == hy.Y_STEP and nodes[0].y == 0
    assert nodes[0].x == (nodes[1].x + nodes[2].x) / 2
    cycle = hy.form_tree("(Cycle 8 3 (8 0 3))")
    assert [n.label for n in cycle] == ["Cycle", "8", "0", "3"]
    assert cycle[0].hits == "8 0 3"                                   # [M86 p. 16]
    assert all(n.edge == "element" for n in cycle[1:])


def test_a_shared_structure_is_one_node_joined_to_its_uses():
    nodes = hy.form_tree("(Tuple 1 3 ((Countup 1) 8 shared))")        # [M86 p. 162]
    assert [(n.kind, n.label, n.box) for n in nodes] == [
        ("form", "Tuple", 0), ("form", "Countup", 1), ("int", "1", 2), ("int", "8", 3),
        ("shared", "shared", 4)]
    assert nodes[4].shares == (0,) and nodes[0].hits == "(1 8 1) (2 8 2) (3 8 3)"
    assert nodes[1].hits == "1"
    assert "shares the structure" in hy.node_tooltip(nodes[4], "box5")
    nested = hy.form_tree("(C-group (S-group 2 3) (shared 0 0))")      # [M86 p. 84]
    assert nested[-1].kind == "shared" and nested[-1].shares == (0, 0)
    assert hy.form_tree(None) == () and hy.form_tree("(nonsense") == ()


def test_the_positions_an_oval_gives():
    text = "(Cycle 3 2 (3 7))"
    nodes = hy.form_tree(text)
    ranges = hy.node_ranges(text, nodes, 6)
    assert ranges[()] == tuple((i, i) for i in range(1, 7))
    assert ranges[(0,)] == ((1, 1), (3, 3), (5, 5)) and ranges[(1,)] == ((2, 2), (4, 4), (6, 6))
    text = "(Tuple 1 3 ((Countup 1) 8 shared))"
    nodes = hy.form_tree(text)
    ranges = hy.node_ranges(text, nodes, 2)
    assert ranges[()] == ((1, 3), (4, 6))
    assert ranges[(0,)] == ((1, 1), (4, 4)) and ranges[(2,)] == ((3, 3), (6, 6))
    assert ranges[(0, 0)] == ()                       # a parameter gives no term
    text = "(S-group 1 (Countup 2))"
    ranges = hy.node_ranges(text, hy.form_tree(text), 3)
    assert ranges[()] == ((1, 2), (3, 5), (6, 9)) and ranges[(1,)] == ()
    model = hy.HypothesisModel()
    model.on_event(RunStarted(1, ()))
    model.on_event(HypothesisMade("(Cycle 3 2 (3 7))", "template"))
    assert model.select_node((1,)) == (2, 4, 6, 8, 10, 12) and model.selected == (1,)
    assert model.highlighted() == (2, 4, 6, 8, 10, 12)
    assert model.select_node((1,)) == () and model.selected is None
    assert model.select_node(()) == tuple(range(1, 13))
    assert model.select_node(None) == ()


# ---- the candidates [M86 pp. 125, 174] ------------------------------------------------------------

def test_run_3s_casts_and_candidates(runs):
    log, _ = runs[RUN3]
    events = log.events
    casts, candidates = indexes(events, "casts"), indexes(events, "candidate")
    model = model_at(log, casts[0])
    assert model.candidates[0].postdicts is None
    assert model.candidate_lines() == (
        "casts ((S-group 1 2) (S-group 1 1)) under S-group --> (S-group 1 2): postdiction pending",)
    model = model_at(log, candidates[0])
    assert model.candidate_lines() == (
        "casts ((S-group 1 2) (S-group 1 1)) under S-group --> (S-group 1 2) postdicts 1 2 1: "
        "the new hypothesis",)                                        # [M86 p. 174]
    assert model.candidates[0].terms == (1, 2, 1)
    assert hy.postdicts_terms("(S-group 1 2)", (1, 2, 1)) is True
    assert hy.postdicts_terms("(S-group 1 2)", (1, 2, 1, 2, 3)) is False
    assert hy.postdicts_terms(None, (1,)) is None
    # the second refutation starts a new episode
    assert model_at(log, indexes(events, "refuted")[1]).candidates == []
    model = model_at(log, len(events) - 1)
    assert len(model.candidates) == 1 and model.candidates[0].form == "(S-group 1 (Countup 2))"
    assert model.freeze_dried == ("(S-group 1 2)", 4)
    assert model.guess == ("(S-group 1 (Countup 2))", "(1 2)(1 2 3)(1 2 3 4)", "ok", True)


def test_a_failed_cast_and_a_cast_with_no_form_on_synthetic_events():
    model = hy.HypothesisModel()
    for event in (RunStarted(7, (("guess_validity", 2),)), TermEntered(1, 1, 0), TermEntered(2, 2, 0),
                  TermEntered(3, 1, 0), HypothesisMade("(Countup 1)", "template"),
                  TermFiltered(3, 1, "(Countup 1)", False, 3),
                  HypothesisRefuted("(Countup 1)", 3, 1, 3),
                  CastsMade("S-group", ("gnoth2", "gnoth3"), "((S-group 1 2) (S-group 1 1))",
                            "(S-group 1 3)", True, None),
                  CastFailed("(S-group 1 3) does not postdict the terms", 1, False),
                  CastsMade("S-group", ("gnoth2", "gnoth3"), "((S-group 1 2) (S-group 1 1))",
                            None, True, "the casts did not generalise"),
                  CandidateMade("(S-group 1 2)", "S-group")):
        model.on_event(event)
    assert model.threshold == 2
    assert model.candidate_lines() == (
        "casts ((S-group 1 2) (S-group 1 1)) under S-group --> (S-group 1 3): "
        "(S-group 1 3) does not postdict the terms",
        "casts ((S-group 1 2) (S-group 1 1)) under S-group: no form (the casts did not generalise)",
        "casts  under S-group --> (S-group 1 2) postdicts 1 2 1: the new hypothesis")
    assert [c.postdicts for c in model.candidates] == [False, False, True]
    lines = hy.pane_lines(model)
    assert [(line.kind, line.bad, line.good) for line in lines] == [
        ("head", False, False), ("candidate", True, False), ("candidate", True, False),
        ("candidate", False, True)]
    assert model.form == "(S-group 1 2)" and model.source == "candidate" and not model.refuted


# ---- the lines, the guess and a start ---------------------------------------------------------

def test_the_lines_and_a_start_reset(runs):
    log, _ = runs[RUN1]
    model = model_at(log, len(log.events) - 1)
    lines = hy.pane_lines(model)
    assert lines[0].text == "guess: 3 7 3 7  (answered ok: accepted) [M86 p. 166]" and lines[0].good
    assert all(line.kind != "box" for line in lines)
    model.show_box = True
    model.open_private()
    model.private_hit()
    lines = hy.pane_lines(model)
    kinds = [line.kind for line in lines]
    assert kinds.count("box") == 6 and kinds.count("pstruc") == 6       # the run's and the private
    assert "private box of (Cycle 3 2 (3 7)) [M86 p. 161]" in [line.text for line in lines]
    assert next(line for line in lines if line.kind == "private").text == "show-box 1: 3"
    box_line = next(line for line in lines if line.kind == "box")
    assert box_line.name == model.boxes[0].name and box_line.tooltip.startswith(box_line.name)
    model.on_event(GuessMade("(Cycle 3 2 (3 7))", "3 7 3 7", (3, 7, 3, 7)))
    assert hy.pane_lines(model)[0].text.endswith("(waiting for the answer) [M86 p. 166]")
    model.on_event(GuessAnswered("no", False))
    assert hy.pane_lines(model)[0].bad
    model.on_event(RunStarted(54, ()))
    assert model.form is None and model.private is None and model.candidates == []
    assert model.validity == 0 and model.boxes == () and model.tree() == ()
    assert model.show_box                             # the toggle is the user's, kept
    assert hy.pane_lines(model) == (hy.HLine("head", "box tree [M86 p. 83]"),
                                    hy.HLine("note", "no hypothesis, no box"))


def test_the_private_box_agrees_with_the_engines_box():
    tree = build_box("(Cycle 3 2 (3 7))")
    box = hy.PrivateBox("(Cycle 3 2 (3 7))")
    assert [box.hit() for _ in range(5)] == ["3", "7", "3", "7", "3"]
    assert [str(tree.hit()) for _ in range(5)] == box.hits

"""Loop0004 item 08: the socratoplasm pane's model (`seekwhence/gui/models/socratoplasm.py`),
Qt-free (G-030, G-031, G-032).

- The names, frames, caps and ranges of the gnoths, the catchall and the parse equal the
  engine's, read by an independent observer, at every event of the three appendix runs
  and of blackburn-1, -5 and -8 (operations, uncapping, blasts), outside a running gnoth
  operation; the fields of every confirmed card equal `Gnoth.show_lines()` at every
  event; the snapshot never has to correct the cards.
- The parse equals `show-parse` at every prompt of run 3, `((1 2) (1))` and
  `((1 2) (1 2 3))` as the transcript prints them [M86 pp. 174, 178].
- The `gnoths:` block equals `Session.show_lines()`'s at the end of each run and the
  snapshot's at every snapshot; `card_fields` rebuilds every record's fields.
- Run 1's gnoth [M86 p. 168] and its catchall [M86 p. 98]; run 3's cards after the
  reformulations [M86 pp. 175, 179]; the template of run 1 with its check line
  [M86 p. 167] and of run 3 with none [M86 p. 171]; the fates on synthetic events
  [M86 pp. 74-75]; a gnoth gone fades; the selection; the boxes over the columns
  [M86 p. 102]; the lines and texts; a start resets.
"""

import io

import pytest

from seekwhence import bench, events
from seekwhence import gnoths as gn
from seekwhence import sequences as sq
from seekwhence.config import Config
from seekwhence.gui.models import commands
from seekwhence.gui.models import cytoplasm as cy
from seekwhence.gui.models import socratoplasm as so
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.selection import Selection
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX, appendix_inputs

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)
# Longer runs with gnoth operations, uncapping and recapping, and blasts [M86 pp. 92,
# 110-113]: fed as the bench feeds them.
BLACKBURN = ("blackburn-1", "blackburn-5", "blackburn-8")
OPERATORS = ("Gnoth-operator", "Capper")


class GnothReader:
    """An independent observer: at every event, the parse, each gnoth's name, frame, cap,
    range and `show_lines()`, the catchall's, and the template's fields text, read from
    the `World` (attribute reads and the pure methods of ENGINE-API.md section 2)."""

    def __init__(self):
        self.world = None
        self.states = []

    def on_event(self, event):
        w = self.world
        if w is None:
            self.states.append(None)
            return
        gnoths = tuple((g.name, g.frame, g.glom.name if g.glom is not None else None,
                        tuple(g.range), tuple(g.show_lines())) for g in gn.gnoths(w))
        c = gn.catchall(w)
        catchall = None if c is None else (c.name, tuple(c.range), tuple(c.show_lines()))
        template = None if w.template is None else w.template.fields_text()
        self.states.append((gn.parse_text(w), gnoths, catchall, template))


def logged(run_id=None, sequence=None, seed=1, dense_tasks=None):
    """A run's `EventLog` with the tap's snapshots and the reader's states."""
    if sequence is None:
        config, lines, answers = appendix_inputs(run_id)
        seed = SEEDS[run_id]
        extra = {}
    else:
        config, lines, answers = Config(), [str(t) for t in sequence.prefix(16)], []
        extra = {"answerer": lambda console: bench.ContinuationAnswerer(console, sequence.terms)}
    log, reader = EventLog(), GnothReader()
    tap = Tap(log) if dense_tasks is None else Tap(log, dense_tasks=dense_tasks)
    session = Session(config, seed, out=io.StringIO(), lines=list(lines),
                      answers=list(answers), observers=[reader, tap], **extra)
    reader.world = session.world
    tap.attach_session(session)
    session.run()
    return log, reader, session


@pytest.fixture(scope="module")
def logs():
    return {run_id: logged(run_id) for run_id, _ in APPENDIX}


def walk(log, check=None, selection=None):
    """The model fed every event with the snapshot in force, as the view feeds it
    (G-030); `check(index, model)` after each."""
    model = so.SocratoplasmModel(selection)
    for i, event in enumerate(log.events):
        model.on_event(event)
        model.update(log.state_at(i))
        if check is not None:
            check(i, model)
    return model


def model_at(log, index, selection=None):
    model = so.SocratoplasmModel(selection)
    for i in range(index + 1):
        model.on_event(log.events[i])
        model.update(log.state_at(i))
    return model


def indexes(log, kind):
    return [i for i, e in enumerate(log.events) if e.kind == kind]


def settled_events(log):
    """The indexes at which the engine's gnoths are not mid-operation: outside a running
    Gnoth-operator or Capper, or at its `operation`, `operated`, `gnoth-capped` or final
    `gnoth-gone` event; and not the `cleaned` event just before a `blasted` one (G-030)."""
    out, running, settled = [], None, True
    for i, e in enumerate(log.events):
        if e.kind == "chosen":
            running, settled = e.type, e.type not in OPERATORS
        elif running in OPERATORS:
            settled = (e.kind in ("operation", "operated", "gnoth-capped")
                       or (e.kind == "gnoth-gone" and e.reason == "nothing left to cap"))
        nxt = log.events[i + 1].kind if i + 1 < len(log.events) else None
        if settled and not (e.kind == "cleaned" and nxt == "blasted"):
            out.append(i)
    return out


# -- exact at every event (G-030) -----------------------------------------------------------------

def exact_everywhere(log, reader, dense=True):
    settled = set(settled_events(log))
    confirmed_checked = 0

    def check(i, model):
        nonlocal confirmed_checked
        state = reader.states[i]
        if state is None:
            return
        parse, gnoths, catchall, template = state
        if i in settled:
            assert [(c.name, c.frame, c.glom, c.range) for c in model.gnoths()] == \
                [g[:4] for g in gnoths], (i, log.events[i])
            assert model.parse == parse, (i, log.events[i])
            mc = model.catchall
            assert (catchall is None) == (mc is None), (i, log.events[i])
            if catchall is not None:
                assert mc.name == catchall[0]
                if mc.confirmed:
                    assert mc.range == catchall[1]
            if template is not None:
                assert model.template is not None and model.template.fields_text == template, i
        for card in model.gnoths():
            if card.confirmed:
                lines = dict((g[0], g[4]) for g in gnoths).get(card.name)
                assert tuple(so.card_lines(card)) == lines, (i, log.events[i], card.name)
                confirmed_checked += 1
        if model.catchall is not None and model.catchall.confirmed and catchall is not None:
            assert tuple(so.card_lines(model.catchall)) == catchall[2], i

    model = walk(log, check)
    assert model.corrections == 0
    assert confirmed_checked > 0
    return model


@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_the_gnoths_and_the_parse_are_exact_at_every_event(logs, run_id):
    log, reader, _ = logs[run_id]
    exact_everywhere(log, reader)


@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_exact_with_keyframes_only(run_id):
    log, reader, _ = logged(run_id, dense_tasks=0)
    exact_everywhere(log, reader, dense=False)


@pytest.mark.parametrize("name", BLACKBURN)
def test_exact_on_a_run_with_operations_uncapping_and_blasts(name):
    log, reader, _ = logged(sequence=sq.find(name))
    kinds = {e.kind for e in log.events}
    if name == "blackburn-1":
        assert {"operation", "gnoth-uncapped", "gnoth-capped", "blasted"} <= kinds
    exact_everywhere(log, reader)


# -- the parse [M86 pp. 174, 178] -------------------------------------------------------------------

def test_the_parse_equals_show_parse_at_every_prompt_of_run_3(logs):
    log, _, _ = logs[RUN3]
    seen = []
    for i in indexes(log, "prompt"):
        model = model_at(log, i)
        assert model.parse == commands.show_parse(log.state_at(i))
        seen.append(model.parse)
    # the transcript's show-parse after the first reformulation, and after the second
    assert "((1 2) (1))" in seen            # [M86 p. 174]
    assert "((1 2) (1 2 3))" in seen        # [M86 p. 178]
    assert seen[0] == "()"
    assert seen[-1] == "((1 2) (1 2 3))"
    assert model_at(log, len(log.events) - 1).parse == "((1 2) (1 2 3))"


# -- the show block [M86 pp. 168, 179] -----------------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_the_show_block_equals_the_sessions_and_the_snapshots(logs, run_id):
    log, _, session = logs[run_id]
    model = model_at(log, len(log.events) - 1)
    lines = session.show_lines()
    assert model.show_block() == lines[lines.index("gnoths:"):]
    for i in log.snapshots:
        snapshot = log.state_at(i)
        shown = commands.show_lines(snapshot)
        model = model_at(log, i)
        assert model.show_block() == shown[shown.index("gnoths:"):]
        for record in snapshot.gnoths + ((snapshot.catchall,) if snapshot.catchall else ()):
            card = model.card(record.name)
            assert card.confirmed and so.card_fields(card) == record.fields


# -- the cards [M86 pp. 78, 98, 168, 175, 179] ---------------------------------------------------------

def test_run_1_ends_with_gnoth1_over_the_cycle_and_a_catchall(logs):
    log, _, _ = logs[RUN1]
    model = model_at(log, len(log.events) - 1)
    (gnoth,) = model.gnoths()
    assert so.card_lines(gnoth) == [
        "class: Gnoths", "name: gnoth1", "frame: 0", "plato-class: Cycle", "glom: glom3",
        "notes: nil", "form: (((Cycle 3 2 (3 7)) pure))", "state: stable", "range: (1 2)"]
    assert gnoth.members == ("glom1",) and gnoth.terms == (3, 7)
    assert gnoth.parse_print == "((3 7))" and model.parse == "(((3 7)))"
    assert so.head_text(gnoth) == "gnoth1  Cycle  frame 0"
    catchall = model.catchall
    assert catchall.name == "gnoth2" and catchall.catchall
    assert catchall.glints == ("glint3", "glint4") and catchall.range == (3, 4)
    assert catchall.terms == (3, 7)
    assert so.head_text(catchall) == "gnoth2  catchall"
    assert "repository for non-troublesome terms" in so.card_tooltip(catchall)
    assert model.show_block() == ["gnoths:", ""] + so.card_lines(gnoth)    # not listed


def test_the_gnoth_event_adds_the_card_before_the_next_snapshot(logs):
    log, _, _ = logs[RUN1]
    (i,) = indexes(log, "gnoth")
    model = model_at(log, i)
    assert log.snapshot_index(i) < i
    card = model.card("gnoth1")
    assert not card.confirmed and card.made_at == i + 1
    assert so.card_lines(card)[1:] == ["name: gnoth1", "frame: 0", "plato-class: Cycle",
                                       "glom: glom3", "notes: nil",
                                       "form: (((Cycle 3 2 (3 7)) pure))", "state: stable",
                                       "range: (1 2)"]
    assert "next one confirms" in so.card_tooltip(card)
    before = model_at(log, i - 1)
    assert before.gnoths() == ()


def test_run_3s_cards_after_the_reformulations(logs):
    log, _, _ = logs[RUN3]
    sets = indexes(log, "gnoths-set")
    assert len(sets) == 2
    # after the first Gnoth-setter and the shift, at the next prompt: gnoth2 over (1 2)
    # and gnoth3 over (1), both S-group [M86 pp. 174-175]
    prompt = next(i for i in indexes(log, "prompt") if i > sets[0])
    model = model_at(log, prompt)
    names = [c.name for c in model.gnoths()]
    assert names == ["gnoth2", "gnoth3"]
    gnoth2 = model.card("gnoth2")
    fields = dict(gnoth2.fields)
    assert fields["plato-class"] == "S-group" and fields["range"] == "(1 2)"
    assert fields["form"] == "(S-group 1 2)" and fields["equivalence-type"] == "parse"
    # the printout has `((bond2 0) (bond3 -5))` [M86 p. 175]; the reconstruction's bonds
    # were made in the other order on this seed, so the names differ and the values agree
    assert sorted(value for _name, value in gnoth2.external_bonds) == [-5, 0]
    assert {name for name, _value in gnoth2.external_bonds} == {"bond2", "bond3"}
    assert fields["internal-bonds"] == "((bond1 10))" and fields["groups"] == "nil"
    assert gnoth2.frame == 1 and gnoth2.glom == "glom7"      # glom9 in the printout
    assert model.card("gnoth1") is None     # gone and pruned [M86 p. 120]
    # the end: gnoth3 over (1 2 3), form (S-group 1 3) [M86 p. 179]
    end = model_at(log, len(log.events) - 1)
    gnoth3 = end.card("gnoth3")
    fields = dict(gnoth3.fields)
    assert fields["form"] == "(S-group 1 3)" and fields["range"] == "(3 5)"
    assert fields["internal-bonds"] == "((bond8 10) (bond5 10))"
    assert end.reigning == "S-group" and end.catchall.name == "gnoth6"


def test_a_gnoth_gone_fades_as_a_ghost_then_is_pruned(logs):
    log, _, _ = logs[RUN3]
    gone = indexes(log, "gnoth-gone")[0]
    assert log.events[gone].name == "gnoth1"
    model = model_at(log, gone)
    ghost = model.card("gnoth1")
    assert not ghost.alive and ghost.reason == "emptied by a gnoth operation"
    assert ghost not in model.gnoths() and ghost in model.ghosts()
    assert "gone:" in so.card_tooltip(ghost)
    box = next(b for b in so.gnoth_boxes(model) if b.name == "gnoth1")
    assert box.ghost == 1.0
    later = model_at(log, gone + cy.FADE // 2)
    box = next(b for b in so.gnoth_boxes(later) if b.name == "gnoth1")
    assert 0 < box.ghost < 1
    assert model_at(log, gone + cy.FADE + 1).card("gnoth1") is None


def test_the_catchall_goes_when_the_gnoths_are_set(logs):
    log, _, _ = logs[RUN3]
    second = indexes(log, "gnoths-set")[1]
    before = model_at(log, indexes(log, "freeze-dried")[1])
    assert before.catchall is not None and before.catchall.name == "gnoth4"
    after = model_at(log, second)
    assert after.catchall is None
    ghost = after.card("gnoth4")
    assert ghost is not None and not ghost.alive and ghost.reason.startswith("the catchall")


# -- the template [M86 pp. 73-76, 167, 171] -----------------------------------------------------------

def test_run_1s_template_with_its_check_line(logs):
    log, _, _ = logs[RUN1]
    (made,) = indexes(log, "template")
    model = model_at(log, made)
    t = model.template
    assert t.fields_text == "(form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)"
    assert t.check is None and so.fate_text(t) == "working"
    assert so.template_lines(t) == [f"template: {t.fields_text}", "fate: working"]
    (checked,) = indexes(log, "template-check")
    t = model_at(log, checked).template
    assert t.check == "check-cycle template (Cycle 3 2 ((3 7)))" and t.passed
    (accepted,) = indexes(log, "template-accepted")
    t = model_at(log, accepted).template
    assert t.state == "accepted" and so.fate_text(t) == "accepted"
    assert "state accepted" in t.fields_text and "glom glom2" in t.fields_text
    # the Gnoth-maker takes the template glom: `glom nil` from its first cap on
    (gnoth,) = indexes(log, "gnoth")
    t = model_at(log, gnoth - 1).template
    assert log.events[gnoth - 1].kind == "top-down" and t.glom is None
    assert t.fields_text.endswith("glom nil)")
    assert so.template_lines(t) == [f"template: {t.fields_text}",
                                    "check-cycle template (Cycle 3 2 ((3 7)))",
                                    "fate: accepted"]


def test_run_3s_template_prints_no_check(logs):
    log, _, _ = logs[RUN3]
    (checked,) = indexes(log, "template-check")
    t = model_at(log, checked).template
    assert t.form == "(S-group 1 2)" and t.immediate and t.check == "" and t.passed
    assert so.template_lines(t)[1] == "check: passed"


def test_the_fates_on_synthetic_events():
    model = so.SocratoplasmModel()
    model.on_event(events.TemplateMade("(S-group 1 n)", "working", (1, 2), "glom2", "glom1"))
    model.on_event(events.TemplateFiltered(3, 3, True))
    model.on_event(events.TemplateFiltered(4, 9, False))
    assert model.template.filtered == ((3, 3, True), (4, 9, False))
    assert so.template_lines(model.template)[-2:] == ["term 3 (3) fits",
                                                      "term 4 (9) does not fit"]
    model.on_event(events.TemplateModified("(S-group 1 n)", "(S-group n 2)"))
    assert model.template.state == "modified"
    model.on_event(events.TemplateMade("(S-group n 2)", "working", (1, 4), "glom5", "glom1"))
    t = model.template
    assert t.modified_from == "(S-group 1 n)" and t.filtered == ()
    assert so.fate_text(t) == "working, modified from (S-group 1 n)"
    model.on_event(events.TemplateChecked("S-group", False, ""))
    model.on_event(events.TemplateAbandoned("(S-group n 2)", "the terms no longer fit",
                                            "Template-evaluator"))
    t = model.template
    assert t.state == "abandoned" and not t.passed
    assert so.fate_text(t) == "abandoned by the Template-evaluator: the terms no longer fit"
    assert so.template_lines(t)[1] == "check: failed"
    assert "state abandoned" in t.fields_text
    model.on_event(events.TemplateMade("(Cycle 1 2 (1 2))", "working", (1, 2), "glom7", "glom6"))
    model.on_event(events.Blast(("glom7", "glom6", "glom3")))
    assert so.fate_text(model.template) == "working, resolved by a blast of 3 gloms"
    assert so.template_lines(None) == ["no template"] and so.fate_text(None) == ""


def test_a_blast_forgets_the_gnoths_and_the_template():
    log, reader, _ = logged(sequence=sq.find("blackburn-8"))
    (blasted,) = indexes(log, "blasted")
    before = model_at(log, blasted - 2)
    assert before.gnoths() and before.template is not None
    after = model_at(log, blasted)
    assert after.gnoths() == () and after.template is None and after.parse == "()"
    assert all("blasted" in c.reason for c in after.ghosts())
    assert reader.states[blasted][0] == "()"


# -- the selection (G-025) ----------------------------------------------------------------------

def test_selecting_a_gnoth_selects_its_glom_and_members(logs):
    log, _, _ = logs[RUN1]
    selection = Selection()
    model = model_at(log, len(log.events) - 1, selection)
    names = model.select_gnoth("gnoth1")
    assert names == ("glom3", "glom1", "glint1", "glint2")
    assert selection.kind == "gnoth" and selection.name == "gnoth1"
    assert all(selection.includes(n) for n in ("gnoth1", "glom3", "glom1", "glint1", "glint2"))
    assert model.is_selected("gnoth1") and not model.is_selected("gnoth2")
    model.select_gnoth("gnoth1")                 # a second click clears
    assert selection.empty
    assert model.select_gnoth("gnoth2") == ("glint3", "glint4")
    assert selection.includes("glint4")
    selection.select("glom", "glom3", ("glom1", "glint1", "glint2"))   # from the cytoplasm
    assert model.is_selected("gnoth1")
    assert model.select_gnoth(None) == () and selection.empty
    assert model.select_gnoth("no-such") == ()


# -- the boxes over the columns [M86 p. 102] (G-031) ---------------------------------------------

def test_the_boxes_are_laid_over_the_cytoplasm_columns(logs):
    log, _, _ = logs[RUN1]
    model = model_at(log, len(log.events) - 1)
    boxes = {b.name: b for b in so.gnoth_boxes(model)}
    gnoth1, catchall = boxes["gnoth1"], boxes["gnoth2"]
    assert gnoth1.left == 1 * cy.SPACING - cy.GLINT_WIDTH / 2 - so.BOX_PAD_X
    assert gnoth1.right == 2 * cy.SPACING + cy.GLINT_WIDTH / 2 + so.BOX_PAD_X
    assert gnoth1.row == 0 and gnoth1.kind == "gnoth" and gnoth1.label == "gnoth1"
    assert gnoth1.sub == "3 7" and not gnoth1.uncapped and gnoth1.confirmed
    assert catchall.kind == "catchall" and catchall.row == 0
    assert catchall.left == 3 * cy.SPACING - cy.GLINT_WIDTH / 2 - so.BOX_PAD_X
    assert so.strip_extent(model) == catchall.right - gnoth1.left
    # a box over the same columns goes to the next row
    model.cards["gnothX"] = so.GnothCard("gnothX", 5, "Cycle", "glomX", "stable", (2, 3), (),
                                         (7, 3), "(7 3)", "nil", None, None, None, None,
                                         False, (), ())
    rows = {b.name: b.row for b in so.gnoth_boxes(model)}
    assert rows["gnoth1"] == 0 and rows["gnothX"] == 1
    assert so.gnoth_boxes(so.SocratoplasmModel()) == ()


def test_an_uncapped_gnoth_is_dashed_and_recapped():
    log, _, _ = logged(sequence=sq.find("blackburn-1"))
    uncapped = indexes(log, "gnoth-uncapped")[0]
    event = log.events[uncapped]
    model = model_at(log, uncapped)
    card = model.card(event.name)
    assert card.uncapped and card.glom is None and card.parse_print == "()"
    assert card.range == tuple(event.range)
    box = next(b for b in so.gnoth_boxes(model) if b.name == event.name)
    assert box.uncapped
    capped = next(i for i in indexes(log, "gnoth-capped") if i > uncapped
                  and log.events[i].name == event.name)
    card = model_at(log, capped).card(event.name)
    assert not card.uncapped and card.glom == log.events[capped].glom


# -- the lines and texts (G-031) ----------------------------------------------------------------

def test_pane_lines_have_the_template_the_cards_and_the_bond_lines(logs):
    log, _, _ = logs[RUN3]
    model = model_at(log, len(log.events) - 1)
    lines = so.pane_lines(model)
    assert lines[0].kind == "template" and lines[1].text.startswith("template: (form")
    heads = [line for line in lines if line.kind == "head"]
    assert [h.text for h in heads] == ["gnoth2  S-group  frame 1", "gnoth3  S-group  frame 2",
                                       "gnoth6  catchall"]
    gnoth3 = [line for line in lines if line.name == "gnoth3"]
    fields = [line.text for line in gnoth3 if line.kind == "field"]
    assert fields == so.card_lines(model.card("gnoth3"))
    bonds = [(line.text, line.bond) for line in gnoth3 if line.kind == "bond"]
    assert ("bond8  10", "bond8") in bonds and ("bond5  10", "bond5") in bonds
    assert bonds.index(("bond8  10", "bond8")) < bonds.index(("bond5  10", "bond5"))
    note = [line for line in lines if line.kind == "note" and line.name == "gnoth6"]
    assert note and "[M86 p. 98]" in note[0].text
    assert so.bonds_text(((("bond2", 0), ("bond3", -5)))) == "((bond2 0) (bond3 -5))"
    assert so.bonds_text(()) == "nil" and so.bonds_text((("bond1", 2.5),)) == "((bond1 2.5))"
    assert so.groups_text((("glint1", "glint2"),)) == "((glint1 glint2))"
    assert so.groups_text(None) == "nil"
    assert so.form_field_text((("(Cycle 3 2 (3 7))", "pure"),)) == "(((Cycle 3 2 (3 7)) pure))"
    assert so.form_field_text(()) == "nil"


def test_an_empty_model_and_a_start_reset(logs):
    model = so.SocratoplasmModel()
    assert model.parse == "()" and model.gnoths() == () and model.catchall is None
    assert model.show_block() == ["gnoths:", ""] and model.template is None
    assert so.pane_lines(model)[1].text == "no template"
    log, _, _ = logs[RUN1]
    model = model_at(log, len(log.events) - 1)
    assert model.gnoths()
    model.on_event(log.events[0])
    assert model.parse == "()" and model.cards == {} and model.template is None
    assert model.events == 1 and model.terms == 0


def test_the_model_keeps_no_engine_object(logs):
    log, _, _ = logs[RUN3]
    model = model_at(log, len(log.events) - 1)
    for card in model.cards.values():
        for value in vars(card).values():
            assert isinstance(value, (str, int, float, bool, tuple, type(None)))
    assert isinstance(model.template, so.TemplateCard)

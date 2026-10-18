"""Loop0004 item 06: the cytoplasm pane's model, part 2 (`seekwhence/gui/models/cytoplasm.py`):
gloms, dubbing, pseudo-gloms, proposals, burst, dissolve, the blast and the selection,
Qt-free (G-024, G-025, G-026).

- Run 2 ends with `glom7` over `glom6` and `glint5` [M86 p. 62]: the hulls nest by depth,
  and selecting `glom7` selects its glints (G-025).
- A glom dubbed carries its class, form and purity, `Cycle (Cycle 3 2 (3 7)) pure`
  [M86 p. 69]; the pseudo-gloms (template glom [M86 p. 73], gnoth-glom [M86 p. 77], frame
  glom) are on the top-down layer, on by default and grey (ANSWERS.md 10.4; G-026, G-053).
- The terraced scan: a proposal `Same -cover proposed --> glint1` [M86 p. 165] is a marker
  while its Glomtester waits, a bracket while the Glommer does, gone at the glom, the
  failure or the stale task [M86 pp. 66-67].
- The blast leaves "only the glints and their bonds" [M86 p. 92]: the hulls are ghosts, the
  bonds stay (blackburn-8, seed 1). A burst takes a glom "and all its non-glint subgloms"
  [M86 p. 69]; a dissolved glom leaves its subgloms (blackburn-5, seed 3) [M86 p. 70].
- The pure style, label and tooltip functions; the layers.
"""

import io

import pytest

from seekwhence import sequences
from seekwhence.config import Config
from seekwhence.gui.models import cytoplasm as cy
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.selection import Selection
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX, appendix_inputs

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
    config, lines, answers = appendix_inputs(run_id)
    return logged(config, SEEDS[run_id], lines, answers)


def blackburn_log(name, seed, n_terms=12):
    """A Blackburn sequence's first terms, scripted, no answers (the guesses are judged
    against the terms, as the gate's cases are)."""
    seq = sequences.find(name)
    return logged(Config(), seed, [str(t) for t in seq.terms[:n_terms]])


@pytest.fixture(scope="module")
def logs():
    return {RUN1: appendix_log(RUN1), RUN2: appendix_log(RUN2), RUN3: appendix_log(RUN3),
            "blackburn-8": blackburn_log("blackburn-8", 1),
            "blackburn-5": blackburn_log("blackburn-5", 3)}


def model_at(log, index, layers=None, selection=None):
    """The model as the view has it at `index` (G-021), with `layers` switched."""
    model = cy.CytoplasmModel(selection)
    for name, on in (layers or {}).items():
        model.set_layer(name, on)
    for i in range(index + 1):
        model.on_event(log.events[i])
        model.update(log.state_at(i))
    return model


def indexes(log, kind, name=None):
    return [i for i, e in enumerate(log.events)
            if e.kind == kind and (name is None or getattr(e, "name", None) == name)]


def hulls_of(model):
    return {h.name: h for h in cy.layout(model).hulls}


# -- run 2: glom7 over glom6 and glint5 [M86 p. 62] ------------------------------------------------

def test_run_2_ends_with_glom7_over_glom6_and_glint5(logs):
    log = logs[RUN2]
    model = model_at(log, len(log.events) - 1, {"top-down": True})
    glom7, glom6 = model.gloms["glom7"], model.gloms["glom6"]
    assert glom7.members == ("glom6", "glint5") and glom7.depth == 2 and glom7.pseudo
    assert glom6.members == ("glint4",) and glom6.depth == 1 and glom6.parent == "glom7"
    assert (glom7.start, glom7.end) == (4, 5) and (glom6.start, glom6.end) == (4, 4)
    assert model.under("glom7") == ("glom6", "glint4", "glint5")
    assert model.glints_under("glom7") == ("glint4", "glint5")
    # the hulls nest: glom7's box holds glom6's and glint5's teardrop, and is taller
    hulls = hulls_of(model)
    outer, inner = hulls["glom7"], hulls["glom6"]
    assert outer.left < inner.left and outer.right > inner.right and outer.top < inner.top
    assert outer.depth == 2 and inner.depth == 1 and outer.bottom == inner.bottom == cy.HULL_BOTTOM
    glint5 = {g.name: g for g in cy.layout(model).glints}["glint5"]
    assert outer.left < glint5.left and glint5.left + glint5.width < outer.right
    assert outer.top == -(cy.GLINT_HEIGHT + cy.HULL_GAP + 2 * cy.HULL_BAND)
    assert inner.top == -(cy.GLINT_HEIGHT + cy.HULL_GAP + cy.HULL_BAND)
    # the outer hull comes first in the layout, so the inner draws on top
    names = [h.name for h in cy.layout(model).hulls]
    assert names.index("glom7") < names.index("glom6")
    # glom1 (3 7) is the one real glom; glom4 (gnoth-glom) holds it
    assert model.gloms["glom1"].parent == "glom4" and model.gloms["glom4"].depth == 2
    assert set(model.gloms) == {g.name for g in log.state_at(len(log.events) - 1).gloms}


def test_selecting_a_glom_selects_its_glints(logs):
    log = logs[RUN2]
    selection = Selection()
    model = model_at(log, len(log.events) - 1, {"top-down": True}, selection)
    model.select("glom7")
    assert (selection.kind, selection.name) == ("glom", "glom7")
    assert selection.members == {"glom7", "glom6", "glint4", "glint5"}
    assert model.is_selected("glint4") and model.is_selected("glint5")
    assert not model.is_selected("glint3") and not model.is_selected("glom1")
    assert cy.hull_style(model.gloms["glom7"], model.index, model.is_selected("glom7")).selected
    assert cy.glint_style(model.glints["glint5"], model.index, model.is_selected("glint5")).border \
        == cy.HIGHLIGHT_BORDER
    model.select("glint1")
    assert selection.members == {"glint1"} and (selection.kind, selection.name) == ("glint", "glint1")
    model.select("bond1")
    assert selection.kind == "bond" and {"glint1", "glint3"} <= selection.members
    model.select(None)
    assert selection.empty and not model.is_selected("glint1")
    model.select("nobody")                      # unknown: the selection is left alone
    assert selection.empty


# -- dubbing and the pseudo-gloms [M86 pp. 69, 73, 77] ----------------------------------------------

def test_a_dubbed_glom_is_labelled_with_class_form_and_purity(logs):
    log = logs[RUN1]
    at = indexes(log, "dubbed", "glom1")[0]
    before, after = model_at(log, at - 1), model_at(log, at)
    assert not before.gloms["glom1"].dubbed
    assert cy.hull_label(before.gloms["glom1"]) == "glom1  Same print-value fence"
    glom1 = after.gloms["glom1"]
    assert glom1.structure == (("(Cycle 3 2 (3 7))", "pure"),) and glom1.dubbed   # [M86 p. 166]
    assert cy.hull_label(glom1) == "glom1  Cycle (Cycle 3 2 (3 7)) pure"           # [M86 p. 69]
    assert cy.hull_label(glom1, names=False) == "Cycle (Cycle 3 2 (3 7)) pure"
    assert cy.hull_tooltip(glom1) == ("glom1 (Same print-value fence) --> (3 7) terms 1 to 2\n"
                                      "subgloms: (glint1 glint2)\n"
                                      "structure: (Cycle 3 2 (3 7)) pure")          # [M86 p. 167]
    style = cy.hull_style(glom1, at)
    assert style.bold and style.border == cy.DUBBED_BORDER and not style.dotted
    # the next snapshot agrees
    end = model_at(log, len(log.events) - 1)
    assert end.gloms["glom1"].structure == glom1.structure


def test_the_pseudo_gloms_are_on_the_top_down_layer(logs):
    log = logs[RUN1]
    at = indexes(log, "top-down", "glom2")[0]           # the template glom [M86 p. 167]
    model = model_at(log, at)
    glom2 = model.gloms["glom2"]
    assert glom2.pseudo and glom2.type == ("template",) and glom2.type_text == "pseudo"
    assert glom2.members == ("glom1",) and glom2.depth == 2 and model.gloms["glom1"].parent == "glom2"
    assert cy.hull_label(glom2) == "glom2  template glom"                           # [M86 p. 73]
    assert cy.hull_tooltip(glom2).startswith("glom2 pseudo --> ((3 7))  terms 1 to 2")
    assert cy.hull_style(glom2, at).dotted and cy.hull_style(glom2, at).flash
    assert hulls_of(model)["glom2"].pseudo              # the layer is on by default (ANSWERS.md 10.4)
    model.set_layer("top-down", False)
    assert "glom2" not in hulls_of(model)                # the toggle hides it (G-026)
    model.set_layer("top-down", True)
    end = model_at(log, len(log.events) - 1, {"top-down": True})
    kinds = {name: cy.hull_label(g, names=False) for name, g in end.gloms.items()}
    assert kinds == {"glom1": "Cycle (Cycle 3 2 (3 7)) pure", "glom3": "gnoth-glom",
                     "glom4": "frame glom"}                                          # [M86 p. 77]
    assert "glom2" not in end.gloms and "glom2" not in end.hull_ghosts   # gone silently, faded


def test_a_glom_gone_silently_fades_as_a_ghost(logs):
    log = logs[RUN1]
    last = max(i for i, e in enumerate(log.events) if e.kind == "top-down" and e.name == "glom2")
    gone = next(i for i in sorted(log.snapshots) if i > last
                and "glom2" not in {g.name for g in log.snapshots[i].gloms})
    model = model_at(log, gone, {"top-down": True})
    ghost = model.hull_ghosts["glom2"]
    assert ghost.fate == "gone" and ghost.gone_at == gone and not ghost.alive
    assert cy.hull_style(ghost, gone).opacity == 1.0
    assert cy.hull_style(ghost, gone + cy.FADE // 2).opacity == pytest.approx(0.5)
    assert "glom2" in hulls_of(model) and cy.hull_tooltip(ghost).endswith("(gone)")


# -- the terraced scan: proposals [M86 pp. 66-67, 165] -----------------------------------------------

def test_a_proposal_is_a_marker_until_resolved(logs):
    log = logs[RUN1]
    posted = [i for i, e in enumerate(log.events) if e.kind == "posted" and e.type == "Glomtester"]
    first = posted[0]
    assert log.events[first - 1].kind == "proposed"                                  # [M86 p. 165]
    model = model_at(log, first)
    (number, mark), = model.proposals.items()
    assert (mark.stage, mark.family, mark.test, mark.target) == ("proposed", "Same", "cover", "glint1")
    assert mark.made_at == first and cy.marker_style(mark, first).flash
    assert not cy.marker_style(mark, first).filled
    assert cy.marker_style(mark, first).color == cy.BOND_COLORS["Same"]
    assert cy.marker_tooltip(mark) == ("Same -cover proposed --> glint1\n"
                                       f"Glomtester on (Same cover glint1) (task {number})")
    (marker,), = [cy.layout(model).markers]
    assert marker.x == cy.SPACING and marker.x1 == marker.x2 == marker.x
    assert marker.label == "Same -cover" and marker.y == -(cy.GLINT_HEIGHT + 3)
    # the Glommer the Glomtester sets out: a bracket over the members
    glommer = next(i for i, e in enumerate(log.events) if e.kind == "posted" and e.type == "Glommer")
    model = model_at(log, glommer)
    marks = {m.number: m for m in model.proposals.values()}
    waiting = marks[log.events[glommer].number]
    assert waiting.stage == "glomming" and waiting.members == ("glint1", "glint2", "glint3")
    assert cy.marker_style(waiting, glommer).filled
    assert cy.marker_tooltip(waiting).startswith(
        "Same -cover proposed --> glint1\nGlommer for Same print-value cover members: (glint1 glint2 glint3)")
    chosen = log.events[glommer - 1]
    assert chosen.kind == "chosen" and chosen.number not in marks     # resolved by its Glommer
    shapes = {m.number: m for m in cy.layout(model).markers}
    assert (shapes[waiting.number].x1, shapes[waiting.number].x2) == (cy.SPACING, 3 * cy.SPACING)
    labelled = [m for m in cy.layout(model).markers if m.label]       # one label per target
    assert len(labelled) == 1 and labelled[0].label == "Same -fence, Same -cover"   # in task order
    # the glom resolves the running Glommer; a stale task resolves a running Glomtester
    made = indexes(log, "glom", "glom1")[0]
    before, after = model_at(log, made - 1), model_at(log, made)
    running = log.events[max(i for i in indexes(log, "chosen") if i < made)].number
    assert running in before.proposals and running not in after.proposals
    failed = indexes(log, "glomtester-failed")[0]                      # [M86 p. 170]
    assert len(model_at(log, failed).proposals) == len(model_at(log, failed - 1).proposals) - 1
    stale = next(i for i, e in enumerate(log.events) if e.kind == "stale" and e.type == "Glomtester")
    assert log.events[stale - 1].kind == "glomtester-failed"           # already resolved by it
    assert model_at(log, stale).proposals == model_at(log, stale - 1).proposals
    # at the end nothing is in flight, and the rack agrees at every snapshot
    end = model_at(log, len(log.events) - 1)
    assert end.proposals == {}
    for i in sorted(log.snapshots)[::7]:
        rack = {t.number for t in log.snapshots[i].rack.tasks if t.type in cy.PROPOSAL_TASKS}
        assert rack <= set(model_at(log, i).proposals)


# -- the blast [M86 p. 92], burst and dissolve [M86 pp. 69-70] ---------------------------------------

def test_the_blast_leaves_the_glints_and_their_bonds(logs):
    log = logs["blackburn-8"]
    (at,) = indexes(log, "blasted")
    event = log.events[at]
    assert set(event.gloms) == {"glom6", "glom7"} and set(event.bonds) == {"bond1", "bond2", "bond3"}
    before = model_at(log, at - 1, {"top-down": True})
    assert set(before.gloms) == {"glom6", "glom7"} and set(hulls_of(before)) == {"glom6", "glom7"}
    after = model_at(log, at, {"top-down": True})
    assert after.gloms == {} and set(after.bonds) == {"bond1", "bond2", "bond3"}
    ghosts = after.hull_ghosts
    assert {n: (g.fate, g.gone_at) for n, g in ghosts.items()} == {
        "glom6": ("blasted", at), "glom7": ("blasted", at)}
    assert set(hulls_of(after)) == {"glom6", "glom7"}          # still drawn, fading
    assert all(cy.hull_style(g, at).opacity == 1.0 for g in ghosts.values())
    later = model_at(log, at + cy.FADE // 2, {"top-down": True})
    assert cy.hull_style(later.hull_ghosts["glom7"], later.index).opacity == pytest.approx(0.5)
    assert set(later.bonds) == {"bond1", "bond2", "bond3"}
    faded = model_at(log, at + cy.FADE, {"top-down": True})
    assert faded.hull_ghosts == {} and hulls_of(faded) == {}
    assert set(faded.bonds) == {"bond1", "bond2", "bond3"} and len(faded.glints) == 3
    # the next snapshot agrees: no gloms
    nxt = min(i for i in log.snapshots if i > at)
    assert log.snapshots[nxt].gloms == () and model_at(log, nxt).gloms == {}


def test_a_burst_takes_the_glom_and_its_subgloms(logs):
    log = logs["blackburn-5"]
    (at,) = indexes(log, "burst")
    event = log.events[at]
    assert event.name == "glom11" and event.glints == ("glint2", "glint3", "glint4", "glint5")
    before = model_at(log, at - 1)
    assert before.gloms["glom11"].members == ("glint2", "glint3", "glom10")
    assert before.gloms["glom11"].depth == 2 and before.gloms["glom10"].parent == "glom11"
    assert before.gloms["glom10"].dubbed and before.gloms["glom10"].structure == (
        ("(Cycle 8 2 (8 1))", "pure"),)
    hulls = hulls_of(before)
    assert hulls["glom11"].left < hulls["glom10"].left and hulls["glom11"].top < hulls["glom10"].top
    after = model_at(log, at)
    assert after.gloms == {}
    assert {n: g.fate for n, g in after.hull_ghosts.items()} == {"glom11": "burst", "glom10": "burst"}
    assert cy.hull_tooltip(after.hull_ghosts["glom10"]).endswith("(burst)")


def test_a_dissolved_glom_frees_its_subgloms(logs):
    log = logs["blackburn-5"]
    at = indexes(log, "dissolved", "glom1")[0]
    event = log.events[at]
    assert event.freed == ("glint2", "glint3", "glint4")
    before, after = model_at(log, at - 1), model_at(log, at)
    assert before.gloms["glom1"].members == event.freed
    assert after.gloms == {} and after.hull_ghosts["glom1"].fate == "dissolved"
    assert after.hull_ghosts["glom1"].gone_at == at
    # a dissolved glom of gloms leaves its subgloms at the top level
    model = cy.CytoplasmModel()
    model.index = 0
    for n in (1, 2):
        model.glints[f"glint{n}"] = cy.GlintMark(f"glint{n}", n, n, None, None, (), True, None)
    model._add_glom("glomA", ("Succ", "print-value", "group"), ("glint1", "glint2"), (1, 2), False)
    model._add_glom("glomB", ("pseudo",), ("glomA",), ((1, 2),), True)
    assert model.gloms["glomA"].parent == "glomB" and model.gloms["glomB"].depth == 2
    model.index = 5
    model._dissolve("glomB")
    assert model.gloms["glomA"].parent is None and model.hull_ghosts["glomB"].fate == "dissolved"
    model._burst("glomA", "burst")
    assert model.gloms == {} and model.hull_ghosts["glomA"].fate == "burst"


# -- the layers (G-026) and the styles (G-024) ---------------------------------------------------------

def test_the_layers(logs):
    log = logs[RUN3]
    index = next(i for i in sorted(log.snapshots)
                 if any(g.external_bonds for g in log.snapshots[i].gnoths))
    model = model_at(log, index)
    assert model.layers == cy.LAYERS and cy.LAYERS == {"sparks": False, "top-down": True,
                                                       "values": True, "names": True}
    lay = cy.layout(model)                        # the pseudo-gloms shown (ANSWERS.md 10.4)
    assert all(a.kind == "bond" for a in lay.arcs) and any(h.pseudo for h in lay.hulls)
    model.set_layer("top-down", False)
    assert not any(h.pseudo for h in cy.layout(model).hulls)
    assert {a.label for a in lay.arcs} == {"+10", "-5", "0"}                     # [M86 p. 102]
    assert all(g.label == g.name for g in lay.glints)
    model.set_layer("sparks", True)
    model.set_layer("top-down", True)
    model.set_layer("values", False)
    model.set_layer("names", False)
    lay = cy.layout(model)
    assert any(a.kind == "spark" for a in lay.arcs) or not model.sparks
    assert all(a.label is None for a in lay.arcs)
    assert all(g.label == "" for g in lay.glints)
    assert all(not h.label.startswith("glom") for h in lay.hulls) and lay.hulls
    with pytest.raises(KeyError):
        model.set_layer("moons", True)
    # the layers survive a new run; the marks do not
    model.on_event(log.events[0])
    assert model.layers["names"] is False and model.gloms == {} and model.proposals == {}


def test_the_hull_and_marker_styles():
    glom = cy.HullMark("glom1", ("Same", "print-value", "fence"), "(Same print-value fence)",
                       (3, 7), 1, 2, ("glint1", "glint2"), None, 1, None, False, 10)
    plain = cy.hull_style(glom, 20)
    assert (plain.fill, plain.border, plain.dotted, plain.bold, plain.flash, plain.opacity) == (
        cy.HULL_FILL, cy.HULL_BORDER, False, False, False, 1.0)
    assert cy.hull_style(glom, 12).flash and not cy.hull_style(glom, 10 + cy.FLASH).flash
    dubbed = cy.hull_style(glom.__class__(**{**glom.__dict__, "structure": (("(Cycle 3 2 (3 7))", "pure"),)}), 20)
    assert dubbed.bold and dubbed.border == cy.DUBBED_BORDER and dubbed.border_width > plain.border_width
    pseudo = cy.hull_style(glom.__class__(**{**glom.__dict__, "pseudo": True, "type": ("frame",)}), 20)
    assert pseudo.dotted and pseudo.border == cy.PSEUDO_BORDER and pseudo.fill == cy.PSEUDO_FILL
    assert pseudo.border != plain.border and pseudo.fill != plain.fill      # ANSWERS.md 10.4: grey, lighter
    chosen = cy.hull_style(glom, 20, selected=True)
    assert chosen.selected and chosen.border == cy.HIGHLIGHT_BORDER and chosen.fill == cy.SELECTED_FILL
    ghost = glom.__class__(**{**glom.__dict__, "gone_at": 30, "fate": "burst"})
    assert cy.hull_style(ghost, 30).opacity == 1.0
    assert cy.hull_style(ghost, 30 + cy.FADE).opacity == 0.0 and not cy.hull_style(ghost, 31).flash
    tester = cy.ProposalMark(28, "proposed", "Pred", "group", "glint2", (), 5)
    assert cy.marker_style(tester, 5) == cy.MarkerStyle(cy.BOND_COLORS["Pred"], False, True)
    glommer = cy.ProposalMark(33, "glomming", "Odd", "cover", "glint1", ("glint1", "glint2"), None)
    assert cy.marker_style(glommer, 5) == cy.MarkerStyle(cy.OTHER_BOND_COLOR, True, False)
    assert cy.PSEUDO_KINDS["gnoth"] == "gnoth-glom" and cy.PSEUDO_KINDS["template"] == "template glom"


def test_the_ends_of_the_runs_match_the_last_snapshot(logs):
    for run_id in (RUN1, RUN2, RUN3, "blackburn-8", "blackburn-5"):
        log = logs[run_id]
        model = model_at(log, len(log.events) - 1)
        snapshot = log.state_at(len(log.events) - 1)
        assert {g.name for g in snapshot.gloms} == set(model.gloms), run_id
        for record in snapshot.gloms:
            mark = model.gloms[record.name]
            assert (mark.members, mark.parent, mark.structure, mark.pseudo) == (
                tuple(record.subgloms), record.parent, record.structure, record.pseudo), run_id
            assert (mark.start, mark.end) == (record.start, record.end)
            assert mark.depth == 1 + max([model.gloms[m].depth for m in mark.members
                                          if m in model.gloms] + [0])

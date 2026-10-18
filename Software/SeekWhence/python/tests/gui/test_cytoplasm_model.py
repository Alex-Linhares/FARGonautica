"""Loop0004 item 05: the cytoplasm pane's model (`seekwhence/gui/models/cytoplasm.py`),
Qt-free (G-021, G-022, G-023).

- Run 1 at term 3: three glints, `bond1 Same print-value (remote) -- (glint1 glint3)`
  [M86 pp. 164, 167], laid out as Figure 1 draws them [M86 p. 102].
- The `glint`, `spark` and `bond` events between two snapshots add what they announce;
  a new spark or bond flashes; a spark used up (silently) fades as a ghost.
- Run 3 under reformulation labels the bonds with their values in the reigning class's
  environment: `+10` for the adjacent successor bond, `-5` for the remote sameness bond
  [M86 p. 102].
- The pure style and layout functions; the tooltips in the thesis's formats
  [M86 pp. 61, 164, 167].
"""

import io

import pytest

from seekwhence.gui.models import cytoplasm as cy
from seekwhence.gui.models.event_log import EventLog
from seekwhence.gui.models.tap import Tap
from seekwhence.session import Session

from golden import APPENDIX, appendix_inputs

RUN1, RUN2, RUN3 = (run_id for run_id, _ in APPENDIX)
SEEDS = dict(APPENDIX)


def logged(run_id):
    """An appendix run's `EventLog` with the tap's snapshots."""
    config, lines, answers = appendix_inputs(run_id)
    log = EventLog()
    tap = Tap(log)
    session = Session(config, SEEDS[run_id], out=io.StringIO(), lines=list(lines),
                      answers=list(answers), observers=[tap])
    tap.attach_session(session)
    session.run()
    return log


@pytest.fixture(scope="module")
def logs():
    return {run_id: logged(run_id) for run_id, _ in APPENDIX}


def model_at(log, index):
    """The model as the view has it at `index`: every event up to it, each followed by
    the snapshot in force (G-021)."""
    model = cy.CytoplasmModel()
    for i in range(index + 1):
        model.on_event(log.events[i])
        model.update(log.state_at(i))
    return model


def indexes(log, kind, name=None):
    return [i for i, e in enumerate(log.events)
            if e.kind == kind and (name is None or e.name == name)]


def arc(model, name):
    return {m.name: m for m in model.arcs()}[name]


# -- run 1 at term 3 [M86 pp. 164-167] -----------------------------------------------------------------

def test_run_1_at_term_3_has_three_glints_and_bond1(logs):
    log = logs[RUN1]
    index = indexes(log, "prompt")[2]        # the prompt after the third term
    model = model_at(log, index)
    assert model.index == index
    glints = sorted(model.glints.values(), key=lambda g: g.position)
    assert [(g.name, g.value, g.position) for g in glints] == [
        ("glint1", 3, 1), ("glint2", 7, 2), ("glint3", 3, 3)]
    assert glints[0].left_nbr is None and glints[0].right_nbr == "glint2"
    assert glints[2].left_nbr == "glint2" and glints[2].right_nbr is None
    assert glints[0].bonds_in == ("bond1",) and glints[1].bonds_in == ()
    assert list(model.bonds) == ["bond1"]
    bond = model.bonds["bond1"]
    assert (bond.kind, bond.type, bond.field, bond.modifier) == ("bond", "Same", "print-value",
                                                                 "remote")
    assert (bond.a, bond.b) == ("glint1", "glint3") and bond.alive and bond.strength == 5
    assert bond.value is None                 # no gnoths yet: no value label
    assert cy.arc_tooltip(bond) == "bond1 Same print-value (remote) -- (glint1 glint3)"
    assert cy.glint_tooltip(glints[2]) == ("name: glint3\nprint-value: 3\nposition: 3\n"
                                           "left-nbr: glint2\nright-nbr: nil\n"
                                           "bonds-in: (bond1)")              # [M86 p. 61]


def test_the_layout_of_run_1_at_term_3(logs):
    log = logs[RUN1]
    model = model_at(log, indexes(log, "prompt")[2])
    model.set_layer("sparks", True)          # off by default (G-026)
    lay = cy.layout(model)
    assert [(b.name, b.x, b.value, b.label) for b in lay.glints] == [
        ("glint1", cy.SPACING, 3, "glint1"), ("glint2", 2 * cy.SPACING, 7, "glint2"),
        ("glint3", 3 * cy.SPACING, 3, "glint3")]
    assert all((b.width, b.height, b.top) == (cy.GLINT_WIDTH, cy.GLINT_HEIGHT, -cy.GLINT_HEIGHT)
               for b in lay.glints)
    arcs = {a.name: a for a in lay.arcs}
    bond = arcs["bond1"]
    assert (bond.x1, bond.x2) == (cy.SPACING, 3 * cy.SPACING)
    assert bond.depth == cy.REMOTE_DEPTH + cy.REMOTE_DEPTH_PER_GAP     # one position of gap
    assert bond.label is None and bond.mid_x == 2 * cy.SPACING
    # glom1 (3 7), flat, under the template glom's hull, shown by default (ANSWERS.md 10.4)
    model.set_layer("top-down", False)
    lay = cy.layout(model)
    assert lay.left == cy.SPACING - cy.GLINT_WIDTH / 2 - cy.HULL_PAD_X
    assert lay.right == 3 * cy.SPACING + cy.GLINT_WIDTH / 2
    assert lay.top == -(cy.GLINT_HEIGHT + cy.HULL_GAP + cy.HULL_BAND)   # one hull band: glom1
    assert lay.bottom >= bond.depth + cy.LABEL_HEIGHT
    assert lay.width == lay.right - lay.left and lay.height == lay.bottom - lay.top
    # the ghosts come first, then the live bonds, then the live sparks (drawn on top)
    order = [a.name for a in lay.arcs]
    expected = ([n for n in model.ghosts if n in order] + [n for n in model.bonds if n in order]
                + [n for n in model.sparks if n in order])
    assert order == expected and model.ghosts


def test_the_bond_event_adds_the_bond_before_the_next_snapshot(logs):
    log = logs[RUN1]
    at = indexes(log, "bond", "bond1")[0]
    assert log.snapshot_index(at) < at        # no snapshot at the bond event itself
    assert "bond1" not in model_at(log, at - 1).bonds
    model = model_at(log, at)
    bond = model.bonds["bond1"]
    assert bond.made_at == at and bond.alive
    assert cy.bond_style(bond, at).flash
    assert cy.bond_style(bond, at + cy.FLASH - 1).flash
    assert not cy.bond_style(bond, at + cy.FLASH).flash
    # the next snapshot keeps the mark and its made_at
    later = model_at(log, at + 1)
    assert later.bonds["bond1"].made_at == at
    assert model_at(log, len(log.events) - 1).bonds["bond1"].made_at == at


def test_a_glint_event_adds_the_glint_with_its_neighbours(logs):
    log = logs[RUN1]
    at = indexes(log, "glint", "glint3")[0]
    before = model_at(log, at - 1)
    assert sorted(before.glints) == ["glint1", "glint2"]
    assert before.glints["glint2"].right_nbr is None
    model = model_at(log, at)
    assert sorted(model.glints) == ["glint1", "glint2", "glint3"]
    glint = model.glints["glint3"]
    assert (glint.value, glint.position, glint.left_nbr, glint.made_at) == (3, 3, "glint2", at)
    assert model.glints["glint2"].right_nbr == "glint3"
    assert cy.glint_style(glint, at).flash and not cy.glint_style(glint, at + cy.FLASH).flash
    assert model.glint_at(3) is glint and model.glint_at(4) is None


def test_a_spark_flashes_when_made_and_fades_when_used(logs):
    log = logs[RUN1]
    at = indexes(log, "spark")[0]
    name = log.events[at].name
    model = model_at(log, at)
    spark = model.sparks[name]
    assert (spark.kind, spark.a, spark.b) == ("spark", log.events[at].a, log.events[at].b)
    assert spark.made_at == at and spark.alive
    assert cy.arc_tooltip(spark) == f"{name} --- between {spark.a}{spark.b}"
    style = cy.spark_style(spark, at)
    assert style.flash and style.dashed and style.opacity == 1.0 and style.color == cy.SPARK_COLOR
    assert not cy.spark_style(spark, at + cy.FLASH).flash
    assert cy.spark_style(spark, at + cy.FLASH).opacity < 1.0
    # used up silently: it leaves the snapshot and stays as a fading ghost
    gone = next(i for i in range(at + 1, len(log.events))
                if name not in model_at(log, i).sparks)
    model = model_at(log, gone)
    ghost = model.ghosts[name]
    assert ghost.gone_at == gone and not ghost.alive and ghost.made_at == at
    assert name not in model.sparks
    opacities = [cy.spark_style(ghost, gone + k).opacity for k in range(cy.FADE + 1)]
    assert opacities[0] < 1.0 and opacities == sorted(opacities, reverse=True)
    assert opacities[-1] == 0.0
    assert name not in model_at(log, gone + cy.FADE).ghosts
    assert name in model_at(log, gone + cy.FADE - 1).ghosts
    assert cy.arc_tooltip(ghost).startswith(f"{name} --- between")


# -- the values under reformulation, run 3 [M86 p. 102] -------------------------------------------------

def test_run_3_labels_the_bonds_with_their_values_in_the_reigning_class(logs):
    log = logs[RUN3]
    index = next(i for i in sorted(log.snapshots)
                 if any(g.external_bonds for g in log.snapshots[i].gnoths))
    model = model_at(log, index)
    assert model.reigning == "S-group"
    assert model.values == {"bond1": 10, "bond2": -5, "bond3": 0}
    bonds = model.bonds
    assert (bonds["bond1"].type, bonds["bond1"].modifier) == ("Succ", "adjacent")
    assert (bonds["bond2"].type, bonds["bond2"].modifier) == ("Same", "remote")
    assert bonds["bond1"].value == 10 and bonds["bond2"].value == -5 and bonds["bond3"].value == 0
    labels = {a.name: a.label for a in cy.layout(model).arcs if a.kind == "bond"}
    assert labels == {"bond1": "+10", "bond2": "-5", "bond3": "0"}     # [M86 p. 102]
    assert cy.arc_tooltip(bonds["bond1"], model.reigning) == (
        "bond1 Succ print-value (adjacent) -- (glint1 glint2)\nvalue in S-group: +10")
    before = model_at(log, index - 1)
    assert before.values == {} and all(b.value is None for b in before.bonds.values())
    # an internal bond's value, once listed, wins over the external listing
    end = model_at(log, len(log.events) - 1)
    assert end.values["bond1"] == 10 and end.values["bond8"] == 10
    assert sorted(end.bonds) == [f"bond{n}" for n in range(1, 9)]


# -- the ends of the three runs ------------------------------------------------------------------------

@pytest.mark.parametrize("run_id", [RUN1, RUN2, RUN3])
def test_at_the_end_the_model_matches_the_last_snapshot(logs, run_id):
    log = logs[run_id]
    last = len(log.events) - 1
    model = model_at(log, last)
    snap = log.state_at(last)
    assert len(model.glints) == len(snap.terms) == len(snap.glints)
    assert sorted(model.bonds) == sorted(b.name for b in snap.bonds)
    assert sorted(model.sparks) == sorted(s.name for s in snap.sparks)
    assert all(b.alive for b in model.bonds.values())
    lay = cy.layout(model)
    assert [b.value for b in lay.glints] == list(snap.terms)
    assert {a.name for a in lay.arcs} >= set(model.bonds)


def test_a_start_event_resets_the_model(logs):
    log = logs[RUN1]
    model = model_at(log, len(log.events) - 1)
    assert model.glints and model.bonds
    model.on_event(log.events[0])
    assert model.index == 0 and not model.glints and not model.bonds and not model.ghosts
    assert model.reigning is None and model.values == {}


# -- a spark between a glom and a glint anchors at the glom's span [M86 p. 62] --------------------------

def test_a_spark_to_a_glom_is_anchored_at_the_gloms_span(logs):
    log = logs[RUN1]
    at = next(i for i in indexes(log, "spark") if log.events[i].a.startswith("glom"))
    event = log.events[at]
    model = model_at(log, at)
    start, end = model.spans[event.a]
    assert model.anchor_x(event.a) == (start + end) / 2 * cy.SPACING
    assert model.anchor_x("nobody") is None
    model.set_layer("sparks", True)
    arcs = {a.name: a for a in cy.layout(model).arcs}
    assert arcs[event.name].x1 == min(model.anchor_x(event.a), model.anchor_x(event.b))
    assert arcs[event.name].kind == "spark"


# -- the pure style and layout functions (G-023) --------------------------------------------------------

def bond(type="Same", modifier="remote", strength=5, value=None, made_at=None, gone_at=None):
    return cy.ArcMark("bond", "bondx", "glint1", "glint3", type, "print-value", modifier,
                      strength, value, made_at, gone_at)


def test_bond_style_by_type_strength_and_age():
    assert cy.bond_style(bond("Same"), 10).color == cy.BOND_COLORS["Same"]
    assert cy.bond_style(bond("Succ"), 10).color == cy.BOND_COLORS["Succ"]
    assert cy.bond_style(bond("Pred"), 10).color == cy.BOND_COLORS["Pred"]
    assert cy.bond_style(bond("Meet"), 10).color == cy.OTHER_BOND_COLOR
    widths = [cy.bond_style(bond(strength=s), 10).width for s in (0, 4, 8, 10)]
    assert widths == sorted(widths) and widths[0] == 1.0 and widths[-1] == 3.5
    live = cy.bond_style(bond(made_at=3), 20)
    assert (live.dashed, live.opacity, live.flash) == (False, 1.0, False)
    assert cy.bond_style(bond(made_at=20), 20).flash
    ghost = bond(made_at=3, gone_at=20)
    assert cy.bond_style(ghost, 20).opacity == 1.0
    assert cy.bond_style(ghost, 20 + cy.FADE // 2).opacity == pytest.approx(0.5)
    assert cy.bond_style(ghost, 20 + cy.FADE).opacity == 0.0
    assert not cy.bond_style(bond(made_at=20, gone_at=20), 20).flash


def test_glint_style_and_highlight():
    glint = cy.GlintMark("glint1", 3, 1, None, None, (), True, 5)
    plain = cy.glint_style(glint, 20)
    assert (plain.fill, plain.border, plain.opacity, plain.flash) == (
        cy.GLINT_FILL, cy.GLINT_BORDER, 1.0, False)
    lit = cy.glint_style(glint, 20, highlighted=True)
    assert lit.border == cy.HIGHLIGHT_BORDER and lit.border_width > plain.border_width
    inactive = cy.glint_style(cy.GlintMark("glint1", 3, 1, None, None, (), False, None), 20)
    assert inactive.opacity < 1.0 and not inactive.flash
    assert cy.glint_tooltip(cy.GlintMark("glint2", 7, 2, "glint1", None, ("bond1",), True,
                                         None)).endswith("bonds-in: (bond1)")


def test_value_labels_as_figure_1_prints_them():
    assert cy.value_label(10) == "+10" and cy.value_label(-5) == "-5"      # [M86 p. 102]
    assert cy.value_label(0) == "0" and cy.value_label(None) is None


def test_layout_depths_adjacent_remote_sparks_and_stacking():
    model = cy.CytoplasmModel()
    model.index = 0
    for n, value in enumerate((1, 2, 3, 4), start=1):
        model.glints[f"glint{n}"] = cy.GlintMark(f"glint{n}", value, n, None, None, (), True,
                                                 None)
    model.bonds["bond1"] = cy.ArcMark("bond", "bond1", "glint1", "glint2", "Succ",
                                      "print-value", "adjacent", 8)
    model.bonds["bond2"] = cy.ArcMark("bond", "bond2", "glint1", "glint3", "Same",
                                      "print-value", "remote", 5)
    model.bonds["bond3"] = cy.ArcMark("bond", "bond3", "glint1", "glint4", "Same",
                                      "print-value", "remote", 5)
    model.bonds["bond4"] = cy.ArcMark("bond", "bond4", "glint1", "glint2", "Same",
                                      "span", "adjacent", 2)
    model.sparks["spark1"] = cy.ArcMark("spark", "spark1", "glint2", "glint1")
    model.sparks["spark2"] = cy.ArcMark("spark", "spark2", "glint9", "glint1")   # not drawn
    model.set_layer("sparks", True)
    arcs = {a.name: a for a in cy.layout(model).arcs}
    assert "spark2" not in arcs
    assert arcs["bond1"].depth == cy.ADJACENT_DEPTH
    assert arcs["bond2"].depth == cy.REMOTE_DEPTH + cy.REMOTE_DEPTH_PER_GAP
    assert arcs["bond3"].depth == cy.REMOTE_DEPTH + 2 * cy.REMOTE_DEPTH_PER_GAP
    assert arcs["bond4"].depth == cy.ADJACENT_DEPTH + cy.STACK_STEP     # the same pair, stacked
    assert arcs["spark1"].depth == cy.REMOTE_DEPTH + cy.SPARK_EXTRA_DEPTH
    assert (arcs["spark1"].x1, arcs["spark1"].x2) == (cy.SPACING, 2 * cy.SPACING)   # sorted
    lay = cy.layout(cy.CytoplasmModel())
    assert lay.glints == () and lay.arcs == () and lay.width > 0 and lay.height > 0

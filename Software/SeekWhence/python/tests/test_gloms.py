"""The terraced scan: Glom-scout, Glomtester, Glommer, Glom-inspector, Plato-scout,
Plato-evaluator and Burster [M86 pp. 65-70, 165, 170, 173] (DECISIONS D-019 to D-024)."""

import io
from collections import Counter

import pytest

from seekwhence import bonds, gloms
from seekwhence import cytoplasm as cy
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.run import TASKS, new_world, run_until_prompt
from seekwhence.trace import Trace


def world_with(*terms, seed=1, scout=False, trace=None):
    """A world with the terms entered and the rack run empty after each, the Glom-scout
    replaced by a no-op unless `scout` (so the tests control what is proposed), and the
    Template-scout always a no-op: these tests are of the scan up to the platoplasm, and a
    template's failure at term level dissolves the engendering glom [M86 p. 75]
    (`tests/test_templates.py` tests that)."""
    recorder = Recorder()
    observers = [recorder] if trace is None else [recorder, trace]
    world = new_world(seed=seed, observers=observers)
    world.handlers["Template-scout"] = lambda w, *args: None
    if not scout:
        world.handlers["Glom-scout"] = lambda w, *args: None
    for term in terms:
        bonds.new_term(world, term)
        run_all(world)
    return world, recorder


def run_all(world):
    while run_until_prompt(world) != "empty":
        pass


def bond_all(world):
    """Make every bond the Tester would pass between every pair of top-level elements
    [M86 p. 64], without running tasks."""
    elements = cy.top_level_elements(world, pseudo=False)
    for i, a in enumerate(elements):
        for b in elements[i + 1:]:
            left, right = cy.left_to_right(a, b)
            for type, field in bonds.bond_tests(left, right):
                if cy.bonded(left, right, type, field) is None:
                    bonds.make_bond(world, type, field, cy.modifier(left, right), left, right)
    world.taskrack.clean(("Sparkler", "Glom-scout"))


def extent_values(world, target, family, test):
    members = gloms.extent(world, world.find(target), family, test)
    return None if members is None else [x.print_value for x in members]


def posted(recorder, type):
    return [e for e in recorder.of_kind("posted") if e.type == type]


def real_gloms(world):
    return [g for g in world.cytoplasm if isinstance(g, cy.Glom) and not g.pseudo]


def glom(world, *names, type=("Same", "print-value", "group")):
    return cy.make_glom(world, type, [world.find(n) for n in names])


# ---- registration ------------------------------------------------------------------------

def test_the_scan_tasks_are_registered():
    """Each module adds its handlers to `run.TASKS` (SPEC 2.4)."""
    for type in ("Glom-scout", "Glomtester", "Glommer", "Glom-inspector", "Plato-scout",
                 "Plato-evaluator", "Burster"):
        assert TASKS[type] is gloms.TASKS[type]
    assert new_world().handlers["Glommer"] is gloms.glommer


# ---- the Glom-scout's quicktests [M86 p. 66] -----------------------------------------------

def test_an_end_of_a_remote_sameness_bond_is_coverable_and_fenceable():
    """`3 7 3`: `Same -cover proposed --> glint1`, `Same -fence proposed --> glint1`
    [M86 p. 165], and the same from the other end, `glint3` of `1 2 1` [M86 p. 173]
    (DECISION D-020)."""
    world, _ = world_with(3, 7, 3)
    assert gloms.quicktests(world, world.find("glint1")) == [("Same", "cover"), ("Same", "fence")]
    assert gloms.quicktests(world, world.find("glint3")) == [("Same", "cover"), ("Same", "fence")]


def test_the_centre_of_a_symmetry_is_coverable_without_a_bond_of_its_own():
    """The 7 of `3 7 3` and the 6 of `5 3 1 6 1 3 4` [M86 p. 67] (DECISION D-020)."""
    world, _ = world_with(3, 7, 3)
    assert gloms.quicktests(world, world.find("glint2")) == [("Same", "cover")]
    world, _ = world_with(5, 3, 1, 6, 1, 3, 4)
    bond_all(world)
    assert gloms.centre_of_symmetry(world, world.find("glint4"))
    assert ("Same", "cover") in gloms.quicktests(world, world.find("glint4"))
    assert not gloms.centre_of_symmetry(world, world.find("glint1"))


def test_an_adjacent_successor_pair_is_groupable_and_labelled_from_the_scouted_side():
    """`Pred -group proposed --> glint2` for `1 2` [M86 p. 170]: the family is named from
    the element's side (DECISION D-020); from the 1 it would be `Succ`."""
    world, _ = world_with(1, 2)
    assert gloms.quicktests(world, world.find("glint2")) == [("Pred", "group")]
    assert gloms.quicktests(world, world.find("glint1")) == [("Succ", "group")]


def test_a_glint_whose_neighbour_glint_is_taken_is_not_groupable():
    """The second scout of [M86 p. 173] proposes `Same -cover` and `Same -fence` for
    `glint3` of `1 2 1`, no group, although `glom1` `(1 2)` is bonded to it by adjacent
    predecessorship (`bond4`): a glint's neighbours are its `left-nbr` and `right-nbr`
    glints [M86 p. 61], and `glint2` is under `glom1` (DECISION D-020)."""
    world, _ = world_with(1, 2)
    g = glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    gloms.dub(world, g, "S-group", "pure", scout=False)   # else the term's Dissolver takes it (D-024)
    bonds.new_term(world, 1)
    run_all(world)
    glint3 = world.find("glint3")
    assert cy.bonded(g, glint3, "Pred", "print-value") is not None
    # bond3 of [M86 p. 172] is between the nested glint1 and glint3; the Sparkler of
    # DECISION D-025 does not spark nested elements, so it is made by hand here.
    bonds.make_bond(world, "Same", "print-value", "remote", world.find("glint1"), glint3)
    bonds.make_bond(world, "Pred", "print-value", "adjacent", world.find("glint2"), glint3)
    assert gloms.quicktests(world, glint3) == [("Same", "cover"), ("Same", "fence")]
    # From the glom's side its neighbour is the top-level glint, so a group is proposed.
    assert gloms.quicktests(world, g) == [("Pred", "group")]


def test_weak_bonds_are_not_followed():
    """A same-span bond is "not considered strong enough to cause glomming" [M86 p. 62]
    (DECISION D-019)."""
    world, _ = world_with(1, 2, 5, 6)
    a = glom(world, "glint1", "glint2", type=("Succ", "print-value", "group"))
    b = glom(world, "glint3", "glint4", type=("Succ", "print-value", "group"))
    bonds.make_bond(world, "Same", "span", "adjacent", a, b)
    assert gloms.strong_bonds(world, a) == [] and gloms.families_of(world, a) == []
    assert gloms.quicktests(world, a) == []


def test_the_scout_draws_a_top_level_real_element_and_proposes_through_a_glomtester():
    """"a Glom-scout chooses a cyto-element (a glint or glom which is not a subglom of any
    other glom)" [M86 p. 66]; every test passed sets out a Glomtester [M86 p. 66] with the
    printed proposal [M86 p. 165] (DECISION D-020)."""
    out = io.StringIO()
    world, recorder = world_with(3, 7, 3, trace=Trace(out))
    seen = Counter()
    for _ in range(60):
        before = len(posted(recorder, "Glomtester"))
        gloms.glom_scout(world)
        new = posted(recorder, "Glomtester")[before:]
        seen[tuple(t.args for t in new)] += 1
    assert set(seen) == {(("Same", "cover", "glint1"), ("Same", "fence", "glint1")),
                         (("Same", "cover", "glint2"),),
                         (("Same", "cover", "glint3"), ("Same", "fence", "glint3"))}
    assert "Same -cover proposed --> glint1" in out.getvalue().splitlines()
    assert "Same -fence proposed --> glint3" in out.getvalue().splitlines()


def test_the_glomtester_urgency_orders_group_above_fence_above_cover_and_same_above_succ():
    """"The Glomtester's weight (urgency) is dependent on the test ('groupable' being
    strongest) and the bond-type involved (sameness being stronger than successorship, and
    so on)" [M86 p. 66]; "a Glomtester for a sameness group is given a higher weight than a
    predecessor fence Glomtester" [M86 p. 67] (DECISION D-020)."""
    world = new_world()
    u = lambda label, test: gloms.glomtester_urgency(world, label, test)
    assert u("Same", "group") > u("Pred", "fence")
    assert u("Same", "group") > u("Same", "fence") > u("Same", "cover")
    assert u("Same", "group") > u("Succ", "group") > u("Pred", "fence") > u("Succ", "cover")
    assert u("Same", "group") == 9 and u("Pred", "fence") == 4


# ---- the Glomtester's extent [M86 p. 67] ------------------------------------------------------

def test_example_1_sameness_in_2_1_1_1_3():
    """"in '2 1 1 1 3', with the second '1' targeted and 'sameness' the bond-type, the
    Glomtester would suggest that '1 1 1' be grouped" [M86 p. 67]."""
    world, _ = world_with(2, 1, 1, 1, 3)
    bond_all(world)
    assert extent_values(world, "glint3", "Same", "group") == [1, 1, 1]
    assert extent_values(world, "glint2", "Same", "group") == [1, 1, 1]


def test_example_2_fence_in_9_1_2_5_9():
    """"in '9 1 2 5 9', with the first '9' targeted and 'fence' the reason, '9 1 2 5'
    would be suggested" [M86 p. 67]; from the second 9 no fence is cut (DECISION D-021)."""
    world, _ = world_with(9, 1, 2, 5, 9)
    bond_all(world)
    assert extent_values(world, "glint1", "Same", "fence") == [9, 1, 2, 5]
    assert extent_values(world, "glint5", "Same", "fence") is None


def test_example_3_symmetry_in_5_3_1_6_1_3_4():
    """"in '5 3 1 6 1 3 4', with '6' targeted and 'symmetry' the reason, '3 1 6 1 3' would
    be suggested" [M86 p. 67]; from an end of the `1`-`1` or `3`-`3` bond the same window
    is found (DECISION D-021)."""
    world, _ = world_with(5, 3, 1, 6, 1, 3, 4)
    bond_all(world)
    assert extent_values(world, "glint4", "Same", "cover") == [3, 1, 6, 1, 3]
    assert extent_values(world, "glint3", "Same", "cover") == [3, 1, 6, 1, 3]
    assert extent_values(world, "glint6", "Same", "cover") == [3, 1, 6, 1, 3]


def test_example_4_the_pred_succ_family_in_1_2_3_2_3():
    """"in '1 2 3 2 3', with the first '2' the target and the 'pred-succ' bond family the
    reason, '1 2 3 2 3' would be suggested" [M86 p. 67]: "all 'chains' that consist of gloms
    related to their neighbors by some element of a bond family" [M86 p. 68]."""
    world, _ = world_with(1, 2, 3, 2, 3)
    bond_all(world)
    assert extent_values(world, "glint2", "pred-succ", "group") == [1, 2, 3, 2, 3]
    assert extent_values(world, "glint5", "pred-succ", "group") == [1, 2, 3, 2, 3]


def test_the_cover_window_from_an_end_must_read_the_same_both_ways():
    """`1 2 1` with `glint1` under `glom1`: the Glomtester on `(Same cover glint3)` prints
    nothing [M86 p. 173] because an end of the window is not free; `3 7 2 3` has a remote
    sameness bond but no symmetric window (DECISION D-021)."""
    world, _ = world_with(1, 2, 1)
    assert extent_values(world, "glint3", "Same", "cover") == [1, 2, 1]
    glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    assert extent_values(world, "glint3", "Same", "cover") is None
    world, _ = world_with(3, 7, 2, 3)
    bond_all(world)
    assert extent_values(world, "glint1", "Same", "cover") is None
    assert extent_values(world, "glint1", "Same", "fence") == [3, 7, 2]


def test_an_extent_through_a_pseudo_glom_is_rejected():
    """A bottom-up glom never takes a pseudo-glom (DECISION D-021)."""
    world, recorder = world_with(3, 7, 3)
    cy.make_glom(world, (), [world.find("glint2")], pseudo=True)
    gloms.glomtester(world, "Same", "fence", "glint1")
    assert posted(recorder, "Glommer") == []
    assert recorder.of_kind("stale")[-1].type == "Glomtester"


def test_the_glomtester_sets_out_the_glommer_with_the_members_and_its_own_urgency():
    """`doing task Glomtester on (Same fence glint1)` then `doing task Glommer on (Same
    print-value fence (glint1 glint2))` [M86 p. 165] (DECISION D-021)."""
    world, recorder = world_with(3, 7, 3)
    gloms.glomtester(world, "Same", "fence", "glint1")
    task = posted(recorder, "Glommer")[-1]
    assert task.args == ("Same", "print-value", "fence", ("glint1", "glint2"))
    assert task.urgency == gloms.glomtester_urgency(world, "Same", "fence") == 6


def test_the_glomtester_fails_aloud_when_its_target_is_no_longer_free():
    """`Glomtester failed glint2` for the second `(Pred group glint2)` after `glint2` went
    into `glom1` [M86 p. 170]; a target that is gone fails the same way [M86 p. 67]."""
    out = io.StringIO()
    world, recorder = world_with(1, 2, trace=Trace(out))
    glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    gloms.glomtester(world, "Pred", "group", "glint2")
    assert out.getvalue().splitlines()[-1] == "Glomtester failed glint2"
    gloms.glomtester(world, "Same", "fence", "glom9")
    assert out.getvalue().splitlines()[-1] == "Glomtester failed glom9"
    assert posted(recorder, "Glommer") == []


def test_a_rejected_extent_is_silent():
    """`doing task Glomtester on (Same fence glint3)` prints nothing [M86 p. 173]: "The
    Glomtester either rejects the group as a glom or creates a Glommer task" [M86 p. 67]."""
    out = io.StringIO()
    world, recorder = world_with(1, 2, 1, trace=Trace(out))
    n = len(out.getvalue().splitlines())
    gloms.glomtester(world, "Same", "fence", "glint3")
    assert len(out.getvalue().splitlines()) == n and posted(recorder, "Glommer") == []


# ---- the Glommer [M86 p. 67] -----------------------------------------------------------------

def test_the_glommer_makes_the_glom_prints_it_and_releases_its_tasks():
    """"It creates the new glom and makes it an active focus ... It also creates a
    Glom-inspector task" [M86 p. 67]; `Glommer for Same print-value fence members: (glint1
    glint2)`, `Sparkler-plus on (glom1 glom1 10)`, `Glom-inspector on (glom1)` [M86 p. 165]
    (DECISIONS D-015, D-021)."""
    out = io.StringIO()
    world, recorder = world_with(3, 7, 3, trace=Trace(out))
    gloms.glommer(world, "Same", "print-value", "fence", ("glint1", "glint2"))
    g = world.find("glom1")
    assert g.type == ("Same", "print-value", "fence") and g.print_value == (3, 7)
    assert g.active and g.top_level and world.find("glint1").parent is g
    assert out.getvalue().splitlines()[-1] == \
        "Glommer for Same print-value fence members: (glint1 glint2)"
    # against every element, the glints under it included (`(glom1 glint1 10)` [M86 p. 172])
    plus = sorted(t.args for t in posted(recorder, "Sparkler-plus")[-4:])
    assert plus == [("glom1", "glint1", 10), ("glom1", "glint2", 10), ("glom1", "glint3", 10),
                    ("glom1", "glom1", 2)]
    assert posted(recorder, "Glom-inspector")[-1].args == ("glom1",)
    made = recorder.of_kind("glom")[-1]
    assert made.name == "glom1" and made.members == ("glint1", "glint2")


def test_the_glommer_fails_when_its_members_were_taken():
    """`failed to glom (glint1 glint2 glint3)` after `(3 7)` took `glint1` and `glint2`
    [M86 p. 165]: the members no longer free are dropped from the ends and one is too few
    (DECISION D-021)."""
    out = io.StringIO()
    world, recorder = world_with(3, 7, 3, trace=Trace(out))
    gloms.glommer(world, "Same", "print-value", "fence", ("glint1", "glint2"))
    gloms.glommer(world, "Same", "print-value", "cover", ("glint1", "glint2", "glint3"))
    assert out.getvalue().splitlines()[-1] == "failed to glom (glint1 glint2 glint3)"
    assert len(real_gloms(world)) == 1 and recorder.of_kind("glom-failed")[-1].members == \
        ("glint1", "glint2", "glint3")


def test_the_glommer_refines_by_dropping_taken_ends_and_keeps_a_free_run():
    """`2 1 1 1 3`: a cover `1 1 1` whose first 1 was taken still gloms the free `1 1`;
    a run with a taken member inside fails (DECISION D-021)."""
    world, recorder = world_with(2, 1, 1, 1, 3)
    glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    gloms.glommer(world, "Same", "print-value", "group", ("glint2", "glint3", "glint4"))
    assert [g.print_value for g in real_gloms(world)] == [(2, 1), (1, 1)]
    world, recorder = world_with(2, 1, 1, 1, 3)
    glom(world, "glint3", "glint4")
    gloms.glommer(world, "Same", "print-value", "group", ("glint2", "glint3", "glint4", "glint5"))
    assert recorder.of_kind("glom-failed")[-1].members == ("glint2", "glint3", "glint4", "glint5")


# ---- the Glom-inspector [M86 p. 68] ----------------------------------------------------------

def test_the_interested_classes_by_glom_type():
    """`Plato-scout on (Cycle glom1)` for the fence `(3 7)` [M86 p. 165], `((S-group
    P-group) glom1)` for the group `(1 2)` [M86 p. 170], all six for a top-down glom
    [M86 p. 173]; `(1 1 1)` interests C-group and Y-group [M86 p. 69] (DECISION D-022)."""
    world, recorder = world_with(3, 7, 3)
    g = glom(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
    gloms.glom_inspector(world, g.name)
    assert posted(recorder, "Plato-scout")[-1].args == ("Cycle", "glom1")
    world, recorder = world_with(1, 2)
    g = glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    gloms.glom_inspector(world, g.name)
    assert posted(recorder, "Plato-scout")[-1].args == (("S-group", "P-group"), "glom1")
    world, _ = world_with(1, 1, 1)
    assert gloms.interested(glom(world, "glint1", "glint2", "glint3")) == ("C-group", "Y-group")
    world, _ = world_with(3, 1, 3)
    assert gloms.interested(glom(world, "glint1", "glint2", "glint3",
                                 type=("Same", "print-value", "cover"))) == ("Y-group",)
    world, _ = world_with(1, 2)
    pseudo = cy.make_glom(world, (), [world.find("glint1"), world.find("glint2")], pseudo=True)
    assert gloms.interested(pseudo) == gloms.ALL_SIX


def test_the_inspector_does_nothing_for_a_glom_that_is_gone():
    """"If the glom still exists" [M86 p. 68] (DECISION D-009)."""
    world, recorder = world_with(1, 2)
    gloms.glom_inspector(world, "glom5")
    assert posted(recorder, "Plato-scout") == [] and recorder.of_kind("stale")[-1].type == \
        "Glom-inspector"


# ---- the Plato-scout and dubbing [M86 pp. 68-69] ---------------------------------------------

def test_a_flat_fence_glom_passes_cycle_by_default_and_is_dubbed():
    """"This permits flat potential Tuple and Cycle gloms to be dubbed as such" [M86 p. 69];
    the structure `(Cycle 3 2 (3 7))` with purity `pure` [M86 pp. 165, 168]; "the Platonic
    ideal has this glom added to its list of manifestations"; "Dubbing causes creation of a
    Template-scout" [M86 p. 69]."""
    world, recorder = world_with(3, 7, 3)
    g = glom(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
    gloms.plato_scout(world, "Cycle", "glom1")
    assert g.dubbed and [(str(f), p) for f, p in g.structure] == [("(Cycle 3 2 (3 7))", "pure")]
    assert world.platoplasm.type("Cycle").manifestations == [g]
    assert posted(recorder, "Template-scout")[-1].args == ("glom1",)
    assert posted(recorder, "Plato-evaluator") == []
    dubbed = recorder.of_kind("dubbed")[-1]
    assert (dubbed.name, dubbed.kind_name, dubbed.form, dubbed.purity) == \
        ("glom1", "Cycle", "(Cycle 3 2 (3 7))", "pure")


def test_a_class_whose_test_fails_gets_a_plato_evaluator():
    """`Plato-scout on ((S-group P-group) glom1)` dubs `(1 2)` as `(S-group 1 2)` and sets
    out `Plato-evaluator on (P-group glom1)` [M86 pp. 170-171]."""
    world, recorder = world_with(1, 2)
    g = glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    gloms.plato_scout(world, ("S-group", "P-group"), "glom1")
    assert [(str(f), p) for f, p in g.structure] == [("(S-group 1 2)", "pure")]
    assert posted(recorder, "Plato-evaluator")[-1].args == ("P-group", "glom1")
    assert len(posted(recorder, "Template-scout")) == 1


def test_1_1_1_is_dubbed_both_c_group_and_y_group():
    """"A glom can be dubbed more than once. The glom '(1 1 1)', for example, might be dubbed
    as both a C-group ('copy-group') and a Y-group ('symmetry-group')" [M86 p. 69]."""
    world, recorder = world_with(1, 1, 1)
    g = glom(world, "glint1", "glint2", "glint3")
    gloms.plato_scout(world, ("C-group", "Y-group"), "glom1")
    assert [(str(f), p) for f, p in g.structure] == \
        [("(C-group 1 3)", "pure"), ("(Y-group 1 3 (1 1 1))", "pure")]
    assert world.platoplasm.type("C-group").manifestations == [g]
    assert world.platoplasm.type("Y-group").manifestations == [g]
    assert len(posted(recorder, "Template-scout")) == 2
    gloms.plato_scout(world, "C-group", "glom1")
    assert len(g.structure) == 2 and len(posted(recorder, "Template-scout")) == 2


def test_a_non_flat_glom_is_not_pure_and_goes_to_the_evaluator():
    """"pure <--> an exact manifestation; flat" [M86 p. 69]; a fence class passes by default
    only "if the glom is 'flat'" [M86 p. 69] (DECISION D-023)."""
    world, recorder = world_with(1, 5, 1, 5, 1, 5)
    inner = [glom(world, f"glint{i}", f"glint{i + 1}", type=("Same", "print-value", "fence"))
             for i in (1, 3, 5)]
    outer = cy.make_glom(world, ("Same", "print-value", "group"), inner)
    gloms.plato_scout(world, ("C-group", "Cycle"), outer.name)
    assert not outer.dubbed
    assert [t.args for t in posted(recorder, "Plato-evaluator")] == \
        [("C-group", "glom4"), ("Cycle", "glom4")]


def test_the_scout_does_nothing_for_a_glom_that_is_gone():
    """"If the glom still exists when the Plato-scout is invoked, the scout begins its work"
    [M86 p. 68]."""
    world, recorder = world_with(3, 7, 3)
    gloms.plato_scout(world, "Cycle", "glom1")
    assert recorder.of_kind("stale")[-1].type == "Plato-scout"
    assert posted(recorder, "Template-scout") == []


# ---- the Plato-evaluator [M86 p. 69] ---------------------------------------------------------

def test_1_2_3_2_3_is_broken_into_1_2_3_and_2_3():
    """"'1 2 3 2 3' might well be sent to a Plato-evaluator, which might break it into the two
    pure successorship groups '1 2 3' and '2 3', and send on the gloms for these two groups to
    be dubbed" [M86 p. 69] (DECISION D-023)."""
    world, recorder = world_with(1, 2, 3, 2, 3)
    bond_all(world)
    g = glom(world, "glint1", "glint2", "glint3", "glint4", "glint5",
             type=("Succ", "print-value", "group"))
    pieces = [(label, [x.print_value for x in members]) for label, members in gloms.pure_pieces(world, g)]
    assert pieces == [("Succ", [1, 2, 3]), ("Pred", [3, 2]), ("Succ", [2, 3])]
    gloms.plato_evaluator(world, "S-group", g.name)
    assert world.find(g.name) is None and all(world.find(f"glint{i}").top_level for i in range(1, 6))
    assert [t.args for t in posted(recorder, "Glommer")] == [
        ("Succ", "print-value", "group", ("glint1", "glint2", "glint3")),
        ("Pred", "print-value", "group", ("glint3", "glint4")),
        ("Succ", "print-value", "group", ("glint4", "glint5"))]
    assert posted(recorder, "Burster") == []


def test_the_pieces_of_1_2_3_2_3_are_glommed_and_dubbed_in_a_run():
    """The pieces are sent on "to be dubbed" [M86 p. 69]: after the run, `1 2 3` and `2 3`
    are S-groups for every seed (the `3 2` piece loses its members to them or takes the 3
    from one of them; DECISION D-023)."""
    seen = Counter()
    for seed in range(12):
        world, _ = world_with(1, 2, 3, 2, 3, seed=seed)
        bond_all(world)
        g = glom(world, "glint1", "glint2", "glint3", "glint4", "glint5",
                 type=("Succ", "print-value", "group"))
        world.post("Plato-evaluator", "S-group", g.name)
        world.post("Plato-evaluator", "P-group", g.name)
        run_all(world)
        seen[tuple(sorted((x.print_value, tuple(f.kind for f, _ in x.structure or ()))
                          for x in real_gloms(world)))] += 1
    assert (((1, 2, 3), ("S-group",)), ((2, 3), ("S-group",))) in seen
    assert all(any(pv == (1, 2, 3) or pv == (3, 2) for pv, _ in key) for key in seen)


def test_the_evaluator_leaves_a_dubbed_glom_alone():
    """`Plato-evaluator on (P-group glom1)` after `(1 2)` was dubbed S-group prints nothing
    and sets out nothing [M86 pp. 170-171]: "and the glom has not already been dubbed"
    [M86 p. 69]."""
    world, recorder = world_with(1, 2)
    g = glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    gloms.plato_scout(world, ("S-group", "P-group"), g.name)
    gloms.plato_evaluator(world, "P-group", g.name)
    assert world.find(g.name) is g and g.top_level
    assert posted(recorder, "Burster") == [] and posted(recorder, "Glommer") == []


def test_an_undubbed_flat_glom_with_no_pure_piece_gets_a_burster():
    """"If the Plato-evaluator has no success and the glom has not already been dubbed ...
    A Burster task is created" [M86 p. 69]."""
    world, recorder = world_with(3, 7, 2, 3)
    bond_all(world)
    g = glom(world, "glint1", "glint2", "glint3", type=("Same", "print-value", "fence"))
    gloms.plato_evaluator(world, "Cycle", g.name)
    assert posted(recorder, "Burster")[-1].args == (g.name,)
    assert world.find(g.name) is g


def test_a_non_flat_glom_of_equal_units_is_dubbed_with_secondary_purity():
    """"secondary <--> not flat; e.g., ((1 5) (1 5) (1 5))" [M86 p. 69] (DECISION D-023):
    the units pass the C-group process, and a dubbed unit lends its form."""
    world, recorder = world_with(1, 5, 1, 5, 1, 5)
    inner = [glom(world, f"glint{i}", f"glint{i + 1}", type=("Same", "print-value", "fence"))
             for i in (1, 3, 5)]
    gloms.dub(world, inner[0], "Cycle", "pure")
    outer = cy.make_glom(world, ("Same", "print-value", "group"), inner)
    gloms.plato_evaluator(world, "C-group", outer.name)
    assert [(str(f), p) for f, p in outer.structure] == \
        [("(C-group (Cycle 1 2 (1 5)) 3)", "secondary")]
    assert world.platoplasm.type("C-group").manifestations == [outer]
    gloms.plato_evaluator(world, "Y-group", outer.name)
    assert str(outer.structure[1][0]) == "(Y-group 1 3 ((Cycle 1 2 (1 5)) (Tuple 1 2 (1 5)) " \
                                         "(Tuple 1 2 (1 5))))"
    world, _ = world_with(1, 5, 1, 5)
    inner = [glom(world, f"glint{i}", f"glint{i + 1}", type=("Same", "print-value", "fence"))
             for i in (1, 3)]
    outer = cy.make_glom(world, ("Same", "print-value", "group"), inner)
    gloms.plato_evaluator(world, "C-group", outer.name)
    assert str(outer.structure[0][0]) == "(C-group (Tuple 1 2 (1 5)) 2)"


def test_a_non_flat_glom_that_passes_nothing_gets_a_burster():
    """"and all its non-glint subgloms" [M86 p. 69] is the Burster's business (DECISION
    D-024); a glom that has escaped into another is left alone [M86 p. 70]."""
    world, recorder = world_with(1, 2, 1)
    inner = glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    outer = cy.make_glom(world, ("Pred", "print-value", "group"), [inner, world.find("glint3")])
    gloms.plato_evaluator(world, "S-group", outer.name)
    assert posted(recorder, "Burster")[-1].args == (outer.name,)
    gloms.plato_evaluator(world, "S-group", inner.name)
    assert len(posted(recorder, "Burster")) == 1
    assert recorder.of_kind("stale")[-1].reason == "the glom escaped into another glom"


# ---- the Burster [M86 pp. 69-70] -----------------------------------------------------------------

def test_the_burster_leaves_only_glints():
    """"If a Burster were set on the glom '((1 1) (2 2))', all levels of glomming would be
    destroyed, leaving only the glints '1', '1', '2', '2' in the cytoplasm" [M86 p. 70]."""
    world, recorder = world_with(1, 1, 2, 2)
    outer = cy.make_glom(world, ("Succ", "print-value", "group"),
                         [glom(world, "glint1", "glint2"), glom(world, "glint3", "glint4")])
    gloms.burster(world, outer.name)
    assert real_gloms(world) == [] and [x.print_value for x in cy.lineup(world)] == [1, 1, 2, 2]
    assert recorder.of_kind("burst")[-1].glints == ("glint1", "glint2", "glint3", "glint4")


def test_the_burster_spares_a_glom_that_was_dubbed_or_glommed_meanwhile():
    """"the glom's only escape route is to become invisible in the cytoplasm by glomming
    with another glom" [M86 p. 70] (DECISION D-024)."""
    world, recorder = world_with(1, 2, 1)
    inner = glom(world, "glint1", "glint2", type=("Pred", "print-value", "group"))
    gloms.dub(world, inner, "S-group", "pure")
    gloms.burster(world, inner.name)
    assert world.find(inner.name) is inner
    other = glom(world, "glint3")
    cy.make_glom(world, ("Pred", "print-value", "group"), [inner, other])
    gloms.burster(world, other.name)
    assert world.find(other.name) is other
    gloms.burster(world, "glom9")
    assert [e.type for e in recorder.of_kind("stale")[-3:]] == ["Burster"] * 3


# ---- runs ------------------------------------------------------------------------------------

def outcome(terms, seed):
    world, _ = world_with(*terms, seed=seed, scout=True)
    return sorted((g.type[2], g.print_value, tuple(f.kind for f, _ in g.structure or ()))
                  for g in real_gloms(world))


def test_3_7_3_makes_the_fence_3_7_dubbed_cycle_or_the_cover_3_7_3_dubbed_y_group():
    """Run 1: `(3 7)` is glommed as `(Same print-value fence)` and scouted for Cycle while
    the cover `(glint1 glint2 glint3)` fails [M86 p. 165]. In the reconstruction the two
    proposals race, and the cover wins more often (34 of 100 seeds give the fence; the
    finding is in PROGRESS.md and RESULTS.md): the Glom-scout's draw reaches the fence only
    from `glint1` (DECISIONS D-020, D-021)."""
    seen = Counter(tuple(outcome((3, 7, 3), seed)) for seed in range(60))
    fence = (("fence", (3, 7), ("Cycle",)),)
    cover = (("cover", (3, 7, 3), ("Y-group",)),)
    assert set(seen) == {fence, cover}
    assert seen[fence] >= 12


def test_the_fence_run_prints_the_run_1_lines_in_order():
    """The milestones of [M86 p. 165] on a seed where the fence wins: the proposals from
    `glint1`, the Glommer line, the failed cover, the Plato-scout for Cycle (SPEC 15.5)."""
    for seed in range(60):
        out = io.StringIO()
        world, _ = world_with(3, 7, 3, seed=seed, scout=True, trace=Trace(out))
        lines = out.getvalue().splitlines()
        if "Glommer for Same print-value fence members: (glint1 glint2)" in lines:
            break
    else:
        pytest.fail("no seed in 60 made the fence first")
    wanted = ["Same -cover proposed --> glint1", "Same -fence proposed --> glint1",
              "doing task Glomtester on (Same fence glint1)",
              "doing task Glommer on (Same print-value fence (glint1 glint2))",
              "Glommer for Same print-value fence members: (glint1 glint2)",
              "doing task Glom-inspector on (glom1)", "doing task Plato-scout on (Cycle glom1)",
              "doing task Template-scout on (glom1)"]
    positions = [lines.index(w) for w in wanted]
    assert positions == sorted(positions)
    g = world.find("glom1")
    assert str(g.structure[0][0]) == "(Cycle 3 2 (3 7))"
    if "doing task Glommer on (Same print-value cover (glint1 glint2 glint3))" in lines:
        assert "failed to glom (glint1 glint2 glint3)" in lines


def test_1_2_makes_the_group_scouted_for_s_group_and_p_group_for_every_seed():
    """Run 3: `(1 2)` is glommed as a group, `Plato-scout on ((S-group P-group) glom1)`,
    `Plato-evaluator on (P-group glom1)`, dubbed `(S-group 1 2)` [M86 pp. 170-171]; the label
    is `Pred` when scouted from `glint2`, `Succ` from `glint1` (DECISION D-020)."""
    labels = Counter()
    for seed in range(30):
        out = io.StringIO()
        world, _ = world_with(1, 2, seed=seed, scout=True, trace=Trace(out))
        (g,) = real_gloms(world)
        assert g.print_value == (1, 2) and str(g.structure[0][0]) == "(S-group 1 2)"
        labels[g.type[0]] += 1
        lines = out.getvalue().splitlines()
        assert "doing task Plato-scout on ((S-group P-group) glom1)" in lines
        assert "doing task Plato-evaluator on (P-group glom1)" in lines
        assert f"Glommer for {g.type[0]} print-value group members: (glint1 glint2)" in lines
    assert labels["Pred"] > 0 and labels["Succ"] > 0


def test_a_second_glomtester_on_a_taken_glint_prints_the_failure_in_a_run():
    """Two scouts proposed `Pred -group` for `glint2`; the second Glomtester prints
    `Glomtester failed glint2` [M86 p. 170]."""
    found = False
    for seed in range(60):
        out = io.StringIO()
        world_with(1, 2, seed=seed, scout=True, trace=Trace(out))
        lines = out.getvalue().splitlines()
        testers = [i for i, l in enumerate(lines) if l.endswith("Glomtester on (Pred group glint2)")]
        glommed = next((i for i, l in enumerate(lines) if l.startswith("Glommer for")), None)
        if len(testers) >= 2 and glommed is not None and glommed < testers[-1]:
            assert lines[testers[-1] + 1] == "Glomtester failed glint2"
            found = True
        elif len(testers) >= 2:
            # both Glomtesters ran before the glom was made: the second Glommer fails instead
            assert "failed to glom (glint1 glint2)" in lines
    assert found, "no seed in 60 ran a second Glomtester after the glom was made"


def test_same_seed_same_run():
    """Reproducibility through the one `Rng`."""
    runs = []
    for _ in range(2):
        out = io.StringIO()
        world_with(5, 3, 1, 6, 1, 3, 4, seed=7, scout=True, trace=Trace(out))
        runs.append(out.getvalue())
    assert runs[0] == runs[1]
    out = io.StringIO()
    world_with(5, 3, 1, 6, 1, 3, 4, seed=8, scout=True, trace=Trace(out))
    assert out.getvalue() != runs[0]


def test_the_glomtester_examples_as_runs_with_all_terms_present():
    """The four examples of [M86 p. 67] as whole runs: with the terms entered first and the
    bonds made, one Glom-scout on the thesis's target proposes, and the Glommers make, the
    suggested glom (the `3 2` piece aside)."""
    cases = [((2, 1, 1, 1, 3), ("Same", "group", "glint3"), (1, 1, 1)),
             ((9, 1, 2, 5, 9), ("Same", "fence", "glint1"), (9, 1, 2, 5)),
             ((5, 3, 1, 6, 1, 3, 4), ("Same", "cover", "glint4"), (3, 1, 6, 1, 3)),
             ((1, 2, 3, 2, 3), ("Pred", "group", "glint2"), (1, 2, 3, 2, 3))]
    for terms, proposal, expected in cases:
        world, recorder = world_with(*terms)
        bond_all(world)
        element = world.find(proposal[2])
        assert proposal[:2] in gloms.quicktests(world, element)
        world.post("Glomtester", *proposal)
        run_all(world)
        made = recorder.of_kind("glom")[0]
        assert made.print_value == expected, (terms, made)
        values = [g.print_value for g in real_gloms(world)]
        if expected != (1, 2, 3, 2, 3):
            assert expected in values, (terms, values)
        else:
            # the chain fails S-group and P-group and is broken into pieces [M86 p. 69]
            assert any(v in values for v in [(1, 2, 3), (3, 2)]), (terms, values)

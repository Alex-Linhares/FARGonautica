"""Sparkler, Sparkler-plus, Tester and Bonder, and what a new term releases
[M86 pp. 61-65, 164-165, 170] (DECISIONS D-015, D-016, D-018, D-019, D-024, D-025, D-058)."""

import io

import pytest

from seekwhence import bonds
from seekwhence import cytoplasm as cy
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.run import TASKS, new_world, run_until_prompt
from seekwhence.trace import Trace


def world_with(*terms, seed=1, config=None, trace=None):
    recorder = Recorder()
    observers = [recorder] if trace is None else [recorder, trace]
    world = new_world(config, seed=seed, observers=observers)
    world.handlers["Glom-scout"] = lambda w, *args: None
    for term in terms:
        bonds.new_term(world, term)
    return world, recorder


def glom(world, *names, type=("Succ", "print-value", "group"), pseudo=False):
    return cy.make_glom(world, type, [world.find(n) for n in names], pseudo=pseudo)


def posted(recorder, type):
    return [e for e in recorder.of_kind("posted") if e.type == type]


def run_all(world):
    while run_until_prompt(world) != "empty":
        pass


# ---- the handlers are installed -------------------------------------------------------------

def test_the_cyto_tasks_are_registered():
    """Each module adds its handlers to `run.TASKS` (SPEC 2.4)."""
    for type in ("Sparkler", "Sparkler-plus", "Tester", "Bonder", "Dissolver"):
        assert TASKS[type] is bonds.TASKS[type]
    assert new_world().handlers["Tester"] is bonds.tester


# ---- a new term [M86 pp. 61, 164, 170] ---------------------------------------------------------

def test_the_first_term_posts_one_self_sparkler_plus_and_a_bare_dissolver():
    """`doing task Sparkler-plus on (glint1 glint1 2)` and `doing task Dissolver` after the
    first term [M86 pp. 164, 170] (DECISIONS D-015, D-024)."""
    world, recorder = world_with(3)
    tasks = [(e.type, e.args, e.urgency) for e in recorder.of_kind("posted")]
    assert tasks == [("Sparkler-plus", ("glint1", "glint1", 2), 2), ("Dissolver", (), 1)]
    assert world.find("glint1").active


def test_the_second_term_posts_sparkler_plus_against_each_element_and_itself():
    """`(glint2 glint1 10)`, `(glint2 glint2 2)` [M86 p. 164]: 10 for distinct real
    elements, 2 for a self-pair (DECISION D-015); plain Sparklers once there is a pair
    (DECISION D-016); a Dissolver on the previous focus (DECISION D-024)."""
    world, recorder = world_with(3, 7)
    after = [(e.type, e.args, e.urgency) for e in recorder.of_kind("posted")][2:]
    assert ("Sparkler-plus", ("glint2", "glint2", 2), 2) in after
    assert ("Sparkler-plus", ("glint2", "glint1", 10), 10) in after
    assert after.count(("Sparkler", (), 5)) == Config().sparklers_per_term
    assert ("Dissolver", ("glint1",), 1) in after
    assert ("Dissolver", (), 1) not in after
    assert not world.find("glint1").active and world.find("glint2").active


def test_a_pseudo_glom_gets_the_low_sparkler_plus_number():
    """`(glint3 glom3 2)` for a glint against the template's pseudo-glom, and `(glint3
    glint1 10)`, `(glint3 glint2 10)`, `(glom1 glint3 10)` against the elements nested under
    it [M86 p. 172] (DECISION D-015 as amended in item 20: every element at any level)."""
    world, recorder = world_with(3, 7)
    g = glom(world, "glint1", "glint2")
    p = glom(world, g.name, type=(), pseudo=True)
    bonds.new_term(world, 3)
    plus = [(e.args, e.urgency) for e in posted(recorder, "Sparkler-plus")
            if e.args[0] == "glint3"]
    assert ((("glint3", p.name, 2), 2)) in plus
    assert ((("glint3", "glint3", 2), 2)) in plus
    assert ((("glint3", "glint1", 10), 10)) in plus
    assert ((("glint3", "glint2", 10), 10)) in plus
    assert ((("glint3", g.name, 10), 10)) in plus
    assert len(plus) == 5


def test_sparklers_per_term_is_a_knob():
    world, recorder = world_with(3, 7, config=Config(sparklers_per_term=5))
    assert len(posted(recorder, "Sparkler")) == 5


def test_the_dissolver_goes_on_every_focus_that_ends():
    """`Dissolver on (glom3)` [M86 p. 172]: a glom that was an active focus (DECISION D-024)."""
    world, recorder = world_with(1, 2)
    g = glom(world, "glint1", "glint2")
    g.active = True
    bonds.new_term(world, 1)
    names = [e.args for e in posted(recorder, "Dissolver")]
    assert names == [(), ("glint1",), (g.name,)]


# ---- sparks [M86 p. 63] ---------------------------------------------------------------------

def test_a_spark_posts_its_testers_and_is_printed():
    """"a horde of 'Testers' is placed on the taskrack" [M86 p. 64]: one per spark
    (DECISION D-016); `spark1 --- between glint1glint2` [M86 p. 164]."""
    trace = Trace(io.StringIO())
    world, recorder = world_with(3, 7, trace=trace)
    spark = bonds.make_spark(world, world.find("glint1"), world.find("glint2"))
    assert spark.name == "spark1" and bonds.sparks(world) == [spark]
    assert len(posted(recorder, "Tester")) == 1
    assert trace.lines[-1] == "spark1 --- between glint1glint2"
    event = recorder.of_kind("spark")[0]
    assert (event.name, event.a, event.b) == ("spark1", "glint1", "glint2")


def test_the_sparkler_draws_two_elements_and_sparks_distinct_ones():
    """"pulls those gloms at random from the cytoplasm" [M86 p. 63]; the same element
    twice makes no spark (DECISION D-025)."""
    world, recorder = world_with(3, 7, 3)
    for _ in range(40):
        bonds.sparkler(world)
    made = recorder.of_kind("spark")
    stale = [e for e in recorder.of_kind("stale") if e.type == "Sparkler"]
    assert made and stale and len(made) + len(stale) == 40
    pairs = {(e.a, e.b) for e in made}
    assert all(a != b for a, b in pairs)
    assert len(pairs) > 1, "both orders and several pairs over 40 draws"


def test_the_sparkler_needs_two_elements_and_skips_pseudo_gloms_and_subgloms():
    """"The Sparkler simply looks for gloms that are not subgloms of each other"
    [M86 p. 63]: any two elements at any level but a glom and its own subglom, pseudo-gloms
    excluded (DECISION D-025 as amended in item 20)."""
    world, recorder = world_with(3)
    bonds.sparkler(world)
    assert not recorder.of_kind("spark")
    bonds.new_term(world, 7)
    bonds.new_term(world, 3)
    g = glom(world, "glint1", "glint2")
    for _ in range(60):
        bonds.sparkler(world)
    pairs = {frozenset((e.a, e.b)) for e in recorder.of_kind("spark")}
    nested = {frozenset((g.name, "glint1")), frozenset((g.name, "glint2"))}
    assert not pairs & nested
    assert frozenset((g.name, "glint3")) in pairs
    assert frozenset(("glint1", "glint3")) in pairs      # nested under g, still sparked
    p = glom(world, g.name, "glint3", type=(), pseudo=True)
    before = len(recorder.of_kind("spark"))
    for _ in range(30):
        bonds.sparkler(world)
    after = recorder.of_kind("spark")[before:]
    assert after and all(p.name not in (e.a, e.b) for e in after)
    assert all(frozenset((e.a, e.b)) not in nested for e in after)


def test_sparkler_plus_sparks_distinct_existing_unnested_elements():
    """`Sparkler-plus on (glint2 glint1 10)` then `spark3 --- between glint2glint1`;
    `(glint2 glint2 2)` makes no spark [M86 p. 164] (DECISION D-015)."""
    world, recorder = world_with(3, 7)
    bonds.sparkler_plus(world, "glint2", "glint1", 10)
    event = recorder.of_kind("spark")[-1]
    assert (event.a, event.b) == ("glint2", "glint1")
    n = len(recorder.of_kind("spark"))
    bonds.sparkler_plus(world, "glint2", "glint2", 2)
    bonds.sparkler_plus(world, "glint2", "glint9", 10)
    g = glom(world, "glint1", "glint2")
    bonds.sparkler_plus(world, g.name, "glint1", 10)
    assert len(recorder.of_kind("spark")) == n
    reasons = [e.reason for e in recorder.of_kind("stale") if e.type == "Sparkler-plus"]
    assert len(reasons) == 3 and "subglom" in reasons[-1]


def test_the_same_pair_may_be_sparked_repeatedly():
    """`spark1`, `spark2` both between glint1 and glint2 [M86 p. 167] (DECISION D-025)."""
    world, recorder = world_with(3, 7)
    bonds.sparkler_plus(world, "glint2", "glint1", 10)
    bonds.sparkler_plus(world, "glint2", "glint1", 10)
    assert [e.name for e in recorder.of_kind("spark")] == ["spark1", "spark2"]


# ---- the bond tests [M86 pp. 63-64] -------------------------------------------------------------

def passed_between(world, a, b):
    return bonds.bond_tests(world.find(a), world.find(b))


def test_glints_same_succ_pred():
    """`1 2` Succ, `2 1` Pred, `1 _ 1` Same [M86 p. 174]; "the glints '1' and '1' might
    very well be bondable, since they have the same print-value" [M86 p. 63]."""
    world, _ = world_with(1, 2, 1, 5)
    assert passed_between(world, "glint1", "glint2") == [("Succ", "print-value")]
    assert passed_between(world, "glint2", "glint1") == [("Succ", "print-value")]
    assert passed_between(world, "glint2", "glint3") == [("Pred", "print-value")]
    assert passed_between(world, "glint1", "glint3") == [("Same", "print-value")]
    assert passed_between(world, "glint1", "glint4") == []
    assert passed_between(world, "glint3", "glint4") == []


def test_a_glint_and_a_glom_bond_by_adjacent_successorship():
    """"The glint '1' might be bonded to the glom '(2 3)' by reason of adjacent successorship
    -- 2 is the successor of 1, and the structures in question are adjacent" [M86 p. 63]
    (DECISION D-018: the end of the glom that faces the glint)."""
    world, _ = world_with(1, 2, 3, 2)
    g = glom(world, "glint2", "glint3")
    assert bonds.bond_tests(world.find("glint1"), g) == [("Succ", "print-value")]
    assert cy.modifier(world.find("glint1"), g) == "adjacent"
    assert bonds.bond_tests(g, world.find("glint4")) == [("Pred", "print-value")]


def test_two_equal_gloms_pass_same_print_value_and_same_span():
    """"gloms '(1 2)' and '(1 2)' might engender both 'same print-value' and 'same span'
    Bonders" [M86 p. 64]."""
    world, _ = world_with(1, 2, 1, 2)
    g12 = glom(world, "glint1", "glint2")
    g34 = glom(world, "glint3", "glint4")
    assert bonds.bond_tests(g12, g34) == [("Same", "print-value"), ("Same", "span")]


def test_gloms_of_equal_span_bond_by_span_and_by_term_wise_successorship():
    """"two gloms such as '(1 2 3)' and '(7 8 9)' or '(1 2 3)' and '(8 1 4)' will generally
    be bonded" [M86 p. 65]: by their same span; `(1 2)` and `(2 3)` also by Succ
    (DECISION D-018)."""
    world, _ = world_with(1, 2, 3, 7, 8, 9, 8, 1, 4, 2, 3)
    g123 = glom(world, "glint1", "glint2", "glint3")
    g789 = glom(world, "glint4", "glint5", "glint6")
    g814 = glom(world, "glint7", "glint8", "glint9")
    g23 = glom(world, "glint10", "glint11")
    assert bonds.bond_tests(g123, g789) == [("Same", "span")]
    assert bonds.bond_tests(g123, g814) == [("Same", "span")]
    assert bonds.bond_tests(g123, g23) == []
    world2, _ = world_with(1, 2, 2, 3)
    a = glom(world2, "glint1", "glint2")
    b = glom(world2, "glint3", "glint4")
    assert bonds.bond_tests(a, b) == [("Succ", "print-value"), ("Same", "span")]


def test_glints_never_pass_same_span():
    """"The 'span' field is really unnecessary in glints" [M86 p. 61] (DECISION D-018)."""
    world, _ = world_with(3, 7)
    assert ("Same", "span") not in passed_between(world, "glint1", "glint2")


# ---- the Tester [M86 p. 64] -------------------------------------------------------------------

def test_the_tester_takes_a_spark_and_posts_one_bonder_per_test_passed():
    """"One Bonder task will be created for each bonding test passed by the two gloms"
    [M86 p. 64], with the transcripts' arguments `(Same print-value (remote) glint1 glint3)`
    [M86 p. 164]; the spark is used up (DECISION D-058)."""
    world, recorder = world_with(3, 7, 3)
    bonds.make_spark(world, world.find("glint3"), world.find("glint1"))
    bonds.tester(world)
    assert bonds.sparks(world) == []
    bonders = posted(recorder, "Bonder")
    assert [e.args for e in bonders] == [("Same", "print-value", ("remote",), "glint1", "glint3")]
    assert bonders[0].urgency == 10


def test_the_tester_draws_one_of_the_flying_sparks_at_random():
    """"each Tester chooses some spark, not necessarily the one whose creation caused the
    tester's creation" [M86 p. 64] (DECISION D-058)."""
    chosen = set()
    for seed in range(20):
        world, recorder = world_with(1, 2, 1, seed=seed)
        bonds.make_spark(world, world.find("glint1"), world.find("glint2"))  # Succ
        bonds.make_spark(world, world.find("glint1"), world.find("glint3"))  # Same
        bonds.tester(world)
        assert len(bonds.sparks(world)) == 1
        chosen.add(posted(recorder, "Bonder")[0].args[0])
    assert chosen == {"Succ", "Same"}


def test_a_tester_with_no_spark_does_nothing():
    world, recorder = world_with(3, 7)
    bonds.tester(world)
    assert not posted(recorder, "Bonder")
    assert recorder.of_kind("stale")[-1].type == "Tester"


def test_a_spark_whose_glom_vanished_makes_nothing():
    """"The spark's members ... are tested to ascertain if they are currently in existence
    (recall that gloms are ephemeral)" [M86 p. 64] (DECISION D-009)."""
    world, recorder = world_with(1, 2, 1, 2)
    g12 = glom(world, "glint1", "glint2")
    g34 = glom(world, "glint3", "glint4")
    spark = bonds.make_spark(world, g12, g34)
    world.cytoplasm.remove(g12.name)
    bonds.tester(world)
    assert not posted(recorder, "Bonder")
    assert spark.name not in world.cytoplasm
    stale = recorder.of_kind("stale")[-1]
    assert stale.type == "Tester" and "gone" in stale.reason


def test_a_spark_whose_members_became_nested_makes_nothing():
    """"no glom can be bonded to one of its own subgloms, so the glom '(1 (1 1))' cannot be
    bonded to the subglom '(1 1)' in any way" [M86 p. 63]."""
    world, recorder = world_with(1, 1, 1)
    inner = glom(world, "glint2", "glint3", type=("Same", "print-value", "group"))
    bonds.make_spark(world, world.find("glint1"), inner)
    outer = glom(world, "glint1", inner.name, type=("Same", "print-value", "group"))
    bonds.make_spark(world, outer, inner)
    bonds.tester(world)
    bonds.tester(world)
    assert not posted(recorder, "Bonder")
    assert bonds.bond_tests(outer, inner) == []  # no feature in common anyway
    assert cy.nested(outer, inner)


# ---- the Bonder [M86 pp. 64-65] ----------------------------------------------------------------

def test_the_bonder_makes_the_bond_prints_it_and_releases_sparklers_and_glom_scouts():
    """`doing task Bonder on (Same print-value (remote) glint1 glint3)` then `bond1 ---
    between glint1glint3` [M86 p. 164]; "Creation of a bond causes the release of more
    Sparklers ... and causes the release of some Glom-scouts" [M86 p. 65] (DECISION D-016)."""
    trace = Trace(io.StringIO())
    world, recorder = world_with(3, 7, 3, trace=trace)
    n_sparklers, n_scouts = len(posted(recorder, "Sparkler")), len(posted(recorder, "Glom-scout"))
    bonds.bonder(world, "Same", "print-value", ("remote",), "glint1", "glint3")
    bond = world.find("bond1")
    assert (bond.type, bond.field, bond.modifier) == ("Same", "print-value", "remote")
    assert [m.name for m in bond.members] == ["glint1", "glint3"]
    assert world.find("glint1").bonds_in == [bond] and world.find("glint3").bonds_in == [bond]
    assert trace.lines[-1] == "bond1 --- between glint1glint3"
    assert len(posted(recorder, "Sparkler")) == n_sparklers + Config().sparklers_per_bond
    assert len(posted(recorder, "Glom-scout")) == n_scouts + Config().glom_scouts_per_bond
    event = recorder.of_kind("bond")[0]
    assert (event.name, event.type, event.modifier, event.a, event.b, event.strength) == \
        ("bond1", "Same", "remote", "glint1", "glint3", 5)


def test_no_duplicate_bonds():
    """"2) checks to see that they are not already bonded in this way" [M86 p. 64]; the
    Bonder of run 3 is re-run four times and prints `bond1` once [M86 p. 170]."""
    world, recorder = world_with(1, 2)
    for _ in range(4):
        bonds.bonder(world, "Succ", "print-value", ("adjacent",), "glint1", "glint2")
    assert [e.name for e in recorder.of_kind("bond")] == ["bond1"]
    assert len(world.find("glint1").bonds_in) == 1
    stale = [e for e in recorder.of_kind("stale") if e.type == "Bonder"]
    assert len(stale) == 3 and all("already bonded" in e.reason for e in stale)
    # The other order of the pair is the same way.
    bonds.bonder(world, "Succ", "print-value", ("adjacent",), "glint2", "glint1")
    assert len(recorder.of_kind("bond")) == 1
    # Another field or type is another way.
    bonds.make_bond(world, "Same", "print-value", "adjacent", world.find("glint1"),
                    world.find("glint2"))
    assert len(world.find("glint1").bonds_in) == 2


def test_the_bonder_checks_that_both_elements_still_exist():
    """"1) checks to see that both gloms still exist" [M86 p. 64] (DECISION D-009)."""
    world, recorder = world_with(1, 2, 1, 2)
    g12 = glom(world, "glint1", "glint2")
    g34 = glom(world, "glint3", "glint4")
    cy.dissolve(world, g34)
    bonds.bonder(world, "Same", "print-value", ("adjacent",), g12.name, g34.name)
    assert not recorder.of_kind("bond")
    assert recorder.of_kind("stale")[-1].reason == "an element is gone"


def test_bond_strengths_follow_type_modifier_and_field():
    """"Bond strength is derived from the bond type ..., any bond modifiers ... and the glom
    characteristic ... that is the subject of the bond" [M86 pp. 64-65] (DECISION D-019)."""
    world, _ = world_with(1)
    assert bonds.bond_strength(world, "Same", "adjacent", "print-value") == 10
    assert bonds.bond_strength(world, "Same", "remote", "print-value") == 5
    assert bonds.bond_strength(world, "Succ", "adjacent", "print-value") == 8
    assert bonds.bond_strength(world, "Pred", "remote", "print-value") == 4
    assert bonds.bond_strength(world, "Same", "adjacent", "span") == 2
    assert bonds.bond_strength(world, "Same", "remote", "span") == 1
    assert all(bonds.bond_strength(world, "Same", m, "span") < Config().glom_bond_minimum
               for m in ("adjacent", "remote")), "span bonds never cause glomming [M86 p. 62]"
    world2, _ = world_with(1, config=Config(bond_type_weights=(("Same", 4), ("Succ", 1),
                                                                ("Pred", 1))))
    assert bonds.bond_strength(world2, "Same", "adjacent", "print-value") == 4


# ---- the Dissolver [M86 pp. 70, 164, 172] -----------------------------------------------------

def test_the_dissolver_on_a_glint_or_without_argument_does_nothing():
    """`doing task Dissolver` and `doing task Dissolver on (glint1)` [M86 p. 164]."""
    world, recorder = world_with(3, 7)
    bonds.dissolver(world)
    bonds.dissolver(world, "glint1")
    assert set(world.cytoplasm.names) == {"glint1", "glint2"}
    assert not recorder.of_kind("message")


def test_the_dissolver_says_when_its_glom_is_gone():
    """`glom3 is not in cytoplasm` [M86 p. 172] (DECISION D-009)."""
    trace = Trace(io.StringIO())
    world, recorder = world_with(1, 2, trace=trace)
    g = glom(world, "glint1", "glint2")
    cy.dissolve(world, g)
    bonds.dissolver(world, g.name)
    assert trace.lines[-1] == "glom1 is not in cytoplasm"


def test_the_dissolver_dissolves_a_free_undubbed_inactive_glom_and_spares_the_rest():
    """DECISION D-024: free, not dubbed, not a pseudo-glom, not an active focus."""
    world, recorder = world_with(1, 2, 1, 2)
    g = glom(world, "glint1", "glint2")
    g.active = True
    bonds.dissolver(world, g.name)
    assert g.name in world.cytoplasm
    g.active = False
    g.structure = ("S-group", 1, 2)
    bonds.dissolver(world, g.name)
    assert g.name in world.cytoplasm
    g.structure = None
    bonds.dissolver(world, g.name)
    assert g.name not in world.cytoplasm
    event = recorder.of_kind("dissolved")[0]
    assert (event.name, event.freed) == (g.name, ("glint1", "glint2"))
    h = glom(world, "glint3", "glint4")
    p = glom(world, h.name, type=(), pseudo=True)
    bonds.dissolver(world, h.name)
    bonds.dissolver(world, p.name)
    assert h.name in world.cytoplasm and p.name in world.cytoplasm


# ---- whole runs of the cyto level ---------------------------------------------------------------

def test_3_7_3_makes_the_remote_sameness_bond_for_every_seed():
    """"bond1 --- between glint1glint3" after the third term of `3 7 3 7` [M86 p. 164], and
    no other bond: for 30 seeds the one bond is `Same print-value (remote)` on glint1 and
    glint3."""
    for seed in range(30):
        world, recorder = world_with(seed=seed)
        for term in (3, 7, 3):
            bonds.new_term(world, term)
            run_all(world)
        made = recorder.of_kind("bond")
        assert len(made) == 1, seed
        assert (made[0].type, made[0].modifier, made[0].a, made[0].b) == \
            ("Same", "remote", "glint1", "glint3")
        assert "bond1" in world.cytoplasm
        assert bonds.sparks(world) == [], "every spark was tested"


def test_the_run_1_opening_lines_and_task_types():
    """The trace of `3 7 3` prints the first term's two lines as run 1 does and only the task
    types of its first three terms (and the Glom-scouts the bond releases) [M86 p. 164]."""
    trace = Trace(io.StringIO())
    world, recorder = world_with(trace=trace)
    bonds.new_term(world, 3)
    run_all(world)
    assert trace.lines[:6] == ["-> (startup)", "", "please enter a term: 3",
                               "doing task Sparkler-plus on (glint1 glint1 2)",
                               "doing task Dissolver", "doing task empty-task"]
    for term in (7, 3):
        bonds.new_term(world, term)
        run_all(world)
    types = {e.type for e in recorder.of_kind("chosen")}
    assert types == {"Sparkler-plus", "Dissolver", "empty-task", "Sparkler", "Tester",
                     "Bonder", "Glom-scout"}
    assert "doing task Bonder on (Same print-value (remote) glint1 glint3)" in trace.lines
    assert "bond1 --- between glint1glint3" in trace.lines
    sparks_printed = [l for l in trace.lines if l.startswith("spark")]
    assert [l.split()[0] for l in sparks_printed] == \
        [f"spark{i}" for i in range(1, len(sparks_printed) + 1)]


def test_the_same_seed_gives_the_same_run_and_another_seed_another():
    """A run is reproducible from its seed."""
    def lines(seed):
        trace = Trace(io.StringIO())
        world, _ = world_with(trace=trace, seed=seed)
        for term in (1, 2, 1, 2, 3):
            bonds.new_term(world, term)
            run_all(world)
        return trace.lines
    assert lines(7) == lines(7)
    assert lines(7) != lines(8)


def test_1_2_1_2_3_makes_the_expected_bonds_and_no_more():
    """Every pair that passes a test is bonded once, when every task runs: Succ 1-2, Pred
    2-1, Succ 1-2, Succ 2-3 (adjacent) and the remote Same and Succ/Pred bonds."""
    world, recorder = world_with()
    for term in (1, 2, 1, 2, 3):
        bonds.new_term(world, term)
        run_all(world)
    made = {(e.type, e.modifier, e.a, e.b) for e in recorder.of_kind("bond")}
    expected = set()
    names = ["glint1", "glint2", "glint3", "glint4", "glint5"]
    for i, a in enumerate(names):
        for b in names[i + 1:]:
            ga, gb = world.find(a), world.find(b)
            for type, field in bonds.bond_tests(ga, gb):
                expected.add((type, cy.modifier(ga, gb), a, b))
    assert made == expected
    assert ("Same", "remote", "glint1", "glint3") in made
    assert ("Succ", "adjacent", "glint4", "glint5") in made
    assert len(made) == len(recorder.of_kind("bond")), "no duplicates"


def test_cleaning_on_a_term_removes_old_cyto_tasks():
    """DECISION D-010: the Sparklers, Testers and Dissolvers posted before the previous term
    are removed when a term is entered; here nothing runs between terms."""
    world, recorder = world_with(3, 7)
    assert recorder.of_kind("term")[-1].cleaned == 0
    bonds.new_term(world, 3)
    # The first term's Sparkler-plus and Dissolver (posted before the previous term) go;
    # the second term's tasks stay.
    assert recorder.of_kind("term")[-1].cleaned == 2
    assert len(world.taskrack) > 0
    kept = {t.at_term for t in world.taskrack.tasks}
    assert kept == {2, 3}


@pytest.mark.parametrize("seed", [0, 1, 2])
def test_elements_under_a_glom_are_sparked_but_never_with_their_own_glom_in_a_run(seed):
    """A new glint is sparked against the glom and against the glints under it, as the third
    term of run 3 is (`spark6 --- between glint3glint1`, `spark8 --- between glom1glint3`
    with glint1 under glom1 [M86 p. 172]); a glom is never sparked with its own subglom
    [M86 p. 63] (DECISION D-025 as amended in item 20)."""
    world, recorder = world_with(1, 2, seed=seed)
    run_all(world)
    g = glom(world, "glint1", "glint2")
    before = len(recorder.of_kind("spark"))
    bonds.new_term(world, 1)
    run_all(world)
    after = recorder.of_kind("spark")[before:]
    pairs = {frozenset((e.a, e.b)) for e in after}
    assert frozenset((g.name, "glint3")) in pairs
    assert frozenset(("glint1", "glint3")) in pairs and frozenset(("glint2", "glint3")) in pairs
    assert frozenset((g.name, "glint1")) not in pairs and frozenset((g.name, "glint2")) not in pairs


def test_the_dissolver_goes_on_the_top_level_holder_of_the_previous_glint():
    """`Dissolver on (glom3)` with glint2 under the bottom-up glom1 under the gnoth-glom
    glom3 [M86 p. 172]; `Dissolver on (glom8)` and `on (glom10)`, the caps of glint3 and
    glint4, after terms 4 and 5 [M86 p. 176] (DECISION D-024 as amended in item 21)."""
    world, recorder = world_with(1, 2)
    g = glom(world, "glint1", "glint2")
    cap = glom(world, g.name, pseudo=True)
    bonds.new_term(world, 1)
    assert [e.args for e in posted(recorder, "Dissolver")] == [(), ("glint1",), (cap.name,)]
    glint3 = world.find("glint3")
    cap3 = glom(world, glint3.name, pseudo=True)
    bonds.new_term(world, 2)
    assert posted(recorder, "Dissolver")[-1].args == (cap3.name,)
    bonds.new_term(world, 3)
    assert posted(recorder, "Dissolver")[-1].args == ("glint4",)

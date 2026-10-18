"""The four cyto-level structures: glints, gloms, sparks and bonds [M86 pp. 60-65], and
dissolving and bursting [M86 pp. 62, 70]."""

import pytest

from seekwhence import bonds
from seekwhence import cytoplasm as cy
from seekwhence.events import Recorder
from seekwhence.run import new_world


def world_with(*terms, seed=1):
    recorder = Recorder()
    world = new_world(seed=seed, observers=[recorder])
    world.handlers["Glom-scout"] = lambda w, *args: None
    for term in terms:
        bonds.new_term(world, term)
    return world, recorder


def glom(world, *names, type=("Same", "print-value", "group"), pseudo=False):
    return cy.make_glom(world, type, [world.find(n) for n in names], pseudo=pseudo)


# ---- glints [M86 p. 61] ----------------------------------------------------------------------

def test_the_glint3_example_of_page_61():
    """"if the terms '1 2 2 3' had been entered, the second 2 might be represented as
    follows: class: Glints name: glint3 print-value: 2 position: 3 span: 1 left-nbr: glint2
    right-nbr: glint4" [M86 p. 61]."""
    world, _ = world_with(1, 2, 2, 3)
    glint3 = world.find("glint3")
    assert glint3.show_fields() == (("class", "Glints"), ("name", "glint3"),
                                    ("print-value", 2), ("position", 3), ("span", 1),
                                    ("left-nbr", "glint2"), ("right-nbr", "glint4"))
    assert glint3.cls == "Glints" and glint3.is_glint and glint3.top_level
    assert glint3.positions_covered == (3, 3) and glint3.flat_values() == (2,)


def test_a_glint_is_listed_as_a_manifestation_of_its_ideal_atom():
    """"glint3 would become a manifestation of ideal2 because its value is 2 and it
    represents an input integer" [M86 p. 61]."""
    world, recorder = world_with(1, 2, 2, 3)
    glint3 = world.find("glint3")
    atom = world.platoplasm.atom(2)
    assert glint3.atom is atom and atom.name == "ideal2"
    assert [g.name for g in atom.manifestations] == ["glint2", "glint3"]
    made = recorder.of_kind("glint")
    assert [(e.name, e.value, e.position) for e in made] == [
        ("glint1", 1, 1), ("glint2", 2, 2), ("glint3", 2, 3), ("glint4", 3, 4)]


def test_the_neighbour_fields_at_the_ends():
    """The first glint has no left neighbour and the last no right one yet [M86 p. 61]."""
    world, _ = world_with(5)
    assert world.find("glint1").left_nbr is None and world.find("glint1").right_nbr is None
    bonds.new_term(world, 6)
    assert world.find("glint1").right_nbr == "glint2"
    assert world.find("glint2").left_nbr == "glint1"


def test_glints_are_indestructible():
    """"Each glint is indestructible -- an 'atomic' glom" [M86 p. 62]."""
    world, _ = world_with(3, 7)
    with pytest.raises(ValueError, match="indestructible"):
        cy.dissolve(world, world.find("glint1"))
    bonds.dissolver(world, "glint1")
    assert "glint1" in world.cytoplasm


# ---- gloms [M86 p. 62] ------------------------------------------------------------------------

def test_the_glom7_example_of_page_62():
    """"in the sequence segment '8 2 2 2 8', the three 2's might be represented jointly as a
    glom, as follows: class: Gloms name: glom7 type: (Same print-value group) print-value:
    (2 2 2) start-position: 2 span: 3 positions-covered: (2 4) subgloms: (glint2 glint3
    glint4)" [M86 p. 62]."""
    world, _ = world_with(8, 2, 2, 2, 8)
    world.counters["glom"] = 6
    g = glom(world, "glint2", "glint3", "glint4")
    assert g.show_fields() == (("class", "Gloms"), ("name", "glom7"),
                               ("type", "(Same print-value group)"), ("print-value", (2, 2, 2)),
                               ("start-position", 2), ("span", 3), ("positions-covered", (2, 4)),
                               ("subgloms", ("glint2", "glint3", "glint4")))
    assert g.flat and not g.dubbed and not g.pseudo and g.top_level
    assert all(world.find(n).parent is g for n in ("glint2", "glint3", "glint4"))
    assert not world.find("glint2").top_level


def test_a_glom_of_gloms_nests_its_print_value_and_is_not_flat():
    """`((1 1) (2 2))` [M86 p. 70]; "has only glints as subgloms" is flat [M86 p. 69]."""
    world, _ = world_with(1, 1, 2, 2)
    inner1 = glom(world, "glint1", "glint2")
    inner2 = glom(world, "glint3", "glint4")
    outer = glom(world, inner1.name, inner2.name, type=("Succ", "print-value", "group"))
    assert outer.print_value == ((1, 1), (2, 2)) and outer.flat_values() == (1, 1, 2, 2)
    assert outer.positions_covered == (1, 4) and outer.span == 4
    assert not outer.flat and inner1.flat
    assert cy.is_subglom(world.find("glint1"), outer) and cy.is_subglom(inner1, outer)
    assert not cy.is_subglom(outer, inner1)


def test_a_pseudo_glom_prints_pseudo_in_place_of_its_type():
    """`glom3 pseudo --> ((3 7)) terms 1 to 2` [M86 p. 167]."""
    world, _ = world_with(3, 7)
    g = glom(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
    p = glom(world, g.name, type=(), pseudo=True)
    assert p.type_text() == "pseudo" and p.print_value == ((3, 7),)
    assert g.type_text() == "(Same print-value fence)"
    assert cy.top_level_elements(world) == [p]
    assert cy.top_level_elements(world, pseudo=False) == []


def test_make_glom_refuses_non_adjacent_or_taken_subgloms():
    """"made of adjacent atoms" [M86 p. 62]."""
    world, _ = world_with(1, 2, 3)
    with pytest.raises(ValueError, match="not adjacent"):
        glom(world, "glint1", "glint3")
    glom(world, "glint1", "glint2")
    with pytest.raises(ValueError, match="already a subglom"):
        glom(world, "glint2", "glint3")
    with pytest.raises(ValueError):
        cy.make_glom(world, ("Same", "print-value", "group"), [])


# ---- adjacency and ordering [M86 pp. 63-65, 174] ---------------------------------------------

def test_adjacent_and_remote():
    """"adjacent terms are more strongly bonded than non-adjacent ones" [M86 pp. 64-65];
    `(adjacent)` and `(remote)` [M86 p. 174]."""
    world, _ = world_with(1, 2, 1)
    g1, g2, g3 = (world.find(n) for n in ("glint1", "glint2", "glint3"))
    assert cy.adjacent(g1, g2) and cy.adjacent(g2, g1) and not cy.adjacent(g1, g3)
    assert cy.modifier(g1, g2) == "adjacent" and cy.modifier(g3, g1) == "remote"
    assert cy.left_to_right(g3, g1) == (g1, g3)
    bonds.new_term(world, 2)
    g = glom(world, "glint3", "glint4")
    assert cy.adjacent(g2, g) and cy.modifier(g1, g) == "remote"


# ---- sparks and bonds as structures [M86 pp. 63-64] ----------------------------------------------

def test_spark_and_bond_lines():
    """`spark6 --- between glint3glint1` [M86 p. 167]; `bond1 --- between glint1glint3`
    [M86 p. 164]; `bond1 Succ print-value (adjacent) -- (glint1 glint2)` [M86 p. 174]."""
    world, _ = world_with(1, 2, 1)
    g1, g2, g3 = (world.find(n) for n in ("glint1", "glint2", "glint3"))
    world.counters["spark"] = 5
    spark = bonds.make_spark(world, g3, g1)
    assert spark.line() == "spark6 --- between glint3glint1"
    assert "spark6" in world.cytoplasm
    b1 = bonds.make_bond(world, "Succ", "print-value", "adjacent", g1, g2)
    b2 = bonds.make_bond(world, "Pred", "print-value", "adjacent", g3, g2)
    b3 = bonds.make_bond(world, "Same", "print-value", "remote", g3, g1)
    assert b1.show_line() == "bond1 Succ print-value (adjacent) -- (glint1 glint2)"
    assert b2.show_line() == "bond2 Pred print-value (adjacent) -- (glint2 glint3)"
    assert b3.show_line() == "bond3 Same print-value (remote) -- (glint1 glint3)"
    assert b3.line() == "bond3 --- between glint1glint3"
    assert (b1.family, b2.family, b3.family) == ("pred-succ", "pred-succ", "Same")
    assert b1.other(g1) is g2 and b1.other(g2) is g1


def test_bonds_in_lists_the_bonds_attached_to_a_glint():
    """`bonds-in`, "the bonds attached" [M86 pp. 61-62]; `bonds_of` by family and strength
    (DECISION D-019)."""
    world, _ = world_with(1, 2, 1)
    g1, g2, g3 = (world.find(n) for n in ("glint1", "glint2", "glint3"))
    b1 = bonds.make_bond(world, "Succ", "print-value", "adjacent", g1, g2)
    b3 = bonds.make_bond(world, "Same", "print-value", "remote", g1, g3)
    assert g1.bonds_in == [b1, b3] and g2.bonds_in == [b1] and g3.bonds_in == [b3]
    assert cy.bonds_of(g1, "Same") == [b3] and cy.bonds_of(g1, "pred-succ") == [b1]
    assert cy.bonds_of(g1, minimum=6) == [b1]
    assert cy.bonded(g1, g3, "Same", "print-value") is b3
    assert cy.bonded(g3, g1, "Same", "print-value") is b3
    assert cy.bonded(g1, g3, "Same", "span") is None
    assert cy.bonded(g1, g2, "Pred", "print-value") is None


def test_make_bond_refuses_unknown_types():
    world, _ = world_with(1, 2)
    g1, g2 = world.find("glint1"), world.find("glint2")
    with pytest.raises(ValueError):
        bonds.make_bond(world, "Meeting", "print-value", "adjacent", g1, g2)
    with pytest.raises(ValueError):
        bonds.make_bond(world, "Same", "colour", "adjacent", g1, g2)


# ---- dissolving and bursting [M86 pp. 62, 70] ------------------------------------------------

def test_dissolving_leaves_the_subgloms_floating_free():
    """"if the glom '((1 1) (2 2))' were to be dissolved, the underlying gloms '(1 1)' and
    '(2 2)' would survive, but would, of course, no longer be glommed with each other"
    [M86 p. 70]."""
    world, _ = world_with(1, 1, 2, 2)
    inner1 = glom(world, "glint1", "glint2")
    inner2 = glom(world, "glint3", "glint4")
    outer = glom(world, inner1.name, inner2.name, type=("Succ", "print-value", "group"))
    freed = cy.dissolve(world, outer)
    assert freed == (inner1, inner2)
    assert outer.name not in world.cytoplasm
    assert inner1.top_level and inner2.top_level and inner1.name in world.cytoplasm
    assert cy.top_level_elements(world) == [inner1, inner2]
    assert not world.find("glint1").top_level


def test_bursting_leaves_only_the_glints():
    """"If a Burster were set on the glom '((1 1) (2 2))', all levels of glomming would be
    destroyed, leaving only the glints" [M86 p. 70]."""
    world, _ = world_with(1, 1, 2, 2)
    inner1 = glom(world, "glint1", "glint2")
    inner2 = glom(world, "glint3", "glint4")
    outer = glom(world, inner1.name, inner2.name, type=("Succ", "print-value", "group"))
    left = cy.burst(world, outer)
    assert [g.name for g in left] == ["glint1", "glint2", "glint3", "glint4"]
    assert set(world.cytoplasm.names) == {"glint1", "glint2", "glint3", "glint4"}
    assert all(g.top_level for g in cy.glints(world))


def test_dissolving_removes_the_bonds_and_sparks_of_the_glom():
    """DECISION D-059: a bond "links the two gloms" [M86 p. 64]; with one gone it goes."""
    world, _ = world_with(1, 2, 1, 2)
    g12 = glom(world, "glint1", "glint2", type=("Succ", "print-value", "group"))
    g34 = glom(world, "glint3", "glint4", type=("Succ", "print-value", "group"))
    bond = bonds.make_bond(world, "Same", "print-value", "adjacent", g12, g34)
    inner = bonds.make_bond(world, "Succ", "print-value", "adjacent",
                            world.find("glint1"), world.find("glint2"))
    spark = bonds.make_spark(world, g12, g34)
    cy.dissolve(world, g12)
    assert bond.name not in world.cytoplasm and spark.name not in world.cytoplasm
    assert g34.bonds_in == [] and g12.bonds_in == []
    assert inner.name in world.cytoplasm and world.find("glint1").bonds_in == [inner]


def test_a_subglom_or_a_missing_glom_cannot_be_dissolved():
    world, _ = world_with(1, 1)
    inner = glom(world, "glint1", "glint2")
    outer = glom(world, inner.name, type=("Same", "print-value", "group"))
    with pytest.raises(ValueError, match="subglom"):
        cy.dissolve(world, inner)
    cy.dissolve(world, outer)
    with pytest.raises(ValueError, match="not in cytoplasm"):
        cy.dissolve(world, outer)


def test_the_bond_fields_and_glom_fields():
    """"the print-value and span are useful bond-fields"; "Only print-value is used as a
    glom-field" [M86 p. 65]."""
    assert cy.BOND_FIELDS == ("print-value", "span") and cy.GLOM_FIELDS == ("print-value",)
    assert cy.BOND_TYPES == ("Same", "Succ", "Pred")
    assert cy.FAMILIES == {"Same": "Same", "Succ": "pred-succ", "Pred": "pred-succ"}

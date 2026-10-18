"""The slipping network and the level-spanning network: s-links, manifestation and
actualization lists, pull and push bond lists, a bond's strength in the environment of a
class, strictness [M86 pp. 90-91, 99-105, 174-175] (DECISIONS D-038, D-039, D-040)."""

from dataclasses import replace

import pytest

from seekwhence import bonds, gloms
from seekwhence import cytoplasm as cy
from seekwhence.config import IDEAL_TYPES, Config
from seekwhence.platoplasm import Platoplasm, bond_key
from seekwhence.run import new_world, run_until_prompt


def world_with(*terms, seed=1):
    """A world with the terms entered and the rack run empty, the Glom-scout a no-op."""
    world = new_world(seed=seed)
    world.handlers["Glom-scout"] = lambda w, *args: None
    for term in terms:
        bonds.new_term(world, term)
        while run_until_prompt(world) != "empty":
            pass
    world.taskrack.clean(("Sparkler", "Glom-scout"))
    return world


def bond(world, type, modifier, a, b, field="print-value"):
    """A bond between two named elements, made without running tasks."""
    return bonds.make_bond(world, type, field, modifier, world.find(a), world.find(b))


def figure_1_bonds(world):
    """The five bonds of Figure 1 for `1 2 1 2 3`: adjacent successor glint1-glint2,
    glint3-glint4 and glint4-glint5; remote same glint1-glint3 and glint2-glint4
    [M86 pp. 101-102]."""
    return [bond(world, "Succ", "adjacent", "glint1", "glint2"),
            bond(world, "Same", "remote", "glint1", "glint3"),
            bond(world, "Same", "remote", "glint2", "glint4"),
            bond(world, "Succ", "adjacent", "glint3", "glint4"),
            bond(world, "Succ", "adjacent", "glint4", "glint5")]


# ---- s-links [M86 pp. 90-91] ---------------------------------------------------------------

class TestSLinks:
    def test_s_group_has_the_five_neighbours_of_p_90(self):
        """"S-group has s-links to Countup, C-group, Y-group, Cycle, and Tuple" [M86 p. 90]."""
        plato = Platoplasm()
        assert set(plato.neighbours("S-group")) == {"Countup", "C-group", "Y-group", "Cycle",
                                                    "Tuple"}
        assert plato.type("S-group").neighbours() == plato.neighbours("S-group")

    def test_the_s_group_values_the_thesis_gives(self):
        """"The s-link from S-group to Countup has slipperiness 0.1, reflecting the fact that
        it is difficult to slip to a stricter class. Slipperiness from S-group to Tuple is
        0.4" [M86 pp. 90-91]; S-group to Y-group is 0.4 in the worked example
        `0.4 [the s-link slipperiness] * (10 + 10) = 8` [M86 p. 104]."""
        plato = Platoplasm()
        assert plato.s_link("S-group", "Countup") == 0.1
        assert plato.s_link("S-group", "Tuple") == 0.4
        assert plato.s_link("S-group", "Y-group") == 0.4

    def test_the_link_to_the_stricter_class_is_the_least_slippery(self):
        """"it is difficult to slip to a stricter class" [M86 p. 90]: of S-group's links the
        one to Countup, the only stricter neighbour, is the least slippery, and the one to
        Tuple, "a generic grouping mechanism" [M86 p. 91], among the most slippery."""
        plato = Platoplasm()
        links = plato.type("S-group").s_links
        assert min(links, key=links.get) == "Countup"
        assert links["Tuple"] == max(links.values())

    def test_slipperiness_is_between_0_and_1_and_absent_links_are_0(self):
        """"a number between 0 (non-slippery) and 1 (perfectly slippery)" [M86 p. 90]; a pair
        with no s-link has no slipperiness (DECISION D-038)."""
        plato = Platoplasm()
        for kind in IDEAL_TYPES:
            assert plato.neighbours(kind), f"{kind} has no s-links"
            assert kind not in plato.neighbours(kind)
            for other in plato.neighbours(kind):
                assert 0 <= plato.s_link(kind, other) <= 1
        assert plato.s_link("Y-group", "Countup") == 0
        assert "Countup" not in plato.neighbours("Y-group")

    def test_s_links_have_direction(self):
        """"the s-links have direction" [M86 p. 90]: the transcript slips `Countup -->
        S-group` [M86 p. 174], a link whose slipperiness need not equal the 0.1 of the
        reverse link (DECISION D-038)."""
        plato = Platoplasm()
        assert "S-group" in plato.neighbours("Countup")
        assert plato.s_link("Countup", "S-group") != plato.s_link("S-group", "Countup")

    def test_the_table_is_read_from_config_once_per_run(self):
        """"The slipperiness values can be changed during processing, although the current
        system does not do so" [M86 p. 91]: the knob is `Config.s_links` (DECISION D-038)."""
        config = replace(Config(), s_links=(("Cycle", "Tuple", 0.9),))
        plato = Platoplasm(config)
        assert plato.neighbours("Cycle") == ("Tuple",)
        assert plato.s_link("Cycle", "Tuple") == 0.9
        assert plato.neighbours("S-group") == ()
        assert plato.type("S-group").most_slippery is None
        assert Platoplasm().s_link("Cycle", "Tuple") == dict(
            ((a, b), s) for a, b, s in Config().s_links)["Cycle", "Tuple"]


# ---- pull and push bond lists [M86 pp. 91, 100] --------------------------------------------

class TestTablesAreChecked:
    """The platoplasm checks the three tables as it reads them: a
    slipperiness lies between 0 and 1 [M86 p. 90], a strength between 1 and 10 [M86 p. 91],
    and every name is one of the eight types."""

    def test_the_default_tables_pass(self):
        Platoplasm(Config())

    def test_an_unknown_class_is_refused(self):
        with pytest.raises(ValueError):
            Platoplasm(replace(Config(), s_links=(("S-group", "Z-group", 0.1),)))
        with pytest.raises(ValueError):
            Platoplasm(replace(Config(), bond_values=(("Z-group", "Same", "adjacent", 10),)))
        with pytest.raises(ValueError):
            Platoplasm(replace(Config(), strict_classes=("Z-group",)))

    def test_a_value_out_of_range_is_refused(self):
        with pytest.raises(ValueError):
            Platoplasm(replace(Config(), s_links=(("S-group", "Tuple", 1.5),)))
        with pytest.raises(ValueError):
            Platoplasm(replace(Config(), s_links=(("S-group", "S-group", 0.5),)))
        with pytest.raises(ValueError):
            Platoplasm(replace(Config(), bond_values=(("S-group", "Same", "adjacent", 0),)))
        with pytest.raises(ValueError):
            Platoplasm(replace(Config(), bond_values=(("S-group", "Same", "adjacent", 11),)))


class TestPullPushLists:
    def test_every_listed_strength_is_from_1_to_10(self):
        """"Each so-designated bond type is given a strength from 1 to 10" [M86 p. 91]; a
        push is held negative (DECISION D-039), and no type is listed both ways."""
        plato = Platoplasm()
        for kind in IDEAL_TYPES:
            pulls, pushes = plato.pull_bonds(kind), plato.push_bonds(kind)
            assert all(1 <= v <= 10 for v in pulls.values()), kind
            assert all(-10 <= v <= -1 for v in pushes.values()), kind
            assert not set(pulls) & set(pushes)
            assert all(mod in ("adjacent", "remote") for _, mod in list(pulls) + list(pushes))

    def test_c_group_favours_adjacent_sameness_most_and_likes_same_span(self):
        """"the C-group concept, because it involves copy or sameness groups, favors adjacent
        sameness bonds most strongly, but also likes to see gloms having the same span"
        [M86 p. 91]; "Should C-group be in ascendancy, sameness bonds become strong whereas
        successorship bonds are made negative" [M86 p. 100]."""
        plato = Platoplasm()
        pulls, pushes = plato.pull_bonds("C-group"), plato.push_bonds("C-group")
        assert max(pulls, key=pulls.get) == ("Same", "adjacent")
        assert pulls["Same", "adjacent"] == plato.strongest_pull("C-group")
        assert pulls["Same-span", "adjacent"] > 0
        assert pulls["Same-span", "adjacent"] < pulls["Same", "adjacent"]
        assert pushes["Succ", "adjacent"] < 0

    def test_s_group_pulls_adjacent_successors_and_pushes_sameness(self):
        """"If, for example, S-group is the reigning class, adjacent successor bonds are given
        large positive values to indicate that they are strong pulling bonds while sameness
        bonds are given negative values to indicate that they tend to push gloms apart"
        [M86 p. 100]."""
        plato = Platoplasm()
        pulls, pushes = plato.pull_bonds("S-group"), plato.push_bonds("S-group")
        assert list(pulls) == [("Succ", "adjacent")] and pulls["Succ", "adjacent"] == 10
        assert all(type == "Same" for type, _ in pushes) and pushes
        assert all(v < 0 for v in pushes.values())

    def test_the_strongest_pull_gives_the_first_threshold(self):
        """"80% of the strongest pull-bond strength for the reigning type" [M86 p. 105] is
        the `(S-group 8.0)` the first Bond-assessor of run 3 carries [M86 p. 174]."""
        plato = Platoplasm()
        assert plato.strongest_pull("S-group") == 10
        assert Config().threshold_fraction * plato.strongest_pull("S-group") == 8.0
        assert plato.type("S-group").strongest_pull == 10
        assert Platoplasm(replace(Config(), bond_values=())).strongest_pull("S-group") == 0

    def test_the_tables_are_read_from_config(self):
        """The strengths "could be, but in practice are not, changed by the system"
        [M86 p. 91]: the knob is `Config.bond_values` (DECISION D-039)."""
        config = replace(Config(), bond_values=(("Tuple", "Succ", "remote", 7),
                                                ("Tuple", "Same", "adjacent", -2)))
        plato = Platoplasm(config)
        assert plato.pull_bonds("Tuple") == {("Succ", "remote"): 7}
        assert plato.push_bonds("Tuple") == {("Same", "adjacent"): -2}
        assert plato.pull_bonds("S-group") == {} and plato.push_bonds("S-group") == {}
        assert plato.pull_bonds("Tuple") is not plato.type("Tuple").pulls


# ---- a bond's strength in the environment of a class [M86 pp. 99-104, 175] ----------------

class TestBondEnvironments:
    def test_figure_1_values_under_s_group(self):
        """"the adjacent-successor bond between glint1 and glint2 has strength +10 because
        S-group is the reigning Platonic type and S-group favors such bonds" [M86 p. 101];
        "The remote sameness bond (with strength -5) between glint2 and glint4" [M86 p. 101];
        the legend "+10 -- adjacent successor value bond", "-5 -- remote same value bond"
        [M86 p. 102]."""
        world = world_with(1, 2, 1, 2, 3)
        values = [world.platoplasm.bond_value("S-group", b) for b in figure_1_bonds(world)]
        assert values == [10, -5, -5, 10, 10]

    def test_figure_1_values_under_y_group_give_the_p_104_sum(self):
        """"0.4 [the s-link slipperiness] * (10 + 10) = 8, where the 10's are the strengths,
        in a Y-group environment, of the 'remote same' bonds between glint1 and glint3 and
        between glint2 and glint4" [M86 p. 104]: the two remote sameness bonds are 10 each
        and the three adjacent successor bonds count for nothing under Y-group, or the sum
        would not be 20 (SPEC 11.5)."""
        world = world_with(1, 2, 1, 2, 3)
        plato = world.platoplasm
        values = [plato.bond_value("Y-group", b) for b in figure_1_bonds(world)]
        assert values == [0, 10, 10, 0, 0]
        assert plato.s_link("S-group", "Y-group") * sum(values) == 8.0

    def test_the_run_3_values_of_p_175(self):
        """`show` after the Gnoth-setter prints `external-bonds: ((bond2 0) (bond3 -5))` and
        `internal-bonds: ((bond1 10))` for `bond1 Succ print-value (adjacent)`, `bond2 Pred
        print-value (adjacent)` and `bond3 Same print-value (remote)` under S-group
        [M86 pp. 174-175]: an adjacent predecessor bond is worth 0 (DECISION D-039)."""
        world = world_with(1, 2, 1)
        bond1 = bond(world, "Succ", "adjacent", "glint1", "glint2")
        bond2 = bond(world, "Pred", "adjacent", "glint2", "glint3")
        bond3 = bond(world, "Same", "remote", "glint1", "glint3")
        plato = world.platoplasm
        assert [plato.bond_value("S-group", b) for b in (bond1, bond2, bond3)] == [10, 0, -5]
        assert [plato.bond_value("Y-group", b) for b in (bond1, bond2, bond3)] == [0, 0, 10]

    def test_the_value_depends_on_the_class_as_well_as_on_the_bond(self):
        """"assigned a strength which depends on the class in question as well as on the type
        of the bond" [M86 p. 99]: the same adjacent sameness bond is a pull under C-group
        and Constant and a push under S-group and P-group (DECISION D-039)."""
        world = world_with(4, 4)
        same = bond(world, "Same", "adjacent", "glint1", "glint2")
        plato = world.platoplasm
        assert plato.bond_value("C-group", same) == 10
        assert plato.bond_value("Constant", same) > 0
        assert plato.bond_value("S-group", same) < 0
        assert plato.bond_value("P-group", same) < 0
        assert plato.bond_value("Tuple", same) == 0

    def test_a_span_bond_is_keyed_same_span(self):
        """A sameness bond on the span is the C-group's liking for "gloms having the same
        span" [M86 p. 91] and nothing to the other classes (DECISION D-039)."""
        assert bond_key("Same", "span") == "Same-span"
        assert bond_key("Same", "print-value") == "Same"
        assert bond_key("Succ", "print-value") == "Succ"
        with pytest.raises(ValueError):
            bond_key("Succ", "span")
        world = world_with(1, 2, 1, 2)
        left = cy.make_glom(world, ("Same", "print-value", "group"),
                            [world.find("glint1"), world.find("glint2")])
        right = cy.make_glom(world, ("Same", "print-value", "group"),
                             [world.find("glint3"), world.find("glint4")])
        span = bond(world, "Same", "adjacent", left.name, right.name, field="span")
        plato = world.platoplasm
        assert plato.bond_value("C-group", span) == plato.pull_bonds("C-group")["Same-span",
                                                                                 "adjacent"]
        assert plato.bond_value("S-group", span) == 0

    def test_the_type_level_lookup_takes_a_key_and_a_modifier(self):
        """`IdealType.bond_value` is the table lookup itself: 0 for an unlisted combination
        (DECISION D-039)."""
        s_group = Platoplasm().type("S-group")
        assert s_group.bond_value("Succ", "adjacent") == 10
        assert s_group.bond_value("Succ", "remote") == 0
        assert s_group.bond_value("Same", "remote") == -5
        assert s_group.bond_value("Pred", "adjacent") == 0
        assert s_group.bond_value("Same-span", "adjacent") == 0


# ---- strictness [M86 p. 99] -----------------------------------------------------------------

class TestStrictness:
    def test_constant_and_countup_are_strict(self):
        """"if a reigning class -- such as Constant -- is very strict in the sense that it is
        difficult to generalize without moving to a new class altogether, slipping is chosen
        immediately" [M86 p. 99]; the transcript slips from Countup at once [M86 p. 174]
        (DECISION D-040)."""
        plato = Platoplasm()
        assert plato.is_strict("Constant") and plato.is_strict("Countup")
        assert not any(plato.is_strict(k) for k in IDEAL_TYPES if k not in ("Constant",
                                                                            "Countup"))
        assert plato.type("Countup").strict

    def test_a_strict_class_slips_to_its_most_slippery_neighbour(self):
        """`Changing to a new reigning class. Countup --> S-group` [M86 p. 174]: Countup's
        most slippery s-link leads to S-group (DECISION D-040)."""
        plato = Platoplasm()
        assert plato.type("Countup").most_slippery == "S-group"
        assert plato.type("Constant").most_slippery == "C-group"

    def test_ties_go_to_the_first_listed_and_the_knob_is_read(self):
        """DECISION D-040: `Config.strict_classes`; a tie between s-links goes to the one
        listed first."""
        config = replace(Config(), strict_classes=("Tuple",),
                         s_links=(("Tuple", "Cycle", 0.4), ("Tuple", "S-group", 0.4)))
        plato = Platoplasm(config)
        assert plato.is_strict("Tuple") and not plato.is_strict("Constant")
        assert plato.type("Tuple").most_slippery == "Cycle"


# ---- the level-spanning network [M86 p. 90] ------------------------------------------------

class TestLevelSpanning:
    def test_manifestations_and_actualizations_are_listed_per_concept(self):
        """"This network includes the lists each Platonic concept maintains of its
        manifestations and actualizations" [M86 p. 90]: a dubbed glom is a manifestation
        [M86 p. 69], a gnoth of the class an actualization [M86 p. 90]."""
        plato = Platoplasm()
        plato.manifest("Cycle", "glom1")
        plato.actualize("Cycle", "gnoth1")
        plato.actualize("S-group", "gnoth2")
        assert plato.type("Cycle").manifestations == ["glom1"]
        assert plato.type("Cycle").actualizations == ["gnoth1"]
        assert plato.type("S-group").actualizations == ["gnoth2"]
        plato.forget("gnoth1")
        assert plato.type("Cycle").actualizations == []
        assert plato.type("Cycle").manifestations == ["glom1"]

    def test_a_burst_glom_is_no_longer_a_manifestation(self):
        """A manifestation is a cyto-level structure "which [has] been dubbed" [M86 p. 90];
        bursting destroys the glom "as a unit" [M86 p. 62], so it leaves the list
        (DECISION D-059)."""
        world = world_with(3, 7, 3)
        g = cy.make_glom(world, ("Same", "print-value", "fence"),
                         [world.find("glint1"), world.find("glint2")])
        gloms.dub(world, g, "Cycle", "pure")
        assert world.platoplasm.type("Cycle").manifestations == [g]
        cy.burst(world, g)
        assert world.platoplasm.type("Cycle").manifestations == []
        assert world.find("glom1") is None

"""The platoplasm [M86 pp. 48-59, 68-69]: atoms, the eight types, hits, processes, purity."""

import pytest

from seekwhence.config import IDEAL_TYPES
from seekwhence.forms import Form, parse, value_text
from seekwhence.platoplasm import (
    CATEGORIES, Hitting, IdealAtom, Platoplasm, accepts, category, generate, hits,
    is_c_group, is_constant, is_countup, is_p_group, is_s_group, is_stricter, is_y_group,
    members, passes, purity, terms)


def printed(values) -> str:
    """Hits as the thesis prints them on successive lines, joined by ';'."""
    return "; ".join(value_text(v) for v in values)


def collapse(value):
    """A group of length one printed as its term, as p. 57 prints the first hit of
    `(C-group (S-group 1 (Countup 1)) 2)` as `(1 1)` [M86 p. 57]."""
    if isinstance(value, tuple):
        if len(value) == 1:
            return collapse(value[0])
        return tuple(collapse(v) for v in value)
    return value


# ---- ideal atoms [M86 pp. 48-49] -----------------------------------------------------------

class TestIdealAtoms:
    def test_predecessor_and_successor(self):
        """ideal5 has predecessor ideal4 and successor ideal6; "'ideal0' has no predecessor
        but has 'ideal1' as successor" [M86 pp. 48-49]."""
        plato = Platoplasm()
        five = plato.atom(5)
        assert five.name == "ideal5"
        assert plato.predecessor(five).name == "ideal4"
        assert plato.successor(five).name == "ideal6"
        zero = plato.atom(0)
        assert plato.predecessor(zero) is None
        assert plato.successor(zero) is plato.atom(1)
        assert IdealAtom(0).predecessor_value is None
        assert IdealAtom(0).successor_value == 1

    def test_atoms_are_made_once_and_only_for_nonnegative_integers(self):
        """Ideal atoms are the analogues of the integers entered [M86 p. 48]; the domain has
        only nonnegative integers [FCCA p. 48]."""
        plato = Platoplasm()
        assert plato.atom(7) is plato.atom(7)
        assert sorted(plato.atoms) == [7]
        with pytest.raises(ValueError):
            plato.atom(-1)
        with pytest.raises(ValueError):
            plato.atom(True)

    def test_manifestations(self):
        """In `5 0 5` "the first and last terms are manifestations of ideal5, while the
        middle term is a manifestation of ideal0" [M86 p. 49]; a glint is listed on the
        atom [M86 p. 61]."""
        plato = Platoplasm()
        glints = ["glint1", "glint2", "glint3"]
        for value, glint in zip((5, 0, 5), glints):
            plato.add_manifestation(value, glint)
        assert plato.atom(5).manifestations == ["glint1", "glint3"]
        assert plato.atom(0).manifestations == ["glint2"]


# ---- the eight types and their hierarchy [M86 pp. 49-56] --------------------------------

class TestHierarchy:
    def test_the_eight_types_and_their_formats(self):
        """"There are eight non-atomic ideal types" [M86 p. 49] with the formats of
        pp. 50-53."""
        plato = Platoplasm()
        assert tuple(plato.types) == IDEAL_TYPES
        assert plato.type("Constant").format == "(Constant arg)"
        assert plato.type("Countup").format == "(Countup n)"
        assert plato.type("C-group").format == "(C-group start length)"
        assert plato.type("S-group").format == "(S-group start length)"
        assert plato.type("P-group").format == "(P-group start length)"
        assert plato.type("Y-group").format == "(Y-group [start] [length] actual)"
        assert plato.type("Cycle").format == "(Cycle actual)"
        assert plato.type("Tuple").format == "(Tuple actual)"
        assert all(t.manifestations == [] and t.actualizations == [] for t in plato.types.values())

    def test_the_four_categories_and_their_members(self):
        """repeater: Constant, Countup; generate: C-group, S-group, P-group; process:
        Y-group; fence: Cycle, Tuple [M86 pp. 54-55]."""
        assert CATEGORIES == ("repeater", "generate", "process", "fence")
        assert members("repeater") == ("Constant", "Countup")
        assert members("generate") == ("C-group", "S-group", "P-group")
        assert members("process") == ("Y-group",)
        assert members("fence") == ("Cycle", "Tuple")
        assert category("Y-group") == "process"

    def test_generators_and_processes_by_category(self):
        """Repeaters and generate types generate; "Any generate type possesses a process"
        [M86 p. 54]; a process type "possesses a 'process'" but no generator [M86 pp. 54-55];
        a fence type "Has no generator, no process" [M86 p. 54]; "all but the fence-type
        plato-classes (Tuple and Cycle) possess a 'process'" [M86 p. 68]."""
        plato = Platoplasm()
        has = {name: (t.has_generator, t.has_process) for name, t in plato.types.items()}
        assert has["Constant"] == (True, True) and has["Countup"] == (True, True)
        assert has["C-group"] == has["S-group"] == has["P-group"] == (True, True)
        assert has["Y-group"] == (False, True)
        assert has["Cycle"] == has["Tuple"] == (False, False)

    def test_strictness(self):
        """Higher in the hierarchy is stricter: the fence types are "the least restrictive"
        [M86 p. 55]; `1 1 1` "can be viewed as a Tuple, it is also a C-group" and should be
        so characterized [M86 p. 59]."""
        assert is_stricter("C-group", "Tuple")
        assert is_stricter("Countup", "S-group")
        assert is_stricter("S-group", "Y-group") and is_stricter("Y-group", "Cycle")
        assert not is_stricter("Tuple", "C-group")
        assert not is_stricter("C-group", "S-group") and not is_stricter("Cycle", "Tuple")


# ---- generating functions [M86 pp. 54, 56] -------------------------------------------------

class TestGenerate:
    def test_the_three_generate_types(self):
        """`(C-group 1 5)` is `(1 1 1 1 1)`, `(S-group 2 3)` is `(2 3 4)`, `(P-group 9 4)` is
        `(9 8 7 6)` [M86 p. 56]; `(C-group 1 3)` is `(1 1 1)` [M86 p. 54]."""
        assert generate("C-group", 1, 5) == (1, 1, 1, 1, 1)
        assert generate("S-group", 2, 3) == (2, 3, 4)
        assert generate("P-group", 9, 4) == (9, 8, 7, 6)
        assert generate("C-group", 1, 3) == (1, 1, 1)
        assert generate("C-group", (1, 2), 2) == ((1, 2), (1, 2))

    def test_p_group_below_zero_is_undefined(self):
        """`(P-group 2 4)` is "undefined (would run to negative numbers)" [M86 p. 52]
        (DECISION D-006); `(P-group 3 4)` reaches 0 and is fine."""
        with pytest.raises(ValueError):
            generate("P-group", 2, 4)
        assert generate("P-group", 3, 4) == (3, 2, 1, 0)
        with pytest.raises(ValueError):
            hits(parse("(P-group 2 4)"), 1)

    def test_only_generate_types_generate(self):
        """The fence and process types have "no generator" [M86 pp. 54-55]."""
        for kind in ("Constant", "Countup", "Y-group", "Cycle", "Tuple"):
            with pytest.raises(ValueError):
                generate(kind, 1, 3)
        with pytest.raises(ValueError):
            generate("S-group", (1, 2), 2)
        with pytest.raises(ValueError):
            generate("S-group", 1, -1)


# ---- hits [M86 pp. 49-58, 80-84, 96-97, 161-163, 166-179] ----------------------------------

# (form, hits as the thesis prints them, page); a derived row is marked in SPEC 4.6.
HITS = [
    ("(Constant 3)", "3; 3; 3", 50),
    ("(Countup 3)", "3; 4; 5", 50),
    ("(Countup 8)", "8; 9; 10", 50),
    ("(Constant 2)", "2; 2", 56),
    ("(C-group 2 3)", "(2 2 2); (2 2 2)", 51),
    ("(C-group (Countup 1) 2)", "(1 1); (2 2); (3 3)", 51),
    ("(S-group 2 3)", "(2 3 4); (2 3 4)", 51),
    ("(S-group 5 4)", "(5 6 7 8); (5 6 7 8)", 51),
    ("(S-group (Countup 1) 2)", "(1 2); (2 3); (3 4)", 51),
    ("(P-group 8 4)", "(8 7 6 5); (8 7 6 5)", 52),
    ("(Y-group 1 5 (1 8 3 8 1))", "(1 8 3 8 1); (1 8 3 8 1)", 52),
    ("(Y-group ((Countup 1) 8 (Countup 1)))", "(1 8 1); (2 8 2); (3 8 3)", 52),
    ("(Cycle (2 1 8))", "2; 1; 8; 2; 1", 53),
    ("(Cycle (3 (Countup 1)))", "3; 1; 3; 2; 3; 3; 3; 4", 53),
    ("(Tuple (1 8 4))", "(1 8 4); (1 8 4)", 53),
    ("(Tuple (2 2))", "(2 2); (2 2)", 54),
    ("(C-group 1 3)", "(1 1 1)", 54),
    ("(C-group 1 5)", "(1 1 1 1 1)", 56),
    ("(S-group 2 3)", "(2 3 4)", 56),
    ("(P-group 9 4)", "(9 8 7 6)", 56),
    ("(Countup 3)", "3; 4; 5", 56),
    ("(Tuple (5 8 3))", "(5 8 3)", 55),
    ("(Cycle (4 7))", "4; 7; 4; 7", 55),
    ("(S-group 1 (Countup 2))", "(1 2); (1 2 3); (1 2 3 4)", 56),
    ("(Cycle (8 (Countup 1) shared))", "8; 1; 2; 8; 3; 4", 57),
    ("(Cycle (8 (Countup 1) (Countup 1)))", "8; 1; 1; 8; 2; 2", 84),
    ("(Tuple (2 (Countup 1) 2))", "(2 1 2); (2 2 2); (2 3 2)", 57),
    ("(Tuple (2 (Countup 1) (shared 0)))", "(2 1 2); (2 2 2); (2 3 2)", 58),
    ("(Tuple (5 (Countup 1) 2))", "(5 1 2); (5 2 2); (5 3 2)", 58),
    ("(Tuple (5 (Countup 1) (shared 0)))", "(5 1 5); (5 2 5); (5 3 5)", 58),
    ("(S-group 1 (Countup 1))", "(1); (1 2); (1 2 3); (1 2 3 4)", 59),
    ("(Cycle (2 (Countup 1)))", "2; 1; 2; 2; 2; 3; 2; 4", 59),
    ("(C-group (S-group 4 3) 2)", "((4 5 6) (4 5 6))", 80),
    ("(S-group (Countup 4) 3)", "(4 5 6); (5 6 7)", 81),
    ("(Cycle ((Countup 1) (Tuple (5 8 4))))", "1; (5 8 4); 2; (5 8 4); 3; (5 8 4)", 81),
    ("(Cycle ((Countup 1) 1))", "1; 1; 2; 1; 3; 1", 81),
    ("(Cycle (1 (S-group 1 2)))", "1; (1 2); 1; (1 2)", 81),
    ("(S-group 1 2)", "(1 2); (1 2); (1 2)", 81),
    ("(Y-group [1] [3] (1 2 1))", "(1 2 1); (1 2 1)", 81),
    ("(C-group (S-group 2 3) 2)", "((2 3 4) (2 3 4))", 82),
    ("(C-group (S-group 2 3) (shared 0 0))", "((2 3 4) (2 3 4))", 84),
    ("(C-group (S-group (Countup 1) 3) 2)",
     "((1 2 3) (1 2 3)); ((2 3 4) (2 3 4)); ((3 4 5) (3 4 5))", 96),
    ("(C-group (Countup 1) (Countup 1))", "(1); (2 2); (3 3 3)", 97),
    ("(C-group (Countup 1) shared)", "(1); (2 2); (3 3 3); (4 4 4 4)", 161),
    ("(Tuple 1 3 ((Countup 1) 8 shared))", "(1 8 1); (2 8 2); (3 8 3); (4 8 4)", 162),
    ("(Cycle 1 3 ((Countup 1) 8 shared))", "1; 8; 2; 3; 8; 4; 5; 8; 6", 163),
    ("(Cycle 3 2 (3 7))", "3; 7; 3; 7", 166),
    ("(Cycle 3 3 (3 7 3))", "3; 7; 3; 3; 7", 169),
    ("(Countup 1)", "1; 2", 171),
]


@pytest.mark.parametrize("text,expected,page", HITS, ids=[f"{h[0]} p.{h[2]}" for h in HITS])
def test_every_example_form_gives_its_printed_hits(text, expected, page):
    """Every form the thesis evaluates gives the values it prints, one per hit
    [M86 pp. 50-59, 80-84, 96-97, 161-163, 166-179] (SPEC.md section 4.6's table)."""
    n = expected.count(";") + 1
    assert printed(hits(parse(text), n)) == expected


def test_a_group_of_length_one_is_printed_as_its_term_on_p57():
    """`(C-group (S-group 1 (Countup 1)) 2)` gives `(1 1)`, `((1 2) (1 2))`,
    `((1 2 3) (1 2 3))` [M86 p. 57]: the first hit is `((1) (1))` with the inner groups of
    length one printed as their terms (the appendix prints `(1)` for a lone one-term group
    [M86 p. 161], so the collapse is the page's, not the value's)."""
    values = hits(parse("(C-group (S-group 1 (Countup 1)) 2)"), 3)
    assert values[0] == ((1,), (1,))
    assert printed(collapse(v) for v in values) == "(1 1); ((1 2) (1 2)); ((1 2 3) (1 2 3))"


# The sequences the thesis prints under "giving:" [M86 pp. 56-59, 80-81, 96-97].
SEQUENCES = [
    ("(S-group 1 (Countup 2))", "1 2 1 2 3 1 2 3 4", 56),
    ("(C-group (S-group 1 (Countup 1)) 2)", "1 1 1 2 1 2 1 2 3 1 2 3", 57),
    ("(Cycle (8 (Countup 1) shared))", "8 1 2 8 3 4 8 5 6", 57),
    ("(Tuple (2 (Countup 1) 2))", "2 1 2 2 2 2 2 3 2 2 4 2 2 5 2", 57),
    ("(Tuple (2 (Countup 1) (shared 0)))", "2 1 2 2 2 2 2 3 2 2 4 2 2 5 2", 58),
    ("(S-group 1 (Countup 1))", "1 1 2 1 2 3 1 2 3 4", 59),
    ("(Cycle (2 (Countup 1)))", "2 1 2 2 2 3 2 4", 59),
    ("(Cycle (8 (Countup 1) (Countup 1)))", "8 1 1 8 2 2 8 3 3", 84),
    ("(C-group (S-group 4 3) 2)", "4 5 6 4 5 6", 80),
    ("(S-group (Countup 4) 3)", "4 5 6 5 6 7", 81),
    ("(Cycle ((Countup 1) (Tuple (5 8 4))))", "1 5 8 4 2 5 8 4 3 5 8 4", 81),
    ("(Cycle ((Countup 1) 1))", "1 1 2 1", 81),
    ("(Cycle (1 (S-group 1 2)))", "1 1 2 1", 81),
    ("(C-group (S-group (Countup 1) 3) 2)", "1 2 3 1 2 3 2 3 4 2 3 4 3 4 5 3 4 5", 96),
    ("(C-group (Countup 1) (Countup 1))", "1 2 2 3 3 3", 97),
    ("(Cycle 3 2 (3 7))", "3 7 3 7", 166),
    ("(Cycle 3 3 (3 7 3))", "3 7 3 3 7", 169),
    ("(S-group 1 (Countup 2))", "1 2 1 2 3 1 2 3 4", 179),
]


@pytest.mark.parametrize("text,sequence,page", SEQUENCES,
                         ids=[f"{s[0]} p.{s[2]}" for s in SEQUENCES])
def test_the_sequence_a_form_gives(text, sequence, page):
    """"thus giving the sequence: 1 2 1 2 3 1 2 3 4 ..." [M86 p. 56] and the other
    "giving:" lines [M86 pp. 57-59, 80-81, 96-97, 166, 169, 179]."""
    expected = [int(t) for t in sequence.split()]
    assert terms(parse(text), len(expected)) == expected


class TestHitting:
    def test_reset_starts_again(self):
        """A fresh box starts from the beginning: `(Countup 3)` "returns a 3 upon first
        request, then a 4, a 5, and so on" [M86 p. 56]."""
        hitting = Hitting(parse("(Countup 3)"))
        assert [hitting.hit() for _ in range(3)] == [3, 4, 5]
        assert hitting.count == 3
        hitting.reset()
        assert hitting.hit() == 3 and hitting.count == 1

    def test_shared_structure_is_read_once_per_hit(self):
        """A shared Countup read twice within one hit gives one value (`(1 8 1)`
        [M86 p. 162]) and advances once per element of a Cycle (`8 1 2` [M86 p. 57])
        (DECISION D-003)."""
        assert hits(parse("(Tuple ((Countup 1) 8 shared))"), 2) == [(1, 8, 1), (2, 8, 2)]
        assert hits(parse("(Cycle (8 (Countup 1) shared))"), 3) == [8, 1, 2]
        assert hits(parse("(Y-group ((Countup 1) 8 shared))"), 2) == [(1, 8, 1), (2, 8, 2)]
        # The arrow into a nested structure [M86 p. 84]: the length is the S-group's start.
        assert hits(parse("(C-group (S-group (Countup 2) 3) (shared 0 0))"), 2) == \
            [((2, 3, 4), (2, 3, 4)), ((3, 4, 5), (3, 4, 5), (3, 4, 5))]

    def test_a_constant_always_returns_one_value(self):
        """"a structure that always returns one value, its argument" [M86 p. 50], even when
        the argument is a structure."""
        assert hits(parse("(Constant (Countup 4))"), 3) == [4, 4, 4]

    def test_a_countup_must_start_at_an_integer(self):
        """"returns nonnegative integers in succession, starting with its argument"
        [M86 p. 50]."""
        with pytest.raises(ValueError):
            hits(parse("(Countup (Tuple (1 2)))"), 1)
        with pytest.raises(ValueError):
            hits(parse("(S-group (Tuple (1 2)) 2)"), 1)

    def test_a_form_without_elements_cannot_be_hit(self):
        """`(Y-group 1 5)` "is not sufficient to generate a unique symmetry group"
        [M86 p. 55]."""
        with pytest.raises(ValueError):
            hits(parse("(Y-group 1 5)"), 1)
        with pytest.raises(TypeError):
            Hitting(3)

    def test_terms_stop_on_an_empty_form(self):
        """A form whose hits have no terms gives no terms, and `terms` stops [M86 p. 49]."""
        assert terms(parse("(C-group 1 0)"), 5) == []
        assert terms(parse("(Cycle ((C-group 1 0) 7))"), 3) == [7, 7, 7]


# ---- process functions, membership and purity [M86 pp. 54-55, 68-69] -----------------------

class TestProcesses:
    def test_the_four_processes(self):
        """All copies of one value [M86 p. 51]; successive integers [M86 p. 51]; a downward
        progression of nonnegative integers [M86 p. 52]; symmetric about the centre
        [M86 p. 52]."""
        assert is_c_group((1, 1, 1)) and not is_c_group((1, 1, 2)) and not is_c_group(())
        assert is_s_group((1, 2, 3)) and is_s_group((2, 3)) and not is_s_group((1, 2, 3, 2, 3))
        assert is_p_group((9, 8, 7, 6)) and is_p_group((1, 0)) and not is_p_group((2, 1, 0, -1))
        assert is_y_group((1, 7, 4, 7, 1)) and is_y_group((1,)) and not is_y_group((1, 8, 2, 5, 1))
        assert not is_s_group(((1, 2), (2, 3)))

    def test_p69_examples(self):
        """`(1 1 1)` "might be dubbed as both a C-group ... and a Y-group" [M86 p. 69];
        `1 2 3 2 3` fails and breaks "into the two pure successorship groups '1 2 3' and
        '2 3'" [M86 p. 69]; `((1 5) (1 5) (1 5))` is a C-group of secondary purity
        [M86 p. 69]."""
        assert passes("C-group", (1, 1, 1)) and passes("Y-group", (1, 1, 1))
        assert not passes("S-group", (1, 1, 1))
        assert not passes("S-group", (1, 2, 3, 2, 3))
        assert passes("S-group", (1, 2, 3)) and passes("S-group", (2, 3))
        assert passes("C-group", ((1, 5), (1, 5), (1, 5)), flat=False)
        assert purity(flat=True) == "pure" and purity(flat=False) == "secondary"

    def test_fence_types_pass_flat_groups_by_default(self):
        """"If the plato-class is a fence type (and so has no process), and if the glom is
        'flat' ... a 'pass by default' occurs" [M86 p. 69]."""
        assert passes("Cycle", (3, 7)) and passes("Tuple", (5, 8, 3))
        assert not passes("Cycle", ((3, 7), (3, 7)), flat=False)
        assert not passes("Tuple", ())

    def test_repeater_processes(self):
        """A Constant's string is one value repeated, a Countup's "nonnegative integers in
        succession" [M86 p. 50] (DECISION D-005)."""
        assert is_constant((2, 2, 2)) and not is_constant((2, 3)) and not is_constant(((1,), (1,)))
        assert is_countup((3, 4, 5)) and is_countup((1,)) and not is_countup((3, 5))
        assert passes("Countup", (1, 2)) and passes("Constant", (7,))

    def test_y_group_1_5(self):
        """`(Y-group 1 5)` "is sufficient to determine that (1 7 4 7 1) is such a Y-group
        whereas (1 8 2 5 1), (1 7 1), and (2 0 0 0 1) are not" [M86 p. 55]."""
        form = parse("(Y-group 1 5)")
        assert accepts(form, (1, 7, 4, 7, 1))
        assert not accepts(form, (1, 8, 2, 5, 1))
        assert not accepts(form, (1, 7, 1))
        assert not accepts(form, (2, 0, 0, 0, 1))
        assert accepts(parse("(Y-group 1 5 (1 8 3 8 1))"), (1, 8, 3, 8, 1))
        assert not accepts(parse("(Y-group 1 5 (1 8 3 8 1))"), (1, 7, 4, 7, 1))

    def test_membership_of_the_other_types(self):
        """`(C-group 1 3)` is `(1 1 1)` [M86 p. 54]; `(S-group 2 3)` is `(2 3 4)` [M86 p. 56];
        `(Tuple (5 8 3))` is the group `5 8 3` [M86 p. 55]; `(Cycle (4 7))` [M86 p. 55]."""
        assert accepts(parse("(C-group 1 3)"), (1, 1, 1))
        assert not accepts(parse("(C-group 1 3)"), (1, 1)) and not accepts(parse("(C-group 1 3)"), (2, 2, 2))
        assert accepts(parse("(S-group 2 3)"), (2, 3, 4)) and not accepts(parse("(S-group 2 3)"), (2, 3))
        assert accepts(parse("(P-group 9 4)"), (9, 8, 7, 6))
        assert accepts(parse("(Tuple (5 8 3))"), (5, 8, 3)) and not accepts(parse("(Tuple (5 8 3))"), (5, 8))
        assert accepts(parse("(Cycle (4 7))"), (4, 7)) and not accepts(parse("(Cycle (4 7))"), (7, 4))
        assert accepts(parse("(Countup 3)"), (3, 4, 5)) and not accepts(parse("(Countup 3)"), (4, 5))
        assert accepts(parse("(Constant 2)"), (2, 2)) and not accepts(parse("(Constant 2)"), (3, 3))

    def test_form_valued_parameters_are_not_checked(self):
        """A parameter that is itself a form is left to the Template-applier [M86 pp. 74-75]:
        `(S-group (Countup 1) 2)` accepts any length-2 successor run."""
        assert accepts(parse("(S-group (Countup 1) 2)"), (3, 4))
        assert not accepts(parse("(S-group (Countup 1) 2)"), (3, 4, 5))
        assert accepts(parse("(Tuple ((Countup 1) 8 shared))"), (4, 8, 4))
        assert not accepts(parse("(Tuple ((Countup 1) 8 shared))"), (4, 7, 4))
        assert accepts(Form("Y-group", 1, 3, None), (1, 2, 1))
        assert not accepts(Form("Y-group", 1, 3, None), (2, 1, 2))

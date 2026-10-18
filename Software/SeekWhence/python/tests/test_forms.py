"""The form language [M86 pp. 49-58, 161-166]: parse, print, the four slots, `shared`."""

import pytest

from seekwhence.forms import (
    Form, Shared, default_shared_index, flatten, form_text, parse, start_value, tokenize,
    value_text)

# Every form of SPEC.md section 4.6's table that the thesis prints, in its printed spelling,
# with the program spelling the reconstruction prints for it (DECISION D-001).
PRINTED_FORMS = [
    ("(Constant 3)", "(Constant 3)", 50),
    ("(Countup 3)", "(Countup 3)", 50),
    ("(Countup 8)", "(Countup 8)", 50),
    ("(Constant 2)", "(Constant 2)", 56),
    ("(C-group 2 3)", "(C-group 2 3)", 51),
    ("(C-group (Countup 1) 2)", "(C-group (Countup 1) 2)", 51),
    ("(S-group 2 3)", "(S-group 2 3)", 51),
    ("(S-group 5 4)", "(S-group 5 4)", 51),
    ("(S-group (Countup 1) 2)", "(S-group (Countup 1) 2)", 51),
    ("(P-group 8 4)", "(P-group 8 4)", 52),
    ("(P-group 2 4)", "(P-group 2 4)", 52),
    ("(Y-group 1 5 (1 8 3 8 1))", "(Y-group 1 5 (1 8 3 8 1))", 52),
    ("(Y-group ((Countup 1) 8 (Countup 1)))", "(Y-group 1 3 ((Countup 1) 8 (Countup 1)))", 52),
    ("(Cycle (2 1 8))", "(Cycle 2 3 (2 1 8))", 53),
    ("(Cycle (3 (Countup 1)))", "(Cycle 3 2 (3 (Countup 1)))", 53),
    ("(Tuple (1 8 4))", "(Tuple 1 3 (1 8 4))", 53),
    ("(Tuple (2 2))", "(Tuple 2 2 (2 2))", 54),
    ("(C-group 1 3)", "(C-group 1 3)", 54),
    ("(C-group 1 5)", "(C-group 1 5)", 56),
    ("(P-group 9 4)", "(P-group 9 4)", 56),
    ("(Y-group 1 5)", "(Y-group 1 5)", 55),
    ("(Tuple (5 8 3))", "(Tuple 5 3 (5 8 3))", 55),
    ("(Cycle (4 7))", "(Cycle 4 2 (4 7))", 55),
    ("(S-group 1 (Countup 2))", "(S-group 1 (Countup 2))", 56),
    ("(C-group (S-group 1 (Countup 1)) 2)", "(C-group (S-group 1 (Countup 1)) 2)", 57),
    ("(Cycle (8 (Countup 1) shared))", "(Cycle 8 3 (8 (Countup 1) shared))", 57),
    ("(Cycle (8 (Countup 1) (Countup 1)))", "(Cycle 8 3 (8 (Countup 1) (Countup 1)))", 84),
    ("(Tuple (2 (Countup 1) 2))", "(Tuple 2 3 (2 (Countup 1) 2))", 57),
    ("(Tuple (2 (Countup 1) (shared 0)))", "(Tuple 2 3 (2 (Countup 1) (shared 0)))", 58),
    ("(Tuple (5 (Countup 1) 2))", "(Tuple 5 3 (5 (Countup 1) 2))", 58),
    ("(Tuple (5 (Countup 1) (shared 0)))", "(Tuple 5 3 (5 (Countup 1) (shared 0)))", 58),
    ("(S-group 1 (Countup 1))", "(S-group 1 (Countup 1))", 59),
    ("(Cycle (2 (Countup 1)))", "(Cycle 2 2 (2 (Countup 1)))", 59),
    ("(C-group (S-group 4 3) 2)", "(C-group (S-group 4 3) 2)", 80),
    ("(S-group (Countup 4) 3)", "(S-group (Countup 4) 3)", 81),
    ("(Cycle ((Countup 1) (Tuple (5 8 4))))", "(Cycle 1 2 ((Countup 1) (Tuple 5 3 (5 8 4))))", 81),
    ("(Cycle ((Countup 1) 1))", "(Cycle 1 2 ((Countup 1) 1))", 81),
    ("(Cycle (1 (S-group 1 2)))", "(Cycle 1 2 (1 (S-group 1 2)))", 81),
    ("(S-group 1 2)", "(S-group 1 2)", 81),
    ("(Y-group [1] [3] (1 2 1))", "(Y-group 1 3 (1 2 1))", 81),
    ("(C-group (S-group 2 3) 2)", "(C-group (S-group 2 3) 2)", 82),
    ("(C-group (S-group 2 3) (shared 0 0))", "(C-group (S-group 2 3) (shared 0 0))", 84),
    ("(C-group (S-group (Countup 1) 3) 2)", "(C-group (S-group (Countup 1) 3) 2)", 96),
    ("(C-group (Countup 1) (Countup 1))", "(C-group (Countup 1) (Countup 1))", 97),
    ("(C-group (Countup 1) shared)", "(C-group (Countup 1) shared)", 161),
    ("(Tuple 1 3 ((Countup 1) 8 shared))", "(Tuple 1 3 ((Countup 1) 8 shared))", 162),
    ("(Cycle 1 3 ((Countup 1) 8 shared))", "(Cycle 1 3 ((Countup 1) 8 shared))", 163),
    ("(Cycle 3 2 (3 7))", "(Cycle 3 2 (3 7))", 166),
    ("(Cycle 3 3 (3 7 3))", "(Cycle 3 3 (3 7 3))", 169),
    ("(Countup 1)", "(Countup 1)", 171),
    ("(S-group 1 1)", "(S-group 1 1)", 175),
    ("(S-group 1 3)", "(S-group 1 3)", 179),
]


@pytest.mark.parametrize("text,printed,page", PRINTED_FORMS, ids=[f[0] for f in PRINTED_FORMS])
def test_every_printed_form_parses_and_prints(text, printed, page):
    """Every form the thesis prints parses, prints in the program spelling (the transcripts'
    `(Cycle 3 2 (3 7))` [M86 p. 166]) and survives a round trip [M86 pp. 50-59, 80-97,
    161-179]."""
    form = parse(text)
    assert form_text(form) == printed
    assert parse(printed) == form
    assert str(form) == printed
    assert parse(form_text(form, prose=True)) == form


def test_prose_spelling_drops_the_fence_parameters():
    """The prose writes `(Cycle (2 1 8))` [M86 p. 53] and `(Tuple (1 8 4))` [M86 p. 53];
    the program writes `(Cycle 3 2 (3 7))` [M86 p. 166] (DECISION D-001)."""
    assert form_text(parse("(Cycle 3 2 (3 7))"), prose=True) == "(Cycle (3 7))"
    assert form_text(parse("(Cycle (2 1 8))")) == "(Cycle 2 3 (2 1 8))"
    assert form_text(parse("(Tuple (1 8 4))"), prose=True) == "(Tuple (1 8 4))"
    assert form_text(parse("(Y-group 1 5)"), prose=True) == "(Y-group 1 5)"


def test_the_four_slots():
    """"(typename start-value length actual-value)" [M86 p. 49]; which slots each type uses
    (DECISION D-001)."""
    assert parse("(Countup 3)") == Form("Countup", 3)
    assert parse("(S-group 2 3)") == Form("S-group", 2, 3)
    assert parse("(S-group 1 (Countup 2))") == Form("S-group", 1, Form("Countup", 2))
    assert parse("(Cycle 3 2 (3 7))") == Form("Cycle", 3, 2, (3, 7))
    assert parse("(Cycle (3 7))") == Form("Cycle", 3, 2, (3, 7))
    assert parse("(Y-group 1 5)") == Form("Y-group", 1, 5, None)
    assert parse("(Y-group 1 5 (1 8 3 8 1))") == Form("Y-group", 1, 5, (1, 8, 3, 8, 1))


def test_a_fence_forms_start_is_its_first_elements_start_value():
    """The appendix prints `(Tuple 1 3 ((Countup 1) 8 shared))` [M86 p. 162] and
    `(Cycle 1 3 ((Countup 1) 8 shared))` [M86 p. 163]: the start is 1, the Countup's start,
    not the Countup (DECISION D-001)."""
    form = parse("(Tuple ((Countup 1) 8 shared))")
    assert form.start == 1 and form.length == 3
    assert form_text(form) == "(Tuple 1 3 ((Countup 1) 8 shared))"
    assert start_value(parse("(Cycle ((Countup 1) (Tuple (5 8 4))))")) == 1
    assert start_value(parse("(C-group (S-group 4 3) 2)")) == 4
    assert start_value(parse("(Tuple (5 8 3))")) == 5
    assert start_value(parse("(Tuple ((C-group (Countup 2) 3) 1))")) == 2


def test_explicit_parameters_are_kept():
    """`(Cycle 3 3 (3 7 3))` [M86 p. 169] keeps the parameters it is given, and the
    brackets of `(Y-group [1] [3] (1 2 1))` [M86 p. 81] mark optional parameters
    [M86 p. 49]."""
    assert parse("(Cycle 3 3 (3 7 3))") == Form("Cycle", 3, 3, (3, 7, 3))
    assert parse("(Y-group [1] [3] (1 2 1))") == Form("Y-group", 1, 3, (1, 2, 1))
    assert tokenize("(Y-group [1] [3] (1 2 1))") == tokenize("(Y-group 1 3 (1 2 1))")


class TestShared:
    """`shared` and its arrow [M86 pp. 56-58, 84, 161-163] (DECISION D-002)."""

    def test_bare_shared_refers_to_the_nearest_form_to_its_left(self):
        """`(Cycle (8 (Countup 1) shared))`: the arrow points to the Countup [M86 p. 57]."""
        form = parse("(Cycle (8 (Countup 1) shared))")
        assert form.actual == (8, Form("Countup", 1), Shared(1))
        assert default_shared_index((8, Form("Countup", 1))) == 1

    def test_a_shared_length_is_the_start(self):
        """`(C-group (Countup 1) shared)`: the length is the same Countup as the start
        [M86 p. 161]."""
        form = parse("(C-group (Countup 1) shared)")
        assert form.length == Shared(0)
        assert form.elements == (Form("Countup", 1), Shared(0))

    def test_shared_index_spelling_for_the_bracketing_integer(self):
        """On p. 58 the arrow points from `shared` to the 2: `(Tuple (2 (Countup 1) shared))`
        [M86 p. 58] is written `(Tuple 2 3 (2 (Countup 1) (shared 0)))`."""
        form = parse("(Tuple (2 (Countup 1) (shared 0)))")
        assert form.actual == (2, Form("Countup", 1), Shared(0))
        assert form_text(form) == "(Tuple 2 3 (2 (Countup 1) (shared 0)))"
        # The default index prints as the bare word even when written with its index.
        assert form_text(parse("(Cycle (8 (Countup 1) (shared 1)))")) == \
            "(Cycle 8 3 (8 (Countup 1) shared))"

    def test_shared_path_into_a_nested_structure(self):
        """On p. 84 the arrow runs from the length of `(C-group (S-group 2 3) shared)` to the
        2 inside the S-group, "so that the sameness of the 2's were to be modeled explicitly"
        [M86 p. 84]."""
        form = parse("(C-group (S-group 2 3) (shared 0 0))")
        assert form.length == Shared((0, 0)) and form.length.path == (0, 0)
        assert Shared(1).path == (1,)
        assert form_text(form) == "(C-group (S-group 2 3) (shared 0 0))"
        assert parse("(Tuple ((S-group (Countup 1) 2) (shared 0 0)))").actual[1] == Shared((0, 0))
        with pytest.raises(ValueError):
            parse("(C-group (S-group 2 3) (shared 0 2))")
        with pytest.raises(ValueError):
            parse("(C-group 2 (shared 0 0))")

    def test_shared_with_nothing_to_share_is_refused(self):
        """A bare `shared` needs a structure to its left [M86 p. 56]."""
        with pytest.raises(ValueError):
            parse("(Tuple (2 shared))")
        with pytest.raises(ValueError):
            parse("(Tuple (shared 2))")
        with pytest.raises(ValueError):
            parse("(Tuple ((Countup 1) (shared 1)))")
        with pytest.raises(ValueError):
            parse("(Tuple ((Countup 1) (shared 2)))")

    def test_build_box_spelling(self):
        """The appendix's `(same pstrucN)` with its binding list [M86 pp. 161-163]."""
        assert parse("(C-group (same pstruc1) (same pstruc1))", {"pstruc1": "(Countup 1)"}) \
            == parse("(C-group (Countup 1) shared)")
        assert parse("(Tuple 1 3 ((same pstruc2) 8 (same pstruc2)))",
                     {"pstruc2": "(Countup 1)"}) == parse("(Tuple 1 3 ((Countup 1) 8 shared))")
        assert parse("(Cycle 1 3 ((same pstruc3) 8 (same pstruc3)))",
                     {"pstruc3": parse("(Countup 1)")}) == \
            parse("(Cycle 1 3 ((Countup 1) 8 shared))")
        with pytest.raises(ValueError):
            parse("(Cycle 1 3 ((same pstruc3) 8 (same pstruc3)))")


class TestRefusals:
    """Malformed forms are refused rather than guessed at [M86 pp. 49-56]."""

    @pytest.mark.parametrize("text", [
        "(Countup)", "(Countup 1 2)", "(Constant 1 2)", "(C-group 1)", "(S-group 1 2 3)",
        "(Cycle)", "(Cycle 1)", "(Cycle 1 2 3 (1 2))", "(Tuple ())", "(Zed 1)", "3", "()",
        "(Countup -1)", "(Countup 1", "(Countup 1))", "(S-group (2 3))",
    ])
    def test_malformed(self, text):
        with pytest.raises(ValueError):
            parse(text)

    def test_form_checks_its_slots(self):
        with pytest.raises(ValueError):
            Form("Zed", 1)
        with pytest.raises(ValueError):
            Form("Countup")
        with pytest.raises(ValueError):
            Form("Countup", 1, 2)
        with pytest.raises(ValueError):
            Form("S-group", 1)
        with pytest.raises(ValueError):
            Form("S-group", 1, 2, (1, 2))
        with pytest.raises(ValueError):
            Form("Cycle")
        with pytest.raises(TypeError):
            Form("Cycle", 3, 2, [3, 7])


def test_value_text_and_flatten():
    """Values print as the thesis prints hits: `(2 2 2)` [M86 p. 51], `3` [M86 p. 50],
    `((1 2) (1 2))` [M86 p. 57]; flattened they are the sequence [M86 p. 56]."""
    assert value_text((2, 2, 2)) == "(2 2 2)"
    assert value_text(3) == "3"
    assert value_text(((1, 2), (1, 2))) == "((1 2) (1 2))"
    assert flatten(((1, 2), (1, 2))) == [1, 2, 1, 2]
    assert flatten(3) == [3]
    assert flatten(()) == []

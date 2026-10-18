"""Boxes, printstructures and firers [M86 pp. 81-85, 161-163]: Figure 3, the shared
printstructures of p. 84, `ready`, and the appendix's three `build-box` sessions."""

import pytest

from seekwhence.boxes import Box, BoxTree, Printstructure, build_box, show_box
from seekwhence.forms import parse, value_text
from seekwhence.platoplasm import hits, terms

from test_platoplasm import HITS, SEQUENCES  # tests/ is on sys.path under pytest


def node(tree: BoxTree, name: str):
    """A box or printstructure of the tree by name."""
    for item in tree.boxes + tree.pstrucs:
        if item.name == name:
            return item
    raise KeyError(name)


# ---- Figure 3 [M86 p. 83] ---------------------------------------------------------------

class TestFigure3:
    def test_the_box_tree_of_c_group_s_group_2_3_2(self):
        """Figure 3, "A box tree for the form (C-group (S-group 2 3) 2)" [M86 p. 83]: Box1
        on Pstruc1 (C-group, n-val Box2, k-val Box5); Box2 on Pstruc2 (S-group, n-val Box3,
        k-val Box4); Pstruc3, Pstruc4 and Pstruc5 Constants with values 2, 3 and 2, each
        feeding its one box."""
        tree = BoxTree(parse("(C-group (S-group 2 3) 2)"))
        assert tree.structure_text() == "\n".join([
            "box1 printstruc: pstruc1",
            "box2 printstruc: pstruc2",
            "box3 printstruc: pstruc3",
            "box4 printstruc: pstruc4",
            "box5 printstruc: pstruc5",
            "pstruc1 type: C-group boxes: (box1) n-val: box2 k-val: box5",
            "pstruc2 type: S-group boxes: (box2) n-val: box3 k-val: box4",
            "pstruc3 type: Constant boxes: (box3) value: 2",
            "pstruc4 type: Constant boxes: (box4) value: 3",
            "pstruc5 type: Constant boxes: (box5) value: 2",
        ])
        assert tree.top is node(tree, "box1")

    def test_the_fire_boxes_of_pstruc1_are_box2_and_box5(self):
        """"the 'fire-boxes' for Pstruc1 -- a C-group printstructure -- are its 'n-val' and
        'k-val' boxes, namely Box2 and Box5" [M86 p. 83]."""
        tree = BoxTree(parse("(C-group (S-group 2 3) 2)"))
        pstruc1 = node(tree, "pstruc1")
        assert [b.name for b in pstruc1.fire_boxes] == ["box2", "box5"]
        assert [b.name for b in node(tree, "pstruc2").fire_boxes] == ["box3", "box4"]
        assert node(tree, "pstruc3").fire_boxes == []

    def test_a_hit_bubbles_up_to_the_top_box(self):
        """"a hit on a top-level box propagates down through the tree ... until the
        top-level answer appears in the top box's 'pstruc-val' field" [M86 pp. 82-83]: the
        value is `((2 3 4) (2 3 4))` [M86 p. 82], and after the hit Box2 holds `(2 3 4)`
        and Box5 holds 2, both ready [M86 pp. 83-84]."""
        tree = BoxTree(parse("(C-group (S-group 2 3) 2)"))
        assert tree.hit() == ((2, 3, 4), (2, 3, 4))
        assert tree.top.pstruc_val == ((2, 3, 4), (2, 3, 4)) and tree.top.ready
        assert node(tree, "box2").pstruc_val == (2, 3, 4) and node(tree, "box2").ready
        assert node(tree, "box5").pstruc_val == 2 and node(tree, "box5").ready

    def test_the_shared_variant_of_figure_3(self):
        """"If the form modeled had been '(C-group (S-group 2 3) shared)' ... Pstruc3 would
        have '(box3 box5)' in its 'boxes' field, and there would be no need for Pstruc5.
        Box5 would point to Pstruc3 as its 'printstruc'" [M86 p. 84]."""
        tree = BoxTree(parse("(C-group (S-group 2 3) (shared 0 0))"))
        assert [b.name for b in node(tree, "pstruc3").boxes] == ["box3", "box5"]
        assert node(tree, "box5").printstruc is node(tree, "pstruc3")
        assert [p.name for p in tree.pstrucs] == ["pstruc1", "pstruc2", "pstruc3", "pstruc4"]
        assert tree.hit() == ((2, 3, 4), (2, 3, 4))


# ---- shared printstructures [M86 p. 84] --------------------------------------------------

class TestSharedPrintstructures:
    def test_cycle_8_countup_1_shared_gives_8_1_2_8_3_4(self):
        """"'8 1 2 8 3 4 8 5 6...', which can be described by the form '(Cycle (8 (Countup
        1) shared))'. Modeling this form requires the creation of three boxes: one for the
        'Constant 8', one for the first 'Countup 1', and one for the second 'Countup 1'";
        "only one Countup printstructure is created" [M86 p. 84]."""
        tree = BoxTree(parse("(Cycle (8 (Countup 1) shared))"))
        assert tree.show(9) == [8, 1, 2, 8, 3, 4, 8, 5, 6]
        cycle = tree.top.printstruc
        assert len(cycle.elements) == 3
        assert [p.type for p in tree.pstrucs].count("Countup") == 1
        assert cycle.elements[1].printstruc is cycle.elements[2].printstruc

    def test_the_unshared_form_gives_8_1_1_8_2_2(self):
        """"the form '(Cycle (8 (Countup 1) (Countup 1)))', which also requires three
        boxes, and which corresponds to the sequence '8 1 1 8 2 2 8 3 3...'"; "the second
        form causes creation of different printstructures for the two Countup boxes"
        [M86 p. 84]."""
        tree = BoxTree(parse("(Cycle (8 (Countup 1) (Countup 1)))"))
        assert tree.show(9) == [8, 1, 1, 8, 2, 2, 8, 3, 3]
        cycle = tree.top.printstruc
        assert len(cycle.elements) == 3
        assert [p.type for p in tree.pstrucs].count("Countup") == 2
        assert cycle.elements[1].printstruc is not cycle.elements[2].printstruc

    def test_the_shared_printstructure_feeds_both_boxes(self):
        """"When that printstructure fires in response to a hit on the first Countup box, it
        feeds both Countup boxes, making both boxes 'ready'. Later on, when the second
        Countup box is hit, the same printstructure will fire, again feeding both boxes,
        but this time with the next value in sequence" [M86 p. 84]."""
        tree = BoxTree(parse("(Cycle (8 (Countup 1) shared))"))
        first, second = tree.top.printstruc.elements[1], tree.top.printstruc.elements[2]
        tree.hit()                                       # 8
        assert tree.hit() == 1                           # the first Countup box is hit
        assert first.ready and second.ready
        assert first.pstruc_val == second.pstruc_val == 1
        assert tree.hit() == 2                           # the second Countup box is hit
        assert first.pstruc_val == second.pstruc_val == 2

    def test_tuple_5_countup_1_shared_generalisation(self):
        """The two generalisations of `2 1 2 2 2 2 2 3 2 ...` to 5 [M86 pp. 57-58]:
        `(Tuple (5 (Countup 1) 2))`, "because the bracketing integers are seen as distinct",
        gives `5 1 2`, `5 2 2`, `5 3 2`; `(Tuple (5 (Countup 1) shared))` with the arrow to
        the 5 gives `5 1 5`, `5 2 5`, `5 3 5` [M86 p. 58]."""
        distinct = BoxTree(parse("(Tuple (5 (Countup 1) 2))"))
        assert distinct.show(3) == [(5, 1, 2), (5, 2, 2), (5, 3, 2)]
        same = BoxTree(parse("(Tuple (5 (Countup 1) (shared 0)))"))
        assert same.show(3) == [(5, 1, 5), (5, 2, 5), (5, 3, 5)]
        elements = same.top.printstruc.elements
        assert elements[0].printstruc is elements[2].printstruc
        assert len(same.pstrucs) == len(distinct.pstrucs) - 1


# ---- ready [M86 pp. 84-85] (DECISION D-003) -----------------------------------------------

class TestReady:
    def test_a_ready_box_reports_its_value_without_firing(self):
        """Boxes "set their 'ready' fields to 'true' -- so that they can report this value
        until the next time they are hit" [M86 p. 85]: a box hit while ready does not make
        its printstructure fire."""
        tree = BoxTree(parse("(Countup 3)"))
        assert tree.hit() == 3
        assert tree.top.ready
        assert tree.top.hit() == 3                       # hit directly: still ready, same value
        assert tree.hit() == 4                           # a top-level hit clears ready first

    def test_fresh_boxes_are_not_ready(self):
        """A box before any hit has no value and is not ready [M86 p. 83]."""
        tree = BoxTree(parse("(C-group 1 3)"))
        assert all(not b.ready and b.pstruc_val is None for b in tree.boxes)

    def test_reset_starts_from_the_beginning(self):
        """"Boxes can also be reset to start from the beginning of the pattern described"
        [M86 p. 85]."""
        tree = BoxTree(parse("(S-group 1 (Countup 2))"))
        assert tree.show(2) == [(1, 2), (1, 2, 3)]
        tree.reset()
        assert all(not b.ready and b.pstruc_val is None for b in tree.boxes)
        assert tree.show(2) == [(1, 2), (1, 2, 3)]
        cycle = BoxTree(parse("(Cycle 3 2 (3 7))"))
        cycle.hit()
        cycle.reset()
        assert cycle.show(2) == [3, 7]


# ---- the appendix's build-box and show-box [M86 pp. 161-163] -----------------------------

class TestAppendixSessions:
    def test_p161_c_group_countup_1_shared(self):
        """`(build-box '(C-group (same pstruc1) (same pstruc1)) '((pstruc1 (Countup 1))))`
        then `show-box` four times: `(1)`, `(2 2)`, `(3 3 3)`, `(4 4 4 4)` [M86 p. 161]."""
        tree = build_box("(C-group (same pstruc1) (same pstruc1))", {"pstruc1": "(Countup 1)"})
        assert [show_box(tree) for _ in range(4)] == ["(1)", "(2 2)", "(3 3 3)", "(4 4 4 4)"]
        assert str(tree.form) == "(C-group (Countup 1) shared)"
        countup = tree.top.printstruc.n_val.printstruc
        assert countup.type == "Countup" and len(countup.boxes) == 2
        assert tree.top.printstruc.k_val.printstruc is countup

    def test_p162_tuple_1_3_countup_1_8_shared(self):
        """`(build-box '(Tuple 1 3 ((same pstruc2) 8 (same pstruc2))) '((pstruc2 (Countup
        1))))` then `show-box`: `(1 8 1)`, `(2 8 2)`, `(3 8 3)`, `(4 8 4)` [M86 p. 162]."""
        tree = build_box("(Tuple 1 3 ((same pstruc2) 8 (same pstruc2)))",
                         {"pstruc2": "(Countup 1)"})
        assert [show_box(tree) for _ in range(4)] == ["(1 8 1)", "(2 8 2)", "(3 8 3)", "(4 8 4)"]
        assert str(tree.form) == "(Tuple 1 3 ((Countup 1) 8 shared))"

    def test_p163_cycle_1_3_countup_1_8_shared(self):
        """`(build-box '(Cycle 1 3 ((same pstruc3) 8 (same pstruc3))) '((pstruc3 (Countup
        1))))` then `show-box` nine times: `1 8 2 3 8 4 5 8 6` [M86 p. 163]."""
        tree = build_box("(Cycle 1 3 ((same pstruc3) 8 (same pstruc3)))",
                         {"pstruc3": "(Countup 1)"})
        assert [show_box(tree) for _ in range(9)] == ["1", "8", "2", "3", "8", "4", "5", "8", "6"]
        assert str(tree.form) == "(Cycle 1 3 ((Countup 1) 8 shared))"

    def test_build_box_accepts_the_other_spellings(self):
        """The prose and program spellings build the same tree as the appendix's
        (DECISION D-002)."""
        a = build_box("(C-group (same pstruc1) (same pstruc1))", {"pstruc1": "(Countup 1)"})
        b = build_box("(C-group (Countup 1) shared)")
        assert a.structure_text() == b.structure_text()
        assert a.show(3) == b.show(3)

    def test_box_numbers_run_on_across_trees_with_shared_counters(self):
        """One counter per class per run (DECISION D-014): a second tree built with the same
        counters continues the numbering, as the appendix's `box5`, `box12`, `box19` do
        [M86 pp. 161-163]; the numbers themselves are not reproduced (DECISION D-056)."""
        counters = {"box": 0, "pstruc": 0}
        first = build_box("(C-group (Countup 1) shared)", counters=counters)
        second = build_box("(Tuple 1 3 ((Countup 1) 8 shared))", counters=counters)
        assert first.top.name == "box1"
        assert second.top.name == f"box{len(first.boxes) + 1}"
        assert [b.name for b in first.boxes + second.boxes] == [
            f"box{i}" for i in range(1, len(first.boxes) + len(second.boxes) + 1)]


# ---- agreement with the reference evaluator [M86 pp. 49-58] ---------------------------------

@pytest.mark.parametrize("text,expected,page", HITS, ids=[f"{h[0]} p.{h[2]}" for h in HITS])
def test_the_box_tree_agrees_with_hits_on_every_printed_form(text, expected, page):
    """The box tree gives the same values as `platoplasm.hits` on every form the thesis
    evaluates [M86 pp. 50-59, 80-84, 96-97, 161-163, 166-179], and those are the printed
    ones."""
    n = expected.count(";") + 1
    form = parse(text)
    values = BoxTree(form).show(n)
    assert values == hits(form, n)
    assert "; ".join(value_text(v) for v in values) == expected


@pytest.mark.parametrize("text,sequence,page", SEQUENCES,
                         ids=[f"{s[0]} p.{s[2]}" for s in SEQUENCES])
def test_the_box_tree_gives_the_printed_sequences(text, sequence, page):
    """The terms a box tree gives are the "giving:" sequences [M86 pp. 56-59, 80-81, 96-97]."""
    expected = [int(t) for t in sequence.split()]
    form = parse(text)
    out = []
    tree = BoxTree(form)
    while len(out) < len(expected):
        value = tree.hit()
        out.extend([value] if isinstance(value, int) else
                   [t for t in _flat(value)])
    assert out[:len(expected)] == expected == terms(form, len(expected))


def _flat(value):
    if isinstance(value, int):
        return [value]
    return [t for part in value for t in _flat(part)]


# ---- errors ---------------------------------------------------------------------------------

class TestErrors:
    def test_p_group_below_zero_raises_on_hit(self):
        """`(P-group 2 4)` "would run to negative numbers" and is undefined [M86 p. 52]: the
        tree builds but the hit raises (DECISION D-006)."""
        tree = BoxTree(parse("(P-group 2 4)"))
        with pytest.raises(ValueError):
            tree.hit()

    def test_a_form_without_elements_cannot_be_built(self):
        """`(Y-group 1 5)` names a class of groups [M86 p. 55], not a pattern to hit."""
        with pytest.raises(ValueError):
            BoxTree(parse("(Y-group 1 5)"))

    def test_only_forms_build_trees(self):
        with pytest.raises(TypeError):
            BoxTree("(Countup 1)")

    def test_a_countup_needs_an_integer_start(self):
        """A Countup "returns nonnegative integers in succession, starting with its
        argument" [M86 p. 50]: a Countup on a group has no next value."""
        with pytest.raises(ValueError):
            BoxTree(parse("(Countup (C-group 1 2))")).hit()

    def test_the_node_classes_are_plain(self):
        """A `Box` has `printstruc`, `pstruc_val`, `ready`; a `Printstructure` `type` and
        `boxes` [M86 p. 83, Figure 3]."""
        pstruc = Printstructure("pstruc1", "Constant", value=4)
        box = Box("box1", pstruc)
        pstruc.boxes.append(box)
        assert (box.pstruc_val, box.ready) == (None, False)
        assert box.hit() == 4 and box.ready and box.pstruc_val == 4

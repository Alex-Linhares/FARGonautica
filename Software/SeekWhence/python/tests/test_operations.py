"""The nine gnoth operations, PROGRAMs, the Gnoth-operator and the Capper [M86 pp. 109-124,
174, 177-178] (DECISIONS D-044, D-045, D-065, D-066, D-067)."""

import io

import pytest

from seekwhence import bonds, gnoths, operations, slipping
from seekwhence import cytoplasm as cy
from seekwhence.config import TASK_TYPES
from seekwhence.events import Recorder
from seekwhence.forms import value_text
from seekwhence.gnoths import Gnoth
from seekwhence.operations import (New, Outcome, apply_operation, capper, contiguous,
                                   gnoth_operator, is_program, operation_text, precheck,
                                   run_program, steps_of, uncover)
from seekwhence.run import enter_term, new_world, run_until_prompt
from seekwhence.trace import Trace

GNOTH = ("gnoth",)


# ---- helpers -------------------------------------------------------------------------------

def still_world(*terms):
    """A world with the terms entered and no task run: the tests build the gloms and gnoths
    by hand, as the thesis's examples give them."""
    recorder, trace = Recorder(), Trace(io.StringIO())
    world = new_world(seed=0, observers=[recorder, trace])
    for term in terms:
        bonds.new_term(world, term)
    world.taskrack.clean(TASK_TYPES)
    return world, recorder, trace


def glom_over(world, *names, type=("Same", "print-value", "group")):
    return cy.make_glom(world, type, [world.find(n) for n in names])


def gnoth_over(world, *names, kind="S-group", frame=0) -> Gnoth:
    return gnoths.make_gnoth(world, kind, [world.find(n) for n in names], frame)


def bracket(gnoth: Gnoth) -> str:
    """A gnoth's value as the thesis prints it, `[(1 1) (2 2)]` [M86 p. 110]."""
    return "[" + " ".join(value_text(x.print_value) for x in gnoth.collection) + "]"


def brackets(world) -> list:
    return [bracket(g) for g in gnoths.gnoths(world)]


def tops(gnoth: Gnoth) -> list:
    return [x.name for x in gnoth.collection]


def subs(world, name) -> list:
    return [x.name for x in world.find(name).subgloms]


def gone(world, *names) -> bool:
    return all(world.find(n) is None for n in names)


def run_to_prompt(world) -> str:
    while True:
        result = run_until_prompt(world)
        if result != "term-cap":
            return result


def full_run(terms, seed):
    """A run with the Reformulator turned off, so that the tests post the transcript's
    operations by hand on the gnoths the Gnoth-setter made."""
    recorder, trace = Recorder(), Trace(io.StringIO())
    world = new_world(seed=seed, observers=[recorder, trace])
    world.handlers["Reformulator"] = lambda w, *args: None
    for term in terms:
        enter_term(world, term)
        run_to_prompt(world)
    return world, recorder, trace


# ---- the printed forms and the PROGRAM shape ---------------------------------------------------

class TestFormats:

    def test_the_nine_formats(self):
        """The formats of pp. 110, 112-113 [M86 pp. 110-113]."""
        assert operation_text(("SHIFT-LEFT", "gnoth2", "gnoth3", ("glom7", "glom10"))) == \
            "(SHIFT-LEFT gnoth2 gnoth3 (glom7 glom10))"
        assert operation_text(("SHIFT-RIGHT", "gnoth2", "gnoth3", ("glom4",))) == \
            "(SHIFT-RIGHT gnoth2 gnoth3 (glom4))"
        assert operation_text(("SPLIT", "gnoth2", ("glom6", "glom10"))) == "(SPLIT gnoth2 (glom6 glom10))"
        assert operation_text(("CAPTURE-RIGHT", "gnoth1", "glom1", "glint3")) == \
            "(CAPTURE-RIGHT gnoth1 glom1 glint3)"
        assert operation_text(("CAPTURE-LEFT", "gnoth1", "glom10", "glom7")) == \
            "(CAPTURE-LEFT gnoth1 glom10 glom7)"
        assert operation_text(("ENCLOSE", "gnoth1", ("glint1", "glint2", "glint3"))) == \
            "(ENCLOSE gnoth1 (glint1 glint2 glint3))"
        assert operation_text(("FRACTURE", "gnoth2")) == "(FRACTURE gnoth2)"
        assert operation_text(("MERGE", "gnoth1", ("glom1", "glom2"))) == "(MERGE gnoth1 (glom1 glom2))"
        assert operation_text(("NO-OP", "gnoth1")) == "(NO-OP gnoth1)"

    def test_nil_and_the_program_of_the_transcript(self):
        """`(ENCLOSE gnoth1 nil)` and the PROGRAM the Gnoth-operator printed [M86 pp. 122,
        174]; a bare operation is a one-step program [M86 p. 177] (DECISION D-067)."""
        assert operation_text(("ENCLOSE", "gnoth1", None)) == "(ENCLOSE gnoth1 nil)"
        assert operation_text(("ENCLOSE", "gnoth1", ())) == "(ENCLOSE gnoth1 nil)"
        program = ("PROGRAM", (("SHIFT-RIGHT", "gnoth1", "gnoth2", ("glint1",)),
                               ("ENCLOSE", "gnoth1", None)))
        assert operation_text(program) == \
            "(PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE gnoth1 nil)))"
        assert is_program(program) and not is_program(program[1][0])
        assert steps_of(program) == list(program[1])
        bare = ("SHIFT-RIGHT", "gnoth3", "gnoth5", ("glint3",))
        assert steps_of(bare) == [bare]

    def test_new_prints_as_a_reference(self):
        """`newglom3` of p. 122 is a reference to an earlier step (DECISION D-044)."""
        assert operation_text(("ENCLOSE", "gnoth4", (New(2), "glom4", "glom2"))) == \
            "(ENCLOSE gnoth4 ((new 2) glom4 glom2))"


# ---- the external operations [M86 pp. 110-111] -----------------------------------------------

class TestShift:

    def setup_method(self):
        """The initial state of the SHIFT EXAMPLE [M86 p. 110]: gnoth2 `[(1 1) (2 2)]` =
        glom3 glom5, gnoth3 `[(3 3) (4 4) (4 4)]` = glom7 glom10 glom15, over the terms
        `1 1 2 2 3 3 4 4 4 4` (the glom numbers here are the test's own)."""
        self.world, self.recorder, _ = still_world(1, 1, 2, 2, 3, 3, 4, 4, 4, 4)
        w = self.world
        self.a = glom_over(w, "glint1", "glint2")     # (1 1)
        self.b = glom_over(w, "glint3", "glint4")     # (2 2)
        self.c = glom_over(w, "glint5", "glint6")     # (3 3)
        self.d = glom_over(w, "glint7", "glint8")     # (4 4)
        self.e = glom_over(w, "glint9", "glint10")    # (4 4)
        self.left = gnoth_over(w, self.a.name, self.b.name, frame=0)
        self.right = gnoth_over(w, self.c.name, self.d.name, self.e.name, frame=1)

    def test_shift_left_example(self):
        """`(SHIFT-LEFT gnoth2 gnoth3 (glom7 glom10))` gives gnoth2 `[(1 1) (2 2) (3 3)
        (4 4)]` and gnoth3 `[(4 4)]`, the gloms keeping their identity [M86 p. 110]."""
        w, left, right = self.world, self.left, self.right
        out = apply_operation(w, ("SHIFT-LEFT", left.name, right.name, (self.c.name, self.d.name)))
        assert out.reason is None and out.acted
        assert brackets(w) == ["[(1 1) (2 2) (3 3) (4 4)]", "[(4 4)]"]
        assert tops(left) == [self.a.name, self.b.name, self.c.name, self.d.name]
        assert tops(right) == [self.e.name]
        assert left.range == (1, 8) and right.range == (9, 10)
        assert all(w.find(g.name) is g for g in (self.a, self.b, self.c, self.d, self.e))
        # both gnoths are recapped: two new pseudo-gloms, the old caps gone
        caps = [e.name for e in self.recorder.of_kind("top-down")][-2:]
        assert [left.glom.name, right.glom.name] == caps
        assert left.glom.pseudo and right.glom.pseudo

    def test_shift_right_moves_the_other_way(self):
        """SHIFT-RIGHT's glomlist goes "from left-gnoth to right-gnoth" [M86 p. 110]."""
        w, left, right = self.world, self.left, self.right
        out = apply_operation(w, ("SHIFT-RIGHT", left.name, right.name, (self.b.name,)))
        assert out.reason is None
        assert brackets(w) == ["[(1 1)]", "[(2 2) (3 3) (4 4) (4 4)]"]
        assert tops(right) == [self.b.name, self.c.name, self.d.name, self.e.name]

    def test_a_shift_of_everything_empties_the_giver(self):
        """A gnoth emptied by a shift disappears: `[] (disappears)` [M86 p. 120]."""
        w, left, right = self.world, self.left, self.right
        out = apply_operation(w, ("SHIFT-LEFT", left.name, right.name,
                                  (self.c.name, self.d.name, self.e.name)))
        assert out.gone == (right.name,) and out.gnoths == (left.name,)
        assert right.name not in w.socratoplasm
        assert brackets(w) == ["[(1 1) (2 2) (3 3) (4 4) (4 4)]"]
        assert [e.name for e in self.recorder.of_kind("gnoth-gone")] == [right.name]

    def test_a_shift_carries_what_lies_between_the_listed_glom_and_the_border(self):
        """`(SHIFT-RIGHT gnoth3 gnoth5 (glint3))` left `glom14 pseudo --> (1 2 3)  terms
        3 to 5` and no gnoth3 [M86 pp. 177-178]: the border moves past the listed glom, so
        what lies between it and the border goes too (DECISION D-065)."""
        w, left, right = self.world, self.left, self.right
        out = apply_operation(w, ("SHIFT-LEFT", left.name, right.name, (self.d.name,)))
        assert out.reason is None
        assert tops(left) == [self.a.name, self.b.name, self.c.name, self.d.name]
        assert tops(right) == [self.e.name]

    def test_a_nested_glom_is_uncovered_before_it_moves(self):
        """`(SHIFT-LEFT gnoth2 gnoth3 (glint6))` takes the 3 out of `(3 4 5)`, leaving
        gnoth3 with the bare glints `[4 5]` [M86 p. 120]."""
        w, _, _ = still_world(3, 4, 3, 4, 5)
        g2 = gnoth_over(w, glom_over(w, "glint1", "glint2").name)
        big = glom_over(w, "glint3", "glint4", "glint5")
        g3 = gnoth_over(w, big.name, frame=1)
        out = apply_operation(w, ("SHIFT-LEFT", g2.name, g3.name, ("glint3",)))
        assert out.reason is None
        assert brackets(w) == ["[(3 4) 3]", "[4 5]"]
        assert gone(w, big.name)

    def test_shift_refuses_what_is_not_there_or_not_under_the_giver(self):
        """A missing gnoth, a glom under the wrong gnoth, gnoths that are not
        neighbours, an empty list (DECISION D-067)."""
        w, left, right = self.world, self.left, self.right
        assert apply_operation(w, ("SHIFT-LEFT", left.name, "gnoth9", (self.c.name,))).reason
        assert "not under" in apply_operation(w, ("SHIFT-LEFT", left.name, right.name,
                                                  (self.a.name,))).reason
        assert "neighbours" in apply_operation(w, ("SHIFT-LEFT", right.name, left.name,
                                                   (self.a.name,))).reason
        assert apply_operation(w, ("SHIFT-LEFT", left.name, right.name, ())).reason
        assert apply_operation(w, ("SHIFT-LEFT", left.name, right.name, ("glom99",))).reason
        assert brackets(w) == ["[(1 1) (2 2)]", "[(3 3) (4 4) (4 4)]"]   # nothing changed


class TestSplit:

    def test_split_example(self):
        """`(SPLIT gnoth2 (glom6 glom10))`: gnoth2 `[(1 2) (2 3) (2 3)]` becomes `[(1 2)]`
        and a new gnoth `[(2 3) (2 3)]` [M86 pp. 110-112]."""
        w, recorder, _ = still_world(1, 2, 2, 3, 2, 3)
        a = glom_over(w, "glint1", "glint2")
        b = glom_over(w, "glint3", "glint4")
        c = glom_over(w, "glint5", "glint6")
        g2 = gnoth_over(w, a.name, b.name, c.name, frame=3)
        out = apply_operation(w, ("SPLIT", g2.name, (b.name, c.name)))
        assert out.reason is None and len(out.created) == 1
        assert brackets(w) == ["[(1 2)]", "[(2 3) (2 3)]"]
        new = w.find(out.created[0])
        assert tops(g2) == [a.name] and tops(new) == [b.name, c.name]
        assert (new.frame, new.plato_class, new.range) == (3, "S-group", (3, 6))
        assert new in w.platoplasm.type("S-group").actualizations
        assert [e.name for e in recorder.of_kind("gnoth")][-1] == new.name

    def test_split_at_the_left_end(self):
        """The splitlist may be the left run too; the new gnoth lies before the old
        (DECISION D-066)."""
        w, _, _ = still_world(1, 2, 2, 3, 2, 3)
        a, b, c = (glom_over(w, f"glint{i}", f"glint{i + 1}") for i in (1, 3, 5))
        g = gnoth_over(w, a.name, b.name, c.name)
        out = apply_operation(w, ("SPLIT", g.name, (a.name,)))
        assert out.reason is None
        assert [x.name for x in gnoths.gnoths(w)] == [out.created[0], g.name]
        assert brackets(w) == ["[(1 2)]", "[(2 3) (2 3)]"]

    def test_split_refuses_the_middle_the_whole_and_a_gap(self):
        """Only a run of neighbours at one end of the gnoth can be split off
        (DECISION D-066)."""
        w, _, _ = still_world(1, 2, 2, 3, 2, 3)
        a, b, c = (glom_over(w, f"glint{i}", f"glint{i + 1}") for i in (1, 3, 5))
        g = gnoth_over(w, a.name, b.name, c.name)
        assert "end" in apply_operation(w, ("SPLIT", g.name, (b.name,))).reason
        assert "whole" in apply_operation(w, ("SPLIT", g.name, (a.name, b.name, c.name))).reason
        assert "neighbours" in apply_operation(w, ("SPLIT", g.name, (a.name, c.name))).reason
        assert brackets(w) == ["[(1 2) (2 3) (2 3)]"]


# ---- the internal operations [M86 pp. 112-118] -----------------------------------------------

class TestCapture:

    def test_capture_right_example(self):
        """`(CAPTURE-RIGHT gnoth1 glom1 glint3)`: `[(1 2) 3]` becomes `[(1 2 3)]`, a new
        glom replacing glom1 [M86 p. 114]."""
        w, recorder, _ = still_world(1, 2, 3)
        glom1 = glom_over(w, "glint1", "glint2")
        g = gnoth_over(w, glom1.name, "glint3")
        out = apply_operation(w, ("CAPTURE-RIGHT", g.name, glom1.name, "glint3"))
        assert out.reason is None and len(out.made) == 1
        assert brackets(w) == ["[(1 2 3)]"]
        new = w.find(out.made[0])
        assert gone(w, glom1.name) and new.name != glom1.name
        assert subs(w, new.name) == ["glint1", "glint2", "glint3"]
        assert new.type == ("capture",) and not new.pseudo
        assert g.collection == (new,)
        posted = [e for e in recorder.of_kind("posted") if e.type == "Plato-scout"]
        assert posted[-1].args[1] == new.name and len(posted[-1].args[0]) == 6

    def test_capture_left_example(self):
        """`(CAPTURE-LEFT gnoth1 glom10 glom7)` on `[((1 2) (2 3) ((2 3) (2 3))) (1 2)]`
        gives `[(1 2) ((2 3) (2 3) (2 3)) (1 2)]`: glom15 is burst and not rebuilt, glom5
        and glom18 survive, glom7 keeps its identity inside the new glom20 and glom10 is
        destroyed [M86 pp. 114-115]."""
        w, _, _ = still_world(1, 2, 2, 3, 2, 3, 2, 3, 1, 2)
        glom5 = glom_over(w, "glint1", "glint2")
        glom7 = glom_over(w, "glint3", "glint4")
        glom8 = glom_over(w, "glint5", "glint6")
        glom9 = glom_over(w, "glint7", "glint8")
        glom10 = glom_over(w, glom8.name, glom9.name)
        glom15 = glom_over(w, glom5.name, glom7.name, glom10.name)
        glom18 = glom_over(w, "glint9", "glint10")
        g = gnoth_over(w, glom15.name, glom18.name)
        assert bracket(g) == "[((1 2) (2 3) ((2 3) (2 3))) (1 2)]"
        out = apply_operation(w, ("CAPTURE-LEFT", g.name, glom10.name, glom7.name))
        assert out.reason is None
        assert brackets(w) == ["[(1 2) ((2 3) (2 3) (2 3)) (1 2)]"]
        glom20 = w.find(out.made[0])
        assert tops(g) == [glom5.name, glom20.name, glom18.name]
        assert subs(w, glom20.name) == [glom7.name, glom8.name, glom9.name]
        assert gone(w, glom15.name, glom10.name)
        assert all(w.find(x.name) is x for x in (glom5, glom7, glom8, glom9, glom18))

    def test_capture_refuses_a_glint_captor_and_a_non_neighbour(self):
        """A glint has no subgloms to carry over; the captive must be the glom's
        neighbour on the named side (DECISION D-067)."""
        w, _, _ = still_world(1, 2, 3, 4)
        glom1 = glom_over(w, "glint1", "glint2")
        g = gnoth_over(w, glom1.name, "glint3", "glint4")
        assert "glint" in apply_operation(w, ("CAPTURE-RIGHT", g.name, "glint3", "glint4")).reason
        assert "neighbour" in apply_operation(w, ("CAPTURE-RIGHT", g.name, glom1.name, "glint4")).reason
        assert "neighbour" in apply_operation(w, ("CAPTURE-LEFT", g.name, glom1.name, "glint3")).reason
        assert "nested" in apply_operation(w, ("CAPTURE-LEFT", g.name, glom1.name, "glint1")).reason
        assert brackets(w) == ["[(1 2) 3 4]"]


class TestEnclose:

    def test_simple_enclose(self):
        """`(ENCLOSE gnoth1 (glint1 glint2 glint3))`: `[1 2 3 1]` becomes `[(1 2 3) 1]`, the
        new glom "dubbed to be of type 'enclose'" [M86 pp. 112, 115]."""
        w, _, _ = still_world(1, 2, 3, 1)
        g = gnoth_over(w, "glint1", "glint2", "glint3", "glint4")
        out = apply_operation(w, ("ENCLOSE", g.name, ("glint1", "glint2", "glint3")))
        assert out.reason is None
        assert brackets(w) == ["[(1 2 3) 1]"]
        assert w.find(out.made[0]).type == ("enclose",)

    def test_nested_enclose(self):
        """`(ENCLOSE gnoth3 (glom5 glom7))` on `[(2 2) ((3) ((3 3) (4 4))) ((4 4) (5 5))]`:
        glom12, glom10 and glom9 are burst to bring glom5 and glom7 to the top, and the
        result is `[(2 2) (3) (3 3) ((4 4) (4 4)) (5 5)]` with nothing rebuilt around
        glom4, glom6 and glom8 [M86 p. 116]."""
        w, _, _ = still_world(2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5)
        glom1 = glom_over(w, "glint1", "glint2")
        glom4 = glom_over(w, "glint3")
        glom6 = glom_over(w, "glint4", "glint5")
        glom5 = glom_over(w, "glint6", "glint7")
        glom10 = glom_over(w, glom6.name, glom5.name)
        glom12 = glom_over(w, glom4.name, glom10.name)
        glom7 = glom_over(w, "glint8", "glint9")
        glom8 = glom_over(w, "glint10", "glint11")
        glom9 = glom_over(w, glom7.name, glom8.name)
        g = gnoth_over(w, glom1.name, glom12.name, glom9.name)
        assert bracket(g) == "[(2 2) ((3) ((3 3) (4 4))) ((4 4) (5 5))]"
        out = apply_operation(w, ("ENCLOSE", g.name, (glom5.name, glom7.name)))
        assert out.reason is None
        assert brackets(w) == ["[(2 2) (3) (3 3) ((4 4) (4 4)) (5 5)]"]
        glom20 = w.find(out.made[0])
        assert tops(g) == [glom1.name, glom4.name, glom6.name, glom20.name, glom8.name]
        assert subs(w, glom20.name) == [glom5.name, glom7.name]
        assert gone(w, glom12.name, glom10.name, glom9.name)

    def test_enclose_nil_does_nothing_even_for_a_gone_gnoth(self):
        """`(ENCLOSE gnoth2 nil)`: "no repair necessary" [M86 p. 122]; the transcript's
        `(ENCLOSE gnoth1 nil)` names the gnoth the shift before it emptied [M86 p. 174]
        (DECISION D-067)."""
        w, recorder, _ = still_world(1, 2)
        g = gnoth_over(w, "glint1", "glint2")
        before = len(recorder.events)
        out = apply_operation(w, ("ENCLOSE", g.name, None))
        assert out.reason is None and not out.acted
        assert apply_operation(w, ("ENCLOSE", "gnoth9", ())).reason is None
        assert len(recorder.events) == before and brackets(w) == ["[1 2]"]

    def test_enclose_refuses_a_gap_and_a_nested_pair(self):
        """The encloselist must be "neighboring gloms within the gnoth" [M86 p. 112]."""
        w, _, _ = still_world(1, 2, 3)
        glom1 = glom_over(w, "glint1", "glint2")
        g = gnoth_over(w, glom1.name, "glint3")
        assert apply_operation(w, ("ENCLOSE", g.name, ("glint1", "glint3"))).reason
        assert apply_operation(w, ("ENCLOSE", g.name, (glom1.name, "glint1"))).reason
        assert brackets(w) == ["[(1 2) 3]"]


class TestFracture:

    def test_fracture_example(self):
        """`(FRACTURE gnoth2)`: `[(1 2) ((3 3) (4 4))]` becomes `[1 2 (3 3) (4 4)]`, one
        level dissolved [M86 pp. 113, 117]."""
        w, _, _ = still_world(1, 2, 3, 3, 4, 4)
        glom1 = glom_over(w, "glint1", "glint2")
        glom4 = glom_over(w, "glint3", "glint4")
        glom3 = glom_over(w, "glint5", "glint6")
        glom5 = glom_over(w, glom4.name, glom3.name)
        g = gnoth_over(w, glom1.name, glom5.name)
        out = apply_operation(w, ("FRACTURE", g.name))
        assert out.reason is None and out.gnoths == (g.name,)
        assert brackets(w) == ["[1 2 (3 3) (4 4)]"]
        assert tops(g) == ["glint1", "glint2", glom4.name, glom3.name]
        assert gone(w, glom1.name, glom5.name)

    def test_fracture_of_bare_glints_changes_nothing_but_the_cap(self):
        w, _, _ = still_world(1, 2)
        g = gnoth_over(w, "glint1", "glint2")
        old = g.glom.name
        assert apply_operation(w, ("FRACTURE", g.name)).reason is None
        assert brackets(w) == ["[1 2]"] and g.glom.name != old


class TestMerge:

    def test_simple_merge(self):
        """`(MERGE gnoth1 (glom1 glom2))`: `[(1 1) (1 1 1)]` becomes `[(1 1 1 1 1)]`, one
        `merge`-type glom of the subgloms [M86 pp. 113, 118]."""
        w, _, _ = still_world(1, 1, 1, 1, 1)
        glom1 = glom_over(w, "glint1", "glint2")
        glom2 = glom_over(w, "glint3", "glint4", "glint5")
        g = gnoth_over(w, glom1.name, glom2.name)
        out = apply_operation(w, ("MERGE", g.name, (glom1.name, glom2.name)))
        assert out.reason is None
        assert brackets(w) == ["[(1 1 1 1 1)]"]
        glom3 = w.find(out.made[0])
        assert glom3.type == ("merge",) and subs(w, glom3.name) == [f"glint{i}" for i in range(1, 6)]
        assert gone(w, glom1.name, glom2.name)

    def test_nested_merge(self):
        """`(MERGE gnoth1 (glom4 glom7))` on `[((1 2 3) (3 4)) (((5 6) (4 5)) (6 7 8))]`:
        glom19 is burst to free glom7, glom10 rises, and the merged glom holds the
        subgloms of glom4 and glom7: `[((1 2 3) (3 4) (5 6) (4 5)) (6 7 8)]`
        [M86 pp. 118-119]."""
        w, _, _ = still_world(1, 2, 3, 3, 4, 5, 6, 4, 5, 6, 7, 8)
        glom1 = glom_over(w, "glint1", "glint2", "glint3")
        glom2 = glom_over(w, "glint4", "glint5")
        glom4 = glom_over(w, glom1.name, glom2.name)
        glom5 = glom_over(w, "glint6", "glint7")
        glom6 = glom_over(w, "glint8", "glint9")
        glom7 = glom_over(w, glom5.name, glom6.name)
        glom10 = glom_over(w, "glint10", "glint11", "glint12")
        glom19 = glom_over(w, glom7.name, glom10.name)
        g = gnoth_over(w, glom4.name, glom19.name)
        assert bracket(g) == "[((1 2 3) (3 4)) (((5 6) (4 5)) (6 7 8))]"
        out = apply_operation(w, ("MERGE", g.name, (glom4.name, glom7.name)))
        assert out.reason is None
        assert brackets(w) == ["[((1 2 3) (3 4) (5 6) (4 5)) (6 7 8)]"]
        glom20 = w.find(out.made[0])
        assert tops(g) == [glom20.name, glom10.name]
        assert subs(w, glom20.name) == [glom1.name, glom2.name, glom5.name, glom6.name]
        assert gone(w, glom4.name, glom7.name, glom19.name)

    def test_merge_keeps_a_listed_glint(self):
        """A glint in the glomlist is kept as it is (it has no subgloms) (DECISION D-066)."""
        w, _, _ = still_world(1, 1, 2)
        glom1 = glom_over(w, "glint1", "glint2")
        g = gnoth_over(w, glom1.name, "glint3")
        out = apply_operation(w, ("MERGE", g.name, (glom1.name, "glint3")))
        assert out.reason is None and brackets(w) == ["[(1 1 2)]"]


class TestUncoverAndContiguity:

    def test_uncover_dissolves_the_containers_outermost_first(self):
        """"all gloms containing it must be destroyed so that it can rise to the top"
        [M86 p. 109]; siblings survive."""
        w, _, _ = still_world(1, 2, 3, 4)
        inner = glom_over(w, "glint1", "glint2")
        side = glom_over(w, "glint3", "glint4")
        outer = glom_over(w, inner.name, side.name)
        dissolved = uncover(w, w.find("glint1"))
        assert [x.name for x in dissolved] == [outer.name, inner.name]
        assert gone(w, outer.name, inner.name) and w.find(side.name) is side
        assert w.find("glint1").top_level and side.top_level

    def test_contiguous(self):
        w, _, _ = still_world(1, 2, 3, 4)
        a = glom_over(w, "glint1", "glint2")
        assert contiguous([a, w.find("glint3")])
        assert not contiguous([a, w.find("glint4")])
        assert not contiguous([a, w.find("glint1")])
        assert not contiguous([])


# ---- the two series [M86 pp. 119-121] -----------------------------------------------------------

class TestSeries:

    def test_series_1(self):
        """The first series on `1 2 3 3 4 3 4 5` [M86 pp. 119-120], step by step: the
        parenthesization after each operation as the page prints it, ending
        `[(1 2) 3] [(3 4) 3] [(4 5)]`."""
        w, _, _ = still_world(1, 2, 3, 3, 4, 3, 4, 5)
        glom1 = glom_over(w, "glint1", "glint2", "glint3")
        glom2 = glom_over(w, "glint4", "glint5")
        glom3 = glom_over(w, "glint6", "glint7")
        glom4 = glom_over(w, glom2.name, glom3.name)
        g1 = gnoth_over(w, glom1.name, frame=0)
        g2 = gnoth_over(w, glom4.name, frame=1)
        g3 = gnoth_over(w, "glint8", frame=2)
        assert brackets(w) == ["[(1 2 3)]", "[((3 4) (3 4))]", "[5]"]
        steps = [
            (("SHIFT-LEFT", g2.name, g3.name, "glint8"), ["[(1 2 3)]", "[((3 4) (3 4)) 5]"]),
            (("CAPTURE-RIGHT", g2.name, glom3.name, "glint8"), ["[(1 2 3)]", "[(3 4) (3 4 5)]"]),
            (("FRACTURE", g1.name), ["[1 2 3]", "[(3 4) (3 4 5)]"]),
            (("ENCLOSE", g1.name, ("glint1", "glint2")), ["[(1 2) 3]", "[(3 4) (3 4 5)]"]),
        ]
        for operation, expected in steps:
            out = apply_operation(w, operation)
            assert out.reason is None, operation
            assert brackets(w) == expected, operation
        glom5 = w.find(gnoths.gnoths(w)[1].collection[1].name)
        assert glom5.print_value == (3, 4, 5) and glom5.name != glom3.name
        assert gone(w, glom4.name) and g3.name not in w.socratoplasm
        out = apply_operation(w, ("SPLIT", g2.name, (glom5.name,)))
        assert out.reason is None
        assert brackets(w) == ["[(1 2) 3]", "[(3 4)]", "[(3 4 5)]"]
        new = w.find(out.created[0])
        out = apply_operation(w, ("SHIFT-LEFT", g2.name, new.name, ("glint6",)))
        assert out.reason is None
        assert brackets(w) == ["[(1 2) 3]", "[(3 4) 3]", "[4 5]"]
        out = apply_operation(w, ("ENCLOSE", new.name, ("glint7", "glint8")))
        assert out.reason is None
        assert brackets(w) == ["[(1 2) 3]", "[(3 4) 3]", "[(4 5)]"]

    def test_series_2(self):
        """The second series on `1 2 3 3 4 5 6 4 5 6 7 8` [M86 p. 121], step by step,
        ending `[(1 2 3)] [(3 4 5 6)] [(4 5 6 7 8)]`."""
        w, _, _ = still_world(1, 2, 3, 3, 4, 5, 6, 4, 5, 6, 7, 8)
        glom1 = glom_over(w, "glint1", "glint2", "glint3")
        glom2 = glom_over(w, "glint4", "glint5")
        glom3 = glom_over(w, "glint6", "glint7")
        glom4 = glom_over(w, "glint8", "glint9")
        glom5 = glom_over(w, "glint10", "glint11", "glint12")
        g1 = gnoth_over(w, glom1.name, frame=0)
        g2 = gnoth_over(w, glom2.name, glom3.name, glom4.name, frame=1)
        g3 = gnoth_over(w, glom5.name, frame=2)
        assert brackets(w) == ["[(1 2 3)]", "[(3 4) (5 6) (4 5)]", "[(6 7 8)]"]
        out = apply_operation(w, ("SHIFT-RIGHT", g2.name, g3.name, (glom4.name,)))
        assert out.reason is None
        assert brackets(w) == ["[(1 2 3)]", "[(3 4) (5 6)]", "[(4 5) (6 7 8)]"]
        out = apply_operation(w, ("MERGE", g3.name, (glom4.name, glom5.name)))
        assert out.reason is None
        assert brackets(w) == ["[(1 2 3)]", "[(3 4) (5 6)]", "[(4 5 6 7 8)]"]
        glom6 = w.find(out.made[0])
        assert tops(g3) == [glom6.name] and glom6.type == ("merge",)
        out = apply_operation(w, ("FRACTURE", g2.name))
        assert out.reason is None
        assert brackets(w) == ["[(1 2 3)]", "[3 4 5 6]", "[(4 5 6 7 8)]"]
        out = apply_operation(w, ("ENCLOSE", g2.name, ("glint4", "glint5", "glint6", "glint7")))
        assert out.reason is None
        assert brackets(w) == ["[(1 2 3)]", "[(3 4 5 6)]", "[(4 5 6 7 8)]"]
        assert tops(g2) == [out.made[0]] and g1.collection == (glom1,)


# ---- PROGRAMs [M86 p. 122] ------------------------------------------------------------------------

def figure_1(world):
    """Figure 1's gnoths over `1 2 1 2 3`: `((1 2)) ((1 2)) (3)` [M86 pp. 101, 122]."""
    glom1 = glom_over(world, "glint1", "glint2", type=("Succ", "print-value", "group"))
    glom2 = glom_over(world, "glint3", "glint4", type=("Succ", "print-value", "group"))
    g1 = gnoth_over(world, glom1.name, frame=0)
    g2 = gnoth_over(world, glom2.name, frame=1)
    g3 = gnoth_over(world, "glint5", frame=2)
    return (g1, g2, g3), (glom1, glom2)


def figure_2(world):
    """Figure 2's two gnoths [M86 p. 106], built over twelve glints: gnoth3 = glom14 glom15,
    glom15 = glom9 glom10, glom10 = glom5 glom6 glom7, glom7 = glint3 glint4 (here glints 5
    and 6); gnoth4 = glom8 glom17, glom8 = glom3 glom4 glom2, glom3 = glom1, glom1 = glint5
    glint6 (here 7 and 8), glom4 = glint7 glint8 (here 9 and 10). The gloms the figure
    leaves unexpanded cover one glint each. Returns the gloms by the figure's names."""
    f = {}
    f["glom14"] = glom_over(world, "glint1")
    f["glom9"] = glom_over(world, "glint2")
    f["glom5"] = glom_over(world, "glint3")
    f["glom6"] = glom_over(world, "glint4")
    f["glom7"] = glom_over(world, "glint5", "glint6")
    f["glom10"] = glom_over(world, f["glom5"].name, f["glom6"].name, f["glom7"].name)
    f["glom15"] = glom_over(world, f["glom9"].name, f["glom10"].name)
    f["glom1"] = glom_over(world, "glint7", "glint8")
    f["glom3"] = glom_over(world, f["glom1"].name)
    f["glom4"] = glom_over(world, "glint9", "glint10")
    f["glom2"] = glom_over(world, "glint11")
    f["glom8"] = glom_over(world, f["glom3"].name, f["glom4"].name, f["glom2"].name)
    f["glom17"] = glom_over(world, "glint12")
    g3 = gnoth_over(world, f["glom14"].name, f["glom15"].name, frame=0)
    g4 = gnoth_over(world, f["glom8"].name, f["glom17"].name, frame=1)
    return (g3, g4), f


class TestPrograms:

    def test_the_real_move_of_p122(self):
        """"In the case of '1 2 1 2 3' [Figure 1], the 2 <--> 3 pull is resolved via"
        the PROGRAM of p. 122, whose states after each step are the page's own:
        `((1 2)) ((1 2) 3)`, then `((1 2)) ((1 2 3))`, with no repair necessary
        [M86 p. 122]."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2) = figure_1(w)
        assert brackets(w) == ["[(1 2)]", "[(1 2)]", "[3]"]
        program = ("PROGRAM", (("SHIFT-LEFT", g2.name, g3.name, ("glint5",)),
                               ("CAPTURE-RIGHT", g2.name, glom2.name, "glint5"),
                               ("ENCLOSE", g2.name, None),
                               ("ENCLOSE", g3.name, None)))
        outcomes = run_program(w, program)
        assert [o.reason for o in outcomes] == [None] * 4
        assert brackets(w) == ["[(1 2)]", "[(1 2 3)]"]
        assert outcomes[0].gone == (g3.name,) and g3.name not in w.socratoplasm
        assert gone(w, glom2.name) and w.find(outcomes[1].made[0]).print_value == (1, 2, 3)
        assert not outcomes[2].acted and not outcomes[3].acted

    def test_the_high_level_move_of_p122(self):
        """"if glom8 is to be pulled away from gnoth4 by glom15, this high-level attraction
        is translated into the move: (SHIFT-LEFT gnoth3 gnoth4 (glom8))" [M86 p. 122]: one
        operation, glom8 whole under gnoth3."""
        w, _, _ = still_world(*range(1, 13))
        (g3, g4), f = figure_2(w)
        out = apply_operation(w, ("SHIFT-LEFT", g3.name, g4.name, (f["glom8"].name,)))
        assert out.reason is None
        assert tops(g3) == [f["glom14"].name, f["glom15"].name, f["glom8"].name]
        assert tops(g4) == [f["glom17"].name]
        assert w.find(f["glom8"].name) is f["glom8"] and subs(w, f["glom8"].name) == \
            [f["glom3"].name, f["glom4"].name, f["glom2"].name]

    def test_the_deeper_move_of_p122(self):
        """"If, however, glom1 is to be pulled away by glom7", the PROGRAM of p. 122: shift
        glom1 over, let glom10 swallow it, then re-enclose the siblings of glom1 (replacing
        glom3), the new glom3 with the siblings of glom3 (replacing glom8) and the
        siblings of glom10 with the new glom10 (replacing glom15) [M86 pp. 122-123]
        (DECISION D-044, with `New` for the thesis's `newglom3`, `newglom10`)."""
        w, _, _ = still_world(*range(1, 13))
        (g3, g4), f = figure_2(w)
        n = {k: v.name for k, v in f.items()}
        program = ("PROGRAM", (
            ("SHIFT-LEFT", g3.name, g4.name, (n["glom1"],)),
            ("CAPTURE-RIGHT", g3.name, n["glom10"], n["glom1"]),
            ("ENCLOSE", g4.name, ()),                               # siblings of glom1: none
            ("ENCLOSE", g4.name, (n["glom4"], n["glom2"])),         # newglom3 & sibs of glom3
            ("ENCLOSE", g3.name, (n["glom9"], New(1))),             # sibs of glom10 & newglom10
        ))
        outcomes = run_program(w, program)
        assert [o.reason for o in outcomes] == [None] * 5
        # the right gnoth: glom8 and glom3 were burst for glom1 to leave; glom4 and glom2
        # were re-enclosed into the new glom8
        new8 = w.find(outcomes[3].made[0])
        assert tops(g4) == [new8.name, n["glom17"]]
        assert subs(w, new8.name) == [n["glom4"], n["glom2"]]
        assert gone(w, n["glom8"], n["glom3"])
        # the left gnoth: glom15 and glom10 were burst for the capture; the new glom10 holds
        # glom5 glom6 glom7 glom1; the new glom15 holds glom9 and the new glom10
        new10 = w.find(outcomes[1].made[0])
        new15 = w.find(outcomes[4].made[0])
        assert subs(w, new10.name) == [n["glom5"], n["glom6"], n["glom7"], n["glom1"]]
        assert subs(w, new15.name) == [n["glom9"], new10.name]
        assert tops(g3) == [n["glom14"], new15.name]
        assert gone(w, n["glom15"], n["glom10"])
        assert g3.range == (1, 8) and g4.range == (9, 12)
        assert brackets(w) == ["[(1) ((2) ((3) (4) (5 6) (7 8)))]", "[((9 10) (11)) (12)]"]


# ---- the Gnoth-operator task [M86 p. 124] ------------------------------------------------------

def post_and_run(world, program):
    """Post a Gnoth-operator with the program and run it alone."""
    task = world.post("Gnoth-operator", program)
    world.taskrack.clean(("Gnoth-operator",))
    world.run_task(task)
    return task


class TestGnothOperator:

    def test_the_transcripts_first_program_on_a_run(self):
        """`1 2` then `1` run to the Gnoth-setter, then the Gnoth-operator with the
        transcript's PROGRAM: `doing task Gnoth-operator on ((PROGRAM ((SHIFT-RIGHT gnoth1
        gnoth2 (glint1)) (ENCLOSE gnoth1 nil))))` and `top-down glom glomN`; afterwards
        `show-parse` prints `((1 2) (1))` and gnoth1 is gone [M86 p. 174]; the threshold,
        set by the Reformulator, goes down by 1 [M86 p. 124]."""
        world, recorder, trace = full_run([1, 2, 1], seed=3)
        assert [g.terms for g in gnoths.gnoths(world)] == [(1,), (2,), (1,)]
        assert world.reigning == "S-group"
        world.threshold = 8.0
        program = ("PROGRAM", (("SHIFT-RIGHT", "gnoth1", "gnoth2", ("glint1",)),
                               ("ENCLOSE", "gnoth1", None)))
        post_and_run(world, program)
        line = ("doing task Gnoth-operator on ((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) "
                "(ENCLOSE gnoth1 nil))))")
        i = trace.lines.index(line)
        assert trace.lines[i + 1].startswith("top-down glom glom")
        assert gnoths.parse_text(world) == "((1 2) (1))"
        listed = gnoths.gnoths(world)
        assert [g.name for g in listed] == ["gnoth2", "gnoth3"]
        assert listed[0].range == (1, 2) and listed[0].frame == 1
        assert listed[0].glom.name == trace.lines[i + 1].split()[-1]
        assert world.threshold == 7.0
        operated = recorder.of_kind("operated")[-1]
        assert (operated.steps, operated.acted, operated.stopped) == (2, True, None)
        assert operated.program == operation_text(program)

    def test_the_transcripts_second_move_on_the_state_of_p176(self):
        """The bare `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` on the gnoths `(1 2) (1 2) (3)`
        of p. 176 leaves gnoth2 over `(1 2)` and gnoth5 capped by a pseudo-glom over
        `(1 2 3)`, terms 3 to 5, with gnoth3 gone [M86 pp. 177-179] (DECISION D-065)."""
        w, recorder, trace = still_world(1, 2, 1, 2, 3)
        g2 = gnoth_over(w, "glint1", "glint2", frame=1)
        g3 = gnoth_over(w, "glint3", "glint4", frame=1)
        g5 = gnoth_over(w, "glint5", frame=2)
        w.reigning = "S-group"
        post_and_run(w, ("SHIFT-RIGHT", g3.name, g5.name, ("glint3",)))
        assert trace.lines[-2] == f"doing task Gnoth-operator on ((SHIFT-RIGHT {g3.name} {g5.name} (glint3)))"
        assert trace.lines[-1] == f"top-down glom {g5.glom.name}"
        assert [g.name for g in gnoths.gnoths(w)] == [g2.name, g5.name]
        assert g5.glom.print_value == (1, 2, 3) and g5.range == (3, 5) and g5.frame == 2
        assert g5.glom.pseudo and g2.range == (1, 2)
        assert gnoths.parse_text(w) == "((1 2) (1 2 3))"

    def test_bonds_are_reassessed_after_the_move(self):
        """After the real move of p. 122 the bond between the 2 and the 3 is internal to
        gnoth2 where it was external [M86 p. 100] (DECISION D-067)."""
        w, _, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2) = figure_1(w)
        e = bonds.make_bond(w, "Succ", "print-value", "adjacent", w.find("glint4"), w.find("glint5"))
        w.reigning = "S-group"
        slipping.assess_bonds(w, "S-group")
        assert g2.external_bonds == [(e, 10)] and g2.internal_bonds == []
        post_and_run(w, ("PROGRAM", (("SHIFT-LEFT", g2.name, g3.name, ("glint5",)),
                                     ("CAPTURE-RIGHT", g2.name, glom2.name, "glint5"))))
        assert g2.internal_bonds == [(e, 10)] and g2.external_bonds == []

    def test_stale_when_a_structure_is_gone(self):
        """"If the relevant structures do still exist, the Gnoth-operator carries out the
        operations; if not, it will simply terminate" [M86 p. 124]: a gnoth or glom gone,
        a bad reference, a malformed step; nothing changes and the threshold stays."""
        w, recorder, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2) = figure_1(w)
        w.threshold = 8.0
        cases = [
            ("SHIFT-LEFT", g2.name, "gnoth9", ("glint5",)),
            ("SHIFT-LEFT", g2.name, g3.name, ("glom99",)),
            ("PROGRAM", (("ENCLOSE", g2.name, (New(0),)),)),
            ("PROGRAM", (("SHIFT-LEFT", g2.name, g3.name, ("glint5",)),
                         ("CAPTURE-RIGHT", g2.name, glom2.name, New(3)))),
            ("CAPTURE-RIGHT", g2.name, glom2.name),
            ("DANCE", g2.name),
        ]
        for program in cases:
            post_and_run(w, program)
            assert recorder.of_kind("stale")[-1].type == "Gnoth-operator", program
        assert brackets(w) == ["[(1 2)]", "[(1 2)]", "[3]"] and w.threshold == 8.0
        assert not recorder.of_kind("operated")
        post_and_run(w, None)
        assert recorder.of_kind("stale")[-1].reason == "no operation"

    def test_a_program_stops_at_a_step_that_cannot_be_done(self):
        """A step that fails its own checks stops the PROGRAM; what was done before stays
        and counts as acting (DECISION D-067)."""
        w, recorder, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2) = figure_1(w)
        w.threshold = 8.0
        post_and_run(w, ("PROGRAM", (("SHIFT-LEFT", g2.name, g3.name, ("glint5",)),
                                     ("SHIFT-LEFT", g2.name, g3.name, ("glint5",)),
                                     ("FRACTURE", g1.name))))
        assert brackets(w) == ["[(1 2)]", "[(1 2) 3]"]
        operated = recorder.of_kind("operated")[-1]
        assert operated.steps == 2 and operated.acted and "gone" in operated.stopped
        assert w.threshold == 7.0

    def test_a_program_of_nothing_leaves_the_threshold_alone(self):
        """Only "When a Gnoth-operator does in fact operate" is the threshold decreased
        [M86 p. 124]; and a threshold no Reformulator has set stays unset (DECISION
        D-046)."""
        w, recorder, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), _ = figure_1(w)
        w.threshold = 8.0
        post_and_run(w, ("PROGRAM", (("ENCLOSE", g1.name, None), ("ENCLOSE", g2.name, ()))))
        assert w.threshold == 8.0 and not recorder.of_kind("operated")[-1].acted
        w.threshold = None
        post_and_run(w, ("FRACTURE", g1.name))
        assert w.threshold is None and recorder.of_kind("operated")[-1].acted

    def test_plato_scouts_on_the_new_gloms(self):
        """"Plato-scout tasks are placed on the taskrack to peruse the gnoth's
        newly-created gloms" [M86 pp. 109-110]: one per glom an operation makes, for all
        six classes (DECISION D-044)."""
        w, recorder, _ = still_world(1, 2, 1, 2, 3)
        (g1, g2, g3), (glom1, glom2) = figure_1(w)
        post_and_run(w, ("PROGRAM", (("SHIFT-LEFT", g2.name, g3.name, ("glint5",)),
                                     ("CAPTURE-RIGHT", g2.name, glom2.name, "glint5"),
                                     ("ENCLOSE", g1.name, ("glint1", "glint2")))))
        scouts = [e for e in recorder.of_kind("posted") if e.type == "Plato-scout"]
        made = [e.name for e in recorder.of_kind("operation-glom")]
        assert [s.args[1] for s in scouts] == made and len(made) == 2
        assert all(len(s.args[0]) == 6 for s in scouts)
        assert w.taskrack.count_by_type()["Plato-scout"] == 2

    def test_same_seed_same_trace(self):
        """Nothing here draws from the `Rng`: the transcript scenario is reproducible."""
        traces = []
        for _ in range(2):
            world, _, trace = full_run([1, 2, 1], seed=3)
            world.threshold = 8.0
            post_and_run(world, ("PROGRAM", (("SHIFT-RIGHT", "gnoth1", "gnoth2", ("glint1",)),
                                             ("ENCLOSE", "gnoth1", None))))
            traces.append(trace.lines)
        assert traces[0] == traces[1]

    def test_precheck_passes_a_program_whose_later_step_names_a_gnoth_the_first_empties(self):
        """`(ENCLOSE gnoth1 nil)` after the shift that empties gnoth1 passes, as the
        transcript's PROGRAM did [M86 p. 174] (DECISION D-067)."""
        w, _, _ = still_world(1, 2, 1)
        g1 = gnoth_over(w, "glint1", frame=0)
        g2 = gnoth_over(w, "glint2", frame=1)
        gnoth_over(w, "glint3", frame=2)
        steps = [("SHIFT-RIGHT", g1.name, g2.name, ("glint1",)), ("ENCLOSE", g1.name, None)]
        assert precheck(w, steps) is None
        assert precheck(w, [("ENCLOSE", "gnoth7", ("glint1",))]) == "gnoth7 is gone"
        assert precheck(w, [("ENCLOSE", g1.name, (New(0),))]) == "(new 0) does not refer to an earlier step"


# ---- NO-OP and the Capper [M86 p. 113] -----------------------------------------------------------

class TestNoOpAndCapper:

    def test_no_op_uncaps_and_sets_out_a_capper(self):
        """NO-OP "causes the gnoth to be 'uncapped' -- have its pseudo-glom suspended --
        and remain that way until the Capper task it sets out is invoked" [M86 p. 113]."""
        w, recorder, _ = still_world(1, 2, 3)
        g = gnoth_over(w, "glint1", "glint2", "glint3")
        cap = g.glom.name
        out = apply_operation(w, ("NO-OP", g.name))
        assert out.reason is None and out.uncapped == (g.name,) and out.acted
        assert g.glom is None and gone(w, cap) and g.range == (1, 3)
        assert all(w.find(f"glint{i}").top_level for i in (1, 2, 3))
        posted = recorder.of_kind("posted")[-1]
        assert (posted.type, posted.args) == ("Capper", (g.name, (1, 3)))
        assert recorder.of_kind("gnoth-uncapped")[-1].range == (1, 3)
        assert "already" in apply_operation(w, ("NO-OP", g.name)).reason

    def test_capper_recaps_what_glommed_meanwhile(self):
        """"The Capper finds all current gloms whose ranges overlap with the original range
        of the gnoth (before it was uncapped) and claims them for the gnoth" [M86 p. 113]:
        a glom formed inside the uncapped gnoth is claimed."""
        w, recorder, trace = still_world(1, 1, 2)
        g = gnoth_over(w, "glint1", "glint2", "glint3")
        apply_operation(w, ("NO-OP", g.name))
        new = glom_over(w, "glint1", "glint2")
        capper(w, g.name, (1, 3))
        assert g.glom is not None and tops(g) == [new.name, "glint3"] and g.range == (1, 3)
        assert trace.lines[-1] == f"top-down glom {g.glom.name}"
        capped = recorder.of_kind("gnoth-capped")[-1]
        assert (capped.name, capped.range, capped.claimed) == (g.name, (1, 3), (new.name, "glint3"))

    def test_the_gnoth_that_recaps_first_gets_the_shared_glom_and_the_extended_range(self):
        """"Should two different gnoths claim the same glom -- one formed, perhaps, by
        combining gloms from the two gnoths -- the gnoth that recaps first will get the
        glom and the extended range" [M86 p. 113] (DECISION D-045)."""
        w, _, _ = still_world(1, 2, 2, 3)
        left = gnoth_over(w, "glint1", "glint2", frame=0)
        right = gnoth_over(w, "glint3", "glint4", frame=1)
        apply_operation(w, ("NO-OP", left.name))
        apply_operation(w, ("NO-OP", right.name))
        across = glom_over(w, "glint2", "glint3")
        capper(w, right.name, (3, 4))
        assert tops(right) == [across.name, "glint4"] and right.range == (2, 4)
        capper(w, left.name, (1, 2))
        assert tops(left) == ["glint1"] and left.range == (1, 1)
        assert brackets(w) == ["[1]", "[(2 2) 3]"]

    def test_capper_with_nothing_left_destroys_the_gnoth(self):
        """A gnoth whose whole range was claimed by its neighbour disappears [M86 p. 108]."""
        w, recorder, _ = still_world(1, 2, 2)
        left = gnoth_over(w, "glint1", frame=0)
        right = gnoth_over(w, "glint2", "glint3", frame=1)
        apply_operation(w, ("NO-OP", left.name))
        apply_operation(w, ("NO-OP", right.name))
        across = glom_over(w, "glint1", "glint2", "glint3")
        capper(w, right.name, (2, 3))
        assert tops(right) == [across.name] and right.range == (1, 3)
        capper(w, left.name, (1, 1))
        assert left.name not in w.socratoplasm
        assert recorder.of_kind("gnoth-gone")[-1].name == left.name

    def test_capper_is_stale_on_a_capped_or_gone_gnoth(self):
        w, recorder, _ = still_world(1, 2)
        g = gnoth_over(w, "glint1", "glint2")
        capper(w, g.name, (1, 2))
        assert recorder.of_kind("stale")[-1].reason == f"{g.name} is capped already"
        capper(w, "gnoth9", (1, 2))
        assert recorder.of_kind("stale")[-1].reason == "gnoth9 is gone"

    def test_capper_runs_as_a_task(self):
        """The Capper is a task of the rack (urgency 2, DECISION D-008), run by the loop."""
        w, recorder, _ = still_world(1, 2)
        g = gnoth_over(w, "glint1", "glint2")
        apply_operation(w, ("NO-OP", g.name))
        assert w.taskrack.count_by_type()["Capper"] == 1
        run_until_prompt(w)
        assert g.glom is not None and tops(g) == ["glint1", "glint2"]
        assert recorder.of_kind("chosen")[-2].type == "Capper"


class TestOutcome:

    def test_acted(self):
        assert not Outcome(("ENCLOSE", "gnoth1", None)).acted
        assert Outcome(("FRACTURE", "gnoth1"), gnoths=("gnoth1",)).acted
        assert not Outcome(("FRACTURE", "gnoth1"), gnoths=("gnoth1",), reason="gone").acted


# ---- the `groups` field after an operation [M86 pp. 175, 179] (item 21) ------------------------

def test_an_operation_sets_groups_on_the_gnoth_it_recaps():
    """After the PROGRAM of p. 174 `show` prints `groups: nil` for gnoth2, the gnoth that
    received glint1, and no `groups` line for gnoth3, which no operation touched
    [M86 p. 175]; after the shift of p. 177, gnoth5 prints `groups: nil` too [M86 p. 179]
    (DECISION D-066 as amended in item 21): a recapped gnoth's `groups` is the list of the
    gloms operations made under it, empty for a shift."""
    world, recorder, trace = full_run([1, 2, 1], seed=3)
    world.threshold = 8.0
    post_and_run(world, ("SHIFT-RIGHT", "gnoth1", "gnoth2", ("glint1",)))
    g2, g3 = gnoths.gnoths(world)
    assert (g2.name, g2.groups) == ("gnoth2", [])
    assert "groups: nil" in g2.show_lines()
    assert g3.name == "gnoth3" and g3.groups is None
    assert not any(line.startswith("groups") for line in g3.show_lines())

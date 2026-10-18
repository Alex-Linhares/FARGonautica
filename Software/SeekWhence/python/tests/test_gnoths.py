"""Gnoths, the Gnoth-maker, the catchall, the three equivalence levels and the Hypothesizer
[M86 pp. 76-79, 94-98, 165-168, 170-171] (DECISIONS D-012, D-030, D-031, D-051, D-062)."""

import io
from collections import Counter

import pytest

from seekwhence import cytoplasm as cy
from seekwhence import gloms, gnoths
from seekwhence.events import Recorder
from seekwhence.forms import Form, parse
from seekwhence.gnoths import (Gnoth, deep_frame, equivalence_type, fallback_form,
                               form_from_template, generalise, parse_equivalent, sexp_text,
                               shallow_frame, structural_equivalent, term_equivalent, to_sexp)
from seekwhence.run import enter_term, new_world, run_until_prompt
from seekwhence.trace import Trace

FENCE_SEED = 4     # a seed on which `3 7 3` gloms the fence `(3 7)`, as runs 1 and 2 did


def run_all(world):
    while run_until_prompt(world) != "empty":
        pass


def run_to_prompt(world):
    """Run until the program asks for a term (a Call-term, or an empty rack)."""
    while run_until_prompt(world) == "term-cap":
        pass


def world_with(*terms, seed=1, scout=False, trace=None, config=None):
    """A world with the terms entered and the rack run empty after each; the Glom-scout is a
    no-op unless `scout`, so that the tests build the gloms; the gnoth tasks are live."""
    recorder = Recorder()
    observers = [recorder] if trace is None else [recorder, trace]
    world = new_world(config, seed=seed, observers=observers)
    if not scout:
        world.handlers["Glom-scout"] = lambda w, *args: None
    for term in terms:
        enter_term(world, term)
        run_all(world)
    return world, recorder


def full_run(terms, seed, trace=None):
    """A run from scratch, every task live, stopping at each prompt as the user would."""
    recorder = Recorder()
    observers = [recorder] if trace is None else [recorder, trace]
    world = new_world(seed=seed, observers=observers)
    for term in terms:
        enter_term(world, term)
        run_to_prompt(world)
    return world, recorder


def glom_over(world, *names, type=("Succ", "print-value", "group")):
    return cy.make_glom(world, type, [world.find(n) for n in names])


def lines(trace, prefix):
    return [l for l in trace.lines if l.startswith(prefix)]


def p78_world(trace=None):
    """The Gnoth-maker example: `1 1 2 1 2 3` with gloms `(1)`, `(1 2)`, `(1 2 3)` under the
    template `(S-group 1 n)` [M86 pp. 77-78], built as `test_templates.py` builds it, and
    then the Gnoth-maker and Hypothesizer run."""
    world, recorder = world_with(1, 1, 2, 1, 2, 3, trace=trace)
    glom = glom_over(world, "glint2", "glint3")
    gloms.dub(world, glom, "S-group", "pure")
    run_all(world)
    return world, recorder


# ---- the Gnoth-maker example [M86 pp. 77-78] --------------------------------------------------

class TestGnothMakerExample:

    def test_three_gnoths_with_frames_and_ranges(self):
        """"the Gnoth-maker would create three gnoths" over `(1)`, `(1 2)` and `(1 2 3)`,
        frames 1, 2, 3 and ranges `(1 1)`, `(2 3)`, `(4 6)` [M86 p. 78]; the program counts
        frames from 0 (`frame: 0` [M86 p. 168]; DECISION D-062)."""
        world, _ = p78_world()
        made = gnoths.gnoths(world)
        assert [g.name for g in made] == ["gnoth1", "gnoth2", "gnoth3"]
        assert [g.frame for g in made] == [0, 1, 2]
        assert [g.frame + 1 for g in made] == [1, 2, 3]
        assert [g.range for g in made] == [(1, 1), (2, 3), (4, 6)]
        assert [g.plato_class for g in made] == ["S-group"] * 3
        assert [g.terms for g in made] == [(1,), (1, 2), (1, 2, 3)]

    def test_each_gnoth_glom_has_the_unit_glom_as_subglom(self):
        """`glom: glom10 (where glom10 has glom2 as subglom)`, "the gnoth's 'pseudo-glom'"
        [M86 p. 78]: one pseudo-glom per gnoth, over exactly the unit."""
        world, _ = p78_world()
        for gnoth in gnoths.gnoths(world):
            assert gnoth.glom.pseudo and gnoth.glom.type == ("gnoth",)
            assert len(gnoth.glom.subgloms) == 1
            unit = gnoth.glom.subgloms[0]
            assert unit.parent is gnoth.glom
            assert gnoth.collection == (unit,)
            assert gnoth.glom.name in world.cytoplasm
        assert [g.glom.subgloms[0].print_value for g in gnoths.gnoths(world)] == [
            (1,), (1, 2), (1, 2, 3)]

    def test_the_template_glom_gives_way_to_the_gnoth_gloms(self):
        """`show` after the Gnoth-maker lists the engendering glom and the gnoth-glom over it
        but not the template glom [M86 pp. 167-168, 171] (DECISION D-062)."""
        trace = Trace(io.StringIO())
        world, _ = p78_world(trace=trace)
        template_glom = [l for l in lines(trace, "create-template-glom")]
        assert template_glom == ["create-template-glom (S-group 1 n)(glom2 glom1 glom3)"]
        assert "glom4" not in world.cytoplasm          # the template glom
        assert all(name in world.cytoplasm for name in ("glom1", "glom2", "glom3"))
        assert world.template.glom is None and world.template.state == "accepted"
        assert lines(trace, "top-down glom")[-3:] == [
            "top-down glom glom5", "top-down glom glom6", "top-down glom glom7"]
        assert "gnoths constructed" in trace.lines

    def test_the_gnoths_are_actualizations_of_the_class(self):
        """"notifies the associated plato-classes of their existence" [M86 p. 77]: each
        gnoth is on the class's actualization list [M86 p. 90]."""
        world, _ = p78_world()
        actualizations = world.platoplasm.type("S-group").actualizations
        assert actualizations == gnoths.gnoths(world)
        assert world.platoplasm.type("C-group").actualizations == []

    def test_the_gnoths_form_is_the_units_structure_field(self):
        """`form: (((Cycle 3 2 (3 7)) pure))` is the dubbed glom's structure field
        [M86 pp. 168-169] (DECISION D-062); here the units' own forms (DECISION D-061)."""
        world, _ = p78_world()
        assert [dict(g.show_fields())["form"] for g in gnoths.gnoths(world)] == [
            "(((S-group 1 1) pure))", "(((S-group 1 2) pure))", "(((S-group 1 3) pure))"]

    def test_the_gnoth_maker_makes_no_catchall(self):
        """The catchall is made by the Hfilter at the first confirming term, not by the
        Gnoth-maker (DECISIONS D-030, D-062, D-063: so that it is numbered after the gnoths
        of the parse, `gnoth4` in run 3 [M86 pp. 173, 176])."""
        world, _ = p78_world()
        assert gnoths.catchall(world) is None

    def test_the_hypothesis_generalises_the_wildcard(self):
        """The Hypothesizer devises the hypothesis "based on the template and the existing
        gnoths" [M86 p. 79]: `(S-group 1 n)` over units of length 1, 2, 3 gives
        `(S-group 1 (Countup 1))`, the thesis's form for `1 1 2 1 2 3 ...` [M86 p. 59]
        (DECISION D-031)."""
        world, _ = p78_world()
        assert world.hypothesis.show_form() == "(S-group 1 (Countup 1))"
        assert world.hypothesis.next_term() == 1
        assert [world.hypothesis.next_term() for _ in range(3)] == [2, 3, 4]


# ---- the gnoth's fields and `show` [M86 pp. 78, 168, 175] -------------------------------------

class TestShow:

    def test_the_nine_fields_in_order(self):
        """`show` prints class, name, frame, plato-class, glom, notes, form, state, range
        [M86 p. 168]."""
        world, _ = p78_world()
        gnoth = gnoths.gnoths(world)[1]
        assert gnoth.show_lines() == [
            "class: Gnoths", "name: gnoth2", "frame: 1", "plato-class: S-group",
            "glom: glom6", "notes: nil", "form: (((S-group 1 2) pure))", "state: stable",
            "range: (2 3)"]

    def test_the_reformulation_fields_come_first_when_set(self):
        """After a reformulation `show` prints external-bonds, internal-bonds,
        equivalence-type and groups before the nine fields [M86 p. 175]: p. 175's gnoth2
        with its fields set by hand."""
        gnoth = Gnoth("gnoth2", 1, "S-group", None, form=parse("(S-group 1 2)"), range=(1, 2))
        gnoth.external_bonds = [("bond2", 0), ("bond3", -5)]
        gnoth.internal_bonds = [("bond1", 10)]
        gnoth.equivalence_type = "parse"
        gnoth.groups = []
        assert gnoth.show_lines() == [
            "external-bonds: ((bond2 0) (bond3 -5))", "internal-bonds: ((bond1 10))",
            "equivalence-type: parse", "groups: nil", "class: Gnoths", "name: gnoth2",
            "frame: 1", "plato-class: S-group", "glom: nil", "notes: nil",
            "form: (S-group 1 2)", "state: stable", "range: (1 2)"]

    def test_p175_gnoth3_prints_bonds_but_no_equivalence_or_groups(self):
        """p. 175's gnoth3 prints `external-bonds` and `internal-bonds: nil` and then the
        nine fields, with no equivalence-type or groups line [M86 p. 175]: fields left
        None are not printed."""
        gnoth = Gnoth("gnoth3", 2, "S-group", None, form=parse("(S-group 1 1)"), range=(3, 3))
        gnoth.external_bonds = [("bond2", 0), ("bond3", -5)]
        gnoth.internal_bonds = []
        assert gnoth.show_lines()[:3] == [
            "external-bonds: ((bond2 0) (bond3 -5))", "internal-bonds: nil", "class: Gnoths"]
        assert not any(l.startswith(("equivalence-type", "groups")) for l in gnoth.show_lines())

    def test_bond_values_print_as_integers_when_whole(self):
        gnoth = Gnoth("gnoth9", 0, "S-group", None)
        gnoth.external_bonds = [("bond5", -5.0), ("bond6", 0.0)]
        assert gnoth.show_lines()[0] == "external-bonds: ((bond5 -5) (bond6 0))"

    def test_the_gnoths_block(self):
        """`gnoths:`, a blank line, then each gnoth's fields, gnoths separated by a blank
        line [M86 pp. 168, 179]; the catchall is not listed (DECISION D-030)."""
        world, _ = p78_world()
        block = gnoths.show_gnoths_lines(world)
        assert block[:3] == ["gnoths:", "", "class: Gnoths"]
        assert block.count("class: Gnoths") == 3
        assert block.count("") == 3
        assert "name: gnoth4" not in block       # the catchall

    def test_show_parse(self):
        """`show-parse` prints the gnoths' parse-prints in one list, `((1 2) (1))`
        [M86 p. 174] (DECISION D-051)."""
        world, _ = world_with(1, 2, 1)
        # After a reformulation the gnoth-gloms stand directly over glints [M86 p. 175].
        gnoths.make_gnoth(world, "S-group", [world.find("glint1"), world.find("glint2")], 0)
        gnoths.make_gnoth(world, "S-group", [world.find("glint3")], 1)
        assert gnoths.parse_text(world) == "((1 2) (1))"
        assert [g.glom.print_value for g in gnoths.gnoths(world)] == [(1, 2), (1,)]


# ---- equivalence [M86 pp. 94-98] ------------------------------------------------------------

class TestEquivalence:

    def p95_gnoths(self):
        """p. 95's example under `(S-group 1 3)`: gnoth2 over `glom1 (1 2)` and `glint3`,
        gnoth3 over three glints [M86 p. 95]."""
        world, _ = world_with(1, 2, 3, 1, 2, 3)
        glom1 = glom_over(world, "glint1", "glint2")
        gnoth2 = gnoths.make_gnoth(world, "S-group", [glom1, world.find("glint3")], 0)
        gnoth3 = gnoths.make_gnoth(world, "S-group", [world.find(n) for n in
                                                     ("glint4", "glint5", "glint6")], 1)
        return world, gnoth2, gnoth3

    def test_values_are_the_parenthesised_collections(self):
        """gnoth2's value is `((1 2) 3)` and gnoth3's `(1 2 3)` [M86 p. 95]."""
        _, gnoth2, gnoth3 = self.p95_gnoths()
        assert gnoth2.parse_print == ((1, 2), 3)
        assert gnoth3.parse_print == (1, 2, 3)
        assert gnoth2.terms == gnoth3.terms == (1, 2, 3)

    def test_term_equivalence_both_parse_equivalence_gnoth3_only(self):
        """Both are term-equivalent to the frame `(1 2 3)`; "gnoth3 is parse-equivalent to
        the given hypothesis while gnoth2 is not" [M86 p. 95]."""
        _, gnoth2, gnoth3 = self.p95_gnoths()
        frame = (1, 2, 3)
        assert term_equivalent(gnoth2, frame) and term_equivalent(gnoth3, frame)
        assert parse_equivalent(gnoth3, frame) and not parse_equivalent(gnoth2, frame)
        assert equivalence_type(gnoth2, frame) == "term"
        assert equivalence_type(gnoth3, frame) == "parse"
        assert equivalence_type(gnoth3, (1, 2)) is None

    def test_a_gnoth_maker_gnoth_over_one_glom_is_term_equivalent_only(self):
        """A gnoth over the single glom `(3 7)` has the parse-print `((3 7))`, so against the
        Cycle's frame `(3 7)` (one turn, DECISION D-004) it is term-equivalent only
        (DECISION D-051, the reading recorded there)."""
        world, _ = full_run([3, 7, 3], FENCE_SEED)
        gnoth = gnoths.gnoths(world)[0]
        assert gnoth.parse_print == ((3, 7),)
        assert equivalence_type(gnoth, (3, 7)) == "term"
        assert gnoths.parse_text(world) == "(((3 7)))"

    @pytest.mark.parametrize("k, deep, shallow", [
        (1, "(C-group (S-group 1 3) 2)", "(C-group (1 2 3) 2)"),
        (2, "(C-group (S-group 2 3) 2)", "(C-group (2 3 4) 2)"),
        (3, "(C-group (S-group 3 3) 2)", "(C-group (3 4 5) 2)"),
    ])
    def test_p96_deep_and_shallow_frames(self, k, deep, shallow):
        """The first three frames of `(C-group (S-group (Countup 1) 3) 2)`, deep and shallow
        [M86 p. 96]."""
        form = parse("(C-group (S-group (Countup 1) 3) 2)")
        assert sexp_text(deep_frame(form, k)) == deep
        assert sexp_text(shallow_frame(form, k)) == shallow

    def test_deep_frame_of_other_forms(self):
        """A group with a Countup length, and a Cycle with a shared Countup [M86 p. 57]."""
        assert sexp_text(deep_frame(parse("(S-group 1 (Countup 2))"), 2)) == "(S-group 1 3)"
        assert sexp_text(deep_frame(parse("(Cycle (8 (Countup 1) shared))"), 2)) == \
            "(Cycle 8 3 (8 2 shared))"
        assert sexp_text(deep_frame(parse("(Cycle 3 2 (3 7))"), 5)) == "(Cycle 3 2 (3 7))"

    def test_structural_equivalence_is_a_check_only(self):
        """"a gnoth must be parse-equivalent to it and the gnoth's form must be the same as
        the corresponding frame of the hypothesis" [M86 pp. 95-96] in deep form: true for a
        gnoth over two S-group gloms with the form `(C-group (S-group 1 3) 2)` against the
        first frame, false against the second; nothing in a run calls it [M86 p. 97]
        (DECISION D-051)."""
        world, _ = world_with(1, 2, 3, 1, 2, 3)
        a = glom_over(world, "glint1", "glint2", "glint3")
        b = glom_over(world, "glint4", "glint5", "glint6")
        gnoth = gnoths.make_gnoth(world, "C-group", [a, b], 0)
        gnoth.form = parse("(C-group (S-group 1 3) 2)")
        form = parse("(C-group (S-group (Countup 1) 3) 2)")
        frame = ((1, 2, 3), (1, 2, 3))
        assert parse_equivalent(gnoth, frame)
        assert structural_equivalent(gnoth, frame, deep_frame(form, 1))
        assert not structural_equivalent(gnoth, frame, deep_frame(form, 2))
        assert not structural_equivalent(gnoth, ((2, 3, 4), (2, 3, 4)), deep_frame(form, 1))
        gnoth.form = [(parse("(C-group (S-group 1 3) 2)"), "pure")]
        assert structural_equivalent(gnoth, frame, deep_frame(form, 1))
        gnoth.form = None
        assert not structural_equivalent(gnoth, frame, deep_frame(form, 1))

    def test_to_sexp_round_trip_with_deep_frame(self):
        assert to_sexp(parse("(C-group (S-group 1 3) 2)")) == ("C-group", ("S-group", 1, 3), 2)
        assert to_sexp(parse("(Cycle 3 2 (3 7))")) == ("Cycle", 3, 2, (3, 7))
        assert to_sexp(parse("(Countup 1)")) == ("Countup", 1)
        assert to_sexp(4) == 4


# ---- the Gnoth-maker and the Hypothesizer in runs [M86 pp. 165-168, 170-171] -----------------

class TestRuns:

    def test_3_7_3_on_a_fence_seed_prints_run_1s_lines(self):
        """After the template, the fence seed prints `top-down glom glom3`, `gnoths
        constructed`, the Hypothesizer's `(Cycle 3 2 (3 7))` and `doing task Call-term` last
        [M86 pp. 165-166]; `show` lists `glom1` and `glom3 pseudo --> ((3 7))`, not `glom2`
        [M86 p. 167], and the gnoth's nine fields exactly [M86 p. 168]."""
        trace = Trace(io.StringIO())
        world, _ = full_run([3, 7, 3], FENCE_SEED, trace=trace)
        assert "top-down glom glom3" in trace.lines and "gnoths constructed" in trace.lines
        assert trace.lines.index("top-down glom glom3") + 1 == trace.lines.index(
            "gnoths constructed")
        assert trace.lines[-1] == "doing task Call-term"
        hyp = trace.lines.index("doing task Hypothesizer")
        assert trace.lines[hyp + 1] == "(Cycle 3 2 (3 7))"
        present = [x.name for x in world.cytoplasm if isinstance(x, cy.Glom)]
        assert present == ["glom1", "glom3"]
        glom3 = world.find("glom3")
        assert glom3.pseudo and glom3.print_value == ((3, 7),) and glom3.subgloms[0].name == "glom1"
        assert gnoths.show_gnoths_lines(world) == [
            "gnoths:", "", "class: Gnoths", "name: gnoth1", "frame: 0", "plato-class: Cycle",
            "glom: glom3", "notes: nil", "form: (((Cycle 3 2 (3 7)) pure))", "state: stable",
            "range: (1 2)"]
        assert world.hypothesis.show_form() == "(Cycle 3 2 (3 7))"
        assert world.hypothesis.validity == 0 and world.hypothesis.next_term() == 7

    def test_the_trailing_term_stays_free(self):
        """The `3` after `(3 7)` is beyond the template's coverage `(1 2)` [M86 p. 167]; it
        is in no listed gnoth and floats free in the cytoplasm until the Hfilter makes the
        catchall at the first confirming term (DECISIONS D-030, D-062, D-063)."""
        world, _ = full_run([3, 7, 3], FENCE_SEED)
        assert gnoths.catchall(world) is None
        assert len(gnoths.gnoths(world)) == 1
        glint3 = world.find("glint3")
        assert glint3.parent is None and glint3 in cy.lineup(world)

    def test_the_hypothesis_cleans_the_template_tasks(self):
        """When a hypothesis is accepted the template types are removed from the rack
        (DECISION D-010); the template object stays, accepted, for the record."""
        world, recorder = full_run([3, 7, 3], FENCE_SEED)
        assert not any(t.startswith("Template") for t in world.taskrack.count_by_type())
        assert world.template.state == "accepted"
        world, recorder = world_with(3, 1, 4)
        world.post("Template-scout", "glom1")
        world.post("Hypothesizer")
        run_all(world)
        cleaned = [e for e in recorder.of_kind("cleaned") if e.moment == "hypothesis"]
        assert len(cleaned) == 1 and cleaned[0].removed == (("Template-scout", 1),)

    def test_3_7_3_ends_stage_one_with_a_hypothesis_for_every_seed(self):
        """Over 60 seeds `3 7 3` ends with `(Cycle 3 2 (3 7))` (the fence, as run 1
        [M86 p. 166]) or `(Y-group 3 3 (3 7 3))` (the cover; see RESULTS.md), one gnoth
        either way, and the prompt comes after both the Gnoth-maker and the Hypothesizer
        in every run (DECISION D-012)."""
        forms = Counter()
        for seed in range(60):
            trace = Trace(io.StringIO())
            world, _ = full_run([3, 7, 3], seed, trace=trace)
            forms[world.hypothesis.show_form()] += 1
            assert len(gnoths.gnoths(world)) == 1, seed
            tasks = [l.split()[2] for l in lines(trace, "doing task ")]
            assert tasks[-1] == "Call-term", seed
            assert "Gnoth-maker" in tasks and "Hypothesizer" in tasks, seed
        assert set(forms) == {"(Cycle 3 2 (3 7))", "(Y-group 3 3 (3 7 3))"}
        assert forms["(Cycle 3 2 (3 7))"] >= 12       # about a third (RESULTS.md)

    def test_1_2_gives_Countup_1_for_every_seed(self):
        """`1 2` "causes the system to hypothesize (Countup 1)" [M86 p. 98], printed after
        `doing task Hypothesizer` [M86 p. 171] (DECISION D-031), for every seed tried; the
        gnoth's fields are p. 171's."""
        for seed in range(30):
            trace = Trace(io.StringIO())
            world, _ = full_run([1, 2], seed, trace=trace)
            assert world.hypothesis.show_form() == "(Countup 1)", seed
            hyp = trace.lines.index("doing task Hypothesizer")
            assert trace.lines[hyp + 1] == "(Countup 1)"
            assert gnoths.show_gnoths_lines(world) == [
                "gnoths:", "", "class: Gnoths", "name: gnoth1", "frame: 0",
                "plato-class: S-group", "glom: glom3", "notes: nil",
                "form: (((S-group 1 2) pure))", "state: stable", "range: (1 2)"], seed
            assert gnoths.catchall(world) is None

    def test_the_prompt_follows_both_tasks_in_either_order(self):
        """In runs 1 and 3 the Hypothesizer and the Gnoth-maker run in either order and the
        Call-term after both [M86 pp. 165, 171] (DECISION D-012): both orders occur and the
        Call-term is never before either."""
        orders = Counter()
        for seed in range(40):
            trace = Trace(io.StringIO())
            full_run([1, 2], seed, trace=trace)
            tasks = [l.split()[2] for l in lines(trace, "doing task ")]
            picked = [t for t in tasks if t in ("Gnoth-maker", "Hypothesizer", "Call-term")]
            assert picked[-1] == "Call-term" and len(picked) == 3, seed
            orders[tuple(picked[:2])] += 1
        assert len(orders) == 2

    def test_1_2_1_2_3_refutes_its_first_hypothesis_at_the_third_term(self):
        """The hypothesis made after `1 2` is refuted by the third term, as the first
        hypothesis of run 3 was [M86 pp. 171-172], and freeze-dried; the template filter
        does nothing once a hypothesis exists [M86 p. 74]."""
        world, recorder = full_run([1, 2, 1, 2, 3], 1)
        assert recorder.of_kind("hypothesis")[0].form == "(Countup 1)"
        assert recorder.of_kind("refuted")[0].n == 3
        assert str(world.freeze_dried[0]) == "(Countup 1) (2 terms)"
        assert len(recorder.of_kind("template-filtered")) == 0

    def test_same_seed_same_trace(self):
        traces = []
        for _ in range(2):
            trace = Trace(io.StringIO())
            full_run([3, 7, 3], 7, trace=trace)
            traces.append(trace.lines)
        assert traces[0] == traces[1]

    def test_events_in_order(self):
        """Each gnoth-glom's `TopDownGlom` precedes its `GnothMade`; `GnothsConstructed`
        closes the Gnoth-maker (no `CatchallMade`: DECISION D-062); `HypothesisMade` names
        the source."""
        world, recorder = full_run([3, 7, 3], FENCE_SEED)
        kinds = [e.kind for e in recorder.events]
        g = kinds.index("gnoth")
        assert kinds[g - 1] == "top-down"
        assert "catchall" not in kinds and "gnoths-constructed" in kinds
        made = recorder.of_kind("gnoth")[0]
        assert (made.name, made.frame, made.plato_class, made.glom, made.range, made.members) == (
            "gnoth1", 0, "Cycle", "glom3", (1, 2), ("glom1",))
        hyp = recorder.of_kind("hypothesis")[0]
        assert (hyp.form, hyp.source) == ("(Cycle 3 2 (3 7))", "template")


# ---- stale cases and destruction ----------------------------------------------------------------

class TestStale:

    def test_gnoth_maker_without_an_accepted_template(self):
        world, recorder = world_with(3, 7)
        world.post("Gnoth-maker")
        run_all(world)
        assert recorder.of_kind("stale")[-1].type == "Gnoth-maker"
        assert gnoths.gnoths(world) == [] and gnoths.catchall(world) is None

    def test_a_second_gnoth_maker_does_nothing(self):
        world, recorder = p78_world()
        before = len(world.cytoplasm)
        world.post("Gnoth-maker")
        run_all(world)
        assert recorder.of_kind("stale")[-1].reason in (
            "the template glom is gone", "gnoths exist already")
        assert len(world.cytoplasm) == before and len(gnoths.gnoths(world)) == 3

    def test_gnoth_maker_whose_template_glom_was_dissolved(self):
        """A template whose glom is gone makes no gnoths (DECISION D-009)."""
        world, recorder = world_with(3, 7, 3, seed=FENCE_SEED)
        # Build the template by hand, dissolve its glom, then run the Gnoth-maker alone.
        world.handlers["Gnoth-maker"] = gnoths.gnoth_maker
        glom = glom_over(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
        gloms.dub(world, glom, "Cycle", "pure", scout=False)
        from seekwhence import templates
        fit = templates.fit_lineup(parse("(Cycle 3 2 (3 7))"), cy.lineup(world))
        template = templates.make_template(world, parse("(Cycle 3 2 (3 7))"), glom.name, fit)
        template.state = "accepted"
        world.taskrack.clean(("Template-evaluator",))
        cy.dissolve(world, template.glom)
        world.post("Gnoth-maker")
        run_all(world)
        assert recorder.of_kind("stale")[-1].reason == "the template glom is gone"
        assert gnoths.gnoths(world) == []

    def test_hypothesizer_with_a_hypothesis_already(self):
        world, recorder = full_run([1, 2], 1)
        form = world.hypothesis.show_form()
        world.post("Hypothesizer")
        run_all(world)
        assert recorder.of_kind("stale")[-1].type == "Hypothesizer"
        assert world.hypothesis.show_form() == form
        assert len(recorder.of_kind("hypothesis")) == 1

    def test_destroy_gnoth(self):
        """Destroying a gnoth removes it from the socratoplasm and the actualization lists
        and dissolves its gnoth-glom, freeing the unit [M86 p. 44]."""
        world, _ = p78_world()
        gnoth = gnoths.gnoths(world)[0]
        unit = gnoth.collection[0]
        gnoths.destroy_gnoth(world, gnoth)
        assert gnoth.name not in world.socratoplasm
        assert gnoth not in world.platoplasm.type("S-group").actualizations
        assert gnoth.glom.name not in world.cytoplasm and unit.parent is None
        assert [g.name for g in gnoths.gnoths(world)] == ["gnoth2", "gnoth3"]


# ---- the Hypothesizer's form [M86 p. 79] (DECISION D-031) --------------------------------------

class FakeTemplate:
    def __init__(self, form, units, state="accepted"):
        self.form = parse(form) if isinstance(form, str) else form
        self.units = units
        self.state = state


def units_of(world, *groups):
    """Make one top-down glom per group of glint names and return their names."""
    names = []
    for group in groups:
        members = [world.find(n) for n in group]
        if len(members) == 1:
            names.append(members[0].name)
        else:
            names.append(cy.make_glom(world, ("top-down",), members, pseudo=True).name)
    return names


class TestHypothesizerForm:

    def test_a_repeated_template_without_wildcard_is_the_form(self):
        """`(Cycle 3 2 (3 7))` from the template `(Cycle 3 2 (3 7))` [M86 pp. 165-166]."""
        world, _ = world_with(3, 7, 3, 7)
        units = units_of(world, ("glint1", "glint2"), ("glint3", "glint4"))
        form = form_from_template(world, FakeTemplate("(Cycle 3 2 (3 7))", units))
        assert str(form) == "(Cycle 3 2 (3 7))"

    def test_a_single_s_group_hit_becomes_a_countup(self):
        """`(Countup 1)` from the template `(S-group 1 2)` with one hit [M86 pp. 98, 171]."""
        world, _ = world_with(1, 2)
        units = units_of(world, ("glint1", "glint2"))
        assert str(form_from_template(world, FakeTemplate("(S-group 1 2)", units))) == "(Countup 1)"

    def test_a_single_c_group_hit_becomes_a_constant(self):
        world, _ = world_with(1, 1)
        units = units_of(world, ("glint1", "glint2"))
        assert str(form_from_template(world, FakeTemplate("(C-group 1 2)", units))) == "(Constant 1)"

    def test_a_single_p_group_or_fence_hit_stays(self):
        """There is no counting-down repeater, so a P-group hit stays; a fence form with one
        hit stays (DECISION D-031)."""
        world, _ = world_with(2, 1)
        units = units_of(world, ("glint1", "glint2"))
        assert str(form_from_template(world, FakeTemplate("(P-group 2 2)", units))) == "(P-group 2 2)"
        world, _ = world_with(3, 7, 3)
        units = units_of(world, ("glint1", "glint2"))
        assert str(form_from_template(world, FakeTemplate("(Cycle 3 2 (3 7))", units))) == \
            "(Cycle 3 2 (3 7))"

    def test_two_s_group_hits_stay_an_s_group(self):
        world, _ = world_with(1, 2, 1, 2)
        units = units_of(world, ("glint1", "glint2"), ("glint3", "glint4"))
        assert str(form_from_template(world, FakeTemplate("(S-group 1 2)", units))) == "(S-group 1 2)"

    def test_wildcards_are_generalised_over_the_units(self):
        """`(S-group 1 n)` over `(1 2)`, `(1 2 3)` gives `(S-group 1 (Countup 2))`, the
        thesis's final form for run 3 [M86 p. 178]; `(S-group n 2)` over `(1 2)`, `(1 2)`
        gives `(S-group 1 2)`; `(S-group n n)` over `(1 2)`, `(2 3 4)` gives
        `(S-group (Countup 1) (Countup 2))`."""
        world, _ = world_with(1, 2, 1, 2, 3)
        units = units_of(world, ("glint1", "glint2"), ("glint3", "glint4", "glint5"))
        assert str(form_from_template(world, FakeTemplate("(S-group 1 n)", units))) == \
            "(S-group 1 (Countup 2))"
        world, _ = world_with(1, 2, 1, 2)
        units = units_of(world, ("glint1", "glint2"), ("glint3", "glint4"))
        assert str(form_from_template(world, FakeTemplate("(S-group n 2)", units))) == "(S-group 1 2)"
        world, _ = world_with(1, 2, 2, 3, 4)
        units = units_of(world, ("glint1", "glint2"), ("glint3", "glint4", "glint5"))
        assert str(form_from_template(world, FakeTemplate("(S-group n n)", units))) == \
            "(S-group (Countup 1) (Countup 2))"

    def test_values_that_neither_repeat_nor_count_up_give_nothing(self):
        world, _ = world_with(1, 2, 1, 2, 3, 4)
        units = units_of(world, ("glint1", "glint2"), ("glint3", "glint4", "glint5", "glint6"))
        assert form_from_template(world, FakeTemplate("(S-group 1 n)", units)) is None

    def test_missing_units_give_nothing(self):
        world, _ = world_with(1, 2)
        assert form_from_template(world, FakeTemplate("(S-group 1 2)", ("glom99",))) is None

    @pytest.mark.parametrize("values, expected", [
        ((1, 1, 1), 1), ((2,), 2), ((1, 2, 3), "(Countup 1)"), ((3, 4), "(Countup 3)"),
        ((1, 3), None), ((2, 1), None), ((), None),
    ])
    def test_generalise(self, values, expected):
        """Equal values stay, values counting up by one become a Countup, else nothing
        (DECISION D-031; DECISION D-048's rule)."""
        result = generalise(values)
        assert (str(result) if isinstance(result, Form) else result) == expected

    def test_the_fallback_is_a_tuple_of_the_terms(self):
        """"the fall-back position of declaring the sequence to be a Tuple, the weakest of
        all plato-classes" [M86 p. 79]."""
        assert str(fallback_form((3, 1, 4))) == "(Tuple 3 3 (3 1 4))"

    def test_hypothesizer_without_a_template_falls_back(self):
        world, recorder = world_with(3, 1, 4)
        assert world.template is None
        world.post("Hypothesizer")
        run_all(world)
        assert world.hypothesis.show_form() == "(Tuple 3 3 (3 1 4))"
        assert recorder.of_kind("hypothesis")[0].source == "fallback"
        assert world.hypothesis.next_term() == 3       # the Tuple repeats

    def test_a_form_that_does_not_postdict_falls_back(self):
        """The form must postdict the known terms (DECISION D-034); a faulty template falls
        back to the Tuple [M86 p. 79]."""
        world, recorder = world_with(1, 2, 5)
        units = units_of(world, ("glint1", "glint2"))
        world.template = FakeTemplate("(S-group 1 2)", units)
        world.post("Hypothesizer")
        run_all(world)
        assert world.hypothesis.show_form() == "(Tuple 1 3 (1 2 5))"
        assert recorder.of_kind("hypothesis")[0].source == "fallback"

    def test_hypothesizer_without_terms_does_nothing(self):
        world = new_world(seed=1)
        world.post("Hypothesizer")
        run_all(world)
        assert world.hypothesis is None

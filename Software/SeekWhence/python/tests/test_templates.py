"""Templates: the Template-scout, applier, resolver, evaluator and reviewer, the template
glom and filtering past a template [M86 pp. 73-76, 165, 167, 170-171] (DECISIONS D-026 to
D-029, D-060, D-061)."""

import io
from collections import Counter
from types import SimpleNamespace

import pytest

from seekwhence import cytoplasm as cy
from seekwhence import gloms, templates
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.forms import WILD, Form, has_wildcard, parse
from seekwhence.run import enter_term, new_world, run_until_prompt
from seekwhence.templates import Template, fit_lineup, loosenings, unit_match, unit_terms
from seekwhence.trace import Trace

FENCE_SEED = 4     # a seed on which `3 7 3` gloms the fence `(3 7)`, as runs 1 and 2 did


def run_all(world):
    while run_until_prompt(world) != "empty":
        pass


def world_with(*terms, seed=1, scout=False, trace=None, config=None, gnoths=False):
    """A world with the terms entered through `run.enter_term` and the rack run empty after
    each; the Glom-scout is a no-op unless `scout`, so that the tests build the gloms, and
    the Gnoth-maker and Hypothesizer that acceptance sets out [M86 p. 76] are no-ops unless
    `gnoths`, so that these tests see the template level alone (the gnoth tests are in
    `test_gnoths.py`)."""
    recorder = Recorder()
    observers = [recorder] if trace is None else [recorder, trace]
    world = new_world(config, seed=seed, observers=observers)
    if not scout:
        world.handlers["Glom-scout"] = lambda w, *args: None
    if not gnoths:
        world.handlers["Gnoth-maker"] = lambda w, *args: None
        world.handlers["Hypothesizer"] = lambda w, *args: None
    for term in terms:
        enter_term(world, term)
        run_all(world)
    return world, recorder


def glom_over(world, *names, type=("Succ", "print-value", "group")):
    return cy.make_glom(world, type, [world.find(n) for n in names])


def posted(recorder, type):
    return [e for e in recorder.of_kind("posted") if e.type == type]


def lines(trace, prefix):
    return [l for l in trace.lines if l.startswith(prefix)]


# ---- units, matching and loosening [M86 pp. 51-53, 73-74] ----------------------------------

class TestUnits:
    """One unit is one repetition of the dubbed structure [M86 p. 74] (DECISION D-026)."""

    @pytest.mark.parametrize("text, terms", [
        ("(S-group 1 2)", (1, 2)),                       # one hit of a group [M86 p. 51]
        ("(Cycle 3 2 (3 7))", (3, 7)),                   # one turn (D-004)
        ("(Y-group 3 3 (3 7 3))", (3, 7, 3)),            # one hit [M86 p. 52]
        ("(C-group (Tuple 1 2 (1 5)) 3)", (1, 5, 1, 5, 1, 5)),  # `((1 5) (1 5) (1 5))` [M86 p. 69]
        ("(C-group (Cycle 1 2 (1 5)) 3)", (1, 1, 1)),             # a Cycle advances per hit (D-003)
        ("(Tuple 1 3 (1 2 3))", (1, 2, 3)),
    ])
    def test_unit_terms(self, text, terms):
        assert unit_terms(parse(text)) == terms

    def test_a_wildcard_form_has_no_fixed_unit(self):
        with pytest.raises(ValueError):
            unit_terms(parse("(S-group 1 n)"))

    def test_exact_matching_complete_prefix_or_nothing(self):
        form = parse("(Cycle 3 2 (3 7))")
        assert unit_match(form, (3, 7)) == "complete"
        assert unit_match(form, (3,)) == "prefix"       # the `3` after `(3 7)` [M86 p. 167]
        assert unit_match(form, (7,)) is None
        assert unit_match(form, (3, 7, 3)) is None
        assert unit_match(form, ()) is None

    def test_the_p73_template_takes_each_grouping_as_a_unit(self):
        """"the template form '(S-group 1 n)' suffices to explain '1 1 2 1 2 3' since it
        'fits' all the term groupings" [M86 p. 73]."""
        form = parse("(S-group 1 n)")
        for unit in ((1,), (1, 2), (1, 2, 3), (1, 2, 3, 4, 5)):
            assert unit_match(form, unit) == "complete"
        assert unit_match(form, (2, 3)) is None          # the start is pinned
        assert unit_match(form, (1, 3)) is None          # not an S-group

    def test_a_wild_start_with_a_fixed_length(self):
        form = Form("S-group", WILD, 2)
        assert unit_match(form, (4, 5)) == "complete"
        assert unit_match(form, (4,)) == "prefix"
        assert unit_match(form, (4, 5, 6)) is None

    def test_both_wild_needs_two_terms(self):
        """With nothing pinned, one term shows no class at all (DECISION D-026)."""
        form = Form("C-group", WILD, WILD)
        assert unit_match(form, (4,)) is None
        assert unit_match(form, (4, 4)) == "complete"
        assert unit_match(form, (4, 5)) is None

    def test_loosening_order(self):
        """The length first, giving `(S-group 1 n)` [M86 p. 74], then the start, then both
        (DECISION D-026)."""
        assert [str(f) for f in loosenings(parse("(S-group 1 2)"))] == [
            "(S-group 1 n)", "(S-group n 2)", "(S-group n n)"]
        assert [str(f) for f in loosenings(parse("(S-group 1 n)"))] == ["(S-group n n)"]
        assert loosenings(parse("(S-group n n)")) == []

    def test_fence_forms_have_nothing_to_loosen(self):
        assert loosenings(parse("(Cycle 3 2 (3 7))")) == []
        assert loosenings(parse("(Y-group 3 3 (3 7 3))")) == []

    def test_the_wildcard_parses_and_prints(self):
        form = parse("(S-group 1 n)")
        assert str(form) == "(S-group 1 n)"
        assert has_wildcard(form) and not has_wildcard(parse("(S-group 1 2)"))
        assert form.length is WILD


# ---- the fit of a lineup [M86 pp. 74-75, 167] -------------------------------------------------

class TestFit:
    """The applier views the lineup "as a repetition" of the form [M86 p. 74]
    (DECISION D-026)."""

    def p74_world(self):
        """"the sequence terms '1 1 2 1 2 3' were entered and the first '1 2' were glommed,
        dubbed as '(S-group 1 2)'" [M86 p. 74]."""
        world, recorder = world_with(1, 1, 2, 1, 2, 3)
        glom = glom_over(world, "glint2", "glint3")
        return world, recorder, glom

    def test_the_strict_form_fails_on_the_initial_1_and_the_trailing_3(self):
        world, _, _ = self.p74_world()
        assert fit_lineup(parse("(S-group 1 2)"), cy.lineup(world)) is None

    def test_the_loosened_form_fits_all_the_groupings(self):
        world, _, glom = self.p74_world()
        fit = fit_lineup(parse("(S-group 1 n)"), cy.lineup(world))
        assert [[x.name for x in unit] for unit in fit.units] == [
            ["glint1"], [glom.name], ["glint4", "glint5", "glint6"]]
        assert fit.trailing == [] and fit.coverage == (1, 6)

    def test_run1_fit_has_a_trailing_prefix(self):
        """`3 7 3` with `(3 7)` glommed: coverage `(1 2)` [M86 p. 167]."""
        world, _ = world_with(3, 7, 3)
        glom = glom_over(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
        fit = fit_lineup(parse("(Cycle 3 2 (3 7))"), cy.lineup(world))
        assert [[x.name for x in unit] for unit in fit.units] == [[glom.name]]
        assert [x.name for x in fit.trailing] == ["glint3"]
        assert fit.coverage == (1, 2)

    def test_the_p75_gloms_fail_the_applier_but_not_the_glints(self):
        """"if the segment '1 1 2 1 2 3' were glommed as (1 1) 2 (1 2 3), the
        Template-applier would fail because of the first glom. A Template-resolver working
        with the template '(S-group 1 n)', however, might be able to find the appropriate
        S-groups by looking at the sequence terms rather than the gloms" [M86 p. 75]."""
        world, _ = world_with(1, 1, 2, 1, 2, 3)
        glom_over(world, "glint1", "glint2", type=("Same", "print-value", "group"))
        glom_over(world, "glint4", "glint5", "glint6")
        form = parse("(S-group 1 n)")
        assert fit_lineup(form, cy.lineup(world)) is None
        fit = fit_lineup(form, cy.glints(world))
        assert [[x.print_value for x in unit] for unit in fit.units] == [[1], [1, 2], [1, 2, 3]]

    def test_a_prefix_glom_that_is_not_last_fails(self):
        world, _ = world_with(3, 7, 3, 7)
        glom_over(world, "glint3", type=("Same", "print-value", "fence"))  # a one-glint glom `3`
        assert fit_lineup(parse("(Cycle 3 2 (3 7))"), cy.lineup(world)) is None

    def test_units_of_free_glints_are_cut_as_long_as_they_can_be(self):
        world, _ = world_with(1, 2, 3, 1, 2, 1)
        fit = fit_lineup(parse("(S-group 1 n)"), cy.lineup(world))
        assert [[x.print_value for x in unit] for unit in fit.units] == [[1, 2, 3], [1, 2], [1]]

    def test_the_templates_own_gloms_are_read_through(self):
        """The template glom is read as its subgloms and the unit gloms the template made
        as their glints (DECISION D-028)."""
        world, _ = world_with(1, 2, 1, 2, 3)
        glom = glom_over(world, "glint1", "glint2")
        fit = fit_lineup(parse("(S-group 1 n)"), cy.lineup(world))
        template = templates.make_template(world, parse("(S-group 1 n)"), glom.name, fit)
        assert template.made and template.glom.top_level
        again = fit_lineup(parse("(S-group 1 n)"), cy.lineup(world), template)
        assert [[x.name for x in unit] for unit in again.units] == [
            [glom.name], ["glint3", "glint4", "glint5"]]
        # Without the template, its glom is one element and no unit.
        assert fit_lineup(parse("(S-group 1 n)"), cy.lineup(world)) is None


# ---- the Template-scout [M86 pp. 73-74, 165, 170-171] -------------------------------------------

class TestScout:
    """"Whenever a glom is dubbed as a manifestation of some plato-type, a Template-scout
    process is placed on the taskrack" [M86 p. 73] (DECISION D-060)."""

    def test_a_glom_that_is_the_whole_sequence_becomes_the_template_at_once(self):
        """Run 3: `Template-scout on (glom1)` is followed directly by `create-template-glom
        (S-group 1 2)(glom1)` [M86 pp. 170-171] (DECISION D-060)."""
        trace = Trace(io.StringIO())
        world, recorder = world_with(1, 2, trace=trace)
        glom = glom_over(world, "glint1", "glint2")
        gloms.dub(world, glom, "S-group", "pure")
        run_all(world)
        assert posted(recorder, "Template-scout") and not posted(recorder, "Template-applier")
        task_lines = [l for l in trace.lines if l.startswith("doing task Template")
                      or l.startswith("create-template")]
        assert task_lines[:2] == ["doing task Template-scout on (glom1)",
                                  "create-template-glom (S-group 1 2)(glom1)"]
        assert world.template.form == parse("(S-group 1 2)")

    def test_otherwise_an_applier_is_set_out_with_the_glom_and_the_form(self):
        """Run 1: `Template-applier on (glom1 (Cycle 3 2 (3 7)))` [M86 p. 165]."""
        world, recorder = world_with(3, 7, 3)
        glom = glom_over(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
        gloms.dub(world, glom, "Cycle", "pure")
        templates.template_scout(world, glom.name)
        (applier,) = posted(recorder, "Template-applier")
        assert applier.args == ("glom1", parse("(Cycle 3 2 (3 7))"))

    def test_a_scout_is_stale_while_a_template_is_alive(self):
        """"no new templates are considered for the lifetime of the given template"
        [M86 p. 74]."""
        world, recorder = world_with(1, 2)
        glom = glom_over(world, "glint1", "glint2")
        gloms.dub(world, glom, "S-group", "pure")
        run_all(world)
        assert world.template is not None
        other = gloms.dub(world, glom, "P-group", "pure")
        assert other is not None
        before = len(recorder.of_kind("template"))
        templates.template_scout(world, glom.name)
        assert len(recorder.of_kind("template")) == before
        assert any(e.type == "Template-scout" for e in recorder.of_kind("stale"))

    def test_each_dubbing_is_applied_once(self):
        """A glom dubbed twice ("A glom can be dubbed more than once" [M86 p. 69]) sets out
        two scouts; between them each dubbed form is applied once (DECISION D-060)."""
        world, recorder = world_with(1, 1, 2)
        glom = glom_over(world, "glint1", "glint2", type=("Same", "print-value", "group"))
        gloms.dub(world, glom, "C-group", "pure")
        gloms.dub(world, glom, "Y-group", "pure")
        assert len(posted(recorder, "Template-scout")) == 2
        templates.template_scout(world, glom.name)
        templates.template_scout(world, glom.name)
        forms = [e.args[1] for e in posted(recorder, "Template-applier")]
        assert forms == [parse("(C-group 1 2)"), parse("(Y-group 1 2 (1 1))")]

    def test_a_gone_or_undubbed_glom_is_stale(self):
        world, recorder = world_with(1, 2)
        glom = glom_over(world, "glint1", "glint2")
        templates.template_scout(world, glom.name)
        templates.template_scout(world, "glom9")
        assert len([e for e in recorder.of_kind("stale") if e.type == "Template-scout"]) == 2


# ---- the Template-applier and the template glom [M86 pp. 73-74, 165, 167] --------------------

class TestApplier:
    """"The applier would attempt to view the sequence as a repetition of '(S-group 1 2)'
    ... checks to see if loosening a parameter or two in its representation would help"
    [M86 p. 74] (DECISION D-026)."""

    def test_run1_the_template_lines_in_order(self):
        """`3 7 3` on a fence seed prints run 1's template lines [M86 pp. 165, 167]."""
        trace = Trace(io.StringIO())
        world, recorder = world_with(3, 7, 3, seed=FENCE_SEED, scout=True, trace=trace)
        wanted = ["doing task Template-scout on (glom1)",
                  "doing task Template-applier on (glom1 (Cycle 3 2 (3 7)))",
                  "create-template-glom (Cycle 3 2 (3 7))(glom1)",
                  "top-down glom glom2",
                  "template made : (form (Cycle 3 2 (3 7)) state working coverage (1 2) "
                  "glom glom2)",
                  "doing task Template-evaluator",
                  "check-cycle template (Cycle 3 2 ((3 7)))"]
        found = [l for l in trace.lines if l in wanted]
        assert found == wanted
        assert world.template.form == parse("(Cycle 3 2 (3 7))")
        assert world.template.state == "accepted"

    def test_the_template_glom_is_a_pseudo_glom_over_the_engendering_glom(self):
        """"it puts a pseudo-glom called the template glom over the highest-level gloms in
        the cytoplasm ... to prevent the disappearance of the gloms that engendered and now
        reflect it" [M86 p. 73]; `show` prints a pseudo-glom's type as `pseudo`
        [M86 p. 167]."""
        world, _ = world_with(3, 7, 3, seed=FENCE_SEED, scout=True)
        template = world.template
        glom1, glom2 = world.find("glom1"), world.find("glom2")
        assert template.glom is glom2 and glom2.pseudo and glom2.type_text() == "pseudo"
        assert glom2.subgloms == (glom1,) and glom1.parent is glom2
        assert glom2.print_value == ((3, 7),) and glom2.positions_covered == (1, 2)
        assert template.coverage == (1, 2) and template.units == ("glom1",)
        assert template.fields_text() == ("(form (Cycle 3 2 (3 7)) state accepted coverage "
                                          "(1 2) glom glom2)")

    def test_the_template_glom_protects_its_subgloms_and_is_inert(self):
        """"A pseudo-glom cannot combine with real gloms, and it prevents its subgloms from
        glomming activity as well" [M86 p. 74]."""
        world, _ = world_with(3, 7, 3, seed=FENCE_SEED, scout=True)
        glom1, glom2 = world.find("glom1"), world.find("glom2")
        assert not glom1.top_level
        assert cy.top_level_elements(world, pseudo=False) == [world.find("glint3")]
        assert gloms.extent(world, glom2, "Same", "group") is None or \
            not gloms.usable(gloms.extent(world, glom2, "Same", "group"))
        world.handlers["Dissolver"](world, "glom2")
        world.handlers["Burster"](world, "glom2")
        assert world.find("glom2") is glom2 and world.find("glom1") is glom1

    def test_3_7_3_over_many_seeds(self):
        """Every seed ends with the fence template `(Cycle 3 2 (3 7))` [M86 p. 165] or the
        cover's `(Y-group 3 3 (3 7 3))` (RESULTS.md); both occur."""
        counts = Counter()
        for seed in range(1, 41):
            world, _ = world_with(3, 7, 3, seed=seed, scout=True)
            counts[str(world.template.form)] += 1
        assert set(counts) == {"(Cycle 3 2 (3 7))", "(Y-group 3 3 (3 7 3))"}
        assert all(n > 0 for n in counts.values())

    def test_p74_loosening_gives_S_group_1_n(self):
        """"changing the form from the original '(S-group 1 2)' to '(S-group 1 n)' ... will
        do the trick. The accepted template will then be '(S-group 1 n)'" [M86 p. 74]."""
        trace = Trace(io.StringIO())
        world, recorder = world_with(1, 1, 2, 1, 2, 3, trace=trace)
        glom = glom_over(world, "glint2", "glint3")
        gloms.dub(world, glom, "S-group", "pure")
        run_all(world)
        template = world.template
        assert str(template.form) == "(S-group 1 n)"
        assert template.state == "accepted" and template.coverage == (1, 6)
        assert not posted(recorder, "Template-resolver")

    def test_p78_the_units_become_gloms_under_the_template_glom(self):
        """The Gnoth-maker example has gloms `(1)`, `(1 2)` and `(1 2 3)` under the
        template `(S-group 1 n)` [M86 p. 78]: the units made of free glints get top-down
        gloms, dubbed with their own form (DECISION D-061)."""
        trace = Trace(io.StringIO())
        world, _ = world_with(1, 1, 2, 1, 2, 3, trace=trace)
        glom = glom_over(world, "glint2", "glint3")
        gloms.dub(world, glom, "S-group", "pure")
        run_all(world)
        template = world.template
        units = [world.find(name) for name in template.units]
        assert [u.print_value for u in units] == [(1,), (1, 2), (1, 2, 3)]
        assert [u.name for u in units] == ["glom2", "glom1", "glom3"]
        assert all(u.pseudo for u in (units[0], units[2])) and not units[1].pseudo
        assert [str(u.structure[0][0]) for u in units] == [
            "(S-group 1 1)", "(S-group 1 2)", "(S-group 1 3)"]
        assert template.glom.subgloms == tuple(units)
        assert template.glom.print_value == ((1,), (1, 2), (1, 2, 3))
        assert lines(trace, "top-down glom") == ["top-down glom glom2", "top-down glom glom3",
                                                 "top-down glom glom4"]
        assert lines(trace, "create-template-glom") == [
            "create-template-glom (S-group 1 n)(glom2 glom1 glom3)"]
        manifestations = world.platoplasm.type("S-group").manifestations
        assert all(u in manifestations for u in units)

    def test_failure_sets_out_a_resolver(self):
        """"If the Template-applier fails, it creates a Template-resolver task" [M86 p. 75]."""
        world, recorder = world_with(1, 1, 2, 1, 2, 3)
        glom_over(world, "glint1", "glint2", type=("Same", "print-value", "group"))
        glom = glom_over(world, "glint4", "glint5", "glint6")
        gloms.dub(world, glom, "S-group", "pure")
        templates.template_applier(world, glom.name, parse("(S-group 1 3)"))
        (resolver,) = posted(recorder, "Template-resolver")
        assert resolver.args == (glom.name, parse("(S-group 1 3)"))
        assert world.template is None

    def test_a_stale_applier(self):
        world, recorder = world_with(1, 2)
        glom = glom_over(world, "glint1", "glint2")
        templates.template_applier(world, glom.name, parse("(S-group 1 2)"))   # not dubbed so
        templates.template_applier(world, "glom7", parse("(S-group 1 2)"))
        assert len([e for e in recorder.of_kind("stale") if e.type == "Template-applier"]) == 2
        assert world.template is None


# ---- the Template-resolver [M86 p. 75] -------------------------------------------------------

class TestResolver:
    """"the resolver looks at glints rather than gloms" [M86 p. 75] (DECISION D-029)."""

    def p75_world(self):
        world, recorder = world_with(1, 1, 2, 1, 2, 3)
        glom_over(world, "glint1", "glint2", type=("Same", "print-value", "group"))
        glom = glom_over(world, "glint4", "glint5", "glint6")
        gloms.dub(world, glom, "S-group", "pure")
        return world, recorder, glom

    def test_success_blasts_the_gloms_and_reglomms_the_glints(self):
        """"If the Template-resolver is successful, it 'blasts' (does an immediate burst of)
        all gloms and has the glints reglommed to fit the template" [M86 p. 75]."""
        world, recorder, glom = self.p75_world()
        templates.template_resolver(world, glom.name, parse("(S-group 1 3)"))
        (blast,) = recorder.of_kind("blast")
        assert blast.burst == ("glom1", "glom2")
        template = world.template
        assert str(template.form) == "(S-group 1 n)"
        units = [world.find(name) for name in template.units]
        assert [u.print_value for u in units] == [(1,), (1, 2), (1, 2, 3)]
        assert all(u.pseudo for u in units) and template.coverage == (1, 6)
        assert world.find("glom1") is None and world.find("glom2") is None

    def test_failure_forgets_the_template_and_dissolves_the_engendering_glom(self):
        """"If the proposed template does not work at term level, it is forgotten and the
        engendering glom dissolved" [M86 p. 75]."""
        world, recorder = world_with(1, 1, 2, 1, 2, 3)
        glom = glom_over(world, "glint1", "glint2", type=("Same", "print-value", "group"))
        gloms.dub(world, glom, "C-group", "pure")
        templates.template_resolver(world, glom.name, parse("(C-group 1 2)"))
        assert world.template is None and world.find("glom1") is None
        assert recorder.of_kind("dissolved")[-1].name == "glom1"
        assert not recorder.of_kind("blast")
        assert world.find("glint1").top_level

    def test_a_resolver_is_stale_while_a_template_is_alive(self):
        world, recorder, glom = self.p75_world()
        templates.template_resolver(world, glom.name, parse("(S-group 1 3)"))
        templates.template_resolver(world, glom.name, parse("(S-group 1 3)"))
        assert len(recorder.of_kind("blast")) == 1


# ---- the Template-evaluator and acceptance [M86 pp. 74-76, 165, 167, 171] -------------------

class TestEvaluator:
    """"The template will be checked by a Template-evaluator task and either be passed, or
    rejected and abandoned" [M86 p. 74] (DECISION D-027)."""

    def test_acceptance_sets_off_a_gnoth_maker_and_a_hypothesizer(self):
        """"it sets off two tasks, a Gnoth-maker and a Hypothesizer" [M86 pp. 75-76], and
        Sparklers for the "increased activity" [M86 p. 75] (DECISION D-061)."""
        world, recorder = world_with(3, 7, 3, seed=FENCE_SEED, scout=True)
        assert world.template.state == "accepted"
        assert len(posted(recorder, "Gnoth-maker")) == 1
        assert len(posted(recorder, "Hypothesizer")) == 1
        accepted = recorder.of_kind("template-accepted")
        assert [e.form for e in accepted] == ["(Cycle 3 2 (3 7))"]
        config = Config(sparklers_on_acceptance=5)
        world2, recorder2 = world_with(3, 7, 3, seed=FENCE_SEED, scout=True, config=config)
        assert len(posted(recorder2, "Sparkler")) - len(posted(recorder, "Sparkler")) == 3

    def test_the_check_line_for_a_fence_class_only(self):
        """`check-cycle template (Cycle 3 2 ((3 7)))` [M86 p. 167]; nothing is printed for
        the S-group template of run 3 [M86 p. 171] (DECISION D-027)."""
        world, _ = world_with(3, 7, 3)
        glom = glom_over(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
        gloms.dub(world, glom, "Cycle", "pure")
        run_all(world)
        assert templates.check_text(world.template) == "check-cycle template (Cycle 3 2 ((3 7)))"
        world, recorder = world_with(1, 2)
        glom = glom_over(world, "glint1", "glint2")
        gloms.dub(world, glom, "S-group", "pure")
        run_all(world)
        (check,) = recorder.of_kind("template-check")
        assert check.passed and check.text == ""

    def test_a_template_whose_glom_is_gone_is_abandoned(self):
        world, recorder = world_with(1, 2)
        glom = glom_over(world, "glint1", "glint2")
        fit = fit_lineup(parse("(S-group 1 2)"), cy.lineup(world))
        template = templates.make_template(world, parse("(S-group 1 2)"), glom.name, fit)
        cy.dissolve(world, template.glom)
        templates.template_evaluator(world)
        assert template.state == "abandoned" and world.template is None
        assert recorder.of_kind("template-abandoned")[-1].by == "Template-evaluator"
        assert not posted(recorder, "Gnoth-maker")

    def test_a_template_the_terms_no_longer_fit_is_abandoned(self):
        """A term may arrive between creation and evaluation (DECISION D-027)."""
        world, recorder = world_with(3, 7, 3)
        glom = glom_over(world, "glint1", "glint2", type=("Same", "print-value", "fence"))
        fit = fit_lineup(parse("(Cycle 3 2 (3 7))"), cy.lineup(world))
        template = templates.make_template(world, parse("(Cycle 3 2 (3 7))"), glom.name, fit)
        world.handlers["Template-reviewer"] = lambda w, *a: None
        enter_term(world, 5)
        templates.template_evaluator(world)
        assert template.state == "abandoned"
        assert world.find("glom1") is glom and glom.top_level     # the engendering glom stays
        assert world.find(template.glom.name) is None

    def test_an_evaluator_without_a_working_template_is_stale(self):
        world, recorder = world_with(1, 2)
        templates.template_evaluator(world)
        assert recorder.of_kind("stale")[-1].type == "Template-evaluator"


# ---- filtering past the template and the Template-reviewer [M86 pp. 74-75] ----------------------

class TestFiltering:
    """"until a hypothesis exists, all new terms will be 'filtered' past the template, checked
    for agreement with it. Should a term not fit the template, a review is set up, with
    resulting modification or rejection of the template" [M86 p. 74] (DECISION D-028)."""

    def cycle_world(self, trace=None):
        world, recorder = world_with(3, 7, 3, seed=FENCE_SEED, scout=True, trace=trace)
        assert str(world.template.form) == "(Cycle 3 2 (3 7))"
        return world, recorder

    def test_a_fitting_term_that_completes_a_unit_extends_the_template_glom(self):
        trace = Trace(io.StringIO())
        world, recorder = self.cycle_world(trace)
        enter_term(world, 7)
        (filtered,) = recorder.of_kind("template-filtered")
        assert (filtered.n, filtered.term, filtered.fits) == (4, 7, True)
        template = world.template
        assert template.coverage == (1, 4) and len(template.units) == 2
        unit = world.find(template.units[1])
        assert unit.pseudo and unit.print_value == (3, 7) and unit.parent is template.glom
        assert str(unit.structure[0][0]) == "(Cycle 3 2 (3 7))"
        assert template.glom.print_value == ((3, 7), (3, 7)) and template.glom.span == 4
        assert lines(trace, "top-down glom")[-1] == f"top-down glom {unit.name}"
        assert not posted(recorder, "Template-reviewer")

    def test_a_term_that_begins_a_unit_fits_as_a_prefix(self):
        world, recorder = self.cycle_world()
        enter_term(world, 7)
        enter_term(world, 3)
        assert [e.fits for e in recorder.of_kind("template-filtered")] == [True, True]
        assert world.template.coverage == (1, 4)

    def test_a_term_that_does_not_fit_sets_up_a_review_that_abandons(self):
        """A Cycle has nothing to loosen and `5` begins no unit, so the reviewer targets the
        template "for abandoning" [M86 p. 75]; the template glom goes, the unit gloms the
        template made are burst, the engendering glom survives (DECISION D-028)."""
        world, recorder = self.cycle_world()
        enter_term(world, 7)
        run_all(world)
        enter_term(world, 5)
        assert recorder.of_kind("template-filtered")[-1].fits is False
        assert posted(recorder, "Template-reviewer")
        template = world.template
        run_all(world)
        assert template.state == "abandoned" and world.template is None
        assert recorder.of_kind("template-abandoned")[-1].by == "Template-reviewer"
        assert world.find("glom1") is not None and world.find("glom1").top_level
        assert world.find(template.glom.name) is None
        assert all(world.find(name) is None for name in template.made)
        assert world.find("glint4").top_level and world.find("glint3").top_level

    def test_the_reviewer_leaves_the_template_alone_when_the_term_could_begin_a_unit(self):
        """`3 7 3` then `3`: the earlier `3` dangles but the new `3` could begin `(3 7)`
        (DECISION D-028, "leave the template alone" [M86 p. 75])."""
        world, recorder = self.cycle_world()
        enter_term(world, 3)
        assert recorder.of_kind("template-filtered")[-1].fits is False
        run_all(world)
        assert world.template is not None and world.template.state == "accepted"
        assert any(e.type == "Template-reviewer" and e.reason == "the template is left alone"
                   for e in recorder.of_kind("stale"))
        assert not recorder.of_kind("template-abandoned")

    def test_the_reviewer_modifies_the_template_by_loosening(self):
        """`1 2` then `1 2 3`: the template `(S-group 1 2)` is modified to `(S-group 1 n)`
        and "the creation and evaluation processes" restart [M86 p. 75]."""
        trace = Trace(io.StringIO())
        world, recorder = world_with(1, 2, trace=trace)
        glom = glom_over(world, "glint1", "glint2")
        gloms.dub(world, glom, "S-group", "pure")
        run_all(world)
        assert str(world.template.form) == "(S-group 1 2)"
        for term in (1, 2, 3):
            enter_term(world, term)
            run_all(world)
        (modified,) = recorder.of_kind("template-modified")
        assert (modified.old, modified.new) == ("(S-group 1 2)", "(S-group 1 n)")
        template = world.template
        assert str(template.form) == "(S-group 1 n)" and template.state == "accepted"
        assert template.coverage == (1, 5)
        assert [world.find(n).print_value for n in template.units] == [(1, 2), (1, 2, 3)]
        assert len(recorder.of_kind("template")) == 2
        assert len(posted(recorder, "Template-evaluator")) == 2
        assert lines(trace, "template made")[-1] == (
            "template made : (form (S-group 1 n) state working coverage (1 5) glom "
            f"{template.glom.name})")

    def test_no_filtering_once_a_hypothesis_exists_or_without_a_template(self):
        world, recorder = self.cycle_world()
        world.hypothesis = SimpleNamespace(refuted=True)   # the Hfilter reports itself stale
        assert templates.filter_term(world) is None
        enter_term(world, 5)
        assert not recorder.of_kind("template-filtered")
        world2, recorder2 = world_with(3, 7)
        assert world2.template is None and templates.filter_term(world2) is None

    def test_a_reviewer_without_a_template_is_stale(self):
        world, recorder = world_with(1, 2)
        templates.template_reviewer(world)
        assert recorder.of_kind("stale")[-1].type == "Template-reviewer"


# ---- whole runs ----------------------------------------------------------------------------

class TestRuns:

    def test_1_2_1_2_3_reaches_S_group_1_n_from_scratch(self):
        """With the gloms the cytoplasm makes on its own and the template level alone (no
        hypothesis is made after `1 2`, as it would be in a full run [M86 p. 171]),
        `1 2 1 2 3` ends with the p. 73 template `(S-group 1 n)` for every seed tried."""
        for seed in range(1, 11):
            world, _ = world_with(1, 2, 1, 2, 3, seed=seed, scout=True)
            assert world.template is not None, seed
            assert str(world.template.form) == "(S-group 1 n)", seed

    def test_1_1_2_1_2_3_from_scratch(self):
        """From scratch the initial `1 1` is glommed first and its C-group template is
        made before any S-group is seen, which keeps the scout from `(S-group 1 n)` for the
        lifetime of that template [M86 p. 74]; recorded in RESULTS.md."""
        forms = Counter()
        for seed in range(1, 11):
            world, _ = world_with(1, 1, 2, 1, 2, 3, seed=seed, scout=True)
            forms[str(world.template.form) if world.template else None] += 1
        assert None not in forms
        assert set(forms) <= {"(C-group n 2)", "(C-group 1 n)", "(S-group 1 n)",
                              "(Y-group 1 2 (1 1))"}

    def test_same_seed_same_trace(self):
        traces = []
        for _ in range(2):
            trace = Trace(io.StringIO())
            world_with(1, 2, 1, 2, 3, seed=3, scout=True, trace=trace)
            traces.append(trace.lines)
        assert traces[0] == traces[1]


def test_the_scouts_own_application_prints_template_and_an_evaluator_on_nil():
    """When the Template-scout applies the form itself (`1 2` with `(1 2)` dubbed, the whole
    sequence, DECISION D-060) the printout reads `template: (form (S-group 1 2)` and `doing
    task Template-evaluator on nil` [M86 pp. 170-171], where the Template-applier's path
    prints `template made : (form ...` and a bare evaluator [M86 pp. 165, 167]."""
    trace = Trace(io.StringIO())
    world, recorder = world_with(1, 2, seed=1, scout=True, trace=trace)
    run_all(world)
    made = [l for l in trace.lines if l.startswith("template")]
    assert made == ["template: (form (S-group 1 2) state working coverage (1 2) glom glom2)"]
    assert "doing task Template-evaluator on nil" in trace.lines
    assert not any(l.startswith("template made") for l in trace.lines)
    assert recorder.of_kind("template")[0].immediate is True

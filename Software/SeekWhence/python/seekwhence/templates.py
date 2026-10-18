"""Templates: "one mold to fit all" [M86 pp. 73-76].

"Whenever a glom is dubbed as a manifestation of some plato-type, a Template-scout process is
placed on the taskrack. It and other processes involved in template creation and evaluation
operate at an intermediate level between the 'real', data-driven cyto-level and the
'perceived', perception-driven socrato-level" [M86 p. 73]. "A template is formed when the
structure of some particular dubbed glom is found to explain, at least roughly, all the
sequence terms seen thus far" [M86 p. 73]; it is a dubbed glom's form, possibly with a
parameter loosened to the wildcard `n`, `(S-group 1 n)` [M86 pp. 73-74], and it "puts a
pseudo-glom called the template glom over the highest-level gloms in the cytoplasm"
[M86 p. 73]. The tasks: the Template-scout [M86 pp. 73-74], the Template-applier, which views
"the sequence as a repetition" of the form and loosens its parameters [M86 p. 74], the
Template-resolver, which "looks at glints rather than gloms" and blasts [M86 p. 75], the
Template-evaluator, which passes or abandons the template [M86 p. 74], and the
Template-reviewer, which reviews a new term that fails it [M86 p. 75]; and the filtering of
new terms "past the template" until a hypothesis exists [M86 p. 74]. Acceptance "sets off two
tasks, a Gnoth-maker and a Hypothesizer" [M86 pp. 75-76], which a later item builds.

What the thesis leaves open is decided in DECISIONS D-026 (the fit and the loosening order),
D-027 (the evaluator's check), D-028 (the reviewer's fixes), D-029 (the resolver's blast),
D-060 (the scout: when it makes the template itself) and D-061 (what a template makes: the
unit gloms, the acceptance's tasks). Nothing here draws from the `Rng`: the template level is
deterministic given the cytoplasm it reads. Every task is a function `task(world, *args)`
registered in `TASKS` by its printed name.
"""

from dataclasses import dataclass, field

from seekwhence import cytoplasm as cy
from seekwhence import events, gloms
from seekwhence import platoplasm as pl
from seekwhence.cytoplasm import Glom
from seekwhence.forms import (GROUP_TYPES, LIST_TYPES, WILD, Form, flatten, form_text,
                              has_wildcard, is_int, is_wild, value_text)

# The states a template passes through: `working` as printed [M86 p. 167], then `accepted`
# ("passed" [M86 p. 74]) or `abandoned` ("rejected and abandoned" [M86 p. 74]).
STATES = ("working", "accepted", "abandoned")


@dataclass(eq=False)
class Template:
    """A template, "a template or descriptor of the sequence ... a preliminary step to
    devising a hypothesis" [M86 p. 73], with the four fields the transcript prints,
    `(form (Cycle 3 2 (3 7)) state working coverage (1 2) glom glom2)` [M86 p. 167]:
    `form`, `state` (one of `STATES`), `coverage` (the first and last position its complete
    units cover) and `glom`, the template glom [M86 p. 73]. `from_glom` is the name of the
    dubbed glom that engendered it [M86 p. 73]; `units` the names of the gloms under the
    template glom, one per complete unit; `made` the names of the pseudo-gloms the template
    itself made for units of free glints (DECISION D-061), so that they can be undone with
    it."""

    form: Form
    from_glom: str
    state: str = "working"
    coverage: tuple = (0, 0)
    glom: Glom | None = None
    units: tuple = ()
    made: list = field(default_factory=list)

    @property
    def alive(self) -> bool:
        """Whether the template is working or accepted: "no new templates are considered
        for the lifetime of the given template" [M86 p. 74]."""
        return self.state != "abandoned"

    def fields_text(self) -> str:
        """The four fields as printed: `(form (Cycle 3 2 (3 7)) state working coverage
        (1 2) glom glom2)` [M86 p. 167]."""
        glom = self.glom.name if self.glom is not None else "nil"
        return (f"(form {form_text(self.form)} state {self.state} coverage "
                f"({self.coverage[0]} {self.coverage[1]}) glom {glom})")


# ---- units: what one repetition of a form looks like ---------------------------------------

def unit_terms(form: Form) -> tuple:
    """The terms of one complete unit of a form without a wildcard, the thing the sequence
    is viewed "as a repetition of" [M86 p. 74]: one hit of a group, Y-group or Tuple
    [M86 pp. 51-53], one full turn of a Cycle (DECISION D-004, as a hypothesis's frame)."""
    if has_wildcard(form):
        raise ValueError(f"a form with a wildcard has no fixed unit: {form_text(form)}")
    if form.kind == "Cycle":
        return tuple(flatten(pl.hits(form, len(form.actual))))
    return tuple(flatten(pl.hits(form, 1)[0]))


def unit_match(form: Form, values) -> str | None:
    """How a run of terms stands to one unit of the form: `complete` (it is one unit),
    `prefix` (a proper beginning of one, the "incomplete" last unit a sequence may end with)
    or None (DECISION D-026). With a wildcard, "where 'n' means 'any nonnegative integer'"
    [M86 p. 74], the class's process must pass and the parameters that are not wild must
    match: `(S-group 1 n)` takes `1`, `1 2` and `1 2 3` as complete units [M86 p. 73]. With
    both parameters wild a unit needs two terms, or any term would be a unit."""
    values = tuple(values)
    if not values:
        return None
    if has_wildcard(form):
        if not pl.PROCESSES[form.kind](values):
            return None
        if not is_wild(form.start) and values[0] != form.start:
            return None
        if is_wild(form.start) and is_wild(form.length) and len(values) < 2:
            # With nothing pinned, one term shows no class at all (DECISION D-026).
            return None
        if is_wild(form.length) or len(values) == form.length:
            return "complete"
        return "prefix" if len(values) < form.length else None
    whole = unit_terms(form)
    if values == whole:
        return "complete"
    if len(values) < len(whole) and whole[:len(values)] == values:
        return "prefix"
    return None


def begins_unit(form: Form, term: int) -> bool:
    """Whether a term could begin a new unit of the form, the Template-reviewer's reason to
    "leave the template alone" (DECISION D-028)."""
    return unit_match(form, (term,)) in ("complete", "prefix")


def loosenings(form: Form) -> list:
    """The looser forms the applier tries, in order (DECISION D-026): "loosening a
    parameter or two in its representation ... changing the form from the original
    '(S-group 1 2)' to '(S-group 1 n)'" [M86 p. 74]: the length to `n`, then the start,
    then both. Only a group form with integer parameters can be loosened; a fence form has
    nothing to loosen (its elements are its structure), so the list is empty for it."""
    if form.kind not in GROUP_TYPES:
        return []
    if not (is_int(form.start) or is_wild(form.start)):
        return []
    if not (is_int(form.length) or is_wild(form.length)):
        return []
    out = []
    if not is_wild(form.length):
        out.append(Form(form.kind, form.start, WILD))
    if not is_wild(form.start):
        out.append(Form(form.kind, WILD, form.length))
    if not is_wild(form.start) and not is_wild(form.length):
        out.append(Form(form.kind, WILD, WILD))
    return out


# ---- the fit: the sequence as repetitions of a form --------------------------------------

@dataclass
class Fit:
    """How a lineup of cyto-elements reads as repetitions of a form (DECISION D-026):
    `units`, one list of elements per complete unit, left to right, and `trailing`, the
    elements of an incomplete last unit (empty when the last unit is complete)."""

    units: list
    trailing: list

    @property
    def coverage(self) -> tuple:
        """The first and last position the complete units cover, `(1 2)` for the template
        `(Cycle 3 2 (3 7))` on `3 7 3` [M86 p. 167]; `(0 0)` with no complete unit."""
        if not self.units:
            return (0, 0)
        return (self.units[0][0].start_position, self.units[-1][-1].end_position)


def fit_lineup(form: Form, line, template: Template | None = None) -> Fit | None:
    """View a lineup as repetitions of a form, the Template-applier's attempt "to view the
    sequence as a repetition of '(S-group 1 2)'" [M86 p. 74] (DECISION D-026): a glom must
    be exactly one complete unit (the applier "would fail because of the first glom" of
    `(1 1) 2 (1 2 3)` [M86 p. 75]); a run of free glints is cut into units from the left,
    each as long as it can be; the last element or glints may be a prefix of a unit, the
    `3` after `(3 7)` in `3 7 3` [M86 p. 167]. With a `template` given, its template glom
    is read as its subgloms and the unit gloms it made from above as their glints, so that
    a looser form can cut the units afresh (DECISION D-028). Returns None when the lineup
    does not fit. Given the glints alone, this is the Template-resolver's view, "looking at
    the sequence terms rather than the gloms" [M86 p. 75]."""
    expanded: list = []
    for element in line:
        if template is not None and element is template.glom:
            for unit in element.subgloms:
                if unit.name in template.made:
                    expanded.extend(unit.subgloms)
                else:
                    expanded.append(unit)
        else:
            expanded.append(element)
    units: list = []
    trailing: list = []
    i = 0
    while i < len(expanded):
        element = expanded[i]
        if not element.is_glint:
            match = unit_match(form, element.flat_values())
            if match == "complete":
                units.append([element])
            elif match == "prefix" and i == len(expanded) - 1:
                trailing = [element]
            else:
                return None
            i += 1
            continue
        j = i
        while j < len(expanded) and expanded[j].is_glint:
            j += 1
        k = i
        while k < j:
            end = _longest_unit(form, expanded, k, j)
            if end is None:
                rest = expanded[k:j]
                if j == len(expanded) and unit_match(
                        form, [g.print_value for g in rest]) == "prefix":
                    trailing = rest
                    break
                return None
            units.append(expanded[k:end])
            k = end
        i = j
    return Fit(units, trailing)


def _longest_unit(form: Form, elements: list, start: int, stop: int):
    """The end (exclusive) of the longest complete unit of glints beginning at `start`, or
    None: the Glomtester's thrust "to get maximally-sized gloms" [M86 p. 67] applied to
    units (DECISION D-026)."""
    for end in range(stop, start, -1):
        values = [g.print_value for g in elements[start:end]]
        if unit_match(form, values) == "complete":
            return end
    return None


def fits(form: Form, line, template: Template | None = None) -> bool:
    """Whether the lineup fits the form with at least one complete unit: a template with
    only an incomplete unit is no template (DECISION D-026)."""
    fit = fit_lineup(form, line, template)
    return fit is not None and bool(fit.units)


# ---- making a template -------------------------------------------------------------------

def make_template(world, form: Form, from_glom: str, fit: Fit, immediate: bool = False) -> Template:
    """Make the template from a fit (DECISION D-061): each complete unit of free glints gets
    a top-down pseudo-glom dubbed with the unit's own form ("direction from above:
    instructions to make or dissolve gloms, to create units of a particular form"
    [M86 p. 74]; p. 78's `glom2: (1)`); the template glom, a pseudo-glom, is put over the
    unit gloms [M86 p. 73], printed `create-template-glom (Cycle 3 2 (3 7))(glom1)` and
    `top-down glom glom2` [M86 p. 167]; then the template itself, `template made : (form
    ... state working coverage (1 2) glom glom2)` [M86 p. 167], and a Template-evaluator is
    set out: "The template will be checked by a Template-evaluator task" [M86 p. 74]. When
    the Template-scout applied the form itself (`immediate`, DECISION D-060) the line reads
    `template: (form (S-group 1 2)` and the evaluator is posted with a nil argument, `doing
    task Template-evaluator on nil` [M86 p. 171]."""
    template = Template(form, from_glom)
    unit_gloms = []
    for members in fit.units:
        if len(members) == 1 and not members[0].is_glint:
            unit_gloms.append(members[0])
            continue
        unit = cy.make_glom(world, ("top-down",), members, pseudo=True)
        world.events.publish(events.TopDownGlom(unit.name, tuple(x.name for x in members),
                                                unit.print_value, True))
        gloms.dub(world, unit, form.kind, pl.purity(unit.flat), scout=False)
        template.made.append(unit.name)
        unit_gloms.append(unit)
    template_glom = cy.make_glom(world, ("template",), unit_gloms, pseudo=True)
    template.glom = template_glom
    template.units = tuple(x.name for x in unit_gloms)
    template.coverage = fit.coverage
    world.events.publish(events.TemplateGlomMade(form_text(form), template.units,
                                                 template_glom.name))
    world.events.publish(events.TopDownGlom(template_glom.name, template.units,
                                            template_glom.print_value, True))
    world.template = template
    world.events.publish(events.TemplateMade(form_text(form), template.state,
                                             template.coverage, template_glom.name, from_glom,
                                             immediate))
    if immediate:
        world.post("Template-evaluator", None)
    else:
        world.post("Template-evaluator")
    return template


def check_text(template: Template) -> str:
    """The Template-evaluator's printed check for a fence-class template, named after the
    class and showing the template glom's print-value: `check-cycle template (Cycle 3 2
    ((3 7)))` [M86 p. 167]; nothing for a group class (`Template-evaluator on nil` prints
    no check for `(S-group 1 2)` [M86 p. 171]) (DECISION D-027)."""
    form = template.form
    if form.kind not in LIST_TYPES or template.glom is None:
        return ""
    return (f"check-{form.kind.lower()} template ({form.kind} {form_text(form.start)} "
            f"{form_text(form.length)} {value_text(template.glom.print_value)})")


def abandon(world, template: Template, reason: str, by: str) -> None:
    """Abandon a template: "rejected and abandoned" [M86 p. 74], "target it for abandoning"
    [M86 p. 75]. The template glom is dissolved, so that "the gloms that engendered and now
    reflect it" [M86 p. 73] float free again, and the unit gloms the template itself made
    are burst (DECISION D-061); the engendering glom is left alone (the resolver's failure,
    which dissolves it, is its own case [M86 p. 75])."""
    _release(world, template)
    world.events.publish(events.TemplateAbandoned(form_text(template.form), reason, by))


def extend(world, template: Template, members: list) -> None:
    """Put one more complete unit under the template glom, when the terms filtered past the
    template complete a unit (DECISION D-061): the unit's top-down glom is made and dubbed
    like the others, the template glom grows over it in place, and the coverage follows."""
    if len(members) == 1 and not members[0].is_glint:
        unit = members[0]
    else:
        unit = cy.make_glom(world, ("top-down",), members, pseudo=True)
        world.events.publish(events.TopDownGlom(unit.name, tuple(x.name for x in members),
                                                unit.print_value, True))
        gloms.dub(world, unit, template.form.kind, pl.purity(unit.flat), scout=False)
        template.made.append(unit.name)
    glom = template.glom
    if unit.start_position != glom.end_position + 1:
        raise ValueError(f"{unit.name} is not next to the template glom")
    unit.parent = glom
    glom.subgloms = glom.subgloms + (unit,)
    glom.print_value = glom.print_value + (unit.print_value,)
    glom.span = unit.end_position - glom.start_position + 1
    template.units = template.units + (unit.name,)
    template.coverage = (template.coverage[0], unit.end_position)


# ---- the tasks ---------------------------------------------------------------------------------

def template_scout(world, name: str) -> None:
    """The Template-scout task, set out by every dubbing [M86 pp. 69, 73]: "When invoked,
    the scout would set out a Template-applier task to determine if the entire sequence seen
    is of that form" [M86 p. 74], one per dubbing not yet taken up, `Template-applier on
    (glom1 (Cycle 3 2 (3 7)))` [M86 p. 165]. When the dubbed glom is itself the entire
    sequence, the scout applies the form at once, as the third run shows for `(1 2)`
    [M86 pp. 170-171] (DECISION D-060). While a template is alive the scout does nothing:
    "no new templates are considered for the lifetime of the given template" [M86 p. 74]."""
    glom = world.find(name)
    if not isinstance(glom, Glom) or not glom.dubbed:
        world.stale("Template-scout", (name,), "the glom is gone or undubbed")
        return
    if world.template is not None and world.template.alive:
        world.stale("Template-scout", (name,), "a template is alive")
        return
    forms = [form for form, _ in glom.structure[glom.scouted:]]
    glom.scouted = len(glom.structure)
    if not forms:
        world.stale("Template-scout", (name,), "every dubbing has been scouted")
        return
    whole = glom.start_position == 1 and glom.end_position == len(world.terms)
    for form in forms:
        if whole:
            if world.template is None or not world.template.alive:
                apply_form(world, glom, form, immediate=True)
        else:
            world.post("Template-applier", name, form)


def template_applier(world, name: str, form: Form) -> None:
    """The Template-applier task, `Template-applier on (glom1 (Cycle 3 2 (3 7)))`
    [M86 p. 165]: "The applier would attempt to view the sequence as a repetition of
    '(S-group 1 2)' and would, of course, fail because of the initial '1' and the trailing
    '3'. The applier does not give up immediately, but rather checks to see if loosening a
    parameter or two in its representation would help" [M86 p. 74] (DECISION D-026). "If the
    Template-applier fails, it creates a Template-resolver task" [M86 p. 75]. Nothing is
    done when the glom is gone, no longer dubbed so, or a template is alive."""
    glom = world.find(name)
    if not isinstance(glom, Glom) or form not in [f for f, _ in glom.structure or ()]:
        world.stale("Template-applier", (name, form), "the glom is gone or not dubbed so")
        return
    if world.template is not None and world.template.alive:
        world.stale("Template-applier", (name, form), "a template is alive")
        return
    apply_form(world, glom, form)


def apply_form(world, glom: Glom, form: Form, immediate: bool = False) -> Template | None:
    """Try the form and then its loosenings on the lineup; the first that fits with a
    complete unit becomes the template, "The accepted template will then be '(S-group 1 n)'"
    [M86 p. 74]; otherwise a Template-resolver is set out "for one final attempt"
    [M86 p. 75] (DECISION D-026). `immediate` marks the Template-scout's own application
    (DECISION D-060)."""
    line = cy.lineup(world)
    for candidate in [form] + loosenings(form):
        fit = fit_lineup(candidate, line)
        if fit is not None and fit.units:
            return make_template(world, candidate, glom.name, fit, immediate)
    world.post("Template-resolver", glom.name, form)
    return None


def template_resolver(world, name: str, form: Form) -> None:
    """The Template-resolver task: "the resolver looks at glints rather than gloms to
    determine whether or not the sequence can be re-viewed to fit into the given mold"
    [M86 p. 75]. "If the Template-resolver is successful, it 'blasts' (does an immediate
    burst of) all gloms and has the glints reglommed to fit the template" [M86 p. 75]: every
    free real glom is burst (pseudo-gloms are not, DECISION D-029), the glints are read as
    units and the template made over them. "If the proposed template does not work at term
    level, it is forgotten and the engendering glom dissolved" [M86 p. 75]."""
    glom = world.find(name)
    if world.template is not None and world.template.alive:
        world.stale("Template-resolver", (name, form), "a template is alive")
        return
    glints = cy.glints(world)
    for candidate in [form] + loosenings(form):
        fit = fit_lineup(candidate, glints)
        if fit is not None and fit.units:
            burst = []
            for element in cy.lineup(world):
                if not element.is_glint and not element.pseudo:
                    cy.burst(world, element)
                    burst.append(element.name)
            world.events.publish(events.Blast(tuple(burst)))
            make_template(world, candidate, name, fit_lineup(candidate, cy.lineup(world)))
            return
    if isinstance(glom, Glom) and glom.parent is None and not glom.pseudo:
        cy.dissolve(world, glom)
        world.events.publish(events.GlomDissolved(name, tuple(s.name for s in glom.subgloms)))
    world.stale("Template-resolver", (name, form), "the template does not work at term level")


def template_evaluator(world, *args) -> None:
    """The Template-evaluator task: "The template will be checked by a Template-evaluator
    task and either be passed, or rejected and abandoned. If it is passed, it will probably
    be the basis for hypothesis and gnoth creation" [M86 p. 74] (DECISION D-027): the
    template passes when its glom still exists and the lineup still fits its form; a
    fence-class template prints `check-cycle template (Cycle 3 2 ((3 7)))` [M86 p. 167].
    Passing is acceptance, which "sets off two tasks, a Gnoth-maker and a Hypothesizer"
    [M86 pp. 75-76] and "causes increased activity in the system" [M86 p. 75]
    (`Config.sparklers_on_acceptance` Sparklers, DECISION D-061)."""
    del args
    template = world.template
    if template is None or template.state != "working":
        world.stale("Template-evaluator", (), "no working template")
        return
    glom_present = (template.glom is not None
                    and world.cytoplasm.find(template.glom.name) is template.glom)
    if not glom_present:
        world.events.publish(events.TemplateChecked(template.form.kind, False, ""))
        abandon(world, template, "the template glom is gone", "Template-evaluator")
        return
    if not fits(template.form, cy.lineup(world), template):
        world.events.publish(events.TemplateChecked(template.form.kind, False, ""))
        abandon(world, template, "the terms no longer fit", "Template-evaluator")
        return
    world.events.publish(events.TemplateChecked(template.form.kind, True, check_text(template)))
    template.state = "accepted"
    world.events.publish(events.TemplateAccepted(form_text(template.form)))
    world.post("Gnoth-maker")
    world.post("Hypothesizer")
    for _ in range(world.config.sparklers_on_acceptance):
        world.post("Sparkler")


def template_reviewer(world, *args) -> None:
    """The Template-reviewer task: "It is invoked after a template has been created (but no
    hypothesis exists) and when new terms fail to fit the template. It can try some very
    simple fixes and can either: 1) call for modification of the template and restart the
    creation and evaluation processes; 2) leave the template alone; 3) target it for
    abandoning" [M86 p. 75] (DECISION D-028): a loosening of the form that fits the lineup
    modifies the template (the old template glom is dissolved and the template re-made and
    re-evaluated); else a last term that could begin a new unit leaves it alone; else it is
    abandoned."""
    del args
    template = world.template
    if template is None or not template.alive:
        world.stale("Template-reviewer", (), "no template to review")
        return
    if world.hypothesis is not None:
        world.stale("Template-reviewer", (), "a hypothesis exists")
        return
    line = cy.lineup(world)
    for candidate in loosenings(template.form):
        fit = fit_lineup(candidate, line, template)
        if fit is not None and fit.units:
            old = form_text(template.form)
            world.events.publish(events.TemplateModified(old, form_text(candidate)))
            _release(world, template)
            make_template(world, candidate, template.from_glom,
                          fit_lineup(candidate, cy.lineup(world)))
            return
    if world.terms and begins_unit(template.form, world.terms[-1]):
        world.stale("Template-reviewer", (), "the template is left alone")
        return
    abandon(world, template, "the new term does not fit", "Template-reviewer")


def _release(world, template: Template) -> None:
    """Undo a template's gloms before it is re-made looser: the template glom is dissolved
    and the unit gloms it made from above are burst, so that the glints can be cut into
    units afresh; bottom-up gloms stay (DECISION D-028)."""
    template.state = "abandoned"
    if template.glom is not None and world.cytoplasm.find(template.glom.name) is template.glom:
        cy.dissolve(world, template.glom)
    for name in template.made:
        unit = world.cytoplasm.find(name)
        if isinstance(unit, Glom) and unit.parent is None:
            cy.burst(world, unit)
    if world.template is template:
        world.template = None


def filter_term(world):
    """Filter the newest term past the template: "until a hypothesis exists, all new terms
    will be 'filtered' past the template, checked for agreement with it. Should a term not
    fit the template, a review is set up" [M86 p. 74]. Returns the `TemplateFiltered` event
    published, or None when there is no live template or a hypothesis exists. A term that
    completes a unit extends the template glom over it (DECISION D-061); a term that does
    not fit sets out a Template-reviewer [M86 p. 75]."""
    template = world.template
    if template is None or not template.alive or world.hypothesis is not None:
        return None
    if template.glom is None or world.cytoplasm.find(template.glom.name) is not template.glom:
        return None     # the Gnoth-maker has taken the template glom (DECISION D-062)
    if not world.terms:
        return None
    n, term = len(world.terms), world.terms[-1]
    fit = fit_lineup(template.form, cy.lineup(world), template)
    ok = fit is not None and bool(fit.units)
    if ok:
        for members in fit.units:
            if members[0].start_position == template.glom.end_position + 1:
                extend(world, template, members)
    event = events.TemplateFiltered(n, term, ok)
    world.events.publish(event)
    if not ok:
        world.post("Template-reviewer")
    return event


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Template-scout": template_scout,
    "Template-applier": template_applier,
    "Template-resolver": template_resolver,
    "Template-evaluator": template_evaluator,
    "Template-reviewer": template_reviewer,
}

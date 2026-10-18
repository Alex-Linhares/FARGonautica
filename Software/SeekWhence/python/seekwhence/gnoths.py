"""Gnoths, the Gnoth-maker and the Hypothesizer: the socratoplasm's structures and the end of
stage one [M86 pp. 76-79, 94-98].

"Each gnoth, a member of the class Gnoths, is viewed in three different ways: 1) it is an
actualization of a Platonic class; 2) it has an underlying glom collection from which it
derives its structure; 3) it represents one 'hit' of the current hypothesis (if there is
one)" [M86 p. 77]. "When a Gnoth-maker task, set off by the system after template creation,
is invoked, it creates one gnoth for each subglom of the template-glom and notifies the
associated plato-classes of their existence" [M86 p. 77]; "Each gnoth places a pseudo-glom
(called a 'gnoth-glom') over its glom collection (which contains just one glom initially)"
[M86 p. 78]. Confirming terms go to "a catchall gnoth, a special gnoth that simply serves as a
repository for non-troublesome terms" [M86 p. 98] (DECISION D-030). A gnoth stands to a
hypothesis frame in one of three equivalence relations, term, parse and structural
[M86 pp. 94-97]; the third, form polishing, is "unimplemented as yet" [M86 p. 97], so it is a
check here and nothing more (DECISION D-051). "When a template is accepted, a Hypothesizer
task is set off along with a Gnoth-maker" [M86 p. 79]; the Hypothesizer devises the
hypothesis "based on the template and the existing gnoths (if any) and gloms" [M86 p. 79],
with the Tuple fall-back (DECISION D-031).

The gnoth's fields and the `show` block are those of the thesis's example [M86 p. 78] and
the transcripts [M86 pp. 168, 175, 179]. Nothing here draws from the `Rng`. The gnoth
operations, the Gnoth-setter and the casting belong to the reformulation modules.
"""

from dataclasses import dataclass

from seekwhence import cytoplasm as cy
from seekwhence import events
from seekwhence import platoplasm as pl
from seekwhence.cytoplasm import Glom
from seekwhence.forms import (GROUP_TYPES, Form, Shared, flatten, form_text, has_wildcard,
                              is_form, is_int, is_wild, value_text)
from seekwhence.hypothesis import Hypothesis

# The one state the transcripts print [M86 pp. 168, 175]; no other is described.
STATES = ("stable",)

# The three levels of gnoth-hypothesis equivalence, weakest first [M86 pp. 94-97].
EQUIVALENCE_TYPES = ("term", "parse", "structural")


@dataclass(eq=False)
class Gnoth:
    """A gnoth, "a socratoplasm-level structure representing a logical grouping of terms in
    the system's parenthesization or parse of a sequence" [M86 p. 180], with the fields of
    the Gnoth-maker's example [M86 p. 78] and of `show` [M86 pp. 168, 175]: `class`
    (`Gnoths`), `name`, `frame` (which hit of the hypothesis it holds; the program counts
    from 0, `frame: 0` [M86 p. 168]), `plato_class` (the class it actualizes), `glom` (its
    gnoth-glom, "the gnoth's 'pseudo-glom'" [M86 p. 78]), `notes` (`nil` in every
    transcript), `form` (the underlying glom's structure field, `(((Cycle 3 2 (3 7)) pure))`
    [M86 p. 168], or a form after reformulation [M86 p. 175]), `state` (`stable`) and
    `range` ("the sequence terms it 'covers'" [M86 p. 78], first and last position). The
    four fields `show` adds after a reformulation, `external_bonds`, `internal_bonds`,
    `equivalence_type` and `groups` [M86 p. 175], are None until a later stage sets them and
    are printed only then. A `catchall` gnoth holds `glints` instead and has no class, frame
    or gnoth-glom (DECISION D-030)."""

    name: str
    frame: int | None = None
    plato_class: str | None = None
    glom: Glom | None = None
    notes: object = None
    form: object = None
    state: str = "stable"
    range: tuple = (0, 0)
    external_bonds: object = None
    internal_bonds: object = None
    equivalence_type: str | None = None
    groups: object = None
    catchall: bool = False
    glints: list = None

    cls = "Gnoths"

    def __post_init__(self):
        if self.glints is None:
            self.glints = []

    @property
    def collection(self) -> tuple:
        """The gnoth's "underlying glom collection from which it derives its structure"
        [M86 p. 77]: the subgloms of its gnoth-glom, left to right; for a catchall, the
        top-level elements that hold its glints (a frame glom the Hfilter has put over a
        confirmed frame counts once, DECISION D-063)."""
        if self.catchall:
            tops: list = []
            for glint in self.glints:
                top = glint
                while top.parent is not None:
                    top = top.parent
                if not tops or tops[-1] is not top:
                    tops.append(top)
            return tuple(tops)
        if self.glom is None:
            return ()
        return tuple(self.glom.subgloms)

    @property
    def terms(self) -> tuple:
        """The terms the gnoth covers, flattened, left to right [M86 p. 78]."""
        return tuple(flatten([x.print_value for x in self.collection]))

    @property
    def parse_print(self) -> tuple:
        """The gnoth's value, "the parenthesized printing of its value" [M86 p. 97]: the list
        of its collection's print-values, `((1 2) 3)` for a gnoth over `glom1 (1 2)` and
        `glint3`, `(1 2 3)` for one over three glints [M86 p. 95] (DECISION D-051)."""
        return tuple(x.print_value for x in self.collection)

    def show_fields(self) -> tuple:
        """The fields `show` prints, in its order, as (field, text) pairs: the four
        reformulation fields first when they are set [M86 p. 175], then class, name, frame,
        plato-class, glom, notes, form, state, range [M86 p. 168]."""
        out = []
        if self.external_bonds is not None:
            out.append(("external-bonds", _bonds_text(self.external_bonds)))
        if self.internal_bonds is not None:
            out.append(("internal-bonds", _bonds_text(self.internal_bonds)))
        if self.equivalence_type is not None:
            out.append(("equivalence-type", self.equivalence_type))
        if self.groups is not None:
            out.append(("groups", _groups_text(self.groups)))
        out.extend([
            ("class", self.cls),
            ("name", self.name),
            ("frame", "nil" if self.frame is None else str(self.frame)),
            ("plato-class", self.plato_class or "nil"),
            ("glom", self.glom.name if self.glom is not None else "nil"),
            ("notes", "nil" if self.notes is None else str(self.notes)),
            ("form", _form_field_text(self.form)),
            ("state", self.state),
            ("range", f"({self.range[0]} {self.range[1]})"),
        ])
        return tuple(out)

    def show_lines(self) -> list:
        """The gnoth's block of `show`, one `field: value` line per field [M86 p. 168]."""
        return [f"{field}: {text}" for field, text in self.show_fields()]


def _form_field_text(form) -> str:
    """The `form` field as printed: a structure field as a list of `(form purity)` pairs,
    `(((Cycle 3 2 (3 7)) pure))` [M86 p. 168]; a bare form `(S-group 1 2)` [M86 p. 175];
    `nil` when empty."""
    if form is None:
        return "nil"
    if is_form(form):
        return form_text(form)
    pairs = " ".join(f"({form_text(f)} {purity})" for f, purity in form)
    return f"({pairs})"


def _bonds_text(bonds) -> str:
    """A bond list with values as printed, `((bond2 0) (bond3 -5))` [M86 p. 175], `nil` when
    empty."""
    if not bonds:
        return "nil"
    items = []
    for bond, value in bonds:
        name = getattr(bond, "name", bond)
        items.append(f"({name} {_num(value)})")
    return "(" + " ".join(items) + ")"


def _num(value) -> str:
    return str(int(value)) if value == int(value) else str(value)


def _groups_text(groups) -> str:
    """The `groups` field as printed, `((glint1 glint2))` [M86 p. 168], `nil` when empty."""
    if not groups:
        return "nil"
    return "(" + " ".join("(" + " ".join(getattr(x, "name", str(x)) for x in group) + ")"
                          for group in groups) + ")"


# ---- the socratoplasm's views ----------------------------------------------------------------

def gnoths(world) -> list:
    """The gnoths of the parse, in sequence order (which is frame order; two gnoths may
    carry the same frame number after a reformulation, `frame: 2` for `gnoth3` and
    `gnoth5` [M86 pp. 175, 179]), the catchall left out: what `show` lists under
    `gnoths:` [M86 pp. 168, 175] (DECISION D-030)."""
    listed = [g for g in world.socratoplasm if isinstance(g, Gnoth) and not g.catchall]
    return sorted(listed, key=lambda g: (g.range[0], g.frame if g.frame is not None else 0))


def catchall(world) -> Gnoth | None:
    """The catchall gnoth, "a rightmost or 'trailer' gnoth that simply holds input terms
    that agree with the hypothesis without parenthesizing them" [M86 p. 180], or None
    (DECISION D-030)."""
    for g in world.socratoplasm:
        if isinstance(g, Gnoth) and g.catchall:
            return g
    return None


def parse_text(world) -> str:
    """The parse as `show-parse` prints it, `((1 2) (1))` [M86 p. 174]: the gnoths'
    parse-prints in frame order, in one pair of parentheses (DECISION D-051)."""
    return "(" + " ".join(value_text(g.parse_print) for g in gnoths(world)) + ")"


def show_gnoths_lines(world) -> list:
    """The `gnoths:` block of `show`: the heading, a blank line, and each gnoth's fields,
    gnoths separated by a blank line [M86 pp. 168, 179]; the catchall is not listed
    (DECISION D-030)."""
    lines = ["gnoths:", ""]
    for i, gnoth in enumerate(gnoths(world)):
        if i:
            lines.append("")
        lines.extend(gnoth.show_lines())
    return lines


# ---- making gnoths ------------------------------------------------------------------------------

def make_gnoth(world, kind: str, members, frame: int) -> Gnoth:
    """Make one gnoth over a glom collection: its gnoth-glom, a pseudo-glom, is put over the
    members ("Each gnoth places a pseudo-glom (called a 'gnoth-glom') over its glom
    collection" [M86 p. 78]) and printed `top-down glom glom3` [M86 p. 167]; the gnoth
    takes the class as its `plato-class`, the frame, the members' span as its `range` and
    the first member's structure field as its `form` (`(((Cycle 3 2 (3 7)) pure))`
    [M86 p. 168]); it is listed as an actualization of the class ("notifies the associated
    plato-classes of their existence" [M86 p. 77])."""
    members = tuple(members)
    glom = cy.make_glom(world, ("gnoth",), members, pseudo=True)
    world.events.publish(events.TopDownGlom(glom.name, tuple(x.name for x in members),
                                            glom.print_value, True))
    form = None
    if len(members) == 1 and not members[0].is_glint and members[0].structure:
        form = list(members[0].structure)
    gnoth = Gnoth(world.name("gnoth"), frame, kind, glom, form=form,
                  range=(glom.start_position, glom.end_position))
    world.socratoplasm.add(gnoth)
    world.platoplasm.actualize(kind, gnoth)
    world.events.publish(events.GnothMade(gnoth.name, frame, kind, glom.name, gnoth.range,
                                          tuple(x.name for x in members)))
    return gnoth


def make_catchall(world, glints) -> Gnoth:
    """Make the catchall gnoth over the given glints, in sequence order (DECISION D-030):
    no class, frame or gnoth-glom; `show` does not list it [M86 pp. 168, 175]."""
    glints = sorted(glints, key=lambda g: g.start_position)
    gnoth = Gnoth(world.name("gnoth"), catchall=True, glints=list(glints))
    if glints:
        tops = gnoth.collection
        gnoth.range = (tops[0].start_position, tops[-1].end_position)
    world.socratoplasm.add(gnoth)
    world.events.publish(events.CatchallMade(gnoth.name, tuple(g.name for g in glints)))
    return gnoth


def destroy_gnoth(world, gnoth: Gnoth, keep_glom: bool = False) -> None:
    """Remove a gnoth from the socratoplasm and from the actualization lists; its gnoth-glom
    is dissolved unless `keep_glom` ("A gnoth ceases to exist only when its subglom
    collection is empty" [M86 p. 44]; how the reformulation empties one is its business)."""
    if gnoth.name in world.socratoplasm:
        world.socratoplasm.remove(gnoth.name)
    world.platoplasm.forget(gnoth)
    if not keep_glom and gnoth.glom is not None and gnoth.glom.name in world.cytoplasm:
        cy.dissolve(world, gnoth.glom)


def gnoth_maker(world, *args) -> None:
    """The Gnoth-maker task: "it creates one gnoth for each subglom of the template-glom and
    notifies the associated plato-classes of their existence" [M86 p. 77], as in the
    example of p. 78 (gnoths over `(1)`, `(1 2)`, `(1 2 3)` with frames 1-3 and ranges
    `(1 1)`, `(2 3)`, `(4 6)`; the program counts frames from 0 [M86 p. 168]). The template
    glom gives way to the gnoth-gloms (`show` lists the gnoth-glom `glom3` over `glom1` and
    no `glom2` [M86 pp. 167-168]; DECISION D-062), each printed `top-down glom glomN`, then
    `gnoths constructed` [M86 p. 167]. The terms beyond the template's coverage, which the
    template filter let pass, stay free in the cytoplasm: the catchall is made by the
    Hfilter at the first confirming term (DECISIONS D-030, D-062, D-063). When the
    Hypothesizer has already run, the Gnoth-maker sets out the Call-term, so that the prompt
    comes after both, as in every transcript [M86 pp. 165, 171] (DECISION D-012). Nothing
    is done without an accepted template whose glom is present, or when gnoths exist
    already."""
    del args
    template = world.template
    if template is None or template.state != "accepted":
        world.stale("Gnoth-maker", (), "no accepted template")
        return
    glom = template.glom
    if glom is None or world.cytoplasm.find(glom.name) is not glom:
        world.stale("Gnoth-maker", (), "the template glom is gone")
        return
    if gnoths(world):
        world.stale("Gnoth-maker", (), "gnoths exist already")
        return
    units = cy.dissolve(world, glom)
    template.glom = None
    made = []
    for frame, unit in enumerate(units):
        made.append(make_gnoth(world, template.form.kind, [unit], frame))
    world.events.publish(events.GnothsConstructed(tuple(g.name for g in made)))
    if world.hypothesis is not None:
        world.post("Call-term")     # the Hypothesizer ran first (DECISION D-012)


# ---- equivalence: term, parse, structural [M86 pp. 94-97] ---------------------------------

def term_equivalent(gnoth: Gnoth, frame) -> bool:
    """Term equivalence, "the weakest of the three types of representation, requires that
    each gnoth govern precisely the same terms as one frame of the hypothesis" [M86 p. 95]:
    the flattened terms agree."""
    return gnoth.terms == tuple(flatten(frame))


def parse_equivalent(gnoth: Gnoth, frame) -> bool:
    """Parse equivalence "requires that the gnoth print its value with the same
    parenthesization as the corresponding hypothesis frame" [M86 p. 95]: the gnoth's
    parse-print equals the frame as a list (DECISION D-051). "gnoth3 is parse-equivalent to
    the given hypothesis while gnoth2 is not" [M86 p. 95]."""
    return gnoth.parse_print == _as_list(frame)


def structural_equivalent(gnoth: Gnoth, frame, deep) -> bool:
    """Structural equivalence: "a gnoth must be parse-equivalent to it and the gnoth's form
    must be the same as the corresponding frame of the hypothesis" [M86 pp. 95-96], the
    frame in its deep-structure form (`deep_frame`, an s-expression). Form polishing, which
    would make gnoths reach it, is "unimplemented as yet" [M86 p. 97], so this is a check
    only (DECISION D-051). The gnoth's form is read as its first dubbed form when it holds a
    structure field."""
    if not parse_equivalent(gnoth, frame):
        return False
    form = gnoth.form
    if form is not None and not is_form(form):
        form = form[0][0] if form else None
    return form is not None and to_sexp(form) == tuple(deep)


def equivalence_type(gnoth: Gnoth, frame) -> str | None:
    """The mark the Gnoth-setter gives a gnoth, "term or parse, depending on agreement with
    the hypothesis' parenthesization" [M86 p. 98]: `parse` when parse-equivalent, `term`
    when term-equivalent only, None when the terms differ (DECISION D-051). Structural
    equivalence is never marked [M86 p. 97]."""
    if parse_equivalent(gnoth, frame):
        return "parse"
    if term_equivalent(gnoth, frame):
        return "term"
    return None


def _as_list(frame) -> tuple:
    return tuple(frame) if isinstance(frame, (tuple, list)) else (frame,)


def to_sexp(form: Form) -> tuple:
    """A form as a nested tuple in the program's spelling, `("C-group", ("S-group", 1, 3),
    2)`, the shape in which the frames of pp. 96 are compared (DECISION D-051); a number
    stays a number, a `Shared` or wildcard is kept as it is."""
    if not is_form(form):
        return form
    if form.kind in GROUP_TYPES:
        return (form.kind, to_sexp(form.start), to_sexp(form.length))
    if form.kind in ("Constant", "Countup"):
        return (form.kind, to_sexp(form.start))
    return (form.kind, form.start, form.length, tuple(to_sexp(x) for x in form.actual or ()))


def sexp_text(sexp) -> str:
    """An s-expression printed as the thesis prints a frame, `(C-group (S-group 1 3) 2)`,
    `(C-group (1 2 3) 2)` [M86 p. 96]."""
    if isinstance(sexp, tuple):
        return "(" + " ".join(sexp_text(x) for x in sexp) + ")"
    if isinstance(sexp, Shared):
        return "shared"
    return str(sexp)


def deep_frame(form: Form, k: int) -> tuple:
    """The deep-structure form of the k-th frame (from 1) of a hypothesis, as an
    s-expression: "More structural detail is retained in the deep-structure form, with only
    the lowest-level structures replaced by constants or runs" [M86 p. 96]; for
    `(C-group (S-group (Countup 1) 3) 2)` the first three frames are `(C-group (S-group 1 3)
    2)`, `(C-group (S-group 2 3) 2)`, `(C-group (S-group 3 3) 2)` [M86 p. 96]. A lowest-level
    structure (one whose arguments are all numbers) is replaced by its value at the k-th
    frame, read as its k-th hit alone (a Countup inside a group advances once per hit of
    the whole, DECISION D-003)."""
    return _replace(form, k, deep=True)


def shallow_frame(form: Form, k: int) -> tuple:
    """The shallow-structure form of the k-th frame, as an s-expression: "In the
    shallow-structure form, all but the top-level structures are so replaced" [M86 p. 96]:
    `(C-group (1 2 3) 2)`, `(C-group (2 3 4) 2)`, `(C-group (3 4 5) 2)` for the same
    hypothesis [M86 p. 96]."""
    return _replace(form, k, deep=False)


def _replace(form: Form, k: int, deep: bool) -> tuple:
    def arg(x):
        if not is_form(x):
            return x
        if deep and not _lowest(x):
            return _replace(x, k, deep)
        value = pl.hits(x, k)[-1]
        return value if is_int(value) else tuple(value)
    if form.kind in GROUP_TYPES:
        return (form.kind, arg(form.start), arg(form.length))
    if form.kind in ("Constant", "Countup"):
        return (form.kind, arg(form.start))
    return (form.kind, form.start if is_int(form.start) else None,
            form.length if is_int(form.length) else None,
            tuple(arg(x) for x in form.actual or ()))


def _lowest(form: Form) -> bool:
    return not any(is_form(x) for x in form.elements)


# ---- the Hypothesizer [M86 p. 79] -----------------------------------------------------------

def generalise(values) -> object:
    """Generalise a parameter over the values it takes in successive units: equal values
    stay, values counting up by one become `(Countup first)`, anything else is None
    (DECISION D-031, the rule of DECISION D-048 for the Gnoth-caster, which may reuse it)."""
    values = list(values)
    if not values:
        return None
    if all(v == values[0] for v in values):
        return values[0]
    if all(b == a + 1 for a, b in zip(values, values[1:])):
        return Form("Countup", values[0])
    return None


def unit_values(world, template) -> list:
    """The terms of each complete unit of the template, from the unit gloms it stands on
    (the gnoths' gloms when gnoths exist): what the Hypothesizer reads, "based on the
    template and the existing gnoths (if any) and gloms" [M86 p. 79]."""
    out = []
    for name in template.units:
        unit = world.cytoplasm.find(name)
        if unit is None:
            return []
        out.append(tuple(unit.flat_values()))
    return out


def form_from_template(world, template) -> Form | None:
    """The Hypothesizer's form from the template (DECISION D-031): (1) a template without a
    wildcard that has repeated is the form itself (`(Cycle 3 2 (3 7))` [M86 pp. 165-166]);
    (2) a wildcard parameter is generalised over the units; (3) a template with a single
    complete hit of a generate class becomes its repeater, `(Countup 1)` from
    `(S-group 1 2)` ("'1 2' -- causes the system to hypothesize (Countup 1)" [M86 p. 98],
    [M86 p. 171]), a P-group or a fence form stays; None when nothing fits."""
    form = template.form
    units = unit_values(world, template)
    if not units:
        return None
    if has_wildcard(form):
        start = form.start
        if is_wild(start):
            start = generalise(u[0] for u in units)
        length = form.length
        if is_wild(length):
            length = generalise(len(u) for u in units)
        if start is None or length is None:
            return None
        return Form(form.kind, start, length)
    if len(units) == 1:
        if form.kind == "S-group":
            return Form("Countup", form.start)
        if form.kind == "C-group":
            return Form("Constant", form.start)
    return form


def fallback_form(terms) -> Form:
    """"the fall-back position of declaring the sequence to be a Tuple, the weakest of all
    plato-classes" [M86 p. 79]: `(Tuple 1 k (all the terms))` (DECISION D-031)."""
    terms = tuple(terms)
    return Form("Tuple", terms[0], len(terms), terms)


def hypothesizer(world, *args) -> None:
    """The Hypothesizer task: "responsible for devising a hypothesis for the sequence, based
    on the template and the existing gnoths (if any) and gloms. If, for some reason, there
    is a faulty template (or none at all), the Hypothesizer can take the fall-back position
    of declaring the sequence to be a Tuple" [M86 p. 79]. The form comes from the template
    (`form_from_template`, DECISION D-031) and must postdict the terms (DECISION D-034), else
    the fall-back is used; the hypothesis is printed as its form, `(Countup 1)` [M86 p. 171];
    the template-level tasks are cleaned (DECISION D-010) and a Call-term set out, unless a
    Gnoth-maker is still waiting, which then sets it out itself (DECISION D-012: the prompt
    follows both tasks in every transcript [M86 pp. 165, 171]). Nothing is done when a
    hypothesis exists already."""
    del args
    if world.hypothesis is not None:
        world.stale("Hypothesizer", (), "a hypothesis exists")
        return
    if not world.terms:
        world.stale("Hypothesizer", (), "no terms")
        return
    template = world.template
    form = None
    source = "fallback"
    if template is not None and template.state == "accepted":
        form = form_from_template(world, template)
        source = "template"
    hypothesis = None
    if form is not None:
        hypothesis = Hypothesis(form, world.counters)
        if not hypothesis.postdict(world.terms):
            hypothesis = None
    if hypothesis is None:
        source = "fallback"
        hypothesis = Hypothesis(fallback_form(world.terms), world.counters)
        hypothesis.postdict(world.terms)
    world.hypothesis = hypothesis
    world.clean("hypothesis")
    world.events.publish(events.HypothesisMade(hypothesis.show_form(), source))
    if not world.taskrack.count_by_type().get("Gnoth-maker"):
        world.post("Call-term")     # else the waiting Gnoth-maker sets it out (DECISION D-012)


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Gnoth-maker": gnoth_maker,
    "Hypothesizer": hypothesizer,
}

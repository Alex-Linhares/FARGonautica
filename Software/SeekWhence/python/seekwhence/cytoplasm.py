"""The four data types of the cytoplasm: glints, gloms, sparks and bonds [M86 pp. 60-65].

"The four data types residing in the cytoplasm are Sparks, Bonds, Glints, and Gloms. The
former two classes are for finding, proposing, and later evaluating glom groupings. The latter
two classes, the Glints and Gloms, are used for representing the sequence terms and any term
groupings of current interest" [M86 pp. 60-61]. This module holds those four structures with
the fields the thesis lists [M86 pp. 61-62], the questions the cyto tasks ask of them (is one
a subglom of the other, are they adjacent, are their print-values the same or successive), and
the two ways a glom disappears short of glomming, dissolving and bursting [M86 pp. 62, 70].
The tasks that make sparks and bonds are in `seekwhence.bonds`; the tasks that make, dub and
destroy gloms are in `seekwhence.gloms`. Nothing here draws from the `Rng`.

Every structure lives in the `World`'s cytoplasm `Arena` under its name (`glint3`, `glom7`,
`spark6`, `bond1`), numbered by the world's counters (DECISION D-014). A glint is a glom
("The class 'Glints' is a distinguished subclass of the class 'Gloms'" [M86 p. 62]), so
`Glint` and `Glom` share the base `CytoElement`, and the span of a glint is 1 [M86 p. 61].
"""

from dataclasses import dataclass, field

# The two bond-fields: "Typically, the print-value and span are useful bond-fields"
# [M86 p. 65]. "Only print-value is used as a glom-field in the current system" [M86 p. 65].
BOND_FIELDS = ("print-value", "span")
GLOM_FIELDS = ("print-value",)

# The bond types the reconstruction makes and the family of each (DECISION D-018): "sameness
# and successor-predecessorship" are "The most important" [M86 p. 64]; "the 'pred-succ' bond
# family" [M86 p. 67].
BOND_TYPES = ("Same", "Succ", "Pred")
FAMILIES = {"Same": "Same", "Succ": "pred-succ", "Pred": "pred-succ"}
MODIFIERS = ("adjacent", "remote")


# ---- glints and gloms --------------------------------------------------------------------

@dataclass(eq=False)
class CytoElement:
    """What a glint and a glom have in common [M86 pp. 61-62]: a `name`, a `print_value`
    (an integer for a glint, a nested tuple for a glom), a `start_position` (counted from
    1), a `span` ("the number of terms covered" [M86 p. 61]), the `bonds_in` ("the bonds
    attached", optional in the thesis [M86 pp. 61-62]), the `parent` glom this one is a
    subglom of (None when it floats free at the top of the cytoplasm), and `active`, whether
    it is now "a site of increased system activity" [M86 p. 67] (an active focus,
    [M86 p. 47]; DECISION D-024 says when that ends)."""

    name: str
    print_value: object
    start_position: int
    span: int
    bonds_in: list = field(default_factory=list)
    parent: object = None
    active: bool = False
    pseudo = False  # only a Glom can be a pseudo-glom (its field below shadows this)

    @property
    def is_glint(self) -> bool:
        """Whether this is a glint, the "'atomic' glom" [M86 p. 62]."""
        return isinstance(self, Glint)

    @property
    def end_position(self) -> int:
        """The position of the last term covered [M86 p. 62] (`positions-covered`'s second
        number)."""
        return self.start_position + self.span - 1

    @property
    def positions_covered(self) -> tuple:
        """`(first last)`, the glom field of [M86 p. 62]; for a glint both are its position."""
        return (self.start_position, self.end_position)

    @property
    def top_level(self) -> bool:
        """Whether the element "is not a subglom of any other glom" [M86 p. 66]."""
        return self.parent is None

    def flat_values(self) -> tuple:
        """The terms covered, left to right, as integers [M86 p. 62]: a glint's value, a
        glom's print-value flattened."""
        return _flatten(self.print_value)


@dataclass(eq=False)
class Glint(CytoElement):
    """A glint, "the cyto-level representation[ ] of the integers entered at the keyboard"
    [M86 p. 61], with the fields of the thesis's example: class `Glints`, `name` (`glint3`),
    `print-value` (2), `position` (3), `span` (1), `left-nbr` and `right-nbr` (`glint2`,
    `glint4`, the names of the neighbouring glints, or None at an end) and the optional
    `bonds-in` [M86 p. 61]. "Each glint is indestructible" [M86 p. 62]. `atom` is the ideal
    atom it is listed as a manifestation of [M86 p. 61]."""

    left_nbr: str | None = None
    right_nbr: str | None = None
    atom: object = None

    cls = "Glints"

    @property
    def position(self) -> int:
        """The glint's `position` field, its place in the sequence from 1 [M86 p. 61]."""
        return self.start_position

    def show_fields(self) -> tuple:
        """The fields in the thesis's order, as (field, value) pairs [M86 p. 61]."""
        return (("class", self.cls), ("name", self.name), ("print-value", self.print_value),
                ("position", self.position), ("span", self.span),
                ("left-nbr", self.left_nbr), ("right-nbr", self.right_nbr))


@dataclass(eq=False)
class Glom(CytoElement):
    """A non-glint glom, "cohesive units, made of adjacent atoms bound by 'bonds' of one type
    or another" [M86 p. 62], with the fields of the thesis's `glom7` example: class `Gloms`,
    `name`, `type` (`(Same print-value group)`: the bond family, the glom-field and the kind
    of grouping, SPEC 5.3), `print-value` (`(2 2 2)`), `start-position`, `span`,
    `positions-covered`, `subgloms`, and the optional `structure` (set by dubbing [M86 p. 69])
    and `bonds-in` [M86 p. 62]. A `pseudo` glom (the template's and the Gnoth-setter's
    top-down gloms) prints `pseudo` in place of its type [M86 pp. 167, 175]. `scouted`
    counts the dubbings a Template-scout has already taken up, so that a glom dubbed twice
    (each dubbing sets out a scout [M86 p. 69]) is not applied twice (DECISION D-060)."""

    type: tuple = ()
    subgloms: tuple = ()
    structure: object = None
    pseudo: bool = False
    scouted: int = 0

    cls = "Gloms"

    @property
    def flat(self) -> bool:
        """Whether the glom "has only glints as subgloms" [M86 p. 69]."""
        return all(sub.is_glint for sub in self.subgloms)

    @property
    def dubbed(self) -> bool:
        """Whether the glom has been dubbed: its `structure` field is filled [M86 p. 69]."""
        return self.structure is not None

    def type_text(self) -> str:
        """The `type` as the transcripts print it: `(Same print-value fence)` or `pseudo`
        [M86 p. 167]."""
        return "pseudo" if self.pseudo else "(" + " ".join(self.type) + ")"

    def show_fields(self) -> tuple:
        """The fields in the thesis's order, as (field, value) pairs [M86 p. 62]."""
        return (("class", self.cls), ("name", self.name), ("type", self.type_text()),
                ("print-value", self.print_value), ("start-position", self.start_position),
                ("span", self.span), ("positions-covered", self.positions_covered),
                ("subgloms", tuple(sub.name for sub in self.subgloms)))


def _flatten(value) -> tuple:
    if isinstance(value, int):
        return (value,)
    out: list = []
    for item in value:
        out.extend(_flatten(item))
    return tuple(out)


def is_subglom(inner: CytoElement, outer: CytoElement) -> bool:
    """Whether `inner` is a subglom of `outer`, at any depth: "no glom can be bonded to one
    of its own subgloms, so the glom '(1 (1 1))' cannot be bonded to the subglom '(1 1)' in
    any way" [M86 p. 63]."""
    parent = inner.parent
    while parent is not None:
        if parent is outer:
            return True
        parent = parent.parent
    return False


def nested(a: CytoElement, b: CytoElement) -> bool:
    """Whether either is a subglom of the other, which the Sparkler and the Tester refuse
    [M86 p. 63]."""
    return is_subglom(a, b) or is_subglom(b, a)


def left_to_right(a: CytoElement, b: CytoElement) -> tuple:
    """The two elements ordered by position, so that `Succ` and `Pred` have a direction
    (`bond1 Succ print-value (adjacent) -- (glint1 glint2)` for `1 2` [M86 p. 174])."""
    return (a, b) if a.start_position <= b.start_position else (b, a)


def adjacent(a: CytoElement, b: CytoElement) -> bool:
    """Whether the two elements are neighbours in the sequence: "adjacent terms are more
    strongly bonded than non-adjacent ones" [M86 pp. 64-65]; the modifier is `(adjacent)`
    or `(remote)` [M86 p. 174]."""
    left, right = left_to_right(a, b)
    return left.end_position + 1 == right.start_position


def modifier(a: CytoElement, b: CytoElement) -> str:
    """`adjacent` when the elements are neighbours, else `remote` [M86 p. 174]
    (DECISION D-018)."""
    return "adjacent" if adjacent(a, b) else "remote"


def make_glint(world, term: int) -> Glint:
    """"When a sequence term is entered, the system creates a glint for it and then lists
    that glint as a manifestation, or cyto-level analogue, of the appropriate ideal-atom in
    the platoplasm" [M86 p. 61]. The new glint is the active focus [M86 p. 47]; its
    `left-nbr` is the previous glint, whose `right-nbr` becomes the new one [M86 p. 61]."""
    position = len(world.terms)
    existing = glints(world)
    previous = existing[-1] if existing else None
    glint = Glint(world.name("glint"), term, position, 1, active=True)
    if previous is not None:
        glint.left_nbr = previous.name
        previous.right_nbr = glint.name
    glint.atom = world.platoplasm.add_manifestation(term, glint)
    world.cytoplasm.add(glint)
    return glint


def make_glom(world, type: tuple, subgloms, pseudo: bool = False) -> Glom:
    """Make a glom of the given subgloms (left to right) and put it in the cytoplasm: its
    print-value is the list of the subgloms' print-values, its start-position, span and
    positions-covered follow from theirs [M86 p. 62], and the subgloms become its parts
    (`parent`). The subgloms must be adjacent and free ("made of adjacent atoms"
    [M86 p. 62]). Who makes gloms, what they are told and what is printed is the business
    of `seekwhence.gloms` (the Glommer [M86 p. 67]) and of the template and gnoth tasks."""
    subgloms = tuple(subgloms)
    if not subgloms:
        raise ValueError("a glom needs at least one subglom")
    for before, after in zip(subgloms, subgloms[1:]):
        if before.end_position + 1 != after.start_position:
            raise ValueError(f"{before.name} and {after.name} are not adjacent")
    for sub in subgloms:
        if sub.parent is not None:
            raise ValueError(f"{sub.name} is already a subglom of {sub.parent.name}")
    start = subgloms[0].start_position
    span = subgloms[-1].end_position - start + 1
    glom = Glom(world.name("glom"), tuple(sub.print_value for sub in subgloms), start, span,
                type=tuple(type), subgloms=subgloms, pseudo=pseudo)
    for sub in subgloms:
        sub.parent = glom
    world.cytoplasm.add(glom)
    return glom


def top_level_elements(world, pseudo: bool = True) -> list:
    """The glints and gloms that are "not a subglom of any other glom" [M86 p. 66], in
    creation order; `pseudo=False` leaves the pseudo-gloms out, as the Sparkler's draw does
    (DECISION D-025)."""
    return [x for x in world.cytoplasm if isinstance(x, CytoElement) and x.top_level
            and (pseudo or not x.pseudo)]


def elements(world, pseudo: bool = True) -> list:
    """Every glint and glom of the cytoplasm at every level, in creation order: what the
    Sparkler-plus of a new element is posted against and what the Sparkler draws from, since
    the thesis forbids only a spark between a glom and its own subglom ("looks for gloms
    that are not subgloms of each other" [M86 p. 63]) and the transcripts spark a new glint
    against glints and gloms nested under others (`spark11 --- between glint1glint2` with
    both under glom1 [M86 p. 165]; `Sparkler-plus on (glint3 glint1 10)`, `(glom1 glint3 10)`
    with glint1 and glom1 under glom3 [M86 p. 172]). `pseudo=False` leaves the pseudo-gloms
    out (DECISIONS D-015, D-025)."""
    return [x for x in world.cytoplasm if isinstance(x, CytoElement)
            and (pseudo or not x.pseudo)]


def top_of(element: CytoElement) -> CytoElement:
    """The top-level element holding an element: the end of its parent chain, or the
    element itself when it is free (the target of a term's Dissolver, `Dissolver on (glom3)`
    with glint2 under glom1 under glom3 [M86 p. 172]; DECISION D-024)."""
    top = element
    while top.parent is not None:
        top = top.parent
    return top


def glints(world) -> list:
    """The glints in order of entry [M86 p. 61]."""
    return [x for x in world.cytoplasm if isinstance(x, Glint)]


def lineup(world) -> list:
    """The top-level elements in sequence order, left to right: the cytoplasm as the
    Glomtester and the Plato-evaluator read it when they measure "the extent of the evolving
    glom" [M86 p. 67]. Every term is under exactly one top-level element, so the lineup
    covers the sequence without gaps or overlaps; pseudo-gloms are in it (their members are
    not free), and the glom tasks refuse to build on them (DECISION D-021)."""
    return sorted(top_level_elements(world), key=lambda x: x.start_position)


def neighbours(world, element: CytoElement) -> tuple:
    """The elements on either side of a top-level element in the lineup, `(left, right)`,
    None at an end: the "neighbors" a Glom-scout "attempts to group it with" [M86 p. 66]
    (DECISION D-020: for a glint they must also be its `left-nbr`/`right-nbr` glints)."""
    line = lineup(world)
    index = next((i for i, x in enumerate(line) if x is element), None)
    if index is None:
        return (None, None)
    left = line[index - 1] if index > 0 else None
    right = line[index + 1] if index + 1 < len(line) else None
    return (left, right)


# ---- destruction -----------------------------------------------------------------------------

def detach(world, element: CytoElement) -> list:
    """Remove the bonds and sparks attached to an element that is being destroyed, from the
    cytoplasm and from the other member's `bonds_in`, and the element itself from the
    platoplasm's manifestation lists; returns the bonds and sparks removed
    (DECISION D-059)."""
    world.platoplasm.forget(element)
    removed = []
    for thing in list(world.cytoplasm):
        if isinstance(thing, (Bond, Spark)) and element in thing.members:
            world.cytoplasm.remove(thing.name)
            removed.append(thing)
            if isinstance(thing, Bond):
                for member in thing.members:
                    if thing in member.bonds_in:
                        member.bonds_in.remove(thing)
    return removed


def dissolve(world, glom: Glom) -> tuple:
    """Dissolve a glom: "being destroyed as a unit, but with all subgloms surviving intact"
    [M86 p. 62], "leaving its highest-level subgloms to float independently in the
    cytoplasm" [M86 p. 70]; returns the freed subgloms. A glint cannot be dissolved: "Each
    glint is indestructible" [M86 p. 62]. Its bonds and sparks go with it (DECISION D-059)."""
    if glom.is_glint:
        raise ValueError(f"{glom.name} is a glint, and glints are indestructible")
    if world.cytoplasm.find(glom.name) is not glom:
        raise ValueError(f"{glom.name} is not in cytoplasm")
    if glom.parent is not None:
        raise ValueError(f"{glom.name} is a subglom of {glom.parent.name}")
    detach(world, glom)
    world.cytoplasm.remove(glom.name)
    for sub in glom.subgloms:
        sub.parent = None
    glom.active = False
    return glom.subgloms


def burst(world, glom: Glom) -> tuple:
    """Burst a glom: "being destroyed as a unit and having all non-glint subgloms burst as
    well -- leaving only the underlying glints" [M86 p. 62]; returns those glints, left to
    right. The example: bursting `((1 1) (2 2))` leaves the glints `1`, `1`, `2`, `2`
    [M86 p. 70]."""
    freed = dissolve(world, glom)
    out: list = []
    for sub in freed:
        if sub.is_glint:
            out.append(sub)
        else:
            out.extend(burst(world, sub))
    return tuple(out)


# ---- sparks and bonds ------------------------------------------------------------------------

@dataclass(eq=False)
class Spark:
    """A spark, "created between two gloms when a Sparkler task pulls those gloms at random
    from the cytoplasm and determines in a very cursory way that the two structures might be
    amenable to bonding" [M86 p. 63]: a `name` (`spark6`) and its two `members`, printed
    `spark6 --- between glint3glint1` [M86 p. 167] in the order they were given."""

    name: str
    members: tuple

    def line(self) -> str:
        """`spark1 --- between glint1glint2`, as the transcripts print a new spark
        [M86 p. 164]."""
        a, b = self.members
        return f"{self.name} --- between {a.name}{b.name}"


@dataclass(eq=False)
class Bond:
    """A bond, "a Bond-class structure ... This bond, which exists in the cytoplasm, links
    the two gloms and has a strength associated with it" [M86 p. 64]: `name` (`bond1`),
    `type` (`Same`, `Succ`, `Pred`), `field` (`print-value` or `span`), `modifier`
    (`adjacent` or `remote`), `members` (left to right) and `strength` (DECISION D-019).
    The family is `Same` or `pred-succ` (DECISION D-018)."""

    name: str
    type: str
    field: str
    modifier: str
    members: tuple
    strength: int

    @property
    def family(self) -> str:
        """`Same` or `pred-succ`: "grouped into families" [M86 p. 64] (DECISION D-018)."""
        return FAMILIES[self.type]

    def other(self, element: CytoElement) -> CytoElement:
        """The member that is not `element` [M86 p. 64]."""
        a, b = self.members
        return b if element is a else a

    def line(self) -> str:
        """`bond1 --- between glint1glint3`, as printed when a bond is made [M86 p. 164]."""
        a, b = self.members
        return f"{self.name} --- between {a.name}{b.name}"

    def show_line(self) -> str:
        """`bond1 Succ print-value (adjacent) -- (glint1 glint2)`, as `show` prints a bond
        [M86 p. 174]."""
        a, b = self.members
        return f"{self.name} {self.type} {self.field} ({self.modifier}) -- ({a.name} {b.name})"


def same_way(bond: Bond, type: str, field: str, a: CytoElement, b: CytoElement) -> bool:
    """Whether `bond` already bonds `a` and `b` "in this way" [M86 p. 64]: same type, same
    field, same pair (in either order) (DECISION D-018)."""
    return (bond.type == type and bond.field == field
            and set(map(id, bond.members)) == {id(a), id(b)})


def bonded(a: CytoElement, b: CytoElement, type: str, field: str) -> Bond | None:
    """The bond of that type and field between `a` and `b`, or None; the Bonder's second
    check, "that they are not already bonded in this way" [M86 p. 64]."""
    for bond in a.bonds_in:
        if same_way(bond, type, field, a, b):
            return bond
    return None


def bonds_of(element: CytoElement, family: str | None = None, minimum: int = 0) -> list:
    """The bonds attached to an element, optionally of one family and of at least a
    strength (the Glom-scout's "any bond-family in which the glom is involved" [M86 p. 66];
    `Config.glom_bond_minimum`, DECISION D-019)."""
    return [bond for bond in element.bonds_in
            if (family is None or bond.family == family) and bond.strength >= minimum]

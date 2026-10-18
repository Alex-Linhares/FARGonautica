"""The nine gnoth operations, PROGRAMs, the Gnoth-operator and the Capper [M86 pp. 109-124].

"Gnoth operations fall into two categories: external or inter-gnoth operations, and internal
or intra-gnoth operations. The external operations are: SHIFT-LEFT, SHIFT-RIGHT, and SPLIT.
The internal operations are: CAPTURE, ENCLOSE, FRACTURE, MERGE, and NO-OP" [M86 p. 109]. They
act on real gloms under the capping discipline of p. 109: "The pseudo-gloms of any gnoths
involved in gnoth operations must be destroyed to permit the true gloms below to interact
with each other" and "if a very deeply nested glom is to be involved in an operation, all
gloms containing it must be destroyed so that it can rise to the top of the cytoplasm and
become available" [M86 p. 109]; "When a gnoth operation is completed, a capping procedure
puts a new pseudo-glom in place above the gnoth's gloms. Often, Plato-scout tasks are placed
on the taskrack to peruse the gnoth's newly-created gloms, searching among them for any new
manifestations of the Platonic concepts" [M86 pp. 109-110].

An operation is a tuple in the thesis's format, `("SHIFT-LEFT", "gnoth2", "gnoth3",
("glom7", "glom10"))` for `(SHIFT-LEFT gnoth2 gnoth3 (glom7 glom10))` [M86 p. 110]; a
PROGRAM is `("PROGRAM", (op, op, ...))`, "a PROGRAM or series of moves designed to put the
proper glom in its proper place and repair as much resulting gnoth-tearing as possible"
[M86 p. 121]. The Gnoth-operator task carries one out [M86 p. 124] and the Capper recaps a
gnoth a NO-OP uncapped [M86 p. 113]. Every example of pp. 110-121 is a test, as are the
PROGRAMs of p. 122 and the transcript's [M86 pp. 174, 177]. Nothing here draws from the
`Rng`; the choices the thesis leaves open are DECISIONS D-044, D-045, D-065, D-066 and
D-067. A builder checks that everything it needs still exists before it changes anything,
and a task that finds its structures gone does nothing.
"""

from dataclasses import dataclass

from seekwhence import cytoplasm as cy
from seekwhence import events
from seekwhence import gloms
from seekwhence import gnoths as gn
from seekwhence import slipping
from seekwhence.cytoplasm import CytoElement, Glom

# The two categories [M86 p. 109], with CAPTURE's two directions [M86 p. 112].
EXTERNAL = ("SHIFT-LEFT", "SHIFT-RIGHT", "SPLIT")
INTERNAL = ("CAPTURE-LEFT", "CAPTURE-RIGHT", "ENCLOSE", "FRACTURE", "MERGE", "NO-OP")
OPERATIONS = EXTERNAL + INTERNAL

# The number of arguments each operation takes [M86 pp. 110-113].
ARITY = {"SHIFT-LEFT": 3, "SHIFT-RIGHT": 3, "SPLIT": 2, "CAPTURE-LEFT": 3, "CAPTURE-RIGHT": 3,
         "ENCLOSE": 2, "FRACTURE": 1, "MERGE": 2, "NO-OP": 1}

# The `type` field of a glom an operation makes: "dubbed to be of type 'enclose'", "a
# 'merge'-type glom" [M86 pp. 112-113]; the capture glom's type is DECISION D-066.
NEW_TYPES = {"CAPTURE-LEFT": ("capture",), "CAPTURE-RIGHT": ("capture",),
             "ENCLOSE": ("enclose",), "MERGE": ("merge",)}

# The type of a gnoth's cap, as the Gnoth-maker and the Gnoth-setter make it [M86 p. 78].
GNOTH_GLOM = slipping.GNOTH_GLOM

# The types of the gloms the operations make (DECISION D-066).
OPERATION_GLOMS = tuple(set(NEW_TYPES.values()))


@dataclass(frozen=True)
class New:
    """A reference, inside a PROGRAM, to the glom made by an earlier `step` of the same
    PROGRAM (counted from 0): the thesis's `newglom3` and `newglom10` in the deeper move of
    p. 122, whose later ENCLOSEs re-enclose "the remaining gloms at each intermediate level"
    around the glom the previous step made [M86 pp. 122-123] (DECISION D-044). Prints as
    `(new 2)`."""

    step: int

    def __str__(self) -> str:
        return f"(new {self.step})"


# ---- the printed forms ------------------------------------------------------------------------

def operation_text(operation) -> str:
    """An operation or PROGRAM in the thesis's spelling: `(SHIFT-LEFT gnoth2 gnoth3 (glom7
    glom10))` [M86 p. 110], `(ENCLOSE gnoth1 nil)` and `(PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2
    (glint1)) (ENCLOSE gnoth1 nil)))` [M86 pp. 122, 174]; an empty list prints `nil`."""
    return _text(operation)


def _text(item) -> str:
    if item is None:
        return "nil"
    if isinstance(item, (tuple, list)):
        if not item:
            return "nil"
        return "(" + " ".join(_text(x) for x in item) + ")"
    return str(item)


def is_program(thing) -> bool:
    """Whether `thing` is a PROGRAM, `("PROGRAM", (op, ...))` [M86 p. 122]."""
    return (isinstance(thing, tuple) and len(thing) == 2 and thing[0] == "PROGRAM"
            and isinstance(thing[1], (tuple, list)))


def steps_of(thing) -> list:
    """The operations of a PROGRAM in order, or the one operation of a bare move: the
    transcript prints both, `((PROGRAM ((SHIFT-RIGHT gnoth1 gnoth2 (glint1)) (ENCLOSE
    gnoth1 nil))))` [M86 p. 174] and `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` [M86 p. 177]
    (DECISION D-067)."""
    return list(thing[1]) if is_program(thing) else [thing]


def _names(listed) -> tuple:
    if listed is None:
        return ()
    if isinstance(listed, (str, New)):
        return (listed,)
    return tuple(listed)


# ---- what an operation did ---------------------------------------------------------------------

@dataclass
class Outcome:
    """What one operation did: the `operation`, the names of the `gnoths` it touched and
    recapped, the gnoths `gone` (emptied and destroyed, "a shell which the system destroys"
    [M86 p. 108]), the gnoths `created` (by SPLIT), the gloms `made` (by CAPTURE, ENCLOSE,
    MERGE), the gnoths left `uncapped` (by NO-OP), and `reason`, None when the operation was
    carried out and otherwise why it could not be (DECISION D-067)."""

    operation: tuple
    gnoths: tuple = ()
    gone: tuple = ()
    created: tuple = ()
    made: tuple = ()
    uncapped: tuple = ()
    reason: str | None = None

    @property
    def acted(self) -> bool:
        """Whether the operation changed anything: an ENCLOSE of `nil` does not, "no repair
        necessary" [M86 p. 122] (DECISION D-067)."""
        return self.reason is None and bool(self.gnoths or self.gone or self.created
                                            or self.uncapped)


# ---- looking things up ---------------------------------------------------------------------------

def gnoth_named(world, name) -> gn.Gnoth | None:
    """The listed gnoth of that name, or None (the catchall is never operated on,
    DECISION D-030)."""
    gnoth = world.socratoplasm.find(name) if isinstance(name, str) else None
    if isinstance(gnoth, gn.Gnoth) and not gnoth.catchall:
        return gnoth
    return None


def element_named(world, name, made=()) -> CytoElement | None:
    """The glint or glom of that name, or None; a `New` reference is the glom the given
    earlier step of the PROGRAM `made` (DECISION D-044)."""
    if isinstance(name, New):
        if 0 <= name.step < len(made) and made[name.step] is not None:
            name = made[name.step]
        else:
            return None
    element = world.cytoplasm.find(name) if isinstance(name, str) else None
    return element if isinstance(element, CytoElement) else None


def holder(world, element: CytoElement) -> gn.Gnoth | None:
    """The gnoth whose cap covers the element, or None; an operation's gloms must be
    "currently under the given gnoth" [M86 p. 110]."""
    return slipping.owner(world, element)


def are_neighbours(world, left: gn.Gnoth, right: gn.Gnoth) -> bool:
    """Whether `right` is the gnoth just after `left` in the parse: "All inter-gnoth moves
    involve the rightmost (at some level) glom of some gnoth and the leftmost (again, at
    some level) glom of the gnoth's neighbor to the right" [M86 p. 106]."""
    listed = gn.gnoths(world)
    for a, b in zip(listed, listed[1:]):
        if a is left and b is right:
            return True
    return False


def members_in(world, start: int, end: int) -> list:
    """The top-level elements of the lineup lying within the positions `start` to `end`,
    left to right: a gnoth's glom collection once its cap is off [M86 p. 78]."""
    return [x for x in cy.lineup(world)
            if start <= x.start_position and x.end_position <= end]


def extent(elements) -> tuple:
    """The first and last position the elements cover [M86 p. 78]."""
    return (min(x.start_position for x in elements), max(x.end_position for x in elements))


def contiguous(elements) -> bool:
    """Whether the elements, none nested in another, cover their extent without a gap: the
    "neighbors in order" an operation's list must be [M86 pp. 110, 112-113] (DECISION
    D-066). Checked on positions, before anything is destroyed."""
    elements = list(elements)
    if not elements:
        return False
    if any(cy.nested(a, b) for i, a in enumerate(elements) for b in elements[i + 1:]):
        return False
    lo, hi = extent(elements)
    return sum(x.span for x in elements) == hi - lo + 1


# ---- caps: uncapping, uncovering, recapping [M86 pp. 109-110] --------------------------------

def uncap(world, gnoth: gn.Gnoth) -> None:
    """Take a gnoth's cap off: its pseudo-glom is dissolved "to permit the true gloms below
    to interact with each other" [M86 p. 109]; the gnoth keeps its range."""
    glom = gnoth.glom
    if glom is not None and world.cytoplasm.find(glom.name) is glom:
        cy.dissolve(world, glom)
    gnoth.glom = None


def uncover(world, element: CytoElement) -> list:
    """Bring an element to the top of the cytoplasm: "all gloms containing it must be
    destroyed so that it can rise to the top of the cytoplasm and become available"
    [M86 p. 109]. Each containing glom is dissolved, outermost first, so that the element's
    siblings at every level survive (p. 114's glom5 and p. 116's glom4, glom6 and glom8 are
    still there after the operation). Returns the gloms dissolved, outermost first."""
    chain = []
    parent = element.parent
    while parent is not None:
        chain.append(parent)
        parent = parent.parent
    for glom in reversed(chain):
        cy.dissolve(world, glom)
    return list(reversed(chain))


def recap(world, gnoth: gn.Gnoth, start: int, end: int) -> Glom | None:
    """"a capping procedure puts a new pseudo-glom in place above the gnoth's gloms" [M86
    p. 109]: a new gnoth-glom over the top-level elements from `start` to `end`, printed
    `top-down glom glomN` [M86 pp. 174, 177]; the gnoth's range follows the cap and its
    `form` is the one member's structure field when it has a single dubbed glom, else nil
    (DECISION D-066). A gnoth with nothing left under it disappears, "a shell which the
    system destroys" [M86 p. 108] (`[] (disappears)` [M86 p. 120]). Returns the cap, or
    None when the gnoth is gone."""
    members = members_in(world, start, end)
    if not members:
        gn.destroy_gnoth(world, gnoth, keep_glom=True)
        world.events.publish(events.GnothGone(gnoth.name, "emptied by a gnoth operation"))
        return None
    lo, hi = extent(members)
    if sum(x.span for x in members) != hi - lo + 1:
        raise RuntimeError(f"{gnoth.name}'s members do not cover {lo}-{hi} without a gap")
    cap = cy.make_glom(world, GNOTH_GLOM, members, pseudo=True)
    world.events.publish(events.TopDownGlom(cap.name, tuple(x.name for x in members),
                                            cap.print_value, True))
    gnoth.glom = cap
    gnoth.range = (lo, hi)
    gnoth.form = None
    if len(members) == 1 and not members[0].is_glint and members[0].structure:
        gnoth.form = list(members[0].structure)
    # the `groups` field is set once a gnoth has been through an operation: the member
    # lists of the gloms operations made under it, `groups: nil` for a shift [M86 pp. 175,
    # 179] (DECISION D-066 as amended in item 21)
    gnoth.groups = [tuple(x.name for x in m.subgloms) for m in members
                    if not m.is_glint and m.type in OPERATION_GLOMS]
    return cap


def new_gnoth(world, like: gn.Gnoth, start: int, end: int) -> gn.Gnoth:
    """A gnoth that "emerges" from a SPLIT [M86 p. 120]: a fresh name, the frame and
    plato-class of the gnoth it was split from, an actualization of that class
    [M86 p. 77], capped over `start` to `end` (DECISION D-066)."""
    gnoth = gn.Gnoth(world.name("gnoth"), like.frame, like.plato_class, state=like.state)
    world.socratoplasm.add(gnoth)
    world.platoplasm.actualize(like.plato_class, gnoth)
    recap(world, gnoth, start, end)
    world.events.publish(events.GnothMade(gnoth.name, gnoth.frame, gnoth.plato_class,
                                          gnoth.glom.name, gnoth.range,
                                          tuple(x.name for x in gnoth.collection)))
    return gnoth


def made_glom(world, kind: str, members) -> Glom:
    """A glom an operation makes, of type `capture`, `enclose` or `merge` [M86 pp. 112-113]
    (DECISION D-066), with a Plato-scout set out on it: "Plato-scout tasks are placed on the
    taskrack to peruse the gnoth's newly-created gloms" [M86 pp. 109-110] (DECISION D-044;
    a top-down glom interests every class, DECISION D-022)."""
    glom = cy.make_glom(world, NEW_TYPES[kind], members)
    world.events.publish(events.OperationGlom(glom.name, glom.type,
                                              tuple(x.name for x in members),
                                              glom.print_value))
    world.post("Plato-scout", gloms.interested(glom), glom.name)
    return glom


# ---- the nine operations [M86 pp. 110-118] ---------------------------------------------------

def apply_operation(world, operation, made=()) -> Outcome:
    """Carry out one operation on the world's gnoths and gloms, after checking that
    everything it names exists and is where the operation needs it (DECISION D-067);
    `made` lists the gloms the earlier steps of a PROGRAM made, one per step (None for a
    step that made none), for `New` references (DECISION D-044). Returns the `Outcome`,
    whose `reason` says why nothing was done when the checks failed."""
    if not isinstance(operation, tuple) or not operation or operation[0] not in OPERATIONS:
        return Outcome(operation, reason="not a gnoth operation")
    name = operation[0]
    if len(operation) - 1 != ARITY[name]:
        return Outcome(operation, reason=f"{name} takes {ARITY[name]} arguments")
    if name in ("SHIFT-LEFT", "SHIFT-RIGHT"):
        return shift(world, operation, made)
    if name == "SPLIT":
        return split(world, operation, made)
    if name in ("CAPTURE-LEFT", "CAPTURE-RIGHT"):
        return capture(world, operation, made)
    if name == "ENCLOSE":
        return enclose(world, operation, made)
    if name == "FRACTURE":
        return fracture(world, operation)
    if name == "MERGE":
        return merge(world, operation, made)
    return no_op(world, operation)


def _listed(world, gnoth: gn.Gnoth, listed, made) -> list | str:
    """The elements an operation lists, all of which must exist and lie under the gnoth
    ("a list of gloms currently under the given gnoth" [M86 p. 110]); a reason when
    not."""
    elements = []
    for item in _names(listed):
        element = element_named(world, item, made)
        if element is None:
            return f"{item} is gone"
        if holder(world, element) is not gnoth:
            return f"{element.name} is not under {gnoth.name}"
        elements.append(element)
    return elements


def shift(world, operation, made=()) -> Outcome:
    """SHIFT-LEFT and SHIFT-RIGHT: `(SHIFT-LEFT <left-gnoth> <right-gnoth> <glomlist>)`,
    "where glomlist is a list of the gloms (which must be neighbors in order) to be
    transferred from right-gnoth to left-gnoth"; SHIFT-RIGHT's glomlist "serves an
    analogous purpose, this time from left-gnoth to right-gnoth" [M86 p. 110]. The gnoths
    must be neighbours and the gloms under the giving gnoth; each listed glom is uncovered
    (`(SHIFT-LEFT gnoth2 gnoth3 (glint6))` takes the 3 out of `(3 4 5)`, leaving `[4 5]`
    [M86 p. 120]) and the border between the gnoths moves past it, so that whatever lies
    between the listed gloms and the border goes with them (DECISION D-065: the
    transcript's `(SHIFT-RIGHT gnoth3 gnoth5 (glint3))` leaves `glom14` over terms 3 to 5
    [M86 pp. 177-178]); the gloms keep their identity (glom7 and glom10 on p. 110). A gnoth
    emptied disappears [M86 p. 120]."""
    name, left_name, right_name, listed = operation
    left, right = gnoth_named(world, left_name), gnoth_named(world, right_name)
    if left is None or right is None:
        return Outcome(operation, reason="a gnoth is gone")
    if not are_neighbours(world, left, right):
        return Outcome(operation, reason=f"{left.name} and {right.name} are not neighbours")
    giver, taker = (right, left) if name == "SHIFT-LEFT" else (left, right)
    elements = _listed(world, giver, listed, made)
    if isinstance(elements, str):
        return Outcome(operation, reason=elements)
    if not elements:
        return Outcome(operation, reason="an empty glomlist")
    lo, hi = extent(elements)
    uncap(world, left)
    uncap(world, right)
    for element in elements:
        uncover(world, element)
    if name == "SHIFT-LEFT":
        left_range, right_range = (left.range[0], hi), (hi + 1, right.range[1])
    else:
        left_range, right_range = (left.range[0], lo - 1), (lo, right.range[1])
    kept, gone = [], []
    for gnoth, (start, end) in ((left, left_range), (right, right_range)):
        if start <= end and recap(world, gnoth, start, end) is not None:
            kept.append(gnoth.name)
        else:
            if gnoth.name in world.socratoplasm:
                gn.destroy_gnoth(world, gnoth, keep_glom=True)
                world.events.publish(events.GnothGone(gnoth.name,
                                                      "emptied by a gnoth operation"))
            gone.append(gnoth.name)
    return Outcome(operation, gnoths=tuple(kept), gone=tuple(gone))


def split(world, operation, made=()) -> Outcome:
    """SPLIT: `(SPLIT <gnoth> <splitlist>)`, "where splitlist is a list of gloms currently
    under the given gnoth. A new gnoth is to be formed using the splitlist gloms as its
    top level" [M86 p. 110]: `(SPLIT gnoth2 (glom6 glom10))` leaves `gnoth2: [(1 2)]` and
    a new gnoth `[(2 3) (2 3)]` [M86 p. 111]. The listed gloms must be neighbours in order
    at one end of the gnoth (DECISION D-066); they are uncovered, the new gnoth is capped
    over them and the old one over what remains."""
    _, gnoth_name, listed = operation
    gnoth = gnoth_named(world, gnoth_name)
    if gnoth is None:
        return Outcome(operation, reason=f"{gnoth_name} is gone")
    elements = _listed(world, gnoth, listed, made)
    if isinstance(elements, str):
        return Outcome(operation, reason=elements)
    if not elements or not contiguous(elements):
        return Outcome(operation, reason="the splitlist is not a run of neighbours")
    lo, hi = extent(elements)
    start, end = gnoth.range
    if lo != start and hi != end:
        return Outcome(operation, reason="the splitlist is not at an end of the gnoth")
    if lo == start and hi == end:
        return Outcome(operation, reason="the splitlist is the whole gnoth")
    uncap(world, gnoth)
    for element in elements:
        uncover(world, element)
    rest = (hi + 1, end) if lo == start else (start, lo - 1)
    if lo == start:
        emerged = new_gnoth(world, gnoth, lo, hi)
        recap(world, gnoth, *rest)
    else:
        recap(world, gnoth, *rest)
        emerged = new_gnoth(world, gnoth, lo, hi)
    return Outcome(operation, gnoths=(gnoth.name,), created=(emerged.name,))


def capture(world, operation, made=()) -> Outcome:
    """CAPTURE-LEFT and CAPTURE-RIGHT: `(CAPTURE-LEFT <gnoth> <glom> <captive>)`, "where the
    given glom within the given gnoth is to swallow its neighbor, captive, whole. Actually,
    the glom is destroyed and a new one created with the captive as its leftmost subglom
    and also containing all the original glom's subgloms" [M86 p. 112]; CAPTURE-RIGHT is
    "analogous" [M86 p. 112], the captive becoming the rightmost subglom. The glom must be
    a real glom (a glint has no subgloms), the captive its neighbour on that side, both
    under the gnoth; both are uncovered first (`(CAPTURE-LEFT gnoth1 glom10 glom7)` bursts
    glom15 and leaves glom5 and the new glom20 at the top level [M86 pp. 114-115]). The new
    glom is of type `capture` and gets a Plato-scout (DECISION D-066)."""
    name, gnoth_name, glom_name, captive_name = operation
    gnoth = gnoth_named(world, gnoth_name)
    if gnoth is None:
        return Outcome(operation, reason=f"{gnoth_name} is gone")
    pair = _listed(world, gnoth, (glom_name, captive_name), made)
    if isinstance(pair, str):
        return Outcome(operation, reason=pair)
    glom, captive = pair
    if glom.is_glint:
        return Outcome(operation, reason=f"{glom.name} is a glint and cannot capture")
    if cy.nested(glom, captive):
        return Outcome(operation, reason=f"{captive.name} is nested with {glom.name}")
    if name == "CAPTURE-LEFT" and captive.end_position + 1 != glom.start_position:
        return Outcome(operation, reason=f"{captive.name} is not {glom.name}'s left neighbour")
    if name == "CAPTURE-RIGHT" and glom.end_position + 1 != captive.start_position:
        return Outcome(operation, reason=f"{captive.name} is not {glom.name}'s right neighbour")
    uncap(world, gnoth)
    uncover(world, glom)
    uncover(world, captive)
    parts = list(cy.dissolve(world, glom))
    members = [captive] + parts if name == "CAPTURE-LEFT" else parts + [captive]
    new = made_glom(world, name, members)
    recap(world, gnoth, *gnoth.range)
    return Outcome(operation, gnoths=(gnoth.name,), made=(new.name,))


def enclose(world, operation, made=()) -> Outcome:
    """ENCLOSE: `(ENCLOSE <gnoth> <encloselist>)`, "where encloselist is a list of
    neighboring gloms within the gnoth to be covered by a new glom, dubbed to be of type
    'enclose'" [M86 p. 112]: `(ENCLOSE gnoth1 (glint1 glint2 glint3))` makes `[(1 2 3) 1]`
    [M86 p. 115]; `(ENCLOSE gnoth3 (glom5 glom7))` uncovers both from different parents and
    leaves `[(2 2) (3) (3 3) ((4 4) (4 4)) (5 5)]` [M86 p. 116]. `(ENCLOSE gnoth2 nil)` is
    the PROGRAM's "no repair necessary" and does nothing [M86 p. 122], even when the gnoth
    has since disappeared (DECISION D-067). The new glom gets a Plato-scout
    (DECISION D-066)."""
    _, gnoth_name, listed = operation
    if not _names(listed):
        return Outcome(operation)
    gnoth = gnoth_named(world, gnoth_name)
    if gnoth is None:
        return Outcome(operation, reason=f"{gnoth_name} is gone")
    elements = _listed(world, gnoth, listed, made)
    if isinstance(elements, str):
        return Outcome(operation, reason=elements)
    if not contiguous(elements):
        return Outcome(operation, reason="the encloselist is not a run of neighbours")
    lo, hi = extent(elements)
    uncap(world, gnoth)
    for element in elements:
        uncover(world, element)
    new = made_glom(world, "ENCLOSE", members_in(world, lo, hi))
    recap(world, gnoth, *gnoth.range)
    return Outcome(operation, gnoths=(gnoth.name,), made=(new.name,))


def fracture(world, operation) -> Outcome:
    """FRACTURE: `(FRACTURE <gnothname>)`, "where the given gnoth is to have all of its
    top-level gloms (the direct subgloms of its pseudo-glom) dissolved, bringing their
    subgloms to the top-level" [M86 p. 113]: `[(1 2) ((3 3) (4 4))]` becomes
    `[1 2 (3 3) (4 4)]` [M86 p. 117], one level only."""
    _, gnoth_name = operation
    gnoth = gnoth_named(world, gnoth_name)
    if gnoth is None:
        return Outcome(operation, reason=f"{gnoth_name} is gone")
    if gnoth.glom is None:
        return Outcome(operation, reason=f"{gnoth.name} is uncapped")
    uncap(world, gnoth)
    for element in members_in(world, *gnoth.range):
        if not element.is_glint:
            cy.dissolve(world, element)
    recap(world, gnoth, *gnoth.range)
    return Outcome(operation, gnoths=(gnoth.name,))


def merge(world, operation, made=()) -> Outcome:
    """MERGE: `(MERGE <gnoth> <glomlist>)`, "where glomlist is a list of neighbor gloms in
    order within the given gnoth. The listed gloms are all uncovered and their subgloms
    glommed into a 'merge'-type glom, which becomes a top-level glom in the gnoth"
    [M86 p. 113]: `(MERGE gnoth1 (glom1 glom2))` turns `[(1 1) (1 1 1)]` into
    `[(1 1 1 1 1)]` [M86 p. 118], and `(MERGE gnoth1 (glom4 glom7))` bursts glom19 to
    reach glom7, lets glom10 rise, and makes `((1 2 3) (3 4) (5 6) (4 5))` of the two
    gloms' subgloms [M86 pp. 118-119]. A listed glint is kept as it is. The new glom gets
    a Plato-scout (DECISION D-066)."""
    _, gnoth_name, listed = operation
    gnoth = gnoth_named(world, gnoth_name)
    if gnoth is None:
        return Outcome(operation, reason=f"{gnoth_name} is gone")
    elements = _listed(world, gnoth, listed, made)
    if isinstance(elements, str):
        return Outcome(operation, reason=elements)
    if not elements or not contiguous(elements):
        return Outcome(operation, reason="the glomlist is not a run of neighbours")
    lo, hi = extent(elements)
    uncap(world, gnoth)
    for element in elements:
        uncover(world, element)
    for element in elements:
        if not element.is_glint:
            cy.dissolve(world, element)
    new = made_glom(world, "MERGE", members_in(world, lo, hi))
    recap(world, gnoth, *gnoth.range)
    return Outcome(operation, gnoths=(gnoth.name,), made=(new.name,))


def no_op(world, operation) -> Outcome:
    """NO-OP: `(NO-OP <gnoth>)`, "which causes the gnoth to be 'uncapped' -- have its
    pseudo-glom suspended -- and remain that way until the Capper task it sets out is
    invoked and recaps the gnoth. This 'slow-recap' permits natural glomming to occur
    within the gnoth, and between gnoths should two neighboring gnoths be uncapped
    simultaneously" [M86 p. 113]. The Capper is set out with the gnoth's original range
    (DECISION D-045)."""
    _, gnoth_name = operation
    gnoth = gnoth_named(world, gnoth_name)
    if gnoth is None:
        return Outcome(operation, reason=f"{gnoth_name} is gone")
    if gnoth.glom is None:
        return Outcome(operation, reason=f"{gnoth.name} is uncapped already")
    original = gnoth.range
    uncap(world, gnoth)
    world.events.publish(events.GnothUncapped(gnoth.name, original))
    world.post("Capper", gnoth.name, original)
    return Outcome(operation, uncapped=(gnoth.name,))


# ---- the Capper task [M86 p. 113] -------------------------------------------------------------

def other_caps(world, gnoth: gn.Gnoth) -> list:
    """The caps of the other gnoths, which a Capper never claims (DECISION D-045)."""
    return [g.glom for g in gn.gnoths(world) if g is not gnoth and g.glom is not None]


def capper(world, name: str, original) -> None:
    """The Capper task: "The Capper finds all current gloms whose ranges overlap with the
    original range of the gnoth (before it was uncapped) and claims them for the gnoth.
    Should two different gnoths claim the same glom -- one formed, perhaps, by combining
    gloms from the two gnoths -- the gnoth that recaps first will get the glom and the
    extended range" [M86 p. 113]. The claim is the top-level elements overlapping the
    original range that are not another gnoth's cap; the cap goes over the longest run of
    them (DECISION D-045), and a gnoth with nothing to claim disappears [M86 p. 108].
    Nothing is done when the gnoth is gone or capped already (DECISION D-009)."""
    gnoth = gnoth_named(world, name)
    if gnoth is None:
        world.stale("Capper", (name, original), f"{name} is gone")
        return
    if gnoth.glom is not None:
        world.stale("Capper", (name, original), f"{name} is capped already")
        return
    start, end = original
    caps = other_caps(world, gnoth)
    overlapping = [x for x in cy.lineup(world)
                   if x.start_position <= end and start <= x.end_position
                   and not any(x is cap for cap in caps)]
    runs: list = []
    for element in overlapping:
        if runs and runs[-1][-1].end_position + 1 == element.start_position:
            runs[-1].append(element)
        else:
            runs.append([element])
    claimed = max(runs, key=len) if runs else []
    if not claimed:
        gn.destroy_gnoth(world, gnoth, keep_glom=True)
        world.events.publish(events.GnothGone(gnoth.name, "nothing left to cap"))
        return
    lo, hi = extent(claimed)
    cap = recap(world, gnoth, lo, hi)
    world.events.publish(events.GnothCapped(gnoth.name, cap.name, (lo, hi),
                                            tuple(x.name for x in claimed)))


# ---- the Gnoth-operator task [M86 p. 124] ------------------------------------------------------

def precheck(world, steps) -> str | None:
    """The Gnoth-operator's first duty: "It must first check to see that all the structures
    relevant to its operation are still in existence, having survived the system's
    activity while the Gnoth-operator was hanging on the taskrack" [M86 p. 124]. Every
    gnoth and glom a step names must exist now; a `New` reference must point to an earlier
    step (DECISION D-067). Returns the reason the check failed, or None."""
    for i, step in enumerate(steps):
        if not isinstance(step, tuple) or not step or step[0] not in OPERATIONS:
            return f"{_text(step)} is not a gnoth operation"
        name = step[0]
        if len(step) - 1 != ARITY[name]:
            return f"{name} takes {ARITY[name]} arguments"
        args = step[1:]
        gnoth_args = args[:2] if name in ("SHIFT-LEFT", "SHIFT-RIGHT") else args[:1]
        for arg in gnoth_args:
            if gnoth_named(world, arg) is None:
                return f"{arg} is gone"
        for arg in args[len(gnoth_args):]:
            for item in _names(arg):
                if isinstance(item, New):
                    if not 0 <= item.step < i:
                        return f"{item} does not refer to an earlier step"
                elif element_named(world, item) is None:
                    return f"{item} is gone"
    return None


def run_program(world, program) -> list:
    """Carry out an operation or PROGRAM step by step (`apply_operation`), stopping at the
    first step that cannot be done (DECISION D-067); returns the `Outcome`s, the last one
    carrying the reason when the PROGRAM stopped short."""
    made: list = []
    outcomes = []
    for step in steps_of(program):
        outcome = apply_operation(world, step, made)
        made.append(outcome.made[0] if outcome.made else None)
        world.events.publish(events.GnothOperation(
            operation_text(step), outcome.gnoths, outcome.gone, outcome.created,
            outcome.made, outcome.uncapped, outcome.reason))
        outcomes.append(outcome)
        if outcome.reason is not None:
            break
    return outcomes


def gnoth_operator(world, program=None) -> None:
    """The Gnoth-operator task, `doing task Gnoth-operator on ((PROGRAM ((SHIFT-RIGHT gnoth1
    gnoth2 (glint1)) (ENCLOSE gnoth1 nil))))` then `top-down glom glom9` [M86 p. 174]: "A
    Gnoth-operator task is charged with carrying out the operation or PROGRAM given it by a
    Reformulator at the time of its creation. It must first check to see that all the
    structures relevant to its operation are still in existence ... If the relevant
    structures do still exist, the Gnoth-operator carries out the operations; if not, it
    will simply terminate. When a Gnoth-operator does in fact operate, its last action is to
    decrease the system's bond-strength threshold by 1" [M86 p. 124], "in effect 'heating
    up' the environment" [M86 p. 124]. Afterwards every gnoth's bonds are assessed afresh in
    the reigning environment [M86 p. 100], since the collections changed (DECISION D-067).
    The threshold is left alone when no Reformulator has set one (DECISION D-046)."""
    if program is None:
        world.stale("Gnoth-operator", (), "no operation")
        return
    steps = steps_of(program)
    reason = precheck(world, steps)
    if reason is not None:
        world.stale("Gnoth-operator", (program,), reason)
        return
    outcomes = run_program(world, program)
    acted = any(o.acted for o in outcomes)
    stopped = outcomes[-1].reason if outcomes and outcomes[-1].reason is not None else None
    if acted:
        if world.reigning is not None:
            slipping.assess_bonds(world, world.reigning)
        if world.threshold is not None:
            world.threshold -= 1
    world.events.publish(events.GnothOperated(operation_text(program), len(outcomes),
                                              acted, stopped, world.threshold))


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Gnoth-operator": gnoth_operator,
    "Capper": capper,
}

"""The terraced scan from the Glom-scout up to the platoplasm [M86 pp. 65-70].

"As was noted earlier, bond creation causes the release of Glom-scouts onto the taskrack.
These tasks look for glommable bond-chains. They also serve to introduce a good example of a
terraced scan in Seek-Whence" [M86 p. 66]: the tasks from here to the Plato-evaluator
"perform increasingly extensive tests on target gloms, screening the gloms as potential
representatives of various Platonic classes. If a glom passes one test, it is targeted for
further evaluation. Should a glom fail a test, it may be re-evaluated by other tasks"
[M86 p. 66]. The scan: a Glom-scout's three quicktests
[M86 p. 66], a Glomtester's extent [M86 p. 67], the Glommer's glom [M86 p. 67], the
Glom-inspector's interested classes [M86 p. 68], the Plato-scout's dubbing [M86 pp. 68-69],
the Plato-evaluator's second look [M86 p. 69] and the Burster [M86 pp. 69-70]. "The
Plato-evaluator creates no other tasks. Our terraced scan has come to an end" [M86 p. 70].

The structures are in `seekwhence.cytoplasm`; the Dissolver, which a new term posts, is in
`seekwhence.bonds`. Every task is a function `task(world, *args)` registered in `TASKS` by
its printed name, and the Glom-scout's one random choice goes through the world's `Rng`
(DECISION D-020). The numbers of the scan are DECISIONS D-019 (which bonds count), D-020 (the
Glomtester's urgency), D-021 (extents and failure), D-022 (interest), D-023 (the
Plato-evaluator) and D-024 (the Burster).
"""

from seekwhence import bonds
from seekwhence import cytoplasm as cy
from seekwhence import events
from seekwhence import platoplasm as pl
from seekwhence.cytoplasm import Bond, CytoElement, Glom
from seekwhence.forms import Form, start_value

# The three quicktests, "the precursors of the plato-level notions of generate, process, and
# fence classes" [M86 p. 66], in the thesis's order.
TESTS = ("group", "cover", "fence")

# The family label of a pred-succ bond seen from the other end (DECISION D-020): `1 2`
# scouted at the 2 is `Pred -group proposed --> glint2` [M86 p. 170].
FLIPPED = {"Succ": "Pred", "Pred": "Succ", "Same": "Same"}

# Which plato-classes find a glom interesting, by its family and test (DECISION D-022):
# `(Cycle glom1)` for the sameness fence `(3 7)` [M86 p. 165], `((S-group P-group) glom1)`
# for the pred-succ group `(1 2)` [M86 p. 170]; `(1 1 1)` may be dubbed C-group and Y-group
# [M86 p. 69].
INTEREST = {
    ("Same", "group"): ("C-group", "Y-group"),
    ("pred-succ", "group"): ("S-group", "P-group"),
    ("Same", "cover"): ("Y-group",),
    ("pred-succ", "cover"): ("Y-group",),
    ("Same", "fence"): ("Cycle",),
    ("pred-succ", "fence"): ("Cycle",),
}
# A top-down glom interests every group and fence class [M86 p. 173] (DECISION D-022).
ALL_SIX = ("C-group", "S-group", "P-group", "Y-group", "Cycle", "Tuple")


# ---- bonds the scan follows ------------------------------------------------------------------

def strong_bonds(world, element: CytoElement, family: str | None = None) -> list:
    """The bonds on an element that can cause glomming: of at least
    `Config.glom_bond_minimum` (DECISION D-019), so that a same-span bond, "not considered
    strong enough to cause glomming" [M86 p. 62], is never followed."""
    return cy.bonds_of(element, family, world.config.glom_bond_minimum)


def families_of(world, element: CytoElement) -> list:
    """The bond families "in which the glom is involved" [M86 p. 66], in the order of
    `cytoplasm.FAMILIES` (sameness first, as the stronger [M86 p. 66])."""
    present = {bond.family for bond in strong_bonds(world, element)}
    return [family for family in ("Same", "pred-succ") if family in present]


def label_for(bond: Bond, element: CytoElement) -> str:
    """The family label of a bond "from the element's side" (DECISION D-020): `Same` for a
    sameness bond; for a pred-succ bond the bond's type when the element is its left member
    and the opposite when it is the right one, which is how the successor pair `1 2`, scouted
    at the 2, is proposed as `Pred -group` and glommed as `(Pred print-value group)`
    [M86 pp. 170-171]."""
    if bond.family == "Same":
        return "Same"
    return bond.type if bond.members[0] is element else FLIPPED[bond.type]


def linked(world, a: CytoElement, b: CytoElement, family: str) -> Bond | None:
    """The strong adjacent bond of the family between two neighbouring elements, or None:
    the link of a "bond-chain" [M86 p. 66]."""
    for bond in strong_bonds(world, a, family):
        if bond.modifier == "adjacent" and bond.other(a) is b:
            return bond
    return None


def usable(elements) -> bool:
    """Whether a run of lineup elements may become a bottom-up glom: at least two, none a
    pseudo-glom (DECISION D-021)."""
    return len(elements) >= 2 and not any(x.pseudo for x in elements)


# ---- the Glom-scout -----------------------------------------------------------------------

def glom_scout(world) -> None:
    """The Glom-scout task: "When invoked, a Glom-scout chooses a cyto-element (a glint or
    glom which is not a subglom of any other glom) and attempts to group it with its
    neighbors. Actually, three quicktests tests are made for any bond-family in which the
    glom is involved: 1) is it groupable? (bonded to any neighbors in this way?) 2) is it
    coverable? (bonded into a symmetry group?) 3) is it fenceable? (are there remote gloms to
    which it is bonded?)" [M86 p. 66]. The element is drawn through the `Rng` from the
    top-level real elements (DECISION D-020); every test passed is printed
    (`Same -cover proposed --> glint1` [M86 p. 165]) and sets out a Glomtester: "Any tests
    passed cause creation of a Glomtester task to make a more extensive test of the glom"
    [M86 p. 66]."""
    candidates = cy.top_level_elements(world, pseudo=False)
    if not candidates:
        world.stale("Glom-scout", (), "nothing to scout")
        return
    element = world.rng.choice(candidates)
    for label, test in quicktests(world, element):
        propose(world, label, test, element)


def quicktests(world, element: CytoElement) -> list:
    """The (label, test) pairs an element passes, in the thesis's order of tests within
    each family [M86 p. 66] (DECISION D-020). The symmetry test is also made for an element
    that is the centre of a symmetry without being bonded itself, which is how the 6 of
    `5 3 1 6 1 3 4` can be targeted "with 'symmetry' the reason" [M86 p. 67]."""
    passed = []
    for family in families_of(world, element):
        group_bond = groupable(world, element, family)
        if group_bond is not None:
            passed.append((label_for(group_bond, element), "group"))
        if family == "Same" and coverable(world, element):
            passed.append(("Same", "cover"))
        fence_bond = fenceable(world, element, family)
        if fence_bond is not None:
            passed.append((label_for(fence_bond, element), "fence"))
    if "Same" not in families_of(world, element) and centre_of_symmetry(world, element):
        passed.append(("Same", "cover"))
    return passed


def groupable(world, element: CytoElement, family: str) -> Bond | None:
    """"is it groupable? (bonded to any neighbors in this way?)" [M86 p. 66]: a strong
    adjacent bond of the family to a neighbour that is free to be grouped. A glint's
    neighbours are its `left-nbr` and `right-nbr` glints [M86 p. 61], which must still be
    top-level; a glom's are the top-level elements beside it (DECISION D-020, from the second
    scout of [M86 p. 173], which proposes no group for `glint3` although `glom1` is bonded
    to it by adjacent predecessorship). Returns the first such bond, in bond order."""
    left, right = cy.neighbours(world, element)
    allowed = [x for x in (left, right) if x is not None]
    if element.is_glint:
        allowed = [x for x in allowed if x.is_glint and x.name in (element.left_nbr,
                                                                     element.right_nbr)]
    for bond in strong_bonds(world, element, family):
        if bond.modifier == "adjacent" and any(bond.other(element) is x for x in allowed):
            return bond
    return None


def coverable(world, element: CytoElement) -> bool:
    """"is it coverable? (bonded into a symmetry group?)" [M86 p. 66]: the element is one end
    of a remote sameness bond (the `3` of `3 7 3` [M86 p. 165], the `1` of `1 2 1`
    [M86 p. 173], whichever end the scout chose) or the centre of one (DECISION D-020)."""
    return (any(bond.modifier == "remote" for bond in strong_bonds(world, element, "Same"))
            or centre_of_symmetry(world, element))


def centre_of_symmetry(world, element: CytoElement) -> bool:
    """Whether the elements on either side of this one are bonded to each other by remote
    sameness, so that it is the centre of a budding symmetry group: the 6 of `5 3 1 6 1 3 4`
    [M86 p. 67] (DECISION D-020)."""
    left, right = cy.neighbours(world, element)
    if left is None or right is None:
        return False
    return any(bond.modifier == "remote" and bond.other(left) is right
               for bond in strong_bonds(world, left, "Same"))


def fenceable(world, element: CytoElement, family: str) -> Bond | None:
    """"is it fenceable? (are there remote gloms to which it is bonded?)" [M86 p. 66]: the
    first strong remote bond of the family on the element, whichever side its partner is on
    (the Glomtester decides whether a fence can be cut from it, DECISION D-021)."""
    for bond in strong_bonds(world, element, family):
        if bond.modifier == "remote":
            return bond
    return None


def glomtester_urgency(world, label: str, test: str) -> int:
    """"The Glomtester's weight (urgency) is dependent on the test ('groupable' being
    strongest) and the bond-type involved (sameness being stronger than successorship, and
    so on)" [M86 p. 66]: the test's weight times the family's (DECISION D-020)."""
    config = world.config
    return (dict(config.glomtester_test_weights)[test]
            * dict(config.glomtester_family_weights)[cy.FAMILIES[label]])


def propose(world, label: str, test: str, element: CytoElement) -> None:
    """Print the proposal and set out the Glomtester with the urgency of DECISION D-020:
    `Same -cover proposed --> glint1` then `doing task Glomtester on (Same cover glint1)`
    [M86 p. 165]."""
    world.events.publish(events.GlomProposed(label, test, element.name))
    world.post("Glomtester", label, test, element.name,
               urgency=glomtester_urgency(world, label, test))


# ---- the Glomtester -----------------------------------------------------------------------

def glomtester(world, label: str, test: str, target: str) -> None:
    """The Glomtester task: "When invoked, a Glomtester task must first be certain that the
    glom it is supposed to test is still in the cytoplasm. If so, it must then determine the
    extent of the evolving glom. The thrust here is to get maximally-sized gloms"
    [M86 p. 67]. A target that is gone, or no longer free, prints `Glomtester failed glint2`
    [M86 p. 170]. "The Glomtester either rejects the group as a glom or creates a Glommer task
    to refine the group and perform the final glomming" [M86 p. 67]: the extent is
    `extent`'s (DECISION D-021), a rejection is silent, and the Glommer is set out with the
    members, `(Same print-value fence (glint1 glint2))` [M86 p. 165], and with this
    Glomtester's own urgency, so that the scan's bias carries up to the glomming
    (DECISION D-021)."""
    element = world.find(target)
    if (not isinstance(element, CytoElement) or not element.top_level
            or element.pseudo):
        world.events.publish(events.GlomtesterFailed(target))
        world.stale("Glomtester", (label, test, target), "the target is not free")
        return
    members = extent(world, element, cy.FAMILIES[label], test)
    if members is None or not usable(members):
        world.stale("Glomtester", (label, test, target), "the group is rejected")
        return
    world.post("Glommer", label, "print-value", test, tuple(x.name for x in members),
               urgency=glomtester_urgency(world, label, test))


def extent(world, element: CytoElement, family: str, test: str) -> list | None:
    """The extent of the evolving glom around a top-level element, as the lineup elements
    it would take, or None [M86 p. 67] (DECISION D-021). The four examples: in `2 1 1 1 3`
    the second 1 by sameness gives `1 1 1`; in `9 1 2 5 9` the first 9 by fence gives
    `9 1 2 5`; in `5 3 1 6 1 3 4` the 6 by symmetry gives `3 1 6 1 3`; in `1 2 3 2 3` the
    first 2 by the pred-succ family gives `1 2 3 2 3` [M86 p. 67]."""
    line = cy.lineup(world)
    index = next((i for i, x in enumerate(line) if x is element), None)
    if index is None:
        return None
    if test == "group":
        return group_extent(world, line, index, family)
    if test == "fence":
        return fence_extent(world, line, index, family)
    if test == "cover":
        return cover_extent(world, line, index)
    raise ValueError(f"unknown test {test!r}")


def group_extent(world, line: list, index: int, family: str) -> list:
    """The maximal chain of neighbours linked by adjacent bonds of the family: "all
    'chains' that consist of gloms related to their neighbors by some element of a bond
    family" [M86 p. 68], so `1 2 3 2 3` is one pred-succ chain [M86 p. 67]
    (DECISION D-021)."""
    left = right = index
    while left > 0 and linked(world, line[left - 1], line[left], family) is not None:
        left -= 1
    while right + 1 < len(line) and linked(world, line[right], line[right + 1], family) is not None:
        right += 1
    return line[left:right + 1]


def fence_extent(world, line: list, index: int, family: str) -> list | None:
    """From the element up to, not including, its nearest remote partner of the family to
    the right: `9 1 2 5` for the first 9 of `9 1 2 5 9` [M86 p. 67]; the partner must be a
    top-level element, and an element with no partner to its right (the second 9, or
    `glint3` of `1 2 1` [M86 p. 173]) cuts no fence (DECISION D-021)."""
    element = line[index]
    partners = []
    for bond in strong_bonds(world, element, family):
        if bond.modifier != "remote":
            continue
        other = bond.other(element)
        position = next((i for i, x in enumerate(line) if x is other), None)
        if position is not None and position > index:
            partners.append(position)
    if not partners:
        return None
    return line[index:min(partners)]


def cover_extent(world, line: list, index: int) -> list | None:
    """The symmetric window around the element, as wide as the mirror elements stay equal:
    `3 1 6 1 3` for the 6 of `5 3 1 6 1 3 4` [M86 p. 67]. From an end of a remote sameness
    bond, the window between the two ends must read the same both ways (both ends top-level,
    [M86 p. 173] rejects `glint3`'s when `glint1` is under `glom1`), and is then widened
    likewise (DECISION D-021)."""
    element = line[index]
    for bond in strong_bonds(world, element, "Same"):
        if bond.modifier != "remote":
            continue
        other = next((i for i, x in enumerate(line) if x is bond.other(element)), None)
        if other is None:
            continue
        left, right = min(index, other), max(index, other)
        if _symmetric(line[left:right + 1]):
            return _widened(line, left, right)
    if centre_of_symmetry(world, element):
        return _widened(line, index, index)
    return None


def _symmetric(elements) -> bool:
    values = [x.print_value for x in elements]
    return values == values[::-1]


def _widened(line: list, left: int, right: int) -> list:
    while left > 0 and right + 1 < len(line) \
            and line[left - 1].print_value == line[right + 1].print_value:
        left, right = left - 1, right + 1
    return line[left:right + 1]


# ---- the Glommer --------------------------------------------------------------------------

def glommer(world, label: str, field: str, test: str, members) -> None:
    """The Glommer task: "When invoked, the Glommer will do a bit of 'bookkeeping'. It
    creates the new glom and makes it an active focus -- a site of increased system activity.
    It also creates a Glom-inspector task to continue pushing the glom up to higher levels"
    [M86 p. 67]. It prints `Glommer for Same print-value fence members: (glint1 glint2)`
    [M86 p. 165]; the refinement and the failure (`failed to glom (glint1 glint2 glint3)`
    [M86 p. 165]) are DECISION D-021: members no longer free are dropped from each end, and
    the glom is not made when fewer than two remain or the rest are not free, adjacent and
    real. The new glom, as a new active focus, gets its Sparkler-plus tasks like a new glint
    (`Sparkler-plus on (glom1 glom1 10)` [M86 p. 165]; DECISION D-015)."""
    names = tuple(members)
    elements = [world.find(name) for name in names]
    if not all(isinstance(x, CytoElement) for x in elements):
        _fail(world, names, "a member is gone")
        return
    while elements and not elements[0].top_level:
        elements.pop(0)
    while elements and not elements[-1].top_level:
        elements.pop()
    if (not usable(elements) or not all(x.top_level for x in elements)
            or not all(cy.adjacent(a, b) and a.start_position < b.start_position
                       for a, b in zip(elements, elements[1:]))):
        _fail(world, names, "the members are no longer a free, adjacent run")
        return
    glom = cy.make_glom(world, (label, field, test), elements)
    glom.active = True
    world.events.publish(events.GlomMade(glom.name, glom.type, tuple(x.name for x in elements),
                                         glom.print_value))
    for other in reversed(cy.elements(world)):
        urgency = bonds.sparkler_plus_urgency(world, glom, other)
        world.post("Sparkler-plus", glom.name, other.name, urgency, urgency=urgency)
    world.post("Glom-inspector", glom.name)


def _fail(world, names: tuple, reason: str) -> None:
    world.events.publish(events.GlomFailed(names))
    world.stale("Glommer", names, reason)


# ---- the Glom-inspector -------------------------------------------------------------------

def glom_inspector(world, name: str) -> None:
    """The Glom-inspector task: "A Glom-inspector determines which, if any, plato-classes
    might find the given glom 'interesting' -- which classes might possibly consider it a
    'manifestation' of themselves. If there are any such classes, the Glom-inspector then
    creates a Plato-scout task, giving it the glom in question and the names of the
    'interested' (candidate) plato-classes" [M86 p. 68]: `Plato-scout on (Cycle glom1)`,
    `((S-group P-group) glom1)` [M86 pp. 165, 170] (DECISION D-022)."""
    glom = world.find(name)
    if not isinstance(glom, Glom):
        world.stale("Glom-inspector", (name,), "the glom is gone")
        return
    classes = interested(glom)
    if not classes:
        world.stale("Glom-inspector", (name,), "no class is interested")
        return
    world.post("Plato-scout", classes[0] if len(classes) == 1 else classes, name)


def interested(glom: Glom) -> tuple:
    """The candidate classes for a glom, by its type (DECISION D-022): a sameness group
    interests C-group and Y-group ("(1 1 1) ... might be dubbed as both" [M86 p. 69]), a
    pred-succ group S-group and P-group, a cover Y-group, a fence Cycle, and a top-down glom
    (a pseudo-glom, or a glom with no bottom-up type) all six [M86 p. 173]."""
    if glom.pseudo or len(glom.type) != 3:
        return ALL_SIX
    label, _, test = glom.type
    return INTEREST.get((cy.FAMILIES.get(label, label), test), ())


# ---- the Plato-scout and dubbing ---------------------------------------------------------------

def plato_scout(world, classes, name: str) -> None:
    """The Plato-scout task: "If the glom still exists when the Plato-scout is invoked, the
    scout begins its work" [M86 p. 68]. "A Plato-scout is given a glom and a list of
    candidate plato-classes. It applies the process function for each candidate class to
    the glom's print-value. If the glom passes the test, then the glom is pure and will be
    dubbed a manifestation of the candidate class. A glom can be dubbed more than once"
    [M86 pp. 68-69]; a fence class passes a flat glom "by default" [M86 p. 69]. A class whose
    test fails sets out a Plato-evaluator for that class (`Plato-evaluator on (P-group
    glom1)` after `(1 2)` was dubbed S-group [M86 p. 170]): "Any glom that does not pass even
    one of its process tests is cause for the creation of a Plato-evaluator task"
    [M86 p. 69]. Only a flat glom is "pure" [M86 p. 69], so a non-flat glom goes to the
    Plato-evaluator for every class (DECISION D-023)."""
    glom = world.find(name)
    if not isinstance(glom, Glom):
        world.stale("Plato-scout", (classes, name), "the glom is gone")
        return
    for kind in (classes,) if isinstance(classes, str) else tuple(classes):
        if glom.flat and pl.passes(kind, glom.print_value, flat=True):
            dub(world, glom, kind, "pure")
        else:
            world.post("Plato-evaluator", kind, name)


def dub(world, glom: Glom, kind: str, purity: str, scout: bool = True) -> Form | None:
    """Dub a glom: "1) the glom's structure field is modified to indicate a) new structure:
    (platotype [start-val] [length] [value]) e.g., (C-group 1 3) or (Cycle (2 9 8 1))
    b) purity: pure <--> an exact manifestation; flat; e.g., (1 1 1) secondary <--> not
    flat; e.g., ((1 5) (1 5) (1 5)) 2) the Platonic ideal has this glom added to its list of
    manifestations" [M86 p. 69]. The structure field holds one `(form purity)` pair per
    dubbing (`form: (((Cycle 3 2 (3 7)) pure))` [M86 p. 168]). "Dubbing causes creation of a
    Template-scout process" [M86 p. 69], unless `scout` is off, as it is for the unit gloms a
    template makes from above (DECISION D-061). A glom already dubbed of the class is not
    dubbed again; returns the form, or None then."""
    if any(form.kind == kind for form, _ in glom.structure or ()):
        return None
    form = dub_form(glom, kind)
    glom.structure = list(glom.structure or ()) + [(form, purity)]
    world.platoplasm.manifest(kind, glom)
    world.events.publish(events.GlomDubbed(glom.name, kind, str(form), purity))
    if scout:
        world.post("Template-scout", glom.name)
    return form


def dub_form(glom: Glom, kind: str) -> Form:
    """The structure a dubbing records, in the program's spelling: `(S-group 1 2)` for
    `(1 2)` [M86 p. 171], `(Cycle 3 2 (3 7))` for `(3 7)` [M86 p. 165]: a group class takes
    the first unit as its start and the number of units as its length [M86 pp. 51-52], a
    Y-group, Cycle or Tuple takes the units as its elements with the derived start and length
    [M86 pp. 52-53] (DECISION D-001). A unit that is a glom is written by its own dubbed form
    when it has one, else as a Tuple of its print-value (DECISION D-023)."""
    units = tuple(unit_form(sub) for sub in glom.subgloms)
    if kind in ("C-group", "S-group", "P-group"):
        return Form(kind, units[0], len(units))
    return Form(kind, start_value(units[0], units), len(units), units)


def unit_form(element: CytoElement):
    """A subglom as an element of a form: a glint's value; a dubbed glom's first form; an
    undubbed glom as a Tuple of its units (DECISION D-023)."""
    if element.is_glint:
        return element.print_value
    if element.structure:
        return element.structure[0][0]
    units = tuple(unit_form(sub) for sub in element.subgloms)
    return Form("Tuple", start_value(units[0], units), len(units), units)


# ---- the Plato-evaluator and the Burster ----------------------------------------------------

def plato_evaluator(world, kind: str, name: str) -> None:
    """The Plato-evaluator task: "When invoked, the Plato-evaluator examines the glom a bit
    further, looking for dubbable gloms of secondary purity (non-flat) and for pure
    subcollections of gloms within the proposed target glom. Thus '1 2 3 2 3' might well be
    sent to a Plato-evaluator, which might break it into the two pure successorship groups
    '1 2 3' and '2 3', and send on the gloms for these two groups to be dubbed" [M86 p. 69].
    "If the Plato-evaluator has no success and the glom has not already been dubbed as a
    manifestation of some other platotype, the scene is set for its destruction. A Burster
    task is created to destroy the glom and all its non-glint subgloms" [M86 p. 69]
    (DECISION D-023): a non-flat glom whose units pass the class's process is dubbed with
    secondary purity; a flat, undubbed glom is cut into pieces where the link type changes,
    and when a proper piece passes the process of a class interested in its family, the glom
    is dissolved and a Glommer set out for each such piece. A glom that has meanwhile been
    glommed into another has escaped [M86 p. 70] and is left alone."""
    glom = world.find(name)
    if not isinstance(glom, Glom):
        world.stale("Plato-evaluator", (kind, name), "the glom is gone")
        return
    if glom.parent is not None:
        world.stale("Plato-evaluator", (kind, name), "the glom escaped into another glom")
        return
    if not glom.flat:
        if kind in pl.PROCESSES and pl.PROCESSES[kind](glom.print_value):
            dub(world, glom, kind, "secondary")
            return
    elif not glom.dubbed:
        pieces = pure_pieces(world, glom)
        if pieces:
            cy.dissolve(world, glom)
            world.events.publish(events.GlomDissolved(name, tuple(x.name for x in glom.subgloms)))
            for label, members in pieces:
                world.post("Glommer", label, "print-value", "group",
                           tuple(x.name for x in members))
            return
    if not glom.dubbed:
        world.post("Burster", name)


def pure_pieces(world, glom: Glom) -> list:
    """The pure subcollections of a flat glom, as (label, members) pairs (DECISION D-023):
    the members are cut wherever the type of the adjacent print-value bond between
    neighbours changes (or there is none), and a piece shorter than the whole, of two or
    more members, is kept when its values pass the process of a class interested in a group
    of its family: `1 2 3 2 3` gives `1 2 3` and `2 3` (the `3 2` piece passes P-group and
    is kept too; the Glommers decide, [M86 p. 69])."""
    members = list(glom.subgloms)
    links = [_link_type(world, a, b) for a, b in zip(members, members[1:])]
    pieces, start = [], 0
    for i in range(1, len(links) + 1):
        if i == len(links) or links[i] != links[start]:
            label = links[start]
            piece = members[start:i + 1]
            if label is not None and len(piece) < len(members):
                values = [x.print_value for x in piece]
                classes = INTEREST[(cy.FAMILIES[label], "group")]
                if any(pl.passes(c, values) for c in classes):
                    pieces.append((label, piece))
            start = i
    return pieces


def _link_type(world, a: CytoElement, b: CytoElement) -> str | None:
    for bond in strong_bonds(world, a):
        if bond.modifier == "adjacent" and bond.field == "print-value" and bond.other(a) is b:
            return bond.type
    return None


def burster(world, name: str) -> None:
    """The Burster task: "A Burster task is even more destructive of glom structure. If a
    Burster were set on the glom '((1 1) (2 2))', all levels of glomming would be destroyed,
    leaving only the glints '1', '1', '2', '2' in the cytoplasm" [M86 p. 70]. It does
    nothing when the glom is gone, or has since been dubbed or glommed into another glom
    ("the glom's only escape route is to become invisible in the cytoplasm by glomming with
    another glom" [M86 p. 70]; DECISION D-024)."""
    glom = world.find(name)
    if not isinstance(glom, Glom):
        world.stale("Burster", (name,), "the glom is gone")
        return
    if glom.dubbed or glom.parent is not None or glom.pseudo:
        world.stale("Burster", (name,), "the glom escaped")
        return
    glints = cy.burst(world, glom)
    world.events.publish(events.GlomBurst(name, tuple(x.name for x in glints)))


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Glom-scout": glom_scout,
    "Glomtester": glomtester,
    "Glommer": glommer,
    "Glom-inspector": glom_inspector,
    "Plato-scout": plato_scout,
    "Plato-evaluator": plato_evaluator,
    "Burster": burster,
}

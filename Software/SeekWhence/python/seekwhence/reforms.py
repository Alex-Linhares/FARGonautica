"""The Reformulator and the Bond-assessor: the threshold, neighbour pulls, moves and PROGRAMs
[M86 pp. 105-109, 124, 174, 177-178].

"At this point, a reigning class has been established -- or reconfirmed -- and so a
Reformulator process is placed on the taskrack. When invoked, this process will attempt to
find salient bonds and will set out Gnoth-operator tasks designed to act upon the bond pulls
or pushes in order to change the gnoths" [M86 p. 105]. Its steps, in the thesis's order: the
threshold ("either 1 more than the existing threshold value or, if none exists, 80% of the
strongest pull-bond strength for the reigning type" [M86 p. 105]); the assessment of every
bond in the reigning environment [M86 p. 105]; the search for a pull between the "lasts" of a
gnoth and the "firsts" of its right neighbour in the order of p. 107 [M86 pp. 106-107]; the
direction of the move and its strength [M86 pp. 108-109]; the operation or PROGRAM for a
Gnoth-operator [M86 pp. 107, 121-123]; and a Bond-assessor, which decides between another
Reformulator and a Gnoth-caster [M86 p. 124]. The threshold is one number per reformulation
episode, raised by 1 by each Reformulator and lowered by 1 by each Gnoth-operator that acts,
"cooling" and "heating" the environment until "the system will settle as the Reformulators
find fewer and fewer relevant operations to suggest" [M86 p. 124]: the transcript's `(S-group
8.0)`, `(S-group 9.0)`, `(S-group 10.0)` [M86 pp. 174, 177-178].

The decisions: the pull between two gloms, the holding pull and the direction (DECISION
D-043), the PROGRAM for a deep move (DECISION D-044), when NO-OP is proposed (DECISION
D-045), when the threshold is cleared (DECISION D-046), the Bond-assessor's test (DECISION
D-047) and the order over the parse (DECISION D-068). Nothing here draws from the `Rng`: the
Reformulator is deterministic given the bonds and the gnoths, and the taskrack's draw decides
only when it runs. The threshold is a feedback number owned by the run, read by whoever
needs it, not a schedule; a task checks what it needs, then builds, and a task that finds
nothing fizzles without changing anything.
"""

from dataclasses import dataclass

from seekwhence import cytoplasm as cy
from seekwhence import events
from seekwhence import gnoths as gn
from seekwhence import slipping
from seekwhence.cytoplasm import CytoElement
from seekwhence.operations import New, operation_text


# ---- the threshold [M86 pp. 105, 124] (DECISION D-046) ------------------------------------------

def set_threshold(world) -> float:
    """"The Reformulator's first act is to determine a threshold bond strength. Bonds or bond
    groups exerting pressures below this threshold will be ignored. Currently, the new
    threshold is set to either 1 more than the existing threshold value or, if none exists,
    80% of the strongest pull-bond strength for the reigning type" [M86 p. 105]: 8.0 for
    S-group, whose strongest pull is 10 [M86 p. 174], then 9.0 and 10.0 [M86 p. 178]. The
    fraction is `Config.threshold_fraction`; the Gnoth-setter and a slip clear the threshold
    so that each episode starts afresh (DECISION D-046): a feedback number, not a schedule
    that only goes down."""
    if world.threshold is None:
        world.threshold = (world.config.threshold_fraction
                           * world.platoplasm.strongest_pull(world.reigning))
    else:
        world.threshold += 1
    return world.threshold


# ---- lasts, firsts and the selection order [M86 pp. 106-107] --------------------------------------

def lasts(gnoth: gn.Gnoth) -> list:
    """The "lasts" of a gnoth: its rightmost top-level glom, that glom's rightmost subglom,
    and so on down to a glint; Figure 2's `(glom15 glom10 glom7 glint4)` [M86 p. 106]."""
    return _chain(gnoth, -1)


def firsts(gnoth: gn.Gnoth) -> list:
    """The "firsts" of a gnoth: its leftmost top-level glom and the leftmost chain below it;
    Figure 2's `(glom8 glom3 glom1 glint5)` [M86 p. 106]."""
    return _chain(gnoth, 0)


def _chain(gnoth: gn.Gnoth, side: int) -> list:
    collection = gnoth.collection
    if not collection:
        return []
    chain = [collection[side]]
    while not chain[-1].is_glint and chain[-1].subgloms:
        chain.append(chain[-1].subgloms[side])
    return chain


def selection_order(left_chain, right_chain) -> list:
    """The order in which the pulls between two neighbouring gnoths are assessed, as p. 107
    lists it for Figure 2: level 1 `glom15 <--> glom8`; level 2 `glom10 <--> glom8`, `glom15
    <--> glom3`, `glom10 <--> glom3`; level 3 `glom7 <--> (glom8 glom3)`, `(glom15 glom10)
    <--> glom1`, `glom7 <--> glom1`; level 4 likewise with glint4 and glint5 [M86 p. 107]:
    "The first moves considered are those at the highest level, under the theory that if a
    glom wants to move, its subgloms should follow" [M86 p. 107]. Returns `(level, lefts,
    rights)` triples, `lefts` and `rights` the lists to "assess pull with each in the list"
    [M86 p. 107]; a level a chain does not reach contributes nothing on that side."""
    order = []
    depth = max(len(left_chain), len(right_chain))
    for k in range(1, depth + 1):
        new_left = left_chain[k - 1] if k <= len(left_chain) else None
        new_right = right_chain[k - 1] if k <= len(right_chain) else None
        if k == 1:
            if new_left is not None and new_right is not None:
                order.append((1, [new_left], [new_right]))
            continue
        if new_left is not None and right_chain[:k - 1]:
            order.append((k, [new_left], list(right_chain[:k - 1])))
        if new_right is not None and left_chain[:k - 1]:
            order.append((k, list(left_chain[:k - 1]), [new_right]))
        if new_left is not None and new_right is not None:
            order.append((k, [new_left], [new_right]))
    return order


# ---- pulls and holdings [M86 pp. 108-109] (DECISION D-043) ------------------------------------

def within(element: CytoElement, member: CytoElement) -> bool:
    """Whether `member` is `element` or lies somewhere under it: what moves with a glom
    (DECISION D-043)."""
    while member is not None:
        if member is element:
            return True
        member = member.parent
    return False


def pull(world, kind: str, a: CytoElement, b: CytoElement) -> float:
    """The mutual attraction of two gloms: the sum of the positive values, in the environment
    of `kind`, of the bonds between the two themselves ("the gloms' mutual attraction"
    [M86 p. 108]; DECISION D-043). A bond on a nested glom is that glom's own pull, found
    at its level of p. 107's order: "the decisive pull on it may be coming from a
    deeply-nested glom, one several levels down from the top. In Figure 2 for example,
    glint4 may be pulled toward glom3" [M86 p. 108]. Figure 1's 2 and 3: the adjacent
    successor bond, 10 under S-group [M86 pp. 101-102, 108]."""
    total = 0.0
    for bond in slipping.bonds(world):
        x, y = bond.members
        if (x is a and y is b) or (x is b and y is a):
            total += max(world.platoplasm.bond_value(kind, bond), 0)
    return total


def holding(world, kind: str, element: CytoElement, gnoth: gn.Gnoth) -> float:
    """"the pull exerted by other gloms to hold them in place" [M86 p. 108]: the sum of the
    positive values of the bonds between `element` (or anything within it) and the other
    elements of its own gnoth (DECISION D-043). A single glom alone in its gnoth has none,
    "Single gloms are the most likely to move" [M86 p. 108]; the 2 of Figure 1's `(1 2)` is
    held by 10 [M86 p. 102]."""
    total = 0.0
    for bond in slipping.bonds(world):
        x, y = bond.members
        for mine, other in ((x, y), (y, x)):
            if within(element, mine) and not within(element, other) \
                    and slipping.owner(world, other) is gnoth:
                total += max(world.platoplasm.bond_value(kind, bond), 0)
                break
    return total


@dataclass(frozen=True)
class Move:
    """A neighbour pull strong enough to act on [M86 pp. 107-109]: the `level` of p. 107's
    order, the `left` and `right` gloms (the left one in `left_gnoth`, the right one in its
    neighbour `right_gnoth`), their mutual `pull`, which `mover` goes (`left` or `right`),
    its holding and the operation's `strength`, the pull minus the mover's holding
    (DECISION D-043)."""

    level: int
    left: CytoElement
    right: CytoElement
    left_gnoth: gn.Gnoth
    right_gnoth: gn.Gnoth
    pull: float
    mover: str
    held: float
    strength: float

    @property
    def moving(self) -> CytoElement:
        """The glom that moves (DECISION D-043)."""
        return self.left if self.mover == "left" else self.right

    @property
    def target(self) -> CytoElement:
        """The glom it is pulled by (DECISION D-043)."""
        return self.right if self.mover == "left" else self.left


def size(element: CytoElement) -> int:
    """How many terms an element covers, the tie-breaker between two movers
    (DECISION D-043)."""
    return element.span


def assess_pair(world, kind: str, level: int, a: CytoElement, b: CytoElement,
                left_gnoth: gn.Gnoth, right_gnoth: gn.Gnoth) -> Move | None:
    """The move a pair of gloms from neighbouring gnoths would make, or None when nothing
    pulls them together: "The system must decide which of the two attracting gloms is to
    move and which is to stay put. This is determined by an analysis of the bonds holding
    the gloms in their respective gnoths" [M86 p. 108]. The glom with the smaller holding
    moves; a tie goes to the one covering fewer terms, then to the left one, moving right,
    as the transcript's `SHIFT-RIGHT gnoth1 gnoth2 (glint1)` on two single glints [M86
    p. 174] (DECISION D-043). The strength is the pull minus the mover's holding
    [M86 pp. 108-109]."""
    attraction = pull(world, kind, a, b)
    if attraction <= 0:
        return None
    held_left = holding(world, kind, a, left_gnoth)
    held_right = holding(world, kind, b, right_gnoth)
    if (held_right, size(b)) < (held_left, size(a)):
        mover, held = "right", held_right
    else:
        mover, held = "left", held_left
    return Move(level, a, b, left_gnoth, right_gnoth, attraction, mover, held,
                attraction - held)


def best_of(world, kind: str, level: int, lefts, rights, left_gnoth, right_gnoth) -> Move | None:
    """"assess pull with each in the list" [M86 p. 107]: of the pairs a group of the order
    offers, the move with the greatest strength, ties to the first listed (DECISION
    D-043)."""
    best = None
    for a in lefts:
        for b in rights:
            move = assess_pair(world, kind, level, a, b, left_gnoth, right_gnoth)
            if move is not None and (best is None or move.strength > best.strength):
                best = move
    return best


def find_move(world, kind: str, threshold: float) -> Move | None:
    """The first move strong enough: "As soon as some reformulation is strong enough -- the
    bond pulls and pushes supporting it exceed the threshold -- the Reformulator creates an
    appropriate gnoth operation or program of operations and sets a Gnoth-operator on the
    taskrack to carry it out. The Reformulator will not suggest any further moves, since
    any others would occur at a lower structural level" [M86 pp. 107-108]. So the levels of
    p. 107's order are taken one at a time over every pair of neighbouring gnoths, left to
    right (DECISION D-068), and the first group whose best move is strictly stronger than
    the threshold wins (DECISION D-043). The catchall is never a neighbour
    (DECISION D-030)."""
    listed = [g for g in gn.gnoths(world) if not g.catchall and g.glom is not None]
    pairs = []
    for left_gnoth, right_gnoth in zip(listed, listed[1:]):
        order = selection_order(lasts(left_gnoth), firsts(right_gnoth))
        pairs.append((left_gnoth, right_gnoth, order))
    depth = max((len(order) and order[-1][0] for _, _, order in pairs), default=0)
    for level in range(1, depth + 1):
        for left_gnoth, right_gnoth, order in pairs:
            for k, lefts, rights in order:
                if k != level:
                    continue
                move = best_of(world, kind, level, lefts, rights, left_gnoth, right_gnoth)
                if move is not None and move.strength > threshold:
                    return move
    return None


# ---- the operation or PROGRAM for a move [M86 pp. 108, 121-123] (DECISION D-044) --------------

def ancestors(element: CytoElement, gnoth: gn.Gnoth) -> list:
    """The gloms containing an element inside its gnoth, innermost first, the cap left out:
    the gloms an operation must burst "so that it can rise to the top of the cytoplasm"
    [M86 p. 109]."""
    chain = []
    parent = element.parent
    while parent is not None and parent is not gnoth.glom:
        chain.append(parent)
        parent = parent.parent
    return chain


def build_program(move: Move):
    """The operation or PROGRAM that carries out a move (DECISION D-044). A pull between two
    top-level gloms is "translated into the move: (SHIFT-LEFT gnoth3 gnoth4 (glom8))"
    [M86 p. 122], one bare operation ("When more than a straightforward top-level move is
    to be required, a Reformulator must create a PROGRAM" [M86 p. 121]), as the transcript's
    `((SHIFT-RIGHT gnoth3 gnoth5 (glint3)))` [M86 p. 177]. Otherwise the PROGRAM of p. 122:
    the shift of the mover; a CAPTURE by the target's parent glom when the target is nested
    (`(CAPTURE-RIGHT gnoth2 glom2 glint5)` puts the 3 beside the 2 inside `(1 2)`
    [M86 p. 122]); then "the remainder of the PROGRAM is designed simply and mechanically to
    repair any concomitant structural damage ... by re-enclosing the remaining gloms at each
    intermediate level" [M86 pp. 122-123]: one ENCLOSE per burst glom, innermost first, on
    the mover's side and then on the target's, over that glom's remaining subgloms and the
    glom the previous step made (`New`), or `nil` when nothing remains to enclose, "no
    repair necessary" [M86 p. 122]."""
    mover, target = move.moving, move.target
    if move.mover == "left":
        giver, taker = move.left_gnoth, move.right_gnoth
        shift = ("SHIFT-RIGHT", giver.name, taker.name, (mover.name,))
        capture_kind = "CAPTURE-LEFT"
    else:
        giver, taker = move.right_gnoth, move.left_gnoth
        shift = ("SHIFT-LEFT", taker.name, giver.name, (mover.name,))
        capture_kind = "CAPTURE-RIGHT"
    above_mover = ancestors(mover, giver)
    above_target = ancestors(target, taker)
    if not above_mover and not above_target:
        return shift
    steps = [shift]
    burst_on_taker: list = []
    if above_target:
        captor = above_target[0]
        steps.append((capture_kind, taker.name, captor.name, mover.name))
        burst_on_taker = above_target[1:]
        last_new, removed = New(len(steps) - 1), captor
    _repairs(steps, giver, above_mover, mover)
    if above_target:
        _repairs(steps, taker, burst_on_taker, removed, last_new)
    return ("PROGRAM", tuple(steps))


def _repairs(steps: list, gnoth: gn.Gnoth, burst: list, removed, replacement=None) -> None:
    """Append one ENCLOSE per burst glom, innermost first, over the glom's subgloms other
    than the one taken out (`removed`) plus the glom the previous step made, if any
    (DECISION D-044)."""
    for glom in burst:
        remaining = [sub.name for sub in glom.subgloms if sub is not removed]
        if replacement is not None:
            remaining.append(replacement)
        if remaining:
            steps.append(("ENCLOSE", gnoth.name, tuple(remaining)))
            replacement = New(len(steps) - 1)
        else:
            steps.append(("ENCLOSE", gnoth.name, None))
            replacement = None
        removed = glom


# ---- NO-OP [M86 p. 113] (DECISION D-045) --------------------------------------------------------

def internal_push(gnoth: gn.Gnoth) -> float:
    """The pushes inside a gnoth, by absolute value, from the list `assess_bonds` filled
    [M86 p. 100]: the one pressure no inter-gnoth move relieves (DECISION D-045)."""
    return sum(-value for _, value in (gnoth.internal_bonds or ()) if value < 0)


def find_no_op(world, threshold: float):
    """The NO-OP a Reformulator proposes when no neighbour pull is strong enough: `(NO-OP
    gnoth)` for the gnoth whose internal pushes exceed the threshold, the largest first
    (DECISION D-045); the gnoth is uncapped until a Capper recaps it, "This 'slow-recap'
    permits natural glomming to occur within the gnoth" [M86 p. 113]. None when no gnoth
    qualifies."""
    best, pressure = None, threshold
    for gnoth in gn.gnoths(world):
        if gnoth.catchall or gnoth.glom is None:
            continue
        pushes = internal_push(gnoth)
        if pushes > pressure:
            best, pressure = gnoth, pushes
    return None if best is None else ("NO-OP", best.name)


# ---- the Reformulator task [M86 pp. 105-109] --------------------------------------------------

def reformulator(world, *args) -> None:
    """The Reformulator task, `doing task Reformulator` [M86 p. 174]: sets the threshold
    (`set_threshold`, [M86 p. 105]); "Because new bonds may have been established since the
    Reformulator's creation time, its next act is to assess all bond pulls in the environment
    of the reigning class" [M86 p. 105] (`slipping.assess_bonds`); finds the first move
    strong enough (`find_move`) and sets out a Gnoth-operator with its operation or PROGRAM
    (`build_program`), or a NO-OP (`find_no_op`, DECISION D-045), or nothing; and "If the
    Reformulator finds no actions to be taken or if it has completed its recommendations, it
    hangs a Bond-assessor task on the taskrack (to determine system 'happiness') and
    terminates" [M86 p. 105], the Bond-assessor carrying the reigning class and the
    threshold, `(S-group 8.0)` [M86 p. 174]. Nothing is done without a reigning class or
    once a new hypothesis exists (DECISION D-009)."""
    del args
    kind = world.reigning
    if kind is None:
        world.stale("Reformulator", (), "no reigning class")
        return
    if world.hypothesis is not None:
        world.stale("Reformulator", (), "a hypothesis exists")
        return
    threshold = set_threshold(world)
    slipping.assess_bonds(world, kind)
    move = find_move(world, kind, threshold)
    program = None
    if move is not None:
        program = build_program(move)
        world.events.publish(events.MoveFound(
            move.level, move.left.name, move.right.name, move.left_gnoth.name,
            move.right_gnoth.name, move.pull, move.mover, move.held, move.strength,
            operation_text(program)))
    else:
        program = find_no_op(world, threshold)
    if program is not None:
        world.post("Gnoth-operator", program)
    world.post("Bond-assessor", kind, threshold)
    world.events.publish(events.Reformulated(kind, threshold,
                                             None if program is None else operation_text(program)))


# ---- the Bond-assessor task [M86 p. 124] (DECISION D-047) ----------------------------------------

def strongest_bond(world, kind: str) -> float:
    """The largest assessed bond value in the environment of `kind`, by absolute value, over
    "all current bonds" [M86 p. 124]; 0 without bonds."""
    return max((abs(world.platoplasm.bond_value(kind, b)) for b in slipping.bonds(world)),
               default=0)


def bond_assessor(world, kind=None, threshold=None) -> None:
    """The Bond-assessor task, `doing task Bond-assessor on (S-group 8.0)` [M86 p. 174]:
    "The Bond-assessor's job is to look at all current bonds and determine whether or not
    there is reason to continue reformulation. If there are sufficiently strong bonds to
    warrant further reforms, the Bond-assessor places a Reformulator on the taskrack. If
    not, it creates a Gnoth-caster task instead and terminates" [M86 p. 124]. Its arguments
    are the reigning class and the threshold when it was set out; "sufficiently strong" is
    a bond whose value, by absolute value, is strictly greater than that threshold
    (DECISION D-047: on p. 178 a Bond-assessor at 10.0 yields to the Gnoth-caster with bonds
    of 10 present). Nothing is done without a reigning class or once a new hypothesis
    exists (DECISION D-009)."""
    if world.reigning is None:
        world.stale("Bond-assessor", (kind, threshold), "no reigning class")
        return
    if world.hypothesis is not None:
        world.stale("Bond-assessor", (kind, threshold), "a hypothesis exists")
        return
    if kind is None:
        kind = world.reigning
    if threshold is None:
        threshold = world.threshold if world.threshold is not None else 0.0
    strongest = strongest_bond(world, kind)
    more = strongest > threshold
    world.post("Reformulator" if more else "Gnoth-caster")
    world.events.publish(events.BondsAssessed(kind, threshold, strongest, more))


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Reformulator": reformulator,
    "Bond-assessor": bond_assessor,
}

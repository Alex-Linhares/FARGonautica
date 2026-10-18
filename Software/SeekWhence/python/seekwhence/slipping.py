"""Reformulation begins: the Gnoth-setter, freeze-dried hypotheses and the slip-or-stay
decision [M86 pp. 90-92, 98-105, 168, 173-176].

"When an unexpected term is encountered, the system immediately sets the hypothesis'
validity to 0, releases sparks to encourage low-level activity, and places a Gnoth-setter
task on the taskrack. When invoked, the Gnoth-setter carefully fills out gnoths in
accordance with the old hypothesis and calls for the system to reconsider its parse"
[M86 p. 98]. "The first step taken during reconsideration is a bookkeeping measure, saving
the form of the old hypothesis and destroying its box, the home of its active
representation" [M86 p. 99]: the form is "freeze-dried" and kept "on a list of old
hypotheses, along with the number of terms of the sequence it explained" [M86 p. 91]. "The
system then decides whether to stay with the reigning class -- the Platonic class at the
highest level of the (former) hypothesis -- or to slip to a new one" [M86 p. 99]: a strict
class slips at once [M86 p. 99]; otherwise the recent tries are counted [M86 p. 99], every
bond is assessed in the environment of the reigning class [M86 pp. 99-100], the gnoths'
stabilities are summed into the staying pressure [M86 pp. 101-103], each s-link neighbour's
slipping pressure is computed [M86 p. 104], and the largest of them wins the throne only if
it is greater than the staying pressure [M86 pp. 104-105]. The transcripts print the result
as `slip-check: stayval: 15.0` / `best: (Y-group 4.0)` [M86 p. 176].

The layout of the gnoths (DECISION D-036), the repeat check (DECISION D-035), the use of the
recent tries (DECISION D-041), the strict classes (DECISION D-040) and which task prints the
check (DECISION D-042) are decisions; the arithmetic is the thesis's, checked against
Figure 1 [M86 pp. 101-104] and the transcript's own numbers [M86 pp. 168, 176]. Nothing here
draws from the `Rng`. The Reformulator this module sets out, the gnoth operations and the
casting belong to the later modules.
"""

from dataclasses import dataclass

from seekwhence import cytoplasm as cy
from seekwhence import events
from seekwhence import gnoths as gn
from seekwhence.forms import flatten
from seekwhence.hypothesis import FreezeDried, Hypothesis

# The type field of a gnoth-glom made here, as of one the Gnoth-maker makes [M86 p. 78].
GNOTH_GLOM = ("gnoth",)


# ---- the layout of the old hypothesis over the terms [M86 pp. 98-99] (DECISION D-036) --------

@dataclass(frozen=True)
class Frame:
    """One gnoth the Gnoth-setter must fill: the `index` of the hypothesis frame it holds
    (counted from 0, as the program prints `frame: 0` [M86 p. 168]), its first and last
    position `start` and `end` (from 1), the frame's `value` as the old box gave it (None
    for the final gnoth of non-fitting terms [M86 p. 98]) and whether the frame was
    `complete` or cut short by the failing term (DECISION D-036)."""

    index: int
    start: int
    end: int
    value: object
    complete: bool


def layout(form, terms) -> list:
    """Where the old hypothesis puts the gnoths [M86 p. 98] (DECISION D-036): its box is
    fired from the first term, frame by frame (one hit, or one turn of a Cycle, DECISION
    D-004); every complete frame that matches the terms gets a gnoth; a frame cut short by
    the failing term gets a gnoth over the terms that matched; and "Any non-fitting terms
    are collected together in a final gnoth" [M86 p. 98], whose index follows the last
    frame's. The example: `(S-group 1 3)` over `1 2 3 1 2 3 1 2 3 4` gives three frames
    and a final gnoth holding "the trailing 4" [M86 p. 98]; `(Countup 1)` over `1 2 1`
    gives "three gnoths, one for each term" [M86 pp. 98-99]."""
    terms = list(terms)
    hypothesis = Hypothesis(form)
    frames: list = []
    pos, index, empty = 0, 0, 0
    while pos < len(terms):
        value = hypothesis.frame()
        flat = flatten(value)
        if not flat:
            empty += 1
            if empty > 100:
                break
            continue
        fits = 0
        while fits < len(flat) and pos + fits < len(terms) and terms[pos + fits] == flat[fits]:
            fits += 1
        if fits:
            frames.append(Frame(index, pos + 1, pos + fits, value, fits == len(flat)))
            pos += fits
        if fits < len(flat):
            break
        index += 1
    if pos < len(terms):
        final = frames[-1].index + 1 if frames else 0
        frames.append(Frame(final, pos + 1, len(terms), None, False))
    return frames


def terms_explained(frames) -> int:
    """"the number of terms of the sequence it explained" [M86 p. 91]: the terms the
    matching frames cover (DECISION D-035)."""
    return sum(f.end - f.start + 1 for f in frames if f.value is not None)


# ---- filling the gnoths (DECISION D-036) --------------------------------------------------------

def owner(world, element) -> gn.Gnoth | None:
    """The gnoth under which an element lies: the one whose gnoth-glom is the top of the
    element's parent chain, or None when the element is free or in no gnoth ("Its
    internal bonds, those among the gloms it covers" [M86 p. 100])."""
    top = element
    while top.parent is not None:
        top = top.parent
    for gnoth in gn.gnoths(world):
        if gnoth.glom is top:
            return gnoth
    return None


def overlaps(gnoth: gn.Gnoth, frame: Frame) -> bool:
    """Whether a gnoth's range and a frame's share a position (DECISION D-036)."""
    return gnoth.range[0] <= frame.end and frame.start <= gnoth.range[1]


def clear_span(world, start: int, end: int) -> list:
    """Make the positions `start` to `end` a run of top-level elements of their own: any
    element of the lineup that straddles one of the two boundaries is burst to its glints
    (the bottom-up `(1 2)` of run 3 lies across the frames `1` and `2` of `(Countup 1)` and
    is gone afterwards: `show` lists bonds between glints only [M86 p. 174]; DECISION
    D-036). Returns the top-level elements covering the span, left to right."""
    for element in cy.lineup(world):
        if element.is_glint:
            continue
        a, b = element.start_position, element.end_position
        if a < start <= b or a <= end < b:
            cy.burst(world, element)
    return [x for x in cy.lineup(world) if start <= x.start_position and x.end_position <= end]


def fill_gnoth(world, gnoth: gn.Gnoth | None, frame: Frame, kind: str) -> gn.Gnoth:
    """Fill one gnoth to a frame (DECISION D-036): the old gnoth-glom, if any, is
    dissolved, the frame's positions are cleared (`clear_span`) and a new gnoth-glom, a
    pseudo-glom printed `top-down glom glomN` [M86 pp. 173, 176], is put over the elements
    there; an existing gnoth keeps its name and frame and gets the new glom and range, a
    new gnoth is made with the frame's index, the reigning class and the range; either is
    listed as an actualization of the class [M86 p. 77]."""
    if gnoth is not None and gnoth.glom is not None and world.cytoplasm.find(gnoth.glom.name) is gnoth.glom:
        cy.dissolve(world, gnoth.glom)
    members = clear_span(world, frame.start, frame.end)
    glom = cy.make_glom(world, GNOTH_GLOM, members, pseudo=True)
    world.events.publish(events.TopDownGlom(glom.name, tuple(x.name for x in members),
                                            glom.print_value, True))
    new = gnoth is None
    if new:
        gnoth = gn.Gnoth(world.name("gnoth"), frame.index, kind, glom)
        world.socratoplasm.add(gnoth)
    gnoth.glom = glom
    gnoth.range = (glom.start_position, glom.end_position)
    gnoth.form = None
    if len(members) == 1 and not members[0].is_glint and members[0].structure:
        gnoth.form = list(members[0].structure)     # as a recap does (D-066)
    set_class(world, gnoth, kind)
    world.events.publish(events.GnothFilled(gnoth.name, gnoth.frame, glom.name, gnoth.range,
                                            tuple(x.name for x in members), new))
    return gnoth


def set_class(world, gnoth: gn.Gnoth, kind: str) -> None:
    """Make a gnoth an actualization of a class [M86 p. 77]: its `plato-class` and the
    platoplasm's list (a slip moves every gnoth to the new reigning class [M86 pp. 174-175])."""
    world.platoplasm.forget(gnoth)
    gnoth.plato_class = kind
    world.platoplasm.actualize(kind, gnoth)


def set_gnoths(world, form, frames) -> list:
    """Fill out the gnoths in accordance with the old hypothesis [M86 p. 98] (DECISION
    D-036): the catchall is destroyed [M86 p. 98]; for each frame in order, a gnoth whose
    range begins where the frame begins is reused (kept intact when its range is the
    frame's and its gnoth-glom is present, as run 3's `glom9` survives the second
    Gnoth-setter [M86 pp. 175, 178]; filled afresh otherwise), every other gnoth
    overlapping the frame is destroyed, and a frame with no gnoth gets a new one; gnoths
    beyond the terms are destroyed. "Each gnoth is marked with the frame and equivalence
    type (term or parse, depending on agreement with the hypothesis' parenthesization)"
    [M86 p. 98] (DECISION D-051); the final gnoth is marked with neither. Returns the
    gnoths in order."""
    kind = form.kind
    catchall = gn.catchall(world)
    if catchall is not None:
        gn.destroy_gnoth(world, catchall)
    existing = sorted(gn.gnoths(world), key=lambda g: g.range[0])
    out = []
    for frame in frames:
        chosen = next((g for g in existing if g.range[0] == frame.start), None)
        for other in list(existing):
            if other is not chosen and overlaps(other, frame):
                gn.destroy_gnoth(world, other)
                existing.remove(other)
        if chosen is not None:
            existing.remove(chosen)
        kept = (chosen is not None and chosen.range == (frame.start, frame.end)
                and chosen.glom is not None
                and world.cytoplasm.find(chosen.glom.name) is chosen.glom)
        if kept:
            gnoth = chosen
            set_class(world, gnoth, kind)
        else:
            gnoth = fill_gnoth(world, chosen, frame, kind)
        gnoth.equivalence_type = (None if frame.value is None
                                  else gn.equivalence_type(gnoth, frame.value))
        out.append(gnoth)
    for other in existing:
        gn.destroy_gnoth(world, other)
    return out


# ---- bond assessment and gnoth stability [M86 pp. 99-103] ---------------------------------------

def bonds(world) -> list:
    """Every bond in the cytoplasm, in creation order ("All bonds are assessed"
    [M86 p. 99])."""
    return [x for x in world.cytoplasm if isinstance(x, cy.Bond)]


def assess_bonds(world, kind: str) -> None:
    """"Procedurally, each gnoth is processed in turn. Its internal bonds, those among the
    gloms it covers, are noted, and their strengths in the current environment -- that of
    the reigning class -- are assigned. Its external bonds, those between its elements and
    those of other gnoths, are similarly assessed" [M86 p. 100]: fills every gnoth's
    `internal_bonds` and `external_bonds` with `(bond, value)` pairs, the lists `show`
    prints as `external-bonds: ((bond2 0) (bond3 -5))` and `internal-bonds: ((bond1 10))`
    [M86 p. 175]. A bond with an end in no gnoth is in neither list. Each list is in the
    order the transcripts print: by the gnoth's elements from right to left (the rightmost
    member of the bond inside the gnoth), a bond's creation order within one element, as
    `((bond2 0) (bond5 -5) (bond6 0) (bond3 -5))` (glint2's three bonds, then glint1's),
    `((bond6 0) (bond5 -5) (bond2 0) (bond3 -5))` and `((bond8 10) (bond7 10))` [M86 p. 179]
    (D-055: a printed format)."""
    listed = gn.gnoths(world)
    for gnoth in listed:
        gnoth.internal_bonds = []
        gnoth.external_bonds = []
    entries = []
    for bond in bonds(world):
        a, b = (owner(world, m) for m in bond.members)
        if a is None or b is None:
            continue
        value = world.platoplasm.bond_value(kind, bond)
        if a is b:
            inside = max(m.end_position for m in bond.members)
            entries.append((a, "internal", inside, bond, value))
        else:
            for gnoth, member in zip((a, b), bond.members):
                entries.append((gnoth, "external", member.end_position, bond, value))
    for gnoth, side, inside, bond, value in sorted(entries, key=lambda e: -e[2]):
        (gnoth.internal_bonds if side == "internal" else gnoth.external_bonds).append((bond, value))


def gnoth_stability(gnoth: gn.Gnoth) -> float:
    """"The stability of a gnoth is the difference between the bond forces holding it
    together and those acting to tear it apart. 'Holding' bonds are internal pulls and
    external pushes. 'Tearing' bonds are external pulls and internal pushes" [M86 p. 101];
    an external bond counts half, "so that external bond values are not counted twice,
    once for each gnoth involved" [M86 p. 102]. Figure 1's gnoths: `(+10 +2.5 +2.5) -(0) =
    15`, `(+10 + 2.5 + 2.5) - (5) = 10`, `(0) - (5) = -5` [M86 pp. 102-103]. Reads the
    lists `assess_bonds` filled."""
    holding = tearing = 0.0
    for _, value in gnoth.internal_bonds or ():
        if value > 0:
            holding += value
        else:
            tearing += -value
    for _, value in gnoth.external_bonds or ():
        if value > 0:
            tearing += value / 2
        else:
            holding += -value / 2
    return holding - tearing


def staying_pressure(world, kind: str) -> float:
    """"The pressure to stay with the reigning class is the sum of what is termed
    'gnoth-stabilities'" [M86 p. 101], the bonds assessed in that class's environment:
    "a total system stability, in this case, of 15 + 10 - 5 = +20" [M86 p. 103]. It first
    re-assesses every bond in that class's environment, so each gnoth's `internal_bonds`
    and `external_bonds` are rewritten as a side effect (what `show` prints, [M86 p. 175])."""
    assess_bonds(world, kind)
    return sum(gnoth_stability(g) for g in gn.gnoths(world))


def recent_tries(world, kind: str) -> int:
    """"The old hypotheses are checked to determine the number of recent hypotheses of
    this class -- how many 'tries' the class has had since it seized power" [M86 p. 99]:
    the freeze-dried hypotheses of the class at the end of the list, not counting the one
    just refuted (DECISION D-041: the transcript's `stayval: 15.0` is the raw sum on the
    class's first failure [M86 p. 176])."""
    count = 0
    for old in reversed(world.freeze_dried):
        if old.form.kind != kind:
            break
        count += 1
    return max(count - 1, 0)


def slipping_pressure(world, frm: str, to: str) -> float:
    """"The slipping pressure from the reigning class to a neighbor is evaluated by: 1)
    adding two quantities -- the sum of all pulling-bond strengths, and the absolute value
    of the sum of all pushing-bond strengths -- taken over all existing bonds, and assessed
    in the environment of the neighboring class, and then 2) multiplying the sum by the
    slipperiness of the link between the monarch and the neighbor" [M86 p. 104]: from
    S-group to Y-group, `0.4 [the s-link slipperiness] * (10 + 10) = 8` [M86 p. 104]."""
    values = [world.platoplasm.bond_value(to, bond) for bond in bonds(world)]
    pulls = sum(v for v in values if v > 0)
    pushes = sum(v for v in values if v < 0)
    return (pulls + abs(pushes)) * world.platoplasm.s_link(frm, to)


@dataclass(frozen=True)
class SlipCheck:
    """The outcome of one slip-or-stay check [M86 pp. 103-105]: the `reigning` class, the
    staying pressure `stayval` (after DECISION D-041's factor), the `best` neighbour and
    its `pressure` (None and 0 when no neighbour scores, printed `best: nil` [M86 p. 168];
    DECISION D-042), every neighbour's pressure in `pressures`, and `slipped`, whether the
    best beats the staying pressure."""

    reigning: str
    stayval: float
    best: str | None
    pressure: float
    pressures: tuple
    slipped: bool


def slip_check(world, factor: float = 1.0) -> SlipCheck:
    """Stay or slip, for a reigning class that is not strict [M86 pp. 99-105]: the staying
    pressure (times `Config.stay_factor_per_try` per recent try, DECISION D-041, and times
    `factor`, the Slip-scout's, DECISION D-049), "Slipping-pressure estimates are calculated
    for each class that is an s-link neighbor of the reigning class. If the largest of
    these values is greater than the 'staying pressure' -- the current stability -- then a
    slip to the corresponding class will be made" [M86 p. 104]; ties between neighbours go
    to the more slippery link, then to the first listed (DECISION D-042). The transcript's
    check: `stayval: 15.0`, `best: (Y-group 4.0)`, a stay [M86 p. 176]. Through
    `staying_pressure` the check rewrites the gnoths' assessed bond lists (a side effect
    the printout relies on: `show` after the check prints them [M86 p. 175])."""
    reigning = world.reigning
    plato = world.platoplasm
    stay = staying_pressure(world, reigning)
    stay *= world.config.stay_factor_per_try ** recent_tries(world, reigning) * factor
    pressures = tuple((to, slipping_pressure(world, reigning, to))
                      for to in plato.neighbours(reigning))
    best, pressure = None, 0.0
    for to, p in pressures:
        if best is None or p > pressure or (p == pressure and plato.s_link(reigning, to)
                                            > plato.s_link(reigning, best)):
            best, pressure = to, p
    if pressure <= 0:
        best, pressure = None, 0.0
    return SlipCheck(reigning, stay, best, pressure, pressures, best is not None and pressure > stay)


def change_class(world, new: str, reason: str) -> None:
    """Slip to a new reigning class: every gnoth becomes an actualization of it, the bonds
    are assessed afresh in its environment (`show` prints S-group's values after the slip
    from Countup [M86 pp. 174-175]), the threshold is cleared (DECISION D-046) and the
    change is announced, `Changing to a new reigning class. Countup --> S-group`, the words
    of Meredith's annotation [M86 p. 174] (DECISION D-042: the program printed nothing)."""
    old = world.reigning
    world.reigning = new
    world.threshold = None
    for gnoth in gn.gnoths(world):
        set_class(world, gnoth, new)
    assess_bonds(world, new)
    world.events.publish(events.ClassChanged(old, new, reason))


def slip_or_stay(world, factor: float = 1.0) -> SlipCheck | None:
    """The decision [M86 pp. 99-105]: "if a reigning class -- such as Constant -- is very
    strict in the sense that it is difficult to generalize without moving to a new class
    altogether, slipping is chosen immediately. Otherwise, some deeper investigation is
    made" [M86 p. 99]. A strict class (DECISION D-040) slips to its most slippery neighbour
    with no check printed (`Countup --> S-group` [M86 pp. 173-174]); otherwise the check is
    made, printed (`slip-check: stayval: ...`, `best: ...` [M86 pp. 168, 176]; DECISION
    D-042) and acted on. Returns the check, or None for a strict class."""
    reigning = world.reigning
    plato = world.platoplasm
    if plato.is_strict(reigning):
        new = plato.type(reigning).most_slippery
        if new is not None:
            change_class(world, new, "strict")
        else:
            assess_bonds(world, reigning)
        return None
    check = slip_check(world, factor)
    world.events.publish(events.SlipChecked(check.reigning, check.stayval, check.best,
                                            check.pressure, check.pressures, check.slipped))
    if check.slipped:
        change_class(world, check.best, "slip")
    return check


# ---- the Gnoth-setter task [M86 pp. 98-99] -----------------------------------------------------

def gnoth_setter(world, *args) -> None:
    """The Gnoth-setter task, `doing task Gnoth-setter` [M86 p. 173]: cleans the
    reformulation tasks of any earlier episode (DECISION D-010); freeze-dries the old
    hypothesis with the number of terms it explained [M86 p. 91] and destroys its box
    [M86 p. 99] (DECISION D-035: `world.hypothesis` is None afterwards, and the box tree
    with it); makes its class the reigning class, "the Platonic class at the highest level
    of the (former) hypothesis" [M86 p. 99], and clears the threshold (DECISION D-046);
    "carefully fills out gnoths in accordance with the old hypothesis" [M86 p. 98]
    (`layout`, `set_gnoths`, DECISION D-036), printing `top-down glom glomN` for each
    gnoth-glom made and `gnoths: (gnoth1 gnoth2 gnoth3)` [M86 pp. 173, 176]; then
    "calls for reconsideration to begin" [M86 p. 99]: the slip-or-stay decision
    (`slip_or_stay`, DECISION D-042) and a Reformulator set out. Nothing is done without a
    refuted hypothesis (DECISION D-009)."""
    del args
    hypothesis = world.hypothesis
    if hypothesis is None:
        world.stale("Gnoth-setter", (), "no hypothesis")
        return
    if not hypothesis.refuted:
        world.stale("Gnoth-setter", (), "the hypothesis is not refuted")
        return
    world.clean("reform")
    world.cast_failures = 0      # a new episode (DECISION D-050)
    form = hypothesis.form
    frames = layout(form, world.terms)
    explained = terms_explained(frames)
    world.freeze_dried.append(FreezeDried(form, explained))
    world.hypothesis = None
    world.reigning = form.kind
    world.threshold = None
    world.events.publish(events.HypothesisFreezeDried(hypothesis.show_form(), explained,
                                                      form.kind))
    set = set_gnoths(world, form, frames)
    world.events.publish(events.GnothsSet(tuple(g.name for g in set),
                                          tuple(g.frame for g in set),
                                          tuple(g.range for g in set)))
    slip_or_stay(world)
    world.post("Reformulator")


# The task handlers of this module, by their printed names (SPEC 2.4).
TASKS = {
    "Gnoth-setter": gnoth_setter,
}

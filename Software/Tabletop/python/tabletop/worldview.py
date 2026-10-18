"""The Worldview: rivals and usurping, the subjunctive Rival Worldview and the fight, the
rating with its coherence and parallelism terms, winners rewarded and losers staying in
the Workspace and fading, and the clean-up that removes them (D-030, D-038, D-040 to
D-044).

"The Worldview is a set of noncontradictory mappings of objects and groups of objects onto
one another." [F95 p. 70] It is the correspondences of the Workspace whose `in_worldview`
flag is set, "the sole, current interpretation of the table" [F95 p. 73]; there is no
separate structure. "The proposed new correspondence must compete with any conflicting
correspondences already in the Worldview." [F95 p. 72] "All the conflicting
correspondences are temporarily removed (subjunctively) from the Old Worldview, and the
new correspondence is temporarily inserted (subjunctively) in their place to form the
Rival Worldview." [F95 p. 72] "The probabilistic winner of this competition between the
Rival Worldview and the Old Worldview is then installed as the new Worldview." [F95 p. 72]
A loser "remains nonetheless in the Workspace with the potential to compete again" [F95
p. 72], and "if the value of a correspondence in Tabletop drops below a certain threshold
(for example, because it has lost a number of competitions), it is (probabilistically)
removed from the Workspace and then must be built again" [F95 p. 73].

Every function here works on the World as it stands; the only draws are the fight's one
coin (`compete`) and the clean-up's one coin per candidate (`clean_up`). Reading a rating
draws nothing.
"""

from dataclasses import dataclass, field

from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import events as EV
from tabletop import geometry as G
from tabletop import roles as R
from tabletop import slipnet_def as SD
from tabletop import temperature as TP
from tabletop.groups import GESTALT

# The reasons a Worldview change records (`docs/engine-api.md`, `WorldviewChanged`).
NO_RIVAL, WON, LOST, REMOVED = "no rival", "won", "lost", "removed"


@dataclass(eq=False)
class Rating:
    """The rating of a Worldview, "the sum of the following" [F95 p. 74] (D-042, D-043):
    `strength`, "The total of the strength ratings of all of the correspondences contained
    in it." [F95 p. 74]; `coherence`, the reward for the coherent pairs less the penalty for
    the incoherent ones; `parallelism`, the points for the parallel pairs; `total`, their
    sum. The pairs are kept as (name, name) tuples for the trace and the GUI."""

    names: list = field(default_factory=list)
    strength: float = 0.0
    coherent_pairs: list = field(default_factory=list)
    incoherent_pairs: list = field(default_factory=list)
    parallel_pairs: list = field(default_factory=list)
    coherence: float = 0.0
    parallelism: float = 0.0
    total: float = 0.0

    @property
    def coherent(self) -> bool:
        """Whether the Worldview has no incoherent pair, "a 'coherent' Worldview in which,
        basically, correspondences tend to bunch together logically" [F95 p. 74] (D-042)."""
        return not self.incoherent_pairs


@dataclass(eq=False)
class Competition:
    """One fight, what a cp-compete codelet did (D-040): the `newcomer`, its `rivals` (the
    Worldview correspondences it would replace, D-044), the two ratings, the `chance` the
    rival had, who won, the exponent, and the clock (`docs/engine-api.md`,
    `CompetitionHeld`)."""

    newcomer: C.Correspondence
    rivals: list
    old_rating: float
    rival_rating: float
    chance: float
    rival_wins: bool
    exponent: float
    clock: int = 0


@dataclass(eq=False)
class Change:
    """One change of the Worldview (D-040, D-041): the correspondences `added` and
    `removed`, the `rating` after it, whether it is `coherent`, the `reason` (`no rival`,
    `won`, `lost`, `removed`) and the clock (`docs/engine-api.md`, `WorldviewChanged`);
    the temperature counts these over its window of ten (D-048)."""

    added: list
    removed: list
    rating: float
    coherent: bool
    reason: str
    clock: int = 0


# ---------------------------------------------------------------------------------------------
# Rivals and usurping (D-044).


def holds(world, group_name: str, item: str) -> bool:
    """Whether the item lies inside the named group: it is the group itself, a member, or a
    group inside it (D-044: "the individual objects inside A" [F95 p. 71])."""
    return group_name == item or C.contains(world, group_name, item)


def reaches_outside(world, group_cp, cp) -> bool:
    """Whether a correspondence has one end inside one of a group correspondence's two
    groups and the other end outside the other: when groups A and B are mapped as wholes,
    "the individual objects inside A can map only to objects inside B and not to some
    other object elsewhere on the table" [F95 p. 71] (D-044). The two groups are taken in
    either order, since the ends follow the picking order, not the sides (D-005)."""
    ga, gb = group_cp.henry, group_cp.eliza
    for a, b in ((cp.henry, cp.eliza), (cp.eliza, cp.henry)):
        inside_a = holds(world, ga, a) and a != ga
        inside_b = holds(world, gb, b) and b != gb
        if inside_a and not inside_b and not holds(world, gb, a):
            return True
        if inside_b and not inside_a and not holds(world, ga, b):
            return True
    return False


def is_group_correspondence(world, cp) -> bool:
    """Whether both ends are groups, "if the program maps a group of objects, A, onto
    another group of objects, B, as wholes" [F95 p. 71] (D-044)."""
    return (C.kind_of_end(world, cp.henry) == C.GROUP
            and C.kind_of_end(world, cp.eliza) == C.GROUP)


def conflicts(world, a, b) -> bool:
    """Whether two correspondences may not both be in the Worldview (D-044): "Rival
    correspondences start or finish at the same end point" [F95 p. 101]; and when one maps
    two groups as wholes, the other may not reach from inside one of them to outside the
    other, in either direction."""
    if a is b:
        return False
    if any(a.has_end(item) for item in b.ends):
        return True
    if is_group_correspondence(world, a) and reaches_outside(world, a, b):
        return True
    if is_group_correspondence(world, b) and reaches_outside(world, b, a):
        return True
    return False


def rivals_of(world, cp) -> list:
    """The Worldview correspondences a newcomer conflicts with, in the order built (D-044):
    "they are forbidden to belong to the Worldview at the same time" [F95 p. 105]. Empty
    when the newcomer "is not a rival to any correspondence already in the Worldview"
    [F95 p. 101]. When no group correspondence stands, members of a group map freely, as
    the two grouped glasses of Blockage 3 do [F95 p. 124]."""
    return [other for other in C.worldview_of(world, exclude=cp) if conflicts(world, cp, other)]


# ---------------------------------------------------------------------------------------------
# Coherence and parallelism (D-042, D-043).


def gestalt_groups(world) -> list:
    """The gestalt groups of the Workspace, in the order built (D-042: coherence is read
    on "a bunch of objects that are physically close together" [F95 p. 97])."""
    return [g for g in world.workspace.groups if g.kind == GESTALT]


def together(world, a: str, b: str) -> bool:
    """Whether two items lie in one gestalt group (D-042): both objects "inside one group"
    [F95 p. 101], a group counting as inside the gestalt group that holds it, and as
    inside itself."""
    if a == b:
        return True
    return any(holds(world, g.name, a) and holds(world, g.name, b) for g in gestalt_groups(world))


def pair_status(world, a, b) -> str | None:
    """How two top-level correspondences sit (D-042): `coherent` when, under one alignment
    of their ends, both the first ends and the second ends lie in one gestalt group each
    ("all of the correspondences went from objects in group I to objects in group 2"
    [F95 p. 74]); `incoherent` when only one side does, as when "one correspondence goes
    from an object inside one group to an object inside another group, and the second
    correspondence (between the two cups) starts inside one group and ends on an isolated
    object" [F95 p. 101]; None when they share no group on either side and are no pair.
    Both alignments are tried, since the ends follow the picking order (D-005)."""
    status = None
    for x, y in ((b.henry, b.eliza), (b.eliza, b.henry)):
        first, second = together(world, a.henry, x), together(world, a.eliza, y)
        if first and second:
            return "coherent"
        if first or second:
            status = "incoherent"
    return status


def top_level(cps) -> list:
    """The correspondences at the top level, in order (D-042, D-043: a correspondence
    inside a group correspondence [F95 p. 108] is counted through its parent)."""
    return [c for c in cps if c.parent is None]


def coherence_pairs(world, cps) -> tuple:
    """The coherent and the incoherent pairs among the top-level correspondences of a
    Worldview, as (name, name) tuples in the order built (D-042)."""
    coherent, incoherent = [], []
    tops = top_level(cps)
    for i, a in enumerate(tops):
        for b in tops[i + 1:]:
            status = pair_status(world, a, b)
            if status == "coherent":
                coherent.append((a.name, b.name))
            elif status == "incoherent":
                incoherent.append((a.name, b.name))
    return coherent, incoherent


def parallel_labels(cp) -> list:
    """The labels of a correspondence's slippages that count for parallelism (D-043): every
    label of every slippage but the count identity, which every correspondence between
    two single objects carries ("one" to "one") and which therefore says nothing about
    "the same type of conceptual slippage" [FCCA p. 387]."""
    labels = []
    for s in cp.slippages:
        if s.kind == C.NUMBER and s.identity:
            continue
        for label in s.labels:
            if label not in labels:
                labels.append(label)
    return labels


def parallel(a, b) -> bool:
    """"One correspondence is said to be parallel to another if the slippages that support
    the first have the same labels as the slippages that support the second." [F95 p. 74]
    Two correspondences at the same level sharing a label (D-043); levels do not mix."""
    if a.level != b.level:
        return False
    labels = parallel_labels(a)
    return any(label in labels for label in parallel_labels(b))


def parallel_pairs(cps) -> list:
    """The parallel pairs of a Worldview, as (name, name) tuples in the order built
    (D-043)."""
    found = []
    for i, a in enumerate(cps):
        for b in cps[i + 1:]:
            if parallel(a, b):
                found.append((a.name, b.name))
    return found


def rate(world, cps) -> Rating:
    """The rating of a set of correspondences taken as a Worldview (D-042, D-043): the sum
    of their strengths, recomputed first ("The total of the strength ratings of all of the
    correspondences contained in it." [F95 p. 74]), plus `coherence_reward` per coherent
    pair less `incoherence_penalty` per incoherent pair, plus `parallel_points` per
    parallel pair. The strengths are read as the Worldview flags stand; `compete` sets
    them subjunctively for the Rival Worldview. Draws nothing."""
    cfg = world.config
    for cp in cps:
        C.evaluate(world, cp)
    rating = Rating(names=[cp.name for cp in cps])
    rating.strength = sum(cp.rating for cp in cps)
    rating.coherent_pairs, rating.incoherent_pairs = coherence_pairs(world, cps)
    rating.parallel_pairs = parallel_pairs(cps)
    rating.coherence = (cfg.coherence_reward * len(rating.coherent_pairs)
                        - cfg.incoherence_penalty * len(rating.incoherent_pairs))
    rating.parallelism = cfg.parallel_points * len(rating.parallel_pairs)
    rating.total = rating.strength + rating.coherence + rating.parallelism
    return rating


def current(world) -> Rating:
    """The rating of the Worldview as it stands (D-042; the structure value of D-062)."""
    return rate(world, world.workspace.worldview())


def rate_subjunctively(world, removed, added) -> Rating:
    """The rating of the Rival Worldview: the Worldview less `removed` plus `added`, with
    the flags set "temporarily" and "subjunctively" [F95 p. 72] while the strengths, whose
    dynamic and salience terms read the flags, are recomputed; then the flags are restored
    and the strengths recomputed as they really stand (D-040)."""
    for cp in removed:
        cp.in_worldview = False
    for cp in added:
        cp.in_worldview = True
    try:
        return rate(world, world.workspace.worldview())
    finally:
        for cp in added:
            cp.in_worldview = False
        for cp in removed:
            cp.in_worldview = True
        for cp in list(removed) + list(added):
            C.evaluate(world, cp)


# ---------------------------------------------------------------------------------------------
# The symmetry axis (D-083, role G3).

# The Slipnet node each axis reading jolts and is strengthened by (D-083).
AXIS_NODES = {G.MIRROR: "mirror symmetry", G.POINT: "diagonal symmetry"}


def placement_votes(world, cps) -> dict:
    """The placements the top-level correspondences of a Worldview share, **leaving out
    every correspondence that contains the touched object** (D-083: the guard against
    reading the answer off the geometry): `mirror` and `point` counted from the placement
    slippages (D-082); a middle-column placement votes for neither."""
    votes = {G.MIRROR: 0, G.POINT: 0}
    for cp in top_level(cps):
        if C.contains_touched(world, cp):
            continue
        for s in C.slippages_of_kind(cp, C.PLACEMENT):
            if s.labels == C.IDENTITY_LABELS:
                votes[G.MIRROR] += 1
            elif s.labels == C.FLIP_LABELS:
                votes[G.POINT] += 1
    return votes


def symmetry_axis(world, item: str) -> list:
    """The SYMMETRY_AXIS detector (D-083, role G3), read on the Worldview item alone and
    only at the top context (the axis of the table is not carried into a group's
    subworld, as "Activation of a concept (in this case, 'mirror symmetry') in one
    context may have no significant influence on choices made in a" lower one [F95
    p. 86], D-021): the placement, mirror or point, that more of the Worldview's
    top-level correspondences share, the touched object's own left out
    (`placement_votes`); none when they tie or there are none. The reading is
    (SYMMETRY_AXIS, (kind,), the votes, the evidence). Draws nothing."""
    if item != R.WORLDVIEW_ITEM or world.context is not None:
        return []
    votes = placement_votes(world, world.workspace.worldview())
    mirror, point = votes[G.MIRROR], votes[G.POINT]
    if mirror == point:
        return []
    kind = G.MIRROR if mirror > point else G.POINT
    return [(R.SYMMETRY_AXIS, (kind,), max(mirror, point),
             f"{mirror} mirror against {point} point placements, the touched object's left out")]


def key_of(world, cp) -> tuple | None:
    """The type substitution one correspondence of the Worldview makes between two
    grounds (D-094): for a group-to-group correspondence, its type slippage (D-030) when
    the groups have types (two sameness groups) or, failing that, the ground types of
    the two gestalt groups (`describe.ground_type_of`: Figure/Ground 1's rows, the forks
    as a whole to the glasses as a whole); for a correspondence between two objects that
    both carry a GROUND tag (D-091), their two types, the background's members standing
    for it. The two type nodes sorted, or None when the correspondence makes no such
    substitution (an identity, a figure, a group with no ground)."""
    ws = world.workspace
    a, b = cp.henry, cp.eliza
    if is_group_correspondence(world, cp):
        slips = [s for s in C.slippages_of_kind(cp, C.TYPE) if not s.identity]
        if slips:
            return tuple(sorted((slips[0].a, slips[0].b)))
        ta, tb = (DS.ground_type_of(world, ws.group_named(g)) for g in (a, b))
    elif a in world.problem.ids() and b in world.problem.ids() and ws.has_role(a, R.GROUND) \
            and ws.has_role(b, R.GROUND):
        ta, tb = world.problem.object(a).type, world.problem.object(b).type
    else:
        return None
    if ta is None or tb is None or ta == tb:
        return None
    return tuple(sorted((SD.node_for_type(ta), SD.node_for_type(tb))))


def type_keys(world, item: str) -> list:
    """The TYPE_KEY detector (D-094, role K2), read on the Worldview item alone and only
    at the top context (D-021, as the axis is): each substitution between two different
    types that a correspondence of the Worldview between two grounds has made (`key_of`:
    forks as a whole to glasses as a whole, or a fork of the one background to a glass of
    the other), read as a substitution to apply to other items of the two types, "a kind
    of temporary conceptual symmetry, in which just during this problem, forks and
    glasses are seen as 'opposites'" [F95 p. 140]. The reading is (TYPE_KEY, (the one
    type's node, the other's), the correspondences making it, the evidence); the pair is
    unordered (the ends of a correspondence are not sides). Draws nothing."""
    if item != R.WORLDVIEW_ITEM or world.context is not None:
        return []
    keys: dict = {}
    for cp in world.workspace.worldview():
        pair = key_of(world, cp)
        if pair is not None:
            keys.setdefault(pair, []).append(cp.name)
    return [(R.TYPE_KEY, pair, len(names), f"{pair[0]} for {pair[1]}, by {' '.join(names)}")
            for pair, names in keys.items()]


def keys_of(world) -> list:
    """The type keys standing on the Worldview item, as (type node, type node) tuples
    (D-094); consumer C1 reads them (D-095), C5 through `correspondences.key_slippages`."""
    return [tuple(t.args) for t in world.workspace.roles_of(R.WORLDVIEW_ITEM) if t.role == R.TYPE_KEY]


def axis_of(world) -> str | None:
    """The axis the Worldview item is tagged with, `mirror` or `point`, or None (D-083);
    consumer C1 reads it to propose images under the axis (D-095), C6 through the node it
    jolts (D-082)."""
    tags = [t for t in world.workspace.roles if t.item == R.WORLDVIEW_ITEM and t.role == R.SYMMETRY_AXIS]
    return tags[-1].args[0] if tags else None


# ---------------------------------------------------------------------------------------------
# The barrier and the claimed counterpart (D-089, roles S5 and S6).


def obvious_counterparts(world) -> list:
    """The touched object's obvious counterparts (D-089): of the candidates in the first
    non-empty natural tier of the re-cast as it reads now (`roles.natural_first_tier`,
    D-093, role K1: read live off the standing ROW tags, the claim left out, so that a
    claimed counterpart stays obvious), those whose type is conceptually nearest the
    touched object's, at the Slipnet's current distances (D-012; ties kept): "the
    natural tendency to touch whatever object on her side of the table has the closest
    conceptual resemblance to the touched object" [F95 p. 114]. Empty while the re-cast
    reads nothing (the rows unread), so that nothing is read off a geometry the run has
    not perceived. Draws nothing."""
    mine = SD.node_for_type(world.problem.object(world.problem.touched).type)
    distances = {c: world.slipnet.distance(mine, SD.node_for_type(world.problem.object(c).type))
                 for c in R.natural_first_tier(world)}
    nearest = min(distances.values(), default=0.0)
    return [c for c, d in distances.items() if d == nearest]


def blockers(world) -> list:
    """The claims on the touched object's obvious counterparts (D-089), at the top context
    only (D-021): one (barrier end, claimed object, correspondence) triple for each
    Worldview correspondence that does not contain the touched object and has an obvious
    counterpart (`obvious_counterparts`) at an end, as the object itself or inside a
    group mapped as a whole (`holds`); the other end is the barrier. French's Blockage:
    "the cup/glass and the glass/glass correspondences are competing with one another"
    [F95 p. 33]. In the order the correspondences were built; empty off the top context
    or while no re-cast stands. Draws nothing."""
    found = []
    if world.context is not None:
        return found
    candidates = obvious_counterparts(world)
    for cp in world.workspace.worldview():
        if C.contains_touched(world, cp):
            continue
        for end, other in ((cp.henry, cp.eliza), (cp.eliza, cp.henry)):
            for candidate in candidates:
                if end == candidate or (C.kind_of_end(world, end) == C.GROUP
                                        and holds(world, end, candidate)):
                    found.append((other, candidate, cp))
    return found


def barrier_and_claimed(world, item: str) -> list:
    """The BARRIER and CLAIMED detector (D-089, roles S5 and S6), the relational reading
    off the Worldview and the re-cast (`blockers`), asked on an object or a group: BARRIER
    on the end of a Worldview correspondence, not the touched object's own, whose other
    end is or holds an obvious counterpart of the touched object, read as (BARRIER, (the
    claimed object, the correspondence), the end's objects, the evidence); CLAIMED on
    that counterpart, (CLAIMED, (the barrier, the correspondence), 1, the evidence),
    read only while the barrier's tag stands (a barrier that lost its fight with a
    SIDEKICK, D-079, claims nothing): "We therefore choose one correspondence or the
    other." [F95 p. 34] Nothing on the table or the Worldview item, nothing while no
    re-cast or no such correspondence stands: nothing is gated. Draws nothing."""
    out = []
    ws = world.workspace
    touched = world.problem.touched
    for barrier, claimed, cp in blockers(world):
        if item == barrier:
            weight = len(C.members_of(world, barrier))
            out.append((R.BARRIER, (claimed, cp.name), weight,
                        f"{cp.label()} claims {claimed}, a counterpart of {touched}"))
        if item == claimed and ws.role_tag(barrier, R.BARRIER, (claimed, cp.name)) is not None:
            out.append((R.CLAIMED, (barrier, cp.name), 1,
                        f"claimed by {barrier} through {cp.label()}"))
    return out


def blocked_items(world) -> list:
    """The items the relational reading may tag, each once, barriers before the objects
    they claim, in the order of `blockers` (D-089); what read-worldview-roles asks the
    detector about."""
    items = []
    for barrier, claimed, _ in blockers(world):
        for item in (barrier, claimed):
            if item not in items:
                items.append(item)
    return items


# ---------------------------------------------------------------------------------------------
# Entering and leaving (D-030, D-038, D-040).


def entry_jolts(world, cp) -> list:
    """The jolts a correspondence's slippages send when it enters the Worldview (D-030):
    `jolt_label` to each label node of each slippage, as two identity slippages "will cause
    activation to be sent to the 'mirror symmetry' concept node in the Slipnet" [F95
    p. 85]. Returns (node, amount) pairs in slippage order."""
    sent = []
    for s in cp.slippages:
        for node in s.labels:
            world.slipnet.jolt(node, world.config.jolt_label)
            sent.append((node, world.config.jolt_label))
    return sent


def record_change(world, added, removed, reason: str) -> Change:
    """Log a change of the Worldview with its rating after the change (D-040, D-048)."""
    rating = current(world)
    change = Change(added=[c.name for c in added], removed=[c.name for c in removed],
                    rating=rating.total, coherent=rating.coherent, reason=reason,
                    clock=world.clock)
    world.workspace.changes.append(change)
    world.workspace.events.record(
        EV.WorldviewChanged, added=tuple(change.added), removed=tuple(change.removed),
        rating=change.rating, coherent=change.coherent, reason=reason)
    return change


def admit(world, cp, reason: str = NO_RIVAL) -> Change:
    """Put a correspondence into the Worldview (D-040): the flag, the entry jolts of its
    slippages (D-030) and the change logged. Used when it "is put in the Worldview without
    a fight" [F95 p. 101] and when its Rival Worldview wins."""
    cp.in_worldview = True
    entry_jolts(world, cp)
    return record_change(world, [cp], [], reason)


def expel(world, cps, reason: str = LOST) -> None:
    """Take correspondences out of the Worldview, where they stay in the Workspace: "a
    loser is merely demoted, in the sense of being removed from the special 'elite' area
    inside the Workspace known as the Worldview" [FCCA p. 390] (D-040, D-041)."""
    for cp in cps:
        cp.in_worldview = False


def alive(world, cp) -> bool:
    """Whether the correspondence is still in the Workspace; a codelet holding one that is
    not fizzles, "have no action at all" [F95 p. 73] (D-041)."""
    return any(c is cp for c in world.workspace.correspondences)


def rival_chance(old_rating: float, rival_rating: float, exponent: float) -> float:
    """The probability that the Rival Worldview wins: R^a / (R^a + O^a) for the two ratings
    floored at 1 and a = a(T) (D-040, the two-outcome law of D-060): "the greater the
    discrepancy between their strengths, the more likely the stronger one is to win the
    fight" [FCCA p. 382]."""
    old, rival = max(1.0, old_rating), max(1.0, rival_rating)
    return TP.pass_probability(rival / (rival + old), exponent)


def compete(world, newcomer, exponent: float) -> "Competition | None":
    """What a cp-compete codelet does (D-040, D-044): the newcomer's rivals are found; with
    none it enters "without a fight" [F95 p. 101]; otherwise the Old Worldview and the
    Rival Worldview (the Old less the rivals plus the newcomer) are rated, one coin at
    `rival_chance` decides, "the rating of each of the correspondences in that Worldview is
    improved and the rating of the losing correspondence is lowered" [F95 p. 72] (D-038;
    once per rivalry, the newcomer with its rivals, D-117),
    the winner's correspondences stand in the Worldview and the losers stay in the
    Workspace (D-041; under `losers_destroyed` they are removed at once). Returns the
    Competition (with `rivals` empty and `chance` 1 for an uncontested entry), or None
    when the codelet fizzles: the newcomer is gone from the Workspace or already in the
    Worldview."""
    ws = world.workspace
    if not alive(world, newcomer) or newcomer.in_worldview:
        return None
    rivals = rivals_of(world, newcomer)
    old = current(world)
    if not rivals:
        admit(world, newcomer, NO_RIVAL)
        new = current(world)
        held = Competition(newcomer, [], old.total, new.total, 1.0, True, exponent, world.clock)
        _hold(world, held)
        return held
    rival = rate_subjunctively(world, rivals, [newcomer])
    chance = rival_chance(old.total, rival.total, exponent)
    rival_wins = TP.passes(chance, 1.0, world.rng)   # the chance is softened already: one coin
    held = Competition(newcomer, list(rivals), old.total, rival.total, chance, rival_wins,
                       exponent, world.clock)
    _hold(world, held)
    rivalry = [newcomer.name] + [r.name for r in rivals]    # the awards are once per rivalry (D-117)
    if rival_wins:
        expel(world, rivals, LOST)
        newcomer.in_worldview = True
        entry_jolts(world, newcomer)
        for cp in ws.worldview():
            C.award(world, cp, True, rivalry)
        for cp in rivals:
            C.award(world, cp, False, rivalry)
        record_change(world, [newcomer], rivals, WON)
        losers = rivals
    else:
        for cp in ws.worldview():
            C.award(world, cp, True, rivalry)
        C.award(world, newcomer, False, rivalry)
        losers = [newcomer]
    if world.config.losers_destroyed:
        for cp in losers:
            remove(world, cp)
    return held


# ---------------------------------------------------------------------------------------------
# Fading and removal (D-041).


def _hold(world, held: Competition) -> None:
    """Log a competition and publish it (`CompetitionHeld`, D-040, D-065)."""
    world.workspace.competitions.append(held)
    world.workspace.events.record(
        EV.CompetitionHeld, newcomer=held.newcomer.name, rivals=tuple(r.name for r in held.rivals),
        old_rating=held.old_rating, rival_rating=held.rival_rating, chance=held.chance,
        rival_wins=held.rival_wins, a=held.exponent)


def remove(world, cp, chance: float = 0.0) -> list:
    """Take a correspondence out of the Workspace for good, "removed from the Workspace
    and then must be built again" [F95 p. 73] (D-041), with every correspondence built
    inside it [F95 p. 108], whose level it was; one in the Worldview is expelled first and
    the change logged. `chance` is the clean-up's, for the event (0 otherwise). Names are
    never reused. Returns the correspondences removed."""
    ws = world.workspace
    gone = [cp] + [c for c in ws.correspondences if c.parent is cp]
    expelled = [c for c in gone if c.in_worldview]
    expel(world, expelled, REMOVED)
    for c in gone:
        ws.remove_correspondence(c)
    ws.events.record(EV.CorrespondenceRemoved, correspondence=cp.name, rating=cp.rating,
                     chance=chance, inner=tuple(c.name for c in gone[1:]))
    if expelled:
        record_change(world, [], expelled, REMOVED)
    return gone


def remove_all(world, cps) -> list:
    """Take several correspondences out of the Workspace at once (D-041): every one in
    the Worldview is expelled before any is removed, so that none is rated with an end
    that has already gone (the run loop drops every correspondence whose group end has
    left the Workspace in one sweep, D-025, D-026). One `CorrespondenceRemoved` per
    correspondence, with the ones built inside it, then one change logged for all the
    expelled. Returns the correspondences removed."""
    ws = world.workspace
    cps = [cp for cp in cps if alive(world, cp)]
    inner = {cp.name: [c for c in ws.correspondences if c.parent is cp] for cp in cps}
    gone = []
    for cp in cps:
        gone += [c for c in [cp] + inner[cp.name] if not any(c is g for g in gone)]
    expelled = [c for c in gone if c.in_worldview]
    expel(world, expelled, REMOVED)
    for cp in cps:
        for c in [cp] + inner[cp.name]:
            ws.remove_correspondence(c)
        ws.events.record(EV.CorrespondenceRemoved, correspondence=cp.name, rating=cp.rating,
                         chance=0.0, inner=tuple(c.name for c in inner[cp.name]))
    if expelled:
        record_change(world, [], expelled, REMOVED)
    return gone


def removal_chance(world, cp) -> float:
    """The chance a faded correspondence is removed: 1 - rating / `removal_threshold` when
    its rating is below the threshold, 0 at or above it (D-041); not softened by
    temperature. Draws nothing."""
    threshold = world.config.removal_threshold
    rating = C.evaluate(world, cp)
    if rating >= threshold:
        return 0.0
    return 1.0 - rating / threshold


def candidates(world) -> list:
    """The correspondences a clean-up-corresp codelet looks at: those in the Workspace but
    not in the Worldview, in the order built (D-041; a Worldview correspondence is never
    removed this way)."""
    return [c for c in world.workspace.correspondences if not c.in_worldview]


def clean_up(world) -> list:
    """What a clean-up-corresp codelet does (D-041): every correspondence outside the
    Worldview whose rating has dropped below the threshold "is (probabilistically) removed
    from the Workspace" [F95 p. 73], one coin each at `removal_chance`, in Workspace
    order; no coin when the chance is 0 or 1. Returns (correspondence, chance) pairs for
    those removed."""
    removed = []
    for cp in candidates(world):
        if not alive(world, cp):
            continue
        chance = removal_chance(world, cp)
        if chance <= 0.0:
            continue
        if chance >= 1.0 or world.rng.coin(chance):
            remove(world, cp, chance)
            removed.append((cp, chance))
    return removed


# ---------------------------------------------------------------------------------------------
# Re-reading and checking (D-040, D-044).


def challengers(world) -> list:
    """What a check-worldview codelet reads (D-040; the legend's label [F95 p. 96 (img)]):
    every strength recomputed, then the tested correspondences outside the Worldview whose
    rating now exceeds that of every rival they have, or that have none, in the order
    built: the ones worth a cp-compete."""
    found = []
    for cp in world.workspace.correspondences:
        C.evaluate(world, cp)
    for cp in world.workspace.correspondences:
        if cp.in_worldview or not cp.passed:
            continue
        rivals = rivals_of(world, cp)
        if all(cp.rating > C.evaluate(world, r) for r in rivals):
            found.append(cp)
    return found


def violations(world) -> list:
    """The ways the Worldview as it stands breaks its own rule, "no object or group of
    objects is the end object for two distinct correspondences" [F95 p. 71] and the
    inside-only rule of D-044, as sentences; empty when it is "a non-self-contradictory
    subset of the Workspace" [FCCA p. 390]. For tests and a GUI's check; the engine keeps
    it empty."""
    found = []
    cps = world.workspace.worldview()
    for i, a in enumerate(cps):
        for b in cps[i + 1:]:
            if conflicts(world, a, b):
                found.append(f"{a.label()} conflicts with {b.label()}")
    return found


def summary(world) -> list:
    """The Worldview as (name, first end, second end) tuples in the order built, for the
    end of the run (`docs/engine-api.md`, `RunEnded.worldview`; D-062)."""
    return [(c.name, c.henry, c.eliza) for c in world.workspace.worldview()]

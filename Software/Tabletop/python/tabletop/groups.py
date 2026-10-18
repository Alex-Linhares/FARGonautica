"""Groups: the `Group` structure, the proximity criterion and gestalt groups found by a seed
and a flood fill (D-022, D-023); the kinds of bottom-up group, a group's strength, fights
between incompatible groups, and the subgroup rule with its fault fixed (D-024 to D-026,
D-069). The three stages of build-object are in `build_object.py`.

"Because Tabletop builds its representations gradually, it initially represents a newly
discovered group of objects in a 'gestalt' manner." [F95 p. 65] After the first find-group
"the program knows next to nothing about this group, other than that it is a bunch of objects
that are physically close together" [F95 p. 97]; "sheer physical proximity is often a
sufficient reason to form a group" [FCCA p. 390], and "Tabletop often builds proximity groups
out of objects it hasn't yet even looked at individually." [FCCA p. 390] No page gives the
criterion of closeness; D-022 reads it off the Blockage series, where the two glasses are
pulled apart by one, two and three glass widths [F95 p. 128] until "The gap in Blockage 10
is so wide that the program no longer perceives the glasses as forming a group." [F95 p. 129]

Within a gestalt group the program builds bottom-up groups of three kinds: "It will almost
always discover 'sameness groups' (i.e., groups made up of identical objects—the spoon-spoon
group and the fork-fork group, in this case)." [F95 p. 65] "It will usually see groups whose
objects share a common ISA category ('shared-ISA groups')." [F95 p. 65] "And it will only very
rarely build groups whose objects have nothing in common." [F95 p. 65] Groups have strengths
[FCCA p. 382] and fight when a proposed group "would lay claim to items already 'taken' by the
pre-existing group" [FCCA p. 383]. French's program "retains all of the subgroups, which means
that it gets hopelessly confused whenever it is given a problem containing groups made up of
more than three identical objects" [F95 p. 140]; that fault is fixed here (D-026).
"""

from dataclasses import dataclass, field

from tabletop import describe as DS
from tabletop import events as EV
from tabletop import objects as O
from tabletop import slipnet_def as SD
from tabletop import temperature as TP

GESTALT = "gestalt"        # a group found by proximity alone [F95 p. 97] (D-023)
SAMENESS = "sameness"      # "groups made up of identical objects" [F95 p. 65] (D-024)
SHARED_ISA = "shared-isa"  # "objects share a common ISA category" [F95 p. 65] (D-024)
MIXED = "mixed"            # "objects have nothing in common" [F95 p. 65] (D-024)
BOTTOM_UP = (SAMENESS, SHARED_ISA, MIXED)
ABSORBED, LOST = "absorbed", "lost"  # why a group leaves the Workspace (D-025, D-026)


@dataclass(eq=False)
class Group:
    """A group of objects, compared by identity (D-023). `members` are object ids in file
    order; `kind` is `gestalt` here (`sameness`, `shared-isa`, `mixed` for the bottom-up
    groups of D-024); `name` is set by the Workspace; `parent` is the gestalt group a
    subgroup was found in (D-026). What the describing codelets find is kept on the group:
    `neighbours`, member id to its (neighbour id, side) relations (D-027); `visited`, the
    members whose neighbours have been looked for; `positions`, member id to its position
    words in the group frame (D-029); `ends`, `middles`, `orientation` (`horizontal` or
    `vertical`) and `linear` (D-028), all None until found. A bottom-up group also has
    `category`, the shared ISA category of a shared-ISA group (None otherwise), and
    `strength`, the last value computed by `strength_of` (D-025)."""

    members: list
    kind: str = GESTALT
    name: str = ""
    parent: "Group | None" = None
    category: str | None = None
    strength: float = 0.0
    neighbours: dict = field(default_factory=dict)
    visited: list = field(default_factory=list)
    positions: dict = field(default_factory=dict)
    ends: list | None = None
    middles: list | None = None
    orientation: str | None = None
    linear: bool | None = None

    def size(self) -> int:
        """The number of members, "the size of the group" [F95 p. 68]."""
        return len(self.members)

    def neighbours_of(self, id_: str) -> list:
        """The (neighbour id, side) relations recorded for a member, in the order found
        (D-027), a copy; empty before find-neighbor has run on it or on a neighbour."""
        return list(self.neighbours.get(id_, []))

    def chain_known(self) -> bool:
        """Whether every member has had its neighbours looked for, so that the chain of
        p. 80 is complete and the ends can be read off it (D-028)."""
        return all(m in self.visited for m in self.members)

    def contains(self, members) -> bool:
        """Whether every one of these ids is a member (D-025: a group inside another is not
        its rival; D-026: a subgroup)."""
        return all(m in self.members for m in members)

    def is_bottom_up(self) -> bool:
        """Whether the group is a bottom-up group, "a much more solid structure in which each
        of the elements knows about its neighbors" [F95 p. 125] (D-024)."""
        return self.kind in BOTTOM_UP


# ---------------------------------------------------------------------------------------------
# The proximity criterion (D-022).


def link_probability(gap_widths: float, config) -> float:
    """The probability that two objects whose footprints are `gap_widths` small-glass widths
    apart are seen as close together (D-022): 1 up to `proximity_full_gap`, falling linearly
    to 0 at `proximity_zero_gap`, 0 beyond, so that a group forms "less and less often" as
    the glasses are pulled apart [F95 pp. 128-129]."""
    full, zero = config.proximity_full_gap, config.proximity_zero_gap
    if gap_widths <= full:
        return 1.0
    if gap_widths >= zero:
        return 0.0
    return (zero - gap_widths) / (zero - full)


def linked(a, b, rng, config) -> bool:
    """Whether two objects are linked by proximity on this look: one coin through the run's
    generator at `link_probability` of their edge gap (D-022). The coin is perceptual, not a
    decision, so temperature does not soften it (D-060); no draw is spent when the
    probability is 0 or 1."""
    p = link_probability(O.gap_in_glass_widths(O.edge_gap(a, b)), config)
    if p >= 1.0:
        return True
    if p <= 0.0:
        return False
    return rng.coin(p)


def proximity_component(problem, seed_id: str, rng, config) -> list:
    """The objects connected to the seed by proximity links, the "bunch of objects that are
    physically close together" [F95 p. 97] a find-group codelet sees (D-023): a flood fill
    from the seed over all the objects of the problem (the physical situation, not only the
    noticed ones [FCCA p. 390]), each pair tested once, in file order, so that the coins
    are drawn in a fixed order. Returns the member ids in file order; a seed with no link
    gives a component of one."""
    objects = list(problem.objects)
    by_id = {o.id: o for o in objects}
    order = {o.id: i for i, o in enumerate(objects)}
    members = [seed_id]
    tested = []  # pairs (a, b) already tested, a before b in file order
    frontier = [seed_id]
    while frontier:
        current = by_id[frontier.pop(0)]
        for other in objects:
            if other.id in members:
                continue
            pair = tuple(sorted((current.id, other.id), key=order.get))
            if pair in tested:
                continue
            tested.append(pair)
            if linked(current, other, rng, config):
                members.append(other.id)
                frontier.append(other.id)
    return [id_ for id_ in problem.ids() if id_ in members]


def find_group_around(world, seed_id: str, by: str = "find-group",
                      exponent: float = 1.0) -> Group | None:
    """What a find-group codelet does once it has its seed (D-023): flood-fill the
    proximity links from it; a component of one marks the seed isolated and jolts
    "isolated" (D-007, D-022); a component already in the Workspace is nothing new (None);
    otherwise the component becomes a gestalt group, which "(1) sends activation to the
    concept 'group' in the Slipnet" [F95 p. 65] (D-016) and is recorded. A gestalt group
    that overlaps an existing group without containing it or being contained by it fights
    it at once (D-025), at the exponent a(T) the codelet passes (D-060); if it loses it is
    removed again and None is returned. The follow-up codelets of p. 76 are posted by the
    codelet, not here. Returns the new group or None."""
    ws, config = world.workspace, world.config
    component = proximity_component(world.problem, seed_id, world.rng, config)
    if len(component) == 1:
        if ws.mark_isolated(seed_id):
            world.slipnet.jolt("isolated", config.jolt_description)
        return None
    if ws.group_with_members(component, bottom_up=False) is not None:
        return None
    group = Group(members=component, kind=GESTALT)
    ws.add_group(group)
    update_strength(world, group)
    announce(world, group)
    world.slipnet.jolt("group", config.jolt_structure)
    if any(g is group for g, _ in resolve_conflicts(world, group, exponent)):
        return None
    return group


# ---------------------------------------------------------------------------------------------
# The kinds of bottom-up group (D-024).


def grouping_categories(type_: str, config) -> list[str]:
    """The ISA categories of an object type that count for grouping (D-024): those reached by
    an ISA link of intrinsic length at most `shared_isa_max_length`, in table order. The
    bound keeps "a common ISA category" [F95 p. 65] to a type's own superordinates
    (silverware, crockery, liquid holder for the drinking vessels, closed object), so that a
    cup and a spoon, which French lists among the objects that "have nothing in common"
    [F95 p. 65], do not share one through the long spoon-liquid holder link of D-009."""
    node = SD.node_for_type(type_)
    return [link.b for link in SD.LINKS
            if link.kind == SD.ISA and link.a == node and link.length <= config.shared_isa_max_length]


def shared_category(problem, members, config) -> str | None:
    """The category every member shares, the nearest in the first member's table order, or
    None: "groups whose objects share a common ISA category" [F95 p. 65] (D-024)."""
    common = grouping_categories(problem.object(members[0]).type, config)
    for m in members[1:]:
        mine = grouping_categories(problem.object(m).type, config)
        common = [c for c in common if c in mine]
    return common[0] if common else None


def kind_of(problem, members, config) -> tuple[str, str | None]:
    """The kind of a bottom-up group over these members and the Slipnet node it stands
    for (D-024): `sameness` with the node "sameness" when all are of one type ("groups made
    up of identical objects" [F95 p. 65]); `shared-isa` with the shared category when they
    share one; else `mixed` with no node."""
    types = [problem.object(m).type for m in members]
    if all(t == types[0] for t in types):
        return SAMENESS, "sameness"
    category = shared_category(problem, members, config)
    if category is not None:
        return SHARED_ISA, category
    return MIXED, None


def group_type(problem, group) -> str | None:
    """The one object type of a sameness group, None for any other group (D-026)."""
    types = [problem.object(m).type for m in group.members]
    return types[0] if group.kind == SAMENESS and all(t == types[0] for t in types) else None


# ---------------------------------------------------------------------------------------------
# Strength (D-025, D-069).


def adjacent_pairs(problem, members, parent=None) -> list:
    """The adjacent pairs among these members (D-025: the gaps between neighbours): the
    neighbour relations of the parent group restricted to the members, each pair once in
    member order; before the chain is known (or with no parent), consecutive members along
    the run's axis."""
    pairs = []
    if parent is not None:
        for a in members:
            for b, _ in parent.neighbours_of(a):
                if b in members and members.index(a) < members.index(b):
                    pairs.append((a, b))
    if pairs or len(members) < 2:
        return pairs
    axis = DS.axis_of(problem, members)
    ordered = sorted(members, key=lambda m: (DS.coordinate(problem.object(m), axis), members.index(m)))
    return list(zip(ordered, ordered[1:]))


def span_of(problem, members) -> float:
    """The longer side of the members' footprint boxes' bounding box, in units ("its physical
    dimensions" [FCCA p. 382]; D-025)."""
    boxes = [O.box(problem.object(m)) for m in members]
    width = max(b[2] for b in boxes) - min(b[0] for b in boxes)
    height = max(b[3] for b in boxes) - min(b[1] for b in boxes)
    return max(width, height)


def mean_gap_widths(problem, members, parent=None) -> float:
    """The mean edge gap between adjacent members, in glass widths ("distance between
    constituent items" [FCCA p. 383]; D-025); 0 for a single member."""
    pairs = adjacent_pairs(problem, members, parent)
    if not pairs:
        return 0.0
    gaps = [O.gap_in_glass_widths(O.edge_gap(problem.object(a), problem.object(b))) for a, b in pairs]
    return sum(gaps) / len(gaps)


def similar_group_elsewhere(workspace, members, kind: str, exclude=None) -> bool:
    """Whether the Workspace holds another group of the same kind and size sharing no member,
    "the prior existence of similar groups" [FCCA p. 383] (D-025)."""
    for g in workspace.groups:
        if g is exclude or g.kind != kind or g.size() != len(members):
            continue
        if not any(m in g.members for m in members):
            return True
    return False


def strength_of(world, members, kind: str, parent=None, exclude=None, salience_of=None) -> float:
    """A group's strength on 0-100 (D-025), the five factors of FCCA pp. 382-383 with the
    weights of `group_strength_weights`: compactness, w x (1 - span / 55); size,
    w x min(n, 5) / 5; uniformity, w x u with u 1 for a sameness group, 0.5 for a shared-ISA
    group, 0 otherwise; closeness, w x (1 - mean gap / `proximity_zero_gap`); and w if a
    similar group exists elsewhere (`exclude` is the group itself, so that it does not
    count as its own twin). Under `french-faults`, `member_salience_in_group_strength`
    times the members' mean salience is added, the fault of D-069; `salience_of` is the
    salience function the salience module supplies (None: no term). Clipped to [0, 100]."""
    problem, config = world.problem, world.config
    w = dict(config.group_strength_weights)
    uniformity = {SAMENESS: 1.0, SHARED_ISA: 0.5}.get(kind, 0.0)
    total = w["compactness"] * max(0.0, 1.0 - span_of(problem, members) / 55.0)
    total += w["size"] * min(len(members), 5) / 5.0
    total += w["uniformity"] * uniformity
    gap = mean_gap_widths(problem, members, parent)
    total += w["gap"] * max(0.0, 1.0 - gap / config.proximity_zero_gap)
    if similar_group_elsewhere(world.workspace, members, kind, exclude):
        total += w["similar_group"]
    total += member_salience_term(world, members, salience_of)
    return min(100.0, max(0.0, total))


def member_salience_term(world, members, salience_of=None) -> float:
    """`member_salience_in_group_strength` times the members' mean salience, 0 under
    `standard` (D-069, FIXED (ANSWERS 2): "without the presence of the touched object, such
    a group would probably never be made in the first place" [F95 p. 142]); 0 too when no
    salience function is supplied."""
    share = world.config.member_salience_in_group_strength
    if share <= 0.0 or salience_of is None:
        return 0.0
    return share * sum(salience_of(m) for m in members) / len(members)


def update_strength(world, group, salience_of=None) -> float:
    """Recompute a group's strength and keep it on the group (D-025: "constant
    re-evaluation" [FCCA p. 382], so a strength is recomputed when it is read)."""
    group.strength = strength_of(world, group.members, group.kind, group.parent,
                                 exclude=group, salience_of=salience_of)
    return group.strength


# ---------------------------------------------------------------------------------------------
# Incompatible groups and their fights (D-025, D-040).


def conflicts(problem, a, b) -> bool:
    """Whether two groups are incompatible (D-025): they share a member and neither contains
    the other, so that one "would lay claim to items already 'taken' by the pre-existing
    group" [FCCA p. 383]; a subgroup inside a group is not its rival [F95 p. 71 (img)].
    Two sameness groups of one type are never rivals either: French's program kept
    "cup1-cup2, cup2-cup3, cup3-cup4, etc." side by side [F95 p. 140] (D-026), and under
    the fix the later one is proposed as their union and absorbs the earlier."""
    shared = any(m in b.members for m in a.members)
    if not shared or a.contains(b.members) or b.contains(a.members):
        return False
    type_a = group_type(problem, a)
    return type_a is None or type_a != group_type(problem, b)


def rivals_of(world, group) -> list:
    """The Workspace groups incompatible with this one, in the order built (D-025)."""
    return [g for g in world.workspace.groups if g is not group and conflicts(world.problem, group, g)]


def fight(world, challenger_strength: float, holder_strength: float, exponent: float) -> bool:
    """One fight between a proposed group and an established one, "in which the probability
    of victory of either competitor is determined by its current strength relative to its
    rival's" [FCCA p. 383]: the challenger wins with probability c^a / (c^a + h^a), the
    rule of D-040 with strengths for ratings, floored at 1; a = a(T) (D-060)."""
    c, h = max(1.0, challenger_strength), max(1.0, holder_strength)
    return TP.passes(c / (c + h), exponent, world.rng)


def resolve_conflicts(world, group, exponent: float, salience_of=None) -> list:
    """Fight every rival of a group just recorded, in Workspace order (D-025): a rival the
    group beats is removed as `lost`; the first fight the group loses removes the group
    itself (its name is never reused). Returns the groups removed, as (group, reason)
    pairs. Groups are removed only this way or by absorption (D-041)."""
    ws = world.workspace
    removed = []
    for rival in rivals_of(world, group):
        mine = update_strength(world, group, salience_of)
        theirs = update_strength(world, rival, salience_of)
        if fight(world, mine, theirs, exponent):
            ws.remove_group(rival, LOST)
            removed.append((rival, LOST))
        else:
            ws.remove_group(group, LOST)
            removed.append((group, LOST))
            break
    return removed


def announce(world, group) -> None:
    """Publish a group just recorded, with its strength as computed (`GroupBuilt`, D-065)."""
    world.workspace.events.record(
        EV.GroupBuilt, group=group.name, members=tuple(group.members), kind=group.kind,
        category=group.category or "", strength=group.strength,
        parent=group.parent.name if group.parent is not None else "",
        orientation=group.orientation or "")


# ---------------------------------------------------------------------------------------------
# Subgroups (D-026).


def covered_by_sameness_group(world, members, kind: str) -> bool:
    """Whether a proposed sameness run is already inside a sameness group of the same type
    (D-026): under the fix such a run is nothing new, since "we tend to see them simply as
    a group of cups" [F95 p. 140]; under `keep_all_subgroups` (French's program) it is
    proposed like any other, and "the program would end up making many of the possible
    groupings of cups" [F95 p. 140]."""
    if world.config.keep_all_subgroups or kind != SAMENESS:
        return False
    type_ = world.problem.object(members[0]).type
    for g in world.workspace.groups:
        if group_type(world.problem, g) == type_ and g.contains(members):
            return True
    return False


def merged_with_overlapping(world, members, kind: str, parent) -> list:
    """Under the fix of D-026, a proposed sameness run that shares a member with a
    sameness group of its type takes in that group's members (repeatedly, until nothing
    overlaps), so that it is proposed as the whole run seen "simply as a group of cups"
    [F95 p. 140] and absorbs the groups inside it when built; returned in the parent's
    member order. Under `keep_all_subgroups`, or for any other kind, the run is unchanged."""
    if world.config.keep_all_subgroups or kind != SAMENESS:
        return list(members)
    type_ = world.problem.object(members[0]).type
    merged = list(members)
    grown = True
    while grown:
        grown = False
        for g in world.workspace.groups:
            if group_type(world.problem, g) != type_ or g.contains(merged):
                continue
            if any(m in merged for m in g.members):
                added = [m for m in g.members if m not in merged]
                merged += added
                grown = grown or bool(added)
    return [m for m in parent.members if m in merged]


def absorb_subgroups(world, group) -> list:
    """When a sameness group is built, remove every smaller sameness group of the same type
    inside it (D-026, FIXED (ANSWERS 2)): French's program "keeps all of the intermediate
    subgroups around" [F95 p. 140]; humans see six adjacent cups "simply as a group of
    cups" [F95 p. 140]. Under `keep_all_subgroups` nothing is removed. Returns the groups
    absorbed. A correspondence ending on an absorbed group is the Worldview's business
    (D-041)."""
    config, ws = world.config, world.workspace
    type_ = group_type(world.problem, group)
    if config.keep_all_subgroups or type_ is None:
        return []
    absorbed = []
    for g in list(ws.groups):
        if g is group or group_type(world.problem, g) != type_:
            continue
        if g.size() < group.size() and group.contains(g.members):
            ws.remove_group(g, ABSORBED)
            absorbed.append(g)
    return absorbed

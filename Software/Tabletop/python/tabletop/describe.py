"""Describing a group and its members: neighbours and the chain of p. 80, ends and middles,
orientation and linearity, counts, and the two frames of position (D-027 to D-029).

"If there is a group consisting of fork1, fork2, knife1, cup1, spoon1, and spoon2, the first
find-neighbor codelet might have as its parameter fork1. It would find fork2 and spawn the
codelet find-neighbor(fork2)." [F95 p. 80] Neighbours are looked for "to the right and left"
of an object [F95 p. 97], and only members of groups have them: "in order for an object to
have a neighbor, it must be in a group" [F95 p. 76]. Find-end codelets "look at the ends of
groups. Whatever category of object such a codelet finds on the end of a group, it sends
activation to that category in the Slipnet and also to the concept 'end.'" [F95 p. 63] The
ends of a row are "on the left- and right-hand ends of the group, respectively, which
explains why 'left' and 'right' (and not 'above' and 'below') are active" [F95 p. 101]; a
group is horizontal when "the objects in it seem to be arranged horizontally, thus
activating the concept 'horizontal.'" [F95 p. 97] The same words name the sides of the table
"where the terms 'left' and 'right' refer to the sides of the table as a whole" [F95 p. 85]:
that is the table frame, the group's positions the group frame (D-029).
"""

from collections import Counter

from tabletop import events as EV
from tabletop import geometry as G
from tabletop import objects as O
from tabletop import roles as R
from tabletop import groups as GR      # a module reference: groups imports describe too
from tabletop.slipnet_def import node_for_type

HORIZONTAL, VERTICAL = "horizontal", "vertical"   # a group's orientation [F95 p. 97]
SIDES = ("left", "right", "above", "below")        # the four sides of an object (D-027)
OPPOSITE_SIDE = {"left": "right", "right": "left", "above": "below", "below": "above"}
COUNTS = ("one", "two", "three", "many")           # the number nodes [F95 p. 62]
END, MIDDLE = "end", "middle"                      # the position descriptors [F95 p. 63]


# ---------------------------------------------------------------------------------------------
# Geometry of a set of members (D-028).


def centroid(problem, members) -> tuple[float, float]:
    """The mean position of these objects, in table units (D-028, D-029: a group's place)."""
    xs = [problem.object(m).x for m in members]
    ys = [problem.object(m).y for m in members]
    return sum(xs) / len(xs), sum(ys) / len(ys)


def spread(problem, members) -> tuple[float, float]:
    """The extent of these objects' centres along x and along y (D-028)."""
    xs = [problem.object(m).x for m in members]
    ys = [problem.object(m).y for m in members]
    return max(xs) - min(xs), max(ys) - min(ys)


def axis_of(problem, members) -> str:
    """A group's axis (D-028): `horizontal` when the members' spread in x is at least their
    spread in y ("the objects in it seem to be arranged horizontally" [F95 p. 97]), else
    `vertical`. A tie reads as horizontal, the orientation of every row on the figures."""
    dx, dy = spread(problem, members)
    return HORIZONTAL if dx >= dy else VERTICAL


def is_linear(problem, members, axis: str) -> bool:
    """Whether the group is a line (D-028): every member's centre lies within half a glass
    width (D-002) of the axis line through the centroid. A group that is not linear is
    "amorphous" [F95 p. 63] and gets no left or right end."""
    cx, cy = centroid(problem, members)
    limit = O.glass_width() / 2
    for m in members:
        obj = problem.object(m)
        off = abs(obj.y - cy) if axis == HORIZONTAL else abs(obj.x - cx)
        if off > limit:
            return False
    return True


def coordinate(obj, axis: str) -> float:
    """An object's coordinate along an axis (D-028)."""
    return obj.x if axis == HORIZONTAL else obj.y


def side_of(a, b) -> str:
    """The side of object `a` on which object `b` lies (D-027): `left` or `right` when the
    displacement is mostly along x ("to the right and left" [F95 p. 97]), `above` or
    `below` when mostly along y; an exact diagonal reads as horizontal."""
    dx, dy = b.x - a.x, b.y - a.y
    if abs(dx) >= abs(dy):
        return "right" if dx > 0 else "left"
    return "below" if dy > 0 else "above"


def _nearest_by_side(problem, group, id_: str) -> dict:
    """Side to (distance, nearest member id) for the other members of the group (D-027);
    a tie goes to the earlier member."""
    obj = problem.object(id_)
    best = {}
    for m in group.members:
        if m == id_:
            continue
        other = problem.object(m)
        side = side_of(obj, other)
        d = G.distance(obj.x, obj.y, other.x, other.y)
        if side not in best or d < best[side][0]:
            best[side] = (d, m)
    return best


def nearest_on_sides(problem, group, id_: str) -> list:
    """The "immediate neighbors" [F95 p. 97] of a member: on each of the four sides, the
    nearest other member of the group, provided the object is in turn that member's nearest
    on the opposite side, as (neighbour id, side) pairs in the order left, right, above,
    below (D-027). The mutual condition keeps the relation symmetric and immediate: a
    member far down the arm of an L is not the neighbour of one in the row, though it may
    be the nearest thing below it. In a linear group only the two sides along its axis are
    occupied, which gives the chain of p. 80."""
    best = _nearest_by_side(problem, group, id_)
    found = []
    for side in SIDES:
        if side not in best:
            continue
        neighbour = best[side][1]
        back = _nearest_by_side(problem, group, neighbour).get(OPPOSITE_SIDE[side])
        if back is not None and back[1] == id_:
            found.append((neighbour, side))
    return found


# ---------------------------------------------------------------------------------------------
# What the describing codelets do (D-027, D-028).


def find_neighbours(world, group, id_: str, by: str = "find-neighbor") -> list:
    """What find-neighbor(x) does (D-027): "takes as a parameter an object in the group just
    found and determines its neighbors" [F95 p. 76]. The nearest member on each side becomes
    a neighbour of x, and x of it on the opposite side; a new relation jolts "neighbor"
    ("If a neighbor is found, activation is sent to the 'neighbor' node in the Slipnet."
    [F95 p. 63]; D-016). The member is marked visited so that the chain's completion can be
    known (D-028). Returns the (neighbour, side) relations that were new; the codelet posts
    find-neighbor on each of them, "and so on" [F95 p. 80]."""
    config = world.config
    new = []
    for neighbour, side in nearest_on_sides(world.problem, group, id_):
        if _record(group, id_, neighbour, side):
            _record(group, neighbour, id_, OPPOSITE_SIDE[side])
            new.append((neighbour, side))
            world.workspace.events.record(EV.NeighborFound, object=id_, neighbor=neighbour,
                                          side=side, group=group.name)
    if id_ not in group.visited:
        group.visited.append(id_)
    if new:
        world.slipnet.jolt("neighbor", config.jolt_structure)
    return new


def _record(group, a: str, b: str, side: str) -> bool:
    """Record that b is a's neighbour on this side; True if the relation is new."""
    relations = group.neighbours.setdefault(a, [])
    if any(n == b for n, _ in relations):
        return False
    relations.append((b, side))
    return True


def ends_of(problem, group, config) -> list:
    """The end objects of a group (D-028). With `ends_from_chain` and the chain of
    neighbours complete, the ends are the members with exactly one neighbour, so an
    L-shaped group has an end on each arm; otherwise (before the chain is known, and always
    under `french-faults`) they are the two extremes along the group's axis, one at each
    extreme, which on an L is the fault French names: "the program will choose only one as
    the upper end object" [F95 p. 63] (FIXED, ANSWERS 2). Among members tied at an extreme the
    first in member order is the end. Returned in member order."""
    if config.ends_from_chain and group.chain_known():
        return [m for m in group.members if len(group.neighbours_of(m)) == 1]
    axis = axis_of(problem, group.members)
    along = {m: coordinate(problem.object(m), axis) for m in group.members}
    lowest, highest = min(along.values()), max(along.values())
    extremes = [m for m in group.members if along[m] in (lowest, highest)]
    low_end = next(m for m in extremes if along[m] == lowest)
    high_end = next(m for m in extremes if along[m] == highest)
    return [m for m in group.members if m in (low_end, high_end)]


def find_ends(world, group, by: str = "find-end-objs") -> list:
    """What find-end-objs does (D-028): finds the ends, describes each as `end`, and, when
    the group is a line with two ends, the lower one along the axis as `left` (`above`) and
    the higher as `right` (`below`) [F95 p. 101]; each new descriptor jolts its node and the
    end's type node, "it sends activation to that category in the Slipnet and also to the
    concept 'end'" [F95 p. 63] (D-016). The ends are kept on the group. Returns them."""
    problem, config = world.problem, world.config
    ends = ends_of(problem, group, config)
    group.ends = ends
    for end in ends:
        _describe_in_group(world, group, end, END, by)
    axis = axis_of(problem, group.members)
    if len(ends) == 2 and is_linear(problem, group.members, axis):
        lower, higher = sorted(ends, key=lambda m: coordinate(problem.object(m), axis))
        first, second = ("left", "right") if axis == HORIZONTAL else ("above", "below")
        _describe_in_group(world, group, lower, first, by)
        _describe_in_group(world, group, higher, second, by)
    return ends


def find_middles(world, group, by: str = "find-middle-objs") -> list:
    """What find-middle-objs does (D-028): "will find objects in the middle of the group"
    [F95 p. 76], the members that are not ends (the ends as found, or as `ends_of` reads
    them now). Each new `middle` jolts "middle" and the member's type (D-016). Returns the
    middles, kept on the group."""
    ends = group.ends if group.ends is not None else ends_of(world.problem, group, world.config)
    middles = [m for m in group.members if m not in ends]
    group.middles = middles
    for m in middles:
        _describe_in_group(world, group, m, MIDDLE, by)
    return middles


def _describe_in_group(world, group, id_: str, word: str, by: str) -> bool:
    """Attach a within-group position word to a member, in the group frame (on the group,
    D-029) and on the object (for salience, D-031); a new word jolts its node and the
    member's type node (D-016). True if new."""
    words = group.positions.setdefault(id_, [])
    if word in words:
        return False
    words.append(word)
    world.workspace.describe(id_, word)
    world.slipnet.jolt(word, world.config.jolt_description)
    world.slipnet.jolt(node_for_type(world.problem.object(id_).type), world.config.jolt_description)
    return True


def notice_orientation(world, group, by: str = "find-group") -> str:
    """Notice how a group lies (D-028): its axis, `horizontal` or `vertical`, becomes a
    descriptor of the group and jolts that node, "thus activating the concept 'horizontal'"
    [F95 p. 97] (D-016); whether it is linear is recorded with it. Returns the orientation."""
    problem = world.problem
    group.orientation = axis_of(problem, group.members)
    group.linear = is_linear(problem, group.members, group.orientation)
    if world.workspace.describe(group.name, group.orientation):
        world.slipnet.jolt(group.orientation, world.config.jolt_description)
    return group.orientation


def group_frame(group, id_: str) -> list:
    """A member's position words within its group, the group frame of D-029: "the
    descriptions of the positions within their respective groups" [F95 p. 68] (`end`,
    `middle`, `left`, `right`, `above`, `below`), in the order found; a copy."""
    return list(group.positions.get(id_, []))


# ---------------------------------------------------------------------------------------------
# Counts and the table frame (D-029).


def count_descriptor(n: int) -> str:
    """The number descriptor of an item: "one, if it is a simple object; otherwise, the size
    of the group" [F95 p. 68], as the Slipnet's nodes name it: one, two, three, or many."""
    return COUNTS[min(n, len(COUNTS)) - 1]


def describe_count(world, item: str) -> str:
    """Attach an item's count (an object: `one`; a group: its size) and jolt the count node
    on the first time (D-016). Returns the descriptor."""
    n = 1 if item in world.problem.ids() else world.workspace.group_named(item).size()
    word = count_descriptor(n)
    if world.workspace.describe(item, word):
        world.slipnet.jolt(word, world.config.jolt_description)
    return word


def table_frame(world, item: str) -> tuple[str, str]:
    """An item's position in the table frame (D-029): the column (`left`, `middle`,
    `right`) and the row (`above` for Henry's row, `middle`, `below` for Eliza's) of the
    region its centre (a group: its centroid) is drawn in, through the blur of D-004, afresh
    each time it is read, "where the terms 'left' and 'right' refer to the sides of the
    table as a whole" [F95 p. 85]."""
    region = seen_region(world, item)
    column = G.COLUMNS[region.column]
    row = ("above", "middle", "below")[region.row]
    return column, row


def seen_region(world, item: str) -> G.Region:
    """The region an item is seen in when the table frame is read (D-029, D-082): its
    centre (a group: its centroid) drawn through the blur of D-004, afresh each time, so
    that the placement of two items across the midline carries the blur's tolerance and
    no distance constant of its own."""
    if item in world.problem.ids():
        obj = world.problem.object(item)
        x, y = obj.x, obj.y
    else:
        x, y = centroid(world.problem, world.workspace.group_named(item).members)
    return G.drawn_region(x, y, world.rng, world.config)


def midline_perceived(world, item: str) -> list:
    """The MIDLINE detector (D-084, role S1), read on the table item alone: the middle
    band of regions is perceived as a separator between the two people's rows when at
    least one cluster stands on an outer row (a gestalt group, or an isolated object in
    no group seen there: `clusters_on`, D-086, so that a table of loners such as Blockage
    2 has its two sides too) and no member of a gestalt group has been seen in the
    band (the Workspace's last drawn region of each noticed member, D-004) unless that
    member's group lies wholly in the band (`stray_group`, D-086: a stray group is of
    neither side, and only a group reaching into the band from a side joins the two);
    an object in no group may lie there (a STRAY, D-086). Nothing is pre-set, "There is
    no pre-set boundary line dividing this arrangement into two disjoint 'situations'"
    [FCCA p. 388] (D-005): the line is perceived, as people "easily and intuitively break
    it into two situations" [F95 p. 39]. Returns one (role, args, weight, evidence)
    tuple, or nothing. Draws nothing."""
    if item != R.TABLE:
        return []
    ws = world.workspace
    gestalt = [g for g in ws.groups if g.kind == GR.GESTALT]
    clusters = clusters_on(world, HENRY_ROW) + clusters_on(world, ELIZA_ROW)
    if not clusters:
        return []
    in_band = [m for g in gestalt if not stray_group(world, g)
               for m in g.members if ws.is_noticed(m)
               and G.in_midline_band(G.region_named(ws.regions[m]))]
    if in_band:
        return []
    strays = [g for g in gestalt if stray_group(world, g)]
    return [(R.MIDLINE, (), 1, f"middle band holds no grouped object"
                               + (f" but {len(strays)} stray group{'s' if len(strays) != 1 else ''}"
                                  if strays else "")
                               + f"; {len(clusters)} cluster{'s' if len(clusters) != 1 else ''} "
                               f"either side")]


def row_role(world, item: str) -> list:
    """The ROW detector (D-085, role L1): the row of the table frame an item in view is
    seen in (`table_frame`, one blur draw), `above` for Henry's row of regions, `below`
    for Eliza's, "the cup on her side of the table" [F95 p. 28]; read only once a MIDLINE
    is perceived (D-084), since a row is a side of a perceived separator, and never for
    an item seen in the middle band, which is of neither side. Returns one (role, args,
    weight, evidence) tuple with the word in `args`, or nothing."""
    ws = world.workspace
    if item not in ws.items() or not ws.has_role(R.TABLE, R.MIDLINE):
        return []
    column, row = table_frame(world, item)
    if row not in R.ROW_WORDS:
        return []
    return [(R.ROW, (row,), len(members_of(world, item)), f"seen in the {column} {row} region")]


def column_role(world, item: str) -> list:
    """The COLUMN detector (D-085, role L2): the column of the table frame an item in view
    is seen in (`table_frame`, one blur draw), `left`, `middle` or `right`, "where the
    terms 'left' and 'right' refer to the sides of the table as a whole" [F95 p. 85]; the
    word D-029's frame was described for, "the cup in front of Henry" [F95 p. 6]. Read with
    or without a midline, for an item in any band. One (role, args, weight, evidence)
    tuple with the word in `args`."""
    ws = world.workspace
    if item not in ws.items():
        return []
    column, row = table_frame(world, item)
    return [(R.COLUMN, (column,), len(members_of(world, item)),
             f"seen in the {column} {row} region")]


def members_of(world, item: str) -> list:
    """The objects an item stands for, an object itself or a group's members (D-085; the
    weight of a tag is the objects in the item, D-079)."""
    if item in world.problem.ids():
        return [item]
    return list(world.workspace.group_named(item).members)


# ---------------------------------------------------------------------------------------------
# The content roles read on one table: MEMBER, SIDEKICK, ANCHOR, CONNECTOR (D-087, D-088,
# D-090; roles A1, A2, A3 and S4, read by find-roles through `roles.detect`, D-084).


def gestalt_group_of(world, id_: str):
    """The gestalt group an object is a member of, the earliest built, or None (D-090): the
    content roles are read inside the group that holds the object, "in order for an object
    to have a neighbor, it must be in a group" [F95 p. 76]."""
    for g in world.workspace.groups:
        if g.kind == GR.GESTALT and id_ in g.members:
            return g
    return None


def axis_sides(problem, group) -> tuple:
    """The two sides along a group's axis (D-028, D-088): `left` and `right` for a
    horizontal group, `above` and `below` for a vertical one, the lower coordinate first."""
    axis = group.orientation or axis_of(problem, group.members)
    return ("left", "right") if axis == HORIZONTAL else ("above", "below")


def axis_neighbours(problem, group, id_: str) -> tuple:
    """A member's immediate neighbours along the group's axis (D-027, D-088), as (the one
    on the lower side, the one on the higher side), each None while unknown."""
    lower, higher = axis_sides(problem, group)
    found = {side: neighbour for neighbour, side in group.neighbours_of(id_)}
    return found.get(lower), found.get(higher)


def nearest_category(world, id_: str) -> str | None:
    """The nearest ISA category of an object's type in the Slipnet, the first of D-009's
    table order (D-087): "liquid holder" for a cup, "silverware" for a spoon; None for a
    type that ISA nothing."""
    categories = world.slipnet.categories_of(node_for_type(world.problem.object(id_).type))
    return categories[0] if categories else None


def in_touched_family(world, id_: str) -> bool:
    """Whether an object is the touched object or shares its nearest ISA category (D-090):
    the objects whose sidekicks are read, "the touched object itself and the candidate
    answers" [F95 p. 130]."""
    touched = world.problem.touched
    return id_ == touched or nearest_category(world, id_) == nearest_category(world, touched)


def run_length(problem, group, id_: str, side: str) -> int:
    """How many members of the object's own type stand in a row from it on one side, the
    object included (D-090): a run of identical neighbours is one sidekick of weight n,
    "weak sidekicks (i.e., single objects as opposed to small sameness groups) are not
    sufficient" [F95 p. 131]."""
    type_ = problem.object(id_).type
    n, current = 1, id_
    while True:
        following = {s: neighbour for neighbour, s in group.neighbours_of(current)}.get(side)
        if following is None or problem.object(following).type != type_:
            return n
        n, current = n + 1, following


def member_roles(world, item: str) -> list:
    """The MEMBER detector (D-090, role A1): an object in a gestalt group is a member,
    with its group-frame words as the evidence (`end`, `middle`, `left`, `right`; D-029),
    French's factors 3 and 4, "End/middle location slippages" and "Position slippages"
    [F95 p. 68]. Weight 1. Nothing for a group, the table, or an object in no group."""
    if item not in world.problem.ids():
        return []
    group = gestalt_group_of(world, item)
    if group is None:
        return []
    words = group_frame(group, item)
    return [(R.MEMBER, (), 1, f"in {group.name}" + (": " + " ".join(words) if words else ""))]


def sidekicks_of(world, item: str) -> list:
    """The SIDEKICK detector (D-090, role A2), read on the sidekick itself: an object is a
    sidekick of each immediate neighbour x in its gestalt group (D-027) whose type differs
    from its own, when x is the touched object or shares its nearest category (the
    candidates, `in_touched_family`); "these 'sidekick' objects are much less important
    than the touched object itself and the candidate answers" [F95 p. 130]. The tag names
    x in `args`, and its weight is the run of the object's own type away from x: a
    doubled sidekick weighs 2. Nothing for an object in no group or with no neighbour
    known yet."""
    problem = world.problem
    if item not in problem.ids():
        return []
    group = gestalt_group_of(world, item)
    if group is None:
        return []
    mine = problem.object(item).type
    found = []
    for x, side in group.neighbours_of(item):
        if problem.object(x).type == mine or not in_touched_family(world, x):
            continue
        n = run_length(problem, group, item, OPPOSITE_SIDE[side])
        found.append((R.SIDEKICK, (x,), n,
                      f"{x}'s {OPPOSITE_SIDE[side]} neighbour; a run of {n} {mine}"))
    return found


def _linear_with_both_neighbours(world, item: str):
    """The gestalt group, the lower and the higher axis neighbour of a member of a linear
    group whose two neighbours along the axis are known (D-087, D-088); None otherwise."""
    problem = world.problem
    if item not in problem.ids():
        return None
    group = gestalt_group_of(world, item)
    if group is None:
        return None
    linear = group.linear if group.linear is not None else is_linear(
        problem, group.members, group.orientation or axis_of(problem, group.members))
    if not linear:
        return None
    lower, higher = axis_neighbours(problem, group, item)
    if lower is None or higher is None:
        return None
    return group, lower, higher


def anchor_of(world, item: str) -> list:
    """The ANCHOR detector (D-087, role A3): a member of a linear gestalt group with a
    neighbour on each side along the axis (D-027) whose nearest ISA category (D-009) no
    other member shares: the cup among silverware, "the saltshaker and the touched cup
    are surrounded by identical groups of objects" [F95 p. 32]. Weight 1."""
    found = _linear_with_both_neighbours(world, item)
    if found is None:
        return []
    group, lower, higher = found
    mine = nearest_category(world, item)
    if mine is None or any(nearest_category(world, m) == mine for m in group.members if m != item):
        return []
    return [(R.ANCHOR, (), 1, f"the one {mine} in {group.name}; {lower} and {higher} either side")]


def connector_of(world, item: str) -> list:
    """The CONNECTOR detector (D-088, role S4): a member of a linear gestalt group whose
    two immediate neighbours along the axis (D-027) are of different types, neither its
    own, and each of them has a neighbour of its own type on its far side: it bridges two
    runs, "the subgroups (two forks, two spoons) are in corresponding positions on either
    side of the touched object and the glass" [F95 p. 31], without needing the bottom-up
    subgroups. Weight 1."""
    found = _linear_with_both_neighbours(world, item)
    if found is None:
        return []
    group, lower, higher = found
    problem = world.problem
    types = {m: problem.object(m).type for m in (item, lower, higher)}
    if len(set(types.values())) < 3:
        return []
    beyond_lower, _ = axis_neighbours(problem, group, lower)
    _, beyond_higher = axis_neighbours(problem, group, higher)
    if beyond_lower is None or beyond_higher is None:
        return []
    if (problem.object(beyond_lower).type != types[lower]
            or problem.object(beyond_higher).type != types[higher]):
        return []
    return [(R.CONNECTOR, (), 1, f"between {lower} ({beyond_lower} beyond) and {higher} "
                                 f"({beyond_higher} beyond)")]


# ---------------------------------------------------------------------------------------------
# The parts of a side and the items of neither: GAP, LONER, STRAY (D-086) and CORNER (D-085;
# roles S2, L4, L5 and L3, read by find-roles through `roles.detect`, D-084, and at an
# object's first sight).

HENRY_ROW, BAND, ELIZA_ROW = 0, 1, 2     # the three row bands of regions (D-004, D-005)


def band_of(world, item: str) -> int | None:
    """The row band an item is seen in (D-086): an object's last drawn region's row
    (D-004), a group's centroid's exact region's row (D-031: reading it draws nothing);
    None for an object not yet noticed. 0 is Henry's row of regions, 1 the middle band,
    2 Eliza's (D-005: bands of regions, not sides)."""
    ws = world.workspace
    if item in world.problem.ids():
        name = ws.regions.get(item)
        return None if name is None else G.row_of(G.region_named(name))
    x, y = centroid(world.problem, ws.group_named(item).members)
    return G.row_of(G.exact_region(x, y))


def cluster_box(problem, members) -> tuple:
    """The box (left, top, right, bottom) around the footprints of these objects (D-002,
    D-086): the extent of a cluster along the row."""
    boxes = [O.box(problem.object(m)) for m in members]
    return (min(b[0] for b in boxes), min(b[1] for b in boxes),
            max(b[2] for b in boxes), max(b[3] for b in boxes))


def clusters_on(world, row: int) -> list:
    """The clusters of one outer row band (D-086): the gestalt groups seen in it and the
    isolated objects (D-022) in no gestalt group seen in it, as (name, box) pairs in the
    order of the boxes' left edges (ties keep the Workspace's order)."""
    ws = world.workspace
    found = []
    for g in ws.groups:
        if g.kind == GR.GESTALT and band_of(world, g.name) == row:
            found.append((g.name, cluster_box(world.problem, g.members)))
    for id_ in ws.isolated:
        if gestalt_group_of(world, id_) is None and band_of(world, id_) == row:
            found.append((id_, cluster_box(world.problem, [id_])))
    return sorted(found, key=lambda pair: pair[1][0])


def gaps_on(world, row: int) -> list:
    """The gaps of one outer row band (D-086, role S2): for each two clusters adjacent
    along the row (`clusters_on`), the white space between their boxes
    (`geometry.gap_between`, D-022) when it is at least `proximity_zero_gap` glass
    widths, the distance at which two objects can no longer be grouped [F95 pp. 128-129]
    (the constant is reused, not added), and no object of the table has its centre
    between the two boxes in that band (the physical situation, as the proximity
    criterion reads it, D-023). Each as (left cluster, right cluster, the gap in glass
    widths). Draws nothing."""
    clusters = clusters_on(world, row)
    found = []
    for (a, box_a), (b, box_b) in zip(clusters, clusters[1:]):
        widths = O.gap_in_glass_widths(G.gap_between(box_a, box_b))
        if widths < world.config.proximity_zero_gap:
            continue
        between = [o.id for o in world.problem.objects
                   if box_a[2] < o.x < box_b[0] and G.exact_region(o.x, o.y).row == row]
        if between:
            continue
        found.append((a, b, widths))
    return found


def gaps_of(world, item: str) -> list:
    """The GAP detector (D-086, role S2), read on the table item alone: each gap of
    either outer row (`gaps_on`) is a separator that parts the side into parts; the two
    glasses of Blockage 2 "are then viewed as part of a single, albeit relatively weak,
    structure, even though they are located at opposite corners of the table" [F95
    p. 123]; the
    tag's `args` are the row word (`above`, Henry's; `below`, Eliza's) and the two
    clusters it parts (a group's name or an isolated object's id). Weight 1; the node
    is `separator`, shared with the MIDLINE (D-080). Nothing while no row has two
    clusters apart."""
    if item != R.TABLE:
        return []
    found = []
    for row, word in ((HENRY_ROW, R.ABOVE), (ELIZA_ROW, R.BELOW)):
        for a, b, widths in gaps_on(world, row):
            found.append((R.GAP, (word, a, b), 1,
                          f"{a} and {b} are {widths:.1f} glass widths apart on the {word} row"))
    return found


def stray_group(world, group) -> bool:
    """Whether a gestalt group is a stray group, of neither side (D-086): its centroid's
    exact region is in the middle band and no noticed member was last seen outside it
    (D-004). The MIDLINE tolerates such a group; a group reaching into the band from a
    side does not leave the two sides apart."""
    ws = world.workspace
    if band_of(world, group.name) != BAND:
        return False
    return all(G.in_midline_band(G.region_named(ws.regions[m]))
               for m in group.members if ws.is_noticed(m))


def stray_role(world, item: str) -> list:
    """The STRAY detector (D-086, role L5): an item of neither side, an object on its own
    (a find-group found no neighbour for it, `Workspace.isolated`, D-022, and it is in no
    gestalt group) whose last drawn region (D-004) is in the middle band, or a gestalt
    group that lies wholly in it (`stray_group`; weight its members; a member is a stray
    through its group, `is_stray`): Surround 10's junk,
    "the junk should not make a significant difference" [F95 p. 121], and "The program
    should be able to ignore--not consider, then reject--the small groups of silverware."
    [F95 p. 36]. The node is `separator`, the band it lies on. Consumer C7 scales a
    stray's region term (D-100); C1 never proposes a pair from one (D-095)."""
    ws = world.workspace
    if item in world.problem.ids():
        if item not in ws.isolated or gestalt_group_of(world, item) is not None:
            return []
        if band_of(world, item) != BAND:
            return []
        return [(R.STRAY, (), 1, f"on its own, seen in {ws.regions[item]}")]
    if item not in ws.items():
        return []
    group = ws.group_named(item)
    if group.kind != GR.GESTALT or not stray_group(world, group):
        return []
    return [(R.STRAY, (), group.size(), f"a group of {group.size()} lying in the middle band")]


def is_stray(world, item: str) -> bool:
    """Whether a STRAY tag stands on an item, or on a gestalt group it is a member of
    (D-086): what C7 scales and C1 leaves out."""
    ws = world.workspace
    if ws.has_role(item, R.STRAY):
        return True
    for group in ws.groups_containing(item):
        if group.kind == GR.GESTALT and ws.has_role(group.name, R.STRAY):
            return True
    return False


def loner_of(world, item: str) -> list:
    """The LONER detector (D-086, role L4): an object a find-group found no neighbour for
    (`Workspace.isolated`, D-022) that is in no gestalt group and is seen in an outer row
    band, set off from the rest of its side by a gap (`gaps_on`: it is one of the two
    clusters a gap parts) or alone on it (no other object of the table has its centre in
    that band): "the cup in the lower right-hand corner is seen as a 'loner,' and so
    there is a good deal of pressure to touch the small glass" [F95 p. 118]. Weight 1;
    the node is `isolated`, "if the 'isolated' node is active" [F95 p. 68]. A member,
    and an object in the middle band (a STRAY, of neither side), is no loner."""
    ws = world.workspace
    if item not in ws.isolated or gestalt_group_of(world, item) is not None:
        return []
    row = band_of(world, item)
    if row not in (HENRY_ROW, ELIZA_ROW):
        return []
    word = R.ABOVE if row == HENRY_ROW else R.BELOW
    others = [o.id for o in world.problem.objects
              if o.id != item and G.exact_region(o.x, o.y).row == row]
    if not others:
        return [(R.LONER, (), 1, f"alone on the {word} row")]
    for a, b, widths in gaps_on(world, row):
        if item in (a, b):
            other = b if item == a else a
            return [(R.LONER, (), 1,
                     f"set off from {other} by a gap of {widths:.1f} glass widths")]
    return []


def corner_role(world, item: str) -> list:
    """The CORNER detector (D-085, role L3): an item seen in one of the four corner
    regions (`geometry.is_corner`): an object on its own (isolated, in no gestalt group,
    as `stray_role` reads it) by its last drawn region (D-004), or a gestalt group by its
    centroid drawn through the blur (`seen_region`, one draw); a member is in a corner
    only through its group, whose frame describes it (D-029). "two correspondences
    between diagonally opposite corners of the table" [F95 p. 43]; "If the touched
    object is in a corner, for example, the most favored region is diagonally opposite
    the touched object" [F95 p. 66]. Weight the item's objects; the node is `corner`
    (D-080). Consumer C1 pairs corner with corner across the midline, the point image
    before the mirror image (D-095)."""
    ws = world.workspace
    if item in world.problem.ids():
        if item not in ws.isolated or gestalt_group_of(world, item) is not None:
            return []
        region = G.region_named(ws.regions[item])
    elif item in ws.items() and ws.group_named(item).kind == GR.GESTALT:
        region = seen_region(world, item)
    else:
        return []
    if not G.is_corner(region):
        return []
    return [(R.CORNER, (), len(members_of(world, item)), f"seen in {region.name}")]


# ---------------------------------------------------------------------------------------------
# The figure and its ground: FIGURE and GROUND (D-091; roles A4 and A5, read by find-roles
# through `roles.detect`, D-084, and at an object's first sight).


def row_objects(problem, row: int) -> list:
    """The objects of the table whose centre's exact region lies on one row band (D-091):
    the physical row, as the proximity criterion reads the table (D-023) and as `loner_of`
    reads a row's company; in file order."""
    return [o.id for o in problem.objects if G.exact_region(o.x, o.y).row == row]


def ground_type_of(world, group) -> str | None:
    """The one type two or more members of a group share, the dull background (D-091):
    None when no type repeats, or when more than one does (two forks and two spoons make
    a composition, not a background: nothing stands out against it)."""
    counts = Counter(world.problem.object(m).type for m in group.members)
    repeated = [t for t, n in counts.items() if n >= 2]
    return repeated[0] if len(repeated) == 1 else None


def figures_on(world, row: int) -> list:
    """The objects of one row band that are the only object of their type there (D-091),
    in file order: "an exceptional object" [F95 p. 140] by type."""
    ids = row_objects(world.problem, row)
    counts = Counter(world.problem.object(i).type for i in ids)
    return [i for i in ids if counts[world.problem.object(i).type] == 1]


def grounds_on(world, row: int) -> list:
    """The grounds of one outer row band (D-091): each gestalt group seen in it (D-086's
    `band_of`) with a ground type (`ground_type_of`) against which a figure of another
    type stands on the row (`figures_on`), as (group, type) pairs in the Workspace's
    order. A background with nothing standing out against it is no ground."""
    found = []
    for g in world.workspace.groups:
        if g.kind != GR.GESTALT or band_of(world, g.name) != row:
            continue
        type_ = ground_type_of(world, g)
        if type_ is None:
            continue
        if any(world.problem.object(f).type != type_ for f in figures_on(world, row)):
            found.append((g, type_))
    return found


def figure_role(world, item: str) -> list:
    """The FIGURE detector (D-091, role A4): an object seen in an outer row band that is
    the only object of its type on that row (`figures_on`), where a ground of another
    type stands (`grounds_on`): "an exceptional object--a 'figure object' standing out
    against a dull background" [F95 p. 140]. A member of a gestalt group may be a figure
    (the fork beside two glasses) and so may an object on its own (the cup of Blockage 6
    against the pair of glasses). Weight 1; the node is `figure` (D-080). Consumers: C1
    (figure with figure across the midline, D-095), C5 (D-098), C7 (D-100)."""
    if item not in world.problem.ids():
        return []
    row = band_of(world, item)
    if row not in (HENRY_ROW, ELIZA_ROW) or item not in figures_on(world, row):
        return []
    mine = world.problem.object(item).type
    grounds = [(g, t) for g, t in grounds_on(world, row) if t != mine]
    if not grounds:
        return []
    group, type_ = grounds[0]
    n = sum(1 for m in group.members if world.problem.object(m).type == type_)
    word = R.ABOVE if row == HENRY_ROW else R.BELOW
    return [(R.FIGURE, (), 1, f"the only {mine} on the {word} row, against the {n} {type_} "
                               f"of {group.name}")]


def ground_parent(world, group):
    """The gestalt group a bottom-up group lies inside, or None (D-091)."""
    for g in world.workspace.groups:
        if g.kind == GR.GESTALT and all(m in g.members for m in group.members):
            return g
    return None


def ground_role(world, item: str) -> list:
    """The GROUND detector (D-091, role A5): on an object, one of the two or more objects
    of the one type a gestalt group seen in an outer row band repeats (`ground_type_of`),
    where a figure of another type stands on that row (`grounds_on`): the "'background
    material'" [F95 p. 139] the figure stands out against; on a bottom-up sameness group
    (D-024) of that type inside such a gestalt group, the ground as a whole, weight its
    members, so that consumer C1 may pair ground with ground group to group (D-095), the
    group correspondence French's account of Figure/Ground 1 rests on. Nothing for the
    table, the Worldview item, a gestalt group or an object in no gestalt group.
    Consumers: C1, C5 (D-098)."""
    ws = world.workspace
    if item in world.problem.ids():
        group, weight = gestalt_group_of(world, item), 1
        if group is None:
            return []
        type_ = world.problem.object(item).type
    elif item in ws.items() and ws.group_named(item).kind == GR.SAMENESS:
        inner = ws.group_named(item)
        group, weight = ground_parent(world, inner), inner.size()
        if group is None:
            return []
        type_ = world.problem.object(inner.members[0]).type
    else:
        return []
    row = band_of(world, group.name)
    if row not in (HENRY_ROW, ELIZA_ROW) or (group, type_) not in grounds_on(world, row):
        return []
    n = sum(1 for m in group.members if world.problem.object(m).type == type_)
    word = R.ABOVE if row == HENRY_ROW else R.BELOW
    what = "one of" if weight == 1 else f"the {weight} of"
    return [(R.GROUND, (), weight, f"{what} the {n} {type_} of {group.name}, the ground of "
                                   f"the {word} row")]


# ---------------------------------------------------------------------------------------------
# CONTAINER, in its two forms (D-087 as revised at item 06; role S3, read by find-roles
# through `roles.detect`, D-084).


def is_linear_group(problem, group) -> bool:
    """Whether a group is a line (D-028): its `linear` once found, else read now along its
    orientation or the axis of its spread (D-087, D-088)."""
    if group.linear is not None:
        return group.linear
    return is_linear(problem, group.members, group.orientation or axis_of(problem, group.members))


def holder_under(world, item: str) -> str | None:
    """The object in view an object stands on (D-087, the holder form of CONTAINER): the
    first in file order whose footprint box (D-002) holds the item's centre and whose
    footprint is the larger; None for the table, a group, or an object on nothing."""
    problem = world.problem
    if item not in problem.ids():
        return None
    obj = problem.object(item)
    mine = O.footprint(obj.type, obj.orientation)
    for other in problem.objects:
        if other.id == item or not world.workspace.is_noticed(other.id):
            continue
        theirs = O.footprint(other.type, other.orientation)
        left, top, right, bottom = O.box(other)
        if theirs[0] * theirs[1] > mine[0] * mine[1] and left <= obj.x <= right and top <= obj.y <= bottom:
            return other.id
    return None


def holder_of(world, item: str) -> list:
    """The holder form of CONTAINER (D-087, role S3), read on the holder: an object whose
    footprint box (D-002) holds the centre of a smaller object in view, a saucer under a
    cup, a plate under a bowl; cups and saucers "are strongly associated because, like
    knives and forks, they are used together" [F95 p. 27]. One tag per object held,
    naming it in `args`, weight 1. Nothing for the table, a group, or an object holding
    nothing."""
    problem = world.problem
    if item not in problem.ids():
        return []
    return [(R.CONTAINER, (other.id,), 1, f"holds {other.id}: its centre inside {item}'s footprint")
            for other in problem.objects
            if other.id != item and world.workspace.is_noticed(other.id)
            and holder_under(world, other.id) == item]


def setting_of(world, item: str) -> list:
    """The setting form of CONTAINER (D-087, role S3), read on a group: a linear gestalt
    group (D-028) with exactly one standing ANCHOR tag among its members (D-087: an anchor
    has members on both sides along the axis), read as one unit laid out around it,
    "various assortments of objects are placed around two of the key objects" [F95
    p. 114]. The tag names the anchor in `args`; its weight is the group's size. Nothing
    for an object, a group that is not a line, or a group with no anchor or two."""
    ws = world.workspace
    if not ws.has_group(item):
        return []
    group = ws.group_named(item)
    if group.kind != GR.GESTALT or not is_linear_group(world.problem, group):
        return []
    anchors = [m for m in group.members if ws.has_role(m, R.ANCHOR)]
    if len(anchors) != 1:
        return []
    return [(R.CONTAINER, (anchors[0],), group.size(),
             f"a setting of {group.size()} laid out around {anchors[0]}")]


def container_of(world, item: str) -> list:
    """The CONTAINER detector (D-087, role S3): the holder form on an object
    (`holder_of`), the setting form on a group (`setting_of`); nothing on the table."""
    return holder_of(world, item) + setting_of(world, item)

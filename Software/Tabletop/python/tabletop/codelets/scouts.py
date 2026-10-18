"""The scouts (D-052's first band): find-group, simple-sameness-detect, find-end-objs,
find-middle-objs, find-sim-nhbr, find-neighbor, pick-salient-obj, pick-salient-subgp and
pick-2nd-salient-obj, each with its follow-ups; and the role scouts (D-081): find-roles,
which reads the single-table roles of the table and of one item, compare-roles, which
reads the comparison roles of one item against the other row, and propose-role-cp,
consumer C1 of the roles (D-095), which proposes the pairs a role relation names;
pick-salient-obj is consumer C2 (D-096), weighting its pick by the re-cast (D-093).

Codelets "scan the table in a biased manner" [FCCA p. 386] and report what they see; "When a
find-group codelet finds a group, in addition to sending activation to the 'group' node in
the Slipnet and updating a short-term record of the number of groups found (a kind of
short-term memory), the program puts the following codelets on the Coderack" [F95 p. 76]: a
find-group, a sameness-detector, find-neighbor codelets, a find-end-object, a
find-middle-object, and pick-salient-object codelets (SPEC.md 5.3). Find-neighbor "would find
fork2 and spawn the codelet find-neighbor(fork2)" [F95 p. 80]. The pick-salient codelets pick
an item "for further processing" with "a bias toward high salience" [F95 p. 65] (D-034). The
follow-up lists of every type but find-group are D-052's.
"""

from tabletop import build_object as BO
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import groups as GR
from tabletop import perceive as PV
from tabletop import roles as R
from tabletop import salience as S
from tabletop import temperature as TP
from tabletop import worldview as WV
from tabletop.codelets import searches as SR
from tabletop import geometry as G
from tabletop.codelets.common import (FOUND, PICKED, PROPOSED, Outcome, argument, child, children, fizzle,
                                      group_of, in_file_order, members, outer_group)
from tabletop.slipnet_def import node_for_type


def find_group(world, codelet, a: float) -> Outcome:
    """find-group (D-023, D-052): with an argument, look around that object; without one,
    choose a region (D-032) and look at it (D-004), the seed being the most salient object
    seen there (D-034). A gestalt group found is recorded with its orientation noticed,
    "thus activating the concept 'horizontal'" [F95 p. 97], and the follow-ups of p. 76
    are posted: one find-group, one simple-sameness-detect, two find-neighbor (the first
    and the last member), one find-end-objs, one find-middle-objs, two pick-salient-obj
    [F95 p. 76; F95 p. 81]. An object with nothing near it is marked isolated (D-022),
    and one find-roles is posted on it (D-081, D-086: a loner is read off `isolated`)."""
    ws = world.workspace
    if codelet.args:
        seed = codelet.args[0]
        if not ws.is_noticed(seed):
            ws.notice(seed, world.rng, by=codelet.type)
    else:
        region = PV.choose_region(world, a)
        seen = PV.look(world, region, by=codelet.type)
        if not seen:
            return fizzle(f"nothing in {region.name}")
        seed = S.choose(world, a, seen)
    isolated_before = len(ws.isolated)
    group = GR.find_group_around(world, seed, by=codelet.type, exponent=a)
    if group is None:
        if len(ws.isolated) > isolated_before:
            return Outcome(FOUND, f"{seed} isolated", [child(world, codelet, "find-roles", (seed,))])
        return fizzle(f"no new group around {seed}")
    DS.notice_orientation(world, group, by=codelet.type)
    posted = children(world, codelet, [
        ("find-group", ()), ("simple-sameness-detect", (group.name,)),
        ("find-neighbor", (group.members[0],)), ("find-neighbor", (group.members[-1],)),
        ("find-end-objs", (group.name,)), ("find-middle-objs", (group.name,)),
        ("pick-salient-obj", (), 2), ("find-roles", (group.name,))])   # D-081: its roles
    return Outcome(FOUND, f"{group.name} {members(world, group.members)} gestalt "
                   f"{group.orientation}", posted)


def twins_of(world, item: str) -> list:
    """The items identical to one, elsewhere: for an object, the noticed objects of its
    type with which a correspondence could be made; for a group, the groups of the same
    composition sharing no member (D-052: simple-sameness-detect's "exact configuration of
    objects" [F95 p. 76])."""
    ws = world.workspace
    if S.is_group(world, item):
        return SR.same_groups(world, ws.group_named(item))
    type_ = world.problem.object(item).type
    return [o for o in ws.noticed if o != item and world.problem.object(o).type == type_
            and C.creatable(world, item, o)[0]]


def simple_sameness_detect(world, codelet, a: float) -> Outcome:
    """simple-sameness-detect (D-052): "A sameness-detector, which looks to see if that
    exact configuration of objects appears elsewhere on the table" [F95 p. 76]; a
    find-same-obj codelet "sends activation to 'sameness'" [F95 p. 63] (D-016). On a find,
    chosen by salience among the twins (D-034), a create-cp is posted for the pair."""
    item = argument(world, codelet, a, world.workspace.items())
    if item is None:
        return fizzle("nothing in view")
    if S.is_group(world, item) and group_of(world, item) is None:
        return fizzle(f"{item} gone")
    twins = twins_of(world, item)
    if not twins:
        return fizzle(f"no twin of {item}")
    twin = S.choose(world, a, twins)
    world.slipnet.jolt("sameness", world.config.jolt_description)
    posted = [child(world, codelet, "create-cp", (item, twin))]
    return Outcome(FOUND, f"{twin} same as {item}", posted)


def find_end_objs(world, codelet, a: float) -> Outcome:
    """find-end-objs (D-028): "A find-end-object codelet, which will find the end objects of
    the group" [F95 p. 76]; the ends are noticed and described (`describe.find_ends`)."""
    name = argument(world, codelet, a, [g.name for g in world.workspace.groups if g.ends is None])
    if name is None:
        return fizzle("no group without ends")
    group = group_of(world, name)
    if group is None:
        return fizzle(f"{name} gone")
    before = list(group.ends or [])
    ends = DS.find_ends(world, group, by=codelet.type)
    for end in ends:
        world.workspace.notice(end, world.rng, by=codelet.type)
    if ends == before:
        return fizzle(f"ends of {group.name} already known")
    return Outcome(FOUND, f"ends of {group.name} {members(world, ends)}")


def find_middle_objs(world, codelet, a: float) -> Outcome:
    """find-middle-objs (D-028): "A find-middle-object codelet, which will find objects in
    the middle of the group" [F95 p. 76]; the middles are noticed and described."""
    name = argument(world, codelet, a, [g.name for g in world.workspace.groups if g.middles is None])
    if name is None:
        return fizzle("no group without middles")
    group = group_of(world, name)
    if group is None:
        return fizzle(f"{name} gone")
    before = list(group.middles or [])
    middles = DS.find_middles(world, group, by=codelet.type)
    for m in middles:
        world.workspace.notice(m, world.rng, by=codelet.type)
    if middles == before:
        return fizzle(f"middles of {group.name} already known")
    return Outcome(FOUND, f"middles of {group.name} {members(world, middles)}")


def propose_pair(world, codelet, group, x: str, y: str) -> list:
    """Propose the pair (x, y) as a bottom-up group inside their gestalt group and post a
    build-obj-0 for it (D-024, D-052); nothing when the pair is not proposable."""
    proposal = BO.propose(world, group, in_file_order(world, [x, y]), by=codelet.type)
    if proposal is None:
        return []
    world.workspace.add_proposal(proposal)
    return [child(world, codelet, "build-obj-0", (proposal.name,))]


def unvisited_members(world) -> list:
    """The noticed objects in a gestalt group whose neighbours have not been looked for, a
    find-neighbor's candidates when it has no argument (D-020, D-027)."""
    out = []
    for g in world.workspace.groups:
        if g.is_bottom_up():
            continue
        out.extend(m for m in g.members if m not in g.visited and m not in out
                   and world.workspace.is_noticed(m))
    return out


def find_neighbor(world, codelet, a: float) -> Outcome:
    """find-neighbor (D-027, D-052): "takes as a parameter an object in the group just found
    and determines its neighbors" [F95 p. 76]; each new neighbour is noticed, gets its own
    find-neighbor ("and so on" [F95 p. 80]), and the pair is proposed as a bottom-up group
    with a build-obj-0 posted (D-024). An object in no group has no neighbour [F95 p. 76].
    A new relation posts one find-roles on the object (D-081, D-090): its sidekick, anchor
    and connector roles are read from its neighbours."""
    ws = world.workspace
    x = argument(world, codelet, a, unvisited_members(world))
    if x is None:
        return fizzle("no member whose neighbours are unknown")
    group = outer_group(world, x)
    if group is None:
        return fizzle(f"{x} is in no group")
    if not ws.is_noticed(x):
        ws.notice(x, world.rng, by=codelet.type)
    new = DS.find_neighbours(world, group, x, by=codelet.type)
    if not new:
        return fizzle(f"no new neighbour of {x}")
    posted = []
    for neighbour, side in new:
        ws.notice(neighbour, world.rng, by=codelet.type)
        posted.append(child(world, codelet, "find-neighbor", (neighbour,)))
        posted.extend(propose_pair(world, codelet, group, x, neighbour))
    posted.append(child(world, codelet, "find-roles", (x,)))      # D-081: the roles of x
    found = ", ".join(f"{n} {side}" for n, side in new)
    return Outcome(FOUND, f"neighbours of {x} in {group.name}: {found}", posted)


def find_sim_nhbr(world, codelet, a: float) -> Outcome:
    """find-sim-nhbr (D-051, inferred from the legend [F95 p. 96 (img)]; D-052): among the
    known neighbours of its argument (the touched object by default), the one whose type is
    conceptually nearest in the Slipnet (D-012); the pair is proposed as a bottom-up group
    with a build-obj-0 posted (D-024)."""
    x = argument(world, codelet, a, list(world.workspace.noticed))
    if x is None:
        return fizzle("nothing in view")
    group = outer_group(world, x)
    if group is None:
        return fizzle(f"{x} is in no group")
    neighbours = [n for n, _ in group.neighbours_of(x)]
    if not neighbours:
        return fizzle(f"no neighbour of {x} known")
    my_node = node_for_type(world.problem.object(x).type)
    distances = [(world.slipnet.distance(my_node, node_for_type(world.problem.object(n).type)), n)
                 for n in neighbours]
    nearest = min(distances, key=lambda pair: pair[0])
    if nearest[0] == float("inf"):
        return fizzle(f"no neighbour of {x} is conceptually near")
    world.workspace.notice(nearest[1], world.rng, by=codelet.type)
    posted = propose_pair(world, codelet, group, x, nearest[1])
    return Outcome(FOUND, f"{nearest[1]} near {x} at distance {nearest[0]:.0f}", posted)


def partners_of(world, item: str) -> list:
    """The items a correspondence could be made with, in the Workspace's order (D-035)."""
    return [y for y in world.workspace.items() if C.creatable(world, item, y)[0]]


def recast_weight(world, item: str) -> float:
    """Consumer C2's reading of the re-cast (D-096): the tier weight (`role_recast_tiers`,
    D-093) of the COUNTERPART tag standing on an object, 0 for an object with none, so
    that a pool with no re-cast leaves pick-salient-obj's choice exactly French's. The
    test-only lesion of C2 patches this function (D-104)."""
    tags = [t for t in world.workspace.roles_of(item) if t.role == R.COUNTERPART]
    if not tags:
        return 0.0
    tier = int(tags[-1].args[1][len("tier"):])
    return float(world.config.role_recast_tiers[tier - 1])


def pick_salient_obj(world, codelet, a: float) -> Outcome:
    """pick-salient-obj (D-034, D-052): "pick other salient objects on the table" [F95
    p. 76]: an object other than the touched one, by salience under a(T) [F95 p. 93], and a
    create-cp posted between it and the touched object, "the current object corresponding
    to the touched object" being what the run is after [F95 p. 105]; a pick a
    correspondence cannot join to the touched object fizzles. Objects only: groups are
    pick-salient-subgp's and the searches', and the pairs inside group correspondences
    examine-cp's [F95 p. 108] (the legend's two pick codelets [F95 p. 96 (img)]; D-052,
    item 24). Consumer C2 (D-096): each object's weight is its salience times one plus
    its re-cast weight (`recast_weight`), so that the counterpart the re-cast names is
    picked more often; with no COUNTERPART tag standing the weights are the saliences and
    the draw is the same."""
    touched = world.problem.touched
    candidates = [o for o in world.workspace.noticed if o != touched]
    if not candidates:
        return fizzle("no other object in view")
    weights = [s * (1.0 + recast_weight(world, o)) for o, s in S.saliences(world, candidates)]
    x = candidates[world.rng.weighted_index(TP.softened_weights(weights, a))]
    for tag in [t for t in world.workspace.roles_of(x) if t.role == R.COUNTERPART]:
        R.consumed(world, tag, R.C2, f"picked {x} for {touched}", codelet.type)
    if not C.creatable(world, x, touched)[0]:
        return fizzle(f"nothing to pair with {x}")
    posted = [child(world, codelet, "create-cp", (x, touched))]
    return Outcome(PICKED, f"{x} and {touched}", posted)


def ungrouped(world, ids) -> list:
    """The objects among these that belong to no group yet (D-052: pick-2nd-salient-obj's
    candidates, the objects not yet described beyond their type)."""
    return [i for i in ids if not world.workspace.groups_containing(i)]


def pick_2nd_salient_obj(world, codelet, a: float) -> Outcome:
    """pick-2nd-salient-obj (D-051, inferred from the legend [F95 p. 96 (img)]; the
    "isolated" node posts it, D-020): pick, by salience, an object in no group among those
    in view, looking at a region first (D-032) when every object in view is grouped; post a
    find-group around it and, when something could correspond to it, a create-cp."""
    ws = world.workspace
    candidates = ungrouped(world, ws.noticed)
    if not candidates:
        region = PV.choose_region(world, a)
        candidates = ungrouped(world, PV.look(world, region, by=codelet.type))
        if not candidates:
            return fizzle(f"no ungrouped object in view or in {region.name}")
    x = S.choose(world, a, candidates)
    posted = [child(world, codelet, "find-group", (x,))]
    partners = partners_of(world, x)
    if partners:
        y = S.choose(world, a, partners)
        posted.append(child(world, codelet, "create-cp", (x, y)))
    return Outcome(PICKED, x, posted)


def pick_salient_subgp(world, codelet, a: float) -> Outcome:
    """pick-salient-subgp (D-051, inferred from the legend [F95 p. 96 (img)]; D-052): a
    bottom-up group by salience (D-034), for which a search-for-sam-gp is posted, "the
    program devotes resources to looking for identical or similar groups in other areas of
    the table" [FCCA p. 386]."""
    groups = [g.name for g in world.workspace.bottom_up_groups()]
    if not groups:
        return fizzle("no subgroup yet")
    g = S.choose(world, a, groups)
    return Outcome(PICKED, g, [child(world, codelet, "search-for-sam-gp", (g,))])


# ---------------------------------------------------------------------------------------------
# The role scouts: find-roles and compare-roles (D-081, D-084, D-099).


def find_roles(world, codelet, a: float) -> Outcome:
    """find-roles (D-081, D-084): read the single-table roles through `roles.perceive`, on
    the table itself (the MIDLINE, D-084, and the gaps, D-086) and on its argument when
    it has one, else on one item in view chosen by salience under a(T) (D-034); each
    reading proposes a tag
    that competes and may be broken (D-079). The gestalt group of an object read as an
    ANCHOR, and the object under an object, are read too, for the setting and the holder
    forms of CONTAINER (`container_items`, D-087). Posts a compare-roles for each item
    read, so that its images are read against the other row (D-099), a propose-role-cp
    when a tag stands (C1, D-095), and a recast-role when the touched object's ROW was
    among the tags read (D-093: the re-cast is due once its place is known). Nothing read
    on anything fizzles, posting nothing (nothing is gated)."""
    ws = world.workspace
    items = [R.TABLE]
    if codelet.args:
        items.append(codelet.args[0])
    elif ws.items():
        items.append(S.choose(world, a, ws.items()))
    tags = []
    for item in items:
        tags.extend(R.perceive(world, item, codelet.type, a))
    for item in container_items(world, items, tags):          # D-087: the setting, the holder
        items.append(item)
        tags.extend(R.perceive(world, item, codelet.type, a))
    if not tags:
        return fizzle("no role read on " + " ".join(items))
    posted = [child(world, codelet, "compare-roles", (item,)) for item in items if item != R.TABLE]
    posted.append(child(world, codelet, "propose-role-cp", ()))
    if any(t.item == world.problem.touched and t.role == R.ROW for t in tags):
        posted.append(child(world, codelet, "recast-role", ()))   # D-081, D-093: its row read
    return Outcome(FOUND, " ".join(t.label() for t in tags), posted)


def container_items(world, items: list, tags: list) -> list:
    """The items find-roles reads after the ones it was given (D-087): the gestalt group
    of each object just read as an ANCHOR (the setting laid out around it) and the holder
    under each object read (`describe.holder_under`), each once and none already read."""
    found = []
    for tag in tags:
        if tag.role == R.ANCHOR:
            group = DS.gestalt_group_of(world, tag.item)
            if group is not None:
                found.append(group.name)
    for item in items:
        holder = DS.holder_under(world, item)
        if holder is not None:
            found.append(holder)
    return [i for n, i in enumerate(found) if i not in items and i not in found[:n]]


def compare_roles(world, codelet, a: float) -> Outcome:
    """compare-roles (D-081, D-099, D-092): read the comparison roles of one item against
    the other row, its mirror and point images (`correspondences.mirror_and_point_images`),
    a group's echoes and an object's twinship with the touched object
    (`correspondences.echoes_of`, `twin_of`), through `roles.perceive`; the item is its argument, else one in view by salience
    under a(T) (D-034). Needs a perceived MIDLINE (D-084), else it fizzles. Posts a
    propose-role-cp when a tag stands (C1, D-095)."""
    ws = world.workspace
    item = argument(world, codelet, a, ws.items())
    if item is None:
        return fizzle("nothing in view")
    if item not in ws.items():
        return fizzle(f"{item} gone")
    if not ws.has_role(R.TABLE, R.MIDLINE):
        return fizzle("no midline perceived")
    tags = R.perceive(world, item, codelet.type, a)
    if not tags:
        return fizzle(f"no comparison role of {item}")
    posted = [child(world, codelet, "propose-role-cp", ())]
    return Outcome(FOUND, " ".join(t.label() for t in tags), posted)


# ---------------------------------------------------------------------------------------------
# Consumer C1: role-guided proposals (D-095).


def row_band(world, item: str) -> int | None:
    """The row band an item lies in as the Workspace has seen it (D-095): its ROW tag's
    row when one stands (D-085: the perceived row, consumer C1 of ROW), else an object's
    last drawn region's row (D-004), a group's centroid's exact region's row (D-031); None
    for an object not yet noticed. 0 is Henry's row of regions, 1 the middle band, 2
    Eliza's (D-005: these are regions, not sides)."""
    ws = world.workspace
    word = R.row_word(world, item)
    if word is not None:
        return 0 if word == R.ABOVE else 2
    if S.is_group(world, item):
        return S.region_of(world, item).row
    name = ws.regions.get(item)
    return None if name is None else G.region_named(name).row


def across(world, a: str, b: str) -> bool:
    """Whether two items lie in the two outer row bands, one in each, across a perceived
    MIDLINE (D-095, D-084): C1 proposes pairs across the table only, "correspondences that
    a priori have nothing to do with (i.e., are nowhere in the vicinity of) the touched
    object" [F95 p. 36]; an item in the middle band is paired with nothing here, and
    nothing is proposed while no midline is perceived."""
    if not world.workspace.has_role(R.TABLE, R.MIDLINE):
        return False
    return {row_band(world, a), row_band(world, b)} == {0, 2}


def pair_weight(world, ta, tb) -> float:
    """A role pair's weight (D-095): the tags' role weights times their strengths (the
    one tag's alone when a tag names its own partner), floored at `role_pair_floor`."""
    weight = ta.weight * ta.strength + (tb.weight * tb.strength if tb is not ta else 0.0)
    return max(world.config.role_pair_floor, weight)


def corner_order(world, a: str, b: str) -> float:
    """How C1 orders two corners across the table (D-085, D-095): D-032's preference of
    the one corner's region relative to the other's, 1 for the point image ("the most
    favored region is diagonally opposite the touched object" [F95 p. 66]) and the
    `opposite` weight for the mirror image, "the next most preferred region" [F95 p. 66];
    no constant of its own."""
    return G.region_weight(S.region_of(world, a), S.region_of(world, b), world.config)


def part_pairs(world, tags) -> list:
    """C1's part pairs (D-086, D-095), from the GAP tags standing on the table: when a gap
    parts each row, the parts of the two sides (a group or a loner each) are paired across
    the midline under the axis (D-083): the left part with the left part under a mirror
    axis, with the right part under a point axis, both ways while no axis stands; group
    to group, loner to loner, as the parts are. Each pair carries the two GAP tags, so
    that their consumption is recorded on them."""
    ws = world.workspace
    axis = WV.axis_of(world)
    pairs = []
    for ta in [t for t in tags if t.args[0] == R.ABOVE]:
        for tb in [t for t in tags if t.args[0] == R.BELOW]:
            left_a, right_a, left_b, right_b = ta.args[1], ta.args[2], tb.args[1], tb.args[2]
            matches = {G.MIRROR: [(left_a, left_b), (right_a, right_b)],
                       G.POINT: [(left_a, right_b), (right_a, left_b)]}
            for x, y in matches.get(axis, matches[G.MIRROR] + matches[G.POINT]):
                if x in ws.items() and y in ws.items() and C.creatable(world, x, y)[0]:
                    pairs.append((ta, tb, x, y, pair_weight(world, ta, tb)))
    return pairs


def key_pairs(world, tags) -> list:
    """C1's key pairs (D-094, D-095), from the TYPE_KEY tags standing on the Worldview
    item: for a key between two types, every object in view of the one type with every
    object in view of the other across the midline, neither already an end of a
    correspondence in the Worldview (`Workspace.in_worldview`: the key is applied to
    what is still unmapped), either way round. Each pair carries the key's tag twice (the
    one tag's weight, `pair_weight`)."""
    ws = world.workspace
    pairs = []
    for tag in tags:
        a, b = tag.args
        of_a = [o for o in ws.noticed if node_for_type(world.problem.object(o).type) == a
                and not ws.in_worldview(o)]
        of_b = [o for o in ws.noticed if node_for_type(world.problem.object(o).type) == b
                and not ws.in_worldview(o)]
        for x in of_a:
            for y in of_b:
                if across(world, x, y) and C.creatable(world, x, y)[0]:
                    pairs.append((tag, tag, x, y, pair_weight(world, tag, tag)))
    return pairs


def like_with_like(world, role: str, a: str, b: str) -> bool:
    """Whether a pair of one role is of one kind (D-087, D-091, D-095): a GROUND is paired
    object with object or group with group, never the ground as a whole with one of the
    other side's, and a CONTAINER holder with holder or setting with setting; every other
    role pairs any two items."""
    return role not in (R.GROUND, R.CONTAINER) or S.is_group(world, a) == S.is_group(world, b)


def role_pairs(world) -> list:
    """The pairs the standing tags name (D-095), each as (tag_a, tag_b, a, b, weight): for
    a pair role read on one table (`roles.SINGLE`: sidekick with sidekick, anchor with
    anchor, connector with connector, loner with loner, corner with corner, ...), every
    two items tagged with it across the table, whatever the tags' `args` name (a
    sidekick's is the object it stands beside, D-090); for a comparison or Worldview
    role, whose tag carries its partner (an image, an echo), the item and its partner;
    for the GAP, the parts it marks off (`part_pairs`, D-086); for a TYPE_KEY, the unmapped
    objects of its two types (`key_pairs`, D-094); a GROUND or CONTAINER pair is object
    with object or group with group (`like_with_like`, D-091, D-087). Only pairs a correspondence may
    be created for (D-035), and never one with a stray at an end (`describe.is_stray`,
    D-086: the junk is left out, not considered then rejected). The weight is the tags'
    role weights times their strengths, floored at `role_pair_floor` (`pair_weight`); a
    corner pair's is scaled by `corner_order`, the point image before the mirror image
    (D-085). Under a SYMMETRY_AXIS (D-083) the image pairs are those of the axis's kind
    alone: the mirror images under a mirror axis, the point images under a point axis,
    both while no axis stands."""
    ws = world.workspace
    pairs = []
    axis = WV.axis_of(world)
    against_the_axis = {G.MIRROR: R.POINT_IMAGE, G.POINT: R.MIRROR_IMAGE}.get(axis)
    for role in [s.name for s in R.REGISTRY if s.pair]:
        if role == against_the_axis:
            continue                    # the images under the axis alone (D-083, D-095)
        tags = [t for t in ws.roles if t.role == role]
        if role == R.GAP:
            pairs.extend(part_pairs(world, tags))
            continue
        if role == R.TYPE_KEY:
            pairs.extend(key_pairs(world, tags))
            continue
        tag_with_tag = R.spec(role).reading == R.SINGLE
        for i, ta in enumerate(tags):
            partners = ([(tb, tb.item) for tb in tags[i + 1:]] if tag_with_tag
                        else [(ta, arg) for arg in ta.args])
            for tb, b in partners:
                if ta.item == b or not across(world, ta.item, b) or not C.creatable(world, ta.item, b)[0]:
                    continue
                if not like_with_like(world, role, ta.item, b):
                    continue
                weight = pair_weight(world, ta, tb)
                if role == R.CORNER:
                    weight *= corner_order(world, ta.item, b)
                pairs.append((ta, tb, ta.item, b, weight))
    return [pair for pair in pairs if not (DS.is_stray(world, pair[2]) or DS.is_stray(world, pair[3]))]


def propose_role_cp(world, codelet, a: float) -> Outcome:
    """propose-role-cp, consumer C1 (D-095; D-081): choose, by role weight under a(T)
    (D-060), one pair that a role relation names across the table, and post a create-cp for
    it into the existing chain (create-cp, build-cp-descrip, cp-compete), which tests,
    fights and fades it like any other (D-039 to D-041); it gets no bonus for its origin.
    The consumption is recorded on the tags (`RoleConsumed`), and one check-roles follows,
    so that a tag that was used is re-examined (the author's answer D1). An empty pool, or
    a pool naming no creatable pair, posts nothing and changes no rating."""
    pairs = role_pairs(world)
    if not pairs:
        return fizzle("no role pair to propose")
    weights = TP.softened_weights([w for *_, w in pairs], a)
    ta, tb, x, y, _ = pairs[world.rng.weighted_index(weights)]
    effect = f"create-cp {x} {y}"
    R.consumed(world, ta, R.C1, effect, codelet.type)
    if tb is not ta:
        R.consumed(world, tb, R.C1, effect, codelet.type)
    for row_tag in [t for item in (x, y) for t in world.workspace.roles_of(item) if t.role == R.ROW]:
        R.consumed(world, row_tag, R.C1, effect, codelet.type)      # D-085: the rows read
    posted = [child(world, codelet, "create-cp", (x, y)), child(world, codelet, "check-roles", ())]
    return Outcome(PROPOSED, f"{x} and {y} as {ta.role.lower()} pair", posted)

"""Salience: how much attention an object or a group draws, and the choices made by it
(D-031 to D-034, D-046).

"In Tabletop, the salience of an object (or correspondence) is a variable that determines
how much attention the program will tend to pay to that object (or correspondence)." [F95
p. 66] French lists seven factors for objects and three more for groups [F95 pp. 66-67]:
"Each of these factors determines a real number, and these numbers are summed to determine
the salience of the object." [F95 p. 67] No weight is given; the weights are D-031's
(`Config.salience_weights`), the group points D-046's, the region order D-032's and the
proximity term D-033's. Salience "is a dynamic quantity that changes as the program
develops its picture of the Tableworld" [F95 p. 67], so it is recomputed every time it is
read, from the Slipnet and the Workspace as they stand, and reading it draws nothing from
the generator. Choices by salience are weighted choices under the one law of temperature
(D-034, D-060): "The selection process approaches determinism as the temperature approaches
0, is completely unbiased (i.e., uniformly distributed random) at 100" [F95 p. 93]. A ninth
term, the role term, is consumer C7 of the roles (D-100): the part an item plays in the
analogy draws attention too, and a stray draws less.
"""


from tabletop import geometry as G
from tabletop import roles as R
from tabletop.describe import END, MIDDLE, centroid, is_stray
from tabletop.groups import SAMENESS, SHARED_ISA
from tabletop.perceive import touched_region
from tabletop.slipnet_def import node_for_type
from tabletop.temperature import softened_weights

GROUP_NODE = "group"   # the node a group's type term reads [F95 p. 62] (D-031)
FLOOR = 1.0            # the least salience, so that every item keeps a chance (D-031)

# The factors in French's order [F95 pp. 66-67], as `terms` names them (D-031), and the
# role term after them (D-100, consumer C7).
FACTORS = ("type", "proximity", "position", "region", "group", "worldview", "competitions",
           "composition", "role")
# The roles that earn salience points (D-100): what the rows are laid out around, what
# bridges them, and the figure against its ground.
SALIENT_ROLES = (R.ANCHOR, R.CONNECTOR, R.FIGURE)


# ---------------------------------------------------------------------------------------------
# The factors, one function each (D-031).


def is_group(world, item: str) -> bool:
    """Whether an item name is a group's (`g1`, ...) rather than an object id (D-031: the
    candidates of a salience choice are the noticed objects and the groups)."""
    return item not in world.problem.ids()


def type_term(world, item: str) -> float:
    """Factor 1, "Activation of the concept corresponding to the type of the object" [F95
    p. 66]: the weight times the activation of the object's type node, or, for a group, of
    the "group" node (D-031)."""
    if is_group(world, item):
        node = GROUP_NODE
    else:
        node = node_for_type(world.problem.object(item).type)
    return weight(world, "type_activation") * world.slipnet.activation_of(node)


def place_of(world, item: str) -> tuple[float, float]:
    """An item's place on the table: an object's centre, a group's centroid (D-031,
    D-033)."""
    if is_group(world, item):
        return centroid(world.problem, world.workspace.group_named(item).members)
    obj = world.problem.object(item)
    return obj.x, obj.y


def proximity_term(world, item: str) -> float:
    """Factor 2, "Proximity to the touched object" [F95 p. 66]: w x max(0, 1 - d / reach),
    d the distance from the item's place to the touched object's centre and reach
    `Config.proximity_reach` (D-033); the touched object itself takes the whole weight."""
    touched = world.problem.touched_object()
    x, y = place_of(world, item)
    d = G.distance(x, y, touched.x, touched.y)
    return weight(world, "proximity") * max(0.0, 1.0 - d / world.config.proximity_reach)


def position_words(world, item: str) -> list:
    """The within-group position words an object carries in any group it belongs to, in
    the order found (the group frame of D-029); empty for a group or an ungrouped object."""
    if is_group(world, item):
        return []
    words = []
    for group in world.workspace.groups_containing(item):
        for word in group.positions.get(item, []):
            if word not in words:
                words.append(word)
    return words


def middle_preferred(world) -> bool:
    """Whether "the concept 'middle' is much more active in the Slipnet than the concept
    'end,'" [F95 p. 66]: at least `Config.middle_over_end_factor` times as active (D-031;
    a group's middles outnumber its ends, so a bare "more active" would flip on the
    describing codelets' own jolts, while the chapter 4 dumps keep "end" far above
    "middle" [F95 pp. 100-107 (img)])."""
    end = world.slipnet.activation_of(END)
    middle = world.slipnet.activation_of(MIDDLE)
    return middle >= world.config.middle_over_end_factor * end


def position_term(world, item: str) -> float:
    """Factor 3, "Extremal position in a group of objects" [F95 p. 66]: the `end` points
    for an object described as the end of a group, "unless, of course, the concept 'middle'
    is much more active in the Slipnet than the concept 'end,'" [F95 p. 66], when the
    `middle` points go to the middles instead (D-031, `middle_preferred`)."""
    words = position_words(world, item)
    if not words:
        return 0.0
    if middle_preferred(world):
        return weight(world, "middle") if MIDDLE in words else 0.0
    return weight(world, "end") if END in words else 0.0


def region_of(world, item: str) -> G.Region:
    """The region an item is taken to be in for the region term: an object's last drawn
    region as the Workspace recorded it (D-004; its exact region if it has not been
    noticed), a group's centroid's exact region (D-031: reading salience draws nothing)."""
    if not is_group(world, item):
        name = world.workspace.regions.get(item)
        if name is not None:
            return G.region_named(name)
    x, y = place_of(world, item)
    return G.exact_region(x, y)


def region_term(world, item: str) -> float:
    """Factor 4, "Preferred regions with respect to the touched object" [F95 p. 66]: the
    weight times the preference of the item's region relative to the touched object's
    region, "the most favored region is diagonally opposite the touched object, and the next
    most preferred region is directly opposite the touched object" [F95 p. 66] (D-032)."""
    preference = G.region_weight(region_of(world, item), touched_region(world), world.config)
    return weight(world, "region") * preference * role_reading(world, item)[1]


def group_term(world, item: str) -> float:
    """Factor 5, "Groups versus simple objects" [F95 p. 66]: "Groups are given salience
    points simply for being groups" [F95 p. 66], `Config.group_salience_points` (D-046:
    weakened from the bias French calls "a bit too strong" [F95 p. 109]); 0 for an
    object."""
    return float(world.config.group_salience_points) if is_group(world, item) else 0.0


def worldview_term(world, item: str) -> float:
    """Factor 6, "Whether or not the object corresponds to another object" [F95 p. 66]: the
    points if the item "is part of a correspondence in the Worldview" [F95 p. 66]; 0
    otherwise, and at the start, when "this factor contributes zero to all calculations of
    salience" [F95 p. 67] (D-031)."""
    return weight(world, "in_worldview") if world.workspace.in_worldview(item) else 0.0


def competition_term(world, item: str) -> float:
    """Factor 7, "How often the object has been part of a correspondence that has won (or
    lost) a competition" [F95 p. 66]: "bonus points are awarded to the two objects making up
    the correspondence. Objects in losing correspondences lose points." [F95 p. 66]; the win
    points per win less the loss points per loss, from the Workspace's record (D-031,
    D-038)."""
    wins, losses = world.workspace.competitions_of(item)
    return weight(world, "win") * wins - weight(world, "loss") * losses


def composition_term(world, item: str) -> float:
    """Factors 8-10, "Composition of the groups of objects" [F95 p. 67]: the sameness
    points for "a sameness group" [F95 p. 67], the shared-ISA points when all the members
    "have the same superordinate object category" [F95 p. 67], and the size points per
    member, "Larger groups are favored over smaller groups." [F95 p. 67] A gestalt group,
    whose kind has not been examined (D-024), takes the size points only; an object 0."""
    if not is_group(world, item):
        return 0.0
    group = world.workspace.group_named(item)
    total = weight(world, "size") * group.size()
    if group.kind == SAMENESS:
        total += weight(world, "sameness_group")
    elif group.kind == SHARED_ISA:
        total += weight(world, "shared_isa_group")
    return total


def role_reading(world, item: str) -> tuple:
    """Consumer C7's reading of the role pool (D-100): the role points, the `role` weight
    of `salience_weights` per unit of weight of each ANCHOR, CONNECTOR or FIGURE tag
    standing on the item (`SALIENT_ROLES`); and the scale on the region term,
    `stray_salience_scale` for a stray (a STRAY tag on the item or on a gestalt group it
    is in, `describe.is_stray`), 1 otherwise. An empty pool reads (0, 1): nothing is
    gated, and the salience is French's. The test-only lesion of C7 patches this
    function (D-104)."""
    tags = [t for t in world.workspace.roles_of(item) if t.role in SALIENT_ROLES]
    points = weight(world, "role") * sum(t.weight for t in tags)
    scale = world.config.stray_salience_scale if is_stray(world, item) else 1.0
    return points, scale


def role_term(world, item: str) -> float:
    """The ninth term, the role term (D-100, consumer C7): the points of the roles the item
    plays (`role_reading`); 0 for an item with none, as for every item in v1."""
    return role_reading(world, item)[0]


def weight(world, name: str) -> float:
    """One of `Config.salience_weights` by name (D-031), from the configuration's own
    table by name (`Config.salience_weights_by_name`, built once; D-112)."""
    return world.config.salience_weights_by_name[name]


# ---------------------------------------------------------------------------------------------
# The sum, and the choices made by it (D-031, D-034).


def terms(world, item: str) -> list:
    """Every factor's contribution, as (name, value) pairs in French's order (D-031) with
    the role term last (D-100), for the trace and the GUI's overlay; `salience` is their
    sum."""
    return [
        ("type", type_term(world, item)),
        ("proximity", proximity_term(world, item)),
        ("position", position_term(world, item)),
        ("region", region_term(world, item)),
        ("group", group_term(world, item)),
        ("worldview", worldview_term(world, item)),
        ("competitions", competition_term(world, item)),
        ("composition", composition_term(world, item)),
        ("role", role_term(world, item)),
    ]


def salience(world, item: str) -> float:
    """The salience of an object or a group: the factors "summed to determine the salience
    of the object" [F95 p. 67], never below `FLOOR` (D-031: so that at a temperature of 100
    the program "is as likely to choose any region as any other" [F95 p. 93] holds of
    items too). Recomputed from the current state every time; draws nothing."""
    return max(FLOOR, sum(value for _, value in terms(world, item)))


def salience_of(world):
    """The salience as a function of an item name, for the group-strength hook of D-069
    (`groups.strength_of(..., salience_of=...)`) and for anything else that takes one."""
    return lambda item: salience(world, item)


def saliences(world, candidates=None) -> list:
    """(item, salience) for every candidate, in the Workspace's order: the noticed objects
    in the order noticed, then the groups in the order built (D-031; `Workspace.items`), or
    the candidates given, in their order."""
    items = world.workspace.items() if candidates is None else list(candidates)
    return [(item, salience(world, item)) for item in items]


def choose(world, exponent: float, candidates=None, exclude=()) -> str | None:
    """A choice by salience (D-034): "picking objects in proportion to their salience"
    [F95 p. 66], with the saliences raised to the temperature exponent a(T) the caller
    passes (D-060), so that at a = 6 the most salient item is almost certain and at a = 0.1
    every item is nearly equally likely [F95 p. 93]. `candidates` defaults to every item
    in view; `exclude` names items left out (a second object must differ from the first).
    Returns None, without a draw, when nothing is left to choose from."""
    pairs = [(item, s) for item, s in saliences(world, candidates) if item not in exclude]
    if not pairs:
        return None
    weights = softened_weights([s for _, s in pairs], exponent)
    return pairs[world.rng.weighted_index(weights)][0]


def choose_object(world, exponent: float, exclude=()) -> str | None:
    """A pick-salient-obj's choice among the noticed objects (D-034)."""
    return choose(world, exponent, list(world.workspace.noticed), exclude)


def choose_group(world, exponent: float, exclude=()) -> str | None:
    """A choice among the groups built, by their salience (D-034: the arguments of
    search-for-same-group and search-for-similar-group)."""
    return choose(world, exponent, [g.name for g in world.workspace.groups], exclude)


def most_salient(world, candidates=None) -> str | None:
    """The item of highest salience, the one a choice at a temperature of 0 would make
    almost surely (D-034); the earliest among equals; None with no candidates."""
    best = None
    for item, s in saliences(world, candidates):
        if best is None or s > best[1]:
            best = (item, s)
    return None if best is None else best[0]


# ---------------------------------------------------------------------------------------------
# The region order relative to the touched object (D-032).


def region_order(world) -> list:
    """The nine regions from the most to the least preferred relative to the touched
    object's region, "an area diagonally opposite the touched object is most preferred, an
    area directly opposite the touched object is next, and so on" [F95 p. 93], with their
    preference weights (D-032); reading order among equals. For a touched object in a
    corner the diagonal corner comes first and the directly opposite region second; in the
    middle column the one region both reflections reach comes first and the two far
    corners next."""
    touched = touched_region(world)
    pairs = [(region, G.region_weight(region, touched, world.config)) for region in G.REGIONS]
    return sorted(pairs, key=lambda pair: -pair[1])

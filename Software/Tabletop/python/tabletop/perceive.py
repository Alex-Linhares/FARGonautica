"""Scanning the table: which regions a scout prefers, and what it sees when it looks at
one (D-004, D-023, D-032).

The first codelets "scan the table in a biased manner, giving probabilistic preference to
certain areas of the table" [FCCA p. 386]: "the most favored region is diagonally opposite the
touched object, and the next most preferred region is directly opposite the touched object"
[F95 p. 66], and group-finders "will preferentially (but not deterministically) look in the
corner of the table diagonally opposite the touched object" [FH91 p. 178]. Looking at a
region draws every object's region afresh through the blur of D-004, so that an object near
a boundary "may sometimes be considered by the program to be in the adjacent region" [F95
p. 65]; the objects seen there enter the Workspace, since the scouts report "what objects
are in interesting areas" [FCCA p. 386]. The weighted choice among regions is softened by
temperature: "If the temperature should ever happen to reach 100, the program is as likely
to choose any region as any other." [F95 p. 93], through the exponent a(T) the caller
passes (D-060).
"""

from tabletop import geometry as G
from tabletop.slipnet_def import node_for_type


def touched_region(world) -> G.Region:
    """The region the touched object is "actually in" [F95 p. 65]: its exact region, which
    the program knows from the start [F95 p. 64] (D-032's reference point)."""
    touched = world.problem.touched_object()
    return G.exact_region(touched.x, touched.y)


def region_preferences(world) -> list:
    """Every region with its preference weight relative to the touched object's region
    (D-032: diagonal 1.0, opposite 0.7, near 0.4, other 0.15), in reading order."""
    touched = touched_region(world)
    return [(region, G.region_weight(region, touched, world.config)) for region in G.REGIONS]


def choose_region(world, exponent: float) -> G.Region:
    """A scout's choice of where to look (D-023, D-032): a weighted choice among the nine
    regions with the preferences raised to the temperature exponent a(T) (D-060), so that
    at a = 6 the diagonal corner is almost certain and at a = 0.1 every region is nearly
    equally likely, as "approaches determinism as the temperature approaches 0, is
    completely unbiased (i.e., uniformly distributed random) at 100" [F95 p. 93] requires."""
    preferences = region_preferences(world)
    weights = [weight ** exponent for _, weight in preferences]
    return preferences[world.rng.weighted_index(weights)][0]


def across(world, region: G.Region) -> bool:
    """Whether a region lies across the table from the touched object, "diagonally or
    directly opposite the touched object" [FH91 p. 178] (D-032's two relations; D-071)."""
    return G.region_relation(region, touched_region(world)) in ("diagonal", "opposite")


def look(world, region: G.Region, by: str = "find-group") -> list:
    """Look at one region (D-023): every object's region is drawn through the blur (D-004),
    in file order, and the objects seen in `region` are noticed in the Workspace with that
    region, their type node jolted on the first look (D-016: a descriptor noticed; the
    scouts report "what objects are in interesting areas" [FCCA p. 386]). An object seen
    across the table, "diagonally or directly opposite the touched object" [FH91 p. 178],
    is described as `opposite` the first time, which jolts that node (D-071). Returns the
    ids seen, in file order; an empty list means the look found nothing there and the
    codelet fizzles."""
    ws, config = world.workspace, world.config
    seen = []
    opposite = across(world, region)
    for obj in world.problem.objects:
        drawn = G.drawn_region(obj.x, obj.y, world.rng, config)
        if drawn != region:
            continue
        new = not ws.is_noticed(obj.id)
        ws.notice(obj.id, world.rng, by=by, region=drawn)
        if new:
            world.slipnet.jolt(node_for_type(obj.type), config.jolt_description)
        if opposite and ws.describe(obj.id, "opposite"):
            world.slipnet.jolt("opposite", config.jolt_description)
        seen.append(obj.id)
    return seen

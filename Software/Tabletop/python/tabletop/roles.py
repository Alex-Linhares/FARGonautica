"""The roles: the one registry of the 23 roles an item can play in the analogy, the role tag
as a Workspace structure with a strength, its rating and its fights, and the perception
entry point the detectors are read through (D-078, D-079).

A role is the function an item plays in the analogy, not what it looks like: "within the
particular context under discussion, X is the counterpart of Y" [F95 p. xv]; "B plays 'the
same' role as A plays" [F95 p. 12]; the "sidekick" objects beside the candidates [F95
p. 130]; the "figure object" [F95 p. 140]; the "loner" [F95 p. 118]. The registry is closed
at the 23 roles of five families (geometric, structural, selector, key, content), each with
its reading (on one table, a comparison of Henry's side with Eliza's, or off the Worldview),
its Slipnet node(s), the detector that reads it and the consumers that use it, named as the
items build them; `pending()` lists the roles whose detector or consumer is still to come.

A tag is a structure like a bond or a correspondence: proposed, rated (`rate`: its
evidence, its node's activation and the support of the Worldview's story), built, and
breakable. Rival roles for one item (`RIVALS`) fight by the Worldview's own law
(`worldview.rival_chance`, one coin under a(T)) and the loser is discarded; compatible
roles coexist. So an item's role is the run's current interpretation, not a fixed label
(the author's answer D1 of the roles loop). `detect(world, item)` is the one entry point of
perception: it asks every built detector for the roles it reads on an item; the roles loop's
oracle probe replaces it with hand-read roles, and nothing in the engine knows the
difference. Standard library only; every draw goes through the run's generator.
"""

import importlib
from dataclasses import dataclass, field
from functools import lru_cache

from tabletop import events as EV
from tabletop import temperature as TP

# The families (D-079).
GEOMETRIC, STRUCTURAL, SELECTOR, KEY, CONTENT = ("geometric", "structural", "selector",
                                                 "key", "content")
FAMILIES = (GEOMETRIC, STRUCTURAL, SELECTOR, KEY, CONTENT)
# The readings (D-078): on one side of the table alone, Henry's side against Eliza's, or
# off the correspondences already standing.
SINGLE, COMPARISON, WORLDVIEW = "single", "comparison", "worldview"
READINGS = (SINGLE, COMPARISON, WORLDVIEW)

# The 23 names (D-079).
MIRROR_IMAGE, POINT_IMAGE, SYMMETRY_AXIS = "MIRROR_IMAGE", "POINT_IMAGE", "SYMMETRY_AXIS"
MIDLINE, GAP, CONTAINER, CONNECTOR, BARRIER, CLAIMED = ("MIDLINE", "GAP", "CONTAINER",
                                                        "CONNECTOR", "BARRIER", "CLAIMED")
ROW, COLUMN, CORNER, LONER, STRAY = "ROW", "COLUMN", "CORNER", "LONER", "STRAY"
COUNTERPART, TYPE_KEY = "COUNTERPART", "TYPE_KEY"
MEMBER, SIDEKICK, ANCHOR, FIGURE, GROUND, ECHO, TWIN = ("MEMBER", "SIDEKICK", "ANCHOR",
                                                        "FIGURE", "GROUND", "ECHO", "TWIN")

# The consumers (D-095 and the later consumers' decisions): C1 role-guided proposals, C2 the role-weighted partner
# choice, C3 the answer from group position, C4 the blocked counterpart, C5 role
# slippages, C6 placement slippages and the axis, C7 role salience, C8 top-down posting.
C1, C2, C3, C4, C5, C6, C7, C8 = "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"

# Who discards a tag (D-079; `RoleDiscarded.reason`).
LOST, BROKEN, STRUCTURE_GONE = "lost", "broken", "structure gone"
# The actor of a tag the oracle probe hands in (`RoleTagged.by`).
PERCEIVED = "perceived"
# The items that carry the table-wide roles (D-084, D-083): the MIDLINE is tagged on the
# table itself, the SYMMETRY_AXIS on the Worldview; neither is an object or a group.
TABLE, WORLDVIEW_ITEM = "table", "worldview"
# The words of the table frame (D-029, D-085): the row an item is tagged with, Henry's or
# Eliza's (the middle band is of neither side, and gets no ROW), and the three columns.
ABOVE, BELOW = "above", "below"
ROW_WORDS = (ABOVE, BELOW)
# The detector of COUNTERPART (D-093): the re-cast reads every candidate at once, from the
# recast-role codelet, so `detect` leaves this row to it.
RECAST_DETECTOR = "roles.recast"


@dataclass(frozen=True)
class RoleSpec:
    """One row of the registry (D-079): the role's `name` and short `code` (G1, S5, ...),
    its `family`, its `reading`, the Slipnet `nodes` it jolts and is posted by (the first
    is the node its strength reads), the `detector` that perceives it (a `module.function`
    name, empty until its item builds it), the `consumers` built so far, `pair` (its tags
    name a pair the consumer C1 may propose: a role read on one table is paired tag with
    tag across the table, whatever its `args` name, as a sidekick's name the object it
    stands beside, D-090; a comparison or Worldview role carries its partner in `args`,
    D-095), and the `item` that built it."""

    name: str
    code: str
    family: str
    reading: str
    nodes: tuple
    detector: str = ""
    consumers: tuple = ()
    pair: bool = False
    item: str = ""

    @property
    def pending(self) -> bool:
        """Whether a later item still owes this role its detector or a consumer (D-079)."""
        return not self.detector or not self.consumers


def _r(name, code, family, reading, nodes, detector="", consumers=(), pair=False, item=""):
    return RoleSpec(name, code, family, reading, tuple(nodes), detector, tuple(consumers), pair,
                    item)


# The registry, closed at these 23 (D-079; the author's answer: a new role needs an answer,
# not an item). The detectors and consumers were filled in by items 04 to 10 (`pending()`
# is empty); C1 (D-095) is built here and reads every pair role, so it is named on each (a
# GAP's pairs are the parts it marks off, D-086; a STRAY is what C1 leaves out); C3, the
# answer from group position (D-097), matches the members' roles (anchor, connector, end,
# middle) and reads the setting around an anchor; C5, the role slippage (D-098), reads
# every single-table role on the two ends of a correspondence but the selectors of place
# (`correspondences.C5_LEAVES_OUT`); C7 (D-100) reads ANCHOR, CONNECTOR, FIGURE and STRAY
# into salience.
REGISTRY = (
    _r(MIRROR_IMAGE, "G1", GEOMETRIC, COMPARISON, ("mirror symmetry", "opposite"),
       "correspondences.mirror_and_point_images", consumers=(C1, C6), pair=True, item="04"),
    _r(POINT_IMAGE, "G2", GEOMETRIC, COMPARISON, ("diagonal symmetry", "opposite"),
       "correspondences.mirror_and_point_images", consumers=(C1, C6), pair=True, item="04"),
    _r(SYMMETRY_AXIS, "G3", GEOMETRIC, WORLDVIEW, ("symmetry",), "worldview.symmetry_axis",
       consumers=(C6, C1), item="04"),
    _r(MIDLINE, "S1", STRUCTURAL, SINGLE, ("separator",), "describe.midline_perceived",
       consumers=(C6, C1), item="04"),
    _r(GAP, "S2", STRUCTURAL, SINGLE, ("separator",), "describe.gaps_of", consumers=(C1,),
       pair=True, item="08"),
    _r(CONTAINER, "S3", STRUCTURAL, SINGLE, ("container",), "describe.container_of",
       consumers=(C1, C3), pair=True, item="06"),
    _r(CONNECTOR, "S4", STRUCTURAL, SINGLE, ("connector",), "describe.connector_of",
       consumers=(C1, C5, C3), pair=True, item="05"),
    _r(BARRIER, "S5", STRUCTURAL, WORLDVIEW, ("barrier",), "worldview.barrier_and_claimed",
       consumers=(C4,), item="10"),
    _r(CLAIMED, "S6", STRUCTURAL, WORLDVIEW, ("barrier",), "worldview.barrier_and_claimed",
       consumers=(C2, C4), item="10"),
    _r(ROW, "L1", SELECTOR, SINGLE, ("row",), "describe.row_role", consumers=(C2, C1),
       item="07"),
    _r(COLUMN, "L2", SELECTOR, SINGLE, ("column",), "describe.column_role",
       consumers=(C2, C6), item="07"),
    _r(CORNER, "L3", SELECTOR, SINGLE, ("corner",), "describe.corner_role", consumers=(C1,),
       pair=True, item="08"),
    _r(LONER, "L4", SELECTOR, SINGLE, ("isolated",), "describe.loner_of", consumers=(C1, C5),
       pair=True, item="08"),
    _r(STRAY, "L5", SELECTOR, SINGLE, ("separator",), "describe.stray_role",
       consumers=(C7, C1), item="08"),
    _r(COUNTERPART, "K1", KEY, COMPARISON, ("counterpart",), "roles.recast",
       consumers=(C2, C1), item="07"),
    _r(TYPE_KEY, "K2", KEY, WORLDVIEW, ("key",), "worldview.type_keys", consumers=(C1, C5),
       pair=True, item="09"),
    _r(MEMBER, "A1", CONTENT, SINGLE, ("group", "end", "middle"), "describe.member_roles",
       consumers=(C5, C3), item="05"),
    _r(SIDEKICK, "A2", CONTENT, SINGLE, ("sidekick",), "describe.sidekicks_of",
       consumers=(C1, C5), pair=True, item="05"),
    _r(ANCHOR, "A3", CONTENT, SINGLE, ("anchor",), "describe.anchor_of",
       consumers=(C1, C5, C3), pair=True, item="05"),
    _r(FIGURE, "A4", CONTENT, SINGLE, ("figure",), "describe.figure_role",
       consumers=(C1, C5, C7), pair=True, item="09"),
    _r(GROUND, "A5", CONTENT, SINGLE, ("ground",), "describe.ground_role",
       consumers=(C1, C5), pair=True, item="09"),
    _r(ECHO, "A6", CONTENT, COMPARISON, ("echo",), "correspondences.echoes_of",
       consumers=(C1,), pair=True, item="09"),
    _r(TWIN, "A7", CONTENT, COMPARISON, ("sameness",), "correspondences.twin_of",
       consumers=(C2,), item="09"),
)
NAMES = tuple(spec.name for spec in REGISTRY)
_INDEX = {spec.name: spec for spec in REGISTRY}

# The roles that cannot both tag one item (D-079): a reading of the same item or pair that
# excludes the other. A newcomer fights every standing rival on its item; the loser is
# discarded. Compatible roles coexist (a glass can be a MEMBER and an ANCHOR).
RIVALS = ((BARRIER, SIDEKICK), (MIRROR_IMAGE, POINT_IMAGE), (LONER, MEMBER), (STRAY, MEMBER),
          (STRAY, LONER), (FIGURE, GROUND))


def spec(name: str) -> RoleSpec:
    """The registry row of a role name (D-079); KeyError for a name outside the 23."""
    return _INDEX[name]


def of_family(family: str) -> list:
    """The roles of one family, in the registry's order (D-079)."""
    return [s for s in REGISTRY if s.family == family]


def pending() -> list:
    """The roles a later item still owes a detector or a consumer (D-079): the list the
    items 04 to 10 emptied role by role; empty since CONTAINER (item 06)."""
    return [s.name for s in REGISTRY if s.pending]


def rivals_of(name: str) -> list:
    """The roles that fight this one for an item (D-079)."""
    return [b if a == name else a for a, b in RIVALS if name in (a, b)]


# ---------------------------------------------------------------------------------------------
# The tag (D-079).


@dataclass(eq=False)
class RoleTag:
    """A role tag, compared by identity (D-079): the `item` (an object id, a group name,
    or `worldview` for the axis), the `role`, its `args` (the partner of a pair role, or
    the item a sidekick stands beside), its `weight` (the number of objects in the item
    carrying it: 1 for an object, n for a group, n for a run of identical sidekicks, "weak
    sidekicks (i.e., single objects as opposed to small sameness groups) are not
    sufficient" [F95 p. 131]), the `evidence` it was read from (the descriptions, in
    words), `by` (the codelet type, or `perceived`), `built_at` (the clock), its `name`
    (`r1`, `r2`, ... as the Workspace gives it) and its `strength`, written by `rate`."""

    item: str
    role: str
    args: tuple = ()
    weight: int = 1
    evidence: str = ""
    by: str = ""
    built_at: int = 0
    name: str = ""
    strength: float = 0.0
    consumed: list = field(default_factory=list)

    def same_as(self, item: str, role: str, args) -> bool:
        """Whether this tag is the one reading: the same item, role and arguments (D-079)."""
        return self.item == item and self.role == role and tuple(self.args) == tuple(args)

    def label(self) -> str:
        """The tag as the trace prints it: `r3 cup2 LONER`, with the arguments after a
        colon when it has any (`r5 spoon1 SIDEKICK:cup1`) (D-105)."""
        text = f"{self.name} {self.item} {self.role}"
        return text + (":" + " ".join(self.args) if self.args else "")


def rate(world, tag: RoleTag) -> float:
    """A tag's strength, in 0 to 100 (D-079): `role_weight_points` per unit of weight; the
    activation of the role's first node over 150 times `role_node_points`, as French's
    slippages take their node's activation [F95 p. 68]; and `role_story_points` per
    correspondence of the Worldview that has the tag's item, or an item it names, at an
    end: the support of the story, which the tag loses when the Worldview changes (the
    author's answer D1). Written on the tag and returned; draws nothing."""
    cfg = world.config
    node = spec(tag.role).nodes[0]
    points = cfg.role_weight_points * tag.weight
    points += cfg.role_node_points * world.slipnet.activation_of(node) / 150.0
    named = {tag.item, *tag.args}
    support = sum(1 for cp in world.workspace.worldview() if named & set(cp.ends))
    points += cfg.role_story_points * support
    tag.strength = max(0.0, min(100.0, points))
    return tag.strength


def propose(world, item: str, role: str, args=(), weight: int = 1, evidence: str = "",
            by: str = "", exponent: float = 1.0) -> "RoleTag | None":
    """Propose a role tag (D-079): the same reading already standing is re-rated and
    returned; otherwise the tag is built in the Workspace's pool (`RoleTagged`), its node
    jolted by `jolt_description` (D-016), and it fights every rival standing on its item
    by `worldview.rival_chance` under a(T), the loser discarded (`RoleDiscarded`, `lost`).
    Returns the tag that stands, or None when the newcomer lost."""
    from tabletop import worldview as WV
    ws = world.workspace
    standing = ws.role_tag(item, role, args)
    if standing is not None:
        rate(world, standing)
        return standing
    tag = RoleTag(item, role, tuple(args), weight, evidence, by or world.events.actor, world.clock)
    rate(world, tag)
    ws.tag_role(tag)
    world.slipnet.jolt(spec(role).nodes[0], world.config.jolt_description)
    for rival in [t for t in ws.roles_of(item) if t.role in rivals_of(role)]:
        rate(world, rival)
        chance = WV.rival_chance(rival.strength, tag.strength, exponent)
        if TP.passes(chance, 1.0, world.rng):      # softened already: one coin
            ws.discard_role(rival, f"{LOST} to {tag.name}", by)
        else:
            ws.discard_role(tag, f"{LOST} to {rival.name}", by)
            return None
    return tag


def break_chance(world, tag: RoleTag) -> float:
    """The chance that a re-examined tag breaks (D-079): 1 - strength / `role_break_scale`,
    floored at 0, before the temperature law; a tag at or above the scale cannot be
    broken, and a strong, story-supported tag is hard to break when the temperature is
    low, as French's structures are."""
    return max(0.0, 1.0 - tag.strength / world.config.role_break_scale)


def detect(world, item: str) -> list:
    """The perception entry point (D-079): the roles the built detectors read on an item,
    as (role, args, weight, evidence) tuples, called at an object's first sight
    (`World.step`) and by the find-roles codelet from the item that builds it. It asks the
    registry's detectors in order; a role with no detector yet reads nothing, and the
    re-cast (COUNTERPART, D-093) is the recast-role codelet's, read for every candidate
    at once. The oracle probe of RESULTS.md §11.3 replaces this function with hand-read
    roles."""
    found = []
    for module, function_name in detectors():
        # the function is looked up at every call, so that a probe or a lesion that
        # replaces it in its module is what runs
        found.extend(getattr(module, function_name)(world, item))
    return found


@lru_cache(maxsize=1)
def detectors() -> tuple:
    """The registry's distinct detectors in registry order, each as (module, function
    name) with the module imported, read once (D-079, D-112): one detector may read
    several roles (the two images); the re-cast's is not a detector of this kind."""
    out, names = [], []
    for s in REGISTRY:
        if s.detector and s.detector != RECAST_DETECTOR and s.detector not in names:
            names.append(s.detector)
            module_name, function_name = s.detector.split(".")
            out.append((importlib.import_module(f"tabletop.{module_name}"), function_name))
    return tuple(out)


def perceive(world, item: str, by: str, exponent: float) -> list:
    """Read an item's roles through `detect` and propose each (D-079); the tags that stand,
    in order. Nothing is gated: an item with no role gets no tag and changes no rating."""
    tags = []
    for role, args, weight, evidence in detect(world, item):
        tag = propose(world, item, role, args, weight, evidence, by, exponent)
        if tag is not None:
            tags.append(tag)
    return tags


# ---------------------------------------------------------------------------------------------
# The re-cast: COUNTERPART, the one place a role is carried across the table (D-093).

# The single-table roles that make up an item's signature (D-093): what it is inside its
# side of the table, not where it stands (ROW and COLUMN are the tiers' criteria).
SIGNATURE_ROLES = (MEMBER, ANCHOR, CONNECTOR, CORNER, LONER, CONTAINER, FIGURE, GROUND)
# The tiers (D-093): four of the signature's transport, and a fifth, last, for a candidate
# CLAIMED by a standing correspondence (D-089, D-101: choosing it means breaking that
# structure).
TIERS = (1, 2, 3, 4, 5)
CLAIMED_TIER = 5
# The `args` of a COUNTERPART tag: the touched object, then the tier as a word.
TIER_WORDS = {tier: f"tier{tier}" for tier in TIERS}
TIER_EVIDENCE = {1: "the image under the axis, playing the signature",
                 2: "the signature in the same column",
                 3: "the same type", 4: "in the other row",
                 5: "claimed by a standing correspondence"}


@dataclass(frozen=True)
class Recast:
    """The re-cast as published (D-093, D-105): the `touched` object, its `signature`
    (words, sorted), the `tiers` as (tier, candidate, weight) tuples in tier order and the
    candidates' file order inside a tier, whether the first non-empty tier is
    `ambiguous` (two or more candidates: a tie, published and never resolved), and the
    `clock` it was read at. Kept on the Workspace (`recast`) for the snapshot."""

    touched: str
    signature: tuple
    tiers: tuple
    ambiguous: bool
    clock: int

    def weight_of(self, item: str) -> float:
        """The published weight of a candidate, 0 for an item outside the re-cast (D-093)."""
        for _, candidate, weight in self.tiers:
            if candidate == item:
                return weight
        return 0.0

    def first_tier(self) -> list:
        """The candidates of the first non-empty tier, in order; empty with none (D-093)."""
        for tier in TIERS:
            found = [c for t, c, _ in self.tiers if t == tier]
            if found:
                return found
        return []


def row_word(world, item: str) -> str | None:
    """The row an item is tagged with, `above` (Henry's) or `below` (Eliza's), or None
    while no ROW tag stands on it (D-085)."""
    tags = [t for t in world.workspace.roles_of(item) if t.role == ROW]
    return tags[-1].args[0] if tags else None


def column_word(world, item: str) -> str | None:
    """The column an item is tagged with, `left`, `middle` or `right`, or None while no
    COLUMN tag stands on it (D-085)."""
    tags = [t for t in world.workspace.roles_of(item) if t.role == COLUMN]
    return tags[-1].args[0] if tags else None


def signature_of(world, item: str) -> tuple:
    """An object's role signature (D-093), read from the tags standing on it, so that a
    discarded role is never re-cast: the names of its signature roles (`SIGNATURE_ROLES`:
    member, anchor, connector, ...), in lower case; `end` or `middle` when it is a member
    with that group-frame word (D-029); `sidekick:<category>` for each SIDEKICK tag
    standing on another object and naming it (the sidekick's nearest ISA category, D-009,
    or its type when it has none: the doubled cups beside the touched spoon read as
    liquid holders); and `group:<categories>` for the gestalt group it is a member of, its
    members' nearest categories sorted and joined. Sorted words; empty for an object with
    no such tag."""
    from tabletop import describe as DS
    ws = world.workspace
    words = []
    for tag in ws.roles_of(item):
        if tag.role in SIGNATURE_ROLES:
            words.append(tag.role.lower())
    if MEMBER in [t.role for t in ws.roles_of(item)]:
        group = DS.gestalt_group_of(world, item)
        if group is not None:
            words.extend(w for w in DS.group_frame(group, item) if w in (DS.END, DS.MIDDLE))
            categories = sorted(DS.nearest_category(world, m) or world.problem.object(m).type
                                for m in group.members)
            words.append("group:" + ",".join(categories))
    for tag in ws.roles:
        if tag.role == SIDEKICK and item in tag.args and tag.item in world.problem.ids():
            words.append("sidekick:" + (DS.nearest_category(world, tag.item)
                                        or world.problem.object(tag.item).type))
    return tuple(sorted(set(words)))


def plays(candidate_signature: tuple, signature: tuple) -> bool:
    """Whether a candidate plays every part of the touched object's signature (D-093): a
    candidate carrying more roles still plays each of these; "B plays 'the same' role as A
    plays" [F95 p. 12]."""
    return set(signature) <= set(candidate_signature)


def image_under_axis(world, touched: str, candidate: str) -> bool:
    """Whether the candidate is the touched object's image under the standing SYMMETRY_AXIS
    (D-083, D-093): a MIRROR_IMAGE tag under a mirror axis, a POINT_IMAGE tag under a
    point axis, read either way round; False while no axis stands."""
    from tabletop import geometry as G
    from tabletop import worldview as WV
    axis = WV.axis_of(world)
    role = {G.MIRROR: MIRROR_IMAGE, G.POINT: POINT_IMAGE}.get(axis)
    if role is None:
        return False
    return any(t.role == role and ((t.item == touched and candidate in t.args)
                                   or (t.item == candidate and touched in t.args))
               for t in world.workspace.roles)


def is_twin(world, touched: str, candidate: str) -> bool:
    """Whether a TWIN tag stands on the candidate naming the touched object (D-092,
    D-093): the third tier reads the perceived twin, not the layout's type."""
    return any(t.role == TWIN and t.item == candidate and touched in t.args
               for t in world.workspace.roles)


def is_claimed(world, candidate: str) -> bool:
    """Whether a CLAIMED tag stands on the candidate (D-089, D-101): it ends a Worldview
    correspondence from an item other than the touched object, read by
    read-worldview-roles; consumer C2 puts it in the last tier."""
    return any(t.role == CLAIMED and t.item == candidate for t in world.workspace.roles)


def tier_of(world, touched: str, candidate: str, signature: tuple) -> int:
    """The tier a candidate of the other row falls in (D-093): its natural tier
    (`natural_tier`), or 5, last, when it is CLAIMED by a standing correspondence
    (`is_claimed`, D-089, D-101), whatever else it would be."""
    if is_claimed(world, candidate):
        return CLAIMED_TIER
    return natural_tier(world, touched, candidate, signature)


def natural_tier(world, touched: str, candidate: str, signature: tuple) -> int:
    """The best tier a candidate of the other row falls in by the signature's transport
    alone (D-093): 1, the image under the axis with the touched object's signature; 2,
    the signature in the touched object's column (both COLUMN tags standing and equal);
    3, a TWIN of the touched object (`is_twin`, D-092: the tag, read by compare-roles,
    not the layout's type); 4, anything else in the row. The claim is left out: this is
    what makes a candidate obvious (D-089), before any structure claims it."""
    theirs = signature_of(world, candidate)
    if plays(theirs, signature):
        if image_under_axis(world, touched, candidate):
            return 1
        column = column_word(world, touched)
        if column is not None and column_word(world, candidate) == column:
            return 2
    if is_twin(world, touched, candidate):
        return 3
    return 4


def candidates_of(world) -> tuple | None:
    """The re-cast's input (D-093): the touched object, its signature and the objects in
    view tagged with the ROW opposite its own; None while the touched object carries no
    ROW tag or no object is tagged with the other row."""
    touched = world.problem.touched
    mine = row_word(world, touched)
    if mine is None:
        return None
    other = BELOW if mine == ABOVE else ABOVE
    candidates = [o for o in world.workspace.noticed
                  if o != touched and row_word(world, o) == other]
    if not candidates:
        return None
    return touched, signature_of(world, touched), candidates


def natural_first_tier(world) -> list:
    """The candidates of the first non-empty natural tier (`natural_tier`, D-093), the
    claim left out: what BARRIER and CLAIMED are read against (D-089). Empty while the
    re-cast reads nothing."""
    read = candidates_of(world)
    if read is None:
        return []
    touched, signature, candidates = read
    placed = [(natural_tier(world, touched, c, signature), c) for c in candidates]
    first = min(t for t, _ in placed)
    return [c for t, c in placed if t == first]


def recast(world) -> Recast | None:
    """The one re-cast (D-093, role K1): the touched object's signature carried to the
    other row, "the cup in front of Henry" into the schema "the cup in front of person X."
    [F95 p. 6]. The candidates are the objects in view tagged with the ROW opposite the
    touched object's own ROW tag (D-085: the transport step needs both rows read); each
    falls in its best tier (`tier_of`; the third is the TWIN tag's, D-092; the fifth and
    last the CLAIMED candidates', D-101) with that tier's weight, `role_recast_tiers`; a tie
    inside a tier is published with equal weights and never resolved, since Eliza must
    touch something and the Coderack chooses. None while the touched object carries no
    ROW tag or no object is tagged with the other row: nothing is gated. Draws nothing."""
    read = candidates_of(world)
    if read is None:
        return None
    touched, signature, candidates = read
    weights = world.config.role_recast_tiers
    placed = [(tier_of(world, touched, c, signature), c) for c in candidates]
    tiers = tuple((tier, c, float(weights[tier - 1])) for tier in TIERS
                  for t, c in placed if t == tier)
    first = [c for t, c, _ in tiers if t == tiers[0][0]]
    return Recast(touched, signature, tiers, len(first) >= 2, world.clock)


def consumed(world, tag: RoleTag, consumer: str, effect: str, by: str = "") -> None:
    """Record that a consumer read a tag (D-095, D-105): kept on the tag and published as
    `RoleConsumed`, so that the trace and the GUI can say which reading moved what."""
    tag.consumed.append((consumer, effect))
    world.events.record(EV.RoleConsumed, role=tag.role, consumer=consumer, item=tag.item,
                        effect=effect, by=by or world.events.actor)

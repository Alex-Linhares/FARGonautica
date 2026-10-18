"""Correspondences: the structure, the conditions of its creation, its supporting
slippages, the static, dynamic and bonus points of its rating, and the probabilistic test
it must pass before it may compete (D-030, D-035 to D-039, D-045).

"A correspondence is defined by its end objects." [F95 p. 67] "The two 'objects' in
correspondence may be either simple objects—e.g., a fork—or chunked groups of objects—e.g.,
a group consisting of a fork, a knife, and a spoon." [F95 p. 68] "Its rating is a function
of three types of factors: static factors, dynamic factors, and a 'bonus' factor." [F95
p. 67] The static factors "involve, for the most part, slippages that 'support' the
correspondence" [F95 p. 68]: a left end mapped to a right end gives "a left-right slippage"
[F95 p. 68]; "When a concept slips to itself (e.g., right-right), this is a degenerate case
of the notion of slippage, called an identity." [F95 p. 68] French lists nine static
factors [F95 pp. 68-69] and three dynamic ones [F95 p. 69]; "the points from the static and
dynamic evaluations and from the winning or losing competitions are added together. This
determines the strength of a particular correspondence." [F95 p. 69] No points are given;
the points are D-036's to D-038's (`Config.cp_static_weights`, `type_weight`,
`cp_dynamic_points`, `win_bonus`, `loss_penalty`), with the group-to-object penalty of D-045
relaxed from the bias French says "should probably be relaxed" [F95 p. 125].

"The incorporation of correspondences into the Worldview is a three-stage process:
correspondence creation, initial testing, and competition." [F95 p. 71] The first two are
here (`create`, `describe`, `test`); the competition is the Worldview's (D-040 to D-044).
Points are recomputed every time a rating is read, from the Slipnet and the Workspace as
they stand; reading a rating draws nothing from the generator.
"""

from dataclasses import dataclass, field

from tabletop import describe as DS
from tabletop import events as EV
from tabletop import geometry as G
from tabletop import roles as R
from tabletop import salience as S
from tabletop import slipnet_def as SD
from tabletop import temperature as TP
from tabletop.groups import SAMENESS, SHARED_ISA

OBJECT, GROUP = "object", "group"       # the two kinds of end [F95 p. 68]
TOP = "top"                             # the level of a correspondence outside any other
# The kinds of slippage, in the order of French's static factors [F95 pp. 68-69] (D-036).
KIND, END_MIDDLE, POSITION, TYPE, NUMBER, ISA, ORIENTATION = (
    "kind", "end-middle", "position", "type", "number", "isa", "orientation")
PLACEMENT = "placement"       # the table-frame slippage of a top-level correspondence (D-082)
ROLE = "role"                 # the role slippage of two ends sharing a role (D-098, consumer C5)
MIDDLE_LABELS = ("symmetry",)   # the placement of two middle-column items (D-082, D-032)
HORIZONTAL_WORDS, VERTICAL_WORDS = ("left", "right"), ("above", "below")
FLIP_LABELS = ("opposite", "diagonal symmetry")     # a left-right slippage's nodes [F95 p. 68]
IDENTITY_LABELS = ("mirror symmetry", "sameness")   # a right-right identity's [F95 p. 85]
CHANCE_FLOOR, CHANCE_CEILING = 0.01, 0.99           # the test's chance is clipped (D-039)


@dataclass(frozen=True)
class Slippage:
    """One slippage supporting a correspondence [F95 p. 68]: its `kind` (one of the seven
    above), the descriptor `a` at the first end slipping to `b` at the second, and the
    `labels`, the Slipnet nodes it feeds when the correspondence enters the Worldview
    (D-030). An identity has `a == b`."""

    kind: str
    a: str
    b: str
    labels: tuple = ()

    @property
    def identity(self) -> bool:
        """"When a concept slips to itself (e.g., right-right), this is a degenerate case of
        the notion of slippage, called an identity." [F95 p. 68]"""
        return self.a == self.b

    def as_tuple(self) -> tuple:
        """(from, to, label) for the trace and the events (`docs/trace-format.md`); the
        first label, or "" when the slippage feeds no node (D-030)."""
        return (self.a, self.b, self.labels[0] if self.labels else "")


@dataclass(eq=False)
class Correspondence:
    """A correspondence between two items, compared by identity (D-035). `henry` and
    `eliza` are its two ends, the first and the second item picked (object ids or group
    names; the names follow the game, not fixed sides, D-005); `name` is `cp1`, `cp2`, ...
    as the Workspace gives it; `parent` is the group-group correspondence it was built
    inside, if any [F95 p. 108], and `level` is `top` or the parent's name; `slippages`
    are written by `describe`; `static`, `dynamic` and `bonus` are the three parts of the
    rating [F95 p. 67], the first two recomputed by `evaluate`, the third accumulated by
    `award` (D-038); `in_worldview` is set by the Worldview (D-040); `tested`, `chance` and
    `passed` record the last test (D-039); `wins` and `losses` the competitions."""

    henry: str
    eliza: str
    name: str = ""
    parent: "Correspondence | None" = None
    built_at: int = 0
    by: str = ""
    slippages: list = field(default_factory=list)
    static: float = 0.0
    dynamic: float = 0.0
    bonus: float = 0.0
    in_worldview: bool = False
    tested: bool = False
    chance: float = 0.0
    passed: bool = False
    wins: int = 0
    losses: int = 0

    @property
    def ends(self) -> tuple:
        """The two end items, "A correspondence is defined by its end objects." [F95 p. 67]"""
        return (self.henry, self.eliza)

    @property
    def level(self) -> str:
        """`top`, or the name of the group correspondence this one lies inside (D-044; the
        levels of [F95 p. 108])."""
        return TOP if self.parent is None else self.parent.name

    @property
    def rating(self) -> float:
        """The strength: static, dynamic and bonus points "added together" [F95 p. 69],
        never below 0 (D-038)."""
        return max(0.0, self.static + self.dynamic + self.bonus)

    def has_end(self, item: str) -> bool:
        """Whether the item is one of the two ends (D-035, D-044: rivals share an end)."""
        return item in self.ends

    def label(self) -> str:
        """The trace's form, `cp2 cup1=small-glass3` (`docs/trace-format.md`; D-035)."""
        return f"{self.name} {self.henry}={self.eliza}"


# ---------------------------------------------------------------------------------------------
# Items: objects and groups as ends (D-035).


def kind_of_end(world, item: str) -> str:
    """`object` for an object id, `group` for a group name [F95 p. 68] (D-035)."""
    return GROUP if S.is_group(world, item) else OBJECT


def members_of(world, item: str) -> list:
    """The objects an item stands for: the object itself, or a group's members in file
    order (D-035: "neither contains the other")."""
    if S.is_group(world, item):
        return list(world.workspace.group_named(item).members)
    return [item]


def contains(world, outer: str, inner: str) -> bool:
    """Whether `outer` is a group that contains `inner` (an object among its members, or a
    different group whose members are all among its own) (D-035)."""
    if outer == inner or not S.is_group(world, outer):
        return False
    return world.workspace.group_named(outer).contains(members_of(world, inner))


def in_one_group(world, a: str, b: str) -> bool:
    """Whether some group holds both items, "the objects in question must not be in the
    same group" [F95 p. 72] (D-035): a group containing both a and b, where a group
    contains an object that is its member and a different group inside it."""
    for g in world.workspace.groups:
        if contains(world, g.name, a) and contains(world, g.name, b):
            return True
    return False


def creatable(world, a: str, b: str) -> tuple:
    """Whether a correspondence between two items may be created, "certain
    not-too-stringent conditions" [F95 p. 71] (D-035): the items are distinct, neither
    contains the other, no group holds both, both are in view, and no correspondence
    between the two exists yet. Returns (True, "") or (False, the reason)."""
    ws = world.workspace
    if a == b:
        return False, "same item"
    if a not in ws.items() or b not in ws.items():
        return False, "not in view"
    if contains(world, a, b) or contains(world, b, a):
        return False, "one contains the other"
    if in_one_group(world, a, b):
        return False, "in one group"
    if ws.correspondence_between(a, b) is not None:
        return False, "exists"
    return True, ""


def create(world, a: str, b: str, by: str = "create-cp", parent=None):
    """What a create-cp codelet does (D-035): if the pair is creatable, a tentative
    correspondence is recorded in the Workspace, "A correspondence will not even be
    created unless certain not-too-stringent conditions are met" [F95 p. 71], with its
    slippages as the ends' descriptors stand; a `parent` group correspondence makes it a
    correspondence inside that one [F95 p. 108] (its ends must then lie inside the parent's
    groups). Returns the correspondence, or None when the codelet fizzles."""
    ok, _ = creatable(world, a, b)
    if not ok:
        return None
    if parent is not None and not (contains(world, parent.henry, a) and contains(world, parent.eliza, b)):
        return None
    cp = Correspondence(henry=a, eliza=b, parent=parent, built_at=world.clock, by=by)
    events = world.workspace.events
    cp.slippages = slippages_of(world, cp)      # before it is in view: an observer of the
    world.workspace.add_correspondence(cp)      # consumption C6 records must see whole ones
    evaluate(world, cp)
    events.record(EV.CorrespondenceProposed, correspondence=cp.name, henry=a, eliza=b,
                  level=cp.level, by=by)
    events.record(EV.CorrespondenceBuilt, correspondence=cp.name, henry=a, eliza=b,
                  static=cp.static, dynamic=cp.dynamic, bonus=cp.bonus)
    return cp


# ---------------------------------------------------------------------------------------------
# Slippages (D-030, D-036).


def type_node_of(world, item: str) -> str | None:
    """The Slipnet node of an item's type: an object's type, a sameness group's one type
    (so that "a spoon-spoon group" may map "to an individual spoon" [F95 p. 111] on its
    type), None for any other group (D-036, factor 5 as built)."""
    if not S.is_group(world, item):
        return SD.node_for_type(world.problem.object(item).type)
    group = world.workspace.group_named(item)
    if group.kind == SAMENESS:
        return SD.node_for_type(world.problem.object(group.members[0]).type)
    return None


def position_words(world, item: str) -> list:
    """An object's group-frame words (`end`, `middle`, `left`, ...) in the order found, over
    every group it is in (D-029); empty for a group (D-036, factors 3 and 4)."""
    return S.position_words(world, item)


def _word_on_axis(words, axis_words) -> str | None:
    for word in words:
        if word in axis_words:
            return word
    return None


def type_slippage(world, a_node: str, b_node: str) -> Slippage:
    """The slippage between two type nodes (D-030): an identity feeds "sameness"; a labeled
    link feeds its labels ("used with", "similar shape to", the size labels); two types
    joined only through a category feed that category; nothing otherwise."""
    if a_node == b_node:
        return Slippage(TYPE, a_node, b_node, ("sameness",))
    net = world.slipnet
    labels = net.labels_between(a_node, b_node)
    if labels:
        return Slippage(TYPE, a_node, b_node, tuple(labels))
    shared = [c for c in net.categories_of(a_node) if c in net.categories_of(b_node)]
    return Slippage(TYPE, a_node, b_node, tuple(shared[:1]))


def position_slippages(world, cp) -> list:
    """The end/middle and position slippages of two objects in groups [F95 p. 68] (D-030,
    D-036 factors 3-4): `end`-`end` or `middle`-`middle` identities and `end`-`middle`
    slippages (the link's label, "position"); on each axis, an identity (left-left, feeding
    "mirror symmetry" and "sameness") or a flip (left-right, feeding "opposite" and
    "diagonal symmetry" [F95 p. 68]); nothing for a group or an object in no group."""
    found = []
    wa, wb = position_words(world, cp.henry), position_words(world, cp.eliza)
    if not wa or not wb:
        return found
    for word_a in (DS.END, DS.MIDDLE):
        if word_a not in wa:
            continue
        for word_b in (DS.END, DS.MIDDLE):
            if word_b in wb:
                labels = (IDENTITY_LABELS if word_a == word_b
                          else world.slipnet.labels_between(word_a, word_b))
                found.append(Slippage(END_MIDDLE, word_a, word_b, tuple(labels)))
    for axis in (HORIZONTAL_WORDS, VERTICAL_WORDS):
        word_a, word_b = _word_on_axis(wa, axis), _word_on_axis(wb, axis)
        if word_a is None or word_b is None:
            continue
        labels = IDENTITY_LABELS if word_a == word_b else FLIP_LABELS
        found.append(Slippage(POSITION, word_a, word_b, labels))
    return found


def placement_slippages(world, cp) -> list:
    """Consumer C6, the placement slippage of a top-level correspondence in the table frame
    (D-082; D-029's second frame, "where the terms 'left' and 'right' refer to the sides of
    the table as a whole" [F95 p. 85], built at last): the two ends' regions are drawn
    (`describe.seen_region`, through the blur) and their `geometry.placement` read: a
    column identity (`mirror`, the end "directly opposite" [F95 p. 66]) feeds "mirror
    symmetry" and "sameness", as French's two identity slippages do [F95 p. 85]; a column
    flip (`point`, "diagonally opposite") feeds "opposite" and "diagonal symmetry", the
    left-right slippage's nodes [F95 p. 68]; two middle-column ends feed "symmetry" alone
    (the two images coincide, D-032). Nothing for a correspondence inside a group
    correspondence (the group frame is theirs, D-029), for ends in one row or in the
    middle band, or while no MIDLINE is perceived (D-084): a table with no perceived
    midline gets no placement points. An end tagged with a COLUMN (D-085) is read in the
    column of its tag, the perceived column, rather than drawn afresh (`tagged_column`;
    consumer C6 of COLUMN); an end with no tag is drawn as before. Scored by factor 4
    (`position_points`) with the `position` weight and the symmetry scale, parallel
    through the labels (D-043)."""
    if cp.parent is not None or not world.workspace.has_role(R.TABLE, R.MIDLINE):
        return []
    ra, rb = DS.seen_region(world, cp.henry), DS.seen_region(world, cp.eliza)
    ra, rb = tagged_column(world, cp.henry, ra), tagged_column(world, cp.eliza, rb)
    kind = G.placement(ra, rb)
    if kind == G.NONE:
        return []
    if kind == G.MIDDLE:
        return [Slippage(PLACEMENT, "middle", "middle", MIDDLE_LABELS)]
    labels = IDENTITY_LABELS if kind == G.MIRROR else FLIP_LABELS
    return [Slippage(PLACEMENT, G.COLUMNS[ra.column], G.COLUMNS[rb.column], labels)]


def tagged_column(world, item: str, drawn: G.Region) -> G.Region:
    """The region an end is read in by the placement (D-085, consumer C6 of COLUMN): the
    drawn region with its column replaced by the item's standing COLUMN tag's, when it
    has one, the consumption recorded on the tag (`RoleConsumed`); the drawn region as it
    is otherwise. Draws nothing."""
    tags = [t for t in world.workspace.roles_of(item) if t.role == R.COLUMN]
    if not tags:
        return drawn
    tag = tags[-1]
    R.consumed(world, tag, R.C6, f"placement of {item} read in the {tag.args[0]} column")
    return G.Region(G.COLUMNS.index(tag.args[0]), drawn.row)


def mirror_and_point_images(world, item: str) -> list:
    """The detector of the two image roles, MIRROR_IMAGE and POINT_IMAGE (D-099, roles G1
    and G2), a comparison of one item with the items of the other row: for an object or a
    group in view, every other item in view whose seen region (`describe.seen_region`,
    through the blur) is the item's reflection across the midline is its mirror image,
    "directly opposite" [F95 p. 66], and every one at its reflection through the centre
    its point image, "diagonally opposite" [F95 p. 28]; in the middle column the two
    coincide and neither is read (D-032). Needs a perceived MIDLINE (D-084). Each reading
    is (role, (partner,), the objects in the item, the two regions); the tag's weight is
    the item's objects (1 for an object, n for a group)."""
    ws = world.workspace
    if item in (R.TABLE, R.WORLDVIEW_ITEM) or item not in ws.items():
        return []
    if not ws.has_role(R.TABLE, R.MIDLINE):
        return []
    mine = DS.seen_region(world, item)
    if G.in_midline_band(mine):
        return []
    weight = len(members_of(world, item))
    found = []
    for other in ws.items():
        if other == item or contains(world, item, other) or contains(world, other, item):
            continue
        theirs = DS.seen_region(world, other)
        kind = G.placement(mine, theirs)
        if kind == G.MIRROR:
            found.append((R.MIRROR_IMAGE, (other,), weight, f"{mine.name} against {theirs.name}"))
        elif kind == G.POINT:
            found.append((R.POINT_IMAGE, (other,), weight, f"{mine.name} against {theirs.name}"))
    return found


def type_multiset(world, item: str) -> tuple:
    """The types of an item's objects, sorted (D-092; D-036's factor 8 read as a role)."""
    return tuple(sorted(world.problem.object(m).type for m in members_of(world, item)))


def category_multiset(world, item: str) -> tuple:
    """The nearest ISA categories (D-009, `describe.nearest_category`) of an item's
    objects, sorted, a type with no category standing for itself (D-092): Surround 5's
    rows read as five silverware and one liquid holder each."""
    return tuple(sorted(DS.nearest_category(world, m) or world.problem.object(m).type
                        for m in members_of(world, item)))


def across_rows(world, a: str, b: str) -> bool:
    """Whether two items are seen in the two outer row bands, one in each (D-092, D-086's
    `band_of`): what a comparison role compares, Henry's side against Eliza's."""
    return {DS.band_of(world, a), DS.band_of(world, b)} == {DS.HENRY_ROW, DS.ELIZA_ROW}


def echoes_of(world, item: str) -> list:
    """The ECHO detector (D-092, role A6), a comparison of a group with the other row: for
    a group in view, every group in view across the midline sharing no member whose
    objects have the same multiset of types (by type) or, failing that, the same multiset
    of nearest categories (by category, D-009): "each one echoes, in object type, a
    larger group on the other side of the table" [F95 p. 140], Blockage 6's pairs of
    glasses, Surround 5's two rows; and, for a sameness group (D-024), every object of its
    type across the midline, an object echoing a group's type. Needs a perceived MIDLINE
    (D-084), like the images. Each reading is (ECHO, (partner,), the group's objects, the
    evidence). Nothing for an object, the table or the Worldview item. Consumer: C1,
    group to group (D-095)."""
    ws = world.workspace
    if not S.is_group(world, item) or item not in ws.items() or not ws.has_role(R.TABLE, R.MIDLINE):
        return []
    group = ws.group_named(item)
    types, categories = type_multiset(world, item), category_multiset(world, item)
    found = []
    for other in ws.items():
        if other == item or not across_rows(world, item, other):
            continue
        if S.is_group(world, other):
            if any(m in group.members for m in ws.group_named(other).members):
                continue
            if type_multiset(world, other) == types:
                found.append((R.ECHO, (other,), group.size(), f"{other} echoes {item} by type"))
            elif category_multiset(world, other) == categories:
                found.append((R.ECHO, (other,), group.size(),
                              f"{other} echoes {item} by category: {' '.join(categories)}"))
        elif group.kind == SAMENESS and world.problem.object(other).type == types[0]:
            found.append((R.ECHO, (other,), group.size(), f"{other} echoes {item}'s type"))
    return found


def twin_of(world, item: str) -> list:
    """The TWIN detector (D-092, role A7), read on an object of the touched object's type
    seen across the midline from it: "aside from literal sameness (which is always a
    pressure)" [F95 p. 28]. The tag names the touched object; weight 1; the node is
    `sameness`. Needs a perceived MIDLINE (D-084). Nothing on the touched object itself,
    on a group, or on an object on the touched object's own row. Consumer: C2, the
    re-cast's third tier (D-093)."""
    ws = world.workspace
    touched = world.problem.touched
    if item == touched or item not in world.problem.ids() or item not in ws.items():
        return []
    if not ws.has_role(R.TABLE, R.MIDLINE) or not across_rows(world, item, touched):
        return []
    if world.problem.object(item).type != world.problem.object(touched).type:
        return []
    return [(R.TWIN, (touched,), 1, f"a {world.problem.object(item).type} across from {touched}")]


# The single-table roles C5 does not read (D-085, D-086, D-098): where an item stands, not
# what it is; a stray is of neither side, and a gap is the table's.
C5_LEAVES_OUT = (R.ROW, R.COLUMN, R.CORNER, R.STRAY, R.GAP)


def read_by_c5(role: str) -> bool:
    """Whether consumer C5 reads a role (D-098, D-085, D-086): a role read on one table
    (`roles.SINGLE`) other than the selectors of place (`C5_LEAVES_OUT`): two ends across
    the table are in different rows by construction, the same column is the placement
    slippage's identity (D-082) and two corners its identity or its flip, which a role
    identity would count twice; a stray is of neither side; a gap is tagged on the
    table, never on an end."""
    return R.spec(role).reading == R.SINGLE and role not in C5_LEAVES_OUT


def role_slippages(world, cp) -> list:
    """Consumer C5, the role slippages of a correspondence (D-098): for each role read on
    one table (`roles.SINGLE`: member, sidekick, anchor, connector, and the later single-
    table roles) that both ends carry, an identity such as anchor-anchor, labelled with
    the role's node, which it jolts on entry (D-030) and is parallel through (D-043); and
    for each pair of rival roles (`roles.RIVALS`) one on each end, a slip that feeds
    nothing and earns nothing (member to loner). "The static factors involve, for the most
    part, slippages that 'support' the correspondence" [F95 p. 68]. The selector roles ROW
    and COLUMN are not read here (`read_by_c5`, D-085). A pair of ends with no
    role in common gets nothing: nothing is gated. Draws nothing."""
    ws = world.workspace
    mine = [t.role for t in ws.roles_of(cp.henry) if read_by_c5(t.role)]
    theirs = [t.role for t in ws.roles_of(cp.eliza) if read_by_c5(t.role)]
    found, seen = [], []
    for role in mine:
        if role in theirs and role not in seen:
            seen.append(role)
            found.append(Slippage(ROLE, role.lower(), role.lower(), (R.spec(role).nodes[0],)))
    for role in mine:
        for other in theirs:
            if other in R.rivals_of(role) and (role, other) not in seen:
                seen.append((role, other))
                found.append(Slippage(ROLE, role.lower(), other.lower(), ()))
    found += key_slippages(world, cp)
    return found


def key_slippages(world, cp) -> list:
    """Consumer C5 of TYPE_KEY (D-094, D-098): when a TYPE_KEY tag stands on the Worldview
    item and the correspondence's type slippage (D-030) is between the key's two types,
    either way round, the correspondence repeats the key, "forks and glasses are seen as
    'opposites'" [F95 p. 140], and earns one role identity `key`, labelled with the `key`
    node; the consumption is recorded on the tag. Nothing with no key standing, or a
    correspondence whose types are not the key's. Draws nothing."""
    ta, tb = type_node_of(world, cp.henry), type_node_of(world, cp.eliza)
    if ta is None or tb is None or ta == tb:
        return []
    found = []
    for tag in [t for t in world.workspace.roles_of(R.WORLDVIEW_ITEM) if t.role == R.TYPE_KEY]:
        if set(tag.args) == {ta, tb}:
            R.consumed(world, tag, R.C5, f"{cp.henry}={cp.eliza} repeats the key {ta}-{tb}")
            found.append(Slippage(ROLE, "key", "key", (R.spec(R.TYPE_KEY).nodes[0],)))
            break
    return found


def group_slippages(world, cp) -> list:
    """The slippages between two groups [F95 p. 69] (D-030, D-036 factors 7 and 9): the
    shared ISA categories of two shared-ISA groups (an identity feeds "sameness"), and the
    orientations of two linear groups ("horizontal-horizontal, vertical-vertical, or
    horizontal-vertical slippages" [F95 p. 69]; the flip feeds nothing, D-009 having no
    horizontal-vertical link)."""
    found = []
    ga, gb = world.workspace.group_named(cp.henry), world.workspace.group_named(cp.eliza)
    if ga.kind == SHARED_ISA and gb.kind == SHARED_ISA and ga.category and gb.category:
        labels = ("sameness",) if ga.category == gb.category else ()
        found.append(Slippage(ISA, ga.category, gb.category, labels))
    if ga.linear and gb.linear and ga.orientation and gb.orientation:
        labels = ("sameness",) if ga.orientation == gb.orientation else ()
        found.append(Slippage(ORIENTATION, ga.orientation, gb.orientation, labels))
    return found


def slippages_of(world, cp) -> list:
    """Every slippage supporting a correspondence, in the order of French's factors [F95
    pp. 68-69] (D-030, D-036): the kind slippage (object-object, group-group, group-object
    [F95 p. 68], feeding nothing); the end/middle and position slippages of two objects;
    the placement slippage of a top-level correspondence in the table frame (D-082); the
    type slippage; the count slippage ("one, if it is a simple object; otherwise, the
    size of the group" [F95 p. 68]; an identity feeds "sameness", a slip feeds "number");
    the ISA and orientation slippages of two groups; and the role slippages of the roles
    the two ends share (D-098, consumer C5)."""
    ka, kb = kind_of_end(world, cp.henry), kind_of_end(world, cp.eliza)
    found = [Slippage(KIND, ka, kb)]
    if ka == OBJECT and kb == OBJECT:
        found += position_slippages(world, cp)
    found += placement_slippages(world, cp)         # the table frame, top level only (D-082)
    ta, tb = type_node_of(world, cp.henry), type_node_of(world, cp.eliza)
    if ta is not None and tb is not None:
        found.append(type_slippage(world, ta, tb))
    na = DS.count_descriptor(len(members_of(world, cp.henry)))
    nb = DS.count_descriptor(len(members_of(world, cp.eliza)))
    found.append(Slippage(NUMBER, na, nb, ("sameness",) if na == nb else ("number",)))
    if ka == GROUP and kb == GROUP:
        found += group_slippages(world, cp)
    found += role_slippages(world, cp)              # the roles the two ends share (D-098)
    return found


def slippages_of_kind(cp, kind: str) -> list:
    """The correspondence's slippages of one kind, in order (D-036, D-043)."""
    return [s for s in cp.slippages if s.kind == kind]


# ---------------------------------------------------------------------------------------------
# The static factors (D-036, D-045).


def weight(world, name: str) -> float:
    """One of `Config.cp_static_weights` by name (D-036)."""
    return float(dict(world.config.cp_static_weights)[name])


def salience_points(world, cp) -> float:
    """Factor 1, "Salience of the two objects that make up the correspondence" [F95 p. 68]:
    the weight times the mean salience of the two ends (D-036)."""
    return weight(world, "salience") * (S.salience(world, cp.henry) + S.salience(world, cp.eliza)) / 2


def kind_points(world, cp) -> float:
    """Factor 2, "Simple-object/group-of-objects slippages" [F95 p. 68]: the kind points for
    group-group or object-object, plus the active-node points for group-group "If the
    'group' node is active" and for object-object "if the 'isolated' node is active" [F95
    p. 68] (above `top_down_threshold`); a group-object correspondence loses
    `group_object_penalty` points instead (D-045, FIXED (ANSWERS 2): 15, half the bias
    French says "should probably be relaxed" [F95 p. 125]; 30 under `french-faults`)."""
    ka, kb = kind_of_end(world, cp.henry), kind_of_end(world, cp.eliza)
    if ka != kb:
        return -float(world.config.group_object_penalty)
    node = "group" if ka == GROUP else "isolated"
    points = weight(world, "kind")
    if world.slipnet.activation_of(node) >= world.config.top_down_threshold:
        points += weight(world, "kind_active")
    return points


def end_middle_points(world, cp) -> float:
    """Factor 3, "End/middle location slippages" [F95 p. 68]: the points when an end maps to
    an end or a middle to a middle (an end-middle slippage earns nothing) (D-036)."""
    if any(s.identity for s in slippages_of_kind(cp, END_MIDDLE)):
        return weight(world, "end_middle")
    return 0.0


def symmetry_scale(world, slippage) -> float:
    """How much a position slippage is worth, "The value of this slippage depends on how
    active the 'diagonal symmetry' and 'opposite' nodes are" [F95 p. 68]: the mean
    activation of the slippage's label nodes over 150 (a flip: "opposite" and "diagonal
    symmetry"; an identity: "mirror symmetry" and "sameness") (D-030)."""
    if not slippage.labels:
        return 0.0
    mean = sum(world.slipnet.activation_of(n) for n in slippage.labels) / len(slippage.labels)
    return mean / 150.0


def position_points(world, cp) -> float:
    """Factor 4, "Position slippages" [F95 p. 68]: the position points times the symmetry
    scale, summed over the axes on which both objects carry a position word (D-036) and
    over the placement slippage of a top-level correspondence in the table frame (D-082,
    consumer C6)."""
    return float(sum(weight(world, "position") * symmetry_scale(world, s)
                     for s in slippages_of_kind(cp, POSITION) + slippages_of_kind(cp, PLACEMENT)))


def closeness(world, a: str, b: str) -> float:
    """1 - d / 100 for the Slipnet distance d between two nodes, 0 beyond 100 or with no
    pathway: "how conceptually close the types of the two corresponding objects are in the
    Slipnet" [F95 p. 68] (D-012, D-036)."""
    d = world.slipnet.distance(a, b)
    return max(0.0, 1.0 - d / 100.0)


def type_points(world, cp) -> float:
    """Factor 5, "Same or similar object type" [F95 p. 68]: `type_weight` times the
    closeness of the two type nodes, the weight French raised for his second runs [F95
    pp. 142-143] and the `human` profile's knob (D-036; ANSWERS.md 7); 0 when an end has
    no type (a group that is not a sameness group)."""
    found = slippages_of_kind(cp, TYPE)
    if not found:
        return 0.0
    return world.config.type_weight * closeness(world, found[0].a, found[0].b)


def number_points(world, cp) -> float:
    """Factor 6, "Number-of-objects slippage" [F95 p. 68]: the number points times the
    closeness of the two count nodes, so that "the first slippage ('two' to 'two') would be
    awarded more points than the second ('one' to 'two')" [F95 p. 68] (D-036)."""
    s = slippages_of_kind(cp, NUMBER)[0]
    return weight(world, "number") * closeness(world, s.a, s.b)


def isa_points(world, cp) -> float:
    """Factor 7, "Same or similar ISA description" [F95 p. 69], for two shared-ISA groups:
    the ISA points times the closeness of their categories (D-036)."""
    found = slippages_of_kind(cp, ISA)
    if not found:
        return 0.0
    return weight(world, "isa") * closeness(world, found[0].a, found[0].b)


def same_members_points(world, cp) -> float:
    """Factor 8, "Same-objects slippage" [F95 p. 69]: the points "If the object types in
    both groups making up the correspondence are the same" [F95 p. 69], as multisets
    (D-036); 0 unless both ends are groups."""
    if kind_of_end(world, cp.henry) != GROUP or kind_of_end(world, cp.eliza) != GROUP:
        return 0.0
    types_a = sorted(world.problem.object(m).type for m in members_of(world, cp.henry))
    types_b = sorted(world.problem.object(m).type for m in members_of(world, cp.eliza))
    return weight(world, "same_members") if types_a == types_b else 0.0


def orientation_points(world, cp) -> float:
    """Factor 9, "Orientation slippage" [F95 p. 69], for two linear groups: the orientation
    points for horizontal-horizontal or vertical-vertical, the flip points for
    horizontal-vertical (D-036)."""
    found = slippages_of_kind(cp, ORIENTATION)
    if not found:
        return 0.0
    return weight(world, "orientation" if found[0].identity else "orientation_flip")


def role_points(world, cp) -> float:
    """Factor 10, the role factor, consumer C5 (D-098): the `role` points of
    `cp_static_weights` times the role node's activation over 150, for each role identity
    the correspondence carries (`role_slippages`), the same form as French's position
    slippage, "The value of this slippage depends on how active the 'diagonal symmetry'
    and 'opposite' nodes are" [F95 p. 68] (D-030); a slip earns nothing. 0 with no role
    slippage: nothing is gated."""
    return float(sum(weight(world, "role") * world.slipnet.activation_of(s.labels[0]) / 150.0
                     for s in slippages_of_kind(cp, ROLE) if s.identity and s.labels))


def static_terms(world, cp) -> list:
    """The static factors' points as (name, value) pairs, French's nine in his order [F95
    pp. 68-69] (D-036) and then the tenth, the role factor of consumer C5 (D-098), for the
    trace and the GUI; `static_points` is their sum."""
    return [
        ("salience", salience_points(world, cp)),
        ("kind", kind_points(world, cp)),
        ("end_middle", end_middle_points(world, cp)),
        ("position", position_points(world, cp)),
        ("type", type_points(world, cp)),
        ("number", number_points(world, cp)),
        ("isa", isa_points(world, cp)),
        ("same_members", same_members_points(world, cp)),
        ("orientation", orientation_points(world, cp)),
        ("role", role_points(world, cp)),
    ]


def static_points(world, cp) -> float:
    """"The static evaluation of a correspondence consists of those factors that do not
    depend on the relation of that correspondence to the other correspondences in the"
    Worldview [F95 p. 67]: the sum of the nine (D-036) and the role factor (D-098)."""
    return sum(value for _, value in static_terms(world, cp))


# ---------------------------------------------------------------------------------------------
# The dynamic factors (D-037).


def contains_touched(world, cp) -> bool:
    """Whether the correspondence "contains the touched object" [F95 p. 69], as an end or
    inside a group at an end: then it is "a 'Hobj correspondence' (for Henry's object)"
    [F95 p. 69] (D-037)."""
    touched = world.problem.touched
    return touched in members_of(world, cp.henry) or touched in members_of(world, cp.eliza)


def items_are_neighbours(world, a: str, b: str) -> bool:
    """Whether two items are neighbours (D-027, D-037): an object of one is recorded as a
    neighbour of an object of the other in some group; two groups, or a group and an
    object, through any of their members."""
    for g in world.workspace.groups:
        for m in members_of(world, a):
            if any(n in members_of(world, b) for n, _ in g.neighbours_of(m)):
                return True
    return False


def are_neighbours(world, cp, other) -> bool:
    """"Two correspondences are considered neighbors if the end objects (i.e., the objects
    that the correspondence maps onto one another) of one correspondence are the neighbors
    of the end objects of the second correspondence." [F95 p. 69] Both ends, each with the
    other's end on the same side (D-037)."""
    return (items_are_neighbours(world, cp.henry, other.henry)
            and items_are_neighbours(world, cp.eliza, other.eliza))


def worldview_of(world, exclude=None) -> list:
    """The correspondences in the Worldview, in the order built, leaving out `exclude`
    (D-037: "other correspondences in the Worldview" [F95 p. 69])."""
    return [c for c in world.workspace.correspondences if c.in_worldview and c is not exclude]


def hobj_correspondence(world) -> "Correspondence | None":
    """The Worldview's Hobj correspondence, the one that contains the touched object [F95
    p. 69]; None when the Worldview has none (D-037)."""
    for c in worldview_of(world):
        if contains_touched(world, c):
            return c
    return None


def dynamic_terms(world, cp) -> list:
    """The three dynamic factors [F95 p. 69] as (name, value) pairs (D-037, D-116): the
    Hobj points for containing the touched object, "given a special status" [F95 p. 69],
    in every rating, a Rival Worldview where the correspondence stands alone included
    (D-116, v5: the status is the correspondence's own, not a relation to another); the
    points for being a neighbour of the Worldview's Hobj correspondence, and the points
    for being a neighbour of any other Worldview correspondence, the two relational terms,
    0 while the Worldview holds no other correspondence, since "The dynamic part of the
    evaluation of a correspondence can be done only when there are other correspondences
    in the Worldview." [F95 p. 69]"""
    points = world.config.as_dict("cp_dynamic_points")
    hobj_points = float(points["hobj"]) if contains_touched(world, cp) else 0.0
    others = worldview_of(world, exclude=cp)
    if not others:
        return [("hobj", hobj_points), ("hobj_neighbor", 0.0), ("worldview_neighbor", 0.0)]
    hobj = hobj_correspondence(world)
    near_hobj = hobj is not None and hobj is not cp and are_neighbours(world, cp, hobj)
    near_other = any(are_neighbours(world, cp, c) for c in others if c is not hobj)
    return [
        ("hobj", hobj_points),
        ("hobj_neighbor", float(points["hobj_neighbor"]) if near_hobj else 0.0),
        ("worldview_neighbor", float(points["worldview_neighbor"]) if near_other else 0.0),
    ]


def dynamic_points(world, cp) -> float:
    """"The dynamic evaluation is a rating of how well the correspondence fits into the
    current Worldview." [F95 p. 68] The sum of the three (D-037)."""
    return sum(value for _, value in dynamic_terms(world, cp))


# ---------------------------------------------------------------------------------------------
# The rating, the bonus and the test (D-038, D-039).


def evaluate(world, cp) -> float:
    """Recompute the static and dynamic points and keep them on the correspondence, "A
    correspondence's strength is not fixed but is constantly being re-evaluated" [FCCA
    p. 382]; the bonus is left as accumulated (D-036 to D-038). Returns the rating."""
    cp.static = static_points(world, cp)
    cp.dynamic = dynamic_points(world, cp)
    return cp.rating


def award(world, cp, won: bool, rivalry=()) -> float:
    """The bonus: "awarded each time the correspondence wins a competition to get into (or
    to remain in) the Worldview. Correspondences that lose competitions lose points." [F95
    p. 68] `win_bonus` added or `loss_penalty` taken off, the win or loss counted on the
    correspondence and on its ends for salience (D-038, D-031). Returns the rating.
    Inertia per rivalry (D-117, v5): `rivalry` names the correspondences of the fight (the
    newcomer and its rivals); the bonus or the penalty is awarded once per correspondence,
    outcome and rivalry (`Workspace.awarded`), so that a rematch of the same pair is "a
    little harder" [F95 p. 72] for the loser, one step, and not a growing tally; the win or
    loss is counted every time. With no rivalry given (a direct call) the award is made."""
    key = (cp.name, won, tuple(sorted(rivalry)))
    if not rivalry or key not in world.workspace.awarded:
        if rivalry:
            world.workspace.awarded.append(key)
        if won:
            cp.bonus += world.config.win_bonus
        else:
            cp.bonus -= world.config.loss_penalty
    if won:
        cp.wins += 1
    else:
        cp.losses += 1
    world.workspace.record_competition(cp.ends, won)
    return cp.rating


def describe(world, cp, by: str = "build-cp-descrip") -> list:
    """What a build-cp-descrip codelet does (D-039; the legend's label [F95 p. 96 (img)]):
    rewrite the correspondence's slippages from the ends' descriptors as they now stand,
    attach each as a descriptor of the correspondence (`left-right`, `cup-cup`) and its
    level, and recompute its points. Returns the slippages."""
    cp.slippages = slippages_of(world, cp)
    ws = world.workspace
    ws.describe(cp.name, f"level:{cp.level}")
    for s in cp.slippages:
        if s.kind != KIND:
            ws.describe(cp.name, f"{s.a}-{s.b}")
    evaluate(world, cp)
    return list(cp.slippages)


def test_chance(world, cp) -> float:
    """The chance the test gives a correspondence: its static points over 100, clipped to
    [0.01, 0.99] so that "there is no rigid evaluation threshold above which the
    correspondence passes and below which it fails" [F95 p. 72] (D-039)."""
    return min(CHANCE_CEILING, max(CHANCE_FLOOR, static_points(world, cp) / 100.0))


def test(world, cp, exponent: float) -> bool:
    """The initial test, "the correspondence will be evaluated on the basis of a set of more
    rigorous conditions. Again, the outcome of this evaluation is a probabilistic one"
    [F95 p. 72] (D-039): one coin at `test_chance` under the law of temperature at the
    exponent a(T) the codelet passes (D-060). The outcome is recorded on the
    correspondence; a failed one stays in the Workspace, tentative, to be tested again."""
    cp.chance = test_chance(world, cp)
    cp.passed = TP.passes(cp.chance, exponent, world.rng)
    cp.tested = True
    evaluate(world, cp)
    world.workspace.events.record(
        EV.CorrespondenceTested, correspondence=cp.name, static=cp.static,
        slippages=tuple(s.as_tuple() for s in cp.slippages), chance=cp.chance, passed=cp.passed)
    return cp.passed


def inner_pairs(world, cp) -> list:
    """For an examine-cp codelet: the pairs of members, one from each group of a
    group-group correspondence, between which a correspondence inside it could be created
    [F95 p. 85; F95 p. 108], in member order, those creatable now (D-035, D-044); empty
    unless both ends are groups."""
    if kind_of_end(world, cp.henry) != GROUP or kind_of_end(world, cp.eliza) != GROUP:
        return []
    pairs = []
    for a in members_of(world, cp.henry):
        for b in members_of(world, cp.eliza):
            if creatable(world, a, b)[0]:
                pairs.append((a, b))
    return pairs

"""The Slipnet network's model (P3b; G-033 to G-035), Qt-free: the 63 nodes (D-080) by level in
three bands, Fig. 3.6's style of circles, ISA arrows and labeled links with their label
nodes tied to them by a stem [F95 p. 61 (img)]; the labeled links at their current
effective lengths, "the length of any labeled link varies dynamically as a function of
the activation of the link's label node" [F95 p. 56] (D-011, D-012); the context push
and pop drawn as their events say [F95 pp. 87-89] (D-021, D-070); and the banner "no
context push in this run" while there has been none (ANSWERS.md 11). GUI design, not
French's: the book has no picture of the whole network.

The activations come from `SlipnetGridModel` (G-030), composed here. The effective
lengths come from the engine's own reports (G-033): every labeled link starts at its
length at the floor, the intrinsic length times (1 - `label_shrink` / 150), which is what
the engine computes with every node at 1 (D-011), and `SlipnetUpdated.lengths` sets the
links whose effective length changed, rounded to a tenth (D-019); so the model is exact to
a tenth at every event index and seeking rebuilds it. The context is a `Push` while the
run is inside a subworld (`ContextPushed`: the correspondence entered and the nodes reset
to 1 with their values before) and None at the top level; the last `Pop` keeps the pop's
reinitialized nodes, the spread, the fixed amounts and the restored values (D-021). The
geometry is a pure function of the model, the size and the selection (G-034), as the
grid's is (G-031).
"""

from dataclasses import dataclass

from tabletop import events as ev
from tabletop import slipnet_def as sd
from tabletop.gui.models.slipnet_grid import SlipnetGridModel
from tabletop.gui.models.temperature import NOT_FRENCHS
from tabletop.slipnet import ACTIVATION_MAX, ACTIVATION_MIN, clip

__all__ = ["BANNER_HEIGHT", "CAPTION", "CAPTION_HEIGHT", "Dot", "GROUP_ORDER", "Line",
           "NO_PUSH", "NetworkLayout", "Pop", "Push", "SlipnetNetworkModel", "Stem",
           "dot_at", "floor_length", "line_at", "link_named", "network_layout",
           "nodes_of_level"]

NO_PUSH = "no context push in this run"                   # ANSWERS.md 11
CAPTION = (NOT_FRENCHS + ", after Fig. 3.6 [F95 p. 61 (img)]; push and pop [F95 pp. 87-89]; "
           "violet: the role nodes and links (re-interpretation, D-080)")
# The 29 links the roles add (D-080), by name (G-062).
ROLE_LINK_NAMES = frozenset(link.name for link in sd.ROLE_LINKS)
# The order of the groups within a band, French's list's [F95 p. 62] (G-034).
GROUP_ORDER = ("object", "category", "relation", "structure", "number", "role")   # "role": the role nodes (D-080) after French's groups
LEVEL_ROWS = {3: 0, 2: 1, 1: 2}      # the most abstract level in the top band (G-034)
# The geometry's proportions (G-034): the margin, the banner's and the caption's heights in
# pixels, the band share the outer sub-rows move by, the band size over which a band's
# nodes go in `SUB_ROWS` staggered sub-rows (so that the names fit and the labeled links
# between neighbours are not collinear).
MARGIN, BANNER_HEIGHT, CAPTION_HEIGHT, STAGGER, STAGGER_ABOVE, SUB_ROWS = 6.0, 16.0, 14.0, 0.28, 8, 3
# The object band's order (G-034): French's list's order [F95 p. 62] rearranged so that
# every labeled pair of types (D-009: cup-saucer, saucer-plate, small glass-cup, cup-big
# glass, small glass-big glass, knife-fork, fork-spoon, saltshaker-pepper shaker) sits
# side by side; the other bands keep the definition's order.
BAND_ORDER = {1: ("plate", "saucer", "cup", "small glass", "big glass", "soup bowl", "knife",
                  "fork", "spoon", "saltshaker", "pepper shaker")}
TENTH = 0.05                         # the events round the lengths to a tenth (D-019)
LINK_INDEX = {link.name: link for link in sd.LINKS}


@dataclass(frozen=True)
class Push:
    """One `ContextPushed` (D-021, D-070): the event's `index` and `clock`, the group
    `correspondence` entered and `reset`, (node, activation before) pairs (G-035)."""
    index: int
    clock: int
    correspondence: str
    reset: tuple

    @property
    def nodes(self) -> tuple:
        """The names of the nodes reset to 1 (G-035)."""
        return tuple(name for name, _before in self.reset)


@dataclass(frozen=True)
class Pop:
    """One `ContextPopped` (D-021): the event's `index` and `clock`, the `correspondence`
    left, the nodes `reinitialized`, the `spread` and `fixed` amounts as (node, amount)
    pairs and `restored`, (node, activation after) pairs (G-035)."""
    index: int
    clock: int
    correspondence: str
    reinitialized: tuple
    spread: tuple
    fixed: tuple
    restored: tuple

    @property
    def nodes(self) -> tuple:
        """The names of the nodes restored (G-035)."""
        return tuple(name for name, _after in self.restored)


@dataclass(frozen=True)
class Dot:
    """One node as drawn (G-034): `name`, `label`, `level`, the centre `x`, `y`, the
    `radius`, the `activation` and the dark disc's radius `fill` (linear in the
    activation, as the grid's square, G-031), `label_node` (it labels a link),
    `shortening` (a link it labels is shorter than at the floor), `reset` (reset to 1 by
    the push in force), `restored` (touched by the pop of this codelet), `selected` and
    `role` (one of the 15 role nodes, not on French's screen; D-080, G-062)."""
    name: str
    label: str
    level: int
    x: float
    y: float
    radius: float
    activation: float
    fill: float
    label_node: bool
    shortening: bool
    reset: bool
    restored: bool
    selected: bool
    role: bool = False


@dataclass(frozen=True)
class Line:
    """One link as drawn (G-034): the link's `name`, ends `a` and `b`, `kind`, `labels`,
    the ends' centres, the intrinsic `length`, the `effective` length, the `shrink` (1 -
    effective / length, 0 at the floor's rounding), `shortened`, `selected` (an end is
    the selected node), the `text` beside a labeled link (the effective length to a
    tenth), `loop` for an identity link (a node to itself) and `role` for one of the 29
    links the roles add (D-080, G-062)."""
    name: str
    a: str
    b: str
    kind: str
    labels: tuple
    x1: float
    y1: float
    x2: float
    y2: float
    length: int
    effective: float
    shrink: float
    shortened: bool
    selected: bool
    text: str
    loop: bool
    role: bool = False

    @property
    def midpoint(self) -> tuple:
        """Where the stem of a label node meets the link (G-034)."""
        return ((self.x1 + self.x2) / 2, (self.y1 + self.y2) / 2)


@dataclass(frozen=True)
class Stem:
    """The dashed stem from a label node to the link it controls, as Fig. 3.6 ties n3 to
    the link A-B [F95 p. 61 (img)] (G-034): the `label` node, the `link`, the label's
    centre and the link's midpoint, `active` while the label shortens the link."""
    label: str
    link: str
    x1: float
    y1: float
    x2: float
    y2: float
    active: bool


@dataclass(frozen=True)
class NetworkLayout:
    """The network's geometry for one size (G-034): the `dots`, the `lines`, the `stems`,
    the `banner` along the top (the context, or `NO_PUSH`), the `caption` along the
    bottom, the three bands' middle `band_y` and the `width` and `height` laid out
    for."""
    dots: tuple
    lines: tuple
    stems: tuple
    banner: str
    caption: str
    band_y: tuple
    width: float
    height: float

    def dot(self, name: str):
        """The dot of the node with this name, or None (G-034)."""
        for dot in self.dots:
            if dot.name == name:
                return dot
        return None

    def line(self, name: str):
        """The line of the link with this name, or None (G-034)."""
        for line in self.lines:
            if line.name == name:
                return line
        return None


def link_named(name: str) -> sd.LinkDef:
    """The link of the table by its name, `cup-saucer` (D-009)."""
    return LINK_INDEX[name]


def floor_length(link: sd.LinkDef, shrink: float) -> float:
    """A link's effective length with every node at the floor of 1: the intrinsic length
    times (1 - `label_shrink` x 1 / 150) for a labeled link with labels, else the
    intrinsic length, as the engine computes it before any update, rounded to a tenth as
    the engine's reports are (D-011, D-019; G-033): the engine reports a new length when
    its tenth changes, so a model holding the rounded floor stays within 0.05 of the
    engine's length at every index (not the floor's own rounding more)."""
    if link.kind == sd.LABELED and link.labels and shrink > 0:
        return round(link.length * (1 - shrink * ACTIVATION_MIN / ACTIVATION_MAX), 1)
    return float(link.length)


def nodes_of_level(level: int) -> tuple:
    """The nodes of a level in the band's order: `BAND_ORDER`'s where it names the band,
    else by group in `GROUP_ORDER`, then the definition's order [F95 p. 62] (G-034)."""
    if level in BAND_ORDER:
        return tuple(sd.node_named(name) for name in BAND_ORDER[level])
    return tuple(node for group in GROUP_ORDER for node in sd.NODES
                 if node.level == level and node.group == group)


class SlipnetNetworkModel:
    """The Slipnet's links and context as the events tell them (G-033, G-035): the
    activations through a composed `SlipnetGridModel` (`grid`), `effective` by link
    name, the `context` (a `Push`, or None at the top level), `pushes` and `pops` seen,
    `last_pop`, the `clock`, the `events` taken and `ended`; `on_event` feeds it,
    `layout` gives the geometry."""

    def __init__(self):
        self.grid = SlipnetGridModel()
        self.reset()

    def reset(self) -> None:
        """Forget the run: every link at its floor length, no context (G-033)."""
        self.grid.reset()
        self.effective = {link.name: floor_length(link, self.shrink) for link in sd.LINKS}
        self.context = None
        self.pushes = []
        self.pops = []
        self.last_pop = None
        self.clock = 0
        self.events = 0
        self.ended = False

    @property
    def shrink(self) -> float:
        """The run's `label_shrink` (D-011), the standard's before a run (G-033)."""
        config = self.grid.config
        return config.label_shrink if config is not None else 0.6

    @property
    def activation(self) -> dict:
        """The activations by node name, the grid model's (G-030)."""
        return self.grid.activation

    @property
    def index(self) -> int:
        """The index of the last event taken, -1 before any (G-033)."""
        return self.events - 1

    @property
    def problem(self):
        """The run's problem name, the grid model's (G-030)."""
        return self.grid.problem

    def on_event(self, event) -> None:
        """Take one event: the start, an update's lengths, a push, a pop or the end
        (D-065; G-033, G-035); the activations go to the grid model."""
        if isinstance(event, ev.RunStarted):
            self.reset()
            self.grid.on_event(event)
            self.effective = {link.name: floor_length(link, self.shrink) for link in sd.LINKS}
        else:
            self.grid.on_event(event)
        index = self.events
        self.events += 1
        self.clock = event.clock
        if isinstance(event, ev.SlipnetUpdated):
            for name, length in event.lengths:
                self.effective[name] = float(length)
        elif isinstance(event, ev.ContextPushed):
            push = Push(index, event.clock, event.correspondence, tuple(event.reset))
            self.context = push
            self.pushes.append(push)
        elif isinstance(event, ev.ContextPopped):
            pop = Pop(index, event.clock, event.correspondence, tuple(event.reinitialized),
                      tuple(event.spread), tuple(event.fixed), tuple(event.restored))
            self.context = None
            self.last_pop = pop
            self.pops.append(pop)
        elif isinstance(event, ev.RunEnded):
            self.ended = True

    # ---- reading ------------------------------------------------------------------------

    def shortened(self, link_name: str) -> bool:
        """Whether a link is shorter than at the floor by at least a tenth (G-033)."""
        link = link_named(link_name)
        return self.effective[link_name] < floor_length(link, self.shrink) - TENTH

    def shortens(self, name: str) -> list:
        """The links a label node shortens now, by name: the link is shorter than at the
        floor and this label is its most active one, the one the engine's rule takes
        (D-011; G-033)."""
        return [link.name for link in sd.LINKS
                if name in link.labels and self.shortened(link.name)
                and self.activation[name] >= max(self.activation[l] for l in link.labels)]

    def labels_links(self, name: str) -> list:
        """The links a node labels, by name (D-009)."""
        return [link.name for link in sd.LINKS if name in link.labels]

    def reset_nodes(self) -> frozenset:
        """The nodes reset to 1 by the push in force; empty at the top level (G-035)."""
        return frozenset(self.context.nodes) if self.context is not None else frozenset()

    def restored_nodes(self) -> frozenset:
        """The nodes the last pop touched, while the clock is the pop's: the pop is
        drawn through its codelet (G-035)."""
        pop = self.last_pop
        if pop is None or pop.clock != self.clock or self.context is not None:
            return frozenset()
        return frozenset(pop.nodes)

    def snapshot_context(self):
        """The context in the snapshot's form, `{"correspondence", "reset", "pushed_at"}`
        or None, for the tests against `Snapshot.context` (D-021; G-035)."""
        if self.context is None:
            return None
        return {"correspondence": self.context.correspondence, "reset": self.context.nodes,
                "pushed_at": self.context.clock}

    def banner(self) -> str:
        """The banner along the top: `NO_PUSH` while no push has happened (ANSWERS.md
        11), the subworld entered and the nodes reset while inside one, the last pop
        after it, with the counts (G-035)."""
        if not self.pushes:
            return NO_PUSH
        counts = f"pushes {len(self.pushes)}, pops {len(self.pops)}"
        if self.context is not None:
            push = self.context
            return (f"inside {push.correspondence} since codelet {push.clock}: "
                    f"{len(push.reset)} nodes reset to 1 ({', '.join(push.nodes)}); {counts}")
        pop = self.last_pop
        spread = [amount for _name, amount in pop.spread if amount > 0]
        fixed = sum(amount for _name, amount in pop.fixed)
        return (f"popped {pop.correspondence} at codelet {pop.clock}: "
                f"{len(pop.reinitialized)} reinitialized, spread into {len(spread)}, "
                f"fixed +{fixed:.0f} over {len(pop.fixed)}; {counts}")

    def layout(self, width, height, selected=None) -> NetworkLayout:
        """The network's geometry for a size (G-034)."""
        return network_layout(self, width, height, selected)

    def tooltip(self, name: str) -> str:
        """The hover text of a node (G-033, G-035)."""
        return node_text(self, name)

    def link_tooltip(self, link_name: str) -> str:
        """The hover text of a link (G-033)."""
        return link_text(self, link_name)

    def selection_of(self, name: str):
        """What a click on a node selects: `("node", name, ())` (G-022, G-030)."""
        return ("node", name, ())


# ---- the geometry (G-034) ------------------------------------------------------------------


def _dots(model, width, height, selected) -> tuple:
    """The dots: three bands, the most abstract level on top, each band's nodes spread
    across the width in `nodes_of_level`'s order, in `SUB_ROWS` staggered sub-rows when
    the band holds more than `STAGGER_ABOVE` (G-034)."""
    top = BANNER_HEIGHT + MARGIN
    band = max(1.0, (height - CAPTION_HEIGHT - MARGIN - top) / 3)
    radius = max(3.0, min(9.0, band * 0.16, width / 60))
    reset, restored = model.reset_nodes(), model.restored_nodes()
    dots = []
    for level, row in LEVEL_ROWS.items():
        nodes = nodes_of_level(level)
        pitch = (width - 2 * MARGIN) / len(nodes)
        rows = SUB_ROWS if len(nodes) > STAGGER_ABOVE else 1
        for i, node in enumerate(nodes):
            offset = STAGGER * band * (2 * (i % rows) / (rows - 1) - 1) if rows > 1 else 0.0
            activation = model.activation[node.name]
            share = (clip(activation) - ACTIVATION_MIN) / (ACTIVATION_MAX - ACTIVATION_MIN)
            labels = model.labels_links(node.name)
            dots.append(Dot(node.name, node.label, level, MARGIN + (i + 0.5) * pitch,
                            top + (row + 0.5) * band + offset, radius, activation,
                            radius * share, bool(labels), bool(model.shortens(node.name)),
                            node.name in reset, node.name in restored, node.name == selected,
                            node.name in sd.ROLE_NODES))
    return tuple(dots)


def _lines(model, dots, selected) -> tuple:
    """The lines: one per link of the table between its ends' centres, the identity
    links as loops, the labeled ones with their effective length as text (G-034)."""
    at = {dot.name: dot for dot in dots}
    lines = []
    for link in sd.LINKS:
        a, b = at[link.a], at[link.b]
        effective = model.effective[link.name]
        shrink = max(0.0, 1 - effective / link.length)
        labeled = link.kind == sd.LABELED
        lines.append(Line(link.name, link.a, link.b, link.kind, link.labels, a.x, a.y, b.x, b.y,
                          link.length, effective, shrink, model.shortened(link.name),
                          selected in (link.a, link.b),
                          f"{effective:.1f}" if labeled else "", link.kind == sd.IDENTITY,
                          link.name in ROLE_LINK_NAMES))
    return tuple(lines)


def _stems(model, dots, lines) -> tuple:
    """The stems: one per label of each labeled link, from the label node to the link's
    midpoint, active while the label shortens the link (G-034)."""
    at = {dot.name: dot for dot in dots}
    stems = []
    for line in lines:
        if line.kind != sd.LABELED:
            continue
        mx, my = line.midpoint
        for label in line.labels:
            node = at[label]
            stems.append(Stem(label, line.name, node.x, node.y, mx, my,
                              line.shortened and line.name in model.shortens(label)))
    return tuple(stems)


def network_layout(model, width, height, selected=None) -> NetworkLayout:
    """The network's geometry: the dots by band, the lines, the stems, the banner and the
    caption (G-034). A pure function of the model, the size and the selection."""
    dots = _dots(model, width, height, selected)
    lines = _lines(model, dots, selected)
    top = BANNER_HEIGHT + MARGIN
    band = max(1.0, (height - CAPTION_HEIGHT - MARGIN - top) / 3)
    band_y = tuple(top + (row + 0.5) * band for row in range(3))
    return NetworkLayout(dots, lines, _stems(model, dots, lines), model.banner(), CAPTION,
                         band_y, float(width), float(height))


def dot_at(layout, x, y):
    """The dot under a point (within 1.6 radii, so that a small dot is easy to hit), or
    None (G-034)."""
    best = None
    for dot in layout.dots:
        d2 = (dot.x - x) ** 2 + (dot.y - y) ** 2
        if d2 <= (1.6 * dot.radius) ** 2 and (best is None or d2 < best[0]):
            best = (d2, dot)
    return best[1] if best is not None else None


def line_at(layout, x, y, tolerance=3.0):
    """The labeled or ISA line under a point, within `tolerance` pixels of the segment,
    or None; the loops are not hit (G-034)."""
    best = None
    for line in layout.lines:
        if line.loop:
            continue
        d = _segment_distance(x, y, line.x1, line.y1, line.x2, line.y2)
        if d <= tolerance and (best is None or d < best[0]):
            best = (d, line)
    return best[1] if best is not None else None


def _segment_distance(x, y, x1, y1, x2, y2) -> float:
    """The distance from a point to a segment (G-034)."""
    dx, dy = x2 - x1, y2 - y1
    length2 = dx * dx + dy * dy
    t = 0.0 if length2 == 0 else max(0.0, min(1.0, ((x - x1) * dx + (y - y1) * dy) / length2))
    px, py = x1 + t * dx, y1 + t * dy
    return ((x - px) ** 2 + (y - py) ** 2) ** 0.5


# ---- the hover texts (G-033, G-035) ----------------------------------------------------------


def _context_lines(model, name) -> list:
    """What the push in force or the pop of this codelet did to a node (G-035)."""
    lines = []
    push = model.context
    if push is not None:
        for node, before in push.reset:
            if node == name:
                lines.append(f"reset to 1 by the push of {push.correspondence} at codelet "
                             f"{push.clock} (was {before:.1f})")
    pop = model.last_pop
    if pop is not None and name in model.restored_nodes():
        parts = [f"+{amount:.1f} spread" for node, amount in pop.spread
                 if node == name and amount > 0]
        fixed = [amount for node, amount in pop.fixed if node == name]
        if fixed:
            parts.append(f"+{sum(fixed):.0f} fixed ({len(fixed)} slippage concepts)")
        after = dict(pop.restored)[name]
        lines.append(f"restored to {after:.1f} by the pop of {pop.correspondence} at codelet "
                     f"{pop.clock}" + (f": {', '.join(parts)}" if parts else ""))
    return lines


def node_text(model, name: str) -> str:
    """The hover text of a node: the activation at the event shown, the level and depth
    (D-008), the links it labels and which it shortens now, the push or pop that touched
    it, and every link of the table with its intrinsic and effective length (G-033,
    G-035)."""
    node = sd.node_named(name)
    lines = [f"{name}: activation {model.activation[name]:.1f} at codelet {model.clock}",
             f"level {node.level}, depth {model.grid.depth(name)}"]
    if name in sd.ROLE_NODES:
        lines.append("a role node: not on French's screen (re-interpretation; D-080)")
    labels = model.labels_links(name)
    if labels:
        shortens = model.shortens(name)
        state = f"shortening {', '.join(shortens)}" if shortens else "shortening none"
        lines.append(f"label node of {', '.join(labels)}; {state}")
    lines.extend(_context_lines(model, name))
    links = [link_text(model, link.name) for link in sd.links_of(name)]
    lines.append(f"links ({len(links)}):" if links else "links: none")
    lines.extend("  " + text for text in links)
    return "\n".join(lines)


def link_text(model, link_name: str) -> str:
    """The hover text of a link: its ends, kind and labels, the intrinsic length (D-010)
    and, for a labeled link, the effective length (D-011) with the label's activation
    (G-033)."""
    link = link_named(link_name)
    if link.kind == sd.ISA:
        text = f"{link.a} ISA {link.b}, length {link.length}"
    elif link.kind == sd.IDENTITY:
        text = f"{link.a} to itself [{', '.join(link.labels)}], length {link.length}"
    else:
        label = max(link.labels, key=lambda l: model.activation[l])
        text = (f"{link.a} - {link.b} [{', '.join(link.labels)}], length {link.length}, "
                f"effective {model.effective[link_name]:.1f} ({label} at "
                f"{model.activation[label]:.1f})")
    return text

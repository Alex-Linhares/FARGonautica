"""The platoplasm pane's model, Qt-free (PANES.md P5; G-036, G-037, G-038).

The platoplasm "houses the ideal types" [M86 p. 48]: the ideal atoms with their
manifestations, the glints [M86 pp. 48-49], and the eight non-atomic ideal types in their
hierarchy of categories, "repeater", "generate", "process" and "fence", "from the strictest
down" [M86 pp. 54-55, 59]. Each type keeps the "lists each Platonic concept maintains of
its manifestations and actualizations" [M86 p. 90], the gloms dubbed of it [M86 p. 69] and
the gnoths of its class [M86 p. 77]; the s-links "have direction and 'slipperiness'" [M86 p. 90]; the pull and push
bonds have "a strength from 1 to 10" [M86 p. 91]. The reigning class is "the Platonic class
at the highest level of the (former) hypothesis" [M86 p. 99], and the threshold the
Reformulator's bond strength [M86 p. 105]. The slip-or-stay check compares the staying
pressure with each s-link neighbour's slipping pressure [M86 pp. 103-105]; a strict class
slips at once [M86 p. 99], `Countup --> S-group` [M86 p. 174].

**GUI design, not Meredith's** (ANSWERS.md 4): the thesis has no figure of this network, and
FCCA none; the layout of `layout()` is the GUI's own (G-036), after the Numbo GUI's Pnet
view (`pnet_view.py`, `pnet_model.py`: a fixed grid, pure layout and style functions).

The model is fed like the stats model (G-016): `on_event` on every event, then `update`
with the snapshot in force. The tables (the types, their s-links, pulls, pushes and
strictness) come from the `start` event's config (DECISIONS D-038, D-039, D-040) and are
confirmed by the snapshot's `platoplasm`. The lists and the reigning class are the
snapshot's, the authority at every task boundary, plus what the events since it announce
(G-037): `dubbed` adds a manifestation, `gnoth` and `gnoth-filled` an actualization,
`class-changed` moves every actualization to the new class [M86 pp. 174-175], the removal
events (`burst`, `dissolved`, `blast`, `blasted`, `gnoth-gone`, `gnoths-set`, `operation`)
forget what they name (DECISION D-059); `glint` adds an atom's manifestation [M86 p. 61].
The reigning class comes from `freeze-dried`, `class-changed` and `blasted`, the threshold
from `reformulated` and `operated` (a Gnoth-operator that acted lowered it by one
[M86 p. 124]), cleared where the engine clears it (DECISION D-046). The slip-check card is the last `slip-check` event of the episode, the class change
the last `class-changed`, the scout the last `slip-scouted` [M86 p. 126]; a `freeze-dried`
begins a new episode and clears them. The crown moves along the s-link from the old class to
the new one over `CROWN_MOVE` events, by event counts, not time, so a replay and a seek show
the same thing (G-021's rule).
"""

from dataclasses import dataclass, field, replace

__all__ = ["CROWN_MOVE", "CATEGORIES", "TYPE_TABLE", "NODE_W", "NODE_H", "COL_GAP", "ROW_GAP",
           "HEAD_H", "ATOM_W", "ATOM_H", "ATOM_GAP", "TAG_W", "TAG_H", "LINK_OFFSET", "LABEL_AT",
           "TypeNode", "SLink", "Atom", "SlipCheckCard", "ClassChange", "Scout",
           "NodeShape", "LinkShape", "AtomShape", "TagShape", "HeadingShape", "CrownShape",
           "Layout", "PLine", "PlatoplasmModel", "layout", "crown_progress", "link_width",
           "slipperiness_text", "pressure_text", "pressure_term", "node_tooltip",
           "link_tooltip", "atom_tooltip", "check_lines", "change_text", "scout_text",
           "bond_lines", "pane_lines", "category_of"]

CROWN_MOVE = 6          # events the crown takes to travel an s-link (G-037)

# The hierarchy "from the strictest down" [M86 pp. 54-55, 59], one column each (G-036).
CATEGORIES = ("repeater", "generate", "process", "fence")
# (name, category, format) in the thesis's order [M86 pp. 50-55] (`platoplasm.TYPE_TABLE`;
# copied so that the model needs no engine import at all).
TYPE_TABLE = (
    ("Constant", "repeater", "(Constant arg)"),
    ("Countup", "repeater", "(Countup n)"),
    ("C-group", "generate", "(C-group start length)"),
    ("S-group", "generate", "(S-group start length)"),
    ("P-group", "generate", "(P-group start length)"),
    ("Y-group", "process", "(Y-group [start] [length] actual)"),
    ("Cycle", "fence", "(Cycle actual)"),
    ("Tuple", "fence", "(Tuple actual)"),
)
_CATEGORY_OF = {name: category for name, category, _ in TYPE_TABLE}

# The layout's units (G-036): a node's box, the gaps, the headings, the atoms' cells and
# the tags of the links layer.
NODE_W, NODE_H = 100, 38
COL_GAP, ROW_GAP = 64, 12
HEAD_H = 16
ATOM_W, ATOM_H, ATOM_GAP = 30, 22, 6
TAG_W, TAG_H = 48, 14
LINK_OFFSET = 4.0       # how far a link of a two-way pair sits off the centre line
LABEL_AT = 0.33         # where along a link its label sits, from the source


def category_of(kind: str) -> str:
    """The hierarchy category of a type name [M86 pp. 54-55]."""
    return _CATEGORY_OF[kind]


# ---- the records ------------------------------------------------------------------------------

@dataclass(frozen=True)
class TypeNode:
    """One ideal type as the pane knows it [M86 pp. 49-55, 90-91]: `name`, `category`,
    `format`, `strict` [M86 p. 99], `manifestations` (dubbed gloms' names [M86 p. 69]),
    `actualizations` (gnoths' names [M86 p. 77]), `s_links` as `(neighbour, slipperiness)`
    pairs in the table's order (DECISION D-038), `pulls` and `pushes` as `((type,
    modifier), strength)` pairs (DECISION D-039)."""
    name: str
    category: str
    format: str
    strict: bool = False
    manifestations: tuple = ()
    actualizations: tuple = ()
    s_links: tuple = ()
    pulls: tuple = ()
    pushes: tuple = ()


@dataclass(frozen=True)
class SLink:
    """A directed s-link with its slipperiness, "a number between 0 (non-slippery) and 1
    (perfectly slippery)" [M86 p. 90] (DECISION D-038)."""
    frm: str
    to: str
    slipperiness: float


@dataclass(frozen=True)
class Atom:
    """An ideal atom [M86 p. 48]: its `value` and the names of its `manifestations`, the
    glints of that value [M86 pp. 49, 61]."""
    value: int
    manifestations: tuple = ()


@dataclass(frozen=True)
class SlipCheckCard:
    """One slip-or-stay check [M86 pp. 103-105], the `slip-check` event's fields: the
    `reigning` class, the staying pressure `stayval`, the `best` neighbour (None when none
    scores, `best: nil` [M86 p. 168]) with its `pressure`, every neighbour's pressure in
    `pressures` (pairs), whether the system `slipped`, `made_at` (the model's event count
    at the check) and `factor`, the Slip-scout's when the check was its (DECISION D-049),
    else None."""
    reigning: str
    stayval: float
    best: str | None
    pressure: float
    pressures: tuple
    slipped: bool
    made_at: int
    factor: float | None = None


@dataclass(frozen=True)
class ClassChange:
    """A change of reigning class, the `class-changed` event: `old`, `new`, `reason`
    (`strict` [M86 p. 99] or `slip` [M86 p. 104]) and `made_at` (G-037)."""
    old: str
    new: str
    reason: str
    made_at: int


@dataclass(frozen=True)
class Scout:
    """A Slip-scout's run [M86 p. 126], the `slip-scouted` event: the class reigning
    `before` and `after`, the `factor` and the `stayval` (None for a strict class)
    (DECISION D-049)."""
    before: str
    after: str
    factor: float
    stayval: float | None


# ---- the shapes of the layout (G-036) ------------------------------------------------------------

@dataclass(frozen=True)
class NodeShape:
    """A type's box: its `name`, the box `x`, `y`, `w`, `h`, its `category`, `format`,
    the `counts` text (`2 m / 3 a`: manifestations and actualizations [M86 p. 90]),
    `strict` [M86 p. 99], `reigning` [M86 p. 99], `selected`, and `best` (the best
    pretender of the current check [M86 p. 103])."""
    name: str
    x: float
    y: float
    w: float
    h: float
    category: str
    format: str
    counts: str
    strict: bool
    reigning: bool
    selected: bool
    best: bool


@dataclass(frozen=True)
class LinkShape:
    """An s-link as an arrow [M86 p. 90]: from `frm` to `to`, the stroke from `(x1, y1)`
    to `(x2, y2)` (clipped at the boxes), its `width` by slipperiness, the `label` and
    where it goes (`lx`, `ly`), the `slipperiness`, and, during a check, the `pressure`
    along it [M86 p. 104] (None otherwise), `best` for the best pretender's link and `lit`
    for every link out of the class checked."""
    frm: str
    to: str
    x1: float
    y1: float
    x2: float
    y2: float
    width: float
    label: str
    lx: float
    ly: float
    slipperiness: float
    pressure: float | None = None
    best: bool = False
    lit: bool = False


@dataclass(frozen=True)
class AtomShape:
    """An ideal atom's cell in the strip [M86 pp. 48-49]: `value`, the box, the `count` of
    its manifestations and their `names`."""
    value: int
    x: float
    y: float
    w: float
    h: float
    count: int
    names: tuple


@dataclass(frozen=True)
class TagShape:
    """A tag of the links layer, Ch. 2 Figure 2's manifestation and actualization links
    [M86 p. 45] (G-038): the `owner` type, the structure's `name`, its `kind`
    (`manifestation` or `actualization`), the tag's box and the line from the node,
    `(x1, y1)` to `(x2, y2)`."""
    owner: str
    name: str
    kind: str
    x: float
    y: float
    w: float
    h: float
    x1: float
    y1: float
    x2: float
    y2: float


@dataclass(frozen=True)
class HeadingShape:
    """A category's heading over its column [M86 pp. 54-55] (G-036)."""
    text: str
    x: float
    y: float
    w: float
    h: float


@dataclass(frozen=True)
class CrownShape:
    """Where the crown is [M86 p. 99]: the point `(x, y)` over the reigning class's box,
    or, while it travels, between the `frm` class's and the `to` class's at `progress`
    (0 to 1) (G-037)."""
    x: float
    y: float
    frm: str | None
    to: str
    progress: float


@dataclass(frozen=True)
class Layout:
    """Everything the network widget paints (G-036): `headings`, `nodes`, `links`,
    `atoms`, `tags`, the `crown` (or None) and the extent `width` x `height`."""
    headings: tuple
    nodes: tuple
    links: tuple
    atoms: tuple
    tags: tuple
    crown: CrownShape | None
    width: float
    height: float


@dataclass(frozen=True)
class PLine:
    """One line the pane's list paints (G-038): `kind` (`head`, `check`, `pressure`,
    `change`, `scout`, `bond`, `note`), `text`, `tooltip`, `name` (a neighbour's, for a
    pressure line), `best`, `grey`."""
    kind: str
    text: str
    tooltip: str = ""
    name: str | None = None
    best: bool = False
    grey: bool = False


# ---- the model -----------------------------------------------------------------------------------

class PlatoplasmModel:
    """The pane's state (see the module docstring; G-037): `types` (name -> `TypeNode`
    in the thesis's order), `atoms` (value -> `Atom`), `reigning`, `threshold`, `check`
    (the episode's last `SlipCheckCard` or None), `change` (the last `ClassChange` or
    None), `scout`, `selected` (a type's name or None), `show_links` (the links layer,
    off by default), `events` (this run's count), `corrections` (snapshots that disagreed
    with the event-built state), and the shared `selection` (G-025)."""

    def __init__(self, selection=None):
        self.selection = selection
        self.show_links = False
        self.reset()

    def reset(self) -> None:
        """The state before any run: the eight types with no tables and no lists
        [M86 pp. 50-55] (G-037)."""
        self.types = {name: TypeNode(name, cat, fmt) for name, cat, fmt in TYPE_TABLE}
        self.atoms = {}
        self.reigning = None
        self.threshold = None
        self.check = None
        self.change = None
        self.scout = None
        self.selected = None
        self.events = 0
        self.corrections = 0
        self._snapshot = None

    # -- reading -------------------------------------------------------------------------------

    def links(self) -> tuple:
        """Every s-link as an `SLink`, by type in the thesis's order and, within a type,
        in the table's order (DECISION D-038)."""
        return tuple(SLink(t.name, to, s) for t in self.types.values() for to, s in t.s_links)

    def link_triples(self) -> tuple:
        """The s-links as `(from, to, slipperiness)` triples, `Config.s_links`'s shape
        (DECISION D-038)."""
        return tuple((l.frm, l.to, l.slipperiness) for l in self.links())

    def slipperiness(self, frm: str, to: str) -> float:
        """The slipperiness from one class to another, 0 where there is no link
        [M86 p. 90] (DECISION D-038)."""
        return dict(self.types[frm].s_links).get(to, 0.0)

    def crown_progress(self) -> float:
        """How far the crown has travelled, 0 at the `class-changed` event to 1 after
        `CROWN_MOVE` events (G-037)."""
        return crown_progress(self.change, self.events)

    def check_current(self) -> bool:
        """Whether the episode's check is the one to show on the links: there is one and
        the class it checked still exists in the tables (G-037)."""
        return self.check is not None and self.check.reigning in self.types

    def pressures(self) -> dict:
        """The current check's pressures by neighbour, `{}` without a check [M86 p. 104]."""
        if not self.check_current():
            return {}
        return {to: float(p) for to, p in self.check.pressures}

    def select(self, name: str | None) -> tuple:
        """Select a type: its manifestations and actualizations go into the shared
        selection, so the cytoplasm and the socratoplasm show them (PANES.md P5: "Click a
        node to list its manifestations and actualizations and select them in P2/P3";
        G-025, G-038); a second click, or None, clears; returns the names."""
        if name is None or name not in self.types:
            self.selected = None
            if self.selection is not None:
                self.selection.clear()
            return ()
        node = self.types[name]
        members = tuple(node.manifestations) + tuple(node.actualizations)
        if self.selected == name:
            self.selected = None
            if self.selection is not None:
                self.selection.clear()
            return ()
        self.selected = name
        if self.selection is not None:
            self.selection.select("class", name, members)
        return members

    def is_selected(self, name: str) -> bool:
        """Whether the type is the one selected, or in the shared selection (G-025)."""
        if self.selected == name:
            return True
        return self.selection is not None and self.selection.includes(name)

    def set_show_links(self, on: bool) -> None:
        """Switch the manifestation and actualization links layer [M86 p. 45] (G-038)."""
        self.show_links = bool(on)

    # -- the events ----------------------------------------------------------------------------

    def on_event(self, event) -> None:
        """Apply what the event announces (see the module docstring; G-037)."""
        kind = event.kind
        if kind == "start":
            self.reset()
            self._read_config(event.config)
        self.events += 1
        handler = self._HANDLERS.get(kind)
        if handler is not None:
            handler(self, event)

    def _read_config(self, config) -> None:
        """The tables from the `start` event's config pairs (DECISIONS D-038, D-039,
        D-040); a value may be a tuple or, from a session file, a list."""
        values = {name: value for name, value in config}
        links = {name: [] for name in self.types}
        for frm, to, slipperiness in values.get("s_links", ()) or ():
            if frm in links and to in self.types:
                links[frm].append((to, float(slipperiness)))
        pulls = {name: [] for name in self.types}
        pushes = {name: [] for name in self.types}
        for kind, type_, modifier, value in values.get("bond_values", ()) or ():
            if kind not in self.types:
                continue
            value = int(value)
            (pulls if value > 0 else pushes)[kind].append(((type_, modifier), value))
        strict = set(values.get("strict_classes", ()) or ())
        for name, node in self.types.items():
            self.types[name] = replace(node, s_links=tuple(links[name]),
                                       pulls=tuple(pulls[name]), pushes=tuple(pushes[name]),
                                       strict=name in strict)

    def _on_glint(self, event) -> None:
        atom = self.atoms.get(event.value) or Atom(event.value)
        if event.name not in atom.manifestations:
            self.atoms[event.value] = replace(
                atom, manifestations=atom.manifestations + (event.name,))

    def _on_dubbed(self, event) -> None:
        # a glom dubbed again under another class stays listed under the first too:
        # the engine's `manifest` appends and never removes [M86 p. 69]
        self._manifest(event.kind_name, event.name)

    def _manifest(self, kind, name) -> None:
        node = self.types.get(kind)
        if node is not None and name not in node.manifestations:
            self.types[kind] = replace(node, manifestations=node.manifestations + (name,))

    def _actualize(self, kind, name) -> None:
        self._forget(name)
        node = self.types.get(kind)
        if node is not None and name not in node.actualizations:
            self.types[kind] = replace(node, actualizations=node.actualizations + (name,))

    def _forget(self, *names) -> None:
        """Drop structures from every list, as the engine's `forget` does (DECISION
        D-059)."""
        gone = set(names)
        for kind, node in self.types.items():
            if gone & (set(node.manifestations) | set(node.actualizations)):
                self.types[kind] = replace(
                    node,
                    manifestations=tuple(m for m in node.manifestations if m not in gone),
                    actualizations=tuple(a for a in node.actualizations if a not in gone))

    def _on_gnoth(self, event) -> None:
        self._actualize(event.plato_class, event.name)

    def _on_gnoth_filled(self, event) -> None:
        if self.reigning is not None:
            self._actualize(self.reigning, event.name)

    def _on_gnoths_set(self, event) -> None:
        keep = set(event.names)
        gone = [a for node in self.types.values() for a in node.actualizations if a not in keep]
        self._forget(*gone)

    def _on_gnoth_gone(self, event) -> None:
        self._forget(event.name)

    def _on_operation(self, event) -> None:
        self._forget(*getattr(event, "gone", ()))

    def _on_burst(self, event) -> None:
        self._forget(event.name)

    def _on_dissolved(self, event) -> None:
        self._forget(event.name)

    def _on_blast(self, event) -> None:
        self._forget(*event.burst)

    def _on_blasted(self, event) -> None:
        self._forget(*event.gnoths, *event.gloms)
        self.reigning = None
        self.threshold = None
        self.check = None
        self.change = None
        self.scout = None

    def _on_freeze_dried(self, event) -> None:
        self.reigning = event.reigning
        self.threshold = None
        self.check = None
        self.change = None
        self.scout = None

    def _on_slip_check(self, event) -> None:
        self.check = SlipCheckCard(event.reigning, float(event.stayval), event.best,
                                   float(event.pressure),
                                   tuple((to, float(p)) for to, p in event.pressures),
                                   bool(event.slipped), self.events)

    def _on_class_changed(self, event) -> None:
        self.change = ClassChange(event.old, event.new, event.reason, self.events)
        self.reigning = event.new
        self.threshold = None
        moved = [a for node in self.types.values() for a in node.actualizations]
        for name in moved:
            self._actualize(event.new, name)

    def _on_slip_scouted(self, event) -> None:
        self.scout = Scout(event.before, event.after, float(event.factor),
                           None if event.stayval is None else float(event.stayval))
        if self.check is not None and event.stayval is not None:
            self.check = replace(self.check, factor=float(event.factor))
        self.reigning = event.after

    def _on_reformulated(self, event) -> None:
        self.reigning = event.kind_
        self.threshold = float(event.threshold)

    def _on_bonds_assessed(self, event) -> None:
        # the event's threshold is the one the task was given, not the world's
        self.reigning = event.kind_

    def _on_operated(self, event) -> None:
        if event.threshold is not None:
            self.threshold = float(event.threshold)

    _HANDLERS = {
        "glint": _on_glint, "dubbed": _on_dubbed, "gnoth": _on_gnoth,
        "gnoth-filled": _on_gnoth_filled, "gnoths-set": _on_gnoths_set,
        "gnoth-gone": _on_gnoth_gone, "operation": _on_operation, "burst": _on_burst,
        "dissolved": _on_dissolved, "blast": _on_blast, "blasted": _on_blasted,
        "freeze-dried": _on_freeze_dried, "slip-check": _on_slip_check,
        "class-changed": _on_class_changed, "slip-scouted": _on_slip_scouted,
        "reformulated": _on_reformulated, "bonds-assessed": _on_bonds_assessed,
        "operated": _on_operated,
    }

    # -- the snapshot --------------------------------------------------------------------------

    def update(self, snapshot) -> None:
        """Take the snapshot in force: its tables, lists, atoms, reigning class and
        threshold are the authority; a snapshot new to the model was taken at the event
        just given (G-033's rule), one already taken is left alone. Where it disagrees
        with the event-built state, `corrections` counts it (G-037)."""
        if snapshot is None or snapshot is self._snapshot:
            return
        self._snapshot = snapshot
        plato = snapshot.platoplasm
        for record in plato.types:
            node = self.types.get(record.name)
            if node is None:
                continue
            new = replace(node, strict=bool(record.strict),
                          manifestations=tuple(record.manifestations),
                          actualizations=tuple(record.actualizations),
                          s_links=tuple((to, float(s)) for to, s in record.s_links),
                          pulls=tuple((tuple(k), int(v)) for k, v in record.pulls),
                          pushes=tuple((tuple(k), int(v)) for k, v in record.pushes))
            if (set(new.manifestations) != set(node.manifestations)
                    or set(new.actualizations) != set(node.actualizations)):
                self.corrections += 1
            self.types[record.name] = new
        atoms = {a.value: Atom(a.value, tuple(a.manifestations)) for a in plato.atoms}
        if {v: set(a.manifestations) for v, a in atoms.items()} != {
                v: set(a.manifestations) for v, a in self.atoms.items()}:
            self.corrections += 1
        self.atoms = atoms
        reform = snapshot.reformulation
        threshold = None if reform.threshold is None else float(reform.threshold)
        if reform.reigning != self.reigning or threshold != self.threshold:
            self.corrections += 1
        self.reigning = reform.reigning
        self.threshold = threshold


# ---- the layout (G-036) ----------------------------------------------------------------------------

def crown_progress(change, events: int) -> float:
    """How far the crown has travelled after a change, 0 at its event to 1 after
    `CROWN_MOVE` events; 1 with no change (G-037)."""
    if change is None:
        return 1.0
    return max(0.0, min(1.0, (events - change.made_at) / CROWN_MOVE))


def link_width(slipperiness: float) -> float:
    """The stroke width of an s-link, 0.6 to 3.0 by slipperiness [M86 p. 90] (G-036)."""
    return round(0.6 + 2.4 * max(0.0, min(1.0, slipperiness)), 2)


def slipperiness_text(slipperiness: float) -> str:
    """`0.4`, as the thesis writes it [M86 p. 90]."""
    return f"{slipperiness:g}"


def pressure_text(pressure: float) -> str:
    """`4.0`, as the transcript prints a pressure [M86 p. 176]."""
    return f"{pressure:.1f}"


def pressure_term(slipperiness: float, pressure: float) -> str:
    """The pressure as p. 104's product: `0.4 * 10.0 = 4.0`, the slipperiness times the
    bond sum in the neighbour's environment [M86 p. 104]; the sum is the pressure over
    the slipperiness (the event carries the product only); `0 * ... = 0.0` for a link of
    slipperiness 0."""
    if slipperiness <= 0:
        return f"0 * ... = {pressure_text(pressure)}"
    total = pressure / slipperiness
    return f"{slipperiness_text(slipperiness)} * {total:.1f} = {pressure_text(pressure)}"


def _columns(model) -> dict:
    """The nodes' boxes by name, one column per category, the strictest on the left
    [M86 pp. 54-55, 59]; the column's nodes stacked in the thesis's order (G-036)."""
    boxes = {}
    for col, cat in enumerate(CATEGORIES):
        names = [n for n in model.types if model.types[n].category == cat]
        x = col * (NODE_W + COL_GAP + (TAG_W + 6 if model.show_links else 0))
        for row, name in enumerate(names):
            boxes[name] = (x, HEAD_H + 4 + row * (NODE_H + ROW_GAP), NODE_W, NODE_H)
    return boxes


def _centre(box) -> tuple:
    x, y, w, h = box
    return (x + w / 2, y + h / 2)


def _edge_point(box, toward, offset=(0.0, 0.0)) -> tuple:
    """Where the line from the box's centre to `toward` leaves the box, shifted by
    `offset` (G-036)."""
    x, y, w, h = box
    cx, cy = x + w / 2 + offset[0], y + h / 2 + offset[1]
    dx, dy = toward[0] - cx, toward[1] - cy
    if dx == 0 and dy == 0:
        return (cx, cy)
    tx = (w / 2) / abs(dx) if dx else float("inf")
    ty = (h / 2) / abs(dy) if dy else float("inf")
    t = min(tx, ty)
    return (cx + dx * t, cy + dy * t)


def _link_shapes(model, boxes) -> tuple:
    """The s-links as arrows between the boxes, a two-way pair offset to each side of
    the centre line, labelled with the slipperiness and, during a check, the pressure
    [M86 pp. 90, 104] (G-036)."""
    links = model.links()
    pairs = {(l.frm, l.to) for l in links}
    pressures = model.pressures()
    check = model.check if model.check_current() else None
    out = []
    for link in links:
        a, b = boxes[link.frm], boxes[link.to]
        ca, cb = _centre(a), _centre(b)
        dx, dy = cb[0] - ca[0], cb[1] - ca[1]
        length = (dx * dx + dy * dy) ** 0.5 or 1.0
        offset = (0.0, 0.0)
        if (link.to, link.frm) in pairs:
            # to the right of the direction of travel, so the pair never overlaps
            offset = (-dy / length * LINK_OFFSET, dx / length * LINK_OFFSET)
        cb_off = (cb[0] + offset[0], cb[1] + offset[1])
        ca_off = (ca[0] + offset[0], ca[1] + offset[1])
        x1, y1 = _edge_point(a, cb_off, offset)
        x2, y2 = _edge_point(b, ca_off, offset)
        pressure = None
        lit = best = False
        if check is not None and link.frm == check.reigning:
            lit = True
            pressure = pressures.get(link.to)
            best = link.to == check.best
        label = slipperiness_text(link.slipperiness)
        if pressure is not None:
            label = f"{label} → {pressure_text(pressure)}"
        # the label sits a third of the way from the source, to the side of the offset,
        # so that the two labels of a two-way pair stay near their own source
        mx, my = x1 + (x2 - x1) * LABEL_AT, y1 + (y2 - y1) * LABEL_AT
        lx = mx + (-dy / length) * 6 + offset[0]
        ly = my + (dx / length) * 6 + offset[1]
        out.append(LinkShape(link.frm, link.to, x1, y1, x2, y2, link_width(link.slipperiness),
                             label, lx, ly, link.slipperiness, pressure, best, lit))
    return tuple(out)


def _tag_shapes(model, boxes) -> tuple:
    """The links layer's tags: each node's manifestations and actualizations beside it,
    joined by a line, the first few per node and `+n` for the rest [M86 p. 45] (G-038)."""
    out = []
    per_node = max(1, NODE_H // TAG_H)
    for name, node in model.types.items():
        items = ([(m, "manifestation") for m in node.manifestations]
                 + [(a, "actualization") for a in node.actualizations])
        if not items:
            continue
        x, y, w, h = boxes[name]
        shown = items[:per_node]
        if len(items) > per_node:
            shown = items[:per_node - 1] + [(f"+{len(items) - per_node + 1}", "more")]
        for i, (label, kind) in enumerate(shown):
            tx, ty = x + w + 6, y + i * TAG_H + (h - len(shown) * TAG_H) / 2
            out.append(TagShape(name, label, kind, tx, ty, TAG_W, TAG_H - 2,
                                x + w, y + h / 2, tx, ty + (TAG_H - 2) / 2))
    return tuple(out)


def layout(model) -> Layout:
    """The whole picture, a pure function of the model (G-036): the four category
    headings over their columns, the eight nodes, the s-link arrows, the ideal atoms'
    strip under the network [M86 pp. 48-49], the links layer's tags when it is on, and
    the crown over the reigning class or on its way along the s-link [M86 pp. 99, 174]."""
    boxes = _columns(model)
    col_w = NODE_W + COL_GAP + (TAG_W + 6 if model.show_links else 0)
    headings = tuple(HeadingShape(cat, i * col_w, 0, NODE_W, HEAD_H)
                     for i, cat in enumerate(CATEGORIES))
    nodes = []
    check = model.check if model.check_current() else None
    for name, node in model.types.items():
        x, y, w, h = boxes[name]
        counts = f"{len(node.manifestations)} m / {len(node.actualizations)} a"
        nodes.append(NodeShape(name, x, y, w, h, node.category, node.format, counts,
                               node.strict, name == model.reigning, model.is_selected(name),
                               check is not None and name == check.best))
    links = _link_shapes(model, boxes)
    bottom = max(y + h for x, y, w, h in boxes.values())
    strip_y = bottom + 14
    atoms = []
    for i, value in enumerate(sorted(model.atoms)):
        atom = model.atoms[value]
        atoms.append(AtomShape(value, i * (ATOM_W + ATOM_GAP), strip_y, ATOM_W, ATOM_H,
                               len(atom.manifestations), tuple(atom.manifestations)))
    tags = _tag_shapes(model, boxes) if model.show_links else ()
    crown = None
    if model.reigning is not None and model.reigning in boxes:
        x, y, w, h = boxes[model.reigning]
        tx, ty = x + w - 10, y - 2
        progress = model.crown_progress()
        change = model.change
        if change is not None and progress < 1.0 and change.old in boxes:
            ox, oy, ow, oh = boxes[change.old]
            fx, fy = ox + ow - 10, oy - 2
            crown = CrownShape(fx + (tx - fx) * progress, fy + (ty - fy) * progress,
                               change.old, model.reigning, progress)
        else:
            crown = CrownShape(tx, ty, None, model.reigning, 1.0)
    width = max([x + w for x, y, w, h in boxes.values()]
                + [a.x + a.w for a in atoms] + [t.x + t.w for t in tags])
    height = strip_y + ATOM_H
    return Layout(headings, tuple(nodes), links, tuple(atoms), tags, crown, width, height)


# ---- the texts (G-038) ------------------------------------------------------------------------------

def _bond_text(key, value) -> str:
    """`Succ adjacent +10`, a bond type in a class's environment [M86 pp. 91, 101]."""
    type_, modifier = key
    return f"{type_} {modifier} {value:+d}"


def node_tooltip(node: TypeNode, reigning: str | None = None) -> str:
    """A type's tooltip: its category and format [M86 pp. 50-55], strictness
    [M86 p. 99], the manifestations and actualizations by name [M86 p. 90], its pull and
    push bonds [M86 p. 91] and its s-links [M86 p. 90]."""
    lines = [f"{node.name}  ({node.category} type)  [M86 pp. 50-55]",
             f"format: {node.format}",
             f"strict: {'yes' if node.strict else 'no'}  [M86 p. 99]"]
    if reigning == node.name:
        lines.append("the reigning class  [M86 p. 99]")
    m = ", ".join(node.manifestations) or "none"
    a = ", ".join(node.actualizations) or "none"
    lines.append(f"manifestations ({len(node.manifestations)}): {m}  [M86 p. 69]")
    lines.append(f"actualizations ({len(node.actualizations)}): {a}  [M86 p. 77]")
    pulls = ", ".join(_bond_text(k, v) for k, v in node.pulls) or "none"
    pushes = ", ".join(_bond_text(k, v) for k, v in node.pushes) or "none"
    lines.append(f"pull-bonds: {pulls}  [M86 p. 91]")
    lines.append(f"push-bonds: {pushes}")
    links = ", ".join(f"{to} {slipperiness_text(s)}" for to, s in node.s_links) or "none"
    lines.append(f"s-links: {links}  [M86 p. 90]")
    return "\n".join(lines)


def link_tooltip(link: LinkShape) -> str:
    """An s-link's tooltip: its direction and slipperiness [M86 p. 90] and, during a
    check, its pressure as p. 104's product [M86 p. 104]."""
    text = (f"s-link {link.frm} --> {link.to}  slipperiness "
            f"{slipperiness_text(link.slipperiness)}  [M86 p. 90]")
    if link.pressure is not None:
        text += (f"\nslipping pressure: {pressure_term(link.slipperiness, link.pressure)}"
                 f"  [M86 p. 104]")
        if link.best:
            text += "\nthe best pretender to the throne  [M86 p. 103]"
    return text


def atom_tooltip(atom: Atom) -> str:
    """An ideal atom's tooltip: its name, predecessor and successor and its
    manifestations, the glints of its value [M86 pp. 48-49]."""
    pred = "none" if atom.value == 0 else f"ideal{atom.value - 1}"
    names = ", ".join(atom.manifestations) or "none"
    return (f"ideal{atom.value}: predecessor {pred}, successor ideal{atom.value + 1}"
            f"  [M86 pp. 48-49]\nmanifestations ({len(atom.manifestations)}): {names}")


def check_lines(check: SlipCheckCard, slipperiness) -> tuple:
    """The slip-check as the transcript prints it [M86 pp. 168, 176] and as pp. 103-105
    reckon it: the staying pressure, each neighbour's slipping pressure, the best and the
    outcome [M86 pp. 103-105]. `slipperiness(frm, to)` gives a link's (G-038)."""
    lines = [PLine("check", f"slip-check: stayval: {pressure_text(check.stayval)}",
                   "the staying pressure, the sum of the gnoth stabilities in the "
                   "reigning class's environment [M86 pp. 101-103] (DECISION D-041)"
                   + (f"; the Slip-scout's factor {check.factor:g} applied "
                      f"[M86 p. 126]" if check.factor is not None else ""))]
    for to, pressure in check.pressures:
        s = slipperiness(check.reigning, to)
        lines.append(PLine("pressure",
                           f"{to}: {pressure_text(pressure)}  ({pressure_term(s, pressure)})",
                           f"the slipping pressure from {check.reigning} to {to}: the "
                           f"bond sum in {to}'s environment times the s-link's "
                           f"slipperiness [M86 p. 104]", name=to, best=to == check.best))
    best = "nil" if check.best is None else f"({check.best} {pressure_text(check.pressure)})"
    lines.append(PLine("check", f"        best: {best}",
                       "the largest slipping pressure [M86 pp. 104, 176]"))
    if check.slipped:
        lines.append(PLine("change", f"slip: {check.reigning} --> {check.best}",
                           "the best beats the staying pressure [M86 p. 104]"))
    else:
        lines.append(PLine("check", f"We will stay with the reigning class -- {check.reigning}.",
                           "no neighbour beats the staying pressure [M86 pp. 104-105, 176]"))
    return tuple(lines)


def change_text(change: ClassChange) -> str:
    """`Changing to a new reigning class. Countup --> S-group`, with the reason
    [M86 pp. 99, 174] (DECISION D-042)."""
    reason = ("strict: slipping at once" if change.reason == "strict"
              else "a neighbour beat the staying pressure")
    return f"Changing to a new reigning class. {change.old} --> {change.new}  ({reason})"


def scout_text(scout: Scout) -> str:
    """A Slip-scout's line [M86 p. 126] (DECISION D-049)."""
    stay = "strict" if scout.stayval is None else f"stayval {pressure_text(scout.stayval)}"
    return f"Slip-scout: x{scout.factor:g}, {stay}: {scout.before} --> {scout.after}"


def bond_lines(node: TypeNode) -> tuple:
    """A class's pull and push bonds as lines, "a strength from 1 to 10" [M86 p. 91]."""
    out = [PLine("bond", f"pull  {_bond_text(k, v)}", "a bond the class uses to hold groups "
                 "together [M86 p. 91]") for k, v in node.pulls]
    out += [PLine("bond", f"push  {_bond_text(k, v)}", "a bond the class uses to separate "
                  "groups [M86 p. 91]") for k, v in node.pushes]
    return tuple(out) or (PLine("note", "no pull or push bonds", grey=True),)


def pane_lines(model: PlatoplasmModel) -> tuple:
    """The pane's list (G-038): the reigning class and the threshold [M86 pp. 99, 105],
    the last class change [M86 p. 174], the slip-check [M86 pp. 103-105], the scout
    [M86 p. 126], and the reigning class's bonds [M86 p. 91]."""
    lines = []
    if model.reigning is None:
        lines.append(PLine("head", "no reigning class yet",
                           "the reigning class is set when a hypothesis is freeze-dried "
                           "[M86 p. 99]", grey=True))
    else:
        threshold = ("none" if model.threshold is None
                     else pressure_text(model.threshold))
        lines.append(PLine("head", f"reigning class: {model.reigning}   threshold {threshold}",
                           "the Platonic class at the highest level of the former hypothesis "
                           "[M86 p. 99]; the Reformulator's threshold bond strength "
                           "[M86 p. 105]", name=model.reigning))
    if model.change is not None:
        lines.append(PLine("change", change_text(model.change),
                           "[M86 pp. 99, 174] (DECISION D-042)", name=model.change.new))
    if model.check is not None:
        lines.append(PLine("head", "slip-check", "[M86 pp. 103-105]"))
        lines.extend(check_lines(model.check, model.slipperiness))
    if model.scout is not None:
        lines.append(PLine("scout", scout_text(model.scout), "[M86 p. 126]"))
    if model.reigning is not None and model.reigning in model.types:
        lines.append(PLine("head", f"{model.reigning}'s bonds", "[M86 p. 91]"))
        lines.extend(bond_lines(model.types[model.reigning]))
    return tuple(lines)

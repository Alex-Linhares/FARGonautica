"""The Pnet model (not 1987 source; loop0003).

The Pnet's structure is fixed: the 88 pnodes of *pnet* (pnet_def.py), each
linked to its neighbors by a link type, itself a pnode (RESULT+, OPERAND,
...).  `pnet_structure()` reads it from pnet_def's data, and
`structure_from_world` from a World, before or after initialize-pnet-2
replaced the neighbor symbols by pnodes; the two are equal.

Its state is each pnode's activation and instances (the cyto-nodes linked
to it, ((type, cyto-node name), ...)), which a `PnetModel` keeps from a
run's events (an observer): pnet-initialized (config's initialize-pnet),
pnet (all 88 after a spread) and pnodes-changed (a pnode's own message, or
repump's set-up-activations).  `PnetModel.from_world` reads it from a
World.  Activations are as the events have them (events.encode_data).

Node kinds: "number" (type NUM), "plus" and "times" (PLUSa-b, TIMESa-b:
a sum or a product, linked to its operands and its result), "operation"
(ADD, SUBTRACTION, MULTIPLY), "link-type" (the six pnodes that are link
types) and "concept" (MULTIPLE-OF-TEN).

`pnet_layout()` is a deterministic grid layout, computed once and cached:
the numbers in a row by value; above each number the sums that make it,
below it the products (a sum or a product sits in its result's column);
then, in a band below, the operations, the concept and the link types.
"""

import dataclasses
import functools
import re

from numbo import events, observe, pnet_def
from numbo.franz import Symbol
from numbo.franz import intern as _S

_PNET = _S("*PNET*")


class PnetModelError(Exception):
    """An event that does not fit the model's Pnet."""


@dataclasses.dataclass(frozen=True)
class PnodeSpec:
    """A pnode of *pnet*: NAME, the HOLDER symbol's name (NODE-1, ...),
    KIND, VALUE (a number's), SHORT_NAME (for graphics) and its NEIGHBORS
    ((pnode name, link type name), ...) in source order."""
    name: str
    holder: str
    kind: str
    value: object
    short_name: object
    neighbors: tuple


@dataclasses.dataclass(frozen=True)
class PnetLink:
    """A neighbor link from SOURCE to TARGET of link type TYPE (pnode
    names)."""
    source: str
    target: str
    type: str


_COMBINATION = re.compile(r"(PLUS|TIMES)[0-9]+-[0-9]+")


@dataclasses.dataclass(frozen=True)
class PnetStructure:
    """The 88 pnodes (PnodeSpecs, in *pnet*'s order) and their links."""
    nodes: tuple
    links: tuple

    @functools.cached_property
    def names(self):
        return tuple(n.name for n in self.nodes)

    @functools.cached_property
    def _index(self):
        return {n.name: i for i, n in enumerate(self.nodes)}

    def index(self, name):
        return self._index[name]

    def node(self, name):
        return self.nodes[self._index[name]]

    def neighbors(self, name):
        return self.node(name).neighbors

    def edges(self):
        """The links without direction: (a, b, type), a < b, each once, in
        link order."""
        seen, edges = set(), []
        for link in self.links:
            a, b = sorted((link.source, link.target))
            if (a, b, link.type) not in seen:
                seen.add((a, b, link.type))
                edges.append((a, b, link.type))
        return tuple(edges)

    def result(self, name):
        """The number a sum or a product makes (its number neighbor of the
        largest value); None for the other kinds."""
        if self.node(name).kind not in ("plus", "times"):
            return None
        numbers = [self.node(n) for n, _ in self.neighbors(name)
                   if self.node(n).kind == "number"]
        return max(numbers, key=lambda n: n.value).name


def _structure(rows):
    """ROWS: (name, holder, value, short name, type, ((neighbor, link type),
    ...)) per pnode, in *pnet*'s order."""
    link_types = {t for row in rows for _, t in row[5]}
    nodes = []
    for name, holder, value, short_name, type, neighbors in rows:
        if type == "NUM":
            kind = "number"
        elif name in link_types:
            kind = "link-type"
        elif _COMBINATION.fullmatch(name):
            kind = name.startswith("PLUS") and "plus" or "times"
        elif neighbors and all(t == "OPERATION" for _, t in neighbors):
            kind = "operation"
        else:
            kind = "concept"
        nodes.append(PnodeSpec(name, holder, kind, value, short_name, tuple(neighbors)))
    links = tuple(PnetLink(node.name, n, t) for node in nodes for n, t in node.neighbors)
    return PnetStructure(tuple(nodes), links)


def _name(x):
    return x.name if isinstance(x, Symbol) else x


@functools.lru_cache(maxsize=None)
def pnet_structure():
    """The Pnet's structure, from pnet_def's data (cached)."""
    init = {holder: dict(keys) for holder, keys in pnet_def.INIT_PNET}
    names = {holder: keys["name"].name for holder, keys in init.items()}
    rows = []
    for holder in pnet_def.PNET:
        keys = init[holder]
        rows.append((names[holder], holder.name, keys.get("value"), keys.get("short_name"),
                     _name(keys.get("type")),
                     tuple((names[n], names[t]) for n, t in keys.get("neighbors") or ())))
    return _structure(rows)


def structure_from_world(world):
    """The structure of WORLD's *pnet*."""
    # Not every symbol whose value is a pnode is its holder (the global NODE
    # of :hotter-neighbor-activation is one).
    holders = {id(world[h]): h for h, _ in pnet_def.INIT_PNET}

    def pnode(x):
        return world[x] if isinstance(x, Symbol) else x

    rows = []
    for p in world[_PNET]:
        p = pnode(p)
        rows.append((p.name.name, holders[id(p)].name, p.value, p.short_name, _name(p.type),
                     tuple((pnode(n).name.name, pnode(t).name.name)
                           for n, t in p.neighbors or ())))
    return _structure(rows)


# config's pseudo-instances (start.py), which the operations and the link
# types hold from the start: ("6g", "node-add"), ("5g", "operand"), ...
PSEUDO_INSTANCE_TYPES = frozenset({"5g", "6g"})


def is_pseudo_instance(instance):
    """Whether INSTANCE, (type, name), is one of config's pseudo-instances
    rather than a cyto-node (whose type is 1t, 2b, 3dt or 4bl)."""
    return instance[0] in PSEUDO_INSTANCE_TYPES


class PnetModel:
    """The pnodes' activations and instances, kept from events (an
    observer), or read from a World (from_world).  `changed`: the names of
    the pnodes the last event changed (all of them for a spread or an
    initialization)."""

    def __init__(self, structure=None):
        self.structure = structure or pnet_structure()
        self.reset()

    def reset(self):
        names = self.structure.names
        self._activations = dict.fromkeys(names)
        self._instances = dict.fromkeys(names)
        self.changed = frozenset()

    def activation(self, name):
        return self._activations[name]

    def activations(self):
        """The activations in *pnet*'s order."""
        return tuple(self._activations.values())

    def instances(self, name):
        return self._instances[name]

    def with_instances(self):
        """The names of the pnodes with cytoplasm instances, in *pnet*'s
        order."""
        return tuple(n for n, i in self._instances.items() if i)

    def cyto_instances(self, name):
        """NAME's instances that are cyto-nodes, without config's
        pseudo-instances."""
        return tuple(i for i in self._instances[name] or () if not is_pseudo_instance(i))

    def with_cyto_instances(self):
        """The names of the pnodes with cyto-node instances, in *pnet*'s
        order."""
        return tuple(n for n in self._instances if self.cyto_instances(n))

    def state(self):
        return (self.activations(), tuple(self._instances.values()))

    def __eq__(self, other):
        if not isinstance(other, PnetModel):
            return NotImplemented
        return self.structure.names == other.structure.names and self.state() == other.state()

    def __repr__(self):
        return f"<PnetModel {len(self.with_instances())} pnodes with instances>"

    @classmethod
    def from_world(cls, world, structure=None):
        model = cls(structure)
        for p in world[_PNET]:
            p = world[p] if isinstance(p, Symbol) else p
            name = p.name.name
            model._activations[name] = events.encode_data(p.activation)
            model._instances[name] = events.plain(p.instances)
        return model

    def on_event(self, event):
        self.changed = frozenset()
        handler = _HANDLERS.get(type(event))
        if handler is not None:
            handler(self, event)

    def _start(self, e):
        if tuple(e.pnet) != self.structure.names:
            raise PnetModelError(f"the run's *pnet* {e.pnet!r} is not the model's")
        self.reset()

    def _set_all(self, activations):
        if len(activations) != len(self._activations):
            raise PnetModelError(f"{len(activations)} activations for "
                                 f"{len(self._activations)} pnodes")
        self._activations = dict(zip(self.structure.names, activations))
        self.changed = frozenset(self.structure.names)

    def _initialized(self, e):
        self._set_all(e.activations)
        self._instances = dict.fromkeys(self.structure.names)

    def _spread(self, e):
        self._set_all(e.activations)

    def _pnodes_changed(self, e):
        table = {"activation": self._activations, "instances": self._instances}[e.field]
        for name, value in e.values:
            if name not in table:
                raise PnetModelError(f"no pnode {name!r}")
            table[name] = value
        self.changed = frozenset(name for name, _ in e.values)


_HANDLERS = {
    observe.RunStarted: PnetModel._start,
    observe.PnetInitialized: PnetModel._initialized,
    observe.PnetActivations: PnetModel._spread,
    observe.PnodesChanged: PnetModel._pnodes_changed,
}


# ---------------------------------------------------------------------------
# Layout

@dataclasses.dataclass(frozen=True)
class PnetLayout:
    """POSITIONS: pnode name -> (x, y), the center of its box of NODE_SIZE
    (w, h); CELLS: name -> (column, row) in a grid of CELL (w, h) cells;
    WIDTH and HEIGHT of the whole, margins included."""
    positions: dict
    cells: dict
    cell: tuple
    node_size: tuple
    width: float
    height: float


def layout_pnet(structure, cell=(64, 44), margin=10):
    """The grid layout of STRUCTURE (see the module docstring)."""
    numbers = sorted((n for n in structure.nodes if n.kind == "number"),
                     key=lambda n: n.value)
    column = {n.name: i for i, n in enumerate(numbers)}
    above, below = {}, {}
    for n in structure.nodes:
        if n.kind in ("plus", "times"):
            stacks = above if n.kind == "plus" else below
            stacks.setdefault(column[structure.result(n.name)], []).append(n.name)
    # An empty row on each side of the numbers: a number's links to a sum or
    # a product far to the side then cross the rows at a slope, instead of
    # running along the first row of boxes.
    up = max(map(len, above.values()), default=0)
    down = max(map(len, below.values()), default=0)
    row = up + 1
    cells = {}
    for n in numbers:
        cells[n.name] = (column[n.name], row)
    for col, names in above.items():
        for i, name in enumerate(names):     # the first nearest its number
            cells[name] = (col, row - 2 - i)
    for col, names in below.items():
        for i, name in enumerate(names):
            cells[name] = (col, row + 2 + i)
    others = [n.name for kind in ("operation", "concept", "link-type")
              for n in structure.nodes if n.kind == kind]
    columns = max(len(numbers), len(others))
    band = row + down + 2
    first = (columns - len(others)) // 2
    for i, name in enumerate(others):
        cells[name] = (first + i, band)
    cw, ch = cell
    positions = {name: (margin + (c + 0.5) * cw, margin + (r + 0.5) * ch)
                 for name, (c, r) in cells.items()}
    return PnetLayout(positions=positions, cells=cells, cell=tuple(cell),
                      node_size=(cw - 8, ch - 14), width=2 * margin + columns * cw,
                      height=2 * margin + (band + 1) * ch)


@functools.lru_cache(maxsize=None)
def pnet_layout(cell=(64, 44)):
    """The layout of pnet_structure() (cached per CELL)."""
    return layout_pnet(pnet_structure(), cell)

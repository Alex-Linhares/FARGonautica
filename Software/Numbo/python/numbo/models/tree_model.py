"""The cytoplasm as a forest of arithmetic trees (not 1987 source; loop0003).

A TreeModel is a shadow of the cytoplasm, by node name: every cyto-node
made in the run (killed ones too) with the fields the trees need, the
cytoplasm's order, *current-target*, and the node the global CYTO-TARGET
holds.  It is built either from a World (TreeModel.from_world) or
incrementally from a run's events (a TreeModel is an observer); after every
event of a run the two are equal (test_tree_model.py).  Everything else, the
forest and the expressions, is a pure function of that state.

How the 1987 graph reads as trees.  An operation node ("5g") has three
neighbors, one linked as RESULT and two as OPERAND (create-op-node), so that
RESULT = OPERAND + OPERAND (or x).  Which of the three it *defines* is not
the result but its upper neighbor, the one with the highest level above the
operation's own ((cyto-node :upper-neighbor)), here called its head:
  - a block made by const-bl+ or const-blx sits two levels above its
    operands, so it is the head; for a difference (const-bl+ with
    SUM = b1 - b2) it is linked as an operand: b1 = b2 + SUM, SUM = b1 - b2;
  - decomp+ hangs its operation node one level under the target it
    decomposes and the new derived target two levels under, so the target is
    the head: TARGET = BLOCK + DERIVED (BLOCK the operand) or, when the block
    is larger, BLOCK = TARGET + DERIVED, TARGET = BLOCK - DERIVED;
  - replace-target puts a block in the derived target's place among the
    operation's neighbors, so the target's tree reaches down to bricks.
The head's tree child is the operation node, whose two children are the
other two neighbors, in the order decompose prints them ((sup close node)
reverses the operation's neighbors), except that for a "-" or "/" the
result comes first: "r - o".  lisp/src/README.md: a PLUSa-b name is made of the
operation's values in whichever order it was built, so it is not read here.

Expressions are written as check_solution writes them: bricks bare, derived
operands in parentheses, "+", "-", "x", "/"; an operand that is neither a
brick nor derived (a derived target not yet matched, or the killed block of
the kill-block gap, PORTING_NOTES.md item 10) is its value in brackets,
"[7]".  decomposition() replays decompose's walk and gives what it prints.

The forest (Forest): the target's tree (with its derived targets), then the
free-standing blocks, then the free bricks, each a tree of frozen TreeNodes;
operation nodes whose head is gone (none have been seen) are `loose`; and
the ghosts, nodes no longer in the cytoplasm that no tree shows.  The
incremental model also remembers when each ghost left (`ghost_since`, in
events), which from_world can't know, so that a view can fade them, and
which names the latest event changed (`changed`; `last_change` and
`last_change_at` for the latest event that changed any), for a view's
highlight; neither is part of the state compared.
"""

import dataclasses
import re
from typing import NamedTuple

from numbo import events, observe
from numbo.cyto_def import CytoNode
from numbo.franz import Symbol
from numbo.franz import intern as _S

_CYTOPLASM = _S("*CYTOPLASM*")
_CURRENT_TARGET = _S("*CURRENT-TARGET*")
_CYTO_TARGET = _S("CYTO-TARGET")
CYTO_TARGET = "CYTO-TARGET"

# The fields kept, in a node's state list.
FIELDS = ("type", "value", "status", "level", "activation", "success", "neighbors")
_INDEX = {f: i for i, f in enumerate(FIELDS)}
_TYPE, _VALUE, _STATUS, _LEVEL, _ACTIVATION, _SUCCESS, _NEIGHBORS = range(len(FIELDS))

OP_TYPE = "5g"
RESULT = "RESULT"
# Root groups by type.
_GROUP = {"1t": "targets", "3dt": "targets", "4bl": "blocks", "2b": "bricks"}
# An operation's symbol: (head is the result, head is an operand).
_SYMBOLS = {"PLUS": ("+", "-"), "TIMES": ("x", "/")}
_COUNTER = re.compile(r"-V(\d+)$")
_BRICK = re.compile(r"^CYTO-BRICK(\d+)$")
_OP = re.compile(r"[A-Z]+")


class NodeInfo(NamedTuple):
    """A node's state in the model (neighbors: ((name, link type), ...) or
    None, as the events carry them)."""
    name: str
    type: object
    value: object
    status: object
    level: object
    activation: object
    success: object
    neighbors: object


@dataclasses.dataclass(frozen=True)
class TreeNode:
    """A node of the forest.  ALIVE: in the cytoplasm.  OP and SYMBOL for an
    operation node ("PLUS", "-").  EXPRESSION: what the node is, as an
    expression over its subtree (a brick's value; for an operation node, its
    head's).  SOLUTION: the node the global CYTO-TARGET holds; CURRENT: the
    current target."""
    name: str
    type: object
    value: object
    status: object
    level: object
    activation: object
    success: object
    alive: bool
    op: object = None
    symbol: object = None
    expression: str = ""
    solution: bool = False
    current: bool = False
    children: tuple = ()

    def walk(self):
        """This node and its subtree, depth first, parents first."""
        yield self
        for child in self.children:
            yield from child.walk()


@dataclasses.dataclass(frozen=True)
class Forest:
    """The cytoplasm drawn as trees: TARGETS (the target's tree first),
    free-standing BLOCKS, free BRICKS, LOOSE operation nodes (no live
    head) and GHOSTS (gone, and in no tree), each in a stable order (bricks
    by number, the rest by the name counter, "-Vn")."""
    targets: tuple = ()
    blocks: tuple = ()
    bricks: tuple = ()
    loose: tuple = ()
    ghosts: tuple = ()

    def trees(self):
        """Every tree, in drawing order (ghosts last)."""
        return self.targets + self.blocks + self.bricks + self.loose + self.ghosts

    def walk(self):
        for tree in self.trees():
            yield from tree.walk()


def order_key(name):
    """A stable order: the target, bricks by number, then by the name
    counter (the "-Vn" every block, derived target and operation has)."""
    if name == CYTO_TARGET:
        return (0, 0, name)
    m = _BRICK.match(name)
    if m:
        return (1, int(m.group(1)), name)
    m = _COUNTER.search(name)
    return (2, int(m.group(1)) if m else -1, name)


_SCALARS = (int, float, str)


def _plain(x):
    """events.plain (an ivar as the events carry it), quicker for the usual
    numbers, strings and nil."""
    if x is None or x is False:
        return None
    if type(x) in _SCALARS:
        return x
    return events.plain(x)


def _plain_neighbors(neighbors):
    """events.plain of a neighbors list: ((name, link type), ...) or None."""
    if not neighbors:
        return None
    if all(len(pair) == 2 and type(pair[0]) is CytoNode and type(pair[1]) is Symbol
           and pair[1].package != "KEYWORD" for pair in neighbors):
        return tuple((pair[0].name.name, pair[1].name) for pair in neighbors)
    return events.plain(neighbors)


def op_name(name):
    """An operation node's operation: its name's letters (PLUS, TIMES)."""
    m = _OP.match(name)
    return m.group() if m else name


class TreeModel:
    """The cytoplasm's trees, from a World or from a run's events."""

    def __init__(self):
        self.reset()

    def reset(self):
        self.nodes = {}            # name -> [field values, in FIELDS order]
        self.cytoplasm = []        # names, in the cytoplasm's order
        self._alive = set()
        self.current_target = None
        self.target_binding = None   # the name of the node CYTO-TARGET holds
        self.events_seen = 0
        self.ghost_since = {}      # name -> events_seen when it left
        self._gone = []            # names, in the order they left
        self._forests = {}
        self.changed = frozenset()       # names the latest event changed
        self.last_change = frozenset()   # ... the latest event that changed any
        self.last_change_at = None       # events_seen at that event

    # -- construction from a World ------------------------------------------

    @classmethod
    def from_world(cls, world):
        """The model of WORLD's cytoplasm now: every cyto-node in the
        registry under its own name, in the cytoplasm, or a neighbor of
        those (the registry loses a target whose global was rebound)."""
        model = cls()
        cytoplasm = world.values.get(_CYTOPLASM)
        if cytoplasm is None:
            return model
        found = {}
        for symbol, value in world.values.items():
            if type(value) is CytoNode and value.name is symbol:
                found[symbol.name] = value
        live = cytoplasm.nodes or ()
        for node in live:
            found.setdefault(node.name.name, node)
        todo = list(found.values())
        while todo:
            for pair in todo.pop().neighbors or ():
                name = pair[0].name.name
                if name not in found:
                    found[name] = pair[0]
                    todo.append(pair[0])
        for name, node in found.items():
            model.nodes[name] = [_plain(node.type), _plain(node.value), _plain(node.status),
                                 _plain(node.level), _plain(node.activation),
                                 _plain(node.success), _plain_neighbors(node.neighbors)]
        model.cytoplasm = [node.name.name for node in live]
        model._alive = set(model.cytoplasm)
        current = world.values.get(_CURRENT_TARGET)
        if current is not None:
            model.current_target = events.plain(current.name)
        bound = world.values.get(_CYTO_TARGET)
        if type(bound) is CytoNode:
            model.target_binding = bound.name.name
        return model

    # -- the incremental observer ----------------------------------------------

    def on_event(self, event):
        self.events_seen += 1
        self.changed = frozenset()
        kind = event.kind
        if kind == "node-changed":
            node = self.nodes.get(event.name)
            i = _INDEX.get(event.field)
            if node is None or i is None:
                return
            node[i] = event.value
            changed = (event.name,)
        elif kind == "node-created":
            self._create(event.name, [event.type, event.value, event.status, event.level,
                                      event.activation, event.success, None])
            if event.name == CYTO_TARGET and self.target_binding is None:
                self.target_binding = event.name
            changed = (event.name,)
        elif kind == "op-node-created":
            self._create(event.name, [
                OP_TYPE, None, None, event.level, None, None,
                ((event.result, RESULT),) + tuple((o, "OPERAND") for o in event.operands)])
            changed = (event.name,)
        elif kind == "node-killed":
            self.cytoplasm = list(event.cytoplasm)
            self._alive.discard(event.name)
            self.ghost_since[event.name] = self.events_seen
            self._gone.append(event.name)
            changed = (event.name,)
        elif kind == "current-target":
            self.current_target = event.name
            changed = (event.name,)
        elif kind == "target-replaced":
            if event.rebound:
                self.target_binding = event.block
            changed = (event.target, event.block)
        elif kind == "disconnect":
            # Nothing changes yet: its two node-killed events follow.  A view
            # can show what is about to go.
            self.changed = self.last_change = frozenset((event.node, event.op))
            self.last_change_at = self.events_seen
            return
        elif kind == "start":
            self.reset()
            self.events_seen = 1
            return
        else:
            return
        self._forests = {}
        self.changed = self.last_change = frozenset(changed)
        self.last_change_at = self.events_seen

    def _create(self, name, fields):
        self.nodes[name] = fields
        self.cytoplasm.insert(0, name)
        self._alive.add(name)
        self.ghost_since.pop(name, None)

    # -- state ------------------------------------------------------------------

    def state(self):
        """What the model is (from_world and the events agree on it)."""
        return (self.nodes, self.cytoplasm, self.current_target, self.target_binding)

    def __eq__(self, other):
        if not isinstance(other, TreeModel):
            return NotImplemented
        return self.state() == other.state()

    __hash__ = None

    def __repr__(self):
        return (f"<TreeModel {len(self.cytoplasm)} in the cytoplasm, "
                f"{len(self.nodes)} nodes>")

    def without(self, name):
        """A copy of the model without the node NAME (as the World is while a
        node-created event for NAME is published)."""
        model = TreeModel()
        model.nodes = {n: list(f) for n, f in self.nodes.items() if n != name}
        model.cytoplasm = [n for n in self.cytoplasm if n != name]
        model._alive = set(model.cytoplasm)
        model.current_target = self.current_target
        model.target_binding = None if self.target_binding == name else self.target_binding
        return model

    def has(self, name):
        return name in self.nodes

    def node(self, name):
        """NAME's NodeInfo (a KeyError if the model has no such node)."""
        return NodeInfo(name, *self.nodes[name])

    def is_alive(self, name):
        """Whether NAME is in the cytoplasm."""
        return name in self._alive

    def ghost_age(self, name):
        """How many events ago NAME left the cytoplasm (None: it hasn't, or
        the model can't know)."""
        since = self.ghost_since.get(name)
        return None if since is None else self.events_seen - since

    @property
    def solution_root(self):
        """The node decompose starts from: the one the global CYTO-TARGET
        holds (a block after "Obvious.")."""
        return self.target_binding

    def _resolve(self, name):
        """(eval name): CYTO-TARGET is the node it holds."""
        if name == CYTO_TARGET and self.target_binding is not None:
            return self.target_binding
        return name

    # -- the graph as trees -------------------------------------------------------

    def head(self, op):
        """The operation node OP's head: (cyto-node :upper-neighbor), the
        neighbor with the highest level above OP's own (the first on ties),
        or None."""
        node = self.nodes[op]
        best, level = None, node[_LEVEL]
        for name, _ in node[_NEIGHBORS] or ():
            other = self.nodes.get(name)
            if other is None:
                continue
            lv = other[_LEVEL]
            if isinstance(lv, (int, float)) and isinstance(level, (int, float)) and lv > level:
                best, level = name, lv
        return best

    def operands(self, op, head):
        """The operation node OP's two other neighbors than HEAD, in reading
        order, and its symbol: (a, b, symbol), for HEAD = a symbol b."""
        pairs = self.nodes[op][_NEIGHBORS] or ()
        others = [(n, t) for n, t in pairs if n != head][::-1]   # (sup close head)
        names = [n for n, _ in others]
        signs = _SYMBOLS.get(op_name(op))
        head_is_result = any(n == head and t == RESULT for n, t in pairs)
        if signs is None:
            symbol = op_name(op)
        elif head_is_result:
            symbol = signs[0]
        else:
            symbol = signs[1]
            results = [n for n, t in others if t == RESULT]
            if results:
                r = results[0]
                names = [r] + [n for n in names if n != r]
        return tuple(names), symbol

    def derivation(self, name):
        """The operation node NAME is derived by: the first operation among
        its neighbors whose head it is, or None."""
        for op, _ in self.nodes[name][_NEIGHBORS] or ():
            node = self.nodes.get(op)
            if node is not None and node[_TYPE] == OP_TYPE and self.head(op) == name:
                return op
        return None

    def expression(self, name, _depth=0):
        """NAME as an expression over bricks (check_solution's notation)."""
        node = self.nodes[name]
        if node[_TYPE] == OP_TYPE:
            head = self.head(name)
            return self.expression(head, _depth) if head is not None else name
        op = self.derivation(name) if _depth < 40 else None
        if op is None:
            return f"{node[_VALUE]}" if node[_TYPE] == "2b" else f"[{node[_VALUE]}]"
        names, symbol = self.operands(op, name)

        def side(n):
            e = self.expression(n, _depth + 1)
            if self.nodes[n][_TYPE] == "2b" or self.derivation(n) is None:
                return e
            return f"({e})"

        if len(names) != 2:
            return f"{symbol}({', '.join(side(n) for n in names)})"
        return f"{side(names[0])} {symbol} {side(names[1])}"

    def equation(self, name):
        """"VALUE = EXPRESSION" for the node NAME (check_solution's form)."""
        return f"{self.nodes[name][_VALUE]} = {self.expression(name)}"

    def decomposition(self, root=None):
        """What (decompose ROOT) prints (ROOT: the solution root), as
        observe.DecompositionSteps: for each operation node next to the
        node, not yet listed, its other two neighbors, then theirs."""
        steps = []
        listed = set()

        def decompose(name):
            name = self._resolve(name)
            for op, _ in self.nodes[name][_NEIGHBORS] or ():
                if op in listed:
                    continue
                listed.add(op)
                close = self.nodes[op][_NEIGHBORS] or ()
                a, b = [n for n, _ in close if n != name][::-1][:2]
                steps.append(observe.DecompositionStep(
                    op=op, a=a, va=self.nodes[self._resolve(a)][_VALUE],
                    b=b, vb=self.nodes[self._resolve(b)][_VALUE], result=name))
                for n in (a, b):
                    if self.nodes[self._resolve(n)][_TYPE] != "2b":
                        decompose(n)

        root = self.solution_root if root is None else root
        if root is not None:
            decompose(root)
        return tuple(steps)

    # -- the forest ---------------------------------------------------------------

    def forest(self, ghost_window=None):
        """The Forest now (cached until the next change).  GHOST_WINDOW:
        None, every ghost; N, only those that left the cytoplasm at most N
        events ago (the ones a view fades; it costs O(N), not O(every node
        ever made), and only the incremental model knows them).  A windowed
        forest is kept, the same object, until a tree changes or a ghost
        leaves the window: ghost ages are not in the forest, so most events
        cost only the O(N) look at the window."""
        key = None if ghost_window is None else tuple(self._recent_ghosts(ghost_window))
        forest = self._forests.get(ghost_window)
        if forest is None or forest[0] != key:
            forest = self._forests[ghost_window] = (key, self._build_forest(ghost_window))
        return forest[1]

    def _recent_ghosts(self, window):
        """The names that left at most WINDOW events ago (newest first)."""
        names = []
        for name in reversed(self._gone):
            age = self.ghost_age(name)
            if age is None or age > window:
                break
            if name not in self._alive:
                names.append(name)
        return names

    def _build_forest(self, ghost_window=None):
        alive = self._alive
        heads = {}       # live op -> its head
        derived = {}     # head -> [live ops], in the head's neighbor order
        child_of = set()
        # The edges come from the operation nodes' side only: a new one's
        # head lists it a few events later (create-op-node's update-neighbors).
        for name in sorted(self.cytoplasm, key=order_key):
            if self.nodes[name][_TYPE] != OP_TYPE:
                continue
            head = self.head(name)
            heads[name] = head
            names, _ = self.operands(name, head)
            child_of.update(names)
            if head is not None and head in alive:
                derived.setdefault(head, []).append(name)
        shown = set()
        expressions = {}

        def expression(name):
            e = expressions.get(name)
            if e is None:
                e = expressions[name] = self.expression(name)
            return e

        def make(name, path):
            shown.add(name)
            node = self.nodes[name]
            is_op = node[_TYPE] == OP_TYPE
            children, op, symbol = (), None, None
            if name not in path:
                path = path | {name}
                if is_op:
                    op = op_name(name)
                    head = heads.get(name)
                    names, symbol = self.operands(name, head)
                    children = tuple(make(n, path) for n in names if n in self.nodes)
                else:
                    children = tuple(make(o, path) for o in derived.get(name, ()))
            return TreeNode(
                name=name, type=node[_TYPE], value=node[_VALUE], status=node[_STATUS],
                level=node[_LEVEL], activation=node[_ACTIVATION], success=node[_SUCCESS],
                alive=name in alive, op=op, symbol=symbol, expression=expression(name),
                solution=name == self.target_binding, current=name == self.current_target,
                children=children)

        groups = {"targets": [], "blocks": [], "bricks": [], "loose": []}
        for name in sorted(self.cytoplasm, key=order_key):
            node = self.nodes[name]
            if node[_TYPE] == OP_TYPE:
                head = heads[name]
                if head is None or head not in alive:
                    groups["loose"].append(name)
            elif name not in child_of:
                groups[_GROUP.get(node[_TYPE], "loose")].append(name)
        trees = {g: tuple(make(n, frozenset()) for n in names) for g, names in groups.items()}
        if ghost_window is None:
            gone = (n for n in self.nodes if n not in alive)
        else:
            gone = self._recent_ghosts(ghost_window)
        ghosts = tuple(make(n, frozenset([n]))
                       for n in sorted(gone, key=order_key) if n not in shown)
        return Forest(ghosts=ghosts, **trees)

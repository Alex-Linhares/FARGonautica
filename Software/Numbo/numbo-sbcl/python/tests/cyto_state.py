"""The cytoplasm state encoding of python/fixtures/cyto_def.json, both ways.

tests/oracle/cyto-def.lisp writes a state as {"globals", "cytoplasm",
"current-target", "context", "nodes"}: the trace's Lisp-data encoding, with
a cyto-node written {"node": i} (its index in "nodes", numbered in the order
the JSON text first reaches it), a pnode {"pnode": holder}, and the other cyto
flavors {"obj": flavor, "global": true if eq to *cytoplasm* /
*current-target* / *context*}.  An unbound variable is ":UNBOUND".

`Encoder` is a port of that writer (same numbering, so a correct Python state
encodes to the oracle's text); `load_state` builds a World from a state.
"""

from conftest import lisp_data
from numbo import pnet_def
from numbo.cyto_def import Cytoplasm, CytoContext, CytoCurrentTarget, CytoNode
from numbo.franz import Symbol, find_package, intern
from numbo.pnet_def import Pnode
from numbo.world import World

UNBOUND = ":UNBOUND"

# (JSON key, symbol), in the order the oracle writes them.
GLOBALS = [("*temperature*", intern("*TEMPERATURE*")),
           ("*problem-solved*", intern("*PROBLEM-SOLVED*")),
           ("cyto-target", intern("CYTO-TARGET")),
           ("type", intern("TYPE")), ("status", intern("STATUS")), ("lv", intern("LV"))]
FREE_GLOBALS = GLOBALS[3:]

CYTOPLASM = intern("*CYTOPLASM*")
CURRENT_TARGET = intern("*CURRENT-TARGET*")
CONTEXT = intern("*CONTEXT*")

FLAVORS = {"cytoplasm": (Cytoplasm, CYTOPLASM),
           "cyto-current-target": (CytoCurrentTarget, CURRENT_TARGET),
           "cyto-context": (CytoContext, CONTEXT)}
FLAVOR_OF = {cls: name for name, (cls, _) in FLAVORS.items()}


def attr(ivar):
    """The Python attribute of a Lisp ivar name (current-target -> current_target)."""
    return ivar.replace("-", "_")


class Encoder:
    """cyto-def.lisp's cd-write and cd-state, on a World."""

    def __init__(self, world):
        self.world = world
        self.holders = {id(world[h]): h for h, _ in pnet_def.INIT_PNET}
        self.new_numbering()

    def new_numbering(self):
        self.ids = {}
        self.queue = []

    def ref(self, node):
        i = self.ids.get(id(node))
        if i is None:
            i = self.ids[id(node)] = len(self.queue)
            self.queue.append(node)
        return i

    def data(self, x):
        if isinstance(x, CytoNode):
            return {"node": self.ref(x)}
        if isinstance(x, Pnode):
            return {"pnode": self.holders[id(x)].name}
        if type(x) in FLAVOR_OF:
            is_global = any(g in self.world and self.world[g] is x
                            for g in (CYTOPLASM, CURRENT_TARGET, CONTEXT))
            return {"obj": FLAVOR_OF[type(x)], "global": True if is_global else None}
        if isinstance(x, list):
            return [self.data(v) for v in x] if x else None
        return lisp_data(x)

    def value(self, symbol):
        return self.data(self.world[symbol]) if symbol in self.world else UNBOUND

    def instance(self, obj, cls):
        if not isinstance(obj, cls):
            return UNBOUND
        return {ivar: self.data(getattr(obj, attr(ivar))) for ivar in cls.IVARS}

    def global_instance(self, symbol, cls):
        return self.instance(self.world[symbol], cls) if symbol in self.world else UNBOUND

    def free_globals(self):
        return {key: self.value(sym) for key, sym in FREE_GLOBALS}

    def state(self):
        """cd-state: a new numbering, kept for the calls after it."""
        self.new_numbering()
        out = {"globals": {key: self.value(sym) for key, sym in GLOBALS},
               "cytoplasm": self.global_instance(CYTOPLASM, Cytoplasm),
               "current-target": self.global_instance(CURRENT_TARGET, CytoCurrentTarget),
               "context": self.global_instance(CONTEXT, CytoContext)}
        nodes = []
        i = 0
        while i < len(self.queue):
            node = self.queue[i]
            record = self.instance(node, CytoNode)
            name = node.name
            record["symbol-value"] = self.value(name) if isinstance(name, Symbol) else None
            nodes.append(record)
            i += 1
        out["nodes"] = nodes
        return out


def load_state(state):
    """A World (with the Pnet) holding STATE.  Returns (world, nodes), nodes
    being the cyto-nodes in the state's numbering."""
    world = World()
    pnet_def.init_pnet(world)
    nodes = [CytoNode() for _ in state["nodes"] or ()]
    objects = {}
    for key, (cls, symbol) in [("cytoplasm", FLAVORS["cytoplasm"]),
                               ("current-target", FLAVORS["cyto-current-target"]),
                               ("context", FLAVORS["cyto-context"])]:
        if state[key] != UNBOUND:
            objects[symbol] = world[symbol] = cls()

    def decode(x):
        if isinstance(x, dict):
            if "node" in x:
                return nodes[x["node"]]
            if "pnode" in x:
                return world[intern(x["pnode"])]
            if "obj" in x:
                assert x["global"], f"a non-global {x['obj']} can't be rebuilt"
                return objects[FLAVORS[x["obj"]][1]]
            assert list(x) == ["str"], x
            return x["str"]
        if isinstance(x, str):
            if x.startswith(":") and len(x) > 1:
                return intern(x[1:], find_package("keyword"))
            return intern(x)
        if isinstance(x, list):
            return [decode(v) for v in x]
        return x

    for key, symbol in GLOBALS:
        if state["globals"][key] != UNBOUND:
            world[symbol] = decode(state["globals"][key])
    for key, (cls, symbol) in [("cytoplasm", FLAVORS["cytoplasm"]),
                               ("current-target", FLAVORS["cyto-current-target"]),
                               ("context", FLAVORS["cyto-context"])]:
        if symbol in objects:
            for ivar, value in state[key].items():
                setattr(objects[symbol], attr(ivar), decode(value))
    for node, record in zip(nodes, state["nodes"] or ()):
        for ivar in CytoNode.IVARS:
            setattr(node, attr(ivar), decode(record[ivar]))
        if record["symbol-value"] not in (None, UNBOUND):
            world[node.name] = decode(record["symbol-value"])
    return world, decode

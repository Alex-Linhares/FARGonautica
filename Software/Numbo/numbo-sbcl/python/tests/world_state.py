"""The World-state encoding of the codelet fixtures, both ways.

tests/oracle/lib/world-state.lisp writes a state as {"globals", "cytoplasm",
"current-target", "context", "pnodes", "coderack", "rng", "nodes"}: the
cytoplasm state of cyto_state.py (same node numbering, in JSON text order),
plus each holder's pnode as [activation, spreadable-activation,
temp-activation-holder, instances], the bins of the rack *coderack* names
([urgency, form, ...], newest first), and the RNG state.

`Encoder` is a port of that writer (a correct Python World encodes to the
oracle's text); `load_world` builds a World from a state.
"""

import cyto_state
from cyto_state import CYTOPLASM, CURRENT_TARGET, CONTEXT, FLAVORS, UNBOUND, attr
from numbo import coderack, pnet_def, pnet_functions
from numbo.coderack import Coderack
from numbo.cyto_def import Cytoplasm, CytoContext, CytoCurrentTarget, CytoNode
from numbo.franz import Symbol, find_package, intern
from numbo.world import World

# +ws-globals+, in its order.
GLOBAL_NAMES = ["*iteration*", "*temperature*", "*problem-solved*", "*name-counter*",
                "*coderack*", "cyto-target", "type", "status", "lv", "node", "res",
                "activation", "a", "brick", "bricki", "cyto-bricki", "target", "div",
                "current-target", "%resultx%", "%result+%", "%operand%",
                "diff", "diffrel", "min", "liste", "similarity", "address",
                "values-to-find", "node1", "node2", "cyto-block1", "cyto-block2",
                "n1", "n2", "n3", "oper", "nn", "pp", "cont", "n", "new", "reserve",
                "weights"]
GLOBALS = [(name, intern(name.upper())) for name in GLOBAL_NAMES]
CODERACK_VAR = intern("*CODERACK*")
PNET = intern("*PNET*")


class Encoder(cyto_state.Encoder):
    """world-state.lisp's ws-write and ws-state, on a World."""

    def nodes_from(self, start):
        """ws-nodes-from: the nodes numbered START and up, with the ones they
        reach in turn."""
        nodes = []
        i = start
        while i < len(self.queue):
            node = self.queue[i]
            record = self.instance(node, CytoNode)
            name = node.name
            record["symbol-value"] = self.value(name) if isinstance(name, Symbol) else None
            nodes.append(record)
            i += 1
        return nodes

    def rack(self):
        world = self.world
        name = world[CODERACK_VAR] if CODERACK_VAR in world else None
        rack = world.get_prop(name, coderack.CODERACK) if isinstance(name, Symbol) else None
        return self.data(rack.bins) if rack is not None else UNBOUND

    def state(self):
        """ws-state: a new numbering, kept for the calls after it."""
        self.new_numbering()
        world = self.world
        out = {"globals": {key: self.value(sym) for key, sym in GLOBALS},
               "cytoplasm": self.global_instance(CYTOPLASM, Cytoplasm),
               "current-target": self.global_instance(CURRENT_TARGET, CytoCurrentTarget),
               "context": self.global_instance(CONTEXT, CytoContext)}
        pnodes = {}
        for holder, _ in pnet_def.INIT_PNET:
            p = world[holder]
            pnodes[holder.name] = self.data([p.activation, p.spreadable_activation,
                                             p.temp_activation_holder, p.instances])
        out["pnodes"] = pnodes
        out["coderack"] = self.rack()
        out["rng"] = world.rng.state
        out["nodes"] = self.nodes_from(0)
        return out


def load_world(state, parameters, extra_nodes=()):
    """A World holding STATE, with the Pnet (init-pnet, *pnet*, the resolved
    neighbors) and PARAMETERS ({name: value}, the %...% globals).
    EXTRA_NODES are the nodes numbered after the state's (a case's
    "arg-nodes").  Returns (world, decode), decode mapping encoded data
    (args) to Python objects."""
    world = World()
    for name, value in parameters.items():
        world[intern(name)] = cyto_state_decode_plain(value)
    pnet_def.init_pnet(world)
    world[PNET] = pnet_def.pnet_list(world)
    pnet_functions.initialize_pnet_2(world)
    records = list(state["nodes"] or ()) + list(extra_nodes or ())
    nodes = [CytoNode() for _ in records]
    objects = {}
    for key in ("cytoplasm", "current-target", "context"):
        cls, symbol = FLAVORS[_FLAVOR_OF_KEY[key]]
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
        elif symbol in world:  # a parameter config sets later (%resultx% ...)
            del world[symbol]
    for key in ("cytoplasm", "current-target", "context"):
        symbol = FLAVORS[_FLAVOR_OF_KEY[key]][1]
        if symbol in objects:
            for ivar, value in state[key].items():
                setattr(objects[symbol], attr(ivar), decode(value))
    for holder, (activation, spreadable, temp, instances) in state["pnodes"].items():
        p = world[intern(holder)]
        p.activation = activation
        p.spreadable_activation = spreadable
        p.temp_activation_holder = temp
        p.instances = decode(instances)
    if state["coderack"] != UNBOUND:
        name = world[CODERACK_VAR]
        world.put_prop(name, Coderack(name, decode(state["coderack"])), coderack.CODERACK)
    world.rng.state = state["rng"]
    for node, record in zip(nodes, records):
        for ivar in CytoNode.IVARS:
            setattr(node, attr(ivar), decode(record[ivar]))
        if record["symbol-value"] not in (None, UNBOUND):
            world[node.name] = decode(record["symbol-value"])
    return world, decode


_FLAVOR_OF_KEY = {"cytoplasm": "cytoplasm", "current-target": "cyto-current-target",
                  "context": "cyto-context"}


def cyto_state_decode_plain(x):
    """Plain Lisp data (no nodes or pnodes), decoded."""
    if isinstance(x, dict):
        assert list(x) == ["str"], x
        return x["str"]
    if isinstance(x, str):
        if x.startswith(":") and len(x) > 1:
            return intern(x[1:], find_package("keyword"))
        return intern(x)
    if isinstance(x, list):
        return [cyto_state_decode_plain(v) for v in x]
    return x

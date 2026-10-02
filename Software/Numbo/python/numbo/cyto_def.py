"""The cytoplasm: its four flavors, their methods, and init-cytoplasm.

Port of lisp/src/cyto-def.lisp, in its order.  A flavor is a class whose instance
variables are plain attributes (all settable, inittable and gettable, nil by
default; a Lisp ivar current-target is the attribute current_target).  A
method (defmethod (cytoplasm :msg) ...) is the function
cytoplasm_msg(world, self, ...), and (cyto-node :msg) is cyto_node_msg;
CYTOPLASM_METHODS and CYTO_NODE_METHODS map the message keyword to it (send).

The registry: the 1987 code keeps each cyto-node in the global variable named
by its name, (set name (make-instance 'cyto-node :name name ...)), and finds
it again with (eval name).  Those are World symbol values (world.py);
register_node and node_named are the two directions.  *cytoplasm*,
*current-target*, *context* and *temperature* are World globals too.

A neighbor is a pair (node link-type), a two-element list.
python/tests/test_cyto_def.py replays the oracle's cases,
python/fixtures/cyto_def.json.
"""

from numbo import franz
from numbo.franz import intern as _S

_TEMPERATURE = _S("*TEMPERATURE*")
_CYTOPLASM = _S("*CYTOPLASM*")
_CURRENT_TARGET = _S("*CURRENT-TARGET*")
_CONTEXT = _S("*CONTEXT*")
_TYPE = _S("TYPE")
_STATUS = _S("STATUS")
_LV = _S("LV")


class _Flavor:
    """A defflavor with :settable-, :inittable- and :gettable-instance-
    variables: IVARS (Lisp names, in defflavor order) are attributes, nil
    (None) until set, as Franz Flavors fills a new instance with nil."""

    IVARS = ()
    __slots__ = ()

    def __init__(self, **init):
        for ivar in self.IVARS:
            setattr(self, ivar.replace("-", "_"), None)
        for key, value in init.items():
            setattr(self, key, value)


class Cytoplasm(_Flavor):
    """cyto-def.lisp: defflavor cytoplasm."""

    IVARS = ("target", "brick1", "brick2", "brick3", "brick4", "brick5", "nodes",
             "current-target", "context")
    __slots__ = tuple(v.replace("-", "_") for v in IVARS)


class CytoNode(_Flavor):
    """cyto-def.lisp: defflavor cyto-node."""

    IVARS = ("activation", "neighbors", "name", "type", "value", "success", "status",
             "level", "listed", "plinks")
    __slots__ = IVARS

    def __repr__(self):
        return f"#<cyto-node {self.name!r}>"


class CytoCurrentTarget(_Flavor):
    """cyto-def.lisp: defflavor cyto-current-target."""

    IVARS = ("interest", "name")
    __slots__ = IVARS


class CytoContext(_Flavor):
    """cyto-def.lisp: defflavor cyto-context."""

    IVARS = ("type", "name", "value", "interest", "location")
    __slots__ = IVARS


def register_node(world, node):
    """(set name node), as create-cyto-node and create-op-node do: NODE
    becomes the value of the global named by its name."""
    world[node.name] = node
    return node


def node_named(world, name):
    """(eval name): the value of the global NAME (an error if unbound)."""
    return world[name]


def _eq(x, y):
    if isinstance(x, str) and isinstance(y, str):
        raise TypeError("eq on two Lisp strings can't be modelled in Python")
    return x is y


def _number(x):
    """< and > take numbers only; anything else (nil, a symbol) is a Lisp error."""
    if isinstance(x, bool) or not isinstance(x, (int, float)):
        raise TypeError(f"the value {x!r} is not of type REAL")
    return x


def _nil(lst):
    """A collected Python list as Lisp data: an empty one is nil."""
    return lst or None


def cytoplasm_cyto_brick_block_nodes(world, self):
    """cyto-def.lisp: (cytoplasm :cyto-brick-block-nodes).  The bricks and
    blocks among the nodes (every type but "1t", "3dt", "5g"), reversed."""
    res = []
    for x in self.nodes or ():
        # 1987: cytoplasm has no type ivar, so (setq type ...) sets the global
        # TYPE (PORTING_NOTES.md, "Compile census", type and status).
        world[_TYPE] = x.type
        if franz.equal("1t", world[_TYPE]):
            pass
        elif franz.equal("3dt", world[_TYPE]):
            pass
        elif franz.equal("5g", world[_TYPE]):
            pass
        else:
            res.insert(0, x)
    return _nil(res)


def cytoplasm_find_new_target(world, self):
    """cyto-def.lisp: (cytoplasm :find-new-target).  The last node that is
    not "linked" and not a "2b", "4bl" or "5g": a free target."""
    res = None
    for x in self.nodes or ():
        # 1987: the globals TYPE and STATUS, not ivars (as above).
        world[_TYPE] = x.type
        world[_STATUS] = x.status
        if franz.equal("linked", world[_STATUS]):
            pass
        elif franz.equal("2b", world[_TYPE]):
            pass
        elif franz.equal("4bl", world[_TYPE]):
            pass
        elif franz.equal("5g", world[_TYPE]):
            pass
        else:
            res = x
    return res


def cytoplasm_free_blocks(world, self):
    """cyto-def.lisp: (cytoplasm :free-blocks).  The nodes that are not
    "linked" and not a "1t", "3dt" or "5g" (free bricks and blocks), reversed."""
    res = []
    for x in self.nodes or ():
        # 1987: the globals TYPE and STATUS, not ivars (as above).
        world[_TYPE] = x.type
        world[_STATUS] = x.status
        if franz.equal("linked", world[_STATUS]):
            pass
        elif franz.equal("1t", world[_TYPE]):
            pass
        elif franz.equal("3dt", world[_TYPE]):
            pass
        elif franz.equal("5g", world[_TYPE]):
            pass
        else:
            res.insert(0, x)
    return _nil(res)


def cytoplasm_free_cyto_nodes(world, self):
    """cyto-def.lisp: (cytoplasm :free-cyto-nodes).  The nodes that are not
    "linked" and not a "5g", reversed."""
    res = []
    for x in self.nodes or ():
        # 1987: the globals STATUS and TYPE, not ivars (as above).
        world[_STATUS] = x.status
        world[_TYPE] = x.type
        if franz.equal("linked", world[_STATUS]):
            pass
        elif franz.equal("5g", world[_TYPE]):
            pass
        else:
            res.insert(0, x)
    return _nil(res)


def cytoplasm_free_secondary_cyto_nodes(world, self):
    """cyto-def.lisp: (cytoplasm :free-secondary-cyto-nodes).  The nodes that
    are not "linked" and not a "1t", "2b" or "5g" (free blocks and derived
    targets), reversed."""
    res = []
    for x in self.nodes or ():
        # 1987: the globals STATUS and TYPE, not ivars (as above).
        world[_STATUS] = x.status
        world[_TYPE] = x.type
        if franz.equal("linked", world[_STATUS]):
            pass
        elif franz.equal("1t", world[_TYPE]):
            pass
        elif franz.equal("2b", world[_TYPE]):
            pass
        elif franz.equal("5g", world[_TYPE]):
            pass
        else:
            res.insert(0, x)
    return _nil(res)


def init_cytoplasm(world, target, brick1, brick2, brick3, brick4, brick5):
    """cyto-def.lisp: init-cytoplasm.  *temperature* 100, a new *context*,
    *current-target* and *cytoplasm* (with no nodes); returns the cytoplasm.
    The old cyto-nodes stay in the registry."""
    world[_TEMPERATURE] = 100
    world[_CONTEXT] = CytoContext()
    world[_CURRENT_TARGET] = CytoCurrentTarget()
    world[_CYTOPLASM] = Cytoplasm(target=target, brick1=brick1, brick2=brick2,
                                  brick3=brick3, brick4=brick4, brick5=brick5,
                                  current_target=world[_CURRENT_TARGET],
                                  context=world[_CONTEXT])
    return world[_CYTOPLASM]


def cyto_node_lower_dtarget_neighbor(world, self):
    """cyto-def.lisp: (cyto-node :lower-dtarget-neighbor).  The "3dt"
    neighbor with the lowest level (the first on ties; nil if none below
    1000)."""
    res = None
    lev = 1000
    for pair in self.neighbors or ():
        # 1987: a free (setq lv ...): the global LV (PORTING_NOTES.md,
        # "Compile census", lv).
        world[_LV] = pair[0].level
        if franz.equal("3dt", pair[0].type) and _number(world[_LV]) < lev:
            lev = world[_LV]
            res = pair[0]
    return res


def cyto_node_lower_neighbor(world, self):
    """cyto-def.lisp: (cyto-node :lower-neighbor).  The neighbor with the
    lowest level (the first on ties), unless the node is a "5g"."""
    res = None
    lev = 1000
    if franz.equal("5g", self.type):
        pass
    else:
        for pair in self.neighbors or ():
            # 1987: the global LV (as above).
            world[_LV] = pair[0].level
            if _number(world[_LV]) < lev:
                lev = world[_LV]
                res = pair[0]
    return res


def cyto_node_block_neighbor(world, self):
    """cyto-def.lisp: (cyto-node :block-neighbor).  For a free "3dt": the
    last "2b" or "4bl" neighbor of its upper neighbor (the operation it came
    from)."""
    res = None
    if franz.nequal("3dt", self.type):
        pass
    elif franz.nequal("free", self.status):
        pass
    else:
        # An error, as in Lisp, when there is no upper neighbor: (send nil
        # :neighbors).
        l = _send_neighbors(cyto_node_upper_neighbor(world, self))
        for pair in l or ():
            node = pair[0]
            if franz.equal("2b", node.type):
                res = node
            elif franz.equal("4bl", node.type):
                res = node
    return res


def _send_neighbors(node):
    """(send node :neighbors), for a cyto-node only."""
    if not isinstance(node, CytoNode):
        raise AttributeError(f"SEND: {node!r} does not handle the message :NEIGHBORS")
    return node.neighbors


def replace_function(l, l1, l2):
    """cyto-def.lisp: replace-function.  L (a list of pairs) with every pair
    whose car is eq to L1 replaced by (L2 . its cdr), reversed."""
    res = []
    for x in l or ():
        if _eq(x[0], l1):
            pair = [l2] + x[1:]
        else:
            pair = x
        res.insert(0, pair)
    return _nil(res)


def cyto_node_replace_neighbors(world, self, l1, l2):
    """cyto-def.lisp: (cyto-node :replace-neighbors).  Replace the neighbor
    L1 by L2 (replace-function, so the neighbors end up reversed)."""
    self.neighbors = replace_function(self.neighbors, l1, l2)
    return self.neighbors


def cytoplasm_secondary_cyto_nodes(world, self):
    """cyto-def.lisp: (cytoplasm :secondary-cyto-nodes).  The nodes that are
    not a "1t", "2b" or "5g" (blocks and derived targets), reversed."""
    res = []
    for x in self.nodes or ():
        # 1987: the global TYPE (as in :cyto-brick-block-nodes).
        world[_TYPE] = x.type
        if franz.equal("1t", world[_TYPE]):
            pass
        elif franz.equal("2b", world[_TYPE]):
            pass
        elif franz.equal("5g", world[_TYPE]):
            pass
        else:
            res.insert(0, x)
    return _nil(res)


def cyto_node_suppress_neighbors(world, self, l1):
    """cyto-def.lisp: (cyto-node :suppress-neighbors).  Remove the pairs
    whose car is eq to L1; the neighbors end up reversed."""
    res = []
    for x in self.neighbors or ():
        if _eq(x[0], l1):
            pass
        else:
            res.insert(0, x)
    self.neighbors = _nil(res)
    return self.neighbors


def cytoplasm_suppress_node(world, self, l):
    """cyto-def.lisp: (cytoplasm :suppress-node).  Remove L from the nodes;
    they end up reversed."""
    res = []
    for x in self.nodes or ():
        if _eq(x, l):
            pass
        else:
            res.insert(0, x)
    self.nodes = _nil(res)
    return self.nodes


def update_context(world, type, name, value, interest, location):
    """cyto-def.lisp: update-context.  Set the five ivars of *context*."""
    context = world[_CONTEXT]
    context.type = type
    context.name = name
    context.value = value
    context.interest = interest
    context.location = location
    return location


def update_current_target(world, name, interest):
    """cyto-def.lisp: update-current-target.  Set the two ivars of
    *current-target*.  NAME is a node or, in some codelets, the symbol
    cyto-target."""
    current_target = world[_CURRENT_TARGET]
    current_target.name = name
    current_target.interest = interest
    return interest


def cyto_node_update_neighbors(world, self, l):
    """cyto-def.lisp: (cyto-node :update-neighbors).  Put the list L of
    neighbors in front of the current ones (append)."""
    # append copies L and shares the old neighbors; here both are copied,
    # which is the same as long as nothing changes a neighbors list in place
    # (nothing in the source does).
    self.neighbors = _nil(list(l or ()) + list(self.neighbors or ()))
    return self.neighbors


def cyto_node_update_plinks(world, self, l):
    """cyto-def.lisp: (cyto-node :update-plinks).  Push the pnode L on the
    plinks."""
    self.plinks = [l] + list(self.plinks or ())
    return self.plinks


def cyto_node_upper_neighbor(world, self):
    """cyto-def.lisp: (cyto-node :upper-neighbor).  The neighbor with the
    highest level above the node's own (the first on ties), or nil."""
    res = None
    lev = self.level
    for pair in self.neighbors or ():
        # 1987: the global LV (as in :lower-dtarget-neighbor).
        world[_LV] = pair[0].level
        if _number(world[_LV]) > _number(lev):
            lev = world[_LV]
            res = pair[0]
    return res


def _keyword(prefix, fn):
    return _S(fn.__name__[len(prefix):].upper().replace("_", "-"), "KEYWORD")


CYTOPLASM_METHODS = {
    _keyword("cytoplasm_", fn): fn
    for fn in (cytoplasm_cyto_brick_block_nodes, cytoplasm_find_new_target,
               cytoplasm_free_blocks, cytoplasm_free_cyto_nodes,
               cytoplasm_free_secondary_cyto_nodes, cytoplasm_secondary_cyto_nodes,
               cytoplasm_suppress_node)
}

CYTO_NODE_METHODS = {
    _keyword("cyto_node_", fn): fn
    for fn in (cyto_node_lower_dtarget_neighbor, cyto_node_lower_neighbor,
               cyto_node_block_neighbor, cyto_node_replace_neighbors,
               cyto_node_suppress_neighbors, cyto_node_update_neighbors,
               cyto_node_update_plinks, cyto_node_upper_neighbor)
}

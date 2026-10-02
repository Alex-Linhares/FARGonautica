"""The pnode methods and the Pnet functions: decay, spreading activation,
posting codelets.

Port of lisp/src/pnet-functions.lisp, in its order.  A method
(defmethod (pnode :msg) ...) is the function pnode_msg(world, self, ...),
and PNODE_METHODS maps the message keyword to it (send).  The instance
variables are Pnode attributes (pnet_def); the globals they read, the
parameters %k%, %first-threshold%, ..., and *pnet*, *iteration*, the holders
and the free NODE and RES, are symbol values in the World (world.py).
python/tests/test_pnet_functions.py replays the oracle's snapshots,
python/fixtures/pnet_functions.json.
"""

import math

from numbo import franz
from numbo.franz import intern as _S
from numbo.world import lisp_eval

_PNET = _S("*PNET*")
_ITERATION = _S("*ITERATION*")
_CODERACK = _S("*CODERACK*")
_VERBOSE = _S("%VERBOSE%")
_NODE = _S("NODE")
_RES = _S("RES")
_SIMILAR = _S("SIMILAR")
_INITIAL_ACTIVATION = _S("%INITIAL-ACTIVATION%")
_MIN_ACTIVATION_TO_BE_ADDED = _S("%MIN-ACTIVATION-TO-BE-ADDED%")
_MAX_ACTIVATION_TO_BE_TRANSMITTED = _S("%MAX-ACTIVATION-TO-BE-TRANSMITTED%")
_K = _S("%K%")
_LENGTH = _S("%LENGTH%")
_FIRST_THRESHOLD = _S("%FIRST-THRESHOLD%")
_UPPER_THRESHOLD = _S("%UPPER-THRESHOLD%")
_MAX = _S("MAX")
_ADD = _S("ADD")
_DECAY_RATES = {
    "1t": _S("%FIRST-DECAY-RATE%"),
    "2b": _S("%SECOND-DECAY-RATE%"),
    "3dt": _S("%THIRD-DECAY-RATE%"),
    "5g": _S("%FIFTH-DECAY-RATE%"),
    "6g": _S("%SIXTH-DECAY-RATE%"),
}
_FOURTH_DECAY_RATE = _S("%FOURTH-DECAY-RATE%")
# The quoted '(minus *iteration*) of :modify-threshold: one constant, shared
# by every threshold it builds.
_MINUS_ITERATION = [_S("MINUS"), _ITERATION]


def _nil(lst):
    """A collected Python list as Lisp data: an empty one is nil."""
    return lst or None


def pnode_activation_decay(world, self):
    """pnet-functions.lisp: (pnode :activation-decay).  The activation the
    pnode loses in a cycle: its decay factor times its activation."""
    return pnode_activation_decay_factor(world, self) * self.activation


def pnode_activation_decay_factor(world, self):
    """pnet-functions.lisp: (pnode :activation-decay-factor).  The decay rate
    of the type of the pnode's first instance in alphabetical order (1t, 2b,
    3dt, 5g, 6g; anything else, or none, is the fourth rate)."""
    # 1987: Franz sortcar is destructive; oracle mode (and franz.sortcar)
    # copies, so the instances are left as they are (PORTING_NOTES.md,
    # "Oracle hooks", copying sortcar).
    instances = franz.sortcar(self.instances, None)
    s = instances[0][0] if instances else None
    if isinstance(s, str) and s in _DECAY_RATES:
        return world[_DECAY_RATES[s]]
    return world[_FOURTH_DECAY_RATE]


def pnode_add_activation(world, self, act):
    """pnet-functions.lisp: (pnode :add-activation).  Add ACT (possibly
    negative) to the activation if |ACT| is above
    %min-activation-to-be-added%; the activation never goes below 0."""
    if abs(act) > world[_MIN_ACTIVATION_TO_BE_ADDED]:
        # (max 0 ...) is the integer 0 when the sum is negative.
        self.activation = franz.max_(0, self.activation + act)
        return self.activation
    return None


def pnode_add_temp_activation_holder(world, self, act):
    """pnet-functions.lisp: (pnode :add-temp-activation-holder)."""
    self.temp_activation_holder = self.temp_activation_holder + act
    return self.temp_activation_holder


def pnode_codelet_urgency(world, self, base_urgency):
    """pnet-functions.lisp: (pnode :codelet-urgency).  The urgency of a
    codelet the pnode posts: its base urgency."""
    return base_urgency


def pnode_hotter_neighbor_activation(world, self):
    """pnet-functions.lisp: (pnode :hotter-neighbor-activation).  The highest
    activation of a neighbor not linked by `similar` (0 if none is higher)."""
    maxi = 0
    for link in self.neighbors or ():
        # 1987: a free (setq node ...): it sets the global NODE, not a local
        # (PORTING_NOTES.md, "Compile census", undefined variable NODE).
        world[_NODE] = link[0]
        if link[1] is world[_SIMILAR]:
            pass
        else:
            act = link[0].activation
            if act > maxi:
                maxi = act
    return maxi


def initialize_codelet(world):
    """pnet-functions.lisp: initialize-codelet.  Every codelet of every
    *pnet* pnode whose threshold is not %first-threshold%'s value gets the
    threshold %first-threshold% (the symbol)."""
    for pnode in world[_PNET]:
        # The loop runs over the list :codelets had when it began, while
        # :modify-threshold replaces (and reverses) it.
        for codelet in pnode.codelets or ():
            codelet_call = codelet[0]
            threshold = codelet[1]
            if franz.nequal(lisp_eval(world, threshold),
                            lisp_eval(world, world[_FIRST_THRESHOLD])):
                pnode_modify_threshold(world, pnode, codelet_call, _FIRST_THRESHOLD)


def initialize_pnet(world):
    """pnet-functions.lisp: initialize-pnet.  Reset every *pnet* pnode's
    activations and instances, then initialize-codelet."""
    for pnode in world[_PNET]:
        pnode.activation = world[_INITIAL_ACTIVATION]
        pnode.temp_activation_holder = 0.0
        pnode.spreadable_activation = 0.0
        pnode.instances = None
    initialize_codelet(world)


def initialize_pnet_2(world):
    """pnet-functions.lisp: initialize-pnet-2.  Evaluate the symbols of every
    *pnet* pnode's neighbors: (node link-type) becomes a pair of pnodes."""
    for pnode in world[_PNET]:
        pnode.neighbors = _nil([[lisp_eval(world, pair[0]), lisp_eval(world, pair[1])]
                                for pair in pnode.neighbors or ()])


def pnode_link_length(world, self, k=0.1):
    """pnet-functions.lisp: (pnode :link-length).  1 + 1/(%k% * activation
    + 1/%length%): %length% + 1 at activation 0, down to 1.  K is unused, as
    in 1987 (the body uses %k%)."""
    return 1 + franz.quotient(float(1), world[_K] * self.activation
                              + franz.quotient(float(1), world[_LENGTH]))


def pnode_modify_threshold(world, self, codelet_call, new=None):
    """pnet-functions.lisp: (pnode :modify-threshold).  Give the codelets
    called CODELET-CALL the threshold NEW, by default the form
    (max %first-threshold% (add *iteration* (minus *iteration*)
    %upper-threshold%)) with the current values filled in, which starts at
    %upper-threshold% and goes down by 1 an iteration."""
    if new is None:
        new = [_MAX, world[_FIRST_THRESHOLD],
               [_ADD, world[_ITERATION], _MINUS_ITERATION, world[_UPPER_THRESHOLD]]]
    res = []
    for codelet in self.codelets or ():
        function = codelet[0]
        if franz.equal(codelet_call, function):
            codelet = [function, new] + codelet[2:]
        res.append(codelet)
    # 1987: res is built with cons, so the codelets come out reversed.
    res.reverse()
    self.codelets = _nil(res)
    return self.codelets


def populate_coderack(world):
    """pnet-functions.lisp: populate-coderack.  Every codelet of every *pnet*
    pnode whose activation is at least the codelet's threshold is posted on
    the coderack with its urgency, and its threshold is reset
    (:modify-threshold)."""
    for pnode in world[_PNET]:
        for codelet in pnode.codelets or ():
            codelet_call = codelet[0]
            threshold = codelet[1]
            base_urgency = codelet[2]
            arguments = codelet[3] if len(codelet) > 3 else None
            if pnode.activation >= lisp_eval(world, threshold):
                if world[_VERBOSE]:
                    world.out.write(f"About to post codelet {franz.princ_to_string(codelet_call)}"
                                    f" {franz.princ_to_string(arguments)}\n")
                world.cr_hang(world[_CODERACK], [codelet_call] + (arguments or []),
                              pnode_codelet_urgency(world, pnode,
                                                    lisp_eval(world, base_urgency)))
                pnode_modify_threshold(world, pnode, codelet_call)


def pnode_print(world, self):
    """pnet-functions.lisp: (pnode :print).  Describe the pnode on
    world.out."""
    out = world.out
    out.write("I am a pnode.\n")
    for label, value in (("name", self.name), ("instances", self.instances),
                         ("activation", self.activation),
                         ("spreadable-activation", self.spreadable_activation),
                         ("temp-activation-holder", self.temp_activation_holder),
                         ("neighbors", self.neighbors)):
        out.write(f"{label}: {franz.princ_fill(value, len(label) + 2)}\n")


def set_up_activations(world, list_of_pnodes_and_activations):
    """pnet-functions.lisp: set-up-activations.  Set the activations as the
    list (node1 act1 node2 act2 ...) says (the nodes are holder symbols)."""
    rest = list_of_pnodes_and_activations or []
    while rest:
        lisp_eval(world, rest[0]).activation = rest[1]
        rest = rest[2:]
    return None


def pnode_spread_activation(world, self):
    """pnet-functions.lisp: (pnode :spread-activation).  Give each neighbor
    (in its temporary holder) the spreadable activation divided by the square
    root of the link type's link-length."""
    for link in self.neighbors or ():
        linked_node = link[0]
        link_type = link[1]
        link_length = pnode_link_length(world, link_type)
        pnode_add_temp_activation_holder(
            world, linked_node,
            franz.quotient(self.spreadable_activation, math.sqrt(link_length)))
    return None


def spread_activation_in_pnet(world):
    """pnet-functions.lisp: spread-activation-in-pnet.  One cycle, in
    parallel: every pnode computes its decay (its spreadable activation),
    then every pnode spreads it to its neighbors, then every pnode updates
    its activation."""
    pnet = world[_PNET]
    for pnode in pnet:
        pnode.spreadable_activation = pnode_activation_decay(world, pnode)
    for pnode in pnet:
        pnode_spread_activation(world, pnode)
    for pnode in pnet:
        pnode_update_activation(world, pnode)
    return None


def pnode_subtract_activation(world, self, act):
    """pnet-functions.lisp: (pnode :subtract-activation).  Subtract ACT if it
    is above %min-activation-to-be-added% (the activation may go negative)."""
    if act > world[_MIN_ACTIVATION_TO_BE_ADDED]:
        self.activation = self.activation - act
        return self.activation
    return None


def _eq(x, y):
    if isinstance(x, str) and isinstance(y, str):
        raise TypeError("eq on two Lisp strings can't be modelled in Python")
    return x is y


def pnode_suppress_instances(world, self, l):
    """pnet-functions.lisp: (pnode :suppress-instances).  Remove the
    instances whose cyto-node is L (the others come out reversed)."""
    # 1987: a free (setq res nil): RES is the global, not a local
    # (PORTING_NOTES.md, "Compile census", undefined variable RES).  The
    # instances slot and RES end up the same list.
    world[_RES] = None
    for x in self.instances or ():
        if _eq(x[1], l):
            pass
        else:
            world[_RES] = [x] + (world[_RES] or [])
    self.instances = world[_RES]
    return self.instances


def pnode_update_activation(world, self):
    """pnet-functions.lisp: (pnode :update-activation).  activation :=
    activation - spreadable activation + received activation (none if a
    non-numeric pnode received less than %min-activation-to-be-added%; at
    most %max-activation-to-be-transmitted%), then clear the two holders."""
    activation_to_add = self.temp_activation_holder
    if self.value is None and activation_to_add < world[_MIN_ACTIVATION_TO_BE_ADDED]:
        activation_to_add = 0
    activation_to_add = franz.min_(world[_MAX_ACTIVATION_TO_BE_TRANSMITTED], activation_to_add)
    new_activation = (self.activation - self.spreadable_activation) + activation_to_add
    self.activation = new_activation
    self.spreadable_activation = 0.0
    self.temp_activation_holder = 0.0
    return self.temp_activation_holder


def pnode_update_instances(world, self, l):
    """pnet-functions.lisp: (pnode :update-instances).  Push the instance L,
    a pair (type cyto-node), on the instances."""
    # A new list (cons): RES may still be the old one.
    self.instances = [l] + (self.instances or [])
    return self.instances


# send: message keyword -> method.
PNODE_METHODS = {
    _S(fn.__name__[len("pnode_"):].upper().replace("_", "-"), "KEYWORD"): fn
    for fn in (pnode_activation_decay, pnode_activation_decay_factor,
               pnode_add_activation, pnode_add_temp_activation_holder,
               pnode_codelet_urgency, pnode_hotter_neighbor_activation,
               pnode_link_length, pnode_modify_threshold, pnode_print,
               pnode_spread_activation, pnode_subtract_activation,
               pnode_suppress_instances, pnode_update_activation,
               pnode_update_instances)
}

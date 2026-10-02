"""The engine's typed events: the hooks that publish them (not 1987 source;
loop0003).

The hook points are lisp/src/oracle.lisp's, as trace.py had them until loop0003:
install() encapsulates (harness.encapsulate, the Python sb-int:encapsulate)
config, cr-choose, cr-hang, mod, cr-empty-coderack, create-cyto-node,
create-op-node, disconnect, spread-activation-in-pnet and decompose, with
hooks of the oracle's names, and adds hooks where the oracle has no event:
codelets', init's and start's `send` (a cyto-node's ivar changing, a node
leaving the cytoplasm, a pnode's activation or instances changing by its
own message), update-current-target, replace-target (which may setq the
global cyto-target to a block), initialize-pnet, set-up-activations (which
sets pnodes' activations directly) and create-coderack.  Every change to a
cyto-node in a run is a send from codelets.py (create-cyto-node and
create-op-node make them): test_observe.py checks the events against the
World after every event, and test_pnet_coderack_models.py the Pnet's and
the coderack's.

A hook calls straight through unless a run is publishing: `current` is a
Publisher (the oracle's run specials) while harness.run_config runs with a
trace or observers.  It then publishes observe.py's events to the run's
Subject; the oracle trace is the observer trace.OracleTraceWriter.  What a
hook computes for an event is what the oracle's hook computed for its line
(the temperature, the rack, the encoded arguments), and the hooks keep the
oracle's one intervention, handing config a form that ends the set-up phase
after its last set-up codelet: so a run with observers is the run with an
oracle trace, which the 220 SBCL-equivalence runs check.
"""

import contextlib
import re

from numbo import codelets, coderack, franz, harness, observe, pnet_functions, solution_checker
from numbo import init, start
from numbo import world as world_module
from numbo.cyto_def import Cytoplasm, CytoContext, CytoCurrentTarget, CytoNode
from numbo.flavors import send
from numbo.franz import Symbol
from numbo.franz import intern as _S
from numbo.pnet_def import Pnode

_ITERATION = _S("*ITERATION*")
_CODERACK = _S("*CODERACK*")
_CYTOPLASM = _S("*CYTOPLASM*")
_PNET = _S("*PNET*")
_X = _S("X")
_QUOTE = _S("QUOTE")
_ORACLE_END_SETUP = _S("ORACLE-END-SETUP")

# oracle.lisp: +oracle-setup-chooses+.  config's set-up phase has exactly 13
# (eval (cr-choose *coderack*)) calls (3 after read-target, 2 after each of
# the 5 read-brick).
SETUP_CHOOSES = 13

_FLAVOR_NAMES = {CytoNode: "cyto-node", Pnode: "pnode", Cytoplasm: "cytoplasm",
                 CytoCurrentTarget: "cyto-current-target", CytoContext: "cyto-context"}

# The publishing run (a Publisher), or None.
current = None


# ---------------------------------------------------------------------------
# Encodings

def encode_data(x):
    """oracle.lisp: oracle-write-data.  Lisp data X as JSON data: nil null, t
    true, a number itself, a string {"str": ...}, a symbol its name (a
    keyword ":NAME"), a list an array, a flavor instance {"obj": flavor,
    "name": its name}; anything else is an error.  (Arrays are tuples here:
    the events are frozen.)"""
    if x is None or x is False or (isinstance(x, list) and not x):
        return None
    if x is True:
        return True
    if isinstance(x, int):
        return x
    if isinstance(x, float):
        if x != x or x in (float("inf"), float("-inf")):
            raise TypeError(f"oracle trace: {x!r} is not a finite double-float")
        return x
    if isinstance(x, str):
        return {"str": x}
    if isinstance(x, Symbol):
        return ":" + x.name if x.package == "KEYWORD" else x.name
    if isinstance(x, list):
        return tuple(encode_data(v) for v in x)
    flavor = _FLAVOR_NAMES.get(type(x))
    if flavor is not None:
        return {"obj": flavor, "name": encode_data(getattr(x, "name", None))}
    raise TypeError(f"oracle trace: cannot encode {x!r}")


def plain(x):
    """Lisp data X as an event field: nil None, t True, a number or a Lisp
    string itself, a symbol its name (a keyword ":NAME"), a cyto-node or a
    pnode its name, a list a tuple (an empty one None)."""
    if x is None or x is False:
        return None
    if x is True or isinstance(x, (int, float, str)):
        return x
    if isinstance(x, Symbol):
        return ":" + x.name if x.package == "KEYWORD" else x.name
    if isinstance(x, (CytoNode, Pnode)):
        return plain(x.name)
    if isinstance(x, list):
        return tuple(plain(v) for v in x) or None
    raise TypeError(f"events: cannot encode {x!r}")


def _rack_field(world, rack):
    """oracle.lisp: oracle-rack-field.  ((urgency, count), ...) in bin order."""
    return tuple((b[0], len(b) - 1) for b in coderack.cr_get(world, rack).bins)


def _form_fields(form, urgency):
    """oracle.lisp: oracle-form-fields.  codelet, args, urgency of a codelet
    FORM (None: all None)."""
    if form is None:
        return {"codelet": None, "args": None, "urgency": None}
    return {"codelet": form[0].name, "args": tuple(encode_data(a) for a in form[1:]),
            "urgency": urgency}


def _node_type(type):
    """oracle.lisp: oracle-node-fields' type: a string itself."""
    return type if isinstance(type, str) else encode_data(type)


# ---------------------------------------------------------------------------
# Run state

class Publisher:
    """The oracle's run specials for one publishing run: the World, the
    Subject, *oracle-phase* (None, "setup", "last-setup" or "loop"),
    *oracle-setup-chooses*, *oracle-last-iteration*, *oracle-in-hook*,
    *oracle-decompose-depth*, *oracle-decomposition* and
    *oracle-choose-draws*."""

    def __init__(self, world, subject):
        self.world = world
        self.subject = subject
        self.publish = subject.publish
        self.phase = None
        self.setup_chooses = 0
        self.last_iteration = None
        self.in_hook = False
        self.decompose_depth = 0
        self.decomposition = None
        self.choose_draws = None

    def run_started(self, problem, seed, max_iterations):
        """oracle-run-config's start event."""
        self.publish(observe.RunStarted(
            problem=encode_data(list(problem)), seed=seed,
            max_iterations=encode_data(max_iterations), rng="splitmix64",
            pnet=tuple(_pnode(self.world, p).name.name for p in self.world[_PNET])))

    def run_ended(self, result):
        """oracle-run-config's last event: done, gave-up, capped or error."""
        self.phase = None
        its = result["iterations"]
        outcome = result["outcome"]
        if outcome == "solved":
            event = observe.Solved(iterations=its, decomposition=self.decomposition or ())
        elif outcome == "gave-up":
            event = observe.GaveUp(iterations=its)
        elif outcome == "capped":
            event = observe.Capped(iterations=its)
        else:
            event = observe.RunError(iterations=its, message=result["error"])
        self.publish(event)


@contextlib.contextmanager
def publishing(world, subject):
    """Bind `current` to a Publisher to SUBJECT for WORLD (None if SUBJECT is
    None) while the block runs, and tell it of WORLD's RNG draws."""
    global current
    saved = current
    current = Publisher(world, subject) if subject is not None else None
    world.rng.sink = oracle_note_draw
    try:
        yield current
    finally:
        current = saved


def oracle_note_draw(n, value):
    """oracle.lisp: oracle-note-draw.  A draw made by cr-choose goes into its
    event; any other one is an rng event."""
    p = current
    if p is None:
        return
    if p.choose_draws is not None:
        p.choose_draws.append((n, value))
    else:
        p.publish(observe.RngDraw(n=n, value=value))


def _pnode(world, p):
    return world[p] if isinstance(p, Symbol) else p


# ---------------------------------------------------------------------------
# World inspection

def oracle_call_without_global_effects(world, thunk):
    """oracle.lisp: oracle-call-without-global-effects.  Call THUNK, then
    undo any change it made to a global's value.  temperature's
    collect-misfortune SETQs the global current-target, which look-for-blx
    reads; the events must not change it."""
    saved = dict(world.values)
    try:
        return thunk()
    finally:
        for symbol in list(world.values):
            if symbol not in saved:
                del world.values[symbol]
        for symbol, value in saved.items():
            if world.values.get(symbol, saved) is not value:
                world.values[symbol] = value


# ---------------------------------------------------------------------------
# Hooks (each one gets the original function and its arguments)

def oracle_begin_iteration(p, n):
    """oracle.lisp: oracle-begin-iteration."""
    world = p.world
    p.in_hook, in_hook = True, p.in_hook
    try:
        temperature = oracle_call_without_global_effects(
            world, lambda: codelets.temperature(world))
    finally:
        p.in_hook = in_hook
    p.publish(observe.IterationBegan(n=n, x=world[_X], temperature=encode_data(temperature),
                                     rack=_rack_field(world, world[_CODERACK])))


def oracle_iteration_hook(fn, *args):
    """oracle.lisp: oracle-iteration-hook.  On mod and cr-empty-coderack:
    config calls one of them first in every main-loop iteration, right after
    (setq *iteration* y)."""
    p = current
    if p is not None and p.phase == "loop" and not p.in_hook:
        n = p.world[_ITERATION]
        if not franz._eql(n, p.last_iteration):
            p.last_iteration = n
            oracle_begin_iteration(p, n)
    return fn(*args)


def oracle_cr_empty_coderack_hook(fn, *args):
    """oracle.lisp: oracle-cr-empty-coderack-hook."""
    result = oracle_iteration_hook(fn, *args)
    if current is not None:
        current.publish(observe.RackEmptied())
    return result


def oracle_config_hook(fn, *args):
    """oracle.lisp: oracle-config-hook.  config's set-up phase begins."""
    p = current
    if p is not None:
        p.phase, p.setup_chooses, p.last_iteration = "setup", 0, None
    try:
        return fn(*args)
    finally:
        if p is not None:
            p.phase = None


def oracle_cr_choose_hook(fn, world, name, full=False):
    """oracle.lisp: oracle-cr-choose-hook.  A setup-choose event in the
    set-up phase; in the main loop, a codelet-chosen event (the oracle trace
    writes it into the iteration's line)."""
    p = current
    if p is None or p.phase is None:
        return fn(world, name, full)
    rack = _rack_field(world, name)
    saved, p.choose_draws = p.choose_draws, []
    try:
        res = fn(world, name, True)
        fields = _form_fields(res[0] if res else None, res[1] if res else None)
        draws = tuple(p.choose_draws)
    finally:
        p.choose_draws = saved
    if p.phase == "setup":
        p.publish(observe.SetupChoose(rack=rack, draws=draws, **fields))
        p.setup_chooses += 1
        if p.setup_chooses == SETUP_CHOOSES:
            # The main loop starts only once config has evaluated this last
            # set-up codelet, which may itself call mod (compare-b-to-t ->
            # digits-in-common).  So hand config a form that evaluates the
            # codelet, then switches phase.
            p.phase = "last-setup"
            if res:
                res = [[_ORACLE_END_SETUP, [_QUOTE, res[0]]], res[1]]
            else:
                p.phase = "loop"
    else:
        # The oracle raises here when no iteration line is pending (a choice
        # outside an iteration, which config never makes); the trace writer
        # reports it.
        p.publish(observe.CodeletChosen(n=p.last_iteration, draws=draws, **fields))
    if full:
        return res
    return res[0] if res else None


def oracle_end_setup(world, form):
    """oracle.lisp: oracle-end-setup.  Evaluate config's last set-up codelet
    FORM, then start the main-loop phase."""
    result = world_module.lisp_eval(world, form)
    if current is not None:
        current.phase = "loop"
    return result


def oracle_cr_hang_hook(fn, world, name, form, urgency):
    """oracle.lisp: oracle-cr-hang-hook.  A post event after every cr-hang."""
    result = fn(world, name, form, urgency)
    if current is not None:
        current.publish(observe.CodeletPosted(**_form_fields(form, urgency)))
    return result


def oracle_create_cyto_node_hook(fn, world, activation, name, value, status, level, type,
                                 success):
    """oracle.lisp: oracle-create-cyto-node-hook.  Published before the call:
    in create-cyto-node nothing that makes an event comes before its "Node ~a
    created" line."""
    if current is not None:
        current.publish(observe.NodeCreated(
            name=name.name, type=_node_type(type), value=encode_data(value),
            status=plain(status), level=plain(level), activation=plain(activation),
            success=plain(success)))
    return fn(world, activation, name, value, status, level, type, success)


_OP = re.compile(r"[A-Z]+")


def oracle_create_op_node_hook(fn, world, name, res, op1, op2, activation, level):
    """oracle.lisp: oracle-create-op-node-hook.  create-op-node makes a "5g"
    node with no value: the operation (its name's letters, PLUS or TIMES)
    of OP1 and OP2, with result RES."""
    if current is not None:
        current.publish(observe.OpNodeCreated(
            name=name.name, op=_OP.match(name.name).group(), result=plain(res),
            operands=(plain(op1), plain(op2)), level=plain(level)))
    return fn(world, name, res, op1, op2, activation, level)


def oracle_disconnect_hook(fn, world, cyto_node, op):
    """oracle.lisp: oracle-disconnect-hook.  disconnect always prints "Node
    <op> killed" then "Node <cyto-node> killed", with no event before them."""
    if current is not None:
        current.publish(observe.Disconnect(
            node=send(world, cyto_node, "name").name,
            node_type=_node_type(send(world, cyto_node, "type")),
            node_value=encode_data(send(world, cyto_node, "value")),
            op=send(world, op, "name").name,
            op_type=_node_type(send(world, op, "type")),
            op_value=encode_data(send(world, op, "value"))))
    return fn(world, cyto_node, op)


def oracle_spread_hook(fn, world, *args):
    """oracle.lisp: oracle-spread-hook.  The 88 activations after every
    spread-activation-in-pnet."""
    result = fn(world, *args)
    if current is not None:
        current.publish(observe.PnetActivations(activations=tuple(
            encode_data(send(world, _pnode(world, p), "activation")) for p in world[_PNET])))
    return result


class _Broadcast:
    """make-broadcast-stream of the World's output and a capture."""

    def __init__(self, out, capture):
        self.out = out
        self.capture = capture

    def write(self, text):
        self.capture.append(text)
        return self.out.write(text)


def oracle_decompose_hook(fn, world, node):
    """oracle.lisp: oracle-decompose-hook.  The top-level decompose's output
    is parsed into a decomposition event (and the done event's
    decomposition)."""
    p = current
    if p is None or p.decompose_depth > 0:
        if p is None:
            return fn(world, node)
        p.decompose_depth += 1
        try:
            return fn(world, node)
        finally:
            p.decompose_depth -= 1
    capture = []
    out = world.out
    world.out = _Broadcast(out, capture)
    p.decompose_depth = 1
    try:
        result = fn(world, node)
    finally:
        world.out = out
        p.decompose_depth = 0
    p.decomposition = tuple(observe.DecompositionStep(**step) for step in
                            oracle_parse_decomposition("".join(capture)))
    p.publish(observe.Decomposition(steps=p.decomposition))
    return result


# The messages after which a cyto-node's ivar has changed, and the ivar.
_NODE_MESSAGES = dict(
    {f"set-{ivar}": ivar for ivar in CytoNode.IVARS},
    **{"update-neighbors": "neighbors", "suppress-neighbors": "neighbors",
       "replace-neighbors": "neighbors", "update-plinks": "plinks"})
_message_names = {}


def _message_name(message):
    """flavors.py's _message, as a lower-case name ("set-status")."""
    name = _message_names.get(message)
    if name is None:
        name = (message.name if isinstance(message, Symbol) else message.lstrip(":")).lower()
        _message_names[message] = name
    return name


# The messages that may change a pnode's activation or instances, and the
# ivar.  Inside spread-activation-in-pnet pnodes change by direct calls, and
# its pnet event has all 88 activations.
_PNODE_MESSAGES = {
    "set-activation": "activation", "add-activation": "activation",
    "subtract-activation": "activation", "set-instances": "instances",
    "update-instances": "instances", "suppress-instances": "instances"}
_PNODE_ENCODINGS = {"activation": encode_data, "instances": plain}


def _same(a, b):
    """A and B are the same event data (0 is not 0.0)."""
    if isinstance(a, tuple) and isinstance(b, tuple):
        return len(a) == len(b) and all(_same(x, y) for x, y in zip(a, b))
    return type(a) is type(b) and a == b


def send_hook(fn, world, object, message, *args):
    """No oracle hook: codelets.py's and init.py's send.  A node-changed
    event after a message that changes a cyto-node's ivar, a node-killed one
    after (cytoplasm :suppress-node) takes a node out of the cytoplasm, a
    pnodes-changed one after a message changed a pnode's activation or
    instances (only if it did change them)."""
    p = current
    if p is None:
        return fn(world, object, message, *args)
    kind = type(object)
    if kind is CytoNode:
        result = fn(world, object, message, *args)
        ivar = _NODE_MESSAGES.get(_message_name(message))
        if ivar is not None:
            p.publish(observe.NodeChanged(name=object.name.name, field=ivar,
                                          value=plain(getattr(object, ivar))))
        return result
    if kind is Pnode:
        ivar = _PNODE_MESSAGES.get(_message_name(message))
        if ivar is None:
            return fn(world, object, message, *args)
        encode = _PNODE_ENCODINGS[ivar]
        before = encode(getattr(object, ivar))
        result = fn(world, object, message, *args)
        after = encode(getattr(object, ivar))
        if not _same(before, after):
            p.publish(observe.PnodesChanged(field=ivar, values=((object.name.name, after),)))
        return result
    if kind is Cytoplasm and _message_name(message) == "suppress-node":
        node = args[0] if args else None
        was_in = any(x is node for x in object.nodes or ())
        result = fn(world, object, message, *args)
        if was_in:
            p.publish(observe.NodeKilled(name=node.name.name, cytoplasm=tuple(
                x.name.name for x in object.nodes or ())))
        return result
    return fn(world, object, message, *args)


_CYTO_TARGET = _S("CYTO-TARGET")


def replace_target_hook(fn, world, cyto_block, cyto_current_target):
    """No oracle hook: codelets.py's replace-target, which may (setq
    cyto-target cyto-block) or take the derived target out of the
    cytoplasm."""
    p = current
    if p is None:
        return fn(world, cyto_block, cyto_current_target)
    target = world.values.get(_CYTO_TARGET)
    cytoplasm = world.values.get(_CYTOPLASM)
    was_in = cytoplasm is not None and any(x is cyto_current_target
                                           for x in cytoplasm.nodes or ())
    result = fn(world, cyto_block, cyto_current_target)
    if world.values.get(_CYTO_TARGET) is not target:
        p.publish(observe.TargetReplaced(target=_CYTO_TARGET.name, block=plain(cyto_block),
                                         rebound=True))
    elif was_in and not any(x is cyto_current_target for x in cytoplasm.nodes or ()):
        p.publish(observe.TargetReplaced(target=plain(cyto_current_target),
                                         block=plain(cyto_block), rebound=False))
    return result


def update_current_target_hook(fn, world, name, interest):
    """No oracle hook: codelets.py's update-current-target."""
    result = fn(world, name, interest)
    if current is not None:
        current.publish(observe.CurrentTargetChanged(name=plain(name), interest=plain(interest)))
    return result


def initialize_pnet_hook(fn, world):
    """No oracle hook: pnet_functions.py's initialize-pnet, which config
    calls first: every pnode's activation and instances are reset."""
    result = fn(world)
    if current is not None:
        current.publish(observe.PnetInitialized(activations=tuple(
            encode_data(_pnode(world, p).activation) for p in world[_PNET])))
    return result


def set_up_activations_hook(fn, world, list_of_pnodes_and_activations):
    """No oracle hook: pnet_functions.py's set-up-activations (repump's),
    which sets pnodes' activations directly: one pnodes-changed event with
    the ones that changed."""
    p = current
    if p is None:
        return fn(world, list_of_pnodes_and_activations)
    holders = (list_of_pnodes_and_activations or [])[::2]
    pnodes = [world_module.lisp_eval(world, h) for h in holders]
    before = [encode_data(x.activation) for x in pnodes]
    result = fn(world, list_of_pnodes_and_activations)
    values = tuple((x.name.name, encode_data(x.activation)) for x, b in zip(pnodes, before)
                   if not _same(b, encode_data(x.activation)))
    if values:
        p.publish(observe.PnodesChanged(field="activation", values=values))
    return result


def create_coderack_hook(fn, world):
    """No oracle hook: codelets.py's create-coderack (init-chiffre's), which
    makes the coderack and names it in *coderack*."""
    result = fn(world)
    if current is not None:
        rack = coderack.cr_get(world, world[_CODERACK])
        current.publish(observe.CoderackCreated(name=plain(rack.name),
                                                levels=tuple(b[0] for b in rack.bins)))
    return result


# oracle-parse-decomposition uses the checker's tokenizer, as in the Lisp.
solution_tokens = solution_checker.solution_tokens


def _parse_integer_junk_allowed(s):
    """(parse-integer s :junk-allowed t): a sign and digits, else None."""
    m = re.match(r"[ \t\n\r]*([+-]?[0-9]+)", s)
    return int(m.group(1)) if m else None


def oracle_parse_decomposition(text):
    """oracle.lisp: oracle-parse-decomposition.  The paragraphs decompose
    printed, "Operation OP has been applied / to A ( VA) and to B ( VB) / to
    get R", as objects; a value of digits and minus signs is an integer."""
    tokens = solution_tokens(text)

    def tok(i):
        return tokens[i] if i < len(tokens) else ""

    def val(s):
        if s and all(c in "0123456789-" for c in s):
            v = _parse_integer_junk_allowed(s)
            return s if v is None else v
        return s

    ops = []
    for i in range(len(tokens)):
        if tok(i) == "Operation":
            if not (tok(i + 2) == "has" and tok(i + 9) == "to" and tok(i + 12) == "to"
                    and tok(i + 13) == "get"):
                raise ValueError(f"oracle: unexpected decompose output {text!r}")
            ops.append({"op": tok(i + 1), "a": tok(i + 6), "va": val(tok(i + 7)),
                        "b": tok(i + 10), "vb": val(tok(i + 11)), "result": tok(i + 14)})
    return ops


# ---------------------------------------------------------------------------
# Installation

HOOKS = [
    (start, "config", "oracle", oracle_config_hook),
    (coderack, "cr_choose", "oracle", oracle_cr_choose_hook),
    (coderack, "cr_hang", "oracle", oracle_cr_hang_hook),
    (franz, "mod", "oracle", oracle_iteration_hook),
    (coderack, "cr_empty_coderack", "oracle", oracle_cr_empty_coderack_hook),
    (codelets, "create_cyto_node", "oracle", oracle_create_cyto_node_hook),
    (codelets, "create_op_node", "oracle", oracle_create_op_node_hook),
    (codelets, "disconnect", "oracle", oracle_disconnect_hook),
    (pnet_functions, "spread_activation_in_pnet", "oracle", oracle_spread_hook),
    (codelets, "decompose", "oracle", oracle_decompose_hook),
    (codelets, "send", "events", send_hook),
    (codelets, "update_current_target", "events", update_current_target_hook),
    (codelets, "replace_target", "events", replace_target_hook),
    (init, "send", "events", send_hook),
    (start, "send", "events", send_hook),
    (pnet_functions, "initialize_pnet", "events", initialize_pnet_hook),
    (pnet_functions, "set_up_activations", "events", set_up_activations_hook),
    (codelets, "create_coderack", "events", create_coderack_hook),
]


def install():
    """oracle.lisp: oracle-install (the trace part), and the two hooks it has
    no event for.  Encapsulate the hooked functions, once; then the
    iteration cap, which must be outside the trace hooks, so that the
    iteration it stops at is never begun in the trace."""
    for module, name, tag, hook in HOOKS:
        tags = getattr(getattr(module, name), "encapsulations", ())
        if "iteration-cap" in tags and "oracle" not in tags:
            raise RuntimeError(f"events.install: the iteration cap would be inside the "
                               f"trace hook of {module.__name__}.{name}")
        harness.encapsulate(module, name, tag, hook)
    harness.install_iteration_cap()


world_module.EVAL_WORLD_FUNCTIONS[_ORACLE_END_SETUP] = oracle_end_setup

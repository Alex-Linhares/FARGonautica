"""The JSON-lines trace of a run, as src/oracle.lisp writes it.

Port of the trace part of src/oracle.lisp (sections "JSON", "Run state",
"Hooks" and oracle-run-config's events).  The events, their fields and the
Lisp-data encoding are specified in that file's header and in
PORTING_NOTES.md, "Oracle hooks"; one event per line, as a JSON object whose
keys come in the oracle's order.

As in the oracle, no ported function writes an event itself.  install()
encapsulates (harness.encapsulate, the Python sb-int:encapsulate) the same
functions oracle-install does, with hooks of the same names: config,
cr-choose, cr-hang, mod, cr-empty-coderack, create-cyto-node, create-op-node,
disconnect, spread-activation-in-pnet and decompose.  A hook calls straight
through unless a trace is open: `current` (the oracle's *oracle-trace* and
the other run specials) is a Trace while harness.run_config runs with one.
"""

import contextlib
import json
import re

from numbo import (codelets, coderack, cyto_def, franz, harness, pnet_functions,
                   solution_checker, start)
from numbo import world as world_module
from numbo.cyto_def import Cytoplasm, CytoContext, CytoCurrentTarget, CytoNode
from numbo.flavors import send
from numbo.franz import Symbol
from numbo.franz import intern as _S
from numbo.pnet_def import Pnode

_ITERATION = _S("*ITERATION*")
_CODERACK = _S("*CODERACK*")
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

# The open trace (a Trace), or None: *oracle-trace* and the run specials.
current = None


# ---------------------------------------------------------------------------
# JSON

def encode_data(x):
    """oracle.lisp: oracle-write-data.  Lisp data X as JSON data: nil null, t
    true, a number itself, a string {"str": ...}, a symbol its name (a
    keyword ":NAME"), a list an array, a flavor instance {"obj": flavor,
    "name": its name}; anything else is an error."""
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
        return [encode_data(v) for v in x]
    flavor = _FLAVOR_NAMES.get(type(x))
    if flavor is not None:
        return {"obj": flavor, "name": encode_data(getattr(x, "name", None))}
    raise TypeError(f"oracle trace: cannot encode {x!r}")


def _rack_field(world, rack):
    """oracle.lisp: oracle-rack-field.  [[urgency, count], ...] in bin order."""
    return [[b[0], len(b) - 1] for b in coderack.cr_get(world, rack).bins]


def _form_fields(form, urgency):
    """oracle.lisp: oracle-form-fields.  codelet, args, urgency of a codelet
    FORM (None: all null)."""
    if form is None:
        return [("codelet", None), ("args", None), ("urgency", None)]
    return [("codelet", form[0].name), ("args", [encode_data(a) for a in form[1:]]),
            ("urgency", urgency)]


def _node_fields(name, type, value):
    """oracle.lisp: oracle-node-fields."""
    return [("name", name.name), ("type", type if isinstance(type, str) else encode_data(type)),
            ("value", encode_data(value))]


# ---------------------------------------------------------------------------
# Run state

class Trace:
    """The oracle's run specials for one traced run: the stream, the World,
    *oracle-rng-events*, *oracle-phase* (None, "setup", "last-setup" or
    "loop"), *oracle-setup-chooses*, *oracle-last-iteration*,
    *oracle-pending* (the current iteration event, not yet written),
    *oracle-in-hook*, *oracle-decompose-depth*, *oracle-decomposition* and
    *oracle-choose-draws*."""

    def __init__(self, stream, world, rng_events=False):
        self.stream = stream
        self.world = world
        self.rng_events = rng_events
        self.phase = None
        self.setup_chooses = 0
        self.last_iteration = None
        self.pending = None
        self.in_hook = False
        self.decompose_depth = 0
        self.decomposition = None
        self.choose_draws = None

    def write_event(self, pairs):
        """oracle.lisp: oracle-write-event."""
        self.stream.write(json.dumps(dict(pairs), allow_nan=False) + "\n")

    def flush_pending(self):
        """oracle.lisp: oracle-flush-pending."""
        if self.pending is not None:
            e, self.pending = self.pending, None
            self.write_event(e)

    def emit(self, ev, *pairs):
        """oracle.lisp: oracle-emit.  Write event EV, after any pending
        iteration event."""
        self.flush_pending()
        self.write_event([("ev", ev)] + list(pairs))

    def emit_start(self, problem, seed, max_iterations):
        """oracle-run-config's start event."""
        self.emit("start", ("problem", encode_data(list(problem))), ("seed", seed),
                  ("max_iterations", encode_data(max_iterations)), ("rng", "splitmix64"),
                  ("pnet", [_pnode(self.world, p).name.name for p in self.world[_PNET]]))

    def emit_outcome(self, result):
        """oracle-run-config's last event: done, gave-up, capped or error."""
        self.phase = None
        self.flush_pending()
        its = ("iterations", result["iterations"])
        outcome = result["outcome"]
        if outcome == "solved":
            self.emit("done", its, ("decomposition", list(self.decomposition or ())))
        elif outcome == "gave-up":
            self.emit("gave-up", its)
        elif outcome == "capped":
            self.emit("capped", its)
        else:
            self.emit("error", its, ("message", result["error"]))
        self.stream.flush()


@contextlib.contextmanager
def tracing(world, stream, rng_events=False):
    """Bind `current` to a Trace on STREAM for WORLD (None if STREAM is None)
    while the block runs, and tell it of WORLD's RNG draws."""
    global current
    saved = current
    current = Trace(stream, world, rng_events) if stream is not None else None
    world.rng.sink = oracle_note_draw
    try:
        yield current
    finally:
        current = saved


def oracle_note_draw(n, value):
    """oracle.lisp: oracle-note-draw.  A draw made by cr-choose goes into its
    event; any other one is an rng event (with :rng-events)."""
    t = current
    if t is None:
        return
    if t.choose_draws is not None:
        t.choose_draws.append([n, value])
    elif t.rng_events:
        t.emit("rng", ("n", n), ("value", value))


def _pnode(world, p):
    return world[p] if isinstance(p, Symbol) else p


# ---------------------------------------------------------------------------
# World inspection

def oracle_call_without_global_effects(world, thunk):
    """oracle.lisp: oracle-call-without-global-effects.  Call THUNK, then
    undo any change it made to a global's value.  temperature's
    collect-misfortune SETQs the global current-target, which look-for-blx
    reads; the trace must not change it."""
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

def oracle_begin_iteration(t, n):
    """oracle.lisp: oracle-begin-iteration."""
    t.flush_pending()
    world = t.world
    t.in_hook, in_hook = True, t.in_hook
    try:
        temperature = oracle_call_without_global_effects(
            world, lambda: codelets.temperature(world))
    finally:
        t.in_hook = in_hook
    t.pending = ([("ev", "iteration"), ("n", n), ("x", world[_X]),
                  ("temperature", encode_data(temperature)),
                  ("rack", _rack_field(world, world[_CODERACK]))]
                 + _form_fields(None, None))


def oracle_iteration_hook(fn, *args):
    """oracle.lisp: oracle-iteration-hook.  On mod and cr-empty-coderack:
    config calls one of them first in every main-loop iteration, right after
    (setq *iteration* y)."""
    t = current
    if t is not None and t.phase == "loop" and not t.in_hook:
        n = t.world[_ITERATION]
        if not franz._eql(n, t.last_iteration):
            t.last_iteration = n
            oracle_begin_iteration(t, n)
    return fn(*args)


def oracle_cr_empty_coderack_hook(fn, *args):
    """oracle.lisp: oracle-cr-empty-coderack-hook."""
    result = oracle_iteration_hook(fn, *args)
    if current is not None:
        current.emit("rack-emptied")
    return result


def oracle_config_hook(fn, *args):
    """oracle.lisp: oracle-config-hook.  config's set-up phase begins."""
    t = current
    if t is not None:
        t.phase, t.setup_chooses, t.last_iteration = "setup", 0, None
    try:
        return fn(*args)
    finally:
        if t is not None:
            t.phase = None


def oracle_cr_choose_hook(fn, world, name, full=False):
    """oracle.lisp: oracle-cr-choose-hook.  A setup-choose event in the
    set-up phase; in the main loop, the choice completes the pending
    iteration event."""
    t = current
    if t is None or t.phase is None:
        return fn(world, name, full)
    rack = _rack_field(world, name)
    saved, t.choose_draws = t.choose_draws, []
    try:
        res = fn(world, name, True)
        fields = _form_fields(res[0] if res else None, res[1] if res else None)
        if t.rng_events:
            fields.append(("rng", list(t.choose_draws)))
    finally:
        t.choose_draws = saved
    if t.phase == "setup":
        t.emit("setup-choose", *fields, ("rack", rack))
        t.setup_chooses += 1
        if t.setup_chooses == SETUP_CHOOSES:
            # The main loop starts only once config has evaluated this last
            # set-up codelet, which may itself call mod (compare-b-to-t ->
            # digits-in-common).  So hand config a form that evaluates the
            # codelet, then switches phase.
            t.phase = "last-setup"
            if res:
                res = [[_ORACLE_END_SETUP, [_QUOTE, res[0]]], res[1]]
            else:
                t.phase = "loop"
    elif t.phase == "loop" and t.pending is not None:
        e, t.pending = t.pending, None
        t.write_event([p for p in e if p[0] not in ("codelet", "args", "urgency")] + fields)
    else:
        raise RuntimeError(f"oracle: cr-choose in phase {t.phase} outside an iteration")
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
        current.emit("post", *_form_fields(form, urgency))
    return result


def oracle_create_cyto_node_hook(fn, world, activation, name, value, status, level, type,
                                 success):
    """oracle.lisp: oracle-create-cyto-node-hook.  Written before the call:
    in create-cyto-node nothing that makes an event comes before its "Node ~a
    created" line."""
    if current is not None:
        current.emit("node-created", *_node_fields(name, type, value))
    return fn(world, activation, name, value, status, level, type, success)


def oracle_create_op_node_hook(fn, world, name, res, op1, op2, activation, level):
    """oracle.lisp: oracle-create-op-node-hook.  create-op-node makes a "5g"
    node with no value."""
    if current is not None:
        current.emit("node-created", *_node_fields(name, "5g", None))
    return fn(world, name, res, op1, op2, activation, level)


def oracle_disconnect_hook(fn, world, cyto_node, op):
    """oracle.lisp: oracle-disconnect-hook.  disconnect always prints "Node
    <op> killed" then "Node <cyto-node> killed", with no event before them."""
    if current is not None:
        for node in (op, cyto_node):
            current.emit("node-killed", *_node_fields(send(world, node, "name"),
                                                      send(world, node, "type"),
                                                      send(world, node, "value")))
    return fn(world, cyto_node, op)


def oracle_spread_hook(fn, world, *args):
    """oracle.lisp: oracle-spread-hook.  The 88 activations after every
    spread-activation-in-pnet."""
    result = fn(world, *args)
    if current is not None:
        current.emit("pnet", ("act", [encode_data(send(world, _pnode(world, p), "activation"))
                                      for p in world[_PNET]]))
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
    is parsed into the done event's decomposition."""
    t = current
    if t is None or t.decompose_depth > 0:
        if t is None:
            return fn(world, node)
        t.decompose_depth += 1
        try:
            return fn(world, node)
        finally:
            t.decompose_depth -= 1
    capture = []
    out = world.out
    world.out = _Broadcast(out, capture)
    t.decompose_depth = 1
    try:
        result = fn(world, node)
    finally:
        world.out = out
        t.decompose_depth = 0
    t.decomposition = oracle_parse_decomposition("".join(capture))
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
    (start, "config", oracle_config_hook),
    (coderack, "cr_choose", oracle_cr_choose_hook),
    (coderack, "cr_hang", oracle_cr_hang_hook),
    (franz, "mod", oracle_iteration_hook),
    (coderack, "cr_empty_coderack", oracle_cr_empty_coderack_hook),
    (codelets, "create_cyto_node", oracle_create_cyto_node_hook),
    (codelets, "create_op_node", oracle_create_op_node_hook),
    (codelets, "disconnect", oracle_disconnect_hook),
    (pnet_functions, "spread_activation_in_pnet", oracle_spread_hook),
    (codelets, "decompose", oracle_decompose_hook),
]


def install():
    """oracle.lisp: oracle-install (the trace part).  Encapsulate the hooked
    functions, once; then the iteration cap, which must be outside the trace
    hooks, so that the iteration it stops at is never begun in the trace."""
    for module, name, hook in HOOKS:
        tags = getattr(getattr(module, name), "encapsulations", ())
        if "iteration-cap" in tags and "oracle" not in tags:
            raise RuntimeError(f"trace.install: the iteration cap would be inside the "
                               f"trace hook of {module.__name__}.{name}")
        harness.encapsulate(module, name, "oracle", hook)
    harness.install_iteration_cap()


world_module.EVAL_WORLD_FUNCTIONS[_ORACLE_END_SETUP] = oracle_end_setup

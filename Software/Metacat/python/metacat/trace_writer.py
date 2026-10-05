"""The JSON-lines trace of a run (docs/trace-format.md), written byte for byte.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
Translated to Python (2026) from chez_scheme/oracle/trace.ss (the oracle's trace)
and its counterpart in the Racket port, racket/trace.rkt.  metacat/trace.py is
trace.ss, the original's Temporal Trace, so this writer has another name.

`install_trace()` wraps, once per process, the engine procedures and objects that
trace.ss wraps with set!: the Coderack (each codelet it hands out), the build and
break procedures, the Workspace's add-rule, update-temperature,
update-slipnet-activations, the Trace window (the Temporal Trace's events) and
abstract-answer-description.  The engine reads them through their modules at call
time, as the original reads its top-level bindings.  Every wrapper only reads model
state through side-effect-free getters and calls the original with the original
arguments, so a traced run is the same run.  Events are written to `PORT` (a text
file, or None for no trace); `on_answer` and `on_halt` let the driver
(metacat/headless.py) print what run.ss prints.
"""
from __future__ import annotations

from fractions import Fraction

import metacat as _metacat
from metacat import chez, objects, setup
from metacat.objects import Lambda, SchemeObject, tell

NULL = object()            # the trace's 'null

PORT = None                # where emit writes; None: no trace
LAST_THEMES = [None]


class _Obj(list):
    """trace.ss: (json-object (key . value) ...), keys in the order given."""


# ---------------------------------------------------------------------------
# JSON output (trace.ss: json-string, json-number, json-write)

def _json_string(s, out):
    out.append('"')
    for c in s:
        if c == '"':
            out.append('\\"')
        elif c == "\\":
            out.append("\\\\")
        elif c == "\n":
            out.append("\\n")
        elif c == "\t":
            out.append("\\t")
        elif c < " ":
            out.append("\\u%04X" % ord(c))
        else:
            out.append(c)
    out.append('"')


def _json_number(x, out):
    if isinstance(x, int):
        out.append(str(x))
    elif isinstance(x, Fraction):
        _json_string("%d/%d" % (x.numerator, x.denominator), out)
    elif isinstance(x, float) and x == x and x not in (float("inf"), float("-inf")):
        out.append(chez.number_to_string(x).split("|")[0])
    else:
        _json_string(chez.number_to_string(x), out)


def json_write(v, out):
    """trace.ss: json-write (out is a list of string pieces)"""
    if v is True:
        out.append("true")
    elif v is False:
        out.append("false")
    elif v is NULL:
        out.append("null")
    elif isinstance(v, str):
        _json_string(v, out)
    elif isinstance(v, (int, Fraction, float)):
        _json_number(v, out)
    elif isinstance(v, _Obj):
        out.append("{")
        for i, (k, val) in enumerate(v):
            if i:
                out.append(",")
            _json_string(k, out)
            out.append(":")
            json_write(val, out)
        out.append("}")
    elif isinstance(v, (list, tuple)):
        out.append("[")
        for i, x in enumerate(v):
            if i:
                out.append(",")
            json_write(x, out)
        out.append("]")
    else:
        raise chez.SchemeError("trace", "cannot write ~s as JSON", v)


def emit(ev, *fields):
    """trace.ss: emit"""
    if PORT is not None:
        out = []
        json_write(_Obj([("t", setup.g_codelet_count), ("ev", ev)] + list(fields)), out)
        out.append("\n")
        PORT.write("".join(out))


# ---------------------------------------------------------------------------
# Names of model objects (trace.ss: $name, $string-of, $structure-fields)

def name(x):
    """trace.ss: $name"""
    if x is False:
        return NULL
    if isinstance(x, (str, int, Fraction, float)):
        return x
    if callable(x):
        t = tell(x, "object-type")
        if t == "slipnode":
            return tell(x, "get-short-name")
        if t in ("letter", "group"):
            n = tell(x, "ascii-name")
            return NULL if n is False else n
        if t == "workspace-string":
            return tell(x, "generic-name")
        if t == "concept-mapping":
            return tell(x, "print-name")
        return "<%s>" % chez.display_string(t)
    return chez.display_string(x)


def string_of(obj):
    """trace.ss: $string-of"""
    return tell(tell(obj, "get-string"), "generic-name")


def names(xs):
    # chez: map over pure getters; the order is not observable
    return [name(x) for x in xs]


def structure_fields(s):
    """trace.ss: $structure-fields"""
    t = tell(s, "object-type")
    if t == "bond":
        return [("string", string_of(s)),
                ("from", name(tell(s, "get-from-object"))),
                ("to", name(tell(s, "get-to-object"))),
                ("category", name(tell(s, "get-bond-category"))),
                ("direction", name(tell(s, "get-direction"))),
                ("facet", name(tell(s, "get-bond-facet")))]
    if t == "group":
        return [("string", string_of(s)),
                ("name", name(s)),
                ("category", name(tell(s, "get-group-category"))),
                ("direction", name(tell(s, "get-direction"))),
                ("facet", name(tell(s, "get-bond-facet"))),
                ("objects", names(tell(s, "get-constituent-objects")))]
    if t == "bridge":
        return [("type", tell(s, "get-bridge-type")),
                ("object1", name(tell(s, "get-object1"))),
                ("object2", name(tell(s, "get-object2"))),
                ("mappings", names(tell(s, "get-all-concept-mappings")))]
    if t == "description":
        obj = tell(s, "get-object")
        return [("string", string_of(obj)),
                ("object", name(obj)),
                ("type", name(tell(s, "get-description-type"))),
                ("descriptor", name(tell(s, "get-descriptor")))]
    if t == "rule":
        return [("type", tell(s, "get-rule-type")),
                ("english", list(tell(s, "get-english-transcription")))]
    return [("object", name(s))]


def emit_structure(ev, kind, s, *extra):
    """trace.ss: emit-structure"""
    if PORT is not None:
        emit(ev, ("kind", kind), *structure_fields(s), *extra)


# ---------------------------------------------------------------------------
# The wrappers (trace.ss: install-trace!)

class _Wrapped(SchemeObject):
    """trace.ss's (lambda msg ... (apply original original (cdr msg))): self inside
    stays the original object."""

    def __init__(this, original, before=None, after=None, watch=None):
        this.original = original
        this.before = before
        this.after = after
        this.watch = watch      # the one message before/after act on (speed, item 12)

    def otherwise(this, self, msg, args):
        if msg != this.watch:
            return this.original(this.original, msg, *args)
        if this.before:
            this.before(msg, args)
        result = this.original(this.original, msg, *args)
        if this.after:
            this.after(msg, result)
        return result


def _codelet_chosen(msg, result):
    if msg == "choose-codelet" and PORT is not None:
        emit("codelet",
             ("type", tell(result, "get-codelet-type-name")),
             ("urgency", tell(result, "get-relative-urgency")),
             ("posted", tell(result, "get-time-stamp")),
             ("rng", chez.random_seed()))


def _rule_added(msg, args):
    if msg == "add-rule" and PORT is not None:
        emit_structure("build", "rule", args[0])


def on_answer(answer_event):
    """run.ss's own wrapper of abstract-answer-description (the driver sets it)."""


def on_halt(message, obj):
    """run.ss's report-error-and-halt (the driver sets it; it must not return)."""
    raise NotImplementedError


_installed = False


def install_trace():
    """trace.ss: install-trace!, and the oracle run.ss's wrappers of
    abstract-answer-description and report-error-and-halt, which call on_answer
    and on_halt.  Once per process."""
    global _installed
    if _installed:
        return
    _installed = True
    m = _metacat
    m.coderack.g_coderack = _Wrapped(m.coderack.g_coderack, after=_codelet_chosen,
                                     watch="choose-codelet")

    o_build_bond, o_break_bond = m.bonds.build_bond, m.bonds.break_bond
    o_build_group, o_break_group = m.groups.build_group, m.groups.break_group
    o_build_bridge, o_break_bridge = m.bridges.build_bridge, m.bridges.break_bridge
    o_build_description = m.descriptions.build_description

    def build_bond(bond):
        emit_structure("build", "bond", bond)
        return o_build_bond(bond)

    def break_bond(bond):
        emit_structure("break", "bond", bond)
        return o_break_bond(bond)

    def build_group(group, flipped_p):
        emit_structure("build", "group", group, ("flipped", flipped_p))
        return o_build_group(group, flipped_p)

    def break_group(group):
        emit_structure("break", "group", group)
        return o_break_group(group)

    def build_bridge(orientation, bridge):
        emit_structure("build", "bridge", bridge)
        return o_build_bridge(orientation, bridge)

    def break_bridge(bridge):
        emit_structure("break", "bridge", bridge)
        return o_break_bridge(bridge)

    def build_description(d):
        emit_structure("build", "description", d)
        return o_build_description(d)

    m.bonds.build_bond, m.bonds.break_bond = build_bond, break_bond
    m.groups.build_group, m.groups.break_group = build_group, break_group
    m.bridges.build_bridge, m.bridges.break_bridge = build_bridge, break_bridge
    m.descriptions.build_description = build_description
    m.workspace.g_workspace = _Wrapped(m.workspace.g_workspace, before=_rule_added,
                                     watch="add-rule")

    o_update_temperature = m.formulas.update_temperature

    def update_temperature():
        o_update_temperature()
        emit("temperature", ("value", setup.g_temperature),
             ("clamped", m.run.g_temperature_clamped_p))

    m.formulas.update_temperature = update_temperature

    o_update_slipnet_activations = m.slipnet.update_slipnet_activations

    def update_slipnet_activations():
        o_update_slipnet_activations()
        if PORT is None:
            return None
        emit("slipnet",
             ("activations", [tell(n, "get-activation") for n in m.slipnet.g_slipnet_nodes]),
             ("rng", chez.random_seed()))
        state = tell(m.themes.g_themespace, "get-complete-state")
        fields = [("active", list(state[1])),
                  ("themes", [[info[0], name(info[1]), name(info[2]), info[3], info[4]]
                              for info in state[2]])]
        if fields != LAST_THEMES[0]:
            LAST_THEMES[0] = fields
            emit("themes", *fields)

    m.slipnet.update_slipnet_activations = update_slipnet_activations

    wrap_trace_window()

    o_abstract_answer_description = m.memory.abstract_answer_description

    def abstract_answer_description(answer_event):
        emit("answer",
             ("answer", tell(tell(answer_event, "get-answer-string"), "print-name")),
             ("quality", tell(answer_event, "get-quality")),
             ("temperature", setup.g_temperature))
        _metacat.trace_writer.on_answer(answer_event)
        return o_abstract_answer_description(answer_event)

    m.memory.abstract_answer_description = abstract_answer_description

    def report_error_and_halt(message, obj):
        emit("halt", ("message", message[1]), ("object", tell(obj, "object-type")))
        return _metacat.trace_writer.on_halt(message, obj)

    objects.report_error_and_halt = report_error_and_halt



def wrap_trace_window():
    """trace.ss: install-trace!'s wrapper of *trace-window* (each add-event is
    emitted), around whichever Trace window is installed: the headless one at
    install time, or the views' (headless.run_problem's views).  Returns the
    wrapper."""
    window = setup.g_trace_window

    def trace_window_fn(self, msg, *args):
        if msg == "add-event" and PORT is not None:
            event = args[0]
            emit("event",
                 ("type", tell(event, "get-type")),
                 ("number", tell(event, "get-event-number")),
                 ("name", tell(event, "print-name")),
                 ("time", tell(event, "get-time")),
                 ("temperature", tell(event, "get-temperature")))
        return window(self, msg, *args)

    setup.g_trace_window = Lambda(trace_window_fn)
    return setup.g_trace_window


def trace_start(strings, seed, max_codelets, keep_going_p):
    """trace.ss: trace-start"""
    LAST_THEMES[0] = None
    emit("start", ("format", 1),
         ("problem", list(strings) + ([NULL] if len(strings) == 3 else [])),
         ("seed", seed), ("max_codelets", NULL if max_codelets is False else max_codelets),
         ("keep_going", keep_going_p),
         ("slipnodes", names(_metacat.slipnet.g_slipnet_nodes)))


def trace_end(reason, answers):
    """trace.ss: trace-end"""
    emit("end", ("reason", reason), ("temperature", setup.g_temperature),
         ("answers", list(answers)), ("rng", chez.random_seed()))
    if PORT is not None:
        PORT.flush()

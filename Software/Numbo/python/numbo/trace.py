"""The JSON-lines trace of a run, as lisp/src/oracle.lisp writes it.

Port of the trace part of lisp/src/oracle.lisp (sections "JSON", "Run state",
"Hooks" and oracle-run-config's events).  The events, their fields and the
Lisp-data encoding are specified in that file's header and in
PORTING_NOTES.md, "Oracle hooks"; one event per line, as a JSON object whose
keys come in the oracle's order.

As in the oracle, no ported function writes an event itself.  Since
loop0003 the oracle's hooks are events.py's, and publish typed events
(observe.py); the trace is OracleTraceWriter, an observer of them, which
writes the oracle's subset: one line per event, except that an iteration's
line also holds the codelet then chosen (the oracle's *oracle-pending*), a
disconnect is two node-killed lines, the done line holds the decomposition,
and the events the oracle has no line for (node changes and kills, the
current target, the decomposition itself) are not written.
"""

import dataclasses
import json

from numbo import observe
from numbo.events import encode_data, oracle_parse_decomposition, solution_tokens

__all__ = ["OracleTraceWriter", "encode_data", "oracle_parse_decomposition",
           "solution_tokens"]

_CODELET_FIELDS = ("codelet", "args", "urgency")


class OracleTraceWriter:
    """The oracle's JSON-lines trace on STREAM, from a run's events; with
    RNG_EVENTS, the RNG draws too (rng events, and each choice's own)."""

    def __init__(self, stream, rng_events=False):
        self.stream = stream
        self.rng_events = rng_events
        self.pending = None

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

    def _codelet(self, e):
        fields = [("codelet", e.codelet), ("args", e.args), ("urgency", e.urgency)]
        if self.rng_events:
            fields.append(("rng", e.draws))
        return fields

    def on_event(self, e):
        handler = _HANDLERS.get(type(e))
        if handler is not None:
            handler(self, e)

    def start(self, e):
        self.emit("start", ("problem", e.problem), ("seed", e.seed),
                  ("max_iterations", e.max_iterations), ("rng", e.rng), ("pnet", e.pnet))

    def rng(self, e):
        if self.rng_events:
            self.emit("rng", ("n", e.n), ("value", e.value))

    def setup_choose(self, e):
        self.emit("setup-choose", *self._codelet(e), ("rack", e.rack))

    def iteration(self, e):
        """oracle.lisp: oracle-begin-iteration.  The line waits for the
        iteration's codelet, or for the next line."""
        self.flush_pending()
        self.pending = ([("ev", "iteration"), ("n", e.n), ("x", e.x),
                         ("temperature", e.temperature), ("rack", e.rack)]
                        + [(f, None) for f in _CODELET_FIELDS])

    def codelet_chosen(self, e):
        """oracle.lisp: oracle-cr-choose-hook, in the main loop."""
        if self.pending is None:
            raise RuntimeError("oracle: cr-choose in the main loop outside an iteration")
        p, self.pending = self.pending, None
        self.write_event([f for f in p if f[0] not in _CODELET_FIELDS] + self._codelet(e))

    def post(self, e):
        self.emit("post", ("codelet", e.codelet), ("args", e.args), ("urgency", e.urgency))

    def node_created(self, e):
        self.emit("node-created", ("name", e.name), ("type", e.type), ("value", e.value))

    def op_node_created(self, e):
        self.emit("node-created", ("name", e.name), ("type", "5g"), ("value", None))

    def disconnect(self, e):
        self.emit("node-killed", ("name", e.op), ("type", e.op_type), ("value", e.op_value))
        self.emit("node-killed", ("name", e.node), ("type", e.node_type),
                  ("value", e.node_value))

    def pnet(self, e):
        self.emit("pnet", ("act", e.activations))

    def rack_emptied(self, e):
        self.emit("rack-emptied")

    def run_ended(self, e):
        """oracle-run-config's last event."""
        its = ("iterations", e.iterations)
        if isinstance(e, observe.Solved):
            self.emit("done", its, ("decomposition",
                                    [dataclasses.asdict(s) for s in e.decomposition]))
        elif isinstance(e, observe.RunError):
            self.emit("error", its, ("message", e.message))
        else:
            self.emit(e.kind, its)
        self.stream.flush()


_HANDLERS = {
    observe.RunStarted: OracleTraceWriter.start,
    observe.RngDraw: OracleTraceWriter.rng,
    observe.SetupChoose: OracleTraceWriter.setup_choose,
    observe.IterationBegan: OracleTraceWriter.iteration,
    observe.CodeletChosen: OracleTraceWriter.codelet_chosen,
    observe.CodeletPosted: OracleTraceWriter.post,
    observe.NodeCreated: OracleTraceWriter.node_created,
    observe.OpNodeCreated: OracleTraceWriter.op_node_created,
    observe.Disconnect: OracleTraceWriter.disconnect,
    observe.PnetActivations: OracleTraceWriter.pnet,
    observe.RackEmptied: OracleTraceWriter.rack_emptied,
    observe.Solved: OracleTraceWriter.run_ended,
    observe.GaveUp: OracleTraceWriter.run_ended,
    observe.Capped: OracleTraceWriter.run_ended,
    observe.RunError: OracleTraceWriter.run_ended,
}

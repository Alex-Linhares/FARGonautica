"""The 1987 global environment: symbol values, and eval of the few forms the
source evaluates.

Port of the globals that src/globals.lisp proclaims (and the free ones it
deliberately leaves out, NODE and RES; PORTING_NOTES.md, "Compile census").
The 1987 code keeps its whole state in global symbols: the parameters
(%k%, %first-threshold%, ...), *pnet*, *iteration*, the 91 pnode holders
(node-5, result+, ...), and later the cytoplasm's nodes, which it reaches
with (eval symbol).  A World holds those values, keyed by Symbol, so that
`world[intern("NODE-5")]` is the Lisp `node-5` (or `(eval 'node-5)`), and
the free SETQs of NODE and RES in pnet-functions.lisp set
`world[intern("NODE")]`, never a Python local.

Symbol property lists ((get symbol indicator), (setf (get ...))) are
`get_prop`/`put_prop`; the coderack lives on its name's plist.

A World also holds what the Lisp gets from its process: `out`, the stream
`(format t ...)` writes to; `rng`, the shared RNG of oracle mode
(src/oracle.lisp, state 0 until seeded); and `cr_hang`, the function
populate-coderack posts with (coderack.lisp: cr-hang).  Tests may replace
`world.cr_hang` with a recorder.
"""

import sys

from numbo import coderack, franz
from numbo.franz import Symbol, intern
from numbo.rng import Rng

T = intern("T")
NIL = intern("NIL")


class World:
    """The global symbol values of one Numbo process (globals.lisp)."""

    def __init__(self):
        self.values = {}
        self.plists = {}
        self.out = sys.stdout
        self.rng = Rng(0)

    def cr_hang(self, name, form, urgency):
        """coderack.lisp: cr-hang, on this World."""
        return coderack.cr_hang(self, name, form, urgency)

    def get_prop(self, symbol, indicator):
        """(get symbol indicator): nil (None) if absent."""
        return self.plists.get(symbol, {}).get(indicator)

    def put_prop(self, symbol, value, indicator):
        """(setf (get symbol indicator) value)."""
        assert isinstance(symbol, Symbol), symbol
        self.plists.setdefault(symbol, {})[indicator] = value
        return value

    def __getitem__(self, symbol):
        """symbol-value: an unbound symbol is an error, as in Lisp."""
        try:
            return self.values[symbol]
        except KeyError:
            # SBCL's UNBOUND-VARIABLE report, which a run's "error" event
            # carries (e.g. puzzle 1, oracle seed 323).
            raise NameError(f"The variable {symbol.name} is unbound.") from None

    def __setitem__(self, symbol, value):
        """setq of a global."""
        assert isinstance(symbol, Symbol), symbol
        self.values[symbol] = value

    def __delitem__(self, symbol):
        """makunbound."""
        del self.values[symbol]

    def __contains__(self, symbol):
        """boundp."""
        return symbol in self.values


class OutputStream:
    """A text stream that knows whether it is at the start of a line, as an
    SBCL stream does for (format t "~&...") (fresh-line)."""

    def __init__(self, stream, at_line_start=True):
        self.stream = stream
        self.at_line_start = at_line_start

    def write(self, text):
        if text:
            self.at_line_start = text.endswith("\n")
        return self.stream.write(text)

    def flush(self):
        return self.stream.flush()


def fresh_line(out):
    """CL fresh-line (format's ~&): a newline unless OUT is at the start of a
    line.  A plain stream is taken to be at a line start unless it can show
    its text (io.StringIO)."""
    at_line_start = getattr(out, "at_line_start", None)
    if at_line_start is None:
        getvalue = getattr(out, "getvalue", None)
        at_line_start = getvalue is None or getvalue()[-1:] in ("", "\n")
    if not at_line_start:
        out.write("\n")


def _add(*numbers):
    """franz-compat.lisp: add (CL +, folded left as CL does)."""
    result = 0
    for x in numbers:
        result = result + x
    return result


def _minus(x):
    """franz-compat.lisp: minus."""
    return -x


# The functions evaluated forms call: the thresholds that
# (pnode :modify-threshold) builds are (max n (add i (minus *iteration*) u)).
EVAL_FUNCTIONS = {
    intern("MAX"): franz.max_,
    intern("ADD"): _add,
    intern("MINUS"): _minus,
}

# The functions of the World that evaluated forms call, f(world, *args):
# the codelets, whose forms config evaluates ((eval (cr-choose *coderack*))).
# codelets.py fills it when it is imported.
EVAL_WORLD_FUNCTIONS = {}

QUOTE = intern("QUOTE")


def lisp_eval(world, form):
    """CL eval, for the data the 1987 source evaluates: a symbol is its value
    in WORLD (t, nil and keywords are themselves), a number, string or
    object is itself, (quote x) is x, and any other list is a call of one of
    EVAL_WORLD_FUNCTIONS (with WORLD) or EVAL_FUNCTIONS on its evaluated
    arguments, in order."""
    if isinstance(form, Symbol):
        if form is T:
            return True
        if form is NIL:
            return None
        if form.package == "KEYWORD":
            return form
        return world[form]
    if isinstance(form, list) and form:
        if form[0] is QUOTE:
            return form[1]
        world_fn = EVAL_WORLD_FUNCTIONS.get(form[0])
        if world_fn is not None:
            return world_fn(world, *[lisp_eval(world, arg) for arg in form[1:]])
        fn = EVAL_FUNCTIONS.get(form[0])
        if fn is None:
            raise NotImplementedError(f"lisp_eval: no function {form[0]!r}")
        return fn(*[lisp_eval(world, arg) for arg in form[1:]])
    if form is None or (isinstance(form, list) and not form):
        return None
    return form

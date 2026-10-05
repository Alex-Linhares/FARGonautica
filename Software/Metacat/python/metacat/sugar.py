"""syntactic-sugar.ss: Metacat's macros and the definitions around them.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026) from syntactic-sugar.ss, with
racket/compat.rkt (its syntax-rules versions) as a worked translation.

Each of the 22 extend-syntax forms becomes a Python function; the bodies and
expressions a macro would not evaluate yet are passed as thunks.  A form with
several patterns becomes one function per pattern (for* each / for* from-to,
repeat* times / forever / until, the all-lengths: forms of category-link* and
instance-link*).  Model code may also write a form inline as plain Python, as
long as it keeps the form's order of evaluation:
  - (for* each x in l do body ...)   -> for x in l: body   (the for_star
    functions only where the value, the last body's, is used)
  - (if* test exp ...)                -> if test: exp
  - (stochastic-if* p exp ...)        -> coin = chez.random(1.0)
                                         if coin < p: exp
    chez: the coin is drawn *before* p is evaluated (stochastic_if_star).
  - (continuation-point* k exp ...)   -> continuation_point_star(lambda k: ...)

The names a macro leaves free are the engine's globals where it is used:
%verbose% and *control-panel* (setup.ss), *coderack* and make-codelet-type
(coderack.ss), make-slipnode and establish-link (slipnet.ss).  They are read
from those modules at call time (metacat.setup.p_verbose ...), so a set! of
%verbose% by the GUI reaches say.  The engine never imports tkinter.
"""
from __future__ import annotations

import metacat as _metacat
from metacat import chez
from metacat.objects import tell

# random seeds cannot be bigger than this number in Chez Scheme
g_largest_random_seed = 4294967295


def concatenate_symbols(*symbols):
    """syntactic-sugar.ss: concatenate-symbols"""
    return "".join(symbols)


def printf(control, *args):
    """syntactic-sugar.ss: printf.  The original captures the output port when
    the file is loaded; Python writes to the sys.stdout current at the call (as
    racket/compat.rkt does), so that tests and the GUI can redirect it."""
    return chez.printf(control, *args)


def newline():
    """syntactic-sugar.ss: newline (to the current sys.stdout, like printf)"""
    return chez.newline()


# ---------------------------------------------------------------------------
# Utility macros

def mcat(*tokens):
    """syntactic-sugar.ss: mcat.  (mcat abc abd xyz [wyz] [seed]).  The
    original's (valid-token-list? '(token ...)) is a fender: invalid tokens
    match no pattern, a syntax error (fixtures mcat-bad-*)."""
    tokens = list(tokens)
    # 1.2: the validity test is an extend-syntax fender, so bad tokens are a syntax error
    if not valid_token_list_p(tokens):
        raise chez.SchemeError("mcat", "invalid syntax ~s", ["mcat"] + tokens)
    return tell(_metacat.setup.g_control_panel, "run-new-problem", tokens)


def _list_p(x) -> bool:
    t = type(x)
    return t is list or t is tuple


def _symbol_p(x) -> bool:
    return isinstance(x, str) and not isinstance(x, (chez.String, chez.Char))


def valid_token_list_p(tokens):
    """syntactic-sugar.ss: valid-token-list?
    Formats: (sym sym sym) (sym sym sym sym) (sym sym sym num) (sym sym sym sym num)"""
    return (_list_p(tokens)
            and len(tokens) >= 3
            and _symbol_p(tokens[0])
            and _symbol_p(tokens[1])
            and _symbol_p(tokens[2])
            and ((len(tokens) == 3)
                 or (len(tokens) == 4 and symbol_or_valid_number_p(tokens[3]))
                 or (len(tokens) == 5 and _symbol_p(tokens[3]) and valid_number_p(tokens[4]))))


def valid_number_p(num):
    """syntactic-sugar.ss: valid-number?"""
    return chez.integer_p(num) and num > 0 and num <= g_largest_random_seed


def symbol_or_valid_number_p(token):
    """syntactic-sugar.ss: symbol-or-valid-number?"""
    return _symbol_p(token) or valid_number_p(token)


def for_star(f, *lists):
    """syntactic-sugar.ss: for* (the `each` patterns).
    (for* each x in l do body ...) and (for* each (x y) in (l1 l2) do body ...)
    are (for-each (lambda (x ...) body ...) l ...): first to last, and the
    value is the last application's (void for an empty list)."""
    # chez: for-each's value is the last application's (anomalies: "sort, remq, for-each and one-armed if differ")
    return chez.for_each(f, *lists)


def for_star_from_to(exp1_value, exp2_value, f):
    """syntactic-sugar.ss: for* (the from/to pattern).
    (for* i from exp1 to exp2 do body ...): i runs from exp1 to exp2 inclusive,
    none if exp2 < exp1.  chez: the caller evaluates exp1 before exp2 (they are
    let bindings; fixture for*-bounds-order)."""
    if exp2_value < exp1_value:
        values = []
    else:
        values = chez.map_(lambda n: chez.add(n, exp1_value),
                           _ascending_index_list(chez.add1(chez.sub(exp2_value, exp1_value))))
    return chez.for_each(f, values)


def _ascending_index_list(n):
    # utilities.ss's ascending-index-list, which the macros call
    return _metacat.utilities.ascending_index_list(n)


def for_each_vector_element_star(v, f):
    """syntactic-sugar.ss: for-each-vector-element*.
    (for-each-vector-element* (v i) do exp ...): i over v's indices.
    1.2: on an empty vector this loops forever, as (ascending-index-list 0)
    does (porting-notes.md, item 03)."""
    return chez.for_each(f, _ascending_index_list(len(v)))


def for_each_table_element_star(t, f):
    """syntactic-sugar.ss: for-each-table-element*.
    (for-each-table-element* (t i j) do exp ...): row by row."""
    return for_each_vector_element_star(
        t, lambda i: chez.for_each(lambda j: f(i, j), _ascending_index_list(len(t[i]))))


def repeat_star_times(n, thunk):
    """syntactic-sugar.ss: repeat* (n times).  The value is void."""
    i = n
    while i > 0:
        thunk()
        i = chez.sub1(i)
    return None


def repeat_star_forever(thunk):
    """syntactic-sugar.ss: repeat* (forever); only an escape ends it."""
    while True:
        thunk()


def repeat_star_until(done, thunk):
    """syntactic-sugar.ss: repeat* (until condition).  done is tested before
    every repetition; the value is void."""
    while not done():
        thunk()
    return None


def if_star(test, thunk):
    """syntactic-sugar.ss: if*.  (if test (begin exp ...) (void)); test is a
    Scheme boolean or object-or-#f (chez: only #f is false)."""
    if test is not False:
        return thunk()
    return None


def stochastic_if_star(prob_thunk, exps_thunk):
    """syntactic-sugar.ss: stochastic-if*.  Exactly one (random 1.0), drawn
    *before* the probability expression is evaluated (it may draw or read state
    itself); the expressions run if the coin is below it."""
    # chez: the coin first, then the probability (anomalies: "Chez doesn't evaluate arguments left to right"; plan, "Evaluation order")
    coin_flip = chez.random(1.0)
    if coin_flip < prob_thunk():
        return exps_thunk()
    return None


class _Escape(Exception):
    """An escape from continuation_point_star: carries its form's token."""

    def __init__(self, token, value):
        super().__init__()
        self.token = token
        self.value = value


def continuation_point_star(body):
    """syntactic-sugar.ss: continuation-point*.  (call/cc (lambda (name) exp ...))
    used only as an escape upwards: body receives the escape procedure, and
    calling it returns its argument from this form.  Each form catches only its
    own escapes; calling an escape after the form has returned is an error, as
    with call/ec (run.ss's break/go, the one re-entry, is handled by the run
    loop: docs/python-translation-plan.md, "Continuations")."""
    token = object()
    active = [True]

    def escape(value=None):
        if not active[0]:
            raise chez.SchemeError("continuation-point*", "escape after its form returned")
        raise _Escape(token, value)
    try:
        return body(escape)
    except _Escape as e:
        if e.token is token:
            return e.value
        raise
    finally:
        active[0] = False


def say(*xs):
    """syntactic-sugar.ss: say.  If %verbose%, say-object each x, then newline."""
    if _metacat.setup.p_verbose is not False:
        for x in xs:
            _metacat.utilities.say_object(x)
        return newline()
    return None


def say_bang(*xs):
    """syntactic-sugar.ss: say!  say-object each x, then newline, always."""
    for x in xs:
        _metacat.utilities.say_object(x)
    return newline()


def vprintf(*args):
    """syntactic-sugar.ss: vprintf.  printf if %verbose%."""
    if _metacat.setup.p_verbose is not False:
        return printf(*args)
    return None


def vprint(*args):
    """syntactic-sugar.ss: vprint.  print if %verbose%."""
    if _metacat.setup.p_verbose is not False:
        return _metacat.utilities.print_(*args)
    return None


# ---------------------------------------------------------------------------
# Slipnet macros

def _define_each(names, values, module):
    for name, value in zip(names, values):
        chez.define_top_level_value(name, value)
        if module is not None:
            from metacat.names import scheme_to_python
            setattr(module, scheme_to_python(name), value)


def slipnet_node_list_star(specs, module=None):
    """syntactic-sugar.ss: slipnet-node-list*.
    (slipnet-node-list* (n s conceptual-depth: d) ...) with specs = [(n, s, d), ...]:
    defines each n as (make-slipnode 'n s d), in order, and returns the list of
    nodes.  The slipnet module passes itself as `module` so that each node is
    also its attribute (plato-a -> slipnet.plato_a)."""
    nodes = []
    for n, s, d in specs:
        node = _metacat.slipnet.make_slipnode(n, s, d)
        _define_each([n], [node], module)
        nodes.append(node)
    return nodes


def slipnet_layout_table_star(rows):
    """syntactic-sugar.ss: slipnet-layout-table*.  (slipnet-layout-table* (n ...) ...)
    is the table of rows, rotated 90 degrees clockwise."""
    return _metacat.utilities.rotate_90_degrees_clockwise(chez.Vector([chez.Vector(r) for r in rows]))


def _link(n1, n2, link_type, length=None, label=None):
    """The body every link macro expands to: establish the link n1-n2-link
    between plato-n1 and plato-n2, then set its length and/or label node, in
    that order.  The nodes are the top-level values of the plato- names."""
    top_level_name = concatenate_symbols(n1, "-", n2, "-link")
    n1_node = chez.top_level_value(concatenate_symbols("plato-", n1))
    n2_node = chez.top_level_value(concatenate_symbols("plato-", n2))
    result = _metacat.slipnet.establish_link(top_level_name, n1_node, n2_node, link_type)
    if length is not None:
        result = tell(chez.top_level_value(top_level_name), "set-link-length", length)
    if label is not None:
        result = tell(chez.top_level_value(top_level_name), "set-label-node",
                      chez.top_level_value(concatenate_symbols("plato-", label)))
    return result


def category_link_star(n1, n2, length):
    """syntactic-sugar.ss: category-link*.  (category-link* n1 --> n2 length: len)"""
    return _link(n1, n2, "category", length=length)


def category_links_star(instances, c, length):
    """syntactic-sugar.ss: category-link*.  (category-link* (i ...) --> c all-lengths: len)"""
    result = None
    for i in instances:
        result = category_link_star(i, c, length)
    return result


def instance_link_star(n1, n2, length):
    """syntactic-sugar.ss: instance-link*.  (instance-link* n1 --> n2 length: len)"""
    return _link(n1, n2, "instance", length=length)


def instance_links_star(c, instances, length):
    """syntactic-sugar.ss: instance-link*.  (instance-link* c --> (i ...) all-lengths: len)"""
    result = None
    for i in instances:
        result = instance_link_star(c, i, length)
    return result


def property_link_star(n1, n2, length):
    """syntactic-sugar.ss: property-link*.  (property-link* n1 --> n2 length: len)"""
    return _link(n1, n2, "property", length=length)


def lateral_link_star(n1, n2, length=None, label=None, two_way=False):
    """syntactic-sugar.ss: lateral-link*.  (lateral-link* n1 --> n2 length: len),
    (... label: n3), (... length: len label: n3); <--> (two_way) makes n1-n2
    then n2-n1."""
    result = _link(n1, n2, "lateral", length, label)
    if two_way:
        result = _link(n2, n1, "lateral", length, label)
    return result


def lateral_sliplink_star(n1, n2, length=None, label=None, two_way=False):
    """syntactic-sugar.ss: lateral-sliplink*.  (lateral-sliplink* n1 --> n2 label: n3)
    or (... length: len); <--> (two_way) makes n1-n2 then n2-n1."""
    result = _link(n1, n2, "lateral-sliplink", length, label)
    if two_way:
        result = _link(n2, n1, "lateral-sliplink", length, label)
    return result


# ---------------------------------------------------------------------------
# Codelet macros

def codelet_type_list_star(specs, module=None):
    """syntactic-sugar.ss: codelet-type-list*.
    (codelet-type-list* (name label ...) ...) with specs = [(name, [label, ...]), ...]:
    defines each name as (make-codelet-type 'name (list label ...)), in order,
    and returns the list of types.  The coderack module passes itself as
    `module` so that each type is also its attribute."""
    types = []
    for name, labels in specs:
        codelet_type = _metacat.coderack.make_codelet_type(name, list(labels))
        _define_each([name], [codelet_type], module)
        types.append(codelet_type)
    return types


def post_codelet_star(rel_urg, codelet_type, *args):
    """syntactic-sugar.ss: post-codelet*.  (post-codelet* urgency: u type arg ...)"""
    return tell(_metacat.coderack.g_coderack, "post", tell(codelet_type, "make-codelet", rel_urg, *args))


fizzle = False


def define_codelet_procedure_star(codelet_type_name, proc):
    """syntactic-sugar.ss: define-codelet-procedure*.
    (define-codelet-procedure* type (lambda formals exp ...)): gives the
    codelet type (the top-level value of codelet_type_name) a procedure that
    runs proc inside an escape.  Codelet code calls sugar.fizzle() (always
    qualified: every codelet rebinds it) to end the codelet with 'done."""
    def codelet_procedure(*formals):
        def body(ret):
            global fizzle

            def fizzle_now():
                """syntactic-sugar.ss: define-codelet-procedure* (the codelet's fizzle)"""
                global fizzle
                fizzle = False
                return ret("done")
            fizzle = fizzle_now
            say("----------------------------------------------")
            say("In ", codelet_type_name, " codelet...")
            return proc(*formals)
        return continuation_point_star(body)
    return tell(chez.top_level_value(codelet_type_name), "set-codelet-procedure", codelet_procedure)


# The macros call utilities.ss procedures where they are used; make sure the
# module is loaded (it imports this one: a cycle Python resolves).
from metacat import utilities as _utilities  # noqa: E402,F401

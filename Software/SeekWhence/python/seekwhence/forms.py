"""The form language: s-expressions over the eight ideal types [M86 pp. 49-58].

"There are eight non-atomic ideal types, each of which is associated with a format having
one or more active parameters: (typename start-value length actual-value)" [M86 p. 49]. A
*form* is an instantiation of a format, written as an s-expression: `(Countup 3)`,
`(S-group 1 (Countup 2))`, `(Cycle 3 2 (3 7))`, `(Tuple (2 (Countup 1) shared))`. This
module holds the representation (`Form`, with the four slots of DECISION D-001, and `Shared`,
the marker of DECISION D-002), the parser and the printer. What a form *does* when hit, and
the types themselves, are in `seekwhence.platoplasm`.

Two spellings are read. The prose spelling of ch. 2 writes a fence form without its start and
length, `(Cycle (2 1 8))` [M86 p. 53], marks optional parameters with square brackets,
`(Y-group [1] [3] (1 2 1))` [M86 p. 81], and writes a shared substructure as the word `shared`
[M86 p. 56]. The program spelling of the transcripts and the appendix writes the start and
length of a fence form, `(Cycle 3 2 (3 7))` [M86 p. 166], and the appendix's `build-box`
writes a shared substructure as `(same pstrucN)` with a binding list [M86 pp. 161-163]. Both
parse to the same `Form`, and the printer gives the program spelling by default.
"""

from dataclasses import dataclass

from seekwhence.config import IDEAL_TYPES

# The types whose format is (type actual), with the actual value a list of elements: the
# fence types and the symmetry group [M86 pp. 52-53]. The other five take a start (and the
# three group types a length) [M86 pp. 50-52].
LIST_TYPES = ("Y-group", "Cycle", "Tuple")
GROUP_TYPES = ("C-group", "S-group", "P-group")
REPEATER_TYPES = ("Constant", "Countup")


@dataclass(frozen=True)
class Shared:
    """A place in a form that is the same structure as an earlier argument (DECISION D-002).

    "Shared structures are indicated by the word 'shared', with an arrow pointing to the first
    instance of the structure to be shared" [M86 p. 56]. The arrow is `index`: the position,
    in the same argument list, of the structure shared. In a Y-group, Cycle or Tuple the
    argument list is the list of elements, so `(Tuple (2 (Countup 1) shared))` [M86 p. 58]
    holds `Shared(0)` in its third place (the arrow on p. 58 points to the 2) and
    `(Cycle (8 (Countup 1) shared))` [M86 p. 57] holds `Shared(1)` (the arrow points to the
    Countup). In a group the argument list is (start, length), so a shared length, as in
    `(C-group (Countup 1) shared)` [M86 p. 161], is `Shared(0)`. An arrow into a nested
    structure, as on p. 84, where the length of `(C-group (S-group 2 3) shared)` shares the
    2 inside the S-group, is a path: `Shared((0, 0))`, argument 0 of argument 0.
    """

    index: int | tuple

    @property
    def path(self) -> tuple:
        """The arrow as a path of argument positions, one per level [M86 p. 84]."""
        return (self.index,) if isinstance(self.index, int) else tuple(self.index)


@dataclass(frozen=True)
class Wildcard:
    """The template wildcard `n`: a parameter of a group form loosened by the
    Template-applier, "where 'n' means 'any nonnegative integer'" [M86 p. 74], as in the
    template `(S-group 1 n)` [M86 pp. 73-74]. Only a template form holds one (a hypothesis
    form never does: templates lack "the predictive ability and expressive power of
    hypotheses" [M86 p. 73]), so a form with a wildcard cannot be hit; the template fit is
    `seekwhence.templates`' business. It prints and parses as `n`."""

    def __str__(self):
        return "n"


WILD = Wildcard()


def is_wild(x) -> bool:
    """Whether x is the wildcard `n` of a template form [M86 p. 74]."""
    return isinstance(x, Wildcard)


def has_wildcard(form) -> bool:
    """Whether a form has a loosened parameter, as the template `(S-group 1 n)` does
    [M86 p. 74]; an integer or a `Shared` has none."""
    if not is_form(form):
        return is_wild(form)
    return any(is_wild(a) or (is_form(a) and has_wildcard(a)) for a in form.elements)


@dataclass(frozen=True)
class Form:
    """One form: a type name and the four slots "(typename start-value length actual-value)"
    [M86 p. 49] (DECISION D-001).

    `kind` is one of the eight type names [M86 pp. 50-53]. `start` is an integer or a `Form`;
    `length` an integer, a `Form` or `Shared(0)`; `actual` a tuple of elements, each an
    integer, a `Form` or a `Shared`, or `None`. Which slots are used depends on the type:

    - Constant and Countup: `start` only, their one argument [M86 pp. 50, 56].
    - C-group, S-group, P-group: `start` and `length` [M86 pp. 51-52].
    - Y-group, Cycle, Tuple: `actual`, the list of elements; `start` and `length` are the
      optional parameters [M86 pp. 52-53], filled in by the parser from the elements when not
      given, as the program's own forms carry them (`(Cycle 3 2 (3 7))` [M86 p. 166],
      `(Tuple 1 3 ((Countup 1) 8 shared))` [M86 p. 162]). A `(Y-group 1 5)` without elements
      [M86 p. 55] has `actual` None.
    """

    kind: str
    start: object = None
    length: object = None
    actual: tuple | None = None

    def __post_init__(self):
        if self.kind not in IDEAL_TYPES:
            raise ValueError(f"not an ideal type: {self.kind!r}")
        if self.kind in LIST_TYPES:
            if self.actual is not None and not isinstance(self.actual, tuple):
                raise TypeError(f"{self.kind} elements must be a tuple, got {self.actual!r}")
            if self.actual is None and (self.start is None or self.length is None):
                raise ValueError(f"({self.kind}) needs its elements or its start and length")
        elif self.actual is not None:
            raise ValueError(f"{self.kind} takes no element list")
        elif self.start is None or (self.kind in GROUP_TYPES and self.length is None):
            raise ValueError(f"({self.kind}) is missing a parameter")
        elif self.kind in REPEATER_TYPES and self.length is not None:
            raise ValueError(f"{self.kind} takes one parameter [M86 p. 56]")

    def __str__(self):
        return form_text(self)

    @property
    def elements(self) -> tuple:
        """The elements of a Y-group, Cycle or Tuple, or the (start, length) argument list of
        a group, or the (start,) of a repeater: the list a `Shared` indexes (DECISION D-002)."""
        if self.kind in LIST_TYPES:
            return self.actual or ()
        if self.kind in GROUP_TYPES:
            return (self.start, self.length)
        return (self.start,)


def is_form(x) -> bool:
    """Whether x is a `Form` (an integer element is not; it stands for a Constant
    [M86 p. 50])."""
    return isinstance(x, Form)


def is_int(x) -> bool:
    """Whether x is a plain integer term (bools are not terms) [FCCA p. 48]."""
    return isinstance(x, int) and not isinstance(x, bool)


def start_value(form, elements=()):
    """The integer a form starts with: its first term [M86 p. 49] (DECISION D-001).

    An integer is its own start; a Constant's or Countup's start is its argument's; a group's
    is its start parameter's; a Y-group's, Cycle's or Tuple's is its given start or else its
    first element's. A `Shared` takes the start of the element it shares, found in `elements`.
    The appendix prints `(Tuple 1 3 ((Countup 1) 8 shared))` [M86 p. 162], so the start of a
    fence form whose first element is a form is that form's start value, not the form.
    """
    if is_int(form):
        return form
    if is_wild(form):
        raise ValueError("a wildcard has no start value [M86 p. 74]")
    if isinstance(form, Shared):
        return start_value(shared_argument(elements, form), elements)
    if form.kind in LIST_TYPES:
        if form.start is not None:
            return start_value(form.start, ())
        if not form.actual:
            raise ValueError(f"({form.kind}) has no elements to start from")
        return start_value(form.actual[0], form.actual)
    return start_value(form.start, form.elements)


def shared_argument(arguments: tuple, shared: Shared):
    """The argument a `Shared` refers to, following its path (DECISION D-002)."""
    item = None
    for index in shared.path:
        item = arguments[index]
        arguments = item.elements if is_form(item) else ()
    return item


# ---- parsing ----------------------------------------------------------------------------

def tokenize(text: str) -> list:
    """Split an s-expression into parentheses and atoms. Square brackets, the thesis's
    marks for optional parameters "([])" [M86 p. 49], are dropped."""
    tokens, atom = [], ""
    for char in text:
        if char in "()":
            if atom:
                tokens.append(atom)
                atom = ""
            tokens.append(char)
        elif char.isspace():
            if atom:
                tokens.append(atom)
                atom = ""
        elif char in "[]":
            continue
        else:
            atom += char
    if atom:
        tokens.append(atom)
    return tokens


def read_sexp(tokens: list, pos: int = 0):
    """Read one s-expression from the tokens: an integer, a symbol (str) or a list; returns
    it with the position after it ([M86 p. 49]: forms are written as s-expressions)."""
    if pos >= len(tokens):
        raise ValueError("unexpected end of form")
    token = tokens[pos]
    if token == "(":
        items, pos = [], pos + 1
        while pos < len(tokens) and tokens[pos] != ")":
            item, pos = read_sexp(tokens, pos)
            items.append(item)
        if pos >= len(tokens):
            raise ValueError("missing ')'")
        return items, pos + 1
    if token == ")":
        raise ValueError("unexpected ')'")
    if token.lstrip("-").isdigit():
        return int(token), pos + 1
    return token, pos + 1


def parse(text: str, bindings: dict | None = None) -> Form:
    """Read a form from its printed spelling [M86 pp. 49-58, 161-166, 169].

    Accepts the prose spelling (`(Cycle (2 1 8))` [M86 p. 53], `(Y-group [1] [3] (1 2 1))`
    [M86 p. 81], `shared` [M86 p. 56]), the program spelling (`(Cycle 3 2 (3 7))`
    [M86 p. 166]) and the appendix's `build-box` spelling, `(same pstrucN)` with `bindings`
    mapping each name to its form [M86 pp. 161-163] (DECISION D-002): the first `(same X)`
    stands for the bound form and each later one for a `Shared` of it. The symbol `n` in a
    parameter's place is the template wildcard, `(S-group 1 n)` [M86 p. 74]. Negative
    numbers are refused: the domain has only nonnegative integers [FCCA p. 48].
    """
    tokens = tokenize(text)
    sexp, pos = read_sexp(tokens)
    if pos != len(tokens):
        raise ValueError(f"text after the form: {' '.join(tokens[pos:])!r}")
    return _form_from_sexp(sexp, dict(bindings or {}))


def _form_from_sexp(sexp, bindings: dict) -> Form:
    if not isinstance(sexp, list) or not sexp or sexp[0] not in IDEAL_TYPES:
        raise ValueError(f"not a form: {_sexp_text(sexp)}")
    kind, args = sexp[0], sexp[1:]
    if kind in REPEATER_TYPES:
        if len(args) != 1:
            raise ValueError(f"({kind} arg) takes one argument [M86 p. 56]: {_sexp_text(sexp)}")
        (start,) = _arguments(args, bindings)
        return Form(kind, start)
    if kind in GROUP_TYPES:
        if len(args) != 2:
            raise ValueError(f"({kind} start length) takes two arguments: {_sexp_text(sexp)}")
        start, length = _arguments(args, bindings)
        return Form(kind, start, length)
    # Y-group, Cycle, Tuple: (type [start] [length] actual), the actual a list [M86 pp. 52-53].
    if args and isinstance(args[-1], list):
        params, elements = args[:-1], _arguments(args[-1], bindings)
    else:
        params, elements = args, None
    if len(params) > 2:
        raise ValueError(f"({kind} [start] [length] actual): {_sexp_text(sexp)}")
    start = length = None
    if params:
        start = _argument(params[0], bindings, ())
    if len(params) == 2:
        length = _argument(params[1], bindings, ())
    if elements is not None:
        if not elements:
            raise ValueError(f"({kind}) has no elements")
        if start is None:
            start = start_value(elements[0], elements)
        if length is None:
            length = len(elements)
    elif start is None or length is None:
        raise ValueError(f"({kind}) needs its elements or both a start and a length")
    return Form(kind, start, length, elements)


def _arguments(sexps: list, bindings: dict) -> tuple:
    """Read an argument list left to right, resolving `shared` and `(same X)`."""
    out = []
    for sexp in sexps:
        out.append(_argument(sexp, bindings, tuple(out)))
    return tuple(out)


def _argument(sexp, bindings: dict, earlier: tuple):
    """One argument: an integer, a form, or a reference to an earlier structure."""
    if is_int(sexp):
        if sexp < 0:
            raise ValueError(f"negative terms are not in the domain: {sexp}")
        return sexp
    if sexp == "n":
        return WILD
    if sexp == "shared":
        index = default_shared_index(earlier)
        if index is None:
            raise ValueError("'shared' with no structure to its left to share")
        return Shared(index)
    if isinstance(sexp, list) and sexp[:1] == ["shared"]:
        path = tuple(sexp[1:])
        if not path or not all(is_int(i) for i in path) or not _argument_at(earlier, path):
            raise ValueError(f"(shared N ...) must name an earlier argument: {_sexp_text(sexp)}")
        return Shared(path[0] if len(path) == 1 else path)
    if isinstance(sexp, list) and sexp[:1] == ["same"]:
        if len(sexp) != 2 or sexp[1] not in bindings:
            raise ValueError(f"unbound structure name in {_sexp_text(sexp)}")
        name = sexp[1]
        bound = bindings[name]
        if isinstance(bound, str):
            bound = bindings[name] = parse(bound)
        for index, item in enumerate(earlier):
            if item is bound:
                return Shared(index)
        return bound
    return _form_from_sexp(sexp, bindings)


def _argument_at(arguments: tuple, path: tuple) -> bool:
    """Whether a path of argument positions leads to an argument [M86 p. 84]."""
    for index in path:
        if not 0 <= index < len(arguments):
            return False
        item = arguments[index]
        arguments = item.elements if is_form(item) else ()
    return True


def default_shared_index(earlier: tuple):
    """The argument a bare `shared` refers to: the nearest form (not an integer) to its left
    in the same argument list, or None if there is none (DECISION D-002). This is where the
    thesis's arrow points on pp. 57, 84 and 161-163; the arrow to a bracketing integer on
    p. 58 is written `(shared 0)` instead."""
    for index in range(len(earlier) - 1, -1, -1):
        if is_form(earlier[index]):
            return index
    return None


def _sexp_text(sexp) -> str:
    if isinstance(sexp, list):
        return "(" + " ".join(_sexp_text(s) for s in sexp) + ")"
    return str(sexp)


# ---- printing ---------------------------------------------------------------------------

def form_text(form, prose: bool = False) -> str:
    """Print a form as the program does, `(Cycle 3 2 (3 7))` [M86 p. 166], or, with `prose`,
    as the thesis's prose does, `(Cycle (2 1 8))` [M86 p. 53] (DECISION D-001).

    A shared structure prints as the word `shared` [M86 p. 56] when it shares the nearest
    form to its left, where the thesis's arrow points on pp. 57, 84 and 161-163, and as
    `(shared N)`, N the index of the argument shared, otherwise, as for the bracketing 2 of
    `(Tuple (2 (Countup 1) shared))` [M86 p. 58], printed `(Tuple 2 3 (2 (Countup 1)
    (shared 0)))`, or the 2 inside the S-group of `(C-group (S-group 2 3) shared)`
    [M86 p. 84], printed `(C-group (S-group 2 3) (shared 0 0))` (DECISION D-002). An integer
    prints as itself. In the prose spelling a
    Y-group, Cycle or Tuple with elements drops its start and length; one without elements,
    `(Y-group 1 5)` [M86 p. 55], keeps them.
    """
    if is_int(form):
        return str(form)
    if is_wild(form):
        return "n"
    if isinstance(form, Shared):
        return "shared"
    if not is_form(form):
        raise TypeError(f"not a form: {form!r}")
    parts = [form.kind]
    if form.kind in LIST_TYPES:
        if form.actual is None or not prose:
            parts += [form_text(form.start, prose), form_text(form.length, prose)]
        if form.actual is not None:
            parts.append("(" + " ".join(_argument_texts(form.actual, prose)) + ")")
    else:
        parts += _argument_texts(form.elements, prose)
    return "(" + " ".join(parts) + ")"


def _argument_texts(arguments: tuple, prose: bool) -> list:
    """Print an argument list, writing each `Shared` as `shared` or `(shared N)`."""
    out = []
    for index, argument in enumerate(arguments):
        if isinstance(argument, Shared):
            if argument.index == default_shared_index(arguments[:index]):
                out.append("shared")
            else:
                out.append("(shared " + " ".join(str(i) for i in argument.path) + ")")
        else:
            out.append(form_text(argument, prose))
    return out


def value_text(value) -> str:
    """Print one hit's value as the thesis prints it: `(2 3 4)` for a group [M86 p. 51],
    `3` for a single term [M86 p. 50], nested parentheses for nested groups [M86 p. 57]."""
    if is_int(value):
        return str(value)
    return "(" + " ".join(value_text(v) for v in value) + ")"


def flatten(value) -> list:
    """The terms of a hit's value, left to right: the sequence a form gives, as "thus giving
    the sequence: 1 2 1 2 3 1 2 3 4 ..." [M86 p. 56]."""
    if is_int(value):
        return [value]
    return [term for part in value for term in flatten(part)]

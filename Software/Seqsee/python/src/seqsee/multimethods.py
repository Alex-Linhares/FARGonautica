"""Port of Class::Multimethods 1.701 (CPAN), the dispatcher behind Seqsee's
``multimethod FindMapping => qw(SInt SInt) => sub {...}`` declarations.

A ``Multimethod`` holds a table from type signatures (tuples of Perl type names) to
functions. A call maps each argument to a type name:

- an object → its Perl class (``util.perl_ref``: ``perl_name``, else the Python class name);
- a list/dict → "ARRAY"/"HASH";
- a number (int/float/bool) → "#";
- any other scalar, **including undef** → "$" (oracle-confirmed: ``~undef & undef`` is
  not 0 under Perl's rules).

Python can't see Perl's numeric flags, so a string that Perl had used as a number
(and would type as "#") is always "$" here.

Resolution follows the Perl code step by step. An exact signature wins. Otherwise
candidates are widened one inheritance step at a time ("#" widens to "$"; a class to
each of its Perl parents), and the first level with any match decides. With no match,
and if any variant has a "*", generic candidates are tried with 1, 2, ... positions
replaced by "*". Exactly one match is called. Two or more raise ``Confess("Cannot
resolve call ...")`` (even when they are the same function, as in Perl), and none
raises ``Confess("No viable candidate ...")``. The resolution cache is not ported: it
only affects speed.

Perl parents come from the Python class hierarchy: the parents of a class are its
nearest bases that set their own ``perl_name`` (bases without one are looked
through). Role mixins with a ``perl_name`` (e.g. SCategory) therefore show up as
parents. That is harmless unless a variant names them.
"""
import re

from seqsee import util
from seqsee.errors import Confess

_PLAIN_TYPE = re.compile(r"[\w:#]+")   # Perl: `next if $candidate->[$i] =~ /[^\w:#]/`


def arg_type(x):
    """The Perl dispatch type of one argument: a class name, "ARRAY", "HASH", "#" or "$"."""
    ref = util.perl_ref(x)
    if ref:
        return ref
    if isinstance(x, (bool, int, float)):
        return "#"
    return "$"


def _perl_parents(cls):
    parents = []
    for b in cls.__bases__:
        if b is object:
            continue
        if "perl_name" in b.__dict__:
            parents.append(b)
        else:
            parents.extend(_perl_parents(b))
    return parents


def _class_name(cls):
    return cls.__dict__.get("perl_name") or cls.__name__


def _collect_isa(x, isa):
    """Add ``name → [parent names]`` for x's class and all its Perl ancestors to isa."""
    if util.perl_ref(x) in ("", "ARRAY", "HASH"):
        return
    start = next((c for c in type(x).__mro__ if "perl_name" in c.__dict__), type(x))
    stack = [start]
    while stack:
        cls = stack.pop()
        name = _class_name(cls)
        if name in isa:
            continue
        parents = _perl_parents(cls)
        isa[name] = [_class_name(p) for p in parents]
        stack.extend(parents)


def perl_isa(x, name):
    """Perl ``$x->isa($name)`` by Perl class name, through the same hierarchy that
    dispatch uses. Scalars are never anything."""
    isa = {}
    _collect_isa(x, isa)
    if not isa:
        return False
    seen, todo = set(), [util.perl_ref(x)]
    while todo:
        n = todo.pop()
        if n == name:
            return True
        if n not in seen:
            seen.add(n)
            todo.extend(isa.get(n, []))
    return False


class Multimethod:
    """Perl: one multimethod name of Class::Multimethods (its %dispatch entry)."""

    def __init__(self, name):
        self.name = name
        self.dispatch = {}
        self.hasgeneric = False

        def call(*args):
            """Dispatch a call. The port's call sites use ``mm.call(...)`` rather than
            ``mm(...)`` because CPython 3.12 counts an instance ``__call__``, and a
            ``bound_method(*args)`` call, against its fixed C recursion limit, which deep
            Seqsee recursions through FindMapping hit (item 050b). This is a plain
            function, and so is the variant it calls: both are Python-to-Python calls."""
            return self.resolve(*args)(*args)
        self.call = call

    def variant(self, *types):
        """Decorator. Perl: ``multimethod NAME => @types => sub {...}`` (a redefinition
        replaces the earlier variant)."""
        def register(fn):
            self.dispatch[tuple(types)] = fn
            if any("*" in t for t in types):
                self.hasgeneric = True
            return fn
        return register

    def __call__(self, *args):
        return self.resolve(*args)(*args)

    def resolve(self, *args):
        """The variant a call with these args runs (raises ``Confess`` like Perl's croak)."""
        types = tuple(arg_type(a) for a in args)
        code = self.dispatch.get(types)
        if code is not None:
            return code
        isa = {}
        for a in args:
            _collect_isa(a, isa)
        tried, matches = set(), []
        candidates = [types]
        while not self._resolve(candidates, matches, tried, isa) and candidates:
            pass
        if not matches and self.hasgeneric:
            gencandidates = [types]
            for _ in range(len(types)):
                candidates = []
                for g in gencandidates:
                    for i in range(len(types)):
                        c = list(g)
                        c[i] = "*"
                        candidates.append(tuple(c))
                gencandidates = list(candidates)
                while not self._resolve(candidates, matches, tried, isa) and candidates:
                    pass
                if matches:
                    break
        sig = ",".join(types)
        if len(matches) == 1:
            return matches[0]
        if matches:
            listed = "\n".join(f"\t{self.name}({','.join(c)})" for c in candidates)
            raise Confess(f"Cannot resolve call to multimethod {self.name}({sig}). "
                          f"The multimethods:\n{listed}\nare equally viable")
        raise Confess(f"No viable candidate for call to multimethod {self.name}({sig})")

    def _resolve(self, candidates, matches, tried, isa):
        """Perl: Class::Multimethods::resolve. Checks one level of candidates and, if
        none matched, replaces them (in place) with the next level."""
        new = {}
        for cand in candidates:
            if cand in tried:
                continue
            tried.add(cand)
            match = self.dispatch.get(cand)
            if match is not None:
                matches.append(match)
                continue
            if not matches:
                for i, t in enumerate(cand):
                    if not _PLAIN_TYPE.fullmatch(t):
                        continue
                    parents = ["$"] if t == "#" else isa.get(t, [])
                    for p in parents:
                        nc = cand[:i] + (p,) + cand[i + 1:]
                        new[nc] = nc
        if not matches:
            candidates[:] = list(new)
        return len(matches)

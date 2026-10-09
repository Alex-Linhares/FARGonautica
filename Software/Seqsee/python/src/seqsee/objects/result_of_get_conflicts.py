"""Port of Seqsee/ResultOfGetConflicts.pm (``Seqsee::ResultOfGetConflicts``): what
SWorkspace::__FindGroupsConflictingWith returns, and how to fight the conflicts.

A Moose class. ``ResultOfGetConflicts({...})``/kwargs is Moose ``new``, checking the
attributes in name order: ``challenger`` (required, a Seqsee::Object, weak),
``exact_conflict`` (untyped, weak) and ``overlapping_conflicts`` (ArrayRef of
Seqsee::Object, default []). The accessors are read-only. The Array handles are
``has_overlapping_conflicts`` and ``overlapping_conflict_count`` (both the count) and
``all_overlapping_conflicts`` (a copy); ``overlapping_conflicts()`` is the list itself.

The ``bool`` overload is ``exact_conflict || count``; with ``fallback => 1`` the string
form is that value too (e.g. "abc" or "0"). ``Resolve`` → ``resolve``.

Hooks, looked up at call time (tests monkeypatch them):
``_fight_unto_death(opts)`` (SWorkspace->FightUntoDeath) and
``_check_liveness(*objects)`` (SWorkspace::__CheckLiveness).
"""
from seqsee import util
from seqsee.errors import Confess
from seqsee.multimethods import perl_isa
from seqsee.objects.object import (_moose_new_args, _moose_weaken, _perl_string,
                                   _type_error)


def _fight_unto_death(opts):
    """Perl: SWorkspace->FightUntoDeath({challenger =>, incumbent =>})."""
    from seqsee import sworkspace
    return sworkspace.fight_unto_death(opts)


def _check_liveness(*objects):
    """Perl: SWorkspace::__CheckLiveness(@objects)."""
    from seqsee import sworkspace
    return sworkspace.check_liveness(*objects)


def _read_only(args):
    if args:
        raise Confess("Cannot assign a value to a read-only accessor")


def _no_args(method, args):
    if args:
        raise Confess(f"Cannot call {method} with any arguments")


class ResultOfGetConflicts:
    """Perl: Seqsee::ResultOfGetConflicts."""

    perl_name = "Seqsee::ResultOfGetConflicts"

    def __init__(self, *args, **kwargs):
        kwargs = _moose_new_args(self.perl_name, args, kwargs)
        if "challenger" not in kwargs:
            raise Confess("Attribute (challenger) is required")
        if not perl_isa(kwargs["challenger"], "Seqsee::Object"):
            raise _type_error("challenger", "Seqsee::Object", kwargs["challenger"])
        overlapping = kwargs.get("overlapping_conflicts", [])
        if not (isinstance(overlapping, list)
                and all(perl_isa(x, "Seqsee::Object") for x in overlapping)):
            raise _type_error("overlapping_conflicts", "ArrayRef[Seqsee::Object]", overlapping)
        self._challenger = _moose_weaken(kwargs["challenger"])
        self._exact_conflict = _moose_weaken(kwargs.get("exact_conflict"))
        self._overlapping_conflicts = overlapping

    def challenger(self, *args):
        _read_only(args)
        return self._challenger()

    def exact_conflict(self, *args):
        _read_only(args)
        return self._exact_conflict()

    def overlapping_conflicts(self, *args):
        _read_only(args)
        return self._overlapping_conflicts

    def has_overlapping_conflicts(self, *args):
        """Perl: has_overlapping_conflicts (handles => 'count', so the count itself)."""
        _no_args("count", args)
        return len(self._overlapping_conflicts)

    def overlapping_conflict_count(self, *args):
        _no_args("count", args)
        return len(self._overlapping_conflicts)

    def all_overlapping_conflicts(self, *args):
        _no_args("elements", args)
        return list(self._overlapping_conflicts)

    def _bool_value(self):
        exact = self.exact_conflict()
        return exact if util.perl_true(exact) else self.overlapping_conflict_count()

    def __bool__(self):
        return util.perl_true(self._bool_value())

    def __str__(self):
        return _perl_string(self._bool_value())

    def resolve(self, opts=None):
        """Perl: Resolve(\\%opts): fight the exact conflict (unless it is
        IgnoreConflictWith; with FailIfExact, give up instead), then each live overlapping
        conflict except IgnoreConflictWith. 1 if the challenger won every fight, else None
        at the first loss."""
        challenger = self.challenger()
        ignore_conflict_with = ""
        fail_if_exact = 0
        if util.perl_true(opts):
            if "IgnoreConflictWith" in opts:
                ignore_conflict_with = opts["IgnoreConflictWith"]
            if util.perl_true(opts.get("FailIfExact")):
                fail_if_exact = opts["FailIfExact"]
        ignored = _perl_string(ignore_conflict_with)

        exact = self.exact_conflict()
        if util.perl_true(exact):
            if ignored != _perl_string(exact):
                if util.perl_true(fail_if_exact):
                    return None
                if not util.perl_true(_fight_unto_death(
                        {"challenger": challenger, "incumbent": exact})):
                    return None

        for some_other in self.all_overlapping_conflicts():
            if _perl_string(some_other) == ignored:
                continue
            if not util.perl_true(_check_liveness(some_other)):
                continue
            if not util.perl_true(_fight_unto_death(
                    {"challenger": challenger, "incumbent": some_other})):
                return None
        return 1

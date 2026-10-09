"""Port of Seqsee/ResultOfCanBeSeenAs.pm (``Seqsee::ResultOfCanBeSeenAs``): what
CanBeSeenAs (Seqsee/Object.pm) returns.

A Moose class. ``ResultOfCanBeSeenAs({...})``/kwargs is Moose ``new``: ``success`` is a
required Bool; ``entire_blemish`` is anything (the predicate is true once it is given,
even as undef); ``part_blemish`` is a HashRef. The ``bool`` overload is ``success``.

Readers keep their Perl meaning: ``GetEntireBlemish``→``get_entire_blemish``,
``IsEntireBlemished``→``is_entire_blemished``, ``GetPartsBlemished``→``get_parts_blemished``,
``ArePartsBlemished``→``are_parts_blemished``, ``IsBlemished``→``is_blemished``. The
constructors ``newUnblemished``/``newEntireBlemish``/``newByPart`` become
``new_unblemished``/``new_entire_blemish``/``new_by_part``, and ``NO()`` is the shared
failure singleton ``NO()`` (also ``ResultOfCanBeSeenAs.NO()``).
"""
from seqsee import util
from seqsee.errors import Confess

_MISSING = object()


def _moose_value(value):
    from seqsee.objects.object import _moose_value as mv
    return mv(value)


def _is_bool(value):
    """Moose ``Bool`` (as object._is_bool; repeated here to keep this module free of
    import-time dependencies on object.py)."""
    return value is None or isinstance(value, bool) or (
        util._is_scalar(value) and util.perl_str(value) in ("", "0", "1"))


def _type_error(attr, type_name, value):
    return Confess(f"Attribute ({attr}) does not pass the type constraint because: "
                   f"Validation failed for '{type_name}' with value {_moose_value(value)}")


class ResultOfCanBeSeenAs:
    """Perl: Seqsee::ResultOfCanBeSeenAs."""

    perl_name = "Seqsee::ResultOfCanBeSeenAs"

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess("Seqsee::ResultOfCanBeSeenAs: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        if "success" not in kwargs:
            raise Confess("Attribute (success) is required")
        # Moose checks the types in attribute-name order (oracle-confirmed).
        if "part_blemish" in kwargs and not isinstance(kwargs["part_blemish"], dict):
            raise _type_error("part_blemish", "HashRef", kwargs["part_blemish"])
        if not _is_bool(kwargs["success"]):
            raise _type_error("success", "Bool", kwargs["success"])
        self.success = kwargs["success"]
        self._entire_blemish = kwargs.get("entire_blemish", _MISSING)
        self._part_blemish = kwargs.get("part_blemish", _MISSING)

    def __bool__(self):
        return util.perl_true(self.success)

    def get_entire_blemish(self):
        """Perl: GetEntireBlemish (the metonym, or None)."""
        return None if self._entire_blemish is _MISSING else self._entire_blemish

    def is_entire_blemished(self):
        """Perl: IsEntireBlemished (the predicate: given at all, even as undef)."""
        return self._entire_blemish is not _MISSING

    def get_parts_blemished(self):
        """Perl: GetPartsBlemished: {index: metonym}, or None."""
        return None if self._part_blemish is _MISSING else self._part_blemish

    def are_parts_blemished(self):
        """Perl: ArePartsBlemished (the predicate: true even for an empty hash)."""
        return self._part_blemish is not _MISSING

    def is_blemished(self):
        """Perl: IsBlemished."""
        return self.is_entire_blemished() or self.are_parts_blemished()

    @classmethod
    def new_unblemished(cls):
        """Perl: newUnblemished (a fresh object each call)."""
        return cls(success=1)

    @classmethod
    def new_entire_blemish(cls, meto):
        """Perl: newEntireBlemish($meto)."""
        return cls(success=1, entire_blemish=meto)

    @classmethod
    def new_by_part(cls, blemish_hash):
        """Perl: newByPart(\\%blemishes)."""
        return cls(success=1, part_blemish=blemish_hash)

    @staticmethod
    def NO():  # noqa: N802 (Perl name: a constant sub)
        """Perl: NO(), the shared failure result."""
        return _NO


_NO = ResultOfCanBeSeenAs(success=0)


def NO():  # noqa: N802
    """Perl: Seqsee::ResultOfCanBeSeenAs::NO()."""
    return _NO

"""Port of Seqsee/ResultOfAttributeCopy.pm (``Seqsee::ResultOfAttributeCopy``): what
SWorkspace::__CopyAttributes returns, and part of a ResultOfPlonk.

A Moose class. ``ResultOfAttributeCopy({...})``/kwargs is Moose ``new``: ``success`` is
a Bool, default 1. ``success()`` reads it and ``success(v)`` sets it (and returns v),
like the Perl rw accessor. ``Success()``/``Failed()`` keep their Perl names (each makes a
fresh object); ``UpdateWith`` → ``update_with``.

There is no ``bool`` overload, so a failed copy result is still true.
"""
from seqsee import util
from seqsee.errors import Confess
from seqsee.objects.object import _is_bool, _moose_new_args, _type_error


class ResultOfAttributeCopy:
    """Perl: Seqsee::ResultOfAttributeCopy."""

    perl_name = "Seqsee::ResultOfAttributeCopy"

    def __init__(self, *args, **kwargs):
        kwargs = _moose_new_args(self.perl_name, args, kwargs)
        value = kwargs.get("success", 1)
        if not _is_bool(value):
            raise _type_error("success", "Bool", value)
        self._success = value

    def success(self, *args):
        """Perl: the ``success`` rw accessor."""
        if args:
            if not _is_bool(args[0]):
                raise _type_error("success", "Bool", args[0])
            self._success = args[0]
        return self._success

    @classmethod
    def Success(cls):  # noqa: N802 (Perl name; ``success`` is the accessor)
        """Perl: Success()."""
        return cls()

    @classmethod
    def Failed(cls):  # noqa: N802
        """Perl: Failed()."""
        return cls(success=0)

    def update_with(self, new_value):
        """Perl: UpdateWith($other): success = success && other's success. Like Perl's
        ``&&``, a false success is kept as it is and the other isn't consulted; a true one
        takes the other's value (even undef or "")."""
        mine = self.success()
        if not util.perl_true(mine):
            return self.success(mine)
        if new_value is None:
            raise Confess('Can\'t call method "success" on an undefined value')
        return self.success(new_value.success())

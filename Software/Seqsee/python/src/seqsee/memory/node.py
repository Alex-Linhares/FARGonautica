"""Port of Memory/Node.pm (Moose class ``Memory::Node``): a node of Memory::LTM.

``Node({...})``/kwargs is Moose ``new``: ``core`` is required and must do
Memory::Storable. ``core()`` reads it; ``core(value)`` type-checks, sets and returns the
new value (the rw accessor).

PERL-QUIRK (oracle-confirmed): Node has no SpikeBy/WeakenBy, although Memory::LTM's
SpikeBy/WeakenBy call them.
"""
from seqsee.errors import Confess
from seqsee.memory.storable import Storable
from seqsee.objects.object import _moose_new_args, _type_error


def _check_core(value):
    if not isinstance(value, Storable):
        raise _type_error("core", "Memory::Storable", value)


class Node:
    """Perl: Memory::Node."""

    perl_name = "Memory::Node"

    def __init__(self, *args, **kwargs):
        kwargs = _moose_new_args(self.perl_name, args, kwargs)
        if "core" not in kwargs:
            raise Confess("Attribute (core) is required")
        _check_core(kwargs["core"])
        self._core = kwargs["core"]

    def core(self, *args):
        if args:
            _check_core(args[0])
            self._core = args[0]
            return args[0]
        return self._core

"""Port of Memory/Insertible.pm (Moose role ``Memory::Insertible``): anything that can
be inserted into memory, perhaps in a normalized form.

The role becomes an abstract mixin. ``requires 'GetNormalizedForMemory'`` is the
abstract ``get_normalized_for_memory``. ``does(role)`` is Moose's ``does``: each mixin
lists the Perl role names it stands for in its own ``ROLES``.
"""
from abc import ABC, abstractmethod


class Insertible(ABC):
    """Perl role Memory::Insertible."""

    ROLES = ("Memory::Insertible",)

    def does(self, role):
        """Perl: Moose ``does($role)``."""
        return any(role in klass.__dict__.get("ROLES", ()) for klass in type(self).__mro__)

    @abstractmethod
    def get_normalized_for_memory(self):
        """Perl: GetNormalizedForMemory."""

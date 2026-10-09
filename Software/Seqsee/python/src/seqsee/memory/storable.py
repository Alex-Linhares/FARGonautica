"""Port of Memory/Storable.pm (Moose role ``Memory::Storable``): anything that can be
stored as-is into memory. It consumes Memory::Insertible, normalizing to itself.

``requires`` GetMemoryDependencies/Serialize/Deserialize become the abstract
``get_memory_dependencies``/``serialize``/classmethod ``deserialize``.
"""
from abc import abstractmethod

from seqsee.memory.insertible import Insertible


class Storable(Insertible):
    """Perl role Memory::Storable."""

    ROLES = ("Memory::Storable",)

    def get_normalized_for_memory(self):
        """Perl: GetNormalizedForMemory: the object itself."""
        return self

    @abstractmethod
    def get_memory_dependencies(self):
        """Perl: GetMemoryDependencies."""

    @abstractmethod
    def serialize(self):
        """Perl: Serialize."""

    @classmethod
    @abstractmethod
    def deserialize(cls, string):
        """Perl: Deserialize."""

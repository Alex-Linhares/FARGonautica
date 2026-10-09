"""Port of SCategory/MetonymySpec.pm, MetonymySpec/NotMetonyable.pm and
MetonymySpec/Metonyable.pm.

``MetonymySpec`` is the role SCategory consumes (``requires 'get_meto_types'``).
``NotMetonyable``/``Metonyable`` are the two implementations the categories mix in;
as in Perl, they don't consume ``MetonymySpec`` themselves.
"""
from abc import ABC, abstractmethod
from functools import cached_property

from seqsee import util
from seqsee.errors import Confess


class MetonymySpec(ABC):
    """Perl role SCategory::MetonymySpec."""

    @abstractmethod
    def get_meto_types(self):
        """Perl: get_meto_types (``requires``)."""


class NotMetonyable:
    """Perl role SCategory::MetonymySpec::NotMetonyable."""

    def get_meto_types(self):
        """Perl: ``return;`` (an empty list)."""
        return []

    def is_metonyable(self):
        """Perl: ``return;`` (undef in scalar context)."""
        return None


class Metonyable:
    """Perl role SCategory::MetonymySpec::Metonyable.

    The two Moose hash attributes are per-instance dicts built on first use from
    ``_default_metonym_finders``/``_default_metonymy_unfinder``, which consumers
    override (Perl: ``has '+metonym_finders' => (default => ...)``).
    A finder is called as ``finder(object, cat, name, bindings)`` and an unfinder as
    ``unfinder(cat, name, info_loss, object)``."""

    def _default_metonym_finders(self):
        return {}

    def _default_metonymy_unfinder(self):
        return {}

    @cached_property
    def metonym_finders(self):
        return self._default_metonym_finders()

    @cached_property
    def metonymy_unfinder(self):
        return self._default_metonymy_unfinder()

    def get_meto_finder(self, name):
        return self.metonym_finders.get(name)

    def get_meto_types(self):
        """Perl: the keys of metonym_finders."""
        return list(self.metonym_finders)

    def get_meto_unfinder(self, name):
        return self.metonymy_unfinder.get(name)

    def find_metonym(self, obj, name):
        """Perl: find_metonym($object, $name)."""
        finder = self.get_meto_finder(name)
        if not finder:
            raise Confess(f"No '{name}' meto_finder installed for category "
                          f"{util.perl_ref_string(self)}")
        bindings = obj.get_binding_for_category(self)
        if not util.perl_true(bindings):
            raise Confess("Object must belong to category")
        metonym = finder(obj, self, name, bindings)
        # "next line kludgy" (Perl)
        from seqsee.objects.anchored import Anchored
        if isinstance(obj, Anchored):
            metonym.get_starred().set_edges(*obj.get_edges())
        return metonym

    def is_metonyable(self):
        return 1

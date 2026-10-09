"""Port of LTMStorable.pm (Moose role ``LTMStorable``) and LTMStorable/Independent.pm
(Moose role ``LTMStorable::Independent``).

``LTMStorable`` (item 028) is the mixin SCategory consumes. Its ``requires``
(get_pure, get_memory_dependencies, serialize, deserialize) are SCategory's abstract
methods; it provides ``SpikeBy`` → ``spike_by(amount)`` and ``InsertISALink`` →
``insert_isa_link(cat)``, which pass through to SLTM::SpikeBy/InsertISALink via the
hooks ``_sltm_spike_by``/``_sltm_insert_isa_link`` and return their value.

Perl's ``deserialize`` is ``eval($string)``, and the strings it sees come from
``string_to_recreate``: ``Package->new()``. The port parses exactly that form and
instantiates the class with that ``perl_name`` (every Independent subclass is
registered). Anything else gives None, like a failed Perl eval.
"""
import re

# perl_name => class, for deserialize.
_RECREATABLE = {}

_NEW_RE = re.compile(r"\s*([\w:]+)->new\(\)\s*;?\s*")


def _sltm_spike_by(amount, *items):
    """Perl: SLTM::SpikeBy($amount, @items)."""
    from seqsee import sltm
    return sltm.spike_by(amount, *items)


def _sltm_insert_isa_link(item, cat):
    """Perl: SLTM::InsertISALink($item, $cat)."""
    from seqsee import sltm
    return sltm.insert_isa_link(item, cat)


class LTMStorable:
    """Perl role LTMStorable."""

    def spike_by(self, amount=None):
        """Perl: SpikeBy($amount)."""
        return _sltm_spike_by(amount, self)

    def insert_isa_link(self, cat=None):
        """Perl: InsertISALink($cat)."""
        return _sltm_insert_isa_link(self, cat)


class Independent:
    """Perl role LTMStorable::Independent. Consumers provide ``string_to_recreate``."""

    def __init_subclass__(cls, **kwargs):
        super().__init_subclass__(**kwargs)
        name = cls.__dict__.get("perl_name")
        if name:
            _RECREATABLE[name] = cls

    def is_pure(self):
        return 1

    def get_pure(self):
        return self

    def get_memory_dependencies(self):
        """Perl: ``return;`` (an empty list)."""
        return []

    def serialize(self):
        return self.string_to_recreate()

    @classmethod
    def deserialize(cls, string):
        """Perl: ``eval($string)``. Returns a new object, or None if the string is not
        ``Package->new()`` for a known package."""
        m = _NEW_RE.fullmatch(string or "")
        target = _RECREATABLE.get(m.group(1)) if m else None
        return target() if target else None

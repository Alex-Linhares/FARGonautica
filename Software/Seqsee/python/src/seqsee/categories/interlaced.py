"""Port of SCategory/Interlaced.pm: ``parts_count`` simpler sequences interlaced.

Instances are made with ``Interlaced.create(n)`` (Perl ``Create``, memoized by the
string form of n in a ``state %MEMO``, which ``_MEMO`` ports). The ``perl_name``
contains "Interlaced", which Categorizable's HasNonAdHocCategory relies on.
Seqsee::Object->create goes through ``base._object_create`` (item 021).
"""
import re

from seqsee import util
from seqsee.categories import base
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.errors import Confess
from seqsee.sbindings import SBindings

# Perl: `state %MEMO` in Create, keyed by the string form of parts_count.
_MEMO = {}

_MISSING = object()

_INT_RE = re.compile(r"-?[0-9]+")


def _check_int(value):
    """Moose ``isa => 'Int'``: a defined non-ref whose string form is ``/\\A-?[0-9]+\\z/``.
    The value itself is kept as given (so "02" stays "02")."""
    if (value is not None and not isinstance(value, bool) and util._is_scalar(value)
            and _INT_RE.fullmatch(util.perl_str(value))):
        return value
    raise Confess(f"Attribute (parts_count) does not pass the type constraint because: "
                  f"Validation failed for 'Int' with value {value!r}")


class Interlaced(NotMetonyable, SCategory):
    """Perl: SCategory::Interlaced."""

    perl_name = "SCategory::Interlaced"

    def __init__(self, parts_count=_MISSING):
        if parts_count is _MISSING:
            raise Confess("Attribute (parts_count) is required")
        self._parts_count = _check_int(parts_count)
        # Perl: memoize('get_name'), memoize('as_text'), keyed by the object.
        self._memo = {}
        super().__init__()

    @classmethod
    def create(cls, parts_count):
        """Perl: Create($parts_count): one instance per string form of parts_count."""
        key = util.perl_str(parts_count)
        if key not in _MEMO:
            _MEMO[key] = cls(parts_count=parts_count)
        return _MEMO[key]

    def get_parts_count(self):
        return self._parts_count

    def set_parts_count(self, value):
        self._parts_count = _check_int(value)

    def is_pure(self):
        return 1

    def instancer(self, obj):
        """Perl: Instancer($object): bindings part_no_1..part_no_n for the n items, if
        there are exactly parts_count of them."""
        parts_count = self.get_parts_count()
        parts = obj.get_items_array()
        if len(parts) != util.perl_num(parts_count):
            return None
        bdgs = {f"part_no_{i}": parts[i - 1] for i in util.perl_range(1, parts_count)}
        return SBindings.create({}, bdgs)

    def build(self, opts):
        """Perl: build(\\%opts): an object of part_no_1..part_no_n (missing parts are None)."""
        ret_parts = [opts.get(f"part_no_{i}") for i in util.perl_range(1, self.get_parts_count())]
        return base._object_create(*ret_parts)

    def get_name(self):
        # PERL-QUIRK (oracle-confirmed): memoized, so the name is fixed by the first call
        # even if set_parts_count changes the count later.
        if "get_name" not in self._memo:
            self._memo["get_name"] = f"Interlaced_{util.perl_str(self.get_parts_count())}"
        return self._memo["get_name"]

    def as_text(self):
        if "as_text" not in self._memo:
            self._memo["as_text"] = self.get_name()
        return self._memo["as_text"]

    def get_pure(self):
        return self

    def get_memory_dependencies(self):
        """Perl: ``return;`` (an empty list)."""
        return []

    def serialize(self):
        # PERL-QUIRK (oracle-confirmed): `my $id = ident $self;` parses as the method call
        # $self->ident, which does not exist, so serialize always dies.
        raise Confess('Can\'t locate object method "ident" via package "SCategory::Interlaced"')

    @classmethod
    def deserialize(cls, string):
        """Perl: deserialize($string) is Create($string)."""
        return cls.create(string)

    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild: 1 if the number of distinct attributes
        starting with "part_no_" equals parts_count."""
        count = sum(1 for a in util.uniq(*atts) if re.match(r"part_no_", util.perl_str(a)))
        if count == util.perl_num(self.get_parts_count()):
            return 1
        return None

    def longer_description(self):
        count = util.perl_str(self.get_parts_count())
        return f"That is, the sequence consists of {count} simpler sequences interlaced together"

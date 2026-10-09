"""Port of SCategory/MappingBased.pm: a group whose items follow one transform.

Instances are made with ``MappingBased.create(transform)`` (Perl ``Create``, memoized
in a ``state %MEMO``, which ``_MEMO`` ports). A relation is replaced by its type.

ApplyMapping and Seqsee::Object->create go through the hooks ``base._apply_mapping``
and ``base._object_create``. SLTM::encode/decode go through ``_sltm_encode`` and
``_sltm_decode`` here.
"""
from seqsee import util
from seqsee.categories import base
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.constants import RELN_SCHEME
from seqsee.errors import Confess
from seqsee.sbindings import SBindings

# Perl: `state %MEMO` in Create, keyed by the stringified transform (its address,
# since Mappings don't overload ""); here by id(transform). The memo keeps the
# category, which keeps the transform, so the id stays unique.
_MEMO = {}

_MISSING = object()


def _sltm_encode(*objects):
    """Perl: SLTM::encode(@objects)."""
    from seqsee import sltm
    return sltm.encode(*objects)


def _sltm_decode(string):
    """Perl: SLTM::decode($string): the list of objects."""
    from seqsee import sltm
    return sltm.decode(string)


def _check_mapping(value):
    """Moose ``isa => 'Mapping'``."""
    from seqsee.mapping import Mapping
    if isinstance(value, Mapping):
        return value
    shown = "undef" if value is None else util.perl_str(value) if util._is_scalar(value) \
        else util.perl_ref_string(value)
    raise Confess("Attribute (transform) does not pass the type constraint because: "
                  f"Validation failed for 'Mapping' with value {shown}")


class MappingBased(NotMetonyable, SCategory):
    """Perl: SCategory::MappingBased."""

    perl_name = "SCategory::MappingBased"

    def __init__(self, transform=_MISSING):
        if transform is _MISSING:
            raise Confess("Attribute (transform) is required")
        self._transform = _check_mapping(transform)
        # Perl: memoize('get_name'), memoize('as_text'), keyed by the object.
        self._memo = {}
        super().__init__()

    @classmethod
    def create(cls, transform):
        """Perl: Create($transform): one instance per transform (a relation counts as
        its type)."""
        from seqsee.srelation import SRelation
        if transform is None:
            raise Confess('Can\'t call method "isa" on an undefined value')
        if isinstance(transform, SRelation):
            transform = transform.get_type()
        key = id(transform) if not util._is_scalar(transform) else util.perl_str(transform)
        if key not in _MEMO:
            _MEMO[key] = cls(transform=transform)
        return _MEMO[key]

    def get_transform(self):
        return self._transform

    def set_transform(self, value):
        self._transform = _check_mapping(value)

    def instancer(self, obj):
        """Perl: Instancer($object): bindings first/last/length if each item maps to the
        next one; else, if the transform's category sees the whole object, a length-1
        instance; else the (false) value is_instance gave."""
        from seqsee.sint import SInt
        transform = self.get_transform()
        parts = obj.get_items_array()
        parts_count = len(parts)
        for part in parts:   # Perl: computes @effective_parts, never used.
            part.get_effective_object()
        if parts_count == 0:
            return None

        failure = False
        for idx in range(parts_count - 1):
            predicted_next = base._apply_mapping(transform, parts[idx])
            if not (util.perl_true(predicted_next) and util.perl_true(
                    parts[idx + 1].can_be_seen_as(predicted_next.get_structure()))):
                failure = True
                break

        if not failure:
            return SBindings(raw_slippages=obj.get_effective_slippages(),
                             bindings={"first": parts[0], "last": parts[-1],
                                       "length": SInt(parts_count)})

        # "maybe this is a length 1 instance!" (Perl)
        cat = transform.get_category()
        found = cat.is_instance(obj)
        if util.perl_true(found):
            return SBindings(raw_slippages=obj.get_effective_slippages(),
                             bindings={"first": obj, "last": obj, "length": SInt(1)})
        # Perl: the sub ends with that `if`, so it returns the false condition value
        # (oracle-confirmed: undef, 0 or "").
        return found

    def build(self, opts):
        """Perl: build(\\%opts): first, then ApplyMapping repeatedly, length items in all
        (only first and length are used). None if either is false, if length isn't
        positive, or if a mapping result is false."""
        from seqsee.sint import SInt
        transform = self.get_transform()
        start = opts.get("first")
        if not util.perl_true(start):
            return None
        length = opts.get("length")
        if not util.perl_true(length):
            return None
        # Perl: `ref($length) ? $length->[0] : $length` (SInt is a blessed array).
        if isinstance(length, SInt):
            length_as_num = length.get_mag()
        elif isinstance(length, (list, tuple)):
            length_as_num = length[0]
        elif util._is_scalar(length):
            length_as_num = length
        else:
            raise Confess("Not an ARRAY reference")
        if not util.perl_num(length_as_num) > 0:
            return None
        ret_items = [start]
        current_end = start
        for _ in util.perl_range(1, util.perl_num(length_as_num) - 1):
            next_ = base._apply_mapping(transform, current_end)
            if not util.perl_true(next_):
                return None
            ret_items.append(next_)
            current_end = next_
        ret = base._object_create(*ret_items)
        # Perl: $ret->[0] / $ret->[-1] go through Seqsee::Object's @{} overload.
        parts = ret.get_parts_ref()
        ret.add_category(self, SBindings(
            raw_slippages={},
            bindings={"first": parts[0], "last": parts[-1], "length": length}))
        ret.set_reln_scheme(RELN_SCHEME.CHAIN)
        return ret

    def get_name(self):
        # PERL-QUIRK (oracle-confirmed): memoized per object, so the name is fixed by the
        # first call even after set_transform.
        if "get_name" not in self._memo:
            self._memo["get_name"] = "Gp based on " + self.get_transform().as_text()
        return self._memo["get_name"]

    def as_text(self):
        if "as_text" not in self._memo:
            self._memo["as_text"] = self.get_name()
        return self._memo["as_text"]

    def is_pure(self):
        return 1

    def get_pure(self):
        return self

    def get_memory_dependencies(self):
        return [self.get_transform()]

    def serialize(self):
        return _sltm_encode(self.get_transform())

    @classmethod
    def deserialize(cls, string):
        type_ = _sltm_decode(string)[0]
        return cls.create(type_)

    def are_attributes_sufficient_to_build(self, *atts):
        """Perl: AreAttributesSufficientToBuild: 1 if both 'first' and 'length' are among
        the attributes (smartmatch, so a numeric 0 matches either; oracle-confirmed)."""
        if not util.smartmatch_in("first", atts):
            return None
        if not util.smartmatch_in("length", atts):
            return None
        return 1

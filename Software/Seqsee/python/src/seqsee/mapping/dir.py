"""Port of Mapping/Dir.pm (``Mapping::Dir``): the relation between two directions.

The Perl object is a blessed scalar ref holding its string (``$$self``), here
``self.string``. It doesn't inherit from Mapping and has no as_text/get_name
(oracle-confirmed). ``eq`` comparisons are on refs, so they are identity.
"""
from seqsee.constants import DIR
from seqsee.mapping import APPLY_MAPPING, FIND_MAPPING
from seqsee.util import perl_str

_MEMO = {}


class MappingDir:
    """Perl: Mapping::Dir. ``MappingDir(s)`` is Perl ``new`` (not memoized), and
    ``MappingDir.create(s)`` is the memoized form."""

    perl_name = "Mapping::Dir"

    def __init__(self, string):
        self.string = string

    @classmethod
    def create(cls, string):
        key = perl_str(string)
        if key not in _MEMO:
            _MEMO[key] = cls(string)
        return _MEMO[key]

    def get_memory_dependencies(self):
        return []

    def is_effectively_a_sameness_relation(self):
        """Perl: IsEffectivelyASamenessRelation (1/0)."""
        return 1 if self is SAME else 0

    def flipped_version(self):
        """Perl: FlippedVersion."""
        return self

    def get_pure(self):
        return self

    def serialize(self):
        return self.string

    @classmethod
    def deserialize(cls, string):
        return cls.create(string)


SAME = MappingDir.create("Same")
DIFFERENT = MappingDir.create("Different")
UNKNOWN = MappingDir.create("Unknown")


@FIND_MAPPING.variant("DIR", "DIR")
def _find_dir(da, db):
    if da is DIR.RIGHT:
        return SAME if db is DIR.RIGHT else DIFFERENT if db is DIR.LEFT else UNKNOWN
    if da is DIR.LEFT:
        return DIFFERENT if db is DIR.RIGHT else SAME if db is DIR.LEFT else UNKNOWN
    return UNKNOWN


@APPLY_MAPPING.variant("Mapping::Dir", "DIR")
def _apply_dir(transform, d):
    if transform is SAME:
        return d
    if transform is DIFFERENT:
        return d.flip()
    return DIR.UNKNOWN

"""Port of PositionStructure.pm.

A PositionStructure is the list of position strings of a group's items: each item's
SWorkspace::__GetPositionStructure (left edges, nested like the groups), passed
through SUtil::StringifyDeepArray. A bare element stays its (unstringified) left edge.
"""
from .util import perl_str, stringify_deep_array


def _get_position_structure(obj):
    """Hook for SWorkspace::__GetPositionStructure (element → left edge, group →
    list of its items' structures)."""
    from seqsee import sworkspace
    return sworkspace.get_position_structure(obj)


def get_position_structure_as_string(group):
    """Perl: SWorkspace::__GetPositionStructureAsString."""
    return stringify_deep_array(_get_position_structure(group))


class PositionStructure(list):
    """Perl: PositionStructure (a blessed array of strings)."""

    perl_name = "PositionStructure"

    @classmethod
    def create(cls, group):
        """Perl: Create."""
        return cls(stringify_deep_array(_get_position_structure(x)) for x in group)

    def is_a_subset_of(self, position_structure):
        """Perl: IsASubsetOf. 1 if self is a contiguous run inside position_structure
        (compared with `eq`), else None. An empty self is a subset of anything."""
        size1, size2 = len(self), len(position_structure)
        if size2 < size1:
            return None
        for i in range(size2 - size1 + 1):
            if all(perl_str(self[j]) == perl_str(position_structure[i + j])
                   for j in range(size1)):
                return 1
        return None

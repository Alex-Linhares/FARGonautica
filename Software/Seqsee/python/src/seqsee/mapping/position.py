"""Port of Mapping/Position.pm (``Mapping::Position``): succ/pred/same between SPos.

A Moose class (not a Mapping subclass, oracle-confirmed) with one required ``Str``
attribute ``text``. ``eq`` on these objects compares refs (identity), so only the
memoized SUCCESSOR/PREDECESSOR/SAME_POS act in ApplyMapping. A ``new`` "succ" maps to
nothing. Perl's %ComplexityLookup is unused and not ported.
"""
from seqsee.errors import Confess
from seqsee.mapping import APPLY_MAPPING, FIND_MAPPING
from seqsee.util import perl_str

_MEMO = {}
_FLIP_NAME = {"same": "same", "pred": "succ", "succ": "pred"}


def _check_str(value):
    """Moose ``isa => 'Str'``: a defined non-ref scalar."""
    if isinstance(value, bool) or not isinstance(value, (str, int, float)):
        shown = "undef" if value is None else repr(value)
        raise Confess("Attribute (text) does not pass the type constraint because: "
                      f"Validation failed for 'Str' with value {shown}")
    return value


class MappingPosition:
    """Perl: Mapping::Position. ``MappingPosition({"text": t})`` or ``MappingPosition(text=t)``
    is Perl ``new`` (not memoized). ``MappingPosition.create(t)`` is the memoized form."""

    perl_name = "Mapping::Position"

    def __init__(self, *args, **kwargs):
        if args:
            if len(args) != 1 or not isinstance(args[0], dict):
                raise Confess("Mapping::Position: odd arguments to constructor")
            kwargs = {**args[0], **kwargs}
        if "text" not in kwargs:
            raise Confess("Attribute (text) is required")
        self._text = _check_str(kwargs["text"])

    def get_text(self):
        return self._text

    def set_text(self, text):
        self._text = _check_str(text)

    @classmethod
    def create(cls, text):
        """Perl: create, memoized on the text's string form (undef keys as "", so
        create(undef) finds a "" entry if there is one, and dies otherwise)."""
        key = perl_str(text)
        if key not in _MEMO:
            _MEMO[key] = cls(text=text)
        return _MEMO[key]

    def get_memory_dependencies(self):
        return []

    def serialize(self):
        return self.get_text()

    @classmethod
    def deserialize(cls, string):
        return cls.create(string)

    def as_text(self):
        return "Mapping::Position " + perl_str(self.get_text())

    def get_pure(self):
        return self

    def is_effectively_a_sameness_relation(self):
        """Perl: IsEffectivelyASamenessRelation (1/0)."""
        return 1 if self is SAME_POS else 0

    def flipped_version(self):
        """Perl: FlippedVersion. PERL-QUIRK: an unknown text flips to create(undef)."""
        return MappingPosition.create(_FLIP_NAME.get(perl_str(self.get_text())))


SUCCESSOR = MappingPosition.create("succ")
PREDECESSOR = MappingPosition.create("pred")
SAME_POS = MappingPosition.create("same")


@FIND_MAPPING.variant("SPos", "SPos")
def _find_pos(p1, p2):
    diff = p2.position - p1.position
    if diff == 1:
        return SUCCESSOR
    if diff == -1:
        return PREDECESSOR
    if diff == 0:
        return SAME_POS
    return None


@APPLY_MAPPING.variant("Mapping::Position", "SPos")
def _apply_pos(rel, pos):
    from seqsee.spos import SPos
    index = pos.position
    if rel is SUCCESSOR:
        return SPos(index + 1)
    if rel is PREDECESSOR:
        return SPos(index - 1)
    if rel is SAME_POS:
        return pos
    return None

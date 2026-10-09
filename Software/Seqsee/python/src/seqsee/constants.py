"""Constant packages from S.pm: DIR, POS_MODE, METO_MODE, EXTENDIBILE,
RELN_SCHEME, DISTANCE_MODE, DISTANCE (S.pm:86-end).

Each Perl package becomes a class of the same name; its blessed singletons
(``$DIR::LEFT`` etc.) become class attributes (``DIR.LEFT``). Perl compares
these with ``eq``/``==``/``~~`` on the reference, i.e. by identity, so the
classes keep Python's default identity equality and hashing.

The category and metonym singletons of S.pm ($S::ASCENDING, $S::DOUBLE, ...)
are ported later (item 046).
"""

from seqsee.errors import Confess
from seqsee.util import toss


class DIR:
    """Direction (Perl package DIR): LEFT, RIGHT, UNKNOWN, NEITHER."""

    __slots__ = ("text",)
    LEFT: "DIR"
    RIGHT: "DIR"
    UNKNOWN: "DIR"
    NEITHER: "DIR"

    def __init__(self, text):
        self.text = text

    def __repr__(self):
        return f"DIR.{self.text.upper()}"

    def flip(self):
        """Perl: Flip. Dies (confess) for UNKNOWN and NEITHER."""
        if self is DIR.RIGHT:
            return DIR.LEFT
        if self is DIR.LEFT:
            return DIR.RIGHT
        raise Confess("Flip called on weird direction")

    def potentially_extendible(self):
        """Perl: PotentiallyExtendible."""
        return self is DIR.LEFT or self is DIR.RIGHT

    def is_left_or_right(self):
        """Perl: IsLeftOrRight."""
        return self is DIR.LEFT or self is DIR.RIGHT

    def as_text(self):
        return self.text


DIR.LEFT = DIR("left")
DIR.RIGHT = DIR("right")
DIR.UNKNOWN = DIR("unknown")
DIR.NEITHER = DIR("neither")


class POS_MODE:
    """Position mode (Perl package POS_MODE): FORWARD, BACKWARD."""

    __slots__ = ("mode",)
    FORWARD: "POS_MODE"
    BACKWARD: "POS_MODE"

    def __init__(self, mode):
        self.mode = mode

    def __repr__(self):
        return f"POS_MODE.{self.mode}"

    def as_text(self):
        return self.mode

    def get_memory_dependencies(self):
        return []

    def serialize(self):
        return self.mode

    @classmethod
    def deserialize(cls, string):
        """Perl looks up the package variable ``${$str}``: an unknown name
        gives undef (None here), not an error."""
        if string in ("FORWARD", "BACKWARD"):
            return getattr(cls, string)
        return None


POS_MODE.FORWARD = POS_MODE("FORWARD")
POS_MODE.BACKWARD = POS_MODE("BACKWARD")


class METO_MODE:
    """Metonymy mode (Perl package METO_MODE): NONE, SINGLE, ALLBUTONE, ALL, OTHER."""

    __slots__ = ("mode",)
    NONE: "METO_MODE"
    SINGLE: "METO_MODE"
    ALLBUTONE: "METO_MODE"
    ALL: "METO_MODE"
    OTHER: "METO_MODE"

    def __init__(self, mode):
        self.mode = mode

    def __repr__(self):
        return f"METO_MODE.{self.mode}"

    def as_text(self):
        return self.mode

    def is_position_relevant(self):
        """1 for SINGLE and ALLBUTONE, else 0."""
        return 1 if self is METO_MODE.SINGLE or self is METO_MODE.ALLBUTONE else 0

    def is_metonymy_present(self):
        return 0 if self is METO_MODE.NONE else 1

    def get_memory_dependencies(self):
        return []

    def serialize(self):
        return self.mode

    @classmethod
    def deserialize(cls, string):
        """Unknown names die (confess "Unknown!")."""
        if string in ("NONE", "SINGLE", "ALLBUTONE", "ALL", "OTHER"):
            return getattr(cls, string)
        raise Confess("Unknown!")

    def get_pure(self):
        return self


METO_MODE.NONE = METO_MODE("NONE")
METO_MODE.SINGLE = METO_MODE("SINGLE")
METO_MODE.ALLBUTONE = METO_MODE("ALLBUTONE")
METO_MODE.ALL = METO_MODE("ALL")
METO_MODE.OTHER = METO_MODE("OTHER")


class EXTENDIBILE:
    """Perl package EXTENDIBILE (sic): NO, PERHAPS, UNKNOWN.

    Kept "in case extendibility of relations comes back"; unused elsewhere.
    Overloads bool: only NO is false.
    """

    __slots__ = ("mode",)
    NO: "EXTENDIBILE"
    PERHAPS: "EXTENDIBILE"
    UNKNOWN: "EXTENDIBILE"

    def __init__(self, mode):
        self.mode = mode

    def __repr__(self):
        return f"EXTENDIBILE.{self.mode}"

    def __bool__(self):
        return self.mode != "NO"


EXTENDIBILE.NO = EXTENDIBILE("NO")
EXTENDIBILE.PERHAPS = EXTENDIBILE("PERHAPS")
EXTENDIBILE.UNKNOWN = EXTENDIBILE("UNKNOWN")


class RELN_SCHEME:
    """Relation scheme (Perl package RELN_SCHEME). NONE is the plain number 0;
    CHAIN is a blessed singleton (compared with ``==``, i.e. identity)."""

    __slots__ = ("type",)
    NONE = 0
    CHAIN: "RELN_SCHEME"

    def __init__(self, type_):
        self.type = type_

    def __repr__(self):
        return f"RELN_SCHEME.{self.type}"


RELN_SCHEME.CHAIN = RELN_SCHEME("CHAIN")


class DISTANCE_MODE:
    """Distance unit (Perl package DISTANCE_MODE): GROUP, ELEMENT."""

    __slots__ = ("mode",)
    GROUP: "DISTANCE_MODE"
    ELEMENT: "DISTANCE_MODE"

    def __init__(self, mode):
        self.mode = mode

    def __repr__(self):
        return f"DISTANCE_MODE.{self.mode.upper()}"

    @staticmethod
    def pick_one():
        """Perl: PickOne. GROUP with probability 0.25 (one toss), else ELEMENT."""
        return DISTANCE_MODE.GROUP if toss(0.25) else DISTANCE_MODE.ELEMENT

    def is_unit_groups(self):
        """Perl: IsUnitGroups."""
        return 1 if self is DISTANCE_MODE.GROUP else 0


DISTANCE_MODE.GROUP = DISTANCE_MODE("group")
DISTANCE_MODE.ELEMENT = DISTANCE_MODE("element")


class DISTANCE:
    """A distance: magnitude plus DISTANCE_MODE (Perl: blessed [$d, $mode])."""

    __slots__ = ("magnitude", "mode")

    def __init__(self, magnitude, mode):
        self.magnitude = magnitude
        self.mode = mode

    def __repr__(self):
        return f"DISTANCE({self.magnitude!r}, {self.mode!r})"

    @classmethod
    def in_elements(cls, distance):
        """Perl: InElements."""
        return cls(distance, DISTANCE_MODE.ELEMENT)

    @classmethod
    def in_groups(cls, distance):
        """Perl: InGroups."""
        return cls(distance, DISTANCE_MODE.GROUP)

    @classmethod
    def zero(cls):
        """Perl: Zero. Note the unit is GROUP (as_text "0 group")."""
        return cls(0, DISTANCE_MODE.GROUP)

    def is_non_zero(self):
        """Perl: IsNonZero. Returns the magnitude itself (truthy if nonzero)."""
        return self.magnitude

    def is_unit_groups(self):
        """Perl: IsUnitGroups."""
        return self.mode.is_unit_groups()

    def get_magnitude(self):
        """Perl: GetMagnitude."""
        return self.magnitude

    def as_text(self):
        return f"{self.magnitude} {self.mode.mode}"

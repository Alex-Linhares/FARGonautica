"""Port of lib/SPos.pm: a 1-based position inside a group (-1 means "last")."""
import re

from seqsee.errors import SErr, Confess

_INT_RE = re.compile(r"-?[0-9]+")


def _check_int(value):
    """Moose ``isa => 'Int'``: ints, or strings matching ``/\\A-?[0-9]+\\z/``."""
    if isinstance(value, int) and not isinstance(value, bool):
        return value
    if isinstance(value, str) and _INT_RE.fullmatch(value):
        return int(value)
    raise Confess(f"Attribute (position) does not pass the type constraint because: "
                  f"Validation failed for 'Int' with value {value!r}")


class SPos:
    """Perl: SPos. ``SPos(3)`` and ``SPos(position=3)`` both work (BUILDARGS).

    Equality (Perl ``eq`` and ``~~`` overloads) compares positions.
    """

    def __init__(self, *args, **kwargs):
        # BUILDARGS: a single non-ref argument is the position.
        if len(args) == 1 and not kwargs and not isinstance(args[0], (list, dict)):
            kwargs = {"position": args[0]}
        elif args:
            raise Confess("SPos: odd arguments to constructor")
        if kwargs.get("position") is None:
            raise Confess("Attribute (position) is required")
        self.position = kwargs["position"]
        # BUILD
        if self.position <= 0 and self.position != -1:
            raise Confess(f"Attempt to set position to {self.position}")

    @property
    def position(self):
        return self._position

    @position.setter
    def position(self, value):
        # The rw accessor checks Int only; BUILD's range check is not re-run,
        # so 0 or -7 can be set here (oracle-confirmed).
        self._position = _check_int(value)

    def __eq__(self, other):
        """Perl ``__equality__`` (the ``eq`` and ``~~`` overloads)."""
        if not isinstance(other, SPos):
            # Perl calls ->position on the non-object and dies.
            raise Confess(f'Can\'t locate object method "position" via {other!r}')
        return self.position == other.position

    def __ne__(self, other):
        # PERL-QUIRK: SPos overloads only `eq`; with `fallback => 1`, `ne` is not
        # derived from it and compares the stringified refs. So two distinct
        # objects are always `ne`, even at the same position.
        return self is not other

    # Perl hash keys use the ref address: identity hashing.
    __hash__ = object.__hash__

    def find_range(self, obj):
        """Perl: find_range. Returns a one-element list with the 0-based index."""
        index = self.position
        size = obj.get_parts_count()
        object_str = obj.get_structure_string()
        msg = f"OutOfRange [obj={object_str}]index={index}, size={size}, "
        if size == 0:
            SErr.throw(msg)
        if index == -1:
            return [size - 1]
        if index < 1 or index > size:
            SErr.throw(msg)
        return [index - 1]

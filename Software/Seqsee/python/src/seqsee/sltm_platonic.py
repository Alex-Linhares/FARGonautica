"""Port of SLTM/Platonic.pm (``SLTM::Platonic``): the memory core for things that
correspond to workspace objects (a structure such as ``[1, [2, 3]]`` or ``5``).

A Class::Std class with two ``:name<...>`` attributes, ``structure_string`` and
``structure``: each is a required init arg with ``get_X``/``set_X`` accessors (``set_X``
returns the old value). ``SLTMPlatonic({...})`` is Class::Std's ``new``, with its error
messages. ``create(string)`` is memoized on the string as given (Perl's ``%MEMO``), so
``"[1, 2]"`` and ``"[1,2]"`` are different objects; ``reset()`` clears the memo (Perl
never does; conftest calls it). ``structure_from_string`` runs before the memo lookup, so
a bad string always dies.

Port decision: Perl's structure holds the tokens as strings. Here a token whose Perl
string form round-trips through a number becomes that number ("1", "-2", "1.5"), as
SLTM's SInt decoding does, so structures compare with the integer magnitudes the rest
of the port uses. Other tokens ("01", "1e3", "a2") stay strings.

PERL-QUIRKs (oracle-confirmed):
- A string whose first token contains a digit anywhere is returned whole: "12abc",
  "a1" and "1,2" are scalar structures.
- Inside brackets, only comma-separated tokens containing a digit are kept, so
  "[1,a,2]" is [1, 2], "[x]" is [] and "[1]x"/"x[1]" are [1].
- An unmatched "]" dies "Modification of non-creatable array value attempted,
  subscript -1"; other unbalanced or multiple top-level structures die
  "Problematic string <string without whitespace>!".
"""
import re

from seqsee import util
from seqsee.errors import Confess

_ATTRS = ("structure_string", "structure")

# Perl: `my %MEMO` in create, keyed by the structure string.
_MEMO = {}


def reset():
    """Clear the create memo (for test isolation)."""
    _MEMO.clear()


def _token_value(token):
    if util.looks_like_number(token):
        num = util.perl_num(token)
        if util.perl_str(num) == token:
            return num
    return token


def structure_from_string(string):
    """Perl: SLTM::Platonic::structure_from_string($string)."""
    string = re.sub(r"\s+", "", util.perl_str(string))
    tokens = re.split(r"([\[\]])", string)
    while tokens and tokens[-1] == "":  # Perl split drops trailing empty fields
        tokens.pop()
    if tokens and re.search(r"\d", tokens[0]):
        return _token_value(tokens[0])
    result = [[]]
    for token in tokens:
        if token == "[":
            result.append([])
        elif token == "]":
            top = result.pop()
            if not result:
                raise Confess("Modification of non-creatable array value attempted, subscript -1")
            result[-1].append(top)
        else:
            result[-1].extend(_token_value(t) for t in token.split(",") if re.search(r"\d", t))
    if not (len(result) == 1 and len(result[0]) == 1):
        raise Confess(f"Problematic string {string}!")
    return result[0][0]


class SLTMPlatonic:
    """Perl: SLTM::Platonic."""

    perl_name = "SLTM::Platonic"

    def __init__(self, *args):
        if args and (len(args) != 1 or not isinstance(args[0], dict)):
            raise Confess(f"Argument to {self.perl_name}->new() must be hash reference")
        arg_ref = args[0] if args else {}
        arg_set = {**arg_ref, **(arg_ref.get(self.perl_name) or {})}
        missing, suss_keys = [], []
        self._values = {}
        for attr in _ATTRS:
            if attr in arg_set:
                self._values[attr] = arg_set[attr]
            else:
                missing.append(f"Missing initializer label for {self.perl_name}: '{attr}'.")
                suss_keys.extend(arg_set)
        if missing:
            from seqsee.objects.result_of_get_something_like import _mislabelled
            raise Confess("\n".join(missing + _mislabelled(suss_keys)
                                    + ["Fatal error in constructor call"]))

    @classmethod
    def create(cls, structure_string):
        """Perl: create($structure_string), memoized on the string."""
        structure = structure_from_string(structure_string)
        key = util.perl_str(structure_string)
        if _MEMO.get(key) is None:
            _MEMO[key] = cls({"structure": structure, "structure_string": structure_string})
        return _MEMO[key]

    def _set(self, attr, args):
        if not args:
            raise Confess(f"Missing new value in call to 'set_{attr}' method")
        old = self._values[attr]
        self._values[attr] = args[0]
        return old

    def get_structure_string(self, *args):
        return self._values["structure_string"]

    def set_structure_string(self, *args):
        return self._set("structure_string", args)

    def get_structure(self, *args):
        return self._values["structure"]

    def set_structure(self, *args):
        return self._set("structure", args)

    def as_text(self):
        return "plat" + util.perl_str(self._values["structure_string"])

    def get_memory_dependencies(self):
        """Perl: ``return;`` (an empty list)."""
        return []

    def serialize(self):
        return self._values["structure_string"]

    @classmethod
    def deserialize(cls, string):
        return cls.create(string)

    def get_pure(self):
        return self

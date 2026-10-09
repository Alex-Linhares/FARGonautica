"""Port of SCategory/Prime.pm: primes up to 97; succ/pred step to the next/previous prime.

The module functions IsPrime/NextPrime/PreviousPrime become ``is_prime``,
``next_prime`` and ``previous_prime``.
"""
import re

from seqsee.categories import numeric
from seqsee.categories.base import SCategory
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.errors import Confess
from seqsee.ltmstorable import Independent
from seqsee.sbindings import SBindings
from seqsee.util import perl_num, perl_str

PRIMES = (2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59,
          61, 67, 71, 73, 79, 83, 89, 97)
_PRIME_KEYS = {str(p) for p in PRIMES}   # Perl: %Primes
LARGEST_PRIME = max(PRIMES)


def is_prime(num):
    """Perl: IsPrime. ``$num ~~ %Primes`` is a string-key lookup (oracle-confirmed): the
    number 2.0 is prime ("2"), the strings "2.0", "07" and " 7" are not. Returns 0/1."""
    return 1 if perl_str(num) in _PRIME_KEYS else 0


def _start(num):
    """The integer that ``$num++`` / ``$num--`` steps from.

    PERL-QUIRK: Perl loops forever when it would step from a non-integer (no number it
    reaches is a key of %Primes), or when ``$num++`` is a magic string increment
    ("abc"++ is "abd"). The port raises Confess instead of hanging."""
    if isinstance(num, str) and re.fullmatch(r"[a-zA-Z]+[0-9]*", num):
        raise Confess(f"NextPrime would loop forever on {num!r} (magic string increment)")
    n = perl_num(num)
    if n != int(n):
        raise Confess(f"Next/PreviousPrime would loop forever on non-integer {perl_str(num)}")
    return int(n)


def next_prime(num):
    """Perl: NextPrime: the smallest prime above num, or None at or above 97."""
    if perl_num(num) >= LARGEST_PRIME:
        return None
    n = _start(num) + 1
    while not is_prime(n):
        n += 1
    return n


def previous_prime(num):
    """Perl: PreviousPrime: the largest prime below num, or None at or below 2."""
    if perl_num(num) <= 2:
        return None
    n = _start(num) - 1
    # Same result as Perl's walk down from n, without walking from huge numbers.
    n = min(n, LARGEST_PRIME)
    while not is_prime(n):
        n -= 1
    return n


class Prime(Independent, NotMetonyable, numeric.Numeric, SCategory):
    """Perl: SCategory::Prime."""

    perl_name = "SCategory::Prime"

    def numeric_instancer(self, mag):
        """Perl: NumericInstancer."""
        if not is_prime(mag):
            return None
        return SBindings.create({}, {})

    def find_mapping_for_cat(self, a, b):
        """Perl: FindMappingForCat($a, $b) on magnitudes: same/succ/pred, else None.

        PERL-QUIRK (oracle-confirmed): NextPrime(97) and PreviousPrime(2) are undef, which
        is == 0, so (97, 0) is "succ" and (2, 0) is "pred"."""
        if perl_num(a) == perl_num(b):
            name = "same"
        elif perl_num(next_prime(a)) == perl_num(b):
            name = "succ"
        elif perl_num(previous_prime(a)) == perl_num(b):
            name = "pred"
        else:
            return None
        return numeric._mapping_numeric_create(name, self)

    def apply_mapping_for_cat(self, transform, obj):
        """Perl: ApplyMappingForCat($transform, $mag). None past either end of the primes."""
        name = transform.get_name()
        if name == "same":
            return obj
        if name == "succ":
            return next_prime(obj)
        if name == "pred":
            return previous_prime(obj)
        return None

    def string_to_recreate(self):
        return "SCategory::Prime->new()"

    def get_name(self):
        return "Prime"

    def as_text(self):
        return "Prime"

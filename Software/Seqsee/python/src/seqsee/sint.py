"""Port of lib/SInt.pm: a bare integer with categories (used in bindings and mappings).

The Perl object is ``[mag, [categories]]``. Overloads: ``+``, ``-``, ``eq``, ``ne``
and ``""``. Perl's numeric ``==`` is not overloaded and dies on SInts; in Python
``==``/``!=`` are the ``eq``/``ne`` overloads.
"""
from seqsee import global_ as Global
from seqsee import s as S
from seqsee.categories import prime
from seqsee.constants import DIR
from seqsee.util import perl_num, perl_str


def _mag_of(x):
    """``ref($s) ? $s->[0] : $s``."""
    return x._mag if isinstance(x, SInt) else x


class SInt:
    """Perl: SInt."""

    def __init__(self, mag):
        self._mag = mag
        self._categories = [S.NUMBER]
        if Global.Feature.get("Primes") and prime.is_prime(mag):
            self.add_category(S.PRIME)
        if Global.Feature.get("Parity"):
            # Perl % truncates floats to integers first.
            if int(perl_num(mag)) % 2:
                self.add_category(S.ODD)
            else:
                self.add_category(S.EVEN)

    # Perl: add_SInt
    def __add__(self, other):
        return SInt(self._mag + _mag_of(other))

    __radd__ = __add__

    # Perl: subtract_SInt
    def __sub__(self, other):
        return SInt(self._mag - _mag_of(other))

    def __rsub__(self, other):
        return SInt(_mag_of(other) - self._mag)

    # Perl: SInt_eq / SInt_ne (numeric comparison of magnitudes)
    def __eq__(self, other):
        return perl_num(_mag_of(other)) == perl_num(self._mag)

    def __ne__(self, other):
        return perl_num(_mag_of(other)) != perl_num(self._mag)

    __hash__ = object.__hash__   # Perl hash keys use the ref address.

    def as_text(self):
        return f"SInt({perl_str(self._mag)})"

    __str__ = as_text
    __repr__ = as_text

    def get_direction(self):
        return DIR.RIGHT

    def get_categories(self):
        """Returns the live category list (Perl returns the array ref)."""
        return self._categories

    def add_category(self, cat, *_ignored):
        """Perl: add_category($cat). Callers such as CheckForAlternation also pass
        bindings, which Perl ignores."""
        if not any(c is cat for c in self._categories):
            self._categories.append(cat)

    def get_mag(self):
        return self._mag

    def get_pure(self):
        """Perl: SLTM::Platonic->create(mag)."""
        from seqsee.sltm_platonic import SLTMPlatonic
        return SLTMPlatonic.create(self._mag)

    def get_common_categories(self, *others):
        """Perl: ``$sint->get_common_categories(@others)`` (or ``SInt.get_common_categories(a, b, ...)``)."""
        return get_common_categories(self, *others)


def get_common_categories(*sints):
    """Perl: SInt::get_common_categories. Categories shared by every SInt.

    Perl keys a hash by the stringified category (its address) and returns hash
    order; this keys by identity and returns first-seen order.
    """
    count = len(sints)
    counter = {}
    for sint in sints:
        for cat in sint._categories:
            entry = counter.setdefault(id(cat), [cat, 0])
            entry[1] += 1
    return [cat for cat, n in counter.values() if n == count]

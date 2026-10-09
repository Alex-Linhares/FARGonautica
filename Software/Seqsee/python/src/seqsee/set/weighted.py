"""Port of Set/Weighted.pm: a bag of [key, weight] pairs.

The Perl object is a blessed array of ``[$key, $weight]`` pairs, so
``SetWeighted`` is a ``list`` of two-element lists.
"""
from seqsee import schoose
from seqsee.util import _is_scalar, perl_num, perl_str


def _key(x):
    """Perl hash key / ``ne`` comparison: scalars by string form, objects by identity.
    SInt overloads ``""`` (as_text), so SInts key by their text ("SInt(4)"): equal
    magnitudes merge (oracle-confirmed in sltm_persistence)."""
    if _is_scalar(x):
        return perl_str(x)
    if _is_sint(x):
        return x.as_text()
    return ("ref", id(x))


def _is_sint(x):
    return getattr(x, "perl_name", None) == "SInt" or type(x).__name__ == "SInt"


def _ne(a, b, b_key):
    """Perl ``$a ne $b``: SInt overloads ``ne`` (numeric on magnitudes); otherwise keys."""
    if _is_sint(a) or _is_sint(b):
        return bool(a != b)
    return _key(a) != b_key


class SetWeighted(list):
    """Perl: Set::Weighted."""

    def __init__(self, *pairs):
        super().__init__()
        self.insert(*pairs)

    def is_not_empty(self):
        return 1 if self else 0

    def is_empty(self):
        return 0 if self else 1

    def insert(self, *pairs):
        self.extend(pairs)

    def merge_keys(self):
        """Sum the weights of pairs with the same key (last key object wins).
        Perl leaves the pairs in hash order; the port uses first-seen order."""
        vivify = {}
        sums = {}
        for k, v in self:
            key = _key(k)
            vivify[key] = k
            sums[key] = sums.get(key, 0) + perl_num(v)
        self[:] = [[vivify[key], total] for key, total in sums.items()]

    def get_elements(self, threshold=None):
        if threshold is None:
            threshold = 0
        return [k for k, v in self if perl_num(v) >= threshold]

    def delete_below_threshold(self, threshold):
        threshold = perl_num(threshold)
        self[:] = [p for p in self if perl_num(p[1]) >= threshold]

    def delete_key(self, key):
        self.merge_keys()
        target = _key(key)
        self[:] = [p for p in self if _ne(p[0], key, target)]

    def choose(self):
        return schoose.choose([p[1] for p in self], [p[0] for p in self])

    def choose_a_few_nonzero(self, howmany):
        return schoose.choose_a_few_nonzero(howmany, [p[1] for p in self], [p[0] for p in self])

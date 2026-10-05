"""helpers.scm's b:canon and b:num, for Python values (test helper).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The batteries print every value through b:canon, so a test computes a value in
Python, canonicalises it here and compares the text with the Chez fixture.
Representations (metacat/chez.py): a Python str is a symbol, chez.String a
Scheme string, chez.Char a character, list/tuple a proper list, chez.Pair a
pair whose cdr is not a list, chez.Vector a vector, None the void object.
`strings=True` canonicalises plain str as Scheme strings instead (for values
that are strings in Scheme, such as format's results).
"""
from __future__ import annotations

import math
from fractions import Fraction

from metacat import chez


def b_num(x) -> str:
    """helpers.scm: b:num"""
    if isinstance(x, (complex, chez.ExactComplex)):
        return "C" + b_num(x.real) + "," + b_num(x.imag)
    if isinstance(x, (int, Fraction)):
        return str(x)
    if math.isnan(x):
        return "F+nan"
    if math.isinf(x):
        return "F+inf" if x > 0 else "F-inf"
    if x == 0.0 and math.copysign(1.0, x) < 0:
        return "F-0"
    return "F" + str(Fraction(x))


def canon(x, strings: bool = False) -> str:
    """helpers.scm: b:canon"""
    if isinstance(x, (list, tuple)) and not isinstance(x, chez.Vector):
        out = "()"
        for item in reversed(x):
            out = "(" + canon(item, strings) + " . " + out + ")"
        return out
    if x is True:
        return "#t"
    if x is False:
        return "#f"
    if x is None:
        return "#<void>"
    if isinstance(x, chez.Char):
        return "#\\" + str(ord(x))
    if isinstance(x, chez.String) or (strings and isinstance(x, str)):
        return '"' + x + '"'
    if isinstance(x, str):
        return "'" + x
    if isinstance(x, (int, float, Fraction, complex, chez.ExactComplex)):
        return b_num(x)
    if isinstance(x, chez.Pair):
        return "(" + canon(x.car, strings) + " . " + canon(x.cdr, strings) + ")"
    if isinstance(x, chez.Vector):
        return "#" + canon(list(x), strings)
    if callable(x):
        return "#<procedure>"
    raise TypeError(f"canon: {x!r}")


class Raw:
    """A piece of canonical text taken as is: for the few Chez values Python
    does not represent (exact complex numbers), so that a test can still
    compare the rest of a list with the fixture."""

    def __init__(self, text: str):
        self.text = text


def canon_with_raw(x, strings: bool = False) -> str:
    if isinstance(x, Raw):
        return x.text
    if isinstance(x, (list, tuple)) and not isinstance(x, chez.Vector):
        out = "()"
        for item in reversed(x):
            out = "(" + canon_with_raw(item, strings) + " . " + out + ")"
        return out
    return canon(x, strings)

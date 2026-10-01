"""Franz Lisp built-ins used by the 1987 source, with the oracle's semantics.

Port of src/franz-compat.lisp (and of the sortcar that src/oracle.lisp
installs in oracle mode).  The census of built-ins is in PORTING_NOTES.md,
"Franz built-ins used by the source (census)".  LISP_NAMES maps each Lisp
name covered here to its Python function; PLAIN_PYTHON lists the rest, which
the translation writes as ordinary Python.  python/tests/test_franz_census.py
checks that the two together cover the census.

The Python model of Lisp data:
  nil            None (an empty list is nil too; a false result is False)
  t              True
  fixnum/bignum  int
  double-float   float (oracle mode: every float is a double)
  string         str
  symbol         Symbol (interned: one object per package and name)
  proper list    list
Division: Franz `/` and `quotient` truncate integers toward zero and `mod`
is `rem`.  Python's // and % floor instead, so the translation never uses
them on possibly negative values; it calls the helpers below.
"""

import decimal
import functools
import math

# ---------------------------------------------------------------------------
# Symbols


class Symbol:
    """A Lisp symbol: its SBCL name (the upper-case name the oracle prints,
    "NODE-31") and its home package ("NUMBO", "KEYWORD"), or None when it is
    uninterned (uconcat).  Interned symbols are unique, so `is` is `eq`."""

    __slots__ = ("name", "package")

    def __init__(self, name, package):
        self.name = name
        self.package = package

    def __repr__(self):
        if self.package == "KEYWORD":
            return ":" + self.name
        if self.package is None:
            return "#:" + self.name
        return self.name


PACKAGES = ("NUMBO", "KEYWORD")
_SYMBOLS = {}


def intern(name, package="NUMBO"):
    """franz-compat.lisp: intern.  The symbol NAME in PACKAGE, made if new.
    NAME may also be a symbol (its name is used), as Franz intern takes."""
    if isinstance(name, Symbol):
        name = name.name
    if package is None:
        package = "NUMBO"
    if package not in PACKAGES:
        raise ValueError(f"unknown package {package!r}")
    key = (package, name)
    sym = _SYMBOLS.get(key)
    if sym is None:
        sym = _SYMBOLS[key] = Symbol(name, package)
    return sym


def find_package(name):
    """franz-compat.lisp: find-package.  CL find-package, also accepting a
    Franz-case name such as "keyword".  Packages are their names."""
    if isinstance(name, Symbol):
        name = name.name
    if name in PACKAGES:
        return name
    if isinstance(name, str) and invert_case(name) in PACKAGES:
        return invert_case(name)
    return None


def invert_case(string):
    """franz-compat.lisp: invert-case.  The readtable-case :invert rule: an
    all-lower-case name is upcased, an all-upper-case one downcased, a
    mixed-case one kept."""
    if not any(c.isupper() for c in string):
        return string.upper()
    if not any(c.islower() for c in string):
        return string.lower()
    return string


def _sbcl_double_pname(x):
    """How SBCL's princ prints the double X at run time (oracle mode reads
    the source with double floats, but *read-default-float-format* is single
    at run time, so the exponent marker is d): "2.5d0", "1.0d7", "-0.0d0".
    Fixed notation for 1e-3 <= |x| < 1e7, else scientific; the digits are the
    shortest that read back, as Python's repr."""
    if math.isinf(x) or math.isnan(x):
        raise ValueError(f"no print name for {x!r}")
    if x != 0 and abs(x) < 2.2250738585072014e-308:
        raise ValueError(f"subnormal {x!r}: SBCL prints it with more digits than repr")
    sign = "-" if math.copysign(1.0, x) < 0 else ""
    if x == 0:
        return sign + "0.0d0"
    shortest = decimal.Decimal(repr(abs(x))).normalize().as_tuple()
    digits = "".join(map(str, shortest.digits))
    point = len(digits) + shortest.exponent     # |x| = 0.<digits> * 10^point
    if 1e-3 <= abs(x) < 1e7:
        if point <= 0:
            text = "0." + "0" * -point + digits
        elif point >= len(digits):
            text = digits + "0" * (point - len(digits)) + ".0"
        else:
            text = digits[:point] + "." + digits[point:]
        return sign + text + "d0"
    return f"{sign}{digits[0]}.{digits[1:] or '0'}d{point - 1}"


def franz_pname(x):
    """franz-compat.lisp: franz-pname.  The print name X would have in Franz:
    symbols are case-inverted, strings are taken as they are, numbers are
    printed in base 10 (doubles as SBCL prints them)."""
    if isinstance(x, str):
        return x
    if isinstance(x, Symbol):
        return invert_case(x.name)
    if x is None:
        return "nil"
    if x is True:
        return "t"
    if isinstance(x, int):
        return str(x)
    if isinstance(x, float):
        return _sbcl_double_pname(x)
    raise TypeError(f"no print name for {x!r}")


def princ_to_string(x):
    """What SBCL's princ (format ~a) prints for X when it fits on a line:
    symbols by their upper-case names (keywords without the colon), strings
    as they are, doubles as 2.5d0, lists in parentheses."""
    if x is None:
        return "NIL"
    if x is True:
        return "T"
    if isinstance(x, Symbol):
        return x.name
    if isinstance(x, list):
        return "(" + " ".join(princ_to_string(v) for v in x) + ")" if x else "NIL"
    return franz_pname(x)


def princ_fill(x, column, margin=80):
    """princ_to_string, with the line breaks of SBCL's pretty printer
    (*print-pretty* is t) for a list X of short elements printed from COLUMN:
    pprint-fill, i.e. an element goes on the next line, under the first one,
    when it would end past MARGIN together with what follows it before the
    next possible break: the space pprint-fill writes after it, or the
    list's closing parenthesis after the last one.  An element too long to
    fit on a line is not modelled."""
    if not isinstance(x, list) or not x:
        return princ_to_string(x)
    parts = [princ_to_string(v) for v in x]
    indent = column + 1
    out = "(" + parts[0]
    col = indent + len(parts[0])
    for part in parts[1:]:
        section = len(part) + 1
        if indent + section > margin:
            raise NotImplementedError("princ_fill: an element longer than a line")
        if col + 1 + section > margin:
            out += "\n" + " " * indent + part
            col = indent + len(part)
        else:
            out += " " + part
            col += 1 + len(part)
    return out + ")"


def concat(*args):
    """franz-compat.lisp: concat.  Concatenate the print names of ARGS and
    intern the result in NUMBO: concat(intern("NODE-"), 31) is NODE-31."""
    return intern(invert_case("".join(franz_pname(a) for a in args)))


def uconcat(*args):
    """franz-compat.lisp: uconcat.  Like concat, but uninterned."""
    return Symbol(invert_case("".join(franz_pname(a) for a in args)), None)


def get_pname(symbol):
    """franz-compat.lisp: get-pname.  The Franz print name, as a string."""
    return franz_pname(symbol)


def string_length(x):
    """franz-compat.lisp: string-length.  Length of a string or print name."""
    return len(franz_pname(x))


def alphalessp(x, y):
    """franz-compat.lisp: alphalessp.  Compare print names (by char code)."""
    return franz_pname(x) < franz_pname(y)


# ---------------------------------------------------------------------------
# Equality


def _is_nil(x):
    return x is None or x is False or (isinstance(x, list) and not x)


def _eql(x, y):
    """CL eql on the data model.  Numbers: same type and value (1 and 1.0
    differ, so do 0.0 and -0.0).  Lisp strings have an identity Python
    strings lack, so they are refused rather than guessed."""
    if isinstance(x, str) or isinstance(y, str):
        raise TypeError("eq/eql on Lisp strings can't be modelled in Python")
    if isinstance(x, bool) or isinstance(y, bool):
        return x is y
    if type(x) is int and type(y) is int:
        return x == y
    if type(x) is float and type(y) is float:
        return x == y and math.copysign(1.0, x) == math.copysign(1.0, y)
    return x is y


def equal(x, y):
    """CL equal (Franz member and nequal use it): lists element by element,
    strings by content, everything else eql."""
    if _is_nil(x) and _is_nil(y):
        return True
    if isinstance(x, list) and isinstance(y, list):
        return len(x) == len(y) and all(equal(a, b) for a, b in zip(x, y))
    if isinstance(x, str) or isinstance(y, str):
        return isinstance(x, str) and isinstance(y, str) and x == y
    if isinstance(x, list) or isinstance(y, list):
        return False
    return _eql(x, y)


def nequal(x, y):
    """franz-compat.lisp: nequal.  (not (equal x y))."""
    return not equal(x, y)


# ---------------------------------------------------------------------------
# Lists


def memq(x, lst):
    """franz-compat.lisp: memq.  The tail of LST starting at the first
    element eq to X, or None.  Fixnums are eq when equal, as in SBCL."""
    for i, item in enumerate(lst or ()):
        if _eql(x, item):
            return lst[i:]
    return None


def member(x, lst):
    """franz-compat.lisp: member.  Like memq, but compares with equal."""
    for i, item in enumerate(lst or ()):
        if equal(x, item):
            return lst[i:]
    return None


def sortcar(lst, predicate):
    """oracle.lisp: oracle-copying-sortcar (franz-compat.lisp: sortcar).
    LST, a list of lists, sorted by their cars with PREDICATE, a function of
    two cars; None means alphabetical (alphalessp).  The sort is stable and,
    as in oracle mode, works on a copy: LST itself is left as it was."""
    less = predicate or alphalessp

    def compare(a, b):
        if less(a[0], b[0]):
            return -1
        if less(b[0], a[0]):
            return 1
        return 0
    return sorted(lst or (), key=functools.cmp_to_key(compare))


# ---------------------------------------------------------------------------
# Arithmetic


def _is_integer(x):
    return isinstance(x, int) and not isinstance(x, bool)


def _truncate(a, b):
    """CL (truncate a b) on integers: the quotient rounded toward zero."""
    q = abs(a) // abs(b)
    return -q if (a < 0) != (b < 0) else q


def _divide_2(a, b):
    """franz-compat.lisp: franz-divide-2."""
    if _is_integer(a) and _is_integer(b):
        # 1987: Franz truncates integer division; codelets.lisp `round` needs
        # (/ 31 10) = 3 (PORTING_NOTES.md, census: `quotient`, `/`).
        return _truncate(a, b)
    return float(a) / b


def quotient(*numbers):
    """franz-compat.lisp: quotient.  Integers: truncating division.  Any
    float: float division.  (quotient) = 1, (quotient x) = x."""
    if not numbers:
        return 1
    return functools.reduce(_divide_2, numbers)


def divide(*numbers):
    """franz-compat.lisp: / (the same as quotient)."""
    return quotient(*numbers)


def star_quo(x, y):
    """franz-compat.lisp: *quo.  Integer quotient, truncated."""
    if _is_integer(x) and _is_integer(y):
        return _truncate(x, y)
    return math.trunc(x / y)


def mod(x, y):
    """franz-compat.lisp: mod.  Franz mod is the remainder, with the sign of
    the dividend (CL rem): mod(-17, 5) = -2."""
    if _is_integer(x) and _is_integer(y):
        # 1987: Franz mod = rem, not Python % (PORTING_NOTES.md, census: `mod`).
        r = abs(x) % abs(y)
        return -r if x < 0 else r
    if y == 0:
        raise ZeroDivisionError("mod by zero")
    return math.fmod(x, y)


def star_mod(x, y):
    """franz-compat.lisp: *mod.  The balanced residue of X modulo Y, in
    [|Y|/2 - |Y| + 1, |Y|/2] (|Y|/2 truncated)."""
    n = abs(y)
    r = x % n           # n > 0: Python's floor modulo is CL mod
    return r - n if r > n // 2 else r


def fix(x):
    """franz-compat.lisp: fix.  The integer closest to X, rounding down."""
    return math.floor(x)


def max_(*numbers):
    """franz-compat.lisp: max.  The largest argument (the first, on ties)."""
    if not numbers:
        # 1987: (max) = 0. codelets.lisp temperature applies max to an empty
        # misfortune list (PORTING_NOTES.md, item 9).
        return 0
    result = numbers[0]
    for x in numbers[1:]:
        if x > result:
            result = x
    return result


def min_(*numbers):
    """franz-compat.lisp: min.  The smallest argument (the first, on ties).
    (min) is an error, as CL:MIN."""
    if not numbers:
        raise TypeError("min needs at least one argument")
    result = numbers[0]
    for x in numbers[1:]:
        if x < result:
            result = x
    return result


# ---------------------------------------------------------------------------
# Census

LISP_NAMES = {
    "quotient": "quotient",
    "/": "divide",
    "franz-divide-2": "_divide_2",
    "*quo": "star_quo",
    "mod": "mod",
    "*mod": "star_mod",
    "fix": "fix",
    "max": "max_",
    "min": "min_",
    "nequal": "nequal",
    "memq": "memq",
    "member": "member",
    "sortcar": "sortcar",
    "concat": "concat",
    "uconcat": "uconcat",
    "get-pname": "get_pname",
    "string-length": "string_length",
    "alphalessp": "alphalessp",
    "invert-case": "invert_case",
    "franz-pname": "franz_pname",
    "intern": "intern",
    "find-package": "find_package",
}

PLAIN_PYTHON = {
    "if": "Python if/elif/else (thenret: the test's value)",
    "defun": "def; the lexpr check-temperature takes *args",
    "defvar": "World attributes / module constants",
    "franz-if-keyword-p": "part of the keyword-if macro: Python if",
    "franz-if-keyword-form-p": "part of the keyword-if macro: Python if",
    "franz-if-clauses": "part of the keyword-if macro: Python if",
    "plus": "+ (int/float contagion as in CL)",
    "add": "+",
    "sum": "+",
    "times": "*",
    "product": "*",
    "difference": "- ((difference x) = x is unused)",
    "diff": "-",
    "minus": "unary -",
    "add1": "x + 1",
    "sub1": "x - 1",
    "remainder": "unused by the source; mod is the remainder",
    "float": "float(); oracle.lisp makes every float a double, as Python's",
    "sqrt": "math.sqrt (of a float: a double, as oracle.lisp's)",
    "getenv": "os.environ.get(name, '')",
    "print": "graphics/debug output only (init.lisp:99); not traced",
    "new-vector": "graphics only (pnet-graphics.lisp); not ported",
    "vref": "graphics only (pnet-graphics.lisp); not ported",
    "vset": "graphics only (pnet-graphics.lisp); not ported",
}

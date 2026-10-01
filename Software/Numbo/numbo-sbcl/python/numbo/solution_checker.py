"""Check the solution config prints after "Done :".

Port of src/solution-checker.lisp (not part of the 1987 source).  When
*problem-solved* is 1, config prints "Done : " and calls
(decompose 'cyto-target), which prints one paragraph per operation node, from
the target down (codelets.py: decompose):

    Operation PLUS24-7-V1 has been applied
    to CYTO-TARGET-7-V1 ( 7) and to CYTO-BRICK3 ( 24)
    to get CYTO-TARGET

An operation node links three cyto-nodes (res = op1 + op2, or res = op1 x
op2) and decompose prints the two that are not the node being expanded.  So
"to get N" means N = a + b or N = |a - b| for a PLUS node, and N = a x b or
N = a / b (exact) for a TIMES node, depending on which side of the operation
N is on.  The value of N is not printed: it is the target for the first
paragraph, and otherwise the value printed where N appears as an operand.

check_solution(output, [31, 3, 5, 24, 3, 14])
  => (True, None, "31 = (14 x (5 - 3)) + 3")-style expression
  or (False, reason, None)
The solution is valid when the operations form a tree whose root is the
target, every leaf is a given brick (CYTO-BRICKi has the i-th brick's value)
used at most once, every other operand is itself derived, and every step is
correct arithmetic.  This rejects a decomposition that goes through a block
killed after a derived target was built on it (the kill-block gap,
PORTING_NOTES.md item 10).

The Lisp's details are kept, since the reasons are compared with the
oracle's word for word: words and names match case-insensitively
(STRING-EQUAL, EQUALP hash tables), "Done :" is found case-sensitively, the
first failure wins, and a value is whatever READ-FROM-STRING reads as an
integer (`_read_integer`).
"""

import re
import unicodedata

__all__ = ["solution_tokens", "brick_index", "check_solution"]


def solution_tokens(text):
    """solution-checker.lisp: solution-tokens.  TEXT split into tokens at
    whitespace, dropping parentheses."""
    return [tok for tok in re.split(r"[ \t\n\r()]+", text) if tok]


def _char_equal_key(s):
    """S as an EQUALP hash key: CHAR-EQUAL ignores case, one character for
    one character."""
    return "".join(u if len(u := c.upper()) == 1 else c for c in s)


def _string_equal(a, b):
    """STRING-EQUAL."""
    return len(a) == len(b) and _char_equal_key(a) == _char_equal_key(b)


def _digit_weight(c, radix=10):
    """DIGIT-CHAR-P: the weight of C in RADIX, or None.  Any Unicode decimal
    digit counts, as in SBCL; letters are ASCII."""
    if c.isdecimal():
        w = unicodedata.decimal(c)
    elif "a" <= c.lower() <= "z" and c.isascii():
        w = ord(c.lower()) - ord("a") + 10
    else:
        return None
    return w if w < radix else None


def _parse_rational(s, radix, allow_point):
    """S as [sign] digits [/ digits] in RADIX (with a trailing decimal point
    allowed when ALLOW_POINT), or None.  A ratio is returned as (n, d)."""
    sign = 1
    if s[:1] in ("+", "-"):
        sign = -1 if s[0] == "-" else 1
        s = s[1:]

    def digits(t):
        if not t:
            return None
        n = 0
        for c in t:
            w = _digit_weight(c, radix)
            if w is None:
                return None
            n = n * radix + w
        return n

    if allow_point and s.endswith(".") and "/" not in s:
        s = s[:-1]
    num, slash, den = s.partition("/")
    n = digits(num)
    if n is None:
        return None
    if not slash:
        return sign * n
    d = digits(den)
    if d is None:
        return None
    return (sign * n, d)


def _read_integer(s):
    """(let ((*read-eval* nil)) (read-from-string s)) when that is an
    integer (INTEGERP), else None (other objects, and IGNORE-ERRORS's nil).

    S is a token, so it has no whitespace or parentheses.  The reader stops
    at the first terminating macro character; what comes before it is read
    as a token, or nothing can be: a lone quote, string, comma, backquote or
    comment is not an integer.  A token is an integer in decimal (an optional
    sign, digits, an optional trailing point), a ratio that reduces to one
    (division by zero is an error), or #x/#b/#o/#Nr in that radix.  Escapes
    (| and \\) make a symbol."""
    m = re.match(r"[^\"';,`]*", s)
    token = m.group(0)
    if not token or "|" in token or "\\" in token:
        return None
    radix, allow_point = 10, True
    if token.startswith("#"):
        m = re.match(r"#(?:([xXbBoO])|([0-9]+)[rR])(.*)\Z", token, re.S)
        if not m:
            return None
        if m.group(1):
            radix = {"x": 16, "b": 2, "o": 8}[m.group(1).lower()]
        else:
            radix = int(m.group(2))
            if not 2 <= radix <= 36:
                return None
        token, allow_point = m.group(3), False
    value = _parse_rational(token, radix, allow_point)
    if isinstance(value, tuple):
        n, d = value
        if d == 0 or n % d != 0:
            return None
        return n // d
    return value


def _prin1_string(s):
    """~s of a string."""
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def brick_index(name):
    """solution-checker.lisp: brick-index.  1..n for a name CYTO-BRICKn,
    else None (and 0 for CYTO-BRICK0)."""
    prefix = "CYTO-BRICK"
    if not (len(name) > len(prefix) and _string_equal(prefix, name[:len(prefix)])):
        return None
    rest = name[len(prefix):]
    if not all(_digit_weight(c) is not None for c in rest):
        return None
    return int(rest)


class _Invalid(Exception):
    """(throw 'invalid (values nil reason nil))."""


def _fail(reason):
    raise _Invalid(reason)


def check_solution(output, problem):
    """solution-checker.lisp: check-solution.  Check the decomposition
    printed in OUTPUT for PROBLEM = [target, b1, ..., b5].  Return
    (valid_p, reason, expression)."""
    try:
        return True, None, _check_solution(output, problem)
    except _Invalid as invalid:
        return False, invalid.args[0], None


def _check_solution(output, problem):
    target, bricks = problem[0], list(problem[1:])
    derivations = {}  # key -> (op, a, va, b, vb)
    claimed = {}      # key -> printed value
    used = set()
    root = None

    done = output.find("Done :")
    if done < 0:
        _fail('no "Done :" in the output')
    # Parse every "Operation ..." paragraph after "Done :".
    tokens = solution_tokens(output[done + 6:])
    i = 0
    while i < len(tokens):
        if not _string_equal(tokens[i], "Operation"):
            i += 1
            continue
        p = i

        def next_():
            nonlocal p
            if p >= len(tokens):
                _fail("truncated operation paragraph")
            p += 1
            return tokens[p - 1]

        def expect(word):
            nonlocal p
            got = ""
            if p < len(tokens):
                got = tokens[p]
                p += 1
            if not _string_equal(got, word):
                _fail(f"expected {_prin1_string(word)}, got {_prin1_string(got)}")

        def int_(s):
            v = _read_integer(s)
            if v is None:
                _fail(f"not an integer value: {_prin1_string(s)}")
            return v

        expect("Operation")
        op = next_()
        for word in ("has", "been", "applied", "to"):
            expect(word)
        a = next_()
        va = int_(next_())
        expect("and")
        expect("to")
        b = next_()
        vb = int_(next_())
        expect("to")
        expect("get")
        r = next_()
        if _char_equal_key(r) in derivations:
            _fail(f"{r} is derived twice")
        if root is None:
            root = r
        derivations[_char_equal_key(r)] = (op, a, va, b, vb)
        for n, v in ((a, va), (b, vb)):
            key = _char_equal_key(n)
            if key in claimed and claimed[key] != v:
                _fail(f"{n} printed as both {claimed[key]} and {v}")
            claimed[key] = v
        i = p
    if root is None:
        _fail('no operation after "Done :"')

    # Rebuild the expression from the root, which must be the target.
    def expand(name, value, depth):
        if depth > 20:
            _fail(f"cycle through {name}")
        i = brick_index(name)
        if i is not None:
            if not 1 <= i <= len(bricks):
                _fail(f"{name}: there are only {len(bricks)} bricks")
            if value != bricks[i - 1]:
                _fail(f"{name} printed as {value} but brick {i} is {bricks[i - 1]}")
            if _char_equal_key(name) in used:
                _fail(f"{name} is used twice")
            used.add(_char_equal_key(name))
            return f"{value}"
        d = derivations.get(_char_equal_key(name))
        if d is None:
            _fail(f"{name} ({value}) is used but never derived, and is not a brick")
        op, a, va, b, vb = d
        plus = len(op) >= 4 and _string_equal("PLUS", op[:4])
        times = len(op) >= 5 and _string_equal("TIMES", op[:5])
        if plus and value == va + vb:
            form = ("a", "+", "b")
        elif plus and value == va - vb:
            form = ("a", "-", "b")
        elif plus and value == vb - va:
            form = ("b", "-", "a")
        elif times and value == va * vb:
            form = ("a", "x", "b")
        elif times and vb != 0 and va == value * vb:  # (= value (/ va vb)), exact
            form = ("a", "/", "b")
        elif times and va != 0 and vb == value * va:
            form = ("b", "/", "a")
        elif not (plus or times):
            _fail(f"unknown operation {op}")
        else:
            _fail(f"{op} on {va} and {vb} cannot give {name} = {value}")
        ea = expand(a, va, depth + 1)
        eb = expand(b, vb, depth + 1)

        def side(k):
            e, n = (ea, a) if k == "a" else (eb, b)
            return e if brick_index(n) is not None else f"({e})"

        return f"{side(form[0])} {form[1]} {side(form[2])}"

    return f"{target} = {expand(root, target, 0)}"

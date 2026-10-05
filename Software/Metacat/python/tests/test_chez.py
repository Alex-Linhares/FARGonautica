"""metacat/chez.py against Chez Scheme 10 (loop0002 item 02).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every expected value is Chez's: the fixtures of python/oracle/batteries/chez-battery.scm
(python/fixtures/chez/) and the Chez-level tests of tests/diff/utilities-battery.scm
(python/fixtures/utilities/).  Each test rebuilds the battery expression in
Python, in the same order of random draws, and compares b:canon's text.
"""
from __future__ import annotations

import io
import math
import re
import struct
from contextlib import redirect_stdout
from fractions import Fraction as Q

import pytest

from chez_fixtures import chez as fixture
from scheme_canon import Raw, b_num, canon, canon_with_raw
from metacat import chez
from metacat.chez import Char, Pair, String, Vector

F = Q  # exact rationals: F(1, 2) is 1/2


def chez_c(test):
    return fixture("chez", test)


def chez_u(test):
    return fixture("utilities", test)


# helpers.scm and chez-battery.scm, in Python ------------------------------------

LOG: list = []


def log(x):
    """helpers.scm: log!"""
    LOG.append(x)
    return x


def with_log(thunk):
    """helpers.scm: with-log"""
    LOG.clear()
    r = thunk()
    return [r, list(LOG)]


def iota(n):
    """helpers.scm: b:iota, 1..n"""
    return list(range(1, n + 1))


def iota0(n):
    """chez-battery.scm: c:iota0, 0..n-1"""
    return list(range(n))


def repeat(n, thunk):
    """helpers.scm: b:repeat (first to last)"""
    return [thunk() for _ in range(n)]


def seeded(seed, thunk):
    """helpers.scm: b:seeded"""
    chez.random_seed(seed)
    r = thunk()
    return [r, chez.random_seed()]


def try_(thunk):
    """chez-battery.scm: c:try; a Chez error is the symbol error"""
    try:
        return thunk()
    except chez.SchemeError:
        return "error"


SEEDS = [1, 2, 3, 7, 42, 1000, 65535, 65536, 123456789, 2147483647,
         2147483648, 4294967295, 3141592653, 2718281828, 99]


def strs(xs):
    return [String(x) for x in xs]


# The generator ------------------------------------------------------------------

RNG_ARGS = [1, 2, 3, 7, 10, 100, 1000, 65535, 65536, 65537, 1000003, 2147483647, 2147483648,
            4294967295, 4294967296, 4294967297, 1099511627776, 1152921504606846975,
            1.0, 0.5, 2.0, 100.0, 3.7, 1e-300, 1e300, 1.0, 1.0]


def draws(seed, args):
    """chez-battery.scm: c:draws, every value and the state after it"""
    chez.random_seed(seed)
    out = []
    for x in args:
        v = chez.random(x)
        out.append([v, chez.random_seed()])
    return out


def test_rng_state_after_each_draw():
    got = [draws(s, RNG_ARGS + RNG_ARGS) for s in SEEDS]
    assert canon(got) == chez_c("rng-state-after-each-draw")


def test_rng_long_run():
    args = [1.0 if i % 3 == 0 else (1 + i % 11 if i % 7 == 1 else 1.0) for i in iota(1500)]
    assert canon(draws(3852097033, args)) == chez_c("rng-long-run")


def test_rng_seed_limits():
    def case(s):
        def go():
            chez.random_seed(s)
            return [chez.random_seed(), chez.random(1000), chez.random_seed()]
        return try_(go)
    got = [case(s) for s in [1, 4294967295, 4294967296, 0, -1, 1.0, F(1, 2), "a"]]
    assert canon(got) == chez_c("rng-seed-limits")


def test_rng_bad_args():
    def case(x):
        def go():
            chez.random_seed(5)
            return [chez.random(x), chez.random_seed()]
        return try_(go)
    got = [case(x) for x in [0, -1, F(1, 2), F(3, 1), 0.0, -0.0, -2.5, "a", String("1"), 4.0]]
    assert canon(got) == chez_c("rng-bad-args")


def test_utilities_rng_ints():
    ns = [1, 2, 3, 10, 100, 1000, 65536, 1000003, 4294967295, 4294967296, 1099511627776,
          576460752303423487]
    got = chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda n: repeat(3, lambda: chez.random(n)), ns)), SEEDS)
    assert canon(got) == chez_u("rng-ints")


def test_utilities_rng_floats():
    xs = [1.0, 0.5, 100.0, 3.7, 1e-300, 1e300, 2.0]
    got = chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda x: repeat(3, lambda: chez.random(x)), xs)), SEEDS)
    assert canon(got) == chez_u("rng-floats")


def test_utilities_rng_interleaved():
    def pair():
        a = chez.random(7)
        b = chez.random(1.0)
        return [a, b]
    assert canon(seeded(17, lambda: repeat(200, pair))) == chez_u("rng-interleaved")


def test_utilities_rng_errors():
    chez.random_seed(12345)
    assert canon(chez.random_seed()) == chez_u("rng-seed-roundtrip")
    for test, thunk in [("rng-seed-zero", lambda: chez.random_seed(0)),
                        ("rng-seed-too-big", lambda: chez.random_seed(4294967296)),
                        ("rng-seed-negative", lambda: chez.random_seed(-5)),
                        ("rng-zero", lambda: chez.random(0)),
                        ("rng-negative", lambda: chez.random(-3)),
                        ("rng-rational", lambda: chez.random(F(1, 2))),
                        ("rng-zero-float", lambda: chez.random(0.0)),
                        ("rng-negative-float", lambda: chez.random(-1.0))]:
        assert chez_u(test) == "ERROR"
        with pytest.raises(chez.SchemeError):
            thunk()


def test_random_float_is_m_over_2_52():
    chez.random_seed(99)
    for _ in range(100):
        x = chez.random(1.0)
        m = Q(x) * 2**52
        assert m.denominator == 1 and 0 <= m < 2**52


# Arithmetic ------------------------------------------------------------------------

NUMS = [0, 1, 2, -3, F(1, 2), F(-2, 3), 0.0, -0.0, 1.5, 2.0, -0.25, math.inf]


def table(op):
    return [[try_(lambda: op(a, b)) for b in NUMS] for a in NUMS]


@pytest.mark.parametrize("name,op", [("add", chez.add), ("sub", chez.sub), ("mul", chez.mul),
                                     ("div", chez.div), ("max", chez.max_), ("min", chez.min_)])
def test_arith_tables(name, op):
    got = canon(table(op))
    assert got == chez_c(f"arith-{name}")
    assert got == chez_c(f"arith-{name}-inline")   # the inlined primitive agrees


def test_arith_unary():
    def case(a):
        return try_(lambda: [chez.sub(a), chez.abs_(a), chez.div(a), chez.add(a), chez.mul(a),
                             chez.max_(a), chez.zero_p(a)])
    assert canon([case(a) for a in NUMS]) == chez_c("arith-unary")


def test_arith_nary():
    got = [chez.add(), chez.mul(), chez.add(1, F(1, 2), 0.5), chez.mul(2, F(1, 2), 3),
           chez.mul(0, 1.5, 2), chez.mul(1.5, 0, 2), chez.mul(1.5, 2, 0), chez.sub(10, F(1, 2), 0.5),
           chez.div(1, 2, 3), chez.div(12, 2, 3), chez.max_(1, 2, 3), chez.max_(1, 2.0, 3),
           chez.min_(3, 2, 1.0), chez.max_(F(1, 2), F(1, 3), F(1, 4)), chez.min_(F(1, 2), F(-1, 3)),
           chez.max_(0, -0.0), chez.min_(0, -0.0), chez.max_(-0.0, 0), chez.min_(-0.0, 0)]
    assert canon(got) == chez_c("arith-nary")


def test_results_are_normalised():
    assert type(chez.add(F(1, 2), F(1, 2))) is int
    assert type(chez.mul(F(2, 3), 3)) is int
    assert type(chez.div(4, 2)) is int
    assert type(chez.sub(F(3, 2), F(1, 2))) is int
    assert type(chez.norm(F(6, 3))) is int
    assert chez.norm(F(1, 3)) == F(1, 3)


def test_integer_division():
    got = [[[chez.quotient(a, b), chez.remainder(a, b), chez.modulo(a, b)] for b in [1, 3, -3, 7, -7]]
           for a in [0, 1, 2, -2, 7, -7, 10, -10, 100]]
    assert canon(got) == chez_c("integer-division")


def test_exactness_predicates():
    got = [[chez.exact_p(a), chez.inexact_p(a), chez.integer_p(a), chez.rational_p(a),
            chez.zero_p(a), chez.positive_p(a), chez.negative_p(a)]
           for a in [0, 1, -1, F(1, 2), 0.0, -0.0, 1.0, 1.5, math.inf]]
    assert canon(got) == chez_c("exactness-predicates")


def test_one_plus():
    got = [[chez.add1(a), chez.sub1(a), chez.add1(a), chez.sub1(a)]
           for a in [0, 5, -1, F(1, 2), F(-1, 2), F(3, 2), 0.0, -0.0, 1.5, -1.0, 1e16,
                     9007199254740993]]
    assert canon(got) == chez_c("one-plus")
    assert canon([chez.add1(5), chez.add1(1.5), chez.sub1(5), chez.add1(F(1, 2)), chez.sub1(0)]) \
        == chez_u("1+")


# Rounding ---------------------------------------------------------------------------

ROUND_ARGS = [2.5, -2.5, 3.5, 0.5, -0.5, 1.5, -1.5, 0.49999999999999994, 4503599627370495.5,
              4503599627370497.0, F(7, 2), F(5, 2), F(-7, 2), F(-5, 2), F(1, 2), F(-1, 2), 3.7, -3.7,
              0.0, -0.0, 1e20, 1e300, 4, -4, 0, F(1, 3), F(-1, 3), F(2, 3), math.inf, -math.inf]


def test_chez_rounding():
    got = [try_(lambda: [chez.truncate(x), chez.ceiling(x), chez.floor_(x), chez.round_(x)])
           for x in ROUND_ARGS]
    assert canon(got) == chez_c("chez-rounding")


def test_exact_rounding():
    got = [try_(lambda: [chez.exact_truncate(x), chez.exact_ceiling(x), chez.exact_floor(x),
                         chez.exact_round(x)])
           for x in ROUND_ARGS]
    assert canon(got) == chez_c("exact-rounding")
    xs = [2.5, -2.5, 3.5, 0.5, -0.5, F(7, 2), F(5, 2), F(-7, 2), 3.7, -3.7, 1e20, 4, 0.0, -0.0,
          F(1, 3), F(2, 3), 1e300]
    got = [[chez.exact_truncate(x), chez.exact_ceiling(x), chez.exact_floor(x), chez.exact_round(x)]
           for x in xs]
    assert canon(got) == chez_u("rounding")
    assert all(type(v) is int for row in got for v in row)


def test_exact_inexact():
    got = [try_(lambda: [chez.inexact(x), chez.exact(x)])
           for x in [0, 1, -7, F(1, 3), F(2, 3), F(-22, 7), F(1, 10), F(123456789, 1000), 0.0, -0.0,
                     0.1, 1.5, 1e300, 5e-324, math.inf]]
    assert canon(got) == chez_c("exact-inexact")


def test_exact_to_inexact_ratnums():
    def one():
        n = chez.random(1000000007)
        d = 1 + chez.random(99991)
        k = chez.random(4)
        if k == 0:
            x = chez.div(n, d)
        elif k == 1:
            x = chez.div(d, n + 1)
        elif k == 2:
            x = chez.div(n * 4294967296 + d, d * 65536 + 1)
        else:
            x = chez.div(n, 100)
        return [x, chez.inexact(x)]
    assert canon(seeded(31, lambda: repeat(400, one))) == chez_c("exact->inexact-ratnums")


# sqrt, exp, log, tanh, expt ------------------------------------------------------------

RATNUMS = ([0, 1, 4, 9, 16, 100, 10000, F(1, 4), F(9, 16), F(1, 100), F(81, 100), 2, 3, F(1, 2),
            F(1, 3), F(7, 10), F(99, 100)]
           + [chez.div(k, 100) for k in iota(200)]
           + [chez.div(k * k, 49) for k in iota(20)])


def flonums():
    chez.random_seed(32)
    return ([0.0, 1.0, 2.0, 4.0, 0.25, 0.5, 0.1, 1e-300, 1e300, 123.456, 5e-324]
            + repeat(150, lambda: chez.mul(100.0, chez.random(1.0))))


def test_sqrt():
    assert canon([[x, chez.sqrt(x)] for x in RATNUMS]) == chez_c("sqrt-exact")
    assert canon([[x, chez.sqrt(x)] for x in flonums()]) == chez_c("sqrt-flonum")


def test_exp():
    assert canon([[x, chez.exp(x), chez.exp(chez.sub(x))] for x in RATNUMS]) == chez_c("exp-exact")
    assert canon([[x, chez.exp(x), chez.exp(chez.sub(x))] for x in flonums()]) == chez_c("exp-flonum")


def test_log():
    assert canon([[x, try_(lambda: chez.log(x))] for x in RATNUMS]) == chez_c("log-exact")
    assert canon([[x, chez.log(x)] for x in flonums()]) == chez_c("log-flonum")


def test_tanh():
    got = [[x, chez.tanh(x), chez.tanh(chez.sub(x)), chez.tanh(chez.mul(F(1, 40), x, 100))]
           for x in RATNUMS]
    assert canon(got) == chez_c("tanh-exact")
    got = [[x, chez.tanh(x), chez.tanh(chez.sub(x))] for x in flonums()]
    assert canon(got) == chez_c("tanh-flonum")
    got = [[raw, chez.tanh(chez.mul(F(1, 40), raw))] for raw in iota0(601)]
    assert canon(got) == chez_c("tanh-strengths")


EXPT_BASES = [0, 1, 2, 3, 10, 100, F(1, 2), F(7, 10), F(99, 100), 4, F(1, 4), 0.0, 1.0, 0.5, 0.6,
              0.95, 1.1, 1.5, 2.0, 3.7, 50.0, -2, -0.5]
EXPT_POWERS = [0, 1, 2, 3, 5, 10, 27, -1, -2, -3, F(1, 2), F(1, 3), F(1, 27), F(5, 3), F(1, 8), 0.0,
               1.0, 0.5, 0.95, 0.98, 2.0, -0.5, F(1, 1000)]


def test_expt_table():
    got = [[try_(lambda: chez.expt(b, p)) for p in EXPT_POWERS] for b in EXPT_BASES]
    assert canon(got) == chez_c("expt-table")


def test_expt_model():
    got = [[chez.expt(s, 0.95) for s in iota0(101)],
           [chez.expt(0.6, chez.div(1, n * n * n)) for n in iota(12)],
           [chez.expt(0.5, n * n * n) for n in iota(8)],
           [chez.expt(0.5, chez.mul(n, n, n, F(1, 10))) for n in iota(8)],
           [chez.expt(chez.div(d, 100), 1) for d in iota0(101)],
           [chez.expt(b, 0.98) for b in [chez.div(k, 7) for k in iota0(50)]],
           [chez.expt(v, F(3, 2)) for v in iota0(30)],
           [chez.expt(chez.div(v, 10), 2.5) for v in iota0(30)]]
    assert canon(got) == chez_c("expt-model")


# Printing ------------------------------------------------------------------------------

PAIR = re.compile(r'\("(F[^"]*|C[^"]*)" \. \("([^"]*)" \. \(\)\)\)')


def num_from_b_num(text: str) -> float:
    assert text.startswith("F")
    body = text[1:]
    if body == "-0":
        return -0.0
    return float(Q(body))


def check_flonum_pairs(fixture_text: str, minimum: int):
    """Each ("F<exact>" . ("<number->string>" . ())) in the fixture: number_to_string
    of that double gives Chez's text."""
    pairs = PAIR.findall(fixture_text)
    assert len(pairs) >= minimum
    for exact, text in pairs:
        x = num_from_b_num(exact)
        assert b_num(x) == exact
        assert chez.number_to_string(x) == text, exact


def test_number_to_string_bits():
    check_flonum_pairs(chez_c("number->string-bits"), 3000)


def test_number_to_string_decades():
    check_flonum_pairs(chez_c("number->string-decades"), 300)


def test_number_to_string_edges():
    check_flonum_pairs(chez_c("number->string-edges"), 23)


def test_utilities_number_to_string_literals():
    xs = [0.0, -0.0, 1.0, -1.0, 10.0, 100.0, 0.1, 0.5, 3.14, 123.456, 1e-7, 1e21, 1e22,
          1.2345678901234567e19, 123456789012.5, 1e10, 9999999999.0, 999999999.0,
          1e9, 1e20, 0.001, 0.0001, 0.00012, 1e-5, 2.5e-5, 5e-324, 1e-310, 2.2250738585072014e-308,
          1.5e300, 1.7976931348623157e308, 12345678901234567890.0, 99999999999999999999.0,
          1e-10, 0.3333333333333333, 0.6666666666666666, -123.5e-20, 4503599627370496.0,
          9007199254740993.0, 0.30000000000000004, 1e100, -2e-3]
    got = [strs([b_num(x), chez.number_to_string(x)]) for x in xs]
    assert canon(got) == chez_u("number->string-literals")


def test_utilities_number_to_string_random():
    scales = [1e-300, 1e-20, 1e-6, 1e-4, 1e-3, 1.0, 10.0, 1e5, 1e9, 1e10, 1e11, 1e17, 1e21, 1e300]

    def one(scale):
        x = chez.random(scale)
        return strs([b_num(x), chez.number_to_string(x)])
    got = seeded(21, lambda: chez.map_(lambda s: repeat(25, lambda: one(s)), scales))
    assert canon(got) == chez_u("number->string-random")


def test_number_to_string_exact():
    got = [chez.number_to_string(x) for x in [F(1, 2), F(-1, 2), F(102, 5), -7, 0, 2**100, -(3**70),
                                              F(1, 10**21)]]
    assert canon(strs(got)) == chez_c("number->string-exact-extra")
    # Chez's exact complex numbers (1+2i, 0+1i) have no Python counterpart and
    # never occur in Metacat: those two are taken from the fixture as is.
    got = [String(chez.number_to_string(x)) for x in [0, -7, 123456789012345678901234567890,
                                                      F(1, 3), F(-22, 7)]]
    got += [Raw('"1+2i"'), String(chez.number_to_string(complex(1.5, -2.0))), Raw('"0+1i"')]
    assert canon_with_raw(got) == chez_u("number->string-exact")
    got = [chez.number_to_string(math.inf), chez.number_to_string(-math.inf)]
    assert canon(strs(got)) == chez_u("number->string-special")
    assert chez.number_to_string(math.nan) == "+nan.0"


CODES = iota0(256) + [0x2028, 0x2029, 0x3bb, 0x10000, 0xfeff]


def test_write_chars():
    got = [[i, String(chez.format_("~s", Char(chr(i)))), String(chez.format_("~a", Char(chr(i))))]
           for i in CODES]
    assert canon(got) == chez_c("write-chars")


def test_write_strings():
    got = [[i, String(chez.format_("~s", String("a" + chr(i) + "b")))] for i in CODES]
    assert canon(got) == chez_c("write-strings")


def test_write_symbols():
    def row(i):
        c = chr(i)
        return [i] + strs(chez.format_("~s", s) for s in [c, c + "a", "a" + c, "-" + c, "+" + c, "." + c])
    assert canon([row(i) for i in CODES]) == chez_c("write-symbols")


def test_write_symbol_specials():
    syms = ["", "+", "-", "...", ".", "..", "->", "->a", "-a", "+a", "1", "1a", "a1", "1+", "-1", "+1",
            "1.5", ".5", "1/2", "a b", "#t", "#f", "#x", "|", "a|b", "\\", "{", "}", "[", "]",
            "lmost=>rmost", "[>]:*", "a.b", "a'b", "a,b", "a`b", 'a"b', "a;b", "@a", "a@", "-.", "+.",
            "-.5", "+i", "-i", "1e5", "#%car", "a#b", "LetterCtgy", "Upper"]
    assert canon(strs(chez.format_("~s", s) for s in syms)) == chez_c("write-symbol-specials")


DATA = [[], True, False, 0, -1, F(1, 2), 1.5, -0.0, String("str"), Char("a"), "sym",
        [1, [2, [3]], String("s"), Char("c"), "x"],
        Pair(1, 2), Pair(1, Pair(2, 3)), [Pair("a", "b")],
        ["quote", "x"], ["quote", ["quote", "x"]], ["quote"], ["quote", "x", "y"],
        Pair("quote", "x"), ["quasiquote", ["unquote", "y"]], ["unquote-splicing", "z"],
        ["syntax", "a"], ["quasisyntax", "a"], ["unsyntax", "a"], ["unsyntax-splicing", "a"],
        [1, ["quote", "x"], 2], ["a", "quote", "b"],
        Vector([]), Vector([1, String("a"), Char("b"), ["quote", "q"]]), [Vector([[1]])],
        "a b", [String('q"uo\\te'), Char(" "), Char("\n")],
        None, [None]]


def test_display_write():
    def row(x):
        p1, p2 = io.StringIO(), io.StringIO()
        chez.display(x, p1)
        chez.write(x, p2)
        return strs([chez.format_("~a", x), chez.format_("~s", x), p1.getvalue(), p2.getvalue()])
    assert canon([row(x) for x in DATA]) == chez_c("display-write")


def test_format_more():
    got = [String(chez.format_("~a~s~a", 1, String("two"), "three")), String(chez.format_("~N~%")),
           String(chez.format_("~~~~")), String(chez.format_("a~nb")),
           String(chez.format_("~a", [1.5, 1e21, 1e-7, F(2, 3)])),
           String(chez.format_("~s", [1.5, 1e21, String("x")])),
           try_(lambda: chez.format_("~a")), try_(lambda: chez.format_("x", 1)),
           try_(lambda: chez.format_("~q", 1)), String(chez.format_("~a+~a", 1, 2)),
           String(chez.format_("~a", [String("a"), [String("b"), Char("c")]])),
           String(chez.format_("~s", [String("a"), [String("b"), Char("c")]]))]
    assert canon(got) == chez_c("format-more")


def test_printf_newline():
    out = io.StringIO()
    with redirect_stdout(out):        # printf and newline write to the current sys.stdout
        chez.printf("~a ~s~%", String("x"), String("x"))
        chez.newline()
        chez.printf("~a", 1e22)
        chez.newline()
    assert canon(String(out.getvalue())) == chez_c("printf-newline")


def test_utilities_format_a():
    data = [1, 1.5, 1e-7, 1e21, String("str"), String('q"uo\\te'), Char("c"), "sym", "LetterCtgy",
            [], True, False, [1, 2.5, String("s"), Char("c"), "x"], ["quote", "x"],
            ["quasiquote", ["unquote", "y"]], ["unquote-splicing", "z"], ["quote", "x", "y"],
            Pair(1, 2), [1, Pair(2, 3)], Vector([1, String("a"), Char("b")]), Vector([]), "a b", "1+",
            "", [[1, 2], Vector([[3]])], F(2, 3)]
    got = [String(chez.format_("~a", x)) for x in data] + [Raw('"1+2i"')]   # exact complex: see above
    assert canon_with_raw(got) == chez_u("format-a")


def test_utilities_format_s():
    data = [1, 1.5, 1e-7, String("str"), String('q"uo\\te'), String("new\nline"), String("tab\there"),
            Char("c"), Char(" "), Char("\n"), Char("\t"), Char("\0"), "sym", "LetterCtgy", [], True,
            False, [1, 2.5, String("s"), Char("c"), "x"], ["quote", "x"],
            ["quasiquote", ["unquote", "y"]], Pair(1, 2), Vector([1, String("a"), Char("b")]),
            "a b", "1+", "+", "-", "...", "->x", "1x", "#foo", "a|b", "lmost=>rmost", "[>]:*", "Upper",
            "x;y", "q'r", "+5", "1/2", "3.5", "a(b"]
    assert canon(strs(chez.format_("~s", x) for x in data)) == chez_u("format-s")


def test_utilities_format_directives_void_printf():
    got = [chez.format_("a~%b~nc~~d"), chez.format_("~a and ~s", String("x"), String("x")),
           chez.format_(""), chez.format_("no directives"), chez.format_("~a~a~a", 1, "two", String("three")),
           chez.format_("~A ~S", String("x"), String("y"))]
    assert canon(strs(got)) == chez_u("format-directives")
    got = seeded(22, lambda: String(chez.format_("~a ~s", repeat(5, lambda: chez.random(1e12)),
                                                 [1e-5, Vector([2e30])])))
    assert canon(got) == chez_u("format-numbers-in-lists")
    assert canon(String(chez.format_("~a", None))) == chez_u("format-void")
    out = io.StringIO()
    with redirect_stdout(out):
        chez.printf("x=~a y=~s~%", 1.5, String("s"))
        chez.newline()
        chez.printf("~a", "done")
    assert canon(String(out.getvalue())) == chez_u("printf-output")


# Lists ------------------------------------------------------------------------------------

def test_utilities_map_orders():
    got = chez.map_(lambda n: with_log(lambda: chez.map_(log, iota(n))), iota(9))
    assert canon(got) == chez_u("map1-order")
    got = chez.map_(lambda n: with_log(lambda: chez.map_(lambda a, b: log(a + 10 * b), iota(n), iota(n))),
                    iota(9))
    assert canon(got) == chez_u("map2-order")
    got = chez.map_(lambda n: with_log(lambda: chez.map_(lambda a, b, c: log(a), iota(n), iota(n), iota(n))),
                    iota(7))
    assert canon(got) == chez_u("map3-order")
    m = chez.map_
    assert canon(with_log(lambda: m(log, iota(7)))) == chez_u("map-as-value")
    assert canon(with_log(lambda: chez.map_(log, *[iota(7)]))) == chez_u("apply-map")
    assert canon(with_log(lambda: chez.for_each(log, iota(6)))) == chez_u("for-each-order")
    assert canon(with_log(lambda: chez.for_each(lambda a, b: log([a, b]), iota(4), iota(4)))) \
        == chez_u("for-each2-order")
    assert canon(with_log(lambda: chez.andmap(lambda x: (log(x), x < 4)[1], iota(6)))) \
        == chez_u("andmap-order")
    assert canon(with_log(lambda: chez.ormap(lambda x: (log(x), x > 3)[1], iota(6)))) \
        == chez_u("ormap-order")


def test_map_orders_long():
    got = [with_log(lambda: chez.map_(log, iota(n))) for n in [0, 10, 11, 25, 26, 31]]
    assert canon(got) == chez_c("map1-order-long")
    got = [with_log(lambda: chez.map_(lambda a, b: log(a + 100 * b), iota(n), iota(n)))
           for n in [0, 1, 10, 11, 25]]
    assert canon(got) == chez_c("map2-order-long")
    got = [with_log(lambda: chez.map_(lambda a, b, c: log(a + b + c), iota(n), iota(n), iota(n)))
           for n in [0, 1, 8, 13]]
    assert canon(got) == chez_c("map3-order-long")
    got = with_log(lambda: chez.map_(lambda a, b, c, d: log([a, b, c, d]), *[iota(5)] * 4))
    assert canon(got) == chez_c("map4-order")
    got = [try_(lambda: chez.map_(lambda *a: list(a), [1, 2], [1])),
           try_(lambda: chez.map_(lambda *a: list(a), [1], [1, 2])),
           try_(lambda: chez.map_(lambda *a: list(a), [1, 2], [1, 2], [1]))]
    assert canon(got) == chez_c("map-length-mismatch")


def test_for_each_values():
    got = [chez.for_each(lambda x: x * 10, iota(3)),
           chez.for_each(lambda x: x, []),
           chez.for_each(lambda a, b: [a, b], iota(3), ["a", "b", "c"]),
           chez.for_each(lambda a, b, c: a + b + c, iota(3), iota(3), iota(3)),
           with_log(lambda: chez.for_each(lambda a, b, c: log([a, b, c]), iota(3), iota(3), iota(3))),
           try_(lambda: chez.for_each(lambda *a: list(a), [1, 2], [1]))]
    assert canon(got) == chez_c("for-each-values")


def test_andmap_ormap_values():
    got = [chez.andmap(lambda x: x, []), chez.ormap(lambda x: x, []),
           chez.andmap(lambda x: x < 5 and x * 10, iota(3)),
           chez.ormap(lambda x: x > 1 and x * 10, iota(3)),
           chez.andmap(lambda a, b: a < b, [1, 2], [2, 3]),
           chez.ormap(lambda a, b: a == b and [a, b], [1, 2], [0, 2]),
           with_log(lambda: chez.andmap(lambda a, b: (log([a, b]), a < 2)[1], iota(4), iota(4))),
           with_log(lambda: chez.ormap(lambda a, b: (log([a, b]), a > 2)[1], iota(4), iota(4)))]
    assert canon(got) == chez_c("andmap-ormap-values")


def logged(cmp):
    return lambda a, b: (log([a, b]), cmp(a, b))[1]


def test_utilities_sorts():
    def lists(n):
        lst = repeat(n, lambda: chez.random(10))
        return [chez.sort(lambda a, b: a < b, lst), chez.sort(lambda a, b: a > b, lst),
                chez.sort(lambda a, b: a <= b, lst), chez.sort(lambda a, b: a >= b, lst)]
    sizes = [0, 1, 2, 3, 4, 5, 8, 13, 24, 25, 26, 40, 61, 100]
    assert canon(seeded(11, lambda: chez.map_(lists, sizes))) == chez_u("sort-lists")

    def stable(n):
        lst = chez.map_(lambda i: Pair(chez.random(4), i), iota(n))
        return [chez.sort(lambda a, b: a.car < b.car, lst), chez.sort(lambda a, b: a.car <= b.car, lst)]
    assert canon(seeded(12, lambda: chez.map_(stable, [2, 3, 7, 24, 25, 30, 57]))) == chez_u("sort-stable")

    def calls(n):
        lst = repeat(n, lambda: chez.random(6))
        return with_log(lambda: chez.sort(logged(lambda a, b: a < b), lst))
    got = seeded(13, lambda: chez.map_(calls, [2, 3, 5, 9, 24, 25, 33]))
    assert canon(got) == chez_u("sort-call-order")


def test_sort_call_order_long():
    def one(n):
        lst = repeat(n, lambda: chez.random(5))
        return [with_log(lambda: chez.sort(logged(lambda a, b: a < b), lst)),
                with_log(lambda: chez.sort(logged(lambda a, b: a <= b), lst)),
                with_log(lambda: chez.sort(logged(lambda a, b: a > b), lst))]
    sizes = [0, 1, 4, 6, 7, 8, 15, 16, 17, 23, 24, 25, 26, 27, 31, 32, 33, 48, 49, 50, 63, 64, 65,
             100, 101, 150]
    assert canon(seeded(51, lambda: [one(n) for n in sizes])) == chez_c("sort-call-order-long")


def test_sort_pairs_and_presorted():
    def one(n):
        lst = [Pair(chez.random(3), i) for i in iota(n)]
        return [chez.sort(lambda a, b: a.car < b.car, lst), chez.sort(lambda a, b: a.car <= b.car, lst),
                chez.sort(lambda a, b: a.car > b.car, lst), chez.sort(lambda a, b: a.car >= b.car, lst)]
    got = seeded(52, lambda: [one(n) for n in [5, 12, 24, 25, 40, 77, 128, 200]])
    assert canon(got) == chez_c("sort-pairs")
    lists = [iota(30), iota(30)[::-1], iota(15) + iota(15), iota(15)[::-1] + iota(15), [1] * 30]
    got = [with_log(lambda: chez.sort(logged(lambda a, b: a < b), lst)) for lst in lists]
    assert canon(got) == chez_c("sort-presorted")


def test_sort_does_not_mutate():
    lst = [3, 1, 2] * 10
    copy = list(lst)
    chez.sort(lambda a, b: a < b, lst)
    assert lst == copy


def test_rem_family():
    got = [chez.remq("a", ["a", "b", "a", "c", "a"]), chez.remq("a", ["a", "a"]), chez.remq("z", []),
           chez.remv(2, [2, 2.0, F(1, 2), 2]), chez.remv(2.0, [2, 2.0, F(1, 2), 2.0]),
           chez.remv(F(1, 2), [F(1, 2), 0.5, F(1, 2)]), chez.remv(0.0, [0.0, -0.0, 0]),
           chez.remv("a", ["a", "b"]),
           chez.remove(2, [2, 2.0, 2]), chez.remove([1, 2], [[1, 2], [1, 2.0], 3, [1, 2]]),
           chez.remove(String("ab"), [String("ab"), String("a"), String("ab")]),
           chez.remove(Vector([1]), [Vector([1]), 1]), chez.remove("x", ["x", "y", "x"])]
    assert canon(got) == chez_c("rem-family")
    assert canon([chez.remq("a", ["a", "b", "a", "c"]), chez.remq("z", ["a"]), chez.remq("a", [])]) \
        == chez_u("remq")
    assert canon([chez.remv(1, [1, 2, 1, 3]), chez.remv(1.5, [1.5, 2])]) == chez_u("remv")
    assert canon([chez.remove(String("a"), [String("a"), String("b"), String("a")]),
                  chez.remove([1], [[1], 2])]) == chez_u("remove")


def test_mem_ass_family():
    got = [chez.memq("c", ["a", "b", "c", "d"]), chez.memq("z", ["a"]), chez.memv(2.0, [2, 2.0, 3]),
           chez.memv(F(1, 2), [0.5, F(1, 2)]), chez.member([1], [2, [1], 3]), chez.member(2.0, [2, 2.0]),
           chez.assq("b", [["a", 1], ["b", 2]]), chez.assv(2, [[2.0, "x"], [2, "y"]]),
           chez.assoc([1], [[[1], "z"]]), chez.assq("z", [["a", 1]])]
    assert canon(got) == chez_c("mem-ass-family")


def test_eqv_equal():
    a_list, a_vec = [1, 2], Vector([1, 2])
    pairs = [(2, 2), (2, 2.0), (F(1, 2), F(1, 2)), (F(1, 2), 0.5), (0.0, -0.0), (0.0, 0.0),
             (math.nan, math.nan), (1.5, 1.5), ("a", "a"), (String("a"), String("a")), (Char("a"), Char("a")),
             ([1, 2], [1, 2]), ([1, 2], [1, 2.0]), ([], []), (Vector([1, 2]), Vector([1, 2])), (True, True),
             (False, []), (0, False)]
    got = [[chez.eqv_p(a, b), chez.equal_p(a, b)] for a, b in pairs]
    assert canon(got) == chez_c("eqv-equal")
    assert chez.eqv_p(a_list, a_list) and chez.eqv_p(a_vec, a_vec)
    assert chez.eq_p("sym", "s" + "ym") and chez.eq_p(5, 5) and not chez.eq_p(True, 1)
    assert not chez.eq_p([1], [1]) and chez.eq_p([], [])


def test_top_level_values():
    name = "c:tlv-a"
    for n in ("c:never-defined", name, "c:tlv-b"):
        chez.TOP_LEVEL.pop(n, None)
    got = [chez.top_level_bound_p("c:never-defined"),
           try_(lambda: chez.top_level_value("c:never-defined"))]
    chez.define_top_level_value(name, 1)
    got.append(chez.top_level_value(name))
    chez.define_top_level_value(name, 2)
    got.append(chez.top_level_value(name))
    chez.set_top_level_value_bang(name, 3)
    got.append(chez.top_level_value(name))
    got.append(chez.top_level_bound_p(name))

    def unbound_set():
        chez.set_top_level_value_bang("c:tlv-b", 4)
        return [chez.top_level_bound_p("c:tlv-b"), chez.top_level_value("c:tlv-b")]
    got.append(try_(unbound_set))
    assert canon(got) == chez_c("top-level-values")
    with pytest.raises(chez.UnboundVariable):
        chez.top_level_value("c:never-defined")


def test_chez_module_imports_no_gui():
    import ast
    import inspect
    tree = ast.parse(inspect.getsource(chez))
    names = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
             for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
    assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in names)


def test_number_to_string_ties():
    check_flonum_pairs(chez_c("number->string-ties"), 131 * 40)

    def digits(text):
        return text.split("e")[0].replace("-", "").replace(".", "").strip("0")
    ties = [t for e, t in PAIR.findall(chez_c("number->string-ties"))
            if digits(t) != digits(repr(num_from_b_num(e)))]
    assert len(ties) >= 10    # Python's repr differs here: the battery exercises the tie rule


def test_expt_extra():
    cases = [(8, F(1, 3)), (8, F(2, 3)), (F(1, 8), F(1, 3)), (4, F(3, 2)), (9, F(-1, 2)), (2, F(1, 2)),
             (0, F(1, 2)), (0, F(-1, 2)), (0.0, F(1, 2)), (-8, F(1, 3)), (1.0, 0), (0, 0),
             (2.5, 0), (-0.0, -1), (-0.0, -2), (1, F(-1, 2)), (1.5, 1), (0, 0.5),
             (16, F(1, 4)), (0.0, 0.0), (1.1, 3), (1.1, 27), (0.95, -3), (F(1, 9), F(1, 2)),
             (F(1, 9), F(-1, 2)), (0.0, -0.5), (-0.0, -0.5), (-0.0, -3), (-0.0, 3),
             (-2, 3), (-2, -3), (F(-1, 2), 3), (10, -1), (1e300, 2), (2, 1024),
             (2.0, 1024), (4, F(1, 2)), (4.0, F(1, 2)), (-4.0, F(1, 2)), (-2, F(1, 2))]
    got = [try_(lambda: chez.expt(b, p)) for b, p in cases]
    assert canon(got) == chez_c("expt-extra")

"""numbo.franz reproduces the Franz built-ins of lisp/src/franz-compat.lisp.

Expected values: python/fixtures/franz_cases.json, written by
lisp/tests/oracle/franz-cases.lisp (the oracle applies each built-in to literal
arguments).  The unit tests below port the remaining checks of
lisp/tests/franz-compat-tests.lisp that are not about a value (identity of
interned symbols, the copying sortcar, the case rule).
"""

import json

import pytest

from conftest import load_fixture
from conftest import lisp_data as encode, lisp_data_decode as decode
from numbo import franz
from numbo.franz import Symbol

CASES = load_fixture("franz_cases.json")["cases"]

ERRORS = {"DIVISION-BY-ZERO": ZeroDivisionError}

PREDICATES = {"<": lambda a, b: a < b, ">": lambda a, b: a > b}


def brick_keyword(i):
    """codelets.lisp read-brick, line 1191."""
    return franz.intern(franz.uconcat("brick", i), franz.find_package("keyword"))


def python_function(op):
    if op == "brick-keyword":
        return brick_keyword
    return getattr(franz, franz.LISP_NAMES[op])


def case_id(case):
    return f"({case['op']} {json.dumps(case['args'])[1:-1] if case['args'] else ''})"


@pytest.mark.parametrize("case", CASES, ids=[case_id(c) for c in CASES])
def test_oracle_case(case):
    fn = python_function(case["op"])
    args = decode(case["args"] or [])
    if case["op"] == "sortcar" and args[1] is not None:
        args[1] = PREDICATES[args[1].name]
    if "error" in case:
        with pytest.raises(ERRORS[case["error"]]):
            fn(*args)
        return
    got = fn(*args)
    # json.dumps keeps 3 and 3.0 (and 0.0 and -0.0) apart.
    assert json.dumps(encode(got)) == json.dumps(case["result"])
    if "package" in case:
        assert isinstance(got, Symbol)
        assert got.package == case["package"]


def test_fixture_covers_every_value_helper():
    ops = {c["op"] for c in CASES}
    assert ops == {"quotient", "/", "*quo", "mod", "*mod", "fix", "max", "min",
                   "nequal", "alphalessp", "sortcar", "concat", "uconcat",
                   "get-pname", "string-length", "brick-keyword", "memq",
                   "member"}
    # negative operands for every division helper
    for op in ("quotient", "/", "*quo", "mod", "*mod"):
        assert any(any(isinstance(a, int) and a < 0 for a in c["args"])
                   for c in CASES if c["op"] == op), op


# --- ported from lisp/tests/franz-compat-tests.lisp ------------------------------

def test_concat_interns():
    assert franz.concat(franz.intern("NODE-"), 31) is franz.intern("NODE-31")
    assert franz.concat("brick", 2) is franz.concat(franz.intern("BRICK"), 2)
    assert franz.concat(franz.intern("A"), franz.intern("B")).package == "NUMBO"


def test_uconcat_is_uninterned():
    a, b = franz.uconcat(franz.intern("A")), franz.uconcat(franz.intern("A"))
    assert a is not b
    assert a.name == "A" and a.package is None
    assert a is not franz.intern("A")
    assert franz.nequal(a, b)


def test_symbols_and_strings_differ():
    assert franz.nequal(franz.intern("FREE"), "free")
    assert franz.nequal(franz.intern("FREE"), "FREE")
    assert not franz.nequal("free", "free")


def test_find_package():
    assert franz.find_package("keyword") == "KEYWORD"
    assert franz.find_package("KEYWORD") == "KEYWORD"
    assert franz.find_package(franz.intern("NUMBO")) == "NUMBO"
    assert franz.find_package("numbo") == "NUMBO"
    assert franz.find_package("no-such-package") is None
    assert franz.intern(franz.intern("BAR")) is franz.intern("BAR")


def test_invert_case():
    assert franz.invert_case("abc") == "ABC"
    assert franz.invert_case("ABC") == "abc"
    assert franz.invert_case("aBc") == "aBc"
    assert franz.invert_case("2b") == "2B"
    assert franz.invert_case("") == ""


def test_round_relies_on_star_mod_and_divide():
    """codelets.lisp round: nearest multiple of 10 to 31 is 30, to 36 is 40."""
    def round_(num, div):
        if franz.star_mod(num, div) < 0:
            return div * (1 + franz.divide(num, div))
        return div * franz.divide(num, div)
    assert round_(31, 10) == 30
    assert round_(36, 10) == 40


def test_star_mod_range():
    assert all(-4 <= franz.star_mod(i, 10) <= 5 for i in range(100))
    # i - (*mod i 7) is a multiple of 7 (a zero remainder has no sign issue)
    assert all((i - franz.star_mod(i, 7)) % 7 == 0 for i in range(-50, 50))


def test_sortcar_copies():
    """Oracle mode's sortcar sorts a copy: the caller's list is unchanged."""
    instances = [["2b", "x"], ["1t", "y"], ["3dt", "z"]]
    before = [list(p) for p in instances]
    got = franz.sortcar(instances, None)
    assert instances == before
    assert got is not instances
    assert [p[0] for p in got] == ["1t", "2b", "3dt"]
    assert got[0] is instances[1]   # the elements are shared, not copied


def test_sortcar_empty_is_nil():
    assert encode(franz.sortcar([], None)) is None
    assert encode(franz.sortcar(None, None)) is None


def test_memq_refuses_strings():
    """Lisp strings have identity, Python's don't: (memq "b" ...) can't be
    modelled.  The source only memq's cyto-node objects (codelets.lisp:727)."""
    with pytest.raises(TypeError):
        franz.memq("b", ["a", "b"])


def test_memq_identity():
    a, b = object(), object()
    assert franz.memq(b, [a, b]) == [b]
    assert franz.memq(object(), [a, b]) is None


def test_equal_nil():
    """nil is both None and the empty list."""
    assert not franz.nequal(None, [])
    assert not franz.nequal([None], [[]])
    assert franz.nequal(None, [None])
    assert franz.nequal(True, 1)
    assert franz.nequal(0, False)


def test_min_needs_an_argument():
    with pytest.raises(TypeError):
        franz.min_()


def test_subnormal_print_name_refused():
    """SBCL prints subnormal doubles with more digits than Python's repr."""
    with pytest.raises(ValueError):
        franz.get_pname(5e-324)
    assert franz.get_pname(2.2250738585072014e-308) == "2.2250738585072014d-308"


def test_get_pname_float():
    assert franz.get_pname(2.5) == "2.5d0"
    assert franz.get_pname(1e7) == "1.0d7"
    assert franz.get_pname(-0.0) == "-0.0d0"

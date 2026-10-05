"""objects.py, sugar.py and utilities.py against Chez (loop0002 item 03).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

Every test of tests/diff/utilities-battery.scm, translated: CASES maps each test
name to a function that rebuilds the battery expression in Python, in the same
order of random draws and side effects, and returns its value.  The value's
b:canon text must equal the frozen Chez output in python/fixtures/utilities/;
a fixture of ERROR means the case must raise chez.SchemeError.
test_every_battery_test_is_translated checks that CASES has exactly the
battery's 197 names.

The battery's own definitions (make-fake, the stand-ins for make-slipnode,
establish-link, make-codelet-type, *coderack* and *control-panel*) are
translated here too.  The macros' free names (%verbose%, *control-panel*,
*coderack*, make-slipnode, establish-link, make-codelet-type) live in later
modules (setup, slipnet, coderack); `engine_module` provides them, as the
battery's (define ...) forms do.
"""
from __future__ import annotations

import importlib
import importlib.util
import io
import math
import sys
import types
from contextlib import contextmanager, redirect_stdout
from fractions import Fraction as F

import pytest

from chez_fixtures import chez as fixture, manifest
from engine_stubs import engine_module
from scheme_canon import canon
from test_chez import LOG, SEEDS, iota, log, repeat, seeded, with_log  # helpers.scm
from metacat import chez
from metacat.chez import Char, Pair, String, Vector

import metacat
from metacat import objects, sugar, utilities as u
from metacat.objects import INVALID, Lambda, SchemeObject, message, tell


@pytest.fixture(scope="module", autouse=True)
def restore_top_level():
    """The battery's slipnet-macro cases define fake plato-p, plato-q, plato-z ...
    and links as top-level values; restore the engine's afterwards, so that
    the test files that run later see the real slipnodes."""
    saved = dict(chez.TOP_LEVEL)
    yield
    chez.TOP_LEVEL.clear()
    chez.TOP_LEVEL.update(saved)

CASES: dict = {}


def case(name):
    def register(fn):
        assert name not in CASES, name
        CASES[name] = fn
        return fn
    return register


def capture(thunk):
    """b:capture: what thunk prints, as a Scheme string."""
    out = io.StringIO()
    with redirect_stdout(out):
        thunk()
    return String(out.getvalue())


def strs(xs):
    return [String(x) for x in xs]


def seq(*values):
    """(begin e1 e2 ...): the arguments are already evaluated, in order; the last value."""
    return values[-1]


# The battery's fake object ----------------------------------------------------------

class Fake(SchemeObject):
    """utilities-battery.scm: make-fake"""
    __slots__ = ("type_", "value")

    def __init__(this, type_, value):
        this.type_ = type_
        this.value = value

    @message("object-type")
    def object_type(this, self):
        return this.type_

    @message("get-value", "get-weight")
    def get_value(this, self):
        return this.value

    @message("print-name")
    def print_name(this, self):
        return "fake-" + chez.number_to_string(this.value)

    @message("ascii-name")
    def ascii_name(this, self):
        return "f" + chez.number_to_string(this.value)

    @message("get-orientation")
    def get_orientation(this, self):
        return "vertical" if this.value % 2 == 0 else "horizontal"

    @message("get-conceptual-depth")
    def get_conceptual_depth(this, self):
        return chez.mul(10, this.value)

    @message("print")
    def print_(this, self):
        return chez.printf("<fake ~a>~%", this.value)

    @message("bump")
    def bump(this, self, n):
        log(["bump", this.value, n])
        return chez.add(this.value, n)

    @message("both")
    def both(this, self, a, *more):
        return [a, list(more)]

    @message("alias1", "alias2")
    def aliased(this, self):
        return "aliased"

    def otherwise(this, self, msg, args):
        return INVALID


def make_fake(type_, value):
    return Fake(type_, value)


FAKES = chez.map_(lambda n: make_fake("thing", n), iota(6))


# The generator ------------------------------------------------------------------------

@case("rng-ints")
def _():
    ns = [1, 2, 3, 10, 100, 1000, 65536, 1000003, 4294967295, 4294967296, 1099511627776,
          576460752303423487]
    return chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda n: repeat(3, lambda: chez.random(n)), ns)), SEEDS)


@case("rng-floats")
def _():
    xs = [1.0, 0.5, 100.0, 3.7, 1e-300, 1e300, 2.0]
    return chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda x: repeat(3, lambda: chez.random(x)), xs)), SEEDS)


@case("rng-interleaved")
def _():
    def pair():
        a = chez.random(7)
        b = chez.random(1.0)
        return [a, b]
    return seeded(17, lambda: repeat(200, pair))


@case("rng-seed-roundtrip")
def _():
    chez.random_seed(12345)
    return chez.random_seed()


for _name, _thunk in [("rng-seed-zero", lambda: chez.random_seed(0)),
                      ("rng-seed-too-big", lambda: chez.random_seed(4294967296)),
                      ("rng-seed-negative", lambda: chez.random_seed(-5)),
                      ("rng-zero", lambda: chez.random(0)),
                      ("rng-negative", lambda: chez.random(-3)),
                      ("rng-rational", lambda: chez.random(F(1, 2))),
                      ("rng-zero-float", lambda: chez.random(0.0)),
                      ("rng-negative-float", lambda: chez.random(-1.0))]:
    case(_name)(_thunk)


# Random utilities --------------------------------------------------------------------

@case("prob?")
def _():
    ps = [0, 0.0, -1, 1, 1.0, 2, 0.5, 0.001, 0.999, F(1, 3), F(2, 3), 0.25]
    return chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda p: repeat(4, lambda: u.prob_p(p)), ps)), SEEDS)


@case("~")
def _():
    ns = [0, 1, 2, 4, 10, 100, 2.5, F(7, 2)]
    return chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda n: repeat(4, lambda: u.rough(n)), ns)), SEEDS)


@case("random-pick")
def _():
    return chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda n: repeat(3, lambda: u.random_pick(iota(n))), iota(12))), SEEDS)


@case("random-pick-empty")
def _():
    return seeded(5, lambda: u.random_pick([]))


@case("stochastic-pick")
def _():
    weights = [[1], [1, 1], [0, 0, 0], [0, 5, 0], [1, 2, 3, 4], [0.5, 0.25, 0.25], [F(1, 3), F(2, 3)],
               [100, 1, 1, 1, 1], [7, 0, 0, 7], [10.5, 20, 30.25]]
    return chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda w: repeat(5, lambda: u.stochastic_pick(iota(len(w)), w)), weights)), SEEDS)


@case("stochastic-pick-by-method")
def _():
    return chez.map_(lambda seed: seeded(seed, lambda: repeat(
        6, lambda: tell(u.stochastic_pick_by_method(FAKES, "get-weight"), "get-value"))), SEEDS)


@case("weighted-index")
def _():
    ws = [0, 5.9, 6, 7.99, 8, 11.5, 12, 17, 18.9, 19, 20.5, 21, 24.99]
    return chez.map_(lambda w: u.weighted_index(w, [6, 2, 4, 7, 2, 4]), ws)


@case("weighted-index-past-end")
def _():
    return u.weighted_index(100, [1, 2])


@case("stochastic-select")
def _():
    sels = [[[1, "a"]],
            [[1, "a"], [3, "b", "c"], [0, "d"]],
            [[0, "x"], [0, "y"]],
            [[0.5, "p"], [F(1, 2), "q"], [2, "r"]]]
    return chez.map_(lambda seed: seeded(seed, lambda: chez.map_(
        lambda sel: repeat(5, lambda: u.stochastic_select(sel)), sels)), SEEDS)


@case("weighted-select")
def _():
    return chez.map_(lambda w: u.weighted_select(w, [[6, "a"], [2, "b"], [4, "c", 1]]),
                     [0, 5.99, 6, 7.5, 8, 11.99])


@case("stochastic-filter")
def _():
    return chez.map_(lambda seed: seeded(seed, lambda: repeat(
        4, lambda: u.stochastic_filter(lambda x: chez.div(x, 10), iota(12)))), SEEDS)


@case("bounded-random-partition")
def _():
    def five():
        a = u.bounded_random_partition(lambda x, y: True, iota(10), 3)
        b = u.bounded_random_partition(lambda x, y: (x % 2 == 0) == (y % 2 == 0), iota(11), 2)
        c = u.bounded_random_partition(lambda x, y: abs(x - y) < 3, iota(9), 4)
        d = u.bounded_random_partition(lambda x, y: False, iota(4), 5)
        e = u.bounded_random_partition(lambda x, y: True, [], 5)
        return [a, b, c, d, e]
    return chez.map_(lambda seed: seeded(seed, five), SEEDS)


@case("randomize-range")
def _():
    u.randomize()
    s = chez.random_seed()
    return chez.integer_p(s) and s > 0 and s < 4294967296


# Evaluation order ---------------------------------------------------------------------

@case("map1-order")
def _():
    return chez.map_(lambda n: with_log(lambda: chez.map_(log, iota(n))), iota(9))


@case("map2-order")
def _():
    return chez.map_(lambda n: with_log(lambda: chez.map_(lambda a, b: log(a + 10 * b), iota(n), iota(n))),
                     iota(9))


@case("map3-order")
def _():
    return chez.map_(lambda n: with_log(lambda: chez.map_(lambda a, b, c: log(a), iota(n), iota(n), iota(n))),
                     iota(7))


@case("map-as-value")
def _():
    m = chez.map_
    return with_log(lambda: m(log, iota(7)))


@case("apply-map")
def _():
    return with_log(lambda: chez.map_(log, *[iota(7)]))


@case("for-each-order")
def _():
    return with_log(lambda: chez.for_each(log, iota(6)))


@case("for-each2-order")
def _():
    return with_log(lambda: chez.for_each(lambda a, b: log([a, b]), iota(4), iota(4)))


@case("andmap-order")
def _():
    return with_log(lambda: chez.andmap(lambda x: seq(log(x), x < 4), iota(6)))


@case("ormap-order")
def _():
    return with_log(lambda: chez.ormap(lambda x: seq(log(x), x > 3), iota(6)))


@case("tell-all-order")
def _():
    return with_log(lambda: u.tell_all(FAKES, "bump", 100))


@case("delegate-to-all-order")
def _():
    return with_log(lambda: u.delegate_to_all("self", "bump", (7,), FAKES[0], FAKES[1], FAKES[2]))


@case("delegate-to-all-invalid")
def _():
    return with_log(lambda: u.delegate_to_all("self", "nope", (), FAKES[0], FAKES[1]))


@case("flatmap-order")
def _():
    return with_log(lambda: u.flatmap(lambda x: seq(log(x), [x, x]), iota(5)))


@case("map-compress-order")
def _():
    return with_log(lambda: u.map_compress(lambda x: seq(log(x), x if x % 2 == 1 else False), iota(7)))


@case("cross-product-map-order")
def _():
    return with_log(lambda: u.cross_product_map(lambda x, y: log([x, y]), iota(3), iota(2)))


@case("cross-product-filter-map-order")
def _():
    return with_log(lambda: u.cross_product_filter_map(
        lambda x, y: seq(log(["p", x, y]), (x + y) % 2 == 1),
        lambda x, y: log(["f", x, y]), iota(3), iota(3)))


@case("cross-product-map-filter-order")
def _():
    return with_log(lambda: u.cross_product_map_filter(
        lambda x, y: seq(log(["f", x, y]), x + y),
        lambda v: seq(log(["p", v]), v % 2 == 1), iota(3), iota(3)))


@case("cross-product-ormap-order")
def _():
    return with_log(lambda: u.cross_product_ormap(lambda x, y: seq(log([x, y]), x + y == 5), iota(3), iota(3)))


@case("cross-product-andmap-order")
def _():
    return with_log(lambda: u.cross_product_andmap(lambda x, y: seq(log([x, y]), x + y < 5), iota(3), iota(3)))


@case("cross-product-for-each-order")
def _():
    return with_log(lambda: u.cross_product_for_each(lambda x, y: log([x, y]), iota(2), iota(3)))


@case("pairwise-map-order")
def _():
    return with_log(lambda: u.pairwise_map(lambda x, y: log([x, y]), iota(5)))


@case("pairwise-do-order")
def _():
    return with_log(lambda: u.pairwise_do(lambda x, y: log([x, y]), iota(4)))


@case("pairwise-andmap-order")
def _():
    return with_log(lambda: u.pairwise_andmap(lambda x, y: seq(log([x, y]), x + y < 6), iota(5)))


@case("filter-order")
def _():
    return with_log(lambda: u.filter_(lambda x: seq(log(x), x % 2 == 1), iota(6)))


@case("filter-map-order")
def _():
    return with_log(lambda: u.filter_map(lambda x: seq(log(["p", x]), x % 2 == 1),
                                         lambda x: log(["f", x]), iota(5)))


@case("map-filter-order")
def _():
    return with_log(lambda: u.map_filter(lambda x: log(["f", x]), lambda v: seq(log(["p", v]), True), iota(4)))


@case("map-leaves-order")
def _():
    return with_log(lambda: u.map_leaves(log, [1, [2, [3, 4]], 5, [6]]))


@case("select-extreme-order")
def _():
    return with_log(lambda: u.select_extreme(chez.max_, lambda x: seq(log(x), 10 - x), iota(6)))


@case("adjacency-map-order")
def _():
    return with_log(lambda: u.adjacency_map(lambda x, y: log([x, y]), iota(5)))


@case("count-order")
def _():
    return with_log(lambda: u.count(lambda x: seq(log(x), x % 2 == 1), iota(6)))


@case("partition-order")
def _():
    return with_log(lambda: u.partition(lambda x, y: seq(log([x, y]), (x % 2) == (y % 2)), iota(5)))


@case("bounded-random-partition-order")
def _():
    return seeded(3, lambda: with_log(lambda: u.bounded_random_partition(
        lambda x, y: seq(log([x, y]), True), iota(6), 2)))


@case("stochastic-filter-order")
def _():
    return seeded(3, lambda: with_log(lambda: u.stochastic_filter(lambda x: seq(log(x), 0.5), iota(6))))


@case("intersect-pred-order")
def _():
    return with_log(lambda: u.intersect_pred(lambda x, y: seq(log([x, y]), x == y), iota(3), iota(4)))


@case("remove-duplicates-pred-order")
def _():
    return with_log(lambda: u.remove_duplicates_pred(lambda x, y: seq(log([x, y]), x == y))([1, 2, 1, 3, 2]))


@case("sort-by-method-order")
def _():
    return with_log(lambda: chez.map_(lambda o: tell(o, "get-value"),
                                      u.sort_by_method("get-value", lambda a, b: seq(log([a, b]), a > b),
                                                       FAKES)))


# sort -----------------------------------------------------------------------------------

def random_list(n, k):
    return repeat(n, lambda: chez.random(k))


@case("sort-lists")
def _():
    def lists(n):
        lst = random_list(n, 10)
        return [chez.sort(lambda a, b: a < b, lst), chez.sort(lambda a, b: a > b, lst),
                chez.sort(lambda a, b: a <= b, lst), chez.sort(lambda a, b: a >= b, lst)]
    return seeded(11, lambda: chez.map_(lists, [0, 1, 2, 3, 4, 5, 8, 13, 24, 25, 26, 40, 61, 100]))


@case("sort-stable")
def _():
    def stable(n):
        lst = chez.map_(lambda i: Pair(chez.random(4), i), iota(n))
        return [chez.sort(lambda a, b: a.car < b.car, lst), chez.sort(lambda a, b: a.car <= b.car, lst)]
    return seeded(12, lambda: chez.map_(stable, [2, 3, 7, 24, 25, 30, 57]))


@case("sort-call-order")
def _():
    def calls(n):
        lst = random_list(n, 6)
        return with_log(lambda: chez.sort(lambda a, b: seq(log([a, b]), a < b), lst))
    return seeded(13, lambda: chez.map_(calls, [2, 3, 5, 9, 24, 25, 33]))


@case("sort-wrt-order")
def _():
    return u.sort_wrt_order(["c", "a", "d", "b"], ["a", "b", "c", "d", "e"])


# Printing ---------------------------------------------------------------------------------

FLONUMS = [0.0, -0.0, 1.0, -1.0, 10.0, 100.0, 0.1, 0.5, 3.14, 123.456, 1e-7, 1e21, 1e22,
           1.2345678901234567e19, 123456789012.5, 1e10, 9999999999.0, 999999999.0,
           1e9, 1e20, 0.001, 0.0001, 0.00012, 1e-5, 2.5e-5, 5e-324, 1e-310, 2.2250738585072014e-308,
           1.5e300, 1.7976931348623157e308, 12345678901234567890.0, 99999999999999999999.0,
           1e-10, 0.3333333333333333, 0.6666666666666666, -123.5e-20, 4503599627370496.0,
           9007199254740993.0, 0.30000000000000004, 1e100, -2e-3]


def b_num_str(x):
    from scheme_canon import b_num
    return strs([b_num(x), chez.number_to_string(x)])


@case("number->string-literals")
def _():
    return chez.map_(b_num_str, FLONUMS)


@case("number->string-random")
def _():
    scales = [1e-300, 1e-20, 1e-6, 1e-4, 1e-3, 1.0, 10.0, 1e5, 1e9, 1e10, 1e11, 1e17, 1e21, 1e300]
    return seeded(21, lambda: chez.map_(lambda s: repeat(25, lambda: b_num_str(chez.random(s))), scales))


@case("number->string-exact")
def _():
    return strs(chez.map_(chez.number_to_string,
                          [0, -7, 123456789012345678901234567890, F(1, 3), F(-22, 7), chez.make_rectangular(1, 2),
                           chez.make_rectangular(1.5, -2.0), chez.make_rectangular(0, 1)]))


@case("number->string-special")
def _():
    return strs(chez.map_(chez.number_to_string, [math.inf, -math.inf]))


@case("format-a")
def _():
    data = [1, 1.5, 1e-7, 1e21, String("str"), String('q"uo\\te'), Char("c"), "sym", "LetterCtgy",
            [], True, False, [1, 2.5, String("s"), Char("c"), "x"], ["quote", "x"],
            ["quasiquote", ["unquote", "y"]], ["unquote-splicing", "z"], ["quote", "x", "y"],
            Pair(1, 2), [1, Pair(2, 3)], Vector([1, String("a"), Char("b")]), Vector([]), "a b", "1+",
            "", [[1, 2], Vector([[3]])], F(2, 3), chez.make_rectangular(1, 2)]
    return strs(chez.map_(lambda x: chez.format_("~a", x), data))


@case("format-s")
def _():
    data = [1, 1.5, 1e-7, String("str"), String('q"uo\\te'), String("new\nline"), String("tab\there"),
            Char("c"), Char(" "), Char("\n"), Char("\t"), Char("\0"), "sym", "LetterCtgy", [], True,
            False, [1, 2.5, String("s"), Char("c"), "x"], ["quote", "x"],
            ["quasiquote", ["unquote", "y"]], Pair(1, 2), Vector([1, String("a"), Char("b")]),
            "a b", "1+", "+", "-", "...", "->x", "1x", "#foo", "a|b", "lmost=>rmost", "[>]:*", "Upper",
            "x;y", "q'r", "+5", "1/2", "3.5", "a(b"]
    return strs(chez.map_(lambda x: chez.format_("~s", x), data))


@case("format-directives")
def _():
    return strs([chez.format_("a~%b~nc~~d"), chez.format_("~a and ~s", String("x"), String("x")),
                 chez.format_(""), chez.format_("no directives"),
                 chez.format_("~a~a~a", 1, "two", String("three")), chez.format_("~A ~S", String("x"), String("y"))])


@case("format-numbers-in-lists")
def _():
    return seeded(22, lambda: String(chez.format_("~a ~s", repeat(5, lambda: chez.random(1e12)),
                                                  [1e-5, Vector([2e30])])))


@case("format-void")
def _():
    return String(chez.format_("~a", None))


@case("printf-output")
def _():
    return capture(lambda: (sugar.printf("x=~a y=~s~%", 1.5, String("s")), sugar.newline(),
                            sugar.printf("~a", "done")))


@case("print-output")
def _():
    return capture(lambda: u.print_(1, [2, [String("three")]], FAKES[0], 1e-9))


@case("say-object-output")
def _():
    return capture(lambda: (u.say_object(String("str")), u.say_object(FAKES[1]), u.say_object(2.5)))


# Objects -------------------------------------------------------------------------------------

@case("tell")
def _():
    return tell(FAKES[0], "get-value")


@case("tell-args")
def _():
    return tell(FAKES[0], "both", 1, 2, 3)


@case("tell-alias")
def _():
    return [tell(FAKES[0], "alias1"), tell(FAKES[0], "alias2")]


@case("tell-invalid")
def _():
    def go():
        try:
            tell(FAKES[0], "no-such-message", 1)
        except objects.Reset:
            return "reset"
    return capture(go)


@case("base-object")
def _():
    return [u.base_object("self", "object-type"), u.base_object("self", "other")]


@case("compose")
def _():
    return [u.compose(u.first, u.rest)([1, 2, 3]), u.compose(lambda x: chez.mul(2, x))(5),
            u.compose(u.first, u.rest, u.rest)([1, 2, 3])]


@case("delegate")
def _():
    parent = Lambda(lambda self, msg, *args: "from-parent" if msg == "inherited" else INVALID)

    class Child(SchemeObject):
        __slots__ = ()

        @message("own")
        def own(this, self):
            return "mine"

        def otherwise(this, self, msg, args):
            return u.delegate(self, msg, args, FAKES[0], parent)
    child = Child()
    return [tell(child, "own"), tell(child, "inherited"), tell(child, "get-value"), child(child, "unknown")]


@case("delegate-to-all")
def _():
    return u.delegate_to_all("self", "get-value", (), FAKES[0], FAKES[1])


@case("type-testers")
def _():
    types_ = ["bond", "letter", "group", "bridge", "concept-mapping", "rule", "description",
              "workspace-string", "workspace", "answer-description", "snag-description",
              "slipnode", "answer-event", "generic-event", "thing"]
    objs = chez.map_(lambda t: make_fake(t, 2), types_)
    return chez.map_(lambda o: [u.bond_p(o), u.letter_p(o), u.group_p(o), u.bridge_p(o),
                                u.concept_mapping_p(o), u.rule_p(o), u.description_p(o),
                                u.workspace_string_p(o), u.workspace_p(o), u.answer_description_p(o),
                                u.snag_description_p(o), u.exists_p(u.event_p(o)), u.slipnode_p(o),
                                u.vertical_bridge_p(o), u.horizontal_bridge_p(o)], objs)


@case("event?-value")
def _():
    return u.event_p(make_fake("group-event", 1))


@case("slipnode?-nonprocedure")
def _():
    return u.slipnode_p(5)


@case("bridges-orientation")
def _():
    return [u.vertical_bridge_p(make_fake("bridge", 2)), u.horizontal_bridge_p(make_fake("bridge", 3))]


@case("cd")
def _():
    return u.cd(FAKES[2])


@case("reveal")
def _():
    return [u.reveal(5), u.reveal(make_fake("letter", 3)),
            u.reveal([1, make_fake("group", 4), [make_fake("rule", 1)]])]


# Plain utilities ----------------------------------------------------------------------------------

@case("exists")
def _():
    return [u.exists_p(False), u.exists_p(0), u.exists_p([]), u.all_exist_p([1, 2]),
            u.all_exist_p([1, False]), u.all_exist_p([])]


B_X15 = chez.inexact(F(3, 2))   # (define b:x1.5 (exact->inexact 3/2))


@case("all-same")
def _():
    return [u.all_same_p([]), u.all_same_p(["a", "a"]), u.all_same_p(["a", "b"]),
            u.all_same_p([B_X15, B_X15]), u.all_same_p([B_X15, chez.inexact(F(3, 2))])]


@case("compress")
def _():
    return u.compress([1, False, 2, False, False, 3])


@case("map-compress")
def _():
    return u.map_compress(lambda x: x * x if x % 2 == 1 else False, iota(7))


@case("flatmap")
def _():
    return u.flatmap(iota, iota(4))


@case("rounding")
def _():
    xs = [2.5, -2.5, 3.5, 0.5, -0.5, F(7, 2), F(5, 2), F(-7, 2), 3.7, -3.7, 1e20, 4, 0.0, -0.0,
          F(1, 3), F(2, 3), 1e300]
    return chez.map_(lambda x: [u.truncate(x), u.ceiling(x), u.floor(x), u.round_(x)], xs)


@case("round-to")
def _():
    xs = [0.12345, F(2, 3), 12.3456, -0.0049, 0.005, 0.015, 0.025, 1e-20, 7, 1.05]
    return chez.map_(lambda x: [u.round_to_10ths(x), u.round_to_100ths(x), u.round_to_1000ths(x)], xs)


@case("powers")
def _():
    return [u.square(3), u.square(1.5), u.square(F(1, 3)), u.cube(2), u.cube(-1.5), u.cube(F(2, 3))]


@case("index-lists")
def _():
    return [u.ascending_index_list(1), u.ascending_index_list(5), u.descending_index_list(0),
            u.descending_index_list(1), u.descending_index_list(5)]


@case("symbol->letter-categories")
def _():
    chez.define_top_level_value("plato-x", "node-x")
    chez.define_top_level_value("plato-y", "node-y")
    return u.symbol_to_letter_categories("xyxx")


@case("char-index")
def _():
    return [u.char_index("b", "abcb"), u.char_index("z", "abc"), u.char_index("a", "")]


@case("strings")
def _():
    return strs([u.string_upcase("abC1"), u.string_downcase("ABc1"), u.capitalize_string("hello world"),
                 u.capitalize_string(""), u.capitalize_string("x"), u.string_suffix("abcdef", 2),
                 u.string_suffix("abc", 3)])


@case("quoted-string")
def _():
    return String(u.quoted_string(FAKES[1]))


@case("tables")
def _():
    t = u.make_table(2, 3)
    v = u.make_table(3, 2, "z")
    u.table_set_bang(t, 0, 1, "a")
    u.table_set_bang(t, 1, 2, "b")
    u.table_set_bang(v, 2, 1, "q")
    return [t, v, u.row_dimension(t), u.column_dimension(t), u.table_to_list(t), u.table_ref(t, 1, 2),
            u.get_row(t, 0), u.get_column(t, 1), u.get_column(v, 1)]


@case("table-ops")
def _():
    t = u.make_table(2, 2, 0)
    s = u.make_table(2, 2, 7)
    v = Vector([1, 2, 3])
    w = chez.make_vector(3, False)
    u.vector_increment_bang(v, 1, 10)
    u.initialize_row_bang(t, 0, 5)
    u.initialize_column_bang(t, 1, 9)
    u.copy_vector_contents_bang(v, w)
    u.copy_table_contents_bang(s, t)
    u.add_scaled_vector_bang(v, Vector([1, 1, 1]), F(1, 2))
    return [t, s, v, w]


@case("make-vector-default")
def _():
    return chez.make_vector(3)


@case("rotate")
def _():
    return u.rotate_90_degrees_clockwise(Vector([Vector([1, 2, 3]), Vector([4, 5, 6])]))


@case("sums")
def _():
    return [u.sum_([]), u.sum_([1, 2.5, F(1, 2)]), u.product([]), u.product([2, 3, F(1, 4)])]


@case("averages")
def _():
    return [u.average([1, 2, 3, 4]), u.average(1, 2), u.average([]), u.average(5), u.average([1.0, 2]),
            u.average(1, 2, 3), u.weighted_average([1, 2, 3], [0, 0, 0]),
            u.weighted_average([10, 20], [1, 3]), u.weighted_average([1.5, 2], [F(1, 2), F(1, 2)])]


@case("log10")
def _():
    return chez.map_(u.log10, [1, 10, 100, 1000, 0.001, 2, 50, 1e-10, 0.5])


@case("sgn")
def _():
    return [u.sgn(-3), u.sgn(0), u.sgn(2.5), u.sgn(-0.0)]


@case("list-index")
def _():
    return [u.list_index(["a", "b", "c"], "c"), u.list_index(["a", "b", "c"], "a")]


@case("list-index-missing")
def _():
    return u.list_index(["a", "b"], "z")


@case("arith-shortcuts")
def _():
    return [u.hundred_minus(30), u.ten_minus(2.5), u.one_minus(F(1, 4)), u.times_100(0.123),
            u.times_100(F(1, 3)), u.times_100(0.125), u.percent(50), u.percent(2.5),
            u.percent_20(10), u.percent_40(1.5), u.percent_80(5)]


@case("sigmoid")
def _():
    return chez.map_(lambda bm: chez.map_(u.sigmoid(bm[0], bm[1]), [0, 10, 25, 50, 75, 100, 33.3]),
                     [[5, 50], [2, 30], [10, 80], [F(1, 2), 50]])


@case("clip")
def _():
    return chez.map_(u.clip_function(0, 100), [-5, 0, 50, 100, 150, 99.5, F(1, 2)])


@case("flatten")
def _():
    return [u.flatten([]), u.flatten([1, [2, [3, []], 4], [[5]]]), u.flatten([Pair(1, 2)])]


@case("select-longest-list")
def _():
    return [u.select_longest_list([]), u.select_longest_list([[1], [1, 2], [3, 4], []])]


@case("select-extreme")
def _():
    ident = lambda x: x  # noqa: E731
    return [u.select_extreme(chez.min_, chez.abs_, [-3, 2, -1, 1]),
            u.select_extreme(chez.max_, len, [[1], [2, 3]]),
            u.select_extreme(chez.max_, ident, []), u.select_extreme(chez.max_, ident, [1, 2.0, 2])]


@case("max-min")
def _():
    return [u.maximum([]), u.maximum([1, 3.5, 2]), u.minimum([]), u.minimum([3, F(1, 2), 2]),
            u.maximum([1, 2]), u.minimum([2, 1.0])]


def odd_p(x):
    return x % 2 == 1


def even_p(x):
    return x % 2 == 0


def list_(*xs):
    return list(xs)


@case("count")
def _():
    return u.count(odd_p, iota(7))


@case("adjacency-map")
def _():
    return u.adjacency_map(list_, iota(4))


@case("select")
def _():
    return [u.select(even_p, iota(5)), u.select(even_p, [1, 3]), u.select(even_p, [])]


@case("filters")
def _():
    return [u.filter_(odd_p, iota(7)), u.filter_out(odd_p, iota(7)), u.filter_(odd_p, [])]


@case("map-leaves")
def _():
    return u.map_leaves(lambda x: chez.mul(x, 10), [1, [2, [3]], [], 4])


@case("filter-map")
def _():
    return u.filter_map(odd_p, lambda x: x * x, iota(6))


@case("map-filter")
def _():
    return u.map_filter(lambda x: x * x, even_p, iota(6))


@case("cross-products")
def _():
    return [u.cross_product([1, 2], ["a", "b", "c"]), u.cross_product([], [1]),
            u.cross_product_filter(lambda x, y: x < y, iota(3), iota(3)),
            u.cross_product_map(chez.add, [1, 2], [10, 20]),
            u.cross_product_ormap(lambda x, y: x == y, [1, 2], [3, 2]),
            u.cross_product_andmap(lambda x, y: x < y, [1, 2], [3, 4]),
            u.cross_product_for_each(lambda x, y: x, [1], [2])]


@case("method-procedures")
def _():
    return [tell(u.select_meth(FAKES, "both", 1), "get-value"),
            len(u.filter_meth(FAKES, "get-value")), u.filter_out_meth(FAKES, "get-value"),
            u.andmap_meth(FAKES, "get-value"), u.ormap_meth(FAKES, "get-orientation"),
            u.count_meth(FAKES, "get-value")]


@case("pairwise")
def _():
    return [u.pairwise_map(list_, iota(4)), u.pairwise_map(list_, []), u.pairwise_do(list_, iota(3)),
            u.pairwise_andmap(lambda x, y: x < y, iota(4)), u.pairwise_andmap(lambda x, y: x > y, iota(4)),
            u.pairwise_andmap(lambda x, y: x > y, [])]


def num_eq(x, y):
    return x == y   # Scheme = on numbers


@case("intersections")
def _():
    return [u.intersect(["a", "b", "c"], ["c", "a", "d"]), u.intersect_pred(num_eq, [1, 2, 3], [3.0, 1]),
            u.intersect_all([["a", "b", "c"], ["b", "c"], ["c", "b", "x"]]),
            u.intersect_all([]), u.intersect_all([[1, 2]]),
            u.intersect_all_pred(chez.equal_p, [[String("a"), String("b")], [String("b")]])]


@case("partition")
def _():
    return [u.partition(lambda x, y: odd_p(x) == odd_p(y), iota(7)), u.partition(num_eq, [])]


@case("members")
def _():
    return [u.member_p("b", ["a", "b"]), u.member_p("z", ["a"]), u.member_pred_p(num_eq, 2, [1, 2.0]),
            u.member_equal_p([1], [[1]]), u.member_p(String("a"), [String("a")])]


@case("sets")
def _():
    return [u.subset_p(["a"], ["a", "b"]), u.subset_p(["c"], ["a"]), u.subset_pred_p(num_eq, [1], [1.0]),
            u.sets_equal_p(["a", "b"], ["b", "a"]), u.sets_equal_p(["a"], ["a", "b"]),
            u.sets_equal_pred_p(num_eq, [1, 2], [2.0, 1.0]), u.sets_disjoint_p(["a"], ["b"]),
            u.sets_disjoint_p(["a"], ["a"]), u.sets_intersect_p(["a", "b"], ["b"]),
            u.sets_intersect_p([], ["b"])]


@case("removals")
def _():
    return [u.remove_elements_pred(num_eq, [1, 2], [1, 2, 3, 1.0, 4]), u.remq_elements(["a"], ["a", "b", "a"]),
            u.remove_elements([String("x")], [String("x"), String("y"), String("x")]),
            u.remove_duplicates_pred(num_eq)([1, 1.0, 2]),
            u.remq_duplicates(["a", "b", "a", "c", "b"]),
            u.remove_duplicates([String("a"), String("b"), String("a")])]


@case("list-access")
def _():
    lst = iota(9)
    return [u.first(lst), u.second(lst), u.third(lst), u.fourth(lst), u.fifth(lst), u.sixth(lst),
            u.seventh(lst), u.eighth(lst), u.rest(lst), u.get_first(3, lst), u.get_first(0, lst),
            u.sublist(lst, 2, 5), u.nth(4, lst), u.snoc(10, [1]), u.last(lst), u.all_but_last(2, lst),
            u.all_but_last(0, [1])]


@case("coords")
def _():
    c = u.coord(3, 4)
    return [c, u.x_coord(c), u.y_coord(c), u.coord(1.5, 0), u.x_coord(7)]


# Chez built-ins -------------------------------------------------------------------------------

@case("remq")
def _():
    return [chez.remq("a", ["a", "b", "a", "c"]), chez.remq("z", ["a"]), chez.remq("a", [])]


@case("remv")
def _():
    return [chez.remv(1, [1, 2, 1, 3]), chez.remv(1.5, [1.5, 2])]


@case("remove")
def _():
    return [chez.remove(String("a"), [String("a"), String("b"), String("a")]), chez.remove([1], [[1], 2])]


@case("1+")
def _():
    return [chez.add1(5), chez.add1(1.5), chez.sub1(5), chez.add1(F(1, 2)), chez.sub1(0)]


@case("assoc-family")
def _():
    return [chez.assq("b", [["a", 1], ["b", 2]]), chez.assv(2.0, [[2, "x"], [2.0, "y"]]),
            chez.assoc(String("b"), [[String("a"), 1], [String("b"), 2]]), chez.assq("z", [])]


@case("member-family")
def _():
    return [chez.memq("c", ["a", "b", "c", "d"]), chez.member(String("b"), [String("a"), String("b")]),
            chez.memv(1.0, [1, 1.0])]


def record_case_example(msg):
    """The battery's record-case, as the translation writes record-case on data:
    if/elif on the key, the formals bound from the rest (plan, "record-case")."""
    key, args = msg[0], msg[1:]
    if key == "one":
        return "one"
    elif key in ("two", "deux"):
        x = args[0]
        return ["two", x]
    elif key == "rest-args":
        a, r = args[0], args[1:]
        return [a, r]
    elif key == "all-args":
        return args
    else:
        return "other"


@case("record-case")
def _():
    return chez.map_(record_case_example, [["one"], ["two", 2], ["deux", 3], ["rest-args", 1, 2, 3],
                                           ["all-args", 4, 5], ["zz"]])


@case("record-case-no-else")
def _():
    msg = ["zz"]
    if msg[0] == "one":
        return "one"
    return None   # no else clause: void


@case("case-single-datum")
def _():
    def f(x):
        if x == "a":
            return "one"
        elif x in ("b", "c"):
            return "two"
        elif x == 7:
            return "seven"
        return "other"
    return chez.map_(f, ["a", "b", "c", 7, "d"])


@case("arithmetic")
def _():
    return [chez.exp(0), chez.exp(1), chez.sqrt(16), chez.sqrt(2), chez.sqrt(F(1, 4)), chez.expt(2, 10),
            chez.expt(2, 0.5), chez.expt(2.0, 3), chez.expt(1000, F(5, 3)), chez.expt(8, F(1, 3)),
            chez.log(10), chez.log(1), chez.atan(1, 1), chez.inexact(F(1, 3)), chez.exact(0.1),
            chez.div(1, 3), chez.div(6, 3), chez.mul(F(1, 25), 3, chez.sub(50, 20)), chez.max_(1, 2.0),
            chez.min_(1, 2.0), chez.string_to_number("1e3"), chez.string_to_number("1/2"),
            chez.string_to_number("abc"), chez.inexact(12345678901234567890), chez.expt(0, 0),
            chez.expt(0.0, 0), chez.div(7, 2.0), chez.quotient(7, 2), chez.remainder(-7, 2),
            chez.modulo(-7, 2), chez.abs_(F(-1, 2))]


# The syntactic-sugar.ss macros ------------------------------------------------------------------

@case("for*-each")
def _():
    return with_log(lambda: sugar.for_star(lambda x: seq(log(x), log(10 * x)), iota(3)))


@case("for*-each-multi")
def _():
    return with_log(lambda: sugar.for_star(lambda x, y: log([x, y]), iota(3), ["a", "b", "c"]))


@case("for*-from-to")
def _():
    return with_log(lambda: sugar.for_star_from_to(2, 5, lambda i: log(i)))


@case("for*-empty")
def _():
    return with_log(lambda: sugar.for_star_from_to(5, 4, lambda i: log(i)))


@case("for*-single")
def _():
    return with_log(lambda: sugar.for_star_from_to(3, 3, lambda i: log(i)))


@case("for*-bounds-order")
def _():
    def go():
        lo = log(1)    # chez: exp1 before exp2 (syntactic-sugar.ss:82)
        hi = log(3)
        return sugar.for_star_from_to(lo, hi, lambda i: log(["i", i]))
    return with_log(go)


@case("for*-value")
def _():
    return sugar.for_star(lambda x: x, [1])


@case("for-each-vector-element*")
def _():
    v = Vector(["a", "b", "c"])
    return with_log(lambda: sugar.for_each_vector_element_star(v, lambda i: log([i, v[i]])))


@case("for-each-table-element*")
def _():
    t = Vector([Vector([1, 2]), Vector([3, 4]), Vector([5, 6])])
    return with_log(lambda: sugar.for_each_table_element_star(t, lambda i, j: log([i, j, t[i][j]])))


@case("repeat*-times")
def _():
    return with_log(lambda: sugar.repeat_star_times(3, lambda: seq(log("x"), log("y"))))


@case("repeat*-zero")
def _():
    return with_log(lambda: sugar.repeat_star_times(0, lambda: log("x")))


@case("repeat*-until")
def _():
    def go():
        n = [0]

        def body():
            log(n[0])
            n[0] = n[0] + 1
        return sugar.repeat_star_until(lambda: n[0] >= 3, body)
    return with_log(go)


@case("repeat*-forever")
def _():
    def go(k):
        n = [0]

        def body():
            log(n[0])
            n[0] = n[0] + 1
            if n[0] == 4:
                k("out")
        return sugar.repeat_star_forever(body)
    return with_log(lambda: sugar.continuation_point_star(go))


@case("if*")
def _():
    return [sugar.if_star(True, lambda: seq(1, 2)), sugar.if_star(False, lambda: seq(1, 2))]


@case("stochastic-if*")
def _():
    return chez.map_(lambda seed: seeded(seed, lambda: repeat(6, lambda: with_log(
        lambda: sugar.stochastic_if_star(lambda: log(0.5), lambda: seq(log("yes"), "done"))))), SEEDS)


@case("stochastic-if*-certain")
def _():
    def go():
        a = sugar.stochastic_if_star(lambda: 1, lambda: "a")
        b = sugar.stochastic_if_star(lambda: 0, lambda: "b")
        c = sugar.stochastic_if_star(lambda: 1.5, lambda: "c")
        return [a, b, c]
    return seeded(9, go)


@case("continuation-point*")
def _():
    def first(k):
        _ = 1
        k(2)
        return 3
    return [sugar.continuation_point_star(first), sugar.continuation_point_star(lambda k: seq(1, 2))]


@case("say-quiet")
def _():
    with engine_module("setup", p_verbose=False):
        return capture(lambda: (sugar.say(String("a"), 1, FAKES[1]), sugar.vprintf("x~a", 1), sugar.vprint(1, 2)))


@case("say!-quiet")
def _():
    with engine_module("setup", p_verbose=False):
        return capture(lambda: sugar.say_bang(String("a"), 1.5, FAKES[1]))


@case("say-verbose")
def _():
    with engine_module("setup", p_verbose=False) as setup:
        def go():
            setup.p_verbose = True
            sugar.say(String("a"), 1, FAKES[1])
            sugar.vprintf("x~a~%", 1)
            sugar.vprint(1, [2.5, "z"])
            setup.p_verbose = False
        return capture(go)


CONTROL_PANEL = Lambda(lambda self, msg, *args: ["ran", args[0]] if msg == "run-new-problem" else None)


def mcat(*tokens):
    with engine_module("setup", g_control_panel=CONTROL_PANEL):
        return sugar.mcat(*tokens)


for _name, _tokens in [("mcat-3", ("abc", "abd", "xyz")), ("mcat-4", ("abc", "abd", "xyz", 7)),
                       ("mcat-4-sym", ("abc", "abd", "xyz", "wyz")),
                       ("mcat-5", ("abc", "abd", "xyz", "wyz", 99)), ("mcat-bad-2", ("abc", "abd")),
                       ("mcat-bad-num", ("abc", "abd", "xyz", 0)), ("mcat-bad-5", ("abc", "abd", "xyz", 7, 7))]:
    case(_name)(lambda _tokens=_tokens: mcat(*_tokens))


@case("valid-token-list")
def _():
    return [sugar.valid_token_list_p(["a", "b", "c"]), sugar.valid_token_list_p(["a", "b", "c", 4294967295]),
            sugar.valid_token_list_p(["a", "b", "c", 4294967296]), sugar.valid_token_list_p(["a", "b", 1]),
            sugar.valid_token_list_p("a"), sugar.valid_token_list_p(["a", "b", "c", "d", 2.5]),
            sugar.symbol_or_valid_number_p("q"), sugar.valid_number_p(0), sugar.g_largest_random_seed]


@case("concatenate-symbols")
def _():
    return [sugar.concatenate_symbols("a", "-", "b"), sugar.concatenate_symbols()]


# slipnet macros, with stand-ins for the slipnet procedures they call

def make_slipnode(name, short, depth):
    log(["make-slipnode", name, short, depth])
    return ["node", name]


@case("slipnet-node-list*")
def _():
    def go():
        nodes = sugar.slipnet_node_list_star([("plato-p", String("p"), 10), ("plato-q", String("q"), 20)])
        return [nodes, chez.top_level_value("plato-p"), chez.top_level_value("plato-q")]
    with engine_module("slipnet", make_slipnode=make_slipnode):
        return with_log(go)


@case("slipnet-layout-table*")
def _():
    return sugar.slipnet_layout_table_star([[1, 2, 3], [4, 5, 6]])


def establish_link(name, from_, to, type_):
    log(["establish-link", name, from_, to, type_])
    chez.define_top_level_value(name, Lambda(lambda self, msg, *args: seq(log([name, msg, *args]), "done")))


def link_case(name, thunk):
    def run():
        chez.define_top_level_value("plato-z", "node-z")
        chez.define_top_level_value("plato-succ", "node-succ")
        with engine_module("slipnet", establish_link=establish_link):
            return with_log(thunk)
    case(name)(run)


link_case("category-link*", lambda: sugar.category_link_star("x", "y", 50))
link_case("category-link*-all", lambda: sugar.category_links_star(["x", "y"], "z", 60))
link_case("instance-link*", lambda: sugar.instance_link_star("x", "y", 100))
link_case("instance-link*-all", lambda: sugar.instance_links_star("z", ["x", "y"], 70))
link_case("property-link*", lambda: sugar.property_link_star("x", "z", 75))
link_case("lateral-link*-length", lambda: sugar.lateral_link_star("x", "y", length=10))
link_case("lateral-link*-label", lambda: sugar.lateral_link_star("x", "y", label="succ"))
link_case("lateral-link*-both", lambda: sugar.lateral_link_star("x", "y", length=10, label="succ"))
link_case("lateral-link*-two-way", lambda: sugar.lateral_link_star("x", "z", label="succ", two_way=True))
link_case("lateral-sliplink*-label", lambda: sugar.lateral_sliplink_star("x", "y", label="succ"))
link_case("lateral-sliplink*-length", lambda: sugar.lateral_sliplink_star("y", "z", length=20))
link_case("lateral-sliplink*-two-way", lambda: sugar.lateral_sliplink_star("x", "y", length=30, two_way=True))


@case("link-top-level-value")
def _():
    return objects.procedure_p(chez.top_level_value("x-y-link"))


# coderack macros, with stand-ins for the coderack

class CodeletTypeStub(SchemeObject):
    """utilities-battery.scm: make-codelet-type"""
    __slots__ = ("name", "proc")

    def __init__(this, name):
        this.name = name
        this.proc = False

    @message("make-codelet")
    def make_codelet(this, self, urgency, *args):
        return ["codelet", this.name, urgency, list(args)]

    @message("set-codelet-procedure")
    def set_codelet_procedure(this, self, p):
        this.proc = p
        return "done"

    @message("run")
    def run(this, self, *args):
        return this.proc(*args)

    def otherwise(this, self, msg, args):
        return INVALID


def make_codelet_type(name, labels):
    log(["make-codelet-type", name, labels])
    return CodeletTypeStub(name)


def post_logger(self, msg, *args):
    if msg == "post":
        log(["post", args[0]])
        return "posted"
    return None


@case("codelet-type-list*")
def _():
    def go():
        types_ = sugar.codelet_type_list_star([("test-scout", [String("Test"), String("scout")]),
                                               ("test-builder", [String("Test builder")])])
        return [len(types_), types_[0] is chez.top_level_value("test-scout")]
    with engine_module("coderack", make_codelet_type=make_codelet_type):
        return with_log(go)


def scout():
    """(define test-scout (top-level-value 'test-scout)), made by codelet-type-list*
    (a stand-in is made if that case has not run, so that cases run alone too)."""
    if not chez.top_level_bound_p("test-scout"):
        chez.define_top_level_value("test-scout", CodeletTypeStub("test-scout"))
    return chez.top_level_value("test-scout")


def with_coderack(thunk):
    with engine_module("coderack", g_coderack=Lambda(post_logger)):
        return thunk()


case("post-codelet*")(lambda: with_coderack(lambda: with_log(
    lambda: sugar.post_codelet_star(35, scout(), "arg1", [2, 3]))))
case("post-codelet*-no-args")(lambda: with_coderack(lambda: with_log(
    lambda: sugar.post_codelet_star(F(1, 2), scout()))))


def define_scout():
    def test_scout_proc(x):
        log(["start", x])
        if x == "quit":
            sugar.fizzle()
        log(["end", x])
        return ["result", x]
    sugar.define_codelet_procedure_star("test-scout", test_scout_proc)


@case("define-codelet-procedure*")
def _():
    with engine_module("setup", p_verbose=False):
        define_scout()
        a = with_log(lambda: tell(scout(), "run", "go"))
        a_fizzle = objects.procedure_p(sugar.fizzle)
        b = with_log(lambda: tell(scout(), "run", "quit"))
        b_fizzle = sugar.fizzle
        return [a, a_fizzle, b, b_fizzle]


@case("define-codelet-procedure*-verbose")
def _():
    with engine_module("setup", p_verbose=False) as setup:
        def go():
            if scout().proc is False:
                define_test_scout()
            setup.p_verbose = True
            tell(scout(), "run", "go")
            setup.p_verbose = False
        return capture(go)


# The tests ------------------------------------------------------------------------------------

def test_every_battery_test_is_translated():
    assert set(CASES) == set(manifest("utilities"))
    assert len(CASES) == 197


def run_case(name):
    try:
        return canon(CASES[name]())
    except chez.SchemeError:
        return "ERROR"


# The battery's forms run in order and share state (top-level values, the codelet
# type defined by codelet-type-list*), so the cases run in the battery's order.
@pytest.mark.parametrize("name", manifest("utilities"))
def test_utilities_battery(name):
    expected = fixture("utilities", name)
    assert run_case(name) == expected


def test_cases_are_independent_of_order_for_the_stateless_ones():
    """Running a pure case twice gives the same text (no hidden state in the port)."""
    for name in ["rounding", "pairwise-map-order", "cross-products", "averages", "tables", "log10"]:
        assert run_case(name) == run_case(name) == fixture("utilities", name)


# Structure ----------------------------------------------------------------------------------

from name_mapping import ORIGINAL  # noqa: E402
from metacat.names import scheme_to_python  # noqa: E402
import re  # noqa: E402


def defines(path):
    text = re.sub(r";[^\n]*", "", path.read_text())
    return re.findall(r"^\(define\s+\(?([^\s()]+)", text, re.M)


# Macros with several patterns become one Python function per pattern.
MACRO_FUNCTIONS = {
    "for*": ["for_star", "for_star_from_to"],
    "repeat*": ["repeat_star_times", "repeat_star_forever", "repeat_star_until"],
    "category-link*": ["category_link_star", "category_links_star"],
    "instance-link*": ["instance_link_star", "instance_links_star"],
}


def test_every_utilities_definition_has_its_python_function():
    missing = [n for n in defines(ORIGINAL / "utilities.ss") if not hasattr(u, scheme_to_python(n))]
    assert missing == []
    assert len(defines(ORIGINAL / "utilities.ss")) > 150


def test_every_sugar_definition_and_macro_has_its_python_function():
    path = ORIGINAL / "syntactic-sugar.ss"
    text = re.sub(r";[^\n]*", "", path.read_text())
    macros = re.findall(r"\(extend-syntax\s+\(([^\s()]+)", text)
    assert len(macros) == 22
    names = [scheme_to_python(n) for n in defines(path)]
    for m in macros:
        names += MACRO_FUNCTIONS.get(m, [scheme_to_python(m)])
    assert [n for n in names if not hasattr(sugar, n)] == []


def test_docstrings_name_their_origin():
    for mod, origin in ((u, "utilities.ss"), (sugar, "syntactic-sugar.ss")):
        for name, fn in vars(mod).items():
            if callable(fn) and getattr(fn, "__module__", None) == mod.__name__ and not name.startswith("_") \
                    and not isinstance(fn, type):
                assert fn.__doc__ and fn.__doc__.split(":")[0] in (origin, "Chez"), (mod.__name__, name)


def test_engine_modules_import_no_gui():
    import ast
    import inspect
    for mod in (objects, sugar, u, importlib.import_module("metacat.names")):
        tree = ast.parse(inspect.getsource(mod))
        names = {a.name for n in ast.walk(tree) if isinstance(n, (ast.Import, ast.ImportFrom))
                 for a in n.names} | {n.module for n in ast.walk(tree) if isinstance(n, ast.ImportFrom)}
        assert not any(str(n).startswith(("tkinter", "metacat.gui")) for n in names), mod


# objects.py beyond the battery -------------------------------------------------------------

def test_self_is_the_receiver_through_delegation():
    class Parent(SchemeObject):
        __slots__ = ()

        @message("who")
        def who(this, self):
            return tell(self, "name")

        def otherwise(this, self, msg, args):
            return INVALID

    class Void(SchemeObject):     # a record-case without else answers void
        __slots__ = ()

    class Child(SchemeObject):
        __slots__ = ("parent",)

        def __init__(this):
            this.parent = Parent()

        @message("name")
        def name(this, self):
            return "child"

        def otherwise(this, self, msg, args):
            return u.delegate(self, msg, args, this.parent, u.base_object)
    c = Child()
    assert tell(c, "who") == "child"
    assert tell(c, "object-type") == "base-object"
    assert u.delegate(c, "object-type", (), Void(), u.base_object) is None


def test_forwarder_passes_the_original_as_self():
    f = objects.Forwarder(FAKES[0])
    assert tell(f, "get-value") == 1
    assert objects.procedure_p(f)


def test_report_error_and_halt_can_be_replaced():
    seen = []
    saved = objects.report_error_and_halt
    objects.report_error_and_halt = lambda message, obj: seen.append(list(message[1:])) or "replaced"
    try:
        assert tell(FAKES[0], "nope", 1) == "replaced"
    finally:
        objects.report_error_and_halt = saved
    assert seen == [["nope", 1]]


def test_continuation_point_escape_is_only_caught_by_its_own_form():
    def outer(k_out):
        def inner(k_in):
            k_out("from-inner")
            return "not reached"
        sugar.continuation_point_star(inner)
        return "not reached either"
    assert sugar.continuation_point_star(outer) == "from-inner"
    saved = []
    sugar.continuation_point_star(lambda k: saved.append(k))
    with pytest.raises(chez.SchemeError):
        saved[0]("too late")


def test_stochastic_if_draws_before_the_probability():
    chez.random_seed(42)
    first = chez.random(1.0)
    chez.random_seed(42)
    seen = []
    sugar.stochastic_if_star(lambda: seen.append(chez.random_seed()) or 0.5, lambda: None)
    chez.random_seed(42)
    chez.random(1.0)
    assert seen == [chez.random_seed()] and first >= 0.0


# python/oracle/batteries/utilities-extra-battery.scm --------------------------------------

EXTRA: dict = {}


def next_draw(seed):
    """utilities-extra-battery.scm: u:next-draw"""
    chez.random_seed(seed)
    return chez.random(1.0)


def tie(seed, at_p, above_p):
    r = next_draw(seed)
    chez.random_seed(seed)
    at = at_p(r)
    chez.random_seed(seed)
    above = above_p(chez.add(r, 1e-15))
    return [at, above]


EXTRA["prob?-ties"] = lambda: chez.map_(lambda s: tie(s, u.prob_p, u.prob_p), SEEDS)
EXTRA["stochastic-if*-ties"] = lambda: chez.map_(
    lambda s: tie(s, lambda p: sugar.stochastic_if_star(lambda: p, lambda: "taken"),
                  lambda p: sugar.stochastic_if_star(lambda: p, lambda: "taken")), SEEDS)
EXTRA["select-extreme-ties"] = lambda: [
    u.select_extreme(chez.max_, chez.abs_, [-3, 3, 1]), u.select_extreme(chez.min_, chez.abs_, [2, -1, 1]),
    u.select_extreme(chez.max_, lambda x: x, [2, 2.0]), u.select_extreme(chez.max_, lambda x: x, [2.0, 2]),
    u.select_extreme(chez.min_, lambda x: chez.mul(0.5, x), [4, 2, 2.0])]
EXTRA["misc"] = lambda: [
    u.stochastic_pick([], []), u.weighted_average([1, 2], [0.5, 0.5]), u.percent(1.5), u.sum_([F(1, 2), 0.5]),
    u.average([F(1, 3), F(2, 3)]), u.round_to_10ths(0.25), u.round_to_10ths(0.35), u.times_100(0.005),
    u.times_100(F(1, 200)), u.log10(F(1, 10))]


def test_every_extra_test_is_translated():
    assert list(EXTRA) == list(manifest("utilities-extra"))


@pytest.mark.parametrize("name", manifest("utilities-extra"))
def test_utilities_extra_battery(name):
    assert canon(EXTRA[name]()) == fixture("utilities-extra", name)

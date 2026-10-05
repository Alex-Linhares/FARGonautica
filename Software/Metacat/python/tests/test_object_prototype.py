"""Item 01: the object-system prototypes against Chez, and their micro-benchmark.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

The expected values are the frozen Chez outputs of tests/diff/utilities-battery.scm's
object tests (python/fixtures/utilities/), in helpers.scm's b:canon notation.  Each
candidate rebuilds the battery's fake objects in its own style.  C3 is the one the
plan chooses; items 03 onwards build metacat/objects.py from it.  Candidate D
(inheritance) can't express delegation to a separate object, so it only enters the
benchmark.  The decision is in docs/python-translation-plan.md, "Objects".
"""
from __future__ import annotations

from fractions import Fraction

import pytest

import object_prototypes as op
from chez_fixtures import chez
from object_prototypes import INVALID, Reset, SchemeObject, message, tell


def canon(x) -> str:
    """helpers.scm's b:canon for the values these tests produce.  Python str stands
    for a Scheme symbol here (no Scheme string reaches canon in these tests)."""
    if x is None:
        return "#<void>"
    if x is True:
        return "#t"
    if x is False:
        return "#f"
    if isinstance(x, str):
        return "'" + x
    if isinstance(x, (int, Fraction)):
        return str(x)
    if isinstance(x, (list, tuple)):
        out = "()"
        for item in reversed(x):
            out = "(" + canon(item) + " . " + out + ")"
        return out
    raise TypeError(x)


def with_log(thunk):
    """helpers.scm: with-log."""
    op.LOG.clear()
    result = thunk()
    return [result, list(op.LOG)]


STYLES = ["A", "B", "C", "C3"]


def fakes(style):
    """utilities-battery.scm: (map (lambda (n) (make-fake 'thing n)) (b:iota 6))."""
    return [op.FAKE_MAKERS[style]("thing", n) for n in range(1, 7)]


def protocol(style):
    """The style's tell and delegate (C3 has its own; the others share them)."""
    if style == "C3":
        return op.tell_c3, op.delegate_c3
    return tell, op.delegate


@pytest.mark.parametrize("style", STYLES)
def test_tell(style):
    f = fakes(style)
    tell, _ = protocol(style)
    assert canon(tell(f[0], "get-value")) == chez("utilities", "tell")
    assert canon(tell(f[0], "both", 1, 2, 3)) == chez("utilities", "tell-args")
    assert canon([tell(f[0], "alias1"), tell(f[0], "alias2")]) == chez("utilities", "tell-alias")


@pytest.mark.parametrize("style", STYLES)
def test_tell_invalid_halts(style):
    out: list = []
    tell, _ = protocol(style)
    with pytest.raises(Reset):
        tell(fakes(style)[0], "no-such-message", 1, out=out)
    assert '"' + "".join(out) + '"' == chez("utilities", "tell-invalid")


def test_base_object():
    assert canon([op.base_object("self", "object-type"), op.base_object("self", "other")]) \
        == chez("utilities", "base-object")
    base = op.BASE_OBJECT
    assert canon([base("self", "object-type"), base("self", "other")]) == chez("utilities", "base-object")


def make_delegate_pair(style, first_fake):
    """utilities-battery.scm's (test delegate ...): a child delegating to a fake and
    then to a parent."""
    _, delegate = protocol(style)
    if style in ("C", "C3"):
        class Parent(SchemeObject):
            @message("inherited")
            def inherited(this, self):
                return "from-parent"

            def otherwise(this, self, msg, args):
                return INVALID

        class Child(SchemeObject):
            __slots__ = ("parent",)

            def __init__(this):
                this.parent = Parent()

            @message("own")
            def own(this, self):
                return "mine"

            def otherwise(this, self, msg, args):
                return delegate(self, msg, args, first_fake, this.parent)
        return Child()

    def parent(self, msg, *args):
        return "from-parent" if msg == "inherited" else INVALID

    def child(self, msg, *args):
        if msg == "own":
            return "mine"
        return delegate(self, msg, args, first_fake, parent)
    return child


@pytest.mark.parametrize("style", STYLES)
def test_delegate(style):
    child = make_delegate_pair(style, fakes(style)[0])
    tell, _ = protocol(style)
    got = [tell(child, "own"), tell(child, "inherited"), tell(child, "get-value"),
           child(child, "unknown")]
    assert canon(got) == chez("utilities", "delegate")


@pytest.mark.parametrize("style", STYLES)
def test_delegate_to_all(style):
    f = fakes(style)
    assert canon(op.delegate_to_all(("self", "get-value"), f[0], f[1])) \
        == chez("utilities", "delegate-to-all")
    assert canon(with_log(lambda: op.delegate_to_all(("self", "bump", 7), f[0], f[1], f[2]))) \
        == chez("utilities", "delegate-to-all-order")
    assert canon(with_log(lambda: op.delegate_to_all(("self", "nope"), f[0], f[1]))) \
        == chez("utilities", "delegate-to-all-invalid")


@pytest.mark.parametrize("style", STYLES)
def test_tell_all_order(style):
    """tell-all goes through Chez's map: 5 6 3 4 1 2 for six objects."""
    f = fakes(style)
    assert canon(with_log(lambda: op.tell_all(f, "bump", 100))) == chez("utilities", "tell-all-order")


def test_record_case_without_else_is_void():
    class OneMessage(SchemeObject):
        @message("one")
        def one(this, self):
            return "one"
    obj = OneMessage()
    assert canon(obj(obj, "zz")) == chez("utilities", "record-case-no-else")


@pytest.mark.parametrize("style", ["C", "C3"])
def test_self_is_the_receiver_through_delegation_and_forwarders(style):
    """A parent's method sees the delegating child as self (utilities.ss's delegate
    passes msg, self included, unchanged), and a trace.ss forwarder
    (lambda msg (apply original original (cdr msg))) passes the original."""
    tell, delegate = protocol(style)
    base = op.BASE_OBJECT if style == "C3" else op.base_object

    class Parent(SchemeObject):
        @message("whoami")
        def whoami(this, self):
            return tell(self, "name")

        def otherwise(this, self, msg, args):
            # without object-type, report-error-and-halt recurses forever, in the
            # original too (porting-notes.md, item 03)
            return delegate(self, msg, args, base)

    class Child(SchemeObject):
        __slots__ = ("parent",)

        def __init__(this):
            this.parent = Parent()

        @message("name")
        def name(this, self):
            return "child"

        def otherwise(this, self, msg, args):
            return delegate(self, msg, args, this.parent)
    child = Child()
    assert tell(child, "whoami") == "child"
    with pytest.raises(Reset):  # the parent alone doesn't understand name
        tell(child.parent, "whoami", out=[])
    if style == "C3":
        assert tell(op.Forwarder(child), "whoami") == "child"
    else:
        def forwarder(self, msg, *args):
            return child(child, msg, *args)
        assert tell(forwarder, "whoami") == "child"
        assert op.tell_c2(forwarder, "whoami") == "child"


def test_chez_map1_order():
    """Chez's one-list map applies f to 7 elements as 7 5 6 3 4 1 2 (compat.rkt)."""
    seen = []
    assert op.chez_map1(lambda x: seen.append(x) or x * 10, [1, 2, 3, 4, 5, 6, 7]) \
        == [10, 20, 30, 40, 50, 60, 70]
    assert seen == [7, 5, 6, 3, 4, 1, 2]
    for n in range(9):  # utilities-battery.scm's map1-order, n = 1..9 there
        seen = []
        op.chez_map1(seen.append, list(range(1, n + 1)))
        assert sorted(seen) == list(range(1, n + 1))


# ---------------------------------------------------------------------------
# The micro-benchmark

@pytest.mark.parametrize("name", list(op.CANDIDATES))
def test_benchmark_objects_answer_alike(name):
    maker, tell_fn = op.CANDIDATES[name]
    obj = maker()
    assert [tell_fn(obj, m) for m in op.CHILD] == list(range(40))
    assert [tell_fn(obj, m) for m in op.PARENT] == list(range(15))
    assert tell_fn(obj, "object-type") == "base-object"
    with pytest.raises(Reset):
        tell_fn(obj, "no-such-message")


def test_micro_benchmark(capsys):
    """Runs the benchmark (a few seconds) and checks the orderings the plan's decision
    rests on, with wide margins so that a loaded machine doesn't fail it."""
    results = op.bench(number=4000, repeat=5)
    with capsys.disabled():
        print("\n" + op.markdown(results))
    a, b = results["A closure-if"], results["B closure-dict"]
    c3 = results["C3 class-dict, lookup inlined in tell and delegate"]
    # the chosen representation (C3): a dict dispatch doesn't depend on the message's
    # place in the record-case, the if/elif chain does
    assert c3["40th of 40 messages"] < a["40th of 40 messages"]
    assert a["40th of 40 messages"] > 2 * a["first message"]
    assert c3["delegated, 8th of parent's 15"] < a["delegated, 8th of parent's 15"]
    # B builds one closure per message for every object
    assert c3["create (child + parent)"] < b["create (child + parent)"]

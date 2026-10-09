"""Tests for seqsee.smetonym_type and seqsee.smetonym.

Mirrors Perl: lib/SMetonymType.pm and lib/SMetonym.pm (golden: smetonym).

The golden cases are replayed in oracle order against a fresh create memo, because
later cases depend on keys the earlier ones registered (e.g. a create with undef
info_loss finds the earlier ``A;each`` entry instead of dying).
"""
import gc

import pytest

import golden
from seqsee import s as S
from seqsee import smetonym, smetonym_type
from seqsee.errors import Confess
from seqsee.sint import SInt
from seqsee.smetonym import SMetonym
from seqsee.smetonym_type import SMetonymType

CASES = golden.load("smetonym")


class FakeObj:
    log = []

    def __init__(self, n):
        self.n = n

    def describe_as(self, cat):
        FakeObj.log.append(f"{self.n}.describe_as({cat.n})")
        return 1

    def get_pure(self):
        return "pure:" + self.n


class FakeCat:
    log = []

    def __init__(self, n):
        self.n = n

    def get_pure(self):
        return "pure:" + self.n

    def get_meto_unfinder(self, name):
        if name != "each":
            return None

        def unfinder(cat, nm, info_loss, obj):
            FakeCat.log.append(",".join([
                cat.n, nm, "/".join(f"{k}={desc(info_loss[k])}" for k in sorted(info_loss)), obj.n]))
            return FakeObj("B" + obj.n)
        return unfinder


def desc(v):
    if v is None:
        return "undef"
    if isinstance(v, SInt):
        return f"SInt({v.get_mag()})"
    if isinstance(v, FakeObj):
        return "obj:" + v.n
    if isinstance(v, FakeCat):
        return "cat:" + v.n
    if isinstance(v, dict):
        return "{" + ",".join(f"{k}={desc(v[k])}" for k in sorted(v)) + "}"
    if isinstance(v, list):
        return "OBJ:ARRAY"
    if isinstance(v, float) and v == int(v):
        return str(int(v))
    return str(v)


class World:
    """The oracle's fixed objects and spec decoding."""

    def __init__(self):
        self.cat = {"A": FakeCat("A"), "B": FakeCat("B")}
        self.obj = {"x": FakeObj("x"), "y": FakeObj("y")}
        self.decode = {}
        self.encoded = []

    def val(self, spec):
        if spec == "u":
            return None
        tag, rest = spec[0], spec[2:]
        if tag == "s":
            return rest
        if tag == "n":
            return float(rest) if "." in rest else int(rest)
        if tag == "i":
            return SInt(int(rest))
        if tag == "o":
            return self.obj[rest]
        raise ValueError(spec)

    def category(self, spec):
        return spec[2:] if spec.startswith("s:") else self.cat[spec]

    def opts(self, c, n, il):
        if isinstance(il, list):
            info = {il[i]: self.val(il[i + 1]) for i in range(0, len(il), 2)}
        elif il == "array":
            info = []
        else:
            info = self.val(il)
        return {"category": self.category(c), "name": self.val(n), "info_loss": info}


def err(fn):
    try:
        return fn(), None
    except Confess as e:
        return None, str(e)


@pytest.fixture
def world(monkeypatch):
    w = World()
    monkeypatch.setattr(smetonym_type, "_METO", {})
    FakeCat.log.clear()
    FakeObj.log.clear()

    def encode(*objs):
        w.encoded.append([desc(o) for o in objs])
        return "ENC"
    monkeypatch.setattr(smetonym_type, "_sltm_encode", encode)
    monkeypatch.setattr(smetonym_type, "_sltm_decode", lambda st: list(w.decode[st]))
    return w


def by_kind(kind):
    return [c for c in CASES if c["kind"] == kind]


# --- golden replay --------------------------------------------------------------------

def test_golden_new(world):
    (case,) = by_kind("new")
    t = SMetonymType(world.opts("A", "s:each", ["length", "n:2"]))
    c, n = t.get_cat_and_name()
    assert desc(t.get_category()) == case["category"]
    assert t.get_name() == case["name"]
    assert desc(t.get_info_loss()) == case["info_loss"]
    assert [desc(c), n] == case["cat_and_name"]
    assert t.as_text() == case["as_text"]
    assert (t.get_pure() is t) == bool(case["get_pure_is_self"])
    again = SMetonymType(world.opts("A", "s:each", ["length", "n:2"]))
    assert (again is not t) == bool(case["new_not_memoized"])


@pytest.mark.parametrize("case", by_kind("new_check"), ids=lambda c: repr(c["spec"]))
def test_golden_new_check(world, case):
    t, e = err(lambda: SMetonymType(world.opts(*case["spec"])))
    assert int(t is None) == case["dies"]
    assert e == case["error"]
    if t is not None:
        assert desc(t.get_category()) == case["category"]
        assert desc(t.get_info_loss()) == case["info_loss"]


def test_golden_replay_create_and_metonym(world):
    """create pairs, create errors, DOUBLE, blemish, deps, deserialize and SMetonym, in order."""
    w = world
    for case in CASES:
        kind = case["kind"]
        if kind == "create_pair":
            o1 = w.opts(*case["first"])
            t1 = SMetonymType.create(o1)
            t2 = SMetonymType.create(w.opts(*case["second"]))
            assert int(t1 is t2) == case["same"], case
            assert int(t2.get_info_loss() is o1["info_loss"]) == case["info_loss_is_first_hash"], case
            assert desc(t2.get_info_loss()) == case["info_loss"], case
        elif kind == "create_error":
            t, e = err(lambda: SMetonymType.create(w.opts(*case["spec"])))
            assert int(t is None) == case["dies"], case
            if case["dies"]:
                if case["error"].startswith("Need"):
                    assert e == case["error"], case
                else:
                    assert e is not None
            t, e = err(lambda: SMetonymType.create(w.opts(*case["spec"])))
            assert int(t is None) == case["dies_again"], case
        elif kind == "double":
            d = S.DOUBLE
            c = SMetonymType.create({"category": S.SAMENESS, "name": "each", "info_loss": {"length": 2}})
            assert d.get_category().get_name() == case["category"]
            assert d.get_name() == case["name"]
            assert desc(d.get_info_loss()) == case["info_loss"]
            assert int(d.get_category() is S.SAMENESS) == case["category_is_sameness"]
            assert int(c is d) == case["create_is_double"]
        elif kind == "blemish":
            t = SMetonymType.create(w.opts(*case["spec"]))
            FakeCat.log.clear()
            FakeObj.log.clear()
            r = t.blemish(w.obj["x"])
            assert desc(r) == case["result"]
            assert FakeCat.log == case["unfinder_calls"]
            assert FakeObj.log == case["describe_calls"]
        elif kind == "blemish_no_unfinder":
            t = SMetonymType.create(w.opts("A", "s:double", ["length", "n:2"]))
            FakeObj.log.clear()
            with pytest.raises(Confess):
                t.blemish(w.obj["x"])
            assert case["dies"] == 1
            assert FakeObj.log == case["describe_calls"]
        elif kind == "deps":
            t = SMetonymType.create(w.opts(*case["spec"]))
            deps = t.get_memory_dependencies()
            w.encoded.clear()
            assert t.serialize() == case["serialized"]
            assert sorted(desc(d) for d in deps) == case["deps_sorted"], case
            assert len(deps) == case["deps_count"]
            assert w.encoded == case["encoded"]
        elif kind == "deserialize":
            t = SMetonymType.create(w.opts("A", "s:each", ["length", "n:4"]))
            w.decode["X"] = [w.cat["A"], "each", {"length": 4}]
            w.decode["Y"] = [w.cat["B"], "each", {"length": 4}]
            dx = SMetonymType.deserialize("X")
            dy = SMetonymType.deserialize("Y")
            dy2 = SMetonymType.deserialize("Y")
            assert int(dx is t) == case["x_is_existing"]
            assert int(dy is not t) == case["y_is_new"]
            assert int(dy is dy2) == case["y_memo"]
            assert desc(dy.get_category()) == case["y_category"]
            assert dy.get_name() == case["y_name"]
            assert desc(dy.get_info_loss()) == case["y_info_loss"]
        elif kind == "deserialize_error":
            w.decode["Z"] = [w.cat["A"], None, {"length": 4}]
            _, e = err(lambda: SMetonymType.deserialize("Z"))
            assert e == case["error"]
        elif kind == "metonym":
            _replay_metonym(w, case)
        elif kind == "metonym_error":
            _replay_metonym_error(w, case)


def _replay_metonym(w, case):
    starred, unstarred = FakeObj("s"), FakeObj("u")
    o = w.opts("A", "s:each", ["length", "n:6"])
    m = SMetonym({**o, "starred": starred, "unstarred": unstarred})
    t = SMetonymType.create(w.opts("A", "s:each", ["length", "n:6"]))
    m2 = SMetonym(**w.opts("A", "s:each", ["length", "n:6"]), starred=starred, unstarred=unstarred)
    m3 = SMetonym(**w.opts("A", "s:each", ["length", "n:7"]), starred=starred, unstarred=unstarred)
    assert int(m.get_type() is t) == case["type_is_create"]
    assert desc(m.get_category()) == case["category"]
    assert m.get_name() == case["name"]
    assert desc(m.get_info_loss()) == case["info_loss"]
    assert int(m.get_info_loss() is o["info_loss"]) == case["info_loss_is_opts_hash"]
    assert desc(m.get_starred()) == case["starred"]
    assert desc(m.get_unstarred()) == case["unstarred"]
    assert int(m2.get_type() is m.get_type()) == case["same_type"]
    assert int(SMetonym.intersection(m, m2) is t) == case["intersection_same"]
    assert int(SMetonym.intersection(m3) is m3.get_type()) == case["intersection_one"]
    assert int(SMetonym.intersection(m, m2, m3) is not None) == case["intersection_differ"]
    with pytest.raises(Confess):
        SMetonym.intersection()
    assert case["intersection_empty_dies"] == 1

    # unstarred is weakened, starred is not.
    (before,) = by_kind("weak_before")
    (after,) = by_kind("weak_after")
    u = FakeObj("tmp")
    weak = SMetonym({**o, "starred": starred, "unstarred": u})
    assert desc(weak.get_unstarred()) == before["unstarred"]
    strong = SMetonym({**o, "starred": FakeObj("tmp2"), "unstarred": unstarred})
    del u
    gc.collect()
    assert desc(weak.get_unstarred()) == after["unstarred"]
    assert desc(strong.get_starred()) == after["starred"]


def _replay_metonym_error(w, case):
    starred, unstarred = FakeObj("s"), FakeObj("u")
    o = w.opts("A", "s:each", ["length", "n:6"])
    args = {
        "no_starred": {**o, "unstarred": unstarred},
        "no_unstarred": {**o, "starred": starred},
        "zero_starred": {**o, "starred": 0, "unstarred": unstarred},
        "empty_unstarred": {**o, "starred": starred, "unstarred": ""},
        "string_starred": {**o, "starred": "abc", "unstarred": unstarred},
        "string_unstarred": {**o, "starred": starred, "unstarred": "abc"},
        "no_category": {**w.opts("s:", "s:each", ["length", "n:6"]), "starred": starred,
                        "unstarred": unstarred},
        "no_name": {**w.opts("A", "u", ["length", "n:6"]), "unstarred": unstarred},
        "no_info_loss": {**w.opts("A", "s:each", "u"), "unstarred": unstarred},
    }[case["label"]]
    m, e = err(lambda: SMetonym(args))
    assert int(m is None) == case["dies"], case
    assert e == case["error"], case
    if m is not None:
        assert desc(m.get_starred()) == case["starred"]
        assert desc(m.get_unstarred()) == case["unstarred"]


# --- from reading the source ----------------------------------------------------------

def test_create_ignores_extra_opts_and_keeps_first(world):
    a = world.cat["A"]
    t = SMetonymType.create({"category": a, "name": "each", "info_loss": {"length": 2},
                             "starred": 1, "unstarred": 2})
    assert SMetonymType.create(category=a, name="each", info_loss={"length": SInt(2)}) is t


def test_create_info_loss_type_errors(world):
    a = world.cat["A"]
    with pytest.raises(Confess, match="Not a HASH reference"):
        SMetonymType.create({"category": a, "name": "each", "info_loss": [1]})
    with pytest.raises(Confess, match=r'Can\'t use string \("abc"\) as a HASH ref'):
        SMetonymType.create({"category": a, "name": "each", "info_loss": "abc"})
    # undef autovivifies to {} for the key, then new dies (nothing memoized yet).
    with pytest.raises(Confess, match="Need info_loss"):
        SMetonymType.create({"category": a, "name": "each", "info_loss": None})


def test_metonym_unstarred_scalar_cannot_be_weakened(world):
    with pytest.raises(Confess, match="Can't weaken a nonreference"):
        SMetonym(category=world.cat["A"], name="each", info_loss={}, starred=1, unstarred=5)


def test_sameness_finder_form(world):
    """Sameness's 'each' finder calls SMetonym(category=, name=, starred=, unstarred=, info_loss=)."""
    a, x, y = world.cat["A"], world.obj["x"], world.obj["y"]
    m = smetonym.SMetonym(category=a, name="each", starred=x, unstarred=y, info_loss={"length": SInt(3)})
    assert m.get_starred() is x and m.get_unstarred() is y
    assert m.get_category() is a and m.get_name() == "each"
    assert m.get_type() is SMetonymType.create(category=a, name="each", info_loss={"length": 3})

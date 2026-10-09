"""Tests for seqsee.schoose and seqsee.set.weighted.

Mirrors Perl: lib/SChoose.pm and lib/Set/Weighted.pm.
Golden data: oracle/schoose.pl -> golden/schoose.json (seeded drand48, exact match).
"""
import pytest

import golden
from seqsee import schoose, util
from seqsee.errors import Confess
from seqsee.set.weighted import SetWeighted

CASES = golden.load("schoose")


def cases(op):
    return [c for c in CASES if c["op"] == op]


def g(x):
    """Golden form of a scalar result (Perl string form, None for undef)."""
    return None if x is None else util.perl_str(x)


def gf(x):
    return float("%.15g" % x)


def case_id(c):
    return "-".join(str(c.get(k)) for k in ("weights", "how_many", "items", "names", "seed")
                    if k in c)


# --- choose / choose_if_non_zero ----------------------------------------------

@pytest.mark.parametrize("fn", ["choose", "choose_if_non_zero"])
def test_choose_golden(fn):
    for c in cases(fn):
        util.srand(c["seed"])
        f = getattr(schoose, fn)
        n = len(c["results"])
        if c["names"] is None:
            res = [g(f(c["weights"])) for _ in range(n)]
        else:
            res = [g(f(c["weights"], c["names"])) for _ in range(n)]
        assert res == c["results"], case_id(c)
        assert gf(util.rand()) == c["next_rand"], case_id(c)


def test_choose_all_zero_returns_last_but_if_non_zero_returns_none():
    util.srand(1)
    assert schoose.choose([0, 0, 0], ["a", "b", "c"]) == "c"
    state = util.RNG.getstate()
    assert schoose.choose_if_non_zero([0, 0], ["a", "b"]) is None
    assert util.RNG.getstate() == state  # no draw when the sum is zero


def test_choose_empty_draws_nothing():
    util.srand(1)
    state = util.RNG.getstate()
    assert schoose.choose([]) is None
    assert schoose.choose_if_non_zero([]) is None
    assert util.RNG.getstate() == state


# --- choose_a_few_nonzero -----------------------------------------------------

@pytest.mark.parametrize("c", cases("choose_a_few_nonzero"), ids=case_id)
def test_choose_a_few_nonzero_golden(c):
    util.srand(c["seed"])
    if c["names"] is None:
        runs = [[g(x) for x in schoose.choose_a_few_nonzero(c["how_many"], c["weights"])]]
    else:
        runs = [[g(x) for x in schoose.choose_a_few_nonzero(c["how_many"], c["weights"], c["names"])]
                for _ in c["results"]]
    assert runs == c["results"]
    assert gf(util.rand()) == c["next_rand"]


def test_choose_a_few_nonzero_does_not_mutate_input():
    w = [1, 2, 3]
    util.srand(3)
    schoose.choose_a_few_nonzero(3, w)
    assert w == [1, 2, 3]


# --- uniform ------------------------------------------------------------------

@pytest.mark.parametrize("c", cases("uniform"), ids=case_id)
def test_uniform_golden(c):
    util.srand(c["seed"])
    assert [g(schoose.uniform(c["items"])) for _ in c["results"]] == c["results"]
    assert gf(util.rand()) == c["next_rand"]


# --- using_fascination --------------------------------------------------------

def test_using_fascination_passes_arguments_swapped():
    # PERL-QUIRK: the objects are used as the weights and the fascinations as names.
    calls = []

    class Obj(int):
        def get_fascination(self, kind):
            calls.append(kind)
            return f"f{int(self)}"

    objs = [Obj(3), Obj(1)]
    util.srand(42)
    got = [schoose.using_fascination(objs, "x") for _ in range(20)]
    util.srand(42)
    want = [schoose.choose(objs, ["f3", "f1"]) for _ in range(20)]
    assert got == want
    assert set(calls) == {"x"}


# --- create -------------------------------------------------------------------

class WObj:
    def __init__(self, n, w):
        self.n, self._w = n, w

    def w(self):
        return self._w

    def name(self):
        return self.n


CHOOSERS = {
    "plain": {},
    "map": {"map": lambda o: o.w()},
    "map_str": {"map": lambda o: o.w() * 2},       # Perl: q{$_->w * 2}
    "grep": {"grep": lambda x: x > 1},
    "grep_str": {"grep": lambda x: x != 3},         # Perl: q{$_ != 3}
    "map_grep": {"map": lambda o: o.w(), "grep": lambda o: o.name()[0] in "ab"},
    "map_grep_odd": {"map": lambda o: o.w(), "grep": lambda o: o.w() != 2},
}


@pytest.mark.parametrize("c", cases("create"), ids=lambda c: f"{c['chooser']}-{c['items']}")
def test_create_golden(c):
    chooser = schoose.create(CHOOSERS[c["chooser"]])
    objs = c["items"] if c["kind"] == "num" else [WObj(*p) for p in c["items"]]
    util.srand(c["seed"])
    res = []
    for _ in c["results"]:
        x = chooser(objs)
        res.append(x.name() if isinstance(x, WObj) else g(x))
    assert res == c["results"]
    # Perl list context gives () only where the port returns None.
    assert [0 if r is None else 1 for r in res] == c["list_counts"]
    assert gf(util.rand()) == c["next_rand"]


def test_create_fall_through_returns_empty_string():
    # PERL-QUIRK: negative likelihoods can leave no partial sum >= the draw.
    chooser = schoose.create({})
    util.srand(1)
    got = [chooser([-1, -2]) for _ in range(10)]
    assert "" in got and -1 in got


def test_create_keyword_form_and_string_code_rejected():
    util.srand(5)
    a = [schoose.create(map=lambda x: x * 2)([1, 2, 3]) for _ in range(10)]
    util.srand(5)
    b = [schoose.create({"map": lambda x: x * 2})([1, 2, 3]) for _ in range(10)]
    assert a == b
    with pytest.raises(Confess):
        schoose.create({"map": "$_ * 2"})


# --- Set::Weighted ------------------------------------------------------------

def mk(pairs):
    return SetWeighted(*[list(p) for p in pairs])


def by_key(pairs):
    return sorted(([k, v] for k, v in pairs), key=lambda p: p[0])


def test_sw_empty():
    (c,) = cases("sw_empty")
    s = SetWeighted()
    assert s.is_empty() == c["is_empty"]
    assert s.is_not_empty() == c["is_not_empty"]
    assert s.get_elements() == c["elements"]
    (c,) = cases("sw_empty_choose")
    util.srand(1)
    assert s.choose() == c["result"]
    assert s.choose_a_few_nonzero(2) == c["few"]
    assert gf(util.rand()) == c["next_rand"]


def test_sw_basic():
    (c,) = cases("sw_basic")
    s = mk(c["pairs"])
    assert s.is_empty() == c["is_empty"]
    assert s.is_not_empty() == c["is_not_empty"]
    assert s.get_elements() == c["elements"]
    assert s.get_elements(1) == c["elements_1"]
    assert s.get_elements(2.5) == c["elements_2_5"]
    assert s.get_elements(-1) == c["elements_neg"]
    assert s.get_elements(100) == c["elements_100"]


def test_sw_insert():
    (c,) = cases("sw_insert")
    s = mk(cases("sw_basic")[0]["pairs"])
    s.insert(["f", 4], ["g", 1])
    assert s.get_elements() == c["elements"]
    assert [list(p) for p in s] == c["raw"]


def test_sw_merge_keys():
    (c,) = cases("sw_merge_keys")
    s = mk(c["pairs"])
    s.merge_keys()
    assert by_key(s) == c["merged"]


@pytest.mark.parametrize("c", cases("sw_delete_below_threshold"), ids=lambda c: str(c["threshold"]))
def test_sw_delete_below_threshold(c):
    s = mk(cases("sw_basic")[0]["pairs"])
    s.delete_below_threshold(c["threshold"])
    assert [list(p) for p in s] == c["raw"]


@pytest.mark.parametrize("c", cases("sw_delete_key"), ids=lambda c: c["key"])
def test_sw_delete_key(c):
    s = mk(cases("sw_basic")[0]["pairs"])
    s.delete_key(c["key"])
    assert by_key(s) == c["remaining"]


def test_sw_choose():
    (c,) = cases("sw_choose")
    s = mk(c["pairs"])
    util.srand(c["seed"])
    assert [s.choose() for _ in c["results"]] == c["results"]
    assert gf(util.rand()) == c["next_rand"]


def test_sw_choose_a_few_nonzero():
    (c,) = cases("sw_choose_a_few_nonzero")
    s = mk(c["pairs"])
    util.srand(c["seed"])
    assert [s.choose_a_few_nonzero(k) for k in c["how_many"]] == c["results"]
    assert gf(util.rand()) == c["next_rand"]


def test_sw_object_keys():
    o1, o2 = WObj("p", 1), WObj("q", 1)
    s = SetWeighted([o1, 1], [o2, 2], [o1, 3])
    s.merge_keys()
    assert sorted([k.name(), v] for k, v in s) == cases("sw_object_keys")[0]["merged"]
    assert any(k is o1 for k, _ in s)
    s = SetWeighted([o1, 1], [o2, 2], [o1, 3])
    s.delete_key(o1)
    assert [[k.name(), v] for k, v in s] == cases("sw_object_delete_key")[0]["remaining"]


def test_sw_scalar_keys_merge_by_string_form():
    s = SetWeighted([1, 1], ["1", 2], [1.0, 3])
    s.merge_keys()
    assert len(s) == 1 and s[0][1] == 6

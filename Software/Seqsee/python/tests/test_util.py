"""Tests for seqsee.util — mirrors lib/SUtil.pm, plus Perl's rand/srand (drand48)
and List::Util::shuffle, which share one RNG state in Perl.

Golden data: oracle/util.pl -> golden/util.json.
"""
import pytest
from hypothesis import given, strategies as st

import golden
from seqsee import util
from seqsee.errors import Confess

GOLDEN = golden.load("util")


def cases(op):
    return [c for c in GOLDEN if c["op"] == op]


def g15(x):
    """Perl (and JSON::PP) prints floats with 15 significant digits."""
    return float("%.15g" % x)


# --- RNG ------------------------------------------------------------------

@pytest.mark.parametrize("case", cases("rand"), ids=lambda c: c["seed"])
def test_rand_golden(case):
    seed = float(case["seed"]) if "." in case["seed"] else int(case["seed"])
    ret = util.srand(seed)
    assert util.perl_str(ret) == case["srand_returns"].replace("0 but true", "0")
    assert [g15(util.rand()) for _ in case["draws"]] == case["draws"]


def test_rand_n_golden():
    (case,) = cases("rand_n")
    util.srand(case["seed"])
    assert [g15(util.rand(n)) for n in case["args"]] == case["draws"]


def test_int_rand_golden():
    (case,) = cases("int_rand")
    util.srand(case["seed"])
    assert [int(util.rand(case["n"])) for _ in case["draws"]] == case["draws"]


def test_reseed_restarts_sequence():
    (case,) = cases("reseed_same")
    util.srand(99)
    a = [util.rand() for _ in range(3)]
    util.srand(99)
    assert ([util.rand() for _ in range(3)] == a) == bool(case["same"])


def test_drand48_formula_exact():
    rng = util.Drand48(42)
    x = (42 << 16) | 0x330E
    for _ in range(100):
        x = (0x5DEECE66D * x + 0xB) % 2**48
        assert rng.rand() == x / 2**48


def test_independent_generators_and_state():
    a, b = util.Drand48(7), util.Drand48(7)
    assert [a.rand() for _ in range(5)] == [b.rand() for _ in range(5)]
    state = a.getstate()
    nxt = [a.rand() for _ in range(3)]
    a.setstate(state)
    assert [a.rand() for _ in range(3)] == nxt


def test_unseeded_rng_autoseeds():
    rng = util.Drand48()
    x = rng.rand()
    assert 0 <= x < 1


def test_srand_without_seed_returns_seed():
    seed = util.srand()
    first = util.rand()
    util.srand(seed)
    assert util.rand() == first


@given(st.integers(min_value=0, max_value=2**32 - 1))
def test_rand_in_unit_interval(seed):
    rng = util.Drand48(seed)
    for _ in range(5):
        assert 0 <= rng.rand() < 1


def test_seeded_fixture_seeds_central_rng(seeded):
    first = util.rand()
    util.srand(12345)
    assert util.rand() == first


# --- toss -------------------------------------------------------------------

@pytest.mark.parametrize("case", cases("toss"), ids=lambda c: f"{c['seed']}-{c['prob']}")
def test_toss_golden(case):
    util.srand(case["seed"])
    assert [util.toss(case["prob"]) for _ in case["draws"]] == case["draws"]


def test_toss_mixed_golden():
    (case,) = cases("toss_mixed")
    util.srand(case["seed"])
    assert [util.toss(p) for p in case["probs"]] == case["draws"]


def test_toss_none_dies():
    assert cases("toss_undef")[0]["dies"] == 1
    with pytest.raises(Confess, match="uninitialized prob"):
        util.toss(None)


# --- shuffle ----------------------------------------------------------------

@pytest.mark.parametrize("case", cases("shuffle"), ids=lambda c: str(c["seed"]))
def test_shuffle_golden(case):
    util.srand(case["seed"])
    assert util.shuffle(case["input"]) == case["output"]
    assert util.shuffle(["a", "b", "c", "d", "e"] if case["input"] else ["x"]) == case["output2"]
    assert g15(util.rand()) == case["after"]


def test_shuffle_does_not_mutate_input():
    items = [1, 2, 3, 4]
    util.srand(1)
    util.shuffle(items)
    assert items == [1, 2, 3, 4]


# --- Perl scalar helpers -----------------------------------------------------

@pytest.mark.parametrize("value, text", [
    (1, "1"), (1.0, "1"), (0.1, "0.1"), (1e20, "1e+20"), (-0.5, "-0.5"),
    (1 / 3, "0.333333333333333"), ("abc", "abc"), (None, ""), (True, "1"), (False, ""),
])
def test_perl_str(value, text):
    assert util.perl_str(value) == text


@pytest.mark.parametrize("value, truth", [
    (0, False), (1, True), ("", False), ("0", False), ("0.0", True), ("00", True),
    (None, False), (0.0, False), ("a", True), ([], True),
])
def test_perl_true(value, truth):
    assert util.perl_true(value) is truth


@pytest.mark.parametrize("value, num", [
    (3, 3), (2.5, 2.5), ("1.0", 1.0), ("12abc", 12), ("abc", 0), ("", 0), (None, 0),
    ("  -3e2x", -300.0), (".5", 0.5),
])
def test_perl_num(value, num):
    assert util.perl_num(value) == num


# --- SUtil functions -----------------------------------------------------------

@pytest.mark.parametrize("case", cases("uniq"), ids=lambda c: repr(c["input"]))
def test_uniq_golden(case):
    # Perl returns hash values (unordered); compare sorted by string.
    out = util.uniq(*case["input"])
    assert len(out) == case["count"]
    assert sorted(out, key=util.perl_str) == case["sorted"]


def test_uniq_keeps_last_value_and_objects_by_identity():
    a, b = object(), object()
    assert util.uniq(1, "1") == ["1"]
    assert util.uniq(a, b, a) == [a, b]


@pytest.mark.parametrize("case", cases("compare_deep"), ids=lambda c: f"{c['a']}~{c['b']}")
def test_compare_deep_golden(case):
    assert util.compare_deep(case["a"], case["b"]) == bool(case["result"])


def test_compare_deep_dies():
    (case,) = cases("compare_deep_dies")
    assert case["one_arg"] == case["hashref"] == 1
    with pytest.raises(Confess):
        util.compare_deep(1)
    with pytest.raises(Confess):
        util.compare_deep({}, {})


class FlatObj:
    def __init__(self, *items):
        self.items = items

    def flatten(self):
        return list(self.items)


@pytest.mark.parametrize("case", cases("equal_when_flattened"),
                         ids=lambda c: f"{c['a']}~{c['b']}")
def test_equal_when_flattened_golden(case):
    def mk(x):
        return FlatObj(*x) if isinstance(x, list) else x
    assert util.equal_when_flattened(mk(case["a"]), mk(case["b"])) == bool(case["result"])


@pytest.mark.parametrize("case", cases("myall"), ids=lambda c: repr(c["input"]))
def test_myall_golden(case):
    assert util.myall(*case["input"]) == bool(case["result"])


@pytest.mark.parametrize("case", cases("all_gt1"), ids=lambda c: repr(c["input"]))
def test_all_golden(case):
    assert util.all_(lambda x: util.perl_num(x) > 0, *case["input"]) == bool(case["result"])


@pytest.mark.parametrize("case", cases("significant"), ids=lambda c: str(c["x"]))
def test_significant_golden(case):
    assert util.significant(case["x"]) == case["result"]


@pytest.mark.parametrize("case", cases("minmax"), ids=lambda c: repr(c["input"]))
def test_minmax_golden(case):
    assert list(util.minmax(*case["input"])) == case["result"]


def test_minmax_empty_dies():
    assert cases("minmax_empty")[0]["dies"] == 1
    with pytest.raises(Confess, match="undefined!"):
        util.minmax()


@pytest.mark.parametrize("case", cases("odd_position"), ids=lambda c: repr(c["input"]))
def test_odd_position_golden(case):
    assert util.odd_position(*case["input"]) == case["result"]


def test_odd_position_needs_three():
    assert cases("odd_position_short")[0]["dies"] == 1
    with pytest.raises(Confess, match="at least three"):
        util.odd_position(1, 2)


@pytest.mark.parametrize("case", cases("naive_brittle_chunking"), ids=lambda c: repr(c["input"]))
def test_naive_brittle_chunking_golden(case):
    assert util.naive_brittle_chunking(case["input"]) == case["result"]


@pytest.mark.parametrize("case", cases("next_available_file_number"), ids=lambda c: c["dir"])
def test_next_available_file_number_golden(case, tmp_path, monkeypatch):
    monkeypatch.chdir(tmp_path)
    if case["dir"] != "missing":
        (tmp_path / case["dir"]).mkdir()
        for f in case["files"]:
            (tmp_path / case["dir"] / f).touch()
    assert util.next_available_file_number(case["dir"]) == case["result"]


@pytest.mark.parametrize("case", cases("structure_to_string"), ids=lambda c: repr(c["input"]))
def test_structure_to_string_golden(case):
    for fn, key in ((util.structure_to_string, "result"), (util.stringify_deep_array, "deep")):
        out = fn(case["input"])
        if isinstance(case["input"], list):
            assert out == case[key]
        else:  # scalars come back unchanged
            assert out == case["input"]


@pytest.mark.parametrize("case", cases("trim"), ids=lambda c: repr(c["input"]))
def test_trim_golden(case):
    assert util.trim(case["input"]) == case["result"]


class TextObj:
    def __init__(self, t):
        self.t = t

    def as_text(self):
        return f"T({self.t})"


def _carp_input(what):
    from seqsee.constants import DIR
    return {
        "scalar": 17, "string": "hi", "undef": None, "as_text": TextObj("x"),
        "array": [1, TextObj("y"), "z"], "empty_array": [],
        "hash_scalar": {"k": 3}, "hash_undef": {"k": None},
        "hash_as_text": {"k": TextObj("v")}, "hash_array": {"k": [1, TextObj("w")]},
        "empty_hash": {}, "code": lambda: 1, "dir": DIR.RIGHT,
    }[what]


@pytest.mark.parametrize("case", [c for c in cases("stringify_for_carp")
                                  if c["what"] != "scalar_ref"], ids=lambda c: c["what"])
def test_stringify_for_carp_golden(case):
    assert util.stringify_for_carp(_carp_input(case["what"])) == case["result"]


def test_stringify_for_carp_other_object():
    class Thing:
        pass
    assert util.stringify_for_carp(Thing()) == "reftype=Thing"


@pytest.mark.parametrize("case", cases("hash_sorted_as_array"), ids=lambda c: repr(c["input"]))
def test_hash_sorted_as_array_golden(case):
    assert util.hash_sorted_as_array(case["input"]) == case["result"]


def test_generate_blemished():
    calls = []

    class Built:
        def apply_blemish_at(self, blemish, pos):
            calls.append(("apply", blemish, pos))
            return "blemished"

    class Cat:
        def build(self, args):
            calls.append(("build", args))
            return Built()

    out = util.generate_blemished(cat=Cat(), blemish="double", pos=2, start=1, end=4)
    assert out == "blemished"
    assert calls == [("build", {"start": 1, "end": 4}), ("apply", "double", 2)]


def test_oddman_is_dead_code():
    # PERL-QUIRK: oddman uses $SCat::ascending::ascending etc., which no longer exist.
    with pytest.raises(Confess):
        util.oddman(FlatObj(1), FlatObj(2), FlatObj(3))


def test_clear_all_clears_coderack():
    from seqsee import scoderack
    from seqsee.scodelet import SCodelet
    scoderack.add_codelet(SCodelet("Foo", 10, {}))
    util.clear_all_but_workspace()
    assert scoderack.get_codelet_count() == 0 and scoderack.CODELETS == []
    scoderack.add_codelet(SCodelet("Foo", 10, {}))
    util.clear_all()
    assert scoderack.get_urgencies_sum() == 0

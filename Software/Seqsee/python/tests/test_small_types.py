"""Tests for seqsee.sint, seqsee.sfasc and seqsee.shistory.

Mirrors Perl: lib/SInt.pm, lib/SFasc.pm, lib/SHistory.pm.
Golden data: oracle/small_types.pl -> golden/small_types.json.
"""
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee.constants import DIR
from seqsee.errors import Confess
from seqsee.sfasc import SFasc
from seqsee.shistory import SHistory, history_string
from seqsee.sint import SInt, get_common_categories

CASES = golden.load("small_types")


def cases(op):
    return [c for c in CASES if c["op"] == op]


def cat_names(sint):
    return [c.get_name() for c in sint.get_categories()]


def set_features(names):
    Global.Feature.clear()
    Global.Feature.update({n: 1 for n in names})


# --- SInt ---------------------------------------------------------------------

@pytest.mark.parametrize("case", cases("new"), ids=lambda c: f"{'+'.join(c['features'])}:{c['mag']}")
def test_sint_new_golden(case):
    set_features(case["features"])
    s = SInt(case["mag"])
    assert s.get_mag() == case["get_mag"]
    assert s.as_text() == case["as_text"]
    assert str(s) == case["str"]
    assert cat_names(s) == case["categories"]


@pytest.mark.parametrize("case", cases("arith"), ids=lambda c: f"{c['a']},{c['b']}")
def test_sint_arith_golden(case):
    a, b = case["a"], case["b"]
    sa, sb = SInt(a), SInt(b)
    assert (sa + sb).get_mag() == case["add_sint"]
    assert (sa + b).get_mag() == case["add_num"]
    assert (a + sb).get_mag() == case["radd_num"]
    assert (sa - sb).get_mag() == case["sub_sint"]
    assert (sa - b).get_mag() == case["sub_num"]
    assert (a - sb).get_mag() == case["rsub_num"]
    assert type(sa + sb).__name__ == case["result_class"]
    assert int(sa == sb) == case["eq_sint"]
    assert int(sa != sb) == case["ne_sint"]
    assert int(sa == b) == case["eq_num"]
    assert int(sa != b) == case["ne_num"]
    assert int(a == sb) == case["req_num"]


def test_sint_eq_numeric_string_golden():
    (case,) = cases("eq_numeric_string")
    s = SInt(4)
    assert int(s == "4.0") == case["eq"]
    assert int(s != "4.0") == case["ne"]


def test_sint_arith_returns_new_object():
    a = SInt(2)
    assert (a + 0) is not a
    assert a.get_mag() == 2


def test_sint_hash_is_identity():
    a, b = SInt(3), SInt(3)
    assert a == b
    assert len({a, b}) == 2


def test_sint_direction_golden():
    (case,) = cases("direction")
    assert case["dir"] == "RIGHT"
    assert SInt(4).get_direction() is DIR.RIGHT


def test_sint_add_category_golden():
    (case,) = cases("add_category")
    s = SInt(4)
    s.add_category(S.PRIME)
    s.add_category(S.PRIME)
    s.add_category(S.NUMBER)
    s.add_category(S.EVEN)
    assert cat_names(s) == case["categories"]


def test_sint_categories_are_the_s_singletons():
    set_features(["Primes", "Parity"])
    cats = SInt(3).get_categories()
    assert cats[0] is S.NUMBER and cats[1] is S.PRIME and cats[2] is S.ODD
    assert SInt(4).get_categories()[1] is S.EVEN


@pytest.mark.parametrize("case", cases("common"), ids=lambda c: str(c["mags"]))
def test_sint_common_categories_golden(case):
    set_features(["Primes", "Parity"])
    sints = [SInt(m) for m in case["mags"]]
    # Perl returns hash order; compare sorted.
    assert sorted(c.get_name() for c in get_common_categories(*sints)) == case["common"]


def test_sint_common_categories_static_alias():
    set_features([])
    assert SInt.get_common_categories(SInt(1), SInt(2)) == [S.NUMBER]


def test_sint_get_pure():
    # Item 028: SLTM::Platonic->create(mag), memoized (more in test_memory_plumbing.py).
    pure = SInt(3).get_pure()
    assert pure.as_text() == "plat3"
    assert SInt(3).get_pure() is pure


_PRIME_KEY_ARGS = {"float 2.0": 2.0, "string 2.0": "2.0", "string 2": "2"}


@pytest.mark.parametrize("case", cases("new_prime_key"), ids=lambda c: c["label"])
def test_sint_is_prime_string_key_golden(case):
    # Perl: $num ~~ %Primes is a lookup on the string form ("2.0" is not a key).
    set_features(["Primes"])
    assert cat_names(SInt(_PRIME_KEY_ARGS[case["label"]])) == case["categories"]


# --- SFasc --------------------------------------------------------------------

_FASC_ARGS = {"none": None, "40": 40, "0": 0, "": "", "75.5": 75.5}


@pytest.mark.parametrize("case", cases("fasc"), ids=lambda c: c["arg"])
def test_sfasc_golden(case):
    arg = _FASC_ARGS[case["arg"]]
    f = SFasc() if case["arg"] == "none" else SFasc({"strength": arg})
    assert f.get_strength() == case["strength"]


def test_sfasc_set_golden():
    (case,) = cases("fasc_set")
    f = SFasc(strength=10)
    f.set_strength(60)
    assert f.get_strength() == case["strength"]


# --- SHistory -----------------------------------------------------------------

def test_shistory_golden_sequence():
    by_op = {c["op"]: c for c in CASES if c["op"].startswith("hist")}

    Global.Steps_Finished = 0
    Global.CurrentRunnableString = ""
    h = SHistory()
    c = by_op["hist_new"]
    assert h.get_history() == c["history"]
    assert h.get_age() == c["age"]
    assert h.unchanged_since(0) == c["unchanged_since_0"]
    assert h.unchanged_since(-1) == c["unchanged_since_neg"]

    Global.Steps_Finished = 5
    Global.CurrentRunnableString = "Codelet(foo)"
    h.add_history("Added category ascending")
    c = by_op["hist_add"]
    assert h.get_history() == c["history"]
    assert h.get_age() == c["age"]
    for since in (4, 5, 6):
        assert h.unchanged_since(since) == c[f"unchanged_since_{since}"]

    Global.Steps_Finished = 12
    Global.CurrentRunnableString = ""
    h.add_history("Removed category ascending")
    c = by_op["hist_add2"]
    assert h.get_history() == c["history"]
    assert h.get_age() == c["age"]
    assert h.history_as_text() == c["as_text"]
    # PERL-QUIRK: the pattern is only tested for truth, never matched.
    assert h.search_history(re.compile("Removed")) == c["search_qr"]
    assert h.search_history("nomatch") == c["search_str"]
    assert h.search_history("") == c["search_empty"]
    assert h.search_history("0") == c["search_zero"]

    h2 = SHistory()
    Global.Steps_Finished = 20
    c = by_op["hist_dob"]
    assert h2.get_history() == c["history"]
    assert h2.get_age() == c["age"]

    Global.Steps_Finished = None
    Global.CurrentRunnableString = "X"
    h3 = SHistory()
    Global.Steps_Finished = 3
    c = by_op["hist_undef_steps"]
    assert h3.get_history() == c["history"]
    assert h3.get_age() == c["age"]


def test_shistory_get_history_is_live():
    h = SHistory()
    hist = h.get_history()
    h.add_history("x")
    assert hist[-1] == "[0]\tx"


def test_history_string():
    Global.Steps_Finished = 7
    Global.CurrentRunnableString = "R"
    assert history_string("m") == "[7]R\tm"


def test_unchanged_since_confesses_on_bad_message():
    h = SHistory()
    h.get_history().append("garbage")
    with pytest.raises(Confess, match="Huh 'garbage'"):
        h.unchanged_since(0)


def test_global_reset_restores_defaults():
    Global.Steps_Finished = 9
    Global.CurrentRunnableString = "x"
    Global.Feature["Primes"] = 1
    Global.reset()
    assert Global.Steps_Finished == 0
    assert Global.CurrentRunnableString == ""
    assert Global.Feature == {}

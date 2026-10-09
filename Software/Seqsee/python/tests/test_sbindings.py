"""Tests for seqsee.sbindings (and the SMetonym.intersection piece it needs).

Mirrors Perl: lib/SBindings.pm (and SMetonym::intersection in lib/SMetonym.pm).
Golden data: oracle/sbindings.pl -> golden/sbindings.json.
"""
import pytest

import golden
from seqsee.constants import METO_MODE, POS_MODE
from seqsee.errors import Confess
from seqsee.sbindings import SBindings
from seqsee.smetonym import SMetonym
from seqsee.spos import SPos

CASES = golden.load("sbindings")


def cases(op):
    return [c for c in CASES if c["op"] == op]


def case(op):
    (c,) = cases(op)
    return c


class FakeCat:
    def __init__(self, n):
        self.n = n

    def get_name(self):
        return self.n


class FakeType:
    def __init__(self, n, cat):
        self.n, self.cat = n, cat

    def get_name(self):
        return self.n

    def get_category(self):
        return self.cat


class FakeMeto:
    def __init__(self, t):
        self.t = t

    def get_type(self):
        return self.t


TYPE1 = FakeType("type1", FakeCat("catA"))
TYPE2 = FakeType("type2", FakeCat("catB"))
TYPES = {"type1": TYPE1, "type2": TYPE2}


def mode_text(m):
    return None if m is None else m.as_text()


def describe(b):
    out = {}
    try:
        out["metonymy_cat"], out["metonymy_cat_dies"] = b.get_metonymy_cat().get_name(), 0
    except Confess:
        out["metonymy_cat"], out["metonymy_cat_dies"] = None, 1
    try:
        out["metonymy_name"], out["metonymy_name_dies"] = b.get_metonymy_name(), 0
    except Confess:
        out["metonymy_name"], out["metonymy_name_dies"] = None, 1
    pos, typ = b.get_position(), b.get_metonymy_type()
    out.update(
        slippages_count=b.slippages_count(),
        slippage_positions=sorted(str(k) for k in b.slippage_positions()),
        all_slippage_types=sorted(m.get_type().get_name() for m in b.all_slippages()),
        metonymy_mode=mode_text(b.get_metonymy_mode()),
        position_mode=mode_text(b.get_position_mode()),
        position=None if pos is None else pos.position,
        metonymy_type=None if typ is None else typ.get_name(),
        binding_keys=sorted(b.get_bindings_ref()),
    )
    return out


def expected(c):
    return {k: c[k] for k in c if k not in ("op", "label", "slippages", "dies")}


def raw_from_spec(spec):
    return {k: FakeMeto(TYPES[v]) for k, v in spec.items()}


# --- golden: create() over slippage sets ---------------------------------------

@pytest.mark.parametrize("c", cases("create"), ids=lambda c: c["label"])
def test_create_golden(c):
    raw = raw_from_spec(c["slippages"])
    if c["dies"]:
        with pytest.raises(Confess):
            SBindings.create(raw, {"a": 1})
        return
    b = SBindings.create(raw, {"a": 1})
    assert describe(b) == expected(c)


def test_int_keys_work_like_string_keys():
    b = SBindings.create({2: FakeMeto(TYPE1)}, {})
    assert b.get_position().position == 3
    assert b.slippage_positions() == [2]


# --- golden: bindings ---------------------------------------------------------------

def test_bindings_golden():
    c = case("bindings")
    bdg = {"a": 1, "b": "x", "length": 3}
    b = SBindings.create({}, bdg, "ignored third arg")
    for k in ("a", "b", "length", "missing"):
        assert b.get_binding_for_attribute(k) == c[k]
    assert (b.get_bindings_ref() is bdg) == bool(c["same_ref"])
    bdg["c"] = 7
    assert b.get_binding_for_attribute("c") == case("bindings_live")["c"]


def test_squinting_raw_shared():
    raw = {1: FakeMeto(TYPE1)}
    b = SBindings.create(raw, {})
    assert (b.get_squinting_raw() is raw) == bool(case("squinting_raw_shared")["same_ref"])


# --- golden: argument checking ---------------------------------------------------------

@pytest.mark.parametrize("op, call", [
    ("create_undef_slippages", lambda: SBindings.create(None, {})),
    ("create_undef_bindings", lambda: SBindings.create({}, None)),
    ("create_no_args", lambda: SBindings.create()),
    ("create_array_bindings", lambda: SBindings.create({}, [1])),
    ("create_array_slippages", lambda: SBindings.create([1], {})),
    ("new_missing_bindings", lambda: SBindings(raw_slippages={})),
    ("new_missing_slippages", lambda: SBindings(bindings={})),
])
def test_argument_checking(op, call):
    assert case(op)["dies"] == 1
    with pytest.raises(Confess):
        call()


def test_new_hashref_with_extra_arg():
    c = case("new_hashref_extra_arg")
    b = SBindings({"raw_slippages": {}, "bindings": {"x": 1}, "object": "o"})
    assert c["dies"] == 0 and b.get_binding_for_attribute("x") == c["x"]
    b = SBindings(raw_slippages={}, bindings={"x": 1}, object="o")
    assert b.get_binding_for_attribute("x") == c["x"]


def test_constructor_modes_golden():
    b0 = SBindings(raw_slippages={}, bindings={},
                   metonymy_mode=METO_MODE.ALL, position_mode=POS_MODE.BACKWARD)
    assert describe(b0) == expected(case("new_modes_empty"))
    b1 = SBindings(raw_slippages={4: FakeMeto(TYPE1)}, bindings={},
                   metonymy_mode=METO_MODE.ALL, position_mode=POS_MODE.BACKWARD,
                   position=SPos(1))
    assert describe(b1) == expected(case("new_modes_one"))
    b2 = SBindings(raw_slippages={4: FakeMeto(TYPE1), 5: FakeMeto(TYPE1)}, bindings={},
                   metonymy_mode=METO_MODE.ALL, position_mode=POS_MODE.BACKWARD,
                   position=SPos(2), metonymy_type=TYPE2)
    assert describe(b2) == expected(case("new_modes_two_same"))
    # PERL-QUIRK: a failed intersection does not clear a constructor-supplied type.
    b3 = SBindings(raw_slippages={4: FakeMeto(TYPE1), 5: FakeMeto(TYPE2)}, bindings={},
                   metonymy_type=TYPE2)
    assert describe(b3) == expected(case("new_type_two_different"))


def test_constructor_type_checks():
    with pytest.raises(Confess):
        SBindings(raw_slippages={}, bindings={}, metonymy_mode="ALL")
    with pytest.raises(Confess):
        SBindings(raw_slippages={}, bindings={}, position=3)


# --- golden: setters ---------------------------------------------------------------------

def _dies(fn):
    try:
        fn()
    except Confess:
        return 1
    return 0


def test_setters_golden():
    b = SBindings.create({}, {})

    def setter(attr, value):
        return lambda: setattr(b, attr, value)

    c = case("set_meto_mode")
    assert _dies(setter("metonymy_mode", METO_MODE.ALLBUTONE)) == c["dies"]
    assert mode_text(b.get_metonymy_mode()) == c["value"]
    assert _dies(setter("metonymy_mode", "ALL")) == case("set_meto_mode_bad")["dies"]
    assert _dies(setter("metonymy_mode", POS_MODE.FORWARD)) == case("set_meto_mode_pos_mode")["dies"]
    assert _dies(setter("metonymy_mode", None)) == case("set_meto_mode_undef")["dies"]
    c = case("set_pos_mode")
    assert _dies(setter("position_mode", POS_MODE.BACKWARD)) == c["dies"]
    assert mode_text(b.get_position_mode()) == c["value"]
    assert _dies(setter("position_mode", METO_MODE.ALL)) == case("set_pos_mode_bad")["dies"]
    c = case("set_position")
    assert _dies(setter("position", SPos(-1))) == c["dies"]
    assert b.get_position().position == c["value"]
    assert _dies(setter("position", 3)) == case("set_position_bad")["dies"]
    c = case("set_meto_type")
    assert _dies(setter("metonymy_type", TYPE2)) == c["dies"]
    assert b.get_metonymy_name() == c["name"]
    assert b.get_metonymy_cat().get_name() == c["cat"]
    c = case("set_meto_type_any")
    assert _dies(setter("metonymy_type", 42)) == c["dies"]
    assert _dies(b.get_metonymy_name) == c["name_dies"]


def test_stories_are_no_ops():
    b = SBindings.create({}, {})
    assert case("tell_stories")["dies"] == 0
    assert b.tell_directed_story(1, 2) is None
    assert b.tell_backward_story() is None
    assert b.tell_forward_story() is None


# --- SMetonym.intersection ---------------------------------------------------------------

def test_intersection():
    with pytest.raises(Confess):
        SMetonym.intersection()
    assert SMetonym.intersection(FakeMeto(TYPE1)) is TYPE1
    assert SMetonym.intersection(FakeMeto(TYPE1), FakeMeto(TYPE1)) is TYPE1
    assert SMetonym.intersection(FakeMeto(TYPE1), FakeMeto(TYPE2)) is None

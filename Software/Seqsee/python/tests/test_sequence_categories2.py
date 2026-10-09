"""Tests for the sequence categories Mountain and Interlaced.

Mirrors lib/SCategory/Mountain.pm and lib/SCategory/Interlaced.pm.
Golden data: oracle/sequence_categories2.pl.

As in the oracle, Seqsee::Object->create is replaced by a recorder; subobjects
are fake Seqsee::Elements. The fakes are shared with test_sequence_categories.
"""
import pytest

import golden
from seqsee import categorizable
from seqsee import s as S
from seqsee.categories import base, interlaced
from seqsee.categories.base import SCategory
from seqsee.categories.interlaced import Interlaced
from seqsee.categories.metonymy_spec import NotMetonyable
from seqsee.categories.mountain import Mountain
from seqsee.constants import RELN_SCHEME
from seqsee.errors import Confess
from seqsee.sbindings import SBindings
from seqsee.sint import SInt
from seqsee.util import perl_str
from test_sequence_categories import (E, FakeBuilt, FakeElemGroup, FakeElemSub, FakeGroup,
                                      FakeMeto, FakeResult, FakeSub, FakeType, FakeVal,
                                      desc, desc_hash, show)

CASES = golden.load("sequence_categories2")


def cases(kind, cat=None):
    return [c for c in CASES if c["kind"] == kind and (cat is None or c.get("cat") == cat)]


def cat_of(key):
    if key == "mountain":
        return S.MOUNTAIN
    return Interlaced.create(int(key.split("_")[1]))


class FakeItemsGroup(FakeGroup):
    """FakeGroup with get_items_array (Interlaced's Instancer)."""

    def get_items_array(self):
        return list(self.items)


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    monkeypatch.setattr(base, "_object_create", lambda *items: FakeBuilt(*items))


# --- inputs, by oracle label ----------------------------------------------------------

MOUNTAIN_BUILDS = {
    "plain": lambda: {"foot": 1, "peak": 4},
    "reversed": lambda: {"foot": 4, "peak": 1},
    "equal": lambda: {"foot": 3, "peak": 3},
    "sint": lambda: {"foot": SInt(2), "peak": SInt(5)},
    "sint equal": lambda: {"foot": SInt(2), "peak": SInt(2)},
    "zero foot": lambda: {"foot": 0, "peak": 2},
    "negative": lambda: {"foot": -1, "peak": 1},
    "floats": lambda: {"foot": 1.5, "peak": 3.7},
    "float peak just above": lambda: {"foot": 1, "peak": 1.5},
    "float equal": lambda: {"foot": 3.5, "peak": 3.5},
    "strings": lambda: {"foot": "2", "peak": "4"},
    "strings equal": lambda: {"foot": "3", "peak": "3.0"},
    "foot undef": lambda: {"foot": None, "peak": 2},
    "peak undef": lambda: {"foot": 2, "peak": None},
    "extra key": lambda: {"foot": 1, "peak": 2, "extra": "x"},
    "missing peak": lambda: {"foot": 1},
    "missing foot": lambda: {"peak": 1},
    "empty": lambda: {},
}

INTERLACED_BUILDS = {
    "all parts": lambda: {"part_no_1": 1, "part_no_2": 2, "part_no_3": 3},
    "objects": lambda: {"part_no_1": FakeVal(1), "part_no_2": SInt(2), "part_no_3": E(3)},
    "missing part": lambda: {"part_no_1": 1, "part_no_3": 3},
    "extra keys": lambda: {"part_no_1": 1, "part_no_2": 2, "part_no_3": 3, "part_no_4": 4, "x": 5},
    "empty": lambda: {},
}

M1 = FakeMeto(FakeType("t1"))
M2 = FakeMeto(FakeType("t2"))


def ok():
    return FakeResult(parts={}, entire=None)


MOUNTAIN_INSTANCER = {
    "plain": lambda: FakeGroup([E(1), E(2), E(3), E(2), E(1)], ok()),
    "even size": lambda: FakeGroup([E(1), E(2), E(2), E(1)], ok()),
    "empty": lambda: FakeGroup([], ok()),
    "single item": lambda: FakeGroup([E(4)], ok()),
    "not seen": lambda: FakeGroup([E(1), E(3), E(1)], None),
    "seen returns 0": lambda: FakeGroup([E(1), E(3), E(1)], 0),
    "zero foot": lambda: FakeGroup([E(0), E(1), E(0)], ok()),
    "valley": lambda: FakeGroup([E(3), E(1), E(3)], ok()),
    "first item undef": lambda: FakeGroup([None, E(2), E(1)], ok()),
    "middle item missing": lambda: FakeGroup([E(1)], ok(), parts_count=3),
    "parts_count even, items odd": lambda: FakeGroup([E(1), E(2), E(1)], ok(), parts_count=2),
    "metonym subobject": lambda: FakeGroup([FakeSub(E(1)), E(5), E(1)], ok()),
    "effective object not an element":
        lambda: FakeGroup([FakeSub(FakeVal(1)), E(5), E(1)], ok()),
    "element subclass peak": lambda: FakeGroup([E(1), FakeElemSub(), E(1)], ok()),
    "parts blemished": lambda: FakeGroup([E(1), E(2), E(1)], FakeResult(parts={1: M1})),
    "parts undef": lambda: FakeGroup([E(1), E(2), E(1)], FakeResult(parts=None)),
    "element group, entire blemish":
        lambda: FakeElemGroup([E(2)], FakeResult(parts={3: M1}, entire=M2)),
    "element group, no entire": lambda: FakeElemGroup([E(2)], FakeResult(parts={2: M1})),
    "non-element group, entire blemish":
        lambda: FakeGroup([E(2), E(3), E(2)], FakeResult(parts={}, entire=M2)),
}

INTERLACED_INSTANCER = {
    "two items": lambda: FakeItemsGroup([E(1), E(2)], ok()),
    "three items": lambda: FakeItemsGroup([E(1), E(2), E(3)], ok()),
    "empty": lambda: FakeItemsGroup([], ok()),
    "undef item": lambda: FakeItemsGroup([None, E(2)], ok()),
    "mixed": lambda: FakeItemsGroup([FakeVal(1), SInt(2), "x"], ok()),
}


# --- golden -------------------------------------------------------------------------------

def test_golden_covers_every_kind():
    kinds = {c["kind"] for c in CASES}
    assert kinds == {"basics", "sufficient", "build", "instancer", "create", "create_dies",
                     "memoize"}
    assert {c["label"] for c in cases("build", "mountain")} == set(MOUNTAIN_BUILDS)
    assert {c["label"] for c in cases("build", "interlaced_3")} == set(INTERLACED_BUILDS)
    assert {c["label"] for c in cases("instancer", "mountain")} == set(MOUNTAIN_INSTANCER)
    assert {c["label"] for c in cases("instancer", "interlaced_2")} == set(INTERLACED_INSTANCER)


@pytest.mark.parametrize("case", cases("basics"), ids=lambda c: c["cat"])
def test_basics_golden(case):
    cat = cat_of(case["cat"])
    assert isinstance(cat, SCategory)
    assert isinstance(cat, NotMetonyable)
    assert cat.perl_name == case["class"]
    assert cat.get_name() == case["get_name"]
    assert cat.as_text() == case["as_text"]
    if case["serialize_died"]:
        with pytest.raises(Confess) as err:
            cat.serialize()
        assert str(err.value) == case["serialize_error"]
    else:
        assert cat.serialize() == case["serialize"]
        copy = type(cat).deserialize(cat.serialize())
        assert copy.perl_name == case["deserialized_class"]
        assert (copy is cat) == bool(case["deserialized_is_same"])
    assert cat.is_pure() == case["is_pure"]
    assert (cat.get_pure() is cat) == bool(case["get_pure_is_self"])
    assert sorted(cat.get_meto_types()) == case["meto_types"]
    assert len(cat.get_memory_dependencies()) == case["memory_deps_count"]
    assert show(cat.is_metonyable()) == case["is_metonyable"]
    assert int(cat.is_numeric()) == case["is_numeric"]
    assert (cat == cat) == bool(case["smartmatch_self"])
    if "parts_count" in case:
        assert cat.get_parts_count() == case["parts_count"]
        assert cat.longer_description() == case["longer_description"]


@pytest.mark.parametrize("case", cases("sufficient"), ids=lambda c: f"{c['cat']}-{c['atts']}")
def test_are_attributes_sufficient_to_build_golden(case):
    got = cat_of(case["cat"]).are_attributes_sufficient_to_build(*case["atts"])
    assert show(got) == case["result"]


@pytest.mark.parametrize("case", cases("build"), ids=lambda c: f"{c['cat']}-{c['label']}")
def test_build_golden(case):
    cat = cat_of(case["cat"])
    table = MOUNTAIN_BUILDS if case["cat"] == "mountain" else INTERLACED_BUILDS
    args = table[case["label"]]()
    if case["died"]:
        with pytest.raises(Confess, match="Too few params"):
            cat.build(args)
    else:
        ret = cat.build(args)
        assert int(ret is not None) == case["defined"]
        if ret is not None:
            assert [desc(x) for x in ret.items] == case["items"]
            assert len(ret.cats) == case["cats_count"]
            if ret.cats:
                got_cat, b = ret.cats[0]
                assert (got_cat is cat) == bool(case["cat_is_self"])
                assert isinstance(b, SBindings)
                assert (b.get_bindings_ref() is args) == bool(case["bindings_shared"])
                assert b.slippages_count() == case["slippages_count"]
            assert (ret.reln is RELN_SCHEME.CHAIN) == bool(case["reln_is_chain"])
    assert desc_hash(args) == case["args_after"]


@pytest.mark.parametrize("case", cases("instancer"), ids=lambda c: f"{c['cat']}-{c['label']}")
def test_instancer_golden(case):
    cat = cat_of(case["cat"])
    table = MOUNTAIN_INSTANCER if case["cat"] == "mountain" else INTERLACED_INSTANCER
    group = table[case["label"]]()
    if case["died"]:
        with pytest.raises(AttributeError):
            cat.instancer(group)
        assert [desc(x) for x in group.seen] == case["seen"]
        return
    b = cat.instancer(group)
    assert [desc(x) for x in group.seen] == case["seen"]
    assert len(group.seen) == case["seen_count"]
    assert int(b is not None) == case["defined"]
    if b is not None:
        assert isinstance(b, SBindings)
        assert desc_hash(b.get_bindings_ref()) == case["bindings"]
        assert sorted(perl_str(k) for k in b.slippage_positions()) == case["slippage_positions"]
        assert b.slippages_count() == case["slippages_count"]
        mode = b.get_metonymy_mode()
        assert (None if mode is None else mode.as_text()) == case["metonymy_mode"]
        pos = b.get_position()
        assert (None if pos is None else pos.position) == case["position"]


def test_create_memo_golden():
    (case,) = cases("create")
    c2 = Interlaced.create(2)
    assert (Interlaced.create(2) is c2) == bool(case["same_int"])
    assert (Interlaced.create("2") is c2) == bool(case["same_string"])
    assert (Interlaced.create(2.0) is c2) == bool(case["same_float"])
    assert (Interlaced.create("02") is c2) == bool(case["leading_zero_same"])
    assert Interlaced.create("02").get_name() == case["leading_zero_name"]
    assert Interlaced.create("02").get_parts_count() == case["leading_zero_count"]
    assert (Interlaced(parts_count=2) is c2) == bool(case["new_is_not_memo"])
    assert (Interlaced.deserialize("2") is c2) == bool(case["deserialize_same"])
    assert Interlaced.deserialize("7").get_name() == case["deserialize_new_name"]
    assert Interlaced.create(-1).get_name() == case["negative_name"]


CREATE_DIES_INPUTS = {"2.5": 2.5, "abc": "abc", "undef": None, "empty string": "",
                      "1e3": "1e3", " 3": " 3"}


@pytest.mark.parametrize("case", cases("create_dies"), ids=lambda c: c["label"])
def test_create_dies_golden(case):
    if case["label"] == "missing":
        assert case["new"] == 1
        with pytest.raises(Confess):
            Interlaced()
        return
    v = CREATE_DIES_INPUTS[case["label"]]
    assert case["create"] == case["new"] == case["create_again"] == 1
    with pytest.raises(Confess, match="Validation failed for 'Int'"):
        Interlaced.create(v)
    with pytest.raises(Confess):
        Interlaced(parts_count=v)
    with pytest.raises(Confess):
        Interlaced.create(v)


def test_memoize_golden():
    (case,) = cases("memoize")
    x = Interlaced(parts_count=5)
    assert x.get_name() == case["name_before"]
    x.set_parts_count(6)
    assert x.as_text() == case["as_text_after"]
    assert x.get_name() == case["name_after"]
    assert x.get_parts_count() == case["parts_count_after"]
    assert x.longer_description() == case["longer_after"]
    y = Interlaced(parts_count=5)
    y.set_parts_count(8)
    assert y.get_name() == case["unnamed_set_name"]
    assert y.as_text() == case["unnamed_set_as_text"]
    with pytest.raises(Confess):
        y.set_parts_count(1.5)
    assert case["set_bad_dies"] == 1
    assert y.get_parts_count() == case["count_after_bad_set"]


# --- from reading the source ----------------------------------------------------------------

def test_mountain_singleton_and_mro():
    assert isinstance(S.MOUNTAIN, Mountain)
    assert S.MOUNTAIN.string_to_recreate() == "SCategory::Mountain->new()"


def test_interlaced_registered_and_non_ad_hoc_check():
    cat = Interlaced.create(4)
    assert categorizable._registered(cat) is cat

    class Obj(categorizable.Categorizable):
        def add_history(self, *args):
            pass

    obj = Obj()
    obj.add_category(cat, SBindings.create({}, {}))
    assert obj.has_non_ad_hoc_category() == 0
    obj.add_category(S.MOUNTAIN, SBindings.create({}, {}))
    assert obj.has_non_ad_hoc_category() == 1


def test_interlaced_create_registers_once():
    a = Interlaced.create(11)
    assert Interlaced.create(11) is a
    assert interlaced._MEMO["11"] is a


def test_mountain_build_items_are_numeric_ranges():
    ret = S.MOUNTAIN.build({"foot": 2, "peak": 4})
    assert ret.items == [2, 3, 4, 3, 2]

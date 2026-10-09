"""Tests for the sequence categories Sameness, Ascending and Descending.

Mirrors lib/SCategory/Sameness.pm, lib/SCategory/Ascending.pm and
lib/SCategory/Descending.pm (plus SCategory/MetonymySpec/Metonyable.pm, which
Sameness consumes). Golden data: oracle/sequence_categories.pl.

As in the oracle, Seqsee::Object->create, Seqsee::Anchored->create and
SMetonym->new are replaced by recorders; subobjects are fake Seqsee::Elements.
"""
import pytest

import golden
from seqsee import s as S
from seqsee import smetonym
from seqsee.categories import base, sameness
from seqsee.categories.ascending import Ascending
from seqsee.categories.base import SCategory
from seqsee.categories.descending import Descending
from seqsee.categories.metonymy_spec import Metonyable, NotMetonyable
from seqsee.categories.sameness import Sameness
from seqsee.constants import RELN_SCHEME
from seqsee.errors import Confess
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.sbindings import SBindings
from seqsee.sint import SInt
from seqsee.util import perl_range, perl_ref, perl_str

CASES = golden.load("sequence_categories")


def cases(kind, cat=None):
    return [c for c in CASES if c["kind"] == kind and (cat is None or c["cat"] == cat)]


def cat_of(key):
    return {"sameness": S.SAMENESS, "ascending": S.ASCENDING, "descending": S.DESCENDING}[key]


def show(v):
    return None if v is None else perl_str(v)


# --- fakes (same names as in the oracle) ---------------------------------------------

class FakeBuilt:
    def __init__(self, *items):
        self.items = list(items)
        self.cats = []
        self.reln = None

    def add_category(self, cat, b):
        self.cats.append((cat, b))

    def set_reln_scheme(self, scheme):
        self.reln = scheme


class FakeVal:
    def __init__(self, n):
        self.n = n


class FakeType:
    def __init__(self, n):
        self.n = n

    def get_name(self):
        return self.n


class FakeMeto:
    def __init__(self, t):
        self.t = t

    def get_type(self):
        return self.t


class FakeResult:
    def __init__(self, parts=None, entire=None):
        self.parts = parts
        self.entire = entire

    def get_parts_blemished(self):
        return self.parts

    def get_entire_blemish(self):
        return self.entire


class FakeGroup:
    def __init__(self, items, result, parts_count=None):
        self.items = items
        self.result = result
        self.parts_count = parts_count
        self.seen = []

    def get_items(self):
        return self.items

    def get_parts_count(self):
        return len(self.items) if self.parts_count is None else self.parts_count

    def can_be_seen_as(self, built):
        self.seen.append(built)
        return self.result


class FakeElemGroup(FakeGroup, Element):
    pass


class FakeElement(Element):
    """A Seqsee::Element (ref() is 'Seqsee::Element') with no metonym."""

    def __init__(self, mag):
        self.mag = mag

    def get_mag(self):
        return self.mag

    def get_effective_object(self):
        return self


def E(mag):
    return FakeElement(mag)


class FakeSub:
    def __init__(self, eo):
        self.eo = eo

    def get_effective_object(self):
        return self.eo


class FakeElemSub(Element):
    """isa Seqsee::Element, but ref() is 'FakeElemSub'."""

    perl_name = "FakeElemSub"

    def __init__(self):
        pass

    def get_effective_object(self):
        return self

    def get_mag(self):
        return 9


class FakeCatObj:
    def __init__(self, b):
        self.b = b

    def get_binding_for_category(self, cat):
        return self.b


class FakeAnchObj(FakeCatObj, Anchored):
    def get_edges(self):
        return (2, 5)


class FakeAnchored:
    def __init__(self, o):
        self.o = o
        self.edges = None

    def set_edges(self, *edges):
        self.edges = list(edges)


class FakeSMetonym:
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)
        self.keys = sorted(kwargs)

    def get_starred(self):
        return self.starred


def desc(v):
    """The oracle's desc()."""
    if v is None:
        return None
    if isinstance(v, (str, int, float, SInt)):
        return perl_str(v)
    if isinstance(v, FakeVal):
        return f"V({v.n})"
    if isinstance(v, FakeElement):
        return f"E({perl_str(v.get_mag())})"
    if isinstance(v, FakeBuilt):
        return "Built(" + ",".join(desc(x) or "" for x in v.items) + ")"
    if isinstance(v, FakeAnchored):
        return f"Anchored({desc(v.o)})"
    return f"OBJ:{type(v).__name__}"


def desc_hash(h):
    return {k: desc(v) for k, v in h.items()}


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    monkeypatch.setattr(base, "_object_create", lambda *items: FakeBuilt(*items))
    monkeypatch.setattr(sameness, "_anchored_create", FakeAnchored)
    monkeypatch.setattr(smetonym, "SMetonym", FakeSMetonym)


# --- inputs, by oracle label ----------------------------------------------------------

SAMENESS_BUILDS = {
    "plain": lambda: {"each": 7, "length": 3},
    "sint length": lambda: {"each": 7, "length": SInt(2)},
    "sint each": lambda: {"each": SInt(4), "length": 1},
    "object each": lambda: {"each": FakeVal(5), "length": 2},
    "length 0": lambda: {"each": 7, "length": 0},
    "length -1": lambda: {"each": 7, "length": -1},
    "sint length 0": lambda: {"each": 7, "length": SInt(0)},
    "length 2.5": lambda: {"each": 7, "length": 2.5},
    "length 0.5": lambda: {"each": 7, "length": 0.5},
    'length "3"': lambda: {"each": 7, "length": "3"},
    'length "abc"': lambda: {"each": 7, "length": "abc"},
    "each undef": lambda: {"each": None, "length": 2},
    "extra key": lambda: {"each": "x", "length": 2, "extra": 1},
    "length undef": lambda: {"each": 7, "length": None},
    "missing length": lambda: {"each": 7},
    "missing each": lambda: {"length": 3},
    "empty": lambda: {},
}

SEQ_BUILDS = {
    "start end": lambda: {"start": 1, "end": 4},
    "start end reversed": lambda: {"start": 4, "end": 1},
    "start end equal": lambda: {"start": 3, "end": 3},
    "start length": lambda: {"start": 1, "length": 3},
    "end length": lambda: {"end": 5, "length": 3},
    "sint start end": lambda: {"start": SInt(2), "end": SInt(5)},
    "sint start, length": lambda: {"start": SInt(2), "length": 3},
    "sint end, sint length": lambda: {"end": SInt(6), "length": SInt(2)},
    "plain end, sint length": lambda: {"end": 6, "length": SInt(2)},
    "all three, length kept": lambda: {"start": 2, "end": 5, "length": 99},
    "all three, length 0": lambda: {"start": 2, "end": 5, "length": 0},
    "start 0, length": lambda: {"start": 0, "length": 3},
    "end 0, length": lambda: {"end": 0, "length": 3},
    "negative": lambda: {"start": -2, "end": 1},
    "floats": lambda: {"start": 1.5, "end": 3.7},
    "strings": lambda: {"start": "2", "length": "3"},
    "length 0 only": lambda: {"start": 4, "length": 0},
    "extra key": lambda: {"start": 1, "end": 3, "extra": "x"},
    "start only": lambda: {"start": 1},
    "length and x": lambda: {"length": 3, "x": 1},
    "empty": lambda: {},
}

M1 = FakeMeto(FakeType("t1"))
M2 = FakeMeto(FakeType("t2"))


def ok():
    return FakeResult(parts={}, entire=None)


SAMENESS_INSTANCER = {
    "plain": lambda: FakeGroup([E(3), E(3), E(3)], ok()),
    "not seen": lambda: FakeGroup([E(3), E(3)], None),
    "seen returns 0": lambda: FakeGroup([E(3), E(3)], 0),
    "parts blemished": lambda: FakeGroup([E(3), E(4)], FakeResult(parts={1: M1})),
    "parts undef": lambda: FakeGroup([E(3)], FakeResult(parts=None)),
    "two blemishes": lambda: FakeGroup([E(3), E(4), E(4)], FakeResult(parts={1: M1, 2: M2})),
    "element group, entire blemish":
        lambda: FakeElemGroup([E(2)], FakeResult(parts={3: M1}, entire=M2)),
    "element group, no entire": lambda: FakeElemGroup([E(2)], FakeResult(parts={2: M1})),
    "non-element group, entire blemish":
        lambda: FakeGroup([E(2), E(2)], FakeResult(parts={}, entire=M2)),
    "empty group": lambda: FakeGroup([], ok()),
    "parts_count differs": lambda: FakeGroup([E(5), E(5)], ok(), parts_count=4),
    "first item undef": lambda: FakeGroup([None, E(1)], ok()),
}

SEQ_INSTANCER = {
    "ascending items": lambda: FakeGroup([E(1), E(2), E(3)], ok()),
    "descending items": lambda: FakeGroup([E(3), E(2), E(1)], ok()),
    "single item": lambda: FakeGroup([E(4)], ok()),
    "zero start": lambda: FakeGroup([E(0), E(2)], ok()),
    "not seen": lambda: FakeGroup([E(1), E(3)], None),
    "first item undef": lambda: FakeGroup([None, E(3)], ok()),
    "last item undef": lambda: FakeGroup([E(1), None], ok()),
    "empty": lambda: FakeGroup([], ok()),
    "metonym subobject": lambda: FakeGroup([FakeSub(E(5)), E(7)], ok()),
    "effective object not an element": lambda: FakeGroup([FakeSub(FakeVal(1)), E(7)], ok()),
    "element subclass": lambda: FakeGroup([E(1), FakeElemSub()], ok()),
    "parts blemished": lambda: FakeGroup([E(1), E(3)], FakeResult(parts={1: M1})),
    "element group, entire blemish":
        lambda: FakeElemGroup([E(2)], FakeResult(parts={}, entire=M1)),
    "non-element group, entire blemish":
        lambda: FakeGroup([E(2), E(4)], FakeResult(parts={0: M2}, entire=M1)),
}


# --- golden -------------------------------------------------------------------------------

def test_golden_covers_every_kind():
    kinds = {c["kind"] for c in CASES}
    assert kinds == {"basics", "sufficient", "build", "instancer", "meto_lookup", "finder",
                     "finder_dies", "unfinder"}
    assert {c["label"] for c in cases("build", "sameness")} == set(SAMENESS_BUILDS)
    assert {c["label"] for c in cases("build", "ascending")} == set(SEQ_BUILDS)
    assert {c["label"] for c in cases("instancer", "sameness")} == set(SAMENESS_INSTANCER)
    assert {c["label"] for c in cases("instancer", "descending")} == set(SEQ_INSTANCER)


@pytest.mark.parametrize("case", cases("basics"), ids=lambda c: c["cat"])
def test_basics_golden(case):
    cat = cat_of(case["cat"])
    assert isinstance(cat, SCategory)
    assert cat.perl_name == case["class"]
    assert cat.get_name() == case["get_name"]
    assert cat.as_text() == case["as_text"]
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


@pytest.mark.parametrize("case", cases("sufficient"), ids=lambda c: f"{c['cat']}-{c['atts']}")
def test_are_attributes_sufficient_to_build_golden(case):
    got = cat_of(case["cat"]).are_attributes_sufficient_to_build(*case["atts"])
    assert show(got) == case["result"]


@pytest.mark.parametrize("case", cases("build"), ids=lambda c: f"{c['cat']}-{c['label']}")
def test_build_golden(case):
    cat = cat_of(case["cat"])
    table = SAMENESS_BUILDS if case["cat"] == "sameness" else SEQ_BUILDS
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
    table = SAMENESS_INSTANCER if case["cat"] == "sameness" else SEQ_INSTANCER
    group = table[case["label"]]()
    assert not case["died"]
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


@pytest.mark.parametrize("case", cases("meto_lookup"), ids=lambda c: c["cat"])
def test_meto_lookup_golden(case):
    cat = cat_of(case["cat"])
    assert int(cat.get_meto_finder("each") is not None) == case["finder_each"]
    assert int(cat.get_meto_finder("x") is not None) == case["finder_x"]
    assert int(cat.get_meto_unfinder("each") is not None) == case["unfinder_each"]
    assert int(cat.get_meto_unfinder("x") is not None) == case["unfinder_x"]


def test_meto_lookup_only_for_sameness():
    assert [c["cat"] for c in cases("meto_lookup")] == ["sameness"]
    assert not hasattr(S.ASCENDING, "get_meto_finder")
    assert not hasattr(S.DESCENDING, "get_meto_finder")


def finder_case(label):
    (case,) = [c for c in cases("finder") if c["label"] == label]
    return case


def test_finder_direct_golden():
    case = finder_case("direct")
    cat = S.SAMENESS
    bindings = SBindings.create({}, {"each": 7, "length": SInt(3)})
    obj = FakeCatObj(bindings)
    m = cat.get_meto_finder("each")(obj, cat, "each", bindings)
    assert type(m).__name__ == case["ref"]
    assert (m.category is cat) == bool(case["category_is_self"])
    assert m.name == case["name"]
    assert desc(m.starred) == case["starred"]
    assert (m.unstarred is obj) == bool(case["unstarred_is_obj"])
    assert desc_hash(m.info_loss) == case["info_loss"]
    assert m.keys == case["keys"]


def test_find_metonym_golden():
    case = finder_case("find_metonym")
    obj = FakeCatObj(SBindings.create({}, {"each": 7, "length": SInt(3)}))
    m = S.SAMENESS.find_metonym(obj, "each")
    assert desc(m.starred) == case["starred"]
    assert m.starred.edges == case["edges"]
    assert desc_hash(m.info_loss) == case["info_loss"]


def test_find_metonym_anchored_golden():
    case = finder_case("find_metonym anchored")
    obj = FakeAnchObj(SBindings.create({}, {"each": SInt(4), "length": 2}))
    m = S.SAMENESS.find_metonym(obj, "each")
    assert desc(m.starred) == case["starred"]
    assert m.starred.edges == case["edges"]
    assert desc_hash(m.info_loss) == case["info_loss"]


def test_find_metonym_dies_golden():
    assert {c["label"]: c["dies"] for c in cases("finder_dies")} == {
        "unknown name": 1, "no binding": 1}
    obj = FakeCatObj(SBindings.create({}, {"each": 7, "length": 3}))
    with pytest.raises(Confess, match="No 'x' meto_finder installed for category SCategory::Sameness"):
        S.SAMENESS.find_metonym(obj, "x")
    with pytest.raises(Confess, match="Object must belong to category"):
        S.SAMENESS.find_metonym(FakeCatObj(None), "each")


UNFINDER_INPUTS = {
    "length": lambda: {"length": 3},
    "length sint": lambda: {"length": SInt(2)},
    "each overridden": lambda: {"length": 2, "each": 8},
    "length 0": lambda: {"length": 0},
    "empty": lambda: {},
    "other key": lambda: {"x": 1},
    "two other keys": lambda: {"x": 1, "y": 2},
}


@pytest.mark.parametrize("case", cases("unfinder"), ids=lambda c: c["label"])
def test_unfinder_golden(case):
    cat = S.SAMENESS
    unfinder = cat.get_meto_unfinder("each")
    info = UNFINDER_INPUTS[case["label"]]()
    if case["died"]:
        with pytest.raises(Confess) as exc:
            unfinder(cat, "each", info, FakeVal(1))
        assert str(exc.value) == case["error"]
    else:
        assert desc(unfinder(cat, "each", info, FakeVal(1))) == case["result"]


# --- from reading the source -------------------------------------------------------------

def test_classes_and_roles():
    assert isinstance(S.SAMENESS, Sameness) and isinstance(S.SAMENESS, Metonyable)
    assert isinstance(S.ASCENDING, Ascending) and isinstance(S.ASCENDING, NotMetonyable)
    assert isinstance(S.DESCENDING, Descending) and isinstance(S.DESCENDING, NotMetonyable)
    assert S.SAMENESS.is_metonyable() == 1


def test_metonym_finders_are_per_instance():
    a, b = Sameness(), Sameness()
    assert a.metonym_finders is not b.metonym_finders
    assert a.metonym_finders == b.metonym_finders
    assert a.metonymy_unfinder is not b.metonymy_unfinder


def test_build_bindings_record_args_and_object():
    args = {"start": 2, "end": 4}
    ret = S.ASCENDING.build(args)
    _, b = ret.cats[0]
    assert b.get_bindings_ref() is args
    assert args == {"start": 2, "end": 4, "length": 3}


def test_descending_build_keeps_sint_arithmetic():
    args = {"start": SInt(9), "length": 3}
    ret = S.DESCENDING.build(args)
    assert isinstance(args["end"], SInt) and args["end"].get_mag() == 7
    assert ret.items == [9, 8, 7]


def test_sameness_instancer_guess_uses_first_item():
    first, second = E(3), E(3)
    group = FakeGroup([first, second], ok())
    b = S.SAMENESS.instancer(group)
    assert b.get_bindings_ref()["each"] is first
    assert group.seen[0].items == [first, first]


def test_guesser_returns_sint():
    group = FakeGroup([E(2), E(5)], ok())
    b = S.ASCENDING.instancer(group)
    start = b.get_bindings_ref()["start"]
    assert isinstance(start, SInt) and start.get_mag() == 2


def test_perl_range():
    assert perl_range(1, 3) == [1, 2, 3]
    assert perl_range(1.5, 3.7) == [1, 2, 3]
    assert perl_range(-1.5, 1) == [-1, 0, 1]
    assert perl_range(None, 2) == [0, 1, 2]
    assert perl_range("2", "4") == [2, 3, 4]
    assert perl_range(3, 1) == []
    assert perl_range(-2.7, -1) == [-2, -1]
    with pytest.raises(Confess):
        perl_range("a", "c")


def test_perl_ref():
    assert perl_ref(3) == "" and perl_ref("x") == "" and perl_ref(None) == ""
    assert perl_ref([]) == "ARRAY" and perl_ref({}) == "HASH"
    assert perl_ref(SInt(1)) == "SInt"
    assert perl_ref(E(1)) == "Seqsee::Element"
    assert perl_ref(FakeElemSub()) == "FakeElemSub"


def test_hooks_are_wired(monkeypatch):
    monkeypatch.undo()
    # Seqsee::Object->create (item 021) with real Seqsee::Element leaves (item 023).
    group = base._object_create(1, 2)
    assert group.get_structure() == [1, 2]
    assert [perl_ref(x) for x in group] == ["Seqsee::Element"] * 2
    # Seqsee::Anchored->create of one item returns it.
    from seqsee.objects.element import Element
    e = Element.create(4, 0)
    assert sameness._anchored_create(e) is e

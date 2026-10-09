"""Tests for the mapping-based category and the metonymy-spec roles.

Mirrors lib/SCategory/MappingBased.pm, lib/SCategory/MetonymySpec.pm and
lib/SCategory/MetonymySpec/{Metonyable,NotMetonyable}.pm. Golden data:
oracle/mapping_based.pl.

As in the oracle, transforms are FakeMaps (subclasses of Mapping), and
ApplyMapping, Seqsee::Object->create and SLTM::encode/decode are replaced by
recorders.
"""
import pytest

import golden
from seqsee import util
from seqsee.categories import base, mapping_based
from seqsee.categories.base import SCategory
from seqsee.categories.mapping_based import MappingBased
from seqsee.categories.metonymy_spec import Metonyable, MetonymySpec, NotMetonyable
from seqsee.constants import RELN_SCHEME
from seqsee.errors import Confess
from seqsee.mapping import Mapping
from seqsee.sbindings import SBindings
from seqsee.sint import SInt
from seqsee.srelation import SRelation
from seqsee.util import perl_str

CASES = golden.load("mapping_based")


def cases(kind):
    return [c for c in CASES if c["kind"] == kind]


def case(kind, label=None):
    (c,) = [c for c in CASES if c["kind"] == kind and c.get("label") == label]
    return c


def show(v):
    return None if v is None else perl_str(v)


# --- fakes (same names as in the oracle) ---------------------------------------------

class FakeMap(Mapping):
    def __init__(self, name, cat=None):
        self.name = name
        self.cat = cat

    def as_text(self):
        return f"M<{self.name}>"

    def get_name(self):
        return self.name

    def get_category(self):
        return self.cat


class FakeRel(SRelation):
    def __init__(self, type_):
        self.type = type_

    def get_type(self):
        return self.type


class NotAMapping:
    pass


class FakeCat:
    def __init__(self, ret):
        self.ret = ret
        self.asked = 0

    def is_instance(self, obj):
        self.asked += 1
        return self.ret


class FakeVal:
    def __init__(self, n):
        self.n = n

    def get_structure(self):
        return self.n


LOG = []


class FakePart(FakeVal):
    def __init__(self, n, any_=False):
        super().__init__(n)
        self.any = any_

    def get_effective_object(self):
        LOG.append(f"eff{self.n}")
        return self

    def can_be_seen_as(self, structure):
        LOG.append(f"see{self.n}?" + ("undef" if structure is None else str(structure)))
        if self.any:
            return 1
        return 1 if structure is not None and structure == self.n else 0


class FakeGroup:
    def __init__(self, *parts):
        self.parts = list(parts)
        self.slippages = {}

    def get_items_array(self):
        return list(self.parts)

    def get_effective_slippages(self):
        return self.slippages


class FakeBuilt:
    def __init__(self, *items):
        self.items = list(items)
        self.cats = []
        self.scheme = None

    def get_parts_ref(self):
        return self.items

    def add_category(self, cat, bindings):
        self.cats.append((cat, bindings))
        return 1

    def set_reln_scheme(self, scheme):
        self.scheme = scheme


APPLY = []
ENCODED = []
DECODE = {}


def fake_apply_mapping(t, o):
    name = t.get_name()
    is_ref = isinstance(o, FakeVal)
    APPLY.append(f"{name}(" + (f"V{o.n}" if is_ref else ("undef" if o is None else perl_str(o))) + ")")
    n = o.n if is_ref else util.perl_num(o)
    r = {"succ": n + 1, "pred": n - 1, "same": n}.get(name)
    if r is None or r > 5:
        return None
    return FakeVal(r) if is_ref else r


@pytest.fixture(autouse=True)
def fakes(monkeypatch):
    APPLY.clear()
    ENCODED.clear()
    DECODE.clear()
    LOG.clear()
    monkeypatch.setattr(base, "_apply_mapping", fake_apply_mapping)
    monkeypatch.setattr(base, "_object_create", lambda *items: FakeBuilt(*items))

    def encode(*objs):
        ENCODED.append([o.as_text() for o in objs])
        return "ENC"
    monkeypatch.setattr(mapping_based, "_sltm_encode", encode)
    monkeypatch.setattr(mapping_based, "_sltm_decode", lambda s: [DECODE.get(s)])


CAT_RET = {
    "bindings": SBindings(raw_slippages={}, bindings={}),
    "undef": None, "zero": 0, "empty": "",
}
CATS = {k: FakeCat(v) for k, v in CAT_RET.items()}


def T(name, catkey="undef"):
    return FakeMap(name, CATS[catkey])


def desc(v):
    if v is None:
        return None
    if isinstance(v, FakePart):
        return f"P{v.n}"
    if isinstance(v, FakeVal):
        return f"V{v.n}"
    if isinstance(v, FakeGroup):
        return "G(" + ",".join(desc(p) for p in v.parts) + ")"
    if isinstance(v, FakeBuilt):
        return "Built(" + ",".join(desc(x) or "undef" for x in v.items) + ")"
    if isinstance(v, list):
        return "[" + ",".join(desc(x) or "undef" for x in v) + "]"
    return perl_str(v)


# --- basics, Create, constructor checks -----------------------------------------------

def test_basics():
    c_ = case("basics")
    t = T("succ")
    c = MappingBased.create(t)
    assert c.perl_name == c_["class"]
    assert c.get_name() == c_["name"]
    assert c.as_text() == c_["as_text"]
    assert (c.get_transform() is t) == bool(c_["transform_is_t"])
    assert c.is_pure() == c_["is_pure"]
    assert (c.get_pure() is c) == bool(c_["get_pure_is_self"])
    assert c.get_memory_dependencies() == [t]
    assert c.get_meto_types() == c_["meto_types"]
    assert show(c.is_metonyable()) == c_["is_metonyable"]
    assert isinstance(c, MetonymySpec) == bool(c_["does_metonymy_spec"])
    assert isinstance(c, NotMetonyable) == bool(c_["does_not_metonyable"])
    assert isinstance(c, Metonyable) == bool(c_["does_metonyable"])
    assert bool(c.is_numeric()) == bool(c_["is_numeric"])
    assert isinstance(c, SCategory)


def test_create_memo():
    c_ = case("create")
    t = T("succ")
    c = MappingBased.create(t)
    assert (MappingBased.create(t) is c) == bool(c_["memo_same"])
    assert (MappingBased.create(T("succ")) is not c) == bool(c_["other_transform_new"])
    assert (MappingBased.create(FakeRel(t)) is c) == bool(c_["relation_uses_type"])
    fresh = MappingBased(transform=t)
    assert (fresh is not c) == bool(c_["new_is_fresh"])
    assert fresh.get_name() == c_["new_name"]


def test_memoized_name():
    c_ = case("memo_name")
    m = MappingBased(transform=T("same"))
    assert m.get_name() == c_["before"]
    m.set_transform(T("pred"))
    assert m.get_name() == c_["after"]
    assert m.as_text() == c_["as_text_after"]
    assert m.get_transform().as_text() == c_["transform_after"]


def test_memoized_name_unset():
    m = MappingBased(transform=T("same"))
    m.set_transform(T("pred"))
    assert m.get_name() == case("memo_name_unset")["name"]


ERRORS = {
    "create_undef": lambda: MappingBased.create(None),
    "create_string": lambda: MappingBased.create("abc"),
    "create_not_mapping": lambda: MappingBased.create(NotAMapping()),
    "create_rel_of_undef": lambda: MappingBased.create(FakeRel(None)),
    "new_missing": lambda: MappingBased(),
    "new_undef": lambda: MappingBased(transform=None),
    "set_not_mapping": lambda: MappingBased(transform=T("succ")).set_transform(NotAMapping()),
    "set_undef": lambda: MappingBased(transform=T("succ")).set_transform(None),
}


@pytest.mark.parametrize("c", cases("error"), ids=lambda c: c["label"])
def test_errors(c):
    assert c["dies"] == 1
    with pytest.raises(Confess):
        ERRORS[c["label"]]()


def test_serialize_deserialize():
    c_ = case("serialize")
    t = T("succ")
    c = MappingBased.create(t)
    assert c.serialize() == c_["serialized"]
    assert ENCODED == c_["encoded"]
    t2 = T("pred")
    DECODE["X"] = t2
    d = MappingBased.deserialize("X")
    assert (d.get_transform() is t2) == bool(c_["deserialized_transform_is_t2"])
    assert (MappingBased.deserialize("X") is d) == bool(c_["deserialize_memo"])
    DECODE["Y"] = t
    assert (MappingBased.deserialize("Y") is c) == bool(c_["deserialize_existing"])


# --- AreAttributesSufficientToBuild --------------------------------------------------

def _att(tag):
    if tag == "u":
        return None
    kind, value = tag.split(":", 1)
    return value if kind == "s" else float(value) if "." in value else int(value)


@pytest.mark.parametrize("c", cases("sufficient"), ids=lambda c: "|".join(c["atts"]) or "empty")
def test_sufficient(c):
    cat = MappingBased(transform=T("succ"))
    atts = [_att(a) for a in c["atts"]]
    assert show(cat.are_attributes_sufficient_to_build(*atts)) == c["result"]


# --- Instancer ------------------------------------------------------------------------

def _part(n):
    if isinstance(n, str) and n.startswith("any:"):
        return FakePart(int(n[4:]), any_=True)
    return FakePart(n)


@pytest.mark.parametrize("c", cases("instancer"), ids=lambda c: c["label"])
def test_instancer(c):
    cat_obj = CATS[c["cat"]]
    cat = MappingBased(transform=FakeMap(c["transform"], cat_obj))
    group = FakeGroup(*(_part(n) for n in c["parts"]))
    asked_before = cat_obj.asked
    res = cat.instancer(group)
    assert APPLY == c["apply"]
    assert LOG == c["log"]
    assert cat_obj.asked - asked_before == c["cat_asked"]
    if c["result_ref"] == "SBindings":
        assert isinstance(res, SBindings)
        b = res.get_bindings_ref()
        assert {k: desc(v) for k, v in b.items()} == c["bindings"]
        assert (b["first"] is group) == bool(c["first_is_group"])
        assert (res.get_squinting_raw() is group.slippages) == bool(c["slippages_shared"])
    else:
        assert not isinstance(res, SBindings)
        assert show(res) == c["result"]
        assert (res is not None) == bool(c["result_defined"])


def test_instancer_via_is_instance_adds_category():
    """SCategory's is_instance on a successful Instancer adds the category."""
    added = []

    class Group(FakeGroup):
        def add_category(self, cat, bindings):
            added.append((cat, bindings))

    cat = MappingBased(transform=T("succ"))
    group = Group(FakePart(1), FakePart(2))
    b = cat.is_instance(group)
    assert added == [(cat, b)]


# --- build ------------------------------------------------------------------------------

def _arg(v):
    if v.startswith("V"):
        return FakeVal(int(v[1:]))
    if v.startswith("S"):
        return SInt(int(v[1:]))
    if v.startswith("A"):
        return [int(v[1:]), "x"]
    for conv in (int, float):
        try:
            return conv(v)
        except ValueError:
            pass
    return v


# The oracle builds these from Perl literals; numeric-looking ones were numbers
# except where the label says "str".
_STRING_ARGS = {("v1_len_str", "length"), ("v1_len_abc", "length"),
                ("n_str_first", "first"), ("v4_same", "extra")}


@pytest.mark.parametrize("c", cases("build"), ids=lambda c: c["label"])
def test_build(c):
    opts = {k: (v if (c["label"], k) in _STRING_ARGS else _arg(v)) for k, v in c["spec"].items()}
    cat = MappingBased(transform=T(c["transform"]))
    ret = cat.build(opts)
    assert c["died"] == 0
    assert APPLY == c["apply"]
    if c["ret_ref"] == "FakeBuilt":
        assert isinstance(ret, FakeBuilt)
        assert desc(ret) == c["items"]
        assert len(ret.cats) == c["cats_added"]
        added_cat, b = ret.cats[0]
        assert (added_cat is cat) == bool(c["cat_is_self"])
        bh = b.get_bindings_ref()
        assert {k: desc(v) for k, v in bh.items()} == c["bindings"]
        assert (bh["length"] is opts["length"]) == bool(c["length_is_arg"])
        assert (bh["first"] is ret.items[0]) == bool(c["first_is_item0"])
        assert len(b.get_squinting_raw()) == c["slippages"]
        assert (ret.scheme is RELN_SCHEME.CHAIN) == bool(c["scheme_is_chain"])
    else:
        assert show(ret) == c["ret"]


# --- MetonymySpec role ------------------------------------------------------------------

def test_metonymy_spec_requires_get_meto_types():
    class NoMeto(MetonymySpec):
        pass

    with pytest.raises(TypeError):
        NoMeto()

    class WithMeto(MetonymySpec):
        def get_meto_types(self):
            return ["x"]

    assert WithMeto().get_meto_types() == ["x"]


def test_scategory_does_metonymy_spec():
    """Perl: SCategory does SCategory::MetonymySpec; the two spec roles don't consume it."""
    assert issubclass(SCategory, MetonymySpec)
    assert not issubclass(NotMetonyable, MetonymySpec)
    assert not issubclass(Metonyable, MetonymySpec)

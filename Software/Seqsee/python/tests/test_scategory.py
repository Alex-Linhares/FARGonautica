"""Tests for the category base: Perl Categorizable.pm and SCategory.pm.

Golden data comes from oracle/scategory.pl (tests/golden/scategory.json). The
oracle fakes everything SCategory talks to (objects, bindings, relations, and
the values inside bindings, with extra FindMapping/ApplyMapping multimethod
variants), so the fakes below mirror the Perl ones one for one.
"""
import re

import pytest

import golden
from seqsee import categorizable, util
from seqsee.categories import base
from seqsee.categories.base import (
    SCategory, calculate_bindings_change, calculate_bindings_change_no_slips,
    calculate_bindings_change_with_slips)
from seqsee.categories.numeric import Numeric
from seqsee.categorizable import Categorizable, get_common_categories, register_category
from seqsee.constants import METO_MODE
from seqsee.errors import Confess
from seqsee.objects.object import SeqseeObject

CASES = golden.load("scategory")


def case(op, label=None):
    found = [c for c in CASES if c["op"] == op and (label is None or c.get("label") == label)]
    assert len(found) == 1, (op, label)
    return found[0]


# --- fakes (mirroring oracle/scategory.pl) ----------------------------------------------------

class FakeVal:
    def __init__(self, n):
        self.n = n


class FakeRel:
    def __init__(self, d):
        self.d = d


def fake_find_mapping(a, b):
    """FindMapping(FakeVal, FakeVal): a FakeRel when |diff| <= 2. Other types have no variant."""
    if not (isinstance(a, FakeVal) and isinstance(b, FakeVal)):
        raise Confess("No viable candidate for call to multimethod FindMapping()")
    d = b.n - a.n
    return FakeRel(d) if abs(d) <= 2 else None


def fake_apply_mapping(r, v):
    if not (isinstance(r, FakeRel) and isinstance(v, FakeVal)):
        raise Confess("No viable candidate for call to multimethod ApplyMapping()")
    n = v.n + r.d
    return None if n > 20 else FakeVal(n)


DIR_SAME = object()


@pytest.fixture
def fakes(monkeypatch):
    """Route SCategory's FindMapping/ApplyMapping/Mapping::Structural/Mapping::Dir hooks to fakes."""
    recorded = {}

    def structural_create(opts):
        recorded["opts"] = opts
        return "STRUCTURAL"

    monkeypatch.setattr(base, "_find_mapping", fake_find_mapping)
    monkeypatch.setattr(base, "_apply_mapping", fake_apply_mapping)
    monkeypatch.setattr(base, "_structural_create", structural_create)
    monkeypatch.setattr(base, "_mapping_dir_same", lambda: DIR_SAME)
    return recorded


class FakeObj(Categorizable):
    def __init__(self, descr=None):
        self.hist = []
        self.descr = descr or {}
        self.calls = []

    def add_history(self, msg):
        self.hist.append(msg)

    def describe_as(self, cat):
        name = cat.get_name() if cat is not None else "undef"
        self.calls.append(f"describe_as {name}")
        return self.descr.get(name)


class FakeSObj(FakeObj, SeqseeObject):
    def __init__(self, id=None, effective=None, built=None, blemish=None, descr=None):
        FakeObj.__init__(self, descr)
        self.id = id
        self.effective = effective
        self.built = built
        self.blemish = blemish
        self.group_p = None

    def get_effective_object(self):
        return self.effective or self

    def apply_blemish_everywhere(self, t):
        return FakeSObj(built=self.built, blemish=f"everywhere {t.n}")

    def apply_blemish_at(self, t, p):
        if p.n > 3:
            raise RuntimeError("bad position")
        return FakeSObj(built=self.built, blemish=f"at {p.n} {t.n}")

    def set_group_p(self, v):
        self.group_p = v


class FakeCat(SCategory):
    perl_name = "FakeCat"

    def __init__(self, name, inst=None, sufficient=1, build_fails=0):
        self.name = name
        self.inst = inst or {}
        self.sufficient = sufficient
        self.build_fails = build_fails
        self.suff_calls = []
        super().__init__()

    def instancer(self, o):
        return self.inst.get(o.id)

    def build(self, b):
        if self.build_fails:
            return None
        return FakeSObj(built={k: v.n for k, v in b.items()})

    def get_name(self):
        return self.name

    def as_text(self):
        return "fake " + self.name

    def are_attributes_sufficient_to_build(self, *atts):
        self.suff_calls.append(list(atts))
        return self.sufficient

    def get_meto_types(self):
        return []

    def get_pure(self):
        return self

    def get_memory_dependencies(self):
        return []

    def serialize(self):
        return self.name

    @classmethod
    def deserialize(cls, s):
        return None


class FakeInterlacedCat(FakeCat):
    perl_name = "FakeInterlacedCat"


class FakeNumCat(Numeric, FakeCat):
    perl_name = "FakeNumCat"


class FakeUnreg:
    perl_name = "FakeUnreg"

    def __init__(self, n):
        self.n = n

    def get_name(self):
        return self.n


class FakeB:
    def __init__(self, mode=None, b=None, pos=None, type=None):
        self.mode, self.b, self.pos, self.type = mode, b, pos, type

    def get_metonymy_mode(self):
        return self.mode

    def get_bindings_ref(self):
        return self.b

    def get_position(self):
        return self.pos

    def get_metonymy_type(self):
        return self.type


class FakeReln:
    def __init__(self, cat, changed, slips, meto_mode, meto_reln=None, pos_reln=None, dir_reln=None):
        self.cat, self.changed, self.slips, self.meto_mode = cat, changed, slips, meto_mode
        self.meto_reln, self.pos_reln, self.dir_reln = meto_reln, pos_reln, dir_reln

    def get_category(self):
        return self.cat

    def get_changed_bindings(self):
        return self.changed

    def get_slippages(self):
        return self.slips

    def get_meto_mode(self):
        return self.meto_mode

    def get_metonymy_reln(self):
        return self.meto_reln

    def get_position_reln(self):
        return self.pos_reln

    def get_direction_reln(self):
        return self.dir_reln


def prefix(s):
    return re.sub(r"=HASH\(0x[0-9a-f]+\)\Z", "", s)


def names(cats):
    return sorted(c.get_name() for c in cats)


def fv(spec):
    return {k: FakeVal(v) for k, v in spec.items()}


def mode(name):
    return None if name is None else getattr(METO_MODE, name)


def fake_b(spec):
    return FakeB(mode=mode(spec["mode"]), b=fv(spec["b"]),
                 pos=FakeVal(spec["pos"]) if spec.get("pos") is not None else None,
                 type=FakeVal(spec["type"]) if spec.get("type") is not None else None)


@pytest.fixture
def cats():
    return {"a": FakeCat("catA"), "b": FakeCat("catB"), "c": FakeCat("catC"),
            "i": FakeInterlacedCat("catI"), "unreg": FakeUnreg("unreg"), "num": FakeNumCat("num")}


# --- Categorizable ------------------------------------------------------------------------

def test_add_lookup_remove(cats):
    a, b, c = cats["a"], cats["b"], cats["c"]
    o = FakeObj()
    ret_a = o.add_category(a, "bA")
    ret_b = o.add_category(b, "bB")
    g = case("add_category")
    assert [ret_a] == g["ret_list"] and ret_b == g["ret_scalar"]
    assert o.hist == g["hist"]
    assert names(o.get_categories()) == g["cats"]
    assert sorted(prefix(s) for s in o.category_list_as_strings()) == g["strings"]
    assert sorted(prefix(s) for s in o.get_categories_as_string().split(", ")) == g["as_string_parts"]
    assert len(o.get_cats_hash()) == g["hash_size"]

    o.add_category(a, "bA2")
    g = case("add_again")
    assert o.hist == g["hist"]
    assert o.is_of_category_p(a) == g["binding"]
    assert len(o.get_categories()) == g["n"]

    g = case("lookup")
    assert o.get_binding_for_category(b) == g["present"]
    assert o.is_of_category_p(c) is None and g["absent_defined"] == 0

    r1 = o.remove_category(b)
    r2 = o.remove_category(c)
    g = case("remove_category")
    assert [r1] == g["ret_present"]
    assert r2 is None and g["ret_absent"] == ["undef"]
    assert o.hist == g["hist"]
    assert names(o.get_categories()) == g["cats"]

    g = case("add_one_arg")
    assert g["dies"] == 1
    with pytest.raises(Confess):
        o.add_category(c)
    assert o.hist == g["hist"]
    assert len(o.get_categories()) == g["n"]


def test_unregistered_category(cats):
    o = FakeObj()
    o.add_category(cats["unreg"], "bU")
    o.add_category(cats["a"], "bA")
    g = case("unregistered")
    got = o.get_categories()
    assert len(got) == g["n"]
    assert sum(1 for x in got if x is None) == g["undefs"]
    assert o.hist == g["hist"]
    assert sorted(prefix(s) for s in o.category_list_as_strings()) == g["strings"]


COMMON = {
    "none": [],
    "one object": [["a", "b"]],
    "two overlap": [["a", "b"], ["b", "c"]],
    "three all share": [["a", "b", "c"], ["c", "a"], ["a", "c", "i"]],
    "disjoint": [["a"], ["b"]],
    "empty object": [["a"], []],
    "common unregistered": [["unreg", "a"], ["unreg"]],
}


@pytest.mark.parametrize("label", list(COMMON))
def test_get_common_categories(cats, label):
    g = case("get_common_categories", label)
    objs = []
    for spec in COMMON[label]:
        o = FakeObj()
        for k in spec:
            o.add_category(cats[k], 1)
        objs.append(o)
    if g["dies"]:
        with pytest.raises(Confess, match="not a cat"):
            get_common_categories(*objs)
    else:
        assert names(get_common_categories(*objs)) == g["names"]


@pytest.mark.parametrize("label,arg", [("non-ref arg", 3), ("undef arg", None)])
def test_get_common_categories_funny_arg(cats, label, arg):
    assert case("get_common_categories", label)["dies"] == 1
    o = FakeObj()
    o.add_category(cats["a"], 1)
    args = (o, arg) if label == "non-ref arg" else (arg,)
    with pytest.raises(Confess, match="Funny arg"):
        get_common_categories(*args)


@pytest.mark.parametrize("label,keys", [
    ("none", []), ("interlaced only", ["i"]), ("plain only", ["a"]),
    ("interlaced and plain", ["i", "a"]), ("unregistered", ["unreg"])])
def test_has_non_ad_hoc_category(cats, label, keys):
    o = FakeObj()
    for k in keys:
        o.add_category(cats[k], 1)
    assert o.has_non_ad_hoc_category() == case("HasNonAdHocCategory", label)["result"]


@pytest.mark.parametrize("label,keys,descr", [
    ("no categories", [], {}),
    ("all describable", ["a", "b"], {"catA": "x", "catB": "y"}),
    ("one fails", ["a", "b"], {"catA": "x"}),
    ("false bindings", ["a"], {"catA": 0})])
def test_copy_categories_to(cats, label, keys, descr):
    g = case("CopyCategoriesTo", label)
    src = FakeObj()
    for k in keys:
        src.add_category(cats[k], 1)
    to = FakeObj(descr=descr)
    assert src.copy_categories_to(to) == g["result"]
    assert sorted(to.calls) == g["calls"]
    assert len(to.get_categories()) == g["to_cats"]


def test_register_category_directly():
    plain = FakeUnreg("manual")
    o = FakeObj()
    o.add_category(plain, 1)
    assert o.get_categories() == [None]
    register_category(plain)
    assert o.get_categories() == [plain]
    assert get_common_categories(o) == [plain]


def test_perl_ref_string():
    x = FakeUnreg("x")
    assert re.fullmatch(r"FakeUnreg=HASH\(0x[0-9a-f]+\)", util.perl_ref_string(x))
    assert util.perl_ref_string(x) == util.perl_ref_string(x)
    assert util.perl_ref_string(x) != util.perl_ref_string(FakeUnreg("x"))


# --- SCategory: construction, is_instance, comparisons --------------------------------------

def test_is_instance(cats):
    a, b, c = cats["a"], cats["b"], cats["c"]
    o = FakeSObj(id="o1")
    a.inst = {"o1": "BIND"}
    c.inst = {"o1": 0}
    g = case("is_instance")
    assert a.is_instance(o) == g["yes"]
    assert b.is_instance(o) is None and g["no_defined"] == 0
    assert c.is_instance(o) is None and g["false_defined"] == 0
    assert names(o.get_categories()) == g["cats"]
    assert o.hist == g["hist"]
    assert o.is_of_category_p(a) == g["binding"]


def test_registration_and_comparisons(cats):
    a, b, num = cats["a"], cats["b"], cats["num"]
    g = case("registration")
    o = FakeObj()
    o.add_category(FakeCat("fresh"), 1)
    assert sum(1 for x in o.get_categories() if x is not None) == g["registered"]
    assert int(a.is_numeric()) == g["is_numeric_fake"]
    assert int(num.is_numeric()) == g["is_numeric_num"]
    # ~~, eq and == are all identity
    assert int(a == a) == g["smartmatch_self"] == g["eq_self"] == g["numeq_self"]
    assert int(a == b) == g["smartmatch_other"] == g["eq_other"] == g["numeq_other"]
    assert int(b in [a, b]) == g["smartmatch_list"]


def test_required_methods_are_abstract():
    class Incomplete(SCategory):
        def get_name(self):
            return "x"

    with pytest.raises(TypeError):
        Incomplete()


# --- FindMappingForCat ---------------------------------------------------------------------

def rel_text(v):
    return v if isinstance(v, str) else f"rel {v.d}"


@pytest.mark.parametrize("g", [c for c in CASES if c["op"] == "FindMappingForCat" and "b1" in c],
                         ids=lambda c: c["label"])
def test_find_mapping_for_cat(fakes, g):
    cat = FakeCat("mapcat", sufficient=g["sufficient"])
    o1, o2 = FakeSObj(id="o1"), FakeSObj(id="o2")
    o1.get_cats_hash()[cat] = fake_b(g["b1"])
    o2.get_cats_hash()[cat] = fake_b(g["b2"])
    assert g["dies"] == 0
    r = cat.find_mapping_for_cat(o1, o2)
    assert r == g["result"]
    assert cat.suff_calls == g["suff_calls"]
    if "opts" not in g:
        assert "opts" not in fakes
        return
    want, opts = g["opts"], fakes["opts"]
    assert sorted(opts) == want["keys"]
    assert opts["category"] is cat
    assert opts["meto_mode"] is mode(want["meto_mode"])
    assert {k: v.d for k, v in opts["changed_bindings"].items()} == want["changed_bindings"]
    assert sorted(opts["unchanged_bindings"]) == want["unchanged_bindings"]
    assert opts["slippages"] == want["slippages"]
    for k in ("position_reln", "metonymy_reln"):
        assert (rel_text(opts[k]) if k in opts else "missing") == want[k]
    assert int(opts["direction_reln"] is DIR_SAME) == want["direction_is_same"]
    assert opts["first"].id == want["first"] and opts["second"].id == want["second"]


def test_find_mapping_for_cat_errors(fakes):
    cat = FakeCat("mapcat")
    o1, o2 = FakeSObj(id="o1"), FakeSObj(id="o2")
    for label, args in [("two args", (o1,)), ("four args", (o1, o2, o2)),
                        ("not Seqsee::Object", (o1, FakeObj()))]:
        assert case("FindMappingForCat", label)["dies"] == 1
        with pytest.raises(Confess):
            cat.find_mapping_for_cat(*args)

    g = case("FindMappingForCat", "not of category")
    neither = cat.find_mapping_for_cat(o1, o2)
    o1.get_cats_hash()[cat] = FakeB(mode=METO_MODE.NONE, b=fv({"a": 1}))
    second_missing = cat.find_mapping_for_cat(o1, o2)
    first_missing = cat.find_mapping_for_cat(o2, o1)
    assert [neither, second_missing, first_missing] == [None, None, None]
    assert g["neither"] == g["second_missing"] == g["first_missing"] == 0

    g = case("FindMappingForCat", "missing key in second")
    o3 = FakeSObj(id="o3")
    o3.get_cats_hash()[cat] = FakeB(mode=METO_MODE.NONE, b=fv({"b": 1}))
    msg = g["error"].split(" at /")[0]
    with pytest.raises(Confess) as exc:
        cat.find_mapping_for_cat(o1, o3)
    assert str(exc.value) == msg


def test_calculate_bindings_change_direct(fakes):
    cat = FakeCat("c")
    out = {}
    assert calculate_bindings_change_no_slips(out, fv({"a": 1}), fv({"a": 2}), cat) == 1
    assert out["changed_bindings"]["a"].d == 1 and out["unchanged_bindings"] == {}
    out = {}
    assert calculate_bindings_change_no_slips(out, fv({"a": 1}), fv({"a": 9}), cat) is None
    assert out == {}
    # with slips: every key of b2 finds a unique partner, whatever the shuffle order
    out = {}
    assert calculate_bindings_change_with_slips(out, fv({"x": 1, "y": 9}), fv({"x": 9, "y": 1}), cat) == 1
    assert out["slippages"] == {"x": "y", "y": "x"}
    assert cat.suff_calls == [["x", "y"], ["x", "y"]]
    # the reverse check does not record slippages
    cat.suff_calls.clear()
    out = {}
    assert calculate_bindings_change_with_slips(out, fv({"x": 1}), fv({"x": 2}), cat, 1) == 1
    assert cat.suff_calls == [["x"]]
    # no_slips success short-circuits the slips path
    out = {}
    assert calculate_bindings_change(out, fv({"a": 1}), fv({"a": 2}), cat) == 1
    assert "slippages" not in out


def test_with_slips_takes_first_match_in_shuffled_order(fakes):
    """Several partners match: the choice follows util.shuffle of uniq(keys b2, keys b1)."""
    cat = FakeCat("c")
    b1, b2 = fv({"p": 1, "q": 2, "r": 3}), fv({"p": 2})
    util.srand(7)
    order = util.shuffle(util.uniq(*b2, *b1))
    util.srand(7)
    out = {}
    assert calculate_bindings_change_with_slips(out, b1, b2, cat, 1) == 1
    assert out["slippages"] == {"p": order[0]}


def test_with_slips_unmapped_key_dies_through_find_mapping(fakes):
    """A key only in b2 yields FindMapping(undef, v): no viable multimethod in Perl."""
    cat = FakeCat("c")
    util.srand(1)
    with pytest.raises(Confess, match="No viable"):
        calculate_bindings_change_with_slips({}, fv({}), fv({"z": 1}), cat, 1)


# --- ApplyMappingForCat ----------------------------------------------------------------

@pytest.mark.parametrize("g", [c for c in CASES if c["op"] == "ApplyMappingForCat" and "reln" in c],
                         ids=lambda c: c["label"])
def test_apply_mapping_for_cat(fakes, g):
    cat = FakeCat("mapcat", build_fails=g["build_fails"])
    rs = g["reln"]
    reln = FakeReln(cat=cat, changed={k: FakeRel(v) for k, v in rs["changed"].items()},
                    slips=rs["slips"], meto_mode=mode(rs["meto_mode"]),
                    meto_reln=FakeRel(rs["meto_reln"]) if "meto_reln" in rs else None,
                    pos_reln=FakeRel(rs["pos_reln"]) if "pos_reln" in rs else None)
    bind = fake_b(g["bindings"]) if g["bindings"] is not None else None
    target = FakeSObj(id="target", descr={"mapcat": bind})
    orig = FakeSObj(id="orig", effective=target) if g["effective"] else target
    if g["dies"]:
        with pytest.raises(Exception):
            cat.apply_mapping_for_cat(reln, orig)
        return
    r = cat.apply_mapping_for_cat(reln, orig)
    assert int(r is not None) == g["defined"]
    assert target.calls == g["target_calls"]
    if r is not None:
        assert r.built == g["built"]
        assert r.blemish == g["blemish"]
        assert r.group_p == g["group_p"]
        assert r.calls == g["result_calls"]


def test_apply_mapping_for_cat_errors(fakes):
    cat, other = FakeCat("mapcat"), FakeCat("other")
    reln = FakeReln(cat=other, changed={}, slips={}, meto_mode=METO_MODE.NONE)
    assert case("ApplyMappingForCat", "category mismatch")["dies"] == 1
    with pytest.raises(Confess, match="do not match"):
        cat.apply_mapping_for_cat(reln, FakeSObj(id="o"))
    assert case("ApplyMappingForCat", "undef object")["dies"] == 1
    with pytest.raises(Confess, match="Missing original_object"):
        cat.apply_mapping_for_cat(reln, None)


def test_unported_hooks_raise():
    # Item 019 ported Mapping::Structural->create, which needs a meto_mode.
    with pytest.raises(Confess, match="need meto_mode"):
        base._structural_create({})
    # Item 016 ported $Mapping::Dir::Same and the multimethods; item 017 Mapping::Numeric,
    # so FindMapping(#,#) now works.
    from seqsee.mapping import dir as mapping_dir
    from seqsee.mapping.numeric import MappingNumeric
    assert base._mapping_dir_same() is mapping_dir.SAME
    found = base._find_mapping(1, 2)
    assert isinstance(found, MappingNumeric) and found.as_text() == "succ"
    with pytest.raises(Confess, match="No viable candidate"):
        base._apply_mapping(1, 2)
    # Item 021 ported Seqsee::Object, whose group_p is required.
    with pytest.raises(Confess, match=r"Attribute \(group_p\) is required"):
        SeqseeObject()

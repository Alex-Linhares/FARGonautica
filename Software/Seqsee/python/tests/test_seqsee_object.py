"""Tests for Seqsee::Object, part I (item 021): Perl lib/Seqsee/Object.pm (construction,
attributes, items, create, describe_as & co., structure basics, relation handles), plus the
``Seqsee::Element::HasAsPartDeep`` sub Object.pm defines.

Golden data comes from oracle/seqsee_object.pl (tests/golden/seqsee_object.json). The
oracle uses real Seqsee::Elements as leaves, and so does this module (item 023). FakeCat
mirrors the oracle's: an SCategory whose
Instancer looks the object's structure string up in a table.
"""
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import util
from seqsee.categories import base
from seqsee.categories.base import SCategory
from seqsee.constants import DIR
from seqsee.errors import Confess, SErr
from seqsee.objects.element import Element
from seqsee.objects.object import SeqseeObject
from seqsee.shistory import SHistory

CASES = golden.load("seqsee_object")


def cases(kind, name=None):
    return [c for c in CASES if c["case"] == kind and (name is None or c.get("name") == name)]


def one(kind, name=None):
    found = cases(kind, name)
    assert len(found) == 1, (kind, name)
    return found[0]


def norm(text):
    """The oracle's rendering of addresses (they differ between Perl and Python)."""
    return re.sub(r"=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)", "=REF", text)


# --- fakes -------------------------------------------------------------------------------------


class FakeCat(SCategory):
    perl_name = "FakeCat"

    def __init__(self, name, inst=None):
        self.name = name
        self.inst = inst or {}
        self.calls = 0
        super().__init__()

    def instancer(self, o):
        self.calls += 1
        return self.inst.get(util.perl_str(o.get_structure_string()))

    def build(self, b):
        return None

    def get_name(self):
        return self.name

    def as_text(self):
        return "fake " + self.name

    def are_attributes_sufficient_to_build(self, *atts):
        return 1

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


def names(o):
    return sorted("UNDEF" if c is None else c.get_name() for c in o.get_categories())


def describe(o):
    if not isinstance(o, SeqseeObject):
        return {"ref": False, "value": o}
    try:
        structure = o.get_structure_string()
    except AttributeError:
        structure = "ERROR"
    return {
        "ref": util.perl_ref(o),
        "structure": structure,
        "group_p": o.get_group_p(),
        "strength": o.get_strength(),
        "count": o.get_parts_count(),
        "cats": names(o),
        "history": list(o.get_history()),
        "items": [describe(x) for x in o.get_items_array() if x is not o],
        "self_item": 1 if any(x is o for x in o.get_items_array()) else 0,
    }


def golden_describe(d):
    """Golden descriptions, with structure errors collapsed to "ERROR"."""
    if "structure" in d and isinstance(d["structure"], dict):
        d = {**d, "structure": "ERROR"}
    if "items" in d:
        d = {**d, "items": [golden_describe(x) for x in d["items"]]}
    return d


def error_text(e):
    return e if isinstance(e, str) else e["message"]


# --- construction ------------------------------------------------------------------------------


def test_defaults():
    c = one("defaults")
    o = SeqseeObject({"group_p": 1})
    assert o.get_strength() == c["strength"]
    assert o.get_group_p() == c["group_p"]
    assert o.get_metonym() is c["metonym"]
    assert o.get_metonym_activeness() == c["metonym_activeness"]
    assert o.get_is_a_metonym() is c["is_a_metonym"]
    assert (o.get_direction() is not None) == bool(c["direction_defined"])
    assert o.get_reln_scheme() is c["reln_scheme"]
    assert o.get_underlying_reln() is c["underlying_reln"]
    assert o.get_parts_ref() == c["parts"]
    assert o.get_parts_count() == c["count"]
    assert (o.get_items() is o.get_parts_ref()) == bool(c["items_is_parts_ref"])
    assert o.all_relations() == c["relations"]
    assert names(o) == c["cats"]
    assert o.get_history() == c["history"]
    assert util.perl_ref(o.get_history_obj()) == c["history_obj_ref"] == "SHistory"
    assert bool(o) == bool(c["true"])


NEW_ARGS = {
    "no args": {},
    "group_p 2": {"group_p": 2},
    "group_p undef": {"group_p": None},
    'group_p "0"': {"group_p": "0"},
    'group_p ""': {"group_p": ""},
    'group_p "1"': {"group_p": "1"},
    "group_p 0.0": {"group_p": 0.5},
    'group_p "abc"': {"group_p": "abc"},
    "items string": {"group_p": 1, "items": "x"},
    "items hash": {"group_p": 1, "items": {}},
    "items undef": {"group_p": 1, "items": None},
    "items numbers": {"group_p": 0, "items": [1, 2]},
    "history_obj 3": {"group_p": 1, "history_obj": 3},
    "metonym_activeness 5": {"group_p": 1, "metonym_activeness": 5},
    "metonym_activeness 1": {"group_p": 1, "metonym_activeness": 1},
    "reln_other_end array": {"group_p": 1, "reln_other_end": []},
    "categories string": {"group_p": 1, "categories": "c"},
    "strength string": {"group_p": 1, "strength": "abc"},
    "strength 35": {"group_p": 1, "strength": 35},
    "item (not init_arg)": {"group_p": 1, "item": [1]},
    "unknown key": {"group_p": 1, "foo": 1},
    "missing + bad items": {"items": "x"},
    "all bad": {"group_p": 2, "items": "x", "history_obj": 3, "metonym_activeness": 5,
                "reln_other_end": [], "categories": "c"},
    "all bad but cats": {"group_p": 2, "items": "x", "history_obj": 3, "metonym_activeness": 5,
                         "reln_other_end": []},
    "bad h/i/m/r": {"group_p": 1, "items": "x", "history_obj": 3, "metonym_activeness": 5,
                    "reln_other_end": []},
    "bad i/m/r": {"group_p": 1, "items": "x", "metonym_activeness": 5, "reln_other_end": []},
    "bad m/r": {"group_p": 1, "metonym_activeness": 5, "reln_other_end": []},
}


@pytest.mark.parametrize("c", cases("new"), ids=lambda c: c["name"])
def test_new_golden(c):
    args = NEW_ARGS[c["name"]]
    if "error" in c:
        with pytest.raises(Confess) as e:
            SeqseeObject(args)
        assert str(e.value) == c["error"]["message"]
    else:
        assert describe(SeqseeObject(args)) == golden_describe(c["ok"])


def test_new_kwargs_and_stored_values():
    c = one("new with values (list form)")
    o = SeqseeObject(group_p=0, strength=12, metonym="M", metonym_activeness=1,
                     is_a_metonym="IAM", direction=DIR.LEFT, reln_scheme="RS",
                     underlying_reln="UR")
    assert o.get_strength() == c["strength"]
    assert o.get_group_p() == c["group_p"]
    assert o.get_metonym() == c["metonym"]
    assert o.get_metonym_activeness() == c["metonym_activeness"]
    assert o.get_is_a_metonym() == c["is_a_metonym"]
    assert (o.get_direction() is DIR.LEFT) == bool(c["direction_is_left"])
    assert o.get_reln_scheme() == c["reln_scheme"]
    assert o.get_underlying_reln() == c["underlying_reln"]


def test_writers_golden():
    c = one("writers")
    o = SeqseeObject(group_p=0, strength=12, metonym="M", metonym_activeness=1,
                     is_a_metonym="IAM", direction=DIR.LEFT, reln_scheme="RS",
                     underlying_reln="UR")
    for r in c["results"]:
        method = getattr(o, r["method"])
        if r["error"] is None:
            method(r["value"])
        else:
            with pytest.raises(Confess) as e:
                method(r["value"])
            assert str(e.value) == r["error"]["message"]
    assert o.get_strength() == c["strength"]
    assert o.get_group_p() == c["group_p"]
    assert o.get_metonym() == c["metonym"]
    assert o.get_metonym_activeness() == c["metonym_activeness"]
    assert o.get_is_a_metonym() == c["is_a_metonym"]
    assert o.get_reln_scheme() == c["reln_scheme"]
    assert o.get_underlying_reln() == c["underlying_reln"]
    assert (o.get_direction() is not None) == bool(c["direction_defined"])


def test_constructor_keeps_given_containers():
    items, relns, cats, hist = [], {}, {}, SHistory()
    o = SeqseeObject(group_p=1, items=items, reln_other_end=relns, categories=cats,
                     history_obj=hist)
    assert o.get_parts_ref() is items
    assert o.reln_other_end() is relns
    assert o.get_cats_hash() is cats
    assert o.get_history_obj() is hist
    with pytest.raises(Confess, match="odd arguments"):
        SeqseeObject(1, 2)


def test_item_accessor():
    o = SeqseeObject(group_p=1)
    assert o.item() is o.get_parts_ref()
    new = [1]
    assert o.item(new) is new
    assert o.get_parts_ref() is new
    with pytest.raises(Confess, match=r"Attribute \(item\) does not pass"):
        o.item("x")


# --- history ----------------------------------------------------------------------------------


def test_history_golden():
    c = one("history")
    Global.Steps_Finished = 3
    o = SeqseeObject(group_p=1)
    Global.Steps_Finished = 5
    o.add_history("five")
    u4 = o.unchanged_since(4)
    u5 = o.unchanged_since(5)
    Global.Steps_Finished = 9
    assert o.get_history() == c["history"]
    assert (u4, u5) == (c["unchanged4"], c["unchanged5"])
    assert o.get_age() == c["age"]
    assert o.history_as_text() == c["as_text"]
    assert o.search_history(re.compile("five")) == c["search"]


# --- relation handles -------------------------------------------------------------------------


def test_relations_golden():
    log = one("relations")["log"]
    o, a, b = (SeqseeObject(group_p=1) for _ in range(3))
    got = [
        ["exists a", o.relation_exists_to(a)],
        ["get a", o.get_relation(a)],
        ["set a", o.set_relation_to(a, "RA")],
        ["set b", o.set_relation_to(b, "RB")],
        ["exists a", o.relation_exists_to(a)],
        ["get a", o.get_relation(a)],
        ["all", sorted(o.all_relations())],
        ["remove a", o.remove_reln_to(a)],
        ["remove a again", o.remove_reln_to(a)],
        ["exists a", o.relation_exists_to(a)],
        ["all", sorted(o.all_relations())],
        ["set a undef", o.set_relation_to(a, None)],
        ["exists a", o.relation_exists_to(a)],
        ["get a", o.get_relation(a)],
        ["history", list(o.get_history())],
    ]
    assert got == log


# --- create -----------------------------------------------------------------------------------


def check_create(name, *args):
    c = one("create", name)
    if "error" in c:
        with pytest.raises(Confess) as e:
            SeqseeObject.create(*args)
        assert str(e.value) == c["error"]
        return None
    o = SeqseeObject.create(*args)
    assert describe(o) == golden_describe(c["ok"])
    return o


@pytest.mark.parametrize("name,args", [
    ("no args", ()),
    ("one number", (5,)),
    ("zero", (0,)),
    ("empty array", ([],)),
    ("array of one", ([5],)),
    ("array of two", ([1, 2],)),
    ("three numbers", (1, 2, 3)),
    ("nested", (1, [2, 3], [[4]])),
    ("nested arrays deep", ([[[1, 2]]],)),
    ("array then empty", (1, [])),
    ("unblessed hash", ({},)),
    ("two unblessed hashes", ({}, {})),
])
def test_create_golden(name, args):
    check_create(name, *args)


def test_create_copies_golden():
    """The oracle's copy section, step for step (the FakeCats count Instancer calls)."""
    even_cat = FakeCat("evenish", {"[2, 4]": "B24", "[1, [2, 3]]": "Bnest", "4": "B4"})
    odd_cat = FakeCat("oddish", {"[2, 4]": "O24"})

    e = Element.create(4, 0)
    e.describe_as(even_cat)
    copy = check_create("copy of element", e)
    assert (copy is e) == bool(one("copy identity", "element")["same"])

    g = SeqseeObject.create(2, 4)
    g.describe_as(even_cat)
    g.describe_as(odd_cat)
    gc = check_create("copy of group with cats", g)
    c = one("copy identity", "group")
    assert (gc is g) == bool(c["same"])
    assert [1 if gc[i] is g[i] else 0 for i in range(2)] == c["items_same"]
    assert [even_cat.calls, odd_cat.calls] == c["calls"]

    check_create("copy of one-item group", SeqseeObject({"group_p": 1, "items": [e]}))
    check_create("copy of empty group", SeqseeObject({"group_p": 1, "items": []}))
    gn = SeqseeObject.create(1, [2, 3])
    gn.describe_as(even_cat)
    check_create("copy of nested group", gn)
    check_create("list of objects", e, g, 7)
    check_create("array of objects", [e, g])


def test_create_uses_class_for_groups_only():
    class Sub(SeqseeObject):
        perl_name = "SubObject"

    g = Sub.create(1, [2, 3])
    assert type(g) is Sub
    # Items are made by Seqsee::Object->create, not $package->create.
    assert type(g[1]) is SeqseeObject
    assert type(Sub.create(5)) is Element


def test_object_create_hook_is_wired():
    o = base._object_create(1, 2)
    assert type(o) is SeqseeObject
    assert o.get_structure() == [1, 2]


# --- describe_as & co -------------------------------------------------------------------------


def test_describe_as_family_golden():
    cat = FakeCat("c1", {"[1, 2]": "B12", "[3, 4]": 0})
    o = SeqseeObject.create(1, 2)
    r1 = o.describe_as(cat)
    c1 = cat.calls
    r2 = o.describe_as(cat)
    c = one("describe_as", "success then cached")
    assert (r1, r2, [c1, cat.calls]) == (c["r1"], c["r2"], c["calls"])
    assert o.get_binding_for_category(cat) == c["binding"]
    assert o.is_of_category_p(cat) == c["is_of"]
    assert (names(o), o.get_history()) == (c["cats"], c["history"])

    for name, args in (("false bindings", (3, 4)), ("no bindings", (5, 6))):
        p = SeqseeObject.create(*args)
        r = p.describe_as(cat)
        c = one("describe_as", name)
        assert (r, names(p), p.get_history()) == (c["r"], c["cats"], c["history"])
    q = p

    c = one("annotate_with_cat", "already")
    assert o.annotate_with_cat(cat) == c["r"]
    assert c["error"] is None

    c = one("annotate_with_cat", "fails")
    with pytest.raises(SErr) as e:
        q.annotate_with_cat(cat)
    assert (type(e.value).perl_name, e.value.message) == (c["error"]["class"],
                                                          c["error"]["message"])
    assert (names(q), q.get_history()) == (c["cats"], c["history"])

    s = SeqseeObject.create(1, 2)
    c = one("annotate_with_cat", "succeeds")
    assert s.annotate_with_cat(cat) == c["r"]
    assert (names(s), s.get_history()) == (c["cats"], c["history"])

    t = SeqseeObject.create(1, 2)
    steps = [("success", None), ("success again", None), ("new bindings", {"[1, 2]": "NEW"}),
             ("failure removes", {}), ("failure when absent", None)]
    for name, inst in steps:
        if inst is not None:
            cat.inst = inst
        r = t.redescribe_as(cat)
        c = one("redescribe_as", name)
        assert r == c["r"], name
        if "binding" in c:
            assert t.get_binding_for_category(cat) == c["binding"]
        assert (names(t), t.get_history()) == (c["cats"], c["history"]), name


def run_recalc(o):
    try:
        o.recalculate_categories()
    except Confess as e:
        return norm(str(e))
    return None


def test_recalculate_categories_golden():
    cat = FakeCat("r1", {"[1, 2]": "R12"})
    o = SeqseeObject.create(1, 2)
    o.describe_as(cat)
    for name, inst in (("keeps", None), ("loses all", {}), ("no categories", None)):
        if inst is not None:
            cat.inst = inst
        err = run_recalc(o)
        c = one("recalculate_categories", name)
        assert err == c["error"], name
        assert (names(o), o.get_history()) == (c["cats"], c["history"]), name

    c2 = FakeCat("r2", {"[1, 2]": "R2"})
    cat.inst = {"[1, 2]": "R12"}
    p = SeqseeObject.create(1, 2)
    p.describe_as(cat)
    p.describe_as(c2)
    cat.inst = {}
    c = one("recalculate_categories", "loses one of two")
    assert run_recalc(p) == c["error"]
    assert names(p) == c["cats"]
    assert sorted(p.get_history()) == c["history_sorted"]

    e = Element.create(9, 0)
    c = one("recalculate_categories", "element")
    assert run_recalc(e) == c["error"]
    assert (names(e), e.get_history()) == (c["cats"], c["history"])


# --- structure basics -------------------------------------------------------------------------


def structure_objects():
    e1, e2, e3 = (Element.create(n, 0) for n in (1, 2, 3))
    g23 = SeqseeObject({"group_p": 1, "items": [e2, e3]})
    g = SeqseeObject({"group_p": 1, "items": [e1, g23]})
    emp = SeqseeObject({"group_p": 1, "items": []})
    return {
        "e1": e1, "g23": g23, "g": g,
        "one": SeqseeObject({"group_p": 1, "items": [g23]}),
        "one1": SeqseeObject({"group_p": 1, "items": [e1]}),
        "emp": emp,
        "wrap": SeqseeObject({"group_p": 1, "items": [emp, e1]}),
    }


@pytest.mark.parametrize("c", cases("structure"), ids=lambda c: c["name"])
def test_structure_golden(c):
    o = structure_objects()[c["name"]]
    assert o.get_structure() == c["structure"]
    assert o.get_structure_string() == c["structure_string"]
    if c["as_text"] is not None:
        assert o.as_text() == c["as_text"]
    assert o.get_flattened() == c["flattened"]
    assert o.get_span() == c["span"]
    assert o.get_parts_count() == c["count"]
    assert len(o) == c["deref_count"]


def test_has_as_golden():
    objs = structure_objects()
    for c in cases("has_as"):
        x, y = objs[c["a"]], objs[c["b"]]
        assert x.has_as_item(y) == c["item"], c
        assert (1 if x.has_as_part_deep(y) else 0) == c["deep"], c
        assert (1 if x == y else 0) == c["smartmatch"], c


def test_deref_golden():
    objs = structure_objects()
    g = objs["g"]
    c = one("deref")
    assert (g[0] is objs["e1"]) == bool(c["first_is_e1"])
    assert (g[1] is objs["g23"]) == bool(c["second_is_g23"])
    assert [util.perl_ref(x) for x in g] == c["elements"]
    c = one("has_as_item scalar")
    assert g.has_as_item(1) == c["r"]
    assert (1 if g.has_as_part_deep("x") else 0) == c["deep"]


def test_has_as_item_on_scalar_items_compares_strings():
    o = SeqseeObject(group_p=0, items=[1, 2.0])
    assert o.has_as_item("1") == 1
    assert o.has_as_item(2) == 1   # perl_str(2.0) == "2"
    assert o.has_as_item(3) == 0

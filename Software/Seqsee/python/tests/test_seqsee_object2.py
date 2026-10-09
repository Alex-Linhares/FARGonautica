"""Tests for Seqsee::Object, part II (item 022): Perl lib/Seqsee/Object.pm (metonyms,
relations, apply_blemish_at, the CanBeSeenAs multimethod and its helpers, effective
slippages, squintability, UpdateStrength, set_underlying_ruleapp, get_pure,
GetAnnotatedStructureString), the ``Seqsee::Element::GetEffectiveStructure`` and
``ContainsAMetonym`` subs Object.pm defines, and lib/Seqsee/ResultOfCanBeSeenAs.pm.
It also checks FindMappingForCat and ApplyMapping(Mapping::Structural, Seqsee::Object) on
real objects (deferred from item 019).

Golden data comes from oracle/seqsee_object2.pl (tests/golden/seqsee_object2.json). The
leaves are real Seqsee::Elements (item 023), as in the oracle. SRelation->new, FindMapping (as Object.pm sees it), SRule->create,
SLTM::GetRealActivationsForConcepts and SLTM::Platonic->create are recorders in the
oracle, and the matching hooks in ``seqsee.objects.object`` are monkeypatched here.
"""
import gc
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import mapping
from seqsee import s as S
from seqsee import util
from seqsee.categories.base import SCategory
from seqsee.constants import DIR, RELN_SCHEME
from seqsee.errors import Confess, ExceptionClassBase
from seqsee.mapping.dir import SAME as MAPPING_DIR_SAME
from seqsee.mapping.numeric import MappingNumeric
from seqsee.objects import object as object_mod
from seqsee.objects.element import Element
from seqsee.objects.object import SeqseeObject, can_be_seen_as
from seqsee.objects.result_of_can_be_seen_as import NO, ResultOfCanBeSeenAs
from seqsee.smetonym import SMetonym
from seqsee.spos import SPos
from seqsee.srelation import SRelation

CASES = golden.load("seqsee_object2")
LOG = []


def cases(kind, name=None):
    return [c for c in CASES if c["case"] == kind and (name is None or c.get("name") == name)]


def one(kind, name=None):
    found = cases(kind, name)
    assert len(found) == 1, (kind, name)
    return found[0]


def norm(text):
    """The oracle's rendering of addresses."""
    if text is None:
        return None
    text = re.sub(r"=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)", "=REF", text)
    return re.sub(r"\b(HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)", r"\1(REF)", text)


def err(e):
    if isinstance(e, ExceptionClassBase):
        return {"class": type(e).perl_name, "message": norm(e.message or "")}
    return norm(str(e))


@pytest.fixture(autouse=True)
def fakes():
    LOG.clear()


# --- fakes -------------------------------------------------------------------------------------


class FakeCat(SCategory):
    """The oracle's FakeCat: Instancer looks up the structure string; metonym finders
    build the starred object from the ``metos`` table."""

    perl_name = "FakeCat"

    def __init__(self, name, inst=None, metos=None):
        self.name = name
        self.inst = inst or {}
        self.metos = metos or {}
        super().__init__()

    def instancer(self, o):
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
        return sorted(self.metos)

    def get_pure(self):
        return self

    def get_memory_dependencies(self):
        return []

    def serialize(self):
        return self.name

    @classmethod
    def deserialize(cls, s):
        return None

    def get_meto_finder(self, name):
        def finder(obj, cat, n, b):
            LOG.append(["finder", cat.get_name(), n, b])
            s = cat.metos.get(n)
            if s is None:
                return None
            return SMetonym({"category": cat, "name": n, "starred": SeqseeObject.create(s),
                             "unstarred": obj, "info_loss": {}})
        return finder

    def find_metonym(self, obj, name):
        return self.get_meto_finder(name)(obj, self, name, "FIND")


class FakeEnd(SeqseeObject):
    perl_name = "FakeEnd"

    def __init__(self, bounds):
        super().__init__(group_p=1)
        self.bounds = bounds

    def get_bounds_string(self):
        return self.bounds


class FakeReln:
    perl_name = "FakeReln"

    def __init__(self, label, ends=(), type_=None):
        self.label, self.ends, self.type = label, list(ends), type_

    def get_ends(self):
        return tuple(self.ends)

    def get_type(self):
        return self.type

    def insert(self):
        LOG.append("insert " + self.label)

    def uninsert(self):
        LOG.append("uninsert " + self.label)


class FakeType:
    def __init__(self, cat):
        self.cat = cat

    def get_category(self):
        return self.cat


class FakeRCat:
    def __init__(self, table):
        self.table = table

    def find_mapping_for_cat(self, f, s):
        k = f.get_bounds_string() + s.get_bounds_string()
        LOG.append("FindMappingForCat " + k)
        return self.table.get(k)


class SRuleBase:
    perl_name = "SRule"


class FakeRule(SRuleBase):
    perl_name = "FakeRule"

    def __init__(self, label, app=None):
        self.label, self.app = label, app

    def check_applicability(self, opts):
        LOG.append(["check", self.label, [o.get_structure_string() for o in opts["objects"]],
                    1 if opts["direction"] is DIR.RIGHT else 0, ",".join(sorted(opts))])
        return self.app


class FakeApp:
    perl_name = "FakeApp"


class FakeSRel(SRelation):
    perl_name = "FakeSRel"

    def __init__(self):
        pass


def fake_srelation_new(opts):
    label = ",".join([opts["first"].get_bounds_string(), opts["second"].get_bounds_string(),
                      "undef" if opts["type"] is None else opts["type"]])
    LOG.append("new " + label)
    if opts["type"] == "NONE":
        return None
    return FakeReln(label, [opts["first"], opts["second"]], opts["type"])


FIND_TABLE = {"AB": "TAB", "BC": "NONE", "CD": None}


def fake_find_mapping(a, b):
    k = a.get_bounds_string() + b.get_bounds_string()
    LOG.append("FindMapping " + k)
    v = FIND_TABLE.get(k)
    return "T" + k if v is None else v


# --- observable descriptions (mirroring the oracle's) -------------------------------------------


def names(o):
    return sorted("UNDEF" if c is None else c.get_name() for c in o.get_categories())


def ss(o):
    if o is None:
        return None
    return o.get_structure_string() if util.perl_ref(o) else util.perl_str(o)


def rd(r):
    if r is None:
        return {"undef": 1}
    if not isinstance(r, ResultOfCanBeSeenAs):
        return {"plain": r}
    parts = r.get_parts_blemished()
    entire = r.get_entire_blemish()
    return {
        "success": r.success,
        "bool": 1 if r else 0,
        "is_no": 1 if r is NO() else 0,
        "entire_p": 1 if r.is_entire_blemished() else 0,
        "entire": ss(entire.get_starred()) if util.perl_ref(entire) else entire,
        "parts_p": 1 if r.are_parts_blemished() else 0,
        "parts": None if parts is None else {str(k): ss(v.get_starred()) for k, v in parts.items()},
        "blemished": 1 if r.is_blemished() else 0,
    }


def same(a, b):
    return 1 if a is not None and b is not None and util.perl_ref(a) and a is b else 0


def item_info(x):
    m = x.get_metonym()
    return {
        "ref": util.perl_ref(x),
        "structure": x.get_structure_string(),
        "active": x.get_metonym_activeness(),
        "has_meto": 1 if m else 0,
        "starred": ss(m.get_starred()) if m else None,
        "meto_name": m.get_name() if m else None,
        "meto_cat": m.get_category().get_name() if m else None,
        "info_loss": m.get_info_loss() if m else None,
        "unstarred_defined": (1 if m.get_unstarred() is not None else 0) if m else None,
        "starred_is_a_metonym_is_x": same(m.get_starred().get_is_a_metonym(), x) if m else None,
        "cats": names(x),
        "history": list(x.get_history()),
        "metonymed": x.is_this_a_metonymed_object(),
        "contains": x.contains_a_metonym(),
        "concrete_is_self": same(x.get_concrete_object(), x),
    }


def strip(c):
    return {k: v for k, v in c.items() if k not in ("case", "name")}


# --- ResultOfCanBeSeenAs ----------------------------------------------------------------------


def test_result_constructors_golden():
    c = one("result", "NO")
    assert rd(NO()) == c["r"]
    assert (1 if NO() is ResultOfCanBeSeenAs.NO() else 0) == c["again_same"]
    c = one("result", "unblemished")
    u = ResultOfCanBeSeenAs.new_unblemished()
    assert rd(u) == c["r"]
    assert (0 if u is ResultOfCanBeSeenAs.new_unblemished() else 1) == c["fresh"]
    m = SMetonym({"category": S.SAMENESS, "name": "each", "info_loss": {"length": 2},
                  "starred": SeqseeObject.create(7), "unstarred": SeqseeObject.create(7, 7)})
    assert rd(ResultOfCanBeSeenAs.new_entire_blemish(m)) == one("result", "entire")["r"]
    assert rd(ResultOfCanBeSeenAs.new_entire_blemish(None)) == one("result", "entire undef")["r"]
    assert rd(ResultOfCanBeSeenAs.new_by_part({2: m})) == one("result", "by part")["r"]
    assert rd(ResultOfCanBeSeenAs.new_by_part({})) == one("result", "by part empty")["r"]


RESULT_NEW = {
    "no success": {},
    "success 2": {"success": 2},
    'success "0"': {"success": "0"},
    "success undef": {"success": None},
    "part_blemish undef": {"success": 1, "part_blemish": None},
    "part_blemish array": {"success": 1, "part_blemish": []},
    "all bad": {"success": 2, "part_blemish": 3},
    "entire 0": {"success": 1, "entire_blemish": 0},
}


@pytest.mark.parametrize("c", cases("result new"), ids=lambda c: c["name"])
def test_result_new_golden(c):
    args = RESULT_NEW[c["name"]]
    if "error" in c:
        with pytest.raises(Confess) as e:
            ResultOfCanBeSeenAs(args)
        assert str(e.value) == c["error"]["message"]
    else:
        assert rd(ResultOfCanBeSeenAs(args)) == c["r"]


def test_perl_true_honours_the_bool_overload():
    assert not util.perl_true(NO())
    assert util.perl_true(ResultOfCanBeSeenAs.new_unblemished())
    assert util.perl_true(SeqseeObject.create())       # empty objects stay true
    assert util.perl_true([]) and util.perl_true({})    # array/hash refs


# --- apply_blemish_at -------------------------------------------------------------------------


def test_blemish_middle_golden():
    orig = SeqseeObject.create(1, 2, 3)
    e2 = orig[1]
    bl = orig.apply_blemish_at(S.DOUBLE, SPos(2))
    gc.collect()
    slips = bl.get_effective_slippages()
    got = {
        "ref": util.perl_ref(bl),
        "structure": bl.get_structure_string(),
        "annotated": bl.get_annotated_structure_string(),
        "effective": bl.get_effective_structure_string(),
        "effective_structure": bl.get_effective_structure(),
        "cats": names(bl),
        "history": list(bl.get_history()),
        "items": [item_info(x) for x in bl],
        "orig_e2_is_a_metonym_is_mid": same(e2.get_is_a_metonym(), bl[1]),
        "orig_e2_metonymed": e2.is_this_a_metonymed_object(),
        "orig_e2_concrete_is_mid": same(e2.get_concrete_object(), bl[1]),
        "orig_contains": orig.contains_a_metonym(),
        "orig_structure": orig.get_structure_string(),
        "contains": bl.contains_a_metonym(),
        "slippages": {str(k): ss(v.get_starred()) for k, v in slips.items()},
        "mid_effective_is_e2": same(bl[1].get_effective_object(), e2),
        "mid_effective_structure": bl[1].get_effective_structure(),
        "e2_effective_structure": e2.get_effective_structure(),
    }
    assert got == strip(one("blemish", "middle"))

    bl[1].SetMetonymActiveness(0)
    got = {
        "annotated": bl.get_annotated_structure_string(),
        "effective": bl.get_effective_structure_string(),
        "slippages": list(bl.get_effective_slippages()),
        "mid_effective_is_mid": same(bl[1].get_effective_object(), bl[1]),
        "contains": bl.contains_a_metonym(),
        "mid_history": list(bl[1].get_history()),
    }
    assert got == strip(one("blemish", "middle, deactivated"))


@pytest.mark.parametrize("name,args,pos", [
    ("first", (4, 5), 1),
    ("last (-1)", (4, 5), -1),
    ("group part", (1, [2, 3]), 2),
])
def test_blemish_positions_golden(name, args, pos):
    r = SeqseeObject.create(*args).apply_blemish_at(S.DOUBLE, SPos(pos))
    gc.collect()
    c = strip(one("blemish", name))
    got = {"structure": r.get_structure_string(), "annotated": r.get_annotated_structure_string(),
           "items": [item_info(x) for x in r]}
    if "effective" in c:
        got["effective"] = r.get_effective_structure_string()
    assert got == c


def test_blemish_element_golden():
    """An element is its own only item: the blemished copy is [5, 5], and its first item
    (an element) gets the metonym."""
    e = Element.create(5, 0)
    r = e.apply_blemish_at(S.DOUBLE, SPos(1))
    gc.collect()
    got = {"ref": util.perl_ref(r), "structure": r.get_structure_string(),
           "annotated": r.get_annotated_structure_string(), "items": [item_info(x) for x in r],
           "e_is_a_metonym_is_r0": same(e.get_is_a_metonym(), r[0])}
    assert got == strip(one("blemish", "element"))


@pytest.mark.parametrize("name,args", [("out of range", ((4, 5), 4)),
                                       ("out of range 0 items", ((), 1))])
def test_blemish_out_of_range_golden(name, args):
    items, pos = args
    with pytest.raises(ExceptionClassBase) as e:
        SeqseeObject.create(*items).apply_blemish_at(S.DOUBLE, SPos(pos))
    assert err(e.value) == one("blemish", name)["error"]


# --- CanBeSeenAs ------------------------------------------------------------------------------


def fresh_objects():
    o = {
        "e5": Element.create(5, 0),
        "g123": SeqseeObject.create(1, 2, 3),
        "gn": SeqseeObject.create(1, [2, 3]),
        "one": SeqseeObject({"group_p": 1, "items": [Element.create(5, 0)]}),
        "emp": SeqseeObject.create(),
        "bl": SeqseeObject.create(1, 2, 3).apply_blemish_at(S.DOUBLE, SPos(2)),
        "blin": SeqseeObject.create(1, 2, 3).apply_blemish_at(S.DOUBLE, SPos(2)),
    }
    o["blin"][1].SetMetonymActiveness(0)
    o["mid"] = o["bl"][1]
    o["midin"] = o["blin"][1]
    return o


STRUCTS = {
    "5": 5, "2": 2, "6": 6, "-3": -3,
    '"5"': "5", '"-5"': "-5", '"5.0"': "5.0", '"abc"': "abc", "undef": None,
    "[5]": [5], "[]": [], "[1,2,3]": [1, 2, 3], "[1,[2,2],3]": [1, [2, 2], 3],
    "[1,[2,3]]": [1, [2, 3]], "[2,2]": [2, 2], "[[1,2,3]]": [[1, 2, 3]], "[1,2]": [1, 2],
    "[[2,3]]": [[2, 3]],
}


def run(f, *args):
    try:
        return {"r": rd(f(*args))}
    except (Confess, ExceptionClassBase) as e:
        return {"error": err(e)}


def test_can_be_seen_as_matrix_golden():
    objs, other = fresh_objects(), fresh_objects()
    matrix = cases("cbsa")
    assert len(matrix) == 9 * 23
    for c in matrix:
        s = c["struct"]
        arg = other[s[4:]] if s.startswith("obj ") else STRUCTS[s]
        got = run(can_be_seen_as, objs[c["obj"]], arg)
        assert got == strip({k: v for k, v in c.items() if k not in ("obj", "struct")}), c


@pytest.mark.parametrize("c", cases("cbsa plain"), ids=lambda c: c["name"])
def test_can_be_seen_as_plain_golden(c):
    x, y = {"5 5": (5, 5), "5 6": (5, 6), "5 5.0": (5, 5.0), "5 [5]": (5, [5]),
            "5 obj": (5, Element.create(5, 0)), '"5" 5': ("5", 5)}[c["name"]]
    assert run(can_be_seen_as, x, y) == strip(c)


HELPERS = {"CanBeSeenAs_Literal": "can_be_seen_as_literal",
           "CanBeSeenAs_Literal0rMeto": "can_be_seen_as_literal_or_meto",
           "CanBeSeenAs_ByPart": "can_be_seen_as_by_part"}


def test_can_be_seen_as_helpers_golden():
    objs, other = fresh_objects(), fresh_objects()
    structs = {"[2,2]": [2, 2], "2": 2, "[1,2,3]": [1, 2, 3], "obj e5": other["e5"]}
    for c in cases("cbsa helpers"):
        for perl, py in HELPERS.items():
            want = c[perl]
            f = getattr(objs[c["obj"]], py)
            if "error" in want:
                with pytest.raises(Confess) as e:
                    f(structs[c["struct"]])
                if want["error"]:
                    assert str(e.value) == want["error"], (c, perl)
                continue
            got = f(structs[c["struct"]])
            if want["n"] == 0 or "undef" in want["r"] or "plain" in want["r"]:
                assert got is None, (c, perl)
            else:
                assert rd(got) == want["r"], (c, perl)


def test_can_be_seen_as_meto_golden():
    c = one("cbsa meto")
    g = SeqseeObject.create(1, 2, 3)
    with pytest.raises(Confess) as e:
        g.can_be_seen_as_meto([1, 2, 3], g)
    assert str(e.value) == c["three_args_error"]
    r = g.can_be_seen_as_meto([1, 2, 3], g, "M")
    assert (r.get_entire_blemish() == "M") == bool(c["match_entire_is_M"])
    assert g.can_be_seen_as_meto([1, 2], g, "M") is None


def test_can_be_seen_as_method_golden():
    assert rd(SeqseeObject.create(1, 2, 3).can_be_seen_as([1, 2, 3])) == one("cbsa method")["r"]


# --- metonym management -----------------------------------------------------------------------


def test_metonym_log_golden():
    o = SeqseeObject.create(1, 2)
    log = []
    try:
        o.SetMetonymActiveness(1)
        e = None
    except ExceptionClassBase as ex:
        e = err(ex)
    log.append(["on without metonym", e, o.get_metonym_activeness()])

    def ret_list(v):
        return [] if v is None else [v]
    log.append(["off without metonym", ret_list(o.SetMetonymActiveness(0)), o.get_metonym_activeness()])
    star = SeqseeObject.create(7, 7)
    m = SMetonym({"category": S.SAMENESS, "name": "each", "info_loss": {}, "starred": star,
                  "unstarred": o})
    r = o.SetMetonym(m)
    log.append(["SetMetonym", 1, same(r, m), same(o.get_metonym(), m), same(star.get_is_a_metonym(), o)])
    log.append(["on", ret_list(o.SetMetonymActiveness(1)), o.get_metonym_activeness()])
    log.append(["on again", ret_list(o.SetMetonymActiveness("yes")), o.get_metonym_activeness()])
    log.append(["effective is star", same(o.get_effective_object(), star)])
    log.append(["off", ret_list(o.SetMetonymActiveness("")), o.get_metonym_activeness()])
    log.append(["effective is self", same(o.get_effective_object(), o)])
    log.append(["off again", ret_list(o.SetMetonymActiveness(None)), o.get_metonym_activeness()])
    log.append(["history", list(o.get_history())])
    m5 = SMetonym({"category": S.SAMENESS, "name": "each", "info_loss": {}, "starred": "5",
                   "unstarred": o})
    with pytest.raises(ExceptionClassBase) as ex:
        o.SetMetonym(m5)
    log.append(["SetMetonym scalar starred", err(ex.value), same(o.get_metonym(), m)])
    mh = SMetonym({"category": S.SAMENESS, "name": "each", "info_loss": {}, "starred": {"a": 1},
                   "unstarred": o})
    with pytest.raises(ExceptionClassBase) as ex:
        o.SetMetonym(mh)
    log.append(["SetMetonym hash starred", err(ex.value)])
    assert log == one("metonym log")["log"]


def test_metonymed_log_golden():
    o = SeqseeObject.create(1, 2)
    star = SeqseeObject.create(7, 7)
    o.SetMetonym(SMetonym({"category": S.SAMENESS, "name": "each", "info_loss": {},
                           "starred": star, "unstarred": o}))
    p = SeqseeObject.create(3, 4)
    log = [["plain", p.is_this_a_metonymed_object(), same(p.get_concrete_object(), p)]]
    p.set_is_a_metonym(p)
    log.append(["self", p.is_this_a_metonymed_object(), same(p.get_concrete_object(), p)])
    p.set_is_a_metonym(o)
    log.append(["other", p.is_this_a_metonymed_object(), same(p.get_concrete_object(), o),
                p.contains_a_metonym()])
    p.set_is_a_metonym(0)
    log.append(["zero", p.is_this_a_metonymed_object(), same(p.get_concrete_object(), p)])
    q = SeqseeObject({"group_p": 1, "items": [SeqseeObject.create(1, 2), star]})
    log.append(["contains via item", q.contains_a_metonym(), star.is_this_a_metonymed_object()])
    log.append(["element", Element.create(3, 0).contains_a_metonym()])
    assert log == one("metonymed log")["log"]


def attempt(f, *args):
    try:
        return f(*args), None
    except (Confess, ExceptionClassBase) as e:
        return None, err(e)


def test_annotate_with_metonym_golden():
    cat = FakeCat("am", {"[1, 2]": "B12"}, {"ok": [7, 7], "none": None})
    o, o2 = SeqseeObject.create(1, 2), SeqseeObject.create(3, 4)
    log = []
    LOG.clear()
    r, e = attempt(o.annotate_with_metonym, cat, "ok")
    meto = o.get_metonym()
    log.append(["annotate ok", e, 1, names(o), list(o.get_history()), ss(meto.get_starred()),
                same(meto.get_starred().get_is_a_metonym(), o), o.get_metonym_activeness(), list(LOG)])
    assert r is meto
    LOG.clear()
    _, e = attempt(o2.annotate_with_metonym, cat, "ok")
    log.append(["annotate not of cat", e, names(o2), list(o2.get_history()), list(LOG)])
    before = o.get_metonym()
    _, e = attempt(o.annotate_with_metonym, cat, "none")
    log.append(["annotate none", e, same(o.get_metonym(), before), len(o.get_history())])
    r, e = attempt(o.maybe_annotate_with_metonym, cat, "none")
    log.append(["maybe none", e, 0 if r is None else 1])
    _, e = attempt(o.maybe_annotate_with_metonym, cat, "ok")
    log.append(["maybe ok", e, len(o.get_history()), ss(o.get_metonym().get_starred()),
                same(o.get_metonym(), before)])
    _, e = attempt(o2.maybe_annotate_with_metonym, cat, "ok")
    log.append(["maybe not of cat", e])
    _, e = attempt(o2.maybe_annotate_with_metonym, S.ASCENDING, "ok")
    log.append(["maybe no find_metonym", e])
    assert log == one("annotate log")["log"]


# --- relations --------------------------------------------------------------------------------


def test_relations_golden():
    o, a, b = FakeEnd("O"), FakeEnd("A"), FakeEnd("B")
    r1, r2, r3 = FakeReln("r1", [o, a]), FakeReln("r2", [b, o]), FakeReln("r3", [a, b])
    log = []
    for what, r in (("add", r1), ("add", r2), ("add", r1), ("add", r3), ("remove", r1),
                    ("remove", r1), ("remove", r3), ("add", r1), ("removeall", None)):
        f = {"add": o.add_relation, "remove": o.remove_relation}.get(what)
        _, e = attempt(f, r) if f else attempt(o.remove_all_relations)
        log.append([what, r.label if r else None, e, sorted(x.label for x in o.all_relations()),
                    1 if o.relation_exists_to(a) else 0, 1 if o.relation_exists_to(b) else 0])
    c = one("relations log")
    assert log == c["log"]
    assert o.get_history() == c["history"]
    assert LOG == c["calls"]

    c = one("other end")
    assert o._get_other_end_of_reln(r1).get_bounds_string() == c["a"]
    assert o._get_other_end_of_reln(r2).get_bounds_string() == c["b"]
    assert o._get_other_end_of_reln(FakeReln("s", [o, o])).get_bounds_string() == c["self"]


def test_recalculate_relations_golden(monkeypatch):
    monkeypatch.setattr(object_mod, "_srelation_new", fake_srelation_new)
    a, b = FakeEnd("A"), FakeEnd("B")
    t = FakeType(FakeRCat({"OA": "NEWT", "BO": None}))
    p = FakeEnd("O")
    p.set_relation_to(a, FakeReln("ra", [p, a], t))
    p.set_relation_to(b, FakeReln("rb", [b, p], t))
    LOG.clear()
    p.recalculate_relations()
    c = one("recalculate_relations")
    assert sorted(LOG) == c["calls"]
    assert p.get_history() == c["history"]


def test_apply_reln_scheme_golden(monkeypatch):
    monkeypatch.setattr(object_mod, "_srelation_new", fake_srelation_new)
    monkeypatch.setattr(object_mod, "_find_mapping", fake_find_mapping)
    ends = [FakeEnd(x) for x in "ABCDE"]
    g = SeqseeObject({"group_p": 1, "items": list(ends)})
    ends[3].set_relation_to(ends[4], "EXISTING")
    ends[2].set_relation_to(ends[3], 0)
    for name, scheme in (("undef", None), ("zero", 0), ("NONE", RELN_SCHEME.NONE),
                         ("CHAIN", RELN_SCHEME.CHAIN), ("string", "foo"), ("number", 5)):
        LOG.clear()
        _, e = attempt(g.apply_reln_scheme, scheme)
        c = one("apply_reln_scheme", name)
        assert (e, LOG, g.get_history()) == (c["error"], c["calls"], c["history"]), name
    single = SeqseeObject({"group_p": 1, "items": [ends[0]]})
    LOG.clear()
    single.apply_reln_scheme(RELN_SCHEME.CHAIN)
    c = one("apply_reln_scheme", "one item")
    assert (LOG, single.get_history()) == (c["calls"], c["history"])


# --- UpdateStrength ---------------------------------------------------------------------------

ACT = {"c1": 0.5, "c2": 1, "c3": None, "c4": 0.123}


def fake_activations(cats):
    LOG.append(["activations", sorted(c.get_name() for c in cats)])
    return [ACT[c.get_name()] for c in cats]


def g15(x):
    return float("%.15g" % x)


def sorted_calls(calls):
    return [[k, sorted(v)] for k, v in calls]


def test_update_strength_golden(monkeypatch):
    monkeypatch.setattr(object_mod, "_get_real_activations_for_concepts", fake_activations)
    cats = [FakeCat(n, {"[1, 2, 3]": "B", "[]": "E"}) for n in ("c1", "c2", "c3", "c4")]
    g = SeqseeObject.create(1, 2, 3)
    log = []

    def step(name):
        LOG.clear()
        r = g.update_strength()
        log.append([name, g15(g.get_strength()), g15(r), list(LOG)])

    step("no cats")
    g.describe_as(cats[0])
    step("c1")
    Global.GroupStrengthByConsistency[g] = 30
    step("c1 + consistency")
    g.describe_as(cats[1])
    step("c1 c2 + consistency (capped)")
    del Global.GroupStrengthByConsistency[g]
    g.remove_category(cats[0])
    g.remove_category(cats[1])
    g.describe_as(cats[2])
    step("c3 undef activation")
    g.describe_as(cats[3])
    g[0].set_strength(33.3)
    step("c3 c4, float part")
    g[1].set_strength(None)
    step("undef part strength")
    Global.GroupStrengthByConsistency[g] = -500
    step("negative consistency")
    want = [[n, s, float(r), sorted_calls(calls)] for n, s, _, r, calls in one("UpdateStrength")["log"]]
    assert log == want

    e = SeqseeObject.create()
    LOG.clear()
    e.update_strength()
    s1 = e.get_strength()
    e.describe_as(cats[3])
    e.update_strength()
    c = one("UpdateStrength empty")
    assert [g15(s1), g15(e.get_strength())] == c["strengths"]
    assert LOG == c["calls"]


# --- set_underlying_ruleapp -------------------------------------------------------------------


def test_set_underlying_ruleapp_golden(monkeypatch, capsys):
    next_rule = {}

    def fake_srule_create(r):
        LOG.append(["SRule->create", util.perl_ref(r)])
        return next_rule["rule"]

    monkeypatch.setattr(object_mod, "_srule_create", fake_srule_create)
    g = SeqseeObject.create(1, [2, 3])
    app = FakeApp()
    log = []

    def attempt_set(name, arg, nxt=None):
        LOG.clear()
        next_rule["rule"] = nxt
        capsys.readouterr()
        r, e = attempt(g.set_underlying_ruleapp, arg)
        out = capsys.readouterr().out
        reln = g.get_underlying_reln()
        log.append([name, e, norm(out), None if e else r, list(LOG),
                    [norm(h) for h in g.get_history()], "APP" if reln is app else reln])

    attempt_set("undef", None)
    attempt_set("zero", 0)
    attempt_set("rule with app", FakeRule("R1", app))
    attempt_set("rule without app", FakeRule("R2"))
    attempt_set("srelation, rule", FakeSRel(), FakeRule("R3", app))
    attempt_set("srelation, no rule", FakeSRel(), None)
    attempt_set("mapping, rule", MappingNumeric.create("succ", S.NUMBER), FakeRule("R4", app))
    attempt_set("string", "foo")
    attempt_set("mapping dir", MAPPING_DIR_SAME)
    want = one("set_underlying_ruleapp")["log"]
    assert len(log) == len(want)
    for got, w in zip(log, want):
        n = w[3]
        # Perl's return count: 0 for errors and for the `return;` path, else 1.
        assert (got[3] is not None) <= (n == 1), got
        assert got[:3] + got[4:] == w[:3] + w[4:], got


# --- get_pure, annotated/effective structure --------------------------------------------------


def test_get_pure_and_annotated_golden(monkeypatch):
    monkeypatch.setattr(object_mod, "_platonic_create", lambda s: f"PLATONIC({s})")
    g = SeqseeObject.create(1, [2, 3])
    got = {
        "pure": g.get_pure(),
        "element": Element.create(4, 0).get_annotated_structure_string(),
        "group": g.get_annotated_structure_string(),
        "effective": g.get_effective_structure(),
        "effective_string": g.get_effective_structure_string(),
        "one_effective": SeqseeObject({"group_p": 1, "items": [Element.create(4, 0)]})
        .get_effective_structure(),
        "empty_effective": SeqseeObject.create().get_effective_structure_string(),
        "empty_annotated": SeqseeObject.create().get_annotated_structure_string(),
    }
    assert got == strip(one("get_pure"))


def test_unported_hooks_raise():
    # Item 025: SRule->create is real now (createRule's multimethod dispatch).
    with pytest.raises(Confess, match=r"No viable candidate for call to multimethod createRule\(\$\)"):
        object_mod._srule_create(None)
    # Item 029: SLTM::GetRealActivationsForConcepts is real now.
    assert object_mod._get_real_activations_for_concepts([]) == []
    # Item 028: SLTM::Platonic->create is real now.
    assert object_mod._platonic_create("[1]").get_structure() == [1]
    # Item 024: SRelation->new is real now (Moose's required check).
    with pytest.raises(Confess, match=r"Attribute \(first\) is required"):
        object_mod._srelation_new({})


# --- squintability ----------------------------------------------------------------------------


def calls_key(entry):
    return " ".join(util.perl_str(x) for x in entry)


def test_check_squintability_golden():
    c1 = FakeCat("sq1", {"[1, 2, 3]": "B1"},
                 {"m_a": [1, 2, 3], "m_b": [9], "m_c": None, "m_d": 4, "m_e": [1, 2, 3]})
    c2 = FakeCat("sq2", {"[1, 2, 3]": "B2"}, {"x": [1, 2, 3], "y": 4})
    c3 = FakeCat("sq3", {}, {"z": [1, 2, 3]})
    o = SeqseeObject.create(1, 2, 3)
    for cat in (c1, c2, c3):
        o.describe_as(cat)
    for name, intended in (("group", SeqseeObject.create(1, 2, 3)),
                           ("element 4", Element.create(4, 0)),
                           ("nothing", SeqseeObject.create(8, 8))):
        LOG.clear()
        r = o.check_squintability(intended)
        c = one("squint", name)
        assert sorted(f"{t.get_category().get_name()}/{t.get_name()}" for t in r) == c["types"]
        assert sorted(LOG, key=calls_key) == c["calls"]
    LOG.clear()
    r = o.check_squintability_for_category("4", c1)
    c = [x for x in cases("squint for cat") if "name" not in x][0]
    assert ([t.get_name() for t in r], LOG) == (c["types"], c["calls"])
    with pytest.raises(Confess) as e:
        o.check_squintability_for_category("4", c3)
    assert str(e.value) == one("squint for cat", "not an instance")["error"]


# --- real objects: FindMappingForCat / ApplyMapping(Mapping::Structural, Seqsee::Object) ------


@pytest.mark.parametrize("c", cases("real mapping"), ids=lambda c: c["name"])
def test_real_object_mapping_golden(c, monkeypatch):
    # SLTM::SpikeAndChoose (item 029): the only common category of two SInts here is
    # $S::NUMBER, which is also FindMapping's fallback for undef.
    monkeypatch.setattr(mapping, "_spike_and_choose", lambda amount, *concepts: None)
    cat, s1, s2 = {
        "asc 123 -> 1234": (S.ASCENDING, (1, 2, 3), (1, 2, 3, 4)),
        "asc 234 -> 123": (S.ASCENDING, (2, 3, 4), (1, 2, 3)),
        "desc 321 -> 4321": (S.DESCENDING, (3, 2, 1), (4, 3, 2, 1)),
        "same 22 -> 333": (S.SAMENESS, (2, 2), (3, 3, 3)),
    }[c["name"]]
    a, b = SeqseeObject.create(*s1), SeqseeObject.create(*s2)
    a.describe_as(cat)
    b.describe_as(cat)
    m = cat.find_mapping_for_cat(a, b)
    cb = dict(m.get_changed_bindings()) if m else {}
    applied = mapping.apply_mapping(m, b) if m else None
    got = {
        "found": 1 if m else 0,
        "ref": util.perl_ref(m),
        "changed": {k: (v.get_name() if util.perl_ref(v) else v) for k, v in cb.items()},
        "applied": ss(applied),
        "applied_cats": names(applied) if applied else None,
        "applied_reln_scheme": "CHAIN" if applied and applied.get_reln_scheme() else None,
    }
    assert got == strip(c)


def test_element_subs_from_object_pm():
    e = Element.create(6, 0)
    assert e.get_effective_structure() == 6
    assert e.contains_a_metonym() == 0
    assert isinstance(e, Element)

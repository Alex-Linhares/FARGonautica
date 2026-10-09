"""Tests for Seqsee::Element and Seqsee::Anchored (item 023): Perl lib/Seqsee/Element.pm
and lib/Seqsee/Anchored.pm.

Golden data comes from oracle/element_anchored.pl (tests/golden/element_anchored.json).
SWorkspace and SLTM::GetRealActivationsForConcepts are replaced there by recorders; here
the same recorders are wired in through the hooks in ``objects.anchored`` and
``objects.object``. TestGroup (an Anchored whose set_underlying_ruleapp is scripted),
SubAnchored, FakeConflicts and FakeRuleApp mirror the oracle's packages.
"""
import gc
import weakref

import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import util
from seqsee.categories import numeric, sameness
from seqsee.constants import DIR
from seqsee.errors import Confess, ExceptionClassBase, SErr
from seqsee.objects import anchored as anchored_mod
from seqsee.objects import object as object_mod
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.objects.object import SeqseeObject
from seqsee.sbindings import SBindings
from seqsee.spos import SPos


def _moose_errors_are_dies(x):
    """Moose throws Moose::Exception::* objects; the port raises Confess with the same
    message, so the golden classes are read as plain dies."""
    if isinstance(x, dict):
        if str(x.get("class", "")).startswith("Moose::Exception::"):
            x["class"] = "DIE"
        for v in x.values():
            _moose_errors_are_dies(v)
    elif isinstance(x, list):
        for v in x:
            _moose_errors_are_dies(v)
    return x


CASES = _moose_errors_are_dies(golden.load("element_anchored"))


def cases(kind, **match):
    found = [c for c in CASES if c["case"] == kind and all(c.get(k) == v for k, v in match.items())]
    assert found, (kind, match)
    return found


def one(kind, **match):
    found = cases(kind, **match)
    assert len(found) == 1, (kind, match)
    return found[0]


# --- recorders (the oracle's) --------------------------------------------------------------

LOG = []
ACT = {}


class World:
    conflicts = None
    super_groups = []
    find_dies = None
    element_count = 0


def bounds(o):
    if o is None:
        return "UNDEF"
    if util._is_scalar(o):
        return util.perl_str(o)
    return ",".join(util.perl_str(x) for x in o.get_edges())


def _conflicts(gp):
    if World.find_dies is not None:
        raise World.find_dies
    LOG.append(["conflicts", bounds(gp), util.perl_ref(gp)])
    return World.conflicts


def _activations(cats):
    LOG.append(["activations", sorted(c.get_name() for c in cats)])
    return [ACT.get(c.get_name()) for c in cats]


def _update_group(gp):
    LOG.append(["UpdateGroup", bounds(gp)])
    return "UPDATED"


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    LOG.clear()
    ACT.clear()
    World.conflicts, World.super_groups, World.find_dies, World.element_count = None, [], None, 0
    monkeypatch.setattr(object_mod, "_get_real_activations_for_concepts", _activations)
    monkeypatch.setattr(anchored_mod, "_find_groups_conflicting_with", _conflicts)
    monkeypatch.setattr(anchored_mod, "_get_super_groups", lambda gp: (
        LOG.append(["GetSuperGroups", "SWorkspace", bounds(gp)]), list(World.super_groups))[1])
    monkeypatch.setattr(anchored_mod, "_delete_group", lambda gp: LOG.append(["DeleteGroup", bounds(gp)]))
    monkeypatch.setattr(anchored_mod, "_remove_gp",
                        lambda gp: LOG.append(["remove_gp", "SWorkspace", bounds(gp)]))
    monkeypatch.setattr(anchored_mod, "_update_group", _update_group)
    monkeypatch.setattr(anchored_mod, "_element_count", lambda: World.element_count)


class FakeConflicts:
    def __init__(self, ok):
        self.ok = ok

    def resolve(self, opts):
        LOG.append(["Resolve", bounds(opts["IgnoreConflictWith"])])
        return self.ok


class FakeRuleApp:
    def __init__(self, rule=None):
        self.rule = rule

    def get_rule(self):
        LOG.append(["get_rule"])
        return self.rule

    def find_extension(self, opts):
        LOG.append(["FindExtension", opts["direction_to_extend_in"].text, opts["skip_this_many_elements"]])
        return "EXT"


class TestGroup(Anchored):
    perl_name = "TestGroup"
    __test__ = False

    def _init_attributes(self, kwargs):
        super()._init_attributes(kwargs)
        self.mode = kwargs.get("mode", "ok")

    def set_underlying_ruleapp(self, rule):
        LOG.append(["set_underlying_ruleapp", rule])
        if self.mode == "die":
            raise Confess("rule failed\n")
        if self.mode == "lose":
            self.set_underlying_reln(None)
        return None


class SubAnchored(Anchored):
    perl_name = "SubAnchored"


# --- helpers -------------------------------------------------------------------------------


def E(mag, pos):
    return Element.create(mag, pos)


def err(e):
    if e is None:
        return None
    if isinstance(e, ExceptionClassBase):
        return {"class": e.perl_name, "message": e.message}
    return {"class": "DIE", "message": str(e)}


def run(f):
    """(result, error) like the oracle's eval."""
    try:
        return f(), None
    except (Confess, ExceptionClassBase) as e:
        return None, err(e)


def as_list(r):
    """Perl's [@r] of a sub's return: `return;`/undef-by-return is []."""
    return [] if r is None else [r]


def cats(o):
    return sorted(c.get_name() for c in o.get_categories())


def hist(o):
    return list(o.get_history())


def describe(o):
    if util._is_scalar(o):
        return {"ref": 0, "value": o}
    d = {
        "ref": util.perl_ref(o),
        "edges": list(o.get_edges()),
        "structure": o.get_structure_string(),
        "strength": o.get_strength(),
        "group_p": o.get_group_p(),
        "cats": cats(o),
        "history": hist(o),
        "nitems": len(o.get_parts_ref()),
    }
    if o.get_parts_ref():
        d["self_item"] = 1 if o.get_parts_ref()[0] is o else 0
    return d


def num_eq(a, b):
    """Compare golden numbers with tolerance (strengths are floats in Python)."""
    if isinstance(a, dict) and isinstance(b, dict):
        return a.keys() == b.keys() and all(num_eq(a[k], b[k]) for k in a)
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(num_eq(x, y) for x, y in zip(a, b))
    if isinstance(a, (int, float)) and isinstance(b, (int, float)) \
            and not isinstance(a, bool) and not isinstance(b, bool):
        return abs(a - b) < 1e-9
    return a == b


# --- Moose new -------------------------------------------------------------------------------

ARGSETS = [
    {}, {"group_p": 0}, {"group_p": 0, "left_edge": 1},
    {"group_p": 0, "left_edge": 1, "right_edge": 1},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": 5},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": 1.5},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": "5"},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": "-3"},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": "+3"},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": " 3"},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": "x"},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": None},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": 5.0},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": []},
    {"group_p": 0, "left_edge": None, "right_edge": "a", "mag": 5},
    {"group_p": 0, "left_edge": 1, "mag": "x"},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": 2, "is_locked_against_deletion": 1},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": 2, "is_locked_against_deletion": 2},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": 2, "is_locked_against_deletion": 2, "items": 4},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "mag": 2, "metonym_activeness": 3},
    {"categories": 5},
    {"group_p": 0, "categories": 5},
    {"group_p": 0, "history_obj": 5},
    {"history_obj": 5},
    {"items": 5},
    {"group_p": 0, "items": 5},
    {"group_p": 0, "is_locked_against_deletion": 5},
    {"is_locked_against_deletion": 5},
    {"group_p": 0, "left_edge": 1, "right_edge": 1, "reln_other_end": 5},
    {"group_p": 0, "left_edge": 1, "metonym_activeness": 5},
    {"group_p": 0, "left_edge": 1, "mag": 2, "metonym_activeness": 5},
    {"group_p": 0, "left_edge": 1, "mag": 2, "reln_other_end": 5},
    {"group_p": 0, "left_edge": 1, "mag": 2, "right_edge": 3, "items": [1, 2]},
]


@pytest.mark.parametrize("cls", [Element, Anchored], ids=["Element", "Anchored"])
@pytest.mark.parametrize("index", range(len(ARGSETS)))
def test_golden_new(cls, index):
    c = one("new", **{"class": cls.perl_name, "index": index})
    o, e = run(lambda: cls(dict(ARGSETS[index])))
    if c.get("error"):
        assert e == c["error"]
        return
    assert e is None
    assert list(o.get_edges()) == c["edges"]
    assert cats(o) == c["cats"]
    assert hist(o) == c["hist"]
    assert len(o.get_parts_ref()) == c["nitems"]
    assert o.get_is_locked_against_deletion() == c["locked"]
    if "mag" in c:
        assert o.get_mag() == c["mag"]
    else:
        assert not hasattr(o, "get_mag")


def test_golden_new_count():
    assert len(cases("new")) == 2 * len(ARGSETS)


def test_object_new_checks_categories_before_group_p():
    """The interleaved Moose order applies to Seqsee::Object too (oracle probe)."""
    with pytest.raises(Confess, match=r"^Attribute \(categories\) does not pass"):
        SeqseeObject(categories=5)
    with pytest.raises(Confess, match=r"^Attribute \(group_p\) is required"):
        SeqseeObject(items=5)


# --- Element ---------------------------------------------------------------------------------


@pytest.mark.parametrize("c", cases("element_create"), ids=lambda c: str(c["args"]))
def test_golden_element_create(c):
    e = E(*c["args"])
    assert num_eq(describe(e), c["describe"])
    assert e.as_text() == c["as_text"]
    assert e.get_structure() == c["structure"]
    assert e.get_flattened() == c["flattened"]
    assert e.get_span() == c["span"]
    assert e.get_bounds_string() == c["bounds"]
    assert util.perl_str(e.get_annotated_structure_string()) == util.perl_str(c["annotated"])
    assert e.is_flush_left() == c["flush_left"]


@pytest.mark.parametrize("c", cases("element_create_bad"), ids=lambda c: str(c["mag"]))
def test_golden_element_create_bad(c):
    _, e = run(lambda: E(c["mag"], 0))
    assert e == c["error"]


@pytest.mark.parametrize("c", cases("element_features"), ids=lambda c: f"{c['feature']}-{c['mag']}")
def test_golden_element_features(c, monkeypatch):
    features = {"Primes": 1, "Parity": 1} if c["feature"] == "Both" else {c["feature"]: 1}
    for k, v in features.items():
        monkeypatch.setitem(Global.Feature, k, v)
    e = E(c["mag"], 0)
    assert cats(e) == c["cats"]
    assert hist(e) == c["hist"]


@pytest.mark.parametrize("c", cases("get_at_position"), ids=lambda c: str(c["position"]))
def test_golden_get_at_position(c):
    e = E(4, 1)
    r, error = run(lambda: e.get_at_position(SPos(c["position"])))
    assert (1 if r is e else 0) == c["is_self"]
    assert error == c["error"]


def test_golden_element_update_strength():
    e = E(4, 1)
    assert e.update_strength() is None
    assert one("element_update_strength")["returned"] == 0
    assert e.get_strength() == one("element_update_strength")["strength"]
    e.set_strength(33)
    e.update_strength()
    assert e.get_strength() == one("element_update_strength_keeps")["strength"]


def test_golden_mag_accessor():
    c = one("mag_accessor")
    e = E(4, 1)
    before = e.mag()
    e.mag(9)
    _, bad = run(lambda: e.mag(2.5))
    assert [before, e.get_mag(), bad, e.as_text()] == [c["before"], c["after"], c["bad"], c["as_text"]]


def test_golden_element_squint():
    c = one("element_squint")
    e = E(4, 0)
    e.remove_category(S.NUMBER)
    assert cats(e) == c["cats_before"]
    # The oracle took `return @ret` in scalar context: the count.
    assert len(e.check_squintability(E(4, 1))) == c["result"]
    assert cats(e) == c["cats_after"]
    assert hist(e) == c["hist"]


def test_element_is_weakrefable_and_collectable():
    e = E(3, 0)
    r = weakref.ref(e)
    del e
    gc.collect()
    assert r() is None


# --- Anchored accessors ----------------------------------------------------------------------


def test_golden_anchored_accessors():
    a = Anchored(group_p=1, left_edge=2, right_edge=4, items=[E(1, 2), E(2, 3), E(3, 4)])
    log = [["edges", list(a.get_edges())], ["bounds", a.get_bounds_string()],
           ["span", a.get_span()], ["as_text", a.as_text()]]
    a.set_underlying_reln(1)
    log.append(["as_text_u", a.as_text()])
    a.set_underlying_reln(0)
    log.append(["as_text_0", a.as_text()])
    ret = a.set_edges(5, 9)
    log.append(["set_edges_returns_self", 1 if ret is a else 0])
    log.append(["edges", list(a.get_edges())])
    log.append(["span", a.get_span()])
    a.recalculate_edges()
    log.append(["recalculated", list(a.get_edges())])
    log.append(["locked_default", a.get_is_locked_against_deletion()])
    a.set_is_locked_against_deletion(1)
    log.append(["locked", a.get_is_locked_against_deletion()])
    log.append(["locked_bad", run(lambda: a.set_is_locked_against_deletion(5))[1]])
    a.set_left_edge("abc")
    log.append(["left_any", a.get_left_edge()])
    assert log == one("anchored_accessors")["log"]


def test_golden_anchored_as_text_empty():
    a = Anchored(group_p=1, left_edge=0, right_edge=1)
    assert a.as_text() == one("anchored_as_text_empty")["as_text"]


@pytest.mark.parametrize("c", cases("next_pos_and_flush"), ids=lambda c: str(c["edges"]))
def test_golden_next_pos_and_flush(c):
    a = Anchored(group_p=1, left_edge=c["edges"][0], right_edge=c["edges"][1])
    got = {}
    for d in ("LEFT", "RIGHT", "UNKNOWN", "NEITHER"):
        r, e = run(lambda: a.get_next_pos_in_dir(getattr(DIR, d)))
        got[d] = e or r
    r, e = run(lambda: a.get_next_pos_in_dir(None))
    got["UNDEF"] = e or r
    assert got == c["next"]
    flush = []
    for count in (0, 1, 2, 6):
        World.element_count = count
        flush.append([count, a.is_flush_right(), a.is_flush_left()])
    assert flush == c["flush"]


def test_golden_spans_overlaps():
    iv = [[0, 0], [0, 3], [1, 2], [2, 5], [3, 3], [4, 6], [7, 8]]
    objs = [Anchored(group_p=1, left_edge=l, right_edge=r) for l, r in iv]
    m = []
    for i, a in enumerate(objs):
        for j, b in enumerate(objs):
            s, o = a.spans(b), a.overlaps(b)
            m.append([iv[i], iv[j], 1 if s else 0, util.perl_str(s), 1 if o else 0, util.perl_str(o)])
    assert m == one("spans_overlaps")["matrix"]


# --- create ------------------------------------------------------------------------------------


def mk_inputs():
    e0, e1, e2, e3 = E(1, 0), E(2, 1), E(3, 2), E(4, 3)
    g12 = Anchored(group_p=1, left_edge=1, right_edge=2, items=[e1, e2])
    obj = SeqseeObject.create(1, 2)
    return {
        "empty": [], "one_elt": [e0], "one_group": [g12], "two": [e0, e1], "three": [e0, e1, e2],
        "hole": [e0, e2], "reversed": [e1, e0], "same": [e0, e0], "group_elt": [g12, e3],
        "elt_group": [e0, g12], "overlap": [e1, g12], "unanchored": [obj], "unanch_2nd": [e0, obj],
        "hole_then_unanch": [e0, e2, obj], "number": [5],
    }


def norm_ref(e):
    if e and "Seqsee::Object=" in e["message"]:
        return {**e, "message": e["message"].split("=")[0] + "=REF"}
    return e


@pytest.mark.parametrize("c", cases("create"), ids=lambda c: c["name"])
def test_golden_create(c):
    ACT.update({"number": 0.1, "ascending": 0.5})
    items = mk_inputs()[c["name"]]
    valid, verr = run(lambda: Anchored._check_validity(*items))
    assert valid == c["valid"]
    assert norm_ref(verr) == c["valid_error"]
    LOG.clear()
    r, cerr = run(lambda: Anchored.create(*items))
    assert norm_ref(cerr) == c["error"]
    if c["result"] is None:
        assert r is None
    else:
        assert num_eq(describe(r), c["result"])
    same = 1 if r is not None and len(items) == 1 and r is items[0] else 0
    assert same == c["same_as_item"]
    assert LOG == c["log"]


def test_golden_create_subclass():
    ACT.update({"number": 0.1, "ascending": 0.5})
    inputs = mk_inputs()
    r = SubAnchored.create(*inputs["two"])
    c = one("create_subclass")
    assert util.perl_ref(r) == c["ref"]
    assert num_eq(describe(r), c["describe"])
    inputs = mk_inputs()
    assert util.perl_ref(SubAnchored.create(*inputs["one_elt"])) == one("create_subclass_one")["ref"]


# --- Extend / SafeExtend --------------------------------------------------------------------

EXT_CASES = {
    # name: (insert index, at_end, conflicts, number of supergroups)
    "end": (3, 1, None, 0), "start": (0, 0, None, 0), "hole_end": (4, 1, None, 0),
    "wrong_side": (0, 1, None, 0), "conflict_ok": (3, 1, 1, 0), "conflict_fail": (3, 1, 0, 0),
    "conflict_undef": (3, 1, "u", 0), "super_seed1": (3, 1, None, 2), "super_seed2": (3, 1, None, 2),
    "super_seed3": (3, 1, None, 2), "super_seed4": (3, 1, None, 2), "at_end_string0": (3, "0", None, 0),
    "at_end_empty": (0, "", None, 0),
}


def ext_fixture():
    e = [E(i + 1, i) for i in range(5)]
    g = Anchored(group_p=1, left_edge=1, right_edge=2, items=[e[1], e[2]])
    g.describe_as(S.ASCENDING)
    return g, e


@pytest.mark.parametrize("c", cases("extend"), ids=lambda c: f"{c['method']}-{c['name']}")
def test_golden_extend(c):
    ACT.update({"number": 0.1, "ascending": 0.5})
    idx, at_end, conf, nsuper = EXT_CASES[c["name"]]
    g, e = ext_fixture()
    LOG.clear()
    World.conflicts = None if conf is None else FakeConflicts(None if conf == "u" else conf)
    World.super_groups = [Anchored(group_p=1, left_edge=0, right_edge=2 + i) for i in range(1, nsuper + 1)]
    util.srand(c["seed"])
    method = {"Extend": g.extend, "SafeExtend": g.safe_extend}[c["method"]]
    r, error = run(lambda: method(e[idx], at_end))
    assert error == c["error"]
    assert (as_list(r) if error is None else []) == c["returned"]
    assert num_eq(describe(g), c["group"])
    assert LOG == c["log"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-15)


def test_golden_extend_cases_covered():
    assert {c["name"] for c in cases("extend")} == set(EXT_CASES)


@pytest.mark.parametrize("c", cases("extend_arity"), ids=lambda c: f"{c['method']}-{c['n']}")
def test_golden_extend_arity(c):
    g, e = ext_fixture()
    method = {"Extend": g.extend, "SafeExtend": g.safe_extend}[c["method"]]
    args = [e[3]] if c["n"] == 1 else [e[3], 1, 2]
    assert run(lambda: method(*args))[1] == c["error"]


@pytest.mark.parametrize("c", cases("safe_extend_rethrow"), ids=lambda c: c["die"].strip())
def test_golden_safe_extend_rethrow(c):
    g, e = ext_fixture()
    World.find_dies = SErr("serr boom") if c["die"] == "obj" else Confess(c["die"])
    r, error = run(lambda: g.safe_extend(e[3], 1))
    assert error == c["error"]
    assert c["returned"] == []


# --- Update ---------------------------------------------------------------------------------


@pytest.mark.parametrize("c", cases("update"), ids=lambda c: c["mode"])
def test_golden_update(c):
    ACT.update({"number": 0.1, "ascending": 0.5})
    e = [E(i + 1, i) for i in range(4)]
    g = TestGroup(group_p=1, left_edge=7, right_edge=9, items=e[0:3])
    g.describe_as(S.ASCENDING)
    if c["mode"] != "none":
        g.set_underlying_reln(FakeRuleApp(rule="RULE"))
        g.mode = c["mode"]
    LOG.clear()
    r, error = run(g.update)
    assert error == c["error"]
    assert (as_list(r) if error is None else []) == c["returned"]
    assert num_eq(describe(g), c["group"])
    assert (1 if util.perl_true(g.get_underlying_reln()) else 0) == c["has_reln"]
    assert LOG == c["log"]


def test_golden_update_lost_cats():
    ACT.update({"number": 0.1, "ascending": 0.5})
    c = one("update_lost_cats")
    e = [E(5, i) for i in range(3)]
    g = TestGroup(group_p=1, left_edge=0, right_edge=2, items=e)
    g.add_category(S.ASCENDING, SBindings.create({}, {}, g))
    LOG.clear()
    r, error = run(g.update)
    error["message"] = error["message"].split("!!! ")[0] + "!!!"
    assert error == c["error"]
    assert c["returned"] == []
    assert LOG == c["log"]


def test_update_does_not_swallow_unported_stubs(monkeypatch):
    g = TestGroup(group_p=1, left_edge=0, right_edge=1, items=[E(1, 0), E(2, 1)])
    g.describe_as(S.ASCENDING)
    g.set_underlying_reln(FakeRuleApp(rule="RULE"))

    def stub(rule):
        raise NotImplementedError("an unported stub")

    monkeypatch.setattr(g, "set_underlying_ruleapp", stub)
    with pytest.raises(NotImplementedError):
        g.update()


# --- FindExtension ----------------------------------------------------------------------------


def test_golden_find_extension():
    g = Anchored(group_p=1, left_edge=0, right_edge=1)
    found = cases("find_extension")
    assert as_list(g.find_extension(DIR.RIGHT, 0)) == found[0]["returned"]
    assert LOG == found[0]["log"]
    g.set_underlying_reln(FakeRuleApp())
    assert as_list(g.find_extension(DIR.LEFT, 2)) == found[1]["returned"]
    assert LOG == found[1]["log"]
    assert run(lambda: g.find_extension(DIR.LEFT))[1] == one("find_extension_arity")["error"]


# --- hooks ------------------------------------------------------------------------------------


def test_create_hooks_are_wired():
    e = numeric._element_create(3, 1)
    assert isinstance(e, Element) and e.as_text() == "Seqsee::Element:[1,1] 3"
    assert sameness._anchored_create(e) is e
    assert isinstance(object_mod._element_create(4, 0), Element)


def test_workspace_hooks_reach_the_workspace(monkeypatch):
    monkeypatch.undo()
    from seqsee import sworkspace
    sworkspace.init({"seq": [1, 2, 3]})
    e = sworkspace.get_elements()
    g = Anchored.create(e[0], e[1])
    # _element_count reads the real workspace since item 031.
    assert g.is_flush_right() == 0
    # The others since item 032.
    assert anchored_mod._find_groups_conflicting_with(g).challenger() is g
    anchored_mod._update_group(g)
    assert anchored_mod._get_super_groups(e[0]) == [g]
    assert sworkspace.LEFT_EDGE_OF[g] == 0 and sworkspace.SPAN_OF[g] == 2
    sworkspace.add_group(g)
    anchored_mod._remove_gp(g)
    assert not sworkspace.check_liveness(g)
    sworkspace.add_group(g)
    anchored_mod._delete_group(g)
    assert not sworkspace.check_liveness(g)

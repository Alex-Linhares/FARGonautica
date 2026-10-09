"""Tests for SRule and SRuleApp (item 025): Perl lib/SRule.pm and lib/SRuleApp.pm.

Golden data comes from oracle/srule.pl (tests/golden/srule.json). The oracle replaces
the parts that aren't ported yet with recorders, and the same recorders are wired in here
through hooks:
SWorkspace::__FindObjectSetDirection (the oracle's copy loops over get_left_edge, like
``srule._find_object_set_direction``), SWorkspace->GetSomethingLike and
check_at_location, $SWorkspace::ElementCount, SCoderack->add_codelet, SCodelet->new,
SLTM::GetRealActivationsForConcepts and main::message. FakeMapping mirrors the oracle's
package.
"""
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import mapping as mapping_mod
from seqsee import s as S
from seqsee import srule as srule_mod
from seqsee import srule_app as srule_app_mod
from seqsee import util
from seqsee.constants import DIR, METO_MODE
from seqsee.errors import Confess, ElementsBeyondKnownSought, ExceptionClassBase, SErr
from seqsee.mapping import Mapping
from seqsee.mapping.dir import MappingDir
from seqsee.mapping.numeric import MappingNumeric
from seqsee.mapping.structural import MappingStructural
from seqsee.objects import object as object_mod
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.srelation import SRelation
from seqsee.srule import SRule
from seqsee.srule_app import SRuleApp

CASES = golden.load("srule")


def cases(kind, **match):
    found = [c for c in CASES if c["kind"] == kind and all(c.get(k) == v for k, v in match.items())]
    assert found, (kind, match)
    return found


def one(kind, **match):
    found = cases(kind, **match)
    assert len(found) == 1, (kind, match)
    return found[0]


_REF_RE = re.compile(r"=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)")


def mask(s):
    return None if s is None else _REF_RE.sub("=REF", s)


def err(e):
    """The oracle's err(): Moose and plain dies are read as class DIE."""
    if e is None:
        return None
    msg = mask(e.message if isinstance(e, ExceptionClassBase) else str(e))
    return {"class": "SErr" if isinstance(e, SErr) else "DIE", "message": msg}


def golden_err(g):
    if g is None:
        return None
    cls = g["class"]
    if cls.startswith("Moose::Exception::"):
        cls = "DIE"
    return {"class": cls, "message": g["message"]}


def run(fn):
    """(result, exception) of a call."""
    try:
        return fn(), None
    except NotImplementedError:
        raise
    except Exception as e:  # noqa: BLE001
        return None, e


def plain(x):
    """Perl scalars as strings, for comparing numbers that Perl may hold as strings."""
    if isinstance(x, list):
        return [plain(i) for i in x]
    if x is None:
        return None
    return util.perl_str(x)


# --- recorders (the oracle's) --------------------------------------------------------------

LOG = []


class World:
    gsl = "FOUND"
    check = 0
    check_dies = None
    element_count = 6


def _direction_text(d):
    if d is None:
        return None
    return d.text if isinstance(d, DIR) else util.perl_str(d)


def _gsl(opts):
    LOG.append(["gsl", opts["object"].get_structure_string(), opts["start"],
                _direction_text(opts["direction"]), opts["trust_level"], opts["reason"],
                [x.as_text() for x in opts["hilit_set"]]])
    return World.gsl


def _check(opts):
    what = opts["what"]
    LOG.append(["check", opts["start"], _direction_text(opts["direction"]),
                None if what is None else what.get_structure_string()])
    if World.check_dies is not None:
        raise World.check_dies
    return World.check


def _scodelet_new(family, urgency, args):
    return [family, urgency, sorted(args), args["exception"]]


def _add_codelet(codelet):
    LOG.append(["add_codelet", *codelet])


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    LOG.clear()
    World.gsl, World.check, World.check_dies, World.element_count = "FOUND", 0, None, 6
    monkeypatch.setattr(srule_app_mod, "_get_something_like", _gsl)
    monkeypatch.setattr(srule_app_mod, "_check_at_location", _check)
    monkeypatch.setattr(srule_app_mod, "_element_count", lambda: World.element_count)
    monkeypatch.setattr(srule_app_mod, "_scodelet_new", _scodelet_new)
    monkeypatch.setattr(srule_app_mod, "_coderack_add_codelet", _add_codelet)
    monkeypatch.setattr(mapping_mod, "_message", lambda text: LOG.append(["message", text]))
    monkeypatch.setattr(object_mod, "_get_real_activations_for_concepts",
                        lambda cats: [0 for _ in cats])
    monkeypatch.setattr(mapping_mod, "_spike_and_choose", lambda *a: None, raising=False)


def take_log():
    out = [[mask(x) if isinstance(x, str) else x for x in entry] for entry in LOG]
    LOG.clear()
    return out


def same_log(got, expected):
    """Logs match, with numbers compared by their Perl string forms."""
    assert plain(got) == plain(expected)


class FakeMapping(Mapping):
    """The oracle's FakeMapping: a Mapping whose FlippedVersion and CheckSanity are scripted."""

    perl_name = "FakeMapping"

    def __init__(self, name, flip=None, sane=1):
        self.name, self.flip, self.sane = name, flip, sane

    def flipped_version(self):
        return self.flip

    def as_text(self):
        return "fake " + self.name

    def check_sanity(self):
        return self.sane


_NAME_RE = re.compile(r"a(\d+)_(\d+)")


class Objects(dict):
    """The oracle's O(): elements "a<mag>_<pos>" and groups "g(x,y)", built once by name."""

    def __missing__(self, name):
        m = _NAME_RE.fullmatch(name)
        if m:
            obj = Element.create(int(m.group(1)), int(m.group(2)))
        elif name.startswith("g(") and name.endswith(")"):
            obj = Anchored.create(*(self[n] for n in name[2:-1].split(",")))
        else:
            raise KeyError(name)
        self[name] = obj
        return obj


def world():
    obj = Objects()
    succ = MappingNumeric.create("succ", S.NUMBER)
    pred = MappingNumeric.create("pred", S.NUMBER)
    struct_args = {"category": S.ASCENDING, "meto_mode": METO_MODE.NONE,
                   "direction_reln": MappingDir.create("Same"), "slippages": {}}
    types = {
        "succ": succ,
        "pred": pred,
        "same": MappingNumeric.create("same", S.NUMBER),
        "noflip": FakeMapping("noflip"),
        "fakeflip": FakeMapping("y", flip=FakeMapping("yflip")),
        "insane": FakeMapping("z", flip=FakeMapping("zflip", sane=0)),
        "struct": MappingStructural.create({**struct_args, "changed_bindings": {"start": succ}}),
        # as_text lists bindings in Perl hash order; the oracle's (fixed seed) was end, start.
        "struct2": MappingStructural.create(
            {**struct_args, "changed_bindings": {"end": succ, "start": succ}}),
    }
    types["rel"] = SRelation({"first": obj["a1_0"], "second": obj["a2_1"], "type": succ})
    types["relpred"] = SRelation({"first": obj["a2_1"], "second": obj["a1_0"], "type": pred})
    return obj, types


def value(name, obj, types):
    if not isinstance(name, str):
        return name
    if name in types:
        return types[name]
    if name in ("RIGHT", "LEFT", "UNKNOWN"):
        return getattr(DIR, name)
    if name == "ARRAY":
        return []
    if re.match(r"a\d|g\(", name):
        return obj[name]
    return name


def text(x):
    if x is None:
        return None
    if hasattr(x, "as_text"):
        return x.as_text()
    return x


def rule_desc(r):
    return {"ref": r.perl_name, "as_text": r.as_text(), "transform": text(r.get_transform()),
            "flipped": text(r.get_flipped_transform())}


def app_desc(a):
    items = a.get_all_items()
    rule = a.get_rule()
    d = {
        "ref": a.perl_name,
        "as_text": mask(a.as_text()),
        "direction": _direction_text(a.get_direction()),
        "items": [text(i) for i in items],
        "rule": text(rule),
    }
    if items and not isinstance(items[0], (str, int)):
        d["edges"] = list(a.get_edges())
        d["span"] = a.get_span()
    return d


def same_app(got, expected):
    got, expected = dict(got), dict(expected)
    if "edges" in expected:
        assert plain(got.pop("edges")) == expected.pop("edges")
        assert plain(got.pop("span")) == plain(expected.pop("span"))
    assert got == expected


def kv(args, obj, types):
    return {args[i]: value(args[i + 1], obj, types) for i in range(0, len(args), 2)}


def ret_of(result, golden_n):
    """Perl's list-context count from a Python return value (None = empty list)."""
    return golden_n if result is not None else 0


# --- SRule->create ---------------------------------------------------------------------------

CREATE = [c for c in CASES if c["kind"] == "create" and "log" in c]


@pytest.mark.parametrize("case", CREATE, ids=[str(c["type"]) for c in CREATE])
def test_create(case):
    obj, types = world()
    take_log()
    v = value(case["type"], obj, types)
    r, e = run(lambda: SRule.create(v))
    assert err(e) == golden_err(case["err"])
    assert take_log() == case["log"]
    assert (0 if r is None else 1) == case["n"]
    if "rule" in case:
        assert rule_desc(r) == case["rule"]
        assert (1 if SRule.create(v) is r else 0) == case["memo"]


def test_create_relation_gives_rule_of_type():
    obj, types = world()
    c = one("create_rel_same")
    assert (1 if SRule.create(types["rel"]) is SRule.create(types["succ"]) else 0) == c["same"]


@pytest.mark.parametrize("which", ["NOARGS", "TWOARGS"])
def test_create_arity(which):
    obj, types = world()
    c = one("create", type=which)
    args = () if which == "NOARGS" else (types["succ"], types["pred"])
    r, e = run(lambda: SRule.create(*args))
    assert r is None
    assert err(e) == golden_err(c["err"])


def test_create_memo_survives_and_reset_clears():
    obj, types = world()
    r = SRule.create(types["succ"])
    assert SRule.create(types["succ"]) is r
    srule_mod.reset()
    assert SRule.create(types["succ"]) is not r


# --- SRule->new ------------------------------------------------------------------------------

@pytest.mark.parametrize("case", cases("rule_new"), ids=lambda c: "-".join(map(str, c["args"])) or "none")
def test_rule_new(case):
    obj, types = world()
    r, e = run(lambda: SRule(kv(case["args"], obj, types)))
    assert err(e) == golden_err(case["err"])
    if case["err"] is None:
        assert text(r.get_transform()) == case["transform"]
        assert text(r.get_flipped_transform()) == case["flipped"]
        t, te = run(r.as_text)
        assert t == case["as_text"]
        assert err(te) == golden_err(case["as_text_err"])


def test_rule_setters():
    obj, types = world()
    r = SRule({"transform": types["succ"], "flipped_transform": types["pred"]})
    r.set_transform(types["same"])
    r.set_flipped_transform(None)
    assert rule_desc(r) == one("rule_set")["rule"]


# --- CreateApplication -----------------------------------------------------------------------

@pytest.mark.parametrize("case", cases("create_app"), ids=lambda c: "-".join(map(str, c["args"])) or "none")
def test_create_application(case):
    obj, types = world()
    rule = SRule.create(types["succ"])
    a, e = run(lambda: rule.create_application(kv(case["args"], obj, types)))
    assert err(e) == golden_err(case["err"])
    if "app" in case:
        same_app(app_desc(a), case["app"])
        assert (1 if a.get_rule() is rule else 0) == case["rule_is"]


# --- SRuleApp->new ---------------------------------------------------------------------------

APP_NEW = cases("app_new")


@pytest.mark.parametrize("case", APP_NEW, ids=[str(i) for i in range(len(APP_NEW))])
def test_app_new(case):
    obj, types = world()
    h = kv(case["args"], obj, types)
    if h.get("rule") is types["succ"]:
        h["rule"] = types["succ"]   # the oracle passes the mapping itself (no type check)
    a, e = run(lambda: SRuleApp(h))
    if case["err"] is not None:
        assert a is None
        got, want = err(e), golden_err(case["err"])
        if "Seqsee::Element={" in want["message"]:
            # Moose dumps the whole object; only the start is portable.
            prefix = want["message"].split(" with value ")[0]
            assert got["message"].startswith(prefix + " with value ")
        else:
            assert got == want
        return
    assert e is None, e
    assert len(a.get_all_items()) == case["items_n"]
    assert _direction_text(a.get_direction()) == case["direction"]
    assert text(a.get_rule()) == case["rule"]


def test_app_new_kwargs():
    c = one("app_new_list")
    a, e = run(lambda: SRuleApp(rule=5, direction=DIR.RIGHT))
    assert e is None and (1 if a else 0) == c["ok"]


def test_app_accessors():
    obj, types = world()
    steps = iter(one("accessors")["steps"])
    rule = SRule.create(types["succ"])
    a = SRuleApp({"rule": rule, "direction": DIR.RIGHT, "items": [obj["a1_0"], obj["a2_1"]]})

    def check(label):
        s = next(steps)
        assert s[0] == label
        same_app(app_desc(a), s[1])

    check("init")
    a.push_item(obj["a3_2"])
    check("push")
    a.unshift_item(obj["a0_5"])
    check("unshift")
    a.set_items([obj["a2_1"]])
    check("set_items")
    s = next(steps)
    _, e = run(lambda: a.set_items(5))
    assert err(e) == golden_err(s[1])
    same_app(app_desc(a), s[2])
    a.set_direction(DIR.LEFT)
    check("set_direction")
    a.set_rule(SRule.create(types["pred"]))
    check("set_rule")
    s = next(steps)
    ref = a.get_items()
    assert [s[1], s[2]] == ["ARRAY", len(ref)]
    assert a.get_items() is ref
    empty = SRuleApp({"rule": rule, "direction": DIR.RIGHT})
    for label, fn in (("empty_edges", empty.get_edges), ("empty_span", empty.get_span)):
        s = next(steps)
        assert s[0] == label
        _, e = run(fn)
        assert err(e) == golden_err(s[1])


# --- CheckApplicability ----------------------------------------------------------------------

CHECK_APP = cases("check_app")


@pytest.mark.parametrize("case", CHECK_APP,
                         ids=[f"{c['type']}-{'-'.join(c['objects'])}" for c in CHECK_APP])
def test_check_applicability(case):
    obj, types = world()
    rule = SRule.create(types[case["type"]])
    objects = [obj[n] for n in case["objects"]]
    take_log()
    a, e = run(lambda: rule.check_applicability({"objects": objects}))
    assert err(e) == golden_err(case["err"])
    assert take_log() == case["log"]
    assert ret_of(a, 1) == case["n"]
    if "app" in case:
        same_app(app_desc(a), case["app"])
        assert (1 if a.get_rule() is rule else 0) == case["rule_is"]


def test_check_applicability_needs_objects():
    obj, types = world()
    _, e = run(lambda: SRule.create(types["succ"]).check_applicability({}))
    assert err(e) == golden_err(one("check_app_noobjects")["err"])


def test_find_object_set_direction():
    obj, _ = world()
    f = srule_mod._find_object_set_direction
    assert f(obj["a1_0"], obj["a1_3"]) is DIR.RIGHT
    assert f(obj["a1_3"], obj["a1_0"]) is DIR.LEFT
    assert f(obj["a1_0"], obj["a1_3"], obj["a1_1"]) is DIR.NEITHER
    assert f(obj["a1_0"], obj["a2_0"]) is DIR.UNKNOWN
    with pytest.raises(Confess, match="Need at least 2"):
        f(obj["a1_0"])


# --- FindExtension ---------------------------------------------------------------------------

FE = {
    "r3": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "RIGHT"}),
    "r3s0": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "RIGHT", "skip_this_many_elements": 0}),
    "r3s1": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "RIGHT", "skip_this_many_elements": 1}),
    "r3s2": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "RIGHT", "skip_this_many_elements": 2}),
    "r3s3": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "RIGHT", "skip_this_many_elements": 3}),
    "r3s9": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "RIGHT", "skip_this_many_elements": 9}),
    "l3": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "LEFT"}),
    "l3s1": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "LEFT", "skip_this_many_elements": 1}),
    "l3edge": ("succ", ["a1_0", "a2_1", "a3_2"], {"direction_to_extend_in": "LEFT"}),
    "pred_r": ("pred", ["a7_2", "a6_3", "a5_4"], {"direction_to_extend_in": "RIGHT"}),
    "pred_l": ("pred", ["a7_2", "a6_3", "a5_4"], {"direction_to_extend_in": "LEFT"}),
    "same_r": ("same", ["a3_1", "a3_2"], {"direction_to_extend_in": "RIGHT"}),
    "nodir": ("succ", ["a5_2", "a6_3", "a7_4"], {}),
    "unknown": ("succ", ["a5_2", "a6_3", "a7_4"], {"direction_to_extend_in": "UNKNOWN"}),
    "groups_num": ("succ", ["g(a1_0,a2_1)", "g(a2_2,a3_3)"], {"direction_to_extend_in": "RIGHT"}),
    "groups_struct": ("struct2", ["g(a1_0,a2_1)", "g(a2_2,a3_3)"], {"direction_to_extend_in": "RIGHT"}),
    "groups_struct_l": ("struct2", ["g(a2_2,a3_3)", "g(a3_4,a4_5)"], {"direction_to_extend_in": "LEFT"}),
    "fake_r": ("fakeflip", ["a5_2", "a6_3"], {"direction_to_extend_in": "RIGHT"}),
}


@pytest.mark.parametrize("case", cases("find_ext"), ids=lambda c: c["name"])
def test_find_extension(case):
    obj, types = world()
    t, names, opts = FE[case["name"]]
    opts = {k: value(v, obj, types) for k, v in opts.items()}
    a = SRuleApp({"rule": SRule.create(types[t]), "direction": DIR.RIGHT,
                  "items": [obj[n] for n in names]})
    take_log()
    r, e = run(lambda: a.find_extension(opts))
    assert err(e) == golden_err(case["err"])
    assert ret_of(r, 1) == case["n"]
    assert r == case["ret"]
    same_log(take_log(), case["log"])


def test_find_extension_odd_rules():
    obj, types = world()
    rule = SRule({"transform": types["succ"], "flipped_transform": None})
    a = SRuleApp({"rule": rule, "direction": DIR.RIGHT, "items": [obj["a5_2"], obj["a6_3"]]})
    take_log()

    def check(name, opts):
        c = one("find_ext_odd", name=name)
        r, e = run(lambda: a.find_extension(opts))
        assert err(e) == golden_err(c["err"])
        assert ret_of(r, 1) == c["n"] or (r is None and c["n"] == 1)
        same_log(take_log(), c["log"])

    check("noflip_LEFT", {"direction_to_extend_in": DIR.LEFT})
    check("noflip_RIGHT", {"direction_to_extend_in": DIR.RIGHT})
    rule.set_transform(5)
    check("strange", {"direction_to_extend_in": DIR.RIGHT})
    rule.set_transform("")
    check("strange_empty", {"direction_to_extend_in": DIR.RIGHT})
    World.gsl = None
    rule.set_transform(types["succ"])
    check("gsl_undef", {"direction_to_extend_in": DIR.RIGHT})


@pytest.mark.parametrize("case", cases("find_ext_count"), ids=lambda c: str(c["count"]))
def test_find_extension_trust_level(case):
    obj, types = world()
    World.element_count = case["count"]
    a = SRuleApp({"rule": SRule.create(types["succ"]), "direction": DIR.RIGHT,
                  "items": [obj["a5_2"], obj["a6_3"]]})
    take_log()
    a.find_extension({"direction_to_extend_in": DIR.RIGHT})
    same_log(take_log(), case["log"])


# --- Seqsee::Object integration --------------------------------------------------------------

SET_UNDERLYING = cases("set_underlying")


@pytest.mark.parametrize("case", SET_UNDERLYING, ids=[c["type"] for c in SET_UNDERLYING])
def test_set_underlying_ruleapp(case):
    obj, types = world()
    t = types[case["type"]]
    if case["type"] == "struct":
        # Perl memoizes FlippedVersion, and the oracle flipped this mapping earlier.
        t.flipped_version()
    g = Anchored.create(*(obj[n] for n in case["objects"]))
    take_log()
    r, e = run(lambda: g.set_underlying_ruleapp(t))
    assert err(e) == golden_err(case["err"])
    assert [mask(h) for h in g.get_history() if "Underlying" in h] == case["hist"]
    u = g.get_underlying_reln()
    assert (1 if u else 0) == case["has_reln"]
    assert take_log() == case["log"]
    if case["err"] is None and case["n"] == 1:
        assert r is u
    if u:
        same_app(app_desc(u), case["app"])
        find = []
        for d in ("RIGHT", "LEFT"):
            for skip in (0, 1):
                x, xe = run(lambda: g.find_extension(getattr(DIR, d), skip))
                find.append([d, skip, err(xe), ret_of(x, 1), take_log()])
        assert plain(find) == plain(case["find"])


# --- CheckConsitencyOfGroup ------------------------------------------------------------------

def test_check_consistency_of_group():
    obj, types = world()
    ga = obj["g(a1_30,a2_31)"]
    gb = obj["g(a3_32,a4_33)"]
    nested = Anchored.create(ga, gb)
    gc = obj["g(a5_34,a6_35)"]
    app = SRuleApp({"rule": SRule.create(types["struct2"]), "direction": DIR.RIGHT,
                    "items": [nested, gc]})
    own = Anchored.create(obj["a2_31"], obj["a3_32"])
    groups = {
        "nested": nested, "ga": ga, "gb": gb, "gc": gc, "straddle": own,
        "outside": obj["g(a6_35,a7_36)"], "left_out": obj["g(a9_29,a1_30)"],
        "whole": Anchored.create(nested, gc), "elem": obj["a1_30"],
    }
    for name, group in groups.items():
        c = one("consistency", group=name)
        r, e = run(lambda: app.check_consitency_of_group(group))
        assert err(e) == golden_err(c["err"]), name
        assert r == c["ret"], name
    own.set_underlying_reln(app)
    assert app.check_consitency_of_group(own) == one("consistency", group="straddle_own")["ret"]


# --- Extend* ---------------------------------------------------------------------------------

EXTEND = {
    "fwd_no": ("extend_forward", None, 0, None, None),
    "fwd2_no": ("extend_forward", 2, 0, None, None),
    "fwd0": ("extend_forward", 0, 0, None, None),
    "fwd_yes": ("extend_forward", 1, 1, None, None),
    "back_no": ("extend_backward", 1, 0, None, None),
    "back_yes": ("extend_backward", 1, 1, None, None),
    "right_no": ("extend_right", 1, 0, None, None),
    "left_no": ("extend_left", 1, 0, None, None),
    "left_max": ("extend_left_maximally", None, 0, None, None),
    "fwd_die_str": ("extend_forward", 1, 0, "boom\n", None),
    "fwd_die_serr": ("extend_forward", 1, 0, "SErr", None),
    "pred_no": ("extend_forward", 1, 0, None, "pred"),
    "fake_no": ("extend_forward", 1, 0, None, "fakeflip"),
}


def mkapp(obj, types, t=None):
    return SRuleApp({"rule": SRule.create(types[t or "succ"]), "direction": DIR.RIGHT,
                     "items": [obj["a1_0"], obj["a2_1"], obj["a3_2"]]})


@pytest.mark.parametrize("case", cases("extend"), ids=lambda c: c["name"])
def test_extend(case):
    obj, types = world()
    method, steps, check, dies, t = EXTEND[case["name"]]
    World.check = check
    if dies == "SErr":
        World.check_dies = SErr("ws err")
    elif dies is not None:
        World.check_dies = Confess(dies)
    a = mkapp(obj, types, t)
    take_log()
    args = () if method == "extend_left_maximally" else (steps,)
    r, e = run(lambda: getattr(a, method)(*args))
    assert err(e) == golden_err(case["err"])
    # Perl's list-context result: () for a bare return, (undef) for ExtendLeftMaximally.
    assert r == (case["ret"][0] if case["ret"] else None)
    s, _ = run(lambda: getattr(mkapp(obj, types, t), method)(*args))
    assert s == case["scalar"]
    same_app(app_desc(a), case["app"])
    same_log(take_log(), case["log"])


@pytest.mark.parametrize("case", cases("extend_beyond"), ids=lambda c: f"{c['seed']}-{c['count']}")
def test_extend_elements_beyond_known(case):
    obj, types = world()
    World.element_count = case["count"]
    World.check_dies = ElementsBeyondKnownSought(next_elements=[4])
    a = mkapp(obj, types)
    take_log()
    util.srand(case["seed"])
    r, e = run(lambda: a.extend_forward(2))
    assert err(e) == golden_err(case["err"])
    assert r is None and case["ret"] == []
    log = take_log()
    for entry in log:
        if entry[0] == "add_codelet":
            assert entry[3] == ["core", "exception"]
    same_log(log, case["log"])
    assert util.perl_str(util.rand()) == util.perl_str(case["next_rand"])


EXTEND_ONE = [
    {}, {"items_ref": []}, {"items_ref": [], "direction_to_extend_in": "RIGHT"},
    {"items_ref": [], "direction_to_extend_in": "RIGHT", "object_at_end": "a1_0"},
    {"items_ref": [], "direction_to_extend_in": "RIGHT", "object_at_end": "a1_0", "transform": "succ"},
    {"items_ref": [], "direction_to_extend_in": "RIGHT", "object_at_end": "a1_0", "transform": "succ",
     "extend_at_start_or_end": "middle"},
]


@pytest.mark.parametrize("opts", EXTEND_ONE, ids=lambda o: str(len(o)))
def test_extend_one_step_options(opts):
    obj, types = world()
    c = one("extend_one", keys=sorted(opts))
    o = {k: value(v, obj, types) for k, v in opts.items()}
    take_log()
    r, e = run(lambda: srule_app_mod._extend_one_step(o))
    assert err(e) == golden_err(c["err"])
    assert ret_of(r, 1) == c["n"]
    same_log(take_log(), c["log"])


def test_plonk_is_undefined_in_srule_app():
    """PERL-QUIRK: SRuleApp.pm never imports the __PlonkIntoPlace multimethod."""
    with pytest.raises(Confess, match=r"Undefined subroutine &SRuleApp::__PlonkIntoPlace called"):
        srule_app_mod._plonk_into_place(1, DIR.RIGHT, None)


def test_extend_zero_steps_updates_consistency():
    obj, types = world()
    a = mkapp(obj, types)
    Global.BestRuleApp = a
    assert a.extend_forward(0) == 1
    assert Global.GroupStrengthByConsistency[obj["a1_0"]] == 40


def test_as_text_has_address():
    obj, types = world()
    a = mkapp(obj, types)
    assert re.fullmatch(r"SRuleApp SRuleApp=HASH\(0x[0-9a-f]+\)", a.as_text())


def test_object_hook_is_wired():
    obj, types = world()
    assert object_mod._srule_create(types["succ"]) is SRule.create(types["succ"])

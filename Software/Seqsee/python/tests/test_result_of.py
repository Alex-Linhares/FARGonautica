"""Tests for the result objects (item 026): Perl lib/Seqsee/ResultOfAttributeCopy.pm,
lib/Seqsee/ResultOfPlonk.pm, lib/Seqsee/ResultOfGetConflicts.pm and
lib/Seqsee/ResultOfGetSomethingLike.pm. (ResultOfCanBeSeenAs.pm is tested in
test_seqsee_object2.py; ResultOfTestRun.pm belongs to item 049.)

Golden data comes from oracle/result_of.pl (tests/golden/result_of.json). The oracle
replaces SWorkspace->FightUntoDeath and SWorkspace::__CheckLiveness with scripted
recorders; here the same recorders go in through the hooks in
``result_of_get_conflicts``.
"""
import gc

import pytest

import golden
from seqsee.constants import DIR
from seqsee.errors import Confess
from seqsee.objects import object as object_mod
from seqsee.objects import result_of_get_conflicts as conflicts_mod
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.objects.result_of_attribute_copy import ResultOfAttributeCopy
from seqsee.objects.result_of_get_conflicts import ResultOfGetConflicts
from seqsee.objects.result_of_get_something_like import ResultOfGetSomethingLike
from seqsee.objects.result_of_plonk import ResultOfPlonk
from seqsee.sbindings import SBindings
from seqsee import util

CASES = golden.load("result_of")


def cases(kind, **match):
    found = [c for c in CASES if c["kind"] == kind and all(c.get(k) == v for k, v in match.items())]
    assert found, (kind, match)
    return found


def one(kind, **match):
    found = cases(kind, **match)
    assert len(found) == 1, (kind, match)
    return found[0]


def ids(kind, key="name"):
    return [c[key] for c in cases(kind)]


# --- the oracle's world --------------------------------------------------------------------

LOG = []


class World:
    wins = {}
    dead = set()


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    LOG.clear()
    World.wins, World.dead = {}, set()
    # Group creation runs Seqsee::Object's UpdateStrength (real SLTM in the oracle).
    monkeypatch.setattr(object_mod, "_get_real_activations_for_concepts",
                        lambda cats: [0 for _ in cats])
    monkeypatch.setattr(conflicts_mod, "_fight_unto_death", _fight)
    monkeypatch.setattr(conflicts_mod, "_check_liveness", _live)


def E(mag, pos):
    return Element.create(mag, pos)


def G(*items):
    return Anchored.create(*items)


OBJ = {}
NAME = {}


@pytest.fixture(autouse=True)
def world(recorders):
    OBJ.clear()
    NAME.clear()
    for name, mag, pos in (("e0", 5, 0), ("e1", 6, 1), ("e2", 7, 2), ("e3", 8, 3), ("e4", 9, 4)):
        OBJ[name] = E(mag, pos)
    OBJ["gA"] = G(OBJ["e0"], OBJ["e1"])
    OBJ["gB"] = G(OBJ["e1"], OBJ["e2"])
    OBJ["gC"] = G(OBJ["e2"], OBJ["e3"])
    OBJ["gD"] = G(OBJ["e3"], OBJ["e4"])
    NAME.update({id(v): k for k, v in OBJ.items()})
    yield
    OBJ.clear()
    NAME.clear()


def name_of(v):
    if v is None or util._is_scalar(v):
        return v
    return NAME.get(id(v), "unnamed " + util.perl_ref(v))


def V(v):
    if v is None:
        return None
    if isinstance(v, list):
        return [V(x) for x in v]
    if not isinstance(v, str):
        return v
    if v in OBJ:
        return OBJ[v]
    return {"ARRAY": lambda: [], "HASH": lambda: {}, "DIR": lambda: DIR.RIGHT,
            "COPY": ResultOfAttributeCopy, "SBindings": SBindings}.get(v, lambda: v)()


def _fight(opts):
    c, i = name_of(opts["challenger"]), name_of(opts["incumbent"])
    LOG.append(["fight", c, i])
    return World.wins.get(i, 1)


def _live(*objects):
    LOG.append(["live", *[name_of(o) for o in objects]])
    return not any(name_of(o) in World.dead for o in objects)


def run(code):
    """(value, error dict) as the oracle's err() records them."""
    try:
        return code(), None
    except Confess as e:
        return None, str(e)


def check_error(expected, got, prefix_only=False):
    if expected is None:
        assert got is None
        return
    assert got is not None, expected
    if prefix_only:
        cut = expected["message"].index(" with value ") + len(" with value ")
        assert got.startswith(expected["message"][:cut]), (got, expected)
    else:
        assert got == expected["message"]


def bool_of(x):
    return 1 if util.perl_true(x) else 0


# --- ResultOfAttributeCopy -----------------------------------------------------------------

COPY_NEW_ARGS = {
    "none": ((), {}),
    "s1": ((), {"success": 1}),
    "s0": ((), {"success": 0}),
    "sundef": ((), {"success": None}),
    "sempty": ((), {"success": ""}),
    "s5": ((), {"success": 5}),
    "sx": ((), {"success": "x"}),
    "sarray": ((), {"success": []}),
    "hashref0": (({"success": 0},), {}),
    "extra": ((), {"success": 0, "foo": 3}),
}


@pytest.mark.parametrize("name", ids("copy_new"))
def test_copy_new(name):
    case = one("copy_new", name=name)
    args, kwargs = COPY_NEW_ARGS[name]
    obj, error = run(lambda: ResultOfAttributeCopy(*args, **kwargs))
    check_error(case["error"], error)
    if obj is not None:
        assert obj.success() == case["success"]
        assert (obj.success() is not None) == bool(case["defined"])
        assert bool_of(obj) == case["bool"]


def test_copy_ctors():
    case = one("copy_ctors")
    s1, s2 = ResultOfAttributeCopy.Success(), ResultOfAttributeCopy.Success()
    f1, f2 = ResultOfAttributeCopy.Failed(), ResultOfAttributeCopy.Failed()
    assert [x.success() for x in (s1, s2, f1, f2)] == case["success"]
    assert (1 if s1 is s2 else 0) == case["same_s"]
    assert (1 if f1 is f2 else 0) == case["same_f"]
    assert bool_of(f1) == case["bool_fail"]   # no bool overload: a failed copy is true
    assert util.perl_ref(f1) == case["ref"]


@pytest.mark.parametrize("case", cases("copy_set"), ids=lambda c: repr(c["value"]))
def test_copy_set(case):
    obj = ResultOfAttributeCopy()
    ret, error = run(lambda: obj.success(case["value"]))
    check_error(case["error"], error)
    assert ret == case["ret"]
    assert (ret is not None) == bool(case["ret_def"])
    assert obj.success() == case["after"]
    assert (obj.success() is not None) == bool(case["after_def"])


@pytest.mark.parametrize("case", cases("copy_update"),
                         ids=lambda c: f"{c['mine']!r}-{c['theirs']!r}")
def test_copy_update_with(case):
    obj = ResultOfAttributeCopy(success=case["mine"])
    other = None if case["theirs"] == "UNDEF_OBJECT" else ResultOfAttributeCopy(success=case["theirs"])
    ret, error = run(lambda: obj.update_with(other))
    check_error(case["error"], error)
    assert ret == case["ret"]
    assert (ret is not None) == bool(case["ret_def"])
    assert obj.success() == case["after"]
    assert (obj.success() is not None) == bool(case["after_def"])
    if other is not None:
        assert other.success() == case["other"]


def test_copy_update_with_undef_message_is_perls():
    with pytest.raises(Confess, match=r'^Can\'t call method "success" on an undefined value$'):
        ResultOfAttributeCopy().update_with(None)


# --- ResultOfPlonk ---------------------------------------------------------------------------

def describe_plonk(p):
    return {
        "object": name_of(p.object_being_plonked()),
        "resultant": name_of(p.resultant_object()),
        "has": p.has_resultant_object(),
        "success": p.plonk_was_successful(),
        "bool": bool_of(p),
        "str": util.perl_str(p),
        "copy_ok": p.attribute_copy_was_successful(),
        "copy_ref": util.perl_ref(p.attribute_copy_result()),
    }


@pytest.mark.parametrize("case", cases("plonk_new"), ids=lambda c: f"{c['form']}-{c['name']}")
def test_plonk_new(case):
    args = {k: V(v) for k, v in case["args"].items()}
    if case["form"] == "list":
        p, error = run(lambda: ResultOfPlonk(**args))
    else:
        p, error = run(lambda: ResultOfPlonk(args))
    # Moose dumps a whole object that fails a type check; compare up to the value.
    check_error(case["error"], error, prefix_only=case["name"] == "copy_obj")
    if p is not None:
        assert describe_plonk(p) == case["result"]


@pytest.mark.parametrize("case", cases("plonk_failed"), ids=lambda c: repr(c["what"]))
def test_plonk_failed(case):
    p, error = run(lambda: ResultOfPlonk.Failed(V(case["what"])))
    check_error(case["error"], error)
    if p is not None:
        assert describe_plonk(p) == case["result"]
        assert p.attribute_copy_result().success() == case["copy_success"]


def test_plonk_writers():
    case = one("plonk_writers")
    p = ResultOfPlonk.Failed(OBJ["gA"])
    steps = {
        "set_res_gB": lambda: p.resultant_object(OBJ["gB"]),
        "set_res_5": lambda: p.resultant_object(5),
        "set_res_undef": lambda: p.resultant_object(None),
        "set_obj_gC": lambda: p.object_being_plonked(OBJ["gC"]),
        "set_obj_x": lambda: p.object_being_plonked("x"),
        "copy_ok_set_1": lambda: p.attribute_copy_was_successful(1),
        "copy_ok_set_5": lambda: p.attribute_copy_was_successful(5),
        "set_copy_failed": lambda: p.attribute_copy_result(ResultOfAttributeCopy.Failed()),
        "set_copy_gA": lambda: p.attribute_copy_result(OBJ["gA"]),
        "set_copy_undef": lambda: p.attribute_copy_result(None),
    }
    for step in case["steps"]:
        ret, error = run(steps[step["label"]])
        check_error(step["error"], error, prefix_only=step["label"] == "set_copy_gA")
        assert (name_of(ret) if ret is not None and not util._is_scalar(ret) else ret) == step["ret"], step
        assert describe_plonk(p) == step["state"], step["label"]


def test_plonk_weak_refs():
    """object_being_plonked and resultant_object are weak refs: a group nobody else holds
    goes away (Perl frees it at once; Python may need the cycle collector). The predicate
    stays true, so the plonk still counts as successful."""
    case = one("plonk_weak")
    p = ResultOfPlonk(object_being_plonked=G(E(1, 7), E(2, 8)),
                      resultant_object=G(E(1, 7), E(2, 8)),
                      attribute_copy_result=ResultOfAttributeCopy.Failed())
    gc.collect()
    assert (1 if p.object_being_plonked() is not None else 0) == case["obj_def"]
    assert (1 if p.resultant_object() is not None else 0) == case["res_def"]
    assert p.has_resultant_object() == case["has"]
    assert bool_of(p) == case["bool"]
    assert (1 if p.attribute_copy_result() is not None else 0) == case["copy_def"]


# --- ResultOfGetConflicts --------------------------------------------------------------------

def describe_conflicts(c):
    exact = c.exact_conflict()
    out = {
        "challenger": name_of(c.challenger()),
        "exact": name_of(exact),
        "exact_def": 1 if exact is not None else 0,
        "has": c.has_overlapping_conflicts(),
        "count": c.overlapping_conflict_count(),
        "all": [name_of(x) for x in c.all_overlapping_conflicts()],
        "bool": bool_of(c),
    }
    if exact is None or util._is_scalar(exact):
        out["str"] = util.perl_str(c)
    return out


@pytest.mark.parametrize("case", cases("conflicts_new"), ids=lambda c: f"{c['form']}-{c['name']}")
def test_conflicts_new(case):
    args = {k: V(v) for k, v in case["args"].items()}
    if case["form"] == "list":
        c, error = run(lambda: ResultOfGetConflicts(**args))
    else:
        c, error = run(lambda: ResultOfGetConflicts(args))
    check_error(case["error"], error, prefix_only=case["name"] == "over_bad_item")
    if c is not None:
        assert describe_conflicts(c) == case["result"]


@pytest.mark.parametrize("case", cases("conflicts_ro"), ids=lambda c: c["method"])
def test_conflicts_read_only(case):
    c = ResultOfGetConflicts(challenger=OBJ["gA"], overlapping_conflicts=[OBJ["gB"]])
    _, error = run(lambda: getattr(c, case["method"])(OBJ["gC"]))
    check_error(case["error"], error)


def test_conflicts_list_is_the_objects_own():
    case = one("conflicts_list_ref")
    c = ResultOfGetConflicts(challenger=OBJ["gA"], overlapping_conflicts=[OBJ["gB"]])
    c.overlapping_conflicts().append(OBJ["gC"])
    assert c.overlapping_conflict_count() == case["count"]
    assert [name_of(x) for x in c.all_overlapping_conflicts()] == case["all"]


def test_conflicts_weak_refs():
    """challenger and exact_conflict are weak; overlapping_conflicts is not.

    Port difference: Perl also weakens an array ref given as exact_conflict (it is undef
    afterwards); Python lists can't be weakly referenced, so the port keeps it."""
    case = one("conflicts_weak")
    c = ResultOfGetConflicts(challenger=G(E(1, 7), E(2, 8)),
                             overlapping_conflicts=[G(E(1, 7), E(2, 8))])
    gc.collect()
    assert (1 if c.challenger() is not None else 0) == case["chal_def"]
    assert c.overlapping_conflict_count() == case["count"]
    assert bool_of(c) == case["bool"]

    case2 = one("conflicts_weak2")
    c2 = ResultOfGetConflicts(challenger=OBJ["gA"], exact_conflict=G(E(1, 7), E(2, 8)))
    gc.collect()
    assert (1 if c2.exact_conflict() is not None else 0) == case2["exact_def"]
    assert bool_of(c2) == case2["bool"]


@pytest.mark.parametrize("case", cases("resolve"), ids=lambda c: c["name"])
def test_resolve(case):
    spec = case["spec"]
    args = {"challenger": OBJ["gA"]}
    if "exact" in spec:
        args["exact_conflict"] = V(spec["exact"])
    if spec.get("over"):
        args["overlapping_conflicts"] = V(spec["over"])
    c = ResultOfGetConflicts(args)
    opts = None if case["opts"] is None else {k: V(v) for k, v in case["opts"].items()}
    World.wins = dict(case["wins"])
    World.dead = set(case["dead"])
    ret, error = run(lambda: c.resolve(opts))
    check_error(case["error"], error)
    assert ret == case["ret"]
    assert (ret is not None) == bool(case["ret_def"])
    assert LOG == case["log"]


def test_resolve_hooks_reach_the_workspace(monkeypatch):
    """Item 032: FightUntoDeath and __CheckLiveness are real. gB isn't in the (empty)
    workspace: a dead incumbent loses without a fight, and dead overlapping conflicts are
    skipped."""
    monkeypatch.undo()
    c = ResultOfGetConflicts(challenger=OBJ["gA"], exact_conflict=OBJ["gB"])
    assert c.resolve() == 1
    assert conflicts_mod._check_liveness(OBJ["gB"]) is False
    c = ResultOfGetConflicts(challenger=OBJ["gA"], overlapping_conflicts=[OBJ["gB"]])
    assert c.resolve() == 1


# --- ResultOfGetSomethingLike ----------------------------------------------------------------

GSL_ATTRS = ("to_ask", "literally_present", "probable_matches", "potential_matches")
GSL = "Seqsee::ResultOfGetSomethingLike"
GSL_ARGS = {
    "full": ({"to_ask": "a", "literally_present": "b", "probable_matches": "c",
              "potential_matches": "d"},),
    "undefs": ({k: None for k in GSL_ATTRS},),
    "extra": ({"to_ask": 1, "literally_present": 2, "probable_matches": 3,
               "potential_matches": 4, "foo": 5},),
    "nested": ({GSL: {"to_ask": 1, "literally_present": 2, "probable_matches": 3,
                      "potential_matches": 4}},),
    "nested_override": ({"to_ask": 1, "literally_present": 2, "probable_matches": 3,
                         "potential_matches": 4, GSL: {"to_ask": 9}},),
    "one_missing": ({"to_ask": 1, "literally_present": 2, "probable_matches": 3},),
    "three_missing": ({"to_ask": 1},),
    "two_missing_two_keys": ({"to_ask": 1, "potential_matches": 4},),
    "all_missing_one_key": ({"foo": 1},),
    "empty": ({},),
    "no_args": (),
    "undef_arg": (None,),
    "number": (5,),
    "array": ([],),
}


def _mislabel_names(message):
    import re
    m = re.search(r"passed: (.*)\?\)", message)
    return sorted(re.findall(r"'(\w+)'", m.group(1))) if m else []


@pytest.mark.parametrize("name", ids("gsl_new"))
def test_gsl_new(name):
    case = one("gsl_new", name=name)
    s, error = run(lambda: ResultOfGetSomethingLike(*GSL_ARGS[name]))
    if name == "two_missing_two_keys":
        # The mislabel names come in Perl hash order; compare everything else exactly.
        assert error is not None
        assert _mislabel_names(error) == _mislabel_names(case["error"]["message"])
        strip = lambda m: m.split("(Did")[0]   # noqa: E731
        assert strip(error) == strip(case["error"]["message"])
        assert error.endswith("Fatal error in constructor call")
    else:
        check_error(case["error"], error)
    if s is not None:
        assert [getattr(s, "get_" + a)() for a in GSL_ATTRS] == case["values"]


def test_gsl_mislabel_two_keys():
    case = one("gsl_mislabel_two_keys")
    _, error = run(lambda: ResultOfGetSomethingLike({"foo": 1, "bar": 2}))
    lines = error.split("\n")
    assert [ln for ln in lines if ln.startswith("Missing ")] == case["missing_lines"]
    assert _mislabel_names(error) == case["names"]
    assert lines[-1] == case["tail"]


def test_gsl_setters():
    case = one("gsl_setters")
    s = ResultOfGetSomethingLike({"to_ask": "old", "literally_present": "b",
                                  "probable_matches": "c", "potential_matches": "d"})
    for step in case["steps"]:
        a = step["attr"]
        ret = getattr(s, "set_" + a)("new_" + a)
        assert (1 if ret is s else 0) == step["ret_is_self"]
        assert ret == step["ret"]
        assert getattr(s, "get_" + a)() == step["after"]
    _, error = run(lambda: s.set_to_ask())
    check_error(case["set_no_value"], error)
    s.set_to_ask(None)
    assert s.get_to_ask() == case["set_undef_after"]
    _, error = run(lambda: s.get_to_ask(5))
    check_error(case["get_with_arg"], error)
    assert s.get_to_ask() is None

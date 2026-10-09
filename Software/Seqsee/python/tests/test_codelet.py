"""Tests for the codelet machinery: SCodeletBase.pm, SCodelet.pm, SAction.pm, MooseX/SCF.pm
(Codelet_Family, ACTION) and Seqsee/SCF.pm (ContinueWith).

Golden data: oracle/codelet.pl → tests/golden/codelet.json. The oracle defines two test
families, Seqsee::SCF::TFam and Seqsee::SCF::Empty; the ``families`` fixture defines the
same ones here.
"""
import logging

import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scodelet_base, sltm, srule_app, sworkspace, util
from seqsee.codelets import family as scf_family
from seqsee.codelets.family import FAMILIES, action, codelet_family, define_codelet_family
from seqsee.codelets.scf import continue_with
from seqsee.errors import Confess
from seqsee.mapping.numeric import MappingNumeric
from seqsee.objects.anchored import Anchored
from seqsee.saction import SAction
from seqsee.scodelet import SCodelet
from seqsee.srelation import SRelation

CASES = golden.load("codelet")


def cases(name):
    found = [c for c in CASES if c["case"] == name]
    assert found, name
    return found


def case(name):
    (c,) = cases(name)
    return c


BODY_LOG = []
MESSAGES = []
NAME_OF = {}


def v(x):
    """The oracle's v(): names for known objects, the ref type for other refs."""
    if x is None or isinstance(x, (str, int, float)):
        return x
    if id(x) in NAME_OF:
        return NAME_OF[id(x)]
    if isinstance(x, list):
        return "ARRAY"
    if isinstance(x, dict):
        return "HASH"
    return type(x).__name__


@pytest.fixture(autouse=True)
def families(monkeypatch):
    # Load the real families first: a lazy load during the test would be forgotten by the
    # restore below, and an imported module never registers again.
    scf_family.load_families()
    saved = dict(FAMILIES)

    @codelet_family("TFam", attributes=[
        ("a", {"required": 1}),
        ("b", {"default": 0}),
        ("c", {"optional": 1}),
        ("d", {}),
        ("e", {"defualt": ""}),
    ])
    def tfam(*args):
        BODY_LOG.append([v(x) for x in args])
        return "ret"

    @codelet_family("Empty", attributes=[])
    def empty(*args):
        BODY_LOG.append([len(args)])
        return 7

    monkeypatch.setattr(scodelet_base, "_message", lambda m: MESSAGES.append(list(m)))
    reset_state()
    yield
    FAMILIES.clear()
    FAMILIES.update(saved)


def reset_state():
    Global.Steps_Finished = 0
    sltm.clear()
    sworkspace.init({"seq": [1, 2, 3, 4, 5, 6]})
    Global.Feature.clear()
    Global.debugMAX = None
    Global.CurrentCodelet = None
    Global.CurrentCodeletFamily = None
    BODY_LOG.clear()
    MESSAGES.clear()
    NAME_OF.clear()
    util.srand(1)


def elements():
    es = sworkspace.get_elements()
    for i, e in enumerate(es):
        NAME_OF[id(e)] = f"e{i}"
    return es


def attempt(code):
    try:
        return v(code()), None
    except Confess as e:
        return None, str(e)


def codelet_view(c):
    a = list(c)
    return [a[0], a[1], a[2], sorted(a[3]) if isinstance(a[3], dict) else v(a[3])]


# ---- SCodelet->new, @{}, as_text ------------------------------------------------------------
NEW_SPECS = {
    "new_basic": ("TFam", 50, {"a": 1}),
    "new_no_args": ("Foo", 10),
    "new_empty_str": ("Foo", 10, ""),
    "new_zero_args": ("Foo", 10, 0),
    "new_float_urg": ("Foo", 0.5, {"x": "y"}),
    "new_undef_urg": ("Foo", None, {"x": None}),
    "new_num_family": (42, 3, {"x": 2}),
}


@pytest.mark.parametrize("name", sorted(NEW_SPECS))
def test_new_golden(name):
    for c in cases(name):
        reset_state()
        Global.Steps_Finished = c["steps"]
        cl = SCodelet(*NEW_SPECS[name])
        assert codelet_view(cl) == c["view"]
        assert cl.as_text() == c["as_text"]
        assert cl.family == c["family"]
        assert cl.urgency == c["urgency"]
        assert cl.creation_time == c["ctime"]


def test_as_text_golden():
    e = elements()
    assert SCodelet("Foo", 20, {"obj": e[2]}).as_text() == case("as_text_object")["as_text"]
    assert SCodelet("Foo", 20, {"items": [e[1], e[2]]}).as_text() == case("as_text_array")["as_text"]
    assert SCodelet("Foo", 20, {"n": 3}).as_text() == case("as_text_number")["as_text"]


def test_new_errors_golden():
    c = case("new_undef_family")
    assert attempt(lambda: SCodelet(None, 5) and 1) == (c["ok"], c["error"])
    c = case("new_only_family")
    assert attempt(lambda: SCodelet("X") and 1) == (c["ok"], c["error"])


def test_indexing_like_perl_overload():
    Global.Steps_Finished = 4
    args = {"a": 1}
    cl = SCodelet("Foo", 30, args)
    assert cl[0] == "Foo" and cl[1] == 30 and cl[2] == 4 and cl[3] is args
    assert cl.arguments is args


# ---- run: validated_list and the body ---------------------------------------------------------
RUN_SPECS = {
    "all_given": ("TFam", {"a": 1, "d": 4, "e": 5}),
    "missing_a": ("TFam", {"d": 4, "e": 5}),
    "missing_e": ("TFam", {"a": 1, "d": 4}),
    "missing_many": ("TFam", {}),
    "extra_one": ("TFam", {"a": 1, "d": 4, "e": 5, "zz": 1}),
    "extra_missing": ("TFam", {"zz": 1}),
    "optional_set": ("TFam", {"a": 1, "d": 4, "e": 5, "c": 3, "b": 7}),
    "undef_values": ("TFam", {"a": None, "d": None, "e": None, "b": None}),
    "empty_family": ("Empty", {}),
    "empty_extra": ("Empty", {"x": 1}),
    "unknown_fam": ("Nope", {}),
    "unknown_args": ("Nope", {"x": 1}),
}


@pytest.mark.parametrize("name", sorted(RUN_SPECS))
def test_run_golden(name):
    c = case(f"run_{name}")
    fam, args = RUN_SPECS[name]
    cl = SCodelet(fam, 50, args)
    assert attempt(cl.run) == (c["ret"], c["error"])
    assert BODY_LOG == c["body"]
    assert Global.CurrentCodeletFamily == c["current_family"]
    assert (1 if Global.CurrentCodelet is cl else 0) == c["current_is_self"]


def test_run_extra_two_golden():
    # The XS validator names one unknown parameter (Perl: in hash order).
    c = case("run_extra_two")
    cl = SCodelet("TFam", 50, {"a": 1, "d": 4, "e": 5, "zz": 1, "yy": 2})
    _, err = attempt(cl.run)
    prefix = c["error"].rsplit(" ", 1)[0]
    assert err.rsplit(" ", 1)[0] == prefix
    assert err.rsplit(" ", 1)[1] in ("yy", "zz")


@pytest.mark.parametrize("name,args", [("array", [1]), ("string", "5")])
def test_run_nonhash_golden(name, args):
    c = case(f"run_nonhash_{name}")
    cl = SCodelet("TFam", 50, args)
    assert attempt(cl.run) == (None, c["error"])
    assert BODY_LOG == c["body"]
    assert Global.CurrentCodeletFamily == c["current_family"]


# ---- freshness --------------------------------------------------------------------------------
def test_fresh_steps_empty_string_golden():
    Global.Steps_Finished = ""
    cl = SCodelet("TFam", 50, {"a": 1, "d": 2, "e": 3})
    c = case("fresh_steps_empty_string")
    assert attempt(cl.run) == (c["ret"], c["error"])
    assert BODY_LOG == c["body"]
    BODY_LOG.clear()
    c = case("fresh_steps_empty_string_noargs")
    assert attempt(SCodelet("Empty", 50, {}).run) == (c["ret"], c["error"])
    assert BODY_LOG == c["body"]


def _poke(obj):
    obj.add_history("poke")


def _at5(fn=None):
    def go(pool):
        Global.Steps_Finished = 5
        if fn:
            fn(pool)
    return go


FRESH = {
    "element": (lambda p: {"a": p[0], "d": p[1], "e": p[2]}, lambda p: None),
    "element_changed": (lambda p: {"a": p[0], "d": 1, "e": 1}, _at5(lambda p: _poke(p[0]))),
    "misc_values": (lambda p: {"a": [1, 2], "d": {"x": 1}, "e": "str"}, lambda p: None),
    "group_unchanged": (lambda p: {"a": p[6], "d": 1, "e": 1}, _at5()),
    "group_changed": (lambda p: {"a": p[6], "d": 1, "e": 1}, _at5(lambda p: _poke(p[6]))),
    "group_changed_same_step": (lambda p: {"a": p[6], "d": 1, "e": 1}, lambda p: _poke(p[6])),
    "reln_unchanged": (lambda p: {"a": p[7], "d": 1, "e": 1}, _at5()),
    "reln_first_changed": (lambda p: {"a": p[7], "d": 1, "e": 1}, _at5(lambda p: _poke(p[0]))),
    "reln_second_changed": (lambda p: {"a": p[7], "d": 1, "e": 1}, _at5(lambda p: _poke(p[1]))),
    "reln_self_changed": (lambda p: {"a": p[7], "d": 1, "e": 1}, _at5(lambda p: _poke(p[7]))),
}


@pytest.mark.parametrize("name", sorted(FRESH))
def test_freshness_golden(name):
    mk, between = FRESH[name]
    c = case(f"fresh_{name}")
    e = elements()
    Global.Steps_Finished = 2
    g = Anchored.create(e[3], e[4])
    NAME_OF[id(g)] = "G"
    rel = SRelation({"first": e[0], "second": e[1],
                     "type": MappingNumeric.create("succ", S.NUMBER)})
    NAME_OF[id(rel)] = "R"
    Global.Steps_Finished = 3
    pool = [*e, g, rel]
    cl = SCodelet("TFam", 50, mk(pool))
    between(pool)
    assert attempt(cl.run) == (c["ret"], c["error"])
    assert BODY_LOG == c["body"]


# ---- debugMAX ----------------------------------------------------------------------------------
def test_debug_messages_golden():
    Global.debugMAX = 1
    c = case("debug_codelet")
    assert attempt(SCodelet("Empty", 50, {}).run) == (c["ret"], c["error"])
    assert MESSAGES == c["messages"]
    MESSAGES.clear()
    act = SAction({"family": "Empty", "urgency": 100, "arguments": {}})
    c = case("debug_action")
    assert attempt(act.run) == (c["ret"], c["error"])
    assert MESSAGES == c["messages"]
    MESSAGES.clear()
    Global.debugMAX = 0
    c = case("debug_off")
    assert attempt(act.run) == (c["ret"], None)
    assert MESSAGES == c["messages"]


def test_default_message_hook_logs(caplog, monkeypatch):
    monkeypatch.undo()
    with caplog.at_level(logging.DEBUG, logger="seqsee.scodelet_base"):
        scodelet_base._message(["F", "green", "About to run: x"])
    assert "About to run: x" in caplog.text


# ---- SAction -------------------------------------------------------------------------------------
ACTION_NEW = {
    "action_new_empty": {},
    "action_new_no_urgency": {"family": "X", "arguments": {}},
    "action_new_no_args": {"family": "X", "urgency": 3},
    "action_new_undef_fam": {"family": None, "urgency": 3, "arguments": {}},
    "action_new_ok": {"family": "X", "urgency": 3, "arguments": []},
}


@pytest.mark.parametrize("name", sorted(ACTION_NEW))
def test_action_new_golden(name):
    c = case(name)
    _, err = attempt(lambda: SAction(ACTION_NEW[name]) and 1)
    assert (0 if err else 1, err) == (c["ok"], c["error"])


@pytest.mark.parametrize("c", cases("conditionally_run"),
                         ids=lambda c: f"seed{c['seed']}-urg{c['urgency']}")
def test_conditionally_run_golden(c):
    util.srand(c["seed"])
    ran = []
    for _ in range(6):
        BODY_LOG.clear()
        r = SAction({"family": "Empty", "urgency": c["urgency"], "arguments": {}}).conditionally_run()
        ran.append(v(r) if BODY_LOG else None)
    assert ran == c["ran"]
    assert util.rand() == pytest.approx(c["next"], abs=1e-12)


@pytest.mark.parametrize("c", cases("action_sub"), ids=lambda c: f"seed{c['seed']}")
def test_action_sub_golden(c):
    util.srand(c["seed"])
    ran = []
    for urg in (10, 60, 95):
        BODY_LOG.clear()
        r = action(urg, "TFam", {"a": urg, "d": 1, "e": 2})
        ran.append([v(r), list(BODY_LOG)])
    assert ran == c["ran"]
    assert util.rand() == pytest.approx(c["next"], abs=1e-12)


def test_action_stale_runs_golden():
    c = case("action_stale_runs")
    e = elements()
    g = Anchored.create(e[3], e[4])
    NAME_OF[id(g)] = "G"
    Global.Steps_Finished = 5
    g.add_history("poke")
    act = SAction({"family": "TFam", "urgency": 100, "arguments": {"a": g, "d": 1, "e": 2}})
    assert attempt(act.run) == (c["ret"], c["error"])
    assert BODY_LOG == c["body"]


@pytest.mark.parametrize("name,args", [("array", [1]), ("string", "5"), ("undef", None)])
def test_action_nonhash_golden(name, args):
    c = case(f"action_nonhash_{name}")
    act = SAction({"family": "TFam", "urgency": 100, "arguments": args})
    assert attempt(act.run) == (None, c["error"])
    assert Global.CurrentCodeletFamily == c["current_family"]


def test_action_unknown_family_golden():
    act = SAction({"family": "Nope", "urgency": 100, "arguments": {}})
    assert attempt(act.run) == (None, case("action_unknown_family")["error"])


# ---- Codelet_Family -------------------------------------------------------------------------------
@pytest.mark.parametrize("name,opts", [
    ("cf_no_attributes", {"body": lambda: None}),
    ("cf_no_body", {"attributes": []}),
    ("cf_neither", {}),
    ("cf_undef_attributes", {"attributes": None, "body": lambda: None}),
])
def test_codelet_family_option_checks_golden(name, opts):
    assert attempt(lambda: define_codelet_family("ZZTmp", **opts) or 1) == (None, case(name)["error"])
    assert "ZZTmp" not in FAMILIES


def test_decorator_registers_and_returns_body():
    def body(x, y):
        return (x, y)

    assert codelet_family("Pair", attributes=("x", "y"))(body) is body
    assert FAMILIES["Pair"].attributes == [("x", {}), ("y", {})]
    assert SCodelet("Pair", 10, {"y": 2, "x": 1}).run() == (1, 2)
    _, err = attempt(SCodelet("Pair", 10, {"x": 1}).run)
    assert err == "Mandatory parameter 'y' missing in call to Seqsee::SCF::Pair::run"


def test_redefining_a_family_replaces_it():
    codelet_family("Empty", attributes=[])(lambda: "new")
    assert SCodelet("Empty", 10).run() == "new"


# ---- ContinueWith ---------------------------------------------------------------------------------
def test_continue_with_errors_golden():
    e = elements()
    for name, args in [("cw_none", []), ("cw_two", [1, 2]), ("cw_string", ["x"]),
                       ("cw_undef", [None]), ("cw_element", [e[0]])]:
        assert attempt(lambda: continue_with(*args) or 1) == (None, case(name)["error"]), name


class FakeThought:
    perl_name = "SThought"


def test_continue_with_adds_thought(monkeypatch):
    added = []
    monkeypatch.setattr(Global.MainStream, "add_thought", added.append, raising=False)
    t = FakeThought()
    continue_with(t)
    assert added == [t]


def test_continue_with_codelet_tree_log(monkeypatch, tmp_path):
    import io
    added = []
    monkeypatch.setattr(Global.MainStream, "add_thought", added.append, raising=False)
    handle = io.StringIO()
    Global.Feature["CodeletTree"] = 1
    monkeypatch.setattr(Global, "CodeletTreeLogHandle", handle)
    t = FakeThought()
    continue_with(t)
    assert handle.getvalue() == f"\t{t}\n"


# ---- schedule and the hooks ---------------------------------------------------------------------
def test_schedule_uses_coderack_hook(monkeypatch):
    from seqsee import scodelet
    seen = []
    monkeypatch.setattr(scodelet, "_coderack_add_codelet", seen.append)
    cl = SCodelet("Foo", 10, {})
    cl.schedule()
    assert seen == [cl]


def test_schedule_adds_to_coderack():
    from seqsee import scoderack
    cl = SCodelet("Foo", 10, {})
    cl.schedule()
    assert scoderack.CODELETS == [cl] and scoderack.get_urgencies_sum() == 10


def test_workspace_and_ruleapp_hooks_build_codelets():
    Global.Steps_Finished = 9
    for mod in (sworkspace, srule_app):
        cl = mod._scodelet_new("TryToSquint", 200, {"x": 1})
        assert isinstance(cl, SCodelet)
        assert list(cl)[:3] == ["TryToSquint", 200, 9]
        assert cl.arguments == {"x": 1}


def test_family_run_name():
    assert scf_family.FAMILIES["TFam"].called == "Seqsee::SCF::TFam::run"

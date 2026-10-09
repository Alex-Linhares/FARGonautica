"""Tests for the test harness: Test/Seqsee.pm (Test::Seqsee), the CPAN Test::Stochastic it
uses, and Seqsee/ResultOfTestRun.pm (item 049).

Golden data: oracle/harness.pl → tests/golden/harness.json. Each scenario is rebuilt here
by name with the same fakes (payload classes, fake codelets, fringe objects) and the same
seeded draws. Test::More results are compared through ``seqsee.testing.more.details``.
RunSeqsee/RegTestHelper scenarios replace ``seqsee_main.interaction_step_n`` by the same
scripted stub as the oracle; the "real" cases are genuine short runs.
"""
import pickle
import re

import pytest

import golden
from seqsee import errors, s, sworkspace, user_interaction, util
from seqsee import global_ as Global
from seqsee import seqsee_main
from seqsee.errors import Confess
from seqsee.objects import result_of_test_run as rotr
from seqsee.testing import harness, more, stochastic

CASES = golden.load("harness")


def cases(kind):
    return [c for c in CASES if c["case"] == kind]


def ids(kind, key="name"):
    return [str(c.get(key, i)) for i, c in enumerate(cases(kind))]


def norm(msg):
    """Normalise a Python error text like the oracle's ``norm`` (drop 0x addresses), and
    ignore the trailing newline Perl's croak/die leaves after " at FILE line N." is cut."""
    if msg is None:
        return None
    msg = re.sub(r"\b(ARRAY|HASH|CODE)\(0x[0-9a-f]+\)", r"\1(0x)", str(msg))
    return msg.rstrip("\n")


def norm_tap(tap):
    out = []
    for ok, name in tap:
        name = norm(name)
        prefix = "Not all expected outputs seen: missing "
        if name is not None and name.startswith(prefix):
            name = prefix + ", ".join(sorted(name[len(prefix):].split(", ")))
        out.append([ok, name])
    return out


def tap_of(fn):
    before = len(more.details)
    ret = fn()
    return [list(d) for d in more.details[before:]], ret


@pytest.fixture
def loaded(capsys):
    """Perl: `use Test::Seqsee` (TestingMode, INITIALIZE_for_testing)."""
    s.load()
    harness.load()
    capsys.readouterr()


# ------------------------------------------------------------------ fakes (as in the oracle)
class Payload:
    perl_name = "Payload"

    def __str__(self):
        return "P:" + util.perl_ref(self)


class AreRelated(Payload):
    perl_name = "SThought::AreRelated"


class Special(AreRelated):
    perl_name = "SThought::Special"


class FocusOnP(Payload):
    perl_name = "Seqsee::SCF::FocusOn::P"


class OtherThing(Payload):
    perl_name = "Other::Thing"


PAYLOADS = {c.perl_name: c for c in (AreRelated, Special, FocusOnP, OtherThing)}


def payload_obj(name):
    return PAYLOADS[name]() if name else None


class FakeErr(Exception):
    def __init__(self, p):
        super().__init__()
        self.p = p

    def payload(self):
        return self.p

    def __str__(self):
        return f"FakeErr({util.perl_ref(self.p) if self.p is not None else 'none'})"


class FakeCodelet:
    def __init__(self, **o):
        self.o = o

    def run(self):
        if self.o.get("string") is not None:
            raise Confess(self.o["string"])
        if "payload" in self.o:
            raise FakeErr(self.o["payload"])
        return "lived"


class FakeCatObj:
    def __init__(self, *cats):
        self.cats = set(cats)

    def __str__(self):
        return "OBJ"

    def instance_of_cat(self, cat):
        return 1 if cat in self.cats else 0


class _Named:
    def __init__(self, name):
        self.perl_name = name


class FakeFringe:
    def __init__(self, k):
        self.k = k

    def get_fringe(self):
        f = [["a", 10]]
        if util.rand() < 0.5:
            f.append(["b", 5])
        if self.k:
            f.append(["z", 1])
        return f

    def get_extended_fringe(self):
        f = [["x", 10]]
        if util.rand() < 0.3:
            f.append(["y", 5])
        return f

    def get_actions(self):
        a = [_Named("SAction")]
        if util.rand() < 0.5:
            a.append(_Named("SCodelet"))
        return a


class Counter:
    calls = 0


def counting(code):
    def sub():
        Counter.calls += 1
        return code()
    return sub


SUBS = {
    "rand3": lambda: int(util.rand(3)),
    "rand2": lambda: int(util.rand(2)),
    "skewed": lambda: "a" if util.rand() < 0.9 else "b",
    "const": lambda: "k",
    "undef_ret": lambda: None if util.rand() < 0.5 else "u",
}


def picker(*outs):
    def sub():
        Counter.calls += 1
        o = outs[int(util.rand(len(outs)))]
        if o == "":
            return 1
        if o == "die":
            raise Confess("boom\n")
        raise FakeErr(payload_obj(o))
    return sub


def lister():
    def sub():
        Counter.calls += 1
        r = ["always"]
        if util.rand() < 0.5:
            r.append("half")
        if util.rand() < 0.05:
            r.append("rare")
        r += ["always", "always"]
        return r
    return sub


def scope_kwargs(flat):
    return dict(zip(flat[::2], flat[1::2]))


# ------------------------------------------------------------------ ParseSeq_, results
@pytest.mark.parametrize("c", cases("parse_seq"), ids=ids("parse_seq", "input"))
def test_parse_seq(c):
    assert list(harness.parse_seq_(c["input"])) == [c["seq"], c["continuation"]]


@pytest.mark.parametrize("c", cases("status"), ids=ids("status"))
def test_status(c):
    st = getattr(rotr, c["name"])
    assert st.get_status_string() == c["status_string"]
    assert st.is_success() == c["is_success"]
    assert st.is_at_least_an_extension() == c["is_at_least_an_extension"]
    assert st.is_a_crash() == c["is_a_crash"]


def test_status_new_and_missing():
    (c,) = cases("status_new")
    st = rotr.TestOutputStatus({"status_string": "Weird"})
    assert st.set_status_string("Successful") == c["old"]
    assert st.get_status_string() == c["now"]
    assert st.is_success() == c["is_success"]
    assert (st is rotr.Successful) == bool(c["shared"])
    (c,) = cases("status_missing")
    with pytest.raises(Confess) as e:
        rotr.TestOutputStatus({})
    assert norm(e.value) == norm(c["error"])


def test_result_of_test_run():
    (c,) = cases("result")
    r = rotr.ResultOfTestRun({"status": rotr.Crashed, "steps": 12, "error": None})
    assert r.set_steps(15) == c["old_steps"]
    assert r.get_status().get_status_string() == c["status"]
    assert r.get_steps() == c["steps"]
    assert r.get_error() == c["error"]


@pytest.mark.parametrize("c", cases("result_missing"), ids=ids("result_missing", "args"))
def test_result_missing(c):
    args = {"steps": 1, "error": "e"} if c["args"] == ["error", "steps"] else \
        {"status": 1, "steps": 2} if c["args"] == ["status", "steps"] else {}
    with pytest.raises(Confess) as e:
        rotr.ResultOfTestRun(args)
    assert norm(e.value) == norm(c["error"])


def test_results_of_test_runs():
    base = {"times": [1], "results": [], "rate": 0.5, "terms": "x", "features": "f", "version": 3}
    (c,) = cases("results")
    rs = rotr.ResultsOfTestRuns(dict(base))
    assert [rs.get_is_ltm_result(), rs.get_context(), rs.get_rate(), rs.get_version()] == \
        [c["is_ltm_result"], c["context"], c["rate"], c["version"]]
    (c,) = cases("results_given")
    rs = rotr.ResultsOfTestRuns({**base, "is_ltm_result": 1, "context": "ctx"})
    assert rs.set_context("new") == c["old_context"]
    assert [rs.get_is_ltm_result(), rs.get_context()] == [c["is_ltm_result"], c["context"]]
    (c,) = cases("results_missing")
    with pytest.raises(Confess) as e:
        rotr.ResultsOfTestRuns({"times": 1})
    assert norm(e.value) == norm(c["error"])


def test_result_pickles():
    """Class::Std::Storable: RunTestOnce.pl freezes the result (pickle here)."""
    r = rotr.ResultOfTestRun({"status": rotr.Successful, "steps": 3, "error": None})
    r2 = pickle.loads(pickle.dumps(r))
    assert r2.get_steps() == 3
    assert r2.get_status().get_status_string() == "Successful"


# ------------------------------------------------------------------ Test::Stochastic
@pytest.mark.parametrize("c", cases("range"), ids=ids("range", "args"))
def test_acceptable_range(c):
    assert list(stochastic._get_acceptable_range(*c["args"])) == c["range"]


@pytest.mark.parametrize("c", cases("stochastic"),
                         ids=[f"{c['fn']}-{c['sub']}-{c['seed']}-{c['order']}"
                              for c in cases("stochastic")])
def test_stochastic(c):
    f = getattr(stochastic, "stochastic_" + c["fn"])
    code = counting(SUBS[c["sub"]])
    arg = c["arg"]
    if c["order"] == "arg_first":
        # The oracle reuses the sub_first case's hash: replay that call first, so an
        # interrupted `each` over the expectation hash carries over.
        util.srand(c["seed"])
        f(code, arg)
    util.srand(c["seed"])
    Counter.calls = 0
    if c["order"] == "sub_first":
        args = (code, arg) + ((c["msg"],) if c["msg"] else ())
    else:
        args = (arg, code)
    tap, _ = tap_of(lambda: f(*args))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert Counter.calls == c["calls"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-14)


def test_stochastic_setup():
    (c,) = cases("stochastic_setup")
    stochastic.setup(times=50, tolerence=0.1)
    util.srand(7)
    Counter.calls = 0
    tap, _ = tap_of(lambda: stochastic.stochastic_ok(counting(SUBS["rand2"]), {"0": 0.5}))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert Counter.calls == c["calls"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-14)
    (c,) = cases("stochastic_setup_bad")
    with pytest.raises(Confess) as e:
        stochastic.setup(bogus=1)
    assert norm(e.value) == norm(c["error"])


def test_stochastic_reset_restores_defaults():
    stochastic.setup(times=5, tolerence=0.5)
    stochastic.reset()
    assert (stochastic.TIMES, stochastic.TOLERENCE) == (1000, 0.2)


# ------------------------------------------------------------------ payload helpers
WRAP = {
    "thought": (lambda: (_ for _ in ()).throw(FakeErr(payload_obj("SThought::AreRelated"))), None),
    "thought_sub": (lambda: (_ for _ in ()).throw(FakeErr(payload_obj("SThought::Special"))), None),
    "scf": (lambda: (_ for _ in ()).throw(FakeErr(payload_obj("Seqsee::SCF::FocusOn::P"))), None),
    "other": (lambda: (_ for _ in ()).throw(FakeErr(payload_obj("Other::Thing"))), None),
    "nopayload": (lambda: (_ for _ in ()).throw(FakeErr(None)), None),
    "string": (lambda: (_ for _ in ()).throw(Confess("plain\n")), None),
    "lives": (lambda: 1, None),
    "check_ok": (lambda: 1, lambda: 1),
    "check_bad": (lambda: 1, lambda: 0),
    "check_ignored_on_throw":
        (lambda: (_ for _ in ()).throw(FakeErr(payload_obj("SThought::AreRelated"))), lambda: 0),
}


@pytest.mark.parametrize("c", cases("wrap"), ids=ids("wrap"))
def test_wrap_to_get_payload_type(c):
    code, check = WRAP[c["name"]]
    wrapped = harness._wrap_to_get_payload_type(code, check)
    if c["error"] is None:
        assert wrapped() == c["ret"]
    else:
        with pytest.raises(Exception) as e:
            wrapped()
        assert norm(e.value) == norm(c["error"])


@pytest.mark.parametrize("c", cases("code_throws"), ids=ids("code_throws", "seed"))
def test_code_throws_stochastic(c):
    util.srand(c["seed"])
    Counter.calls = 0
    f = getattr(harness, "code_throws_stochastic_" + c["fn"])
    tap, _ = tap_of(lambda: f(picker(*c["outs"]), c["expect"]))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert Counter.calls == c["calls"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-14)


def test_code_throws_with_check_sub():
    (c,) = cases("code_throws_check_fails")
    util.srand(400)
    Counter.calls = 0
    tap, _ = tap_of(lambda: harness.code_throws_stochastic_ok(
        picker("", "SThought::AreRelated"), ["", "AreRelated"], lambda: 0))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert Counter.calls == c["calls"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-14)
    (c,) = cases("code_throws_all_and_only_check")
    util.srand(401)
    Counter.calls = 0
    tap, _ = tap_of(lambda: harness.code_throws_stochastic_all_and_only_ok(
        picker("", "SThought::AreRelated"), ["", "AreRelated"], lambda: 1))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert Counter.calls == c["calls"]


TT = {
    "match": {"payload": payload_obj("SThought::AreRelated")},
    "match_full": {"payload": payload_obj("SThought::AreRelated")},
    "match_list": {"payload": payload_obj("SThought::AreRelated")},
    "match_isa": {"payload": payload_obj("SThought::Special")},
    "wrong": {"payload": payload_obj("SThought::AreRelated")},
    "no_thought": {},
    "no_payload": {"payload": None},
    "string": {"string": "oops\n"},
}


@pytest.mark.parametrize("c", cases("throws_thought"), ids=ids("throws_thought"))
def test_throws_thought_ok(c):
    cl = FakeCodelet(**TT[c["name"]])
    if c["died"]:
        with pytest.raises(Exception) as e:
            harness.throws_thought_ok(cl, c["type"])
        assert "Can't locate object method \"payload\"" in str(e.value)
        return
    tap, ret = tap_of(lambda: harness.throws_thought_ok(cl, c["type"]))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert (util.perl_ref(ret) or None) == c["ret"]


@pytest.mark.parametrize("c", cases("throws_no_thought"), ids=ids("throws_no_thought"))
def test_throws_no_thought_ok(c):
    opts = {"lives": {}, "string": {"string": "x\n"},
            "thought": {"payload": payload_obj("SThought::AreRelated")}}[c["name"]]
    tap, _ = tap_of(lambda: harness.throws_no_thought_ok(FakeCodelet(**opts)))
    assert norm_tap(tap) == norm_tap(c["tap"])


@pytest.mark.parametrize("c", cases("undef_ok"), ids=ids("undef_ok", "args"))
def test_undef_ok(c):
    tap, _ = tap_of(lambda: harness.undef_ok(*c["args"]))
    assert norm_tap(tap) == norm_tap(c["tap"])


@pytest.mark.parametrize("c", cases("instance_of_cat_ok"), ids=ids("instance_of_cat_ok", "args"))
def test_instance_of_cat_ok(c):
    tap, _ = tap_of(lambda: harness.instance_of_cat_ok(FakeCatObj("cat1"), *c["args"]))
    assert norm_tap(tap) == norm_tap(c["tap"])


# ------------------------------------------------------------------ output_contains & co.
@pytest.mark.parametrize("c", cases("output_contains"), ids=ids("output_contains", "seed"))
def test_output_contains(c):
    util.srand(c["seed"])
    Counter.calls = 0
    tap, _ = tap_of(lambda: harness.output_contains(lister(), **scope_kwargs(c["scope"])))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert Counter.calls == c["calls"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-14)


def test_output_contains_unknown_quantifier():
    (c,) = cases("output_contains_bad")
    util.srand(550)
    with pytest.raises(Confess) as e:
        harness.output_contains(lister(), bogus=["x"])
    assert norm(e.value) == norm(c["error"])


@pytest.mark.parametrize("c", cases("output_wrapper"), ids=ids("output_wrapper", "seed"))
def test_output_wrappers(c):
    util.srand(c["seed"])
    Counter.calls = 0
    f = getattr(harness, f"output_{c['kind']}_contains")
    tap, _ = tap_of(lambda: f(lister(), c["arg"]))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert Counter.calls == c["calls"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-14)


@pytest.mark.parametrize("c", cases("fringe"), ids=ids("fringe", "seed"))
def test_fringe_contains(c):
    util.srand(c["seed"])
    setups = []
    target = FakeFringe(0) if c["how"] == "object" else (lambda: setups.append(1) or FakeFringe(1))
    f = getattr(harness, f"{c['kind']}_contains")
    tap, _ = tap_of(lambda: f(target, **scope_kwargs(c["scope"])))
    assert norm_tap(tap) == norm_tap(c["tap"])
    assert len(setups) == c["setups"]
    assert util.rand() == pytest.approx(c["next_rand"], abs=1e-14)


# ------------------------------------------------------------------ INITIALIZE_for_testing
def test_initialize_for_testing(loaded):
    (c,) = cases("initialize")
    assert Global.TestingMode == c["testing_mode"]
    assert Global.CurrentRunnableString == c["current_runnable_string"]
    assert Global.Steps_Finished == c["steps_finished"]
    opts = {k: v for k, v in Global.TestingOptionsRef.items() if k != "seed"}
    assert opts == {k: v for k, v in c["options"].items() if k != "seed"}
    assert isinstance(util.perl_num(Global.TestingOptionsRef["seed"]), (int, float))
    assert user_interaction.ask_user_extension is user_interaction.testing_ask_user_extension


def test_load_prints_view_and_is_repeatable(capsys):
    s.load()
    harness.load()
    assert capsys.readouterr().out == "View: 1!\n"
    harness.load()
    assert Global.TestingMode == 1


# ------------------------------------------------------------------ RunSeqsee / RegTestHelper
class Stub:
    """The oracle's scripted Seqsee::Interaction_step_n."""

    def __init__(self, actions):
        self.actions = [dict(a) for a in actions]
        self.called = []

    def __call__(self, opts):
        self.called.append({k: opts.get(k) for k in ("n", "max_steps", "update_after")})
        a = self.actions.pop(0) if self.actions else {"ret": 1}
        Global.Steps_Finished += a.get("steps", 0)
        if a.get("insert"):
            sworkspace.insert_elements(*a["insert"])
        for _ in range(a.get("fail", 0)):
            user_interaction.increment_failed_requests()
        if a.get("reject"):
            Global.ExtensionRejectedByUser[a["reject"]] = 1
        t = a.get("throw", "")
        if t == "got_it":
            errors.FinishedTest.throw(got_it=1)
        if t == "not_got_it":
            errors.FinishedTest.throw(got_it=0)
        if t == "clairvoyant":
            errors.NotClairvoyant.throw()
        if t == "blemished":
            errors.FinishedTestBlemished.throw()
        if t == "serr":
            errors.SErr.throw("objerr")
        if t == "die":
            raise Confess("boom\n")
        if t == "die_nonl":
            raise Confess("no newline\n")
        return a.get("ret")


def fresh(monkeypatch, actions):
    util.clear_all()
    Global.RealSequence.clear()
    Global.ExtensionRejectedByUser.clear()
    Global.Steps_Finished = 0
    sworkspace.ReadHead = 7
    stub = Stub(actions)
    monkeypatch.setattr(seqsee_main, "interaction_step_n", stub)
    return stub


def state_after(stub):
    return {
        "called": stub.called,
        "elements": [int(util.perl_num(e.get_mag())) for e in sworkspace.get_elements()],
        "real_sequence": [util.perl_num(x) for x in Global.RealSequence],
        "failed": user_interaction.get_failed_requests(),
        "read_head": sworkspace.ReadHead,
        "steps": Global.Steps_Finished,
        "rejected": sorted(Global.ExtensionRejectedByUser),
    }


def expected_state(c):
    return {k: c[k] for k in ("called", "elements", "real_sequence", "failed", "read_head",
                              "steps", "rejected")}


@pytest.mark.parametrize("c", cases("run_seqsee"), ids=ids("run_seqsee"))
def test_run_seqsee_stubbed(c, loaded, monkeypatch, capsys):
    stub = fresh(monkeypatch, c["stub"])
    if c["died"]:
        with pytest.raises(Exception) as e:
            harness.run_seqsee([1, 2, 3], [4, 5, 6, 7], 50, 2, c["min_extension"])
        assert norm(e.value) == norm(c["died"])
    else:
        r = harness.run_seqsee([1, 2, 3], [4, 5, 6, 7], 50, 2, c["min_extension"])
        assert r.get_status().get_status_string() == c["status"]
        assert r.get_steps() == c["result_steps"]
        assert norm(r.get_error()) == norm(c["error"])
    assert capsys.readouterr().out == c["stdout"]
    assert state_after(stub) == expected_state(c)


def test_run_seqsee_ltm_missing(loaded, monkeypatch, capsys):
    (c,) = cases("run_seqsee_ltm_missing")
    dumped = []
    monkeypatch.setattr(harness, "LTM_FILE", "/nonexistent/memory_dump.dat")
    monkeypatch.setattr(harness, "_sltm_dump", dumped.append)
    monkeypatch.setitem(Global.Feature, "LTM", 1)
    stub = fresh(monkeypatch, [{"steps": 3}, {"ret": 1}])
    r = harness.run_seqsee([1, 2, 3], [4], 50, 2, 3)
    assert r.get_status().get_status_string() == c["status"]
    assert r.get_steps() == c["result_steps"]
    assert norm(r.get_error()) == norm(c["error"])
    assert dumped == c["dumped"]
    assert capsys.readouterr().out == c["stdout"]
    assert state_after(stub) == expected_state(c)


def test_run_seqsee_ltm_dumps_after_run(loaded, monkeypatch, capsys):
    """From the source: with LTM on and a load that works, the LTM is dumped after the run
    (unreachable in Perl, where SLTM->Load always dies)."""
    dumped, loads = [], []
    monkeypatch.setattr(harness, "LTM_FILE", "some/memory_dump.dat")
    monkeypatch.setattr(harness, "_sltm_dump", dumped.append)
    monkeypatch.setattr(harness.sltm, "load", loads.append)
    monkeypatch.setitem(Global.Feature, "LTM", 1)
    fresh(monkeypatch, [{"steps": 2, "throw": "blemished"}])
    r = harness.run_seqsee([1, 2, 3], [4], 50, 2, 3)
    assert r.get_status() is rotr.InitialBlemish
    assert loads == dumped == ["some/memory_dump.dat"]
    assert "LTM not passed" not in capsys.readouterr().out


@pytest.mark.parametrize("c", cases("reg_test_helper"), ids=ids("reg_test_helper"))
def test_reg_test_helper_stubbed(c, loaded, monkeypatch, capsys):
    stub = fresh(monkeypatch, c["stub"])
    opts = {"seq": [1, 2, 3], "continuation": [4, 5, 6, 7], "max_false": 2, "max_steps": 40,
            "min_extension": c["min_extension"]}
    ret = []
    if c["died"]:
        with pytest.raises(Exception) as e:
            harness.reg_test_helper(opts)
        assert norm(e.value) == norm(c["died"])
    else:
        ret = list(harness.reg_test_helper(opts))
    assert ret == c["ret"]
    out = capsys.readouterr()
    assert out.out == c["stdout"]
    assert out.err == c["stderr"]
    assert state_after(stub) == expected_state(c)


def test_reg_test_helper_missing_option(loaded):
    (c,) = cases("reg_test_helper_missing")
    with pytest.raises(Confess) as e:
        harness.reg_test_helper({"seq": [1], "continuation": [], "max_false": 1, "max_steps": 1})
    assert norm(e.value) == norm(c["error"])


# ------------------------------------------------------------------ RegStat, RegHarness
def _opts_text(o):
    return ",".join(f"{k}=" + (" ".join(util.perl_str(x) for x in v) if isinstance(v, list)
                               else util.perl_str(v)) for k, v in sorted(o.items()))


def _blocks(lines):
    """RegStat's output: '====', option lines, '====', count lines (hash order: as sets)."""
    assert lines[0] == "============"
    second = lines.index("============", 1)
    return sorted(lines[1:second]), sorted(lines[second + 1:])


@pytest.mark.parametrize("c", cases("reg_stat"), ids=ids("reg_stat"))
def test_reg_stat(c, loaded, monkeypatch, capsys):
    script = list(c["script"])
    seen = []

    def helper(opts):
        seen.append(_opts_text(opts))
        kind, val = script.pop(0)
        if kind == "die":
            raise Confess(val)
        return kind, val

    monkeypatch.setattr(harness, "reg_test_helper", helper)
    opts = {"seq": [1, 2], "continuation": [3], "max_false": 4, "max_steps": 10, "min_extension": 2}
    res = harness.reg_stat(opts)
    out = capsys.readouterr().out
    assert {k: v for k, v in res.items() if k != "RESULTS"} == c["outputs"]
    assert [norm(r) for r in res["RESULTS"]] == [norm(r) for r in c["results"]]
    assert _blocks(out.split("\n")[:-1]) == _blocks(c["stdout_lines"])
    assert seen == c["seen_opts"]


def _read_lines(path):
    return path.read_text().splitlines() if path.exists() else None


@pytest.mark.parametrize("c", cases("reg_harness"), ids=ids("reg_harness"))
def test_reg_harness(c, loaded, monkeypatch, capsys, tmp_path):
    monkeypatch.chdir(tmp_path)
    shell_opts = []

    def shell(o):
        shell_opts.append(dict(o))
        return {**c["shell"], "RESULTS": list(c["shell"].get("RESULTS") or [])}

    monkeypatch.setattr(harness, "reg_stat_shell", shell)
    if c["earlier"] is not None:
        (tmp_path / ".last_res").write_text(c["earlier"])
    arg = c["input"]
    if isinstance(arg, str):
        (tmp_path / "in.reg").write_text(arg)
        arg = "in.reg"
    if c["died"]:
        with pytest.raises(Exception) as e:
            harness.reg_harness("x", arg) if c["second"] else harness.reg_harness(arg)
        assert norm(e.value).split("\n")[0] == norm(c["died"]).split("\n")[0]
        ret = [None] * 4
    else:
        ret = harness.reg_harness("x", arg) if c["second"] else harness.reg_harness(arg)
    out = re.sub(r"Processing time: \d+", "Processing time: N", capsys.readouterr().out)
    assert out == c["stdout"]
    assert ret[0] == c["improved"]
    assert ret[1] == c["worse"]
    assert ret[2] == c["results"]
    assert ret[3] == c["opts"]
    assert shell_opts == c["shell_opts"]
    last = _read_lines(tmp_path / ".last_res")
    if c["died"]:
        assert c["last_res"] == [] and last is None
    else:
        assert sorted(norm(x) for x in last) == c["last_res"]
    log = _read_lines(tmp_path / ".log_res")
    if c["log_res"] is None:
        assert log is None
    else:
        # the output hash is written in hash order: compare the key lines as a set
        assert re.fullmatch(r"\[\d+\]", log[0]) and c["log_res"][0] == "[T]"
        assert sorted(norm(x) for x in log[1:]) == sorted(c["log_res"][1:])


def test_reg_harness_name_prefixes_result_files(loaded, monkeypatch, capsys, tmp_path):
    """Perl names the files after ``$_`` (the regtest.pl loop variable): ``$_.last_res``."""
    monkeypatch.chdir(tmp_path)
    monkeypatch.setattr(harness, "reg_stat_shell", lambda o: {"GotIt": 2, "RESULTS": []})
    harness.reg_harness({"seq": "1 2 | 3"}, name="foo.reg")
    assert (tmp_path / "foo.reg.last_res").read_text().splitlines()[0] == "GotIt = 2"
    assert (tmp_path / "foo.reg.log_res").exists()
    assert not (tmp_path / ".last_res").exists()
    harness.reg_harness({"seq": "1 2 | 3"}, name="foo.reg")
    assert "PERFORMANCE" not in capsys.readouterr().out.split("Processing time")[-1]


def test_reg_stat_shell_runs_in_fresh_state_with_features(loaded, monkeypatch):
    """RegStatShell runs RegStat in a fresh Perl process with the current features; here it
    resets the state in-process, keeps Global.Feature and reloads the harness."""
    seen = {}

    def reg_stat(opts):
        seen["features"] = dict(Global.Feature)
        seen["testing"] = Global.TestingMode
        seen["opts"] = opts
        return {"RESULTS": []}

    monkeypatch.setattr(harness, "reg_stat", reg_stat)
    Global.Feature["AllowSquinting"] = 1
    Global.Steps_Finished = 99
    out = harness.reg_stat_shell({"seq": [1]})
    assert out == {"RESULTS": []}
    assert seen["features"].get("AllowSquinting") == 1
    assert seen["testing"] == 1
    assert seen["opts"] == {"seq": [1]}
    assert Global.Steps_Finished == 0


# ------------------------------------------------------------------ stochastic_test_codelet
def test_stochastic_test_codelet(loaded, monkeypatch):
    """From the source: each trial clears the workspace, builds the codelet from setup's
    options with urgency 100, runs it, and the payload type ('' when it lives) is collected;
    then post_run is checked once."""
    from seqsee.codelets import family

    runs = []

    class Probe(family.CodeletFamily):
        def run(self, action_object, args):
            runs.append((action_object.urgency, dict(args)))

    monkeypatch.setitem(family.FAMILIES, "HarnessProbe", Probe("HarnessProbe", [], None))
    clears = []
    real_clear = util.clear_all
    monkeypatch.setattr(harness.util, "clear_all", lambda: (clears.append(1), real_clear()))
    tap, _ = tap_of(lambda: harness.stochastic_test_codelet(
        setup=lambda: {"x": 1}, throws=[""], codefamily="HarnessProbe"))
    assert tap == [[1, "stochastic_all_seen_ok"], [1, "No check_sub: nothing to check"]]
    assert runs == [(100, {"x": 1})] and clears == [1]
    tap, _ = tap_of(lambda: harness.stochastic_test_codelet(
        setup=lambda: {}, throws=[""], codefamily="HarnessProbe", post_run=lambda: 0))
    assert tap == [[1, "stochastic_all_seen_ok"], [0, "checking the after effects"]]


# ------------------------------------------------------------------ real short runs
@pytest.mark.parametrize("c", cases("real"), ids=ids("real", "seed"))
def test_real_run(c, loaded, capsys):
    util.srand(c["seed"])
    r = harness.run_seqsee(c["seq"].split(), c["continuation"].split(), c["max_steps"],
                           c["max_false"], c["min_extension"])
    assert r.get_status().get_status_string() == c["status"]
    assert r.get_steps() == c["steps"]
    assert norm(r.get_error()) == norm(c["error"])
    assert [int(util.perl_num(e.get_mag())) for e in sworkspace.get_elements()] == c["elements"]
    assert capsys.readouterr().out == c["stdout"]


def test_run_seqsee_uses_testing_answers(loaded, monkeypatch):
    """From the source: RunSeqsee resets the failed-request count and answers questions
    from the real sequence (input + continuation) through testing_ask_user_extension."""
    fresh(monkeypatch, [])
    user_interaction.increment_failed_requests()
    answers = []

    def step(opts):
        answers.append(user_interaction.ask_user_extension(["4", "5"]))
        answers.append(user_interaction.ask_user_extension(["9"]))
        return 1

    monkeypatch.setattr(seqsee_main, "interaction_step_n", step)
    r = harness.run_seqsee(["1", "2", "3"], ["4", "5"], 10, 3, 3)
    assert answers == [1, None]
    assert user_interaction.get_failed_requests() == 1
    assert r.get_status() is rotr.NotEvenExtended

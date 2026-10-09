"""Tests for the stream of thoughts (item 039).

Mirrors lib/SStream2.pm (CreateNew, clear, init, add_thought, _think_the_current_thought,
_maybe_expell_thoughts, _recalculate_Compstrength, antiquate_current_thought,
_is_there_a_hit, thoughtTypeMatch). Golden data: oracle/sstream2.pl, whose scenarios are
replayed here op for op with fake thoughts (classes FT/FTB) and real SThought::SCat thoughts.
"""
import io
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, sstream2, util
from seqsee.codelets import family as scf_family
from seqsee.errors import Confess
from seqsee.saction import SAction
from seqsee.scodelet import SCodelet
from seqsee.sint import SInt
from seqsee.sstream2 import SStream2
from seqsee.sthought import SThought
from seqsee.sthought.scat import SThoughtSCat

CASES = golden.load("sstream2")


class FT:
    perl_name = "FT"

    def __init__(self, core, fringe, actions):
        self._core = core
        self.fringe = fringe
        self.actions = actions
        self._sf = None

    def core(self):
        return self._core

    def get_fringe(self):
        return self.fringe

    def stored_fringe(self, *v):
        if v:
            self._sf = v[0]
        return self._sf

    def get_actions(self):
        return list(self.actions)


class FTB(FT):
    perl_name = "FTB"


class FakeComp:
    def __init__(self, name):
        self.name = name


@pytest.fixture
def probe():
    ran = []

    def body(tag):
        ran.append(tag)

    scf_family.define_codelet_family("OracleProbe", attributes=["tag"], body=body)
    yield ran
    del scf_family.FAMILIES["OracleProbe"]


def _cat(name):
    return {"ascending": S.ASCENDING, "descending": S.DESCENDING, "sameness": S.SAMENESS}[name]


class Replay:
    def __init__(self, case, ran):
        self.ran = ran
        self.obj = {}
        self.name_of = {}      # id(obj) → name
        self.comp = {}
        self.stream = SStream2.create_new("S_" + case["name"], case.get("opts"))
        for name in sorted(case.get("thoughts") or {}):
            self.make_thought(name, case["thoughts"][name])

    def reg(self, name, o):
        self.obj[name] = o
        self.name_of[id(o)] = name

    def comp_of(self, spec):
        k, v = spec.split(":", 1)
        if k == "s":
            return v
        if k == "n":
            return util.perl_num(v)
        if v not in self.comp:
            self.comp[v] = FakeComp(v)
            self.name_of[id(self.comp[v])] = spec
        return self.comp[v]

    def action(self, spec):
        kind, *a = spec
        if kind == "codelet":
            return SCodelet(a[0], a[1], {"tag": a[2]})
        if kind == "action":
            return SAction({"family": "OracleProbe", "urgency": a[0], "arguments": {"tag": a[1]}})
        return {"undef": None, "zero": 0}.get(kind, a[0] if a else None)

    def make_thought(self, name, spec):
        kind = spec.get("kind", "FT")
        if kind == "scat":
            t = SThought.create(_cat(spec["cat"]))
        elif kind == "scat_list":
            t = SThought.create(_cat(spec["cat"]), list_context=True)
        elif kind == "scat_new":
            t = SThoughtSCat({"core": _cat(spec["cat"])})
        else:
            klass = {"FT": FT, "FTB": FTB}[kind]
            t = klass(spec.get("core", 1),
                      [[self.comp_of(c), act] for c, act in spec.get("fringe") or []],
                      [self.action(a) for a in spec.get("actions") or []])
        self.reg(name, t)
        if spec.get("cat"):
            self.name_of[id(_cat(spec["cat"]))] = "cat:" + spec["cat"]

    def nm(self, o):
        if o is None:
            return None
        if util._is_scalar(o):
            return util.perl_str(o)
        return self.name_of.get(id(o), "?" + util.perl_ref(o))

    def knm(self, k):
        """Name of a hash key: a thought/component object, or a string key."""
        if isinstance(k, str):
            return "s:" + k
        return self.name_of[id(k)]

    def op(self, kind, *args):
        s = self.stream
        if kind == "add":
            t = self.obj[args[0]]
            s.add_thought(t)
            sf = t.stored_fringe()
            return {"sf": None if sf is None else len(sf)}
        if kind == "add_args":
            s.add_thought(*[self.obj[a] for a in args])
            return None
        if kind == "antiquate":
            s.antiquate_current_thought()
            return None
        if kind == "clear":
            s.clear()
            return None
        if kind == "init":
            s.init()
            return None
        if kind == "type_match":
            return s.thought_type_match(self.obj[args[0]], self.obj[args[1]])
        if kind == "fringe_len":
            f = self.obj[args[0]].stored_fringe()
            return None if f is None else len(f)
        raise AssertionError(kind)

    def state(self):
        s = self.stream
        return {
            "current": "" if s.current_thought == "" else self.nm(s.current_thought),
            "older": [self.nm(t) for t in s.older_thoughts],
            "count": s.older_thought_count,
            "set": sorted(self.knm(k) for k in s.thoughts_set),
            "own": {self.knm(c): {self.knm(t): act for t, act in h.items()}
                    for c, h in s.component_ownership_of.items()},
            "vivify": sorted(self.knm(k) for k in s.vivify),
            "hit": {self.knm(c): v for c, v in s.hit_intensity.items()},
            "thit": {self.knm(t): v for t, v in s.thought_hit_intensity.items()},
            "rack": [[cl[0], cl[1], {k: self.nm(v) for k, v in cl[3].items()}]
                     for cl in scoderack.CODELETS],
            "ran": list(self.ran),
            "family": Global.CurrentCodeletFamily,
        }


def _err(e):
    return re.sub(r"=HASH\(0x[0-9a-f]+\)", "=HASH", str(e))


def _approx_dict(d):
    return {k: pytest.approx(v, rel=1e-9) for k, v in d.items()}


@pytest.mark.parametrize("case", CASES, ids=[c["name"] for c in CASES])
def test_golden(case, probe):
    r = Replay(case, probe)
    assert r.stream.discount_factor == case["discount"]
    assert r.stream.max_older_thoughts == case["max"]
    util.srand(case.get("seed", 1))
    for op, want in zip(case["ops"], case["results"]):
        try:
            value, error = r.op(*op), None
        except Confess as e:
            value, error = None, _err(e)
        assert error == want["error"], op
        assert value == want["value"], op
        got = r.state()
        ws = want["state"]
        for key in ("current", "older", "count", "set", "vivify", "ran", "family"):
            assert got[key] == ws[key], (op, key)
        assert got["own"] == {c: _approx_dict(h) for c, h in ws["own"].items()}, op
        assert got["hit"] == _approx_dict(ws["hit"]), op
        assert got["thit"] == _approx_dict(ws["thit"]), op
        assert len(got["rack"]) == len(ws["rack"]), op
        for g, w in zip(got["rack"], ws["rack"]):
            assert g[:2] == w[:2], op
            if g[0] == "ActOnOverlappingThoughts" and len(ws["thit"]) > 1 and g is got["rack"][-1]:
                # Hash order picks among several hit thoughts: only membership is comparable.
                assert g[2]["b"] == w[2]["b"]
                assert g[2]["a"] in ws["thit"]
            else:
                assert g[2] == w[2], op
        assert util.rand() == pytest.approx(want["rand"], abs=1e-12), op


# --- tests from reading the source -----------------------------------------------------


def test_main_stream_is_memoized_sstream2():
    assert isinstance(Global.MainStream, SStream2)
    assert SStream2.create_new("MainStream") is Global.MainStream
    with pytest.raises(Confess, match="Missing name!"):
        SStream2.create_new("")


def test_hit_codelet_and_current_codelet(probe):
    s = SStream2.create_new("x")
    a = FT(1, [["k", 10]], [])
    b = FT(1, [["k", 10]], [])
    s.add_thought(a)
    s.add_thought(b)
    assert Global.CurrentCodelet is b
    assert Global.CurrentCodeletFamily == "FT"
    (cl,) = scoderack.CODELETS
    assert isinstance(cl, SCodelet)
    assert cl.family == "ActOnOverlappingThoughts" and cl.urgency == 100
    assert cl.arguments["a"] is a and cl.arguments["b"] is b
    assert s.thought_hit_intensity == {a: pytest.approx(80.0)}


def test_number_and_string_components_share_a_key():
    s = SStream2.create_new("x")
    a = FT(1, [[5, 10]], [])
    b = FT(1, [["5", 10]], [])
    s.add_thought(a)
    s.add_thought(b)
    assert list(s.component_ownership_of) == ["5"]
    assert len(scoderack.CODELETS) == 1


def test_sint_component_keys_by_its_string():
    # SInt overloads "" ("SInt(3)"), so two SInts of the same magnitude share a Perl hash key.
    s = SStream2.create_new("x")
    s.add_thought(FT(1, [[SInt(3), 10]], []))
    s.add_thought(FT(1, [[SInt(3), 10]], []))
    assert list(s.component_ownership_of) == ["SInt(3)"]
    assert len(scoderack.CODELETS) == 1


def test_type_match_mapping_never_matches():
    # PERL-QUIRK: the table uses "A:B" keys but the lookup builds "A;B".
    s = SStream2.create_new("x")
    assert s.thought_type_match(FT(1, [], []), FTB(1, [], [])) == 0
    assert s.thought_type_match(FTB(1, [], []), FTB(1, [], [])) == 1


def test_codelet_tree_log(probe):
    Global.Feature["CodeletTree"] = 1
    Global.CodeletTreeLogHandle = io.StringIO()
    s = SStream2.create_new("x")
    t = FT(1, [], [SAction({"family": "OracleProbe", "urgency": 100, "arguments": {"tag": "p"}})])
    s.add_thought(t)
    lines = Global.CodeletTreeLogHandle.getvalue().splitlines()
    assert re.fullmatch(r"Chose FT=HASH\(0x[0-9a-f]+\)", lines[0])
    assert re.fullmatch(r"\tSAction=HASH\(0x[0-9a-f]+\)\tOracleProbe\t100", lines[1])
    assert probe == ["p"]


def test_continue_with_reaches_the_stream():
    from seqsee.codelets.scf import continue_with
    t = SThought.create(S.ASCENDING, list_context=True)
    continue_with(t)
    assert Global.MainStream.current_thought is t
    assert t.stored_fringe() == [[S.ASCENDING, 100]]


def test_reset_forgets_streams():
    s = SStream2.create_new("x")
    sstream2.reset()
    assert SStream2.create_new("x") is not s

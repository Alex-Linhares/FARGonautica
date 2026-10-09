"""Tests for the script runner and the DescribeSolution scripts (item 045).

Mirrors lib/Seqsee/Scripts.pm (Seqsee::Scripts::run, RETURN, SCRIPT) and
lib/Seqsee/Scripts/DescribeSolution.pm (the script families DescribeSolution,
DescribeInitialBlemish, DescribeBlocks, DescribeRule, DescribeMapping,
DescribeRelationSimple, DescribeRelationCompound, DescribeRelnCategory,
DescribeInterlacedCategory, Describe2InterlacedCategory,
DescribeMultipleInterlacedCategory, DescribeRelnMetoMode); DescribeSolution2.pm and
Scripts/Load.pm are empty / only load these. Golden data: oracle/scripts.pl, whose
scenarios are replayed here op for op (with test_ui's interpreter). The oracle's
MooseX::Params::Validate spec cache is ``scripts._cached_spec``.
"""
import re

import pytest

import golden
from test_sthought import err
from test_sthought_sobject import _norm
from test_sworkspace_groups import same
from test_ui import Replay as _UIReplay, _same_err
from seqsee import global_ as Global
from seqsee import scoderack, scripts, sworkspace, user_interaction, util
from seqsee.codelets.family import FAMILIES, family_run, load_families
from seqsee.constants import METO_MODE
from seqsee.errors import CallSubscript, Confess, ExceptionClassBase, ScriptReturn
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.scodelet import SCodelet
from seqsee.scripts import describe_solution as DS

CASES = golden.load("scripts")

UI = user_interaction
SC = UI.SolutionConfirmation


def _noaddr(s):
    return re.sub(r"=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)", "=REF", s)


class Replay(_UIReplay):
    """test_ui's interpreter, plus this oracle's ops."""

    def __init__(self, monkeypatch):
        super().__init__(monkeypatch)
        self.messages = []
        self.responses = []
        self.dumps = []
        monkeypatch.setattr(DS, "_message", lambda m, l=None: self.messages.append([m, l]))
        monkeypatch.setattr(DS, "_debug_message",
                            lambda m, l=None: self.messages.append(["DEBUG", _noaddr(m), l]))
        monkeypatch.setattr(DS, "_sltm_dump", lambda f: self.dumps.append(f))

    def nm(self, o):
        if isinstance(o, (int, float, str)) and not isinstance(o, bool):
            return o
        return super().nm(o)

    def argval(self, spec):
        if spec[0] == "meto":
            return getattr(METO_MODE, spec[1])
        return super().argval(spec)

    def _response(self, choices, question):
        """The oracle's FakeCommentary::MessageRequiringAResponse."""
        self.responses.append(["+".join(choices), question])
        return self.answers.pop(0) if self.answers else None

    def args_summary(self, h):
        if not isinstance(h, dict):
            return self.nm(h)
        return [[k, self.summarize(h[k])] for k in sorted(h)]

    def summarize(self, v):
        if isinstance(v, list):
            return [self.nm(x) for x in v]
        return self.nm(v)

    def scripts(self):
        out = []
        for cl in scoderack.CODELETS:
            a = cl[3]
            out.append([cl[0], cl[1], a.get("__S_T_E_P__"), self.args_summary(a.get("__A_R_G_S__")),
                        [[f[0], f[2], self.args_summary(f[1])] for f in a.get("__S_T_A_C_K__")]])
        return out

    def pairs(self, pairs):
        return {name: self.argval(spec) for name, spec in pairs}

    def run(self, kind, args):
        if kind == "init":
            n = super().run(kind, args)
            scripts.reset()
            self.mp.setattr(UI, "response", UI.perl_headless_response)
            self.messages.clear()
            self.responses.clear()
            self.dumps.clear()
            return n
        if kind == "commentary":
            super().run(kind, args)
            self.mp.setattr(UI, "response", self._response)
            return None
        if kind == "no_commentary":
            super().run(kind, args)
            self.mp.setattr(UI, "response", UI.perl_headless_response)
            return None
        if kind == "clear_spec_cache":
            scripts.clear_spec_cache()
            return None
        if kind == "messages":
            m = list(self.messages)
            self.messages.clear()
            return m
        if kind == "responses":
            r = list(self.responses)
            self.responses.clear()
            return r
        if kind == "dumps":
            d = list(self.dumps)
            self.dumps.clear()
            return d
        if kind == "scripts":
            return self.scripts()
        if kind == "script":
            family, *pairs = args
            a = self.pairs(pairs)
            family_run(family, SCodelet(family, 50, a), a)
            return None
        if kind == "script_resume":
            family, step, *pairs = args
            full = {"__S_T_E_P__": step, "__A_R_G_S__": self.pairs(pairs), "__S_T_A_C_K__": []}
            family_run(family, SCodelet(family, 50, full), full)
            return None
        if kind == "script_raw":
            family, action, raw = args
            action = SCodelet(family, 50, {}) if action else None
            family_run(family, action, raw)
            return None
        if kind == "run_next":
            if len(scoderack.CODELETS) != 1:
                raise Confess(f"coderack has {len(scoderack.CODELETS)}\n")
            cl = scoderack.CODELETS[0]
            scoderack.clear()
            cl.run()
            return cl[0]
        if kind == "meta":
            load_families()
            fam = FAMILIES[args[0]]
            return [fam.number_of_steps(), fam.expected_attributes()]
        if kind == "name_rule":
            self.reg(args[0], self.obj[args[1]].get_underlying_reln().get_rule())
            return None
        if kind == "name_type":
            self.reg(args[0], self.obj[args[1]].get_type())
            return None
        if kind == "best":
            return [self.nm(Global.BestRuleApp), self.nm(Global.BestRule)]
        if kind == "sc_full":
            ps = SC.accepted_position_structure
            return [self.nm(SC.accepted_rule), None if ps is None else list(ps),
                    sorted([self.name_of.get(id(r), "?"), [list(p) for p in v]]
                           for r, v in SC.rejected.items())]
        return super().run(kind, args)


@pytest.mark.parametrize("case", CASES, ids=[c["scenario"] for c in CASES])
def test_golden_scenario(case, monkeypatch):
    replay = Replay(monkeypatch)
    for op, want in zip(case["ops"], case["results"]):
        kind, args = op[0], op[1:]
        try:
            got = {"value": replay.run(kind, args)}
        except (Confess, ExceptionClassBase) as e:
            got = {"error": err(e)}
        if "error" in want:
            assert "error" in got, (op, got, want)
            g = got["error"].rstrip("\n") if isinstance(got["error"], str) else got["error"]
            assert _same_err(g, want["error"]), (op, got, want)
        else:
            assert "value" in got, (op, got, want)
            assert same(_norm(got["value"]), _norm(want["value"])), (op, got, want)


# --- tests from reading the source ------------------------------------------------------
SCRIPT_NAMES = ("DescribeSolution", "DescribeInitialBlemish", "DescribeBlocks", "DescribeRule",
                "DescribeMapping", "DescribeRelationSimple", "DescribeRelationCompound",
                "DescribeRelnCategory", "DescribeInterlacedCategory",
                "Describe2InterlacedCategory", "DescribeMultipleInterlacedCategory",
                "DescribeRelnMetoMode")


def test_scripts_are_registered_families():
    load_families()
    for name in SCRIPT_NAMES:
        assert isinstance(FAMILIES[name], scripts.Script), name


def test_return_and_script_raise():
    with pytest.raises(ScriptReturn):
        scripts.RETURN()
    with pytest.raises(CallSubscript) as e:
        scripts.SCRIPT("DescribeBlocks", {"group": 1})
    assert e.value.name == "DescribeBlocks" and e.value.arguments == {"group": 1}


def _toy(monkeypatch, steps, attributes=("x",)):
    """A throwaway script family 'Toy' with the given steps."""
    load_families()
    monkeypatch.setitem(FAMILIES, "Toy", scripts.Script("Toy", [(a, {}) for a in attributes], steps))


def test_steps_run_in_order_with_validated_args(monkeypatch):
    seen = []
    _toy(monkeypatch, [lambda x=None, *_: seen.append(("a", x)),
                       lambda x=None, *_: seen.append(("b", x))])
    family_run("Toy", SCodelet("Toy", 50, {}), {"x": 7})
    assert seen == [("a", 7), ("b", 7)]
    assert scoderack.CODELETS == []


def test_resume_at_step(monkeypatch):
    seen = []
    _toy(monkeypatch, [lambda *a: seen.append(0), lambda *a: seen.append(1),
                       lambda *a: seen.append(2)])
    full = {"__S_T_E_P__": 1, "__A_R_G_S__": {"x": 1}, "__S_T_A_C_K__": []}
    family_run("Toy", SCodelet("Toy", 50, full), full)
    assert seen == [1, 2]


def test_other_errors_propagate(monkeypatch):
    def boom(*a):
        raise Confess("boom")
    _toy(monkeypatch, [boom])
    with pytest.raises(Confess, match="boom"):
        family_run("Toy", SCodelet("Toy", 50, {}), {"x": 1})


def test_family_comes_from_the_action_object(monkeypatch):
    """Perl: the package is 'Seqsee::SCF::' . $action_object->family."""
    seen = []
    _toy(monkeypatch, [lambda x=None, *_: seen.append(x)])
    family_run("DescribeBlocks", SCodelet("Toy", 50, {}), {"x": 3})
    assert seen == [3]


def test_spec_cache_is_reset(monkeypatch):
    _toy(monkeypatch, [lambda *a: None])
    family_run("Toy", SCodelet("Toy", 50, {}), {"x": 1})
    assert scripts._cached_spec is not None
    scripts.reset()
    assert scripts._cached_spec is None


def test_describe_solution_step1_deletes_inconsistent_objects(monkeypatch):
    calls = []
    monkeypatch.setattr(sworkspace, "delete_objects_inconsistent_with", calls.append)
    monkeypatch.setattr(DS, "_message", lambda *a: None)

    class G:
        def get_underlying_reln(self):
            return "RA"
    with pytest.raises(CallSubscript) as e:
        FAMILIES["DescribeSolution"].get_step(1)(G())
    assert calls == ["RA"]
    assert e.value.name == "DescribeInitialBlemish"


def test_default_response_rejects(monkeypatch):
    """With the default (no) answer, the solution is recorded as rejected."""
    assert UI.response is UI.no_response
    assert UI.no_response(["Yes", "No"], "q?") is None
    UI.install(response=UI.perl_headless_response)
    assert UI.response is UI.perl_headless_response
    UI.reset()
    assert UI.response is UI.no_response


def test_ltm_feature_dumps(monkeypatch):
    dumps = []
    monkeypatch.setattr(DS, "_sltm_dump", dumps.append)
    step = FAMILIES["DescribeSolution"].get_step(4)
    step(None)
    assert dumps == []
    monkeypatch.setitem(Global.Feature, "LTM", 1)
    step(None)
    assert dumps == ["memory_dump.dat"]


def test_describe_2_interlaced_category_skips_the_second_extra_category(monkeypatch):
    """PERL-QUIRK: '@first_categories[2 .. $#first_categories]' skips index 1."""
    msgs = []
    monkeypatch.setattr(DS, "_message", lambda m, l=None: msgs.append(m))

    class C:
        def __init__(self, n):
            self.n = n

        def as_text(self):
            return self.n

    class I:
        def __init__(self, s):
            self.s = s

        def get_structure_string(self):
            return self.s

    cats = [C("a"), C("b"), C("c")]
    monkeypatch.setattr(DS, "get_common_categories", lambda *items: cats)

    class RA:
        def get_items(self):
            return [[I("1"), I("6")], [I("2"), I("7")]]
    FAMILIES["Describe2InterlacedCategory"].get_step(0)(None, RA())
    assert ("instance of 'a'.  and also of the categories 'c'. The second item in the template "
            "is an instance of 'a'.  and also of the categories 'c'. ") in msgs[0]
    assert msgs[0].endswith("consists of 1, 2 and so forth, whereas the second consists of 6, 7 "
                            "and so forth.")

"""Tests for the AllMX2 and LargeGp codelet families (item 043).

Mirrors lib/Seqsee/SCF_MX/AllMX2.pm (AttemptExtensionOfGroup, TryToSquint) and
lib/Seqsee/SCF_MX/LargeGp.pm (LargeGroup, MaybeStartBlemish, InterlacedInitialBlemish,
ArbitraryInitialBlemish), packages Seqsee::SCF::<Name>, run through their installed ``run``
(MooseX/SCF.pm). Golden data: oracle/scf_allmx2.pl, whose scenarios are replayed here op for
op (with test_scf_allmx's interpreter).
"""
import pytest

import golden
from test_scf_allmx import Replay as _AllMXReplay
from test_scf_allmx import _mask_overlapping
from test_scf_general1 import _same_error
from test_sthought import err
from test_sthought_sobject import _norm, mapping
from test_sworkspace_groups import same
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sltm, sworkspace, user_interaction, util
from seqsee.categories.interlaced import Interlaced
from seqsee.codelets import large_gp
from seqsee.codelets.family import FAMILIES, family_run
from seqsee.constants import DIR
from seqsee.errors import Confess, ExceptionClassBase, FinishedTestBlemished
from seqsee.objects.anchored import Anchored
from seqsee.srelation import SRelation

CASES = golden.load("scf_allmx2")


class Replay(_AllMXReplay):
    """scf_allmx.pl's op interpreter, plus this oracle's ops.

    SErr::ElementsBeyondKnownSought's Ask gets the answer the oracle sees without a GUI:
    ``$SGUI::Commentary`` is undef, so asking dies."""

    def __init__(self, monkeypatch):
        super().__init__(monkeypatch)
        monkeypatch.setattr(user_interaction, "boolean_response",
                            user_interaction.perl_headless_boolean_response)

    def run(self, kind, args):
        ws = sworkspace
        if kind == "strength":
            return self.obj[args[0]].get_strength()
        if kind == "metonym":
            o = self.obj[args[0]]
            act = o.get_metonym_activeness()
            act = None if act is None else util.perl_num(act)
            m = o.get_metonym()
            if not util.perl_true(m):
                return [act]
            return [act, m.get_category().as_text(), m.get_name(),
                    m.get_starred().get_structure_string()]
        if kind == "categories":
            return sorted(c.as_text() for c in self.obj[args[0]].get_categories())
        if kind == "flush":
            g = self.obj[args[0]]
            return [1 if util.perl_true(g.is_flush_left()) else 0,
                    1 if util.perl_true(g.is_flush_right()) else 0]
        if kind == "live":
            return 1 if ws.check_liveness(self.obj[args[0]]) else 0
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
        if kind == "coderack" and "value" in got and "value" in want:
            got["value"], want = _mask_overlapping(got["value"]), \
                {"value": _mask_overlapping(want["value"])}
        if "error" in want:
            assert "error" in got, (op, got, want)
            g = got["error"].rstrip("\n") if isinstance(got["error"], str) else got["error"]
            assert _same_error(g, want["error"]), (op, got, want)
        else:
            assert "value" in got, (op, got, want)
            assert same(_norm(got["value"]), _norm(want["value"])), (op, got, want)


# --- tests from reading the source ------------------------------------------------------

def _elements(seq):
    sworkspace.init({"seq": seq})
    return sworkspace.get_elements()


def test_families_are_registered():
    want = {"AttemptExtensionOfGroup": [("object", {}), ("direction", {})],
            "TryToSquint": [("actual", {}), ("intended", {})],
            "LargeGroup": [("group", {})],
            "MaybeStartBlemish": [("group", {})],
            "InterlacedInitialBlemish": [("count", {}), ("group", {}), ("cat", {})],
            "ArbitraryInitialBlemish": [("group", {})]}
    for name, attrs in want.items():
        assert FAMILIES[name].called == f"Seqsee::SCF::{name}::run"
        assert FAMILIES[name].attributes == attrs


def _extendable():
    e = _elements([1, 2, 3, 4, 5, 6])
    sltm.spike_by(10, S.NUMBER)
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})
    r.insert()
    g = Anchored.create(*e[:3])
    sworkspace.add_group(g)
    g.describe_as(S.ASCENDING)
    g.set_underlying_ruleapp(r)
    return e, g


def test_attempt_extension_of_group_schedules_are_we_done_on_toss(monkeypatch):
    e, g = _extendable()
    monkeypatch.setattr(Anchored, "find_extension", lambda self, d, skip: e[3])
    tosses = []
    monkeypatch.setattr(util, "toss", lambda p: tosses.append(p) or 1)
    family_run("AttemptExtensionOfGroup", None, {"object": g, "direction": DIR.RIGHT})
    assert list(g) == e[:4]
    assert tosses == [pytest.approx(g.get_strength() / 100)]
    from seqsee import scoderack
    [cl] = scoderack.CODELETS
    assert (cl[0], cl[1], cl[3]) == ("AreWeDone", 100, {"group": g})


def test_attempt_extension_of_group_safe_extend_failure_stops(monkeypatch):
    e, g = _extendable()
    monkeypatch.setattr(Anchored, "find_extension", lambda self, d, skip: e[3])
    monkeypatch.setattr(Anchored, "safe_extend", lambda self, x, at_end: 0)
    tosses = []
    monkeypatch.setattr(util, "toss", lambda p: tosses.append(p) or 1)
    family_run("AttemptExtensionOfGroup", None, {"object": g, "direction": DIR.RIGHT})
    assert tosses == []


def test_attempt_extension_of_group_confesses_if_rule_lost(monkeypatch):
    e, g = _extendable()

    def lose(self, x, at_end):
        self.set_underlying_reln(None)
        return 1
    monkeypatch.setattr(Anchored, "safe_extend", lose)
    monkeypatch.setattr(util, "toss", lambda p: 0)
    with pytest.raises(Confess, match="underlying_reln lost!"):
        family_run("AttemptExtensionOfGroup", None, {"object": g, "direction": DIR.RIGHT})


def test_try_to_squint_with_no_activation_does_nothing(monkeypatch):
    e = _elements([3, 3, 3])
    g = Anchored.create(*e)
    sworkspace.add_group(g)
    g.describe_as(S.SAMENESS)
    monkeypatch.setattr(sltm, "spike_and_choose", lambda amount, *c: None)
    family_run("TryToSquint", None, {"actual": g, "intended": e[0]})
    assert not util.perl_true(g.get_metonym_activeness())


def test_maybe_start_blemish_interlaced_with_count(monkeypatch):
    # The Interlaced_N name gives the count (a string, as Perl's $1).
    e = _elements([5, 1, 6, 2, 7, 3, 8])
    seen = []
    monkeypatch.setattr(large_gp, "_schedule", lambda f, u, a: seen.append((f, u, a)))
    cat = Interlaced.create(2)

    class _T:
        def get_category(self):
            return cat

    class _Rule:
        def get_transform(self):
            return _T()

    class _RA:
        def get_rule(self):
            return _Rule()

    g = Anchored.create(*e[1:])
    monkeypatch.setattr(Anchored, "find_extension", lambda self, d, skip: None)
    monkeypatch.setattr(Anchored, "get_underlying_reln", lambda self: _RA())
    monkeypatch.setattr(large_gp, "perl_isa", lambda x, cls: True)
    family_run("MaybeStartBlemish", None, {"group": g})
    assert seen == [("InterlacedInitialBlemish", 100, {"count": "2", "group": g, "cat": cat})]


def test_arbitrary_initial_blemish_in_testing_mode():
    e = _elements([1, 2])
    Global.TestingMode = 1
    with pytest.raises(FinishedTestBlemished):
        family_run("ArbitraryInitialBlemish", None, {"group": e[0]})


def test_interlaced_initial_blemish_dead_group_does_nothing():
    e = _elements([1, 6, 2, 7])
    g = Anchored.create(*e)
    family_run("InterlacedInitialBlemish", None,
               {"count": 2, "group": g, "cat": Interlaced.create(2)})
    assert list(sworkspace.get_groups()) == []

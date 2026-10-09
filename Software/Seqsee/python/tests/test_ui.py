"""Tests for the headless user interaction (item 044).

Mirrors lib/UserInteraction.pm (SErr::ElementsBeyondKnownSought's Ask, AskBasedOnRelation,
AskBasedOnRuleApp, AskBasedOnGroup, DoInsertBookKeeping, RuleAppPenetration,
RelationPenetration; RulesAskedSoFar; SolutionConfirmation) and lib/Seqsee/SCF_MX/UI.pm
(the codelet families AskIfThisIsTheContinuation, MaybeAskTheseTerms,
MaybeAskUsingThisGoodRule and DoTheAsking). Also Seqsee.pm's already_rejected_by_user,
UI/Graphical.pm's and Test/Seqsee.pm's main::ask_user_extension. Golden data:
oracle/ui.pl, whose scenarios are replayed here op for op (with test_scf_allmx2's
interpreter). The oracle's fake $SGUI::Commentary is user_interaction.boolean_response.
"""
import pytest

import golden
from test_scf_allmx2 import Replay as _AllMX2Replay
from test_scf_general1 import _same_error
from test_sthought import err
from test_sthought_sobject import _norm, mapping
from test_sworkspace_groups import same
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, sltm, sworkspace, user_interaction, util
from seqsee.codelets.family import FAMILIES, family_run
from seqsee.errors import Confess, ElementsBeyondKnownSought, ExceptionClassBase, NotClairvoyant
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.position_structure import PositionStructure
from seqsee.srelation import SRelation
from seqsee.srule import SRule

CASES = golden.load("ui")

UI = user_interaction
RAS = UI.RulesAskedSoFar
SC = UI.SolutionConfirmation


def _scal(v):
    return None if v is None else util.perl_str(v)


class Replay(_AllMX2Replay):
    """scf_allmx2.pl's op interpreter, plus this oracle's ops."""

    def __init__(self, monkeypatch):
        super().__init__(monkeypatch)
        self.answers = []
        self.asked = []
        # The oracle runs Test::Seqsee's INITIALIZE_for_testing once.
        monkeypatch.setattr(UI, "ask_user_extension", UI.testing_ask_user_extension)

    def _commentary(self, *args):
        """The oracle's FakeCommentary::MessageRequiringBooleanResponse."""
        self.asked.append([["+".join(a) if isinstance(a, list) else a for a in args],
                           sorted(self.name_of.get(id(o), "?") for o in Global.Hilit)])
        return self.answers.pop(0) if self.answers else None

    def first_codelet(self, family):
        for cl in scoderack.CODELETS:
            if cl[0] == family:
                return cl
        raise Confess(f"no {family}")

    def run(self, kind, args):
        ws = sworkspace
        if kind == "init":
            n = super().run(kind, args)
            Global.AcceptableTrustLevel = 0.5
            Global.Break_Loop = None
            Global.ExtensionRejectedByUser.clear()
            Global.RealSequence.clear()
            RAS.reset()
            SC.reset()
            UI.reset_failed_requests()
            self.mp.setattr(UI, "boolean_response", UI.perl_headless_boolean_response)
            self.answers.clear()
            self.asked.clear()
            return n
        if kind == "name_ruleapp":
            ra = self.obj[args[1]].get_underlying_reln()
            self.reg(args[0], ra)
            return None if ra is None else util.perl_ref(ra)
        if kind == "rule":
            self.reg(args[0], SRule.create(self.obj[args[1]]))
            return None
        if kind == "element":
            self.reg(args[0], Element.create(args[1], -1))
            return None
        if kind == "commentary":
            self.mp.setattr(UI, "boolean_response", self._commentary)
            self.answers[:] = args
            return None
        if kind == "no_commentary":
            self.mp.setattr(UI, "boolean_response", UI.perl_headless_boolean_response)
            return None
        if kind == "asked":
            a = list(self.asked)
            self.asked.clear()
            return a
        if kind == "real_seq":
            Global.RealSequence[:] = args
            return None
        if kind == "beyond":
            self.reg(args[0], ElementsBeyondKnownSought(next_elements=list(args[1:])))
            return None
        if kind == "name_arg":
            self.reg(args[0], self.first_codelet(args[1])[3][args[2]])
            return None
        if kind == "run_scheduled":
            cl = self.first_codelet(args[0])
            scoderack.clear()
            family_run(args[0], None, cl[3])
            return None
        if kind == "flush_coderack":
            scoderack.clear()
            return None
        if kind == "next_elements":
            return list(self.obj[args[0]].next_elements)
        if kind == "ask":
            return _scal(self.obj[args[0]].ask(*args[1:]))
        if kind == "ask_relation":
            return _scal(self.obj[args[0]].ask_based_on_relation(self.obj[args[1]], args[2]))
        if kind == "ask_ruleapp":
            return _scal(self.obj[args[0]].ask_based_on_rule_app(self.obj[args[1]], args[2]))
        if kind == "ask_group":
            return _scal(self.obj[args[0]].ask_based_on_group(self.obj[args[1]], args[2]))
        if kind == "bookkeeping":
            self.obj[args[0]].do_insert_book_keeping()
            return None
        if kind == "rule_app_penetration":
            return self.obj[args[0]].rule_app_penetration(args[1])
        if kind == "relation_penetration":
            # Perl's list-context value: the two lvalues of `my ($self, $relation) = @_`.
            return [2, _scal(self.obj[args[0]].relation_penetration(self.obj[args[1]]))]
        if kind == "already_rejected":
            return _scal(UI.already_rejected_by_user(list(args)))
        if kind == "rejected":
            return sorted(Global.ExtensionRejectedByUser)
        if kind == "set_rejected":
            for k in args:
                Global.ExtensionRejectedByUser[k] = 1
            return None
        if kind == "getg":
            return _scal(getattr(Global, args[0]))
        if kind == "ws":
            return [ws.ElementCount, [e.get_mag() for e in ws.get_elements()]]
        if kind == "user_ext":
            return _scal(UI.ask_user_extension(list(args)))
        if kind == "failed_requests":
            return UI.get_failed_requests()
        if kind == "ras":
            op, r = args
            f = {"most_recent": RAS.is_most_recent_successful_rule,
                 "time_success": RAS.time_since_rule_used_to_extend_successfully,
                 "time_failure": RAS.time_since_rule_used_to_extend_unsuccessfully,
                 "add_success": RAS.add_rule_to_success_list,
                 "add_failure": RAS.add_rule_to_failure_list,
                 "mark_rejected": RAS.mark_rule_as_rejected,
                 "mark_confirmed": RAS.mark_rule_as_confirmed,
                 "has_confirmed": RAS.has_rule_been_confirmed,
                 "has_rejected": RAS.has_rule_been_rejected}[op]
            v = f(self.obj[r])
            return None if op.startswith(("add_", "mark_")) else _scal(v)
        if kind == "ras_state":
            return [[[self.nm(r), t] for r, t in RAS.successful_rules],
                    [[self.nm(r), t] for r, t in RAS.unsuccessful_rules],
                    sorted(self.nm(r) for r in RAS.accepted_rules.values()),
                    sorted(self.nm(r) for r in RAS.rejected_rules.values())]
        if kind == "ps":
            ps = PositionStructure.create(self.obj[args[1]])
            self.reg(args[0], ps)
            return list(ps)
        if kind == "ps_list":
            self.reg(args[0], PositionStructure(args[1:]))
            return None
        if kind == "sc_reject":
            SC.add_rejected_solution(self.obj[args[0]], self.obj[args[1]])
            return None
        if kind == "sc_accept":
            SC.set_accepted_solution(self.obj[args[0]], self.obj[args[1]])
            return None
        if kind == "sc_has":
            return _scal(SC.has_this_been_rejected(self.obj[args[0]], self.obj[args[1]]))
        if kind == "sc_state":
            return [self.nm(SC.accepted_rule), self.nm(SC.accepted_position_structure),
                    sorted([self.name_of.get(id(r), "?"), len(v)] for r, v in SC.rejected.items())]
        return super().run(kind, args)


def _same_err(got, want):
    # A failed Smart::Comments `### require:` dies "\n" in Perl (the port: "require: ...").
    if want == "\n":
        return isinstance(got, str) and got.startswith("require: ")
    return _same_error(got, want)


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

def _elements(seq):
    sworkspace.init({"seq": seq})
    return sworkspace.get_elements()


def test_families_are_registered():
    want = {"AskIfThisIsTheContinuation": [
                ("relation", {"default": 0}), ("group", {"default": 0}),
                ("exception", {"required": 1}), ("expected_object", {"required": 1}),
                ("start_position", {"required": 1}), ("known_term_count", {"required": 1})],
            "MaybeAskTheseTerms": [("core", {}), ("exception", {})],
            "MaybeAskUsingThisGoodRule": [("core", {}), ("rule", {}), ("exception", {})],
            "DoTheAsking": [("core", {}), ("exception", {}), ("msg_prefix", {"defualt": ""})]}
    for name, attrs in want.items():
        assert FAMILIES[name].called == f"Seqsee::SCF::{name}::run"
        assert FAMILIES[name].attributes == attrs


def test_default_answer_is_no_answer():
    _elements([1, 2, 3])
    e = ElementsBeyondKnownSought(next_elements=[4])
    assert e.ask() is None
    assert Global.ExtensionRejectedByUser == {"4": 1}
    assert sworkspace.ElementCount == 3


def test_install_callbacks():
    _elements([1, 2, 3])
    UI.install(boolean_response=lambda q, *rest: 1)
    assert ElementsBeyondKnownSought(next_elements=[4]).ask() == 1
    assert sworkspace.ElementCount == 4
    UI.install(ask_user_extension=lambda items, msg=None: "yes")
    assert sworkspace._ask_user_extension([5], "m") == "yes"
    UI.reset()
    assert UI.boolean_response is UI.no_answer
    assert UI.ask_user_extension is UI.gui_ask_user_extension


def test_gui_ask_user_extension(monkeypatch):
    asked = []
    monkeypatch.setattr(UI, "boolean_response", lambda *a: asked.append(a) or 1)
    assert UI.gui_ask_user_extension([4]) == 1
    assert Global.AtLeastOneUserVerification == 1
    Global.Feature["debug"] = 1
    assert UI.gui_ask_user_extension([4, 5], "sfx") == 1
    assert asked == [("Is the next term 4?",),
                     ("Are the next terms: 4 5?", "", "sfx", ["debug"])]
    Global.ExtensionRejectedByUser["4"] = 1
    assert UI.gui_ask_user_extension([4, 5]) is None
    assert len(asked) == 2


def test_testing_ask_user_extension_counts_failures_from_undef():
    _elements([1, 2])
    Global.RealSequence[:] = [1, 2, 3]
    assert UI.get_failed_requests() is None
    assert UI.testing_ask_user_extension([9]) is None
    assert UI.get_failed_requests() == 1
    with pytest.raises(NotClairvoyant):
        UI.testing_ask_user_extension([3, 4])


def test_ask_in_testing_mode_passes_items_only(monkeypatch):
    _elements([1, 2])
    Global.TestingMode = 1
    seen = []
    monkeypatch.setattr(UI, "ask_user_extension", lambda *a: seen.append(a) or 1)
    assert ElementsBeyondKnownSought(next_elements=[3]).ask("prefix") == 1
    assert seen == [([3],)]
    assert sworkspace.ElementCount == 2
    assert Global.Break_Loop is None


def _relation_world():
    e = _elements([1, 2, 3])
    r = SRelation({"first": e[1], "second": e[2], "type": mapping("succ")})
    r.insert()
    return e, r


def test_maybe_ask_these_terms_relation_toss_and_spike(monkeypatch):
    e, r = _relation_world()
    rule = SRule.create(r)
    tosses = []
    monkeypatch.setattr(util, "toss", lambda p: tosses.append(p) or 1)
    spikes = []
    monkeypatch.setattr(sltm, "spike_by", lambda amount, c: spikes.append((amount, c)))
    x = ElementsBeyondKnownSought(next_elements=[4])
    family_run("MaybeAskTheseTerms", None, {"core": r, "exception": x})
    assert spikes == [(10, r.get_type())]
    assert tosses == [pytest.approx(r.get_strength() / 100)]
    # PERL-QUIRK: $success is never set: nothing is asked, the rule counts as a failure.
    assert RAS.unsuccessful_rules == [[rule, 0]]
    assert scoderack.CODELETS == []


def test_maybe_ask_these_terms_good_rule(monkeypatch):
    e, r = _relation_world()
    rule = SRule.create(r)
    Global.Steps_Finished = 1
    RAS.add_rule_to_success_list(rule)
    Global.Steps_Finished = 4
    x = ElementsBeyondKnownSought(next_elements=[4])
    family_run("MaybeAskTheseTerms", None, {"core": r, "exception": x})
    [cl] = scoderack.CODELETS
    assert (cl[0], cl[1], cl[3]) == ("MaybeAskUsingThisGoodRule", 100,
                                     {"core": r, "rule": rule, "exception": x})
    assert RAS.unsuccessful_rules == []


def test_get_core_type_and_rule():
    from seqsee.codelets import ui
    e, r = _relation_world()
    assert ui.get_core_type_and_rule(r) == ("relation", SRule.create(r))
    with pytest.raises(Confess, match="Strange core 7"):
        ui.get_core_type_and_rule(7)


def test_ask_continuation_group_insert_failure_stops(monkeypatch):
    e = _elements([1, 2, 3])
    sltm.spike_by(10, S.NUMBER)
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})
    r.insert()
    g = Anchored.create(*e)
    sworkspace.add_group(g)
    g.describe_as(S.ASCENDING)
    g.set_underlying_ruleapp(r)
    monkeypatch.setattr(UI, "boolean_response", lambda *a: 1)
    monkeypatch.setattr(SRelation, "insert", lambda self: None)
    extended = []
    monkeypatch.setattr(Anchored, "extend", lambda self, *a: extended.append(a))
    family_run("AskIfThisIsTheContinuation", None, {
        "group": g, "exception": ElementsBeyondKnownSought(next_elements=[4]),
        "expected_object": Element.create(4, -1), "start_position": 3, "known_term_count": 3})
    assert sworkspace.ElementCount == 4
    assert extended == []


def test_solution_confirmation_autovivifies():
    rule = object()
    assert SC.has_this_been_rejected(rule, PositionStructure([0])) is None
    assert SC.rejected == {rule: []}

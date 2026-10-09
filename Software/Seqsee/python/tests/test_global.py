"""Tests for seqsee.global_ (and the seqsee.sstream2 stub it needs).

Mirrors Perl: lib/Global.pm (plus SStream2.pm's CreateNew/clear, used for $Global::MainStream).
Golden data: oracle/global.pl -> golden/global.json.
"""
import pytest

import golden
from seqsee import global_ as Global
from seqsee import sstream2
from seqsee.sstream2 import SStream2
from seqsee.util import perl_num

CASES = golden.load("global")


def cases(op):
    return [c for c in CASES if c["op"] == op]


class FakeRuleApp:
    def __init__(self, rule, *items):
        self.rule, self.items = rule, list(items)

    def get_rule(self):
        return self.rule

    def get_items(self):
        return self.items


@pytest.fixture
def objs():
    return [object() for _ in range(5)]


def by_name(d, objs):
    names = {id(o): f"o{i}" for i, o in enumerate(objs)}
    return {names[id(k)]: v for k, v in d.items()}


# --- load-time values -----------------------------------------------------------

def test_initial_values():
    (c,) = cases("initial")
    # PERL-QUIRK: under `use S`, SHistory.pm's `||= ''` turns Steps_Finished 0 into ''. The port keeps Global.pm's 0.
    assert c["Steps_Finished"] == ""
    assert Global.Steps_Finished == perl_num(c["Steps_Finished"]) == 0
    for name in ("Break_Loop", "AtLeastOneUserVerification", "BestRule", "InitialTermCount", "debugMAX"):
        assert c[name] is None
        assert getattr(Global, name) is None
    for name in ("LogString", "TimeOfLastNewElement", "TimeOfNewStructure", "InterstepSleep", "Sanity",
                 "AcceptableTrustLevel", "CodeletTreeLogfile", "ActivationsLogfile", "RealSequence"):
        assert getattr(Global, name) == c[name], name
    assert sorted(Global.PossibleFeatures) == c["PossibleFeatures"]
    assert all(v == 1 for v in Global.PossibleFeatures.values())
    assert sorted(Global.Feature) == c["Feature"]
    for name in ("CurrentCodelet", "CurrentCodeletFamily", "TestingOptionsRef", "TestingMode", "Options_ref",
                 "BestRuleApp", "RecentPromisingRule", "RecentPromisingRuleApp",
                 "CodeletTreeLogHandle", "ActivationsLogHandle"):
        assert getattr(Global, name) is None, name
    for name in ("ExtensionRejectedByUser", "Hilit", "GroupStrengthByConsistency"):
        assert getattr(Global, name) == {}, name


def test_main_stream():
    (c,) = cases("initial")
    ms = Global.MainStream
    assert type(ms).__name__ == c["MainStream_class"]
    assert ms.name == c["MainStream_name"]
    assert ms.discount_factor == c["MainStream_DiscountFactor"]
    assert ms.max_older_thoughts == c["MainStream_MaxOlderThoughts"]
    assert (SStream2.create_new("MainStream") is ms) == bool(c["MainStream_memo"])


# --- Hilit ------------------------------------------------------------------------

def test_hilit(objs):
    expected = {c["after"]: c["hilit"] for c in cases("hilit")}
    Global.hilit(1, objs[0], objs[1])
    Global.hilit(2, objs[1], objs[2])
    assert by_name(Global.Hilit, objs) == expected["two calls"]
    Global.hilit(1)
    assert by_name(Global.Hilit, objs) == expected["no objects"]
    Global.clear_hilit()
    assert by_name(Global.Hilit, objs) == expected["ClearHilit"]


# --- rules ---------------------------------------------------------------------------

def test_rule_apps(objs):
    expected = {c["after"]: c for c in cases("rules")}
    o = objs
    ra1 = FakeRuleApp("rule1", o[0], o[1], o[2])
    ra2 = FakeRuleApp("rule2", o[2], o[3], o[2])
    steps = [("best ra1", Global.set_rule_app_as_best, ra1),
             ("recent ra2", Global.set_rule_app_as_recent, ra2),
             ("best ra2", Global.set_rule_app_as_best, ra2)]
    for after, fn, ra in steps:
        fn(ra)
        c = expected[after]
        assert Global.BestRule == c["BestRule"]
        assert Global.RecentPromisingRule == c["RecentPromisingRule"]
        assert by_name(Global.GroupStrengthByConsistency, objs) == c["gsbc"]
    assert Global.BestRuleApp is ra2 and Global.RecentPromisingRuleApp is ra2


# --- clear ----------------------------------------------------------------------------

def test_clear(objs):
    (c,) = cases("clear")
    ra1 = FakeRuleApp("rule1", objs[0], objs[1], objs[2])
    ra2 = FakeRuleApp("rule2", objs[2], objs[3], objs[2])
    Global.set_rule_app_as_best(ra2)
    Global.set_rule_app_as_recent(ra2)
    del ra1
    Global.Steps_Finished = 17
    Global.AtLeastOneUserVerification = 1
    Global.ExtensionRejectedByUser["1, 2"] = 1
    Global.LogString = "abc"
    Global.hilit(1, objs[4])
    Global.TimeOfLastNewElement = 9
    Global.clear()
    assert Global.Steps_Finished == c["Steps_Finished"]
    assert Global.AtLeastOneUserVerification == c["AtLeastOneUserVerification"]
    assert list(Global.ExtensionRejectedByUser) == c["ExtensionRejectedByUser"]
    assert Global.LogString == c["LogString"]
    assert by_name(Global.Hilit, objs) == c["hilit"]
    assert Global.BestRule == c["BestRule"]
    assert Global.RecentPromisingRule == c["RecentPromisingRule"]
    # PERL-QUIRK: clear() leaves the rule apps (and TimeOfLastNewElement) alone.
    assert (Global.BestRuleApp is not None) == bool(c["BestRuleApp_defined"])
    assert (Global.RecentPromisingRuleApp is not None) == bool(c["RecentPromisingRuleApp_defined"])
    assert by_name(Global.GroupStrengthByConsistency, objs) == c["gsbc"]
    assert Global.TimeOfLastNewElement == c["TimeOfLastNewElement"]
    Global.update_group_strength_by_consistency()
    assert by_name(Global.GroupStrengthByConsistency, objs) == cases("update_after_clear")[0]["gsbc"]


def test_clear_keeps_dict_identity():
    hilit, rejected, gsbc = Global.Hilit, Global.ExtensionRejectedByUser, Global.GroupStrengthByConsistency
    Global.clear()
    assert Global.Hilit is hilit
    assert Global.ExtensionRejectedByUser is rejected
    assert Global.GroupStrengthByConsistency is gsbc


# --- SetFutureTerms ---------------------------------------------------------------------

def test_set_future_terms():
    (c,) = cases("set_future_terms")
    Global.RealSequence[:] = [1, 2]
    Global.set_future_terms(3, 4)
    Global.set_future_terms()
    Global.set_future_terms(5)
    assert Global.RealSequence == c["RealSequence"]


# --- UpdateExtensionsRejectedByUser -----------------------------------------------------------

@pytest.mark.parametrize("c", cases("update_rejected"),
                         ids=lambda c: f"{c['keys']}-{c['prefix']}")
def test_update_extensions_rejected_by_user(c):
    Global.ExtensionRejectedByUser.update({k: 1 for k in c["keys"]})
    Global.update_extensions_rejected_by_user(*c["prefix"])
    assert sorted(Global.ExtensionRejectedByUser) == c["result"]
    assert [Global.ExtensionRejectedByUser[k] for k in sorted(Global.ExtensionRejectedByUser)] == c["values"]


# --- reset (test isolation) -----------------------------------------------------------------

def test_reset_restores_load_time_values(objs):
    old_stream = Global.MainStream
    Global.Steps_Finished = 5
    Global.Break_Loop = 1
    Global.Feature["Primes"] = 1
    Global.PossibleFeatures["bogus"] = 1
    Global.hilit(1, objs[0])
    Global.RealSequence.append(3)
    Global.AcceptableTrustLevel = 0.9
    Global.set_rule_app_as_best(FakeRuleApp("r", objs[0]))
    Global.reset()
    assert Global.Steps_Finished == 0 and Global.Break_Loop is None
    assert Global.Feature == {} and "bogus" not in Global.PossibleFeatures
    assert Global.Hilit == {} and Global.RealSequence == []
    assert Global.AcceptableTrustLevel == 0.5
    assert Global.BestRule is None and Global.BestRuleApp is None
    assert Global.GroupStrengthByConsistency == {}
    assert Global.MainStream is not old_stream
    assert Global.MainStream is SStream2.create_new("MainStream")


# --- SStream2 stub -----------------------------------------------------------------------------

def test_sstream2_create_new_is_memoized_by_name():
    a = SStream2.create_new("A", {"DiscountFactor": 0.5, "MaxOlderThoughts": 3})
    assert (a.discount_factor, a.max_older_thoughts) == (0.5, 3)
    assert SStream2.create_new("A", {"DiscountFactor": 0.1}) is a
    b = SStream2.create_new("B", {"DiscountFactor": 0, "MaxOlderThoughts": ""})
    assert (b.discount_factor, b.max_older_thoughts) == (0.8, 10)   # Perl `||` defaults
    assert b.current_thought == "" and b.older_thoughts == [] and b.older_thought_count == 0


def test_sstream2_missing_name():
    from seqsee.errors import Confess
    for name in (None, "", "0", 0):
        with pytest.raises(Confess, match="Missing name!"):
            SStream2.create_new(name)


def test_sstream2_clear_and_init():
    # The thought machinery (item 039) is tested in test_sstream2.py.
    s = SStream2.create_new("C")
    s.current_thought = "x"
    s.older_thoughts.append("y")
    s.older_thought_count = 1
    s.hit_intensity["k"] = 1
    s.clear()
    assert (s.current_thought, s.older_thoughts, s.older_thought_count) == ("", [], 0)
    assert s.hit_intensity == {"k": 1}   # clear() leaves hit_intensity alone, as in Perl
    assert s.init() is None


def test_sstream2_reset_memo():
    a = SStream2.create_new("D")
    sstream2.reset()
    assert SStream2.create_new("D") is not a

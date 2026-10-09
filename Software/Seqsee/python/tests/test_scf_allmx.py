"""Tests for the AllMX codelet families (item 042).

Mirrors lib/Seqsee/SCF_MX/AllMX.pm: the families CheckIfInstance, FocusOn,
ActOnOverlappingThoughts (with its ActionForThoughtTypes multimethod), AreTheseGroupable,
AreWeDone (with BelieveDone), ConvulseEnd and CheckProgress (with CalculateDesperation),
packages Seqsee::SCF::<Name>, run through their installed ``run`` (MooseX/SCF.pm). Also the
two Sanity.pm SanityCheck variants ConvulseEnd calls. Golden data: oracle/scf_allmx.pl,
whose scenarios are replayed here op for op.
"""
import pytest

import golden
from test_scf_general1 import _same_error
from test_scf_general2 import Replay as _General2Replay
from test_sthought import err
from test_sthought_sobject import _norm, cat, mapping
from test_sworkspace_groups import same
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, sltm, sworkspace, util
from seqsee.codelets import all_mx
from seqsee.codelets.family import FAMILIES, family_run, load_families
from seqsee.constants import DIR
from seqsee.errors import Confess, CouldNotCreateExtendedGroup, ExceptionClassBase, FinishedTest
from seqsee.objects.anchored import Anchored
from seqsee.scodelet import SCodelet
from seqsee.srelation import SRelation
from seqsee.srule import SRule
from seqsee.srule_app import SRuleApp
from seqsee.sthought import SThought

CASES = golden.load("scf_allmx")


class _Probe:
    """Stands in for a family's installed run: records the arguments."""

    def __init__(self, replay, family):
        self.replay, self.family = replay, family

    def run(self, action_object, args):
        r = self.replay
        r.probes.append([self.family, *[[k, r.summarize(args[k])] for k in sorted(args)]])


class Replay(_General2Replay):
    """The oracle's op interpreter (scf_general2.pl's, plus this oracle's ops)."""

    def __init__(self, monkeypatch):
        super().__init__()
        self.mp = monkeypatch
        self.probes = []
        monkeypatch.setattr(all_mx, "_ask_for_more_terms",
                            lambda: self.probes.append(["ask_for_more_terms"]))

    def state(self):
        ws = sworkspace
        out = []
        for o in sorted(ws.OBJECTS.values(), key=self.nm):
            ul = o.get_underlying_reln()
            out.append([self.nm(o), ws.LEFT_EDGE_OF.get(o), ws.RIGHT_EDGE_OF.get(o),
                        sorted(c.as_text() for c in o.get_categories()),
                        ul.get_rule().get_transform().as_text() if util.perl_true(ul) else None,
                        [self.nm(x) for x in o]])
        return out

    def thought_name(self, t):
        if not util.perl_true(t):
            return ""
        return f"{util.perl_ref(t)}:{self.nm(t.core())}"

    def run(self, kind, args):
        ws = sworkspace
        if kind == "init":
            n = super().run(kind, args)
            Global.MainStream.clear()
            Global.Hilit.clear()
            Global.Steps_Finished = 0
            Global.TimeOfLastNewElement = 0
            Global.TimeOfNewStructure = 0
            Global.AtLeastOneUserVerification = None
            Global.TestingMode = None
            Global.RecentPromisingRuleApp = None
            Global.RecentPromisingRule = None
            Global.CurrentCodelet = None
            all_mx._last_solution_description_time = None
            all_mx._last_time_progresschecker_run = 0
            self.probes.clear()
            return n
        if kind == "global":
            setattr(Global, args[0], args[1])
            return None
        if kind == "activation" and len(args) == 1:
            return sltm.get_real_activations_for_one_concept(cat(args[0]))
        if kind == "fake_ruleapp":
            g, r, *items = args
            ra = SRuleApp({"rule": SRule.create(self.obj[r]),
                           "items": self.O(items) if items else list(self.obj[g]),
                           "direction": DIR.RIGHT})
            self.obj[g].set_underlying_reln(ra)
            return None
        if kind == "thought":
            t = SThought.create(self.obj[args[1]])
            self.reg(args[0], t)
            return util.perl_ref(t)
        if kind == "thought_cat":
            t = SThought.create(cat(args[1]))
            self.reg(args[0], t)
            return util.perl_ref(t)
        if kind == "stream":
            s = Global.MainStream
            return [self.thought_name(s.current_thought), s.older_thought_count,
                    [self.thought_name(t) for t in s.older_thoughts]]
        if kind == "current_codelet":
            Global.CurrentCodelet = SCodelet(args[0], 50, {})
            return None
        if kind == "readhead":
            return ws.ReadHead
        if kind == "set_readhead":
            ws.ReadHead = args[0]
            return None
        if kind == "items":
            g = self.obj[args[0]]
            return [[self.nm(x) for x in g], g.get_left_edge(), g.get_right_edge()]
        if kind == "supergroups":
            return sorted(self.nm(x) for x in ws.get_super_groups(self.obj[args[0]]))
        if kind == "hilit":
            return sorted(([self.name_of.get(id(o), "?"), v] for o, v in Global.Hilit.items()),
                          key=lambda p: p[0])
        if kind == "recent":
            ra = Global.RecentPromisingRuleApp
            if not util.perl_true(ra):
                return None
            return 1 if ra is self.obj[args[0]].get_underlying_reln() else 0
        if kind == "last_solution":
            return all_mx._last_solution_description_time
        if kind == "last_progress":
            return all_mx._last_time_progresschecker_run
        if kind == "desperation":
            return all_mx.calculate_desperation(*args)
        if kind == "probe":
            load_families()  # so undoing the patch restores the real family, not nothing
            self.mp.setitem(FAMILIES, args[0], _Probe(self, args[0]))
            return None
        if kind == "probes":
            p = list(self.probes)
            self.probes.clear()
            return p
        return super().run(kind, args)


def _mask_overlapping(coderack):
    """When the stream's current thought hits several older thoughts, which one it pairs
    with in ActOnOverlappingThoughts depends on Perl hash order (see test_sstream2), so the
    ``a`` argument is not compared. Everything else, including the draw count, is."""
    return sorted(([c[0], c[1], *("a=*" if a.startswith("a=") else a for a in c[2:])]
                   if c[0] == "ActOnOverlappingThoughts" else c) for c in coderack)


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
    want = {"CheckIfInstance": [("obj", {}), ("cat", {})],
            "FocusOn": [("what", {"optional": 1})],
            "ActOnOverlappingThoughts": [("a", {}), ("b", {})],
            "AreTheseGroupable": [("items", {}), ("reln", {})],
            "AreWeDone": [("group", {})],
            "ConvulseEnd": [("object", {}), ("direction", {})],
            "CheckProgress": []}
    for name, attrs in want.items():
        assert FAMILIES[name].called == f"Seqsee::SCF::{name}::run"
        assert FAMILIES[name].attributes == attrs


def test_action_for_thought_types_dispatch(monkeypatch):
    e = _elements([1, 2, 3])
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})
    seen = []
    monkeypatch.setattr(all_mx, "_action", lambda u, f, o: seen.append((u, f, o)))
    all_mx.action_for_thought_types(r, r)
    all_mx.action_for_thought_types(e[0], e[1])
    all_mx.action_for_thought_types(e[0], r)       # mixed: the default, nothing
    all_mx.action_for_thought_types(None, e[0])    # no core
    all_mx.action_for_thought_types(S.ASCENDING, S.ASCENDING)
    assert seen == [(100, "FindIfRelatedRelations", {"a": r, "b": r}),
                    (100, "FindIfRelated", {"a": e[0], "b": e[1]})]


def test_check_if_instance_spikes_then_dies_under_ltm():
    # PERL-QUIRK: Seqsee objects have no InsertISALink method.
    e = _elements([1, 2, 3])
    before = sltm.get_real_activations_for_one_concept(S.ODD)
    family_run("CheckIfInstance", None, {"obj": e[0], "cat": S.ODD})
    assert sltm.get_real_activations_for_one_concept(S.ODD) == before
    Global.Feature["LTM"] = 1
    with pytest.raises(Confess, match='Can\'t locate object method "InsertISALink" via '
                                      'package "Seqsee::Element"'):
        family_run("CheckIfInstance", None, {"obj": e[2], "cat": S.ODD})
    assert sltm.get_real_activations_for_one_concept(S.ODD) > before


def test_focus_on_reader_sameness_branch(monkeypatch):
    e = _elements([1, 1, 2])
    tosses = []
    monkeypatch.setattr(util, "toss", lambda p: tosses.append(p) or 1)
    family_run("FocusOn", None, {})
    assert tosses[0] == 0.3
    assert [g.get_edges() for g in sworkspace.get_groups()] == [(0, 1)]
    assert not util.perl_true(Global.MainStream.current_thought)


def test_are_these_groupable_unknown_numeric_mapping_confesses(monkeypatch):
    e = _elements([1, 2, 3])
    sltm.spike_by(10, S.NUMBER)
    t = mapping("succ")
    r = SRelation({"first": e[0], "second": e[1], "type": t})
    r.insert()
    monkeypatch.setattr(Anchored, "set_underlying_ruleapp", lambda self, reln: None)
    monkeypatch.setattr(t, "get_name", lambda: "double")
    with pytest.raises(Confess, match="Should not be here"):
        family_run("AreTheseGroupable", None, {"items": e, "reln": r})


def _convulsable():
    e = _elements([1, 2, 3, 4, 5, 6])
    sltm.spike_by(10, S.NUMBER)
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})
    r.insert()
    g = Anchored.create(*e[:4])
    sworkspace.add_group(g)
    g.set_underlying_ruleapp(r)
    return e, g


def test_convulse_end_puts_the_ejected_item_back(monkeypatch):
    e, g = _convulsable()
    monkeypatch.setattr(Anchored, "find_extension", lambda self, d, skip: e[4])
    monkeypatch.setattr(Anchored, "extend", lambda self, x, at_end: None)
    family_run("ConvulseEnd", None, {"object": g, "direction": DIR.RIGHT})
    assert list(g) == e[:4] and g.get_edges() == (0, 3)
    family_run("ConvulseEnd", None, {"object": g, "direction": DIR.LEFT})
    assert list(g) == e[:4]


def test_convulse_end_errors(monkeypatch):
    e, g = _convulsable()
    monkeypatch.setattr(Anchored, "find_extension", lambda self, d, skip: e[4])

    def cannot(self, x, at_end):
        raise CouldNotCreateExtendedGroup("Extended group creation failed")
    monkeypatch.setattr(Anchored, "extend", cannot)
    with pytest.raises(Confess, match="Unable to extend group!"):
        family_run("ConvulseEnd", None, {"object": g, "direction": DIR.RIGHT})

    e, g = _convulsable()

    def boom(self, x, at_end):
        raise Confess("boom")
    monkeypatch.setattr(Anchored, "extend", boom)
    with pytest.raises(Confess, match="boom"):
        family_run("ConvulseEnd", None, {"object": g, "direction": DIR.RIGHT})


def test_check_progress_uninserts_old_weak_relations(monkeypatch):
    e = _elements([1, 2, 3, 4])
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})
    r.insert()
    tosses = []
    monkeypatch.setattr(util, "toss", lambda p: tosses.append(p) or 1)
    Global.Steps_Finished = 300
    Global.TimeOfNewStructure = 50     # desperation 20
    Global.TimeOfLastNewElement = 0
    family_run("CheckProgress", None, {})
    assert sworkspace.relations == {}
    assert tosses == [pytest.approx((100 - r.get_strength()) / 200), pytest.approx(300 / 400)]
    assert all_mx._last_time_progresschecker_run == 300


def test_believe_done_in_testing_mode():
    e = _elements([1, 2])
    Global.TestingMode = 1
    with pytest.raises(FinishedTest) as info:
        all_mx.believe_done(e[0])
    assert info.value.got_it == 1

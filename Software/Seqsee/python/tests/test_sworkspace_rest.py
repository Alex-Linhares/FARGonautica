"""Tests for SWorkspace.pm, part IV (item 034): choice distributions, reading, saccades,
strength updates, the uniform choosers, category lookups, holes, SErr::AskUser's
WorthAsking/Ask and the deletion helpers.

Mirrors lib/SWorkspace.pm (__UpdateObjectStrengths, __ReadObjectOrRelation,
__GetObjectOrRelationChoiceProbabilityDistribution, __GetObjectChoiceProbabilityDistribution,
__GetRelationChoiceProbabilityDistribution, __GetObjectsBelongingToCategory,
__GetObjectsBelongingToSimilarCategories, __ChooseByStrength, read_relation,
_get_some_object_at, _saccade, are_there_holes_here, SErr::AskUser::WorthAsking,
SErr::AskUser::Ask, DeleteObjectsInconsistentWith, __DeleteNonSubgroupsOfFrom). Golden data:
oracle/sworkspace_rest.pl, whose scenarios are replayed here op for op.
"""
import pytest

import golden
from test_sworkspace_groups import same, same_error
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sltm, srelation, sworkspace, util
from seqsee.constants import DIR
from seqsee.errors import AskUser, Confess, ExceptionClassBase
from seqsee.mapping.numeric import MappingNumeric
from seqsee.objects.anchored import Anchored
from seqsee.objects.object import SeqseeObject
from seqsee.srelation import SRelation

CASES = golden.load("sworkspace_rest")

DIRS = {"LEFT": DIR.LEFT, "RIGHT": DIR.RIGHT, "UNKNOWN": DIR.UNKNOWN, "NEITHER": DIR.NEITHER}


def err(e):
    if isinstance(e, ExceptionClassBase):
        return {"class": e.perl_name, "message": e.message or ""}
    return str(e)


def cat(name):
    return {"ascending": S.ASCENDING, "descending": S.DESCENDING, "sameness": S.SAMENESS,
            "number": S.NUMBER, "prime": S.PRIME}[name]


class FakeRuleApp:
    def __init__(self, replay, bad):
        self.replay = replay
        self.bad = set(bad)

    def check_consitency_of_group(self, group):
        return 0 if self.replay.nm(group) in self.bad else 1


class Replay:
    """The oracle's op interpreter."""

    def __init__(self, monkeypatch):
        self.obj = {}
        self.name_of = {}
        self.answers = []
        self.ui_log = []
        self.by_ends_at_init = 0
        monkeypatch.setattr(sworkspace, "_ask_user_extension", self._ask_user_extension)
        monkeypatch.setattr(sworkspace, "_update_display",
                            lambda: self.ui_log.append(["update_display"]))

    def _ask_user_extension(self, next_elements, msg):
        self.ui_log.append(["ask", [int(x) for x in next_elements], msg])
        return self.answers.pop(0) if self.answers else None

    def reg(self, name, o):
        self.obj[name] = o
        self.name_of[id(o)] = name

    def nm(self, o):
        if o is None:
            return None
        if isinstance(o, (str, int, float)):
            return o
        if id(o) in self.name_of:
            return self.name_of[id(o)]
        return "?" + o.as_text()

    def names(self, objs):
        return sorted(self.nm(o) for o in objs)

    def O(self, names):
        return [self.obj.get(n) for n in names]

    def pairs(self, values, objects):
        return sorted(([self.nm(o), v] for o, v in zip(objects, values)), key=lambda p: p[0])

    def state(self):
        ws = sworkspace
        live = sorted(ws.OBJECTS.values(), key=self.nm)
        return {
            "objects": [[self.nm(o), ws.LEFT_EDGE_OF.get(o), ws.RIGHT_EDGE_OF.get(o)]
                        for o in live],
            "relations": self.names(ws.relations.values()),
            "by_ends": len(ws.relations_by_ends) - self.by_ends_at_init,
        }

    def strengths(self):
        ws = sworkspace
        objs = list(ws.OBJECTS.values()) + list(ws.relations.values())
        return sorted(([self.nm(o), o.get_strength()] for o in objs), key=lambda p: p[0])

    def run(self, kind, args):
        ws = sworkspace
        if kind == "init":
            sltm.clear()
            ws.init({"seq": args[0]})
            ws.clear_bar_lines()
            Global.Feature.clear()
            Global.Steps_Finished = 0
            Global.AcceptableTrustLevel = 0.5
            Global.Break_Loop = None
            Global.ExtensionRejectedByUser.clear()
            self.obj.clear()
            self.name_of.clear()
            self.answers.clear()
            self.ui_log.clear()
            util.srand(1)
            self.by_ends_at_init = len(ws.relations_by_ends)
            es = ws.get_elements()
            for i, e in enumerate(es):
                self.reg(f"e{i}", e)
            return len(es)
        if kind == "srand":
            util.srand(args[0])
            return None
        if kind == "rand":
            return util.rand()
        if kind == "gp":
            g = Anchored.create(*self.O(args[1:]))
            self.reg(args[0], g)
            return g.as_text()
        if kind == "obj":
            o = SeqseeObject.create(*args[1:])
            self.reg(args[0], o)
            return o.as_text()
        if kind == "add":
            r = ws.add_group(self.obj[args[0]])
            return [] if r is None else [r]
        if kind == "describe":
            return 1 if util.perl_true(self.obj[args[0]].describe_as(cat(args[1]))) else 0
        if kind == "strength":
            self.obj[args[0]].set_strength(args[1])
            return None
        if kind == "reln":
            name, a, b, t = args
            r = SRelation({"first": self.obj[a], "second": self.obj[b],
                           "type": MappingNumeric.create(t, S.NUMBER)})
            self.reg(name, r)
            r.insert()
            return [r.as_text(), 1 if r in ws.relations else 0]
        if kind == "spike":
            sltm.spike_by(args[1], cat(args[0]))
            return sltm.get_real_activations_for_one_concept(cat(args[0]))
        if kind == "state":
            return self.state()
        if kind == "strengths":
            return self.strengths()
        if kind == "readhead":
            if args:
                ws.ReadHead = args[0]
            return ws.ReadHead
        if kind == "update_strengths":
            ws.update_object_strengths()
            return self.strengths()
        if kind == "obj_dist":
            return self.pairs(*ws.get_object_choice_probability_distribution())
        if kind == "rel_dist":
            return self.pairs(*ws.get_relation_choice_probability_distribution())
        if kind == "both_dist":
            v, o = ws.get_object_or_relation_choice_probability_distribution()
            return [self.pairs(v, o), len(o)]
        if kind == "read":
            c = ws.read_object_or_relation()
            return [self.nm(c), ws.ReadHead]
        if kind == "saccade":
            return [ws._saccade(), ws.ReadHead]
        if kind == "read_relation":
            return self.nm(ws.read_relation())
        if kind == "some_object_at":
            return self.nm(ws._get_some_object_at(args[0]))
        if kind == "choose_by_strength":
            return self.nm(ws.choose_by_strength(*self.O(args)))
        if kind == "some_object_at_set":
            return sorted({self.nm(ws._get_some_object_at(args[0])) or "undef"
                           for _ in range(60)})
        if kind == "read_relation_set":
            return sorted({self.nm(ws.read_relation()) or "undef" for _ in range(60)})
        if kind == "in_category":
            return self.names(ws.get_objects_belonging_to_category(cat(args[0])))
        if kind == "similar":
            r = ws.get_objects_belonging_to_similar_categories(self.obj[args[0]])
            if r is None:
                return []
            return sorted(([self.nm(k), v] for k, v in r), key=lambda p: (p[0], p[1]))
        if kind == "holes":
            return ws.are_there_holes_here(*self.O(args))
        if kind == "holes_raw":
            return ws.are_there_holes_here(*args)
        if kind == "worth_asking":
            matched, nxt, trust, acceptable = args
            Global.AcceptableTrustLevel = acceptable
            e = AskUser(already_matched=matched, next_elements=nxt)
            return e.worth_asking(trust)
        if kind == "ask":
            matched, nxt, msg, answer, *rest = args
            self.answers[:] = [answer]
            self.ui_log.clear()
            fields = {"already_matched": matched, "next_elements": nxt}
            if rest and rest[0] is not None:
                fields.update(object=self.obj[rest[0]], from_position=rest[1],
                              direction=DIRS[rest[2]])
            r = AskUser(**fields).ask(msg)
            return {
                "answer": r,
                "ui": list(self.ui_log),
                "count": ws.ElementCount,
                "trust": Global.AcceptableTrustLevel,
                "break_loop": Global.Break_Loop,
                "rejected": sorted(Global.ExtensionRejectedByUser),
            }
        if kind == "delete_inconsistent":
            ws.delete_objects_inconsistent_with(FakeRuleApp(self, args))
            return self.state()
        if kind == "delete_non_subgroups":
            of, frm = args
            opts = {}
            if of is not None:
                opts["of"] = self.O(of)
            if frm is not None:
                opts["from"] = self.O(frm)
            ws.delete_non_subgroups_of_from(opts)
            return self.state()
        raise AssertionError(f"unknown op {kind}")


@pytest.mark.parametrize("case", CASES, ids=[c["scenario"] for c in CASES])
def test_golden_scenario(case, monkeypatch):
    r = Replay(monkeypatch)
    for i, (op, want) in enumerate(zip(case["ops"], case["results"])):
        kind, args = op[0], op[1:]
        try:
            got = {"value": r.run(kind, args)}
        except (Confess, ExceptionClassBase, ZeroDivisionError) as e:
            got = {"error": err(e)}
        where = f"op {i}: {op}"
        if "error" in want:
            assert "error" in got, f"{where}: expected error {want['error']!r}, got {got}"
            assert same_error(got["error"], want["error"]), \
                f"{where}: {got['error']!r} != {want['error']!r}"
        else:
            assert "value" in got, f"{where}: unexpected error {got.get('error')!r}"
            assert same(got["value"], want["value"]), \
                f"{where}: {got['value']!r} != {want['value']!r}"


# --- tests from reading the source --------------------------------------------------------


def _ws(seq):
    sworkspace.init({"seq": seq})
    return sworkspace.get_elements()


def test_read_at_the_end_saccades():
    """Reading an object that ends at the last element saccades (toss(0.5) → 0, else a
    random position); otherwise ReadHead moves just past the object."""
    e = _ws([1, 2, 3])
    for x in e[:2]:
        x.set_strength(0)
    sworkspace.ReadHead = 2
    util.srand(7)
    sworkspace.read_object_or_relation()     # chooses e2, then saccades
    after = sworkspace.ReadHead
    util.srand(7)
    util.rand()                              # the choice
    expected = 0 if util.toss(0.5) else int(util.rand() * 3)
    assert after == expected


def test_relation_distribution_discounts_ends_in_supergroups():
    """Relation strength × 0.8 when either end has a supergroup; object strength × 0.3."""
    e = _ws([1, 2, 3, 4])
    g = Anchored.create(e[0], e[1])
    sworkspace.add_group(g)
    r1 = SRelation({"first": e[0], "second": e[1], "type": MappingNumeric.create("succ", S.NUMBER)})
    r2 = SRelation({"first": e[2], "second": e[3], "type": MappingNumeric.create("succ", S.NUMBER)})
    for r in (r1, r2):
        r.insert()
        r.set_strength(10)
    v, o = sworkspace.get_relation_choice_probability_distribution()
    d = dict(zip(o, v))
    assert d[r1] == pytest.approx(8 / 18)
    assert d[r2] == pytest.approx(10 / 18)


def test_worth_asking_returns_trust_or_zero():
    """trust += (1 - trust) × matched / (matched + asked); 0 below
    $Global::AcceptableTrustLevel, else toss(trust) ? trust : 0."""
    _ws([1])
    Global.AcceptableTrustLevel = 0.5
    e = AskUser(already_matched=[1, 2, 3], next_elements=[4])
    util.srand(3)
    r = util.rand()
    util.srand(3)
    assert e.worth_asking(0.6) == (0.9 if r < 0.9 else 0)


def test_srelation_holes_hook_uses_the_workspace():
    e = _ws([1, 2, 3])
    assert srelation._are_there_holes_here(e[0], e[2]) == 1
    assert srelation._are_there_holes_here(e[0], e[1]) == 0


def test_ask_user_extension_hook_uses_the_pluggable_callback(monkeypatch):
    from seqsee import user_interaction
    seen = []
    monkeypatch.setattr(user_interaction, "ask_user_extension",
                        lambda items, msg: seen.append((items, msg)) or 1)
    assert sworkspace._ask_user_extension([1], "msg") == 1
    assert seen == [([1], "msg")]

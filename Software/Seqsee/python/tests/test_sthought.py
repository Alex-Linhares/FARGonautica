"""Tests for the thought base class and the category and relation thoughts (item 037).

Mirrors lib/SThought.pm (create, schedule, force_to_be_next_runnable, display_self),
lib/SThought/SCat.pm (SThought::SCat) and lib/SThought/Relations.pm (SThought::SRelation).
Golden data: oracle/sthought.pl, whose scenarios are replayed here op for op.
"""
import re

import pytest

import golden
from test_sworkspace_groups import same
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sltm, sworkspace, util
from seqsee.categories.interlaced import Interlaced
from seqsee.errors import Confess, ExceptionClassBase
from seqsee.mapping.numeric import MappingNumeric
from seqsee.multimethods import perl_isa
from seqsee.objects.anchored import Anchored
from seqsee.sbindings import SBindings
from seqsee.scodelet import SCodelet
from seqsee.srelation import SRelation
from seqsee.sthought import SThought
from seqsee.sthought.relations import SThoughtSRelation
from seqsee.sthought.scat import SThoughtSCat

CASES = golden.load("sthought")

CLASSES = {"SThought::SCat": SThoughtSCat, "SThought::SRelation": SThoughtSRelation}


def err(e):
    if isinstance(e, ExceptionClassBase):
        return {"class": e.perl_name, "message": e.message or ""}
    return re.sub(r"=HASH\(0x[0-9a-f]+\)", "=HASH", str(e))


def same_error(got, want):
    # A Moose exception object (Moose::Exception::AttributeIsRequired) is a Confess here.
    if isinstance(want, dict) and want["class"].startswith("Moose::Exception::"):
        return got == want["message"]
    return got == want


def cat(name):
    if name.startswith("interlaced"):
        return Interlaced.create(int(name[len("interlaced"):]))
    return {"ascending": S.ASCENDING, "descending": S.DESCENDING, "sameness": S.SAMENESS,
            "number": S.NUMBER}[name]


class Replay:
    """The oracle's op interpreter."""

    def __init__(self):
        self.obj = {}
        self.name_of = {}

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

    def O(self, names):
        return [self.obj.get(n) for n in names]

    def core(self, spec):
        kind = spec[0]
        if kind == "obj":
            return self.obj[spec[1]]
        if kind == "cat":
            return cat(spec[1])
        if kind == "mapping":
            return MappingNumeric.create(spec[1], S.NUMBER)
        if kind == "str":
            return spec[1]
        if kind == "undef":
            return None
        raise AssertionError(kind)

    def summarize(self, v):
        if isinstance(v, list):
            return [self.nm(x) for x in v]
        return self.nm(v)

    def codelet(self, cl):
        args = cl[3]
        return [util.perl_ref(cl), cl[0], cl[1],
                [[k, self.summarize(args[k])] for k in sorted(args)]]

    def state(self):
        ws = sworkspace
        live = sorted(ws.OBJECTS.values(), key=self.nm)
        return [[self.nm(o), ws.LEFT_EDGE_OF.get(o), ws.RIGHT_EDGE_OF.get(o),
                 sorted(c.as_text() for c in o.get_categories())] for o in live]

    def run(self, kind, args):
        ws = sworkspace
        if kind == "init":
            sltm.clear()
            ws.init({"seq": args[0]})
            ws.clear_bar_lines()
            Global.Feature.clear()
            Global.Steps_Finished = 0
            self.obj.clear()
            self.name_of.clear()
            util.srand(1)
            es = ws.get_elements()
            for i, e in enumerate(es):
                self.reg(f"e{i}", e)
            return len(es)
        if kind == "srand":
            util.srand(args[0])
            return None
        if kind == "rand":
            return util.rand()
        if kind == "feature":
            Global.Feature[args[0]] = args[1]
            return None
        if kind == "gp":
            g = Anchored.create(*self.O(args[1:]))
            self.reg(args[0], g)
            return g.as_text()
        if kind == "add":
            r = ws.add_group(self.obj[args[0]])
            return [] if r is None else [r]
        if kind == "addcat":
            o = self.obj[args[0]]
            o.add_category(cat(args[1]), SBindings.create({}, {}, o))
            return None
        if kind == "reln":
            name, a, b, t = args
            r = SRelation({"first": self.obj[a], "second": self.obj[b],
                           "type": MappingNumeric.create(t, S.NUMBER)})
            self.reg(name, r)
            r.insert()
            return r.as_text()
        if kind == "spike":
            sltm.spike_by(args[1], cat(args[0]))
            return sltm.get_real_activations_for_one_concept(cat(args[0]))
        if kind == "activation":
            c = self.obj[args[1]].get_type() if args[0] == "type" else cat(args[1])
            return sltm.get_real_activations_for_one_concept(c)
        if kind == "state":
            return self.state()
        if kind == "create":
            t = SThought.create(self.core(args[1]))
            same_as = self.name_of.get(id(t))
            self.reg(args[0], t)
            return [util.perl_ref(t), t.as_text(), same_as, self.nm(t.core())]
        if kind == "create_list":
            t = SThought.create(self.core(args[1]), list_context=True)
            same_as = self.name_of.get(id(t))
            self.reg(args[0], t)
            return [util.perl_ref(t), t.as_text(), same_as]
        if kind == "new":
            cls = CLASSES[args[0]]
            t = cls({"core": self.core(args[1])} if len(args) > 1 else {})
            return [util.perl_ref(t), t.as_text(), 0 if t.stored_fringe() is None else 1]
        if kind == "stored_fringe":
            t = self.obj[args[0]]
            if len(args) > 1:
                t.stored_fringe(args[1])
            return t.stored_fringe()
        if kind == "fringe":
            return [[self.nm(x), w] for x, w in self.obj[args[0]].get_fringe()]
        if kind == "actions":
            return [self.codelet(cl) for cl in self.obj[args[0]].get_actions()]
        if kind == "actions_sorted":
            out = []
            for cl in self.obj[args[0]].get_actions():
                c = self.codelet(cl)
                c[3] = sorted((["arg", v] for _, v in c[3]), key=lambda p: p[1])
                out.append(c)
            return sorted(out, key=lambda c: ",".join(v for _, v in c[3]))
        if kind == "schedule":
            self.obj[args[0]].schedule()
            return None
        if kind == "force":
            self.obj[args[0]].force_to_be_next_runnable()
            return None
        if kind == "name":
            return CLASSES[args[0]].NAME
        raise AssertionError(f"unknown op {kind}")


@pytest.mark.parametrize("case", CASES, ids=[c["scenario"] for c in CASES])
def test_golden_scenario(case):
    replay = Replay()
    for op, want in zip(case["ops"], case["results"]):
        kind, args = op[0], op[1:]
        try:
            got = {"value": replay.run(kind, args)}
        except (Confess, ExceptionClassBase) as e:
            got = {"error": err(e)}
        if "error" in want:
            assert "error" in got, (op, got, want)
            assert same_error(got["error"], want["error"]), (op, got, want)
        else:
            assert "value" in got, (op, got, want)
            assert same(got["value"], want["value"]), (op, got, want)


# --- tests from reading the source ------------------------------------------------------

def test_create_memoizes_per_context():
    t1 = SThought.create(S.ASCENDING)
    assert SThought.create(S.ASCENDING) is t1
    t2 = SThought.create(S.ASCENDING, list_context=True)
    assert t2 is not t1
    assert SThought.create(S.ASCENDING, list_context=True) is t2
    SThought.reset()
    assert SThought.create(S.ASCENDING) is not t1


def test_create_dispatch():
    sworkspace.init({"seq": [1, 2, 3, 4]})
    e = sworkspace.get_elements()
    r = SRelation({"first": e[0], "second": e[1], "type": MappingNumeric.create("succ", S.NUMBER)})
    assert type(SThought.create(r)) is SThoughtSRelation
    assert type(SThought.create(S.SAMENESS)) is SThoughtSCat
    assert type(SThought.create(Interlaced.create(4))) is SThoughtSCat
    # ContinueWith (Seqsee/SCF.pm) checks isa SThought.
    assert perl_isa(SThought.create(S.SAMENESS), "SThought")
    assert perl_isa(SThought.create(r), "SThought")
    # The table is keyed by the exact ref, so a subclass isn't in it.
    with pytest.raises(Confess, match=r"^Don't know how to think about Seqsee::Anchored::Sub=HASH"):
        SThought.create(object.__new__(_FakeAnchoredSub))


class _FakeAnchoredSub(Anchored):
    perl_name = "Seqsee::Anchored::Sub"


def test_schedule_and_force_die_like_perl():
    t = SThoughtSCat({"core": S.ASCENDING})
    with pytest.raises(Confess, match='"schedule_thought" via package "SCoderack"'):
        t.schedule()
    with pytest.raises(Confess, match='"force_thought" via package "SCoderack"'):
        t.force_to_be_next_runnable()


def test_core_is_rw_and_required():
    t = SThoughtSCat({"core": S.ASCENDING})
    assert t.core() is S.ASCENDING
    assert t.core(S.DESCENDING) is S.DESCENDING
    assert t.as_text() == "Category descending"
    t2 = SThoughtSCat(core=S.SAMENESS, stored_fringe=[1])
    assert t2.stored_fringe() == [1]
    with pytest.raises(Confess, match=r"^Attribute \(core\) is required"):
        SThoughtSRelation()


def test_relation_fringe_needs_core():
    t = SThoughtSRelation({"core": S.ASCENDING})
    t.core(None)
    with pytest.raises(Confess, match="^Core is empty!"):
        t.get_fringe()


def test_display_self():
    calls = []

    class Widget:
        def Display(self, *args):
            calls.append(args)

    SThoughtSCat({"core": S.ASCENDING}).display_self(Widget())
    assert calls == [("Thought", ["heading"], "\n", "Category ascending")]


def test_scat_actions_are_merge_codelets():
    sworkspace.init({"seq": list(range(1, 9))})
    e = sworkspace.get_elements()
    a = Anchored.create(e[1], e[2])
    sworkspace.add_group(a)
    s1 = Anchored.create(a, e[3])
    sworkspace.add_group(s1)
    s2 = Anchored.create(e[0], a)
    sworkspace.add_group(s2)
    for g in (s1, s2):
        g.add_category(S.ASCENDING, SBindings.create({}, {}, g))
    (cl,) = SThought.create(S.ASCENDING).get_actions()
    assert isinstance(cl, SCodelet)
    assert cl[0] == "MergeGroups" and cl[1] == 100
    assert {id(cl[3]["a"]), id(cl[3]["b"])} == {id(s1), id(s2)}

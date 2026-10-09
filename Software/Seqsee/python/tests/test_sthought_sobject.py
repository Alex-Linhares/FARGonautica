"""Tests for the group and element thoughts (item 038).

Mirrors lib/SThought/SObject.pm: SThought::Seqsee::Anchored (get_fringe_for on a group,
StrengthenLink, ExtendFromMemory, AddCategoriesFromMemory, IsThisAMountainUpslope,
get_fringe, get_actions, as_text) and SThought::Seqsee::Element (magnitude,
get_fringe_for on an element, get_actions, as_text), plus SThought->create on elements
and groups. Golden data: oracle/sthought_sobject.pl, whose scenarios are replayed here op
for op.
"""
import re

import pytest

import golden
from test_sthought import Replay as _BaseReplay
from test_sthought import err, same_error
from test_sworkspace_groups import same
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sltm, sworkspace, util
from seqsee.categories.interlaced import Interlaced
from seqsee.errors import Confess, ExceptionClassBase
from seqsee.mapping import find_mapping
from seqsee.mapping.numeric import MappingNumeric
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.sbindings import SBindings
from seqsee.scodelet import SCodelet
from seqsee.smetonym import SMetonym
from seqsee.smetonym_type import SMetonymType
from seqsee.srelation import SRelation
from seqsee.sthought import SThought
from seqsee.sthought import sobject
from seqsee.sthought.sobject import SThoughtSeqseeAnchored, SThoughtSeqseeElement

CASES = golden.load("sthought_sobject")

CLASSES = {"SThought::Seqsee::Element": SThoughtSeqseeElement,
           "SThought::Seqsee::Anchored": SThoughtSeqseeAnchored}


def cat(name):
    if name.startswith("interlaced"):
        return Interlaced.create(int(name[len("interlaced"):]))
    return {"ascending": S.ASCENDING, "descending": S.DESCENDING, "sameness": S.SAMENESS,
            "number": S.NUMBER, "prime": S.PRIME, "odd": S.ODD, "even": S.EVEN,
            "mountain": S.MOUNTAIN}[name]


def mapping(name, catname=None):
    return MappingNumeric.create(name, cat(catname or "number"))


class Replay(_BaseReplay):
    """The oracle's op interpreter (sthought.pl's, plus this oracle's ops)."""

    def nm(self, o):
        # Perl has no as_text for these, so the oracle shows the class name.
        if isinstance(o, SMetonymType) or (o is not None and not isinstance(o, (str, int, float))
                                           and not hasattr(o, "as_text")):
            if id(o) not in self.name_of:
                return "?" + util.perl_ref(o)
        return super().nm(o)

    def core(self, spec):
        if spec[0] == "cat":
            return cat(spec[1])
        return super().core(spec)

    def codelet(self, cl):
        if not util.perl_ref(cl):
            return ["NONREF", cl]
        return super().codelet(cl)

    def fringe_entries(self, fringe):
        return [[self.nm(x), w] for x, w in fringe]

    def run(self, kind, args):
        ws = sworkspace
        if kind == "feature":
            if args[1] == "none":
                Global.Feature.pop(args[0], None)
            else:
                Global.Feature[args[0]] = args[1]
            return None
        if kind == "describe":
            r = self.obj[args[0]].describe_as(cat(args[1]))
            return 0 if r is None or r is False else 1
        if kind == "addcat":
            o = self.obj[args[0]]
            o.add_category(cat(args[1]), SBindings.create({}, {}, o))
            return None
        if kind == "addcat_meto":
            name, catname, index = args
            o = self.obj[name]
            m = SMetonym({"category": S.SAMENESS, "name": "each", "info_loss": {"length": 2},
                          "starred": Element.create(1, -1), "unstarred": o[index]})
            o.add_category(cat(catname), SBindings.create({index: m}, {}, o))
            return o.get_binding_for_category(cat(catname)).get_metonymy_mode().as_text()
        if kind == "set_metonym":
            name, mag = args
            o = self.obj[name]
            m = SMetonym({"category": S.SAMENESS, "name": "each",
                          "info_loss": {"length": len(o)},
                          "starred": Element.create(mag, -1), "unstarred": o})
            o.SetMetonym(m)
            o.SetMetonymActiveness(1)
            return o.get_effective_object().as_text()
        if kind == "reln":
            name, a, b, t = args[:4]
            r = SRelation({"first": self.obj[a], "second": self.obj[b],
                           "type": mapping(t, args[4] if len(args) > 4 else None)})
            self.reg(name, r)
            r.insert()
            return r.as_text()
        if kind == "relnf":
            name, a, b = args
            t = find_mapping(self.obj[a], self.obj[b])
            if not util.perl_true(t):
                return None
            r = SRelation({"first": self.obj[a], "second": self.obj[b], "type": t})
            self.reg(name, r)
            r.insert()
            return r.as_text()
        if kind == "ruleapp":
            ra = self.obj[args[0]].set_underlying_ruleapp(self.obj[args[1]])
            return None if ra is None else util.perl_ref(ra)
        if kind == "spike":
            sltm.spike_by(args[1], cat(args[0]))
            return sltm.get_real_activations_for_one_concept(cat(args[0]))
        if kind == "activation":
            k, v = args
            if k == "type":
                c = self.obj[v].get_type()
            elif k == "obj":
                c = self.obj[v]
            elif k == "transform":
                c = self.obj[v].get_underlying_reln().get_rule().get_transform()
            else:
                c = cat(v)
            return sltm.get_real_activations_for_one_concept(c)
        if kind == "follows":
            catname, mname, mcat, amount = args
            sla_spike(sltm.insert_follows_link(cat(catname), mapping(mname, mcat)), amount)
            return None
        if kind == "follows_type":
            catname, r, amount = args
            sla_spike(sltm.insert_follows_link(cat(catname), self.obj[r].get_type()), amount)
            return None
        if kind == "isa":
            o, catname, amount = args
            sla_spike(sltm.insert_isa_link(self.obj[o], cat(catname)), amount)
            return None
        if kind == "link_activation":
            k, frm, to = args
            if k == "isa":
                f, t, type_ = self.obj[frm], cat(to), sltm.LTM_IS
            else:
                f, t, type_ = cat(frm), self.obj[to], sltm.LTM_FOLLOWS
            fi = sltm.get_memory_index(f)
            ti = sltm.get_memory_index(t)
            links = sltm.OUT_LINKS[fi]
            table = links[type_] if type_ < len(links) and links[type_] else {}
            link = table.get(sltm._hash_key(ti))
            return None if link is None else [link[0], link[3]]
        if kind == "node_count":
            return sltm.NodeCount
        if kind == "set_metonym_activeness":
            self.obj[args[0]].set_metonym_activeness(args[1])
            return None
        if kind == "new":
            klass = CLASSES[args[0]]
            t = klass({"core": self.core(args[1])} if len(args) > 1 else {})
            return [util.perl_ref(t), t.as_text(), 0 if t.stored_fringe() is None else 1]
        if kind == "magnitude":
            t = self.obj[args[0]]
            if len(args) > 1:
                t.magnitude(args[1])
            return t.magnitude()
        if kind == "fringe":
            return self.fringe_entries(self.obj[args[0]].get_fringe())
        if kind == "fringe_sorted":
            f = self.fringe_entries(self.obj[args[0]].get_fringe())
            return sorted(f, key=lambda p: f"{p[0]} {util.perl_str(p[1])}")
        if kind == "upslope":
            return self.nm(sobject.is_this_a_mountain_upslope(self.obj[args[0]]))
        if kind == "strengthen":
            r = sobject.strengthen_link(*self.O(args))
            return 0 if r is None else 1
        if kind == "add_from_memory":
            return [self.codelet(c) for c in sobject.add_categories_from_memory(self.obj[args[0]])]
        if kind == "extend_from_memory":
            return [self.codelet(c) for c in sobject.extend_from_memory(self.obj[args[0]])]
        if kind == "name":
            return CLASSES[args[0]].NAME
        return super().run(kind, args)


def sla_spike(link, amount):
    from seqsee import slink_activation as sla
    sla.spike(link, amount)


_SLIPPAGES = re.compile(r"\] ((?:\w+ => \w+;)*\w+ => \w+)")


def _norm(value):
    """Sort the "k => v;..." slippage lists in Mapping::Structural texts (Perl hash order)."""
    if isinstance(value, str):
        return _SLIPPAGES.sub(lambda m: "] " + ";".join(sorted(m.group(1).split(";"))), value)
    if isinstance(value, list):
        return [_norm(v) for v in value]
    return value


def _strip_alternating(value):
    """Drop CheckIfAlternating codelets: which neighbour is chosen (a group or the element
    with the same edge) depends on Perl's hash order. The draw is used up either way."""
    return [c for c in value if not (isinstance(c, list) and len(c) > 1
                                     and c[1] == "CheckIfAlternating")]


@pytest.mark.parametrize("case", CASES, ids=[c["scenario"] for c in CASES])
def test_golden_scenario(case):
    replay = Replay()
    alternating = case["scenario"].startswith("alternating")
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
            g, w = _norm(got["value"]), _norm(want["value"])
            if alternating and kind == "actions":
                g, w = _strip_alternating(g), _strip_alternating(w)
            assert same(g, w), (op, got, want)


# --- tests from reading the source ------------------------------------------------------

def _groups(seq, *spans):
    sworkspace.init({"seq": seq})
    e = sworkspace.get_elements()
    out = []
    for lo, hi in spans:
        g = Anchored.create(*e[lo:hi + 1])
        sworkspace.add_group(g)
        out.append(g)
    return e, out


def test_create_dispatches_elements_and_groups():
    e, (g,) = _groups([1, 2, 3, 4], (1, 2))
    assert type(SThought.create(e[0])) is SThoughtSeqseeElement
    assert type(SThought.create(g)) is SThoughtSeqseeAnchored
    assert SThought.create(g, list_context=True) is not SThought.create(g)
    assert SThought.create(e[0]).as_text() == "Element " + e[0].as_text()
    assert SThought.create(g).as_text() == "Group " + g.as_text()


def test_magnitude_is_lazy():
    e, _ = _groups([4, 5], )
    t = SThoughtSeqseeElement({"core": e[1]})
    t.core(e[0])
    assert t.magnitude() == 4          # built on first read, from the current core
    t.core(e[1])
    assert t.magnitude() == 4          # and then kept
    assert SThoughtSeqseeElement(core=e[1], magnitude=9).magnitude() == 9


def test_alternating_neighbours_that_are_groups():
    # With only groups at the neighbouring edges' ends (no element ends there), the
    # CheckIfAlternating codelet doesn't depend on hash order.
    e, (a, b, c) = _groups([1, 2, 1, 2, 1, 2, 7], (0, 1), (2, 3), (4, 5))
    for g in (a, b, c):
        g.add_category(S.ASCENDING, SBindings.create({}, {}, g))
    Global.Feature["Alternating"] = 1
    found = set()
    for seed in range(1, 30):
        util.srand(seed)
        acts = SThoughtSeqseeAnchored({"core": b}).get_actions()
        for cl in acts:
            if cl[0] == "CheckIfAlternating":
                args = cl[3]
                assert args["second"] is b
                found.add((id(args["first"]), id(args["third"])))
    assert (id(a), id(c)) in found
    # The elements at the edges have no common category with b, so they never appear.
    assert found == {(id(a), id(c))}


def test_extend_from_memory_falls_through_with_feature_value():
    e, (a, b) = _groups([1, 2, 3, 2, 3, 4, 9], (0, 2), (3, 5))
    for g in (a, b):
        g.describe_as(S.ASCENDING)
    sltm.spike_by(100, S.ASCENDING)
    t = find_mapping(a, b)
    sla_spike(sltm.insert_follows_link(S.ASCENDING, t), 50)
    assert sobject.extend_from_memory(a) == [None]
    Global.Feature["LTM_expt"] = ""
    assert sobject.extend_from_memory(a) == [""]


def test_extend_from_memory_needs_followers():
    e, (a,) = _groups([1, 2, 3, 4], (0, 1))
    assert sobject.extend_from_memory(a) == []


def test_flush_right_follower_asks_the_user(monkeypatch):
    # Asking is SErr::ElementsBeyondKnownSought->Ask, answered by the UI callback.
    from seqsee import user_interaction
    asked = []
    monkeypatch.setattr(user_interaction, "boolean_response", lambda *a: asked.append(a))
    e, (a, b) = _groups([1, 2, 3, 2, 3, 4], (0, 2), (3, 5))
    for g in (a, b):
        g.describe_as(S.ASCENDING)
    sltm.spike_by(100, S.ASCENDING)
    sla_spike(sltm.insert_follows_link(S.ASCENDING, find_mapping(a, b)), 50)
    # b is flush right, but not flush left, and there are more than 3 elements.
    assert sobject.extend_from_memory(b) == [None]
    assert asked == []
    # With 3 elements, a spans everything (flush right and left): ask.
    monkeypatch.setattr(sworkspace, "ElementCount", 3)
    sobject.extend_from_memory(a)
    assert len(asked) == 1


def test_get_fringe_for_needs_an_object():
    with pytest.raises(Confess, match="No viable candidate"):
        sobject.get_fringe_for(S.ASCENDING)


def test_actions_are_codelets():
    e, (g,) = _groups([1, 2, 3, 4, 5, 6], (2, 3))
    util.srand(1)
    acts = SThought.create(g).get_actions()
    assert all(isinstance(cl, SCodelet) for cl in acts)
    assert [cl[0] for cl in acts][-2:] == ["LookForSimilarGroups", "CleanUpGroup"]
    assert SThought.create(e[0]).get_actions() == []

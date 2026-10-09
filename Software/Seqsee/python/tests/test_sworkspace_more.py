"""Tests for SWorkspace.pm, part III (item 033): distances and positions, position
structures, sameness groups, check_at_location, rapid_create_gp, attribute copying,
plonking, GetSomethingLike and LookForSomethingLike.

Mirrors lib/SWorkspace.pm (__GetLongestNonAdHocWithEndsExactly,
__GetLongestNonAdHocWithLeftExactRightBelow, __FindDistance, __FindDistanceHelper_,
__GetPositionInDirectionAtDistance, get_longest_non_adhoc_object_starting_at/ending_at,
get_intervening_objects, __GetPositionStructure(AsString), __GetSamenessAround,
__CreateSamenessGroupAround, check_at_location, CheckElementsRightwardFromLocation,
rapid_create_gp, __CopyAttributes, __PlonkIntoPlace, GetSomethingLike,
LookForSomethingLike). Golden data: oracle/sworkspace_more.pl, whose scenarios are
replayed here op for op.
"""
import pytest

import golden
from test_sworkspace_groups import same, same_error
from seqsee import global_ as Global
from seqsee import position_structure as ps_mod
from seqsee import s as S
from seqsee import sltm, srule_app, sworkspace, util
from seqsee.categories.interlaced import Interlaced
from seqsee.constants import DIR, DISTANCE, DISTANCE_MODE, RELN_SCHEME
from seqsee.errors import Confess, ElementsBeyondKnownSought, ExceptionClassBase
from seqsee.objects.anchored import Anchored
from seqsee.objects.object import SeqseeObject
from seqsee.position_structure import PositionStructure

CASES = golden.load("sworkspace_more")

DIRS = {"LEFT": DIR.LEFT, "RIGHT": DIR.RIGHT, "UNKNOWN": DIR.UNKNOWN, "NEITHER": DIR.NEITHER}


def err(e):
    if isinstance(e, ExceptionClassBase):
        r = {"class": e.perl_name, "message": e.message or ""}
        if isinstance(e, ElementsBeyondKnownSought):
            r["next_elements"] = list(e.next_elements)
        return r
    return str(e)


def cat(name):
    if name.startswith("interlaced"):
        return Interlaced.create(int(name[len("interlaced"):]))
    return {"ascending": S.ASCENDING, "descending": S.DESCENDING, "sameness": S.SAMENESS,
            "number": S.NUMBER, "mountain": S.MOUNTAIN}[name]


def mode(m):
    if m is None:
        return None
    return DISTANCE_MODE.GROUP if m == "group" else DISTANCE_MODE.ELEMENT


def dist_out(d):
    return [d.get_magnitude(), d.mode.mode]


def as_list(r):
    """A Perl sub called in list context: ``return;`` gives ()."""
    return [] if r is None else [r]


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

    def names(self, objs):
        return sorted(self.nm(o) for o in objs)

    def O(self, names):
        return [self.obj.get(n) if n is not None else None for n in names]

    def describe_obj(self, o):
        m = o.get_metonym()
        return {
            "text": o.as_text(),
            "cats": sorted(c.get_name() for c in o.get_categories()),
            "group_p": 1 if util.perl_true(o.get_group_p()) else 0,
            "meto": [m.get_category().get_name(), m.get_name()] if util.perl_true(m) else None,
            "meto_active": 1 if util.perl_true(o.get_metonym_activeness()) else 0,
            "live": 1 if sworkspace.check_liveness(o) else 0,
        }

    def rapid_args(self, spec):
        cats_spec, *items = spec
        out = []
        c = list(cats_spec)
        while c:
            nxt = c.pop(0)
            if nxt == "metonym":
                out += [nxt, cat(c.pop(0)), c.pop(0)]
            else:
                out.append(cat(nxt))
        return [out, *[self.rapid_args(i) if isinstance(i, list) else self.obj[i]
                       for i in items]]

    def state(self):
        ws = sworkspace
        live = sorted(ws.OBJECTS.values(), key=self.nm)
        return [[self.nm(o), ws.LEFT_EDGE_OF.get(o), ws.RIGHT_EDGE_OF.get(o)] for o in live]

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
        if kind == "obj":
            o = SeqseeObject.create(*args[1:])
            self.reg(args[0], o)
            return [util.perl_ref(o), o.as_text()]
        if kind == "add":
            return as_list(ws.add_group(self.obj[args[0]]))
        if kind == "remove":
            ws.remove_gp(self.obj[args[0]])
            return None
        if kind == "describe":
            return 1 if util.perl_true(self.obj[args[0]].describe_as(cat(args[1]))) else 0
        if kind == "metonym":
            o = self.obj[args[0]]
            o.annotate_with_metonym(cat(args[1]), args[2])
            o.SetMetonymActiveness(1)
            return None
        if kind == "meto_off":
            self.obj[args[0]].SetMetonymActiveness(0)
            return None
        if kind == "reln_scheme":
            self.obj[args[0]].set_reln_scheme(RELN_SCHEME.CHAIN)
            return None
        if kind == "info":
            return self.describe_obj(self.obj[args[0]])
        if kind == "state":
            return self.state()
        if kind == "distance":
            a, b, *m = args
            return dist_out(ws.find_distance(self.obj[a], self.obj[b],
                                             mode(m[0] if m else None)))
        if kind == "distance_helper":
            return dist_out(ws._find_distance_helper(args[0], args[1], mode(args[2])))
        if kind == "pos_at":
            frm, d, mag, m = args
            dist = DISTANCE.in_groups(mag) if m == "group" else DISTANCE.in_elements(mag)
            r = ws.get_position_in_direction_at_distance(
                {"from_object": self.obj.get(frm), "direction": DIRS.get(d), "distance": dist})
            return ["huge" if x > 1e6 else x for x in as_list(r)]
        if kind == "pos_at_raw":
            frm, d, distance = args
            return as_list(ws.get_position_in_direction_at_distance(
                {"from_object": self.obj.get(frm), "direction": DIRS.get(d),
                 "distance": distance}))
        if kind == "longest_exact":
            return self.nm(ws.get_longest_non_adhoc_with_ends_exactly(*args))
        if kind == "longest_lerb":
            return self.nm(ws.get_longest_non_adhoc_with_left_exact_right_below(*args))
        if kind == "longest_start":
            return self.nm(ws.get_longest_non_adhoc_object_starting_at(*args))
        if kind == "longest_end":
            return self.nm(ws.get_longest_non_adhoc_object_ending_at(*args))
        if kind == "intervening":
            return [self.nm(o) for o in ws.get_intervening_objects(*args)]
        if kind == "posstruct":
            return ws.get_position_structure(self.obj[args[0]])
        if kind == "posstruct_str":
            return ws.get_position_structure_as_string(self.obj[args[0]])
        if kind == "posstruct_obj":
            return list(PositionStructure.create(self.obj[args[0]]))
        if kind == "sameness_around":
            return list(ws.get_sameness_around(args[0]))
        if kind == "sameness_group":
            before = set(map(id, ws.NON_ELT_OBJECTS))
            r = ws.create_sameness_group_around(args[0])
            new = [g for g in ws.NON_ELT_OBJECTS if id(g) not in before]
            if len(new) == 1 and len(args) > 1:
                self.reg(args[1], new[0])
            return [as_list(r), [self.describe_obj(g) for g in new]]
        if kind == "check_at":
            start, d, what = args
            return as_list(ws.check_at_location(
                {"start": start, "direction": DIRS.get(d), "what": self.obj[what]}))
        if kind == "check_rightward":
            return as_list(ws.check_elements_rightward_from_location(args[0], args[1]))
        if kind == "rapid":
            o = ws.rapid_create_gp(*self.rapid_args(args[1]))
            self.reg(args[0], o)
            return self.describe_obj(o)
        if kind in ("copy_attr", "copy_attr_missing"):
            r = ws.copy_attributes({"from": self.obj.get(args[0]), "to": self.obj.get(args[1])})
            if kind == "copy_attr_missing":
                return None
            return [r.success(), self.describe_obj(self.obj[args[1]])]
        if kind == "plonk":
            name, start, d, what = args
            r = ws.plonk_into_place(start, DIRS[d], self.obj[what])
            res = r.resultant_object()
            if res is not None and id(res) not in self.name_of:
                self.reg(name, res)
            out = {"ok": 1 if r.plonk_was_successful() else 0, "resultant": self.nm(res),
                   "copy": r.attribute_copy_result().success(),
                   "plonked": self.nm(r.object_being_plonked())}
            if res is not None:
                out["info"] = self.describe_obj(res)
            return out
        if kind == "something_like":
            what, start, d, trust, *reason = args
            opts = {"object": self.obj.get(what), "start": start, "direction": DIRS.get(d),
                    "trust_level": trust, "reason": reason[0] if reason else None}
            return [self.nm(o) for o in as_list(ws.get_something_like(opts))]
        if kind == "look_like":
            what, start, d = args
            r = ws.look_for_something_like({"object": self.obj.get(what),
                                            "start_position": start,
                                            "direction": DIRS.get(d)})
            ask = r.get_to_ask()
            lit = r.get_literally_present()
            return {
                "to_ask": ({"expected": self.nm(ask["expected_object"]),
                            "start": ask["start_position"], "exception": err(ask["exception"])}
                           if ask else ask),
                "literally_present": ([lit[0], {v: k for k, v in DIRS.items()}[lit[1]],
                                       self.nm(lit[2])] if isinstance(lit, list) else lit),
                "probable": self.names(r.get_probable_matches()),
                "potential": self.names(r.get_potential_matches()),
            }
        raise AssertionError(f"unknown op {kind}")


@pytest.mark.parametrize("case", CASES, ids=[c["scenario"] for c in CASES])
def test_golden_scenario(case):
    r = Replay()
    for i, (op, want) in enumerate(zip(case["ops"], case["results"])):
        kind, args = op[0], op[1:]
        try:
            got = {"value": r.run(kind, args)}
        except (Confess, ExceptionClassBase) as e:
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


def test_unrequested_group_distance_is_converted_to_elements():
    """PERL-QUIRK: __FindDistanceHelper_ advances by the group's *left* edge + 1, so a group
    distance always counts elements, and an unrequested GROUP mode is always relabelled
    as ELEMENT. PickOne still makes its one draw, even for adjacent objects."""
    e = _ws([1, 2, 3, 4, 5, 6])
    g = Anchored.create(e[1], e[2], e[3])
    sworkspace.add_group(g)
    g.describe_as(S.ASCENDING)
    assert dist_out(sworkspace.find_distance(e[0], e[5], DISTANCE_MODE.GROUP)) == [4, "group"]
    util.srand(9)
    before = util.rand()
    util.srand(9)
    assert dist_out(sworkspace.find_distance(e[0], e[1])) == [0, "group"]
    assert util.rand() != before  # one draw was used


def test_position_right_in_element_units_adds_an_address():
    """PERL-QUIRK: ``$end + $distance`` numifies the DISTANCE ref (its address)."""
    e = _ws([1, 2, 3])
    d = DISTANCE.in_elements(1)
    r = sworkspace.get_position_in_direction_at_distance(
        {"from_object": e[0], "direction": DIR.RIGHT, "distance": d})
    assert r > 1e6
    assert sworkspace.get_position_in_direction_at_distance(
        {"from_object": e[2], "direction": DIR.LEFT, "distance": d}) is None


def test_strength_chooser_ignores_strengths(monkeypatch):
    """PERL-QUIRK: ``\\&SFasc::get_strength`` on a Seqsee object reads an unset Class::Std
    attribute (undef), so the chooser picks uniformly: one draw, index int(rand * n)."""
    e = _ws([1, 2, 3])
    e[0].set_strength(0)
    e[1].set_strength(100)
    util.srand(5)
    r = util.rand()
    util.srand(5)
    assert sworkspace._strength_chooser([e[0], e[1]]) is [e[0], e[1]][int(r * 2)]


def test_get_something_like_asks_when_trusted(monkeypatch):
    """On ElementsBeyondKnownSought, toss(trust_level * 0.02) decides whether to ask; a
    refusal returns nothing. Hilit gets the hilit_set."""
    _ws([1, 2, 3])
    x = SeqseeObject.create(3, 4)
    calls = []
    monkeypatch.setattr(ElementsBeyondKnownSought, "ask",
                        lambda self, *a: calls.append(("ask", a)) or 0)
    monkeypatch.setattr(Global, "hilit", lambda v, *objs: calls.append(("hilit", v, objs)))
    r = sworkspace.get_something_like({"object": x, "start": 2, "direction": DIR.RIGHT,
                                       "trust_level": 50, "reason": "Because",
                                       "hilit_set": ["h"]})
    assert r is None
    assert calls == [("hilit", 1, ("h",)), ("ask", ("Because. ", ""))]


def test_get_something_like_squints(monkeypatch):
    """With the AllowSquinting feature, each potential match gets a TryToSquint codelet
    (unless a literally present object was returned first)."""
    e = _ws([1, 2, 3])
    x = SeqseeObject.create(2, 9)
    made = []
    monkeypatch.setattr(sworkspace, "_scodelet_new", lambda *a: ("codelet", a))
    monkeypatch.setattr(sworkspace, "_coderack_add_codelet", made.append)
    Global.Feature["AllowSquinting"] = 1
    util.srand(1)
    sworkspace.get_something_like({"object": x, "start": 1, "direction": DIR.RIGHT,
                                   "trust_level": 0.5})
    assert made == [("codelet", ("TryToSquint", 200, {"actual": e[1], "intended": x}))]


def test_rapid_create_gp_consumes_the_cats_list():
    """Perl shifts the categories off the caller's array."""
    e = _ws([1, 2, 3])
    cats = [S.ASCENDING]
    g = sworkspace.rapid_create_gp(cats, e[0], e[1], e[2])
    assert cats == []
    assert sworkspace.check_liveness(g)


def test_hooks_point_at_the_workspace():
    e = _ws([4, 5, 6])
    g = Anchored.create(e[0], e[1])
    assert ps_mod._get_position_structure(g) == [0, 1]
    assert srule_app._check_at_location(
        {"start": 0, "direction": DIR.RIGHT, "what": SeqseeObject.create(4, 5)}) == 1
    util.srand(1)
    assert srule_app._get_something_like(
        {"object": SeqseeObject.create(9), "start": 0, "direction": DIR.RIGHT,
         "trust_level": 0}) is None

"""Tests for SRelation and SRelation::Structural (item 024): Perl lib/SRelation.pm and
lib/SRelation/Structural.pm.

Golden data comes from oracle/srelation.pl (tests/golden/srelation.json). The oracle
replaces SWorkspace->AddRelation/RemoveRelation and SLTM::GetRealActivationsForOneConcept
with recorders; here the same recorders are wired in through the hooks in ``srelation``.
SWorkspace->are_there_holes_here is the real one in both. FakeMapping mirrors the
oracle's package.
"""
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import srelation as srelation_mod
from seqsee import util
from seqsee.categories.mapping_based import MappingBased
from seqsee.constants import DIR, METO_MODE, RELN_SCHEME
from seqsee.errors import Confess, ExceptionClassBase, SErr
from seqsee.mapping import Mapping
from seqsee.mapping.dir import MappingDir
from seqsee.mapping.numeric import MappingNumeric
from seqsee.mapping.structural import MappingStructural
from seqsee.objects import object as object_mod
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.objects.object import SeqseeObject
from seqsee.shistory import SHistory
from seqsee.srelation import SRelation, SRelationStructural

CASES = golden.load("srelation")


def cases(kind, **match):
    found = [c for c in CASES if c["kind"] == kind and all(c.get(k) == v for k, v in match.items())]
    assert found, (kind, match)
    return found


def one(kind, **match):
    found = cases(kind, **match)
    assert len(found) == 1, (kind, match)
    return found[0]


# --- recorders (the oracle's) --------------------------------------------------------------

LOG = []
ACT = {}


class World:
    add = 1
    add_dies = None


def _activation(concept):
    LOG.append(["activation", concept.as_text()])
    return ACT.get(concept.as_text())


def _ws_add(reln):
    LOG.append(["ws_add", "SWorkspace", reln.as_text()])
    if World.add_dies is not None:
        raise World.add_dies
    return World.add


def _ws_remove(reln):
    LOG.append(["ws_remove", "SWorkspace", reln.as_text()])
    return "REMOVED"


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    LOG.clear()
    ACT.clear()
    World.add, World.add_dies = 1, None
    monkeypatch.setattr(srelation_mod, "_get_real_activations_for_one_concept", _activation)
    monkeypatch.setattr(srelation_mod, "_workspace_add_relation", _ws_add)
    monkeypatch.setattr(srelation_mod, "_workspace_remove_relation", _ws_remove)
    # Group creation runs Seqsee::Object's UpdateStrength (real SLTM in the oracle).
    monkeypatch.setattr(object_mod, "_get_real_activations_for_concepts",
                        lambda cats: [0 for _ in cats])
    # FindMapping on elements picks a category with SLTM::SpikeAndChoose (item 029).
    from seqsee import mapping
    monkeypatch.setattr(mapping, "_spike_and_choose", lambda *a: None, raising=False)


def take_log():
    out = list(LOG)
    LOG.clear()
    return out


class FakeMapping(Mapping):
    """The oracle's FakeMapping: a Mapping whose FlippedVersion is scripted."""

    perl_name = "FakeMapping"

    def __init__(self, name, cat, flip=None):
        self.name, self.cat, self.flip = name, cat, flip

    def flipped_version(self):
        return self.flip

    def as_text(self):
        return "fake " + self.name

    def get_category(self):
        return self.cat

    def get_name(self):
        return self.name


def E(mag, pos):
    return Element.create(mag, pos)


def G(*items):
    return Anchored.create(*items)


def world():
    obj = {"e0": E(5, 0), "e1": E(6, 1), "e2": E(7, 2), "e3": E(8, 3), "e5": E(9, 5), "e1b": E(6, 1)}
    obj["g01"] = G(obj["e0"], obj["e1"])
    obj["g23"] = G(obj["e2"], obj["e3"])
    obj["obj"] = SeqseeObject.create(1, 2)
    types = {
        "succ": MappingNumeric.create("succ", S.NUMBER),
        "pred": MappingNumeric.create("pred", S.NUMBER),
        "same": MappingNumeric.create("same", S.NUMBER),
        "foo": MappingNumeric.create("foo", S.NUMBER),
        "evensucc": MappingNumeric.create("succ", S.EVEN),
        "dir": DIR.RIGHT,
        "fake": FakeMapping("x", S.NUMBER),
        "fakeflip": FakeMapping("y", S.PRIME, flip=FakeMapping("yflip", S.PRIME)),
        "struct": MappingStructural.create({
            "category": S.ASCENDING, "meto_mode": METO_MODE.NONE,
            "direction_reln": MappingDir.create("Same"),
            "changed_bindings": {"start": MappingNumeric.create("succ", S.NUMBER)},
            "slippages": {}}),
    }
    return obj, types


def value(name, obj, types):
    if name is None:
        return None
    if isinstance(name, str):
        if name in obj:
            return obj[name]
        if name in types:
            return types[name]
        if name == "ARRAY":
            return []
        if name == "HASH":
            return {}
        if name == "SHistory":
            return SHistory()
    return name


_REF_RE = re.compile(r"=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)")


def err(e):
    """The oracle's err(): Moose and plain dies are read as class DIE."""
    if e is None:
        return None
    msg = _REF_RE.sub("=REF", e.message if isinstance(e, ExceptionClassBase) else str(e))
    return {"class": "SErr" if isinstance(e, SErr) else "DIE", "message": msg}


def golden_err(g):
    if g is None:
        return None
    cls = g["class"]
    if cls.startswith("Moose::Exception::"):
        cls = "DIE"
    return {"class": cls, "message": g["message"]}


def describe(r):
    return {
        "ref": r.perl_name,
        "as_text": r.as_text(),
        "strength": r.get_strength(),
        "holeyness": r.get_holeyness(),
        "dir_reln": r.get_direction_reln(),
        "history": list(r.get_history()),
        "extent": list(r.get_extent()),
        "span": r.get_span(),
        "contig": r.are_ends_contiguous(),
        "direction": r.get_direction().text,
        "pure_is_type": 1 if r.get_pure() is r.get_type() else 0,
    }


def make(cls, args, obj, types):
    h = {args[i]: value(args[i + 1], obj, types) for i in range(0, len(args), 2)}
    try:
        return cls(h), None
    except Exception as e:  # noqa: BLE001
        return None, e


# --- Moose new -----------------------------------------------------------------------------

NEW = [c for c in CASES if c["kind"] == "new"]


@pytest.mark.parametrize("case", NEW, ids=[f"{c['class']}-{i}" for i, c in enumerate(NEW)])
def test_new(case):
    obj, types = world()
    cls = SRelation if case["class"] == "SRelation" else SRelationStructural
    r, e = make(cls, case["args"], obj, types)
    if case["err"] is not None:
        assert r is None
        assert err(e) == golden_err(case["err"])
        return
    assert e is None, e
    assert describe(r) == case["obj"]
    if cls is SRelationStructural:
        assert sorted(r.get_unchanged_bindings()) == case["unchanged"]
        assert r.no_unchanged_bindings() == case["no_unchanged"]


def test_new_kwargs():
    obj, types = world()
    c = one("new_list")
    r = SRelation(first=obj["e0"], second=obj["e1"], type=types["succ"])
    assert r.as_text() == c["as_text"]


# --- accessors and writers -----------------------------------------------------------------

@pytest.mark.parametrize("case", cases("accessors"), ids=lambda c: "-".join(c["pair"]))
def test_accessors(case):
    obj, types = world()
    a, b = (obj[x] for x in case["pair"])
    r = SRelation({"first": a, "second": b, "type": types["succ"]})
    ends = r.get_ends()
    assert len(ends) == case["nends"]
    assert (1 if ends[0] is a and ends[1] is b else 0) == case["ends_ok"]
    assert describe(r) == case["obj"]


WRITERS = cases("writer")


@pytest.mark.parametrize("case", WRITERS, ids=[f"{c['writer'][0]}-{i}" for i, c in enumerate(WRITERS)])
def test_writer(case):
    obj, types = world()
    r = SRelation({"first": obj["e0"], "second": obj["e1"], "type": types["succ"]})
    m, v = case["writer"]
    e = None
    try:
        getattr(r, m)(value(v, obj, types))
    except Exception as ex:  # noqa: BLE001
        e = ex
    assert err(e) == golden_err(case["err"])
    assert describe(r) == case["obj"]


def test_writer_unchanged_bindings():
    obj, types = world()
    r = SRelationStructural({"first": obj["e0"], "second": obj["e1"], "type": types["struct"]})
    for case in cases("writer_unchanged"):
        e = None
        try:
            r.set_unchanged_bindings(case["value"])
        except Exception as ex:  # noqa: BLE001
            e = ex
        assert err(e) == golden_err(case["err"])
        assert sorted(r.get_unchanged_bindings()) == case["unchanged"]
        assert r.no_unchanged_bindings() == case["no_unchanged"]


def test_history_delegations():
    obj, types = world()
    c = one("history")
    r = SRelation({"first": obj["e0"], "second": obj["e1"], "type": types["succ"]})
    r.add_history("first note")
    Global.Steps_Finished = 7
    r.add_history("second note")
    assert r.get_history() == c["history"]
    assert r.search_history(re.compile("note")) == c["search"]
    assert r.get_age() == c["age"]
    assert [r.unchanged_since(x) for x in (0, 3, 7, 9)] == c["unchanged"]
    assert r.history_as_text() == c["as_text"]
    assert r.history_object().__class__ is SHistory and c["hobj_ref"] == "SHistory"
    with pytest.raises(Confess) as ei:
        r.set_history([])
    assert one("set_history")["err"]["message"].startswith(str(ei.value))


# --- UpdateStrength, as_text, SuggestCategory ----------------------------------------------

@pytest.mark.parametrize("case", cases("update_strength"),
                         ids=lambda c: f"{c['act']}-{'-'.join(c['pair'])}")
def test_update_strength(case):
    obj, types = world()
    ACT["succ"] = case["act"]
    a, b = (obj[x] for x in case["pair"])
    r = SRelation({"first": a, "second": b, "type": types["succ"]})
    ret = r.update_strength()
    assert ret == pytest.approx(case["ret"])
    assert r.get_strength() == pytest.approx(case["strength"])
    assert take_log() == case["log"]


@pytest.mark.parametrize("case", cases("as_text"), ids=lambda c: c["type"])
def test_as_text(case):
    obj, types = world()
    r = SRelation({"first": obj["e0"], "second": obj["g23"], "type": types[case["type"]]})
    assert r.as_text() == case["as_text"]


@pytest.mark.parametrize("case", cases("suggest"), ids=lambda c: c["type"])
def test_suggest_category(case):
    obj, types = world()
    t = types[case["type"]]
    r = SRelation({"first": obj["e0"], "second": obj["e1"], "type": t})
    sc = r.suggest_category()
    if "ref" in case:
        assert sc.perl_name == case["ref"]
        assert sc.get_name() == case["name"]
        named = {"SAMENESS": S.SAMENESS, "ASCENDING": S.ASCENDING, "DESCENDING": S.DESCENDING}
        if case["same_as"] == "MB":
            assert sc is MappingBased.create(t)
        else:
            assert sc is named[case["same_as"]]
    else:
        assert sc == case["value"]
    ends = one("suggest_ends", type=case["type"])
    assert r.suggest_category_for_ends() is None and ends["scalar"] == 0


# --- FlippedVersion ------------------------------------------------------------------------

# Mapping::Structural's FlippedVersion calls main::message, which the oracle doesn't
# define ("Undefined subroutine &main::message"); Python logs it instead.
FLIPS = [c for c in cases("flip") if c["case"][2] != "struct"]


@pytest.mark.parametrize("case", FLIPS, ids=lambda c: "-".join(c["case"]))
def test_flipped_version(case):
    obj, types = world()
    a, b, t = case["case"]
    r = SRelation({"first": obj[a], "second": obj[b], "type": types[t]})
    e = f = None
    try:
        f = r.flipped_version()
    except Exception as ex:  # noqa: BLE001
        e = ex
    assert err(e) == golden_err(case["err"])
    assert (0 if f is None else 1) == case["defined"]
    if f is not None:
        assert describe(f) == case["obj"]
        assert f.perl_name == case["ref"]
        assert (1 if f.get_first() is obj[b] and f.get_second() is obj[a] else 0) == case["ends_swapped"]


def test_flip_struct_oracle_died_on_message():
    c = one("flip", case=["g01", "g23", "struct"])
    assert c["err"]["message"] == "Undefined subroutine &main::message called"


# --- insert / uninsert ---------------------------------------------------------------------

def ends_state(*objs):
    return [{"rels": len(x.all_relations()),
             "hist": [h for h in x.get_history() if "reln" in h]} for x in objs]


def test_insert_uninsert_sequence():
    ACT["succ"], ACT["pred"] = 2, 1
    types = world()[1]
    a, b, c = E(1, 10), E(2, 11), E(3, 13)

    r1 = SRelation({"first": a, "second": b, "type": types["succ"]})
    take_log()
    ret = r1.insert()
    g = one("insert", step="first")
    assert ret == g["ret"] and r1.get_strength() == g["strength"]
    assert take_log() == g["log"]
    assert (1 if a.get_relation(b) is r1 and b.get_relation(a) is r1 else 0) == g["exists"]
    assert ends_state(a, b) == g["ends"]

    r2 = SRelation({"first": a, "second": b, "type": types["pred"]})
    take_log()
    r2.insert()
    g = one("insert", step="replace")
    assert take_log() == g["log"] and r2.get_strength() == g["strength"]
    assert (1 if a.get_relation(b) is r2 and b.get_relation(a) is r2 else 0) == g["exists"]
    assert ends_state(a, b) == g["ends"]

    r3 = SRelation({"first": b, "second": a, "type": types["succ"]})
    take_log()
    r3.insert()
    g = one("insert", step="reverse")
    assert take_log() == g["log"] and r3.get_strength() == g["strength"]
    assert (1 if a.get_relation(b) is r3 else 0) == g["exists"]
    assert ends_state(a, b) == g["ends"]

    World.add = None
    r4 = SRelation({"first": b, "second": c, "type": types["succ"]})
    take_log()
    ret4 = r4.insert()
    g = one("insert", step="refused")
    assert ret4 == pytest.approx(g["ret"]) and r4.get_strength() == pytest.approx(g["strength"])
    assert r4.get_holeyness() == g["holey"]
    assert take_log() == g["log"]
    assert (1 if b.get_relation(c) else 0) == g["exists"]
    assert ends_state(b, c) == g["ends"]

    World.add = 0
    r4 = SRelation({"first": b, "second": c, "type": types["succ"]})
    take_log()
    r4.insert()
    g = one("insert", step="refused0")
    assert take_log() == g["log"] and (1 if b.get_relation(c) else 0) == g["exists"]
    World.add = 1

    World.add_dies = Confess("boom\n")
    r5 = SRelation({"first": b, "second": c, "type": types["succ"]})
    take_log()
    with pytest.raises(Confess) as ei:
        r5.insert()
    g = one("insert", step="dies")
    assert err(ei.value) == golden_err(g["err"])
    assert take_log() == g["log"] and (1 if b.get_relation(c) else 0) == g["exists"]

    World.add_dies = SErr("ws err")
    r5 = SRelation({"first": b, "second": c, "type": types["succ"]})
    take_log()
    with pytest.raises(Confess) as ei:
        r5.insert()
    g = one("insert", step="dies_obj")
    assert err(ei.value) == golden_err(g["err"]) and take_log() == g["log"]
    World.add_dies = None

    u = r3.uninsert()
    g = [x for x in cases("uninsert") if "step" not in x][0]
    assert u == g["ret"] and take_log() == g["log"]
    assert (1 if a.get_relation(b) else 0) == g["exists"]
    assert ends_state(a, b) == g["ends"]

    r3.uninsert()
    g = one("uninsert", step="again")
    assert g["err"] is None and take_log() == g["log"] and ends_state(a, b) == g["ends"]

    r6 = SRelation({"first": a, "second": b, "type": types["succ"]})
    r6.insert()
    take_log()
    r7 = SRelation({"first": a, "second": c, "type": types["succ"]})
    r7.insert()
    g = one("insert", step="second_pair")
    assert take_log() == g["log"] and len(a.all_relations()) == g["rels_a"]
    assert ends_state(a, b, c) == g["ends"]

    a.remove_all_relations()
    g = one("remove_all")
    assert take_log() == g["log"] and ends_state(a, b, c) == g["ends"]


def test_insert_hooks_reach_the_workspace(monkeypatch):
    monkeypatch.undo()
    from seqsee import slink_activation, sworkspace
    a, b = E(1, 0), E(2, 1)
    r = SRelation({"first": a, "second": b, "type": MappingNumeric.create("succ", S.NUMBER)})
    # Item 029: SLTM::GetRealActivationsForOneConcept is real (a fresh node: 20 × PRECALCULATED[2]).
    assert r.insert() == pytest.approx(20 * slink_activation.PRECALCULATED[2])
    # Item 032: SWorkspace->AddRelation/RemoveRelation are real.
    assert list(sworkspace.relations) == [r] and a.get_relation(b) is r
    r.uninsert()
    assert not sworkspace.relations and not sworkspace.relations_by_ends


# --- Object.pm integration -----------------------------------------------------------------

def test_chain_and_recalculate():
    ACT["succ"], ACT["same"] = 3, 1
    e = [E(4, 20), E(5, 21), E(6, 22)]
    g = G(*e)
    take_log()
    g.apply_reln_scheme(RELN_SCHEME.CHAIN)
    want = one("chain")
    assert take_log() == want["log"]
    pairs = []
    for i in (0, 1):
        r = e[i].get_relation(e[i + 1])
        pairs.append({"as_text": r.as_text(), "strength": r.get_strength(), "ref": r.perl_name})
    assert pairs == want["pairs"]

    e[1].mag(4)
    e[0].recalculate_relations()
    want = one("recalc")
    assert take_log() == want["log"]
    r = e[0].get_relation(e[1])
    assert (r.as_text() if r else None) == want["rel"]
    r2 = e[1].get_relation(e[2])
    assert (r2.as_text() if r2 else None) == want["rel2"]


# --- are_there_holes_here (SWorkspace, used by BUILD) --------------------------------------

def test_are_there_holes_here():
    holes = srelation_mod._are_there_holes_here
    a, b, c = E(1, 0), E(2, 1), E(3, 3)
    assert holes() == 0
    assert holes(a, b) == 0
    assert holes(a, c) == 1
    assert holes(b, a) == 0
    assert holes(a, G(b, E(9, 2)), c) == 0
    with pytest.raises(SErr):
        holes(a, 5)

"""Tests for the mapping base and the multimethod dispatch.

Mirrors lib/Mapping.pm, lib/Mapping/Dir.pm and lib/Mapping/Position.pm, plus the
Class::Multimethods 1.701 dispatch they use (seqsee/multimethods.py). Golden data:
oracle/mapping_base.pl.

As in the oracle, SLTM::SpikeAndChoose, Mapping::Numeric->create (plain `new` in the
oracle), Seqsee::Element->create and main::message are replaced by recorders, and the
objects are fakes whose classes inherit from the real ones.
"""
import pytest

import golden
from seqsee import multimethods, s as S, util
from seqsee.categories import base, numeric
from seqsee.constants import DIR
from seqsee.errors import Confess
from seqsee import mapping
from seqsee.mapping import Mapping, apply_mapping, find_mapping
from seqsee.mapping import dir as mapping_dir
from seqsee.mapping import position
from seqsee.mapping.dir import MappingDir
from seqsee.mapping.position import MappingPosition
from seqsee.objects.anchored import Anchored
from seqsee.objects.element import Element
from seqsee.objects.object import SeqseeObject
from seqsee.sint import SInt
from seqsee.spos import SPos
from seqsee.util import perl_str

CASES = golden.load("mapping_base")


def cases(kind):
    return [c for c in CASES if c["kind"] == kind]


def case(kind, label):
    (c,) = [c for c in CASES if c["kind"] == kind and c.get("label") == label]
    return c


# --- fakes (same names as in the oracle) ---------------------------------------------

LOG = []
SPIKE = []
MESSAGES = []


class FakeCat:
    def __init__(self, name, apply=None, sufficient=None):
        self.name = name
        self.apply = apply
        self.sufficient = sufficient

    def as_text(self):
        return self.name

    def is_numeric(self):
        return 0

    def find_mapping_for_cat(self, a, b):
        LOG.append(f"find:{util.perl_ref(a)},{util.perl_ref(b)}")
        return f"found-by-{self.name}"

    def apply_mapping_for_cat(self, t, o):
        LOG.append(f"apply:{util.perl_ref(t)},{util.perl_ref(o)}")
        return self.apply

    def are_attributes_sufficient_to_build(self, *atts):
        LOG.append("suff:" + ",".join(sorted(atts)))
        return self.sufficient


class FakeElem(Element):
    perl_name = "FakeElem"

    def __init__(self, mag=None, *cats):
        self.fmag = mag
        self.fcats = list(cats)
        self.created = None

    def get_mag(self):
        return self.fmag

    def get_common_categories(self, other):
        return [c for c in self.fcats if any(c is o for o in other.fcats)]


class FakeAnch(Anchored):
    perl_name = "FakeAnch"

    def __init__(self, *cats):
        self.fcats = list(cats)

    get_common_categories = FakeElem.get_common_categories


class FakeObj(SeqseeObject):
    perl_name = "FakeObj"

    def __init__(self):
        pass


class MappingNumericStub(Mapping):
    """Stands in for Mapping::Numeric (item 017)."""

    perl_name = "Mapping::Numeric"

    def __init__(self, name, category):
        self.name = name
        self.category = category

    def get_name(self):
        return self.name

    def get_category(self):
        return self.category


class StructuralStub(Mapping):
    """Stands in for Mapping::Structural (item 019)."""

    perl_name = "Mapping::Structural"


class FakeStruct(StructuralStub):
    perl_name = "FakeStruct"

    def __init__(self, cat=None, changed=None):
        self.cat = cat
        self.changed = changed

    def get_category(self):
        return self.cat

    def get_changed_bindings(self):
        return self.changed


PICK = [lambda *c: c[0]]


@pytest.fixture(autouse=True)
def recorders(monkeypatch):
    LOG.clear()
    SPIKE.clear()
    MESSAGES.clear()
    PICK[0] = lambda *c: c[0]

    def spike(amount, *concepts):
        SPIKE.append([amount] + [c.as_text() for c in concepts])
        return PICK[0](*concepts)

    def element_create(mag, pos):
        e = FakeElem(mag)
        e.created = [mag, pos]
        return e

    monkeypatch.setattr(mapping, "_spike_and_choose", spike)
    monkeypatch.setattr(mapping, "_message", MESSAGES.append)
    monkeypatch.setattr(numeric, "_mapping_numeric_create", MappingNumericStub)
    monkeypatch.setattr(numeric, "_element_create", element_create)


def describe(x):
    if x is None:
        return None
    if util._is_scalar(x):
        return perl_str(x)
    if isinstance(x, MappingNumericStub):
        return f"Mapping::Numeric:{x.get_name()}/{x.get_category().as_text()}"
    if isinstance(x, MappingDir):
        return f"Mapping::Dir:{x.string}"
    if isinstance(x, MappingPosition):
        return f"Mapping::Position:{perl_str(x.get_text())}"
    if isinstance(x, SInt):
        return f"SInt:{perl_str(x.get_mag())}"
    if isinstance(x, SPos):
        return f"SPos:{x.position}"
    if isinstance(x, DIR):
        return f"DIR:{x.as_text()}"
    if isinstance(x, FakeElem):
        return "FakeElem:" + ("undef" if x.fmag is None else perl_str(x.fmag))
    return util.perl_ref(x)


def run(code):
    LOG.clear()
    SPIKE.clear()
    MESSAGES.clear()
    try:
        result, error = code(), None
    except Confess as e:
        result, error = None, str(e)
    except (AttributeError, TypeError) as e:   # Perl: "Can't call method ... on undef"
        result, error = None, e
    return {"result": describe(result), "error": error, "spike": list(SPIKE),
            "log": list(LOG), "messages": list(MESSAGES)}


def check(c, got, exact_error=True):
    if c["error"] is None:
        assert got["error"] is None, got["error"]
        assert got["result"] == c["result"]
    else:
        assert got["error"] is not None
        if isinstance(got["error"], str):
            if exact_error:
                assert got["error"] == c["error"]
            else:
                # Moose dumps a rejected ref its own way ("[  ]"): compare up to it.
                assert c["error"].startswith(got["error"].split(" with value [")[0])
    for k in ("spike", "log", "messages"):
        assert got[k] == c[k], k


# --- the dispatch engine (multimethod Probe) ------------------------------------------

class PA:
    perl_name = "PA"


class PB(PA):
    perl_name = "PB"


class PC(PB):
    perl_name = "PC"


class PX:
    perl_name = "PX"


class PD(PA, PX):
    perl_name = "PD"


class PY:
    perl_name = "PY"


PROBE = multimethods.Multimethod("Probe")
for _sig in [("PA", "PA"), ("PB", "PA"), ("PA", "PB"), ("#", "#"), ("$", "$"),
             ("ARRAY", "*"), ("*", "PX"), ("*", "PY"), ("PY", "*"), ("HASH", "HASH")]:
    PROBE.variant(*_sig)(lambda *a, _l=",".join(_sig): _l)

MAKE = {"PA": PA, "PB": PB, "PC": PC, "PD": PD, "PX": PX, "PY": PY,
        "ARRAY": list, "HASH": dict, "num": lambda: 3, "float": lambda: 2.5,
        "neg": lambda: -1, "str": lambda: "abc", "numstr": lambda: "3",
        "empty": lambda: "", "undef": lambda: None}


@pytest.mark.parametrize("c", cases("probe"), ids=lambda c: ",".join(c["args"]))
def test_probe_dispatch(c):
    args = [MAKE[a]() for a in c["args"]]
    if c["error"] is None:
        assert PROBE(*args) == c["result"]
    else:
        with pytest.raises(Confess) as e:
            PROBE(*args)
        assert str(e.value).split("\n")[0] == c["error"]


def test_arg_types():
    assert multimethods.arg_type(3) == "#"
    assert multimethods.arg_type(2.5) == "#"
    assert multimethods.arg_type(True) == "#"
    assert multimethods.arg_type("3") == "$"
    assert multimethods.arg_type(None) == "$"
    assert multimethods.arg_type([]) == "ARRAY"
    assert multimethods.arg_type({}) == "HASH"
    assert multimethods.arg_type(SInt(1)) == "SInt"
    assert multimethods.arg_type(DIR.LEFT) == "DIR"


def test_perl_isa():
    assert multimethods.perl_isa(FakeElem(1), "Seqsee::Object")
    assert multimethods.perl_isa(FakeStruct(), "Mapping")
    assert not multimethods.perl_isa(mapping_dir.SAME, "Mapping")
    assert not multimethods.perl_isa(3, "Mapping")


def test_ambiguous_lists_candidates():
    with pytest.raises(Confess) as e:
        PROBE(PB(), PB())
    lines = str(e.value).split("\n")
    assert lines[-1] == "are equally viable"
    assert sorted(lines[1:-1]) == ["\tProbe(PA,PB)", "\tProbe(PB,PA)"]


# --- FindMapping ----------------------------------------------------------------------

@pytest.mark.parametrize("c", cases("find_num"), ids=lambda c: f"{c['a']!r},{c['b']!r}")
def test_find_num(c):
    check(c, run(lambda: find_mapping(c["a"], c["b"])))


FC = FakeCat("fc")


def test_find_cat():
    check(case("find_cat", "number"), run(lambda: find_mapping(3, 4, S.NUMBER)))
    check(case("find_cat", "number_strings"), run(lambda: find_mapping("5", "6", S.NUMBER)))
    check(case("find_cat", "fake_sints"), run(lambda: find_mapping(SInt(1), SInt(2), FC)))
    check(case("find_cat", "fake_undef"), run(lambda: find_mapping(None, None, FC)))
    check(case("find_cat", "undef_cat"), run(lambda: find_mapping(1, 2, None)))


def _sint(mag, *cats):
    x = SInt(mag)
    for c in cats:
        x.add_category(c)
    return x


def _pick_fake(*c):
    return next((x for x in c if isinstance(x, FakeCat)), None)


def test_find_sint():
    check(case("find_sint", "succ"), run(lambda: find_mapping(SInt(3), SInt(4))))
    check(case("find_sint", "none"), run(lambda: find_mapping(SInt(3), SInt(9))))
    PICK[0] = lambda *c: None
    check(case("find_sint", "pick_undef"), run(lambda: find_mapping(SInt(5), SInt(4))))
    PICK[0] = _pick_fake
    check(case("find_sint", "fake_cat"), run(lambda: find_mapping(_sint(5, FC), _sint(4, FC))))

    def no_common():
        a = SInt(1)
        a._categories = []
        return find_mapping(a, SInt(2))
    check(case("find_sint", "no_common"), run(no_common))

    def undef_cat():
        a, b = SInt(1), SInt(2)
        a._categories.append(None)
        b._categories.append(None)
        return find_mapping(a, b)
    c = case("find_sint", "undef_cat")
    got = run(undef_cat)
    prefix = "undef in common_categories FindMapping SInt/Seqsee::Element SInt/Seqsee::Element:"
    assert c["error"].startswith(prefix) and got["error"].startswith(prefix)
    assert got["error"].endswith(", ")   # the undef joins as ""


def test_find_elem():
    E = FakeElem
    check(case("find_elem", "succ"), run(lambda: find_mapping(E(3, S.NUMBER), E(4, S.NUMBER))))
    check(case("find_elem", "same_two_cats"),
          run(lambda: find_mapping(E(7, S.NUMBER, FC), E(7, FC, S.NUMBER))))
    PICK[0] = _pick_fake
    check(case("find_elem", "fake_cat"),
          run(lambda: find_mapping(E(7, S.NUMBER, FC), E(9, FC, S.NUMBER))))
    check(case("find_elem", "no_common"), run(lambda: find_mapping(E(3, S.NUMBER), E(4, FC))))
    PICK[0] = lambda *c: None
    check(case("find_elem", "pick_undef"), run(lambda: find_mapping(E(3, FC), E(2, FC))))


def test_find_anch():
    A, E = FakeAnch, FakeElem
    check(case("find_anch", "fake_cat"), run(lambda: find_mapping(A(FC), A(FC))))
    check(case("find_anch", "no_common"), run(lambda: find_mapping(A(FC), A())))
    check(case("find_anch", "elem_anch"), run(lambda: find_mapping(E(1, FC), A(FC))))
    check(case("find_anch", "anch_elem"), run(lambda: find_mapping(A(FC), E(1, FC))))
    PICK[0] = lambda *c: None
    check(case("find_anch", "pick_undef"), run(lambda: find_mapping(A(FC), A(FC))))


def test_find_mixed():
    A, E = FakeAnch, FakeElem
    calls = {
        "sint_elem": lambda: find_mapping(SInt(1), E(1, FC)),
        "elem_sint": lambda: find_mapping(E(1, FC), SInt(1)),
        "anch_sint": lambda: find_mapping(A(FC), SInt(1)),
        "sint_anch": lambda: find_mapping(SInt(1), A(FC)),
        "obj_obj": lambda: find_mapping(FakeObj(), FakeObj()),
        "sint_num": lambda: find_mapping(SInt(1), 2),
        "num_sint": lambda: find_mapping(1, SInt(2)),
        "elem_num": lambda: find_mapping(E(1, FC), 2),
        "one_arg": lambda: find_mapping(1),
        "dir_spos": lambda: find_mapping(DIR.LEFT, SPos(1)),
    }
    assert {c["label"] for c in cases("find_mixed")} == set(calls)
    for label, call in calls.items():
        check(case("find_mixed", label), run(call))


DIRS = {"left": DIR.LEFT, "right": DIR.RIGHT, "unknown": DIR.UNKNOWN, "neither": DIR.NEITHER}


@pytest.mark.parametrize("c", cases("find_dir"), ids=lambda c: f"{c['a']},{c['b']}")
def test_find_dir(c):
    r = find_mapping(DIRS[c["a"]], DIRS[c["b"]])
    assert describe(r) == c["result"]
    assert (r is MappingDir.create(r.string)) == bool(c["is_memo"])


@pytest.mark.parametrize("c", cases("find_pos"), ids=lambda c: f"{c['a']},{c['b']}")
def test_find_pos(c):
    check(c, run(lambda: find_mapping(SPos(c["a"]), SPos(c["b"]))))


# --- ApplyMapping ---------------------------------------------------------------------

CATS = {"number": S.NUMBER, "prime": S.PRIME, "odd": S.ODD, "even": S.EVEN}


@pytest.mark.parametrize("c", cases("apply_num"), ids=lambda c: f"{c['cat']}-{c['name']}-{c['num']}")
def test_apply_num(c):
    t = MappingNumericStub(c["name"], CATS[c["cat"]])
    check(c, run(lambda: apply_mapping(t, c["num"])))


@pytest.mark.parametrize("c", cases("apply_sint"), ids=lambda c: f"{c['cat']}-{c['name']}-{c['num']}")
def test_apply_sint(c):
    t = MappingNumericStub(c["name"], CATS[c["cat"]])
    check(c, run(lambda: apply_mapping(t, SInt(c["num"]))))


@pytest.mark.parametrize("c", cases("apply_elem"), ids=lambda c: f"{c['cat']}-{c['name']}-{c['num']}")
def test_apply_elem(c):
    t = MappingNumericStub(c["name"], CATS[c["cat"]])
    holder = []

    def call():
        r = apply_mapping(t, FakeElem(c["num"]))
        holder.append(r)
        return r
    check(c, run(call))
    r = holder[0]
    if c["created"] is None:
        assert r is None
    else:
        _pkg, mag, pos = c["created"]
        assert [perl_str(r.created[0]), r.created[1]] == [mag, pos]


def test_apply_misc():
    t = MappingNumericStub("succ", S.NUMBER)
    st = FakeStruct(cat=FakeCat("fc", apply="applied"))
    calls = {
        "numeric_str": lambda: apply_mapping(t, "3"),
        "numeric_anch": lambda: apply_mapping(t, FakeAnch()),
        "numeric_obj": lambda: apply_mapping(t, FakeObj()),
        "numeric_spos": lambda: apply_mapping(t, SPos(1)),
        "struct_obj": lambda: apply_mapping(st, FakeObj()),
        "struct_elem": lambda: apply_mapping(st, FakeElem(1)),
        "struct_anch": lambda: apply_mapping(st, FakeAnch()),
        "struct_sint": lambda: apply_mapping(st, SInt(1)),
        "struct_num": lambda: apply_mapping(st, 1),
        "struct_undef_result": lambda: apply_mapping(FakeStruct(cat=FakeCat("u")), FakeObj()),
        "dir_spos": lambda: apply_mapping(mapping_dir.SAME, SPos(1)),
        "pos_dir": lambda: apply_mapping(MappingPosition.create("same"), DIR.LEFT),
        "undef_undef": lambda: apply_mapping(None, None),
    }
    assert {c["label"] for c in cases("apply_misc")} == set(calls)
    for label, call in calls.items():
        check(case("apply_misc", label), run(call))


DIR_MAPS = {"Same": lambda: mapping_dir.SAME, "Different": lambda: mapping_dir.DIFFERENT,
            "Unknown": lambda: mapping_dir.UNKNOWN, "NewSame": lambda: MappingDir("Same")}


@pytest.mark.parametrize("c", cases("apply_dir"), ids=lambda c: f"{c['map']}-{c['dir']}")
def test_apply_dir(c):
    m, d = DIR_MAPS[c["map"]](), DIRS[c["dir"]]
    got = run(lambda: apply_mapping(m, d))
    check(c, got, exact_error=True)
    if c["error"] is None:
        assert (apply_mapping(m, d) is d) == bool(c["is_input"])


POS_MAPS = {"succ": lambda: MappingPosition.create("succ"),
            "pred": lambda: MappingPosition.create("pred"),
            "same": lambda: MappingPosition.create("same"),
            "foo": lambda: MappingPosition.create("foo"),
            "new_succ": lambda: MappingPosition({"text": "succ"})}


@pytest.mark.parametrize("c", cases("apply_pos"), ids=lambda c: f"{c['map']}-{c['pos']}")
def test_apply_pos(c):
    m, p = POS_MAPS[c["map"]](), SPos(c["pos"])
    check(c, run(lambda: apply_mapping(m, p)))
    if c["error"] is None:
        assert (apply_mapping(m, p) is p) == bool(c["is_input"])


# --- Mapping::Dir ---------------------------------------------------------------------

def test_dir_basics():
    c = cases("dir_basics")[0]
    S_, D, U = mapping_dir.SAME, mapping_dir.DIFFERENT, mapping_dir.UNKNOWN
    assert [util.perl_ref(x) for x in (S_, D, U)] == c["refs"]
    assert [x.string for x in (S_, D, U)] == c["strings"]
    assert [x.serialize() for x in (S_, D, U)] == c["serialize"]
    assert (MappingDir.create("Same") is S_) == bool(c["create_same_is_memo"])
    assert (MappingDir("Same") is S_) == bool(c["new_same_is_memo"])
    assert (MappingDir.deserialize("Different") is D) == bool(c["deserialize_is_memo"])
    assert [x.is_effectively_a_sameness_relation()
            for x in (S_, D, U, MappingDir("Same"))] == c["sameness"]
    assert [int(x.flipped_version() is x) for x in (S_, D, U)] == c["flipped_is_self"]
    assert [int(x.get_pure() is x) for x in (S_, D, U)] == c["pure_is_self"]
    assert [len(x.get_memory_dependencies()) for x in (S_, D, U)] == c["deps"]
    assert int(isinstance(S_, Mapping)) == c["isa_mapping"]
    assert int(hasattr(S_, "check_sanity")) == c["can_check_sanity"]
    assert int(hasattr(S_, "as_text")) == c["can_as_text"]
    assert int(hasattr(S_, "get_name")) == c["can_get_name"]


def test_dir_other(monkeypatch):
    monkeypatch.setattr(mapping_dir, "_MEMO", dict(mapping_dir._MEMO))
    c = cases("dir_other")[0]
    other = MappingDir.create("Other")
    assert other.string == c["string"]
    assert (MappingDir.create("Other") is other) == bool(c["again_is_memo"])
    assert (MappingDir.deserialize("Other") is other) == bool(c["deserialize_is_memo"])
    assert other.is_effectively_a_sameness_relation() == c["sameness"]


def test_base_dir_same_hook():
    assert base._mapping_dir_same() is mapping_dir.SAME


# --- Mapping::Position ----------------------------------------------------------------

@pytest.mark.parametrize("c", cases("pos_basics"), ids=lambda c: c["text"])
def test_pos_basics(c):
    t = c["text"]
    p = MappingPosition.create(t)
    f = p.flipped_version()
    assert util.perl_ref(p) == c["ref"]
    assert p.get_text() == c["get_text"]
    assert p.as_text() == c["as_text"]
    assert p.serialize() == c["serialize"]
    assert (MappingPosition.deserialize(t) is p) == bool(c["deserialize_is_memo"])
    assert (MappingPosition.create(t) is p) == bool(c["create_is_memo"])
    assert (MappingPosition({"text": t}) is p) == bool(c["new_is_memo"])
    assert p.is_effectively_a_sameness_relation() == c["sameness"]
    assert MappingPosition(text=t).is_effectively_a_sameness_relation() == c["new_sameness"]
    assert f.get_text() == c["flipped"]
    assert (f is MappingPosition.create(f.get_text())) == bool(c["flipped_is_memo"])
    assert (p.get_pure() is p) == bool(c["pure_is_self"])
    assert len(p.get_memory_dependencies()) == c["deps"]
    assert int(isinstance(p, Mapping)) == c["isa_mapping"]
    assert int(hasattr(p, "check_sanity")) == c["can_check_sanity"]


def test_pos_edges(monkeypatch):
    # Replay in oracle order against the load-time memo (whether "" exists matters).
    monkeypatch.setattr(position, "_MEMO", {k: v for k, v in position._MEMO.items()
                                            if k in ("succ", "pred", "same")})
    foo = MappingPosition.create("foo")
    steps = [
        ("flip_foo_before_empty", lambda: foo.flipped_version()),
        ("create_undef", lambda: MappingPosition.create(None)),
        ("create_num", lambda: MappingPosition.create(5).get_text()),
        ("create_num_str_same",
         lambda: int(MappingPosition.create("5") is MappingPosition.create(5))),
        ("new_array", lambda: MappingPosition({"text": []})),
        ("new_missing", lambda: MappingPosition({})),
    ]
    for label, call in steps:
        check(case("pos_edge", label), run(call), exact_error=False)
    empty = MappingPosition.create("")
    check(case("pos_edge", "create_empty"), run(lambda: empty.get_text()))
    check(case("pos_edge", "flip_foo_after_empty"),
          run(lambda: "is_empty" if foo.flipped_version() is empty else "other"))
    check(case("pos_edge", "create_undef_after_empty"),
          run(lambda: "is_empty" if MappingPosition.create(None) is empty else "other"))


# --- CheckSanity ----------------------------------------------------------------------

def test_check_sanity():
    check(case("sanity", "numeric"),
          run(lambda: MappingNumericStub("succ", S.NUMBER).check_sanity()))
    for label, changed, suff in [("ok_two", {"a": 1, "b": 2}, 1), ("bad_one", {"a": 1}, 0),
                                 ("bad_none", {}, ""), ("ok_none", {}, 1),
                                 ("bad_undef", {"x": 1}, None)]:
        st = FakeStruct(cat=FakeCat("sc", sufficient=suff), changed=changed)
        check(case("sanity", label), run(st.check_sanity))
    check(case("sanity", "plain_mapping"), run(lambda: Mapping().check_sanity()))

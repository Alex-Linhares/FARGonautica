"""Tests for the memory plumbing (item 028): Perl lib/SLTM/Platonic.pm,
lib/Memory/Insertible.pm, lib/Memory/Storable.pm, lib/Memory/Node.pm, lib/Memory/LTM.pm,
lib/LTMStorable.pm (and a check of lib/LTMStorable/Independent.pm).

Golden data comes from oracle/memory_plumbing.pl (tests/golden/memory_plumbing.json).
The oracle replaces SLTM::SpikeBy/InsertISALink with recorders; here the same recorders
go in through the hooks in ``ltmstorable``. Memory::LTM doesn't compile in Perl (it
calls ``confess`` without importing Carp); the oracle imports Carp into its package
first, and the Python port is of the module as it would then behave. The oracle's
T::Store/T::Ins/T::Plain test classes are mirrored by TStore/TIns/TPlain below.
"""
import pytest

import golden
from seqsee import ltmstorable
from seqsee import s as S
from seqsee import sltm_platonic, util
from seqsee.categories.ascending import Ascending
from seqsee.errors import Confess
from seqsee.ltmstorable import LTMStorable
from seqsee.memory import ltm
from seqsee.memory.insertible import Insertible
from seqsee.memory.node import Node
from seqsee.memory.storable import Storable
from seqsee.objects.element import Element
from seqsee.objects.object import SeqseeObject
from seqsee.sint import SInt
from seqsee.sltm_platonic import SLTMPlatonic, structure_from_string

CASES = golden.load("memory_plumbing")


def cases(op, **match):
    found = [c for c in CASES if c["op"] == op and all(c.get(k) == v for k, v in match.items())]
    assert found, (op, match)
    return found


def one(op, **match):
    found = cases(op, **match)
    assert len(found) == 1, (op, match)
    return found[0]


def as_perl(x):
    """Perl's view of a structure: scalars as their strings, lists recursively."""
    if isinstance(x, list):
        return [as_perl(v) for v in x]
    return None if x is None else util.perl_str(x)


# ---- SLTM::Platonic::structure_from_string ----

@pytest.mark.parametrize("case", cases("structure_from_string"), ids=lambda c: repr(c["string"]))
def test_structure_from_string(case):
    if case["error"] is not None:
        with pytest.raises(Confess) as info:
            structure_from_string(case["string"])
        assert str(info.value) == case["error"]
    else:
        assert as_perl(structure_from_string(case["string"])) == case["result"]


def test_structure_numbers_are_numeric():
    # Port decision: tokens whose Perl string form round-trips become numbers.
    assert structure_from_string("[1, -2, 1.5, [0]]") == [1, -2, 1.5, [0]]
    assert structure_from_string("5") == 5
    assert structure_from_string("[01,1e3,a2]") == ["01", "1e3", "a2"]
    assert structure_from_string("12abc") == "12abc"
    assert structure_from_string(7) == 7


# ---- SLTM::Platonic objects ----

@pytest.mark.parametrize("case", cases("create"), ids=lambda c: c["string"])
def test_create(case):
    p = SLTMPlatonic.create(case["string"])
    assert p.as_text() == case["as_text"]
    assert p.serialize() == case["serialize"]
    assert as_perl(p.get_structure()) == case["structure"]
    assert p.get_structure_string() == case["structure_string"]
    assert (p is SLTMPlatonic.create(case["string"])) == bool(case["memoized"])
    assert (p is SLTMPlatonic.deserialize(p.serialize())) == bool(case["deserialize_same"])
    assert (p is p.get_pure()) == bool(case["pure_same"])
    assert p.get_memory_dependencies() == case["deps"]
    assert util.perl_ref(p) == case["ref"]


def test_create_number():
    case = one("create_number")
    p = SLTMPlatonic.create(7)
    assert p.as_text() == case["as_text"]
    assert as_perl(p.get_structure()) == case["structure"]
    assert p.get_structure_string() == case["structure_string"] == 7
    assert (p is SLTMPlatonic.create("7")) == bool(case["same_as_string"])


def test_whitespace_keys_differ():
    case = one("whitespace_keys_differ")
    assert (SLTMPlatonic.create("[1, 2]") is SLTMPlatonic.create("[1,2]")) == bool(case["same"])


@pytest.mark.parametrize("case", cases("create_dies"), ids=lambda c: repr(c["string"]))
def test_create_dies(case):
    with pytest.raises(Confess) as info:
        SLTMPlatonic.create(case["string"])
    assert str(info.value) == case["error"]
    assert case["string"] not in sltm_platonic._MEMO


def test_setters():
    case = one("set_structure_string")
    q = SLTMPlatonic.create("[1, 2]")
    assert q.set_structure_string("zz") == case["old"]
    assert q.as_text() == case["as_text"]
    assert q.serialize() == case["serialize"]
    assert (q is SLTMPlatonic.create("[1, 2]")) == bool(case["still_memoized"])
    with pytest.raises(Confess):
        SLTMPlatonic.create("zz")
    assert case["zz_dies"] == 1
    case = one("set_structure")
    assert as_perl(q.set_structure([9])) == case["old"]
    assert q.get_structure() == case["structure"]
    with pytest.raises(Confess, match="Missing new value in call to 'set_structure' method"):
        q.set_structure()


@pytest.mark.parametrize("case", cases("new"), ids=lambda c: repr(c["args"]))
def test_new(case):
    if case["error"] is not None:
        with pytest.raises(Confess) as info:
            SLTMPlatonic(case["args"])
        assert str(info.value) == case["error"]
    else:
        n = SLTMPlatonic(case["args"])
        assert n.as_text() == case["as_text"]
        assert (n is SLTMPlatonic.create("1")) == bool(case["same_as_create"])


def test_new_nonhash():
    with pytest.raises(Confess) as info:
        SLTMPlatonic(1, 2)
    assert str(info.value) == one("new_nonhash")["error"]


def test_reset_clears_memo():
    p = SLTMPlatonic.create("5")
    sltm_platonic.reset()
    assert SLTMPlatonic.create("5") is not p


# ---- Callers ----

@pytest.mark.parametrize("case", cases("sint_pure"), ids=lambda c: str(c["mag"]))
def test_sint_get_pure(case):
    pure = SInt(case["mag"]).get_pure()
    assert pure.as_text() == case["as_text"]
    assert (pure is SLTMPlatonic.create(case["mag"])) == bool(case["same"])


def test_object_get_pure():
    e_case, g_case = cases("object_pure")
    for obj, case in ((Element.create(4, 0), e_case), (SeqseeObject.create(1, [2, 3]), g_case)):
        assert obj.get_structure_string() == case["structure_string"]
        pure = obj.get_pure()
        assert pure.as_text() == case["as_text"]
        assert as_perl(pure.get_structure()) == case["structure"]
        assert (pure is SLTMPlatonic.create(obj.get_structure_string())) == bool(case["same"])


# ---- LTMStorable ----

def test_ltmstorable_delegates_to_sltm(monkeypatch):
    case = one("ltmstorable")
    calls = []

    def spike(amount, *items):
        calls.append(["SpikeBy", amount, *(i.as_text() for i in items)])
        return "spiked"

    def link(*items):
        calls.append(["InsertISALink", *(i.as_text() for i in items)])
        return "linked"

    monkeypatch.setattr(ltmstorable, "_sltm_spike_by", spike)
    monkeypatch.setattr(ltmstorable, "_sltm_insert_isa_link", link)
    returns = [S.ASCENDING.spike_by(5), S.ASCENDING.insert_isa_link(S.DESCENDING), S.ODD.spike_by()]
    assert calls == case["calls"]
    assert returns == case["returns"]
    assert [int(isinstance(c, LTMStorable)) for c in (S.ASCENDING, S.ODD)] == case["does"]


def test_platonic_is_not_ltmstorable():
    assert one("platonic_not_ltmstorable")["dies"] == 1
    assert not hasattr(SLTMPlatonic.create(5), "spike_by")


def test_sltm_hooks_reach_sltm():
    # Item 029: the hooks call the real SLTM::SpikeBy/InsertISALink (more in test_sltm_core).
    from seqsee import sltm
    assert ltmstorable._sltm_spike_by(1, S.ODD) == sltm.get_real_activations_for_one_concept(S.ODD)
    assert ltmstorable._sltm_insert_isa_link(S.ODD, S.EVEN) is sltm.insert_isa_link(S.ODD, S.EVEN)


def test_independent():
    case = one("independent")
    c = S.ASCENDING
    assert c.is_pure() == case["is_pure"]
    assert (c.get_pure() is c) == bool(case["pure_same"])
    assert c.get_memory_dependencies() == case["deps"]
    assert c.serialize() == case["serialize"]
    assert util.perl_ref(Ascending.deserialize(c.serialize())) == case["deserialize_ref"]


# ---- Memory::* ----

LOG = []


class TStore(Storable):
    """The oracle's T::Store."""

    perl_name = "T::Store"

    def __init__(self, name, deps=None):
        self.name = name
        self.deps = list(deps or [])

    def get_memory_dependencies(self):
        LOG.append(self.name)
        return list(self.deps)

    def serialize(self):
        return self.name

    @classmethod
    def deserialize(cls, string):
        return None


class TIns(Insertible):
    """The oracle's T::Ins."""

    perl_name = "T::Ins"

    def __init__(self, target):
        self.target = target

    def get_normalized_for_memory(self):
        return self.target


class TPlain:
    """The oracle's T::Plain: a Moose object (so it has ``does``) without the role."""

    perl_name = "T::Plain"

    def does(self, role):
        return False


class Foo:
    perl_name = "Foo"


@pytest.fixture(autouse=True)
def _clear_log():
    LOG.clear()
    yield
    LOG.clear()


def test_memory_ltm_load_quirk():
    case = one("memory_ltm_load")
    assert case == {"op": "memory_ltm_load", "loaded": 0, "error": "syntax error"}


def _err(fn):
    with pytest.raises(Confess) as info:
        fn()
    return str(info.value)


def test_memory_ltm_scenario():
    """Replays the oracle's Memory::LTM sequence; state (including the leaked
    "currently installing" marks) carries over between steps, as in Perl."""
    a = TStore("a")
    b = TStore("b", [a])
    nb = ltm.insert_item(b)
    case = one("insert")
    assert util.perl_ref(nb) == case["node_ref"]
    assert nb.core().name == case["core"]
    assert LOG == case["log"]

    case = one("insert_again")
    assert (ltm.insert_item(b) is nb) == bool(case["same"])
    assert LOG == case["log"]

    case = one("insert_normalized")
    assert ltm.insert_item(TIns(a)).core().name == case["core"]
    assert LOG == case["log"]

    case = one("insert_via_normalized")
    x = TStore("x")
    nx = ltm.insert_item(TIns(x))
    assert nx.core().name == case["core"]
    assert (ltm.insert_item(x) is nx) == bool(case["same"])
    assert LOG == case["log"]

    c = TStore("c", [a])
    d = TStore("d", [b, c])
    LOG.clear()
    ltm.insert_item(d)
    assert LOG == one("insert_diamond")["log"]

    case = one("storable_normalized")
    assert (a.get_normalized_for_memory() is a) == bool(case["same"])
    assert int(a.does("Memory::Insertible")) == case["does"]

    for name, fn in (("spike", lambda: ltm.spike_by(5, a)),
                     ("weaken", lambda: ltm.weaken_by(5, a)),
                     ("spike_none", lambda: ltm.spike_by(5))):
        case = one("spike_weaken", case=name)
        if case["ok"]:
            fn()
        else:
            assert _err(fn) == case["error"]

    LOG.clear()
    fresh = TStore("fresh")
    case = one("spike_inserts_first")
    assert _err(lambda: ltm.spike_by(5, fresh)) == case["error"]
    assert ltm.insert_item(fresh).core().name == case["core"]
    assert LOG == case["log"]

    LOG.clear()
    for name, fn in (("normalized_string", lambda: ltm.insert_item(TIns("str"))),
                     ("string", lambda: ltm.insert_item("str")),
                     ("blessed", lambda: ltm.insert_item(Foo())),
                     ("undef", lambda: ltm.insert_item(None)),
                     ("plain_moose", lambda: ltm.insert_item(TPlain()))):
        expected = one("insert_dies", case=name)["error"]
        got = _err(fn)
        if name == "plain_moose":
            # Perl shows the address; compare up to it.
            assert expected.startswith("Non-insertible object 'T::Plain=HASH(0x")
            assert got.startswith("Non-insertible object 'T::Plain=HASH(0x")
        else:
            assert got == expected

    p = TStore("p")
    q = TStore("q", [p])
    p.deps = [q]
    LOG.clear()
    case = one("loop")
    assert _err(lambda: ltm.insert_item(p)) == case["error"]
    assert LOG == case["log"]
    LOG.clear()
    case = one("loop_again")
    assert _err(lambda: ltm.insert_item(p)) == case["error"]
    assert LOG == case["log"]
    case = one("loop_q")
    assert _err(lambda: ltm.insert_item(q)) == case["error"]
    assert LOG == case["log"]
    self_dep = TStore("self")
    self_dep.deps = [self_dep]
    assert _err(lambda: ltm.insert_item(self_dep)) == one("self_loop")["error"]


def test_memory_ltm_reset():
    a = TStore("a")
    n = ltm.insert_item(a)
    with pytest.raises(Confess):
        ltm.insert_item(TIns("str"))
    ltm.reset()
    assert ltm.insert_item(a) is not n
    p = TStore("p")
    p.deps = [p]
    assert _err(lambda: ltm.insert_item(p)) == "Loop in dependencies detected! Currently installing: 1"


def test_memory_node():
    a, b = TStore("na"), TStore("nb")
    n = Node(core=a)
    assert n.core().name == one("node_new")["core"]
    case = one("node_set")
    ret = n.core(b)
    assert n.core().name == case["core"]
    assert ret.name == case["ret"]
    assert Node({"core": a}).core().name == one("node_new_hashref")["core"]
    for name, fn in (("missing", lambda: Node()),
                     ("blessed", lambda: Node(core=Foo())),
                     ("number", lambda: Node(core=1)),
                     ("string", lambda: Node(core="x")),
                     ("undef", lambda: Node(core=None)),
                     ("insertible", lambda: Node(core=TIns(a))),
                     ("set_number", lambda: n.core(1)),
                     ("set_undef", lambda: n.core(None))):
        expected = one("node_dies", case=name)["error"]
        got = _err(fn)
        if name in ("blessed", "insertible"):
            # Moose dumps the object's contents; compare up to "with value".
            prefix = expected.split(" with value ")[0]
            assert got.startswith(prefix + " with value ")
        else:
            assert got == expected
    assert n.core() is b


def test_roles():
    assert Insertible.ROLES == ("Memory::Insertible",)
    s = TStore("s")
    assert s.does("Memory::Storable") and s.does("Memory::Insertible")
    assert not TIns(s).does("Memory::Storable")
    assert TIns(s).does("Memory::Insertible")

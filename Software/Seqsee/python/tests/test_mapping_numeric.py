"""Tests for seqsee.mapping.numeric, mirroring Mapping/Numeric.pm, and the part of
SLTM.pm that its create memo relies on (encode/decode, InsertUnlessPresent → seqsee.sltm).

Golden data: oracle/mapping_numeric.pl → tests/golden/mapping_numeric.json.
"""
import pytest

import golden
from seqsee import s as S
from seqsee import sltm
from seqsee.categories import numeric
from seqsee.categories.alternating import Alternating
from seqsee.categories.ascending import Ascending
from seqsee.categories.descending import Descending
from seqsee.categories.mapping_based import MappingBased
from seqsee.categories.sameness import Sameness
from seqsee.errors import Confess
from seqsee.mapping import Mapping
from seqsee.mapping import numeric as mnum
from seqsee.mapping.numeric import MappingNumeric
from seqsee.sint import SInt

CASES = golden.load("mapping_numeric")


def cases(kind):
    return [c for c in CASES if c["kind"] == kind]


class FakeAlt(Alternating):
    """Perl: FakeAlt, @ISA = SCategory::Alternating."""

    perl_name = "FakeAlt"

    def __init__(self):
        pass

    def as_text(self):
        return "fakealt"

    def get_pure(self):
        return self

    def get_memory_dependencies(self):
        return []


ALT = FakeAlt()
NAMED = {"NUMBER": S.NUMBER, "EVEN": S.EVEN, "ODD": S.ODD, "PRIME": S.PRIME, "ALT": ALT}
TOKENS = {"<S1>": chr(129), "<S2>": chr(130), "<C1>": chr(131), "<C2>": chr(132), "<C3>": chr(133)}


def vis(string):
    if string is None:
        return None
    for tok, ch in TOKENS.items():
        string = string.replace(ch, tok)
    return string


def unvis(string):
    for tok, ch in TOKENS.items():
        string = string.replace(tok, ch)
    return string


def cat(spec):
    if spec in NAMED:
        return NAMED[spec]
    if spec == "u":
        return None
    kind, _, rest = spec.partition(":")
    if kind == "s":
        return rest
    if kind == "i":
        return SInt(int(rest))
    if kind == "h":
        key, _, value = rest.partition("=")
        return {key: value}
    raise ValueError(spec)


def catdesc(c):
    if c is None:
        return "undef"
    if isinstance(c, str):
        return "s:" + c
    if isinstance(c, SInt):
        return f"i:{c.get_mag()}"
    for name, obj in NAMED.items():
        if c is obj:
            return name
    if isinstance(c, dict):
        return "HASH"
    return "OBJ:" + type(c).__name__


def assert_error_matches(perl_error, py_error):
    """Perl messages carry Moose's " at constructor …" tail and Perl's own rendering of
    the bad value; compare the Python message up to " with value"."""
    assert perl_error.startswith(str(py_error).split(" with value")[0])


# --- constructor --------------------------------------------------------------------

@pytest.mark.parametrize("case", cases("new"), ids=lambda c: repr(c["args"]))
def test_new_golden(case):
    args = dict(case["args"])
    if "category" in args:
        args["category"] = cat(args["category"])
    if args.get("name") == "ARRAY":
        args["name"] = []
    if case["error"]:
        with pytest.raises(Confess) as err:
            MappingNumeric(args)
        assert_error_matches(case["error"], err.value)
        return
    m = MappingNumeric(args)
    assert m.get_name() == case["name"]
    assert catdesc(m.get_category()) == case["category"]
    assert isinstance(m, Mapping) == bool(case["isa_mapping"])
    assert m.check_sanity() == case["check_sanity"]


def test_new_accepts_kwargs_and_dict():
    a = MappingNumeric(name="succ", category=S.NUMBER)
    b = MappingNumeric({"name": "succ", "category": S.NUMBER})
    assert a is not b
    assert a.get_name() == b.get_name() == "succ"
    assert a.perl_name == "Mapping::Numeric"


@pytest.mark.parametrize("case", cases("create_error"), ids=lambda c: repr(c["name"]))
def test_create_errors_golden(case):
    with pytest.raises(Confess, match="^" + case["error"] + "$"):
        MappingNumeric.create(case["name"], S.NUMBER)
    assert mnum._MEMO == {}


def test_set_name_checks_str():
    case = cases("set_name_undef")[0]
    m = MappingNumeric(name="pred", category=S.EVEN)
    with pytest.raises(Confess) as err:
        m.set_name(None)
    assert_error_matches(case["error"], err.value)
    assert m.get_name() == case["name"]


# --- the create memo, SLTM encode/decode, serialize/deserialize, FlippedVersion -----

def test_script_golden():
    """Replay the oracle's sequence against a fresh memo and LTM (conftest resets them)."""
    results = {}
    objs = []

    def label(m):
        for i, o in enumerate(objs):
            if o is m:
                return i
        objs.append(m)
        return len(objs) - 1

    for case in cases("script"):
        what, *args = case["op"]
        r = None
        got = {"kind": "script", "step": case["step"], "op": case["op"]}
        obj = results.get(args[0]) if what in ("serialize", "deserialize", "flip",
                                                 "insert_mapping") else None
        try:
            if what == "create":
                r = MappingNumeric.create(args[0], cat(unvis(args[1])))
            elif what == "insert":
                got["index"] = sltm.insert_unless_present(cat(args[0]))
            elif what == "insert_mapping":
                got["index"] = sltm.insert_unless_present(obj)
                got["node_count"] = sltm.get_node_count()
            elif what == "serialize":
                got["result"] = vis(obj.serialize())
            elif what in ("deserialize", "deserialize_str"):
                string = obj.serialize() if what == "deserialize" else unvis(args[0])
                r = MappingNumeric.deserialize(string)
            elif what == "flip":
                r = obj.flipped_version()
        except Confess as e:
            got["error"] = str(e)
        if r is not None:
            got.update(label=label(r), name=r.get_name(), category=catdesc(r.get_category()))
        results[case["step"]] = r
        assert got == case, case


def test_memo_collides_while_category_is_not_in_ltm():
    """PERL-QUIRK (oracle-confirmed): the memo key is SLTM::encode(name, category), and an
    uninserted category encodes as an empty index."""
    a = MappingNumeric.create("succ", S.EVEN)
    assert MappingNumeric.create("succ", S.NUMBER) is a
    assert a.get_category() is S.EVEN
    sltm.insert_unless_present(S.NUMBER)
    b = MappingNumeric.create("succ", S.NUMBER)
    assert b is not a and b.get_category() is S.NUMBER
    assert MappingNumeric.create("succ", S.NUMBER) is b


def test_numeric_category_hook_uses_create():
    m = numeric._mapping_numeric_create("pred", S.ODD)
    assert isinstance(m, MappingNumeric)
    assert m is MappingNumeric.create("pred", S.ODD)


# --- sltm encode/decode on their own ---------------------------------------------------

def test_sltm_encode_decode():
    assert sltm.encode("a", None, 3, SInt(4)) == "a\x81\x81" + "3\x81\x854"
    i = sltm.insert_unless_present(S.EVEN)
    assert sltm.encode(S.EVEN) == "\x83" + str(i)
    assert sltm.decode("\x83" + str(i)) == [S.EVEN]
    assert sltm.decode("x\x81\x83\x81") == ["x", "!!!"]   # trailing empty field dropped
    assert sltm.decode("\x8499") == [{"99": None}]       # odd list: the last value is undef
    (h,) = sltm.decode(sltm.encode({"k": S.EVEN}))
    assert h == {"k": S.EVEN}
    (n,) = sltm.decode("\x857")
    assert isinstance(n, SInt) and n.get_mag() == 7
    with pytest.raises(Confess, match="Recursive hash"):
        sltm.encode({"k": {}})
    with pytest.raises(Confess, match="unrecognized reference"):
        sltm.encode({"k": S.ODD})


def test_sltm_insert_node():
    with pytest.raises(Confess, match="bogus"):
        sltm.insert_node("x")
    m = MappingNumeric(name="succ", category=S.PRIME)
    assert sltm.insert_unless_present(m) == 2          # PRIME (its dependency) is 1
    assert sltm.memory_index(S.PRIME) == 1
    assert sltm.MEMORY[1] is S.PRIME and sltm.MEMORY[2] is m
    assert sltm.insert_unless_present(m) == 2
    assert sltm.get_node_count() == 2
    sltm.clear()
    assert sltm.get_node_count() == 0 and sltm.memory_index(m) is None


# --- methods ---------------------------------------------------------------------------

RBC = {"SCategory::Sameness": Sameness, "SCategory::Ascending": Ascending,
       "SCategory::Descending": Descending, "SCategory::MappingBased": MappingBased}


@pytest.mark.parametrize("case", cases("methods"), ids=lambda c: f"{c['category']}-{c['name']}")
def test_methods_golden(case):
    m = MappingNumeric(name=case["name"], category=cat(case["category"]))
    assert m.is_effectively_a_sameness_relation() == case["is_sameness"]
    assert (m.get_pure() is m) == bool(case["pure_is_self"])
    assert [catdesc(d) for d in m.get_memory_dependencies()] == case["deps"]
    for meth in ("as_text", "get_complexity"):
        if f"{meth}_error" in case:
            with pytest.raises(Confess) as err:
                getattr(m, meth)()
            assert str(err.value) == case[f"{meth}_error"]
        else:
            assert getattr(m, meth)() == case[meth]
    if "rbc_error" in case:
        with pytest.raises(Confess, match="^" + case["rbc_error"] + "$"):
            m.get_relation_based_category()
        return
    rbc = m.get_relation_based_category()
    assert type(rbc) is RBC[case["rbc"]]
    if case["rbc_is"] == "singleton":
        assert any(rbc is x for x in (S.ASCENDING, S.SAMENESS, S.DESCENDING))
    else:
        assert case["rbc_is"] == "mapping_based_of_self"
        assert rbc.get_transform() is m
    assert (m.get_relation_based_category() is rbc) == bool(case["rbc_memo"])


def test_as_text_and_complexity_are_memoized_per_object():
    case = cases("stale")[0]
    m = MappingNumeric(name="succ", category=S.EVEN)
    assert [m.as_text(), m.get_complexity()] == case["before"]
    m.set_name("same")
    m.set_category(S.NUMBER)
    assert [m.as_text(), m.get_complexity()] == case["after"]
    assert m.is_effectively_a_sameness_relation() == case["is_sameness_after"]
    assert type(m.get_relation_based_category()) is RBC[case["rbc_after"]]
    fresh = cases("stale_fresh")[0]
    n = MappingNumeric(name="succ", category=S.EVEN)
    n.set_name("pred")
    assert n.as_text() == fresh["as_text"]
    assert n.get_complexity() == fresh["get_complexity"]

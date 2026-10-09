"""Tests for seqsee.mapping.meto_type, mirroring Mapping/MetoType.pm (Mapping::MetoType,
and its FindMapping(SMetonymType, SMetonymType) / ApplyMapping(Mapping::MetoType,
SMetonymType) variants), plus the Perl ``each``-iterator emulation in seqsee.util.

Golden data: oracle/mapping_meto_type.pl → tests/golden/mapping_meto_type.json.
"""
import re

import pytest

import golden
from seqsee import s as S
from seqsee import sltm, util
from seqsee.constants import DIR
from seqsee.errors import Confess
from seqsee.mapping import Mapping, apply_mapping, find_mapping
from seqsee.mapping import meto_type as mmt
from seqsee.mapping.dir import MappingDir
from seqsee.mapping.meto_type import MappingMetoType
from seqsee.mapping.numeric import MappingNumeric
from seqsee.mapping.position import MappingPosition
from seqsee.sint import SInt
from seqsee.smetonym_type import SMetonymType
from seqsee.spos import SPos

CASES = golden.load("mapping_meto_type")
CATS = {"NUMBER": S.NUMBER, "EVEN": S.EVEN, "ODD": S.ODD}
DIRS = {"left": DIR.LEFT, "right": DIR.RIGHT, "unknown": DIR.UNKNOWN, "neither": DIR.NEITHER}
TOKENS = {"<S1>": chr(129), "<S2>": chr(130), "<C1>": chr(131), "<C2>": chr(132), "<C3>": chr(133)}


def cases(kind):
    return [c for c in CASES if c["kind"] == kind]


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
    if spec in CATS:
        return CATS[spec]
    if spec == "u":
        return None
    assert spec.startswith("s:"), spec
    return spec[2:]


def catdesc(c):
    if c is None:
        return "u"
    if isinstance(c, (str, int, float)):
        return "s:" + util.perl_str(c)
    for name, obj in CATS.items():
        if c is obj:
            return name
    return "OBJ:" + util.perl_ref(c)


def val(spec):
    if spec == "u":
        return None
    kind, _, rest = spec.partition(":")
    if kind == "N":
        return MappingNumeric.create(rest, S.NUMBER)
    if kind == "MD":
        return MappingDir.create(rest)
    if kind == "MP":
        return MappingPosition.create(rest)
    if kind == "s":
        return rest
    if kind == "i":
        return SInt(int(rest))
    if kind == "n":
        return int(rest)
    if kind == "D":
        return DIRS[rest]
    if kind == "P":
        return SPos(int(rest))
    raise ValueError(spec)


def valdesc(v):
    if v is None:
        return "u"
    if isinstance(v, str):
        return "s:" + v
    if isinstance(v, (int, float)):
        return "n:" + util.perl_str(v)
    if isinstance(v, MappingNumeric):
        return f"N:{v.get_name()}/{catdesc(v.get_category())}"
    if isinstance(v, MappingDir):
        return "MD:" + v.string
    if isinstance(v, MappingPosition):
        return "MP:" + v.get_text()
    if isinstance(v, SInt):
        return f"i:{v.get_mag()}"
    if isinstance(v, DIR):
        return "D:" + v.text
    if isinstance(v, SPos):
        return f"P:{v.position}"
    return "OBJ:" + util.perl_ref(v)


def hash_of(pairs):
    if pairs is None or pairs == "u":
        return None
    return {k: val(spec) for k, spec in pairs}


def hashdesc(h):
    if h is None:
        return None
    return [[k, valdesc(h[k])] for k in sorted(h)]


def smt(c, n, pairs):
    return SMetonymType({"category": cat(c), "name": n, "info_loss": hash_of(pairs)})


def assert_error_matches(perl_error, py_error):
    """Moose messages end in " (defined …"; Perl renders bad values its own way, so compare
    up to " with value" when there is one."""
    py = str(py_error)
    if " with value" in py:
        assert perl_error.startswith(py.split(" with value")[0])
    else:
        assert perl_error == py or perl_error.startswith(py + " (defined")


# --- new / setters -------------------------------------------------------------------

def _new_args(spec):
    args = {}
    if "category" in spec:
        args["category"] = cat(spec["category"])
    if "name" in spec:
        args["name"] = {"u": None, "ARRAY": []}.get(spec["name"], spec["name"])
    if "change" in spec:
        args["change_ref"] = hash_of(spec["change"])
    return args


@pytest.mark.parametrize("case", cases("new"), ids=lambda c: repr(c["args"]))
def test_new_golden(case):
    args = _new_args(case["args"])
    if case["error"]:
        with pytest.raises(Confess) as err:
            MappingMetoType(args)
        assert_error_matches(case["error"], err.value)
        return
    m = MappingMetoType(args)
    assert m.get_name() == case["name"]
    assert catdesc(m.get_category()) == case["category"]
    assert hashdesc(m.get_change_ref()) == case["change"]
    assert isinstance(m, Mapping) == bool(case["isa_mapping"])
    assert (m.get_pure() is m) == bool(case["pure_is_self"])


def test_new_kwargs_and_perl_name():
    a = MappingMetoType(category=S.NUMBER, name="x", change_ref={})
    b = MappingMetoType({"category": S.NUMBER, "name": "x", "change_ref": {}})
    assert a is not b
    assert a.perl_name == "Mapping::MetoType"
    assert not isinstance(a, Mapping)


def test_setters_golden():
    case = cases("setters")[0]
    m = MappingMetoType(category=S.NUMBER, name="x", change_ref={})
    m.set_name("y")
    m.set_category(S.EVEN)
    m.set_change_ref({"a": val("N:same")})
    with pytest.raises(Confess) as err:
        m.set_name(None)
    assert_error_matches(case["set_name_undef_error"], err.value)
    assert m.get_name() == case["name"]
    assert catdesc(m.get_category()) == case["category"]
    assert hashdesc(m.get_change_ref()) == case["change"]


# --- the scripted sequence -----------------------------------------------------------

def _deps_desc(x):
    d = catdesc(x)
    return valdesc(x) if d.startswith("OBJ") else d


def test_script_golden():
    """Replay the oracle's sequence against fresh memos and LTM (conftest resets them)."""
    results = {}
    objs = []

    def label(m):
        for i, o in enumerate(objs):
            if o is m:
                return i
        objs.append(m)
        return len(objs) - 1

    def mtdesc(m):
        if m is None:
            return {"result": None}
        return {"label": label(m), "name": m.get_name(), "category": catdesc(m.get_category()),
                "change": hashdesc(m.get_change_ref())}

    for case in cases("script"):
        what, *args = case["op"]
        got = {"kind": "script", "step": case["step"], "op": case["op"]}
        obj = None
        if what in ("flip", "serialize", "deserialize", "insert_mt", "apply", "sameness",
                    "deps", "as_text"):
            obj = results.get(args[0])
            if obj is None:
                results[case["step"]] = None
                got["missing"] = 1
                assert got == case, case
                continue
        r = None
        mt_result = what in ("create", "flip", "find", "deserialize", "deserialize_str")
        try:
            if what == "create":
                c, n, pairs = args
                r = MappingMetoType.create({"category": cat(c), "name": None if n == "u" else n,
                                            "change_ref": hash_of(pairs)})
            elif what == "flip":
                r = obj.flipped_version()
            elif what == "find":
                r = find_mapping(smt(*args[0]), smt(*args[1]))
            elif what == "apply":
                t = smt(*args[1])
                res = apply_mapping(obj, t)
                got.update(name=res.get_name(), category=catdesc(res.get_category()),
                           info_loss=hashdesc(res.get_info_loss()),
                           is_smt=int(type(res) is SMetonymType))
                again = apply_mapping(obj, t)
                got["fresh"] = int(again is not res)
                got["info_loss_shared"] = int(again.get_info_loss() is res.get_info_loss())
            elif what == "sameness":
                got["result"] = obj.is_effectively_a_sameness_relation()
            elif what == "deps":
                got["result"] = [_deps_desc(x) for x in obj.get_memory_dependencies()]
            elif what == "as_text":
                # The oracle's rendering: « » → < >, then every < and > doubled.
                text = obj.as_text().replace("\xab", "<").replace("\xbb", ">")
                got["result"] = re.sub(r"([<>])", r"\1\1", text)
            elif what == "serialize":
                got["result"] = None
                got["result"] = vis(obj.serialize())
            elif what in ("deserialize", "deserialize_str"):
                string = obj.serialize() if what == "deserialize" else unvis(args[0])
                r = MappingMetoType.deserialize(string)
            elif what == "insert":
                x = CATS[args[0]] if args[0] in CATS else val(args[0])
                got["index"] = sltm.insert_unless_present(x)
            elif what == "insert_mt":
                got["index"] = sltm.insert_unless_present(obj)
                got["node_count"] = sltm.get_node_count()
        except Confess as e:
            got["error"] = str(e)
        if mt_result:
            got.update(mtdesc(r))
        results[case["step"]] = r
        if what == "deps":   # Perl lists the hash values in hash order
            assert got["result"][:1] == case["result"][:1]
            assert sorted(got.pop("result")[1:]) == sorted(case["result"][1:])
            case = {k: v for k, v in case.items() if k != "result"}
        if case.get("error") == got.get("error", "") + " (defined":   # Moose's tail
            got["error"] = case["error"]
        if "unrecognized reference to '" in case.get("error", ""):
            # The message carries the object's address.
            assert got.get("error", "").startswith("unrecognized reference to 'Mapping::Numeric=HASH(")
            got["error"] = case["error"]
        assert got == case, case


def test_create_memo_key_sorts_pairs_and_keys_objects_by_identity():
    a = MappingMetoType.create({"category": S.NUMBER, "name": "x",
                                "change_ref": {"a": val("N:succ"), "b": val("MD:Same")}})
    b = MappingMetoType.create({"category": S.NUMBER, "name": "x",
                                "change_ref": {"b": val("MD:Same"), "a": val("N:succ")}})
    assert a is b
    c = MappingMetoType.create({"category": S.NUMBER, "name": "x",
                                "change_ref": {"a": MappingNumeric(name="succ", category=S.NUMBER)}})
    assert c is not a
    # PERL-QUIRK: a plain join: undef and "" categories collide, and so do "x;a;q" + {} and
    # "x" + {a => "q"}.
    d = MappingMetoType.create({"category": None, "name": "x", "change_ref": {}})
    assert MappingMetoType.create({"category": "", "name": "x", "change_ref": {}}) is d
    e = MappingMetoType.create({"category": S.NUMBER, "name": "x;a;q", "change_ref": {}})
    assert MappingMetoType.create({"category": S.NUMBER, "name": "x",
                                   "change_ref": {"a": "q"}}) is e


def test_create_change_ref_errors():
    with pytest.raises(Confess, match="Can't use an undefined value as a HASH reference"):
        MappingMetoType.create({"category": S.NUMBER, "name": "x"})
    with pytest.raises(Confess, match="Not a HASH reference"):
        MappingMetoType.create({"category": S.NUMBER, "name": "x", "change_ref": []})
    assert mmt._MEMO == {}


def test_reset_clears_memo():
    a = MappingMetoType.create({"category": S.NUMBER, "name": "x", "change_ref": {}})
    mmt.reset()
    assert MappingMetoType.create({"category": S.NUMBER, "name": "x", "change_ref": {}}) is not a


# --- the each-iterator quirk ---------------------------------------------------------

def _ordered(order, spec):
    return {k: val(spec[k]) for k in order}


def test_each_quirk_golden():
    case = cases("each_quirk")[0]
    spec = {"a": "N:same", "b": "N:succ", "c": "N:same", "d": "N:succ"}
    m = MappingMetoType(category=S.NUMBER, name="x", change_ref=_ordered(case["order"], spec))
    seq = [1 if m.is_effectively_a_sameness_relation() else 0 for _ in range(7)]
    assert seq == case["sameness_seq"]
    assert sorted(m.flipped_version().get_change_ref()) == case["flipped_keys"]
    assert sorted(m.flipped_version().get_change_ref()) == case["flipped_again_keys"]


def test_each_reset_golden():
    case = cases("each_reset")[0]
    spec = {"a": "N:same", "b": "N:succ", "c": "N:same", "d": "N:same"}
    m = MappingMetoType(category=S.NUMBER, name="x", change_ref=_ordered(case["order"], spec))
    assert (1 if m.is_effectively_a_sameness_relation() else 0) == case["first"]
    assert (1 if m.is_effectively_a_sameness_relation() else 0) == case["without_reset"]
    m.is_effectively_a_sameness_relation()
    util.perl_keys(m.get_change_ref())
    assert (1 if m.is_effectively_a_sameness_relation() else 0) == case["after_keys"]


def test_each_find_apply_golden():
    case = cases("each_find_apply")[0]
    spec = {"a": "P:1", "b": "P:-1", "c": "P:1", "d": "P:1"}
    t1 = SMetonymType({"category": S.NUMBER, "name": "each",
                       "info_loss": _ordered(case["order"], spec)})
    t2 = SMetonymType({"category": S.NUMBER, "name": "each",
                       "info_loss": {k: SPos(2) for k in case["order"]}})
    assert (0 if find_mapping(t1, t2) is None else 1) == case["found"]
    mt = MappingMetoType.create({"category": S.NUMBER, "name": "each", "change_ref": {}})
    assert sorted(apply_mapping(mt, t1).get_info_loss()) == case["applied_keys"]
    assert sorted(apply_mapping(mt, t1).get_info_loss()) == case["applied_again_keys"]


def test_perl_each_helpers():
    d = {"a": 1, "b": 2, "c": 3}
    for k, _v in util.perl_each(d):
        if k == "a":
            break
    assert list(util.perl_each(d)) == [("b", 2), ("c", 3)]
    assert list(util.perl_each(d)) == [("a", 1), ("b", 2), ("c", 3)]
    next(iter(util.perl_each(d)))
    assert util.perl_keys(d) == ["a", "b", "c"]
    assert list(util.perl_each(d)) == [("a", 1), ("b", 2), ("c", 3)]
    next(iter(util.perl_each(d)))
    util.reset_each_iterators()
    assert list(util.perl_each(d)) == [("a", 1), ("b", 2), ("c", 3)]


# --- dispatch ------------------------------------------------------------------------

def test_dispatch_golden():
    case = cases("dispatch")[0]
    mt = MappingMetoType.create({"category": S.NUMBER, "name": "x", "change_ref": {}})
    t = smt("NUMBER", "x", [])
    for key, call in (("find_mt_mt", lambda: find_mapping(mt, mt)),
                      ("apply_smt_mt", lambda: apply_mapping(t, mt)),
                      ("apply_mt_num", lambda: apply_mapping(mt, 3))):
        with pytest.raises(Confess) as err:
            call()
        assert str(err.value) == case[key]


def test_as_text_shape():
    m = MappingMetoType(category=S.NUMBER, name="x", change_ref={"a": val("N:succ")})
    assert m.as_text() == "Mapping::MetoType(change=>{ a => \xabsucc\xbb,  }, category=>\xabnumber\xbb)"

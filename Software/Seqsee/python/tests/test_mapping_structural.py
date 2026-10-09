"""Tests for seqsee.mapping.structural, mirroring Mapping/Structural.pm
(Mapping::Structural), plus the parts of Mapping.pm (CheckSanity) and SCategory.pm
(FindMappingForCat's Mapping::Structural->create) it is reached through.

Golden data: oracle/mapping_structural.pl → tests/golden/mapping_structural.json.
"""
import re

import pytest

import golden
from seqsee import mapping, sltm, util
from seqsee import s as S
from seqsee.categories import base
from seqsee.categories.interlaced import Interlaced
from seqsee.constants import METO_MODE
from seqsee.errors import Confess
from seqsee.mapping import Mapping, apply_mapping, find_mapping
from seqsee.mapping import structural as mst
from seqsee.mapping.dir import MappingDir
from seqsee.mapping.meto_type import MappingMetoType
from seqsee.mapping.numeric import MappingNumeric
from seqsee.mapping.position import MappingPosition
from seqsee.mapping.structural import MappingStructural

CASES = golden.load("mapping_structural")
CATS = {"ASC": S.ASCENDING, "DESC": S.DESCENDING, "SAME": S.SAMENESS, "NUMBER": S.NUMBER,
        "PRIME": S.PRIME, "MOUNTAIN": S.MOUNTAIN}
MODES = {"NONE": METO_MODE.NONE, "SINGLE": METO_MODE.SINGLE,
         "ALLBUTONE": METO_MODE.ALLBUTONE, "ALL": METO_MODE.ALL}
TOKENS = {"<S1>": chr(129), "<S2>": chr(130), "<C1>": chr(131), "<C2>": chr(132), "<C3>": chr(133)}
ATTRS = ("category", "meto_mode", "position_reln", "metonymy_reln", "direction_reln")


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


def norm(text):
    """The oracle's rendering of addresses (they differ between Perl and Python)."""
    return re.sub(r"=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)", "=REF", text)


class Ctx:
    """Script state: results by step, and Mapping::Structural labels in first-seen order."""

    def __init__(self):
        self.results = {}
        self.objs = []

    def label(self, m):
        for i, o in enumerate(self.objs):
            if o is m:
                return i
        self.objs.append(m)
        return len(self.objs) - 1

    def thing(self, spec):
        if spec == "u":
            return None
        if spec in CATS:
            return CATS[spec]
        if spec in MODES:
            return MODES[spec]
        if spec in ("IL2", "IL3"):
            return Interlaced.create(int(spec[2]))
        kind, _, rest = spec.partition(":")
        if kind == "s":
            return rest
        if kind == "N":
            return MappingNumeric.create(rest, S.NUMBER)
        if kind == "MD":
            return MappingDir.create(rest)
        if kind == "MP":
            return MappingPosition.create(rest)
        if kind == "MT":
            name, key, num = rest.split(":")
            return MappingMetoType.create({"category": S.NUMBER, "name": name,
                                           "change_ref": {key: MappingNumeric.create(num, S.NUMBER)}})
        if kind == "ST":
            return self.results[int(rest)]
        raise ValueError(spec)

    def desc(self, v):
        if v is None:
            return "u"
        if util.perl_ref(v) == "":
            return "s:" + util.perl_str(v)
        for name, obj in CATS.items():
            if v is obj:
                return name
        for n in (2, 3):
            if v is Interlaced.create(n):
                return f"IL{n}"
        for name, obj in MODES.items():
            if v is obj:
                return name
        if isinstance(v, MappingNumeric):
            return f"N:{v.get_name()}/{self.desc(v.get_category())}"
        if isinstance(v, MappingDir):
            return "MD:" + v.string
        if isinstance(v, MappingPosition):
            return "MP:" + v.get_text()
        if isinstance(v, MappingMetoType):
            return "MT:" + v.get_name()
        if isinstance(v, MappingStructural):
            return f"ST#{self.label(v)}"
        if isinstance(v, dict):
            return "HASH"
        return "OBJ:" + util.perl_ref(v)

    def hashdesc(self, h):
        if not isinstance(h, dict):
            return self.desc(h)
        return [[k, self.desc(h[k])] for k in sorted(h, key=util.perl_str)]

    def hash_of(self, pairs, plain=False):
        if not isinstance(pairs, list):
            return self.thing(pairs)
        return {k: (v if plain else self.thing(v)) for k, v in pairs}

    def opts_of(self, spec):
        opts = {a: self.thing(spec[a]) for a in ATTRS if a in spec}
        if "cb" in spec:
            opts["changed_bindings"] = self.hash_of(spec["cb"])
        if "sl" in spec:
            opts["slippages"] = self.hash_of(spec["sl"], plain=True)
        return opts

    def stdesc(self, m, with_label=True):
        if m is None:
            return {"result": None}
        out = {a: self.desc(getattr(m, "get_" + a)()) for a in ATTRS}
        out.update(cb=self.hashdesc(m.get_changed_bindings()), sl=self.hashdesc(m.get_slippages()))
        if with_label:
            out["label"] = self.label(m)
        return out


def _as_text_parts(text):
    """as_text's binding parts come in Perl hash order: compare them as a multiset."""
    head, _, tail = text.partition("] ")
    return head, sorted(tail.split(";"))


def test_script_golden(monkeypatch):
    """Replay the oracle's sequence against fresh memos and LTM (conftest resets them)."""
    messages = []
    monkeypatch.setattr(mapping, "_message", lambda text: messages.append(norm(text)))
    ctx = Ctx()
    for case in cases("script"):
        what, arg = case["op"]
        messages.clear()
        got = {"kind": "script", "step": case["step"], "op": case["op"]}
        obj = None
        if what not in ("create", "new", "insert", "deserialize_str"):
            obj = ctx.results.get(arg)
            if obj is None:
                ctx.results[case["step"]] = None
                got["missing"] = 1
                assert got == case, case
                continue
        r = None
        try:
            if what in ("create", "new"):
                opts = ctx.opts_of(arg)
                try:
                    r = MappingStructural.create(opts) if what == "create" else MappingStructural(opts)
                finally:
                    got["opts_after"] = {k: ctx.hashdesc(opts[k]) for k in sorted(opts)}
                got.update(ctx.stdesc(r))
                got["isa_mapping"] = int(isinstance(r, Mapping))
                got["pure_is_self"] = int(r.get_pure() is r)
                cb = opts.get("changed_bindings")
                got["cb_is_given"] = int(isinstance(cb, dict) and r.get_changed_bindings() is cb)
            elif what == "flip":
                r = obj.flipped_version()
                got.update(ctx.stdesc(r))
            elif what == "sanity":
                got["result"] = obj.check_sanity()
            elif what == "sameness":
                got["result"] = None
                got["result"] = obj.is_effectively_a_sameness_relation()
            elif what == "deps":
                got["result"] = [ctx.desc(x) for x in obj.get_memory_dependencies()]
            elif what == "as_text":
                got["result"] = None
                got["result"] = obj.as_text()
            elif what == "complexity":
                got["result"] = None
                got["result"] = obj.get_complexity()
            elif what == "serialize":
                got["result"] = None
                got["result"] = vis(obj.serialize())
            elif what in ("deserialize", "deserialize_str"):
                string = obj.serialize() if what == "deserialize" else unvis(arg)
                r = MappingStructural.deserialize(string)
                got.update(ctx.stdesc(r))
            elif what == "insert":
                got["index"] = sltm.insert_unless_present(ctx.thing(arg))
            elif what == "insert_st":
                got["index"] = None
                got["index"] = sltm.insert_unless_present(obj)
                got["node_count"] = sltm.get_node_count()
        except Confess as e:
            got["error"] = norm(str(e))
            if case.get("error") == got["error"] + " (defined":   # Moose's tail
                got["error"] = case["error"]
        if messages:
            got["messages"] = list(messages)
        ctx.results[case["step"]] = r
        if what == "as_text" and case.get("result") and got.get("result"):
            assert _as_text_parts(got.pop("result")) == _as_text_parts(case["result"]), case
            case = {k: v for k, v in case.items() if k != "result"}
        if what == "complexity" and case.get("result") is not None:
            assert got.pop("result") == pytest.approx(case["result"]), case
            case = {k: v for k, v in case.items() if k != "result"}
        assert got == case, case


# --- create --------------------------------------------------------------------------

def _opts(**kw):
    base_opts = {"category": S.ASCENDING, "meto_mode": METO_MODE.NONE,
                 "direction_reln": MappingDir.create("Same")}
    return {**base_opts, **kw}


def test_create_memo_and_reset():
    succ = MappingNumeric.create("succ", S.NUMBER)
    a = MappingStructural.create(_opts(changed_bindings={"start": succ}))
    assert MappingStructural.create(_opts(changed_bindings={"start": succ})) is a
    # Values key by identity: an equal-looking `new` Mapping::Numeric is a different key.
    other = MappingNumeric(name="succ", category=S.NUMBER)
    assert MappingStructural.create(_opts(changed_bindings={"start": other})) is not a
    mst.reset()
    assert MappingStructural.create(_opts(changed_bindings={"start": succ})) is not a


def test_create_mutates_opts():
    """Perl: create writes 'x' into the caller's hash, and autovivifies the two hashes."""
    opts = _opts(position_reln="junk")
    m = MappingStructural.create(opts)
    assert opts["metonymy_reln"] == "x" and opts["position_reln"] == "x"
    assert opts["changed_bindings"] == {} and opts["slippages"] == {}
    assert m.get_changed_bindings() is opts["changed_bindings"]
    opts = {"category": S.SAMENESS, "meto_mode": METO_MODE.ALL, "direction_reln": None,
            "metonymy_reln": "M", "position_reln": "P"}
    MappingStructural.create(opts)
    assert opts["metonymy_reln"] == "M" and opts["position_reln"] == "x"


def test_create_errors():
    with pytest.raises(Confess, match="^need meto_mode$"):
        MappingStructural.create({"category": S.ASCENDING, "meto_mode": 0})
    with pytest.raises(Confess, match="Not a HASH reference"):
        MappingStructural.create(_opts(changed_bindings=[]))
    assert mst._MEMO == {}


def test_new_kwargs_and_perl_name():
    kw = dict(category=S.ASCENDING, meto_mode=METO_MODE.NONE, direction_reln=None,
              position_reln="x", metonymy_reln="x")
    a = MappingStructural(**kw)
    b = MappingStructural(kw)
    assert a is not b
    assert a.perl_name == "Mapping::Structural"
    assert isinstance(a, Mapping)
    assert a.get_slippages() == {} and a.get_slippages() is not b.get_slippages()
    a.set_category(S.DESCENDING)
    a.set_meto_mode(METO_MODE.ALL)
    a.set_position_reln("p")
    a.set_metonymy_reln("m")
    a.set_direction_reln("d")
    assert (a.get_category(), a.get_meto_mode(), a.get_position_reln(), a.get_metonymy_reln(),
            a.get_direction_reln()) == (S.DESCENDING, METO_MODE.ALL, "p", "m", "d")


def test_scategory_hook_uses_create():
    opts = _opts()
    m = base._structural_create(opts)
    assert isinstance(m, MappingStructural)
    assert MappingStructural.create(_opts()) is m


# --- golden extras -------------------------------------------------------------------

def _plain(ctx, **kw):
    return MappingStructural({"category": S.ASCENDING, "meto_mode": METO_MODE.NONE,
                              "direction_reln": MappingDir.create("Same"), "position_reln": "x",
                              "metonymy_reln": "x", **kw})


def test_flip_memo_golden():
    case = cases("flip_memo")[0]
    ctx = Ctx()
    m = _plain(ctx, changed_bindings={"start": MappingNumeric.create("succ", S.NUMBER)})
    f1 = m.flipped_version()
    m.get_changed_bindings()["start"] = MappingNumeric.create("same", S.NUMBER)
    f2 = m.flipped_version()
    m.set_category(S.DESCENDING)
    expected = {k: v for k, v in case["flip"].items() if k != "label"}
    assert int(f1 is f2) == case["same_object"]
    assert ctx.stdesc(f2, with_label=False) == expected
    assert ctx.desc(m.get_category()) == case["category_after_set"]


def test_flip_undef_is_memoized():
    m = _plain(Ctx(), slippages={"a": "x", "b": "x"})
    assert m.flipped_version() is None
    m.get_slippages()["b"] = "y"
    assert m.flipped_version() is None


def test_each_quirk_slippages_golden():
    case = cases("each_quirk_slippages")[0]
    spec = {"a": "a", "b": "x", "c": "c", "d": "y"}
    m = _plain(Ctx(), changed_bindings={}, slippages={k: spec[k] for k in case["order"]})
    seq = [1 if m.is_effectively_a_sameness_relation() else 0 for _ in range(7)]
    assert seq == case["sameness_seq"]
    m.is_effectively_a_sameness_relation()
    m.get_complexity()
    assert (1 if m.is_effectively_a_sameness_relation() else 0) == case["after_copy"]


def test_each_quirk_bindings_golden():
    case = cases("each_quirk_bindings")[0]
    spec = {"a": "same", "b": "succ", "c": "same", "d": "pred"}
    m = _plain(Ctx(), changed_bindings={k: MappingNumeric.create(spec[k], S.NUMBER)
                                        for k in case["order"]})
    seq = [1 if m.is_effectively_a_sameness_relation() else 0 for _ in range(7)]
    assert seq == case["sameness_seq"]
    m.is_effectively_a_sameness_relation()
    m.get_memory_dependencies()
    assert (1 if m.is_effectively_a_sameness_relation() else 0) == case["after_values"]


def test_dispatch_golden():
    case = cases("dispatch")[0]
    m = MappingStructural.create(_opts(changed_bindings={"start": MappingNumeric.create("succ", S.NUMBER)}))
    with pytest.raises(Confess) as err:
        apply_mapping(m, 3)
    assert str(err.value) == case["apply_num"]
    with pytest.raises(Confess) as err:
        find_mapping(m, m)
    assert str(err.value) == case["find_st_st"]

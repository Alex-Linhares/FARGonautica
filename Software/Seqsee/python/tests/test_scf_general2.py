"""Tests for the second half of the general codelet families (item 041).

Mirrors lib/Seqsee/SCF_MX/General.pm: the families FindIfRelatedRelations,
CheckIfAlternating, FindIfRelated (with ShouldIContinue) and AttemptExtensionOfRelation
(with EstimateAskability), packages Seqsee::SCF::<Name>, run through their installed ``run``
(MooseX/SCF.pm). Golden data: oracle/scf_general2.pl, whose scenarios are replayed here op
for op.
"""
import re

import pytest

import golden
from test_scf_general1 import Replay as _General1Replay
from test_scf_general1 import _same_error
from test_sthought import err
from test_sthought_sobject import _norm, mapping
from test_sworkspace_groups import same
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import scoderack, sltm, sworkspace, util
from seqsee.codelets import general
from seqsee.codelets.family import FAMILIES, family_run
from seqsee.constants import DIR
from seqsee.errors import Confess, ElementsBeyondKnownSought, ExceptionClassBase
from seqsee.objects.anchored import Anchored
from seqsee.srelation import SRelation

CASES = golden.load("scf_general2")

_ADDR = re.compile(r"=(?:HASH|SCALAR|ARRAY)\(0x[0-9a-f]+\)")


def noaddr(v):
    """The oracle's noaddr: an Alternating category's name stringifies its objects."""
    if isinstance(v, str):
        return _ADDR.sub("=REF", v)
    if isinstance(v, list):
        return [noaddr(x) for x in v]
    return v


def _num_or_undef(v):
    return None if v is None else util.perl_num(v)


class Replay(_General1Replay):
    """The oracle's op interpreter (scf_general1.pl's, plus this oracle's ops)."""

    def run(self, kind, args):
        if kind == "reln_only":
            name, a, b, t = args[:4]
            r = SRelation({"first": self.obj[a], "second": self.obj[b],
                           "type": mapping(t, args[4] if len(args) > 4 else None)})
            self.reg(name, r)
            return r.as_text()
        if kind == "spike_map":
            amount, *m = args
            sltm.spike_by(amount, mapping(*m))
            return sltm.get_real_activations_for_one_concept(mapping(*m))
        if kind == "spike_type":
            t = self.obj[args[0]].get_type()
            sltm.spike_by(args[1], t)
            return sltm.get_real_activations_for_one_concept(t)
        if kind == "activation_map":
            return sltm.get_real_activations_for_one_concept(mapping(*args))
        if kind == "activation_type":
            return sltm.get_real_activations_for_one_concept(self.obj[args[0]].get_type())
        if kind == "shouldicontinue":
            return general.should_i_continue(*args)
        if kind == "askability":
            r = self.obj[args[0]]
            return _num_or_undef(general.estimate_askability(r, r.get_type(), *r.get_ends()))
        if kind == "ask_details":
            out = []
            for cl in scoderack.CODELETS:
                if cl[0] != "AskIfThisIsTheContinuation":
                    continue
                a = cl[3]
                out.append([util.perl_ref(a["exception"]),
                            [util.perl_num(x) for x in a["exception"].next_elements],
                            a["expected_object"].as_text(), util.perl_num(a["start_position"]),
                            util.perl_num(a["known_term_count"]), self.nm(a["relation"])])
            return out
        if kind in ("state", "relations", "coderack"):
            return noaddr(super().run(kind, args))
        return super().run(kind, args)


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
            assert _same_error(got["error"], want["error"]), (op, got, want)
        else:
            assert "value" in got, (op, got, want)
            assert same(_norm(got["value"]), _norm(want["value"])), (op, got, want)


# --- tests from reading the source ------------------------------------------------------

def _elements(seq):
    sworkspace.init({"seq": seq})
    return sworkspace.get_elements()


def test_families_are_registered():
    for name, attrs in (("FindIfRelatedRelations", ["a", "b"]),
                        ("CheckIfAlternating", ["first", "second", "third"]),
                        ("FindIfRelated", ["a", "b"]),
                        ("AttemptExtensionOfRelation", ["core", "direction"])):
        fam = FAMILIES[name]
        assert fam.called == f"Seqsee::SCF::{name}::run"
        assert [a for a, _ in fam.attributes] == attrs
        assert all(spec == {"required": 1} for _, spec in fam.attributes)


def test_should_i_continue_formula():
    # 1 - complexity * (1 - activation) * sqrt(distance)
    assert general.should_i_continue(0.5, 0.2, 4) == pytest.approx(0.2)
    assert general.should_i_continue(1, 1, 100) == 1
    assert general.should_i_continue(0.25, 0, 0) == 1
    with pytest.raises(ValueError):  # Perl: "Can't take sqrt of -1"
        general.should_i_continue(1, 0, -1)


def test_find_if_related_relations_alternation_needs_the_feature():
    e = _elements([1, 2, 1])
    succ, pred = mapping("succ"), mapping("pred")
    a = SRelation({"first": e[0], "second": e[1], "type": succ})
    b = SRelation({"first": e[1], "second": e[2], "type": pred})
    family_run("FindIfRelatedRelations", None, {"a": a, "b": b})
    assert scoderack.CODELETS == []
    Global.Feature["Alternating"] = 1
    family_run("FindIfRelatedRelations", None, {"a": b, "b": a})  # swapped: same result
    (cl,) = scoderack.CODELETS
    assert cl[0] == "CreateGroup" and cl[1] == 100
    assert cl[3]["items"] == [e[0], e[1], e[2]]
    assert cl[3]["transform"].get_name() == "flip"


def test_find_if_related_overlap_needs_a_shared_subgroup(monkeypatch):
    # Perl: `$a->[-1] ~~ @$b`, i.e. a's last item must be one of b's items (by identity).
    e = _elements([1, 2, 3, 4, 5, 6])
    sltm.spike_by(10, S.NUMBER)
    succ = mapping("succ")
    a = Anchored.create(e[0], e[1], e[2])
    b = Anchored.create(e[2], e[3], e[4])
    sworkspace.add_group(a)
    sworkspace.add_group(b)
    r1 = SRelation({"first": e[0], "second": e[1], "type": succ})
    r1.insert()
    r2 = SRelation({"first": e[3], "second": e[4], "type": succ})
    r2.insert()
    a.set_underlying_ruleapp(r1)
    b.set_underlying_ruleapp(r2)
    monkeypatch.setattr(Anchored, "__getitem__", lambda self, i: e[5] if i == -1
                        else list(self.get_parts_ref())[i])
    family_run("FindIfRelated", None, {"a": a, "b": b})
    assert scoderack.CODELETS == []
    monkeypatch.undo()
    family_run("FindIfRelated", None, {"a": b, "b": a})
    assert [(cl[0], cl[1]) for cl in scoderack.CODELETS] == [("MergeGroups", 200)]
    assert scoderack.CODELETS[0][3] == {"a": a, "b": b}


def test_find_if_related_toss_uses_should_i_continue(monkeypatch):
    e = _elements([1, 9, 9, 2])
    seen = []
    monkeypatch.setattr(general, "should_i_continue",
                        lambda c, a, d: seen.append((c, a, d)) or 0)
    family_run("FindIfRelated", None, {"a": e[3], "b": e[0]})
    assert seen and seen[0][0] == 0.1 and seen[0][2] == 2
    assert sworkspace.relations == {} and scoderack.CODELETS == []


def test_estimate_askability_supergroups(monkeypatch):
    e = _elements([1, 2, 3, 4])
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})
    tosses = []
    monkeypatch.setattr(util, "toss", lambda p: tosses.append(p) or 1)
    monkeypatch.setattr(sltm, "get_real_activations_for_one_concept", lambda c: 0.5)
    assert general.estimate_askability(r, r.get_type(), e[0], e[1]) == 1
    g = Anchored.create(e[0], e[1])
    sworkspace.add_group(g)
    general.estimate_askability(r, r.get_type(), e[0], e[1])
    h = Anchored.create(g, e[2])
    sworkspace.add_group(h)
    assert general.estimate_askability(r, r.get_type(), e[0], e[1]) == 0
    assert tosses == [pytest.approx(0.5), pytest.approx(0.2)]


def test_attempt_extension_rethrows_other_errors(monkeypatch):
    e = _elements([1, 2, 3, 4])
    r = SRelation({"first": e[0], "second": e[1], "type": mapping("succ")})

    def boom(opts):
        raise Confess("boom")
    monkeypatch.setattr(sworkspace, "check_at_location", boom)
    with pytest.raises(Confess, match="boom"):
        family_run("AttemptExtensionOfRelation", None, {"core": r, "direction": DIR.RIGHT})


def test_attempt_extension_asks_with_the_exception(monkeypatch):
    e = _elements([1, 2, 3])
    r = SRelation({"first": e[1], "second": e[2], "type": mapping("succ")})
    monkeypatch.setattr(general, "estimate_askability", lambda *a: 1)
    family_run("AttemptExtensionOfRelation", None, {"core": r, "direction": DIR.RIGHT})
    (cl,) = scoderack.CODELETS
    assert cl[0] == "AskIfThisIsTheContinuation" and cl[1] == 100
    assert isinstance(cl[3]["exception"], ElementsBeyondKnownSought)
    assert cl[3]["relation"] is r
    assert cl[3]["start_position"] == 3 and cl[3]["known_term_count"] == 3
    assert [x.get_mag() for x in cl[3]["expected_object"]] == [4] or \
        cl[3]["expected_object"].get_mag() == 4

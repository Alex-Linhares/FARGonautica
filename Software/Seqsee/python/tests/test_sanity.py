"""Tests for item 046 (Global wiring): the S.pm singletons, S.pm's load order (``s.load``)
and Sanity.pm.

Mirrors lib/S.pm (``$S::ASCENDING`` … ``$S::EVEN``, ``$S::AD_HOC``, ``$S::DOUBLE`` and the
``use`` list) and lib/Sanity.pm (SanityFail and every SanityCheck variant). Golden data:
oracle/sanity.pl. Each golden scenario is rebuilt here step by step, in the same order.
"""
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sanity, sltm, sworkspace, util
from seqsee.categories.ascending import Ascending
from seqsee.errors import ExceptionClassBase
from seqsee.mapping.numeric import MappingNumeric
from seqsee.objects.anchored import Anchored
from seqsee.objects.object import SeqseeObject
from seqsee.scodelet import SCodelet
from seqsee.smetonym_type import SMetonymType
from seqsee.srelation import SRelation

CASES = {c["case"]: c for c in golden.load("sanity")}
_ADDR = re.compile(r"=(HASH|ARRAY)\(0x[0-9a-f]+\)")


def _mask(s):
    return _ADDR.sub(r"=\1", s)


def _err(e):
    if isinstance(e, ExceptionClassBase):
        return {"class": e.perl_name, "message": _mask(e.message or "")}
    return _mask(str(e))


@pytest.fixture
def messages(monkeypatch):
    got = []
    monkeypatch.setattr(sanity, "_message", got.append)
    return got


def check(messages, *args):
    messages.clear()
    try:
        sanity.sanity_check(*args)
        e = None
    except Exception as ex:  # noqa: BLE001 - compared with the oracle's error
        e = _err(ex)
    return {"error": e, "messages": [_mask(m) for m in messages]}


def expect(name, got):
    want = {k: v for k, v in CASES[name].items() if k != "case"}
    assert got == want, name


def init(*seq):
    Global.Feature.clear()
    Global.Steps_Finished = 0
    Global.CurrentRunnableString = ""
    Global.CurrentCodelet = None
    sltm.clear()
    sworkspace.init({"seq": list(seq)})
    sworkspace.clear_bar_lines()
    return sworkspace.get_elements()


def reln(f, s, typ):
    return SRelation({"first": f, "second": s, "type": MappingNumeric.create(typ, S.NUMBER)})


def with_codelet():
    Global.CurrentCodelet = SCodelet("FocusOn", 50, {})


# --- S.pm singletons ------------------------------------------------------------------------
def test_singletons():
    names = ["ASCENDING", "DESCENDING", "MOUNTAIN", "SAMENESS", "NUMBER", "PRIME", "ODD", "EVEN"]
    objs = [getattr(S, n) for n in names]
    d = S.DOUBLE
    expect("singletons", {
        "classes": [util.perl_ref(o) for o in objs],
        "names": [o.get_name() for o in objs],
        "texts": [o.as_text() for o in objs],
        "ad_hoc": 0 if S.AD_HOC is None else 1,
        "double": [util.perl_ref(d), d.get_name(), 1 if d.get_category() is S.SAMENESS else 0,
                   d.get_info_loss()["length"], d.as_text()],
        "new_is_new": 0 if Ascending() is S.ASCENDING else 1,
    })


def test_ad_hoc_is_undef():
    """PERL-QUIRK: ``$S::AD_HOC = $SCat::ad_hoc::AD_HOC`` names a package that no longer
    exists, so it is undef."""
    assert S.AD_HOC is None


def test_double_is_not_in_the_create_memo():
    assert isinstance(S.DOUBLE, SMetonymType)
    assert SMetonymType.create({"category": S.SAMENESS, "name": "each",
                                "info_loss": {"length": 2}}) is not S.DOUBLE


# --- S.pm load order --------------------------------------------------------------------------
def test_load_imports_everything_and_registers_families():
    from seqsee.codelets import family
    mods = S.load()
    assert mods == S.load()          # idempotent
    names = [m.__name__ for m in mods]
    assert names[0] == "seqsee.global_"
    assert names.index("seqsee.categories.even") < names.index("seqsee.sworkspace")
    assert names[-1] == "seqsee.sanity"
    assert len(set(names)) == len(names)
    assert sorted(family.FAMILIES) == CASES["families"]["families"]
    for sig in [(), ("Seqsee::Element",), ("Seqsee::Anchored",), ("SRelation",),
                ("Seqsee::Anchored", "SRuleApp"), ("Seqsee::Anchored", "SRuleApp", "$")]:
        assert sig in sanity.SANITY_CHECK.dispatch, sig


# --- Sanity.pm: the golden scenarios ----------------------------------------------------------
def test_clean_workspace_and_element_variant(messages):
    e = init(1, 2, 3, 4, 5, 6)
    g = Anchored.create(*e[0:3])
    g.describe_as(S.ASCENDING)
    sworkspace.add_group(g)
    r = reln(e[3], e[4], "succ")
    r.insert()
    expect("all_clean", {"groups": len(sworkspace.get_groups()), **check(messages)})
    expect("group_clean", check(messages, g))
    expect("relation_clean", check(messages, r))
    expect("element_clean", check(messages, e[0]))

    e[1].describe_as(S.ASCENDING)
    b = e[1].get_binding_for_category(S.ASCENDING).get_bindings_ref()
    b["start"] = 7
    for k in [k for k in b if k != "start"]:
        del b[k]
    expect("element_nonref", check(messages, e[1]))
    # The failed check left the bindings' each iterator at the end: this one passes.
    with_codelet()
    expect("element_nonref_codelet", check(messages, e[1]))
    Global.CurrentRunnableString = "Seqsee::SCF::FocusOn"
    Global.Steps_Finished = 17
    expect("element_nonref_steps", check(messages, e[1]))


def test_anchored_variant(messages):
    e = init(1, 2, 3, 4, 5, 6)
    with_codelet()
    g = Anchored.create(*e[0:3])
    expect("group_no_category_count", {"cats": len(g.get_categories())})
    for c in g.get_categories():
        g.remove_category(c)
    expect("group_no_category", check(messages, g))

    g.describe_as(S.ASCENDING)
    b = g.get_binding_for_category(S.ASCENDING).get_bindings_ref()
    expect("group_binding_keys", {"keys": sorted(b)})
    for k in list(b):
        b[k] = 3
    for k in [k for k in b if k != "start"]:
        del b[k]
    expect("group_nonref", check(messages, g))
    g.remove_category(S.ASCENDING)
    g.describe_as(S.ASCENDING)

    g.set_edges(0, 6)
    expect("edge_right", check(messages, g))
    g.set_edges(2, 1)
    expect("edge_order", check(messages, g))
    g.set_edges(-1, 2)
    expect("edge_left", check(messages, g))
    g.set_edges(0, 2)
    expect("edges_restored", check(messages, g))

    a = Anchored.create(e[0], e[1])
    bb = Anchored.create(e[2], e[3])
    gg = Anchored.create(a, bb)
    if not gg.get_categories():
        gg.describe_as(S.SAMENESS)
    expect("gg_cats", {"cats": [c.get_name() for c in gg.get_categories()]})
    expect("gg_clean", check(messages, gg))
    bb.set_edges(3, 3)
    expect("holes", check(messages, gg))
    bb.set_edges(2, 3)

    a.set_is_a_metonym(a)
    expect("metonym_part_self", check(messages, gg))
    a.set_is_a_metonym(None)

    gg.get_parts_ref().append(5)
    expect("unanchored_scalar", check(messages, gg))
    gg.get_parts_ref().pop()
    gg.get_parts_ref().append(SeqseeObject.create(7, 8))
    expect("unanchored_object", check(messages, gg))
    gg.get_parts_ref().pop()

    r = reln(e[0], e[1], "succ")
    r.insert()
    g2 = Anchored.create(*e[0:3])
    g2.describe_as(S.ASCENDING)
    g2.set_underlying_ruleapp(r)
    ra = g2.get_underlying_reln()
    expect("ruleapp_items", {"count": len(ra.get_items())})
    expect("ruleapp_clean", check(messages, g2))
    ra.get_items().pop()
    expect("ruleapp_out_of_sync", check(messages, g2))


def test_relation_variant_and_whole_workspace(messages):
    e = init(1, 2, 3, 4, 5, 6)
    with_codelet()
    r = reln(e[2], e[1], "pred")
    expect("relation_leftward", check(messages, r))
    r2 = reln(e[1], e[2], "succ")
    e[2].set_is_a_metonym(e[1])
    expect("relation_metonymed_end", check(messages, r2))
    e[2].set_is_a_metonym(e[2])
    expect("relation_self_metonym", check(messages, r2))
    e[2].set_is_a_metonym(None)

    r3 = reln(e[3], e[4], "succ")
    r3.insert()
    e[4].set_is_a_metonym(e[3])
    expect("all_relation_bad", check(messages))
    e[4].set_is_a_metonym(None)

    g = Anchored.create(*e[0:3])
    g.describe_as(S.ASCENDING)
    sworkspace.add_group(g)
    expect("all_clean2", check(messages))
    g.set_edges(0, 9)
    expect("all_group_bad", check(messages))


@pytest.mark.parametrize("kind,arg", [("number", 3), ("string", "x"), ("undef", None)])
def test_dispatch_on_scalars(messages, kind, arg):
    init(1, 2, 3)
    expect(f"dispatch_{kind}", check(messages, arg))


# --- from reading the source ---------------------------------------------------------------
def test_unanchored_part_check_is_unreachable(messages):
    """PERL-QUIRK: are_there_holes_here throws on a non-anchored part before the
    "Unanchored part!" check can run (see unanchored_* above)."""
    e = init(1, 2, 3)
    with_codelet()
    g = Anchored.create(e[0], e[1])
    g.describe_as(S.ASCENDING)
    g.get_parts_ref().append("x")
    got = check(messages, g)
    assert got["error"]["class"] == "SErr"
    assert "Unanchored" not in str(got)


def test_group_history_lists_subgroups(messages):
    e = init(1, 2, 3, 4)
    with_codelet()
    a = Anchored.create(e[0], e[1])
    a.add_history("note")
    g = Anchored.create(a, Anchored.create(e[2], e[3]))
    for c in g.get_categories():
        g.remove_category(c)
    got = check(messages, g)["error"]
    assert "Group without any category:" in got
    assert "-------- " + a.as_text() + "\n" in got
    assert "\tnote" in got


def test_sanity_fail_without_codelet_has_no_message(messages):
    init(1, 2)
    with pytest.raises(Exception, match='"as_text" on an undefined value'):
        sanity.sanity_fail("x")
    assert messages == []

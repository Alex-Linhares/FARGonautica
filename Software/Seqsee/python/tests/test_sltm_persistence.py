"""Tests for SLTM, part II: init, Dump, Load/Load_Helper, FindActiveFollowers,
FindActiveCategories, LogActivations/PrintNode and Print.

Mirrors Perl ``lib/SLTM.pm`` (golden: sltm_persistence, from ``oracle/sltm_persistence.pl``).
The scenarios below rebuild the oracle's, step by step. Link order in dumped files and in
Set::Weighted results follows Perl hash order, so those are compared sorted.
"""
import io
import os
import re

import pytest

import golden
from seqsee import global_ as Global
from seqsee import s as S
from seqsee import sltm
from seqsee import slink_activation as sla
from seqsee.constants import METO_MODE, POS_MODE
from seqsee.errors import Confess, LTM_LoadFailure
from seqsee.mapping.numeric import MappingNumeric
from seqsee.sbindings import SBindings
from seqsee.sint import SInt
from seqsee.sltm_platonic import SLTMPlatonic
from test_sltm_core import same

CASES = golden.load("sltm_persistence")


def cases(op, **match):
    found = [c for c in CASES if c["op"] == op and all(c.get(k) == v for k, v in match.items())]
    assert found, (op, match)
    return found


def one(op, **match):
    (case,) = cases(op, **match)
    return case


@pytest.fixture
def d(tmp_path):
    """Files live in tmp_path; recorded paths say DIR instead."""
    class Dir:
        root = str(tmp_path)

        def path(self, name):
            return f"{self.root}/{name}"

        def rel(self, text):
            return text.replace(self.root, "DIR")

        def read(self, name):
            with open(self.path(name), encoding="latin-1", newline="") as fh:
                return fh.read()

        def write(self, name, text):
            with open(self.path(name), "w", encoding="latin-1", newline="") as fh:
                fh.write(text)

    return Dir()


def norm(msg):
    """The oracle's normalization: object addresses are removed."""
    return re.sub(r"=(HASH|ARRAY|SCALAR)\(0x[0-9a-f]+\)", "", msg)


def error_text(e):
    return norm(e.what if isinstance(e, LTM_LoadFailure) else str(e))


def P(s):
    return SLTMPlatonic.create(str(s) if not isinstance(s, str) else s)


def state():
    links = []
    for frm in range(1, len(sltm.OUT_LINKS)):
        lr = sltm.OUT_LINKS[frm]
        for typ, h in enumerate(lr):
            if not h:
                continue
            for to in sorted(h):
                links.append([frm, typ, to, list(h[to])])
    return {
        "node_count": sltm.NodeCount,
        "nodes": [[n.perl_name if hasattr(n, "perl_name") else type(n).__name__, n.as_text()]
                  for n in sltm.MEMORY[1:]],
        "activations": [list(a) for a in sltm.ACTIVATIONS[1:]],
        "links": links,
        "link_count": len(sltm.LINKS),
    }


def dumped(text):
    nodes, _, links = text.partition("#####\n")
    return {"nodes": nodes, "links": sorted(x for x in links.split("\n") if x), "whole": text}


def same_dump(got_text, want):
    got = dumped(got_text)
    assert got["nodes"] == want["nodes"]
    assert got["links"] == want["links"]


def build_small():
    sltm.clear()
    sltm.get_memory_index(S.NUMBER)
    p1, p2 = P(1), P("[1,2]")
    P("[[1,2],3]")
    sltm.insert_isa_link(p1, p2)
    sltm.insert_follows_link(S.ASCENDING, MappingNumeric.create("succ", S.NUMBER))
    sltm.get_memory_index(METO_MODE.ALL)
    sltm.get_memory_index(POS_MODE.FORWARD)
    sltm.insert_link_unless_present(3, 6, 7, sltm.LTM_CAN_BE_SEEN_AS)
    sltm.insert_link_unless_present(3, 2, 0, sltm.LTM_IS)
    sltm.insert_link_unless_present(2, 7, 0, sltm.LTM_FOLLOWS)
    link = sltm.insert_link_unless_present(7, 1, 2, sltm.LTM_IS)
    link[sla.RAW_SIGNIFICANCE] = 123.456789
    link[sla.STABILITY_RECIPROCAL] = 0.000049
    sltm.strengthen_link_given_nodes(p1, p2, sltm.LTM_IS, 30)
    sltm.spike_by(30, p1)
    sltm.set_depth_reciprocal_for_index(3, 0.5)
    sltm.set_depth_reciprocal_for_index(4, "7")


# ---- init ----

def test_init(capsys):
    sltm.init()
    assert capsys.readouterr().out == one("init")["out"]


def test_init_with_activation_log(d, capsys, monkeypatch):
    c = one("init_log")
    monkeypatch.setitem(Global.Feature, "LogActivations", 1)
    monkeypatch.setattr(Global, "ActivationsLogfile", d.path("act.log"))
    monkeypatch.setattr(Global, "ActivationsLogHandle", None)
    sltm.init()
    assert d.rel(capsys.readouterr().out) == c["out"]
    handle = Global.ActivationsLogHandle
    assert bool(handle) == bool(c["opened"])
    assert os.path.exists(d.path("act.log")) == bool(c["exists"])
    sltm.init()
    assert d.rel(capsys.readouterr().out) == c["again"]
    assert Global.ActivationsLogHandle is handle
    handle.close()


# ---- Dump ----

def test_dump_empty(d, capsys):
    c = one("dump_empty")
    sltm.dump(d.path("empty.dat"))
    assert d.rel(capsys.readouterr().out) == c["out"]
    assert d.read("empty.dat") == c["file"]


def test_dump_small(d, capsys):
    c = one("dump_small")
    build_small()
    same(state(), c["state"])
    sltm.dump(d.path("small.dat"))
    assert d.rel(capsys.readouterr().out) == c["out"]
    same_dump(d.read("small.dat"), c["file"])


def test_dump_to_file_object(d):
    c = one("dump_file_temp")
    build_small()
    fh = open(d.path("tmp.dat"), "w", encoding="latin-1", newline="")
    sltm.dump(fh)
    assert fh.closed
    same_dump(d.read("tmp.dat"), c["file"])


@pytest.mark.parametrize("op,arg", [("dump_bad_ref", []), ("dump_bad_blessed", object())])
def test_dump_bad_reference(op, arg):
    c = one(op)
    with pytest.raises(Confess) as e:
        sltm.dump(arg)
    assert str(e.value) == c["error"]


def test_dump_odd_values(d):
    c = one("dump_odd")
    sltm.insert_isa_link(P(5), P(6))
    sltm.set_depth_reciprocal_for_index(1, None)
    sltm.set_depth_reciprocal_for_index(2, "abc")
    link = sltm.insert_link_unless_present(1, 2, 0, sltm.LTM_IS)
    link[sla.RAW_SIGNIFICANCE] = -12345.6789
    link[sla.STABILITY_RECIPROCAL] = 99999.123456
    sltm.dump(d.path("odd.dat"))
    same_dump(d.read("odd.dat"), c["file"])


@pytest.mark.parametrize("c", cases("dump_number"), ids=lambda c: f"{c['sig']}-{c['stab']}")
def test_dump_number_formats(d, c):
    sltm.insert_isa_link(P(1), P(2))
    link = sltm.insert_link_unless_present(1, 2, 0, sltm.LTM_IS)
    link[sla.RAW_SIGNIFICANCE] = c["sig"]
    link[sla.STABILITY_RECIPROCAL] = c["stab"]
    if c["undef_modifier"]:
        link[sla.MODIFIER_NODE_INDEX] = None
    sltm.dump(d.path("n.dat"))
    assert d.read("n.dat") == c["file"]


# ---- Load_Helper ----

def test_load_round_trip(d, capsys):
    c = one("load_round_trip")
    p1, p2, p3 = P(1), P("[1,2]"), P("[[1,2],3]")
    sltm.insert_isa_link(p1, p2)
    sltm.insert_follows_link(S.ASCENDING, p3)
    sltm.get_memory_index(METO_MODE.ALLBUTONE)
    sltm.get_memory_index(POS_MODE.BACKWARD)
    sltm.insert_link_unless_present(3, 5, 6, sltm.LTM_CAN_BE_SEEN_AS)
    sltm.strengthen_link_given_nodes(p1, p2, sltm.LTM_IS, 30)
    link = sltm.insert_link_unless_present(5, 1, 2, sltm.LTM_IS)
    link[sla.RAW_SIGNIFICANCE] = 42.123456
    link[sla.STABILITY_RECIPROCAL] = 0.123456
    sltm.set_depth_reciprocal_for_index(2, 0.75)
    sltm.spike_by(20, p3)
    sltm.dump(d.path("rt.dat"))
    text = d.read("rt.dat")
    assert dumped(text)["nodes"] == dumped(c["file_text"])["nodes"]
    assert dumped(text)["links"] == dumped(c["file_text"])["links"]
    capsys.readouterr()
    sltm.load_helper(d.path("rt.dat"))
    assert d.rel(capsys.readouterr().out) == c["out"]
    same(state(), c["state"])
    assert (sltm.MEMORY[1] is p1) == bool(c["same_platonic"])
    sltm.dump(d.path("rt2.dat"))
    same_dump(d.read("rt2.dat"), c["redump"])


def test_load_small_mapping_memo_collision(d):
    """PERL-QUIRK: SCategory::Number->new() makes a new Number, and Mapping::Numeric's
    create memo (keyed by the category's memory index) hands back the old mapping, whose
    category is not in the LTM any more; its insertion adds a node, so Load fails."""
    c = one("load_small")
    build_small()
    sltm.dump(d.path("small.dat"))
    with pytest.raises(LTM_LoadFailure) as e:
        sltm.load_helper(d.path("small.dat"))
    assert c["died"] == 1
    assert error_text(e.value) == c["error"]
    assert sltm.NodeCount == c["node_count"]


@pytest.mark.parametrize("c", cases("load_file"), ids=lambda c: c["name"])
def test_load_file(d, capsys, c):
    d.write(f"{c['name']}.dat", c["text"])
    try:
        sltm.load_helper(d.path(f"{c['name']}.dat"))
        died, err = 0, ""
    except (LTM_LoadFailure, Confess) as e:
        died, err = 1, error_text(e)
    assert d.rel(capsys.readouterr().out) == c["out"]
    assert (died, err) == (c["died"], c["error"])
    same(state(), c["state"])


def test_load_missing_file(d):
    c = one("load_missing")
    with pytest.raises(Confess) as e:
        sltm.load_helper(d.path("missing.dat"))
    assert d.rel(str(e.value)) == c["error"]


# ---- Load ----

def test_load_wrapper_dies_even_on_success(d, capsys):
    """PERL-QUIRK: with no error, ``Exception::Class->caught()`` returns "" and Load dies
    with it ("Died")."""
    c = one("load_wrapper_ok")
    d.write("ok.dat", "=== 1: SLTM::Platonic 0.2\n1\n#####\n")
    with pytest.raises(Confess) as e:
        sltm.load(d.path("ok.dat"))
    assert str(e.value) == c["error"]
    captured = capsys.readouterr()
    assert d.rel(captured.out) == c["out"]
    assert captured.err == ""
    assert sltm.NodeCount == c["node_count"]


def test_load_wrapper_failure_warns_and_exits(d, capsys):
    c = one("load_wrapper_failure")
    d.write("bad_pos.dat", "=== 1: SLTM::Platonic 0.2\n1\n=== 2: POS_MODE 0.2\nBOGUS\n")
    with pytest.raises(SystemExit) as e:
        sltm.load(d.path("bad_pos.dat"))
    assert e.value.code in (None, 0)
    captured = capsys.readouterr()
    assert d.rel(captured.out) == c["out"]
    assert captured.err.split("\n")[0] == c["warn_first_lines"][0]
    assert sltm.NodeCount == c["node_count"]


def test_load_wrapper_rethrows_other_errors(d, capsys):
    c = one("load_wrapper_other")
    with pytest.raises(Confess) as e:
        sltm.load(d.path("missing.dat"))
    assert d.rel(str(e.value)) == c["error"]
    assert capsys.readouterr().err == ""


# ---- FindActiveFollowers ----

def weighted(ws):
    return sorted([[k.as_text(), w] for k, w in ws])


def bind(obj, cat):
    obj.add_category(cat, SBindings.create({}, {}, obj))


def test_find_active_followers():
    succ = MappingNumeric.create("succ", S.NUMBER)
    pred = MappingNumeric.create("pred", S.NUMBER)
    same_ = MappingNumeric.create("same", S.NUMBER)
    odd_succ = MappingNumeric.create("succ", S.ODD)
    sltm.insert_follows_link(S.NUMBER, succ)
    sltm.insert_follows_link(S.NUMBER, pred)
    sltm.insert_follows_link(S.ODD, odd_succ)
    sltm.insert_isa_link(S.NUMBER, same_)

    three = SInt(3)
    sltm.find_active_followers(three)
    assert one("followers_none")["died"] == 0
    c = one("followers_no_categories")
    res = sltm.find_active_followers(three)
    assert weighted(res) == c["result"]
    assert state()["nodes"] == c["nodes"]

    bind(three, S.NUMBER)
    c = one("followers_number")
    res = sltm.find_active_followers(three)
    assert weighted(res) == c["result"]
    assert res.is_not_empty() == c["is_not_empty"]

    bind(three, S.ODD)
    bind(three, S.PRIME)
    c = one("followers_three_cats")
    res = sltm.find_active_followers(three)
    assert weighted(res) == c["result"]
    same(state(), c["state"])

    zero = SInt(0)
    bind(zero, S.NUMBER)
    sltm.insert_follows_link(S.NUMBER, MappingNumeric.create("succ", S.NUMBER))
    assert weighted(sltm.find_active_followers(zero)) == one("followers_zero")["result"]


def test_set_weighted_sint_keys():
    """Set::Weighted (Set/Weighted.pm) on SInt keys: SInt overloads "" (merge_keys keys
    by "SInt(n)") and ne (delete_key compares magnitudes)."""
    from seqsee.set.weighted import SetWeighted
    c = one("weighted_sint")

    def txt(ws):
        pairs = sorted(ws, key=lambda p: p[0].as_text() if hasattr(p[0], "as_text") else p[0])
        return ",".join(f"{p[0].as_text() if hasattr(p[0], 'as_text') else p[0]}:{p[1]}" for p in pairs)

    s = SetWeighted([SInt(4), 1], [SInt(4), 2], ["x", 1], [SInt(5), 1], ["SInt(5)", 4])
    s.merge_keys()
    assert txt(s) == c["merged"]
    s = SetWeighted([SInt(4), 1], [SInt(4), 2], ["x", 1], [SInt(5), 1])
    s.delete_key(4)
    assert txt(s) == c["delete_number"]
    s = SetWeighted([SInt(4), 1], ["4", 1], ["x", 1])
    s.delete_key(SInt(4))
    assert txt(s) == c["delete_sint"]


# ---- FindActiveCategories ----

def test_find_active_categories():
    three = SInt(3)
    pure = three.get_pure()
    c = one("categories_none")
    res = sltm.find_active_categories(three)
    assert weighted(res) == c["result"]
    assert state()["nodes"] == c["nodes"]

    sltm.insert_isa_link(pure, S.ODD)
    sltm.insert_isa_link(pure, S.PRIME)
    sltm.insert_follows_link(pure, S.NUMBER)
    sltm.spike_by(40, S.PRIME)
    same(weighted(sltm.find_active_categories(three)), one("categories_some")["result"])

    bind(three, S.ODD)
    c = one("categories_with_current")
    assert c["died"] == 0
    same(weighted(sltm.find_active_categories(three)), c["result"])


# ---- LogActivations / PrintNode ----

def test_log_activations(monkeypatch):
    c = one("log_activations")
    buf = io.StringIO()
    monkeypatch.setattr(Global, "ActivationsLogHandle", buf)
    monkeypatch.setattr(Global, "Steps_Finished", 17)
    p = [P(i) for i in range(1, 5)]
    for x in p:
        sltm.get_memory_index(x)
    sltm.spike_by(10, p[1])
    sltm.spike_by(50, p[3])
    sltm.log_activations()
    monkeypatch.setattr(Global, "Steps_Finished", 18)
    sltm.spike_by(30, p[0])
    sltm.log_activations()
    sltm.print_node(99, "hello")
    monkeypatch.setattr(Global, "Steps_Finished", 19)
    sltm.log_activations()
    assert buf.getvalue() == c["log"]
    same(state(), c["state"])


def test_log_activations_remembers_printed_nodes_across_clear(monkeypatch):
    """%NodesAlreadyPrinted survives Clear (only sltm.reset() empties it)."""
    buf = io.StringIO()
    monkeypatch.setattr(Global, "ActivationsLogHandle", buf)
    sltm.spike_by(50, P(1))
    sltm.log_activations()
    sltm.clear()
    sltm.spike_by(50, P(2))
    sltm.log_activations()
    assert buf.getvalue().count("NewNode") == 1
    sltm.reset()
    sltm.spike_by(50, P(2))
    sltm.log_activations()
    assert buf.getvalue().count("NewNode") == 2


# ---- Print ----

def test_print_small(capsys):
    c = one("print_small")
    build_small()
    sltm.print_ltm()
    assert capsys.readouterr().out == c["out"]


def test_print_empty(capsys):
    sltm.print_ltm()
    assert capsys.readouterr().out == one("print_empty")["out"]

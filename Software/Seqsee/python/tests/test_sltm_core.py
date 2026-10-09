"""Tests for SLTM, part I: nodes, links, activations, spreading, SpikeAndChoose, DecayAll and
the Get*/Set*/Choose* helpers.

Mirrors Perl ``lib/SLTM.pm`` (golden: sltm_core, from ``oracle/sltm_core.pl``). Persistence
(Dump/Load), FindActive*, Print and LogActivations are item 030.
"""
import pytest

import golden
from seqsee import ltmstorable, sltm, util
from seqsee import slink_activation as sla
from seqsee import snode_activation as sna
from seqsee.errors import Confess
from seqsee.sltm_platonic import SLTMPlatonic

CASES = golden.load("sltm_core")


def cases(op):
    found = [c for c in CASES if c["op"] == op]
    assert found, op
    return found


def one(op):
    (case,) = cases(op)
    return case


def same(got, want):
    """Compare Perl values: numbers approximately (JSON::PP prints 15 digits), the rest exactly."""
    if isinstance(want, list):
        assert isinstance(got, (list, tuple)) and len(got) == len(want), (got, want)
        for g, w in zip(got, want):
            same(g, w)
    elif isinstance(want, dict):
        assert isinstance(got, dict) and sorted(got) == sorted(want), (got, want)
        for k in want:
            same(got[k], want[k])
    elif isinstance(got, str) and isinstance(want, (int, float)) and not isinstance(want, bool):
        # JSON::PP prints a string as a number once Perl has used it numerically.
        same(util.perl_num(got), want)
    elif isinstance(want, float) or (isinstance(want, int) and isinstance(got, float)):
        assert got == pytest.approx(want, rel=1e-13, abs=1e-15), (got, want)
    else:
        assert got == want, (got, want)


def P(s):
    return SLTMPlatonic.create(str(s))


def state():
    """The Python counterpart of the oracle's ``state()``."""
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
        "nodes": [n.as_text() for n in sltm.MEMORY[1:]],
        "activations": [list(a) for a in sltm.ACTIVATIONS],
        "links": links,
        "link_count": len(sltm.LINKS),
        "out_links_shape": [len(x) if isinstance(x, list) else x for x in sltm.OUT_LINKS],
    }


def text_or_none(x):
    return None if x is None else x.as_text()


# ---- constants, Clear, InsertNode ----

def test_constants():
    c = one("constants")
    assert (sltm.LTM_FOLLOWS, sltm.LTM_IS, sltm.LTM_CAN_BE_SEEN_AS, sltm.LTM_TYPE_COUNT) == (
        c["LTM_FOLLOWS"], c["LTM_IS"], c["LTM_CAN_BE_SEEN_AS"], c["LTM_TYPE_COUNT"])
    assert {str(k): v for k, v in sltm.LinkType2Str.items()} == c["link_type_2_str"]


def test_clear():
    sltm.get_memory_index(P(1))
    sltm.insert_isa_link(P(1), P(2))
    sltm.clear()
    same(state(), one("clear")["state"])
    assert isinstance(sltm.ACTIVATIONS[0], sna.SNodeActivation)


def test_clear_keeps_list_identity():
    acts, links, out = sltm.ACTIVATIONS, sltm.LINKS, sltm.OUT_LINKS
    sltm.clear()
    assert sltm.ACTIVATIONS is acts and sltm.LINKS is links and sltm.OUT_LINKS is out


def test_insert_nodes():
    c = one("insert_nodes")
    assert [sltm.get_memory_index(P(i)) for i in range(1, 5)] == c["indices"]
    assert sltm.get_memory_index(P(2)) == c["again"]
    same(state(), c["state"])


# ---- links ----

def test_insert_links():
    c = one("insert_links")
    l1 = sltm.insert_isa_link(P(1), P(2))
    l2 = sltm.insert_isa_link(P(1), P(2))
    l3 = sltm.insert_follows_link(P(1), P(3))
    l4 = sltm.insert_isa_link(P(3), P(1))
    l5 = sltm.insert_link_unless_present(2, 3, 1, 3)
    l6 = sltm.insert_link_unless_present(2, 3, 2, 3)
    assert int(l1 is l2) == c["same"]
    assert int(l5 is l6) == c["same_modifier_ignored"]
    assert int(l1 is not l3 and l3 is not l4) == c["distinct"]
    assert isinstance(l1, sla.SLinkActivation)
    same(list(l1), c["new_link"])
    same(list(l3), c["follows"])
    same(list(l5), c["modifier_link"])
    same(state(), c["state"])


def test_strengthen():
    c = one("strengthen")
    sltm.insert_isa_link(P(1), P(2))
    got = []
    for amt, _ in c["results"][:-1]:
        got.append([amt, sltm.strengthen_link_given_nodes(P(1), P(2), sltm.LTM_IS, amt)])
    got.append(["index", sltm.strengthen_link_given_index(1, 2, 2, 7)])
    same(got, c["results"])
    # Perl: the Smart::Comments ``### require:`` fails and dies "\n".
    assert c["missing_type_error"] == "\n" and c["missing_link_error"] == "\n"
    with pytest.raises(Confess, match="require"):
        sltm.strengthen_link_given_index(1, 2, 1, 7)
    with pytest.raises(Confess, match="require"):
        sltm.strengthen_link_given_nodes(P(2), P(1), 2, 7)
    same(state(), c["state"])
    # The failed lookups autovivified empty hashes.
    assert sltm.OUT_LINKS[1][1] == {} and sltm.OUT_LINKS[2][2] == {}


# ---- SpikeBy / WeakenBy ----

def test_spike_weaken():
    c = one("spike_weaken")
    got = [
        ["spike", 10, sltm.spike_by(10, P(1), P(2))],
        ["spike", 0, sltm.spike_by(0, P(1))],
        ["spike", None, sltm.spike_by(None, P(2))],
        ["spike_strings_skipped", 5, sltm.spike_by(5, "abc", P(3), None, 7)],
        ["spike_twice", 20, sltm.spike_by(20, P(3), P(3))],
        ["spike_big", 400, sltm.spike_by(400, P(4))],
        ["weaken", 3, sltm.weaken_by(3, P(1), P(3))],
        ["weaken", 0, sltm.weaken_by(0, P(2))],
        ["weaken_big", 1000, sltm.weaken_by(1000, P(4))],
    ]
    same(got, c["results"])
    for args, key in (((5,), "no_concepts_error"), ((5, "x", None), "only_strings_error")):
        with pytest.raises(Confess) as e:
            sltm.spike_by(*args)
        assert str(e.value) == c[key]
    with pytest.raises(Confess) as e:
        sltm.weaken_by(5)
    assert str(e.value) == c["weaken_none_error"]
    same(state(), c["state"])


def test_ltmstorable_hooks_reach_sltm():
    from seqsee import s as S
    got = S.ODD.spike_by(10)
    assert got == sltm.get_real_activations_for_one_concept(S.ODD)
    link = S.ODD.insert_isa_link(S.EVEN)
    assert link is sltm.insert_isa_link(S.ODD, S.EVEN)
    assert ltmstorable._sltm_spike_by(3, S.ODD) == sltm.get_real_activations_for_one_concept(S.ODD)


# ---- SpikeAndChoose ----

def test_spike_and_choose():
    c = one("spike_and_choose")
    util.srand(42)
    got = [["empty", [] if sltm.spike_and_choose(10) is None else ["?"]]]
    with pytest.raises(Confess) as e:
        sltm.spike_and_choose(10, P(1), None)
    assert c["undef_error"] == str(e.value)
    for k in range(1, 41):
        concepts = [P(i) for i in range(1, 6) if (k + i) % 3]
        amt = (k * 7) % 30
        chosen = sltm.spike_and_choose(amt, *concepts)
        got.append([k, amt, [x.as_text() for x in concepts],
                    [] if chosen is None else [chosen.as_text()]])
    got.append(["after", util.rand()])
    same(got, c["results"])
    same(state(), c["state"])


def test_spike_and_choose_low_activations():
    c = one("spike_and_choose_low")
    sltm.get_memory_index(P(1))
    sltm.get_memory_index(P(2))
    for i in (1, 2):
        sltm.ACTIVATIONS[i][0] = -150
    util.srand(5)
    chosen = sltm.spike_and_choose(1, P(1), P(2))
    assert [text_or_none(chosen)] == c["got"]
    same(util.rand(), c["next_rand"])
    same(state(), c["state"])
    c2 = one("spike_and_choose_undef_activation")
    sltm.ACTIVATIONS[1][0] = -300
    util.srand(5)
    chosen = sltm.spike_and_choose(1, P(1), P(2))
    assert [text_or_none(chosen)] == c2["got"]
    same(util.rand(), c2["next_rand"])
    same(state(), c2["state"])


def test_spike_and_choose_all_below_threshold_draws_nothing():
    sltm.get_memory_index(P(1))
    util.srand(9)
    assert sltm.spike_and_choose(1, P(1)) is None   # 2.2 raw -> 0.003 <= 0.02
    util.srand(9)
    first = util.rand()
    util.srand(9)
    sltm.spike_and_choose(1, P(1))
    assert util.rand() == first


# ---- SpreadActivationFrom ----

def build_graph(links, acts):
    sltm.clear()
    for i in range(1, 9):
        sltm.get_memory_index(P(i))
    for spec in links:
        frm, to, mod, typ, *rest = spec
        link = sltm.insert_link_unless_present(frm, to, mod, typ)
        if len(rest) > 0 and rest[0] is not None:
            link[1] = rest[0]
        if len(rest) > 1 and rest[1] is not None:
            link[2] = rest[1]
    for i, raw in acts or []:
        sltm.ACTIVATIONS[i][0] = raw
        sltm.ACTIVATIONS[i][2] = sla.PRECALCULATED[raw]


@pytest.mark.parametrize("case", cases("spread"), ids=lambda c: c["name"])
def test_spread(case, monkeypatch):
    build_graph(case["links"], case["acts"])
    same(state(), case["before"])
    debug = []
    monkeypatch.setattr(sltm, "_debug_message", lambda *args: debug.append(list(args)))
    assert case["error"] is None
    sltm.spread_activation_from(case["root"])
    same(state(), case["after"])
    assert sorted(debug, key=lambda d: d[0]) == case["debug"]


def test_spread_missing_root():
    with pytest.raises(Confess) as e:
        sltm.spread_activation_from(3)
    assert str(e.value) == one("spread_missing_root")["error"]


# ---- DecayAll ----

def test_decay_all():
    c = one("decay_all")
    build_graph([[1, 2, 0, 2, 3], [2, 3, 0, 1], [3, 4, 0, 3, 1.5, 0.5]], [[1, 60], [2, 3], [3, 30]])
    sltm.ACTIVATIONS[0][0] = 40
    for want in c["states"]:
        sltm.decay_all()
        same(state(), want)
    sltm.clear()
    assert [sltm.decay_all()] == ["" if x is False else x for x in c["empty_return"]]
    same(state(), c["empty_state"])


def test_decay_all_vivifies_holes():
    # Oracle-checked by hand: holes left by out-of-range getters/setters become
    # [2, undef, PRECALCULATED[2]]; a vivified [5] becomes [5, undef, PRECALCULATED[5]].
    sltm.get_memory_index(P(1))
    sltm.get_raw_activations_for_indices([4])
    assert len(sltm.ACTIVATIONS) == 5 and sltm.ACTIVATIONS[2] is None
    sltm.decay_all()
    for i in (2, 3, 4):
        assert sltm.ACTIVATIONS[i] == [2, None, sla.PRECALCULATED[2]]
    assert sltm.get_real_activations_for_indices([7]) == [None]
    assert len(sltm.ACTIVATIONS) == 8
    sltm.set_raw_activation_for_index(9, 5)
    assert sltm.ACTIVATIONS[5:] == [None, None, [], None, [5]]
    sltm.decay_all()
    assert sltm.ACTIVATIONS[9] == [5, None, sla.PRECALCULATED[5]]


# ---- getters, setters, GetTopConcepts ----

def test_getters():
    c = one("getters")
    build_graph([], [[1, 60], [2, 3], [3, 30]])
    same(sltm.get_raw_activations_for_indices([1, 2, 3, 3, 0]), c["raw_for_indices"])
    same(sltm.get_real_activations_for_indices([3, 1]), c["real_for_indices"])
    same(sltm.get_real_activations_for_concepts([P(1), P(3)]), c["real_for_concepts"])
    same(sltm.get_real_activations_for_one_concept(P(2)), c["real_for_one"])
    same(sltm.get_real_activations_for_one_concept(P(99)), c["real_for_new_concept"])
    assert sltm.get_raw_activations_for_indices([]) == c["empty"]
    same([[x.as_text(), real, raw] for x, real, raw in sltm.get_top_concepts(3)], c["top"])
    same(state(), c["state"])
    c2 = one("getter_out_of_range")
    assert sltm.get_raw_activations_for_indices([20]) == c2["result"]
    assert len(sltm.ACTIVATIONS) == c2["activations_len"]
    assert sltm.NodeCount == c2["node_count"]


def test_setters():
    c = one("setters")
    build_graph([], [])
    sltm.set_significance_and_stability_for_index(1, 23, 0.5)
    sltm.set_significance_and_stability_for_index(2, -7, 0.25)
    sltm.set_depth_reciprocal_for_index(3, 0.125)
    sltm.set_depth_reciprocal_for_index(4, "0.5")
    sltm.set_raw_activation_for_index(5, 77)
    returns = [sltm.set_raw_activation_for_index(6, 12), sltm.set_depth_reciprocal_for_index(7, 0.3),
               sltm.set_significance_and_stability_for_index(8, 12, 0.1)]
    same(returns, c["returns"])
    same(state(), c["state"])
    sltm.spike_by(10, P(3), P(4), P(5))
    same(state(), one("setters_then_spike")["state"])
    sltm.clear()
    assert sltm.get_top_concepts(5) == one("top_empty")["top"]


# ---- Choose* ----

def test_choose():
    c = one("choose")
    build_graph([], [[1, 60], [2, 3], [3, 30], [4, 95]])
    util.srand(7)
    got = []
    for _ in range(25):
        got.append([
            sltm.choose_index_given_index([1, 2, 3, 4]),
            sltm.choose_concept_given_index([2, 3]).as_text(),
            sltm.choose_index_given_concept([P(1), P(4)]),
            sltm.choose_concept_given_concept([P(3), P(2), P(1)]).as_text(),
        ])
    assert got == c["results"]
    assert sltm.choose_index_given_index([]) is None
    assert sltm.choose_concept_given_concept([]) is None
    assert c["empty"] == []
    assert sltm.choose_concept_given_concept([P(50), P(51)]).as_text() == c["unknown"]
    assert sltm.NodeCount == c["unknown_inserted"]
    same(util.rand(), c["next_rand"])


# ---- a seeded random walk ----

def test_random_walk():
    c = one("random_walk")
    util.srand(2024)
    concepts = [P(i) for i in range(1, 8)]
    log = []
    for step in range(1, 301):
        op = int(util.rand(7))
        a = concepts[int(util.rand(7))]
        b = concepts[int(util.rand(7))]
        amt = int(util.rand(40))
        if op == 0:
            sltm.insert_isa_link(a, b)
            res = "isa"
        elif op == 1:
            sltm.insert_follows_link(a, b)
            res = "follows"
        elif op == 2:
            res = sltm.spike_by(amt, a, b)
        elif op == 3:
            res = text_or_none(sltm.spike_and_choose(amt, a, b))
        elif op == 4:
            sltm.decay_all()
            res = "decay"
        elif op == 5:
            i = sltm.get_memory_index(a)
            t = (amt % 2) + 1
            out = sltm.OUT_LINKS[i]
            h = out[t] if t < len(out) else None
            if h:
                res = sltm.strengthen_link_given_index(i, min(h), t, amt * 5)
            else:
                res = "nolink"
        else:
            sltm.spread_activation_from(sltm.get_memory_index(a))
            res = "spread"
        log.append([step, op, res])
        same(log[-1], c["log"][step - 1])
    same(state(), c["state"])


# ---- hooks in other modules now reach SLTM ----

def test_other_hooks_wired():
    from seqsee import mapping, smetonym_type, srelation, srule_app
    from seqsee.categories import alternating, mapping_based
    from seqsee.objects import object as object_mod
    for mod in (alternating, mapping_based, smetonym_type):
        sltm.get_memory_index(P(7))
        assert mod._sltm_encode(P(7), "x") == sltm.encode(P(7), "x")
        assert mod._sltm_decode(sltm.encode(P(7)))[0] is P(7)
    assert object_mod._get_real_activations_for_concepts([P(7)]) == \
        sltm.get_real_activations_for_concepts([P(7)])
    assert srelation._get_real_activations_for_one_concept(P(7)) == \
        sltm.get_real_activations_for_one_concept(P(7))
    before = sltm.ACTIVATIONS[sltm.get_memory_index(P(7))][0]
    srule_app._sltm_spike_by(10, P(7))
    assert sltm.ACTIVATIONS[sltm.get_memory_index(P(7))][0] == pytest.approx(before + 2)
    util.srand(1)
    assert mapping._spike_and_choose(50, P(7)) is P(7)

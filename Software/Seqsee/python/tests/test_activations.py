"""Tests for the activation objects.

Mirrors Perl ``lib/SNodeActivation.pm`` and ``lib/SLinkActivation.pm``
(golden: activations, from ``oracle/activations.pl``).
"""
import math

import pytest

import golden
from seqsee import sltm, util
from seqsee import slink_activation as sla
from seqsee import snode_activation as sna
from seqsee.errors import Confess
from seqsee.slink_activation import SLinkActivation
from seqsee.snode_activation import SNodeActivation

CASES = golden.load("activations")


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
        assert isinstance(got, list) and len(got) == len(want), (got, want)
        for g, w in zip(got, want):
            same(g, w)
    elif isinstance(want, float) or (isinstance(want, int) and isinstance(got, float)):
        assert got == pytest.approx(want, rel=1e-13, abs=1e-15), (got, want)
    else:
        assert got == want, (got, want)


def state(act):
    return list(act)


# ---- constants and the table ----

def test_precalculated_table():
    want = one("precalculated")["values"]
    assert len(sla.PRECALCULATED) == 201
    same(sla.PRECALCULATED, want)


def test_link_constants():
    c = one("link_constants")
    for name in ("RAW_ACTIVATION", "RAW_SIGNIFICANCE", "STABILITY_RECIPROCAL", "REAL_ACTIVATION",
                 "MODIFIER_NODE_INDEX", "Initial_Raw_Activation", "Initial_Raw_Significance",
                 "Initial_Stability", "Initial_Stability_Reciprocal"):
        same(getattr(sla, name), c[name])
        if name.isupper():
            assert getattr(SLinkActivation, name) == c[name]


def test_node_constants():
    c = one("node_constants")
    for name in ("RAW_ACTIVATION", "DEPTH_RECIPROCAL", "REAL_ACTIVATION", "Initial_Raw_Activation",
                 "Initial_Depth", "Initial_Depth_Reciprocal"):
        same(getattr(sna, name), c[name])
    for name in ("RAW_ACTIVATION", "DEPTH_RECIPROCAL", "REAL_ACTIVATION"):
        assert getattr(SNodeActivation, name) == c[name]


def test_load_order_quirk_is_documented():
    # Perl confesses "Load order issues" if SNodeActivation is loaded before SLinkActivation.
    # Python imports slink_activation from snode_activation, so the order can't go wrong.
    assert one("load_order") == {"op": "load_order", "loaded": 0, "error": "Load order issues"}
    assert sna.PRECALCULATED is sla.PRECALCULATED


# ---- SNodeActivation ----

@pytest.mark.parametrize("case", cases("node_new"), ids=lambda c: repr(c["args"]))
def test_node_new(case):
    n = SNodeActivation(*case["args"])
    assert isinstance(n, list)
    same(state(n), case["state"])


@pytest.mark.parametrize("case", cases("node_spike"),
                         ids=lambda c: f"{c['spike']!r}-{c['depth_reciprocal']}")
def test_node_spike(case):
    n = SNodeActivation(case["depth_reciprocal"])
    for step in case["steps"]:
        ret = sna.spike_several(case["spike"], n)
        same(ret, step["ret"])
        same(state(n), step["state"])


@pytest.mark.parametrize("case", cases("node_weaken"),
                         ids=lambda c: f"{c['spike']!r}-{c['depth_reciprocal']}")
def test_node_weaken(case):
    n = SNodeActivation(case["depth_reciprocal"])
    sna.spike_several(60, n)
    for step in case["steps"]:
        ret = sna.weaken_several(case["spike"], n)
        same(ret, step["ret"])
        same(state(n), step["state"])


@pytest.mark.parametrize("case", cases("node_decay"),
                         ids=lambda c: f"{c['times']!r}-{c['depth_reciprocal']}")
def test_node_decay(case):
    n = SNodeActivation(case["depth_reciprocal"])
    sna.spike_several(40, n)
    for want in case["steps"]:
        assert sna.decay_many_times(case["times"], n) is None
        same(state(n), want)


def test_node_several_at_once():
    n = [SNodeActivation(d) for d in (0.2, 0.5, 1)]
    c = one("node_spike_several")
    same(sna.spike_several(7, *n, n[0]), c["ret"])
    same([state(x) for x in n], c["states"])
    c = one("node_weaken_several")
    same(sna.weaken_several(3, n[2], *n), c["ret"])
    same([state(x) for x in n], c["states"])
    c = one("node_decay_several")
    sna.decay_many_times(2, *n, n[1])
    same([state(x) for x in n], c["states"])
    assert one("node_decay_none")["ok"] == 1
    sna.decay_many_times(2)


@pytest.mark.parametrize("case", cases("node_empty"), ids=lambda c: c["func"])
def test_node_empty_dies(case):
    func = {"SpikeSeveral": sna.spike_several, "WeakenSeveral": sna.weaken_several}[case["func"]]
    with pytest.raises(Confess) as e:
        func(3)
    assert str(e.value) == case["error"]


def test_node_index_semantics():
    # Fractional raw activations truncate; negative ones index from the end of the table.
    n = SNodeActivation(0.3)
    sna.spike_several(1, n)
    assert n[0] == pytest.approx(2.3) and n[2] == sla.PRECALCULATED[2]
    sna.spike_several(-10, n)
    assert n[2] == sla.PRECALCULATED[int(n[0])]
    sna.spike_several(-1000, n)
    assert n[2] is None


# ---- SLinkActivation ----

@pytest.mark.parametrize("case", cases("link_new"), ids=lambda c: repr(c["args"]))
def test_link_new(case):
    link = SLinkActivation(*case["args"])
    assert isinstance(link, list)
    same(state(link), case["state"])
    same(link.get_raw_activation(), case["raw"])
    same(link.get_raw_significance(), case["sig"])
    same(link.get_stability_reciprocal(), case["stab_r"])


@pytest.mark.parametrize("case", cases("link_decay"), ids=lambda c: str(c["pre"]))
def test_link_decay(case):
    link = SLinkActivation()
    if case["pre"]:
        sla.spike(link, case["pre"])
    for step in case["steps"]:
        same(sla.decay(link), step["ret"])
        same(state(link), step["state"])


@pytest.mark.parametrize("case", cases("link_spike"), ids=lambda c: repr(c["spike"]))
def test_link_spike(case):
    link = SLinkActivation()
    for step in case["steps"]:
        same(sla.spike(link, case["spike"]), step["ret"])
        same(state(link), step["state"])


def test_link_spike_default():
    a, b = SLinkActivation(), SLinkActivation()
    assert sla.spike(a) == sla.spike(b, 1)
    assert a == b


def test_link_saturate():
    link = SLinkActivation()
    for step in one("link_saturate")["steps"]:
        same(sla.spike(link, 100), step["ret"])
        same(state(link), step["state"])


@pytest.mark.parametrize("case", cases("link_decay_many"), ids=lambda c: str(c["cnt"]))
def test_link_decay_many(case):
    arr = ["zero"] + [SLinkActivation() for _ in range(3)]
    for i in (1, 2, 3):
        sla.spike(arr[i], 10 * i)
    for _ in range(3):
        sla.decay_many(arr, case["cnt"])
    assert arr[0] == case["first"] and len(arr) == case["length"]
    same([state(x) for x in arr[1:]], case["states"])


def test_link_decay_many_holes():
    c = one("link_decay_many_holes")
    arr = [None, SLinkActivation(), None, SLinkActivation()]
    sla.decay_many(arr, 4)
    assert len(arr) == c["length"]
    assert [int(x is not None) for x in arr] == c["defs"]
    same([state(arr[1]), state(arr[3])], c["states"])


@pytest.mark.parametrize("case", cases("link_decay_many_dies"), ids=lambda c: str(c["nargs"]))
def test_link_decay_many_dies(case):
    args = [[], [[]], [[], 1, 2]][[0, 1, 3].index(case["nargs"])]
    with pytest.raises(Confess) as e:
        sla.decay_many(*args)
    assert case["error"].startswith(str(e.value))
    assert str(e.value) == "DecayMany needs 2 args"


@pytest.fixture
def ltm_activations(monkeypatch):
    acts = [SNodeActivation(), SNodeActivation(), SNodeActivation(0.5)]
    sna.spike_several(30, acts[2])
    sna.spike_several(9, acts[1])
    monkeypatch.setattr(sltm, "ACTIVATIONS", acts, raising=False)
    return acts


@pytest.mark.parametrize("case", cases("amount_to_spread"),
                         ids=lambda c: f"{c['modifier']!r}-{c['pre']}-{c['amount']}")
def test_amount_to_spread(case, ltm_activations):
    link = SLinkActivation(case["modifier"])
    if case["pre"]:
        sla.spike(link, case["pre"])
    same(link.amount_to_spread(case["amount"]), case["ret"])


@pytest.mark.parametrize("case", cases("amount_to_spread_dies"), ids=lambda c: str(c["modifier"]))
def test_amount_to_spread_dies(case, ltm_activations):
    link = SLinkActivation(case["modifier"])
    assert case["dies"] == 1
    with pytest.raises(Confess) as e:
        link.amount_to_spread(10)
    assert str(e.value).startswith(case["prefix"])


def test_amount_to_spread_without_ltm_activations(monkeypatch):
    monkeypatch.delattr(sltm, "ACTIVATIONS", raising=False)
    assert SLinkActivation().amount_to_spread(10) == pytest.approx(10 * 1 / 0.02 / 100 + 1)
    with pytest.raises(Confess):
        SLinkActivation(1).amount_to_spread(10)


# ---- a seeded walk over every operation ----

def test_random_walk():
    c = one("random_walk")
    util.srand(c["seed"])
    nodes = [SNodeActivation() for _ in range(3)]
    links = [None] + [SLinkActivation() for _ in range(3)]
    for step in c["steps"]:
        op = int(util.rand(5))
        who = int(util.rand(3))
        amt = int(util.rand(40)) - 5
        assert (op, who, amt) == (step["op"], step["who"], step["amt"])
        ret = None
        if op == 0:
            ret = sna.spike_several(amt, nodes[who])
        elif op == 1:
            ret = sna.weaken_several(amt, nodes[who])
        elif op == 2:
            sna.decay_many_times(who, *nodes)
        elif op == 3:
            ret = sla.spike(links[who + 1], amt)
        else:
            sla.decay_many(links, who + 1)
        same(ret, step["ret"])
        same([state(x) for x in nodes], step["nodes"])
        same([state(x) for x in links[1:]], step["links"])
        assert all(x[2] is None or not math.isnan(x[2]) for x in nodes)

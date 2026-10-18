"""The compiled Slipnet (`tabletop/_native/slipnet.pyx`, D-111) against the pure one
(`tabletop/slipnet.py`, the specification): the seam serves it on the compiled engine; a run
of jolts and updates from the same state gives the same activations, lengths, distances,
reports and top-down postings on both classes (the item's bound is 1e-9; this build gives
identity, which is what the identity digests of D-108 rely on); the reading interface is
the Python's, with writable views where the Python has lists. Every test skips, with a
reason, when the python engine was asked for explicitly (`tests/test_slipnet.py` runs the
shared unit tests on both classes).
"""

import importlib
import math

import pytest

from tabletop import accel
from tabletop import slipnet as pure
from tabletop import slipnet_def as D
from tabletop.config import PROFILES, Config
from tabletop.rng import Rng

CFG = PROFILES["standard"]

pytestmark = pytest.mark.skipif(
    accel.info()["engine"] != "compiled",
    reason="the compiled engine is not the one running (TABLETOP_ENGINE=python was requested "
           "explicitly)")

JOLTS = ("cup", "group", "used with", "fork", "knife", "left", "right", "neighbor", "opposite",
         "silverware", "one", "two", "sameness", "mirror symmetry", "end", "middle")


def native_module():
    return importlib.import_module(accel.Slipnet.__module__)


def test_the_compiled_engine_serves_the_native_slipnet():
    """`accel.Slipnet` is the compiled class, built from the same definition (D-111)."""
    assert accel.Slipnet.__module__ == "tabletop._native.slipnet"
    assert accel.Slipnet is not pure.Slipnet
    assert "slipnet" in accel.info()["modules"]
    net = accel.Slipnet(CFG)
    assert net.nodes == pure.Slipnet(CFG).nodes and net.links == pure.Slipnet(CFG).links
    assert net.index == {node.name: i for i, node in enumerate(D.NODES)}
    assert net.depth == pure.Slipnet(CFG).depth and net.updates == 0


def test_the_module_constants_and_functions_mirror_the_python():
    mod = native_module()
    assert (mod.ACTIVATION_MIN, mod.ACTIVATION_MAX) == (pure.ACTIVATION_MIN, pure.ACTIVATION_MAX)
    for value in (-5, 0, 1, 77.5, 150, 1000):
        assert mod.clip(value) == pure.clip(value)
    for lengths in ([60, 60, 60], [25], [], [65, 45, 40]):
        assert mod.parallel_distance(lengths) == pure.parallel_distance(lengths)


def same_state(a, b):
    assert list(a.activation) == list(b.activation)
    assert list(a.effective) == list(b.effective)
    assert list(a.distances) == list(b.distances)
    assert a.activations() == b.activations()
    assert a.updates == b.updates


@pytest.mark.parametrize("config", [
    CFG,
    Config(rho=1.5, spread_radius=60.0, slipnet_update_every=2, decay_rate=0.2),
    Config(label_shrink=0.0),
    Config(spread_constant=0.0, label_trickle=0.0),
    Config(max_path_links=1),
])
def test_a_run_of_jolts_and_updates_agrees_exactly(config):
    """From the same start, the same jolts (seeded) and updates leave the two classes in
    the same state after every update, and their reports and postings agree (D-111)."""
    p, n = pure.Slipnet(config), accel.Slipnet(config)
    rp, rn = Rng(7), Rng(7)
    same_state(p, n)
    first_p, first_n = p.start("cup"), n.start("cup")
    assert first_p.changed == first_n.changed and first_p.lengths == first_n.lengths
    same_state(p, n)
    jolter = Rng(11)
    for step in range(300):
        for _ in range(jolter.below(3)):
            name, amount = jolter.choice(JOLTS), jolter.random() * 80
            assert p.jolt(name, amount) == n.jolt(name, amount)
        report_p, report_n = p.update(), n.update()
        same_state(p, n)
        assert report_p.changed == report_n.changed
        assert report_p.lengths == report_n.lengths
        assert report_p.activation_before == report_n.activation_before
        assert report_p.lengths_after == report_n.lengths_after
        assert p.top_down_postings(rp) == n.top_down_postings(rn)
        assert rp.draws == rn.draws
        if step % 50 == 25:
            names = [node.name for node in D.NODES[step % 7::9]]
            assert p.spread_step_into(names) == n.spread_step_into(names)
            assert p.reset(names) == n.reset(names)
            for name in names:
                assert p.restore(name, 30.5 + step) == n.restore(name, 30.5 + step)
            same_state(p, n)
    assert p.updates == n.updates == 301
    for a in D.NODE_NAMES:
        assert n.activation_of(a) == pytest.approx(p.activation_of(a), abs=1e-9)
        for b in D.NODE_NAMES:
            assert n.distance(a, b) == p.distance(a, b)
    assert n.neighbours("cup") == p.neighbours("cup")


def test_the_views_read_and_write_the_arrays():
    """`activation`, `effective` and `distances` are views on the C arrays: reading them is
    reading the state, writing through them changes it (as `tests/test_invariants.py` does
    with the Python's list)."""
    net = accel.Slipnet(CFG)
    n, m = len(net.nodes), len(net.links)
    assert len(net.activation) == n and len(net.effective) == m and len(net.distances) == n * n
    net.activation[0] = 200.0
    assert net.activation_of(net.nodes[0].name) == 200.0 and net.activations()[0][1] == 200.0
    net.jolt(net.nodes[0].name, 0)
    assert net.activation_of(net.nodes[0].name) == 150.0      # clipped again (D-015)
    i, j = net.index["cup"], net.index["liquid holder"]
    assert net.distances[i * n + j] == net.distance("cup", "liquid holder") \
        == pure.Slipnet(CFG).distance("cup", "liquid holder") < 15.0   # the two-link paths too
    assert net.distances[i * n + i] == 0.0 and net.distance("cup", "group") == math.inf
    assert list(net.effective) == pure.Slipnet(CFG).effective     # shrunk by the labels at 1


def test_the_report_has_the_python_fields_and_is_worked_out_once():
    """The native report carries the four lists and computes `changed` and `lengths` on
    first read, the same values as the Python's (D-108, D-111)."""
    net = accel.Slipnet(CFG)
    net.set_activation("cup", 100)
    net.set_activation("opposite", 150)
    report = net.update()
    assert len(report.activation_before) == len(net.nodes) == len(report.activation_after)
    assert len(report.lengths_before) == len(net.links) == len(report.lengths_after)
    assert report.activation_before[net.index["cup"]] == 100.0
    assert report.changed is report.changed and report.lengths is report.lengths
    ref = pure.Slipnet(CFG)
    ref.set_activation("cup", 100)
    ref.set_activation("opposite", 150)
    ref_report = ref.update()
    assert report.changed == ref_report.changed and report.lengths == ref_report.lengths
    assert ("cup", round(net.activation_of("cup"), 1)) in report.changed
    mod = native_module()
    rebuilt = mod.UpdateReport(net.nodes, net.links, report.activation_before,
                               report.lengths_before, report.activation_after,
                               report.lengths_after)
    assert rebuilt.changed == report.changed and rebuilt.lengths == report.lengths


def test_a_custom_table_on_both_classes():
    """A Slipnet built from another node set and link table (Figure 3.6's chunk in
    `tests/test_slipnet.py`) gives the same distances and lengths on both classes."""
    nodes = [D.NodeDef(n, n, "test", 1) for n in ("A", "B")] + \
            [D.NodeDef(n, n, "test", 2) for n in ("n1", "n2", "n3")]
    links = [D.LinkDef("A", "n1", D.ISA, 30), D.LinkDef("B", "n1", D.ISA, 35),
             D.LinkDef("A", "n2", D.ISA, 20), D.LinkDef("B", "n2", D.ISA, 25),
             D.LinkDef("A", "B", D.LABELED, 40, ("n3",))]
    cfg = Config(spread_constant=0.0, label_trickle=0.0, decay_rate=0.0, label_shrink=0.6)
    p, n = pure.Slipnet(cfg, nodes, links), accel.Slipnet(cfg, nodes, links)
    same_state(p, n)
    assert n._paths == p._paths and n._neighbours == p._neighbours
    for net in (p, n):
        net.set_activation("n3", 150)
        net.update()
    same_state(p, n)
    assert n.distance("A", "B") == p.distance("A", "B") == pytest.approx(1 / (1 / 65 + 1 / 45 + 1 / 16))
    assert n.effective_length(n.links[4]) == p.effective_length(p.links[4])


def test_the_publisher_is_the_worlds_and_a_lone_net_publishes_to_no_one():
    """`events` is set by the World (D-065); a Slipnet on its own records nothing."""
    from tabletop.problems import load_problem
    from tabletop.world import World
    net = accel.Slipnet(CFG)
    net.jolt("cup", 10.0)
    assert net.events.count == 0
    world = World(load_problem("surround-05"), CFG, 1)
    assert type(world.slipnet) is accel.Slipnet and world.slipnet.events is world.events

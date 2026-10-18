"""The Slipnet (D-007 to D-020): the node set and the screen's grid, the levels, the link
table against every link the sources name, the parallel-path formula on Figure 3.6's
example [F95 p. 61 (img)], the ten orderings of D-009, label-controlled lengths, spreading,
the label trickle, decay by depth, the 1-150 range, the initial state against the first
screen dump (direction only, never fitted; ANSWERS.md 1), the stability of the net over many
updates, and the top-down posting hook (seeded, with binomial bounds). Every test runs on
both Slipnet classes (D-111): the pure `tabletop.slipnet.Slipnet`, the specification, and the
compiled `accel.Slipnet` when the compiled engine is running (skipped, with a reason, only
when the python engine was asked for explicitly); the fixture `Slipnet` below swaps the
module's name, so the helpers (`figure_3_6`, `chain`) build the class under test.
"""

import json
import math
import sys
from pathlib import Path

import pytest

from tabletop import accel
from tabletop import objects as O
from tabletop import slipnet_def as D
from tabletop.config import PROFILES, Config
from tabletop.rng import Rng
from tabletop.slipnet import ACTIVATION_MAX, ACTIVATION_MIN, Slipnet, clip, parallel_distance
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
ENGINES = ["python", "compiled"]


@pytest.fixture(params=ENGINES, autouse=True)
def Slipnet(request, monkeypatch):
    """The Slipnet class under test, one engine per parameter (D-111), bound to this
    module's `Slipnet` name for the tests and their helpers."""
    import tabletop.slipnet as pure
    if request.param == "python":
        cls = pure.Slipnet
    elif accel.info()["engine"] != "compiled":
        pytest.skip("the compiled engine is not the one running (TABLETOP_ENGINE=python was "
                    "requested explicitly)")
    else:
        cls = accel.Slipnet
    monkeypatch.setattr(sys.modules[__name__], "Slipnet", cls)
    return cls
CH4 = json.loads((Path(__file__).parent / "data" / "ch4.json").read_text())

# French's list of forty-seven concepts [F95 p. 62], in SPEC.md 3.1's five groups.
FRENCHS_LIST = {
    "object": ["cup", "saucer", "plate", "knife", "fork", "spoon", "saltshaker",
               "pepper shaker", "small glass", "big glass", "soup bowl"],
    "category": ["sharp object", "silverware", "crockery", "liquid holder", "food holder",
                 "eat-from object", "receptacle", "buccal object", "open object",
                 "closed object"],
    "relation": ["sameness", "opposite", "bigger than", "smaller than", "similar shape to",
                 "used with", "symmetry", "mirror symmetry", "diagonal symmetry"],
    "structure": ["group", "neighbor", "direction", "left", "right", "above", "below",
                  "horizontal", "vertical", "position", "end", "middle"],
    "number": ["number", "one", "two", "three", "many"],
}


def quiet(**changes) -> Config:
    """A configuration with spreading and the trickle off, to test one mechanism alone."""
    return v2(spread_constant=0.0, label_trickle=0.0, **changes)


def still(**changes) -> Config:
    """`quiet` with decay off too: only the lengths and distances move."""
    return quiet(decay_rate=0.0, **changes)


# ---------------------------------------------------------------------------------------------
# The nodes (D-007, D-008).


def test_the_47_of_frenchs_list_plus_isolated():
    """'forty-six nodes' [F95 p. 62] against the 47 listed; 'isolated' [F95 p. 68] (D-007)."""
    names = [node.name for node in D.NODES]
    assert len(names) == 63 and len(set(names)) == 63      # 48 + the 15 role nodes (D-080)
    listed = [name for group in FRENCHS_LIST.values() for name in group]
    assert len(listed) == 47
    assert names[:47] == listed
    assert names[47] == "isolated" and D.node_named("isolated").on_screen is False
    assert names[48:] == list(D.ROLE_NODES) and len(D.ROLE_NODES) == 15
    assert all(not D.node_named(n).on_screen for n in D.ROLE_NODES)
    for group, members in FRENCHS_LIST.items():
        assert [n.name for n in D.NODES if n.group == group
                and n.name not in ("isolated",) + D.ROLE_NODES] == members


def test_every_object_type_has_a_node():
    for type_ in O.TYPES:
        node = D.node_named(D.node_for_type(type_))
        assert node.level == 1 and node.group == "object"
    assert D.node_for_type("salt shaker") == "saltshaker"


def test_the_screen_grid_is_the_chapter_4_grid_minus_the_absent_types():
    """The 41 boxes [F95 p. 98 (img)] are the nodes marked on screen; the six missing are the
    object types absent from the chapter 4 configuration (SPEC.md 3.1)."""
    assert [list(row) for row in D.SCREEN_GRID] == CH4["slipnet_grid"]
    on_screen = [node.label for node in D.NODES if node.on_screen]
    grid = [label for row in D.SCREEN_GRID for label in row]
    assert sorted(on_screen) == sorted(grid) and len(grid) == 41
    for label in grid:
        assert D.node_labelled(label).label == label
    off_screen = [node.name for node in D.NODES if not node.on_screen]
    assert off_screen == ["saucer", "plate", "saltshaker", "pepper shaker", "big glass",
                          "soup bowl", "isolated"] + list(D.ROLE_NODES)
    on_table = {obj["type"] for obj in CH4["problem"]["objects"]}
    assert on_table == {"cup", "fork", "knife", "spoon", "small glass"}
    assert not on_table & set(off_screen)


@pytest.mark.parametrize("name, level", [
    ("cup", 1), ("fork", 1), ("plate", 1),                        # [F95 p. 62]
    ("crockery", 2), ("silverware", 2), ("sharp object", 2),      # [F95 p. 62]
    ("used with", 2), ("bigger than", 2), ("neighbor", 2), ("left", 2),  # [F95 p. 62]
    ("opposite", 3), ("direction", 3),                            # [F95 p. 62]
    ("position", 3), ("number", 3), ("symmetry", 3), ("sameness", 3),  # D-008's rule
])
def test_the_levels_follow_the_three_heuristics(name, level):
    assert D.node_named(name).level == level


def test_every_node_has_a_level_and_a_depth_from_the_config():
    net = Slipnet(CFG)
    for node in D.NODES:
        assert node.level in D.LEVELS
        assert net.depth[net.index[node.name]] == CFG.depth_by_level[node.level - 1]
    assert CFG.depth_by_level == (20, 50, 80)


# ---------------------------------------------------------------------------------------------
# The link table (D-009, D-010).


def test_links_join_nodes_and_labels_are_nodes():
    seen = []
    for link in D.LINKS:
        assert link.a in D.NODE_INDEX and link.b in D.NODE_INDEX
        assert link.kind in (D.ISA, D.LABELED, D.IDENTITY)
        assert 10 <= link.length <= 100
        for label in link.labels:
            assert label in D.NODE_INDEX
        if link.kind == D.ISA:
            assert not link.labels
        else:
            assert link.labels
        if link.kind == D.IDENTITY:
            assert link.a == link.b
        else:
            assert link.a != link.b
            pair = frozenset((link.a, link.b))
            assert pair not in seen, f"two links join {link.a} and {link.b}"
            seen.append(pair)
        assert ("F95" in link.source or "FCCA" in link.source
                or link.source in ("domain judgement", D.REINTERPRETATION))   # D-080's rows


@pytest.mark.parametrize("a, b, kind, label", [
    ("soup bowl", "liquid holder", D.ISA, None),     # [F95 p. 42]
    ("soup bowl", "food holder", D.ISA, None),       # [F95 p. 42]
    ("soup bowl", "crockery", D.ISA, None),          # [F95 p. 42]
    ("knife", "silverware", D.ISA, None),            # [F95 p. 42]
    ("knife", "sharp object", D.ISA, None),          # [F95 p. 42]
    ("cup", "liquid holder", D.ISA, None),           # [F95 p. 63]
    ("small glass", "liquid holder", D.ISA, None),   # Fig. 3.5 [F95 p. 60 (img)]
    ("big glass", "liquid holder", D.ISA, None),     # Fig. 3.5 [F95 p. 60 (img)]
    ("small glass", "big glass", D.LABELED, "smaller than"),  # Fig. 3.5 [F95 p. 60 (img)]
    ("knife", "fork", D.LABELED, "used with"),       # [F95 p. 59; F95 p. 62]
    ("cup", "saucer", D.LABELED, "used with"),       # [F95 p. 59; F95 p. 62]
    ("saucer", "plate", D.LABELED, "smaller than"),  # [F95 p. 59]
    ("fork", "silverware", D.ISA, None),             # [F95 p. 117]
    ("saucer", "crockery", D.ISA, None),             # [F95 p. 119]
    ("cup", "crockery", D.ISA, None),                # [F95 p. 119]
    ("left", "right", D.LABELED, "opposite"),        # [F95 p. 62]
    ("left", "right", D.LABELED, "diagonal symmetry"),  # [F95 p. 68]
    ("left", "direction", D.ISA, None),              # [F95 p. 62]
    ("one", "two", D.LABELED, "number"),             # [F95 p. 68]
    ("spoon", "silverware", D.ISA, None),            # [FCCA p. 389]
])
def test_every_link_the_sources_name_is_in_the_table(a, b, kind, label):
    links = [l for l in D.links_of(a) if {l.a, l.b} == {a, b} and l.kind == kind]
    assert len(links) == 1, f"{a}-{b} {kind}"
    if kind == D.ISA:
        assert links[0].a == a and links[0].b == b
    if label:
        assert label in links[0].labels
        assert label in D.labels_between(a, b)


def test_spoon_and_knife_share_only_silverware():
    """'Spoons and knives, aside from being silverware, share only ...' [FCCA p. 389]."""
    shared = set(D.categories_of("spoon")) & set(D.categories_of("knife"))
    assert "silverware" in shared
    assert shared <= {"silverware", "buccal object"}


def test_the_links_left_out_are_absent_and_say_why():
    for a, b, reason in D.LINKS_LEFT_OUT:
        assert not [l for l in D.LINKS if {l.a, l.b} == {a, b}], f"{a}-{b} is in the table"
        assert reason


def test_identity_links_name_mirror_symmetry_and_isa_links_have_no_label():
    """A concept slipping to itself feeds 'mirror symmetry' [F95 p. 85] (D-009, D-030)."""
    for name in ("left", "right", "above", "below"):
        assert D.labels_between(name, name) == ("mirror symmetry",)
    assert D.labels_between("cup", "liquid holder") == ()
    assert D.labels_between("cup", "group") == ()
    assert D.categories_of("cup")[0] == "liquid holder"


# ---------------------------------------------------------------------------------------------
# The parallel-path distance (D-012) [F95 p. 61 (img)].


def test_the_resistor_formula():
    """R = 1 / (1/r1 + 1/r2 + 1/r3) [F95 p. 61 (img)]."""
    assert parallel_distance([60, 60, 60]) == pytest.approx(20)
    assert parallel_distance([25]) == 25
    assert parallel_distance([]) == math.inf
    assert parallel_distance([65, 45, 40]) == pytest.approx(1 / (1 / 65 + 1 / 45 + 1 / 40))


def figure_3_6(label_shrink=0.0):
    """Figure 3.6's chunk of a Slipnet: A and B joined directly (d3, labelled by n3) and
    through n1 (d1, d1') and n2 (d2, d2') by ISA links [F95 p. 61 (img)]."""
    nodes = [D.NodeDef(n, n, "test", 1) for n in ("A", "B")] + \
            [D.NodeDef(n, n, "test", 2) for n in ("n1", "n2", "n3")]
    links = [
        D.LinkDef("A", "n1", D.ISA, 30), D.LinkDef("B", "n1", D.ISA, 35),   # d1 = 30, d1' = 35
        D.LinkDef("A", "n2", D.ISA, 20), D.LinkDef("B", "n2", D.ISA, 25),   # d2 = 20, d2' = 25
        D.LinkDef("A", "B", D.LABELED, 40, ("n3",)),                      # d3 = 40
    ]
    return Slipnet(still(label_shrink=label_shrink), nodes, links)


def test_figure_3_6_distance_is_the_parallel_combination_of_its_three_pathways():
    """Distance(A,B) = 1 / (1/(d1 + d1') + 1/(d2 + d2') + 1/d3) [F95 p. 61 (img)]."""
    net = figure_3_6()
    expected = 1 / (1 / (30 + 35) + 1 / (20 + 25) + 1 / 40)
    assert net.distance("A", "B") == pytest.approx(expected)
    assert net.distance("B", "A") == pytest.approx(expected)
    assert net.distance("A", "A") == 0.0
    # Each pathway alone is longer than their combination [F95 p. 60].
    assert expected < 40 and expected < 45 and expected < 65
    # Through one ISA node: A-n1 directly 30, and A-n2-B... no: n1 and n2 share no link.
    assert net.distance("n1", "n2") == pytest.approx(1 / (1 / (30 + 20) + 1 / (35 + 25)))
    assert net.distance("A", "n3") == math.inf   # a label hangs off a link, it is not on it


def test_an_active_label_shrinks_the_direct_pathway_and_the_distance():
    """'the length of any labeled link varies dynamically as a function of the activation
    of the link's label node' [F95 p. 56] (D-011)."""
    net = figure_3_6(label_shrink=0.6)
    far = net.distance("A", "B")
    net.set_activation("n3", 150)
    net.update()
    assert net.effective_length(net.links[4]) == pytest.approx(40 * (1 - 0.6))
    assert net.distance("A", "B") == pytest.approx(1 / (1 / 65 + 1 / 45 + 1 / 16))
    assert net.distance("A", "B") < far
    assert net.effective_length(net.links[0]) == 30   # ISA links never change


def test_only_one_and_two_link_paths_count_by_default():
    """D-012: `max_path_links` = 2; with 1, only direct links."""
    net = Slipnet(v2(max_path_links=1))
    assert net.distance("cup", "liquid holder") == 15      # the direct ISA link alone
    assert net.distance("cup", "plate") == math.inf        # joined only through crockery
    assert Slipnet(CFG).distance("cup", "plate") < math.inf
    assert CFG.max_path_links == 2


# ---------------------------------------------------------------------------------------------
# The orderings of D-009 (chapter 5's qualitative claims; SPEC.md 3.3).


@pytest.fixture
def net(Slipnet):
    return Slipnet(CFG)


def test_knife_and_fork_are_quite_close_and_knife_to_saltshaker_very_long(net):
    """[F95 p. 59; F95 p. 117]"""
    types = [D.node_for_type(t) for t in O.TYPES]
    shortest = min(net.distance(a, b) for a in types for b in types if a != b)
    assert net.distance("knife", "fork") <= shortest * 1.2
    assert net.distance("knife", "saltshaker") == math.inf
    assert net.distance("knife", "saltshaker") > net.distance("cup", "plate")


def test_cup_and_small_glass_are_very_close(net):
    """[F95 p. 122; F95 p. 126]: the small glass is the type nearest to the cup."""
    others = [D.node_for_type(t) for t in O.TYPES if t != "cup"]
    assert min(others, key=lambda t: net.distance("cup", t)) == "small glass"


def test_cup_and_saucer_are_almost_as_close_and_plates_farther(net):
    """'almost as close together ... as the concepts cup and' small glass [F95 p. 119];
    'plates are conceptually quite far removed from cups' yet 'at least weakly related'
    [F95 p. 120; F95 p. 139]."""
    assert net.distance("cup", "small glass") <= net.distance("cup", "saucer")
    assert net.distance("cup", "saucer") <= net.distance("cup", "small glass") * 1.5
    assert net.distance("cup", "saucer") < net.distance("cup", "plate") < math.inf


def test_a_saltshaker_is_completely_dissimilar_to_a_cup_and_a_spoon(net):
    """[F95 p. 121; F95 p. 136; F95 p. 139]"""
    assert net.distance("cup", "saltshaker") == math.inf
    assert net.distance("spoon", "saltshaker") == math.inf
    assert net.distance("cup", "saltshaker") > net.distance("cup", "plate")


def test_a_spoon_is_very_far_from_a_glass_but_forks_and_spoons_close(net):
    """[F95 p. 125; F95 p. 135]"""
    assert net.distance("spoon", "small glass") > 3 * net.distance("fork", "spoon")
    assert net.distance("fork", "spoon") < 2 * net.distance("knife", "fork")


def test_a_cup_is_much_closer_to_a_glass_than_to_a_saltshaker(net):
    """[F95 p. 136]"""
    assert net.distance("cup", "small glass") * 5 < net.distance("cup", "saltshaker")


def test_two_to_two_is_closer_than_one_to_two(net):
    """[F95 p. 68]"""
    assert net.distance("two", "two") < net.distance("one", "two") < math.inf


def test_the_cups_nearest_concept_is_liquid_holder(net):
    """The first dump: 'some activation has already begun to spread from cup to liquid
    holder, crockery, and so on' [F95 p. 97]; liquid holder 4, the other four 2, receptacle 1
    [F95 p. 96 (img)] (D-009, D-010)."""
    nearest = [name for name, _ in net.neighbours("cup")]
    assert nearest[0] == "liquid holder"
    for name in ("small glass", "crockery", "open object", "eat-from object"):
        assert net.distance("cup", name) < net.distance("cup", "receptacle")
    assert net.distance("cup", "receptacle") < math.inf
    assert net.distance("cup", "group") == math.inf


# ---------------------------------------------------------------------------------------------
# Activation: the range, jolts, decay (D-014 to D-016).


def test_the_range_is_1_to_150_and_the_clip():
    """'Activation values range from 1 to 150.' [F95 p. 95] (D-015)"""
    assert (ACTIVATION_MIN, ACTIVATION_MAX) == (1.0, 150.0)
    assert clip(-5) == 1 and clip(1000) == 150 and clip(77.5) == 77.5
    net = Slipnet(quiet())
    assert net.jolt("cup", 200) == 150
    assert net.jolt("cup", 10) == 150
    assert net.set_activation("fork", -3) == 1
    assert net.set_activation("fork", 40) == 40
    assert net.jolt("fork", CFG.jolt_structure) == 80
    net.update()
    assert all(1 <= a <= 150 for _, a in net.activations())
    assert net.reset(["cup", "fork"]) == ["cup", "fork"]
    assert net.activation_of("cup") == 1 and net.activation_of("fork") == 1


def test_a_fresh_slipnet_is_dormant_and_updates_change_nothing():
    net = Slipnet(CFG)
    assert all(a == 1 for _, a in net.activations())
    report = net.update()
    assert report.changed == [] and report.lengths == []
    assert all(a == 1 for _, a in net.activations())


def test_decay_is_slower_for_deeper_nodes():
    """'The greater the conceptual depth of a node, the less its activation will decay with
    each cycle.' [F95 p. 62] (D-014): a <- a - (a - 1)(100 - depth)/100 x rate."""
    net = Slipnet(quiet())
    for name in ("cup", "crockery", "opposite"):      # levels 1, 2, 3
        net.set_activation(name, 100)
    net.update()
    cup, crockery, opposite = (net.activation_of(n) for n in ("cup", "crockery", "opposite"))
    assert cup == pytest.approx(100 - 99 * 0.8 * 0.1)
    assert crockery == pytest.approx(100 - 99 * 0.5 * 0.1)
    assert opposite == pytest.approx(100 - 99 * 0.2 * 0.1)
    assert cup < crockery < opposite


def test_decay_returns_a_node_to_dormancy_and_never_below_it():
    """'the node will thus tend to return to dormancy' [F95 p. 62]: a level-1 jolt of 40
    fades to within 5 of the floor in about 30 updates (D-014's reason)."""
    net = Slipnet(quiet())
    net.jolt("cup", CFG.jolt_structure)
    values = []
    for _ in range(30):
        net.update()
        values.append(net.activation_of("cup"))
    assert values == sorted(values, reverse=True)
    assert 1 < values[-1] < 6
    for _ in range(500):
        net.update()
    assert net.activation_of("cup") >= 1


def test_flat_depths_are_the_lesion():
    """'make all concepts have the same conceptual-depth value' [FCCA p. 365] (D-008)."""
    net = Slipnet(quiet(depth_by_level=(50, 50, 50)))
    for name in ("cup", "crockery", "opposite"):
        net.set_activation(name, 100)
    net.update()
    assert len({round(net.activation_of(n), 6) for n in ("cup", "crockery", "opposite")}) == 1


# ---------------------------------------------------------------------------------------------
# Spreading (D-013), on a three-node chain: A ISA B (20), B ISA C (20).


def chain(**changes):
    nodes = [D.NodeDef("A", "A", "t", 1), D.NodeDef("B", "B", "t", 2), D.NodeDef("C", "C", "t", 2)]
    links = [D.LinkDef("A", "B", D.ISA, 20), D.LinkDef("B", "C", D.ISA, 20)]
    cfg = v2(**{"spread_constant": 1.0, "decay_rate": 0.0, "label_trickle": 0.0,
                    **changes})
    return Slipnet(cfg, nodes, links)


def test_spreading_is_inversely_proportional_to_distance_and_buffered():
    """'the amount of activation that spreads from A to B is proportional to the activation
    of A divided by dA,B' [F95 p. 61]; the two-link path A-B-C has length 40 (D-012); what B
    receives is not forwarded in the same update (D-013)."""
    net = chain()
    net.set_activation("A", 101)
    net.update()
    assert net.activation_of("B") == pytest.approx(1 + 100 / 20)
    assert net.activation_of("C") == pytest.approx(1 + 100 / 40)
    assert net.activation_of("A") == 101                 # the sender keeps what it has


def test_activation_radiates_from_the_warmer_node_only():
    """'the more activation is radiated from the warmer node to its neighbor' [F95 p. 60]."""
    net = chain()
    net.set_activation("A", 101)
    net.set_activation("B", 150)
    net.update()
    # B is warmer than A: A sends B nothing; B sends A (150 - 101)/20 and C 149/20; A still
    # sends C 100/40 along the two-link path.
    assert net.activation_of("A") == pytest.approx(101 + 49 / 20)
    assert net.activation_of("B") == 150
    assert net.activation_of("C") == pytest.approx(1 + 100 / 40 + 149 / 20)


def test_a_node_cannot_spread_more_than_it_possesses():
    """[F95 p. 59]: the total sent is capped at the excess over dormancy, shared in
    proportion (D-013)."""
    net = chain(spread_constant=100.0)
    net.set_activation("A", 101)
    net.update()
    sent = (net.activation_of("B") - 1) + (net.activation_of("C") - 1)
    assert sent == pytest.approx(100)
    assert net.activation_of("B") - 1 == pytest.approx(2 * (net.activation_of("C") - 1))


def test_nothing_spreads_beyond_the_radius_or_with_the_constant_at_zero():
    net = chain(spread_radius=30.0)
    net.set_activation("A", 101)
    net.update()
    assert net.activation_of("B") > 1 and net.activation_of("C") == 1
    net = chain(spread_constant=0.0)
    net.set_activation("A", 101)
    net.update()
    assert net.activation_of("B") == 1 and net.activation_of("C") == 1


# ---------------------------------------------------------------------------------------------
# The label trickle (D-018) and label-controlled lengths (D-011) on the real net.


def test_a_label_gets_a_small_amount_when_both_its_concepts_are_active():
    """'whenever both of the concepts joined by a labeled link are active, a small amount of
    activation is also sent to the label node' [F95 p. 62]: knife and fork feed 'used with'."""
    net = Slipnet(v2(spread_constant=0.0))
    net.set_activation("knife", 100)
    net.set_activation("fork", 100)
    net.update()
    used_with = net.activation_of("used with")
    assert 1 + 5 * 0.95 <= used_with <= 1 + 5           # 0.05 x 100, then level-2 decay
    assert net.activation_of("similar shape to") == 1   # fork-spoon: the spoon is dormant
    net.set_activation("fork", 40)                      # below the threshold of 50
    before = net.activation_of("used with")
    net.update()
    assert net.activation_of("used with") < before
    assert CFG.label_trickle_threshold == 50 and CFG.label_trickle == 0.05


def test_identity_links_never_trickle():
    """A self-link joins one concept, not two (D-018)."""
    net = Slipnet(v2(spread_constant=0.0))
    net.set_activation("right", 150)
    for _ in range(5):
        net.update()
    assert net.activation_of("mirror symmetry") == 1


def test_opposite_shrinks_every_link_it_labels_and_the_shrink_is_undone():
    """'This will then shrink the length of all of the links labeled by the label node.'
    [F95 p. 62]; 'the original, long default distance returns' when the context decays
    [F95 p. 56] (D-011)."""
    net = Slipnet(still(label_shrink=0.6))
    left_right = [l for l in net.links if {l.a, l.b} == {"left", "right"}][0]
    above_below = [l for l in net.links if {l.a, l.b} == {"above", "below"}][0]
    cup_lh = [l for l in net.links if (l.a, l.b) == ("cup", "liquid holder")][0]
    assert net.effective_length(left_right) == pytest.approx(40 * (1 - 0.6 / 150))
    net.set_activation("opposite", 150)
    report = net.update()
    assert net.effective_length(left_right) == pytest.approx(16)
    assert net.effective_length(above_below) == pytest.approx(16)
    assert net.effective_length(cup_lh) == 15
    assert ("left-right", 16.0) in report.lengths and ("above-below", 16.0) in report.lengths
    assert net.distance("left", "right") < 16                # plus the path through direction
    net.set_activation("opposite", 1)
    net.update()
    assert net.effective_length(left_right) == pytest.approx(40 * (1 - 0.6 / 150))


def test_the_most_active_of_a_links_labels_sets_its_length_and_the_lesion_freezes_it():
    net = Slipnet(still(label_shrink=0.6))
    net.set_activation("diagonal symmetry", 150)
    net.update()
    left_right = [l for l in net.links if {l.a, l.b} == {"left", "right"}][0]
    assert net.effective_length(left_right) == pytest.approx(16)
    frozen = Slipnet(still(label_shrink=0.0))               # [FCCA p. 365]
    frozen.set_activation("opposite", 150)
    report = frozen.update()
    assert frozen.effective_length(frozen.links[frozen.links.index(left_right)]) == 40
    assert report.lengths == []


# ---------------------------------------------------------------------------------------------
# The initial state against the first screen dump (D-017): direction only, never fitted.


def dump(figure):
    return [d for d in CH4["dumps"] if str(d["figure"]) == str(figure)][0]["slipnet"]


def test_start_sets_opposite_and_the_touched_type_then_updates_once():
    """'the concept opposite and the category of the touched object ... are active
    initially' [F95 p. 66]; 139 and 51 read off Fig. 4.1 [F95 p. 96 (img)] (D-017)."""
    net = Slipnet(CFG)
    report = net.start("cup")
    assert net.updates == 1
    ranked = sorted(net.activations(), key=lambda p: -p[1])
    assert [name for name, _ in ranked[:2]] == ["opposite", "cup"]
    assert 130 < net.activation_of("opposite") <= 139
    assert 45 < net.activation_of("cup") <= 51
    assert ("cup", round(net.activation_of("cup"), 1)) in report.changed
    assert report.changed[0][0] == "cup"   # definition order
    assert CFG.initial_opposite == 139 and CFG.initial_touched_type == 51
    # The touched type may be given as the palette spells it.
    other = Slipnet(CFG)
    other.start("salt shaker")
    assert other.activation_of("saltshaker") > 45


def test_the_first_update_spreads_where_the_first_dump_shows_it():
    """Fig. 4.1 at 0 codelets: 'liq holder' 4; 'small glass', 'crockery', 'open obj' and
    'eat-from-ob' 2; every other box 1 [F95 p. 96 (img)]; 'some activation has already begun
    to spread from cup to liquid holder, crockery, and so on' [F95 p. 97]. Direction only:
    which boxes are above the floor and which is highest (ANSWERS.md 1)."""
    net = Slipnet(CFG)
    net.start("cup")
    shown = dump(4.1)
    ours = {D.node_labelled(label).name: a for label, a in
            [(n.label, net.activation_of(n.name)) for n in D.NODES if n.on_screen]}
    above = [D.node_labelled(label).name for label, a in shown.items()
             if a > 1 and label not in ("cup", "opposite")]
    assert sorted(above) == sorted(["liquid holder", "small glass", "crockery",
                                    "open object", "eat-from object"])
    for name in above:
        assert ours[name] > 1.5, name
    assert max(above, key=ours.get) == "liquid holder"
    for label, a in shown.items():
        if a == 1:
            assert round(ours[D.node_labelled(label).name]) == 1, label
    # Nothing reaches a node with no pathway from the cup.
    for name in ("group", "neighbor", "symmetry", "one", "many", "isolated", "knife"):
        assert net.activation_of(name) == 1


def test_the_net_does_not_run_away_over_a_run_without_jolts():
    """Fifty-five updates with no codelet (the chapter 4 run's length [F95 p. 107 (img)]):
    every node stays below the cup's starting activation, the cup decays, and the total
    excess falls (D-013's stability, D-014)."""
    net = Slipnet(CFG)
    net.start("cup")
    totals, roles = [], []
    for _ in range(55):
        net.update()
        totals.append(sum(a - 1 for name, a in net.activations()
                          if name != "opposite" and name not in D.ROLE_NODES))
        roles.append(sum(a - 1 for name, a in net.activations() if name in D.ROLE_NODES))
    assert net.activation_of("cup") < 10
    assert max(a for name, a in net.activations() if name != "opposite") < 51
    assert totals[-1] < totals[0]
    # The role nodes are warmed by "opposite", which had no link in v1 and now has one to
    # "key" (D-080): their excess is bounded by what "opposite" spreads, and settles.
    assert roles[-1] < 15 * 50 and roles[-1] <= max(roles) and roles[-1] - roles[-2] < 1.0


def test_a_cup_held_near_the_ceiling_warms_its_categories_in_the_dumps_order():
    """Figs. 4.3-4.9: with 'cup' at 127-130, 'liq holder' (32-50) is the most active of the
    cup's categories and 'eat-from-ob' (14-21) the least, 'receptacle' (3-6) below all of
    them; no node is warmer than its source; the nodes with no pathway from the cup or
    from 'opposite' stay at 1 [F95 pp. 99-107 (img)]. Direction only (ANSWERS.md 1)."""
    net = Slipnet(CFG)
    net.start("cup")
    for _ in range(50):
        net.set_activation("cup", 130)
        net.set_activation("opposite", 150)
        net.update()
    categories = {name: net.activation_of(name) for name in D.categories_of("cup")}
    assert sorted(categories) == ["crockery", "eat-from object", "liquid holder", "open object"]
    assert max(categories, key=categories.get) == "liquid holder"
    assert min(categories, key=categories.get) == "eat-from object"
    assert net.activation_of("receptacle") < min(categories.values())
    assert net.activation_of("receptacle") < net.activation_of("liquid holder") / 1.5
    assert all(a <= net.activation_of("cup") for name, a in net.activations()
               if name != "opposite")
    for name in ("one", "many"):
        assert net.activation_of(name) == 1
    # "group", "neighbor", "symmetry", "isolated", "end" and "sameness" now have a pathway
    # from "opposite" through the role nodes (opposite - key - role, then container - group,
    # sidekick - neighbor, separator - mirror symmetry - symmetry, figure - isolated,
    # anchor - middle - end, counterpart - sameness; D-080): warmed a little, never near
    # the cup's categories, which the dumps hold at 14 and above.
    for name in ("group", "neighbor", "symmetry", "isolated", "end", "sameness"):
        assert 1 < net.activation_of(name) < 14, name
    assert net.activation_of("role") < net.activation_of("opposite") / 3
    assert net.activation_of("closed object") > 1       # through open object [F95 pp. 99-107 (img)]


# ---------------------------------------------------------------------------------------------
# The top-down posting hook (D-020).


def test_a_rising_node_posts_its_codelets_in_table_order_a_steady_one_nothing():
    """"when the concept group is sufficiently active, find-group codelets are posted" [F95
    p. 63], in proportion to the rise, "the rate of change of activation of 'group'" [F95
    p. 64] (D-020): a rise of two structure jolts posts two codelets without a draw; a node
    that holds, however active, posts nothing (the dumps show "opposite" at 137-150 through
    nine dumps and never a pick-strong-cp bar [F95 pp. 96-107 (img)])."""
    net = Slipnet(quiet())
    rng = Rng(1)
    assert net.top_down_postings(rng) == []
    net.set_activation("opposite", 150)
    net.set_activation("neighbor", 1 + 2 * CFG.top_down_rise)
    net.set_activation("group", 1 + CFG.top_down_rise)
    net.set_activation("end", 1 + CFG.top_down_rise)
    net._posted_at[net.index["opposite"]] = 150
    assert net.top_down_postings(rng) == [("group", "find-group"), ("neighbor", "find-neighbor"),
                                          ("neighbor", "find-neighbor"), ("end", "find-end-objs")]
    assert rng.draws == 0
    assert net.top_down_postings(rng) == []        # nothing has risen since
    net.set_activation("group", 1)
    assert net.top_down_postings(rng) == []        # a fall posts nothing
    assert dict(CFG.top_down_table)["cup"] == "pick-salient-obj"
    assert CFG.top_down_rise == 40


def test_the_remainder_of_a_rise_posts_with_one_coin():
    """A rise of half a `top_down_rise` posts one codelet half the time (D-020). 600 trials
    with one seed: the binomial mean is 300 and the standard deviation 12.2, so the count
    lies within 251-349 unless the rule is wrong (four standard deviations)."""
    net = Slipnet(quiet())
    rng = Rng(2026)
    posted = 0
    for _ in range(600):
        net._posted_at[net.index["group"]] = 1
        net.set_activation("group", 1 + CFG.top_down_rise / 2)
        posted += len(net.top_down_postings(rng))
    assert 251 <= posted <= 349
    assert rng.draws == 600


def test_postings_are_reproducible_from_the_seed():
    def run(seed):
        net = Slipnet(quiet())
        rng = Rng(seed)
        out = []
        for i in range(40):
            net.set_activation("group", 20 + 10 * (i % 5))
            net.set_activation("cup", 50 + 25 * (i % 3))
            out.append(net.top_down_postings(rng))
        return out
    assert run(7) == run(7)
    assert run(7) != run(8)


# ---------------------------------------------------------------------------------------------
# Reports, snapshots and independence.


def test_the_update_report_lists_what_moved_in_definition_order():
    net = Slipnet(quiet())
    net.set_activation("opposite", 150)
    net.set_activation("cup", 100)
    report = net.update()
    names = [name for name, _ in report.changed]
    assert names == ["cup", "opposite"]
    assert all(isinstance(a, float) and a == round(a, 1) for _, a in report.changed)
    assert [name for name, _ in report.lengths] == ["open object-closed object", "left-right",
                                                    "above-below",
                                                    "mirror symmetry-diagonal symmetry",
                                                    "figure-ground"]   # D-080's two "opposite" links


def test_two_slipnets_do_not_share_state_and_neighbours_are_sorted():
    a, b = Slipnet(CFG), Slipnet(CFG)
    a.jolt("cup", 100)
    assert b.activation_of("cup") == 1
    assert D.NODES[0].name == "cup" and Slipnet(CFG).activation_of("cup") == 1
    near = a.neighbours("cup")
    assert [d for _, d in near] == sorted(d for _, d in near)
    assert all(d <= CFG.spread_radius for _, d in near)
    assert a.level("cup") == 1 and a.level("opposite") == 3


def test_the_update_does_not_depend_on_the_order_of_the_nodes_or_the_links():
    """The update buffers every transfer before applying any (D-013, D-019), so a Slipnet
    built from the same table in reverse order, driven by the same jolts, holds the same
    activations node by node and the same distances; the Worldview's queries answer from
    the Slipnet's own table (D-009)."""
    forward = Slipnet(CFG)
    backward = Slipnet(CFG, nodes=tuple(reversed(D.NODES)), links=tuple(reversed(D.LINKS)))
    for net in (forward, backward):
        net.start("cup")
        net.jolt("group", 40)
        net.jolt("opposite", 20)
        net.jolt("end", 40)
        for _ in range(25):
            net.update()
            net.jolt("cup", 20)
    assert dict(forward.activations()) == pytest.approx(dict(backward.activations()))
    assert forward.distance("cup", "small glass") == pytest.approx(backward.distance("cup", "small glass"))
    assert forward.distance("knife", "fork") == pytest.approx(backward.distance("knife", "fork"))
    assert forward.labels_between("knife", "fork") == backward.labels_between("knife", "fork")
    assert sorted(forward.categories_of("cup")) == sorted(backward.categories_of("cup"))   # table order
    assert forward.effective_length(forward.links[3]) == backward.effective_length(backward.links[-4])

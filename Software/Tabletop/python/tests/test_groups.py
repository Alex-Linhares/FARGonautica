"""Gestalt groups by proximity (D-022, D-023): the link probability, the flood fill, the
Blockage 6-10 glasses over seeds against French's words [F95 pp. 128-129], and what a
find-group does on a layout.
"""

import math

import pytest

from tabletop import groups as GR
from tabletop import objects as O
from tabletop import problems as P
from tabletop.config import PROFILES
from tabletop.rng import Rng
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
WHOLE_ROW_SEED = 1  # a seed whose coins link the whole row (0.93 each; see the tests below)


@pytest.fixture(scope="module")
def problems():
    return {p.name: p for p in P.load_all()}


# ---------------------------------------------------------------------------------------------
# The proximity criterion (D-022).


def test_link_probability_is_one_up_to_half_a_width_and_zero_from_two_and_a_half():
    """D-022: p = 1 for g <= 0.5, falling linearly to 0 at 2.5, 0 beyond."""
    assert GR.link_probability(0.0, CFG) == 1.0
    assert GR.link_probability(0.5, CFG) == 1.0
    assert GR.link_probability(1.0, CFG) == pytest.approx(0.75)
    assert GR.link_probability(1.5, CFG) == pytest.approx(0.5)
    assert GR.link_probability(2.5, CFG) == 0.0
    assert GR.link_probability(3.0, CFG) == 0.0


def test_blockage_10s_gap_is_never_linked_under_any_profile():
    """D-118 (v5): French's Blockage 10, its glasses 2.64 widths apart on the digitized
    figure, is the pair "the program no longer perceives" as a group [F95 p. 129]: the
    link probability at that gap is 0 under every profile, since no `Config` may set
    `proximity_zero_gap` above the bound."""
    for cfg in PROFILES.values():
        assert GR.link_probability(2.64, cfg) == 0.0
        assert GR.link_probability(2.6, cfg) == 0.0
        assert GR.link_probability(2.5, cfg) > 0.0 or cfg.proximity_zero_gap <= 2.5
    assert GR.link_probability(2.64, CFG) == 0.0          # v2's 2.5 as well


def test_a_certain_link_spends_no_draw_and_an_uncertain_one_spends_one(problems):
    """The coin is drawn only when the probability is strictly between 0 and 1 (D-022)."""
    s5 = problems["surround-05"]
    rng = Rng(1)
    assert GR.linked(s5.object("fork1"), s5.object("fork2"), rng, CFG) is True   # 0.3 widths
    assert GR.linked(s5.object("fork1"), s5.object("cup2"), rng, CFG) is False  # far apart
    assert rng.draws == 0
    b8 = problems["blockage-08"]
    GR.linked(b8.object("small-glass2"), b8.object("small-glass4"), rng, CFG)   # 1.27 widths
    assert rng.draws == 1


# ---------------------------------------------------------------------------------------------
# The flood fill (D-023).


def test_the_flood_fill_finds_henrys_row_whole_most_of_the_time(problems):
    """The first find-group of chapter 4 found the whole group of six on Henry's side at
    once, "a bunch of objects that are physically close together" [F95 p. 97] (D-023). The
    row's gaps are 0.3 to 0.64 glass widths, so under D-022 four of the five links are
    coins at about 0.93: from fork1 the component is always a run of the row starting at
    fork1, and it is the whole row about p^4 of the time, p the product of the four link
    probabilities (seeded; four binomial standard deviations)."""
    s5 = problems["surround-05"]
    p = 1.0
    for a, b in zip(HENRY_ROW, HENRY_ROW[1:]):
        p *= GR.link_probability(O.gap_in_glass_widths(O.edge_gap(s5.object(a), s5.object(b))), CFG)
    assert 0.5 < p < 1.0
    n, whole = 200, 0
    for seed in range(n):
        component = GR.proximity_component(s5, "fork1", Rng(seed), CFG)
        assert component == HENRY_ROW[:len(component)]
        whole += component == HENRY_ROW
    sd = math.sqrt(n * p * (1 - p))
    assert abs(whole - n * p) <= 4 * sd, (whole, p)


def test_from_a_middle_seed_the_component_is_a_run_around_the_seed(problems):
    """A flood fill from cup1 gives a contiguous run of Henry's row containing cup1, in file
    order (D-023)."""
    s5 = problems["surround-05"]
    for seed in range(50):
        component = GR.proximity_component(s5, "cup1", Rng(seed), CFG)
        start = HENRY_ROW.index(component[0])
        assert component == HENRY_ROW[start:start + len(component)]
        assert "cup1" in component


def test_the_flood_fill_returns_members_in_file_order_and_the_lone_cup_alone(problems):
    """Members come back in file order whatever the seed object; the isolated cup of
    Surround 5 is a component of one (D-022, D-023)."""
    s5 = problems["surround-05"]
    eliza = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]
    component = GR.proximity_component(s5, "knife2", Rng(3), CFG)
    assert component == [m for m in eliza if m in component]
    assert "knife2" in component
    assert GR.proximity_component(s5, "cup2", Rng(3), CFG) == ["cup2"]


def test_the_flood_fill_draws_the_same_coins_for_the_same_seed(problems):
    """Reproducibility: the same seed gives the same component and the same draw count
    (D-064)."""
    b9 = problems["blockage-09"]
    runs = []
    for _ in range(3):
        rng = Rng(11)
        runs.append((GR.proximity_component(b9, "small-glass2", rng, CFG), rng.draws))
    assert runs[0] == runs[1] == runs[2]


# Blockage 6-10 over seeds (D-022), against French's words. The counts are seeded; the
# bounds are four binomial standard deviations around n p, with p the engine's own link
# probability for the file's gap, so a correct implementation fails about once in 16,000.
N_SEEDS = 200


def _grouped_count(problem, n=N_SEEDS):
    grouped = 0
    for seed in range(n):
        component = GR.proximity_component(problem, "small-glass2", Rng(seed), CFG)
        grouped += "small-glass4" in component
    return grouped


def test_blockage_10_never_groups_the_two_glasses(problems):
    """"The gap in Blockage 10 is so wide that the program no longer perceives the glasses as
    forming a group." [F95 p. 129] The file's gap is 2.64 widths, past `proximity_zero_gap`,
    so the glasses are never linked, from any seed."""
    b10 = problems["blockage-10"]
    gap = O.gap_in_glass_widths(O.edge_gap(b10.object("small-glass2"), b10.object("small-glass4")))
    assert gap > CFG.proximity_zero_gap
    assert _grouped_count(b10) == 0


def test_blockage_6_always_groups_the_two_glasses(problems):
    """In Blockage 6 the two glasses touch (0.49 widths, under `proximity_full_gap`), so they
    are always one gestalt group (D-022)."""
    assert _grouped_count(problems["blockage-06"]) == N_SEEDS


def test_blockage_8_and_9_group_the_glasses_less_and_less_often(problems):
    """The group forms "less and less often" as the gap grows by a glass width at a time
    [F95 p. 128] (D-022): the frequencies fall from Blockage 6 to 8 to 9 to 10, each within
    four binomial standard deviations of n times the engine's link probability."""
    counts = {}
    for name in ("blockage-08", "blockage-09"):
        problem = problems[name]
        gap = O.gap_in_glass_widths(O.edge_gap(problem.object("small-glass2"),
                                               problem.object("small-glass4")))
        p = GR.link_probability(gap, CFG)
        assert 0 < p < 1
        counts[name] = _grouped_count(problem)
        sd = math.sqrt(N_SEEDS * p * (1 - p))
        assert abs(counts[name] - N_SEEDS * p) <= 4 * sd, (name, counts[name], p)
    assert N_SEEDS > counts["blockage-08"] > counts["blockage-09"] > 0


# ---------------------------------------------------------------------------------------------
# What a find-group does (D-023).


def test_find_group_around_records_a_gestalt_group_and_jolts_group(problems):
    """"(1) sends activation to the concept 'group' in the Slipnet" [F95 p. 65]; the group
    is named g1, is a gestalt group with no parent, and is in the Workspace (D-016, D-023)."""
    world = World(problems["surround-05"], CFG, WHOLE_ROW_SEED)
    before = world.slipnet.activation_of("group")
    group = GR.find_group_around(world, "fork1")
    assert group is not None
    assert group.name == "g1" and group.kind == GR.GESTALT and group.parent is None
    assert group.members == HENRY_ROW
    assert world.workspace.groups == [group]
    assert world.slipnet.activation_of("group") == pytest.approx(before + CFG.jolt_structure)
    assert group.ends is None and group.middles is None and group.orientation is None


def test_the_same_component_found_again_is_nothing_new(problems):
    """A component "not already in the Workspace" is proposed; the same one again is not
    (D-023), and no jolt is sent."""
    world = World(problems["surround-05"], CFG, WHOLE_ROW_SEED)
    first = GR.find_group_around(world, "fork1")
    assert first.members == HENRY_ROW
    after = world.slipnet.activation_of("group")
    world.rng = Rng(WHOLE_ROW_SEED)  # the same coins again
    assert GR.find_group_around(world, "fork1") is None
    assert world.workspace.groups == [first]
    assert world.slipnet.activation_of("group") == after


def test_a_component_of_one_marks_the_seed_isolated_and_jolts_isolated(problems):
    """An object with no link "is noticed as isolated (a jolt to 'isolated', D-007)"
    (D-022); once only."""
    world = World(problems["surround-05"], CFG, 1)
    before = world.slipnet.activation_of("isolated")
    assert GR.find_group_around(world, "cup2") is None
    assert world.workspace.isolated == ["cup2"]
    assert world.workspace.has("cup2", "isolated")
    assert world.slipnet.activation_of("isolated") == pytest.approx(before + CFG.jolt_description)
    assert GR.find_group_around(world, "cup2") is None
    assert world.slipnet.activation_of("isolated") == pytest.approx(before + CFG.jolt_description)
    assert world.workspace.groups == []


def test_groups_compare_by_identity_not_by_value():
    """Two groups with the same members are two structures (D-023)."""
    a = GR.Group(members=["fork1", "fork2"])
    b = GR.Group(members=["fork1", "fork2"])
    assert a != b and a == a
    assert a.size() == 2 and not a.chain_known()

"""The World and the Workspace (D-064, D-066; D-023, D-029) and scanning (D-004, D-023,
D-032): a fresh world per run, objects becoming known by looking, descriptions, the group
registry, the region preferences and the look at a region.
"""

import math

import pytest

from tabletop import geometry as G
from tabletop import groups as GR
from tabletop import perceive as PV
from tabletop import problems as P
from tabletop.config import PROFILES
from tabletop.world import World
from tabletop.workspace import Workspace
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]


@pytest.fixture(scope="module")
def problems():
    return {p.name: p for p in P.load_all()}


# ---------------------------------------------------------------------------------------------
# The World (D-064, D-066).


def test_a_world_starts_knowing_only_the_touched_object(problems):
    """"Initially, the program explicitly knows only the location and object type of the
    touched object." [F95 p. 64] It is noticed in its exact region, its type is its one
    descriptor, and nothing else is in view."""
    world = World(problems["surround-05"], CFG, 1)
    assert world.workspace.noticed == ["cup1"]
    assert world.workspace.regions == {"cup1": "upper-left"}
    assert world.workspace.descriptors("cup1") == ["cup"]
    assert world.workspace.groups == [] and world.workspace.isolated == []
    assert world.clock == 0 and world.seed == 1


def test_the_slipnet_is_started_on_the_touched_type(problems):
    """D-017: "opposite" and the touched object's type are the two active nodes after the
    start's one update; a Buridan world starts on "spoon"."""
    world = World(problems["surround-05"], CFG, 1)
    top = sorted(world.slipnet.activations(), key=lambda pair: -pair[1])[:2]
    assert [name for name, _ in top] == ["opposite", "cup"]
    buridan = World(problems["buridan-01"], CFG, 1)
    assert buridan.slipnet.activation_of("spoon") > 40
    assert buridan.slipnet.activation_of("cup") < 5


def test_two_worlds_share_nothing(problems):
    """A fresh world per run (D-064): building structures in one leaves the other untouched,
    and the same seed gives the same draws."""
    a = World(problems["surround-05"], CFG, 5)
    b = World(problems["surround-05"], CFG, 5)
    GR.find_group_around(a, "fork1")
    a.slipnet.jolt("end", 50)
    a.workspace.describe("cup1", "end")
    assert b.workspace.groups == [] and b.slipnet.activation_of("end") == 1.0
    assert b.workspace.descriptors("cup1") == ["cup"]
    GR.find_group_around(b, "fork1")
    assert a.rng.draws == b.rng.draws
    assert PV.look(a, G.region_named("lower-left")) == PV.look(b, G.region_named("lower-left"))
    assert a.rng.draws == b.rng.draws


def test_the_touched_object_draws_no_region_when_far_from_every_line(problems):
    """The start spends no random draw on a touched object far from the lines (D-004)."""
    world = World(problems["surround-05"], CFG, 1)
    assert world.rng.draws == 0


# ---------------------------------------------------------------------------------------------
# The Workspace: objects in view and descriptions (D-004, D-029).


def test_notice_records_the_first_look_once_and_the_region_every_time(problems):
    """The first look adds the object and its type; later looks only update the region
    (D-004, D-029)."""
    world = World(problems["surround-05"], CFG, 1)
    ws = world.workspace
    region = ws.notice("spoon3", world.rng, by="test")
    assert ws.noticed == ["cup1", "spoon3"]
    assert ws.regions["spoon3"] == region.name == "lower-left"
    assert ws.descriptors("spoon3") == ["spoon"]
    ws.notice("spoon3", world.rng, by="test", region=G.region_named("middle"))
    assert ws.noticed == ["cup1", "spoon3"]
    assert ws.regions["spoon3"] == "middle"
    assert ws.descriptors("spoon3") == ["spoon"]
    assert ws.is_noticed("spoon3") and not ws.is_noticed("fork1")


def test_an_object_near_a_line_is_sometimes_noticed_across_it(problems):
    """"may sometimes be considered by the program to be in the adjacent region" [F95 p. 65]:
    Surround 5's spoon1 (x 38.8, 2.1 units right of the first column line) is noticed in the
    upper left in some looks and the upper middle in others (D-004); seeded."""
    seen = set()
    for seed in range(60):
        world = World(problems["surround-05"], CFG, seed)
        seen.add(world.workspace.notice("spoon1", world.rng).name)
    assert seen == {"upper-left", "upper-middle"}


def test_descriptions_are_ordered_unique_and_copied(problems):
    """"attaches an appropriate descriptor to the object concerned" [F95 p. 75] (D-029)."""
    ws = Workspace(problems["surround-05"], CFG)
    assert ws.describe("fork1", "fork") is True
    assert ws.describe("fork1", "end") is True
    assert ws.describe("fork1", "fork") is False
    assert ws.descriptors("fork1") == ["fork", "end"]
    assert ws.has("fork1", "end") and not ws.has("fork1", "middle")
    ws.descriptors("fork1").append("junk")
    assert ws.descriptors("fork1") == ["fork", "end"]
    assert ws.descriptors("nothing") == []


def test_mark_isolated_once(problems):
    ws = Workspace(problems["surround-05"], CFG)
    assert ws.mark_isolated("cup2") is True
    assert ws.mark_isolated("cup2") is False
    assert ws.isolated == ["cup2"] and ws.has("cup2", "isolated")


# ---------------------------------------------------------------------------------------------
# The group registry (D-023, D-026).


def test_groups_are_named_in_order_and_found_by_members(problems):
    ws = Workspace(problems["surround-05"], CFG)
    a = GR.Group(members=["fork1", "fork2"])
    b = GR.Group(members=["fork1", "fork2", "knife1"])
    assert ws.add_group(a) == "g1" and ws.add_group(b) == "g2"
    assert ws.group_named("g2") is b
    assert ws.group_with_members(["fork2", "fork1"]) is a
    assert ws.group_with_members(["fork1"]) is None
    assert ws.groups_containing("fork1") == [a, b]
    assert ws.groups_containing("cup1") == []
    assert ws.innermost_group("fork1") is a
    assert ws.innermost_group("knife1") is b
    assert ws.innermost_group("cup1") is None
    with pytest.raises(KeyError):
        ws.group_named("g9")


def test_a_removed_groups_name_is_never_reused(problems):
    ws = Workspace(problems["surround-05"], CFG)
    a = GR.Group(members=["fork1", "fork2"])
    ws.add_group(a)
    ws.remove_group(a)
    assert ws.groups == []
    assert ws.add_group(GR.Group(members=["spoon1", "spoon2"])) == "g2"


def test_items_lists_noticed_objects_then_groups(problems):
    world = World(problems["surround-05"], CFG, 1)
    world.workspace.notice("cup2", world.rng)
    g = GR.find_group_around(world, "fork1")
    assert world.workspace.items() == ["cup1", "cup2", g.name]


# ---------------------------------------------------------------------------------------------
# Scanning (D-023, D-032).


def test_the_region_preferences_of_a_surround_layout(problems):
    """"the most favored region is diagonally opposite the touched object, and the next most
    preferred region is directly opposite the touched object" [F95 p. 66] (D-032): for a
    cup in the upper left, the lower right 1.0, the lower left 0.7, the three neighbours and
    the region itself 0.4, the rest 0.15."""
    world = World(problems["surround-05"], CFG, 1)
    assert PV.touched_region(world).name == "upper-left"
    weights = {region.name: w for region, w in PV.region_preferences(world)}
    assert weights["lower-right"] == 1.0 and weights["lower-left"] == 0.7
    assert {weights[n] for n in ("upper-left", "upper-middle", "middle-left", "middle")} == {0.4}
    assert {weights[n] for n in ("upper-right", "middle-right", "lower-middle")} == {0.15}
    assert [r.name for r, _ in PV.region_preferences(world)] == [r.name for r in G.REGIONS]


def test_the_choice_of_region_is_sharp_at_a_6_and_flat_at_a_0_1(problems):
    """"approaches determinism as the temperature approaches 0, is completely unbiased (i.e.,
    uniformly distributed random) at 100" [F95 p. 93] (D-060): over 300 seeds the diagonal
    corner is chosen about 88 percent of the time at a = 6 (1 against 0.7^6 + 4 x 0.4^6 +
    3 x 0.15^6) and about a ninth of the time at a = 0.1; bounds of four binomial standard
    deviations."""
    n = 300
    for a, expected in ((6.0, 1 / (1 + 0.7 ** 6 + 4 * 0.4 ** 6 + 3 * 0.15 ** 6)),
                        (0.1, 1 / (1 + 0.7 ** 0.1 + 4 * 0.4 ** 0.1 + 3 * 0.15 ** 0.1))):
        diagonal = 0
        for seed in range(n):
            world = World(problems["surround-05"], CFG, seed)
            diagonal += PV.choose_region(world, a).name == "lower-right"
        sd = math.sqrt(n * expected * (1 - expected))
        assert abs(diagonal - n * expected) <= 4 * sd, (a, diagonal, expected)


def test_a_look_notices_the_objects_seen_in_the_region_and_jolts_their_types(problems):
    """The scouts report "what objects are in interesting areas" [FCCA p. 386] (D-023):
    looking at the lower right of Surround 5 finds the isolated cup, and the first look
    jolts its type (D-016); a region with nothing in it returns nothing."""
    world = World(problems["surround-05"], CFG, 1)
    cup_before = world.slipnet.activation_of("cup")
    assert PV.look(world, G.region_named("lower-right")) == ["cup2"]
    assert world.workspace.noticed == ["cup1", "cup2"]
    assert world.workspace.regions["cup2"] == "lower-right"
    assert world.slipnet.activation_of("cup") == pytest.approx(cup_before + CFG.jolt_description)
    assert PV.look(world, G.region_named("lower-right")) == ["cup2"]
    assert world.slipnet.activation_of("cup") == pytest.approx(cup_before + CFG.jolt_description)
    assert PV.look(world, G.region_named("middle")) == []


def test_a_look_returns_ids_in_file_order_and_sees_the_blur(problems):
    """Over seeds, the upper middle of Surround 5 shows spoon2 always (x 45.1, far from the
    line) and spoon1 (2.1 units past it) most of the time; ids come in file order (D-004,
    D-023)."""
    with_spoon1 = 0
    for seed in range(60):
        world = World(problems["surround-05"], CFG, seed)
        seen = PV.look(world, G.region_named("upper-middle"))
        assert "spoon2" in seen
        assert seen in (["spoon2"], ["spoon1", "spoon2"])
        with_spoon1 += "spoon1" in seen
    assert 0 < with_spoon1 < 60

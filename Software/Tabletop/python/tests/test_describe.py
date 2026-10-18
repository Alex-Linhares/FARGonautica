"""Describing groups (D-027 to D-029): the chain of neighbours of p. 80 on Surround 5, ends
and middles with left and right, the L-shaped group under the fix and under French's
reading, orientation, counts, and the two frames of position.
"""

import pytest

from tabletop import describe as DS
from tabletop import groups as GR
from tabletop import problems as P
from tabletop.config import PROFILES
from tabletop.world import World
from v2_defaults import V2  # the mechanism at its decision's values (D-114, D-115)

# The mechanism at its decision's values (D-114, D-115, D-118): the jolts, the ends and the
# gaps of the describing tests are read at the a-priori defaults, where Henry's row is found
# whole from fork1 by seed 1 (v5's bounded zero gap, D-118, finds part of it).
CFG = V2
FAULTS = PROFILES["french-faults"]

HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]


@pytest.fixture(scope="module")
def problems():
    return {p.name: p for p in P.load_all()}


def synthetic(objects, touched):
    """A problem built in the test: objects as (id, type, x, y)."""
    prov = P.Provenance(figure="synthetic", page=0, image="none")
    table = tuple(P.TableObject(i, t, x, y, None, (0, 0, 0, 0), prov) for i, t, x, y in objects)
    return P.Problem(name="synthetic", title="synthetic", family="test", source={},
                     width=110.0, height=100.0, frame_px=(0, 0, 0, 0), touched=touched,
                     answers={}, objects=table, method="", uncertainty={}, notes=(), review="")


def henry_group(problems, config=V2, seed=1):
    # Henry's row found whole from fork1 at these values (D-022's a-priori gaps, under
    # which seed 1 links every pair); on v5's bounded zero gap (D-118) seed 1 finds part of
    # the row, so the describing tests, which are about the row once found, run on V2.
    world = World(problems["surround-05"], config, seed)
    return world, GR.find_group_around(world, "fork1")


# An L-shaped group: a row of four and an arm of two hanging from its right end. Chapter
# 2's Figure 2.10 shows such a shape [F95 p. 29 (img)]; this one is synthetic.
L_SHAPE = [("a", "cup", 10.0, 10.0), ("b", "cup", 20.0, 10.0), ("c", "cup", 30.0, 10.0),
           ("f", "cup", 40.0, 10.0), ("d", "cup", 40.0, 25.0), ("e", "cup", 40.0, 40.0)]


def l_world(config=CFG):
    world = World(synthetic(L_SHAPE, "a"), config, 1)
    group = GR.Group(members=[i for i, _, _, _ in L_SHAPE])
    world.workspace.add_group(group)
    return world, group


# ---------------------------------------------------------------------------------------------
# Geometry (D-028).


def test_axis_and_linearity_of_henrys_row_and_of_a_column(problems):
    """"the objects in it seem to be arranged horizontally" [F95 p. 97]; a column is
    vertical; both are lines (D-028)."""
    s5 = problems["surround-05"]
    assert DS.axis_of(s5, HENRY_ROW) == DS.HORIZONTAL
    assert DS.is_linear(s5, HENRY_ROW, DS.HORIZONTAL)
    column = synthetic([("a", "cup", 50.0, 10.0), ("b", "cup", 50.0, 20.0), ("c", "cup", 51.0, 30.0)], "a")
    assert DS.axis_of(column, ["a", "b", "c"]) == DS.VERTICAL
    assert DS.is_linear(column, ["a", "b", "c"], DS.VERTICAL)
    assert DS.spread(column, ["a", "b", "c"]) == (1.0, 20.0)
    assert DS.centroid(column, ["a", "b", "c"]) == pytest.approx((50.333, 20.0), abs=0.01)


def test_an_l_is_amorphous_and_a_tie_reads_as_horizontal():
    """"Tabletop cannot handle completely amorphous groups." [F95 p. 63] The L's spread is
    30 by 30: a tie, read as horizontal, and not a line (D-028)."""
    problem = synthetic(L_SHAPE, "a")
    members = [i for i, _, _, _ in L_SHAPE]
    assert DS.axis_of(problem, members) == DS.HORIZONTAL
    assert not DS.is_linear(problem, members, DS.HORIZONTAL)


def test_side_of_by_the_dominant_displacement(problems):
    """"to the right and left" [F95 p. 97], or above and below (D-027)."""
    s5 = problems["surround-05"]
    assert DS.side_of(s5.object("fork1"), s5.object("fork2")) == "right"
    assert DS.side_of(s5.object("fork2"), s5.object("fork1")) == "left"
    assert DS.side_of(s5.object("fork1"), s5.object("spoon3")) == "below"
    assert DS.side_of(s5.object("spoon3"), s5.object("fork1")) == "above"
    assert DS.side_of(s5.object("cup1"), s5.object("cup2")) == "below"


def test_nearest_on_each_side_in_a_row(problems):
    """In a linear group only the two sides along its axis are occupied (D-027)."""
    world, group = henry_group(problems)
    assert DS.nearest_on_sides(world.problem, group, "fork2") == [("fork1", "left"), ("knife1", "right")]
    assert DS.nearest_on_sides(world.problem, group, "fork1") == [("fork2", "right")]
    assert DS.nearest_on_sides(world.problem, group, "spoon2") == [("spoon1", "left")]


# ---------------------------------------------------------------------------------------------
# The chain of p. 80 (D-027).


def test_the_chain_of_find_neighbor_codelets_on_henrys_row(problems):
    """"the first find-neighbor codelet might have as its parameter fork1. It would find
    fork2 and spawn the codelet find-neighbor(fork2)." [F95 p. 80], which finds knife1,
    "and so on" (D-027): each step returns only the new relation, the relation is recorded
    both ways, "neighbor" is jolted on every new relation, and the chain is known when the
    last member has been visited."""
    world, group = henry_group(problems)
    before = world.slipnet.activation_of("neighbor")
    assert DS.find_neighbours(world, group, "fork1") == [("fork2", "right")]
    assert group.neighbours_of("fork2") == [("fork1", "left")]
    assert world.slipnet.activation_of("neighbor") == pytest.approx(before + CFG.jolt_structure)
    assert DS.find_neighbours(world, group, "fork2") == [("knife1", "right")]
    assert DS.find_neighbours(world, group, "knife1") == [("cup1", "right")]
    assert DS.find_neighbours(world, group, "cup1") == [("spoon1", "right")]
    assert DS.find_neighbours(world, group, "spoon1") == [("spoon2", "right")]
    assert not group.chain_known()
    after = world.slipnet.activation_of("neighbor")
    assert DS.find_neighbours(world, group, "spoon2") == []
    assert world.slipnet.activation_of("neighbor") == after
    assert group.chain_known()
    assert group.neighbours_of("knife1") == [("fork2", "left"), ("cup1", "right")]
    assert group.visited == HENRY_ROW


def test_find_neighbours_from_the_middle_finds_both_sides(problems):
    world, group = henry_group(problems)
    assert DS.find_neighbours(world, group, "cup1") == [("knife1", "left"), ("spoon1", "right")]
    assert DS.find_neighbours(world, group, "cup1") == []
    assert group.neighbours_of("spoon1") == [("cup1", "left")]


# ---------------------------------------------------------------------------------------------
# Ends and middles (D-028).


def test_ends_and_middles_of_henrys_row_with_left_and_right(problems):
    """"on the left- and right-hand ends of the group, respectively, which explains why
    'left' and 'right' (and not 'above' and 'below') are active" [F95 p. 101]; find-end
    "sends activation to that category in the Slipnet and also to the concept 'end'" [F95
    p. 63] (D-028, D-029)."""
    world, group = henry_group(problems)
    sn = world.slipnet
    before = {n: sn.activation_of(n) for n in ("end", "left", "right", "above", "fork", "spoon", "middle")}
    assert DS.find_ends(world, group) == ["fork1", "spoon2"]
    assert group.ends == ["fork1", "spoon2"]
    assert DS.group_frame(group, "fork1") == ["end", "left"]
    assert DS.group_frame(group, "spoon2") == ["end", "right"]
    assert world.workspace.descriptors("fork1") == ["end", "left"]
    assert sn.activation_of("end") == pytest.approx(before["end"] + 2 * CFG.jolt_description)
    assert sn.activation_of("left") == pytest.approx(before["left"] + CFG.jolt_description)
    assert sn.activation_of("right") == pytest.approx(before["right"] + CFG.jolt_description)
    assert sn.activation_of("above") == before["above"]
    assert sn.activation_of("fork") == pytest.approx(before["fork"] + 2 * CFG.jolt_description)
    assert DS.find_middles(world, group) == ["fork2", "knife1", "cup1", "spoon1"]
    assert group.middles == ["fork2", "knife1", "cup1", "spoon1"]
    assert DS.group_frame(group, "cup1") == ["middle"]
    assert sn.activation_of("middle") == pytest.approx(before["middle"] + 4 * CFG.jolt_description)
    assert DS.find_ends(world, group) == ["fork1", "spoon2"]
    assert sn.activation_of("end") == pytest.approx(before["end"] + 2 * CFG.jolt_description)


def test_middles_can_be_found_before_the_ends(problems):
    """find-end-object and find-middle-object have "roughly the same urgency" [F95 p. 79]
    and either may run first (D-028)."""
    world, group = henry_group(problems)
    assert DS.find_middles(world, group) == ["fork2", "knife1", "cup1", "spoon1"]
    assert group.ends is None
    assert world.workspace.has("knife1", "middle")


def test_ends_before_the_chain_is_known_are_the_extremes_of_the_axis(problems):
    """Before any neighbour is found the ends are the extremes along the axis; on a row the
    two readings agree (D-028)."""
    world, group = henry_group(problems)
    assert DS.ends_of(world.problem, group, CFG) == ["fork1", "spoon2"]
    for m in HENRY_ROW:
        DS.find_neighbours(world, group, m)
    assert DS.ends_of(world.problem, group, CFG) == ["fork1", "spoon2"]


def test_an_l_gets_an_end_on_each_arm_once_its_chain_is_known():
    """FIXED (ANSWERS 2), D-028: with the chain known, the members with exactly one
    neighbour are the two extremities of the L; before it is known, the extremes along the
    axis give the row's two ends; the L gets no left or right word, being amorphous."""
    world, group = l_world()
    assert DS.ends_of(world.problem, group, CFG) == ["a", "f"]
    for m in group.members:
        DS.find_neighbours(world, group, m)
    assert group.chain_known()
    assert group.neighbours_of("f") == [("c", "left"), ("d", "below")]
    assert group.neighbours_of("d") == [("f", "above"), ("e", "below")]
    assert DS.find_ends(world, group) == ["a", "e"]
    assert DS.group_frame(group, "a") == ["end"] and DS.group_frame(group, "e") == ["end"]
    assert DS.find_middles(world, group) == ["b", "c", "f", "d"]


def test_french_reading_of_an_l_chooses_only_one_upper_end_object():
    """"the program will choose only one as the upper end object" [F95 p. 63]: under
    `french-faults` (`ends_from_chain` False) the ends stay the extremes along the axis
    whatever the chain says, and the arm's extremity is never an end (D-028, fault 7)."""
    world, group = l_world(FAULTS)
    for m in group.members:
        DS.find_neighbours(world, group, m)
    assert DS.find_ends(world, group) == ["a", "f"]
    assert "e" not in group.ends


# ---------------------------------------------------------------------------------------------
# Orientation, counts and the two frames (D-028, D-029).


def test_orientation_is_a_descriptor_of_the_group_and_jolts_its_node(problems):
    """"thus activating the concept 'horizontal'" [F95 p. 97] (D-028)."""
    world, group = henry_group(problems)
    before = world.slipnet.activation_of("horizontal")
    assert DS.notice_orientation(world, group) == DS.HORIZONTAL
    assert group.orientation == DS.HORIZONTAL and group.linear is True
    assert world.workspace.descriptors(group.name) == ["horizontal"]
    assert world.slipnet.activation_of("horizontal") == pytest.approx(before + CFG.jolt_description)
    assert DS.notice_orientation(world, group) == DS.HORIZONTAL
    assert world.slipnet.activation_of("horizontal") == pytest.approx(before + CFG.jolt_description)
    assert world.slipnet.activation_of("vertical") == 1.0


def test_count_descriptors_one_two_three_many(problems):
    """"one, if it is a simple object; otherwise, the size of the group" [F95 p. 68]."""
    assert [DS.count_descriptor(n) for n in (1, 2, 3, 4, 9)] == ["one", "two", "three", "many", "many"]
    world, group = henry_group(problems)
    assert DS.describe_count(world, "cup1") == "one"
    assert DS.describe_count(world, group.name) == "many"
    assert world.workspace.has("cup1", "one") and world.workspace.has(group.name, "many")
    assert world.slipnet.activation_of("many") == pytest.approx(1 + CFG.jolt_description)
    pair = GR.Group(members=["fork1", "fork2"])
    world.workspace.add_group(pair)
    assert DS.describe_count(world, pair.name) == "two"


def test_the_two_frames_of_position(problems):
    """"the descriptions of the positions within their respective groups" [F95 p. 68]
    against "the sides of the table as a whole" [F95 p. 85] (D-029): spoon2 is the right end
    of its group but in the table's middle column; the lone cup is right and below; the
    group lies left and above."""
    world, group = henry_group(problems)
    DS.find_ends(world, group)
    assert DS.group_frame(group, "spoon2") == ["end", "right"]
    assert DS.table_frame(world, "spoon2") == ("middle", "above")
    assert DS.table_frame(world, "cup1") == ("left", "above")
    assert DS.table_frame(world, "cup2") == ("right", "below")
    assert DS.table_frame(world, group.name) == ("left", "above")
    assert DS.group_frame(group, "cup2") == []


def test_the_table_frame_is_drawn_each_time_it_is_read(problems):
    """spoon1 sits 2.1 units past the first column line, so its table column is `left` in
    some readings and `middle` in others (D-004, D-029); seeded. On the v2 defaults, D-004's
    blur (width 4, at most 0.5): under v4's (1.73, 0.072) the crossing has a chance of about
    0.02 per reading, and none of these 60 seeds draws it (D-115)."""
    columns = set()
    for seed in range(60):
        world = World(problems["surround-05"], V2, seed)
        columns.add(DS.table_frame(world, "spoon1")[0])
    assert columns == {"left", "middle"}


def test_a_full_description_pass_is_reproducible_from_the_seed(problems):
    """Same seed, same structures and same draws (D-064)."""
    def pass_(seed):
        world, group = henry_group(problems, seed=seed)
        DS.notice_orientation(world, group)
        for m in group.members:
            DS.find_neighbours(world, group, m)
        DS.find_ends(world, group)
        DS.find_middles(world, group)
        frame = DS.table_frame(world, "spoon1")
        return (group.neighbours, group.ends, group.middles, group.positions,
                world.workspace.descriptions, frame, world.rng.draws,
                [round(a, 3) for a in world.slipnet.activation])
    assert pass_(4) == pass_(4)

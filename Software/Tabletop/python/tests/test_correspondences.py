"""Correspondences (D-030, D-035 to D-039, D-045, D-046): the creation conditions [F95
pp. 71-72], the slippages and identities [F95 p. 68], each of the nine static factors in its
direction [F95 pp. 68-69] with the p. 68 number example, the three dynamic factors [F95
p. 69], the bonus and the penalty [F95 p. 68], the probabilistic test [F95 p. 72], and the
two biases French calls too strong under `standard` and `french-faults` [F95 pp. 109, 125].

The statistical tests are seeded; their bounds are four binomial standard deviations of
the law's own probability (D-060), stated beside each.
"""

import dataclasses
import math

import pytest

from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import geometry as G
from tabletop import groups as GR
from tabletop import problems as P
from tabletop import salience as S
from tabletop import temperature as TP
from tabletop import worldview as WV
from tabletop.config import PROFILES
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
FAULTS = V2_PROFILES["french-faults"]
HUMAN = V2_PROFILES["human"]
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
ELIZA_ROW = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]
A_COLD, A_HOT = TP.exponent_at(0, CFG), TP.exponent_at(100, CFG)   # 6 and 0.1 [F95 p. 92]
W = dict(CFG.cp_static_weights)


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


def notice_all(world):
    for obj in world.problem.objects:
        world.workspace.notice(obj.id, world.rng, region=G.exact_region(obj.x, obj.y))


def described_group(world, members, kind=None):
    """A group placed directly with its chain known, its ends, middles and orientation
    found (D-027, D-028); a bottom-up kind when given."""
    if kind is None:
        group = GR.Group(members=list(members))
    else:
        k, node = GR.kind_of(world.problem, members, world.config)
        group = GR.Group(members=list(members), kind=k, category=node)
    world.workspace.add_group(group)
    frontier = [members[0]]
    while frontier:
        for n, _ in DS.find_neighbours(world, group, frontier.pop(0)):
            frontier.append(n)
    assert group.chain_known()
    DS.find_ends(world, group)
    DS.find_middles(world, group)
    DS.notice_orientation(world, group)
    return group


def two_rows(problems, config=CFG, seed=1):
    """Surround 5 with every object in view and both rows as described gestalt groups
    (`g1` Henry's, `g2` Eliza's); the isolated cup is in no group."""
    world = World(problems["surround-05"], config, seed)
    notice_all(world)
    henry = described_group(world, HENRY_ROW)
    eliza = described_group(world, ELIZA_ROW)
    return world, henry, eliza


def term(world, cp, name):
    return dict(C.static_terms(world, cp))[name]


# ---------------------------------------------------------------------------------------------
# The structure and the creation conditions (D-035).


def test_a_correspondence_is_defined_by_its_ends_and_named_in_order(problems):
    world, _, _ = two_rows(problems)
    cp1 = C.create(world, "cup1", "cup2")
    cp2 = C.create(world, "fork1", "knife2")
    assert cp1.ends == ("cup1", "cup2") and cp1.name == "cp1" and cp2.name == "cp2"
    assert cp1.label() == "cp1 cup1=cup2"
    assert world.workspace.correspondences == [cp1, cp2]
    assert world.workspace.correspondence_named("cp2") is cp2
    assert world.workspace.correspondence_between("cup2", "cup1") is cp1
    assert world.workspace.correspondences_of("cup1") == [cp1]
    assert cp1.level == "top" and not cp1.in_worldview and not cp1.tested
    assert cp1.by == "create-cp" and cp1.built_at == 0


def test_correspondences_compare_by_identity_and_names_are_never_reused(problems):
    world, _, _ = two_rows(problems)
    cp1 = C.create(world, "cup1", "cup2")
    other = C.Correspondence(henry="cup1", eliza="cup2")
    assert cp1 != other and cp1 is not other
    world.workspace.remove_correspondence(cp1)
    assert world.workspace.correspondences == []
    assert C.create(world, "cup1", "cup2").name == "cp2"


def test_the_same_item_and_a_pair_already_mapped_are_refused(problems):
    """"A correspondence will not even be created unless certain not-too-stringent
    conditions are met" [F95 p. 71]."""
    world, _, _ = two_rows(problems)
    assert C.creatable(world, "cup1", "cup1") == (False, "same item")
    assert C.create(world, "cup1", "cup2") is not None
    assert C.creatable(world, "cup2", "cup1") == (False, "exists")
    assert C.create(world, "cup2", "cup1") is None


def test_two_objects_in_one_group_are_refused(problems):
    """"the objects in question must not be in the same group" [F95 p. 72]."""
    world, _, _ = two_rows(problems)
    assert C.creatable(world, "cup1", "fork1") == (False, "in one group")
    assert C.creatable(world, "fork3", "knife2") == (False, "in one group")
    assert C.creatable(world, "fork1", "fork4")[0]


def test_a_group_and_its_own_member_or_subgroup_are_refused(problems):
    world, henry, _ = two_rows(problems)
    forks = GR.Group(members=["fork1", "fork2"], kind=GR.SAMENESS, parent=henry)
    world.workspace.add_group(forks)
    assert C.creatable(world, "g1", "cup1") == (False, "one contains the other")
    assert C.creatable(world, "cup1", "g1") == (False, "one contains the other")
    assert C.creatable(world, "g1", forks.name) == (False, "one contains the other")
    # two subgroups of one row are in one group, like two of its objects
    spoons = GR.Group(members=["spoon1", "spoon2"], kind=GR.SAMENESS, parent=henry)
    world.workspace.add_group(spoons)
    assert C.creatable(world, forks.name, spoons.name) == (False, "in one group")


def test_an_object_may_map_to_a_group_and_groups_to_groups(problems):
    """"A fork, for example, could very well be mapped onto a chunked group of two forks."
    [F95 p. 71]"""
    world, _, eliza = two_rows(problems)
    forks = GR.Group(members=["fork3", "fork4"], kind=GR.SAMENESS, parent=eliza)
    world.workspace.add_group(forks)
    assert C.creatable(world, "fork1", forks.name)[0]
    assert C.creatable(world, "g1", "g2")[0]
    assert C.creatable(world, "g1", "cup2")[0]
    cp = C.create(world, "fork1", forks.name)
    assert cp is not None and C.kind_of_end(world, cp.eliza) == "group"


def test_an_object_not_yet_in_view_cannot_be_mapped(problems):
    world = World(problems["surround-05"], CFG, 1)
    assert C.creatable(world, "cup1", "cup2") == (False, "not in view")


def test_a_correspondence_inside_a_group_correspondence(problems):
    """"built correspondences from both of the forks of the upper group to corresponding
    forks in the lower fork-fork group, as well as a correspondence between the two
    subgroups themselves" [F95 p. 108]."""
    world, henry, eliza = two_rows(problems)
    hf = GR.Group(members=["fork1", "fork2"], kind=GR.SAMENESS, parent=henry)
    ef = GR.Group(members=["fork3", "fork4"], kind=GR.SAMENESS, parent=eliza)
    world.workspace.add_group(hf)
    world.workspace.add_group(ef)
    outer = C.create(world, hf.name, ef.name)
    assert C.inner_pairs(world, outer) == [("fork1", "fork3"), ("fork1", "fork4"),
                                           ("fork2", "fork3"), ("fork2", "fork4")]
    inner = C.create(world, "fork1", "fork3", parent=outer)
    assert inner.level == outer.name and inner.parent is outer
    assert C.inner_pairs(world, outer) == [("fork1", "fork4"), ("fork2", "fork3"),
                                           ("fork2", "fork4")]
    assert C.create(world, "fork2", "knife2", parent=outer) is None   # not inside
    assert C.inner_pairs(world, inner) == []


# ---------------------------------------------------------------------------------------------
# Slippages and identities (D-030).


def test_slippages_and_identities_of_two_end_objects(problems):
    """A left end to a right end "will produce a left-right slippage" [F95 p. 68]; "When a
    concept slips to itself (e.g., right-right), this is a degenerate case of the notion
    of slippage, called an identity." [F95 p. 68]"""
    world, _, _ = two_rows(problems)
    cp = C.create(world, "fork1", "knife2")   # left end of Henry's row to right end of Eliza's
    tuples = [s.as_tuple() for s in cp.slippages]
    assert ("end", "end", "mirror symmetry") in tuples
    assert ("left", "right", "opposite") in tuples
    assert ("fork", "knife", "used with") in tuples
    assert ("one", "one", "sameness") in tuples
    flip = C.slippages_of_kind(cp, C.POSITION)[0]
    assert not flip.identity and flip.labels == ("opposite", "diagonal symmetry")
    same_end = C.slippages_of_kind(cp, C.END_MIDDLE)[0]
    assert same_end.identity and same_end.labels == ("mirror symmetry", "sameness")
    cp2 = C.create(world, "fork1", "spoon3")  # left end to left end
    ident = C.slippages_of_kind(cp2, C.POSITION)[0]
    assert ident.as_tuple() == ("left", "left", "mirror symmetry") and ident.identity
    assert C.slippages_of_kind(cp2, C.TYPE)[0].as_tuple() == ("fork", "spoon", "similar shape to")


def test_type_slippages_feed_the_label_or_the_shared_category(problems):
    world, _, _ = two_rows(problems)
    assert C.type_slippage(world, "cup", "cup").labels == ("sameness",)
    assert C.type_slippage(world, "cup", "small glass").labels == ("smaller than", "bigger than")
    assert C.type_slippage(world, "cup", "spoon").labels == ("liquid holder",)
    assert C.type_slippage(world, "fork", "saltshaker").labels == ()


def test_slippages_of_groups(problems):
    """"horizontal-horizontal, vertical-vertical, or horizontal-vertical slippages" [F95
    p. 69]; the count is "the size of the group" [F95 p. 68]."""
    world, _, _ = two_rows(problems)
    cp = C.create(world, "g1", "g2")
    tuples = [s.as_tuple() for s in cp.slippages]
    assert tuples[0] == ("group", "group", "")
    assert ("many", "many", "sameness") in tuples
    assert ("horizontal", "horizontal", "sameness") in tuples
    assert C.slippages_of_kind(cp, C.TYPE) == []      # a gestalt row has no one type
    assert C.slippages_of_kind(cp, C.POSITION) == []  # groups carry no group-frame words


def test_describe_rewrites_the_slippages_as_descriptors(problems):
    world, _, _ = two_rows(problems)
    cp = C.create(world, "fork1", "knife2")
    C.describe(world, cp)
    descriptors = world.workspace.descriptors(cp.name)
    assert descriptors[0] == "level:top"
    assert "left-right" in descriptors and "fork-knife" in descriptors
    assert "object-object" not in descriptors


# ---------------------------------------------------------------------------------------------
# The nine static factors, each in its direction [F95 pp. 68-69] (D-036), and the roles'
# tenth after them (D-098).


def test_the_nine_terms_in_order_sum_to_the_static_points(problems):
    world, _, _ = two_rows(problems)
    cp = C.create(world, "cup1", "small-glass1")
    names = [n for n, _ in C.static_terms(world, cp)]
    assert names == ["salience", "kind", "end_middle", "position", "type", "number", "isa",
                     "same_members", "orientation", "role"]
    assert math.isclose(C.static_points(world, cp), sum(v for _, v in C.static_terms(world, cp)))
    assert math.isclose(cp.static, C.static_points(world, cp))
    # The dynamic part is the Hobj points alone (the cup is the touched object) since v5
    # (D-116); through v4 it was 0 for a correspondence with no other in the Worldview.
    assert cp.rating == cp.static + cp.dynamic + cp.bonus and cp.bonus == 0
    assert cp.dynamic == dict(CFG.cp_dynamic_points)["hobj"]


def test_factor_1_more_salient_ends_earn_more(problems):
    """"The higher the salience of the objects (see previous section), the more points are
    awarded for the correspondence." [F95 p. 68]"""
    world, _, _ = two_rows(problems)
    cp = C.create(world, "cup1", "cup2")
    before = term(world, cp, "salience")
    world.slipnet.jolt("cup", 60)
    assert term(world, cp, "salience") > before
    assert math.isclose(term(world, cp, "salience"),
                        W["salience"] * (S.salience(world, "cup1") + S.salience(world, "cup2")) / 2)


def test_factor_2_object_object_and_group_group_over_group_object(problems):
    """"both of these slippages are preferred over group-to-simple-object slippages" [F95
    p. 68]."""
    world, _, _ = two_rows(problems)
    oo = C.create(world, "cup1", "cup2")
    gg = C.create(world, "g1", "g2")
    go = C.create(world, "g1", "cup2")
    assert term(world, oo, "kind") == W["kind"] == term(world, gg, "kind")
    assert term(world, go, "kind") == -CFG.group_object_penalty < 0


def test_factor_2_the_active_node_favours_its_kind(problems):
    """"If the 'group' node is active, the former are favored over the latter; if the
    'isolated' node is active, the latter are favored" [F95 p. 68]."""
    world, _, _ = two_rows(problems)
    oo = C.create(world, "cup1", "cup2")
    gg = C.create(world, "g1", "g2")
    world.slipnet.set_activation("group", 1)
    world.slipnet.set_activation("isolated", 1)
    assert term(world, oo, "kind") == term(world, gg, "kind") == W["kind"]
    world.slipnet.set_activation("group", 120)
    assert term(world, gg, "kind") == W["kind"] + W["kind_active"] > term(world, oo, "kind")
    world.slipnet.set_activation("group", 1)
    world.slipnet.set_activation("isolated", 120)
    assert term(world, oo, "kind") == W["kind"] + W["kind_active"] > term(world, gg, "kind")


def test_factor_3_end_to_end_and_middle_to_middle(problems):
    """"The program favors mapping an object on the end of one group to an object on the
    end of another group or, alternately, an object in the middle of one group to an object
    in the middle of another group." [F95 p. 68]"""
    world, _, _ = two_rows(problems)
    end_end = C.create(world, "fork1", "knife2")
    mid_mid = C.create(world, "cup1", "small-glass1")
    end_mid = C.create(world, "spoon2", "fork3")
    assert term(world, end_end, "end_middle") == W["end_middle"]
    assert term(world, mid_mid, "end_middle") == W["end_middle"]
    assert term(world, end_mid, "end_middle") == 0.0
    assert C.slippages_of_kind(end_mid, C.END_MIDDLE)[0].as_tuple() == ("end", "middle", "position")


def test_factor_4_position_slippages_are_worth_their_symmetry_nodes(problems):
    """"The value of this slippage depends on how active the 'diagonal symmetry' and
    'opposite' nodes are" [F95 p. 68]; a right-right identity rests on "mirror symmetry"
    [F95 p. 85] (D-030)."""
    world, _, _ = two_rows(problems)
    flip = C.create(world, "fork1", "knife2")    # left-right
    ident = C.create(world, "spoon2", "fork3")   # the right end to a middle: no axis word
    same = C.create(world, "fork2", "spoon3")    # a middle to the left end: none either
    assert term(world, ident, "position") == 0.0 == term(world, same, "position")
    for node in ("opposite", "diagonal symmetry", "mirror symmetry", "sameness"):
        world.slipnet.set_activation(node, 1)
    assert math.isclose(term(world, flip, "position"), W["position"] * 1 / 150)
    world.slipnet.set_activation("opposite", 150)
    world.slipnet.set_activation("diagonal symmetry", 150)
    assert math.isclose(term(world, flip, "position"), W["position"])
    left_left = C.create(world, "fork1", "spoon3")
    assert math.isclose(term(world, left_left, "position"), W["position"] * 1 / 150)
    world.slipnet.set_activation("mirror symmetry", 150)
    world.slipnet.set_activation("sameness", 150)
    assert math.isclose(term(world, left_left, "position"), W["position"])


def test_factor_5_closer_types_earn_more_and_the_human_knob_scales_it(problems):
    """"Points are awarded based on how conceptually close the types of the two
    corresponding objects are in the Slipnet." [F95 p. 68] The weight is the one French
    raised [F95 pp. 142-143], the `human` profile's knob (ANSWERS.md 7)."""
    world, _, _ = two_rows(problems)
    cup_cup = C.create(world, "cup1", "cup2")
    cup_glass = C.create(world, "cup1", "small-glass1")
    cup_spoon = C.create(world, "cup1", "spoon4")
    cup_fork = C.create(world, "cup1", "fork4")
    points = [term(world, cp, "type") for cp in (cup_cup, cup_glass, cup_spoon, cup_fork)]
    assert points[0] == CFG.type_weight and points[0] > points[1] > points[2] > points[3] == 0.0
    doubled = dataclasses.replace(CFG, type_weight=2 * CFG.type_weight)
    world2, _, _ = two_rows(problems, doubled)
    assert term(world2, C.create(world2, "cup1", "small-glass1"), "type") == 2 * points[1]
    assert HUMAN.type_weight > CFG.type_weight   # the calibrated profile raises it (D-076)


def test_factor_6_the_p68_number_example(problems):
    """"the conceptual distance between 'two' and 'two' in the Slipnet is less than the
    distance between 'one' and 'two.' For this reason, the first slippage ('two' to 'two')
    would be awarded more points than the second ('one' to 'two')." [F95 p. 68]"""
    world, henry, eliza = two_rows(problems)
    hf = GR.Group(members=["fork1", "fork2"], kind=GR.SAMENESS, parent=henry)
    ef = GR.Group(members=["fork3", "fork4"], kind=GR.SAMENESS, parent=eliza)
    world.workspace.add_group(hf)
    world.workspace.add_group(ef)
    two_two = C.create(world, hf.name, ef.name)
    one_two = C.create(world, "knife1", ef.name)
    assert world.slipnet.distance("two", "two") < world.slipnet.distance("one", "two")
    assert C.slippages_of_kind(two_two, C.NUMBER)[0].as_tuple() == ("two", "two", "sameness")
    assert C.slippages_of_kind(one_two, C.NUMBER)[0].as_tuple() == ("one", "two", "number")
    assert term(world, two_two, "number") == W["number"] > term(world, one_two, "number") > 0
    # and farther counts earn less still: "many" to "one" through "number"
    many_one = C.create(world, "g1", "cup2")
    assert term(world, one_two, "number") > term(world, many_one, "number") >= 0


def test_factor_7_shared_isa_groups_by_their_categories(problems):
    """"Points are awarded on the basis of how conceptually close the ISA description of
    one group (e.g., 'silverware') is to that of the other." [F95 p. 69]"""
    world = World(synthetic([("fork1", "fork", 10, 15), ("knife1", "knife", 16, 15),
                             ("cup1", "cup", 40, 15),
                             ("spoon1", "spoon", 10, 80), ("knife2", "knife", 16, 80),
                             ("cup2", "cup", 40, 80), ("small-glass1", "small glass", 46, 80)],
                            "cup1"), CFG, 1)
    notice_all(world)
    silver_h = described_group(world, ["fork1", "knife1"], kind=True)
    silver_e = described_group(world, ["spoon1", "knife2"], kind=True)
    drink_e = described_group(world, ["cup2", "small-glass1"], kind=True)
    assert silver_h.kind == silver_e.kind == drink_e.kind == GR.SHARED_ISA
    same = C.create(world, silver_h.name, silver_e.name)
    different = C.create(world, silver_h.name, drink_e.name)
    assert C.slippages_of_kind(same, C.ISA)[0].as_tuple() == ("silverware", "silverware", "sameness")
    assert term(world, same, "isa") == W["isa"]
    # silverware and liquid holder are joined only through the spoon (D-009), far apart
    assert 0.0 < term(world, different, "isa") < W["isa"] / 2
    plain = C.create(world, "cup1", silver_e.name)
    assert term(world, plain, "isa") == 0.0


def test_factor_8_the_same_object_types_in_both_groups(problems):
    """"If the object types in both groups making up the correspondence are the same (e.g.,
    both groups consist of a fork, a knife, and a cup), points are awarded" [F95 p. 69]."""
    world, henry, eliza = two_rows(problems)
    hf = GR.Group(members=["fork1", "fork2"], kind=GR.SAMENESS, parent=henry)
    ef = GR.Group(members=["fork3", "fork4"], kind=GR.SAMENESS, parent=eliza)
    es = GR.Group(members=["spoon3", "spoon4"], kind=GR.SAMENESS, parent=eliza)
    for g in (hf, ef, es):
        world.workspace.add_group(g)
    assert term(world, C.create(world, hf.name, ef.name), "same_members") == W["same_members"]
    assert term(world, C.create(world, hf.name, es.name), "same_members") == 0.0
    assert term(world, C.create(world, "g1", "g2"), "same_members") == 0.0  # a glass for a cup
    assert term(world, C.create(world, "fork1", "fork3"), "same_members") == 0.0  # objects


def test_factor_9_orientation_of_two_linear_groups(problems):
    """"If both groups are linear, there can be horizontal-horizontal, vertical-vertical,
    or horizontal-vertical slippages." [F95 p. 69]"""
    world = World(synthetic([("fork1", "fork", 10, 15), ("fork2", "fork", 16, 15),
                             ("cup1", "cup", 40, 15),
                             ("fork3", "fork", 90, 60), ("fork4", "fork", 90, 72),
                             ("spoon1", "spoon", 10, 80), ("spoon2", "spoon", 16, 80)],
                            "cup1"), CFG, 1)
    notice_all(world)
    row = described_group(world, ["fork1", "fork2"], kind=True)
    column = described_group(world, ["fork3", "fork4"], kind=True)
    row2 = described_group(world, ["spoon1", "spoon2"], kind=True)
    assert row.orientation == "horizontal" and column.orientation == "vertical"
    same = C.create(world, row.name, row2.name)
    flip = C.create(world, row.name, column.name)
    assert term(world, same, "orientation") == W["orientation"]
    assert term(world, flip, "orientation") == W["orientation_flip"] < W["orientation"]
    assert C.slippages_of_kind(flip, C.ORIENTATION)[0].as_tuple() == ("horizontal", "vertical", "")


def test_static_points_draw_nothing_and_are_recomputed_when_read(problems):
    world, _, _ = two_rows(problems)
    cp = C.create(world, "cup1", "cup2")
    draws = world.rng.draws
    before = C.evaluate(world, cp)
    world.slipnet.jolt("cup", 50)
    after = C.evaluate(world, cp)
    assert after > before and world.rng.draws == draws


# ---------------------------------------------------------------------------------------------
# The dynamic factors [F95 p. 69] (D-037).


def test_the_hobj_points_stand_alone_and_the_neighbour_terms_wait_for_another_correspondence(problems):
    """"The dynamic part of the evaluation of a correspondence can be done only when there
    are other correspondences in the Worldview." [F95 p. 69] read, since v5 (D-116), as the
    condition of the two relational terms: the Hobj status, "given a special status" [F95
    p. 69], is the correspondence's own and counts in every rating, alone included.
    Through v4 this test asserted 0 for a lone Hobj correspondence, the reading that lost
    the touched object's correspondence its points in exactly the Blockage contest where
    it stands alone in the Rival Worldview (RESULTS.md, v5)."""
    world, _, _ = two_rows(problems)
    hobj = C.create(world, "cup1", "cup2")
    assert C.contains_touched(world, hobj)
    assert C.dynamic_points(world, hobj) == 15.0        # the Hobj points, nothing else
    assert dict(C.dynamic_terms(world, hobj)) == {"hobj": 15.0, "hobj_neighbor": 0.0,
                                                  "worldview_neighbor": 0.0}
    hobj.in_worldview = True
    assert C.dynamic_points(world, hobj) == 15.0        # alone in the Worldview, the same
    plain = C.create(world, "fork1", "knife2")
    assert C.dynamic_points(world, plain) == 0.0        # no touched object: nothing alone
    plain.in_worldview = True
    assert C.hobj_correspondence(world) is hobj
    assert dict(C.dynamic_terms(world, hobj))["hobj"] == 15.0


def test_a_lone_hobj_correspondence_outrates_a_lone_rival_by_the_hobj_points(problems):
    """D-116 (v5): in a Rival Worldview where each stands alone, the touched object's
    correspondence rates its static points plus the Hobj points, the rival its static
    points alone, so with equal statics the Hobj one is ahead by exactly those points:
    French's "relatively weak" glass-glass blocker [F95 p. 123] no longer outrates the
    cup's own correspondence by its statics alone."""
    world, _, _ = two_rows(problems)
    hobj = C.create(world, "cup1", "small-glass1")
    rival = C.create(world, "fork1", "small-glass1")     # shares an end: a rival, not a Hobj
    assert WV.conflicts(world, hobj, rival) and not C.contains_touched(world, rival)
    alone_h = WV.rate(world, [hobj])
    alone_r = WV.rate(world, [rival])
    points = dict(CFG.cp_dynamic_points)["hobj"]
    assert alone_h.total == pytest.approx(hobj.static + points)
    assert alone_r.total == pytest.approx(rival.static)
    assert (alone_h.total - hobj.static) - (alone_r.total - rival.static) == pytest.approx(points)
    # The subjunctive rating of the fight reads the same (D-040): the Rival Worldview
    # {hobj} against the Old {rival}, the statics read with the flags set as the fight sets
    # them (the ends' salience reads the Worldview flag, D-031).
    WV.admit(world, rival)
    old = WV.current(world)
    assert old.total == pytest.approx(rival.static)
    rival.in_worldview, hobj.in_worldview = False, True
    static_inside = C.static_points(world, hobj)
    rival.in_worldview, hobj.in_worldview = True, False
    assert WV.rate_subjunctively(world, [rival], [hobj]).total == pytest.approx(static_inside + points)
    assert not hobj.in_worldview and rival.in_worldview


def test_a_rematch_of_one_rivalry_awards_no_second_bonus_or_penalty(problems):
    """D-117 (v5): the win bonus and the loss penalty are awarded once per correspondence,
    outcome and rivalry (the newcomer with its rivals), so that a rematch is "a little
    harder" [F95 p. 72] for the loser, one step, and the competition is then decided by the
    ratings as they stand; the wins and losses are still counted every time (D-031)."""
    from unittest import mock
    world, _, _ = two_rows(problems)
    incumbent = C.create(world, "cup1", "cup2")
    newcomer = C.create(world, "cup1", "small-glass1")
    WV.admit(world, incumbent)
    newcomer.passed = True
    with mock.patch.object(TP, "passes", return_value=False):      # the Old Worldview holds
        for _ in range(5):
            held = WV.compete(world, newcomer, 6.0)
            assert held is not None and not held.rival_wins
    assert incumbent.bonus == CFG.win_bonus and newcomer.bonus == -CFG.loss_penalty
    assert incumbent.wins == 5 and newcomer.losses == 5
    assert world.workspace.competitions_of("cup1") == (5, 5)
    assert len(world.workspace.awarded) == 2
    with mock.patch.object(TP, "passes", return_value=True):       # the Rival Worldview wins
        held = WV.compete(world, newcomer, 6.0)
        assert held.rival_wins and newcomer.in_worldview and not incumbent.in_worldview
    assert newcomer.bonus == CFG.win_bonus - CFG.loss_penalty      # one step each way
    assert incumbent.bonus == CFG.win_bonus - CFG.loss_penalty
    with mock.patch.object(TP, "passes", return_value=False):      # it holds against the comeback
        for _ in range(3):
            WV.compete(world, incumbent, 6.0)
    assert newcomer.bonus == CFG.win_bonus - CFG.loss_penalty and incumbent.bonus == CFG.win_bonus - CFG.loss_penalty
    assert len(world.workspace.awarded) == 4                          # two correspondences x two outcomes
    # A direct award, outside any fight, is made every time (the tests above).
    C.award(world, newcomer, True)
    C.award(world, newcomer, True)
    assert newcomer.bonus == 3 * CFG.win_bonus - CFG.loss_penalty


def test_the_hobj_correspondence_and_its_neighbours(problems):
    """A Hobj correspondence "receives points" [F95 p. 69]; "Two correspondences are
    considered neighbors if the end objects ... of one correspondence are the neighbors of
    the end objects of the second correspondence." [F95 p. 69]"""
    world, _, _ = two_rows(problems)
    hobj = C.create(world, "cup1", "small-glass1")    # the two middles
    hobj.in_worldview = True
    beside = C.create(world, "knife1", "spoon4")     # knife1 is cup1's neighbour, spoon4 the glass's
    far = C.create(world, "fork1", "spoon3")         # the two left ends
    far.in_worldview = True
    assert C.are_neighbours(world, beside, hobj) and not C.are_neighbours(world, far, hobj)
    assert C.are_neighbours(world, far, C.create(world, "fork2", "fork3")) is False
    terms = dict(C.dynamic_terms(world, beside))
    assert terms == {"hobj": 0.0, "hobj_neighbor": 10.0, "worldview_neighbor": 0.0}
    next_to_far = C.create(world, "fork2", "spoon4")  # fork2 beside fork1, spoon4 beside spoon3
    assert dict(C.dynamic_terms(world, next_to_far)) == {"hobj": 0.0, "hobj_neighbor": 0.0,
                                                         "worldview_neighbor": 5.0}
    assert dict(C.dynamic_terms(world, hobj)) == {"hobj": 15.0, "hobj_neighbor": 0.0,
                                                  "worldview_neighbor": 0.0}
    assert C.dynamic_points(world, beside) == 10.0


def test_a_neighbour_of_the_hobj_outranks_a_neighbour_of_another(problems):
    """"Being a neighbor of a non-Hobj correspondence in the Worldview is worth fewer
    points than being the neighbor of the Hobj correspondence" [F95 p. 69]."""
    points = dict(CFG.cp_dynamic_points)
    assert points["hobj"] > points["hobj_neighbor"] > points["worldview_neighbor"] > 0


def test_a_group_containing_the_touched_object_makes_a_hobj_correspondence(problems):
    world, _, _ = two_rows(problems)
    gg = C.create(world, "g1", "g2")
    other = C.create(world, "fork1", "knife2")
    other.in_worldview = True
    gg.in_worldview = True
    assert C.contains_touched(world, gg) and C.hobj_correspondence(world) is gg


# ---------------------------------------------------------------------------------------------
# The bonus and the penalty [F95 p. 68] (D-038).


def test_wins_add_the_bonus_and_losses_take_the_penalty(problems):
    """"The bonus is awarded each time the correspondence wins a competition to get into (or
    to remain in) the Worldview. Correspondences that lose competitions lose points." [F95
    p. 68]"""
    world, _, _ = two_rows(problems)
    cp = C.create(world, "cup1", "cup2")
    base = cp.rating
    assert C.award(world, cp, True) == base + CFG.win_bonus
    assert C.award(world, cp, True) == base + 2 * CFG.win_bonus
    assert cp.wins == 2 and cp.losses == 0
    assert C.award(world, cp, False) == base + 2 * CFG.win_bonus - CFG.loss_penalty
    assert cp.losses == 1
    assert world.workspace.competitions_of("cup1") == (2, 1) == world.workspace.competitions_of("cup2")
    assert CFG.loss_penalty > CFG.win_bonus   # "the weak get weaker" [F95 p. 69] a little faster


def test_the_rating_never_falls_below_zero_and_survives_re_evaluation(problems):
    world, _, _ = two_rows(problems)
    cp = C.create(world, "cup1", "cup2")
    for _ in range(30):
        C.award(world, cp, False)
    assert cp.rating == 0.0 and cp.bonus == -30 * CFG.loss_penalty
    C.evaluate(world, cp)
    assert cp.bonus == -30 * CFG.loss_penalty   # the bonus is accumulated, not recomputed
    assert cp.static > 0


# ---------------------------------------------------------------------------------------------
# The test stage [F95 p. 72] (D-039).


def test_the_chance_is_the_static_points_over_100_clipped(problems):
    world, _, _ = two_rows(problems)
    good = C.create(world, "cup1", "cup2")
    poor = C.create(world, "g1", "cup2")
    assert math.isclose(C.test_chance(world, good), good.static / 100)
    assert 0.5 < C.test_chance(world, good) < 0.99
    assert C.test_chance(world, poor) < 0.1


def test_the_test_is_deterministic_cold_and_a_coin_warm(problems):
    """"the outcome of this evaluation is a probabilistic one: there is no rigid evaluation
    threshold above which the correspondence passes and below which it fails" [F95 p. 72];
    over 400 seeds at a(80) = 1.28 the pass rate is within four binomial standard
    deviations of the law's own probability (D-060)."""
    passes, expected = 0, None
    for seed in range(400):
        world, _, _ = two_rows(problems, seed=seed)
        cp = C.create(world, "cup1", "small-glass1")
        draws = world.rng.draws
        passed = C.test(world, cp, TP.exponent_at(80, CFG))
        assert cp.tested and cp.passed == passed and world.rng.draws == draws + 1
        assert cp in world.workspace.correspondences   # a failure stays, tentative
        expected = TP.pass_probability(cp.chance, TP.exponent_at(80, CFG))
        passes += passed
    sigma = math.sqrt(400 * expected * (1 - expected))
    assert abs(passes - 400 * expected) <= 4 * sigma, (passes, expected)
    # cold: a chance above a half always passes, one below never
    world, _, _ = two_rows(problems)
    assert C.test(world, C.create(world, "cup1", "cup2"), A_COLD)
    assert not C.test(world, C.create(world, "g1", "cup2"), A_COLD)


def test_the_test_is_reproducible_from_the_seed(problems):
    results = []
    for _ in range(2):
        world, _, _ = two_rows(problems, seed=7)
        cps = [C.create(world, a, b) for a, b in (("cup1", "cup2"), ("fork1", "fork4"),
                                                   ("g1", "cup2"), ("spoon2", "spoon3"))]
        results.append([C.test(world, cp, 1.0) for cp in cps] + [world.rng.draws])
    assert results[0] == results[1]


# ---------------------------------------------------------------------------------------------
# The two biases French calls too strong, fixed (D-045, D-046; ANSWERS.md 2).


def test_the_group_to_object_penalty_is_relaxed_by_half(problems):
    """"This strong bias against group-to-isolated-object mappings should probably be
    relaxed." [F95 p. 125] 15 points under `standard`, French's 30 under
    `french-faults`."""
    assert CFG.group_object_penalty == 15 and FAULTS.group_object_penalty == 30
    standard, faults = two_rows(problems, CFG)[0], two_rows(problems, FAULTS)[0]
    cp_s = C.create(standard, "g1", "cup2")
    cp_f = C.create(faults, "g1", "cup2")
    assert term(standard, cp_s, "kind") == -15.0 and term(faults, cp_f, "kind") == -30.0
    # the whole difference is the penalty, less the half point the group's extra salience
    # points (D-046, below) add to factor 1 under `french-faults`
    assert math.isclose(cp_s.static - cp_f.static, 15.0 - W["salience"] * 5 / 2)
    # the preference French defends is kept: a group-object pair is still well behind
    assert term(standard, cp_s, "kind") < 0 < term(standard, C.create(standard, "cup1", "cup2"), "kind")


def test_the_group_level_bias_is_weakened_by_a_third(problems):
    """"this bias is clearly a bit too strong" [F95 p. 109]: the salience points for being
    a group are 10 under `standard` and 15 under `french-faults` (D-046); the static kind
    points for a group-group correspondence are the same under both."""
    assert CFG.group_salience_points == 10 and FAULTS.group_salience_points == 15
    standard, faults = two_rows(problems, CFG)[0], two_rows(problems, FAULTS)[0]
    assert S.group_term(standard, "g1") == 10.0 and S.group_term(faults, "g1") == 15.0
    gg_s, gg_f = C.create(standard, "g1", "g2"), C.create(faults, "g1", "g2")
    assert term(standard, gg_s, "kind") == term(faults, gg_f, "kind")
    assert term(faults, gg_f, "salience") > term(standard, gg_s, "salience")  # through salience only


def test_the_profiles_differ_in_the_fault_knobs_only_here(problems):
    changed = FAULTS.differences(CFG)
    assert "group_object_penalty" in changed and "group_salience_points" in changed
    assert "type_weight" not in changed and "cp_static_weights" not in changed

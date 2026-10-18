"""Salience (D-031 to D-034, D-046): French's ten factors each raise it [F95 pp. 66-67],
the region order relative to the touched object in the corner and middle-column cases
[F95 p. 66; F95 p. 93], the chapter 4 run's isolated cup [F95 p. 97], and choices by
salience that are deterministic at T = 0 and near-uniform at T = 100 [F95 p. 93].

The statistical tests are seeded; their bounds are four binomial standard deviations of
the law's own probability (D-060), stated beside each.
"""

import math

import pytest

from tabletop import describe as DS
from tabletop import geometry as G
from tabletop import groups as GR
from tabletop import problems as P
from tabletop import salience as S
from tabletop import temperature as TP
from tabletop.config import PROFILES
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
FAULTS = V2_PROFILES["french-faults"]
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
A_COLD, A_HOT = TP.exponent_at(0, CFG), TP.exponent_at(100, CFG)   # 6 and 0.1 [F95 p. 92]
W = dict(CFG.salience_weights)


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
    """Every object in view, each in its exact region (no blur draw), in file order."""
    for obj in world.problem.objects:
        world.workspace.notice(obj.id, world.rng, region=G.exact_region(obj.x, obj.y))


def henry_row(problems, config=CFG, seed=1):
    """Surround 5 with every object in view and Henry's row as a gestalt group whose chain
    of neighbours is known (the ends can then be read off it, D-028)."""
    world = World(problems["surround-05"], config, seed)
    notice_all(world)
    group = GR.Group(members=list(HENRY_ROW))
    world.workspace.add_group(group)
    frontier = ["fork1"]
    while frontier:
        for n, _ in DS.find_neighbours(world, group, frontier.pop(0)):
            frontier.append(n)
    assert group.chain_known()
    return world, group


def bottom_up(world, parent, members):
    """A bottom-up group of these members placed directly, its kind read off their types
    (D-024), so that no proximity coin is spent on the way (the tests here are about the
    terms, not the building)."""
    kind, node = GR.kind_of(world.problem, members, world.config)
    group = GR.Group(members=list(members), kind=kind, category=node, parent=parent)
    world.workspace.add_group(group)
    return group


class StubCorrespondence:
    """The two fields of a correspondence that salience reads (D-031, factor 6)."""

    def __init__(self, ends, in_worldview):
        self.ends = ends
        self.in_worldview = in_worldview


def four_sigma(p, n):
    return 4 * math.sqrt(p * (1 - p) / n)


# ---------------------------------------------------------------------------------------------
# The sum and the start.


def test_the_terms_are_frenchs_factors_in_order_and_sum_to_the_salience(problems):
    """"Each of these factors determines a real number, and these numbers are summed to
    determine the salience of the object." [F95 p. 67]"""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    for item in world.workspace.items():
        names = [name for name, _ in S.terms(world, item)]
        assert tuple(names) == S.FACTORS
        assert S.salience(world, item) == pytest.approx(
            max(S.FLOOR, sum(v for _, v in S.terms(world, item))))


def test_at_the_start_only_the_touched_object_has_a_salience_and_no_dynamic_terms(problems):
    """"at the beginning of a run, there are no objects, in correspondence with other
    objects, and no relations between objects have been found yet. In this case, then, this
    factor contributes zero to all calculations of salience" [F95 p. 67]"""
    world = World(problems["surround-05"], CFG, 1)
    assert [item for item, _ in S.saliences(world)] == ["cup1"]
    terms = dict(S.terms(world, "cup1"))
    assert terms["worldview"] == 0 and terms["competitions"] == 0 and terms["position"] == 0
    assert terms["group"] == 0 and terms["composition"] == 0
    assert terms["proximity"] == W["proximity"]          # d = 0: the whole weight (D-033)
    assert terms["region"] == W["region"] * dict(CFG.region_preference)["near"]
    assert terms["type"] == pytest.approx(W["type_activation"] * world.slipnet.activation_of("cup"))


def test_reading_salience_draws_nothing(problems):
    """Salience is recomputed when read, from the state as it stands, without the
    generator (D-031)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    before = world.rng.draws
    S.saliences(world)
    S.terms(world, "cup2")
    S.most_salient(world)
    S.region_order(world)
    assert world.rng.draws == before


# ---------------------------------------------------------------------------------------------
# Each factor raises salience [F95 pp. 66-67].


def test_factor_1_the_activation_of_the_type_node_raises_the_salience_of_its_objects(problems):
    """"if a certain number of spoons have been noticed by the program, the concept 'spoon'
    will be active. This will contribute to the salience of all spoons" [F95 p. 66]"""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    before = {item: S.salience(world, item) for item in world.workspace.noticed}
    world.slipnet.jolt("spoon", 60)
    for spoon in ("spoon1", "spoon2", "spoon3", "spoon4"):
        assert S.salience(world, spoon) == pytest.approx(before[spoon] + W["type_activation"] * 60)
    for other in ("cup1", "cup2", "fork1", "small-glass1"):
        assert S.salience(world, other) == pytest.approx(before[other])


def test_factor_2_proximity_to_the_touched_object(problems):
    """"Objects close to the touched object (usually the most salient object on the table)
    are noticed preferentially because of this proximity." [F95 p. 66] (D-033: linear to 0
    at `proximity_reach`)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    terms = {item: S.proximity_term(world, item) for item in world.workspace.noticed}
    assert terms["cup1"] == W["proximity"]
    assert terms["knife1"] > terms["fork2"] > terms["fork1"] > 0      # nearer is more
    assert terms["spoon1"] > terms["spoon2"]
    assert terms["cup2"] == 0 and terms["small-glass1"] == 0           # beyond the reach
    knife = world.problem.object("knife1")
    d = G.distance(knife.x, knife.y, 30.3, 15.3)
    assert terms["knife1"] == pytest.approx(W["proximity"] * (1 - d / CFG.proximity_reach), abs=0.3)


def test_factor_3_ends_of_a_group_are_more_salient_unless_middle_is_more_active(problems):
    """"Objects at the ends of a group are, in general, more likely to be noticed than
    objects in the middle of a group (unless, of course, the concept 'middle' is much more
    active in the Slipnet than the concept 'end,' in which case the default preference for
    objects on the ends of groups can be overridden)." [F95 p. 66]"""
    world, group = henry_row(problems)
    middles = ("fork2", "knife1", "cup1", "spoon1")
    assert all(S.position_term(world, m) == 0 for m in HENRY_ROW)
    DS.find_ends(world, group)
    assert group.ends == ["fork1", "spoon2"]
    assert S.position_term(world, "fork1") == W["end"] == S.position_term(world, "spoon2")
    assert all(S.position_term(world, m) == 0 for m in middles)
    # Four middles against two ends: "middle" is now more active than "end" by the
    # describing codelets' own jolts, but not "much more", so the ends keep their points.
    DS.find_middles(world, group)
    assert world.slipnet.activation_of("middle") > world.slipnet.activation_of("end")
    assert not S.middle_preferred(world)
    assert S.position_term(world, "fork1") == W["end"] == S.position_term(world, "spoon2")
    assert all(S.position_term(world, m) == 0 for m in middles)
    world.slipnet.set_activation("middle", 150)
    world.slipnet.set_activation("end", 60)
    assert S.middle_preferred(world)
    assert S.position_term(world, "fork1") == 0 == S.position_term(world, "spoon2")
    assert all(S.position_term(world, m) == W["middle"] for m in middles)
    assert S.position_term(world, group.name) == 0
    world.slipnet.set_activation("end", 76)
    assert not S.middle_preferred(world)            # 150 < 2 x 76 (D-031's factor)


def test_factor_4_the_preferred_regions_relative_to_the_touched_object(problems):
    """"the most favored region is diagonally opposite the touched object, and the next
    most preferred region is directly opposite the touched object" [F95 p. 66] (D-032)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    pref = dict(CFG.region_preference)
    assert S.region_term(world, "cup2") == W["region"] * pref["diagonal"]        # lower right
    assert S.region_term(world, "spoon3") == W["region"] * pref["opposite"]      # lower left
    assert S.region_term(world, "fork1") == W["region"] * pref["near"]           # Henry's corner
    assert S.region_term(world, "spoon1") == W["region"] * pref["near"]          # upper middle
    assert S.region_term(world, "knife2") == W["region"] * pref["other"]         # lower middle
    assert S.region_term(world, "cup2") > S.region_term(world, "spoon3") \
        > S.region_term(world, "fork1") > S.region_term(world, "knife2")


def test_the_region_term_reads_the_region_the_object_was_last_seen_in(problems):
    """An object near a boundary "may sometimes be considered by the program to be in the
    adjacent region" [F95 p. 65]: its salience follows the region drawn at the last look,
    not its exact one (D-004, D-031)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    exact = S.region_term(world, "knife2")                          # lower middle: other
    world.workspace.notice("knife2", world.rng, region=G.region_named("lower-left"))
    assert S.region_term(world, "knife2") == W["region"] * dict(CFG.region_preference)["opposite"]
    assert S.region_term(world, "knife2") > exact
    assert S.region_of(world, "spoon4") == G.exact_region(19.8, 74.7)
    # An object not yet noticed is read in its exact region.
    fresh = World(problems["surround-05"], CFG, 1)
    assert S.region_of(fresh, "cup2") == G.region_named("lower-right")


def test_factor_5_groups_get_points_for_being_groups_fewer_under_the_fix(problems):
    """"Groups are given salience points simply for being groups, which constitutes a
    pressure in favor of looking at them over simple objects." [F95 p. 66]; the bias is
    "a bit too strong" [F95 p. 109] (D-046, FIXED (ANSWERS 2): 10 against 15)."""
    for config, points in ((CFG, 10), (FAULTS, 15)):
        world, group = henry_row(problems, config)
        assert S.group_term(world, group.name) == points
        assert all(S.group_term(world, m) == 0 for m in HENRY_ROW)
        assert config.group_salience_points == points


def test_factor_6_an_object_in_a_worldview_correspondence_gains_salience(problems):
    """"An object's salience increases if it has been mapped to another object on the table
    (specifically, if the object is part of a correspondence in the Worldview" [F95 p. 66];
    "this will increase the salience of both objects" [F95 p. 67]"""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    before = {item: S.salience(world, item) for item in ("cup1", "cup2", "small-glass1")}
    world.workspace.correspondences.append(StubCorrespondence(["cup1", "small-glass1"], False))
    assert all(S.worldview_term(world, item) == 0 for item in before)      # tentative only
    world.workspace.correspondences.append(StubCorrespondence(["cup1", "cup2"], True))
    assert world.workspace.in_worldview("cup1") and world.workspace.in_worldview("cup2")
    assert not world.workspace.in_worldview("small-glass1")
    assert S.salience(world, "cup1") == pytest.approx(before["cup1"] + W["in_worldview"])
    assert S.salience(world, "cup2") == pytest.approx(before["cup2"] + W["in_worldview"])
    assert S.salience(world, "small-glass1") == pytest.approx(before["small-glass1"])


def test_factor_7_wins_add_points_and_losses_take_them_away(problems):
    """"Each time a correspondence competes to become part of the Worldview and wins, bonus
    points are awarded to the two objects making up the correspondence. Objects in losing
    correspondences lose points." [F95 p. 66]"""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    ws = world.workspace
    before = {item: S.salience(world, item) for item in ("cup1", "cup2", "small-glass1")}
    ws.record_competition(["cup1", "cup2"], won=True)
    ws.record_competition(["cup1", "small-glass1"], won=False)
    assert ws.competitions_of("cup1") == (1, 1) and ws.competitions_of("cup2") == (1, 0)
    assert ws.competitions_of("small-glass1") == (0, 1) and ws.competitions_of("fork1") == (0, 0)
    assert S.competition_term(world, "cup1") == W["win"] - W["loss"]
    assert S.salience(world, "cup2") == pytest.approx(before["cup2"] + W["win"])
    assert S.salience(world, "small-glass1") == pytest.approx(before["small-glass1"] - W["loss"])
    for _ in range(20):
        ws.record_competition(["small-glass1"], won=False)
    assert S.salience(world, "small-glass1") == S.FLOOR        # never below the floor


def test_factors_8_to_10_the_composition_of_a_group(problems):
    """"If all the objects in a group are of the same type (i.e., it is a sameness group),
    this increases the group's salience." [F95 p. 67]; the same for a common superordinate
    category; "Larger groups are favored over smaller groups." [F95 p. 67]"""
    world, parent = henry_row(problems)
    forks = bottom_up(world, parent, ["fork1", "fork2"])
    silver = bottom_up(world, parent, ["spoon2", "knife1"])
    assert forks.kind == GR.SAMENESS and silver.kind == GR.SHARED_ISA
    assert S.composition_term(world, forks.name) == W["sameness_group"] + 2 * W["size"]
    assert S.composition_term(world, silver.name) == W["shared_isa_group"] + 2 * W["size"]
    assert S.composition_term(world, parent.name) == 6 * W["size"]      # gestalt: size only
    assert S.composition_term(world, "fork1") == 0
    mixed = GR.Group(members=["cup1", "spoon1"], kind=GR.MIXED, parent=parent)
    world.workspace.add_group(mixed)
    assert S.composition_term(world, mixed.name) == 2 * W["size"]
    cups = [(f"cup{i}", "cup", 10.0 + 9 * i, 50.0) for i in range(1, 6)]
    big = World(synthetic(cups, "cup1"), CFG, 1)
    small = GR.Group(members=["cup1", "cup2"], kind=GR.SAMENESS)
    large = GR.Group(members=["cup1", "cup2", "cup3", "cup4", "cup5"], kind=GR.SAMENESS)
    big.workspace.add_group(small)
    big.workspace.add_group(large)
    assert S.composition_term(big, large.name) > S.composition_term(big, small.name)


def test_a_group_outranks_its_own_members_through_its_group_terms(problems):
    """"Groups of objects compete for attention with single objects." [F95 p. 66]: the
    sameness group of the two forks is more salient than either fork."""
    world, parent = henry_row(problems)
    forks = bottom_up(world, parent, ["fork1", "fork2"])
    assert S.salience(world, forks.name) > max(S.salience(world, "fork1"), S.salience(world, "fork2"))
    terms = dict(S.terms(world, forks.name))
    assert terms["group"] == CFG.group_salience_points and terms["composition"] > 0
    assert terms["type"] == pytest.approx(W["type_activation"] * world.slipnet.activation_of("group"))


# ---------------------------------------------------------------------------------------------
# The chapter 4 run's early focus [F95 p. 97].


def test_the_isolated_cup_of_chapter_4_is_the_most_salient_object_for_frenchs_two_reasons(problems):
    """"it shares object type ('cup') with the touched object, and it is in a very good
    position on the table with respect to the touched object (i.e., roughly diagonally
    opposite from it)" [F95 p. 97]: on Surround 5, with everything in view and the Slipnet
    as it starts, the isolated cup is the most salient object after the touched one, and
    its two largest terms are the type and the region."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    others = [item for item in world.workspace.noticed if item != "cup1"]
    assert S.most_salient(world, others) == "cup2"
    terms = sorted(S.terms(world, "cup2"), key=lambda pair: -pair[1])
    assert {terms[0][0], terms[1][0]} == {"region", "type"}
    assert S.salience(world, "cup2") > S.salience(world, "small-glass1")
    assert S.most_salient(world) == "cup1"              # the touched object, nearest and a cup


# ---------------------------------------------------------------------------------------------
# The region order (D-032) [F95 p. 66; F95 p. 93].


def test_the_region_order_for_a_touched_object_in_a_corner(problems):
    """"an area diagonally opposite the touched object is most preferred, an area directly
    opposite the touched object is next, and so on." [F95 p. 93]: Surround's cup in the
    upper left."""
    world = World(problems["surround-05"], CFG, 1)
    order = [(region.name, weight) for region, weight in S.region_order(world)]
    assert order[0] == ("lower-right", 1.0)
    assert order[1] == ("lower-left", 0.7)
    assert [name for name, w in order if w == 0.4] == ["upper-left", "upper-middle", "middle-left", "middle"]
    assert [name for name, w in order if w == 0.15] == ["upper-right", "middle-right", "lower-middle"]
    assert len(order) == 9


def test_the_region_order_for_a_touched_object_in_the_middle_column(problems):
    """Buridan's spoon is at the top centre [F95 p. 129]: the diagonal and the directly
    opposite reflections coincide at the lower middle, and the two far corners come next
    (D-032's reading of what "diagonally opposite" can mean there)."""
    world = World(problems["buridan-01"], CFG, 1)
    order = [(region.name, weight) for region, weight in S.region_order(world)]
    assert order[0] == ("lower-middle", 1.0)
    assert order[1:3] == [("lower-left", 0.7), ("lower-right", 0.7)]
    assert all(w == 0.4 for _, w in order[3:])
    fg = World(problems["figure-ground-01"], CFG, 1)        # the touched fork at the bottom centre
    fg_order = [region.name for region, _ in S.region_order(fg)]
    assert fg_order[:3] == ["upper-middle", "upper-left", "upper-right"]


def test_the_region_order_agrees_with_the_scouts_preferences(problems):
    """One table of preferences for the scouts (`perceive.region_preferences`, D-023) and
    for salience (D-032)."""
    from tabletop import perceive as PV
    world = World(problems["blockage-05"], CFG, 1)
    scouts = dict((r.name, w) for r, w in PV.region_preferences(world))
    assert scouts == dict((r.name, w) for r, w in S.region_order(world))


# ---------------------------------------------------------------------------------------------
# Choice by salience under temperature (D-034, D-060) [F95 p. 93].


def nine_objects():
    """Nine cups, one at the centre of each region, touched in the upper left."""
    objects = []
    k = 1
    for row in (16.7, 50.0, 83.3):
        for col in (18.3, 55.0, 91.7):
            objects.append((f"cup{k}", "cup", col, row))
            k += 1
    return synthetic(objects, "cup1")


def test_at_temperature_0_the_choice_is_essentially_deterministic():
    """"When the temperature is very low, the program deterministically respects the
    priorities of the regions" [F95 p. 93]: at a(0) = 6 the object in the diagonal corner
    (salience 54) is chosen first, the one directly opposite (42) second, and the six
    others almost never, by the law w^6 / sum w^6 (0.72, 0.16, at most 0.06 each); the
    diagonal corner's chance is more than three times what it is at a = 1. Bound: four
    binomial standard deviations around the law's probability."""
    n = 300
    world = World(nine_objects(), CFG, 1)
    notice_all(world)
    others = [item for item in world.workspace.noticed if item != "cup1"]
    assert S.most_salient(world, others) == "cup9"
    saliences = [s for _, s in S.saliences(world, others)]
    cold = [s ** A_COLD for s in saliences]
    law = {item: w / sum(cold) for item, w in zip(others, cold)}
    assert law["cup9"] > 0.7 and law["cup7"] > 0.15
    assert all(law[item] <= 0.06 for item in others if item not in ("cup9", "cup7"))
    assert law["cup9"] > 3 * (saliences[-1] / sum(saliences))
    counts = {item: 0 for item in others}
    for seed in range(n):
        w = World(nine_objects(), CFG, seed)
        notice_all(w)
        counts[S.choose(w, A_COLD, others)] += 1
    for item in ("cup9", "cup7"):
        assert abs(counts[item] / n - law[item]) <= four_sigma(law[item], n), item
    assert counts["cup9"] > counts["cup7"] > max(counts[i] for i in others if i not in ("cup9", "cup7"))


def test_at_temperature_100_the_choice_is_nearly_uniform():
    """"If the temperature should ever happen to reach 100, the program is as likely to
    choose any region as any other." [F95 p. 93]; "The selection of objects or
    correspondences based on their salience works in the same way." [F95 p. 93]: at a(100)
    = 0.1 every one of the eight objects has a chance between 0.08 and 0.17 by the law, and
    the observed frequencies lie within four binomial standard deviations of the law's."""
    n = 1200
    world = World(nine_objects(), CFG, 1)
    notice_all(world)
    others = [item for item in world.workspace.noticed if item != "cup1"]
    weights = [s ** A_HOT for _, s in S.saliences(world, others)]
    law = [w / sum(weights) for w in weights]
    assert all(0.08 <= p <= 0.17 for p in law)
    assert max(law) / min(law) < 1.5
    counts = {item: 0 for item in others}
    for seed in range(n):
        w = World(nine_objects(), CFG, seed)
        notice_all(w)
        counts[S.choose(w, A_HOT, others)] += 1
    for item, p in zip(others, law):
        assert abs(counts[item] / n - p) <= four_sigma(p, n), (item, counts[item] / n, p)


def test_the_choice_is_one_draw_reproducible_and_honours_candidates_and_exclusions(problems):
    world = World(problems["surround-05"], CFG, 7)
    notice_all(world)
    twin = World(problems["surround-05"], CFG, 7)
    notice_all(twin)
    before = world.rng.draws
    first = S.choose(world, 1.0)
    assert world.rng.draws == before + 1
    assert first == S.choose(twin, 1.0)
    picks = [S.choose(world, 1.0, exclude=(first,)) for _ in range(30)]
    assert first not in picks and all(p is not None for p in picks)
    assert S.choose(world, 1.0, [], ()) is None
    assert S.choose(world, 1.0, ["cup2"], ("cup2",)) is None
    drawn = world.rng.draws
    assert S.choose(world, 1.0, ["spoon3"]) == "spoon3" and world.rng.draws == drawn + 1


def test_choose_object_and_choose_group_keep_to_their_kind(problems):
    world, parent = henry_row(problems, seed=3)
    forks = bottom_up(world, parent, ["fork1", "fork2"])
    for _ in range(20):
        assert S.choose_object(world, 1.0) in world.workspace.noticed
        assert S.choose_group(world, 1.0) in (parent.name, forks.name)
    assert S.choose_group(world, 1.0, exclude=(parent.name, forks.name)) is None
    assert set(S.most_salient(world, [parent.name, forks.name]) for _ in range(3)) <= {parent.name, forks.name}


def test_most_salient_takes_the_earliest_among_equals():
    cups = [("cup1", "cup", 20.0, 20.0), ("cup2", "cup", 40.0, 60.0), ("cup3", "cup", 40.0, 60.0)]
    world = World(synthetic(cups, "cup1"), CFG, 1)
    notice_all(world)
    assert S.salience(world, "cup2") == S.salience(world, "cup3")
    assert S.most_salient(world, ["cup2", "cup3"]) == "cup2"
    assert S.most_salient(world, ["cup3", "cup2"]) == "cup3"
    assert S.most_salient(world, []) is None


# ---------------------------------------------------------------------------------------------
# The hook of D-069: the salience function in a group's strength.


def test_the_salience_function_plugs_into_group_strength_only_under_french_faults(problems):
    """French's program made a fork-glass group because of the "degree of salience of the
    touched fork" [F95 p. 142]: with the real salience function, `french-faults` adds its
    share of the members' mean salience to the strength; `standard` adds nothing (D-069,
    FIXED (ANSWERS 2))."""
    world, parent = henry_row(problems)
    plain = GR.strength_of(world, ["cup1", "spoon1"], GR.MIXED, parent)
    assert GR.strength_of(world, ["cup1", "spoon1"], GR.MIXED, parent,
                          salience_of=S.salience_of(world)) == plain
    faulty, parent_f = henry_row(problems, FAULTS)
    plain_f = GR.strength_of(faulty, ["cup1", "spoon1"], GR.MIXED, parent_f)
    mean = (S.salience(faulty, "cup1") + S.salience(faulty, "spoon1")) / 2
    assert GR.strength_of(faulty, ["cup1", "spoon1"], GR.MIXED, parent_f,
                          salience_of=S.salience_of(faulty)) == pytest.approx(
        min(100.0, plain_f + FAULTS.member_salience_in_group_strength * mean))
    assert mean > 30     # the touched cup is salient: its term is not negligible

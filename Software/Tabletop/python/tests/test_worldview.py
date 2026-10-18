"""The Worldview (D-030, D-038, D-040 to D-044): rivals and usurping [F95 pp. 71, 101,
105, 124], the subjunctive Rival Worldview and the fight [F95 p. 72], the rating with its
coherence and parallelism terms [F95 pp. 74-75; FCCA p. 387] and the p. 74 three-group
example, the Fig. 4.8 case as French explains it [F95 pp. 101, 105], winners rewarded and
losers staying in the Workspace [F95 pp. 72-73], the clean-up [F95 p. 73], and the
invariants of a Worldview [F95 p. 71; FCCA p. 390].

The statistical tests are seeded; their bounds are four binomial standard deviations of
the engine's own chance, stated beside each.
"""

import math

import pytest

from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import geometry as G
from tabletop import groups as GR
from tabletop import problems as P
from tabletop import temperature as TP
from tabletop import worldview as WV
from tabletop.config import PROFILES, Config
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
ELIZA_ROW = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]
A_COLD, A_HOT = TP.exponent_at(0, CFG), TP.exponent_at(100, CFG)   # 6 and 0.1 [F95 p. 92]
A_FIG_4_7 = TP.exponent_at(35.8, CFG)   # the thermometer of Fig. 4.7 [F95 p. 104 (img)]


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


def described_group(world, members, kind=None, middles=True):
    """A group placed directly with its chain known, its ends and orientation found, and
    its middles when asked (D-027, D-028); a bottom-up kind when given."""
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
    if middles:
        DS.find_middles(world, group)
    DS.notice_orientation(world, group)
    return group


def chapter_4_table(problems, seed=1, config=CFG):
    """The central configuration as the chapter 4 run saw it by Fig. 4.6 [F95 pp. 101-103
    (img)]: every object in view, both rows as gestalt groups with their ends found but
    not their middles ("middle" stays at 5-8 through the dumps, SPEC.md 6c), and the two
    spoon-spoon subgroups as sameness groups. Returns the world and the two subgroups."""
    world = World(problems["surround-05"], config, seed)
    notice_all(world)
    described_group(world, HENRY_ROW, middles=False)
    described_group(world, ELIZA_ROW, middles=False)
    spoons_h = described_group(world, ["spoon1", "spoon2"], kind=True)
    spoons_e = described_group(world, ["spoon3", "spoon4"], kind=True)
    return world, spoons_h, spoons_e


def fig_4_7(problems, seed=1, config=CFG):
    """The state of Fig. 4.7 [F95 p. 104 (img)]: cup-cup in the Worldview without a fight,
    the spoon-group correspondence in without a fight, and the cup-glass newcomer created
    [F95 pp. 101, 105]. Returns (world, cup-cup, spoons, cup-glass)."""
    world, spoons_h, spoons_e = chapter_4_table(problems, seed, config)
    cup_cup = C.create(world, "cup1", "cup2")
    WV.compete(world, cup_cup, A_FIG_4_7)
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    WV.compete(world, spoons, A_FIG_4_7)
    cup_glass = C.create(world, "cup1", "small-glass1")
    return world, cup_cup, spoons, cup_glass


def names(cps):
    return [c.name for c in cps]


# ---------------------------------------------------------------------------------------------
# The Worldview is the flagged subset; rivals (D-044).


def test_the_worldview_is_the_set_of_flagged_correspondences(problems):
    """"The Worldview is a set of noncontradictory mappings of objects and groups of
    objects onto one another." [F95 p. 70]"""
    world, _, _ = chapter_4_table(problems)
    assert world.workspace.worldview() == [] and WV.summary(world) == []
    cp = C.create(world, "cup1", "cup2")
    assert WV.rivals_of(world, cp) == []
    WV.admit(world, cp)
    assert cp.in_worldview and world.workspace.worldview() == [cp]
    assert WV.summary(world) == [("cp1", "cup1", "cup2")]
    assert world.workspace.in_worldview("cup1") and world.workspace.in_worldview("cup2")


def test_rivals_share_an_end_in_the_order_built(problems):
    """"Rival correspondences start or finish at the same end point" [F95 p. 101]; "no
    object can simultaneously correspond to two or more different objects" [F95 p. 70]."""
    world, _, _ = chapter_4_table(problems)
    cup_cup = C.create(world, "cup1", "cup2")
    fork_knife = C.create(world, "fork1", "knife2")
    spoon_glass = C.create(world, "spoon1", "small-glass1")
    for cp in (cup_cup, fork_knife, spoon_glass):
        WV.admit(world, cp)
    cup_glass = C.create(world, "cup1", "small-glass1")
    assert WV.rivals_of(world, cup_glass) == [cup_cup, spoon_glass]
    assert WV.rivals_of(world, C.create(world, "fork2", "fork3")) == []
    assert WV.conflicts(world, cup_cup, cup_cup) is False
    assert WV.violations(world) == []


def test_usurping_inside_only_when_the_group_correspondence_stands(problems):
    """"if the program maps a group of objects, A, onto another group of objects, B, as
    wholes, then the individual objects inside A can map only to objects inside B and not
    to some other object elsewhere on the table" [F95 p. 71] (D-044)."""
    world, spoons_h, spoons_e = chapter_4_table(problems)
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    WV.admit(world, spoons)
    outside = C.create(world, "spoon1", "fork3")
    inside = C.create(world, "spoon2", "spoon3", parent=spoons)
    elsewhere = C.create(world, "fork1", "knife2")
    assert WV.rivals_of(world, outside) == [spoons]
    assert WV.rivals_of(world, inside) == []
    assert WV.rivals_of(world, elsewhere) == []
    # The other way round: a group correspondence proposed over a standing escape.
    assert WV.conflicts(world, outside, spoons) and WV.conflicts(world, spoons, outside)
    # A group and one of its members mapped elsewhere share no end but conflict too.
    to_glass = C.create(world, "spoon4", "cup2")
    assert WV.rivals_of(world, to_glass) == [spoons]


def test_members_of_a_group_map_freely_when_the_group_itself_is_not_mapped(problems):
    """In Blockage 3 the two grouped glasses are mapped separately, to the touched cup and
    to a far glass, "offset only by the incoherence of the Worldview they create" [F95
    p. 124] (D-044)."""
    world, spoons_h, spoons_e = chapter_4_table(problems)
    a = C.create(world, "spoon1", "fork3")
    b = C.create(world, "spoon2", "cup2")
    held = WV.compete(world, a, A_COLD)
    assert held.rivals == [] and a.in_worldview
    held = WV.compete(world, b, A_COLD)
    assert held.rivals == [] and b.in_worldview
    assert WV.violations(world) == []


# ---------------------------------------------------------------------------------------------
# Coherence and parallelism (D-042, D-043).


def test_the_chapter_4_incoherent_and_coherent_pairs(problems):
    """"one correspondence goes from an object inside one group to an object inside another
    group, and the second correspondence (between the two cups) starts inside one group
    and ends on an isolated object" [F95 p. 101]: incoherent; the cup-glass version is
    "a coherent Worldview" [F95 p. 105] (Figs. 3.12 and 3.13 [F95 pp. 74-75 (img)])."""
    world, spoons_h, spoons_e = chapter_4_table(problems)
    cup_cup = C.create(world, "cup1", "cup2")
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    cup_glass = C.create(world, "cup1", "small-glass1")
    forks = C.create(world, "fork1", "knife2")
    assert WV.pair_status(world, cup_cup, spoons) == "incoherent"
    assert WV.pair_status(world, spoons, cup_cup) == "incoherent"
    assert WV.pair_status(world, cup_glass, spoons) == "coherent"
    assert WV.pair_status(world, cup_glass, forks) == "coherent"
    assert WV.coherence_pairs(world, [cup_cup, spoons, forks]) == (
        [("cp2", "cp4")], [("cp1", "cp2"), ("cp1", "cp4")])
    assert WV.coherence_pairs(world, [spoons, cup_glass]) == ([("cp2", "cp3")], [])


def test_the_pair_is_read_under_either_alignment_of_the_ends(problems):
    """The ends follow the picking order, not the sides (D-005), so cup1-glass and
    knife2-fork1 are the same coherent pair as cup1-glass and fork1-knife2 (D-042)."""
    world, _, _ = chapter_4_table(problems)
    cup_glass = C.create(world, "cup1", "small-glass1")
    reversed_ = C.create(world, "knife2", "fork1")
    assert WV.pair_status(world, cup_glass, reversed_) == "coherent"
    assert WV.pair_status(world, reversed_, cup_glass) == "coherent"


def test_separate_subworlds_are_no_pair_and_inner_correspondences_count_through_their_parent():
    """"There can be two (or more) physically separate groups of correspondences on the
    table" [F95 p. 75] with no interaction; and correspondences inside a group
    correspondence [F95 p. 108] are counted through it (D-042, D-043)."""
    objects = [("a1", "fork", 10, 10), ("a2", "fork", 16, 10), ("b1", "fork", 10, 90),
               ("b2", "fork", 16, 90), ("c1", "cup", 90, 10), ("c2", "cup", 96, 10),
               ("d1", "cup", 90, 90), ("d2", "cup", 96, 90)]
    world = World(synthetic(objects, "a1"), CFG, 1)
    notice_all(world)
    groups = {}
    for name, members in (("a", ["a1", "a2"]), ("b", ["b1", "b2"]), ("c", ["c1", "c2"]),
                          ("d", ["d1", "d2"])):
        groups[name] = described_group(world, members)
    left = C.create(world, "a1", "b1")
    right = C.create(world, "c1", "d1")
    assert WV.pair_status(world, left, right) is None
    assert WV.coherence_pairs(world, [left, right]) == ([], [])
    whole = C.create(world, groups["a"].name, groups["b"].name)
    inner = C.create(world, "a2", "b2", parent=whole)
    assert WV.top_level([left, right, whole, inner]) == [left, right, whole]
    assert WV.coherence_pairs(world, [whole, inner, right]) == ([], [])
    assert not WV.parallel(whole, inner)


def test_the_three_group_example_of_page_74():
    """"if there were three groups of objects on the table, the program would penalize a
    Worldview consisting of four strong correspondences from objects in group 1 to objects
    in group 2 and one correspondence from an object in group 1 to another object in group
    3" [F95 p. 74]; coherence "would be much higher in the above example if all of the
    correspondences went from objects in group I to objects in group 2" [F95 p. 74]."""
    objects = ([(f"a{i}", "fork", 10 + 6 * i, 10) for i in range(5)]
               + [(f"b{i}", "fork", 10 + 6 * i, 90) for i in range(5)]
               + [(f"c{i}", "fork", 70 + 6 * i, 90) for i in range(5)])
    world = World(synthetic(objects, "a0"), CFG, 1)
    notice_all(world)
    for members in ([f"a{i}" for i in range(5)], [f"b{i}" for i in range(5)],
                    [f"c{i}" for i in range(5)]):
        described_group(world, members)
    four_and_one = [C.create(world, f"a{i}", f"b{i}") for i in range(4)]
    four_and_one.append(C.create(world, "a4", "c4"))
    penalized = WV.rate(world, four_and_one)
    assert len(penalized.coherent_pairs) == 6 and len(penalized.incoherent_pairs) == 4
    assert penalized.coherence == 6 * CFG.coherence_reward - 4 * CFG.incoherence_penalty
    for cp in four_and_one:
        world.workspace.remove_correspondence(cp)
    all_five = [C.create(world, f"a{i}", f"b{i}") for i in range(5)]
    ideal = WV.rate(world, all_five)
    assert len(ideal.coherent_pairs) == 10 and ideal.incoherent_pairs == []
    assert ideal.coherence == 10 * CFG.coherence_reward
    assert ideal.total > penalized.total + 100


def test_parallel_correspondences_share_a_slippage_label(problems):
    """"One correspondence is said to be parallel to another if the slippages that support
    the first have the same labels as the slippages that support the second." [F95 p. 74]
    A left-to-right slippage "has an 'opposite' label" [F95 p. 74], and a parallel one
    "would have a slippage with the same label (e.g., a right-to-left or an above-to-below
    slippage)" [F95 p. 75] (D-043)."""
    world, _, _ = chapter_4_table(problems)
    left_right = C.create(world, "fork1", "knife2")
    right_left = C.create(world, "spoon2", "spoon3")
    cup_glass = C.create(world, "cup1", "small-glass1")
    assert "opposite" in WV.parallel_labels(left_right)
    assert "opposite" in WV.parallel_labels(right_left)
    assert WV.parallel(left_right, right_left)
    assert not WV.parallel(left_right, cup_glass)
    assert WV.parallel_pairs([left_right, right_left, cup_glass]) == [("cp1", "cp2")]
    rating = WV.rate(world, [left_right, right_left, cup_glass])
    assert rating.parallelism == CFG.parallel_points


def test_count_identities_and_levels_do_not_make_parallels():
    """Every correspondence between two single objects carries the "one"-"one" identity,
    so it says nothing about "the same type of conceptual slippage" [FCCA p. 387]; levels
    do not mix (D-043)."""
    one_one = C.Slippage(C.NUMBER, "one", "one", ("sameness",))
    a = C.Correspondence("x", "y", name="cp1", slippages=[one_one])
    b = C.Correspondence("u", "v", name="cp2", slippages=[one_one])
    assert WV.parallel_labels(a) == [] and not WV.parallel(a, b)
    flip = C.Slippage(C.POSITION, "left", "right", ("opposite", "diagonal symmetry"))
    a.slippages.append(flip)
    b.slippages.append(C.Slippage(C.POSITION, "above", "below", ("opposite", "diagonal symmetry")))
    assert WV.parallel(a, b)
    b.parent = C.Correspondence("g1", "g2", name="cp0")
    assert not WV.parallel(a, b)
    assert WV.parallel_labels(a) == ["opposite", "diagonal symmetry"]


def test_the_rating_sums_strength_coherence_and_parallelism(problems):
    """"The evaluation of the Worldview is the sum of the following:" [F95 p. 74] the
    strengths, the coherence and the parallelism (D-042, D-043)."""
    world, spoons_h, spoons_e = chapter_4_table(problems)
    cup_cup = C.create(world, "cup1", "cup2")
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    rating = WV.rate(world, [cup_cup, spoons])
    assert rating.names == ["cp1", "cp2"]
    assert rating.strength == pytest.approx(cup_cup.rating + spoons.rating)
    assert rating.incoherent_pairs == [("cp1", "cp2")] and not rating.coherent
    assert rating.coherence == -CFG.incoherence_penalty
    assert rating.total == pytest.approx(rating.strength + rating.coherence + rating.parallelism)
    assert WV.rate(world, []).total == 0.0 and WV.rate(world, []).coherent
    for _ in range(3):
        assert WV.current(world).total == 0.0   # nothing in the Worldview yet


def test_the_buridan_profile_knob_raises_the_penalty(problems):
    """French raised the penalty for Buridan 2-13, "more strongly resistant to incoherent
    mappings" [F95 p. 130]: the `buridan` profile's knob (D-042; ANSWERS.md 7)."""
    heavier = v2(incoherence_penalty=100)
    world, spoons_h, spoons_e = chapter_4_table(problems, config=heavier)
    cup_cup = C.create(world, "cup1", "cup2")
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    assert WV.rate(world, [cup_cup, spoons]).coherence == -100


# ---------------------------------------------------------------------------------------------
# The competition's probability (D-040).


def test_the_rival_chance_has_the_form_of_the_law_of_temperature():
    """R^a / (R^a + O^a): at a = 1 the ratings' shares; at a = 6 French's 6-to-3 example
    gives 0.985 [F95 p. 93]; at a = 0.1 nearly even; equal ratings always a half;
    ratings below 1 are floored (D-040, D-060)."""
    assert WV.rival_chance(3, 6, 1.0) == pytest.approx(2 / 3)
    assert WV.rival_chance(3, 6, A_COLD) == pytest.approx(46656 / (46656 + 729), abs=1e-6)
    assert 0.5 < WV.rival_chance(3, 6, A_HOT) < 0.52
    assert WV.rival_chance(50, 50, A_COLD) == 0.5 == WV.rival_chance(50, 50, A_HOT)
    assert WV.rival_chance(0, 1, 1.0) == 0.5 and WV.rival_chance(-5, 1, 1.0) == 0.5
    assert WV.rival_chance(10, 100, A_COLD) > 0.999
    assert WV.rival_chance(100, 10, A_COLD) < 0.001
    assert WV.rival_chance(100, 150, A_COLD) == pytest.approx(1 - WV.rival_chance(150, 100, A_COLD))


def test_an_uncontested_newcomer_enters_without_a_fight_and_without_a_draw(problems):
    """"Since this correspondence is not a rival to any correspondence already in the
    Worldview, it is put in the Worldview without a fight." [F95 p. 101] The entry jolts
    its slippages' label nodes (D-030) and counts as a Worldview change (D-040)."""
    world, _, _ = chapter_4_table(problems)
    cup_cup = C.create(world, "cup1", "cup2")
    before = world.slipnet.activation_of("sameness")
    draws = world.rng.draws
    held = WV.compete(world, cup_cup, A_FIG_4_7)
    assert world.rng.draws == draws
    assert held.rivals == [] and held.chance == 1.0 and held.rival_wins
    assert held.old_rating == 0.0 and held.rival_rating == pytest.approx(cup_cup.rating)
    assert cup_cup.in_worldview and cup_cup.wins == 0 and cup_cup.bonus == 0
    labels = [label for s in cup_cup.slippages for label in s.labels]
    assert labels.count("sameness") == 2   # the type identity and the count identity
    assert world.slipnet.activation_of("sameness") == before + 2 * CFG.jolt_label
    change = world.workspace.changes[-1]
    assert (change.added, change.removed, change.reason) == (["cp1"], [], "no rival")
    assert change.rating == pytest.approx(cup_cup.rating) and change.coherent
    assert world.workspace.competitions == [held]
    assert WV.compete(world, cup_cup, A_FIG_4_7) is None   # already in: the codelet fizzles


# ---------------------------------------------------------------------------------------------
# The Fig. 4.8 case (D-040, D-042).


def test_the_fig_4_8_case_as_french_explains_it(problems):
    """"the cup/small-glass correspondence has won the fight, because the structure
    rating of a coherent Worldview containing a cup-glass correspondence is higher than
    that of an incoherent Worldview containing a cup-cup correspondence, even though a
    cup-cup correspondence is a priori stronger than a cup-glass correspondence" [F95
    p. 105]."""
    world, cup_cup, spoons, cup_glass = fig_4_7(problems)
    assert names(world.workspace.worldview()) == ["cp1", "cp2"]
    assert C.static_points(world, cup_cup) > C.static_points(world, cup_glass)
    assert WV.rivals_of(world, cup_glass) == [cup_cup]
    old = WV.current(world)
    rival = WV.rate_subjunctively(world, [cup_cup], [cup_glass])
    assert not old.coherent and old.incoherent_pairs == [("cp1", "cp2")]
    assert rival.coherent and rival.coherent_pairs == [("cp2", "cp3")]
    assert rival.total > old.total
    assert rival.total - old.total > CFG.incoherence_penalty   # the coherence terms decide
    # The subjunctive rating leaves the Worldview and the strengths as they were.
    assert names(world.workspace.worldview()) == ["cp1", "cp2"] and not cup_glass.in_worldview
    assert WV.current(world).total == pytest.approx(old.total)


def test_the_rival_wins_the_fig_4_8_fight_at_the_engines_chance(problems):
    """Over 200 seeds at the temperature of Fig. 4.7 (35.8), the cup-glass Rival Worldview
    wins within four binomial standard deviations of the chance the engine reports, "a
    probabilistically determined victory" [F95 p. 105] (D-040); one draw per fight."""
    wins, chances = 0, []
    for seed in range(200):
        world, cup_cup, spoons, cup_glass = fig_4_7(problems, seed)
        draws = world.rng.draws
        held = WV.compete(world, cup_glass, A_FIG_4_7)
        assert world.rng.draws == draws + 1
        assert held.rivals == [cup_cup] and held.newcomer is cup_glass
        chances.append(held.chance)
        wins += held.rival_wins
    p = sum(chances) / len(chances)
    assert 0.6 < p < 0.95
    sd = math.sqrt(200 * p * (1 - p))
    assert abs(wins - 200 * p) <= 4 * sd


def test_after_the_fight_the_loser_leaves_the_worldview_but_stays_in_the_workspace(problems):
    """"The fact that the Worldview has just changed (the cup-cup correspondence has been
    removed, and the cup-glass correspondence has been added)" [F95 p. 105]; "The loser,
    although not in the Worldview, remains nonetheless in the Workspace with the potential
    to compete again." [F95 p. 72] Winners gain the bonus, the loser the penalty [F95
    p. 72] (D-038, D-041)."""
    for seed in range(40):
        world, cup_cup, spoons, cup_glass = fig_4_7(problems, seed)
        held = WV.compete(world, cup_glass, A_FIG_4_7)
        if held.rival_wins:
            break
    else:
        pytest.fail("no seed in 40 let the rival win")
    assert names(world.workspace.worldview()) == ["cp2", "cp3"]
    assert not cup_cup.in_worldview and WV.alive(world, cup_cup)
    assert names(world.workspace.correspondences) == ["cp1", "cp2", "cp3"]
    assert (cup_cup.wins, cup_cup.losses, cup_cup.bonus) == (0, 1, -CFG.loss_penalty)
    assert (cup_glass.wins, cup_glass.bonus) == (1, CFG.win_bonus)
    assert (spoons.wins, spoons.bonus) == (1, CFG.win_bonus)
    assert world.workspace.competitions_of("cup1") == (1, 1)
    assert world.workspace.competitions_of("cup2") == (0, 1)
    assert world.workspace.competitions_of("small-glass1") == (1, 0)
    change = world.workspace.changes[-1]
    assert (change.added, change.removed, change.reason) == (["cp3"], ["cp1"], "won")
    assert change.coherent and WV.current(world).coherent
    assert WV.violations(world) == []
    # The loser may compete again: it is a rival of the standing cup-glass now.
    assert WV.rivals_of(world, cup_cup) == [cup_glass]
    assert WV.compete(world, cup_cup, A_FIG_4_7) is not None


def test_when_the_old_worldview_wins_the_newcomer_stays_tentative(problems):
    """"it becomes a little harder for the loser to compete the next time around" [F95
    p. 72]: the newcomer loses points, the standing correspondences gain (D-038)."""
    for seed in range(40):
        world, cup_cup, spoons, cup_glass = fig_4_7(problems, seed)
        held = WV.compete(world, cup_glass, A_FIG_4_7)
        if not held.rival_wins:
            break
    else:
        pytest.fail("no seed in 40 let the old Worldview win")
    assert names(world.workspace.worldview()) == ["cp1", "cp2"]
    assert not cup_glass.in_worldview and WV.alive(world, cup_glass)
    assert (cup_glass.losses, cup_glass.bonus) == (1, -CFG.loss_penalty)
    assert (cup_cup.wins, cup_cup.bonus) == (1, CFG.win_bonus)
    assert (spoons.wins, spoons.bonus) == (1, CFG.win_bonus)
    assert world.workspace.changes[-1].reason == "no rival"   # nothing changed by the fight
    assert len(world.workspace.competitions) == 3


def test_a_fight_is_reproducible_from_the_seed_and_sharp_when_cold(problems):
    """Same seed, same outcome (D-064); at T = 0 a large margin is "essentially
    deterministic" [F95 p. 93] (D-060)."""
    first = [WV.compete(*fig_4_7(problems, 7)[::3], A_FIG_4_7).rival_wins for _ in range(3)]
    assert len(set(first)) == 1
    outcomes = []
    for seed in range(30):
        world, cup_cup, spoons, cup_glass = fig_4_7(problems, seed)
        outcomes.append(WV.compete(world, cup_glass, A_COLD).rival_wins)
    assert sum(outcomes) >= 26   # the chance at a = 6 for 188 against 134 is about 0.89


# ---------------------------------------------------------------------------------------------
# The invariants (D-040, D-044).


def test_no_item_is_an_end_of_two_worldview_correspondences_however_the_fights_go(problems):
    """"no object or group of objects is the end object for two distinct correspondences"
    [F95 p. 71]; "the selected items all be mutually compatible" [FCCA p. 390]: after any
    sequence of competitions over the central configuration, at any temperature."""
    for seed in range(6):
        world, spoons_h, spoons_e = chapter_4_table(problems, seed)
        pairs = [(a, b) for a in HENRY_ROW + ["cup2"] for b in ELIZA_ROW + ["cup2"] if a != b]
        pairs += [(spoons_h.name, spoons_e.name), (spoons_h.name, "cup2"),
                  ("spoon1", "spoon3"), ("spoon1", "fork3")]
        for a, b in pairs:
            cp = C.create(world, a, b)
            if cp is None:
                continue
            temperature = 100.0 * world.rng.random()
            WV.compete(world, cp, TP.exponent_at(temperature, CFG))
            assert WV.violations(world) == []
            ends = [e for c in world.workspace.worldview() for e in c.ends]
            assert len(ends) == len(set(ends))
        assert len(world.workspace.worldview()) >= 2
        assert len(world.workspace.competitions) == sum(1 for c in world.workspace.correspondences)


def test_an_inner_correspondence_may_stand_beside_its_parent(problems):
    """"the individual objects inside A can map only to objects inside B" [F95 p. 71]: a
    correspondence inside the group correspondence is no rival of it [F95 p. 108]."""
    world, spoons_h, spoons_e = chapter_4_table(problems)
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    WV.compete(world, spoons, A_COLD)
    inner = C.create(world, "spoon1", "spoon4", parent=spoons)
    held = WV.compete(world, inner, A_COLD)
    assert held.rivals == [] and inner.in_worldview and spoons.in_worldview
    assert WV.violations(world) == []


# ---------------------------------------------------------------------------------------------
# Fading and the clean-up (D-041).


def test_a_repeated_loser_fades_to_the_threshold(problems):
    """"each time a structure is involved in a competition and loses, its salience rating
    drops until finally, if this happens enough times, it gets removed from the Workspace"
    [F95 p. 73]: the penalty accumulates and the removal chance rises below the threshold
    (D-038, D-041)."""
    world, _, _ = chapter_4_table(problems)
    cp = C.create(world, "cup1", "cup2")
    assert WV.removal_chance(world, cp) == 0.0
    chances = []
    for _ in range(12):
        C.award(world, cp, False)
        chances.append(WV.removal_chance(world, cp))
    assert chances[0] == 0.0 and chances[-1] == 1.0
    assert all(b >= a for a, b in zip(chances, chances[1:]))
    assert sum(1 for c in chances if 0.0 < c < 1.0) >= 2   # it fades, it does not fall
    assert cp.rating == 0.0 and cp.losses == 12


def test_clean_up_removes_below_the_threshold_at_the_linear_chance(problems):
    """"it is (probabilistically) removed from the Workspace and then must be built again"
    [F95 p. 73] (D-041): over 300 seeds a correspondence at half the threshold goes about
    half the time, within four binomial standard deviations; one coin each; a Worldview
    correspondence is never a candidate; one at rating 0 goes without a draw."""
    removed = 0
    for seed in range(300):
        world, _, _ = chapter_4_table(problems, seed)
        kept = C.create(world, "fork1", "knife2")
        WV.admit(world, kept)
        faded = C.create(world, "cup1", "cup2")   # it holds the touched cup: 15 Hobj points
        faded.bonus = -(faded.static + faded.dynamic) + CFG.removal_threshold / 2
        assert WV.removal_chance(world, faded) == pytest.approx(0.5)
        assert WV.candidates(world) == [faded]
        draws = world.rng.draws
        gone = WV.clean_up(world)
        assert world.rng.draws == draws + 1
        if gone:
            assert gone == [(faded, 0.5)] and not WV.alive(world, faded)
            removed += 1
        assert WV.alive(world, kept)
    sd = math.sqrt(300 * 0.25)
    assert abs(removed - 150) <= 4 * sd
    world, _, _ = chapter_4_table(problems)
    dead = C.create(world, "cup1", "cup2")
    dead.bonus = -1000
    draws = world.rng.draws
    assert WV.clean_up(world) == [(dead, 1.0)] and world.rng.draws == draws
    assert names(world.workspace.correspondences) == []
    assert C.create(world, "cup1", "cup2").name == "cp2"   # names are never reused


def test_removal_takes_the_correspondences_inside_and_logs_a_worldview_change(problems):
    """A correspondence inside a removed group correspondence [F95 p. 108] goes with it;
    one removed from the Worldview is a change (D-041)."""
    world, spoons_h, spoons_e = chapter_4_table(problems)
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    WV.admit(world, spoons)
    inner = C.create(world, "spoon1", "spoon3", parent=spoons)
    WV.admit(world, inner)
    other = C.create(world, "cup1", "cup2")
    assert WV.remove(world, spoons) == [spoons, inner]
    assert names(world.workspace.correspondences) == ["cp3"]
    assert not spoons.in_worldview and not inner.in_worldview
    change = world.workspace.changes[-1]
    assert (change.added, change.removed, change.reason) == ([], ["cp1", "cp2"], "removed")
    assert WV.alive(world, other) and not WV.alive(world, spoons)
    assert WV.compete(world, spoons, A_COLD) is None   # a codelet holding it fizzles


def test_the_losers_destroyed_lesion_removes_the_loser_at_once(problems):
    """Copycat's "gladiatorial" competition [F95 p. 70], the lesion of FCCA's criterion
    [FCCA p. 365] (D-041): the loser does not stay."""
    lesioned = v2(losers_destroyed=True)
    for seed in range(40):
        world, cup_cup, spoons, cup_glass = fig_4_7(problems, seed, lesioned)
        held = WV.compete(world, cup_glass, A_FIG_4_7)
        loser = cup_cup if held.rival_wins else cup_glass
        assert not WV.alive(world, loser)
        assert len(world.workspace.correspondences) == 2
        if held.rival_wins:
            break
    else:
        pytest.fail("no seed in 40 let the rival win")


# ---------------------------------------------------------------------------------------------
# Re-reading the Worldview (D-040).


def test_check_worldview_lists_the_tested_correspondences_that_now_outrate_their_rivals(problems):
    """A check-worldview codelet re-reads the ratings and finds the tentative
    correspondences worth a cp-compete (the legend's label [F95 p. 96 (img)]; D-040)."""
    world, cup_cup, spoons, cup_glass = fig_4_7(problems)
    assert WV.challengers(world) == []          # cup-glass has not passed its test
    cup_glass.passed = cup_glass.tested = True
    assert WV.challengers(world) == []          # and rates below cup-cup, its rival
    cup_cup.bonus = -40
    assert WV.challengers(world) == [cup_glass]
    free = C.create(world, "fork1", "knife2")
    free.passed = True
    assert WV.challengers(world) == [cup_glass, free]
    WV.compete(world, free, A_COLD)
    assert WV.challengers(world) == [cup_glass]


# ---------------------------------------------------------------------------------------------
# Orphans dropped in one sweep (D-041; the run loop's drop_orphans).


def test_two_orphans_are_dropped_together_without_rating_a_gone_end(problems):
    """Two Worldview correspondences whose group ends have both left the Workspace (lost
    or absorbed, D-025, D-026) are removed in one sweep: neither is rated with an end that
    is gone (a KeyError before), one change is logged naming both, and both are gone."""
    world, spoons_h, spoons_e = chapter_4_table(problems)
    henry_row = world.workspace.group_with_members(HENRY_ROW)
    eliza_row = world.workspace.group_with_members(ELIZA_ROW)
    spoons = C.create(world, spoons_h.name, spoons_e.name)
    WV.compete(world, spoons, A_FIG_4_7)
    rows = C.create(world, henry_row.name, eliza_row.name)
    WV.compete(world, rows, A_FIG_4_7)
    knives = C.create(world, "knife1", "knife2")
    assert spoons.in_worldview and rows.in_worldview and not knives.in_worldview
    world.workspace.remove_group(spoons_h, "lost")
    world.workspace.remove_group(eliza_row, "lost")
    changes = len(world.workspace.changes)
    gone = world.drop_orphans()
    assert names(gone) == [spoons.name, rows.name]
    assert names(world.workspace.correspondences) == [knives.name]
    assert world.workspace.worldview() == []
    change = world.workspace.changes[-1]
    assert len(world.workspace.changes) == changes + 1
    assert change.removed == [spoons.name, rows.name] and change.reason == "removed"
    assert world.drop_orphans() == []

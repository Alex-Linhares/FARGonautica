"""Bottom-up groups (D-024 to D-026, D-069): the three kinds on Henry's row and French's
examples [F95 p. 65; F95 p. 125], the three graded stages with sameness >> shared-ISA >>
mixed over seeds, a group's strength, fights between incompatible groups, and the subgroup
rule: four or more identical objects give one group under the fix and every intermediate
grouping under `french-faults` [F95 p. 140].
"""

import math

import pytest

from tabletop import build_object as BO
from tabletop import describe as DS
from tabletop import groups as GR
from tabletop import problems as P
from tabletop import temperature as TP
from tabletop.config import PROFILES
from tabletop.rng import Rng
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
FAULTS = V2_PROFILES["french-faults"]
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
A_COLD, A_WARM = 6.0, 1.28  # a(0) and a(80) (D-060)


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


def chain(world, group, start):
    """Run the find-neighbor chain of p. 80 from a member until the chain is known."""
    frontier = [start]
    while frontier:
        for n, _ in DS.find_neighbours(world, group, frontier.pop(0)):
            frontier.append(n)
    assert group.chain_known()


def henry_world(problems, config=CFG, seed=1):
    """Surround 5 with Henry's row as a gestalt group whose chain is known, placed directly
    (the flood fill finds the whole row about three times in four, D-022)."""
    world = World(problems["surround-05"], config, seed)
    group = GR.Group(members=list(HENRY_ROW))
    world.workspace.add_group(group)
    chain(world, group, "fork1")
    return world, group


def row_of(type_, n, gap=9.0):
    return [(f"{type_.replace(' ', '-')}{i}", type_, 10.0 + gap * i, 20.0) for i in range(1, n + 1)]


def accrete(world, parent, exponent=A_COLD, limit=60):
    """Propose and build every candidate run, pass after pass, until a pass builds nothing,
    as a run of build-object codelets would (D-024)."""
    for _ in range(limit):
        built = 0
        for members, base in BO.candidate_runs(world, parent):
            proposal = BO.propose(world, parent, members, base)
            if proposal is not None and BO.build_object(world, proposal, exponent) is not None:
                built += 1
        if built == 0:
            return
    raise AssertionError("accretion did not settle")


# ---------------------------------------------------------------------------------------------
# Kinds (D-024).


@pytest.mark.parametrize("types, kind, node", [
    (("fork", "fork"), GR.SAMENESS, "sameness"),
    (("spoon", "spoon", "spoon"), GR.SAMENESS, "sameness"),
    (("fork", "knife"), GR.SHARED_ISA, "silverware"),
    (("fork", "knife", "spoon"), GR.SHARED_ISA, "silverware"),
    (("cup", "saucer"), GR.SHARED_ISA, "crockery"),
    (("cup", "plate"), GR.SHARED_ISA, "crockery"),
    (("cup", "small glass"), GR.SHARED_ISA, "liquid holder"),
    (("salt shaker", "pepper shaker"), GR.SHARED_ISA, "closed object"),
    (("cup", "spoon"), GR.MIXED, None),
    (("spoon", "small glass"), GR.MIXED, None),
    (("knife", "cup"), GR.MIXED, None),
])
def test_the_kind_of_a_group_by_its_members_types(types, kind, node):
    """"sameness groups (i.e., groups made up of identical objects ...)", "groups whose
    objects share a common ISA category ('shared-ISA groups')" such as "silverware groups
    or crockery groups", and groups "whose objects have nothing in common", French's
    example being "a cup-spoon grouping" [F95 p. 65]; "a spoon is conceptually very far
    from a glass" [F95 p. 125] (D-024)."""
    objects = [(f"o{i}", t, 10.0 + 9.0 * i, 20.0) for i, t in enumerate(types)]
    problem = synthetic(objects, "o0")
    assert GR.kind_of(problem, [o[0] for o in objects], CFG) == (kind, node)


def test_grouping_categories_are_bounded_by_link_length():
    """A spoon's grouping categories stop at silverware: the long spoon-liquid holder link
    (35, D-009) is beyond `shared_isa_max_length`, which is what keeps cup-spoon mixed
    (D-024)."""
    assert GR.grouping_categories("spoon", CFG) == ["silverware"]
    assert GR.grouping_categories("cup", CFG) == ["liquid holder", "crockery"]
    assert GR.grouping_categories("salt shaker", CFG) == ["closed object"]
    assert CFG.shared_isa_max_length == 30


def test_group_type_is_the_one_type_of_a_sameness_group(problems):
    """The type a subgroup rule compares (D-026); None for any other group."""
    s5 = problems["surround-05"]
    assert GR.group_type(s5, GR.Group(members=["fork1", "fork2"], kind=GR.SAMENESS)) == "fork"
    assert GR.group_type(s5, GR.Group(members=["fork1", "knife1"], kind=GR.SHARED_ISA)) is None
    assert GR.group_type(s5, GR.Group(members=["fork1", "fork2"])) is None


# ---------------------------------------------------------------------------------------------
# Candidates and proposals (D-024, D-027).


def test_candidate_runs_are_the_neighbour_pairs_then_a_group_plus_an_adjacent_member(problems):
    """"build-object (fork1, fork2)" takes two objects [F95 p. 77]; groups grow "by
    progressively adding adjacent objects to already-existing groups" [F95 p. 140]
    (D-024). Before the chain is known nothing is proposable."""
    world = World(problems["surround-05"], CFG, 1)
    parent = GR.find_group_around(world, "fork1")
    assert BO.candidate_runs(world, parent) == []
    chain(world, parent, "fork1")
    pairs = [[a, b] for a, b in zip(HENRY_ROW, HENRY_ROW[1:])]
    assert BO.candidate_runs(world, parent) == [(p, None) for p in pairs]
    forks = BO.build_object(world, BO.propose(world, parent, ["fork1", "fork2"]), A_COLD)
    assert forks is not None
    runs = BO.candidate_runs(world, parent)
    assert runs[:5] == [(p, None) for p in pairs]
    assert runs[5:] == [(["fork1", "fork2", "knife1"], forks)]


def test_propose_reads_the_kind_and_the_links_to_test(problems):
    """A pair tests itself; a group plus one tests the new member against the member it
    joins (D-024); a proposal is compared by identity."""
    world, parent = henry_world(problems)
    pair = BO.propose(world, parent, ["cup1", "spoon1"])
    assert (pair.kind, pair.node, pair.new_links, pair.base) == (GR.MIXED, None, [("cup1", "spoon1")], None)
    assert pair.parent is parent and pair.stage == 0 and not pair.done()
    spoons = BO.build_object(world, BO.propose(world, parent, ["spoon1", "spoon2"]), A_COLD)
    longer = BO.propose(world, parent, ["cup1", "spoon1", "spoon2"], base=spoons)
    assert longer.members == ["cup1", "spoon1", "spoon2"]
    assert longer.new_links == [("spoon1", "cup1")] and longer.base is spoons
    assert longer.kind == GR.MIXED
    assert BO.propose(world, parent, ["cup1", "spoon1"]) is not pair


def test_an_existing_bottom_up_group_is_not_proposed_again_but_a_gestalt_one_is(problems):
    """A bottom-up group over the same objects as a gestalt group is a new structure, "a
    much more solid structure" [F95 p. 125]; the same bottom-up group twice is nothing
    (D-024)."""
    cups = row_of("cup", 4)
    ids = [c[0] for c in cups]
    world = World(synthetic(cups, "cup1"), CFG, 1)
    parent = GR.find_group_around(world, "cup1")
    assert parent.members == ids
    chain(world, parent, "cup1")
    whole = BO.propose(world, parent, ids)
    assert whole is not None and whole.kind == GR.SAMENESS
    built = BO.build_object(world, whole, A_COLD)
    assert built is not None and built is not parent and built.parent is parent
    assert BO.propose(world, parent, ids) is None
    assert world.workspace.group_with_members(ids, bottom_up=False) is parent
    assert world.workspace.group_with_members(ids, bottom_up=True) is built
    assert GR.find_group_around(world, "cup1") is None  # the gestalt bunch is known too


# ---------------------------------------------------------------------------------------------
# The three stages (D-024).


def test_stage_0_is_the_proximity_coin_and_spends_no_draw_on_a_touching_pair(problems):
    """build-obj-0 checks "the physical ... distances between prospective group members"
    [FCCA p. 384] with the coin of D-022: the forks (0.3 widths) pass without a draw; the
    Blockage 10 glasses (2.64 widths) never pass (D-024)."""
    world, parent = henry_world(problems)
    pair = BO.propose(world, parent, ["fork1", "fork2"])
    draws = world.rng.draws
    assert BO.run_stage(world, pair, A_COLD) is True
    assert world.rng.draws == draws and pair.stage == 1 and pair.results == [(0, 1.0, True)]
    b10 = World(problems["blockage-10"], CFG, 1)
    gestalt = GR.Group(members=["small-glass2", "small-glass4"])
    b10.workspace.add_group(gestalt)
    gestalt.neighbours = {"small-glass2": [("small-glass4", "right")], "small-glass4": [("small-glass2", "left")]}
    gestalt.visited = list(gestalt.members)
    glasses = BO.propose(b10, gestalt, ["small-glass2", "small-glass4"])
    assert BO.run_stage(b10, glasses, A_COLD) is False
    assert glasses.done() and glasses.results == [(0, 0.0, False)]
    assert BO.run_stage(b10, glasses, A_COLD) is False  # an aborted proposal stays aborted


def test_stage_1_passes_sameness_almost_always_shared_isa_usually_and_mixed_very_rarely(problems):
    """"It will almost always discover 'sameness groups'", "will usually see ... shared-ISA
    groups", "will only very rarely build groups whose objects have nothing in common"
    [F95 p. 65] (D-024). Seeded, at a(80) = 1.28 and at a(0) = 6: each frequency within
    four binomial standard deviations of the law's probability for its chance, and the
    three in French's order; at T = 0 sameness always and mixed never."""
    n = 300
    runs = {GR.SAMENESS: ["fork1", "fork2"], GR.SHARED_ISA: ["fork2", "knife1"], GR.MIXED: ["cup1", "spoon1"]}
    chances = dict(CFG.group_stage_pass)
    counts = {}
    for kind, members in runs.items():
        hits = 0
        for seed in range(n):
            world, parent = henry_world(problems, seed=seed)
            proposal = BO.propose(world, parent, members)
            assert proposal.kind == kind
            proposal.stage = 1  # as if build-obj-0's proximity coin had passed
            hits += BO.run_stage(world, proposal, A_WARM)
        p = TP.pass_probability(chances[kind], A_WARM)
        assert abs(hits - n * p) <= 4 * math.sqrt(n * p * (1 - p)), (kind, hits, p)
        counts[kind] = hits
    assert counts[GR.SAMENESS] > counts[GR.SHARED_ISA] > counts[GR.MIXED]
    assert counts[GR.SAMENESS] > 0.9 * n and counts[GR.MIXED] < 0.15 * n
    cold = {}
    for kind, members in runs.items():
        hits = 0
        for seed in range(60):
            world, parent = henry_world(problems, seed=seed)
            proposal = BO.propose(world, parent, members)
            proposal.stage = 1
            hits += BO.run_stage(world, proposal, A_COLD)
        cold[kind] = hits
    assert cold[GR.SAMENESS] == 60 and cold[GR.MIXED] == 0 and 0 < cold[GR.SHARED_ISA] < 60


def test_stage_2_builds_the_group_with_its_chain_strength_and_jolts(problems):
    """"the group gets officially created by a specific codelet" [FCCA p. 384]: g2, kind
    sameness, parent g1, the parent's neighbour relations among its members and the chain
    known, a strength on (0, 100], "group" and "sameness" jolted (D-016, D-024, D-025);
    its ends are then the two forks, left and right."""
    world, parent = henry_world(problems)
    before_group = world.slipnet.activation_of("group")
    before_same = world.slipnet.activation_of("sameness")
    proposal = BO.propose(world, parent, ["fork1", "fork2"])
    group = BO.build_object(world, proposal, A_COLD)
    assert group is proposal.group and proposal.done() and proposal.stage == 3
    assert group.name == "g2" and group.kind == GR.SAMENESS and group.parent is parent
    assert group.category is None and 0 < group.strength <= 100
    assert group.neighbours == {"fork1": [("fork2", "right")], "fork2": [("fork1", "left")]}
    assert group.chain_known()
    assert world.slipnet.activation_of("group") == pytest.approx(before_group + CFG.jolt_structure)
    assert world.slipnet.activation_of("sameness") == pytest.approx(before_same + CFG.jolt_description)
    assert DS.find_ends(world, group) == ["fork1", "fork2"]
    assert DS.group_frame(group, "fork1") == ["end", "left"]
    assert world.workspace.groups == [parent, group]
    assert world.workspace.bottom_up_groups() == [group]
    assert world.workspace.innermost_group("fork1") is group


def test_a_shared_isa_group_carries_and_jolts_its_category(problems):
    """A fork-knife group is a silverware group [F95 p. 65]: `category` is the shared node,
    which is jolted when the group is built (D-024)."""
    world, parent = henry_world(problems)
    before = world.slipnet.activation_of("silverware")
    proposal = BO.propose(world, parent, ["fork2", "knife1"])
    group = BO.build_object(world, proposal, A_COLD)
    if group is None:  # the shared-ISA coin can fail even at a = 6 (about 8 percent)
        pytest.skip("the conceptual stage failed for this seed")
    assert group.kind == GR.SHARED_ISA and group.category == "silverware"
    assert world.slipnet.activation_of("silverware") == pytest.approx(before + CFG.jolt_description)


def test_the_stages_are_reproducible_from_the_seed(problems):
    """Same seed, same proposals, same results, same draw count (D-064)."""
    outcomes = []
    for _ in range(2):
        world, parent = henry_world(problems, seed=5)
        accrete(world, parent, A_WARM)
        outcomes.append(([(g.name, g.members, g.kind, round(g.strength, 6)) for g in world.workspace.groups],
                         world.rng.draws))
    assert outcomes[0] == outcomes[1]


# ---------------------------------------------------------------------------------------------
# Strength (D-025, D-069).


def test_strength_orders_uniformity_closeness_and_size():
    """The five factors of FCCA pp. 382-383 (D-025): a sameness pair is stronger than a
    shared-ISA pair, which is stronger than a mixed one, on the same geometry; a closer
    pair is stronger than a looser one; a run of five is stronger than a pair of the same
    spacing up to the size cap; a twin elsewhere adds the bonus; nothing leaves [0, 100]."""
    def strength(objects, members, kind):
        world = World(synthetic(objects, objects[0][0]), CFG, 1)
        return GR.strength_of(world, members, kind)
    tight = row_of("cup", 2, gap=8.0)
    assert strength(tight, ["cup1", "cup2"], GR.SAMENESS) > strength(tight, ["cup1", "cup2"], GR.SHARED_ISA) \
        > strength(tight, ["cup1", "cup2"], GR.MIXED)
    loose = row_of("cup", 2, gap=14.0)
    assert strength(tight, ["cup1", "cup2"], GR.SAMENESS) > strength(loose, ["cup1", "cup2"], GR.SAMENESS)
    five = row_of("cup", 5, gap=8.0)
    w = dict(CFG.group_strength_weights)
    assert strength(five, [c[0] for c in five], GR.SAMENESS) > strength(tight, ["cup1", "cup2"], GR.SAMENESS) \
        - w["compactness"]  # the run is longer (less compact) but bigger
    world = World(synthetic(tight + [("cup3", "cup", 80.0, 70.0), ("cup4", "cup", 88.0, 70.0)], "cup1"), CFG, 1)
    alone = GR.strength_of(world, ["cup1", "cup2"], GR.SAMENESS)
    world.workspace.add_group(GR.Group(members=["cup3", "cup4"], kind=GR.SAMENESS))
    assert GR.strength_of(world, ["cup1", "cup2"], GR.SAMENESS) == pytest.approx(alone + w["similar_group"])
    for objects, kind in ((tight, GR.SAMENESS), (row_of("cup", 2, gap=60.0), GR.MIXED)):
        assert 0 <= strength(objects, ["cup1", "cup2"], kind) <= 100


def test_the_gap_and_span_factors_use_the_parents_chain_and_the_footprints(problems):
    """The mean gap is over adjacent pairs from the parent's chain, in glass widths; the
    span is the footprint boxes' bounding box (D-002, D-025)."""
    world, parent = henry_world(problems)
    assert GR.adjacent_pairs(world.problem, ["fork1", "fork2", "knife1"], parent) == [("fork1", "fork2"), ("fork2", "knife1")]
    assert GR.adjacent_pairs(world.problem, ["fork1", "knife1"], parent) == [("fork1", "knife1")]  # by the axis
    assert GR.adjacent_pairs(world.problem, ["fork1"], parent) == []
    gap = GR.mean_gap_widths(world.problem, ["fork1", "fork2"], parent)
    assert 0 < gap < 0.5
    # a fork's footprint is taller (9.6 units) than two forks are wide, so a pair's span is
    # still the height; the whole row's is its width
    assert GR.span_of(world.problem, ["fork1", "fork2"]) >= GR.span_of(world.problem, ["fork1"]) > 0
    assert GR.span_of(world.problem, HENRY_ROW) > 30
    assert GR.mean_gap_widths(world.problem, ["fork1"], parent) == 0.0


def test_member_salience_enters_the_strength_only_under_french_faults(problems):
    """"Because of the high degree of salience of the touched fork, the program 'reasons'
    that if it creates a group out of the touched fork and the neighboring glass, the new
    group will be, if not a great one, at least not too bad." [F95 p. 142] Under
    `standard` the term is 0 (D-069, FIXED); under `french-faults` a fifth of the mean
    salience is added to the strength and to the stage-1 chance."""
    def salience(id_):
        return 100.0 if id_ == "cup1" else 0.0
    world, parent = henry_world(problems)
    plain = GR.strength_of(world, ["cup1", "spoon1"], GR.MIXED, parent)
    assert GR.strength_of(world, ["cup1", "spoon1"], GR.MIXED, parent, salience_of=salience) == plain
    assert BO.stage_chance(world, BO.propose(world, parent, ["cup1", "spoon1"]), salience) == pytest.approx(0.1)
    faulty, parent_f = henry_world(problems, config=FAULTS)
    assert GR.strength_of(faulty, ["cup1", "spoon1"], GR.MIXED, parent_f, salience_of=salience) \
        == pytest.approx(min(100.0, plain + 0.2 * 50.0))
    assert BO.stage_chance(faulty, BO.propose(faulty, parent_f, ["cup1", "spoon1"]), salience) == pytest.approx(0.2)
    assert BO.stage_chance(faulty, BO.propose(faulty, parent_f, ["cup1", "spoon1"])) == pytest.approx(0.1)


# ---------------------------------------------------------------------------------------------
# Incompatible groups and fights (D-025, D-040).


def test_conflicts_share_a_member_and_neither_contains_the_other(problems):
    """"it would lay claim to items already 'taken' by the pre-existing group" [FCCA
    p. 383]; a subgroup inside a group is not a rival [F95 p. 71 (img)]; two sameness
    groups of one type are not rivals (D-025, D-026)."""
    s5 = problems["surround-05"]
    forks = GR.Group(members=["fork1", "fork2"], kind=GR.SAMENESS)
    silver = GR.Group(members=["fork2", "knife1"], kind=GR.SHARED_ISA)
    row = GR.Group(members=list(HENRY_ROW))
    other = GR.Group(members=["spoon1", "spoon2"], kind=GR.SAMENESS)
    assert GR.conflicts(s5, forks, silver) and GR.conflicts(s5, silver, forks)
    assert not GR.conflicts(s5, forks, row) and not GR.conflicts(s5, row, forks)
    assert not GR.conflicts(s5, forks, other)
    cups = synthetic(row_of("cup", 3), "cup1")
    a = GR.Group(members=["cup1", "cup2"], kind=GR.SAMENESS)
    b = GR.Group(members=["cup2", "cup3"], kind=GR.SAMENESS)
    assert not GR.conflicts(cups, a, b)
    c = GR.Group(members=["cup2", "cup3"], kind=GR.MIXED)
    assert GR.conflicts(cups, a, c)


def test_a_fight_is_decided_by_strength_and_sharpened_by_temperature(problems):
    """"the probability of victory of either competitor is determined by its current
    strength relative to its rival's" [FCCA p. 383] under D-040's rule: at a = 6 the
    stronger side wins nearly always, at a = 1 in proportion to the strengths (seeded,
    four binomial standard deviations)."""
    world = World(problems["surround-05"], CFG, 3)
    n = 400
    wins = sum(GR.fight(world, 80.0, 40.0, 6.0) for _ in range(n))
    assert wins >= n - 8
    wins = sum(GR.fight(world, 60.0, 40.0, 1.0) for _ in range(n))
    assert abs(wins - 0.6 * n) <= 4 * math.sqrt(n * 0.24)
    assert GR.fight(world, 0.0, 0.0, 6.0) in (True, False)  # strengths are floored at 1


def test_a_built_group_fights_the_incompatible_one_and_the_loser_leaves(problems):
    """A fork-knife group lays claim to fork2, taken by the fork-fork group: they fight
    at once and one is removed as `lost` (D-025); over seeds both outcomes occur, and the
    Workspace never holds both."""
    outcomes = set()
    for seed in range(30):
        world, parent = henry_world(problems, seed=seed)
        forks = BO.build_object(world, BO.propose(world, parent, ["fork1", "fork2"]), A_COLD)
        proposal = BO.propose(world, parent, ["fork2", "knife1"])
        silver = BO.build_object(world, proposal, 1.0)
        groups = world.workspace.groups
        if silver is None and proposal.results[-1][0] == 2:
            assert groups == [parent, forks]
            outcomes.add("forks")
        elif silver is not None:
            assert groups == [parent, silver] and silver.name == "g3"
            outcomes.add("silver")
    assert outcomes == {"forks", "silver"}


def test_overlapping_gestalt_groups_fight_at_discovery(problems):
    """Two flood fills with different coins may give overlapping bunches; the second
    fights the first when it is found (D-025), so the Workspace keeps one unless one
    contains the other."""
    world = World(problems["surround-05"], CFG, 1)
    first = GR.Group(members=["fork1", "fork2", "knife1", "cup1"])
    world.workspace.add_group(first)
    second = GR.Group(members=["knife1", "cup1", "spoon1", "spoon2"])
    world.workspace.add_group(second)
    removed = GR.resolve_conflicts(world, second, A_COLD)
    assert len(removed) == 1 and removed[0][1] == GR.LOST
    assert len(world.workspace.groups) == 1
    assert GR.rivals_of(world, world.workspace.groups[0]) == []


# ---------------------------------------------------------------------------------------------
# Subgroups (D-026).


@pytest.mark.parametrize("n", [4, 6])
def test_a_run_of_identical_objects_is_one_group_under_the_fix_and_every_grouping_under_french_faults(n):
    """"the program would end up making many of the possible groupings of cups (e.g.,
    cup1-cup2, cup2-cup3, cup3-cup4, etc., and then cup1-cup2-cup3, cup3-cup4-cup5, etc.,
    gradually working its way up to the entire group" [F95 p. 140]; humans "tend to see
    them simply as a group of cups" [F95 p. 140]. Under `standard` the accretion ends with
    one sameness group of all n cups beside the gestalt group; under `french-faults` with
    every run of two or more, n(n-1)/2 of them (D-026, FIXED (ANSWERS 2))."""
    cups = row_of("cup", n)
    ids = [c[0] for c in cups]
    world = World(synthetic(cups, "cup1"), CFG, 1)
    parent = GR.find_group_around(world, "cup1")
    chain(world, parent, "cup1")
    accrete(world, parent)
    built = world.workspace.bottom_up_groups()
    assert len(built) == 1 and built[0].members == ids and built[0].kind == GR.SAMENESS
    assert world.workspace.groups == [parent, built[0]]
    faulty = World(synthetic(cups, "cup1"), FAULTS, 1)
    parent_f = GR.find_group_around(faulty, "cup1")
    chain(faulty, parent_f, "cup1")
    accrete(faulty, parent_f)
    runs = [g.members for g in faulty.workspace.bottom_up_groups()]
    assert len(runs) == n * (n - 1) // 2
    for length in range(2, n + 1):
        for start in range(n - length + 1):
            assert ids[start:start + length] in runs


def test_the_fix_absorbs_the_smaller_sameness_group_and_names_are_not_reused():
    """A cup2-cup3 proposal beside an existing cup1-cup2 group is proposed as cup1-cup2-cup3
    and absorbs the pair when built (D-026); under `french-faults` the pair is built as
    its own group beside the other."""
    cups = row_of("cup", 3)
    world = World(synthetic(cups, "cup1"), CFG, 1)
    parent = GR.find_group_around(world, "cup1")
    chain(world, parent, "cup1")
    pair = BO.build_object(world, BO.propose(world, parent, ["cup1", "cup2"]), A_COLD)
    assert BO.propose(world, parent, ["cup1", "cup2"]) is None
    proposal = BO.propose(world, parent, ["cup2", "cup3"])
    assert proposal.members == ["cup1", "cup2", "cup3"] and proposal.new_links == [("cup2", "cup3")]
    whole = BO.build_object(world, proposal, A_COLD)
    assert world.workspace.groups == [parent, whole] and whole.name == "g3"
    assert pair not in world.workspace.groups
    assert BO.propose(world, parent, ["cup2", "cup3"]) is None  # covered
    faulty = World(synthetic(cups, "cup1"), FAULTS, 1)
    parent_f = GR.find_group_around(faulty, "cup1")
    chain(faulty, parent_f, "cup1")
    BO.build_object(faulty, BO.propose(faulty, parent_f, ["cup1", "cup2"]), A_COLD)
    proposal = BO.propose(faulty, parent_f, ["cup2", "cup3"])
    assert proposal.members == ["cup2", "cup3"]
    BO.build_object(faulty, proposal, A_COLD)
    assert [g.members for g in faulty.workspace.bottom_up_groups()] == [["cup1", "cup2"], ["cup2", "cup3"]]


def test_subgroups_of_other_kinds_are_kept(problems):
    """"Subgroups of different kinds (a fork-fork inside a silverware group) and gestalt
    groups containing subgroups are kept, as Fig. 3.9 shows them" (D-026): a silverware
    group over fork1, fork2, knife1 keeps the fork-fork group inside it."""
    for seed in range(20):
        world, parent = henry_world(problems, seed=seed)
        forks = BO.build_object(world, BO.propose(world, parent, ["fork1", "fork2"]), A_COLD)
        silver = BO.build_object(world, BO.propose(world, parent, ["fork1", "fork2", "knife1"], base=forks), A_COLD)
        if silver is not None:
            assert world.workspace.groups == [parent, forks, silver]
            assert silver.kind == GR.SHARED_ISA and silver.category == "silverware"
            return
    raise AssertionError("the silverware group never formed at a = 6")


def test_absorb_subgroups_directly_and_the_knob():
    """`absorb_subgroups` removes every smaller sameness group of the type inside the new
    one, nothing under `keep_all_subgroups` (D-026)."""
    cups = row_of("cup", 4)
    for config, expected in ((CFG, 2), (FAULTS, 0)):
        world = World(synthetic(cups, "cup1"), config, 1)
        a = GR.Group(members=["cup1", "cup2"], kind=GR.SAMENESS)
        b = GR.Group(members=["cup3", "cup4"], kind=GR.SAMENESS)
        c = GR.Group(members=["cup2", "cup3"], kind=GR.MIXED)
        for g in (a, b, c):
            world.workspace.add_group(g)
        whole = GR.Group(members=["cup1", "cup2", "cup3", "cup4"], kind=GR.SAMENESS)
        world.workspace.add_group(whole)
        absorbed = GR.absorb_subgroups(world, whole)
        assert len(absorbed) == expected
        assert c in world.workspace.groups
        if expected:
            assert absorbed == [a, b] and world.workspace.groups == [c, whole]

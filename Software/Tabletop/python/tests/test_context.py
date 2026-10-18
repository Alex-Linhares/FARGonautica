"""The context push and pop (D-021, D-070) [F95 pp. 85-89]: which codelets carry a
context, what the push wipes clean under both readings [F95 pp. 87-88], the pop's
reconstruction with French's fixed amounts [F95 pp. 88-89] and the one spreading step of
the fix [F95 p. 89], the run loop entering and leaving, and the off switch.
"""

import dataclasses

import pytest

from tabletop import coderack as CR
from tabletop import context as CX
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import geometry as G
from tabletop import problems as P
from tabletop.codelets import dispatch
from tabletop.codelets.common import context_args
from tabletop.config import PROFILES
from tabletop.groups import Group
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

STANDARD, FAULTS = V2_PROFILES["standard"], V2_PROFILES["french-faults"]
A_COLD = 6.0
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
ELIZA_ROW = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]
MIRROR, DIAGONAL = "mirror symmetry", "diagonal symmetry"


@pytest.fixture(scope="module")
def problems():
    return {p.name: p for p in P.load_all()}


def codelet(type_, *args):
    return CR.make_codelet(type_, STANDARD, 0, args=args)


def run(world, type_, *args, a=A_COLD):
    return dispatch.run_codelet(world, codelet(type_, *args), a)


def row(world, members):
    g = Group(members=list(members))
    world.workspace.add_group(g)
    frontier = [members[0]]
    while frontier:
        for n, _ in DS.find_neighbours(world, g, frontier.pop(0)):
            frontier.append(n)
    DS.find_ends(world, g)
    DS.find_middles(world, g)
    DS.notice_orientation(world, g)
    return g


def chapter_4_world(config, seed=1):
    """The central configuration with both rows found, the row-to-row correspondence and
    fork1-knife2 described and in the Worldview [F95 pp. 85-86; F95 p. 108]."""
    world = World(P.load_problem("surround-05"), config, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng, region=G.exact_region(o.x, o.y))
    h, e = row(world, HENRY_ROW), row(world, ELIZA_ROW)
    rows = C.create(world, h.name, e.name)
    C.describe(world, rows)
    rows.in_worldview = rows.tested = rows.passed = True
    ends = C.create(world, "fork1", "knife2")
    C.describe(world, ends)
    ends.in_worldview = ends.tested = ends.passed = True
    return world, rows, ends


def activations(world, names):
    return {n: world.slipnet.activation_of(n) for n in names}


def with_slippages(world, henry, eliza, slippages, in_worldview=True, parent=None):
    """A correspondence with its slippages set by hand, for the exact pattern of pp. 88-89."""
    cp = C.create(world, henry, eliza, parent=parent)
    cp.slippages = list(slippages)
    cp.in_worldview = in_worldview
    return cp


# ---- which codelets carry a context ---------------------------------------------------

def test_the_context_a_codelet_carries_is_the_group_correspondence_its_argument_names():
    """examine-cp(cp1) "enters a subset of the full Tableworld" [F95 p. 85]; the codelets
    it spawns inside carry the same name (D-021)."""
    assert CX.context_of(codelet("examine-cp", "cp1")) == "cp1"
    assert CX.context_of(codelet("create-cp", "fork1", "fork3", "cp1")) == "cp1"
    assert CX.context_of(codelet("build-cp-descrip", "cp2", "cp1")) == "cp1"
    assert CX.context_of(codelet("cp-compete", "cp2", "cp1")) == "cp1"
    for top_level in (codelet("create-cp", "fork1", "fork3"), codelet("build-cp-descrip", "cp2"),
                      codelet("cp-compete", "cp2"), codelet("find-group", "cup1"),
                      codelet("pick-strong-cp"), codelet("examine-cp")):
        assert CX.context_of(top_level) is None


def test_the_codelets_spawned_inside_a_group_correspondence_carry_it(problems):
    """The chain examine-cp -> create-cp -> cp-compete -> build-cp-descrip inside cp1
    [F95 p. 85; F95 p. 108] names cp1 at every step; outside, nothing is named (D-021)."""
    world, rows, ends = chapter_4_world(STANDARD)
    assert context_args(ends) == (ends.name,)
    ex = run(world, "examine-cp", rows.name)
    x, y, parent = ex.posted[0].args
    assert parent == rows.name
    built = run(world, "create-cp", x, y, parent, a=A_COLD)   # tested at once; cold, yet it may fail
    if built.action == "passed":
        inner = world.workspace.correspondences[-1]
        assert inner.parent is rows and context_args(inner) == (inner.name, rows.name)
        assert built.posted[0].type == "cp-compete" and built.posted[0].args == (inner.name, rows.name)
    else:
        assert built.detail.endswith("removed")
        inner = C.create(world, x, y, parent=rows)
        inner.passed = inner.tested = True
    picked = run(world, "pick-strong-cp")          # the one correspondence outside the Worldview
    assert picked.posted[0].type == "cp-compete" and picked.posted[0].args == (inner.name, rows.name)
    held = run(world, "cp-compete", inner.name, rows.name)
    if held.action == "entered":
        assert held.posted[0].type == "build-cp-descrip"
        assert held.posted[0].args == (inner.name, rows.name)
    checked = run(world, "check-worldview")
    if not checked.fizzled:
        assert all(c.args == (inner.name, rows.name) for c in checked.posted)


# ---- the push [F95 pp. 87-88] ----------------------------------------------------------

def test_the_push_wipes_the_slippage_concepts_and_the_two_symmetries_clean(problems):
    """"the activation level is set to its initial value (in this case, 1). So, in Tabletop,
    the activation slate really is wiped clean." [F95 p. 87], for "concepts that
    participate in the supporting slippages of the correspondence under investigation"
    [F95 p. 88] (D-070); everything else keeps its level."""
    world, rows, ends = chapter_4_world(STANDARD)
    world.slipnet.jolt(MIRROR, 100)
    world.slipnet.jolt(DIAGONAL, 60)
    world.slipnet.jolt("horizontal", 40)
    named = CX.reset_set(world, rows)
    # The row-to-row correspondence's slippages: many-many (sameness), horizontal-horizontal
    # (sameness); the kind slippage group-group names no node.
    assert named == [MIRROR, DIAGONAL, "many", "sameness", "horizontal"]
    untouched = ["cup", "left", "right", "end", "opposite", "group", "fork"]
    before = activations(world, untouched)
    assert before["cup"] > 1 and before["end"] > 1 and before["opposite"] > 100
    draws = world.rng.draws
    pushed = CX.push(world, rows)
    assert pushed.correspondence == rows.name
    assert [n for n, _ in pushed.reset] == named
    assert dict(pushed.reset)[MIRROR] == pytest.approx(101) and dict(pushed.reset)[DIAGONAL] == pytest.approx(61)
    assert all(world.slipnet.activation_of(n) == 1 for n in named)
    assert activations(world, untouched) == before
    assert world.context is not None and world.context.correspondence == rows.name
    assert world.context.reset == named and world.rng.draws == draws


def test_french_s_push_reset_only_the_two_symmetry_nodes(problems):
    """"In reality, however, only the 'mirror-symmetry' and 'diagonal-symmetry' nodes are
    reinitialized" [F95 p. 88]: the `french-faults` reading (D-070)."""
    world, rows, ends = chapter_4_world(FAULTS)
    world.slipnet.jolt(MIRROR, 100)
    world.slipnet.jolt("horizontal", 40)
    horizontal = world.slipnet.activation_of("horizontal")
    assert horizontal > 41
    assert CX.reset_set(world, rows) == [MIRROR, DIAGONAL]
    pushed = CX.push(world, rows)
    assert [n for n, _ in pushed.reset] == [MIRROR, DIAGONAL]
    assert world.slipnet.activation_of(MIRROR) == 1 and world.slipnet.activation_of(DIAGONAL) == 1
    assert world.slipnet.activation_of("horizontal") == horizontal


def test_the_two_profiles_differ_in_the_reset_set_only_by_the_knob():
    assert STANDARD.push_reset_set == "slippage-concepts" and FAULTS.push_reset_set == "symmetries"
    assert STANDARD.pop_restores_spread and not FAULTS.pop_restores_spread
    assert STANDARD.context_mechanism and FAULTS.context_mechanism


def test_a_correspondence_not_yet_described_resets_the_symmetries_only(problems):
    world, rows, ends = chapter_4_world(STANDARD)
    rows.slippages = []
    assert CX.reset_set(world, rows) == [MIRROR, DIAGONAL]


def test_inside_the_subworld_a_rise_counts_from_one(problems):
    """The push resets the posting levels with the activations, so the diagonal
    sub-correspondences' activation of "diagonal symmetry" [F95 p. 87] is a rise the
    Slipnet posts on (D-020)."""
    world, rows, ends = chapter_4_world(STANDARD)
    world.slipnet.jolt(DIAGONAL, 100)
    world.slipnet.top_down_postings(world.rng)       # flush the rises so far
    CX.push(world, rows)
    world.slipnet.jolt(DIAGONAL, 80)                 # two structure jolts inside
    posted = world.slipnet.top_down_postings(world.rng)
    assert (DIAGONAL, "pick-strong-cp") in posted
    assert posted.count((DIAGONAL, "pick-strong-cp")) == 2


# ---- the pop [F95 pp. 88-89] -----------------------------------------------------------

def right_left_and_right_right(config):
    """A world whose top-level Worldview holds one right-to-left slippage and one
    right-to-right identity, the two cases of pp. 88-89, and nothing else."""
    world, rows, ends = chapter_4_world(config)
    rows.in_worldview = ends.in_worldview = False
    flip = with_slippages(world, "fork2", "fork4",
                          [C.Slippage(C.POSITION, "right", "left", C.FLIP_LABELS)])
    same = with_slippages(world, "knife1", "fork3",
                          [C.Slippage(C.POSITION, "right", "right", C.IDENTITY_LABELS)])
    return world, rows, flip, same


def test_french_s_pop_puts_a_fixed_amount_into_the_concepts_of_each_slippage():
    """"for each 'right'-to-'left' slippage, it puts a fixed amount of activation into the
    concepts 'right,' 'left,' and 'diagonal symmetry.'" [F95 p. 88] and "for each
    'right'-to-'right' slippage it finds, it puts a fixed amount of activation into 'right'
    (twice) and into 'mirror symmetry.'" [F95 p. 89]; nothing else is touched, and the
    labels of D-030 ("opposite", "sameness") receive the amount with their symmetry."""
    world, rows, flip, same = right_left_and_right_right(FAULTS)
    sn = world.slipnet
    sn.jolt(MIRROR, 100)
    sn.jolt(DIAGONAL, 60)
    before = activations(world, [n.name for n in sn.nodes])
    CX.push(world, rows)
    sn.jolt(DIAGONAL, 90)                            # what the subworld found
    amount = FAULTS.pop_fixed_amount
    draws = world.rng.draws
    popped = CX.pop(world)
    assert popped.correspondence == rows.name and popped.reinitialized == [MIRROR, DIAGONAL]
    assert popped.spread == []
    assert popped.fixed == [("right", amount), ("left", amount), ("opposite", amount),
                            (DIAGONAL, amount), ("right", amount), ("right", amount),
                            (MIRROR, amount), ("sameness", amount)]
    after = activations(world, [n.name for n in sn.nodes])
    assert after["right"] == pytest.approx(before["right"] + 3 * amount)
    assert after["left"] == pytest.approx(before["left"] + amount)
    assert after[DIAGONAL] == pytest.approx(1 + amount)
    assert after[MIRROR] == pytest.approx(1 + amount)
    assert after["opposite"] == pytest.approx(min(150, before["opposite"] + amount))
    assert after["sameness"] == pytest.approx(before["sameness"] + amount)
    for name in before:
        if name not in ("right", "left", "opposite", "sameness", MIRROR, DIAGONAL):
            assert after[name] == before[name], name
    assert world.context is None and world.rng.draws == draws
    assert dict(popped.restored)[MIRROR] == pytest.approx(1 + amount)


def test_the_fixed_pop_adds_what_one_spreading_step_would_bring():
    """The fix (D-021, FIXED (ANSWERS 2)): "all of the (possibly considerable) activation
    that might have spread to these concepts from other concepts is lost" [F95 p. 89] under
    French's pop; the default pop adds one spreading step from the neighbours at their
    current levels into the reinitialized nodes."""
    world, rows, flip, same = right_left_and_right_right(STANDARD)
    sn = world.slipnet
    sn.jolt("symmetry", 100)                        # the symmetries' category, warm
    sn.jolt("direction", 100)                       # left's and right's
    rows.slippages = flip.slippages + same.slippages   # the correspondence entered names them
    named = CX.reset_set(world, rows)
    assert named == [MIRROR, DIAGONAL, "right", "left", "opposite", "sameness"]
    CX.push(world, rows)
    expected = dict(sn.spread_step_into(named))
    assert expected[MIRROR] > 1 and expected[DIAGONAL] > 1 and expected["right"] > 1
    # In v1 the two label nodes had no links and received nothing; from D-080 on "opposite"
    # is linked to "key" and "sameness" to "echo" and "counterpart", so a little comes in.
    assert 0 <= expected["opposite"] < 1 and 0 <= expected["sameness"] < 1
    amount = STANDARD.pop_fixed_amount
    popped = CX.pop(world)
    assert dict(popped.spread) == pytest.approx(expected)
    assert sn.activation_of(MIRROR) == pytest.approx(1 + expected[MIRROR] + amount)
    assert sn.activation_of(DIAGONAL) == pytest.approx(1 + expected[DIAGONAL] + amount)
    assert sn.activation_of("right") == pytest.approx(1 + expected["right"] + 3 * amount)
    assert sn.activation_of("left") == pytest.approx(1 + expected["left"] + amount)
    assert sn.activation_of("direction") == pytest.approx(101)         # a sender keeps its level


def test_the_fixed_pop_recovers_more_than_french_s_on_the_same_state():
    """With the same Worldview and the same warm neighbours, every reinitialized node ends
    at least as high under the fix, and the symmetries strictly higher [F95 p. 89]."""
    results = {}
    for name, config in (("standard", STANDARD), ("french-faults", FAULTS)):
        world, rows, flip, same = right_left_and_right_right(
            dataclasses.replace(config, push_reset_set="symmetries"))
        world.slipnet.jolt("symmetry", 100)
        CX.push(world, rows)
        CX.pop(world)
        results[name] = activations(world, [MIRROR, DIAGONAL, "right", "left"])
    assert results["standard"][MIRROR] > results["french-faults"][MIRROR]
    assert results["standard"][DIAGONAL] > results["french-faults"][DIAGONAL]
    assert results["standard"]["right"] == results["french-faults"]["right"]
    assert results["standard"]["left"] == results["french-faults"]["left"]


def test_the_pop_reads_the_worldview_s_top_level_only():
    """"finding the correspondences in that context" [F95 p. 88]: a correspondence outside
    the Worldview, and one built inside a group correspondence [F95 p. 108], contribute no
    fixed amount; the group correspondence itself does."""
    world, rows, flip, same = right_left_and_right_right(FAULTS)
    flip.in_worldview = False
    rows.in_worldview = True
    inner = with_slippages(world, "spoon1", "spoon3",
                           [C.Slippage(C.POSITION, "above", "below", C.FLIP_LABELS)], parent=rows)
    assert inner.level == rows.name
    CX.push(world, rows)
    popped = CX.pop(world)
    fed = [n for n, _ in popped.fixed]
    assert "left" not in fed and "above" not in fed and "below" not in fed
    assert fed.count("right") == 2 and MIRROR in fed
    assert "many" in fed and "horizontal" in fed             # the rows' own slippages


def test_the_pop_itself_posts_no_codelet():
    """The reconstruction is not a rise the Workspace produced: the posting levels follow
    the restored activations (D-020, D-021)."""
    world, rows, flip, same = right_left_and_right_right(STANDARD)
    world.slipnet.jolt("symmetry", 100)
    world.slipnet.top_down_postings(world.rng)
    CX.push(world, rows)
    world.slipnet.top_down_postings(world.rng)
    CX.pop(world)
    assert world.slipnet.activation_of("opposite") > 100     # restored high
    assert world.slipnet.top_down_postings(world.rng) == []


def test_the_pop_at_the_top_level_is_nothing():
    world, rows, flip, same = right_left_and_right_right(STANDARD)
    assert world.context is None and CX.pop(world) is None


def test_the_sum_is_clipped_at_the_ceiling():
    world, rows, flip, same = right_left_and_right_right(FAULTS)
    world.slipnet.set_activation("right", 140)
    CX.push(world, rows)
    CX.pop(world)
    assert world.slipnet.activation_of("right") == 150


# ---- the run loop entering and leaving (D-021) ------------------------------------------

def load(world, *codelets):
    world.coderack = CR.Coderack(world.config)
    world.coderack.post_all(list(codelets))


def test_the_run_loop_pushes_before_an_examine_cp_and_pops_before_the_next_top_level_codelet(problems):
    """"when examine-cp(cp1) runs, it enters a subset of the full Tableworld" [F95 p. 85];
    "pops" to a previous context when the program leaves it [F95 p. 88]."""
    world, rows, ends = chapter_4_world(STANDARD)
    load(world, codelet("examine-cp", rows.name))
    step = world.step()
    assert step.pushed is not None and step.pushed.correspondence == rows.name
    assert step.popped is None and world.context.correspondence == rows.name
    assert step.codelet.type == "examine-cp" and not step.outcome.fizzled
    create = step.outcome.posted[0]
    load(world, create)                                  # inside the same context
    step = world.step()
    assert step.pushed is None and step.popped is None and world.context.correspondence == rows.name
    load(world, codelet("find-group", "cup1"))
    step = world.step()
    assert step.popped is not None and step.popped.correspondence == rows.name
    assert step.pushed is None and world.context is None


def test_contexts_do_not_nest_a_second_group_correspondence_pops_the_first(problems):
    world, rows, ends = chapter_4_world(STANDARD)
    left = Group(members=["fork1", "fork2"])
    right = Group(members=["fork3", "fork4"])
    world.workspace.add_group(left)
    world.workspace.add_group(right)
    pairs = C.create(world, left.name, right.name)
    C.describe(world, pairs)
    load(world, codelet("examine-cp", rows.name))
    world.step()
    load(world, codelet("examine-cp", pairs.name))
    step = world.step()
    assert step.popped.correspondence == rows.name and step.pushed.correspondence == pairs.name
    assert world.context.correspondence == pairs.name


def test_a_codelet_whose_context_is_gone_runs_at_the_top_level(problems):
    """A codelet holding a removed correspondence does nothing [F95 p. 73] and enters
    nothing."""
    world, rows, ends = chapter_4_world(STANDARD)
    name = rows.name
    world.workspace.remove_correspondence(rows)
    assert CX.enter(world, codelet("examine-cp", name)) == (None, None)
    assert world.context is None
    assert run(world, "examine-cp", name).fizzled


def test_the_lesion_off_means_no_change(problems):
    """`context_mechanism` = False (D-021; a lesion [FCCA p. 365]): no push, no pop, no
    activation moved, and the context never set."""
    off = dataclasses.replace(STANDARD, context_mechanism=False)
    world, rows, ends = chapter_4_world(off)
    world.slipnet.jolt(MIRROR, 100)
    before = activations(world, [n.name for n in world.slipnet.nodes])
    assert CX.enter(world, codelet("examine-cp", rows.name)) == (None, None)
    assert world.context is None
    assert activations(world, [n.name for n in world.slipnet.nodes]) == before
    load(world, codelet("examine-cp", rows.name))
    step = world.step()
    assert step.pushed is None and step.popped is None and world.context is None
    assert not step.outcome.fizzled


def pushes_and_pops(problem_list, off_list):
    """Over the problems with seed 1, every push is followed by a pop before the next, the
    push and the pop spend no draw, and a run with the mechanism off never pushes; returns
    (pushes, pops)."""
    pushes = pops = 0
    for problem in problem_list:
        world = World(problem, STANDARD, 1)
        inside = False
        while not world.finished:
            draws = world.rng.draws
            step = world.step()
            if step.popped is not None:
                assert inside
                inside, pops = False, pops + 1
            if step.pushed is not None:
                assert not inside
                inside, pushes = True, pushes + 1
            assert (world.context is not None) == inside
            if step.pushed is not None or step.popped is not None:
                assert step.codelet.type in CX.CONTEXT_ARGUMENT or step.popped is not None
            assert world.rng.draws >= draws
    assert pushes >= pops >= 0
    off = dataclasses.replace(STANDARD, context_mechanism=False)
    for problem in off_list:
        world = World(problem, off, 1)
        while not world.finished:
            step = world.step()
            assert step.pushed is None and step.popped is None
    return pushes, pops


def test_whole_runs_push_and_pop_in_pairs_and_draw_nothing_for_it(problems):
    """Six problems here (the 53 are `tests_slow/test_context_slow.py`, ANSWERS.md D2)."""
    names = ("surround-05", "blockage-06", "buridan-01", "figure-ground-01", "ch2-fig08",
             "ch2-fig11")
    pushes_and_pops([problems[n] for n in names], [problems[n] for n in names[:2]])

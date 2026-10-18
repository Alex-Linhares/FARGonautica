"""The codelet functions (D-052): one function per type of the legend [F95 p. 96 (img)],
each with the follow-ups it posts, "this codelet then posts a new set of codelets to the
Coderack" [F95 p. 63]; find-group's list [F95 p. 76], find-neighbor's chain [F95 p. 80],
the three build-object stages [FCCA p. 384], examine-cp inside a group correspondence [F95
p. 85; F95 p. 108], and a codelet whose structure is gone doing nothing [F95 p. 73].
"""

import dataclasses

import pytest

from tabletop import build_object as BO
from tabletop import coderack as CR
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import geometry as G
from tabletop import problems as P
from tabletop import worldview as WV
from tabletop.codelets import catalogue, dispatch
from tabletop.codelets.common import FIZZLED
from tabletop.config import PROFILES
from tabletop.groups import Group
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
A_COLD = 6.0
HENRY_ROW = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
ELIZA_ROW = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]


@pytest.fixture(scope="module")
def problems():
    return {p.name: p for p in P.load_all()}


def codelet(type_, *args, generation=0):
    return CR.make_codelet(type_, CFG, 0, args=args, generation=generation)


def run(world, type_, *args, a=A_COLD):
    return dispatch.run_codelet(world, codelet(type_, *args), a)


def notice_all(world):
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng, region=G.exact_region(o.x, o.y))


def row(world, members, kind=None):
    """A gestalt (or bottom-up) group with its chain, ends and middles found."""
    g = Group(members=list(members)) if kind is None else Group(members=list(members), kind=kind)
    world.workspace.add_group(g)
    frontier = [members[0]]
    while frontier:
        for n, _ in DS.find_neighbours(world, g, frontier.pop(0)):
            frontier.append(n)
    DS.find_ends(world, g)
    DS.find_middles(world, g)
    DS.notice_orientation(world, g)
    return g


def types(posted):
    return [c.type for c in posted]


def test_every_type_of_the_legend_has_a_function():
    assert sorted(dispatch.FUNCTIONS) == sorted(catalogue.names()) and len(dispatch.FUNCTIONS) == 29


def test_find_group_posts_the_follow_ups_of_p_76(problems):
    """"the program puts the following codelets on the Coderack" [F95 p. 76]: a find-group,
    a sameness-detector, find-neighbor codelets, a find-end-object, a find-middle-object
    and pick-salient-object codelets; two find-neighbors [F95 p. 81] (D-052)."""
    world = World(problems["surround-05"], CFG, 1)
    out = run(world, "find-group", "cup1")
    assert out.action == "found"
    g = world.workspace.groups[0]
    # The row's gaps are coins at about 0.93 each (D-022), so the component is a run of
    # Henry's row around the cup, the whole row about three times in four.
    assert "cup1" in g.members and len(g.members) >= 2 and g.orientation == "horizontal"
    assert g.members == [m for m in HENRY_ROW if m in g.members]
    assert types(out.posted) == ["find-group", "simple-sameness-detect", "find-neighbor",
                                 "find-neighbor", "find-end-objs", "find-middle-objs",
                                 "pick-salient-obj", "pick-salient-obj", "find-roles"]
    assert out.posted[2].args == (g.members[0],) and out.posted[3].args == (g.members[-1],)
    assert all(c.generation == 1 and c.by == "find-group" for c in out.posted)
    assert "isolated" in run(world, "find-group", "cup2").detail   # cup2 is alone [F95 p. 97]


def test_find_group_without_an_argument_looks_at_a_region(problems):
    world = World(problems["surround-05"], CFG, 1)
    outcomes = [run(world, "find-group", a=0.1) for _ in range(30)]
    assert any(o.action == "found" for o in outcomes)
    assert any(o.fizzled and o.detail.startswith("nothing in") for o in outcomes)
    assert any("isolated" in o.detail for o in outcomes)      # cup2 is alone [F95 p. 97]


def test_find_neighbor_chains_and_proposes_the_pair(problems):
    """"It would find fork2 and spawn the codelet find-neighbor(fork2)" [F95 p. 80]; the
    adjacent pair is proposed as a bottom-up group with a build-obj-0 (D-024, D-052)."""
    world = World(problems["surround-05"], CFG, 1)
    run(world, "find-group", "cup1")
    out = run(world, "find-neighbor", "fork1")
    assert out.action == "found" and "fork2 right" in out.detail
    assert types(out.posted) == ["find-neighbor", "build-obj-0", "find-roles"]   # D-081, D-090
    assert out.posted[0].args == ("fork2",)
    proposal = world.workspace.proposal_named(out.posted[1].args[0])
    assert proposal.members == ["fork1", "fork2"] and proposal.kind == "sameness"
    assert world.workspace.is_noticed("fork2")
    assert run(world, "find-neighbor", "fork1").fizzled
    assert run(world, "find-neighbor", "cup2").fizzled        # in no group [F95 p. 76]


def test_the_three_stages_build_a_group_and_post_the_searches(problems):
    """build-obj-0, -1, -2 [FCCA p. 384]; the built group posts a build-obj-desc, twelve
    search-for-sam-gp and two search-for-sim-gp (D-052) [F95 p. 101]."""
    world = World(problems["surround-05"], CFG, 1)
    run(world, "find-group", "cup1")
    out = run(world, "find-neighbor", "fork1")
    name = out.posted[1].args[0]
    o0 = run(world, "build-obj-0", name)
    assert o0.action == "passed" and types(o0.posted) == ["build-obj-1"]
    assert run(world, "build-obj-2", name).fizzled            # the wrong stage
    o1 = run(world, "build-obj-1", name)
    assert o1.action == "passed" and types(o1.posted) == ["build-obj-2"]
    o2 = run(world, "build-obj-2", name)
    assert o2.action == "built"
    assert types(o2.posted) == ["build-obj-desc"] + ["search-for-sam-gp"] * 12 + ["search-for-sim-gp"] * 2 + ["find-roles"]
    g = world.workspace.bottom_up_groups()[0]
    assert g.members == ["fork1", "fork2"] and g.kind == "sameness"
    assert run(world, "build-obj-2", name).fizzled            # finished
    desc = run(world, "build-obj-desc", g.name)
    assert desc.action == "described" and world.workspace.has(g.name, "two")
    assert world.workspace.has(g.name, "sameness") and world.workspace.has(g.name, "horizontal")


def test_a_mixed_pair_fails_its_conceptual_stage_when_cold(problems):
    """Groups of unrelated objects form "only very rarely" [F95 p. 65]."""
    world = World(problems["surround-05"], CFG, 1)
    run(world, "find-group", "cup1")
    out = run(world, "find-neighbor", "cup1")
    names = [c.args[0] for c in out.posted if c.type == "build-obj-0"]
    results = []
    for name in names:
        assert run(world, "build-obj-0", name).action == "passed"
        results.append(run(world, "build-obj-1", name).action)
    assert "failed" in results


def test_the_searches_find_twins_among_the_groups(problems):
    """search-for-sam-gp looks for a group of the same composition elsewhere, the
    search-for-same-group codelets of chapter 4 [F95 p. 101]; search-for-sim-gp for one of
    the same kind and size (D-052)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    h = row(world, ["spoon1", "spoon2"], kind="sameness")
    assert run(world, "search-for-sam-gp", h.name).fizzled
    e = row(world, ["spoon3", "spoon4"], kind="sameness")
    out = run(world, "search-for-sam-gp", h.name)
    assert out.action == "found" and types(out.posted) == ["create-cp"]
    assert out.posted[0].args == (h.name, e.name)
    f = row(world, ["fork3", "fork4"], kind="sameness")
    sim = run(world, "search-for-sim-gp", h.name)
    assert sim.action == "found" and sim.posted[0].args[1] in (e.name, f.name)
    assert run(world, "search-for-sam-gp", "g99").fizzled


def test_create_cp_tests_competes_and_describes(problems):
    """create-cp creates and tests at once [F95 pp. 71-72] and posts cp-compete; cp-compete
    enters without a fight [F95 p. 101] and posts a build-cp-descrip, a clean-up and a
    check; build-cp-descrip writes the descriptors (D-035, D-039, D-040, D-052; item 24)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    out = run(world, "create-cp", "cup1", "small-glass1")
    assert out.action == "passed" and types(out.posted) == ["cp-compete"]
    cp = world.workspace.correspondences[0]
    assert cp.tested and cp.passed
    assert run(world, "create-cp", "cup1", "small-glass1").detail.endswith("exists")
    assert run(world, "create-cp", "cup1", "cup1").fizzled
    held = run(world, "cp-compete", cp.name)
    assert held.action == "entered"
    assert types(held.posted) == ["build-cp-descrip", "clean-up-corresp", "check-worldview",
                                  "read-worldview-roles"]          # D-083: the axis re-read
    assert cp.in_worldview and world.workspace.changes
    described = run(world, "build-cp-descrip", cp.name)
    assert described.action == "described" and not described.posted
    assert world.workspace.has(cp.name, "cup-small glass")
    assert run(world, "cp-compete", cp.name).fizzled


def test_a_failed_test_removes_the_correspondence_and_a_pick_may_propose_it_again(problems):
    """A correspondence that fails its initial test is removed (D-039, `failed_test_removes`),
    so a later pick can propose the pair again; under the other reading it stays tentative."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    out = run(world, "create-cp", "cup1", "fork3", a=A_COLD)      # a weak pair fails when cold
    assert out.action == "failed" and out.detail.endswith("removed") and not out.posted
    assert world.workspace.correspondences == []
    assert run(world, "create-cp", "cup1", "fork3", a=A_COLD).action == "failed"
    keep = World(problems["surround-05"], dataclasses.replace(CFG, failed_test_removes=False), 1)
    notice_all(keep)
    out = run(keep, "create-cp", "cup1", "fork3", a=A_COLD)
    assert out.action == "failed" and len(keep.workspace.correspondences) == 1
    assert not keep.workspace.correspondences[0].passed


def test_a_group_correspondence_entering_posts_four_examine_cps_which_propose_inner_pairs(problems):
    """"a number of examine-cp(cp1) codelets were posted to the Coderack" [F95 p. 85]; each
    proposes one correspondence inside [F95 p. 108] (D-052)."""
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    h, e = row(world, HENRY_ROW), row(world, ELIZA_ROW)
    cp = C.create(world, h.name, e.name)
    cp.passed = cp.tested = True
    held = run(world, "cp-compete", cp.name)
    assert held.action == "entered"
    assert types(held.posted) == (["build-cp-descrip", "clean-up-corresp", "check-worldview",
                                   "read-worldview-roles"] + ["examine-cp"] * 4)
    ex = run(world, "examine-cp", cp.name)
    assert ex.action == "proposed" and types(ex.posted) == ["create-cp"]
    x, y, parent = ex.posted[0].args
    assert x in HENRY_ROW and y in ELIZA_ROW and parent == cp.name
    inner = run(world, "create-cp", x, y, parent)        # created and tested at once (D-039)
    assert inner.action in ("passed", "failed")
    if inner.action == "passed":
        assert world.workspace.correspondences[-1].parent is cp


def test_pick_salient_codelets_post_create_cps(problems):
    world = World(problems["surround-05"], CFG, 1)
    assert run(world, "pick-salient-obj").fizzled              # only cup1 in view
    notice_all(world)
    out = run(world, "pick-salient-obj")
    assert out.action == "picked" and types(out.posted) == ["create-cp"]
    assert C.creatable(world, *out.posted[0].args)[0]
    second = run(world, "pick-2nd-salient-obj")
    assert second.action == "picked" and types(second.posted) == ["find-group", "create-cp"]
    assert run(world, "pick-salient-subgp").fizzled
    row(world, ["spoon1", "spoon2"], kind="sameness")
    sub = run(world, "pick-salient-subgp")
    assert sub.action == "picked" and types(sub.posted) == ["search-for-sam-gp"]


def test_pick_strong_cp_tests_a_tentative_correspondence(problems):
    """pick-salient-correspondence codelets pick one "for further processing" [F95 p. 65]."""
    world = World(problems["surround-05"], CFG, 1)
    assert run(world, "pick-strong-cp").fizzled
    notice_all(world)
    cp = C.create(world, "cup1", "cup2")
    out = run(world, "pick-strong-cp")
    assert out.action in ("passed", "failed") and cp.tested
    if out.action == "passed":
        assert types(out.posted) == ["cp-compete"]


def test_check_worldview_and_clean_up(problems):
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    assert run(world, "check-worldview").fizzled and run(world, "clean-up-corresp").fizzled
    cp = C.create(world, "cup1", "cup2")
    cp.passed = cp.tested = True
    out = run(world, "check-worldview")
    assert out.action == "found" and types(out.posted) == ["cp-compete"]
    weak = C.create(world, "fork1", "small-glass1")
    weak.bonus = -200
    gone = run(world, "clean-up-corresp")
    assert gone.action == "removed" and weak.name in gone.detail
    assert not WV.alive(world, weak)


def test_sameness_detect_and_the_describers(problems):
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    sam = run(world, "simple-sameness-detect", "cup1")
    assert sam.action == "found" and sam.posted[0].args == ("cup1", "cup2")
    g = Group(members=list(HENRY_ROW))
    world.workspace.add_group(g)
    ends = run(world, "find-end-objs", g.name)
    assert ends.action == "found" and g.ends == ["fork1", "spoon2"]
    assert run(world, "find-end-objs", g.name).fizzled
    mids = run(world, "find-middle-objs")                     # no argument: the group without middles
    assert mids.action == "found" and len(g.middles) == 4
    assert run(world, "find-end-objs", "g99").fizzled


def test_find_sim_nhbr_picks_the_conceptually_nearest_neighbour(problems):
    world = World(problems["surround-05"], CFG, 1)
    notice_all(world)
    g = row(world, HENRY_ROW)
    out = run(world, "find-sim-nhbr", "knife1")              # its neighbours: fork2 and cup1
    assert out.action == "found" and out.detail.startswith("fork2 near knife1")
    assert types(out.posted) == ["build-obj-0"]
    assert run(world, "find-sim-nhbr", "cup2").fizzled


def test_find_answer_announces_and_a_codelet_with_a_gone_structure_fizzles(problems):
    world = World(problems["surround-05"], CFG, 1)
    out = run(world, "find-answer")
    assert out.action == "answer" and out.answer.object == "cup1" and out.answer.literal
    assert run(world, "build-obj-1", "p9").fizzled
    assert run(world, "cp-compete", "cp9").fizzled and run(world, "examine-cp", "cp9").fizzled
    assert run(world, "build-cp-descrip", "cp9").fizzled


def test_codelets_without_arguments_choose_their_own_target(problems):
    """A codelet the Slipnet posts has no argument and chooses what to look at (D-020)."""
    world = World(problems["surround-05"], CFG, 1)
    for t in ("find-neighbor", "find-end-objs", "search-for-sam-gp", "simple-sameness-detect",
              "build-obj-desc", "find-sim-nhbr"):
        out = run(world, t)
        assert out.fizzled, t
    run(world, "find-group", "cup1")
    assert run(world, "find-neighbor").action == "found"

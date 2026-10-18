"""Item 06 of the roles loop: consumer C3, the answer from group position by the members'
roles (D-097; D-058 revised), and CONTAINER in its two forms (D-087): the holder under a
cup and the setting laid out around an anchor. Nothing is gated: with an empty pool C3
reads nothing and the words decide; `role_member` draws nothing; the C3 lesion (D-104)
patches one function.
"""

import dataclasses

import pytest

from tabletop import answer as AN
from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import events as EV
from tabletop import roles as R
from tabletop import worldview as WV
from tabletop.bench import variants as V
from tabletop.codelets import dispatch
from tabletop.codelets import judges as J
from tabletop.codelets import scouts as SC
from tabletop.coderack import make_codelet
from tabletop.config import GROUP_POSITION_MATCHES, PROFILES, Config
from tabletop.groups import Group
from tabletop.problems import load_problem
from tabletop.run import run_problem
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

import role_probes as RP

CFG = V2_PROFILES["standard"]
HENRY = ["fork1", "fork2", "knife1", "cup1", "spoon1", "spoon2"]
ELIZA = ["spoon3", "spoon4", "small-glass1", "fork3", "fork4", "knife2"]


def world_with_rows(config=CFG, seed=1, name="surround-05"):
    """Surround 5 with every object in view and the two rows built as gestalt groups with
    their neighbours, ends and middles known (D-027 to D-029), as `tests/test_run.py`
    builds them; no role tag yet."""
    world = World(load_problem(name), config, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    rows = []
    for members in (HENRY, ELIZA):
        g = Group(members=list(members))
        world.workspace.add_group(g)
        frontier = [members[0]]
        while frontier:
            for n, _ in DS.find_neighbours(world, g, frontier.pop(0)):
                frontier.append(n)
        DS.find_ends(world, g)
        DS.find_middles(world, g)
        rows.append(g)
    return world, rows


def tag(world, item, role, args=()):
    return R.propose(world, item, role, args, 1, "by hand", "test", 1.0)


def group_cp(world, rows):
    cp = C.create(world, rows[0].name, rows[1].name)
    WV.admit(world, cp)
    return cp


# ---- consumer C3: the answer from group position (D-097) ----------------------------------------


def test_an_empty_pool_reads_nothing_and_the_words_decide():
    """Nothing is gated: with no tag standing `role_member` is None without a draw, and the
    answer is D-058's member in the same position."""
    world, rows = world_with_rows()
    cp = group_cp(world, rows)
    draws = world.rng.draws
    assert AN.roles_in_group(world, rows[0], "cup1") == []
    assert AN.role_member(world, cp, "cup1") is None and world.rng.draws == draws
    a = AN.answer_for(world)
    assert a.object == rows[1].middles[0] and a.through == AN.GROUP and a.roles == ()
    assert "position" in a.note


def test_the_anchor_answers_for_the_anchor_before_the_words():
    """The member with the touched object's roles: anchor for anchor (D-087), even when the
    words would pick another middle (the words' first middle is a spoon)."""
    world, rows = world_with_rows()
    cp = group_cp(world, rows)
    for item in ("cup1", "small-glass1"):
        tag(world, item, R.MEMBER)
        tag(world, item, R.ANCHOR)
    assert AN.roles_in_group(world, rows[0], "cup1") == [R.ANCHOR, DS.MIDDLE]
    draws = world.rng.draws
    assert AN.role_member(world, cp, "cup1") == "small-glass1" and world.rng.draws == draws
    a = AN.answer_for(world)
    assert a.object == "small-glass1" and a.label == "E1" and a.through == AN.GROUP
    assert a.roles == (R.ANCHOR, DS.MIDDLE) and "roles anchor middle" in a.note
    assert not a.literal and a.via == cp.name


def test_each_role_narrows_the_candidates_and_an_unmatched_role_is_skipped():
    """Anchor, connector, end, middle in `ROLE_ORDER`: a role no candidate carries goes
    unmatched; the first candidate left in member order answers."""
    world, rows = world_with_rows()
    cp = group_cp(world, rows)
    tag(world, "cup1", R.MEMBER)
    tag(world, "cup1", R.CONNECTOR)
    for m in ELIZA:
        tag(world, m, R.MEMBER)
    # No connector across: the connector goes unmatched, the middle word decides.
    assert AN.role_member(world, cp, "cup1") == rows[1].middles[0]
    tag(world, "fork3", R.CONNECTOR)
    assert AN.role_member(world, cp, "cup1") == "fork3"       # connector for connector
    # An end for an end, by the words of a member (a MEMBER tag standing).
    assert AN.role_member(world, cp, "fork1") is None
    tag(world, "fork1", R.MEMBER)
    assert AN.role_member(world, cp, "fork1") == rows[1].ends[0]
    # A member with a role none shares and no words: None (not the first member).
    world2, rows2 = world_with_rows()
    cp2 = group_cp(world2, rows2)
    tag(world2, "cup1", R.ANCHOR)
    assert AN.roles_in_group(world2, rows2[0], "cup1") == [R.ANCHOR]
    assert AN.role_member(world2, cp2, "cup1") is None


def test_the_words_value_skips_the_roles_and_both_answer_from_group_position():
    world, rows = world_with_rows(dataclasses.replace(CFG, group_position_match="words"))
    cp = group_cp(world, rows)
    for item in ("cup1", "small-glass1"):
        tag(world, item, R.MEMBER)
        tag(world, item, R.ANCHOR)
    a = AN.answer_for(world)
    assert a.object == rows[1].middles[0] and a.through == AN.GROUP and a.roles == ()
    assert GROUP_POSITION_MATCHES == ("roles", "words")
    for bad in ("literal", "", "off", "none"):
        with pytest.raises(ValueError):
            v2(group_position_match=bad)
    assert not any(f.name == "answer_from_group_position" for f in dataclasses.fields(Config))


def test_find_answer_records_the_roles_matched_as_consumed():
    """The tags matched are consumed by C3 at find-answer (D-105), not by `answer_for`."""
    world, rows = world_with_rows()
    group_cp(world, rows)
    for item in ("cup1", "small-glass1"):
        tag(world, item, R.MEMBER)
        tag(world, item, R.ANCHOR)
    log = EV.Collector()
    world.events.observers.append(log)
    AN.answer_for(world)
    assert log.of_type(EV.RoleConsumed) == []
    out = dispatch.run_codelet(world, make_codelet("find-answer", CFG, 0), 1.0)
    assert out.answer.object == "small-glass1"
    used = log.of_type(EV.RoleConsumed)
    assert [(e.item, e.role, e.consumer) for e in used] == [("cup1", R.ANCHOR, R.C3), ("cup1", R.MEMBER, R.C3)]
    assert all(e.effect == "answer small-glass1" for e in used)
    assert "roles anchor middle" in out.detail


def test_examine_cp_offers_the_anchor_to_the_anchor_first_without_a_draw():
    world, rows = world_with_rows()
    cp = group_cp(world, rows)
    codelet = make_codelet("examine-cp", CFG, 0, (cp.name,))
    # Without anchors: the two salience draws of D-034.
    draws = world.rng.draws
    out = dispatch.run_codelet(world, codelet, 1.0)
    assert out.action == "proposed" and world.rng.draws > draws and "anchor" not in out.detail
    # With one anchor in each group: that pair, no draw, both tags consumed.
    for item in ("cup1", "small-glass1"):
        tag(world, item, R.ANCHOR)
    pairs = C.inner_pairs(world, cp)
    assert J.anchor_pair(world, cp, pairs) == ("cup1", "small-glass1")
    log = EV.Collector()
    world.events.observers.append(log)
    draws = world.rng.draws
    out = dispatch.run_codelet(world, codelet, 1.0)
    assert world.rng.draws == draws and out.detail.endswith("anchor to anchor")
    assert [c.type for c in out.posted] == ["create-cp"] and out.posted[0].args == ("cup1", "small-glass1", cp.name)
    assert {e.item for e in log.of_type(EV.RoleConsumed)} == {"cup1", "small-glass1"}
    # The pair once taken, or a second anchor, or the other anchor missing: back to the draws.
    C.create(world, "cup1", "small-glass1", parent=cp)
    assert J.anchor_pair(world, cp, C.inner_pairs(world, cp)) is None
    world2, rows2 = world_with_rows()
    cp2 = group_cp(world2, rows2)
    tag(world2, "cup1", R.ANCHOR)
    tag(world2, "spoon1", R.ANCHOR)
    tag(world2, "small-glass1", R.ANCHOR)
    assert J.anchor_pair(world2, cp2, C.inner_pairs(world2, cp2)) is None


def test_the_c3_lesion_patches_one_function():
    world, rows = world_with_rows()
    cp = group_cp(world, rows)
    for item in ("cup1", "small-glass1"):
        tag(world, item, R.MEMBER)
        tag(world, item, R.ANCHOR)
    with RP.lesioned("C3"):
        assert AN.answer_for(world).object == rows[1].middles[0]
        assert J.anchor_pair(world, cp, C.inner_pairs(world, cp)) is None
    assert AN.answer_for(world).object == "small-glass1"


# ---- CONTAINER (D-087): the setting and the holder ----------------------------------------------


def test_the_setting_is_a_linear_group_around_its_one_anchor():
    world, rows = world_with_rows()
    henry = rows[0]
    assert DS.setting_of(world, henry.name) == [] and DS.container_of(world, "cup1") == []
    tag(world, "cup1", R.ANCHOR)
    found = DS.container_of(world, henry.name)
    assert found == [(R.CONTAINER, ("cup1",), 6, f"a setting of 6 laid out around cup1")]
    tag(world, "spoon1", R.ANCHOR)
    assert DS.setting_of(world, henry.name) == []          # two anchors: no setting
    assert DS.container_of(world, R.TABLE) == [] and DS.container_of(world, "nothing") == []
    assert DS.is_linear_group(world.problem, henry)


def test_the_holder_is_the_larger_object_under_anothers_centre():
    """The on-saucer variant of Surround 5 (D-102) stands a saucer under the isolated cup."""
    v = V.variant_of("surround-05", "on-saucer")
    cup, saucer = v.params["cup"], v.params["saucer"]
    world = World(v.to_problem(), CFG, 1)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    assert DS.holder_under(world, cup) == saucer and DS.holder_under(world, saucer) is None
    assert DS.holder_of(world, saucer) == [(R.CONTAINER, (cup,), 1,
                                             f"holds {cup}: its centre inside {saucer}'s footprint")]
    assert DS.holder_of(world, cup) == [] and DS.holder_of(world, "cup1") == []
    assert (R.CONTAINER, (cup,), 1) == R.detect(world, saucer)[0][:3]
    # An unnoticed cup is not held yet.
    fresh = World(v.to_problem(), CFG, 1)
    fresh.workspace.notice(saucer, fresh.rng)
    assert DS.holder_of(fresh, saucer) == []


def test_find_roles_reads_the_setting_around_an_anchor_and_the_holder_under_an_object():
    world, rows = world_with_rows()
    henry = rows[0]
    tag(world, "cup1", R.ANCHOR)
    assert SC.container_items(world, ["cup1"], list(world.workspace.roles_of("cup1"))) == [henry.name]
    out = dispatch.run_codelet(world, make_codelet("find-roles", CFG, 0, ("cup1",)), 1.0)
    assert world.workspace.has_role(henry.name, R.CONTAINER)
    assert [c.args for c in out.posted if c.type == "compare-roles"] == [("cup1",), (henry.name,)]
    v = V.variant_of("surround-05", "on-saucer")
    cup, saucer = v.params["cup"], v.params["saucer"]
    world = World(v.to_problem(), CFG, 1)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    assert SC.container_items(world, [R.TABLE, cup], []) == [saucer]
    dispatch.run_codelet(world, make_codelet("find-roles", CFG, 0, (cup,)), 1.0)
    assert world.workspace.has_role(saucer, R.CONTAINER)


def test_c1_pairs_a_setting_with_a_setting_and_a_holder_with_a_holder():
    world, rows = world_with_rows()
    tag(world, R.TABLE, R.MIDLINE)
    tag(world, rows[0].name, R.CONTAINER, ("cup1",))
    tag(world, rows[1].name, R.CONTAINER, ("small-glass1",))
    tag(world, "spoon3", R.CONTAINER, ("fork3",))            # a holder, by hand
    pairs = [(a, b) for *_, a, b, _ in SC.role_pairs(world)]
    assert (rows[0].name, rows[1].name) in pairs
    assert (rows[0].name, "spoon3") not in pairs             # a setting never with a holder
    assert SC.like_with_like(world, R.CONTAINER, rows[0].name, "spoon3") is False
    assert SC.like_with_like(world, R.CONTAINER, "cup1", "spoon3")


def test_whole_runs_read_the_container_and_answer_through_the_roles():
    """A run of the on-saucer variant reads the holder; a library run whose answer comes
    through a group correspondence answers from group position (surround-05 seed 100 at
    this item, seed 8 since v5's fix, D-116 to D-120: the touched cup is a middle, no
    anchor read before the answer, so the roles' reading and the words' agree on the first
    middle of Eliza's row)."""
    v = V.variant_of("surround-05", "on-saucer")
    roles = set()
    for seed in range(1, 9):
        world = World(v.to_problem(), CFG, seed)
        while not world.finished:
            world.step()
        roles |= {(t.item, t.role) for t in world.workspace.roles}
    assert (v.params["saucer"], R.CONTAINER) in roles
    result = run_problem(load_problem("surround-05"), CFG, 8)
    assert result.through == AN.GROUP and result.answer == "spoon4" and not result.literal

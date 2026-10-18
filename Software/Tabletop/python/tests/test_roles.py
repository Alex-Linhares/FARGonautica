"""The role substrate (D-078 to D-081, D-105): the closed registry of 23, the pool as a part
of the Workspace, tags as structures with a strength that fight their rivals and go with
their structure, the role checker, the 15 Slipnet nodes and their links, the two role
codelets in the catalogue, the events and the snapshot, the pending list the later items
empty, and the no-switch rule made live.
"""

import dataclasses
import json
from pathlib import Path

import pytest

from tabletop import correspondences as C
from tabletop import events as EV
from tabletop import roles as R
from tabletop import slipnet_def as D
from tabletop import snapshot as SN
from tabletop import worldview as WV
from tabletop.codelets import catalogue, dispatch
from tabletop.codelets import judges as J
from tabletop.config import CODELET_TYPES, PROFILES, Config
from tabletop.coderack import make_codelet
from tabletop.groups import Group
from tabletop.problems import load_problem
from tabletop.run import run_problem
from tabletop.world import World
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
DOCS = Path(__file__).resolve().parents[1] / "docs"


def world_of(name="surround-05", seed=1):
    world = World(load_problem(name), CFG, seed)
    for o in world.problem.objects:
        world.workspace.notice(o.id, world.rng)
    return world


# ---- the registry (D-079) -------------------------------------------------------------------


def test_the_registry_is_closed_at_23_in_five_families_with_a_node_each():
    assert len(R.REGISTRY) == 23 and len(set(R.NAMES)) == 23
    assert {s.family for s in R.REGISTRY} == set(R.FAMILIES)
    assert [len(R.of_family(f)) for f in R.FAMILIES] == [3, 6, 5, 2, 7]
    for s in R.REGISTRY:
        assert s.reading in R.READINGS and s.nodes and s.item in ("04", "05", "06", "07", "08", "09", "10")
        for node in s.nodes:
            assert node in D.NODE_INDEX, (s.name, node)
        assert s.code[0] in "GSLKA" and s.code[1:].isdigit()
    assert R.spec("LONER").nodes == ("isolated",) and R.spec("BARRIER").nodes == ("barrier",)
    with pytest.raises(KeyError):
        R.spec("HERO")


def test_the_pending_list_names_the_roles_later_items_still_owe():
    """Item 03 builds the substrate and consumer C1; every detector and the other
    consumers come with items 04 to 10, which empty this list role by role; CONTAINER
    (item 06, re-opened last) empties it."""
    pending = R.pending()
    assert pending == []
    built = {"MIRROR_IMAGE", "POINT_IMAGE", "SYMMETRY_AXIS", "MIDLINE",     # item 04
             "MEMBER", "SIDEKICK", "ANCHOR", "CONNECTOR",                   # item 05
             "CONTAINER",                                                   # item 06
             "ROW", "COLUMN", "COUNTERPART",                                # item 07
             "GAP", "CORNER", "LONER", "STRAY",                             # item 08
             "FIGURE", "GROUND", "ECHO", "TWIN", "TYPE_KEY",                # item 09
             "BARRIER", "CLAIMED"}                                          # item 10
    assert built == set(R.NAMES)
    assert R.spec("CONTAINER").consumers == ("C1", "C3") and "C3" in R.spec("ANCHOR").consumers
    assert all(R.spec(n).detector and R.spec(n).consumers for n in built)
    assert all("C1" in R.spec(n).consumers for n in R.NAMES if R.spec(n).pair)
    assert [r for r, *_ in R.detect(world_of(), "cup1")] == ["COLUMN"]     # item 07: at first sight


def test_rivals_are_symmetric_and_name_registered_roles():
    for a, b in R.RIVALS:
        assert a in R.NAMES and b in R.NAMES and a != b
        assert b in R.rivals_of(a) and a in R.rivals_of(b)
    assert R.rivals_of("ANCHOR") == []        # an anchor and a connector coexist


# ---- the pool and the tags (D-079) ----------------------------------------------------------


def test_tagging_names_rates_jolts_and_publishes():
    world = world_of()
    log = EV.Collector()
    world.events.observers.append(log)
    before = world.slipnet.activation_of("isolated")
    tag = R.propose(world, "cup2", "LONER", (), 1, "isolated", "test", 1.0)
    assert tag.name == "r1" and tag in world.workspace.roles and tag.strength > 0
    assert world.workspace.roles_of("cup2") == [tag] and world.workspace.items_with("LONER") == ["cup2"]
    assert world.workspace.role_tag("cup2", "LONER") is tag and world.workspace.has_role("cup2", "LONER")
    assert world.slipnet.activation_of("isolated") == before + CFG.jolt_description
    tagged = log.of_type(EV.RoleTagged)
    assert len(tagged) == 1 and tagged[0].tag == "r1" and tagged[0].role == "LONER"
    assert tagged[0].by == "test" and tagged[0].strength == tag.strength
    # The same reading again is re-rated, not doubled.
    assert R.propose(world, "cup2", "LONER", (), 1, "isolated", "test", 1.0) is tag
    assert len(world.workspace.roles) == 1


def test_the_strength_takes_weight_node_and_story():
    world = world_of()
    node = world.slipnet.activation_of("sidekick") / 150.0    # rated before its own jolt
    tag = R.propose(world, "spoon1", "SIDEKICK", ("cup1",), 2, "two spoons", "test", 1.0)
    assert tag.strength == pytest.approx(2 * CFG.role_weight_points + CFG.role_node_points * node)
    cp = C.create(world, "cup1", "cup2")
    WV.compete(world, cp, 6.0)
    assert cp.in_worldview
    node = world.slipnet.activation_of("sidekick") / 150.0
    assert R.rate(world, tag) == pytest.approx(
        2 * CFG.role_weight_points + CFG.role_node_points * node + CFG.role_story_points)
    assert 0 <= R.rate(world, tag) <= 100


def test_rival_tags_fight_and_the_loser_is_discarded():
    world = world_of()
    log = EV.Collector()
    world.events.observers.append(log)
    member = R.propose(world, "cup2", "MEMBER", (), 1, "in a row", "test", 6.0)
    loner = R.propose(world, "cup2", "LONER", (), 1, "isolated", "test", 6.0)
    standing = world.workspace.roles_of("cup2")
    assert len(standing) == 1 and standing[0] in (member, loner)
    dropped = log.of_type(EV.RoleDiscarded)
    assert len(dropped) == 1 and dropped[0].reason.startswith("lost to r")
    # Compatible roles coexist.
    R.propose(world, "cup1", "MEMBER", (), 1, "", "test", 6.0)
    R.propose(world, "cup1", "ANCHOR", (), 1, "", "test", 6.0)
    assert {t.role for t in world.workspace.roles_of("cup1")} == {"MEMBER", "ANCHOR"}


def test_tags_go_with_their_structure():
    world = world_of()
    g = Group(members=["spoon1", "spoon2"])
    world.workspace.add_group(g)
    R.propose(world, g.name, "ECHO", (), 2, "", "test", 1.0)
    R.propose(world, "spoon1", "SIDEKICK", (g.name,), 1, "", "test", 1.0)
    R.propose(world, "cup2", "LONER", (), 1, "", "test", 1.0)
    world.workspace.remove_group(g)
    assert [t.role for t in world.workspace.roles] == ["LONER"]
    cp = C.create(world, "cup1", "cup2")
    R.propose(world, cp.name, "CLAIMED", (), 1, "", "test", 1.0)
    world.workspace.remove_correspondence(cp)
    assert [t.role for t in world.workspace.roles] == ["LONER"]


def test_the_checker_re_rates_and_may_break_and_fizzles_on_an_empty_pool():
    world = world_of()
    codelet = make_codelet("check-roles", CFG, 0)
    draws = world.rng.draws
    out = J.check_roles(world, codelet, 6.0)
    assert out.fizzled and world.rng.draws == draws
    strong = v2(role_weight_points=1000.0)     # re-rated to the ceiling: unbreakable
    world = World(load_problem("surround-05"), strong, 1)
    world.workspace.notice("cup2", world.rng)
    tag = R.propose(world, "cup2", "LONER", (), 1, "", "test", 1.0)
    out = dispatch.run_codelet(world, codelet, 6.0)
    assert out.action == "found" and tag in world.workspace.roles and tag.strength == 100.0
    assert R.break_chance(world, tag) == max(0.0, 1 - tag.strength / CFG.role_break_scale)
    weak = v2(role_weight_points=0.01, role_node_points=0.01, role_story_points=0.01)
    world = World(load_problem("surround-05"), weak, 1)
    world.workspace.notice("cup2", world.rng)
    weak_tag = R.propose(world, "cup2", "LONER", (), 1, "", "test", 1.0)
    assert weak_tag.strength < 1
    broken = 0
    for _ in range(20):
        if weak_tag not in world.workspace.roles:
            break
        if dispatch.run_codelet(world, codelet, 6.0).action == "removed":
            broken += 1
    assert broken == 1 and weak_tag not in world.workspace.roles


# ---- the Slipnet (D-080) and the catalogue (D-081) ---------------------------------------------


def test_the_slipnet_has_63_nodes_and_the_roles_links():
    assert len(D.NODES) == 63 and len(D.ROLE_NODES) == 15
    assert D.node_named("role").level == 3 and D.node_named("counterpart").level == 3
    assert all(D.node_named(n).level == 2 for n in D.ROLE_NODES if n not in ("role", "counterpart"))
    isa = [l for l in D.ROLE_LINKS if l.kind == D.ISA]
    labelled = [l for l in D.ROLE_LINKS if l.kind == D.LABELED]
    assert len(isa) == 14 and len(labelled) == 15
    assert {l.b for l in isa} == {"role", "position"}
    assert D.labels_between("mirror symmetry", "diagonal symmetry") == ("opposite",)
    assert D.labels_between("figure", "ground") == ("opposite",)
    assert D.labels_between("key", "opposite") == ("role",)
    assert all(s.nodes[0] in D.NODE_INDEX for s in R.REGISTRY)
    net = World(load_problem("surround-05"), CFG, 1).slipnet
    assert net.distance("mirror symmetry", "diagonal symmetry") <= 40


def test_the_catalogue_has_29_types_and_the_role_rows_post_top_down():
    assert len(CODELET_TYPES) == 29 and catalogue.names()[23:] == [
        "propose-role-cp", "check-roles", "find-roles", "compare-roles", "read-worldview-roles",
        "recast-role"]
    assert CFG.as_dict("urgencies")["recast-role"] == 40                     # D-081, item 07
    assert [n for n, c in CFG.top_down_table if c == "recast-role"] == ["counterpart", "row", "column"]
    assert CFG.as_dict("urgencies")["find-roles"] == 30 and CFG.as_dict("urgencies")["read-worldview-roles"] == 50
    assert ("separator", "find-roles") in CFG.top_down_table
    assert ("symmetry", "read-worldview-roles") in CFG.top_down_table
    table = CFG.as_dict("urgencies")
    assert table["propose-role-cp"] == 60 and table["check-roles"] == 20
    rows = [(n, c) for n, c in CFG.top_down_table if c in ("propose-role-cp", "check-roles")]
    assert ("role", "check-roles") in rows and len(rows) == 10
    assert all(n in D.ROLE_NODES for n, _ in rows)
    assert set(dispatch.FUNCTIONS) == set(catalogue.names())


# ---- events and the snapshot (D-105) ----------------------------------------------------------


def test_the_role_events_are_appended_and_round_trip_through_json():
    names = [cls.__name__ for cls in EV.EVENT_TYPES]
    assert names[-4:] == ["RoleTagged", "RoleDiscarded", "RoleRecast", "RoleConsumed"]
    world = world_of()
    log = EV.Collector()
    world.events.observers.append(log)
    tag = R.propose(world, "cup2", "LONER", (), 1, "isolated", "test", 1.0)
    R.consumed(world, tag, "C1", "create-cp cup2 cup1", "test")
    world.workspace.discard_role(tag, R.BROKEN, "test")
    for e in log.events:
        if e.name.startswith("Role"):
            record = json.loads(json.dumps(EV.record_of(e)))
            cls = getattr(EV, record.pop("event"))
            again = cls(**{k: tuple(v) if isinstance(v, list) else v for k, v in record.items()})
            assert again == e
    assert [e.name for e in log.events if e.name.startswith("Role")] == [
        "RoleTagged", "RoleConsumed", "RoleDiscarded"]
    assert tag.consumed == [("C1", "create-cp cup2 cup1")]


def test_the_snapshot_carries_the_pool_the_recast_and_the_axis():
    world = world_of()
    s = SN.snapshot(world)
    assert s.roles == () and s.recast is None and s.axis is None
    R.propose(world, "cup2", "LONER", (), 1, "isolated", "test", 1.0)
    R.propose(world, "worldview", "SYMMETRY_AXIS", ("mirror",), 1, "", "test", 1.0)
    s = SN.snapshot(world)
    assert [r["role"] for r in s.roles] == ["LONER", "SYMMETRY_AXIS"]
    assert s.axis["args"] == ("mirror",) and s.axis["name"] == "r2"
    assert dataclasses.is_dataclass(s) and json.dumps(dataclasses.asdict(s))


def test_the_docs_name_the_role_events_the_rl_lines_and_the_snapshot_fields():
    api = (DOCS / "engine-api.md").read_text()
    for name in ("RoleTagged", "RoleDiscarded", "RoleRecast", "RoleConsumed"):
        assert f"`{name}`" in api
    assert "`roles`" in api and "`recast`" in api and "`axis`" in api
    trace = (DOCS / "trace-format.md").read_text()
    assert "RL tag" in trace and "RL drop" in trace and "RL used" in trace


# ---- the no-switch rule, live (D-078; ANSWERS.md C4) ----------------------------------------


def test_every_role_knob_is_refused_at_zero_and_below_and_profiles_run_with_roles():
    knobs = [f.name for f in dataclasses.fields(Config) if "role" in f.name]
    assert knobs == ["role_weight_points", "role_node_points", "role_story_points",
                     "role_break_scale", "role_pair_floor", "role_recast_tiers"]
    for name in knobs + ["stray_salience_scale"]:       # D-100: no "role" in its name
        for bad in (0, -1, -0.5):
            with pytest.raises(ValueError):
                dataclasses.replace(V2, **{name: bad})
    with pytest.raises(ValueError):                      # D-100: the ninth salience term
        v2(salience_weights=tuple((n, 0 if n == "role" else w)
                                      for n, w in V2.salience_weights))
    with pytest.raises(ValueError):
        table = tuple((n, 0 if n == "propose-role-cp" else u) for n, u in V2.urgencies)
        v2(urgencies=table)
    for profile in PROFILES.values():
        assert dict(profile.urgencies)["propose-role-cp"] > 0      # 60 in v2, 89 in v3 (D-114)
        assert dict(profile.top_down_table)["role"] == "check-roles"
    result = run_problem(load_problem("blockage-02"), V2_PROFILES["french-faults"], 1)
    assert result.answer in load_problem("blockage-02").ids()

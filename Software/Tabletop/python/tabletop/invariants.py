"""The invariants a run must keep after every codelet, and a run that checks them (D-077).

Each invariant is a rule the sources state or a decision fixes: the Worldview is "a
non-self-contradictory subset of the Workspace" [FCCA p. 390] in which "no object or
group of objects is the end object for two distinct correspondences" [F95 p. 71] (D-044);
activations lie between 1 and 150 (D-015); the Coderack holds at most 150 codelets [F95
p. 82] and none older than its age limit (D-053); the temperature lies between 0 and 100
(D-047); every structure in the Workspace stands on things that exist (D-041, D-025); a
sameness group holds one type and, under the fix of D-026, no sameness group of its type
lies inside another; names are never reused (D-065); every role tag stands on an item that
exists, names only structures that stand, is the one tag of its reading, fights no standing
rival, and a CLAIMED tag has its BARRIER (D-079, D-089; D-077 as revised by the roles
loop's item 11). `check(world)` returns the
violations as sentences, empty when every invariant holds; it reads the state and draws
nothing from the generator, so a checked run is the same run (`tests/test_invariants.py`
asserts the fingerprint). `checked_run` steps a run with the check after every codelet
and returns the result with the violations found, for the robustness bench and tests.
"""

from __future__ import annotations

import math
import re

from tabletop import answer as AN
from tabletop import groups as GR
from tabletop import roles as R
from tabletop import worldview as WV
from tabletop.config import Config
from tabletop.problems import Problem
from tabletop.run import Result, result_of
from tabletop.world import World

THROUGH = ("correspondence", "group", "default", "blocked")   # `Answer.through` (D-057, D-063, D-101)


def check(world: World) -> list:
    """Every invariant, checked on the world as it stands (D-077); the violations as
    sentences, in the order of the components: Slipnet, Coderack, temperature, Workspace
    objects, groups, correspondences, Worldview, roles, context, and the end of the run."""
    faults = []
    faults += check_slipnet(world)
    faults += check_coderack(world)
    faults += check_temperature(world)
    faults += check_objects(world)
    faults += check_groups(world)
    faults += check_correspondences(world)
    faults += check_worldview(world)
    faults += check_roles(world)
    faults += check_context(world)
    if world.finished:
        faults += check_end(world)
    return faults


def check_slipnet(world: World) -> list:
    """Every activation between 1 and 150 (D-015: "from 1 to 150" [F95 p. 59]) and
    finite."""
    low, high = 1, 150
    return [f"{name} is at {value}, outside [{low}, {high}]"
            for name, value in world.slipnet.activations()
            if not (math.isfinite(value) and low <= value <= high)]


def check_coderack(world: World) -> list:
    """At most `coderack_capacity` codelets [F95 p. 82]; none posted in the future; none
    older than the age limit plus the codelet just run, since ageing happens at the start
    of a step (D-053); every urgency finite and at or above the floor (D-052)."""
    cfg, rack = world.config, world.coderack
    faults = []
    if len(rack) > cfg.coderack_capacity:
        faults.append(f"the Coderack holds {len(rack)}, over its capacity of "
                      f"{cfg.coderack_capacity}")
    for c in rack.codelets:
        if c.posted_at > world.clock:
            faults.append(f"{c.label} was posted at {c.posted_at}, after the clock {world.clock}")
        if c.age(world.clock) > cfg.max_codelet_age + 1:
            faults.append(f"{c.label} is {c.age(world.clock)} codelets old")
        if not (math.isfinite(c.urgency) and c.urgency >= cfg.urgency_floor):
            faults.append(f"{c.label} has urgency {c.urgency}")
    return faults


def check_temperature(world: World) -> list:
    """T between 0 and 100 (D-047) and its exponent between the two ends of a(T) [F95
    p. 92]; Q within (0, 1]."""
    cfg, t = world.config, world.temperature
    faults = []
    if not (0 <= t.value <= 100):
        faults.append(f"T is {t.value}")
    low, high = sorted((cfg.exponent_at_zero, cfg.exponent_at_hundred))
    if not (low - 1e-9 <= t.exponent <= high + 1e-9):
        faults.append(f"a(T) is {t.exponent}")
    if not (0 < t.q <= 1):
        faults.append(f"Q is {t.q}")
    return faults


def check_objects(world: World) -> list:
    """The objects in view are objects of the problem, each noticed once, the touched one
    among them [F95 p. 64]; the isolated ones are in view; every description belongs to an
    object in view, a group or a correspondence in the Workspace (D-029, D-035; removed
    structures take their descriptions with them)."""
    ws, ids = world.workspace, world.problem.ids()
    faults = []
    if len(set(ws.noticed)) != len(ws.noticed):
        faults.append("an object was noticed twice")
    for id_ in ws.noticed:
        if id_ not in ids:
            faults.append(f"{id_} in view is not an object of the problem")
    if world.problem.touched not in ws.noticed:
        faults.append("the touched object is not in view")
    for id_ in ws.isolated:
        if id_ not in ws.noticed:
            faults.append(f"{id_} is isolated but not in view")
    names = [g.name for g in ws.groups] + [c.name for c in ws.correspondences]
    for item in ws.descriptions:
        if item not in ws.noticed and item not in names:
            faults.append(f"{item} is described but is neither in view nor a structure")
    return faults


def check_groups(world: World) -> list:
    """Names unique and never reused (D-065); two or more distinct members, every one an
    object of the problem; a parent, when there is one, holds every member (D-024); a
    sameness group holds one type [F95 p. 65]; under the fix of D-026 no sameness group
    lies inside another of its type; strengths finite."""
    ws, problem, cfg = world.workspace, world.problem, world.config
    ids = problem.ids()
    faults = []
    names = [g.name for g in ws.groups]
    if len(set(names)) != len(names):
        faults.append("two groups share a name")
    for g in ws.groups:
        if len(g.members) < 2 or len(set(g.members)) != len(g.members):
            faults.append(f"{g.name} has members {g.members}")
        for m in g.members:
            if m not in ids:
                faults.append(f"{g.name} has a member {m} that is not an object")
        if g.parent is not None and not g.parent.contains(g.members):
            faults.append(f"{g.name}'s parent does not hold its members")
        if g.kind == GR.SAMENESS and GR.group_type(problem, g) is None:
            faults.append(f"{g.name} is a sameness group of several types")
        if not math.isfinite(g.strength):
            faults.append(f"{g.name} has strength {g.strength}")
    if not cfg.keep_all_subgroups:
        for g in ws.groups:
            type_ = GR.group_type(problem, g)
            if type_ is None:
                continue
            for h in ws.groups:
                if h is not g and GR.group_type(problem, h) == type_ \
                        and h.contains(g.members) and len(h.members) > len(g.members):
                    faults.append(f"{g.name} lies inside {h.name}, a sameness group of its type")
    return faults


def check_correspondences(world: World) -> list:
    """Names unique (D-065); two distinct ends, each an object of the problem or a group
    in the Workspace (D-041: orphans are dropped); a parent in the Workspace; points
    finite and the rating never negative (D-038)."""
    ws, ids = world.workspace, world.problem.ids()
    faults = []
    names = [c.name for c in ws.correspondences]
    if len(set(names)) != len(names):
        faults.append("two correspondences share a name")
    for c in ws.correspondences:
        if c.henry == c.eliza:
            faults.append(f"{c.name} maps {c.henry} to itself")
        for end in c.ends:
            if end not in ids and not ws.has_group(end):
                faults.append(f"{c.name} ends on {end}, which does not exist")
        if c.parent is not None and c.parent not in ws.correspondences:
            faults.append(f"{c.name}'s parent {c.parent.name} is not in the Workspace")
        if not all(math.isfinite(v) for v in (c.static, c.dynamic, c.bonus)):
            faults.append(f"{c.name} has points {c.static}, {c.dynamic}, {c.bonus}")
        if c.rating < 0:
            faults.append(f"{c.name} has a negative rating")
    return faults


def check_worldview(world: World) -> list:
    """The Worldview's own rule: no item the end of two of its correspondences and no
    usurping [F95 p. 71] (D-044), read by `worldview.violations`; and the structure value
    equals the rating of the Worldview as it stands (D-062)."""
    faults = WV.violations(world)
    if world.structure_value() != WV.current(world).total:
        faults.append("the structure value is not the Worldview's rating")
    if world.structure_value() < 0:
        faults.append("the structure value is negative")
    return faults


GROUP_NAME = re.compile(r"g\d+")             # a group's name (D-065)
CORRESPONDENCE_NAME = re.compile(r"cp\d+")     # a correspondence's name (D-065)


def check_roles(world: World) -> list:
    """The role pool's invariants (D-079, D-089; D-077 revised): names unique; every tag a
    role of the registry with a finite strength in 0 to 100 and a weight of one or more;
    every tag's item exists (an object in view, a group in the Workspace, the table or
    the Worldview item); every structure a tag names in its arguments stands (an object
    in view, a group or a correspondence in the Workspace: no tag on a removed structure);
    one tag per reading (`RoleTag.same_as`); no two standing rivals on one item (`RIVALS`,
    the fight leaves one); a CLAIMED tag has a standing BARRIER naming it (D-089); the
    COUNTERPART tags and the re-cast are the touched object's (D-093)."""
    ws, problem = world.workspace, world.problem
    ids, touched = problem.ids(), problem.touched
    tags = list(ws.roles)
    faults = []
    names = [t.name for t in tags]
    if len(set(names)) != len(names):
        faults.append("two role tags share a name")
    cp_names = [c.name for c in ws.correspondences]
    for i, t in enumerate(tags):
        if t.role not in R.NAMES:
            faults.append(f"{t.name} carries {t.role}, no role of the registry")
        if not (math.isfinite(t.strength) and 0.0 <= t.strength <= 100.0) or t.weight < 1:
            faults.append(f"{t.name} has strength {t.strength} and weight {t.weight}")
        if not (t.item in ws.noticed or ws.has_group(t.item)
                or t.item in (R.TABLE, R.WORLDVIEW_ITEM)):
            faults.append(f"{t.name} ({t.role}) stands on {t.item}, which does not exist")
        for arg in t.args:
            if (arg in ids and arg not in ws.noticed) \
                    or (GROUP_NAME.fullmatch(arg) and not ws.has_group(arg)) \
                    or (CORRESPONDENCE_NAME.fullmatch(arg) and arg not in cp_names):
                faults.append(f"{t.name} ({t.role}) names {arg}, which does not stand")
        for u in tags[i + 1:]:
            if u.same_as(t.item, t.role, t.args):
                faults.append(f"{t.name} and {u.name} are one reading, {t.role} on {t.item}")
            if u.item == t.item and u.role in R.rivals_of(t.role):
                faults.append(f"{t.name} ({t.role}) and {u.name} ({u.role}) are rivals on {t.item}")
        if t.role == R.CLAIMED and not any(
                b.role == R.BARRIER and t.args and b.item == t.args[0] and t.item in b.args
                for b in tags):
            faults.append(f"{t.name} is CLAIMED without a BARRIER")
        if t.role == R.COUNTERPART and (not t.args or t.args[0] != touched):
            faults.append(f"{t.name} is a COUNTERPART of {t.args[:1]}, not of the touched object")
    if ws.recast is not None and ws.recast.touched != touched:
        faults.append("the re-cast is not the touched object's")
    return faults


def check_context(world: World) -> list:
    """A context, when the program is in one, names a correspondence and reset nodes that
    exist (D-021); the engine never nests contexts."""
    cx = world.context
    if cx is None:
        return []
    faults = []
    names = [n for n, _ in world.slipnet.activations()]
    for node in cx.reset:
        if node not in names:
            faults.append(f"the context reset {node}, which is no node")
    if cx.pushed_at > world.clock:
        faults.append("the context was pushed in the future")
    return faults


def check_end(world: World) -> list:
    """At the end: the answer is an object of the problem, literal exactly when it is the
    touched object (D-063), read through a known route (D-057), `blocked` only for a
    literal answer while a CLAIMED tag stands (D-101); a capped run has run exactly
    `max_codelets` and no run more (D-061); the codelets run equal the clock."""
    cfg, answer = world.config, world.answer
    faults = []
    if answer is None:
        return ["the run is finished without an answer"]
    if answer.object not in world.problem.ids():
        faults.append(f"the answer {answer.object} is not an object")
    if answer.literal != (answer.object == world.problem.touched):
        faults.append("the answer's literal flag disagrees with the touched object")
    if answer.through not in THROUGH:
        faults.append(f"the answer came through {answer.through!r}")
    claimed = any(t.role == "CLAIMED" for t in world.workspace.roles)
    if (answer.through == "blocked") != (claimed and answer.through in ("blocked", "default")):
        faults.append("the answer's blocked record disagrees with the CLAIMED tags")
    if answer.label != AN.label_for(world.problem, answer.object):
        faults.append("the answer's label is not the file's")
    if world.clock > cfg.max_codelets:
        faults.append(f"{world.clock} codelets ran, over the cap")
    if world.capped and world.clock != cfg.max_codelets:
        faults.append("a capped run did not run to the cap")
    return faults


def checked_run(problem: Problem, config: Config, seed: int, observers=()) -> tuple:
    """One whole run with `check` after every codelet and at the end (D-077): returns
    (`Result`, the violations as (clock, sentence) pairs). The check draws nothing, so the
    result equals `run_problem`'s for the same arguments."""
    world = World(problem, config, seed, observers)
    faults = [(0, f) for f in check(world)]
    while not world.finished:
        world.step()
        faults += [(world.clock, f) for f in check(world)]
    return result_of(world), faults


def summary(faults: list) -> dict:
    """The violations counted by sentence, the clock of the first dropped (D-077)."""
    counts: dict[str, int] = {}
    for _, sentence in faults:
        counts[sentence] = counts.get(sentence, 0) + 1
    return counts


__all__ = ["check", "checked_run", "summary", "Result"]

"""A frozen copy of a run's state for a GUI to read between events (D-065;
`docs/engine-api.md`, "The read-only state").

French's screen showed, at any moment, the table with its structures, the thermometer, the
Slipnet grid and the Coderack bars [F95 pp. 95-107 (img)]. `snapshot(world)` copies all of
that into plain data: a `Snapshot` of tuples, dictionaries, numbers and strings, built from
the engine's read-only accessors, which draw nothing from the generator. Changing a
snapshot changes nothing in the run.
"""

from dataclasses import dataclass

from tabletop import answer as AN
from tabletop import correspondences as C
from tabletop import objects as O
from tabletop import salience as S
from tabletop import worldview as WV
from tabletop.slipnet_def import LABELED


@dataclass(frozen=True)
class Snapshot:
    """The state of a run at one moment, as plain data (D-065): the run (`clock`, `draws`,
    `finished`, `capped`, `problem`, `seed`, `profile`, `touched`, `answer`), the `table`
    (one dictionary per object: id, type, x, y, orientation, box, region, noticed), the
    `slipnet` (name, label, level, depth, activation per node) and its labeled `links`
    (a, b, labels, length, effective), the Workspace (`noticed`, `isolated`,
    `descriptions`, `groups`, `proposals`, `correspondences`, `saliences`), the
    `worldview` (names, the three terms, the total, the pairs, `coherent`), the
    `competitions` and `changes` logs, the `context`, the `coderack` (codelets, shares,
    total urgency), the `temperature` (value, q, dq, dwv, tcp, a, rates), and the roles
    (D-105): `roles` (one dictionary per tag standing: name, item, role, args, weight,
    strength, evidence, by, built_at, consumed), `recast` (the last re-cast published,
    D-093: touched, signature, tiers, ambiguous, clock; None until one is) and `axis` (the symmetry axis tag's dictionary, None until
    one is tagged)."""

    clock: int
    draws: int
    finished: bool
    capped: bool
    problem: str
    seed: int
    profile: str
    touched: str
    answer: dict | None
    table: tuple
    slipnet: tuple
    links: tuple
    noticed: tuple
    isolated: tuple
    descriptions: dict
    groups: tuple
    proposals: tuple
    correspondences: tuple
    saliences: tuple
    worldview: dict
    competitions: tuple
    changes: tuple
    context: dict | None
    coderack: dict
    temperature: dict
    roles: tuple = ()
    recast: dict | None = None
    axis: dict | None = None


def table_of(world) -> tuple:
    """The objects with their footprints (D-001 to D-003) and the region each was last seen
    in (D-004), None until noticed."""
    ws = world.workspace
    return tuple({"id": o.id, "type": o.type, "x": o.x, "y": o.y, "orientation": o.orientation,
                  "box": O.box(o), "region": ws.regions.get(o.id), "noticed": ws.is_noticed(o.id)}
                 for o in world.problem.objects)


def slipnet_of(world) -> tuple:
    """Every node with its label, level, depth and activation [F95 p. 95] (D-008, D-015)."""
    net = world.slipnet
    activation = net.activation        # the list, or the compiled engine's view (D-111)
    return tuple({"name": n.name, "label": n.label, "level": n.level, "depth": net.depth[i],
                  "activation": activation[i]} for i, n in enumerate(net.nodes))


def links_of(world) -> tuple:
    """The labeled links with their intrinsic and effective lengths [F95 p. 56] (D-011)."""
    net = world.slipnet
    effective = net.effective          # the list, or the compiled engine's view (D-111)
    return tuple({"a": l.a, "b": l.b, "labels": tuple(l.labels), "length": l.length,
                  "effective": effective[i]}
                 for i, l in enumerate(net.links) if l.kind == LABELED)


def group_of(g) -> dict:
    """One group as the GUI reads it (D-023 to D-029)."""
    return {"name": g.name, "members": tuple(g.members), "kind": g.kind, "category": g.category,
            "strength": g.strength, "parent": g.parent.name if g.parent is not None else None,
            "orientation": g.orientation, "linear": g.linear,
            "ends": tuple(g.ends) if g.ends is not None else None,
            "middles": tuple(g.middles) if g.middles is not None else None,
            "neighbours": {k: tuple(tuple(p) for p in v) for k, v in g.neighbours.items()},
            "visited": tuple(g.visited), "positions": dict(g.positions)}


def correspondence_of(world, cp) -> dict:
    """One correspondence as the GUI reads it (D-035 to D-039), its points recomputed as
    they stand now without being stored, so that reading changes nothing."""
    static, dynamic = C.static_points(world, cp), C.dynamic_points(world, cp)
    ws = world.workspace
    return {"name": cp.name, "henry": cp.henry, "eliza": cp.eliza, "level": cp.level,
            "static": static, "dynamic": dynamic, "bonus": cp.bonus,
            "rating": max(0.0, static + dynamic + cp.bonus),
            "slippages": tuple(s.as_tuple() for s in cp.slippages), "wins": cp.wins,
            "losses": cp.losses, "tentative": not cp.tested, "tested": cp.tested,
            "passed": cp.passed, "in_worldview": cp.in_worldview,
            "loser": cp.losses > 0 and not cp.in_worldview,
            "descriptors": tuple(ws.descriptors(cp.name))}


def worldview_of(world) -> dict:
    """The Worldview's rating (D-042, D-043) and its pairs."""
    r = WV.current(world)
    return {"names": tuple(r.names), "strength": r.strength, "coherence": r.coherence,
            "parallelism": r.parallelism, "total": r.total, "coherent": r.coherent,
            "coherent_pairs": tuple(tuple(p) for p in r.coherent_pairs),
            "incoherent_pairs": tuple(tuple(p) for p in r.incoherent_pairs),
            "parallel_pairs": tuple(tuple(p) for p in r.parallel_pairs)}


def coderack_of(world) -> dict:
    """The rack's codelets in posting order, the strip's shares and the total urgency
    [F95 p. 97] (D-052, D-053)."""
    rack = world.coderack
    return {"codelets": tuple({"type": c.type, "label": c.label, "args": tuple(c.args),
                               "raw": c.raw, "urgency": c.urgency, "generation": c.generation,
                               "posted_at": c.posted_at, "age": c.age(world.clock), "by": c.by,
                               "chance": c.chance} for c in rack.codelets),
            "shares": tuple(rack.shares()), "total_urgency": rack.total_urgency()}


def temperature_of(world) -> dict:
    """T and its inputs (D-047 to D-050)."""
    t = world.temperature
    return {"value": t.value, "q": t.q, "dq": t.dq, "dwv": t.dwv, "tcp": t.tcp,
            "a": t.exponent, "rates": tuple(t.rates)}


def role_of(tag) -> dict:
    """One role tag as the GUI reads it (D-079, D-105)."""
    return {"name": tag.name, "item": tag.item, "role": tag.role, "args": tuple(tag.args),
            "weight": tag.weight, "strength": tag.strength, "evidence": tag.evidence,
            "by": tag.by, "built_at": tag.built_at,
            "consumed": tuple(tuple(c) for c in tag.consumed)}


def roles_of(world) -> tuple:
    """Every tag of the pool, in the order tagged (D-079)."""
    return tuple(role_of(t) for t in world.workspace.roles)


def recast_of(world) -> dict | None:
    """The last re-cast published, as a dictionary (D-093, D-105): `touched`, `signature`,
    `tiers` ((tier, candidate, weight) tuples), `ambiguous`, `clock`; None until the
    recast-role codelet publishes one."""
    rc = world.workspace.recast
    if rc is None:
        return None
    return {"touched": rc.touched, "signature": tuple(rc.signature), "tiers": tuple(rc.tiers),
            "ambiguous": rc.ambiguous, "clock": rc.clock}


def axis_of(world) -> dict | None:
    """The symmetry axis as tagged on the item `worldview` (D-105; the axis itself comes
    with the roles loop's item 04), None until one is."""
    tags = [t for t in world.workspace.roles if t.item == "worldview"]
    return role_of(tags[-1]) if tags else None


def snapshot(world) -> Snapshot:
    """Copy the run's state (D-065). Draws nothing: every accessor it reads is read-only."""
    ws = world.workspace
    answer = world.answer
    return Snapshot(
        clock=world.clock, draws=world.rng.draws, finished=world.finished, capped=world.capped,
        problem=world.problem.name, seed=world.seed, profile=world.config.profile,
        touched=world.problem.touched,
        answer=(None if answer is None else {"object": answer.object, "label": answer.label,
                                             "literal": answer.literal, "through": answer.through,
                                             "via": answer.via, "note": answer.note}),
        table=table_of(world), slipnet=slipnet_of(world), links=links_of(world),
        noticed=tuple(ws.noticed), isolated=tuple(ws.isolated),
        descriptions={k: tuple(v) for k, v in ws.descriptions.items()},
        groups=tuple(group_of(g) for g in ws.groups),
        proposals=tuple({"name": p.name, "members": tuple(p.members), "kind": p.kind,
                         "stage": p.stage, "results": tuple(tuple(r) for r in p.results),
                         "group": p.group.name if p.group is not None else None}
                        for p in ws.proposals),
        correspondences=tuple(correspondence_of(world, cp) for cp in ws.correspondences),
        saliences=tuple(S.saliences(world)),
        worldview=worldview_of(world),
        competitions=tuple({"newcomer": c.newcomer.name, "rivals": tuple(r.name for r in c.rivals),
                            "old_rating": c.old_rating, "rival_rating": c.rival_rating,
                            "chance": c.chance, "rival_wins": c.rival_wins, "a": c.exponent,
                            "clock": c.clock} for c in ws.competitions),
        changes=tuple({"added": tuple(c.added), "removed": tuple(c.removed), "rating": c.rating,
                       "coherent": c.coherent, "reason": c.reason, "clock": c.clock}
                      for c in ws.changes),
        context=(None if world.context is None else
                 {"correspondence": world.context.correspondence,
                  "reset": tuple(world.context.reset), "pushed_at": world.context.pushed_at}),
        coderack=coderack_of(world), temperature=temperature_of(world),
        roles=roles_of(world), recast=recast_of(world), axis=axis_of(world))


def answer_now(world) -> dict:
    """The answer as the Worldview stands at this moment (D-057), drawing nothing."""
    a = AN.answer_for(world)
    return {"object": a.object, "label": a.label, "literal": a.literal, "through": a.through,
            "via": a.via, "note": a.note}

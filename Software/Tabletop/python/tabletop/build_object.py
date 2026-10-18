"""Build-object: a bottom-up group proposed inside a gestalt group and taken through three
graded stages, one codelet each (D-024, with D-025, D-026 and D-069 at the last stage).

The codelet takes two objects, "build-object (fork1, fork2)" [F95 p. 77], and groups grow by
"progressively adding adjacent objects to already-existing groups" [F95 p. 140]. Building a
group "involves an escalating series of 'micro-tests' that check the physical and conceptual
distances between prospective group members, as well as the compatibility of the prospective
group with other already-established groups" [FCCA p. 384]; "If any such test fails, the
potential group is aborted" [FCCA p. 384]; "If all these prerequisite hurdles are cleared,
the group gets officially created by a specific codelet." [FCCA p. 384] The three stages
follow the Coderack strip's labels build-obj-0, -1 and -2 [F95 p. 96 (img)] in FCCA's order
(D-024). The result is "a much more solid structure in which each of the elements knows
about its neighbors" [F95 p. 125].
"""

from dataclasses import dataclass, field

from tabletop import events as EV
from tabletop import groups as GR
from tabletop import temperature as TP

STAGES = ("build-obj-0", "build-obj-1", "build-obj-2")  # the strip's labels [F95 p. 96 (img)]


@dataclass(eq=False)
class Proposal:
    """A bottom-up group on its way through the stages (D-024), compared by identity:
    `members` in file order; `kind` and `node` from `groups.kind_of`; `parent`, the gestalt
    group it lies in; `base`, the existing bottom-up group being extended by one member, or
    None for a pair; `new_links`, the (member, new member) pairs whose physical closeness
    stage 0 tests; `stage`, the next stage to run (0, 1, 2; 3 when built); `results`, the
    (stage, chance, passed) records; `group`, the group built by stage 2."""

    members: list
    kind: str
    node: str | None
    parent: GR.Group
    base: GR.Group | None = None
    new_links: list = field(default_factory=list)
    stage: int = 0
    results: list = field(default_factory=list)
    group: GR.Group | None = None
    name: str = ""

    def done(self) -> bool:
        """Whether the proposal is finished, built or aborted (D-024)."""
        return self.stage >= len(STAGES) or (self.results and not self.results[-1][2])


# ---------------------------------------------------------------------------------------------
# What may be proposed (D-024, D-027).


def candidate_runs(world, parent) -> list:
    """The runs of adjacent members a build-object may propose inside a gestalt group
    (D-024): every pair of neighbours in the parent's chain (D-027), then every bottom-up
    group inside the parent plus one member adjacent to it, as (members in file order,
    base group or None) pairs, in a fixed order (members in member order, groups in the
    order built). Nothing is known before find-neighbor has run on the parent."""
    runs = []
    for a in parent.members:
        for b, _ in parent.neighbours_of(a):
            if parent.members.index(a) < parent.members.index(b):
                runs.append(([a, b], None))
    for g in world.workspace.groups:
        if not g.is_bottom_up() or g.parent is not parent:
            continue
        for m in g.members:
            for n, _ in parent.neighbours_of(m):
                if n in g.members:
                    continue
                members = [x for x in parent.members if x in g.members or x == n]
                if not any(r == members for r, _ in runs):
                    runs.append((members, g))
    return runs


def propose(world, parent, members, base=None, by: str = "build-object") -> Proposal | None:
    """Propose a bottom-up group over these members of a gestalt group (D-024). Nothing is
    proposed (None) when a bottom-up group with exactly these members exists, or when, under the fix
    of D-026, a sameness run already lies inside a sameness group of its type; under that
    fix a sameness run overlapping such a group is proposed as their union
    (`groups.merged_with_overlapping`). The kind is read off the members' types
    (`groups.kind_of`); the links stage 0 will test are the pair itself, or the new member
    and the member of the base it is adjacent to."""
    ws = world.workspace
    if ws.group_with_members(members, bottom_up=True) is not None:
        return None
    kind, node = GR.kind_of(world.problem, members, world.config)
    if GR.covered_by_sameness_group(world, members, kind):
        return None
    if base is None:
        links = [(members[0], members[1])]
    else:
        new = [m for m in members if m not in base.members]
        links = [(m, n) for n in new for m, _ in parent.neighbours_of(n) if m in base.members]
    members = GR.merged_with_overlapping(world, members, kind, parent)
    if ws.group_with_members(members, bottom_up=True) is not None:
        return None
    return Proposal(members=members, kind=kind, node=node, parent=parent, base=base,
                    new_links=links)


# ---------------------------------------------------------------------------------------------
# The three stages (D-024).


def stage_physical(world, proposal) -> bool:
    """build-obj-0, the physical micro-test [FCCA p. 384] (D-024): the proximity coin of
    D-022 between each new member and the member it joins; a perceptual coin, not
    softened by temperature (D-060). Records the result; True if every link holds."""
    problem = world.problem
    passed = all(GR.linked(problem.object(a), problem.object(b), world.rng, world.config)
                 for a, b in proposal.new_links)
    _record(world, proposal, 1.0 if passed else 0.0, passed)
    return passed


def stage_chance(world, proposal, salience_of=None) -> float:
    """The chance q of the conceptual micro-test by the group's kind, `group_stage_pass`
    (D-024: 0.95 sameness, 0.6 shared-ISA, 0.1 mixed), plus, under `french-faults`, the
    members' mean salience share of D-069 as a fraction of 100, capped at 1."""
    q = dict(world.config.group_stage_pass)[proposal.kind]
    q += GR.member_salience_term(world, proposal.members, salience_of) / 100.0
    return min(1.0, q)


def stage_conceptual(world, proposal, exponent: float, salience_of=None) -> bool:
    """build-obj-1, the conceptual micro-test [FCCA p. 384] (D-024): one coin at the
    stage's chance under the law of D-060 with the exponent a(T) the codelet passes, so
    that sameness groups form "almost always", shared-ISA groups "usually" and the rest
    "only very rarely" [F95 p. 65], and more willingly at high temperature [F95 p. 111]."""
    q = stage_chance(world, proposal, salience_of)
    passed = TP.passes(q, exponent, world.rng)
    _record(world, proposal, q, passed)
    return passed


def stage_compatibility(world, proposal, exponent: float, salience_of=None) -> GR.Group | None:
    """build-obj-2, the compatibility micro-test and the building [FCCA p. 384] (D-024):
    the group is created inside its parent with the parent's neighbour relations among its
    members (so that "each of the elements knows about its neighbors" [F95 p. 125]), its
    strength computed (D-025), "group" and its kind's node jolted (D-016); then it fights
    every incompatible group (D-025) and, if it survives, absorbs the smaller sameness
    groups of its type inside it (D-026). Returns the group, or None when it lost."""
    config, ws = world.config, world.workspace
    group = GR.Group(members=list(proposal.members), kind=proposal.kind, parent=proposal.parent,
                     category=proposal.node if proposal.kind == GR.SHARED_ISA else None)
    inherit_chain(proposal.parent, group)
    ws.add_group(group)
    GR.update_strength(world, group, salience_of)
    GR.announce(world, group)
    world.slipnet.jolt("group", config.jolt_structure)
    if proposal.node is not None:
        world.slipnet.jolt(proposal.node, config.jolt_description)
    lost = any(g is group for g, _ in GR.resolve_conflicts(world, group, exponent, salience_of))
    _record(world, proposal, 1.0, not lost)
    if lost:
        return None
    GR.absorb_subgroups(world, group)
    proposal.group = group
    return group


def inherit_chain(parent, group) -> None:
    """Give a bottom-up group the neighbour relations its parent found among its members,
    and mark them visited, so that its chain is known and its ends can be read off it
    (D-024, D-028)."""
    for m in group.members:
        relations = [(n, side) for n, side in parent.neighbours_of(m) if n in group.members]
        if relations:
            group.neighbours[m] = relations
        group.visited.append(m)


def run_stage(world, proposal, exponent: float, salience_of=None) -> bool:
    """Run the proposal's next stage, what one build-obj-N codelet does (D-024); each stage
    that passes posts the next "one generation on" (the codelet's business). Returns
    whether the stage passed; after stage 2 the group is on `proposal.group`."""
    if proposal.done():
        return False
    stage = proposal.stage
    if stage == 0:
        passed = stage_physical(world, proposal)
    elif stage == 1:
        passed = stage_conceptual(world, proposal, exponent, salience_of)
    else:
        passed = stage_compatibility(world, proposal, exponent, salience_of) is not None
    if passed:
        proposal.stage += 1
    return passed


def build_object(world, proposal, exponent: float, salience_of=None) -> GR.Group | None:
    """Take a proposal through all its remaining stages at once (D-024), as the three
    codelets would if nothing ran between them. Returns the group built, or None."""
    while not proposal.done():
        if not run_stage(world, proposal, exponent, salience_of):
            return None
    return proposal.group


def _record(world, proposal, chance: float, passed: bool) -> None:
    proposal.results.append((proposal.stage, chance, passed))
    world.workspace.events.record(EV.GroupStagePassed, group=proposal.name, stage=proposal.stage,
                                  chance=chance, passed=passed)

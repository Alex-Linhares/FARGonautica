"""The Workspace: the objects in view, their descriptions, the groups and (from the item
that builds them) the correspondences, with the registry that names every structure
(D-023, D-027 to D-029), and the role pool, the role tags standing as structures of the
run (D-079).

The Workspace is where "codelets build descriptions, groups and correspondences" out of the
physical situation the problem file records; "It is not aware of the other objects, their
positions on the table, their groupings, and so on. Such things will be discovered during
the run of the program." [F95 p. 64] An object enters the Workspace when a codelet first
looks at it (`notice`), with the region it was seen in, drawn through the blur of D-004;
a codelet's first action is to notice an aspect of the table and attach "an appropriate
descriptor to the object concerned" [F95 p. 75] (`describe`). Groups and correspondences
are compared by identity, never by value, and are kept in lists in the order built, so
that nothing depends on hashing. Structure names (`g1`, `cp2`) are the trace's
(`docs/trace-format.md`).
"""

from tabletop import events as EV
from tabletop import geometry as G
from tabletop.config import Config
from tabletop.groups import Group
from tabletop.problems import Problem

# The two Worldview roles the pool cascades between (D-089, D-119): `roles.BARRIER` and
# `roles.CLAIMED`, named here so that the pool does not import the registry.
BARRIER, CLAIMED = "BARRIER", "CLAIMED"


class Workspace:
    """The structures of one run (D-023, D-029): `noticed`, the ids of the objects looked
    at so far in the order of their first look; `regions`, each noticed object's last drawn
    region name (D-004); `isolated`, the objects a find-group found no neighbour for
    (D-022); `descriptions`, item name to its descriptors in the order attached;
    `groups`, the groups built, by identity, in the order built; `proposals`, the bottom-up
    groups proposed (`build_object.Proposal`, named `p1`, `p2`, ...; D-024); `correspondences`, likewise
    (`correspondences.Correspondence`, D-035); `wins` and `losses`, item name to the competitions
    its correspondences have won and lost, for salience (D-031, D-038); `competitions`, every
    fight held (`worldview.Competition`) and `changes`, every change of the Worldview
    (`worldview.Change`), both in order, for the temperature's window of ten (D-040,
    D-048); `awarded`, the (correspondence name, won, rivalry) triples a bonus or a penalty
    has been awarded for, in order, so that a rematch of one rivalry awards nothing
    (D-117); `roles`, the role pool: every `roles.RoleTag` standing, in the order tagged
    (D-079), named `r1`, `r2`, ... and removed with the structure it stands on; `recast`,
    the last re-cast of the touched object's signature published (D-093), None until the
    recast-role codelet publishes one. Nothing
    here draws random numbers except `notice`, which takes the run's
    generator for the region blur. `events` is the run's publisher (D-065), which the
    `World` sets; a Workspace built on its own publishes to no one."""

    def __init__(self, problem: Problem, config: Config):
        self.problem = problem
        self.config = config
        self.noticed: list[str] = []
        self.regions: dict[str, str] = {}
        self.isolated: list[str] = []
        self.descriptions: dict[str, list[str]] = {}
        self.groups: list[Group] = []
        self.correspondences: list = []
        self.wins: dict[str, int] = {}
        self.losses: dict[str, int] = {}
        self.competitions: list = []
        self.changes: list = []
        self.awarded: list = []     # (name, won, rivalry) awarded once each (D-117)
        self.proposals: list = []
        self.roles: list = []
        self._roles_by_item: dict = {}   # the tags of each item, an index of `roles` (D-112)
        self._roles_indexed = 0          # how many tags the index holds
        self.recast = None          # the last re-cast published (`roles.Recast`, D-093)
        self._counts = {"g": 0, "cp": 0, "p": 0, "r": 0}
        self.events = EV.Publisher()

    # ---- objects in view ------------------------------------------------------------------

    def notice(self, id_: str, rng, by: str = "", region: G.Region | None = None) -> G.Region:
        """Look at an object: it becomes known, in the region it is seen in, "an object that
        is actually in one region but very close to a region boundary may sometimes be
        considered by the program to be in the adjacent region" [F95 p. 65] (D-004). The
        region is drawn through the run's generator unless the caller has drawn it already
        (`perceive.look` draws one region per object and passes it). The first look adds
        the object to `noticed` and its type to its descriptions; every look records the
        region. Returns the region."""
        obj = self.problem.object(id_)
        if region is None:
            region = G.drawn_region(obj.x, obj.y, rng, self.config)
        if id_ not in self.noticed:
            self.noticed.append(id_)
            self.regions[id_] = region.name
            self.events.record(EV.ObjectNoticed, object=id_, type=obj.type, region=region.name,
                               by=by or self.events.actor)
            self.describe(id_, obj.type)
        self.regions[id_] = region.name
        return region

    def is_noticed(self, id_: str) -> bool:
        """Whether a codelet has looked at this object yet [F95 p. 64]."""
        return id_ in self.noticed

    def mark_isolated(self, id_: str) -> bool:
        """Record that a find-group found this object alone, with nothing near enough to
        group with (D-022; the "isolated" node, D-007). Returns True the first time."""
        if id_ in self.isolated:
            return False
        self.isolated.append(id_)
        self.describe(id_, "isolated")
        return True

    # ---- descriptions ---------------------------------------------------------------------

    def describe(self, item: str, descriptor: str) -> bool:
        """Attach a descriptor (a type, `end`, `middle`, `left`, `horizontal`, `two`, ...)
        to an item, an object id or a group name, "an appropriate descriptor to the object
        concerned" [F95 p. 75] (D-028, D-029). Returns True if it is new."""
        descriptors = self.descriptions.setdefault(item, [])
        if descriptor in descriptors:
            return False
        descriptors.append(descriptor)
        self.events.record(EV.DescriptionAdded, item=item, descriptor=descriptor,
                           by=self.events.actor)
        return True

    def descriptors(self, item: str) -> list[str]:
        """The descriptors of an item in the order attached (a copy); "The salience of an
        object thus varies according to how well developed its description is." [F95
        p. 67]"""
        return list(self.descriptions.get(item, []))

    def has(self, item: str, descriptor: str) -> bool:
        """Whether the item carries this descriptor (D-029)."""
        return descriptor in self.descriptions.get(item, [])

    # ---- groups ---------------------------------------------------------------------------

    def add_group(self, group: Group) -> str:
        """Register a group under the next name (`g1`, `g2`, ...; `docs/trace-format.md`)
        and keep it in the order built (D-023). Returns the name."""
        self._counts["g"] += 1
        group.name = f"g{self._counts['g']}"
        self.groups.append(group)
        return group.name

    def remove_group(self, group: Group, reason: str = "removed") -> None:
        """Drop a group (`absorbed` or `lost`, D-025, D-026) with its descriptions; its name
        is never reused."""
        self.groups = [g for g in self.groups if g is not group]
        self.descriptions.pop(group.name, None)
        self.events.record(EV.GroupRemoved, group=group.name, reason=reason)
        self.discard_roles_on(group.name)

    def group_named(self, name: str) -> Group:
        """The group with this name (`g1`, `g2`, ...; D-023)."""
        for g in self.groups:
            if g.name == name:
                return g
        raise KeyError(name)

    def has_group(self, name: str) -> bool:
        """Whether a group of this name is still in the Workspace (D-025: a codelet holding
        the name of a group that lost a fight or was absorbed fizzles)."""
        return any(g.name == name for g in self.groups)

    # ---- proposals (D-024) ----------------------------------------------------------------

    def add_proposal(self, proposal) -> str:
        """Register a bottom-up group proposal (`build_object.Proposal`) under the next name
        (`p1`, `p2`, ...), so that the three build-obj codelets that carry it through its
        stages can name it in their arguments (D-024). Returns the name."""
        self._counts["p"] += 1
        proposal.name = f"p{self._counts['p']}"
        self.proposals.append(proposal)
        self.events.record(EV.GroupProposed, group=proposal.name, members=tuple(proposal.members),
                           kind=proposal.kind, category=proposal.node or "",
                           base=proposal.base.name if proposal.base is not None else "",
                           by=self.events.actor)
        return proposal.name

    def proposal_named(self, name: str):
        """The proposal with this name, or None when there is none (D-024)."""
        for p in self.proposals:
            if p.name == name:
                return p
        return None

    def group_with_members(self, members, bottom_up: bool | None = None) -> Group | None:
        """The group whose members are exactly these ids, if one exists (D-023: a component
        "not already in the Workspace"); members are compared as sets of ids. With
        `bottom_up` True only the bottom-up groups are searched, with False only the gestalt
        ones (D-024: a gestalt group and a bottom-up group over the same objects are two
        structures, "a much more solid structure" [F95 p. 125] being the second)."""
        wanted = sorted(members)
        for g in self.groups:
            if bottom_up is not None and g.is_bottom_up() != bottom_up:
                continue
            if sorted(g.members) == wanted:
                return g
        return None

    def bottom_up_groups(self) -> list[Group]:
        """The bottom-up groups, in the order built (a copy; D-024)."""
        return [g for g in self.groups if g.is_bottom_up()]

    def groups_containing(self, id_: str) -> list[Group]:
        """The groups this object belongs to, in the order built (a copy; D-026: an object
        may be in a group and in its subgroup)."""
        return [g for g in self.groups if id_ in g.members]

    def innermost_group(self, id_: str) -> Group | None:
        """The smallest group this object belongs to, the one a within-group description
        refers to (D-029); the earliest built among equals; None when it is in no group,
        and then it has no neighbour: "in order for an object to have a neighbor, it must
        be in a group" [F95 p. 76]."""
        best = None
        for g in self.groups:
            if id_ in g.members and (best is None or len(g.members) < len(best.members)):
                best = g
        return best

    # ---- correspondences (D-035) ----------------------------------------------------------

    def add_correspondence(self, cp) -> str:
        """Register a correspondence under the next name (`cp1`, `cp2`, ...;
        `docs/trace-format.md`) and keep it in the order built (D-035). Returns the name."""
        self._counts["cp"] += 1
        cp.name = f"cp{self._counts['cp']}"
        self.correspondences.append(cp)
        return cp.name

    def remove_correspondence(self, cp) -> None:
        """Drop a correspondence (removed below the threshold, D-041) with its
        descriptions; its name is never reused."""
        self.correspondences = [c for c in self.correspondences if c is not cp]
        self.descriptions.pop(cp.name, None)
        self.discard_roles_on(cp.name)

    def correspondence_named(self, name: str):
        """The correspondence with this name (`cp1`, `cp2`, ...; D-035)."""
        for c in self.correspondences:
            if c.name == name:
                return c
        raise KeyError(name)

    def correspondence_between(self, a: str, b: str):
        """The correspondence whose two ends are these items, in either order, or None
        (D-035: one correspondence per pair of items)."""
        for c in self.correspondences:
            if (c.henry, c.eliza) in ((a, b), (b, a)):
                return c
        return None

    def correspondences_of(self, item: str) -> list:
        """The correspondences with this item at an end, in the order built (a copy; D-044:
        "Rival correspondences start or finish at the same end point" [F95 p. 101])."""
        return [c for c in self.correspondences if item in c.ends]

    def worldview(self) -> list:
        """The correspondences in the Worldview, in the order built (a copy; D-040): "the
        sole, current interpretation of the table" [F95 p. 73], the correspondences whose
        `in_worldview` flag `worldview.compete` has set."""
        return [c for c in self.correspondences if c.in_worldview]

    # ---- the role pool (D-079) ------------------------------------------------------------

    def tag_role(self, tag) -> str:
        """Register a role tag (`roles.RoleTag`) under the next name (`r1`, `r2`, ...) and
        keep it in the order tagged (D-079); publishes `RoleTagged`. Returns the name. The
        fight with its rivals is `roles.propose`'s, not the pool's."""
        self._counts["r"] += 1
        tag.name = f"r{self._counts['r']}"
        self.roles.append(tag)
        self._roles_by_item.setdefault(tag.item, []).append(tag)
        self._roles_indexed += 1
        self.events.record(EV.RoleTagged, tag=tag.name, item=tag.item, role=tag.role,
                           args=tuple(tag.args), weight=tag.weight, strength=tag.strength,
                           evidence=tag.evidence, by=tag.by or self.events.actor)
        return tag.name

    def discard_role(self, tag, reason: str, by: str = "") -> None:
        """Drop a role tag with the `reason` (`lost to rN`, `broken`, `structure gone`;
        D-079); publishes `RoleDiscarded`; its name is never reused. A BARRIER tag takes
        with it, for the same reason, every CLAIMED tag that names it (the claimed object's
        tag whose arguments are the barrier and the correspondence, D-089), since a claim
        stands only while its barrier does (D-119, v5): the cascade `discard_roles_on`
        already makes when the correspondence itself leaves the Workspace."""
        if not any(t is tag for t in self.roles):
            return
        self.roles = [t for t in self.roles if t is not tag]
        self._roles_by_item[tag.item] = [t for t in self._roles_by_item.get(tag.item, ())
                                         if t is not tag]
        self._roles_indexed -= 1
        self.events.record(EV.RoleDiscarded, tag=tag.name, item=tag.item, role=tag.role,
                           reason=reason, by=by or self.events.actor)
        if tag.role == BARRIER:
            for claim in [t for t in self.roles if t.role == CLAIMED and len(t.args) >= 2
                          and t.args[0] == tag.item and t.args[1] == tag.args[1]]:
                self.discard_role(claim, reason, by)

    def discard_roles_on(self, name: str) -> None:
        """Drop every tag on a structure that has left the Workspace, or naming it in its
        arguments (D-079: tags are removed with their structure)."""
        for tag in [t for t in self.roles if t.item == name or name in t.args]:
            self.discard_role(tag, "structure gone")

    def _tags_on(self, item: str) -> list:
        """The tags on an item from the index, in the order tagged (D-112). `tag_role` and
        `discard_role` keep the index; a hand edit of `roles` (a test's) that changes its
        length rebuilds it here."""
        if len(self.roles) != self._roles_indexed:
            self._roles_by_item = {}
            for t in self.roles:
                self._roles_by_item.setdefault(t.item, []).append(t)
            self._roles_indexed = len(self.roles)
        return self._roles_by_item.get(item, ())

    def roles_of(self, item: str) -> list:
        """The tags on an item, in the order tagged (a copy; D-079)."""
        return list(self._tags_on(item))

    def items_with(self, role: str) -> list:
        """The items tagged with a role, in the order tagged, each once (D-079)."""
        out = []
        for t in self.roles:
            if t.role == role and t.item not in out:
                out.append(t.item)
        return out

    def role_tag(self, item: str, role: str, args=()):
        """The tag with this item, role and arguments, or None (D-079: one tag per
        reading)."""
        for t in self.roles:
            if t.same_as(item, role, args):
                return t
        return None

    def has_role(self, item: str, role: str) -> bool:
        """Whether the item carries a tag of this role, with any arguments (D-079)."""
        for tag in self._tags_on(item):
            if tag.role == role:
                return True
        return False

    # ---- competitions, as salience reads them (D-031) -------------------------------------

    def in_worldview(self, item: str) -> bool:
        """Whether the item "is part of a correspondence in the Worldview" [F95 p. 66]: a
        correspondence of `correspondences` with `in_worldview` set and the item among its
        `ends` (`correspondences.Correspondence`; D-031, factor 6). False at the start,
        when "this factor contributes zero to all calculations of salience" [F95 p. 67]."""
        for cp in self.correspondences:
            if cp.in_worldview and item in cp.ends:
                return True
        return False

    def record_competition(self, items, won: bool) -> None:
        """Count a competition for the items at the ends of a correspondence that took part:
        "Each time a correspondence competes to become part of the Worldview and wins, bonus
        points are awarded to the two objects making up the correspondence. Objects in losing
        correspondences lose points." [F95 p. 66] (D-031, factor 7; D-038)."""
        tally = self.wins if won else self.losses
        for item in items:
            tally[item] = tally.get(item, 0) + 1

    def competitions_of(self, item: str) -> tuple[int, int]:
        """(wins, losses) recorded for an item (D-031, factor 7); (0, 0) at the start."""
        return self.wins.get(item, 0), self.losses.get(item, 0)

    def items(self) -> list[str]:
        """Every item a codelet can pick: the noticed objects in the order noticed, then
        the groups in the order built, by name (D-031's candidates)."""
        return list(self.noticed) + [g.name for g in self.groups]

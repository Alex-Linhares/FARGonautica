"""The searches (D-052's band of 30): search-for-sam-gp and search-for-sim-gp, posted in
bulk when a group is built, "Once any group is built, the program devotes resources to
looking for identical or similar groups in other areas of the table." [FCCA p. 386]

The chapter 4 run shows them: "a large number of search-for-same-group codelets were posted
to the Coderack (shown by the tall bar in the middle, with '70' above it)" [F95 p. 101], and
a spoon-spoon group found on each side leads to "a correspondence between the two
spoon-spoon subgroups" [F95 p. 101]. A search looks among the groups the Workspace holds;
finding the other side's groups is the scouts' work (D-023). The band's third codelet is
the re-interpretation's recast-role (D-081, D-093), which searches the other row for the
touched object's counterpart.
"""

from tabletop import correspondences as C
from tabletop import events as EV
from tabletop import roles as R
from tabletop import salience as S
from tabletop.codelets.common import FOUND, PROPOSED, Outcome, argument, child, fizzle, group_of


def same_groups(world, group) -> list:
    """Groups of exactly the same composition elsewhere: the same types, no shared member,
    a correspondence possible (D-052; "that exact configuration of objects" [F95 p. 76])."""
    types = sorted(world.problem.object(m).type for m in group.members)
    return [g.name for g in world.workspace.groups if g is not group
            and sorted(world.problem.object(m).type for m in g.members) == types
            and not any(m in group.members for m in g.members)
            and C.creatable(world, group.name, g.name)[0]]


def similar_groups(world, group) -> list:
    """Groups of the same kind and size elsewhere, sharing no member, with which a
    correspondence is possible (D-052: search-for-sim-gp, inferred from the legend [F95
    p. 96 (img)])."""
    return [g.name for g in world.workspace.groups if g is not group
            and g.kind == group.kind and g.size() == group.size()
            and not any(m in group.members for m in g.members)
            and C.creatable(world, group.name, g.name)[0]]


def search(world, codelet, a: float, finder, word: str) -> Outcome:
    """The common body: the argument's group, the candidates `finder` gives, one chosen by
    salience (D-034), and a create-cp posted for the pair (D-052)."""
    name = argument(world, codelet, a, [g.name for g in world.workspace.groups])
    if name is None:
        return fizzle("no group yet")
    group = group_of(world, name)
    if group is None:
        return fizzle(f"{name} gone")
    found = finder(world, group)
    if not found:
        return fizzle(f"no {word} group to {group.name}")
    other = S.choose(world, a, found)
    posted = [child(world, codelet, "create-cp", (group.name, other))]
    return Outcome(FOUND, f"{other} {word} to {group.name}", posted)


def search_for_sam_gp(world, codelet, a: float) -> Outcome:
    """search-for-sam-gp (D-052): the search-for-same-group codelets of the chapter 4 run
    [F95 p. 101], looking for a group of the same composition elsewhere on the table; a
    find posts a create-cp."""
    return search(world, codelet, a, same_groups, "same")


def search_for_sim_gp(world, codelet, a: float) -> Outcome:
    """search-for-sim-gp (D-051, inferred from the legend [F95 p. 96 (img)]; D-052): a
    group of the same kind and size elsewhere; a find posts a create-cp."""
    return search(world, codelet, a, similar_groups, "similar")


# ---------------------------------------------------------------------------------------------
# The re-cast: recast-role (D-081, D-093; role K1 COUNTERPART).


def recast_role(world, codelet, a: float) -> Outcome:
    """recast-role (D-081, D-093): carry the touched object's role signature to the other
    row through the one re-cast (`roles.recast`), "the cup in front of Henry" into the
    schema "the cup in front of person X." [F95 p. 6]; publish it (`RoleRecast`, kept on the
    Workspace), tag every candidate COUNTERPART with its tier in `args` (a candidate whose
    tier changed since the last re-cast loses its old tag, `broken`), record the
    consumption of the touched object's ROW and COLUMN tags (consumer C2 reads them
    through the re-cast, D-085), and post one create-cp for the touched object and each
    tier-1 candidate a correspondence may be created for (consumer C1's part, D-095) with
    one check-roles. Fizzles, drawing nothing, while the touched object's row or the other
    row is unread: nothing is gated. Draws nothing itself."""
    ws = world.workspace
    touched = world.problem.touched
    rc = R.recast(world)
    if rc is None:
        return fizzle(f"the rows of {touched} and its counterparts are not read")
    ws.recast = rc
    world.events.record(EV.RoleRecast, touched=touched, signature=tuple(rc.signature),
                        tiers=tuple(rc.tiers), ambiguous=rc.ambiguous, by=codelet.type)
    for tag in [t for t in ws.roles_of(touched) if t.role in (R.ROW, R.COLUMN)]:
        R.consumed(world, tag, R.C2, f"re-cast of {touched}", codelet.type)
    current = {c: R.TIER_WORDS[t] for t, c, _ in rc.tiers}
    for tag in [t for t in ws.roles if t.role == R.COUNTERPART
                and current.get(t.item) != t.args[1]]:
        ws.discard_role(tag, R.BROKEN, codelet.type)
    tags, posted = [], []
    for tier, candidate, weight in rc.tiers:
        tag = R.propose(world, candidate, R.COUNTERPART, (touched, R.TIER_WORDS[tier]), 1,
                        R.TIER_EVIDENCE[tier], codelet.type, a)
        if tag is not None:
            tags.append(tag)
            if tier == 1 and C.creatable(world, touched, candidate)[0]:
                R.consumed(world, tag, R.C1, f"create-cp {touched} {candidate}", codelet.type)
                posted.append(child(world, codelet, "create-cp", (touched, candidate)))
    if posted:
        posted.append(child(world, codelet, "check-roles", ()))
    tiers = " ".join(f"{t}:{c}" for t, c, _ in rc.tiers)
    word = " (tie)" if rc.ambiguous else ""
    return Outcome(PROPOSED if posted else FOUND,
                   f"{touched} [{' '.join(rc.signature)}] tiers {tiers}{word}", posted)

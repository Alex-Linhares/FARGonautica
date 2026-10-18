"""The Worldview's codelets (D-052's band of 50): pick-strong-cp, examine-cp, cp-compete,
check-worldview and clean-up-corresp.

"When a rival correspondence is proposed, the existing one and the upstart must 'fight'"
[FCCA p. 382]; the fight is `worldview.compete` (D-040). When a group-level correspondence
is built, "a number of examine-cp(cp1) codelets were posted to the Coderack" [F95 p. 85],
each looking for a correspondence between members of the two groups [F95 p. 108]; the
context push and pop around it are `context.py`'s (D-021, D-070): the run loop pushes the
context a codelet carries before it runs, so examine-cp and the create-cp, build-cp-descrip
and cp-compete it spawns inside a group correspondence name it in their arguments. A faded
correspondence "is
(probabilistically) removed from the Workspace" [F95 p. 73] (D-041).
"""

from tabletop import answer as AN
from tabletop import correspondences as C
from tabletop import roles as R
from tabletop import salience as S
from tabletop import temperature as TP
from tabletop import worldview as WV
from tabletop.codelets.common import (ENTERED, FAILED, FIGHT, FOUND, PASSED, PROPOSED, REMOVED,
                                      Outcome, child, children, context_args, correspondence_of,
                                      fizzle)
from tabletop.temperature import softened_weights


def pick_strong_cp(world, codelet, a: float) -> Outcome:
    """pick-strong-cp, the text's pick-salient-correspondence (D-034, D-039, D-052): a
    correspondence outside the Worldview chosen by its rating under a(T) [F95 p. 65; F95
    p. 93]; one that has passed its test gets a cp-compete, one that has not is tested now
    and gets a cp-compete on a pass; the cp-compete carries the correspondence's context
    (D-021). With an argument, a barrier's correspondence (consumer C4, D-101), the
    candidates are its challengers (`challengers_of`): the correspondences outside the
    Worldview that conflict with it while it stands, or itself once it has lost its
    place, "with the potential to compete again" [F95 p. 72]."""
    if codelet.args:
        named = correspondence_of(world, codelet.args[0])
        if named is None:
            return fizzle(f"{codelet.args[0]} gone")
        candidates = challengers_of(world, named)
        if not candidates:
            return fizzle(f"no challenger of {named.name}")
    else:
        candidates = [cp for cp in world.workspace.correspondences if not cp.in_worldview]
    if not candidates:
        return fizzle("no correspondence outside the Worldview")
    weights = softened_weights([max(1.0, C.evaluate(world, cp)) for cp in candidates], a)
    cp = candidates[world.rng.weighted_index(weights)]
    if cp.passed:
        posted = [child(world, codelet, "cp-compete", context_args(cp))]
        return Outcome(PASSED, f"{cp.label()} rating {cp.rating:.0f}", posted)
    if C.test(world, cp, a):
        posted = [child(world, codelet, "cp-compete", context_args(cp))]
        return Outcome(PASSED, f"{cp.label()} p={cp.chance:.2f}", posted)
    return Outcome(FAILED, f"{cp.label()} p={cp.chance:.2f}")


def challengers_of(world, cp) -> list:
    """The correspondences a named one must keep fighting (D-101): outside the Worldview
    and in conflict with it (`worldview.conflicts`) while it stands in the Worldview; the
    correspondence itself when it stands outside. In the order built."""
    if cp.in_worldview:
        return [other for other in world.workspace.correspondences
                if not other.in_worldview and WV.conflicts(world, other, cp)]
    return [cp]


def anchor_pair(world, cp, pairs: list) -> tuple | None:
    """The anchor-to-anchor pair inside a group correspondence (consumer C3, D-097): the
    one member of the first group tagged ANCHOR (D-087) and the member of the other group
    `answer.role_member` matches to it, when that member is an anchor too and the pair is
    among those creatable; None otherwise, and nothing is drawn."""
    ws = world.workspace
    anchors = [m for m in C.members_of(world, cp.henry) if ws.has_role(m, R.ANCHOR)]
    if len(anchors) != 1:
        return None
    x = anchors[0]
    y = AN.role_member(world, cp, x)
    if y is None or not ws.has_role(y, R.ANCHOR) or (x, y) not in pairs:
        return None
    return x, y


def examine_cp(world, codelet, a: float) -> Outcome:
    """examine-cp (D-035, D-052): takes a group-level correspondence and looks for
    correspondences between the objects inside its two groups [F95 p. 85]: the anchor to
    the anchor first, when each group has one and the pair is still open (consumer C3,
    D-097, no draw), else one member of the first group by salience among those with a
    partner and one partner by salience (D-034); a create-cp inside the correspondence is
    posted for the pair [F95 p. 108]."""
    cp = correspondence_of(world, codelet.args[0])
    if cp is None:
        return fizzle(f"{codelet.args[0]} gone")
    pairs = C.inner_pairs(world, cp)
    if not pairs:
        return fizzle(f"no pair left inside {cp.name}")
    found = anchor_pair(world, cp, pairs)
    if found is not None:
        x, y = found
        for item in (x, y):
            R.consumed(world, world.workspace.role_tag(item, R.ANCHOR), R.C3,
                       f"create-cp {x} {y} inside {cp.name}", codelet.type)
        posted = [child(world, codelet, "create-cp", (x, y, cp.name))]
        return Outcome(PROPOSED, f"{x}={y} inside {cp.name}, anchor to anchor", posted)
    firsts = []
    for x, _ in pairs:
        if x not in firsts:
            firsts.append(x)
    x = S.choose(world, a, firsts)
    y = S.choose(world, a, [b for m, b in pairs if m == x])
    posted = [child(world, codelet, "create-cp", (x, y, cp.name))]
    return Outcome(PROPOSED, f"{x}={y} inside {cp.name}", posted)


def cp_compete(world, codelet, a: float) -> Outcome:
    """cp-compete (D-040, D-052): the fight, or the entry without one [F95 p. 101]; then a
    clean-up-corresp and a check-worldview are posted, a build-cp-descrip for a newcomer
    that is in (D-039), and four examine-cp when a group correspondence has entered [F95
    p. 85]."""
    cp = correspondence_of(world, codelet.args[0])
    if cp is None:
        return fizzle(f"{codelet.args[0]} gone")
    if cp.in_worldview:
        return fizzle(f"{cp.name} already in the Worldview")
    held = WV.compete(world, cp, a)
    if held is None:
        return fizzle(f"{cp.name} gone")
    posted = children(world, codelet, [("clean-up-corresp", ()), ("check-worldview", ())])
    if held.rival_wins:
        # The newcomer is in: its descriptors are written (D-039, item 24), and the
        # Worldview's roles are re-read after the change (D-083, D-081).
        posted.insert(0, child(world, codelet, "build-cp-descrip", context_args(cp)))
        posted.append(child(world, codelet, "read-worldview-roles", ()))
    if held.rival_wins and WV.is_group_correspondence(world, cp):
        posted.extend(children(world, codelet, [("examine-cp", (cp.name,), 4)]))
    if not held.rivals:
        return Outcome(ENTERED, f"{cp.label()} entered the Worldview without a fight, rating "
                       f"{held.rival_rating:.0f}", posted)
    rivals = " ".join(r.name for r in held.rivals)
    who = "rival wins" if held.rival_wins else "old holds"
    return Outcome(FIGHT, f"{cp.label()} vs {rivals}: old {held.old_rating:.0f} vs rival "
                   f"{held.rival_rating:.0f}, p={held.chance:.2f}, {who}", posted)


def check_worldview(world, codelet, a: float) -> Outcome:
    """check-worldview (D-051, inferred from the legend [F95 p. 96 (img)]; D-040): re-read
    every rating and post a cp-compete for each tested correspondence that now outrates its
    rivals or has none (`worldview.challengers`); then one pick-strong-cp for each
    barrier's correspondence (`barrier_fights`, consumer C4, D-101)."""
    found = WV.challengers(world)
    posted = [child(world, codelet, "cp-compete", context_args(cp)) for cp in found]
    fights = barrier_fights(world, codelet)
    if not found and not fights:
        return fizzle("no challenger")
    words = ["challengers " + " ".join(cp.name for cp in found)] if found else []
    if fights:
        words.append("barrier fights " + " ".join(c.args[0] for c in fights))
    return Outcome(FOUND, ", ".join(words), posted + fights)


def barrier_fights(world, codelet) -> list:
    """Consumer C4's posting (D-101), from check-worldview and from a read-worldview-roles
    that read a barrier: one pick-strong-cp for the correspondence each standing BARRIER
    tag names (D-089), each correspondence once, so that the claim on the touched
    object's obvious counterpart keeps being contested, "strong enough to make Eliza
    very reluctant to break it" [F95 p. 127] or not; the tag is recorded as consumed. A
    pool with no barrier posts nothing: nothing is gated. Draws nothing."""
    posted, named = [], []
    for tag in [t for t in world.workspace.roles if t.role == R.BARRIER]:
        cp = correspondence_of(world, tag.args[1])
        if cp is None or cp.name in named:
            continue
        named.append(cp.name)
        R.consumed(world, tag, R.C4, f"pick-strong-cp {cp.name}", codelet.type)
        posted.append(child(world, codelet, "pick-strong-cp", (cp.name,)))
    return posted


def clean_up_corresp(world, codelet, a: float) -> Outcome:
    """clean-up-corresp (D-041): remove, probabilistically, every correspondence outside
    the Worldview whose rating has dropped below the threshold [F95 p. 73]."""
    removed = WV.clean_up(world)
    if not removed:
        return fizzle("nothing below the threshold")
    what = " ".join(f"{cp.name} p={chance:.2f}" for cp, chance in removed)
    return Outcome(REMOVED, what)


# ---------------------------------------------------------------------------------------------
# The Worldview's roles: read-worldview-roles (D-081, D-083).


def read_worldview_roles(world, codelet, a: float) -> Outcome:
    """read-worldview-roles (D-081, D-083, D-094, D-089): read the roles the Worldview as
    it stands supports, at the top context only. On the Worldview item, through
    `roles.detect`: the SYMMETRY_AXIS, the placement most of its top-level
    correspondences share with the touched object's own left out
    (`worldview.symmetry_axis`), and the TYPE_KEYs, the type slippages its group
    correspondences have made (`worldview.type_keys`). Then the relational reading
    (`read_blockers`): BARRIER on the end of a correspondence that claims an obvious
    counterpart of the touched object and CLAIMED on that counterpart
    (`worldview.barrier_and_claimed`). A standing tag the reading no longer supports (an
    axis of the other kind, a key whose group correspondence left, a barrier whose
    correspondence lost its place) is discarded (`broken`); each reading is proposed
    (`roles.perceive`, `roles.propose`) and its node jolted by `jolt_structure` (D-016):
    "mirror symmetry" or "diagonal symmetry" for the axis, so that the placements that
    agree with it score more (consumer C6, D-082) and C1 proposes the images under it
    (D-095); `key` for a key. A barrier read posts consumer C4's pick-strong-cp for its
    correspondence at once (`barrier_fights`, D-101), as check-worldview does for the
    barriers standing. With nothing to read and nothing standing it fizzles, drawing
    nothing."""
    ws = world.workspace
    standing = list(ws.roles_of(R.WORLDVIEW_ITEM))
    read = R.detect(world, R.WORLDVIEW_ITEM)
    readings = [(role, tuple(args)) for role, args, _, _ in read]
    dropped = [t for t in standing if (t.role, tuple(t.args)) not in readings]
    for tag in dropped:
        ws.discard_role(tag, R.BROKEN, codelet.type)
    tags = R.perceive(world, R.WORLDVIEW_ITEM, codelet.type, a) if read else []
    for role, args in readings:
        node = WV.AXIS_NODES[args[0]] if role == R.SYMMETRY_AXIS else R.spec(role).nodes[0]
        world.slipnet.jolt(node, world.config.jolt_structure)
    blocked, dropped_blockers = read_blockers(world, codelet, a)
    dropped.extend(dropped_blockers)
    tags.extend(blocked)
    if not read and not blocked:
        if dropped:
            return Outcome(REMOVED, "dropped: " + " ".join(t.label() for t in dropped))
        return fizzle("no axis, key or barrier in the Worldview")
    posted = barrier_fights(world, codelet) if any(t.role == R.BARRIER for t in blocked) else []
    return Outcome(FOUND, " ".join(t.label() for t in tags) or "the reading lost its fight",
                   posted)


def read_blockers(world, codelet, a: float) -> tuple:
    """The relational reading of read-worldview-roles (D-089): the BARRIER and CLAIMED
    readings on the items `worldview.blocked_items` names, item by item and each
    barrier before the object it claims, through the detector
    (`worldview.barrier_and_claimed`, which reads a CLAIMED only while its barrier's tag
    stands); each reading is proposed; every standing BARRIER or CLAIMED tag the reading
    did not give is then discarded (`broken`); the `barrier` node is jolted once by
    `jolt_structure` when a barrier is read. Returns (the tags standing, the tags
    dropped). Nothing stands or is read: nothing changes, no draw."""
    ws = world.workspace
    tags, keys, barrier_read = [], [], False
    for item in WV.blocked_items(world):
        for role, args, weight, evidence in WV.barrier_and_claimed(world, item):
            keys.append((item, role, tuple(args)))
            barrier_read = barrier_read or role == R.BARRIER
            tag = R.propose(world, item, role, args, weight, evidence, codelet.type, a)
            if tag is not None:
                tags.append(tag)
    dropped = [t for t in ws.roles if t.role in (R.BARRIER, R.CLAIMED)
               and (t.item, t.role, tuple(t.args)) not in keys]
    for tag in dropped:
        ws.discard_role(tag, R.BROKEN, codelet.type)
    if barrier_read:
        world.slipnet.jolt(R.spec(R.BARRIER).nodes[0], world.config.jolt_structure)
    return tags, dropped


# ---------------------------------------------------------------------------------------------
# The role checker (D-079, D-081; the author's answer D1).


def check_roles(world, codelet, a: float) -> Outcome:
    """check-roles (D-081): sample one role tag, the weak ones first (by 101 minus the
    strength, under a(T)), re-rate it from its evidence, its node and the Worldview's
    support (`roles.rate`), and break it with one coin at `roles.break_chance` under the
    temperature law (D-060): a strong, story-supported tag is hard to break when the
    temperature is low, as French's structures are, "have no action at all" [F95 p. 73]
    being the fate of what stands on nothing. An empty pool fizzles, drawing nothing."""
    tags = list(world.workspace.roles)
    if not tags:
        return fizzle("no role tag to check")
    for tag in tags:
        R.rate(world, tag)
    weights = TP.softened_weights([101.0 - t.strength for t in tags], a)
    tag = tags[world.rng.weighted_index(weights)]
    if TP.passes(R.break_chance(world, tag), a, world.rng):
        world.workspace.discard_role(tag, R.BROKEN, codelet.type)
        return Outcome(REMOVED, f"{tag.label()} broken at strength {tag.strength:.0f}")
    return Outcome(FOUND, f"{tag.label()} stands at strength {tag.strength:.0f}")

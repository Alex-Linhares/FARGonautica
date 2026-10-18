"""The builders (D-052's band of 40): build-obj-0, -1, -2 and build-obj-desc for groups,
create-cp and build-cp-descrip for correspondences.

Building a group "involves an escalating series of 'micro-tests'" [FCCA p. 384], one
codelet each (D-024); "If all these prerequisite hurdles are cleared, the group gets
officially created by a specific codelet." [FCCA p. 384] A correspondence "will not even be
created unless certain not-too-stringent conditions are met" [F95 p. 71], then "will be
evaluated on the basis of a set of more rigorous conditions. Again, the outcome of this
evaluation is a probabilistic one" [F95 p. 72] (D-035, D-039).
"""

from tabletop import build_object as BO
from tabletop import correspondences as C
from tabletop import worldview as WV
from tabletop import describe as DS
from tabletop import salience as S
from tabletop.codelets.common import (BUILT, DESCRIBED, FAILED, PASSED, Outcome, argument, child,
                                      context_args,
                                      children, correspondence_of, fizzle, group_of, members)


def build_obj_stage(world, codelet, a: float) -> Outcome:
    """build-obj-0, build-obj-1 and build-obj-2 (D-024): run the proposal's next stage at
    a(T); a pass posts the next stage one generation on; the last stage builds the group
    and posts its follow-ups: a build-obj-desc, twelve search-for-sam-gp (the bulk posting
    the dumps show, "a large number of search-for-same-group codelets were posted" [F95
    p. 101]), two search-for-sim-gp (D-052) and one find-roles on the group (D-081, D-091:
    a sameness subgroup may be a ground, and a group echoes across the table, D-092). A
    failed stage aborts the proposal, "If any such test fails, the potential group is
    aborted" [FCCA p. 384]."""
    ws = world.workspace
    proposal = ws.proposal_named(codelet.args[0])
    if proposal is None or proposal.done():
        return fizzle(f"{codelet.args[0]} finished")
    if not ws.has_group(proposal.parent.name):
        proposal.results.append((proposal.stage, 0.0, False))
        return fizzle(f"{proposal.parent.name} gone")
    if BO.STAGES[proposal.stage] != codelet.type:
        return fizzle(f"{proposal.name} is at stage {proposal.stage}")
    passed = BO.run_stage(world, proposal, a, S.salience_of(world))
    stage, chance, _ = proposal.results[-1]
    what = f"{proposal.name} {members(world, proposal.members)} {proposal.kind}"
    if not passed:
        return Outcome(FAILED, f"{what} at stage {stage}, p={chance:.2f}")
    if proposal.group is None:
        posted = [child(world, codelet, BO.STAGES[proposal.stage], (proposal.name,))]
        return Outcome(PASSED, f"{what} stage {stage}, p={chance:.2f}", posted)
    group = proposal.group
    posted = children(world, codelet, [
        ("build-obj-desc", (group.name,)), ("search-for-sam-gp", (group.name,), 12),
        ("search-for-sim-gp", (group.name,), 2), ("find-roles", (group.name,))])
    return Outcome(BUILT, f"{group.name} {members(world, group.members)} {group.kind} "
                   f"strength {group.strength:.0f} from {proposal.name}", posted)


def build_obj_desc(world, codelet, a: float) -> Outcome:
    """build-obj-desc (D-051, inferred from the legend [F95 p. 96 (img)]; D-029): write a
    built group's descriptors: its count (one, two, three, many [F95 p. 68]), its
    orientation [F95 p. 97] and its kind."""
    name = argument(world, codelet, a, [g.name for g in world.workspace.bottom_up_groups()])
    if name is None:
        return fizzle("no group to describe")
    group = group_of(world, name)
    if group is None:
        return fizzle(f"{name} gone")
    ws = world.workspace
    count = DS.describe_count(world, group.name)
    orientation = DS.notice_orientation(world, group, by=codelet.type)
    ws.describe(group.name, group.kind)
    if group.category is not None:
        ws.describe(group.name, group.category)
    return Outcome(DESCRIBED, f"{group.name}: {count} {orientation} {group.kind}")


def create_cp(world, codelet, a: float) -> Outcome:
    """create-cp (D-035, D-039): create the correspondence between its two arguments, inside
    the group correspondence a third argument names [F95 p. 108], and run its initial test
    at once, "Once created, the correspondence will be evaluated" [F95 p. 72], at a(T): a
    pass posts a cp-compete carrying the correspondence's context (D-021); a failure removes
    the correspondence, which a later pick may propose again (`failed_test_removes`; item
    24). A pair the conditions refuse fizzles with the reason."""
    x, y = codelet.args[0], codelet.args[1]
    parent = None
    if len(codelet.args) > 2:
        parent = correspondence_of(world, codelet.args[2])
        if parent is None:
            return fizzle(f"{codelet.args[2]} gone")
    ok, reason = C.creatable(world, x, y)
    if not ok:
        return fizzle(f"{x}={y}: {reason}")
    cp = C.create(world, x, y, by=codelet.type, parent=parent)
    if cp is None:
        return fizzle(f"{x}={y}: not inside {parent.name}")
    if C.test(world, cp, a):
        posted = [child(world, codelet, "cp-compete", context_args(cp))]
        return Outcome(PASSED, f"{cp.label()} static {cp.static:.0f} p={cp.chance:.2f}", posted)
    if world.config.failed_test_removes:
        WV.remove(world, cp)
        return Outcome(FAILED, f"{cp.label()} static {cp.static:.0f} p={cp.chance:.2f}, removed")
    return Outcome(FAILED, f"{cp.label()} static {cp.static:.0f} p={cp.chance:.2f}")


def build_cp_descrip(world, codelet, a: float) -> Outcome:
    """build-cp-descrip (D-039; the legend's label [F95 p. 96 (img)]): write the descriptors
    of a correspondence that has entered the Worldview, its slippages and level, as
    build-obj-desc does for a built group (D-051; item 24)."""
    cp = correspondence_of(world, codelet.args[0])
    if cp is None:
        return fizzle(f"{codelet.args[0]} gone")
    slippages = C.describe(world, cp, by=codelet.type)
    words = " ".join(f"{s.a}-{s.b}" for s in slippages if s.kind != C.KIND)
    return Outcome(DESCRIBED, f"{cp.label()} [{words}] level {cp.level}")

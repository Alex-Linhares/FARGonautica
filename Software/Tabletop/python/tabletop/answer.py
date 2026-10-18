"""The answer: what a find-answer codelet announces, "the current object corresponding to
the touched object" [F95 p. 105] (D-057, D-058, D-063; D-059 for the labels; D-097 for
consumer C3, the answer from group position by the members' roles).

"When a find-answer codelet runs, an answer to the 'Do this!' challenge is picked, and the
program stops." [F95 p. 94] The answer is read off the Worldview: "In order for the program
to give an answer, it needs to have built up one or more mappings between objects or groups
of objects across the table (called correspondences)." [FH91 p. 180] "It should be noted that
the default object corresponding to the touched object is the touched object itself." [F95
p. 105] Reading the answer draws nothing from the generator.
"""

from dataclasses import dataclass

from tabletop import correspondences as C
from tabletop import describe as DS
from tabletop import roles as R
from tabletop import salience as S
from tabletop.config import GROUP_POSITION_MATCHES

# How the answer was reached (`docs/engine-api.md`, `AnswerGiven.through`).
CORRESPONDENCE = "correspondence"   # the other end of the touched object's correspondence
GROUP = "group"                     # read off a group correspondence (D-057, D-058)
DEFAULT = "default"                 # the touched object itself [F95 p. 105] (D-063)
BLOCKED = "blocked"                 # the default, with its obvious counterpart CLAIMED (D-101)

# The readings of `group_position_match` (D-097; D-058 revised): by the members' roles,
# then the group-frame words, then the literal object; or by the words, then the literal
# object. Both answer from group position.
ROLES, WORDS = GROUP_POSITION_MATCHES
# The roles consumer C3 matches, in the order they are matched (D-097): anchor for anchor,
# connector for connector, end for end, middle for middle.
ROLE_ORDER = (R.ANCHOR, R.CONNECTOR, DS.END, DS.MIDDLE)


@dataclass(frozen=True)
class Answer:
    """What Eliza touches (D-057): the `object` id, the problem's `label` for it (`E1`;
    several labels joined by `+`; empty when none names it, D-059), whether it is
    `literal` (the touched object, D-063), `through` (`correspondence`, `group`,
    `default`, or `blocked`: the default while a CLAIMED tag stands, D-101), `via` (the
    correspondence the answer was read off, or empty), a `note` for the trace (the group
    case of D-058; the claim), and `roles`, the touched object's roles the member was
    matched on (consumer C3, D-097; empty otherwise)."""

    object: str
    label: str
    literal: bool
    through: str
    via: str = ""
    note: str = ""
    roles: tuple = ()


def label_for(problem, id_: str) -> str:
    """The problem file's label(s) for an object, `E1`, or `E1+E2` when two labels name it,
    as the bar graphs label their answers [F95 p. 114] (D-059); empty when none does."""
    labels = [label for label, target in problem.answers.items() if target == id_]
    return "+".join(labels)


def touched_correspondence(world):
    """The Worldview correspondence with the touched object itself at an end, in the order
    built, or None (D-057)."""
    touched = world.problem.touched
    for cp in world.workspace.worldview():
        if cp.has_end(touched):
            return cp
    return None


def group_correspondence(world):
    """A Worldview group correspondence whose first end is a group holding the touched
    object, when the touched object is "mapped only through its group" (D-058), or None."""
    touched = world.problem.touched
    for cp in world.workspace.worldview():
        if (C.kind_of_end(world, cp.henry) == C.GROUP and C.kind_of_end(world, cp.eliza) == C.GROUP
                and touched in C.members_of(world, cp.henry)):
            return cp
    return None


def roles_in_group(world, group, id_: str) -> list:
    """The roles consumer C3 matches on a member (D-097), in `ROLE_ORDER`: the ANCHOR and
    CONNECTOR tags standing on it (D-087, D-088) and, while a MEMBER tag stands on it
    (D-090), its `end` or `middle` word in the group frame (D-029); empty for a member
    with none, so an empty pool reads nothing."""
    ws = world.workspace
    words = DS.group_frame(group, id_) if ws.has_role(id_, R.MEMBER) else []
    return [r for r in ROLE_ORDER
            if (ws.has_role(id_, r) if r in (R.ANCHOR, R.CONNECTOR) else r in words)]


def role_member(world, cp, id_: str) -> str | None:
    """Consumer C3 (D-097): the member of the group at the other end of a group
    correspondence with the same roles as `id_`, a member of the group at the first end:
    anchor for anchor, connector for connector, end for end, middle for middle. Each role
    of `id_` is matched in `ROLE_ORDER`: when some candidates carry it, only those stay;
    when none does, that role goes unmatched. The first of the candidates left, in member
    order (as D-058 takes the first), or None when `id_` has no role to match or no
    candidate shares one: nothing is gated. Draws nothing. The C3 lesion (D-104) patches
    this function."""
    ws = world.workspace
    mine, theirs = ws.group_named(cp.henry), ws.group_named(cp.eliza)
    candidates = list(theirs.members)
    matched = []
    for role in roles_in_group(world, mine, id_):
        sharing = [m for m in candidates if role in roles_in_group(world, theirs, m)]
        if sharing:
            candidates, matched = sharing, matched + [role]
    return candidates[0] if matched else None


def member_in_same_position(world, cp, touched: str) -> str | None:
    """The member of the corresponding group with the same group-frame words as the touched
    object (left end for left end, middle for middle): the `words` reading of
    `group_position_match`, and what the `roles` reading falls back on (D-058, D-097);
    None when no member has them."""
    ws = world.workspace
    mine = DS.group_frame(ws.group_named(cp.henry), touched)
    if not mine:
        return None
    for member in ws.group_named(cp.eliza).members:
        if sorted(DS.group_frame(ws.group_named(cp.eliza), member)) == sorted(mine):
            return member
    return None


def claimed_counterpart(world):
    """The CLAIMED tag standing on an obvious counterpart of the touched object, the
    first tagged, or None (D-089, D-101): read by consumer C4 when the answer is the
    default, "This new, unanticipated pressure makes it considerably more likely that
    Eliza will wind up simply touching Henry's cup" [F95 p. 34]."""
    tags = [t for t in world.workspace.roles if t.role == R.CLAIMED]
    return tags[0] if tags else None


def default_answer(world, via: str = "", note: str = "") -> Answer:
    """The default, the touched object itself [F95 p. 105] (D-063), recorded as `blocked`
    instead of `default` when a CLAIMED tag stands on one of its obvious counterparts
    (consumer C4, D-101): the object is the same, only the record and the note differ."""
    problem = world.problem
    touched = problem.touched
    tag = claimed_counterpart(world)
    if tag is None:
        return Answer(touched, label_for(problem, touched), True, DEFAULT, via, note)
    claim = f"{tag.item} is claimed by {tag.args[0]} through {tag.args[1]}"
    return Answer(touched, label_for(problem, touched), True, BLOCKED, via,
                  f"{note}; {claim}" if note else claim)


def answer_through_group(world, cp, touched: str) -> Answer:
    """The answer when the touched object is mapped only through its group (D-058 as
    revised, D-097): under `group_position_match` `roles`, the member with the same roles
    (`role_member`, consumer C3), then the member with the same group-frame words, then
    the literal object (`default_answer`, D-063, D-101); under `words`, the words, then
    the literal object."""
    problem = world.problem
    ws = world.workspace
    if world.config.group_position_match == ROLES:
        member = role_member(world, cp, touched)
        if member is not None:
            roles = tuple(roles_in_group(world, ws.group_named(cp.henry), touched))
            return Answer(member, label_for(problem, member), False, GROUP, cp.name,
                          f"the member of {cp.eliza} with the touched object's roles "
                          + " ".join(r.lower() for r in roles), roles)
    member = member_in_same_position(world, cp, touched)
    if member is not None:
        return Answer(member, label_for(problem, member), False, GROUP, cp.name,
                      f"the member of {cp.eliza} in the touched object's position")
    return default_answer(world, cp.name, f"the touched object is mapped only through {cp.henry}")


def answer_for(world) -> Answer:
    """The answer as the Worldview stands (D-057, D-058, D-063, D-097): the other end of
    the Worldview correspondence holding the touched object; a group at that end answers
    with its most salient member; a touched object covered only by a group correspondence
    answers from its group position (`answer_through_group`: the roles, the words, the
    literal object); and with no correspondence at all "Eliza will elect to touch the
    touched object" [F95 p. 105], recorded as `blocked` when its obvious counterpart is
    CLAIMED (`default_answer`, D-101)."""
    problem = world.problem
    touched = problem.touched
    cp = touched_correspondence(world)
    if cp is not None:
        other = cp.eliza if cp.henry == touched else cp.henry
        if C.kind_of_end(world, other) == C.GROUP:
            member = S.most_salient(world, C.members_of(world, other))
            return Answer(member, label_for(problem, member), member == touched, GROUP,
                          cp.name, f"the most salient member of {other}")
        return Answer(other, label_for(problem, other), other == touched, CORRESPONDENCE,
                      cp.name)
    cp = group_correspondence(world)
    if cp is not None:
        return answer_through_group(world, cp, touched)
    return default_answer(world)


def consume_roles(world, answer: Answer, by: str) -> None:
    """Record consumer C3's reading on the touched object's tags (D-097, D-105): the
    ANCHOR and CONNECTOR tags the answer was matched on, and the MEMBER tag for an end or
    middle match, each `RoleConsumed`. Called by find-answer, not by `answer_for`, which
    the snapshot reads without a side effect."""
    ws = world.workspace
    touched = world.problem.touched
    for role in answer.roles:
        tag = ws.role_tag(touched, role if role in (R.ANCHOR, R.CONNECTOR) else R.MEMBER)
        if tag is not None:
            R.consumed(world, tag, R.C3, f"answer {answer.object}", by)

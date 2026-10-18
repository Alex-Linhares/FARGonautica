"""What every codelet function shares (D-052): the `Outcome` it returns, the follow-ups it
posts one generation on, and the fizzle for a codelet whose argument is gone.

A codelet function takes the world, the codelet and the temperature exponent a(T) of the
step that chose it (D-060), does "a small, local task" [F95 p. 76], and returns an
`Outcome` saying what it did in the trace's vocabulary (`docs/trace-format.md`) and which
codelets it posts: "A particular codelet runs and succeeds in doing the task for which it
was designed; this codelet then posts a new set of codelets to the Coderack" [F95 p. 63]. A
codelet whose structure has gone from the Workspace does nothing: such codelets "have no
action at all" [F95 p. 73] (D-041).
"""

from dataclasses import dataclass, field

from tabletop import coderack as CR
from tabletop import salience as S

# The actions of the trace's vocabulary (`docs/trace-format.md`).
FOUND, BUILT, FIZZLED, PROPOSED, PASSED, FAILED, PICKED = (
    "found", "built", "fizzled", "proposed", "passed", "failed", "picked")
DESCRIBED, ENTERED, FIGHT, REMOVED, ANSWER = "described", "entered", "fight", "removed", "answer"


@dataclass(eq=False)
class Outcome:
    """What one codelet did (D-052): `action`, a word of the trace's vocabulary; `detail`,
    the rest of the line in plain words; `posted`, the follow-up codelets in posting order;
    `answer`, the `answer.Answer` a find-answer codelet announces, else None. The jolts a
    codelet sends are not kept here: the Slipnet publishes each as a `NodeJolted` event with
    the running codelet as its `by` (D-065)."""

    action: str
    detail: str = ""
    posted: list = field(default_factory=list)
    answer: object = None

    @property
    def fizzled(self) -> bool:
        """Whether the codelet did nothing [F95 p. 73]."""
        return self.action == FIZZLED


def fizzle(reason: str) -> Outcome:
    """The outcome of a codelet that did nothing, with its reason (D-041) [F95 p. 73]."""
    return Outcome(FIZZLED, reason)


def child(world, parent: CR.Codelet, codelet_type: str, args=(), raw=None) -> CR.Codelet:
    """A follow-up codelet, one generation on from its parent, hung at the current clock
    [F95 p. 81] (D-052)."""
    return CR.child_of(parent, codelet_type, world.config, world.clock, args, raw)


def children(world, parent: CR.Codelet, postings) -> list:
    """Several follow-ups from (type, args) pairs, or (type, args, count) triples, in order
    (D-052)."""
    out = []
    for posting in postings:
        codelet_type, args = posting[0], posting[1]
        count = posting[2] if len(posting) > 2 else 1
        for _ in range(count):
            out.append(child(world, parent, codelet_type, args))
    return out


def argument(world, codelet, exponent: float, candidates) -> str | None:
    """A codelet's target: its first argument when it has one, else a choice by salience
    among `candidates` under a(T) (D-020: a codelet the Slipnet posts has no argument and
    "chooses what to look at when it runs"; D-034); None when there is nothing to choose."""
    if codelet.args:
        return codelet.args[0]
    return S.choose(world, exponent, list(candidates))


def group_of(world, name: str):
    """The group a codelet's argument names, or None when it has gone from the Workspace
    (lost a fight or absorbed, D-025, D-026)."""
    ws = world.workspace
    return ws.group_named(name) if ws.has_group(name) else None


def correspondence_of(world, name: str):
    """The correspondence a codelet's argument names, or None when it has been removed
    (D-041)."""
    for cp in world.workspace.correspondences:
        if cp.name == name:
            return cp
    return None


def context_args(cp) -> tuple:
    """The arguments of a codelet that works on a correspondence: its name, followed by its
    parent's when it was built inside a group correspondence [F95 p. 108], so that the
    codelet carries its context (D-021, `context.context_of`)."""
    if cp.parent is None:
        return (cp.name,)
    return (cp.name, cp.parent.name)


def outer_group(world, id_: str):
    """The group whose chain a find-neighbor on this object extends (D-027): the earliest
    built gestalt group holding the object, else its innermost group, else None, since
    "in order for an object to have a neighbor, it must be in a group" [F95 p. 76]."""
    for g in world.workspace.groups:
        if id_ in g.members and not g.is_bottom_up():
            return g
    return world.workspace.innermost_group(id_)


def in_file_order(world, ids) -> list:
    """Ids sorted into the problem file's order, the order members are kept in (D-023)."""
    return [i for i in world.problem.ids() if i in ids]


def members(world, names) -> str:
    """Ids joined for the trace: `{fork1 fork2}` (`docs/trace-format.md`; D-065)."""
    return "{" + " ".join(names) + "}"

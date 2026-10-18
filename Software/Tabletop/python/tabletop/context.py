"""The context push and pop (D-021, D-070): entering the subworld of a group-level
correspondence and leaving it, with the Slipnet "wiped clean" on the way in and rebuilt by
reconstruction on the way out [F95 pp. 85-89].

"in some sense, it is as if when examine-cp(cp1) runs, it enters a subset of the full
Tableworld, a smaller world consisting of just the two groups in question" [F95 p. 85].
Two questions follow, "a 'context push'" and "a 'context pop'" [F95 p. 87]. The push: "the
activation level is set to its initial value (in this case, 1). So, in Tabletop, the
activation slate really is wiped clean." [F95 p. 87], for the concepts "that participate in
the supporting slippages of the correspondence under investigation" [F95 p. 88] (D-070; the
`french-faults` profile keeps French's two symmetry nodes). The pop: the program "reconstructs
the activation values at that level by looking around in the Workspace and finding the
correspondences in that context" [F95 p. 88], putting "a fixed amount of activation" into the
concepts of each supporting slippage [F95 p. 88]; the default profile adds the one spreading
step French asks for, "taking into account the activation levels of the nodes connected to the
reinitialized nodes" [F95 p. 89] (D-021, FIXED (ANSWERS 2)).

A context is a group-level correspondence; a codelet that works inside one (examine-cp, and
the create-cp, build-cp-descrip and cp-compete codelets it spawns) carries its name as an
argument (`context_of`). The run loop calls `enter` before each codelet: a codelet whose
context differs from the current one pops the current context (if any) and pushes its own
(if any). Contexts do not nest. `Config.context_mechanism` = False turns both off, the lesion
of FCCA's eleventh criterion [FCCA p. 365].
"""

from dataclasses import dataclass, field

from tabletop import correspondences as C

SYMMETRIES = ("mirror symmetry", "diagonal symmetry")   # French's two nodes [F95 p. 88]
SLIPPAGE_CONCEPTS, SYMMETRIES_ONLY = "slippage-concepts", "symmetries"   # D-070's values

# Which argument of a codelet names its context (D-021): examine-cp's first, create-cp's
# third [F95 p. 108], and the second of the two codelets create-cp's chain posts inside.
CONTEXT_ARGUMENT = {"examine-cp": 0, "create-cp": 2, "build-cp-descrip": 1, "cp-compete": 1}


@dataclass(eq=False)
class Context:
    """The subworld the program is in (D-021): the `correspondence` (its name) it entered,
    the `reset` nodes the push set to 1 (D-070), and `pushed_at`, the clock then."""

    correspondence: str
    reset: list = field(default_factory=list)
    pushed_at: int = 0


@dataclass(eq=False)
class Push:
    """What a push did, for the `ContextPushed` event: the `correspondence` entered and the
    nodes `reset` to 1, as (node, activation before) pairs (D-021, D-070)."""

    correspondence: str
    reset: list = field(default_factory=list)


@dataclass(eq=False)
class Pop:
    """What a pop did, for the `ContextPopped` event: the `correspondence` left, the nodes
    reinitialized, the `fixed` amounts added as (node, amount) pairs in the order found,
    the `spread` step as (node, amount) pairs (empty under the fault), and `restored`, every
    node touched with its activation after the pop (D-021)."""

    correspondence: str
    reinitialized: list = field(default_factory=list)
    fixed: list = field(default_factory=list)
    spread: list = field(default_factory=list)
    restored: list = field(default_factory=list)


def context_of(codelet) -> str | None:
    """The context a codelet carries (D-021): the name of the group-level correspondence
    its argument names, or None for a codelet that works at the top level, "the 'main
    world'" [F95 p. 87]."""
    position = CONTEXT_ARGUMENT.get(codelet.type)
    if position is None or len(codelet.args) <= position:
        return None
    return codelet.args[position]


def slippage_nodes(world, slippage) -> list:
    """The Slipnet nodes a slippage feeds on a pop (D-021), with repeats: its first concept,
    its second (so that an identity feeds its one concept twice, "into 'right' (twice)" [F95
    p. 89]), then its labels; the kind slippage (object-object, group-group, ...) names no
    node [F95 p. 68]. Names that are not nodes are left out."""
    if slippage.kind == C.KIND:
        return []
    names = (slippage.a, slippage.b) + tuple(slippage.labels)
    return [name for name in names if name in world.slipnet.index]


def concepts_of(world, slippage) -> list:
    """The distinct nodes a slippage names (D-070), in the order of `slippage_nodes`."""
    names = []
    for name in slippage_nodes(world, slippage):
        if name not in names:
            names.append(name)
    return names


def reset_set(world, cp) -> list:
    """The nodes a push reinitializes (D-070): under "slippage-concepts", every node named
    by a supporting slippage of the correspondence being entered, "Only concepts that
    participate in the supporting slippages of the correspondence under investigation are
    reinitialized." [F95 p. 88], and always the two symmetry nodes; under "symmetries",
    French's "only the 'mirror-symmetry' and 'diagonal-symmetry' nodes" [F95 p. 88]
    (`french-faults`)."""
    names = list(SYMMETRIES)
    if world.config.push_reset_set == SYMMETRIES_ONLY:
        return names
    for slippage in cp.slippages:
        for name in concepts_of(world, slippage):
            if name not in names:
                names.append(name)
    return names


def push(world, cp) -> Push:
    """Enter the subworld of a group-level correspondence (D-021, D-070): the reset set is
    "set to its initial value (in this case, 1)" [F95 p. 87], and the Slipnet's posting
    levels with it, so that a rise inside the subworld counts from 1 (D-020). Draws
    nothing."""
    names = reset_set(world, cp)
    before = [(name, world.slipnet.activation_of(name)) for name in names]
    world.slipnet.reset(names)
    world.context = Context(cp.name, names, world.clock)
    return Push(cp.name, before)


def correspondences_at_top(world) -> list:
    """The correspondences the pop reads: those of the Worldview at the top level, "the
    correspondences in that context" [F95 p. 88], in the order built. A correspondence built
    inside a group correspondence [F95 p. 108] belongs to its subworld, not to the top."""
    return [cp for cp in world.workspace.worldview() if cp.level == C.TOP]


def pop(world) -> Pop | None:
    """Leave the current subworld for the top level (D-021): the reset set is reinitialized
    again, "it reinitializes the activation values of the concepts associated with the
    correspondences in that context" [F95 p. 88]; then the top level is reconstructed from
    the Worldview's correspondences there. The fix first (`pop_restores_spread`, FIXED
    (ANSWERS 2)): what one spreading step (D-013) from every other node, at its current
    level, would send into the reinitialized nodes at 1, the refinement French names,
    "taking into account the activation levels of the nodes connected to the reinitialized
    nodes" [F95 p. 89]. Then the fixed amounts: for each supporting slippage,
    `pop_fixed_amount` goes into each of its concepts and labels, as "for each
    'right'-to-'left' slippage, it puts a fixed amount of activation into the concepts
    'right,' 'left,' and 'diagonal symmetry.'" [F95 p. 88], an identity feeding its one
    concept twice [F95 p. 89]. The sum is clipped once (D-015), and the posting levels are
    set to the restored activations, so the pop itself posts no codelet (D-020). None when
    the program is at the top level already. Draws nothing."""
    context = world.context
    if context is None:
        return None
    slipnet, cfg = world.slipnet, world.config
    record = Pop(context.correspondence, list(context.reset))
    levels = {name: 1.0 for name in context.reset}           # unclipped working levels
    slipnet.reset(context.reset)
    if cfg.pop_restores_spread:
        record.spread = slipnet.spread_step_into(context.reset)
        for name, amount in record.spread:
            levels[name] += amount
    for cp in correspondences_at_top(world):
        for slippage in cp.slippages:
            for name in slippage_nodes(world, slippage):
                if name not in levels:
                    levels[name] = slipnet.activation_of(name)
                levels[name] += cfg.pop_fixed_amount
                record.fixed.append((name, cfg.pop_fixed_amount))
    for name, level in levels.items():
        slipnet.restore(name, level)
    record.restored = [(name, slipnet.activation_of(name)) for name in levels]
    world.context = None
    return record


def alive(world, name: str) -> bool:
    """Whether the correspondence a context names is still in the Workspace (D-041)."""
    return any(cp.name == name for cp in world.workspace.correspondences)


def enter(world, codelet) -> tuple:
    """What the run loop does before a codelet runs (D-021): if the codelet's context is
    the current one, nothing; otherwise the current context, if any, is popped, and the
    codelet's, if it names a living correspondence, is pushed. Returns (push, pop), each
    None when it did not happen. With `context_mechanism` off, nothing ever happens."""
    if not world.config.context_mechanism:
        return (None, None)
    wanted = context_of(codelet)
    if wanted is not None and not alive(world, wanted):
        wanted = None
    current = world.context.correspondence if world.context is not None else None
    if wanted == current:
        return (None, None)
    popped = pop(world) if world.context is not None else None
    pushed = None
    if wanted is not None:
        pushed = push(world, world.workspace.correspondence_named(wanted))
    return (pushed, popped)

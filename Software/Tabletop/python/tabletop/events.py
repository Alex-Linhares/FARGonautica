"""The typed events of a run and the publisher that sends them to observers (D-065).

French's screen showed every step of a run: the table with its groups and correspondences,
the thermometer, the Slipnet grid and the Coderack bars [F95 pp. 95-107 (img)]; a run
reports "the object touched by Eliza, the sequence of structures that led to that answer,
the amount of resources expended, and the type and quality of structures present at the
end of the run" [F95 p. 145]. The engine says what it does by publishing the frozen
dataclasses below, each carrying `clock` (codelets run) and `draws` (the generator's draw
count, D-064) first and then plain data only: ids, names, numbers, strings and tuples,
never an engine object. Observers receive every event in subscription order and cannot
change the run: an observer that raises is recorded and skipped (`Publisher`), and no
observer is given the generator. `docs/engine-api.md` documents every event and
`docs/trace-format.md` the JSON record each becomes (`record_of`).
"""

import dataclasses
import sys
from dataclasses import dataclass


@dataclass(frozen=True)
class Event:
    """What every event carries first (D-065): `clock`, the codelets run when it was
    published, and `draws`, the generator's draw count then (D-064)."""

    clock: int
    draws: int

    @property
    def name(self) -> str:
        """The event's class name, the `event` key of its JSON record (D-065)."""
        return type(self).__name__


# ---- the run ----------------------------------------------------------------------------------


@dataclass(frozen=True)
class RunStarted(Event):
    """Once, before the Slipnet's first update (D-017, D-066): the `problem`, `seed` and
    `profile`, the `engine` that runs it (`compiled` or `python`, D-107), the fields
    `changed` from `standard` as (field, value) pairs, the `touched` object and the
    `objects` in file order."""

    problem: str
    seed: int
    profile: str
    engine: str
    changed: tuple
    touched: str
    objects: tuple


@dataclass(frozen=True)
class RunEnded(Event):
    """Once, last: what a run reports [F95 p. 145] (D-061, D-062): the `answer` and its
    `label`, `literal`, `capped`, the `structure_value`, the `codelets` run, the final
    `temperature` and the `worldview` as (name, end, end) tuples."""

    answer: str
    label: str
    literal: bool
    capped: bool
    structure_value: float
    codelets: int
    temperature: float
    worldview: tuple


# ---- codelets (D-052 to D-056) ----------------------------------------------------------------


@dataclass(frozen=True)
class CodeletPosted(Event):
    """A codelet hung on the Coderack [F95 p. 63] (D-020, D-052, D-055, D-056): its type,
    `args`, `raw` urgency, `generation`, adjusted `urgency` and `by` (`start`, `slipnet`,
    `answer-schedule`, or the parent's type)."""

    codelet: str
    args: tuple
    raw: float
    generation: int
    urgency: float
    by: str


@dataclass(frozen=True)
class CodeletChosen(Event):
    """A codelet chosen to run: "Codelets are selected to run based on two factors: their
    urgency and the current computational temperature." [F95 p. 77] (D-060): its `chance`
    at the draw, the exponent `a` [F95 p. 92], the `temperature` and the `rack_size` before
    it was taken off."""

    codelet: str
    args: tuple
    generation: int
    urgency: float
    chance: float
    a: float
    temperature: float
    rack_size: int


@dataclass(frozen=True)
class CodeletRan(Event):
    """What a codelet did (D-052, D-065): its `action` in the trace's vocabulary
    (`docs/trace-format.md`), the `detail` in words, and the types of the codelets it
    `posted`, "this codelet then posts a new set of codelets to the Coderack" [F95 p. 63]."""

    codelet: str
    args: tuple
    generation: int
    action: str
    detail: str
    posted: tuple


@dataclass(frozen=True)
class CodeletFizzled(Event):
    """A codelet that did nothing, "have no action at all" [F95 p. 73] (D-041), with the
    `reason`."""

    codelet: str
    args: tuple
    reason: str


@dataclass(frozen=True)
class CodeletRemoved(Event):
    """A codelet taken off the rack unrun, for `age` or `capacity` (D-053) [F95 p. 76;
    F95 p. 82]."""

    codelet: str
    args: tuple
    posted_at: int
    reason: str


# ---- the Slipnet (D-016, D-019, D-021) --------------------------------------------------------


@dataclass(frozen=True)
class SlipnetUpdated(Event):
    """One Slipnet update (D-019): `changed`, (node, activation) pairs for the nodes that
    moved, and `lengths`, (link, effective length) pairs for the labeled links that changed,
    both rounded to a tenth."""

    changed: tuple
    lengths: tuple


@dataclass(frozen=True)
class NodeJolted(Event):
    """A jolt of activation to a node, "a standard jolt" [F95 p. 97] (D-016): the `node`,
    the `amount`, `by` (the codelet type) and the `activation` after it."""

    node: str
    amount: float
    by: str
    activation: float


@dataclass(frozen=True)
class ContextPushed(Event):
    """A context push [F95 p. 87] (D-021, D-070): the group `correspondence` entered and
    the nodes `reset` to 1, as (node, activation before) pairs."""

    correspondence: str
    reset: tuple


@dataclass(frozen=True)
class ContextPopped(Event):
    """A context pop [F95 pp. 88-89] (D-021): the `correspondence` left, the nodes
    `reinitialized`, the `spread` of the one spreading step and the `fixed` amounts as
    (node, amount) pairs, and `restored`, every node touched with its activation after."""

    correspondence: str
    reinitialized: tuple
    spread: tuple
    fixed: tuple
    restored: tuple


# ---- the Workspace (D-004, D-023 to D-029) ----------------------------------------------------


@dataclass(frozen=True)
class ObjectNoticed(Event):
    """The first look at an object [F95 p. 64] (D-004): its id, `type`, the drawn `region`'s
    name and `by` (`start` for the touched object, else the codelet type)."""

    object: str
    type: str
    region: str
    by: str


@dataclass(frozen=True)
class DescriptionAdded(Event):
    """A new descriptor of an item, "an appropriate descriptor to the object concerned"
    [F95 p. 75] (D-028, D-029, D-071): the `item`, the `descriptor` and `by`."""

    item: str
    descriptor: str
    by: str


@dataclass(frozen=True)
class NeighborFound(Event):
    """A new neighbour relation inside a group [F95 p. 76] (D-027): `neighbor` lies on
    `side` of `object`, in `group`."""

    object: str
    neighbor: str
    side: str
    group: str


@dataclass(frozen=True)
class GroupProposed(Event):
    """A bottom-up group proposed (D-024): the proposal's name (`p1`), its `members`,
    `kind`, `category`, the `base` group extended or empty, and `by`."""

    group: str
    members: tuple
    kind: str
    category: str
    base: str
    by: str


@dataclass(frozen=True)
class GroupStagePassed(Event):
    """One of the three micro-tests of a proposal [FCCA p. 384] (D-024): the proposal, the
    `stage` (0, 1, 2), its `chance` and whether it `passed`."""

    group: str
    stage: int
    chance: float
    passed: bool


@dataclass(frozen=True)
class GroupBuilt(Event):
    """A group recorded in the Workspace (D-023 to D-025): its name, `members`, `kind`,
    `category`, `strength`, `parent` (the gestalt group or empty) and `orientation`."""

    group: str
    members: tuple
    kind: str
    category: str
    strength: float
    parent: str
    orientation: str


@dataclass(frozen=True)
class GroupRemoved(Event):
    """A group dropped from the Workspace, `absorbed` or `lost` (D-025, D-026)."""

    group: str
    reason: str


# ---- correspondences and the Worldview (D-035 to D-044) ---------------------------------------


@dataclass(frozen=True)
class CorrespondenceProposed(Event):
    """A pair a create-cp codelet found creatable [F95 p. 71] (D-035): the name it got,
    the `henry` and `eliza` ends, the `level` (`top` or the group correspondence's name)
    and `by`."""

    correspondence: str
    henry: str
    eliza: str
    level: str
    by: str


@dataclass(frozen=True)
class CorrespondenceBuilt(Event):
    """A correspondence entered the Workspace as a tentative structure (D-035, D-036): its
    `static` and `dynamic` points and `bonus` as first evaluated."""

    correspondence: str
    henry: str
    eliza: str
    static: float
    dynamic: float
    bonus: float


@dataclass(frozen=True)
class CorrespondenceTested(Event):
    """The initial test of a correspondence [F95 p. 72] (D-039): its `static` points, its
    `slippages` as (from, to, label) tuples, the `chance` and whether it `passed`."""

    correspondence: str
    static: float
    slippages: tuple
    chance: float
    passed: bool


@dataclass(frozen=True)
class CorrespondenceRemoved(Event):
    """A correspondence removed from the Workspace [F95 p. 73] (D-041): its `rating`, the
    `chance` of the removal (0 when it was not the clean-up's), and `inner`, the names of
    the correspondences built inside it, removed with it [F95 p. 108]."""

    correspondence: str
    rating: float
    chance: float
    inner: tuple


@dataclass(frozen=True)
class CompetitionHeld(Event):
    """One fight for the Worldview [F95 p. 72] (D-040, D-044): the `newcomer`, its `rivals`
    (empty for an entry without a fight), the `old_rating` and `rival_rating`, the `chance`
    the rival had (1 without a fight), `rival_wins` and the exponent `a`."""

    newcomer: str
    rivals: tuple
    old_rating: float
    rival_rating: float
    chance: float
    rival_wins: bool
    a: float


@dataclass(frozen=True)
class WorldviewChanged(Event):
    """A change of the Worldview (D-040 to D-042): the names `added` and `removed`, the
    `rating` after it, whether it is `coherent`, and the `reason` (`no rival`, `won`,
    `removed`)."""

    added: tuple
    removed: tuple
    rating: float
    coherent: bool
    reason: str


# ---- temperature and the answer (D-047 to D-050, D-056 to D-059) ------------------------------


@dataclass(frozen=True)
class TemperatureUpdated(Event):
    """A recomputation of T [F95 p. 91] (D-047 to D-050): the new `temperature`, the inputs
    `q`, `dq`, `dwv`, `tcp`, the exponent `a`, `held` (T still at its initial value, D-049)
    and `by` (`start`, `schedule`, `competition`, `change`)."""

    temperature: float
    q: float
    dq: float
    dwv: int
    tcp: int
    a: float
    held: bool
    by: str


@dataclass(frozen=True)
class AnswerCodeletPosted(Event):
    """One find-answer codelet hung by the schedule, "The lower the temperature, the more
    such codelets are hung on the Coderack." [F95 p. 94] (D-056): its `urgency` and the
    `temperature`."""

    urgency: float
    temperature: float


@dataclass(frozen=True)
class AnswerGiven(Event):
    """The answer [F95 p. 105] (D-057 to D-059, D-063): the object, its `label`, `literal`,
    `through` (`correspondence`, `group`, `default`, or `blocked`, the default with the
    obvious counterpart claimed, D-101), `via` and the `note`."""

    answer: str
    label: str
    literal: bool
    through: str
    via: str
    note: str


# ---- the roles (D-079, D-105) -------------------------------------------------------------------


@dataclass(frozen=True)
class RoleTagged(Event):
    """A role tag built in the pool (D-079, D-105): its name `tag` (`r1`, `r2`, ...), the
    `item` (an object id, a group name, or `worldview` for the axis), the `role`, its
    `args` (the partner of a pair role, the item a sidekick stands beside), its `weight`,
    its `strength` at tagging, the `evidence` it was read from, and `by` (the codelet type,
    or `perceived` for the oracle probe's hand-read tags)."""

    tag: str
    item: str
    role: str
    args: tuple
    weight: int
    strength: float
    evidence: str
    by: str


@dataclass(frozen=True)
class RoleDiscarded(Event):
    """A role tag removed (D-079, D-105): `tag`, `item`, `role`, the `reason` (`lost to rN`
    after a fight with a rival reading, `broken` by a role checker, `structure gone` with
    the group or correspondence it stood on) and `by`."""

    tag: str
    item: str
    role: str
    reason: str
    by: str


@dataclass(frozen=True)
class RoleRecast(Event):
    """The re-cast of the touched object's role signature onto the other row (D-105;
    published from the item of the roles loop that builds `roles.recast`, item 07): the `touched` object, its
    `signature` (its roles as words), the `tiers` (tuples of (tier, candidate, weight)),
    whether the first non-empty tier is `ambiguous` (two or more candidates), and `by`."""

    touched: str
    signature: tuple
    tiers: tuple
    ambiguous: bool
    by: str


@dataclass(frozen=True)
class RoleConsumed(Event):
    """A consumer read a role tag (D-095 and the later consumers' decisions; D-105): the `role`, the `consumer`
    (`C1` to `C8`), the `item` the tag stands on, the `effect` in words (`create-cp cup2
    small-glass1`) and `by`, the codelet type."""

    role: str
    consumer: str
    item: str
    effect: str
    by: str


EVENT_TYPES = (
    RunStarted, RunEnded, CodeletPosted, CodeletChosen, CodeletRan, CodeletFizzled,
    CodeletRemoved, SlipnetUpdated, NodeJolted, ContextPushed, ContextPopped, ObjectNoticed,
    DescriptionAdded, NeighborFound, GroupProposed, GroupStagePassed, GroupBuilt, GroupRemoved,
    CorrespondenceProposed, CorrespondenceBuilt, CorrespondenceTested, CorrespondenceRemoved,
    CompetitionHeld, WorldviewChanged, TemperatureUpdated, AnswerCodeletPosted, AnswerGiven,
    RoleTagged, RoleDiscarded, RoleRecast, RoleConsumed,
)
"""Every event class, in the order `docs/engine-api.md` lists them (D-065); the four role
events are appended after the reconstruction's 27 (D-105)."""


def record_of(event: Event) -> dict:
    """The JSON record of an event (`docs/trace-format.md`): `event`, `clock`, `draws`
    first, then the event's fields in dataclass order; tuples become lists when written,
    engine objects never appear (D-065)."""
    record = {"event": event.name}
    record.update(dataclasses.asdict(event))
    return record


# ---- the publisher ------------------------------------------------------------------------------


class Publisher:
    """Sends events to the observers of a run (D-065). `record(cls, **fields)` builds the
    event with the world's clock and draw count and hands it to every observer in
    subscription order; an observer that raises is caught, the error recorded in `errors`
    as (observer, event, error) strings and reported on stderr, and the run goes on exactly
    as without it. `actor` is the codelet type running, for the events that say `by`
    without being told. With no observers nothing is built. A publisher without a world
    (the default of a `Workspace` or `Slipnet` built on its own) records clock 0 and
    draws 0."""

    def __init__(self, world=None, observers=()):
        self.world = world
        self.observers = list(observers)
        self.errors: list = []
        self.actor = "start"
        self.count = 0

    @property
    def listening(self) -> bool:
        """Whether anyone observes the run (D-065): `record` builds nothing otherwise, and a
        caller may skip the work of a field that only the event would read."""
        return bool(self.observers)

    def record(self, cls, **fields):
        """Build and publish one event (D-065); returns it, or None when no one is listening."""
        if not self.listening:
            return None
        clock = self.world.clock if self.world is not None else 0
        draws = self.world.rng.draws if self.world is not None else 0
        event = cls(clock, draws, **fields)
        self.publish(event)
        return event

    def publish(self, event: Event) -> None:
        """Hand an event to every observer, in subscription order, catching errors (D-065)."""
        self.count += 1
        for observer in self.observers:
            try:
                observer.on_event(event)
            except Exception as error:   # an observer must never stop the run (D-065)
                note = (type(observer).__name__, event.name, f"{type(error).__name__}: {error}")
                self.errors.append(note)
                print(f"tabletop: observer {note[0]} raised on {note[1]}: {note[2]}",
                      file=sys.stderr)


class Collector:
    """An observer that keeps every event in a list, for tests and for a GUI's event log
    (D-065)."""

    def __init__(self):
        self.events: list = []

    def on_event(self, event: Event) -> None:
        """Keep the event (D-065)."""
        self.events.append(event)

    def of_type(self, cls) -> list:
        """The events of one class, in order (D-065)."""
        return [e for e in self.events if isinstance(e, cls)]

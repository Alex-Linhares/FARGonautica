"""The typed events of a run and the `Subject` that publishes them (DECISION D-055).

The engine says what it does by publishing events: frozen dataclasses of plain data (names,
integers, strings, tuples), never engine objects. The text trace that imitates the
transcripts (`seekwhence.trace`) is one observer; the tests' recorders are others; the engine
never depends on any of them, and an observer that raises is reported and skipped so that it
cannot change the run's course. This is the Numbo port's `observe.py` pattern, adopted by
DECISION D-055 ("the engine emits typed events and the trace is one observer").

The event types of this module cover the run loop and the taskrack (SPEC 2.3, 2.5): the start
of a run, a term entered, a task posted, chosen, found stale or cleaned, the prompt and the
caps, and `Message`, one line printed as the transcripts print it [M86 pp. 164-179]. The
modules of the cytoplasm, the gnoths and the reformulation add their own event types.
"""

import dataclasses
import sys
import traceback
from typing import ClassVar, Protocol, runtime_checkable


@runtime_checkable
class Observer(Protocol):
    """Anything with `on_event(event)` can watch a run (DECISION D-055)."""

    def on_event(self, event: "Event") -> None:
        """Receive one event (DECISION D-055)."""


@dataclasses.dataclass(frozen=True)
class ObserverError:
    """`observer` raised `exception` on `event` (DECISION D-055: an observer never stops the
    run, so the error is recorded instead)."""
    observer: object
    event: "Event"
    exception: BaseException


def report_to_stderr(error: ObserverError) -> None:
    """The default `on_error`: say on stderr which observer failed on which event, with the
    traceback (DECISION D-055)."""
    sys.stderr.write(f"seekwhence: observer {error.observer!r} failed on a "
                     f"{error.event.kind} event:\n")
    traceback.print_exception(error.exception, file=sys.stderr)


class Subject:
    """Publishes events to its observers, in subscription order (DECISION D-055).

    An observer's exception is recorded in `errors`, passed to `on_error` (None to keep
    quiet) and otherwise ignored: the run and the other observers go on. A `Subject` is
    itself an observer, so one can be subscribed to another to pass events on.
    """

    def __init__(self, on_error=report_to_stderr):
        self._observers: list = []
        self.on_error = on_error
        self.errors: list = []

    @property
    def observers(self) -> tuple:
        """The observers subscribed now, in order (DECISION D-055)."""
        return tuple(self._observers)

    def subscribe(self, observer):
        """Add `observer` last; returns it (DECISION D-055)."""
        if not isinstance(observer, Observer):
            raise TypeError(f"{observer!r} has no on_event method")
        self._observers.append(observer)
        return observer

    def unsubscribe(self, observer) -> None:
        """Remove `observer`; a ValueError if it is not subscribed (DECISION D-055)."""
        self._observers.remove(observer)

    def publish(self, event: "Event") -> None:
        """Give `event` to every observer subscribed now (DECISION D-055)."""
        if not isinstance(event, Event):
            raise TypeError(f"not an event: {event!r}")
        for observer in tuple(self._observers):
            try:
                observer.on_event(event)
            except Exception as exception:
                error = ObserverError(observer, event, exception)
                self.errors.append(error)
                if self.on_error is not None:
                    self.on_error(error)

    on_event = publish


class Recorder:
    """An observer that keeps every event in a list, for tests and the session's history
    (DECISION D-055)."""

    def __init__(self):
        self.events: list = []

    def on_event(self, event: "Event") -> None:
        """Append the event (DECISION D-055)."""
        self.events.append(event)

    def of_kind(self, kind: str) -> list:
        """The recorded events of one `kind`, in order (DECISION D-055)."""
        return [e for e in self.events if e.kind == kind]


# ---------------------------------------------------------------------------------------
# The events

EVENT_TYPES: dict = {}


def _event(cls):
    cls = dataclasses.dataclass(frozen=True)(cls)
    if "kind" in cls.__dict__:
        assert cls.kind not in EVENT_TYPES, cls.kind
        EVENT_TYPES[cls.kind] = cls
    return cls


@_event
class Event:
    """An event of a run: plain data with a `kind` (DECISION D-055)."""
    kind: ClassVar[str]


@_event
class RunStarted(Event):
    """A run began from `seed` with the `config` (its field names and values as a tuple of
    pairs); the trace prints `-> (startup)` [M86 p. 164]."""
    kind = "start"
    seed: int
    config: tuple


@_event
class TermEntered(Event):
    """Term number `n` (from 1) was entered with the value `term` [M86 p. 164]; `cleaned` is
    how many old tasks the entry removed (DECISION D-010)."""
    kind = "term"
    n: int
    term: int
    cleaned: int


@_event
class TaskPosted(Event):
    """A task was placed on the taskrack "where [it] will stay until chosen and run"
    [M86 p. 41]: its `number`, `type`, `args` (names and numbers), `urgency`, `arena` and
    `created_at` (the clock, DECISION D-007)."""
    kind = "posted"
    number: int
    type: str
    args: tuple
    urgency: int
    arena: str
    created_at: int


@_event
class TaskChosen(Event):
    """A task was chosen from the taskrack and is about to run; `clock` is its ordinal in the
    run. The trace prints `doing task <type> on (<args>)` [M86 p. 164] (DECISION D-055).
    `empty-task` is reported this way too, with no task number (DECISION D-013)."""
    kind = "chosen"
    clock: int
    number: int | None
    type: str
    args: tuple
    urgency: int
    arena: str


@_event
class TaskStale(Event):
    """A task ran and found "the Seek-Whence world quite different than when it was
    created" [M86 p. 42], so it did nothing; `reason` says what was missing
    (DECISION D-009)."""
    kind = "stale"
    type: str
    args: tuple
    reason: str


@_event
class TasksCleaned(Event):
    """Old tasks were removed from the taskrack at one of the three cleaning `moment`s
    (`term`, `reform`, `hypothesis`; DECISION D-010): `removed` is a tuple of (type, count)
    pairs, in the rack's order of first appearance."""
    kind = "cleaned"
    moment: str
    removed: tuple


@_event
class TermRequested(Event):
    """The run loop stopped to ask for the next term: `reason` is `call-term` (a Call-term
    task ran [M86 p. 165]), `empty` (the taskrack was empty, DECISION D-013) or `term-cap`
    (the per-term cap, DECISION D-011); `tasks` is how many tasks ran since the last prompt."""
    kind = "prompt"
    reason: str
    tasks: int


@_event
class RunCapped(Event):
    """The whole-run cap `max_tasks` was reached after `clock` tasks (DECISION D-011)."""
    kind = "capped"
    clock: int


@_event
class Message(Event):
    """One line the program prints, as the transcripts print it [M86 pp. 164-179]: `text` is
    the whole line. Used for lines that are messages and nothing more (`gnoths constructed`,
    `bye`); structured happenings get events of their own."""
    kind = "message"
    text: str


# ---- the cytoplasm: glints, sparks and bonds [M86 pp. 61-65] ----------------------------------

@_event
class GlintMade(Event):
    """A term's glint was made and listed as a manifestation of its ideal atom [M86 p. 61]:
    its `name` (`glint3`), `value` and `position` (from 1). Nothing is printed for it."""
    kind = "glint"
    name: str
    value: int
    position: int


@_event
class SparkMade(Event):
    """A spark was made between two elements [M86 p. 63]; the trace prints
    `spark1 --- between glint1glint2` [M86 p. 164]."""
    kind = "spark"
    name: str
    a: str
    b: str


@_event
class BondMade(Event):
    """A bond was made [M86 p. 64]: its `name`, `type` (`Same`, `Succ`, `Pred`), `field`
    (`print-value`, `span`), `modifier` (`adjacent`, `remote`), its members `a` and `b`
    (left to right) and its `strength` (DECISION D-019); the trace prints
    `bond1 --- between glint1glint3` [M86 p. 164]."""
    kind = "bond"
    name: str
    type: str
    field: str
    modifier: str
    a: str
    b: str
    strength: int


@_event
class GlomDissolved(Event):
    """A Dissolver destroyed a glom "leaving its highest-level subgloms to float
    independently in the cytoplasm" [M86 p. 70]: the glom's `name` and the `freed`
    subgloms' names. Nothing is printed for it."""
    kind = "dissolved"
    name: str
    freed: tuple


# ---- the cytoplasm: gloms and the terraced scan [M86 pp. 65-70] -----------------------------

@_event
class GlomProposed(Event):
    """A Glom-scout's quicktest passed and a Glomtester was set out [M86 p. 66]: the bond
    `family` as labelled from the element's side (`Same`, `Succ`, `Pred`; DECISION D-020),
    the `test` (`group`, `cover`, `fence`) and the `target` element; the trace prints
    `Same -cover proposed --> glint1` [M86 p. 165]."""
    kind = "proposed"
    family: str
    test: str
    target: str


@_event
class GlomtesterFailed(Event):
    """A Glomtester found its target no longer free in the cytoplasm [M86 p. 67]; the trace
    prints `Glomtester failed glint2` [M86 p. 170]."""
    kind = "glomtester-failed"
    target: str


@_event
class GlomMade(Event):
    """A Glommer made a glom and an active focus [M86 p. 67]: its `name`, `type` (the
    three words of the glom's type field), `members` (names, left to right) and
    `print_value`; the trace prints `Glommer for Same print-value fence members: (glint1
    glint2)` [M86 p. 165]."""
    kind = "glom"
    name: str
    type: tuple
    members: tuple
    print_value: tuple


@_event
class GlomFailed(Event):
    """A Glommer could not make its glom (DECISION D-021); the trace prints `failed to glom
    (glint1 glint2 glint3)` [M86 p. 165]."""
    kind = "glom-failed"
    members: tuple


@_event
class GlomDubbed(Event):
    """A glom was dubbed a manifestation of a plato-class [M86 p. 69]: its `name`, the
    class `kind`, the `form` text put in its structure field and its `purity` (`pure` or
    `secondary`). Nothing is printed for it."""
    kind = "dubbed"
    name: str
    kind_name: str
    form: str
    purity: str


@_event
class GlomBurst(Event):
    """A Burster destroyed a glom "and all its non-glint subgloms" [M86 p. 69]: the glom's
    `name` and the `glints` left floating. Nothing is printed for it."""
    kind = "burst"
    name: str
    glints: tuple


# ---- templates [M86 pp. 73-76] ---------------------------------------------------------------

@_event
class TopDownGlom(Event):
    """A glom was made from above, "direction from above: instructions to make or dissolve
    gloms, to create units of a particular form" [M86 p. 74]: its `name`, `members` (the
    subgloms' names) and `print_value`, and whether it is a `pseudo` glom (the template glom
    [M86 p. 73], a gnoth-glom [M86 p. 77]) or a real unit; the trace prints `top-down glom
    glom2` [M86 p. 167]."""
    kind = "top-down"
    name: str
    members: tuple
    print_value: tuple
    pseudo: bool


@_event
class TemplateGlomMade(Event):
    """A template put its template glom "over the highest-level gloms in the cytoplasm"
    [M86 p. 73]: the template's `form` text, the `members` covered and the glom's `name`;
    the trace prints `create-template-glom (Cycle 3 2 (3 7))(glom1)` [M86 p. 167], and the
    glom's own `TopDownGlom` follows."""
    kind = "template-glom"
    form: str
    members: tuple
    name: str


@_event
class TemplateMade(Event):
    """A template was made (or re-made by a Template-reviewer's modification [M86 p. 75]):
    its four fields as printed, `template made : (form (Cycle 3 2 (3 7)) state working
    coverage (1 2) glom glom2)` [M86 p. 167]: the `form` text, the `state`, the `coverage`
    (first and last position covered) and the template `glom`'s name; `from_glom` is the
    dubbed glom that engendered it [M86 p. 73]; `immediate` when the Template-scout applied
    the form itself, which prints `template: (form (S-group 1 2)` [M86 p. 171] (DECISION
    D-060)."""
    kind = "template"
    form: str
    state: str
    coverage: tuple
    glom: str
    from_glom: str
    immediate: bool = False


@_event
class TemplateChecked(Event):
    """A Template-evaluator checked a template [M86 p. 74]: the template's class
    `kind_name`, whether it `passed`, and the `text` printed, `check-cycle template (Cycle 3
    2 ((3 7)))` [M86 p. 167], or an empty string when nothing is printed (DECISION D-027)."""
    kind = "template-check"
    kind_name: str
    passed: bool
    text: str


@_event
class TemplateAccepted(Event):
    """A template passed its evaluation: "The creation and acceptance of a template ...
    sets off two tasks, a Gnoth-maker and a Hypothesizer" [M86 pp. 75-76]; `form` is the
    template's form text. Nothing is printed for it."""
    kind = "template-accepted"
    form: str


@_event
class TemplateAbandoned(Event):
    """A template was "rejected and abandoned" [M86 p. 74] or targeted "for abandoning"
    [M86 p. 75]: its `form` text, the `reason` and the task that did it (`by`). Nothing is
    printed for it."""
    kind = "template-abandoned"
    form: str
    reason: str
    by: str


@_event
class TemplateFiltered(Event):
    """A new term was "'filtered' past the template, checked for agreement with it"
    [M86 p. 74]: the `term`, its position `n` and whether it `fits`; a term that does not
    fit sets up a review [M86 p. 74]. Nothing is printed for it."""
    kind = "template-filtered"
    n: int
    term: int
    fits: bool


@_event
class TemplateModified(Event):
    """A Template-reviewer called "for modification of the template" [M86 p. 75]: the `old`
    and `new` form texts. The re-creation prints its own lines."""
    kind = "template-modified"
    old: str
    new: str


@_event
class Blast(Event):
    """A Template-resolver "'blasts' (does an immediate burst of) all gloms" [M86 p. 75]:
    the names of the gloms `burst`. Nothing is printed for it."""
    kind = "blast"
    burst: tuple


# ---- gnoths and the hypothesis [M86 pp. 76-79] -----------------------------------------------

@_event
class GnothMade(Event):
    """A gnoth was made over a glom collection [M86 pp. 77-78]: its `name`, `frame`, the
    class it actualizes (`plato_class`), its gnoth-glom's name (`glom`), its `range` and the
    names of the `members` of its collection. The gnoth-glom's own `TopDownGlom` precedes
    it; nothing else is printed."""
    kind = "gnoth"
    name: str
    frame: int
    plato_class: str
    glom: str
    range: tuple
    members: tuple


@_event
class CatchallMade(Event):
    """The catchall gnoth was made, "a repository for non-troublesome terms" [M86 p. 98]
    (DECISION D-030): its `name` and the names of the `glints` it holds. Nothing is
    printed."""
    kind = "catchall"
    name: str
    glints: tuple


@_event
class GnothsConstructed(Event):
    """A Gnoth-maker finished: the `names` of the gnoths made; the trace prints
    `gnoths constructed` [M86 p. 167]."""
    kind = "gnoths-constructed"
    names: tuple


@_event
class HypothesisMade(Event):
    """The Hypothesizer made the hypothesis [M86 p. 79]: its `form` text, printed on its own
    line, `(Countup 1)` [M86 p. 171], and its `source`, `template` or `fallback` (the Tuple
    fall-back of p. 79; DECISION D-031)."""
    kind = "hypothesis"
    form: str
    source: str


# ---- filtering and the guess [M86 pp. 98, 166, 168, 175-176] --------------------------------

@_event
class TermFiltered(Event):
    """The Hfilter checked the term at position `n` (from 1), `term`, against the
    hypothesis's `form` [M86 p. 166]: `ok` says whether it was the `predicted` term. The
    trace prints `new term being hypothesis-filtered through (Cycle 3 2 (3 7))`."""
    kind = "filtered"
    n: int
    term: int
    form: str
    ok: bool
    predicted: int


@_event
class HypothesisRefuted(Event):
    """A term the hypothesis did not predict: the validity is set to 0 and the Gnoth-setter
    set out [M86 p. 98]; the `form` refuted, the position `n` and the `term` that refuted
    it, and the term `predicted`. The trace prints `set-out -- validity: 0` [M86 p. 168]."""
    kind = "refuted"
    form: str
    n: int
    term: int
    predicted: int


@_event
class GuessMade(Event):
    """The program ventured a guess [M86 p. 166]: the hypothesis's `form`, the `block` as
    printed (`3 7 3 7`, DECISION D-033) and its `terms` flattened. The trace prints
    `I have a guess!`, `hypothesis: (Cycle 3 2 (3 7))` and the block."""
    kind = "guess"
    form: str
    block: str
    terms: tuple


@_event
class GuessAnswered(Event):
    """The user answered a guess: the `answer` as given and whether it was `accepted`
    (`ok`, DECISION D-053). The trace prints `enter no if wrong, ok if right ok` and, when
    accepted, `bye` [M86 p. 166]."""
    kind = "answered"
    answer: str
    accepted: bool


@_event
class RunEnded(Event):
    """The run is over: the `reason`, `ok` when the user accepted a guess (`bye`
    [M86 p. 166]). Nothing more is printed."""
    kind = "ended"
    reason: str


@_event
class HypothesisFreezeDried(Event):
    """The Gnoth-setter freeze-dried the refuted hypothesis [M86 pp. 91, 99]: its `form`
    text, the number of terms it `explained` and the class made `reigning` ("the Platonic
    class at the highest level of the (former) hypothesis" [M86 p. 99]). Nothing is
    printed."""
    kind = "freeze-dried"
    form: str
    explained: int
    reigning: str


@_event
class GnothFilled(Event):
    """The Gnoth-setter filled a gnoth to a frame of the old hypothesis [M86 p. 98]
    (DECISION D-036): its `name`, `frame`, the new gnoth-glom's name (`glom`), its `range`,
    the names of the `members` of its collection and whether the gnoth is `new`. The
    gnoth-glom's own `TopDownGlom` precedes it; nothing else is printed."""
    kind = "gnoth-filled"
    name: str
    frame: int
    glom: str
    range: tuple
    members: tuple
    new: bool


@_event
class GnothsSet(Event):
    """A Gnoth-setter finished laying out the gnoths (DECISION D-036): their `names`,
    `frames` and `ranges` in order; the trace prints `gnoths: (gnoth1 gnoth2 gnoth3)`
    [M86 pp. 168, 173, 176]."""
    kind = "gnoths-set"
    names: tuple
    frames: tuple
    ranges: tuple


@_event
class SlipChecked(Event):
    """The slip-or-stay check was made for a reigning class that is not strict [M86
    pp. 103-105] (DECISION D-042): the `reigning` class, the staying pressure `stayval`, the
    `best` neighbour (None when none scores) with its `pressure`, every neighbour's
    pressure in `pressures` (pairs) and whether the system `slipped`. The trace prints
    `slip-check: stayval: 15.0` and `        best: (Y-group 4.0)` [M86 p. 176], or
    `best: nil` [M86 p. 168]."""
    kind = "slip-check"
    reigning: str
    stayval: float
    best: str | None
    pressure: float
    pressures: tuple
    slipped: bool


@_event
class ClassChanged(Event):
    """The reigning class changed, from `old` to `new`, for a `reason` (`strict`, a strict
    class slipping at once [M86 p. 99], or `slip`, a neighbour beating the staying pressure
    [M86 p. 104]). The trace prints `Changing to a new reigning class. Countup --> S-group`,
    the words of Meredith's annotation [M86 p. 174] (DECISION D-042)."""
    kind = "class-changed"
    old: str
    new: str
    reason: str


# ---- gnoth operations, the Gnoth-operator and the Capper [M86 pp. 109-124] -----------------

@_event
class OperationGlom(Event):
    """A gnoth operation made a glom [M86 pp. 112-113]: its `name`, `type` (`capture`,
    `enclose` or `merge`, DECISION D-066), the `members` it covers and its `print_value`.
    Nothing is printed; the Plato-scout set out on it shows in the task lines."""
    kind = "operation-glom"
    name: str
    type: tuple
    members: tuple
    print_value: tuple


@_event
class GnothGone(Event):
    """A gnoth disappeared, "a shell which the system destroys" [M86 p. 108] (`[]
    (disappears)` [M86 p. 120]): its `name` and the `reason`. Nothing is printed."""
    kind = "gnoth-gone"
    name: str
    reason: str


@_event
class GnothUncapped(Event):
    """A NO-OP took a gnoth's cap off until a Capper recaps it [M86 p. 113]: its `name`
    and its `range` at the time, which the Capper will claim (DECISION D-045). Nothing is
    printed."""
    kind = "gnoth-uncapped"
    name: str
    range: tuple


@_event
class GnothCapped(Event):
    """A Capper recapped a gnoth [M86 p. 113]: its `name`, the new cap `glom`, the `range`
    claimed and the names of the elements `claimed`. The cap's own `TopDownGlom`
    precedes it."""
    kind = "gnoth-capped"
    name: str
    glom: str
    range: tuple
    claimed: tuple


@_event
class GnothOperation(Event):
    """One gnoth operation was attempted [M86 pp. 110-118] (DECISION D-067): the
    `operation` as printed, the `gnoths` touched and recapped, the gnoths `gone`,
    `created` (by SPLIT), the gloms `made`, the gnoths left `uncapped` (by NO-OP), and
    `reason`, None when it was carried out and otherwise why not. The caps' `TopDownGlom`
    events precede it; nothing else is printed."""
    kind = "operation"
    operation: str
    gnoths: tuple
    gone: tuple
    created: tuple
    made: tuple
    uncapped: tuple
    reason: str | None


@_event
class GnothOperated(Event):
    """A Gnoth-operator finished [M86 p. 124]: the `program` as printed, how many `steps`
    were attempted, whether it `acted` (then the threshold was decreased by 1), the
    reason it `stopped` short (None when it ran through) and the `threshold` afterwards.
    Nothing is printed beyond the task line and the caps' `top-down glom` lines
    [M86 pp. 174, 177]."""
    kind = "operated"
    program: str
    steps: int
    acted: bool
    stopped: str | None
    threshold: float | None


# ---- the Reformulator and the Bond-assessor [M86 pp. 105-109, 124] -------------------------

@_event
class MoveFound(Event):
    """A Reformulator found a neighbour pull strong enough [M86 pp. 107-109]: the `level`
    of p. 107's order, the `left` and `right` gloms, their gnoths `left_gnoth` and
    `right_gnoth`, their `pull`, which one is the `mover` (`left` or `right`), the pull
    `held` on it by its own gnoth, the `strength` (pull minus holding, DECISION D-043) and
    the `program` built for it, as printed (DECISION D-044). Nothing is printed; the
    Gnoth-operator's task line shows the program [M86 p. 174]."""
    kind = "move-found"
    level: int
    left: str
    right: str
    left_gnoth: str
    right_gnoth: str
    pull: float
    mover: str
    held: float
    strength: float
    program: str


@_event
class Reformulated(Event):
    """A Reformulator finished [M86 p. 105]: the reigning `kind`, the `threshold` it set
    (DECISION D-046) and the `program` it gave a Gnoth-operator, as printed, or None when
    it "finds no actions to be taken"; a Bond-assessor follows either way. Nothing is
    printed beyond the task line."""
    kind = "reformulated"
    kind_: str
    threshold: float
    program: str | None


@_event
class BondsAssessed(Event):
    """A Bond-assessor decided [M86 p. 124]: the reigning class `kind_`, the `threshold` it
    was given, the `strongest` bond value (by absolute value) in that environment and
    whether reformulation goes on (`more`: a Reformulator was set out, else a Gnoth-caster)
    (DECISION D-047). Nothing is printed beyond the task line, `doing task Bond-assessor on
    (S-group 8.0)` [M86 p. 174]."""
    kind = "bonds-assessed"
    kind_: str
    threshold: float
    strongest: float
    more: bool


# ---- the Gnoth-caster, the Slip-scout and the blast [M86 pp. 92, 124-126] --------------------

@_event
class CastsMade(Event):
    """A Gnoth-caster cast the parse [M86 pp. 124-125]: the reigning class `kind_`, the
    `gnoths` cast in order, the `casts` as the `casts:` line prints them, `((S-group 1 2)
    (S-group 1 1))` [M86 p. 174], the `generalised` form as printed on the next line, or
    None, whether the `last` gnoth was left out as the one that "shows promise"
    [M86 p. 125], and the `reason` when no form came out (DECISION D-048)."""
    kind = "casts"
    kind_: str
    gnoths: tuple
    casts: str
    generalised: str | None
    last: bool
    reason: str | None


@_event
class CandidateMade(Event):
    """A candidate postdicted the terms and "is instantiated as the new hypothesis for the
    system" [M86 p. 125]: its `form`, printed `new hypoth candidate (S-group 1 2)`
    [M86 p. 174], and the reigning class `kind_` it was cast under (DECISION D-034)."""
    kind = "candidate"
    form: str
    kind_: str


@_event
class CastFailed(Event):
    """"the reformulation effort has failed" [M86 p. 126]: the `reason` (a gnoth could not
    be cast, the casts did not generalise, a freeze-dried repeat, or no postdiction), the
    number of `failures` in this episode and whether the `blast` follows instead of a
    Slip-scout (DECISION D-050). Nothing is printed."""
    kind = "cast-failed"
    reason: str
    failures: int
    blast: bool


@_event
class SlipScouted(Event):
    """A Slip-scout ran [M86 p. 126]: the class reigning `before` and `after` it, the
    `factor` applied to the staying pressure and the `stayval` of its check (None for a
    strict class, which slips at once) (DECISION D-049). The check's own `SlipChecked`
    and any `ClassChanged` precede it; nothing else is printed."""
    kind = "slip-scouted"
    before: str
    after: str
    factor: float
    stayval: float | None


@_event
class Blasted(Event):
    """The blast [M86 p. 92]: the `reason`, the `gnoths` destroyed, the `gloms` burst and
    the `bonds` that survive between glints (DECISION D-050); printed as the
    reconstruction's own line, `blasting away all gnoths and gloms`."""
    kind = "blasted"
    reason: str
    gnoths: tuple
    gloms: tuple
    bonds: tuple

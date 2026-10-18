"""The Coderack: "the staging area for the codelets" [F95 p. 76], where codelets wait with
an urgency, a generation and an age until one is chosen to run (D-052 to D-055).

"Each codelet put on the Coderack has an urgency - a number that represents approximately how
important the program estimates it is to run that particular codelet." [F95 p. 76] "A typical
codelet hung on the Coderack looks like this: (codelet-name codelet-age codelet-urgency)."
[F95 p. 77] Each codelet is also "tagged to indicate the generation to which it belongs"
[F95 p. 81]: its adjusted urgency is its raw urgency divided by rho to the power of its
generation, rho = 3 [F95 p. 81 (img)], French's answer to urgency explosion. "Codelets are
selected to run based on two factors: their urgency and the current computational
temperature." [F95 p. 77] Each urgency u is raised to the temperature exponent a, from 6 at
T = 0 to 0.1 at T = 100 [F95 p. 92], and the chance of a codelet is its share of the total
(D-060): 0.6, 0.3 and 0.1 for urgencies 6, 3 and 1 at a = 1 [F95 p. 77], 0.985, 0.015 and
under 0.001 at a = 6 [F95 p. 93]. The rack holds 150 and "the oldest codelets will
eventually be 'pushed off' the Coderack in order to make room for the newcomers" [F95 p. 82];
"A codelet that has been on the Coderack too long is eliminated" [F95 p. 76]; nothing is
removed for low urgency, "since, at least in theory, this will lead to a greater diversity in
representational possibilities" [F95 p. 77].

The rack is a list in posting order, so the oldest codelet is always first; the temperature
exponent is passed in by the run loop (`temperature.exponent_at`); every draw goes through
the run's generator, one per choice.
"""

from dataclasses import dataclass

from tabletop import accel
from tabletop.config import CODELET_TYPES, codelet_label

_INFINITY = float("inf")       # the adjusted urgency past an underflowed power of rho (D-052, D-060)

# Who posted a codelet, when it was not a parent codelet (D-054, D-055, D-056).
START = "start"
SLIPNET = "slipnet"
ANSWER_SCHEDULE = "answer-schedule"

# Why a codelet left the rack without running (D-053) [F95 p. 76; F95 p. 82].
AGE = "age"
CAPACITY = "capacity"


@dataclass(eq=False)
class Codelet:
    """One codelet on the rack, in the form "(codelet-name codelet-age codelet-urgency)"
    [F95 p. 77] with its generation tag [F95 p. 81]: `type` is one of the 23 catalogue names
    (D-051), `args` its parameters (ids and structure names, in order), `raw` its raw urgency
    "typically an integer value between 1 and 100" [F95 p. 81], `generation` 0 for the
    original codelets and one more per descent [F95 p. 81], `posted_at` the clock when it was
    hung (its age is the codelets run since, D-053), `by` who posted it (`start`, `slipnet`,
    `answer-schedule`, or the parent's type), `urgency` the adjusted urgency it carries on the
    rack (D-052), and `chance` the probability it had at the last draw (D-060), None before
    any. Codelets compare by identity."""

    type: str
    args: tuple = ()
    raw: float = 1
    generation: int = 0
    posted_at: int = 0
    by: str = START
    urgency: float = 1.0
    chance: float | None = None

    @property
    def label(self) -> str:
        """The Coderack strip's label, as the trace prints it [F95 p. 107 (img)] (D-051)."""
        return codelet_label(self.type)

    def age(self, clock: int) -> int:
        """How long the codelet has been on the rack, in codelets run (D-053) [F95 p. 76]."""
        return clock - self.posted_at


# ---------------------------------------------------------------------------------------------
# Urgency (D-052) and the making of codelets (D-054).


def raw_urgency(codelet_type: str, config) -> int:
    """The raw urgency of a codelet type, "the urgency associated with the codelet originally"
    [F95 p. 81], from `Config.urgencies` (D-052); find-answer's is set at posting (D-056)."""
    return config.as_dict("urgencies")[codelet_type]


def adjusted_urgency(raw: float, generation: int, config) -> float:
    """The adjusted urgency raw / rho^G [F95 p. 81 (img)], "where p is the urgency reduction
    coefficient and G is the generation number of the codelet" [F95 p. 81]; rho = 3 in the
    program [F95 p. 81]; at rho = 1 every codelet keeps its raw urgency [F95 p. 82]. A float,
    never below `Config.urgency_floor` (D-052). Below rho = 1 the power falls with the
    generation and underflows to 0 past about the thousandth generation (the `rho` bench's
    0.5, a chain of consumer C4's fights, D-101): the urgency is then infinite, the limit
    the law of temperature already takes (D-060 as revised for v3); above rho = 1 the same
    depth overflows the power, and the urgency is the floor."""
    try:
        divisor = config.rho ** generation
    except OverflowError:
        return config.urgency_floor
    if divisor == 0.0:
        return _INFINITY
    return max(config.urgency_floor, raw / divisor)


def make_codelet(codelet_type: str, config, clock: int, args=(), generation: int = 0,
                 by: str = START, raw=None) -> Codelet:
    """A codelet ready to hang: its raw urgency from the table unless given, its adjusted
    urgency from its generation (D-052). Codelets with no parent (the starting set, the
    Slipnet's top-down postings, the find-answer schedule) are generation 0 (D-054)."""
    if raw is None:
        raw = raw_urgency(codelet_type, config)
    return Codelet(type=codelet_type, args=tuple(args), raw=raw, generation=generation,
                   posted_at=clock, by=by, urgency=adjusted_urgency(raw, generation, config))


def child_of(parent: Codelet, codelet_type: str, config, clock: int, args=(),
             raw=None) -> Codelet:
    """A follow-up codelet, one generation on from its parent: "their children are of
    generation 1, their grandchildren are of generation 2, and so on" [F95 p. 81]; posted
    `by` the parent's type, at the type's raw urgency unless the parent sets one (D-052)."""
    return make_codelet(codelet_type, config, clock, args, parent.generation + 1,
                        by=parent.type, raw=raw)


# ---------------------------------------------------------------------------------------------
# The rack (D-053, D-055).


class Coderack:
    """The codelets waiting to run, in posting order [F95 p. 76] (D-053): `post` hangs a
    codelet and pushes the oldest off beyond the capacity of 150 [F95 p. 82], `remove_aged`
    eliminates those on the rack too long [F95 p. 76], `choose` draws one by urgency under
    the temperature exponent [F95 p. 77; F95 p. 92] and takes it off the rack. Removals are
    returned as (codelet, reason) pairs for the run loop to report. No codelet is ever
    removed for low urgency [F95 p. 77]."""

    def __init__(self, config):
        self.config = config
        self.codelets: list[Codelet] = []

    def __len__(self) -> int:
        return len(self.codelets)

    def post(self, codelet: Codelet) -> list:
        """Hang a codelet at the end of the rack; when the rack would hold more than
        `Config.coderack_capacity`, "the oldest codelets will eventually be 'pushed off' the
        Coderack in order to make room for the newcomers" [F95 p. 82], the oldest first.
        Returns the codelets pushed off, each with the reason `capacity` (D-053)."""
        self.codelets.append(codelet)
        removed = []
        while len(self.codelets) > self.config.coderack_capacity:
            removed.append((self.codelets.pop(0), CAPACITY))
        return removed

    def post_all(self, codelets) -> list:
        """Hang several codelets in order; the removals of every posting (D-053)."""
        removed = []
        for codelet in codelets:
            removed.extend(self.post(codelet))
        return removed

    def remove_aged(self, clock: int) -> list:
        """Eliminate every codelet older than `Config.max_codelet_age`: "A codelet that has
        been on the Coderack too long is eliminated, because chances are that it is no longer
        relevant to the continually evolving representation of the table." [F95 p. 76]
        Returns them, in rack order, each with the reason `age` (D-053)."""
        limit = self.config.max_codelet_age
        aged, kept = [], []
        for c in self.codelets:             # one pass, each codelet's age read once (D-112)
            if c.age(clock) > limit:
                aged.append(c)
            else:
                kept.append(c)
        self.codelets = kept
        return [(c, AGE) for c in aged]

    def chances(self, exponent: float) -> list:
        """The probability of each codelet at the exponent a, in rack order, without a draw:
        u^a over the total of u^a [F95 p. 92 (img)] (D-060); the share of the total at a = 1
        [F95 p. 77]. Empty for an empty rack."""
        return accel.chances_of([c.urgency for c in self.codelets], exponent)

    def choose(self, exponent: float, rng) -> Codelet | None:
        """Draw one codelet with probability proportional to urgency^a and take it off the
        rack: "It will choose the find-group-around codelet with a probability of .6, the
        find-neighbor codelet with a probability of .3, and the build-object codelet with a
        probability of .1." [F95 p. 77] at a = 1; at a = 6 the leader is nearly certain
        [F95 p. 93]. Equal urgencies stay a random choice [F95 p. 93]. One draw through the
        run's generator; every codelet's `chance` is recorded (D-060). None, without a draw,
        when the rack is empty."""
        if not self.codelets:
            return None
        chances = self.chances(exponent)
        for codelet, chance in zip(self.codelets, chances):
            codelet.chance = chance
        index = rng.weighted_index(chances)
        return self.codelets.pop(index)

    def total_urgency(self) -> float:
        """The "Total Coderack urgency" of French's tables [F95 p. 77]: the sum of the
        adjusted urgencies on the rack (D-052)."""
        return sum(c.urgency for c in self.codelets)

    def of_type(self, codelet_type: str) -> list:
        """The codelets of one type, in rack order (D-051)."""
        return [c for c in self.codelets if c.type == codelet_type]

    def counts(self) -> list:
        """How many codelets of each type are waiting, as (type, count) pairs in the strip's
        order, types with none left out [F95 p. 97] (D-051)."""
        return [(t, n) for t, _ in CODELET_TYPES if (n := len(self.of_type(t)))]

    def shares(self) -> list:
        """The screen's bar graph, "indicating the percentage of particular codelet types
        waiting to run" [F95 p. 97]: (label, percent) pairs in the strip's order for the types
        present, summing to 100 [F95 p. 107 (img)]."""
        total = len(self.codelets)
        return [(codelet_label(t), 100.0 * n / total) for t, n in self.counts()]


def initial_rack(config, clock: int = 0) -> Coderack:
    """The rack at the start of a run, "loaded up with its standard set of starting codelets"
    [F95 p. 97]: `Config.initial_codelets`, as (type, count) pairs in order, every one
    generation 0 at its raw urgency with no argument, posted by `start` (D-055) [F95 p. 65;
    F95 p. 98 (img)]."""
    rack = Coderack(config)
    for codelet_type, count in config.initial_codelets:
        for _ in range(count):
            rack.post(make_codelet(codelet_type, config, clock, by=START))
    return rack

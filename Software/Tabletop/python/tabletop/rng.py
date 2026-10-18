"""The random number generator: splitmix64, written out in full (DECISION D-064).

Every random choice in Tabletop goes through one `Rng`, held by the run and passed in; the
`random` module is never used. The generator is splitmix64 (Steele, Lea and Flood 2014; Vigna's
reference code), kept here with its published test vectors (`tests/test_rng.py`). Because it is a few lines of integer arithmetic, the same seed gives the
same numbers on every machine and every Python version, and the draw counter `draws` says how
many 64-bit outputs a run has taken, which the events and the fingerprint record.

French's book does not name his program's generator (the text layer of F95 was searched for
"random number", "generator" and "seed"); it says only that the codelets "are dispatched
stochastically from the Coderack" [F95 p. 55] and that "each codelet has an assigned urgency
that determines its probability of being executed" [F95 p. 92]. The choosing functions follow Mitchell's
Lisp Copycat (util.l): `weighted_index` is select-list-position, `choice`
is random-list-item, and `coin` is flip-coin, with the differences noted in their docstrings.
"""

from collections.abc import Sequence

MASK_64 = (1 << 64) - 1          # keep 64 bits, as unsigned C arithmetic would
TWO_64 = 1 << 64
GOLDEN_GAMMA = 0x9E3779B97F4A7C15  # the step added to the state on every draw


class Rng:
    """A splitmix64 generator; its whole state is one 64-bit integer (D-064).

    Fields: `seed`, the integer the run started from, kept so it can be printed; `state`, the
    64-bit state; `draws`, how many 64-bit outputs have been drawn so far.
    """

    def __init__(self, seed):
        if isinstance(seed, bool) or not isinstance(seed, int):
            raise TypeError(f"the seed must be an integer, got {seed!r}")
        self.seed = seed
        self.state = seed & MASK_64   # any integer is a valid seed, even 0
        self.draws = 0                # 64-bit outputs drawn so far

    def next_u64(self):
        """The next output: a whole number from 0 to 2**64 - 1 (D-064, splitmix64's step and
        mixing)."""
        self.state = z = (self.state + GOLDEN_GAMMA) & MASK_64
        z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & MASK_64
        z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & MASK_64
        self.draws += 1
        return z ^ (z >> 31)

    def below(self, n):
        """A whole number from 0 to n - 1, each equally likely (D-064).

        Taking next_u64() % n would favour small numbers slightly, because 2**64 is not a
        multiple of n. So outputs at or above the largest multiple of n are thrown away and
        another is drawn (rejection sampling).
        """
        if isinstance(n, bool) or not isinstance(n, int) or not 1 <= n <= TWO_64:
            raise ValueError(f"below(n) needs a whole number 1 <= n <= 2**64, got {n!r}")
        limit = TWO_64 - TWO_64 % n
        while True:
            x = self.next_u64()
            if x < limit:
                return x % n

    def random(self):
        """A float in [0, 1): the top 53 bits of one output, scaled (D-064)."""
        return (self.next_u64() >> 11) / (1 << 53)

    def choice(self, items):
        """One item of a list or tuple, each equally likely (util.l: random-list-item; D-064).

        Sets and dicts are refused: their order can change from one run to the next, and then
        the same seed would choose a different item.
        """
        if not isinstance(items, Sequence):
            raise TypeError(f"choice needs an ordered sequence, got {type(items).__name__}")
        if not items:
            raise ValueError("choice from an empty sequence")
        return items[self.below(len(items))]

    def weighted_index(self, weights):
        """The position of one weight, chosen with probability weight / total (util.l:
        select-list-position; D-064).

        Walk the list adding up the weights, and stop at the first running total that passes a
        random point in [0, total). If every weight is 0, choose a position uniformly, as the
        Lisp does. The Lisp draws an integer below an integer total; here the weights may be
        floats, so the point is random() * total.
        """
        if not weights:
            raise ValueError("weighted_index of an empty list")
        if any(w < 0 for w in weights):
            raise ValueError(f"weights must not be negative: {list(weights)!r}")
        total = 0
        for w in weights:
            total += w
        if total == 0:
            return self.below(len(weights))
        point = self.random() * total
        running = 0
        for position, w in enumerate(weights):
            running += w
            if running > point:
                return position
        raise AssertionError("unreachable: the last running total is the total")

    def coin(self, p=0.5):
        """True with probability p (util.l: flip-coin; D-064).

        As in the Lisp, p >= 1 gives True without drawing a number. Unlike the Lisp, p is not
        first rounded to thousandths.
        """
        if p >= 1:
            return True
        return self.random() < p


def run_seeds(batch_seed, count):
    """The seeds of the runs of a batch: the first `count` outputs of an Rng seeded with
    `batch_seed`, so any run of the batch can be replayed alone, from its own seed (D-064)."""
    rng = Rng(batch_seed)
    return [rng.next_u64() for _ in range(count)]

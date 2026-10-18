"""The run's only random number generator ([M86 p. 42]; DECISION D-007).

Every random choice in Seek-Whence goes through one `Rng` made from the run's seed and
passed in; the `random` module's global state is never used, so a run repeats exactly from
its seed. `Rng` wraps `random.Random` but draws only through its `random()` method, the one
whose sequence Python guarantees for a given seed across versions (the module's other
algorithms, `choice` and `randrange` among them, may change). The three draws the program
needs are built on that one fraction.
"""

import random
from collections.abc import Sequence


class Rng:
    """A seeded generator wrapping `random.Random`, the one every draw goes through
    (DECISION D-007).

    Fields: `seed`, the integer the run started from, kept so it can be printed; `draws`,
    how many fractions have been drawn so far, which tests use to check that a function
    draws as many numbers as it should.
    """

    def __init__(self, seed: int):
        if isinstance(seed, bool) or not isinstance(seed, int):
            raise TypeError(f"the seed must be an integer, got {seed!r}")
        if seed < 0:
            # `random.Random(-n)` draws the same numbers as `random.Random(n)`, so a
            # negative seed would silently repeat another run (distinct seeds, distinct
            # runs; found by the audit of item 24; DECISION D-069).
            raise ValueError(f"the seed must be a nonnegative integer, got {seed!r}")
        self.seed = seed
        self.draws = 0
        self._random = random.Random(seed)

    def random(self) -> float:
        """A fraction in [0, 1), the one draw everything else is built on (the draws of
        DECISIONS D-007, D-020, D-025 and D-058)."""
        self.draws += 1
        return self._random.random()

    def below(self, n: int) -> int:
        """A whole number from 0 to n - 1, each equally likely: the fraction scaled to n
        (the uniform draws of DECISIONS D-020, D-025 and D-058)."""
        if isinstance(n, bool) or not isinstance(n, int) or n < 1:
            raise ValueError(f"below(n) needs a whole number n >= 1, got {n!r}")
        return int(self.random() * n)

    def choice(self, items: Sequence):
        """One item of a list or tuple, each equally likely (DECISIONS D-020, D-025, D-058).

        Sets and dicts are refused: their order can change from one run to the next, and
        then the same seed would choose a different item.
        """
        if not isinstance(items, Sequence) or isinstance(items, (str, bytes)):
            raise TypeError(f"choice needs an ordered sequence, got {type(items).__name__}")
        if not items:
            raise ValueError("choice from an empty sequence")
        return items[self.below(len(items))]

    def weighted_index(self, weights: Sequence[int]) -> int:
        """The position of one weight, chosen with probability weight / total (DECISION
        D-007).

        The weights are non-negative integers (Seek-Whence's urgencies are integer weights
        [M86 p. 42]). Walk the list adding up the weights and stop at the first running total
        that passes a random point in [0, total); a weight of 0 is never chosen. If every
        weight is 0, choose a position uniformly.
        """
        if not weights:
            raise ValueError("weighted_index of an empty list")
        for w in weights:
            if isinstance(w, bool) or not isinstance(w, int) or w < 0:
                raise ValueError(f"weights must be non-negative integers: {list(weights)!r}")
        total = sum(weights)
        if total == 0:
            return self.below(len(weights))
        point = self.random() * total
        running = 0
        for position, w in enumerate(weights):
            running += w
            if running > point:
                return position
        return len(weights) - 1  # only if rounding put the point at the total

    def weighted_choice(self, items: Sequence, weights: Sequence[int]):
        """One item, chosen with probability proportional to its integer weight
        (DECISION D-007)."""
        if len(items) != len(weights):
            raise ValueError(f"{len(items)} items but {len(weights)} weights")
        return items[self.weighted_index(weights)]

"""The shared RNG of oracle mode: splitmix64 with rejection sampling.

Port of the RNG section of src/oracle.lisp, bit for bit (the specification
is in that file's header and in PORTING_NOTES.md, "Oracle hooks").  In oracle
mode the 7 (random n) call sites (coderack.lisp x2, codelets.lisp x5) draw
from it; the Python calls Rng.random at the same 7 places.
"""

MASK_64 = (1 << 64) - 1
TWO_64 = 1 << 64


class Rng:
    """oracle.lisp: *oracle-rng-state*, *oracle-rng-draws*, *oracle-rng-sink*.

    state: the splitmix64 state, an unsigned 64-bit integer.
    draws: the number of 64-bit outputs drawn since the last seed.
    sink:  None, or a function of (n, value) told of every random(n).
    """

    def __init__(self, seed=0):
        self.sink = None
        self.seed(seed)

    def seed(self, seed):
        """oracle.lisp: oracle-seed.  The state becomes seed mod 2^64."""
        self.state = seed & MASK_64
        self.draws = 0
        return seed

    def next_u64(self):
        """oracle.lisp: oracle-next-u64.  The next splitmix64 output."""
        self.state = z = (self.state + 0x9E3779B97F4A7C15) & MASK_64
        z = ((z ^ (z >> 30)) * 0xBF58476D1CE4E5B9) & MASK_64
        z = ((z ^ (z >> 27)) * 0x94D049BB133111EB) & MASK_64
        self.draws += 1
        return z ^ (z >> 31)

    def random(self, n):
        """oracle.lisp: random.  An integer in [0, n) by rejection sampling.

        n must be an integer with 1 <= n <= 2^64.  random(1) still draws.
        """
        if isinstance(n, bool) or not isinstance(n, int) or not 1 <= n <= TWO_64:
            raise ValueError(f"oracle RANDOM: N must be an integer in [1, 2^64], got {n!r}")
        limit = TWO_64 - TWO_64 % n
        while True:
            x = self.next_u64()
            if x < limit:
                v = x % n
                if self.sink is not None:
                    self.sink(n, v)
                return v

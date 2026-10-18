# cython: language_level=3
# cython: boundscheck=False, wraparound=False
"""The compiled generator: splitmix64 in native 64-bit arithmetic, mirroring `tabletop/rng.py`
function for function (D-064, D-109). The algorithm is the pure engine's, so the two
engines draw identical streams from the same seed: the published vectors of
`tests/test_rng.py` pass on both. C unsigned arithmetic wraps modulo 2**64, which is what
the Python's `& MASK_64` after every `+` and `*` computes, so the masks are not needed here.

The build embeds its stamp, flags, compiler and Cython version as C string macros (set by
`setup.py`; D-110), read below into module attributes for `tabletop.accel`.
"""

from collections.abc import Sequence

cdef extern from *:
    """
    #ifndef TABLETOP_BUILD_STAMP
    #define TABLETOP_BUILD_STAMP "unstamped"
    #endif
    #ifndef TABLETOP_BUILD_FLAGS
    #define TABLETOP_BUILD_FLAGS ""
    #endif
    #ifndef TABLETOP_BUILD_COMPILER
    #define TABLETOP_BUILD_COMPILER ""
    #endif
    #ifndef TABLETOP_CYTHON_VERSION
    #define TABLETOP_CYTHON_VERSION ""
    #endif
    /* splitmix64's constants (Vigna's reference code), above 2**63, so written here in C:
       Cython would keep such literals as Python ints (rng.py: GOLDEN_GAMMA and the two
       multipliers of next_u64). */
    #define TT_GOLDEN_GAMMA 0x9E3779B97F4A7C15ULL
    #define TT_MIX_1 0xBF58476D1CE4E5B9ULL
    #define TT_MIX_2 0x94D049BB133111EBULL
    """
    const char *TABLETOP_BUILD_STAMP
    const char *TABLETOP_BUILD_FLAGS
    const char *TABLETOP_BUILD_COMPILER
    const char *TABLETOP_CYTHON_VERSION
    unsigned long long TT_GOLDEN_GAMMA
    unsigned long long TT_MIX_1
    unsigned long long TT_MIX_2

cdef const char *_stamp = TABLETOP_BUILD_STAMP
cdef const char *_flags = TABLETOP_BUILD_FLAGS
cdef const char *_compiler = TABLETOP_BUILD_COMPILER
cdef const char *_cython = TABLETOP_CYTHON_VERSION
BUILD_STAMP = _stamp.decode("utf-8")
BUILD_FLAGS = _flags.decode("utf-8")
BUILD_COMPILER = _compiler.decode("utf-8")
CYTHON_VERSION = _cython.decode("utf-8")

MASK_64 = (1 << 64) - 1          # rng.py: MASK_64 (kept for parity; the C arithmetic wraps)
TWO_64 = 1 << 64                 # rng.py: TWO_64
GOLDEN_GAMMA = 0x9E3779B97F4A7C15  # rng.py: GOLDEN_GAMMA (the C step is TT_GOLDEN_GAMMA)


cdef class Rng:
    """A splitmix64 generator; its whole state is one 64-bit integer (D-064). Mirrors
    `tabletop/rng.py`, class `Rng`: the same fields `seed`, `state` and `draws`, the same
    methods, the same outputs and draw counts."""

    cdef public object seed
    cdef public unsigned long long state
    cdef public unsigned long long draws

    def __init__(self, seed):
        # mirrors rng.py, Rng.__init__
        if isinstance(seed, bool) or not isinstance(seed, int):
            raise TypeError(f"the seed must be an integer, got {seed!r}")
        self.seed = seed
        self.state = seed & MASK_64   # any integer is a valid seed, even 0
        self.draws = 0                # 64-bit outputs drawn so far

    cpdef unsigned long long next_u64(self):
        """The next output: a whole number from 0 to 2**64 - 1 (D-064, splitmix64's step
        and mixing). Mirrors rng.py, Rng.next_u64, in C unsigned arithmetic."""
        cdef unsigned long long z
        self.state += TT_GOLDEN_GAMMA
        z = self.state
        z = (z ^ (z >> 30)) * TT_MIX_1
        z = (z ^ (z >> 27)) * TT_MIX_2
        self.draws += 1
        return z ^ (z >> 31)

    def below(self, n):
        """A whole number from 0 to n - 1, each equally likely, by rejection sampling
        (D-064). Mirrors rng.py, Rng.below: the same outputs rejected, so the same draws.
        The Python's limit is 2**64 - 2**64 % n; here `rem` is 2**64 % n computed in 64
        bits as (2**64 - n) % n, and an output x is kept when x < 2**64 - rem, which is
        every output when rem is 0 (n a power of two, or n = 2**64)."""
        cdef unsigned long long m, rem, x
        if isinstance(n, bool) or not isinstance(n, int) or not 1 <= n <= TWO_64:
            raise ValueError(f"below(n) needs a whole number 1 <= n <= 2**64, got {n!r}")
        if n == TWO_64:
            return self.next_u64()
        m = n
        rem = (0 - m) % m
        while True:
            x = self.next_u64()
            if rem == 0 or x < 0 - rem:
                return x % m

    cpdef double random(self):
        """A float in [0, 1): the top 53 bits of one output, scaled (D-064). Mirrors
        rng.py, Rng.random; a division by 2**53 is exact in both."""
        return (self.next_u64() >> 11) / 9007199254740992.0

    def choice(self, items):
        """One item of a list or tuple, each equally likely (util.l: random-list-item;
        D-064). Mirrors rng.py, Rng.choice: sets and dicts are refused."""
        if not isinstance(items, Sequence):
            raise TypeError(f"choice needs an ordered sequence, got {type(items).__name__}")
        if not items:
            raise ValueError("choice from an empty sequence")
        return items[self.below(len(items))]

    def weighted_index(self, weights):
        """The position of one weight, chosen with probability weight / total (util.l:
        select-list-position; D-064). Mirrors rng.py, Rng.weighted_index: the same running
        sum in Python arithmetic, so the same float result for the same weights."""
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
        """True with probability p (util.l: flip-coin; D-064). Mirrors rng.py, Rng.coin:
        p >= 1 gives True without drawing a number."""
        if p >= 1:
            return True
        return self.random() < p


def run_seeds(batch_seed, count):
    """The seeds of the runs of a batch: the first `count` outputs of an Rng seeded with
    `batch_seed` (D-064). Mirrors rng.py, run_seeds."""
    rng = Rng(batch_seed)
    return [rng.next_u64() for _ in range(count)]

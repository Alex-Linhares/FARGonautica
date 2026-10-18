"""The statistics the benches report, in one place (D-072): a z score for a count against
a known chance, the two-sample z of our proportion against French's, a chi-square test of
homogeneity between our answer counts and his, with its p-value and limit by the
Wilson-Hilferty approximation, and the false-alarm chance of a k-standard-error test.

French's numbers are themselves samples: "Almost all of the configurations were run fifty
times. No configuration was ever run fewer than forty times." [F95 p. 114], so a frequency
of his is one estimate of a chance, not the chance itself, and the comparison with ours is
a two-sample one. Nothing here reads the engine; the functions take counts and return
numbers (D-072).
"""

import math
from statistics import NormalDist

FRENCH_RUNS = 50   # French's n, "fifty times" [F95 p. 114]; "a total of 50 times" [FCCA p. 392]


def normal_tail(z: float) -> float:
    """The chance that a standard normal value exceeds `z` (one-sided) (D-072)."""
    return 0.5 * math.erfc(z / math.sqrt(2.0))


def z_score(count: int, n: int, p: float) -> float:
    """How many standard errors `count` successes in `n` trials lie from the expected
    n p, when each trial succeeds with chance `p` strictly between 0 and 1 (D-072)."""
    if not 0.0 < p < 1.0:
        raise ValueError(f"p must be strictly between 0 and 1, not {p!r}")
    return (count - n * p) / math.sqrt(n * p * (1.0 - p))


def two_sample_z(count1: float, n1: float, count2: float, n2: float) -> float:
    """The z score of the difference between two observed proportions, count1 / n1 and
    count2 / n2, both samples (ours and French's fifty [F95 p. 114]); the common chance
    is estimated from both together. 0 when both are 0 or both are 1 (D-072)."""
    pooled = (count1 + count2) / (n1 + n2)
    if pooled <= 0.0 or pooled >= 1.0:
        return 0.0
    error = math.sqrt(pooled * (1.0 - pooled) * (1.0 / n1 + 1.0 / n2))
    return (count1 / n1 - count2 / n2) / error


def french_count(frequency: float, runs: int = FRENCH_RUNS) -> float:
    """French's count of runs behind a printed percentage: frequency x runs / 100, with
    his fifty runs [F95 p. 114]; fractional for the odd percentages his graphs print
    (D-072)."""
    return frequency * runs / 100.0


def false_alarm(k: float) -> float:
    """The chance that a correct program lands more than `k` standard errors from its
    expected value, two-sided, by the normal approximation (D-072): 0.046 at k = 2, about
    one in 15,800 at k = 4."""
    return 2.0 * normal_tail(k)


def chi_square_homogeneity(counts1: list, counts2: list) -> tuple:
    """The chi-square statistic of homogeneity between two samples counted over the same
    cells (ours and French's over his answers and "other", D-072), with its degrees of
    freedom: the expected count of each cell is the cell's share of both samples together
    times the sample's size; cells empty in both samples are dropped. Returns
    (statistic, degrees of freedom); (0.0, 0) when nothing can be compared."""
    if len(counts1) != len(counts2):
        raise ValueError("the two samples need the same cells")
    n1, n2 = sum(counts1), sum(counts2)
    total = n1 + n2
    if total <= 0 or n1 <= 0 or n2 <= 0:
        return 0.0, 0
    statistic, cells = 0.0, 0
    for c1, c2 in zip(counts1, counts2):
        both = c1 + c2
        if both <= 0:
            continue
        cells += 1
        for count, n in ((c1, n1), (c2, n2)):
            expected = both * n / total
            statistic += (count - expected) ** 2 / expected
    return statistic, max(0, cells - 1)


def chi_square_limit(df: int, alpha: float = 1e-4) -> float:
    """The value a chi-square statistic with `df` degrees of freedom exceeds with chance
    `alpha`, by the Wilson-Hilferty approximation df (1 - h + z sqrt(h))^3 with h = 2 /
    (9 df) and z the normal point exceeded with chance alpha (D-072). 0 at df 0."""
    if df <= 0:
        return 0.0
    z = NormalDist().inv_cdf(1.0 - alpha)
    h = 2.0 / (9.0 * df)
    return df * (1.0 - h + z * math.sqrt(h)) ** 3


def chi_square_p(statistic: float, df: int) -> float:
    """The chance that a chi-square variable with `df` degrees of freedom exceeds
    `statistic`, by the Wilson-Hilferty approximation inverted: z = ((x / df)^(1/3) -
    (1 - h)) / sqrt(h) (D-072). 1.0 at df 0, since there is nothing to test."""
    if df <= 0:
        return 1.0
    if statistic <= 0.0:
        return 1.0
    h = 2.0 / (9.0 * df)
    z = ((statistic / df) ** (1.0 / 3.0) - (1.0 - h)) / math.sqrt(h)
    return normal_tail(z)

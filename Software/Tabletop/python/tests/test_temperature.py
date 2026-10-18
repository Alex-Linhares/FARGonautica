"""The one law of temperature (D-060): the exponent a(T) through French's two points
[F95 p. 92], the softened weights, and the yes-or-no form, deterministic at T = 0 and
uniform at T = 100 [F95 p. 93]. The temperature's own computation (the formula, its
windows and its schedule, D-047 to D-050) is tested in `test_run.py`.
"""

import math

import pytest

from tabletop import temperature as TP
from tabletop.config import PROFILES
from tabletop.rng import Rng
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]


def test_the_exponent_runs_from_6_at_0_to_0_1_at_100_and_is_linear():
    """a = 6 at T = 0 and 0.1 at T = 100 [F95 p. 92]; a(50) = 3.05, the only line
    through the two points (D-060); T is clipped."""
    assert TP.exponent_at(0, CFG) == 6.0
    assert TP.exponent_at(100, CFG) == pytest.approx(0.1)
    assert TP.exponent_at(50, CFG) == pytest.approx(3.05)
    assert TP.exponent_at(80, CFG) == pytest.approx(1.28)
    assert TP.exponent_at(-5, CFG) == 6.0 and TP.exponent_at(250, CFG) == pytest.approx(0.1)


def test_softened_weights_survive_an_urgency_explosion():
    """An urgency that would overflow a float to the exponent (the `rho` bench below 1,
    [F95 p. 82]) is scaled by the largest weight first: the same chances, no OverflowError
    (D-060); ordinary weights are untouched."""
    huge = TP.softened_weights([1e300, 1e299, 0.0], 6.0)
    assert all(math.isfinite(w) for w in huge) and huge[0] > huge[1] > huge[2] == 0.0
    assert huge[1] / huge[0] == pytest.approx((1e299 / 1e300) ** 6)
    assert TP.softened_weights([1.0, 0.7, 0.4], 6.0) == [1.0 ** 6, 0.7 ** 6, 0.4 ** 6]


def test_an_infinite_urgency_takes_the_limit_of_the_rule():
    """An urgency past float range (generation 1,024 and on at `rho` 0.5, reached by one run
    of the `rho` bench on v3's defaults): the infinite weights share every chance, the
    limit of the weights over the largest, where the rule gave nan (D-060 as revised, D-114)."""
    inf = float("inf")
    assert TP.softened_weights([inf, 5.0, inf, 0.0], 6.7) == [1.0, 0.0, 1.0, 0.0]
    assert TP.chances_of([inf, 5.0, inf], 6.7) == [0.5, 0.0, 0.5]


def test_softened_weights_are_sharp_at_6_and_flat_at_a_tenth():
    """"approaches determinism as the temperature approaches 0, is completely unbiased
    ... at 100" [F95 p. 93] (D-060)."""
    sharp = TP.softened_weights([1.0, 0.7, 0.4], 6.0)
    flat = TP.softened_weights([1.0, 0.7, 0.4], 0.1)
    assert sharp[0] / sum(sharp) > 0.85
    assert max(flat) / sum(flat) < 0.37
    assert TP.softened_weights([2.0, 0.0, -1.0], 2.0) == [4.0, 0.0, 0.0]


def test_the_yes_or_no_form_of_the_law():
    """q^a / (q^a + (1 - q)^a): q at a = 1, a half at q = 0.5 for every a, certain at the
    ends, sharpened at a = 6 and flattened at a = 0.1 (D-060)."""
    for q in (0.1, 0.6, 0.95):
        assert TP.pass_probability(q, 1.0) == pytest.approx(q)
    for a in (6.0, 1.0, 0.1):
        assert TP.pass_probability(0.5, a) == pytest.approx(0.5)
    assert TP.pass_probability(0.0, 0.1) == 0.0 and TP.pass_probability(1.0, 0.1) == 1.0
    assert TP.pass_probability(0.95, 6.0) > 0.9999
    assert TP.pass_probability(0.1, 6.0) < 1e-5
    assert 0.4 < TP.pass_probability(0.1, 0.1) < 0.5
    assert TP.pass_probability(0.6, 3.5) == pytest.approx(0.805, abs=0.001)  # D-024's "about 0.8"


def test_passes_draws_one_coin_at_the_softened_chance_and_none_when_certain():
    """Seeded: 2,000 coins at q = 0.6 and a = 1.28 land within four binomial standard
    deviations of the law's probability; a certain outcome spends no draw (D-060)."""
    rng = Rng(7)
    p = TP.pass_probability(0.6, 1.28)
    n = 2000
    hits = sum(TP.passes(0.6, 1.28, rng) for _ in range(n))
    assert rng.draws == n
    assert abs(hits - n * p) <= 4 * math.sqrt(n * p * (1 - p)), (hits, p)
    assert TP.passes(1.0, 0.1, rng) is True and TP.passes(0.0, 6.0, rng) is False
    assert rng.draws == n

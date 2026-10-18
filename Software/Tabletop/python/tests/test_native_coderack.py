"""The compiled Coderack chances (`tabletop/_native/coderack.pyx`, D-112) against the
specification (`tabletop/temperature.py`, `chances_of`): the seam serves it on the
compiled engine, the Coderack draws through it, and on random weights and exponents (the
overflowing powers of the `rho` bench included) the two functions agree exactly, which is
what keeps the compiled engine's runs the pure engine's bit for bit. Every test skips,
with a reason, when the python engine was asked for explicitly.
"""

import importlib
import random

import pytest

from tabletop import accel
from tabletop import coderack as CR
from tabletop import temperature as TP
from tabletop.config import PROFILES

CFG = PROFILES["standard"]

pytestmark = pytest.mark.skipif(
    accel.info()["engine"] != "compiled",
    reason="the compiled engine is not the one running (TABLETOP_ENGINE=python was requested "
           "explicitly)")


def native_module():
    return importlib.import_module("tabletop._native.coderack")


def test_the_compiled_engine_serves_the_native_chances():
    """`accel.chances_of` is the compiled function, and the Coderack reads through the
    seam (D-112)."""
    assert accel.chances_of.__module__ == "tabletop._native.coderack"
    assert accel.chances_of is not TP.chances_of
    assert "coderack" in accel.info()["modules"]
    mod = native_module()
    assert mod.BUILD_STAMP == accel.info()["build_stamp"]
    assert CR.accel is accel


def weights_and_exponents(seed: int, trials: int):
    rnd = random.Random(seed)
    pool = (0.0, 1.0, 0.1, 100.0, 20 / 3, 20 / 9, 1e-9, 1e300, 1e-300)
    for _ in range(trials):
        k = rnd.choice((1, 2, 3, 10, 150))
        weights = [rnd.choice(pool) if rnd.random() < 0.3 else rnd.random() * 100
                   for _ in range(k)]
        yield weights, rnd.choice((0.1, 1.0, 6.0, 3.05, rnd.random() * 6))


def test_the_chances_agree_exactly_with_the_specification():
    """Random racks, random exponents: the same list of floats, element for element,
    the total summed as Python's `sum` sums it (D-112)."""
    checked = 0
    for weights, exponent in weights_and_exponents(5, 3000):
        expected = TP.chances_of(weights, exponent)
        got = accel.chances_of(weights, exponent)
        assert got == expected and all(type(c) is float for c in got), (weights, exponent)
        checked += 1
    assert checked == 3000


def test_the_softened_weights_agree_exactly_with_the_specification():
    mod = native_module()
    for weights, exponent in weights_and_exponents(6, 1000):
        assert mod.softened_weights(weights, exponent) == TP.softened_weights(weights, exponent)


def test_the_overflow_rule_and_the_degenerate_racks():
    """A power that overflows divides every weight by the largest first (the `rho` bench's
    explosion); zeros share equally; an empty rack has no chances (D-060, D-112)."""
    exploding = [1e300, 1e200, 5.0, 0.0]
    assert accel.chances_of(exploding, 6.0) == TP.chances_of(exploding, 6.0)
    assert sum(accel.chances_of(exploding, 6.0)) == pytest.approx(1.0)
    assert accel.chances_of([0.0, 0.0, 0.0], 2.0) == [1 / 3, 1 / 3, 1 / 3]
    assert accel.chances_of([], 1.0) == []
    assert accel.chances_of([7.0], 0.1) == [1.0]
    infinite = [float("inf"), 1e300, float("inf"), 2.0]           # the limit (D-114)
    assert accel.chances_of(infinite, 6.7) == TP.chances_of(infinite, 6.7) == [0.5, 0.0, 0.5, 0.0]


def test_the_rack_records_the_same_chances_on_both_engines():
    """A rack of mixed urgencies and generations: `Coderack.chances` through the seam is
    the specification's list, and `choose` records it on the codelets (D-060)."""
    rack = CR.Coderack(CFG)
    types = list(CFG.as_dict("urgencies"))
    for i, generation in enumerate((0, 1, 2, 3, 0, 1, 4, 0)):
        rack.post(CR.make_codelet(types[i % len(types)], CFG, i, generation=generation))
    for a in (0.1, 1.0, 3.3, 6.0):
        urgencies = [c.urgency for c in rack.codelets]
        assert rack.chances(a) == TP.chances_of(urgencies, a)
    chosen = rack.choose(1.0, accel.Rng(7))
    assert chosen is not None and all(c.chance is not None for c in rack.codelets)

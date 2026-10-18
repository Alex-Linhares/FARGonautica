"""The run's only generator, splitmix64 (D-064, D-109): splitmix64's published
vectors, same seed same draws, the draw counter, and the choosing functions' frequencies
(fixed seeds, so a test passes or fails every time), on both engines: the pure `Rng` of
`tabletop.rng` (the specification) and the compiled one of `tabletop.accel` when the
compiled engine is running (D-107; skipped, with a reason, only when the python engine was
asked for explicitly). The two draw identical streams (`test_both_engines_draw_the_same`).

The expected outputs were computed by an independent implementation of splitmix64 in SBCL
(the Numbo port's oracle, in FARGonautica). Seed 0's
first output, 0xE220A8397B1DCDAF, is also the value of Vigna's reference splitmix64.c.
"""

import pytest

from tabletop import accel
from tabletop import rng as pure

ENGINES = ["python", "compiled"]


def _engine(name):
    """The `Rng` and `run_seeds` of one engine: the pure module, or the compiled engine's
    through the seam; the compiled one is skipped when the python engine was requested."""
    if name == "python":
        return pure.Rng, pure.run_seeds
    if accel.info()["engine"] != "compiled":
        pytest.skip("the compiled engine is not the one running (TABLETOP_ENGINE=python was "
                    "requested explicitly)")
    return accel.Rng, accel.run_seeds


@pytest.fixture(params=ENGINES, autouse=True)
def Rng(request):
    """The generator class under test, one engine per parameter."""
    return _engine(request.param)[0]


@pytest.fixture
def run_seeds(Rng):
    return accel.run_seeds if Rng is accel.Rng else pure.run_seeds

FIRST_OUTPUTS = {
    0: [0xE220A8397B1DCDAF, 0x6E789E6AA1B965F4, 0x06C45D188009454F,
        0xF88BB8A8724C81EC, 0x1B39896A51A8749B],
    1: [0x910A2DEC89025CC1, 0xBEEB8DA1658EEC67, 0xF893A2EEFB32555E,
        0x71C18690EE42C90B, 0x71BB54D8D101B5B9],
    18: [0x1120B3D00955F032, 0xB6D46A242257C018, 0x6E372283ACB06862,
         0xB4133692FB1847A0, 0x1EFEDE435C3B0B96],
}


@pytest.mark.parametrize("seed", sorted(FIRST_OUTPUTS))
def test_outputs_match_the_published_vectors(seed, Rng):
    rng = Rng(seed)
    assert [rng.next_u64() for _ in range(5)] == FIRST_OUTPUTS[seed]
    assert rng.draws == 5


BELOW_N = [1, 2, 3, 5, 7, 10, 13, 100, 600, 1000, 6, 31, 64, 1000000007, 4294967296,
           9223372036854775809, 18446744073709551615, 9223372036854775809, 2,
           18446744073709551616]
BELOW_VALUES = {
    0: ([0, 0, 1, 4, 2, 0, 10, 40, 299, 390, 1, 24, 59, 463217319, 13389081,
         9018883062403043925, 14109521515791744902, 3775962213208117092, 0,
         15781000307351985879], 21),
    1: ([0, 1, 0, 0, 5, 8, 4, 33, 120, 950, 3, 15, 0, 312715763, 1867274152,
         3081251696030599739, 11904322950028659555, 1216750802008901446, 0,
         9147370558249537485], 23),
}


@pytest.mark.parametrize("seed", sorted(BELOW_VALUES))
def test_below_matches_the_oracle_including_rejections(seed, Rng):
    values, draws = BELOW_VALUES[seed]
    rng = Rng(seed)
    assert [rng.below(n) for n in BELOW_N] == values
    assert rng.draws == draws          # more draws than calls: some outputs were rejected


def test_a_seed_is_reduced_modulo_2_to_the_64(Rng):
    assert Rng(-1).state == Rng(2**64 - 1).state
    assert Rng(2**64 + 18).next_u64() == FIRST_OUTPUTS[18][0]


@pytest.mark.parametrize("bad", [1.5, "1", None, True])
def test_a_seed_must_be_an_integer(bad, Rng):
    with pytest.raises(TypeError):
        Rng(bad)


@pytest.mark.parametrize("bad", [0, -1, 2**64 + 1, 2.0, True])
def test_below_rejects_bad_bounds(bad, Rng):
    with pytest.raises(ValueError):
        Rng(0).below(bad)


def test_below_1_still_draws(Rng):
    rng = Rng(5)
    assert rng.below(1) == 0
    assert rng.draws == 1


def test_random_is_in_the_unit_interval_and_uses_53_bits(Rng):
    rng = Rng(0)
    assert rng.random() == (FIRST_OUTPUTS[0][0] >> 11) / 2**53
    values = [rng.random() for _ in range(10_000)]
    assert all(0 <= v < 1 for v in values)
    # the mean of 10,000 uniforms has s.d. 1 / sqrt(12 x 10,000) = 0.0029; four s.d. is 0.0115
    assert abs(sum(values) / len(values) - 0.5) < 0.0116


def test_below_is_uniform_by_a_chi_square_test(Rng):
    """6000 draws of a die. With 5 degrees of freedom, a chi-square statistic above 20.52
    would happen by chance with probability 0.001; the seed is fixed, so the test cannot
    fail at random."""
    rng = Rng(2026)
    counts = [0] * 6
    for _ in range(6000):
        counts[rng.below(6)] += 1
    chi_square = sum((c - 1000) ** 2 / 1000 for c in counts)
    assert chi_square < 20.52


def test_same_seed_same_draws_and_the_counter_agrees(Rng):
    a, b, c = Rng(42), Rng(42), Rng(43)
    xs = [a.next_u64() for _ in range(100)]
    assert xs == [b.next_u64() for _ in range(100)]
    assert xs != [c.next_u64() for _ in range(100)]
    assert [a.random() for _ in range(50)] == [b.random() for _ in range(50)]
    assert [a.below(10) for _ in range(50)] == [b.below(10) for _ in range(50)]
    assert [a.choice(["x", "y", "z"]) for _ in range(50)] == \
        [b.choice(["x", "y", "z"]) for _ in range(50)]
    assert [a.weighted_index([1, 2, 3]) for _ in range(50)] == \
        [b.weighted_index([1, 2, 3]) for _ in range(50)]
    assert [a.coin(0.4) for _ in range(50)] == [b.coin(0.4) for _ in range(50)]
    assert a.draws == b.draws >= 350      # at least one draw per call; `below` may reject


def test_choice_takes_items_of_a_sequence(Rng):
    rng = Rng(3)
    picks = [rng.choice(["a", "b", "c"]) for _ in range(300)]
    assert set(picks) == {"a", "b", "c"}
    assert Rng(3).choice(("x",)) == "x"


@pytest.mark.parametrize("unordered", [{1, 2, 3}, frozenset("ab"), {"a": 1}])
def test_choice_refuses_unordered_containers(unordered, Rng):
    with pytest.raises(TypeError):
        Rng(0).choice(unordered)


def test_choice_of_nothing_is_an_error(Rng):
    with pytest.raises(ValueError):
        Rng(0).choice([])


def test_weighted_index_follows_the_weights(Rng):
    rng = Rng(11)
    counts = [0, 0, 0, 0]
    for _ in range(10_000):
        counts[rng.weighted_index([1, 0, 3, 6])] += 1
    assert counts[1] == 0                         # a weight of 0 is never chosen
    for count, share in zip(counts, [0.1, 0.0, 0.3, 0.6]):
        # the widest s.d. of a share here is sqrt(0.6 x 0.4 / 10,000) = 0.0049; 0.02 is four s.d.
        assert abs(count / 10_000 - share) < 0.02


def test_weighted_index_with_all_zero_weights_is_uniform(Rng):
    rng = Rng(12)
    picks = {rng.weighted_index([0, 0, 0]) for _ in range(200)}
    assert picks == {0, 1, 2}


def test_weighted_index_draws_exactly_once(Rng):
    rng = Rng(13)
    rng.weighted_index([2.5, 0.5])
    assert rng.draws == 1


def test_weighted_index_rejects_bad_weights(Rng):
    with pytest.raises(ValueError):
        Rng(0).weighted_index([])
    with pytest.raises(ValueError):
        Rng(0).weighted_index([1, -1])


def test_coin(Rng):
    rng = Rng(14)
    heads = sum(rng.coin(0.3) for _ in range(10_000))
    assert abs(heads / 10_000 - 0.3) < 0.02      # s.d. sqrt(0.3 x 0.7 / 10,000) = 0.0046; four s.d. 0.018
    assert not any(rng.coin(0) for _ in range(100))


def test_a_certain_coin_does_not_draw(Rng):
    rng = Rng(15)
    assert rng.coin(1) and rng.coin(1.5)
    assert rng.draws == 0


def test_run_seeds_are_the_outputs_of_the_batch_generator(run_seeds):
    assert run_seeds(0, 3) == FIRST_OUTPUTS[0][:3]
    assert run_seeds(7, 10)[:4] == run_seeds(7, 4)    # a longer batch starts the same way
    assert len(set(run_seeds(7, 1000))) == 1000


# ---- the two engines against each other (D-109) ----------------------------------------------


def test_both_engines_draw_the_same():
    """The compiled generator is the pure one's algorithm in native arithmetic (D-109): the
    same outputs, the same rejections in `below`, the same floats, the same draw counts."""
    native = _engine("compiled")[0]
    for seed in (0, 1, 18, -1, 2**64 + 18, 2026):
        a, b = native(seed), pure.Rng(seed)
        assert [a.next_u64() for _ in range(200)] == [b.next_u64() for _ in range(200)]
        assert [a.below(n) for n in BELOW_N * 3] == [b.below(n) for n in BELOW_N * 3]
        assert [a.random() for _ in range(200)] == [b.random() for _ in range(200)]
        weights = [0.1, 2.5, 0, 3.3, 1e-9, 7]
        assert [a.weighted_index(weights) for _ in range(100)] == \
            [b.weighted_index(weights) for _ in range(100)]
        assert [a.coin(0.3) for _ in range(100)] == [b.coin(0.3) for _ in range(100)]
        assert a.draws == b.draws and a.state == b.state
    assert accel.run_seeds(1001, 50) == pure.run_seeds(1001, 50)

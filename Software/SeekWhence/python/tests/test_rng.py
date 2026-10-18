"""The run's only generator: same seed, same draws; weighted choice follows the weights
(DECISION D-007)."""

from collections import Counter

import pytest

from seekwhence.rng import Rng


def test_same_seed_same_draws():
    a, b = Rng(7), Rng(7)
    assert [a.random() for _ in range(50)] == [b.random() for _ in range(50)]
    assert [a.below(10) for _ in range(50)] == [b.below(10) for _ in range(50)]
    assert [a.choice("xyz".split()) for _ in range(50)] == [b.choice("xyz".split()) for _ in range(50)]
    assert ([a.weighted_index([1, 2, 3]) for _ in range(50)]
            == [b.weighted_index([1, 2, 3]) for _ in range(50)])
    assert a.draws == b.draws == 200


def test_different_seeds_differ():
    assert [Rng(1).random() for _ in range(5)] != [Rng(2).random() for _ in range(5)]


def test_random_is_the_guaranteed_sequence():
    """Only Random.random() is drawn, so the draws equal the module's own for the seed."""
    import random
    assert [Rng(123).random() for _ in range(5)] == [random.Random(123).random() for _ in range(5)]


def test_seed_must_be_an_integer():
    for bad in (1.5, "1", True, None):
        with pytest.raises(TypeError):
            Rng(bad)


def test_below_range_and_errors():
    rng = Rng(3)
    draws = [rng.below(4) for _ in range(2000)]
    assert set(draws) == {0, 1, 2, 3}
    for bad in (0, -1, 2.0, True):
        with pytest.raises(ValueError):
            rng.below(bad)


def test_choice_refuses_unordered_and_empty():
    rng = Rng(3)
    assert rng.choice([5]) == 5
    assert rng.choice((1, 2)) in (1, 2)
    with pytest.raises(TypeError):
        rng.choice({1, 2})
    with pytest.raises(TypeError):
        rng.choice({1: 2})
    with pytest.raises(TypeError):
        rng.choice("ab")
    with pytest.raises(ValueError):
        rng.choice([])


def test_weighted_index_frequencies_follow_the_weights():
    weights = [1, 2, 7]
    n = 20000
    rng = Rng(11)
    counts = Counter(rng.weighted_index(weights) for _ in range(n))
    for position, w in enumerate(weights):
        assert abs(counts[position] / n - w / 10) < 0.02, counts


def test_weighted_index_never_picks_zero_weight():
    rng = Rng(5)
    assert all(rng.weighted_index([0, 3, 0, 1]) in (1, 3) for _ in range(500))


def test_weighted_index_all_zero_is_uniform():
    rng = Rng(9)
    counts = Counter(rng.weighted_index([0, 0, 0]) for _ in range(3000))
    assert set(counts) == {0, 1, 2}
    assert all(abs(c / 3000 - 1 / 3) < 0.05 for c in counts.values())


def test_weighted_index_errors():
    rng = Rng(1)
    with pytest.raises(ValueError):
        rng.weighted_index([])
    for bad in ([1, -1], [0.5, 1], [True, 1]):
        with pytest.raises(ValueError):
            rng.weighted_index(bad)


def test_negative_seeds_are_refused():
    """`random.Random(-n)` draws what `random.Random(n)` draws, so a negative seed would
    silently repeat another run; `Rng` refuses it (item 24)."""
    import random
    assert random.Random(-5).random() == random.Random(5).random()
    with pytest.raises(ValueError):
        Rng(-5)
    assert Rng(0).random() != Rng(5).random()


def test_weighted_choice():
    rng = Rng(2)
    assert rng.weighted_choice(["a", "b"], [0, 1]) == "b"
    assert rng.weighted_choice(["a", "b"], [5, 0]) == "a"
    with pytest.raises(ValueError):
        rng.weighted_choice(["a"], [1, 2])

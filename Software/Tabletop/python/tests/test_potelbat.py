"""The Potelbat baseline (item 23; D-067): a deterministic weighted sum of a type term and
a place term [FCCA pp. 327-328], for contrast with Tabletop (measure M9). No randomness,
no structures; its answers on every problem of the catalogue, and where FCCA says a
formula must fail."""

import pytest

from tabletop import potelbat as PB
from tabletop.config import PROFILES
from tabletop.problems import catalogue, load_all, load_problem
from tabletop.rng import Rng

CFG = PROFILES["standard"]


def test_every_problem_has_an_answer_and_the_touched_object_is_scored_too():
    for pb in load_all():
        order = PB.ranked(pb, CFG)
        assert len(order) == len(pb.objects)
        assert {s.id for s in order} == set(pb.ids())
        assert PB.answer(pb, CFG) == order[0]
        assert order == sorted(order, key=lambda s: -s.total)


def test_scores_are_the_sum_of_the_two_terms_and_draw_nothing():
    pb = load_problem("surround-05")
    for s in PB.scores(pb, CFG):
        assert s.total == pytest.approx(s.type_term + s.place_term)
        assert 0 <= s.type_term <= PB.TYPE_WEIGHT * 100
        assert 0 <= s.place_term <= PB.PLACE_WEIGHT
    # the same scores twice: nothing stochastic [FCCA p. 327]
    assert PB.scores(pb, CFG) == PB.scores(pb, CFG)


def test_the_touched_object_takes_the_whole_type_term_and_a_far_place_term():
    """The touched cup of Surround 5 is in the upper left; the diagonal place is the lower
    right, more than 40 units away, so its place term is 0 (D-067)."""
    pb = load_problem("surround-05")
    s = next(s for s in PB.scores(pb, CFG) if s.id == pb.touched)
    assert s.distance == 0.0 and s.type_term == PB.TYPE_WEIGHT * 100 and s.place_term == 0.0


def test_type_distance_is_intrinsic_cup_nearer_glass_than_fork():
    net = PB.intrinsic_slipnet(CFG)
    assert PB.type_distance(net, "cup", "cup") == 0.0
    assert PB.type_distance(net, "cup", "small glass") < PB.type_distance(net, "cup", "saucer")
    assert PB.type_distance(net, "cup", "small glass") < PB.type_distance(net, "cup", "fork")
    assert PB.type_distance(net, "cup", "salt shaker") == PB.FAR     # no pathway (D-009)


def test_nearness_falls_linearly_to_zero_over_forty_units():
    assert PB.nearness(10, 10, 10, 10) == 1.0
    assert PB.nearness(10, 10, 30, 10) == pytest.approx(0.5)
    assert PB.nearness(10, 10, 50, 10) == 0.0
    assert PB.nearness(10, 10, 90, 10) == 0.0


def test_the_place_term_prefers_the_diagonal_place_at_the_full_weight():
    """Surround 1 [F95 p. 115]: two cups, the touched one upper left, the other lower
    right, diagonally opposite: the place term is near the whole 40 (D-067)."""
    pb = load_problem("surround-01")
    other = next(s for s in PB.scores(pb, CFG) if s.id == pb.answers["E2"])
    assert other.place_term > 0.8 * PB.PLACE_WEIGHT
    glass = next(s for s in PB.scores(pb, CFG) if s.id == pb.answers["E1"])
    assert glass.place_term < PB.OPPOSITE_FACTOR * PB.PLACE_WEIGHT < other.place_term
    assert other.type_term == PB.TYPE_WEIGHT * 100


def test_potelbat_fails_where_fcca_says_a_formula_must():
    """It "sees no relationships between objects, makes no groupings, builds no bridges"
    [FCCA p. 356]: on Surround 5 it takes the isolated cup, where French's program takes
    the small glass 86 times in 100 [F95 p. 118]; on Blockage 6 it takes a glass, where
    the program sees the blockage and touches the cup [F95 p. 128] (measure M9)."""
    s5 = load_problem("surround-05")
    assert PB.answer(s5, CFG).id == s5.answers["E2"]
    b6 = load_problem("blockage-06")
    assert PB.answer(b6, CFG).id in (b6.answers["E1"], b6.answers["E2"])
    assert PB.answer(b6, CFG).id != b6.touched


def test_potelbat_does_well_on_the_simple_problems():
    """"performed reasonably well on simple Tabletop problems" [FCCA p. 328]: the first
    three Surrounds, where French's program takes the diagonal cup [F95 pp. 115-117]."""
    for name in ("surround-01", "surround-02", "surround-03"):
        pb = load_problem(name)
        assert PB.answer(pb, CFG).id == pb.answers["E2"]


def test_margin_is_the_gap_to_the_runner_up():
    pb = load_problem("surround-01")
    order = PB.ranked(pb, CFG)
    assert PB.margin(pb, CFG) == pytest.approx(order[0].total - order[1].total)
    assert PB.margin(load_problem("ch2-fig02"), CFG) == pytest.approx(PB.TYPE_WEIGHT * 100)


def test_the_catalogue_is_covered_without_a_draw():
    rng = Rng(1)
    before = rng.draws
    for name in catalogue():
        PB.answer(load_problem(name), CFG)
    assert rng.draws == before

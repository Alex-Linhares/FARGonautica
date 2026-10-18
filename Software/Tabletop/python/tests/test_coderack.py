"""The Coderack (D-051 to D-055): the catalogue of 23 types, raw and adjusted urgency
(raw / rho^G [F95 p. 81]), French's worked examples (urgencies 6, 3, 1 giving .6, .3, .1
[F95 p. 77]; 46,656 / 47,386 at a = 6 [F95 p. 93]; 8 / 2^3 = 1 [F95 p. 81]; the limits of rho
[F95 p. 82]; the urgency explosion [F95 p. 77]), the seeded chi-square test of the choice,
the capacity of 150 with the oldest pushed off [F95 p. 82], ageing [F95 p. 76], no removal
by urgency [F95 p. 77], and the initial rack [F95 p. 65; F95 p. 98 (img)].

The statistical tests are seeded; their bounds are stated beside each (a chi-square
threshold at p = 0.001, or four binomial standard deviations).
"""

import dataclasses
import json
import math
import re
from pathlib import Path

import pytest

from tabletop import coderack as CR
from tabletop import temperature as TP
from tabletop.codelets import BANDS, CATALOGUE, CodeletType, entry, names, of_band, role_codelets
from tabletop.codelets import catalogue as CAT
from tabletop.config import CODELET_TYPES, PROFILES, Config, codelet_label
from tabletop.rng import Rng
from v2_defaults import V2, V2_PROFILES, v2  # the mechanism at its decision's values (D-114)

CFG = V2_PROFILES["standard"]
DATA = Path(__file__).resolve().parent / "data" / "worked_examples.json"
TAG = re.compile(r"\[(F95|FCCA|FH91) pp?\. \d+(-\d+)?( \(img\))?(; (F95|FCCA|FH91) pp?\. \d+(-\d+)?( \(img\))?)*\]")


def examples() -> dict:
    return json.loads(DATA.read_text())


def with_rho(rho: float, **changes) -> Config:
    return dataclasses.replace(CFG, rho=rho, **changes)


def rack_of(urgencies, config=CFG, clock=0) -> CR.Coderack:
    """A rack of generation-0 find-group codelets carrying the given raw urgencies."""
    rack = CR.Coderack(config)
    for u in urgencies:
        rack.post(CR.make_codelet("find-group", config, clock, raw=u))
    return rack


# ---------------------------------------------------------------------------------------------
# The catalogue (D-051).


def test_the_catalogue_is_the_legend_s_23_types_in_the_strip_s_order_then_the_role_codelets():
    """The legend's 23 first (D-051), then the six role codelets (D-081): propose-role-cp,
    check-roles, find-roles, compare-roles, read-worldview-roles and recast-role (D-093)."""
    assert len(CATALOGUE) == 29
    assert names() == [t for t, _ in CODELET_TYPES]
    assert [row.label for row in CATALOGUE] == [label for _, label in CODELET_TYPES]
    assert names()[0] == "find-group" and names()[22] == "find-answer"
    assert names()[23:] == role_codelets() == ["propose-role-cp", "check-roles", "find-roles",
                                                "compare-roles", "read-worldview-roles",
                                                "recast-role"]
    assert all(row.job_source == CAT.REINTERPRETATION for row in CATALOGUE[23:])
    assert len(set(names())) == 29


def test_every_row_has_a_job_a_source_and_well_formed_pages():
    for row in CATALOGUE:
        assert isinstance(row, CodeletType)
        assert len(row.job.split()) >= 5, row.name
        assert row.job_source in (CAT.TEXT, CAT.INFERRED, CAT.REINTERPRETATION), row.name
        assert TAG.fullmatch(row.pages), (row.name, row.pages)
        assert row.band in BANDS


def test_the_seven_inferred_jobs_are_the_ones_d_051_names():
    # The legend's name is all the sources give for these (D-051); every other job is in
    # the text [F95 p. 63; F95 pp. 76-77; F95 p. 85; F95 p. 94].
    inferred = [row.name for row in CATALOGUE if row.job_source == CAT.INFERRED]
    assert inferred == ["find-sim-nhbr", "build-obj-desc", "search-for-sim-gp",
                        "pick-salient-subgp", "pick-2nd-salient-obj", "build-cp-descrip",
                        "check-worldview"]
    for row in CATALOGUE:
        if row.job_source == CAT.INFERRED:
            assert "p. 96 (img)" in row.pages, row.name     # the legend's page


def test_entry_and_of_band():
    assert entry("examine-cp").label == "EXAM-CP"
    assert entry("find-answer").band == CAT.ANSWER
    with pytest.raises(KeyError):
        entry("find-everything")
    covered = [n for band in BANDS for n in of_band(band)]
    assert sorted(covered) == sorted(names()) and len(covered) == 29
    assert of_band(CAT.SEARCH) == ["search-for-sam-gp", "search-for-sim-gp", "recast-role"]
    assert of_band(CAT.ANSWER) == ["find-answer"]
    assert len(of_band(CAT.SCOUT)) == 13 and len(of_band(CAT.BUILDER)) == 6
    assert len(of_band(CAT.WORLDVIEW)) == 6                   # read-worldview-roles (D-081)
    assert entry("propose-role-cp").band == entry("check-roles").band == CAT.SCOUT


def test_the_bands_are_d_052_s_urgency_bands():
    # Scouts below searches below builders below the Worldview's codelets; one raw urgency per
    # band; find-answer's is set at posting (D-056), so the table holds 0 for it.
    # The correspondence chain (create-cp, build-cp-descrip, cp-compete) is at the top of
    # French's range, so that it is pursued after the generation discount (item 24).
    table = CFG.as_dict("urgencies")
    chain = {"create-cp", "build-cp-descrip", "cp-compete"}
    assert all(table[n] == 100 for n in chain)
    bands = {}
    for band in (CAT.SCOUT, CAT.SEARCH, CAT.BUILDER, CAT.WORLDVIEW):
        values = {table[n] for n in of_band(band) if n not in chain and n not in role_codelets()}
        assert len(values) == 1, (band, values)
        bands[band] = values.pop()
    # The role codelets' urgencies are a priori and their own (D-081): propose-role-cp
    # between the scouts and the chain, check-roles at the scouts' level.
    assert bands[CAT.SCOUT] < table["propose-role-cp"] < table["create-cp"]
    assert table["check-roles"] == bands[CAT.SCOUT]
    assert bands[CAT.SCOUT] < bands[CAT.SEARCH] < bands[CAT.BUILDER] < bands[CAT.WORLDVIEW]
    assert all(1 <= bands[r] <= 100 for r in bands)     # "between 1 and 100" [F95 p. 81]
    assert table["find-answer"] == 0


# ---------------------------------------------------------------------------------------------
# Codelets and urgency (D-052, D-054).


def test_a_codelet_has_the_form_name_age_urgency_with_its_generation():
    # "(codelet-name codelet-age codelet-urgency)" [F95 p. 77], tagged with its generation
    # [F95 p. 81].
    c = CR.make_codelet("find-neighbor", CFG, clock=10, args=("cup1",))
    assert c.type == "find-neighbor" and c.label == "FD-NNBR"
    assert c.args == ("cup1",) and isinstance(c.args, tuple)
    assert c.raw == 20 and c.generation == 0 and c.by == CR.START
    assert c.urgency == 20.0 and c.chance is None
    assert c.age(10) == 0 and c.age(25) == 15
    twin = CR.make_codelet("find-neighbor", CFG, clock=10, args=("cup1",))
    assert c != twin and c == c          # identity, never value


def test_raw_urgency_comes_from_the_table_and_may_be_overridden():
    assert CR.raw_urgency("find-group", CFG) == 20
    assert CR.raw_urgency("cp-compete", CFG) == 100
    assert CR.make_codelet("find-answer", CFG, 0, raw=49).raw == 49
    with pytest.raises(KeyError):
        CR.raw_urgency("find-everything", CFG)


def test_adjusted_urgency_is_raw_over_rho_to_the_generation():
    # raw / rho^G [F95 p. 81 (img)]: with rho = 2, a raw urgency of 8 at generation 3 is 1
    # [F95 p. 81]; with rho = 3 a child of raw 20 carries 6.67, a grandchild 2.22.
    assert CR.adjusted_urgency(8, 3, with_rho(2)) == 1.0
    assert CR.adjusted_urgency(20, 0, CFG) == 20.0
    assert CR.adjusted_urgency(20, 1, CFG) == pytest.approx(20 / 3)
    assert CR.adjusted_urgency(20, 2, CFG) == pytest.approx(20 / 9)
    assert CFG.rho == 3.0                                       # [F95 p. 81]


def test_the_floor_keeps_a_deep_descendant_choosable():
    # raw 1 at generation 4 under rho = 3 is 1/81 = 0.012, above the floor of 0.01; at
    # generation 5 it would be 0.004 and is floored (D-052).
    assert CR.adjusted_urgency(1, 4, CFG) == pytest.approx(1 / 81)
    assert CR.adjusted_urgency(1, 5, CFG) == CFG.urgency_floor == 0.01
    assert CR.adjusted_urgency(0, 0, CFG) == CFG.urgency_floor


def test_below_rho_1_an_underflowed_power_gives_an_infinite_urgency_not_an_error():
    """rho 0.5 to the 1,075th generation underflows to 0.0 (the `rho` bench's chain of consumer
    C4's fights reached it under v5): the adjusted urgency is then infinite, the limit D-060's
    revision takes, and no division by zero (D-052)."""
    cfg = v2(rho=0.5)
    assert CR.adjusted_urgency(20.0, 10, cfg) == 20.0 * 2 ** 10
    assert CR.adjusted_urgency(20.0, 1100, cfg) == float("inf")
    assert CR.adjusted_urgency(20.0, 1100, v2(rho=3.0)) == cfg.urgency_floor


def test_children_are_one_generation_on_and_posted_by_their_parent():
    # "The original codelets on the Coderack are of generation 0, their children are of
    # generation 1, their grandchildren are of generation 2, and so on." [F95 p. 81]
    parent = CR.make_codelet("find-group", CFG, 0)
    child = CR.child_of(parent, "find-end-objs", CFG, clock=3, args=("g1",))
    grandchild = CR.child_of(child, "find-neighbor", CFG, clock=7, args=("fork1",))
    assert (parent.generation, child.generation, grandchild.generation) == (0, 1, 2)
    assert child.by == "find-group" and grandchild.by == "find-end-objs"
    assert child.posted_at == 3 and child.raw == 20 and child.urgency == pytest.approx(20 / 3)
    assert grandchild.urgency == pytest.approx(20 / 9)
    assert CR.child_of(parent, "build-obj-0", CFG, 3, raw=60).urgency == pytest.approx(20)


def test_codelets_without_a_parent_are_generation_0():
    # The Slipnet's postings and the find-answer schedule have no parent (D-054).
    top_down = CR.make_codelet("find-group", CFG, 30, by=CR.SLIPNET)
    answer = CR.make_codelet("find-answer", CFG, 30, by=CR.ANSWER_SCHEDULE, raw=10)
    assert top_down.generation == 0 and top_down.urgency == 20.0
    assert answer.generation == 0 and answer.urgency == 10.0


# ---------------------------------------------------------------------------------------------
# The limits of rho [F95 p. 82].


def test_rho_1_gives_every_codelet_its_raw_urgency():
    # "If p is 1, we have the original situation, in which all codelets, when put on the
    # Coderack, are given their raw urgency." [F95 p. 82]
    cfg = with_rho(1)
    c = CR.make_codelet("find-group", cfg, 0)
    for generation in range(6):
        c = CR.child_of(c, "find-neighbor", cfg, 0)
        assert c.urgency == c.raw == 20


def test_a_larger_rho_favours_the_elders_and_a_small_one_the_children():
    # "As the size of p increases, the relative importance of parent and grandparent codelets
    # increases with respect to the younger generations of codelets." [F95 p. 82]; large:
    # breadth-first; "less than 1": the newest "would always run", depth-first [F95 p. 82].
    def parent_share(rho):
        cfg = with_rho(rho)
        rack = CR.Coderack(cfg)
        parent = CR.make_codelet("find-group", cfg, 0)
        rack.post(parent)
        for _ in range(6):
            rack.post(CR.child_of(parent, "find-neighbor", cfg, 1))
        return rack.chances(1.0)[0]
    shares = [parent_share(rho) for rho in (0.5, 1, 1.5, 2, 3, 10, 100)]
    assert shares == sorted(shares) and len(set(shares)) == 7
    assert shares[0] < 1 / 7 < shares[2]            # rho 0.5: the children outrank it
    assert shares[1] == pytest.approx(1 / 7)        # rho 1: one of seven equals
    assert shares[-1] > 0.94                        # rho 100: the parent runs first


# ---------------------------------------------------------------------------------------------
# The choice: French's worked examples [F95 p. 77; F95 pp. 92-93].


def test_the_p_77_example_gives_point_6_point_3_and_point_1_at_a_equal_1():
    ex = examples()["urgency_choice"]
    rack = rack_of([c["urgency"] for c in ex["codelets"]])
    assert rack.total_urgency() == ex["total_urgency"] == 10
    chances = rack.chances(1.0)
    assert chances == pytest.approx([c["probability"] for c in ex["codelets"]])
    assert chances == pytest.approx([0.6, 0.3, 0.1])


def test_the_p_93_example_at_a_equal_6_gives_46656_over_47386():
    # 6^6 = 46,656, 3^6 = 729, 1^6 = 1, total 47,386; 0.985, 0.015, under 0.001 [F95 p. 93].
    ex = examples()["temperature_exponent"]
    rack = rack_of([c["urgency"] for c in ex["at_a_equal_6"]["codelets"]])
    a = TP.exponent_at(0, CFG)
    assert a == ex["a_at_temperature_0"] == 6
    chances = rack.chances(a)
    total = ex["at_a_equal_6"]["total_powered"]
    assert total == 47386
    assert chances[0] == pytest.approx(46656 / total, abs=1e-9)
    assert chances[1] == pytest.approx(729 / total, abs=1e-9)
    assert chances[2] == pytest.approx(1 / total, abs=1e-9)
    assert round(chances[0], 3) == 0.985 and round(chances[1], 3) == 0.015
    assert chances[2] < 0.001


def test_at_a_equal_point_1_every_codelet_is_nearly_as_likely_as_any_other():
    # "When the temperature is 100, the temperature-dependent urgency for all codelets, for
    # all intents and purposes, is 1." [F95 p. 92]: 6^0.1 is 1.196, so the chances of 6, 3
    # and 1 are 0.36, 0.34 and 0.30.
    rack = rack_of([6, 3, 1])
    a = TP.exponent_at(100, CFG)
    assert a == pytest.approx(0.1)
    chances = rack.chances(a)
    assert chances[0] == pytest.approx(6 ** 0.1 / (6 ** 0.1 + 3 ** 0.1 + 1), abs=1e-9)
    assert all(0.29 < c < 0.37 for c in chances)
    assert chances[0] > chances[1] > chances[2]     # still ordered, barely


def test_the_seeded_chi_square_of_3000_draws_from_the_p_77_rack():
    # Expected 1800, 900 and 300 of 3000. With 2 degrees of freedom a statistic above 13.82
    # would happen by chance with probability 0.001; the seed is fixed. The chosen codelet is
    # hung again after each draw, so the rack is the same at every draw.
    rng = Rng(2026)
    rack = rack_of([6, 3, 1])
    counts = [0, 0, 0]
    for _ in range(3000):
        c = rack.choose(1.0, rng)
        counts[[6, 3, 1].index(c.raw)] += 1
        rack.post(c)
    expected = [1800, 900, 300]
    chi_square = sum((c - e) ** 2 / e for c, e in zip(counts, expected))
    assert chi_square < 13.82, (counts, chi_square)
    assert rng.draws == 3000                        # one draw per choice


def test_at_a_equal_6_the_leader_is_chosen_almost_always():
    # 0.985 of 1000 draws: 985 +/- 4 * sqrt(1000 * 0.985 * 0.015) = 985 +/- 15.
    rng = Rng(7)
    rack = rack_of([6, 3, 1])
    wins = 0
    for _ in range(1000):
        c = rack.choose(6.0, rng)
        wins += c.raw == 6
        rack.post(c)
    assert abs(wins - 985) <= 15, wins


def test_equal_urgencies_remain_a_random_choice_at_any_exponent():
    # "(If two codelets have the same assigned urgency, the selection between them remains,
    # of course, random.)" [F95 p. 93]: 400 draws at a = 6, 200 +/- 4 * sqrt(100) = 40.
    rng = Rng(11)
    rack = rack_of([40, 40])
    first, second = rack.codelets
    firsts = 0
    for _ in range(400):
        c = rack.choose(6.0, rng)
        firsts += c is first
        rack.post(c)
    assert rack.chances(6.0) == pytest.approx([0.5, 0.5])
    assert abs(firsts - 200) <= 40, firsts


def test_choose_takes_the_codelet_off_records_every_chance_and_is_reproducible():
    rack = rack_of([6, 3, 1])
    rng = Rng(5)
    before = rng.draws
    chosen = rack.choose(1.0, rng)
    assert rng.draws == before + 1
    assert chosen not in rack.codelets and len(rack) == 2
    assert chosen.chance in (0.6, 0.3, 0.1)
    assert all(c.chance is not None for c in rack.codelets)
    again = rack_of([6, 3, 1]).choose(1.0, Rng(5))
    assert again.raw == chosen.raw
    assert rack_of([]).choose(1.0, rng) is None and rng.draws == before + 1


# ---------------------------------------------------------------------------------------------
# Generations against the explosion [F95 p. 77; F95 pp. 81-82].


def test_the_p_81_example_children_of_raw_8_at_generation_3_contribute_4_not_32():
    # rho = 2, two second-generation find-groups at adjusted 4; one spawns four children of
    # raw 8, each 8 / 2^3 = 1, total 4 "as opposed to 32 in the standard implementation"
    # [F95 p. 81].
    ex = examples()["generations"]
    cfg = with_rho(ex["rho_in_example"])
    rack = CR.Coderack(cfg)
    parents = [CR.make_codelet("find-group", cfg, 0, generation=2, raw=16) for _ in range(2)]
    rack.post_all(parents)
    assert [p.urgency for p in parents] == [4.0, 4.0]
    assert rack.chances(1.0) == pytest.approx([0.5, 0.5])          # 50 percent each
    runner = rack.choose(1.0, Rng(1))
    children = [CR.child_of(runner, c["name"].replace("object", "objs"), cfg, 1, raw=8)
                for c in ex["children"]]
    assert [c.generation for c in children] == [3, 3, 3, 3]
    assert [c.urgency for c in children] == [1.0, 1.0, 1.0, 1.0]
    assert sum(c.urgency for c in children) == ex["children_total_adjusted"] == 4
    assert 4 * 8 == ex["children_total_standard"] == 32
    rack.post_all(children)
    # The remaining find-group keeps half of the rack's urgency; in the standard
    # implementation it would have 4 of 36, 11.1 percent (French prints 12.5) [F95 p. 81].
    assert rack.chances(1.0)[0] == pytest.approx(0.5)
    standard = 4 / (4 + 32)
    assert round(100 * standard, 1) == ex["competitor_share_after_standard_percent_computed"]


def test_the_p_77_explosion_under_rho_1_and_how_rho_3_damps_it():
    # Two find-groups of urgency 10; one runs and spawns six of raw 10: the survivor falls
    # from one half to one seventh, "approximately one-quarter" [F95 p. 77]. Under rho = 3
    # the six children carry 10/3 each and the survivor keeps a third.
    ex = examples()["urgency_explosion"]
    for rho, share_after in ((1, ex["share_after"]), (3, 1 / 3)):
        cfg = with_rho(rho)
        rack = CR.Coderack(cfg)
        a, b = (CR.make_codelet("find-group", cfg, 0, raw=10) for _ in range(2))
        rack.post_all([a, b])
        assert rack.chances(1.0)[0] == pytest.approx(ex["share_before"])
        rack.codelets.remove(a)
        for _ in range(ex["children_spawned"]):
            rack.post(CR.child_of(a, "find-neighbor", cfg, 1, raw=ex["child_urgency"]))
        assert rack.chances(1.0)[0] == pytest.approx(share_after, abs=1e-6)


# ---------------------------------------------------------------------------------------------
# Capacity and age (D-053) [F95 p. 76; F95 p. 82].


def test_the_capacity_is_150_and_the_oldest_are_pushed_off_first():
    assert CFG.coderack_capacity == 150                             # [F95 p. 82]
    rack = CR.Coderack(CFG)
    codelets = [CR.make_codelet("find-group", CFG, clock) for clock in range(150)]
    assert rack.post_all(codelets) == []
    assert len(rack) == 150
    newcomer = CR.make_codelet("find-neighbor", CFG, 150)
    removed = rack.post(newcomer)
    assert removed == [(codelets[0], CR.CAPACITY)]
    assert len(rack) == 150 and rack.codelets[0] is codelets[1]
    assert rack.codelets[-1] is newcomer


def test_the_oldest_is_pushed_off_even_when_it_is_the_most_urgent():
    # Removal is by age alone, never by urgency [F95 p. 77].
    cfg = dataclasses.replace(CFG, coderack_capacity=3)
    rack = CR.Coderack(cfg)
    elder = CR.make_codelet("cp-compete", cfg, 0, raw=100)
    rack.post(elder)
    rack.post_all(CR.make_codelet("find-group", cfg, 1, raw=1) for _ in range(2))
    removed = rack.post(CR.make_codelet("find-group", cfg, 2, raw=1))
    assert removed == [(elder, CR.CAPACITY)]
    assert [c.raw for c in rack.codelets] == [1, 1, 1]


def test_a_codelet_older_than_the_maximum_age_is_removed_at_the_next_update():
    assert CFG.max_codelet_age == 60
    rack = CR.Coderack(CFG)
    old = CR.make_codelet("find-group", CFG, 0)
    young = CR.make_codelet("find-neighbor", CFG, 30)
    rack.post_all([old, young])
    assert rack.remove_aged(60) == []                   # exactly 60: not yet "too long"
    assert rack.remove_aged(61) == [(old, CR.AGE)]
    assert rack.codelets == [young]
    assert rack.remove_aged(91) == [(young, CR.AGE)] and len(rack) == 0


def test_aged_removals_come_in_rack_order_and_the_rest_keep_theirs():
    rack = CR.Coderack(CFG)
    posted = [CR.make_codelet("find-group", CFG, clock) for clock in (0, 5, 40, 2, 50)]
    rack.post_all(posted)
    removed = rack.remove_aged(63)      # ages 63, 58, 23, 61, 13
    assert [c for c, _ in removed] == [posted[0], posted[3]]
    assert all(reason == CR.AGE for _, reason in removed)
    assert rack.codelets == [posted[1], posted[2], posted[4]]


def test_a_low_urgency_codelet_is_never_removed_for_its_urgency():
    # "I chose to leave low-urgency codelets on the Coderack" [F95 p. 77]: a floored codelet
    # survives choices and age updates until it is too old or pushed off.
    rack = CR.Coderack(CFG)
    weakling = CR.make_codelet("find-neighbor", CFG, 0, generation=8)   # 20 / 3^8 = 0.003
    assert weakling.urgency == CFG.urgency_floor
    rack.post(weakling)
    rng = Rng(3)
    for clock in range(1, 60):
        rack.post(CR.make_codelet("cp-compete", CFG, clock))
        chosen = rack.choose(TP.exponent_at(0, CFG), rng)
        assert chosen is not weakling
        assert rack.remove_aged(clock) == []
    assert weakling in rack.codelets
    assert rack.remove_aged(61) == [(weakling, CR.AGE)]


# ---------------------------------------------------------------------------------------------
# The initial rack (D-055) and the screen's shares [F95 p. 97].


def test_the_initial_rack_is_d_055_s_25_codelets_at_generation_0():
    # "There are a number of find-group codelets initially on the Coderack" and also
    # pick-salient-object and pick-salient-correspondence codelets [F95 p. 65]; the dump two
    # codelets in shows the find-group and pick-2nd-salient-obj counts [F95 p. 98 (img)].
    rack = CR.initial_rack(CFG)
    assert len(rack) == 25                     # 24 and one find-roles (D-081, item 04)
    assert rack.counts() == [("find-group", 20), ("pick-salient-obj", 1),
                             ("pick-2nd-salient-obj", 2), ("pick-strong-cp", 1),
                             ("find-roles", 1)]
    assert [c.type for c in rack.codelets[:20]] == ["find-group"] * 20
    for c in rack.codelets:
        assert c.generation == 0 and c.by == CR.START and c.args == ()
        assert c.posted_at == 0 and c.raw == CR.raw_urgency(c.type, CFG)
        assert c.urgency == c.raw and c.chance is None
    assert rack.total_urgency() == 20 * 20 + 2 * 20 + 20 + 50 + 30     # find-roles at 30 (D-081)


def test_the_initial_rack_is_reproducible_and_draws_nothing():
    a, b = CR.initial_rack(CFG), CR.initial_rack(CFG, clock=0)
    assert [(c.type, c.raw, c.urgency) for c in a.codelets] == \
        [(c.type, c.raw, c.urgency) for c in b.codelets]
    assert a.codelets[0] is not b.codelets[0]
    rng = Rng(1)
    chosen = CR.initial_rack(CFG).choose(TP.exponent_at(80, CFG), rng)
    assert rng.draws == 1 and chosen.type in ("find-group", "pick-salient-obj",
                                              "pick-2nd-salient-obj", "pick-strong-cp")


def test_the_shares_are_the_screen_s_percentages_in_the_strip_s_order():
    rack = CR.initial_rack(CFG)
    shares = rack.shares()
    assert [label for label, _ in shares] == ["FND-GP", "PK-SALO", "PK2SALO", "PK-SALC", "FND-RL"]
    assert sum(p for _, p in shares) == pytest.approx(100)
    assert dict(shares)["FND-GP"] == pytest.approx(100 * 20 / 25)
    assert CR.Coderack(CFG).shares() == [] and CR.Coderack(CFG).counts() == []


def test_the_chapter_4_final_rack_s_bars_are_reproduced_from_its_counts():
    # The strip at 55 codelets, nine bars summing to 100 [F95 p. 107 (img)]: a rack with that
    # many codelets of each type prints exactly those bars.
    bars = examples()["chapter_4_final_rack"]["bars"]
    rack = CR.Coderack(CFG)
    label_to_type = {label: t for t, label in CODELET_TYPES}
    for bar in bars:
        for _ in range(bar["percent"]):
            rack.post(CR.make_codelet(label_to_type[bar["label"]], CFG, 55))
    assert len(rack) == 100
    assert rack.shares() == [(b["label"], float(b["percent"])) for b in bars]
    assert [label for label, _ in rack.shares()] == [b["label"] for b in bars]   # strip order


def test_chances_sum_to_one_at_every_exponent():
    rack = CR.initial_rack(CFG)
    for t in (0, 25, 50, 80, 100):
        chances = rack.chances(TP.exponent_at(t, CFG))
        assert math.isclose(sum(chances), 1.0)
        assert len(chances) == 25
    assert codelet_label("find-answer") == "FND-ANS"

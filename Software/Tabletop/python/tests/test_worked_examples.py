"""French's worked examples of the Coderack and temperature, as data (tests/data/worked_examples.json)
and as arithmetic: the urgency choice of p. 77, the temperature exponent of pp. 92-93 with its
print slip, the urgency explosion of p. 77, the generation formula of p. 81, and the
oscillating Worldview of p. 90 [F95 p. 77; F95 p. 81; F95 pp. 90-93].
"""

import json
from pathlib import Path

DATA = Path(__file__).resolve().parent / "data" / "worked_examples.json"


def load() -> dict:
    return json.loads(DATA.read_text())


def test_the_data_file_parses_and_cites_its_pages():
    data = load()
    for key in ("urgency_choice", "temperature_exponent", "urgency_explosion", "generations",
                "coderack_capacity", "worldview_oscillation", "temperature_formula",
                "chapter_4_final_rack"):
        assert key in data, key
        assert data[key]["source"].startswith("[F95 p"), key


def test_urgency_choice_is_the_share_of_the_total():
    # Urgencies 6, 3 and 1 give chances .6, .3 and .1 [F95 p. 77].
    ex = load()["urgency_choice"]
    total = sum(c["urgency"] for c in ex["codelets"])
    assert total == ex["total_urgency"] == 10
    for c in ex["codelets"]:
        assert abs(c["urgency"] / total - c["probability"]) < 1e-9


def test_temperature_exponent_six_amplifies_as_printed():
    # 6^6 = 46,656, 3^6 = 729, 1^6 = 1, total 47,386; chances 0.985, 0.015, < 0.001 [F95 p. 93].
    ex = load()["temperature_exponent"]
    a = ex["a_at_temperature_0"]
    assert a == 6 and ex["a_at_temperature_100"] == 0.1
    table = ex["at_a_equal_6"]
    assert [c["urgency"] ** a for c in table["codelets"]] == [c["powered"] for c in table["codelets"]]
    total = sum(c["powered"] for c in table["codelets"])
    assert total == table["total_powered"] == 47386
    first, second, third = table["codelets"]
    assert round(first["powered"] / total, 3) == first["probability"] == 0.985
    assert round(second["powered"] / total, 3) == second["probability"] == 0.015
    assert third["powered"] / total < third["probability_below"] == 0.001


def test_the_point_four_on_page_93_is_a_print_slip():
    # At a = 1 the chances are those of p. 77; p. 93 prints "0.4" where 0.3 is meant.
    ex = load()["temperature_exponent"]
    urgencies = [c["urgency"] for c in load()["urgency_choice"]["codelets"]]
    computed = [u / sum(urgencies) for u in urgencies]
    assert [round(x, 1) for x in computed] == ex["at_a_equal_1_correct"] == [0.6, 0.3, 0.1]
    assert ex["at_a_equal_1_as_printed"][1] == 0.4 and "0.4" in ex["print_slip"]


def test_the_exponent_is_linear_in_the_temperature():
    # "varies linearly between 6 (when the temperature is 0) and 0.1 (when the temperature is
    # 100)" [F95 p. 92]: the only line through both points.
    ex = load()["temperature_exponent"]
    a0, a100 = ex["a_at_temperature_0"], ex["a_at_temperature_100"]

    def a(t):
        return a0 + (a100 - a0) * t / 100

    assert a(0) == 6 and abs(a(100) - 0.1) < 1e-12 and abs(a(50) - 3.05) < 1e-12
    assert 6 ** a(100) < 1.2  # "for all intents and purposes, is 1" [F95 p. 92]


def test_urgency_explosion_leaves_the_survivor_a_seventh():
    # Two find-groups of 10; one runs and spawns six of 10: the other falls from 1/2 to 1/7
    # of the rack, "approximately one-quarter of its previous value" [F95 p. 77].
    ex = load()["urgency_explosion"]
    before = ex["rack_before"][1]["urgency"] / sum(c["urgency"] for c in ex["rack_before"])
    after = 10 / (10 + ex["children_spawned"] * ex["child_urgency"])
    assert before == ex["share_before"] == 0.5
    assert abs(after - ex["share_after"]) < 1e-6
    assert 0.2 < after / before < 0.35


def test_generations_divide_the_raw_urgency_by_rho_to_the_generation():
    # rho = 2, children of generation 3 with raw urgency 8: 8 / 2^3 = 1 each, 4 in all against
    # 32 "in the standard implementation" [F95 p. 81]; the program's rho is 3.
    ex = load()["generations"]
    rho = ex["rho_in_example"]
    assert rho == 2 and ex["rho_in_program"] == 3 and ex["raw_urgency_range"] == [1, 100]
    for child in ex["children"]:
        assert child["generation"] == ex["parent"]["generation"] + 1
        assert child["raw_urgency"] / rho ** child["generation"] == child["adjusted_urgency"] == 1
    assert sum(c["adjusted_urgency"] for c in ex["children"]) == ex["children_total_adjusted"] == 4
    assert sum(c["raw_urgency"] for c in ex["children"]) == ex["children_total_standard"] == 32
    parent = ex["parent"]["adjusted_urgency"]
    computed = 100 * parent / (parent + ex["children_total_standard"])
    assert round(computed, 1) == ex["competitor_share_after_standard_percent_computed"] == 11.1
    assert ex["competitor_share_after_standard_percent_as_printed"] == 12.5  # as printed


def test_the_oscillating_worldview_changes_little_in_quality():
    # 274, 267, 280, 273, 285: small changes of quality, every step a change of composition
    # [F95 p. 90].
    ratings = load()["worldview_oscillation"]["ratings"]
    assert ratings == [274, 267, 280, 273, 285]
    steps = [b - a for a, b in zip(ratings, ratings[1:])]
    assert all(abs(s) <= 13 for s in steps) and len(steps) == 4


def test_the_final_rack_of_chapter_4_sums_to_one_hundred():
    # The bars of Fig. 4.9's Coderack strip, 55 codelets in [F95 p. 107 (img)].
    rack = load()["chapter_4_final_rack"]
    assert rack["codelets_run"] == 55
    assert sum(b["percent"] for b in rack["bars"]) == 100
    assert rack["bars"][-1] == {"label": "FND-ANS", "percent": 30}


def test_capacity_and_windows():
    data = load()
    assert data["coderack_capacity"]["capacity"] == 150  # [F95 p. 82]
    assert data["temperature_formula"]["dQ_window"] == 6  # [F95 p. 90]
    assert data["temperature_formula"]["dWV_window_competitions"] == 10  # [F95 p. 91]

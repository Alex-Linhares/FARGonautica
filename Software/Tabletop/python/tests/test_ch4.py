"""The chapter 4 run as data (tests/data/ch4.json, docs/ch4-run.md): the nine screen dumps of
Figs. 4.1-4.9 and the five Worldviews of Figs. 4.10-4.14, read on the page images [F95
pp. 95-111]. The data must parse and agree with itself: the codelet counts never fall, the
bars sum to about 100, every label is one of the strip's 23 [F95 p. 107 (img)], every Slipnet
has the grid's 41 boxes within 1-150 [F95 p. 95], the thermometer stays within 0-100, and the
configuration is Surround 5's thirteen objects [F95 p. 96 (img); F95 p. 118 (img)].
"""

import json
import re
from pathlib import Path

HERE = Path(__file__).resolve().parent
DATA = HERE / "data" / "ch4.json"
WORKED = HERE / "data" / "worked_examples.json"
DOC = HERE.parent / "docs" / "ch4-run.md"
SPEC = HERE.parent / "SPEC.md"


def load() -> dict:
    return json.loads(DATA.read_text())


def test_the_file_parses_and_has_the_nine_dumps_and_five_other_runs():
    data = load()
    assert data["source"] == "[F95 pp. 95-111]"
    figs = [d["figure"] for d in data["dumps"]]
    assert figs == [f"4.{n}" for n in range(1, 10)]
    assert [r["figure"] for r in data["other_runs"]] == [f"4.{n}" for n in range(10, 15)]
    pages = [d["page"] for d in data["dumps"]]
    assert pages == [96, 98, 99, 100, 102, 103, 104, 106, 107]  # Figs. 4.1-4.9
    assert all(r["page"] in (108, 109, 110) for r in data["other_runs"])


def test_codelet_counts_never_fall_and_end_at_fifty_five():
    # 0, 2, 12, 22, 33, 36, 47, 47, 55 [F95 pp. 96-107 (img)]; Figs. 4.7 and 4.8 share 47.
    counts = [d["codelets_run"] for d in load()["dumps"]]
    assert counts == sorted(counts)
    assert counts[0] == 0 and counts[-1] == 55
    repeated = [a for a, b in zip(counts, counts[1:]) if a == b]
    assert repeated == [47]
    final = json.loads(WORKED.read_text())["chapter_4_final_rack"]
    assert final["codelets_run"] == counts[-1]


def test_every_bar_label_is_in_the_catalogue_and_shares_sum_to_about_one_hundred():
    data = load()
    labels = data["coderack_labels"]
    assert len(labels) == 23 and labels[0] == "FND-GP" and labels[-1] == "FND-ANS"
    for dump in data["dumps"]:
        bars = dump["coderack"]
        if dump["codelets_run"] == 0:
            assert bars == []
            continue
        assert all(b["label"] in labels for b in bars), dump["figure"]
        assert len({b["label"] for b in bars}) == len(bars)  # one bar per label
        assert all(1 <= b["percent"] <= 100 for b in bars)
        assert 97 <= sum(b["percent"] for b in bars) <= 101, dump["figure"]
        order = [labels.index(b["label"]) for b in bars]
        assert order == sorted(order)  # bars listed left to right


def test_the_final_dump_agrees_with_the_worked_examples_file():
    last = load()["dumps"][-1]
    final = json.loads(WORKED.read_text())["chapter_4_final_rack"]
    assert last["coderack"] == final["bars"]
    assert last["structures"]["answer"] == "small-glass1" and last["temperature"] == 2.0


def test_the_legend_names_every_label():
    # The printed legend beside Fig. 4.1 [F95 p. 96 (img)].
    data = load()
    names = data["coderack_legend"]["names"]
    assert list(names) == data["coderack_labels"]
    assert names["FND-GP"] == "find-group" and names["FND-ANS"] == "find-answer"
    assert names["PK-SALC"] == "pick-strong-cp" and names["BLD-CP"] == "build-cp-descrip."
    assert len(set(names.values())) == 23


def test_every_slipnet_has_the_forty_one_boxes_within_range():
    data = load()
    grid = [node for row in data["slipnet_grid"] for node in row]
    assert len(grid) == 41 and [len(r) for r in data["slipnet_grid"]] == [7, 7, 7, 7, 7, 6]
    spec_grid = SPEC.read_text()
    assert all(node in spec_grid for node in grid)
    for dump in data["dumps"]:
        slip = dump["slipnet"]
        assert list(slip) == grid, dump["figure"]
        assert all(isinstance(v, int) and 1 <= v <= 150 for v in slip.values()), dump["figure"]


def test_the_slipnet_readings_match_frenchs_words():
    dumps = {d["figure"]: d["slipnet"] for d in load()["dumps"]}
    # At the start 'opposite' and 'cup' are the active nodes [F95 p. 97; F95 p. 96 (img)].
    start = dumps["4.1"]
    assert start["opposite"] == 139 and start["cup"] == 51
    assert sum(1 for v in start.values() if v > 2) == 3  # opposite, cup, liq holder
    # Fig. 4.3: after 'cup', 'small glass' is the most active object type [F95 p. 101].
    s = dumps["4.3"]
    assert s["small glass"] > max(s["fork"], s["spoon"], s["knife"])
    # Fig. 4.4: 'end' highly active; 'left' and 'right', not 'above' and 'below' [F95 p. 101].
    s = dumps["4.4"]
    assert s["end"] >= 100 and min(s["left"], s["right"]) > max(s["above"], s["below"])
    # The ceiling of 150 is reached [F95 p. 95; F95 p. 102 (img)].
    assert dumps["4.5"]["opposite"] == 150 and dumps["4.7"]["right"] == 150
    # Figs. 4.7-4.9 share one Slipnet (flagged as doubtful in the data).
    assert dumps["4.7"] == dumps["4.8"] == dumps["4.9"]
    # 'cup' never falls [F95 pp. 96-107 (img)].
    cups = [dumps[f"4.{n}"]["cup"] for n in range(1, 10)]
    assert cups == sorted(cups) and cups[-1] == 130


def test_the_thermometer_readings():
    # 80.0, (empty), 59.0, 43.2, 31.3, 35.8, 13.6, 2.0 [F95 pp. 98-107 (img)].
    temps = [d["temperature"] for d in load()["dumps"]]
    assert temps == [None, 80.0, None, 59.0, 43.2, 31.3, 35.8, 13.6, 2.0]
    assert all(0 <= t <= 100 for t in temps if t is not None)
    other = {r["figure"]: r["temperature"] for r in load()["other_runs"]}
    assert other == {"4.10": None, "4.11": None, "4.12": None, "4.13": None, "4.14": 97.5}


def test_the_configuration_is_surround_five():
    # Fork, fork, knife, cup (touched), spoon, spoon / spoon, spoon, small glass, fork, fork,
    # knife / an isolated cup [F95 p. 96 (img); F95 p. 118 (img)].
    prob = load()["problem"]
    objects = {o["id"]: o for o in prob["objects"]}
    assert len(objects) == 13
    counts = {}
    for o in objects.values():
        counts[o["type"]] = counts.get(o["type"], 0) + 1
    assert counts == prob["palette_counts"] == {"fork": 4, "knife": 2, "spoon": 4, "cup": 2,
                                                "small glass": 1}
    assert objects[prob["touched"]]["type"] == "cup" and objects[prob["touched"]]["side"] == "henry"
    assert [objects[i]["type"] for i in prob["henry_side_group"]] == \
        ["fork", "fork", "knife", "cup", "spoon", "spoon"]
    assert [objects[i]["type"] for i in prob["eliza_side_group"]] == \
        ["spoon", "spoon", "small glass", "fork", "fork", "knife"]
    assert prob["isolated"] == ["cup2"] and objects["cup2"]["group"] is None
    assert prob["expected_answers"] == {"E1": "small-glass1", "E2": "cup2"}
    for o in objects.values():
        assert 0 < o["x"] < 1 and 0 < o["y"] < 1
    henry = [objects[i] for i in prob["henry_side_group"]]
    eliza = [objects[i] for i in prob["eliza_side_group"]]
    assert all(h["y"] < 0.5 < e["y"] for h in henry for e in eliza)  # Henry at the top
    assert [o["x"] for o in henry] == sorted(o["x"] for o in henry)  # left to right
    assert [o["x"] for o in eliza] == sorted(o["x"] for o in eliza)
    assert objects["cup2"]["x"] > 0.7 and objects["cup2"]["y"] > 0.7  # lower right


def test_the_worldview_narrative_of_the_run():
    dumps = {d["figure"]: d for d in load()["dumps"]}
    assert dumps["4.3"]["structures"]["worldview"] == []
    assert dumps["4.4"]["structures"]["worldview"] == ["cup1-cup2"]
    assert dumps["4.7"]["structures"]["worldview"] == ["cup1-cup2", "spoon1-spoon2 to spoon3-spoon4"]
    assert dumps["4.8"]["structures"]["worldview"] == dumps["4.9"]["structures"]["worldview"] == \
        ["cup1-small-glass1", "spoon1-spoon2 to spoon3-spoon4"]
    kinds = [c["status"] for c in dumps["4.7"]["structures"]["correspondences"]]
    assert "tentative rival" in kinds
    for d in dumps.values():
        for key in ("narration", "doubtful", "structures"):
            assert key in d, d["figure"]
        assert all(95 <= n["page"] <= 111 for n in d["narration"])


def test_the_document_covers_every_figure_and_cites_images():
    doc = DOC.read_text()
    for n in range(1, 15):
        assert f"4.{n}" in doc, n
    assert len(re.findall(r"\[F95 pp?\. \d+(?:-\d+)? \(img\)\]", doc)) >= 20
    assert "Surround 5" in doc and "pick-strong-cp" in doc
    for label in load()["coderack_labels"]:
        assert label in doc, label

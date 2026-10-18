"""Chapter 2's nineteen configurations, Figures 2.2-2.20 [F95 pp. 27-35 (img)], digitized as
`problems/ch2-figNN.json` with French's expected answer in his words (`expected`), and the
cross-check of the eighteen layouts FCCA redraws [FCCA pp. 394-399 (img)] against the files
(`tests/data/fcca_crosscheck.json`, `docs/problems.md` 6 and 9; D-003, ANSWERS.md 4).

The quotations in every `expected` block are checked against the text layer of the cited
page by `tools/check_quotes.py` (skipped when F95 is not found: `tests/sources.py`).
"""

import json
import re
import subprocess
import sys
from pathlib import Path

import pytest

import tabletop.problems as P
from sources import CHECKER, PDFTOTEXT_MISSING, available, skip_reason
from tabletop.geometry import TABLE_HEIGHT, TABLE_WIDTH, distance
from tabletop.objects import footprint

CH2 = [f"ch2-fig{n:02d}" for n in range(2, 21)]
PAGES = {2: 27, 3: 27, 4: 28, 5: 28, 6: 29, 7: 29, 8: 29, 9: 30, 10: 30, 11: 31, 12: 31,
         13: 32, 14: 32, 15: 33, 16: 33, 17: 34, 18: 34, 19: 35, 20: 35}  # [F95 pp. 27-35]
DATA = Path(__file__).resolve().parent / "data"
DOCS = Path(__file__).resolve().parents[1] / "docs" / "problems.md"
FCCA_FIGURES = [f"IX-{s}{c}" for s in "123" for c in "abcdef"]


@pytest.fixture(scope="module")
def problems():
    return {name: P.load_problem(name) for name in CH2}


def number(name: str) -> int:
    return int(name[-2:])


def henrys_side(p: P.Problem) -> list[str]:
    """The types of the objects on Henry's half, left to right [F95 p. 27 (img)]."""
    return [o.type for o in sorted(p.objects, key=lambda o: o.x) if o.y < TABLE_HEIGHT / 2]


def elizas_left(p: P.Problem) -> list[str]:
    return [o.type for o in sorted(p.objects, key=lambda o: o.x)
            if o.y > TABLE_HEIGHT / 2 and o.x < TABLE_WIDTH / 2]


def elizas_right(p: P.Problem) -> list[str]:
    return [o.type for o in p.objects if o.y > TABLE_HEIGHT / 2 and o.x > TABLE_WIDTH / 2]


# ---------------------------------------------------------------------------------------------
# The files.


def test_the_nineteen_files(problems):
    """Figures 2.2-2.20, two or three to a page, pp. 27-35 (PDF page + 12); the figures are
    the odd-numbered images `pdfimages` extracts (the even ones are their masks)."""
    assert P.by_family("ch2") == CH2
    for name, p in problems.items():
        n = number(name)
        assert p.title == f"Figure 2.{n}" and p.family == "ch2"
        assert p.source["page"] == PAGES[n] and p.source["pdf_page"] == PAGES[n] + 12
        assert p.source["image_index"] == 2 * n - 3
        assert p.source["figure"] == p.title


def test_every_file_validates_with_an_expectation(problems):
    """No E arrows in chapter 2: `answers` is empty and `expected` carries French's words,
    each quotation with its page [F95 pp. 26-35]."""
    for name, p in problems.items():
        data = json.loads(P.problem_path(name).read_text())
        assert P.validate(data) == [], name
        assert p.answers == {} and p.expected is not None
        exp = p.expected
        assert exp["plausible"] and all(i in p.ids() for i in exp["plausible"])
        assert exp["answer"] is None or exp["answer"] in exp["plausible"]
        assert (p.expected_object() is None) == (exp["answer"] is None)
        assert exp["quotes"]
        for q in exp["quotes"]:
            assert 26 <= q["page"] <= 41 and len(q["text"].split()) >= 6, name


def test_the_validator_guards_the_expectation():
    data = json.loads(P.problem_path("ch2-fig03").read_text())
    assert P.validate(data) == []
    bad = json.loads(json.dumps(data))
    del bad["expected"]
    assert any("expected" in e for e in P.validate(bad))
    bad = json.loads(json.dumps(data))
    bad["expected"]["answer"] = "plate9"
    assert any("not an object" in e for e in P.validate(bad))
    bad = json.loads(json.dumps(data))
    bad["expected"]["quotes"] = [{"text": "too short", "page": 26}]
    assert any("six words" in e for e in P.validate(bad))
    bad = json.loads(json.dumps(data))
    bad["expected"]["comparisons"] = [{"toward": "cup2", "more_than": "surround-05"}]
    assert any("comparison" in e for e in P.validate(bad))
    bad = json.loads(json.dumps(data))
    bad["expected"]["plausible"] = []
    assert any("plausible" in e for e in P.validate(bad))
    # A chapter 5 file still needs its labelled answers.
    ch5 = json.loads(P.problem_path("surround-01").read_text())
    ch5["answers"] = {}
    assert "no answers" in P.validate(ch5)


@pytest.mark.skipif(PDFTOTEXT_MISSING, reason="pdftotext is not installed")
@pytest.mark.skipif(not available("F95"), reason=skip_reason("F95"))
def test_the_expectations_quote_french(problems, tmp_path):
    """Every `expected` quotation, written as a tagged quotation, passes the checker."""
    lines = []
    for name, p in problems.items():
        for q in p.expected["quotes"]:
            lines.append(f'{name}: "{q["text"]}" [F95 p. {q["page"]}]')
    probe = tmp_path / "expected.md"
    probe.write_text("\n\n".join(lines) + "\n")
    proc = subprocess.run([sys.executable, str(CHECKER), str(probe)],
                          capture_output=True, text=True)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "FAIL" not in proc.stdout


def test_comparisons_name_chapter_2_problems(problems):
    """French's comparatives ('more likely than in figure 2.7') point at earlier figures."""
    for name, p in problems.items():
        for c in p.expected["comparisons"]:
            other = c.get("more_than") or c.get("less_than")
            assert other in CH2 and number(other) < number(name), (name, c)
            assert c["toward"] in p.expected["plausible"], (name, c)
    assert problems["ch2-fig06"].expected["comparisons"] == [
        {"toward": "saucer1", "more_than": "ch2-fig05"},
        {"toward": "saucer1", "less_than": "ch2-fig04"}]


def test_the_touched_object_is_henrys_cup(problems):
    """Henry touches a cup in every chapter 2 figure: at the top centre in Figures 2.2-2.6
    and in the upper left from Figure 2.7 on [F95 pp. 27-35 (img)]."""
    for name, p in problems.items():
        t = p.touched_object()
        assert t.id == "cup1" and t.type == "cup" and t.y < TABLE_HEIGHT / 3, name
        if number(name) <= 6:
            assert TABLE_WIDTH / 3 < t.x < 2 * TABLE_WIDTH / 3, name
        else:
            assert t.x < TABLE_WIDTH / 3, name


STRUCTURE = {
    # figure: (Henry's half, left to right; Eliza's left half; Eliza's right half)
    2: (["cup"], [], []),
    3: (["cup"], [], ["cup"]),                       # her cup faces his, at the centre
    4: (["cup"], [], ["small glass"]),
    5: (["cup"], [], ["fork"]),
    6: (["cup"], [], ["saucer"]),
    7: (["cup"], ["small glass"], ["cup"]),
    8: (["saucer", "knife", "fork", "cup", "salt shaker"],
        ["spoon", "small glass", "spoon", "knife"], ["cup"]),
    9: (["cup", "salt shaker", "fork", "knife", "saucer"],
        ["small glass", "spoon", "knife", "spoon"], ["cup"]),
    10: (["cup", "spoon", "fork", "spoon"],
         ["small glass", "knife", "knife", "fork", "fork"], ["cup"]),
    11: (["fork", "fork", "knife", "cup", "spoon", "spoon"],
         ["spoon", "spoon", "small glass", "fork", "fork", "knife"], ["cup"]),
    12: (["fork", "fork", "cup", "spoon", "spoon"],
         ["spoon", "spoon", "small glass", "fork", "fork"], ["cup"]),
    13: (["fork", "fork", "cup", "spoon", "spoon"],
         ["spoon", "spoon", "saucer", "fork", "fork"], ["cup"]),
    14: (["fork", "fork", "cup", "spoon", "spoon"],
         ["spoon", "spoon", "salt shaker", "fork", "fork"], ["cup"]),
    15: (["cup", "spoon"], ["small glass"], []),
    16: (["cup", "small glass"], ["small glass"], []),
    17: (["cup", "small glass", "small glass"], ["small glass", "small glass"], []),
    18: (["cup", "small glass", "small glass"], ["small glass", "spoon"], []),
    19: (["cup", "salt shaker", "plate"], ["small glass", "spoon"], []),
    20: (["cup", "salt shaker", "plate"], ["small glass"], []),
}


@pytest.mark.parametrize("n", sorted(STRUCTURE))
def test_each_figure_shows_what_french_describes(problems, n):
    """The objects of each figure, by half of the table, as the text and the image give
    them [F95 pp. 26-35]: the first series grows from one cup to the central problem and
    varies Eliza's object; the second series is the Blockage series [F95 p. 32]."""
    p = problems[f"ch2-fig{n:02d}"]
    henry, left, right = STRUCTURE[n]
    assert henrys_side(p) == henry, n
    if n == 3:
        assert elizas_right(p) == [] and [o.type for o in p.objects if o.y > 50] == ["cup"]
        assert TABLE_WIDTH / 3 < p.object("cup2").x < 2 * TABLE_WIDTH / 3
    elif n <= 6:
        assert [o.type for o in p.objects if o.y > 50] == right
    else:
        assert elizas_left(p) == left and elizas_right(p) == right, n
    for o in p.objects:
        if o.type in ("fork", "knife", "spoon"):
            assert o.orientation == "up", (n, o.id)


def test_figure_2_11_is_surround_5(problems):
    """Figure 2.11 [F95 p. 31 (img)] is the configuration of Surround 5 [F95 p. 118 (img)]
    and of the chapter 4 run: the same thirteen ids and types, every position within 2.5
    units on an independent drawing (the uncertainty check of D-003 for chapter 2)."""
    a, b = problems["ch2-fig11"], P.load_problem("surround-05")
    assert a.ids() == b.ids()
    for o in a.objects:
        q = b.object(o.id)
        assert o.type == q.type
        assert abs(o.x - q.x) <= 2.5 and abs(o.y - q.y) <= 2.5, (o.id, o.x, o.y, q.x, q.y)
    assert "surround-05" in a.uncertainty["note"]


VARIANTS = {
    # name: (base, ids removed from the base, ids added)
    "ch2-fig03": ("ch2-fig02", [], ["cup2"]),
    "ch2-fig04": ("ch2-fig03", ["cup2"], ["small-glass1"]),
    "ch2-fig05": ("ch2-fig04", ["small-glass1"], ["fork1"]),
    "ch2-fig06": ("ch2-fig05", ["fork1"], ["saucer1"]),
    "ch2-fig12": ("ch2-fig11", ["knife1", "knife2"], []),
    "ch2-fig13": ("ch2-fig12", ["small-glass1"], ["saucer1"]),
    "ch2-fig14": ("ch2-fig13", ["saucer1"], ["salt-shaker1"]),
    "ch2-fig16": ("ch2-fig15", ["spoon1"], ["small-glass2"]),
    "ch2-fig17": ("ch2-fig16", [], ["small-glass3", "small-glass4"]),
    "ch2-fig18": ("ch2-fig17", ["small-glass4"], ["spoon2"]),
    "ch2-fig19": ("ch2-fig18", ["small-glass2", "small-glass3"], ["salt-shaker1", "plate1"]),
    "ch2-fig20": ("ch2-fig19", ["spoon2"], []),
}


@pytest.mark.parametrize("name", sorted(VARIANTS))
def test_the_variants_differ_from_their_bases_only_where_french_says(problems, name):
    """A variant keeps its base's ids; the shared objects stay within 5 units (D-003), except
    that Figure 2.12's forks close up by 7 units where the knife was removed [F95 p. 31
    (img)]; the change is exactly the one French describes [F95 pp. 26-35]."""
    base, removed, added = VARIANTS[name]
    p, b = problems[name], problems[base]
    tolerance = 8.0 if name == "ch2-fig12" else 5.0
    assert p.based_on == base
    assert sorted(set(b.ids()) - set(p.ids())) == sorted(removed), name
    assert sorted(set(p.ids()) - set(b.ids())) == sorted(added), name
    for o in p.objects:
        if o.id in b.ids():
            q = b.object(o.id)
            assert o.type == q.type
            assert distance(o.x, o.y, q.x, q.y) <= tolerance, (name, o.id)


def test_figures_2_8_to_2_11_surround_figure_2_7(problems):
    """Figures 2.8-2.11 keep the cup, the glass and the lone cup of Figure 2.7 and add
    objects around the first two [F95 pp. 28-30]."""
    base = problems["ch2-fig07"]
    for n in (8, 9, 10, 11):
        p = problems[f"ch2-fig{n:02d}"]
        assert p.based_on == "ch2-fig07"
        for id_ in ("cup1", "small-glass1", "cup2"):
            o, q = p.object(id_), base.object(id_)
            assert o.type == q.type and distance(o.x, o.y, q.x, q.y) <= 6.0, (n, id_)
        assert len(p.objects) > 3


def test_figure_2_17s_pairs_are_close_and_diagonal(problems):
    """Two identical pairs 'diagonally opposite one another' [F95 p. 34]: each pair's glasses
    less than a glass width apart, one pair upper right, the other lower left."""
    p = problems["ch2-fig17"]
    width = footprint("small glass")[0]
    g2, g3, g1, g4 = (p.object(i) for i in ("small-glass2", "small-glass3", "small-glass1",
                                             "small-glass4"))
    assert 0 < g3.x - g2.x - width < width and abs(g3.y - g2.y) < 1
    assert 0 < g4.x - g1.x - width < width and abs(g4.y - g1.y) < 1
    assert g2.x > 2 * TABLE_WIDTH / 3 and g2.y < TABLE_HEIGHT / 3
    assert g1.x < TABLE_WIDTH / 3 and g1.y > 2 * TABLE_HEIGHT / 3


# ---------------------------------------------------------------------------------------------
# The cross-check against FCCA's figures [FCCA pp. 394-399 (img)].


def region_of(o: P.TableObject, slack: float = 0.04) -> set[str]:
    """The ninth(s) of the table an object may be read in: its own, and a neighbour's when
    it stands within `slack` of the boundary (the nine blurry regions [F95 p. 85])."""
    fx, fy = o.x / TABLE_WIDTH, o.y / TABLE_HEIGHT
    cols = {("left", "centre", "right")[min(2, int(f * 3))] for f in (fx - slack, fx, fx + slack)
            if 0 <= f <= 1}
    rows = {("upper", "middle", "lower")[min(2, int(f * 3))] for f in (fy - slack, fy, fy + slack)
            if 0 <= f <= 1}
    return {f"{r} {c}" for r in rows for c in cols}


@pytest.fixture(scope="module")
def crosscheck():
    return json.loads((DATA / "fcca_crosscheck.json").read_text())


def test_the_crosscheck_covers_all_eighteen_layouts(crosscheck):
    figs = crosscheck["figures"]
    assert [f["figure"] for f in figs] == FCCA_FIGURES
    assert [f["page"] for f in figs] == sorted(f["page"] for f in figs)
    assert all(394 <= f["page"] <= 399 for f in figs)
    assert crosscheck["source"] and crosscheck["method"] and "square" in crosscheck["frame"]
    problems = [f["problem"] for f in figs]
    assert len(set(problems)) == 18 and all(p in P.catalogue() for p in problems)


def test_every_fcca_object_matches_ours_object_by_object(crosscheck):
    """Each object FCCA draws is one of the file's objects, of the same type, in the same
    ninth of the table (or at its edge); the touched object carries H and the answers their
    labels; the file's objects are all accounted for."""
    for entry in crosscheck["figures"]:
        p = P.load_problem(entry["problem"])
        assert entry["title"] == p.title
        seen = []
        for rec in entry["objects"]:
            o = p.object(rec["id"])
            assert o.type == rec["type"], (entry["figure"], rec["id"])
            assert rec["region"] in region_of(o), (entry["figure"], rec["id"], rec["region"])
            labels = ([] if o.id != p.touched else ["H"]) + \
                     [k for k, v in p.answers.items() if v == o.id]
            assert rec["label"] == ("/".join(labels) or None), (entry["figure"], rec["id"])
            seen.append(o.id)
        assert sorted(seen) == sorted(p.ids()), entry["figure"]
        assert entry["missing_in_fcca"] == []


def test_the_disagreements_are_surround_10_and_blockage_9(crosscheck):
    """The book's figure wins (ANSWERS.md 4): FCCA's IX-1f has a sixth junk object, a cup
    [FCCA p. 395]; FCCA's IX-2f draws the left upper-right glass 8 units further left."""
    disagreeing = [f["figure"] for f in crosscheck["figures"] if not f["agrees"]]
    assert disagreeing == ["IX-1f", "IX-2f"]
    by = {f["figure"]: f for f in crosscheck["figures"]}
    assert by["IX-1f"]["extra_in_fcca"][0]["type"] == "cup"
    assert all(f["extra_in_fcca"] == [] for f in crosscheck["figures"] if f["figure"] != "IX-1f")
    glass = [o for o in by["IX-2f"]["objects"] if o["id"] == "small-glass2"][0]
    assert glass["agrees"] is False and abs(glass["fcca_fx"] - 0.649) < 1e-9
    ours = P.load_problem("blockage-09").object("small-glass2").x / TABLE_WIDTH
    assert 0.06 <= ours - glass["fcca_fx"] <= 0.08
    assert all(o["agrees"] for f in crosscheck["figures"] for o in f["objects"]
               if not (f["figure"] == "IX-2f" and o["id"] == "small-glass2"))


def test_the_crosscheck_table_in_the_docs_covers_all_eighteen(crosscheck):
    """`docs/problems.md` 6 tabulates the eighteen figures, one row each, with the problem's
    title and the verdict; the two disagreements are in their rows."""
    text = DOCS.read_text()
    section = text.split("## 6.")[1].split("\n## ")[0]
    rows = {m.group(1): m.group(0) for m in re.finditer(r"^\| (IX-\d[a-f]) \|.*$", section,
                                                        re.MULTILINE)}
    assert sorted(rows) == sorted(FCCA_FIGURES)
    for entry in crosscheck["figures"]:
        assert entry["title"] in rows[entry["figure"]], entry["figure"]
        assert ("agree" in rows[entry["figure"]]) == entry["agrees"] or not entry["agrees"]
    assert "cup" in rows["IX-1f"] and "8 units" in rows["IX-2f"]

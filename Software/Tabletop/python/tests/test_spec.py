"""The shape of SPEC.md: its sections, its citation tags and its quotations.

The quotations themselves are checked against the sources by ``test_quotes.py`` (through
``tools/check_quotes.py``); this file checks what can be checked without the PDFs: that the
sections TASK.md asks for are there, that every citation tag names a page the source has, and
that the written sections carry checked quotations (item 01: sections 1 and 2; item 02:
section 3; item 03: section 4) [F95 p. 25],
[FCCA p. 377], [FH91 p. 175].
"""

import re
from pathlib import Path

SPEC = Path(__file__).resolve().parents[1] / "SPEC.md"

# The printed pages each source has (the checker's page maps; F95 by its "Page N" markers).
PAGE_RANGES = {"F95": (1, 190), "FCCA": (1, 518), "FH91": (175, 182)}

# One citation: `F95 p. 65`, `FCCA pp. 353-354`, with an optional `(img)`; a bracket may hold
# several, separated by semicolons (`[F95 p. 64; FH91 p. 177]`).
CITATION = r"(F95|FCCA|FH91)\s+(pp?)\.\s*(\d+)(?:-(\d+))?(?:\s*\(img\))?"
TAG = re.compile(r"\[" + CITATION + r"(?:;\s*" + CITATION + r")*\]")
ONE = re.compile(CITATION)
QUOTE = re.compile(r'"([^"\n][^"]{8,1500}?)"[^"\[\n]{0,12}\[(F95|FCCA|FH91)\s+pp?\.\s*\d+')


def text() -> str:
    return SPEC.read_text()


def test_the_numbered_sections_are_present_in_order():
    heads = re.findall(r"^## (\d)\. ", text(), flags=re.M)
    assert heads == ["1", "2", "3", "4", "5", "6", "7"]


def test_sections_one_and_two_have_their_subsections():
    t = text()
    for head in ("### 1.1 The game", "### 1.2 The objects", "### 1.3 The table",
                 "### 1.4 Realistic tables only", "### 1.5 The input",
                 "### 1.6 Chapter 2's nineteen configurations",
                 "### 2.2 The three components", "### 2.3 French's six differences from Copycat",
                 "### 2.4 The eleven features", "### 2.6 The flow of a run",
                 "### 2.7 The end of a run and its output"):
        assert head in t, head


def test_section_three_has_its_subsections():
    t = text()
    for head in ("### 3.1 The nodes", "### 3.2 Conceptual depth and the three levels",
                 "### 3.3 Links", "### 3.4 Lengths that change with the label's activation",
                 "### 3.5 Conceptual distance over several paths", "### 3.6 Activation",
                 "### 3.7 Label nodes", "### 3.8 The initial state",
                 "### 3.9 Concepts post codelets",
                 "### 3.10 One node, several activations: the context push and pop",
                 "### 3.11 The unspecified points of the Slipnet"):
        assert head in t, head


def test_section_three_lists_every_slipnet_decision():
    # PLAN.md numbers the Slipnet's open points D-007 to D-021; each is named in 3.11 [F95
    # pp. 55-63, 82-89].
    t = text()
    table = t[t.index("### 3.11 "):t.index("## 4. ")]
    found = sorted(set(re.findall(r"D-0(\d\d)", table)))
    assert found == [f"{n:02d}" for n in range(7, 22)], found


def test_the_forty_seven_nodes_and_the_forty_one_cells():
    # F95 p. 62 lists 47 concepts in five groups; the screen's grid has 41 cells [F95 p. 98
    # (img)].
    t = text()
    section = t[t.index("### 3.1 "):t.index("### 3.2 ")]
    groups = re.findall(r"^\| [A-Z][a-z ]+ \((\d+)\) \|", section, flags=re.M)
    assert sum(int(g) for g in groups) == 47, groups
    rows = re.findall(r"^\| [1-6] \| (.+) \|$", section, flags=re.M)
    cells = [c.strip() for r in rows for c in r.split("|") if c.strip() not in ("", "—")]
    assert len(rows) == 6 and len(cells) == 41, (len(rows), len(cells))
    assert cells[0] == "spoon" and "opposite" in cells and "saucer" not in cells


def test_the_six_differences_and_the_eleven_features_are_enumerated():
    t = text()
    six = t[t.index("### 2.3 "):t.index("### 2.4 ")]
    assert re.findall(r"^\d+\. \*\*", six, flags=re.M) == [f"{i}. **" for i in range(1, 7)]
    eleven = t[t.index("### 2.4 "):t.index("### 2.5 ")]
    assert [int(n) for n in re.findall(r"^(\d+)\. ", eleven, flags=re.M)] == list(range(1, 12))


def test_the_nineteen_configurations_are_tabulated():
    t = text()
    section = t[t.index("### 1.6 "):t.index("### 1.7 ")]
    figures = re.findall(r"^\| (2\.\d+) \| (\d+) \|", section, flags=re.M)
    assert [f for f, _ in figures] == [f"2.{n}" for n in range(2, 21)]
    assert all(26 <= int(p) <= 35 for _, p in figures)


def test_every_citation_tag_is_well_formed_and_in_range():
    t = text()
    loose = re.findall(r"\[(?:F95|FCCA|FH91)[^\]]*\]", t)
    bad = [m for m in loose if not TAG.fullmatch(m)]
    assert not bad, f"malformed tags: {bad}"
    for m in TAG.finditer(t):
        for source, pp, first, last in ONE.findall(m.group(0)):
            check_range(source, pp, first, last)


def check_range(source: str, pp: str, first: str, last: str) -> None:
    lo, hi = PAGE_RANGES[source]
    a, b = int(first), int(last) if last else int(first)
    assert lo <= a <= b <= hi, (source, first, last)
    assert (pp == "pp") == bool(last), f"'p.' for one page, 'pp.' for a range: {source} {first}-{last}"


def test_sections_one_and_two_carry_checked_quotations():
    t = text()
    body = t[t.index("## 1. "):t.index("## 3. ")]
    assert len(QUOTE.findall(body)) >= 100


def test_section_three_carries_checked_quotations():
    t = text()
    body = t[t.index("## 3. "):t.index("## 4. ")]
    assert len(QUOTE.findall(body)) >= 100


def test_no_nested_double_quotes_inside_a_quotation():
    # A quotation with an inner double quote is invisible to the checker; inner quotes are
    # written as single quotes (the match ignores punctuation).
    assert '""' not in text()


def test_section_four_has_its_subsections():
    t = text()
    for head in ("### 4.1 The Workspace and what lives in it",
                 "### 4.2 What is known at the start, and scanning", "### 4.3 Descriptions",
                 "### 4.4 Groups", "#### 4.4.1 Gestalt groups, found first",
                 "#### 4.4.2 Bottom-up groups: sameness, shared-ISA, and the rest",
                 "#### 4.4.3 Group strength and fights between groups",
                 "#### 4.4.4 Subgroups, and the fault of keeping them all",
                 "#### 4.4.5 Ends, middles, neighbours and orientation", "### 4.5 Salience",
                 "### 4.6 Correspondences", "#### 4.6.1 What a correspondence is",
                 "#### 4.6.2 The static factors", "#### 4.6.3 The dynamic factors and the bonus",
                 "#### 4.6.4 Creation, testing, competition", "### 4.7 The Worldview",
                 "#### 4.7.1 Definition", "#### 4.7.2 Rivals and incoherence",
                 "#### 4.7.3 Competition: the subjunctive Rival Worldview",
                 "#### 4.7.4 Losers stay, fade and go",
                 "#### 4.7.5 The rating of a Worldview, and FCCA's structure-value",
                 "### 4.8 The unspecified points of the Workspace"):
        assert head in t, head


def test_section_four_lists_every_workspace_decision():
    # PLAN.md numbers the Workspace's open points D-022 to D-046; each is named in 4.8 [F95
    # pp. 63-75; FCCA pp. 381-391].
    t = text()
    table = t[t.index("### 4.8 "):t.index("## 5. ")]
    found = sorted(set(re.findall(r"D-0(\d\d)", table)))
    assert found == [f"{n:02d}" for n in range(22, 47)], found


def test_the_salience_and_correspondence_factors_are_enumerated():
    # Ten salience factors, seven for objects and three for groups [F95 pp. 66-67]; eight in
    # FCCA's list [FCCA p. 385]; nine static and three dynamic factors of a correspondence
    # [F95 pp. 68-69]; three terms of the Worldview's rating [F95 p. 74].
    t = text()
    salience = t[t.index("### 4.5 "):t.index("### 4.6 ")]
    assert [int(n) for n in re.findall(r"^(\d+)\. \*\*", salience, flags=re.M)] == list(range(1, 11))
    assert len(re.findall(r'^\| "[^"]+" \[FCCA p\. 385\] \|', salience, flags=re.M)) == 8
    static = t[t.index("#### 4.6.2 "):t.index("#### 4.6.3 ")]
    assert [int(n) for n in re.findall(r"^(\d+)\. \*\*", static, flags=re.M)] == list(range(1, 10))
    dynamic = t[t.index("#### 4.6.3 "):t.index("#### 4.6.4 ")]
    assert [int(n) for n in re.findall(r"^(\d+)\. \*\*", dynamic, flags=re.M)] == [1, 2, 3]
    rating = t[t.index("#### 4.7.5 "):t.index("### 4.8 ")]
    assert [int(n) for n in re.findall(r"^(\d+)\. \*\*", rating, flags=re.M)] == [1, 2, 3]


def test_section_four_carries_checked_quotations():
    t = text()
    body = t[t.index("## 4. "):t.index("## 5. ")]
    assert len(QUOTE.findall(body)) >= 250


def test_sections_five_and_six_have_their_subsections():
    t = text()
    for head in ("### 5.1 Codelets and what one does",
                 "### 5.2 The catalogue: the names in the text and the 23 labels on the screen",
                 "### 5.3 Posting: top-down from the Slipnet, and follow-ups",
                 "### 5.4 Urgency and the choice of a codelet",
                 "### 5.5 Urgency explosion and generations", "### 5.6 Capacity and age",
                 "### 5.7 The unspecified points of the Coderack",
                 "### 6.1 What temperature means in Tabletop", "### 6.2 The four factors",
                 "### 6.3 The formula", "### 6.4 What temperature does",
                 "### 6.5 Find-answer and the end of a run", "### 6.6 The output of a run",
                 "### 6.7 The unspecified points of temperature and the answer",
                 "### 6b. French's faults", "### 6c. The chapter 4 run"):
        assert head in t, head


def test_sections_five_and_six_list_every_coderack_and_temperature_decision():
    # PLAN.md numbers temperature D-047 to D-050, the Coderack D-051 to D-056 and the answer
    # D-057 to D-063 [F95 pp. 75-94].
    t = text()
    coderack = t[t.index("### 5.7 "):t.index("## 6. ")]
    assert sorted(set(re.findall(r"D-0(\d\d)", coderack))) == [f"{n}" for n in range(51, 57)]
    answer = t[t.index("### 6.7 "):t.index("### 6b. ")]
    found = sorted(set(re.findall(r"D-0(\d\d)", answer)))
    assert found == [f"{n}" for n in (47, 48, 49, 50, 57, 58, 59, 60, 61, 62, 63)], found


def test_the_twenty_three_labels_and_the_six_alternatives():
    # Fig. 4.9's strip carries 23 labels [F95 p. 107 (img)]; French weighs six answers to
    # urgency explosion and adopts the sixth [F95 pp. 79-81].
    t = text()
    catalogue = t[t.index("### 5.2 "):t.index("### 5.3 ")]
    rows = re.findall(r"^\| (\d+(?:-\d+)?) \| ([A-Z0-9, -]+) \|", catalogue, flags=re.M)
    labels = [lab.strip() for _, labs in rows for lab in labs.split(",")]
    assert len(labels) == 23 and labels[0] == "FND-GP" and labels[-1] == "FND-ANS", labels
    explosion = t[t.index("### 5.5 "):t.index("### 5.6 ")]
    assert [int(n) for n in re.findall(r"^(\d)\. ", explosion, flags=re.M)] == list(range(1, 7))
    factors = t[t.index("### 6.2 "):t.index("### 6.3 ")]
    assert [int(n) for n in re.findall(r"^(\d)\. \*\*", factors, flags=re.M)] == [1, 2, 3, 4]


def test_the_faults_table_names_the_pages_french_criticizes_himself_on():
    # ANSWERS.md 2: at least pp. 89, 109, 125 and 140, each with its decision.
    t = text()
    faults = t[t.index("### 6b. "):t.index("### 6c. ")]
    rows = re.findall(r"^\| (\d) \| .+ \| \[F95 pp?\. (\d+)(?:-\d+)? *\] \| (D-0\d\d) \|$", faults,
                      flags=re.M)
    assert len(rows) >= 7, rows
    pages = {int(p) for _, p, _ in rows}
    assert {89, 109, 125, 140}.issubset(pages), pages
    assert len({d for _, _, d in rows}) == len(rows)  # one decision per fault
    assert "FIXED" in faults and "french-faults" in faults


def test_sections_five_and_six_carry_checked_quotations():
    t = text()
    body = t[t.index("## 5. "):t.index("### 6c. ")]
    assert len(QUOTE.findall(body)) >= 150


def test_section_six_c_has_its_points_and_the_run_table():
    # §6c numbers its observations 6c.1-6c.14 and tabulates the nine dumps [F95 pp. 96-107].
    t = text()
    body = t[t.index("### 6c. "):t.index("## 7. ")]
    points = re.findall(r"^\*\*6c\.(\d+) ", body, flags=re.M)
    assert [int(p) for p in points] == list(range(1, 15)), points
    rows = re.findall(r"^\| 4\.(\d) \| (\d+) \| ([\d.()a-z ]+) \|", body, flags=re.M)
    assert [int(r[0]) for r in rows] == list(range(1, 10))
    assert [int(r[1]) for r in rows] == [0, 2, 12, 22, 33, 36, 47, 47, 55]
    assert "pick-strong-cp" in body and "Surround 5" in body


def test_section_six_c_carries_checked_quotations():
    t = text()
    body = t[t.index("### 6c. "):t.index("## 7. ")]
    assert len(QUOTE.findall(body)) >= 12


def test_section_seven_has_its_subsections_and_the_thirty_five_sets():
    # §7 tabulates chapter 5's result sets and the three human tables [F95 pp. 115-144].
    t = text()
    body = t[t.index("## 7. "):]
    heads = re.findall(r"^### 7\.(\d) ", body, flags=re.M)
    assert [int(h) for h in heads] == list(range(1, 8)), heads
    rows = re.findall(r"^\| (Surround \d+|Blockage \d+|Buridan \d+(?: \((?:original|modified)\))?|"
                      r"Figure/Ground 1) \| (\d+)", body, flags=re.M)
    assert len(rows) == 35 + 23            # chapter 5's sets, then the 23 table rows
    assert rows[0] == ("Surround 1", "5") and rows[34] == ("Figure/Ground 1", "100")
    assert "IX-1d" in body and "IX-1f" in body and "Bur18" in body
    for item in ("1.", "2.", "3.", "4.", "5.", "6.", "7.", "8.", "9.", "10."):
        assert re.search(rf"^{re.escape(item)} \*\*", body, flags=re.M), item


def test_section_seven_carries_checked_quotations():
    t = text()
    body = t[t.index("## 7. "):]
    assert len(QUOTE.findall(body)) >= 25

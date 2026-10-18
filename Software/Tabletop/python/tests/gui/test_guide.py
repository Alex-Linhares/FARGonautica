"""Loop0006 items 16 and 17: the help page's model (`tabletop/gui/models/guide.py`; G-053)
and the user guide (`docs/gui/panes.md`; G-055),
Qt-free, after the Seek-Whence GUI's `test_guide.py`.

- The built-in table has one row per pane of PANES.md (the fourteen docks, the table and
  the timeline), each with a name, what it shows and pages that cite a source or a
  precedent; the new designs are marked.
- The parser round-trips the table through Markdown and refuses a bad one; `guide_rows`
  prefers a file and falls back to the built-in table.
- The HTML lists every pane with its pages, marks the GUI designs in bold and says what to
  expect from the dynamics (ANSWERS.md 11).
"""

import html
import re

import pytest

from tabletop.gui.models import guide as gm

EXPECTED_PANES = ("P1", "P2", "P2b", "P3", "P3b", "P4", "P4b", "P5", "P6", "P7", "P8", "P9",
                  "P10", "P11", "P12", "P13", "P14")
TAG = re.compile(r"\[(F95|FCCA|FH91) pp?\. \d+|Numbo GUI|Seek-Whence GUI|ANSWERS\.md")


def test_the_table_has_one_row_per_pane_with_its_pages():
    panes = tuple(r.pane for r in gm.PANE_ROWS)
    assert panes == EXPECTED_PANES
    for row in gm.PANE_ROWS:
        assert row.name and row.shows and row.drawing, row.pane
        assert TAG.search(row.pages), row.pane
        assert gm.pages_of(row.pane) == row.pages and gm.row_of(row.pane) is row
    with pytest.raises(KeyError):
        gm.pages_of("P99")


def test_the_table_row_puts_henry_where_the_table_model_does():
    """The guide's words agree with the picture: the table model writes Henry's name at
    y = 0, the top edge, as the dumps print it [F95 p. 98 (img)] (the review of item 20
    corrected the row, which said the opposite; G-059)."""
    from tabletop.gui.models import table as tm
    from tabletop.problems import load_problem
    model = tm.TableModel()
    model.set_problem(load_problem("surround-05"))
    names = {n.text: n.y for n in model.layout().names}
    assert names[tm.HENRY] == 0.0 and names[tm.ELIZA] == model.height
    row = gm.row_of("P1")
    assert "Henry's side above and Eliza's below" in row.shows


def test_the_new_designs_are_marked_and_the_rest_are_not():
    """ANSWERS.md 4: the panes with no figure in the sources say so; a pane with only an
    overlay or an editor of the GUI's names it in the text (G-053)."""
    whole = {r.pane for r in gm.PANE_ROWS if r.gui_design}
    assert whole == {"P2b", "P3b", "P4b", "P5", "P6", "P14"}
    for pane in ("P1", "P7"):
        row = gm.row_of(pane)
        assert gm.GUI_DESIGN in row.pages and gm.GUI_DESIGN in row.drawing and not row.gui_design
    assert gm.ICONS_NOTE in gm.row_of("P1").pages and gm.ICONS_NOTE in gm.row_of("P7").drawing
    assert "[F95 pp. 115-139]" in gm.row_of("P8").pages


def markdown(rows):
    lines = ["| " + " | ".join(gm.HEADER) + " |", "|---|---|---|---|---|"]
    lines += [f"| {r.pane} | {r.name} | {r.shows} | {r.pages} | {r.drawing} |" for r in rows]
    return "\n".join(["# A guide", "", "Some prose.", ""] + lines + ["", "More prose."])


def test_the_parser_round_trips_the_table_and_refuses_a_bad_one():
    assert gm.parse_guide(markdown(gm.PANE_ROWS)) == gm.PANE_ROWS
    with pytest.raises(ValueError):
        gm.parse_guide("# no table here\n\n| a | b |\n|---|---|\n| 1 | 2 |\n")
    with pytest.raises(ValueError):
        gm.parse_guide(markdown(gm.PANE_ROWS[:2]) .replace("| P2 |", "| P2 | extra |"))


def test_guide_rows_prefers_a_file_and_falls_back(tmp_path):
    path = tmp_path / "panes.md"
    path.write_text(markdown(gm.PANE_ROWS[:3]), encoding="utf-8")
    assert gm.guide_rows(path) == gm.PANE_ROWS[:3]
    assert gm.read_guide(path) == gm.PANE_ROWS[:3]
    assert gm.guide_rows(tmp_path / "missing.md") == gm.PANE_ROWS
    path.write_text("no table\n", encoding="utf-8")
    assert gm.guide_rows(path) == gm.PANE_ROWS
    assert gm.GUIDE_PATH.name == "panes.md" and gm.GUIDE_PATH.parent.name == "gui"


def test_the_html_lists_every_pane_with_its_pages_and_what_to_expect():
    text = gm.guide_html()
    for row in gm.PANE_ROWS:
        assert f"<b>{row.pane}</b> {row.name}</td>" in text, row.pane
        assert html.escape(row.pages, quote=False) in text, row.pane
    assert text.count("<b>GUI design, not French's") == 6 + 1      # the rows (P14 since the roles loop) and the key
    assert "What to expect" in text and "no context push in this run" in text
    assert "RESULTS.md 6.4" in text and gm.ICONS_NOTE in text
    assert "[F95 p. N]" in text and "[FCCA p. N]" in text
    lines = gm.guide_lines()
    assert len(lines) == len(gm.PANE_ROWS)
    assert lines[0].startswith("P1 Table: ") and lines[4].endswith(gm.GUI_DESIGN)
    assert not lines[0].endswith(gm.GUI_DESIGN)


# -- the user guide, docs/gui/panes.md (item 17; G-055) -----------------------------------------

DOCK_TITLES = ("Controls", "Palette and editor", "Stats", "Thermometer", "Slipnet grid",
               "Slipnet network", "Worldview", "Competitions", "Temperature plot",
               "Roles", "Configuration", "Coderack bars", "Rack list", "Trace and event log",
               "Results")
SECTIONS = ("## Starting", "## The window", "## The panes", "## What to expect",
            "## Sessions and replay", "## Changing the default layout")


def guide_text() -> str:
    assert gm.GUIDE_PATH.is_file(), gm.GUIDE_PATH
    return gm.GUIDE_PATH.read_text(encoding="utf-8")


def test_the_guides_table_is_the_models():
    """Help's page is generated from the guide: the file's table parses to exactly
    `PANE_ROWS`, so the pane, its tooltip and the guide are one set of words (G-053,
    G-055)."""
    assert gm.read_guide() == gm.PANE_ROWS
    assert gm.guide_rows() == gm.PANE_ROWS
    assert gm.guide_html(gm.guide_rows()) == gm.guide_html()


def test_the_guide_names_every_pane_and_every_dock():
    """One section per pane in the panes' order, each dock's title and the central table
    and the timeline named, the new designs said in their sections (ANSWERS.md 4; G-055)."""
    text = guide_text()
    headings = re.findall(r"^### (P\d+b?)\. (.+)$", text, re.M)
    assert [h[0] for h in headings] == list(EXPECTED_PANES)
    assert dict(headings) == {r.pane: r.name for r in gm.PANE_ROWS}
    for title in DOCK_TITLES + ("Table", "Timeline"):
        assert title in text, title
    for section in SECTIONS:
        assert section in text, section
    for row in gm.PANE_ROWS:
        body = text.split(f"### {row.pane}. {row.name}", 1)[1].split("\n### ", 1)[0]
        if row.gui_design:
            assert body.startswith(f"\n\n**{gm.GUI_DESIGN}.**"), row.pane
    assert gm.ICONS_NOTE.split(":")[1].strip() in text        # new drawings, not French's


def test_the_guide_says_what_to_expect_and_cites_only_known_decisions():
    """The expectations of ANSWERS.md 11 with their RESULTS.md sections; every `G-NNN` is
    a heading of GUI-DECISIONS.md and every `D-NNN` of DECISIONS.md (G-055)."""
    text = guide_text()
    python = gm.GUIDE_PATH.parents[2]
    for line in gm.EXPECTATIONS:
        for section in re.findall(r"RESULTS\.md (\d\.\d)", line):
            assert f"RESULTS.md §{section}" in text, section
    assert "no context push in this run" in text and "no competition in this run" in text
    assert "one correspondence; no pairs to rate" in text
    decisions = (python / "DECISIONS.md").read_text(encoding="utf-8")
    gui_decisions = (python / "GUI-DECISIONS.md").read_text(encoding="utf-8")
    known_d = set(re.findall(r"^### (D-\d{3})", decisions, re.M))
    known_g = set(re.findall(r"^### (G-\d{3})", gui_decisions, re.M))
    assert set(re.findall(r"\bD-\d{3}\b", text)) <= known_d
    assert set(re.findall(r"\bG-\d{3}\b", text)) <= known_g
    assert "G-055" in text

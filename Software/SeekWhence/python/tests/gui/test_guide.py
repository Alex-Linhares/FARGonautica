"""Loop0004 item 16: the help page's model, Qt-free (`seekwhence/gui/models/guide.py`;
G-055; PANES.md section 4; ANSWERS.md 4).

- `docs/gui/panes.md` has the pane-to-pages table: every pane P1-P13 once, in order, each
  with pages or a precedent, and P5 and P6 marked GUI design, not Meredith's.
- The lines and the HTML name every pane with its pages and mark the two drawings.
- A text without the table, or a short row, is refused.
"""

import pytest

from seekwhence.gui.models import guide

PANE_IDS = tuple(f"P{n}" for n in range(1, 14))


def test_the_guide_lists_every_pane_once_with_its_pages():
    rows = guide.read_guide()
    assert tuple(r.pane for r in rows) == PANE_IDS
    for r in rows:
        assert r.name and r.shows and r.pages, r.pane
        assert "[M86" in r.pages or "[FCCA" in r.pages or "Numbo GUI" in r.pages \
            or "ANSWERS.md" in r.pages, r.pane


def test_the_platoplasm_and_the_taskrack_are_gui_design():                  # ANSWERS.md 4
    rows = {r.pane: r for r in guide.read_guide()}
    assert rows["P5"].gui_design and rows["P6"].gui_design
    assert not any(r.gui_design for p, r in rows.items() if p not in ("P5", "P6"))
    assert rows["P2"].drawing.startswith("Meredith's")


def test_the_lines_and_the_html():
    rows = guide.read_guide()
    lines = guide.guide_lines(rows)
    assert len(lines) == 13
    assert lines[0].startswith("P1 Sequence and input: ") and "[M86 pp. 7, 37" in lines[0]
    assert lines[4].endswith(guide.GUI_DESIGN) and lines[5].endswith(guide.GUI_DESIGN)
    assert not lines[1].endswith(guide.GUI_DESIGN)
    html = guide.guide_html(rows)
    for r in rows:
        assert f"<b>{r.pane}</b> {r.name}" in html and r.pages.replace("'", "&#x27;") in html
    assert html.count(f"<b>{guide.GUI_DESIGN.replace(chr(39), '&#x27;')}</b>") == 3   # the note and the two rows


@pytest.mark.parametrize("text", [
    "# nothing\n\nno table here\n",
    "| Pane | Name | What it shows | Pages | Drawing |\n|---|---|---|---|---|\n| P1 | only two |\n",
])
def test_a_text_without_the_table_or_a_short_row_is_refused(text):
    with pytest.raises(ValueError):
        guide.parse_guide(text)

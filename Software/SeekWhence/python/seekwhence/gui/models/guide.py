"""The help page's model (item 16; G-055): the pane-to-pages table of the user guide,
`docs/gui/panes.md`, read into plain rows for Help > "What am I looking at?" (PANES.md
section 4). The table marks the platoplasm network (P5) and the taskrack drawing (P6)
as GUI design, not Meredith's (ANSWERS.md 4). Qt-free; the window turns the rows into
HTML with `guide_html`.
"""

import dataclasses
import html
from pathlib import Path

__all__ = ["GUIDE_PATH", "GUI_DESIGN", "GuideRow", "read_guide", "parse_guide",
           "guide_lines", "guide_html"]

GUIDE_PATH = Path(__file__).resolve().parents[3] / "docs" / "gui" / "panes.md"
GUI_DESIGN = "GUI design, not Meredith's"
HEADER = ("Pane", "Name", "What it shows", "Pages", "Drawing")


@dataclasses.dataclass(frozen=True)
class GuideRow:
    """One pane of the table (G-055): its id (`P5`), name, what it shows, the pages or
    precedent behind it, and its `drawing` (whose figure it follows, or GUI_DESIGN)."""
    pane: str
    name: str
    shows: str
    pages: str
    drawing: str

    @property
    def gui_design(self) -> bool:
        """Whether the drawing is the GUI's own, not Meredith's (ANSWERS.md 4; G-055)."""
        return GUI_DESIGN in self.drawing


def _cells(line: str) -> list:
    return [cell.strip() for cell in line.strip().strip("|").split("|")]


def parse_guide(text: str) -> tuple:
    """The rows of the first table of `text` whose header is HEADER, in order (G-055);
    a `ValueError` when there is none."""
    lines = text.splitlines()
    for i, line in enumerate(lines):
        if line.startswith("|") and tuple(_cells(line)) == HEADER:
            rows = []
            for row in lines[i + 2:]:
                if not row.startswith("|"):
                    break
                cells = _cells(row)
                if len(cells) != len(HEADER):
                    raise ValueError(f"a row of the panes table has {len(cells)} cells: {row!r}")
                rows.append(GuideRow(*cells))
            return tuple(rows)
    raise ValueError("no panes table (| Pane | Name | What it shows | Pages | Drawing |)")


def read_guide(path=GUIDE_PATH) -> tuple:
    """The rows of the user guide's table at `path` (G-055)."""
    return parse_guide(Path(path).read_text(encoding="utf-8"))


def guide_lines(rows) -> list:
    """The page as plain lines, one per pane: `P5 Platoplasm: ... [pages] — GUI design,
    not Meredith's` (G-055)."""
    out = []
    for r in rows:
        line = f"{r.pane} {r.name}: {r.shows}. {r.pages}"
        if r.gui_design:
            line += f" — {GUI_DESIGN}"
        out.append(line)
    return out


def guide_html(rows) -> str:
    """The page as HTML: a heading, one table row per pane, the GUI-design rows marked
    in bold, and the tags explained (G-055)."""
    parts = ["<h3>What am I looking at?</h3>",
             "<p>Each pane of the window, what it shows, and the pages behind it. "
             "<b>[M86 p. N]</b> is Meredith's thesis (1986), printed page N; "
             "<b>[FCCA p. N]</b> is <i>Fluid Concepts and Creative Analogies</i>, ch. 1; "
             "\"Numbo GUI\" is the precedent followed. Two drawings have no figure in the "
             f"sources and are <b>{html.escape(GUI_DESIGN)}</b>.</p>",
             "<table cellpadding='4'><tr><th>Pane</th><th>What it shows</th><th>Pages</th>"
             "<th>Drawing</th></tr>"]
    for r in rows:
        drawing = html.escape(r.drawing)
        if r.gui_design:
            drawing = f"<b>{drawing}</b>"
        parts.append(f"<tr><td><b>{html.escape(r.pane)}</b> {html.escape(r.name)}</td>"
                     f"<td>{html.escape(r.shows)}</td><td>{html.escape(r.pages)}</td>"
                     f"<td>{drawing}</td></tr>")
    parts.append("</table>")
    return "\n".join(parts)

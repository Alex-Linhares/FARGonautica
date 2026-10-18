"""The help page's model (item 16; G-053): the pane-to-pages table behind Help > "What am
I looking at?" (PANES.md section 4; Seek-Whence GUI, `models/guide.py`).

The table is data here (`PANE_ROWS`): one row per pane of PANES.md with its id, its name,
what it shows, the pages or precedent behind it and whose drawing it follows, or that it is
GUI design, not French's (ANSWERS.md 4). The window's dock table reads its pages texts from
here (`pages_of`), so the pane, its tooltip and the help say the same words. The user guide
(`docs/gui/panes.md`, item 17) carries the same table in Markdown; `read_guide` reads it
back, `guide_rows` prefers the file when it is there and parses, and a test keeps the two
equal. Qt-free; the window turns the rows into HTML with `guide_html`.
"""

import dataclasses
import html
from pathlib import Path

__all__ = ["GUIDE_PATH", "GUI_DESIGN", "ICONS_NOTE", "HEADER", "EXPECTATIONS", "GuideRow",
           "PANE_ROWS", "pages_of", "row_of", "parse_guide", "read_guide", "guide_rows",
           "guide_lines", "guide_html"]

GUIDE_PATH = Path(__file__).resolve().parents[3] / "docs" / "gui" / "panes.md"
GUI_DESIGN = "GUI design, not French's"
ICONS_NOTE = "icons: new drawings, not French's"
HEADER = ("Pane", "Name", "What it shows", "Pages", "Drawing")

# What to expect from the dynamics (ANSWERS.md 11; PANES.md section 0): said in the help,
# so that an empty pane is read as the run's behaviour, not a defect.
EXPECTATIONS = (
    "The Worldview usually holds one correspondence (RESULTS.md 6.4), so the Worldview and "
    "the competitions panes are often short; an empty one says so.",
    "A context push happens in well under one percent of runs (RESULTS.md 6.2): the Slipnet "
    "network's banner says \"no context push in this run\" almost always.",
    "The thermometer jumps to about 96 at the first Worldview change (RESULTS.md 8); the "
    "temperature plot shows the jump and the three terms behind it.",
    "Nothing is tuned, smoothed or hidden: the GUI is a tool for seeing why runs miss.",
)


@dataclasses.dataclass(frozen=True)
class GuideRow:
    """One pane of the table (G-053): its id (`P5`), name, what it shows, the pages or
    precedent behind it, and its `drawing` (whose figure it follows, or GUI_DESIGN)."""
    pane: str
    name: str
    shows: str
    pages: str
    drawing: str

    @property
    def gui_design(self) -> bool:
        """Whether the pane's drawing as a whole is the GUI's own, not French's (the
        drawing text starts with GUI_DESIGN; a pane with only an overlay or an editor of
        the GUI's names it in the text instead) (ANSWERS.md 4; G-053)."""
        return self.drawing.startswith(GUI_DESIGN)


PANE_ROWS = (
    GuideRow("P1", "Table",
             "the table from above, Henry's side above and Eliza's below, the objects as "
             "icons, the touch (H) and the answer (E), the groups, the proposals and the "
             "correspondences as the dumps draw them; overlays for the regions, the "
             "salience and the region ranks, and Eliza's view",
             "Fig. 4.1 and the dumps [F95 pp. 96-107 (img)]; the palette [F95 p. 26 (img)]; "
             + ICONS_NOTE + "; the overlays (regions, salience, region ranks) and Eliza's "
             "view: GUI design, not French's, from the prose [F95 pp. 65-67]",
             "Figs. 4.1-4.9 and SPEC.md 6c.14; the overlays and Eliza's view are "
             + GUI_DESIGN),
    GuideRow("P2", "Thermometer",
             "French's column, filled to the temperature, with the value; grey while held",
             "Fig. 4.1 [F95 pp. 95-96] and the dumps [F95 pp. 98-107 (img)]; the grey band "
             "while held, the reason tick, the scale and the age are the GUI's",
             "Fig. 4.1; the held colour, the tick, the scale and the age are the GUI's"),
    GuideRow("P2b", "Temperature plot",
             "the temperature over the codelets with its three terms, the competitions and "
             "the changes marked, and French's chapter 4 readings on request",
             GUI_DESIGN + ", from the formula [F95 p. 91 (img)]; French's chapter 4 "
             "thermometer beside the run's [F95 pp. 98-107 (img)]",
             GUI_DESIGN),
    GuideRow("P3", "Slipnet grid",
             "French's grid of 41 boxes, each with its number and a black square for the "
             "activation, and a seventh row for the nodes the screen lacks",
             "[F95 p. 95] and the dumps [F95 pp. 96-107 (img)]; the seventh row (the six "
             "object nodes the screen lacks, and isolated, D-007) is the GUI's, not "
             "French's; French's chapter 4 grid beside the run's on request",
             "Fig. 4.1 and the dumps; the seventh row is the GUI's"),
    GuideRow("P3b", "Slipnet network",
             "the nodes and links at their current lengths, with the labels that shorten "
             "them, and the context push and pop",
             GUI_DESIGN + ", in Fig. 3.6's style [F95 p. 61 (img)]",
             GUI_DESIGN + ", after Fig. 3.6"),
    GuideRow("P4", "Coderack bars",
             "French's strip: a bar per codelet type with the percentage waiting, and the "
             "count of codelets run",
             "[F95 p. 97] and the dumps [F95 pp. 98-107 (img)]; the catalogue legend "
             "(D-051) on request",
             "Fig. 4.1 and the dumps"),
    GuideRow("P4b", "Rack list",
             "the codelets waiting, oldest first, with their urgencies and chances, and the "
             "one chosen",
             GUI_DESIGN + ", after the Numbo GUI's coderack_view.py",
             GUI_DESIGN),
    GuideRow("P5", "Worldview",
             "the correspondences in the Worldview and the shadow cabinet with their points, "
             "the rating and its pairs, and the proposals",
             GUI_DESIGN + ", from the text of [F95 pp. 67-74]",
             GUI_DESIGN),
    GuideRow("P6", "Competitions",
             "each competition as the Old Worldview beside the Rival Worldview, with the "
             "ratings and the chance",
             GUI_DESIGN + ", after Fig. 3.11 [F95 p. 72 (img)]",
             GUI_DESIGN + ", after Fig. 3.11"),
    GuideRow("P7", "Palette and editor",
             "Fig. 2.1's column of types with the counts, and a canvas for setting a table "
             "to run",
             "the palette [F95 p. 96 (img)]; the editor is " + GUI_DESIGN,
             "Figs. 2.1 and 4.1; " + ICONS_NOTE + "; the editor is " + GUI_DESIGN),
    GuideRow("P8", "Results",
             "a batch of runs of the problem, then French's bar graph and table of "
             "frequency, average structure and average codelets per answer beside ours",
             "the chapter 5 layout [F95 pp. 115-139]",
             "the bar graphs and tables of chapter 5 [F95 pp. 115-139 (img)]"),
    GuideRow("P9", "Controls",
             "the problem, the profile, the seed, the run buttons and the speed",
             "Numbo GUI, controls.py", "Numbo GUI"),
    GuideRow("P10", "Trace and event log",
             "the text trace as the command line prints it, and every event as a row",
             "Numbo GUI, log_view.py", "Numbo GUI"),
    GuideRow("P11", "Timeline",
             "the slider over the run's events with the markers and a strip of the "
             "temperature",
             "Numbo GUI, timeline.py", "Numbo GUI"),
    GuideRow("P12", "Stats",
             "the run's numbers as they stand, the invariants' verdict and French's report "
             "line at the end",
             "Numbo GUI, stats_view.py", "Numbo GUI"),
    GuideRow("P13", "Configuration",
             "every field of the configuration with its comment, the profile's value and "
             "the edits for the next run",
             "ANSWERS.md 3 (Edit > Configuration): every field of Config with the comments "
             "of config.py (D-066) and French's faults marked; Seek-Whence GUI, "
             "config_view.py",
             "Seek-Whence GUI"),
    GuideRow("P14", "Roles",
             "the role tags of the run by family, each with its evidence, weight, readers "
             "and, once lost, when and why; the axis and the re-cast's tiers",
             GUI_DESIGN + ", a re-interpretation (the roles loop's ANSWERS.md): the roles "
             "have no figure in French, who names the loner, the sidekick and the figure in "
             "the prose [F95 pp. 118-140] (D-078, D-079); the families and codes of the "
             "registry",
             GUI_DESIGN + ", a re-interpretation"),
)

_BY_PANE = {row.pane: row for row in PANE_ROWS}


def row_of(pane: str) -> GuideRow:
    """The built-in row of a pane id (G-053); a `KeyError` for an unknown one."""
    return _BY_PANE[pane]


def pages_of(pane: str) -> str:
    """The pages text of a pane, the words the window's dock table, its tooltips and the
    help share (G-053)."""
    return _BY_PANE[pane].pages


def _esc(text: str) -> str:
    """HTML-escape `<`, `>` and `&`, leaving the quotes readable (G-053)."""
    return html.escape(text, quote=False)


def _cells(line: str) -> list:
    return [cell.strip() for cell in line.strip().strip("|").split("|")]


def parse_guide(text: str) -> tuple:
    """The rows of the first table of `text` whose header is HEADER, in order
    (Seek-Whence GUI, `models/guide.py`; G-053); a `ValueError` when there is none."""
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
    """The rows of the user guide's table at `path` (G-053)."""
    return parse_guide(Path(path).read_text(encoding="utf-8"))


def guide_rows(path=GUIDE_PATH) -> tuple:
    """The rows the help shows: the user guide's when the file is there and parses
    (item 17), else the built-in table (G-053)."""
    try:
        return read_guide(path)
    except (OSError, ValueError):
        return PANE_ROWS


def guide_lines(rows=PANE_ROWS) -> list:
    """The page as plain lines, one per pane: `P5 Worldview: ... [pages] — GUI design,
    not French's` (G-053)."""
    out = []
    for r in rows:
        line = f"{r.pane} {r.name}: {r.shows}. {r.pages}"
        if r.gui_design:
            line += f" — {GUI_DESIGN}"
        out.append(line)
    return out


def guide_html(rows=PANE_ROWS) -> str:
    """The page as HTML: a heading, the tags explained, one table row per pane with the
    GUI-design rows marked in bold, and what to expect from the dynamics (ANSWERS.md 4,
    11; G-053)."""
    parts = ["<h3>What am I looking at?</h3>",
             "<p>Each pane of the window, what it shows, and the pages behind it. "
             "<b>[F95 p. N]</b> is French's <i>The Subtlety of Sameness</i> (1995), printed "
             "page N, <b>(img)</b> a figure or a dump; <b>[FCCA p. N]</b> is <i>Fluid "
             "Concepts and Creative Analogies</i>, chs. 8-9; \"Numbo GUI\" and \"Seek-Whence "
             "GUI\" are the precedents followed. The drawings with no figure in the sources "
             f"are <b>{GUI_DESIGN}</b>, and the {ICONS_NOTE}.</p>",
             "<table cellpadding='4'><tr><th>Pane</th><th>What it shows</th><th>Pages</th>"
             "<th>Drawing</th></tr>"]
    for r in rows:
        drawing = _esc(r.drawing)
        if r.gui_design:
            drawing = f"<b>{drawing}</b>"
        parts.append(f"<tr><td><b>{_esc(r.pane)}</b> {_esc(r.name)}</td>"
                     f"<td>{_esc(r.shows)}</td><td>{_esc(r.pages)}</td>"
                     f"<td>{drawing}</td></tr>")
    parts.append("</table>")
    parts.append("<h4>What to expect</h4><ul>"
                 + "".join(f"<li>{_esc(line)}</li>" for line in EXPECTATIONS)
                 + "</ul>")
    return "\n".join(parts)

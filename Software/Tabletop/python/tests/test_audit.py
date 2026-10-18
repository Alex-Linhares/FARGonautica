"""The citation audit (`docs/citation-audit.md`, `tools/audit_sample.py`): the sample was
drawn with a recorded seed from the citation inventory, every item of it has a verdict, every
bar-graph value of `reported.py` and ten layouts were re-verified, and the tool reproduces a
sample of the recorded size from the current tree."""

import importlib.util
import json
import re
import subprocess
import sys
from pathlib import Path

from tabletop import reported

PYTHON_DIR = Path(__file__).resolve().parent.parent
DOC = PYTHON_DIR / "docs" / "citation-audit.md"
TOOL = PYTHON_DIR / "tools" / "audit_sample.py"
ROW = re.compile(r"^\| (\d+) \| `([^`]+):(\d+)` \| `(\[[^`]+\])` \| .* \| (\*\*fixed\*\*|yes)")


def load_tool():
    spec = importlib.util.spec_from_file_location("audit_sample", TOOL)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def rows(section: str) -> list:
    text = DOC.read_text()
    start = text.index(section)
    end = text.index("\n## ", start + 1)
    return [m for m in (ROW.match(line) for line in text[start:end].split("\n")) if m]


def test_the_document_records_the_seed_and_the_inventory():
    text = DOC.read_text()
    assert "seed 31" in text and "2,293 citations" in text
    assert "212 `(img)`" in text and "2,081 plain" in text


def test_every_sampled_citation_is_listed_with_a_verdict():
    img, plain = rows("## 2. The 24 `(img)` citations"), rows("## 3. The 24 plain citations")
    assert [int(m.group(1)) for m in img] == list(range(1, 25))
    assert [int(m.group(1)) for m in plain] == list(range(1, 25))
    for m in img:
        assert "(img)" in m.group(4)
    for m in plain:
        assert "(img)" not in m.group(4)
    fixed = [m for m in img + plain if m.group(5) == "**fixed**"]
    assert len(fixed) == 3


def test_every_listed_location_exists_and_still_carries_the_citation():
    for m in rows("## 2. The 24 `(img)` citations") + rows("## 3. The 24 plain citations"):
        path = PYTHON_DIR / m.group(2)
        assert path.is_file(), m.group(2)
        line = path.read_text().split("\n")[int(m.group(3)) - 1]
        first_tag = m.group(4).split(";")[0].rstrip("]")
        assert first_tag in line, (m.group(2), m.group(3), first_tag)


def test_every_result_set_and_table_is_named():
    text = " ".join(DOC.read_text().split())
    for rs in reported.CHAPTER5:
        title = rs.title.replace(" (original program)", " (original)").replace(
            " (modified program)", " (modified)")
        assert title in text, title
    for rs in reported.FCCA:
        assert rs.figure in ("IX-1d", "IX-1f") or "All 18 agree" in text
    assert "IX-1d (86/14, 253/154, 107/85)" in text and "IX-1f (82/18, 242/148, 91/127)" in text
    for name in ("TABLE_5_1", "TABLE_5_2", "TABLE_5_3", "FIGURE_5_4"):
        assert name in text
    labels = ", ".join(label for label, _ in reported.FIGURE_5_4)
    assert labels in text


def test_the_recorded_frequencies_equal_reported():
    text = DOC.read_text()
    for rs in reported.CHAPTER5:
        title = rs.title.replace(" (original program)", " (original)").replace(
            " (modified program)", " (modified)")
        recorded = " / ".join(str(a.frequency) for a in rs.answers)
        pattern = rf"\| {re.escape(title)} \| [^|]+ \| {re.escape(recorded)} \|"
        assert re.search(pattern, text), (title, recorded)


def test_the_ten_layouts_are_real_problems():
    text = DOC.read_text()
    stems = {p.stem for p in (PYTHON_DIR / "problems").glob("*.json")}
    listed = re.findall(r"`((?:surround|blockage|buridan|figure-ground|ch2)-[a-z0-9]+)`",
                        text[text.index("## 5."):text.index("## 6.")])
    assert len(set(listed)) == 10 and set(listed) <= stems
    assert "Ten of ten agree" in text


def test_the_tool_draws_the_recorded_sample_shape():
    tool = load_tool()
    s = tool.sample(31, 24, 24, 10)
    assert s["inventory"] == s["img_total"] + s["plain_total"] >= 2000
    assert len(s["img"]) == 24 and len(s["plain"]) == 24 and len(s["layouts"]) == 10
    assert all(i["img"] for i in s["img"]) and not any(i["img"] for i in s["plain"])
    assert len({(i["file"], i["line"], i["cite"]) for i in s["img"]}) == 24
    assert s == tool.sample(31, 24, 24, 10)
    assert s["layouts"] != tool.sample(32, 24, 24, 10)["layouts"]


def test_the_tool_lists_the_layouts_the_document_names_when_the_inventory_is_unchanged():
    """The draws depend on the inventory's size; while it is the audit's, the layouts are
    the audit's. If a later item adds or removes citations this assertion is retired."""
    tool = load_tool()
    s = tool.sample(31, 24, 24, 10)
    if s["inventory"] == 2293 and s["img_total"] == 212:
        assert s["layouts"] == ["surround-03", "surround-10", "blockage-01", "surround-01",
                                "surround-05", "buridan-06", "buridan-07", "ch2-fig05",
                                "ch2-fig07", "ch2-fig15"]


def test_the_command_line():
    proc = subprocess.run([sys.executable, str(TOOL), "--json", "--img", "2", "--plain", "3",
                           "--layouts", "1"], capture_output=True, text=True, timeout=120)
    assert proc.returncode == 0
    s = json.loads(proc.stdout)
    assert len(s["img"]) == 2 and len(s["plain"]) == 3 and len(s["layouts"]) == 1
    text = subprocess.run([sys.executable, str(TOOL), "--img", "1", "--plain", "1"],
                          capture_output=True, text=True, timeout=120).stdout
    assert text.startswith("Seed 31;") and "### layouts" in text

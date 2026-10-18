"""Loop0004 item 17: the GUI's documentation agrees with the code (Numbo GUI,
`tests/test_readme.py`; G-056, G-057: what a document promises, a test runs).

- `python/README.md` has a "The GUI" section that says how to install (`pip install -e
  '.[gui]'`), how to run, shows a screenshot and links the guide; its `sh` blocks run
  from a fresh shell (PATH, HOME, LANG, offscreen) and print the output shown after them;
- the four screenshots are PNGs in `docs/`, each under 200 KB, tracked by git and
  referenced from the README or the guide; `scripts/docs_screenshots.py` renders the four
  into a temporary folder (never over the committed ones);
- the guide, `docs/gui/panes.md`, has one section per pane with its pages, and P5 and P6
  say "GUI design, not Meredith's" (ANSWERS.md 4); its quotations pass the engine's
  checker (`tools/check_quotes.py`);
- `seekwhence/gui/README.md` names every module of the GUI, `tests/gui/README.md` every
  test file, and the top-level README points at the section and the guide.

The tests that need PySide6 skip without it; the rest are Qt-free and run in the engine's
suite too.
"""

import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

PYTHON_DIR = Path(__file__).resolve().parents[2]
README = PYTHON_DIR / "README.md"
TOP_README = PYTHON_DIR.parent / "README.md"
GUIDE = PYTHON_DIR / "docs" / "gui" / "panes.md"
GUI_README = PYTHON_DIR / "seekwhence" / "gui" / "README.md"
TESTS_README = PYTHON_DIR / "tests" / "gui" / "README.md"
SCRIPT = PYTHON_DIR / "scripts" / "docs_screenshots.py"
CHECKER = PYTHON_DIR / "tools" / "check_quotes.py"


def sources_found() -> bool:
    """Meredith's PDFs are in FARGonautica's Literature/: found when this folder is inside a
    FARGonautica clone, or when $FARGONAUTICA names one (the standalone Seek-Whence
    repository)."""
    if os.environ.get("FARGONAUTICA"):
        return True
    return any((d / ".git").exists() and (d / "Literature").is_dir()
               for d in Path(__file__).resolve().parents)
DOCS = PYTHON_DIR / "docs"
LIMIT = 200 * 1024
PICTURES = ("gui-window.png", "gui-cytoplasm.png", "gui-reformulation.png",
            "gui-platoplasm.png")
GUI_DESIGN = "GUI design, not Meredith's"
PANE_IDS = tuple(f"P{n}" for n in range(1, 14))


def section(text, title):
    """The text of the `## TITLE` section, up to the next `## ` heading (Numbo GUI,
    `test_readme.py`)."""
    match = re.search(rf"^## {re.escape(title)}\n(.*?)(?=^## |\Z)", text, re.M | re.S)
    assert match, f"no '## {title}' section"
    return match.group(1)


def sh_blocks(text):
    return re.findall(r"^```sh\n(.*?)^```$", text, re.M | re.S)


def blocks_with_shown_output(text):
    """(sh block, shown output) for each ```sh block followed by a plain ``` block with
    only prose ending in a colon, or nothing, in between (Numbo GUI, `test_readme.py`)."""
    fences = re.findall(r"^```(\w*)\n(.*?)^```$\n(.*?)(?=^```|\Z)", text, re.M | re.S)
    pairs = []
    for (lang, body, between), (next_lang, next_body, _) in zip(fences, fences[1:]):
        if lang == "sh" and next_lang == "" and (not between.strip()
                                                 or between.strip().endswith(":")):
            pairs.append((body, next_body))
    return pairs


def images_in(text):
    return re.findall(r"!\[[^\]]+\]\(([^)]+)\)", text)


def fresh_env():
    """PATH, HOME and LANG only, as Numbo's; Qt offscreen, so no window opens."""
    env = {k: os.environ[k] for k in ("PATH", "HOME", "LANG") if k in os.environ}
    env["QT_QPA_PLATFORM"] = "offscreen"
    return env


# -- the README's section -----------------------------------------------------------------

def test_the_gui_section_installs_runs_shows_a_picture_and_links_the_guide():
    text = section(README.read_text(encoding="utf-8"), "The GUI")
    assert "pip install -e '.[gui]'" in text
    assert "python3 -m seekwhence.gui" in text and "seekwhence-gui" in text
    assert "docs/gui/panes.md" in text and "GUI-DECISIONS.md" in text
    assert "scripts/docs_screenshots.py" in text
    assert GUI_DESIGN in " ".join(text.split())                 # ANSWERS.md 4; wrapped
    assert images_in(text), "a screenshot"
    for key in ("Space", "Esc", "--smoke", "--preset", "--terms", "Reset layout"):
        assert key in text, key


def test_the_gui_section_runs_the_smoke_command_and_the_cli_with_shown_output():
    text = section(README.read_text(encoding="utf-8"), "The GUI")
    blocks = sh_blocks(text)
    assert any("python3 -m seekwhence.gui --smoke" in b for b in blocks)
    assert any(b.startswith("python3 -m seekwhence ") for b in blocks), "the same run in the CLI"
    pairs = blocks_with_shown_output(text)
    assert len(pairs) >= 2
    assert any("seekwhence.gui --smoke" in body for body, _ in pairs), "its output is shown"


@pytest.fixture(scope="module")
def section_outputs():
    """Every `sh` block of the GUI section run by bash from python/ in a fresh
    environment (needs PySide6 for the smoke command)."""
    pytest.importorskip("PySide6")
    text = section(README.read_text(encoding="utf-8"), "The GUI")
    results = []
    for block in sh_blocks(text):
        proc = subprocess.run(["bash", "-c", block], cwd=PYTHON_DIR, env=fresh_env(),
                              capture_output=True, text=True, timeout=600)
        results.append((block, proc))
    return results


def test_every_block_of_the_gui_section_runs(section_outputs):
    assert len(section_outputs) >= 2
    for block, proc in section_outputs:
        assert proc.returncode == 0, f"{block}\nstdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
        assert proc.stderr == "", (block, proc.stderr)


def test_the_shown_outputs_are_what_the_blocks_print(section_outputs):
    """Every output the README shows after a block is what the block prints (its end,
    trailing blanks ignored); the smoke line and the CLI's line are the same."""
    printed = {block: proc.stdout + proc.stderr for block, proc in section_outputs}
    text = section(README.read_text(encoding="utf-8"), "The GUI")
    pairs = blocks_with_shown_output(text)
    assert len(pairs) >= 2
    lines = set()
    for block, shown in pairs:
        expected = [line.rstrip() for line in shown.splitlines()]
        got = [line.rstrip() for line in printed[block].splitlines()]
        assert got[-len(expected):] == expected, block
        lines.add(tuple(expected))
    assert len(lines) == 1, "the GUI's smoke line equals the CLI's"
    assert lines.pop()[-1].startswith("outcome: ok after 4 terms")      # [M86 p. 166]


# -- the screenshots ----------------------------------------------------------------------

def test_the_screenshots_are_committed_small_pngs_and_referenced():
    referenced = set()
    for path, base in ((README, PYTHON_DIR), (GUIDE, GUIDE.parent)):
        for image in images_in(path.read_text(encoding="utf-8")):
            resolved = (base / image).resolve()
            assert resolved.parent == DOCS, (path.name, image)
            referenced.add(resolved.name)
    assert referenced == set(PICTURES), referenced
    for name in PICTURES:
        path = DOCS / name
        data = path.read_bytes()
        assert data.startswith(b"\x89PNG\r\n\x1a\n"), name
        assert len(data) < LIMIT, (name, len(data))
        ignored = subprocess.run(["git", "check-ignore", "-q", str(path)], cwd=PYTHON_DIR)
        assert ignored.returncode == 1, f"{name} is gitignored"
    for image in images_in(section(README.read_text(encoding="utf-8"), "The GUI")):
        assert image.startswith("docs/gui-"), image


def test_the_guide_lists_the_four_pictures_with_their_pages():
    text = section(GUIDE.read_text(encoding="utf-8"), "Screenshots")
    for name in PICTURES:
        assert f"`docs/{name}`" in text, name
    assert "[M86 p. 166]" in text and "[M86 p. 174]" in text and "200 KB" in text
    assert "scripts/docs_screenshots.py" in text


def test_the_screenshot_script_renders_the_four_pictures_offscreen(tmp_path):
    """`scripts/docs_screenshots.py --out DIR` writes the four PNGs, each under the
    limit, with the sizes the script names (the window 1000 px wide), and prints a
    line per picture; the committed pictures are not touched (G-056)."""
    pytest.importorskip("PySide6")
    before = {name: (DOCS / name).read_bytes() for name in PICTURES}
    out = tmp_path / "shots"
    proc = subprocess.run([sys.executable, str(SCRIPT), "--out", str(out)], cwd=PYTHON_DIR,
                          env=fresh_env(), capture_output=True, text=True, timeout=600)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert proc.stderr == "", proc.stderr
    assert sorted(p.name for p in out.iterdir()) == sorted(PICTURES)
    for name in PICTURES:
        data = (out / name).read_bytes()
        assert data.startswith(b"\x89PNG\r\n\x1a\n"), name
        assert 1000 < len(data) < LIMIT, (name, len(data))
        assert f"{name}: " in proc.stdout, name
    width = int(re.search(r"gui-window\.png: (\d+) x", proc.stdout).group(1))
    assert width == 1000
    assert {name: (DOCS / name).read_bytes() for name in PICTURES} == before


# -- the guide ----------------------------------------------------------------------------

def guide_sections():
    """`{pane id: section text}` for every `### Pn.` heading of the guide."""
    text = GUIDE.read_text(encoding="utf-8")
    parts = re.split(r"^### (P\d+)\. [^\n]*\n", text, flags=re.M)
    return dict(zip(parts[1::2], parts[2::2]))


def test_the_guide_has_a_section_per_pane_with_its_pages():
    sections = guide_sections()
    assert tuple(sections) == PANE_IDS
    for pane, text in sections.items():
        assert "**What it shows" in text, pane
        assert re.search(r"\[M86 pp?\. |\[FCCA pp?\. |Numbo GUI", text), pane


def test_the_guide_marks_the_platoplasm_and_the_taskrack_as_gui_design():     # ANSWERS.md 4
    sections = guide_sections()
    for pane in ("P5", "P6"):
        assert sections[pane].lstrip().startswith(f"**{GUI_DESIGN}.**"), pane
    for pane, text in sections.items():
        if pane not in ("P5", "P6"):
            assert GUI_DESIGN not in text, pane
    text = GUIDE.read_text(encoding="utf-8")
    assert text.count(GUI_DESIGN) >= 5          # the note, the two rows, the two sections


def test_the_guide_explains_the_tags_the_menus_and_the_layout_file():
    text = GUIDE.read_text(encoding="utf-8")
    for words in ("[M86 p. N]", "[FCCA p. N]", "Numbo GUI", "| Session |", "| Help |",
                  "default_layout.json", "Reset layout", "What am I looking at?",
                  "pip install -e '.[gui]'", "--smoke"):
        assert words in text, words


@pytest.mark.skipif(shutil.which("pdftotext") is None, reason="pdftotext (poppler) is missing")
@pytest.mark.skipif(not sources_found(), reason="the sources are not found: set FARGONAUTICA "
                                                "to a clone of FARGonautica")
def test_the_guides_quotations_are_on_their_pages():
    proc = subprocess.run([sys.executable, str(CHECKER), str(GUIDE)],
                          capture_output=True, text=True, timeout=600)
    output = proc.stdout + proc.stderr
    assert proc.returncode == 0, output
    assert "quotations:" in output


# -- the module maps and the top-level README ------------------------------------------------

def test_the_gui_readme_names_every_module():
    text = GUI_README.read_text(encoding="utf-8")
    gui = PYTHON_DIR / "seekwhence" / "gui"
    modules = [p.relative_to(gui).as_posix() for p in gui.rglob("*.py")
               if p.name != "__init__.py" and "__pycache__" not in p.parts]
    assert len(modules) > 30
    missing = [m for m in modules if f"`{m}`" not in text and f"`{m}," not in text
               and f", `{m}`" not in text]
    assert not missing, missing
    assert "docs/gui/panes.md" in text and "scripts/docs_screenshots.py" in text


def test_the_tests_readme_names_every_test_file():
    text = TESTS_README.read_text(encoding="utf-8")
    here = Path(__file__).resolve().parent
    files = sorted(p.name for p in here.glob("test_*.py"))
    missing = [f for f in files if f"`{f}`" not in text]
    assert not missing, missing


def test_the_top_readme_points_at_the_gui():
    """The folder's front page shows the window, says how to install and start the GUI, and
    links this README and the guide."""
    text = TOP_README.read_text(encoding="utf-8")
    assert "python3 -m pip install -e '.[gui]'" in text
    assert "python3 -m seekwhence.gui" in text
    assert "(python/docs/gui-window.png)" in text
    assert "(python/README.md)" in text and "(python/docs/gui/panes.md)" in text

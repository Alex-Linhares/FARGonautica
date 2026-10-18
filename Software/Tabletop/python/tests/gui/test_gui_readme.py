"""Item 18: the GUI's documentation agrees with the code (Numbo GUI, `test_readme.py`,
through the Seek-Whence GUI's; G-056, G-057: what a document promises, a test runs).

- The front page (`../README.md`) has a "Using the GUI" section after "Using the command
  line" that says how to install (`pip install -e '.[gui]'`), how to run, shows the five
  screenshots with captions, names the GUI's own designs and links the guide and the
  decisions; `python/README.md` has "The GUI" with the same promises.
- Their `sh` blocks run from a fresh shell (PATH, HOME, LANG; Qt offscreen) and print the
  output shown after them: the `--smoke` line is the CLI's report line.
- The five screenshots (four from the quick start's run, the roles picture from
  `blockage-07` seed 104 since v5's fix, D-116 to D-120; seed 1 in v4, D-115; seed 27 in v3, D-114) are PNGs under `docs/`, each under 200 KB, tracked by git, and they
  are exactly the pictures the two GUI sections and the guide reference; the guide lists
  them with their pages. `scripts/docs_screenshots.py --out DIR` renders the five into a
  temporary folder (`slow`; never over the committed ones).
- `tabletop/gui/README.md` names every module of the GUI, `tests/gui/README.md` every test
  file, and the engine's `tests/test_readme.py` leaves `gui-*.png` to this file (G-057).

The tests that run the GUI's commands need PySide6 and skip without it; the rest are
Qt-free and run in the engine's suite too.
"""

import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel

PYTHON_DIR = Path(__file__).resolve().parents[2]
TARGET = PYTHON_DIR.parent
FRONT = TARGET / "README.md"
README = PYTHON_DIR / "README.md"
GUIDE = PYTHON_DIR / "docs" / "gui" / "panes.md"
GUI_README = PYTHON_DIR / "tabletop" / "gui" / "README.md"
TESTS_README = PYTHON_DIR / "tests" / "gui" / "README.md"
ENGINE_README_TEST = PYTHON_DIR / "tests" / "test_readme.py"
SCRIPT = PYTHON_DIR / "scripts" / "docs_screenshots.py"
DOCS = PYTHON_DIR / "docs"
LIMIT = 200 * 1024
PICTURES = ("gui-window.png", "gui-table.png", "gui-slipnet.png", "gui-temperature.png",
            "gui-roles.png")
GUI_DESIGN = "GUI design, not French's"
REPORT_LINE = "Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3."   # the quick start since v5's fix, D-116 to D-120 (cup2 (E2), structure 88, 305 codelets, T 17.8 in v4, D-115; small-glass1 (E1), 144 codelets, T 31.0 in v3, D-114; cup2 (E2), 44 codelets, T 41.0 since item 08; small-glass1 (E1), 77 codelets, T 34.6 at item 07; cup2 (E2), 67 codelets, T 77.5 at item 05)


def section(text, title):
    """The text of the `## TITLE` section, up to the next `## ` heading (Numbo GUI,
    `test_readme.py`)."""
    match = re.search(rf"^## {re.escape(title)}\n(.*?)(?=^## |\Z)", text, re.M | re.S)
    assert match, f"no '## {title}' section"
    return match.group(1)


def sh_blocks(text):
    return re.findall(r"^```sh\n(.*?)^```$", text, re.M | re.S)


def blocks_with_shown_output(text):
    """(sh block, shown output) for each ```sh block followed by a plain ``` block with only
    prose in between, or nothing (Numbo GUI, `test_readme.py`)."""
    fences = re.findall(r"^```(\w*)\n(.*?)^```$\n(.*?)(?=^```|\Z)", text, re.M | re.S)
    pairs = []
    for (lang, body, between), (next_lang, next_body, _) in zip(fences, fences[1:]):
        if lang == "sh" and next_lang == "" and "```" not in between:
            pairs.append((body, next_body))
    return pairs


def images_in(text):
    return re.findall(r"!\[[^\]]+\]\(([^)]+)\)", text)


def unwrapped(text):
    """The text with every run of whitespace one space, so that a phrase wrapped across
    lines is found."""
    return " ".join(text.split())


def captioned_images_in(text):
    """The pictures followed by a blank line and an italic caption, the front page's form
    (the engine's `test_readme.py` requires it of every picture there)."""
    return re.findall(r"^!\[[^\]]+\]\(([^)]+)\)\n\n\*[^\n]+", text, re.M)


def fresh_env():
    """PATH, HOME and LANG only, as Numbo's; Qt offscreen, so no window opens."""
    # PATH, HOME and LANG, and the engine request when the suite itself runs on the
    # explicitly requested python engine (D-107; `tests/test_readme.py`'s `fresh_env`).
    env = {k: os.environ[k] for k in ("PATH", "HOME", "LANG", "TABLETOP_ENGINE")
           if k in os.environ}
    env["QT_QPA_PLATFORM"] = "offscreen"
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    return env


def run_block(block, cwd):
    """Every command line of a block run by bash on its own from `cwd`, as a reader would
    type them; the outputs joined, the first non-zero status kept (the engine's
    `test_readme.py`)."""
    out, err, code = [], [], 0
    for line in block.splitlines():
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        proc = subprocess.run(["bash", "-c", line], cwd=cwd, env=fresh_env(),
                              capture_output=True, text=True, timeout=600)
        out.append(proc.stdout)
        err.append(proc.stderr)
        code = code or proc.returncode
    return subprocess.CompletedProcess(block, code, "".join(out), "".join(err))


def gui_sections():
    """(the file, the folder its blocks run from, the section's text) for the two GUI
    sections."""
    return ((FRONT, TARGET, section(FRONT.read_text(encoding="utf-8"), "Using the GUI")),
            (README, PYTHON_DIR, section(README.read_text(encoding="utf-8"), "The GUI")))


# -- the two sections' words and links ------------------------------------------------------

def test_the_front_page_section_installs_runs_shows_the_pictures_and_links_the_guide():
    text = FRONT.read_text(encoding="utf-8")
    found = [h for h in re.findall(r"^## (.+?)\s*$", text, re.M)
             if h in ("Using the command line", "Using the GUI", "Replaying French's experiments")]
    assert found == ["Using the command line", "Using the GUI", "Replaying French's experiments"]
    assert "- [Using the GUI](#using-the-gui)" in section(text, "Contents")
    gui = section(text, "Using the GUI")
    assert "python3 -m pip install -e '.[gui]'" in gui
    assert "python3 -m tabletop.gui" in gui and "tabletop-gui" in gui
    assert "(python/docs/gui/panes.md)" in gui and "(python/GUI-DECISIONS.md)" in gui
    assert GUI_DESIGN in unwrapped(gui)                              # ANSWERS.md 4; wrapped
    assert "new drawings" in gui                                      # the icons
    for key in ("Space", "Right", "Shift+Right", "Esc", "Left", "--smoke", "Reset layout",
                "Save session", "Do this!", "[F95 p. 96]", "[F95 p. 95]", "[F95 p. 91]"):
        assert key in unwrapped(gui), key
    assert captioned_images_in(gui) == [f"python/docs/{name}" for name in PICTURES]
    assert len(images_in(gui)) == len(PICTURES), "every picture has a caption"
    assert "| `pip install -e '.[gui]'` |" in section(text, "Installing")
    assert "python3 -m scripts.docs_screenshots" in section(text, "Installing")
    assert "`docs_screenshots.py`" in section(text, "What's in this folder")
    assert "python/GUI-DECISIONS.md" in section(text, "What's in this folder")
    assert "../" not in text, "no relative link leaves the folder"


def test_the_readme_section_installs_runs_shows_a_picture_and_links_the_guide():
    text = README.read_text(encoding="utf-8")
    assert "- [The GUI](#the-gui)" in section(text, "Contents")
    gui = section(text, "The GUI")
    assert "pip install -e '.[gui]'" in gui
    assert "python3 -m tabletop.gui" in gui and "tabletop-gui" in gui
    assert "(docs/gui/panes.md)" in gui and "(GUI-DECISIONS.md)" in gui
    assert "(tabletop/gui/README.md)" in gui and "(tests/gui/)" in gui
    assert "scripts.docs_screenshots" in gui
    assert GUI_DESIGN in unwrapped(gui)
    for key in ("Space", "Shift+Right", "Esc", "--smoke", "--problem", "--seed", "--profile",
                "--set", "--play", "Reset layout", "default_layout.json", "Save session",
                "What am I looking at?"):
        assert key in unwrapped(gui), key
    assert images_in(gui) == ["docs/gui-window.png"]
    for name in ("Table", "Thermometer", "Slipnet grid", "Coderack bars", "Worldview",
                 "Competitions", "Timeline", "Controls", "Stats", "Configuration",
                 "Palette and editor", "Results"):
        assert f"**{name}**" in gui, name
    assert "pip install -e '.[gui]'" in section(text, "Installing")
    assert "| `gui/` |" in section(text, "The module map")
    assert "scripts/docs_screenshots.py" in section(text, "Layout")
    assert "tests/gui/test_gui_readme.py" in section(text, "Running the tests")


# -- the commands run and print what is shown -----------------------------------------------

@pytest.fixture(scope="module")
def section_outputs():
    """Every `sh` block of the two GUI sections run by bash from its folder in a fresh
    environment (the smoke command needs PySide6)."""
    pytest.importorskip("PySide6")
    results = []
    for path, cwd, text in gui_sections():
        for block in sh_blocks(text):
            results.append((path.name, block, run_block(block, cwd)))
    return results


def test_the_gui_sections_show_the_smoke_command_with_its_output():
    for path, _cwd, text in gui_sections():
        blocks = sh_blocks(text)
        assert any("tabletop.gui --smoke" in b for b in blocks), path.name
        assert all("--smoke" in b or b.startswith("python3 -m tabletop ") for b in blocks), \
            f"{path.name}: a block that opens a window would hang the test"
        pairs = blocks_with_shown_output(text)
        assert any("tabletop.gui --smoke" in body for body, _ in pairs), path.name


def test_every_block_of_the_gui_sections_runs(section_outputs):
    assert len(section_outputs) >= 2
    for name, block, proc in section_outputs:
        assert proc.returncode == 0, f"{name}: {block}\nstdout:\n{proc.stdout}\nstderr:\n{proc.stderr}"
        assert proc.stderr == "", (name, block, proc.stderr)


def test_the_shown_outputs_are_what_the_blocks_print(section_outputs):
    """Every output a GUI section shows after a block is what the block prints (its end,
    trailing blanks ignored); the smoke line is the quick start's report line, the same
    last line as the CLI's `--quiet` run (ANSWERS.md 1)."""
    printed = {(name, block): proc.stdout for name, block, proc in section_outputs}
    smoke_lines = set()
    for path, _cwd, text in gui_sections():
        pairs = blocks_with_shown_output(text)
        assert pairs, path.name
        for block, shown in pairs:
            # The documented lines show the reference engine's output (`engine compiled`),
            # as the engine running the tests prints it (D-107; `tests/test_readme.py`).
            expected = [line.rstrip().replace("engine compiled", f"engine {accel.ENGINE}")
                        for line in shown.rstrip("\n").splitlines()]
            got = [line.rstrip() for line in printed[path.name, block].splitlines()]
            assert got[-len(expected):] == expected, (path.name, block)
            if "--smoke" in block:
                smoke_lines.add(expected[-1])
    assert smoke_lines == {REPORT_LINE}


# -- the screenshots ------------------------------------------------------------------------

def test_the_screenshots_are_committed_small_pngs_and_exactly_the_ones_referenced():
    referenced = set()
    for path, base in ((FRONT, TARGET), (README, PYTHON_DIR), (GUIDE, GUIDE.parent)):
        for image in images_in(path.read_text(encoding="utf-8")):
            resolved = (base / image).resolve()
            assert resolved.parent == DOCS and resolved.is_file(), (path.name, image)
            if resolved.name.startswith("gui-"):
                referenced.add(resolved.name)
    assert referenced == set(PICTURES), referenced
    for name in PICTURES:
        path = DOCS / name
        data = path.read_bytes()
        assert data.startswith(b"\x89PNG\r\n\x1a\n"), name
        assert 1000 < len(data) < LIMIT, (name, len(data))
        ignored = subprocess.run(["git", "check-ignore", "-q", str(path)], cwd=PYTHON_DIR,
                                 capture_output=True)
        assert ignored.returncode != 0, f"{name} is gitignored"   # 128 outside a repository
    for _path, _cwd, text in gui_sections():
        for image in images_in(text):
            assert Path(image).name.startswith("gui-"), image


def test_the_guide_lists_the_five_pictures_with_their_pages():
    text = section(GUIDE.read_text(encoding="utf-8"), "Screenshots")
    for name in PICTURES:
        assert f"| `docs/{name}` |" in text, name
    for words in ("[F95 pp. 96-107 (img)]", "[F95 p. 95]", "200 KB", "G-056",
                  "python3 -m scripts.docs_screenshots", GUI_DESIGN, "surround-05"):
        assert words in text, words


def test_the_screenshot_script_is_a_module_with_no_path_tricks_and_the_five_pictures():
    script = SCRIPT.read_text(encoding="utf-8")
    assert "sys.path.insert" not in script and "sys.path.append" not in script   # ANSWERS.md 9
    assert "python3 -m scripts.docs_screenshots" in script
    for name in PICTURES:
        assert f'"{name}"' in script, name
    assert 'os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")' in script
    assert re.search(r"LIMIT = 200 \* 1024", script)
    assert "G-056" in script


# -- the module maps and the engine's test ----------------------------------------------------

def test_the_gui_readme_names_every_module_and_the_screenshots():
    text = GUI_README.read_text(encoding="utf-8")
    gui = PYTHON_DIR / "tabletop" / "gui"
    modules = [p.relative_to(gui).as_posix() for p in gui.rglob("*.py")
               if p.name != "__init__.py" and "__pycache__" not in p.parts]
    assert len(modules) > 30
    missing = [m for m in modules if f"`{m}`" not in text and f"`{Path(m).name}`" not in text]
    assert not missing, missing
    assert "docs/gui/panes.md" in text and "scripts.docs_screenshots" in text


def test_the_tests_readme_names_every_test_file():
    text = TESTS_README.read_text(encoding="utf-8")
    files = sorted(p.name for p in Path(__file__).resolve().parent.glob("test_*.py"))
    missing = [f for f in files if f"`{f}`" not in text]
    assert not missing, missing


def test_the_engine_readme_test_leaves_the_screenshots_to_this_file():
    """The engine's front-page pictures test compares the figures with `docs_figures.py`'s
    registry and leaves the pictures named `gui-*` to this file (G-057)."""
    text = ENGINE_README_TEST.read_text(encoding="utf-8")
    assert 'startswith("gui-")' in text and "tests/gui/test_gui_readme.py" in text
    proc = subprocess.run([sys.executable, "-m", "pytest", "-q", "-p", "no:cacheprovider",
                           str(ENGINE_README_TEST), "-k", "front_page_pictures"],
                          cwd=PYTHON_DIR, capture_output=True, text=True, timeout=600)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "1 passed" in proc.stdout

"""The documentation agrees with the code (item 29): what a document promises, a test runs.

- The front page (`../README.md`) and this folder's `README.md` have the sections the
  item asks for; every `#anchor` in their tables of contents names a heading.
- The `sh` blocks of the sections that show commands run from a fresh shell (PATH, HOME and
  LANG only), exit 0, print nothing on stderr, and print the output the README shows after
  them (the block's last lines, trailing blanks ignored).
- The table of knobs in `README.md` equals what `tools/knobs_table.py` prints from
  `config.py`; the module map names every module of the package; the profiles section
  names the four profiles and every knob `french-faults` restores.
- The trace lines shown in `README.md` ("How to read a trace") and in `docs/trace-format.md`
  are lines of the run they claim to come from.
- `DECISIONS-INDEX.md`'s index by kind lists every decision, the seven FIXED ones and the
  calibrated one; `SPEC.md`'s module table names real modules and tests and every numbered
  section; `docs/engine-api.md` names every pane of the plan and marks the icons as new
  design; `docs/problems.md` documents every key of the problem files.

No PDF and nothing outside the folder is needed, so every test here runs in the standalone
copy too.
"""

import json
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel

PYTHON_DIR = Path(__file__).resolve().parents[1]
TARGET = PYTHON_DIR.parent
FRONT = TARGET / "README.md"
README = PYTHON_DIR / "README.md"
DECISIONS = PYTHON_DIR / "DECISIONS.md"
DECISIONS_INDEX = PYTHON_DIR / "DECISIONS-INDEX.md"   # the index, by number and by kind
SPEC = PYTHON_DIR / "SPEC.md"
ENGINE_API = PYTHON_DIR / "docs" / "engine-api.md"
TRACE_FORMAT = PYTHON_DIR / "docs" / "trace-format.md"
PROBLEMS_DOC = PYTHON_DIR / "docs" / "problems.md"
KNOBS = PYTHON_DIR / "tools" / "knobs_table.py"

FRONT_SECTIONS = ("What the program does", "Quick start", "Installing",
                  "Using the command line", "Replaying French's experiments",
                  "How well it does", "Running the tests", "How the reconstruction was made",
                  "What's in this folder", "Further reading and credits")
README_SECTIONS = ("Layout", "The module map", "The profiles", "The knobs", "Installing",
                   "Using the command line", "How to read a trace", "Running the benches",
                   "Running the tests", "The sources", "The quotation checker", "Conventions")
# The sections whose `sh` blocks the tests run: quick, and from the folder the block says.
FRONT_RUN = ("Quick start", "Using the command line", "Replaying French's experiments")
# "Installing" is not run: it makes a virtual environment and calls pip.
README_RUN = ("Using the command line",)
README_RUN_BLOCKS = ("python3 -m tabletop.bench --list",)      # the quick-look block only


def section(text, title):
    """The text of the `## TITLE` section, up to the next `## ` heading."""
    match = re.search(rf"^## {re.escape(title)}\n(.*?)(?=^## |\Z)", text, re.M | re.S)
    assert match, f"no '## {title}' section"
    return match.group(1)


def sh_blocks(text):
    return re.findall(r"^```sh\n(.*?)^```$", text, re.M | re.S)


def blocks_with_shown_output(text):
    """(sh block, shown output) for each ```sh block followed by a plain ``` block with only
    prose in between (or nothing)."""
    fences = re.findall(r"^```(\w*)\n(.*?)^```$\n(.*?)(?=^```|\Z)", text, re.M | re.S)
    pairs = []
    for (lang, body, between), (next_lang, next_body, _) in zip(fences, fences[1:]):
        if lang == "sh" and next_lang == "" and "```" not in between:
            pairs.append((body, next_body))
    return pairs


def anchor(heading: str) -> str:
    """GitHub's anchor for a heading: lower case, punctuation dropped, spaces to hyphens."""
    text = re.sub(r"[^\w\- ]", "", heading.strip().lower())
    return text.replace(" ", "-")


def headings(text):
    return re.findall(r"^#{1,6} (.+?)\s*$", text, re.M)


def fresh_env():
    """A reader's environment: PATH, HOME and LANG, and the engine request when the suite
    itself runs on the explicitly requested python engine (D-107)."""
    env = {k: os.environ[k] for k in ("PATH", "HOME", "LANG", "TABLETOP_ENGINE")
           if k in os.environ}
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    return env


def as_run_here(line: str) -> str:
    """A documented line, which shows the reference engine's output (`engine compiled`),
    as the engine running the tests prints it (D-107)."""
    return line.replace("engine compiled", f"engine {accel.ENGINE}")


def run_block(block, cwd):
    """Every command line of a block run by bash on its own, from `cwd`, as a reader would
    type them (the front page's lines each begin with `cd python &&`); the outputs joined.
    Returns a CompletedProcess-like record: returncode (the first non-zero), stdout, stderr."""
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


# -- the sections and the anchors ----------------------------------------------------------

def test_the_front_page_has_its_sections_in_order():
    text = FRONT.read_text(encoding="utf-8")
    found = [h for h in headings(text) if h in FRONT_SECTIONS]
    assert tuple(found) == FRONT_SECTIONS


def test_the_readme_has_its_sections_in_order():
    text = README.read_text(encoding="utf-8")
    found = [h for h in headings(text) if h in README_SECTIONS]
    assert tuple(found) == README_SECTIONS


@pytest.mark.parametrize("path", [FRONT, README])
def test_every_anchor_link_names_a_heading(path):
    text = path.read_text(encoding="utf-8")
    anchors = {anchor(h) for h in headings(text)}
    links = re.findall(r"\]\(#([^)]+)\)", text)
    assert links, "a table of contents"
    missing = [l for l in links if l not in anchors]
    assert not missing, missing


def test_the_front_page_says_where_the_code_lives_and_links_the_documents():
    text = FRONT.read_text(encoding="utf-8")
    assert "https://github.com/fargonauts/FARGonautica" in text
    assert "https://github.com/fargonauts/Tabletop" in text
    for doc in ("python/README.md", "python/SPEC.md", "python/DECISIONS.md",
                "python/RESULTS.md", "python/docs/problems.md", "python/docs/engine-api.md"):
        assert f"]({doc})" in text, doc
    assert "../" not in text, "no relative link leaves the folder"


# -- the commands run and print what is shown ---------------------------------------------

def front_blocks():
    text = FRONT.read_text(encoding="utf-8")
    blocks = []
    for title in FRONT_RUN:
        blocks += sh_blocks(section(text, title))
    return blocks


@pytest.fixture(scope="module")
def front_outputs():
    return [(b, run_block(b, TARGET)) for b in front_blocks()]


def shown_pairs() -> list:
    """The front page's command blocks that show their output, with the output shown."""
    text = FRONT.read_text(encoding="utf-8")
    pairs = []
    for title in FRONT_RUN:
        pairs += blocks_with_shown_output(section(text, title))
    assert len(pairs) >= 2, "the quick start and the batch show their output"
    return pairs


def block_prints_what_is_shown(block: str, shown: str, printed: str) -> None:
    expected = [as_run_here(line.rstrip()) for line in shown.rstrip("\n").splitlines()]
    got = [line.rstrip() for line in printed.splitlines()]
    assert got[-len(expected):] == expected, block


def test_the_quick_start_block_prints_what_the_front_page_shows():
    """The quick start alone, about a second; every block that shows its output is
    `tests_slow/test_readme_slow.py` (ANSWERS.md D2)."""
    block, shown = next((b, s) for b, s in shown_pairs() if "--quiet" in b and "--batch" not in b)
    assert shown.splitlines()[-1].startswith("Eliza touches ")
    block_prints_what_is_shown(block, shown, run_block(block, TARGET).stdout)


def readme_run_blocks():
    text = README.read_text(encoding="utf-8")
    blocks = []
    for title in README_RUN:
        blocks += sh_blocks(section(text, title))
    for title in ("Running the benches",):
        blocks += [b for b in sh_blocks(section(text, title))
                   if any(start in b for start in README_RUN_BLOCKS)]
    return blocks


@pytest.fixture(scope="module")
def readme_outputs():
    return [(b, run_block(b, PYTHON_DIR)) for b in readme_run_blocks()]


def test_the_readme_bench_section_names_only_real_benches():
    text = section(README.read_text(encoding="utf-8"), "Running the benches")
    listed = subprocess.run([sys.executable, "-m", "tabletop.bench", "--list"],
                            cwd=PYTHON_DIR, capture_output=True, text=True, check=True)
    names = {line.split()[0] for line in listed.stdout.splitlines()}
    used = set(re.findall(r"python3 -m tabletop\.bench ([a-z0-9]+)", text))
    used |= set(re.findall(r"\| ([a-z0-9]+) ", text))
    unknown = {u for u in used if u not in names and u not in ("--list",)}
    assert not unknown, unknown
    assert names <= used | {"all"}, names - used


def test_the_python_example_prints_the_line_the_readme_shows():
    text = section(README.read_text(encoding="utf-8"), "Using the command line")
    shown = re.search(r"print\(report_line\(result\)\)\s+# (Eliza touches .*)$", text, re.M)
    assert shown, "the Python example shows its output"
    proc = subprocess.run([sys.executable, "-m", "tabletop", "surround-05", "--seed", "3",
                           "--quiet"], cwd=PYTHON_DIR, capture_output=True, text=True,
                          check=True)
    assert proc.stdout.splitlines()[-1] == shown.group(1).strip()


# -- the knobs, the module map, the profiles ----------------------------------------------

def test_the_table_of_knobs_is_generated_from_config_py():
    proc = subprocess.run([sys.executable, str(KNOBS), "--check"], cwd=PYTHON_DIR,
                          capture_output=True, text=True)
    assert proc.returncode == 0, proc.stdout + proc.stderr
    from tabletop.config import Config
    import dataclasses
    text = section(README.read_text(encoding="utf-8"), "The knobs")
    for field in dataclasses.fields(Config):
        assert f"| `{field.name}` |" in text, field.name
    assert "`human` 50" in text and "`french-faults` True" in text


def test_the_module_map_names_every_module():
    text = section(README.read_text(encoding="utf-8"), "The module map")
    package = PYTHON_DIR / "tabletop"
    modules = [p for p in package.rglob("*.py") if p.relative_to(package).parts[0] != "gui"  # the GUI's own map
               if p.name != "__init__.py" and "__pycache__" not in p.parts]
    assert len(modules) > 40
    missing = []
    for p in modules:
        rel = p.relative_to(package).as_posix()
        if f"`{rel}`" not in text and f"`{p.name}`" not in text:
            missing.append(rel)
    assert not missing, missing


def test_the_profiles_section_names_the_four_and_the_faults():
    from tabletop.config import FAULT_KNOBS, PROFILES
    text = section(README.read_text(encoding="utf-8"), "The profiles")
    for name in PROFILES:
        assert f"| `{name}` |" in text, name
    for knob, _ in FAULT_KNOBS:
        assert f"`{knob}`" in text, knob
    assert "`type_weight` 30 → 50" in text and "never tuned" in text


# -- the trace lines shown are real ---------------------------------------------------------

@pytest.fixture(scope="module")
def seed_three_trace():
    proc = subprocess.run([sys.executable, "-m", "tabletop", "surround-05", "--seed", "3"],
                          cwd=PYTHON_DIR, capture_output=True, text=True, check=True)
    return proc.stdout.splitlines()


def shown_trace_lines(text):
    lines = []
    for block in re.findall(r"^```text\n(.*?)^```$", text, re.M | re.S):
        lines += [as_run_here(l) for l in block.splitlines() if l.strip() and l.strip() != "..."
                  and not l.startswith("{")]
    return lines


def test_the_readme_trace_walkthrough_quotes_the_run(seed_three_trace):
    text = section(README.read_text(encoding="utf-8"), "How to read a trace")
    lines = shown_trace_lines(text)
    assert len(lines) >= 12
    for line in lines:
        assert line in seed_three_trace, line
    assert lines[0] == seed_three_trace[0] and lines[-1] == seed_three_trace[-1]
    codelets = sum(bool(re.match(r"\s*\d+  T=", l)) for l in seed_three_trace)
    assert f"prints {codelets} codelet lines" in text


def test_the_trace_format_example_quotes_the_run(seed_three_trace):
    text = TRACE_FORMAT.read_text(encoding="utf-8")
    block = text[:text.index("## The JSON-lines trace")]
    lines = shown_trace_lines(block)
    assert len(lines) >= 8
    for line in lines:
        assert line in seed_three_trace, line


# -- DECISIONS.md by kind, SPEC.md's module table, the docs -------------------------------

def test_decisions_are_indexed_by_kind():
    from tabletop.config import FAULT_KNOBS
    text = DECISIONS.read_text(encoding="utf-8")
    index = section(DECISIONS_INDEX.read_text(encoding="utf-8"), "Index by kind")
    numbers = re.findall(r"^### (D-\d+):", text, re.M)
    listed = set(re.findall(r"^- (D-\d+):", index, re.M))
    assert listed == set(numbers), set(numbers) ^ listed
    fixed = re.search(r"\*\*FIXED \(ANSWERS 2\)\*\* \((\d+)\):.*?\n\n((?:- D-\d+[^\n]*\n)+)",
                      index, re.S)
    assert fixed and int(fixed.group(1)) == len(FAULT_KNOBS) == 7
    fixed_numbers = set(re.findall(r"- (D-\d+)", fixed.group(2)))
    expected = {"D-021", "D-026", "D-028", "D-045", "D-046", "D-069", "D-070"}
    assert fixed_numbers == expected
    for number in expected:
        entry = re.search(rf"^### {number}:.*?\n- \*\*Kind:\*\* ([^\n]*)", text, re.M | re.S)
        assert "FIXED (ANSWERS 2)" in entry.group(1), number
    calibrated = re.search(r"\*\*calibrated \(ANSWERS 7\)\*\* \((\d+)\):.*?\n\n((?:- D-\d+[^\n]*\n)+)",
                           index, re.S)
    assert calibrated and "D-076" in calibrated.group(2) and "D-042" in calibrated.group(2)
    for kind in ("spec-inferred", "screen-dump-inferred", "digitization", "Copycat choice",
                 "simplest", "extension"):
        assert f"**{kind}**" in index, kind


def test_spec_is_cross_referenced_to_real_modules_and_tests():
    text = section(SPEC.read_text(encoding="utf-8"), "The modules that implement each section")
    rows = [l for l in text.splitlines() if l.startswith("| ") and not l.startswith("| Section")
            and not l.startswith("|---")]
    assert len(rows) >= 20
    sections = " ".join(rows)
    for number in ("1.", "2.", "3.", "4.", "5.", "6.", "6b", "6c", "7"):
        assert re.search(rf"\| {re.escape(number)}", sections), number
    for name in re.findall(r"`([\w/\-]+\.py)`", sections):
        if name.startswith("tests/"):
            path = PYTHON_DIR / name
        elif name.startswith("test_"):
            path = PYTHON_DIR / "tests" / name
        else:
            path = PYTHON_DIR / "tabletop" / name
        assert path.is_file(), name
    decisions = set(re.findall(r"^### (D-\d+):", DECISIONS.read_text(), re.M))
    for d in re.findall(r"D-\d+", sections):
        assert d in decisions, d


def test_engine_api_names_every_pane_and_marks_the_icons_as_new_design():
    text = ENGINE_API.read_text(encoding="utf-8")
    notes = section(text, "For loop0006")
    for pane in ("P1", "P2", "P2b", "P3", "P3b", "P4", "P4b", "P5", "P6", "P7", "P8", "P9",
                 "P13"):
        assert re.search(rf"\b{pane}\b", notes), pane
    assert "vector icons" in notes and "new design, not French's" in notes
    assert "ANSWERS.md 9" in notes and "[F95 p. 26 (img)]" in notes
    assert "config_defaults.lock" in notes and "--smoke" in notes
    assert "Open questions" in notes
    panes = section(text, "The panes of loop0006, and what feeds them")
    assert panes.count("| P") >= 13


def test_problems_doc_documents_every_key_of_the_files():
    text = section(PROBLEMS_DOC.read_text(encoding="utf-8"),
                   "10. The file format, and what the engine reads")
    keys = set()
    for path in (PYTHON_DIR / "problems").glob("*.json"):
        data = json.loads(path.read_text())
        keys |= set(data)
        keys |= {f"objects.{k}" for k in data["objects"][0]}
    for key in keys:
        name = key.split(".")[-1]
        assert f"`{name}`" in text, key
    assert "`tabletop.problems.validate`" in text


def test_the_knobs_tool_prints_the_table_and_touches_no_file(tmp_path):
    before = README.read_bytes()
    proc = subprocess.run([sys.executable, str(KNOBS)], cwd=tmp_path,
                          capture_output=True, text=True)
    assert proc.returncode == 0 and proc.stdout.startswith("**")
    assert proc.stdout.count("\n| `") == len(re.findall(r"^    [a-z_]+: ", (
        PYTHON_DIR / "tabletop" / "config.py").read_text(), re.M))
    assert README.read_bytes() == before


# -- the pictures of the front page ---------------------------------------------------------

FIGURES_SCRIPT = PYTHON_DIR / "scripts" / "docs_figures.py"


def test_the_front_page_pictures_exist_are_small_captioned_and_regenerated_by_the_script():
    """Every picture of the front page is a PNG under python/docs/, under 200 KB, followed
    by an italic caption, and drawn by `scripts/docs_figures.py` (read as text: the script
    needs PySide6, the tests do not)."""
    text = FRONT.read_text(encoding="utf-8")
    pictures = re.findall(r"^!\[[^\]]+\]\((python/docs/([\w-]+)\.png)\)\n\n\*[^\n]+", text, re.M)
    assert len(pictures) == len(re.findall(r"!\[", text)) >= 5, "each picture has a caption"
    pictures = [(p, n) for p, n in pictures if not n.startswith("gui-")]   # the GUI's screenshots: tests/gui/test_gui_readme.py
    script = FIGURES_SCRIPT.read_text(encoding="utf-8")
    drawn = set(re.findall(r'^    "([\w-]+)": fig_\w+,$', script, re.M))
    assert {name for _, name in pictures} == drawn
    for path, name in pictures:
        png = TARGET / path
        assert png.read_bytes()[:8] == b"\x89PNG\r\n\x1a\n", path
        assert png.stat().st_size < 200 * 1024, path
        assert f'"{name}.png"' in script, name
    assert "python3 -m scripts.docs_figures" in section(text, "Installing")

"""The documentation agrees with the built package (loop item 23: what a document
promises, a test runs).

- every command in a ```sh block of README.md runs and exits 0 (the bench commands cut to
  two seeds and a few terms so the test stays fast; the interactive command is fed run 1's
  terms [M86 pp. 164-166]; the `pip` and `pytest` lines are skipped);
- the module map of README.md names every module of `seekwhence/`, and the knobs table every
  field of `Config`;
- the index of DECISIONS.md lists every entry, with one of the four kinds, the knob of every
  `Config` field, and every decision the code cites;
- every *Implemented in* line of SPEC.md names only files that exist, and every section has
  one;
- the top-level README's scripted commands (CLI, benches, the GUI's `--smoke`) run.
"""

import os
import re
import shlex
import subprocess
import sys
from dataclasses import fields
from pathlib import Path

import pytest

from seekwhence.config import Config

PYTHON = Path(__file__).resolve().parents[1]
PACKAGE = PYTHON / "seekwhence"
README = PYTHON / "README.md"
DECISIONS = PYTHON / "DECISIONS.md"
SPEC = PYTHON / "SPEC.md"
TOP_README = PYTHON.parent / "README.md"

KINDS = ("transcript-inferred", "Meredith-silent", "engineering choice", "extension")


def sh_blocks(text: str) -> list[str]:
    """The lines of every ```sh block, comments stripped."""
    lines = []
    for block in re.findall(r"```sh\n(.*?)```", text, flags=re.S):
        for line in block.splitlines():
            line = line.split("  #")[0].strip()
            if line:
                lines.append(line)
    return lines


def fast(command: str) -> list[str] | None:
    """The command as argv, cut down for the test: `pip` and `pytest` lines are skipped
    (None), bench commands run two seeds and a few terms, the interactive run reads run 1's
    terms from a script. `python3` is this interpreter."""
    argv = shlex.split(command)
    if "pip" in argv or "pytest" in argv:
        return None
    if argv[0] == "python3":
        argv[0] = sys.executable
    if "seekwhence.bench" in argv:
        if "--seeds" in argv:
            argv[argv.index("--seeds") + 1] = "2"
        elif "--golden" not in argv and "--side-by-side" not in argv and "--trace" not in argv:
            argv += ["--seeds", "2"]
        if argv[argv.index("seekwhence.bench") + 1] == "dozen" and "--trace" not in argv:
            if "--terms" in argv:
                argv[argv.index("--terms") + 1] = "8"
            else:
                argv += ["--terms", "8"]
    return argv


def run(argv: list[str], stdin: str = "") -> subprocess.CompletedProcess:
    env = dict(os.environ, PYTHONPATH=str(PYTHON))
    return subprocess.run(argv, cwd=PYTHON, input=stdin, capture_output=True, text=True,
                          env=env, timeout=300)


def test_readme_has_commands():
    commands = sh_blocks(README.read_text())
    assert len(commands) >= 12
    assert any("seekwhence.bench dozen" in c for c in commands)
    assert any("seekwhence.bench appendix" in c for c in commands)


@pytest.mark.parametrize("command", sh_blocks(README.read_text()))
def test_readme_command_runs(command):
    argv = fast(command)
    if argv is None:
        pytest.skip("pip and pytest lines are not run by the smoke test")
    interactive = "--terms" not in argv and "seekwhence.bench" not in argv
    stdin = "3\n7\n3\n7\nok\n" if interactive else ""
    proc = run(argv, stdin)
    assert proc.returncode == 0, f"{command!r} failed:\n{proc.stdout[-800:]}\n{proc.stderr[-800:]}"
    if interactive:
        assert "I have a guess!" in proc.stdout and proc.stdout.rstrip().endswith("bye")
    if "--json" in argv:
        out = Path(argv[argv.index("--json") + 1])
        assert out.exists() and out.stat().st_size > 0
        out.unlink()


def top_readme_commands() -> list[str]:
    """The top-level README's commands that run without a user or a window: the scripted CLI,
    the benches and the GUI's `--smoke`, each as `cd python && ...`."""
    return [c.removeprefix("cd python && ") for c in sh_blocks(TOP_README.read_text())
            if c.startswith("cd python && ")]


def test_top_readme_quick_start_runs():
    command = top_readme_commands()[0]
    proc = run(fast(command))
    assert proc.returncode == 0 and proc.stdout.rstrip().endswith("(Cycle 3 2 (3 7))")


@pytest.mark.parametrize("command", top_readme_commands()[1:])
def test_top_readme_command_runs(command):
    if "seekwhence.gui" in command:
        pytest.importorskip("PySide6")
    proc = run(fast(command))
    assert proc.returncode == 0, f"{command!r} failed:\n{proc.stdout[-800:]}\n{proc.stderr[-800:]}"


def modules() -> set[str]:
    return {p.name for p in PACKAGE.glob("*.py") if p.name != "__init__.py"}


def test_module_map_names_every_module():
    text = README.read_text()
    named = set(re.findall(r"`seekwhence/(\w+\.py)`", text))
    assert modules() <= named, f"modules missing from the README's module map: {sorted(modules() - named)}"
    assert named <= modules() | {"__init__.py"}, f"README names modules that do not exist: {sorted(named - modules())}"


def test_knobs_table_lists_every_config_field():
    text = README.read_text()
    table = text.split("## The knobs")[1].split("\n## ")[0]
    rows = re.findall(r"^\| `(\w+)` \|", table, flags=re.M)
    assert rows == [f.name for f in fields(Config)], "the knobs table must list every Config field, in config.py's order"
    for row in re.findall(r"^\| `\w+` \|.*$", table, flags=re.M):
        assert re.search(r"D-\d{3}|\[M86 p", row), f"a knob row cites nothing: {row}"


def decision_entries(text: str) -> list[str]:
    return re.findall(r"^### (D-\d{3}):", text, flags=re.M)


def index_rows(text: str) -> dict[str, list[str]]:
    index = text.split("## Index")[1].split("\n## ")[0]
    rows = {}
    for line in index.splitlines():
        if line.startswith("| D-"):
            cells = [c.strip() for c in line.strip("|").split("|")]
            rows[cells[0]] = cells[1:]
    return rows


def test_decisions_index_lists_every_entry_with_a_kind():
    text = DECISIONS.read_text()
    rows = index_rows(text)
    entries = decision_entries(text)
    assert list(rows) == entries, "the index must list every decision, in order"
    for number, (title, kind, knob, cited) in rows.items():
        assert kind in KINDS, f"{number}: kind {kind!r} is not one of {KINDS}"
        assert f"### {number}: {title}" in text, f"{number}: the index title differs from the heading"
    for kind in KINDS:
        assert any(row[1] == kind for row in rows.values()), f"no decision of kind {kind}"


def test_decisions_index_lists_every_knob():
    rows = index_rows(DECISIONS.read_text())
    knobs = {name for row in rows.values() for name in re.findall(r"`(\w+)`", row[2])}
    names = {f.name for f in fields(Config)}
    assert names <= knobs, f"Config fields absent from the index's knob column: {sorted(names - knobs)}"
    assert knobs <= names, f"the index names knobs that are not fields: {sorted(knobs - names)}"


def test_decisions_index_lists_every_decision_the_code_cites():
    rows = index_rows(DECISIONS.read_text())
    for path in sorted(PACKAGE.glob("*.py")):
        for number in set(re.findall(r"\bD-\d{3}\b", path.read_text())):
            assert number in rows, f"{path.name} cites {number}, which the index lacks"
            cited = rows[number][3]
            assert f"`{path.stem}`" in cited, f"the index does not say {path.name} cites {number}"
    for number, row in rows.items():
        for module in re.findall(r"`(\w+)`", row[3]):
            assert number in (PACKAGE / f"{module}.py").read_text(), \
                f"the index says {module}.py cites {number}, but it does not"


def test_spec_sections_name_their_modules():
    text = SPEC.read_text()
    headings = re.findall(r"^### (\d+)\. ", text, flags=re.M)
    assert [int(h) for h in headings] == list(range(1, 16))
    lines = re.findall(r"^\*Implemented in:\* (.*)$", text, flags=re.M)
    assert len(lines) == 15, "every section needs one *Implemented in* line"
    for line in lines:
        for name in re.findall(r"`((?:seekwhence|tests|tools)/[\w./]+)`", line):
            assert (PYTHON / name).exists(), f"SPEC names {name}, which does not exist"
        assert re.search(r"`seekwhence/\w+\.py`|no module|`tests/data/`", line)
    # the summary table in the preamble names existing modules
    table = text.split("| Section | Modules |")[1].split("\n\n")[0]
    for name in re.findall(r"`(\w+)`", table):
        if name not in ("__main__",):
            assert (PACKAGE / f"{name}.py").exists() or name == "tests/data", name


def test_the_transcript_citation_checker_runs():
    """`tools/audit_transcript_citations.py` checks every citation of pp. 164-179 against the
    transcriptions [M86 pp. 164-179]; it must run on the package, and the citations it
    confirms outright must stay the majority of those with a checkable snippet
    (`docs/citation-audit.md` records the hand review of the rest)."""
    proc = subprocess.run([sys.executable, str(PYTHON / "tools" / "audit_transcript_citations.py")],
                          capture_output=True, text=True, cwd=PYTHON)
    assert proc.returncode == 0, proc.stderr
    summary = proc.stdout.strip().splitlines()[-1]
    m = re.search(r"(\d+) transcript citations: EXACT (\d+), NEAR (\d+), ELSEWHERE (\d+), "
                  r"NOTFOUND (\d+), NOSNIPPET (\d+)", summary)
    assert m, summary
    total, exact, near, elsewhere, notfound, nosnippet = map(int, m.groups())
    assert total == exact + near + elsewhere + notfound + nosnippet
    assert exact > near + elsewhere + notfound

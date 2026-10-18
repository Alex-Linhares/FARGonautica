"""The GUI's conventions (loop0004 item 00; TASK.md "Conventions"; this test takes the
place of the engine's `test_conventions.py`, which no longer scans `seekwhence/gui/`,
ANSWERS.md 1):

- every public module, class and function docstring under `seekwhence/gui/` cites a source
  tag (`[M86 p. N]`, `[FCCA p. N]`, `[MHN p. N]`), an engine decision `D-NNN`, a GUI
  decision `G-NNN` or a precedent ("Numbo GUI");
- every `G-NNN` used under `seekwhence/gui/` is a heading of `GUI-DECISIONS.md`, and every
  `D-NNN` a heading of `DECISIONS.md`;
- `GUI-DECISIONS.md` has the entry format and its numbers are unique.

Qt-free: it reads the files and runs in the engine's suite without PySide6.
"""

import ast
import re
from pathlib import Path

import seekwhence

PACKAGE = Path(seekwhence.__file__).resolve().parent
GUI = PACKAGE / "gui"
ROOT = PACKAGE.parent
DECISIONS = ROOT / "DECISIONS.md"
GUI_DECISIONS = ROOT / "GUI-DECISIONS.md"

DECISION = re.compile(r"\bD-\d{3}\b")
GUI_DECISION = re.compile(r"\bG-\d{3}\b")
SOURCE_TAG = re.compile(r"\[(M86|FCCA|MHN)\s+pp?\.\s*[ivxlc\d]+(?:\s*[-–]\s*[ivxlc\d]+)?\]")
PRECEDENT = re.compile(r"Numbo\s+GUI")


def modules() -> list[Path]:
    return sorted(GUI.rglob("*.py"))


def cites(text: str | None) -> bool:
    return bool(text) and bool(SOURCE_TAG.search(text) or DECISION.search(text)
                              or GUI_DECISION.search(text) or PRECEDENT.search(text))


def headings(path: Path, pattern: str) -> list[str]:
    return re.findall(pattern, path.read_text(), flags=re.M)


def public_definitions(tree: ast.Module):
    """Every public class and function, at module level or inside a public class (the
    engine's `test_conventions.py`)."""
    for node in tree.body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            if node.name.startswith("_"):
                continue
            yield node
            if isinstance(node, ast.ClassDef):
                for sub in node.body:
                    if (isinstance(sub, (ast.FunctionDef, ast.AsyncFunctionDef))
                            and not sub.name.startswith("_")):
                        yield sub


def test_the_gui_package_exists_with_its_models():
    assert (GUI / "__init__.py").exists()
    assert (GUI / "models" / "__init__.py").exists()
    assert len(modules()) >= 2


def test_every_public_docstring_cites_a_source_or_a_precedent():
    uncited = []
    for path in modules():
        tree = ast.parse(path.read_text(), filename=str(path))
        rel = path.relative_to(GUI)
        if not cites(ast.get_docstring(tree)):
            uncited.append(f"{rel}: module docstring")
        for node in public_definitions(tree):
            if not cites(ast.get_docstring(node)):
                uncited.append(f"{rel}: {node.name}")
    assert not uncited, ("public docstrings without a source tag, decision or Numbo GUI:\n"
                         + "\n".join(uncited))


def test_every_gui_decision_in_the_code_is_a_heading():
    known = set(headings(GUI_DECISIONS, r"^###\s+(G-\d{3})\b"))
    missing = {}
    for path in modules():
        used = set(GUI_DECISION.findall(path.read_text()))
        if used - known:
            missing[str(path.relative_to(GUI))] = sorted(used - known)
    assert not missing, f"G-decisions used in the GUI but absent from GUI-DECISIONS.md: {missing}"


def test_every_engine_decision_in_the_gui_is_a_heading():
    known = set(headings(DECISIONS, r"^###\s+(D-\d{3})\b"))
    missing = {}
    for path in modules():
        used = set(DECISION.findall(path.read_text()))
        if used - known:
            missing[str(path.relative_to(GUI))] = sorted(used - known)
    assert not missing, f"D-decisions used in the GUI but absent from DECISIONS.md: {missing}"


def test_gui_decisions_file_has_the_entry_format():
    text = GUI_DECISIONS.read_text()
    assert "### G-001:" in text and "### G-002:" in text
    for field in ("**Sources:**", "**Choice:**", "**Reason:**", "**Alternatives:**"):
        assert field in text
    numbers = headings(GUI_DECISIONS, r"^###\s+(G-\d{3})\b")
    assert len(numbers) == len(set(numbers)), "a GUI decision number is used twice"
    for number in numbers:
        entry = text.split(f"### {number}:", 1)[1].split("\n### ", 1)[0]
        for field in ("**Sources:**", "**Choice:**", "**Reason:**", "**Alternatives:**"):
            assert field in entry, f"{number} lacks {field}"


def is_qt_import(node) -> bool:
    if isinstance(node, ast.Import):
        names = [a.name for a in node.names]
    elif isinstance(node, ast.ImportFrom):
        names = [node.module or ""]
    else:
        return False
    return any(n == "PySide6" or n.startswith("PySide6.") for n in names)


def importorskip_line(body, name: str) -> int | None:
    """The line of a `pytest.importorskip(name)` statement in `body`, or None."""
    for node in body:
        if isinstance(node, ast.Expr) and isinstance(node.value, ast.Call):
            call = node.value
            if (isinstance(call.func, ast.Attribute) and call.func.attr == "importorskip"
                    and call.args and isinstance(call.args[0], ast.Constant)
                    and call.args[0].value == name):
                return node.lineno
    return None


def test_every_gui_test_file_is_offscreen_and_skips_without_pyside6():
    """Qt tests importorskip PySide6 and pytestqt at module level, before any PySide6
    import; a Qt-free test file imports PySide6 only inside a test that importorskips it
    first (Numbo GUI, `test_packaging.py`), so the file runs in the engine's suite;
    conftest sets offscreen by setdefault."""
    tests = Path(__file__).parent
    conftest = (tests / "conftest.py").read_text()
    assert 'os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")' in conftest
    for path in sorted(tests.glob("test_*.py")):
        tree = ast.parse(path.read_text(), filename=str(path))
        skip_qt = importorskip_line(tree.body, "PySide6")
        skip_pytestqt = importorskip_line(tree.body, "pytestqt")
        for node in tree.body:
            if is_qt_import(node):
                assert skip_qt is not None and skip_pytestqt is not None, path.name
                assert max(skip_qt, skip_pytestqt) < node.lineno, path.name
            if isinstance(node, ast.FunctionDef) and skip_qt is None:
                guard = importorskip_line(node.body, "PySide6")
                for sub in ast.walk(node):
                    if is_qt_import(sub):
                        assert guard is not None and guard < sub.lineno, \
                            f"{path.name}: {node.name} imports PySide6 without importorskip"


# Functions over this many code lines (the docstring not counted) must be split, or listed
# here with their reason (G-063): these declare widgets and connect signals in a straight
# line, the shape of the Numbo GUI's constructors.
LONG_FUNCTION_LINES = 60
LONG_FUNCTIONS_JUSTIFIED = {
    ("controls.py", "ControlsPanel", "__init__"),
    ("config_view.py", "ConfigView", "__init__"),
    ("main_window.py", "MainWindow", "_build_menus"),
    ("console_view.py", "ConsoleView", "__init__"),
    ("hypothesis_view.py", "HypothesisView", "__init__"),
    ("taskrack_view.py", "TaskrackView", "__init__"),
}


def code_lines(node) -> int:
    """The lines of a function apart from its docstring."""
    lines = node.end_lineno - node.lineno + 1
    first = node.body[0] if node.body else None
    if (isinstance(first, ast.Expr) and isinstance(first.value, ast.Constant)
            and isinstance(first.value.value, str)):
        lines -= first.end_lineno - first.lineno + 1
    return lines


def long_functions() -> set:
    """Every function under `seekwhence/gui/` over `LONG_FUNCTION_LINES` code lines, as
    (file name, class name or "", function name)."""
    found = set()
    for path in modules():
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                for item in node.body:
                    if isinstance(item, ast.FunctionDef) and code_lines(item) > LONG_FUNCTION_LINES:
                        found.add((path.name, node.name, item.name))
            elif isinstance(node, ast.FunctionDef) and code_lines(node) > LONG_FUNCTION_LINES:
                if not any(node is item for cls in ast.walk(tree)
                           if isinstance(cls, ast.ClassDef) for item in cls.body):
                    found.add((path.name, "", node.name))
    return found


def test_functions_over_sixty_lines_are_split_or_justified():
    found = long_functions()
    unjustified = found - LONG_FUNCTIONS_JUSTIFIED
    assert not unjustified, f"split these, or list them in LONG_FUNCTIONS_JUSTIFIED with a reason: {sorted(unjustified)}"
    stale = LONG_FUNCTIONS_JUSTIFIED - found
    assert not stale, f"no longer over the limit; remove from LONG_FUNCTIONS_JUSTIFIED: {sorted(stale)}"

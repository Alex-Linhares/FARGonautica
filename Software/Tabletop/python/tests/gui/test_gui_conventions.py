"""The GUI's conventions (loop0006 item 00; TASK.md "Conventions"; this test takes the
place of the engine's `test_conventions.py` and `test_package.py`, which no longer scan
`tabletop/gui/`, G-003):

- every public module, class and function docstring under `tabletop/gui/` cites a source
  tag (`[F95 p. N]`, `[FCCA p. N]`, `[FH91 p. N]`), an engine decision `D-NNN`, a GUI
  decision `G-NNN` or a precedent ("Numbo GUI", "Seek-Whence GUI");
- every `G-NNN` used under `tabletop/gui/` and `tests/gui/` is a heading of
  `GUI-DECISIONS.md`, and every `D-NNN` a heading of `DECISIONS.md`;
- `GUI-DECISIONS.md` has the entry format, an index naming every entry, and unique numbers;
- no function under `tabletop/gui/` is over 60 code lines unless listed here with its
  G-decision;
- nothing under `tabletop/gui/models/` imports Qt (G-001);
- every test file here is offscreen and skips without PySide6 (G-002).

Qt-free: it reads the files and runs in the engine's suite without PySide6.
"""

import ast
import re
from pathlib import Path

import tabletop

PACKAGE = Path(tabletop.__file__).resolve().parent
GUI = PACKAGE / "gui"
MODELS = GUI / "models"
ROOT = PACKAGE.parent
TESTS = Path(__file__).resolve().parent
DECISIONS = ROOT / "DECISIONS.md"
GUI_DECISIONS = ROOT / "GUI-DECISIONS.md"

DECISION = re.compile(r"\bD-\d{3}\b")
GUI_DECISION = re.compile(r"\bG-\d{3}\b")
SOURCE_TAG = re.compile(r"\[(F95|FCCA|FH91)\s+pp?\.\s*\d+(?:\s*[-–]\s*\d+)?(?:\s*\(img\))?\]")
PRECEDENT = re.compile(r"(Numbo|Seek-Whence) GUI")      # one space, as the gate's regex
QT_ROOTS = ("PySide6", "shiboken6", "PySide2", "PyQt5", "PyQt6", "pytestqt")


def modules() -> list[Path]:
    return sorted(p for p in GUI.rglob("*.py") if "__pycache__" not in p.parts)


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


def imported_roots(path: Path) -> dict[str, int]:
    """Top-level name -> first line, for every absolute import in a module."""
    roots = {}
    for node in ast.walk(ast.parse(path.read_text(), filename=str(path))):
        if isinstance(node, ast.Import):
            for alias in node.names:
                roots.setdefault(alias.name.split(".")[0], node.lineno)
        elif isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
            roots.setdefault(node.module.split(".")[0], node.lineno)
    return roots


# -- the package ------------------------------------------------------------------------

def test_the_gui_package_exists_with_its_models():
    assert (GUI / "__init__.py").exists()
    assert (GUI / "models" / "__init__.py").exists()
    assert (GUI / "app.py").exists() and (GUI / "__main__.py").exists()
    assert (GUI / "README.md").exists(), "the GUI's own module map"
    assert len(modules()) >= 4


def test_the_gui_readme_names_every_gui_module():
    """`tabletop/gui/README.md` is the GUI's module map (G-003: the engine's module-map test
    leaves `tabletop/gui/` to it). Every module but the `__init__.py`s is named, by its file
    name or its path relative to `tabletop/gui/`; the models by `models/` or by name."""
    text = (GUI / "README.md").read_text(encoding="utf-8")
    missing = []
    for p in modules():
        if p.name == "__init__.py":
            continue
        rel = p.relative_to(GUI).as_posix()
        if f"`{rel}`" not in text and f"`{p.name}`" not in text:
            missing.append(rel)
    assert not missing, f"modules absent from tabletop/gui/README.md: {missing}"


# -- docstrings and decisions -------------------------------------------------------------

def test_every_public_docstring_cites_a_source_or_a_precedent():
    uncited = []
    for path in modules():
        tree = ast.parse(path.read_text(), filename=str(path))
        rel = path.relative_to(GUI)
        if path.read_text().strip() and not cites(ast.get_docstring(tree)):
            uncited.append(f"{rel}: module docstring")
        for node in public_definitions(tree):
            if not cites(ast.get_docstring(node)):
                uncited.append(f"{rel}: {node.name}")
    assert not uncited, ("public docstrings without a source tag, decision or precedent:\n"
                         + "\n".join(uncited))


def scanned_files() -> list[Path]:
    """The GUI's code and tests, whose decision numbers must be headings."""
    return modules() + sorted(TESTS.glob("*.py"))


def test_every_gui_decision_in_the_code_is_a_heading():
    known = set(headings(GUI_DECISIONS, r"^###\s+(G-\d{3})\b"))
    missing = {}
    for path in scanned_files():
        used = set(GUI_DECISION.findall(path.read_text()))
        if used - known:
            missing[str(path.relative_to(ROOT))] = sorted(used - known)
    assert not missing, f"G-decisions used but absent from GUI-DECISIONS.md: {missing}"


def test_every_engine_decision_in_the_gui_is_a_heading():
    known = set(headings(DECISIONS, r"^###\s+(D-\d{3})\b"))
    missing = {}
    for path in scanned_files():
        used = set(DECISION.findall(path.read_text()))
        if used - known:
            missing[str(path.relative_to(ROOT))] = sorted(used - known)
    assert not missing, f"D-decisions used in the GUI but absent from DECISIONS.md: {missing}"


def test_gui_decisions_file_has_the_entry_format_and_an_index():
    text = GUI_DECISIONS.read_text()
    numbers = headings(GUI_DECISIONS, r"^###\s+(G-\d{3})\b")
    assert numbers[:4] == ["G-001", "G-002", "G-003", "G-004"]
    assert len(numbers) == len(set(numbers)), "a GUI decision number is used twice"
    for number in numbers:
        entry = text.split(f"### {number}:", 1)[1].split("\n### ", 1)[0]
        for field in ("**Sources:**", "**Choice:**", "**Reason:**", "**Alternatives:**"):
            assert field in entry, f"{number} lacks {field}"
    index = text.split("## Index", 1)[1].split("\n## ", 1)[0]
    indexed = re.findall(r"^\|\s*(G-\d{3})\s*\|", index, flags=re.M)
    assert indexed == numbers, "the index does not list every decision once, in order"


# -- the models are Qt-free (G-001) --------------------------------------------------------

def test_nothing_under_models_imports_qt():
    bad = [f"{p.relative_to(GUI)}:{line} imports {root}"
           for p in modules() if MODELS in p.parents
           for root, line in imported_roots(p).items() if root in QT_ROOTS]
    assert not bad, "\n".join(bad)


def test_the_gui_package_itself_is_importable_without_qt_at_import_time():
    """`tabletop.gui` and `tabletop.gui.app` import Qt only inside functions (Numbo GUI,
    `app.py`), so `run()` can say what to install when PySide6 is missing."""
    for name in ("__init__.py", "app.py", "__main__.py"):
        tree = ast.parse((GUI / name).read_text())
        top = [node for node in tree.body if is_qt_import(node)]
        assert not top, f"{name} imports Qt at module level: line {top[0].lineno}"


# -- the test files (G-002) ----------------------------------------------------------------

def is_qt_import(node) -> bool:
    if isinstance(node, ast.Import):
        names = [a.name for a in node.names]
    elif isinstance(node, ast.ImportFrom):
        names = [node.module or ""]
    else:
        return False
    return any(n.split(".")[0] in ("PySide6", "shiboken6") for n in names)


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
    conftest sets offscreen by setdefault (G-002)."""
    conftest = (TESTS / "conftest.py").read_text()
    assert 'os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")' in conftest
    for path in sorted(TESTS.glob("test_*.py")):
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


# -- long functions ------------------------------------------------------------------------

# Functions over this many code lines (the docstring not counted) must be split, or listed
# here as (file name, class name or "", function name) with the G-decision that justifies
# them (TASK.md "Conventions"). None yet.
LONG_FUNCTION_LINES = 60
LONG_FUNCTIONS_JUSTIFIED: set[tuple[str, str, str]] = set()


def code_lines(node) -> int:
    """The lines of a function apart from its docstring."""
    lines = node.end_lineno - node.lineno + 1
    first = node.body[0] if node.body else None
    if (isinstance(first, ast.Expr) and isinstance(first.value, ast.Constant)
            and isinstance(first.value.value, str)):
        lines -= first.end_lineno - first.lineno + 1
    return lines


def long_functions() -> set:
    """Every function under `tabletop/gui/` over `LONG_FUNCTION_LINES` code lines, as
    (file name, class name or "", function name)."""
    found = set()
    for path in modules():
        tree = ast.parse(path.read_text(encoding="utf-8"))
        in_class = set()
        for cls in ast.walk(tree):
            if isinstance(cls, ast.ClassDef):
                for item in cls.body:
                    if isinstance(item, ast.FunctionDef):
                        in_class.add(id(item))
                        if code_lines(item) > LONG_FUNCTION_LINES:
                            found.add((path.name, cls.name, item.name))
        for node in ast.walk(tree):
            if (isinstance(node, ast.FunctionDef) and id(node) not in in_class
                    and code_lines(node) > LONG_FUNCTION_LINES):
                found.add((path.name, "", node.name))
    return found


def test_functions_over_sixty_lines_are_split_or_justified():
    found = long_functions()
    unjustified = found - LONG_FUNCTIONS_JUSTIFIED
    assert not unjustified, ("split these, or list them in LONG_FUNCTIONS_JUSTIFIED with their "
                             f"G-decision: {sorted(unjustified)}")
    stale = LONG_FUNCTIONS_JUSTIFIED - found
    assert not stale, f"no longer over the limit; remove from LONG_FUNCTIONS_JUSTIFIED: {sorted(stale)}"

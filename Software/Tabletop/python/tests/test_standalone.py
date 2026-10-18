"""Software/Tabletop/ works on its own, as the repository `git subtree split` will make
(ANSWERS.md 6; TASK.md "Standalone"): nothing under it imports anything but the standard
library, `tabletop`, pytest and its own test helpers; no file names an absolute home path,
the Zotero store or the loop folders; no `sys.path` tricks; every relative Markdown link
resolves inside the folder; links that leave it are absolute URLs. The compiled engine's
build artefacts (the generated C and HTML beside a `.pyx`, the extension modules,
setuptools' `build/`) are git-ignored and skipped by every scanner here (`is_generated`,
shared with `tests/test_conventions.py`); a checkout without a build refuses to run, and
`tools/standalone_check.sh` checks that, builds the engine where a compiler and Cython are
found, and runs the tests on the pure engine by explicit request alone (D-107, D-110).
"""

import ast
import re
import sys
from pathlib import Path
from urllib.parse import unquote

from sources import PYTHON_DIR, TARGET

SKIP_DIRS = {"__pycache__", ".pytest_cache", ".venv", "build", "dist"}
# The compiled engine's build artefacts (D-110): extension modules anywhere, and the C and
# the annotation HTML Cython generates beside a `.pyx`.
BUILT_SUFFIXES = {".so", ".pyd", ".o"}
GENERATED_BESIDE_PYX = {".c", ".html"}
# Written with character classes so that this pattern's own text never matches it.
FORBIDDEN = re.compile(r"(^|[\s\"'=(])/(home|Users)/[\w.-]+/|zotero[-]snap/|Zotero/stor[a]ge/"
                       r"|(^|/)ralph_loops(/|$)")


def is_generated(path: Path) -> bool:
    """Whether `path` is a build artefact a text scanner must skip (D-110): under a build,
    cache or environment folder, an extension module, or a `.c`/`.html` beside a `.pyx`
    of the same name."""
    if any(p in SKIP_DIRS or p.endswith(".egg-info") for p in path.parts):
        return True
    if path.suffix in BUILT_SUFFIXES:
        return True
    return path.suffix in GENERATED_BESIDE_PYX and path.with_suffix(".pyx").exists()


def files_under(root: Path):
    for f in sorted(root.rglob("*")):
        if is_generated(f.relative_to(root)) or is_generated(f):
            continue
        if f.is_file():
            yield f


def py_files():
    return [f for f in files_under(TARGET) if f.suffix == ".py"]


def imports_of(path: Path):
    tree = ast.parse(path.read_text(), filename=str(path))
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for a in node.names:
                yield a.name.split(".")[0], node.lineno
        elif isinstance(node, ast.ImportFrom) and not node.level and node.module:
            yield node.module.split(".")[0], node.lineno


# Qt may be imported only in the GUI's code places (loop0006; GUI-DECISIONS.md G-003): the
# GUI package, its tests and the scripts (docs_figures.py, docs_screenshots.py).
QT_MODULES = {"PySide6", "shiboken6", "pytestqt"}
GUI_CODE_PLACES = ("tabletop/gui/", "tests/gui/", "tests_slow/", "scripts/")
# Cython and setuptools only in the two files that build the compiled engine (D-110).
BUILD_MODULES = {"Cython", "setuptools"}
BUILD_FILES = ("setup.py", "tools/build_fast.py")


def in_gui_code(f: Path) -> bool:
    """Whether `f` is under one of the GUI's code places (relative to python/)."""
    return PYTHON_DIR in f.parents and f.relative_to(PYTHON_DIR).as_posix().startswith(GUI_CODE_PLACES)


def in_build_code(f: Path) -> bool:
    """Whether `f` is one of the build files (relative to python/)."""
    return PYTHON_DIR in f.parents and f.relative_to(PYTHON_DIR).as_posix() in BUILD_FILES


def test_every_import_is_stdlib_tabletop_pytest_or_a_local_helper():
    local = {f.stem for f in py_files()} | {d.name for d in TARGET.rglob("*") if d.is_dir()}
    allowed = set(sys.stdlib_module_names) | {"pytest", "_pytest"} | local
    bad = [f"{f.relative_to(TARGET)}:{line} imports {mod}"
           for f in py_files() for mod, line in imports_of(f)
           if mod not in allowed and not (mod in QT_MODULES and in_gui_code(f))
           and not (mod in BUILD_MODULES and in_build_code(f))]
    assert not bad, "\n".join(bad)


def docstring_ids(tree: ast.Module) -> set[int]:
    return {id(n.body[0].value) for n in ast.walk(tree)
            if isinstance(n, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef))
            and n.body and isinstance(n.body[0], ast.Expr)
            and isinstance(n.body[0].value, ast.Constant)}


def test_no_path_outside_the_folder_in_code():
    bad = []
    for f in py_files():
        tree = ast.parse(f.read_text(), filename=str(f))
        skip = docstring_ids(tree)
        for node in ast.walk(tree):
            if (isinstance(node, ast.Constant) and isinstance(node.value, str)
                    and id(node) not in skip and FORBIDDEN.search(node.value)):
                bad.append(f"{f.relative_to(TARGET)}:{node.lineno} {node.value[:60]!r}")
    for f in files_under(TARGET):
        if f.suffix in {".sh", ".toml", ".cfg", ".ini", ".json"}:
            for n, line in enumerate(f.read_text(errors="replace").splitlines(), 1):
                if FORBIDDEN.search(line) and not line.lstrip().startswith("#"):
                    bad.append(f"{f.relative_to(TARGET)}:{n}")
    assert not bad, "paths outside Software/Tabletop/:\n" + "\n".join(bad)


def test_no_sys_path_tricks():
    bad = []
    for f in py_files():
        for node in ast.walk(ast.parse(f.read_text(), filename=str(f))):
            if (isinstance(node, ast.Attribute) and node.attr == "path"
                    and isinstance(node.value, ast.Name) and node.value.id == "sys"):
                bad.append(f"{f.relative_to(TARGET)}:{node.lineno}")
    assert not bad, "sys.path is touched in:\n" + "\n".join(bad)


def strip_fences(text: str) -> str:
    text = re.sub(r"^```.*?^```", "", text, flags=re.M | re.S)
    return re.sub(r"`[^`\n]*`", "", text)


LINK = re.compile(r"\]\(\s*<?([^)\s>]+)>?(?:\s+\"[^\"]*\")?\s*\)")


def test_every_relative_markdown_link_resolves_inside_the_folder():
    root = TARGET.resolve()
    bad = []
    for f in files_under(TARGET):
        if f.suffix != ".md":
            continue
        for m in LINK.finditer(strip_fences(f.read_text(errors="replace"))):
            link = m.group(1)
            if re.match(r"^(https?:|mailto:|#)", link):
                continue
            target = link.split("#")[0]
            dest = (f.parent / unquote(target)).resolve()
            if target.startswith("/") or not (dest == root or root in dest.parents):
                bad.append(f"{f.relative_to(TARGET)}: {link} leaves the folder")
            elif not dest.exists():
                bad.append(f"{f.relative_to(TARGET)}: {link} points at nothing")
    assert not bad, "\n".join(bad)


def test_outside_links_are_absolute_github_urls():
    """Links to FARGonautica's files use its GitHub URL (spaces as %20), never `../`."""
    pattern = re.compile(r"https://github\.com/fargonauts/FARGonautica/blob/master/\S+")
    for f in files_under(TARGET):
        if f.suffix != ".md":
            continue
        for m in pattern.finditer(f.read_text(errors="replace")):
            assert " " not in m.group(0) and ".." not in m.group(0), f"{f.name}: {m.group(0)}"


def test_the_package_layout():
    for name in ("README.md", "SPEC.md", "DECISIONS.md", "RESULTS.md", "pyproject.toml",
                 "run-tests.sh", ".gitignore", "tools/check_quotes.py", "tests/sources.py"):
        assert (PYTHON_DIR / name).is_file(), name
    assert (TARGET / "README.md").is_file()


def test_the_front_page_links_stay_inside_or_go_to_github():
    """Every link of `Software/Tabletop/README.md` is an anchor, an absolute GitHub URL or a
    path into `python/` that exists (item 29: the front page is read in two places)."""
    text = strip_fences((TARGET / "README.md").read_text(errors="replace"))
    links = [m.group(1) for m in LINK.finditer(text)]
    assert len(links) >= 20
    for link in links:
        if link.startswith("#"):
            continue
        if link.startswith("https://"):
            assert link.startswith("https://github.com/fargonauts/"), link
            continue
        assert link.startswith("python/"), link
        assert (TARGET / unquote(link.split("#")[0])).exists(), link


# ---- the compiled engine in a standalone checkout (D-107, D-110) ------------------------

def test_generated_files_are_recognised_and_skipped(tmp_path):
    """The scanners skip what a build leaves: a `.c` or `.html` beside a `.pyx`, any
    extension module, anything under `build/`; a `.pyx`, a `.py` and a `.c` with no
    `.pyx` beside it are scanned."""
    native = tmp_path / "tabletop" / "_native"
    native.mkdir(parents=True)
    for name in ("rng.pyx", "rng.c", "rng.html", "rng.cpython-312-x86_64-linux-gnu.so",
                 "__init__.py", "other.c"):
        (native / name).write_text("")
    assert is_generated(native / "rng.c") and is_generated(native / "rng.html")
    assert is_generated(native / "rng.cpython-312-x86_64-linux-gnu.so")
    assert is_generated(tmp_path / "build" / "lib" / "x.py")
    assert not is_generated(native / "rng.pyx") and not is_generated(native / "__init__.py")
    assert not is_generated(native / "other.c")
    scanned = {f.name for f in files_under(tmp_path)}
    assert scanned == {"rng.pyx", "__init__.py", "other.c"}
    real = {f.name for f in files_under(TARGET) if "_native" in f.parts}
    assert real and all(f.endswith((".pyx", ".pxd", ".py")) for f in real), real


def test_the_gitignore_covers_every_build_artefact():
    """`Software/Tabletop/.gitignore` ignores the generated C and HTML, the extension
    modules and setuptools' `build/`, so a build never reaches the repository (D-110)."""
    patterns = [l.strip() for l in (TARGET / ".gitignore").read_text().splitlines()
                if l.strip() and not l.startswith("#")]
    for wanted in ("python/tabletop/_native/*.c", "python/tabletop/_native/*.html",
                   "python/tabletop/_native/*.so", "python/tabletop/_native/*.pyd",
                   "python/build/", "__pycache__/"):
        assert wanted in patterns, wanted


def test_the_standalone_script_refuses_without_a_build_builds_where_it_can_and_asks_explicitly():
    """`tools/standalone_check.sh` (its full run is recorded in `docs/standalone.md`): the
    checkout without a build is first checked to refuse (`no-build-refuses`, a run with
    TABLETOP_ENGINE unset that must fail naming the build); its test steps request the
    pure engine explicitly, the only value the script ever gives the variable; the
    compiled engine is built there, and the suite run on it, only where a C compiler and
    Cython are found, and the venv install is expected to fail clearly otherwise."""
    text = (PYTHON_DIR / "tools" / "standalone_check.sh").read_text()
    assert "step no-build-refuses" in text
    assert "env -u TABLETOP_ENGINE" in text
    values = set(re.findall(r"TABLETOP_ENGINE=(\w+)", text))
    assert values == {"python"}, values
    assert "build_fast.py" in text and "HAVE_CC" in text and "HAVE_CYTHON" in text
    assert "step tests-compiled-engine" in text and "step build-compiled-engine" in text
    assert "venv-install-fails-clearly" in text

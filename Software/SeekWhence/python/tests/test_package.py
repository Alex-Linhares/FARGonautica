"""The package imports, and depends on nothing but the standard library and itself
(the package's convention: standard library only)."""

import ast
import importlib
import sys
from pathlib import Path

import seekwhence

PACKAGE = Path(seekwhence.__file__).resolve().parent


def modules():
    # seekwhence/gui/ (the optional PySide6 GUI) is checked by tests/gui/; the engine
    # never imports it (loop0004, ANSWERS.md 1).
    return sorted(p for p in PACKAGE.rglob("*.py") if p.relative_to(PACKAGE).parts[0] != "gui")


def test_package_imports():
    assert seekwhence.__version__
    for path in modules():
        name = "seekwhence." + ".".join(path.relative_to(PACKAGE).with_suffix("").parts)
        name = name.removesuffix(".__init__")
        importlib.import_module(name)


def imported_roots(path: Path) -> set[str]:
    """The top-level names imported by a module, found with ast (relative imports are
    inside the package and are not returned)."""
    roots = set()
    for node in ast.walk(ast.parse(path.read_text(), filename=str(path))):
        if isinstance(node, ast.Import):
            roots.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
            roots.add(node.module.split(".")[0])
    return roots


def test_only_stdlib_and_seekwhence_are_imported():
    allowed = set(sys.stdlib_module_names) | {"seekwhence"}
    offenders = {}
    for path in modules():
        extra = imported_roots(path) - allowed
        if extra:
            offenders[path.name] = sorted(extra)
    assert not offenders, f"third-party imports: {offenders}"


def test_main_serves_help_and_runs_a_script(capsys):
    """`python3 -m seekwhence` (DECISION D-069): `--help` exits 0; a scripted run of run 1's
    terms on the fence seed ends with `bye` and the outcome line [M86 p. 166]."""
    import pytest
    from seekwhence.__main__ import main
    with pytest.raises(SystemExit) as exit:
        main(["--help"])
    assert exit.value.code == 0
    assert "--terms" in capsys.readouterr().out
    assert main(["--seed", "4", "--terms", "3", "7", "3", "7", "--answers", "ok"]) == 0
    lines = capsys.readouterr().out.splitlines()
    assert lines[-2] == "bye" and lines[-1].endswith("(seed 4): (Cycle 3 2 (3 7))")

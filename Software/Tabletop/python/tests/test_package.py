"""The package imports, depends on nothing but the standard library and itself, never on the
`random` module, and has no GUI (README.md, "Conventions"; ANSWERS.md 5)."""

import ast
import importlib
import subprocess
import sys
from pathlib import Path

import pytest

import tabletop

PACKAGE = Path(tabletop.__file__).resolve().parent
GUI_MODULES = {"PySide6", "PySide2", "PyQt5", "PyQt6", "tkinter", "Tkinter", "wx", "gi",
               "kivy", "pygame"}


def modules() -> list[Path]:
    # tabletop/gui/ (loop0006) is scanned by tests/gui/test_gui_conventions.py instead.
    return sorted(p for p in PACKAGE.rglob("*.py") if p.relative_to(PACKAGE).parts[0] != "gui")


def imported_roots(path: Path) -> dict[str, int]:
    """Top-level name -> first line, for every absolute import in a module (relative imports
    are inside the package and are not returned)."""
    roots = {}
    for node in ast.walk(ast.parse(path.read_text(), filename=str(path))):
        if isinstance(node, ast.Import):
            for alias in node.names:
                roots.setdefault(alias.name.split(".")[0], node.lineno)
        elif isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
            roots.setdefault(node.module.split(".")[0], node.lineno)
    return roots


def test_package_imports():
    assert tabletop.__version__
    for path in modules():
        name = "tabletop." + ".".join(path.relative_to(PACKAGE).with_suffix("").parts)
        importlib.import_module(name.removesuffix(".__init__"))


def test_only_stdlib_and_tabletop_are_imported():
    allowed = set(sys.stdlib_module_names) | {"tabletop"}
    offenders = {path.name: sorted(set(imported_roots(path)) - allowed)
                 for path in modules() if set(imported_roots(path)) - allowed}
    assert not offenders, f"third-party imports in the engine: {offenders}"


def test_the_random_module_is_never_imported():
    """All randomness goes through `tabletop.rng` (D-064)."""
    offenders = [f"{path.name}:{imported_roots(path)['random']}"
                 for path in modules() if "random" in imported_roots(path)]
    assert not offenders, f"`import random` in the engine: {offenders}"


def test_no_gui_code():
    """No GUI toolkit is imported outside `tabletop/gui/` (loop0006; GUI-DECISIONS.md G-003)."""
    offenders = [f"{path.name}: {sorted(set(imported_roots(path)) & GUI_MODULES)}"
                 for path in modules() if set(imported_roots(path)) & GUI_MODULES]
    assert not offenders, f"GUI imports in the engine: {offenders}"


def test_main_without_a_problem_is_a_usage_error(capsys):
    """`python3 -m tabletop` with no problem prints the usage and exits 2 (item 21; the
    options are tested in `test_cli.py`)."""
    from tabletop.__main__ import main
    assert main([]) == 2
    assert "problem name" in capsys.readouterr().err
    proc = subprocess.run([sys.executable, "-m", "tabletop"], cwd=PACKAGE.parent,
                          capture_output=True, text=True, timeout=60)
    assert proc.returncode == 2 and "usage" in proc.stderr


def test_config_is_a_frozen_dataclass_with_the_four_profiles():
    """Item 10's `Config` holds every knob (DECISIONS.md D-004 to D-070); `PROFILES` has
    `standard`, `buridan`, `human` and `french-faults` (D-066)."""
    from dataclasses import fields, is_dataclass
    from tabletop.config import PROFILES, Config
    assert is_dataclass(Config) and Config.__dataclass_params__.frozen
    assert len(fields(Config)) >= 60
    assert PROFILES["standard"] == Config()
    assert list(PROFILES) == ["standard", "buridan", "human", "french-faults"]
    with pytest.raises(AttributeError):
        setattr(PROFILES["standard"], "anything", 1)

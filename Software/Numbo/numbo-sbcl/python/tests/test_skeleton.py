"""The package skeleton exists and pytest is wired to it (loop0002 item 2)."""

import importlib
import pathlib

from conftest import FIXTURES_DIR, PYTHON_DIR


def test_numbo_package_imports_from_python_dir():
    numbo = importlib.import_module("numbo")
    assert pathlib.Path(numbo.__file__).resolve().parent == PYTHON_DIR / "numbo"


def test_layout():
    for rel in ["pyproject.toml", "numbo/__init__.py", "tests/conftest.py",
                "scripts/regen_fixtures.sh", "fixtures"]:
        assert (PYTHON_DIR / rel).exists(), rel


def test_fixtures_stay_small():
    # TASK.md: keep python/fixtures/ under about 20 MB.
    total = sum(p.stat().st_size for p in FIXTURES_DIR.rglob("*") if p.is_file())
    assert total < 20 * 1024 * 1024

"""Access to the frozen Chez outputs in python/fixtures/ (see oracle/capture.py).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

    from chez_fixtures import chez
    chez("utilities", "prob?")      # the text Chez printed for (test prob? ...)
"""
from __future__ import annotations

from functools import lru_cache
from pathlib import Path

PYTHON = Path(__file__).resolve().parents[1]
FIXTURES = PYTHON / "fixtures"

import capture  # python/oracle/capture.py, on sys.path through conftest.py


@lru_cache(maxsize=None)
def manifest(battery: str, root: Path = FIXTURES) -> tuple[str, ...]:
    """The battery's test names, in order."""
    return tuple((root / battery / "MANIFEST").read_text().splitlines())


@lru_cache(maxsize=None)
def values(battery: str, root: Path = FIXTURES) -> dict[str, str]:
    """Test name -> the value Chez printed, as text."""
    return {
        name: (root / battery / capture.file_name(i, name)).read_bytes().decode()
        for i, name in enumerate(manifest(battery, root))
    }


def chez(battery: str, test: str) -> str:
    """What Chez printed after "TEST => " for the battery's (test TEST ...)."""
    return values(battery)[test]

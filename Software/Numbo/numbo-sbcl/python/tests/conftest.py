"""Shared pytest helpers for the Numbo translation.

Every expected value in these tests comes from the SBCL oracle (src/, oracle
mode), through the JSON fixtures that tests/oracle/*.lisp write into
python/fixtures/.  Nothing here is computed by the Python under test.
"""

import json
import pathlib
import shutil
import subprocess

import pytest

from numbo.franz import Symbol, find_package, intern

PYTHON_DIR = pathlib.Path(__file__).resolve().parent.parent
REPO_DIR = PYTHON_DIR.parent
FIXTURES_DIR = PYTHON_DIR / "fixtures"
REGEN_SCRIPT = PYTHON_DIR / "scripts" / "regen_fixtures.sh"


def load_fixture(name):
    """Parse python/fixtures/<name> as JSON (integers stay exact Python ints)."""
    with open(FIXTURES_DIR / name, encoding="utf-8") as f:
        return json.load(f)


def regenerate_fixtures(out_dir):
    """Run python/scripts/regen_fixtures.sh with its output sent to out_dir."""
    return subprocess.run(
        ["bash", str(REGEN_SCRIPT), str(out_dir)],
        cwd=REPO_DIR, capture_output=True, text=True, timeout=600,
    )


def lisp_data(x):
    """The Python model of Lisp data -> the trace's Lisp-data encoding
    (src/oracle.lisp): a symbol is its name, a string is {"str": ...}, nil
    is null.  Compare results with json.dumps, which keeps 3 and 3.0 apart."""
    if x is None or x is False:
        return None
    if x is True:
        return True
    if isinstance(x, (int, float)):
        return x
    if isinstance(x, str):
        return {"str": x}
    if isinstance(x, Symbol):
        return ":" + x.name if x.package == "KEYWORD" else x.name
    if isinstance(x, (list, tuple)):
        return [lisp_data(v) for v in x] if x else None
    raise TypeError(f"cannot encode {x!r}")


def lisp_data_decode(x):
    """The trace's Lisp-data encoding -> the Python model of Lisp data."""
    if isinstance(x, str):
        if x.startswith(":") and len(x) > 1:
            return intern(x[1:], find_package("keyword"))
        return intern(x)
    if isinstance(x, dict):
        assert list(x) == ["str"], x
        return x["str"]
    if isinstance(x, list):
        return [lisp_data_decode(v) for v in x]
    return x


@pytest.fixture
def fixture():
    """The load_fixture function, for tests that prefer a pytest fixture."""
    return load_fixture


requires_sbcl = pytest.mark.skipif(
    shutil.which("sbcl") is None, reason="sbcl (the oracle) is not on PATH")

"""Shared pytest helpers for the Numbo translation.

Every expected value in these tests comes from the SBCL oracle (lisp/src/, oracle
mode), through the JSON fixtures that lisp/tests/oracle/*.lisp write into
python/fixtures/.  Nothing here is computed by the Python under test.
"""

import json
import os
import pathlib
import shutil
import subprocess

import pytest

from numbo.franz import Symbol, find_package, intern

# Qt (the GUI tests, python/tests/gui/) draws offscreen unless told otherwise.
os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")

PYTHON_DIR = pathlib.Path(__file__).resolve().parent.parent
REPO_DIR = PYTHON_DIR.parent
LISP_DIR = REPO_DIR / "lisp"   # the SBCL port, the tests' oracle
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
    (lisp/src/oracle.lisp): a symbol is its name, a string is {"str": ...}, nil
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


# -- process pools started early ------------------------------------------------
#
# A module whose tests check many long runs in a process pool can declare
# EARLY_JOBS = (fixture name, function, [args, ...]).  If a selected test
# uses that fixture, the jobs start as soon as collection is done, in a
# child Python process (its own pool there: a pool's threads in this
# process would make every later fork() here unsafe), and run while the
# other tests do.  The fixture collects them with early_results() (and runs
# them itself if they weren't started).  The suite's wall time then no
# longer waits for the longest run (item 2's pool: 38 s).

_EARLY = {}     # module name -> (Popen, results path, log path)

_EARLY_CHILD = """
import concurrent.futures, importlib, os, pickle, sys
tests, name, out = sys.argv[1:4]
sys.path[:0] = [tests, os.path.dirname(tests)]
fixture, fn, runs = importlib.import_module(name).EARLY_JOBS
with concurrent.futures.ProcessPoolExecutor(max_workers=min(len(runs), os.cpu_count() or 1)) as pool:
    jobs = {run: pool.submit(fn, *run) for run in runs}
    results = {run: job.result() for run, job in jobs.items()}
with open(out, "wb") as f:
    pickle.dump(results, f)
"""


def pytest_collection_finish(session):
    if session.config.option.collectonly:
        return
    import sys
    import tempfile
    for item in session.items:
        module = getattr(item, "module", None)
        jobs = getattr(module, "EARLY_JOBS", None)
        if jobs is None or module.__name__ in _EARLY:
            continue
        if jobs[0] not in getattr(item, "fixturenames", ()):
            continue
        fd, out = tempfile.mkstemp(suffix=".pickle", prefix="numbo-early-")
        os.close(fd)
        log = open(out + ".log", "w+")
        proc = subprocess.Popen([sys.executable, "-c", _EARLY_CHILD,
                                 str(pathlib.Path(__file__).parent), module.__name__, out],
                                stdout=log, stderr=subprocess.STDOUT)
        _EARLY[module.__name__] = (proc, out, log)


def early_results(module_name):
    """{args: result} of MODULE_NAME's early jobs, or None if they weren't
    started (a RuntimeError with the child's output if it failed)."""
    import pickle
    entry = _EARLY.pop(module_name, None)
    if entry is None:
        return None
    proc, out, log = entry
    try:
        if proc.wait() != 0:
            log.seek(0)
            raise RuntimeError(f"the early jobs of {module_name} failed:\n{log.read()}")
        with open(out, "rb") as f:
            return pickle.load(f)
    finally:
        log.close()
        for path in (out, out + ".log"):
            if os.path.exists(path):
                os.remove(path)


def pytest_sessionfinish(session):
    while _EARLY:
        proc, out, log = _EARLY.popitem()[1]
        proc.kill()
        proc.wait()
        log.close()
        for path in (out, out + ".log"):
            if os.path.exists(path):
                os.remove(path)

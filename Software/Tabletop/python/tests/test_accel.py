"""The engine seam, `tabletop.accel` (D-107, D-110): the compiled engine is the one running
by default and records its build; without a build, with a stale build, or with a misnamed
`TABLETOP_ENGINE`, every entry point refuses to start with an error that names the build
command or the variable, and nothing falls back to the pure engine; the pure engine runs on
the explicit request alone and says so. The refusals run in subprocesses on throwaway copies
of the package (the extension removed, or a `.pyx` edited after the build).
"""

import json
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from tabletop import accel
from tabletop import _native as native

PYTHON_DIR = Path(__file__).resolve().parents[1]
PACKAGE = PYTHON_DIR / "tabletop"
ARTEFACTS = (".so", ".pyd", ".c", ".html")
RUN = ("from tabletop.config import PROFILES; from tabletop.problems import load_problem; "
       "from tabletop.run import run_problem; "
       "r = run_problem(load_problem('surround-05'), PROFILES['standard'], 1); print('RAN', r.engine)")
INFO = "import json; from tabletop import accel; print(json.dumps(accel.info()))"


def compiled_running() -> bool:
    return accel.info()["engine"] == "compiled"


def needs_compiled():
    if not compiled_running():
        pytest.skip("the compiled engine is not the one running (TABLETOP_ENGINE=python was "
                    "requested explicitly)")


def run_in(cwd: Path, code: str, engine=None, extra_env=None) -> subprocess.CompletedProcess:
    """`python -c code` in `cwd`, with TABLETOP_ENGINE unset, or set to `engine` (any string,
    the empty one included)."""
    env = dict(os.environ)
    env.pop("TABLETOP_ENGINE", None)
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    if engine is not None:
        env["TABLETOP_ENGINE"] = engine
    env.update(extra_env or {})
    return subprocess.run([sys.executable, "-c", code], cwd=cwd, env=env, capture_output=True,
                          text=True, timeout=120)


def copy_package(where: Path, with_extension: bool) -> Path:
    """A copy of `tabletop/` under `where` (no caches), with or without the built extension;
    returns the folder to run in."""
    dest = where / "tabletop"
    shutil.copytree(PACKAGE, dest, ignore=shutil.ignore_patterns("__pycache__", "*.pyc"))
    if not with_extension:
        for p in (dest / "_native").iterdir():
            if p.suffix in ARTEFACTS:
                p.unlink()
    os.symlink(PYTHON_DIR / "problems", where / "problems")     # the problem files
    return where


# ---- the engine that is running ----------------------------------------------------------------


def test_the_variable_is_read_in_accel_alone():
    """Only accel.py names the variable in code (ANSWERS of the fast-engine loop: one place
    to audit); the engine's modules construct through the seam."""
    offenders = []
    for path in PACKAGE.rglob("*.py"):
        if path.name == "accel.py":
            continue
        text = path.read_text(encoding="utf-8")
        if "TABLETOP_ENGINE" in text.replace('"""', "").split("import")[0] and \
                "TABLETOP_ENGINE" in "\n".join(l for l in text.splitlines()
                                              if not l.strip().startswith("#")
                                              and "TABLETOP_ENGINE" in l and ("os.environ" in l)):
            offenders.append(path.relative_to(PACKAGE).as_posix())
    assert offenders == []


def test_info_names_the_engine_and_the_generator():
    info = accel.info()
    assert info["engine"] in ("compiled", "python") and info["rng"] == "splitmix64"
    assert info["engine"] == accel.ENGINE
    info["engine"] = "tampered"
    assert accel.info()["engine"] == accel.ENGINE          # a copy each time


def test_the_compiled_engine_records_its_build():
    needs_compiled()
    info = accel.info()
    assert info["build_stamp"] == native.stamp()
    assert info["cflags"] == " ".join(native.CFLAGS) and "-O2" in info["cflags"]
    assert "-ffp-contract=off" in info["cflags"]
    assert "march" not in info["cflags"] and "fast-math" not in info["cflags"]
    assert info["compiler"] and info["cython"] and info["modules"] == list(native.MODULES)
    assert accel.Rng.__module__ == "tabletop._native.rng"


def test_every_construction_goes_through_the_seam():
    """`World` and the batches use the seam's `Rng`, `Slipnet` and `run_seeds` (D-107)."""
    from tabletop import run, world
    from tabletop.bench import harness
    assert world.Rng is accel.Rng and world.Slipnet is accel.Slipnet
    assert run.run_seeds is accel.run_seeds and harness.run_seeds is accel.run_seeds
    for path in sorted(PACKAGE.rglob("*.py")):
        rel = path.relative_to(PACKAGE).as_posix()
        text = path.read_text(encoding="utf-8")
        if rel not in ("accel.py", "rng.py", "slipnet.py") and not rel.startswith("_native/"):
            assert "from tabletop.rng import Rng" not in text, rel
            assert "from tabletop.rng import run_seeds" not in text, rel
            assert "from tabletop.slipnet import Slipnet" not in text, rel
            assert "tabletop._native" not in text.replace('"""', ""), rel


def test_the_run_records_its_engine():
    from tabletop import events as EV
    from tabletop.config import PROFILES
    from tabletop.problems import load_problem
    from tabletop.run import run_problem, trace_lines
    log = EV.Collector()
    result = run_problem(load_problem("blockage-01"), PROFILES["standard"], 1, [log])
    assert result.engine == accel.ENGINE
    assert log.events[0].engine == accel.ENGINE
    first = trace_lines(load_problem("blockage-01"), PROFILES["standard"], 1, quiet=True)[0]
    assert f"  engine {accel.ENGINE}  " in first


def test_the_python_engine_runs_on_explicit_request_only():
    proc = run_in(PYTHON_DIR, INFO + "; print(accel.Rng.__module__)", engine="python")
    assert proc.returncode == 0, proc.stderr
    info = json.loads(proc.stdout.splitlines()[0])
    assert info == {"engine": "python", "rng": "splitmix64"}
    assert proc.stdout.splitlines()[1] == "tabletop.rng"
    proc = run_in(PYTHON_DIR, RUN, engine="python")
    assert proc.returncode == 0 and proc.stdout.strip() == "RAN python"


@pytest.mark.parametrize("value", ["pure", "auto", "1", "Python", "COMPILED", " python"])
def test_a_misnamed_engine_is_an_error_at_start(value):
    proc = run_in(PYTHON_DIR, RUN, engine=value)
    assert proc.returncode != 0 and "RAN" not in proc.stdout
    assert "TABLETOP_ENGINE" in proc.stderr and "'python'" in proc.stderr
    assert "EngineError" in proc.stderr


@pytest.mark.parametrize("value", [None, "", "compiled"])
def test_the_default_is_the_compiled_engine(value):
    needs_compiled()
    proc = run_in(PYTHON_DIR, INFO, engine=value)
    assert proc.returncode == 0, proc.stderr
    assert json.loads(proc.stdout)["engine"] == "compiled"


# ---- the refusals, on copies -------------------------------------------------------------------


@pytest.mark.parametrize("value", [None, "", "compiled"])
def test_without_a_build_every_entry_point_refuses(tmp_path, value):
    """No extension: `run_problem`, the CLI and the bench CLI fail at start naming the build
    command; nothing runs the pure engine instead."""
    where = copy_package(tmp_path, with_extension=False)
    proc = run_in(where, RUN, engine=value)
    assert proc.returncode != 0 and "RAN" not in proc.stdout
    assert "not built" in proc.stderr and "tools/build_fast.py" in proc.stderr
    for module in ("tabletop", "tabletop.bench"):
        args = ["surround-05", "--seed", "1", "--quiet"] if module == "tabletop" else \
            ["families", "--problems", "surround-05", "--runs", "1", "--quiet"]
        env = dict(os.environ)
        env.pop("TABLETOP_ENGINE", None)
        if value is not None:
            env["TABLETOP_ENGINE"] = value
        cli = subprocess.run([sys.executable, "-m", module, *args], cwd=where, env=env,
                             capture_output=True, text=True, timeout=120)
        assert cli.returncode != 0 and "Eliza" not in cli.stdout, module
        assert "tools/build_fast.py" in cli.stderr, module
    proc = run_in(where, RUN, engine="python")
    assert proc.returncode == 0 and proc.stdout.strip() == "RAN python"


def test_a_stale_build_refuses(tmp_path):
    """A `.pyx` edited after the build: the stamp no longer matches and the old extension is
    refused, naming the build command (TASK's stale-build rule, D-110)."""
    needs_compiled()
    where = copy_package(tmp_path, with_extension=True)
    proc = run_in(where, INFO)
    assert proc.returncode == 0, proc.stderr          # the copy runs the same build
    with (where / "tabletop" / "_native" / "rng.pyx").open("a") as fh:
        fh.write("\n# a test's stale-build probe\n")
    proc = run_in(where, "from tabletop import run; print('IMPORTED')")
    assert proc.returncode != 0 and "IMPORTED" not in proc.stdout
    assert "stale" in proc.stderr and "tools/build_fast.py" in proc.stderr
    proc = run_in(where, RUN, engine="python")         # the pure engine, on request, is unaffected
    assert proc.returncode == 0 and proc.stdout.strip() == "RAN python"


def test_the_stamp_covers_the_sources_and_the_flags(tmp_path, monkeypatch):
    sources = [p.name for p in native.sources()]
    assert "rng.pyx" in sources
    before = native.stamp()
    monkeypatch.setattr(native, "CFLAGS", ("-O2",))
    assert native.stamp() != before
    monkeypatch.setattr(native, "CFLAGS", ("-O2", "-ffp-contract=off"))
    assert native.stamp() == before


# ---- the build tool --------------------------------------------------------------------------


def test_build_fast_refuses_clearly_without_cython_and_a_compiler(tmp_path):
    """With no Cython and no compiler, the tool says what is missing, exits 2 and leaves no
    artefact; the copy's engine still refuses (nothing half-built)."""
    where = copy_package(tmp_path, with_extension=False)
    (where / "tools").mkdir()
    shutil.copy2(PYTHON_DIR / "tools" / "build_fast.py", where / "tools")
    shutil.copy2(PYTHON_DIR / "setup.py", where)
    env = dict(os.environ, CC="/nonexistent/cc", TABLETOP_CYTHON="/nonexistent/cython")
    env.pop("TABLETOP_ENGINE", None)
    proc = subprocess.run([sys.executable, "tools/build_fast.py", "--jobs", "9"], cwd=where,
                          env=env, capture_output=True, text=True, timeout=120)
    assert proc.returncode == 2
    out = proc.stdout + proc.stderr
    assert "Cython" in out and "compiler" in out and "pip install cython" in out
    assert "capped at 4" in out
    assert not [p for p in (where / "tabletop" / "_native").iterdir() if p.suffix in ARTEFACTS]
    proc = run_in(where, RUN)
    assert proc.returncode != 0 and "tools/build_fast.py" in proc.stderr


def test_build_fast_help_names_the_options():
    proc = subprocess.run([sys.executable, "tools/build_fast.py", "--help"], cwd=PYTHON_DIR,
                          capture_output=True, text=True, timeout=60)
    assert proc.returncode == 0 and "--jobs" in proc.stdout and "--cython" in proc.stdout

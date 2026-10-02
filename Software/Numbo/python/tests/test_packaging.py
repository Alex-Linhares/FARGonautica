"""Loop0003 item 12: packaging.

- pyproject.toml has a `gui` extra (PySide6) and two console scripts,
  `numbo` (the CLI) and `numbo-gui`, whose targets exist.
- The engine, its models and the CLI work without PySide6. "Without" is a
  stub `PySide6` package first on PYTHONPATH that raises
  ModuleNotFoundError, as a missing package does; the GUI's entry points
  then say what to install and exit 1, with no traceback, and the GUI tests
  skip cleanly.
- The package installs into a fresh virtualenv (from a copy of the sources,
  so nothing is written into python/), with the extra in its metadata, and
  the two installed commands run.
"""

import importlib
import os
import pathlib
import pkgutil
import shutil
import subprocess
import sys
import tomllib

import pytest

from full_runs import PYTHON_DIR

PYPROJECT = PYTHON_DIR / "pyproject.toml"
SOLVED = ["outcome: solved, 45 iterations (seed 1)",
          "check: valid: 114 = (6 x 20) - (7 - 1)"]
NEEDS_PYSIDE = "the GUI needs PySide6 (pip install -e 'python[gui]')"


def project():
    return tomllib.loads(PYPROJECT.read_text())["project"]


def engine_modules():
    """Every numbo module but the GUI's."""
    import numbo
    import numbo.models
    names = ["numbo"]
    for package in (numbo, numbo.models):
        for info in pkgutil.iter_modules(package.__path__, package.__name__ + "."):
            if not info.name.startswith("numbo.gui"):
                names.append(info.name)
    return names


@pytest.fixture(scope="module")
def no_pyside(tmp_path_factory):
    """An environment in which `import PySide6` fails as if it weren't
    installed (and so does every PySide6.* module)."""
    stub = tmp_path_factory.mktemp("no-pyside") / "PySide6"
    stub.mkdir()
    (stub / "__init__.py").write_text(
        "raise ModuleNotFoundError(\"No module named 'PySide6'\", name='PySide6')\n")
    env = dict(os.environ, PYTHONPATH=os.pathsep.join([str(stub.parent), str(PYTHON_DIR)]))
    env.pop("QT_QPA_PLATFORM", None)
    return env


def run(args, env, cwd=PYTHON_DIR, timeout=120):
    return subprocess.run(args, cwd=cwd, env=env, capture_output=True, text=True,
                          timeout=timeout)


# -- pyproject.toml ----------------------------------------------------------------

def test_the_gui_extra_is_pyside6():
    extras = project()["optional-dependencies"]
    assert [r for r in extras["gui"] if r.startswith("PySide6")], extras
    assert not project().get("dependencies"), "the engine needs only the standard library"


def test_the_test_extra_has_pytest_and_pytest_qt():
    test = project()["optional-dependencies"]["test"]
    assert any(r.startswith("pytest>=") for r in test)
    assert any(r.startswith("pytest-qt") for r in test)


def test_the_console_scripts():
    scripts = project()["scripts"]
    assert scripts == {"numbo": "numbo.__main__:main", "numbo-gui": "numbo.gui.app:run"}
    for target in scripts.values():
        module, attr = target.split(":")
        assert callable(getattr(importlib.import_module(module), attr))


def test_every_package_is_listed():
    listed = tomllib.loads(PYPROJECT.read_text())["tool"]["setuptools"]["packages"]
    found = sorted(str(p.parent.relative_to(PYTHON_DIR)).replace(os.sep, ".")
                   for p in (PYTHON_DIR / "numbo").rglob("__init__.py"))
    assert sorted(listed) == found


# -- without PySide6 ----------------------------------------------------------------

def test_the_stub_hides_pyside6(no_pyside):
    p = run([sys.executable, "-c", "import PySide6.QtCore"], no_pyside)
    assert p.returncode == 1 and "ModuleNotFoundError: No module named 'PySide6'" in p.stderr


def test_the_engine_does_not_import_pyside6():
    code = ("import importlib, sys\n"
            f"for name in {engine_modules()!r}: importlib.import_module(name)\n"
            "print(sorted(m for m in sys.modules if m.split('.')[0] in "
            "('PySide6', 'shiboken6', 'numbo') and m.startswith(('PySide6', 'shiboken6', "
            "'numbo.gui'))))\n")
    p = run([sys.executable, "-c", code], dict(os.environ, PYTHONPATH=str(PYTHON_DIR)))
    assert p.returncode == 0, p.stderr
    assert p.stdout.strip() == "[]"


def test_every_engine_module_imports_without_pyside6(no_pyside):
    names = engine_modules()
    assert {"numbo.harness", "numbo.session", "numbo.models.run_controller",
            "numbo.models.tree_layout", "numbo.__main__"} <= set(names)
    code = f"import importlib\nfor name in {names!r}: importlib.import_module(name)\n"
    p = run([sys.executable, "-c", code], no_pyside)
    assert p.returncode == 0, p.stderr


def test_the_cli_without_pyside6(no_pyside):
    p = run([sys.executable, "-m", "numbo", "114", "11", "20", "7", "1", "6", "--seed", "1"],
            no_pyside)
    assert p.returncode == 0, p.stderr
    assert p.stdout.splitlines()[-2:] == SOLVED
    assert p.stderr == ""


def test_a_run_with_every_model_and_a_session_without_pyside6(no_pyside, tmp_path):
    code = f"""
import io
from numbo import harness, session
from numbo.models import (coderack_model, event_log, pnet_model, run_controller,
                          run_stats, tree_model)
models = [tree_model.TreeModel(), pnet_model.PnetModel(), coderack_model.CoderackModel(),
          run_stats.RunStats(), event_log.EventLog()]
path = {str(tmp_path / "s.jsonl")!r}
recorder = session.SessionRecorder(path)
out = io.StringIO()
result = harness.run_config([114, 11, 20, 7, 1, 6], seed=1, max_iterations=20000,
                            out=out, observers=[recorder, *models])
recorder.close()
loaded = session.load_session(path)
tree = tree_model.TreeModel()
session.replay(loaded.events, [tree])
print(result["outcome"], result["iterations"], len(loaded.events), loaded.ended,
      tree.equation(tree.solution_root))
c = run_controller.RunController([])
c.start_run([114, 11, 20, 7, 1, 6], 1, 20000)
c.run_to_end()
assert c.wait(30)
print(c.state, c.outcome.result["outcome"], c.outcome.events)
"""
    p = run([sys.executable, "-c", code], no_pyside)
    assert p.returncode == 0, p.stderr
    assert p.stdout.splitlines() == ["solved 45 286 True 114 = (6 x 20) - (7 - 1)",
                                     "finished solved 286"]


@pytest.mark.parametrize("how", ["module", "entry point"])
def test_the_gui_entry_points_say_what_to_install(no_pyside, how):
    args = ([sys.executable, "-m", "numbo.gui", "--smoke"] if how == "module" else
            [sys.executable, "-c", "import sys; from numbo.gui.app import run; "
                                   "sys.exit(run(['--smoke']))"])
    p = run(args, no_pyside)
    assert p.returncode == 1
    assert p.stdout == ""
    assert p.stderr.strip().endswith(NEEDS_PYSIDE), p.stderr
    assert "Traceback" not in p.stderr


def test_the_gui_tests_skip_cleanly_without_pyside6(no_pyside):
    """With pytest-qt installed but no Qt binding, and with neither."""
    for extra in ([], ["-p", "no:pytestqt"]):
        p = run([sys.executable, "-m", "pytest", "tests/gui", "tests/test_run_stats.py",
                 "-q", "-rs", *extra], no_pyside)
        assert p.returncode == 0, p.stdout + p.stderr
        gui_files = sorted(f.name for f in (PYTHON_DIR / "tests" / "gui").glob("test_*.py"))
        assert len(gui_files) >= 5
        for name in gui_files:
            assert f"SKIPPED [1] tests/gui/{name}" in p.stdout, (name, p.stdout)
            assert f"could not import 'PySide6'" in p.stdout
        assert f"{len(gui_files)} skipped" in p.stdout
        assert "error" not in p.stdout.lower() and "failed" not in p.stdout.lower()


# -- with PySide6 --------------------------------------------------------------------

def test_the_gui_entry_point_runs_the_smoke_command():
    pytest.importorskip("PySide6")
    p = run([sys.executable, "-c", "import sys; from numbo.gui.app import run; "
                                   "sys.exit(run(['--smoke', '--puzzle', '1', '--seed', '1']))"],
            dict(os.environ, PYTHONPATH=str(PYTHON_DIR)))
    assert p.returncode == 0, p.stderr
    assert p.stdout.splitlines() == SOLVED
    # The offscreen plugin's "does not support propagateSizeHints()" warning
    # is not shown: the smoke command prints only the CLI's lines.
    assert p.stderr == ""


def test_the_smoke_command_from_a_bare_environment():
    """Only PATH, HOME and LANG, as test_readme.py runs the README's blocks.
    A qt.conf next to the Python executable (Anaconda ships one for its own
    Qt 5) must not send Qt 6 to another Qt's plugins."""
    pytest.importorskip("PySide6")
    env = {k: os.environ[k] for k in ("PATH", "HOME", "LANG") if k in os.environ}
    p = run([sys.executable, "-m", "numbo.gui", "--smoke", "--puzzle", "1", "--seed", "1"],
            env)
    assert p.returncode == 0, p.stderr
    assert p.stdout.splitlines() == SOLVED
    assert p.stderr == ""


def test_a_plugin_path_given_by_the_user_is_kept():
    pytest.importorskip("PySide6")
    code = ("import os, sys\n"
            "from numbo.gui import app\n"
            "app.use_pyside_plugins()\n"
            "print(os.environ['QT_PLUGIN_PATH'])\n")
    env = dict(os.environ, PYTHONPATH=str(PYTHON_DIR), QT_PLUGIN_PATH="/somewhere")
    p = run([sys.executable, "-c", code], env)
    assert p.returncode == 0, p.stderr
    assert p.stdout.strip() == "/somewhere"
    env.pop("QT_PLUGIN_PATH")
    p = run([sys.executable, "-c", code], env)
    import PySide6
    assert p.stdout.strip() == str(pathlib.Path(PySide6.__file__).parent / "Qt" / "plugins")


# -- installed -----------------------------------------------------------------------

@pytest.fixture(scope="module")
def venv(tmp_path_factory):
    """A fresh virtualenv (seeing the system's packages, for setuptools and
    PySide6) with the package installed from a copy of python/: its
    pyproject.toml, README.md and numbo/, no network, no build isolation."""
    root = tmp_path_factory.mktemp("install")
    src = root / "src"
    src.mkdir()
    shutil.copy(PYPROJECT, src)
    shutil.copy(PYTHON_DIR / "README.md", src)
    shutil.copytree(PYTHON_DIR / "numbo", src / "numbo",
                    ignore=shutil.ignore_patterns("__pycache__"))
    env_dir = root / "venv"
    subprocess.run([sys.executable, "-m", "venv", "--system-site-packages", str(env_dir)],
                   check=True, capture_output=True, timeout=120)
    bin_dir = env_dir / ("Scripts" if os.name == "nt" else "bin")
    p = subprocess.run([str(bin_dir / "python"), "-m", "pip", "install", "--no-deps",
                        "--no-build-isolation", "--no-index", "--quiet", str(src)],
                       cwd=root, capture_output=True, text=True, timeout=300)
    if p.returncode != 0:
        pytest.fail(f"pip install failed:\n{p.stdout}\n{p.stderr}")
    return root, bin_dir


def installed_env(no_pyside=None):
    env = dict(no_pyside or os.environ)
    if no_pyside is None:
        env.pop("PYTHONPATH", None)
    else:
        env["PYTHONPATH"] = env["PYTHONPATH"].split(os.pathsep)[0]   # the stub only
    return env


def test_the_installed_metadata(venv):
    root, bin_dir = venv
    code = ("import importlib.metadata as m, numbo\n"
            "d = m.distribution('numbo')\n"
            "print(numbo.__file__)\n"
            "print(sorted(d.metadata.get_all('Provides-Extra')))\n"
            "print([r for r in d.requires if 'PySide6' in r])\n"
            "print(sorted((e.name, e.value) for e in d.entry_points "
            "if e.group == 'console_scripts'))\n")
    p = run([str(bin_dir / "python"), "-c", code], installed_env(), cwd=root)
    assert p.returncode == 0, p.stderr
    lines = p.stdout.splitlines()
    assert str(root / "venv") in lines[0], "the installed copy, not python/"
    assert lines[1] == "['gui', 'test']"
    assert "extra == \"gui\"" in lines[2]
    assert lines[3] == ("[('numbo', 'numbo.__main__:main'), "
                        "('numbo-gui', 'numbo.gui.app:run')]")


def test_the_installed_cli(venv, no_pyside):
    root, bin_dir = venv
    for env in (installed_env(), installed_env(no_pyside)):
        p = run([str(bin_dir / "numbo"), "114", "11", "20", "7", "1", "6", "--quiet"],
                env, cwd=root)
        assert p.returncode == 0, p.stderr
        assert p.stdout.splitlines() == SOLVED


def test_the_installed_gui_command(venv, no_pyside):
    root, bin_dir = venv
    p = run([str(bin_dir / "numbo-gui"), "--smoke"], installed_env(no_pyside), cwd=root)
    assert p.returncode == 1 and p.stderr.strip().endswith(NEEDS_PYSIDE), p.stderr
    assert "Traceback" not in p.stderr
    pytest.importorskip("PySide6")
    p = run([str(bin_dir / "numbo-gui"), "--smoke", "--puzzle", "1", "--seed", "1"],
            installed_env(), cwd=root)
    assert p.returncode == 0, p.stderr
    assert p.stdout.splitlines() == SOLVED

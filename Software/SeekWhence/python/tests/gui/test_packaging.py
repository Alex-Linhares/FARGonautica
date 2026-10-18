"""Loop0004 item 00: packaging, after the Numbo GUI's `tests/test_packaging.py`.

- pyproject.toml has the `gui` extra (PySide6) and the `test` extra (pytest, pytest-qt),
  and two console scripts, `seekwhence` (the CLI) and `seekwhence-gui`, whose targets
  exist; every package is listed.
- The engine, the CLI, the Qt-free models and `seekwhence.gui.headless` work without
  PySide6. "Without" is a stub `PySide6` package first on PYTHONPATH that raises
  ModuleNotFoundError, as a missing package does; the GUI's entry points then say what to
  install and exit 1, with no traceback.
- With PySide6, `python3 -m seekwhence.gui --smoke` runs run 1 offscreen, prints the CLI's
  summary line and exits 0 (item 16).

Qt-free itself: it runs in the engine's suite without PySide6 (the smoke test skips).
"""

import importlib
import os
import pathlib
import subprocess
import sys
import tomllib

import pytest

PYTHON_DIR = pathlib.Path(__file__).resolve().parents[2]
PYPROJECT = PYTHON_DIR / "pyproject.toml"
NEEDS_PYSIDE = "the GUI needs PySide6 (pip install -e '.[gui]')"
RUN1 = ["--seed", "4", "--terms", "3", "7", "3", "7", "--answers", "ok"]


def project():
    return tomllib.loads(PYPROJECT.read_text())["project"]


def engine_modules():
    """Every seekwhence module but the GUI's views: the engine, and the GUI's Qt-free
    `models` and `headless`."""
    package = PYTHON_DIR / "seekwhence"
    names = []
    for path in sorted(package.rglob("*.py")):
        rel = path.relative_to(package)
        if rel.parts[0] == "gui" and rel.parts[1:] not in (("models", "__init__.py"),
                                                           ("headless.py",)):
            continue
        name = "seekwhence." + ".".join(rel.with_suffix("").parts)
        names.append(name.removesuffix(".__init__"))
    return names


@pytest.fixture(scope="module")
def no_pyside(tmp_path_factory):
    """An environment in which `import PySide6` fails as if it weren't installed (and so
    does every PySide6.* module)."""
    stub = tmp_path_factory.mktemp("no-pyside") / "PySide6"
    stub.mkdir()
    (stub / "__init__.py").write_text(
        "raise ModuleNotFoundError(\"No module named 'PySide6'\", name='PySide6')\n")
    env = dict(os.environ, PYTHONPATH=os.pathsep.join([str(stub.parent), str(PYTHON_DIR)]),
               PYTHONDONTWRITEBYTECODE="1")
    env.pop("QT_QPA_PLATFORM", None)
    return env


def with_pyside():
    env = dict(os.environ, PYTHONPATH=str(PYTHON_DIR), PYTHONDONTWRITEBYTECODE="1",
               QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    return env


def run(args, env, cwd=PYTHON_DIR, timeout=120):
    return subprocess.run(args, cwd=cwd, env=env, capture_output=True, text=True,
                          timeout=timeout)


# -- pyproject.toml ----------------------------------------------------------------

def test_the_gui_extra_is_pyside6():
    extras = project()["optional-dependencies"]
    assert extras["gui"] == ["PySide6>=6.6"]
    assert not project().get("dependencies"), "the engine needs only the standard library"


def test_the_test_extra_has_pytest_and_pytest_qt():
    assert project()["optional-dependencies"]["test"] == ["pytest>=7.4", "pytest-qt>=4.5",
                                                      "pytest-xdist>=3.0"]


def test_the_console_scripts():
    scripts = project()["scripts"]
    assert scripts == {"seekwhence": "seekwhence.__main__:main",
                       "seekwhence-gui": "seekwhence.gui.app:run"}
    for target in scripts.values():
        module, attr = target.split(":")
        assert callable(getattr(importlib.import_module(module), attr))


def test_every_package_is_listed():
    listed = tomllib.loads(PYPROJECT.read_text())["tool"]["setuptools"]["packages"]
    found = sorted(str(p.parent.relative_to(PYTHON_DIR)).replace(os.sep, ".")
                   for p in (PYTHON_DIR / "seekwhence").rglob("__init__.py"))
    assert sorted(listed) == found
    assert {"seekwhence.gui", "seekwhence.gui.models"} <= set(listed)


# -- without PySide6 ----------------------------------------------------------------

def test_the_stub_hides_pyside6(no_pyside):
    p = run([sys.executable, "-c", "import PySide6.QtCore"], no_pyside)
    assert p.returncode == 1 and "ModuleNotFoundError: No module named 'PySide6'" in p.stderr


def test_the_engine_does_not_import_pyside6_or_the_gui():
    """Importing every engine module (and the GUI's Qt-free ones) loads neither PySide6
    nor a GUI view: of `seekwhence.gui` only the package, `headless` and the Qt-free
    `models` may be loaded."""
    code = ("import importlib, sys\n"
            f"for name in {engine_modules()!r}: importlib.import_module(name)\n"
            "print(sorted(m for m in sys.modules if m.startswith(('PySide6', 'shiboken6'))"
            " or (m.startswith('seekwhence.gui') and m not in ('seekwhence.gui', "
            "'seekwhence.gui.headless') and not m.startswith('seekwhence.gui.models'))))\n")
    p = run([sys.executable, "-c", code], with_pyside())
    assert p.returncode == 0, p.stderr
    assert p.stdout.strip() == "[]"


def test_every_engine_module_imports_without_pyside6(no_pyside):
    names = engine_modules()
    assert {"seekwhence", "seekwhence.session", "seekwhence.__main__", "seekwhence.gui.models",
            "seekwhence.gui.headless"} <= set(names)
    code = f"import importlib\nfor name in {names!r}: importlib.import_module(name)\n"
    p = run([sys.executable, "-c", code], no_pyside)
    assert p.returncode == 0, p.stderr


def test_the_cli_without_pyside6(no_pyside):
    """Run 1's terms on the fence seed, as the engine's `test_package.py` runs them
    [M86 p. 166]."""
    p = run([sys.executable, "-m", "seekwhence", *RUN1], no_pyside)
    assert p.returncode == 0, p.stderr
    lines = p.stdout.splitlines()
    assert lines[-2] == "bye" and lines[-1].endswith("(seed 4): (Cycle 3 2 (3 7))")
    assert p.stderr == ""


def test_headless_without_pyside6_equals_a_plain_session(no_pyside):
    """`headless.run(window=False)` is Qt-free and gives the plain session's records,
    draws and outcome for run 1 [M86 pp. 164-166]."""
    code = """
import io, json, sys
from seekwhence.config import Config
from seekwhence.events import Recorder
from seekwhence.session import Session
from seekwhence.trace import event_record
from seekwhence.gui import headless
lines, answers = [3, 7, 3, 7], ["ok"]
rec = Recorder()
s = Session(Config(), 4, out=io.StringIO(), lines=lines, answers=answers, observers=[rec])
s.run()
got = headless.run(Config(), 4, lines, answers, window=False)
assert sorted(got) == ["draws", "outcome", "records"], got.keys()
assert got["records"] == [event_record(e) for e in rec.events]
assert got["draws"] == s.world.rng.draws and got["outcome"] == s.outcome
assert "PySide6" not in sys.modules
print(len(got["records"]), got["draws"], got["outcome"])
"""
    p = run([sys.executable, "-c", code], no_pyside)
    assert p.returncode == 0, p.stderr
    count, draws, outcome = p.stdout.split()
    assert outcome == "ok" and int(count) > 10 and int(draws) > 0


@pytest.mark.parametrize("how", ["module", "entry point"])
def test_the_gui_entry_points_say_what_to_install(no_pyside, how):
    args = ([sys.executable, "-m", "seekwhence.gui", "--smoke"] if how == "module" else
            [sys.executable, "-c", "import sys; from seekwhence.gui.app import run; "
                                   "sys.exit(run(['--smoke']))"])
    p = run(args, no_pyside)
    assert p.returncode == 1
    assert p.stdout == ""
    assert p.stderr.strip().endswith(NEEDS_PYSIDE), p.stderr
    assert "Traceback" not in p.stderr


def test_the_gui_tests_skip_cleanly_without_pyside6(no_pyside):
    """With pytest-qt installed but no Qt binding, and with neither: the Qt-free tests of
    tests/gui/ pass and the Qt ones (if any yet) skip. Under recent pytest (9.1), pytest-qt
    itself refuses to start without a Qt binding (a usage error before any test is
    collected); that refusal is pytest-qt's, so the variant is accepted as it is."""
    for extra in ([], ["-p", "no:pytestqt"]):
        p = run([sys.executable, "-m", "pytest", "tests/gui", "-q", "-rs", "-p",
                 "no:cacheprovider", "-k", "not test_the_gui_tests_skip_cleanly", *extra],
                no_pyside, timeout=600)
        if not extra and p.returncode == 4 and "pytest-qt requires either" in p.stdout + p.stderr:
            continue
        assert p.returncode == 0, p.stdout + p.stderr
        assert "error" not in p.stdout.lower() and "failed" not in p.stdout.lower()


# -- with PySide6 --------------------------------------------------------------------

def test_help_works():
    p = run([sys.executable, "-m", "seekwhence.gui", "--help"], with_pyside())
    assert p.returncode == 0, p.stderr
    assert "--smoke" in p.stdout


def test_the_smoke_command_opens_the_window_offscreen_and_exits_0():
    pytest.importorskip("PySide6")
    p = run([sys.executable, "-m", "seekwhence.gui", "--smoke"], with_pyside())
    assert p.returncode == 0, p.stderr
    # Item 16: the smoke run runs the default preset (run 1, seed 54) to the end and
    # prints the CLI's summary line (G-055).
    assert p.stdout == "outcome: ok after 4 terms, 52 tasks (seed 54): (Cycle 3 2 (3 7))\n"
    # The offscreen plugin's "does not support propagateSizeHints()" warning is not
    # shown: a smoke run prints only its own lines.
    assert p.stderr == ""


def test_the_smoke_command_from_a_bare_environment():
    """Only PATH, HOME and LANG (Numbo GUI, `test_packaging.py`): a qt.conf next to the
    Python executable must not send Qt 6 to another Qt's plugins."""
    pytest.importorskip("PySide6")
    env = {k: os.environ[k] for k in ("PATH", "HOME", "LANG") if k in os.environ}
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    p = run([sys.executable, "-m", "seekwhence.gui", "--smoke"], env)
    assert p.returncode == 0, p.stderr
    assert p.stdout.startswith("outcome: ok after 4 terms") and p.stderr == ""


def test_a_plugin_path_given_by_the_user_is_kept():
    pytest.importorskip("PySide6")
    code = ("import os, sys\n"
            "from seekwhence.gui import app\n"
            "app.use_pyside_plugins()\n"
            "print(os.environ['QT_PLUGIN_PATH'])\n")
    env = dict(with_pyside(), QT_PLUGIN_PATH="/somewhere")
    p = run([sys.executable, "-c", code], env)
    assert p.returncode == 0, p.stderr
    assert p.stdout.strip() == "/somewhere"
    env.pop("QT_PLUGIN_PATH")
    p = run([sys.executable, "-c", code], env)
    import PySide6
    assert p.stdout.strip() == str(pathlib.Path(PySide6.__file__).parent / "Qt" / "plugins")

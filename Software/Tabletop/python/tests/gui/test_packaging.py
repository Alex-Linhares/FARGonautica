"""Loop0006 item 00: packaging, after the Numbo GUI's `tests/test_packaging.py` through the
Seek-Whence GUI's (G-004, G-005).

- pyproject.toml has the `gui` extra (PySide6 and pytest-qt, G-004), the `test` extra
  (pytest and pytest-xdist, no pytest-qt), the `docs` extra, no dependencies, and two
  console scripts, `tabletop` (the CLI) and `tabletop-gui`, whose targets exist; the GUI's
  packages are listed, with `default_layout.json` as package data.
- The engine, the CLI, the Qt-free models and (from item 01) `tabletop.gui.headless` work
  without PySide6. "Without" is a stub `PySide6` package first on PYTHONPATH that raises
  ModuleNotFoundError, as a missing package does; the GUI's entry points then say what to
  install and exit 1, with no traceback, and the GUI's tests collect without error.
- With PySide6, `python3 -m tabletop.gui --help` works and `--smoke` opens the window
  offscreen and exits 0 (item 16 makes it run a problem and print French's report line).
- The engine (G-063; D-107): in a copy of the package without the built extension the GUI
  refuses to start, with the seam's message on stderr under `--smoke` and in a dialog
  where Qt is up (exit status 1, no traceback); with the pure engine asked for explicitly
  it runs, and the window's status bar says so.

Qt-free itself: it runs in the engine's suite without PySide6 (the Qt tests skip).
"""

import importlib
import os
import pathlib
import shutil
import subprocess
import sys
import tomllib

import pytest

PYTHON_DIR = pathlib.Path(__file__).resolve().parents[2]
PYPROJECT = PYTHON_DIR / "pyproject.toml"
GUI = PYTHON_DIR / "tabletop" / "gui"
NEEDS_PYSIDE = "the GUI needs PySide6: pip install -e '.[gui]'"


def project():
    return tomllib.loads(PYPROJECT.read_text())["project"]


def engine_modules():
    """Every tabletop module but the GUI's views: the engine, and the GUI's Qt-free
    `models` package and `headless` (once item 01 has written it)."""
    package = PYTHON_DIR / "tabletop"
    names = []
    for path in sorted(package.rglob("*.py")):
        rel = path.relative_to(package)
        if "__pycache__" in rel.parts:
            continue
        if rel.parts[0] == "gui" and rel.parts[1:] not in (("models", "__init__.py"),
                                                           ("headless.py",)):
            continue
        name = "tabletop." + ".".join(rel.with_suffix("").parts)
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

def test_the_gui_extra_is_pyside6_and_pytest_qt():
    """pytest-qt goes with the GUI, not with `test` (G-004; ANSWERS.md 1)."""
    extras = project()["optional-dependencies"]
    assert extras["gui"] == ["PySide6>=6.6", "pytest-qt>=4.5"]
    assert not project().get("dependencies"), "the engine needs only the standard library"


def test_the_test_extra_has_pytest_and_xdist_but_not_pytest_qt():
    test = project()["optional-dependencies"]["test"]
    assert test == ["pytest>=7.4", "pytest-xdist>=3.0"]
    assert project()["optional-dependencies"]["docs"] == ["PySide6>=6.6"]


def test_the_console_scripts():
    scripts = project()["scripts"]
    assert scripts == {"tabletop": "tabletop.__main__:main",
                       "tabletop-gui": "tabletop.gui.app:run"}
    for target in scripts.values():
        module, attr = target.split(":")
        assert callable(getattr(importlib.import_module(module), attr))


def test_the_gui_packages_and_the_layout_file_are_listed():
    tool = tomllib.loads(PYPROJECT.read_text())["tool"]["setuptools"]
    assert {"tabletop", "tabletop.gui", "tabletop.gui.models"} <= set(tool["packages"])
    assert tool["package-data"]["tabletop.gui"] == ["default_layout.json"]
    assert (GUI / "models" / "__init__.py").exists()


def test_the_slow_marker_is_left_out_by_default():
    options = tomllib.loads(PYPROJECT.read_text())["tool"]["pytest"]["ini_options"]
    assert "not slow" in options["addopts"]


# -- without PySide6 ----------------------------------------------------------------

def test_the_stub_hides_pyside6(no_pyside):
    p = run([sys.executable, "-c", "import PySide6.QtCore"], no_pyside)
    assert p.returncode == 1 and "ModuleNotFoundError: No module named 'PySide6'" in p.stderr


def test_the_engine_does_not_import_pyside6_or_the_gui():
    """Importing every engine module (and the GUI's Qt-free ones) loads neither PySide6
    nor a GUI view: of `tabletop.gui` only the package, `headless` and the Qt-free
    `models` may be loaded."""
    code = ("import importlib, sys\n"
            f"for name in {engine_modules()!r}: importlib.import_module(name)\n"
            "print(sorted(m for m in sys.modules if m.startswith(('PySide6', 'shiboken6'))"
            " or (m.startswith('tabletop.gui') and m not in ('tabletop.gui', "
            "'tabletop.gui.headless') and not m.startswith('tabletop.gui.models'))))\n")
    p = run([sys.executable, "-c", code], with_pyside())
    assert p.returncode == 0, p.stderr
    assert p.stdout.strip() == "[]"


def test_every_engine_module_imports_without_pyside6(no_pyside):
    names = engine_modules()
    expected = {"tabletop", "tabletop.world", "tabletop.run", "tabletop.__main__",
                "tabletop.gui.models"}
    if (GUI / "headless.py").exists():                 # item 01 writes it
        expected.add("tabletop.gui.headless")
    assert expected <= set(names)
    code = f"import importlib\nfor name in {names!r}: importlib.import_module(name)\n"
    p = run([sys.executable, "-c", code], no_pyside)
    assert p.returncode == 0, p.stderr


def test_the_cli_without_pyside6(no_pyside):
    """The front page's quick start: `surround-05 --seed 3 --quiet` ends with Eliza touching
    cup2 (E2) after 384 codelets since v5's fix (D-116 to D-120; after 305 in v4, D-115;
    small-glass1 (E1) after 144 in v3, D-114; cup2 (E2) after 44 since
    item 08, small-glass1 (E1) after 77 at item 07; cup2 (E2) after 67 at item 05)
    [F95 p. 145]."""
    p = run([sys.executable, "-m", "tabletop", "surround-05", "--seed", "3", "--quiet"],
            no_pyside)
    assert p.returncode == 0, p.stderr
    last = p.stdout.splitlines()[-1]
    assert last.startswith("Eliza touches cup2 (E2).") and "Codelets 384." in last, last
    assert p.stderr == ""


@pytest.mark.parametrize("how", ["module", "entry point"])
def test_the_gui_entry_points_say_what_to_install(no_pyside, how):
    args = ([sys.executable, "-m", "tabletop.gui", "--smoke"] if how == "module" else
            [sys.executable, "-c", "import sys; from tabletop.gui.app import run; "
                                   "sys.exit(run(['--smoke']))"])
    p = run(args, no_pyside)
    assert p.returncode == 1
    assert p.stdout == ""
    assert p.stderr.strip().endswith(NEEDS_PYSIDE), p.stderr
    assert "Traceback" not in p.stderr


def test_the_gui_tests_collect_cleanly_without_pyside6(no_pyside):
    """Without a Qt binding (pytest-qt switched off, as on a machine without it) the Qt
    test files of tests/gui/ skip at collection and the Qt-free ones collect; no error.
    Collection only: the loop's gate runs the whole suite in a standalone copy with
    PySide6 unimportable (G-005)."""
    p = run([sys.executable, "-m", "pytest", "tests/gui", "-q", "--co", "-p", "no:pytest-qt",
             "-p", "no:cacheprovider"], no_pyside, timeout=300)
    assert p.returncode == 0, p.stdout + p.stderr
    assert "ERROR" not in p.stdout and " error" not in p.stdout.splitlines()[-1], p.stdout


# -- with PySide6 --------------------------------------------------------------------

def test_help_works():
    p = run([sys.executable, "-m", "tabletop.gui", "--help"], with_pyside())
    pytest.importorskip("PySide6")
    assert p.returncode == 0, p.stderr
    assert "--smoke" in p.stdout and "--problem" in p.stdout and "--seed" in p.stdout


def test_a_bad_argument_is_a_usage_error():
    pytest.importorskip("PySide6")
    p = run([sys.executable, "-m", "tabletop.gui", "--profile", "nowhere"], with_pyside())
    assert p.returncode == 2 and "usage" in p.stderr and "Traceback" not in p.stderr


def cli_last_line(problem, seed, *extra):
    """The last line of `python3 -m tabletop PROBLEM --seed N --quiet` (G-054)."""
    p = run([sys.executable, "-m", "tabletop", problem, "--seed", str(seed), "--quiet", *extra],
            with_pyside())
    assert p.returncode in (0, 1), p.stderr
    return p.stdout.rstrip("\n").splitlines()[-1]


def bare_environment():
    """Only PATH, HOME and LANG (Numbo GUI, `test_packaging.py`): a qt.conf next to the
    Python executable must not send Qt 6 to another Qt's plugins. The engine request is
    passed through when the suite itself runs on the explicitly requested python engine
    (D-107; `tests/test_readme.py`'s `fresh_env`)."""
    env = {k: os.environ[k] for k in ("PATH", "HOME", "LANG", "TABLETOP_ENGINE")
           if k in os.environ}
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    return env


def test_a_capped_smoke_run_from_a_bare_environment():
    """The window opens offscreen from the bare environment and runs the default problem,
    capped at five codelets (the whole run with the defaults is the slow suite's
    `test_the_smoke_command_with_the_defaults_from_a_bare_environment`); G-044, G-054."""
    pytest.importorskip("PySide6")
    p = run([sys.executable, "-m", "tabletop.gui", "--smoke", "--set", "max_codelets=5"],
            bare_environment())
    assert p.returncode == 1, p.stderr
    assert "(capped)" in p.stdout and p.stderr == ""
    assert p.stdout.rstrip("\n") == cli_last_line("surround-05", 1, "--set", "max_codelets=5")


def test_a_capped_smoke_run_exits_1_and_set_edits_the_configuration():
    """`--set max_codelets=5` through the CLI's own parser (G-054): the run is capped,
    the line says so, and the status is 1."""
    pytest.importorskip("PySide6")
    args = ["--problem", "surround-05", "--seed", "3", "--set", "max_codelets=5"]
    p = run([sys.executable, "-m", "tabletop.gui", "--smoke", *args], with_pyside())
    assert p.returncode == 1, p.stderr
    assert "(capped)" in p.stdout and p.stderr == ""
    assert p.stdout.rstrip("\n") == cli_last_line("surround-05", 3, "--set", "max_codelets=5")


@pytest.mark.parametrize("args", [["--set", "nope=1"], ["--set", "type_weight=abc"],
                                  ["--problem", "nowhere"], ["--seed", "x"]])
def test_an_unknown_problem_or_a_bad_set_is_a_usage_error(args):
    pytest.importorskip("PySide6")
    p = run([sys.executable, "-m", "tabletop.gui", "--smoke", *args], with_pyside())
    assert p.returncode == 2 and "usage" in p.stderr and "Traceback" not in p.stderr, p.stderr
    assert p.stdout == ""


def test_a_plugin_path_given_by_the_user_is_kept():
    pytest.importorskip("PySide6")
    code = ("import os, sys\n"
            "from tabletop.gui import app\n"
            "app.use_pyside_plugins()\n"
            "print(os.environ['QT_PLUGIN_PATH'])\n")
    env = dict(with_pyside(), QT_PLUGIN_PATH="/somewhere")
    p = run([sys.executable, "-c", code], env)
    assert p.returncode == 0, p.stderr
    assert p.stdout.strip() == "/somewhere"


# -- the engine (G-063; D-107) -------------------------------------------------------

ARTEFACTS = (".so", ".pyd", ".c", ".html")
# What the window reads beside the package: the problems, the decisions' titles
# (`models/config_table.py`), the user guide (`models/guide.py`).
BESIDE = ("problems", "docs", "DECISIONS.md", "GUI-DECISIONS.md")


def package_without_the_extension(where: pathlib.Path) -> pathlib.Path:
    """A copy of `tabletop/` with no built extension, as a checkout before any build (the
    artefacts left out, no caches), with links to what the window reads beside it; returns
    the folder to run in (`tests/test_accel.py`'s `copy_package`, for the GUI)."""
    shutil.copytree(GUI.parent, where / "tabletop",
                    ignore=shutil.ignore_patterns("__pycache__", "*.pyc",
                                                  *("*" + s for s in ARTEFACTS)))
    for name in BESIDE:
        os.symlink(PYTHON_DIR / name, where / name)
    return where


def engine_env(where: pathlib.Path, engine=None) -> dict:
    """The offscreen environment of a run in the copy `where`: TABLETOP_ENGINE unset, or
    set to `engine`."""
    env = with_pyside()
    env["PYTHONPATH"] = str(where)
    env.pop("TABLETOP_ENGINE", None)
    if engine is not None:
        env["TABLETOP_ENGINE"] = engine
    return env


NOTE_SCRIPT = ("from PySide6.QtWidgets import QApplication\n"
               "app = QApplication([])\n"
               "from tabletop.gui.main_window import MainWindow\n"
               "w = MainWindow(settings=None, layout_path=None)\n"
               "print(w.engine_label.text()); print(w.engine_label.toolTip()); w.close()\n")


@pytest.mark.parametrize("value", [None, "compiled"])
def test_without_a_build_the_smoke_run_refuses_on_stderr(tmp_path, value):
    """No extension in the copy: `--smoke` prints the seam's message (the build command,
    the explicit request) on stderr, prefixed as the usage errors are, exits 1, runs
    nothing and shows no traceback; no dialog under `--smoke` (G-063; D-107)."""
    pytest.importorskip("PySide6")
    where = package_without_the_extension(tmp_path)
    p = run([sys.executable, "-m", "tabletop.gui", "--smoke", "--set", "max_codelets=5"],
            engine_env(where, value), cwd=where)
    assert p.returncode == 1, p.stderr
    assert p.stdout == ""
    lines = p.stderr.strip().splitlines()
    assert len(lines) == 1 and lines[0].startswith("python -m tabletop.gui: "), p.stderr
    assert "not built" in lines[0] and "tools/build_fast.py" in lines[0]
    assert "Traceback" not in p.stderr


def test_without_a_build_the_pure_engine_runs_on_request_and_the_window_says_so(tmp_path):
    """The same copy with the pure engine asked for explicitly: `--smoke` runs (the capped
    line, exit 1) and the window's status bar note says `engine: python (explicit
    request)`, its tooltip that the pure engine is the specification (G-063; D-107)."""
    pytest.importorskip("PySide6")
    where = package_without_the_extension(tmp_path)
    env = engine_env(where, "python")
    p = run([sys.executable, "-m", "tabletop.gui", "--smoke", "--set", "max_codelets=5"],
            env, cwd=where)
    assert p.returncode == 1 and "(capped)" in p.stdout and p.stderr == "", p.stderr
    cli = run([sys.executable, "-m", "tabletop", "surround-05", "--seed", "1", "--quiet",
               "--set", "max_codelets=5"], env, cwd=where)
    assert "  engine python  " in cli.stdout.splitlines()[0]
    assert p.stdout.rstrip("\n") == cli.stdout.rstrip("\n").splitlines()[-1]
    note = run([sys.executable, "-c", NOTE_SCRIPT], env, cwd=where)
    assert note.returncode == 0, note.stderr
    text, tooltip = note.stdout.splitlines()[:2]
    assert text == "engine: python (explicit request)"
    assert "specification" in tooltip and "explicit request" in tooltip


def test_the_refusal_is_a_dialog_where_qt_is_up(monkeypatch, capsys):
    """Where Qt is up and `--smoke` was not given, the refusal is a critical message box
    with the seam's message as its informative text, and the message on stderr too; under
    `--smoke` the message alone (G-063). The seam's error is stood in for, the engine being
    built here; the box's modal `exec` is recorded, not run. (No pytest-qt fixture: the
    file collects without the plugin, G-005.)"""
    pytest.importorskip("PySide6")
    from PySide6.QtWidgets import QApplication, QMessageBox
    from tabletop.gui import app
    QApplication.instance() or QApplication([])
    message = ("the compiled Tabletop engine is not built (a probe): run "
               "`python3 tools/build_fast.py` from the python/ folder")
    monkeypatch.setattr(app, "engine_error", lambda: ImportError(message))
    shown = []
    monkeypatch.setattr(QMessageBox, "exec", lambda box: shown.append(box) or 0)
    assert app.main(["--problem", "surround-05", "--seed", "3"]) == app.NOT_STARTED == 1
    assert len(shown) == 1
    box = shown[0]
    assert box.icon() == QMessageBox.Icon.Critical and box.informativeText() == message
    assert "cannot start" in box.windowTitle() and "tools/build_fast.py" in box.informativeText()
    assert capsys.readouterr().err == f"python -m tabletop.gui: {message}\n"
    assert app.main(["--smoke", "--problem", "surround-05"]) == 1
    assert len(shown) == 1 and capsys.readouterr().err == f"python -m tabletop.gui: {message}\n"

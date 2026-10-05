"""The Qt GUI's skeleton and test harness (loop0003 item 01).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

- the main window opens and closes headlessly, in this process (the `qapp`
  fixture of conftest.py: QT_QPA_PLATFORM=offscreen) and as
  `python3 -m metacat.qt` in a fresh process;
- the screenshot helper (metacat.qt.grab) writes a PNG of a widget;
- the harness itself: every test_qt_*.py skips cleanly when PySide6 is missing
  (checked in a fresh pytest with PySide6 made unimportable), and importing
  metacat.qt needs no PySide6;
- pyproject.toml declares the qt extra and the package.
"""
from __future__ import annotations

import os
import re
import subprocess
import sys
import tomllib
from pathlib import Path

import pytest

QtWidgets = pytest.importorskip("PySide6.QtWidgets")

HERE = Path(__file__).resolve().parent
PY = HERE.parent


def test_main_window_opens_and_closes(qapp):
    from metacat.qt.mainwindow import MainWindow
    window = MainWindow()
    window.show()
    qapp.processEvents()
    assert window.isVisible()
    assert window.windowTitle() == "Metacat"
    assert isinstance(window, QtWidgets.QMainWindow)
    window.close()
    qapp.processEvents()
    assert not window.isVisible()


def test_the_screenshot_helper_writes_a_png(qapp, tmp_path):
    from PySide6.QtGui import QImage
    from metacat.qt.grab import grab_png
    from metacat.qt.mainwindow import MINIMUM_SIZE, MainWindow
    window = MainWindow()
    w, h = MINIMUM_SIZE[0] + 100, MINIMUM_SIZE[1] + 100      # (item 08: a minimum size)
    window.resize(w, h)
    window.show()
    qapp.processEvents()
    path = grab_png(window, tmp_path / "sub" / "window.png")
    window.close()
    assert path == tmp_path / "sub" / "window.png"
    assert path.read_bytes()[:8] == b"\x89PNG\r\n\x1a\n"
    image = QImage(str(path))
    ratio = window.devicePixelRatioF()
    assert (image.width(), image.height()) == (round(w * ratio), round(h * ratio))


def test_the_grab_fixture(grab, qapp):
    label = QtWidgets.QLabel("Metacat")
    label.resize(120, 40)
    label.show()
    path = grab(label, "label")
    label.close()
    assert path.name == "label.png" and path.stat().st_size > 0


def test_python_m_metacat_qt_opens_and_quits():
    env = dict(os.environ, QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run([sys.executable, "-m", "metacat.qt", "--quit-after", "200"],
                          cwd=PY, env=env, capture_output=True, text=True, timeout=60)
    assert proc.returncode == 0, proc.stderr
    assert "Metacat" in proc.stdout     # the title of the window it opened


def test_importing_metacat_qt_needs_no_pyside6():
    code = ("import sys, builtins\n"
            "real = builtins.__import__\n"
            "def guard(name, *a, **k):\n"
            "    if name.startswith('PySide6'): raise ImportError('no PySide6')\n"
            "    return real(name, *a, **k)\n"
            "builtins.__import__ = guard\n"
            "import metacat.qt\n"
            "print(metacat.qt.has_pyside6())\n")
    proc = subprocess.run([sys.executable, "-c", code], cwd=PY,
                          capture_output=True, text=True, timeout=60)
    assert proc.returncode == 0, proc.stderr
    assert proc.stdout.strip() == "False"


def test_qt_tests_skip_without_pyside6(tmp_path):
    """a fresh pytest with PySide6 unimportable skips every test_qt_*.py: no
    error, no failure, no test run"""
    blocker = tmp_path / "block"
    (blocker / "PySide6").mkdir(parents=True)
    (blocker / "PySide6" / "__init__.py").write_text(
        "raise ImportError('PySide6 is hidden by test_qt_skeleton.py')\n")
    files = sorted(p.name for p in HERE.glob("test_qt_*.py"))
    assert "test_qt_skeleton.py" in files
    env = dict(os.environ, PYTHONPATH=str(blocker), QT_QPA_PLATFORM="offscreen")
    proc = subprocess.run([sys.executable, "-m", "pytest", "-q", "-rs", "-p", "no:cacheprovider",
                           *("tests/" + f for f in files)],
                          cwd=PY, env=env, capture_output=True, text=True, timeout=300)
    # 5: a module-level skip leaves no test collected (0 inside a larger run)
    assert proc.returncode in (0, 5), proc.stdout + proc.stderr
    assert not re.search(r"\d+ (passed|failed|error)", proc.stdout), proc.stdout
    assert re.search(rf"{len(files)} skipped", proc.stdout), proc.stdout


def test_every_qt_test_file_guards_its_pyside6_import():
    """the convention that makes them skip: importorskip before any PySide6 import"""
    for path in HERE.glob("test_qt_*.py"):
        text = path.read_text()
        guard = text.find('pytest.importorskip("PySide6')
        assert guard >= 0, path.name
        first = min((m.start() for m in re.finditer(r"^(from|import) (PySide6|metacat\.qt)",
                                                     text, re.M)), default=len(text))
        assert guard < first, path.name


def test_pyproject_declares_the_qt_extra_and_package():
    data = tomllib.loads((PY / "pyproject.toml").read_text())
    qt = data["project"]["optional-dependencies"]["qt"]
    assert len(qt) == 1 and qt[0].startswith("PySide6")
    assert data["project"]["dependencies"] == []
    assert "metacat.qt" in data["tool"]["setuptools"]["packages"]


def test_run_tests_sh_keeps_qt_headless():
    text = (PY / "run-tests.sh").read_text()
    assert "export QT_QPA_PLATFORM=offscreen" in text
    assert "--qt" in text


def test_no_engine_module_imports_qt():
    for path in (PY / "metacat").glob("*.py"):
        assert "PySide6" not in path.read_text(), path.name
    for path in (PY / "metacat" / "gui").glob("*.py"):
        assert "PySide6" not in path.read_text(), path.name


def test_qt_tests_run_after_the_others(request):
    """the QApplication starts a thread of its own, and many tests fork
    (golden_harness, the extra seeds); forking a multi-threaded process may
    deadlock, so conftest.py runs every test_qt_*.py file last"""
    names = [item.path.name for item in request.session.items]
    first = next(i for i, n in enumerate(names) if n.startswith("test_qt_"))
    assert all(n.startswith("test_qt_") for n in names[first:]), names[first:]

"""Shared fixtures of the GUI tests (Numbo GUI, `tests/gui/test_gui_shell.py` and
`tests/conftest.py`; G-002: the offscreen setting lives here, not in `tests/`).

Qt draws offscreen unless told otherwise, so no window ever opens on the owner's screen.
`thread_errors` fails a test whose worker thread raised; `window(qtbot, thread_errors)`
is Numbo's: a `MainWindow` with `settings=None` and no default layout file, shown
offscreen, closed at the end with its worker asserted finished (it imports PySide6 only when used, so the engine's suite can
load this file without it); `module_window` is the same window shared by every test of a
module, for speed (ANSWERS.md 10); `ini` is the path of a temporary INI for the settings
tests. The window fixtures work from item 02, when `tabletop/gui/main_window.py` exists.
`golden_runs` (session-scoped, Qt-free) holds the four golden runs of
`tests/data/golden/ENGINE/fingerprints.json` (the set of the engine that is running, D-107) as plain runs (`golden.py`'s `GoldenRun`: the
events, their records, the draws, the result and French's report line), the reference
the GUI's models are tested against (G-009).
"""

import os
import threading

import pytest

from golden import golden_cases, plain_run

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")


@pytest.fixture(scope="session")
def golden_runs() -> dict:
    """The four golden runs, keyed `(problem, seed)`, run once per session (G-009)."""
    return {(problem, seed): plain_run(problem, seed) for problem, seed in golden_cases()}


@pytest.fixture
def thread_errors(monkeypatch):
    """The exceptions raised on other threads during the test; the test fails if there
    were any (Numbo GUI, `test_gui_shell.py`)."""
    errors = []
    monkeypatch.setattr(threading, "excepthook", lambda args: errors.append(args))
    yield errors
    assert errors == []


@pytest.fixture
def ini(tmp_path):
    """A temporary INI file for `QSettings` (Numbo GUI, `test_polish.py`)."""
    return str(tmp_path / "tabletop-gui.ini")


def open_window():
    """A `MainWindow(settings=None)` with no default layout file (the built-in
    arrangement), shown offscreen (Numbo GUI, `test_gui_shell.py`)."""
    from PySide6.QtWidgets import QApplication
    from tabletop.gui.main_window import MainWindow
    w = MainWindow(settings=None, layout_path=None)
    w.show()
    QApplication.instance().processEvents()
    return w


def close_window(w):
    """Close the window and check that its worker thread has finished."""
    w.close()
    assert not w.controller.is_alive()


@pytest.fixture
def window(qtbot, thread_errors):
    """A fresh `MainWindow(settings=None)`, shown offscreen; closed at the end, with no
    live worker (Numbo GUI, `test_gui_shell.py`)."""
    w = open_window()
    qtbot.addWidget(w)
    qtbot.waitExposed(w)
    yield w
    close_window(w)


@pytest.fixture(scope="module")
def module_window(qapp):
    """One `MainWindow(settings=None)` shared by the tests of a module (ANSWERS.md 10: one
    window per module where the tests allow it). Exceptions on other threads during the
    module fail at its end, as `thread_errors` does per test."""
    errors = []
    previous = threading.excepthook
    threading.excepthook = lambda args: errors.append(args)
    w = open_window()
    try:
        yield w
    finally:
        close_window(w)
        threading.excepthook = previous
    assert errors == []

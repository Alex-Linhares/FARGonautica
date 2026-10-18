"""Shared fixtures of the GUI tests (Numbo GUI, `tests/gui/test_gui_shell.py` and
`tests/conftest.py`; G-002: the offscreen setting lives here, not in `tests/`).

Qt draws offscreen unless told otherwise, so no window ever opens on the owner's screen.
`thread_errors` fails a test whose worker thread raised; `window(qtbot, thread_errors)`
is Numbo's: a `MainWindow` with `settings=None` and no default layout file, shown
offscreen, closed at the end with its worker asserted finished (it imports PySide6 only
when used, so the engine's suite can load this file without it); `ini` is the path of a
temporary INI for the settings tests. `appendix_runs` gives the three appendix runs
with Meredith's inputs on the golden seeds [M86 pp. 164-179], each run once, plain, as the
reference every observer is compared with (`golden.py`; Numbo GUI,
`test_run_controller.py`'s `reference`).
"""

import os
import threading

import pytest

from golden import APPENDIX, run_plain

os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")


@pytest.fixture(scope="session")
def appendix_runs():
    """The three appendix runs, plain, keyed by run id."""
    return {run_id: run_plain(run_id, seed) for run_id, seed in APPENDIX}


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
    return str(tmp_path / "seekwhence-gui.ini")


@pytest.fixture
def window(qtbot, thread_errors):
    """A `MainWindow(settings=None, layout_path=None)`, shown offscreen; closed at the
    end, with no live worker (Numbo GUI, `test_gui_shell.py`)."""
    from seekwhence.gui.main_window import MainWindow
    w = MainWindow(settings=None, layout_path=None)
    qtbot.addWidget(w)
    w.show()
    qtbot.waitExposed(w)
    yield w
    w.close()
    assert not w.controller.is_alive()

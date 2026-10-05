# Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
# Makes python/oracle/ (capture.py) and python/tests/ (helpers) importable.
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
for path in (HERE, HERE.parent / "oracle"):
    if str(path) not in sys.path:
        sys.path.insert(0, str(path))


# --- Qt (loop0003): a headless QApplication and a screenshot helper ----------
# PySide6 is optional: these fixtures import it only when a test asks for them,
# and every test_qt_*.py starts with pytest.importorskip("PySide6...").

import pytest  # noqa: E402

SCREENSHOTS = HERE / "screenshots-qt"     # grab()'s pictures (not committed)

# `python3 -m metacat.qt` saves its layout in the user's QSettings; the tests'
# runs of it (and their subprocesses) use a throw-away INI file instead
import os  # noqa: E402
import tempfile  # noqa: E402
os.environ["METACAT_QT_SETTINGS"] = str(Path(tempfile.mkdtemp(prefix="metacat-qt-settings-"))
                                        / "metacat-qt.ini")


def pytest_collection_modifyitems(session, config, items):
    """the test_qt_*.py files run last: the QApplication starts a thread, and
    forking a multi-threaded process (golden_harness, the extra seeds) may
    deadlock (Python warns: "use of fork() may lead to deadlocks")"""
    items.sort(key=lambda item: item.path.name.startswith("test_qt_"))


@pytest.fixture(scope="session")
def qapp():
    """The QApplication of the session, offscreen: never a window on the real
    screen.  The garbage collector runs on its thread only, as in the program
    (hosts.collect_on_gui_thread).  At the end, every window is closed and the
    application quits."""
    import os
    os.environ["QT_QPA_PLATFORM"] = "offscreen"
    os.environ.pop("WAYLAND_DISPLAY", None)
    pytest.importorskip("PySide6.QtWidgets")
    from PySide6.QtWidgets import QApplication
    from metacat.qt.hosts import collect_on_gui_thread
    app = QApplication.instance() or QApplication(["metacat-tests"])
    collect_on_gui_thread()
    yield app
    app.closeAllWindows()
    app.processEvents()
    app.quit()


@pytest.fixture(autouse=True)
def collect_qt_garbage_on_the_gui_thread(request):
    """After each Qt test, collect the cycles it left (closed MainWindows,
    fake hosts: QtHost <-> Pane, with their PaneViews and QGraphicsScenes) on
    the GUI thread.  Otherwise Python's cycle collector frees them whenever an
    allocation triggers it, in whichever thread: a later test's worker thread
    then destroyed QWidgets off the GUI thread and hung, holding the paint
    gate (test_fonts_measure_from_several_threads_at_once; anomalies: "Qt
    widgets freed by the cycle collector in a worker thread")."""
    yield
    if not request.node.path.name.startswith("test_qt_") or "PySide6.QtWidgets" not in sys.modules:
        return
    import gc
    from PySide6.QtWidgets import QApplication
    app = QApplication.instance()
    if app is not None:
        app.processEvents()
        gc.collect()


@pytest.fixture
def grab(qapp, request):
    """grab(widget, name) -> the path of a PNG of the widget, in
    tests/screenshots-qt/<test name>/<name>.png, for inspection."""
    from metacat.qt.grab import grab_png

    def take(widget, name):
        qapp.processEvents()
        return grab_png(widget, SCREENSHOTS / request.node.name / (name + ".png"))
    return take

"""The Qt GUI program: the QApplication and the main window.  `python3 -m
metacat.qt` runs `main`.

Metacat is copyright (c) 1999, 2003 by James B. Marshall; this translation is free
software under the GNU General Public License, version 2 or later, like Metacat
itself.  Translated to Python (2026).  Item 01 of loop0003 made the window;
item 04 put the panels in it (`setup`, setup.ss's window part on Qt hosts);
item 05 the engine thread and the run controls (the control strip); item 06
the menus and dialogs (docs/qt-gui-plan.md); item 08 the saved layout, the
window's size on each screen, high DPI and the icon (`make_application`,
`open_window`).
"""
from __future__ import annotations

import argparse
import os
import sys

SETTINGS_ENV = "METACAT_QT_SETTINGS"     # an INI file instead of the user's settings
# port: the problem the command line starts with (Run 7 of the dissertation), so that
# Enter starts a run at once.  The original's box starts empty; see docs/divergences.md.
DEFAULT_PROBLEM = "abc abd xyz 3852097033"


def parse_args(argv):
    parser = argparse.ArgumentParser(prog="python3 -m metacat.qt",
                                     description="Metacat in one window (Qt).")
    parser.add_argument("problem", nargs="*", metavar="WORD",
                        help="the problem to put in the command line, e.g. abc abd ijk 7 "
                        "(default: %s)" % DEFAULT_PROBLEM)
    parser.add_argument("--empty", action="store_true",
                        help="start with an empty command line, as the original does")
    parser.add_argument("--quit-after", type=int, metavar="MS", default=None,
                        help="close the window and quit after MS milliseconds")
    parser.add_argument("--screenshot", metavar="PNG", default=None,
                        help="with --quit-after: grab the window into PNG before quitting")
    parser.add_argument("--settings", metavar="INI", default=None,
                        help="save the layout in this INI file (default: $%s, or the "
                        "user's QSettings for fargonauts/metacat-qt)" % SETTINGS_ENV)
    return parser.parse_args(argv)


def setup(window):
    """setup.ss: setup, on Qt hosts in window's panes (GUI thread): the Qt
    fonts and hosts, the engine and the graphics files loaded, every window
    made as views.attach_views makes them and placed in its pane, the control
    panel (metacat/qt/controls.py) in the window's control strip, the engine
    thread (metacat/qt/engine_bridge.py), then enable-resizing (each panel
    redraws at its pane's size).  Returns the windows by name."""
    from metacat import engine
    from metacat import setup as S
    from metacat.gui import app as gui_app
    from metacat.gui import views
    from metacat.qt import controls, hosts
    from metacat.qt.engine_bridge import EngineBridge
    hosts.install()
    engine.load()
    views.load_views()
    windows = views.attach_views()
    window.place_windows(windows)
    bridge = EngineBridge()
    S.g_control_panel = controls.make_control_panel(bridge.invoker)
    window.place_control_panel(S.g_control_panel, bridge)
    from metacat.qt import icon
    window.setWindowIcon(icon.app_icon())
    gui_app.enable_resizing()
    return windows


def make_application():
    """the QApplication, with high-DPI scale factors used as the screen gives
    them (Qt 6 always scales: every size in the GUI is in logical pixels)"""
    from PySide6.QtCore import Qt
    from PySide6.QtGui import QGuiApplication
    from PySide6.QtWidgets import QApplication
    app = QApplication.instance()
    if app is None:
        QGuiApplication.setHighDpiScaleFactorRoundingPolicy(
            Qt.HighDpiScaleFactorRoundingPolicy.PassThrough)
        app = QApplication(["metacat"])
    app.setApplicationName("metacat-qt")
    app.setOrganizationName("fargonauts")
    return app


def open_settings(path=None):
    """the QSettings the layout is saved in: the INI file path, $%s, or
    the user's settings for fargonauts/metacat-qt"""
    from PySide6.QtCore import QSettings
    path = path or os.environ.get(SETTINGS_ENV)
    if path:
        return QSettings(path, QSettings.IniFormat)
    return QSettings("fargonauts", "metacat-qt")


def prefill_command_line(text):
    """port: put text in the control strip's command line, with the cursor at its end
    and the focus on it, so that Enter starts the run"""
    from metacat import setup as S
    line = S.g_control_panel.command_line
    if text and line.isEnabled():
        line.setText(text)
        line.setCursorPosition(len(text))
        line.setFocus()


def open_window(settings):
    """the main window, set up and shown: at its saved geometry, or else
    maximised on a screen larger than the 1080p default size (the default
    sizes of docs/qt-gui-plan.md 2.2 follow the window), then with the saved
    panes and splitter sizes"""
    from PySide6.QtWidgets import QApplication
    from metacat.qt.mainwindow import DEFAULT_SIZE, MainWindow
    app = QApplication.instance()
    window = MainWindow(settings)
    setup(window)
    app.setWindowIcon(window.windowIcon())
    if window.restore_geometry():
        window.show()
    else:
        avail = window.screen().availableGeometry()
        if avail.width() >= DEFAULT_SIZE[0] and avail.height() > DEFAULT_SIZE[1]:
            window.showMaximized()
        else:
            window.show()
    app.processEvents()
    window.restore_layout()
    return window


def main(argv=None):
    """port: the program.  Prints the window's title once it is shown."""
    args = parse_args(sys.argv[1:] if argv is None else argv)
    from metacat import qt
    if not qt.has_pyside6():
        print("The Qt GUI needs PySide6: pip install -e 'python[qt]'", file=sys.stderr)
        return 1
    from PySide6.QtCore import QTimer
    app = make_application()
    window = open_window(open_settings(args.settings))
    if not args.empty:
        prefill_command_line(" ".join(args.problem) or DEFAULT_PROBLEM)
    print(window.windowTitle(), flush=True)
    print("Panes: " + " ".join(name for name, pane in window.panes.items()
                               if pane.isVisibleTo(window)), flush=True)
    if args.quit_after is not None:
        def finish():
            # one callback, so that the grab always comes before the close
            if args.screenshot:
                from metacat.qt.grab import grab_png
                window.sync()
                grab_png(window, args.screenshot)
            window.close()
            app.quit()
        QTimer.singleShot(args.quit_after, finish)
    status = app.exec()
    if window.bridge is not None and window.bridge.busy():
        # port: the engine thread is a daemon in the middle of a run; leave
        # without waiting for it (closing the original's control panel exited)
        import os
        sys.stdout.flush()
        os._exit(status)
    return status

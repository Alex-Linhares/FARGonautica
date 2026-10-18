"""python3 -m tabletop.gui [--problem NAME] [--seed N] [--profile NAME] [--set FIELD=VALUE]
                         [--play] [--smoke]

(tabletop-gui once installed: pyproject.toml's entry point is run().)

The command line of the GUI, after the Numbo GUI's `app.py` through the Seek-Whence GUI's
(G-005, G-054). Opens the main window with the next run's inputs set from the arguments,
which win over the remembered ones (`--problem`, `--seed`, `--profile` and `--set
FIELD=VALUE`, the CLI's names and the CLI's own parser for `--set`); `--play` starts the
run at once. `--smoke` uses no settings, sets the offscreen platform, silences the
offscreen plugin's warnings, runs to the end through the window with every event drawn,
prints French's report line (the last line of `python3 -m tabletop NAME --seed N
--quiet`) and exits: 0 when the run ends with an answer, 1 when it is capped, 2 for a
usage error (ANSWERS.md 1).

The engine is started only once Qt is up (`engine_error`, G-063): the GUI runs the compiled
engine, the reference, and when it is not built (or stale) it refuses to start with the
seam's own message, in a dialog, or on stderr alone under `--smoke` (exit status 1, as when
PySide6 is missing). It runs the pure engine only when `tabletop.accel` was asked for it
explicitly, and the window then says so in its status bar (D-107).
"""

import argparse
import os
import sys

from tabletop.config import PROFILES          # needs no engine (D-107)

__all__ = ["parser", "use_pyside_plugins", "quiet_offscreen_warnings", "run", "main",
           "smoke", "engine_error", "refusal_dialog", "refuse", "USAGE_ERROR", "NOT_STARTED"]

NEEDS_PYSIDE = "python -m tabletop.gui: the GUI needs PySide6: pip install -e '.[gui]'"
USAGE_ERROR = 2
NOT_STARTED = 1          # the GUI could not start: no PySide6, or the engine is not built


def parser():
    """The arguments (Numbo GUI, `app.py`; G-005, G-054): the run's inputs with the
    CLI's names (`--problem`, `--seed`, `--profile`, `--set`), `--play` and `--smoke`."""
    p = argparse.ArgumentParser(prog="python -m tabletop.gui",
                                description="Watch Tabletop (French, 1992) answer a table, "
                                            "one event at a time.",
                                epilog="Exit status of --smoke: 0 when the run ends with an "
                                       "answer, 1 when it is capped or the GUI could not "
                                       "start (no PySide6, or the engine is not built), 2 "
                                       "for a usage error.")
    p.add_argument("--problem", metavar="NAME",
                   help="a problem name, as `python3 -m tabletop --list` prints them "
                        "(default: as last time, or surround-05)")
    p.add_argument("--seed", type=int, metavar="N",
                   help="the seed of the run's only random generator (default: as last "
                        "time, or 1)")
    p.add_argument("--profile", choices=list(PROFILES), metavar="NAME",
                   help="the configuration profile: " + ", ".join(PROFILES)
                        + " (default: as last time, or standard)")
    p.add_argument("--set", action="append", default=[], metavar="FIELD=VALUE",
                   help="a Config field for the next run, as the CLI's --set (may repeat): "
                        "the profile's value with this edit")
    p.add_argument("--play", action="store_true", help="start the run at once")
    p.add_argument("--smoke", action="store_true",
                   help="run to the end offscreen, print French's report line, and exit")
    return p


def use_pyside_plugins():
    """Point Qt at PySide6's own plugins (QT_PLUGIN_PATH, unless it is set), as the
    Numbo GUI does: a qt.conf next to the Python executable (Anaconda ships one for its
    own Qt 5) would otherwise send Qt 6 to another Qt's plugins."""
    import PySide6
    plugins = os.path.join(os.path.dirname(PySide6.__file__), "Qt", "plugins")
    if os.path.isdir(plugins):
        os.environ.setdefault("QT_PLUGIN_PATH", plugins)


OFFSCREEN_NOISE = ("This plugin does not support propagateSizeHints()",
                   "This plugin does not support raise()")    # a floated dock


def quiet_offscreen_warnings():
    """Drop the offscreen platform plugin's warnings that say nothing about the run, so
    that a smoke run prints only its own lines (Numbo GUI, `app.py`); every other Qt
    message is printed as Qt would."""
    from PySide6.QtCore import qFormatLogMessage, qInstallMessageHandler

    def handler(mode, context, message):
        if message not in OFFSCREEN_NOISE:
            sys.stderr.write(qFormatLogMessage(mode, context, message) + "\n")

    qInstallMessageHandler(handler)


def run(argv=None):
    """The tabletop-gui command (and python3 -m tabletop.gui): main, or a message saying
    what to install when PySide6 is missing, with exit status 1 (Numbo GUI, `app.py`)."""
    try:
        import PySide6  # noqa: F401
    except ImportError:
        sys.stderr.write(NEEDS_PYSIDE + "\n")
        return NOT_STARTED
    return main(argv)


def engine_error():
    """Start the engine seam (`tabletop.accel`, D-107) and return None, or the error it
    raised: the compiled engine not built or stale, or a misnamed request (G-063). The GUI
    only relays the seam's message; it chooses no engine and never carries on without one
    (there is no fallback, D-107)."""
    try:
        import tabletop.accel  # noqa: F401
    except ImportError as error:
        return error
    return None


def refusal_dialog(error):
    """The dialog that says the engine could not start, with the seam's message (the build
    command, the explicit request) as its text (G-063; Numbo GUI's error dialogs). Built,
    not shown: `refuse` shows it modally."""
    from PySide6.QtWidgets import QMessageBox
    box = QMessageBox(QMessageBox.Icon.Critical, "Tabletop cannot start",
                      "The engine could not start, so the window did not open.")
    box.setInformativeText(str(error))
    box.setStandardButtons(QMessageBox.StandardButton.Close)
    return box


def refuse(error, dialog: bool) -> int:
    """The GUI cannot start (G-063): the seam's message on stderr, prefixed as the usage
    errors are, and in a dialog where Qt is up (`dialog`: not under `--smoke`, where a
    modal dialog would wait for nobody). Returns `NOT_STARTED`."""
    sys.stderr.write(f"python -m tabletop.gui: {error}\n")
    if dialog:
        refusal_dialog(error).exec()
    return NOT_STARTED


def main(argv=None):
    """Parse the arguments, open the window (`main_window.py`; the settings are read and
    written outside a smoke run), set the next run from the arguments given (the others
    stay as remembered) and run the event loop (Numbo GUI, `app.py`; G-005, G-054). A
    smoke run is `smoke`; a bad argument, an unknown problem or a bad `--set` is a usage
    error, status 2. The engine is started once Qt is up, so that a missing build is
    refused in a dialog (`refuse`, G-063), before anything of the window is imported."""
    args = parser().parse_args(argv)
    use_pyside_plugins()
    if args.smoke:
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        quiet_offscreen_warnings()
    from PySide6.QtCore import QSettings
    from PySide6.QtWidgets import QApplication

    app = QApplication.instance() or QApplication(sys.argv[:1])
    error = engine_error()
    if error is not None:
        return refuse(error, dialog=not args.smoke)
    from tabletop.__main__ import configure
    from tabletop.gui.main_window import MainWindow

    # QSettings("tabletop", "tabletop-gui") is read and written outside a smoke run
    # (ANSWERS.md 2; G-005).
    settings = None if args.smoke else QSettings("tabletop", "tabletop-gui")
    window = MainWindow(settings=settings)
    try:
        window.set_next_run(problem=args.problem, profile=args.profile, seed=args.seed)
        if args.set:
            window.set_next_config(configure(window.profile, args.set))
    except ValueError as error:
        window.close()
        parser().error(str(error))      # prints the usage and exits with status 2
    window.show()

    if not args.smoke:
        if args.play:
            window.play()
        return app.exec()
    return smoke(app, window)


def smoke(app, window) -> int:
    """Run the window's next run to the end, every event drawn, print French's report
    line (`report_line`, the last line of the CLI's `--quiet` trace) and return 0 for
    an answer, 1 when the run was capped or did not end, 2 when it could not start
    (Numbo GUI, `app.py`; ANSWERS.md 1; G-054)."""
    from tabletop.run import report_line
    window.run_finished.connect(lambda outcome: app.quit())
    window.run_to_end()
    if not window.controller.is_alive() and window.outcome is None:
        print(f"python -m tabletop.gui: {window.statusBar().currentMessage()}", file=sys.stderr)
        window.close()
        return USAGE_ERROR
    app.exec()
    outcome = window.outcome
    window.close()
    if outcome is None or outcome.result is None:
        why = outcome.error if outcome is not None and outcome.error is not None else "stopped"
        print(f"python -m tabletop.gui: the run did not end: {why}", file=sys.stderr)
        return 1
    print(report_line(outcome.result))
    return 1 if outcome.result.capped else 0

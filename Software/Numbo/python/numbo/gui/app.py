"""python3 -m numbo.gui [--puzzle N | --custom "T B B B B B"] [--seed S] ...

(numbo-gui once installed: pyproject.toml's entry point is run().)

Opens the main window with the controls set from the arguments (the
others, and the window's size and docks, are as they were left: QSettings
"numbo"/"numbo-gui"); --play starts the run.  --smoke runs the puzzle to
the end with every event drawn (offscreen unless QT_QPA_PLATFORM says
otherwise), with the defaults for the inputs not given and no settings
read or written, prints the CLI's two summary lines and exits: 0 for a
valid solution, 1 otherwise, 2 for a usage error.
"""

import argparse
import os
import sys

from numbo.models import run_stats


def parser():
    p = argparse.ArgumentParser(prog="python -m numbo.gui",
                                description="Watch Numbo (Defays, 1987) solve a puzzle, "
                                            "one event at a time.")
    p.add_argument("--puzzle", type=int, choices=range(1, len(run_stats.CHAPTER_PUZZLES) + 1),
                   metavar="N", help="one of the chapter's 11 puzzles (default: as last time, "
                                     "or 1)")
    p.add_argument("--custom", metavar="'T B B B B B'",
                   help="a custom target and 5 bricks instead of a chapter puzzle")
    p.add_argument("--seed", help="seed of the shared RNG (default: as last time, or 1)")
    p.add_argument("--max-iterations", metavar="N",
                   help="stop after N iterations, or 'none' (default: as last time, or 20000)")
    p.add_argument("--play", action="store_true", help="start the run at once")
    p.add_argument("--smoke", action="store_true",
                   help="run to the end, print the outcome and the check, and exit")
    return p


def use_pyside_plugins():
    """Point Qt at PySide6's own plugins (QT_PLUGIN_PATH, unless it is set).
    Qt reads a qt.conf next to the Python executable; Anaconda ships one for
    its own Qt 5, and outside an activated conda environment Qt 6 then
    looks for its platform plugins there, finds only Qt 5 ones, and aborts."""
    import PySide6
    plugins = os.path.join(os.path.dirname(PySide6.__file__), "Qt", "plugins")
    if os.path.isdir(plugins):
        os.environ.setdefault("QT_PLUGIN_PATH", plugins)


OFFSCREEN_NOISE =("This plugin does not support propagateSizeHints()",)


def quiet_offscreen_warnings():
    """Drop the offscreen platform plugin's warnings that say nothing about
    the run (a smoke run prints only the CLI's lines); every other Qt
    message is printed as Qt would."""
    from PySide6.QtCore import qFormatLogMessage, qInstallMessageHandler

    def handler(mode, context, message):
        if message not in OFFSCREEN_NOISE:
            sys.stderr.write(qFormatLogMessage(mode, context, message) + "\n")

    qInstallMessageHandler(handler)


def run(argv=None):
    """The numbo-gui command (and python3 -m numbo.gui): main, or a message
    saying what to install when PySide6 is missing (exit status 1)."""
    try:
        import PySide6  # noqa: F401
    except ImportError:
        sys.stderr.write("python -m numbo.gui: the GUI needs PySide6 "
                         "(pip install -e 'python[gui]')\n")
        return 1
    return main(argv)


def main(argv=None):
    args = parser().parse_args(argv)
    use_pyside_plugins()
    if args.smoke:
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        quiet_offscreen_warnings()
    from PySide6.QtCore import QSettings
    from PySide6.QtWidgets import QApplication
    from numbo.gui.main_window import MainWindow
    from numbo.gui.controls import CUSTOM

    app = QApplication.instance() or QApplication(sys.argv[:1])
    # The window's size, docks and inputs are remembered between sessions;
    # a smoke run neither reads nor writes them.
    settings = None if args.smoke else QSettings("numbo", "numbo-gui")
    window = MainWindow(settings=settings)
    c = window.controls
    # The inputs given here win over the remembered ones; a smoke run uses
    # the defaults for the others.
    if args.custom is not None:
        c.puzzle_combo.setCurrentText(CUSTOM)
        c.custom_edit.setText(args.custom)
    elif args.puzzle is not None or args.smoke:
        c.puzzle_combo.setCurrentIndex((args.puzzle or 1) - 1)
    for value, default, edit in ((args.seed, "1", c.seed_edit),
                                 (args.max_iterations, "20000", c.cap_edit)):
        if value is not None or args.smoke:
            edit.setText(value if value is not None else default)
    app.aboutToQuit.connect(window.shutdown)
    window.show()

    if not args.smoke:
        if args.play:
            window.play()
        return app.exec()

    try:
        c.inputs()
    except run_stats.InputError as error:
        parser().error(str(error))
    window.run_finished.connect(lambda outcome: app.quit())
    window.run_to_end()
    app.exec()
    m = window.stats.model
    if window.outcome is None or m.ended is None:
        print("outcome: the run did not end", file=sys.stderr)
        return 1
    line = f"outcome: {m.outcome}, {m.iterations} iterations (seed {m.seed})"
    if m.outcome == "error":
        line += f": {m.ended.message}"
    print(line)
    print(f"check: {m.check_text()}")
    return 0 if m.check is not None and m.check[0] else 1

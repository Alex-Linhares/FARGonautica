"""python3 -m seekwhence.gui [--preset NAME | --terms T ...] [--seed N] [--answers A ...]
                           [--max-steps N] [--play] [--smoke]

(seekwhence-gui once installed: pyproject.toml's entry point is run().)

The command line of the GUI, after the Numbo GUI's `app.py` (item 16; G-055). Opens the
main window with the controls set from the arguments; the others, and the window's
size and docks, are as they were left (QSettings "seekwhence"/"seekwhence-gui").
`--play` starts the run at once. `--smoke` runs the inputs to the end with every event
drawn, offscreen unless QT_QPA_PLATFORM says otherwise, with the defaults for the
inputs not given and no settings read or written, prints the CLI's summary line
(`Session.summary()`, DECISION D-069) and exits: 0 for an accepted guess, 1 otherwise,
2 for a usage error.
"""

import argparse
import dataclasses
import os
import sys

from seekwhence.gui.models import controls as cm
from seekwhence.gui.models import presets

__all__ = ["parser", "use_pyside_plugins", "quiet_offscreen_warnings", "run", "main",
           "apply_arguments", "smoke_line"]


def parser():
    """The arguments (Numbo GUI, `app.py`): the run's inputs, `--play` and `--smoke`
    (G-055). `--max-steps` is the CLI's name for the cap on the run's tasks."""
    p = argparse.ArgumentParser(prog="python -m seekwhence.gui",
                                description="Watch Seek-Whence (Meredith, 1986) extend a "
                                            "sequence, one event at a time.",
                                epilog="Exit status of --smoke: 0 for an accepted guess, "
                                       "1 otherwise, 2 for a usage error.")
    what = p.add_mutually_exclusive_group()
    what.add_argument("--preset", metavar="NAME",
                      help="a preset by id: an appendix run (run1_3737, run2_373373, "
                           "run3_121231234) or a sequence of the lists, such as "
                           "blackburn-1 (default: as last time, or run1_3737)")
    what.add_argument("--terms", nargs="+", metavar="TERM",
                      help="your own terms (console commands allowed among them) instead "
                           "of a preset")
    p.add_argument("--seed", help="the seed of the run's only random generator (default: "
                                  "as last time, or the preset's)")
    p.add_argument("--answers", nargs="+", metavar="ANSWER",
                   help="the answers to the guesses, in order (ok or no), as the CLI's")
    p.add_argument("--max-steps", metavar="N",
                   help="stop the run after N tasks (default: as last time, or the "
                        "configuration's cap)")
    p.add_argument("--play", action="store_true", help="start the run at once")
    p.add_argument("--smoke", action="store_true",
                   help="run to the end offscreen, print the CLI's summary line, and exit")
    return p


def use_pyside_plugins():
    """Point Qt at PySide6's own plugins (QT_PLUGIN_PATH, unless it is set), as the Numbo
    GUI does: a qt.conf next to the Python executable (Anaconda ships one for its own
    Qt 5) would otherwise send Qt 6 to another Qt's plugins."""
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
    """The seekwhence-gui command (and python3 -m seekwhence.gui): main, or a message
    saying what to install when PySide6 is missing, with exit status 1 (Numbo GUI,
    `app.py`)."""
    try:
        import PySide6  # noqa: F401
    except ImportError:
        sys.stderr.write("python -m seekwhence.gui: the GUI needs PySide6 "
                         "(pip install -e '.[gui]')\n")
        return 1
    return main(argv)


def apply_arguments(window, args):
    """Set the window's controls from the arguments: the inputs given win over the
    remembered ones, and a smoke run uses the defaults for the others (Numbo GUI,
    `app.py`; G-055). Returns the window's inputs; `cm.InputError` for a bad value."""
    c = window.controls
    values = c.values()
    if args.smoke:
        values = cm.ControlValues(speed=values.speed)
    if args.terms is not None:
        values = dataclasses.replace(values, preset=cm.CUSTOM, terms=" ".join(args.terms),
                                     answering="scripted")
    elif args.preset is not None:
        values = dataclasses.replace(values, preset=args.preset, answering="scripted")
    if args.preset is not None or args.terms is not None or args.smoke:
        try:
            entry = cm.entry_of(values.preset)
        except KeyError:
            raise cm.InputError(f"no preset {values.preset!r}") from None
        if args.seed is None:
            values = dataclasses.replace(values, seed=str(entry.seed), verbose=entry.verbose)
    if args.seed is not None:
        values = dataclasses.replace(values, seed=args.seed)
    if args.max_steps is not None:
        values = dataclasses.replace(values, cap_total=args.max_steps)
    inputs = cm.run_inputs(values, c.base_config)
    if args.answers is not None:
        inputs = dataclasses.replace(inputs, answers=tuple(args.answers))
    window.set_inputs(inputs)
    return inputs


def smoke_line(window):
    """The line a smoke run prints and its exit status (G-055): the CLI's summary line
    (`Session.summary()`, DECISION D-069) and 0 for an accepted guess, else 1."""
    outcome = window.outcome
    result = outcome.result if outcome is not None else None
    if result is None:
        if outcome is not None and outcome.error is not None:
            return f"outcome: the run did not end: {outcome.error!r}", 1
        return "outcome: the run did not end", 1
    return result["summary"], 0 if result["outcome"] == "ok" else 1


def main(argv=None):
    """Parse the arguments, open the window and run the event loop (Numbo GUI, `app.py`;
    G-055). A smoke run runs the inputs to the end offscreen, prints the summary line
    and returns its status; a bad argument is a usage error, status 2."""
    args = parser().parse_args(argv)
    use_pyside_plugins()
    if args.smoke:
        os.environ.setdefault("QT_QPA_PLATFORM", "offscreen")
        quiet_offscreen_warnings()
    from PySide6.QtCore import QSettings
    from PySide6.QtWidgets import QApplication
    from seekwhence.gui.main_window import MainWindow

    app = QApplication.instance() or QApplication(sys.argv[:1])
    # The window's size, docks and inputs are remembered between sessions; a smoke run
    # neither reads nor writes them.
    settings = None if args.smoke else QSettings("seekwhence", "seekwhence-gui")
    window = MainWindow(settings=settings)
    try:
        apply_arguments(window, args)
    except cm.InputError as error:
        window.shutdown()
        parser().error(str(error))
    app.aboutToQuit.connect(window.shutdown)
    window.show()

    if not args.smoke:
        if args.play:
            window.play()
        return app.exec()

    window.run_finished.connect(lambda outcome: app.quit())
    window.run_to_end()
    app.exec()
    line, status = smoke_line(window)
    ended = window.outcome is not None and window.outcome.result is not None
    print(line, file=sys.stdout if ended else sys.stderr)
    return status

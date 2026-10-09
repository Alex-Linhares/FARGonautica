"""The GUI entry point: ``python3 -m seqsee --gui [options]`` or ``python3 -m seqsee.gui
[options]``, Seqsee.pl with its Tk display.

Seqsee.pl reads ``_read_config(_read_commandline())`` (``--seq``, ``--seed``, ``--max_steps``
or ``-n``, ``--update_interval``, ``--view``, ``--gui_config`` or ``--gui``, ``-f FEATURE``;
the hyphenated ``--max-steps`` / ``--update-interval`` work too, as in the CLI), runs
INITIALIZE (init_display is lib/SGUI.pm's ``setup`` with config/<gui_config>.conf; no
sequence → SGUI::ask_seq) and enters MainLoop: nothing runs until Start / a key. Here:

- ``parse_args``: the same parsing (``seqsee_main.read_commandline`` / ``read_config``); a
  bad ``--seq``, an unknown feature or a gui_config SGUI::setup would die on
  (``check_gui_config``) is a usage error (exit 2). The CLI's ``--continuation`` /
  ``--answer`` / ``--json`` are for headless runs only. ``--gui`` alone just asks for the GUI
  (``cli.split_gui_flag``); ``--gui X`` is Perl's synonym of ``--gui_config X``.
- ``window_options``: what the window gets: the sequence, update_interval and gui_config,
  plus seed, max_steps and view when the command line gives them (they beat the remembered
  settings; without ``--seed`` the seed field stays empty: a random seed per run, as
  Seqsee.pl's load-time default). ``runner_defaults`` (``Runner.defaults``): update_interval,
  gui_config and a given view for the run's options, and the -f features.
- ``build``: the window (``mainwindow.MainWindow``) with a ``runner.Runner`` attached and
  started; with a sequence, a fresh run is set up on it (``start_sequence``: reset, seed,
  INITIALIZE; no steps), else ask_seq's window opens.
- ``main``: no PySide6 → ``missing_pyside6``'s message and exit 1; else QApplication, the
  window with the user's settings (``mainwindow.default_settings``) and the event loop.

The window's layout is GUI_sparse.conf's (the only window layout in config/; GUI_ws3.conf is
the drawing modules' layout and has no [frames], so Perl's SGUI::setup dies on it).

This module imports no Qt until ``build`` / ``main`` need it.
"""
import contextlib
import io
import re
import sys
from dataclasses import dataclass, field

from seqsee import cli
from seqsee import global_ as Global
from seqsee import s, seqsee_main, util
from seqsee.cli import UsageError
from seqsee.errors import Confess

HEADLESS_HINT = 'python3 -m seqsee --seq "1 1 2 1 2 3"'


@dataclass
class GuiArgs:
    options: dict                       # read_config's result (Seqsee.pl's $OPTIONS_ref)
    window_options: dict                # MainWindow(options=)
    runner_defaults: dict               # Runner(defaults=)
    seq: str = ""                       # the sequence, "" for none (ask_seq)
    rest: list = field(default_factory=list)    # the non-options left in @ARGV


def check_gui_config(name):
    """SGUI::setup's ``read_config "config/${gui_config_name}.conf"`` and CreateWidgets'
    ``$config_ref->{frames}{frames} or confess``, as usage errors."""
    rel = f"config/{name}.conf"
    path = cli._REPO / rel
    if not path.is_file():
        raise UsageError(f"Can't open config file '{rel}' (no such file or directory)")
    section = re.search(r"^\[frames\][^\n]*\n(.*?)(?=^\[|\Z)", path.read_text(errors="replace"),
                        re.M | re.S)
    if section is None or not re.search(r"^[ \t]*frames[ \t]*[:=]", section.group(1), re.M):
        raise UsageError(f"{rel} has no [frames] section with frames: it is not a window "
                         "layout (Perl's SGUI::setup dies on it); use GUI_sparse")


def parse_args(argv, check_config=True):
    """Seqsee.pl's command line, for the GUI. Raises UsageError."""
    argv, _ = cli.split_gui_flag(cli.translate_argv(list(argv)))
    rest, cli_opts = cli.split_cli_options(argv)
    for name in ("continuation", "answer", "json"):
        if name in cli_opts:
            raise UsageError(f"--{name} is only for headless runs ({HEADLESS_HINT} ...)")
    s.load()
    s.reset_all()
    quiet = io.StringIO()       # "View: 1!", "<feature> will be turned on"
    try:
        with contextlib.redirect_stdout(quiet):
            given = seqsee_main.read_commandline(rest)      # leaves the non-options in rest
            options = seqsee_main.read_config(**given)
    except Confess as e:
        raise UsageError(str(e).split("\n")[0]) from None
    except SystemExit:          # -f with an unknown feature: "No feature X. Typo?"
        lines = quiet.getvalue().strip().splitlines()
        raise UsageError(lines[-1] if lines else "bad -f feature") from None
    if check_config:
        check_gui_config(util.perl_str(options["gui_config"]))

    seq = " ".join(options["seq"])
    window_options = {"seq": seq, "update_interval": options["update_interval"],
                      "gui_config": options["gui_config"]}
    for key in ("seed", "max_steps", "view"):
        if key in given and given[key] is not None:
            window_options[key] = int(util.perl_num(given[key]))
    runner_defaults = {"update_interval": options["update_interval"],
                       "gui_config": options["gui_config"], "features": dict(Global.Feature)}
    if "view" in window_options:
        runner_defaults["view"] = window_options["view"]
    return GuiArgs(options=options, window_options=window_options,
                   runner_defaults=runner_defaults, seq=seq, rest=list(rest))


def missing_pyside6():
    """None if PySide6 can be imported, else the message to show."""
    try:
        import PySide6.QtWidgets  # noqa: F401
    except ImportError as e:
        return (f"seqsee: the GUI needs PySide6, which could not be imported ({e}).\n"
                "Install it with: pip install PySide6\n"
                f"Or run Seqsee without the GUI: {HEADLESS_HINT}")
    return None


def build(args, settings=None, runner=None):
    """The main window for ``args`` (a GuiArgs), with ``runner`` (default: a new
    ``Runner``) attached and started; then a fresh run on the sequence, or ask_seq. Needs a
    QApplication."""
    from seqsee.gui.qt import mainwindow
    from seqsee.gui.runner import Runner
    if runner is None:
        runner = Runner(defaults=args.runner_defaults)
    else:
        runner.defaults.update(args.runner_defaults)
    window = mainwindow.MainWindow(args.window_options, settings=settings)
    window.attach_runner(runner)
    runner.start()
    if "debugMAX" in args.runner_defaults.get("features", {}):
        window.run_command("debug_max", 1)
    if args.seq:
        window.start_sequence(args.seq)
    else:
        window.ask_seq()
    return window


def _exec(qapp, window):
    import signal
    signal.signal(signal.SIGINT, signal.SIG_DFL)    # Ctrl+C in the terminal quits
    return qapp.exec()


def main(argv=None, err=None, settings=None, run=None):
    """Entry point. Returns the exit status: the event loop's, 1 without PySide6, 2 for a
    usage error. ``settings`` defaults to the user's; ``run(qapp, window)`` (default: the
    event loop) is for tests."""
    err = err if err is not None else sys.stderr
    argv = sys.argv[1:] if argv is None else argv
    try:
        args = parse_args(argv)
    except UsageError as e:
        err.write(f"seqsee: {e}\n")
        return 2
    msg = missing_pyside6()
    if msg is not None:
        err.write(msg + "\n")
        return 1
    from PySide6.QtWidgets import QApplication
    from seqsee.gui.qt import mainwindow
    qapp = QApplication.instance() or QApplication([sys.argv[0] if sys.argv else "seqsee"])
    qapp.setApplicationName("Seqsee")
    qapp.setOrganizationName("Seqsee")
    if settings is None:
        settings = mainwindow.default_settings()
    window = build(args, settings=settings)
    window.show()
    status = (run or _exec)(qapp, window)
    if window.runner is not None:
        window.runner.quit(timeout=5)
    return int(status or 0)

"""The GUI entry point (seqsee/gui/app.py, seqsee/gui/__main__.py, the ``--gui`` switch of
seqsee/cli.py): ``python3 -m seqsee --gui`` and ``python3 -m seqsee.gui``.

Mirrors Seqsee.pl (``_read_config(_read_commandline())``, INITIALIZE with init_display, which
is lib/SGUI.pm's ``setup`` with config/<gui_config>.conf, and SGUI::ask_seq when there is no
sequence) and lib/Seqsee.pm's GetOptions (``gui_config=s``, ``gui=s``: the same option).

Golden: oracle/gui_entry.pl runs the real Seqsee.pl with Tk::MainLoop replaced (``launch``:
the options, the workspace's elements, whether ask_seq opened its window; a screenshot,
docs/gui/perl/entry_window.png), parses several command lines (``options``) and calls
SGUI::setup with each gui_config (``setup``).
"""
import io
import os
import subprocess
import sys
from pathlib import Path

import pytest

import golden
from seqsee import cli
from seqsee import global_ as Global
from seqsee import util
from seqsee.gui import app

GOLDEN = golden.load("gui_entry")
PY = Path(__file__).resolve().parents[1]
SRC = PY / "src"
SCREENS_DIR = PY / "docs" / "gui" / "screens"
OPTION_KEYS = ("seq", "max_steps", "update_interval", "view", "gui_config")


def _cases(name):
    return [c for c in GOLDEN if c["name"] == name]


def _norm(value):
    """Perl's numbers and numeric strings alike ("100000" and 100000)."""
    if isinstance(value, list):
        return [_norm(v) for v in value]
    return util.perl_str(value)


def _env():
    env = dict(os.environ)
    env["PYTHONPATH"] = str(SRC) + os.pathsep + env.get("PYTHONPATH", "")
    env["QT_QPA_PLATFORM"] = "offscreen"
    return env


# ---- the --gui switch ------------------------------------------------------------------------
@pytest.mark.parametrize("argv,rest,gui", [
    (["--gui", "--seq", "1 2"], ["--seq", "1 2"], True),
    (["--seq", "1 2", "--gui"], ["--seq", "1 2"], True),
    (["-gui"], [], True),
    (["--GUI"], [], True),
    (["--gui", "GUI_sparse", "--seq", "1"], ["--gui", "GUI_sparse", "--seq", "1"], True),
    (["--gui=GUI_sparse"], ["--gui=GUI_sparse"], True),
    (["--gui_config", "GUI_sparse"], ["--gui_config", "GUI_sparse"], False),
    (["--seq", "1 2"], ["--seq", "1 2"], False),
    (["--", "--gui"], ["--", "--gui"], False),
    ([], [], False),
])
def test_split_gui_flag(argv, rest, gui):
    """A bare ``--gui`` asks for the GUI and is taken out; ``--gui X`` / ``--gui=X`` (Perl's
    synonym of --gui_config) ask for it too and stay for Getopt."""
    assert cli.split_gui_flag(argv) == (rest, gui)


def test_cli_dispatches_gui(monkeypatch):
    calls = []
    monkeypatch.setattr(app, "main", lambda argv, err=None: calls.append(list(argv)) or 7)
    assert cli.main(["--gui", "--seq", "1 2"]) == 7
    assert calls == [["--gui", "--seq", "1 2"]]


def test_headless_cli_unchanged():
    """Without --gui the CLI runs headless, as before."""
    out, err = io.StringIO(), io.StringIO()
    assert cli.main(["--seq", "1 2 3", "--seed", "3", "--max-steps", "5"], out=out, err=err) == 0
    assert out.getvalue().startswith("Sequence: 1 2 3\n")
    assert "after 5 steps" in out.getvalue() or "Status:" in out.getvalue()


def test_core_never_imports_gui():
    """A headless run imports neither seqsee.gui nor PySide6."""
    code = ("import sys, io; from seqsee import cli; "
            "cli.main(['--seq', '1 2 3', '--seed', '3', '--max-steps', '3'], out=io.StringIO()); "
            "bad = [m for m in sys.modules if m.startswith(('seqsee.gui', 'PySide6'))]; "
            "print(bad); sys.exit(1 if bad else 0)")
    r = subprocess.run([sys.executable, "-c", code], capture_output=True, text=True, env=_env(),
                       timeout=120)
    assert r.returncode == 0, r.stdout + r.stderr


def test_app_module_imports_without_qt():
    """seqsee.gui.app can be imported (and give its message) without loading Qt."""
    code = ("import sys; import seqsee.gui.app; "
            "bad = [m for m in sys.modules if m.startswith('PySide6')]; "
            "sys.exit(1 if bad else 0)")
    r = subprocess.run([sys.executable, "-c", code], capture_output=True, text=True, env=_env(),
                       timeout=60)
    assert r.returncode == 0, r.stdout + r.stderr


# ---- missing PySide6 -------------------------------------------------------------------------
_NO_PYSIDE6 = ("import runpy, sys; sys.modules['PySide6'] = None; "
               "sys.argv = [{mod!r}] + {argv!r}; "
               "runpy.run_module({mod!r}, run_name='__main__', alter_sys=True)")


@pytest.mark.parametrize("mod,argv", [
    ("seqsee", ["--gui", "--seq", "1 2"]),
    ("seqsee.gui", ["--seq", "1 2"]),
])
def test_missing_pyside6(mod, argv):
    """Without PySide6: a clear message on stderr (no traceback) and exit status 1."""
    code = _NO_PYSIDE6.format(mod=mod, argv=argv)
    r = subprocess.run([sys.executable, "-c", code], capture_output=True, text=True,
                       env=_env(), timeout=120)
    assert r.returncode == 1, r.stdout + r.stderr
    assert "PySide6" in r.stderr
    assert "pip install PySide6" in r.stderr
    assert "python3 -m seqsee --seq" in r.stderr          # the headless alternative
    assert "Traceback" not in r.stderr


def test_missing_pyside6_message(monkeypatch):
    monkeypatch.setitem(sys.modules, "PySide6", None)
    monkeypatch.delitem(sys.modules, "PySide6.QtWidgets", raising=False)
    msg = app.missing_pyside6()
    assert msg is not None and "PySide6" in msg
    err = io.StringIO()
    assert app.main(["--seq", "1 2"], err=err) == 1
    assert err.getvalue() == msg + "\n"


# ---- command line vs Seqsee.pl ---------------------------------------------------------------
@pytest.mark.parametrize("case", _cases("options"), ids=lambda c: " ".join(c["argv"]) or "none")
def test_options_match_perl(case):
    """The GUI reads the command line as Seqsee.pl does."""
    args = app.parse_args(case["argv"], check_config=False)
    for key in OPTION_KEYS:
        assert _norm(args.options[key]) == _norm(case["options"][key]), key
    if case["seed"] == "random":
        assert "seed" not in args.window_options       # the field stays empty
    else:
        assert args.window_options["seed"] == case["seed"]
        assert args.options["seed"] == case["seed"]
    assert sorted(args.runner_defaults["features"]) == case["features"]
    assert args.rest == case["rest"]
    assert args.seq == " ".join(case["options"]["seq"])


def test_window_options():
    """Only what the command line gives goes to the window (it beats the remembered
    settings); update_interval always goes to the runner."""
    args = app.parse_args(["--seq", "1, 2, 3", "--seed", "7", "--max-steps", "50",
                           "--update-interval", "5", "--view", "1"])
    assert args.window_options == {"seq": "1 2 3", "seed": 7, "max_steps": 50, "view": 1,
                                   "update_interval": 5, "gui_config": "GUI_sparse"}
    assert args.runner_defaults["update_interval"] == 5
    bare = app.parse_args([])
    assert set(bare.window_options) == {"seq", "update_interval", "gui_config"}
    assert bare.seq == ""
    assert _norm(bare.runner_defaults["update_interval"]) == "15"   # config/seqsee.conf
    n = app.parse_args(["-n", "30"])
    assert n.window_options["max_steps"] == 30


@pytest.mark.parametrize("case", _cases("setup"), ids=lambda c: c["gui_config"])
def test_gui_config_like_sgui_setup(case):
    """SGUI::setup reads config/<gui_config>.conf and dies without one or without [frames]
    (GUI_ws3.conf is the drawing modules' layout, not a window layout)."""
    if case["died"]:
        with pytest.raises(cli.UsageError) as e:
            app.check_gui_config(case["gui_config"])
        if "config/" in case["error"]:
            assert f"config/{case['gui_config']}.conf" in str(e.value)
        else:
            assert "[frames]" in str(e.value)
    else:
        app.check_gui_config(case["gui_config"])


@pytest.mark.parametrize("argv,text", [
    (["--seq", "1 a"], "--seq"),
    (["--gui_config", "GUI_ws3"], "[frames]"),
    (["--gui", "nosuch"], "config/nosuch.conf"),
    (["--json"], "headless"),
    (["--continuation", "4"], "headless"),
    (["--answer", "no"], "headless"),
    (["-f", "NoSuchFeature"], "NoSuchFeature"),
])
def test_usage_errors(argv, text):
    with pytest.raises(cli.UsageError) as e:
        app.parse_args(argv)
    assert text in str(e.value)
    err = io.StringIO()
    assert app.main(argv, err=err) == 2
    assert err.getvalue().startswith("seqsee: ") and text in err.getvalue()


def test_gui_switch_stripped():
    """``python3 -m seqsee.gui --gui`` works too; ``--gui X`` names the config."""
    assert app.parse_args(["--gui", "--seq", "1 2"]).seq == "1 2"
    assert app.parse_args(["--gui", "GUI_sparse"]).options["gui_config"] == "GUI_sparse"


# ---- the window (Qt) -------------------------------------------------------------------------
qt = pytest.mark.gui


def _wait_command(qtbot, runner, name, timeout=20000):
    done = []
    runner.command_done.connect(lambda n, r: done.append(n))
    return lambda: qtbot.waitUntil(lambda: name in done, timeout=timeout)


@pytest.fixture
def launched(qtbot):
    made = []

    def launch(argv, settings=None):
        pytest.importorskip("PySide6.QtWidgets")
        args = app.parse_args(argv)
        from seqsee.gui.runner import Runner
        runner = Runner()
        wait = _wait_command(qtbot, runner, "new_sequence")
        window = app.build(args, settings=settings, runner=runner)
        qtbot.addWidget(window)
        made.append(window)
        return window, runner, wait

    yield launch
    for w in made:
        w.close()
        if w.runner is not None:
            w.runner.quit(timeout=5)


@qt
def test_launch_matches_perl(qtbot, launched):
    """Seqsee.pl --seq "1 1 2 1 2 3" --seed 7 ...: INITIALIZE with the sequence, no steps,
    no ask_seq; the fields and view from the command line."""
    case = _cases("launch")[0]
    window, runner, wait = launched(case["argv"])
    wait()
    qtbot.waitUntil(lambda: window.snapshot is not None, timeout=5000)
    for key in OPTION_KEYS:
        assert _norm(runner.options[key]) == _norm(case["options"][key]), key
    assert runner.options["seed"] == case["seed"]
    assert window.seed() == case["seed"]
    assert window.max_steps() == case["options"]["max_steps"]
    assert window.view == case["options"]["view"]
    assert window.last_sequence == "1 1 2 1 2 3"
    assert [e.mag for e in window.snapshot.elements] == case["elements"]
    assert Global.Steps_Finished == case["steps"]
    assert (window.seq_dialog is not None) == bool(case["ask_seq"])
    assert window.runner is runner and runner.is_alive()


@qt
def test_launch_without_seq_asks(qtbot, launched):
    """No --seq: SGUI::ask_seq's window opens; the run starts when a sequence is accepted."""
    case = _cases("launch_no_seq")[0]
    window, runner, _ = launched(case["argv"])
    assert case["ask_seq"] == 1
    assert window.seq_dialog is not None and window.seq_dialog.isVisible()
    assert window.seed() == case["seed"]
    assert runner.options is None


@qt
def test_launch_runs(qtbot, launched):
    """The runner drives the model with the command line's update_interval and max_steps."""
    window, runner, wait = launched(["--seq", "1 2 3", "--seed", "3", "--max-steps", "4",
                                     "--update-interval", "2"])
    wait()
    assert runner.options["update_interval"] == 2
    done = _wait_command(qtbot, runner, "continue")
    window.run_command("continue")
    done()
    assert Global.Steps_Finished == 4


@qt
def test_launch_features(qtbot, launched):
    """-f features survive the fresh run's reset (Seqsee.pl turns them on once, at start);
    -f debugMAX checks the m action."""
    window, runner, wait = launched(["-f", "Primes", "-f", "debugMAX", "--seq", "7 11"])
    wait()
    assert Global.Feature.get("Primes") and Global.Feature.get("debugMAX")
    assert window.debug_max == 1 and Global.debugMAX == 1
    assert window.run_actions["m"].isChecked()


@qt
def test_main_shows_window_and_remembers(qtbot, tmp_path):
    """main(): the window is shown, the event loop runs (here a stand-in), the settings are
    saved on close; the default settings are the user's (mainwindow.default_settings)."""
    pytest.importorskip("PySide6.QtWidgets")
    from PySide6.QtCore import QSettings
    from seqsee.gui.qt import mainwindow
    path = tmp_path / "seqsee.ini"
    seen = {}

    def run(qapp, window):
        seen["visible"] = window.isVisible()
        seen["title"] = window.windowTitle()
        seen["seed"] = window.seed()
        window.close()
        return 0

    settings = QSettings(str(path), QSettings.IniFormat)
    assert app.main(["--seq", "1 2", "--seed", "11"], settings=settings, run=run) == 0
    assert seen == {"visible": True, "title": "Seqsee", "seed": 11}
    assert QSettings(str(path), QSettings.IniFormat).value("seed") == "11"

    used = []
    orig = mainwindow.default_settings

    def fake_default():
        s = QSettings(str(tmp_path / "default.ini"), QSettings.IniFormat)
        used.append(s)
        return s

    mainwindow.default_settings = fake_default
    try:
        assert app.main(["--seq", "1 2"], run=lambda qapp, w: w.close() and 0) == 0
    finally:
        mainwindow.default_settings = orig
    assert len(used) == 1 and (tmp_path / "default.ini").exists()


@qt
def test_entry_screenshot(qtbot, launched, tmp_path, request):
    """The window as launched by ``python3 -m seqsee --gui --seq "1 1 2 1 2 3" --seed 7
    --max-steps 40 --update-interval 5 --view 0`` (Perl: docs/gui/perl/entry_window.png).
    ``pytest --write-screens`` saves it under docs/gui/screens/."""
    out_dir = SCREENS_DIR if request.config.getoption("--write-screens") else tmp_path
    window, runner, wait = launched(_cases("launch")[0]["argv"])
    wait()
    qtbot.waitUntil(lambda: window.snapshot is not None, timeout=5000)
    window.show()
    qtbot.wait(30)
    path = out_dir / "entry_window.png"
    assert window.grab().save(str(path), "PNG")

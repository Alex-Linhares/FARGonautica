"""Loop0004 item 16: the command line and the help, offscreen (`seekwhence/gui/app.py`,
`MainWindow.show_guide`, `show_about`; G-055; Numbo GUI, `app.py`).

- `--smoke --preset run1_3737` prints the line the CLI prints for seed 54, and exits 0;
  `--smoke --terms 3 7 3 7 --seed 54 --answers ok` prints the same; a capped run exits 1.
- A bad argument (a seed that is not a number, an unknown preset, an unknown option) exits
  2 with argparse's usage error and no traceback.
- `apply_arguments` sets the controls: the arguments given win over the remembered ones,
  `--smoke` takes the defaults for the rest; `--play` starts the run.
- Help > What am I looking at? lists every pane with its pages and marks P5 and P6; Help >
  About names the engine's version.
"""

import os
import subprocess
import sys
from pathlib import Path

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from PySide6.QtCore import QSettings                                    # noqa: E402
from PySide6.QtWidgets import QDialog, QTextBrowser                     # noqa: E402

from seekwhence.gui import app as gui_app                               # noqa: E402
from seekwhence.gui.main_window import PANES, about_text, title_for     # noqa: E402
from seekwhence.gui.models import controls as cm                        # noqa: E402
from seekwhence.gui.models import guide, presets                        # noqa: E402

PYTHON_DIR = Path(__file__).resolve().parents[2]
TIMEOUT = 240


def env():
    e = dict(os.environ, PYTHONPATH=str(PYTHON_DIR), PYTHONDONTWRITEBYTECODE="1",
             QT_QPA_PLATFORM="offscreen")
    e.pop("WAYLAND_DISPLAY", None)
    return e


def run(*args):
    return subprocess.run([sys.executable, "-m", "seekwhence.gui", *args], cwd=PYTHON_DIR,
                          env=env(), capture_output=True, text=True, timeout=TIMEOUT)


def cli(*args):
    return subprocess.run([sys.executable, "-m", "seekwhence", *args], cwd=PYTHON_DIR,
                          env=env(), capture_output=True, text=True, timeout=TIMEOUT)


# -- --smoke -------------------------------------------------------------------------------

def test_smoke_of_run_1_prints_the_line_the_cli_prints_for_seed_54():
    p = run("--smoke", "--preset", "run1_3737")
    expected = cli("--seed", "54", "--terms", "3", "7", "3", "show-hypothesis", "7",
                   "--answers", "ok")
    assert p.returncode == 0, p.stderr
    assert p.stdout == expected.stdout.splitlines()[-1] + "\n"
    assert p.stdout.startswith("outcome: ok after 4 terms, 52 tasks (seed 54): (Cycle 3 2 (3 7))")
    assert p.stderr == ""                                                   # [M86 p. 166]


def test_smoke_with_terms_seed_and_answers_equals_the_cli():
    p = run("--smoke", "--terms", "3", "7", "3", "7", "--seed", "54", "--answers", "ok")
    expected = cli("--seed", "54", "--terms", "3", "7", "3", "7", "--answers", "ok")
    assert (p.returncode, p.stdout) == (0, expected.stdout.splitlines()[-1] + "\n")


def test_smoke_of_a_capped_run_exits_1():
    p = run("--smoke", "--preset", "blackburn-1", "--seed", "1", "--max-steps", "300")
    assert p.returncode == 1, p.stdout + p.stderr
    assert p.stdout.startswith("outcome: capped after") and "(seed 1)" in p.stdout


@pytest.mark.parametrize("args", [("--smoke", "--seed", "many"),
                                  ("--smoke", "--preset", "no-such-list"),
                                  ("--bogus",),
                                  ("--preset", "run1_3737", "--terms", "3", "7")])
def test_a_bad_argument_exits_2(args):
    p = run(*args)
    assert p.returncode == 2, p.stdout + p.stderr
    assert "usage:" in p.stderr and "Traceback" not in p.stderr and p.stdout == ""


# -- the arguments and the window ------------------------------------------------------------

def test_the_arguments_given_win_over_the_remembered_ones(window):
    window.choose_preset("run3_121231234")
    assert window.inputs.preset == "run3_121231234" and window.inputs.seed == 30
    args = gui_app.parser().parse_args(["--seed", "7"])
    inputs = gui_app.apply_arguments(window, args)
    assert (inputs.preset, inputs.seed) == ("run3_121231234", 7)          # the preset remembered
    assert window.controls.values().seed == "7" and window.inputs == inputs
    args = gui_app.parser().parse_args(["--preset", "blackburn-2", "--max-steps", "500"])
    inputs = gui_app.apply_arguments(window, args)
    assert (inputs.preset, inputs.seed, inputs.config.max_tasks) == ("blackburn-2", 1, 500)
    assert window.controls.preset == "blackburn-2" and window.windowTitle() == title_for(inputs)
    args = gui_app.parser().parse_args(["--terms", "1", "2", "1", "2", "--answers", "no", "ok"])
    inputs = gui_app.apply_arguments(window, args)
    assert inputs.preset == cm.CUSTOM and inputs.lines == ("1", "2", "1", "2")
    assert inputs.answers == ("no", "ok") and window.controls.values().terms == "1 2 1 2"
    with pytest.raises(cm.InputError):
        gui_app.apply_arguments(window, gui_app.parser().parse_args(["--seed", "x"]))


def test_smoke_takes_the_defaults_for_the_arguments_not_given(window):
    window.choose_preset("run2_373373")
    window.controls.seed_edit.setText("99")
    args = gui_app.parser().parse_args(["--smoke"])
    inputs = gui_app.apply_arguments(window, args)
    assert inputs == presets.appendix_inputs(presets.DEFAULT_PRESET)
    assert inputs.seed == 54 and window.controls.preset == presets.DEFAULT_PRESET


def test_play_starts_the_run(qtbot, window):
    args = gui_app.parser().parse_args(["--preset", "run1_3737", "--play"])
    gui_app.apply_arguments(window, args)
    window.controller.set_delay(0)
    window.play()
    qtbot.waitUntil(lambda: window.outcome is not None, timeout=60000)
    assert window.outcome.result["outcome"] == "ok" and not window.controller.is_alive()
    assert gui_app.smoke_line(window) == (window.outcome.result["summary"], 0)


# -- Help -----------------------------------------------------------------------------------

def test_the_help_page_lists_every_pane_with_its_pages(qtbot, window):
    dialog = window.show_guide()
    qtbot.addWidget(dialog)
    assert isinstance(dialog, QDialog) and dialog.isVisible()
    html = dialog.findChild(QTextBrowser).toHtml()
    text = dialog.findChild(QTextBrowser).toPlainText()
    rows = guide.read_guide()
    assert [r.pane for r in rows] == [f"P{n}" for n in range(1, 14)]
    for r in rows:
        assert f"{r.pane} {r.name}" in text and r.pages in text, r.pane
    assert text.count(guide.GUI_DESIGN) == 3                        # the note, P5 and P6
    for _a, _n, title, pane, pages, *_ in PANES:                    # every dock's pages too
        assert f"{pane} {title}" in text
    assert "P5 Platoplasm" in text and "P6 Taskrack" in text and "font-weight" in html
    dialog.close()


def test_about_names_the_engine_and_the_sources():
    import seekwhence
    assert seekwhence.__version__ in about_text()
    assert "Meredith" in about_text() and "What am I looking at?" in about_text()

"""The Qt GUI's launcher: the problem the command line starts with (port).

`python3 -m metacat.qt` starts with app.DEFAULT_PROBLEM (Run 7) in the command line,
`python3 -m metacat.qt WORD...` with the words given, and `--empty` with nothing, as
the original's control panel does.  Each case runs the real program in a subprocess,
offscreen, with a scratch settings file.
"""
import os
import subprocess
import sys
from pathlib import Path

import pytest

pytest.importorskip("PySide6")

PYTHON_DIR = Path(__file__).resolve().parents[1]

SPY = """
import sys
from metacat.qt import app
original = app.prefill_command_line
def spy(text):
    original(text)
    from metacat import setup as S
    line = S.g_control_panel.command_line
    print("LINE", repr(line.text()), "FOCUS", line.hasFocus(), flush=True)
app.prefill_command_line = spy
sys.exit(app.main(sys.argv[1:]))
"""


def run_launcher(tmp_path, *args):
    env = dict(os.environ, QT_QPA_PLATFORM="offscreen")
    env.pop("WAYLAND_DISPLAY", None)
    proc = subprocess.run(
        [sys.executable, "-c", SPY, "--quit-after", "1500",
         "--settings", str(tmp_path / "layout.ini"), *args],
        cwd=PYTHON_DIR, env=env, capture_output=True, text=True, timeout=120)
    assert proc.returncode == 0, proc.stdout[-2000:] + proc.stderr[-2000:]
    return [line for line in proc.stdout.splitlines() if line.startswith("LINE ")]


def test_the_command_line_starts_with_run_7(tmp_path):
    from metacat.qt.app import DEFAULT_PROBLEM
    assert DEFAULT_PROBLEM == "abc abd xyz 3852097033"
    assert run_launcher(tmp_path) == ["LINE 'abc abd xyz 3852097033' FOCUS True"]


def test_the_words_given_replace_the_default(tmp_path):
    assert run_launcher(tmp_path, "abc", "abd", "ijk", "7") == ["LINE 'abc abd ijk 7' FOCUS True"]


def test_empty_leaves_the_command_line_as_the_original_does(tmp_path):
    assert run_launcher(tmp_path, "--empty") == []

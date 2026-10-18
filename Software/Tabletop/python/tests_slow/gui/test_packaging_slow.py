"""The slow tests of `tests/gui/test_packaging.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_packaging import (bare_environment, cli_last_line, pytest, run, sys, with_pyside)

pytestmark = pytest.mark.slow


def test_the_smoke_command_prints_the_clis_last_line_and_exits_0():
    """Item 16 (G-054; ANSWERS.md 1): `--smoke --problem surround-05 --seed 3` runs to
    the end through the window and prints French's report line, the last line of
    `python3 -m tabletop surround-05 --seed 3 --quiet`."""
    pytest.importorskip("PySide6")
    p = run([sys.executable, "-m", "tabletop.gui", "--smoke", "--problem", "surround-05",
             "--seed", "3"], with_pyside())
    assert p.returncode == 0, p.stderr
    assert p.stdout == "Eliza touches cup2 (E2). Structure 72. Codelets 384. T 28.3.\n", p.stdout   # v5's fix, D-116 to D-120 (cup2 (E2), structure 88, 305 codelets, T 17.8 in v4, D-115; small-glass1 (E1), 144 codelets, T 31.0 in v3)
    assert p.stdout.rstrip("\n") == cli_last_line("surround-05", 3)
    assert p.stderr == "", p.stderr          # the offscreen plugin's warnings are dropped


def test_the_smoke_command_with_the_defaults_from_a_bare_environment():
    """Only PATH, HOME and LANG (Numbo GUI, `test_packaging.py`): a qt.conf next to the
    Python executable must not send Qt 6 to another Qt's plugins; the defaults are
    `surround-05` seed 1 under `standard` (G-044, G-054). A whole run through the window
    in a subprocess, then the CLI's: the capped form stays in the fast suite."""
    pytest.importorskip("PySide6")
    p = run([sys.executable, "-m", "tabletop.gui", "--smoke"], bare_environment())
    assert p.returncode == 0, p.stderr
    assert p.stdout.rstrip("\n") == cli_last_line("surround-05", 1) and p.stderr == ""

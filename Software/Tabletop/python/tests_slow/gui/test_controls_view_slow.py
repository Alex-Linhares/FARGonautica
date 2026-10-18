"""The slow tests of `tests/gui/test_controls_view.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_controls_view import (SHORT, cm, pytest, wait_finished, window)

pytestmark = pytest.mark.slow


def test_the_same_seed_repeats_the_run(window, qtbot):
    c = window.controls_view
    c.problem_combo.setCurrentIndex(c.problem_combo.findData(SHORT[0]))
    c.seed_edit.setText(str(SHORT[1]))
    c.seed_edit.editingFinished.emit()
    runs = []
    for _ in range(2):
        c.run_to_end_button.click()
        wait_finished(qtbot, window)
        runs.append((list(window.history.records), window.outcome.result.draws))
    assert runs[0] == runs[1] and window.seed == SHORT[1]
    assert window.stats_view.text("report") == (
        "Eliza touches spoon2 (E1). Structure 69. Codelets 162. T 10.8.")   # buridan-03 seed 2 since v5's fix, D-116 to D-120 (spoon2 (E1), structure 59, 171 codelets, T 10.3 in v4, D-115; spoon2 (E1), 125 codelets, T 25.9 in v3, D-114; cup3 (none), 84 codelets since item 09 (small-glass1 (none), 46 codelets at items 07 and 08; spoon2 (E1), 66 codelets at item 05)

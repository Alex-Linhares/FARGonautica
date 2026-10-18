"""The slow tests of `tests/gui/test_palette_view.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_palette_view import (
    P,
    PROFILES,
    QTest,
    TIMEOUT,
    build,
    pv,
    pytest,
    run_problem,
    shown,
    title_for,
    wait_finished,
    window,
)

pytestmark = pytest.mark.slow


def test_do_this_runs_the_table_through_the_window(window, qtbot):
    pane = window.palette_view
    build(pane.editor)
    html = pane.messages_label.text()
    assert "a table French would set" in html and pv.GOOD in html
    problem = pane.editor.problem()
    assert window.run_edited_table()
    assert window.started == ("edited", "standard", 3)
    outcome = wait_finished(qtbot, window)
    plain = run_problem(problem, PROFILES["standard"], 3)
    assert outcome.result == plain.__class__(**{**plain.__dict__}) and outcome.result == plain
    assert window.history.records[0]["problem"] == "edited"
    assert window.windowTitle() == title_for("edited", "standard", 3)
    assert window.stats_view.texts()["run"] == "edited, seed 3"
    assert window.table_view.view.model.layout().objects[0].id == "cup1"
    # The session header stores the table as its dict (G-048).
    header = window.session_header()
    assert isinstance(header.problem, dict) and header.problem["name"] == "edited"
    assert P.validate(header.problem) == []
    # Run it again: the next seed, through the button.
    pane.buttons["again"].click()
    qtbot.waitUntil(lambda: window.started == ("edited", "standard", 4), timeout=TIMEOUT)
    outcome = wait_finished(qtbot, window)
    assert outcome.result == run_problem(problem, PROFILES["standard"], 4)
    assert window.seed == 4
    assert pane.edit_line.text().startswith("editing:")

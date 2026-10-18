"""The slow tests of `tests/gui/test_results_model.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

pytest.importorskip("PySide6")
pytest.importorskip("pytestqt")

from test_results_model import (
    STANDARD,
    batch_through_worker,
    load_problem,
    pytest,
    rm,
    run_batch,
)

pytestmark = pytest.mark.slow


@pytest.mark.slow
def test_a_batch_of_fifty_equals_run_batch():
    problem = load_problem("surround-05")
    model = rm.BatchModel()
    batch_through_worker(model, problem, STANDARD, 1, 50)
    assert model.results == run_batch(problem, STANDARD, 1, 50)
    assert sum(r.count for r in model.rows()) == 50 and model.survey().runs == 50

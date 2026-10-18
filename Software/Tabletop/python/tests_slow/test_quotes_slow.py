"""The slow tests of `tests/test_quotes.py`, moved here so that the default suite stays
fast (ANSWERS.md D2): each takes more than about two seconds on its own, or is a
multi-seed survey. The quick checks of the same features stay in that file, which
this one imports its helpers and fixtures from. Run with `python3 -m pytest -q tests_slow`.
"""

import pytest

from test_quotes import (needs_f95, needs_pdftotext, run_checker)

pytestmark = pytest.mark.slow


@needs_f95
@needs_pdftotext
def test_page_and_find():
    """`--page F95 55` prints that page's text layer; `--find F95 "words"` names the page."""
    proc = run_checker("--page", "F95", "55")
    assert proc.returncode == 0 and "three main components" in proc.stdout
    proc = run_checker("--find", "F95", "consists of three main components")
    assert proc.returncode == 0 and f"[F95 p. {55}]" in proc.stdout

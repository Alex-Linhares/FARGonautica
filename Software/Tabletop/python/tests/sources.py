"""The one place the tests look for the source PDFs, shared with `tools/check_quotes.py`
(ANSWERS.md 6: the sources are found only through the environment or an ancestor checkout).

F95 and FH91 come from `$F95_PDF` / `$FH91_PDF`, else `Literature/` of `$FARGONAUTICA`, else
`Literature/` of the nearest ancestor directory holding `.git` and `Literature/`; FCCA only
from `$FCCA_PDF`. A test that needs a PDF skips, with a reason naming the variable, when it is
not found: `pytest.mark.skipif(not available(TAG), reason=skip_reason(TAG))`.

The lookup itself lives in the checker; this helper loads it by path (no sys.path changes)
so that the tests and the checker can never disagree about where a source is.
"""

import importlib.util
from pathlib import Path

PYTHON_DIR = Path(__file__).resolve().parents[1]      # Software/Tabletop/python
TARGET = PYTHON_DIR.parent                             # Software/Tabletop
CHECKER = PYTHON_DIR / "tools" / "check_quotes.py"


def load_checker():
    """The `tools/check_quotes.py` module, imported from its file."""
    spec = importlib.util.spec_from_file_location("tabletop_check_quotes", CHECKER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


checker = load_checker()
TAGS = checker.TAGS
source_path = checker.source_path
available = checker.available
skip_reason = checker.skip_reason
PDFTOTEXT_MISSING = checker.shutil.which("pdftotext") is None

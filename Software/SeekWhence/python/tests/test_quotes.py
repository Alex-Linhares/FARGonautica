"""Every quotation followed by a citation tag under Software/SeekWhence/ is found on the page
it cites (tools/check_quotes.py; the tags are [M86 p. N], [FCCA p. N], [MHN p. N])."""

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[1]      # Software/SeekWhence/python
TARGET = ROOT.parent                             # Software/SeekWhence
CHECKER = ROOT / "tools" / "check_quotes.py"


def sources_found() -> bool:
    """Meredith's PDFs are in FARGonautica's Literature/: found when this folder is inside a
    FARGonautica clone, or when $FARGONAUTICA names one (the standalone Seek-Whence
    repository)."""
    if os.environ.get("FARGONAUTICA"):
        return True
    return any((d / ".git").exists() and (d / "Literature").is_dir()
               for d in Path(__file__).resolve().parents)


@pytest.mark.skipif(shutil.which("pdftotext") is None, reason="pdftotext (poppler) is missing")
@pytest.mark.skipif(not sources_found(), reason="the sources are not found: set FARGONAUTICA "
                                                "to a clone of FARGonautica")
def test_every_checked_quotation_is_on_its_page():
    proc = subprocess.run([sys.executable, str(CHECKER), str(TARGET)],
                          capture_output=True, text=True, timeout=600)
    output = proc.stdout + proc.stderr
    assert proc.returncode == 0, output
    assert "quotations:" in output


def test_checker_is_the_loop_copy():
    """tools/check_quotes.py is a copy of the loop's scripts/check_quotes.py; if the loop's
    copy is present, the two must agree (never weaken the checker)."""
    loop_copy = ROOT.parents[2] / "ralph_loops" / "loop0003" / "scripts" / "check_quotes.py"
    if not loop_copy.exists():
        pytest.skip("the loop folder is not present")
    assert CHECKER.read_text() == loop_copy.read_text()

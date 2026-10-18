"""Every quotation followed by a citation tag under Software/Tabletop/ is found on the page it
cites (`tools/check_quotes.py`; the tags are [F95 p. N], [FCCA p. N], [FH91 p. N]), and the
checker itself passes a true quotation and fails a made-up one (TASK.md, "Quotations are
checked by machine": never weaken the checker).

The tests that read a PDF skip, naming the variable to set, when it is not on this machine
(`tests/sources.py`). FCCA is checked only when `FCCA_PDF` is set; without it the checker
reports its quotations as SKIP, and `test_fcca_quotations_are_checked` is skipped too.
"""

import subprocess
import sys

import pytest

from sources import CHECKER, PDFTOTEXT_MISSING, TARGET, available, checker, skip_reason

needs_f95 = pytest.mark.skipif(not available("F95"), reason=skip_reason("F95"))
needs_fh91 = pytest.mark.skipif(not available("FH91"), reason=skip_reason("FH91"))
needs_fcca = pytest.mark.skipif(not available("FCCA"), reason=skip_reason("FCCA"))
needs_pdftotext = pytest.mark.skipif(PDFTOTEXT_MISSING, reason="pdftotext (poppler) is missing")

GOOD = ("Tabletop, like Copycat, consists of three main components: the Slipnet, the "
        "Workspace, and the Coderack.")
BAD = "The Coderack was stored as a balanced binary heap of codelets sorted by their age."


def tagged(quote: str, tag: str, page: int) -> str:
    """A quotation followed by its tag, assembled at run time so that this file's own text
    holds no tagged quotation for the checker to find."""
    return f'"{quote}" [{tag} p. {page:d}]'


def run_checker(*args: str) -> subprocess.CompletedProcess:
    return subprocess.run([sys.executable, str(CHECKER), *args], capture_output=True,
                          text=True, timeout=600)


def test_limits_are_the_agreed_ones():
    """The gate reads these literals and refuses a weaker checker."""
    assert checker.MIN_WORDS <= 6 and checker.MAX_WORDS <= 80 and checker.THRESHOLD >= 0.90


@needs_f95
@needs_fh91
def test_every_checked_quotation_is_on_its_page():
    proc = run_checker(str(TARGET))
    output = proc.stdout + proc.stderr
    assert proc.returncode == 0, output
    assert "quotations:" in output
    assert "FAIL" not in output


@needs_fcca
def test_fcca_quotations_are_checked():
    """With FCCA_PDF set, no FCCA quotation is skipped (the F95 and FH91 quotations may
    be, where the suite runs outside a checkout: a fresh copy under /tmp)."""
    proc = run_checker(str(TARGET))
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "source FCCA not found" not in proc.stdout
    assert not [l for l in proc.stdout.splitlines() if l.startswith("SKIP") and "FCCA" in l]


@needs_f95
def test_a_true_quotation_passes_and_a_made_up_one_fails(tmp_path):
    good = tmp_path / "good.md"
    good.write_text("French writes " + tagged(GOOD, "F95", 55) + ".\n")
    assert run_checker(str(good)).returncode == 0
    bad = tmp_path / "bad.md"
    bad.write_text("French never wrote " + tagged(BAD, "F95", 55) + ".\n")
    proc = run_checker(str(bad))
    assert proc.returncode == 1 and "NOT FOUND" in proc.stdout
    wrong_page = tmp_path / "wrong.md"
    wrong_page.write_text("Right words, wrong page: " + tagged(GOOD, "F95", 120) + ".\n")
    assert run_checker(str(wrong_page)).returncode == 1


@needs_f95
def test_long_quotations_and_curly_quotes_and_ellipses(tmp_path):
    long = tmp_path / "long.md"
    long.write_text(tagged(" ".join(["word"] * (checker.MAX_WORDS + 1)), "F95", 55) + "\n")
    proc = run_checker(str(long))
    assert proc.returncode == 1 and "TOO LONG" in proc.stdout
    curly = tmp_path / "curly.md"
    curly.write_text("“" + GOOD + "” [F95 p. 55]\n")
    assert run_checker(str(curly)).returncode == 0
    split = tmp_path / "split.md"
    split.write_text(tagged("Tabletop, like Copycat, consists of three main components ... "
                            "the Slipnet, the Workspace, and the Coderack", "F95", 55) + "\n")
    assert run_checker(str(split)).returncode == 0


@needs_f95
def test_short_parts_of_a_split_quotation_are_checked(tmp_path):
    """An ellipsis-split quotation whose parts are all under MIN_WORDS words is still
    checked, part by part, once the whole has MIN_WORDS words (the citation audit closed
    this gap): a true one passes, one with a made-up part fails."""
    true = tmp_path / "true.md"
    true.write_text(tagged("Tabletop, like Copycat ... the Slipnet, the Workspace ... "
                           "this completes the run", "F95", 55) + "\n")
    assert run_checker(str(true)).returncode == 0
    false = tmp_path / "false.md"
    false.write_text(tagged("Tabletop, like Copycat ... a balanced binary heap ... "
                            "this completes the run", "F95", 55) + "\n")
    proc = run_checker(str(false))
    assert proc.returncode == 1 and "NOT FOUND" in proc.stdout
    assert checker.PART_MIN_WORDS <= 3


def test_without_sources_every_quotation_is_skipped(tmp_path, monkeypatch):
    """The standalone repository without the PDFs: SKIP lines, exit 0."""
    for var in ("FARGONAUTICA", "F95_PDF", "FH91_PDF", "FCCA_PDF"):
        monkeypatch.delenv(var, raising=False)
    monkeypatch.setenv("FARGONAUTICA", str(tmp_path / "nowhere"))
    probe = tmp_path / "probe.md"
    probe.write_text(tagged(GOOD, "F95", 55) + "\n" + tagged(BAD, "FCCA", 323) + "\n")
    proc = run_checker(str(probe))
    assert proc.returncode == 0, proc.stdout
    assert proc.stdout.count("SKIP") == 2
    assert "F95_PDF" in proc.stdout and "FCCA_PDF" in proc.stdout


def test_f95_page_map_follows_the_markers():
    """The PDF skips blank pages, so printed p. 55 is PDF p. 66 but printed p. 115 is
    PDF p. 125 (TASK.md's offset table); the map comes from the "Page N" markers."""
    if not available("F95"):
        pytest.skip(skip_reason("F95"))
    assert checker.pdf_pages("F95", 1) == [14]
    assert checker.pdf_pages("F95", 55) == [66]
    assert checker.pdf_pages("F95", 115) == [125]
    assert checker.pdf_pages("F95", 190) == [198]
    assert checker.pdf_pages("F95", 999) == []
    assert checker.pdf_pages("FCCA", 377) == [387]
    assert checker.pdf_pages("FH91", 181) == [7]

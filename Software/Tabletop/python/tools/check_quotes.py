#!/usr/bin/env python3
"""Check the quotations in the Tabletop reconstruction against the sources' text layers.

Standard library only; needs `pdftotext` (poppler) on PATH. Exit 0 = every checked quotation
was found on the page it cites (quotations whose source is not on this machine are reported
as SKIP and never fail).

    python3 check_quotes.py FILE_OR_DIR [...]        # check these files (.md and .py)
    python3 check_quotes.py --page F95 55            # print the text layer of printed p. 55
    python3 check_quotes.py --find F95 "some words"  # where do these words occur?

What is checked
---------------
A *checked quotation* is text in double quotes (straight "..." or curly), at least MIN_WORDS
words long, followed within a few characters by a citation tag:

    "Tabletop, like Copycat, consists of three main components ..." [F95 p. 55]
    "..." [F95 pp. 90-91]      "..." [FCCA p. 387]      "..." [FH91 p. 181]

Tags (the page is always the PRINTED page):
    F95   French 1995, The Subtlety of Sameness. The PDF skips blank pages, so the printed page
          is found by the "Page N" marker the text layer carries on every page.
    FCCA  Hofstadter & FARG 1995, Fluid Concepts and Creative Analogies. PDF = printed + 10.
    FH91  French & Hofstadter 1991 (the reprint, pp. 175-182). PDF = printed - 174.

Shorter quoted phrases (fewer than MIN_WORDS words) are not checked, so single terms ("cup",
"opposite") can be quoted freely. An ellipsis ("...") inside a quotation splits it: when the
whole quotation has MIN_WORDS words or more, each part of three words or more is checked
separately (shorter parts are skipped), so that a quotation cannot escape the check by being
cut into short pieces. Text in [square brackets]
inside a quotation (an editorial insertion) is dropped before matching. No quotation may be
longer than MAX_WORDS words (fair use: the books are read in place, never copied).

Matching is on a lossy canonical form (lowercase letters and digits only; i, l, 1 merged; the
"Page N" markers removed), against the cited pages plus one page either side (for page
breaks), and the quotation must match some window at THRESHOLD or better (a difflib ratio).
The limits MIN_WORDS, MAX_WORDS and THRESHOLD are module-level literals: the loop's gate reads
them and refuses a weaker checker.

Where the sources are found (ANSWERS.md 6: nothing here names a path outside this folder or
the environment):
    F95, FH91  $F95_PDF / $FH91_PDF if set; else Literature/ of $FARGONAUTICA (a FARGonautica
               checkout); else Literature/ of the nearest ancestor directory holding both .git
               and Literature/.
    FCCA       $FCCA_PDF only (FCCA is copyrighted and never in a repository).
A missing source skips its quotations (reported); it never fails them.
"""

import difflib
import os
import re
import shutil
import subprocess
import sys
from functools import lru_cache
from pathlib import Path

MIN_WORDS = 6
MAX_WORDS = 80
THRESHOLD = 0.90

TAGS = ("F95", "FCCA", "FH91")
F95_NAME = "Tabletop-French-1995-The Subtlety of Sameness.pdf"
FH91_NAME = "Tabletop-French, Hofstadter-1992.pdf"
OFFSET = {"FCCA": 10, "FH91": -174}       # PDF page = printed page + offset; F95 uses markers
VARIABLE = {"F95": "F95_PDF", "FCCA": "FCCA_PDF", "FH91": "FH91_PDF"}


# ----------------------------------------------------------------- finding the sources

def fargonautica(start: Path | None = None) -> Path | None:
    """The FARGonautica root: $FARGONAUTICA if set, else the nearest ancestor of `start` (this
    file by default) holding both .git and Literature/; None when there is none."""
    if os.environ.get("FARGONAUTICA"):
        return Path(os.environ["FARGONAUTICA"]).expanduser()
    for d in (start or Path(__file__)).resolve().parents:
        if (d / ".git").exists() and (d / "Literature").is_dir():
            return d
    return None


def source_path(tag: str, start: Path | None = None) -> Path | None:
    """Where the PDF of `tag` should be: the override variable, else FARGonautica's
    Literature/ (F95, FH91 only); None when nothing names it."""
    if os.environ.get(VARIABLE[tag]):
        return Path(os.environ[VARIABLE[tag]]).expanduser()
    name = {"F95": F95_NAME, "FH91": FH91_NAME}.get(tag)
    root = fargonautica(start)
    return root / "Literature" / name if root and name else None


def available(tag: str) -> bool:
    """True when the PDF of `tag` is on this machine and pdftotext can read it."""
    path = source_path(tag)
    return bool(shutil.which("pdftotext")) and path is not None and path.is_file()


def skip_reason(tag: str) -> str:
    """Why the quotations of `tag` cannot be checked here (for SKIP lines and pytest skips)."""
    if not shutil.which("pdftotext"):
        return "pdftotext (poppler) is not on PATH"
    path = source_path(tag)
    if tag == "FCCA":
        return "FCCA is not available: set FCCA_PDF to a copy of the book"
    where = f" ({path})" if path else ""
    return (f"source {tag} not found{where}: set {VARIABLE[tag]}, or FARGONAUTICA to a "
            "FARGonautica checkout")


# --------------------------------------------------------------------- the text layers

@lru_cache(maxsize=None)
def pages_of(tag: str) -> list[str] | None:
    """The text layer of every PDF page of `tag` (index 0 = PDF page 1), or None."""
    if not available(tag):
        return None
    out = subprocess.run(["pdftotext", str(source_path(tag)), "-"], capture_output=True,
                         text=True, errors="replace").stdout
    return out.split("\f")


@lru_cache(maxsize=None)
def f95_map() -> dict[str, int]:
    """Printed page (as printed, "55" or "xv") -> PDF page, from F95's "Page N" markers."""
    pages = pages_of("F95") or []
    found = {}
    for pdf, text in enumerate(pages, 1):
        m = re.search(r"Page (\w+)", text)
        if m and m.group(1) not in found:
            found[m.group(1)] = pdf
    return found


def pdf_pages(tag: str, printed: int) -> list[int]:
    """The PDF page(s) of a printed page (the cited page only, not its neighbours)."""
    if tag == "F95":
        return [f95_map()[str(printed)]] if str(printed) in f95_map() else []
    return [printed + OFFSET[tag]]


def printed_of(tag: str, pdf: int) -> str:
    """The printed page number of a PDF page, for --find's report."""
    if tag == "F95":
        return next((k for k, v in f95_map().items() if v == pdf), "?")
    return str(pdf - OFFSET[tag])


def canon(s: str) -> str:
    """The lossy form both sides are compared in: letters and digits, i/l/1 merged, no
    "Page N" markers, soft hyphens and ligatures resolved."""
    s = re.sub(r"Page [0-9ivx]+", " ", s)
    s = s.lower().replace("­", "").replace("ﬁ", "fi").replace("ﬂ", "fl")
    s = re.sub(r"[il1|!]", "l", s)
    return re.sub(r"[^a-z0-9]", "", s)


@lru_cache(maxsize=None)
def page_text(tag: str, printed: int) -> str:
    """The canonical text of a printed page and its two neighbours."""
    pages = pages_of(tag) or []
    pdfs = sorted({p for d in (-1, 0, 1) for p in pdf_pages(tag, printed + d)
                   if 1 <= p <= len(pages)})
    return canon(" ".join(pages[p - 1] for p in pdfs))


def score(part: str, hay: str) -> float:
    """How well `part` matches the best window of `hay` (1.0 when it is a substring)."""
    c = canon(part)
    if not c:
        return 1.0
    if c in hay:
        return 1.0
    best, n = 0.0, len(c)
    step = max(1, n // 8)
    for i in range(0, max(1, len(hay) - n + 1), step):
        r = difflib.SequenceMatcher(None, c, hay[i:i + n]).ratio()
        if r > best:
            best = r
    return best


def found(part: str, tag: str, a: int, b: int) -> float:
    """The best score of `part` on printed pages a..b (each with its neighbours)."""
    return score(part, "".join(page_text(tag, p) for p in range(a, b + 1)))


# -------------------------------------------------------------------- the quotations

QUOTE = re.compile(r'"([^"\n][^"]{8,1500}?)"[^"\[\n]{0,12}\[(F95|FCCA|FH91)\s+pp?\.\s*(\d+)'
                   r'(?:\s*[-–]\s*(\d+))?\]')


def quotations(text: str):
    """(line, quotation, tag, first page, last page) for every tagged quotation in `text`.
    Curly quotes are read as straight ones; the quotation ends at the closing quote nearest
    the tag."""
    text = text.replace("“", '"').replace("”", '"')
    for m in QUOTE.finditer(text):
        a = int(m.group(3))
        b = int(m.group(4) or a)
        yield text.count("\n", 0, m.start()) + 1, m.group(1), m.group(2), a, b


PART_MIN_WORDS = 3   # the shortest part of an ellipsis-split quotation that is checked


def parts_of(quote: str) -> list[str]:
    """The parts of a quotation that are checked: insertions dropped, then split at ellipses.
    A quotation without an ellipsis is one part (checked when it has MIN_WORDS words; the
    caller tests that). With ellipses, every part of PART_MIN_WORDS words or more is checked
    once the whole has MIN_WORDS words, so an ellipsis-split quotation whose parts are all
    short is still checked part by part (the loop's citation audit closed that gap)."""
    quote = re.sub(r"\[[^\]]*\]", " ", quote)
    parts = [p.strip() for p in re.split(r"\.\s*\.\s*\.|…", quote)]
    if len(parts) == 1:
        return [p for p in parts if len(p.split()) >= MIN_WORDS]
    if len(quote.split()) < MIN_WORDS:
        return []
    return [p for p in parts if len(p.split()) >= PART_MIN_WORDS]


def check_text(text: str, where: str) -> tuple[int, int, int, list[str]]:
    """(checked, skipped, failed, messages) for one file's text."""
    checked = skipped = failed = 0
    msgs = []
    for line, quote, tag, a, b in quotations(text):
        words = len(quote.split())
        if words < MIN_WORDS:
            continue
        loc = f"{where}:{line}"
        if words > MAX_WORDS:
            failed += 1
            msgs.append(f"FAIL {loc}: TOO LONG, {words} words (the limit is {MAX_WORDS})")
            continue
        if pages_of(tag) is None:
            skipped += 1
            msgs.append(f"SKIP {loc}: {skip_reason(tag)}: {' '.join(quote.split())[:60]!r}")
            continue
        if not all(pdf_pages(tag, p) for p in (a, b)) or b < a:
            failed += 1
            msgs.append(f"FAIL {loc}: [{tag} p. {a}{'-' + str(b) if b != a else ''}] is not a "
                        "page of the document")
            continue
        for part in parts_of(quote):
            checked += 1
            s = found(part, tag, a, b)
            if s < THRESHOLD:
                failed += 1
                msgs.append(f"FAIL {loc}: NOT FOUND on [{tag} p. {a}"
                            f"{'-' + str(b) if b != a else ''}] (best match {s:.2f}): "
                            f"\"{' '.join(part.split())[:90]}\"")
    return checked, skipped, failed, msgs


def files_in(args: list[str]) -> list[Path]:
    """The .md and .py files named by the arguments (directories searched recursively)."""
    out = []
    for a in args:
        p = Path(a)
        if p.is_dir():
            out += sorted(x for x in p.rglob("*") if x.suffix in (".md", ".py") and x.is_file()
                          and "__pycache__" not in x.parts)
        elif p.is_file():
            out.append(p)
        else:
            raise SystemExit(f"check_quotes.py: no such file or directory: {a}")
    return out


def show_page(tag: str, printed: str) -> int:
    pages = pages_of(tag)
    if pages is None:
        print(f"SKIP: {skip_reason(tag)}")
        return 0
    for pdf in pdf_pages(tag, int(printed)):
        print(pages[pdf - 1])
        return 0
    print(f"printed page {printed} not found in {tag}")
    return 1


def find_words(tag: str, words: str) -> int:
    pages = pages_of(tag)
    if pages is None:
        print(f"SKIP: {skip_reason(tag)}")
        return 0
    for pdf, text in enumerate(pages, 1):
        s = score(words, canon(text))
        if s >= THRESHOLD:
            print(f"[{tag} p. {printed_of(tag, pdf)}] (PDF page {pdf}): match {s:.2f}")
    return 0


def main(argv: list[str]) -> int:
    if argv[:1] == ["--page"] and len(argv) == 3 and argv[1] in TAGS:
        return show_page(argv[1], argv[2])
    if argv[:1] == ["--find"] and len(argv) == 3 and argv[1] in TAGS:
        return find_words(argv[1], argv[2])
    if not argv or argv[0].startswith("-"):
        print(__doc__)
        return 2
    total = [0, 0, 0]
    for f in files_in(argv):
        c, s, x, msgs = check_text(f.read_text(errors="replace"), str(f))
        total = [total[0] + c, total[1] + s, total[2] + x]
        for msg in msgs:
            print(msg)
    print(f"quotations: {total[0]} checked, {total[1]} skipped (source missing), "
          f"{total[2]} failed")
    return 1 if total[2] else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

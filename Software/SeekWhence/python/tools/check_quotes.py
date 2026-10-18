#!/usr/bin/env python3
"""Check the quotations in the Seek-Whence reconstruction against the sources.

Standard library only; needs `pdftotext` (poppler) on PATH. Exit 0 = every checked
quotation was found on the page it cites (or the source is unavailable on this
machine, which is reported and skipped).

    python3 check_quotes.py FILE_OR_DIR [...]      # check these files (.md, .py)
    python3 check_quotes.py --find M86 "some words" # where do these words occur?
    python3 check_quotes.py --page M86 101         # print the text layer of printed p. 101

What is checked
---------------
A *checked quotation* is text in double quotes (straight "..." or curly “...”), at
least MIN_WORDS words long, followed within a few characters by a citation tag:

    "The stability of a gnoth is the difference ..." [M86 p. 101]
    "... cloud of potential hypotheses ..." [M86 pp. 89-90]
    "a somewhat sticky ball of rice" [MHN p. 5]
    "sameness glue" ... [FCCA p. 58]

Tags (page numbers are the PRINTED page, except MHN):
    M86   Meredith (1986), the thesis. Arabic body pages: PDF = printed + 12.
          Roman front matter (iv-xiii): PDF = roman value - 1.
    FCCA  Hofstadter & FARG (1995). PDF = printed + 10 (chapter 1 is pp. 13-86).
    MHN   Meredith's historical notes (2017). No printed numbers: the PDF page.

Shorter quoted phrases (fewer than MIN_WORDS words) are not checked, so quote
single terms ("glom", "reigning class") freely. An ellipsis ("...", "…") inside a
quotation splits it: each part is checked separately (parts under MIN_WORDS words
are skipped). Text in square brackets inside a quotation (an editorial insertion)
is dropped before matching.

Matching is tolerant of the thesis's OCR errors (see the reading notes: w read as
v, a as "e.", b as "l>", digits for letters, ...): both sides are reduced to a
lossy canonical form (lowercase letters and digits only, with confusable
characters merged), and the quotation must match some window of the cited pages
(plus one page either side, for page breaks) at MATCH_THRESHOLD or better.

Every quotation must also be at most MAX_WORDS words long (fair use; the books are
read in place, never copied into the repository).
"""
from __future__ import annotations

import difflib
import os
import re
import shutil
import subprocess
import sys
from functools import lru_cache
from pathlib import Path


def find_repo() -> Path:
    """The FARGonautica root: $FARGONAUTICA if set, else the nearest ancestor holding .git and
    Literature/ (so this script works both from ralph_loops/loop0003/scripts/ and from the
    package's tools/). Outside FARGonautica (the standalone Seek-Whence repository) set
    FARGONAUTICA to a clone of it."""
    if os.environ.get("FARGONAUTICA"):
        return Path(os.environ["FARGONAUTICA"]).expanduser()
    for d in Path(__file__).resolve().parents:
        if (d / ".git").exists() and (d / "Literature").is_dir():
            return d
    raise SystemExit("check_quotes.py: cannot find the FARGonautica repository root "
                     "(set FARGONAUTICA to a clone of it)")


REPO = find_repo()

SOURCES = {
    "M86": REPO / "Literature" / "Ph.D. Theses" / "Meredith-Seek.whence.pdf",
    "MHN": REPO / "Literature" / "Meredith-Historical-Notes-on-Seek-Whence.pdf",
    # The author's copy, outside the repository (FCCA is copyrighted). FCCA_PDF overrides.
    "FCCA": Path(os.environ.get("FCCA_PDF",
                 "/home/al/snap/zotero-snap/common/Zotero/storage/R9L8T9GC/"
                 "Hofstadter - 1995 - Fluid concepts & creative analogies computer mode.pdf")),
}
OFFSET = {"M86": 12, "FCCA": 10, "MHN": 0}

MIN_WORDS = 6
MAX_WORDS = 80
MATCH_THRESHOLD = 0.92

ROMAN = {"i": 1, "v": 5, "x": 10, "l": 50, "c": 100}

TAG = r"\[(M86|FCCA|MHN)\s+pp?\.\s*([ivxlc]+|\d+)(?:\s*[-–]\s*([ivxlc]+|\d+))?\]"
QUOTE = re.compile(r'(?:"([^"]{10,800}?)"|“([^”]{10,800}?)”)[\s,.;:)]{0,4}' + TAG, re.S)

# Characters the OCR confuses, merged into one class each (applied after lowercasing
# and before stripping punctuation, so "l>" -> "l" and "e." -> "e" survive).
CONFUSABLE = str.maketrans({
    "w": "v", "a": "e", "b": "l", "1": "l", "i": "l", "!": "l", "|": "l",
    "0": "o", "5": "s", "$": "s", "z": "2", "j": "3", "8": "l",
})


def canon(text: str) -> str:
    text = text.lower().replace("ﬁ", "fi").replace("ﬂ", "fl")
    text = text.translate(CONFUSABLE)
    return re.sub(r"[^a-z0-9]", "", text)


def roman_value(s: str) -> int:
    total, prev = 0, 0
    for ch in reversed(s):
        v = ROMAN[ch]
        total += -v if v < prev else v
        prev = max(prev, v)
    return total


def pdf_page(tag: str, page: str) -> int:
    if page.isdigit():
        return int(page) + OFFSET[tag]
    if tag != "M86":
        raise ValueError(f"roman page {page} only exists for M86")
    return roman_value(page) - 1


@lru_cache(maxsize=None)
def pages_of(tag: str) -> list[str] | None:
    path = SOURCES[tag]
    if not path.exists() or not shutil.which("pdftotext"):
        return None
    out = subprocess.run(["pdftotext", str(path), "-"], capture_output=True, text=True,
                         errors="replace").stdout
    return out.split("\f")


def best_match(quote: str, haystack: str) -> float:
    """Fraction of the quote's canonical characters matched, in order, inside the
    best window of the haystack."""
    q, h = canon(quote), canon(haystack)
    if not q:
        return 1.0
    if q in h:
        return 1.0
    width = int(len(q) * 1.3) + 4
    step = max(1, len(q) // 5)
    best = 0.0
    for start in range(0, max(1, len(h) - len(q) // 2), step):
        window = h[start:start + width]
        sm = difflib.SequenceMatcher(None, q, window, autojunk=False)
        matched = sum(b.size for b in sm.get_matching_blocks() if b.size >= 3)
        best = max(best, matched / len(q))
        if best >= 0.999:
            break
    return best


def parts_of(quote: str) -> list[str]:
    quote = re.sub(r"\[[^\]]*\]", " ", quote)               # editorial insertions
    parts = re.split(r"\.\s*\.\s*\.|…", quote)
    return [p.strip() for p in parts if len(p.split()) >= MIN_WORDS]


def check_text(text: str, where: str) -> tuple[int, int, int, list[str]]:
    """Returns (checked, skipped, failed, messages)."""
    checked = skipped = failed = 0
    msgs = []
    for m in QUOTE.finditer(text):
        quote = m.group(1) or m.group(2)
        tag, p1, p2 = m.group(3), m.group(4), m.group(5) or m.group(4)
        line = text.count("\n", 0, m.start()) + 1
        words = len(quote.split())
        if words < MIN_WORDS:
            continue
        loc = f"{where}:{line}"
        if words > MAX_WORDS:
            failed += 1
            msgs.append(f"FAIL {loc}: quotation of {words} words is over the {MAX_WORDS}-word limit")
            continue
        pages = pages_of(tag)
        if pages is None:
            skipped += 1
            msgs.append(f"SKIP {loc}: source {tag} not available here ({SOURCES[tag]})")
            continue
        try:
            a, b = pdf_page(tag, p1), pdf_page(tag, p2)
        except (ValueError, KeyError) as exc:
            failed += 1
            msgs.append(f"FAIL {loc}: bad page in citation [{tag} p. {p1}]: {exc}")
            continue
        if not (1 <= a <= b <= len(pages)):
            failed += 1
            msgs.append(f"FAIL {loc}: [{tag} p. {p1}] is outside the document")
            continue
        hay = " ".join(pages[max(0, a - 2):min(len(pages), b + 1)])
        for part in parts_of(quote) or [quote]:
            checked += 1
            score = best_match(part, hay)
            if score < MATCH_THRESHOLD:
                failed += 1
                msgs.append(f"FAIL {loc}: not found on [{tag} p. {p1}{'-' + p2 if p2 != p1 else ''}] "
                            f"(best match {score:.2f}): \"{' '.join(part.split())[:90]}\"")
    return checked, skipped, failed, msgs


def files_in(args: list[str]) -> list[Path]:
    out = []
    for a in args:
        p = Path(a)
        if p.is_dir():
            out += sorted(x for x in p.rglob("*") if x.suffix in (".md", ".py") and x.is_file()
                          and "__pycache__" not in x.parts)
        elif p.is_file():
            out.append(p)
    return out


def main(argv: list[str]) -> int:
    if argv[:1] == ["--page"] and len(argv) == 3:
        pages = pages_of(argv[1])
        print(pages[pdf_page(argv[1], argv[2]) - 1] if pages else "source not available")
        return 0
    if argv[:1] == ["--find"] and len(argv) == 3:
        pages = pages_of(argv[1]) or []
        for i, pg in enumerate(pages, 1):
            s = best_match(argv[2], pg)
            if s >= MATCH_THRESHOLD:
                print(f"PDF p. {i} (printed {i - OFFSET[argv[1]]}): match {s:.2f}")
        return 0
    total = [0, 0, 0]
    for f in files_in(argv):
        c, s, x, msgs = check_text(f.read_text(errors="replace"), str(f))
        total = [total[0] + c, total[1] + s, total[2] + x]
        for msg in msgs:
            print(msg)
    print(f"quotations: {total[0]} checked, {total[1]} skipped (source missing), {total[2]} failed")
    return 1 if total[2] else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

#!/usr/bin/env python3
"""Check every citation of the appendix transcripts [M86 pp. 164-179] against the transcriptions.

Standard library only. The three appendix runs were transcribed line for line from the page
images into `tests/data/run1_3737.txt`, `run2_373373.txt` and `run3_121231234.txt` (item 04;
`tests/test_data.py` guards them). A citation of a transcript page usually names what the page
prints, in backticks (`doing task Hfilter`, `(Cycle 3 2 (3 7))`) or in double quotes. This
script finds every tag that cites a page in 164-179 in the given files, takes the text before
the tag back to the previous tag (at most CONTEXT characters), extracts its backticked and
quoted snippets, and looks each up in the transcription of the cited pages:

    EXACT      every snippet is printed on a cited page
    NEAR       a snippet is printed only on the page before or after (a page break)
    ELSEWHERE  a snippet is printed in a transcript, but not on the cited pages nor next to them
    NOTFOUND   a snippet is in no transcript (the reconstruction's own wording, a paraphrase,
               or a transcription difference: judged by hand, see docs/citation-audit.md)
    NOSNIPPET  nothing checkable in the context

Matching ignores whitespace (the printout runs `glint1glint2` together where the package prints
`glint1 glint2`, and a wrapped line is two lines in the transcription). Snippets of fewer than
MIN_CHARS characters, and single lower-case words (`stale`, `ok`), are ignored. In a .py file the
context is the docstring (or comment line) the tag is in; in a .md file, the paragraph.

    python3 tools/audit_transcript_citations.py [--all] [FILE_OR_DIR ...]

With no arguments: SPEC.md, DECISIONS.md, README.md, RESULTS.md, docs/, seekwhence/, tests/.
By default only the citations that are not EXACT are listed; --all lists every one.
Exit 0 always: the judgement is the auditor's (docs/citation-audit.md records it).
"""
from __future__ import annotations

import re
import sys
from collections import Counter
from pathlib import Path

HERE = Path(__file__).resolve().parent.parent
DATA = HERE / "tests" / "data"
RUNS = ("run1_3737.txt", "run2_373373.txt", "run3_121231234.txt")
FIRST, LAST = 164, 179
CONTEXT = 400
MIN_CHARS = 3

TAG = re.compile(r"\[M86 (pp?\.) ([0-9ivx, -]+)\]")
SNIPPET = re.compile(r"`([^`\n]+)`|\"([^\"\n]{3,}?)\"")


def squash(text: str) -> str:
    """Whitespace removed; the only normalisation applied to both sides."""
    return re.sub(r"\s+", "", text)


def load_pages() -> dict[int, str]:
    """Printed page -> the squashed console text of that page (notes and headers left out)."""
    pages: dict[int, list[str]] = {}
    for name in RUNS:
        page = None
        for line in (DATA / name).read_text().splitlines():
            if line.startswith("# page:"):
                page = int(line.split(":")[1])
                pages.setdefault(page, [])
            elif line.startswith("#") or page is None:
                continue
            else:
                pages[page].append(line)
    return {p: squash("\n".join(lines)) for p, lines in pages.items()}


def pages_of(tag_pages: str) -> list[int]:
    """`164`, `164-166`, `164, 170`, `165, 167-169` -> the arabic pages named."""
    out: list[int] = []
    for part in tag_pages.split(","):
        part = part.strip()
        if not part or not part[0].isdigit():
            continue
        if "-" in part:
            a, b = part.split("-", 1)
            out.extend(range(int(a), int(b) + 1))
        else:
            out.append(int(part))
    return out


def context_start(text: str, pos: int, last_end: int, is_code: bool) -> int:
    """Where the context of a tag at `pos` begins: after the previous tag, within CONTEXT
    characters, within the paragraph; in a .py file within the docstring (or the comment
    line) the tag is in, so that code is never read as a claim."""
    start = max(last_end, pos - CONTEXT)
    para = text.rfind("\n\n", start, pos)
    if para >= 0:
        start = para + 2
    if is_code:
        if text.count('"""', 0, pos) % 2 == 1:
            start = max(start, text.rfind('"""', 0, pos) + 3)
        else:
            start = max(start, text.rfind("\n", 0, pos) + 1)
    return start


def citations(text: str, is_code: bool = False):
    """(tag, pages, context, position) for each tag citing a transcript page."""
    last_end = 0
    for m in TAG.finditer(text):
        pages = pages_of(m.group(2))
        context = text[context_start(text, m.start(), last_end, is_code):m.start()]
        last_end = m.end()
        if any(FIRST <= p <= LAST for p in pages):
            yield m.group(0), [p for p in pages if FIRST <= p <= LAST], context, m.start()


def snippets(context: str) -> list[str]:
    out = []
    for m in SNIPPET.finditer(context):
        s = (m.group(1) or m.group(2)).strip()
        if len(squash(s)) >= MIN_CHARS and not re.fullmatch(r"[a-z]+", s):
            out.append(s)
    return out


def judge(snip: str, pages: list[int], texts: dict[int, str]) -> str:
    key = squash(snip)
    if any(key in texts.get(p, "") for p in pages):
        return "EXACT"
    near = {p + d for p in pages for d in (-1, 1)} - set(pages)
    if any(key in texts.get(p, "") for p in near):
        return "NEAR"
    if any(key in t for t in texts.values()):
        return "ELSEWHERE"
    return "NOTFOUND"


def where(snip: str, texts: dict[int, str]) -> list[int]:
    key = squash(snip)
    return [p for p, t in texts.items() if key in t]


ORDER = ["EXACT", "NEAR", "ELSEWHERE", "NOTFOUND", "NOSNIPPET"]


def audit(paths: list[Path], show_all: bool) -> Counter:
    texts = load_pages()
    counts: Counter = Counter()
    for path in paths:
        text = path.read_text()
        for tag, pages, context, pos in citations(text, path.suffix == ".py"):
            line = text.count("\n", 0, pos) + 1
            snips = snippets(context)
            if not snips:
                verdict, detail = "NOSNIPPET", []
            else:
                verdicts = {s: judge(s, pages, texts) for s in snips}
                verdict = max(verdicts.values(), key=ORDER.index)
                detail = [f"{v}: `{s}`" + (f" (on {where(s, texts)})" if v in ("NEAR", "ELSEWHERE") else "")
                          for s, v in verdicts.items() if v != "EXACT"]
            counts[verdict] += 1
            if show_all or verdict not in ("EXACT", "NOSNIPPET"):
                rel = path.relative_to(HERE) if path.is_relative_to(HERE) else path
                print(f"{rel}:{line} {tag} {verdict}")
                for d in detail:
                    print(f"    {d}")
    return counts


def collect(args: list[str]) -> list[Path]:
    if not args:
        args = ["SPEC.md", "DECISIONS.md", "README.md", "RESULTS.md", "docs", "seekwhence", "tests"]
    paths: list[Path] = []
    for a in args:
        p = (HERE / a) if not Path(a).is_absolute() else Path(a)
        if p.is_dir():
            paths.extend(sorted(q for q in p.rglob("*") if q.suffix in (".md", ".py")))
        else:
            paths.append(p)
    return paths


def main(argv: list[str]) -> int:
    show_all = "--all" in argv
    args = [a for a in argv if a != "--all"]
    counts = audit(collect(args), show_all)
    total = sum(counts.values())
    print(f"\n{total} transcript citations: " + ", ".join(f"{k} {counts[k]}" for k in ORDER))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

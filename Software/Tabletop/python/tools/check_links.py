#!/usr/bin/env python3
"""Check every Markdown link under Software/Tabletop/ in both of the places the folder is
read (ANSWERS.md 6; TASK.md "Standalone"): as a folder of FARGonautica and as a repository
of its own, where nothing outside the folder exists.

The rules the links keep, and what is checked for each kind:

- A relative link (`python/README.md`, `docs/problems.md#8`) must stay inside the folder and
  point at a file or directory that exists; its `#fragment`, if any, must be a heading of the
  Markdown file it points at.
- An anchor link (`#how-well-it-does`) must be a heading of its own file (GitHub's rule:
  lower case, punctuation dropped, spaces to hyphens).
- A link that leaves the folder is an absolute URL. FARGonautica's own files are linked as
  `https://github.com/fargonauts/FARGonautica/blob/master/<path>` (spaces as `%20`); when a
  FARGonautica checkout is found (`$FARGONAUTICA`, or an ancestor directory holding `.git`
  and `Literature/`, the lookup of `check_quotes.py`), `<path>` must exist in it. Without a
  checkout these paths are only counted, and `--online` is the way to check them.
- With `--online`, every distinct absolute URL is fetched once (GET, a timeout, one retry on
  a server error) and anything but 200 is reported as `ONLINE`. A 404 on GitHub may mean the
  file is not pushed there yet (the Gitea origin is pushed first): read the report, do not
  edit the link.

Usage:
    python3 tools/check_links.py [ROOT] [--fargonautica DIR] [--online] [--timeout S]

ROOT is the folder to check (default: the Software/Tabletop/ this file is in). Prints one
`FAIL` line per broken link, one `ONLINE` line per URL that did not answer 200, then a
summary; exits 1 when any line was printed, 0 otherwise. Code and documentation are checked
by `tests/test_links.py`; the whole standalone procedure that runs this is
`tools/standalone_check.sh` and `docs/standalone.md`.
"""

import argparse
import importlib.util
import re
import sys
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from pathlib import Path
from urllib.parse import unquote

HERE = Path(__file__).resolve().parent                   # python/tools
DEFAULT_ROOT = HERE.parents[1]                           # Software/Tabletop
SKIP_DIRS = {"__pycache__", ".pytest_cache", ".venv", "build", "dist", ".git",
             "bench-results"}
GITHUB_BLOB = re.compile(r"^https://github\.com/fargonauts/FARGonautica/(?:blob|tree)/master/(.+)$")
# A Markdown link's target: `](target)`, `](<target>)` or `](target "title")`.
LINK = re.compile(r"\]\(\s*<?([^)\s>]+)>?(?:\s+\"[^\"]*\")?\s*\)")
USER_AGENT = "Mozilla/5.0 (tabletop link check)"


def strip_fences(text: str) -> str:
    """The text without fenced blocks and inline code, where `](...)` is not a link."""
    text = re.sub(r"^```.*?^```", "", text, flags=re.M | re.S)
    return re.sub(r"`[^`\n]*`", "", text)


def anchor(heading: str) -> str:
    """GitHub's anchor for a heading: lower case, punctuation dropped, spaces to hyphens."""
    text = re.sub(r"[^\w\- ]", "", heading.strip().lower())
    return text.replace(" ", "-")


def headings(text: str) -> list[str]:
    return re.findall(r"^#{1,6} (.+?)\s*$", strip_fences(text), re.M)


def anchors_of(path: Path) -> set[str]:
    return {anchor(h) for h in headings(path.read_text(encoding="utf-8", errors="replace"))}


def fargonautica_lookup():
    """`check_quotes.fargonautica`, loaded from its file (the one lookup, ANSWERS.md 6)."""
    spec = importlib.util.spec_from_file_location("tabletop_check_quotes",
                                                  HERE / "check_quotes.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.fargonautica


def markdown_files(root: Path):
    for f in sorted(root.rglob("*.md")):
        rel = f.relative_to(root)
        if any(p in SKIP_DIRS or p.endswith(".egg-info") for p in rel.parts):
            continue
        if f.is_file():
            yield f


@dataclass
class Link:
    file: Path          # the Markdown file, relative to the root
    target: str         # the link as written
    kind: str           # anchor | relative | github | outside | mailto


@dataclass
class Report:
    root: Path
    checkout: Path | None
    links: list = field(default_factory=list)
    failures: list = field(default_factory=list)      # "FAIL file: link (reason)"
    online: dict = field(default_factory=dict)        # url -> status (int or an error word)

    def count(self, kind: str) -> int:
        return sum(1 for l in self.links if l.kind == kind)

    @property
    def urls(self) -> list[str]:
        seen = []
        for l in self.links:
            if l.kind in ("github", "outside") and l.target not in seen:
                seen.append(l.target)
        return seen

    def online_failures(self) -> list[str]:
        return [f"ONLINE {url}: {status}" for url, status in self.online.items()
                if status != 200]


def kind_of(target: str) -> str:
    if target.startswith("#"):
        return "anchor"
    if target.startswith("mailto:"):
        return "mailto"
    if GITHUB_BLOB.match(target):
        return "github"
    if re.match(r"^https?://", target):
        return "outside"
    return "relative"


def check_relative(report: Report, link: Link, source: Path) -> None:
    root = report.root.resolve()
    path, _, fragment = link.target.partition("#")
    dest = (source.parent / unquote(path)).resolve()
    if path.startswith("/") or not (dest == root or root in dest.parents):
        report.failures.append(f"FAIL {link.file}: {link.target} (leaves the folder)")
    elif not dest.exists():
        report.failures.append(f"FAIL {link.file}: {link.target} (points at nothing)")
    elif fragment and dest.suffix == ".md" and fragment not in anchors_of(dest):
        report.failures.append(f"FAIL {link.file}: {link.target} (no such heading in "
                               f"{dest.name})")


def check_github(report: Report, link: Link) -> None:
    if report.checkout is None:
        return
    rel = unquote(GITHUB_BLOB.match(link.target).group(1))
    if not (report.checkout / rel).exists():
        report.failures.append(f"FAIL {link.file}: {link.target} (no {rel} in the "
                               f"FARGonautica checkout {report.checkout})")


def check_folder(root: Path, checkout: Path | None) -> Report:
    """Every link of every Markdown file under `root`, checked offline."""
    report = Report(root, checkout)
    for source in markdown_files(root):
        text = strip_fences(source.read_text(encoding="utf-8", errors="replace"))
        own_anchors = None
        for m in LINK.finditer(text):
            link = Link(source.relative_to(root), m.group(1), kind_of(m.group(1)))
            report.links.append(link)
            if link.kind == "anchor":
                if own_anchors is None:
                    own_anchors = anchors_of(source)
                if link.target[1:] not in own_anchors:
                    report.failures.append(f"FAIL {link.file}: {link.target} (no such heading)")
            elif link.kind == "relative":
                check_relative(report, link, source)
            elif link.kind == "github":
                check_github(report, link)
    return report


def fetch_status(url: str, timeout: float):
    """The HTTP status of a GET of `url` (the body is not read), an HTTP error's code, or
    the error's name when there was no answer."""
    request = urllib.request.Request(url, headers={"User-Agent": USER_AGENT})
    try:
        with urllib.request.urlopen(request, timeout=timeout) as answer:
            return answer.status
    except urllib.error.HTTPError as error:
        return error.code
    except (urllib.error.URLError, OSError, ValueError) as error:
        return type(error).__name__


def check_online(report: Report, timeout: float, fetch=fetch_status) -> None:
    """Fetch every distinct absolute URL once, retrying once on a server error (5xx)."""
    for url in report.urls:
        status = fetch(url, timeout)
        if isinstance(status, int) and status >= 500:
            status = fetch(url, timeout)
        report.online[url] = status


def summary(report: Report) -> str:
    place = (f"inside the FARGonautica checkout {report.checkout}" if report.checkout
             else "with no FARGonautica checkout (GitHub paths not checked against files)")
    files = len({l.file for l in report.links})
    lines = [f"{len(report.links)} links in {files} Markdown files under {report.root}: "
             f"{report.count('relative')} relative, {report.count('anchor')} anchors, "
             f"{report.count('github')} to FARGonautica's files on GitHub, "
             f"{report.count('outside')} other URLs; {place}."]
    if report.online:
        ok = sum(1 for s in report.online.values() if s == 200)
        lines.append(f"online: {ok} of {len(report.online)} URLs answered 200.")
    bad = len(report.failures) + len(report.online_failures())
    lines.append("every link checked is good." if not bad else f"{bad} problem(s).")
    return "\n".join(lines)


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("root", nargs="?", default=str(DEFAULT_ROOT),
                        help="the folder to check (default: this Software/Tabletop/)")
    parser.add_argument("--fargonautica", metavar="DIR",
                        help="a FARGonautica checkout to check the GitHub paths against "
                             "(default: $FARGONAUTICA or an ancestor checkout)")
    parser.add_argument("--online", action="store_true",
                        help="also fetch every absolute URL and report its HTTP status")
    parser.add_argument("--timeout", type=float, default=20.0,
                        help="seconds per fetch with --online (default 20)")
    args = parser.parse_args(argv)
    root = Path(args.root).resolve()
    if not root.is_dir():
        print(f"not a directory: {root}", file=sys.stderr)
        return 2
    checkout = Path(args.fargonautica).expanduser() if args.fargonautica else \
        fargonautica_lookup()(root)
    report = check_folder(root, checkout)
    if args.online:
        check_online(report, args.timeout)
    for line in report.failures + report.online_failures():
        print(line)
    if report.online:
        for url, status in report.online.items():
            print(f"  {status}  {url}")
    print(summary(report))
    return 1 if report.failures or report.online_failures() else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))

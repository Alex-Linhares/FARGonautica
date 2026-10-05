#!/usr/bin/env python3
"""Capture Chez's output for the differential batteries, split per test.

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

    python3 python/oracle/capture.py BATTERY...     # utilities, sgl-battery, tests/diff/rule-battery.scm
    python3 python/oracle/capture.py --all          # every battery, in parallel
    python3 python/oracle/capture.py --all --out DIR   # somewhere else (the freshness test)

A battery (tests/diff/NAME-battery.scm, or python/oracle/batteries/NAME-battery.scm
for the Python port's own vectors, such as chez-battery.scm) is evaluated by Chez Scheme 10 with the
whole original loaded, through chez_scheme/oracle/diff-eval.ss, unedited, after
tests/diff/helpers.scm (and, for sgl, tests/diff/sgl-chez-setup.ss), exactly as
racket/tests/diff-runner.rkt does.  diff-eval.ss prints one "NAME => VALUE"
record per (test NAME EXPR) form; a VALUE may span lines.  The test names, in
order, come from Chez's own reader (python/oracle/list-tests.ss), so the split
does not guess where a record ends.

Written to python/fixtures/NAME/:
    MANIFEST          the test names, one per line, in battery order
    NNN-NAME.txt      the VALUE of test number NNN (from 000), byte for byte,
                      without the record's final newline; characters outside
                      [A-Za-z0-9.+_-] in NAME are written %XX
    SOURCES           sha256 of every input of the capture, and Chez's version,
                      so that tests/test_fixtures.py can tell a stale fixture
                      without running Chez

Joining the records again ("NAME => VALUE\\n" per test) gives Chez's output
byte for byte; the capture checks that before writing anything.  Fixtures are
never edited by hand.
"""
from __future__ import annotations

import argparse
import hashlib
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
DIFF = REPO / "tests" / "diff"
# the Python port's own batteries (Chez vectors the frozen tests/diff/ lacks)
LOCAL = Path(__file__).resolve().parent / "batteries"
DIFF_EVAL = REPO / "chez_scheme" / "oracle" / "diff-eval.ss"
PRELUDE = REPO / "chez_scheme" / "oracle" / "prelude.ss"
LIST_TESTS = Path(__file__).resolve().parent / "list-tests.ss"
FIXTURES = REPO / "python" / "fixtures"
HELPERS = DIFF / "helpers.scm"

# Chez-only files evaluated between helpers.scm and the battery, as in the
# Racket side's #:chez-setup (racket/tests/sgl-diff-test.rkt)
SETUP = {"sgl": [DIFF / "sgl-chez-setup.ss"]}

SAFE = set("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789.+_-")


class CaptureError(Exception):
    pass


def batteries() -> list[str]:
    """Every battery's short name (utilities, coderack, ...), sorted."""
    return sorted(p.name[: -len("-battery.scm")]
                  for d in (DIFF, LOCAL) for p in d.glob("*-battery.scm"))


def battery_name(arg: str) -> str:
    """utilities, utilities-battery, utilities-battery.scm or a path -> utilities."""
    name = Path(arg).name
    for suffix in (".scm", "-battery"):
        if name.endswith(suffix):
            name = name[: -len(suffix)]
    if not (DIFF / f"{name}-battery.scm").exists() and not (LOCAL / f"{name}-battery.scm").exists():
        raise CaptureError(f"no battery tests/diff/{name}-battery.scm")
    return name


def battery_path(name: str) -> Path:
    local = LOCAL / f"{name}-battery.scm"
    return local if local.exists() else DIFF / f"{name}-battery.scm"


def file_name(index: int, test: str) -> str:
    safe = "".join(c if c in SAFE else "%%%02X" % ord(c) for c in test)
    return f"{index:03d}-{safe}.txt"


def scheme() -> str:
    for exe in ("scheme", "chezscheme"):
        path = shutil.which(exe)
        if path:
            return path
    raise CaptureError("Chez Scheme not found (scheme or chezscheme)")


def chez_version() -> str:
    proc = subprocess.run([scheme(), "--version"], capture_output=True, text=True)
    return (proc.stdout + proc.stderr).strip()


def run_chez(args: list[str]) -> bytes:
    proc = subprocess.run([scheme(), "--script", *args], cwd=REPO, capture_output=True)
    if proc.returncode != 0 or proc.stderr:
        raise CaptureError(
            f"scheme --script {' '.join(args)} exited {proc.returncode}:\n"
            + proc.stderr.decode(errors="replace")
        )
    return proc.stdout


def test_names(name: str) -> list[str]:
    """The battery's test names, in order, as read by Chez."""
    out = run_chez([str(LIST_TESTS), str(battery_path(name))]).decode()
    return out.splitlines()


def chez_output(name: str) -> bytes:
    """diff-eval.ss's whole output for the battery."""
    files = [HELPERS, *SETUP.get(name, []), battery_path(name)]
    return run_chez([str(DIFF_EVAL), *(str(f) for f in files)])


def split(raw: bytes, names: list[str]) -> list[bytes]:
    """The VALUE of each record, given the test names in order."""
    values = []
    pos = 0
    for i, test in enumerate(names):
        head = test.encode() + b" => "
        if not raw.startswith(head, pos):
            raise CaptureError(f"record {i} does not start with {head!r} at byte {pos}")
        start = pos + len(head)
        if i + 1 < len(names):
            end = raw.find(b"\n" + names[i + 1].encode() + b" => ", start)
            if end < 0:
                raise CaptureError(f"no record for {names[i + 1]!r} after {test!r}")
        else:
            if not raw.endswith(b"\n"):
                raise CaptureError("output does not end with a newline")
            end = len(raw) - 1
        values.append(raw[start:end])
        pos = end + 1
    if pos != len(raw):
        raise CaptureError(f"{len(raw) - pos} bytes after the last record")
    return values


def join(names: list[str], values: list[bytes]) -> bytes:
    """The inverse of split: diff-eval.ss's output."""
    return b"".join(n.encode() + b" => " + v + b"\n" for n, v in zip(names, values))


def source_files(name: str) -> list[Path]:
    """Everything a capture depends on, besides chez_scheme/original/ (the gate pins it)."""
    files = [DIFF_EVAL, PRELUDE, LIST_TESTS, *sorted(DIFF.iterdir())]
    if battery_path(name).parent == LOCAL:
        files.append(battery_path(name))
    return files


def sources(name: str, version: str) -> str:
    lines = [f"chez {version}"]
    for path in source_files(name):
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        lines.append(f"{digest}  {path.relative_to(REPO)}")
    return "\n".join(lines) + "\n"


def capture(name: str, out: Path = FIXTURES, version: str | None = None) -> int:
    """Capture one battery into out/NAME/; returns the number of tests."""
    names = test_names(name)
    if len(set(names)) != len(names):
        raise CaptureError(f"{name}: duplicate test names")
    raw = chez_output(name)
    values = split(raw, names)
    if join(names, values) != raw:
        raise CaptureError(f"{name}: split/join does not round-trip")
    target = out / name
    if target.exists():
        shutil.rmtree(target)
    target.mkdir(parents=True)
    (target / "MANIFEST").write_text("".join(n + "\n" for n in names))
    for i, (test, value) in enumerate(zip(names, values)):
        (target / file_name(i, test)).write_bytes(value)
    (target / "SOURCES").write_text(sources(name, version or chez_version()))
    return len(names)


def capture_all(names: list[str], out: Path = FIXTURES) -> dict[str, int]:
    version = chez_version()
    with ThreadPoolExecutor(max_workers=len(names) or 1) as pool:
        counts = pool.map(lambda n: capture(n, out, version), names)
        return dict(zip(names, counts))


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("battery", nargs="*", help="battery name or path")
    parser.add_argument("--all", action="store_true", help="every battery (tests/diff/ and oracle/batteries/)")
    parser.add_argument("--out", type=Path, default=FIXTURES, help="fixtures directory")
    args = parser.parse_args(argv)
    try:
        names = batteries() if args.all else [battery_name(b) for b in args.battery]
        if not names:
            parser.error("name a battery, or --all")
        for name, count in capture_all(names, args.out).items():
            print(f"{name}: {count} tests -> {args.out / name}")
    except CaptureError as e:
        print(f"capture: {e}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())

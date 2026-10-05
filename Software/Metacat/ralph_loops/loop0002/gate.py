#!/usr/bin/env python3
"""Regression gate for loop0002 (the Python port). Exit 0 = green.

1. chez_scheme/original/ (the Metacat 1.2 source) is byte-for-byte the tree imported
   in commit 9f072c0.
2. The references this loop translates against are frozen: chez_scheme/ (original
   and oracle), racket/, tests/golden/ and tests/diff/ are identical to commit
   FROZEN_AT, with no uncommitted edits or untracked files. Oracle capture scripts
   for the Python port go in python/oracle/.
3. python/run-tests.sh, once item 00 has created it, passes. It is the single entry
   point for the Python port's tests.

Run from anywhere: python3 ralph_loops/loop0002/gate.py
"""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
IMPORT_COMMIT = "9f072c0"
IMPORT_PATH = "Metacat"            # where the import commit put the original
ORIGINAL = "chez_scheme/original"  # where it lives now
FROZEN_AT = "9684b16"              # end of loop0001: the finished Racket port
FROZEN = ["chez_scheme", "racket", "tests/golden", "tests/diff"]
RUN_TESTS = REPO / "python" / "run-tests.sh"


def git(*args: str) -> subprocess.CompletedProcess:
    return subprocess.run(["git", *args], cwd=REPO, capture_output=True, text=True)


def step(name: str, cmd: list[str]) -> bool:
    print(f"== {name}: {' '.join(cmd)}", flush=True)
    proc = subprocess.run(cmd, cwd=REPO)
    print(f"== {name}: {'ok' if proc.returncode == 0 else 'FAILED'}", flush=True)
    return proc.returncode == 0


def original_untouched() -> bool:
    imported = git("rev-parse", f"{IMPORT_COMMIT}:{IMPORT_PATH}").stdout.strip()
    committed = git("rev-parse", f"HEAD:{ORIGINAL}").stdout.strip()
    ok = bool(imported) and committed == imported
    if not ok:
        print(f"{ORIGINAL}/ in HEAD differs from the import commit")
    print(f"== original untouched: {'ok' if ok else 'FAILED'}", flush=True)
    return ok


def references_frozen() -> bool:
    ok = True
    for path in FROZEN:
        # README.md files are documentation, not reference code: they may be added later.
        spec = [path, ":(exclude,glob)**/README.md"]
        if git("diff", "--quiet", FROZEN_AT, "--", *spec).returncode != 0:
            print(f"{path}/ differs from {FROZEN_AT} (committed or not); this loop must not edit it")
            ok = False
        untracked = git("ls-files", "--others", "--exclude-standard", *spec).stdout.strip()
        if untracked:
            print(f"untracked files in {path}/:\n{untracked}")
            ok = False
    print(f"== references frozen: {'ok' if ok else 'FAILED'}", flush=True)
    return ok


def main() -> int:
    ok = original_untouched()
    ok = references_frozen() and ok
    if RUN_TESTS.exists():
        ok = step("python tests", ["bash", str(RUN_TESTS)]) and ok
    else:
        print("== python tests: python/run-tests.sh does not exist yet (item 00 creates it)")
    print("GATE " + ("PASSED" if ok else "FAILED"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())

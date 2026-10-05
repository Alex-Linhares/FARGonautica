#!/usr/bin/env python3
"""Regression gate for loop0001. Exit 0 = green.

1. chez_scheme/original/ (the Metacat 1.2 source) is byte-for-byte the tree
   imported in commit 9f072c0, with no uncommitted edits or untracked files.
2. tests/run-tests.sh, once item 00 has created it, passes. It is the single
   entry point for every test in the repo (Chez oracle checks, raco test, the
   Racket-vs-Chez equivalence runs, the GUI render checks).

Run from anywhere: python3 ralph_loops/loop0001/gate.py
"""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
IMPORT_COMMIT = "9f072c0"
IMPORT_PATH = "Metacat"            # where the import commit put it
ORIGINAL = "chez_scheme/original"  # where it lives now
RUN_TESTS = REPO / "tests" / "run-tests.sh"


def step(name: str, cmd: list[str]) -> bool:
    print(f"== {name}: {' '.join(cmd)}", flush=True)
    proc = subprocess.run(cmd, cwd=REPO)
    print(f"== {name}: {'ok' if proc.returncode == 0 else 'FAILED'}", flush=True)
    return proc.returncode == 0


def original_untouched() -> bool:
    # Compare git tree hashes, so the check holds wherever the original lives.
    def tree(spec: str) -> str:
        return subprocess.run(["git", "rev-parse", spec], cwd=REPO,
                              capture_output=True, text=True).stdout.strip()
    imported = tree(f"{IMPORT_COMMIT}:{IMPORT_PATH}")
    committed = tree(f"HEAD:{ORIGINAL}")
    worktree = subprocess.run(["git", "diff", "--quiet", "HEAD", "--", ORIGINAL],
                              cwd=REPO).returncode
    untracked = subprocess.run(["git", "ls-files", "--others", "--exclude-standard", ORIGINAL],
                               cwd=REPO, capture_output=True, text=True).stdout.strip()
    if committed != imported:
        print(f"{ORIGINAL}/ in HEAD differs from the import commit; the original must not be edited")
    if worktree != 0:
        print(f"{ORIGINAL}/ has uncommitted edits; the original must not be edited")
    if untracked:
        print(f"untracked files in {ORIGINAL}/:\n" + untracked)
    ok = bool(imported) and committed == imported and worktree == 0 and not untracked
    print(f"== original untouched: {'ok' if ok else 'FAILED'}", flush=True)
    return ok


def main() -> int:
    ok = original_untouched()
    if RUN_TESTS.exists():
        ok = step("tests", ["bash", str(RUN_TESTS)]) and ok
    else:
        print("== tests: tests/run-tests.sh does not exist yet (item 00 creates it)")
    print("GATE " + ("PASSED" if ok else "FAILED"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())

#!/usr/bin/env python3
"""Regression gate for loop0003 (the single-window Qt GUI). Exit 0 = green.

1. chez_scheme/original/ is byte-for-byte the tree imported in commit 9f072c0.
2. Frozen since FROZEN_AT, committed or not, README.md files aside:
   - the references: chez_scheme/, tests/golden/, tests/diff/;
   - the Python engine: python/metacat/*.py (the GUI watches; it never changes a run);
   - the frozen Chez outputs: python/fixtures/.
3. python/run-tests.sh passes: the engine, the tkinter GUI, and the new Qt GUI.
4. If racket/ changed since FROZEN_AT (the optional Racket item), tests/run-tests.sh
   passes too.

Run from anywhere: python3 ralph_loops/loop0003/gate.py
"""
from __future__ import annotations

import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
IMPORT_COMMIT = "9f072c0"
IMPORT_PATH = "Metacat"
ORIGINAL = "chez_scheme/original"
FROZEN_AT = "3defda7"              # after loop0002 and the READMEs
FROZEN = ["chez_scheme", "tests/golden", "tests/diff", "python/fixtures",
          ":(glob)python/metacat/*.py"]
NOT_README = ":(exclude,glob)**/README.md"


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
    print(f"== original untouched: {'ok' if ok else 'FAILED'}", flush=True)
    return ok


def changed(path: str) -> bool:
    diff = git("diff", "--quiet", FROZEN_AT, "--", path, NOT_README).returncode != 0
    untracked = git("ls-files", "--others", "--exclude-standard", "--", path, NOT_README).stdout.strip()
    return diff or bool(untracked)


def frozen_untouched() -> bool:
    ok = True
    for path in FROZEN:
        if changed(path):
            print(f"{path} differs from {FROZEN_AT}; this loop must not change it")
            print(git("status", "--short", "--", path, NOT_README).stdout)
            ok = False
    print(f"== frozen paths untouched: {'ok' if ok else 'FAILED'}", flush=True)
    return ok


def main() -> int:
    ok = original_untouched()
    ok = frozen_untouched() and ok
    ok = step("python tests", ["bash", "python/run-tests.sh"]) and ok
    if changed("racket"):
        ok = step("racket tests (racket/ changed)", ["bash", "tests/run-tests.sh"]) and ok
    else:
        print("== racket tests: skipped, racket/ unchanged")
    print("GATE " + ("PASSED" if ok else "FAILED"))
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())

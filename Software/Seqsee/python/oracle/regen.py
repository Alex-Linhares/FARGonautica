#!/usr/bin/env python3
"""Regenerate golden files: oracle/NAME.pl -> tests/golden/NAME.json.

Usage: python3 oracle/regen.py [NAME ...]   (default: all oracle/*.pl)
GUI oracles (oracle/gui_*.pl) run through run_perl_gui.sh (Perl/Tk on a private Xvfb).
"""
import json
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
GOLDEN = HERE.parent / "tests" / "golden"


def runner(script):
    """GUI oracles (gui_*.pl, which use Perl/Tk) need the Xvfb wrapper."""
    return "run_perl_gui.sh" if script.stem.startswith("gui_") else "run_perl.sh"


def regen(script):
    out = subprocess.run(
        [str(HERE / runner(script)), str(script)],
        capture_output=True, text=True,
    )
    if out.returncode != 0:
        sys.exit(f"{script.name} failed:\n{out.stderr}")
    data = json.loads(out.stdout)  # validate
    target = GOLDEN / f"{script.stem}.json"
    target.write_text(json.dumps(data, indent=1, sort_keys=True) + "\n")
    print(f"{script.name} -> {target.relative_to(HERE.parent)} ({len(data)} cases)")


def main():
    GOLDEN.mkdir(parents=True, exist_ok=True)
    names = sys.argv[1:]
    scripts = [HERE / f"{n.removesuffix('.pl')}.pl" for n in names] or sorted(HERE.glob("*.pl"))
    for s in scripts:
        regen(s)


if __name__ == "__main__":
    main()

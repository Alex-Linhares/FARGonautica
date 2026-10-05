"""Capture the Tcl command stream of the original's SGL interpreter (loop0002 item 13).

Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).

  python3 python/oracle/capture_sgl_tcl.py [--out DIR]

Runs `scheme --script python/oracle/sgl-tcl.ss python/oracle/sgl-fixture.scm` from
the repository root: the unedited sgl-interpreter.ss and fonts.ss draw the fixture
of every SGL form on each of its viewports, with swl:tcl-eval recording what they
send to Tk (see sgl-tcl.ss).  Writes DIR (default python/fixtures/sgl-tcl/):
  NAME.txt   one file per viewport of the fixture: the written commands, one per
             line, in the order the original sends them
  SOURCES    sha256 of every input of the capture, and Chez's version, for the
             freshness check in test_sgl.py
Nothing here is hand-edited; a rerun writes the same files.
"""
from __future__ import annotations

import argparse
import hashlib
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
REPO = HERE.parents[1]
sys.path.insert(0, str(HERE))

import capture  # noqa: E402

OUT = REPO / "python" / "fixtures" / "sgl-tcl"
SCRIPT = "python/oracle/sgl-tcl.ss"
FIXTURE = "python/oracle/sgl-fixture.scm"
SOURCE_FILES = ["chez_scheme/oracle/prelude.ss", "chez_scheme/original/sgl-interpreter.ss",
                "chez_scheme/original/fonts.ss", "chez_scheme/original/constants.ss",
                SCRIPT, FIXTURE, "python/oracle/capture_sgl_tcl.py"]


def sources():
    lines = ["chez " + capture.chez_version()]
    for f in SOURCE_FILES:
        lines.append("%s  %s" % (hashlib.sha256((REPO / f).read_bytes()).hexdigest(), f))
    return "\n".join(lines) + "\n"


def split(text):
    """The stream, per viewport: {name: lines}."""
    streams, name = {}, None
    for line in text.splitlines():
        if line.startswith(";; viewport "):
            name = line[len(";; viewport "):]
            streams[name] = []
        elif name is None:
            raise SystemExit("output before the first viewport: %r" % line)
        else:
            streams[name].append(line)
    return streams


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default=str(OUT))
    args = ap.parse_args()
    proc = subprocess.run(["scheme", "--script", SCRIPT, FIXTURE], cwd=REPO,
                          capture_output=True, text=True)
    if proc.returncode != 0 or proc.stderr:
        raise SystemExit("sgl-tcl.ss failed (%d):\n%s%s" % (proc.returncode, proc.stdout[-2000:],
                                                            proc.stderr))
    streams = split(proc.stdout)
    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)
    for name, lines in streams.items():
        (out / (name + ".txt")).write_text("\n".join(lines) + "\n")
    (out / "SOURCES").write_text(sources())
    print(", ".join("%s: %d commands" % (n, len(l)) for n, l in streams.items()))


if __name__ == "__main__":
    main()

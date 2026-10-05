#!/usr/bin/env bash
# Builds the standalone Metacat program (racket/metacat.rkt) with raco exe and
# raco distribute.  The result needs no Racket installation:
#
#   bash make-dist.sh [DEST]          # default DEST: build/metacat
#   DEST/bin/metacat                  # the GUI
#   DEST/bin/metacat abc abd xyz --seed 7   # a headless run, as racket/cli.rkt
#
# DEST is replaced.  Part of the Racket port of Metacat (GPL v2 or later).
set -euo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEST="${1:-$REPO/build/metacat}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

raco make -v "$REPO/racket/metacat.rkt"
# --gui: a GUI executable (on Linux the same kind of binary; on Windows and
# macOS it is what makes the windows work); it still runs headless when
# given a problem.
raco exe --gui -o "$WORK/metacat" "$REPO/racket/metacat.rkt"
rm -rf "$DEST"
mkdir -p "$(dirname "$DEST")"
raco distribute "$DEST" "$WORK/metacat"
for f in README.md; do cp "$REPO/$f" "$DEST/"; done
cp "$REPO/chez_scheme/original/LICENSE" "$DEST/LICENSE"
echo "make-dist: built $DEST"

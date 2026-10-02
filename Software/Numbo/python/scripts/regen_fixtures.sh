#!/usr/bin/env bash
# Regenerate python/fixtures/ from the SBCL oracle (lisp/src/, oracle mode).
#
# Usage: python/scripts/regen_fixtures.sh [OUTPUT_DIR]
#   Runs every capture script lisp/tests/oracle/*.lisp except common.lisp (the
#   shared prologue), in name order.  OUTPUT_DIR defaults to python/fixtures/;
#   python/tests/test_fixtures_current.py passes a temporary directory and
#   compares the result with the committed fixtures.
set -euo pipefail
repo="$(cd "$(dirname "$0")/../.." && pwd)"
out="${1:-$repo/python/fixtures}"
mkdir -p "$out"
export NUMBO_FIXTURE_DIR="$(cd "$out" && pwd)"
unset WINDOW_GFX

shopt -s nullglob
for script in "$repo"/lisp/tests/oracle/*.lisp; do
    [ "$(basename "$script")" = common.lisp ] && continue
    sbcl --noinform --non-interactive --no-userinit --no-sysinit \
         --load "$script"
done

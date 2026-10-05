#!/usr/bin/env bash
# Single entry point for every test in the repo. Fails on the first error.
#   1. raco test racket/            (Racket unit tests of the port)
#   2. chez_scheme/oracle/tests/*.ss (Chez oracle checks, each run with `scheme --script`;
#                                    a check fails by exiting non-zero)
set -euo pipefail
REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO"

SCHEME="$(command -v scheme || command -v chezscheme || true)"
if [ -z "$SCHEME" ]; then
  echo "run-tests: Chez Scheme not found (sudo apt install chezscheme)" >&2
  exit 1
fi

# Compile every module first, through the compilation manager: raco test and
# plain `racket` (cli-test.rkt runs racket/cli.rkt) load a module's .zo
# without checking its dependencies, so a changed module could leave others'
# .zo files stale (docs/anomalies_and_quirks.md).
echo "== raco make racket/"
find racket -name '*.rkt' -not -path '*/compiled/*' -print0 | xargs -0 raco make

echo "== raco test racket/"
raco test racket/

# GUI tests open racket/gui windows: only on a virtual display.  GTK prefers
# Wayland when WAYLAND_DISPLAY is set and would then ignore Xvfb and use the
# owner's screen, so it is unset and the X11 backend forced.
echo "== raco test racket/gui-tests (xvfb-run)"
env -u WAYLAND_DISPLAY GDK_BACKEND=x11 \
  xvfb-run -a -s "-screen 0 1920x1200x24" raco test racket/gui-tests/*.rkt

echo "== Chez oracle checks"
shopt -s nullglob
checks=(chez_scheme/oracle/tests/*.ss)
if [ ${#checks[@]} -eq 0 ]; then
  echo "run-tests: no Chez checks found in chez_scheme/oracle/tests/" >&2
  exit 1
fi
for t in "${checks[@]}"; do
  echo "-- $t"
  "$SCHEME" --script "$t"
done

echo "== all tests passed"

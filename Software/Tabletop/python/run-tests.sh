#!/usr/bin/env bash
# The entry point for Tabletop's tests, engine and GUI.
#
#   bash run-tests.sh            # the default suite, tests/ (the fast one: every test takes
#                                # under about two seconds on its own)
#   bash run-tests.sh --slow     # the slow suite alone, tests_slow/ (the multi-seed surveys,
#                                # the whole-run and window tests; minutes)
#   bash run-tests.sh --all      # both suites
#
# The tests run in parallel (-n auto) when pytest-xdist is installed (it is in the `test`
# extra: pip install -e '.[test]'), and one at a time otherwise. Qt always runs offscreen
# here, never on the real screen; the GUI tests (tests/gui/) skip without PySide6.
#
# Tests that read the source PDFs find them through FARGONAUTICA (a FARGonautica checkout),
# F95_PDF, FH91_PDF and FCCA_PDF, or an ancestor FARGonautica checkout; without them they skip.
#
# Any other arguments are passed to pytest (`bash run-tests.sh tests/test_rng.py -x`).
set -euo pipefail
cd "$(dirname "$0")"
export QT_QPA_PLATFORM=offscreen
unset WAYLAND_DISPLAY
args=()
if [[ "${1:-}" == "--all" ]]; then
  args+=(-m "" tests tests_slow)
  shift
elif [[ "${1:-}" == "--slow" ]]; then
  args+=(tests_slow)
  shift
fi
if python3 -c "import xdist" 2>/dev/null; then
  args+=(-n auto)
fi
exec python3 -m pytest -q "${args[@]}" "$@"

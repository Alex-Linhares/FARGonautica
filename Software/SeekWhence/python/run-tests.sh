#!/usr/bin/env bash
# The entry point for Seek-Whence's tests, engine and GUI.
#
#   bash run-tests.sh            # every test except the ones marked slow; about 45 s on
#                                # many cores with pytest-xdist, about 4 min without it
#   bash run-tests.sh --all      # every test, the slow ones too (whole Blackburn
#                                # sequences drawn event by event; about 4 more minutes
#                                # of CPU, in parallel when pytest-xdist is installed)
#   bash run-tests.sh --slow     # the slow tests alone
#
# The tests run in parallel (-n auto) when pytest-xdist is installed (it is in the
# `test` extra: pip install -e '.[test]'), and one at a time otherwise. Qt always runs
# offscreen here, never on the real screen; the GUI tests skip without PySide6.
#
# Any other arguments are passed to pytest (`bash run-tests.sh tests/test_hfilter.py -x`).
set -euo pipefail
cd "$(dirname "$0")"
export QT_QPA_PLATFORM=offscreen
unset WAYLAND_DISPLAY
args=()
if [[ "${1:-}" == "--all" ]]; then
  args+=(-m "")
  shift
elif [[ "${1:-}" == "--slow" ]]; then
  args+=(-m slow)
  shift
fi
if python3 -c "import xdist" 2>/dev/null; then
  args+=(-n auto)
fi
exec python3 -m pytest -q "${args[@]}" "$@"

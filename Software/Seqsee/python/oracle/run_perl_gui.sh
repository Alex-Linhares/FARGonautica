#!/usr/bin/env bash
# Run a Perl/Tk script against the original Seqsee lib/ on a private virtual X display
# (Xvfb), with run_perl.sh's determinism and memory cap. Usage: oracle/run_perl_gui.sh script.pl [args...]
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
exec xvfb-run -a -s "-screen 0 1280x1024x24" "$here/run_perl.sh" "$@"

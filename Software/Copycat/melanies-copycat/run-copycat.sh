#!/usr/bin/env bash
# Run Melanie Mitchell's Copycat headless on SBCL.
#
#   ./run-copycat.sh abc abd ijk            # one run, seeded from the clock
#   ./run-copycat.sh abc abd ijk 7          # one run with seed 7 (reproducible)
#   ./run-copycat.sh --sweep 100 abc abd xyz [FIRST-SEED] [LOG-FILE]
#                                           # 100 seeds, answer table; one line
#                                           # per run appended to LOG-FILE
#
# Exit status: 0 with an answer, 2 if the run ended without one, 1 on a
# Lisp error.  Run from anywhere.
set -u
cd "$(dirname "$0")" || exit 1
args=""
for a in "$@"; do args="$args \"$a\""; done
exec sbcl --noinform --non-interactive --no-userinit --no-sysinit \
     --load load.lisp \
     --eval "(copycat:run-cli (list $args))"

#!/usr/bin/env bash
# Tests for Mitchell's Copycat on SBCL.  Run from anywhere; exit 0 = all pass.
#
#   bash tests/run-tests.sh
#
# Each test is a fresh SBCL.  They take about a minute in all.
set -u
cd "$(dirname "$0")/.." || exit 1
fail=0

run() {
    local name="$1"; shift
    if "$@"; then
        echo "PASS: $name"
    else
        echo "FAIL: $name"
        fail=1
    fi
}

lisp() {
    sbcl --noinform --non-interactive --no-userinit --no-sysinit "$@"
}

# --- toolchain ---------------------------------------------------------------
run "sbcl on PATH" sbcl --version

# --- the archived original is untouched ---------------------------------------
# The archive is read-only (CLAUDE.md); this port works on the copy in src/.
run "Melanie-Mitchell-[LISP]/ unchanged in git" \
    git diff --quiet HEAD -- "../Melanie-Mitchell-[LISP]"

# --- src/ is the archive, byte for byte, except the edits in PORTING_NOTES.md --
run "src/ matches the archive (allowed differences listed in tests/src-diff-allowed.txt)" \
    bash tests/src-diff.sh

# --- the compatibility layer, on its own -------------------------------------
run "compat unit tests (Lucid shims, Flavors on CLOS)" \
    lisp --load tests/compat-tests.lisp

# --- the whole system loads with no error and every referenced function exists
run "load: no errors, no function left undefined" \
    lisp --load tests/load-tests.lisp

# --- seeded problems finish with an answer; a seed reproduces a run ---------
run "seeded runs (six problems) finish with an answer; reproducible" \
    lisp --load tests/run-tests.lisp

# --- the shell wrapper -------------------------------------------------------
run "run-copycat.sh abc abd ijk 1 prints an answer" \
    bash -c './run-copycat.sh abc abd ijk 1 > /tmp/ccat-wrapper-test.out && grep "^Answer: ijl$" /tmp/ccat-wrapper-test.out'

# --- Mitchell's batch tool, copystat, and its summary ------------------------
run "copystat + copystat-summary (3 runs into a temp dir)" \
    lisp --load tests/copystat-tests.lisp

if [ "$fail" -ne 0 ]; then
    echo "SOME TESTS FAILED"
    exit 1
fi
echo "ALL TESTS PASSED"
exit 0

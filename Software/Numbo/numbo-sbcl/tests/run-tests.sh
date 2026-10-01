#!/usr/bin/env bash
# Single test entry point.  Run from anywhere; exit 0 = all tests pass.
set -u
cd "$(dirname "$0")/.." || exit 1

fail=0

# Headless: init-chiffre turns graphics on when WINDOW_GFX is non-empty.
unset WINDOW_GFX

run() {
    local name="$1"; shift
    if "$@"; then
        echo "PASS: $name"
    else
        echo "FAIL: $name"
        fail=1
    fi
}

# --- toolchain ---------------------------------------------------------------
run "sbcl on PATH" sbcl --version

# --- ../numbo-digitized/ (the 1987 source) is read-only ---------------------
run "numbo-digitized/ unchanged" git diff --quiet -- ../numbo-digitized/

# --- package.lisp loads and defines NUMBO ------------------------------------
run "package.lisp loads" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --eval '(defvar cl-user::*numbo-no-autoload* t)' \
    --load src/load.lisp \
    --eval '(unless (cl-user::numbo-load (list "package")) (sb-ext:exit :code 1))' \
    --eval '(unless (find-package :numbo) (sb-ext:exit :code 1))' \
    --eval '(sb-ext:exit :code 0)'

# --- reader pass: every ported source file READs (no evaluation) ------------
run "reader pass (all source files READ)" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/read-pass.lisp

# --- Franz Lisp compatibility shims (src/franz-compat.lisp) ------------------
run "franz-compat unit tests" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/franz-compat-tests.lisp

# --- Flavors on CLOS (src/flavors-compat.lisp) + the real flavor files ------
run "flavors-compat unit tests" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/flavors-compat-tests.lisp

# --- Coderack (src/coderack.lisp, RECONSTRUCTED) ------------------------------
run "coderack unit tests" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/coderack-tests.lisp

# --- Graphics stubs (src/graphics-stubs.lisp) + undefined-function census -----
run "graphics stubs tests" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/graphics-tests.lisp

# --- Pnet files through compile-file; initialize-pnet (item 7) ----------------
run "pnet compile tests" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/pnet-compile-tests.lisp

# --- cyto-def + codelets through compile-file; whole-system census (item 8) ---
run "cyto/codelets compile tests" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/cyto-codelets-compile-tests.lisp

# --- full load, init-chiffre, 500 iterations of (config 31 3 5 24 3 14) (item 9)
run "boot smoke test (500 iterations, seed 31)" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/boot-tests.lisp

# --- run (config 31 3 5 24 3 14) to "Done :" and check the solution (item 10)
run "run to completion + solution checker (seed 18)" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/solution-tests.lisp

# --- validation against trace3.31 and the chapter's puzzles (item 11) --------
run "validation vs trace3.31 + chapter puzzles" sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load tests/validation-tests.lisp

# --- the README's commands, run as written (item 12) --------------------------
run "src/README.md commands" bash tests/readme-test.sh

if [ "$fail" -ne 0 ]; then
    echo "SOME TESTS FAILED"
    exit 1
fi
echo "ALL TESTS PASSED"
exit 0

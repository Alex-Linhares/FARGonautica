#!/usr/bin/env bash
# Every test in the repo: the Lisp port (lisp/), then the Python implementation
# (python/, whose tests run the Lisp port as their oracle).  Run from anywhere;
# exit 0 = all tests pass.
set -u
cd "$(dirname "$0")" || exit 1
fail=0

echo "=== lisp/ (SBCL port) ==="
bash lisp/tests/run-tests.sh || fail=1

echo "=== python/ (Python implementation) ==="
if python3 -m pytest python/tests -q; then
    echo "PASS: python tests"
else
    echo "FAIL: python tests"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    echo "SOME TESTS FAILED"
    exit 1
fi
echo "ALL TESTS PASSED"
exit 0

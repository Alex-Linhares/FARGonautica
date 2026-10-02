#!/usr/bin/env bash
# Oracle mode is opt-in (loop0002 item 1).  Checks that:
#  - a default load has none of src/oracle.lisp: CL's RANDOM, FLOAT and SQRT,
#    single floats, no NUMBO::*ORACLE*;
#  - NUMBO_ORACLE=1 turns oracle mode on, and NUMBO_ORACLE=0 / "" do not.
# Exit 0 if all pass.
set -u
cd "$(dirname "$0")/.." || exit 1
fail=0

default_checks='(progn
  (numbo::init-chiffre)
  (unless (and (eq (find-symbol "RANDOM" :numbo) (quote cl:random))
               (eq (find-symbol "FLOAT" :numbo) (quote cl:float))
               (eq (find-symbol "SQRT" :numbo) (quote cl:sqrt))
               (null (find-symbol "*ORACLE*" :numbo))
               (null (find-symbol "ORACLE-INSTALL" :numbo))
               (typep numbo::%first-decay-rate% (quote single-float))
               (null cl-user::*numbo-load-failures*))
    (sb-ext:exit :code 1))
  (sb-ext:exit :code 0))'

oracle_checks='(progn
  (unless (and (symbol-value (find-symbol "*ORACLE*" :numbo))
               (not (eq (find-symbol "RANDOM" :numbo) (quote cl:random)))
               (null cl-user::*numbo-load-failures*))
    (sb-ext:exit :code 1))
  (sb-ext:exit :code 0))'

check() {
    local name="$1"; shift
    if "$@" >/dev/null 2>&1; then echo "  ok: $name"; else echo "  FAIL: $name"; fail=1; fi
}

check "default load has no oracle hooks" \
    env -u NUMBO_ORACLE sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load src/load.lisp --eval "$default_checks"
check "NUMBO_ORACLE=0 is default mode" \
    env NUMBO_ORACLE=0 sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load src/load.lisp --eval "$default_checks"
check "NUMBO_ORACLE= (empty) is default mode" \
    env NUMBO_ORACLE= sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load src/load.lisp --eval "$default_checks"
check "NUMBO_ORACLE=1 is oracle mode" \
    env NUMBO_ORACLE=1 sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --load src/load.lisp --eval "$oracle_checks"
check "cl-user::*numbo-oracle* t is oracle mode" \
    env -u NUMBO_ORACLE sbcl --noinform --non-interactive --no-userinit --no-sysinit \
    --eval '(defvar cl-user::*numbo-oracle* t)' --load src/load.lisp --eval "$oracle_checks"

exit $fail

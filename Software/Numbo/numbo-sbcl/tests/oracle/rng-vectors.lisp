;;; rng-vectors.lisp -- write python/fixtures/rng_vectors.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/rng-vectors.lisp
;;; (or python/scripts/regen_fixtures.sh, which runs every capture script).
;;; The file holds the first 20 splitmix64 outputs for seeds 0, 1 and 18, and
;;; a sequence of (random n) calls per seed (src/oracle.lisp).  Running it
;;; twice gives identical files; tests/oracle-tests.lisp and
;;; python/tests/test_fixtures_current.py check that the committed file is
;;; what this script writes.

(load (merge-pathnames "common.lisp" *load-truename*))

(cl-user::write-fixture "rng_vectors.json"
                        (funcall (intern "ORACLE-RNG-VECTORS-JSON" :numbo)))

;;; full-run.lisp -- one full oracle run in this (fresh) SBCL process
;;; (loop0002 item 12: python/tests/test_full_runs.py).
;;;
;;; It lives in lib/, so python/scripts/regen_fixtures.sh does not run it as a
;;; capture script: the full traces are too big to commit, so the differential
;;; test regenerates them into a temporary directory instead.
;;;
;;; The caller defines the run before loading this file:
;;;   sbcl ... --eval '(defparameter cl-user::*full-run*
;;;                      (quote ((114 11 20 7 1 6) 1 20000 t "/tmp/x")))'
;;;            --load tests/oracle/lib/full-run.lisp
;;; i.e. (problem seed max-iterations rng-events base).  The run's JSON-lines
;;; trace goes to BASE.jsonl, and a JSON object with its oracle-run-config
;;; plist, what it printed, and check-solution's three values on what it
;;; printed (src/solution-checker.lisp; loop0002 item 13) goes to BASE.json.  Each run gets its own
;;; process, as in tests/oracle/main-loop.lisp: config does not reset every
;;; global (the free SETQs min, liste, ..., the pnode codelet lists).

(load (merge-pathnames "../common.lisp" *load-truename*))

(in-package :numbo)

(destructuring-bind (problem seed cap rng-events base) cl-user::*full-run*
  (let* ((output (make-string-output-stream))
         (result (let ((*standard-output* output))
                   (oracle-run-config problem :seed seed :max-iterations cap
                                              :trace (concatenate 'string base ".jsonl")
                                              :rng-events rng-events))))
    (with-open-file (s (concatenate 'string base ".json") :direction :output
                                                          :if-exists :supersede
                                                          :external-format :utf-8)
      (format s "{\"outcome\": ")
      (oracle-write-json-string (string-downcase (symbol-name (getf result :outcome))) s)
      (format s ", \"iterations\": ~d, \"problem-solved\": " (getf result :iterations))
      (oracle-write-data (getf result :problem-solved) s)
      (format s ", \"error\": ")
      (cl:if (getf result :error)
             (oracle-write-json-string (getf result :error) s)
             (write-string "null" s))
      (let ((text (get-output-stream-string output)))
        (format s ", \"output\": ")
        (oracle-write-json-string text s)
        (multiple-value-bind (valid reason expression) (check-solution text problem)
          (format s ", \"check\": [~:[false~;true~], " valid)
          (cl:if reason (oracle-write-json-string reason s) (write-string "null" s))
          (write-string ", " s)
          (cl:if expression (oracle-write-json-string expression s) (write-string "null" s))
          (write-string "]" s)))
      (format s "}~%"))))

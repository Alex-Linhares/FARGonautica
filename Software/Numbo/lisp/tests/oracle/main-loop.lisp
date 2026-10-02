;;; main-loop.lisp -- capture ../python/fixtures/main_loop.json (loop0002 item 11).
;;;
;;; Full oracle runs, (oracle-run-config problem :seed s :max-iterations cap
;;; :trace stream), each in a fresh SBCL process, so that no run sees the
;;; globals an earlier run left (config does not reset them all: the free
;;; SETQs min, liste, ..., the pnode codelet lists).  For each run the fixture
;;; holds the run's plist, what it printed (stdout, which starts at column 0),
;;; and its JSON-lines trace, one event per array element.
;;;
;;; Runs: the two the item names (puzzle 1 and puzzle 7, seed 1), puzzle 1
;;; again with the RNG draws in the trace, a capped run of puzzle 3 that goes
;;; past x = 400 (the high-temperature clause), a run of puzzle 6 that
;;; gives up (seed 18, 520 iterations: the retry, then the coderack empty),
;;; and puzzle 6 seed 1 capped at 1200, past the remove-dd EQ case at
;;; iteration 1176 that the Python first got wrong, and puzzle 3 seed 9
;;; capped at 400, hot at exactly x = 400.
;;;
;;; The parent process runs this file again once per run, with the run in the
;;; environment variable NUMBO_MAIN_LOOP_RUN; the child prints the run's JSON
;;; object on stdout.

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package :numbo)

(cl:defparameter *ml-runs*
  ;; (problem seed max-iterations rng-events)
  '(((114 11 20 7 1 6) 1 20000 nil)
    ((6 3 3 17 11 22) 1 20000 nil)
    ((114 11 20 7 1 6) 1 20000 t)
    ((31 3 5 24 3 14) 1 450 t)
    ((146 12 2 5 7 18) 18 20000 nil)
    ;; Iteration 1176: find-in's weights hold two equal doubles (two blocks
    ;; at 43.199999999999996); remove-dd's EQ must take the one at its index.
    ((146 12 2 5 7 18) 1 1200 nil)
    ;; Iteration 388 is x = 400 with a temperature over 200: (> x 400) is
    ;; false there, so the coderack is not emptied.
    ((31 3 5 24 3 14) 9 400 nil)))

(cl:defun ml-split-lines (text)
  (with-input-from-string (s text)
    (loop for line = (read-line s nil) while line collect line)))

(cl:defun ml-child (spec)
  "Run SPEC and write its JSON object to stdout."
  (destructuring-bind (problem seed cap rng-events) spec
    (let* ((trace (make-string-output-stream))
           (output (make-string-output-stream))
           (result (let ((*standard-output* output))
                     (oracle-run-config problem :seed seed :max-iterations cap
                                                :trace trace :rng-events rng-events)))
           (s *standard-output*))
      (format s "{\"problem\": ")
      (oracle-write-data problem s)
      (format s ", \"seed\": ~d, \"max-iterations\": ~d, \"rng-events\": ~:[false~;true~],~%"
              seed cap rng-events)
      (format s " \"result\": {\"outcome\": ")
      (oracle-write-json-string (string-downcase (symbol-name (getf result :outcome))) s)
      (format s ", \"iterations\": ~d, \"problem-solved\": " (getf result :iterations))
      (oracle-write-data (getf result :problem-solved) s)
      (format s ", \"error\": ")
      (cl:if (getf result :error)
             (oracle-write-json-string (getf result :error) s)
             (write-string "null" s))
      (format s "},~% \"output\": ")
      (oracle-write-json-string (get-output-stream-string output) s)
      (format s ",~% \"trace\": [")
      (loop for (line . more) on (ml-split-lines (get-output-stream-string trace))
            do (format s "~%  ~a~:[~;,~]" line more))
      (format s "]}"))))

(cl:defun ml-run-child (spec)
  (let ((out (make-string-output-stream)))
    (let ((process (sb-ext:run-program
                    "sbcl" (list "--noinform" "--non-interactive" "--no-userinit"
                                 "--no-sysinit" "--load" (namestring *load-truename*))
                    :search t :output out :error nil
                    :environment (append (list (format nil "NUMBO_MAIN_LOOP_RUN=~s" spec))
                                         (remove-if (lambda (e) (eql 0 (search "NUMBO_MAIN_LOOP_RUN=" e)))
                                                    (sb-ext:posix-environ))))))
      (unless (eql 0 (sb-ext:process-exit-code process))
        (error "main-loop.lisp: the child for ~s failed" spec)))
    (get-output-stream-string out)))

(let ((child (sb-ext:posix-getenv "NUMBO_MAIN_LOOP_RUN")))
  (cl:if (and child (plusp (length child)))
         (let ((*package* (cl:find-package :numbo)))
           (ml-child (read-from-string child)))
         (cl-user::write-fixture
          "main_loop.json"
          (with-output-to-string (s)
            (format s "{\"runs\": [")
            (loop for (spec . more) on *ml-runs*
                  do (format s "~%~a~:[~;,~]" (ml-run-child spec) more))
            (format s "~%]}~%")))))

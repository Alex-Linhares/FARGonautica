;;; harness.lisp -- a headless entry point for Mitchell's Copycat.
;;;
;;;   (copycat:run "abc" "abd" "ijk" :seed 1)
;;;
;;; runs init-ccat with graphics off and a seeded random state, then prints
;;; the rule, the translated rule, the answer, the final temperature and
;;; the number of codelets run.  Returns a plist with the same facts.
;;; (copycat:sweep ...) runs many seeds and tabulates the answers.
;;;
;;; Seeding.  Mitchell's init-ccat takes :random-state; with none it calls
;;; (make-random-state t), a fresh state from the clock, and either way it
;;; copies the state into *random-state-this-run* so copystat can save it
;;; and a run can be replayed exactly (util.l's get-random-state reads one
;;; back).  Every random draw in the program goes through CL:RANDOM on
;;; *random-state* (util.l's select-list-position, select-assoc,
;;; random-list-item, flip-coin, blur; coderack.l's :choose), so a seed
;;; determines the whole run.  Here a seed is an integer turned into a
;;; state with SB-EXT:SEED-RANDOM-STATE, SBCL's way of seeding its
;;; Mersenne Twister; the state object is what init-ccat gets.
;;;
;;; Nothing in this file touches Mitchell's code; it only calls it and reads
;;; her globals afterwards.

(cl:in-package :copycat)

(defun problem-symbol (x)
  "init-ccat takes its strings as symbols and reads their SYMBOL-NAME
(Lucid's reader upcased them); accept strings and symbols alike."
  (values (intern (string-upcase (string x)) :copycat)))

(defun rule-string (rule)
  (if rule
      (string-right-trim '(#\Newline #\Space)
                         (with-output-to-string (*standard-output*)
                           (send rule :print)))
      "none"))

(defun run (initial modified target
            &key seed (stream *standard-output*) (timeout nil) (trace nil))
  "Run Copycat on INITIAL -> MODIFIED :: TARGET -> ?, headless.
SEED (an integer) makes the run reproducible; without it the run is seeded
from the clock, as Mitchell's init-ccat does.  TIMEOUT, in seconds, gives
up on a run that has not found an answer by then (the result then has
:answer NIL).  TRACE T prints each codelet as it runs (%slightly-verbose%).
Prints a summary to STREAM (NIL for none) and returns a plist."
  (let ((random-state (if seed
                          (sb-ext:seed-random-state seed)
                          (make-random-state t)))
        (timed-out nil))
    (flet ((go-copycat ()
             (init-ccat (problem-symbol initial) (problem-symbol modified)
                        (problem-symbol target)
                        :random-state random-state :no-graphics t)))
      ;; init-constants resets %slightly-verbose% at the start of every run,
      ;; so a trace has to be switched on just after it.
      (when trace
        (sb-int:encapsulate 'init-constants 'copycat-trace
                            (lambda (fn) (cl:funcall fn)
                              (setq %slightly-verbose% t))))
      (unwind-protect
           (if timeout
               (handler-case (sb-ext:with-timeout timeout (go-copycat))
                 (sb-ext:timeout () (setq timed-out t)))
               (go-copycat))
        (when trace (sb-int:unencapsulate 'init-constants 'copycat-trace))))
    (let ((result (list :initial (string-downcase (string initial))
                        :modified (string-downcase (string modified))
                        :target (string-downcase (string target))
                        :seed seed
                        :answer (and *found-answer* (not timed-out)
                                     (send *answer-string* :pstring))
                        :rule (rule-string *rule*)
                        :translated-rule (rule-string *translated-rule*)
                        :temperature *temperature*
                        :codelets *codelet-count*
                        :snags *snag-count*
                        :timed-out timed-out)))
      (when stream
        (format stream "~&Problem: ~a -> ~a :: ~a -> ?~@[  (seed ~a)~]~%"
                (getf result :initial) (getf result :modified)
                (getf result :target) seed)
        (format stream "Rule: ~a~%" (getf result :rule))
        (format stream "Translated rule: ~a~%" (getf result :translated-rule))
        (format stream "Answer: ~a~%" (or (getf result :answer)
                                          (if timed-out "none (timed out)" "none")))
        (format stream "Temperature: ~a~%" (getf result :temperature))
        (format stream "Codelets run: ~a~%" (getf result :codelets))
        (format stream "Snags: ~a~%" (getf result :snags)))
      result)))

(defun sweep (initial modified target
              &key (seeds 20) (first-seed 1) (stream *standard-output*)
                (timeout 600) (log nil))
  "Run SEEDS seeds from FIRST-SEED and tabulate the answers.  One line per
run goes to LOG (a stream or pathname) if given; the table to STREAM.
Returns an alist ((answer . count) ...) sorted by count."
  (let ((results '()))
    (flet ((one (seed)
             (let ((r (run initial modified target :seed seed :stream nil
                                                   :timeout timeout)))
               (push r results)
               r)))
      (let ((seed-list (cl:loop for s from first-seed
                                repeat seeds collect s)))
        (if log
            (with-open-file (out log :direction :output :if-exists :append
                                     :if-does-not-exist :create)
              (dolist (seed seed-list)
                (let ((r (one seed)))
                  (format out "~a ~a ~a seed ~a answer ~a temperature ~a codelets ~a snags ~a~%"
                          (getf r :initial) (getf r :modified)
                          (getf r :target) seed
                          (or (getf r :answer) "NONE")
                          (getf r :temperature) (getf r :codelets)
                          (getf r :snags))
                  (finish-output out))))
            (dolist (seed seed-list) (one seed)))))
    (let ((table '()))
      (dolist (r results)
        (let* ((a (or (getf r :answer) "NONE"))
               (cell (assoc a table :test #'string=)))
          (if cell (incf (cdr cell)) (push (cons a 1) table))))
      (setq table (cl:sort table #'> :key #'cdr))
      (when stream
        (format stream "~&~a -> ~a :: ~a -> ?  (~a runs, seeds ~a-~a)~%"
                (string-downcase (string initial))
                (string-downcase (string modified))
                (string-downcase (string target))
                seeds first-seed (+ first-seed seeds -1))
        (dolist (cell table)
          (format stream "  ~8a ~4d  (~3d%)~%" (car cell) (cdr cell)
                  (round (* 100 (cdr cell)) seeds)))
        (format stream "  mean codelets ~d, mean final temperature ~d~%"
                (round (reduce #'+ results :key (lambda (r) (getf r :codelets)))
                       seeds)
                (round (reduce #'+ results :key (lambda (r) (getf r :temperature)))
                       seeds)))
      table)))

(defun run-cli (args)
  "run-copycat.sh's entry: ARGS is (initial modified target [seed]) or
(\"--sweep\" n initial modified target [first-seed] [log-file])."
  (handler-case
      (cond ((and args (string= (first args) "--sweep"))
             (destructuring-bind (n initial modified target
                                  &optional (first-seed "1") log)
                 (rest args)
               (sweep initial modified target
                      :seeds (parse-integer n)
                      :first-seed (parse-integer first-seed)
                      :log log)))
            ((<= 3 (length args) 4)
             (destructuring-bind (initial modified target &optional seed) args
               (let ((r (run initial modified target
                             :seed (and seed (parse-integer seed)))))
                 (unless (getf r :answer) (sb-ext:exit :code 2)))))
            (t (format *error-output*
                       "usage: run-copycat.sh INITIAL MODIFIED TARGET [SEED]~%~
                        ~7t run-copycat.sh --sweep N INITIAL MODIFIED TARGET [FIRST-SEED] [LOG-FILE]~%")
               (sb-ext:exit :code 64)))
    (error (c)
      (format *error-output* "~&copycat: error: ~a~%" c)
      (sb-ext:exit :code 1))))

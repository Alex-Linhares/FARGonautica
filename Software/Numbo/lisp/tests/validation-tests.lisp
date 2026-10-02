;;; validation-tests.lisp -- item 11: the port against the 1987 trace
;;; (trace3.31) and against the chapter's puzzles.
;;;
;;; Run: sbcl --non-interactive --load tests/validation-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;;  1. trace3.31 parses into the events PORTING_NOTES.md describes (48 node
;;;     events, 5 posts) and satisfies the event invariants of
;;;     tests/trace-tools.lisp; broken streams violate them (controls).
;;;  2. (config 31 3 5 24 3 14) with %verbose% t, seeds 1..20, 1000
;;;     iterations: every run's event stream satisfies the same invariants,
;;;     uses only the trace's kinds of event, and opens like the trace (the
;;;     first post is look-for-blx (30 3 10), often followed by (30 5 6)).
;;;     Seed 16 is the known reactivate-cyto/plinks error (item 10).
;;;  3. %verbose% nil (the default) prints no "About to post" lines.
;;;  4. The chapter's claims that the port reproduces (src/RESULTS.md):
;;;     #1 -> 20 x 6 - (7 - 1), #7 -> 3 + 3, #8 -> 2 x 5 + 1 (19/20),
;;;     #9 -> 6 x 20 - 2 - (16 - 14), all 20/20 seeds; #3 rarely (2/20).

(defvar cl-user::*numbo-no-autoload* t)
(defvar cl-user::*chapter-runs-no-report* t)
(let ((*error-output* (make-broadcast-stream))
      (*standard-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "../src/load.lisp" *load-truename*))
    (funcall (intern "NUMBO-LOAD" "CL-USER"))
    (load (merge-pathnames "trace-tools.lisp" *load-truename*))
    (load (merge-pathnames "chapter-runs.lisp" *load-truename*))))

(in-package :numbo-trace)

(defvar *failures* 0)
(defvar *checks* 0)

(defmacro check (form expected &key (test '#'equal))
  `(let ((got (handler-case ,form
                (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (funcall ,test got ,expected)
       (incf *failures*)
       (format t "~&FAIL: ~s~%  expected ~s~%  got      ~s~%" ',form ,expected got))))

(check cl-user::*numbo-load-failures* nil)

;;; --- 1. the 1987 trace --------------------------------------------------------
(defparameter *trace* (parse-events (trace3.31-text)))
(defparameter *trace-counts* (event-counts *trace*))

(check (getf *trace-counts* :created) 32)
(check (getf *trace-counts* :killed) 16)
(check (getf *trace-counts* :posts) 5)
(check (getf *trace-counts* :post-kinds) '(("look-for-blx" . 4) ("look-for-diff" . 1)))
(check (event-problems *trace* :complete nil) nil)
;; The trace stops mid-run (plus3-2-v13 has been created; its last line is
;; the end of page 2), and it never prints "Done :".
(check (car (last (first-ops *trace*))) "plus3-2-v13")
(check (first-ops *trace*) '("times3-3-v1" "plus3-24-v2" "plus27-4-v3" "plus4-1-v4"
                             "plus1-2-v5" "plus4-1-v6" "plus4-1-v7" "plus24-7-v8"
                             "plus7-7-v9" "plus5-2-v10" "plus2-3-v11" "plus2-5-v12"
                             "plus3-2-v13"))
(check (subseq (remove-if-not (lambda (e) (eq (car e) :post)) *trace*) 0 2)
       '((:post "look-for-blx" "(30 3 10)") (:post "look-for-blx" "(30 5 6)")))
(check (mapcar #'node-kind '("cyto-target" "cyto-brick3" "cyto-block27-v2"
                             "cyto-target-4-v3" "plus27-4-v3" "times3-3-v1" "foo"))
       '(:target :brick :block :dtarget :plus :times :unknown))

;; Controls: the invariants catch broken streams.
(defun ev (&rest specs)
  "(ev :c \"cyto-target\" :k \"plus1-2-v1\" :p \"look-for-blx\") => events."
  (loop for (k x) on specs by #'cddr
        collect (ecase k (:c (list :created x)) (:k (list :killed x)) (:p (list :post x "()")))))
(defparameter *puzzle*
  (ev :c "cyto-target" :c "cyto-brick1" :c "cyto-brick2" :c "cyto-brick3"
      :c "cyto-brick4" :c "cyto-brick5"))
(check (event-problems (append *puzzle* (ev :c "cyto-block9-v1" :c "times3-3-v1"
                                            :k "times3-3-v1" :k "cyto-block9-v1")))
       nil)
(check (length (event-problems (append *puzzle* (ev :c "cyto-block9-v2" :c "times3-3-v2")))) 1)
(check (length (event-problems (append *puzzle* (ev :c "cyto-block9-v1" :c "times3-3-v2")))) 2)
(check (length (event-problems (append *puzzle* (ev :c "times3-3-v1")))) 1)
(check (length (event-problems (append *puzzle* (ev :c "cyto-block9-v1" :c "times3-3-v1"
                                                    :k "cyto-block9-v1")))) 1)
(check (length (event-problems (append *puzzle* (ev :c "cyto-block9-v1" :c "times3-3-v1"
                                                    :k "times3-3-v1" :k "cyto-block9-v1"
                                                    :k "times3-3-v1" :k "cyto-block9-v1"))))
       2)
(check (length (event-problems (append *puzzle* (ev :k "cyto-brick1")))) 1)
(check (length (event-problems (append *puzzle* (ev :c "cyto-block9-v1"))))  1)
(check (event-problems (append *puzzle* (ev :c "cyto-block9-v1")) :complete nil) nil)
(check (length (event-problems (ev :c "cyto-block9-v1" :c "times3-3-v1"))) 2)

;;; --- 2. the port's verbose runs -------------------------------------------------
(defparameter *trace-kinds* '(:created :killed :post))
(defparameter *codelets-posted* '("look-for-blx" "look-for-diff" "look-for-bl+"))

(defun post-events (events) (remove-if-not (lambda (e) (eq (car e) :post)) events))

(let ((runs 0) (errors nil) (bad nil) (openings nil) (kinds nil) (codelets nil))
  (loop for seed from 1 to 20 do
    (multiple-value-bind (r out err)
        (run-captured '(31 3 5 24 3 14) :seed seed :max-iterations 1000 :verbose t)
      (declare (ignore r))
      (let ((events (parse-events out)))
        (incf runs)
        (when err (push (list seed err) errors))
        (let ((p (event-problems events :complete (null err))))
          (when p (push (list seed p) bad)))
        (push (list seed
                    (equal (first (post-events events)) (first (post-events *trace*)))
                    (equal (subseq (post-events events) 0 2) (subseq (post-events *trace*) 0 2)))
              openings)
        (dolist (e events)
          (pushnew (car e) kinds)
          (when (eq (car e) :post) (pushnew (cadr e) codelets :test #'string=))))))
  (check runs 20)
  (check (mapcar #'car errors) '(16))
  (check (search "does not handle the message :SET-ACTIVATION" (cadar errors)) 6
         :test (lambda (got want) (declare (ignore want)) (integerp got)))
  (check bad nil)
  ;; The first post is the trace's look-for-blx (30 3 10) on every seed but
  ;; 16 (where a derived target forms before the bricks are all read); 11/20
  ;; also post the trace's second, look-for-blx (30 5 6), in the same refresh.
  (check (mapcar #'car (remove-if #'second openings)) '(16))
  (check (count-if #'third openings) 11)
  (check (sort kinds #'string<) (sort (copy-list *trace-kinds*) #'string<))
  (check (sort codelets #'string<) (sort (copy-list *codelets-posted*) #'string<)))

;; The difference PORTING_NOTES.md records: the port posts many look-for-bl+
;; codelets; the trace's first 48 node events have none.
(multiple-value-bind (r out) (run-captured '(31 3 5 24 3 14) :seed 18 :max-iterations nil :verbose t)
  (let* ((events (parse-events out))
         (prefix (event-counts (node-event-prefix events 48))))
    (check (getf r :outcome) :solved)
    (check (event-problems events) nil)
    (check (> (post-count prefix "look-for-bl+") 0) t)
    (check (post-count prefix "look-for-blx") 12)))

;;; --- 3. %verbose% nil -----------------------------------------------------------
(multiple-value-bind (r out) (run-captured '(31 3 5 24 3 14) :seed 18 :max-iterations 300)
  (check (getf r :outcome) :capped)
  (check (count :post (parse-events out) :key #'car) 0)
  (check (> (count :created (parse-events out) :key #'car) 10) t))

;;; --- 4. the chapter's puzzles (20 seeds each, as in src/RESULTS.md) -------------
(defun solutions (res) (getf res :expressions))
(defun valid (res) (getf (getf res :counts) :valid))

(let ((r1 (run-puzzle 114 '(11 20 7 1 6))))
  (check (valid r1) 20)
  (check (solutions r1) '(("114 = (20 x 6) - (7 - 1)" . 20))))
(let ((r7 (run-puzzle 6 '(3 3 17 11 22))))
  (check (solutions r7) '(("6 = 3 + 3" . 20)))
  (check (<= (reduce #'max (getf r7 :iterations)) 40) t))
(let ((r8 (run-puzzle 11 '(2 5 1 25 23))))
  (check (valid r8) 20)
  (check (cdr (assoc "11 = (5 x 2) + 1" (solutions r8) :test #'string=)) 19))
(let ((r9 (run-puzzle 116 '(20 2 16 14 6))))
  (check (solutions r9) '(("116 = (20 x 6) - (2 + (16 - 14))" . 20))))
(let ((r3 (run-puzzle 31 '(3 5 24 3 14))))
  (check (getf r3 :counts) '(:valid 2 :invalid 0 :gave-up 16 :capped 1 :error 1))
  (check (solutions r3) '(("31 = (14 x (5 - 3)) + 3" . 2))))

;; canonical-solution
(check (canonical-solution "11 = 1 + (2 x 5)") "11 = (5 x 2) + 1")
(check (canonical-solution "116 = (6 x 20) - ((16 - 14) + 2)") "116 = (20 x 6) - (2 + (16 - 14))")
(check (canonical-solution "6 = 3 + 3") "6 = 3 + 3")

(format t "~&validation tests: ~a checks, ~a failures~%" *checks* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))

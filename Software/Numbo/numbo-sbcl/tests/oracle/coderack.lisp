;;; coderack.lisp -- write python/fixtures/coderack.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/coderack.lisp
;;; (or python/scripts/regen_fixtures.sh).
;;;
;;; Exact cases for src/coderack.lisp with the shared RNG (src/oracle.lisp);
;;; python/tests/test_coderack.py replays them on python/numbo/coderack.py:
;;;
;;;   levels    the urgency levels (create-coderack) makes after (init-chiffre)
;;;   ops       a scripted sequence of cr- calls (the cases of
;;;             tests/coderack-tests.lisp, made exact): each op with its
;;;             result (or :error), the (random n) draws it made as (n value),
;;;             the RNG state after it, and the bins of its rack after it
;;;   runs      every cr- call of real oracle runs (run-config, no trace), in
;;;             order: cr-make-coderack, cr-hang, cr-choose (with the RNG
;;;             state before it, since the codelets draw too) and
;;;             cr-empty-coderack, with results, draws, the rack's count
;;;             after each call and its full bins every 25 calls and at the end
;;;
;;; Values use the trace's Lisp-data encoding (src/oracle.lisp).
;;;
;;; This file is READ with SBCL's default float format (single), so every
;;; float literal below is written with d0.

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package :numbo)

(cl:defparameter *cr-levels* '(600 300 150 7 4 1 0))

(cl:defvar *cr-draws* nil)

(cl:defun cr-bins-field (name)
  "The bins of the rack NAME as (:data ((urgency form ...) ...)), or :null."
  (let ((rack (cl:and (symbolp name) name (get name 'coderack))))
    (cl:if rack
           (cons :data (cl:mapcar (cl:lambda (b) (cons (car b) (copy-list (cdr b))))
                                  (coderack-bins rack)))
           :null)))

(cl:defmacro with-draws (&body body)
  "Run BODY with the (random n) draws collected in *cr-draws* as (n value)."
  `(let* ((*cr-draws* nil)
          (*oracle-rng-sink* (cl:lambda (n v) (push (list n v) *cr-draws*))))
     ,@body))

(cl:defun cr-draws-field ()
  (cons :data (cl:or (reverse *cr-draws*) nil)))

;;; ---------------------------------------------------------------------------
;;; Scripted ops

(cl:defun cr-run-op (op)
  "Run OP, a list (kind . args), and return its fixture record."
  (destructuring-bind (kind &rest args) op
    (cl:if (eq kind :seed)
           (progn (oracle-seed (car args))
                  (list :object (cons "op" "seed") (cons "args" (cons :data args))
                        (cons "state" *oracle-rng-state*)))
           (let (result error)
             (with-draws
               (handler-case
                   (setq result
                         (ecase kind
                           (:make (cr-make-coderack (first args) (second args)))
                           (:hang (cr-hang (first args) (second args) (third args)))
                           (:choose (cr-choose (first args)))
                           (:choose-full (cr-choose (first args) t))
                           (:count (cr-count (first args)))
                           (:empty? (cr-empty? (first args)))
                           (:empty (cr-empty-coderack (first args)))))
                 (error () (setq error t)))
               (list :object
                     (cons "op" (string-downcase (symbol-name kind)))
                     (cons "args" (cons :data args))
                     (cons "result" (cl:if error :null (cons :data result)))
                     (cons "error" (cl:if error t :null))
                     (cons "draws" (cr-draws-field))
                     (cons "state" *oracle-rng-state*)
                     (cons "bins" (cr-bins-field (first args)))))))))

(cl:defun cr-script ()
  "The scripted ops, in order."
  (let ((ops nil))
    (flet ((op (&rest o) (push o ops)))
      ;; making, hanging, emptiness, clearing (coderack-tests.lisp)
      (op :seed 1987)
      (op :make 'test-rack *cr-levels*)
      (op :empty? 'test-rack) (op :count 'test-rack)
      (op :choose 'test-rack) (op :choose-full 'test-rack)
      (op :hang 'test-rack '(look-for-new-block) 4)
      (op :empty? 'test-rack) (op :count 'test-rack)
      (op :choose 'test-rack) (op :empty? 'test-rack)
      (op :hang 'test-rack '(kill-node x) 300)
      (op :choose-full 'test-rack)
      (cl:dotimes (i 10) (op :hang 'test-rack (list 'c i) (nth (mod i 7) *cr-levels*)))
      (op :count 'test-rack)
      (op :choose-full 'test-rack) (op :choose-full 'test-rack) (op :choose-full 'test-rack)
      (op :empty 'test-rack) (op :empty? 'test-rack) (op :choose 'test-rack)
      (op :empty 'test-rack)
      (op :hang 'test-rack '(again) 1) (op :choose 'test-rack)
      (op :hang 'test-rack '(old) 1)
      (op :make 'test-rack *cr-levels*) (op :empty? 'test-rack)
      (op :make 'other-rack '(10 1)) (op :hang 'other-rack '(o) 10)
      (op :empty? 'test-rack) (op :count 'other-rack)
      ;; duplicate levels are merged, first occurrence kept
      (op :make 'dup-rack '(4 1 4 0 1))
      ;; draining 60 codelets
      (cl:dotimes (i 60) (op :hang 'test-rack (list 'codelet i) (nth (mod i 7) *cr-levels*)))
      (cl:dotimes (i 61) (op :choose-full 'test-rack))
      ;; the same codelet twice
      (op :hang 'test-rack '(twice) 4) (op :hang 'test-rack '(twice) 4)
      (op :choose 'test-rack) (op :choose 'test-rack) (op :empty? 'test-rack)
      ;; urgency 0
      (cl:dotimes (trial 5)
        (op :hang 'test-rack '(zero) 0) (op :hang 'test-rack '(one) 1)
        (op :choose 'test-rack) (op :choose 'test-rack))
      (op :hang 'test-rack '(z1) 0) (op :hang 'test-rack '(z2) 0) (op :hang 'test-rack '(z3) 0)
      (op :empty? 'test-rack)
      (op :choose 'test-rack) (op :choose 'test-rack) (op :choose 'test-rack)
      ;; errors; a failed hang posts nothing
      (op :hang 'test-rack '(x) 5)
      (op :hang 'test-rack '(x) nil)
      (op :hang 'test-rack '(x) 4.0d0)          ; eql: 4.0 is not the level 4
      (op :hang 'no-such-rack '(x) 4)
      (op :choose 'no-such-rack)
      (op :empty? 'no-such-rack)
      (op :empty 'no-such-rack)
      (op :hang nil '(x) 4)
      (op :make 'bad-rack '(10 -1))
      (op :make 'bad-rack '(10 x))
      (op :make 'bad-rack '(10 nil))
      (op :make 'bad-rack '(10 t))
      (op :empty? 'test-rack)
      ;; float levels: hanging works, choosing calls (random 2.5d0), an error
      (op :make 'float-rack '(2.5d0 1 0.0d0))
      (op :hang 'float-rack '(f) 2.5d0)
      (op :hang 'float-rack '(i) 1)
      (op :hang 'float-rack '(z) 0.0d0)
      (op :hang 'float-rack '(z) 0)             ; 0 is not the level 0.0
      (op :choose 'float-rack)
      (op :count 'float-rack)
      ;; only a 0.0 level left: chosen without drawing
      (op :make 'float-rack '(2.5d0 1 0.0d0))
      (op :hang 'float-rack '(z) 0.0d0)
      (op :choose-full 'float-rack)
      ;; two urgency-0 levels (0 and 0.0): the first nonempty one is taken
      (op :make 'zero-rack '(1 0 0.0d0))
      (op :hang 'zero-rack '(zf) 0.0d0)
      (op :hang 'zero-rack '(z0) 0)
      (op :choose-full 'zero-rack) (op :choose-full 'zero-rack)
      ;; weighted first choices from fresh racks
      (op :seed 18)
      (cl:dolist (postings '((((a) 300) ((b) 150) ((c) 7))
                             (((four 1) 4) ((four 2) 4) ((four 3) 4) ((seven) 7))
                             (((u) 600) ((f1) 300) ((s) 150) ((t3) 7) ((f4) 4) ((f5) 1))
                             (((x) 0) ((y) 4) ((z) 1))))
        (cl:dotimes (trial 40)
          (op :empty 'test-rack)
          (cl:dolist (p postings) (op :hang 'test-rack (car p) (cadr p)))
          (op :choose 'test-rack)))
      ;; reproducibility: choice sequences for seeds 31 and 32
      (cl:dolist (s '(31 32))
        (op :seed s)
        (op :empty 'test-rack)
        (cl:dotimes (i 40) (op :hang 'test-rack (list 'c i) (nth (mod i 6) *cr-levels*)))
        (cl:dotimes (i 40) (op :choose-full 'test-rack))))
    (nreverse ops)))

;;; ---------------------------------------------------------------------------
;;; Real runs

(cl:defvar *cr-calls* nil)
(cl:defvar *cr-capturing* nil)

(cl:defun cr-rack-name () (symbol-value '*coderack*))

(cl:defun cr-record (fields name)
  (push (append (list :object) fields
                (list (cons "count" (cl:if (cl:and (symbolp name) (get name 'coderack))
                                           (cr-count name) :null))))
        *cr-calls*)
  (when (zerop (mod (length *cr-calls*) 25))
    (setf (car *cr-calls*)
          (append (car *cr-calls*) (list (cons "bins" (cr-bins-field name)))))))

(cl:defun cr-capture-make (fn name urgencies)
  (let ((result (funcall fn name urgencies)))
    (cr-record (list (cons "op" "make") (cons "args" (cons :data (list name urgencies)))
                     (cons "result" (cons :data result)))
               name)
    result))

(cl:defun cr-capture-hang (fn name form urgency)
  (let ((result (funcall fn name form urgency)))
    (cr-record (list (cons "op" "hang") (cons "args" (cons :data (list name form urgency))))
               name)
    result))

(cl:defun cr-capture-choose (fn name &optional full)
  (let ((before *oracle-rng-state*) result)
    (with-draws
      (setq result (funcall fn name full))
      (cr-record (list (cons "op" (cl:if full "choose-full" "choose"))
                       (cons "args" (cons :data (list name)))
                       (cons "state_before" before)
                       (cons "result" (cons :data result))
                       (cons "draws" (cr-draws-field))
                       (cons "state" *oracle-rng-state*))
                 name))
    result))

(cl:defun cr-capture-empty (fn name)
  (let ((result (funcall fn name)))
    (cr-record (list (cons "op" "empty") (cons "args" (cons :data (list name)))
                     (cons "result" (cons :data result)))
               name)
    result))

(cl:defparameter *cr-captures*
  '((cr-make-coderack cr-capture-make)
    (cr-hang cr-capture-hang)
    (cr-choose cr-capture-choose)
    (cr-empty-coderack cr-capture-empty)))

(cl:defun cr-real-run (problem seed max-iterations)
  (let ((*cr-calls* nil) result)
    (cl:dolist (c *cr-captures*)
      (sb-int:encapsulate (first c) 'cr-capture (symbol-function (second c))))
    (unwind-protect
         (let ((*standard-output* (make-broadcast-stream)))
           (setq result (oracle-run-config problem :seed seed :max-iterations max-iterations)))
      (cl:dolist (c *cr-captures*)
        (sb-int:unencapsulate (first c) 'cr-capture)))
    (setf (car *cr-calls*)
          (append (cl:remove "bins" (car *cr-calls*)
                             :key (cl:lambda (f) (cl:and (consp f) (car f)))
                             :test #'equal)
                  (list (cons "bins" (cr-bins-field (cr-rack-name))))))
    (list :object
          (cons "problem" (cons :data problem))
          (cons "seed" seed)
          (cons "max_iterations" max-iterations)
          (cons "outcome" (cons :data (getf result :outcome)))
          (cons "calls" (cons :list (reverse *cr-calls*))))))

;;; ---------------------------------------------------------------------------

(let* ((runs (list (cr-real-run '(114 11 20 7 1 6) 1 20000)
                   (cr-real-run '(31 3 5 24 3 14) 1 400)
                   (cr-real-run '(146 12 2 5 7 18) 2 20000)))
       ;; after the runs: (init-chiffre) has set the urgencies
       (levels (progn (create-coderack)
                      (cl:mapcar #'car (coderack-bins (cr-get (symbol-value '*coderack*))))))
       (ops (cl:mapcar #'cr-run-op (cr-script))))
  (cl-user::write-fixture
   "coderack.json"
   (with-output-to-string (s)
     (oracle-write-object
      (list (cons "levels" (cons :data levels))
            (cons "ops" (cons :list ops))
            (cons "runs" (cons :list runs)))
      s)
     (terpri s))))

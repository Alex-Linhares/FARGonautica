;;; coderack-tests.lisp -- unit tests for src/coderack.lisp (RECONSTRUCTED).
;;;
;;; Run: sbcl --non-interactive --load tests/coderack-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.  The RNG is seeded, so runs
;;; are reproducible.

(defvar cl-user::*numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(unless (cl-user::numbo-load '("package" "franz-compat" "flavors-compat" "coderack"))
  (format t "~&coderack: failed to load~%")
  (sb-ext:exit :code 1))

(in-package :numbo)

(defvar *failures* 0)
(defvar *checks* 0)

(defmacro check (form expected &key (test '#'equal))
  `(let ((got (handler-case ,form
                (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (funcall ,test got ,expected)
       (incf *failures*)
       (format t "~&FAIL: ~s~%  expected ~s~%  got      ~s~%" ',form ,expected got))))

(defmacro check-error (form)
  `(progn
     (incf *checks*)
     (unless (handler-case (progn ,form nil) (error () t))
       (incf *failures*)
       (format t "~&FAIL: expected an error from ~s~%" ',form))))

(defun seed (n) (setq *random-state* (sb-ext:seed-random-state n)))
(seed 1987)

;; init-chiffre's levels
(defparameter *levels* '(600 300 150 7 4 1 0))

;;; --- making, hanging, emptiness, clearing --------------------------------------
(check (cr-make-coderack 'test-rack *levels*) 'test-rack)
(check (cr-empty? 'test-rack) t)
(check (cr-count 'test-rack) 0)
(check (cr-choose 'test-rack) nil)
(check (cr-choose 'test-rack t) nil)
(check (cr-hang 'test-rack '(look-for-new-block) 4) '(look-for-new-block))
(check (cr-empty? 'test-rack) nil)
(check (cr-count 'test-rack) 1)
;; the chosen codelet is removed and returned, ready to EVAL
(check (cr-choose 'test-rack) '(look-for-new-block))
(check (cr-empty? 'test-rack) t)
;; (cr-choose rack t), as in start.lisp's commented-out call: (form urgency)
(cr-hang 'test-rack '(kill-node x) 300)
(check (cr-choose 'test-rack t) '((kill-node x) 300))
;; the coderack is named by a symbol held in a variable, as in the source
(defvar *coderack*)
(setq *coderack* 'test-rack)
(cr-hang *coderack* (list '+ 1 2) 150)
(check (eval (cr-choose *coderack*)) 3)
;; clearing
(dotimes (i 10) (cr-hang 'test-rack (list 'c i) (nth (mod i 7) *levels*)))
(check (cr-count 'test-rack) 10)
(check (cr-empty-coderack 'test-rack) 'test-rack)
(check (cr-empty? 'test-rack) t)
(check (cr-choose 'test-rack) nil)
(check (cr-empty-coderack 'test-rack) 'test-rack)   ; clearing an empty rack is fine
;; usable again after clearing
(cr-hang 'test-rack '(again) 1)
(check (cr-choose 'test-rack) '(again))
;; remaking a rack starts it empty
(cr-hang 'test-rack '(old) 1)
(cr-make-coderack 'test-rack *levels*)
(check (cr-empty? 'test-rack) t)
;; two racks are independent
(cr-make-coderack 'other-rack '(10 1))
(cr-hang 'other-rack '(o) 10)
(check (cr-empty? 'test-rack) t)
(check (cr-count 'other-rack) 1)

;;; --- draining: every codelet comes out exactly once ----------------------------
(let ((posted (loop for i below 60 collect (list 'codelet i))))
  (loop for f in posted for i from 0 do (cr-hang 'test-rack f (nth (mod i 7) *levels*)))
  (check (cr-count 'test-rack) 60)
  (let ((got (loop until (cr-empty? 'test-rack) collect (cr-choose 'test-rack))))
    (check (length got) 60)
    (check (null (set-exclusive-or got posted :test #'equal)) t)
    (check (cr-choose 'test-rack) nil)))
;; the same codelet posted twice is two codelets
(cr-hang 'test-rack '(twice) 4)
(cr-hang 'test-rack '(twice) 4)
(check (cr-count 'test-rack) 2)
(check (list (cr-choose 'test-rack) (cr-choose 'test-rack)) '((twice) (twice)))
(check (cr-empty? 'test-rack) t)

;;; --- urgency 0 ------------------------------------------------------------------
;; a 0-urgency codelet is never chosen while any positive-urgency codelet waits
(let ((ok t))
  (dotimes (trial 200)
    (cr-empty-coderack 'test-rack)
    (cr-hang 'test-rack '(zero) 0)
    (cr-hang 'test-rack '(one) 1)
    (unless (equal (cr-choose 'test-rack) '(one)) (setq ok nil))
    (unless (equal (cr-choose 'test-rack) '(zero)) (setq ok nil))
    (unless (cr-empty? 'test-rack) (setq ok nil)))
  (check ok t))
;; only 0-urgency codelets: they still come out (rack is not empty)
(cr-empty-coderack 'test-rack)
(cr-hang 'test-rack '(z1) 0)
(cr-hang 'test-rack '(z2) 0)
(check (cr-empty? 'test-rack) nil)
(check (null (set-exclusive-or (list (cr-choose 'test-rack) (cr-choose 'test-rack))
                               '((z1) (z2)) :test #'equal))
       t)
(check (cr-empty? 'test-rack) t)

;;; --- errors ---------------------------------------------------------------------
(check-error (cr-hang 'test-rack '(x) 5))        ; not a level of this rack
(check-error (cr-hang 'test-rack '(x) nil))
(check-error (cr-hang 'no-such-rack '(x) 4))
(check-error (cr-choose 'no-such-rack))
(check-error (cr-empty? 'no-such-rack))
(check-error (cr-make-coderack 'bad-rack '(10 -1)))
(check-error (cr-make-coderack 'bad-rack '(10 x)))
(check (cr-empty? 'test-rack) t)                 ; a failed hang posts nothing

;;; --- weighted selection (statistical) ---------------------------------------------
;;; P(codelet) = urgency / total urgency on the rack (chapter p.143).  Each
;;; observed frequency must be within 4.5 standard deviations of its expected
;;; value (seeded, so this is deterministic; a correct rack fails a 4.5-sigma
;;; check about once in 150,000 cases).
(defun tally-first-choice (postings trials)
  ;; POSTINGS: list of (form urgency).  Returns an alist form -> count of
  ;; times it was the first codelet chosen from a fresh rack.
  (let ((counts (mapcar (lambda (p) (cons (car p) 0)) postings)))
    (dotimes (i trials counts)
      (cr-empty-coderack 'test-rack)
      (dolist (p postings) (cr-hang 'test-rack (car p) (cadr p)))
      (incf (cdr (assoc (cr-choose 'test-rack) counts :test #'equal))))))

(defun check-proportional (postings trials)
  (let ((total (reduce #'+ postings :key #'cadr))
        (counts (tally-first-choice postings trials)))
    (dolist (p postings)
      (let* ((prob (cl:/ (cadr p) total))   ; NUMBO / is Franz integer division
             (expected (* trials prob))
             (sd (sqrt (* trials prob (- 1 prob))))
             (got (cdr (assoc (car p) counts :test #'equal))))
        (incf *checks*)
        (unless (<= (abs (- got expected)) (max 1 (* 4.5 sd)))
          (incf *failures*)
          (format t "~&FAIL: weighted choice of ~s (urgency ~s of ~s): ~
                     expected ~,1f +- ~,1f, got ~d in ~d trials~%"
                  (car p) (cadr p) total expected sd got trials))))))

;; one codelet per bin
(check-proportional '(((a) 300) ((b) 150) ((c) 7)) 20000)
;; several codelets in one bin: each (four) has chance 4/19, (seven) 7/19
(check-proportional '(((four 1) 4) ((four 2) 4) ((four 3) 4) ((seven) 7)) 20000)
;; all of init-chiffre's positive levels at once
(check-proportional '(((u) 600) ((f1) 300) ((s) 150) ((t3) 7) ((f4) 4) ((f5) 1)) 40000)
;; a codelet at urgency 0 is never chosen first
(check-proportional '(((x) 0) ((y) 4) ((z) 1)) 5000)

;;; Choosing without replacement: with (a)@300 and (b)@150, (a) comes out
;;; first 2/3 of the time, and (b) is always the one left.
(let ((second-b 0) (trials 2000))
  (dotimes (i trials)
    (cr-empty-coderack 'test-rack)
    (cr-hang 'test-rack '(a) 300)
    (cr-hang 'test-rack '(b) 150)
    (when (equal (cr-choose 'test-rack) '(a))
      (when (equal (cr-choose 'test-rack) '(b)) (incf second-b))))
  (incf *checks*)
  (unless (< (abs (- second-b (* 2/3 trials))) (* 4.5 (sqrt (* trials 2/9))))
    (incf *failures*)
    (format t "~&FAIL: (a) then (b) ~d times in ~d trials, expected ~~~d~%"
            second-b trials (cl:round (* 2/3 trials)))))

;;; --- reproducibility: the same seed gives the same choices ------------------------
(defun choice-sequence (seed-value)
  (seed seed-value)
  (cr-empty-coderack 'test-rack)
  (loop for i below 40 do (cr-hang 'test-rack (list 'c i) (nth (mod i 6) *levels*)))
  (loop until (cr-empty? 'test-rack) collect (cadr (cr-choose 'test-rack))))
(check (equal (choice-sequence 31) (choice-sequence 31)) t)
(check (equal (choice-sequence 31) (choice-sequence 32)) nil)

;;; --- the source's CREATE-CODERACK (codelets.lisp) -------------------------------
;;; Load the real files up to codelets.lisp and run create-coderack with
;;; init-chiffre's urgency values (init.lisp does not load yet).
(unless (cl-user::numbo-load '("pnet-def" "pnet-functions" "cyto-def" "codelets"))
  (incf *failures*)
  (format t "~&FAIL: could not load the source files for create-coderack~%"))
(defvar %upper-urgency%)  (setq %upper-urgency% 600)
(defvar %first-urgency%)  (setq %first-urgency% 300)
(defvar %second-urgency%) (setq %second-urgency% 150)
(defvar %third-urgency%)  (setq %third-urgency% 7)
(defvar %fourth-urgency%) (setq %fourth-urgency% 4)
(defvar %fifth-urgency%)  (setq %fifth-urgency% 1)
(check (create-coderack) 'my-coderack)
(check *coderack* 'my-coderack)
(check (mapcar #'car (coderack-bins (cr-get *coderack*))) '(600 300 150 7 4 1 0))
(check (cr-empty? *coderack*) t)
;; source-shaped calls
(cr-hang *coderack* '(look-for-new-block) %fourth-urgency%)
(cr-hang *coderack* (list 'compare-b-to-t 'cyto-brick1) %upper-urgency%)
(check (cr-count *coderack*) 2)
(cr-empty-coderack *coderack*)
(check (cr-empty? *coderack*) t)

(format t "~&coderack: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (cl:if (zerop *failures*) 0 1))

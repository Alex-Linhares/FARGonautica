;;; solution-tests.lisp -- item 10: run to completion.
;;;
;;; Run: sbcl --non-interactive --load tests/solution-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;;  1. The solution checker (src/solution-checker.lisp) on hand-written
;;;     decompositions: valid ones (+, -, x, and the exact division a TIMES
;;;     node gives a derived target) and every way of being invalid (brick
;;;     used twice, wrong brick value, bad arithmetic, an operand that is
;;;     never derived, no "Done :").
;;;  2. (config 31 3 5 24 3 14) with seed 18 runs to "Done :" (1328 main-loop
;;;     iterations) and the printed decomposition passes the checker:
;;;     31 = (14 x (5 - 3)) + 3.  Reproducible.
;;;  3. Seed 93 also reaches "Done :", but its decomposition goes through a
;;;     block that was killed after a derived target was built on it, which
;;;     the checker rejects.  That is 1987 behaviour (codelets.lisp
;;;     `kill-block', scan PDF p.49; see PORTING_NOTES.md, item 10).
;;;  4. The chapter's easy puzzles (#7, #8 and the sample run #1) are solved
;;;     with the solutions the chapter reports.

(defvar cl-user::*numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(cl-user::numbo-load)

(in-package :numbo)

(cl:defvar *failures* 0)
(cl:defvar *checks* 0)

(defmacro check (form expected &key (test '#'equal))
  `(let ((got (handler-case ,form
                (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (funcall ,test got ,expected)
       (incf *failures*)
       (format t "~&FAIL: ~s~%  expected ~s~%  got      ~s~%" ',form ,expected got))))

(check cl-user::*numbo-load-failures* nil)

;;; --- 1. the checker --------------------------------------------------------------
(cl:defun checked (output problem)
  "(valid-p reason expression) as a list."
  (multiple-value-list (check-solution output problem)))

(cl:defun substitute-string (s old new)
  "S with the first OLD replaced by NEW."
  (let ((p (search old s)))
    (cl:if p (concatenate 'string (subseq s 0 p) new (subseq s (+ p (length old)))) s)))

(cl:defun reason-has (output problem text)
  (let ((r (checked output problem)))
    (and (null (first r)) (stringp (second r)) (search text (second r)) t)))

(cl:defparameter *seed-18-text* "Node PLUS28-3-V30 created
Done : Operation PLUS28-3-V30 has been applied
to CYTO-BLOCK28-V29 ( 28) and to CYTO-BRICK1 ( 3)
to get CYTO-TARGET
Operation TIMES2-14-V29 has been applied
to CYTO-BRICK5 ( 14) and to CYTO-BLOCK2-V28 ( 2)
to get CYTO-BLOCK28-V29
Operation PLUS2-3-V28 has been applied
to CYTO-BRICK4 ( 3) and to CYTO-BRICK2 ( 5)
to get CYTO-BLOCK2-V28
")
(cl:defparameter *p3* '(31 3 5 24 3 14))

(check (checked *seed-18-text* *p3*) '(t nil "31 = (14 x (5 - 3)) + 3"))
;; the same tree with brick 4 replaced by brick 1 in 5 - 3 uses brick 1 twice
(check (reason-has (substitute-string *seed-18-text* "CYTO-BRICK4 ( 3)" "CYTO-BRICK1 ( 3)")
                   *p3* "CYTO-BRICK1 is used twice")
       t)
;; a printed brick value that is not the brick's
(check (reason-has (substitute-string *seed-18-text* "CYTO-BRICK2 ( 5)" "CYTO-BRICK3 ( 5)")
                   *p3* "brick 3 is 24")
       t)
;; bad arithmetic: 14 x 2 is not 27
(check (reason-has (substitute-string
                    (substitute-string *seed-18-text* "CYTO-BLOCK28-V29 ( 28)"
                                       "CYTO-BLOCK28-V29 ( 27)")
                    "PLUS28-3" "PLUS27-3")
                   *p3* "cannot give")
       t)
;; the root must be the target: the same tree does not solve target 32
(check (reason-has *seed-18-text* '(32 3 5 24 3 14) "cannot give") t)
(check (reason-has "Node CYTO-TARGET created" *p3* "no \"Done :\"") t)
(check (reason-has "Done : " *p3* "no operation") t)
(check (reason-has "Done : Operation PLUS2-3-V1 has been applied to" *p3* "truncated") t)

;; seed 93's decomposition: 9 (block 3 x 3) was killed, so it is never expanded
(cl:defparameter *seed-93-text* "Done : Operation PLUS24-7-V1 has been applied
to CYTO-TARGET-7-V1 ( 7) and to CYTO-BRICK3 ( 24)
to get CYTO-TARGET
Operation PLUS7-2-V4 has been applied
to CYTO-BLOCK9-V3 ( 9) and to CYTO-BLOCK2-V6 ( 2)
to get CYTO-TARGET-7-V1
Operation PLUS2-3-V6 has been applied
to CYTO-BRICK4 ( 3) and to CYTO-BRICK2 ( 5)
to get CYTO-BLOCK2-V6
")
(check (reason-has *seed-93-text* *p3* "CYTO-BLOCK9-V3 (9) is used but never derived") t)

;; TIMES on a derived target (decompx): 5 = 10 / 2
(check (checked "Done : Operation TIMES4-5-V1 has been applied
to CYTO-TARGET-5-V1 ( 5) and to CYTO-BRICK1 ( 4)
to get CYTO-TARGET
Operation TIMES2-5-V3 has been applied
to CYTO-BRICK3 ( 2) and to CYTO-BLOCK10-V2 ( 10)
to get CYTO-TARGET-5-V1
Operation PLUS1-9-V2 has been applied
to CYTO-BRICK4 ( 1) and to CYTO-BRICK5 ( 9)
to get CYTO-BLOCK10-V2
" '(20 4 6 2 1 9))
       '(t nil "20 = ((1 + 9) / 2) x 4"))
;; "Obvious." (replace-target): the root is a block equal to the target
(check (checked "Obvious. Done : Operation PLUS3-3-V1 has been applied
to CYTO-BRICK1 ( 3) and to CYTO-BRICK2 ( 3)
to get CYTO-BLOCK6-V1
" '(6 3 3 17 11 22))
       '(t nil "6 = 3 + 3"))

;;; --- 2. (config 31 3 5 24 3 14) runs to completion --------------------------------
(cl:defun solve-run (problem seed &optional (cap 20000))
  "Run PROBLEM; return (result output)."
  (let* (result
         (output (with-output-to-string (*standard-output*)
                   (setq result
                         (handler-case (run-config problem :seed seed :max-iterations cap)
                           (error (c) (list :error (princ-to-string c))))))))
    (list result output)))

(cl:defparameter *t0* (get-internal-real-time))
(cl:defparameter *run* (solve-run *p3* 18))
(format t "~&solution: (config 31 3 5 24 3 14) seed 18 in ~,1fs~%"
        (cl:/ (- (get-internal-real-time) *t0*) internal-time-units-per-second 1.0))
(check (first *run*) '(:outcome :solved :iterations 1328 :seed 18 :problem-solved 1))
(check (and (search "Done : Operation " (second *run*)) t) t)
(check (checked (second *run*) *p3*) '(t nil "31 = (14 x (5 - 3)) + 3"))
(format t "~&solution: ~a~%" (third (checked (second *run*) *p3*)))
(check (string= (second *run*) (second (solve-run *p3* 18))) t)

;;; --- 3. a "Done :" the checker rejects (1987 kill-block behaviour) ---------------
(cl:defparameter *run-93* (solve-run *p3* 93))
(check (getf (first *run-93*) :outcome) :solved)
(check (reason-has (second *run-93*) *p3* "CYTO-BLOCK9-V3 (9) is used but never derived") t)
(check (and (search "Node CYTO-BLOCK9-V3 killed" (second *run-93*)) t) t)

;;; --- 4. the chapter's easy puzzles ------------------------------------------------
;; #7 "Numbo immediately comes up with the solution 3 + 3" (p.152-153)
(cl:defparameter *run-7* (solve-run '(6 3 3 17 11 22) 1 500))
(check (getf (first *run-7*) :outcome) :solved)
(check (checked (second *run-7*) '(6 3 3 17 11 22)) '(t nil "6 = 3 + 3"))
;; #8 "it will immediately answer 2 x 5 + 1" (p.153)
(cl:defparameter *run-8* (solve-run '(11 2 5 1 25 23) 1 500))
(check (getf (first *run-8*) :outcome) :solved)
(check (checked (second *run-8*) '(11 2 5 1 25 23)) '(t nil "11 = (2 x 5) + 1"))
;; #1, the sample run of Fig. III-3 (p.144): 20 x 6 = 120, 7 - 1 = 6, 120 - 6
(cl:defparameter *run-1* (solve-run '(114 11 20 7 1 6) 1 500))
(check (getf (first *run-1*) :outcome) :solved)
(check (checked (second *run-1*) '(114 11 20 7 1 6))
       '(t nil "114 = (20 x 6) - (7 - 1)"))

(format t "~&solution: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (cl:if (zerop *failures*) 0 1))

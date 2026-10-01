;;; harness.lisp -- run CONFIG headless, with a fixed seed and an iteration cap.
;;;
;;; Not part of the 1987 source.  CONFIG (start.lisp) is unchanged: its main
;;; loop is (do ((y 0 (add1 y))) ...) with (setq *iteration* y) at the top of
;;; every iteration, and it ends only when *problem-solved* is 1 ("Done :")
;;; or when the coderack is empty twice ((return)).  To stop it after N
;;; iterations without editing it, the harness encapsulates the two functions
;;; the loop body always calls after setting *iteration*: MOD (the (mod x 40)
;;; / (mod x 20) / (mod x 5) tests) and CR-EMPTY-CODERACK (the high-temperature
;;; clause, which skips the MOD tests).  The first such call that sees
;;; *iteration* = N throws out of CONFIG, so iterations 0 .. N-1 have run.
;;; Calls to MOD made by codelets during iteration N-1 see N-1 and pass.
;;;
;;; (run-config '(31 3 5 24 3 14) :seed 1 :max-iterations 500)
;;;   => a plist (:outcome :capped|:solved|:gave-up  :iterations n
;;;               :seed s  :problem-solved p)
;;;   :solved    CONFIG returned with *problem-solved* = 1 (printed "Done :")
;;;   :gave-up   CONFIG returned otherwise: the coderack was empty after the
;;;              last retry (start.lisp's (t (return)))
;;;   :capped    the cap was reached first.
;;;
;;; The seed goes through SB-EXT:SEED-RANDOM-STATE into *RANDOM-STATE*, which
;;; every random choice uses (coderack.lisp, the codelets' RANDOM calls).

(in-package :numbo)

(cl:defvar *iteration-cap* nil
  "When non-nil, CONFIG is stopped at the start of iteration *ITERATION-CAP*.")

(cl:defun iteration-cap-check ()
  (when (and *iteration-cap* (>= *iteration* *iteration-cap*))
    (throw 'iteration-cap *iteration*)))

(cl:defun install-iteration-cap ()
  (dolist (f '(mod cr-empty-coderack))
    (unless (sb-int:encapsulated-p f 'iteration-cap)
      (sb-int:encapsulate f 'iteration-cap
                          (lambda (fn &rest args)
                            (iteration-cap-check)
                            (apply fn args))))))

(cl:defun run-config (problem &key (seed 1) (max-iterations 500) verbose)
  "Run (init-chiffre) then (apply #'config PROBLEM) with *RANDOM-STATE* seeded
from SEED, stopping after MAX-ITERATIONS main-loop iterations (nil = no cap).
VERBOSE non-nil sets %verbose% to t after init-chiffre (which sets it to nil),
as trace3.31 was made: it adds the \"About to post codelet\" lines."
  (install-iteration-cap)
  (setq *random-state* (sb-ext:seed-random-state seed))
  (setq *iteration* 0)
  (let ((*iteration-cap* nil))
    (init-chiffre))
  (when verbose (setq %verbose% t))
  (let* ((*iteration-cap* max-iterations)
         (capped t)
         (result (catch 'iteration-cap
                   (apply #'config problem)
                   (setq capped nil))))
    (declare (ignore result))
    (list :outcome (cond (capped :capped)
                         ((eql *problem-solved* 1) :solved)
                         (t :gave-up))
          :iterations (cl:if capped max-iterations (1+ *iteration*))
          :seed seed
          :problem-solved *problem-solved*)))

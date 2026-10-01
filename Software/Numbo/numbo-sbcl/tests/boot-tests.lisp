;;; boot-tests.lisp -- item 9: compile init and start; first boot.
;;;
;;; Run: sbcl --non-interactive --load tests/boot-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;;  1. The whole system loads through src/load.lisp with no failures,
;;;     including init.lisp (its (defvar *print-array*) no longer hits the CL
;;;     package lock) and start.lisp.
;;;  2. (init-chiffre) runs: it sets the printer flag, the urgencies, and builds
;;;     the coderack with levels (600 300 150 7 4 1 0).
;;;  3. Smoke test: (config 31 3 5 24 3 14) through the harness (src/harness.lisp),
;;;     seed 31, capped at 500 main-loop iterations, headless.  No error, the
;;;     cap is what stops it, and the output has the expected 1987 shape.
;;;  4. The run is reproducible: the same seed gives the same output, and a
;;;     different seed gives a different one.
;;;  5. The cap stops exactly where asked (10 iterations).

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

;;; --- 1. full load ---------------------------------------------------------------
(check cl-user::*numbo-load-failures* nil)
(check (every #'fboundp '(init-chiffre config reactivate-cyto refresh-everything quick
                          run-config))
       t)

;;; --- 2. init-chiffre --------------------------------------------------------------
(setq cl:*print-array* t)
(check (with-output-to-string (*standard-output*) (init-chiffre))
       (format nil "Graphics is OFF.~%"))
(check cl:*print-array* nil)                       ; "don't print circular vectors"
(check %graphics% nil)
(check (list %upper-urgency% %first-urgency% %second-urgency% %third-urgency%
             %fourth-urgency% %fifth-urgency%)
       '(600 300 150 7 4 1))
(check *coderack* 'my-coderack)
(check (cr-empty? *coderack*) t)
(check *name-counter* 1)

;;; --- 3. smoke test: 500 iterations of (config 31 3 5 24 3 14) -------------------
(defun boot-run (seed cap)
  "Run the 1987 problem; return (result output).  A runtime error gives
((:error message) partial-output), so the checks below report it."
  (let* (result
         (output (with-output-to-string (*standard-output*)
                   (setq result
                         (handler-case (run-config '(31 3 5 24 3 14)
                                                   :seed seed :max-iterations cap)
                           (error (c) (list :error (princ-to-string c))))))))
    (list result output)))

(defparameter *t0* (get-internal-real-time))
(defparameter *run* (boot-run 31 500))
(format t "~&boot: 500 iterations in ~,1fs~%"
        (cl:/ (- (get-internal-real-time) *t0*) internal-time-units-per-second 1.0))
(defparameter *result* (first *run*))
(defparameter *output* (second *run*))

(check *result* '(:outcome :capped :iterations 500 :seed 31 :problem-solved 0))
(check *iteration* 500)
(defun output-lines (s)
  (with-input-from-string (in s)
    (loop for line = (read-line in nil) while line collect line)))
(defparameter *lines* (output-lines *output*))
(check (subseq *lines* 0 (min 5 (length *lines*)))
       '("Graphics is OFF."
         "Le jeu des chiffres"
         "Initial configuration :"
         "   Target : 31"
         " Bricks : 3 5 24 3 14"))
;; the target and every brick are read into the cytoplasm
(check (loop for n in '("CYTO-TARGET" "CYTO-BRICK1" "CYTO-BRICK2" "CYTO-BRICK3"
                        "CYTO-BRICK4" "CYTO-BRICK5")
             always (cl:member (format nil "Node ~a created" n) *lines* :test #'string=))
       t)
;; blocks and dtargets get built and killed, as in trace3.31
(defun count-matching (re-prefix suffix)
  (count-if (lambda (l) (and (search re-prefix l) (search suffix l))) *lines*))
(check (> (count-matching "Node CYTO-BLOCK" "created") 0) t)
(check (> (count-matching "Node CYTO-TARGET-" "created") 0) t)
(check (> (count-matching "Node " "killed") 0) t)
;; every output line is one of the kinds trace3.31 has
(check (remove-if (lambda (l)
                    (or (search "Node " l :end2 (min 5 (length l)))
                        (search "About to post codelet " l)
                        (cl:member l (subseq *lines* 0 5) :test #'string=)))
                  *lines*)
       nil)
;; the cytoplasm is consistent: every node it lists is a cyto-node
(check (every (lambda (n) (typep n 'cyto-node)) (send *cytoplasm* :nodes)) t)
(check (numberp (temperature)) t)

;;; --- 4. reproducibility ------------------------------------------------------------
(defparameter *again* (boot-run 31 200))
(defparameter *again2* (boot-run 31 200))
(check (string= (second *again*) (second *again2*)) t)
(check (> (length (second *again*)) 0) t)
(check (string= (second *again*) (second (boot-run 32 200))) nil)

;;; --- 5. the cap is exact -------------------------------------------------------------
(check (getf (first (boot-run 7 10)) :iterations) 10)
(check *iteration* 10)

(format t "~&boot: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (cl:if (zerop *failures*) 0 1))

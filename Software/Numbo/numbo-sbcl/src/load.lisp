;;; load.lisp -- load the Numbo system in dependency order.
;;;
;;; Usage:  sbcl --load src/load.lisp
;;;
;;; Each file is loaded inside its own handler, so an error in one file is
;;; reported (file name + condition) and loading continues with the next.
;;; Files not yet created are reported as missing and skipped.
;;; After loading, CL-USER::*NUMBO-LOAD-FAILURES* holds a list of
;;; (file . condition-or-:missing) for every file that did not load.

(in-package :cl-user)

(defparameter *numbo-src-dir*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defparameter *numbo-files*
  '("package"
    "franz-compat"
    "flavors-compat"
    "coderack"
    "graphics-stubs"
    "globals"
    "pnet-def"
    "pnet-functions"
    "pnet-graphics"
    "cyto-def"
    "codelets"
    "init"
    "start"
    "harness"
    "solution-checker")
  "Source files, in load order.")

(defvar *numbo-load-failures* nil)

(defun numbo-load-file (name)
  "Load src/NAME.lisp.  Return T on success, else record the failure and return NIL."
  (let ((path (merge-pathnames (make-pathname :name name :type "lisp")
                               *numbo-src-dir*)))
    (cond ((not (probe-file path))
           (format t "~&;; numbo load: ~a.lisp MISSING (skipped)~%" name)
           (push (cons name :missing) *numbo-load-failures*)
           nil)
          (t
           (handler-case
               ;; The original files have no IN-PACKAGE; load them into NUMBO.
               (progn (let ((*package* (or (find-package :numbo) *package*)))
                        (load path))
                      (format t "~&;; numbo load: ~a.lisp ok~%" name)
                      t)
             (error (c)
               (format t "~&;; numbo load: ~a.lisp ERROR: ~a~%" name c)
               (push (cons name c) *numbo-load-failures*)
               nil))))))

(defun numbo-load (&optional (files *numbo-files*))
  "Load FILES in order.  Return T if all loaded."
  (setf *numbo-load-failures* nil)
  (dolist (f files)
    (numbo-load-file f))
  (setf *numbo-load-failures* (nreverse *numbo-load-failures*))
  (null *numbo-load-failures*))

;; Set CL-USER::*NUMBO-NO-AUTOLOAD* to T before loading this file to get the
;; loader functions without loading the system (used by the tests).
(defvar *numbo-no-autoload* nil)
(unless *numbo-no-autoload*
  (numbo-load))

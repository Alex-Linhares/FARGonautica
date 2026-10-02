;;; load.lisp -- load the Numbo system in dependency order.
;;;
;;; Usage:  sbcl --load src/load.lisp
;;;
;;; Each file is loaded inside its own handler, so an error in one file is
;;; reported (file name + condition) and loading continues with the next.
;;; Files not yet created are reported as missing and skipped.
;;; After loading, CL-USER::*NUMBO-LOAD-FAILURES* holds a list of
;;; (file . condition-or-:missing) for every file that did not load.
;;;
;;; Oracle mode (loop0002, opt-in): set CL-USER::*NUMBO-ORACLE* to T before
;;; loading this file, or set the environment variable NUMBO_ORACLE to
;;; anything but "" or "0".  Then src/oracle.lisp is loaded right after
;;; package.lisp, every file is read with *READ-DEFAULT-FLOAT-FORMAT* bound to
;;; DOUBLE-FLOAT (only while loading), and NUMBO::ORACLE-INSTALL runs once all
;;; files are loaded.  See oracle.lisp and PORTING_NOTES.md, "Oracle hooks".
;;; In default mode none of this happens.

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

(defvar *numbo-oracle* nil
  "T (before loading) to load the system in oracle mode.")

(defun numbo-oracle-mode-p ()
  (let ((env (sb-ext:posix-getenv "NUMBO_ORACLE")))
    (and (or *numbo-oracle*
             (and env (string/= env "") (string/= env "0")))
         t)))

(defun numbo-file-list ()
  "*NUMBO-FILES*, with \"oracle\" after \"package\" in oracle mode."
  (if (numbo-oracle-mode-p)
      (loop for f in *numbo-files*
            collect f
            when (string= f "package") collect "oracle")
      *numbo-files*))

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
               (progn (let ((*package* (or (find-package :numbo) *package*))
                            ;; Oracle mode: Franz flonums were doubles.
                            (*read-default-float-format*
                              (if (numbo-oracle-mode-p)
                                  'double-float
                                  *read-default-float-format*)))
                        (load path))
                      (format t "~&;; numbo load: ~a.lisp ok~%" name)
                      t)
             (error (c)
               (format t "~&;; numbo load: ~a.lisp ERROR: ~a~%" name c)
               (push (cons name c) *numbo-load-failures*)
               nil))))))

(defun numbo-load (&optional (files (numbo-file-list)))
  "Load FILES in order.  Return T if all loaded.  In oracle mode, when FILES
includes oracle.lisp and everything loaded, install the oracle hooks."
  (setf *numbo-load-failures* nil)
  (dolist (f files)
    (numbo-load-file f))
  (setf *numbo-load-failures* (nreverse *numbo-load-failures*))
  (when (and (numbo-oracle-mode-p)
             (member "oracle" files :test #'string=)
             (null *numbo-load-failures*))
    (handler-case
        (progn (funcall (intern "ORACLE-INSTALL" :numbo))
               (format t "~&;; numbo load: oracle hooks installed~%"))
      (error (c)
        (format t "~&;; numbo load: oracle-install ERROR: ~a~%" c)
        (push (cons "oracle-install" c) *numbo-load-failures*))))
  (null *numbo-load-failures*))

;; Set CL-USER::*NUMBO-NO-AUTOLOAD* to T before loading this file to get the
;; loader functions without loading the system (used by the tests).
(defvar *numbo-no-autoload* nil)
(unless *numbo-no-autoload*
  (numbo-load))

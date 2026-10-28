;;; load-tests.lisp -- the whole system loads with no error, and after the
;;; load no function the compiler saw referenced is left undefined (the
;;; graphics functions the engine calls behind its flags must all have
;;; stubs).  Exit 0 = pass.

(in-package :cl-user)

(defparameter *dir* (make-pathname :name nil :type nil :defaults *load-truename*))
(defvar *undefined* '())
(defvar *errors* 0)

;; Load with warnings visible, collecting the undefined-function names SBCL
;; reports at the end of each file's compilation unit.
(defvar *copycat-verbose-load* t)
(handler-bind ((sb-int:simple-style-warning
                 (lambda (w)
                   (let ((text (princ-to-string w)))
                     (when (search "undefined function" text)
                       (let ((name (subseq text (+ (search "function: " text) 10))))
                         (pushnew (string-trim '(#\Space #\Newline) name)
                                  *undefined* :test #'string=))))
                   (muffle-warning w)))
               (warning #'muffle-warning)
               (error (lambda (c) (incf *errors*)
                        (format t "ERROR while loading: ~a~%" c))))
  (let ((*error-output* (make-broadcast-stream)))
    (load (merge-pathnames "../load.lisp" *dir*))))

(format t "load errors: ~a~%" *errors*)
(let ((still '()))
  (dolist (name *undefined*)
    (let* ((bare (if (search "::" name) (subseq name (+ 2 (search "::" name)))
                     (if (search ":" name) (subseq name (+ 1 (search ":" name))) name)))
           (sym (find-symbol bare :copycat)))
      (unless (and sym (fboundp sym))
        (push name still))))
  (format t "functions referenced during load: ~a; undefined after load: ~a~%"
          (length *undefined*) (length still))
  (dolist (s still) (format t "  undefined: ~a~%" s))
  (format t "flavors defined: ~a~%"
          (count-if (lambda (s) (get s 'copycat::flavor-defined))
                    (let (l) (do-symbols (s :copycat) (push s l)) l)))
  (sb-ext:exit :code (if (and (zerop *errors*) (null still)) 0 1)))

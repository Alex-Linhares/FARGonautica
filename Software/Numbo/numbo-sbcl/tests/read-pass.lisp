;;; read-pass.lisp -- READ every form of every ported source file (no evaluation).
;;;
;;; Usage: sbcl --non-interactive --load tests/read-pass.lisp
;;; Exits 0 if every file reads cleanly, 1 otherwise.  Each reader error is
;;; reported with the file, the line where the failing form starts, and the
;;; condition.

(in-package :cl-user)

(defvar *numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(unless (numbo-load '("package"))
  (format t "~&read-pass: package.lisp failed to load~%")
  (sb-ext:exit :code 1))

(defparameter *read-pass-files*
  '("pnet-def" "pnet-functions" "pnet-graphics" "cyto-def" "codelets" "init" "start"))

(defvar *read-pass-verbose* (sb-ext:posix-getenv "NUMBO_READ_VERBOSE")
  "If true (env NUMBO_READ_VERBOSE, or set before loading), print the line
span and head of every top-level form.")

(defun line-of (path pos)
  "1-based line number of character position POS in PATH."
  (with-open-file (s path)
    (let ((line 1))
      (dotimes (i pos line)
        (when (eql (read-char s nil #\Newline) #\Newline)
          (incf line))))))

(defun read-pass-file (name)
  "READ all forms of src/NAME.lisp.  Return (values ok-p form-count)."
  (let ((path (merge-pathnames (make-pathname :name name :type "lisp")
                               *numbo-src-dir*))
        (count 0)
        (*package* (find-package :numbo))
        (*read-eval* nil))
    (with-open-file (s path)
      (loop
        (let ((start (progn (peek-char t s nil nil) (file-position s))))
          (handler-case
              (let ((form (read s nil s)))
                (when (eq form s)
                  (format t "~&read-pass: ~a.lisp ok (~d forms)~%" name count)
                  (return (values t count)))
                (when *read-pass-verbose*
                  (format t "~&  ~a:~d-~d ~s~%" name (line-of path start)
                          (line-of path (file-position s))
                          (if (consp form)
                              (list (car form) (if (consp (cdr form)) (cadr form)))
                              form)))
                (incf count))
            (error (c)
              (format t "~&read-pass: ~a.lisp ERROR in form starting at line ~d: ~a~%"
                      name (line-of path start) c)
              (return (values nil count)))))))))

(defun non-ascii-lines (name)
  "Line numbers of src/NAME.lisp containing non-ASCII characters (OCR debris
that would otherwise READ silently as symbol constituents)."
  (with-open-file (s (merge-pathnames (make-pathname :name name :type "lisp")
                                      *numbo-src-dir*))
    (loop for line = (read-line s nil)
          for n from 1
          while line
          when (find-if (lambda (c) (> (char-code c) 127)) line)
            collect n)))

(let ((ok t))
  (dolist (f *read-pass-files*)
    (unless (read-pass-file f)
      (setf ok nil))
    (let ((bad (non-ascii-lines f)))
      (when bad
        (format t "~&read-pass: ~a.lisp non-ASCII characters on lines ~{~d~^, ~}~%" f bad)
        (setf ok nil))))
  (sb-ext:exit :code (if ok 0 1)))

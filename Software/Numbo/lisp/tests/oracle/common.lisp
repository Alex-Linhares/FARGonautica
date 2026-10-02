;;; common.lisp -- shared prologue for the fixture capture scripts in tests/oracle/.
;;;
;;; Each capture script starts with
;;;   (load (merge-pathnames "common.lisp" *load-truename*))
;;; which loads the numbo sources in oracle mode (src/oracle.lisp), silently,
;;; and defines FIXTURE-PATH and WRITE-FIXTURE in CL-USER.
;;;
;;; Fixtures go to ../python/fixtures/ unless the environment variable
;;; NUMBO_FIXTURE_DIR names another directory (../python/tests uses that to
;;; regenerate into a temporary directory and compare with the committed files).

(defvar cl-user::*numbo-no-autoload* t)
(defvar cl-user::*numbo-oracle* t)

(defparameter cl-user::*oracle-repo*
  (merge-pathnames "../../" (make-pathname :name nil :type nil :defaults *load-truename*)))

(let ((*error-output* (make-broadcast-stream))
      (*standard-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "src/load.lisp" cl-user::*oracle-repo*))
    (funcall (intern "NUMBO-LOAD" "CL-USER"))))

(defun cl-user::fixture-dir ()
  (let ((env (sb-ext:posix-getenv "NUMBO_FIXTURE_DIR")))
    (if (and env (plusp (length env)))
        (pathname (if (char= (char env (1- (length env))) #\/) env
                      (concatenate 'string env "/")))
        (merge-pathnames "../python/fixtures/" cl-user::*oracle-repo*))))

(defun cl-user::fixture-path (name)
  (merge-pathnames name (cl-user::fixture-dir)))

(defun cl-user::write-fixture (name string)
  "Write STRING to the fixture file NAME and report the path on stdout."
  (let ((path (cl-user::fixture-path name)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string string out))
    (format t "wrote ~a~%" (namestring (truename path)))))

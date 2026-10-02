;;; compile-helpers.lisp -- shared by the compile tests (items 7 and 8).
;;;
;;; Load after src/load.lisp has loaded at least "package":
;;;   CHECK                  the test macro; counts *CHECKS* / *FAILURES*
;;;   SOURCE-PATH, READ-FORMS  src/NAME.lisp and its top-level forms (READ only)
;;;   *SOURCE-FILES*         the seven ported source files, in load order
;;;   *FASL-DIR*             a per-process temp directory for the fasls
;;;   COMPILE-AND-LOAD       COMPILE-FILE + LOAD, collecting the conditions
;;;   UNDEFINED-NAMES        names out of SBCL's "undefined KIND: NAME" messages
;;;   LEXICALLY-BOUND-SYMBOLS  walker for the globals.lisp census
;;;   CLEANUP-FASL-DIR

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

(defun source-path (name)
  (merge-pathnames (make-pathname :name name :type "lisp") cl-user::*numbo-src-dir*))

(defun read-forms (name)
  ;; Every top-level form of src/NAME.lisp, READ in NUMBO, no evaluation.
  (let ((*package* (find-package :numbo)) (*read-eval* nil))
    (with-open-file (s (source-path name))
      (loop for form = (read s nil s) until (eq form s) collect form))))

(defparameter *source-files*
  '("pnet-def" "pnet-functions" "pnet-graphics" "cyto-def" "codelets" "init" "start"))

;;; --- compile-file ------------------------------------------------------------
(defparameter *fasl-dir*
  (merge-pathnames (format nil "numbo-compile-fasl-~d/" (sb-unix:unix-getpid))
                   (or (sb-ext:posix-getenv "TMPDIR") "/tmp/")))

(defun fasl-path (name)
  (merge-pathnames (make-pathname :name name :type "fasl") *fasl-dir*))

(defun compile-and-load (name &key (load t))
  "COMPILE-FILE src/NAME.lisp and (unless LOAD is nil) load the fasl.
Return (fasl warnings-p failure-p full-warnings style-warnings errors), each
warning list holding the printed conditions."
  (ensure-directories-exist *fasl-dir*)
  (let (full style errors)
    (multiple-value-bind (fasl warnings-p failure-p)
        (handler-bind ((style-warning (lambda (c) (push (princ-to-string c) style)))
                       ((and warning (not style-warning))
                         (lambda (c) (push (princ-to-string c) full)))
                       (error (lambda (c) (push (princ-to-string c) errors))))
          (let ((*package* (find-package :numbo))
                (*compile-verbose* nil) (*compile-print* nil))
            (compile-file (source-path name) :output-file (fasl-path name))))
      (when (and fasl load)
        (let ((*package* (find-package :numbo)))
          (load fasl)))
      (list fasl warnings-p failure-p (reverse full) (reverse style) (reverse errors)))))

(defun undefined-names (messages kind)
  ;; Names from SBCL's "undefined KIND: PKG::NAME" messages.
  (let ((prefix (format nil "undefined ~a: " kind)))
    (sort (remove-duplicates
           (loop for m in messages
                 when (and (> (length m) (length prefix))
                           (string= prefix m :end2 (length prefix)))
                   collect (subseq m (length prefix)))
           :test #'string=)
          #'string<)))

(defun cleanup-fasl-dir ()
  (dolist (f (directory (merge-pathnames "*.fasl" *fasl-dir*))) (delete-file f))
  (ignore-errors (sb-ext:delete-directory *fasl-dir*)))

;;; --- globals.lisp census -----------------------------------------------------
(defun binding-names (lambda-list)
  ;; Variable names in an ordinary / Franz lambda list.  A Franz lexpr has an
  ;; atom in place of the list.
  (cond ((null lambda-list) nil)
        ((symbolp lambda-list) (list lambda-list))
        (t (loop for x in lambda-list
                 unless (cl:member x lambda-list-keywords)
                   collect (if (consp x) (car x) x)))))

(defun lexically-bound-symbols (tree)
  ;; Over-approximation of every symbol bound lexically in TREE: LET/LET*/
  ;; PROG/DO/DO* variables, DEFUN/DEFMETHOD/LAMBDA parameters, LOOP FOR/AS.
  ;; (The source uses no other binding forms: no DOLIST, DOTIMES, FLET, ...)
  (let (acc)
    (labels ((add (syms) (dolist (s syms) (when (and s (symbolp s)) (pushnew s acc))))
             (walk (x)
               (when (consp x)
                 (let ((head (car x)))
                   (when (symbolp head)
                     (cond ((cl:member head '(let let* prog do do*))
                            (when (listp (cadr x))
                              (add (mapcar (lambda (b) (if (consp b) (car b) b)) (cadr x)))))
                           ((cl:member head '(defun defmethod))
                            (add (binding-names (caddr x))))
                           ((eq head 'lambda)
                            (add (binding-names (cadr x))))
                           ((eq head 'loop)
                            (loop for (a b) on (cdr x)
                                  when (and (symbolp a) (cl:member (symbol-name a) '("FOR" "AS")
                                                                   :test #'string=))
                                    do (add (if (consp b) b (list b))))))))
                 (loop for y on x while (consp y) do (walk (car y))))))
      (walk tree))
    acc))

(defun globally-special-p (s) (eq (sb-int:info :variable :kind s) :special))

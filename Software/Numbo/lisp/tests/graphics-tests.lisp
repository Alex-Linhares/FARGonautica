;;; graphics-tests.lisp -- tests for src/graphics-stubs.lisp (item 6).
;;;
;;; Run: sbcl --non-interactive --load tests/graphics-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;;  1. Each stub exists, and the drawing stubs return nil.
;;;  2. pnet-graphics.lisp loads.
;;;  3. Census: after a full load, every function that is still undefined is
;;;     defined in some source file (so it is only missing because that file
;;;     didn't load yet).  No graphics primitive is left unstubbed.  Every
;;;     stub is called somewhere in the source, and none of them redefines a
;;;     source function.
;;;  4. With %graphics% bound to t, the whole graphics path in pnet-graphics
;;;     runs on the real *pnet* through the stubs.
;;;  5. The reactivate-cyto call in start.lisp matches the definition in
;;;     init.lisp.  %graphics% defaults to nil.

(defvar cl-user::*numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(unless (cl-user::numbo-load '("package" "franz-compat" "flavors-compat" "coderack"
                                "graphics-stubs"))
  (format t "~&graphics: failed to load~%")
  (sb-ext:exit :code 1))

(in-package :numbo)

(declaim (special *pnet*))               ; set by pnet-def.lisp at load time

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

(defparameter *source-files*
  '("pnet-def" "pnet-functions" "pnet-graphics" "cyto-def" "codelets" "init" "start"))

(defun read-forms (name)
  ;; Every top-level form of src/NAME.lisp, READ in NUMBO, no evaluation.
  (let ((*package* (find-package :numbo)) (*read-eval* nil))
    (with-open-file (s (source-path name))
      (loop for form = (read s nil s) until (eq form s) collect form))))

(defun tree-symbols (tree)
  (let (acc)
    (labels ((walk (x) (cond ((symbolp x) (pushnew x acc))
                             ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk tree))
    acc))

(defparameter *source-defuns*
  (loop for f in *source-files*
        append (loop for form in (read-forms f)
                     when (and (consp form) (member (car form) '(defun defmacro)))
                       collect (second form))))

(defparameter *source-symbols*
  (loop for f in *source-files* append (loop for form in (read-forms f)
                                             append (tree-symbols form))))

;;; --- 1. the stubs -----------------------------------------------------------
(check (length *graphics-stubs*) 10)
(check (every #'fboundp *graphics-stubs*) t)
(check (open-window) nil)
(check (clear-window) nil)
(check (draw-rect 0 0 10 10) nil)
(check (draw-unfilled-rect 0 0 10 10) nil)
(check (erase-rect 0 0 10 10) nil)
(check (draw-text 1 2 "n31") nil)
(check (draw-number 1 2 99) nil)
(check (dump-window "gr12") nil)
(check (and (integerp (window-height)) (integerp (window-width))
            (> (window-height) (window-width)))
       t)
(check (gethash 'draw-rect *graphics-stub-calls*) 1)

;;; --- 2. pnet-graphics.lisp loads --------------------------------------------
;; (the Franz free globals give many undefined-variable warnings; hide them)
(check (let ((*error-output* (make-broadcast-stream)))
         (cl-user::numbo-load '("pnet-def" "pnet-functions" "pnet-graphics")))
       t)
(check (every #'fboundp '(init-pnet-graphics display-pnet setup-regions make-boxsizes
                          outline-regions update-pnet-display shrink-box drawbox
                          erasebox round-off))
       t)

;;; --- 3. census of undefined functions after a full load ---------------------
(defparameter *undefined-after-load*
  ;; (the compilation-unit summary is printed when the unit ends, so the
  ;; streams are bound around it)
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (with-compilation-unit ()
      (cl-user::numbo-load)
      (remove-if #'fboundp
                 (mapcar #'sb-c::undefined-warning-name
                         (remove-if-not (lambda (w) (eq (sb-c::undefined-warning-kind w)
                                                        :function))
                                        sb-c::*undefined-warnings*))))))
(format t "~&graphics: undefined after full load: ~s~%" *undefined-after-load*)
(check (remove-if (lambda (s) (member s *source-defuns*)) *undefined-after-load*) nil)
(check (remove-if (lambda (s) (member s *source-symbols*)) *graphics-stubs*) nil)
(check (intersection *graphics-stubs* *source-defuns*) nil)

;;; --- 4. the graphics path runs through the stubs ----------------------------
(clrhash *graphics-stub-calls*)
;; In a real run initialize-pnet has set every activation before the graphics
;; start.  It needs init.lisp's globals, so set the activations directly here.
(dolist (p *pnet*) (send p :set-activation 0))
(check (let ((%graphics% t))
         (declare (special %graphics%))
         (init-pnet-graphics *pnet*)
         (display-pnet *pnet*)
         (send (car *pnet*) :set-activation 100)
         (update-pnet-display *pnet*)
         (send (car *pnet*) :set-activation 5)
         (update-pnet-display *pnet*)          ; shrinks a box
         (dump-window (get-pname (concat 'gr 1)))
         t)
       t)
(check (loop for s in *graphics-stubs* always (plusp (gethash s *graphics-stub-calls* 0)))
       t)
(check (gethash 'draw-unfilled-rect *graphics-stub-calls*) (length *pnet*))
;; setup-regions gave every pnode a region inside the stub window
(check (every (lambda (p) (and (integerp (send p :x-region))
                               (< -1 (send p :x-region) (window-width))
                               (< -1 (send p :y-region) (window-height))))
              *pnet*)
       t)

;;; --- 5. reactivate-cyto, %graphics% -----------------------------------------
(check (and (member 'reactivate-cyto (tree-symbols (read-forms "init"))) t) t)
(check (and (member 'reactivate-cyto *source-defuns*) t) t)
(check (and (member 'reactivate-cyto (tree-symbols (read-forms "start"))) t) t)
(check (find-symbol "REACTIVATE-CTYO" :numbo) nil)
(check (find '(defvar %graphics% nil) (read-forms "init") :test #'equal)
       '(defvar %graphics% nil))

(format t "~&graphics: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (cl:if (zerop *failures*) 0 1))

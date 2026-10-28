;;; compat-tests.lisp -- the compatibility layer on its own, without
;;; Mitchell's files.  Loads compat/ and checks each shim against the
;;; behaviour Lucid gave the 1989 source (see PORTING_NOTES.md).
;;; Exit 0 = pass.

(in-package :cl-user)

(defparameter *dir* (make-pathname :name nil :type nil :defaults *load-truename*))
(dolist (f '("package" "lucid-compat" "flavors-compat" "graphics-stubs"))
  (load (merge-pathnames (format nil "../compat/~a.lisp" f) *dir*)))

(defvar *failures* 0)
(defmacro check (name form)
  `(handler-case
       (if ,form
           (format t "  ok   ~a~%" ,name)
           (progn (incf *failures*) (format t "  FAIL ~a~%" ,name)))
     (error (c) (incf *failures*) (format t "  FAIL ~a: ~a~%" ,name c))))

(in-package :copycat)

;;; (in-package 'user) is accepted and selects COPYCAT
(cl-user::check "(in-package 'user) selects COPYCAT"
                (equal (macroexpand-1 '(in-package 'user))
                       '(cl:in-package :copycat)))

;;; LOOP: empty do
(cl-user::check "loop: `do collect' as in initialization.l"
                (equal (loop for i from 0 to 2 do collect i) '(0 1 2)))
(cl-user::check "loop: `do when ... do' as in answer.l"
                (equal (let ((out '()))
                         (loop for x in '(1 2 3) do
                               when (oddp x)
                               do (push x out))
                         out)
                       '(3 1)))
(cl-user::check "loop: `do ()' as in util.l's pauses"
                (progn (loop for i from 1 to 10 do ()) t))
(cl-user::check "loop: ordinary loops are untouched"
                (and (equal (loop for i in '(1 2) collect (* 2 i)) '(2 4))
                     (= (loop for i from 1 to 3 sum i) 6)
                     (equal (macroexpand-1 '(loop for x in l do (f x) (g x)))
                            '(cl:loop for x in l do (f x) (g x)))))
(cl-user::check "loop: simple (loop form...) form"
                (= (let ((n 0)) (loop (incf n) (when (= n 3) (return n)))) 3))

;;; FUNCALL and SORT with quoted lambda expressions
(cl-user::check "funcall of a quoted lambda (slipnet-def.l description testers)"
                (= (funcall '(lambda (x) (* x 2)) 21) 42))
(cl-user::check "funcall of a quoted lambda is compiled once"
                (let ((f '(lambda (x) (1+ x))))
                  (funcall f 1)
                  (eq (gethash f *compiled-lambda-expressions*)
                      (progn (funcall f 2)
                             (gethash f *compiled-lambda-expressions*)))))
(cl-user::check "funcall of a symbol and of a function"
                (and (= (funcall '+ 1 2) 3) (= (funcall #'+ 1 2) 3)))
(cl-user::check "sort :key with a quoted lambda (copystat.l)"
                (equal (sort (list 3 1 2) #'> :key '(lambda (x) x)) '(3 2 1)))

;;; Lucid built-ins
(cl-user::check "memq" (and (memq 'b '(a b c)) (not (memq 'z '(a b c)))))
(cl-user::check "string-append of strings and symbols"
                (string= (string-append "ab" 'cd "e") "abCDe"))
(cl-user::check "append-symbols interns in COPYCAT, upper case"
                (and (eq (append-symbols 'plato- "A") 'plato-a)
                     (eq (append-symbols 'a '- 'b '-link) 'a-b-link)
                     (eq (append-symbols "1" '-length-link) '1-length-link)))
(cl-user::check "production-mode is a no-op" (null (production-mode)))
(cl-user::check "run-program runs a command (copystat.l's cp)"
                (let ((f (format nil "/tmp/ccat-compat-test-~a" (get-universal-time))))
                  (with-open-file (s f :direction :output) (write-line "x" s))
                  (run-program "cp" :arguments (list f (string-append f ".2")))
                  (prog1 (probe-file (string-append f ".2"))
                    (delete-file f) (delete-file (string-append f ".2")))))
(cl-user::check "with-open-file supersedes an existing output file"
                (let ((f (format nil "/tmp/ccat-compat-test-~a" (get-universal-time))))
                  (with-open-file (s f :direction :output :if-does-not-exist :create)
                    (write-line "first" s))
                  (with-open-file (s f :direction :output :if-does-not-exist :create)
                    (write-line "second" s))
                  (prog1 (with-open-file (s f) (string= (read-line s) "second"))
                    (delete-file f))))
(cl-user::check "with-open-file keeps an explicit :if-exists"
                (equal (macroexpand-1 '(with-open-file (s "f" :direction :output
                                                          :if-exists :append)
                                         (x)))
                       '(cl:with-open-file (s "f" :direction :output :if-exists :append)
                          (x))))

;;; Flavors on CLOS
(defflavor thing (a (b 2) c) () :gettable-instance-variables
                                 :settable-instance-variables
                                 :initable-instance-variables)
(defflavor sub-thing (d) (thing) :gettable-instance-variables
                                 :settable-instance-variables
                                 :initable-instance-variables)
(defflavor bare-thing (e) :gettable-instance-variables :initable-instance-variables)   ; no component list
(defmethod (thing :sum) () (+ (or a 0) b (or c 0)))
(defmethod (thing :set-all) (a new-c) (setq c new-c) (setq b a) b)
(defmethod (thing :describe-self) (&aux (s "")) (setq s (string-append s "thing")) s)
(defmethod (sub-thing :sum) () (+ (send self :d) 100))
(defmethod (bare-thing :e-twice) () (* 2 e))

(let ((x (make-instance 'thing :a 1 :c 3)))
  (cl-user::check "make-instance with init keywords; ivar default" 
                  (and (= (send x :a) 1) (= (send x :b) 2) (= (send x :c) 3)))
  (cl-user::check "method reads ivars free" (= (send x :sum) 6))
  (cl-user::check "parameter shadows ivar; setq of ivar writes the slot"
                  (and (= (send x :set-all 7 9) 7) (= (send x :b) 7)
                       (= (send x :c) 9) (= (send x :a) 1)))
  (cl-user::check "&aux variable shadows nothing and works"
                  (string= (send x :describe-self) "thing"))
  (cl-user::check ":set-ivar messages" (progn (send x :set-a 10) (= (send x :a) 10)))
  (cl-user::check "unset ivar is NIL" (null (send (make-instance 'thing) :a)))
  (cl-user::check "unknown message is an error"
                  (handler-case (progn (send x :no-such-message) nil)
                    (error () t))))
(let ((y (make-instance 'sub-thing :a 1 :d 5)))
  (cl-user::check "component flavor: inherited ivars and methods"
                  (and (= (send y :a) 1) (= (send y :b) 2)))
  (cl-user::check "component flavor: own method wins" (= (send y :sum) 105))
  (cl-user::check "typep on flavor and component" 
                  (and (typep y 'sub-thing) (typep y 'thing)))
  (cl-user::check "flavors::flavor-type-of" (eq (flavors:flavor-type-of y) 'sub-thing)))
(cl-user::check "defflavor without a component list (file-system.l)"
                (= (send (make-instance 'bare-thing :e 4) :e-twice) 8))
(cl-user::check "defmethod on an undefined flavor is an error at expansion"
                (handler-case (progn (macroexpand '(defmethod (no-flavor :m) () 1)) nil)
                  (error () t)))

;;; graphics stubs
(cl-user::check "graphics stubs are no-ops" 
                (and (null (display-ccat)) (null (open-window 1 2 3 4))
                     (null (draw-bond-grope nil nil))))

(cl:in-package :cl-user)
(format t "~a failure~:p~%" *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))

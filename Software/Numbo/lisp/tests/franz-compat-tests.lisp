;;; franz-compat-tests.lisp -- unit tests for src/franz-compat.lisp.
;;;
;;; Run: sbcl --non-interactive --load tests/franz-compat-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.

(defvar cl-user::*numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(unless (cl-user::numbo-load '("package" "franz-compat"))
  (format t "~&franz-compat: failed to load~%")
  (sb-ext:exit :code 1))

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

(defmacro check-error (form)
  `(progn
     (incf *checks*)
     (unless (handler-case (progn (macroexpand-1 ',form) nil)
               (error () t))
       (incf *failures*)
       (format t "~&FAIL: expected a macroexpansion error from ~s~%" ',form))))

;;; --- DEFUN -----------------------------------------------------------------
(defun fc-test-plain (a &optional (b 2)) (list a b))
(check (fc-test-plain 1) '(1 2))
(defun fc-test-expr expr (a) (* a 10))
(check (fc-test-expr 4) 40)
;; lexpr, shaped like codelets.lisp check-temperature: (defun f function () ...)
(defun fc-test-lexpr function () (list function :body))
(check (fc-test-lexpr) '(0 :body))
(check (fc-test-lexpr 'a 'b 'c) '(3 :body))
(defun fc-test-lexpr-args n (list n *lexpr-args*))
(check (fc-test-lexpr-args 'x 'y) '(2 (x y)))
(check-error (defun fc-bad fexpr (l) l))
(check-error (defun fc-bad macro (l) l))

;;; --- keyword IF ------------------------------------------------------------
(check (if t then 1) 1)
(check (if nil then 1) nil)
(check (if t then 1 2 3) 3)
(check (if nil then 1 else 2) 2)
(check (if nil then 1 else 2 3) 3)
(check (if t then nil else 2) nil)
(check (if t then else 2) nil)
(check (if nil then 1 else) nil)
(check (if nil then 1 elseif t then 2 else 3) 2)
(check (if nil then 1 elseif nil then 2 else 3) 3)
(check (if nil then 1 elseif nil then 2) nil)
(check (if 7 thenret else 3) 7)
(check (if nil thenret else 3) 3)
(check (let ((n 0)) (if t then (incf n) (incf n)) n) 2)
(check (let ((n 0)) (if nil then (incf n) else (incf n 10) (incf n 10)) n) 20)
;; multi-line style used in start.lisp / pnet-graphics.lisp
(check (let ((x 1)) (if (= x 1)
                       then (setq x 10)
                       else (setq x 20))
         x)
       10)
;; nested, as in pnet-graphics.lisp update-pnet-display
(check (if t then (if nil then 1 else 2) else 3) 2)
;; plain CL-style forms still work
(check (if t 1 2) 1)
(check (if nil 1 2) 2)
(check (if nil 1) nil)
(check-error (if))
(check-error (if a b c d))
(check-error (if a b then c))
(check-error (if a then b else c else d))

;;; --- arithmetic -------------------------------------------------------------
(check (plus) 0)
(check (plus 1 2 3) 6)
(check (plus 1 0.5) 1.5)
(check (add 1 2) 3)
(check (apply 'add '(1 2 3)) 6)
(check (sum 4 5) 9)
(check (times 2 3 4) 24)
(check (times 2 0.5) 1.0)
(check (product 2 3) 6)
(check (difference 10 3 2) 5)
(check (difference 10) 10)
(check (diff 60 10 5) 45)
(check (minus 5) -5)
(check (minus -2.5) 2.5)
;; MIN is shadowed (codelets `eliminate' SETQs it); the function is CL:MIN.
(check (eq 'min 'cl:min) nil)
(check (min 3 1 2) 1)
(check (min 2 1.5) 1.5)
(check (apply 'min '(4 9)) 4)
;; MAX is shadowed: (max) with no arguments is 0 (codelets `temperature'
;; applies it to an empty misfortune list); otherwise CL:MAX.
(check (eq 'max 'cl:max) nil)
(check (max 3 7 2) 7)
(check (max 2 2.5) 2.5)
(check (apply 'max '(4 9)) 9)
(check (apply 'max nil) 0)
(check (max) 0)

;;; --- DEFVAR ------------------------------------------------------------------
;; Ordinary DEFVAR is CL:DEFVAR.
(check (eq 'defvar 'cl:defvar) nil)
(defvar fc-test-var 5 "doc")
(check fc-test-var 5)
(defvar fc-test-var 6)
(check fc-test-var 5)
(check (documentation 'fc-test-var 'variable) "doc")
(defvar fc-test-unbound)
(check (list (boundp 'fc-test-unbound) (sb-int:info :variable :kind 'fc-test-unbound))
       '(nil :special))
;; A CL symbol (init.lisp's (defvar *print-array*)): no package-lock error and
;; no change of value; init-chiffre's SETQ then sets the real printer flag.
(check (eq '*print-array* 'cl:*print-array*) t)
(check (let ((cl:*print-array* t)) (eval '(defvar *print-array*)) cl:*print-array*) t)
(check (let ((cl:*print-array* t)) (eval '(defvar *print-array* nil)) cl:*print-array*) t)
(check (add1 4) 5)
(check (sub1 4) 3)
(check (quotient 7 2) 3)
(check (quotient -7 2) -3)
(check (quotient 100 5 2) 10)
(check (quotient 7.0 2) 3.5)
(check (quotient (float 1) (float 4)) 0.25)
(check (quotient 9) 9)
(check (/ 31 10) 3)
(check (/ 98 2) 49)
(check (/ 7 2.0) 3.5)
(check (fix 3.7) 3)
(check (fix 3) 3)
(check (fix (+ 2.5 0.5)) 3)
(check (*quo 456 10) 45)
(check (*quo 456 100) 4)
(check (*quo -7 2) -3)
(check (mod 17 5) 2)
(check (mod -17 5) -2)
(check (mod 20 10) 0)
(check (remainder 17 5) 2)
;; *mod: balanced representation, range [-4, 5] for 10, [-2, 2] for 5
(check (*mod 31 10) 1)
(check (*mod 35 10) 5)
(check (*mod 36 10) -4)
(check (*mod 39 10) -1)
(check (*mod 13 5) -2)
(check (*mod 12 5) 2)
(check (*mod 125 50) 25)
(check (*mod 130 50) -20)
(check (loop for i from 0 below 100 always (<= -4 (*mod i 10) 5)) t)
(check (loop for i from -50 below 50 always (zerop (cl:mod (- i (*mod i 7)) 7))) t)
;; the 1987 `round' (codelets.lisp) relies on *mod and integer `/':
;; nearest multiple of 10 to 31 is 30, to 36 is 40
(check (let ((num 31) (div 10))
         (cond ((< (*mod num div) 0) (times div (+ 1 (/ num div))))
               (t (times div (/ num div)))))
       30)
(check (let ((num 36) (div 10))
         (cond ((< (*mod num div) 0) (times div (+ 1 (/ num div))))
               (t (times div (/ num div)))))
       40)
(check (nequal "2b" "2b") nil)
(check (nequal "2b" "1b") t)
(check (nequal '(1 2) '(1 2)) nil)
(check (nequal 1 1.0) t)

;;; --- lists ------------------------------------------------------------------
(check (memq 'b '(a b c)) '(b c))
(check (memq "b" (list "a" "b")) nil)
(check (member "b" (list "a" "b")) '("b"))
(check (member '(1) '((0) (1) (2))) '((1) (2)))
(check (member 3 '(1 2 3)) '(3))
(check (sortcar (list (list 3 'c) (list 1 'a) (list 2 'b)) #'<)
       '((1 a) (2 b) (3 c)))
(check (sortcar (list (list 3 'c) (list 1 'a)) '>) '((3 c) (1 a)))
;; nil predicate = alphabetical, as in pnet-functions.lisp (sortcar ... ())
(check (sortcar (list (list "2b" 'x) (list "1b" 'y) (list "3" 'z)) nil)
       '(("1b" y) ("2b" x) ("3" z)))
(check (sortcar (list (list 'cyto 1) (list 'block 2)) nil)
       '((block 2) (cyto 1)))

;;; --- symbols and strings -----------------------------------------------------
(check (concat 'node- 31) 'node-31)
(check (concat 'node- 31) 'node-31 :test #'eq)
(check (symbol-package (concat 'a 'b)) (cl:find-package :numbo))
(check (concat 'cyto-block 12 '-v 3) 'cyto-block12-v3 :test #'eq)
(check (concat 'times 3 '- 5 '-v 1) 'times3-5-v1 :test #'eq)
(check (concat "brick" 2) 'brick2 :test #'eq)
(check (concat 'node- 2.5) '|NODE-2.5| :test #'eq)
(check (let ((s (uconcat "brick" 1)))
         (list (symbol-name s) (symbol-package s)))
       '("BRICK1" nil))
(check (eq (uconcat 'a) (uconcat 'a)) nil)
(check (get-pname 'gr12) "gr12")
(check (get-pname (concat 'gr 7)) "gr7")
(check (get-pname '|MixedCase|) "MixedCase")
(check (get-pname "str") "str")
(check (string-length "WINDOW") 6)
(check (string-length "") 0)
(check (string-length 'abc) 3)
(check (alphalessp 'abc 'abd) t)
(check (alphalessp "b" "a") nil)
(check (alphalessp 'zeta "zz") t)
(check (invert-case "abc") "ABC")
(check (invert-case "ABC") "abc")
(check (invert-case "aBc") "aBc")
(check (invert-case "2b") "2B")
;; codelets.lisp read-brick:
;;   (intern (uconcat "brick" i) (find-package "keyword"))  => :brick1
(check (find-package "keyword") (cl:find-package :keyword))
(check (find-package :numbo) (cl:find-package :numbo))
(check (find-package "no-such-package") nil)
(check (intern (uconcat "brick" 1) (find-package "keyword")) :brick1)
(check (intern "FOO" :numbo) 'foo :test #'eq)
(check (intern 'bar) 'bar :test #'eq)

;;; --- vectors ------------------------------------------------------------------
(check (length (new-vector 4)) 4)
(check (vref (new-vector 3) 1) nil)
(check (vref (new-vector 3 0) 2) 0)
(check (let ((v (new-vector 3))) (list (vset v 1 'x) (vref v 1))) '(x x))

;;; --- I/O and environment --------------------------------------------------------
(check (with-output-to-string (*standard-output*) (print '(a "b"))) "(A \"b\")")
(check (with-output-to-string (*standard-output*) (print 3) (terpri))
       (format nil "3~%"))
(check (let ((*standard-output* (make-broadcast-stream))) (print 'x)) 'x)
(check (stringp (getenv "PATH")) t)
(check (getenv "NUMBO_SURELY_UNSET_VARIABLE_XYZ") "")
(check (string-length (getenv "NUMBO_SURELY_UNSET_VARIABLE_XYZ")) 0)

;;; --- declarations ---------------------------------------------------------------
;; (declaim (macros t)) -- the port of pnet-functions.lisp's top-level
;; (declare (macros t)) -- must be accepted without a warning.
(check (handler-case (progn (proclaim '(macros t)) :ok)
         (warning () :warned))
       :ok)

;;; --- coverage: every Franz built-in the source uses is defined -----------------
;;; This is the census listed in src/PORTING_NOTES.md.
(defparameter *franz-builtins-used*
  '(if add1 add concat diff difference fix get-pname getenv memq member minus
    mod nequal new-vector plus print quotient sortcar string-length times
    uconcat vref vset / *mod *quo intern find-package))

(dolist (f *franz-builtins-used*)
  (incf *checks*)
  (unless (and (fboundp f) (eq (symbol-package f) (cl:find-package :numbo)))
    (incf *failures*)
    (format t "~&FAIL: Franz built-in ~s is not defined in NUMBO~%" f)))

(format t "~&franz-compat: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (cl:if (zerop *failures*) 0 1))

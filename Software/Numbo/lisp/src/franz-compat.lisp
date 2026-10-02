;;; franz-compat.lisp -- Franz Lisp built-ins used by the 1987 Numbo source.
;;;
;;; Semantics follow the Franz Lisp Manual (Opus 38, 1983).  Only the
;;; functions the source actually calls are defined here; the census is in
;;; PORTING_NOTES.md ("Franz built-ins used by the source").
;;;
;;; Print names.  Franz is case-sensitive and the source is written in lower
;;; case, so the Franz symbol `node-31' is read by SBCL as NODE-31.  Franz
;;; functions that turn names into strings or strings into names (concat,
;;; uconcat, get-pname, alphalessp, ...) translate between the two worlds with
;;; INVERT-CASE (the same rule as readtable-case :invert): an all-lower-case
;;; name is upcased, an all-upper-case name is downcased, a mixed-case name is
;;; kept.  So (concat 'node- 31) => NODE-31 and (get-pname 'gr12) => "gr12",
;;; as in Franz.

(in-package :numbo)

;;; ---------------------------------------------------------------------------
;;; DEFUN
;;;
;;;   (defun name (args...) body...)        ; ordinary: CL DEFUN
;;;   (defun name expr (args...) body...)   ; explicit type EXPR: same
;;;   (defun name n body...)                ; atom arglist: a LEXPR
;;;
;;; A lexpr takes any number of arguments and binds the atom to their count
;;; (Franz Manual ch. 8).  codelets.lisp has (defun check-temperature function
;;; () ...): FUNCTION is the lexpr's count variable and () is the first body
;;; form.  Lexpr arguments are kept in *LEXPR-ARGS* for completeness; the
;;; source never reads them (no ARG / LISTIFY calls).  FEXPR and MACRO types
;;; are not used by the source and are rejected.
;;; Defined first: everything below in this package uses it.

(cl:defvar *lexpr-args* nil)

;;; ---------------------------------------------------------------------------
;;; DEFVAR
;;;
;;; init.lisp:3 is (defvar *print-array*), and init-chiffre later does
;;; (setq *print-array* nil) ; don't print circular vectors.  The reader gives
;;; CL:*PRINT-ARRAY*, the printer flag, which is what Defays meant.  Franz had
;;; no package locks, but SBCL signals one for DEFVAR (a special proclamation)
;;; of a CL symbol.  A CL variable is already special, so for a CL symbol
;;; DEFVAR only does its other job: assign the value if the variable is
;;; unbound.  Every other DEFVAR is CL:DEFVAR.

(defmacro defvar (name &rest value-and-doc)
  (cl:if (eq (symbol-package name) (cl:find-package :common-lisp))
         `(progn ,@(when value-and-doc
                     `((unless (boundp ',name) (setq ,name ,(car value-and-doc)))))
                 ',name)
         `(cl:defvar ,name ,@value-and-doc)))

(defmacro defun (name args &body body)
  (cond ((listp args)
         `(cl:defun ,name ,args ,@body))
        ((cl:member (symbol-name args) '("EXPR") :test #'string=)
         `(cl:defun ,name ,@body))
        ((cl:member (symbol-name args) '("FEXPR" "LEXPR" "MACRO" "ARGS")
                 :test #'string=)
         (error "Franz DEFUN: function type ~s is not supported (in ~s)"
                args name))
        (t
         (let ((rest (gensym "LEXPR-ARGS")))
           `(cl:defun ,name (&rest ,rest)
              (let ((*lexpr-args* ,rest)
                    (,args (length ,rest)))
                (declare (ignorable ,args))
                ,@body))))))

;;; ---------------------------------------------------------------------------
;;; Declarations

;; pnet-functions.lisp has a top-level Franz compiler directive
;; (declare (macros t)), ported as (declaim (macros t)).  MACROS is not a CL
;; declaration; make SBCL accept it silently.
(declaim (declaration macros))

;;; ---------------------------------------------------------------------------
;;; Keyword-style IF
;;;
;;;   (if a then b c ... elseif d then e ... else f g ...)
;;;   (if a thenret else f)      ; thenret: return the value of the test
;;;   (if a b) / (if a b c)      ; no keywords: plain CL IF
;;;
;;; Keywords are recognised by name, whatever package they were read in.

(defun franz-if-keyword-p (x name)
  (and (symbolp x) x (string= (symbol-name x) name)))

(defun franz-if-keyword-form-p (args)
  (some (lambda (x)
          (some (lambda (k) (franz-if-keyword-p x k))
                '("THEN" "THENRET" "ELSE" "ELSEIF")))
        args))

(defun franz-if-clauses (args)
  "ARGS = (test then ...), (test thenret ...).  Return a list of COND clauses."
  (when (null args)
    (error "Franz IF: missing test"))
  (let ((test (pop args))
        (key (pop args)))
    (multiple-value-bind (body rest)
        (let ((pos (position-if (lambda (x)
                                  (or (franz-if-keyword-p x "ELSE")
                                      (franz-if-keyword-p x "ELSEIF")))
                                args)))
          (values (subseq args 0 pos) (and pos (nthcdr pos args))))
      (let ((clause
              (cond ((franz-if-keyword-p key "THEN")
                     ;; (cond (test)) would return the test; THEN with no
                     ;; forms returns nil.
                     `(,test ,@(or body '(nil))))
                    ((franz-if-keyword-p key "THENRET")
                     (when body
                       (error "Franz IF: forms after THENRET: ~s" body))
                     `(,test))
                    (t (error "Franz IF: expected THEN or THENRET after test, got ~s"
                              key)))))
        (cons clause
              (cond ((null rest) nil)
                    ((franz-if-keyword-p (car rest) "ELSEIF")
                     (franz-if-clauses (cdr rest)))
                    (t ; ELSE
                     (when (find-if (lambda (x)
                                      (or (franz-if-keyword-p x "ELSE")
                                          (franz-if-keyword-p x "ELSEIF")))
                                    (cdr rest))
                       (error "Franz IF: keyword after ELSE: ~s" rest))
                     `((t ,@(or (cdr rest) '(nil)))))))))))

(defmacro if (&rest args)
  (cond ((franz-if-keyword-form-p (cdr args))
         `(cond ,@(franz-if-clauses args)))
        ((<= 2 (length args) 3)
         `(cl:if ,@args))
        (t (error "Franz IF: illegal form ~s" (cons 'if args)))))

;;; ---------------------------------------------------------------------------
;;; Arithmetic
;;;
;;; plus/add/sum, times/product, difference/diff are generic (any mix of
;;; fixnums and flonums, contagion to flonum).  quotient and / divide
;;; integers by truncation, like Franz; a flonum argument makes it a flonum
;;; division.  As in MacLisp/Franz, (difference x) and (quotient x) return x;
;;; negation is MINUS.

(defun plus (&rest numbers) (apply #'+ numbers))
(defun add (&rest numbers) (apply #'+ numbers))
(defun sum (&rest numbers) (apply #'+ numbers))
(defun times (&rest numbers) (apply #'* numbers))
(defun product (&rest numbers) (apply #'* numbers))

(defun difference (&rest numbers)
  (cl:if numbers
         (reduce #'- numbers)
         0))
(defun diff (&rest numbers) (apply #'difference numbers))

(defun franz-divide-2 (a b)
  (cl:if (and (integerp a) (integerp b))
         (values (truncate a b))
         (cl:/ (float a) b)))

(defun quotient (&rest numbers)
  (cond ((null numbers) 1)
        (t (reduce #'franz-divide-2 numbers))))

(defun / (&rest numbers) (apply #'quotient numbers))

(defun minus (x) (- x))
;; MIN is shadowed only because codelets.lisp `eliminate' also uses it as a
;; free variable (package.lisp); the function is plain CL:MIN.
(defun min (&rest numbers) (apply #'cl:min numbers))
;; MAX: codelets.lisp `temperature' does (apply 'max misfort), and misfort is
;; nil whenever the cytoplasm has no blocks or dtargets yet (the first
;; check-temperature of every run).  CL:MAX requires an argument.  The 1987
;; run did not fail there, so Franz (max) must have returned a number; 0 is
;; assumed (no misfortune; `mean' also returns 0 for an empty list).  With
;; arguments this is CL:MAX.  See PORTING_NOTES.md, item 9.
(defun max (&rest numbers)
  (cl:if numbers (apply #'cl:max numbers) 0))
(defun add1 (x) (+ x 1))
(defun sub1 (x) (- x 1))

(defun fix (x)
  "Franz FIX: the fixnum closest to X, rounding down."
  (values (floor x)))

(defun *quo (x y)
  "Franz *QUO: integer quotient, truncated."
  (values (truncate x y)))

(defun mod (x y)
  "Franz MOD (= remainder): the result has the sign of the dividend."
  (rem x y))

(defun remainder (x y) (rem x y))

(defun *mod (x y)
  "Franz *MOD: balanced representation of X modulo Y, i.e. a value in
[|Y|/2 - |Y| + 1, |Y|/2] congruent to X mod Y (|Y|/2 truncated)."
  (let* ((n (abs y))
         (r (cl:mod x n)))
    (cl:if (> r (floor n 2)) (- r n) r)))

(defun nequal (x y) (not (equal x y)))

;;; ---------------------------------------------------------------------------
;;; Lists

(defun memq (x list) (cl:member x list :test #'eq))

(defun member (x list)
  "Franz MEMBER compares with EQUAL."
  (cl:member x list :test #'equal))

(defun sortcar (list predicate)
  "Franz SORTCAR: sort LIST of lists by their cars with PREDICATE (destructive).
A nil PREDICATE means alphabetical order (ALPHALESSP)."
  (sort list (or predicate 'alphalessp) :key #'car))

;;; ---------------------------------------------------------------------------
;;; Symbols and strings

(defun invert-case (string)
  (cond ((notany #'upper-case-p string) (string-upcase string))
        ((notany #'lower-case-p string) (string-downcase string))
        (t (copy-seq string))))

(defun franz-pname (x)
  "The print name X would have in Franz: symbols are case-inverted, strings
are taken as they are, numbers are printed in base 10."
  (cond ((stringp x) x)
        ((symbolp x) (invert-case (symbol-name x)))
        (t (let ((*print-base* 10) (*print-radix* nil))
             (princ-to-string x)))))

(defun concat (&rest args)
  "Franz CONCAT: concatenate the print names of ARGS and intern the result."
  (values (cl:intern (invert-case (apply #'concatenate 'string
                                         (mapcar #'franz-pname args)))
                     :numbo)))

(defun uconcat (&rest args)
  "Franz UCONCAT: like CONCAT, but the result is an uninterned symbol."
  (make-symbol (invert-case (apply #'concatenate 'string
                                   (mapcar #'franz-pname args)))))

(defun get-pname (symbol)
  "Franz GET-PNAME: the print name of SYMBOL, as a string."
  (franz-pname symbol))

(defun alphalessp (x y)
  "Franz ALPHALESSP: compare print names."
  (and (string< (franz-pname x) (franz-pname y)) t))

(defun string-length (x)
  "Franz STRING-LENGTH: length of a string or of a symbol's print name."
  (length (franz-pname x)))

(defun intern (name &optional (package *package*))
  "Like CL INTERN, but NAME may also be a symbol (as Franz INTERN takes)."
  (values (cl:intern (cl:if (symbolp name) (symbol-name name) name)
                     (or package *package*))))

(defun find-package (name)
  "CL FIND-PACKAGE, also accepting a Franz-case name such as \"keyword\"."
  (or (cl:find-package name)
      (and (or (stringp name) (symbolp name))
           (cl:find-package (invert-case (string name))))))

;;; ---------------------------------------------------------------------------
;;; Vectors

(defun new-vector (size &optional fill property)
  "Franz NEW-VECTOR: a vector of SIZE elements, each FILL.  The property
argument is accepted and ignored."
  (declare (ignore property))
  (make-array size :initial-element fill))

(defun vref (vector index) (svref vector index))

(defun vset (vector index value)
  (setf (svref vector index) value))

;;; ---------------------------------------------------------------------------
;;; I/O and environment

(defun print (x &optional (stream *standard-output*))
  "Franz PRINT: like PRIN1 (no leading newline, no trailing space)."
  (prin1 x stream))

(defun getenv (name)
  "Franz GETENV: value of the environment variable NAME, or \"\" if unset."
  (or (sb-ext:posix-getenv (franz-pname name)) ""))

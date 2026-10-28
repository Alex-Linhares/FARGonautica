;;; lucid-compat.lisp -- Lucid Common Lisp (Sun, 1989-1993) behaviour that
;;; Mitchell's Copycat relies on and ANSI Common Lisp / SBCL does not provide.
;;;
;;; Every definition here was found by loading the unmodified source and
;;; reading the error or warning it produced; the census is in
;;; PORTING_NOTES.md.  Nothing here changes the program's behaviour, it only
;;; makes the 1989 text mean on SBCL what it meant on Lucid.
;;;
;;; This file is loaded before any of Mitchell's files, so it must not use
;;; anything they define (if* and sqr are in her lucid-util.l).

(cl:in-package :copycat)

;;; ---------------------------------------------------------------------------
;;; (in-package 'user)
;;;
;;; CLtL1's IN-PACKAGE was a function taking a package name, so the quoted
;;; symbol worked.  ANSI's is a macro over a string designator, and SBCL
;;; signals "'USER is not a string designator".  All of Copycat lives in one
;;; package here, so whatever the argument, the form selects COPYCAT.

(defmacro in-package (name)
  (declare (ignore name))
  '(cl:in-package :copycat))

;;; ---------------------------------------------------------------------------
;;; (production-mode)
;;;
;;; A Lucid compiler switch (ccat-sys.l): "the most efficient compiled code".
;;; SBCL's equivalent would be an OPTIMIZE declaim; the default is kept.

(defun production-mode () nil)

;;; ---------------------------------------------------------------------------
;;; LOOP: an empty `do' before a keyword
;;;
;;; Four loops in the source (initialization.l x2, answer.l, correspondences.l)
;;; read "... do collect i" or "... do when ...": a `do' with no body form,
;;; followed by the next clause.  Three more (util.l's pause functions) read
;;; "(loop for i from 1 to 1000 do ())".  Lucid's LOOP took both as an empty
;;; `do'; SBCL's says "A compound form was expected, but COLLECT found" (or
;;; "but NIL found").  This LOOP drops a `do'/`doing' that is followed by a
;;; loop keyword or is last, drops a `do ()' pair, and hands everything else
;;; to CL:LOOP unchanged.

(defparameter *loop-keywords*
  '("NAMED" "WITH" "FOR" "AS" "REPEAT" "INITIALLY" "FINALLY"
    "DO" "DOING" "RETURN" "COLLECT" "COLLECTING" "APPEND" "APPENDING"
    "NCONC" "NCONCING" "COUNT" "COUNTING" "SUM" "SUMMING"
    "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING"
    "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS"
    "WHEN" "IF" "UNLESS" "ELSE" "END" "AND" "INTO")
  "The ANSI LOOP keywords, by name (LOOP compares them by name).")

(defun loop-keyword-p (x)
  (and (symbolp x) x (member (symbol-name x) *loop-keywords* :test #'string=)))

(defun loop-do-p (x)
  (and (symbolp x) x (member (symbol-name x) '("DO" "DOING") :test #'string=)))

(defun drop-empty-do-clauses (clauses)
  (let ((kept '()) (skip nil))
    (cl:loop for (this . rest) on clauses
             do (cond (skip (setq skip nil))         ; the () of a `do ()'
                      ((and (loop-do-p this)
                            (or (null rest) (loop-keyword-p (car rest))))
                       nil)                          ; `do' before a keyword, or last
                      ((and (loop-do-p this) (null (car rest)))
                       (setq skip t))                ; `do ()'
                      (t (push this kept))))
    (nreverse kept)))

(defmacro loop (&rest clauses)
  (if (every #'consp clauses)            ; the simple (loop form...) form
      `(cl:loop ,@clauses)
      `(cl:loop ,@(drop-empty-do-clauses clauses))))

;;; ---------------------------------------------------------------------------
;;; Quoted lambda expressions as functions
;;;
;;; slipnet-def.l stores description testers as '(lambda (object) ...) and
;;; slipnet-def.l:1171 and answer.l:117 FUNCALL them; copystat.l:185 sorts
;;; with :key '(lambda (x) ...).  CLtL1 allowed a lambda expression wherever
;;; a function was wanted; ANSI function designators are functions and
;;; symbols only, and SBCL refuses the list.  These FUNCALL and SORT compile
;;; such a list once (cached on the list itself, which is a literal in the
;;; source) and otherwise behave as CL:FUNCALL and CL:SORT.

(defvar *compiled-lambda-expressions* (make-hash-table :test 'eq))

(defun as-function (designator)
  (if (and (consp designator) (eq (car designator) 'lambda))
      (or (gethash designator *compiled-lambda-expressions*)
          (setf (gethash designator *compiled-lambda-expressions*)
                (coerce designator 'function)))
      designator))

(defun funcall (function &rest arguments)
  (apply (as-function function) arguments))

(defun sort (sequence predicate &key key)
  (cl:sort sequence (as-function predicate)
           :key (and key (as-function key))))

;;; ---------------------------------------------------------------------------
;;; WITH-OPEN-FILE on an existing output file
;;;
;;; copystat.l rewrites its .random-state file after every run with
;;; (with-open-file (s file :direction :output :if-does-not-exist :create)),
;;; no :if-exists.  The standard default is :new-version when the pathname's
;;; version is :newest (as it is for Unix names); Lucid on SunOS, with no
;;; file versions, wrote the file afresh, SBCL signals "file exists".  This
;;; WITH-OPEN-FILE supplies :if-exists :supersede for an output file when
;;; the form gives no :if-exists, and is otherwise CL:WITH-OPEN-FILE.  Only
;;; copystat.l and util.l's get-random-state open files; the engine does not.

(defmacro with-open-file ((stream filespec &rest options) &body body)
  (let ((options (if (and (eq (getf options :direction) :output)
                          (not (getf options :if-exists)))
                     (list* :if-exists :supersede options)
                     options)))
    `(cl:with-open-file (,stream ,filespec ,@options) ,@body)))

;;; ---------------------------------------------------------------------------
;;; Lucid built-ins that are not in ANSI CL

(defun memq (item list)
  (member item list :test #'eq))

(defun string-append (&rest strings)
  "Lucid STRING-APPEND: concatenate string designators.  Symbols are
accepted: copystat.l passes init-ccat's arguments, which must be symbols
(init-ccat takes their SYMBOL-NAME), to this function too."
  (apply #'concatenate 'string (mapcar #'string strings)))

(defun append-symbols (&rest parts)
  "Lucid APPEND-SYMBOLS: intern the concatenated print names of PARTS.
Symbols read by Lucid's reader are upper case, as here, so
(append-symbols 'plato- \"A\") is PLATO-A, the slipnode variable."
  (values (intern (apply #'concatenate 'string (mapcar #'string parts))
                  :copycat)))

(defun run-program (program &key arguments (input nil) (output nil) (wait t))
  "Lucid RUN-PROGRAM (copystat.l: (run-program \"cp\" :arguments (list ...)))."
  (sb-ext:run-program program arguments :search t :wait wait
                                        :input input :output output))

;;; util.l: (defun flavor-type (instance) (flavors::flavor-type-of instance)),
;;; used to compare the flavors of two objects with EQ.  Flavors are CLOS
;;; classes here (flavors-compat.lisp), so the flavor's name is the class name.
(defun flavors:flavor-type-of (instance)
  (class-name (class-of instance)))

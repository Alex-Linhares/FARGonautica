;;; package.lisp -- the COPYCAT package, and the FLAVORS package it refers to.
;;;
;;; Mitchell's files all begin with (in-package 'user): in Lucid Common Lisp
;;; (1989) they lived in the USER package.  Here every file is read into the
;;; COPYCAT package, which shadows the handful of CL symbols whose Lucid-era
;;; behaviour the code relies on.  The shadows are defined in
;;; lucid-compat.lisp and flavors-compat.lisp.
;;;
;;;   defmethod    Flavors syntax (defmethod (flavor :message) ...)
;;;   in-package   (in-package 'user): CLtL1 took a symbol, ANSI does not
;;;   loop         Lucid's LOOP accepted `do' with no body before a keyword
;;;   funcall      Lucid accepted a quoted (lambda ...) as a function
;;;   sort         same, for the :key argument
;;;   with-open-file  Lucid superseded an existing output file by default

(defpackage :copycat
  (:use :common-lisp)
  (:shadow #:defmethod #:in-package #:loop #:funcall #:sort #:with-open-file)
  (:export #:run #:sweep #:run-cli
           ;; Mitchell's entry points, for the REPL.
           #:init-ccat #:run-ccat #:copystat #:copystat-summary
           #:*answer-string* #:*rule* #:*translated-rule* #:*temperature*
           #:*codelet-count* #:*snag-count* #:*random-state-this-run*))

;;; util.l's FLAVOR-TYPE calls flavors::flavor-type-of, Lucid's Flavors
;;; package.  Only that one symbol is needed.
(defpackage :flavors
  (:use)
  (:export #:flavor-type-of))

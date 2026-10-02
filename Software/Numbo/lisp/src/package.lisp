;;; package.lisp -- the NUMBO package.
;;;
;;; The 1987 source is Franz Lisp + Flavors.  It is loaded into its own
;;; package so that Franz-style definitions (keyword `if`, Flavors-style
;;; `defmethod`, ...) can shadow the CL symbols without touching CL-USER.
;;; The shadowed symbols are defined in franz-compat.lisp / flavors-compat.lisp.
;;;
;;; Shadowed for Franz semantics (franz-compat.lisp):
;;;   DEFUN       Franz defun: optional function type, atom arglist = lexpr
;;;   IF          keyword-style (if c then a else b)
;;;   /           Franz `/` is integer (truncating) division on fixnums
;;;   MOD         Franz `mod` is the remainder (sign of the dividend) = CL REM
;;;   MEMBER      Franz `member` tests with EQUAL (CL default is EQL)
;;;   PRINT       Franz `print` = PRIN1, no leading newline / trailing space
;;;   INTERN      Franz-style: accepts a symbol as well as a string
;;;   FIND-PACKAGE  accepts a lower-case Franz name ("keyword")
;;;   DEFVAR      init.lisp:3 (defvar *print-array*) names a CL variable;
;;;               no package lock in Franz (franz-compat.lisp)
;;;   MAX         (max) with no arguments returns 0 (codelets `temperature')
;;; Shadowed for Flavors (flavors-compat.lisp):
;;;   DEFMETHOD   (defmethod (flavor :message) lambda-list body...)
;;;   MAKE-INSTANCE  Flavors keyword init + :init message
;;; Shadowed because the 1987 source defines its own function of that name
;;; (CL package lock otherwise):
;;;   ROUND       codelets.lisp `round` (nearest multiple of 5/10/50)
;;;   RATIO       codelets.lisp `ratio`
;;; Shadowed because the source uses it as a global variable:
;;;   TYPE        cyto-def.lisp cytoplasm methods (setq type ...) a free
;;;               variable (CL package lock: "setting the value of TYPE");
;;;               also a cyto-node / cyto-context instance variable.
;;;   MIN         codelets.lisp `eliminate' (setq min ...) a free variable
;;;               (CL package lock); the function is CL:MIN (franz-compat).

(defpackage :numbo
  (:use :common-lisp)
  (:shadow #:defun #:defvar #:if #:/ #:mod #:member #:print #:intern #:find-package
           #:round #:ratio #:type #:min #:max
           #:defmethod #:make-instance))

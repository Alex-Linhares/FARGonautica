;;; copycat.asd -- ASDF system for Melanie Mitchell's Copycat on SBCL.
;;;
;;; load.lisp is the primary loader (no ASDF needed); this system loads the
;;; same files in the same order for people who use (asdf:load-system
;;; "copycat") or Quicklisp's local-projects.  Mitchell's files keep their
;;; ".l" extension, hence the :type on each.
;;;
;;;   (asdf:load-system "copycat")
;;;   (copycat:run "abc" "abd" "ijk" :seed 1)

(defclass cl-source-file.l (cl-source-file)
  ((type :initform "l"))
  (:documentation "A Mitchell source file: Lisp with the extension .l."))

(defun compile-in-copycat (thunk)
  "Mitchell's files start with (in-package 'user), which only means what it
should when read in the COPYCAT package (compat/lucid-compat.lisp)."
  (let ((*package* (or (find-package :copycat) *package*)))
    (funcall thunk)))

(defsystem "copycat"
  :description "Melanie Mitchell's Copycat (Lucid Common Lisp, 1989-93) on SBCL, headless."
  :author "Melanie Mitchell (Copycat); compatibility layer, FARGonautica, 2026"
  :depends-on ()
  :serial t
  :around-compile compile-in-copycat
  :components
  ((:module "compat"
    :serial t
    :components ((:file "package")
                 (:file "lucid-compat")
                 (:file "flavors-compat")
                 (:file "graphics-stubs")))
   (:module "src"
    :serial t
    :default-component-class :cl-source-file.l
    :components ((:file "lucid-util")
                 (:file "file-system")
                 (:file "ccat-sys")
                 (:file "util")
                 (:file "constants")
                 (:file "workspace")
                 (:file "workspace-structures")
                 (:file "workspace-strings")
                 (:file "workspace-objects")
                 (:file "initialization")
                 (:file "run")
                 (:file "slipnet-def")
                 (:file "slipnet-links")
                 (:file "slipnet-functions")
                 (:file "descriptions")
                 (:file "bonds")
                 (:file "groups")
                 (:file "concept-mappings")
                 (:file "correspondences")
                 (:file "replacements")
                 (:file "breakers")
                 (:file "coderack")))
   (:module "compat2" :pathname "compat"
    :components ((:file "coderack-graphics-methods")))
   (:module "src2" :pathname "src"
    :serial t
    :default-component-class :cl-source-file.l
    :components ((:file "rule")
                 (:file "answer")
                 (:file "workspace-object-formulas")
                 (:file "workspace-structure-formulas")
                 (:file "formulas")
                 (:file "copystat")))
   (:file "harness")))

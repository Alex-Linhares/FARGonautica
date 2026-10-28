;;; load.lisp -- load Melanie Mitchell's Copycat into SBCL, headless.
;;;
;;;   sbcl --non-interactive --load load.lisp \
;;;        --eval '(copycat:run "abc" "abd" "ijk" :seed 1)'
;;;
;;; Order: the compatibility layer (compat/), then Mitchell's files in the
;;; order of her system definition (src/ccat-sys.l), minus the graphics
;;; files, which compat/graphics-stubs.lisp stands in for.  Every file is
;;; read into the COPYCAT package; her (in-package 'user) lines are kept and
;;; mean that (see compat/lucid-compat.lisp).
;;;
;;; The files are loaded as source.  SBCL compiles each top-level form as it
;;; is loaded, so the running code is compiled code; loading takes about
;;; four seconds.  Set CL-USER::*COPYCAT-VERBOSE-LOAD* to T to see the
;;; compiler's notes (they are muffled by default: the 1989 code produces
;;; style warnings about undefined variables that are proclaimed special
;;; later in the same file, and about unused &aux variables).

(cl:in-package :cl-user)

(defparameter *copycat-dir*
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *default-pathname-defaults*)))

(defvar *copycat-verbose-load* nil)

(defparameter *copycat-compat-files*
  '("package" "lucid-compat" "flavors-compat" "graphics-stubs"))

(defparameter *copycat-source-files*
  ;; src/ccat-sys.l's defsys list, in its order, minus the graphics files
  ;; (slipnet-graphics, graphics-util, coderack-graphics, letter-graphics,
  ;; description-graphics, bond-graphics, group-graphics,
  ;; correspondence-graphics, replacement-graphics, rule-graphics,
  ;; workspace-graphics, temperature-graphics, ccat-bar-graph, ccat-menu).
  ;; lucid-util, file-system and ccat-sys come first, as they did in
  ;; Mitchell's Lucid image (ccat-sys.l needs defsys from file-system.l and
  ;; if* from lucid-util.l, and proclaims every global special).
  '("lucid-util" "file-system" "ccat-sys"
    "util" "constants" "workspace" "workspace-structures"
    "workspace-strings" "workspace-objects" "initialization" "run"
    "slipnet-def" "slipnet-links" "slipnet-functions"
    "descriptions" "bonds" "groups" "concept-mappings" "correspondences"
    "replacements" "breakers" "coderack"
    ;; the two coderack methods from coderack-graphics.l (see the file)
    (:compat "coderack-graphics-methods")
    "rule" "answer" "workspace-object-formulas"
    "workspace-structure-formulas" "formulas" "copystat"))

(defun copycat-load-file (subdir name type)
  (let ((path (merge-pathnames (make-pathname :directory (list :relative subdir)
                                              :name name :type type)
                               *copycat-dir*)))
    (unless (probe-file path)
      (error "copycat load: ~a is missing" path))
    (let ((*package* (or (find-package :copycat) *package*)))
      (if *copycat-verbose-load*
          (load path)
          (handler-bind ((warning #'muffle-warning))
            (let ((*error-output* (make-broadcast-stream)))
              (load path)))))))

(defun copycat-load ()
  (dolist (f *copycat-compat-files*)
    (copycat-load-file "compat" f "lisp"))
  (dolist (f *copycat-source-files*)
    (if (consp f)
        (copycat-load-file "compat" (second f) "lisp")
        (copycat-load-file "src" f "l")))
  (copycat-load-file "" "harness" "lisp")
  t)

(copycat-load)

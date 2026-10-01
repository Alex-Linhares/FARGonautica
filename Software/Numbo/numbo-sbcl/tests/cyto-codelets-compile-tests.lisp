;;; cyto-codelets-compile-tests.lisp -- item 8: compile the cytoplasm and codelets.
;;;
;;; Run: sbcl --non-interactive --load tests/cyto-codelets-compile-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;;  1. pnet-def, pnet-functions, pnet-graphics, cyto-def, codelets go through
;;;     COMPILE-FILE in load order (each fasl loaded before the next file is
;;;     compiled).  cyto-def and codelets: no errors, no undefined functions,
;;;     no package-lock warnings.  The only full WARNINGs are undefined
;;;     variables, exactly the documented sets.  The style warnings are all
;;;     unused locals of the original code.
;;;  2. Whole system: all seven source files compiled in ONE compilation unit.
;;;     Zero undefined functions at the end of the unit, and no errors.
;;;     (Until item 9 init.lisp's (defvar *print-array*) was a package-lock
;;;     error, continued past here; the franz-compat DEFVAR fixed it, so any
;;;     package-lock error is now counted and must not happen.)  A control file calling an undefined function shows that the
;;;     census does catch one.
;;;  3. globals.lisp census for the item-8 globals: never bound lexically, not
;;;     instance variables, proclaimed special.  Every undefined variable left
;;;     in part 1 IS bound lexically somewhere (or is an instance variable),
;;;     which is why it stays undeclared.  TYPE and MIN are NUMBO symbols.
;;;  4. The compiled codelet code runs: eliminate (free MIN), round (free DIV),
;;;     sim, remove-dd, randlist, and on a real cytoplasm for (31 3 5 24 3 14),
;;;     read-brick hangs its create-cyto-node codelet and that codelet runs.

(defvar cl-user::*numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(unless (cl-user::numbo-load '("package" "franz-compat" "flavors-compat" "coderack"
                                "graphics-stubs" "globals"))
  (format t "~&cyto-codelets-compile: failed to load~%")
  (sb-ext:exit :code 1))

(in-package :numbo)

(load (merge-pathnames "compile-helpers.lisp" *load-truename*))

;;; --- 1. compile-file, file by file -------------------------------------------
(defparameter *results*
  (loop for f in '("pnet-def" "pnet-functions" "pnet-graphics" "cyto-def" "codelets")
        collect (cons f (compile-and-load f))))

(defun result (name) (cdr (assoc name *results* :test #'string=)))

(defun unused-variable-message-p (m)
  (or (search "is defined but never used." m)
      (search "is assigned but never read." m)))

(defun undefined-variable-message-p (m)
  (or (search "undefined variable" m)))    ; also "N more uses of undefined variable X"

;; The free variables left undeclared on purpose (see part 3 and
;; PORTING_NOTES.md, item 8).  SBCL reports each undefined variable once per
;; image, so NODE and RES (already reported for pnet-functions) and TYPE and
;; STATUS (cyto-def) are not repeated for codelets.
(defparameter *expected-undefined-variables*
  '(("cyto-def" "NUMBO::STATUS" "NUMBO::TYPE")
    ("codelets" "NUMBO::ACTIVATION" "NUMBO::ADDRESS" "NUMBO::CURRENT-TARGET"
     "NUMBO::CYTO-BLOCK1" "NUMBO::CYTO-BLOCK2" "NUMBO::DIFF" "NUMBO::N1" "NUMBO::N2"
     "NUMBO::N3" "NUMBO::NEW" "NUMBO::NN" "NUMBO::NODE1" "NUMBO::NODE2"
     "NUMBO::OPER" "NUMBO::TARGET" "NUMBO::VALUES-TO-FIND")))

(dolist (f '("cyto-def" "codelets"))
  (destructuring-bind (fasl warnings-p failure-p full style errors) (result f)
    (declare (ignore warnings-p failure-p))
    (check (list f (and fasl (probe-file fasl) t)) (list f t))
    (check (list f errors) (list f nil))
    (check (cons f (undefined-names full "variable"))
           (assoc f *expected-undefined-variables* :test #'string=))
    (check (list f (remove-if #'undefined-variable-message-p full)) (list f nil))
    (check (list f (remove-if-not (lambda (m) (search "package lock" m)) (append full style)))
           (list f nil))
    (check (list f (undefined-names style "function")) (list f nil))
    (check (list f (remove-if #'unused-variable-message-p style)) (list f nil))))

;; the earlier files are unchanged by item 8
(destructuring-bind (fasl warnings-p failure-p full style errors) (result "pnet-graphics")
  (declare (ignore style))
  (check (list (and fasl t) warnings-p failure-p full errors) '(t nil nil nil nil)))
(check (undefined-names (fourth (result "pnet-functions")) "variable")
       '("NUMBO::NODE" "NUMBO::RES"))

(check (every #'compiled-function-p
              (list #'init-cytoplasm #'read-brick #'look-for-new-block #'eliminate
                    #'check-temperature #'propagate-success #'create-coderack))
       t)

;;; --- 2. whole system in one compilation unit ----------------------------------
(defun compile-system-census (files)
  "COMPILE-FILE every file of FILES (names, or pathnames of extra files) in one
compilation unit, without loading.  Return (undefined-functions errors
package-lock-continues)."
  (let (undefined errors (continues 0))
    (handler-bind
        ((style-warning
           (lambda (c)
             (let ((m (princ-to-string c)))
               (when (search "undefined function" m) (push m undefined)))))
         (sb-ext:symbol-package-locked-error
           (lambda (c)
             ;; init.lisp:3 (defvar *print-array*) used to land here (item 9).
             (incf continues)
             (continue c)))
         (error (lambda (c) (push (princ-to-string c) errors))))
      (with-compilation-unit ()
        (dolist (f files)
          (let ((*package* (find-package :numbo))
                (*compile-verbose* nil) (*compile-print* nil)
                (src (if (stringp f) (source-path f) f)))
            (handler-bind ((warning #'muffle-warning))
              (compile-file src :output-file (fasl-path (format nil "census-~a"
                                                                (pathname-name src)))))))))
    (list (undefined-names (reverse undefined) "function") (reverse errors) continues)))

(defparameter *census* (compile-system-census *source-files*))
(check (first *census*) nil)                       ; zero undefined functions
(check (second *census*) nil)
(check (third *census*) 0)                         ; no package locks

;; control: the census does see an undefined function
(defparameter *control-file* (fasl-path "census-control"))
(defparameter *control-src* (make-pathname :type "lisp" :defaults *control-file*))
(with-open-file (s *control-src* :direction :output :if-exists :supersede)
  (write-string "(defun census-control-caller () (census-control-no-such-function 1))" s))
(check (first (compile-system-census (list "cyto-def" *control-src*)))
       '("NUMBO::CENSUS-CONTROL-NO-SUCH-FUNCTION"))
(delete-file *control-src*)

;;; --- 3. globals.lisp census ---------------------------------------------------
(defparameter *all-source-forms*
  (loop for f in *source-files* append (read-forms f)))
(defparameter *lexicals* (lexically-bound-symbols *all-source-forms*))
(defparameter *ivars*
  (loop for form in *all-source-forms*
        when (and (consp form) (eq (car form) 'defflavor))
          append (mapcar (lambda (v) (if (consp v) (car v) v)) (caddr form))))

(defparameter *item8-globals*
  '(*cytoplasm* *context* *temperature* *problem-solved* cyto-target
    %operand% %result+% %resultx% a brick bricki cyto-bricki cont n pp
    diffrel div liste similarity reserve weights min lv))
(defparameter *globals*
  (loop for form in (read-forms "globals")
        when (and (consp form) (eq (car form) 'defvar)) collect (cadr form)))

(check (set-difference *item8-globals* *globals*) nil)
(check (intersection *item8-globals* *lexicals*) nil)
(check (intersection *item8-globals* *ivars*) nil)
(check (remove-if #'globally-special-p *item8-globals*) nil)
;; every item-8 global really is used free in the source
(defun occurs-p (sym tree)
  (or (eq sym tree) (and (consp tree) (or (occurs-p sym (car tree)) (occurs-p sym (cdr tree))))))
(check (remove-if (lambda (g) (occurs-p g *all-source-forms*)) *item8-globals*) nil)

;; the undefined variables left in part 1 are bound lexically somewhere (so
;; proclaiming them special would turn those LETs dynamic) and none of them
;; is special
(defparameter *left-undeclared*
  (remove-duplicates
   (loop for (nil . r) in *results*
         append (mapcar (lambda (n) (cl:intern (subseq n (length "NUMBO::")) :numbo))
                        (undefined-names (fourth r) "variable")))))
(check (length *left-undeclared*) 20)
(check (remove-if (lambda (v) (cl:member v *lexicals*)) *left-undeclared*) nil)
(check (remove-if-not #'globally-special-p *left-undeclared*) nil)

;; TYPE and MIN are NUMBO's own (shadowed) symbols; MIN is still CL:MIN as a function
(check (mapcar #'symbol-package '(type min))
       (list (cl:find-package :numbo) (cl:find-package :numbo)))
(check (min 4 2 9) 2)

;;; --- 4. the compiled code runs --------------------------------------------------
;; init.lisp's DEFVAR values (init.lisp itself does not load yet, item 9).
(dolist (form (read-forms "init"))
  (when (and (consp form) (eq (car form) 'defvar) (cddr form)
             (not (eq (cadr form) '*print-array*)))
    (eval form)))

(check (eliminate 8 '(4 6 7)) '(4 6))              ; the docstring example
(check min 7)                                      ; eliminate's free SETQ
(check (eliminate 8 '(9 7)) '(7))                  ; tie: first best goes
(check (list (round 17) (round 31) (round 36) (round 160)) '(15 30 40 150))
(check div 50)                                     ; round's free SETQ
(check (list (sim 31 31) (sim 30 31) (sim 25 31) (sim 3 31)) '(0 1 2 4))
(check (remove-dd 3 '(2 3 4 3)) '(2 4 3))          ; the docstring example
(check (randlist nil) nil)
(check (randlist '(100 0)) 0)
(check (ratio 31) (/ 30.0 31.0))

;; A real cytoplasm and coderack, then read-brick and its codelet.
(setq *random-state* (sb-ext:seed-random-state 31))
(initialize-pnet)
(initialize-pnet-2)
(init-cytoplasm 31 3 5 24 3 14)
(check (create-coderack) 'my-coderack)
(check (cr-empty? *coderack*) t)
(check (progn (read-brick 1) t) t)
(check (list bricki a brick) '(brick1 :brick1 3))  ; read-brick's free SETQs
(check cyto-bricki 'cyto-brick1)
(check (cr-count *coderack*) 1)
(defparameter *codelet* (cr-choose *coderack*))
(check (car *codelet*) 'create-cyto-node)
(check (progn (eval *codelet*) t) t)
(check (typep cyto-brick1 'cyto-node) t)
(check (list (send cyto-brick1 :value) (send cyto-brick1 :type) (send cyto-brick1 :status))
       '(3 "2b" "free"))
(check (and (cl:member cyto-brick1 (send *cytoplasm* :nodes)) t) t)

;;; --- cleanup -----------------------------------------------------------------
(cleanup-fasl-dir)

(format t "~&cyto-codelets-compile: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (cl:if (zerop *failures*) 0 1))

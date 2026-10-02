;;; pnet-compile-tests.lisp -- item 7: compile the Pnet files cleanly.
;;;
;;; Run: sbcl --non-interactive --load tests/pnet-compile-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;;  1. pnet-def.lisp and pnet-functions.lisp go through COMPILE-FILE (in
;;;     load order, each fasl loaded before the next file is compiled).  No
;;;     errors and no undefined-function warnings.  The only full WARNINGs are
;;;     the two documented undefined variables NODE and RES (pnet-functions).
;;;  2. Census for src/globals.lisp: every proclaimed global is either an
;;;     init-pnet holder, a DEFVAR in init.lisp, or *pnet*.  None of them is
;;;     bound lexically anywhere in the source, and none is a flavor instance
;;;     variable.  Every init-pnet holder is proclaimed.  NODE and RES are not.
;;;  3. Loading the fasls ran (init-pnet): 91 pnode holders, of which 88 are
;;;     in *pnet* (all except PLUS, MINUS, TIMES).
;;;  4. With init.lisp's DEFVAR values, (initialize-pnet) runs and resets
;;;     every pnode.  (initialize-pnet-2) resolves every neighbor to a pnode,
;;;     and one spread-activation-in-pnet cycle runs on the compiled code.

(defvar cl-user::*numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(unless (cl-user::numbo-load '("package" "franz-compat" "flavors-compat" "coderack"
                                "graphics-stubs" "globals"))
  (format t "~&pnet-compile: failed to load~%")
  (sb-ext:exit :code 1))

(in-package :numbo)

(load (merge-pathnames "compile-helpers.lisp" *load-truename*))

;;; --- 1. compile-file ---------------------------------------------------------
(defparameter *pnet-def-result* (compile-and-load "pnet-def"))
(defparameter *pnet-functions-result* (compile-and-load "pnet-functions"))

(destructuring-bind (fasl warnings-p failure-p full style errors) *pnet-def-result*
  (declare (ignore style))
  (check (and fasl (probe-file fasl) t) t)
  (check errors nil)
  (check full nil)
  (check (list warnings-p failure-p) '(nil nil)))

(destructuring-bind (fasl warnings-p failure-p full style errors) *pnet-functions-result*
  (declare (ignore warnings-p failure-p))
  (check (and fasl (probe-file fasl) t) t)
  (check errors nil)
  ;; Every full warning is an undefined-variable warning (or SBCL's
  ;; "N more uses of undefined variable" summary line) for NODE or RES.
  (check (undefined-names full "variable") '("NUMBO::NODE" "NUMBO::RES"))
  (check (remove-if (lambda (m) (or (search "undefined variable" m)
                                    (search "use of undefined variable" m)
                                    (search "uses of undefined variable" m)))
                    full)
         nil)
  ;; No undefined functions at all (they would be style warnings).
  (check (undefined-names style "function") nil)
  ;; The style warnings are the four unused locals of the original code.
  (check (sort (copy-list style) #'string<)
         '("The variable ARGUMENTS is defined but never used."
           "The variable BASE-URGENCY is defined but never used."
           "The variable K is defined but never used."
           "The variable X is defined but never used.")))

;; The functions really are the compiled ones.
(check (compiled-function-p #'initialize-pnet) t)
(check (compiled-function-p #'init-pnet) t)

;;; --- 2. globals.lisp census --------------------------------------------------
(defparameter *all-source-forms*
  (loop for f in *source-files* append (read-forms f)))

(defparameter *lexicals* (lexically-bound-symbols *all-source-forms*))

(defparameter *ivars*
  (loop for form in *all-source-forms*
        when (and (consp form) (eq (car form) 'defflavor))
          append (mapcar (lambda (v) (if (consp v) (car v) v)) (caddr form))))

(defparameter *globals*
  (loop for form in (read-forms "globals")
        when (and (consp form) (eq (car form) 'defvar))
          collect (cadr form)))

(defparameter *init-pnet-holders*
  (let ((def (find-if (lambda (f) (and (consp f) (eq (car f) 'defun) (eq (cadr f) 'init-pnet)))
                      (read-forms "pnet-def"))))
    (loop for x in (cdddr def)
          when (and (consp x) (eq (car x) 'setq)) collect (cadr x))))

(defparameter *init-defvars*
  (loop for form in (read-forms "init")
        when (and (consp form) (eq (car form) 'defvar)) collect (cadr form)))

;; the walker finds known lexicals (sanity check of the walker itself)
(check (subsetp '(node res activation-to-add pnode codelet threshold) *lexicals*) t)
(check (length *init-pnet-holders*) 91)
(check (length (remove-duplicates *globals*)) (length *globals*))
;; Globals added for cyto-def/codelets/init/start (item 8); their census is
;; in tests/cyto-codelets-compile-tests.lisp.
(defparameter *item8-globals*
  '(*cytoplasm* *context* *temperature* *problem-solved* cyto-target
    %operand% %result+% %resultx% a brick bricki cyto-bricki cont n pp
    diffrel div liste similarity reserve weights min lv))
(check (remove-if (lambda (g) (or (cl:member g *init-pnet-holders*)
                                  (cl:member g *init-defvars*)
                                  (cl:member g *item8-globals*)
                                  (eq g '*pnet*)))
                  *globals*)
       nil)
(check (set-difference *init-pnet-holders* *globals*) nil)
(check (set-difference (remove '*print-array* *init-defvars*) *globals*) nil)
(check (intersection *globals* *lexicals*) nil)
(check (intersection *globals* *ivars*) nil)
(check (every #'globally-special-p *globals*) t)
(check (mapcar #'globally-special-p '(node res)) '(nil nil))
;; NODE and RES are kept undeclared because they ARE bound lexically elsewhere.
(check (and (subsetp '(node res) *lexicals*) t) t)

;;; --- 3. init-pnet ran when the fasl loaded -----------------------------------
(defun pnodep (x) (typep x 'pnode))

(check (every (lambda (s) (and (boundp s) (pnodep (symbol-value s)))) *init-pnet-holders*) t)
(check (length *pnet*) 88)
(check (length (remove-duplicates *pnet*)) 88)
(check (every #'pnodep *pnet*) t)
(check (set-exclusive-or *pnet* (mapcar #'symbol-value *init-pnet-holders*)) (list plus minus times)
       :test (lambda (a b) (null (set-exclusive-or a b))))
(check (length (remove-duplicates (mapcar (lambda (p) (send p :name)) *pnet*))) 88)
(check (send node-1 :value) 1)
(check (send node-1 :name) 'one)
(check (send node-150 :value) 150)
(check (send times9-9 :short-name) "9x9")
;; Before initialize-pnet-2 the neighbors are still symbols.
(check (every (lambda (p) (every (lambda (l) (and (symbolp (car l)) (symbolp (cadr l))))
                                 (send p :neighbors)))
              *pnet*)
       t)

;;; --- 4. initialize-pnet, initialize-pnet-2, one spreading cycle --------------
;; Use init.lisp's own DEFVAR values (init.lisp itself does not load yet:
;; its (defvar *print-array*) is item 9).
(dolist (form (read-forms "init"))
  (when (and (consp form) (eq (car form) 'defvar) (cddr form)
             (not (eq (cadr form) '*print-array*)))
    (eval form)))
(check %initial-activation% 0.0)
(check %first-threshold% 30)

(dolist (p *pnet*)                       ; dirty every pnode first
  (send p :set-activation 42)
  (send p :set-temp-activation-holder 7)
  (send p :set-spreadable-activation 3)
  (send p :set-instances '(("1t" foo))))
(check (progn (initialize-pnet) t) t)
(check (every (lambda (p) (and (eql (send p :activation) 0.0)
                               (eql (send p :temp-activation-holder) 0.0)
                               (eql (send p :spreadable-activation) 0.0)
                               (null (send p :instances))))
              *pnet*)
       t)
;; initialize-codelet: every codelet threshold now evaluates to %first-threshold%
(check (loop for p in *pnet*
             always (loop for c in (send p :codelets)
                          always (eql (eval (cadr c)) %first-threshold%)))
       t)
(check (plusp (loop for p in *pnet* sum (length (send p :codelets)))) t)

(check (progn (initialize-pnet-2) t) t)
(defparameter *holder-values* (mapcar #'symbol-value *init-pnet-holders*))
(check (loop for p in *pnet*
             always (loop for (n l) in (send p :neighbors)
                          always (and (pnodep n) (pnodep l)
                                      (cl:member n *holder-values*)
                                      (cl:member l *holder-values*))))
       t)
(check (plusp (loop for p in *pnet* sum (length (send p :neighbors)))) t)

;; One spreading cycle from a hot node-30 reaches its neighbors.
(send node-30 :set-activation 100)
(check (progn (spread-activation-in-pnet) t) t)
(check (every (lambda (p) (and (realp (send p :activation)) (>= (send p :activation) 0)))
              *pnet*)
       t)
(check (< 0 (send node-30 :activation) 100) t)
(check (every (lambda (p) (and (eql (send p :temp-activation-holder) 0.0)
                               (eql (send p :spreadable-activation) 0.0)))
              *pnet*)
       t)

;;; --- cleanup -----------------------------------------------------------------
(cleanup-fasl-dir)

(format t "~&pnet-compile: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (cl:if (zerop *failures*) 0 1))

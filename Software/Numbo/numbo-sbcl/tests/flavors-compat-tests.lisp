;;; flavors-compat-tests.lisp -- unit tests for src/flavors-compat.lisp.
;;;
;;; Run: sbcl --non-interactive --load tests/flavors-compat-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;; Part 1 tests the layer on test flavors.  Part 2 loads the real flavor
;;; files (pnet-def, pnet-functions, pnet-graphics, cyto-def) and exercises their flavors.
;;; Part 3 is a census: every message the source SENDs must be handled by
;;; some flavor (a DEFMETHOD or a generated :ivar / :set-ivar message).

(defvar cl-user::*numbo-no-autoload* t)
(load (merge-pathnames "../src/load.lisp" *load-truename*))
(unless (cl-user::numbo-load '("package" "franz-compat" "flavors-compat"))
  (format t "~&flavors-compat: failed to load~%")
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
     (unless (handler-case (progn ,form nil)
               (error () t))
       (incf *failures*)
       (format t "~&FAIL: expected an error from ~s~%" ',form))))

;;; ===========================================================================
;;; Part 1: the layer

;; Shaped like the source's defflavors: bare ivars, no components, the three
;; options in bare form.
(defflavor ft-node
           (activation
            name
            type
            neighbors)
           ()
; a comment between the components and the options, as in the source
           :settable-instance-variables
           :inittable-instance-variables
           :gettable-instance-variables)

;; Generated messages and keyword init
(defvar *n* (make-instance 'ft-node :activation 10 :name 'n1 :type "2b"))
(check (send *n* :activation) 10)
(check (send *n* :name) 'n1)
(check (send *n* :type) "2b")
(check (send *n* :neighbors) nil)                ; uninitialized ivar = NIL
(check (send *n* :set-activation 42) 42)
(check (send *n* :activation) 42)
(check (progn (send *n* :set-neighbors '((a b))) (send *n* :neighbors)) '((a b)))
(check (typep *n* 'ft-node) t)
(check-error (make-instance 'ft-node :bogus 1))  ; not an inittable ivar
(check-error (send *n* :no-such-message))
(check-error (send nil :activation))
(check-error (send 'ft-node :activation))        ; symbols are not instances

;; Methods: ivars are free variables; SETQ writes the instance.
(defmethod (ft-node :add-activation) (act)
  (if (> act 0)
   then (setq activation (+ activation act))))
(check (send *n* :add-activation 8) 50)
(check (send *n* :activation) 50)
(check (send *n* :add-activation -1) nil)
(check (send *n* :activation) 50)

;; TYPE as an ivar (cyto-node, cyto-context have one; NUMBO shadows CL:TYPE)
(defmethod (ft-node :retype) (new)
  (setq type new)
  type)
(check (send *n* :retype "4bl") "4bl")
(check (send *n* :type) "4bl")

;; &optional with a default, as in pnode :link-length / :modify-threshold
(defmethod (ft-node :opt) (a &optional (k 0.5) (new nil))
  (list a k new name))
(check (send *n* :opt 1) '(1 0.5 nil n1))
(check (send *n* :opt 1 2 3) '(1 2 3 n1))

;; SELF, MY, and SEND to self (my = send self)
(defmethod (ft-node :double) ()
  (* 2 (my :activation)))
(check (send *n* :double) 100)
(defmethod (ft-node :via-self) ()
  (send self :set-name 'renamed)
  (my :name))
(check (send *n* :via-self) 'renamed)
(check (send *n* :name) 'renamed)

;; A method can shadow a generated message (pnode :print vs nothing; here :name)
(defmethod (ft-node :describe) () (list 'node name))
(check (send *n* :describe) '(node renamed))

;; A lexical binding of an ivar name shadows the ivar inside the method
(defmethod (ft-node :shadow) ()
  (let ((activation 1)) (setq activation 2) activation))
(check (send *n* :shadow) 2)
(check (send *n* :activation) 50)

;; Free variables that are not ivars stay global (cyto-def does this with
;; `status' and `type' in cytoplasm methods).
(defvar ft-global nil)
(defmethod (ft-node :set-global) () (setq ft-global activation))
(check (progn (send *n* :set-global) ft-global) 50)

;; (declare (special ...)) at the head of a method body (pnode :modify-threshold)
(defvar *ft-iteration* 7)
(defmethod (ft-node :decl) ()
  (declare (special *ft-iteration*))
  (list *ft-iteration* activation))
(check (send *n* :decl) '(7 50))

;; Instances are independent
(defvar *m* (make-instance 'ft-node :activation 1))
(check (progn (send *m* :add-activation 1) (list (send *m* :activation)
                                                 (send *n* :activation)))
       '(2 50))

;; Redefining a method replaces it
(defmethod (ft-node :double) () (* 3 (my :activation)))
(check (send *n* :double) 150)

;; Options: list form, settable implies gettable+inittable, init values,
;; components inherit ivars and methods, unknown options are errors.
(defflavor ft-partial ((a 1) b c) ()
  (:gettable-instance-variables a)
  (:settable-instance-variables b))
(defvar *p* (make-instance 'ft-partial :b 2))
(check (send *p* :a) 1)
(check (send *p* :b) 2)
(check (send *p* :set-b 3) 3)
(check-error (send *p* :c))
(check-error (send *p* :set-a 0))
(check-error (make-instance 'ft-partial :a 0))
(check-error (make-instance 'ft-partial :c 0))
(defflavor ft-sub (d) (ft-node) :settable-instance-variables)
(defmethod (ft-sub :both) () (list name d))
(defvar *s* (make-instance 'ft-sub :d 4))
(check (progn (send *s* :set-name 'sub) (send *s* :both)) '(sub 4))
(check (progn (send *s* :set-activation 2) (send *s* :double)) 6) ; inherited method
(check-error (macroexpand-1 '(defflavor ft-bad (x) () :no-such-option)))
(check-error (macroexpand-1 '(defmethod (not-a-flavor :m) () 1)))

;; :init message is sent by make-instance, with the init plist
(defflavor ft-init (x log) () :settable-instance-variables)
(defmethod (ft-init :init) (plist) (setq log plist))
(check (send (make-instance 'ft-init :x 1) :log) '(:x 1))

;; COMPILE-FLAVOR-METHODS is a no-op (it appears inside a defun in init.l)
(check (compile-flavor-methods ft-node) nil)
(check (funcall (lambda () (compile-flavor-methods ft-node) :ok)) :ok)

;;; ===========================================================================
;;; Part 2: the real flavors

(unless (cl-user::numbo-load '("pnet-def" "pnet-functions" "pnet-graphics"
                               "cyto-def"))
  (format t "~&FAIL: pnet-def / pnet-functions / pnet-graphics / cyto-def did not load: ~s~%"
          cl-user::*numbo-load-failures*)
  (sb-ext:exit :code 1))

;; Globals that init.l sets; enough for the methods exercised here.
(setq %min-activation-to-be-added% 1 %max-activation-to-be-transmitted% 100
      %first-decay-rate% 0.5 %second-decay-rate% 0.5 %third-decay-rate% 0.7
      %fourth-decay-rate% 0.9 %fifth-decay-rate% 0.9 %sixth-decay-rate% 0.0)

;; pnet-def.l ran (init-pnet) at load time and built *pnet* from pnodes.
(check (length *pnet*) 88)
(check (every (lambda (p) (typep p 'pnode)) *pnet*) t)
(check (send node-1 :value) 1)
(check (send node-1 :name) 'one)
(check (send node-1 :short-name) "1")
(check (car (send node-1 :neighbors)) '(plus1-1 result+))
(check (send node-1 :activation) nil)            ; not initialized yet

;; pnet-functions methods on a real pnode
(send node-1 :set-activation 50)
(send node-1 :set-temp-activation-holder 0)
(send node-1 :set-spreadable-activation 0)
(check (send node-1 :add-activation 20) 70)      ; method SETQs ivar
(check (send node-1 :activation) 70)
(check (send node-1 :subtract-activation 30) 40)
(check (send node-1 :add-temp-activation-holder 5) 5)  ; via (my ...) + send self
(check (send node-1 :activation-decay-factor) 0.9)     ; no instances -> 4th rate
(send node-1 :update-instances '("2b" b1))
(check (send node-1 :instances) '(("2b" b1)))
(check (send node-1 :activation-decay-factor) 0.5)
(check (send node-1 :activation-decay) 20.0)
(send node-1 :suppress-instances 'b1)
(check (send node-1 :instances) nil)
(send node-1 :update-activation)                 ; 40 - 0 + 5
(check (list (send node-1 :activation) (send node-1 :temp-activation-holder))
       '(45 0.0))

;; cyto-def: init-cytoplasm and the cytoplasm methods
(init-cytoplasm 31 3 5 24 3 14)
(check (send *cytoplasm* :target) 31)
(check (send *cytoplasm* :brick4) 3)
(check (eq (send *cytoplasm* :context) *context*) t)
(check (typep *current-target* 'cyto-current-target) t)
(setq ct1 (make-instance 'cyto-node :name 'ct1 :type "1t" :status "free"
                                    :level 99 :value 31)
      cb1 (make-instance 'cyto-node :name 'cb1 :type "2b" :status "free"
                                    :level 1 :value 3)
      cb2 (make-instance 'cyto-node :name 'cb2 :type "2b" :status "linked"
                                    :level 1 :value 5))
(send *cytoplasm* :set-nodes (list ct1 cb1 cb2))
(check (send *cytoplasm* :free-blocks) (list cb1))
(check (send *cytoplasm* :free-cyto-nodes) (list cb1 ct1))
(check (send *cytoplasm* :cyto-brick-block-nodes) (list cb2 cb1))
(check (send *cytoplasm* :find-new-target) ct1)
(send ct1 :update-neighbors (list (list cb1 'operand)))
(check (send ct1 :neighbors) (list (list cb1 'operand)))
(check (send ct1 :lower-neighbor) cb1)

;;; ===========================================================================
;;; Part 3: census of SENT messages

(defparameter *source-files*
  '("pnet-def" "pnet-functions" "pnet-graphics" "cyto-def" "codelets"
    "init" "start"))

(defun collect-messages (form acc)
  "Push every keyword in message position of (send x :msg ...) / (my :msg ...)."
  (when (consp form)
    (when (and (eq (car form) 'send) (consp (cdr form)) (consp (cddr form))
               (keywordp (caddr form)))
      (pushnew (caddr form) (car acc)))
    (when (and (eq (car form) 'my) (consp (cdr form)) (keywordp (cadr form)))
      (pushnew (cadr form) (car acc)))
    (loop for x on form
          do (collect-messages (car x) acc)
          while (consp (cdr x)))))

(defvar *sent* (list nil))
(let ((*package* (find-package :numbo)) (*read-eval* nil))
  (dolist (f *source-files*)
    (with-open-file (in (merge-pathnames (format nil "../src/~a.lisp" f)
                                         *load-truename*))
      (loop for form = (read in nil in) until (eq form in)
            do (collect-messages form *sent*)))))

(defun handled-by-some-flavor-p (message)
  (loop for fl in '(pnode cytoplasm cyto-node cyto-current-target cyto-context)
        thereis (gethash message (flavor-method-table fl))))

(check (> (length (car *sent*)) 40) t)
(let ((unhandled (sort (remove-if #'handled-by-some-flavor-p (car *sent*))
                       #'string< :key #'symbol-name)))
  (check unhandled nil))

;;; ===========================================================================

(format t "~&flavors-compat: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))

;;; flavors-compat.lisp -- the subset of Flavors used by the 1987 source, on CLOS.
;;;
;;; The source (pnet-def, pnet-functions, cyto-def, codelets, init) uses:
;;;
;;;   (defflavor name (ivar...) (component...) option...)
;;;   (defmethod (flavor :message) lambda-list body...)
;;;   (send object :message arg...)
;;;   (my :message arg...)                 ; = (send self :message arg...)
;;;   (make-instance 'flavor :ivar value ...)
;;;   (compile-flavor-methods flavor)
;;;
;;; Census of every DEFFLAVOR form (pnode, cytoplasm, cyto-node,
;;; cyto-current-target, cyto-context): plain-symbol ivars, no init values,
;;; no component flavors, and exactly the three options
;;; :settable-instance-variables :inittable-instance-variables
;;; :gettable-instance-variables, all in bare (= all ivars) form.
;;; Census of every DEFMETHOD form (pnet-functions, cyto-def): all are
;;; (defmethod (flavor :message) lambda-list ...) with primary methods only
;;; (no :before/:after/whoppers); lambda lists use only &optional.  See
;;; PORTING_NOTES.md ("Flavors on CLOS").
;;;
;;; Implementation:
;;;   - a flavor is a CLOS standard class; each ivar is a slot (initform NIL)
;;;     with an :initarg keyword when it is inittable.
;;;   - messages live in a per-flavor hash table (message keyword -> function
;;;     of SELF and the method's lambda list).  SEND walks the class precedence
;;;     list, so component flavors would work.
;;;   - in a method body, every ivar of the flavor (and its components) is a
;;;     SYMBOL-MACROLET over (SLOT-VALUE SELF 'ivar), so free references and
;;;     SETQs of ivars read and write the instance, as in Flavors.
;;;   - :gettable-instance-variables generates :ivar messages,
;;;     :settable-instance-variables generates :set-ivar messages (and, as in
;;;     Flavors, implies gettable and inittable).
;;;
;;; Uninitialized ivars are NIL rather than unbound.  Franz Flavors allocates
;;; instances as vectors filled with nil, and the source relies on it
;;; (e.g. (send node :instances) on a fresh cyto-node).

(in-package :numbo)

;;; ---------------------------------------------------------------------------
;;; Flavor records (on the flavor name's plist)

(cl:defun flavor-ivars (flavor)
  "All instance variables of FLAVOR, its own and its components'."
  (get flavor 'flavor-ivars))

(cl:defun flavor-method-table (flavor)
  (or (get flavor 'flavor-methods)
      (setf (get flavor 'flavor-methods) (make-hash-table :test 'eq))))

(cl:defun flavor-p (name)
  (and (symbolp name) (get name 'flavor-ivars-defined)))

(cl:defun message-keyword (&rest parts)
  (cl:intern (format nil "~{~a~}" parts) "KEYWORD"))

;;; ---------------------------------------------------------------------------
;;; DEFFLAVOR

(cl:defun parse-flavor-options (options ivars)
  "Return (values gettable settable inittable), each a list of ivar names."
  (let (gettable settable inittable)
    (dolist (opt options)
      (let* ((key (if (consp opt) (car opt) opt))
             (vars (if (consp opt) (cdr opt) ivars)))
        (case key
          (:gettable-instance-variables (setf gettable (union gettable vars)))
          (:settable-instance-variables (setf settable (union settable vars)))
          ((:inittable-instance-variables :initable-instance-variables)
           (setf inittable (union inittable vars)))
          (t (error "DEFFLAVOR: unsupported option ~s" opt)))))
    ;; Settable implies gettable and inittable (Flavors manual).
    (values (union gettable settable)
            settable
            (union inittable settable))))

(defmacro defflavor (name ivar-specs components &rest options)
  (let* ((ivars (mapcar (lambda (s) (if (consp s) (car s) s)) ivar-specs))
         (inits (mapcar (lambda (s) (if (consp s) (cadr s) nil)) ivar-specs)))
    (multiple-value-bind (gettable settable inittable)
        (parse-flavor-options options ivars)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (get ',name 'flavor-ivars-defined) t
                 (get ',name 'flavor-ivars)
                 (remove-duplicates
                  (append ',ivars
                          (loop for c in ',components append (flavor-ivars c)))
                  :from-end t)))
         (defclass ,name ,components
           ,(loop for v in ivars
                  for init in inits
                  collect `(,v :initform ,init
                               ,@(when (cl:member v inittable)
                                   `(:initarg ,(message-keyword v))))))
         ,@(loop for v in gettable
                 collect `(setf (gethash ,(message-keyword v)
                                         (flavor-method-table ',name))
                                (lambda (self) (slot-value self ',v))))
         ,@(loop for v in settable
                 collect `(setf (gethash ,(message-keyword "SET-" v)
                                         (flavor-method-table ',name))
                                (lambda (self value)
                                  (setf (slot-value self ',v) value))))
         ',name))))

;;; ---------------------------------------------------------------------------
;;; DEFMETHOD

(defmacro defmethod (spec &rest rest)
  "(defmethod (flavor :message) lambda-list body...).  A symbol SPEC is a
CL DEFMETHOD (not used by the source)."
  (if (symbolp spec)
      `(cl:defmethod ,spec ,@rest)
      (destructuring-bind (flavor message) spec
        (destructuring-bind (lambda-list &rest body) rest
          (let ((ivars (flavor-ivars flavor)))
            (unless (flavor-p flavor)
              (error "DEFMETHOD: ~s is not a flavor" flavor))
            `(progn
               (setf (gethash ,message (flavor-method-table ',flavor))
                     (lambda (self ,@lambda-list)
                       (declare (ignorable self))
                       (symbol-macrolet
                           ,(loop for v in ivars
                                  collect `(,v (slot-value self ',v)))
                         ,@body)))
               '(,flavor ,message)))))))

;;; ---------------------------------------------------------------------------
;;; SEND, MY

(cl:defun find-flavor-method (object message)
  (let ((class (class-of object)))
    (dolist (c (sb-mop:class-precedence-list class) nil)
      (let ((table (get (class-name c) 'flavor-methods)))
        (when table
          (let ((fn (gethash message table)))
            (when fn (return fn))))))))

(cl:defun send (object message &rest args)
  (let ((fn (find-flavor-method object message)))
    (if fn
        (apply fn object args)
        (error "SEND: ~s does not handle the message ~s" object message))))

;; RECONSTRUCTED: `my' is called 16 times in pnet-functions.l, always as
;; (my :message) inside a method, but no source file defines it.  The calls
;; are both to gettable ivars, (my :activation), and to methods,
;; (my :activation-decay-factor), so it is read as (send self :message).
(defmacro my (message &rest args)
  `(send self ,message ,@args))

;;; ---------------------------------------------------------------------------
;;; MAKE-INSTANCE, COMPILE-FLAVOR-METHODS

(cl:defun make-instance (flavor &rest init-plist)
  "Flavors MAKE-INSTANCE: keyword init for inittable ivars, then the :init
message if the flavor handles it.  Non-flavor classes go to CL."
  (let ((object (apply #'cl:make-instance flavor init-plist)))
    (when (and (flavor-p flavor) (find-flavor-method object :init))
      (send object :init init-plist))
    object))

(defmacro compile-flavor-methods (&rest flavors)
  "No-op: methods are compiled when defined."
  (declare (ignore flavors))
  nil)

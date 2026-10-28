;;; flavors-compat.lisp -- the subset of Lucid Flavors that Copycat uses, on CLOS.
;;;
;;; Mitchell's code uses:
;;;
;;;   (defflavor name (ivar | (ivar init) ...) (component...) option...)
;;;       options seen: :gettable-instance-variables,
;;;       :settable-instance-variables, :initable-instance-variables, always
;;;       in the bare form (all instance variables).  Lucid let the
;;;       component list be left out when there are none (file-system.l
;;;       does: the first option follows the ivars directly).
;;;   (defmethod (flavor :message) lambda-list body...)
;;;       primary methods only; no :before/:after, no whoppers, no :init
;;;       methods.  Lambda lists use &optional, &rest and &aux.  Instance
;;;       variables are referenced free in the body and assigned with SETQ;
;;;       SELF is the instance.
;;;   (send object :message arg...)
;;;   (make-instance 'flavor :ivar value ...)
;;;   (typep object 'flavor), including component flavors
;;;   (flavors::flavor-type-of object)
;;;
;;; That is the same shape as the Numbo port's layer
;;; (Software/Numbo/lisp/src/flavors-compat.lisp), and this file follows it:
;;;
;;;   - a flavor is a CLOS standard class; its components are superclasses
;;;     (group has two: workspace-object and workspace-structure); each ivar
;;;     is a slot with an :initarg keyword, so CL:MAKE-INSTANCE serves as is.
;;;   - methods live in a per-flavor hash table (message -> function of SELF
;;;     and the lambda list).  SEND looks the message up along the class
;;;     precedence list, so component flavors' methods are inherited; the
;;;     lookup is cached per (class, message).
;;;   - inside a method body each ivar of the flavor and its components is a
;;;     SYMBOL-MACROLET over (SLOT-VALUE SELF 'ivar), except ivars whose name
;;;     is also a parameter of the method (the parameter wins, as in Flavors).
;;;   - :gettable-instance-variables makes :ivar messages,
;;;     :settable-instance-variables makes :set-ivar messages and, as in
;;;     Flavors, implies gettable and initable.
;;;
;;; Unset ivars are NIL rather than unbound, which is what the code expects
;;; (it tests ivars such as `group' and `graphics-obj' with IF before they
;;; are set).

(cl:in-package :copycat)

;;; ---------------------------------------------------------------------------
;;; Flavor records, on the flavor name's plist

(defun flavor-ivars (flavor)
  "All instance variables of FLAVOR, its own and its components'."
  (get flavor 'flavor-ivars))

(defun flavor-p (name)
  (and (symbolp name) (get name 'flavor-defined)))

(defun flavor-method-table (flavor)
  (or (get flavor 'flavor-methods)
      (setf (get flavor 'flavor-methods) (make-hash-table :test 'eq))))

(defun message-keyword (&rest parts)
  (values (intern (format nil "~{~a~}" parts) :keyword)))

;;; ---------------------------------------------------------------------------
;;; DEFFLAVOR

(defun parse-flavor-options (options ivars)
  "Return (values gettable settable initable), each a list of ivar names."
  (let (gettable settable initable)
    (dolist (opt options)
      (let* ((key (if (consp opt) (car opt) opt))
             (vars (if (consp opt) (cdr opt) ivars)))
        (case key
          (:gettable-instance-variables (setf gettable (union gettable vars)))
          (:settable-instance-variables (setf settable (union settable vars)))
          ((:initable-instance-variables :inittable-instance-variables)
           (setf initable (union initable vars)))
          (t (error "DEFFLAVOR: option ~s is not supported" opt)))))
    (values (union gettable settable)
            settable
            (union initable settable))))

(defmacro defflavor (name ivar-specs &rest components-and-options)
  (let* ((components (if (listp (car components-and-options))
                         (pop components-and-options)
                         nil))
         (options components-and-options)
         (ivars (mapcar (lambda (s) (if (consp s) (car s) s)) ivar-specs))
         (inits (mapcar (lambda (s) (if (consp s) (cadr s) nil)) ivar-specs)))
    (multiple-value-bind (gettable settable initable)
        (parse-flavor-options options ivars)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (get ',name 'flavor-defined) t
                 (get ',name 'flavor-ivars)
                 (remove-duplicates
                  (append ',ivars
                          (cl:loop for c in ',components
                                   append (flavor-ivars c)))
                  :from-end t)))
         (defclass ,name ,components
           ,(cl:loop for v in ivars
                     for init in inits
                     collect `(,v :initform ,init
                                  ,@(when (member v initable)
                                      `(:initarg ,(message-keyword v))))))
         ,@(cl:loop for v in gettable
                    collect `(define-flavor-method ',name ,(message-keyword v)
                               (lambda (self) (slot-value self ',v))))
         ,@(cl:loop for v in settable
                    collect `(define-flavor-method ',name
                                 ,(message-keyword "SET-" v)
                               (lambda (self value)
                                 (setf (slot-value self ',v) value))))
         ',name))))

;;; ---------------------------------------------------------------------------
;;; DEFMETHOD

(defun lambda-list-variables (lambda-list)
  "The variables a lambda list binds: required, &optional, &rest, &key, &aux."
  (cl:loop for item in lambda-list
           unless (member item lambda-list-keywords)
             collect (cond ((symbolp item) item)
                           ((consp (car item)) (cadar item)) ; &key ((:k var) d)
                           (t (car item)))))

(defmacro defmethod (spec &rest rest)
  "(defmethod (flavor :message) lambda-list body...).  A symbol SPEC is a
CL:DEFMETHOD (not used by Mitchell's code)."
  (if (symbolp spec)
      `(cl:defmethod ,spec ,@rest)
      (destructuring-bind (flavor message) spec
        (destructuring-bind (lambda-list &rest body) rest
          (unless (flavor-p flavor)
            (error "DEFMETHOD (~s ~s): ~s is not a defined flavor"
                   flavor message flavor))
          (let ((ivars (set-difference (flavor-ivars flavor)
                                       (lambda-list-variables lambda-list))))
            `(define-flavor-method ',flavor ,message
               (lambda (self ,@lambda-list)
                 (declare (ignorable self))
                 (symbol-macrolet
                     ,(cl:loop for v in ivars
                               collect `(,v (slot-value self ',v)))
                   ,@body))))))))

;;; ---------------------------------------------------------------------------
;;; Method tables and SEND

(defvar *send-cache* (make-hash-table :test 'eq)
  "class -> (message -> function), filled by SEND, cleared on definition.")

(defun define-flavor-method (flavor message function)
  (setf (gethash message (flavor-method-table flavor)) function)
  (clrhash *send-cache*)
  (list flavor message))

(defun find-flavor-method (class message)
  (dolist (c (sb-mop:class-precedence-list class) nil)
    (let ((table (get (class-name c) 'flavor-methods)))
      (when table
        (let ((fn (gethash message table)))
          (when fn (return fn)))))))

(defun send (object message &rest args)
  "Flavors SEND: run OBJECT's method for MESSAGE on ARGS."
  (let* ((class (class-of object))
         (cache (or (gethash class *send-cache*)
                    (setf (gethash class *send-cache*)
                          (make-hash-table :test 'eq))))
         (fn (or (gethash message cache)
                 (setf (gethash message cache)
                       (find-flavor-method class message)))))
    (if fn
        (apply fn object args)
        (error "SEND: ~s does not handle the message ~s" object message))))

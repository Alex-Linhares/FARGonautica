;;; world-state.lisp -- the World-state encoding of the codelet fixtures.
;;;
;;; Not a capture script (../python/scripts/regen_fixtures.sh runs only
;;; tests/oracle/*.lisp).  A codelet capture script loads it after common.lisp:
;;;   (load (merge-pathnames "lib/world-state.lisp" *load-truename*))
;;; ../python/tests/world_state.py is its Python twin (same numbering, so a
;;; correct Python World encodes to the oracle's text).
;;;
;;; A World state extends tests/oracle/cyto-def.lisp's cytoplasm state with
;;; the Pnet, the coderack and the RNG:
;;;   {"globals":        the symbols of +WS-GLOBALS+ (":UNBOUND" if unbound),
;;;    "cytoplasm":      *cytoplasm*'s 9 ivars,
;;;    "current-target": *current-target*'s 2, "context": *context*'s 5,
;;;    "pnodes":         {holder: [activation, spreadable-activation,
;;;                       temp-activation-holder, instances]} for the 91
;;;                       holders, in init-pnet order,
;;;    "coderack":       the bins of the rack *coderack* names, each
;;;                       [urgency, form, ...] (newest form first),
;;;    "rng":            *oracle-rng-state*,
;;;    "nodes":          every cyto-node reached, in the order first reached,
;;;                       each with its 10 ivars and "symbol-value" (the value
;;;                       of its name: the registry).}
;;; Values use the trace's Lisp-data encoding (src/oracle.lisp), except that a
;;; cyto-node is {"node": i} (its index in "nodes"), a pnode is
;;; {"pnode": holder}, and the other cyto flavors are {"obj": flavor,
;;; "global": true if eq to *cytoplasm* / *current-target* / *context*}.
;;; Nodes are numbered as they are first written, in the order of the JSON
;;; text above.
;;;
;;; This file is READ with SBCL's default float format (single); it has no
;;; float literals.

(in-package :numbo)

(cl:defparameter *ws-holders*
  (let ((*package* (cl:find-package :numbo))
        (*read-default-float-format* 'double-float))
    (with-open-file (in (merge-pathnames "src/pnet-def.lisp" cl-user::*oracle-repo*))
      (cl:loop for form = (read in nil in)
               until (eq form in)
               when (and (consp form) (eq (car form) 'defun) (eq (cadr form) 'init-pnet))
                 return (cl:mapcar #'cadr (cdddr form)))))
  "The 91 holders init-pnet SETQs, in its order.")

(assert (= (length *ws-holders*) 91))

(cl:defparameter +ws-ivars+
  '((cytoplasm target brick1 brick2 brick3 brick4 brick5 nodes current-target context)
    (cyto-node activation neighbors name type value success status level listed plinks)
    (cyto-current-target interest name)
    (cyto-context type name value interest location)))

(dolist (entry +ws-ivars+)
  (assert (equal (flavor-ivars (car entry)) (cdr entry))))

(cl:defun ws-ivars (flavor) (cdr (assoc flavor +ws-ivars+)))

(cl:defparameter +ws-globals+
  '(*iteration* *temperature* *problem-solved* *name-counter* *coderack* cyto-target
    ;; free SETQs: cyto-def's cytoplasm and cyto-node methods (type status lv),
    ;; pnet-functions (node res), and the codelets (PORTING_NOTES "Compile
    ;; census"): link-to-pnet and read-brick (activation), read-brick (a brick
    ;; bricki cyto-bricki), read-target (target), round (div),
    ;; collect-misfortune (current-target), repump (%resultx% %result+% %operand%);
    ;; sim (diff diffrel), eliminate (diff min), look-for-new-block (liste),
    ;; look-for-diff (similarity), find-node (address), look-for-blx
    ;; (current-target values-to-find node1 node2 cyto-block1 cyto-block2);
    ;; create-op-node (n1 n2 n3), decomp+ (oper), replace-target and
    ;; propagate-success (nn), replace-target (pp), propagate-success (cont n),
    ;; decrease-interest (new), check-temperature (reserve weights)
    type status lv node res activation a brick bricki cyto-bricki target div
    current-target %resultx% %result+% %operand%
    diff diffrel min liste similarity address values-to-find node1 node2
    cyto-block1 cyto-block2
    n1 n2 n3 oper nn pp cont n new reserve weights)
  "The globals every state records, in this order (keys: lower-case names).")

(cl:defun ws-name (sym) (string-downcase (symbol-name sym)))

;;; ---------------------------------------------------------------------------
;;; Encoding

(cl:defvar *ws-ids* nil "eq hash table: cyto-node -> index")
(cl:defvar *ws-queue* nil "the nodes in index order")

(cl:defun ws-new-numbering ()
  (setq *ws-ids* (make-hash-table :test 'eq)
        *ws-queue* (make-array 0 :adjustable t :fill-pointer t)))

(cl:defun ws-ref (node)
  (or (gethash node *ws-ids*)
      (prog1 (setf (gethash node *ws-ids*) (length *ws-queue*))
        (vector-push-extend node *ws-queue*))))

(cl:defun ws-holder-of (pnode)
  (or (cl:find-if (cl:lambda (h) (eq (symbol-value h) pnode)) *ws-holders*)
      (error "no holder for ~s" pnode)))

(cl:defun ws-global-of (x)
  (cl:loop for g in '(*cytoplasm* *current-target* *context*)
           thereis (and (boundp g) (eq (symbol-value g) x))))

(cl:defun ws-write (x s)
  (cond ((typep x 'cyto-node) (format s "{\"node\":~d}" (ws-ref x)))
        ((typep x 'pnode)
         (write-string "{\"pnode\":" s)
         (oracle-write-json-string (symbol-name (ws-holder-of x)) s)
         (write-char #\} s))
        ((typep x 'standard-object)
         (format s "{\"obj\":\"~(~a~)\",\"global\":~:[null~;true~]}"
                 (class-name (class-of x)) (ws-global-of x)))
        ((and (consp x) (oracle-proper-list-p x))
         (write-char #\[ s)
         (cl:loop for (item . more) on x
                  do (ws-write item s) (when more (write-char #\, s)))
         (write-char #\] s))
        ((consp x)
         (write-string "{\"cons\":[" s) (ws-write (car x) s) (write-char #\, s)
         (ws-write (cdr x) s) (write-string "]}" s))
        (t (oracle-write-data x s))))

(cl:defun ws-json (x) (with-output-to-string (s) (ws-write x s)))

(cl:defun ws-key (x) (with-output-to-string (s) (oracle-write-json-string x s)))

(cl:defun ws-object (pairs)
  "PAIRS ((key . json-text) ...) as a JSON object."
  (format nil "{~{~a~^,~}}"
          (cl:loop for (k . v) in pairs collect (format nil "~a:~a" (ws-key k) v))))

(cl:defun ws-array (texts) (format nil "[~{~a~^,~%~}]" texts))

(cl:defun ws-value (sym)
  (cl:if (boundp sym) (ws-json (symbol-value sym)) (ws-json :unbound)))

(cl:defun ws-instance (object flavor)
  (cl:if (typep object flavor)
         (ws-object (cl:loop for v in (ws-ivars flavor)
                             collect (cons (ws-name v) (ws-json (slot-value object v)))))
         (ws-json :unbound)))

(cl:defun ws-global-instance (sym flavor)
  (cl:if (boundp sym) (ws-instance (symbol-value sym) flavor) (ws-json :unbound)))

(cl:defun ws-rack ()
  (let ((rack (and (boundp '*coderack*)
                   (symbolp *coderack*)
                   (get *coderack* 'coderack))))
    (cl:if rack (ws-json (coderack-bins rack)) (ws-json :unbound))))

(cl:defun ws-pnodes ()
  (ws-object (cl:loop for h in *ws-holders*
                      collect (let ((p (symbol-value h)))
                                (cons (symbol-name h)
                                      (ws-json (list (send p :activation)
                                                     (send p :spreadable-activation)
                                                     (send p :temp-activation-holder)
                                                     (send p :instances))))))))

(cl:defun ws-nodes-from (start)
  "The JSON texts of the nodes numbered START and up (each with its ivars and
the value of its name), including the ones they reach in turn."
  (cl:loop for i from start
           while (< i (length *ws-queue*))
           collect (let* ((node (aref *ws-queue* i))
                          (ivars (ws-instance node 'cyto-node))
                          (name (slot-value node 'name)))
                     (format nil "~a,\"symbol-value\":~a}"
                             (subseq ivars 0 (1- (length ivars)))
                             (cl:if (and name (symbolp name))
                                    (ws-value name)
                                    "null")))))

(cl:defun ws-state ()
  "The World state, with a new numbering (left in *ws-ids* for the calls
after it).  Evaluated strictly in JSON text order."
  (ws-new-numbering)
  (let* ((globals (ws-object (cl:loop for sym in +ws-globals+
                                      collect (cons (ws-name sym) (ws-value sym)))))
         (cytoplasm (ws-global-instance '*cytoplasm* 'cytoplasm))
         (current-target (ws-global-instance '*current-target* 'cyto-current-target))
         (context (ws-global-instance '*context* 'cyto-context))
         (pnodes (ws-pnodes))
         (rack (ws-rack))
         (rng (ws-json *oracle-rng-state*))
         (nodes (ws-nodes-from 0)))
    (ws-object (list (cons "globals" globals)
                     (cons "cytoplasm" cytoplasm)
                     (cons "current-target" current-target)
                     (cons "context" context)
                     (cons "pnodes" pnodes)
                     (cons "coderack" rack)
                     (cons "rng" rng)
                     (cons "nodes" (ws-array nodes))))))

;;; ---------------------------------------------------------------------------
;;; Which globals a call changes

(cl:defun ws-infrastructure-p (sym)
  "Globals of the oracle hooks, the harness and the capture scripts."
  (let ((name (symbol-name sym)))
    (or (cl:some (cl:lambda (prefix) (and (>= (length name) (length prefix))
                                          (string= prefix name :end2 (length prefix))))
                 '("*ORACLE" "*WS-" "*CA-" "*CB-" "*CC-" "+WS-" "+CA-"))
        (member sym '(*iteration-cap*)))))

(cl:defun ws-global-snapshot ()
  (cl:loop for s in (oracle-numbo-symbols)
           unless (or (ws-infrastructure-p s) (constantp s))
             collect (cl:if (boundp s) (list s t (symbol-value s)) (list s nil))))

(cl:defun ws-changed (snapshot)
  "The NUMBO globals whose value is no longer eq to the one in SNAPSHOT (or
that became bound or unbound), sorted by name."
  (let ((before (make-hash-table :test 'eq)) changed)
    (dolist (entry snapshot) (setf (gethash (car entry) before) (cdr entry)))
    (dolist (s (oracle-numbo-symbols))
      (unless (or (ws-infrastructure-p s) (constantp s))
        (let ((old (gethash s before '(nil))))
          (unless (cl:if (car old)
                         (and (boundp s) (eq (symbol-value s) (cadr old)))
                         (not (boundp s)))
            (push s changed)))))
    (sort changed #'string< :key #'symbol-name)))

(cl:defun ws-changed-json (syms)
  "The changed globals and their values (in the current numbering)."
  (ws-object (cl:loop for s in syms collect (cons (symbol-name s) (ws-value s)))))

(cl:defun ws-parameters ()
  "Every bound %...% global, as an object keyed by upper-case name."
  (let (syms)
    (dolist (s (oracle-numbo-symbols))
      (let ((name (symbol-name s)))
        (when (and (boundp s) (> (length name) 1)
                   (char= (char name 0) #\%) (char= (char name (1- (length name))) #\%))
          (push s syms))))
    (ws-changed-json (sort syms #'string< :key #'symbol-name))))

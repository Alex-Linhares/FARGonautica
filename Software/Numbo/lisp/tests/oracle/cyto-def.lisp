;;; cyto-def.lisp -- write ../python/fixtures/cyto_def.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/cyto-def.lisp
;;; (or ../python/scripts/regen_fixtures.sh).
;;;
;;; The cytoplasm's state around the 15 methods and 4 functions of
;;; src/cyto-def.lisp (../python/tests/test_cyto_def.py replays them on
;;; ../python/numbo/cyto_def.py):
;;;
;;;   init       the 11 chapter puzzles run in turn in one image (seed 1, each
;;;              capped), and for each, the state just before and just after
;;;              config's (init-cytoplasm t1 b1 b2 b3 b4 b5), with its arguments.
;;;              The state before is what the previous run left.
;;;   snapshots  the state each of those runs ends in, a few mid-run states
;;;              (seeds 2-4), and a made-up cytoplasm, each with
;;;              - readers: every reader method on the cytoplasm
;;;                (:cyto-brick-block-nodes, :find-new-target, :free-blocks,
;;;                :free-cyto-nodes, :free-secondary-cyto-nodes,
;;;                :secondary-cyto-nodes) and on every node (:lower-dtarget-
;;;                neighbor, :lower-neighbor, :block-neighbor,
;;;                :upper-neighbor), in sequence, each with its result (or an
;;;                error) and the free globals TYPE, STATUS and LV after it
;;;                (all three are set to :before before each call);
;;;              - ops (not for the mid-run states): a sequence of mutating sends and function calls
;;;                (:replace-neighbors, :suppress-neighbors,
;;;                :update-neighbors, :update-plinks, :suppress-node,
;;;                update-context, update-current-target, replace-function,
;;;                init-cytoplasm), each with its result and the full state
;;;                after it.
;;;
;;; A state is
;;;   {"globals":   *temperature*, *problem-solved*, cyto-target, and the free
;;;                 TYPE, STATUS, LV (cyto-def's cytoplasm methods SETQ type
;;;                 and status, which are not cytoplasm ivars; the cyto-node
;;;                 methods SETQ lv; PORTING_NOTES "Compile census"),
;;;    "cytoplasm": *cytoplasm*'s 9 ivars (null if unbound),
;;;    "current-target": *current-target*'s 2 ivars, "context": *context*'s 5,
;;;    "nodes":     every cyto-node reached, in the order first reached, each
;;;                 with its 10 ivars and "symbol-value", the value of its name
;;;                 (the registry: (eval name)).}
;;; Values use the trace's Lisp-data encoding (src/oracle.lisp), except that a
;;; cyto-node is {"node": i} (its index in "nodes"), a pnode is
;;; {"pnode": holder}, and the other cyto flavors are {"obj": flavor,
;;; "global": t if eq to *cytoplasm* / *current-target* / *context*}.
;;; An unbound variable is ":UNBOUND".  Nodes are numbered as they are first
;;; written, in the order of the JSON text: globals, cytoplasm, current-target,
;;; context, then each node's ivars in turn.  Results and op arguments are
;;; written with the numbering of the snapshot's state (extended in the same
;;; way if they reach new nodes).
;;;
;;; This file is READ with SBCL's default float format (single); it has no
;;; float literals.

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package :numbo)

(cl:defparameter *cd-puzzles*
  '((114 11 20 7 1 6) (87 8 3 9 10 7) (31 3 5 24 3 14) (25 8 5 5 11 2)
    (102 6 17 2 4 1) (146 12 2 5 7 18) (6 3 3 17 11 22) (11 2 5 1 25 23)
    (116 20 2 16 14 6) (127 6 4 22 5 7) (41 5 16 22 25 1)))

(cl:defparameter *cd-caps* '(25 60 90 130 170 220 280 350 450 600 800)
  "The iteration cap of each puzzle's run (solved runs stop earlier).")

(cl:defparameter *cd-holders*
  (let ((*package* (cl:find-package :numbo))
        (*read-default-float-format* 'double-float))
    (with-open-file (in (merge-pathnames "src/pnet-def.lisp" cl-user::*oracle-repo*))
      (cl:loop for form = (read in nil in)
               until (eq form in)
               when (and (consp form) (eq (car form) 'defun) (eq (cadr form) 'init-pnet))
                 return (cl:mapcar #'cadr (cdddr form)))))
  "The 91 holders init-pnet SETQs, in its order.")

(assert (= (length *cd-holders*) 91))

(cl:defparameter +cd-ivars+
  '((cytoplasm target brick1 brick2 brick3 brick4 brick5 nodes current-target context)
    (cyto-node activation neighbors name type value success status level listed plinks)
    (cyto-current-target interest name)
    (cyto-context type name value interest location)))

(cl:defun cd-ivars (flavor) (cdr (assoc flavor +cd-ivars+)))

;; The ivar lists above are the defflavor forms' (checked against the loaded
;; flavors, so a change to cyto-def.lisp can't go unnoticed).
(dolist (entry +cd-ivars+)
  (assert (equal (flavor-ivars (car entry)) (cdr entry))))

;;; ---------------------------------------------------------------------------
;;; Encoding

(cl:defvar *cd-ids* nil "eq hash table: cyto-node -> index")
(cl:defvar *cd-queue* nil "the nodes in index order")

(cl:defun cd-new-numbering ()
  (setq *cd-ids* (make-hash-table :test 'eq)
        *cd-queue* (make-array 0 :adjustable t :fill-pointer t)))

(cl:defun cd-ref (node)
  (or (gethash node *cd-ids*)
      (prog1 (setf (gethash node *cd-ids*) (length *cd-queue*))
        (vector-push-extend node *cd-queue*))))

(cl:defun cd-holder-of (pnode)
  (or (cl:find-if (cl:lambda (h) (eq (symbol-value h) pnode)) *cd-holders*)
      (error "no holder for ~s" pnode)))

(cl:defun cd-global-of (x)
  (cl:loop for g in '(*cytoplasm* *current-target* *context*)
           thereis (and (boundp g) (eq (symbol-value g) x))))

(cl:defun cd-write (x s)
  (cond ((typep x 'cyto-node) (format s "{\"node\":~d}" (cd-ref x)))
        ((typep x 'pnode)
         (write-string "{\"pnode\":" s)
         (oracle-write-json-string (symbol-name (cd-holder-of x)) s)
         (write-char #\} s))
        ((typep x 'standard-object)
         (format s "{\"obj\":\"~(~a~)\",\"global\":~:[null~;true~]}"
                 (class-name (class-of x)) (cd-global-of x)))
        ((and (consp x) (oracle-proper-list-p x))
         (write-char #\[ s)
         (cl:loop for (item . more) on x
                  do (cd-write item s) (when more (write-char #\, s)))
         (write-char #\] s))
        ((consp x)
         (write-string "{\"cons\":[" s) (cd-write (car x) s) (write-char #\, s)
         (cd-write (cdr x) s) (write-string "]}" s))
        (t (oracle-write-data x s))))

(cl:defun cd-json (x) (with-output-to-string (s) (cd-write x s)))

(cl:defun cd-key (x) (with-output-to-string (s) (oracle-write-json-string x s)))

(cl:defun cd-object (pairs)
  "PAIRS ((key . json-text) ...) as a JSON object."
  (format nil "{~{~a~^,~}}"
          (cl:loop for (k . v) in pairs collect (format nil "~a:~a" (cd-key k) v))))

(cl:defun cd-array (texts) (format nil "[~{~a~^,~%~}]" texts))

(cl:defun cd-value (sym)
  (cl:if (boundp sym) (cd-json (symbol-value sym)) (cd-json :unbound)))

(cl:defun cd-instance (object flavor)
  "OBJECT's ivars, as an object (null if OBJECT is not a FLAVOR instance)."
  (cl:if (typep object flavor)
         (cd-object (cl:loop for v in (cd-ivars flavor)
                             collect (cons (string-downcase (symbol-name v))
                                           (cd-json (slot-value object v)))))
         (cd-json :unbound)))

(cl:defun cd-global-instance (sym flavor)
  (cl:if (boundp sym) (cd-instance (symbol-value sym) flavor) (cd-json :unbound)))

(cl:defun cd-state ()
  "The state, with a new numbering (left in *cd-ids* for the calls after it).
Evaluated strictly in JSON text order, so nodes are numbered in that order."
  (cd-new-numbering)
  (let* ((globals (cd-object
                   (cl:loop for (key sym) in '(("*temperature*" *temperature*)
                                               ("*problem-solved*" *problem-solved*)
                                               ("cyto-target" cyto-target)
                                               ("type" type) ("status" status) ("lv" lv))
                            collect (cons key (cd-value sym)))))
         (cytoplasm (cd-global-instance '*cytoplasm* 'cytoplasm))
         (current-target (cd-global-instance '*current-target* 'cyto-current-target))
         (context (cd-global-instance '*context* 'cyto-context))
         (nodes (cl:loop for i from 0
                         while (< i (length *cd-queue*))
                         collect (let* ((node (aref *cd-queue* i))
                                        (ivars (cd-instance node 'cyto-node))
                                        (name (slot-value node 'name)))
                                   (format nil "~a,\"symbol-value\":~a}"
                                           (subseq ivars 0 (1- (length ivars)))
                                           (cl:if (and name (symbolp name))
                                                  (cd-value name)
                                                  "null"))))))
    (cd-object (list (cons "globals" globals)
                     (cons "cytoplasm" cytoplasm)
                     (cons "current-target" current-target)
                     (cons "context" context)
                     (cons "nodes" (cd-array nodes))))))

(cl:defun cd-free-globals ()
  (cd-object (cl:loop for (key sym) in '(("type" type) ("status" status) ("lv" lv))
                      collect (cons key (cd-value sym)))))

;;; ---------------------------------------------------------------------------
;;; Calls

(cl:defun cd-call (fn)
  "(values result-json error-p): FN's value, or an error.  The free globals
TYPE, STATUS and LV are set to :before first, so what a call leaves in them
is its own doing."
  (setq type :before status :before lv :before)
  (handler-case (values (cd-json (funcall fn)) nil)
    (error () (values "null" t))))

(cl:defun cd-reader-calls ()
  "Every reader on the cytoplasm, then on every node of the state (in the
state's numbering), each with its result and the free globals after it."
  (let ((nodes (coerce *cd-queue* 'list)))
    (append
     (cl:loop for msg in '(:cyto-brick-block-nodes :find-new-target :free-blocks
                           :free-cyto-nodes :free-secondary-cyto-nodes
                           :secondary-cyto-nodes)
              collect (multiple-value-bind (result error)
                          (cd-call (cl:lambda () (send *cytoplasm* msg)))
                        (cd-object (list (cons "recv" (cd-json *cytoplasm*))
                                         (cons "msg" (cd-json msg))
                                         (cons "result" result)
                                         (cons "error" (cd-json error))
                                         (cons "globals" (cd-free-globals))))))
     (cl:loop for node in nodes
              append (cl:loop for msg in '(:lower-dtarget-neighbor :lower-neighbor
                                           :block-neighbor :upper-neighbor)
                              collect (multiple-value-bind (result error)
                                          (cd-call (cl:lambda () (send node msg)))
                                        (cd-object (list (cons "recv" (cd-json node))
                                                         (cons "msg" (cd-json msg))
                                                         (cons "result" result)
                                                         (cons "error" (cd-json error))
                                                         (cons "globals" (cd-free-globals))))))))))

(cl:defun cd-ops ()
  "A script of mutating calls on the current state; each op records its
receiver (a node, the cytoplasm, or null for a function), message or
function name, arguments, result, and the full state after it.  Arguments
and results use the numbering of the state before the script."
  (let* ((ids *cd-ids*) (queue *cd-queue*)
         (ns (send *cytoplasm* :nodes))
         (a (or (cl:find-if (cl:lambda (n) (send n :neighbors)) ns) (car ns)))
         (b (car (last ns)))
         (c (or (cadr ns) (car ns)))
         (m (cl:reduce (cl:lambda (x y) (cl:if (> (length (send y :neighbors))
                                                  (length (send x :neighbors)))
                                               y x))
                       ns))
         (script
           (list (list a :replace-neighbors (car (car (send a :neighbors))) b)
                 (list a :replace-neighbors 'no-such-node b)
                 (list c :suppress-neighbors (or (car (car (send c :neighbors))) a))
                 (list m :suppress-neighbors 'no-such-node)
                 (list b :update-neighbors (list (list a 'operand) (list c 'result)))
                 (list b :update-neighbors nil)
                 (list b :update-plinks node-5)
                 (list a :update-plinks resultx)
                 (list *cytoplasm* :suppress-node c)
                 (list *cytoplasm* :suppress-node 'no-such-node)
                 (list nil 'update-context "2b" 'cyto-brick9 9 50 "cyto")
                 (list nil 'update-current-target b 77)
                 (list nil 'update-current-target 'cyto-target 100)
                 (list nil 'replace-function (list (list a 'operand) (list 'x 1 2) (list a)
                                                   (list b 'result))
                       a 'z)
                 (list nil 'replace-function nil a b)
                 (list nil 'init-cytoplasm 99 1 2 3 4 5)))
         records)
    (dolist (op script (nreverse records))
      (destructuring-bind (recv msg &rest args) op
        (let (recv-json args-json result error)
          (setq *cd-ids* ids *cd-queue* queue)
          (setq recv-json (cd-json recv)
                args-json (cd-json args))
          (multiple-value-setq (result error)
            (handler-case (values (funcall (cl:if recv
                                                  (cl:lambda () (apply #'send recv msg args))
                                                  (cl:lambda () (apply msg args))))
                                  nil)
              (error () (values nil t))))
          (setq result (cd-json result)
                ids *cd-ids* queue *cd-queue*)
          (push (cd-object (list (cons "recv" recv-json)
                                 (cons "msg" (cd-json msg))
                                 (cons "args" args-json)
                                 (cons "result" result)
                                 (cons "error" (cd-json error))
                                 (cons "state" (cd-state))))
                records))))))

;;; ---------------------------------------------------------------------------
;;; init-cytoplasm in real runs

(cl:defvar *cd-init-records* nil)
(cl:defvar *cd-recording* nil)

(cl:defun cd-init-hook (fn &rest args)
  (cl:if (not *cd-recording*)
         (apply fn args)
         (let ((before (cd-state)) result)
           (setq result (apply fn args))
           (push (list args before (cd-state) (cd-json result)) *cd-init-records*)
           result)))

(sb-int:encapsulate 'init-cytoplasm 'cyto-fixture #'cd-init-hook)

;;; A made-up cytoplasm for the cases the real runs don't reach: an error in
;;; :block-neighbor (a free "3dt" with no upper neighbor: (send nil
;;; :neighbors)) and in :upper-neighbor (no level), a non-"3dt" neighbor with
;;; no level (:lower-dtarget-neighbor's AND stops before the <), equal levels
;;; (the first one is kept), a "5g" :lower-neighbor (LV left alone), a
;;; symbol type, and several free targets (:find-new-target keeps the last).

(cl:defun cd-node (name &rest ivars)
  (setf (symbol-value name)
        (apply #'make-instance 'cyto-node :name name ivars)))

(cl:defun cd-made-up-cytoplasm ()
  (init-cytoplasm 50 1 2 3 4 5)
  (let* ((t1 (cd-node 'mu-target :type "1t" :status "free" :level 99 :value 50))
         (d1 (cd-node 'mu-dt1 :type "3dt" :status "free" :level 98 :value 40))
         (d2 (cd-node 'mu-dt2 :type "3dt" :status "free" :level 97 :value 30))
         (lone (cd-node 'mu-lone :type "3dt" :status "free" :level 96))
         (b1 (cd-node 'mu-brick1 :type "2b" :status "free" :level 1 :value 10))
         (b2 (cd-node 'mu-brick2 :type "2b" :status "linked" :level 1 :value 4))
         (bl (cd-node 'mu-block :type "4bl" :status "free" :level 2 :value 14))
         (nolevel (cd-node 'mu-nolevel :type "4bl" :status "free"))
         (odd (cd-node 'mu-odd :type 'weird :status 'free :level 5))
         (op1 (cd-node 'mu-op1 :type "5g" :level 99))
         (op2 (cd-node 'mu-op2 :type "5g" :level 98))
         (op3 (cd-node 'mu-op3 :type "5g" :level 2))
         (op4 (cd-node 'mu-op4 :type "5g" :level 98))
         (dt3 (cd-node 'mu-dt3 :type "3dt" :status "free" :level 98 :value 39))
         (dt4 (cd-node 'mu-dt4 :type "3dt" :status "free" :level 50 :value 20))
         (tie (cd-node 'mu-tie :type "4bl" :status "free" :level 3 :value 7)))
    (send t1 :set-neighbors (list (list op1 'result)))
    (send op1 :set-neighbors (list (list t1 'result) (list d1 'operand) (list bl 'operand)))
    (send d1 :set-neighbors (list (list op1 'operand) (list op2 'result)))
    (send op2 :set-neighbors (list (list d1 'result) (list d2 'operand) (list b1 'operand)
                                   (list b2 'operand)))
    (send d2 :set-neighbors (list (list op2 'operand) (list nolevel 'operand)))
    (send bl :set-neighbors (list (list op1 'operand) (list op3 'result)))
    (send op3 :set-neighbors (list (list bl 'result) (list b1 'operand) (list b2 'operand)))
    (send b1 :set-neighbors (list (list op3 'operand) (list op2 'operand)))
    (send nolevel :set-neighbors (list (list d2 'operand)))
    (send odd :set-neighbors (list (list b1 'operand) (list bl 'operand)))
    ;; ties: b1/b2 (level 1, :lower-neighbor), d1/dt3 (98, "3dt",
    ;; :lower-dtarget-neighbor), d1/dt3/op2/op4 (98, :upper-neighbor);
    ;; dt4's upper neighbor op2 has two "2b" neighbors (:block-neighbor)
    (send tie :set-neighbors (list (list b1 'operand) (list b2 'operand) (list d1 'result)
                                   (list dt3 'result) (list op2 'result) (list op4 'result)))
    (send dt4 :set-neighbors (list (list op2 'result)))
    (send *cytoplasm* :set-nodes (list op3 odd nolevel lone bl b2 b1 d2 op2 d1 op1 t1
                                       tie dt3 dt4 op4))
    (update-current-target d1 50)
    (update-context "3dt" 'mu-dt1 40 50 "cyto")))

(cl:defun cd-snapshot (problem seed cap ops)
  "Run PROBLEM (or make up a cytoplasm, PROBLEM nil) and record the state it
ends in, the reader calls, and (OPS true) the op script."
  (let ((result (cl:if problem
                       (let ((*standard-output* (make-broadcast-stream)))
                         (oracle-run-config problem :seed seed :max-iterations cap))
                       (progn (cd-made-up-cytoplasm) nil))))
    (assert (not (eq (getf result :outcome) :error)))
    (let* ((state (cd-state))
           (readers (cd-array (cd-reader-calls)))
           (ops (cl:if ops
                       (let ((*cd-recording* nil)) ; its init-cytoplasm isn't config's
                         (cd-state)             ; the numbering of STATE again
                         (cd-array (cd-ops)))
                       "[]")))
      (cd-object (list (cons "problem" (cd-json problem))
                       (cons "seed" (cd-json seed))
                       (cons "cap" (cd-json cap))
                       (cons "outcome" (cd-json (getf result :outcome)))
                       (cons "iterations" (cd-json (getf result :iterations)))
                       (cons "state" state)
                       (cons "readers" readers)
                       (cons "ops" ops))))))

(cl:defparameter *cd-output*
  (let (inits snapshots)
    ;; The 11 puzzles in turn, seed 1: init-cytoplasm, and the end state with
    ;; the readers and the op script (which the next run's init-cytoplasm
    ;; then replaces).
    (cl:loop for problem in *cd-puzzles*
             for cap in *cd-caps*
             do (setq *cd-init-records* nil)
                (push (let ((*cd-recording* t)) (cd-snapshot problem 1 cap t)) snapshots)
                (assert (= (length *cd-init-records*) 1))
                (destructuring-bind (args before after value) (car *cd-init-records*)
                  (push (cd-object (list (cons "problem" (cd-json problem))
                                         (cons "args" (cd-json args))
                                         (cons "before" before)
                                         (cons "after" after)
                                         (cons "result" value)))
                        inits)))
    ;; Mid-run states of the puzzles with the largest cytoplasms: readers only.
    (cl:loop for (problem seed cap) in '(((31 3 5 24 3 14) 2 40) ((146 12 2 5 7 18) 2 150)
                                         ((116 20 2 16 14 6) 3 60) ((127 6 4 22 5 7) 3 150)
                                         ((146 12 2 5 7 18) 4 400) ((127 6 4 22 5 7) 2 300))
             do (push (cd-snapshot problem seed cap nil) snapshots))
    (push (cd-snapshot nil nil nil t) snapshots)
    (format nil "{~%\"holders\": ~a,~%\"init\": ~a,~%\"snapshots\": ~a~%}~%"
            (cd-json *cd-holders*)
            (cd-array (reverse inits))
            (cd-array (reverse snapshots)))))

(cl-user::write-fixture "cyto_def.json" *cd-output*)

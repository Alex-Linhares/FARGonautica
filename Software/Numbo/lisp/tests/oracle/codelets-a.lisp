;;; codelets-a.lisp -- write ../python/fixtures/codelets_a.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/codelets-a.lisp
;;; (or ../python/scripts/regen_fixtures.sh).
;;;
;;; Cases for the codelets of loop0002 item 8 (../python/tests/test_codelets_a.py
;;; replays them on ../python/numbo/codelets.py): create-cyto-node, link-to-pnet,
;;; activate, kill-node, free-from-pnet, read-target and read-brick.
;;;
;;;   parameters  every %...% global after init-chiffre (config changes only
;;;               %resultx% %result+% %operand%, which each state records)
;;;   cases       calls of the 7 functions in real oracle runs (no trace):
;;;               - every call that is the first of its function in a run of
;;;                 the 11 chapter puzzles with seeds 1, 2 and 3 (capped at
;;;                 *ca-cap* iterations);
;;;               - in those runs, every call whose signature (see
;;;                 CA-SIGNATURE: the branch it will take) was not seen
;;;                 before;
;;;               - made-up calls on a real state, for branches the runs
;;;                 don't reach (a killed node in link-to-pnet, ...).
;;;               Each case is {"source", "codelet", "args", "before",
;;;               "arg-nodes", "result", "error", "output", "draws", "after",
;;;               "changed"}: the World state before the call
;;;               (lib/world-state.lisp), the arguments as the function
;;;               received them (a codelet run from the coderack gets its
;;;               form's arguments evaluated), the nodes the arguments reach
;;;               that the state doesn't (numbered after its nodes), its value
;;;               (or an error), what it printed, the number of 64-bit RNG
;;;               outputs it drew, the World state after it, and every NUMBO
;;;               global whose value it changed (by eq) with its new value.
;;;               args and result use the numbering of "before" (extended to
;;;               new nodes); "changed" that of "after".
;;;   helpers     round (including negative values, where truncating / and
;;;               *mod matter), ratio and activate's (expt 0.9 val) on a range
;;;               of values
;;;
;;; This file is READ with SBCL's default float format (single), so every
;;; float literal below is written with d0.

(load (merge-pathnames "common.lisp" *load-truename*))
(load (merge-pathnames "lib/world-state.lisp" *load-truename*))

(in-package :numbo)

(cl:defparameter *ca-puzzles*
  '((114 11 20 7 1 6) (87 8 3 9 10 7) (31 3 5 24 3 14) (25 8 5 5 11 2)
    (102 6 17 2 4 1) (146 12 2 5 7 18) (6 3 3 17 11 22) (11 2 5 1 25 23)
    (116 20 2 16 14 6) (127 6 4 22 5 7) (41 5 16 22 25 1)))

(cl:defparameter *ca-codelets*
  '(create-cyto-node link-to-pnet activate kill-node free-from-pnet read-target read-brick))

(cl:defparameter *ca-cap* 400 "The iteration cap of every run.")

;;; ---------------------------------------------------------------------------
;;; Signatures: the branch a call will take, computed without side effects

(cl:defun ca-quiet (thunk)
  (oracle-call-without-global-effects thunk))

(cl:defun ca-signature (codelet args)
  (ecase codelet
    (read-target nil)
    (read-brick
     (zerop (cl:rem (send *cytoplasm* (cl:intern (format nil "BRICK~d" (car args)) :keyword))
                    10)))
    (create-cyto-node (list (nth 5 args) (nth 3 args)))
    (link-to-pnet
     (destructuring-bind (name type value) args
       (list type
             (equal "killed" (send (eval name) :status))
             (ca-quiet (cl:lambda ()
                         (let ((address (concat 'node- value)))
                           (cond ((boundp address) :bound)
                                 ((= 0 value) :zero)
                                 ((< (round value) 200) :rounded)
                                 (t :big))))))))
    (activate
     (destructuring-bind (activation pnode repump) args
       (list (and repump t) (symbolp pnode)
             (> (abs activation) %min-activation-to-be-added%)
             (cl:if (< (+ (send (eval pnode) :activation) activation) 0) :floor :sum))))
    (kill-node
     (let ((node (car args)))
       (list (and (memq node (send *cytoplasm* :nodes)) t)
             (send node :type)
             (send node :status)
             (and (ca-quiet (cl:lambda () (send *cytoplasm* :find-new-target))) t)
             ;; a linked block: the type of the node above its operation
             ;; ("3dt" is kill-block's gap, PORTING_NOTES item 10)
             (and (equal "4bl" (send node :type)) (equal "linked" (send node :status))
                  (ca-quiet (cl:lambda ()
                              (let ((op (caar (send node :neighbors))))
                                (and op (send op :upper-neighbor)
                                     (send (send op :upper-neighbor) :type)))))))))
    (free-from-pnet (and (car (send (car args) :plinks)) t))))

;;; ---------------------------------------------------------------------------
;;; Recording

(cl:defvar *ca-recording* nil)
(cl:defvar *ca-first-per-run* nil "Record the first call of each codelet in the run.")
(cl:defvar *ca-run-seen* nil "The codelets already recorded in this run.")
(cl:defvar *ca-seen* (make-hash-table :test 'equal) "(codelet . signature) recorded.")
(cl:defvar *ca-inside* nil)
(cl:defvar *ca-source* nil)
(cl:defvar *ca-cases* nil)

(cl:defun ca-record (codelet fn args)
  "Call FN on ARGS, recording the case.  Returns FN's value (or resignals)."
  (let* ((before (ws-state))
         (count (length *ws-queue*))
         (args-json (ws-json args))
         (arg-nodes (ws-array (ws-nodes-from count)))
         (snapshot (ws-global-snapshot))
         (draws *oracle-rng-draws*)
         (*ca-inside* t)
         result error output)
    (setq output
          (with-output-to-string (*standard-output*)
            (handler-case (setq result (apply fn args))
              (error (c) (setq error (princ-to-string c))))))
    (let* ((changed (ws-changed snapshot))
           (result-json (ws-json result))
           (draws-json (ws-json (- *oracle-rng-draws* draws)))
           (after (ws-state))
           (changed-json (ws-changed-json changed)))
      (push (ws-object (list (cons "source" (ws-json *ca-source*))
                             (cons "codelet" (ws-json codelet))
                             (cons "args" args-json)
                             (cons "before" before)
                             (cons "arg-nodes" arg-nodes)
                             (cons "result" result-json)
                             (cons "error" (ws-json (and error t)))
                             (cons "output" (ws-key output))
                             (cons "draws" draws-json)
                             (cons "after" after)
                             (cons "changed" changed-json)))
            *ca-cases*))
    (when error (error "~a" error))
    result))

(cl:defun ca-hook (codelet fn &rest args)
  (cl:if (or (not *ca-recording*) *ca-inside*)
         (apply fn args)
         (let* ((key (cons codelet (ca-signature codelet args)))
                (first (and *ca-first-per-run* (not (member codelet *ca-run-seen*))))
                (new (not (gethash key *ca-seen*))))
           (cl:if (or first new)
                  (progn (setf (gethash key *ca-seen*) t)
                         (push codelet *ca-run-seen*)
                         (ca-record codelet fn args))
                  (apply fn args)))))

(dolist (codelet *ca-codelets*)
  (let ((codelet codelet))
    (sb-int:encapsulate codelet 'codelets-a-fixture
                        (cl:lambda (fn &rest args) (apply #'ca-hook codelet fn args)))))

(cl:defun ca-run (problem seed first-per-run)
  (let ((*ca-recording* t)
        (*ca-first-per-run* first-per-run)
        (*ca-run-seen* nil)
        (*ca-source* (list :run problem seed))
        result)
    (setq result (let ((*standard-output* (make-broadcast-stream)))
                   (oracle-run-config problem :seed seed :max-iterations *ca-cap*)))
    (assert (not (eq (getf result :outcome) :error)) () "~s seed ~s: ~s" problem seed result)
    result))

;;; ---------------------------------------------------------------------------
;;; Made-up calls, on the state a run ends in

(cl:defun ca-made-up (label codelet &rest args)
  ;; ca-record binds *ca-inside*, so the encapsulation just calls through.
  (let ((*ca-source* (list :made-up label)))
    (ignore-errors (ca-record codelet (symbol-function codelet) args))))

(cl:defun ca-made-up-cases ()
  ;; Puzzle 1 (114 11 20 7 1 6) seed 1 run to its end.
  (let ((*standard-output* (make-broadcast-stream)))
    (oracle-run-config '(114 11 20 7 1 6) :seed 1 :max-iterations *ca-cap*))
  ;; (Local names avoid the source's special globals: brick, n, a, ...)
  (let ((ca-brick (cl:find "2b" (send *cytoplasm* :nodes)
                           :key (cl:lambda (ca-n) (send ca-n :type)) :test #'equal)))
    ;; link-to-pnet on a node killed before its link-to-pnet runs.
    (send ca-brick :set-status "killed")
    (ca-made-up "killed" 'link-to-pnet ca-brick "2b" (send ca-brick :value))
    (send ca-brick :set-status "free")
    ;; link-to-pnet: value 0, a value >= 200 (node-150), a rounded value, an
    ;; unknown type (activation keeps its last global value).
    (ca-made-up "zero" 'link-to-pnet ca-brick "4bl" 0)
    (ca-made-up "big" 'link-to-pnet ca-brick "4bl" 260)
    (ca-made-up "rounded" 'link-to-pnet ca-brick "3dt" 37)
    (ca-made-up "rounded-big" 'link-to-pnet ca-brick "1t" 176)
    (ca-made-up "type-5g" 'link-to-pnet ca-brick "5g" 13)
    ;; activate: below %min-activation-to-be-added%, with repump.
    (ca-made-up "small-repump" 'activate 10 node-6 t)
    (ca-made-up "negative" 'activate -40 node-7 nil)
    (ca-made-up "repump-big" 'activate 50 node-150 t)
    ;; kill-node on a node no longer in the cytoplasm, and on a brick.
    (ca-made-up "brick" 'kill-node ca-brick)
    (ca-made-up "not-in-cytoplasm" 'kill-node
                (make-instance 'cyto-node :name 'ca-ghost :type "4bl" :status "free"))
    ;; free-from-pnet with no plinks.
    (ca-made-up "no-plinks" 'free-from-pnet
                (make-instance 'cyto-node :name 'ca-ghost2 :type "4bl"))
    ;; create-cyto-node of a "3dt" and a "4bl" from scratch.
    (ca-made-up "3dt" 'create-cyto-node 200 'ca-dt 9 "free" 97 "3dt" 0)
    (ca-made-up "4bl" 'create-cyto-node 50 'ca-bl 30 "free" 2 "4bl" 0)
    ;; read-brick of a multiple of 10, and an error (no brick 6).
    (ca-made-up "brick2" 'read-brick 2)
    (ca-made-up "brick6" 'read-brick 6)))

;;; ---------------------------------------------------------------------------
;;; Helpers

(cl:defun ca-helpers ()
  (ws-object
   (list (cons "round" (ws-json (cl:loop for v from -100 to 1000 collect (list v (round v)))))
         (cons "ratio" (ws-json (cl:loop for v from 1 to 1000 collect (list v (ratio v)))))
         (cons "expt" (ws-json (cl:loop for v from 0 to 400 collect (list v (expt 0.9d0 v))))))))

;;; ---------------------------------------------------------------------------

(cl:defparameter *ca-output*
  (let (parameters)
    (cl:loop for problem in *ca-puzzles* do (ca-run problem 1 t))
    (setq parameters (ws-parameters))
    (cl:loop for seed in '(2 3)
             do (cl:loop for problem in *ca-puzzles* do (ca-run problem seed t)))
    (ca-made-up-cases)
    (format nil "{~%\"holders\": ~a,~%\"parameters\": ~a,~%\"helpers\": ~a,~%\"cases\": ~a~%}~%"
            (ws-json *ws-holders*)
            parameters
            (ca-helpers)
            (ws-array (reverse *ca-cases*)))))

(cl-user::write-fixture "codelets_a.json" *ca-output*)

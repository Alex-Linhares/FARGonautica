;;; codelets-b.lisp -- write python/fixtures/codelets_b.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/codelets-b.lisp
;;; (or python/scripts/regen_fixtures.sh).
;;;
;;; Cases for the block-search codelets of loop0002 item 9
;;; (python/tests/test_codelets_b.py replays them on python/numbo/codelets.py):
;;; look-for-new-block, look-for-blx, look-for-bl+, look-for-approx-blx,
;;; look-for-approx-bl+, look-for-diff, compare-b-to-t and
;;; test-if-possible-and-desirable.
;;;
;;;   parameters  every %...% global after init-chiffre
;;;   cases       calls of the 8 codelets in real oracle runs (no trace):
;;;               - the first call of each codelet in each chapter puzzle with
;;;                 seed 1;
;;;               - in the runs of the 11 puzzles with seeds 1, 2 and 3
;;;                 (capped at *cb-cap* iterations), every call whose outcome
;;;                 signature (CB-SIGNATURE: the branch it took, read before
;;;                 the call, and what it posted, printed and drew) was not
;;;                 seen before;
;;;               - made-up calls on a real state, for branches the runs
;;;                 don't reach.
;;;               Each case has the format of codelets_a.json (see
;;;               codelets-a.lisp): source, codelet, args, before, arg-nodes,
;;;               result, error, output, draws, after, changed.
;;;   helpers     the pure helpers on ranges of values: sim (with diffrel),
;;;               digits-in-common, multiple, compare, eliminate, remove-dd,
;;;               randlist (with the RNG draws) and find-node (address).
;;;
;;; This file is READ with SBCL's default float format (single), so every
;;; float literal below is written with d0.

(load (merge-pathnames "common.lisp" *load-truename*))
(load (merge-pathnames "lib/world-state.lisp" *load-truename*))

(in-package :numbo)

(cl:defparameter *cb-puzzles*
  '((114 11 20 7 1 6) (87 8 3 9 10 7) (31 3 5 24 3 14) (25 8 5 5 11 2)
    (102 6 17 2 4 1) (146 12 2 5 7 18) (6 3 3 17 11 22) (11 2 5 1 25 23)
    (116 20 2 16 14 6) (127 6 4 22 5 7) (41 5 16 22 25 1)))

(cl:defparameter *cb-codelets*
  '(look-for-new-block look-for-blx look-for-bl+ look-for-approx-blx look-for-approx-bl+
    look-for-diff compare-b-to-t test-if-possible-and-desirable))

(cl:defparameter *cb-cap* 400 "The iteration cap of every run.")

;;; ---------------------------------------------------------------------------
;;; Signatures

(cl:defun cb-quiet (thunk)
  "THUNK's value, or :error; global changes undone."
  (oracle-call-without-global-effects
   (cl:lambda () (handler-case (funcall thunk) (error () :error)))))

(cl:defun cb-branch (codelet args)
  "Read before the call: which cond clause it will take, where the posts
don't tell."
  (case codelet
    (compare-b-to-t
     (let ((b (car args)))
       (cb-quiet (cl:lambda ()
                   (let ((block (send b :value))
                         (ct (send (eval (send *current-target* :name)) :value)))
                     (list (sim block ct) (sim block (send cyto-target :value))
                           (send b :status)
                           (and (is-linked-to-target b) t)
                           (and (digits-in-common block ct) t)))))))
    (test-if-possible-and-desirable
     (destructuring-bind (b1 b2 fun res) args
       (declare (ignore fun))
       (cb-quiet (cl:lambda ()
                   (list (cond ((is-linked-to b1 b2) :linked-to)
                               ((= res (send cyto-target :value)) :target)
                               ((is-linked-to-target b1) :linked-1)
                               ((is-linked-to-target b2) :linked-2)
                               ((= 0 res) :zero)
                               (t :interest))
                         (send b1 :status) (send b2 :status))))))
    ((look-for-approx-bl+ look-for-approx-blx)
     (cb-quiet (cl:lambda ()
                 (compare (send (eval (send *current-target* :name)) :value) args))))
    (look-for-diff (list (car args) (and (send *cytoplasm* :free-blocks) t)))
    (t nil)))

(cl:defvar *cb-posts* nil)

(sb-int:encapsulate 'cr-hang 'codelets-b-fixture
                    (cl:lambda (fn rack form urgency)
                      (push (list* (car form) urgency
                                   (cl:if (eq (car form) 'test-if-possible-and-desirable)
                                          (list (nth 3 form) (eql 0 (nth 4 form)))
                                          nil))
                            *cb-posts*)
                      (funcall fn rack form urgency)))

(cl:defun cb-signature (codelet branch posts error output draws)
  (list codelet branch
        (remove-duplicates (reverse posts) :test #'equal :from-end t)
        (and error t)
        (and (cl:search "killed" output) t)
        (cl:min draws 3)))

;;; ---------------------------------------------------------------------------
;;; Recording

(cl:defvar *cb-recording* nil)
(cl:defvar *cb-first-per-run* nil "Record the first call of each codelet in the run.")
(cl:defvar *cb-run-seen* nil "The codelets already recorded in this run.")
(cl:defvar *cb-seen* (make-hash-table :test 'equal) "Signatures recorded.")
(cl:defvar *cb-inside* nil)
(cl:defvar *cb-source* nil)
(cl:defvar *cb-cases* nil)
(cl:defvar *cb-calls* 0 "Calls of the 8 codelets seen in the runs.")

(cl:defun cb-record (codelet fn args force)
  "Call FN on ARGS.  Record the case if FORCE, if it is the first call of
CODELET in the run (when *cb-first-per-run*), or if its signature is new.
Returns FN's value (or resignals)."
  (let* ((branch (cb-branch codelet args))
         (before (ws-state))
         (count (length *ws-queue*))
         (args-json (ws-json args))
         (arg-nodes (ws-array (ws-nodes-from count)))
         (snapshot (ws-global-snapshot))
         (draws *oracle-rng-draws*)
         (*cb-inside* t)
         (*cb-posts* nil)
         result error output)
    (setq output
          (with-output-to-string (*standard-output*)
            (handler-case (setq result (apply fn args))
              (error (c) (setq error (princ-to-string c))))))
    (let* ((n-draws (- *oracle-rng-draws* draws))
           (key (cb-signature codelet branch *cb-posts* error output n-draws))
           (first (and *cb-first-per-run* (not (member codelet *cb-run-seen*)))))
      (when (or force first (not (gethash key *cb-seen*)))
        (setf (gethash key *cb-seen*) t)
        (push codelet *cb-run-seen*)
        (let* ((changed (ws-changed snapshot))
               (result-json (ws-json result))
               (after (ws-state))
               (changed-json (ws-changed-json changed)))
          (push (ws-object (list (cons "source" (ws-json *cb-source*))
                                 (cons "codelet" (ws-json codelet))
                                 (cons "args" args-json)
                                 (cons "before" before)
                                 (cons "arg-nodes" arg-nodes)
                                 (cons "result" result-json)
                                 (cons "error" (ws-json (and error t)))
                                 (cons "output" (ws-key output))
                                 (cons "draws" (ws-json n-draws))
                                 (cons "after" after)
                                 (cons "changed" changed-json)))
                *cb-cases*))))
    (when error (error "~a" error))
    result))

(cl:defun cb-hook (codelet fn &rest args)
  (cl:if (or (not *cb-recording*) *cb-inside*)
         (apply fn args)
         (progn (incf *cb-calls*) (cb-record codelet fn args nil))))

(dolist (codelet *cb-codelets*)
  (let ((codelet codelet))
    (sb-int:encapsulate codelet 'codelets-b-fixture
                        (cl:lambda (fn &rest args) (apply #'cb-hook codelet fn args)))))

(cl:defun cb-run (problem seed first-per-run)
  (let ((*cb-recording* t)
        (*cb-first-per-run* first-per-run)
        (*cb-run-seen* nil)
        (*cb-source* (list :run problem seed))
        result)
    (setq result (let ((*standard-output* (make-broadcast-stream)))
                   (oracle-run-config problem :seed seed :max-iterations *cb-cap*)))
    (assert (not (eq (getf result :outcome) :error)) () "~s seed ~s: ~s" problem seed result)
    result))

;;; ---------------------------------------------------------------------------
;;; Made-up calls, on the state a run ends in

(cl:defun cb-made-up (label codelet &rest args)
  ;; cb-record binds *cb-inside*, so the encapsulation just calls through.
  (let ((*cb-source* (list :made-up label)))
    (ignore-errors (cb-record codelet (symbol-function codelet) args t))))

(cl:defun cb-of-type (type)
  (cl:remove-if-not (cl:lambda (cb-n) (equal type (send cb-n :type)))
                    (send *cytoplasm* :nodes)))

(cl:defun cb-made-up-cases ()
  ;; Puzzle 1 (114 11 20 7 1 6) seed 1 run to its end.
  (let ((*standard-output* (make-broadcast-stream)))
    (oracle-run-config '(114 11 20 7 1 6) :seed 1 :max-iterations *cb-cap*))
  ;; (Local names avoid the source's special globals: node1, cyto-block1, ...)
  (let* ((cb-bricks (cb-of-type "2b"))
         (cb-b1 (first cb-bricks))
         (cb-b2 (second cb-bricks)))
    ;; look-for-diff: trial 4 (no repost), and no free blocks.
    (cb-made-up "diff-trial-4" 'look-for-diff 4)
    (cb-made-up "diff-trial-3" 'look-for-diff 3)
    ;; test-if-possible-and-desirable: res 0, res = the target, a product.
    (cb-made-up "test-zero" 'test-if-possible-and-desirable cb-b1 cb-b2 "const-bl+" 0)
    (cb-made-up "test-target" 'test-if-possible-and-desirable cb-b1 cb-b2 "const-blx"
                (send cyto-target :value))
    (cb-made-up "test-big" 'test-if-possible-and-desirable cb-b1 cb-b2 "const-blx" 300)
    (cb-made-up "test-500" 'test-if-possible-and-desirable cb-b1 cb-b2 "const-bl+" 650)
    ;; look-for-bl+ / blx on values with no node, and on a value = the target.
    (cb-made-up "bl+-none" 'look-for-bl+ 190 95 95)
    (cb-made-up "blx-none" 'look-for-blx 190 95 2)
    ;; approx with nothing similar to the current target (compare fails).
    (cb-made-up "approx-bl+-far" 'look-for-approx-bl+ 3 1 2)
    (cb-made-up "approx-blx-far" 'look-for-approx-blx 3 1 3)
    ;; compare-b-to-t on a killed node (status neither free nor linked).
    (let ((cb-k (make-instance 'cyto-node :name 'cb-ghost :type "4bl" :status "killed"
                                          :value 113 :level 3)))
      (cb-made-up "compare-killed" 'compare-b-to-t cb-k))
    ;; look-for-new-block with every block at activation 0: randlist gives
    ;; nil, and (nth nil ...) is an error.
    (dolist (cb-n (send *cytoplasm* :cyto-brick-block-nodes)) (send cb-n :set-activation 0))
    (cb-made-up "new-block-zero" 'look-for-new-block)
    ;; find-in with no node similar enough: approx with every block weight 0
    ;; after the first.
    (cb-made-up "approx-zero" 'look-for-approx-bl+ 114 100 14))
  ;; Puzzle 1 seed 1 again, with the target as the current target.
  (let ((*standard-output* (make-broadcast-stream)))
    (oracle-run-config '(114 11 20 7 1 6) :seed 1 :max-iterations *cb-cap*))
  (update-current-target cyto-target 100)
  ;; find-interest-in-pnet on a pnode with no instances, its activation just
  ;; under or on each threshold (10, 14, 50): two free unlinked bricks.
  (let ((cb-f1 (make-instance 'cyto-node :name 'cb-free1 :type "2b" :status "free"
                                         :value 3 :level 1 :activation 50))
        (cb-f2 (make-instance 'cyto-node :name 'cb-free2 :type "2b" :status "free"
                                         :value 16 :level 1 :activation 50))
        (cb-p (find-node 49)))
    (send cb-p :set-instances nil)
    (dolist (cb-a '(9.5d0 10 13.75d0 14 49.5d0 50))
      (send cb-p :set-activation cb-a)
      (cb-made-up (format nil "interest-~a" cb-a)
                  'test-if-possible-and-desirable cb-f1 cb-f2 "const-blx" 49)))
  ;; look-for-bl+ where res is the first value to find: sum = v1 - v2 makes
  ;; the current target (120 - 6 = 114).
  (let ((cb-120 (make-instance 'cyto-node :name 'cb-block120 :type "4bl" :status "free"
                                          :value 120 :level 3 :activation 200)))
    (send *cytoplasm* :set-nodes (cons cb-120 (send *cytoplasm* :nodes)))
    (send (find-node 120) :update-instances (list "4bl" cb-120))
    (cb-made-up "bl+-difference" 'look-for-bl+ 120 6 114)))

;;; ---------------------------------------------------------------------------
;;; Helpers

(cl:defun cb-try (thunk)
  "(:value v) or (:error)."
  (handler-case (list :value (funcall thunk)) (error () (list :error))))

(cl:defun cb-helpers ()
  (let ((values '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 17 19 20 21 22 25 27 30 31 33
                  36 40 44 45 50 55 60 63 66 70 77 80 90 99 100 101 110 114 120 127 140
                  146 150 199 200 201 250 300 499 500 707 999 1000 1500)))
    (ws-object
     (list
      (cons "sim"
            (ws-json (cl:loop for a in values
                              nconc (cl:loop for b in values
                                             collect (let ((r (cb-try (cl:lambda () (sim a b)))))
                                                       (list a b r (cl:if (eq (car r) :value)
                                                                          (symbol-value 'diffrel)
                                                                          :none)))))))
      (cons "digits-in-common"
            (ws-json (cl:loop for a in values
                              nconc (cl:loop for b in values
                                             collect (list a b (digits-in-common a b))))))
      (cons "multiple"
            (ws-json (cl:loop for a in (cdr values)
                              nconc (cl:loop for b in values
                                             collect (list a b (multiple a b))))))
      (cons "compare"
            (ws-json (cl:loop for (e l) in '((10 (100 50 11)) (10 (100 50 15)) (10 (13 14 20))
                                             (114 (100 120 14)) (114 (50 60 70)) (3 nil)
                                             (5 (5)) (20 (14 30 26)) (100 (130 131 70)))
                              collect (list e l (compare e l)))))
      (cons "eliminate"
            (ws-json (cl:loop for (m e l) in '((nil 8 (4 6 7)) (nil 8 (7 9 4)) (nil 5 (3 7 5))
                                               (nil 114 (100 20 120)) (nil 0 (1 1 2))
                                               (nil 10 (20 0 30)) (2000 0 (1000 2000))
                                               (7 0 (1000 2000)) (9 3 nil)
                                               (nil 50 (50 50 50)) (nil -5 (-3 -9 4)))
                              collect (progn (cl:when m (setf (symbol-value 'min) m))
                                             (let ((r (eliminate e l)))
                                               (list m e l r (symbol-value 'diff) (symbol-value 'min)))))))
      (cons "remove-dd"
            (ws-json (cl:loop for (x l) in '((3 (2 3 4 3)) (3 (3)) (5 (1 2 3 4)) (1 nil)
                                             (7 (7 7 7)) (9 (1 9)))
                              collect (list x l (remove-dd x l)))))
      (cons "randlist"
            (ws-json (cl:loop for (seed l) in '((1 (100 50)) (2 (100 50)) (3 (1 1 1 1))
                                                (4 (0.5d0 0.4d0)) (5 nil) (6 (0 0 0))
                                                (7 (2.7d0 0 1.6d0)) (8 (300 -50 10))
                                                (9 (36.75d0 12.5d0 10.75d0)) (10 (0 0 5))
                                                (11 (1)) (12 (1.5d0 1.5d0)))
                              collect (progn (oracle-seed seed)
                                             (let* ((d *oracle-rng-draws*)
                                                    (r (randlist l)))
                                               (list seed l r (- *oracle-rng-draws* d)
                                                     *oracle-rng-state*))))))
      (cons "find-node"
            (ws-json (cl:loop for v in values
                              collect (let ((p (find-node v)))
                                        (list v (ws-holder-of p) (symbol-value 'address) (symbol-value 'div))))))))))

;;; ---------------------------------------------------------------------------

(cl:defparameter *cb-output*
  (let (parameters helpers)
    (cl:loop for problem in *cb-puzzles* do (cb-run problem 1 t))
    (setq parameters (ws-parameters))
    (cl:loop for seed in '(2 3)
             do (cl:loop for problem in *cb-puzzles* do (cb-run problem seed nil)))
    (cb-made-up-cases)
    (setq helpers (cb-helpers))
    (format nil "{~%\"holders\": ~a,~%\"parameters\": ~a,~%\"helpers\": ~a,~%\"calls\": ~d,~%\"cases\": ~a~%}~%"
            (ws-json *ws-holders*)
            parameters
            helpers
            *cb-calls*
            (ws-array (reverse *cb-cases*)))))

(cl-user::write-fixture "codelets_b.json" *cb-output*)

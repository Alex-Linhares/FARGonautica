;;; codelets-c.lisp -- write python/fixtures/codelets_c.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/codelets-c.lisp
;;; (or python/scripts/regen_fixtures.sh).
;;;
;;; Cases for the codelets and functions of loop0002 item 10
;;; (python/tests/test_codelets_c.py replays them on python/numbo/codelets.py):
;;; decomp+, decompi, decompx, const-bl+, const-blx, replace-target,
;;; propagate-success, create-op-node, update-success, temperature,
;;; collect-misfortune, check-temperature, decrease-interest, decompose,
;;; create-coderack, kill-block, repump (and kill-node in the gap run).
;;;
;;;   parameters  every %...% global after init-chiffre
;;;   cases       calls of those functions, in the format of codelets_a.json
;;;               (see codelets-a.lisp): source, codelet, args, before,
;;;               arg-nodes, result, error, output, draws, after, changed.
;;;               Unlike items 8 and 9, nested calls are recorded too
;;;               (propagate-success inside replace-target, create-op-node
;;;               inside const-bl+, ...), each with its own state numbering.
;;;               - the first call of each codelet the coderack or the main
;;;                 loop runs (*cc-first-per-run*), in each chapter puzzle with
;;;                 seed 1;
;;;               - in the runs of the 11 puzzles with seeds 1, 2 and 3 (capped
;;;                 at *cc-cap* iterations), every call whose outcome signature
;;;                 (CC-SIGNATURE) is new;
;;;               - every call of kill-node, kill-block, replace-target,
;;;                 propagate-success and decompose in the gap run (below);
;;;               - made-up calls on real states, for branches the runs don't
;;;                 reach.
;;;   gap-run     the kill-block gap (PORTING_NOTES.md, item 10): the oracle
;;;               run of puzzle 3 (31 3 5 24 3 14) with seed 8 prints "Done :"
;;;               with an invalid decomposition.  Its outcome, full output and
;;;               check-solution's verdict.
;;;   helpers     misfortune, mean and diff300 on ranges of values.
;;;
;;; This file is READ with SBCL's default float format (single), so every
;;; float literal below is written with d0.

(load (merge-pathnames "common.lisp" *load-truename*))
(load (merge-pathnames "lib/world-state.lisp" *load-truename*))

(in-package :numbo)

(cl:defparameter *cc-puzzles*
  '((114 11 20 7 1 6) (87 8 3 9 10 7) (31 3 5 24 3 14) (25 8 5 5 11 2)
    (102 6 17 2 4 1) (146 12 2 5 7 18) (6 3 3 17 11 22) (11 2 5 1 25 23)
    (116 20 2 16 14 6) (127 6 4 22 5 7) (41 5 16 22 25 1)))

(cl:defparameter *cc-functions*
  '(decomp+ decompi decompx const-bl+ const-blx replace-target propagate-success
    create-op-node update-success temperature collect-misfortune check-temperature
    decrease-interest decompose create-coderack kill-block repump kill-node))

(cl:defparameter *cc-first-per-run-functions*
  '(decomp+ decompi const-bl+ const-blx replace-target check-temperature decrease-interest)
  "The codelets the coderack or the main loop runs.")

(cl:defparameter *cc-gap-functions*
  '(kill-node kill-block replace-target propagate-success decompose)
  "Recorded at every call in the gap run.")

(cl:defparameter *cc-gap-problem* '(31 3 5 24 3 14))
(cl:defparameter *cc-gap-seed* 8)

(cl:defparameter *cc-cap* 400 "The iteration cap of every run.")

;;; ---------------------------------------------------------------------------
;;; Signatures

(cl:defvar *cc-quiet* nil "Inside a signature computation: don't record.")

(cl:defun cc-quiet (thunk)
  "THUNK's value, or :error; global changes undone, nothing recorded."
  (let ((*cc-quiet* t))
    (oracle-call-without-global-effects
     (cl:lambda () (handler-case (funcall thunk) (error () :error))))))

(cl:defun cc-current-target ()
  (eval (send *current-target* :name)))

(cl:defun cc-branch (codelet args)
  "Read before the call: which cond clause it will take, where the posts
don't tell."
  (case codelet
    (decomp+
     (destructuring-bind (b ct) args
       (cc-quiet (cl:lambda ()
                   (list (send b :status) (send ct :status)
                         (= (send b :value) (send ct :value))
                         (> (send b :value) (send ct :value)))))))
    (decompi
     (destructuring-bind (b ct rank) args
       (declare (ignore ct))
       (cc-quiet (cl:lambda () (list (= 1 (send b :value)) rank)))))
    ((const-bl+ const-blx)
     (destructuring-bind (b1 b2 v) args
       (cc-quiet (cl:lambda ()
                   (let ((v1 (send b1 :value)) (v2 (send b2 :value)))
                     (list (send b1 :status) (send b2 :status)
                           (cond ((equal v (+ v1 v2)) :sum)
                                 ((equal v (- v1 v2)) :d12)
                                 ((equal v (- v2 v1)) :d21)
                                 (t :none))
                           (cl:if (integerp v) (list (= 0 (mod v 5)) (= 0 (mod v 10)) (minusp v))
                                  :not-integer)))))))
    (replace-target
     (destructuring-bind (b ct) args
       (cc-quiet (cl:lambda ()
                   (cond ((equal (send cyto-target :value) (send b :value)) :obvious)
                         ((eq ct (send *current-target* :name)) :current)
                         (t :other))))))
    (temperature
     (cc-quiet (cl:lambda ()
                 (list (> (+ (length (send *cytoplasm* :free-cyto-nodes))
                             (/ (- 99 (send (send *current-target* :name) :level)) 2))
                          4)
                       (null (send *cytoplasm* :secondary-cyto-nodes))))))
    (check-temperature
     (cc-quiet (cl:lambda () (< %temperature-threshold% (temperature)))))
    (decrease-interest
     (cc-quiet (cl:lambda () (null (send *cytoplasm* :free-secondary-cyto-nodes)))))
    (collect-misfortune
     (cc-quiet (cl:lambda () (null (send *cytoplasm* :secondary-cyto-nodes)))))
    (decompose
     (cc-quiet (cl:lambda ()
                 (let ((op (send (eval (car args)) :neighbors)))
                   (list (length op) (count-if (cl:lambda (p) (send (car p) :listed)) op))))))
    (t nil)))

(cl:defun cc-dangling-p ()
  "Whether an operation node of the cytoplasm has a killed neighbor: what the
kill-block gap leaves."
  (cc-quiet (cl:lambda ()
              (and (cl:some (cl:lambda (cc-n)
                              (and (equal "5g" (send cc-n :type))
                                   (cl:some (cl:lambda (p) (equal "killed" (send (car p) :status)))
                                            (send cc-n :neighbors))))
                            (send *cytoplasm* :nodes))
                   t))))

(cl:defun cc-result-kind (result)
  (cond ((null result) nil)
        ((typep result 'cyto-node) :node)
        ((integerp result) :integer)
        ((floatp result) :float)
        (t :other)))

(cl:defvar *cc-posts* nil)

(sb-int:encapsulate 'cr-hang 'codelets-c-fixture
                    (cl:lambda (fn rack form urgency)
                      (push (list (car form) urgency) *cc-posts*)
                      (funcall fn rack form urgency)))

(cl:defun cc-signature (codelet branch posts error output draws result)
  (list codelet branch
        (remove-duplicates (reverse posts) :test #'equal :from-end t)
        (and error t)
        (and (cl:search "killed" output) t)
        (cl:min draws 3)
        (and (boundp '*problem-solved*) *problem-solved*)
        (cc-dangling-p)
        (cc-result-kind result)))

;;; ---------------------------------------------------------------------------
;;; Recording

(cl:defvar *cc-recording* nil)
(cl:defvar *cc-first-per-run* nil "Record the first call of each codelet in the run.")
(cl:defvar *cc-force* nil "Record every call of these functions.")
(cl:defvar *cc-run-seen* nil "The codelets already recorded in this run.")
(cl:defvar *cc-seen* (make-hash-table :test 'equal) "Signatures recorded.")
(cl:defvar *cc-source* nil)
(cl:defvar *cc-cases* nil)
(cl:defvar *cc-calls* 0 "Calls of the recorded functions seen in the runs.")

(cl:defun cc-record (codelet fn args force)
  "Call FN on ARGS.  Record the case if FORCE, if it is the first call of
CODELET in the run (when *cc-first-per-run*), or if its signature is new.
A nested call has its own node numbering, posts and output; its posts and
output are then passed on to the enclosing call.  Returns FN's value (or
resignals)."
  (let (posts result error output)
    (let* ((*ws-ids* nil)
           (*ws-queue* nil)
           (branch (cc-branch codelet args))
           (before (ws-state))
           (count (length *ws-queue*))
           (args-json (ws-json args))
           (arg-nodes (ws-array (ws-nodes-from count)))
           (snapshot (ws-global-snapshot))
           (draws *oracle-rng-draws*)
           (*cc-posts* nil))
      (setq output
            (with-output-to-string (*standard-output*)
              (handler-case (setq result (apply fn args))
                (error (c) (setq error (princ-to-string c))))))
      (setq posts *cc-posts*)
      (let* ((n-draws (- *oracle-rng-draws* draws))
             (key (cc-signature codelet branch posts error output n-draws result))
             (first (and *cc-first-per-run*
                         (member codelet *cc-first-per-run-functions*)
                         (not (member codelet *cc-run-seen*)))))
        (when (or force first (member codelet *cc-force*) (not (gethash key *cc-seen*)))
          (setf (gethash key *cc-seen*) t)
          (push codelet *cc-run-seen*)
          (let* ((changed (ws-changed snapshot))
                 (result-json (ws-json result))
                 (after (ws-state))
                 (changed-json (ws-changed-json changed)))
            (push (ws-object (list (cons "source" (ws-json *cc-source*))
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
                  *cc-cases*)))))
    (setq *cc-posts* (append posts *cc-posts*))
    (write-string output)
    (when error (error "~a" error))
    result))

(cl:defun cc-hook (codelet fn &rest args)
  (cl:if (or (not *cc-recording*) *cc-quiet*
             (and (eq codelet 'kill-node) (not (member 'kill-node *cc-force*))))
         (apply fn args)
         (progn (incf *cc-calls*) (cc-record codelet fn args nil))))

(dolist (codelet *cc-functions*)
  (let ((codelet codelet))
    (sb-int:encapsulate codelet 'codelets-c-fixture
                        (cl:lambda (fn &rest args) (apply #'cc-hook codelet fn args)))))

(cl:defun cc-run (problem seed first-per-run &optional force)
  "Run PROBLEM with SEED, recording; returns (result output)."
  (let ((*cc-recording* t)
        (*cc-first-per-run* first-per-run)
        (*cc-force* force)
        (*cc-run-seen* nil)
        (*cc-source* (list :run problem seed))
        result output)
    (setq output (with-output-to-string (*standard-output*)
                   (setq result (oracle-run-config problem :seed seed
                                                           :max-iterations *cc-cap*))))
    (assert (not (eq (getf result :outcome) :error)) () "~s seed ~s: ~s" problem seed result)
    (list result output)))

;;; ---------------------------------------------------------------------------
;;; The gap run

(cl:defun cc-gap-run ()
  (destructuring-bind (result output) (cc-run *cc-gap-problem* *cc-gap-seed* nil
                                              *cc-gap-functions*)
    (multiple-value-bind (valid reason) (check-solution output *cc-gap-problem*)
      (assert (eq (getf result :outcome) :solved))
      (assert (not valid))
      (ws-object (list (cons "problem" (ws-json *cc-gap-problem*))
                       (cons "seed" (ws-json *cc-gap-seed*))
                       (cons "outcome" (ws-json (getf result :outcome)))
                       (cons "iterations" (ws-json (getf result :iterations)))
                       (cons "valid" (ws-json valid))
                       (cons "reason" (ws-key reason))
                       (cons "output" (ws-key output)))))))

;;; ---------------------------------------------------------------------------
;;; Made-up calls, on the state a run ends in

(cl:defun cc-made-up (label codelet &rest args)
  (let ((*cc-source* (list :made-up label))
        (*cc-recording* nil))
    (let ((*standard-output* (make-broadcast-stream)))
      (ignore-errors (cc-record codelet (symbol-function codelet) args t)))))

(cl:defun cc-quiet-run (problem seed)
  (let ((*standard-output* (make-broadcast-stream)))
    (oracle-run-config problem :seed seed :max-iterations *cc-cap*)))

(cl:defun cc-node (name type value status level &optional (activation 50) (success 0))
  "A cyto-node outside the cytoplasm (local names avoid the source's globals)."
  (make-instance 'cyto-node :name name :type type :value value :status status
                            :level level :activation activation :success success))

(cl:defun cc-made-up-cases ()
  ;; The gap run's end state: decompose again, every operation node listed.
  (cc-made-up "decompose-listed" 'decompose 'cyto-target)
  ;; Puzzle 3 seed 1, capped (unsolved).
  (cc-quiet-run '(31 3 5 24 3 14) 1)
  (let ((cc-ct (cc-node 'cc-dtarget30 "3dt" 30 "free" 97 200))
        (cc-b30 (cc-node 'cc-block30 "4bl" 30 "free" 3 200 1))
        (cc-b40 (cc-node 'cc-block40 "4bl" 40 "free" 3 200 1))
        (cc-b6 (cc-node 'cc-block6 "4bl" 6 "free" 3 200 1))
        (cc-l40 (cc-node 'cc-linked40 "4bl" 40 "linked" 3 200 1))
        (cc-b1 (cc-node 'cc-brick1 "2b" 1 "free" 1 50 1))
        (cc-3 (cc-node 'cc-brick3 "2b" 3 "free" 1 50 1))
        (cc-8 (cc-node 'cc-brick8 "2b" 8 "free" 1 50 1))
        (cc-12 (cc-node 'cc-block12 "4bl" 12 "free" 3 200 1))
        (cc-10 (cc-node 'cc-block10 "4bl" 10 "free" 3 200 1))
        (cc-5 (cc-node 'cc-brick5 "2b" 5 "free" 1 50 1))
        (cc-7 (cc-node 'cc-brick7 "2b" 7 "free" 1 50 1)))
    ;; decompx: its LET rebinds cyto-block to nil, so (send nil :status).
    (cc-made-up "decompx" 'decompx cc-b6 cc-ct)
    ;; decomp+: equal, linked block, block above and below the target.
    (cc-made-up "decomp+-equal" 'decomp+ cc-b30 cc-ct)
    (cc-made-up "decomp+-linked" 'decomp+ cc-l40 cc-ct)
    (cc-made-up "decomp+-above" 'decomp+ cc-b40 cc-ct)
    (cc-made-up "decomp+-below" 'decomp+ cc-b6 (cc-node 'cc-dtarget25 "3dt" 25 "free" 95 200))
    ;; decompi: a block of 1, and each rank list.
    (cc-made-up "decompi-one" 'decompi cc-b1 cc-ct '(1 1 0))
    (cc-made-up "decompi-110" 'decompi cc-3 cc-ct '(1 1 0))
    (cc-made-up "decompi-100" 'decompi cc-3 cc-ct '(1 0 0))
    (cc-made-up "decompi-001" 'decompi cc-3 cc-ct '(0 0 1))
    ;; const-bl+: each clause, negative sums, the activations, no clause.
    (cc-made-up "const-bl+-d21" 'const-bl+ cc-3 cc-8 5)
    (cc-made-up "const-bl+-d12" 'const-bl+ (cc-node 'cc-brick3b "2b" 3 "free" 1)
                (cc-node 'cc-brick8b "2b" 8 "free" 1) -5)
    (cc-made-up "const-bl+-none" 'const-bl+ (cc-node 'cc-brick3c "2b" 3 "free" 1)
                (cc-node 'cc-brick8c "2b" 8 "free" 1) 100)
    (cc-made-up "const-bl+-20" 'const-bl+ cc-12 cc-8 20)
    (cc-made-up "const-bl+-15" 'const-bl+ cc-10 cc-5 15)
    (cc-made-up "const-bl+-linked" 'const-bl+ cc-10 cc-5 15)
    ;; const-blx: the activations, and a linked block.
    (cc-made-up "const-blx-21" 'const-blx cc-7 (cc-node 'cc-brick3d "2b" 3 "free" 1) 21)
    (cc-made-up "const-blx-15" 'const-blx (cc-node 'cc-brick3e "2b" 3 "free" 1)
                (cc-node 'cc-brick5b "2b" 5 "free" 1) 15)
    (cc-made-up "const-blx-50" 'const-blx (cc-node 'cc-brick10 "2b" 10 "free" 1)
                (cc-node 'cc-brick5c "2b" 5 "free" 1) 50)
    (cc-made-up "const-blx-linked" 'const-blx cc-7 cc-3 21)
    ;; replace-target: the target's value ("Obvious."), and a current target
    ;; that is not *current-target*'s.
    (cc-made-up "replace-other" 'replace-target cc-b40 cc-ct)
    (cc-made-up "replace-obvious" 'replace-target
                (cc-node 'cc-block31 "4bl" 31 "free" 3 200 1) cc-ct))
  ;; update-success on each success pattern.
  ;; (2 0 0) and (0 2 0) sum to 2 with two zeros: the first zero wins.
  (cl:loop for (s1 s2 s3) in '((0 1 1) (1 0 1) (1 1 0) (1 1 1) (0 0 1) (0 0 0) (1 1 nil)
                               (2 0 0) (0 2 0))
           for i from 0
           do (cc-made-up (format nil "update-success-~a~a~a" s1 s2 s3) 'update-success
                          (list (list (cc-node 'cc-s1 "4bl" 10 "linked" 3 50 s1) 'result)
                                (list (cc-node 'cc-s2 "2b" 4 "linked" 1 50 s2) 'operand)
                                (list (cc-node 'cc-s3 "2b" 6 "linked" 1 50 s3) 'operand))))
  ;; create-coderack.
  (cc-made-up "create-coderack" 'create-coderack)
  ;; The end state of the first unsolved seed-1 run with two or more
  ;; secondary nodes.  check-temperature and temperature on a hot
  ;; cytoplasm: a secondary node with activation 0.05 has misfortune 400+.
  (cl:loop for cc-p in *cc-puzzles*
           until (and (not (eql 1 (progn (cc-quiet-run cc-p 1) *problem-solved*)))
                      (>= (length (send *cytoplasm* :secondary-cyto-nodes)) 2)))
  (let ((cc-secondary (send *cytoplasm* :secondary-cyto-nodes)))
    (assert (>= (length cc-secondary) 2))
    (send (first cc-secondary) :set-activation 0.05d0)
    (cc-made-up "temperature-hot" 'temperature)
    (cc-made-up "check-temperature-hot" 'check-temperature)
    ;; ... and every other weight negative: randlist gives nil, nothing posted.
    (dolist (cc-n (rest cc-secondary)) (send cc-n :set-activation 1000))
    (cc-made-up "check-temperature-no-victim" 'check-temperature)
    ;; A misfortune of an activation 0: (quotient 20 0).
    (send (first cc-secondary) :set-activation 0)
    (cc-made-up "temperature-zero" 'temperature)
    ;; The current target named by the symbol cyto-target (as read-target
    ;; and replace-target leave it): (send 'cyto-target :level).
    (update-current-target 'cyto-target 100)
    (cc-made-up "temperature-symbol" 'temperature))
  ;; Puzzle 7 (6 3 3 17 11 22) seed 1, solved: propagate-success again.
  (cc-quiet-run '(6 3 3 17 11 22) 1)
  (cc-made-up "propagate-solved" 'propagate-success)
  (cc-made-up "decrease-interest-solved" 'decrease-interest))

;;; ---------------------------------------------------------------------------
;;; Helpers

(cl:defun cc-try (thunk)
  "(:value v) or (:error)."
  (handler-case (list :value (funcall thunk)) (error () (list :error))))

(cl:defun cc-helpers ()
  (let ((interests '(0 1 2 3 7 10 19 20 21 40 50 100 200 300 301 1000 -5
                     0.0d0 0.05d0 0.6d0 1.5d0 18.0d0 30.0d0 120.0d0 299.99d0 -2.5d0))
        (statuses (list "free" "linked" "killed" nil)))
    (ws-object
     (list
      (cons "misfortune"
            (ws-json (cl:loop for i in interests
                              nconc (cl:loop for s in statuses
                                             collect (list i 3 s (cc-try (cl:lambda () (misfortune i 3 s))))))))
      (cons "mean"
            (ws-json (cl:loop for l in '(nil (5) (1 2) (1 2 4) (10 20 30) (10.5d0 2) (7 -3 1)
                                         (12 0.0d0) (1.0d0 2.0d0 4.0d0) (-7 2) (20 10 10 0.5d0))
                              collect (list l (cc-try (cl:lambda () (mean l)))))))
      (cons "diff300"
            (ws-json (cl:loop for v in '(0 1 50 299 300 301 1000 -3 0.5d0 299.95d0 300.0d0 120.0d0)
                              collect (list v (diff300 v)))))))))

;;; ---------------------------------------------------------------------------

(cl:defparameter *cc-output*
  (let (parameters gap helpers)
    (cl:loop for problem in *cc-puzzles* do (cc-run problem 1 t))
    (setq parameters (ws-parameters))
    (cl:loop for seed in '(2 3)
             do (cl:loop for problem in *cc-puzzles* do (cc-run problem seed nil)))
    (setq gap (cc-gap-run))
    (cc-made-up-cases)
    (setq helpers (cc-helpers))
    (format nil "{~%\"holders\": ~a,~%\"parameters\": ~a,~%\"gap-run\": ~a,~%\"helpers\": ~a,~%\"calls\": ~d,~%\"cases\": ~a~%}~%"
            (ws-json *ws-holders*)
            parameters
            gap
            helpers
            *cc-calls*
            (ws-array (reverse *cc-cases*)))))

(cl-user::write-fixture "codelets_c.json" *cc-output*)

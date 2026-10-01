;;; trace-tools.lisp -- item 11: parse CONFIG output into events and compare
;;; it with the 1987 trace (../../numbo-digitized/trace3.31).
;;;
;;; Not part of the 1987 source.  Loaded by tests/validation-tests.lisp and
;;; tests/chapter-runs.lisp, after src/load.lisp.
;;;
;;; The trace has three kinds of event line, all printed by the source:
;;;   Node <name> created          create-cyto-node / create-op-node (codelets.lisp:228, 254)
;;;   Node <name> killed           the kill paragraph at codelets.lisp:487, 491
;;;   About to post codelet <f> <args>   populate-coderack, when %verbose% (pnet-functions.lisp:152)
;;; Franz printed symbols in lower case; SBCL prints them in upper case, so
;;; names are compared after STRING-DOWNCASE.
;;;
;;; Node names (from the trace and the source):
;;;   cyto-target, cyto-brick1 .. cyto-brick5       the puzzle, created once
;;;   cyto-block<v>-v<k>, cyto-target-<v>-v<k>      a block / derived target
;;;   plus<a>-<b>-v<k>, times<a>-<b>-v<k>           its operation node
;;; Version k comes from *name-counter*: one cyto node and one operation node
;;; share it.

;;; Its own package, using only CL: the source proclaims many short names
;;; special (globals.lisp: n, a, brick, liste, ...), and a LET of one of them
;;; here would be clobbered by the codelets.
(defpackage :numbo-trace
  (:use :common-lisp)
  (:export #:parse-events #:node-version #:node-kind #:event-problems
           #:event-counts #:node-event-prefix #:first-ops #:post-count
           #:trace3.31-text #:run-captured))

(in-package :numbo-trace)

(cl:defun split-lines (text)
  (with-input-from-string (s text)
    (loop for line = (read-line s nil) while line collect line)))

(cl:defun string-prefix-p (prefix s)
  (and (>= (length s) (length prefix)) (string= prefix s :end2 (length prefix))))

(cl:defun parse-events (text)
  "TEXT (CONFIG output or the trace file) => list of events:
  (:created name) (:killed name) (:post codelet args) ; names lower case."
  (let ((events nil))
    (dolist (raw (split-lines text) (nreverse events))
      (let ((line (string-trim '(#\Space #\Tab #\Return) (string-downcase raw))))
        (cond
          ((and (string-prefix-p "node " line)
                (or (search " created" line) (search " killed" line)))
           (let* ((sp (position #\Space line :start 5))
                  (name (subseq line 5 sp))
                  (verb (string-trim " " (subseq line sp))))
             (push (list (cl:if (string= verb "created") :created :killed) name)
                   events)))
          ((string-prefix-p "about to post codelet " line)
           (let* ((rest (subseq line (length "about to post codelet ")))
                  (sp (position #\Space rest)))
             (push (list :post (subseq rest 0 sp)
                         (string-trim " " (subseq rest sp)))
                   events))))))))

(cl:defun node-version (name)
  "k for a name ending in -v<k>, else nil."
  (let ((p (search "-v" name :from-end t)))
    (and p (> (length name) (+ p 2))
         (every #'digit-char-p (subseq name (+ p 2)))
         (parse-integer name :start (+ p 2)))))

(cl:defun node-kind (name)
  "One of :target :brick :block :dtarget :plus :times :unknown."
  (cond ((string= name "cyto-target") :target)
        ((and (string-prefix-p "cyto-brick" name) (= (length name) 11)
              (digit-char-p (char name 10)))
         :brick)
        ((null (node-version name)) :unknown)
        ((string-prefix-p "cyto-block" name) :block)
        ((string-prefix-p "cyto-target-" name) :dtarget)
        ((string-prefix-p "plus" name) :plus)
        ((string-prefix-p "times" name) :times)
        (t :unknown)))

(cl:defun op-kind-p (kind) (cl:member kind '(:plus :times)))
(cl:defun cyto-kind-p (kind) (cl:member kind '(:block :dtarget)))

(cl:defun event-problems (events &key (complete t))
  "Structural invariants of the trace's event stream.  Return a list of
problem strings (nil = all hold).  COMPLETE nil = EVENTS may be a prefix
(the trace stops mid-run), so a dangling last pair is not a problem.
  1. every node name has a known shape;
  2. cyto-target and the five bricks are each created once; cyto-target
     before any versioned node.  (A block may appear before the last brick:
     CONFIG interleaves read-brick with cr-choose, so a brick's
     create-cyto-node codelet can still be on the rack.)
  3. versioned nodes are created in pairs, cyto node then its operation
     node, with the same version, and versions go 1, 2, 3, ...;
  4. kills come in pairs, operation node then the cyto node of the same
     version, and only live nodes are killed; puzzle nodes are never killed."
  (let ((problems nil)
        (alive (make-hash-table :test #'equal))
        (created (make-hash-table :test #'equal))
        (node-events (remove :post events :key #'car))
        (bricks-seen 0) (target-seen nil) (next-version 1))
    (flet ((problem (fmt &rest args)
             (push (apply #'format nil fmt args) problems)))
      (loop with rest = node-events
            while rest do
        (destructuring-bind (verb name) (pop rest)
          (let ((kind (node-kind name)))
            (cond
              ((eq kind :unknown) (problem "unknown node name ~a" name))
              ((eq verb :created)
               (when (gethash name created) (problem "~a created twice" name))
               (setf (gethash name created) t (gethash name alive) t)
               (case kind
                 (:target (cl:if target-seen (problem "second cyto-target")
                                 (setq target-seen t)))
                 (:brick (incf bricks-seen))
                 (t
                  (unless target-seen
                    (problem "~a created before cyto-target" name))
                  (cond
                    ((op-kind-p kind)
                     (problem "operation node ~a created without its cyto node" name))
                    (t
                     (unless (eql (node-version name) next-version)
                       (problem "~a: expected version ~a" name next-version))
                     (setq next-version (1+ (node-version name)))
                     (let ((nxt (car rest)))
                       (cond
                         ((null nxt)
                          (when complete (problem "~a: no operation node follows" name)))
                         ((not (and (eq (car nxt) :created)
                                    (op-kind-p (node-kind (cadr nxt)))
                                    (eql (node-version (cadr nxt)) (node-version name))))
                          (problem "~a not followed by its operation node (got ~a)" name nxt))
                         (t (pop rest)
                            (when (gethash (cadr nxt) created)
                              (problem "~a created twice" (cadr nxt)))
                            (setf (gethash (cadr nxt) created) t
                                  (gethash (cadr nxt) alive) t)))))))))
              (t                        ; killed
               (cond
                 ((cl:member kind '(:target :brick)) (problem "puzzle node ~a killed" name))
                 ((cyto-kind-p kind) (problem "~a killed without its operation node first" name))
                 (t
                  (unless (gethash name alive) (problem "~a killed but not alive" name))
                  (remhash name alive)
                  (let ((nxt (car rest)))
                    (cond
                      ((null nxt)
                       (when complete (problem "~a: no cyto node killed after it" name)))
                      ((not (and (eq (car nxt) :killed)
                                 (cyto-kind-p (node-kind (cadr nxt)))
                                 (eql (node-version (cadr nxt)) (node-version name))))
                       (problem "~a killed, then ~a (expected its cyto node)" name nxt))
                      (t (pop rest)
                         (unless (gethash (cadr nxt) alive)
                           (problem "~a killed but not alive" (cadr nxt)))
                         (remhash (cadr nxt) alive)))))))))))
      (when (and complete (not (and target-seen (= bricks-seen 5))))
        (problem "puzzle not read: target ~a, ~a bricks" target-seen bricks-seen)))
    (nreverse problems)))

(cl:defun event-counts (events)
  "Plist of counts: :created :killed :posts, plus one entry per posted codelet
name, e.g. (\"look-for-blx\" . 4), in :post-kinds."
  (let ((kinds nil))
    (dolist (e events)
      (when (eq (car e) :post)
        (let ((cell (assoc (cadr e) kinds :test #'string=)))
          (cl:if cell (incf (cdr cell)) (push (cons (cadr e) 1) kinds)))))
    (list :created (count :created events :key #'car)
          :killed (count :killed events :key #'car)
          :posts (count :post events :key #'car)
          :post-kinds (sort kinds #'string< :key #'car))))

(cl:defun post-count (counts codelet)
  "Number of posts of CODELET (lower-case name) in an EVENT-COUNTS plist."
  (or (cdr (assoc codelet (getf counts :post-kinds) :test #'string=)) 0))

(cl:defun node-event-prefix (events n)
  "The events up to and including the N-th node (created/killed) event."
  (let ((k 0))
    (loop for e in events
          collect e
          do (unless (eq (car e) :post) (incf k))
          until (>= k n))))

(cl:defun first-ops (events)
  "The operation nodes created, in order, as strings."
  (loop for (verb name) in events
        when (and (eq verb :created) (op-kind-p (node-kind name)))
          collect name))

(cl:defparameter *trace3.31-path*
  (merge-pathnames "../../numbo-digitized/trace3.31" *load-truename*))

(cl:defun trace3.31-text ()
  (with-open-file (s *trace3.31-path*)
    (let ((out (make-string-output-stream)))
      (loop for line = (read-line s nil) while line
            do (write-line line out))
      (get-output-stream-string out))))

(cl:defun run-captured (problem &rest keys)
  "Run RUN-CONFIG with KEYS, capturing its output.  Return (values result
output error-string): RESULT is RUN-CONFIG's plist, or nil if it signalled."
  (let* ((result nil) (err nil)
         (output (with-output-to-string (*standard-output*)
                   (handler-case (setq result (apply #'numbo::run-config problem keys))
                     (error (c) (setq err (princ-to-string c)))))))
    (values result output err)))

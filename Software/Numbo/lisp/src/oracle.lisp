;;; oracle.lisp -- opt-in hooks that make the SBCL port an exact oracle for
;;; the Python translation (loop0002).
;;;
;;; Not part of the 1987 source.  load.lisp loads this file only in oracle
;;; mode: (defvar cl-user::*numbo-oracle* t) before loading load.lisp, or the
;;; environment variable NUMBO_ORACLE set to anything but "" or "0".  In
;;; default mode nothing here exists and the port behaves exactly as before
;;; (all the loop0001 tests, and README's seed-18 run).
;;;
;;; It is loaded right after package.lisp, before franz-compat, so the
;;; symbols it shadows are the ones every later file reads.  Every definition
;;; here therefore uses CL:DEFUN, CL:DEFVAR, CL:IF, ... explicitly (NUMBO's
;;; own DEFUN, IF, MOD, MAX ... are only defined by franz-compat.lisp).
;;;
;;; What oracle mode changes (each one logged in PORTING_NOTES.md, "Oracle
;;; hooks"):
;;;
;;; 1. Double floats.  load.lisp binds *READ-DEFAULT-FLOAT-FORMAT* to
;;;    DOUBLE-FLOAT while it loads the numbo files, so 0.9 reads as a double,
;;;    as Franz flonums were.  Floats made at run time from integers are made
;;;    double too: FLOAT and SQRT are shadowed below ((float 1) and (sqrt 16)
;;;    give single floats in CL).  Nothing else in the source makes a float
;;;    from rationals; ORACLE-FIND-SINGLE-FLOATS checks the whole world for
;;;    any single float, and the trace writer refuses one.
;;;
;;; 2. A shared RNG.  RANDOM is shadowed: the 7 (random n) call sites
;;;    (coderack.lisp x2, codelets.lisp x5) use splitmix64, specified below
;;;    bit for bit, instead of CL's *RANDOM-STATE*.
;;;
;;; 3. A copying SORTCAR (ORACLE-INSTALL replaces franz-compat's): the list
;;;    is copied before the stable merge sort, so the caller's list (a
;;;    pnode's instances slot) is never reordered or truncated.
;;;
;;; 4. A JSON-lines trace, written by ORACLE-RUN-CONFIG through
;;;    encapsulations (sb-int:encapsulate, as harness.lisp does) of the
;;;    source's own functions.  No source file is edited.  When no trace is
;;;    open the encapsulations just call through.
;;;
;;; ---------------------------------------------------------------------------
;;; The RNG (splitmix64, Steele, Lea & Flood 2014; Vigna's reference code)
;;;
;;;   state: an unsigned 64-bit integer.  (oracle-seed s) sets it to s mod 2^64.
;;;   next:  state <- (state + #x9E3779B97F4A7C15) mod 2^64
;;;          z <- state
;;;          z <- ((z xor (z >> 30)) * #xBF58476D1CE4E5B9) mod 2^64
;;;          z <- ((z xor (z >> 27)) * #x94D049BB133111EB) mod 2^64
;;;          return z xor (z >> 31)
;;;   (random n), n an integer with 1 <= n <= 2^64, anything else an error:
;;;          limit <- 2^64 - (2^64 mod n)
;;;          repeat x <- next until x < limit      (rejection: no modulo bias)
;;;          return x mod n
;;;   Seed 0's first output is #xE220A8397B1DCDAF.  (random 1) still draws.
;;;   ORACLE-RUN-CONFIG seeds it with the run's seed before init-chiffre.
;;;
;;; ---------------------------------------------------------------------------
;;; The trace: one JSON object per line, in the order things happen.
;;;
;;; Lisp data (codelet args, decomposition values) is encoded as:
;;;   integer -> number          double-float -> number (shortest round trip)
;;;   nil -> null                t -> true
;;;   symbol -> its name, "CYTO-BLOCK27-V2" (keyword: ":NAME")
;;;   string -> {"str": "..."}   (so "free" and the symbol FREE differ)
;;;   proper list -> array       dotted pair -> {"cons": [car, cdr]}
;;;   flavor instance -> {"obj": "cyto-node", "name": <its name>}
;;;   anything else (single float, ratio, ...) -> an error
;;; Event fields that are always names or type strings ("name", "type",
;;; "codelet", "op", ...) are plain JSON strings.  "args" is always an array
;;; of Lisp data ([] for (look-for-new-block)).
;;;
;;; Events ("ev"):
;;;   start         problem, seed, max_iterations, rng ("splitmix64"),
;;;                 pnet (the names of the 88 *pnet* pnodes, in order)
;;;   setup-choose  one of the 13 (eval (cr-choose *coderack*)) of config's
;;;                 set-up phase: codelet (null if the rack was empty), args,
;;;                 urgency, rack (before the choice)
;;;   iteration     one per main-loop iteration (config's do loop, y = n):
;;;                 n, x (config's counter, already incremented), temperature
;;;                 ((temperature) at the start of the iteration, computed
;;;                 without side effects), rack ([[urgency, count], ...] in
;;;                 bin order, at the start of the iteration), and codelet,
;;;                 args, urgency of the codelet chosen in this iteration
;;;                 (all null when the iteration chose none: refresh,
;;;                 look-for-new-block, hot or empty-rack iterations)
;;;   post          every cr-hang: codelet, args, urgency
;;;   node-created  create-cyto-node / create-op-node: name, type, value
;;;   node-killed   disconnect (the op node, then the cyto node): name, type, value
;;;   pnet          after every spread-activation-in-pnet: act, the 88
;;;                 activations in the order of start's pnet
;;;   rack-emptied  every cr-empty-coderack
;;;   rng           every draw, when :rng-events is true: n, value.  Draws
;;;                 made by cr-choose go into the choosing event's "rng"
;;;                 field ([[n, value], ...]) instead.
;;;   done          iterations, decomposition ([{op, a, va, b, vb, result}],
;;;                 parsed from what decompose printed, in printed order)
;;;   gave-up       iterations
;;;   capped        iterations
;;;   error         iterations, message (a Lisp error ended the run, e.g.
;;;                 the reactivate-cyto race, PORTING_NOTES item 10)
;;; "iterations" is the harness's count (run-config :iterations).
;;; An iteration event is written when the iteration starts, but only once
;;; its choice is known: it is held until cr-choose or the next event.

(in-package :numbo)

(shadow '("RANDOM" "FLOAT" "SQRT") :numbo)

(cl:defvar *oracle* t "True when the oracle hooks are loaded.")

;;; ---------------------------------------------------------------------------
;;; 1. Floats made from rationals are doubles

(cl:defun float (x &optional (prototype 1d0))
  "CL FLOAT, but a rational becomes a DOUBLE-FLOAT by default (oracle mode)."
  (cl:float x prototype))

(cl:defun sqrt (x)
  "CL SQRT, but the root of a rational is a DOUBLE-FLOAT (oracle mode)."
  (cl:sqrt (cl:if (rationalp x) (cl:float x 1d0) x)))

;;; ---------------------------------------------------------------------------
;;; 2. The shared RNG

(cl:defparameter +two-64+ (expt 2 64))
(cl:defvar *oracle-rng-state* 0)
(cl:defvar *oracle-rng-draws* 0 "Number of 64-bit outputs drawn since the last seed.")
(cl:defvar *oracle-rng-sink* nil
  "When non-nil, a function of (n value) told of every (random n).")

(cl:defun oracle-seed (seed)
  (setq *oracle-rng-state* (cl:mod seed +two-64+)
        *oracle-rng-draws* 0)
  seed)

(cl:defun oracle-next-u64 ()
  "The next splitmix64 output."
  (let ((z (setq *oracle-rng-state*
                 (ldb (byte 64 0) (+ *oracle-rng-state* #x9E3779B97F4A7C15)))))
    (setq z (ldb (byte 64 0) (* (logxor z (ash z -30)) #xBF58476D1CE4E5B9)))
    (setq z (ldb (byte 64 0) (* (logxor z (ash z -27)) #x94D049BB133111EB)))
    (incf *oracle-rng-draws*)
    (logxor z (ash z -31))))

(cl:defun random (n &optional state)
  "Oracle-mode RANDOM: an integer in [0, N) from splitmix64, by rejection."
  (declare (ignore state))
  (unless (and (integerp n) (<= 1 n +two-64+))
    (error "oracle RANDOM: N must be an integer in [1, 2^64], got ~s" n))
  (let ((limit (- +two-64+ (cl:mod +two-64+ n))))
    (loop
      (let ((x (oracle-next-u64)))
        (when (< x limit)
          (let ((v (cl:mod x n)))
            (when *oracle-rng-sink* (funcall *oracle-rng-sink* n v))
            (return v)))))))

;;; ---------------------------------------------------------------------------
;;; JSON

(cl:defun oracle-write-json-string (s stream)
  (write-char #\" stream)
  (loop for c across s
        do (cond ((char= c #\") (write-string "\\\"" stream))
                 ((char= c #\\) (write-string "\\\\" stream))
                 ((< (char-code c) 32) (format stream "\\u~4,'0x" (char-code c)))
                 (t (write-char c stream))))
  (write-char #\" stream))

(cl:defun oracle-write-number (x stream)
  (cond ((integerp x) (format stream "~d" x))
        ((and (typep x 'double-float)
              (not (sb-ext:float-infinity-p x))
              (not (sb-ext:float-nan-p x)))
         (let ((*read-default-float-format* 'double-float))
           (write-string (prin1-to-string x) stream)))
        (t (error "oracle trace: ~s (~s) is not an integer or a finite double-float"
                  x (type-of x)))))

(cl:defun oracle-proper-list-p (x)
  (loop (cond ((null x) (return t))
              ((atom x) (return nil))
              (t (setq x (cdr x))))))

(cl:defun oracle-write-data (x stream)
  "Write Lisp data X with the encoding described at the top of this file."
  (cond ((null x) (write-string "null" stream))
        ((eq x t) (write-string "true" stream))
        ((numberp x) (oracle-write-number x stream))
        ((stringp x)
         (write-string "{\"str\":" stream) (oracle-write-json-string x stream)
         (write-char #\} stream))
        ((keywordp x) (oracle-write-json-string (concatenate 'string ":" (symbol-name x))
                                                stream))
        ((symbolp x) (oracle-write-json-string (symbol-name x) stream))
        ((consp x)
         (cond ((oracle-proper-list-p x)
                (write-char #\[ stream)
                (loop for (item . more) on x
                      do (oracle-write-data item stream)
                         (when more (write-char #\, stream)))
                (write-char #\] stream))
               (t (write-string "{\"cons\":[" stream)
                  (oracle-write-data (car x) stream)
                  (write-char #\, stream)
                  (oracle-write-data (cdr x) stream)
                  (write-string "]}" stream))))
        ((typep x 'standard-object)
         (write-string "{\"obj\":" stream)
         (oracle-write-json-string (string-downcase (symbol-name (class-name (class-of x))))
                                   stream)
         (write-string ",\"name\":" stream)
         (oracle-write-data (cl:if (and (slot-exists-p x 'name) (slot-boundp x 'name))
                                   (slot-value x 'name)
                                   nil)
                            stream)
         (write-char #\} stream))
        (t (error "oracle trace: cannot encode ~s" x))))

(cl:defun oracle-json-string (x)
  "X (Lisp data) encoded as JSON, as a string."
  (with-output-to-string (s) (oracle-write-data x s)))

;;; Event field values: a Lisp string is a JSON string, (:data . x) is Lisp
;;; data, (:list v ...) is an array of field values, (:object (k . v) ...)
;;; an object, :null null; numbers and symbols as in Lisp data.
(cl:defun oracle-write-field (v stream)
  (cond ((stringp v) (oracle-write-json-string v stream))
        ((eq v :null) (write-string "null" stream))
        ((and (consp v) (eq (car v) :data)) (oracle-write-data (cdr v) stream))
        ((and (consp v) (eq (car v) :list))
         (write-char #\[ stream)
         (loop for (item . more) on (cdr v)
               do (oracle-write-field item stream)
                  (when more (write-char #\, stream)))
         (write-char #\] stream))
        ((and (consp v) (eq (car v) :object))
         (oracle-write-object (cdr v) stream))
        (t (oracle-write-data v stream))))

(cl:defun oracle-write-object (pairs stream)
  (write-char #\{ stream)
  (loop for ((k . v) . more) on pairs
        do (oracle-write-json-string k stream)
           (write-char #\: stream)
           (oracle-write-field v stream)
           (when more (write-char #\, stream)))
  (write-char #\} stream))

;;; ---------------------------------------------------------------------------
;;; Run state

(cl:defvar *oracle-trace* nil "The open trace stream, or nil.")
(cl:defvar *oracle-rng-events* nil)
(cl:defvar *oracle-float-check* nil)
(cl:defvar *oracle-phase* nil "nil, :setup (config before its main loop) or :loop.")
(cl:defvar *oracle-setup-chooses* 0)
(cl:defvar *oracle-last-iteration* nil)
(cl:defvar *oracle-pending* nil "The current iteration event, not yet written.")
(cl:defvar *oracle-in-hook* nil)
(cl:defvar *oracle-decompose-depth* 0)
(cl:defvar *oracle-decomposition* nil)
(cl:defvar *oracle-choose-draws* nil)
(cl:defvar *oracle-single-floats* nil)
(cl:defvar *oracle-doubles-seen* 0)
(cl:defvar *oracle-test-probe* nil "Scratch global for the float-walk control test.")

(cl:defparameter +oracle-setup-chooses+ 13
  "config's set-up phase has exactly 13 (eval (cr-choose *coderack*)) calls
(start.lisp: 3 after read-target, 2 after each of the 5 read-brick).")

(cl:defun oracle-write-event (pairs)
  (oracle-write-object pairs *oracle-trace*)
  (terpri *oracle-trace*))

(cl:defun oracle-flush-pending ()
  (when *oracle-pending*
    (let ((e *oracle-pending*))
      (setq *oracle-pending* nil)
      (oracle-write-event e))))

(cl:defun oracle-emit (ev &rest pairs)
  "Write event EV with PAIRS (key . value), after any pending iteration event."
  (when *oracle-trace*
    (oracle-flush-pending)
    (oracle-write-event (cons (cons "ev" ev) pairs))))

(cl:defun oracle-note-draw (n v)
  (cond (*oracle-choose-draws* (push (list n v) (cdr *oracle-choose-draws*)))
        (*oracle-rng-events*
         (oracle-emit "rng" (cons "n" n) (cons "value" v)))))

;;; ---------------------------------------------------------------------------
;;; World inspection

(cl:defun oracle-numbo-symbols ()
  (let ((syms nil) (pkg (cl:find-package :numbo)))
    (do-symbols (s pkg)
      (when (eq (symbol-package s) pkg) (push s syms)))
    syms))

(cl:defun oracle-call-without-global-effects (thunk)
  "Call THUNK, then undo any change it made to the value of a NUMBO symbol.
temperature's collect-misfortune SETQs the free global current-target,
which look-for-blx reads; the trace must not change it."
  (let* ((syms (oracle-numbo-symbols))
         (saved (loop for s in syms
                      collect (cl:if (boundp s) (cons t (symbol-value s)) (list nil)))))
    (unwind-protect (funcall thunk)
      (loop for s in syms
            for (bound . value) in saved
            do (cond ((and bound (not (and (boundp s) (eq (symbol-value s) value))))
                      (setf (symbol-value s) value))
                     ((and (not bound) (boundp s) (not (constantp s)))
                      (makunbound s)))))))

(cl:defun oracle-find-single-floats ()
  "Walk every NUMBO global (values and property lists) and everything they
reach (conses, vectors, instance and structure slots, hash tables).  Return
a list of descriptions of the non-double floats found; count the doubles in
*ORACLE-DOUBLES-SEEN*."
  (let ((seen (make-hash-table :test #'eq))
        (found nil))
    (labels ((walk (x root)
               (cond ((typep x 'double-float) (incf *oracle-doubles-seen*))
                     ((floatp x) (push (format nil "~s in ~a" x root) found))
                     ((or (numberp x) (symbolp x) (characterp x) (stringp x)
                          (functionp x) (streamp x) (packagep x)))
                     ((gethash x seen))
                     (t
                      (setf (gethash x seen) t)
                      (cond ((consp x) (walk (car x) root) (walk (cdr x) root))
                            ((vectorp x) (loop for e across x do (walk e root)))
                            ((hash-table-p x)
                             (maphash (lambda (k v) (walk k root) (walk v root)) x))
                            ((or (typep x 'standard-object) (typep x 'structure-object))
                             (dolist (slot (sb-mop:class-slots (class-of x)))
                               (let ((name (sb-mop:slot-definition-name slot)))
                                 (when (slot-boundp x name)
                                   (walk (slot-value x name) root))))))))))
      (dolist (s (oracle-numbo-symbols))
        (unless (cl:member s '(*oracle-single-floats* *oracle-trace*))
          (when (boundp s) (walk (symbol-value s) (symbol-name s)))
          (walk (symbol-plist s) (format nil "plist of ~a" (symbol-name s))))))
    found))

(cl:defun oracle-float-check (where)
  (when *oracle-float-check*
    (let ((found (oracle-find-single-floats)))
      (when found
        (push (list where found) *oracle-single-floats*)))))

(cl:defun oracle-rack-field (rack)
  (cons :list (loop for bin in (coderack-bins (cr-get rack))
                    collect (list :list (car bin) (length (cdr bin))))))

(cl:defun oracle-pnode-name (p)
  (send (cl:if (symbolp p) (symbol-value p) p) :name))

(cl:defun oracle-form-fields (form urgency)
  "codelet, args, urgency fields for a codelet FORM (nil = none)."
  (list (cons "codelet" (cl:if form (symbol-name (car form)) :null))
        (cons "args" (cl:if form (cons :list (mapcar (lambda (a) (cons :data a)) (cdr form))) :null))
        (cons "urgency" (cl:if form urgency :null))))

;;; ---------------------------------------------------------------------------
;;; Hooks (each one is an sb-int:encapsulate function of (fn &rest args))

(cl:defun oracle-begin-iteration (n)
  (oracle-flush-pending)
  (oracle-float-check (format nil "iteration ~d" n))
  (let ((temperature (let ((*oracle-in-hook* t))
                       (oracle-call-without-global-effects #'temperature))))
    (setq *oracle-pending*
          (list* (cons "ev" "iteration")
                 (cons "n" n)
                 (cons "x" (symbol-value 'x))
                 (cons "temperature" temperature)
                 (cons "rack" (oracle-rack-field (symbol-value '*coderack*)))
                 (oracle-form-fields nil nil)))))

(cl:defun oracle-iteration-hook (fn &rest args)
  "On MOD and CR-EMPTY-CODERACK: config calls one of them first in every
main-loop iteration, right after (setq *iteration* y)."
  (when (and *oracle-trace* (eq *oracle-phase* :loop) (not *oracle-in-hook*))
    (let ((n (symbol-value '*iteration*)))
      (unless (eql n *oracle-last-iteration*)
        (setq *oracle-last-iteration* n)
        (oracle-begin-iteration n))))
  (apply fn args))

(cl:defun oracle-cr-empty-coderack-hook (fn &rest args)
  (multiple-value-prog1 (apply #'oracle-iteration-hook fn args)
    (oracle-emit "rack-emptied")))

(cl:defun oracle-config-hook (fn &rest args)
  (when *oracle-trace*
    (setq *oracle-phase* :setup *oracle-setup-chooses* 0 *oracle-last-iteration* nil))
  (unwind-protect (apply fn args)
    (setq *oracle-phase* nil)))

(cl:defun oracle-cr-choose-hook (fn name &optional full)
  (cond
    ((or (null *oracle-trace*) (null *oracle-phase*))
     (funcall fn name full))
    (t
     (let* ((rack (oracle-rack-field name))
            (*oracle-choose-draws* (list :draws))
            (res (funcall fn name t))
            (fields (append (oracle-form-fields (car res) (cadr res))
                            (when *oracle-rng-events*
                              (list (cons "rng" (cons :list (mapcar (lambda (d) (cons :list d))
                                                                    (reverse (cdr *oracle-choose-draws*))))))))))
       (cond ((eq *oracle-phase* :setup)
              (apply #'oracle-emit "setup-choose"
                     (append fields (list (cons "rack" rack))))
              (when (= (incf *oracle-setup-chooses*) +oracle-setup-chooses+)
                ;; The main loop starts only once config has evaluated this
                ;; last set-up codelet, which may itself call MOD (e.g.
                ;; compare-b-to-t -> digits-in-common).  So hand config a
                ;; form that evaluates the codelet, then switches phase.
                (setq *oracle-phase* :last-setup)
                (cl:if (car res)
                       (setq res (list (list 'oracle-end-setup (list 'quote (car res)))
                                       (cadr res)))
                       (setq *oracle-phase* :loop))))
             ((and (eq *oracle-phase* :loop) *oracle-pending*)
              (let ((e *oracle-pending*))
                (setq *oracle-pending* nil)
                (oracle-write-event
                 (append (remove-if (lambda (p) (cl:member (car p) '("codelet" "args" "urgency")
                                                           :test #'string=))
                                    e)
                         fields))))
             (t (error "oracle: cr-choose in phase ~s outside an iteration" *oracle-phase*)))
       (cl:if full res (car res))))))

(cl:defun oracle-end-setup (form)
  "Evaluate config's last set-up codelet FORM (as config's own EVAL would, in
the null lexical environment), then start the main-loop phase."
  (multiple-value-prog1 (eval form)
    (setq *oracle-phase* :loop)))

(cl:defun oracle-cr-hang-hook (fn name form urgency)
  (multiple-value-prog1 (funcall fn name form urgency)
    (when *oracle-trace*
      (apply #'oracle-emit "post" (oracle-form-fields form urgency)))))

(cl:defun oracle-node-fields (name type value)
  (list (cons "name" (symbol-name name))
        (cons "type" (cl:if (stringp type) type (cons :data type)))
        (cons "value" (cons :data value))))

(cl:defun oracle-create-cyto-node-hook (fn activation name value status level type success)
  ;; Written before the call: in create-cyto-node nothing that makes an event
  ;; comes before its "Node ~a created" line.
  (when *oracle-trace*
    (apply #'oracle-emit "node-created" (oracle-node-fields name type value)))
  (funcall fn activation name value status level type success))

(cl:defun oracle-create-op-node-hook (fn name res op1 op2 activation level)
  ;; create-op-node makes a "5g" node with no value.
  (when *oracle-trace*
    (apply #'oracle-emit "node-created" (oracle-node-fields name "5g" nil)))
  (funcall fn name res op1 op2 activation level))

(cl:defun oracle-disconnect-hook (fn cyto-node op)
  ;; disconnect always prints "Node <op> killed" then "Node <cyto-node>
  ;; killed", with no event before them.
  (when *oracle-trace*
    (dolist (node (list op cyto-node))
      (apply #'oracle-emit "node-killed"
             (oracle-node-fields (send node :name) (send node :type) (send node :value)))))
  (funcall fn cyto-node op))

(cl:defun oracle-spread-hook (fn &rest args)
  (multiple-value-prog1 (apply fn args)
    (when *oracle-trace*
      (oracle-emit "pnet"
                   (cons "act" (cons :list (mapcar (lambda (p)
                                                     (send (cl:if (symbolp p) (symbol-value p) p)
                                                           :activation))
                                                   (symbol-value '*pnet*))))))))

(cl:defun oracle-parse-decomposition (text)
  "The paragraphs decompose printed, as field objects:
Operation OP has been applied / to A ( VA) and to B ( VB) / to get R"
  (let ((tokens (coerce (solution-tokens text) 'vector))
        (ops nil))
    (flet ((tok (i) (cl:if (< i (length tokens)) (aref tokens i) ""))
           (val (s) (cl:if (and (plusp (length s))
                                (every (lambda (c) (or (digit-char-p c) (char= c #\-))) s))
                           (or (parse-integer s :junk-allowed t) s)
                           s)))
      (loop for i below (length tokens)
            when (string= (tok i) "Operation")
              do (unless (and (string= (tok (+ i 2)) "has") (string= (tok (+ i 9)) "to")
                              (string= (tok (+ i 12)) "to") (string= (tok (+ i 13)) "get"))
                   (error "oracle: unexpected decompose output ~s" text))
                 (push (list :object
                             (cons "op" (tok (+ i 1)))
                             (cons "a" (tok (+ i 6))) (cons "va" (val (tok (+ i 7))))
                             (cons "b" (tok (+ i 10))) (cons "vb" (val (tok (+ i 11))))
                             (cons "result" (tok (+ i 14))))
                       ops)))
    (nreverse ops)))

(cl:defun oracle-decompose-hook (fn node)
  (cond ((or (null *oracle-trace*) (plusp *oracle-decompose-depth*))
         (let ((*oracle-decompose-depth* (1+ *oracle-decompose-depth*)))
           (funcall fn node)))
        (t
         (let* ((capture (make-string-output-stream))
                (result (let ((*standard-output* (make-broadcast-stream *standard-output* capture))
                              (*oracle-decompose-depth* 1))
                          (funcall fn node))))
           (setq *oracle-decomposition*
                 (oracle-parse-decomposition (get-output-stream-string capture)))
           result))))

(cl:defun oracle-copying-sortcar (list predicate)
  "Franz SORTCAR, copying (oracle mode): sort a copy of LIST of lists by their
cars with PREDICATE; nil means alphabetical (ALPHALESSP).  The caller's list
is left as it was."
  (stable-sort (copy-list list) (or predicate 'alphalessp) :key #'car))

(cl:defparameter +oracle-hooks+
  '((config oracle-config-hook)
    (cr-choose oracle-cr-choose-hook)
    (cr-hang oracle-cr-hang-hook)
    (mod oracle-iteration-hook)
    (cr-empty-coderack oracle-cr-empty-coderack-hook)
    (create-cyto-node oracle-create-cyto-node-hook)
    (create-op-node oracle-create-op-node-hook)
    (disconnect oracle-disconnect-hook)
    (spread-activation-in-pnet oracle-spread-hook)
    (decompose oracle-decompose-hook)))

(cl:defun oracle-install ()
  "Called by load.lisp after every file is loaded (oracle mode only)."
  (setf (fdefinition 'sortcar) #'oracle-copying-sortcar)
  (dolist (h +oracle-hooks+)
    (destructuring-bind (f hook) h
      (unless (fboundp f) (error "oracle-install: ~s is not defined" f))
      (unless (sb-int:encapsulated-p f 'oracle)
        (sb-int:encapsulate f 'oracle (symbol-function hook)))))
  ;; The harness's iteration cap must be outside the oracle hooks, so that
  ;; the iteration it stops at is never begun in the trace.
  (install-iteration-cap)
  (setq *oracle-rng-sink* #'oracle-note-draw)
  t)

;;; ---------------------------------------------------------------------------
;;; Runs

(cl:defun oracle-run-config (problem &key (seed 1) (max-iterations 500) trace
                                          rng-events float-check verbose)
  "RUN-CONFIG in oracle mode: the shared RNG seeded with SEED, and a JSON-lines
trace written to TRACE (a stream, a pathname or a namestring, or nil for
none).  RNG-EVENTS adds the RNG draws to the trace.  FLOAT-CHECK walks the
whole world for single floats at the start of every iteration and at the end.
Returns run-config's plist, with :outcome :error (and :error message) if a
Lisp error ended the run, plus :single-floats and :doubles-seen."
  (let ((stream (cl:if (or (stringp trace) (pathnamep trace))
                       (open trace :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
                       trace)))
    (unwind-protect
         (let ((*oracle-trace* stream)
               (*oracle-rng-events* rng-events)
               (*oracle-float-check* float-check)
               (*oracle-phase* nil)
               (*oracle-setup-chooses* 0)
               (*oracle-last-iteration* nil)
               (*oracle-pending* nil)
               (*oracle-in-hook* nil)
               (*oracle-decompose-depth* 0)
               (*oracle-decomposition* nil)
               (*oracle-choose-draws* nil)
               (*oracle-single-floats* nil)
               (*oracle-doubles-seen* 0)
               result)
           (oracle-seed seed)
           (oracle-emit "start"
                        (cons "problem" (cons :data problem))
                        (cons "seed" seed)
                        (cons "max_iterations" (cons :data max-iterations))
                        (cons "rng" "splitmix64")
                        (cons "pnet" (cons :list (mapcar (lambda (p) (symbol-name (oracle-pnode-name p)))
                                                         (symbol-value '*pnet*)))))
           (setq result
                 (handler-case (run-config problem :seed seed :max-iterations max-iterations
                                                   :verbose verbose)
                   (error (c)
                     (list :outcome :error
                           :iterations (cl:if *oracle-last-iteration* (1+ *oracle-last-iteration*) 0)
                           :seed seed
                           :problem-solved (symbol-value '*problem-solved*)
                           :error (princ-to-string c)))))
           (setq *oracle-phase* nil)
           (when *oracle-trace*
             (oracle-flush-pending)
             (let ((its (cons "iterations" (getf result :iterations))))
               (ecase (getf result :outcome)
                 (:solved (oracle-emit "done" its
                                       (cons "decomposition" (cons :list *oracle-decomposition*))))
                 (:gave-up (oracle-emit "gave-up" its))
                 (:capped (oracle-emit "capped" its))
                 (:error (oracle-emit "error" its (cons "message" (getf result :error))))))
             (finish-output *oracle-trace*))
           (oracle-float-check "end of run")
           (append result
                   (list :single-floats (reverse *oracle-single-floats*)
                         :doubles-seen *oracle-doubles-seen*)))
      (when (and stream (not (eq stream trace)))
        (close stream)))))

;;; ---------------------------------------------------------------------------
;;; Test vectors for the Python RNG (../python/fixtures/rng_vectors.json)

(cl:defparameter +oracle-rng-vector-seeds+ '(0 1 18))
(cl:defparameter +oracle-rng-vector-ns+
  (list 1 2 3 5 7 10 13 100 600 1000 6 31 64 1000000007
        (expt 2 32) (1+ (expt 2 63)) (1- (expt 2 64)) (1+ (expt 2 63)) 2 (expt 2 64))
  "Several n, including ones whose rejection rate is about 1/2 (2^63 + 1).")

(cl:defun oracle-rng-vectors-json ()
  "The text of ../python/fixtures/rng_vectors.json."
  (with-output-to-string (s)
    (format s "{~%  \"algorithm\": \"splitmix64\",~%")
    (format s "  \"spec\": \"src/oracle.lisp: state = seed mod 2^64; next: state += 0x9E3779B97F4A7C15, z = state, z = (z ^ z>>30) * 0xBF58476D1CE4E5B9, z = (z ^ z>>27) * 0x94D049BB133111EB, return z ^ z>>31 (all mod 2^64); random(n): limit = 2^64 - 2^64 mod n, draw until x < limit, return x mod n\",~%")
    (format s "  \"outputs\": {~%")
    (loop for (seed . more) on +oracle-rng-vector-seeds+
          do (oracle-seed seed)
             (format s "    \"~d\": " seed)
             (oracle-write-data (loop repeat 20 collect (oracle-next-u64)) s)
             (format s "~:[~;,~]~%" more))
    (format s "  },~%  \"random\": [~%")
    (loop for (seed . more) on +oracle-rng-vector-seeds+
          do (oracle-seed seed)
             (let ((values (let ((*oracle-rng-sink* nil))
                             (mapcar #'random +oracle-rng-vector-ns+))))
               (format s "    {\"seed\": ~d, \"n\": " seed)
               (oracle-write-data +oracle-rng-vector-ns+ s)
               (format s ", \"values\": ")
               (oracle-write-data values s)
               (format s ", \"draws\": ~d}~:[~;,~]~%" *oracle-rng-draws* more)))
    (format s "  ]~%}~%")))

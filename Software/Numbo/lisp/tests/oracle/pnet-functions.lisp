;;; pnet-functions.lisp -- write ../python/fixtures/pnet_functions.json from the oracle.
;;;
;;; Run from anywhere: sbcl --non-interactive --load tests/oracle/pnet-functions.lisp
;;; (or ../python/scripts/regen_fixtures.sh).
;;;
;;; Snapshots of the Pnet's state around the 14 pnode methods and 6 functions
;;; of src/pnet-functions.lisp (../python/tests/test_pnet_functions.py replays
;;; them on ../python/numbo/pnet_functions.py):
;;;
;;;   loaded            the 91 holders' pnodes right after loading (init-pnet)
;;;   print             what (pnode :print) prints for a few fresh pnodes
;;;   init-chiffre      after (init-chiffre), which runs initialize-pnet-2
;;;   initialize-pnet   after the first (initialize-pnet) (*iteration* 0)
;;;   scenarios         starting states (synthetic ones made from config's
;;;                     start, and real ones taken from oracle runs just before
;;;                     their Kth spread-activation-in-pnet), each with
;;;                     the state after 1, 2 and 10 cycles of
;;;                     spread-activation-in-pnet, three populate-coderack
;;;                     calls after cycle 1 (the posts, in order, and the
;;;                     state after each), and an initialize-pnet at the end
;;;   methods           a starting state, every reader method on every *pnet*
;;;                     pnode, and from the state after them (ops-start,
;;;                     *iteration* 17) a sequence of mutating sends, each with
;;;                     its return value and the state after it
;;;
;;; A state is {"iteration", "node", "res", "pnodes"}: *iteration*, the free
;;; globals NODE and RES that :hotter-neighbor-activation and
;;; :suppress-instances SETQ (PORTING_NOTES "Compile census"; :unbound when
;;; unbound), and for each of the 91 holders (init-pnet order) its pnode's
;;; activation, spreadable-activation, temp-activation-holder, instances and
;;; codelets (thresholds as the forms they are), plus neighbors in "loaded"
;;; and "init-chiffre".  Values use the trace's Lisp-data encoding
;;; (src/oracle.lisp); a pnode inside a value is written (:pnode holder).
;;;
;;; This file is READ with SBCL's default float format (single), so every
;;; float literal below is written with d0.

(load (merge-pathnames "common.lisp" *load-truename*))

(in-package :numbo)

(cl:defun pf-source-forms (file)
  "Every top-level form of src/FILE, read as numbo-load reads it."
  (let ((*package* (cl:find-package :numbo))
        (*read-default-float-format* 'double-float))
    (with-open-file (in (merge-pathnames (concatenate 'string "src/" file)
                                         cl-user::*oracle-repo*))
      (cl:loop for form = (read in nil in)
               until (eq form in)
               collect form))))

(cl:defun pf-defun-body (file name)
  (cddr (cdr (cl:find-if (cl:lambda (f) (and (consp f) (eq (car f) 'defun)
                                             (eq (cadr f) name)))
                         (pf-source-forms file)))))

(cl:defparameter *pf-holders*
  (cl:mapcar #'cadr (pf-defun-body "pnet-def.lisp" 'init-pnet))
  "The 91 holders init-pnet SETQs, in its order.")

(assert (= (length *pf-holders*) 91))

(cl:defparameter *pf-config-forms*
  (cl:loop for form in (pf-defun-body "start.lisp" 'config)
           until (and (consp form) (eq (car form) 'init-cytoplasm))
           collect form)
  "config's forms before (init-cytoplasm ...): initialize-pnet and the
9 pseudo-instances.")

(assert (= (length *pf-config-forms*) 10))

;;; ---------------------------------------------------------------------------
;;; Encoding

(cl:defun pf-holder-of (pnode)
  (or (cl:find-if (cl:lambda (h) (eq (symbol-value h) pnode)) *pf-holders*)
      (error "no holder for ~s" pnode)))

(cl:defun pf-data (x)
  "X with every pnode replaced by (:pnode holder)."
  (cond ((typep x 'pnode) (list :pnode (pf-holder-of x)))
        ((consp x) (cons (pf-data (car x)) (pf-data (cdr x))))
        (t x)))

(cl:defun pf-json (x) (oracle-json-string (pf-data x)))

(cl:defun pf-key (x) (with-output-to-string (s) (oracle-write-json-string x s)))

(cl:defun pf-object (pairs)
  "PAIRS ((key . json-text) ...) as a JSON object."
  (format nil "{~{~a~^,~}}"
          (cl:loop for (k . v) in pairs
                   collect (format nil "~a:~a" (pf-key k) v))))

(cl:defun pf-array (texts) (format nil "[~{~a~^,~%~}]" texts))

(cl:defun pf-global (sym)
  (pf-json (cl:if (boundp sym) (symbol-value sym) :unbound)))

(cl:defun pf-pnode-state (holder &optional neighbors)
  (let ((p (symbol-value holder)))
    (pf-object
     (append
      (list (cons "holder" (pf-json holder))
            (cons "activation" (pf-json (send p :activation)))
            (cons "spreadable-activation" (pf-json (send p :spreadable-activation)))
            (cons "temp-activation-holder" (pf-json (send p :temp-activation-holder)))
            (cons "instances" (pf-json (send p :instances)))
            (cons "codelets" (pf-json (send p :codelets))))
      (cl:if neighbors (list (cons "neighbors" (pf-json (send p :neighbors)))))))))

(cl:defun pf-state (&optional neighbors)
  (pf-object
   (list (cons "iteration" (pf-global '*iteration*))
         (cons "node" (pf-global 'node))
         (cons "res" (pf-global 'res))
         (cons "pnodes" (pf-array (cl:mapcar (cl:lambda (h) (pf-pnode-state h neighbors))
                                             *pf-holders*))))))

;;; ---------------------------------------------------------------------------
;;; populate-coderack with its cr-hang calls recorded

(cl:defvar *pf-posts* :off)

(cl:defun pf-cr-hang-hook (fn name form urgency)
  (unless (eq *pf-posts* :off) (push (list form urgency) *pf-posts*))
  (funcall fn name form urgency))

(sb-int:encapsulate 'cr-hang 'pnet-fixture #'pf-cr-hang-hook)

(cl:defun pf-populate (iteration &optional verbose)
  "Set *iteration* to ITERATION and run (populate-coderack), with %verbose%
VERBOSE.  An object with the posts, the output and the state after it."
  (setf (symbol-value '*iteration*) iteration)
  (let ((%verbose% verbose) (*pf-posts* nil) output)
    (declare (special %verbose%))
    (setq output (with-output-to-string (*standard-output*) (populate-coderack)))
    (pf-object (list (cons "iteration" (pf-json iteration))
                     (cons "verbose" (pf-json verbose))
                     (cons "posts" (pf-json (reverse *pf-posts*)))
                     (cons "output" (pf-key output))
                     (cons "state" (pf-state))))))

;;; ---------------------------------------------------------------------------
;;; A scenario: from the current state, 10 cycles, 3 populate-coderacks and an
;;; initialize-pnet.  SPREAD is the function to call for one cycle.

(cl:defun pf-scenario (name source spread)
  ;; As in the main loop, the populate-coderacks come right after a spread
  ;; (after cycle 1).  They change only codelets, which spreading never reads.
  ;; Thresholds set by the first populate are (max 30 (add base ...)) and by
  ;; the second (max 30 (add base+7 ...)); at base+33 the first evaluate to 30
  ;; (left alone by initialize-codelet), the second to 34.
  (let* ((start (pf-state))
         (base (symbol-value '*iteration*))
         populates
         (cycles (cl:loop for n from 1 to 10
                          do (funcall spread)
                          when (cl:member n '(1 2 10))
                            collect (pf-object (list (cons "cycles" (pf-json n))
                                                     (cons "state" (pf-state))))
                          when (= n 1)
                            do (setq populates (list (pf-populate base t)
                                                     (pf-populate (+ base 7))
                                                     (pf-populate (+ base 12)))))))
    (setf (symbol-value '*iteration*) (+ base 33))
    (initialize-pnet)
    (pf-object (list (cons "name" (pf-key name))
                     (cons "source" (pf-key source))
                     (cons "start" start)
                     (cons "cycles" (pf-array cycles))
                     (cons "populate" (pf-array populates))
                     (cons "initialize-pnet"
                           (pf-object (list (cons "iteration" (pf-json (+ base 33)))
                                            (cons "state" (pf-state)))))))))

;;; ---------------------------------------------------------------------------
;;; Fresh pnodes: the state, and :print

(cl:defparameter *pf-loaded* (pf-state t))

(cl:defun pf-print (holder)
  (pf-object (list (cons "holder" (pf-json holder))
                   (cons "output" (pf-key (with-output-to-string (*standard-output*)
                                            (send (symbol-value holder) :print)))))))

(cl:defparameter *pf-print*
  (prog2
      (progn (send result+ :set-instances '(("5g" "result+")))
             ;; "instances: " lines that end at column 80 (fits), 81 (the
             ;; last element and its paren don't fit), and a middle element
             ;; ending at 80 (fits: no paren after it)
             (send node-7 :set-instances '(("4bl" cyto-block100-v1234) ("4bl" cyto-block1-v1)
                                           ("4bl" cyto-block2-v1)))
             (send node-8 :set-instances '(("4bl" cyto-block1000-v1234) ("4bl" cyto-block1-v1)
                                           ("4bl" cyto-block2-v1)))
             (send node-9 :set-instances '(("4bl" cyto-block1000-v1234) ("4bl" cyto-block1-v1)
                                           ("4bl" cyto-block2-v1) ("4bl" cyto-block3-v1)))
             (send node-5 :set-instances '(("2b" cyto-brick2) ("1t" cyto-target)))
             (send node-5 :set-activation 50.5d0))
      (list (pf-print 'node-5) (pf-print 'result+) (pf-print 'plus2-3)
            (pf-print 'node-multiply) (pf-print 'node-1) (pf-print 'node-10)
            (pf-print 'node-20) (pf-print 'times10-15) (pf-print 'node-7)
            (pf-print 'node-8) (pf-print 'node-9))
    (send node-7 :set-instances nil)
    (send node-8 :set-instances nil)
    (send node-9 :set-instances nil)
    (send result+ :set-instances nil)
    (send node-5 :set-instances nil)
    (send node-5 :set-activation nil)))

(let ((*standard-output* (make-broadcast-stream)))
  (init-chiffre))

(cl:defparameter *pf-init-chiffre* (pf-state t))

(setf (symbol-value '*iteration*) 0)
(initialize-pnet)

(cl:defparameter *pf-initialize-pnet* (pf-state))

;;; ---------------------------------------------------------------------------
;;; Real scenarios: oracle runs stopped just before their Kth spread.  They
;;; come first, before anything below changes the Pnet as no run would.

(cl:defparameter *pf-real*
  '(((114 11 20 7 1 6) 1 1) ((31 3 5 24 3 14) 1 3) ((116 20 2 16 14 6) 1 5)))

(cl:defvar *pf-spread-calls* 0)
(cl:defvar *pf-stop-at* nil)
(cl:defvar *pf-name* nil)

(cl:defun pf-spread-hook (fn &rest args)
  (incf *pf-spread-calls*)
  (cl:if (eql *pf-spread-calls* *pf-stop-at*)
         (throw 'pf-stop
           (let ((*standard-output* (make-broadcast-stream)))
             (pf-scenario (format nil "~(~s~) seed ~d spread ~d"
                                  (first *pf-name*) (second *pf-name*) *pf-stop-at*)
                          (format nil "oracle run ~s seed ~d, before spread-activation-in-pnet call ~d"
                                  (first *pf-name*) (second *pf-name*) *pf-stop-at*)
                          (cl:lambda () (apply fn args)))))
         (apply fn args)))

(sb-int:encapsulate 'spread-activation-in-pnet 'pnet-fixture #'pf-spread-hook)

(cl:defparameter *pf-real-json*
  (cl:loop for (problem seed k) in *pf-real*
           collect (let ((*pf-spread-calls* 0) (*pf-stop-at* k)
                         (*pf-name* (list problem seed k)))
                     (let ((json (catch 'pf-stop
                                   (let ((*standard-output* (make-broadcast-stream)))
                                     (oracle-run-config problem :seed seed :max-iterations 3000))
                                   nil)))
                       (or json (error "run ~s ended before spread ~d" problem k))))))

(sb-int:unencapsulate 'spread-activation-in-pnet 'pnet-fixture)

;;; ---------------------------------------------------------------------------
;;; Synthetic scenarios, from config's start and set-up-activations

(cl:defun pf-config-start (iteration activations instances)
  "config's start (initialize-pnet, pseudo-instances), then *iteration*,
set-up-activations ACTIVATIONS and the INSTANCES ((holder instances) ...)."
  (setf (symbol-value '*iteration*) 0)
  (cl:dolist (form *pf-config-forms*) (eval form))
  (setf (symbol-value '*iteration*) iteration)
  (set-up-activations activations)
  (cl:loop for (h i) in instances do (send (symbol-value h) :set-instances i)))

(cl:defparameter *pf-synthetic*
  '(("link nodes and a few numbers" 0
     (node-5 50.5d0 plus2-3 24.0d0 operation 7 result+ 100.0d0 resultx 200.0d0
      operand 50.0d0 similar 50.0d0)
     ())
    ("puzzle-like (114 11 20 7 1 6)" 5
     (node-100 170 node-10 60 node-20 60 node-7 60 node-1 60 node-6 60
      result+ 100.0d0 resultx 200.0d0 operand 50.0d0 similar 50.0d0
      operation 200 instance 200 node-add 60.0d0 node-multiply 40.0d0)
     ((node-100 (("1t" cyto-target)))
      (node-10 (("2b" cyto-brick1)))
      (node-20 (("2b" cyto-brick2)))
      (node-7 (("2b" cyto-brick3)))
      (node-1 (("2b" cyto-brick4)))
      (node-6 (("4bl" cyto-block6-v2) ("2b" cyto-brick5)))
      (node-15 (("3dt" cyto-target-14-v1)))
      (times2-3 (("4bl" cyto-block6-v2)))))
    ("thresholds and clamps" 40
     (node-50 1000.0d0 node-60 24.0d0 node-70 23.999d0 plus5-5 500 times5-10 0
      node-12 0.0d0 node-9 35.0d0 plus2-5 -3.0d0 result+ 63.25d0 resultx 88.125d0
      node-subtract 31.0d0)
     ((node-50 (("3dt" cyto-target-50-v2) ("1t" cyto-target)))
      (node-60 (("2b" cyto-brick1) ("3dt" cyto-target-60-v1)))
      (plus5-5 (("4bl" cyto-block10-v4)))
      (node-9 (("zz" odd) ("5g" odd2)))))))

(cl:defparameter *pf-synthetic-json*
  (cl:loop for (name iteration activations instances) in *pf-synthetic*
           collect (progn (pf-config-start iteration activations instances)
                          (pf-scenario name
                                       (format nil "config start, *iteration* ~d, set-up-activations ~s"
                                               iteration activations)
                                       #'spread-activation-in-pnet))))

;;; ---------------------------------------------------------------------------
;;; Method cases, from the second synthetic start after 2 cycles

(destructuring-bind (name iteration activations instances) (second *pf-synthetic*)
  (declare (ignore name))
  (pf-config-start iteration activations instances))
(spread-activation-in-pnet)
(spread-activation-in-pnet)

(cl:defparameter *pf-methods-start* (pf-state))

(cl:defparameter *pf-readers*
  (cl:loop for p in (symbol-value '*pnet*)
           collect (let ((hot (send p :hotter-neighbor-activation)))
                     (pf-json (list (pf-holder-of p)
                                    (send p :activation-decay)
                                    (send p :activation-decay-factor)
                                    (send p :link-length)
                                    (send p :link-length 5)
                                    hot
                                    (symbol-value 'node)
                                    (send p :codelet-urgency 150))))))

(cl:defparameter *pf-ops*
  '((:add-activation node-5 30.0d0) (:add-activation node-5 24.0d0)
    (:add-activation node-5 24.5d0) (:add-activation node-5 -24.0d0)
    (:add-activation node-5 -30.0d0) (:add-activation node-5 -1000)
    (:add-activation node-5 25) (:add-activation node-5 -25)
    (:add-activation node-4 -1000.0d0)
    (:subtract-activation node-20 24.0d0) (:subtract-activation node-20 25.0d0)
    (:subtract-activation node-20 100)
    (:add-temp-activation-holder node-5 3.5d0) (:add-temp-activation-holder node-5 7)
    (:update-activation node-5)
    (:add-temp-activation-holder result+ 3.5d0) (:update-activation result+)
    (:add-temp-activation-holder plus2-3 100.0d0) (:update-activation plus2-3)
    (:add-temp-activation-holder plus2-4 24.0d0) (:update-activation plus2-4)
    (:update-activation plus2-5)
    (:update-instances node-6 ("3dt" cyto-target-6-v3))
    (:update-instances node-6 ("2b" cyto-brick2))
    (:suppress-instances node-6 cyto-brick2)
    (:suppress-instances node-6 cyto-nothing)
    (:suppress-instances node-100 cyto-target)
    (:update-instances node-100 ("1t" cyto-target))
    (:hotter-neighbor-activation node-add)
    (:modify-threshold plus2-3 look-for-bl+ %upper-threshold%)
    (:modify-threshold plus2-3 look-for-bl+)
    (:modify-threshold node-multiply look-for-no-such-codelet)
    (:spread-activation node-10)
    (:set-up-activations nil (node-1 7 node-2 8.5d0 plus1-1 0))
    ;; No pnode of the 1987 Pnet has more than one codelet, so the order
    ;; :modify-threshold reverses is only seen on a made-up list.  The
    ;; activations sit on the thresholds: 30 = %first-threshold%, and 60.0 =
    ;; (max 30 (add 17 (minus 17) 60)) at *iteration* 17.
    (:set-codelets plus3-4 ((look-for-bl+ %first-threshold% %second-urgency% (7 3 4))
                            (look-for-diff %first-threshold% %first-urgency% (0))
                            (look-for-blx %upper-threshold% %third-urgency% (12 3 4))))
    (:set-activation plus3-4 30)
    (:populate-coderack nil)
    (:modify-threshold plus3-4 look-for-diff)
    (:set-activation plus3-4 60.0d0)
    (:populate-coderack nil)
    (:set-activation plus3-4 59.99d0)
    (:populate-coderack nil)))

(setf (symbol-value '*iteration*) 17)

(cl:defparameter *pf-ops-start* (pf-state))

(cl:defparameter *pf-ops-json*
  (cl:loop for (message holder . args) in *pf-ops*
           collect (let ((result (case message
                                   (:set-up-activations (apply #'set-up-activations args))
                                   ;; the result is the posts, in order
                                   (:populate-coderack
                                    (let ((*pf-posts* nil))
                                      (populate-coderack)
                                      (reverse *pf-posts*)))
                                   (t (apply #'send (symbol-value holder) message args)))))
                     (pf-object (list (cons "send" (pf-json (list* message holder args)))
                                      (cons "result" (pf-json result))
                                      (cons "state" (pf-state)))))))

;;; init-chiffre and initialize-pnet don't reset codelets: put plus3-4's back.
(send plus3-4 :set-codelets '((look-for-bl+ %first-threshold% %second-urgency% (7 3 4))))

(sb-int:unencapsulate 'cr-hang 'pnet-fixture)

(cl-user::write-fixture
 "pnet_functions.json"
 (format nil "~a~%"
         (pf-object
          (list (cons "loaded" *pf-loaded*)
                (cons "print" (pf-array *pf-print*))
                (cons "init-chiffre" *pf-init-chiffre*)
                (cons "initialize-pnet" *pf-initialize-pnet*)
                (cons "scenarios" (pf-array (append *pf-synthetic-json* *pf-real-json*)))
                (cons "methods"
                      (pf-object (list (cons "start" *pf-methods-start*)
                                       (cons "readers" (pf-array *pf-readers*))
                                       (cons "ops-start" *pf-ops-start*)
                                       (cons "ops" (pf-array *pf-ops-json*)))))))))

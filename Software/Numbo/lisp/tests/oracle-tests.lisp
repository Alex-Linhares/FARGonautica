;;; oracle-tests.lisp -- loop0002 item 1: the opt-in oracle hooks (src/oracle.lisp).
;;;
;;; Run: sbcl --non-interactive --load tests/oracle-tests.lisp
;;; Exits 0 if every check passes, 1 otherwise.
;;;
;;; Loads the system in oracle mode (cl-user::*numbo-oracle* = t) and checks:
;;;  1. Oracle mode is on: RANDOM, FLOAT and SQRT are shadowed in NUMBO, the
;;;     numbo sources were read with double floats, and the reader default is
;;;     back to single-float after loading.
;;;  2. The shared RNG: splitmix64 reference outputs (seed 0's first output is
;;;     Vigna's published 0xE220A8397B1DCDAF), (random n) by rejection, errors
;;;     for a bad n, and ../python/fixtures/rng_vectors.json is up to date.
;;;  3. SORTCAR copies its argument.
;;;  4. The 11 chapter puzzles, seeds 1-2, run in oracle mode without error and
;;;     write a valid JSON-lines trace (parsed here, and by python3's json);
;;;     the event stream is consistent with the run's outcome.
;;;  5. Every float in those runs is a DOUBLE-FLOAT: the whole world (every
;;;     NUMBO global, every pnode and cyto-node slot, the coderack) is walked
;;;     at the start of every main-loop iteration, and the trace writer
;;;     refuses any other float.  A control shows the walk finds a single.
;;;  6. Same seed => byte-identical trace; different seed => different;
;;;     CL's *RANDOM-STATE* plays no part.

(require :sb-posix)
(defvar cl-user::*numbo-no-autoload* t)
(defvar cl-user::*numbo-oracle* t)
(let ((*error-output* (make-broadcast-stream))
      (*standard-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (load (merge-pathnames "../src/load.lisp" *load-truename*))
    (funcall (intern "NUMBO-LOAD" "CL-USER"))))

;;; Own package: a LET of a name the 1987 source proclaims special (N, A, ...)
;;; would be clobbered by the codelets (see tests/trace-tools.lisp).
(defpackage :numbo-oracle-tests (:use :common-lisp))
(in-package :numbo-oracle-tests)

(defvar *failures* 0)
(defvar *checks* 0)
(defvar *repo* (merge-pathnames "../" (make-pathname :name nil :type nil
                                                     :defaults *load-truename*)))

(defmacro check (form expected &key (test '#'equal))
  `(let ((got (handler-case ,form
                (error (c) (list :error (princ-to-string c))))))
     (incf *checks*)
     (unless (funcall ,test got ,expected)
       (incf *failures*)
       (format t "~&FAIL: ~s~%  expected ~s~%  got      ~s~%" ',form ,expected got))))

(defun sym (name) (find-symbol name :numbo))
(defun fn (name) (symbol-function (sym name)))
(defun call (name &rest args) (apply (fn name) args))

;;; --- a minimal JSON reader (objects -> alists, arrays -> lists) ---------------------
(defun json-parse (string)
  (let ((i 0) (n (length string)))
    (labels ((peek () (cl:if (< i n) (char string i) nil))
             (ws () (loop while (and (< i n) (member (char string i) '(#\Space #\Tab #\Newline #\Return)))
                          do (incf i)))
             (fail (what) (error "JSON: ~a at ~d in ~s" what i string))
             (expect (c) (unless (eql (peek) c) (fail (format nil "expected ~a" c))) (incf i))
             (lit (word value)
               (unless (and (<= (+ i (length word)) n)
                            (string= word string :start2 i :end2 (+ i (length word))))
                 (fail "bad literal"))
               (incf i (length word)) value)
             (str ()
               (expect #\")
               (with-output-to-string (out)
                 (loop
                   (let ((c (peek)))
                     (cond ((null c) (fail "unterminated string"))
                           ((char= c #\") (incf i) (return))
                           ((char= c #\\)
                            (incf i)
                            (let ((e (peek)))
                              (incf i)
                              (case e
                                (#\" (write-char #\" out)) (#\\ (write-char #\\ out))
                                (#\/ (write-char #\/ out)) (#\n (write-char #\Newline out))
                                (#\t (write-char #\Tab out)) (#\r (write-char #\Return out))
                                (#\b (write-char #\Backspace out)) (#\f (write-char #\Page out))
                                (#\u (write-char (code-char (parse-integer string :start i :end (+ i 4)
                                                                                  :radix 16))
                                                 out)
                                 (incf i 4))
                                (t (fail "bad escape")))))
                           ((< (char-code c) 32) (fail "control character in string"))
                           (t (write-char c out) (incf i)))))))
             (num ()
               (let ((start i))
                 (when (eql (peek) #\-) (incf i))
                 (unless (and (peek) (digit-char-p (peek))) (fail "bad number"))
                 (if (eql (peek) #\0)
                     (incf i)
                     (loop while (and (peek) (digit-char-p (peek))) do (incf i)))
                 (let ((float nil))
                   (when (eql (peek) #\.)
                     (setq float t) (incf i)
                     (unless (and (peek) (digit-char-p (peek))) (fail "bad fraction"))
                     (loop while (and (peek) (digit-char-p (peek))) do (incf i)))
                   (when (member (peek) '(#\e #\E))
                     (setq float t) (incf i)
                     (when (member (peek) '(#\+ #\-)) (incf i))
                     (unless (and (peek) (digit-char-p (peek))) (fail "bad exponent"))
                     (loop while (and (peek) (digit-char-p (peek))) do (incf i)))
                   (let ((text (subseq string start i)))
                     (cl:if float
                            (let ((*read-default-float-format* 'double-float)
                                  (*read-eval* nil))
                              (coerce (read-from-string text) 'double-float))
                            (parse-integer text))))))
             (val ()
               (ws)
               (let ((c (peek)))
                 (prog1
                     (cond ((null c) (fail "unexpected end"))
                           ((char= c #\{)
                            (incf i) (ws)
                            (cl:if (eql (peek) #\})
                                   (progn (incf i) (list :obj))
                                   (let ((pairs nil))
                                     (loop
                                       (ws)
                                       (let ((k (str)))
                                         (ws) (expect #\:)
                                         (push (cons k (val)) pairs))
                                       (ws)
                                       (cond ((eql (peek) #\,) (incf i))
                                             ((eql (peek) #\}) (incf i) (return))
                                             (t (fail "expected , or }"))))
                                     (cons :obj (nreverse pairs)))))
                           ((char= c #\[)
                            (incf i) (ws)
                            (cl:if (eql (peek) #\])
                                   (progn (incf i) nil)
                                   (let ((items nil))
                                     (loop
                                       (push (val) items)
                                       (ws)
                                       (cond ((eql (peek) #\,) (incf i))
                                             ((eql (peek) #\]) (incf i) (return))
                                             (t (fail "expected , or ]"))))
                                     (nreverse items))))
                           ((char= c #\") (str))
                           ((char= c #\t) (lit "true" :true))
                           ((char= c #\f) (lit "false" :false))
                           ((char= c #\n) (lit "null" :null))
                           (t (num)))
                   (ws)))))
      (prog1 (val)
        (unless (= i n) (fail "trailing characters"))))))

(defun jobj-p (x) (and (consp x) (eq (car x) :obj)))
(defun jget (obj key) (cdr (assoc key (cdr obj) :test #'string=)))
(defun jhas (obj key) (and (assoc key (cdr obj) :test #'string=) t))

(defun split-lines (text)
  (with-input-from-string (in text)
    (loop for line = (read-line in nil) while line collect line)))

;;; --- 1. oracle mode is on -------------------------------------------------------------
(check cl-user::*numbo-load-failures* nil)
(check (and (sym "*ORACLE*") (symbol-value (sym "*ORACLE*"))) t)
(check (eq (sym "RANDOM") 'cl:random) nil)
(check (eq (sym "FLOAT") 'cl:float) nil)
(check (eq (sym "SQRT") 'cl:sqrt) nil)
(check *read-default-float-format* 'single-float)  ; only bound while loading
(let ((*standard-output* (make-broadcast-stream))) (call "INIT-CHIFFRE"))
(dolist (v '("%FIRST-DECAY-RATE%" "%INITIAL-ACTIVATION%" "%K%"
             "%MIN-ACTIVATION-TO-BE-ADDED%" "%MAX-ACTIVATION-TO-BE-TRANSMITTED%"))
  (check (type-of (symbol-value (sym v))) 'double-float))
(check (type-of (call "FLOAT" 1)) 'double-float)
(check (call "SQRT" 16) 4.0d0)
(check (type-of (call "SQRT" 2)) 'double-float)
(check (type-of (call "QUOTIENT" (call "FLOAT" 1) 1000)) 'double-float)
(check (call "QUOTIENT" 7 2) 3)                    ; still truncating on integers
(check (call "QUOTIENT" -7 2) -3)
;; the pnode :link-length arithmetic, (quotient (float 1) ...), is double
(call "INITIALIZE-PNET")
(let ((p (symbol-value (sym "NODE-30"))))
  (check (type-of (call "SEND" p :link-length)) 'double-float))

;;; --- 2. the shared RNG ------------------------------------------------------------------
(defun first-outputs (seed k)
  (call "ORACLE-SEED" seed)
  (loop repeat k collect (call "ORACLE-NEXT-U64")))

(check (first-outputs 0 3) '(#xE220A8397B1DCDAF #x6E789E6AA1B965F4 #x06C45D188009454F))
(check (first-outputs 1 3) '(#x910A2DEC89025CC1 #xBEEB8DA1658EEC67 #xF893A2EEFB32555E))
(check (first-outputs 18 3) '(#x1120B3D00955F032 #xB6D46A242257C018 #x6E372283ACB06862))
(check (progn (call "ORACLE-SEED" (+ (expt 2 64) 5)) (call "ORACLE-NEXT-U64"))
       (car (first-outputs 5 1)))                  ; the seed is taken mod 2^64
;; (random n) = x mod n for the first draw x below 2^64 - (2^64 mod n)
(check (progn (call "ORACLE-SEED" 0) (call "RANDOM" 10)) (mod #xE220A8397B1DCDAF 10))
(check (progn (call "ORACLE-SEED" 0) (loop repeat 50 collect (call "RANDOM" 1)))
       (make-list 50 :initial-element 0))
(check (progn (call "ORACLE-SEED" 1) (every (lambda (x) (<= 0 x 6))
                                            (loop repeat 1000 collect (call "RANDOM" 7))))
       t)
;; n = 2^63 + 1 rejects every draw >= 2^63 + 1: seed 0's first draw is rejected
(check (let ((n (1+ (expt 2 63))))
         (call "ORACLE-SEED" 0)
         (list (call "RANDOM" n) (symbol-value (sym "*ORACLE-RNG-DRAWS*"))))
       (list (mod #x6E789E6AA1B965F4 (1+ (expt 2 63))) 2))
(check (handler-case (progn (call "RANDOM" 0) :no-error) (error () :error)) :error)
(check (handler-case (progn (call "RANDOM" -3) :no-error) (error () :error)) :error)
(check (handler-case (progn (call "RANDOM" 2.5d0) :no-error) (error () :error)) :error)
;; the fixture is what the oracle generates now
(let* ((path (merge-pathnames "../python/fixtures/rng_vectors.json" *repo*))
       (on-disk (and (probe-file path)
                     (with-open-file (in path)
                       (let ((s (make-string (file-length in))))
                         (subseq s 0 (read-sequence s in)))))))
  (check (and on-disk (string= on-disk (call "ORACLE-RNG-VECTORS-JSON"))) t)
  (let ((j (and on-disk (ignore-errors (json-parse on-disk)))))
    (check (jobj-p j) t)
    (when (jobj-p j)
      (check (jget j "algorithm") "splitmix64")
      (check (sort (mapcar #'car (cdr (jget j "outputs"))) #'string<) '("0" "1" "18"))
      (check (subseq (jget (jget j "outputs") "0") 0 3)
             '(#xE220A8397B1DCDAF #x6E789E6AA1B965F4 #x06C45D188009454F))
      (check (every (lambda (k) (= 20 (length (jget (jget j "outputs") k)))) '("0" "1" "18")) t)
      (check (>= (length (jget j "random")) 3) t))))

;;; --- 3. copying sortcar -----------------------------------------------------------------
(let* ((l (list (list (sym "B") 1) (list (sym "A") 2) (list (sym "C") 3)))
       (copy (copy-tree l))
       (sorted (call "SORTCAR" l nil)))
  (check (mapcar #'car sorted) (list (sym "A") (sym "B") (sym "C")))
  (check (equal l copy) t))                        ; the argument is untouched

;;; --- 4-5. chapter puzzles in oracle mode --------------------------------------------
(defparameter *puzzles*
  '((114 11 20 7 1 6) (87 8 3 9 10 7) (31 3 5 24 3 14) (25 8 5 5 11 2)
    (102 6 17 2 4 1) (146 12 2 5 7 18) (6 3 3 17 11 22) (11 2 5 1 25 23)
    (116 20 2 16 14 6) (127 6 4 22 5 7) (41 5 16 22 25 1)))
(defparameter *cap* 3000)
(defparameter *event-types*
  '("start" "setup-choose" "iteration" "post" "node-created" "node-killed" "pnet"
    "rack-emptied" "rng" "done" "gave-up" "capped" "error"))

(defun oracle-run (problem seed &rest keys)
  "Run PROBLEM in oracle mode; return (values plist trace-string output-string)."
  (let* (result
         (trace (make-string-output-stream))
         (output (with-output-to-string (*standard-output*)
                   (setq result (apply (fn "ORACLE-RUN-CONFIG") problem
                                       :seed seed :max-iterations *cap*
                                       :trace trace keys)))))
    (values result (get-output-stream-string trace) output)))

(defun check-trace (problem seed result trace output)
  "Structural checks on one run's trace.  Returns the parsed events."
  (let* ((lines (split-lines trace))
         (events (mapcar (lambda (l) (handler-case (json-parse l) (error () :bad))) lines))
         (tag (format nil "~a seed ~a" problem seed)))
    (flet ((ok (what x) (check (list tag what (and x t)) (list tag what t))))
      (ok "lines parse as JSON objects" (and events (every #'jobj-p events)))
      (unless (every #'jobj-p events) (return-from check-trace nil))
      (ok "known event types" (every (lambda (e) (member (jget e "ev") *event-types* :test #'equal))
                                     events))
      (ok "first event is start" (equal (jget (first events) "ev") "start"))
      (ok "start event" (and (equal (jget (first events) "problem") problem)
                             (equal (jget (first events) "seed") seed)
                             (= 88 (length (jget (first events) "pnet")))))
      (let* ((last (car (last events)))
             (outcome (getf result :outcome))
             (its (remove "iteration" events :key (lambda (e) (jget e "ev")) :test-not #'equal)))
        (ok "no error" (not (eq outcome :error)))
        (ok "last event = outcome"
            (equal (jget last "ev") (case outcome (:solved "done") (t (string-downcase (symbol-name outcome))))))
        (ok "last event iterations" (eql (jget last "iterations") (getf result :iterations)))
        (ok "one end event" (= 1 (count-if (lambda (e) (member (jget e "ev") '("done" "gave-up" "capped" "error")
                                                               :test #'equal))
                                           events)))
        (ok "iteration events 0..k-1" (equal (mapcar (lambda (e) (jget e "n")) its)
                                             (loop for k below (getf result :iterations) collect k)))
        ;; config's set-up phase makes exactly 13 choices, then the loop starts
        ;; (seed 14 has a 13th codelet that calls MOD, which once confused this)
        (ok "13 setup chooses, all before the first iteration"
            (let ((first-it (position "iteration" events :key (lambda (e) (jget e "ev")) :test #'equal))
                  (setups (loop for e in events for k from 0
                                when (equal (jget e "ev") "setup-choose") collect k)))
              (cl:if first-it
                     (and (= 13 (length setups)) (< (car (last setups)) first-it))
                     (<= (length setups) 13))))
        (ok "iteration fields" (every (lambda (e) (every (lambda (k) (jhas e k))
                                                         '("n" "x" "temperature" "rack" "codelet"
                                                           "args" "urgency")))
                                      its))
        (ok "x = n + 12 until the first hot reset (x := 39)"
            (let ((e (find-if (lambda (e) (/= (jget e "x") (+ 12 (jget e "n")))) its)))
              (or (null e) (= (jget e "x") 40))))
        (ok "rack has the 7 levels" (every (lambda (e) (equal (mapcar #'car (jget e "rack"))
                                                              '(600 300 150 7 4 1 0)))
                                           its))
        (ok "pnet events have 88 doubles"
            (every (lambda (e) (or (not (equal (jget e "ev") "pnet"))
                                   (and (= 88 (length (jget e "act")))
                                        (every (lambda (a) (typep a 'double-float)) (jget e "act")))))
                   events))
        (ok "at least one pnet event" (find "pnet" events :key (lambda (e) (jget e "ev")) :test #'equal))
;; a puzzle can be solved ("Obvious.") before every brick is read
        (ok "target and first brick created"
            (subsetp '("CYTO-TARGET" "CYTO-BRICK1")
                     (loop for e in events when (equal (jget e "ev") "node-created")
                           collect (jget e "name"))
                     :test #'equal))
        (ok "node events match the printed Node lines"
            (equal (loop for e in events
                         when (member (jget e "ev") '("node-created" "node-killed") :test #'equal)
                           collect (format nil "Node ~a ~a" (jget e "name")
                                           (subseq (jget e "ev") 5)))
                   (remove-if-not (lambda (l) (and (> (length l) 5) (string= "Node " l :end2 5)))
                                  (split-lines output))))
        (ok "post events have codelet, args, urgency"
            (every (lambda (e) (or (not (equal (jget e "ev") "post"))
                                   (and (stringp (jget e "codelet")) (listp (jget e "args"))
                                        (member (jget e "urgency") '(600 300 150 7 4 1 0)))))
                   events))
        (when (eq outcome :solved)
          (let ((d (jget last "decomposition")))
            (ok "decomposition" (and d (every (lambda (o) (every (lambda (k) (jhas o k))
                                                                 '("op" "a" "va" "b" "vb" "result")))
                                              d)))
            ;; or, after "Obvious.", the block equal to the target
            (ok "decomposition root is the target"
                (let ((root (jget (first d) "result")))
                  (or (equal root "CYTO-TARGET")
                      (let ((prefix (format nil "CYTO-BLOCK~d-V" (car problem))))
                        (and (> (length root) (length prefix))
                             (string= prefix root :end2 (length prefix))
                             (search "Obvious." output))))))
            (ok "one decomposition entry per printed Operation"
                (= (length d) (count "Operation" (split-lines output)
                                     :test (lambda (w l) (search w l)))))))
        events))))

(defvar *python-ok* (ignore-errors (zerop (sb-ext:process-exit-code
                                          (sb-ext:run-program "python3" '("--version")
                                                              :search t :output nil)))))

(defun python-validates (trace)
  "python3's json module parses every line of TRACE as an object."
  (let ((path (format nil "/tmp/numbo-oracle-trace-~d.jsonl" (sb-posix:getpid))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string trace out))
    (prog1 (zerop (sb-ext:process-exit-code
                   (sb-ext:run-program
                    "python3"
                    (list "-c" "import json,sys
n=0
for line in open(sys.argv[1]):
    assert isinstance(json.loads(line), dict); n+=1
assert n>0" path)
                    :search t :output nil :error nil)))
      (delete-file path))))

(let ((outcomes nil))
  (dolist (seed '(1 2 14))
    (dolist (p *puzzles*)
      (multiple-value-bind (result trace output) (oracle-run p seed :float-check t)
        (push (getf result :outcome) outcomes)
        (check (list p seed (getf result :outcome) (getf result :error))
               (list p seed (getf result :outcome) nil))
        (check-trace p seed result trace output)
        ;; 5. no single float anywhere in the world, at any iteration
        (check (list p seed :singles (getf result :single-floats))
               (list p seed :singles nil))
        (check (list p seed :doubles-seen (> (getf result :doubles-seen 0) 0))
               (list p seed :doubles-seen t))
        (when (and *python-ok* (= seed 1))
          (check (list p seed :python-json (python-validates trace))
                 (list p seed :python-json t))))))
  ;; the chapter's easy puzzles are still solved
  (check (and (member :solved outcomes) t) t))

;; file output: :trace may be a pathname
(let ((path (format nil "/tmp/numbo-oracle-file-~d.jsonl" (sb-posix:getpid))))
  (let ((*standard-output* (make-broadcast-stream)))
    (call "ORACLE-RUN-CONFIG" '(6 3 3 17 11 22) :seed 1 :max-iterations 200 :trace path))
  (check (let ((lines (with-open-file (in path) (loop for l = (read-line in nil) while l collect l))))
           (and lines (every (lambda (l) (jobj-p (json-parse l))) lines)
                (equal (jget (json-parse (car (last lines))) "ev") "done")))
         t)
  (delete-file path))

;; rng events, when asked for, show every draw
(multiple-value-bind (result trace) (oracle-run '(114 11 20 7 1 6) 1 :rng-events t)
  (declare (ignore result))
  (let ((rng (remove "rng" (mapcar #'json-parse (split-lines trace))
                     :key (lambda (e) (jget e "ev")) :test-not #'equal)))
    (check (and rng (every (lambda (e) (< -1 (jget e "value") (jget e "n"))) rng)) t)))

;; control: the walk finds a single float hidden in a global
(let ((s (sym "*ORACLE-TEST-PROBE*")))
  (check (null s) nil)
  (when s
    (setf (symbol-value s) (list 1 (list 2.5f0)))
    (check (length (call "ORACLE-FIND-SINGLE-FLOATS")) 1)
    (setf (symbol-value s) nil)
    (check (call "ORACLE-FIND-SINGLE-FLOATS") nil)))
;; control: the trace writer refuses a single float
(check (handler-case (progn (call "ORACLE-JSON-STRING" (list 1 2.5f0)) :no-error) (error () :error))
       :error)
(check (call "ORACLE-JSON-STRING" (list 1 2.5d0 "free" (sym "CYTO-TARGET") nil t))
       "[1,2.5,{\"str\":\"free\"},\"CYTO-TARGET\",null,true]")

;;; --- 6. determinism ---------------------------------------------------------------------
;; the hooks only observe: the printed run is the same with no trace at all
;; (puzzle 3 runs past x = 400, where config itself calls temperature)
(dolist (case '(((31 3 5 24 3 14) 1) ((41 5 16 22 25 1) 14) ((102 6 17 2 4 1) 14)))
  (destructuring-bind (p seed) case
    (let* ((quiet (with-output-to-string (*standard-output*)
                    (call "ORACLE-RUN-CONFIG" p :seed seed :max-iterations *cap* :trace nil)))
           (traced (nth-value 2 (oracle-run p seed :rng-events t :float-check t))))
      (check (list p seed :same-output (string= quiet traced)) (list p seed :same-output t)))))

(multiple-value-bind (r1 t1) (oracle-run '(114 11 20 7 1 6) 1)
  (declare (ignore r1))
  (setq *random-state* (make-random-state t))      ; CL's generator must not matter
  (multiple-value-bind (r2 t2) (oracle-run '(114 11 20 7 1 6) 1)
    (declare (ignore r2))
    (check (string= t1 t2) t))
  (multiple-value-bind (r3 t3) (oracle-run '(114 11 20 7 1 6) 2)
    (declare (ignore r3))
    (check (string= t1 t3) nil)))

(format t "~&oracle tests: ~d checks, ~d failures~%" *checks* *failures*)
(sb-ext:exit :code (if (zerop *failures*) 0 1))

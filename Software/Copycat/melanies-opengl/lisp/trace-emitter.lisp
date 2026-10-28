;;; trace-emitter.lisp -- a JSON-lines observer for Melanie Mitchell's Copycat.
;;;
;;; Loaded after ../../melanies-copycat/load.lisp.  It hooks the run loop of
;;; Mitchell's program from the outside and writes one JSON object per
;;; codelet to a stream (stdout, normally), so that another program can draw
;;; the run as it happens.  Nothing in Mitchell's files (melanies-copycat/src/)
;;; is touched: the two hooks are
;;;
;;;   - the Flavors method (codelet :run), rewrapped through the compatibility
;;;     layer's method table so the codelet that just ran is remembered;
;;;   - the functions STEP-CCAT and RUN-CCAT of run.l, wrapped with
;;;     SB-INT:ENCAPSULATE (as harness.lisp already wraps init-constants).
;;;
;;; Every line is a complete snapshot: the strings and their letters, the
;;; bonds, groups and correspondences (built and proposed), the rule and the
;;; translated rule, the temperature, the slipnet's activations and the
;;; coderack's contents.  The first line ("k":"init") also carries the
;;; problem, the seed, %time-step-length% and the slipnet's node names; the
;;; last ("k":"final") the answer.  docs/trace-format.md documents the schema.
;;;
;;; The JSON is written by hand (no library): it is flat and small.
;;;
;;;   (copycat::mo-run "abc" "abd" "ijk" :seed 1 :every 1)

(cl:in-package :copycat)

(defvar *mo-out* nil "The stream the JSON lines go to.")
(defvar *mo-every* 1 "Emit a line every this many codelets (init and final always).")
(defvar *mo-last-codelet* nil "The codelet whose :run method ran last.")
(defvar *mo-installed* nil)

;;; ---------------------------------------------------------------------------
;;; JSON primitives

(defun mo-str (x s)
  "Write X (a string or symbol) as a JSON string."
  (let ((str (if (symbolp x) (string-downcase (symbol-name x)) (string x))))
    (write-char #\" s)
    (cl:loop for ch across str do
      (case ch
        (#\" (write-string "\\\"" s))
        (#\\ (write-string "\\\\" s))
        (#\Newline (write-string "\\n" s))
        (#\Tab (write-string "\\t" s))
        (t (if (< (char-code ch) 32)
               (format s "\\u~4,'0X" (char-code ch))
               (write-char ch s)))))
    (write-char #\" s)))

(defun mo-num (x s)
  "Write a number: integers as such, ratios and floats with two decimals."
  (cond ((integerp x) (format s "~D" x))
        ((null x) (write-string "null" s))
        (t (let ((f (float x 1.0d0)))
             (if (= f (ffloor f))
                 (format s "~D" (round f))
                 (format s "~,2F" f))))))

(defun mo-bool (x s) (write-string (if x "true" "false") s))

(defun mo-str-or-null (x s) (if x (mo-str x s) (write-string "null" s)))

(defun mo-pname-or-null (node s)
  (if node (mo-str (send node :pname) s) (write-string "null" s)))

;;; ---------------------------------------------------------------------------
;;; The workspace

(defun mo-string-key (string)
  "A one-letter key for one of the four workspace strings."
  (cond ((eq string *initial-string*) "i")
        ((eq string *modified-string*) "m")
        ((eq string *target-string*) "t")
        ((eq string *answer-string*) "a")
        (t "?")))

(defun mo-ref (obj s)
  "An object reference: [string-key, left-position, right-position, \"g\"|\"l\"]."
  (write-char #\[ s)
  (mo-str (mo-string-key (send obj :string)) s)
  (format s ",~D,~D," (send obj :left-string-position)
          (send obj :right-string-position))
  (mo-str (if (typep obj 'group) "g" "l") s)
  (write-char #\] s))

(defun mo-letters (string s)
  "The letters of STRING: [{\"p\":name,\"pos\":n,\"ch\":bool,\"d\":[[type,descriptor]...]}...]."
  (if (null string)
      (write-string "null" s)
      (let ((first t))
        (write-char #\[ s)
        (dolist (letter (send string :letter-list))
          (unless first (write-char #\, s))
          (setq first nil)
          (write-string "{\"p\":" s)
          (mo-str (send letter :pname) s)
          (format s ",\"pos\":~D,\"ch\":" (send letter :left-string-position))
          (mo-bool (send letter :changed?) s)
          (write-string ",\"d\":[" s)
          (let ((firstd t))
            (dolist (d (send letter :descriptions))
              (unless firstd (write-char #\, s))
              (setq firstd nil)
              (write-char #\[ s)
              (mo-pname-or-null (send d :description-type) s)
              (write-char #\, s)
              (mo-pname-or-null (send d :descriptor) s)
              (write-char #\] s)))
          (write-string "]}" s))
        (write-char #\] s))))

(defun mo-bond (bond s)
  (write-string "{\"s\":" s)
  (mo-str (mo-string-key (send bond :string)) s)
  (format s ",\"from\":~D,\"to\":~D,\"lvl\":~D,\"cat\":"
          (send (send bond :from-obj) :left-string-position)
          (send (send bond :to-obj) :left-string-position)
          (send bond :proposal-level))
  (mo-pname-or-null (send bond :bond-category) s)
  (write-string ",\"dir\":" s)
  (mo-pname-or-null (send bond :direction-category) s)
  (write-string ",\"lo\":" s) (mo-ref (send bond :left-obj) s)
  (write-string ",\"ro\":" s) (mo-ref (send bond :right-obj) s)
  (write-char #\} s))

(defun mo-group (group s)
  (write-string "{\"s\":" s)
  (mo-str (mo-string-key (send group :string)) s)
  (format s ",\"l\":~D,\"r\":~D,\"lvl\":~D,\"cat\":"
          (send group :left-string-position)
          (send group :right-string-position)
          (send group :proposal-level))
  (mo-pname-or-null (send group :group-category) s)
  (write-string ",\"dir\":" s)
  (mo-pname-or-null (send group :direction-category) s)
  (write-string ",\"objs\":[" s)
  (let ((first t))
    (dolist (obj (send group :object-list))
      (unless first (write-char #\, s))
      (setq first nil)
      (mo-ref obj s)))
  (write-string "]}" s))

(defun mo-correspondence (c s)
  (write-string "{\"o1\":" s) (mo-ref (send c :obj1) s)
  (write-string ",\"o2\":" s) (mo-ref (send c :obj2) s)
  (format s ",\"lvl\":~D,\"cms\":[" (send c :proposal-level))
  (let ((first t))
    (dolist (cm (send c :concept-mapping-list))
      (unless first (write-char #\, s))
      (setq first nil)
      (write-char #\[ s)
      (mo-pname-or-null (send cm :descriptor1) s) (write-char #\, s)
      (mo-pname-or-null (send cm :descriptor2) s) (write-char #\, s)
      (mo-bool (send cm :distinguishing?) s)
      (write-char #\] s)))
  (write-string "]}" s))

(defun mo-list (items writer s)
  (write-char #\[ s)
  (let ((first t))
    (dolist (item items)
      (unless first (write-char #\, s))
      (setq first nil)
      (cl:funcall writer item s)))
  (write-char #\] s))

(defun mo-rule-text (rule)
  "Mitchell's (rule :print) output, as a string, or NIL."
  (and rule
       (string-right-trim '(#\Newline #\Space #\Return)
                          (with-output-to-string (*standard-output*)
                            (send rule :print)))))

;;; ---------------------------------------------------------------------------
;;; The slipnet and the coderack

(defun mo-nodes (s)
  "The slipnet's nodes, in *slipnet* order: [[pname, cm-name], ...]."
  (mo-list *slipnet*
           (lambda (node s)
             (write-char #\[ s)
             (mo-str (send node :pname) s) (write-char #\, s)
             (mo-str-or-null (send node :cm-name) s)
             (write-char #\] s))
           s))

(defun mo-activations (s)
  (mo-list *slipnet* (lambda (node s) (mo-num (send node :activation) s)) s))

(defun mo-clamps (s)
  (mo-list *slipnet* (lambda (node s) (mo-bool (send node :clamp) s)) s))

(defun mo-rack (s)
  "The coderack's codelets, counted by type: {\"bond-builder\":3,...}."
  (let ((counts '()))
    (dolist (codelet (send *coderack* :codelet-list))
      (let* ((name (send codelet :codelet-type))
             (cell (assoc name counts)))
        (if cell (incf (cdr cell)) (push (cons name 1) counts))))
    (write-char #\{ s)
    (let ((first t))
      (dolist (cell counts)
        (unless first (write-char #\, s))
        (setq first nil)
        (mo-str (car cell) s)
        (format s ":~D" (cdr cell))))
    (write-char #\} s)))

;;; ---------------------------------------------------------------------------
;;; One line

(defun mo-emit (kind &key problem seed)
  "Write one JSON line of KIND (\"init\", \"step\" or \"final\")."
  (let ((s *mo-out*))
    (write-string "{\"k\":" s) (mo-str kind s)
    (format s ",\"n\":~D,\"temp\":" *codelet-count*)
    (mo-num *temperature* s)
    (write-string ",\"codelet\":" s)
    (mo-str-or-null (and *mo-last-codelet* (send *mo-last-codelet* :codelet-type)) s)
    (write-string ",\"snag\":" s) (mo-bool *snag-condition* s)
    (format s ",\"snags\":~D,\"found\":" *snag-count*)
    (mo-bool *found-answer* s)
    (when (string= kind "init")
      (write-string ",\"problem\":" s)
      (mo-list problem #'mo-str s)
      (write-string ",\"seed\":" s) (mo-num seed s)
      (format s ",\"timestep\":~D,\"nodes\":" %time-step-length%)
      (mo-nodes s))
    (write-string ",\"strings\":{\"i\":" s) (mo-letters *initial-string* s)
    (write-string ",\"m\":" s) (mo-letters *modified-string* s)
    (write-string ",\"t\":" s) (mo-letters *target-string* s)
    (write-string ",\"a\":" s) (mo-letters (and *found-answer* *answer-string*) s)
    (write-string "},\"bonds\":" s)
    (mo-list (append (send *workspace* :bond-list)
                     (send *workspace* :proposed-bond-list))
             #'mo-bond s)
    (write-string ",\"groups\":" s)
    (mo-list (append (send *workspace* :group-list)
                     (send *workspace* :proposed-group-list))
             #'mo-group s)
    (write-string ",\"corrs\":" s)
    (mo-list (append (send *workspace* :correspondence-list)
                     (send *workspace* :proposed-correspondence-list))
             #'mo-correspondence s)
    (write-string ",\"rule\":" s) (mo-str-or-null (mo-rule-text *rule*) s)
    (write-string ",\"trule\":" s) (mo-str-or-null (mo-rule-text *translated-rule*) s)
    (write-string ",\"act\":" s) (mo-activations s)
    (write-string ",\"clamp\":" s) (mo-clamps s)
    (write-string ",\"rack\":" s) (mo-rack s)
    (write-char #\} s)
    (terpri s)
    (when (string/= kind "step") (finish-output s))))

;;; ---------------------------------------------------------------------------
;;; The hooks

(defvar *mo-problem* nil)
(defvar *mo-seed* nil)

(defun mo-after-step ()
  (when (or (= *mo-every* 1) (zerop (mod *codelet-count* *mo-every*)))
    (mo-emit "step")))

(defun mo-install ()
  "Wrap (codelet :run), STEP-CCAT and RUN-CCAT.  Idempotent."
  (unless *mo-installed*
    (let ((original-run (gethash :run (flavor-method-table 'codelet))))
      (define-flavor-method 'codelet :run
        (lambda (self)
          (setq *mo-last-codelet* self)
          (cl:funcall original-run self))))
    (sb-int:encapsulate 'step-ccat 'mo-trace
                        (lambda (fn) (cl:funcall fn) (mo-after-step)))
    (sb-int:encapsulate 'run-ccat 'mo-trace
                        (lambda (fn)
                          (setq *mo-last-codelet* nil)
                          (mo-emit "init" :problem *mo-problem* :seed *mo-seed*)
                          (cl:funcall fn)
                          (mo-emit "final")))
    (setq *mo-installed* t)))

(defun mo-run (initial modified target &key seed (every 1) (out *standard-output*))
  "Run Mitchell's Copycat on INITIAL -> MODIFIED :: TARGET -> ? with SEED,
writing the JSON trace to OUT (stdout by default) and nothing else there.
Mitchell's own printing, if any, goes to *error-output*."
  (setq *mo-out* out
        *mo-every* (max 1 every)
        *mo-problem* (list (string-downcase (string initial))
                           (string-downcase (string modified))
                           (string-downcase (string target)))
        *mo-seed* seed)
  (mo-install)
  (let ((result (let ((*standard-output* *error-output*))
                  (run initial modified target :seed seed :stream nil))))
    (finish-output out)
    result))

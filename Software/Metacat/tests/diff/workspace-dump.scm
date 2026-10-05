;;; workspace-dump.scm -- the initial workspace of a problem, as data (item 06).
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Loaded (with `load', the repository as current directory) by
;;; tests/diff/workspace-battery.scm under both runners, and by
;;; chez_scheme/oracle/tests/workspace-init-check.ss, which checks that
;;; b:init-problem builds the same workspace as the original's own init-mcat.
;;;
;;; b:init-problem does what run.ss's init-mcat does to the Workspace (run.ss
;;; is not ported yet): reset the slipnet, make the strings, give the letters
;;; their string-position descriptions, activate their descriptors fully, and
;;; compute the workspace values (update-workspace-values).  The procedures
;;; below are copies of init-workspace, add-string-position-descriptions-to-
;;; letters and update-workspace-values, under b: names so that they do not
;;; replace the original's under Chez.  The globals are set with b:set-global!.

;; problems.txt, as a list of (strings seeds): strings are symbols, 3 or 4
(define b:read-line
  (lambda (port)
    (let loop ((acc '()))
      (let ((c (read-char port)))
        (cond
          ((eof-object? c) (if (null? acc) c (list->string (reverse acc))))
          ((char=? c #\newline) (list->string (reverse acc)))
          (else (loop (cons c acc))))))))

(define b:split
  (lambda (s sep)
    (let loop ((cs (string->list s)) (word '()) (words '()))
      (cond
        ((null? cs)
         (reverse (if (null? word) words (cons (list->string (reverse word)) words))))
        ((char=? (car cs) sep)
         (loop (cdr cs) '()
               (if (null? word) words (cons (list->string (reverse word)) words))))
        (else (loop (cdr cs) (cons (car cs) word) words))))))

(define b:strip-comment
  (lambda (s)
    (let loop ((cs (string->list s)) (acc '()))
      (if (or (null? cs) (char=? (car cs) #\#))
          (list->string (reverse acc))
          (loop (cdr cs) (cons (car cs) acc))))))

(define b:problems
  (call-with-input-file "tests/problems.txt"
    (lambda (port)
      (let loop ((acc '()))
        (let ((line (b:read-line port)))
          (if (eof-object? line)
              (reverse acc)
              (let ((fields (b:split (b:strip-comment line) #\|)))
                (if (< (length fields) 3)
                    (loop acc)
                    (loop (cons (list (map string->symbol (b:split (car fields) #\space))
                                      (map string->number (b:split (cadr fields) #\space)))
                                acc))))))))))

;;---------------------------------------------------------------------------
;; Initialising a problem's workspace

(define b:add-string-position-descriptions-to-letters
  (lambda (string)
    (let ((string-length (tell string 'get-length))
          (leftmost-letter (tell string 'get-letter 0)))
      (if (= string-length 1)
        (tell leftmost-letter 'new-description
          plato-string-position-category plato-single)
        (let ((rightmost-letter (tell string 'get-letter (sub1 string-length))))
          (tell leftmost-letter 'new-description
            plato-string-position-category plato-leftmost)
          (tell rightmost-letter 'new-description
            plato-string-position-category plato-rightmost)
          (if (odd? string-length)
            (let ((middle-letter
                    (tell string 'get-letter (truncate (/ string-length 2)))))
              (tell middle-letter 'new-description
                plato-string-position-category plato-middle))))))))

(define b:update-workspace-values
  (lambda ()
    (for-each (lambda (structure) (tell structure 'update-strength))
      (tell *workspace* 'get-structures))
    (let ((objects (tell *workspace* 'get-objects)))
      (for-each (lambda (object) (tell object 'update-raw-importance)) objects)
      (tell *initial-string* 'update-all-relative-importances)
      (tell *modified-string* 'update-all-relative-importances)
      (tell *target-string* 'update-all-relative-importances)
      (if %justify-mode%
        (tell *answer-string* 'update-all-relative-importances))
      (for-each (lambda (object) (tell object 'update-object-values)) objects))
    (tell *initial-string* 'update-average-intra-string-unhappiness)
    (tell *modified-string* 'update-average-intra-string-unhappiness)
    (tell *target-string* 'update-average-intra-string-unhappiness)
    (if %justify-mode%
      (tell *answer-string* 'update-average-intra-string-unhappiness))
    (tell *workspace* 'update-average-unhappiness-values)))

;; strings: a list of 3 or 4 symbols
(define b:init-problem
  (lambda (strings seed)
    (let* ((answer-sym (if (= (length strings) 4) (cadddr strings) #f)))
      (b:set-global! '%justify-mode% (and answer-sym #t))
      (random-seed seed)
      (b:set-global! '*codelet-count* 0)
      (b:set-global! '*temperature* 100)
      (b:set-global! '*temperature-clamped?* #f)
      (for-each (lambda (node) (tell node 'reset)) *slipnet-nodes*)
      ;; init-workspace (in Chez's order: let evaluates its inits last first)
      (let* ((answer-string (if answer-sym (make-workspace-string 'answer answer-sym) #f))
             (target-string (make-workspace-string 'target (caddr strings)))
             (modified-string (make-workspace-string 'modified (cadr strings)))
             (initial-string (make-workspace-string 'initial (car strings))))
        (tell *workspace* 'initialize
          initial-string modified-string target-string answer-string)
        (b:set-global! '*initial-string* initial-string)
        (b:set-global! '*modified-string* modified-string)
        (b:set-global! '*target-string* target-string)
        (b:set-global! '*answer-string* answer-string)
        (b:set-global! '*top-strings* (list initial-string modified-string))
        (b:set-global! '*bottom-strings* (list target-string answer-string))
        (b:set-global! '*vertical-strings* (list initial-string target-string))
        (b:set-global! '*non-answer-strings*
          (list initial-string modified-string target-string))
        (b:set-global! '*all-strings*
          (list initial-string modified-string target-string answer-string)))
      (b:add-string-position-descriptions-to-letters *initial-string*)
      (b:add-string-position-descriptions-to-letters *modified-string*)
      (b:add-string-position-descriptions-to-letters *target-string*)
      (if %justify-mode%
        (b:add-string-position-descriptions-to-letters *answer-string*))
      (if (or (= (tell *initial-string* 'get-length) 1)
              (= (tell *modified-string* 'get-length) 1)
              (= (tell *target-string* 'get-length) 1)
              (and %justify-mode% (= (tell *answer-string* 'get-length) 1)))
        (tell plato-object-category 'set-activation %max-activation%))
      (for-each
        (lambda (obj)
          (for-each (lambda (descriptor)
                      (tell descriptor 'set-activation %max-activation%))
            (tell-all (tell obj 'get-descriptions) 'get-descriptor)))
        (tell *workspace* 'get-objects))
      (b:update-workspace-values)
      'done)))

;;---------------------------------------------------------------------------
;; The workspace's stored state, as data (no live queries, so that the dump
;; can be compared after init-mcat, which also clamps slipnodes)

(define b:nm (lambda (node) (if node (tell node 'get-name-symbol) #f)))
(define b:deep-nm
  (lambda (x)
    (cond
      ((pair? x) (let* ((a (b:deep-nm (car x))) (d (b:deep-nm (cdr x)))) (cons a d)))
      ((procedure? x) (b:nm x))
      (else x))))

(define b:dump-description
  (lambda (d)
    (list (b:nm (tell d 'get-description-type))
          (b:nm (tell d 'get-descriptor))
          (tell d 'print-name)
          (tell d 'get-proposal-level)
          (tell d 'get-strength)
          (tell d 'get-time-stamp))))

(define b:dump-object
  (lambda (obj)
    (list (tell obj 'object-type)
          (tell obj 'ascii-name)
          (tell obj 'print-name)
          (tell obj 'get-id-num)
          (tell obj 'get-left-string-pos)
          (tell obj 'get-right-string-pos)
          (tell obj 'which-string)
          (b:nm (tell obj 'get-letter-category))
          (map b:dump-description (tell obj 'get-descriptions))
          (list 'importance
                (tell obj 'get-raw-importance)
                (tell obj 'get-relative-importance))
          (list 'unhappiness
                (tell obj 'get-intra-string-unhappiness)
                (tell obj 'get-inter-string-unhappiness 'horizontal)
                (tell obj 'get-inter-string-unhappiness 'vertical)
                (tell obj 'get-average-unhappiness))
          (list 'salience
                (tell obj 'get-intra-string-salience)
                (tell obj 'get-inter-string-salience 'horizontal)
                (tell obj 'get-inter-string-salience 'vertical)
                (tell obj 'get-average-salience))
          (list 'bonds (tell obj 'get-left-bond) (tell obj 'get-right-bond)
                (tell obj 'get-enclosing-group)
                (tell obj 'get-bridge 'horizontal) (tell obj 'get-bridge 'vertical))
          (b:deep-nm (tell (tell obj 'get-image) 'generate)))))

(define b:dump-string
  (lambda (s)
    (if (not s)
        #f
        (list (tell s 'object-type)
              (tell s 'print-name)
              (tell s 'ascii-name)
              (tell s 'generic-name)
              (tell s 'symbol-name)
              (tell s 'get-string-type)
              (tell s 'get-length)
              (tell s 'get-max-object-capacity)
              (map b:nm (tell s 'get-letter-categories))
              (tell s 'get-average-intra-string-unhappiness)
              (length (tell s 'get-groups))
              (length (tell s 'get-all-bonds))
              (b:deep-nm (tell s 'generate-image-letters))
              (map b:dump-object (tell s 'get-objects))))))

(define b:dump-workspace
  (lambda ()
    (list (map b:dump-string *all-strings*)
          (list 'ids (map (lambda (o) (tell o 'get-id-num)) (tell *workspace* 'get-objects)))
          (list 'workspace
                (tell *workspace* 'get-average-intra-string-unhappiness)
                (tell *workspace* 'get-average-inter-string-unhappiness 'top)
                (tell *workspace* 'get-average-inter-string-unhappiness 'bottom)
                (tell *workspace* 'get-average-inter-string-unhappiness 'vertical)
                (tell *workspace* 'get-average-unhappiness)
                (tell *workspace* 'get-mapping-strength 'top)
                (tell *workspace* 'get-mapping-strength 'bottom)
                (tell *workspace* 'get-mapping-strength 'vertical)
                (length (tell *workspace* 'get-structures))
                (tell *workspace* 'get-possible-rule-types)
                (length (tell *workspace* 'get-all-proposed-bridges))))))

(define b:activations
  (lambda ()
    (map (lambda (n) (list (b:nm n) (tell n 'get-activation) (tell n 'frozen?)))
         *slipnet-nodes*)))

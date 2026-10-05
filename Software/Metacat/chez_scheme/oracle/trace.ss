;;; trace.ss -- the JSON-lines trace of a Metacat run (docs/trace-format.md).
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Load after prelude.ss, (load-metacat) and (install-headless-windows!);
;;; then (install-trace! port) before init-mcat.  Instrumentation is done
;;; entirely from outside the original: top-level procedures are wrapped
;;; with set! (callers reach them through their top-level bindings), the
;;; *coderack* object is wrapped to see each codelet it hands out, and the
;;; null Trace window is replaced by one that records the Temporal Trace's
;;; events.  Every wrapper only reads model state through side-effect-free
;;; getters and calls the original procedure with the original arguments,
;;; so a traced run is the same run (chez_scheme/oracle/tests/trace-check.ss
;;; checks this).  (random-seed) with no argument reads the generator state
;;; without changing it.

(define $trace-port #f)
(define $last-themes #f)

;;;---------------------------------------------------------------------------
;;; JSON output.  A value is written by its Scheme type:
;;;   #t/#f -> true/false, '() -> [], 'null -> null, string -> string,
;;;   symbol -> its name as a string, list -> array,
;;;   (json-object (key . value) ...) -> object, keys in the order given,
;;;   exact integer -> digits, other exact rational -> string "n/d",
;;;   finite flonum -> Chez's number->string (shortest round-trip form,
;;;   e.g. 21.0, 0.5, 1e-7, 1.2345678901234567e19), non-finite -> string.

(define json-object (lambda pairs (cons '$object pairs)))

(define json-string
  (lambda (s p)
    (write-char #\" p)
    (string-for-each
      (lambda (c)
        (cond
          ((char=? c #\") (put-string p "\\\""))
          ((char=? c #\\) (put-string p "\\\\"))
          ((char=? c #\newline) (put-string p "\\n"))
          ((char=? c #\tab) (put-string p "\\t"))
          ((char<? c #\space)
           (put-string p (format "\\u~4,'0x" (char->integer c))))
          (else (write-char c p))))
      s)
    (write-char #\" p)))

(define json-number
  (lambda (x p)
    (cond
      ((and (exact? x) (integer? x)) (put-string p (number->string x)))
      ((exact? x) (json-string (number->string x) p))
      ((and (flonum? x) (not (nan? x)) (not (infinite? x)))
       (let* ((s (number->string x))
              ;; Chez marks subnormals with a precision suffix, 5e-324|1
              (bar (let loop ((i 0))
                     (cond ((= i (string-length s)) #f)
                           ((char=? (string-ref s i) #\|) i)
                           (else (loop (+ i 1)))))))
         (put-string p (if bar (substring s 0 bar) s))))
      (else (json-string (number->string x) p)))))

(define json-write
  (lambda (v p)
    (cond
      ((eq? v #t) (put-string p "true"))
      ((eq? v #f) (put-string p "false"))
      ((eq? v 'null) (put-string p "null"))
      ((null? v) (put-string p "[]"))
      ((string? v) (json-string v p))
      ((symbol? v) (json-string (symbol->string v) p))
      ((number? v) (json-number v p))
      ((and (pair? v) (eq? (car v) '$object))
       (write-char #\{ p)
       (let loop ((pairs (cdr v)) (first? #t))
         (unless (null? pairs)
           (unless first? (write-char #\, p))
           (json-string (symbol->string (caar pairs)) p)
           (write-char #\: p)
           (json-write (cdar pairs) p)
           (loop (cdr pairs) #f)))
       (write-char #\} p))
      ((list? v)
       (write-char #\[ p)
       (let loop ((xs v) (first? #t))
         (unless (null? xs)
           (unless first? (write-char #\, p))
           (json-write (car xs) p)
           (loop (cdr xs) #f)))
       (write-char #\] p))
      (else (errorf 'trace "cannot write ~s as JSON" v)))))

;; One event: {"t":<codelet count>,"ev":<type>, fields...}
(define emit
  (lambda (ev . fields)
    (when $trace-port
      (json-write (apply json-object (cons 't *codelet-count*) (cons 'ev ev) fields)
                  $trace-port)
      (newline $trace-port))))

;;;---------------------------------------------------------------------------
;;; Names of model objects, read with side-effect-free getters only.

(define $name
  (lambda (x)
    (cond
      ((not x) 'null)
      ((symbol? x) x)
      ((string? x) x)
      ((number? x) x)
      ((procedure? x)
       (case (tell x 'object-type)
         ((slipnode) (tell x 'get-short-name))
         ((letter group) (or (tell x 'ascii-name) 'null))
         ((workspace-string) (tell x 'generic-name))
         ((concept-mapping) (tell x 'print-name))
         (else (format "<~a>" (tell x 'object-type)))))
      (else (format "~a" x)))))

(define $string-of
  (lambda (obj) (tell (tell obj 'get-string) 'generic-name)))

;; The fields that identify a structure (after its kind).
(define $structure-fields
  (lambda (s)
    (case (tell s 'object-type)
      ((bond)
       (list (cons 'string ($string-of s))
             (cons 'from ($name (tell s 'get-from-object)))
             (cons 'to ($name (tell s 'get-to-object)))
             (cons 'category ($name (tell s 'get-bond-category)))
             (cons 'direction ($name (tell s 'get-direction)))
             (cons 'facet ($name (tell s 'get-bond-facet)))))
      ((group)
       (list (cons 'string ($string-of s))
             (cons 'name ($name s))
             (cons 'category ($name (tell s 'get-group-category)))
             (cons 'direction ($name (tell s 'get-direction)))
             (cons 'facet ($name (tell s 'get-bond-facet)))
             (cons 'objects (map $name (tell s 'get-constituent-objects)))))
      ((bridge)
       (list (cons 'type (tell s 'get-bridge-type))
             (cons 'object1 ($name (tell s 'get-object1)))
             (cons 'object2 ($name (tell s 'get-object2)))
             (cons 'mappings (map $name (tell s 'get-all-concept-mappings)))))
      ((description)
       (let ((object (tell s 'get-object)))
         (list (cons 'string ($string-of object))
               (cons 'object ($name object))
               (cons 'type ($name (tell s 'get-description-type)))
               (cons 'descriptor ($name (tell s 'get-descriptor))))))
      ((rule)
       (list (cons 'type (tell s 'get-rule-type))
             (cons 'english (tell s 'get-english-transcription))))
      (else (list (cons 'object ($name s)))))))

(define emit-structure
  (lambda (ev kind s . extra)
    (apply emit ev (cons 'kind kind) (append ($structure-fields s) extra))))

;;;---------------------------------------------------------------------------
;;; The wrappers

(define install-trace!
  (lambda (port)
    (set! $trace-port port)

    ;; Codelets: the coderack hands out the next codelet in step-mcat.
    (let ((coderack *coderack*))
      (set! *coderack*
        (lambda msg
          (let ((result (apply coderack coderack (cdr msg))))
            (when (eq? (cadr msg) 'choose-codelet)
              (emit 'codelet
                    (cons 'type (tell result 'get-codelet-type-name))
                    (cons 'urgency (tell result 'get-relative-urgency))
                    (cons 'posted (tell result 'get-time-stamp))
                    (cons 'rng (random-seed))))
            result))))

    ;; Structures built and broken (emitted on entry, before the change).
    (let ((original build-bond))
      (set! build-bond
        (lambda (bond) (emit-structure 'build 'bond bond) (original bond))))
    (let ((original break-bond))
      (set! break-bond
        (lambda (bond) (emit-structure 'break 'bond bond) (original bond))))
    (let ((original build-group))
      (set! build-group
        (lambda (group flipped?)
          (emit-structure 'build 'group group (cons 'flipped flipped?))
          (original group flipped?))))
    (let ((original break-group))
      (set! break-group
        (lambda (group) (emit-structure 'break 'group group) (original group))))
    (let ((original build-bridge))
      (set! build-bridge
        (lambda (orientation bridge)
          (emit-structure 'build 'bridge bridge)
          (original orientation bridge))))
    (let ((original break-bridge))
      (set! break-bridge
        (lambda (bridge) (emit-structure 'break 'bridge bridge) (original bridge))))
    (let ((original build-description))
      (set! build-description
        (lambda (d) (emit-structure 'build 'description d) (original d))))
    ;; Rules are built by rule-builder and, translated, by justify.ss, both
    ;; through the workspace's add-rule (delete-rule is unused).
    (let ((workspace *workspace*))
      (set! *workspace*
        (lambda msg
          (when (eq? (cadr msg) 'add-rule)
            (emit-structure 'build 'rule (caddr msg)))
          (apply workspace workspace (cdr msg)))))

    ;; Temperature, after each update.
    (let ((original update-temperature))
      (set! update-temperature
        (lambda ()
          (original)
          (emit 'temperature
                (cons 'value *temperature*)
                (cons 'clamped *temperature-clamped?*)))))

    ;; Slipnet activations and Themespace state, after each slipnet update
    ;; (every %update-cycle-length% codelets, in update-everything, right
    ;; after the themespace has spread activation).
    (let ((original update-slipnet-activations))
      (set! update-slipnet-activations
        (lambda ()
          (original)
          (emit 'slipnet
                (cons 'activations (map (lambda (n) (tell n 'get-activation))
                                        *slipnet-nodes*))
                (cons 'rng (random-seed)))
          ;; themes only when different from the last themes event
          (let* ((state (tell *themespace* 'get-complete-state))
                 (fields
                   (list (cons 'active (cadr state))
                         (cons 'themes
                               (map (lambda (info)
                                      (list (car info) ($name (cadr info)) ($name (caddr info))
                                            (cadddr info) (car (cddddr info))))
                                    (caddr state))))))
            (unless (equal? fields $last-themes)
              (set! $last-themes fields)
              (apply emit 'themes fields))))))

    ;; Temporal Trace events (answer, snag, clamp, rule, group,
    ;; concept-mapping, concept-activation): the trace sends each new event
    ;; to the Trace window.
    (let ((window *trace-window*))
      (set! *trace-window*
        (lambda (self . msg)
          (when (eq? (car msg) 'add-event)
            (let ((event (cadr msg)))
              (emit 'event
                    (cons 'type (tell event 'get-type))
                    (cons 'number (tell event 'get-event-number))
                    (cons 'name (tell event 'print-name))
                    (cons 'time (tell event 'get-time))
                    (cons 'temperature (tell event 'get-temperature)))))
          (apply window self msg))))

    ;; Answers: report-new-answer calls abstract-answer-description once
    ;; per answer.
    (let ((original abstract-answer-description))
      (set! abstract-answer-description
        (lambda (answer-event)
          (emit 'answer
                (cons 'answer (tell (tell answer-event 'get-answer-string) 'print-name))
                (cons 'quality (tell answer-event 'get-quality))
                (cons 'temperature *temperature*))
          (original answer-event))))

    ;; The original halting on a message an object does not understand.
    (let ((original report-error-and-halt))
      (set! report-error-and-halt
        (lambda (message object)
          (emit 'halt
                (cons 'message (cadr message))
                (cons 'object (tell object 'object-type)))
          (original message object))))

    ;; Commentary paragraphs, as drawn in the Commentary window.
    (let ((hook $commentary-hook))
      (set! $commentary-hook
        (lambda (paragraph)
          (emit 'comment (cons 'text paragraph))
          (hook paragraph))))))

(define trace-start
  (lambda (strings seed max-codelets keep-going?)
    (emit 'start
          (cons 'format 1)
          (cons 'problem (append strings (if (= (length strings) 3) '(null) '())))
          (cons 'seed seed)
          (cons 'max_codelets (or max-codelets 'null))
          (cons 'keep_going keep-going?)
          (cons 'slipnodes (map $name *slipnet-nodes*)))))

(define trace-end
  (lambda (reason answers)
    (emit 'end
          (cons 'reason reason)
          (cons 'temperature *temperature*)
          (cons 'answers answers)
          (cons 'rng (random-seed)))
    (flush-output-port $trace-port)))

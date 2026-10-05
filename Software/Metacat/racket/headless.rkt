#lang racket/base
;; Headless runs of the engine: the Racket counterpart of
;; chez_scheme/oracle/run.ss with prelude.ss's headless windows and trace.ss.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; (run-problem strings seed cap keep-going? [trace-port]) runs one problem
;; with the engine's own init-mcat and run-mcat (run.ss) and prints to the
;; current output port what the oracle's run.ss prints: the Problem line, the
;; commentary as it is written, each answer, what the model itself prints
;; (suspend's "Type (go)...", run-mcat's "Codelets run:" at the cap) and the
;; summary.  With a trace port, it also writes the run's JSON-lines trace
;; (docs/trace-format.md), as run.ss --trace does.  It is made of:
;;   - prelude.ss's headless windows (install-headless-windows!), with the
;;     Commentary window as commentary-graphics.ss's make-comment-window
;;     drawing on a recording text window;
;;   - trace.ss's instrumentation (install-trace!): the same wrappers, set
;;     with the engine's set-global! instead of set!;
;;   - run.ss's driver: break and quiet-break end the run, or with
;;     keep-going return at once as (go) would.
;; Every wrapper only reads model state, so the run is the engine's own.
;;
;; The engine is one module instance, and some of its state outlives a run
;; (the Memory keeps its answers, codelet types their counts), whereas the
;; oracle runs each problem in a fresh Chez process.  So run one problem per
;; instance of the engine: per process (cli.rkt) or per namespace (the tests).
;;
;; This module does not require racket/gui.

(require racket/flonum racket/port
         (only-in racket/math nan? infinite?)
         racket/string
         "compat.rkt"
         "utilities.rkt"
         "engine.rkt")

(provide run-problem partial-trace golden-runs golden-file-name install-headless-windows!
         trace-gui-runs!)

;;;---------------------------------------------------------------------------
;;; JSON output (oracle trace.ss, json-*)

(define out #f)                         ; the trace's string port

(define (json-string s p)
  (write-char #\" p)
  (for ([c (in-string s)])
    (cond
      [(char=? c #\") (write-string "\\\"" p)]
      [(char=? c #\\) (write-string "\\\\" p)]
      [(char=? c #\newline) (write-string "\\n" p)]
      [(char=? c #\tab) (write-string "\\t" p)]
      [(char<? c #\space)
       (write-string (string-append "\\u" (string-pad (number->string (char->integer c) 16)))
                     p)]
      [else (write-char c p)]))
  (write-char #\" p))

(define (string-pad s)
  (string-append (make-string (max 0 (- 4 (string-length s))) #\0) s))

(define (json-number x p)
  (cond
    [(and (exact? x) (integer? x)) (write-string (number->string x) p)]
    [(exact? x) (json-string (number->string x) p)]
    [(and (flonum? x) (not (nan? x)) (not (infinite? x)))
     ;; Chez marks subnormals with a precision suffix, 5e-324|1
     (write-string (car (string-split (number->string x) "|" #:trim? #f)) p)]
    [else (json-string (number->string x) p)]))

(define (json-object . pairs) (cons '$object pairs))

(define (json-write v p)
  (cond
    [(eq? v #t) (write-string "true" p)]
    [(eq? v #f) (write-string "false" p)]
    [(eq? v 'null) (write-string "null" p)]
    [(null? v) (write-string "[]" p)]
    [(string? v) (json-string v p)]
    [(symbol? v) (json-string (symbol->string v) p)]
    [(number? v) (json-number v p)]
    [(and (pair? v) (eq? (car v) '$object))
     (write-char #\{ p)
     (for ([pair (in-list (cdr v))] [i (in-naturals)])
       (unless (zero? i) (write-char #\, p))
       (json-string (symbol->string (car pair)) p)
       (write-char #\: p)
       (json-write (cdr pair) p))
     (write-char #\} p)]
    [(list? v)
     (write-char #\[ p)
     (for ([x (in-list v)] [i (in-naturals)])
       (unless (zero? i) (write-char #\, p))
       (json-write x p))
     (write-char #\] p)]
    [else (error 'trace "cannot write ~s as JSON" v)]))

;; One event: {"t":<codelet count>,"ev":<type>, fields...}
(define (emit ev . fields)
  (when out
    (json-write (apply json-object (cons 't *codelet-count*) (cons 'ev ev) fields) out)
    (write-char #\newline out)))

;;;---------------------------------------------------------------------------
;;; Names of model objects (trace.ss's $name, $string-of, $structure-fields)

(define ($name x)
  (cond
    [(not x) 'null]
    [(symbol? x) x]
    [(string? x) x]
    [(number? x) x]
    [(procedure? x)
     (case (tell x 'object-type)
       [(slipnode) (tell x 'get-short-name)]
       [(letter group) (or (tell x 'ascii-name) 'null)]
       [(workspace-string) (tell x 'generic-name)]
       [(concept-mapping) (tell x 'print-name)]
       [else (format "<~a>" (tell x 'object-type))])]
    [else (format "~a" x)]))

(define ($string-of obj) (tell (tell obj 'get-string) 'generic-name))

(define ($structure-fields s)
  (case (tell s 'object-type)
    [(bond)
     (list (cons 'string ($string-of s))
           (cons 'from ($name (tell s 'get-from-object)))
           (cons 'to ($name (tell s 'get-to-object)))
           (cons 'category ($name (tell s 'get-bond-category)))
           (cons 'direction ($name (tell s 'get-direction)))
           (cons 'facet ($name (tell s 'get-bond-facet))))]
    [(group)
     (list (cons 'string ($string-of s))
           (cons 'name ($name s))
           (cons 'category ($name (tell s 'get-group-category)))
           (cons 'direction ($name (tell s 'get-direction)))
           (cons 'facet ($name (tell s 'get-bond-facet)))
           (cons 'objects (map $name (tell s 'get-constituent-objects))))]
    [(bridge)
     (list (cons 'type (tell s 'get-bridge-type))
           (cons 'object1 ($name (tell s 'get-object1)))
           (cons 'object2 ($name (tell s 'get-object2)))
           (cons 'mappings (map $name (tell s 'get-all-concept-mappings))))]
    [(description)
     (let ([object (tell s 'get-object)])
       (list (cons 'string ($string-of object))
             (cons 'object ($name object))
             (cons 'type ($name (tell s 'get-description-type)))
             (cons 'descriptor ($name (tell s 'get-descriptor)))))]
    [(rule)
     (list (cons 'type (tell s 'get-rule-type))
           (cons 'english (tell s 'get-english-transcription)))]
    [else (list (cons 'object ($name s)))]))

(define (emit-structure ev kind s . extra)
  (apply emit ev (cons 'kind kind) (append ($structure-fields s) extra)))

;;;---------------------------------------------------------------------------
;;; Headless windows (prelude.ss, install-headless-windows!)

(define (make-null-window name messages)
  (lambda (self . msg)
    (if (memq (car msg) messages)
        'done
        (error 'headless-window "~s received unexpected message ~s" name msg))))

;; commentary-graphics.ss's comment window (its model part: the eliza and
;; non-eliza paragraphs) without a text window; the recorder below prints
;; and emits each paragraph drawn
(define (make-headless-comment-window)
  (let ([eliza-paragraphs '()]
        [non-eliza-paragraphs '()])
    (lambda msg
      (let ([self (car msg)])
        (record-case (cdr msg)
          (object-type () 'comment-window)
          (new-problem (initial-sym modified-sym target-sym answer-sym)
            (tell self 'add-comment
              (if %justify-mode%
                (list
                  (format "Let's see... \"~a\" changes to \"~a\", and"
                          initial-sym modified-sym)
                  (format " \"~a\" changes to \"~a\".  Hmm..."
                          target-sym answer-sym))
                (list
                  (format "Okay, if \"~a\" changes to \"~a\", what"
                          initial-sym modified-sym)
                  (format " does \"~a\" change to?  Hmm..." target-sym)))
              (if %justify-mode%
                (list
                  (format "Beginning justify run:  \"~a\" changes to \"~a\", and"
                          initial-sym modified-sym)
                  (format " \"~a\" changes to \"~a\"..."
                          target-sym answer-sym))
                (list
                  (format "Beginning run:  If \"~a\" changes to \"~a\", what"
                          initial-sym modified-sym)
                  (format " does \"~a\" change to?" target-sym))))
            'done)
          (add-comment (lines1 lines2)
            (let ([paragraph1 (apply string-append lines1)]
                  [paragraph2 (apply string-append lines2)])
              (set! eliza-paragraphs (cons 1 (cons paragraph1 eliza-paragraphs)))
              (set! non-eliza-paragraphs (cons 1 (cons paragraph2 non-eliza-paragraphs)))
              'done))
          (clear ()
            (set! eliza-paragraphs '())
            (set! non-eliza-paragraphs '())
            'done)
          (initialize () (tell self 'clear) 'done)
          (else (error 'headless-window "comment window received unexpected message ~s"
                       msg)))))))

(define (install-headless-windows!)
  (set-global! '%workspace-graphics% #f)
  (set-global! '%slipnet-graphics% #f)
  (set-global! '%coderack-graphics% #f)
  (set-global! '*workspace-window* (make-null-window 'workspace '(garbage-collect caching-on flush)))
  (set-global! '*slipnet-window* (make-null-window 'slipnet '(clear)))
  (set-global! '*coderack-window* (make-null-window 'coderack '(clear)))
  (set-global! '*themespace-window*
               (make-null-window 'themespace
                                 '(erase-all-themes update-thematic-pressure update-graphics
                                   set-theme-graphics-parameters-and-draw garbage-collect)))
  (set-global! '*top-themes-window* (make-null-window 'top-themes '()))
  (set-global! '*bottom-themes-window* (make-null-window 'bottom-themes '()))
  (set-global! '*vertical-themes-window* (make-null-window 'vertical-themes '()))
  (set-global! '*memory-window*
               ;; add-memory-icon gives each answer or snag description its
               ;; icon drawing procedures (memory-graphics.ss), which memory.ss
               ;; calls even when nothing is displayed; here they draw nothing
               (let ([null-window (make-null-window 'memory '(draw))])
                 (lambda (self . msg)
                   (case (car msg)
                     [(add-memory-icon)
                      (tell (cadr msg) 'set-graphics-info (lambda (activation) 'no-icon) 'no-icon)
                      'done]
                     [else (apply null-window self msg)]))))
  (set-global! '*trace-window* (make-null-window 'trace '(initialize add-event)))
  (set-global! '*temperature-window* (make-null-window 'temperature '(initialize update-graphics)))
  (set-global! '*EEG-window* (make-null-window 'EEG '(initialize plot-current-values)))
  (let ([coderack-graphics (make-null-window 'coderack-graphics '(set-last-codelet-type))])
    (for-each
      (lambda (type)
        (tell type 'set-graphics-parameters coderack-graphics #f #f #f #f #f #f #f #f))
      *codelet-types*))
  (set-global! '*control-panel*
               (lambda (self . msg)
                 (case (car msg)
                   ;; as in gui.ss, with the verbose checkbox off unless
                   ;; run-problem's #:verbose? is true
                   [(set-verbose-step-mode) (set-global! '%verbose% (or (cadr msg) verbose?))
                                            'done]
                   [else (error 'headless-window "control panel received unexpected message ~s"
                                msg)])))
  (set-global! '*comment-window* (make-headless-comment-window)))

;; The recorders: the Commentary window's paragraphs (the oracle's
;; $commentary-hook prints each one as it is drawn) and the Trace window's
;; events (trace.ss), around whichever windows are installed, headless or
;; the views' (racket/gui/views.rkt).  Each wrapper passes itself as self,
;; so the window's own (tell self 'add-comment ...) is recorded too.
(define recorders '())

(define (install-recorders!)
  (unless (memq *comment-window* recorders)
    (let ([window *comment-window*])
      (define recorder
        (lambda (self . msg)
          (when (eq? (car msg) 'add-comment)
            (let ([paragraph (apply string-append
                                    (if %eliza-mode% (cadr msg) (caddr msg)))])
              (printf "Comment: ~a~%" paragraph)
              (emit 'comment (cons 'text paragraph))))
          (apply window self msg)))
      (set! recorders (cons recorder recorders))
      (set-global! '*comment-window* recorder)))
  (unless (memq *trace-window* recorders)
    (let ([window *trace-window*])
      (define recorder
        (lambda (self . msg)
          (when (eq? (car msg) 'add-event)
            (let ([event (cadr msg)])
              (emit 'event
                    (cons 'type (tell event 'get-type))
                    (cons 'number (tell event 'get-event-number))
                    (cons 'name (tell event 'print-name))
                    (cons 'time (tell event 'get-time))
                    (cons 'temperature (tell event 'get-temperature)))))
          (apply window self msg)))
      (set! recorders (cons recorder recorders))
      (set-global! '*trace-window* recorder))))

;;;---------------------------------------------------------------------------
;;; The trace's wrappers (trace.ss, install-trace!), installed once around
;;; the engine's own procedures and objects; with run.ss's printing of
;;; answers and of report-error-and-halt's message

(define last-themes #f)
(define answers '())
(define stop-run #f)

(define originals #f)

(define (install-trace!)
  (unless originals
    (set! originals
          (list *coderack* build-bond break-bond build-group break-group build-bridge
                break-bridge build-description *workspace* update-temperature
                update-slipnet-activations abstract-answer-description)))
  (define-values (coderack o-build-bond o-break-bond o-build-group o-break-group
                  o-build-bridge o-break-bridge o-build-description workspace
                  o-update-temperature o-update-slipnet-activations
                  o-abstract-answer-description)
    (apply values originals))
  ;; Codelets: the coderack hands out the next codelet in step-mcat.
  (set-global! '*coderack*
               (lambda msg
                 (let ([result (apply coderack coderack (cdr msg))])
                   (when (eq? (cadr msg) 'choose-codelet)
                     (emit 'codelet
                           (cons 'type (tell result 'get-codelet-type-name))
                           (cons 'urgency (tell result 'get-relative-urgency))
                           (cons 'posted (tell result 'get-time-stamp))
                           (cons 'rng (random-seed))))
                   result)))
  ;; Structures built and broken (emitted on entry, before the change).
  (set-global! 'build-bond (lambda (bond) (emit-structure 'build 'bond bond) (o-build-bond bond)))
  (set-global! 'break-bond (lambda (bond) (emit-structure 'break 'bond bond) (o-break-bond bond)))
  (set-global! 'build-group
               (lambda (group flipped?)
                 (emit-structure 'build 'group group (cons 'flipped flipped?))
                 (o-build-group group flipped?)))
  (set-global! 'break-group
               (lambda (group) (emit-structure 'break 'group group) (o-break-group group)))
  (set-global! 'build-bridge
               (lambda (orientation bridge)
                 (emit-structure 'build 'bridge bridge)
                 (o-build-bridge orientation bridge)))
  (set-global! 'break-bridge
               (lambda (bridge) (emit-structure 'break 'bridge bridge) (o-break-bridge bridge)))
  (set-global! 'build-description
               (lambda (d) (emit-structure 'build 'description d) (o-build-description d)))
  ;; Rules are built through the workspace's add-rule.
  (set-global! '*workspace*
               (lambda msg
                 (when (eq? (cadr msg) 'add-rule)
                   (emit-structure 'build 'rule (caddr msg)))
                 (apply workspace workspace (cdr msg))))
  ;; Temperature, after each update.
  (set-global! 'update-temperature
               (lambda ()
                 (o-update-temperature)
                 (emit 'temperature
                       (cons 'value *temperature*)
                       (cons 'clamped *temperature-clamped?*))))
  ;; Slipnet activations and Themespace state, after each slipnet update.
  (set-global! 'update-slipnet-activations
               (lambda ()
                 (o-update-slipnet-activations)
                 (emit 'slipnet
                       (cons 'activations (map (lambda (n) (tell n 'get-activation))
                                               *slipnet-nodes*))
                       (cons 'rng (random-seed)))
                 ;; themes only when different from the last themes event
                 (let* ([state (tell *themespace* 'get-complete-state)]
                        [fields
                         (list (cons 'active (cadr state))
                               (cons 'themes
                                     (map (lambda (info)
                                            (list (car info) ($name (cadr info))
                                                  ($name (caddr info))
                                                  (cadddr info) (car (cddddr info))))
                                          (caddr state))))])
                   (unless (equal? fields last-themes)
                     (set! last-themes fields)
                     (apply emit 'themes fields)))))
  ;; Answers: report-new-answer calls abstract-answer-description once per
  ;; answer (run.ss collects them for the summary).
  (set-global! 'abstract-answer-description
               (lambda (answer-event)
                 (let ([answer (tell (tell answer-event 'get-answer-string) 'print-name)])
                   (set! answers (append answers (list answer)))
                   (printf "Answer: ~a  quality ~a  codelet ~a  temperature ~a~%"
                           answer (tell answer-event 'get-quality) *codelet-count* *temperature*)
                   (emit 'answer
                         (cons 'answer answer)
                         (cons 'quality (tell answer-event 'get-quality))
                         (cons 'temperature *temperature*)))
                 (o-abstract-answer-description answer-event)))
  ;; The original halting on a message an object does not understand: run.ss
  ;; prints it and ends the run.
  (set-report-error-and-halt!
   (lambda (message object)
     (printf "Ooops: bad message \"~a\" sent to object of type ~a~%"
             (cadr message) (tell object 'object-type))
     (emit 'halt
           (cons 'message (cadr message))
           (cons 'object (tell object 'object-type)))
     (stop-run 'halt))))

;;;---------------------------------------------------------------------------
;;; The driver (chez_scheme/oracle/run.ss)

(define keep-going? #f)

;; run.ss's break and quiet-break, as chez_scheme/oracle/run.ss's
;; headless-break: the run ends, or with keep-going continues as (go) would
(define (headless-break)
  (set-global! '*running?* #f)
  (if (and keep-going? (not (and *break-time* (= *break-time* *codelet-count*))))
      (begin (set-global! '*running?* #t) 'ignore)
      (stop-run (if (and *break-time* (= *break-time* *codelet-count*)) 'cap 'suspend))))

(define installed? #f)

;; the trace up to the error, after run-problem raised
(define the-partial-trace #f)
(define (partial-trace) the-partial-trace)

;; verbose mode (gui.ss's Options > Verbose mode checkbox): the model's
;; vprintf output is printed (cli.rkt --verbose, the oracle's run.ss --verbose)
(define verbose? #f)

;; strings: 3 or 4 symbols; seed: 1 to 2^32-1; cap: a codelet count or #f.
;; verbose: #t turns verbose mode on, as the oracle's run.ss --verbose does.
;; views: #f, or a thunk that attaches views (racket/gui/views.rkt) to the
;; run once the headless windows are installed; watching must not change
;; the run.  Returns why the run stopped: suspend, cap or halt.
(define (run-problem strings seed cap keep? [trace-port #f] #:views [views #f]
                     #:verbose? [verbose #f])
  (unless installed?
    (install-headless-windows!)
    (install-trace!)
    (set-global! 'break headless-break)
    (set-global! 'quiet-break headless-break)
    (set! installed? #t))
  (when views (views))
  (install-recorders!)
  (set! verbose? verbose)
  (set-global! '%verbose% verbose)
  (set! out trace-port)
  (set! last-themes #f)
  (set! answers '())
  (set! keep-going? keep?)
  (define initial (car strings))
  (define modified (cadr strings))
  (define target (caddr strings))
  (define answer (if (= (length strings) 4) (cadddr strings) #f))
  (emit 'start
        (cons 'format 1)
        (cons 'problem (append strings (if (= (length strings) 3) '(null) '())))
        (cons 'seed seed)
        (cons 'max_codelets (or cap 'null))
        (cons 'keep_going keep?)
        (cons 'slipnodes (map $name *slipnet-nodes*)))
  (set-global! '%justify-mode% (and answer #t))
  (printf "Problem: ~a -> ~a; ~a -> ~a  seed ~a~%"
          initial modified target (or answer '?) seed)
  (define reason
    (let/ec k
      (set! stop-run k)
      ;; a Racket error (the original's Chez errors) ends the run; the
      ;; trace so far is kept for partial-trace
      (with-handlers ([exn:fail? (lambda (e)
                                   (when out (flush-output out))
                                   (set! the-partial-trace
                                         (and out (string-port? out) (get-output-string out)))
                                   (set! out #f)
                                   (raise e))])
        (init-mcat initial modified target answer seed)
        (set-global! '*break-time* cap)
        (run-mcat))))
  (emit 'end
        (cons 'reason reason)
        (cons 'temperature *temperature*)
        (cons 'answers answers)
        (cons 'rng (random-seed)))
  (set! out #f)
  (printf "Stopped: ~a~%" reason)
  (printf "Codelets: ~a~%" *codelet-count*)
  (printf "Temperature: ~a~%" *temperature*)
  (printf "Answers: ~a~%" (if (null? answers) "none" answers))
  reason)

;; A run driven by a GUI (racket/gui-tests/one-window-test.rkt): the
;; trace's wrappers, and its recorders around the GUI's own Commentary and
;; Trace windows, write the events after the start event to trace-port (#f
;; stops writing).  Unlike run-problem, this installs no headless windows
;; and keeps the GUI's break and quiet-break: the run is the GUI's own.
(define (trace-gui-runs! trace-port)
  (install-trace!)
  (install-recorders!)
  (set! last-themes #f)
  (set! answers '())
  (set! out trace-port))

;;;---------------------------------------------------------------------------
;;; tests/problems.txt (chez_scheme/oracle/make-golden.ss's reading of it)

(define (golden-file-name strings seed)
  (format "~a_~a.jsonl" (string-join (map symbol->string strings) "-") seed))

;; every run: (list file-name strings seed cap keep-going?)
(define (golden-runs problems-file)
  (apply append
    (for/list ([line (in-list (call-with-input-file problems-file
                                (lambda (in) (for/list ([l (in-lines in)]) l))))]
               #:unless (null? (string-split (regexp-replace #rx"#.*$" line ""))))
      (define fields (map string-split (string-split (regexp-replace #rx"#.*$" line "")
                                                     "|" #:trim? #f)))
      (define strings (map string->symbol (car fields)))
      (define seeds (map string->number (cadr fields)))
      (define cap (string->number (car (caddr fields))))
      (define keep? (and (= (length fields) 4) (equal? (cadddr fields) '("keep-going"))))
      (for/list ([seed (in-list seeds)])
        (list (golden-file-name strings seed) strings seed cap keep?)))))

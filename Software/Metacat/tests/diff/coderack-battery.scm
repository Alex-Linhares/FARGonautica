;;; coderack-battery.scm -- differential checks of the engine's constants.ss,
;;; setup.ss, coderack.ss and descriptions.ss (item 04) against the original
;;; under Chez Scheme 10.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss       (Chez, with the whole original loaded)
;;;   racket/tests/coderack-diff-test.rkt   (Racket: compat + utilities + engine)
;;; The rules of utilities-battery.scm apply: no two side-effecting
;;; subexpressions in one call or one `let'; lists for `map' from b:iota or
;;; b:copy.  Globals of the original are set with b:set-global!.
;;;
;;; The Workspace, Themespace, Temporal Trace and Slipnet are not ported yet;
;;; fakes below stand in for *workspace*, *themespace*, *trace* and
;;; *top-down-slipnodes*, answering the messages the coderack sends them.

;;;---------------------------------------------------------------------------
;;; setup.ss defaults, before anything changes them

(test setup-defaults
  (list *codelet-count* *temperature*
        %eliza-mode% %justify-mode% %self-watching-enabled% %verbose%
        %workspace-graphics% %slipnet-graphics% %coderack-graphics%
        %codelet-count-graphics% %highlight-last-codelet% %nice-graphics%))

(test setup-windows
  (list *workspace-window* *slipnet-window* *coderack-window* *themespace-window*
        *top-themes-window* *bottom-themes-window* *vertical-themes-window*
        *memory-window* *comment-window* *trace-window* *temperature-window*
        *EEG-window* *control-panel* *repl-thread*))

(test coderack-constants
  (list %max-coderack-size% %num-of-coderack-bins%
        %extremely-low-urgency% %very-low-urgency% %low-urgency% %medium-urgency%
        %high-urgency% %very-high-urgency% %extremely-high-urgency%))

;;;---------------------------------------------------------------------------
;;; Helpers

(define b:index-of
  (lambda (x l)
    (let loop ((l l) (i 0))
      (cond
        ((null? l) #f)
        ((eq? x (car l)) i)
        (else (loop (cdr l) (+ i 1)))))))

(define b:bin-index
  (lambda (bin) (b:index-of bin (tell *coderack* 'get-all-bins))))

(define b:type-names
  (lambda (types) (map (lambda (t) (tell t 'get-codelet-type-name)) types)))

;; a codelet as data
(define b:codelet
  (lambda (c)
    (list (tell c 'get-codelet-type-name)
          (tell c 'get-relative-urgency)
          (tell c 'get-time-stamp)
          (tell c 'get-index-in-bin)
          (b:bin-index (tell c 'get-coderack-bin))
          (tell c 'proposed-structure-argument?))))

;; the whole coderack as data
(define b:rack
  (lambda ()
    (list (tell *coderack* 'get-num-of-codelets)
          (tell *coderack* 'empty?)
          (tell *coderack* 'get-total-urgency-sum)
          (tell *coderack* 'get-highest-bin-urgency)
          (map (lambda (bin)
                 (list (tell bin 'get-num-of-codelets)
                       (tell bin 'get-urgency)
                       (tell bin 'get-urgency-sum)
                       (map b:codelet (tell bin 'get-codelets))))
               (tell *coderack* 'get-all-bins))
          (map b:codelet (tell *coderack* 'get-all-codelets)))))

(define b:urgencies
  (list 0 7 15/2 21 35 49 63 77 91 100 3 10 14 28 50 99 1 42 70 85
        102/5 7.5 33.3 66.6 1/3 99.99 13 57 64 12))

;; post n codelets, cycling through the codelet types and b:urgencies,
;; advancing *codelet-count* by 1-3 between posts
(define b:post-n
  (lambda (n offset)
    (let loop ((i 0))
      (if (< i n)
          (let* ((k (+ i offset))
                 (type (list-ref *codelet-types* (modulo k (length *codelet-types*))))
                 (urgency (list-ref b:urgencies (modulo (* 7 k) (length b:urgencies))))
                 (codelet (tell type 'make-codelet urgency)))
            (b:set-global! '*codelet-count* (+ *codelet-count* 1 (modulo k 3)))
            (tell *coderack* 'post codelet)
            (loop (+ i 1)))))))

(define b:choose-n
  (lambda (n)
    (b:repeat n
      (lambda ()
        (let* ((c (tell *coderack* 'choose-codelet))
               (s (random-seed)))
          (list (tell c 'get-codelet-type-name) (tell c 'get-relative-urgency)
                (tell c 'get-time-stamp) s))))))

;; a logging stand-in for the window each codelet type draws on
(define b:window-log '())
(define b:log-window
  (lambda msg
    (set! b:window-log
      (cons (if (eq? (cadr msg) 'set-last-codelet-type)
                (list 'set-last-codelet-type (tell (caddr msg) 'get-codelet-type-name))
                (cdr msg))
            b:window-log))
    'done))

;; fakes for not-yet-ported model objects.  b:ws-settings is an alist read
;; by the fake workspace.
(define b:ws-settings '())
(define b:setting (lambda (key) (cdr (assq key b:ws-settings))))
(define b:deleted '())

(define b:fake-workspace
  (lambda msg
    (record-case (cdr msg)
      (object-type () 'workspace)
      (get-average-intra-string-unhappiness () (b:setting 'intra))
      (get-min-mapping-strength () (b:setting 'min-mapping))
      (get-average-unhappiness () (b:setting 'unhappiness))
      (get-possible-rule-types () (b:setting 'rule-types))
      (supported-rule-exists? (which) (memq which (b:setting 'supported)))
      (get-rough-num-of-unrelated-objects () (b:setting 'unrelated))
      (get-bonds () (b:setting 'bonds))
      (get-rough-num-of-ungrouped-objects () (b:setting 'ungrouped))
      (get-rough-num-of-unmapped-objects () (b:setting 'unmapped))
      (get-max-inter-string-unhappiness () (b:setting 'inter))
      (delete-proposed-structure (s)
        (set! b:deleted (cons (tell s 'get-value) b:deleted))
        'done)
      (else 'invalid-message-indicator))))

(define b:fake-themespace
  (lambda msg
    (record-case (cdr msg)
      (object-type () 'themespace)
      (thematic-pressure? () (b:setting 'pressure))
      (get-active-bridge-theme-types () (b:setting 'bridge-themes))
      (get-max-positive-theme-activation (types)
        (if (null? types) 0 (b:setting 'theme-activation)))
      (else 'invalid-message-indicator))))

(define b:fake-trace
  (lambda msg
    (record-case (cdr msg)
      (object-type () 'trace)
      (within-snag-period? () (b:setting 'snag))
      (within-clamp-period? () (b:setting 'clamp))
      (else 'invalid-message-indicator))))

(define b:top-down-log '())
(define b:make-fake-node
  (lambda (n)
    (lambda msg
      (record-case (cdr msg)
        (object-type () 'slipnode)
        (attempt-to-post-top-down-codelets ()
          (set! b:top-down-log (cons n b:top-down-log))
          'done)
        (else 'invalid-message-indicator)))))

;; a fake proposed structure or description, as a codelet argument
(define b:make-fake-structure
  (lambda (type n)
    (lambda msg
      (record-case (cdr msg)
        (object-type () type)
        (get-value () n)
        (print () (printf "<fake ~a ~a>~%" type n))
        (else 'invalid-message-indicator)))))

(define b:settings-list
  (list
    (list (cons 'intra 30) (cons 'min-mapping 40) (cons 'unhappiness 55)
          (cons 'rule-types '()) (cons 'supported '())
          (cons 'unrelated 'few) (cons 'bonds '()) (cons 'ungrouped 'few)
          (cons 'unmapped 'some) (cons 'inter 45)
          (cons 'pressure #f) (cons 'bridge-themes '()) (cons 'theme-activation 0)
          (cons 'snag #f) (cons 'clamp #f))
    (list (cons 'intra 80) (cons 'min-mapping 10) (cons 'unhappiness 90)
          (cons 'rule-types (list 'top)) (cons 'supported (list 'top))
          (cons 'unrelated 'many) (cons 'bonds (list 'b1)) (cons 'ungrouped 'many)
          (cons 'unmapped 'many) (cons 'inter 100)
          (cons 'pressure #t) (cons 'bridge-themes (list 'top-bridge)) (cons 'theme-activation 85)
          (cons 'snag #t) (cons 'clamp #f))
    (list (cons 'intra 0) (cons 'min-mapping 100) (cons 'unhappiness 0)
          (cons 'rule-types (list 'top 'bottom)) (cons 'supported (list 'bottom))
          (cons 'unrelated 'some) (cons 'bonds (list 'b1 'b2)) (cons 'ungrouped 'some)
          (cons 'unmapped 'few) (cons 'inter 33.3)
          (cons 'pressure #f) (cons 'bridge-themes (list 'top-bridge 'bottom-bridge))
          (cons 'theme-activation 40)
          (cons 'snag #f) (cons 'clamp #t))
    (list (cons 'intra 12.5) (cons 'min-mapping 77.7) (cons 'unhappiness 102/5)
          (cons 'rule-types (list 'top)) (cons 'supported (list 'top 'bottom))
          (cons 'unrelated 'many) (cons 'bonds (list 'b1)) (cons 'ungrouped 'few)
          (cons 'unmapped 'some) (cons 'inter 64)
          (cons 'pressure #t) (cons 'bridge-themes '()) (cons 'theme-activation 0)
          (cons 'snag #f) (cons 'clamp #f))))

(define b:reset
  (lambda ()
    (b:set-global! '*codelet-count* 0)
    (b:set-global! '*temperature* 0)
    (tell *coderack* 'initialize)))

;;;---------------------------------------------------------------------------
;;; Switch the display off and install the fakes

(b:set-global! '%coderack-graphics% #f)
(b:set-global! '%codelet-count-graphics% #f)
(b:set-global! '%workspace-graphics% #f)
(b:set-global! '%slipnet-graphics% #f)
(b:set-global! '*workspace* b:fake-workspace)
(b:set-global! '*themespace* b:fake-themespace)
(b:set-global! '*trace* b:fake-trace)
(b:set-global! '*top-down-slipnodes* (map b:make-fake-node (b:iota 5)))
(for-each
  (lambda (type)
    (tell type 'set-graphics-parameters b:log-window #f #f #f #f #f #f #f #f))
  *codelet-types*)

;;;---------------------------------------------------------------------------
;;; Urgencies and bins

(test urgency-value-table %urgency-value-table%)

(test urgency-names
  (map urgency-name
       (append (b:iota 101)
               (b:copy (list -1 0 7 7.0 15/2 7.000001 21 21.5 35 49 49.0 63 77 91 100 1000)))))

(test coderack-bin-indices
  (map (lambda (u) (b:bin-index (tell *coderack* 'get-coderack-bin u)))
       (append (b:iota 101)
               (b:copy (list -5 0 1/2 100/7 99/7 101/7 14.285714285714286 14.285714285714285
                             200/7 99.9 100 100.0 120 7.5 33.3 66.6 85.71428571428571 600/7)))))

(test bin-urgencies-by-temperature
  (map (lambda (t)
         (b:set-global! '*temperature* t)
         (list t (tell *coderack* 'get-highest-bin-urgency)
               (map (lambda (bin) (tell bin 'get-urgency)) (tell *coderack* 'get-all-bins))))
       (append (b:iota 100) (b:copy (list 0)))))

(test codelet-type-names (b:type-names *codelet-types*))
(test thematic-codelet-types (b:type-names *thematic-codelet-types*))
(test bottom-up-codelet-types (b:type-names *bottom-up-codelet-types*))
(test self-watching-codelet-types (b:type-names *self-watching-codelet-types*))
(test codelet-type-object-type (map (lambda (t) (tell t 'object-type)) *codelet-types*))

(test graphics-labels-off
  (map (lambda (t) (tell t 'get-graphics-labels)) *codelet-types*))
(test graphics-labels-on
  (begin
    (b:set-global! '%codelet-count-graphics% #t)
    (let* ((labels (map (lambda (t) (tell t 'get-graphics-labels)) *codelet-types*)))
      (b:set-global! '%codelet-count-graphics% #f)
      labels)))

;;;---------------------------------------------------------------------------
;;; Posting and choosing

(test post-30
  (begin
    (b:reset)
    (b:post-n 30 0)
    (b:rack)))

(test choose-by-seed-and-temperature
  (map (lambda (seed)
         (map (lambda (t)
                (b:reset)
                (b:post-n 40 seed)
                (b:set-global! '*temperature* t)
                (b:seeded seed (lambda () (list (b:choose-n 25) (b:rack)))))
              (b:copy (list 0 15 35 50 64 85 100))))
       (b:copy (list 1 2 3 7 42 1000 123456789 4294967295))))

(test choose-until-empty
  (map (lambda (seed)
         (b:reset)
         (b:post-n 17 seed)
         (b:set-global! '*temperature* 40)
         (b:seeded seed (lambda () (list (b:choose-n 17) (b:rack)))))
       b:seeds))

(test overflow-deletes
  (map (lambda (seed)
         (b:reset)
         (b:set-global! '*temperature* 30)
         (b:seeded seed
           (lambda ()
             (b:post-n 160 seed)
             (b:rack))))
       (b:copy (list 1 2 3 42 65536 2147483648 3141592653))))

(test overflow-deletes-proposed-structures
  (map (lambda (seed)
         (b:reset)
         (set! b:deleted '())
         (b:seeded seed
           (lambda ()
             (let loop ((i 0))
               (if (< i 130)
                   (let* ((type (list-ref *codelet-types* (modulo i 27)))
                          (arg (b:make-fake-structure
                                 (list-ref (b:copy (list 'bond 'group 'bridge 'description 'letter))
                                           (modulo i 5))
                                 i))
                          (c (tell type 'make-codelet (list-ref b:urgencies (modulo i 30)) arg)))
                     (b:set-global! '*codelet-count* (+ *codelet-count* 1))
                     (tell *coderack* 'post c)
                     (loop (+ i 1)))))
             (list (reverse b:deleted) (b:rack)))))
       (b:copy (list 1 5 99))))

(test removal-weights
  (begin
    (b:reset)
    (b:post-n 50 3)
    (b:set-global! '*codelet-count* 500)
    (b:set-global! '*temperature* 70)
    (map (lambda (c) (tell c 'get-removal-weight)) (tell *coderack* 'get-all-codelets))))

;;;---------------------------------------------------------------------------
;;; Deferred codelets

(test post-deferred-small
  (map (lambda (seed)
         (b:reset)
         (b:post-n 70 seed)
         (b:seeded seed
           (lambda ()
             (let loop ((i 0))
               (if (< i 45)
                   (let* ((type (list-ref *codelet-types* (modulo (* 5 i) 27))))
                     (tell *coderack* 'add-deferred-codelet
                       (tell type 'make-codelet (list-ref b:urgencies (modulo i 30))))
                     (loop (+ i 1)))))
             (tell *coderack* 'post-deferred-codelets)
             (b:rack))))
       (b:copy (list 1 2 3 77))))

(test post-deferred-large
  (map (lambda (seed)
         (b:reset)
         (b:post-n 60 seed)
         (b:seeded seed
           (lambda ()
             (let loop ((i 0))
               (if (< i 137)
                   (let* ((type (list-ref *codelet-types* (modulo (* 11 i) 27))))
                     (tell *coderack* 'add-deferred-codelet
                       (tell type 'make-codelet (list-ref b:urgencies (modulo i 30))))
                     (loop (+ i 1)))))
             (tell *coderack* 'post-deferred-codelets)
             (b:rack))))
       (b:copy (list 1 2 3 77))))

(test post-deferred-exactly-100
  (begin
    (b:reset)
    (b:post-n 10 0)
    (b:seeded 9
      (lambda ()
        (let loop ((i 0))
          (if (< i 100)
              (begin
                (tell *coderack* 'add-deferred-codelet
                  (tell bottom-up-bond-scout 'make-codelet (list-ref b:urgencies (modulo i 30))))
                (loop (+ i 1)))))
        (tell *coderack* 'post-deferred-codelets)
        (b:rack)))))

;;;---------------------------------------------------------------------------
;;; Clamping and urgency adjustments

(test clamp-unclamp
  (begin
    (b:reset)
    (b:post-n 54 0)
    (let* ((before (b:rack))
           (r1 (tell bond-builder 'clamp 90))
           (clamped (list (tell bond-builder 'clamped?) (tell bond-builder 'get-clamped-urgency)))
           (after-clamp (b:rack))
           (new (tell bond-builder 'make-codelet 10))
           (new-urgency (tell new 'get-relative-urgency))
           (r2 (tell bond-builder 'clamp 90))
           (r3 (tell bond-builder 'clamp 20))
           (after-reclamp (b:rack))
           (r4 (tell bond-builder 'unclamp))
           (r5 (tell bond-builder 'unclamp))
           (after-unclamp (b:rack)))
      (list before r1 clamped after-clamp new-urgency r2 r3 after-reclamp r4 r5
            after-unclamp (tell bond-builder 'clamped?)))))

(test adjust-and-set-urgencies
  (begin
    (b:reset)
    (b:post-n 54 1)
    (let* ((r1 (tell *coderack* 'adjust-urgencies rule-scout 30))
           (a1 (b:rack))
           (r2 (tell *coderack* 'adjust-urgencies rule-scout -200))
           (a2 (b:rack))
           (r3 (tell *coderack* 'adjust-urgencies breaker 1/3))
           (a3 (b:rack))
           (r4 (tell *coderack* 'set-urgencies jootser 77))
           (a4 (b:rack))
           (r5 (tell *coderack* 'reset-urgencies jootser))
           (a5 (b:rack))
           (r6 (tell *coderack* 'reset-urgencies rule-scout))
           (a6 (b:rack)))
      (list r1 a1 r2 a2 r3 a3 r4 a4 r5 a5 r6 a6))))

(test codelets-of-type
  (begin
    (b:reset)
    (b:post-n 80 2)
    (map (lambda (t) (map b:codelet (tell *coderack* 'get-codelets-of-type t)))
         *codelet-types*)))

(test choose-after-clamp
  (map (lambda (seed)
         (b:reset)
         (b:post-n 60 seed)
         (tell answer-finder 'clamp 100)
         (tell description-builder 'clamp 5)
         (b:set-global! '*temperature* 55)
         (let* ((r (b:seeded seed (lambda () (b:choose-n 30)))))
           (tell answer-finder 'unclamp)
           (tell description-builder 'unclamp)
           (list r (b:rack))))
       (b:copy (list 1 2 3 4 5))))

(test update-all-selection-probabilities
  (begin
    (b:reset)
    (b:post-n 33 5)
    (b:set-global! '*temperature* 45)
    (let* ((r1 (tell *coderack* 'update-all-selection-probabilities))
           (r2 (tell *coderack* 'initialize))
           (r3 (tell *coderack* 'update-all-selection-probabilities)))
      (list r1 r2 r3))))

(test delete-all-codelets
  (begin
    (b:reset)
    (set! b:deleted '())
    (tell *coderack* 'post
      (tell bond-builder 'make-codelet 50 (b:make-fake-structure 'bond 1)))
    (tell *coderack* 'post
      (tell group-builder 'make-codelet 50 (b:make-fake-structure 'group 2)))
    (tell *coderack* 'post
      (tell description-builder 'make-codelet 50 (b:make-fake-structure 'description 3)))
    (b:post-n 5 0)
    (let* ((r (tell *coderack* 'delete-all-codelets)))
      (list r b:deleted (b:rack)))))

;;;---------------------------------------------------------------------------
;;; Codelets

(test codelet-accessors
  (begin
    (b:reset)
    (b:set-global! '*codelet-count* 17)
    (let* ((s1 (b:make-fake-structure 'bridge 4))
           (s2 (b:make-fake-structure 'workspace 5))
           (c (tell top-down-description-scout 'make-codelet 102/5 s1 s2))
           (r (tell *coderack* 'post c)))
      (list r (tell c 'object-type) (tell c 'get-relative-urgency)
            (tell c 'codelet-type? top-down-description-scout)
            (tell c 'codelet-type? bond-builder)
            (tell c 'proposed-structure-argument?)
            (tell (tell c 'get-proposed-structure) 'get-value)
            (tell (tell c 'get-argument 0) 'get-value)
            (tell (tell c 'get-argument 1) 'get-value)
            (tell c 'get-time-stamp) (tell c 'get-index-in-bin)
            (b:bin-index (tell c 'get-coderack-bin))))))

(test codelet-run-and-fizzle
  (begin
    (b:reset)
    (set! b:window-log '())
    (set! b:log '())
    (define-codelet-procedure* breaker
      (lambda (x y)
        (log! (list 'breaker (tell x 'get-value) y))
        (if (> (tell x 'get-value) 3) (fizzle))
        (log! (list 'breaker-after (tell x 'get-value)))
        'finished))
    (define-codelet-procedure* jootser
      (lambda ()
        (log! 'jootser)
        'jootser-done))
    (let* ((c1 (tell breaker 'make-codelet 20 (b:make-fake-structure 'letter 2) 'a))
           (c2 (tell breaker 'make-codelet 20 (b:make-fake-structure 'letter 5) 'b))
           (c3 (tell jootser 'make-codelet 50))
           (r1 (tell c1 'run))
           (r2 (tell c2 'run))
           (r3 (tell c3 'run)))
      (list r1 r2 r3 (reverse b:log) (reverse b:window-log) fizzle))))

(test codelet-type-print
  (begin
    (b:reset)
    (tell rule-scout 'clamp 65)
    (let* ((s1 (b:capture (lambda () (print rule-scout))))
           (r (tell rule-scout 'unclamp))
           (s2 (b:capture (lambda () (print rule-scout))))
           (s3 (b:capture (lambda () (print bottom-up-description-scout)))))
      (list s1 r s2 s3))))

(test codelet-print
  (begin
    (b:reset)
    (b:set-global! '*codelet-count* 3)
    (let* ((c1 (tell bond-evaluator 'make-codelet 102/5 (b:make-fake-structure 'bond 7)))
           (c2 (tell top-down-bond-scout:category 'make-codelet 33.3
                 (b:make-fake-structure 'slipnode 8) (b:make-fake-structure 'string 9)))
           (c3 (tell breaker 'make-codelet 7)))
      (tell *coderack* 'post c1)
      (tell *coderack* 'post c3)
      (list (b:capture (lambda () (print c1)))
            (b:capture (lambda () (print c2)))
            (b:capture (lambda () (print c3)))))))

(test coderack-print
  (begin
    (b:reset)
    (b:post-n 40 4)
    (b:set-global! '*temperature* 62)
    (list (b:capture (lambda () (print *coderack*)))
          (b:capture (lambda () (tell *coderack* 'show-bin 2)))
          (b:capture (lambda () (tell *coderack* 'show-bin 6))))))

;;;---------------------------------------------------------------------------
;;; Bottom-up and top-down posting (with the fakes)

(define b:posting-row
  (lambda (types)
    (map (lambda (t)
           (list (tell t 'get-codelet-type-name)
                 (post-codelet-probability t)
                 (num-of-codelets-to-post t)
                 (bottom-up-urgency t)))
         types)))

(test posting-parameters
  (map (lambda (settings)
         (set! b:ws-settings settings)
         (map (lambda (modes)
                (b:set-global! '%justify-mode% (car modes))
                (b:set-global! '%self-watching-enabled% (cadr modes))
                (b:set-global! '*temperature* (caddr modes))
                (list (b:posting-row *codelet-types*)
                      (thematic-codelet-urgency thematic-bridge-scout)))
              (b:copy (list (list #f #t 30) (list #t #t 80) (list #f #f 0) (list #t #f 100)))))
       b:settings-list))

(test posting-with-clamps
  (begin
    (set! b:ws-settings (car b:settings-list))
    (b:set-global! '%justify-mode% #f)
    (b:set-global! '%self-watching-enabled% #t)
    (tell breaker 'clamp 70)
    (tell answer-justifier 'clamp 70)
    (tell jootser 'clamp 33)
    (let* ((row (b:posting-row *codelet-types*)))
      (tell breaker 'unclamp)
      (tell answer-justifier 'unclamp)
      (tell jootser 'unclamp)
      row)))

(test add-bottom-up-codelets
  (map (lambda (settings)
         (set! b:ws-settings settings)
         (map (lambda (seed)
                (b:reset)
                (b:post-n 20 seed)
                (b:set-global! '%justify-mode% (odd? seed))
                (b:set-global! '%self-watching-enabled% (< seed 3))
                (b:set-global! '*temperature* (* 20 seed))
                (b:seeded seed
                  (lambda ()
                    (add-bottom-up-codelets)
                    (tell *coderack* 'post-deferred-codelets)
                    (b:rack))))
              (b:copy (list 1 2 3 4))))
       b:settings-list))

(test add-top-down-codelets
  (map (lambda (settings)
         (set! b:ws-settings settings)
         (map (lambda (seed)
                (b:reset)
                (set! b:top-down-log '())
                (b:post-n 95 seed)
                (b:set-global! '%self-watching-enabled% (odd? seed))
                (b:seeded seed
                  (lambda ()
                    (add-top-down-codelets)
                    (tell *coderack* 'post-deferred-codelets)
                    (list (reverse b:top-down-log) (b:rack)))))
              (b:copy (list 1 2 3))))
       b:settings-list))

(b:set-global! '%justify-mode% #f)
(b:set-global! '%self-watching-enabled% #t)

;;;---------------------------------------------------------------------------
;;; constants.ss: the translation-temperature threshold distributions

(test threshold-distributions
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (d)
                    (list (tell d 'object-type)
                          (b:repeat 12 (lambda () (tell d 'choose-value)))))
                  (b:copy
                    (list %very-low-translation-temperature-threshold-distribution%
                          %low-translation-temperature-threshold-distribution%
                          %medium-translation-temperature-threshold-distribution%
                          %high-translation-temperature-threshold-distribution%
                          %very-high-translation-temperature-threshold-distribution%))))))
       b:seeds))

;;;---------------------------------------------------------------------------
;;; setup.ss user commands (with logging windows)

(define b:cmd-log '())
(define b:make-log-window
  (lambda (name)
    (lambda msg
      (set! b:cmd-log (cons (cons name (cdr msg)) b:cmd-log))
      (if (eq? (cadr msg) 'verbose-mode?) #f 'done))))

(test setup-commands
  (begin
    (b:set-global! '*comment-window* (b:make-log-window 'comment))
    (b:set-global! '*slipnet-window* (b:make-log-window 'slipnet))
    (b:set-global! '*coderack-window* (b:make-log-window 'coderack))
    (b:set-global! '*control-panel* (b:make-log-window 'control-panel))
    (b:set-global! '*display-mode?* #f)
    (set! b:cmd-log '())
    (let* ((r1 (eliza-mode-off)) (v1 %eliza-mode%)
           (r2 (eliza-mode-on)) (v2 %eliza-mode%)
           (r3 (slipnet-off)) (v3 %slipnet-graphics%)
           (r4 (slipnet-on)) (v4 %slipnet-graphics%)
           (r5 (coderack-off)) (v5 %coderack-graphics%)
           (r6 (coderack-on)) (v6 %coderack-graphics%)
           (r7 (codelet-counts-on)) (v7 %codelet-count-graphics%)
           (r8 (codelet-counts-off)) (v8 %codelet-count-graphics%)
           (r9 (verbose-on)) (r10 (verbose-off))
           (r (list r1 v1 r2 v2 r3 v3 r4 v4 r5 v5 r6 v6 r7 v7 r8 v8 r9 r10)))
      (b:set-global! '%slipnet-graphics% #f)
      (b:set-global! '%coderack-graphics% #f)
      (list r (reverse b:cmd-log)))))

;;;---------------------------------------------------------------------------
;;; descriptions.ss: what can run before the Workspace is ported

(define b:make-fake-description
  (lambda (type descriptor)
    (lambda msg
      (record-case (cdr msg)
        (object-type () 'description)
        (get-description-type () type)
        (get-descriptor () descriptor)
        (else 'invalid-message-indicator)))))

(define b:descs
  (map (lambda (k) (b:make-fake-description (modulo k 2) (modulo k 3))) (b:iota 6)))

(test descriptions-equal
  (map (lambda (d1) (map (lambda (d2) (descriptions-equal? d1 d2)) b:descs)) b:descs))

(test description-member
  (map (lambda (d)
         (list (description-member? d '())
               (description-member? d (b:copy (list (car b:descs))))
               (description-member? d (cdr b:descs))))
       b:descs))

(test description-codelet-types-have-procedures
  (map (lambda (t) (tell t 'get-codelet-type-name))
       (b:copy (list bottom-up-description-scout top-down-description-scout
                     description-evaluator description-builder))))

;;; rule-battery.scm -- the codelet-level differential checks of item 09:
;;; rules.ss and answers.ss.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss       (Chez, with the whole original loaded)
;;;   racket/tests/rule-diff-test.rkt       (Racket: compat + utilities + engine)
;;; The harness of items 07-08 (codelet-harness.scm) with b:bridges? and
;;; b:rules? on: run.ss's initial codelets, every bottom-up codelet type of
;;; the original posted by its own add-bottom-up-codelets (self-watching off),
;;; check-if-rules-possible at every update, and all of *top-down-slipnodes*.
;;; So rule scouts abstract rules from the top bridges, rule evaluators and
;;; builders build them, and answer finders translate them, apply them to
;;; the target string, build the translated (answer) string, hit snags and
;;; report answers.  For every problem in tests/problems.txt and each of its
;;; seeds, the trace runs up to the codelet that reports the first answer (or
;;; b:codelet-cap codelets): besides everything items 07-08 record, every
;;; rule built and broken (type, English transcription, clauses, strength,
;;; quality values, supporting bridges), the rule monitor, snags (failure
;;; result, rules, slippages), the answer (letters, groups of the answer
;;; string, both rules, bridges, slippage log, quality) and the commentary
;;; that answers.ss writes.  Then a summary of the first answers, and rules
;;; applied and translated directly.
;;;
;;; What the files not ported yet would do is replaced by fakes, the same in
;;; both runners: the Trace (trace.ss: events kept in a list; no snag or
;;; clamp periods), the Memory (memory.ss: no answer or snag is ever
;;; present), answer and snag events (trace.ss: the answer's quality computed
;;; as get-absolute-quality does; a snag's activate does nothing),
;;; abstract-answer/snag-description (memory.ss), monitor-new-rules
;;; (trace.ss), the Commentary window, suspend (run.ss: ends the run), and
;;; answer-justifier's procedure (justify.ss: records its call).  A snag
;;; clamps the temperature at 100 (process-snag); the fake Trace then has a
;;; snag period whose progress is always 50, so each update ends it, and
;;; unclamps the temperature, with probability 1/2 (the real Trace measures
;;; the structures built since the snag).

(load "tests/diff/codelet-harness.scm")

(b:enable-bridges! #t)
(set! b:rules? #t)

(define b:codelet-cap 2500)

;;;---------------------------------------------------------------------------
;;; Fakes

(define b:nms (lambda (nodes) (map b:nm nodes)))

(define b:slippage-log-data
  (lambda (log)
    (list (b:datum (tell log 'get-directly-applied-slippages))
          (b:datum (tell log 'get-coattail-slippages))
          (b:datum (tell log 'get-coattail-inducing-slippages))
          (map b:bridge-data (tell log 'get-slippage-bridges)))))

(define b:answer-data
  (lambda (answer-string top-rule bottom-rule vertical-bridges groups
           top-refs bottom-refs slippage-log unjustified quality)
    (list 'answer
          (b:nms (tell answer-string 'get-letter-categories))
          (tell answer-string 'print-name)
          (map b:group-data (tell answer-string 'get-groups))
          (b:rule-data top-rule) (b:rule-data bottom-rule)
          (list (tell bottom-rule 'get-quality) (tell bottom-rule 'get-uniformity)
                (tell bottom-rule 'get-abstractness) (tell bottom-rule 'get-succinctness))
          (tell bottom-rule 'translated?)
          (b:datum (tell bottom-rule 'get-supporting-horizontal-bridges))
          (map b:bridge-data vertical-bridges)
          (map b:obj-id groups)
          (b:datum top-refs) (b:datum bottom-refs)
          (b:slippage-log-data slippage-log)
          (b:datum unjustified)
          quality)))

(define b:fake-answer-event
  (lambda (initial modified target answer-string top-rule bottom-rule
           vertical-bridges groups top-refs bottom-refs slippage-log unjustified)
    (let ((temperature *temperature*))
      (lambda msg
        (let ((self (car msg)))
          (record-case (cdr msg)
            (get-type () 'answer)
            (type? (type) (eq? type 'answer))
            (get-temperature () temperature)
            (get-quality ()
              (round (weighted-average
                       (list (tell top-rule 'get-quality) (100- temperature))
                       (list 60 40))))
            (b:data ()
              (b:answer-data answer-string top-rule bottom-rule vertical-bridges
                groups top-refs bottom-refs slippage-log unjustified
                (tell self 'get-quality)))
            (else (error 'fake-answer-event "unexpected message" msg))))))))

(define b:fake-snag-event
  (lambda (failure-result rule translated-rule vertical-bridges slippage-log
           rule-ref-objects)
    (lambda msg
      (record-case (cdr msg)
        (get-type () 'snag)
        (type? (type) (eq? type 'snag))
        (get-explanation ()
          (string-append "the " (symbol->string (car failure-result)) " failed"))
        (activate () (b:event! (list 'snag-activate)))
        (b:data ()
          (list 'snag (b:datum failure-result)
                (b:rule-data rule) (b:rule-data translated-rule)
                (map b:bridge-data vertical-bridges)
                (b:slippage-log-data slippage-log)
                (b:datum rule-ref-objects)))
        (else (error 'fake-snag-event "unexpected message" msg))))))

(define b:trace-events '())
(define b:snag-period? #f)

;; the first answer of the current run (b:answer-data), and of every run so
;; far, newest first
(define b:first-answer #f)
(define b:first-answers '())

(define b:fake-trace
  (lambda msg
    (record-case (cdr msg)
      (add-event (event)
        (set! b:trace-events (cons event b:trace-events))
        (if (tell event 'type? 'snag) (set! b:snag-period? #t))
        (let* ((data (tell event 'b:data)))
          (if (and (eq? (car data) 'answer) (not b:first-answer))
              (set! b:first-answer data))
          (b:event! (list 'trace-event data)))
        'done)
      (get-num-of-events (type)
        (let loop ((l b:trace-events) (n 0))
          (cond
            ((null? l) n)
            ((tell (car l) 'type? type) (loop (cdr l) (+ n 1)))
            (else (loop (cdr l) n)))))
      (get-all-events () (reverse b:trace-events))
      (within-clamp-period? () #f)
      (within-snag-period? () b:snag-period?)
      (progress-since-last-snag () 50)
      (undo-snag-condition ()
        (set! b:snag-period? #f)
        (b:set-global! '*temperature-clamped?* #f)
        (b:event! (list 'undo-snag-condition))
        'done)
      (else (error 'fake-trace "unexpected message" msg)))))

(define b:fake-memory
  (lambda msg
    (record-case (cdr msg)
      (answer-present? (letters rule translated-rule)
        (b:event! (list 'memory-answer-present? (b:nms letters)
                        (b:rule-data rule) (b:rule-data translated-rule)))
        #f)
      (snag-present? (rule)
        (b:event! (list 'memory-snag-present? (b:rule-data rule)))
        #f)
      (get-equivalent-snag (answer) #f)
      (else (error 'fake-memory "unexpected message" msg)))))

(define b:fake-comment-window
  (lambda msg
    (b:event! (cons 'comment (cdr msg)))))

(b:set-global! '*trace* b:fake-trace)
(b:set-global! '*memory* b:fake-memory)
(b:set-global! '*comment-window* b:fake-comment-window)
(b:set-global! 'make-answer-event b:fake-answer-event)
(b:set-global! 'make-snag-event b:fake-snag-event)
(b:set-global! 'abstract-answer-description
  (lambda (event) (b:event! (list 'abstract-answer-description))))
(b:set-global! 'abstract-snag-description
  (lambda (event) (b:event! (list 'abstract-snag-description))))
(b:set-global! 'monitor-new-rules
  (lambda (rule) (b:event! (list 'new-rule (b:rule-data rule)))))
(b:set-global! 'suspend
  (lambda () (set! b:answered #t) (b:event! (list 'suspend))))
(b:set-global! 'update-everything b:update-everything)
(b:set-global! 'post-initial-codelets b:post-initial-codelets)
(tell answer-justifier 'set-codelet-procedure
  (lambda () (b:event! (list 'answer-justifier)) 'done))

;;;---------------------------------------------------------------------------
;;; The traces

(define b:run-problem
  (lambda (strings seed k)
    (set! b:trace-events '())
    (set! b:snag-period? #f)
    (set! b:first-answer #f)
    (let* ((trace (b:run-codelets strings seed k)))
      (set! b:first-answers
        (cons (if b:answered
                  ;; (answered PROBLEM SEED CODELET LETTERS QUALITY TOP-RULE
                  ;;  TRANSLATED-RULE)
                  (list 'answered strings seed *codelet-count*
                        (list-ref b:first-answer 1) (list-ref b:first-answer 15)
                        (list-ref (list-ref b:first-answer 4) 2)
                        (list-ref (list-ref b:first-answer 5) 2))
                  (list 'no-answer strings seed *codelet-count*))
              b:first-answers))
      trace)))

(define b:trace-lines
  (lambda (strings seed k)
    (let* ((trace (b:run-problem strings seed k)))
      (apply string-append
        (map (lambda (entry)
               (string-append (b:compact strings) " " (number->string seed) " "
                              (b:compact entry) "\n"))
             trace)))))

;; the trace lines of problem i, all its seeds, as one string
(define b:problem-trace
  (lambda (i)
    (let* ((p (list-ref b:problems i)))
      (let loop ((seeds (cadr p)) (acc '()))
        (if (null? seeds)
            (apply string-append (reverse acc))
            (let* ((lines (b:trace-lines (car p) (car seeds) b:codelet-cap)))
              (loop (cdr seeds) (cons lines acc))))))))

(test rules-problem-count (length b:problems))
(test rules-00 (b:problem-trace 0))
(test rules-01 (b:problem-trace 1))
(test rules-02 (b:problem-trace 2))
(test rules-03 (b:problem-trace 3))
(test rules-04 (b:problem-trace 4))
(test rules-05 (b:problem-trace 5))
(test rules-06 (b:problem-trace 6))
(test rules-07 (b:problem-trace 7))
(test rules-08 (b:problem-trace 8))
(test rules-09 (b:problem-trace 9))
(test rules-10 (b:problem-trace 10))
(test rules-11 (b:problem-trace 11))
(test rules-12 (b:problem-trace 12))
(test rules-13 (b:problem-trace 13))
(test rules-14 (b:problem-trace 14))
(test rules-15 (b:problem-trace 15))
(test rules-16 (b:problem-trace 16))
(test rules-17 (b:problem-trace 17))
(test rules-18 (b:problem-trace 18))
(test rules-19 (b:problem-trace 19))
(test rules-20 (b:problem-trace 20))
(test rules-21 (b:problem-trace 21))
(test rules-22 (b:problem-trace 22))
(test rules-23 (b:problem-trace 23))
(test rules-24 (b:problem-trace 24))
(test rules-25 (b:problem-trace 25))
(test rules-26 (b:problem-trace 26))
(test rules-27 (b:problem-trace 27))
(test rules-28 (b:problem-trace 28))
(test rules-29 (b:problem-trace 29))
(test rules-30 (b:problem-trace 30))
(test rules-31 (b:problem-trace 31))
(test rules-32 (b:problem-trace 32))
(test rules-33 (b:problem-trace 33))
(test rules-34 (b:problem-trace 34))
(test rules-35 (b:problem-trace 35))

;; every run's first answer: the answer string, its quality, the rule and
;; its translation in English, and the codelet that found it
(test first-answers (b:compact (reverse b:first-answers)))

;;;---------------------------------------------------------------------------
;;; Rules applied and translated directly.  After a run of the harness (up
;;; to its first answer), for every rule of the Workspace: the rule, whether
;;; it currently works, the result of applying it to the string it describes
;;; (apply-rule with ignore-snag: the transforms per object, then the letters
;;; of the string's image), and its translation (translate: the translated
;;; rule, the vertical bridges, the slippage log, the supporting groups and
;;; the reference objects), applied to the other string likewise.
;;; translate draws (it ignores dimensions with probability 0.4), so the
;;; generator state is recorded after each rule.

(define b:apply-data
  (lambda (rule string)
    (let* ((result (apply-rule rule string ignore-snag)))
      (list (b:datum result)
            (b:nms (tell string 'generate-image-letters))))))

(define b:examine-rule
  (lambda (rule)
    (let* ((type (tell rule 'get-rule-type))
           (from (if (eq? type 'top) *initial-string* *target-string*))
           (to (if (eq? type 'top) *target-string* *initial-string*))
           (entry (b:rule-entry rule))
           (works (tell rule 'currently-works?))
           (applied (b:apply-data rule from))
           (result (translate rule))
           (translation
             (if result
                 (let* ((translated (list-ref result 0))
                        (applied-to (b:apply-data translated to)))
                   (list (b:rule-data translated)
                         (map b:bridge-data (list-ref result 1))
                         (b:slippage-log-data (list-ref result 2))
                         (map b:obj-id (list-ref result 3))
                         (b:datum (list-ref result 4))
                         (b:datum (list-ref result 5))
                         applied-to))
                 #f)))
      (list entry works applied translation (random-seed)))))

(define b:rule-matrix
  (lambda (strings seed k)
    (b:run-problem strings seed k)
    (let loop ((rules (tell *workspace* 'get-all-rules)) (acc '()))
      (if (null? rules)
          (b:compact (reverse acc))
          (let* ((v (b:examine-rule (car rules))))
            (loop (cdr rules) (cons v acc)))))))

(test rule-matrix-abc-abd-xyz (b:rule-matrix '(abc abd xyz) 1 1500))
(test rule-matrix-abc-abd-xyz-2 (b:rule-matrix '(abc abd xyz) 2 1500))
(test rule-matrix-abc-abd-mrrjjj (b:rule-matrix '(abc abd mrrjjj) 2 1500))
(test rule-matrix-abc-abd-kji (b:rule-matrix '(abc abd kji) 3 1500))
(test rule-matrix-abc-abd-iijjkk (b:rule-matrix '(abc abd iijjkk) 1 1500))
(test rule-matrix-abc-aabbcc-kkjjii (b:rule-matrix '(abc aabbcc kkjjii) 1 2000))
(test rule-matrix-eqe-qeq-abbbc (b:rule-matrix '(eqe qeq abbbc) 2 2000))
(test rule-matrix-apc-abc-opc (b:rule-matrix '(apc abc opc) 2 1500))
(test rule-matrix-abc-ccbbaa-ijk (b:rule-matrix '(abc ccbbaa ijk) 1 1500))
(test rule-matrix-xqc-xqd-mrrjjj-mrrjjjj (b:rule-matrix '(xqc xqd mrrjjj mrrjjjj) 2 1500))
(test rule-matrix-rst-rsu-xyz-uyz (b:rule-matrix '(rst rsu xyz uyz) 1 1500))
(test rule-matrix-abc-abd-mrrjjj-mrrkkk (b:rule-matrix '(abc abd mrrjjj mrrkkk) 1 1500))

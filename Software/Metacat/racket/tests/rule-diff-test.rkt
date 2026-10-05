#lang racket/base
;; Item 09: rules.ss and answers.ss in the codelet-level differential
;; harness.  tests/diff/rule-battery.scm runs codelet-harness.scm with
;; b:bridges? and b:rules? on (the item-08 setting plus the original's
;; add-bottom-up-codelets over all bottom-up types, self-watching off, and
;; check-if-rules-possible at every update; fakes for the Trace, Memory,
;; events, Commentary and suspend) for every problem × seed of
;; tests/problems.txt up to the first answer, under Chez Scheme 10 with
;; chez_scheme/original/ loaded and here with the engine; then a summary of
;; the first answers and rules applied and translated directly.  The traces
;; (rules built and broken with their English, clauses, strengths and
;; quality values, snags, the answer with its string, rules, bridges and
;; slippage log, the commentary, besides everything items 07-08 record) must
;; agree line for line; the first differing line is reported.
(require rackunit
         racket/runtime-path
         racket/string
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/rule-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")
(define-runtime-path engine "../engine.rkt")

(define chez (string-split (chez-output battery) "\n" #:trim? #f))
(define rkt (string-split (racket-output battery (list compat utilities engine) 'set-global!)
                          "\n" #:trim? #f))

(define (short s) (if (> (string-length s) 600) (string-append (substring s 0 600) " ...") s))

(define test-count
  (with-input-from-file battery
    (lambda ()
      (for/sum ([form (in-port read)])
        (if (and (pair? form) (eq? (car form) 'test)) 1 0)))))
(check-equal? (length (filter (lambda (l) (regexp-match? #rx"^[^ ]+ => " l)) chez))
              test-count "Chez printed one result per test")
(check-false (ormap (lambda (l) (regexp-match? #rx"^[^ ]+ => ERROR$" l)) chez)
             "no battery test fails under Chez")
(check-true (> (length chez) 100000) "the traces have one line per codelet")
;; what the runs reach
(define chez-text (string-join chez "\n"))
(for ([type '("rule-scout" "rule-evaluator" "rule-builder" "answer-finder"
              "answer-justifier")])
  (check-true (regexp-match? (regexp (string-append "[(][0-9]+ " (regexp-quote type) " "))
                             chez-text)
              type))
(check-true (regexp-match? #rx"[(]new-rule [(]rule top " chez-text) "a top rule is built")
(check-true (regexp-match? #rx"[(]new-rule [(]rule bottom " chez-text)
            "a bottom rule is built (justify runs)")
(check-true (regexp-match? #rx"[(]trace-event [(]answer " chez-text) "an answer is reported")
(check-true (regexp-match? #rx"[(]trace-event [(]snag " chez-text) "a snag is hit")
(check-true (regexp-match? #rx"[(]comment add-comment [(]\"The answer " chez-text)
            "answers.ss writes the commentary")
(check-true (regexp-match? #rx"[(]comment add-comment [(]\"Uh-oh" chez-text)
            "answers.ss comments on a snag")
(check-true (regexp-match? #rx"[(]memory-answer-present[?] " chez-text)
            "answer-finder asks the Memory")
;; the first answers: at least one per non-justify problem family, and the
;; summary lists every run
(check-true (regexp-match? #rx"first-answers => " chez-text))
(check-true (>= (length (regexp-match* #rx"[(]answered " chez-text)) 30)
            "at least 30 runs reach an answer")
(check-true (regexp-match? #rx"rule-matrix-[a-z0-9-]+ => \"[(][(][(][(]rule " chez-text)
            "the rule matrix examines rules")

(let loop ([c chez] [r rkt] [n 1])
  (cond
    [(and (null? c) (null? r)) (check-true #t)]
    [(null? r) (fail (format "Racket trace ends early, at line ~a; Chez has: ~a"
                             n (short (car c))))]
    [(null? c) (fail (format "Racket trace is longer, from line ~a: ~a" n (short (car r))))]
    [(string=? (car c) (car r)) (loop (cdr c) (cdr r) (add1 n))]
    [else (fail (format "traces differ at line ~a\n  Chez:   ~a\n  Racket: ~a"
                        n (short (car c)) (short (car r))))]))

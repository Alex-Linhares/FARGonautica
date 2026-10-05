#lang racket/base
;; Item 08: bridges.ss and breakers.ss in the codelet-level differential
;; harness.  tests/diff/bridge-battery.scm runs codelet-harness.scm with
;; b:bridges? on (run.ss's initial bond and bridge scouts; every bottom-up
;; codelet type but those of rules, answers and self-watching, so bridge
;; scouts, description scouts and the breaker too; all top-down slipnodes)
;; for every problem × seed of tests/problems.txt, under Chez Scheme 10 with
;; chez_scheme/original/ loaded and here with the engine.  The traces (one
;; line per codelet and per update cycle; bridges built and broken with
;; their concept mappings, flipped groups and strengths, proposed bridges,
;; description counts, the concept-mapping monitor and Themespace boosts,
;; besides item 07's bonds, groups, temperature and activations) must agree
;; line for line; the first differing line is reported.
(require rackunit
         racket/runtime-path
         racket/string
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/bridge-battery.scm")
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
(for ([type '("bottom-up-bridge-scout" "important-object-bridge-scout"
              "bridge-evaluator" "bridge-builder" "breaker"
              "bottom-up-description-scout" "top-down-description-scout"
              "description-evaluator" "description-builder")])
  (check-true (regexp-match? (regexp (string-append "[(][0-9]+ " (regexp-quote type) " "))
                             chez-text)
              type))
(check-true (regexp-match? #rx"[(]built [(][(][(]bridge top " chez-text) "a top bridge is built")
(check-true (regexp-match? #rx"[(]built [(][(][(]bridge vertical " chez-text)
            "a vertical bridge is built")
(check-true (regexp-match? #rx"[(]built [(][(][(]bridge bottom " chez-text)
            "a bottom bridge is built (justify runs)")
(check-true (regexp-match? #rx"[(]broken [(][^\n]*[(][(]bridge " chez-text) "a bridge is broken")
(check-true (regexp-match? #rx"breaker [0-9/.]+ [0-9]+ [0-9]+ [(]built [(][)][)] [(]broken [(][(]"
                           chez-text)
            "the breaker breaks a structure")
(check-true (regexp-match? #rx"[(]bridge [a-z]+ [(][^)]*[)] [(][^)]*[)] (#t #f|#f #t|#t #t)[)]"
                           chez-text)
            "a bridge with a flipped group")
(check-true (regexp-match? #rx"[(]new-cms " chez-text) "monitor-new-concept-mappings is called")
(check-true (regexp-match? #rx"[(]add-theme " chez-text) "bridge-builder boosts themes")

(let loop ([c chez] [r rkt] [n 1])
  (cond
    [(and (null? c) (null? r)) (check-true #t)]
    [(null? r) (fail (format "Racket trace ends early, at line ~a; Chez has: ~a"
                             n (short (car c))))]
    [(null? c) (fail (format "Racket trace is longer, from line ~a: ~a" n (short (car r))))]
    [(string=? (car c) (car r)) (loop (cdr c) (cdr r) (add1 n))]
    [else (fail (format "traces differ at line ~a\n  Chez:   ~a\n  Racket: ~a"
                        n (short (car c)) (short (car r))))]))

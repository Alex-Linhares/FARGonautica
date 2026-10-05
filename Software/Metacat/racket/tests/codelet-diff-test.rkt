#lang racket/base
;; Item 07: the codelet-level differential harness.  bonds.ss, groups.ss and
;; concept-mappings.ss run codelet by codelet in the oracle and in the port:
;; tests/diff/codelet-battery.scm (with tests/diff/codelet-harness.scm) runs
;; every problem of tests/problems.txt, each of its seeds, for its first
;; codelets with only the bond and group codelet types enabled, under Chez
;; Scheme 10 with chez_scheme/original/ loaded and here with the engine.  The
;; traces (one line per codelet and per update cycle: codelet type, urgency,
;; time stamp, generator state, structures built and broken, monitor calls,
;; temperature, activations, workspace objects) must agree line for line;
;; the first differing line is reported, so a failure names the problem,
;; the seed and the codelet where the runs part.
(require rackunit
         racket/runtime-path
         racket/string
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/codelet-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")
(define-runtime-path engine "../engine.rkt")

(define chez (string-split (chez-output battery) "\n" #:trim? #f))
(define rkt (string-split (racket-output battery (list compat utilities engine) 'set-global!)
                          "\n" #:trim? #f))

(define (short s) (if (> (string-length s) 600) (string-append (substring s 0 600) " ...") s))

;; one result per test, none an error
(define test-count
  (with-input-from-file battery
    (lambda ()
      (for/sum ([form (in-port read)])
        (if (and (pair? form) (eq? (car form) 'test)) 1 0)))))
(check-equal? (length (filter (lambda (l) (regexp-match? #rx"^[^ ]+ => " l)) chez))
              test-count "Chez printed one result per test")
(check-false (ormap (lambda (l) (regexp-match? #rx"^[^ ]+ => ERROR$" l)) chez)
             "no battery test fails under Chez")
(check-true (> (length chez) 50000) "the traces have one line per codelet")
;; what the runs reach: every enabled codelet type, bonds and groups built
;; and broken, and group-builder's consolidation of sameness groups (whose
;; ungated group-graphics call messages the Workspace window)
(define chez-text (string-join chez "\n"))
(for ([type '("bottom-up-bond-scout" "top-down-bond-scout:category"
              "top-down-bond-scout:direction" "bond-evaluator" "bond-builder"
              "top-down-group-scout:category" "top-down-group-scout:direction"
              "group-scout:whole-string" "group-evaluator" "group-builder")])
  (check-true (regexp-match? (regexp (string-append "[(][0-9]+ " (regexp-quote type) " "))
                             chez-text)
              type))
(check-true (regexp-match? #rx"[(]built [(][(][(]bond " chez-text) "a bond is built")
(check-true (regexp-match? #rx"[(]built [(][(][(]group " chez-text) "a group is built")
(check-true (regexp-match? #rx"[(]broken [(][(][(]bond " chez-text) "a bond is broken")
(check-true (regexp-match? #rx"[(]broken [(][(][(]group " chez-text) "a group is broken")
(check-true (regexp-match? #rx"[(]window caching-on[)]" chez-text) "group-graphics is called")

;; the first line where the traces differ
(let loop ([c chez] [r rkt] [n 1])
  (cond
    [(and (null? c) (null? r)) (check-true #t)]
    [(null? r) (fail (format "Racket trace ends early, at line ~a; Chez has: ~a"
                             n (short (car c))))]
    [(null? c) (fail (format "Racket trace is longer, from line ~a: ~a" n (short (car r))))]
    [(string=? (car c) (car r)) (loop (cdr c) (cdr r) (add1 n))]
    [else (fail (format "traces differ at line ~a\n  Chez:   ~a\n  Racket: ~a"
                        n (short (car c)) (short (car r))))]))

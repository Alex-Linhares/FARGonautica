#lang racket/base
;; Item 03: differential checks of compat.rkt and utilities.rkt against the
;; original.  tests/diff/utilities-battery.scm is evaluated twice: by Chez
;; Scheme 10 with chez_scheme/original/ loaded (chez_scheme/oracle/diff-eval.ss),
;; and here, in a namespace made of racket/base + compat.rkt + utilities.rkt.
;; Every line of output (one per test form) must be identical.
(require racket/runtime-path
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/utilities-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")

(check-battery battery (list compat utilities))

#lang racket/base
;; Item 04: differential checks of the engine's constants.ss, setup.ss,
;; coderack.ss and descriptions.ss against the original.
;; tests/diff/coderack-battery.scm is evaluated by Chez Scheme 10 with
;; chez_scheme/original/ loaded (chez_scheme/oracle/diff-eval.ss), and here in
;; a namespace made of racket/base + compat.rkt + utilities.rkt + engine.rkt,
;; with the engine's set-global! as b:set-global!.  Every line of output must
;; be identical: the coderack's bins, urgencies, posting, deletion and
;; selection, draw for draw.
(require racket/runtime-path
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/coderack-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")
(define-runtime-path engine "../engine.rkt")

(check-battery battery (list compat utilities engine) #:set-global! 'set-global!)

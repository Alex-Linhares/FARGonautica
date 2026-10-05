#lang racket/base
;; Item 05: differential checks of the engine's slipnet.ss and images.ss
;; against the original.  tests/diff/slipnet-battery.scm is evaluated by Chez
;; Scheme 10 with chez_scheme/original/ loaded (chez_scheme/oracle/diff-eval.ss),
;; and here in a namespace made of racket/base + compat.rkt + utilities.rkt +
;; engine.rkt, with the engine's set-global! as b:set-global!.  Every line of
;; output must be identical: the initial slipnet (nodes, links, lengths,
;; conceptual depths), activation spreading and decay over 20 updates from
;; fixed states with the generator state after each, slippages, top-down
;; codelets and images.
(require racket/runtime-path
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/slipnet-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")
(define-runtime-path engine "../engine.rkt")

(check-battery battery (list compat utilities engine) #:set-global! 'set-global!)

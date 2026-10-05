#lang racket/base
;; Item 13: differential checks of the engine's part of the graphics files
;; (general-graphics.ss's pexp builders and text helpers, group-graphics.ss,
;; bridge-graphics.ss, rule-graphics.ss) against the original.
;; tests/diff/graphics-battery.scm is evaluated by Chez Scheme 10 with the
;; original loaded and here with racket/engine.rkt; the pexps (every
;; coordinate) and the Workspace-window messages must be identical.
(require racket/runtime-path
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/graphics-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")
(define-runtime-path engine "../engine.rkt")

(check-battery battery (list compat utilities engine) #:set-global! 'set-global!)

#lang racket/base
;; Item 06: differential checks of the engine's workspace.ss,
;; workspace-objects.ss, workspace-structures.ss, workspace-strings.ss,
;; workspace-structure-formulas.ss and formulas.ss against the original.
;; tests/diff/workspace-battery.scm is evaluated by Chez Scheme 10 with
;; chez_scheme/original/ loaded (chez_scheme/oracle/diff-eval.ss), and here in
;; a namespace made of racket/base + compat.rkt + utilities.rkt + engine.rkt,
;; with the engine's set-global! as b:set-global!.  Every line of output must
;; be identical: above all, the initial workspace (letters, descriptions,
;; salience, importance and unhappiness values) of every problem in
;; tests/problems.txt, for each of its seeds.
(require racket/runtime-path
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/workspace-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")
(define-runtime-path engine "../engine.rkt")

(check-battery battery (list compat utilities engine) #:set-global! 'set-global!)

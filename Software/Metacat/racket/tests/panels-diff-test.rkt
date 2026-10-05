#lang racket/base
;; Item 14: differential checks of the panel code that works without a
;; window (slipnet-, coderack-, temperature-, theme-, trace-, memory-,
;; commentary- and eeg-graphics.ss: pexp builders, Themespace panel layout
;; and panel objects, the Trace and Memory windows' mouse handlers, layout
;; tables, the EEG object) against the original.  tests/diff/panels-battery.scm
;; is evaluated by Chez Scheme 10 with the original loaded and here with
;; racket/engine.rkt and racket/gui/views.rkt; every pexp (every coordinate)
;; and every message to the fake windows and model objects must be
;; identical.  The windows are checked by views-test.rkt.
(require racket/runtime-path
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/panels-battery.scm")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path utilities "../utilities.rkt")
(define-runtime-path engine "../engine.rkt")
(define-runtime-path views "../gui/views.rkt")

(check-battery battery (list compat utilities engine views) #:set-global! 'set-global!)

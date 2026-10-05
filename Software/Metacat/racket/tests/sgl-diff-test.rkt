#lang racket/base
;; Item 12: differential checks of racket/gui/sgl.rkt's SGL interpreter
;; against the original's sgl-interpreter.ss.  tests/diff/sgl-battery.scm is
;; evaluated by Chez Scheme 10 with the original loaded and its
;; sgl-interpreter.ss reloaded against a recording `send'
;; (tests/diff/sgl-chez-setup.ss), and here with racket/tests/sgl-recorder.rkt.
;; The viewport messages (one per Tk canvas item the original creates, with
;; every argument) must be identical.
(require racket/runtime-path
         "diff-runner.rkt")

(define-runtime-path battery "../../tests/diff/sgl-battery.scm")
(define-runtime-path setup "../../tests/diff/sgl-chez-setup.ss")
(define-runtime-path compat "../compat.rkt")
(define-runtime-path recorder "sgl-recorder.rkt")

(check-battery battery (list compat recorder) #:chez-setup (list setup))

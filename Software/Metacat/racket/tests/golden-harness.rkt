#lang racket/base
;; Items 10-11: a full run of the port, traced in the golden format, and
;; printed as chez_scheme/oracle/run.ss prints it.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;
;; (golden-trace strings seed cap keep-going?) runs a problem with the engine
;; (racket/headless.rkt around the engine's own run.ss) and returns its
;; JSON-lines trace as a string, as chez_scheme/oracle/run.ss --trace writes
;; it for tests/golden/ (docs/trace-format.md).  golden-run returns the
;; trace and the printed output.  Use one fresh engine (namespace) per run.

(require racket/port
         "../headless.rkt")

(provide golden-trace golden-run golden-partial-trace golden-runs golden-file-name)

;; (values trace stdout)
(define (golden-run strings seed cap keep?)
  (define trace (open-output-string))
  (define stdout (open-output-string))
  (parameterize ([current-output-port stdout])
    (run-problem strings seed cap keep? trace))
  (values (get-output-string trace) (get-output-string stdout)))

(define (golden-trace strings seed cap keep?)
  (define-values (trace stdout) (golden-run strings seed cap keep?))
  trace)

(define (golden-partial-trace) (partial-trace))

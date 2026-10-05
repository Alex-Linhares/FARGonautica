#lang racket/base
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;; Metacat is Copyright (c) 1999, 2003 by James B. Marshall; this file is the
;; port's counterpart of chez_scheme/oracle/run.ss, the original run headless.
;;
;;   racket racket/cli.rkt INITIAL MODIFIED TARGET [ANSWER]
;;          [--seed N] [--max-codelets K] [--keep-going] [--trace FILE] [--verbose]
;;
;; Runs one problem with the engine, headless, and prints what the oracle's
;; run.ss prints for the same arguments: the problem, the commentary as it is
;; written, each answer found (answer, quality, codelet count, temperature)
;; and a summary.  With ANSWER the run is a justify run.  Without --seed the
;; seed comes from the clock (the original's randomize) and is printed on the
;; Problem line; giving it back with --seed replays the run.
;;
;; The original stops (suspend) when it finds an answer or gives up, and
;; waits for Go.  The run ends there, or with --keep-going continues as if Go
;; were pressed, until K codelets have run.  --max-codelets K is the
;; original's breakpoint (runtil K): the run stops after codelet K.  Without
;; it there is no cap.  "Stopped:" says why the run ended: suspend, cap, or
;; halt (the original's report-error-and-halt).
;;
;; --trace FILE also writes the run's JSON-lines trace (docs/trace-format.md),
;; which for the runs of tests/problems.txt equals tests/golden/.
;;
;; --verbose turns on the original's verbose mode (gui.ss's Options menu
;; checkbox): the model's vprintf output is printed too, as with the oracle's
;; run.ss --verbose.
;;
;; Exit codes: 0 after a run, 2 for bad arguments, 1 if the run raises an
;; error (as the original does on some runs: docs/anomalies_and_quirks.md).

(require racket/port
         "compat.rkt"
         "utilities.rkt"
         "headless.rkt")

;; racket/metacat.rkt (the standalone executable) runs the CLI as cli-main
(provide (rename-out (main cli-main)))

(define (usage)
  (eprintf "usage: cli.rkt INITIAL MODIFIED TARGET [ANSWER] [--seed N] [--max-codelets K] [--keep-going] [--trace FILE] [--verbose]\n")
  (exit 2))

(define (parse-positive s)
  (let ([n (string->number s 10)])
    (if (and n (exact? n) (integer? n) (> n 0)) n (usage))))

;; (list strings seed max-codelets keep-going? trace-file verbose?), as the oracle
;; parses them: words starting with a letter are the strings
(define (parse-args args)
  (define verbose? #f)
  (let loop ([args args] [strings '()] [seed #f] [max #f] [keep? #f] [trace #f])
    (cond
      [(null? args)
       (if (memv (length strings) '(3 4))
           (list (reverse strings) seed max keep? trace verbose?)
           (usage))]
      [(string=? (car args) "--seed")
       (if (null? (cdr args)) (usage)
           (loop (cddr args) strings (parse-positive (cadr args)) max keep? trace))]
      [(string=? (car args) "--max-codelets")
       (if (null? (cdr args)) (usage)
           (loop (cddr args) strings seed (parse-positive (cadr args)) keep? trace))]
      [(string=? (car args) "--trace")
       (if (null? (cdr args)) (usage)
           (loop (cddr args) strings seed max keep? (cadr args)))]
      [(string=? (car args) "--keep-going")
       (loop (cdr args) strings seed max #t trace)]
      [(string=? (car args) "--verbose")
       (set! verbose? #t)
       (loop (cdr args) strings seed max keep? trace)]
      [(and (> (string-length (car args)) 0)
            (char-alphabetic? (string-ref (car args) 0)))
       (loop (cdr args) (cons (string->symbol (car args)) strings) seed max keep? trace)]
      [else (usage)])))

(define (main args)
  (define options (parse-args args))
  (define seed
    (or (cadr options)
        (begin (randomize) (random-seed))))
  (unless (valid-number? seed)
    (eprintf "cli.rkt: the seed must be between 1 and 4294967295\n")
    (exit 2))
  (define trace-file (list-ref options 4))
  (define trace-port
    (and trace-file (open-output-file trace-file #:exists 'truncate/replace)))
  (dynamic-wind
    void
    (lambda ()
      (run-problem (car options) seed (caddr options) (cadddr options) trace-port
                   #:verbose? (list-ref options 5)))
    (lambda ()
      (when trace-port (close-output-port trace-port))
      (flush-output (current-output-port)))))

(module+ main
  (void (main (vector->list (current-command-line-arguments)))))

#lang racket/base
;; Run times per problem: racket/cli.rkt against chez_scheme/oracle/run.ss.
;;
;;   racket tests/bench-runs.rkt [OUTPUT.md]
;;
;; Runs every run of tests/problems.txt once with each program, one process
;; at a time (so run it on an otherwise idle machine), and checks that both
;; print the same output.  Times are wall-clock seconds per process, startup
;; included; the startup of each (loading Metacat and setting up a problem:
;; a run with --max-codelets 1) is measured separately, the median of 5.
;; Writes a Markdown table per problem (seeds summed) to OUTPUT.md, or to
;; stdout.  Not part of the test suite (it takes several minutes).
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../racket/headless.rkt")

(define-runtime-path cli "../racket/cli.rkt")
(define-runtime-path oracle "../chez_scheme/oracle/run.ss")
(define-runtime-path problems "problems.txt")

(define racket-exe
  (let ([r (find-system-path 'exec-file)])
    (if (absolute-path? r) r (find-executable-path r))))
(define scheme (or (find-executable-path "scheme") (find-executable-path "chezscheme")))

;; (values seconds stdout)
(define (timed exe . args)
  (define out (open-output-string))
  (define start (current-inexact-milliseconds))
  (parameterize ([current-output-port out] [current-error-port (open-output-nowhere)])
    (apply system* exe args))
  (values (/ (- (current-inexact-milliseconds) start) 1000.0) (get-output-string out)))

(define (args-of run)
  (append (map symbol->string (list-ref run 1))
          (list "--seed" (number->string (list-ref run 2)))
          (if (list-ref run 3) (list "--max-codelets" (number->string (list-ref run 3))) '())
          (if (list-ref run 4) (list "--keep-going") '())))

(define (median xs) (list-ref (sort xs <) (quotient (length xs) 2)))

(define (fmt x) (real->decimal-string x 2))

(module+ main
  (define out-file
    (let ([args (current-command-line-arguments)]) (and (> (vector-length args) 0) (vector-ref args 0))))
  (define (startup exe . args)
    (median (for/list ([i 5]) (let-values ([(s o) (apply timed exe args)]) s))))
  (define racket-startup
    (startup racket-exe (path->string cli) "abc" "abd" "xyz" "--seed" "1" "--max-codelets" "1"))
  (define chez-startup
    (startup scheme "--script" (path->string oracle) "abc" "abd" "xyz" "--seed" "1"
             "--max-codelets" "1"))
  (define runs (golden-runs problems))
  ;; problem -> list of (codelets chez-seconds racket-seconds)
  (define rows '())
  (define mismatches 0)
  (for ([run runs])
    (define args (args-of run))
    (define-values (cs co) (apply timed scheme "--script" (path->string oracle) args))
    (define-values (rs ro) (apply timed racket-exe (path->string cli) args))
    (unless (equal? co ro) (set! mismatches (add1 mismatches)))
    (define codelets (string->number (cadr (regexp-match #rx"\nCodelets: ([0-9]+)\n" co))))
    (define problem (string-join (map symbol->string (list-ref run 1)) " "))
    (set! rows (cons (list problem codelets cs rs) rows))
    (eprintf "~a seed ~a: ~a codelets, Chez ~as, Racket ~as\n"
             problem (list-ref run 2) codelets (fmt cs) (fmt rs)))
  (define problems-in-order (remove-duplicates (map car (reverse rows))))
  (define (sum xs) (apply + xs))
  (define (report)
    (printf "Startup (load, set up a problem, run 1 codelet), median of 5: Chez ~a s, Racket ~a s.\n\n"
            (fmt chez-startup) (fmt racket-startup))
    (printf "| Problem | Runs | Codelets | Chez s | Racket s | Racket / Chez | Chez ms/1000 codelets | Racket ms/1000 codelets |\n")
    (printf "| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |\n")
    (define (line name rs)
      (define n (length rs))
      (define codelets (sum (map cadr rs)))
      (define c (sum (map caddr rs)))
      (define r (sum (map cadddr rs)))
      (define (per-k t startup) (fmt (* 1000 (/ (max 0 (- t (* n startup))) (max 1 (/ codelets 1000))))))
      (printf "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |\n" name n codelets (fmt c) (fmt r)
              (fmt (/ r c)) (per-k c chez-startup) (per-k r racket-startup)))
    (for ([p problems-in-order])
      (line (format "`~a`" p) (filter (lambda (r) (equal? (car r) p)) rows)))
    (line "**all**" rows)
    (printf "\nOutputs identical: ~a of ~a runs.\n" (- (length rows) mismatches) (length rows)))
  (if out-file
      (with-output-to-file out-file report #:exists 'truncate/replace)
      (report)))

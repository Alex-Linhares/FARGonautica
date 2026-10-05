#lang racket/base
;; Item 11: racket/cli.rkt, run as a program, against chez_scheme/oracle/run.ss.
;; Same arguments, same printed output, byte for byte: the Problem line, the
;; commentary, the answers, what the model prints (suspend, the cap's
;; "Codelets run:", report-error-and-halt's "Ooops:") and the summary.  Also
;; --trace against tests/golden/, a run without --seed (the seed it prints,
;; given to the oracle, gives the same run), the original's crash, and the
;; usage errors.  (golden-test.rkt compares the output of every golden run.)
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/file
         racket/port
         racket/runtime-path
         racket/string
         racket/system)

(define-runtime-path cli "../cli.rkt")
(define-runtime-path oracle "../../chez_scheme/oracle/run.ss")
(define-runtime-path golden-dir "../../tests/golden")

(define racket (find-system-path 'exec-file))
(define racket-exe
  (if (absolute-path? racket) racket (or (find-executable-path racket) racket)))
(define scheme (or (find-executable-path "scheme") (find-executable-path "chezscheme")))

;; (list exit-code stdout stderr)
(define (run-program exe . args)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out] [current-error-port err]
                   [current-input-port (open-input-string "")])
      (apply system*/exit-code exe args)))
  (list code (get-output-string out) (get-output-string err)))

(define (port-run . args) (apply run-program racket-exe (path->string cli) args))
(define (oracle-run . args) (apply run-program scheme "--script" (path->string oracle) args))

(define (same-as-oracle . args)
  (define p (apply port-run args))
  (define o (apply oracle-run args))
  (check-equal? (car p) (car o) (format "exit code for ~a" args))
  (check-equal? (cadr p) (cadr o) (format "output for ~a" args))
  (check-equal? (caddr p) "" (format "no error output for ~a" args))
  p)

(module+ test
  (check-true (file-exists? cli) "racket/cli.rkt exists")

  ;; an answer, then suspend
  (define r1 (same-as-oracle "abc" "abd" "xyz" "--seed" "3" "--max-codelets" "10000"))
  (check-true (regexp-match? #rx"\nAnswer: yyz  quality 88  codelet 2427" (cadr r1)))
  ;; no cap at all
  (void (same-as-oracle "abc" "abd" "xyz" "--seed" "3"))
  ;; the cap, before any answer
  (define r2 (same-as-oracle "abc" "abd" "ijk" "--seed" "2" "--max-codelets" "300"))
  (check-true (regexp-match? #rx"\nCodelets run: 300\nStopped: cap\n" (cadr r2)))
  ;; a justify run
  (void (same-as-oracle "abc" "abd" "mrrjjj" "mrrjjjj" "--seed" "1" "--max-codelets" "10000"))
  ;; keep going past answers
  (define r3 (same-as-oracle "a" "b" "z" "--seed" "1" "--max-codelets" "1000" "--keep-going"))
  (check-true (regexp-match? #rx"\nStopped: cap\n" (cadr r3)))
  ;; the original halts (report-error-and-halt)
  (define r4 (same-as-oracle "eqe" "qeq" "abbba" "aaabaaa" "--seed" "3" "--max-codelets" "17000"))
  (check-true (regexp-match? #rx"\nOoops: bad message .*\nStopped: halt\n" (cadr r4)))

  ;; --verbose (item 17): the original's verbose mode prints the model's
  ;; vprintf output.  These runs reach jootsing.ss's (reveal entry), which
  ;; needs format-slipnode as a top-level value; the trace is unchanged.
  (define v1 (same-as-oracle "a" "b" "z" "--seed" "1" "--max-codelets" "1000" "--keep-going"
                             "--verbose"))
  (check-true (> (length (string-split (cadr v1) "\n")) 5000) "verbose output is long")
  (check-true (regexp-match? #rx"\n[(]<[a-z]+> <[a-z]+>[)] entry: overlap = " (cadr v1))
              "jootsing's reveal names slipnodes")
  (define v-trace (make-temporary-file "metacat-cli-~a.jsonl"))
  (define v2 (same-as-oracle "abc" "abd" "xyz" "--seed" "3009318743" "--max-codelets" "10000"
                             "--verbose" "--trace" (path->string v-trace)))
  (check-true (regexp-match? #rx"entry: overlap = " (cadr v2)))
  (check-equal? (file->string v-trace)
                (file->string (build-path golden-dir "abc-abd-xyz_3009318743.jsonl"))
                "--verbose does not change the trace")
  (delete-file v-trace)

  ;; --trace writes the golden trace
  (define trace-file (make-temporary-file "metacat-cli-~a.jsonl"))
  (define r5 (port-run "abc" "abd" "xyz" "--seed" "3852097033" "--max-codelets" "10000"
                       "--trace" (path->string trace-file)))
  (check-equal? (car r5) 0)
  (check-equal? (file->string trace-file)
                (file->string (build-path golden-dir "abc-abd-xyz_3852097033.jsonl"))
                "--trace gives the golden trace")
  (check-equal? (cadr r5)
                (cadr (oracle-run "abc" "abd" "xyz" "--seed" "3852097033"
                                  "--max-codelets" "10000"))
                "--trace does not change the output")
  (delete-file trace-file)

  ;; without --seed, the seed comes from the clock and is printed
  (define r6 (port-run "abc" "abd" "xyz" "--max-codelets" "500"))
  (check-equal? (car r6) 0)
  (define seed (cadr (or (regexp-match #rx"^Problem: abc -> abd; xyz -> [?]  seed ([0-9]+)\n" (cadr r6))
                         '(#f "0"))))
  (check-true (> (string->number seed) 0) "a clock seed is printed")
  (check-equal? (cadr r6) (cadr (oracle-run "abc" "abd" "xyz" "--seed" seed "--max-codelets" "500"))
                "the printed seed replays the run in the oracle")

  ;; the original crashes on abc ccbbaa ijk, seed 3 (anomalies_and_quirks.md):
  ;; both fail, with the same output up to the crash
  (define p7 (port-run "abc" "ccbbaa" "ijk" "--seed" "3" "--max-codelets" "10000"))
  (define o7 (oracle-run "abc" "ccbbaa" "ijk" "--seed" "3" "--max-codelets" "10000"))
  (check-not-equal? (car o7) 0)
  (check-not-equal? (car p7) 0)
  (check-equal? (cadr p7) (cadr o7) "same output up to the crash")
  (check-true (regexp-match? #rx"caddr" (caddr p7)) "the port reports the caddr error")

  ;; usage errors: exit code 2, nothing on stdout, a message on stderr
  (for ([args '(() ("abc" "abd") ("abc" "abd" "xyz" "--seed") ("abc" "abd" "xyz" "--seed" "0")
                ("abc" "abd" "xyz" "--seed" "x") ("abc" "abd" "xyz" "--max-codelets" "-3")
                ("abc" "abd" "xyz" "--bogus") ("abc" "abd" "xyz" "--seed" "4294967296")
                ("a" "b" "c" "d" "e"))])
    (define p (apply port-run args))
    (define o (apply oracle-run args))
    (check-equal? (car o) 2 (format "the oracle rejects ~s" args))
    (check-equal? (car p) 2 (format "the CLI rejects ~s" args))
    (check-equal? (cadr p) "" (format "no output for ~s" args))
    (check-true (> (string-length (caddr p)) 0) (format "a message for ~s" args))))

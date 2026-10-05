#lang racket/base
;; Items 10-11: the port against the golden traces and the oracle's output.
;; Since item 11 the runs use the engine's own run.ss (init-mcat, run-mcat),
;; driven by racket/headless.rkt, and each run's printed output (what
;; racket/cli.rkt prints) must also equal what chez_scheme/oracle/run.ss
;; prints for the same run, run live here: commentary, answers, the model's
;; own messages and the summary.
;;
;; Item 10: the port against the golden traces.  With themes.ss, justify.ss,
;; trace.ss, jootsing.ss and memory.ss ported, every run of
;; tests/problems.txt is run by the engine (golden-harness.rkt: the oracle's
;; headless windows and trace instrumentation around a copy of run.ss) and
;; its JSON-lines trace must equal tests/golden/<run>.jsonl byte for byte:
;; codelets, structures built and broken, temperature, slipnet activations,
;; the Themespace's themes, the Temporal Trace's events (answer, snag, clamp,
;; rule, group, concept-mapping, concept-activation), answers, commentary and
;; the end of the run.  The first differing line of each run is reported.
;;
;; As the oracle runs each golden in a fresh Chez process, each run here
;; gets a fresh instance of the engine (a new namespace): the Memory, for
;; one, keeps its answers from one run to the next.  Runs are spread over
;; places.
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require rackunit
         racket/file
         racket/list
         (only-in racket/future processor-count)
         "golden-pool.rkt"
         racket/runtime-path
         racket/port
         racket/string
         racket/system)

(define-runtime-path harness "golden-harness.rkt")
(define-runtime-path problems "../../tests/problems.txt")
(define-runtime-path golden-dir "../../tests/golden")

;; chez_scheme/oracle/run.ss's arguments for a run of golden-runs
(define (oracle-args run)
  (append (map symbol->string (list-ref run 1))
          (list "--seed" (number->string (list-ref run 2)))
          (if (list-ref run 3) (list "--max-codelets" (number->string (list-ref run 3))) '())
          (if (list-ref run 4) (list "--keep-going") '())))

;; the oracle's stdout for every run, n processes at a time:
;; hash file-name -> (cons stdout exit-code)
(define (oracle-outputs scheme oracle-run runs n)
  (define results (make-hash))
  (let loop ([queue runs] [running '()])
    (cond
      [(and (null? queue) (null? running)) results]
      [(and (pair? queue) (< (length running) n))
       (define run (car queue))
       (define-values (proc stdout stdin stderr)
         (apply subprocess #f #f 'stdout scheme "--script" (path->string oracle-run)
                (oracle-args run)))
       (close-output-port stdin)
       ;; read the output as it comes, so that a full pipe never blocks Chez
       (define text (open-output-string))
       (define reader (thread (lambda () (copy-port stdout text) (close-input-port stdout))))
       (loop (cdr queue) (cons (list run proc reader text) running))]
      [else
       (define done
         (apply sync (for/list ([r running]) (wrap-evt (car (cdr r)) (lambda (_) r)))))
       (thread-wait (caddr done))
       (hash-set! results (car (car done))
                  (cons (get-output-string (cadddr done)) (subprocess-status (cadr done))))
       (loop queue (remq done running))])))

(module+ test
  (define runs (load-golden-runs harness problems))
  (check-equal? (sort (map car runs) string<?)
                (sort (for/list ([f (directory-list golden-dir)]
                                 #:when (regexp-match? #rx"[.]jsonl$" (path->string f)))
                        (path->string f))
                      string<?)
                "problems.txt lists exactly the runs of tests/golden/")
  ;; the oracle, live, in a background thread while the port runs
  (define-runtime-path oracle-run-script "../../chez_scheme/oracle/run.ss")
  (define chez (or (find-executable-path "scheme") (find-executable-path "chezscheme")))
  (define oracle-results #f)
  (define oracle-thread
    (thread (lambda ()
              (set! oracle-results
                    (oracle-outputs chez oracle-run-script runs
                                    (max 1 (quotient (processor-count) 2)))))))
  (define results (run-golden-runs harness 'golden-run runs golden-dir))
  (thread-wait oracle-thread)
  (define output-lines 0)
  (define themes-lines 0)
  (define event-lines 0)
  (for ([result (sort results string<? #:key car)])
    (define name (car result))
    (define want (file->string (build-path golden-dir name)))
    (cond
      [(not (caddr result)) (fail (format "~a: the port raised: ~a" name (cadr result)))]
      [else
       (define d (first-difference (cadr result) want))
       (if d
           (fail (format "~a: traces differ at line ~a\n  golden: ~a\n  port:   ~a"
                         name (car d) (short (caddr d)) (short (cadr d))))
           (check-true #t))
       ;; the printed output against the oracle's
       (define oracle (hash-ref oracle-results name))
       (check-equal? (cdr oracle) 0 (format "~a: the oracle exits 0" name))
       (define o (first-difference (list-ref result 3) (car oracle)))
       (if o
           (fail (format "~a: output differs from the oracle's at line ~a\n  oracle: ~a\n  port:   ~a"
                         name (car o) (short (caddr o)) (short (cadr o))))
           (check-true #t))
       (set! output-lines (+ output-lines (length (string-split (car oracle) "\n"))))
       (set! themes-lines (+ themes-lines (length (regexp-match* #rx"\"ev\":\"themes\"" want))))
       (set! event-lines (+ event-lines (length (regexp-match* #rx"\"ev\":\"event\"" want))))]))
  ;; what the outputs cover: commentary, answers, the summary, cap and halt
  (define all-output (apply string-append (map (lambda (r) (list-ref r 3)) results)))
  (for ([rx (list #rx"\nComment: " #rx"\nAnswer: " #rx"\nType \\(go\\) or click"
                  #rx"\nCodelets run: [0-9]+\n" #rx"\nStopped: cap\n" #rx"\nStopped: halt\n"
                  #rx"\nOoops: bad message" #rx"\nAnswers: none\n")])
    (check-true (regexp-match? rx all-output) (format "the outputs have ~a" (object-name rx))))
  ;; what the goldens cover of the self-watching half
  (check-true (> themes-lines 10000) "the goldens have themes events")
  (check-true (> event-lines 1000) "the goldens have Temporal Trace events")
  (define all (apply string-append (map (lambda (r) (file->string (build-path golden-dir (car r))))
                                        runs)))
  (for ([type '("answer" "snag" "clamp" "rule" "group" "concept-mapping" "concept-activation")])
    (check-true (regexp-match? (regexp (format "\"ev\":\"event\",\"type\":\"~a\"" type)) all)
                (format "a ~a event" type)))
  (for ([type '("thematic-bridge-scout" "answer-justifier" "progress-watcher" "jootser")])
    (check-true (regexp-match? (regexp (format "\"ev\":\"codelet\",\"type\":\"~a\"" type)) all)
                (format "a ~a codelet" type))))

;; The original crashes on abc ccbbaa ijk, seed 3 (a Chez error, caddr of #f,
;; in transcribe-to-english: docs/anomalies_and_quirks.md), so the golden
;; set leaves it out.  The port must crash at the same point: the oracle is
;; run here, and its trace up to the error must equal the port's.
(module+ test
  (define-runtime-path oracle-run "../../chez_scheme/oracle/run.ss")
  (define scheme (or (find-executable-path "scheme") (find-executable-path "chezscheme")))
  (define chez-file (make-temporary-file "metacat-crash-~a.jsonl"))
  (define chez-ok?
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)])
      (system* scheme "--script" oracle-run "abc" "ccbbaa" "ijk" "--seed" "3"
               "--max-codelets" "10000" "--trace" (path->string chez-file))))
  (check-false chez-ok? "the original crashes on abc ccbbaa ijk, seed 3")
  (define-values (raised partial)
    (parameterize ([current-namespace (make-base-namespace)])
      (define golden-trace (dynamic-require harness 'golden-trace))
      (define golden-partial-trace (dynamic-require harness 'golden-partial-trace))
      (with-handlers ([exn:fail? (lambda (e) (values (exn-message e) (golden-partial-trace)))])
        (golden-trace '(abc ccbbaa ijk) 3 10000 #f)
        (values #f #f))))
  (check-true (and raised (regexp-match? #rx"^caddr: " raised) #t)
              (format "the port crashes in caddr too: ~a" raised))
  (define chez-trace (file->string chez-file))
  (delete-file chez-file)
  (check-true (> (length (string-split chez-trace "\n")) 1000))
  (check-equal? (and partial (first-difference partial chez-trace)) #f
                "the traces up to the crash are the same"))

#lang racket/base
;; Running golden runs (tests/problems.txt) in parallel, each in a fresh
;; engine: shared by golden-test.rkt (items 10-11) and
;; views-test.rkt (items 13-14).
;;
;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
(require compiler/cm
         racket/file
         racket/place
         (only-in racket/future processor-count)
         racket/string)

(provide first-difference short run-golden-runs load-golden-runs)

;; the first line where two traces differ, or #f
(define (first-difference got want)
  (let loop ([g (string-split got "\n")] [w (string-split want "\n")] [n 1])
    (cond
      [(and (null? g) (null? w)) #f]
      [(null? g) (list n "<end of trace>" (car w))]
      [(null? w) (list n (car g) "<end of trace>")]
      [(string=? (car g) (car w)) (loop (cdr g) (cdr w) (add1 n))]
      [else (list n (car g) (car w))])))

(define (short s) (if (> (string-length s) 400) (string-append (substring s 0 400) " ...") s))

;; one run, in a fresh engine, with the procedure `name' of module `harness'
;; (called with the run's strings seed cap keep-going?, returning
;; (values trace stdout)):
;; (list file-name trace-or-error-message ok? stdout seconds)
(define (run-one harness name run)
  (with-handlers ([(lambda (e) #t)
                   (lambda (e) (list (car run) (if (exn? e) (exn-message e) (format "~s" e)) #f
                                     "" 0))])
    (define golden-run
      (parameterize ([current-namespace (make-base-namespace)])
        (dynamic-require (string->path harness) name)))
    (define start (current-inexact-milliseconds))
    (define-values (trace stdout) (apply golden-run (cdr run)))
    (list (car run) trace #t stdout (/ (- (current-inexact-milliseconds) start) 1000.0))))

(define (start-worker)
  (place ch
    (let loop ()
      (define msg (place-channel-get ch))
      (when msg
        (place-channel-put ch (run-one (car msg) (cadr msg) (cddr msg)))
        (loop)))))

;; golden-runs of the harness, loaded through the compilation manager, which
;; recompiles the engine when an included engine/*.rktl changed (the default
;; load handler would load a stale .zo: docs/anomalies_and_quirks.md); the
;; places then load the fresh .zo files
(define (load-golden-runs harness problems)
  (define golden-runs
    (parameterize ([current-load/use-compiled (make-compilation-manager-load/use-compiled-handler)])
      (dynamic-require harness 'golden-runs)))
  (golden-runs problems))

;; every run of runs (golden-runs' lists), on up to 16 places, longest
;; (by golden size) first; results as run-one's, in no particular order
(define (run-golden-runs harness name runs golden-dir)
  (define n-workers (max 1 (min 16 (processor-count) (length runs))))
  (define workers (for/list ([i n-workers]) (start-worker)))
  (define queue
    (sort runs > #:key (lambda (r) (file-size (build-path golden-dir (car r))))
          #:cache-keys? #t))
  (define results
    (let loop ([queue queue] [idle workers] [busy 0] [acc '()])
      (cond
        [(and (null? queue) (zero? busy)) acc]
        [(and (pair? queue) (pair? idle))
         (place-channel-put (car idle) (list* (path->string harness) name (car queue)))
         (loop (cdr queue) (cdr idle) (add1 busy) acc)]
        [else
         (define-values (w result)
           (apply sync (for/list ([w workers] #:unless (memq w idle))
                         (wrap-evt w (lambda (r) (values w r))))))
         (loop queue (cons w idle) (sub1 busy) (cons result acc))])))
  (for ([w workers]) (place-channel-put w #f) (place-wait w))
  results)

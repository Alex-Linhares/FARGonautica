;; Item 02: the JSON-lines trace (docs/trace-format.md).
;;
;; For a few problems and seeds, `run.ss ... --trace FILE` must
;;  - exit 0 and print exactly what the same run prints without --trace
;;    (tracing must not change the run: no random draws, no state changes);
;;  - write a trace that chez_scheme/oracle/validate-trace.py accepts (every
;;    line a JSON object of a known event type with the specified fields,
;;    start first, end last, one codelet event per codelet run, ...), and
;;    that contains the event types the run must produce;
;;  - write the same trace, byte for byte, when run again.

(define scheme-program
  (if (= 0 (system "command -v scheme > /dev/null 2>&1")) "scheme" "chezscheme"))

(define read-file
  (lambda (path)
    (if (file-exists? path)
        (call-with-input-file path
          (lambda (in)
            (let loop ((acc '()))
              (let ((c (read-char in)))
                (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc)))))))
        "")))

(define tmp (format "/tmp/metacat-trace-check-~a" (random 1000000000)))

(define remove-file
  (lambda (path) (when (file-exists? path) (delete-file path))))

;; Returns (exit-status stdout trace)
(define run
  (lambda (args trace?)
    (remove-file (string-append tmp ".out"))
    (remove-file (string-append tmp ".jsonl"))
    (let* ((status (system (format "~a --script chez_scheme/oracle/run.ss ~a~a > ~a.out 2>&1"
                                   scheme-program args
                                   (if trace? (format " --trace ~a.jsonl" tmp) "")
                                   tmp)))
           (out (read-file (string-append tmp ".out")))
           (trace (read-file (string-append tmp ".jsonl"))))
      (remove-file (string-append tmp ".out"))
      (list status out trace))))

(define validate
  (lambda (trace required)
    (let ((file (string-append tmp ".v.jsonl")))
      (call-with-output-file file (lambda (p) (display trace p)) 'replace)
      (let ((status (system (format "python3 chez_scheme/oracle/validate-trace.py ~a --require ~a"
                                    file required))))
        (remove-file file)
        (= status 0)))))

;; problem arguments, and the event types its trace must contain
(define cases
  '(("abc abd xyz --seed 3 --max-codelets 3000"
     "codelet,build,break,temperature,slipnet,themes,answer,comment,event")
    ("abc abd mrrjjj --seed 2 --max-codelets 3000"
     "codelet,build,temperature,slipnet,answer,comment,event")
    ("a b z --seed 3861033416 --max-codelets 1200 --keep-going"
     "codelet,build,temperature,slipnet,answer,comment,event")
    ("abc abd ijk abd --seed 3386544399 --max-codelets 1500"
     "codelet,build,temperature,slipnet,comment,event")))

(define failures 0)
(define fail!
  (lambda (fmt . args)
    (set! failures (+ failures 1))
    (apply printf (string-append "FAIL: " fmt "~%") args)))

(for-each
  (lambda (c)
    (let ((args (car c)) (required (cadr c)))
      (let ((plain (run args #f))
            (traced (run args #t))
            (again (run args #t)))
        (cond
          ((not (= 0 (car plain) (car traced) (car again)))
           (fail! "~a exited ~a/~a/~a:~%~a" args (car plain) (car traced) (car again)
                  (cadr traced)))
          ((not (string=? (cadr plain) (cadr traced)))
           (fail! "~a: tracing changed the run's output:~%-- without --trace:~%~a-- with:~%~a"
                  args (cadr plain) (cadr traced)))
          ((string=? (caddr traced) "")
           (fail! "~a: no trace written" args))
          ((not (validate (caddr traced) required))
           (fail! "~a: invalid trace" args))
          ((not (string=? (caddr traced) (caddr again)))
           (fail! "~a: a second run gave a different trace" args))
          (else (printf "  ok ~a~%" args))))))
  cases)

(remove-file (string-append tmp ".jsonl"))
(if (> failures 0)
    (begin (printf "trace-check: ~a failures~%" failures) (exit 1))
    (printf "trace-check: ~a traced runs valid, reproducible and unchanged by tracing~%"
            (length cases)))

;; Item 01: the original runs headless under Chez 10.  For each problem and
;; seed, chez_scheme/oracle/run.ss must exit 0, load and run the unmodified
;; source, and reach an answer within the codelet cap; and running the same
;; problem and seed again must give byte-identical output.

(define problems '("abc abd xyz" "abc abd ijk" "eqe qeq abbbc" "abc abd mrrjjj"))
(define seeds '(1 2 3))
(define max-codelets 20000)

(define scheme-program
  (if (= 0 (system "command -v scheme > /dev/null 2>&1")) "scheme" "chezscheme"))

(define read-file
  (lambda (path)
    (call-with-input-file path
      (lambda (in)
        (let loop ((acc '()))
          (let ((c (read-char in)))
            (if (eof-object? c) (list->string (reverse acc)) (loop (cons c acc)))))))))

(define tmp (format "/tmp/metacat-headless-check-~a" (random 1000000000)))

;; Returns (exit-status . output)
(define run
  (lambda (problem seed)
    (let ((status (system (format "~a --script chez_scheme/oracle/run.ss ~a --seed ~a --max-codelets ~a > ~a 2>&1"
                                  scheme-program problem seed max-codelets tmp))))
      (let ((out (if (file-exists? tmp) (read-file tmp) "")))
        (when (file-exists? tmp) (delete-file tmp))
        (cons status out)))))

(define contains?
  (lambda (s pat)
    (let ((n (string-length s)) (m (string-length pat)))
      (let loop ((i 0))
        (cond ((> (+ i m) n) #f)
              ((string=? (substring s i (+ i m)) pat) #t)
              (else (loop (+ i 1))))))))

(define failures 0)
(for-each
  (lambda (problem)
    (for-each
      (lambda (seed)
        (let ((r1 (run problem seed)) (r2 (run problem seed)))
          (cond
            ((not (= (car r1) 0))
             (set! failures (+ failures 1))
             (printf "FAIL: ~a seed ~a exited ~a:~%~a~%" problem seed (car r1) (cdr r1)))
            ((not (contains? (cdr r1) "\nAnswer: "))
             (set! failures (+ failures 1))
             (printf "FAIL: ~a seed ~a found no answer in ~a codelets:~%~a~%"
                     problem seed max-codelets (cdr r1)))
            ((not (and (= (car r2) 0) (string=? (cdr r1) (cdr r2))))
             (set! failures (+ failures 1))
             (printf "FAIL: ~a seed ~a: a second run gave different output~%" problem seed))
            (else
             (let* ((s (cdr r1))
                    (i (let loop ((i 0)) (if (string=? (substring s i (+ i 9)) "\nAnswer: ") (+ i 1) (loop (+ i 1)))))
                    (j (let loop ((j i)) (if (char=? (string-ref s j) #\newline) j (loop (+ j 1))))))
               (printf "  ok ~a seed ~a: ~a~%" problem seed (substring s i j)))))))
      seeds))
  problems)

(if (> failures 0)
    (begin (printf "headless-run-check: ~a failures~%" failures) (exit 1))
    (printf "headless-run-check: ~a problems x ~a seeds reached an answer, reproducibly~%"
            (length problems) (length seeds)))

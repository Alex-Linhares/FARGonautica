;;; count-calls.ss -- how often a run calls tell and delegate.
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;; Item 01 (docs/python-translation-plan.md, "Objects") uses it to project the cost
;;; of message dispatch in Python.  Runs chez_scheme/oracle/run.ss (unedited) form by
;;; form, wrapping the counted procedures once the original is loaded:
;;;
;;;   scheme --script python/oracle/count-calls.ss abc abd xyz --seed 3852097033
;;;
;;; prints run.ss's output, then one line "COUNTS codelets N tell N delegate N"
;;; (delegate counts the calls of utilities.ss's delegate, i.e. messages that fall
;;; through an object's own record-case).  Chez's own random can't be wrapped: it is
;;; an immutable built-in.

(define $oracle-directory "chez_scheme/oracle")
(define $counts (make-vector 2 0))
(define $count! (lambda (i) (vector-set! $counts i (+ 1 (vector-ref $counts i)))))

(define $install-counters!
  (lambda ()
    (let ((tell0 tell) (delegate0 delegate))
      (set! tell (lambda args ($count! 0) (apply tell0 args)))
      (set! delegate (lambda args ($count! 1) (apply delegate0 args))))))

(define $report
  (lambda ()
    (printf "COUNTS codelets ~a tell ~a delegate ~a~%"
            *codelet-count* (vector-ref $counts 0) (vector-ref $counts 1))))

;; run.ss's forms, with its own $oracle-directory replaced, the counters installed
;; right after (load-metacat), and the report before its final (exit 0)
(let ((p (open-input-file (string-append $oracle-directory "/run.ss"))))
  (let loop ()
    (let ((form (read p)))
      (cond
        ((eof-object? form) (void))
        ((and (pair? form) (eq? (car form) 'define) (eq? (cadr form) '$oracle-directory))
         (loop))
        ((equal? form '(load-metacat)) (eval form) ($install-counters!) (loop))
        ((equal? form '(exit 0)) ($report) (eval form))
        (else (eval form) (loop))))))

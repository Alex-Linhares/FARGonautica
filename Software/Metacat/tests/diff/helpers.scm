;;; helpers.scm -- helpers shared by the differential batteries in tests/diff/.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Both runners (chez_scheme/oracle/diff-eval.ss and racket/tests/diff-runner.rkt)
;;; evaluate this file before a battery.  Besides these, each runner defines
;;;   (b:capture thunk)        -> the string printed by thunk
;;;   (b:set-global! name val) -> set the engine's global variable NAME
;;;                               (Chez: set-top-level-value!)

(define b:num
  (lambda (x)
    (cond
      ((not (real? x)) (string-append "C" (b:num (real-part x)) "," (b:num (imag-part x))))
      ((exact? x) (number->string x))
      ((not (= x x)) "F+nan")
      ((and (not (= x 0)) (= x (* 2 x))) (if (> x 0) "F+inf" "F-inf"))
      ((eqv? x -0.0) "F-0")
      (else (string-append "F" (number->string (inexact->exact x)))))))

(define b:canon
  (lambda (x)
    (cond
      ((null? x) "()")
      ((eq? x #t) "#t")
      ((eq? x #f) "#f")
      ((symbol? x) (string-append "'" (symbol->string x)))
      ((string? x) (string-append "\"" x "\""))
      ((char? x) (string-append "#\\" (number->string (char->integer x))))
      ((number? x) (b:num x))
      ((pair? x) (string-append "(" (b:canon (car x)) " . " (b:canon (cdr x)) ")"))
      ((vector? x) (string-append "#" (b:canon (vector->list x))))
      ((procedure? x) "#<procedure>")
      ((eq? x (void)) "#<void>")
      (else "#<other>"))))

(define b:log '())
(define log! (lambda (x) (set! b:log (cons x b:log)) x))
(define with-log
  (lambda (thunk)
    (set! b:log '())
    (let* ((r (thunk)) (l (reverse b:log)))
      (list r l))))

(define b:iota
  (lambda (n)
    (let loop ((i n) (acc '()))
      (if (= i 0) acc (loop (- i 1) (cons i acc))))))

(define b:copy (lambda (l) (if (null? l) '() (cons (car l) (b:copy (cdr l))))))

(define b:repeat
  (lambda (n thunk)
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (reverse acc)
          (let* ((v (thunk))) (loop (+ i 1) (cons v acc)))))))

;; run thunk after seeding; return its value and the generator state after
(define b:seeded
  (lambda (seed thunk)
    (random-seed seed)
    (let* ((r (thunk)) (s (random-seed)))
      (list r s))))

(define b:seeds (list 1 2 3 7 42 1000 65535 65536 123456789 2147483647
                      2147483648 4294967295 3141592653 2718281828 99))

;; Item 01: the randomness plan.  The oracle uses Chez 10's own random and
;; random-seed; docs/trace-format.md specifies them so the port can
;; reproduce them exactly.  This check implements that specification in
;; portable exact arithmetic and compares it with Chez's built-ins, value for
;; value and bit for bit, including the seed after every draw.

(define M32 4294967296)
(define step (lambda (s) (modulo (+ (* s 72931) 90763387) M32)))

;; spec-random: state -> (values result new-state)
(define spec-random-int
  (lambda (s n)                         ; 0 < n <= most-positive-fixnum
    (let* ((s1 (step s)) (s2 (step s1))
           (t (+ (quotient s1 65536) (* 65536 (quotient s2 65536)))))
      (if (<= n #xFFFFFFFF)
          (values (modulo t n) s2)
          (let* ((s3 (step s2)) (s4 (step s3))
                 (t (+ (* t 65536) (quotient s3 65536)))
                 (t (modulo t (expt 2 64)))      ; uptr arithmetic
                 (t (+ (* t 65536) (quotient s4 65536)))
                 (t (modulo t (expt 2 64))))
            (values (modulo t n) s4))))))

(define spec-random-float
  (lambda (s scale)                     ; scale a positive flonum
    (let* ((s1 (step s)) (s2 (step s1)) (s3 (step s2)) (s4 (step s3))
           (hi (lambda (x) (quotient x 65536)))
           (m (+ (* (modulo (hi s1) 16) (expt 2 48))
                 (* (hi s2) (expt 2 32))
                 (* (hi s3) (expt 2 16))
                 (hi s4))))
      ;; (1.m - 1.0) * scale, computed as in C: the subtraction is exact
      (values (fl* (inexact (/ m (expt 2 52))) scale) s4))))

(define failures 0)
(define fail
  (lambda (fmt . args)
    (set! failures (+ failures 1))
    (when (<= failures 10) (apply printf fmt args))))

(define check-draw
  (lambda (seed arg draws)
    (random-seed seed)
    (let loop ((i 0) (s seed))
      (when (< i draws)
        (let ((actual (random arg)))
          (let-values (((expected s2)
                        (if (flonum? arg) (spec-random-float s arg) (spec-random-int s arg))))
            (unless (and (eqv? actual expected) (= (random-seed) s2))
              (fail "FAIL: seed ~a arg ~s draw ~a: Chez ~s state ~a, spec ~s state ~a~%"
                    seed arg i actual (random-seed) expected s2))
            (loop (+ i 1) s2)))))))

(define seeds '(1 2 3 7 42 1000 48026 123456789 2147483647 2147483648 4294967295))
(define args (list 1 2 3 7 10 100 1000 65536 65537 4294967295 4294967296
                   (expt 2 40) (most-positive-fixnum)
                   1.0 2.0 0.5 100.0 3.7 1e-300))
(for-each (lambda (seed) (for-each (lambda (arg) (check-draw seed arg 200)) args)) seeds)

;; Interleaved draws, as the model makes them
(let ()
  (random-seed 7)
  (let loop ((i 0) (s 7))
    (when (< i 2000)
      (let ((arg (if (even? i) 1.0 (+ 1 (modulo i 13)))))
        (let ((actual (random arg)))
          (let-values (((expected s2)
                        (if (flonum? arg) (spec-random-float s arg) (spec-random-int s arg))))
            (unless (eqv? actual expected)
              (fail "FAIL: interleaved draw ~a: Chez ~s spec ~s~%" i actual expected))
            (loop (+ i 1) s2)))))))

;; random-seed accepts exactly 1 .. 2^32-1
(for-each
  (lambda (bad)
    (unless (guard (e (#t #t)) (random-seed bad) #f)
      (fail "FAIL: (random-seed ~s) was accepted~%" bad)))
  '(0 4294967296 -1))

(if (> failures 0)
    (begin (printf "rng-check: ~a failures~%" failures) (exit 1))
    (printf "rng-check: Chez 10 random/random-seed match the specification in docs/trace-format.md~%"))

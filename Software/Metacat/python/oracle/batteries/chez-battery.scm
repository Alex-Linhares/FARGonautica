;;; chez-battery.scm -- Chez Scheme 10 semantics that python/metacat/chez.py
;;; reproduces (loop0002 item 02), beyond what tests/diff/utilities-battery.scm
;;; already pins.
;;;
;;; Part of the Python translation of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Captured like tests/diff/*-battery.scm by python/oracle/capture.py:
;;; chez_scheme/oracle/diff-eval.ss evaluates tests/diff/helpers.scm, then this
;;; file, with the original loaded.  So round, floor, ceiling and truncate are
;;; utilities.ss's exact versions here; Chez's own are #%round etc.  Every
;;; (test NAME EXPR) prints "NAME => (b:canon value)" or "NAME => ERROR".
;;; The Python side is python/tests/test_chez.py.

;; errors as a value, so that one test can hold many cases
(define c:try
  (lambda (thunk)
    (call/cc
      (lambda (k)
        (with-exception-handler (lambda (c) (k 'error)) thunk)))))

;; left to right, explicitly (Chez's map has its own order)
(define c:map
  (lambda (f l)
    (let loop ((l l) (acc '()))
      (if (null? l) (reverse acc) (let* ((v (f (car l)))) (loop (cdr l) (cons v acc)))))))

(define c:iota0 (lambda (n) (c:map (lambda (i) (- i 1)) (b:iota n))))

;;;---------------------------------------------------------------------------
;;; The generator: the value of every draw and the state right after it

(define c:draws
  (lambda (seed args)
    (random-seed seed)
    (c:map (lambda (x) (let* ((v (random x)) (s (random-seed))) (list v s))) args)))

(define c:rng-args
  (list 1 2 3 7 10 100 1000 65535 65536 65537 1000003 2147483647 2147483648
        4294967295 4294967296 4294967297 1099511627776 1152921504606846975
        1.0 0.5 2.0 100.0 3.7 1e-300 1e300 1.0 1.0))

(test rng-state-after-each-draw
  (c:map (lambda (seed) (c:draws seed (append c:rng-args c:rng-args))) b:seeds))

;; a long, Metacat-like sequence: mostly (random 1.0), some small ranges
(test rng-long-run
  (c:draws 3852097033
    (c:map (lambda (i) (cond ((= 0 (modulo i 3)) 1.0) ((= 1 (modulo i 7)) (+ 1 (modulo i 11))) (else 1.0)))
           (b:iota 1500))))

(test rng-seed-limits
  (c:map (lambda (s) (c:try (lambda () (random-seed s) (list (random-seed) (random 1000) (random-seed)))))
         (list 1 4294967295 4294967296 0 -1 1.0 1/2 'a)))

(test rng-bad-args
  (c:map (lambda (x) (c:try (lambda () (random-seed 5) (list (random x) (random-seed)))))
         (list 0 -1 1/2 3/1 0.0 -0.0 -2.5 'a "1" 4.0)))

;;;---------------------------------------------------------------------------
;;; Arithmetic: exactness and contagion (each case through a procedure value,
;;; and through the inlined primitive)

(define c:nums (list 0 1 2 -3 1/2 -2/3 0.0 -0.0 1.5 2.0 -0.25 +inf.0))

(define c:table
  (lambda (op)
    (c:map (lambda (a) (c:map (lambda (b) (c:try (lambda () (op a b)))) c:nums)) c:nums)))

(test arith-add (c:table +))
(test arith-sub (c:table -))
(test arith-mul (c:table *))
(test arith-div (c:table /))
(test arith-max (c:table max))
(test arith-min (c:table min))
(test arith-add-inline (c:table (lambda (a b) (+ a b))))
(test arith-sub-inline (c:table (lambda (a b) (- a b))))
(test arith-mul-inline (c:table (lambda (a b) (* a b))))
(test arith-div-inline (c:table (lambda (a b) (/ a b))))
(test arith-max-inline (c:table (lambda (a b) (max a b))))
(test arith-min-inline (c:table (lambda (a b) (min a b))))
(test arith-unary
  (c:map (lambda (a) (c:try (lambda () (list (- a) (abs a) (/ a) (+ a) (* a) (max a) (zero? a)))))
         c:nums))
(test arith-nary
  (list (+) (*) (+ 1 1/2 0.5) (* 2 1/2 3) (* 0 1.5 2) (* 1.5 0 2) (* 1.5 2 0) (- 10 1/2 0.5)
        (/ 1 2 3) (/ 12 2 3) (max 1 2 3) (max 1 2.0 3) (min 3 2 1.0) (max 1/2 1/3 1/4)
        (min 1/2 -1/3) (max 0 -0.0) (min 0 -0.0) (max -0.0 0) (min -0.0 0)))
(test integer-division
  (c:map (lambda (a)
           (c:map (lambda (b) (list (quotient a b) (remainder a b) (modulo a b)))
                  (list 1 3 -3 7 -7)))
         (list 0 1 2 -2 7 -7 10 -10 100)))
(test exactness-predicates
  (c:map (lambda (a) (list (exact? a) (inexact? a) (integer? a) (rational? a) (zero? a)
                           (positive? a) (negative? a)))
         (list 0 1 -1 1/2 0.0 -0.0 1.0 1.5 +inf.0)))
(test one-plus
  (c:map (lambda (a) (list (1+ a) (-1+ a) (add1 a) (sub1 a)))
         (list 0 5 -1 1/2 -1/2 3/2 0.0 -0.0 1.5 -1.0 1e16 9007199254740993)))

;;;---------------------------------------------------------------------------
;;; Rounding: Chez's own (#%round etc. keep flonums inexact) and utilities.ss's

(define c:round-args
  (list 2.5 -2.5 3.5 0.5 -0.5 1.5 -1.5 0.49999999999999994 4503599627370495.5
        4503599627370497.0 7/2 5/2 -7/2 -5/2 1/2 -1/2 3.7 -3.7 0.0 -0.0 1e20 1e300
        4 -4 0 1/3 -1/3 2/3 +inf.0 -inf.0))

(test chez-rounding
  (c:map (lambda (x) (c:try (lambda () (list (#%truncate x) (#%ceiling x) (#%floor x) (#%round x)))))
         c:round-args))
(test exact-rounding
  (c:map (lambda (x) (c:try (lambda () (list (truncate x) (ceiling x) (floor x) (round x)))))
         c:round-args))
(test exact-inexact
  (c:map (lambda (x) (c:try (lambda () (list (exact->inexact x) (inexact->exact x)))))
         (list 0 1 -7 1/3 2/3 -22/7 1/10 123456789/1000 0.0 -0.0 0.1 1.5 1e300 5e-324 +inf.0)))

;; exact->inexact of ratnums, near and far from flonum boundaries
(test exact->inexact-ratnums
  (b:seeded 31
    (lambda ()
      (b:repeat 400
        (lambda ()
          (let* ((n (random 1000000007))
                 (d (+ 1 (random 99991)))
                 (k (random 4))
                 (x (cond ((= k 0) (/ n d))
                          ((= k 1) (/ d (+ n 1)))
                          ((= k 2) (/ (+ (* n 4294967296) d) (+ (* d 65536) 1)))
                          (else (/ n 100)))))
            (list x (exact->inexact x))))))))

;;;---------------------------------------------------------------------------
;;; sqrt, exp, log, tanh, expt: exact in, exact out where Chez says so;
;;; otherwise the doubles, bit for bit

(define c:ratnums
  (append (list 0 1 4 9 16 100 10000 1/4 9/16 1/100 81/100 2 3 1/2 1/3 7/10 99/100)
          (c:map (lambda (k) (/ k 100)) (b:iota 200))
          (c:map (lambda (k) (/ (* k k) 49)) (b:iota 20))))

(define c:flonums
  (append (list 0.0 1.0 2.0 4.0 0.25 0.5 0.1 1e-300 1e300 123.456 5e-324)
          (car (b:seeded 32 (lambda () (b:repeat 150 (lambda () (* 100.0 (random 1.0)))))))))

(test sqrt-exact (c:map (lambda (x) (list x (sqrt x))) c:ratnums))
(test sqrt-flonum (c:map (lambda (x) (list x (sqrt x))) c:flonums))
(test exp-exact (c:map (lambda (x) (list x (exp x) (exp (- x)))) c:ratnums))
(test exp-flonum (c:map (lambda (x) (list x (exp x) (exp (- x)))) c:flonums))
(test log-exact (c:map (lambda (x) (list x (c:try (lambda () (log x))))) c:ratnums))
(test log-flonum (c:map (lambda (x) (list x (log x))) c:flonums))
(test tanh-exact (c:map (lambda (x) (list x (tanh x) (tanh (- x)) (tanh (* 1/40 x 100)))) c:ratnums))
(test tanh-flonum (c:map (lambda (x) (list x (tanh x) (tanh (- x)))) c:flonums))
;; workspace.ss's mapping strengths: (tanh (* 1/40 raw)) for raw strengths 0..600
(test tanh-strengths (c:map (lambda (raw) (list raw (tanh (* 1/40 raw)))) (c:iota0 601)))

(define c:expt-bases
  (list 0 1 2 3 10 100 1/2 7/10 99/100 4 1/4 0.0 1.0 0.5 0.6 0.95 1.1 1.5 2.0 3.7 50.0 -2 -0.5))
(define c:expt-powers
  (list 0 1 2 3 5 10 27 -1 -2 -3 1/2 1/3 1/27 5/3 1/8 0.0 1.0 0.5 0.95 0.98 2.0 -0.5 1/1000))

(test expt-table
  (c:map (lambda (b) (c:map (lambda (p) (c:try (lambda () (expt b p)))) c:expt-powers)) c:expt-bases))
;; exact roots: only a power of 1/2 (sqrt) stays exact
(test expt-extra
  (c:map (lambda (bp) (c:try (lambda () (expt (car bp) (cadr bp)))))
         (list (list 8 1/3) (list 8 2/3) (list 1/8 1/3) (list 4 3/2) (list 9 -1/2) (list 2 1/2)
               (list 0 1/2) (list 0 -1/2) (list 0.0 1/2) (list -8 1/3) (list 1.0 0) (list 0 0)
               (list 2.5 0) (list -0.0 -1) (list -0.0 -2) (list 1 -1/2) (list 1.5 1) (list 0 0.5)
               (list 16 1/4) (list 0.0 0.0) (list 1.1 3) (list 1.1 27) (list 0.95 -3) (list 1/9 1/2)
               (list 1/9 -1/2) (list 0.0 -0.5) (list -0.0 -0.5) (list -0.0 -3) (list -0.0 3)
               (list -2 3) (list -2 -3) (list -1/2 3) (list 10 -1) (list 1e300 2) (list 2 1024)
               (list 2.0 1024) (list 4 1/2) (list 4.0 1/2) (list -4.0 1/2) (list -2 1/2))))

;; the model's own shapes: (expt strength 0.95), (expt 0.6 (/ 1 (^3 n))),
;; (expt 0.5 (* (^3 len) ...)), (expt (% depth) 1), (expt bin 1.5) ...
(test expt-model
  (list (c:map (lambda (s) (expt s 0.95)) (c:iota0 101))
        (c:map (lambda (n) (expt 0.6 (/ 1 (* n n n)))) (b:iota 12))
        (c:map (lambda (n) (expt 0.5 (* n n n))) (b:iota 8))
        (c:map (lambda (n) (expt 0.5 (* n n n 1/10))) (b:iota 8))
        (c:map (lambda (d) (expt (/ d 100) 1)) (c:iota0 101))
        (c:map (lambda (b) (expt b 0.98)) (c:map (lambda (k) (/ k 7)) (c:iota0 50)))
        (c:map (lambda (v) (expt v 3/2)) (c:iota0 30))
        (c:map (lambda (v) (expt (/ v 10) 2.5)) (c:iota0 30))))

;;;---------------------------------------------------------------------------
;;; Printing

;; random doubles of every magnitude, built from random bits (no NaN, no infinity)
(define c:random-double
  (lambda ()
    (let ((bv (make-bytevector 8)))
      (bytevector-u16-native-set! bv 0 (random 65536))
      (bytevector-u16-native-set! bv 2 (random 65536))
      (bytevector-u16-native-set! bv 4 (random 65536))
      (bytevector-u16-native-set! bv 6 (+ (random 32752) (* 32768 (random 2))))
      (bytevector-ieee-double-native-ref bv 0))))

(test number->string-bits
  (b:seeded 41 (lambda () (b:repeat 3000 (lambda () (let* ((x (c:random-double))) (list (b:num x) (number->string x))))))))

(test number->string-decades
  (b:seeded 42
    (lambda ()
      (c:map (lambda (e)
               (b:repeat 6 (lambda () (let* ((x (* (random 10.0) (expt 10.0 e)))) (list (b:num x) (number->string x))))))
             (c:map (lambda (k) (- k 26)) (b:iota 50))))))

;; 53-bit mantissas times 2^s: many doubles exactly halfway between two
;; shortest digit strings, where Chez's printer rounds up
(test number->string-ties
  (b:seeded 43
    (lambda ()
      (c:map (lambda (s)
               (b:repeat 40
                 (lambda ()
                   (let* ((hi (random 2097152)) (lo (random 4294967296))
                          (x (exact->inexact (* (+ (* (+ hi 2097152) 4294967296) lo) (expt 2 s)))))
                     (list (b:num x) (number->string x))))))
             (c:map (lambda (k) (- k 61)) (b:iota 131))))))

(test number->string-edges
  (c:map (lambda (x) (list (b:num x) (number->string x)))
         (list 1e-4 9.999999999999999e-5 1e-3 1e9 9999999999.999998 1e10 0.1 0.2 0.30000000000000004
               2.2250738585072009e-308 4.9406564584124654e-324 1e-323 2e-323 1.5e-323 1e-320
               3e-310 1.7976931348623157e308 5e-324 -5e-324 -1e10 -1e-5 123e18 1.5e15 1.5e16)))

(test number->string-exact-extra
  (map number->string (list 1/2 -1/2 102/5 -7 0 (expt 2 100) (- (expt 3 70)) 1/1000000000000000000000)))

;; characters, strings and symbols, written, for every code point below 256
;; and a few beyond
(define c:codes (append (c:iota0 256) (list #x2028 #x2029 #x3bb #x10000 #xfeff)))

(test write-chars
  (c:map (lambda (i) (list i (format "~s" (integer->char i)) (format "~a" (integer->char i)))) c:codes))
(test write-strings
  (c:map (lambda (i) (list i (format "~s" (string #\a (integer->char i) #\b)))) c:codes))
(test write-symbols
  (c:map (lambda (i)
           (let ((c (integer->char i)))
             (list i (format "~s" (string->symbol (string c)))
                   (format "~s" (string->symbol (string c #\a)))
                   (format "~s" (string->symbol (string #\a c)))
                   (format "~s" (string->symbol (string #\- c)))
                   (format "~s" (string->symbol (string #\+ c)))
                   (format "~s" (string->symbol (string #\. c))))))
         c:codes))
(test write-symbol-specials
  (c:map (lambda (s) (format "~s" (string->symbol s)))
         (list "" "+" "-" "..." "." ".." "->" "->a" "-a" "+a" "1" "1a" "a1" "1+" "-1" "+1" "1.5" ".5"
               "1/2" "a b" "#t" "#f" "#x" "|" "a|b" "\\" "{" "}" "[" "]" "lmost=>rmost" "[>]:*"
               "a.b" "a'b" "a,b" "a`b" "a\"b" "a;b" "@a" "a@" "-." "+." "-.5" "+i" "-i" "1e5"
               "#%car" "a#b" "LetterCtgy" "Upper")))

;; display, write and their abbreviations, on nested data
(define c:data
  (list '() #t #f 0 -1 1/2 1.5 -0.0 "str" #\a 'sym
        (list 1 (list 2 (list 3)) "s" #\c 'x)
        (cons 1 2) (cons 1 (cons 2 3)) (list (cons 'a 'b))
        (list 'quote 'x) (list 'quote (list 'quote 'x)) (list 'quote) (list 'quote 'x 'y)
        (cons 'quote 'x) (list 'quasiquote (list 'unquote 'y)) (list 'unquote-splicing 'z)
        (list 'syntax 'a) (list 'quasisyntax 'a) (list 'unsyntax 'a) (list 'unsyntax-splicing 'a)
        (list 1 (list 'quote 'x) 2) (list 'a 'quote 'b)
        (vector) (vector 1 "a" #\b (list 'quote 'q)) (list (vector (list 1)))
        (string->symbol "a b") (list "q\"uo\\te" #\space #\newline)
        (void) (list (void))))

(test display-write
  (c:map (lambda (x)
           (list (format "~a" x) (format "~s" x)
                 (let-values (((p get) (open-string-output-port))) (display x p) (get))
                 (let-values (((p get) (open-string-output-port))) (write x p) (get))))
         c:data))

;; format through a variable: a literal bad control string is a compile-time
;; warning, which diff-eval.ss reports as ERROR for the whole test
(define c:format (lambda args (apply format args)))

(test format-more
  (list (format "~a~s~a" 1 "two" 'three) (format "~N~%") (format "~~~~") (format "a~nb")
        (format "~a" (list 1.5 1e21 1e-7 2/3)) (format "~s" (list 1.5 1e21 "x"))
        (c:try (lambda () (c:format "~a"))) (c:try (lambda () (c:format "x" 1)))
        (c:try (lambda () (c:format "~q" 1))) (format #f "~a+~a" 1 2)
        (format "~a" (list "a" (list "b" #\c))) (format "~s" (list "a" (list "b" #\c)))))

(test printf-newline
  (b:capture (lambda () (printf "~a ~s~%" "x" "x") (newline) (printf "~a" 1e22) (newline))))

;;;---------------------------------------------------------------------------
;;; Lists: map, for-each, andmap, ormap, sort, remq/remv/remove, eq?/eqv?/equal?

(test map1-order-long
  (c:map (lambda (n) (with-log (lambda () (map log! (b:iota n))))) (list 0 10 11 25 26 31)))
(test map2-order-long
  (c:map (lambda (n) (with-log (lambda () (map (lambda (a b) (log! (+ a (* 100 b)))) (b:iota n) (b:iota n)))))
         (list 0 1 10 11 25)))
(test map3-order-long
  (c:map (lambda (n) (with-log (lambda () (map (lambda (a b c) (log! (+ a b c))) (b:iota n) (b:iota n) (b:iota n)))))
         (list 0 1 8 13)))
(test map4-order
  (with-log (lambda () (map (lambda (a b c d) (log! (list a b c d))) (b:iota 5) (b:iota 5) (b:iota 5) (b:iota 5)))))
(test map-length-mismatch
  (list (c:try (lambda () (map list (list 1 2) (list 1))))
        (c:try (lambda () (map list (list 1) (list 1 2))))
        (c:try (lambda () (map list (list 1 2) (list 1 2) (list 1))))))
(test for-each-values
  (list (for-each (lambda (x) (* x 10)) (b:iota 3))
        (for-each (lambda (x) x) '())
        (for-each (lambda (a b) (list a b)) (b:iota 3) (list 'a 'b 'c))
        (for-each (lambda (a b c) (+ a b c)) (b:iota 3) (b:iota 3) (b:iota 3))
        (with-log (lambda () (for-each (lambda (a b c) (log! (list a b c))) (b:iota 3) (b:iota 3) (b:iota 3))))
        (c:try (lambda () (for-each list (list 1 2) (list 1))))))
(test andmap-ormap-values
  (list (andmap (lambda (x) x) '()) (ormap (lambda (x) x) '())
        (andmap (lambda (x) (and (< x 5) (* x 10))) (b:iota 3))
        (ormap (lambda (x) (and (> x 1) (* x 10))) (b:iota 3))
        (andmap (lambda (a b) (< a b)) (list 1 2) (list 2 3))
        (ormap (lambda (a b) (and (= a b) (list a b))) (list 1 2) (list 0 2))
        (with-log (lambda () (andmap (lambda (a b) (log! (list a b)) (< a 2)) (b:iota 4) (b:iota 4))))
        (with-log (lambda () (ormap (lambda (a b) (log! (list a b)) (> a 2)) (b:iota 4) (b:iota 4))))))

(test sort-call-order-long
  (b:seeded 51
    (lambda ()
      (c:map (lambda (n)
               (let* ((l (b:repeat n (lambda () (random 5)))))
                 (list (with-log (lambda () (sort (lambda (a b) (log! (list a b)) (< a b)) l)))
                       (with-log (lambda () (sort (lambda (a b) (log! (list a b)) (<= a b)) l)))
                       (with-log (lambda () (sort (lambda (a b) (log! (list a b)) (> a b)) l))))))
             (list 0 1 4 6 7 8 15 16 17 23 24 25 26 27 31 32 33 48 49 50 63 64 65 100 101 150)))))

(test sort-pairs
  (b:seeded 52
    (lambda ()
      (c:map (lambda (n)
               (let* ((l (c:map (lambda (i) (cons (random 3) i)) (b:iota n))))
                 (list (sort (lambda (a b) (< (car a) (car b))) l)
                       (sort (lambda (a b) (<= (car a) (car b))) l)
                       (sort (lambda (a b) (> (car a) (car b))) l)
                       (sort (lambda (a b) (>= (car a) (car b))) l))))
             (list 5 12 24 25 40 77 128 200)))))

(test sort-presorted
  (c:map (lambda (l) (with-log (lambda () (sort (lambda (a b) (log! (list a b)) (< a b)) l))))
         (list (b:iota 30) (reverse (b:iota 30)) (append (b:iota 15) (b:iota 15))
               (append (reverse (b:iota 15)) (b:iota 15)) (c:map (lambda (i) 1) (b:iota 30)))))

(test rem-family
  (list (remq 'a (list 'a 'b 'a 'c 'a)) (remq 'a (list 'a 'a)) (remq 'z '())
        (remv 2 (list 2 2.0 1/2 2)) (remv 2.0 (list 2 2.0 1/2 2.0)) (remv 1/2 (list 1/2 0.5 1/2))
        (remv 0.0 (list 0.0 -0.0 0)) (remv 'a (list 'a 'b))
        (remove 2 (list 2 2.0 2)) (remove (list 1 2) (list (list 1 2) (list 1 2.0) 3 (list 1 2)))
        (remove "ab" (list "ab" "a" "ab")) (remove (vector 1) (list (vector 1) 1))
        (remove 'x (list 'x 'y 'x))))
(test mem-ass-family
  (list (memq 'c (list 'a 'b 'c 'd)) (memq 'z (list 'a)) (memv 2.0 (list 2 2.0 3)) (memv 1/2 (list 0.5 1/2))
        (member (list 1) (list 2 (list 1) 3)) (member 2.0 (list 2 2.0))
        (assq 'b (list (list 'a 1) (list 'b 2))) (assv 2 (list (list 2.0 'x) (list 2 'y)))
        (assoc (list 1) (list (list (list 1) 'z))) (assq 'z (list (list 'a 1)))))
(test eqv-equal
  (c:map (lambda (p) (list (eqv? (car p) (cadr p)) (equal? (car p) (cadr p))))
         (list (list 2 2) (list 2 2.0) (list 1/2 1/2) (list 1/2 0.5) (list 0.0 -0.0) (list 0.0 0.0)
               (list +nan.0 +nan.0) (list 1.5 1.5) (list 'a 'a) (list "a" "a") (list #\a #\a)
               (list (list 1 2) (list 1 2)) (list (list 1 2) (list 1 2.0)) (list '() '())
               (list (vector 1 2) (vector 1 2)) (list #t #t) (list #f '()) (list 0 #f))))

;;;---------------------------------------------------------------------------
;;; Top-level values (slipnet.ss's links, the node and codelet-type macros,
;;; symbol->letter-categories).  Note: set-top-level-value! of an unbound
;;; name binds it, in Chez's interaction environment.

(test top-level-values
  (list (top-level-bound? 'c:never-defined)
        (c:try (lambda () (top-level-value 'c:never-defined)))
        (begin (define-top-level-value 'c:tlv-a 1) (top-level-value 'c:tlv-a))
        (begin (define-top-level-value 'c:tlv-a 2) (top-level-value 'c:tlv-a))
        (begin (set-top-level-value! 'c:tlv-a 3) (top-level-value 'c:tlv-a))
        (top-level-bound? 'c:tlv-a)
        (c:try (lambda () (set-top-level-value! 'c:tlv-b 4) (list (top-level-bound? 'c:tlv-b) (top-level-value 'c:tlv-b))))))

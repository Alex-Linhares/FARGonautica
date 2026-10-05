;;; utilities-battery.scm -- differential checks of racket/compat.rkt and
;;; racket/utilities.rkt against the original under Chez Scheme 10.
;;;
;;; Part of the Racket port of Metacat (GPL v2 or later, like Metacat itself).
;;;
;;; Read form by form, after tests/diff/helpers.scm, by two runners:
;;;   chez_scheme/oracle/diff-eval.ss   (Chez, with the whole original loaded)
;;;   racket/tests/utilities-diff-test.rkt (Racket: racket/base + compat + utilities)
;;; A form (test NAME EXPR) prints "NAME => " and the canonical form of EXPR's
;;; value (or ERROR if it raises); every other form is evaluated as is.
;;; Both outputs must be identical line for line.
;;;
;;; Rules for writing forms here, so that the two *evaluators* cannot differ:
;;; no two side-effecting subexpressions among the arguments of one call or
;;; the bindings of one `let' (Chez does not evaluate them left to right);
;;; sequence with let*, begin or b:repeat instead.  Lists handed to `map'
;;; come from b:iota or b:copy, never quoted constants, because Chez's cp0
;;; inlines map over short constant lists with a different order.
;;; The shared helpers (b:canon, b:seeded, ...) are in helpers.scm.


;; a fake object answering messages, logging the ones it is told
(define make-fake
  (lambda (type value)
    (lambda msg
      (record-case (cdr msg)
        (object-type () type)
        (get-value () value)
        (get-weight () value)
        (print-name () (string-append "fake-" (number->string value)))
        (ascii-name () (string->symbol (string-append "f" (number->string value))))
        (get-orientation () (if (even? value) 'vertical 'horizontal))
        (get-conceptual-depth () (* 10 value))
        (print () (printf "<fake ~a>~%" value))
        (bump (n) (log! (list 'bump value n)) (+ value n))
        (both (a . more) (list a more))
        ((alias1 alias2) () 'aliased)
        (else 'invalid-message-indicator)))))

(define fakes (map (lambda (n) (make-fake 'thing n)) (b:iota 6)))

;;;---------------------------------------------------------------------------
;;; The generator itself (compat.rkt)

(test rng-ints
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (n) (b:repeat 3 (lambda () (random n))))
                  (b:copy (list 1 2 3 10 100 1000 65536 1000003 4294967295
                                4294967296 1099511627776 576460752303423487))))))
       b:seeds))

(test rng-floats
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (x) (b:repeat 3 (lambda () (random x))))
                  (b:copy (list 1.0 0.5 100.0 3.7 1e-300 1e300 2.0))))))
       b:seeds))

(test rng-interleaved
  (b:seeded 17
    (lambda ()
      (b:repeat 200
        (lambda ()
          (let* ((a (random 7)) (b (random 1.0))) (list a b)))))))

(test rng-seed-roundtrip
  (begin (random-seed 12345) (random-seed)))
(test rng-seed-zero (random-seed 0))
(test rng-seed-too-big (random-seed 4294967296))
(test rng-seed-negative (random-seed -5))
(test rng-zero (random 0))
(test rng-negative (random -3))
(test rng-rational (random 1/2))
(test rng-zero-float (random 0.0))
(test rng-negative-float (random -1.0))

;;;---------------------------------------------------------------------------
;;; Random utilities (utilities.ss)

(test prob?
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (p) (b:repeat 4 (lambda () (prob? p))))
                  (b:copy (list 0 0.0 -1 1 1.0 2 0.5 0.001 0.999 1/3 2/3 0.25))))))
       b:seeds))

(test ~
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (n) (b:repeat 4 (lambda () (~ n))))
                  (b:copy (list 0 1 2 4 10 100 2.5 7/2))))))
       b:seeds))

(test random-pick
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (n) (b:repeat 3 (lambda () (random-pick (b:iota n)))))
                  (b:iota 12)))))
       b:seeds))
(test random-pick-empty (b:seeded 5 (lambda () (random-pick '()))))

(test stochastic-pick
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (weights)
                    (b:repeat 5 (lambda () (stochastic-pick (b:iota (length weights)) weights))))
                  (b:copy (list (list 1) (list 1 1) (list 0 0 0) (list 0 5 0) (list 1 2 3 4)
                                (list 0.5 0.25 0.25) (list 1/3 2/3) (list 100 1 1 1 1)
                                (list 7 0 0 7) (list 10.5 20 30.25)))))))
       b:seeds))

(test stochastic-pick-by-method
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (b:repeat 6 (lambda () (tell (stochastic-pick-by-method fakes 'get-weight) 'get-value))))))
       b:seeds))

(test weighted-index
  (map (lambda (w) (weighted-index w (b:copy (list 6 2 4 7 2 4))))
       (b:copy (list 0 5.9 6 7.99 8 11.5 12 17 18.9 19 20.5 21 24.99))))
(test weighted-index-past-end (weighted-index 100 (b:copy (list 1 2))))

(test stochastic-select
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (map (lambda (sel) (b:repeat 5 (lambda () (stochastic-select sel))))
                  (b:copy (list (list (list 1 'a))
                                (list (list 1 'a) (list 3 'b 'c) (list 0 'd))
                                (list (list 0 'x) (list 0 'y))
                                (list (list 0.5 'p) (list 1/2 'q) (list 2 'r))))))))
       b:seeds))

(test weighted-select
  (map (lambda (w) (weighted-select w (b:copy (list (list 6 'a) (list 2 'b) (list 4 'c 1)))))
       (b:copy (list 0 5.99 6 7.5 8 11.99))))

(test stochastic-filter
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (b:repeat 4 (lambda ()
                           (stochastic-filter (lambda (x) (/ x 10)) (b:iota 12)))))))
       b:seeds))

(test bounded-random-partition
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (list
              (bounded-random-partition (lambda (x y) #t) (b:iota 10) 3)
              (bounded-random-partition (lambda (x y) (eq? (even? x) (even? y))) (b:iota 11) 2)
              (bounded-random-partition (lambda (x y) (< (abs (- x y)) 3)) (b:iota 9) 4)
              (bounded-random-partition (lambda (x y) #f) (b:iota 4) 5)
              (bounded-random-partition (lambda (x y) #t) '() 5)))))
       b:seeds))

(test randomize-range
  (let* ((s (begin (randomize) (random-seed))))
    (and (integer? s) (> s 0) (< s 4294967296))))

;;;---------------------------------------------------------------------------
;;; Evaluation order inside utilities and Chez built-ins

(test map1-order (map (lambda (n) (with-log (lambda () (map log! (b:iota n))))) (b:iota 9)))
(test map2-order
  (map (lambda (n) (with-log (lambda () (map (lambda (a b) (log! (+ a (* 10 b)))) (b:iota n) (b:iota n)))))
       (b:iota 9)))
(test map3-order
  (map (lambda (n) (with-log (lambda () (map (lambda (a b c) (log! a)) (b:iota n) (b:iota n) (b:iota n)))))
       (b:iota 7)))
(test map-as-value (let* ((m map)) (with-log (lambda () (m log! (b:iota 7))))))
(test apply-map (with-log (lambda () (apply map log! (list (b:iota 7))))))
(test for-each-order (with-log (lambda () (for-each log! (b:iota 6)))))
(test for-each2-order (with-log (lambda () (for-each (lambda (a b) (log! (list a b))) (b:iota 4) (b:iota 4)))))
(test andmap-order (with-log (lambda () (andmap (lambda (x) (log! x) (< x 4)) (b:iota 6)))))
(test ormap-order (with-log (lambda () (ormap (lambda (x) (log! x) (> x 3)) (b:iota 6)))))
(test tell-all-order (with-log (lambda () (tell-all fakes 'bump 100))))
(test delegate-to-all-order
  (with-log (lambda () (delegate-to-all (list 'self 'bump 7) (car fakes) (cadr fakes) (caddr fakes)))))
(test delegate-to-all-invalid
  (with-log (lambda () (delegate-to-all (list 'self 'nope) (car fakes) (cadr fakes)))))
(test flatmap-order (with-log (lambda () (flatmap (lambda (x) (log! x) (list x x)) (b:iota 5)))))
(test map-compress-order
  (with-log (lambda () (map-compress (lambda (x) (log! x) (and (odd? x) x)) (b:iota 7)))))
(test cross-product-map-order
  (with-log (lambda () (cross-product-map (lambda (x y) (log! (list x y))) (b:iota 3) (b:iota 2)))))
(test cross-product-filter-map-order
  (with-log (lambda ()
              (cross-product-filter-map (lambda (x y) (log! (list 'p x y)) (odd? (+ x y)))
                                        (lambda (x y) (log! (list 'f x y)))
                                        (b:iota 3) (b:iota 3)))))
(test cross-product-map-filter-order
  (with-log (lambda ()
              (cross-product-map-filter (lambda (x y) (log! (list 'f x y)) (+ x y))
                                        (lambda (v) (log! (list 'p v)) (odd? v))
                                        (b:iota 3) (b:iota 3)))))
(test cross-product-ormap-order
  (with-log (lambda () (cross-product-ormap (lambda (x y) (log! (list x y)) (= (+ x y) 5)) (b:iota 3) (b:iota 3)))))
(test cross-product-andmap-order
  (with-log (lambda () (cross-product-andmap (lambda (x y) (log! (list x y)) (< (+ x y) 5)) (b:iota 3) (b:iota 3)))))
(test cross-product-for-each-order
  (with-log (lambda () (cross-product-for-each (lambda (x y) (log! (list x y))) (b:iota 2) (b:iota 3)))))
(test pairwise-map-order
  (with-log (lambda () (pairwise-map (lambda (x y) (log! (list x y))) (b:iota 5)))))
(test pairwise-do-order
  (with-log (lambda () (pairwise-do (lambda (x y) (log! (list x y))) (b:iota 4)))))
(test pairwise-andmap-order
  (with-log (lambda () (pairwise-andmap (lambda (x y) (log! (list x y)) (< (+ x y) 6)) (b:iota 5)))))
(test filter-order (with-log (lambda () (filter (lambda (x) (log! x) (odd? x)) (b:iota 6)))))
(test filter-map-order
  (with-log (lambda () (filter-map (lambda (x) (log! (list 'p x)) (odd? x)) (lambda (x) (log! (list 'f x))) (b:iota 5)))))
(test map-filter-order
  (with-log (lambda () (map-filter (lambda (x) (log! (list 'f x))) (lambda (v) (log! (list 'p v)) #t) (b:iota 4)))))
(test map-leaves-order
  (with-log (lambda () (map-leaves log! (list 1 (list 2 (list 3 4)) 5 (list 6))))))
(test select-extreme-order
  (with-log (lambda () (select-extreme max (lambda (x) (log! x) (- 10 x)) (b:iota 6)))))
(test adjacency-map-order
  (with-log (lambda () (adjacency-map (lambda (x y) (log! (list x y))) (b:iota 5)))))
(test count-order (with-log (lambda () (count (lambda (x) (log! x) (odd? x)) (b:iota 6)))))
(test partition-order
  (with-log (lambda () (partition (lambda (x y) (log! (list x y)) (eq? (odd? x) (odd? y))) (b:iota 5)))))
(test bounded-random-partition-order
  (b:seeded 3 (lambda () (with-log (lambda () (bounded-random-partition (lambda (x y) (log! (list x y)) #t) (b:iota 6) 2))))))
(test stochastic-filter-order
  (b:seeded 3 (lambda () (with-log (lambda () (stochastic-filter (lambda (x) (log! x) 0.5) (b:iota 6)))))))
(test intersect-pred-order
  (with-log (lambda () (intersect-pred (lambda (x y) (log! (list x y)) (= x y)) (b:iota 3) (b:iota 4)))))
(test remove-duplicates-pred-order
  (with-log (lambda () ((remove-duplicates-pred (lambda (x y) (log! (list x y)) (= x y))) (list 1 2 1 3 2)))))
(test sort-by-method-order
  (with-log (lambda () (map (lambda (o) (tell o 'get-value))
                            (sort-by-method 'get-value (lambda (a b) (log! (list a b)) (> a b)) fakes)))))

;;;---------------------------------------------------------------------------
;;; sort (Chez argument order and algorithm)

(define b:random-list
  (lambda (n k) (b:repeat n (lambda () (random k)))))

(test sort-lists
  (b:seeded 11
    (lambda ()
      (map (lambda (n)
             (let* ((l (b:random-list n 10)))
               (list (sort < l) (sort > l) (sort <= l) (sort >= l))))
           (b:copy (list 0 1 2 3 4 5 8 13 24 25 26 40 61 100))))))

(test sort-stable
  (b:seeded 12
    (lambda ()
      (map (lambda (n)
             (let* ((l (map (lambda (i) (cons (random 4) i)) (b:iota n))))
               (list (sort (lambda (a b) (< (car a) (car b))) l)
                     (sort (lambda (a b) (<= (car a) (car b))) l))))
           (b:copy (list 2 3 7 24 25 30 57))))))

(test sort-call-order
  (b:seeded 13
    (lambda ()
      (map (lambda (n)
             (let* ((l (b:random-list n 6)))
               (with-log (lambda () (sort (lambda (a b) (log! (list a b)) (< a b)) l)))))
           (b:copy (list 2 3 5 9 24 25 33))))))

(test sort-wrt-order (sort-wrt-order (list 'c 'a 'd 'b) (list 'a 'b 'c 'd 'e)))

;;;---------------------------------------------------------------------------
;;; Printing: format and number->string (compat.rkt)

(define b:flonums
  (list 0.0 -0.0 1.0 -1.0 10.0 100.0 0.1 0.5 3.14 123.456 1e-7 1e21 1e22
        1.2345678901234567e19 123456789012.5 1e10 9999999999.0 999999999.0
        1e9 1e20 0.001 0.0001 0.00012 1e-5 2.5e-5 5e-324 1e-310 2.2250738585072014e-308
        1.5e300 1.7976931348623157e308 12345678901234567890.0 99999999999999999999.0
        1e-10 0.3333333333333333 0.6666666666666666 -123.5e-20 4503599627370496.0
        9007199254740993.0 0.30000000000000004 1e100 -2e-3))

(test number->string-literals (map (lambda (x) (list (b:num x) (number->string x))) b:flonums))

(test number->string-random
  (b:seeded 21
    (lambda ()
      (map (lambda (scale)
             (b:repeat 25 (lambda () (let* ((x (random scale))) (list (b:num x) (number->string x))))))
           (b:copy (list 1e-300 1e-20 1e-6 1e-4 1e-3 1.0 10.0 1e5 1e9 1e10 1e11 1e17 1e21 1e300))))))

(test number->string-exact
  (map number->string (list 0 -7 123456789012345678901234567890 1/3 -22/7 (make-rectangular 1 2)
                            (make-rectangular 1.5 -2.0) (make-rectangular 0 1))))

(test number->string-special
  (map number->string (list (/ 1.0 0.0) (/ -1.0 0.0))))

(test format-a
  (map (lambda (x) (format "~a" x))
       (list 1 1.5 1e-7 1e21 "str" "q\"uo\\te" #\c 'sym 'LetterCtgy '() #t #f
             (list 1 2.5 "s" #\c 'x) (list 'quote 'x) (list 'quasiquote (list 'unquote 'y))
             (list 'unquote-splicing 'z) (list 'quote 'x 'y) (cons 1 2) (list 1 (cons 2 3))
             (vector 1 "a" #\b) (vector) (string->symbol "a b") (string->symbol "1+")
             (string->symbol "") (list (list 1 2) (vector (list 3))) 2/3 (make-rectangular 1 2))))

(test format-s
  (map (lambda (x) (format "~s" x))
       (list 1 1.5 1e-7 "str" "q\"uo\\te" "new\nline" "tab\there" #\c #\space #\newline #\tab
             #\nul 'sym 'LetterCtgy '() #t #f (list 1 2.5 "s" #\c 'x) (list 'quote 'x)
             (list 'quasiquote (list 'unquote 'y)) (cons 1 2) (vector 1 "a" #\b)
             (string->symbol "a b") (string->symbol "1+") (string->symbol "+") (string->symbol "-")
             (string->symbol "...") (string->symbol "->x") (string->symbol "1x")
             (string->symbol "#foo") (string->symbol "a|b") (string->symbol "lmost=>rmost")
             (string->symbol "[>]:*") (string->symbol "Upper") (string->symbol "x;y")
             (string->symbol "q'r") (string->symbol "+5") (string->symbol "1/2")
             (string->symbol "3.5") (string->symbol "a(b"))))

(test format-directives
  (list (format "a~%b~nc~~d") (format "~a and ~s" "x" "x") (format "") (format "no directives")
        (format "~a~a~a" 1 'two "three") (format "~A ~S" "x" "y")))

(test format-numbers-in-lists
  (b:seeded 22 (lambda () (format "~a ~s" (b:repeat 5 (lambda () (random 1e12))) (list 1e-5 (vector 2e30))))))

(test format-void (format "~a" (void)))

;; printf, newline and print write to the output port
(test printf-output (b:capture (lambda () (printf "x=~a y=~s~%" 1.5 "s") (newline) (printf "~a" 'done))))
(test print-output (b:capture (lambda () (print 1 (list 2 (list "three")) (car fakes) 1e-9))))
(test say-object-output (b:capture (lambda () (say-object "str") (say-object (cadr fakes)) (say-object 2.5))))

;;;---------------------------------------------------------------------------
;;; Objects (utilities.ss)

(test tell (tell (car fakes) 'get-value))
(test tell-args (tell (car fakes) 'both 1 2 3))
(test tell-alias (list (tell (car fakes) 'alias1) (tell (car fakes) 'alias2)))
(test tell-invalid
  (let* ((out (b:capture
                (lambda ()
                  (call/cc (lambda (k)
                             (parameterize ((reset-handler (lambda () (k 'reset))))
                               (tell (car fakes) 'no-such-message 1))))))))
    out))
(test base-object (list (base-object 'self 'object-type) (base-object 'self 'other)))
(test compose (list ((compose car cdr) (list 1 2 3)) ((compose (lambda (x) (* 2 x))) 5)
                    ((compose car cdr cdr) (list 1 2 3))))
(test delegate
  (let* ((parent (lambda msg (record-case (cdr msg) (inherited () 'from-parent) (else 'invalid-message-indicator))))
         (child (lambda msg (record-case (cdr msg) (own () 'mine) (else (delegate msg (car fakes) parent))))))
    (list (tell child 'own) (tell child 'inherited) (tell child 'get-value)
          (child child 'unknown))))
(test delegate-to-all
  (delegate-to-all (list 'self 'get-value) (car fakes) (cadr fakes)))
(test type-testers
  (let* ((objs (map (lambda (t) (make-fake t 2))
                    (list 'bond 'letter 'group 'bridge 'concept-mapping 'rule 'description
                          'workspace-string 'workspace 'answer-description 'snag-description
                          'slipnode 'answer-event 'generic-event 'thing))))
    (map (lambda (o)
           (list (bond? o) (letter? o) (group? o) (bridge? o) (concept-mapping? o) (rule? o)
                 (description? o) (workspace-string? o) (workspace? o) (answer-description? o)
                 (snag-description? o) (exists? (event? o)) (slipnode? o)
                 (vertical-bridge? o) (horizontal-bridge? o)))
         objs)))
(test event?-value (event? (make-fake 'group-event 1)))
(test slipnode?-nonprocedure (slipnode? 5))
(test bridges-orientation
  (list (vertical-bridge? (make-fake 'bridge 2)) (horizontal-bridge? (make-fake 'bridge 3))))
(test cd (cd (caddr fakes)))
(test reveal
  (list (reveal 5) (reveal (make-fake 'letter 3)) (reveal (list 1 (make-fake 'group 4) (list (make-fake 'rule 1))))))

;;;---------------------------------------------------------------------------
;;; Plain utilities

(test exists (list (exists? #f) (exists? 0) (exists? '()) (all-exist? (list 1 2)) (all-exist? (list 1 #f))
                   (all-exist? '())))
;; Racket interns literal flonums and strings (so two 1.5 literals are eq?),
;; Chez does not; values computed at run time are used where eq? matters.
(define b:x1.5 (exact->inexact 3/2))
(test all-same (list (all-same? '()) (all-same? (list 'a 'a)) (all-same? (list 'a 'b))
                     (all-same? (list b:x1.5 b:x1.5)) (all-same? (list b:x1.5 (exact->inexact 3/2)))))
(test compress (compress (list 1 #f 2 #f #f 3)))
(test map-compress (map-compress (lambda (x) (and (odd? x) (* x x))) (b:iota 7)))
(test flatmap (flatmap (lambda (x) (b:iota x)) (b:iota 4)))
(test rounding
  (map (lambda (x) (list (truncate x) (ceiling x) (floor x) (round x)))
       (list 2.5 -2.5 3.5 0.5 -0.5 7/2 5/2 -7/2 3.7 -3.7 1e20 4 0.0 -0.0 1/3 2/3 1e300)))
(test round-to
  (map (lambda (x) (list (round-to-10ths x) (round-to-100ths x) (round-to-1000ths x)))
       (list 0.12345 2/3 12.3456 -0.0049 0.005 0.015 0.025 1e-20 7 1.05)))
(test powers (list (^2 3) (^2 1.5) (^2 1/3) (^3 2) (^3 -1.5) (^3 2/3)))
;; (ascending-index-list 0) loops forever in the original, so it is not tested.
(test index-lists (list (ascending-index-list 1) (ascending-index-list 5)
                        (descending-index-list 0) (descending-index-list 1) (descending-index-list 5)))
(define plato-x 'node-x)
(define plato-y 'node-y)
(define-top-level-value 'plato-x 'node-x)
(define-top-level-value 'plato-y 'node-y)
(test symbol->letter-categories (symbol->letter-categories 'xyxx))
(test char-index (list (char-index #\b "abcb") (char-index #\z "abc") (char-index #\a "")))
(test strings (list (string-upcase "abC1") (string-downcase "ABc1") (capitalize-string "hello world")
                    (capitalize-string "") (capitalize-string "x") (string-suffix "abcdef" 2)
                    (string-suffix "abc" 3)))
(test quoted-string (quoted-string (cadr fakes)))
(test tables
  (let* ((t (make-table 2 3))
         (u (make-table 3 2 'z)))
    (table-set! t 0 1 'a)
    (table-set! t 1 2 'b)
    (table-set! u 2 1 'q)
    (list t u (row-dimension t) (column-dimension t) (table->list t) (table-ref t 1 2)
          (get-row t 0) (get-column t 1) (get-column u 1))))
(test table-ops
  (let* ((t (make-table 2 2 0))
         (s (make-table 2 2 7))
         (v (vector 1 2 3))
         (w (make-vector 3 #f)))
    (vector-increment! v 1 10)
    (initialize-row! t 0 5)
    (initialize-column! t 1 9)
    (copy-vector-contents! v w)
    (copy-table-contents! s t)
    (add-scaled-vector! v (vector 1 1 1) 1/2)
    (list t s v w)))
(test make-vector-default (make-vector 3))
(test rotate (rotate-90-degrees-clockwise (vector (vector 1 2 3) (vector 4 5 6))))
(test sums (list (sum '()) (sum (list 1 2.5 1/2)) (product '()) (product (list 2 3 1/4))))
(test averages
  (list (average (list 1 2 3 4)) (average 1 2) (average '()) (average 5) (average (list 1.0 2))
        (average 1 2 3) (weighted-average (list 1 2 3) (list 0 0 0))
        (weighted-average (list 10 20) (list 1 3)) (weighted-average (list 1.5 2) (list 1/2 1/2))))
(test log10 (map log10 (list 1 10 100 1000 0.001 2 50 1e-10 0.5)))
(test sgn (list (sgn -3) (sgn 0) (sgn 2.5) (sgn -0.0)))
(test list-index (list (list-index (list 'a 'b 'c) 'c) (list-index (list 'a 'b 'c) 'a)))
(test list-index-missing (list-index (list 'a 'b) 'z))
(test arith-shortcuts (list (100- 30) (10- 2.5) (1- 1/4) (100* 0.123) (100* 1/3) (100* 0.125) (% 50) (% 2.5)
                            (20% 10) (40% 1.5) (80% 5)))
(test sigmoid
  (map (lambda (bm) (map (sigmoid (car bm) (cadr bm)) (list 0 10 25 50 75 100 33.3)))
       (list (list 5 50) (list 2 30) (list 10 80) (list 1/2 50))))
(test clip (map (clip-function 0 100) (list -5 0 50 100 150 99.5 1/2)))
(test flatten (list (flatten '()) (flatten (list 1 (list 2 (list 3 '()) 4) (list (list 5))))
                    (flatten (list (cons 1 2)))))
(test select-longest-list
  (list (select-longest-list '()) (select-longest-list (list (list 1) (list 1 2) (list 3 4) '()))))
(test select-extreme
  (list (select-extreme min abs (list -3 2 -1 1)) (select-extreme max length (list (list 1) (list 2 3)))
        (select-extreme max (lambda (x) x) '()) (select-extreme max (lambda (x) x) (list 1 2.0 2))))
(test max-min (list (maximum '()) (maximum (list 1 3.5 2)) (minimum '()) (minimum (list 3 1/2 2))
                    (maximum (list 1 2)) (minimum (list 2 1.0))))
(test count (count odd? (b:iota 7)))
(test adjacency-map (adjacency-map list (b:iota 4)))
(test select (list (select even? (b:iota 5)) (select even? (list 1 3)) (select even? '())))
(test filters (list (filter odd? (b:iota 7)) (filter-out odd? (b:iota 7)) (filter odd? '())))
(test map-leaves (map-leaves (lambda (x) (* x 10)) (list 1 (list 2 (list 3)) '() 4)))
(test filter-map (filter-map odd? (lambda (x) (* x x)) (b:iota 6)))
(test map-filter (map-filter (lambda (x) (* x x)) even? (b:iota 6)))
(test cross-products
  (list (cross-product (list 1 2) (list 'a 'b 'c)) (cross-product '() (list 1))
        (cross-product-filter (lambda (x y) (< x y)) (b:iota 3) (b:iota 3))
        (cross-product-map + (list 1 2) (list 10 20))
        (cross-product-ormap = (list 1 2) (list 3 2)) (cross-product-andmap < (list 1 2) (list 3 4))
        (cross-product-for-each (lambda (x y) x) (list 1) (list 2))))
(test method-procedures
  (list (tell (select-meth fakes 'both 1) 'get-value)
        (length (filter-meth fakes 'get-value)) (filter-out-meth fakes 'get-value)
        (andmap-meth fakes 'get-value) (ormap-meth fakes 'get-orientation)
        (count-meth fakes 'get-value)))
(test pairwise (list (pairwise-map list (b:iota 4)) (pairwise-map list '()) (pairwise-do list (b:iota 3))
                     (pairwise-andmap < (b:iota 4)) (pairwise-andmap > (b:iota 4)) (pairwise-andmap > '())))
(test intersections
  (list (intersect (list 'a 'b 'c) (list 'c 'a 'd)) (intersect-pred = (list 1 2 3) (list 3.0 1))
        (intersect-all (list (list 'a 'b 'c) (list 'b 'c) (list 'c 'b 'x)))
        (intersect-all '()) (intersect-all (list (list 1 2)))
        (intersect-all-pred equal? (list (list "a" "b") (list "b")))))
(test partition (list (partition (lambda (x y) (eq? (odd? x) (odd? y))) (b:iota 7)) (partition = '())))
(test members (list (member? 'b (list 'a 'b)) (member? 'z (list 'a)) (member-pred? = 2 (list 1 2.0))
                    (member-equal? (list 1) (list (list 1))) (member? (string #\a) (list (string #\a)))))
(test sets
  (list (subset? (list 'a) (list 'a 'b)) (subset? (list 'c) (list 'a)) (subset-pred? = (list 1) (list 1.0))
        (sets-equal? (list 'a 'b) (list 'b 'a)) (sets-equal? (list 'a) (list 'a 'b))
        (sets-equal-pred? = (list 1 2) (list 2.0 1.0)) (sets-disjoint? (list 'a) (list 'b))
        (sets-disjoint? (list 'a) (list 'a)) (sets-intersect? (list 'a 'b) (list 'b))
        (sets-intersect? '() (list 'b))))
(test removals
  (list (remove-elements-pred = (list 1 2) (list 1 2 3 1.0 4)) (remq-elements (list 'a) (list 'a 'b 'a))
        (remove-elements (list "x") (list "x" "y" "x")) ((remove-duplicates-pred =) (list 1 1.0 2))
        (remq-duplicates (list 'a 'b 'a 'c 'b)) (remove-duplicates (list "a" "b" "a"))))
(test list-access
  (let* ((l (list 1 2 3 4 5 6 7 8 9)))
    (list (1st l) (2nd l) (3rd l) (4th l) (5th l) (6th l) (7th l) (8th l) (rest l)
          (get-first 3 l) (get-first 0 l) (sublist l 2 5) (nth 4 l) (snoc 10 (list 1)) (last l)
          (all-but-last 2 l) (all-but-last 0 (list 1)))))
(test coords (let* ((c (coord 3 4))) (list c (x-coord c) (y-coord c) (coord 1.5 0) (x-coord 7))))

;;;---------------------------------------------------------------------------
;;; Chez built-ins the engine relies on (compat.rkt)

(test remq (list (remq 'a (list 'a 'b 'a 'c)) (remq 'z (list 'a)) (remq 'a '())))
(test remv (list (remv 1 (list 1 2 1 3)) (remv 1.5 (list 1.5 2))))
(test remove (list (remove "a" (list "a" "b" "a")) (remove (list 1) (list (list 1) 2))))
(test 1+ (list (1+ 5) (1+ 1.5) (-1+ 5) (add1 1/2) (sub1 0)))
(test assoc-family (list (assq 'b (list (list 'a 1) (list 'b 2))) (assv 2.0 (list (list 2 'x) (list 2.0 'y)))
                         (assoc "b" (list (list "a" 1) (list "b" 2))) (assq 'z '())))
(test member-family (list (memq 'c (list 'a 'b 'c 'd)) (member "b" (list "a" "b")) (memv 1.0 (list 1 1.0))))
(test record-case
  (map (lambda (msg)
         (record-case msg
           (one () 'one)
           ((two deux) (x) (list 'two x))
           (rest-args (a . r) (list a r))
           (all-args args args)
           (else 'other)))
       (list (list 'one) (list 'two 2) (list 'deux 3) (list 'rest-args 1 2 3) (list 'all-args 4 5)
             (list 'zz))))
(test record-case-no-else (record-case (list 'zz) (one () 'one)))
;; Chez's case: a single datum as a clause key (coderack.ss uses it)
(test case-single-datum
  (map (lambda (x) (case x (a 'one) ((b c) 'two) (7 'seven) (else 'other)))
       (b:copy (list 'a 'b 'c 7 'd))))
(test arithmetic
  (list (exp 0) (exp 1) (sqrt 16) (sqrt 2) (sqrt 1/4) (expt 2 10) (expt 2 0.5) (expt 2.0 3)
        (expt 1000 5/3) (expt 8 1/3) (log 10) (log 1) (atan 1 1) (exact->inexact 1/3)
        (inexact->exact 0.1) (/ 1 3) (/ 6 3) (* 1/25 3 (- 50 20)) (max 1 2.0) (min 1 2.0)
        (string->number "1e3") (string->number "1/2") (string->number "abc") (exact->inexact 12345678901234567890)
        (expt 0 0) (expt 0.0 0) (/ 7 2.0) (quotient 7 2) (remainder -7 2) (modulo -7 2) (abs -1/2)))

;;;---------------------------------------------------------------------------
;;; The syntactic-sugar.ss macros (compat.rkt)

(test for*-each (with-log (lambda () (for* each x in (b:iota 3) do (log! x) (log! (* 10 x))))))
(test for*-each-multi (with-log (lambda () (for* each (x y) in ((b:iota 3) (list 'a 'b 'c)) do (log! (list x y))))))
(test for*-from-to (with-log (lambda () (for* i from 2 to 5 do (log! i)))))
(test for*-empty (with-log (lambda () (for* i from 5 to 4 do (log! i)))))
(test for*-single (with-log (lambda () (for* i from 3 to 3 do (log! i)))))
(test for*-bounds-order (with-log (lambda () (for* i from (log! 1) to (log! 3) do (log! (list 'i i))))))
(test for*-value (for* each x in (list 1) do x))
(test for-each-vector-element*
  (with-log (lambda () (let* ((v (vector 'a 'b 'c))) (for-each-vector-element* (v i) do (log! (list i (vector-ref v i))))))))
(test for-each-table-element*
  (with-log (lambda () (let* ((t (vector (vector 1 2) (vector 3 4) (vector 5 6))))
                         (for-each-table-element* (t i j) do (log! (list i j (vector-ref (vector-ref t i) j))))))))
(test repeat*-times (with-log (lambda () (repeat* 3 times (log! 'x) (log! 'y)))))
(test repeat*-zero (with-log (lambda () (repeat* 0 times (log! 'x)))))
(test repeat*-until
  (with-log (lambda () (let* ((n 0)) (repeat* until (>= n 3) (log! n) (set! n (+ n 1)))))))
(test repeat*-forever
  (with-log (lambda ()
              (call/cc (lambda (k) (let* ((n 0)) (repeat* forever (log! n) (set! n (+ n 1)) (if (= n 4) (k 'out)))))))))
(test if* (list (if* #t 1 2) (if* #f 1 2)))
(test stochastic-if*
  (map (lambda (seed)
         (b:seeded seed
           (lambda ()
             (b:repeat 6 (lambda () (with-log (lambda () (stochastic-if* (log! 0.5) (log! 'yes) 'done))))))))
       b:seeds))
(test stochastic-if*-certain
  (b:seeded 9 (lambda () (list (stochastic-if* 1 'a) (stochastic-if* 0 'b) (stochastic-if* 1.5 'c)))))
(test continuation-point*
  (list (continuation-point* k 1 (k 2) 3) (continuation-point* k 1 2)))
(define %verbose% #f)
(test say-quiet (b:capture (lambda () (say "a" 1 (cadr fakes)) (vprintf "x~a" 1) (vprint 1 2))))
(test say!-quiet (b:capture (lambda () (say! "a" 1.5 (cadr fakes)))))
(test say-verbose
  (b:capture (lambda ()
               (set! %verbose% #t)
               (say "a" 1 (cadr fakes)) (vprintf "x~a~%" 1) (vprint 1 (list 2.5 'z))
               (set! %verbose% #f))))
(define *control-panel* (lambda msg (record-case (cdr msg) (run-new-problem (tokens) (list 'ran tokens)))))
(test mcat-3 (mcat abc abd xyz))
(test mcat-4 (mcat abc abd xyz 7))
(test mcat-4-sym (mcat abc abd xyz wyz))
(test mcat-5 (mcat abc abd xyz wyz 99))
(test mcat-bad-2 (mcat abc abd))
(test mcat-bad-num (mcat abc abd xyz 0))
(test mcat-bad-5 (mcat abc abd xyz 7 7))
(test valid-token-list
  (list (valid-token-list? (list 'a 'b 'c)) (valid-token-list? (list 'a 'b 'c 4294967295))
        (valid-token-list? (list 'a 'b 'c 4294967296)) (valid-token-list? (list 'a 'b 1))
        (valid-token-list? 'a) (valid-token-list? (list 'a 'b 'c 'd 2.5))
        (symbol-or-valid-number? 'q) (valid-number? 0) *largest-random-seed*))
(test concatenate-symbols (list (concatenate-symbols 'a '- 'b) (concatenate-symbols)))

;; slipnet macros, with stand-ins for the slipnet procedures they call
(define make-slipnode (lambda (name short depth) (log! (list 'make-slipnode name short depth)) (list 'node name)))
(test slipnet-node-list*
  (with-log (lambda ()
              (let* ((nodes (slipnet-node-list* (plato-p "p" conceptual-depth: 10)
                                                (plato-q "q" conceptual-depth: 20))))
                (list nodes (top-level-value 'plato-p) (top-level-value 'plato-q))))))
(test slipnet-layout-table* (slipnet-layout-table* (1 2 3) (4 5 6)))
(define establish-link
  (lambda (name from to type)
    (log! (list 'establish-link name from to type))
    (define-top-level-value name
      (lambda msg (log! (cons name (cdr msg))) 'done))))
(define plato-z 'node-z)
(define plato-succ 'node-succ)
(test category-link* (with-log (lambda () (category-link* x --> y length: 50))))
(test category-link*-all (with-log (lambda () (category-link* (x y) --> z all-lengths: 60))))
(test instance-link* (with-log (lambda () (instance-link* x --> y length: 100))))
(test instance-link*-all (with-log (lambda () (instance-link* z --> (x y) all-lengths: 70))))
(test property-link* (with-log (lambda () (property-link* x --> z length: 75))))
(test lateral-link*-length (with-log (lambda () (lateral-link* x --> y length: 10))))
(test lateral-link*-label (with-log (lambda () (lateral-link* x --> y label: succ))))
(test lateral-link*-both (with-log (lambda () (lateral-link* x --> y length: 10 label: succ))))
(test lateral-link*-two-way (with-log (lambda () (lateral-link* x <--> z label: succ))))
(test lateral-sliplink*-label (with-log (lambda () (lateral-sliplink* x --> y label: succ))))
(test lateral-sliplink*-length (with-log (lambda () (lateral-sliplink* y --> z length: 20))))
(test lateral-sliplink*-two-way (with-log (lambda () (lateral-sliplink* x <--> y length: 30))))
(test link-top-level-value (procedure? (top-level-value 'x-y-link)))

;; coderack macros, with stand-ins for the coderack
(define make-codelet-type
  (lambda (name labels)
    (log! (list 'make-codelet-type name labels))
    (let ((proc #f))
      (lambda msg
        (record-case (cdr msg)
          (make-codelet (urgency . args) (list 'codelet name urgency args))
          (set-codelet-procedure (p) (set! proc p) 'done)
          (run args (apply proc args))
          (else 'invalid-message-indicator))))))
(define *coderack* (lambda msg (record-case (cdr msg) (post (c) (log! (list 'post c)) 'posted))))
(test codelet-type-list*
  (with-log (lambda ()
              (let* ((types (codelet-type-list* (test-scout "Test" "scout") (test-builder "Test builder"))))
                (list (length types) (eq? (car types) (top-level-value 'test-scout)))))))
(define test-scout (top-level-value 'test-scout))
(test post-codelet* (with-log (lambda () (post-codelet* urgency: 35 test-scout 'arg1 (list 2 3)))))
(test post-codelet*-no-args (with-log (lambda () (post-codelet* urgency: 1/2 test-scout))))
(define-codelet-procedure* test-scout
  (lambda (x)
    (log! (list 'start x))
    (if (eq? x 'quit) (fizzle))
    (log! (list 'end x))
    (list 'result x)))
(test define-codelet-procedure*
  (let* ((a (with-log (lambda () (tell test-scout 'run 'go))))
         (a-fizzle (procedure? fizzle))
         (b (with-log (lambda () (tell test-scout 'run 'quit))))
         (b-fizzle fizzle))
    (list a a-fizzle b b-fizzle)))
(test define-codelet-procedure*-verbose
  (b:capture (lambda () (set! %verbose% #t) (tell test-scout 'run 'go) (set! %verbose% #f))))

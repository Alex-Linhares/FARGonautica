#lang racket/base
;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================
;; Ported to Racket, 2026: syntactic-sugar.ss, plus the Chez Scheme 10
;; built-ins Metacat relies on where Racket lacks them or differs.

;; The compatibility layer.  Engine modules require this module (and
;; utilities.rkt); its bindings shadow racket/base's.  Sections:
;;   1. Chez's global random number generator (random, random-seed)
;;   2. Chez list procedures: map (Chez's order), sort (Chez's algorithm and
;;      argument order), remq/remv/remove (all occurrences), 1+, -1+
;;   3. top-level values (define-top-level-value, top-level-value, ...)
;;   4. Chez printing: number->string, display, write, format, fprintf,
;;      error; plus syntactic-sugar.ss's printf and newline
;;   5. record-case, reset, collect, real-time
;;   6. syntactic-sugar.ss: definitions and the 22 extend-syntax macros
;; See docs/porting-notes.md (item 03) for what each one reproduces.

(require (for-syntax racket/base)
         (prefix-in rkt: racket/base)
         (only-in ffi/unsafe/vm vm-primitive))

(provide random random-seed
         (rename-out [chez-if if] [chez-case case] [chez-map map] [chez-for-each for-each]) sort remq remv remove 1+ -1+
         tanh
         define-top-level-value set-top-level-value! top-level-value top-level-bound?
         number->string display write format fprintf printf newline error
         record-case reset reset-handler (struct-out metacat-reset) collect real-time
         *largest-random-seed* concatenate-symbols
         valid-token-list? valid-number? symbol-or-valid-number?
         fizzle set-fizzle!
         mcat for* for-each-vector-element* for-each-table-element* repeat* if*
         stochastic-if* continuation-point* say say! vprintf vprint
         slipnet-node-list* define-slipnet-node-list* slipnet-layout-table*
         category-link* instance-link* property-link* lateral-link* lateral-sliplink*
         codelet-type-list* define-codelet-type-list* post-codelet* define-codelet-procedure*)

;;;===========================================================================
;;; 1. The random number generator (docs/trace-format.md)
;;;
;;; Chez 10's global generator: one 32-bit state S, S := S*72931 + 90763387
;;; mod 2^32.  Racket's own `random' is never used.

(define rng-state 1)

(define (rng-step!)
  (set! rng-state (bitwise-and (+ (* rng-state 72931) 90763387) #xFFFFFFFF))
  rng-state)

(define (rng-high16!) (arithmetic-shift (rng-step!) -16))

(define most-positive-chez-fixnum (- (expt 2 60) 1))

(define (random x)
  (cond
    [(and (exact-positive-integer? x) (<= x most-positive-chez-fixnum))
     (let* ([s1 (rng-step!)]
            [s2 (rng-step!)]
            [t (+ (arithmetic-shift s1 -16) (bitwise-and s2 #xFFFF0000))])
       (if (<= x #xFFFFFFFF)
           (modulo t x)
           (let* ([t (bitwise-and (+ (* t 65536) (rng-high16!)) #xFFFFFFFFFFFFFFFF)]
                  [t (bitwise-and (+ (* t 65536) (rng-high16!)) #xFFFFFFFFFFFFFFFF)])
             (modulo t x))))]
    [(and (flonum? x) (> x 0.0))
     (let* ([h1 (bitwise-and (rng-high16!) 15)]
            [h2 (rng-high16!)]
            [h3 (rng-high16!)]
            [h4 (rng-high16!)]
            [m (+ (arithmetic-shift h1 48) (arithmetic-shift h2 32)
                  (arithmetic-shift h3 16) h4)])
       ;; m/2^52 is exact as a double, so this rounds once, as Chez does
       (* (* (exact->inexact m) 2.220446049250313e-16) x))]
    [(exact-positive-integer? x)
     (rkt:error 'random "bignum ranges are not supported: ~s" x)]
    [else (rkt:error 'random "invalid argument ~s" x)]))

(define random-seed
  (case-lambda
    [() rng-state]
    [(n)
     (unless (and (exact-integer? n) (<= 1 n #xFFFFFFFF))
       (rkt:error 'random-seed "invalid seed ~s" n))
     (set! rng-state n)
     (void)]))

;;;===========================================================================
;;; 2. One-armed if, and lists

;; Chez allows (if test then); its value is unspecified (void) when test is
;; false.  Racket's if requires both arms.
(define-syntax chez-if
  (syntax-rules ()
    [(_ test then) (if test then (void))]
    [(_ test then else) (if test then else)]))

;; Chez's case also accepts a single datum as a clause key, (case x (a e)),
;; meaning ((a) e); coderack.ss writes (rule-scout ...) and the like.
;; Keys are compared with eqv? in Chez and equal? in Racket; the model's
;; keys are symbols and numbers, for which the two agree.
(define-syntax (chez-case stx)
  (syntax-case stx ()
    [(_ e clause ...)
     (with-syntax ([(clause ...)
                    (map (lambda (c)
                           (syntax-case c ()
                             [(k body ...)
                              (and (not (syntax->list #'k))
                                   (not (and (identifier? #'k) (eq? (syntax-e #'k) 'else))))
                              #'((k) body ...)]
                             [_ c]))
                         (syntax->list #'(clause ...)))])
       #'(case e clause ...))]))

;; Chez's map applies the procedure in a fixed but unusual order (Chez
;; library entries map1/map2, s/library.ss): for one or two lists, pairs of
;; elements from the end of the list towards the front, e.g. 7 5 6 3 4 1 2
;; for seven elements; for three or more lists, last to first.  This matters
;; wherever the mapped procedure has side effects (random draws, posting
;; codelets).  Note: Chez's compiler (cp0) also inlines map over a literal
;; (list ...) or a short quoted list at the call site, in an order of its own;
;; such sites are checked one by one as the engine is ported.
(define chez-map
  (case-lambda
    [(f ls)
     (let map1 ([ls ls])
       (if (null? ls)
           '()
           (let ([r (cdr ls)])
             (if (null? r)
                 (list (f (car ls)))
                 (let* ([tail (map1 (cdr r))]
                        [a (f (car ls))]
                        [b (f (car r))])
                   (list* a b tail))))))]
    [(f ls1 ls2)
     (unless (= (length ls1) (length ls2))
       (rkt:error 'map "lists ~s and ~s differ in length" ls1 ls2))
     (let map2 ([ls1 ls1] [ls2 ls2])
       (if (null? ls1)
           '()
           (let ([r1 (cdr ls1)])
             (if (null? r1)
                 (list (f (car ls1) (car ls2)))
                 (let* ([r2 (cdr ls2)]
                        [tail (map2 (cdr r1) (cdr r2))]
                        [a (f (car ls1) (car ls2))]
                        [b (f (car r1) (car r2))])
                   (list* a b tail))))))]
    [(f ls . more)
     (let ([lss (cons ls more)])
       (unless (apply = (rkt:map length lss))
         (rkt:error 'map "lists ~s differ in length" lss))
       (let mapn ([lss lss])
         (if (null? (car lss))
             '()
             (let* ([tail (mapn (rkt:map cdr lss))])
               (cons (apply f (rkt:map car lss)) tail)))))]))

;; Chez's for-each returns the value of the last application (void for
;; empty lists); the original's for* loops pass that value on.
(define chez-for-each
  (case-lambda
    [(f ls)
     (if (null? ls)
         (void)
         (let loop ([ls ls])
           (if (null? (cdr ls))
               (f (car ls))
               (begin (f (car ls)) (loop (cdr ls))))))]
    [(f ls . more)
     (let ([lss (cons ls more)])
       (unless (apply = (rkt:map length lss))
         (rkt:error 'for-each "lists ~s differ in length" lss))
       (if (null? ls)
           (void)
           (let loop ([lss lss])
             (if (null? (cdr (car lss)))
                 (apply f (rkt:map car lss))
                 (begin (apply f (rkt:map car lss)) (loop (rkt:map cdr lss)))))))]))

;; Chez's sort: (sort pred list), stable.  This is Chez 10's algorithm
;; (s/5_6.ss), so that predicates which are not strict orders, and
;; predicates with side effects, behave as in the original: below 25
;; elements a top-down list merge sort, otherwise Olin Shivers's
;; opportunistic vector merge sort.
(define (sort elt< ls)
  (let ([n (length ls)])
    (if (< n 25)
        (if (<= n 1) ls (dolsort elt< ls n))
        (vector->list (dovsort! elt< (list->vector ls) n)))))

(define (dolsort elt< ls n)
  (cond
    [(= n 1) (list (car ls))]
    [(= n 2)
     (let ([x (car ls)] [y (cadr ls)])
       (if (elt< y x) (list y x) (list x y)))]
    [else
     ;; Chez sorts the second half first (seen in the predicate calls)
     (let* ([i (arithmetic-shift n -1)]
            [b (dolsort elt< (list-tail ls i) (- n i))]
            [a (dolsort elt< ls i)])
       (dolmerge elt< a b))]))

(define (dolmerge elt< ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [(null? ls2) ls1]
    [(elt< (car ls2) (car ls1))
     (cons (car ls2) (dolmerge elt< ls1 (cdr ls2)))]
    [else (cons (car ls1) (dolmerge elt< (cdr ls1) ls2))]))

(define (vmerge elt< target v1 v2 l len1 len2)
  ;; merge v1[l,l+len1-1] and v2[l+len1,l+len1+len2-1] into target[l,...]
  (let* ([r1 (+ l len1)] [r2 (+ r1 len2)])
    (let lp ([i l] [j l] [x (vector-ref v1 l)] [k r1] [y (vector-ref v2 r1)])
      (if (elt< y x)
          (let ([k (+ k 1)])
            (vector-set! target i y)
            (if (< k r2)
                (lp (+ i 1) j x k (vector-ref v2 k))
                (vblit v1 j target (+ i 1) r1)))
          (let ([j (+ j 1)])
            (vector-set! target i x)
            (if (< j r1)
                (lp (+ i 1) j (vector-ref v1 j) k y)
                (unless (eq? v2 target)
                  (vblit v2 k target (+ i 1) r2))))))))

(define (vblit fromv j tov i n)
  (let lp ([j j] [i i])
    (vector-set! tov i (vector-ref fromv j))
    (let ([j (+ j 1)])
      (unless (= j n) (lp j (+ i 1))))))

(define (getrun elt< v l r)
  (let lp ([i (+ l 1)] [x (vector-ref v l)])
    (if (= i r)
        (- i l)
        (let ([y (vector-ref v i)])
          (if (elt< y x) (- i l) (lp (+ i 1) y))))))

(define (dovsort! elt< v0 n)
  (let ([temp0 (make-vector n)])
    (define (recur l want)
      (let lp ([pfxlen (getrun elt< v0 l n)] [v v0] [temp temp0])
        (if (or (>= pfxlen want) (= pfxlen (- n l)))
            (values pfxlen v)
            (let-values ([(outlen outvec) (recur (+ l pfxlen) pfxlen)])
              (vmerge elt< temp v outvec l pfxlen outlen)
              (lp (+ pfxlen outlen) temp v)))))
    (let-values ([(outlen outvec) (recur 0 n)]) outvec)))

;; Chez's remq, remv and remove remove every occurrence (Racket's remove
;; only the first one).
(define (remq x ls)
  (cond [(null? ls) '()]
        [(eq? (car ls) x) (remq x (cdr ls))]
        [else (cons (car ls) (remq x (cdr ls)))]))
(define (remv x ls)
  (cond [(null? ls) '()]
        [(eqv? (car ls) x) (remv x (cdr ls))]
        [else (cons (car ls) (remv x (cdr ls)))]))
(define (remove x ls)
  (cond [(null? ls) '()]
        [(equal? (car ls) x) (remove x (cdr ls))]
        [else (cons (car ls) (remove x (cdr ls)))]))

(define (1+ x) (+ x 1))
(define (-1+ x) (- x 1))

;; tanh (workspace.ss's mapping strengths): racket/base has none, and
;; racket/math's is computed in Racket, so it may differ from Chez's in the
;; last bit.  Racket CS runs on Chez Scheme, so take Chez's own primitive.
(define tanh (vm-primitive 'tanh))

;;;===========================================================================
;;; 3. Top-level values
;;;
;;; The original creates global variables at run time from computed names
;;; (define-top-level-value in slipnet.ss's establish-link, the slipnet and
;;; codelet macros) and looks names up with eval (utilities.ss's
;;; symbol->letter-categories).  Racket modules have no such global
;;; environment, so these live in one table.

(define top-level-table (make-hasheq))

(define (define-top-level-value name value) (hash-set! top-level-table name value))
(define (set-top-level-value! name value)
  (unless (hash-has-key? top-level-table name)
    (rkt:error 'set-top-level-value! "variable ~s is not bound" name))
  (hash-set! top-level-table name value))
(define (top-level-value name)
  (hash-ref top-level-table name
            (lambda () (rkt:error 'top-level-value "variable ~s is not bound" name))))
(define (top-level-bound? name) (hash-has-key? top-level-table name))

;;;===========================================================================
;;; 4. Printing, as Chez Scheme 10 prints (s/print.ss)

;; Flonums: the shortest digits that read back (Racket and Chez agree on
;; these), laid out Chez's way: positional when the exponent e of the
;; leading digit satisfies -4 < e < 10, otherwise d.ddde<exp> with no `+';
;; subnormals get Chez's |<precision> suffix.
(define (flonum->chez-string x)
  (cond
    [(eqv? x +nan.0) "+nan.0"]
    [(eqv? x +inf.0) "+inf.0"]
    [(eqv? x -inf.0) "-inf.0"]
    [else
     (let*-values ([(neg?) (or (< x 0.0) (eqv? x -0.0))]
                   [(digits e) (flonum-digits (abs x))])
       (string-append
        (if neg? "-" "")
        (if (or (<= e -4) (>= e 10))
            (string-append (substring digits 0 1)
                           (if (> (string-length digits) 1)
                               (string-append "." (substring digits 1))
                               "")
                           "e" (rkt:number->string e))
            (positional digits e))
        (subnormal-suffix (abs x))))]))

;; digits: shortest digit string without leading/trailing zeros ("0" for
;; zero); e: exponent of the first digit, value = 0.d1d2... * 10^(e+1)
(define (flonum-digits x)
  (if (= x 0.0)
      (values "0" 0)
      (let* ([s (rkt:number->string x)]
             [epos (for/first ([c (in-string s)] [i (in-naturals)]
                               #:when (memv c '(#\e #\E)))
                     i)]
             [mant (if epos (substring s 0 epos) s)]
             [exp10 (if epos (string->number (substring s (+ epos 1))) 0)]
             [dot (or (for/first ([c (in-string mant)] [i (in-naturals)]
                                  #:when (char=? c #\.))
                        i)
                      (string-length mant))]
             [all (string-append (substring mant 0 dot)
                                 (if (< dot (string-length mant))
                                     (substring mant (+ dot 1))
                                     ""))]
             [lead (let loop ([i 0])
                     (if (char=? (string-ref all i) #\0) (loop (+ i 1)) i))]
             [trail (let loop ([i (string-length all)])
                      (if (char=? (string-ref all (- i 1)) #\0) (loop (- i 1)) i))])
        (values (substring all lead trail)
                (+ exp10 (- dot lead 1))))))

(define (positional digits e)
  (let ([n (string-length digits)])
    (if (< e 0)
        (string-append "0." (make-string (- (- e) 1) #\0) digits)
        (let ([int-len (+ e 1)])
          (if (>= int-len n)
              (string-append digits (make-string (- int-len n) #\0) ".0")
              (string-append (substring digits 0 int-len) "." (substring digits int-len)))))))

(define (subnormal-suffix ax)
  (if (and (> ax 0.0) (< ax 2.2250738585072014e-308))
      (let ([m (integer-length (* (inexact->exact ax) (expt 2 1074)))])
        (if (< 0 m 53) (string-append "|" (rkt:number->string m)) ""))
      ""))

(define (number->string z [radix 10])
  (cond
    [(not (= radix 10)) (rkt:number->string z radix)]
    [(flonum? z) (flonum->chez-string z)]
    [(and (complex? z) (not (real? z)))
     (let ([re (real-part z)] [im (imag-part z)])
       (if (and (inexact? re) (inexact? im))
           (string-append (flonum->chez-string re)
                          (if (or (< im 0.0) (eqv? im -0.0) (eqv? im +nan.0)
                                  (eqv? im +inf.0))
                              ""
                              "+")
                          (flonum->chez-string im) "i")
           (rkt:number->string z)))]
    [else (rkt:number->string z)]))

(define char-names
  '((#\nul . "nul") (#\u7 . "alarm") (#\backspace . "backspace") (#\tab . "tab")
    (#\newline . "newline") (#\vtab . "vtab") (#\page . "page") (#\return . "return")
    (#\u1B . "esc") (#\space . "space") (#\rubout . "delete")))

(define line-separators (list (integer->char #x85) (integer->char #x2028)))

(define (hex n) (string-upcase (rkt:number->string n 16)))

(define (write-char-chez c port)
  (write-string "#\\" port)
  (cond
    [(assv c char-names) => (lambda (p) (write-string (cdr p) port))]
    [(or (char<=? #\u21 c #\u7E) (and (char>=? c #\u80) (not (memv c line-separators))))
     (rkt:write-char c port)]
    [else (write-string "x" port) (write-string (hex (char->integer c)) port)]))

(define (write-string-chez s port)
  (rkt:write-char #\" port)
  (for ([c (in-string s)])
    (cond
      [(memv c '(#\" #\\)) (rkt:write-char #\\ port) (rkt:write-char c port)]
      [(or (char<=? #\space c #\~) (and (char>=? c #\u80) (not (memv c line-separators))))
       (rkt:write-char c port)]
      [(assv c '((#\u7 . #\a) (#\backspace . #\b) (#\newline . #\n) (#\page . #\f)
                 (#\return . #\r) (#\tab . #\t) (#\vtab . #\v)))
       => (lambda (p) (rkt:write-char #\\ port) (rkt:write-char (cdr p) port))]
      [else (write-string (string-append "\\x" (hex (char->integer c)) ";") port)]))
  (rkt:write-char #\" port))

(define (symbol-hex c port)
  (write-string (string-append "\\x" (hex (char->integer c)) ";") port))

(define (initial-char? c)
  (or (char-alphabetic-ascii? c) (memv c (string->list "*=<>/!$%&:?^_~"))))
(define (subsequent-char? c)
  (or (char-alphabetic-ascii? c) (char<=? #\0 c #\9)
      (memv c (string->list "-?*!=><$%&/:^_~+.@"))))
(define (char-alphabetic-ascii? c) (or (char<=? #\a c #\z) (char<=? #\A c #\Z)))

(define (write-symbol-chez sym port)
  (let* ([s (symbol->string sym)] [n (string-length s)])
    (if (= n 0)
        (write-string "||" port)
        (let ([c (string-ref s 0)])
          (cond
            [(initial-char? c) (rkt:write-char c port)]
            [(char=? c #\.)
             (if (string=? s "...") (rkt:write-char c port) (symbol-hex c port))]
            [(char=? c #\-)
             (if (or (= n 1) (char=? (string-ref s 1) #\>)) (rkt:write-char c port) (symbol-hex c port))]
            [(char=? c #\+) (if (= n 1) (rkt:write-char c port) (symbol-hex c port))]
            [(char>=? c #\u80) (rkt:write-char c port)]
            [else (symbol-hex c port)])
          (for ([c (in-string s 1)])
            (if (or (subsequent-char? c) (char>=? c #\u80))
                (rkt:write-char c port)
                (symbol-hex c port)))))))

(define abbreviations
  '((quote . "'") (quasiquote . "`") (unquote . ",") (unquote-splicing . ",@")
    (syntax . "#'") (quasisyntax . "#`") (unsyntax . "#,") (unsyntax-splicing . "#,@")))

;; Chez's display abbreviates (quote x) as 'x; its write does not.
(define (print-chez x port write?)
  (cond
    [(null? x) (write-string "()" port)]
    [(eq? x #t) (write-string "#t" port)]
    [(eq? x #f) (write-string "#f" port)]
    [(number? x) (write-string (number->string x) port)]
    [(symbol? x) (if write? (write-symbol-chez x port) (write-string (symbol->string x) port))]
    [(string? x) (if write? (write-string-chez x port) (write-string x port))]
    [(char? x) (if write? (write-char-chez x port) (rkt:write-char x port))]
    [(pair? x)
     (let ([abbrev (and (not write?) (symbol? (car x)) (pair? (cdr x)) (null? (cddr x))
                        (assq (car x) abbreviations))])
       (if abbrev
           (begin (write-string (cdr abbrev) port) (print-chez (cadr x) port write?))
           (begin
             (write-string "(" port)
             (print-chez (car x) port write?)
             (let loop ([rest (cdr x)])
               (cond
                 [(null? rest) (void)]
                 [(pair? rest) (write-string " " port) (print-chez (car rest) port write?) (loop (cdr rest))]
                 [else (write-string " . " port) (print-chez rest port write?)]))
             (write-string ")" port))))]
    [(vector? x)
     (write-string "#(" port)
     (for ([e (in-vector x)] [i (in-naturals)])
       (unless (= i 0) (write-string " " port))
       (print-chez e port write?))
     (write-string ")" port)]
    [(box? x) (write-string "#&" port) (print-chez (unbox x) port write?)]
    [(void? x) (write-string "#<void>" port)]
    [(eof-object? x) (write-string "#!eof" port)]
    ;; Chez also names anonymous procedures by source position; not reproduced
    [(procedure? x)
     (let ([name (object-name x)])
       (write-string (if name (rkt:format "#<procedure ~a>" name) "#<procedure>") port))]
    [else (rkt:display x port)]))

(define (display x [port (current-output-port)]) (print-chez x port #f))
(define (write x [port (current-output-port)]) (print-chez x port #t))

;; Chez format directives used by Metacat: ~a ~s ~% ~n ~~ (either case).
(define (format-to port control args)
  (let ([n (string-length control)])
    (let loop ([i 0] [args args])
      (cond
        [(= i n)
         (unless (null? args)
           (rkt:error 'format "too many arguments for control string ~s" control))]
        [(and (char=? (string-ref control i) #\~) (< (+ i 1) n))
         (let ([d (char-downcase (string-ref control (+ i 1)))])
           (case d
             [(#\a #\s)
              (when (null? args)
                (rkt:error 'format "too few arguments for control string ~s" control))
              (print-chez (car args) port (char=? d #\s))
              (loop (+ i 2) (cdr args))]
             [(#\% #\n) (rkt:newline port) (loop (+ i 2) args)]
             [(#\~) (rkt:write-char #\~ port) (loop (+ i 2) args)]
             [else (rkt:error 'format "unsupported directive ~~~a in ~s" d control)]))]
        [else (rkt:write-char (string-ref control i) port) (loop (+ i 1) args)]))))

;; (format string arg ...) returns a string, as do (format #f string ...);
;; (format #t string ...) and (format port string ...) print.
(define (format dest . rest)
  (cond
    [(string? dest)
     (let ([o (open-output-string)]) (format-to o dest rest) (get-output-string o))]
    [(eq? dest #f) (apply format rest)]
    [(eq? dest #t) (format-to (current-output-port) (car rest) (cdr rest))]
    [else (format-to dest (car rest) (cdr rest))]))

(define (fprintf port control . args) (format-to port control args))

;; syntactic-sugar.ss redefines printf and newline to write to the output
;; port current when it was loaded; here, the port current at the call.
(define (printf . args) (apply fprintf (current-output-port) args))
(define (newline) (fprintf (current-output-port) "~%"))

;; Chez's (error who message arg ...): who is a symbol, string or #f, and
;; message a format control string.
(define (error who msg . args)
  (let ([text (apply format msg args)])
    (raise (make-exn:fail (if who (format "~a: ~a" who text) text)
                          (current-continuation-marks)))))

;;;===========================================================================
;;; 5. record-case, reset, collect, real-time

;; (record-case exp (key formals body ...) ... (else body ...)): dispatch on
;; (car exp), binding formals (a list, an improper list or a symbol) to
;; (cdr exp).  key may be a symbol or a list of symbols.  As in Chez, the
;; formals are bound with car and cdr, not by applying a lambda: arguments
;; beyond the formals are ignored, and too few raise (car of ()).
;; trace.ss relies on this (porting-notes.md, item 13).
(define-syntax (record-case stx)
  (syntax-case stx ()
    [(_ e clause ...)
     (with-syntax ([(cl ...)
                    (for/list ([c (in-list (syntax->list #'(clause ...)))])
                      (syntax-case c (else)
                        [(else body ...) c]
                        [((key ...) formals body ...)
                         #'((key ...) (record-case-bind formals (cdr r) body ...))]
                        [(key formals body ...)
                         #'((key) (record-case-bind formals (cdr r) body ...))]))])
       #'(let ([r e]) (case (car r) cl ...)))]))

(define-syntax (record-case-bind stx)
  (syntax-case stx ()
    [(_ () args body ...) #'(let () body ...)]
    [(_ (x . more) args body ...)
     #'(let* ([a args] [x (car a)]) (record-case-bind more (cdr a) body ...))]
    [(_ x args body ...) (identifier? #'x) #'(let ([x args]) body ...)]))

;; Chez's (reset) calls the reset handler, which under the REPL abandons the
;; computation (and under --script exits).  Here the default handler raises
;; a metacat-reset value, which the run loop catches.
(struct metacat-reset ())
(define reset-handler (make-parameter (lambda () (raise (metacat-reset)))))
(define (reset) ((reset-handler)))

(define (collect . args) (void))
(define (real-time) (current-milliseconds))

;;;===========================================================================
;;; 6. syntactic-sugar.ss

;; random seeds cannot be bigger than this number in Chez Scheme
(define *largest-random-seed* 4294967295)

(define concatenate-symbols
  (lambda symbols
    (string->symbol (apply string-append (rkt:map symbol->string symbols)))))

;; The token validators are needed at expansion time too (mcat's fender).
(module tokens racket/base
  (provide valid-token-list? valid-number? symbol-or-valid-number?)
  (define *largest-random-seed* 4294967295)
  (define valid-token-list?
    (lambda (tokens)
      (and (list? tokens)
           (>= (length tokens) 3)
           (symbol? (car tokens))
           (symbol? (cadr tokens))
           (symbol? (caddr tokens))
           (or (= (length tokens) 3)
               (and (= (length tokens) 4)
                    (symbol-or-valid-number? (cadddr tokens)))
               (and (= (length tokens) 5)
                    (symbol? (cadddr tokens))
                    (valid-number? (car (cddddr tokens))))))))
  (define valid-number?
    (lambda (num)
      (and (integer? num)
           (> num 0)
           (<= num *largest-random-seed*))))
  (define symbol-or-valid-number?
    (lambda (token)
      (or (symbol? token) (valid-number? token)))))
(require 'tokens (for-syntax 'tokens))

;; set by define-codelet-procedure*'s codelets; a codelet calls (fizzle) to
;; give up.  Other modules cannot set! an imported variable, hence set-fizzle!.
(define fizzle #f)
(define (set-fizzle! v) (set! fizzle v))

;; ascending-index-list from utilities.ss, which for-each-vector-element*
;; uses; a private copy, so that this module does not depend on utilities.
(define %ascending-index-list
  (lambda (n)
    (letrec
      ((accumulate
         (lambda (i numbers)
           (if (zero? i)
             (cons i numbers)
             (accumulate (sub1 i) (cons i numbers))))))
      (accumulate (sub1 n) '()))))

;;; The macros.  extend-syntax keywords are matched by name.  Names that the
;;; original resolves in the global top level at the macro's use (tell,
;;; *coderack*, %verbose%, say-object, print, make-slipnode, establish-link,
;;; make-codelet-type, rotate-90-degrees-clockwise, the plato-... nodes) are
;;; given the lexical context of the macro keyword, so that they refer to the
;;; engine's bindings where the macro is used.

(begin-for-syntax
  (define (kw? id sym) (and (identifier? id) (eq? (syntax-e id) sym)))
  (define (kws? . pairs)
    (let loop ([p pairs])
      (or (null? p) (and (kw? (car p) (cadr p)) (loop (cddr p))))))
  ;; an identifier named sym in the context of the macro keyword of stx
  (define (free stx sym) (datum->syntax (car (syntax-e stx)) sym))
  (define (sym-append ctx . parts)
    (datum->syntax ctx
                   (string->symbol
                    (apply string-append
                           (map (lambda (p) (if (symbol? p) (symbol->string p)
                                                (symbol->string (syntax-e p))))
                                parts))))))

(define-syntax (mcat stx)
  (syntax-case stx ()
    [(_ token ...)
     (valid-token-list? (syntax->datum #'(token ...)))
     (with-syntax ([tell (free stx 'tell)] [*control-panel* (free stx '*control-panel*)])
       #'(tell *control-panel* 'run-new-problem '(token ...)))]))

(define-syntax (for* stx)
  (syntax-case stx ()
    [(_ each formal in exp do body ...)
     (and (kws? #'each 'each #'in 'in #'do 'do) (identifier? #'formal))
     #'(chez-for-each (lambda (formal) body ...) exp)]
    [(_ each (formal ...) in (exp ...) do body ...)
     (and (kws? #'each 'each #'in 'in #'do 'do)
          (andmap identifier? (syntax->list #'(formal ...))))
     #'(chez-for-each (lambda (formal ...) body ...) exp ...)]
    [(_ formal from exp1 to exp2 do body ...)
     (and (kws? #'from 'from #'to 'to #'do 'do) (identifier? #'formal))
     ;; exp1 before exp2, as in the oracle
     #'(chez-for-each (lambda (formal) body ...)
                      (let* ((exp1-value exp1)
                             (exp2-value exp2))
                   (if (< exp2-value exp1-value)
                       '()
                       (chez-map (lambda (n) (+ n exp1-value))
                                 (%ascending-index-list (add1 (- exp2-value exp1-value)))))))]))

(define-syntax (for-each-vector-element* stx)
  (syntax-case stx ()
    [(_ (v i) do exp ...)
     (kw? #'do 'do)
     #'(chez-for-each (lambda (i) exp ...) (%ascending-index-list (vector-length v)))]))

(define-syntax (for-each-table-element* stx)
  (syntax-case stx ()
    [(k (t i j) do exp ...)
     (kw? #'do 'do)
     #'(for-each-vector-element* (t i) do
         (chez-for-each (lambda (j) exp ...)
                   (%ascending-index-list (vector-length (vector-ref t i)))))]))

(define-syntax (repeat* stx)
  (syntax-case stx ()
    [(_ n times exp ...)
     (kw? #'times 'times)
     #'(let ((thunk (lambda () exp ...))
             (i n))
         (letrec ((loop (lambda () (if* (> i 0) (thunk) (set! i (sub1 i)) (loop)))))
           (loop)))]
    [(_ forever exp ...)
     (kw? #'forever 'forever)
     #'(let ((thunk (lambda () exp ...)))
         (letrec ((loop (lambda () (thunk) (loop))))
           (loop)))]
    [(_ until condition exp ...)
     (kw? #'until 'until)
     #'(let ((done? (lambda () condition))
             (thunk (lambda () exp ...)))
         (letrec ((loop (lambda () (if* (not (done?)) (thunk) (loop)))))
           (loop)))]))

(define-syntax-rule (if* test exp ...)
  (if test (begin exp ...) (void)))

;; one (random 1.0) draw, before prob is evaluated
(define-syntax-rule (stochastic-if* prob exp ...)
  (let ((prob-thunk (lambda () prob))
        (exps-thunk (lambda () exp ...)))
    (let ((coin-flip (random 1.0)))
      (if (< coin-flip (prob-thunk)) (exps-thunk) (void)))))

;; The original uses call/cc only through this macro, and only to escape
;; upwards (return from a codelet, fizzle, fail).  Escape continuations do
;; exactly that, and an escape attempted after the form has returned raises
;; an error instead of re-entering it.
(define-syntax-rule (continuation-point* name exp ...)
  (call/ec (lambda (name) exp ...)))

(define-syntax (say stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-syntax ([%verbose% (free stx '%verbose%)] [say-object (free stx 'say-object)])
       #'(if* %verbose% (say-object x) ... (newline)))]))

(define-syntax (say! stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-syntax ([say-object (free stx 'say-object)])
       #'(begin (say-object x) ... (newline)))]))

(define-syntax (vprintf stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-syntax ([%verbose% (free stx '%verbose%)])
       #'(if* %verbose% (printf x ...)))]))

(define-syntax (vprint stx)
  (syntax-case stx ()
    [(_ x ...)
     (with-syntax ([%verbose% (free stx '%verbose%)] [print (free stx 'print)])
       #'(if* %verbose% (print x ...)))]))

;;; Slipnet macros

;; (slipnet-node-list* (n s conceptual-depth: d) ...): makes each node, as
;; the top-level value n, and returns the list of nodes.
(define-syntax (slipnet-node-list* stx)
  (syntax-case stx ()
    [(_ (n s cd d) ...)
     (andmap (lambda (k) (kw? k 'conceptual-depth:)) (syntax->list #'(cd ...)))
     (with-syntax ([make-slipnode (free stx 'make-slipnode)])
       #'(begin
           (define-top-level-value 'n (make-slipnode 'n s d)) ...
           (list (top-level-value 'n) ...)))]))

;; The same at module level, where the original's top-level variables
;; become definitions: (define-slipnet-node-list* var (n s conceptual-depth: d) ...)
;; defines each n (also as a top-level value) and var as the list of nodes.
(define-syntax (define-slipnet-node-list* stx)
  (syntax-case stx ()
    [(_ var (n s cd d) ...)
     (andmap (lambda (k) (kw? k 'conceptual-depth:)) (syntax->list #'(cd ...)))
     (with-syntax ([make-slipnode (free stx 'make-slipnode)])
       #'(begin
           (define n (let ([node (make-slipnode 'n s d)])
                       (define-top-level-value 'n node)
                       node)) ...
           (define var (list n ...))))]))

(define-syntax (slipnet-layout-table* stx)
  (syntax-case stx ()
    [(_ (n ...) ...)
     (with-syntax ([rotate (free stx 'rotate-90-degrees-clockwise)])
       #'(rotate (vector (vector n ...) ...)))]))

;; (establish-link 'n1-n2-link plato-n1 plato-n2 type), then messages to the
;; new link, the top-level value n1-n2-link
(define-for-syntax (link-expansion stx n1 n2 type messages)
  (with-syntax ([establish-link (free stx 'establish-link)]
                [tell (free stx 'tell)]
                [name (sym-append n1 n1 '- n2 '-link)]
                [n1-name (sym-append n1 'plato- n1)]
                [n2-name (sym-append n2 'plato- n2)]
                [type type]
                [((msg arg) ...) messages])
    #'(begin (establish-link 'name n1-name n2-name 'type)
             (tell (top-level-value 'name) 'msg arg) ...)))

(define-for-syntax (plato-node stx n) (sym-append n 'plato- n))

(define-syntax (category-link* stx)
  (syntax-case stx ()
    [(_ n1 arrow n2 length: len)
     (kws? #'arrow '--> #'length: 'length:)
     (link-expansion stx #'n1 #'n2 'category (list (list #'set-link-length #'len)))]
    [(k (i ...) arrow c all-lengths: len)
     (kws? #'arrow '--> #'all-lengths: 'all-lengths:)
     (with-syntax ([length: (datum->syntax #'k 'length:)])
       #'(begin (k i arrow c length: len) ...))]))

(define-syntax (instance-link* stx)
  (syntax-case stx ()
    [(_ n1 arrow n2 length: len)
     (kws? #'arrow '--> #'length: 'length:)
     (link-expansion stx #'n1 #'n2 'instance (list (list #'set-link-length #'len)))]
    [(k c arrow (i ...) all-lengths: len)
     (kws? #'arrow '--> #'all-lengths: 'all-lengths:)
     (with-syntax ([length: (datum->syntax #'k 'length:)])
       #'(begin (k c arrow i length: len) ...))]))

(define-syntax (property-link* stx)
  (syntax-case stx ()
    [(_ n1 arrow n2 length: len)
     (kws? #'arrow '--> #'length: 'length:)
     (link-expansion stx #'n1 #'n2 'property (list (list #'set-link-length #'len)))]))

(define-for-syntax (lateral-expansion stx type)
  (syntax-case stx ()
    [(_ n1 arrow n2 length: len)
     (kws? #'arrow '--> #'length: 'length:)
     (link-expansion stx #'n1 #'n2 type (list (list #'set-link-length #'len)))]
    [(_ n1 arrow n2 label: n3)
     (kws? #'arrow '--> #'label: 'label:)
     (link-expansion stx #'n1 #'n2 type (list (list #'set-label-node (plato-node stx #'n3))))]
    [(_ n1 arrow n2 length: len label: n3)
     (and (eq? type 'lateral) (kws? #'arrow '--> #'length: 'length: #'label: 'label:))
     (link-expansion stx #'n1 #'n2 type (list (list #'set-link-length #'len)
                                              (list #'set-label-node (plato-node stx #'n3))))]
    [(k n1 arrow n2 x ...)
     (kw? #'arrow '<-->)
     (with-syntax ([--> (datum->syntax #'k '-->)])
       #'(begin (k n1 --> n2 x ...)
                (k n2 --> n1 x ...)))]))

(define-syntax (lateral-link* stx) (lateral-expansion stx 'lateral))
(define-syntax (lateral-sliplink* stx) (lateral-expansion stx 'lateral-sliplink))

;;; Codelet macros

(define-syntax (codelet-type-list* stx)
  (syntax-case stx ()
    [(_ (name label ...) ...)
     (with-syntax ([make-codelet-type (free stx 'make-codelet-type)])
       #'(begin
           (define-top-level-value 'name (make-codelet-type 'name (list label ...))) ...
           (list (top-level-value 'name) ...)))]))

;; module-level form: (define-codelet-type-list* var (name label ...) ...)
(define-syntax (define-codelet-type-list* stx)
  (syntax-case stx ()
    [(_ var (name label ...) ...)
     (with-syntax ([make-codelet-type (free stx 'make-codelet-type)])
       #'(begin
           (define name (let ([type (make-codelet-type 'name (list label ...))])
                          (define-top-level-value 'name type)
                          type)) ...
           (define var (list name ...))))]))

(define-syntax (post-codelet* stx)
  (syntax-case stx ()
    [(_ urgency: rel-urg codelet-type arg ...)
     (kw? #'urgency: 'urgency:)
     (with-syntax ([tell (free stx 'tell)] [*coderack* (free stx '*coderack*)])
       #'(tell *coderack* 'post (tell codelet-type 'make-codelet rel-urg arg ...)))]))

(define-syntax (define-codelet-procedure* stx)
  (syntax-case stx ()
    [(_ codelet-type (lam formals exp ...))
     (kw? #'lam 'lambda)
     (with-syntax ([tell (free stx 'tell)] [say (free stx 'say)])
       #'(tell codelet-type 'set-codelet-procedure
           (lambda formals
             (continuation-point* return
               (set-fizzle! (lambda () (set-fizzle! #f) (return 'done)))
               (say "----------------------------------------------")
               (say "In " 'codelet-type " codelet...")
               exp ...))))]))

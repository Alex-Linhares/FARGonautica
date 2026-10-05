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

;; random seeds cannot be bigger than this number in Chez Scheme
(define *largest-random-seed* 4294967295)

(define concatenate-symbols
  (lambda symbols
    (string->symbol (apply string-append (map symbol->string symbols)))))

(define printf
  (let ((out (current-output-port)))
    (lambda args
      (apply fprintf (cons out args)))))

(define newline
  (let ((out (current-output-port)))
    (lambda ()
      (fprintf out "~%"))))

;;----------------------------------------------------------------------------------
;; Utility Macros

(extend-syntax (mcat)
  ((mcat token ...)
   (valid-token-list? '(token ...))
   (tell *control-panel* 'run-new-problem '(token ...))))

;; valid token list formats:
;; (sym sym sym)
;; (sym sym sym sym)
;; (sym sym sym num)
;; (sym sym sym sym num)

(define valid-token-list?
  (lambda (tokens)
    (and (list? tokens)
	 (>= (length tokens) 3)
	 (symbol? (1st tokens))
	 (symbol? (2nd tokens))
	 (symbol? (3rd tokens))
	 (or (= (length tokens) 3)
	     (and (= (length tokens) 4)
		  (symbol-or-valid-number? (4th tokens)))
	     (and (= (length tokens) 5)
		  (symbol? (4th tokens))
		  (valid-number? (5th tokens)))))))

(define valid-number?
  (lambda (num)
    (and (integer? num)
         (> num 0)
	 (<= num *largest-random-seed*))))

(define symbol-or-valid-number?
  (lambda (token)
    (or (symbol? token) (valid-number? token))))

(extend-syntax (for* each in from to do)
  ((for* each formal in exp do body ...)
   (symbol? 'formal)
   (for-each (lambda (formal) body ...) exp))
  ((for* each (formal ...) in (exp ...) do body ...)
   (andmap symbol? '(formal ...))
   (for-each (lambda (formal ...) body ...) exp ...))
  ((for* formal from exp1 to exp2 do body ...)
   (for* each formal in
	 (let ((exp1-value exp1)
	       (exp2-value exp2))
	   (if (< exp2-value exp1-value)
	       '()
	       (map (lambda (n) (+ n exp1-value))
		    (ascending-index-list (add1 (- exp2-value exp1-value)))))) do
	 body ...)))

(extend-syntax (for-each-vector-element* do)
  ((for-each-vector-element* (v i) do exp ...)
   (for* each i in (ascending-index-list (vector-length v)) do exp ...)))

(extend-syntax (for-each-table-element* do)
  ((for-each-table-element* (t i j) do exp ...)
   (for-each-vector-element* (t i) do
     (for* each j in (ascending-index-list (vector-length (vector-ref t i))) do
       exp ...))))

(extend-syntax (repeat* times forever until)
  ((repeat* n times exp ...)
   (let ((thunk (lambda () exp ...))
	 (i n))
     (letrec ((loop (lambda () (if* (> i 0) (thunk) (set! i (sub1 i)) (loop)))))
       (loop))))
  ((repeat* forever exp ...)
   (let ((thunk (lambda () exp ...)))
     (letrec ((loop (lambda () (thunk) (loop))))
       (loop))))
  ((repeat* until condition exp ...)
   (let ((done? (lambda () condition))
	 (thunk (lambda () exp ...)))
     (letrec ((loop (lambda () (if* (not (done?)) (thunk) (loop)))))
       (loop)))))

(extend-syntax (if*)
  ((if* test exp ...) (if test (begin exp ...) (void))))

(extend-syntax (stochastic-if*)
  ((stochastic-if* prob exp ...)
   (let ((prob-thunk (lambda () prob))
	 (exps-thunk (lambda () exp ...)))
     (let ((coin-flip (random 1.0)))
       (if (< coin-flip (prob-thunk)) (exps-thunk) (void))))))

(extend-syntax (continuation-point*)
  ((continuation-point* name exp ...) (call/cc (lambda (name) exp ...))))

(extend-syntax (say)
  ((say x ...) (if* %verbose% (say-object x) ... (newline))))

(extend-syntax (say!)
  ((say! x ...) (begin (say-object x) ... (newline))))

(extend-syntax (vprintf)
  ((vprintf x ...) (if* %verbose% (printf x ...))))

(extend-syntax (vprint)
  ((vprint x ...) (if* %verbose% (print x ...))))

;;----------------------------------------------------------------------------------
;; Slipnet Macros

(extend-syntax (slipnet-node-list* conceptual-depth:)
  ((slipnet-node-list* (n s conceptual-depth: d) ...)
   (begin
     (define-top-level-value 'n (make-slipnode 'n s d)) ...
     (list n ...))))

(extend-syntax (slipnet-layout-table*)
  ((slipnet-layout-table* (n ...) ...)
   (rotate-90-degrees-clockwise (vector (vector n ...) ...))))

(extend-syntax (category-link* --> length: all-lengths:)
  ((category-link* n1 --> n2 length: len)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2)))
       (begin (establish-link 'top-level-name n1-name n2-name 'category)
	      (tell top-level-name 'set-link-length len))))
  ((category-link* (i ...) --> c all-lengths: len)
     (begin (category-link* i --> c length: len) ...)))

(extend-syntax (instance-link* --> length: all-lengths:)
  ((instance-link* n1 --> n2 length: len)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2)))
       (begin (establish-link 'top-level-name n1-name n2-name 'instance)
	      (tell top-level-name 'set-link-length len))))
  ((instance-link* c --> (i ...) all-lengths: len)
     (begin (instance-link* c --> i length: len) ...)))

(extend-syntax (property-link* --> length:)
  ((property-link* n1 --> n2 length: len)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2)))
       (begin (establish-link 'top-level-name n1-name n2-name 'property)
	      (tell top-level-name 'set-link-length len)))))

(extend-syntax (lateral-link* --> <--> label: length:)
  ((lateral-link* n1 --> n2 length: len)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2)))
       (begin (establish-link 'top-level-name n1-name n2-name 'lateral)
	      (tell top-level-name 'set-link-length len))))
  ((lateral-link* n1 --> n2 label: n3)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2))
	    (n3-name (concatenate-symbols 'plato- 'n3)))
       (begin (establish-link 'top-level-name n1-name n2-name 'lateral)
	      (tell top-level-name 'set-label-node n3-name))))
  ((lateral-link* n1 --> n2 length: len label: n3)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2))
	    (n3-name (concatenate-symbols 'plato- 'n3)))
       (begin (establish-link 'top-level-name n1-name n2-name 'lateral)
	      (tell top-level-name 'set-link-length len)
	      (tell top-level-name 'set-label-node n3-name))))
  ((lateral-link* n1 <--> n2 x ...)
     (begin
       (lateral-link* n1 --> n2 x ...)
       (lateral-link* n2 --> n1 x ...))))

(extend-syntax (lateral-sliplink* --> <--> label: length:)
  ((lateral-sliplink* n1 --> n2 label: n3)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2))
	    (n3-name (concatenate-symbols 'plato- 'n3)))
       (begin (establish-link 'top-level-name n1-name n2-name 'lateral-sliplink)
	      (tell top-level-name 'set-label-node n3-name))))
  ((lateral-sliplink* n1 --> n2 length: len)
     (with ((top-level-name (concatenate-symbols 'n1 '- 'n2 '-link))
	    (n1-name (concatenate-symbols 'plato- 'n1))
	    (n2-name (concatenate-symbols 'plato- 'n2)))
       (begin (establish-link 'top-level-name n1-name n2-name 'lateral-sliplink)
	      (tell top-level-name 'set-link-length len))))
  ((lateral-sliplink* n1 <--> n2 x ...)
     (begin
       (lateral-sliplink* n1 --> n2 x ...)
       (lateral-sliplink* n2 --> n1 x ...))))

;;----------------------------------------------------------------------------------
;; Codelet Macros

(extend-syntax (codelet-type-list*)
  ((codelet-type-list* (name label ...) ...)
   (begin
     (define-top-level-value 'name (make-codelet-type 'name (list label ...))) ...
     (list name ...))))

(extend-syntax (post-codelet* urgency:)
  ((post-codelet* urgency: rel-urg codelet-type arg ...)
   (tell *coderack* 'post (tell codelet-type 'make-codelet rel-urg arg ...))))

(define fizzle #f)

(extend-syntax (define-codelet-procedure*)
  ((define-codelet-procedure* codelet-type (lambda formals exp ...))
   (tell codelet-type 'set-codelet-procedure
     (lambda formals
       (continuation-point* return
	 (set! fizzle (lambda () (set! fizzle #f) (return 'done)))
	 (say "----------------------------------------------")
	 (say "In " 'codelet-type " codelet...")
	 exp ...)))))

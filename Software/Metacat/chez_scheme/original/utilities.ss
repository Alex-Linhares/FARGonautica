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

;; for debugging:
;;(define *object* #f)
;;(define *message* #f)
;;(define *last-object* #f)
;;(define *last-message* #f)
;;
;;(define where?
;;  (lambda ()
;;    (set! *object* *last-object*)
;;    (set! *message* *last-message*)
;;    (printf "Last method was ~a method \"~a\"~%"
;;      (tell *object* 'object-type)    
;;      (1st *message*))
;;    (printf "(Check *object* and *message*). Good luck!~%")))

(define compose
  (lambda (f . l)
    (if (null? l)
	f
	(let ((g (apply compose l)))
	  (lambda (x) (f (g x)))))))

(define base-object
  (lambda msg
    (record-case (rest msg)
      (object-type () 'base-object)
      (else 'invalid-message-indicator))))

(define tell
  (lambda args
    (let ((object (1st args))
	  (message args))
;; for debugging:
;;      (set! *last-object* object)
;;      (set! *last-message* (rest message))
      (let ((result (apply object message)))
	(if (eq? result 'invalid-message-indicator)
	  (report-error-and-halt message object)
	  result)))))

(define report-error-and-halt
  (lambda (message object)
    (printf "Ooops: bad message \"~a\" sent to object of type ~a~%"
      (2nd message)
      (tell object 'object-type))
    (reset)))

(define tell-all
  (lambda (objects . message)
    (map (lambda (x) (apply tell (cons x message))) objects)))

(define delegate
  (lambda args
    (let ((message (1st args))
	  (objects (rest args)))
      (continuation-point* return-immediately
	(for* each object in objects do
	  (let ((result (apply object message)))
	    (if* (not (eq? result 'invalid-message-indicator))
	      (return-immediately result))))
	'invalid-message-indicator))))

(define delegate-to-all
  (lambda args
    (let ((message (rest (1st args)))
	  (objects (rest args)))
      (continuation-point* return-immediately
	(map (lambda (object)
	       (let ((result (apply object (cons object message))))
		 (if (eq? result 'invalid-message-indicator)
		   (return-immediately 'invalid-message-indicator)
		   result)))
	  objects)))))

(define print
  (lambda l
    (for* each obj in (flatten l) do
      (if (procedure? obj)
	(tell obj 'print)
	(printf "~a~%" obj)))))

(define say-object
  (lambda (x)
    (printf "~a" (if (procedure? x) (tell x 'print-name) x))))

(define ask
  (lambda l
    (for-each display l)
    (clear-input-port)
    (let ((first-char (read-char)))
      (cond
       ((char=? first-char #\newline) 'nothing)
       (else (unread-char first-char) (read))))))

(define type-tester
  (lambda (type)
    (lambda (object)
      (eq? (tell object 'object-type) type))))

(define bond? (type-tester 'bond))
(define letter? (type-tester 'letter))
(define group? (type-tester 'group))
(define bridge? (type-tester 'bridge))
(define concept-mapping? (type-tester 'concept-mapping))
(define rule? (type-tester 'rule))
(define description? (type-tester 'description))
(define workspace-string? (type-tester 'workspace-string))
(define workspace? (type-tester 'workspace))
(define answer-description? (type-tester 'answer-description))
(define snag-description? (type-tester 'snag-description))

(define event?
  (lambda (object)
    (member (tell object 'object-type)
      '(generic-event answer-event clamp-event concept-activation-event
	 group-event rule-event concept-mapping-event snag-event))))

(define slipnode?
  (lambda (object)
    (and (procedure? object) (eq? (tell object 'object-type) 'slipnode))))

(define vertical-bridge?
  (lambda (object)
    (and (bridge? object) (eq? (tell object 'get-orientation) 'vertical))))

(define horizontal-bridge?
  (lambda (object)
    (and (bridge? object) (eq? (tell object 'get-orientation) 'horizontal))))

(define exists?
  (lambda (x) (if x #t #f)))

(define all-exist?
  (lambda (l) (andmap exists? l)))

(define all-same?
  (lambda (l)
    (or (null? l)
        (andmap (lambda (x) (eq? x (1st l))) l))))

(define compress
  (lambda (l) (filter exists? l)))

(define map-compress
  (lambda (proc l) (map-filter proc exists? l)))

(define flatmap
  (lambda (proc l)
    (apply append (map proc l))))

(define scheme-truncate truncate)
(define truncate
  (lambda (n) (inexact->exact (scheme-truncate n))))

(define scheme-ceiling ceiling)
(define ceiling
  (lambda (n) (inexact->exact (scheme-ceiling n))))

(define scheme-floor floor)
(define floor
  (lambda (n) (inexact->exact (scheme-floor n))))

(define scheme-round round)
(define round
  (lambda (n) (inexact->exact (scheme-round n))))

(define round-to-10ths
  (lambda (n)
    (exact->inexact (/ (round (* n 10)) 10))))

(define round-to-100ths
  (lambda (n)
    (exact->inexact (/ (round (* n 100)) 100))))

(define round-to-1000ths
  (lambda (n)
    (exact->inexact (/ (round (* n 1000)) 1000))))

(define ^2 (lambda (x) (* x x)))

(define ^3 (lambda (x) (* x x x)))

(define sort-wrt-order
  (lambda (l order)
    (sort (lambda (v1 v2)
	    (< (list-index order v1)
	       (list-index order v2)))
      l)))

(define sort-by-method
  (lambda (method-name pred? l)
    (sort (lambda (v1 v2)
	    (pred? (tell v1 method-name) (tell v2 method-name)))
      l)))

;; ascending-index-list returns 0-based indices

(define ascending-index-list
  (lambda (n)
    (letrec
      ((accumulate
	 (lambda (i numbers)
	   (if (zero? i)
	     (cons i numbers)
	     (accumulate (sub1 i) (cons i numbers))))))
      (accumulate (sub1 n) '()))))

(define descending-index-list
  (lambda (n)
    (letrec
      ((accumulate
	 (lambda (i numbers)
	   (if (= i n)
	     numbers
	     (accumulate (add1 i) (cons i numbers))))))
      (accumulate 0 '()))))

(define symbol->letter-categories
  (lambda (sym)
    (map (compose
	   eval
	   string->symbol
	   (lambda (s) (string-append "plato-" s))
	   string)
	 (string->list (symbol->string sym)))))

(define char-index
  (lambda (char string)
    (let loop ((i 0))
      (cond
	((= i (string-length string)) -1)
	((char=? (string-ref string i) char) i)
	(else (loop (+ i 1)))))))

(define string-upcase
  (lambda (s)
    (list->string (map char-upcase (string->list s)))))

(define string-downcase
  (lambda (s)
    (list->string (map char-downcase (string->list s)))))

(define capitalize-string
  (lambda (s)
    (if (string=? s "")
      ""
      (string-append
	(string (char-upcase (string-ref s 0)))
	(substring s 1 (string-length s))))))

(define quoted-string
  (lambda (string)
    (format "\"~a\"" (tell string 'print-name))))

(define make-table
  (lambda (row-dim column-dim . optional-args)
    (let ((initial-value (if (null? optional-args) #f (1st optional-args)))
	  (table (make-vector row-dim)))
      (for-each-vector-element* (table i) do
	(vector-set! table i (make-vector column-dim initial-value)))
      table)))

(define row-dimension vector-length)

(define column-dimension
  (lambda (table)
    (vector-length (vector-ref table 0))))

(define table->list
  (lambda (table)
    (apply append (map vector->list (vector->list table)))))

(define table-set!
  (lambda (table i j value)
    (vector-set! (vector-ref table i) j value)))

(define table-ref
  (lambda (table i j)
    (vector-ref (vector-ref table i) j)))

(define vector-increment!
  (lambda (v i value)
    (vector-set! v i (+ value (vector-ref v i)))))

(define initialize-row!
  (lambda (table i value)
    (vector-fill! (vector-ref table i) value)))

(define initialize-column!
  (lambda (table j value)
    (for-each-vector-element* (table i) do
      (table-set! table i j value))))

(define get-row
  (lambda (table i)
    (vector->list (vector-ref table i))))

(define get-column
  (lambda (table j)
    (map (lambda (row) (vector-ref row j)) (vector->list table))))

(define copy-vector-contents!
  (lambda (source-vector target-vector)
    (for-each-vector-element* (source-vector i) do
      (vector-set! target-vector i (vector-ref source-vector i)))))

(define copy-table-contents!
  (lambda (source-table target-table)
    (for-each-table-element* (source-table i j) do
      (table-set! target-table i j (table-ref source-table i j)))))

(define add-scaled-vector!
  (lambda (target source scalar)
    (for-each-vector-element* (target i) do
      (vector-set! target i
	(+ (vector-ref target i) (* scalar (vector-ref source i)))))))

(define rotate-90-degrees-clockwise
  (lambda (old-table)
    (let* ((row-dim (row-dimension old-table))
	   (column-dim (column-dimension old-table))
	   (new-table (make-table column-dim row-dim)))
      (for-each-table-element* (old-table i j) do
        (table-set! new-table j (sub1 (- row-dim i)) (table-ref old-table i j)))
      new-table)))

(define sum
  (lambda (l)
    (apply + l)))

(define product
  (lambda (l)
    (apply * l)))

(define average
  (lambda l
    (cond
      ((null? (1st l)) 0)
      ((and (null? (rest l)) (list? (1st l)))
       (/ (sum (1st l)) (length (1st l))))
      (else (/ (sum l) (length l))))))

(define weighted-average
  (lambda (values weights)
    (if (zero? (sum weights))
      0
      (/ (sum (map * weights values)) (sum weights)))))

(define log10
  (let ((ln10 (log 10)))
    (lambda (x)
      (let ((result (/ (log x) ln10)))
	(+ result (* (sgn result) 1E-15))))))

(define sgn
  (lambda (x)
    (if (negative? x) -1 1)))

(define list-index
  (lambda (l x)
    (if (eq? (1st l) x)
	0
	(add1 (list-index (rest l) x)))))

(define cd
  (lambda (x)
    (tell x 'get-conceptual-depth)))

(define randomize
  (let ((upper-bound (expt 2 32)))
    (lambda ()
      (random-seed (modulo (round (expt (real-time) 5/3)) upper-bound)))))

(define prob?
  (lambda (p)
    (cond
     ((<= p 0.0) #f)
     ((>= p 1.0) #t)
     (else (> p (random 1.0))))))
	
(define ~
  (lambda (n)
    (let ((delta (random (add1 (round (sqrt n))))))
      (if (prob? 0.5) (+ n delta) (- n delta)))))

;; random-pick picks an object at random from a list

(define random-pick
  (lambda (l)
    (if (null? l)
      #f
      (nth (random (length l)) l))))

;; l is a list of N objects.  weights is a list of
;; N integer/real numbers.  Stochastic-pick picks one of
;; the N objects probabilistically, biased by the weights.

(define stochastic-pick
  (lambda (l weights)
    (let ((weight-sum (sum weights)))
      (if (zero? weight-sum)
	  (random-pick l)
	  (nth (weighted-index (random (exact->inexact weight-sum)) weights) l)))))

;; stochastic-pick-by-method tells message to each object in
;; object-list, yielding a list of integer/real numbers.  It
;; then performs a stochastic-pick using these numbers as weights.

(define stochastic-pick-by-method
  (lambda (object-list . message)
    (let ((weights (apply tell-all (cons object-list message))))
      (stochastic-pick object-list weights))))

;; weighted-index example:
;; weights is a list of 6 real numbers.  w is a real number.
;; weights = |.....6.....|.2.|...4...|......7.....|.2.|...4...|
;;           .................. w = 17 ........^
;; weighted-index returns 3 (the index of the 4th weight)

(define weighted-index
  (lambda (w weights)
    (if (< w (1st weights))
	0
	(add1 (weighted-index (- w (1st weights)) (rest weights))))))

;; selection-list is a list of lists:  ((<weight> <obj> ...) ... )
;; stochastic-select picks an element stochastically, biased by the weights.
;; weighted-select picks an element based on a value "w" as in weighted-index.

(define stochastic-select
  (lambda (selection-list)
    (let ((weight-sum (sum (map 1st selection-list))))
      (if (zero? weight-sum)
	  (random-pick selection-list)
	  (weighted-select (random (exact->inexact weight-sum)) selection-list)))))

(define weighted-select
  (lambda (w selection-list)
    (let ((first-element (1st selection-list)))
      (if (< w (1st first-element))
	  first-element
	  (weighted-select (- w (1st first-element)) (rest selection-list))))))

(define stochastic-filter
  (lambda (proc l)
    (cond
      ((null? l) '())
      ((prob? (proc (1st l))) (cons (1st l) (stochastic-filter proc (rest l))))
      (else (stochastic-filter proc (rest l))))))

(define 100- (lambda (n) (- 100 n)))

(define 10- (lambda (x) (- 10 x)))

(define 1- (lambda (x) (- 1 x)))

(define 100* (lambda (x) (round (* 100 x))))

(define % (lambda (n) (/ n 100)))
(define 20% (lambda (n) (* 1/5 n)))
(define 40% (lambda (n) (* 2/5 n)))
(define 80% (lambda (n) (* 4/5 n)))

;; Returns a sigmoid function f defined on the interval 0..100 with range 0..1,
;; where f(m) = 0.5 and beta specifies the steepness of the curve at m:
;;
;; Maple plot code for (sigmoid beta m):  plot(1/(1+exp(beta*(m-x)/25)),x=0..100);

(define sigmoid
  (lambda (beta m)
    (lambda (x)
      (/ 1 (+ 1 (exp (* 1/25 beta (- m x))))))))

(define clip-function
  (lambda (lower-bound upper-bound)
    (lambda (x)
      (max lower-bound (min x upper-bound)))))

(define flatten
  (lambda (l)
    (letrec
      ((walk (lambda (l leaves)
	       (cond
		 ((null? l) leaves)
		 ((list? (1st l)) (walk (1st l) (walk (rest l) leaves)))
		 (else (cons (1st l) (walk (rest l) leaves)))))))
      (walk l '()))))

(define select-longest-list
  (lambda (l)
    (let ((result (select-extreme max length l)))
      (if (exists? result)
	result
	'()))))

(define select-extreme
  (lambda (min/max proc l)
    (if (null? l)
      #f
      (let ((values (map proc l)))
	(2nd (assv (apply min/max values) (map list values l)))))))

(define maximum
  (lambda (l)
    (if (null? l) 0 (apply max l))))

(define minimum
  (lambda (l)
    (if (null? l) 0 (apply min l))))

(define count
  (lambda (predicate? l)
    (cond
      ((null? l) 0)
      ((predicate? (1st l)) (add1 (count predicate? (rest l))))
      (else (count predicate? (rest l))))))

(define adjacency-map
  (lambda (f l)
    (map f (all-but-last 1 l) (rest l))))

;; Letrec is necessary here because select already exists in SWL
;; as a macro.  Without the letrec, (select predicate? (rest l))
;; is treated as an SWL macro call, not a recursive function call.
(define select
  (letrec
    ((select
       (lambda (predicate? l)
	 (cond
	   ((null? l) #f)
	   ((predicate? (1st l)) (1st l))
	   (else (select predicate? (rest l)))))))
    select))

;; filter retains exactly the elements of l which satisfy pred?

(define filter
  (lambda (pred? l)
    (cond
     ((null? l) '())
     ((pred? (1st l)) (cons (1st l) (filter pred? (rest l))))
     (else (filter pred? (rest l))))))

;; filter-out removes exactly the elements of l which satisfy pred?

(define filter-out
  (lambda (pred? l)
    (filter (compose not pred?) l)))


(define map-leaves
  (lambda (proc l)
    (cond
      ((null? l) '())
      ((pair? (1st l)) (cons (map-leaves proc (1st l)) (map-leaves proc (rest l))))
      (else (cons (proc (1st l)) (map-leaves proc (rest l)))))))

;; filter-map first filters l according to pred?, then maps
;; proc to the elements of l that remain.

(define filter-map
  (lambda (pred? proc l)
    (cond
      ((null? l) '())
      ((pred? (1st l)) (cons (proc (1st l)) (filter-map pred? proc (rest l))))
      (else (filter-map pred? proc (rest l))))))

;; map-filter first maps proc to the elements of l, then filters
;; the resulting elements according to pred?

(define map-filter
  (lambda (proc pred? l)
    (if (null? l)
      '()
      (let ((value (proc (1st l))))
	(if (pred? value)
	  (cons value (map-filter proc pred? (rest l)))
	  (map-filter proc pred? (rest l)))))))

;; cross-product procedures take 2-ary pred? and proc procedures as arguments,
;; which get applied to the pairs of the cross-product l1 x l2.
;;
;; cross-product-filter-map first filters l1 x l2 according to pred?,
;; then applies proc to the elements of l1 x l2 that remain.
;;
;; cross-product-map-filter performs the mapping first, then filters the result.
;; NOTE: pred? must be a unary procedure in this case, expecting values of
;; the type returned by proc.

(define cross-product
  (lambda (l1 l2)
    (cross-product-filter-map (lambda (x y) #t) list l1 l2)))

(define cross-product-filter
  (lambda (pred? l1 l2)
    (cross-product-filter-map pred? list l1 l2)))

(define cross-product-map
  (lambda (proc l1 l2)
    (cross-product-filter-map (lambda (x y) #t) proc l1 l2)))

(define cross-product-filter-map
  (lambda (pred? proc l1 l2)
    (letrec
      ((f (lambda (l result)
	    (if (null? l)
	      result
	      (g (1st l) l2 (f (rest l) result)))))
       (g (lambda (x l result)
	    (cond
	      ((null? l) result)
	      ((pred? x (1st l)) (cons (proc x (1st l)) (g x (rest l) result)))
	      (else (g x (rest l) result))))))
      (f l1 '()))))

(define cross-product-map-filter
  (lambda (proc pred? l1 l2)
    (letrec
      ((f (lambda (l result)
	    (if (null? l)
	      result
	      (g (1st l) l2 (f (rest l) result)))))
       (g (lambda (x l result)
	    (if (null? l)
	      result
	      (let ((value (proc x (1st l))))
		(if (pred? value)
		  (cons value (g x (rest l) result))
		  (g x (rest l) result)))))))
      (f l1 '()))))

(define cross-product-ormap
  (lambda (pred? l1 l2)
    (ormap (lambda (x)
	     (ormap (lambda (y) (pred? x y))
	       l2))
      l1)))

(define cross-product-andmap
  (lambda (pred? l1 l2)
    (andmap (lambda (x)
	      (andmap (lambda (y) (pred? x y))
		l2))
      l1)))

(define cross-product-for-each
  (lambda (proc l1 l2)
    (cross-product-ormap
      (lambda (x y) (proc x y) #f)
      l1 l2)))

;; The following procedures allow filtering, etc., of a list by specifying
;; a method (possibly with arguments), rather than a predicate function.
;; Example: (filter-meth l 'predicate-method-name? arg1 arg2 ...)

(define make-list-method-procedure
  (lambda (proc)
    (lambda (l . message)
      (proc (lambda (x) (apply tell (cons x message))) l))))

(define select-meth (make-list-method-procedure select))
(define filter-meth (make-list-method-procedure filter))
(define filter-out-meth (make-list-method-procedure filter-out))
(define andmap-meth (make-list-method-procedure andmap))
(define ormap-meth (make-list-method-procedure ormap))
(define count-meth (make-list-method-procedure count))

;; pairwise-map maps proc to each unique, non-reflexive pair of elements in l
;; Example: (pairwise-map list '(1 2 3)) ==> ((1 2) (1 3) (2 3))

(define pairwise-map
  (lambda (proc l)
    (letrec
      ((pairwise-map
	 (lambda (l)
	   (if (null? l)
	     '()
	     (append
	       (map (lambda (x) (proc (1st l) x)) (rest l))
	       (pairwise-map (rest l)))))))
      (pairwise-map l))))

(define pairwise-do
  (lambda (proc l)
    (letrec
      ((pairwise-do
	 (lambda (l)
	   (if* (not (null? l))
	     (for* each obj in (rest l) do (proc (1st l) obj))
	     (pairwise-do (rest l))))))
      (pairwise-do l)
      'done)))

(define pairwise-andmap
  (lambda (pred? l)
    (letrec
      ((pairwise-andmap
	 (lambda (l)
	   (or (null? l)
	       (and (andmap (lambda (x) (pred? (1st l) x)) (rest l))
		    (pairwise-andmap (rest l)))))))
      (pairwise-andmap l))))

(define intersect
  (lambda (l1 l2)
    (intersect-pred eq? l1 l2)))

(define intersect-pred
  (lambda (equiv-pred? l1 l2)
    (cross-product-filter-map equiv-pred? (lambda (x y) x) l1 l2)))

(define intersect-all
  (lambda (l)
    (intersect-all-pred eq? l)))

(define intersect-all-pred
  (lambda (equiv-pred? l)
    (cond
     ((null? l) '())
     ((null? (rest l)) (1st l))
     (else (intersect-pred equiv-pred?
	     (1st l) (intersect-all-pred equiv-pred? (rest l)))))))

(define partition
  (lambda (pred? l)
    (letrec ((partition
	      (lambda (l)
		(if (null? l)
		    '()
		    (insert (1st l) (partition (rest l))))))
	     (insert
	      (lambda (x l)
		(cond
		 ((null? l) (cons (list x) l))
		 ((andmap (lambda (y) (pred? x y)) (1st l))
		  (cons (cons x (1st l)) (rest l)))
		 (else (cons (1st l) (insert x (rest l))))))))
      (partition l))))

;; bounded-random-partition randomly partitions the elements of l into
;; equivalence classes (under pred?) of size bound.  (If the number of
;; elements of l is not an integer multiple of bound, then exactly one
;; class will contain fewer elements than bound.)

(define bounded-random-partition
  (lambda (pred? l bound)
    (letrec
      ((partition
	 (lambda (l)
	   (if (null? l)
	     '()
	     (let ((x (random-pick l)))
	       (insert x (partition (remove-first x l)))))))
       (insert
	 (lambda (x l)
	   (cond
	     ((null? l) (cons (list x) l))
	     ((and (< (length (1st l)) bound)
		   (andmap (lambda (y) (pred? x y)) (1st l)))
	      (cons (cons x (1st l)) (rest l)))
	     (else (cons (1st l) (insert x (rest l)))))))
       (remove-first
	 (lambda (x l)
	   (cond
	     ((null? l) '())
	     ((eq? (1st l) x) (rest l))
	     (else (cons (1st l) (remove-first x (rest l))))))))
      (partition l))))

;; force member? to return #t or #f
(define member?
  (lambda (a l)
    (if (memq a l) #t #f)))

(define member-pred?
  (lambda (pred? x l)
    (ormap (lambda (y) (pred? x y)) l)))

(define member-equal?
  (lambda (x l)
    (member-pred? equal? x l)))

(define subset?
  (lambda (set1 set2)
    (andmap (lambda (x) (member? x set2)) set1)))

(define subset-pred?
  (lambda (pred? set1 set2)
    (andmap (lambda (x) (member-pred? pred? x set2)) set1)))

(define sets-equal?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define sets-equal-pred?
  (lambda (pred? set1 set2)
    (and (subset-pred? pred? set1 set2)
         (subset-pred? pred? set2 set1))))

(define sets-disjoint?
  (lambda (set1 set2)
    (andmap (lambda (x) (not (member? x set2))) set1)))

(define sets-intersect?
  (lambda (set1 set2)
    (ormap (lambda (x) (member? x set2)) set1)))

(define remove-elements-pred
  (lambda (pred? elements l)
    (cond
      ((null? l) '())
      ((member-pred? pred? (1st l) elements)
       (remove-elements-pred pred? elements (rest l)))
      (else (cons (1st l) (remove-elements-pred pred? elements (rest l)))))))

(define remq-elements
  (lambda (elements l)
    (cond
      ((null? l) '())
      ((member? (1st l) elements) (remq-elements elements (rest l)))
      (else (cons (1st l) (remq-elements elements (rest l)))))))

(define remove-elements
  (lambda (elements l)
    (remove-elements-pred equal? elements l)))

(define remove-duplicates-pred
  (lambda (pred?)
    (letrec
      ((remove-duplicates
	 (lambda (l)
	   (cond
	     ((null? l) '())
	     ((member-pred? pred? (1st l) (rest l)) (remove-duplicates (rest l)))
	     (else (cons (1st l) (remove-duplicates (rest l))))))))
      remove-duplicates)))

(define remq-duplicates
  (remove-duplicates-pred eq?))

(define remove-duplicates
  (remove-duplicates-pred equal?))

(define string-suffix
  (lambda (s i)
    (substring s i (string-length s))))

(define reveal
  (lambda (x)
    (if (list? x)
      (map-leaves reveal-obj x)
      (reveal-obj x))))

(define reveal-obj
  (lambda (obj)
    (cond
      ((not (procedure? obj)) obj)
      ((slipnode? obj) (string->symbol (string-downcase (format-slipnode obj))))
      ((or (letter? obj) (group? obj)) (tell obj 'ascii-name))
      (else (string->symbol (format "<~a>" (tell obj 'object-type)))))))

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)
(define 4th cadddr)
(define 5th (compose car cddddr))
(define 6th (compose cadr cddddr))
(define 7th (compose caddr cddddr))
(define 8th (compose cadddr cddddr))
(define rest cdr)
(define coord make-rectangular)

(define get-first
  (lambda (n l)
    (if (zero? n)
      '()
      (cons (1st l) (get-first (- n 1) (rest l))))))

(define sublist
  (lambda (l m n)
    (get-first (- n m) (list-tail l m))))

(define nth
  (lambda (n l)
    (list-ref l n)))

(define snoc
  (lambda (x l)
    (append l (list x))))

(define last
  (lambda (l)
    (if (null? (rest l))
      (1st l)
      (last (rest l)))))

(define all-but-last
  (lambda (n l)
    (if (= (length l) n)
      '()
      (cons (1st l) (all-but-last n (rest l))))))

(define x-coord real-part)
(define y-coord imag-part)

(define pause
  (lambda (ms)
    (thread-sleep ms)))

(define rand-num
  (lambda ()
    (- (n-sided-die 101) 1)))

(define rand-power
  (lambda (exponent)
    (* 100.0 (- 1 (power (* (rand-num) 0.01) exponent)))))

(define of-trio
  (lambda ()
    (min (rand-power 1) (rand-power 1) (rand-power 1))))

(define new-qual-vect
  (lambda (n)
    (cond
     [(eq? n 1) (list (- (n-sided-die 101) 1))]
     [else (cons
	    (of-trio)
	    ; (rand-power 1) 2, 3.321928, 6.643856
	    (new-qual-vect (- n 1)))])))

(define best-item-try
  (lambda (vector)
    (let*
	([len (length vector)]
	 [extra (new-qual-vect len)])
      (map max extra vector))))

(define best-vect-try
  (lambda (vector)
    (let*
	([len (length vector)]
	 [extra (new-qual-vect len)])
      (if (> (average vector) (average extra))
	  vector
	  extra))))

(define pair-vect-scores
  (lambda (n)
    (let*
	([dee (new-qual-vect n)]
	 [dum (new-qual-vect n)])
      (list
       (* 1.0 (average dee))
       (* 1.0 (average dum))))))

(define k-vectors
  (lambda (k n)
    (cond
     [(eq? k 1) (list (new-qual-vect n))]
     [else (cons (new-qual-vect n)
		 (k-vectors (- k 1) n))])))

(define best-whole-score
  (lambda (vectors)
    (* 1.0 (apply max (map average vectors)))))

(define best-item-vector
  (lambda (vectors)
    (cond
     [(eq? (length vectors) 2) (map max (car vectors) (cadr vectors))]
     [else (map max (car vectors) (best-item-vector (cdr vectors)))])))

(define best-item-score
  (lambda (vectors) 
    (* 1.0 (average (best-item-vector vectors)))))

; m SETS of k VECTORS each, of LENGTH n
(define m-k-n-vectors
  (lambda (m k n)
    (if (eq? m 1)
	(list (k-vectors k n))
	(cons (k-vectors k n)
	      (m-k-n-vectors (- m 1) k n)))))

; k vectors of length n
(define item-best-score
  (lambda (m k n)
    (let
	([vectors (m-k-n-vectors m k n)])
      (average (map best-item-score vectors)))))

; k vectors of length n
(define whole-best-score
  (lambda (m k n)
    (let
	([vectors (m-k-n-vectors m k n)])
      (average (map best-whole-score vectors)))))

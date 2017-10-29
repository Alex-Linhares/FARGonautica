; not part of LS program -- just for stats

(define within-five
  (lambda (n ls)
    (cond
     [(null? ls) 0]
     [else (if (< (abs (- n (car ls))) 5)
	       (+ 1 (within-five n (cdr ls)))
	       (within-five n (cdr ls)))])))

(define range
  (lambda (m n)
    (cond
     [(eq? n m) (list m)]
     [else (cons m (range (+ 1 m) n))])))

(define blur-histo
  (lambda (ls)
    (let*
	([spread (range 1 (apply max ls))]
	 [neighbs
	  (lambda (x)
	    (within-five x ls))])
      (map list spread (map neighbs spread)))))

(define quint
  (lambda (x)
    (* 5 x)))


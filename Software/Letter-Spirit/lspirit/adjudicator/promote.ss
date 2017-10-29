; just fooling for now

(define promote-SP
  (lambda (n saw?)
    (if saw?
	(if (> (n-sided-die 3) 1)
	    n (- n 1))
	(if (> (n-sided-die 3) 1)
	    (+ n 1) n))))


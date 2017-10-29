; get a REALLY random seed from the clock

(set! pi 3.14159265358979326)
(set! e (exp 1))

(define power
  (lambda (x y)
    (exp (* y (log x)))))

(define randomize
  (let ((upper-bound (expt 2 32)))
    (lambda ()
      (random-seed (modulo (round (expt (real-time) 5/3)) upper-bound)))))

(define z-to-dist
  (lambda (z)
    (*
     (/ 1.0 (sqrt (* 2 pi)))
     (power e (* -0.5 (* z z))))))

(define dist-iterate
  (lambda (step z)
    (if
	(< z step)
	0
	(+
	 (* step (z-to-dist z))
	 (dist-iterate step (- z step))))))

(define z-to-dens
  (lambda (z)
    (if (< z 0)
	(- 1 (z-to-dens (* -1.0 z)))
	(+ 0.5
	   (dist-iterate 0.00001 z)))))

(define real-in-range
  (lambda (a b)
    (+ a (* (- b a) (/ (random 1000) 1000.0)))))

(define random-goodness
  (lambda ()
    (real-in-range 0 1)))

(define random-set
  (lambda (n)
    (if (eq? 0 n) '()
	(cons (random-goodness) (random-set (- n 1))))))

(set! threshold 0.90)

(define random-replace
  (lambda (x)
    (if (> x threshold) x (random-goodness))))

(define address-least ; try to do something about the worst
  (lambda (nls)
    (let*
	([least (apply min nls)]
	 [rest (remove-item least nls)]) ; what if there's a tie?
      (cons (max least (random-goodness)) rest))))

(define revise-n-times
  (lambda (nls n)
    (cond
     [(< n 1) nls]
     [(eq? n 1) (address-least nls)]
     [else (revise-n-times (address-least nls) (- n 1))])))

(define ls-loop-n
  (lambda (n)
    (round-3 (average (revise-n-times (random-set 26) n)))))

; n tests where you revise things m times
(define loop-tests-m-n
  (lambda (m n)
    (cond
     [(< n 1) '()]
     [(eq? n 1) (list (ls-loop-n m))]
     [else (cons (ls-loop-n m)
		 (loop-tests-m-n m (- n 1)))])))

; n revisions per experimental gridfont generation
(define loop-investigate
  (lambda (n)
    (round-3 (average (loop-tests-m-n n 1000)))))
     
; (z-to-dens (real-in-range -4 4))

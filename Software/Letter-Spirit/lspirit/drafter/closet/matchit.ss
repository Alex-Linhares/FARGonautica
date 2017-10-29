(define same-start-len
  (lambda (s1 s2)
    (begin
      (set! *cntr* (+ 1 *cntr*))
      (cond
       [(or (null? s1) (null? s2)) 0]
       [(equal? (car s1) (car s2))
	(+ 1 (same-start-len (cdr s1) (cdr s2)))]
       [else (same-start-len (cdr s1) (cdr s2))]))))

(define same-sub-len
  (lambda (s1 s2)
    (let*
	([cdr-s1 (cdr s1)]
	 [cdr-s2 (cdr s2)]
	 [len-s1 (length s1)]
	 [len-s2 (length s2)]
	 [len-part-s1 (length cdr-s1)]
	 [len-part-s2 (length cdr-s2)]
	 [start-full (same-start-len s1 s2)]
	 [next (max
		start-full
		(if (> (min len-part-s1 len-s2) start-full)
		    (same-sub-len cdr-s1 s2) 0)
		(if (> (min len-s1 len-part-s2) start-full)
		    (same-sub-len s1 cdr-s2) 0))])
      (max
       next
       (if (> (min len-part-s1 len-part-s2) next)
	   (same-sub-len cdr-s1 cdr-s2) 0)))))

; new tack

(define n-copies
  (lambda (item n)
    (if (eq? n 0) '()
	(cons item (n-copies item (- n 1))))))

; only s2 will be chopped down -- this saves time in testing
(define longest-streak
  (lambda (s1 s2 best current)
    (cond
     [(or (null? s2) (null? s1)) best]
     [(eq? (car s1) (car s2))
      (let
	  ([new-current (+ 1 current)])
      (longest-streak (cdr s1) (cdr s2) (max best new-current) new-current))]
     [else
      (longest-streak (cdr s1) (cdr s2) best 0)])))

(define longest-match
  (lambda (s1 s2)
    (if (or (null? s2) (eq? (car s2) 'X)) 0
	(max (longest-streak s1 s2 0 0)
	     (longest-match s1 (cdr s2))))))

; this assumes that 'X and 'Y will not be in either string
; this is intended for motifs, so it should hold
(define match-strings
  (lambda (s1 s2)
    (let*
	([extra-1 (- (length s2) 1)]
	 [extra-2 (- (length s1) 1)]
	 [big-1 (append (n-copies 'Y extra-2) s1)]
	 [big-2 (append s2 (n-copies 'X extra-1))])
      (longest-match big-1 big-2))))

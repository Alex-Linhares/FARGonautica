(define pt-remove-candidate
  (lambda (key)
    (set! *candidates* (remove-key key *candidates*))))

(define quantum-remove-candidate
  (lambda (key)
    (set! *candidates* (remove-key-2 key *candidates*))))

(define tip-touch-scores
  (lambda (which-tip)
    (let*
	([norm-scores (touch-norm-scores
		       (find-max (touch-norms *the-whole* *the-role*)))]
	 [should-touch?
	  (eq? 1
	       (if (eq? which-tip 1)
		   (car norm-scores)
		   (caddr norm-scores)))]
	 [good-points
	  (apply append *touch-points*)]
	 [bad-points
	  (if should-touch?
	      (apply append *avoid-points*)
	      (append
	       (apply append *avoid-points*)
	       (quanta-to-points *other-stuff*)))]
	 [touch-score
	  (lambda (pt)
	    (cond
	     [(and (member? pt good-points) should-touch?)
	      2000.0]
	     [(member? pt bad-points)
	      0.0005]
	     [else 1.0]))])
      (map list *all-points* (map touch-score *all-points*)))))

(define touch-norm-scores
  (lambda (touch-norm)
    (case touch-norm
      [nc '(0 0 0)]
      [all-over '(1 2 1)]
      [c-lf '(1 0 0)]
      [c-rt '(0 0 1)]
      [t1 '(1 0 0)]
      [m '(0 1 0)]
      [t2 '(0 0 1)]
      [2ts '(1 0 1)]
      [t1m '(1 1 0)]
      [mt2 '(0 1 1)]
      [2ms '(0 2 0)])))

; but remember that this material may have fulfilled its touching
; while this role was drawn

(define record-touching-info
  (lambda ()
    (let*
	([norm-scores (touch-norm-scores
		       (find-max (touch-norms *the-whole* *the-role*)))]
	 [points (quanta-to-points *own-stuff*)]
	 [other-points (quanta-to-points *other-stuff*)]
	 [t1 (if
		 (null? points)
		 '()
		 (list (car points)))]
	 [t2 (if
		 (null? points)
		 '()
		 (list (tail points)))]
	 [m (if
		 (null? points)
		 '()
		 (anticdr (cdr points)))])
      (if (and
	   (eq? 1 (car norm-scores))
	   (not (null? t1))
	   (not (member? (car t1) other-points)))
	  (set! *touch-points* (cons t1 *touch-points*))
	  (set! *avoid-points* (cons t1 *avoid-points*)))
      (if (and
	   (eq? 1 (cadr norm-scores))
	   (not (null? m))
	   (not (overlap? m other-points)))
	  (set! *touch-points* (cons m *touch-points*))
	  (begin
	    (set! *avoid-quanta* (cons *own-stuff* *avoid-quanta*))
	    (set! *avoid-points* (cons m *avoid-points*))))
       (if (and
	   (eq? 1 (caddr norm-scores))
	   (not (null? t2))
	   (not (member? (car t2) other-points)))
	  (set! *touch-points* (cons t2 *touch-points*))
	  (set! *avoid-points* (cons t2 *avoid-points*))))))

(define candidate-touching
  (lambda (candidate)
    (let
	([cand-pt (car candidate)]
	 [cand-q (cadr candidate)])
      (cond
       [(member? cand-pt (apply append *touch-points*))
	(if *mid-touch*
	    50.0
	    (if (or (null? *other-touch*)
		    (not *alt-mid-touch*))
		-2500.0
		0))]
       [(member? cand-pt (apply append *avoid-points*))
	-2500.0]
       [(member? cand-pt (quanta-to-points *other-stuff*))
	(if *mid-touch*
	    -50.0
	    2500.0)]
       [(mid-quanta-touch? (list cand-q) (apply append *avoid-quanta*))
	(if (or *mid-touch* (and (not (null? *other-touch*))
				 *alt-mid-touch*))
	    -250.0
	    -500.0)]       
       [else 0]))))

; for considering moves that would be the final of a role filler
(define candidate-quit-touching
  (lambda (candidate)
    (let
	([cand-pt (car candidate)]
	 [cand-q (cadr candidate)])
      (cond
       [(member? cand-pt (apply append *touch-points*))
	(if *t2-touch*
	    100.0
	    (if (and (not (null? *other-touch*))
		     *alt-t2-touch*)
		0
		-1500.0))]
       [(member? cand-pt (apply append *avoid-points*))
	-2500.0]
       [(member? cand-pt (apply append *avoid-points*))
	(if *t2-touch*
	    (if (or (null? *other-touch*)
		    *alt-t2-touch*)
		-2500.0
		0)
	    100.0)]
       [(mid-quanta-touch? (list cand-q) (apply append *avoid-quanta*))
	(if *t2-touch*
	    (if (or (null? *other-touch*)
		    *alt-t2-touch*)
		-2500.0
		0)
	    -500.0)]
       ; condition based on whether this is the *last* role?
       [(and *t2-touch* (not (null? *touch-points*)))
	(if (member? cand-pt (apply append *touch-points*))
	    100.0
	    -100.0)]
       [else 0]))))

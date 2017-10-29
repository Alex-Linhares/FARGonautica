; coming up with good scoring systems for the Examiner and Adjudicator

(define examiner-role-scores
  (lambda ()
    (map cadar (map sparked-roles *workspace*))))

(define examiner-goodness
  (lambda ()
    (if (eq? *answer* 'quit)
	300
	(round-3
	 (* (sqrt (/ *punish* -25))
	    (+
	     (r-role-score *answer*)
	     (let
		 ([sub-scores
		   (map cadar (map sparked-roles *workspace*))])
	       (if (null? sub-scores) 0
		   (* 0.8 (average sub-scores))))))))))


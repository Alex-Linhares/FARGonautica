(define points-curve
  (lambda (p-ls)
    (if
	(eq? (length p-ls) 1)
	0
    (let*
	([end1 (car p-ls)]
	 [end2 (tail p-ls)]
	 [point-dist
	  (lambda (x)
		       (*
			(point-line-error end1 x end2)
			(curve-sign end1 x end2)))])
      (round-3 (average (map point-dist p-ls)))))))

; gives the two points (in real coords) which "square off" with the
; two given points (passed as grid labels)
(define square-points
  (lambda (p1 p2)
    (let*
	([coords1 (point-coords p1)]
	 [coords2 (point-coords p2)]
	 [coords-mid (map average (map list coords1 coords2))]
	 [mid-x (car coords-mid)]
	 [mid-y (cadr coords-mid)]
	 [xdiff (- (car coords1) mid-x)]
	 [ydiff (- (cadr coords1) mid-y)]
	 [square-right (list
		       (- mid-x ydiff)
		       (+ mid-y xdiff))]
	 [square-left (list
		       (+ mid-x ydiff)
		       (- mid-y xdiff))])
      (list
       square-left
       square-right))))

; the first point is in the form (x-coord y-coords) and may not be
; an actual point on the grid
; the second point is given by grid number

(define real-point-grid-dist
  (lambda (real-pt grid-pt)
    (let*
	([grid-coords (point-coords grid-pt)]
	 [four-coords (append real-pt grid-coords)])
      (sqrt (apply dist-sq four-coords)))))

(set! *all-points*
      '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21))

(set! *all-quanta*
      '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19
	  20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39
	  40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55))

(define all-dist
  (lambda (real-pt)
    (let
	([this-dist
	  (lambda (grid-pt)
	    (real-point-grid-dist real-pt grid-pt))])
      (map this-dist *all-points*))))

; gives coords of point fraction r of the way from p1 to p2
; p1 and p2 are pairs of coords
(define points-slide
  (lambda (p1 p2 r)
    (list
     (+ (* r (- (car p2) (car p1))) (car p1))
     (+ (* r (- (cadr p2) (cadr p1))) (cadr p1)))))

; point to use as the center of arc when plotting curve on grid
; note -- *straight* handled differently
(define arc-center
  (lambda (curve p1 p2)
    (let
	([fraction
	  (case curve
	    [*full-left* 0.1]
	    [*strong-left* 0.25]
	    [*square-left* 0.5]
	    [*slight-left* 1.0]
	    [*slight-right* 0.0]
	    [*square-right* 0.5]
	    [*strong-right* 0.75]
	    [*full-right* 0.9])]
	 [squared-out (square-points p1 p2)])
      (points-slide (car squared-out) (cadr squared-out) fraction))))

; may need to tinker with these
(define arc-radius
  (lambda (curve p1 p2)
    (let
	([fraction
	  (case curve
	    [*full-left* 0.64] ; was 0707
	    [*strong-left* 0.559]
	    [*square-left* 0.65]
	    [*slight-left* 0.707]
	    [*slight-right* 0.707]
	    [*square-right* 0.65]
	    [*strong-right* 0.559]
	    [*full-right* 0.64])]) ; was 0707
      (* fraction (points-dist p1 p2)))))

(define all-from-center
  (lambda (curve p1 p2)
    (all-dist (arc-center curve p1 p2))))

(define all-diff
  (lambda (n ls)
    (let
	([diff-n (lambda (m) (abs (- m n)))])
      (map diff-n ls))))

(define arc-dists
  (lambda (curve p1 p2)
    (all-diff
     (arc-radius curve p1 p2)
     (all-from-center curve p1 p2))))

(define arc-score
  (lambda (curve-dist)
    (round-3 (min 100 (max 0 (- 100 (* 20.0 curve-dist)))))))

(define curve-label-sign
  (lambda (curve)
    (case curve
      [(*full-left* *strong-left* *square-left* *slight-left*) -1]
      [(*full-right* *strong-right* *square-right* *slight-right*) 1]
      [else 0])))

(define curve-draw-sign
  (lambda (p1 pt p2)
    (if (or (eq? p1 pt) (eq? p2 pt))
	0
	(curve-sign p1 pt p2))))

(define curve-signs
  (lambda (curve p1 p2)
    (let*
	([label-sign (curve-label-sign curve)]
	 [local-curve-sign
	  (lambda (pt)
	    (curve-draw-sign p1 pt p2))]
	 [same-sign?
	  (lambda (sign)
	    (if (< (abs (- label-sign sign)) 1.5) 1.0 0.1))])
      (map same-sign? (map local-curve-sign *all-points*)))))

; for all curve types but *straight*, give the scores for these parameters
(define arc-weights
  (lambda (curve p1 p2)
    (map round-3
	 (map *
	      (map arc-score (arc-dists curve p1 p2))
	      (curve-signs curve p1 p2)))))

; gotta handle *straight*
; needs to be from a line segment -- not from a line?
; 5.5 constant should probably be bigger
(define line-scores
  (lambda (p1 p2)
    (let
	([point-dist
	  (lambda (pt)
	    (* 5.5 (abs (points-curve (list p1 pt p2)))))])
      (map arc-score (map point-dist *all-points*)))))

(define curve-weights
  (lambda (curve p1 p2)
    (if (eq? curve '*straight*)
	(line-scores p1 p2)
	(arc-weights curve p1 p2))))

; want to employ NVs later
(define norm-curve
  (lambda ()
    (let
	([type (lookup 'topology (eval *the-role*))])
      (if (eq? type 'bisegment)
	  (list
	   (find-max
	    (car (lookup-list 'curve1 *norms*)))
	   (find-max
	    (car (lookup-list 'curve2 *norms*))))
	   (find-max (car (lookup-list 'curve *norms*)))))))

; want to employ NVs later
(define role-curve
  (lambda ()
    (map round-3 (curve-weights (norm-curve) *tip-1* *tip-2*))))

(define candidate-breadcrumbs
  (lambda (candidate)
    (round-3 (nth (car candidate) *role-curve*))))

(define candidate-homing
  (lambda (candidate)
    (let*
	([option (point-coords (car candidate))]
	 [goal (point-coords *tip-2*)]
	 [from (point-coords *current-point*)]
	 [from-x (car from)]
	 [from-y (cadr from)]
	 [dx-would (- (car option) from-x)]
	 [dy-would (- (cadr option) from-y)]
	 [dx-should (- (car goal) from-x)]
	 [dy-should (- (cadr goal) from-y)])
      (if
	  (or (same-sign? dx-would dx-should)
	       (same-sign? dy-would dy-should))
	  0.0
	  -100.0))))

(define start-compass-norm
  (lambda (compass)
    (let*
	([norms (cdadar (lookup-list 'tips *norms*))]
	 [converted (map list
			 (tip-flip (map car norms))
			 (map cadr norms))]
	 [score (* 10 (lookup-score compass converted))])
      (if (>= score 0) score -5))))

(define candidate-homing
  (lambda (candidate)
    (let*
	([turn (nth 4 candidate)]
	 [direction (nth 3 candidate)]
	 [payoff (if (eq? *last-point* 'none)
		     (start-compass-norm direction)
		     20.0)])
      (* 0.9
	 (case turn
	   [(180) (* 1.25 payoff)]
	   [(135 -135) (* 0.75 payoff)]
	   [(90 -90) (* 0.45 payoff)]
	   [(45 -45) 0.0]
	   [else payoff])))))

; -----------------------------------------------------------------------
; new attempt at curve rewards
; -----------------------------------------------------------------------

; p1 is the arc center, p2 is the point on the curve
(define aim-angle
  (lambda (curve p1 p2)
    (if (eq? curve '*straight*)
	(points-angle p1 p2)
	(let
	    ([direction
	      (if (member? curve
			   '(*slight-left* *square-left*
					   *strong-left* *full-left*))
		  'left
		  'right)])
	  (tangent-heading p1 p2 direction)))))

(define angle-score
  (lambda (curve p1 p2 pt angle)
    (let
	([diff
	  (/
	   (abs
	    (angle-diff angle
			(if (eq? curve '*straight*)
			    (points-angle (point-coords p2)
					  (point-coords p1))
			    (let
				([coords (point-coords pt)]
				 [center (arc-center curve p1 p2)])
			      (if (tiny? (distance (car coords)
						   (cadr coords)
						   (car center)
						   (cadr center)))
				  250.0
				  (aim-angle curve
					     center
					     coords))))))
	   15.0)])
      (round-3
       (- 300
	  (* 1.66667 diff))))))
	   

(define heading-score
  (lambda (p1 p2)
    (* 10
       (angle-score *norm-curve* *tip-1* *tip-2* p1
		    (points-angle (point-coords p1) (point-coords p2))))))

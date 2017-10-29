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
	    [*full-left* 1.0]
	    [*strong-left* 0.75]
	    [*square-left* 0.5]
	    [*slight-left* 0.0]
	    [*slight-right* 1.0]
	    [*square-right* 0.5]
	    [*strong-right* 0.25]
	    [*full-right* 0.0])]
	 [squared-out (square-points p1 p2)])
      (points-slide (car squared-out) (cadr squared-out) fraction))))

; may need to tinker with these
(define arc-radius
  (lambda (curve p1 p2)
    (let
	([fraction
	  (case curve
	    [*full-left* 0.707]
	    [*strong-left* 0.559]
	    [*square-left* 0.5]
	    [*slight-left* 0.707]
	    [*slight-right* 0.707]
	    [*square-right* 0.5]
	    [*strong-right* 0.559]
	    [*full-right* 0.707])])
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
    (round-3 (min 100 (max 0 (- 100 (* 50.0 curve-dist)))))))

(define curve-label-sign
  (lambda (curve)
    (case curve
      [(*full-left* *strong-left* *square-left* *slight-left*) 1]
      [(*full-right* *strong-right* *square-right* *slight-right*) -1]
      [else 0])))

(define curve-signs
  (lambda (curve p1 p2)
    (let*
	([label-sign (curve-label-sign curve)]
	 [local-curve-sign
	  (lambda (pt)
	    (if (< (abs (points-curve (list p1 pt p2))) 0.06)
		-1
		(- (curve-sign p1 pt p2))))]
	 [same-sign?
	  (lambda (sign)
	    (if (eq? label-sign sign) 1.0 0.0))])
      (map same-sign? (map local-curve-sign *all-points*)))))

; for all curve types but *straight*, give the scores for these parameters
(define arc-weights
  (lambda (curve p1 p2)
    (map round-3
	 (map *
	      (map arc-score (arc-dists curve p1 p2))
	      (curve-signs curve p1 p2)))))

(define within-segment
  (lambda (p1 pt p2)
    (let*
	([mid (midpoint (map point-coords (list p1 p2)))]
	 [pt-length (real-point-grid-dist mid pt)]
	 [hypotenuese-sq (* pt-length pt-length)]
	 [mid-leg-sq (* 0.25 (points-dist-sq p1 p2))]
	 [out-leg1-sq (points-dist-sq p1 pt)]
	 [out-leg2-sq (points-dist-sq p2 pt)]
	 [leg-sq-diff (- hypotenuese-sq mid-leg-sq)])
      (if (and
	   (< (- leg-sq-diff out-leg1-sq) 0.05)
	   (< (- leg-sq-diff out-leg2-sq) 0.05))
	  1 0))))

; gotta handle *straight*
; needs to be from a line segment -- not from a line?
; 5.5 constant should probably be bigger
(define line-scores
  (lambda (p1 p2)
    (let
	([point-dist
	  (lambda (pt)
	    (* 5.5 (abs (points-curve (list p1 pt p2)))))]
	 [point-within-segment
	  (lambda (pt)
	    (within-segment p1 pt p2))])
	 (map *
	      (map point-within-segment *all-points*)
	      (map arc-score
		   (map point-dist *all-points*))))))

(define curve-weights
  (lambda (curve p1 p2)
    (if (eq? curve '*straight*)
	(line-scores p1 p2)
	(arc-weights curve p1 p2))))

(define bisegment-curve-cast
  (lambda (curve1 curve2 p1 mid p2)
    (let
	([seg1 (curve-weights curve1 p1 mid)]
	 [seg2 (curve-weights curve2 mid p2)])
      (map + seg1 seg2))))

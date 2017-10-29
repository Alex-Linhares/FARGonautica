(define smart-parse
  (lambda ()
    (set! *workspace* (q-lists-to-workspace (gestalt-parse)))
    (if *exam-verbose*
	(printf "~s smartparse to ~s~%"
		(+ 1 *codelets-run*) (map collapse (map car *workspace*))))
    (dampen)
    (add-n-to-coderack (* 2 (length *workspace*))
		       looker-codelet
		       'looker *very-high-urgency* (add1 0))))

; ideally, you'd do this on a roulette basis
; so the runners-up have a chance
(define gestalt-winner
  (lambda ()
    (find-max *gestalt-index*)))

(define active-gestalts
  (lambda (ls)
    (cond
     [(null? ls) '()]
     [(> (cadar ls) 0) (cons (car ls) (active-gestalts (cdr ls)))]
     [else (active-gestalts (cdr ls))])))

(define roulette-index
  (lambda ()
    (n-sided-die (apply + (map cadr (active-gestalts *gestalt-index*))))))

(define gestalt-parse
  (lambda ()
    (let*
	([options (active-gestalts *gestalt-index*)]
	 [winner (if (null? options)
		     'o
		     (if (< *codelets-run* 5)
			 (gestalt-winner)
			 (roulette-select (active-gestalts *gestalt-index*)
					  (roulette-index))))]
	 [gestalt-string (roulette (lookup winner *parse-smarts*))]
	 [first-pass (map linearize ; BUG HERE!? 9/25/99
			  (eval-string (car gestalt-string) *quanta-list*))])
      (if (or (eq? (length gestalt-string) 1)
	      (same-contents (car first-pass) *quanta-list*))
	  first-pass
	  (cons (car first-pass)
		(eval-string (cadr gestalt-string)
			     (apply append (cdr first-pass))))))))

(define q-lists-to-workspace
  (lambda (q-lists)
    (map quanta-to-part q-lists)))

(define quanta-to-part
  (lambda (qls)
    (let
	((q-list (if (atom? qls) (list qls) qls)))
      (list
       (quanta-joints q-list)
       '(**whine 10)))))

;----------------------------------------------------------------------
; Evaluating the parse commands
;----------------------------------------------------------------------

(define eval-string
  (lambda (parse-string qls)
    (let*
	; took out robust-linearize, 8/30/99
	; may re-introduce bug on weird shapes, but works best
	; for e.g. standard-square w
	([island1 (glom-islands qls)]
	 [island1-stuff (apply append island1)]
	 [island1-missing (subtract qls island1-stuff)]
	 [islands
	  (if (null? island1-missing)
	      island1
	      (append island1
		      (glom-islands island1-missing)))])
      (case (car parse-string)
	[glom-islands islands]
	[cleave-out
	 (let*
	     ([start-point (parse-item (cadr parse-string) 'no-except qls)]
	      [cleaveland (island-with-point start-point islands)]
	      [rest (remove-item cleaveland islands)]
	      [finish-point
	       (parse-item (caddr parse-string) start-point cleaveland)])
	   (append rest (cleave-out start-point finish-point cleaveland)))]))))

; allows for:
; point-nearest (after previous cleaves)
; tip-nearest (after previous cleaves)
; global-tip (ignoring previous cleaves)

(define parse-item
  (lambda (item except qls)
    (if (and (list? item) (or
			   (eq? (length item) 2)
			   (eq? (car item) 'trp)
			   (eq? (car item) 'quad)))
	(case (car item)
	  [tp-n (tip-nearest (cadr item) except qls)]
	  [pt-n (point-nearest (cadr item) except qls)]
	  [g-tp (tip-nearest (cadr item) '() qls)]
	  [trp (triple-point qls)]
	  [quad (quadruple-point qls)]))))

(define cleave-out
  (lambda (pt1 pt2 qls)
    (let*
	([one-part (shortest-path pt1 pt2 qls)]
	 [rest (subtract qls one-part)])
      (if (null? rest)
	  (list one-part)
	  (let*
	      ([others (robust-linearize rest)]
	       [others-ready
		(if (atom? (car others))
		    (list (linearize others))
		    (robust-linearize rest))])
	    (cons one-part others-ready))))))

(define quantum-neighbors
  (lambda (q)
    (car (lookup-list q *neighbors*))))

(define quantum-endpoints
  (lambda (q)
    (car (lookup-list q *quanta-endpoints*))))

(define point-in-quanta?
  (lambda (pt qls)
    (member? pt (apply append (map quantum-endpoints qls)))))

(define island-with-point
  (lambda (pt ls-ls)
    (cond
     ((null? ls-ls) #f)
     ((point-in-quanta? pt (car ls-ls)) (car ls-ls))
     (else (island-with-point pt (cdr ls-ls))))))

; equivalence relation: separates qls into separate non-touching parts
(define glom-islands
  (lambda (qls)
    (let
	([islands (list-split (list (car qls)) (cdr qls))])
      (if (atom? (car islands))
	  (list islands)
	  islands))))

(define list-split
  (lambda (ls1 ls2)
    (let* ([options
	   (lambda (qls)
	     (remove '- (uniquify
			 (apply append
				(map quantum-neighbors qls)))))]
	  [absorb (intersect ls2 (options ls1))])
      (cond
       [(null? ls2) (list ls1)]
       [(null? absorb)
	(append (list ls1) (list-split (list (car ls2)) (cdr ls2)))]
       [else (list-split (append absorb ls1) (subtract ls2 absorb))]))))

;----------------------------------------------------------------------
; linearizing
; even as of 8/10/99, gridletters which fail the Koenigsberg bridge
; test can cause an Examiner crash
;----------------------------------------------------------------------

(define robust-linearize
  (lambda (qls)
    (if (null? qls)
	'()
	(let*
	    ([tips (quanta-real-tips qls)]
	     [start (if (null? tips)
			(car qls)
			(car (intersect
			      (lookup (car tips) *point-list*) qls)))]
	     [first-swing
	      (robust-neighbor-sort (list start) (remove start qls))])
	  (append
	   first-swing
	   (robust-linearize (subtract qls (apply append first-swing))))))))

(define robust-neighbor-sort
  (lambda (ls1 ls2)
    (let* ([tail1 (tail ls1)]
	   [bypassed
	    (if
		(> (length ls1) 1)
		(lookup
		(car (intersect
		      (quantum-get-points tail1)
		      (quantum-get-points (tail (anticdr ls1)))))
		*point-list*)
		'())]
	   [options (subtract
		     (intersect ls2 (lookup tail1 *neighbors*))
		     bypassed)]
	   [deadend (or (null? ls2) (null? options))]
	   [head2
	    (if (not deadend) (roulette options) nil)])
      (cond
       [(null? ls2) (list ls1)]
       [(null? options)
	(cons ls1
		(car (map robust-linearize (glom-islands ls2))))]
       [else
	(robust-neighbor-sort (snoc head2 ls1) (remove head2 ls2))]))))

;----------------------------------------------------------------------

; could include loners with (cons (list q) [map] if needed
(define quantum-joints
  (lambda (q qls)
    (let*
	([local-neighbors (intersect qls (lookup q *neighbors*))]
	 [make-joint
	  (lambda (q2)
	    (list q q2))])
      (map make-joint local-neighbors))))

(define quanta-joints
  (lambda (qls)
    (if
	(null? qls) '()
	(append (list (list (car qls)))
		(quantum-joints (car qls) (cdr qls))
		(quanta-joints (cdr qls))))))

;----------------------------------------------------------------------

(define point-nearest-points
  (lambda (pt except candidates)
    (let*
	([distance
	  (lambda (cand-pt)
	    (list cand-pt (points-dist pt cand-pt)))])
      (find-min (map distance (subtract candidates (list except)))))))

(define point-nearest
  (lambda (pt except qls)
    (point-nearest-points pt except (quanta-get-points qls))))

(define tip-nearest
  (lambda (pt except qls)
    (let
	([tips (quanta-real-tips qls)])
    (if (null? (remq except tips))
	(point-nearest-points pt except (quanta-get-points qls))
	(point-nearest-points pt except tips)))))

; for e and k parse
; 4 is the backup, since that's where action tends to occur

(define triple-point
  (lambda (qls)
    (let
	([trip-points (triple-points *point-list*)])
      (if (null? trip-points)
	  (point-nearest 4 '() qls)
	  (roulette trip-points)))))

(define quadruple-point
  (lambda (qls)
    (let
	([quad-points (quadruple-points *point-list*)])
      (if (null? quad-points)
	  (triple-point qls)
	  (roulette quad-points)))))

;----------------------------------------------------------------------
; find the shortest path between two points on the grid
;----------------------------------------------------------------------

(define add-adj-graph
  (lambda (q graph)
    (let*
	([pts (lookup q *quanta-endpoints*)]
	 [pt1 (car pts)]
	 [pt2 (cadr pts)]
	 [pt1-old (lookup-list pt1 graph)]
	 [pt2-old (lookup-list pt2 graph)]
	 [pt1-added
	  (if
	      (null? pt1-old)
	      (cons (list pt1 (list pt2)) graph)
	      (cons (list pt1 (cons pt2 (car pt1-old)))
		    (remove-key pt1 graph)))])
      (if
	  (null? pt2-old)
	  (cons (list pt2 (list pt1)) pt1-added)
	  (cons (list pt2 (cons pt1 (car pt2-old)))
		(remove-key pt2 pt1-added))))))

(define make-adj-graph
  (lambda (qls graph)
    (if (null? qls) graph
	(add-adj-graph (car qls) (make-adj-graph (cdr qls) graph)))))

; search the graph from source to target
(define breadth-first-search-graph
  (lambda (s t graph path)
    (let
        ([s-next (lookup-list s graph)]
         [search-next
          (lambda (node)
	    (breadth-first-search-graph
	     node t (remove-key s graph) (cons node path)))])
      (cond
        [(null? s-next) 'no-way]
        [(member? t (car s-next)) (cons t path)]
        [else (map search-next (car s-next))]))))

(define shortest-path
  (lambda (s t qls)
    (let*
	([path
	  (breadth-first-search-graph s t (make-adj-graph qls '()) (list s))])
      (if
	 (not path) '()
	 (points-to-quanta (reverse (soln? path)))))))

(define soln?
  (lambda (ls)
    (cond
      ((null? ls) #f)
      ((and (atom? (car ls))
	    (not (eq? 'no-way (car ls))))
       ls)
      ((list? (car ls)) (better-path (soln? (car ls))
				     (soln? (cdr ls))))
      (else (soln? (cdr ls))))))

(define better-path
  (lambda (p1 p2)
    (if (not p1) p2
	(if (not p2) p1
	    (if (< (length p1) (length p2))
		p1
		p2)))))

;----------------------------------------------------------------------
; Top-down parsing info
; could automate from tip info in role-definitions... ideally

; if this fails, a good backup would be to go with alternate tip
; locations for the roles
; simpler, you could parse the gridletter with the motif worms
;----------------------------------------------------------------------
(set! *parse-smarts*
      '((a (((cleave-out (tp-n 3) (pt-n 19)))))
        (b (((cleave-out (tp-n 1) (pt-n 5)))))
        (c (((glom-islands))))
        (d (((cleave-out (tp-n 15) (pt-n 19)))))
        (e (((cleave-out (pt-n 18) (pt-n 4)))
	    ((cleave-out (trp) (pt-n 18)))))
        (f (((cleave-out (tp-n 15) (tp-n 12)))))
        (g (((cleave-out (tp-n 7) (pt-n 17)))))
        (h (((cleave-out (tp-n 1) (tp-n 5)))))
        (i (((glom-islands))))
        (j (((glom-islands))))
        (k (((cleave-out (tp-n 1) (pt-n 5))
	     (cleave-out (tp-n 17) (pt-n 5)))
	    ((cleave-out (quad) (tp-n 17))
	     (cleave-out (quad) (tp-n 19)))
	    ((cleave-out (tp-n 1) (pt-n 5))
	     (cleave-out (tp-n 17) (pt-n 3)))))
        (l (((glom-islands))))
        (m (((cleave-out (tp-n 5) (pt-n 3))
	     (cleave-out (tp-n 5) (pt-n 3)))
	    ((cleave-out (pt-n 11) (pt-n 10)))
	    ((cleave-out (tp-n 5) (pt-n 3))
	     (cleave-out (tp-n 11) (pt-n 3)))
	    ((cleave-out (tp-n 5) (pt-n 3))
	     (cleave-out (tp-n 3) (pt-n 12)))))
        (n (((cleave-out (tp-n 5) (pt-n 3)))))
        (o (((glom-islands))))
	(p (((cleave-out (tp-n 7) (pt-n 3)))))
        (q (((cleave-out (tp-n 21) (pt-n 17)))))
        (r (((cleave-out (tp-n 5) (pt-n 3)))))
        (s (((cleave-out (tp-n 17) (pt-n 11)))))
        (t (((cleave-out (tp-n 9) (tp-n 19)))))
        (u (((cleave-out (tp-n 17) (pt-n 19)))))
        (v (((cleave-out (tp-n 17) (pt-n 13)))))
        (w (((cleave-out (tp-n 17) (pt-n 19))
	     (cleave-out (tp-n 17) (pt-n 19)))
	    ((cleave-out (tp-n 17) (pt-n 19))
	     (cleave-out (tp-n 11) (pt-n 19)))
	    ((cleave-out (tp-n 17) (pt-n 19))
	     (cleave-out (tp-n 19) (pt-n 10)))))
        (x (((cleave-out (tp-n 17) (tp-n 5)))))
        (y (((cleave-out (tp-n 17) (tp-n 7)))))
        (z (((cleave-out (tp-n 3) (pt-n 17))
	     (cleave-out (tp-n 19) (pt-n 5)))
	    ((cleave-out (tp-n 19) (pt-n 5))
	     (cleave-out (tp-n 5) (pt-n 17)))))))

(set! *radial-order* '(*n* *ne* *e* *se* *s* *sw* *w* *nw*))

; take a bunch of quanta making up a part and do an endpoint-forward
; traversal... if they describe a loop, start anywhere
; may as well do this with parts right at the beginning of the Adjudicator
; if it has more than two tips, forget about it; it'll break soon, anyway
(define linearize
  (lambda (qls)
    (let*
	([tips (quanta-real-tips qls)]
	 [start (if (null? tips)
		    (car qls)
		    (car (intersect
			  (lookup (car tips) *point-list*) qls)))])
      (if (< (length tips) 3)
	  (apply append
		 (robust-neighbor-sort (list start) (remove start qls)))
	  qls))))

(define neighbor-sort
  (lambda (ls1 ls2)
    (let* ([tail1 (tail ls1)]
	   [head2
	    (if (not (null? (intersect ls2 (lookup tail1 *neighbors*))))
		; find a quantum from which to start linearizing
		(car (intersect ls2 (lookup tail1 *neighbors*)))
		nil)])
      (cond
       ((null? ls2) ls1)
       (else (neighbor-sort (snoc head2 ls1) (remove head2 ls2)))))))

(define quanta-neighbors?
  (lambda (a b)
    (member? a (lookup b *neighbors*))))

(define neighbor-sortable?
  (lambda (ls1 ls2)
    (let* ([tail1 (tail ls1)]
	   [branches (intersect ls2 (lookup tail1 *neighbors*))]
	   [head2
	    (if (not (null? (append ls2 branches))) (car branches) nil)]
	   [num-branches (length branches)]
	   [fork-in-road?
	    (or (> num-branches 2)
		(and (eq? num-branches 2)
		     (> (length (append ls1 ls2)) 3)
		     (quanta-neighbors?
		      (car branches) (cadr branches))))])
      (cond
       [(or (null? (append ls1 ls2)) fork-in-road?) #f]
       [(null? ls2) #t]
       [else (neighbor-sortable? (snoc head2 ls1) (remove head2 ls2))]))))

(define linearizable?
    (lambda (qls)
      (let*
	  ([tips (quanta-real-tips qls)]
	   [start (if
		      (null? tips)
		      (car qls)
		      (car (intersect
			    (lookup (car tips) *point-list*) qls)))])
	(if (< (length tips) 3)
	    (neighbor-sortable? (list start) (remove start qls))
	    #f))))

(define quantum-get-points
 (lambda (q)
   (lookup q *quanta-endpoints*)))

(define quanta-get-points
 (lambda (qls)
   (uniquify (apply append (map quantum-get-points qls)))))

(define remove
 (lambda (item ls)
   (cond
      ((null? ls) '())
      ((eq? item (car ls)) (remove item (cdr ls)))
      (else (cons (car ls) (remove item (cdr ls)))))))

(define find-uniques
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((member? (car ls) (cdr ls)) (find-uniques (remove (car ls) (cdr ls))))
      (else (cons (car ls)
		  (find-uniques (cdr ls)))))))

; gives the tips for a part
; will traverse around and always find a second "tip" for one-tip parts
(define quanta-get-tips
 (lambda (qls)
   (let
       ([the-points (quanta-to-points qls)])
   (list (car the-points) (tail the-points)))))

; only finds actual, free-flyin' tips
(define quanta-real-tips
 (lambda (qls)
   (find-uniques (apply append (map quantum-get-points qls)))))

;===========================================================================
; Conversions between quantum lists, point lists, turtle graphics, etc.
;===========================================================================

(define points-get-quantum
  (lambda (a b)
    (lookup-pair (list a b) (list b a) *quanta-endpoints*)))

(define points-to-quanta
  (lambda (pls)
    (if (< (length pls) 2) '()
	(cons (points-get-quantum (car pls) (cadr pls))
	      (points-to-quanta (cdr pls))))))     

; quanta path converted to point path
; input must BE a quanta path
(define quanta-to-points
 (lambda (qls)
   (cond
    [(null? qls) '()]
    [(eq? (length qls) 1) (quantum-get-points (car qls))]
    [else
     (let*
	 ([primed-list (map quantum-get-points qls)]
	  [starting-point (subtract (car primed-list) (cadr primed-list))])
       (point-convert starting-point primed-list))])))
     
(define point-convert
  (lambda (ls1 ls2)
    (let* ([head-two (car ls2)]
	   [neck-two (cadr ls2)]
	   [next-point (intersect head-two neck-two)]
	   [grab-next (append ls1 next-point)])
      (cond
       ((eq? (length ls2) 2)
	(append grab-next (subtract neck-two next-point)))
       (else (point-convert
	      grab-next
	      (cdr ls2)))))))

;===========================================================================

(set! *point-coords*
      '((1 (0 6)) (2 (0 5)) (3 (0 4)) (4 (0 3)) (5 (0 2)) (6 (0 1))
	(7 (0 0)) (8 (1 6)) (9 (1 5)) (10 (1 4)) (11 (1 3)) (12 (1 2))
	(13 (1 1)) (14 (1 0)) (15 (2 6)) (16 (2 5)) (17 (2 4))
	(18 (2 3)) (19 (2 2)) (20 (2 1)) (21 (2 0))))

(define point-coords
  (lambda (x)
    (lookup x *point-coords*)))

; conversions to compass directions

(define points-to-compass
  (lambda (plist)
    (let*
	([p1 (lookup (car plist) *point-coords*)]
	 [p2 (lookup (cadr plist) *point-coords*)]
	 [x1 (car p1)]
	 [y1 (cadr p1)]
	 [x2 (car p2)]
	 [y2 (cadr p2)]
	 [next-compass (coords-to-compass (- x2 x1) (- y2 y1))])
      (cond
       ((eq? (length plist) 2) (list next-compass))
       (else
	(cons next-compass
	      (points-to-compass (cdr plist))))))))

(define coords-to-compass
  (lambda (x y)
    (cond
     ((> x 0)
      (cond
       ((> y 0) '*NE*)
       ((< y 0) '*SE*)
       (else '*E*)))
     ((< x 0)
      (cond
       ((> y 0) '*NW*)
       ((< y 0) '*SW*)
       (else '*W*)))
     (else
      (cond
       ((> y 0) '*N*)
       ((< y 0) '*S*)
       (else '*no-move*))))))

(define quanta-to-compass
  (lambda (qls)
    (points-to-compass (quanta-to-points qls))))

;===========================================================================

; conversions to turtle graphics angles
; then all the inverse direction operations

(set! *compass-angles*
      '((*n* 0) (*ne* 45) (*e* 90) (*se* 135)
	(*s* 180) (*sw* 225) (*w* 270) (*nw* 315)))

; turning from an orientation of ang1 to an orientation of ang2
(define angle-diff
  (lambda (ang1 ang2)
    (let
	((temp (- 180 (- ang2 ang1))))
      (cond
       ((> temp 180) (- temp 360))
       ((< temp -180) (+ temp 360))
       (else temp)))))

(define compass-to-angles
  (lambda (clist)
    (let*
	([last-compass (lookup (car clist) *compass-angles*)]
	 [next-compass (lookup (cadr clist) *compass-angles*)]
	 [next-angle (angle-diff last-compass next-compass)])
      (cond
       ((eq? (length clist) 2) (list next-angle))
       (else
	(cons next-angle
	      (compass-to-angles (cdr clist))))))))

(define points-to-angles
  (lambda (plist)
    (compass-to-angles (points-to-compass plist))))

(define quanta-to-angles
  (lambda (qls)
    (points-to-angles (quanta-to-points qls))))

;===========================================================================

(define compass-to-coords
  (lambda (compass)
    (list
     (case compass
       [(*nw* *w* *sw*) -1]
       [(*n* *s*) 0]
       [(*ne* *e* *se*) 1])
     (case compass
       [(*nw* *n* *ne*) 1]
       [(*w* *e*) 0]
       [(*sw* *s* *se*) -1]))))

(define point-vector-coords
  (lambda (pt compass)
    (let
	([pt-coords (point-coords pt)]
	 [compass-coords (compass-to-coords compass)])
      (list (+ (car pt-coords) (car compass-coords))
	    (+ (cadr pt-coords) (cadr compass-coords))))))

(define point-vector-point
  (lambda (pt compass)
    (let
	([new-coords (point-vector-coords pt compass)])
      (if (and (eq? (car new-coords) (min 2 (max 0 (car new-coords))))
	       (eq? (cadr new-coords) (min 6 (max 0 (cadr new-coords)))))
	  (lookup new-coords (map pair-flip *point-coords*))
	  'off-grid))))

(define point-neighbors
  (lambda (pt)
    (let
	([point-neighbor
	  (lambda (compass)
	    (point-vector-point pt compass))])
      (remove 'off-grid (map point-neighbor *radial-order*)))))
  
;===========================================================================

; mirror-relativized turtle graphics
; care about angle measure, but may flip left and right
; (consistent reflection for all angles in a motif)
; we don't need to actually reify these
; use angle motifs and randomly decide to flip them all on the fly

;===========================================================================

; midpoint info
; find part-midpoint (point Euclideanly between tips)
; mid-part-point (point halfway along part between tips)

; halfway between two points
(define midpoint
  (lambda (ls)
    (list (average (list (caar ls) (caadr ls)))
	  (average (list (cadar ls) (cadadr ls))))))

; turns a list of two points into a list of their four coords
; (A B) -> (Ax Ay Bx By)
(define two-points-coords
  (lambda (p-ls)
    (let
	((coords (map point-coords p-ls)))
      (append (car coords) (cadr coords)))))

; finds point right between the tips
; needs to have part in ordered quanta list
(define part-midpoint
  (lambda (ls)
     (midpoint (map point-coords (head-n-tail (quanta-to-points ls))))))

;===========================================================================
; Curvature
; After much soul-searching, JAR, June 1998
;===========================================================================
; Curvature of a part (or half-part) will be defined in terms of the average
; distance of each point (junction of quanta) in the part from the line
; connecting the two tips of the part

; Heron's Theorem: Area = sqrt (s * (s-a) * (s-b) * (s-c))
; s = (a + b + c) / 2

; The distance is a / (b ^ 2), where b is the side bearing the two tips

(define dist-sq
  (lambda (a b x y)
    (+ (sqr (- a x)) (sqr (- b y)))))

; the distance squared
(define points-dist-sq
  (lambda (p1 p2)
    (apply dist-sq (two-points-coords (list p1 p2)))))

(define points-dist
  (lambda (p1 p2)
    (sqrt (apply dist-sq (two-points-coords (list p1 p2))))))

; just the distance from the endpoint-endpoint line, not the segment
(define point-line-error
  (lambda (end1 mid end2)
    (if
	(or (eq? end1 mid) (eq? end2 mid))
	0
	(let*
	  ([len-a-sq (points-dist-sq end1 mid)]
	   [len-b-sq (points-dist-sq end1 end2)]
	   [len-c-sq (points-dist-sq end2 mid)]
	   [len-a (sqrt len-a-sq)]
	   [len-b (sqrt len-b-sq)]
	   [len-c (sqrt len-c-sq)]
	   [len-s (/ (+ len-a len-b len-c) 2)])
	  (/ (sqrt
	     (* len-s (- len-s len-a) (- len-s len-b) (- len-s len-c)))
	(* 0.5 len-b len-b))))))

; curve sign
; veering right is positive, left is negative

(define curve-sign
  (lambda (end1 mid end2)
    (let*
	([end1-coord (point-coords end1)]
	 [end2-coord (point-coords end2)]
	 [mid-coord (point-coords mid)]
	 [end1-x (car end1-coord)]
	 [end1-y (cadr end1-coord)]
	 [end2-x (car end2-coord)]
	 [end2-y (cadr end2-coord)]
	 [mid-x (car mid-coord)]
	 [mid-y (cadr mid-coord)]
	 [ends-vertical? (= end1-x end2-x)]
	 [e2-above-e1 (truth-sign (< end1-y end2-y))]
	 [mid-right (truth-sign (> mid-x end1-x))]
	 [e1-right-e2 (truth-sign (> end1-x end2-x))]
	 [line-y
	  (if
	      ends-vertical?
	      0
	      (+ end1-y
		 (/ (* [- end2-y end1-y] [- mid-x end1-x])
		    [- end2-x end1-x])))]
	 [mid-above-line (truth-sign (> mid-y line-y))]
	 [vertical-case (* e2-above-e1 mid-right)]
	 [non-vert-case (* e1-right-e2 mid-above-line)])
      (if
	  ends-vertical?
	  vertical-case
	  non-vert-case))))

(define part-curve
  (lambda (q-ls)
    (if
	(eq? (length q-ls) 1)
	0
	(points-curve (quanta-to-points q-ls)))))

(define points-curve
  (lambda (p-ls)
    (if
	(< (length p-ls) 2)
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
    
; more than half if odd-numbered length
(define first-half
  (lambda (ls)
    (sublist 0 (- (round (/ (length ls) 2)) 1) ls)))

; more than half if odd-numbered length
(define second-half
  (lambda (ls)
    (let
	((len (length ls)))
      (sublist (- len (round (+ (/ len 2) 0.1))) (- len 1) ls))))

; tip matching

(define weighted-point
  (lambda (ls)
    (let
	((point (point-coords (car ls))))
      (list (* (cadr ls) (car point))
	    (* (cadr ls) (cadr point))))))

; will take results from either tip1-norm or tip2-norm
(define mean-point-norm
  (lambda (ls)
    (let
	((sum-x (apply + (map car (map weighted-point ls))))
	 (sum-y (apply + (map cadr (map weighted-point ls))))
	 (weight (apply + (map cadr ls))))
      (list (* 1.0 (/ sum-x weight)) (* 1.0 (/ sum-y weight))))))

(define tip1-norm
  (lambda (role)
    (cdaadr (caddr (cadr role)))))

(define tip2-norm
  (lambda (role)
    (cdaadr (cdaddr (cadr role)))))

; the part may be parsed in the opposite direction that
; the role is considered. we'll reverse it if needed

; breakable with very weird parts, like e-bowl
; may still want to try things both ways

(define reorder-part-to-role
  (lambda (q-ls role)
    (let*
	((part-tips (quanta-get-tips q-ls))
	 (part-tip1 (point-coords (car part-tips)))
	 (part-tip2 (point-coords (cadr part-tips)))
	 (role-tip1 (mean-point-norm (tip1-norm role)))
	 (role-tip2 (mean-point-norm (tip2-norm role))))
      (if
	  (> (+
	      (apply distance (append part-tip1 role-tip1))
	      (apply distance (append part-tip2 role-tip2)))
	     (+
	      (apply distance (append part-tip1 role-tip2))
	      (apply distance (append part-tip2 role-tip1))))
	  (reverse q-ls)
	  q-ls))))

;===========================================================================
; Cupped?
; Auxiliary to shape
; Just here for left-wing and right-wing, which should not be cupped
;===========================================================================

(define southwards
  (lambda (p)
    (x-wards p '*s*)))

(define northwards
  (lambda (p)
    (x-wards p '*n*)))

(define cupped?
  (lambda (qls)
    (let*
	([points (quanta-to-points qls)]
	 [tips (list (car points) (tail points))]
	 [mids (subtract points tips)])
      (and
       (not (null? mids))
       (>= (apply max (map southwards mids))
	   (apply max (map southwards tips)))))))

; if the part has two humps, on top or bottom
; doesn't allow tips to count as the humps, because (picture it)
; that's a part with one hump, not two
(define bactrian?
  (lambda (qls)
    (let*
	([points (quanta-to-points qls)]
	 [trim-one-end (cdr points)]
	 [trim-other-end (cdr (reverse points))])
      (or
       (and
	(udders? points)
	(or (udders? trim-one-end) (udders? trim-other-end)))
       (and
	(humps? points)
	(or (humps? trim-one-end) (humps? trim-other-end)))))))

(define udders? ; two minima on bottom
  (lambda (points)
    (let*
	([southmarks (map southwards points)]
	 [southmost (apply max southmarks)]
	 [trough (eat-up-to-item
		  southmost
		  (reverse (eat-up-to-item southmost southmarks)))]
	 [trough-peaks (remove southmost trough)])
      (> (length trough-peaks) 0))))

(define humps? ; two peaks on top
  (lambda (points)
    (let*
	([northmarks (map northwards points)]
	 [northmost (apply max northmarks)]
	 [plateau (eat-up-to-item
		   northmost
		   (reverse (eat-up-to-item northmost northmarks)))]
	 [plateau-dips (remove northmost plateau)])
      (> (length plateau-dips) 0))))

(define eat-up-to-item
  (lambda (item ls)
    (cond
     [(null? ls) '()]
     [(eq? (car ls) item) ls]
     [else (eat-up-to-item item (cdr ls))])))

; FINDING OUT IF OTHER STUFF EXISTS IN DIRECTION x FROM PART
; JAR 9/19/98

; how far is a point in the specified direction?
(define x-wards
  (lambda (p x)
    (let
	((coords (point-coords p)))
    (case x
      (*n* (cadr coords))
      (*ne* (+ (cadr coords) (car coords)))
      (*e* (car coords))
      (*se* (- (car coords) (cadr coords)))
      (*s* (- (cadr coords)))
      (*sw* (- (+ (cadr coords) (car coords))))
      (*w* (- (car coords)))
      (*nw* (- (cadr coords) (car coords)))))))

; most x-ward value of a part (in quanta)

(define x-wardmost
  (lambda (qls x)
    (if (null? qls) 0
	(let
	    ((x-ward-x
	      (lambda (p)
		(x-wards p x))))
	  (apply max (map x-ward-x (quanta-to-points qls)))))))

(define x-warder-neighbor
  (lambda (qls other x)
    (let
	((part-ward (x-wardmost qls x))
	 (other-ward (x-wardmost other x)))
      (cond
       ((> other-ward part-ward) 'y)
       ((< other-ward part-ward) 'n)
       (else 'eq)))))

(define neighborhood-watch
  (lambda (qls other)
    (if (or (null? qls) (null? other))
	'(n n n n n n n n)
	(let
	    ((check-x
	      (lambda (x)
		(x-warder-neighbor qls other x))))
	  (map check-x *radial-order*)))))

(define neighbor-watch-item-match
  (lambda (norm observed)
    (or
     (eq? norm 'dc) (eq? observed 'eq) (eq? norm observed))))

; Does the observed neighborhood match the pattern?
(define neighbor-watch-match
  (lambda (norm observed)
    (let
	([match-item
	  (lambda (norm-item observed-item)
	    (neighbor-watch-item-match norm-item observed-item))])
      (eval (cons 'and (map match-item norm observed))))))

(define neighbor-watch-match
  (lambda (norm observed)
    (cond
       [(null? norm) #t]
       [else
	(let
	    ([match-item
	      (or
	       (eq? (car norm) 'dc)
	       (eq? (car observed) 'eq)
	       (eq? (car norm) (car observed)))])
	  (and match-item
	       (neighbor-watch-match (cdr norm) (cdr observed))))])))


; mid-quanta touching
; two quanta can cross without sharing tips in common - e.g. #32 and #44
; GEM's code does not detect closure or touching that results from this
; happens to occur with diagonals differing by 12


(set! *mid-quanta-touch-list*
      '((32 44) (33 45) (34 46) (35 47) (36 48) (37 49) (38 50)
	(39 51) (40 52) (41 53) (42 54) (43 55) (44 32) (45 33)
	(46 34) (47 35) (48 36) (49 37) (50 38) (51 39) (52 40)
	(53 41) (54 42) (55 43)))

(define mid-quanta-touch?
  (lambda (qls1 qls2)
    (let
	((cross-here
	  (lambda (pair)
	    (and (member? (car pair) qls1) (member? (cadr pair) qls2)))))
      (eval (cons 'or (map cross-here *mid-quanta-touch-list*))))))

(define mid-quanta-touches
  (lambda (qls1 qls2)
    (let
	((cross-here
	  (lambda (pair)
	    (boolean-to-int
	     (and
	      (member? (car pair) qls1)
	      (member? (cadr pair) qls2))))))
      (apply + (map cross-here *mid-quanta-touch-list*)))))

; should appear EVERYWHERE closure is used
(define mid-quanta-closure?
  (lambda (qls)
    (mid-quanta-touch? qls qls)))

; because of the demented way parts are represented as joint-lists
; one-joint parts are a pair, bigger parts are lists of pairs
(define part-quanta
  (lambda (part)
    (let
	((jls (car part)))
      (if (atom? (car jls))
	  jls
	  (linearize (uniquify (apply append (car part))))))))

(define has-tips?
  (lambda (qls)
    (not (null? (quanta-real-tips qls)))))

(define part-tips?
  (lambda (part)
    (has-tips? (collapse (car part)))))

;----------------------------------------------------------------------
; Code for finding closure in a given set of quanta.
; Jim and Doug helped GEM write this.

(define make-graph
  (lambda (qls graph)
    (cond
     [(null? qls) graph]
     [else (make-graph (cdr qls)
		       (add-to-graph (car qls) graph))])))

(define add-to-graph
  (lambda (q gr)
    (letrec ([entry? (lambda (item gr)
		       (let ([ls (map car gr)])
			 (member? item ls)))]
	     [add-to-entry (lambda (endpts gr)
			     (cond
			      [(null? gr) '()]
			      [(= (car endpts) (caar gr))
			       (cons (list (car endpts) (snoc (cadr endpts)
							      (cadar gr)))
				     (add-to-entry endpts (cdr gr)))]
			      [else (cons (car gr)
					  (add-to-entry endpts (cdr gr)))]))])
      (let* ([endpts (lookup q *quanta-endpoints*)]
	     [backpts (reverse endpts)]
	     [new-gr (cond
		      [(entry? (car endpts) gr) (add-to-entry endpts gr)]
		      [else (cons (list (car endpts) (list (cadr endpts)))
				  gr)])])
	(cond
	 [(entry? (car backpts) new-gr) (add-to-entry backpts new-gr)]
	 [else (cons (list (car backpts) (list (cadr backpts)))
		     new-gr)])))))

(define search
  (lambda (g)
    (let ((visited '()))
      (call/cc
       (lambda (return)
	 (letrec ((search
		   (lambda (from-n)
		     (lambda (n)
		       (let* ((node (pick-node g n))
			      (links (remove from-n (cadr node))))
			 (if (member? n visited)
			     (return #t)
			     (begin (set! visited (cons n visited))
				    (map (search n) links)
				    #f)))))))
	   ((search -1) (caar g))))))))

(define pick-node
  (lambda (l n)
    (cond
     ((null? l) #f)
     ((= (caar l) n) (car l))
     (else (pick-node (cdr l) n)))))

; tips meet together -- nothing open
(define real-closure?
  (lambda (qls)
    (search (make-graph qls '()))))

; mid-quanta closure or real closure
(define any-closure?
  (lambda (qls)
    (or (mid-quanta-closure? qls)
	(search (make-graph qls '())))))

; JAR wrote these two helpers, 11/4/96
(define ascender-in-shape?
  (lambda (qls)
    (overlap*? qls
	       '(0 1 2 3 14 15 16 17 18 19 32 33 34 35 44 45 46 47))))

(define descender-in-shape?
  (lambda (qls)
    (overlap*? qls
	       '(10 11 12 13 26 27 28 29 30 31 40 41 42 43 52 53 54 55))))

(define parts-touches
  (lambda (qls1 qls2)
    (+
     (length (uniquify
	      (intersect 
	       (quanta-to-points qls1)
	       (quanta-to-points qls2))))
     (mid-quanta-touches qls1 qls2))))

(define parts-touch?
  (lambda (qls1 qls2)
    (if (or (null? qls1) (null? qls2)) #f
	(> (parts-touches qls1 qls2) 0))))

; NOTE: quanta-to-points is for linearly contiguous sets of quanta
; quanta-get-points is for possibly-disjoint sets of quanta
; in this code, qls1 is the part we're investigating, so it always
; takes quanta-to-points
; qls2 is the rest of the stuff on the grid, so it takes quanta-get-points

; doesn't count mid-quanta crossing
; only counts middle of qls1
(define mid-touches
  (lambda (qls1 qls2)
    (+
     (length (uniquify
	     (intersect 
	      (cdr (reverse (cdr (quanta-to-points qls1))))
	      (quanta-get-points qls2))))
     (mid-quanta-touches qls1 qls2))))

; touches by qls2 to qls1's tip1, middle, tip2
(define touching-pattern
  (lambda (qls1 qls2)
    (if
	(or (null? qls1) (null? qls2))
	'(#f 0 #f)
	(list
	 (member? (car (quanta-to-points qls1)) (quanta-get-points qls2))
	 (mid-touches qls1 qls2)
	 (member? (car (reverse (quanta-to-points qls1)))
		  (quanta-get-points qls2))))))

; to determine arity of points

(define num-spokes
  (lambda (pt)
    (length (intersect *quanta-list* (lookup pt *point-list*)))))

; to find points of arity 3 (for parsing e and k)
; has nothing to do with chemistry
; call (triple-points *point-list*)

(define triple-points
  (lambda (pt-ls)
    (cond
     [(null? pt-ls) '()]
     [(eq? 3 (num-spokes (caar pt-ls)))
      (cons (caar pt-ls) (triple-points (cdr pt-ls)))]
     [else (triple-points (cdr pt-ls))])))
     
(define quadruple-points
  (lambda (pt-ls)
    (cond
     [(null? pt-ls) '()]
     [(eq? 4 (num-spokes (caar pt-ls)))
      (cons (caar pt-ls) (quadruple-points (cdr pt-ls)))]
     [else (quadruple-points (cdr pt-ls))])))

(define halo-points
  (lambda (pt)
    (let
	([neighbors (point-neighbors pt)])
      (cons
       (list pt 500)
       (map list
	    neighbors (n-copies 200 (length neighbors)))))))

; -------------------------------------------------------------------

; 2-ended role flip

(define tip-flip
  (lambda (thing)
    (deep-substitute thing *tip-flip-list*)))

; turns are clockwise

(define compass-reflect-horiz
  (lambda (thing)
    (deep-substitute thing *compass-horiz-flip-list*)))

(define compass-reflect-vert
  (lambda (thing)
    (deep-substitute thing *compass-vert-flip-list*)))

(define compass-turn-180
  (lambda (thing)
    (deep-substitute thing *compass-180-list*)))

(define compass-turn-90
  (lambda (thing)
    (deep-substitute thing *compass-90-clockwise-list*)))

(define list-substitute
  (lambda (item ls)
    (let
	((subs (lookup-score item ls)))
      (if (eq? subs 0) item subs))))

(define deep-substitute
  (lambda (l sub-list)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cons (list-substitute (car l) sub-list)
	     (deep-substitute (cdr l) sub-list)))
      (else
       (cons (deep-substitute (car l) sub-list)
	     (deep-substitute (cdr l) sub-list))))))

(define left-edge
  (lambda (qls)
    (let
	([q-left
	  (lambda (q)
	    (lookup q *left-horiz-vals*))])
      (apply min (map q-left qls)))))

(define right-edge
  (lambda (qls)
    (let
	([q-right
	  (lambda (q)
	    (lookup q *right-horiz-vals*))])
      (apply max (map q-right qls)))))

(define roof-val
  (lambda (qls)
    (let
	([q-top
	  (lambda (q)
	    (lookup q *vert-hi-vals*))])
      (apply max (map q-top qls)))))

(define floor-val
  (lambda (qls)
    (let
	([q-bottom
	  (lambda (q)
	    (lookup q *vert-lo-vals*))])
      (apply min (map q-bottom qls)))))

(define angle-complement
  (lambda (theta)
    (- 180 theta)))

; rounds to integer... hope half-degrees aren't vital
(define angle-normalize
  (lambda (theta)
    (mod (round theta) 360)))

(define points-angle
  (lambda (p1 p2)
    (let
	([dx (- (car p2) (car p1))]
	 [dy (- (cadr p2) (cadr p1))])
      (if (eq? 0 dx)
	  (if (< 0 dy) 90.0 270.0)
	  (let*
	      ([slp (/ dy dx)]
	       [raw (round-3 (* (/ 180 3.14159) (atan slp)))]
	       [quadrant
		(cond
		 [(and (< dx 0) (>= dy 0)) 2]
		 [(and (< dx 0) (< dy 0)) 3]
		 [(and (> dx 0) (< dy 0)) 4]
		 [else 1])])
	    (case quadrant
	      [1 raw]
	      [2 (+ 180 raw)]
	      [3 (+ 180 raw)]
	      [4 (+ 360 raw)]))))))

; direction is left or right
; p1 should be the center of a circle/arc
(define tangent-heading
  (lambda (p1 p2 direction)
    (+ (points-angle p1 p2)
       (if (eq? direction 'right) -90 90))))

(set! *tip-flip-list*
      '((*n* *s*)
	(*ne* *sw*)
	(*e* *w*)
	(*se* *nw*)
	(*s* *n*)
	(*sw* *ne*)
	(*w* *e*)
	(*nw* *se*)
	(*full-right* *full-left*)
	(*full-left* *full-right*)
	(*strong-right* *strong-left*)
	(*square-right* *square-left*)
	(*slight-right* *slight-left*)
	(*strong-left* *strong-right*)
	(*square-left* *square-right*)
	(*slight-left* *slight-right*)
	(curve1 curve2)
	(curve2 curve1)
	(t1 t2)
	(t2 t1)
	(t1m mt2)
	(mt2 t1m)))

(set! *compass-horiz-flip-list*
      '((*ne* *nw*)
	(*e* *w*)
	(*se* *sw*)
	(*sw* *se*)
	(*w* *e*)
	(*nw* *ne*)))

(set! *compass-vert-flip-list*
      '((*n* *s*)
	(*se* *ne*)
	(*ne* *se*)
	(*s* *n*)
	(*nw* *sw*)
	(*sw* *nw*)))

(set! *compass-90-clockwise-list*
      '((*n* *e*)
	(*ne* *se*)
	(*e* *s*)
	(*se* *sw*)
	(*s* *w*)
	(*sw* *nw*)
	(*w* *n*)
	(*nw* *ne*)))

(set! *compass-180-list*
      '((*n* *s*)
	(*ne* *sw*)
	(*e* *w*)
	(*se* *nw*)
	(*s* *n*)
	(*sw* *ne*)
	(*w* *e*)
	(*nw* *se*)))

(set! *horiz-flip-list*
      '((0 1) (1 0) (2 3) (3 2) (4 5) (5 4) (6 7) (7 6) (8 9) (9 8)
	(10 11) (11 10) (12 13) (13 12) (14 16) (15 15) (16 14) (17 19)
	(18 18) (19 17) (20 22) (21 21) (22 20) (23 25) (24 24) (25 23)
	(26 28) (27 27) (28 26) (29 31) (30 30) (31 29) (32 45) (33 44)
	(34 47) (35 46) (36 49) (37 48) (38 51) (39 50) (40 53) (41 52)
	(42 55) (43 54) (44 33) (45 32) (46 35) (47 34) (48 37) (49 36)
	(50 39) (51 38) (52 41) (53 40) (54 43) (55 42)))

(set! *vert-flip-list*
   '((0 12) (1 13) (2 10) (3 11) (4 8) (5 9) (6 6) (7 7) (8 4) (9 5)
     (10 2) (11 3) (12 0) (13 1) (14 29) (15 30) (16 31) (17 26) (18 27)
     (19 28) (20 23) (21 24) (22 25) (23 20) (24 21) (25 22) (26 17)
     (27 18) (28 19) (29 14) (30 15) (31 16) (32 54) (33 55) (34 52)
     (35 53) (36 50) (37 51) (38 48) (39 49) (40 46) (41 47) (42 44)
     (43 45) (44 42) (45 43) (46 40) (47 41) (48 38) (49 39) (50 36)
     (51 37) (52 34) (53 35) (54 32) (55 33)))

; rotates grid 180 degrees around its center

(set! *180-turn-list*
   '((0 13) (1 12) (2 11) (3 10) (4 9) (5 8) (6 7) (7 6) (8 5) (9 4)
     (10 3) (11 2) (12 1) (13 0) (14 31) (15 30) (16 29) (17 28) (18 27)
     (19 26) (20 25) (21 24) (22 23) (23 22) (24 21) (25 20) (26 19)
     (27 18) (28 17) (29 16) (30 15) (31 14) (32 43) (33 42) (34 41)
     (35 40) (36 39) (37 38) (38 37) (39 36) (40 35) (41 34) (42 33)
     (43 32) (44 55) (45 54) (46 53) (47 52) (48 51) (49 50) (50 49)
     (51 48) (52 47) (53 46) (54 45) (55 44)))

; rotates grid 180 degrees around the center of the
; central zone + ascender zone

(set! *180-high-turn-list*
      '((0 9) (1 8) (2 7) (3 6) (4 5) (5 4) (7 2) (8 1) (9 0) (14 25)
	(17 22) (20 19) (23 16) (25 14) (22 17) (19 20) (16 23) (32 39)
	(34 37) (36 35) (38 33) (39 32) (37 34) (35 36) (33 38) (44 51)
	(46 49) (48 47) (50 45) (51 44) (49 46) (47 48) (45 50) (15 24)
	(18 21) (21 18) (24 15)))

; quanta-list to bit vector
; 7/22/99
; should have done this long ago

(define quanta-into-bits
  (lambda (q-ls bits)
    (let
	    ([pos (length bits)])
      (if (null? q-ls)
	  (if (> pos 55)
	      bits
	      (append bits (n-copies 0 (- 56 pos))))
	(if (member? pos q-ls)
	      (quanta-into-bits (remove-item pos q-ls)
				(append bits '(1)))
	      (quanta-into-bits (remove-item pos q-ls)
				(append bits '(0))))))))

(define canonicalize-q-ls
  (lambda (q-ls)
    (q-list (quanta-into-bits q-ls '()) 0)))

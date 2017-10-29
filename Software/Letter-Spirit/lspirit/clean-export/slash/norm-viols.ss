;===========================================================================
; norm-viols.ss : code for computing norm violations
;===========================================================================
; was labels.ss
; JAR working on, March 1998

;===========================================================================
; GENERAL norm-violation code
;===========================================================================

(set!
 *nv-types*
 '(Height Width Weight Left-Edge Right-Edge Roof Floor Curve Touch Tips))

; the order in these lists is used to compare heights

(set! *heights-list* '(no-height very-short short medium-ht tall very-tall))
(set! *widths-list* '(skinny half-wide wide))
(set! *weights-list* '(very-light light medium-wt heavy huge))

(set! *left-edge-list* '(l-edge-rt l-edge-md l-edge-lf))
(set! *right-edge-list* '(r-edge-lf r-edge-md r-edge-rt))
(set! *roof-list* '(roof-bottom roof-middown roof-baseline roof-midline
				roof-x-height roof-t-height roof-top))
(set! *floor-list* '(floor-top floor-t-height floor-x-height floor-midline
			       floor-baseline floor-middown floor-bottom))
(set! *left-curve-list*
      '(*straight*
	*slight-left* *square-left* *strong-left* *full-left* *closure*))
(set! *right-curve-list*
      '(*straight*
	*slight-right* *square-right* *strong-right* *full-right* *closure*))

(set! *curves-list*
      '(*full-left* *strong-left* *square-left* *slight-left*
	*straight*
	*slight-right* *square-right* *strong-right* *full-right* *closure*))


; called by the comparer codelet which picks at random one of the types from
; *nv-types*
(define compare-nv-type
  (lambda (part nvtype)
    (let* ([quanta (cadr part)]
	   [rolename (car part)]
	   [norms (get-norms (eval rolename))])
      (case nvtype
	[Height
	 (list-nv-compare
	  (height-label (height quanta))
	  (best-norm *heights-list* norms)
	  *heights-list*)]
	[Width
	 (list-nv-compare
	  (width-label (width quanta))
	  (best-norm *widths-list* norms)
	  *widths-list*)]
	[Weight
	 (list-nv-compare
	  (weight-label (weight-qls quanta))
	  (best-norm *weights-list* norms)
	  *weights-list*)]
	[Roof
	 (list-nv-compare
	  (roof-label (roof-val quanta))
	  (best-norm *roof-list* norms)
	  *roof-list*)]
	[Floor
	 (list-nv-compare
	  (floor-label (floor-val quanta))
	  (best-norm *floor-list* norms)
	  *floor-list*)]
	[Left-edge
	 (list-nv-compare
	  (apply left-edge-label (horiz-touches quanta))
	  (best-norm *left-edge-list* norms)
	  *left-edge-list*)]
	[Right-edge
	 (list-nv-compare
	  (apply right-edge-label (horiz-touches quanta))
	  (best-norm *right-edge-list* norms)
	  *right-edge-list*)]
	[Curve
	 (let
	     ([type (lookup 'topology (eval (car part)))])
	   (if (or (eq? type 'dot)
		   (eq? type 'loop))
	       'same
	       (let*
		   ([crv-norms
		     (if (eq? type 'bisegment)
			 (append (car (lookup-list 'curve1 norms))
				 (car (lookup-list 'curve2 norms)))
			 (car (lookup-list 'curve norms)))]
		    [first-crv (part-curve (first-half quanta))]
		    [second-crv (part-curve (second-half quanta))]
		    [correct-label
		     (if (eq? type 'bisegment)
			 (if (> (abs first-crv) (abs second-crv))
			     (curve-label (first-half quanta))
			     (curve-label (second-half quanta)))
			 (curve-label quanta))])
		 (curve-compare
		  correct-label
		  (find-max crv-norms)
		  *left-curve-list* *right-curve-list*))))]
	   [Touch
	    (touch-compare
	     (contact-label quanta)
	     (role-touch-norm rolename))]
	   [Tips
	    (let
		([type (lookup 'topology (eval (car part)))])
	      (if (or (eq? type 'dot)
		      (eq? type 'loop))
		  'none
		  (let*
		      ([pick (cointoss)]
		       [which-tip (if pick 'tip1 'tip2)]
		       [norm-tips (map find-max
				       (map cdar (lookup-list 'tips norms)))]
		       [part-tips (quanta-get-tips quanta)]
		       [norm-tip (if pick
				     (car norm-tips) (cadr norm-tips))]
		       [part-tip (if pick
				     (car part-tips) (cadr part-tips))])
		    (list (tip-type rolename which-tip)
			  (compare-tip norm-tip part-tip)))))]
	   [else (error 'nvtype "no such norm type ~s~%" nvtype)]))))

(define val-to-val-nv-type
  (lambda (part nvtype)
    (let* ([quanta (cadr part)]
	   [rolename (car part)]
	   [norms (get-norms (eval rolename))])
      (case nvtype
	[Height
	 (list
	  (best-norm *heights-list* norms)
	  (height-label (height quanta)))]
	[Width
	 (list
	  (best-norm *widths-list* norms)
	  (width-label (width quanta)))]
	[Weight
	 (list
	  (best-norm *weights-list* norms)
	  (weight-label (weight-qls quanta)))]
	[Roof
	 (list
	  (best-norm *roof-list* norms)
	  (roof-label (roof-val quanta)))]
	[Floor
	 (list
	  (best-norm *floor-list* norms)
	  (floor-label (floor-val quanta)))]
	[Left-edge
	 (list
	  (best-norm *left-edge-list* norms)
	  (apply left-edge-label (horiz-touches quanta)))]
	[Right-edge
	 (list
	  (best-norm *right-edge-list* norms)
	  (apply right-edge-label (horiz-touches quanta)))]
	[Curve
	 (let
	     ([type (lookup 'topology (eval (car part)))])
	   (if (or (eq? type 'dot)
		   (eq? type 'loop))
	       '(dont-care dont-care)
	       (let*
		   ([crv-norms
		     (if (eq? type 'bisegment)
			 (append (car (lookup-list 'curve1 norms))
				 (car (lookup-list 'curve2 norms)))
			 (car (lookup-list 'curve norms)))]
		    [first-crv (part-curve (first-half quanta))]
		    [second-crv (part-curve (second-half quanta))]
		    [correct-label
		     (if (eq? type 'bisegment)
			 (if (> (abs first-crv) (abs second-crv))
			     (curve-label (first-half quanta))
			     (curve-label (second-half quanta)))
			 (curve-label quanta))])
		 (list
		  correct-label
		  (find-max crv-norms)))))]
	[Touch
	 (list
	  (contact-label quanta)
	  (role-touch-norm rolename))]
	[Tips
	 (let
	     ([type (lookup 'topology (eval (car part)))])
	   (if (or (eq? type 'dot)
		   (eq? type 'loop))
	       'none
	       (let*
		   ([pick (cointoss)]
		    [which-tip (if pick 'tip1 'tip2)]
		    [norm-tips (map find-max
				    (map cdar (lookup-list 'tips norms)))]
		    [part-tips (quanta-get-tips quanta)]
		    [norm-tip (if pick
				  (car norm-tips) (cadr norm-tips))]
		    [part-tip (if pick
				  (car part-tips) (cadr part-tips))])
		 (list (tip-type rolename which-tip)
		       (list norm-tip part-tip)))))]
	[else (error 'nvtype "no such norm type ~s~%" nvtype)]))))

(define best-norm
   (lambda (type-ls norms)
     (find-max (norms-of-type type-ls norms))))

(define norm-of-type?
  (lambda (item type-ls)
    (member? (car item) type-ls)))

(define norms-of-type
  (lambda (type-ls norms)
    (cond
     [(null? norms) '()]
     [(norm-of-type? (car norms) type-ls)
      (cons (car norms) (norms-of-type type-ls (cdr norms)))]
     [else (norms-of-type type-ls (cdr norms))])))

; makes a comparison based on the position of labels in a list
(define list-nv-compare
  (lambda (x y ls)
    (let ([x-ord (order x ls)]
	  [y-ord (order y ls)])
    (cond
     ((eq? (* x-ord y-ord) 0) 'no-comparison)
     ((eq? x-ord y-ord) 'same)
     ((> x-ord y-ord) 'more)
     (else 'less)))))

(define curve-compare
  (lambda (x y r-ls l-ls)
    (let ([x-ord (max (order x r-ls) (order x l-ls))]
	  [y-ord (max (order y r-ls) (order y l-ls))])
    (cond
     [(eq? (* x-ord y-ord) 0) 'no-comparison]
     [(and (eq? x-ord y-ord)
	   (eq? x y)) 'same]
     [(eq? x-ord y-ord) 'flipped]     
     [(> x-ord y-ord) 'more]
     [else 'less]))))

(define comparative-label
  (lambda (comparative mode)
    (case mode
      [Height
       (case comparative
	 [less 'shorter]
	 [same 'same-height]
	 [more 'taller])]
       [Width
	(case comparative
	  [less 'skinnier]
	  [same 'same-width]
	  [more 'wider])]
       [Weight
	(case comparative
	  [less 'lighter]
	  [same 'same-weight]
	  [more 'heavier])]
       [Left-Edge
	(case comparative
	  [less 'left-shift]
	  [same 'same-horizontal]
	  [more 'right-shift])]
       [Right-Edge
	(case comparative
	  [less 'left-shift]
	  [same 'same-horizontal]
	  [more 'right-shift])]
       [Roof
	(case comparative
	  [less 'lower-roof]
	  [same 'same-roof]
	  [more 'raise-roof])]
       [Floor
	(case comparative
	  [less 'lower-floor]
	  [same 'same-floor]
	  [more 'raise-floor])])))

(define height-label
  (lambda (int)
    (case int
      (0 'no-height)
      (1 'very-short)
      (2 'short)
      (3 'medium-ht)
      (4 'tall)
      (else 'very-tall))))

(define width-label
  (lambda (int)
    (case int
      (0 'skinny)
      (1 'half-wide)
      (else 'wide))))

(define weight-quantum
  (lambda (int)
    (if (< int 32) 1 1.4142)))

(define weight-qls
  (lambda (qls)
    (apply + (map weight-quantum qls))))

(define weight-label
  (lambda (int)
    (cond
      ((between? int 1 1.5) 'very-light)
      ((between? int 1.5 2.9) 'light)
      ((between? int 2.9 5) 'medium-wt)
      ((between? int 5 8) 'heavy)
      (else 'huge))))

(define curve-label
  (lambda (qls)
    (if (real-closure? qls)
	'*closure*
	(let ([n (part-curve qls)])
	  (cond
	   ((between? n -0.05 0.05) '*straight*)
	   ((between? n 0.051 0.15) '*slight-right*)
	   ((between? n 0.151 0.251) '*square-right*)
	   ((between? n 0.251 0.381) '*strong-right*)
	   ((> n 0.381) '*full-right*)
	   ((between? n -0.151 -0.05) '*slight-left*)
	   ((between? n -0.251 -0.151) '*square-left*)
	   ((between? n -0.381 -0.251) '*strong-left*)
	   ((< n -0.381) '*full-left*)
	   (else '*weird-curve*))))))

(define shape-label
  (lambda (qls)
    (let
	([closure? (real-closure? qls)]
	 [mid-q-closure? (mid-quanta-closure? qls)])
      (cond
       [(or mid-q-closure? (and (has-tips? qls) closure?))
	'*spiky-closure*]
       [closure? '*closure*]
       [(bactrian? qls) '*bactrian*] ; to keep w from being called u
       [(cupped? qls) '*cupped*]     ; to keep w from being called v
       [else '*simple*]))))     

(define floor-label
  (lambda (n)
    (case n
      [0 'floor-bottom]
      [1 'floor-middown]
      [2 'floor-baseline]
      [3 'floor-midline]
      [4 'floor-x-height]
      [5 'floor-t-height]
      [6 'floor-top]
      [else 'error])))

(define roof-label
  (lambda (n)
    (case n
      [0 'roof-bottom]
      [1 'roof-middown]
      [2 'roof-baseline]
      [3 'roof-midline]
      [4 'roof-x-height]
      [5 'roof-t-height]
      [6 'roof-top]
      [else 'error])))


;===========================================================================
; HORIZONTAL NV code
;===========================================================================

(define horiz-label-value
  (lambda (label)
    (case label
      ['left   0]
      ['middle 1]
      ['right  2]
      [else   'error])))

(set! *left-quanta*
      '(0 2 4 6 8 10 12 14 17 20 23 26 29 32 34 36 38 40 42 44 46 48 50 52 54))
(set! *middle-quanta*
      '(0 2 4 6 8 10 12 32 34 36 38 40 42 44 46 48 50 52 54 1 3 5 7 9 11 13
	  33 35 37 39 41 43 45 47 49 51 53 55 15 18 21 24 27 30))
(set! *right-quanta*
      '(1 3 5 7 9 11 13 16 19 22 25 28 31 33 35 37 39 41 43 45 47 49 51 53 55))

; given a list of a subset of (left middle right), returns the "rightness"
; 0=left, 2=right

(define horiz-labels-value
 (lambda (ls)
   (average (map horiz-label-value ls))))

(define right-edge-label
  (lambda (lf? md? rt?)
    (if
	rt?
	`r-edge-rt
	(if
	    md?
	    `r-edge-md
	    `r-edge-lf))))

(define left-edge-label
  (lambda (lf? md? rt?)
    (if
	lf?
	`l-edge-lf
	(if
	    md?
	    `l-edge-md
	    `l-edge-rt))))

(define horiz-quanta-value
  (lambda (qls)
    (let* ([touchlist '()]
	   [touchlist (if (overlap? qls *left-quanta*)
			  (cons '0 touchlist) touchlist)]
	   [touchlist (if (overlap? qls *middle-quanta*)
			  (cons '1 touchlist) touchlist)]
	   [touchlist (if (overlap? qls *right-quanta*)
			  (cons '2 touchlist) touchlist)])
      (average touchlist))))


(define compare-horiz
  (lambda (qls label-list)
    (let* ([filler (horiz-quanta-value qls)]
	   [norm (horiz-labels-value label-list)])
      (cond
       ((eq? filler norm) 'same)
       ((> filler norm) 'more)
       (else 'less)))))

;===========================================================================
; VERTICAL NV code (floor, roof)
;===========================================================================

(define vert-label-value
  (lambda (label)
    (case label
      ['bottom   0]
      ['hi-desc  1]
      ['right  2]
      [else   'error])))

(set! *vertical-values*
      '((ascender            (5 6))
	(bottom              (0))
	(descender           (0 1))
	(top                 (6))
	(x-zone              (2 3 4))
	(baseline--bottom    (0 1 2))
	(baseline--midline   (2 3))
	(baseline--t-height  (2 3 4 5))
	(baseline--top       (2 3 4 5 6))
	(baseline--x-height  (2 3 4))
	(midline--x-height   (3 4))
	(x-height--bottom    (0 1 2 3 4))
	(x-height--top       (2 3 4 5 6))))

(set! *vert-quanta*
      '((0 (12 13 29 42 54 30 43 55 31))
	(1 (10 11 29 42 54 30 43 55 31 26 40 52 27 41 53 28))
	(2 (8 9 26 40 52 27 41 53 28 23 38 50 24 39 51 25))
	(3 (6 7 23 38 50 24 39 51 25 20 36 48 21 37 49 22))
	(4 (4 5 20 36 48 21 37 49 22 17 34 46 18 35 47 19))
	(5 (2 3 17 34 46 18 35 47 19 14 32 44 15 33 45 16))
	(6 (0 1 14 32 44 15 33 45 16))))

(define touches-vert
 (lambda (i qls)
   (if (overlap? qls (lookup i *vert-quanta*))
       (list i) nil)))

; gives the vertical values touched by the quanta list
(define quanta-verts
  (lambda (qls)
    (append (touches-vert 0 qls) (touches-vert 1 qls)
	  (touches-vert 2 qls) (touches-vert 3 qls)
	  (touches-vert 4 qls) (touches-vert 5 qls)
	  (touches-vert 6 qls))))

(define label-verts
  (lambda (label)
    (lookup label *vertical-values*)))

; gives verts touched by a bunch of labels
(define labels-verts
  (lambda (vlabels)
    (cond
     ((null? vlabels) nil)
     (else (append (label-verts (car vlabels))
		 (labels-verts (cdr vlabels)))))))

; Now we just need to compare max and min of labels-verts and
; verts-touched by the norms and quanta of a role!

(define compare-roof
  (lambda (qls label-list)
    (let* ([filler (apply max (quanta-verts qls))]
	   [norm (apply max (labels-verts label-list))])
      (cond
       ((eq? filler norm) 'same)
       ((> filler norm) 'more)
       (else 'less)))))

(define compare-floor
  (lambda (qls label-list)
    (let* ([filler (apply min (quanta-verts qls))]
	   [norm  (apply min (labels-verts label-list))])
      (cond
       ((eq? filler norm) 'same)
       ((> filler norm) 'more)
       (else 'less)))))

;===========================================================================
; TIP NV code
;===========================================================================

(define compare-tip
  (lambda (norm-tip part-tip)
    (if (cointoss)
	(zoom-tip-NV norm-tip part-tip)
	(car (points-to-compass
	      (list norm-tip part-tip))))))

; computes tip NV based on whether point is drawn inwards
; or pushed outwards from the center of the grid

(define zoom-tip-NV
  (lambda (norm-tip part-tip) ; length 2: norm-tip then part-tip
    (case (sgn (- (points-dist-sq norm-tip 11)
		  (points-dist-sq part-tip 11)))
      [-1 '*outward*]
      [1 '*inward*]
      [0 '*no-move*])))

; used to have plain "tip" in every list, but it's too generic
; I'm putting it back, 7/13/99
(define tip-types
  (lambda (rolename which-tip)
    (let*
	([roletype (lookup 'stroke (eval rolename))]
	 [tip1? (eq? which-tip 'tip1)])
      (case roletype
	[down (if tip1?
		  '(tip up-tip)
		  '(tip down-tip))]
	[downright (if tip1?
		       '(tip up-tip left-tip up-left-tip)
		       '(tip down-tip right-tip down-right-tip))]
	[right (if tip1?
		   '(tip left-tip)
		   '(tip right-tip))]
	[upright (if tip1?
		     '(tip down-tip left-tip down-left-tip)
		     '(tip up-tip right-tip up-right-tip))]))))

(define tip-type
  (lambda (rolename which-tip)
    (roulette (tip-types rolename which-tip))))

; 4 types of roles, by tip order and position
; top->bottom, left->right, the two slash types

;===========================================================================
; R-ROLE NV code (touch)
;===========================================================================

; now that we've committed to knowing the answer from the Examiner,
; we have the quanta in correct tip1->tip2 order

(define role-touch-norm
  (lambda (role)
    (find-max (touch-norms *answer* role))))

; src expected, but trg seen
(define touch-compare
  (lambda (trg src)
    (if (eq? src trg) 'same
	(if (eq? trg 'all-over)
	    'more
	    (if (eq? trg 'nc)
		'less
		(case src
		  [nc 'more]
		  [all-over 'less]
		  [c 'diff]
		  [t1 (case trg
			[m 'into]
			[2ts 'plus-t]
			[t1m 'plus-m]
			[else 'diff])]
		  [m (case trg
		       [t1 'out-of]
		       [t2 'out-of]
		       [t1m 'plus-t]
		       [mt2 'plus-t]
		       [2ms 'plus-m]
		       [else 'diff])]
		  [t2 (case trg
			[m 'into]
			[2ts 'plus-t]
			[mt2 'plus-m]
			[else 'diff])]
		  [2ts (case trg
			 [m 'diff]
			 [t1 'minus-t]
			 [t2 'minus-t]
			 [else 'into])]
		  [t1m (case trg
			 [t1 'minus-m]
			 [m 'minus-t]
			 [2ts 'out-of]
			 [2ms 'into]
			 [else 'diff])]
		  [mt2 (case trg
			 [m 'minus-t]
			 [t2 'minus-m]
			 [2ts 'out-of]
			 [2ms 'into]
			 [else 'diff])]
		  [2ms (case trg
			 [m 'minus-m]
			 [t1 'diff]
			 [t2 'diff]
			 [else 'out-of])]))))))

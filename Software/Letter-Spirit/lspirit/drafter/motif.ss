;===========================================================================
; Motif.ss : code for computing motifs
;===========================================================================
; JAR working on, April 1998

;===========================================================================
; GENERAL motif code
;===========================================================================

(set! *motif-types* '(literal translate turn-180 turn-90 turn-45))
(set! *motif-lists*
      '(*literal-motifs* *translate-motifs* *turn-180-motifs*
			 *turn-90-motifs* *turn-45-motifs*))

; called by the worm codelet which picks at random one of the types from
; *motif-types*

; changed worm to snake (can eat its own tail) 2/14/99

(define worm-type
  (lambda (mtftype)
    (let
	([path (canonicalize-loops (lazy-snake-crawl *quanta-list*))])
      (case mtftype
	[literal path]
	[(translate turn-180 turn-90) (quanta-to-compass path)]
	[turn-45
	 (if
	     (> (length path) 1)
	     (quanta-to-angles path)
	     '(no-angle))]
	[else (error 'mtftype "no such motif type ~s~%" mtftype)]))))

; randomly roots around a gridfigure until it hits a dead end
; works so well it's embarrassing

(define worm-crawl
  (lambda (qlist)
    (let
	([start (roulette qlist)])
      (neighbor-random (list start) (remove start qlist)))))

(define neighbor-random
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
       (deadend ls1)
       (else (neighbor-random (snoc head2 ls1) (remove head2 ls2)))))))

; crawls around randomly, but may stop at any time, or at a dead end
(define lazy-worm-crawl
  (lambda (qlist)
    (let
	([start (roulette qlist)])
      (lazy-neighbor-random (list start) (remove start qlist)))))

; may just stop sampling at any time
(define lazy-neighbor-random
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
	   [deadend
	    (or
	     (and (eq? (n-sided-die 5) 5) (> (length ls1) 1))
	     (null? ls2)
	     (null? options))]
	   [head2
	    (if (not deadend) (roulette options) nil)])
      (cond
       (deadend ls1)
       (else (lazy-neighbor-random (snoc head2 ls1) (remove head2 ls2)))))))

; crawls around randomly, but may stop at any time, or at a dead end
(define lazy-snake-crawl
  (lambda (qlist)
    (let
	([start (roulette qlist)])
      (lazy-neighbor-random-rehash (list start) qlist))))

; may just stop sampling at any time

; PROBLEM: start on spur, run through loop, re-enter spur
; if last quantum and both of the first two quanta all have one
; point in common, stop right there

(define lazy-neighbor-random-rehash
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
	   [deadend
	    (or
	     (and (eq? (n-sided-die 12) 6) (> (length ls1) 1))
	     (if (> (length ls1) 2)
		 (not (null? (intersect
			      (quantum-get-points tail1)
			      (intersect
			       (quantum-get-points (car ls1))
			       (quantum-get-points (cadr ls1))))))
		 #f)
	     (null? ls2)
	     (null? options))]
	   [head2
	    (if (not deadend) (roulette options) nil)])
      (cond
       (deadend ls1)
       (else (lazy-neighbor-random-rehash (snoc head2 ls1)
					  (remove head2 ls2)))))))

(define turn-180-expand
  (lambda (motif-ls)
    (let
	([reflect-horiz (map compass-reflect-horiz motif-ls)])
      (append
       motif-ls
       reflect-horiz
       (map compass-reflect-vert motif-ls)
       (map compass-reflect-vert reflect-horiz)))))

(define turn-90-expand
  (lambda (motif-ls)
    (let*
	([turn-90 (map compass-turn-90 motif-ls)]
	 [turn-180 (map compass-turn-90 turn-90)]
	 [turn-270 (map compass-turn-90 turn-180)]
	 [all-turns (append motif-ls turn-90 turn-180 turn-270)])
      (append
       all-turns
       (map compass-reflect-horiz all-turns)))))

(define angle-mirror
  (lambda (angle-ls)
    (map negate angle-ls)))

(define turn-45-expand
  (lambda (motif-ls)
    (append motif-ls (map angle-mirror motif-ls))))

(define motif-sublists
  (lambda (sp-type motif-ls)
    (case sp-type
      [*literal-motifs*
       (append (sublists motif-ls)
	       (sublists (reverse motif-ls)))]
      [*translate-motifs*
       (append (sublists motif-ls)
	       (sublists (compass-turn-180 (reverse motif-ls))))]
      [*turn-180-motifs*
       (turn-180-expand
	(append (sublists motif-ls)
		(sublists (compass-turn-180 (reverse motif-ls)))))]
      [*turn-90-motifs*
       (turn-90-expand
	(append (sublists motif-ls)
		(sublists (compass-turn-90 (reverse motif-ls)))))]
      [*turn-45-motifs*
       (let
	   ([clean-ls (remove-item 'no-angle motif-ls)])
	 (append (sublists clean-ls)
		 (sublists (map - (reverse clean-ls)))))])))

(define map-reverse
  (lambda (ls-ls)
    (append ls-ls (map reverse ls-ls))))

(define map-180-turn
  (lambda (ls-ls)
    (append ls-ls (map compass-turn-180 ls-ls))))

(define map-90-turn
  (lambda (ls-ls)
    (append ls-ls (map compass-turn-90 ls-ls))))

(define map-negate
  (lambda (ls-ls)
    (append ls-ls (map negate ls-ls))))

; sublists cache
; for efficiency reasons, better to cache motif sublists
; once per run of Adjudicator or Drafter

(define clear-sublist-cache
  (lambda ()
    (set! *sublist-cache* '())))

(define cache-motif-sublists
  (lambda (sp-type mtf sublists)
    (set! *sublist-cache* (append *sublist-cache*
				   (list
				    (list
				     (list sp-type mtf) sublists))))))

; finds the answer, caching it if it has to derive it from scratch
(define find-motif-sublists
  (lambda (sp-type mtf)
    (let
	([cache-try (lookup-score (list sp-type mtf) *sublist-cache*)])
      (if
	  (eq? 0 cache-try)
	  (let
	      ([answer (uniquify (motif-sublists sp-type mtf))])
	    (cache-motif-sublists sp-type mtf answer)
	    answer)
	  cache-try))))

; (define find-motif-sublists
;  (lambda (sp-type mtf)
;    (motif-sublists sp-type mtf)))

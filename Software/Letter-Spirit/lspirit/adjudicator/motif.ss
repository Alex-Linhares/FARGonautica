;===========================================================================
; Motif.ss : code for computing motifs
;===========================================================================
; JAR working on, April 1998

;===========================================================================
; GENERAL motif code
;===========================================================================

(set! *motif-types* '(literal translate turn-180 turn-90 turn-45))

; called by the worm codelet which picks at random one of the types from
; *motif-types*

; changed worm to snake (can eat its own tail) 2/14/99

(define worm-type
  (lambda (mtftype)
    (let
	([path (lazy-snake-crawl *quanta-list*)])
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
	     (and (eq? (n-sided-die 6) 6) (> (length ls1) 1))
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

(define motif-expand
  (lambda (sp-type motif-ls)
    (case sp-type
      [*literal-motifs* motif-ls]
      [*translate-motifs* motif-ls]
      [*turn-180-motifs* (turn-180-expand motif-ls)]
      [*turn-90-motifs* (turn-90-expand motif-ls)]
      [*turn-45-motifs* (turn-45-expand motif-ls)])))

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
       (append (sublists motif-ls)
	       (sublists (compass-turn-180 (reverse motif-ls))))]
      [*turn-90-motifs*
       (append (sublists motif-ls)
	       (sublists (compass-turn-180 (reverse motif-ls))))]
      [*turn-45-motifs*
       (append (sublists motif-ls)
	       (sublists (reverse motif-ls)))])))



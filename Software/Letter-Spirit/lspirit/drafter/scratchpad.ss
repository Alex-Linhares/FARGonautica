; JAR begins tinkering with Letter Spirit graphics 7/7/95
;-----------------------[ font display ]-------------------------------
(set! scratch-scale 15)

; display for fonts (26 glyphs a..z)
(define make-scratchpad
  (lambda ()
    (begin (set! wide (* 4 scratch-scale)) (set! tall (* 9 scratch-scale))
	     (set! *scratch-gc* (create-graphics-viewport
				 (* 13 wide) (* 2 tall)
				 (+ 4 (* 13 wide))  (+ 4 (* 2 tall))
				 0 0
				 (* 13 wide) (* 2 tall)))
	     (scratch-init alphabet))))

(define make-lobby
  (lambda ()
    (begin (set! wide (* 4 scratch-scale)) (set! tall (* 9 scratch-scale))
	     (set! *lobby-gc* (create-graphics-viewport
				 (* 13 wide) (* 2 tall)
				 (+ 4 (* 13 wide))  (+ 4 (* 2 tall))
				 0 0
				 (* 13 wide) (* 2 tall)))
	     (scratch-init alphabet))))

(set! *scratch-bg* "White")

(define clear-scratchpad
  (lambda ()
    (draw! *scratch-gc* `(let-sgl ((background-color ,*scratch-bg*)
				(foreground-color ,*scratch-bg*))
			 (filled-rectangle (0 0) (,(* 13 wide) ,(* 2 tall))))
	   'omit-from-database)))

(define clear-block
  (lambda (x y)
    (draw! *scratch-gc* `(let-sgl ((background-color ,*scratch-bg*)
				(foreground-color ,*scratch-bg*))
			 (filled-rectangle (0 0) (,(+ x wide) ,(+ y tall))))
	   'omit-from-database)))

; vertical bar between letters
(define draw-v-dividers
  (lambda (x y)
    (begin
      (draw! *scratch-gc* `(let-sgl ((foreground-color "Black")
				  (line-width 1))
				 (line (,(+ x wide) ,(+ y 0))
				       (,(+ x wide) ,(+ y tall))))
	     'omit-from-database)
      (draw! *scratch-gc* `(let-sgl ((foreground-color "Black")
				     (line-width 1))
				    (line (,(+ x 0) ,(+ y 0))
					  (,(+ x 0) ,(+ y tall))))
	     'omit-from-database))))

; horizontal line divider
(define draw-h-dividers
  (lambda (x y)
    (begin
      (draw! *scratch-gc* `(let-sgl ((foreground-color "Black")
				  (line-width 1))
				 (line (,(+ x 0) ,(+ y tall))
				       (,(+ x wide) ,(+ y tall))))
	     'omit-from-database)
      (draw! *scratch-gc* `(let-sgl ((foreground-color "Black")
				  (line-width 1))
				 (line (,(+ x 0) ,(+ y 0))
				       (,(+ x wide) ,(+ y 0))))
	     'omit-from-database))))

; grid - 3x7 grid
(define draw-grid
  (lambda (x y)
    (let f1 ((i (- scratch-scale 1)))
      (if (< i (* 3.5 scratch-scale))
	  (begin
	    (let f2 ((j (- (* 2 scratch-scale) 1)))
	      (if (< j (* 8.5 scratch-scale))
		  (begin
		    (draw! *scratch-gc* `(let-sgl ((foreground-color "Black")
					 (background-color "Black"))
				 (filled-rectangle
				  (,(+ x i) ,(+ y j))
				  (,(+ i 3 x) ,(+ j 3 y))))
		      'with-backing-store)
		    (f2 (+ j scratch-scale)))))
	    (f1 (+ i scratch-scale)))))
    (*flush-event-queue*)))

(define grid-place
  (lambda (l)
    (let*
      ((rank (letter-order l alphabet))
      (x (* wide (mod rank 13)))
      (y (* tall (- 1 (floor (/ rank 13))))))
;      (clear-block x y)
      (draw-grid x y)
      (draw-h-dividers x y)
      (draw-v-dividers x y)
      (draw-letter-name (+ x (* 2 scratch-scale))
			(+ y (/ scratch-scale 2)) l))))

(define blot-place
  (lambda (l)
    (let*
      ([rank (letter-order l alphabet)]
       [x (* wide (mod rank 13))]
       [y (* tall (- 1 (floor (/ rank 13))))])
      (draw! *scratch-gc* `(let-sgl ((foreground-color  ,*scratch-bg*))
			   (filled-rectangle (,x ,y)
					     (,(+ wide x) ,(+ tall y))))))))
(define token-place
  (lambda (l ls)
    (let*
      ((rank (letter-order l alphabet))
      (x (* wide (mod rank 13)))
      (y (* tall (- 1 (floor (/ rank 13))))))
      (blot-place l)
      (grid-place l)
      (scratch-draw-quanta x y ls))))

(define scratch-init
  (lambda (list)
    (cond
     ((null? list) (*flush-event-queue*))
     (else (begin
	    (grid-place (car list))
	    (scratch-init (cdr list)))))))

(set! alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define letter-order
  (lambda (l list)
    (cond
     ((eq? l (car list)) 0)
     (else (+ 1 (letter-order l (cdr list)))))))

(define draw-letter-name
  (lambda (x y name)
    (let* ([namestr (symbol->string name)])
      (draw! *scratch-gc* `(let-sgl ([foreground-color "Black"]
				  [origin (,x ,y)]
				  [text-justification center])
				 (text ,namestr)) 'omit-from-database))))

; x and y position for the grid (q is quanta width)
(set! *x* 15)
(set! *y* 15)
(set! *q* 15)

; draw and erase routines for quanta.  these routines expect a list of
; quanta that are "on"
(define scratch-draw-quanta
  (lambda (x y ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (draw-quantum *scratch-gc* x y (car ls))
	    (scratch-draw-quanta x y (cdr ls))))))

(define scratch-erase-quanta
  (lambda (x y ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (erase-quantum *scratch-gc* x y (car ls))
	    (scratch-erase-quanta x y (cdr ls))))))

(define draw-v-dividers
  (lambda (x y)
    (begin
      (draw! *scratch-gc* `(let-sgl ((foreground-color "Black")
				  (line-width 1))
				 (line (,(+ x wide) ,(+ y 0))
				       (,(+ x wide) ,(+ y tall))))
	     'omit-from-database)
      (draw! *scratch-gc* `(let-sgl ((foreground-color "Black")
				     (line-width 1))
				    (line (,(+ x 0) ,(+ y 0))
					  (,(+ x 0) ,(+ y tall))))
	     'omit-from-database))))

; draw or erase a single quantum using the endpoints list
(define draw-quantum
  (lambda (gc x y q)
    (let* ((endpoints (lookup q *endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color "Black")
			   (line-width 3))
		   (line (,(+ x (car end1)) ,(+ *y* y (cadr end1)))
			   (,(+ x (car end2)) ,(+ *y* y (cadr end2)))))
	'omit-from-database))))

(define erase-quantum
  (lambda (gc x y q)
    (let* ((endpoints (lookup q *endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color "White")
			   (line-width 3))
		   (line (,(+ x (car end1)) ,(+ *y* y (cadr end1)))
			   (,(+ x (car end2)) ,(+ *y* y (cadr end2)))))
	'omit-from-database))))

; list of quanta -> endpoints associations for display
(set! *endpoints*
  `((0 ((,*x* ,(+ *y* (* 6 *q*)))(,(+ *x* *q*) ,(+ *y* (* 6 *q*)))))
    (1 ((,(+ *x* *q*) ,(+ *y* (* 6 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 6 *q*)))))
    (2 ((,*x* ,(+ *y* (* 5 *q*)))(,(+ *x* *q*) ,(+ *y* (* 5 *q*)))))
    (3 ((,(+ *x* *q*) ,(+ *y* (* 5 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 5 *q*)))))
    (4 ((,*x* ,(+ *y* (* 4 *q*)))(,(+ *x* *q*) ,(+ *y* (* 4 *q*)))))
    (5 ((,(+ *x* *q*) ,(+ *y* (* 4 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 4 *q*)))))
    (6 ((,*x* ,(+ *y* (* 3 *q*)))(,(+ *x* *q*) ,(+ *y* (* 3 *q*)))))
    (7 ((,(+ *x* *q*) ,(+ *y* (* 3 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 3 *q*)))))
    (8 ((,*x* ,(+ *y* (* 2 *q*)))(,(+ *x* *q*) ,(+ *y* (* 2 *q*)))))
    (9 ((,(+ *x* *q*) ,(+ *y* (* 2 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 2 *q*)))))
    (10 ((,*x* ,(+ *y* *q*))(,(+ *x* *q*) ,(+ *y* *q*))))
    (11 ((,(+ *x* *q*) ,(+ *y* *q*))(,(+ (* 2 *q*) *x*) ,(+ *y* *q*))))
    (12 ((,*x* ,*y*)(,(+ *x* *q*) ,*y*)))
    (13 ((,(+ *x* *q*) ,*y*)(,(+ (* 2 *q*) *x*) ,*y*)))
    (14 ((,*x* ,(+ *y* (* 6 *q*)))(,*x* ,(+ *y* (* 5 *q*)))))
    (15 ((,(+ *x* *q*) ,(+ *y* (* 6 *q*)))(,(+ *x* *q*) ,(+ *y* (* 5 *q*)))))
    (16 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 6 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 5 *q*)))))
    (17 ((,*x* ,(+ *y* (* 5 *q*)))(,*x* ,(+ *y* (* 4 *q*)))))
    (18 ((,(+ *x* *q*) ,(+ *y* (* 5 *q*)))(,(+ *x* *q*) ,(+ *y* (* 4 *q*)))))
    (19 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 5 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 4 *q*)))))
    (20 ((,*x* ,(+ *y* (* 4 *q*)))(,*x* ,(+ *y* (* 3 *q*)))))
    (21 ((,(+ *x* *q*) ,(+ *y* (* 4 *q*)))(,(+ *x* *q*) ,(+ *y* (* 3 *q*)))))
    (22 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 4 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 3 *q*)))))
    (23 ((,*x* ,(+ *y* (* 3 *q*)))(,*x* ,(+ *y* (* 2 *q*)))))
    (24 ((,(+ *x* *q*) ,(+ *y* (* 3 *q*)))(,(+ *x* *q*) ,(+ *y* (* 2 *q*)))))
    (25 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 3 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 2 *q*)))))
    (26 ((,*x* ,(+ *y* (* 2 *q*)))(,*x* ,(+ *y* *q*))))
    (27 ((,(+ *x* *q*) ,(+ *y* (* 2 *q*)))(,(+ *x* *q*) ,(+ *y* *q*))))
    (28 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 2 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* *q*))))
    (29 ((,*x* ,(+ *y* *q*))(,*x* ,*y*)))
    (30 ((,(+ *x* *q*) ,(+ *y* *q*))(,(+ *x* *q*) ,*y*)))
    (31 ((,(+ (* 2 *q*) *x*) ,(+ *y* *q*))(,(+ (* 2 *q*) *x*) ,*y*)))
    (32 ((,(+ *x* *q*) ,(+ *y* (* 6 *q*)))(,*x* ,(+ *y* (* 5 *q*)))))
    (33 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 6 *q*)))(,(+ *x* *q*) ,(+ *y* (* 5 *q*)))))
    (34 ((,(+ *x* *q*) ,(+ *y* (* 5 *q*)))(,*x* ,(+ *y* (* 4 *q*)))))
    (35 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 5 *q*)))(,(+ *x* *q*) ,(+ *y* (* 4 *q*)))))
    (36 ((,(+ *x* *q*) ,(+ *y* (* 4 *q*)))(,*x* ,(+ *y* (* 3 *q*)))))
    (37 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 4 *q*)))(,(+ *x* *q*) ,(+ *y* (* 3 *q*)))))
    (38 ((,(+ *x* *q*) ,(+ *y* (* 3 *q*)))(,*x* ,(+ *y* (* 2 *q*)))))
    (39 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 3 *q*)))(,(+ *x* *q*) ,(+ *y* (* 2 *q*)))))
    (40 ((,(+ *x* *q*) ,(+ *y* (* 2 *q*)))(,*x* ,(+ *y* *q*))))
    (41 ((,(+ (* 2 *q*) *x*) ,(+ *y* (* 2 *q*)))(,(+ *x* *q*) ,(+ *y* *q*))))
    (42 ((,(+ *x* *q*) ,(+ *y* *q*))(,*x* ,*y*)))
    (43 ((,(+ (* 2 *q*) *x*) ,(+ *y* *q*))(,(+ *x* *q*) ,*y*)))
    (44 ((,*x* ,(+ *y* (* 6 *q*)))(,(+ *x* *q*) ,(+ *y* (* 5 *q*)))))
    (45 ((,(+ *x* *q*) ,(+ *y* (* 6 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 5 *q*)))))
    (46 ((,*x* ,(+ *y* (* 5 *q*)))(,(+ *x* *q*) ,(+ *y* (* 4 *q*)))))
    (47 ((,(+ *x* *q*) ,(+ *y* (* 5 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 4 *q*)))))
    (48 ((,*x* ,(+ *y* (* 4 *q*)))(,(+ *x* *q*) ,(+ *y* (* 3 *q*)))))
    (49 ((,(+ *x* *q*) ,(+ *y* (* 4 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 3 *q*)))))
    (50 ((,*x* ,(+ *y* (* 3 *q*)))(,(+ *x* *q*) ,(+ *y* (* 2 *q*)))))
    (51 ((,(+ *x* *q*) ,(+ *y* (* 3 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* (* 2 *q*)))))
    (52 ((,*x* ,(+ *y* (* 2 *q*)))(,(+ *x* *q*) ,(+ *y* *q*))))
    (53 ((,(+ *x* *q*) ,(+ *y* (* 2 *q*)))(,(+ (* 2 *q*) *x*) ,(+ *y* *q*))))
    (54 ((,*x* ,(+ *y* *q*))(,(+ *x* *q*) ,*y*)))
    (55 ((,(+ *x* *q*) ,(+ *y* *q*))(,(+ (* 2 *q*) *x*) ,*y*)))))

;----------- all done with that

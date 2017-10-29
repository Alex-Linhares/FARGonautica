(set! scratch-scale 15)

; display for fonts (26 glyphs a..z)
(define make-scratchpad
  (lambda ()
    (begin (set! wide (* 4 scratch-scale)) (set! tall (* 9 scratch-scale))
	   (set! scratchGP (create-graphics-viewport
			       (* 13 wide) (* 2 tall)
			       (+ 4 (* 13 wide))  (+ 4 (* 2 tall))
			       0 0
			       (* 13 wide) (* 2 tall)))
	   (scratch-init *scratch-gc* 26 alphabet))))

(define make-lobby
  (lambda ()
    (begin (set! wide (* 4 scratch-scale)) (set! tall (* 9 scratch-scale))
	   (set! lobbyGP (create-graphics-viewport
			       (* 13 wide) (* 2 tall)
			       (+ 4 (* 13 wide))  (+ 4 (* 2 tall))
			       0 0
			       (* 13 wide) (* 2 tall)))
	   (lobby-init *lobby-gc* 26 alphabet))))

(set! *scratch-bg* "White")

; grid - 3x7 grid
(define draw-small-grid
  (lambda (gc x y)
    (let f1 ((i (- scratch-scale 1)))
      (if (< i (* 3.5 scratch-scale))
	  (begin
	    (let f2 ((j (- (* 2 scratch-scale) 1)))
	      (if (< j (* 8.5 scratch-scale))
		  (begin
		    
		    (myDraw gc `(filled-rectangle
				  (,(+ x i) ,(+ y j))
				  (,(+ i 3 x) ,(+ j 3 y))))
		    
		    (f2 (+ j scratch-scale)))))
	    (f1 (+ i scratch-scale)))))
    (*flush-event-queue*)))

; write name, too, if flag=1
(define grid-place
  (lambda (gc rank flag)
    (let*
      ((x (* wide (mod rank 13)))
      (y (* tall (- 1 (floor (/ rank 13))))))
      (blot-place gc "cornsilk" rank)
      (draw-small-grid gc x y)
      (draw-dividers gc x y)
      (if (eq? flag 1)
	  (draw-letter-name gc
		(+ x (* 2 scratch-scale))
		(+ y (/ scratch-scale 2)) (nth-letter rank alphabet))))))

;(rank (letter-order l alphabet))
; puts a solid rectangle where a gridletter might otherwise be
(define blot-place
  (lambda (gc color rank)
    (let*
      ((x (* wide (mod rank 13)))
      (y (* tall (- 1 (floor (/ rank 13))))))
      (myDraw gc `(let-sgl ((foreground-color ,color))
			     (filled-rectangle (,x ,y)
				  (,(+ wide x) ,(+ tall y))))))))

(define token-place
  (lambda (gc rank ls)
    (let*
      ((x (* wide (mod rank 13)))
      (y (* tall (- 1 (floor (/ rank 13))))))
      (scratch-draw-quanta gc x y ls))))

(define scratch-init
  (lambda (gc num list)
    (cond
     ((or (null? list) (< num 0))(*flush-event-queue*))
     (else (begin
	    (grid-place gc (- num 1) 1)
	    (scratch-init gc (- num 1) (cdr list)))))))

(define lobby-init
  (lambda (gc num list)
    (cond
     ((or (null? list) (< num 0))(*flush-event-queue*))
     (else (begin
	    (blot-place gc "DarkSalmon" (- num 1))
	    (lobby-init gc (- num 1) (cdr list)))))))

(set! alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define letter-order
  (lambda (l list)
    (cond
     ((eq? l (car list)) 0)
     (else (+ 1 (letter-order l (cdr list)))))))

(define nth-letter
  (lambda (n list)
    (cond
     ((eq? n 0) (car list))
     (else (nth-letter (- n 1) (cdr list))))))

; x and y position for the grid (q is quanta width)
(set! *x* 15)
(set! *y* 15)
(set! *q* 15)

; draw and erase routines for quanta.  these routines expect a list of
; quanta that are "on"
(define scratch-draw-quanta
  (lambda (gc x y ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (scratch-draw-quantum gc x y (car ls))
	    (scratch-draw-quanta gc x y (cdr ls))))))

(define scratch-erase-quanta
  (lambda (gc x y ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (scratch-erase-quantum gc x y (car ls))
	    (scratch-erase-quanta gc x y (cdr ls))))))

(define draw-dividers
  (lambda (gc x y)
    (begin
      (myDraw gc `(line (,(+ x 0) ,(+ y 0))
				       (,(+ x wide) ,(+ y 0))))
      (myDraw gc `(line (,(+ x 0) ,(+ y 0))
				       (,(+ x 0) ,(+ y tall))))
      (myDraw gc `(line (,(+ x wide) ,(+ y tall))
				       (,(+ x wide) ,(+ y 0))))
      (myDraw gc `(line (,(+ x wide) ,(+ y tall))
					  (,(+ x 0) ,(+ y tall)))))))

(define draw-letter-name
  (lambda (gc x y name)
    (let* ([namestr (symbol->string name)])
      (myDraw gc `(text (,x ,y) ,namestr)))))

; draw or erase a single quantum using the endpoints list
(define scratch-draw-quantum
  (lambda (gc x y q)
    (let* ((endpoints (lookup q *scratch-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (myDraw gc `(let-sgl ((foreground-color "Black")
			   (line-width 3))
		   (line (,(+ x (car end1)) ,(+ *y* y (cadr end1)))
			   (,(+ x (car end2)) ,(+ *y* y (cadr end2)))))))))

(define scratch-erase-quantum
  (lambda (gc x y q)
    (let* ((endpoints (lookup q *scratch-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (myDraw gc `(let-sgl ((foreground-color "White")
			   (line-width 3))
		   (line (,(+ x (car end1)) ,(+ *y* y (cadr end1)))
			   (,(+ x (car end2)) ,(+ *y* y (cadr end2)))))))))

; list of quanta -> endpoints associations for display
(set! *scratch-endpoints*
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

;=============================================================================

(if *graphics*
    ; sxm windows for each of these which can be turned off
    ; they MUST be on initially in order for the windows to set
    ; themselves up
    (begin
      (set! workTrove '())
      (set! scratchTrove '())
      (set! lobbyTrove '())
      (set! tempTrove '())
      (set! roleTrove '())
      (set! rackTrove '())
      (set! *display-roles* #t)
      (set! *display-temp* #t)
      (set! *display-coderack* #t)
      (set! *draw-codelets* #t)
      ; draw activated roles in the workspace
      (set! *draw-roles* #t))     ; <-- overridden by display-roles
    (begin
      ; these are always false since they require *graphics*
      (set! *display-roles* #f)
      (set! *display-temp* #f)
      (set! *display-coderack* #f)
      (set! *draw-roles* #f)))
    

(define get-seeds
  (lambda ()
    (begin
      (get-data)
      (grid-place *lobby-gc* (- *num-seeds* 1) 0)
      (token-place *lobby-gc* (- *num-seeds* 1) *quanta-list*)
      (set! *lobby* (append *lobby* (list (list *quanta* *quanta-list*))))
      (graf-flush)
      (if (not *seeds-done*)
	  (get-seeds)))))


(define examine-head
   (lambda ()
     (begin
       (set! *quanta* (caar *lobby*))
       (set! *quanta-list* (cadar *lobby*))
       (set! *lobby* (cdr *lobby*))
       (re-rec))))

(set! *lobby* '())
(set! *scratchpad* '())
(set! *seeds-done* #f)
(set! *num-seeds* 0)
(make-scratchpad)
(make-lobby)
; (graf-flush)
; (get-seeds)
; (examine-head)
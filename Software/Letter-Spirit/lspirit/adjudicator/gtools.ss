;;===========================================================================
; gtools.ss : motif graphics routines   |  requires sxm and sgl
;===========================================================================
; These functions are enabled with the *graphics* flag.  If it is set to
; #t then they will do their thing.
;---------------------------------------------------------------------------
; Note: the file get-mystery.ss also includes a graphics application which
; takes advantage of some of the fuctions defined here.

; myDraw and flush by JAR, 2/97
; speed graphics up by minimizing number of draws
; troves hold graphics operations and are cleared in recog.ss
; grafStep: update every how many codelets?
(set! grafStep 1)

(define myDraw
  (lambda (where what)
    (case (car where)
  ((workGP)
   (set! workTrove (append workTrove (list what))))
  ((tempGP)
   (set! tempTrove (append tempTrove (list what))))
  ((roleGP)
   (set! roleTrove (append roleTrove (list what))))
  ((rackGP)
   (set! rackTrove (append rackTrove (list what)))))))

  
; changed to only flush if there's something in the trove, JAR 4/15/97
(define flush
  (lambda (where)
    (if (not (eq? (cadr where) '()))
      (draw! (eval (car where))
	     (append '(let-sgl ()) (eval (cadr where))) 'omit-from-database))))

(define graf-flush
  (lambda ()
    (begin
      (flush *gc*)
      (flush *temp-gc*)
;      (flush *role-gc*)
      (flush *rack-gc*)
      (set! workTrove '())
      (set! tempTrove '())
      (set! roleTrove '())
      (set! rackTrove '()))))

(set! *gc* '(workGP workTrove))
(set! *temp-gc* '(tempGP tempTrove))
(set! *role-gc* '(roleGP roleTrove))
(set! *rack-gc* '(rackGP rackTrove))

(set! *grid-bg* "LemonChiffon1")
(set! *grid-fg* "NavyBlue")
(set! *grid-dots* "black")

; central divider - dividing "line" between grid and info section
(define draw-divider
  (lambda (gc)
    (begin
      (draw! gc '(let-sgl ((foreground-color "LightGrey")
			   (line-width 4))
		   (line (20 35)(20 -2))))
      (draw! gc '(let-sgl ((foreground-color "WhiteSmoke")
			   (line-width 1))
		   (line (19.8 35)(19.8 -2))))
      (draw! gc '(let-sgl ((foreground-color "LightSlateGrey")
			   (line-width 2))
		   (line (20.3 35)(20.3 -2)))))))

; background for the grid
(define draw-grid-back
  (lambda (gc)
    (draw! gc `(let-sgl ((background-color ,*grid-bg*)
			 (foreground-color ,*grid-bg*))
		 (filled-rectangle (0 -2) (19.7 35))))))

; grid - 3x7 grid on the left side of the window
(define draw-grid
  (lambda (gc)
    (let f1 ((i 0))
      (if (< i 15)
	  (begin
	    (let f2 ((j 0))
	      (if (< j 35)
		  (begin
		    (draw! gc `(let-sgl ((foreground-color ,*grid-dots*)
					 (background-color ,*grid-dots*))
				 (filled-rectangle
				  (,(- (+ i 5) .1) ,(- (+ j 3) .1))
				  (,(+ (+ i 5) .1) ,(+ (+ j 3) .1)))))
		    (f2 (+ j 5)))))
	    (f1 (+ i 5)))))
    (*flush-event-queue*)))

; the info bar on the right side of the window.  Tells which phase the
; program is in.
(define draw-info-bar
  (lambda (txt)
    (myDraw *gc* '(let-sgl ((foreground-color "LightGrey"))
		   (filled-rectangle (20.4 33) (95 35))))
    (myDraw *gc* '(let-sgl ((foreground-color "LightSlateGrey"))
		   (rectangle (20.4 33) (95 35))))
    (let ((lngth (string-length txt)))
      (myDraw *gc* `(let-sgl ((foreground-color "blue")
			     (origin (,(- 45 (/ lngth 2)) 33.5)))
		     (text ,txt))))))

; erase all information in the info area (left side of window)
(define erase-info
  (lambda (gc)
    (draw! gc '(let-sgl ((foreground-color "white"))
		 (filled-rectangle (20.4 0) (95 32.9))))))


; make a box and draw a labeled value in it - computes the box size based
; on the string length
(define draw-value
  (lambda (gc label value x y)
    (let* ((numstr (number->string value))
	   (newstr (string-append label ":  " numstr))
	   (l (+ 3 (string-length newstr))))
      (draw! gc `(let-sgl ((foreground-color "LightGrey"))
		     (filled-rectangle (,x ,y) (,(+ x l) ,(+ y 2)))))
      (draw! gc `(let-sgl ((foreground-color "LightSlateGrey"))
		     (rectangle (,x ,y) (,(+ x l) ,(+ y 2)))))
      (draw! gc `(let-sgl ((foreground-color "blue")
			     (origin (,(add1 x) ,(+ y .5))))
		     (text ,newstr))))))

; update a value only (must know what coordinates are)
(define update-value
  (lambda (gc value x y)
    (let* ((str (number->string value))
	  (l (string-length str)))
      (draw! gc `(let-sgl ((foreground-color "LightGrey"))
		     (filled-rectangle (,x ,(+ y .1)) (,(+ x l) ,(+ y 1.5)))))
      (draw! gc `(let-sgl ((foreground-color "blue")
			     (origin (,x ,(+ y .5))))
		     (text ,str))))))
;--------------------------------------------------------------------------
; routines for role display
; the values #\001 - #\backspace seem to be good for dashes

(define dline
  (lambda (x off ch)
    (myDraw *gc*
    `(let-sgl ([background-color ,*grid-bg*]
	       [dash-offset ,off]
	       [dashes ,ch]
	       [line-style "LineDoubleDash"])
	(line (,x 3) (,x 33))))))      

; a lookup table for color and dash width (role-drawing graphics)
(set! *role-drawing-values*
      `((0 (line2 ,*grid-bg*))
	(1 (#\backspace "grey"))
	(1.0 (#\backspace "grey"))
	(2 (#\007 "grey"))
	(2.0 (#\007 "grey"))
	(3 (#\006 "grey"))
	(3.0 (#\006 "grey"))
	(4 (#\005 "grey"))
	(4.0 (#\005 "grey"))
	(5 (#\004 "black"))
	(5.0 (#\004 "black"))
	(6 (#\003 "black"))
	(6.0 (#\003 "black"))
	(7 (#\002 "black"))
	(7.0 (#\002 "black"))
	(8 (line "black"))
	(8.0 (line "black"))
	(9 (line2 "black"))
	(9.0 (line2 "black"))
	(10 (line2 "black"))
	(10.0 (line2 "black"))
	(20 (line2 ,*grid-bg*))))
	  
; amount of arc in the curves for role-drawing
(set! *cwidth* .3)

(define draw-left-post
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (4 13) (4 33))
			    (line (6 13) (6 33))
			    (arc (5 13) (2 2) 180 180)
			    (arc (5 33) (2 2) 0 180))
;	     oname
	 ))))

(define erase-left-post
  (lambda ()
    (draw-left-post 200)))

(define draw-left-uppost
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (4 23) (4 33))
			    (line (6 23) (6 33))
			    (arc (5 23) (2 2) 180 180)
			    (arc (5 33) (2 2) 0 180))
;	     oname
	 ))))

(define erase-left-uppost
  (lambda ()
    (draw-left-uppost 200)))

(define draw-right-uppost
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (14 23) (14 33))
			    (line (16 23) (16 33))
			    (arc (15 23) (2 2) 180 180)
			    (arc (15 33) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-uppost
  (lambda ()
    (draw-right-uppost 200)))

(define draw-left-halfpost
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (4 13) (4 23))
			    (line (6 13) (6 23))
			    (arc (5 13) (2 2) 180 180)
			    (arc (5 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-left-halfpost
  (lambda ()
    (draw-left-halfpost 200)))

(define draw-right-halfpost
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (14 13) (14 23))
			    (line (16 13) (16 23))
			    (arc (15 13) (2 2) 180 180)
			    (arc (15 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-halfpost
  (lambda ()
    (draw-right-halfpost 200)))

(define draw-center-post
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (9 13) (9 33))
			    (line (11 13) (11 33))
			    (arc (10 13) (2 2) 180 180)
			    (arc (10 33) (2 2) 0 180))
;	     oname
	 ))))

(define erase-center-post
  (lambda ()
    (draw-center-post 200)))

(define draw-f-post
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (9 12) (9 28))
			    (line (11 12) (11 28))
			    (arc (10 13) (2 2) 180 180)
			    (arc (15 28) (2 2) 180 180)
			    (arc (12.5 28) (3.2 8) 0 180)
			    (arc (12.5 28) (7.3 12) 0 180))
;	     oname
	 ))))

(define draw-t-post
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (9 18) (9 28))
			    (line (11 18) (11 28))
			    (arc (10 28) (2 2) 0 180)
			    (arc (15 13) (2 2) 270 180)
			    (arc (15 18) (8 8) 180 90)
			    (arc (15 18) (12 12) 180 90))
;	     oname
	 ))))

(define erase-t-post
  (lambda ()
    (draw-t-post 200)))

(define draw-center-hook
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (9 8) (9 23))
			    (line (11 8) (11 23))
			    (arc (10 23) (2 2) 0 180)
			    (arc (5 3) (2 2) 90 180)
			    (arc (5 8) (8 8) 270 90)
			    (arc (5 8) (12 12) 270 90))
;	     oname
	 ))))

(define erase-center-hook
  (lambda ()
    (draw-center-hook 200)))

(define draw-center-halfpost
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (9 13) (9 23))
			    (line (11 13) (11 23))
			    (arc (10 13) (2 2) 180 180)
			    (arc (10 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-center-halfpost
  (lambda ()
    (draw-center-halfpost 200)))

(define draw-dot
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 30) (4 4) 0 360))
;	     oname
	 ))))

(define erase-dot
  (lambda ()
    (draw-dot 200)))


(define draw-right-post
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (14 13) (14 33))
			    (line (16 13) (16 33))
			    (arc (15 13) (2 2) 180 180)
			    (arc (15 33) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-post
  (lambda ()
    (draw-right-post 200)))

(define draw-left-tail
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (4 3) (4 23))
			    (line (6 3) (6 23))
			    (arc (5 3) (2 2) 180 180)
			    (arc (5 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-left-tail
  (lambda ()
    (draw-left-tail 200)))

(define draw-left-downtail
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (4 3) (4 13))
			    (line (6 3) (6 13))
			    (arc (5 3) (2 2) 180 180)
			    (arc (5 13) (2 2) 0 180))
;	     oname
	 ))))

(define erase-left-downtail
  (lambda ()
    (draw-left-downtail 200)))

(define draw-right-tail
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (14 3) (14 23))
			    (line (16 3) (16 23))
			    (arc (15 3) (2 2) 180 180)
			    (arc (15 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-tail
  (lambda ()
    (draw-right-tail 200)))

(define draw-right-downtail
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (14 3) (14 13))
			    (line (16 3) (16 13))
			    (arc (15 3) (2 2) 180 180)
			    (arc (15 13) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-downtail
  (lambda ()
    (draw-right-downtail 200)))

(define draw-right-hook
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (14 8) (14 23))
			    (line (16 8) (16 23))
			    (arc (10 8) (8 8) 180 180)
			    (arc (10 8) (12 12) 180 180)
			    (arc (5 8) (2 2) 0 180)
			    (arc (15 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-hook
  (lambda ()
    (draw-right-hook 200)))

(define draw-right-halfhook
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (14 8) (14 13))
			    (line (16 8) (16 13))
			    (arc (10 8) (8 8) 180 180)
			    (arc (10 8) (12 12) 180 180)
			    (arc (15 13) (2 2) 0 180)
			    (arc (5 8) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-halfhook
  (lambda ()
    (draw-right-halfhook 200)))

(define draw-bottombar
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (10 2) (15 2))
			    (line (10 4) (15 4))
			    (arc (10 8) (8 8) 180 90)
			    (arc (10 8) (12 12) 180 90)
			    (arc (5 8) (2 2) 0 180)
			    (arc (15 3) (2 2) 270 180))
;	     oname
	 ))))

(define erase-bottombar
  (lambda ()
    (draw-bottombar 200)))

(define draw-right-bowl
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 18) (8 8) 210 295)
			    (arc (10 18) (12 12) 220 290)
			    (arc (5.9 21) (2 2) 160 180)
			    (arc (5.9 15) (2 2) 60 180))
;	     oname
	 ))))

(define erase-left-bowl
  (lambda ()
    (draw-left-bowl 200)))

(define draw-left-bowl
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 18) (8 8) 30 295)
			    (arc (10 18) (12 12) 35 290)
			    (arc (13.9 21) (2 2) 220 180)
			    (arc (13.9 15) (2 2) 340 180))
;	     oname
	 ))))

(define erase-right-bowl
  (lambda ()
    (draw-right-bowl 200)))

(define draw-left-halfbowl
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 15.5) (8 3) 30 295)
			    (arc (10 15.5) (12 7) 35 290)
			    (arc (13.9 17) (2 2) 220 180)
			    (arc (13.9 14) (2 2) 340 180))
;	     oname
	 ))))

(define erase-left-halfbowl
  (lambda ()
    (draw-left-halfbowl 200)))

(define draw-left-uphalfbowl
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 20.5) (8 3) 30 295)
			    (arc (10 20.5) (12 7) 35 290)
			    (arc (13.9 22) (2 2) 220 180)
			    (arc (13.9 19) (2 2) 340 180))
;	     oname
	 ))))

(define erase-left-uphalfbowl
  (lambda ()
    (draw-left-uphalfbowl 200)))

(define draw-right-halfbowl
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 15.5) (8 3) 210 295)
			    (arc (10 15.5) (12 7) 220 290)
			    (arc (5.9 17) (2 2) 160 180)
			    (arc (5.9 14) (2 2) 60 180))
;	     oname
	 ))))

(define erase-right-halfbowl
  (lambda ()
    (draw-right-halfbowl 200)))

(define draw-bowl
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (4 18) (4 23))
			    (line (6 18) (6 23))
			    (line (14 18) (14 23))
			    (line (16 18) (16 23))
			    (arc (5 23) (2 2) 0 180)
			    (arc (15 23) (2 2) 0 180)
			    (arc (10 18) (8 8) 180 180)
			    (arc (10 18) (12 12) 180 180))
;	     oname
	 ))))

(define erase-bowl
  (lambda ()
    (draw-bowl 200)))

(define draw-arch
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (4 13) (4 18))
			    (line (6 13) (6 18))
			    (line (14 13) (14 18))
			    (line (16 13) (16 18))
			    (arc (5 13) (2 2) 180 180)
			    (arc (15 13) (2 2) 180 180)
			    (arc (10 18) (8 8) 0 180)
			    (arc (10 18) (12 12) 0 180))
;	     oname
	 ))))

(define erase-arch
  (lambda ()
    (draw-arch 200)))

(define draw-circle
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 18) (8 8) 0 360)
			    (arc (10 18) (12 12) 0 360))
;	     oname
	 ))))

(define erase-circle
  (lambda ()
    (draw-circle 200)))

(define draw-down-circle
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 15.5) (8 3) 0 360)
			    (arc (10 15.5) (12 7) 0 360))
;	     oname
	 ))))

(define erase-down-circle
  (lambda ()
    (draw-down-circle 200)))

(define draw-up-circle
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 20.5) (8 3) 0 360)
			    (arc (10 20.5) (12 7) 0 360))
;	     oname
	 ))))

(define erase-up-circle
  (lambda ()
    (draw-up-circle 200)))

(define draw-right-arm
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 18) (14 8)  47 86)
			    (arc (10 18) (18 12) 48 84)
			    (arc (15 21.9) (2 2) 260 140)
			    (arc (5 21.9) (2 2) 145 140))
;	     oname
	 ))))

(define erase-right-arm
  (lambda ()
    (draw-right-arm 200)))

(define draw-mid-crossbar
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (5 19) (15 19))
			    (line (5 17) (15 17))
			    (arc (5 18) (2 2) 90 180)
			    (arc (15 18) (2 2) 270 180))
;	     oname
	 ))))

(define erase-mid-crossbar
  (lambda ()
    (draw-mid-crossbar 200)))

(define draw-basebar
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (5 14) (15 14))
			    (line (5 12) (15 12))
			    (arc (5 13) (2 2) 90 180)
			    (arc (15 13) (2 2) 270 180))
;	     oname
	 ))))

(define erase-basebar
  (lambda ()
    (draw-basebar 200)))

(define draw-sled
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (5 14) (10 14))
			    (line (5 12) (10 12))
			    (arc (10 18) (8 8) 270 90)
			    (arc (10 18) (12 12) 270 90)	 
			    (arc (5 13) (2 2) 90 180)
			    (arc (15 18) (2 2) 0 180))
;	     oname
	 ))))

(define erase-sled
  (lambda ()
    (draw-sled 200)))

(define draw-crossbar
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (5 24) (15 24))
			    (line (5 22) (15 22))
			    (arc (5 23) (2 2) 90 180)
			    (arc (15 23) (2 2) 270 180))
;	     oname
	 ))))

(define erase-crossbar
  (lambda ()
    (draw-crossbar 200)))

(define draw-f-slash
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (5.5 12) (15.5 22))
			    (line (4.5 14) (14.5 24))
			    (arc (5 13) (2 2) 115 180)
			    (arc (15 23) (2 2) 295 180))
;	     oname
	 ))))

(define erase-f-slash
  (lambda ()
    (draw-f-slash 200)))

(define draw-b-slash
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (5.5 24) (15.5 14))
			    (line (4.5 22) (14.5 12))
			    (arc (15 13) (2 2) 255 180)
			    (arc (5 23) (2 2) 65 180))
;	     oname
	 ))))

(define erase-b-slash
  (lambda ()
    (draw-b-slash 200)))

(define draw-kickstand
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (line (10.5 19) (15.5 14))
			    (line (9.5 17) (14.5 12))
			    (arc (15 13) (2 2) 255 180)
			    (arc (10 18) (2 2) 65 180))
;	     oname
	 ))))

(define erase-kickstand
  (lambda ()
    (draw-kickstand 200)))

(define draw-left-buttress
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (13 18) (14 8) 73 100)
			    (arc (13 18) (18 12) 73 100)
			    (line (4 19) (4 13))
			    (line (6 18.8) (6 13))
			    (arc (15 22.8) (2 2) 275 180)
			    (arc (5 13) (2 2) 180 180))
;	     oname
	 ))))

(define erase-left-buttress
  (lambda ()
    (draw-left-buttress 200)))

(define draw-right-buttress
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (7 18) (14 8) 5 100)
			    (arc (7 18) (18 12) 5 100)
			    (line (14 18.8) (14 13))
			    (line (16 19) (16 13))
			    (arc (5 22.8) (2 2) 95 180)
			    (arc (15 13) (2 2) 180 180))
;	     oname
	 ))))

(define erase-right-buttress
  (lambda ()
    (draw-right-buttress 200)))

(define draw-right-halfbuttress
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (12 18) (3.7 8) 5 120)
			    (arc (12 18) (8 12) 5 120)
			    (line (14 18.8) (14 13))
			    (line (16 19) (16 13))
			    (arc (10.2 22) (2 2) 120 210)
			    (arc (15 13) (2 2) 180 180))
;	     oname
	 ))))

(define erase-right-halfbuttress
  (lambda ()
    (draw-right-halfbuttress 200)))

(define draw-left-halfbuttress
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (7 18) (3.7 8) 5 120)
			    (arc (7 18) (8 12) 5 120)
			    (line (9 18.8) (9 13))
			    (line (11 19) (11 13))
			    (arc (5.2 22) (2 2) 120 210)
			    (arc (10 13) (2 2) 180 180))
;	     oname
	 ))))

(define erase-left-halfbuttress
  (lambda ()
    (draw-left-halfbuttress 200)))

(define draw-right-cap
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (10 18) (8 8) 90 90)
			    (arc (10 18) (12 12) 90 90)
			    (line (10 24) (15 24))
			    (line (10 22) (15 22))
			    (arc (15 23) (2 2) 270 180)
			    (arc (5 18) (2 2) 180 180))
;	     oname
	 ))))

(define erase-right-cap
  (lambda ()
    (draw-right-cap 200)))

(define draw-left-uparc
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (13 18) (14 8) 185 100)
			    (arc (13 18) (18 12) 185 100)
			    (line (4 17.2) (4 23))
			    (line (6 17.4) (6 23))
			    (arc (15 13.2) (2 2) 275 180)
			    (arc (5 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-left-uparc
  (lambda ()
    (draw-left-uparc 200)))

(define draw-left-half-uparc
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (8 18) (4 8) 185 120)
			    (arc (8 18) (8 12) 185 120)
			    (line (4 17.2) (4 23))
			    (line (6 17.4) (6 23))
			    (arc (9.8 13.9) (2 2) 310 180)
			    (arc (5 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-left-half-uparc
  (lambda ()
    (draw-left-half-uparc 200)))

(define draw-right-half-uparc
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (12 18) (4 8) 235 120)
			    (arc (12 18) (8 12) 235 120)
			    (line (14 17.4) (14 23))
			    (line (16 17.2) (16 23))
			    (arc (10.3 13.9) (2 2) 60 180)
			    (arc (15 23) (2 2) 0 180))
;	     oname
	 ))))

(define erase-right-half-uparc
  (lambda ()
    (draw-right-half-uparc 200)))

(define draw-left-wing
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (5 23) (2 2) 45 180)
			    (arc (10 13) (2 2) 225 180)
			    (line (9.3 12.2) (4.3 22.3))
			    (line (10.7 13.7) (5.7 23.8)))
;	     oname
	 ))))

(define erase-left-wing
  (lambda ()
    (draw-left-wing 200)))

(define draw-right-wing
  (lambda (act)
    (let* ([data (lookup (floor (/ act 10)) *role-drawing-values*)]
	   [dash-char (car data)]
	   [color (cadr data)])
;	   [oname (if (equal? color "black")
;		      '(overlays role-overlay)
;		      '(overlays role-overlay2))])
      (myDraw *gc* `(let-sgl ([foreground-color ,color]
			     [background-color ,*grid-bg*]
			     [dash-offset 0]
			     [dashes ,(case dash-char
					[(line2 line) #\001]
					[else dash-char])]
			     [line-width ,(case dash-char
					    [(line2) 2]
					    [else 1])]
			     [line-style ,(case dash-char
					    [(line line2) "LineSolid"]
					    [else "LineOnOffDash"])])
			    (arc (15 23) (2 2) 315 180)
			    (arc (10 13) (2 2) 135 180)
			    (line (9.3 13.7) (14.3 23.8))
			    (line (10.7 12.2) (15.7 22.3)))
;	     oname
	 ))))

(define erase-right-wing
  (lambda ()
    (draw-right-wing 200)))

;--------------------------------------------------------------------------
;;; Grid side drawing routines

; draw and erase routines for quanta
(define draw-quanta
  (lambda (gc ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (draw-quantum gc (car ls))
	    (draw-quanta gc (cdr ls))))))

(define erase-quanta
  (lambda (gc ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (erase-quantum gc (car ls))
	    (erase-quanta gc (cdr ls))))))

(define draw-pt-quanta
  (lambda (gc ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (draw-pt-quantum gc (car ls))
	    (draw-pt-quanta gc (cdr ls))))))

(define erase-pt-quanta
  (lambda (gc ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (erase-pt-quantum gc (car ls))
	    (erase-pt-quanta gc (cdr ls))))))

(define draw-pt-quanta-C
  (lambda (gc ls color)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (draw-pt-quantum-C gc (car ls) color)
	    (draw-pt-quanta-C gc (cdr ls) color)))))

(define draw-quantum
  (lambda (gc q)
    (let* ((endpoints (lookup q *real-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color ,*grid-fg*)
			   (line-width 4))
		   (line ,end1 ,end2))
	))))

(define erase-quantum
  (lambda (gc q)
    (let* ((endpoints (lookup q *real-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color ,*grid-bg*)
			   (line-width 4))
		   (line ,end1 ,end2))
	))))

(define draw-pt-quantum
  (lambda (gc q)
    (let* ((endpoints (lookup q *endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color ,*grid-fg*)
			   (line-width 4))
		   (line ,end1 ,end2))
	))))

(define erase-pt-quantum
  (lambda (gc q)
    (let* ((endpoints (lookup q *endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color ,*grid-bg*)
			   (line-width 4))
		   (line ,end1 ,end2))
	))))

(define draw-pt-quantum-C
  (lambda (gc q color)
    (let* ((endpoints (lookup q *endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color ,color)
			   (line-width 4))
		   (line ,end1 ,end2))
	))))

;;;----------------------------------------------------------------------
;;; Global values for use by the graphics functions (mostly lookup tables)

; list of quanta-pairs -> "sub-gloms" associations
(set! *part-numbers* '(((0 14) (1 5))
		       ((0 44) (1 6))
		       ((0 32) (2 7))
		       ((0 15) (2 8))
		       ((0 45) (2 9))
		       ((0 1) (2 3))
		       ((1 32) (3 7))
		       ((1 15) (3 8))
		       ((1 45) (3 9))
		       ((1 33) (4 10))
		       ((1 16) (4 11))
		       ((2 14) (19 12))
		       ((2 32) (19 13))
		       ((2 46) (19 24))
		       ((2 17) (19 23))
		       ((2 44) (20 14))
		       ((2 15) (20 15))
		       ((2 33) (20 16))
		       ((2 3) (20 21))
		       ((2 47) (20 27))
		       ((2 18) (20 26))
		       ((2 34) (20 25))
		       ((3 47) (21 27))
		       ((3 18) (21 26))
		       ((3 34) (21 25))
		       ((3 44) (21 14))
		       ((3 15) (15 21))
		       ((3 33) (21 16))
		       ((3 45) (22 17))
		       ((3 16) (22 18))
		       ((3 19) (22 29))
		       ((3 35) (22 28))
		       ((4 17) (37 30))
		       ((4 34) (37 31))
		       ((4 48) (37 42))
		       ((4 20) (37 41))
		       ((4 46) (38 32))
		       ((4 18) (38 33))
		       ((4 35) (38 34))
		       ((4 5) (38 39))
		       ((4 49) (38 45))
		       ((4 21) (38 44))
		       ((4 36) (38 43))
		       ((5 49) (39 45))
		       ((5 21) (39 44))
		       ((5 36) (39 43))
		       ((5 46) (39 32))
		       ((5 18) (39 33))
		       ((5 35) (39 34))
		       ((5 47) (40 35))
		       ((5 19) (40 36))
		       ((5 22) (40 47))
		       ((5 37) (40 46))
		       ((6 20) (55 48))
		       ((6 36) (55 49))
		       ((6 50) (55 60))
		       ((6 23) (55 59))
		       ((6 48) (56 50))
		       ((6 21) (56 51))
		       ((6 37) (56 52))
		       ((6 7) (56 57))
		       ((6 51) (56 63))
		       ((6 24) (56 62))
		       ((6 38) (56 61))
		       ((7 51) (57 63))
		       ((7 24) (57 62))
		       ((7 38) (57 61))
		       ((7 48) (57 50))
		       ((7 21) (57 51))
		       ((7 37) (57 52))
		       ((7 49) (58 53))
		       ((7 22) (58 54))
		       ((7 25) (58 65))
		       ((7 39) (58 64))
		       ((8 23) (73 66))
		       ((8 38) (73 67))
		       ((8 52) (73 78))
		       ((8 26) (73 77))
		       ((8 50) (74 68))
		       ((8 24) (74 69))
		       ((8 39) (74 70))
		       ((8 9) (74 75))
		       ((8 53) (74 81))
		       ((8 27) (74 80))
		       ((8 40) (74 79))
		       ((9 53) (75 81))
		       ((9 27) (75 80))
		       ((9 40) (75 79))
		       ((9 50) (75 68))
		       ((9 24) (75 69))
		       ((9 39) (75 70))
		       ((9 51) (76 71))
		       ((9 25) (76 72))
		       ((9 28) (76 83))
		       ((9 41) (76 82))
		       ((10 26) (91 84))
		       ((10 40) (91 85))
		       ((10 54) (91 96))
		       ((10 29) (91 95))
		       ((10 52) (92 86))
		       ((10 27) (92 87))
		       ((10 41) (92 88))
		       ((10 11) (92 93))
		       ((10 53) (92 99))
		       ((10 30) (92 98))
		       ((10 42) (92 97))
		       ((11 53) (93 99))
		       ((11 30) (93 98))
		       ((11 42) (93 97))
		       ((11 52) (93 86))
		       ((11 27) (93 87))
		       ((11 41) (93 88))
		       ((11 53) (94 89))
		       ((11 28) (94 90))
		       ((11 31) (94 101))
		       ((11 43) (94 100))
		       ((12 29) (109 102))
		       ((12 42) (109 103))
		       ((12 54) (110 104))
		       ((12 30) (110 105))
		       ((12 43) (110 106))
		       ((12 13) (110 111))
		       ((13 54) (111 104))
		       ((13 30) (111 105))
		       ((13 43) (111 106))
		       ((13 55) (112 107))
		       ((13 31) (112 108))
		       ((14 44) (5 6))
		       ((14 32) (12 13))
		       ((14 46) (12 24))
		       ((14 17) (12 23))
		       ((15 32) (8 7))
		       ((15 45) (8 9))
		       ((15 33) (15 16))
		       ((15 47) (15 27))
		       ((15 18) (15 26))
		       ((15 34) (15 25))
		       ((15 44) (15 14))
		       ((16 33) (11 10))
		       ((16 45) (18 17))
		       ((16 35) (18 28))
		       ((16 19) (18 29))
		       ((17 32) (23 13))
		       ((17 46) (23 24))
		       ((17 34) (30 31))
		       ((17 48) (30 42))
		       ((17 20) (30 41))
		       ((18 34) (25 26))
		       ((18 44) (26 14))
		       ((18 33) (26 16))
		       ((18 47) (26 27))
		       ((18 35) (33 34))
		       ((18 49) (33 45))
		       ((18 21) (33 44))
		       ((18 36) (33 43))
		       ((18 46) (33 32))
		       ((19 45) (29 17))
		       ((19 35) (28 29))
		       ((19 47) (36 35))
		       ((19 37) (36 46))
		       ((19 22) (36 47))
		       ((20 34) (41 31))
		       ((20 48) (41 42))
		       ((20 36) (48 49))
		       ((20 50) (48 60))
		       ((20 23) (48 59))
		       ((21 36) (44 43))
		       ((21 46) (44 32))
		       ((21 35) (44 34))
		       ((21 49) (44 45))
		       ((21 37) (51 52))
		       ((21 51) (51 63))
		       ((21 24) (51 62))
		       ((21 38) (51 61))
		       ((21 48) (51 50))
		       ((22 37) (47 46))
		       ((22 47) (47 35))
		       ((22 49) (54 53))
		       ((22 39) (54 64))
		       ((22 25) (54 65))
		       ((23 36) (59 49))
		       ((23 50) (59 60))
		       ((23 38) (66 67))
		       ((23 52) (66 78))
		       ((23 26) (66 77))
		       ((24 38) (62 61))
		       ((24 48) (62 50))
		       ((24 37) (62 52))
		       ((24 51) (62 63))
		       ((24 39) (69 70))
		       ((24 53) (69 81))
		       ((24 27) (69 80))
		       ((24 40) (69 79))
		       ((24 50) (69 68))
		       ((25 39) (65 64))
		       ((25 49) (65 53))
		       ((25 51) (72 71))
		       ((25 41) (72 82))
		       ((25 28) (72 83))
		       ((26 38) (77 67))
		       ((26 52) (77 78))
		       ((26 40) (84 85))
		       ((26 54) (84 96))
		       ((26 29) (84 95))
		       ((27 40) (80 79))
		       ((27 50) (80 68))
		       ((27 39) (80 70))
		       ((27 53) (80 81))
		       ((27 52) (87 86))
		       ((27 42) (87 97))
		       ((27 30) (87 98))
		       ((27 55) (87 99))
		       ((27 41) (87 88))
		       ((28 41) (83 82))
		       ((28 51) (83 71))
		       ((28 53) (90 89))
		       ((28 43) (90 100))
		       ((28 31) (90 101))
		       ((29 40) (95 85))
		       ((29 54) (95 96))
		       ((29 42) (102 103))
		       ((30 42) (98 97))
		       ((30 52) (98 86))
		       ((30 41) (98 88))
		       ((30 53) (98 99))
		       ((30 54) (105 104))
		       ((30 43) (105 106))
		       ((31 53) (101 89))
		       ((31 43) (101 100))
		       ((31 53) (108 107))
		       ((32 46) (13 24))
		       ((32 45) (7 9))
		       ((33 44) (16 14))
		       ((33 34) (16 25))
		       ((33 47) (16 27))
		       ((34 48) (31 42))
		       ((34 44) (25 14))
		       ((34 47) (25 27))
		       ((35 46) (34 32))
		       ((35 36) (34 43))
		       ((35 49) (34 45))
		       ((35 45) (28 17))
		       ((36 50) (49 60))
		       ((36 46) (43 32))
		       ((36 49) (43 45))
		       ((37 48) (52 50))
		       ((37 38) (52 61))
		       ((37 51) (52 63))
		       ((37 47) (46 35))
		       ((38 52) (67 78))
		       ((38 48) (61 50))
		       ((38 51) (61 63))
		       ((39 50) (70 68))
		       ((39 40) (70 79))
		       ((39 53) (70 81))
		       ((39 49) (64 53))
		       ((40 54) (85 96))
		       ((40 50) (79 68))
		       ((40 53) (79 81))
		       ((41 52) (88 86))
		       ((41 42) (88 97))
		       ((41 53) (88 99))
		       ((41 51) (82 71))
		       ((42 52) (97 87))
		       ((42 55) (97 99))
		       ((43 54) (111 104))
		       ((43 53) (100 89))
		       ((44 47) (14 27))
		       ((46 49) (32 45))
		       ((48 51) (50 63))
		       ((50 53) (68 81))
		       ((52 55) (86 99))
		       ; GEM left this somehow incomplete
		       ; JAR trying to fix 4/16/98
		       ; may not be patched
; missing pairs will give lookup error: item (x y) not found..
		       ((10 55) (86 92))
		       ((11 55) (86 93))
		       ((30 55) (86 98))
		       ((31 55) (86 108))
		       ((41 55) (88 99))))

; subglom -> endpoints associations
(set! *subglom-endpoints*
  '((1 ((5 33) (6 33)))
    (2 ((9 33) (10 33)))
    (3 ((10 33) (11 33)))
    (4 ((14 33) (15 33)))
    (5 ((5 33) (5 32)))
    (6 ((5 33) (6 32)))
    (7 ((9 32) (10 33)))
    (8 ((10 32) (10 33)))
    (9 ((11 32) (10 33)))
    (10 ((14 32) (15 33)))
    (11 ((15 32) (15 33)))
    (12 ((5 28) (5 29)))
    (13 ((5 28) (6 29)))
    (14 ((9 29) (10 28)))
    (15 ((10 29) (10 28)))
    (16 ((10 28) (11 29)))
    (17 ((14 29) (15 28)))
    (18 ((15 29) (15 28)))
    (19 ((5 28) (6 28)))
    (20 ((9 28) (10 28)))
    (21 ((10 28) (11 28)))
    (22 ((14 28) (15 28)))
    (23 ((5 28) (5 27)))
    (24 ((5 28) (6 27)))
    (25 ((9 27) (10 28)))
    (26 ((10 27) (10 28)))
    (27 ((11 27) (10 28)))
    (28 ((14 27) (15 28)))
    (29 ((15 28) (15 27)))
    (30 ((5 23) (5 24)))
    (31 ((5 23) (6 24)))
    (32 ((9 24) (10 23)))
    (33 ((10 24) (10 23)))
    (34 ((10 23) (11 24)))
    (35 ((14 24) (15 23)))
    (36 ((15 24) (15 23)))
    (37 ((5 23) (6 23)))
    (38 ((9 23) (10 23)))
    (39 ((10 23) (11 23)))
    (40 ((14 23) (15 23)))
    (41 ((5 23) (5 22)))
    (42 ((5 23) (6 22)))
    (43 ((9 22) (10 23)))
    (44 ((10 22) (10 23)))
    (45 ((11 22) (10 23)))
    (46 ((14 22) (15 23)))
    (47 ((15 23) (15 22)))
    (48 ((5 18) (5 19)))
    (49 ((5 18) (6 19)))
    (50 ((9 19) (10 18)))
    (51 ((10 19) (10 18)))
    (52 ((10 18) (11 19)))
    (53 ((14 19) (15 18)))
    (54 ((15 19) (15 18)))
    (55 ((5 18) (6 18)))
    (56 ((9 18) (10 18)))
    (57 ((10 18) (11 18)))
    (58 ((14 18) (15 18)))
    (59 ((5 18) (5 17)))
    (60 ((5 18) (6 17)))
    (61 ((9 17) (10 18)))
    (62 ((10 17) (10 18)))
    (63 ((11 17) (10 18)))
    (64 ((14 17) (15 18)))
    (65 ((15 18) (15 17)))
    (66 ((5 13) (5 14)))
    (67 ((5 13) (6 14)))
    (68 ((9 14) (10 13)))
    (69 ((10 14) (10 13)))
    (70 ((10 13) (11 14)))
    (71 ((14 14) (15 13)))
    (72 ((15 14) (15 13)))
    (73 ((5 13) (6 13)))
    (74 ((9 13) (10 13)))
    (75 ((10 13) (11 13)))
    (76 ((14 13) (15 13)))
    (77 ((5 13) (5 12)))
    (78 ((5 13) (6 12)))
    (79 ((9 12) (10 13)))
    (80 ((10 12) (10 13)))
    (81 ((11 12) (10 13)))
    (82 ((14 12) (15 13)))
    (83 ((15 13) (15 12)))
    (84 ((5 8) (5 9)))
    (85 ((5 8) (6 9)))
    (86 ((9 9) (10 8)))
    (87 ((10 9) (10 8)))
    (88 ((10 8) (11 9)))
    (89 ((14 9) (15 8)))
    (90 ((15 9) (15 8)))
    (91 ((5 8) (6 8)))
    (92 ((9 8) (10 8)))
    (93 ((10 8) (11 8)))
    (94 ((14 8) (15 8)))
    (95 ((5 8) (5 7)))
    (96 ((5 8) (6 7)))
    (97 ((9 7) (10 8)))
    (98 ((10 7) (10 8)))
    (99 ((11 7) (10 8)))
    (100 ((14 7) (15 8)))
    (101 ((15 8) (15 7)))
    (102 ((5 3) (5 4)))
    (103 ((5 3) (6 4)))
    (104 ((9 4) (10 3)))
    (105 ((10 4) (10 3)))
    (106 ((10 3) (11 4)))
    (107 ((14 4) (15 3)))
    (108 ((15 4) (15 3)))
    (109 ((5 3) (6 3)))
    (110 ((9 3) (10 3)))
    (111 ((10 3) (11 3)))
    (112 ((14 3) (15 3)))))
    
; list of quanta -> endpoints associations for display
(set! *endpoints*
  '((0 ((6 33)(9 33)))
    (1 ((11 33)(14 33)))
    (2 ((6 28)(9 28)))
    (3 ((11 28)(14 28)))
    (4 ((6 23)(9 23)))
    (5 ((11 23)(14 23)))
    (6 ((6 18)(9 18)))
    (7 ((11 18)(14 18)))
    (8 ((6 13)(9 13)))
    (9 ((11 13)(14 13)))
    (10 ((6 8)(9 8)))
    (11 ((11 8)(14 8)))
    (12 ((6 3)(9 3)))
    (13 ((11 3)(14 3)))
    (14 ((5 32)(5 29)))
    (15 ((10 32)(10 29)))
    (16 ((15 32)(15 29)))
    (17 ((5 27)(5 24)))
    (18 ((10 27)(10 24)))
    (19 ((15 27)(15 24)))
    (20 ((5 22)(5 19)))
    (21 ((10 22)(10 19)))
    (22 ((15 22)(15 19)))
    (23 ((5 17)(5 14)))
    (24 ((10 17)(10 14)))
    (25 ((15 17)(15 14)))
    (26 ((5 12)(5 9)))
    (27 ((10 12)(10 9)))
    (28 ((15 12)(15 9)))
    (29 ((5 7)(5 4)))
    (30 ((10 7)(10 4)))
    (31 ((15 7)(15 4)))
    (32 ((9 32)(6 29)))
    (33 ((14 32)(11 29)))
    (34 ((9 27)(6 24)))
    (35 ((14 27)(11 24)))
    (36 ((9 22)(6 19)))
    (37 ((14 22)(11 19)))
    (38 ((9 17)(6 14)))
    (39 ((14 17)(11 14)))
    (40 ((9 12)(6 9)))
    (41 ((14 12)(11 9)))
    (42 ((9 7)(6 4)))
    (43 ((14 7)(11 4)))
    (44 ((6 32)(9 29)))
    (45 ((11 32)(14 29)))
    (46 ((6 27)(9 24)))
    (47 ((11 27)(14 24)))
    (48 ((6 22)(9 19)))
    (49 ((11 22)(14 19)))
    (50 ((6 17)(9 14)))
    (51 ((11 17)(14 14)))
    (52 ((6 12)(9 9)))
    (53 ((11 12)(14 9)))
    (54 ((6 7)(9 4)))
    (55 ((11 7)(14 4)))))


; list of quanta -> endpoints associations for display
(set! *real-endpoints*
  '((0 ((5 33)(10 33)))
    (1 ((10 33)(15 33)))
    (2 ((5 28)(10 28)))
    (3 ((10 28)(15 28)))
    (4 ((5 23)(10 23)))
    (5 ((10 23)(15 23)))
    (6 ((5 18)(10 18)))
    (7 ((10 18)(15 18)))
    (8 ((5 13)(10 13)))
    (9 ((10 13)(15 13)))
    (10 ((5 8)(10 8)))
    (11 ((10 8)(15 8)))
    (12 ((5 3)(10 3)))
    (13 ((10 3)(15 3)))
    (14 ((5 33)(5 28)))
    (15 ((10 33)(10 28)))
    (16 ((15 33)(15 28)))
    (17 ((5 28)(5 23)))
    (18 ((10 28)(10 23)))
    (19 ((15 28)(15 23)))
    (20 ((5 23)(5 18)))
    (21 ((10 23)(10 18)))
    (22 ((15 23)(15 18)))
    (23 ((5 18)(5 13)))
    (24 ((10 18)(10 13)))
    (25 ((15 18)(15 13)))
    (26 ((5 13)(5 8)))
    (27 ((10 13)(10 8)))
    (28 ((15 13)(15 8)))
    (29 ((5 8)(5 3)))
    (30 ((10 8)(10 3)))
    (31 ((15 8)(15 3)))
    (32 ((10 33)(5 28)))
    (33 ((15 33)(10 28)))
    (34 ((10 28)(5 23)))
    (35 ((15 28)(10 23)))
    (36 ((10 23)(5 18)))
    (37 ((15 23)(10 18)))
    (38 ((10 18)(5 13)))
    (39 ((15 18)(10 13)))
    (40 ((10 13)(5 8)))
    (41 ((15 13)(10 8)))
    (42 ((10 8)(5 3)))
    (43 ((15 8)(10 3)))
    (44 ((5 33)(10 28)))
    (45 ((10 33)(15 28)))
    (46 ((5 28)(10 23)))
    (47 ((10 28)(15 23)))
    (48 ((5 23)(10 18)))
    (49 ((10 23)(15 18)))
    (50 ((5 18)(10 13)))
    (51 ((10 18)(15 13)))
    (52 ((5 13)(10 8)))
    (53 ((10 13)(15 8)))
    (54 ((5 8)(10 3)))
    (55 ((10 8)(15 3)))))


;------------------------------
; display a gridletter on the workspace grid to check a hex code
(define show-let
  (lambda (lt hex)
    (draw-grid-back workGP)
    (load-letter lt hex)
    (draw-quanta workGP *quanta-list*)))

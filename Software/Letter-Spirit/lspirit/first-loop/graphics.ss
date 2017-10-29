;===========================================================================
; graphics.ss : motif graphics routines   |  requires sxm and sgl
;===========================================================================
; These functions are enabled with the *graphics* flag.  If it is set to
; #t then they will do their thing.
;---------------------------------------------------------------------------
; Note: the file get-mystery.ss also includes a graphics application which
; takes advantage of some of the fuctions defined here.

; Set up display graphics for Letter Spirit run
; should figure out how to disable resizing!
(sgl) ;<-- load the SXM graphics package

(load "gtools.ss")

; create a scroll-bar window for extra workspace graphics.
; used a negative lower bound on the y so that the scroll bar would
; work properly
(if *graphics*
    (begin
      (define workGP (create-graphics-viewport 950 370 704 401 0 -2 95 35))
      (draw-divider workGP)
      (draw-grid-back workGP)
      (draw-grid workGP)
      (draw-info-bar "information bar")))

; easy change colors
(set! *grid-sg* "orange")
(set! *ant* "firebrick")
(set! *dissolve* "blue2")


;--------------------------------------------------------------------------
;;; The following functions are used in the glomming and shaking phases 

; Info side drawing routines during bonding
(define draw-bonding-info
  (lambda ()
    (draw-info-bar "bond ants running...")
    (myDraw *gc* `(let-sgl ((foreground-color "purple"))
		   (filled-rectangle (30 12) (59 25))))
    (myDraw *gc* `(let-sgl ((foreground-color "DarkSlateBlue"))
		   (rectangle (30 12) (59 25))))
    (draw-value workGP "Redundancy meter" 0 31 22)
    (draw-value workGP "Ants run so far" *bond-ants* 31 19)
    (draw-value workGP "Ants dropping glue" *globs-down* 31 16)
    (draw-value workGP "Total glue" *total-glue* 31 13)
    (*flush-event-queue*)))

; quick redraw of numbers
(define update-bonding-info
  (lambda ()
    (update-value workGP (round (/ (* *redund-ants* 100)
			    *redund-ant-c*)) 49 22)
    (update-value workGP *bond-ants* 48 19)
    (update-value workGP *globs-down* 51 16)
    (update-value workGP *total-glue* 43.5 13)))

;;; shaking routines (draw quanta to left and right of normal place)
(define draw-shake
  (lambda (ls)
    (draw-shaky-quanta ls 'right)
    (erase-pt-quanta workGP ls)
    (*flush-event-queue*)
    (draw-pt-quanta workGP ls)
    (erase-shaky-quanta ls 'right)
    (*flush-event-queue*)
    (draw-shaky-quanta ls 'left)
    (erase-pt-quanta workGP ls)
    (*flush-event-queue*)
    (draw-pt-quanta workGP ls)
    (erase-shaky-quanta ls 'left)
    (*flush-event-queue*)
    (draw-pt-quanta workGP ls)
    (*flush-event-queue*)))

(define draw-shaky-quanta
  (lambda (ls side)
    (let ((adj (if (equal? side 'right)
		   0.5
		   -0.5)))
      (cond
	((null? ls) (*flush-event-queue*))
	(else (draw-shaky-quantum (car ls) adj)
	      (draw-shaky-quanta (cdr ls) side))))))

(define erase-shaky-quanta
  (lambda (ls side)
    (let ((adj (if (equal? side 'right)
		   0.5
		   -0.5)))
      (cond
	((null? ls) (*flush-event-queue*))
	(else (erase-shaky-quantum (car ls) adj)
	      (erase-shaky-quanta (cdr ls) side))))))

(define draw-shaky-quantum
  (lambda (q adj)
    (let* ((endpoints (lookup q *endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints))
	   (aend1 (list (+ adj (car end1)) (cadr end1)))
	   (aend2 (list (+ adj (car end2)) (cadr end2))))
      (myDraw *gc* `(let-sgl ((foreground-color ,*grid-fg*)
			  (line-width 4))
		  (line ,aend1 ,aend2))))))

(define erase-shaky-quantum
  (lambda (q adj)
    (let* ((endpoints (lookup q *endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints))
	   (aend1 (list (+ adj (car end1)) (cadr end1)))
	   (aend2 (list (+ adj (car end2)) (cadr end2))))
      (myDraw *gc* `(let-sgl ((foreground-color ,*grid-bg*)
			  (line-width 4))
		  (line ,aend1 ,aend2))))))

;-------------------------------------------------------------------------
;;; Labeling graphics
; coderack graphics occur in a separate section of the window
;(sload 'code-g)

(set! *codelet-font* "*misc-fixed-medium*12*")
(set! *label-font* "*misc-fixed-medium*10*")
; unfortunately, 34 is as big as it gets!
(set! *r-role-font* "*helvetica-medium-r-normal--34*")
(set! *lfore* "black")
(set! *lback* "DodgerBlue")
(set! *lfore2* "LightSkyBlue1")

(define draw-codelet-count
  (lambda (num)
     (myDraw *gc* `(let-sgl ((foreground-color "black")
			    (background-color "yellow")
			    (text-mode image)
			    (font ,*label-font*))
			   (text (20.5 27.5) ,(number->string num))))))   

(define draw-no-codelets
  (lambda ()
    (myDraw *gc* `(let-sgl ((foreground-color ,*lback*))
			  (filled-rectangle (20.4 27) (95 32.9))))
    (myDraw *gc* '(let-sgl ((foreground-color "black")
			   (font "*times-medium-i-normal*18*"))
			  (text (22 30) "Codelet messages off")))))
    
(define draw-codelet-message-back
  (lambda ()
    (myDraw *gc* `(let-sgl ((foreground-color ,*lback*))
			  (filled-rectangle (20.5 27) (95 32.9))))))


(define draw-codelet
  (lambda (codelet gen)
    (if *draw-codelets*
	(begin
	  (myDraw *gc* `(let-sgl ((foreground-color ,*lback*))
				(filled-rectangle (24 27) (95 32.9))))
	  (myDraw *gc* `(let-sgl ((foreground-color ,*lfore*)
				 (font ,*codelet-font*)
				 (origin (24 31.5)))
				(text
				 ,(string-append codelet " from generation "
						 (number->string gen))))))
	(myDraw *gc* `(let-sgl ((foreground-color ,*lback*))
			      (filled-rectangle (59 27) (67 32)))))
    (*flush-event-queue*)))

(define draw-codelet-message
  (lambda (msg)
    (if *draw-codelets*
	(begin
	  (myDraw *gc* `(let-sgl ((foreground-color ,*lfore*)
				 (font ,*label-font*)
				 (origin (25 29.7)))
				(text ,msg)))
	  (*flush-event-queue*)))))

(define draw-codelet-message2
  (lambda (msg)
    (if *draw-codelets*
	(begin
	  (myDraw *gc* `(let-sgl ((foreground-color ,*lfore*)
				 (font ,*label-font*)
				 (origin (25 27.7)))
				(text ,msg)))
	  (*flush-event-queue*)))))
  
(define draw-r-role-letter
  (lambda (msg)
    (myDraw *gc* `(let-sgl ((foreground-color "yellow"))
		   (filled-rectangle (59 27.8) (65 32))))
    (myDraw *gc* `(let-sgl ((foreground-color ,*lfore*)
			   (font ,*r-role-font*)
			   (origin (60 28.7)))
		   (text ,msg)))
    (*flush-event-queue*)))

(define draw-crossout
  (lambda ()
    (myDraw *gc* `(let-sgl ([foreground-color "red"]
			   [line-width 4])
			  (line (59.2 28) (64.8 31.8))
			  (line (64.8 28) (59.2 31.8))))))

(define draw-checkmark
  (lambda ()
     (myDraw *gc* `(let-sgl ([foreground-color "red"]
			   [line-width 4])
			   (line (64.2 29.5) (64.8 28.7))
			   (line (64.8 28.7) (66.8 31))))))

; Colors for the parts.
(set! *wfore* '("ForestGreen" "red" "DarkViolet" "NavyBlue" "DarkGoldenrod" "RoyalBlue3" "DarkOrange" "azure4"))
(set! *wback* "LightSkyBlue1")
; If the workspace has not changed, then don't redraw it!
(set! *wold* '())

(define draw-workspace
  (lambda (w)
    (if (equal? *wold* w)
	#t
	(begin
	  (myDraw *gc* `(let-sgl ((foreground-color ,*wback*))
				(filled-rectangle (20.4 -2) (95 26.9))))
	  (draw-workspace-work w 22 25 *wfore*)
	  (set! *wold* w)
	  (*flush-event-queue*)))))
    
(define draw-workspace-work
  (lambda (ls x y colorls)
    (letrec ((loop (lambda (sls n color)
		     (cond
		       ((null? sls) n)
		       (else ;(printf "~s ~s~%" n (car sls))
			     (myDraw *gc* `(let-sgl ((font ,*label-font*)
						    (foreground-color ,color)
						    (origin (,x ,n)))
					    (text ,(format "~s" (car sls)))))
			     (loop (cdr sls) (sub1 n) color))))))
      (cond
       ((null? ls) #t)
       (else (let* ([color (car colorls)]
		    [quanta (collapse (caar ls))]
		    [nonesuch (draw-pt-quanta-C workGP quanta color)]
		    [lines (- (loop (car ls) y color) y 2)])
	       (cond
		[(and (= x 22)
		      (< (+ y lines 1) 15))
		 (draw-workspace-work (cdr ls) 45 25 (cdr colorls))]
		[(and (= x 45)
		      (< (+ y lines 1) 15))
		 (draw-workspace-work (cdr ls) 68 25 (cdr colorls))]
		[else (draw-workspace-work
		       (cdr ls) x (+ y lines 1) (cdr colorls))])))))))


(define test-font
  (lambda (f x y)
    (myDraw *gc* `(let-sgl ((origin (,x ,y))
			   (font ,f))
		   (text ,(format "test of font ~%line 2"))))))
;--------------------------------------------------------------------------
;;; Grid side drawing routines

; bond ant graphic
; in order to erase ant, use (erase! workGP *ant-obj*) (can't bind this in)
(define draw-ant
  (lambda (quanta)
    (set! *ant-obj*
      (let* ((subgloms (lookup quanta *endpoints*))
	     (p1 (car subgloms))
	     (p2 (cadr subgloms))
	     (flashpoint (list (+ (/ (+ (car p1) (car p2)) 2) .7)
			       (/ (+ (cadr p1) (cadr p2)) 2))))
	(myDraw *gc* `(let-sgl ((foreground-color ,*ant*)
			    (origin ,flashpoint))
		    (filled-arc (0 0) (1 .6) 0 360)
		    (filled-arc (-.7 0) (.6 .6) 0 360)
		    (filled-arc (-1.26 0) (.6 .6) 0 360)
		    (polyline (.1 -.6) (-.4 -.4) (0 0) (-.4 .4) (.1 .6))
		    (polyline (-.3 -.6) (-.8 -.4) (-.4 0) (-.8 .4) (-.3 .6))
		    (polyline (-.8 -.6) (-1.1 -.4) (-.8 0) (-1.1 .4) (-.8 .6))
		    (polyline (-1.6 -.1) (-1.8 -.2) (-2 -.1))
		    (polyline (-1.6 .1) (-1.8 .2) (-2 .1))))))
    (*flush-event-queue*)))

; draw and erase functions for gloms between quanta
(define draw-subgloms
  (lambda (ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (draw-subglom (car ls))
	    (draw-subgloms (cdr ls))))))
    
(define erase-subgloms
  (lambda (ls)
    (cond
      ((null? ls) (*flush-event-queue*)
                  (draw-grid workGP))
      (else (erase-subglom (car ls))
	    (erase-subgloms (cdr ls))))))
    

; subgloms are the part-glue connectors
(define draw-subglom
  (lambda (sg)
    (let* ((endpoints (lookup sg *subglom-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (myDraw *gc* `(let-sgl ((foreground-color ,*grid-sg*)
			  (line-width 2))
		  (line ,end1 ,end2))))))

(define erase-subglom
  (lambda (sg)
    (let* ((endpoints (lookup sg *subglom-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (myDraw *gc* `(let-sgl ((foreground-color ,*grid-bg*)
			  (line-width 2))
		  (line ,end1 ,end2))))))

;-----------------------[ temperature display ]------------------------
; display for temperature
(if *display-temp*
    (set! tempGP (create-graphics-viewport 70 200 74 204 0 0 7 20)))

(set! *temp-font* "*misc-fixed-medium*12*")
(set! *temp-bg* "LightSkyBlue1")
(set! *temp-therm* "white")


(define clear-temp-window
  (lambda ()
    (myDraw *temp-gc* `(let-sgl ((background-color ,*temp-bg*)
				(foreground-color ,*temp-bg*))
			 (filled-rectangle (0 0) (7 20))))))

(define draw-thermometer
  (lambda ()
    (clear-temp-window)
    (myDraw *temp-gc* `(let-sgl ([foreground-color "navyblue"]
				[origin (2 18)]
				[font ,*temp-font*])
			       (text ,"Temp")))
    (myDraw *temp-gc*
	   `(let-sgl ((foreground-color ,"LightGrey"))
		     (filled-arc (3.5 3.5) (3 3) 0 360)))
    (myDraw *temp-gc*
	   `(let-sgl ((foreground-color ,"red"))
		     (filled-arc (3.5 3.5) (2.6 2.6) 0 360)))
    (myDraw *temp-gc*
	   `(let-sgl ((foreground-color ,"LightGrey"))
		     (filled-rectangle (3 4.9) (4 15))
		     (filled-arc (3.5 15) (.9 .9) 0 360)))
    (myDraw *temp-gc*
	   `(let-sgl ((foreground-color ,"WhiteSmoke")
		      (line-width 1))
		     (arc (3.5 3.5) (3 3) 108 162)
		     (arc (3.5 15) (1 1) 90 90)
		     (line (3 4.9) (3 15.1)))) 
    (myDraw *temp-gc*
	   `(let-sgl ((foreground-color ,"LightSlateGrey")
		      (line-width 1))
		     (arc (3.5 3.5) (3 3) 270 162)
		     (arc (3.5 15) (1 1) 0 90)
		     (line (4 4.9) (4 15.1))))
    (hash-mark 5 1)
    (hash-mark 6 0)
    (hash-mark 7 0)
    (hash-mark 8 0)
    (hash-mark 9 0)
    (hash-mark 10 1)
    (hash-mark 11 0)
    (hash-mark 12 0)
    (hash-mark 13 0)
    (hash-mark 14 0)
    (hash-mark 15 1)
    (draw-temp 100)))

; draw a hash mark if ln = 1 long else short
(define hash-mark
  (lambda (y ln)
    (if (= ln 1)
	(myDraw *temp-gc*
	       `(let-sgl ((foreground-color ,"navyblue")
			  (line-width 2))
			 (line (1 ,y) (2 ,y))))
	(myDraw *temp-gc*
	       `(let-sgl ((foreground-color ,"navyblue")
			  (line-width 2))
			 (line (1.5 ,y) (2 ,y)))))))

; fill in the mercury to a give temperature
(define draw-temp
  (lambda (temp)
    (myDraw *temp-gc*
	   `(let-sgl ((foreground-color ,"LightGrey"))
		     (filled-rectangle (3.1 4.9) (3.9 15))))
    (let ((top ( + 5 (/ temp 10))))
      (myDraw *temp-gc*
	     `(let-sgl ((foreground-color ,"red"))
		       (filled-rectangle (3.17 4.5) (3.9 ,top)))))
    (*flush-event-queue*)))
 

(if *display-temp* (draw-thermometer))
;-----------------------[ role display ]-------------------------------

; display for roles (depends on number of roles in the memory)
(if *display-roles*
    (let* ([roles (length *rolesNwholes*)]
	   [lines (ceiling (/ (* roles 5) 105))]
	   [width 1050]
	   [height (* lines 50)])
      (set! roleGP (create-graphics-viewport width height
						(+ 4 width)
						(+ 4 height) 0 0
						(/ width 10) (/ height -10)))))

;;(set! *role-font* "-misc-fixed-medium-r-normal--0-80-*-*-*-*-iso8859-1")
(set! *role-font* "*helvetica-medium-r-normal--8*")
(set! *roles-bg* "LightSkyBlue1")

(define clear-role-window
  (lambda ()
    (myDraw *role-gc* `(let-sgl ((background-color ,*roles-bg*)
				(foreground-color ,*roles-bg*))
			 (filled-rectangle (0 0) (105 -105))))))

; vertical bar between roles
(define draw-v-role-divider
  (lambda (x y)
    (begin
      (myDraw *role-gc* `(let-sgl ((foreground-color "LightGrey")
				  (line-width 3))
				 (line (,x ,y) (,x ,(+ y 10)))))
      (myDraw *role-gc* `(let-sgl ((foreground-color "WhiteSmoke")
				  (line-width 1))
				 (line (,(- x .2) ,y) (,(- x .2) ,(+ y 10)))))
      (myDraw *role-gc* `(let-sgl ((foreground-color "LightSlateGrey")
				  (line-width 2))
				 (line (,(+ x .3) ,y) (,(+ x .3) ,(+ y 10))))))))

; horizontal role-line divider
(define draw-h-role-divider
  (lambda (y)
    (begin
      (myDraw *role-gc* `(let-sgl ((foreground-color "LightGrey")
				  (line-width 3))
				 (line (0 ,(- y .1)) (105 ,(- y .1)))))
      (myDraw *role-gc* `(let-sgl ((foreground-color "WhiteSmoke")
				  (line-width 1))
				 (line (0 ,(+ y .1)) (105 ,(+ y .1)))))
      (myDraw *role-gc* `(let-sgl ((foreground-color "LightSlateGrey")
				  (line-width 2))
				 (line (0 ,(- y .3)) (105 ,(- y .3))))))))

; Draw all activation values.  This is called only at setup or after a
; clean.
(define draw-roles-activation
  (lambda (x y act color)
    (let* ([gconst (/ act 60)]
	   [p1 (list (- x gconst) (- (+ y gconst) .8))]
	   [p2 (list (+ x gconst) (- (- y gconst) .8))])
      (myDraw *role-gc* `(let-sgl ((foreground-color ,color))
				 (filled-rectangle ,p1 ,p2))))))

; draw a role activation using the new lookup table of places
(define draw-role-activation
  (lambda (role act)
    (let* ([econst (/ 100 60)]
	   [absact (abs act)]
	   [gconst (/ absact 60)]
	   [point (lookup role *role-activation-points*)]
	   [x (car point)]
	   [y (cadr point)]
	   [p1 (list (- x gconst) (- (+ y gconst) .8))]
	   [p2 (list (+ x gconst) (- (- y gconst) .8))]
	   [ep1 (list (- x econst) (- (+ y econst) .8))]
	   [ep2 (list (+ x econst) (- (- y econst) .8))])
      (myDraw *role-gc* `(let-sgl ((foreground-color ,*roles-bg*))
				 (filled-rectangle ,ep1 ,ep2)))
      (myDraw *role-gc* `(let-sgl ((foreground-color ,(if (positive? act)
							"black"
							"red")))
				  (filled-rectangle ,p1 ,p2))))))

(define draw-role-name
  (lambda (x y name)
    (let* ([name-sym (role-name (eval name))]
	   [new-x (+ .2 x)]
	   [new-y (+ 4.2 y)]
	   [namestr (symbol->string name-sym)])
      (myDraw *role-gc* `(let-sgl ([foreground-color "black"]
				  [origin (,new-x ,new-y)]
				  [text-justification center]
				  [font ,*role-font*])
				 (text ,namestr))))))

(define role-name
  (lambda (role)
    (caar (lookup-list 'names (eval role)))))

; Compute this list at setup time and refer to it to tweak only one
; role's activation graphics.
(define compute-role-activation-points
  (lambda ()
    (letrec ([r-loop (lambda (x y rolels)
		       (cond
			[(null? rolels) '()]
			[else
			 (let* ([name (role-name (car rolels))]
				[x-pt (+ 2.5 x)]
				[y-pt (+ 2.8 y)])
			   (cons
			    (list name (list x-pt y-pt))
			    (if (= x 100)
				(begin
				  (r-loop 0 (+ y 5) (cdr rolels)))
				(r-loop (+ x 5) y (cdr rolels)))))]))])
      (r-loop 0 (/ (* 50 (ceiling (/ (* (length *rolesNwholes*) 5) 105))) -10)
	      *rolesNwholes*))))

(set! *role-activation-points* (compute-role-activation-points))

(define draw-role-activations
  (lambda ()
    (letrec ([r-loop (lambda (x y rolels)
			 (cond
			  [(null? rolels) #t]
			  [else
			   (let* ([name (role-name (car rolels))]
				  [act (get-role-activation name)])
			     (draw-roles-activation
			      (+ 2.5 x) (+ y 2.8) 100 *roles-bg*)
			     (if (positive? act)
				 (draw-roles-activation
				  (+ 2.5 x) (+ y 2.8) act "black")
				 (draw-roles-activation
				  (+ 2.5 x) (+ y 2.8) (abs act) "red"))
			     (if (= x 100)
				 (begin
				   (r-loop 0 (+ y 5) (cdr rolels)))
				 (r-loop (+ x 5) y (cdr rolels))))]))])
;      (unmap-all-overlays! workGP)
;      (clear-overlay! workGP 'role-overlay)
;      (clear-overlay! workGP 'role-overlay2)
      (r-loop 0 (/ (* 50 (ceiling (/ (* (length *rolesNwholes*) 5) 105))) -10) *rolesNwholes*)
;      (map-overlay! workGP 'role-overlay2)
;      (map-overlay! workGP 'role-overlay)
      (*flush-event-queue*))))
  
(define draw-roles
  (lambda ()
    (clear-role-window)
    (letrec ([r-loop (lambda (x y rolels)
			 (cond
			  [(null? rolels) #t]
			  [else
			   (let* ([name (role-name (car rolels))]
				  [act (get-role-activation name)])
			     (draw-v-role-divider (+ 5 x) y)
			     (draw-roles-activation
			      (+ 2.5 x) (+ y 2.8) act "black")
			     (draw-role-name (+ x .3) (+ y .3) name)
			     (if (= x 100)
				 (begin
				   (draw-h-role-divider (+ y 5))
				   (r-loop 0 (+ y 5) (cdr rolels)))
				 (r-loop (+ x 5) y (cdr rolels))))]))])
;      (unmap-all-overlays! workGP)
;      (clear-overlay! workGP 'role-overlay)
;      (clear-overlay! workGP 'role-overlay2)
      (r-loop 0 (/ (* 50 (ceiling (/ (* (length *rolesNwholes*) 5) 105))) -10) *rolesNwholes*)
;      (map-overlay! workGP 'role-overlay2)
;      (map-overlay! workGP 'role-overlay)
      (*flush-event-queue*))))

;;(if *display-roles* (begin (init-overlays)(draw-roles)))
(if *display-roles* (draw-roles))

(define draw-grid-role
  (lambda (name act)
    (case name
      [(left-post) (draw-left-post act)]
      [(right-post) (draw-right-post act)]
      [(left-tail) (draw-left-tail act)]
      [(left-halfpost) (draw-left-halfpost act)]
      [(right-tail) (draw-right-tail act)]
      [(right-buttress) (draw-right-buttress act)]
      [(left-buttress) (draw-left-buttress act)]
      [(right-bowl) (draw-right-bowl act)]
      [(left-bowl) (draw-left-bowl act)]
      [(center-post) (draw-center-post act)]
      [(right-arm) (draw-right-arm act)]
      [(mid-crossbar) (draw-mid-crossbar act)]
      [(circle) (draw-circle act)]
      [(right-halfpost) (draw-right-halfpost act)]
      [(left-uparc) (draw-left-uparc act)]
      [(center-halfpost) (draw-center-halfpost act)]
      [(dot) (draw-dot act)]
      [(left-halfbowl) (draw-left-halfbowl act)]
      [(crossbar) (draw-crossbar act)]
      [(f-post) (draw-f-post act)]
      [(t-post) (draw-t-post act)]
      [(right-hook) (draw-right-hook act)]
      [(center-hook) (draw-center-hook act)]
      [(left-uppost) (draw-left-uppost act)]
      [(right-halfbuttress) (draw-right-halfbuttress act)]
      [(left-halfbuttress) (draw-left-halfbuttress act)]
      [(f-slash) (draw-f-slash act)]
      [(b-slash) (draw-b-slash act)]
      [(basebar) (draw-basebar act)]
      [(left-half-uparc) (draw-left-half-uparc act)]
      [(right-half-uparc) (draw-right-half-uparc act)]
      [(kickstand) (draw-kickstand act)]
      [(right-wing) (draw-right-wing act)]
      [(left-wing) (draw-left-wing act)]
      [(right-cap) (draw-right-cap act)]
      [(right-halfbowl) (draw-right-halfbowl act)]
      [(up-circle) (draw-up-circle act)]
      [(down-circle) (draw-down-circle act)]
      [(arch) (draw-arch act)]
      [(bowl) (draw-bowl act)]
      [(right-halfhook) (draw-right-halfhook act)]
      [(right-uppost) (draw-right-uppost act)]
      [(left-downtail) (draw-left-downtail act)]
      [(right-downtail) (draw-right-downtail act)]
      [(bottombar) (draw-bottombar act)]
      [(sled) (draw-sled act)]
      [(left-uphalfbowl) (draw-left-uphalfbowl act)]
      [else (codemsg "Sorry, no picture for ~a~%" name)])))
	   

;-----------------------[ coderack display ]-------------------------------
; display for coderack
(if *display-coderack*
    (let* ([width 200]
	   [height 350])
      (set! rackGP (create-graphics-viewport width height
						(+ 4 width) (+ 4 height)
						0 0
						(/ width 10) (/ height 10)))))

(set! *rack-font* "-misc-fixed-medium-r-normal--0-80-*-*-*-*-iso8859-1")
(set! *rack-bg* "LightSkyBlue1")

(define draw-coderack
  (lambda (w)
    (myDraw *rack-gc* `(let-sgl ((foreground-color ,*rack-bg*))
			       (filled-rectangle (0 0) (25 35))))
    (myDraw *rack-gc* `(let-sgl ((font ,*label-font*)
				(foreground-color ,(car *wfore*))
				(origin (1 33)))
			       (text ,(format "URG  ~a GEN"
					      (pad "Codelet Type" 20)))))
    (draw-coderack-work (sort racksortkey w) 1 31)
    (*flush-event-queue*)))

(define racksortkey
  (lambda (ls1 ls2)
    (if (> (caddr ls1) (caddr ls2))
	#t
	#f)))

(define draw-coderack-work
  (lambda (ls x y)
    (cond
     [(null? ls) #t]
     [else 
      (myDraw *rack-gc* `(let-sgl ((font ,*label-font*)
				  (foreground-color ,(car *wfore*))
				  (origin (,x ,y)))
				 (text ,(format "~s   ~a ~s" (caddar ls) (pad (symbol->string (cadar ls)) 21)(cadddr (car ls))))))
      (draw-coderack-work (cdr ls) x (sub1 y))])))
      
(if *display-coderack*
    (begin
      (set! *coderack* '((empty empty 0 0)))
      (draw-coderack *coderack*)))

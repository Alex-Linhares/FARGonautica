;===========================================================================
; get-mystery.ss : motif graphics interface for the gridfont database
;===========================================================================
; This code requires both the SGL graphics hooks and the load_fio library
; from Motorola.  This is ALMOST a stand-alone program.  It does like to
; run with a frozen continuation though that it can invoke when it dies.

; load in the motorola scanning code
(load "/u/rehling/bin/load_fio.ss")
      
; pointer to the gridfonts database
(set! *data-file* "/u/rehling/bin/gridfonts.data")

;------------------------------------------------------------------------
; lookup gridletter in database (vector) of gridfonts.  return it.

(define lookup-ptr
  (lambda (f_no l_no fvec)
    (let* ([font-num (lookup-current-font-index f_no *findex*)]
	   [fontls (cdr (vector-ref fvec font-num))]
	   [letter (lookup l_no *alphadex*)])
      (letrec ([ltloop (lambda (lt fls)
			      (cond
				[(null? fls)
				 (error 'lookup-letter "cannot find letter ~s in font ~s" letter name)]
				[(equal? lt (caar fls))
				 (hex->bin (string->list (cadar fls)))]
				[else (ltloop lt (cdr fls))]))])
	     (ltloop letter fontls)))))

;---------------------------------------------------------------------
; load the gridfont database into scheme (using scan from load_fio)
; do this in two passes, first building an index and then filling
; in proper-length vector (this saves memory at the cost of time)

; in the end there are two main data structures, an index of vector
; positions *findex* ((<name-sym> <vec-no>) ...)
; and the vector itself *fonts* which has objects of the form
; (name-sym (a "HEXCODE") (b "HEXCODE")...(z "HEXCODE"))


(define load-data
  (lambda ()
    (let ((ip (open-input-file *data-file*)))
      (codemsg "Building index from ~s .." *data-file*)
      (set! *findex* (sort! skey (build-index ip '() 0)))      
      (set! *alphadex* '((1 a)(2 b)(3 c)(4 d)(5 e)(6 f)(7 g)(8 h)(9 i)(10 j)
			 (11 k)(12 l)(13 m)(14 n)(15 o)(16 p)(17 q)(18 r)
			 (19 s)(20 t)(21 u)(22 v)(23 w)(24 x)(25 y)(26 z)))
      (close-input-port ip)
      (let* ([np (open-input-file *data-file*)]        ; re-open file
	     [indx-ln (length *findex*)]
	     [data-vec (make-vector indx-ln)])
	(codemsg "Loading gridfonts ..")
	(set! *fonts* (load-fonts np data-vec 0)))
      (close-input-port ip))))

(define wind-to-symbol
  (lambda (port sym)
    (let ((x (scan 'a port)))
      (if (eof-object? x)
	  #f
	  (if (equal? x sym)
	      #t
	      (begin
		(wind-to-symbol port sym)))))))

; return a vector of lists with the gridfont data
(define load-fonts
  (lambda (port vec num)    
    (if (equal? #f (wind-to-symbol port 'font))
	(begin
	  (codemsg "~%")
	  vec)
	(let ([data (load-font port)])
	  (vector-set! vec num data)
	  (load-fonts port vec (add1 num))))))

(define load-font
  (lambda (port)
    (let* ([name (get-name port '())]
	   [font (get-gridfont port)])
      (append (list (string->symbol name)) font))))


; return an index to the database with the form ((<sym> <rnum>)...)
(define build-index
  (lambda (port ls num)
    (if (equal? #f (wind-to-symbol port 'font))
	(begin
	  (codemsg "~%")
	  ls)
	(let* ([name (get-name port '())]
	       [font (get-gridfont port)])                ; retrive and disgard
	  (build-index port (cons (list (string->symbol name) num)
				  ls) (add1 num))))))

; sort key for alphabetical sorting of entries in *findex*
(define skey
  (lambda (i1 i2)
    (let ([i1l (symbol->list (car i1))]
	  [i2l (symbol->list (car i2))])
      (letrec ([loop (lambda (i1l i2l)
		       (cond
			 [(null? i1l) #t]
			 [(null? i2l) #f]
			 [(char>? (car i1l)
			    (car i2l)) #f]
			 [(char<? (car i1l)
			    (car i2l)) #t]
			 [else (loop (cdr i1l) (cdr i2l))]))])
	(loop i1l i2l)))))
;-------------------------------------------------------------------
; help functions for loading the database

(define get-name
  (lambda (port ls)
    (let ((x (scan 'a port)))
      (if (equal? x ':)
	  (get-name port ls)
	  (if (equal? x 'creator)
	      (list-to-string (reverse ls) "")
	      (get-name port (cons x ls)))))))

(define get-gridfont
  (lambda (port)
    (wind-to-symbol port 'a)
    (let* ([ahx (substring (scan 's15 port) 1 15)]
	   [apr (list 'a ahx)]
	   [bpr (get-pair port)]
	   [cpr (get-pair port)]
	   [dpr (get-pair port)]
	   [epr (get-pair port)]
	   [fpr (get-pair port)]
	   [gpr (get-pair port)]
	   [hpr (get-pair port)]
	   [ipr (get-pair port)]
	   [jpr (get-pair port)]
	   [kpr (get-pair port)]
	   [lpr (get-pair port)]
	   [mpr (get-pair port)]
	   [npr (get-pair port)]
	   [opr (get-pair port)]
	   [ppr (get-pair port)]
	   [qpr (get-pair port)]
	   [rpr (get-pair port)]
	   [spr (get-pair port)]
	   [tpr (get-pair port)]
	   [upr (get-pair port)]
	   [vpr (get-pair port)]
	   [wpr (get-pair port)]
	   [xpr (get-pair port)]
	   [ypr (get-pair port)]
	   [zpr (get-pair port)])
      (list apr bpr cpr dpr epr fpr gpr hpr ipr jpr kpr lpr mpr npr opr ppr qpr rpr spr tpr upr vpr wpr xpr ypr zpr))))
	
(define get-pair
  (lambda (port)
    (let ([l (scan 'a port)]
	  [hx (substring (scan 's15 port) 1 15)])
      (list l hx))))
	  
(define lookup-current-font-index
  (lambda (current-font font-index)
    (cadr (list-ref font-index current-font))))
    

;------------------------------------------------------------------
; quanta computation based on mouse-click location

(define find-quantum
  (lambda (mx my)
    (cond
      [(inside? '(5.2 6.8 16.8 17.2) mx my) 0]
      [(inside? '(7.2 8.8 16.8 17.2) mx my) 1]
      [(inside? '(5.2 6.8 14.8 15.2) mx my) 2]
      [(inside? '(7.2 8.8 14.8 15.2) mx my) 3]
      [(inside? '(5.2 6.8 12.8 13.2) mx my) 4]
      [(inside? '(7.2 8.8 12.8 13.2) mx my) 5]
      [(inside? '(5.2 6.8 10.8 11.2) mx my) 6]
      [(inside? '(7.2 8.8 10.8 11.2) mx my) 7]
      [(inside? '(5.2 6.8 8.8 9.2) mx my) 8]
      [(inside? '(7.2 8.8 8.8 9.2) mx my) 9]
      [(inside? '(5.2 6.8 6.8 7.2) mx my) 10]
      [(inside? '(7.2 8.8 6.8 7.2) mx my) 11]
      [(inside? '(5.2 6.8 4.8 5.2) mx my) 12]
      [(inside? '(7.2 8.8 4.8 5.2) mx my) 13]
      [(inside? '(4.8 5.2 15 17) mx my) 14]
      [(inside? '(6.8 7.2 15 17) mx my) 15]
      [(inside? '(8.8 9.2 15 17) mx my) 16]
      [(inside? '(4.8 5.2 13 15) mx my) 17]
      [(inside? '(6.8 7.2 13 15) mx my) 18]
      [(inside? '(8.8 9.2 13 15) mx my) 19]
      [(inside? '(4.8 5.2 11 13) mx my) 20]
      [(inside? '(6.8 7.2 11 13) mx my) 21]
      [(inside? '(8.8 9.2 11 13) mx my) 22]
      [(inside? '(4.8 5.2 9 11) mx my) 23]
      [(inside? '(6.8 7.2 9 11) mx my) 24]
      [(inside? '(8.8 9.2 9 11) mx my) 25]
      [(inside? '(4.8 5.2 7 9) mx my) 26]
      [(inside? '(6.8 7.2 7 9) mx my) 27]
      [(inside? '(8.8 9.2 7 9) mx my) 28]
      [(inside? '(4.8 5.2 5 7) mx my) 29]
      [(inside? '(6.8 7.2 5 7) mx my) 30]
      [(inside? '(8.8 9.2 5 7) mx my) 31]
      [(or (inside? '(5.2 6 15.2 16) mx my)
	   (inside? '(6 6.8 16 16.8) mx my)) 32]
      [(or (inside? '(7.2 8 15.2 16) mx my)
	   (inside? '(8 8.8 16 16.8) mx my)) 33]
      [(or (inside? '(5.2 6 13.2 14) mx my)
	   (inside? '(6 6.8 14 14.8) mx my)) 34]
      [(or (inside? '(7.2 8 13.2 14) mx my)
	   (inside? '(8 8.8 14 14.8) mx my)) 35]
      [(or (inside? '(5.2 6 11.2 12) mx my)
	   (inside? '(6 6.8 12 12.8) mx my)) 36]
      [(or (inside? '(7.2 8 11.2 12) mx my)
	   (inside? '(8 8.8 12 12.8) mx my)) 37]
      [(or (inside? '(5.2 6 9.2 10) mx my)
	   (inside? '(6 6.8 10 10.8) mx my)) 38]
      [(or (inside? '(7.2 8 9.2 10) mx my)
	   (inside? '(8 8.8 10 10.8) mx my)) 39]
      [(or (inside? '(5.2 6 7.2 8) mx my)
	   (inside? '(6 6.8 8 8.8) mx my)) 40]
      [(or (inside? '(7.2 8 7.2 8) mx my)
	   (inside? '(8 8.8 8 8.8) mx my)) 41]
      [(or (inside? '(5.2 6 5.2 6) mx my)
	   (inside? '(6 6.8 6 6.8) mx my)) 42]
      [(or (inside? '(7.2 8 5.2 6) mx my)
	   (inside? '(8 8.8 6 6.8) mx my)) 43]
      [(or (inside? '(5.2 6 16 16.8) mx my)
	   (inside? '(6 6.8 15.2 16) mx my)) 44]
      [(or (inside? '(7.2 8 16 16.8) mx my)
	   (inside? '(8 8.8 15.2 16) mx my)) 45]
      [(or (inside? '(5.2 6 14 14.8) mx my)
	   (inside? '(6 6.8 13.2 14) mx my)) 46]
      [(or (inside? '(7.2 8 14 14.8) mx my)
	   (inside? '(8 8.8 13.2 14) mx my)) 47]
      [(or (inside? '(5.2 6 12 12.8) mx my)
	   (inside? '(6 6.8 11.2 12) mx my)) 48]
      [(or (inside? '(7.2 8 12 12.8) mx my)
	   (inside? '(8 8.8 11.2 12) mx my)) 49]
      [(or (inside? '(5.2 6 10 10.8) mx my)
	   (inside? '(6 6.8 9.2 10) mx my)) 50]
      [(or (inside? '(7.2 8 10 10.8) mx my)
	   (inside? '(8 8.8 9.2 10) mx my)) 51]
      [(or (inside? '(5.2 6 8 8.8) mx my)
	   (inside? '(6 6.8 7.2 8) mx my)) 52]
      [(or (inside? '(7.2 8 8 8.8) mx my)
	   (inside? '(8 8.8 7.2 8) mx my)) 53]
      [(or (inside? '(5.2 6 6 6.8) mx my)
	   (inside? '(6 6.8 5.2 6) mx my)) 54]
      [(or (inside? '(7.2 8 6 6.8) mx my)
	   (inside? '(8 8.8 5.2 6) mx my)) 55]
      [else #f])))

;-------------------------------------------------------------------
; standard definitions

; Load the global variable *fonts* with a copy of the gridfonts database
; *fonts* is a vector of lists of the various gridfonts (in hex)
(load-data)

;========================[ graphics code ]===============================
; graphics routines

; colors
(set! *grid-bg* "LemonChiffon1")
(set! *grid-fg* "NavyBlue")
(set! *grid-dots* "black")
(set! *buttons* "grey")
(set! *button-l* "black")
(set! *bborder-l* "slategrey")
(set! *bborder-u* "lightgrey")
(set! *label-font* "*misc-fixed-medium*10*")

; x and y position for the grid (q is quanta width)
(set! *x* 5)
(set! *y* 5)
(set! *q* 2)

; grid - 3x7 grid in the center of the window
(define mys-draw-grid
  (lambda (gc)
    (let f1 ((i (- *x* *q*)))
      (if (< i (+ *x* (* 2 *q*)))
	  (begin
	    (let f2 ((j (- *y* *q*)))
	      (if (< j (+ *y* (* 6 *q*)))
		  (begin
		    (draw! gc `(let-sgl ((foreground-color ,*grid-dots*)
					 (background-color ,*grid-dots*))
				 (filled-rectangle
				  (,(- (+ i *q*) .1) ,(- (+ j *q*) .1))
				  (,(+ (+ i *q*) .1) ,(+ (+ j *q*) .1)) ))
		      'omit-from-database)
		    (f2 (+ j *q*)))))
	    (f1 (+ i *q*)))))
    (*flush-event-queue*)))

; background for the grid
(define mys-draw-grid-back
  (lambda (gc)
    (draw! gc `(let-sgl ((background-color ,*grid-bg*)
			 (foreground-color ,*grid-bg*))
		 (filled-rectangle (0 0) (15 20)))
      'omit-from-database)))

; draw and erase routines for quanta.  these routines expect a list of
; quanta that are "on"
(define mys-draw-quanta
  (lambda (gc ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (mys-draw-quantum gc (car ls))
	    (mys-draw-quanta gc (cdr ls))))))

(define mys-erase-quanta
  (lambda (gc ls)
    (cond
      ((null? ls) (*flush-event-queue*))
      (else (mys-erase-quantum gc (car ls))
	    (mys-erase-quanta gc (cdr ls))))))

; draw or erase a single quantum using the endpoints list
(define mys-draw-quantum
  (lambda (gc q)
    (let* ((endpoints (lookup q *mys-real-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color ,*grid-fg*)
			   (line-width 4))
		   (line ,end1 ,end2))
	'omit-from-database))))

(define mys-erase-quantum
  (lambda (gc q)
    (let* ((endpoints (lookup q *mys-real-endpoints*))
	   (end1 (car endpoints))
	   (end2 (cadr endpoints)))
      (draw! gc `(let-sgl ((foreground-color ,*grid-bg*)
			   (line-width 4))
		   (line ,end1 ,end2))
	'omit-from-database))))

; list of quanta -> endpoints associations for display
(set! *mys-real-endpoints*
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

;---------------------------------------------------------------------------
; button drawing code

; up font button
(define draw-up-fbutton
  (lambda ()
    (draw! *mlw*
	   `(let-sgl ((foreground-color ,*buttons*))
	      (filled-rectangle (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 3.3)))
		(,(- *x* *q*) ,(+ *y* (* *q* 4.3)))))
	   'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(- *x* *q*) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 4.3))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 3.3))))
		    ; arrow
		    (let-sgl ((font ,*label-font*)
			      (foreground-color ,*button-l*)
			      (origin (,(- *x* (* 1.4 *q*)) ,(+ *y* (* *q* 3.5)))))
		      (text "^")))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 4.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 4.3)))))
      'omit-from-database)))

; invert button for push in look
(define flash-up-fbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(- *x* *q*) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 4.3))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 3.3)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 4.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 4.3)))))
      'omit-from-database)))

; make it back to normal
(define unflash-up-fbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(- *x* *q*) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 4.3))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 3.3)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 4.3)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 4.3)))))
      'omit-from-database)))

; down font button
(define draw-down-fbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*buttons*))
		    (filled-rectangle (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 1.6)))
		      (,(- *x* *q*) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(- *x* *q*) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 2.6))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 1.6))))
		    ; arrow
		    (let-sgl ((font ,*label-font*)
			      (foreground-color ,*button-l*)
			      (origin (,(- *x* (* 1.4 *q*)) ,(+ *y* (* *q* 1.95)))))
		      (text "v")))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 2.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)))

(define flash-down-fbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(- *x* *q*) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 2.6))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 1.6)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 2.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)))

(define unflash-down-fbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(- *x* *q*) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 2.6))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 1.6)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(- *x* (* 1.5 *q*)) ,(+ *y* (* *q* 2.6)))
			  (,(- *x* *q*) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)))

; up letter button
(define draw-up-lbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*buttons*))
		    (filled-rectangle (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 3.3)))
		      (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 4.3)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 3.3))))
		    ; arrow
		    (let-sgl ((font ,*label-font*)
			      (foreground-color ,*button-l*)
			      (origin (,(+ *x* (* 3.1 *q*)) ,(+ *y* (* *q* 3.5)))))
		      (text "^")))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 4.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 4.3)))))
      'omit-from-database)))

(define flash-up-lbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 3.3)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 4.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 4.3)))))
      'omit-from-database)))

(define unflash-up-lbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 3.3)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 3.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 4.3))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 4.3)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 4.3)))))
      'omit-from-database)))

; down letter button
(define draw-down-lbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*buttons*))
		    (filled-rectangle (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 1.6)))
		      (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 1.6))))
		    ; arrow
		    (let-sgl ((font ,*label-font*)
			      (foreground-color ,*button-l*)
			      (origin (,(+ *x* (* 3.1 *q*)) ,(+ *y* (* *q* 1.95)))))
		      (text "v")))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 2.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)))

(define flash-down-lbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 1.6)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 2.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)))

(define unflash-down-lbutton
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 1.6)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 1.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 2.6))))
		    (line (,(+ *x* (* 3.5 *q*)) ,(+ *y* (* *q* 2.6)))
			  (,(+ *x* (* 3 *q*)) ,(+ *y* (* *q* 2.6)))))
      'omit-from-database)))

; down letter button
(define draw-accept-button
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*buttons*))
		    (filled-rectangle (,*x* ,(- *y* (* *q* 1.5)))
				      (,(+ *x* (* 2 *q*))
				       ,(- *y* (* 0.5 *q*)))))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 1.5)))
			  (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 0.5))))
		    (line (,*x* ,(- *y* (* *q* 1.5)))
			  (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 1.5))))
		    ; arrow
		    (let-sgl ((font ,*label-font*)
			      (foreground-color ,*button-l*)
			      (origin (,(+ *x* (* 0.1 *q*))
				       ,(- *y* (* *q* 1.15)))))
		      (text "accept")))
      'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,*x* ,(- *y* (* *q* 1.5)))
			  (,*x* ,(- *y* (* *q* 0.5))))
		    (line (,*x* ,(- *y* (* *q* 0.5)))
			  (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 0.5)))))
      'omit-from-database)))

(define flash-accept-button
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
		    (line (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 1.5)))
			  (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 0.5))))
		    (line (,*x* ,(- *y* (* *q* 1.5)))
			  (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 1.5)))))
	   'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
			   (line (,*x* ,(- *y* (* *q* 1.5)))
				 (,*x* ,(- *y* (* *q* 0.5))))
			   (line (,*x* ,(- *y* (* *q* 0.5)))
				 (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 0.5)))))
	   'omit-from-database)))   

(define unflash-accept-button
  (lambda ()
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-l*))
		    (line (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 1.5)))
			  (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 0.5))))
		    (line (,*x* ,(- *y* (* *q* 1.5)))
			  (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 1.5)))))
	   'omit-from-database)
    (draw! *mlw* `(let-sgl ((foreground-color ,*bborder-u*))
			   (line (,*x* ,(- *y* (* *q* 1.5)))
				 (,*x* ,(- *y* (* *q* 0.5))))
			   (line (,*x* ,(- *y* (* *q* 0.5)))
				 (,(+ *x* (* 2 *q*)) ,(- *y* (* *q* 0.5)))))
	   'omit-from-database))) 

;---------------------------------------------------------------------------
; draw the gridletter pointed to by the global pointers *current-font*
; and *current-letter* (look it up in the database first) also draw the
; name of the font.  Set *quanta-list* for bit-level tweaking.
(define mys-draw-gridletter+name
  (lambda ()
    (let ([letter
	    (q-list (lookup-ptr *current-font* *current-letter* *fonts*) 0)])
      (set! *quanta-list* letter)
      (mys-draw-quanta *mlw* letter))
    (draw! *mlw* `(let-sgl ((foreground-color ,*grid-fg*)
			    (font ,*label-font*)
			    (origin (,(- *x* (* 2 *q*)) ,(- *y* (* 2.2 *q*)))))
		    (text ,(string-append "font: "
			     (symbol->string
			       (car (list-ref *findex* *current-font*))))))
      'omit-from-database)))

; draw only a gridletter with no font name
(define mys-draw-gridletter
  (lambda ()
    (let ([letter
	    (q-list (lookup-ptr *current-font* *current-letter* *fonts*) 0)])
      (set! *quanta-list* letter)
      (mys-draw-quanta *mlw* letter))))

; refresh a gridletter that has had a quanta change
(define refresh-gridletter
  (lambda ()
    (mys-draw-quanta *mlw* *quanta-list*)))

; erase a gridletter and font name
(define erase-gridletter+name
  (lambda ()
    (mys-erase-quanta *mlw* *quanta-list*)
    (mys-draw-grid *mlw*)
    (draw! *mlw* `(let-sgl ((foreground-color ,*grid-bg*))
		    (filled-rectangle
		      (,(- *x* (* .5 *q*)) ,(- *y* (* 2.5 *q*)))
		      (,(+ *x* (* 5 *q*)) ,(- *y* (* 1.6 *q*)))))
      'omit-from-database)))

; erase only a gridletter leaving font name intact
(define erase-gridletter
  (lambda ()
    (mys-erase-quanta *mlw* *quanta-list*)
    (mys-draw-grid *mlw*)))

; draw the font and letter labels
(define draw-labels
  (lambda ()
    (draw! *mlw* `(let-sgl
		    ((foreground-color ,*grid-fg*)
		     (font ,*label-font*)
		     (origin (,(- *x* (* 1.9 *q*)) ,(+ *y* (* 2.75 *q*)))))
		    (text "font"))
      'omit-from-database)
    (draw! *mlw* `(let-sgl
		    ((foreground-color ,*grid-fg*)
		     (font ,*label-font*)
		     (origin (,(+ *x* (* 2.4 *q*)) ,(+ *y* (* 2.75 *q*)))))
		    (text "letter"))
      'omit-from-database)))

;---------------------------------------------------------------------------
; help functions (non-graphical)

; this should be in any scheme!
(define member?
  (lambda (item ls)
    (if (member item ls)
        #t
        #f)))

; make a list of on-quanta (by number) for the letter plotter
(define q-list
  (lambda (bits c)
    (cond
      ((null? bits) '())
      ((= (car bits) 1) (cons c
                              (q-list (cdr bits) (add1 c))))
      (else (q-list (cdr bits) (add1 c))))))

; good old lookup...should be replaced by select
(define lookup
  (lambda (item ls)
    (cond
      ((null? ls) (error 'lookup "item ~s not found." item))
      ((equal? (caar ls) item) (cadar ls))
      (else (lookup item (cdr ls))))))

; get the position of a particular symbol in a list
(define find-pos
  (lambda (item ls num)
    (cond
      [(null? ls) (error 'find-pos "item ~s not found." item)]
      [(equal? (caar ls) item) num]
      [else (find-pos item (cdr ls) (add1 num))])))

; move global pointers up or down (length dependant) and zero-based
(define up-font
  (lambda ()
    (set! *current-font* (if (= (sub1 (length *findex*)) *current-font*)
			     0
			     (add1 *current-font*)))))

(define down-font
  (lambda ()
    (set! *current-font* (if (= 0 *current-font*)
			     (sub1 (length *findex*))
			     (sub1 *current-font*)))))

(define up-letter
  (lambda ()
    (set! *current-letter* (if (= 26 *current-letter*)
			       1
			       (add1 *current-letter*)))))

(define down-letter
  (lambda ()
    (set! *current-letter* (if (= 1 *current-letter*)
			       26
			       (sub1 *current-letter*)))))

; if bit is in list, delete it, else add it and reset *quanta-list*
(define fix-bit-list
  (lambda (bitls bit)
    (if (member? bit bitls)
	(set! *quanta-list* (remove bit bitls))
	(set! *quanta-list* (sort < (cons bit bitls))))))

; turn a quanta-list into an n-bit list
; example: (mnbl '(0 1 2) 0 '() 56)
(define make-n-bit-list
  (lambda (q-list ctr bits n)
    (cond
      ((= ctr n) (reverse bits))
      ((member? ctr q-list)
         (make-n-bit-list q-list (add1 ctr) (cons 1 bits) n))
      (else (make-n-bit-list q-list (add1 ctr) (cons 0 bits) n)))))


;===========================================================================
; call the graphics routines and fire up the event handler

; test pointer click location against a rectangle
(define inside?
  (lambda (pts x y)
    (if (and (>= x (car pts))
	     (<= x (cadr pts))
	     (>= y (caddr pts))
	     (<= y (cadddr pts)))
	#t
	#f)))

; routine to handle mouse clicks
(define b-handler
  (lambda (event gc pics px py)
    (if (= (=> 'get-button event) 1)
	(begin
;	  (codemsg "button1: ~s ~s~%" px py)
	  (cond
	    [(inside? *fu* px py) (flash-up-fbutton)
	                          (erase-gridletter+name)
	                          (up-font)
	                          (mys-draw-gridletter+name)
				  (unflash-up-fbutton)]
	    [(inside? *fd* px py) (flash-down-fbutton)
	                          (erase-gridletter+name)
	                          (down-font)
				  (mys-draw-gridletter+name)
				  (unflash-down-fbutton)]
	    [(inside? *lu* px py) (flash-up-lbutton)
	                          (erase-gridletter)
	                          (up-letter)
				  (mys-draw-gridletter)
				  (unflash-up-lbutton)]
	    [(inside? *ld* px py) (flash-down-lbutton)
	                          (erase-gridletter)
	                          (down-letter)
				  (mys-draw-gridletter)
				  (unflash-down-lbutton)]
	    [(inside? *ab* px py)
	     (flash-accept-button)
	     (*flush-event-queue*)
	     (let ([qlst *quanta-list*])
	       (draw! *mlw*
		      `(let-sgl
			((foreground-color ,*grid-fg*)
			 (font ,*label-font*)
			 (origin
			  (,(- *x* (* 2.2 *q*)) ,(+ *y* (* 6.5 *q*)))))
			(text "Mystery Letter Accepted"))
		      'omit-from-database)
	       ; erase buttons
	       (draw! *mlw* `(let-sgl ((foreground-color ,*grid-bg*))
				      (filled-rectangle (1 8)(4 14)))
		      'omit-from-database)
	       (draw! *mlw* `(let-sgl ((foreground-color ,*grid-bg*))
				      (filled-rectangle (10 8)(14 14)))
		      'omit-from-database)
	       (unflash-accept-button)
	       (draw! *mlw* `(let-sgl ((foreground-color ,*grid-bg*))
				      (filled-rectangle (4 1.9)(10 4.1)))
		      'omit-from-database) 
	       (set! *mystery-letter*
		     (append (list (lookup *current-letter* *alphadex*))
			     (make-n-bit-list qlst 0 '() 56)))
	       (sgl-remove-pick *pick-id*)
	       (codemsg "Mystery letter loaded.~%")
	       (collect)
	       (toplevel-k #f))]
	    [else
	     (let ([quantum (find-quantum px py)])
;		(codemsg "x=~s  y=~s~%" px py)
		(if quantum
		    (begin
;		      (codemsg "Clicked on ~s~%" quantum)
		      (if (member? quantum *quanta-list*)
			    (mys-erase-quantum *mlw* quantum)
			    (mys-draw-quantum *mlw* quantum))
		      (mys-draw-grid *mlw*)
		      (draw! *mlw*
			     `(let-sgl ((foreground-color ,*grid-bg*))
				(filled-rectangle
				  (,(- *x* (* .5 *q*)) ,(- *y* (* 2.5 *q*)))
				  (,(+ *x* (* 5 *q*)) ,(- *y* (* 1.6 *q*)))))
			     'omit-from-database)
		      (fix-bit-list *quanta-list* quantum)
		      (refresh-gridletter)
;		      (codemsg "ql ~s~%" *quanta-list*)
		      )
		    (begin
		      (printf "")
		      (flush-output-port))))])))
    (if (or (= (=> 'get-button event) 2)
	    (= (=> 'get-button event) 3))
	(begin
	  (printf "")
	  (flush-output-port)))))

; blank function for mouse click releases
(define r-handler
  (lambda (event gc px py)
    #f))

(define get-mystery
  (lambda ()
    (set! *mystery-letter* #f)
    (set! *mlw* (create-graphics-window 150 200 15 20))
    (if (not (and (top-level-bound? '*current-font*)
		  (top-level-bound? '*current-letter*)))
	(begin
	  ; default gridletter is "standard square" `b'
	  (set! *current-font* (find-pos '|standard square| *findex* 0))
	  (set! *current-letter* 2)))
    ; invoke the graphics
    (mys-draw-grid-back *mlw*)
    (mys-draw-grid *mlw*)
    (draw-up-fbutton)
    (draw-down-fbutton)
    (draw-up-lbutton)
    (draw-down-lbutton)
    (draw-accept-button)
    (draw-labels)
    (mys-draw-gridletter+name)
    ; button placement data (rough, non-generic...should use *q* *x* and *y*)
    (set! *fu* '(2 3 11.6 13.6))
    (set! *fd* '(2 3 8.2 10.2))
    (set! *lu* '(11 12 11.6 13.6))
    (set! *ld* '(11 12 8.2 10.2))
    (set! *ab* '(5 9 2.0 4.0))
    (set! *pick-id* (sgl-pick *mlw* b-handler r-handler))))
    

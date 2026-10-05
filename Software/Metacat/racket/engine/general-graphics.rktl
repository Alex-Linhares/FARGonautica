;;=============================================================================
;; Copyright (c) 1999, 2003 by James B. Marshall
;;
;; This file is part of Metacat.
;;
;; Metacat is based on Copycat, which was originally written in Common
;; Lisp by Melanie Mitchell.
;;
;; Metacat is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or (at your option) any later
;; version.
;;
;; Metacat is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;=============================================================================
;; Ported to Racket, 2026: the engine's part of general-graphics.ss, i.e.
;; the procedures that build SGL expressions (pexps) and the text helpers,
;; which the model calls when %workspace-graphics% is on (group, bridge and
;; rule pexps) and which rules.ss calls on every rule
;; (find-next-space-position).  They draw nothing and need no toolkit.  The
;; window part of the file (make-graphics-window, the scrollable text
;; window, the resize listener, the default colours and font) is in
;; racket/gui/general-graphics.rktl.  Apart from that split, verbatim; see
;; docs/porting-notes.md, item 13.

;; port: metacat.ss defines these for the whole program; the dotted and
;; dashed shapes below read them (the SGL interpreter, racket/gui/sgl.rkt,
;; has its own copies)
(define *platform* 'linux)
(define *tcl/tk-version* 8.5)
(define *tcl/tk-version-8_3?* (>= *tcl/tk-version* 8.3))


(define pi 3.14159265359)
(define pi/180 (/ pi 180))
(define 180/pi (/ 180 pi))

(define remove-leading-blanks
  (lambda (line)
    (let ((len (string-length line)))
      (letrec
	((find-beginning
	   (lambda (i)
	     (cond
	       ((= i len) line)
	       ((char-whitespace? (string-ref line i)) (find-beginning (+ i 1)))
	       (else (substring line i len))))))
	(find-beginning 0)))))

(define break-into-lines
  (lambda (g font max-line-length text-string)
    (let combine ((previous-lines '())
		  (current-line "")
		  (words-left (separate-into-words text-string))
		  (space-left max-line-length))
      (if (null? words-left)
	(filter-out
	  (lambda (s) (string=? s ""))
	  (reverse (cons current-line previous-lines)))
	(let* ((next-word (1st words-left))
	       (word-length (tell g 'get-string-width next-word font)))
	  (cond
	    ((< word-length space-left)
	     (combine
	       previous-lines
	       (string-append current-line next-word)
	       (rest words-left)
	       (- space-left word-length)))
	    ((>= word-length max-line-length)
	     (combine
	       (cons next-word (cons current-line previous-lines))
	       ""
	       (rest words-left)
	       max-line-length))
	    (else
	      (combine
		(cons current-line previous-lines)
		""
		words-left
		max-line-length))))))))

(define separate-into-words
  (lambda (text-string)
    (let* ((next-pos (find-next-space-position text-string 0))
	   (next-word (string-append " " (substring text-string 0 next-pos)))
	   (length (string-length text-string)))
      (if (= next-pos length)
	(list next-word)
	(cons next-word
	  (separate-into-words (substring text-string (+ next-pos 1) length)))))))

(define find-next-space-position
  (lambda (s i)
    (cond
      ((>= i (string-length s)) (string-length s))
      ((char=? (string-ref s i) #\space) i)
      (else (find-next-space-position s (+ i 1))))))

;;------------------------------- Circles etc. -------------------------------

(define circle
  (lambda (center diameter)
    `(arc ,center (,diameter ,diameter) 0 360)))

(define disk
  (lambda (center diameter)
    `(filled-arc ,center (,diameter ,diameter) 0 360)))

(define pie-slice
  (lambda (center diameter start-deg sweep-deg)
    `(filled-arc ,center (,diameter ,diameter) ,start-deg ,sweep-deg)))

;;--------------------------------- Boxes ------------------------------------

(define outline-box
  (lambda (x0 y0 x1 y1)
    `(rectangle (,x0 ,y0) (,x1 ,y1))))

(define solid-box
  (lambda (x0 y0 x1 y1)
    `(filled-rectangle (,x0 ,y0) (,x1 ,y1))))

(define centered-ovaloid
  (lambda (xc yc width height ovalness)
    (let* ((x1 (- xc (* 1/2 width)))
	   (x2 (+ xc (* 1/2 width)))
	   (interior-height (* height (- 1 ovalness)))
	   (y1 (- yc (* 1/2 interior-height)))
	   (y2 (+ yc (* 1/2 interior-height)))
	   (exterior-height (- height interior-height)))
      `(let-sgl ()
	 (arc (,xc ,y2) (,width ,exterior-height) 0 180)
	 (line (,x2 ,y1) (,x2 ,y2))
	 (arc (,xc ,y1) (,width ,exterior-height) 180 180)
	 (line (,x1 ,y2) (,x1 ,y1))))))

(define filled-centered-ovaloid
  (lambda (xc yc width height ovalness)
    (let* ((x1 (- xc (* 1/2 width)))
	   (x2 (+ xc (* 1/2 width)))
	   (interior-height (* height (- 1 ovalness)))
	   (y1 (- yc (* 1/2 interior-height)))
	   (y2 (+ yc (* 1/2 interior-height)))
	   (exterior-height (- height interior-height)))
      `(let-sgl ()
	 (filled-arc (,xc ,y2) (,width ,exterior-height) 0 180)
	 (filled-rectangle (,x1 ,y1) (,x2 ,y2))
	 (filled-arc (,xc ,y1) (,width ,exterior-height) 180 180)))))

(define centered-rounded-box
    (lambda (xc yc width height corner-radius)
      (let* ((left (- xc (* 1/2 width)))
	     (right (+ xc (* 1/2 width)))
	     (top (+ yc (* 1/2 height)))
	     (bottom (- yc (* 1/2 height)))
	     (x1 (+ left corner-radius))
	     (x2 (- right corner-radius))
	     (y1 (+ bottom corner-radius))
	     (y2 (- top corner-radius))
	     (corner-diameter (* 2 corner-radius)))
	`(let-sgl ()
	   (line (,x1 ,bottom) (,x2 ,bottom))
	   (arc (,x2 ,y1) (,corner-diameter ,corner-diameter) 270 90)
	   (line (,right ,y1) (,right ,y2))
	   (arc (,x2 ,y2) (,corner-diameter ,corner-diameter) 0 90)
	   (line (,x2 ,top) (,x1 ,top))
	   (arc (,x1 ,y2) (,corner-diameter ,corner-diameter) 90 90)
	   (line (,left ,y2) (,left ,y1))
	   (arc (,x1 ,y1) (,corner-diameter ,corner-diameter) 180 90)))))

(define filled-centered-rounded-box
  (lambda (xc yc width height corner-radius pixel)
      (let* ((left (- xc (* 1/2 width)))
	     (right (+ xc (* 1/2 width)))
	     (top (+ yc (* 1/2 height)))
	     (bottom (- yc (* 1/2 height)))
	     (x1 (+ left corner-radius))
	     (x2 (- right corner-radius))
	     (y1 (+ bottom corner-radius))
	     (y2 (- top corner-radius))
	     (corner-diameter (* 2 corner-radius)))
	`(let-sgl ()
	   (filled-rectangle (,left ,(- y1 pixel)) (,right ,(+ y2 pixel)))
	   (filled-rectangle (,(- x1 pixel) ,bottom) (,(+ x2 pixel) ,top))
	   (filled-arc (,x2 ,y1) (,corner-diameter ,corner-diameter) 270 90)
	   (filled-arc (,x2 ,y2) (,corner-diameter ,corner-diameter) 0 90)
	   (filled-arc (,x1 ,y2) (,corner-diameter ,corner-diameter) 90 90)
	   (filled-arc (,x1 ,y1) (,corner-diameter ,corner-diameter) 180 90)))))

;;-------------------------------- Octagons ----------------------------------

(define centered-octagon
  (lambda (xc yc width)
    (let* ((a (* 1/2 width))
	   (b (* a (/ 1 (+ 1 (sqrt 2)))))
	   (x1 (- xc a))
	   (x2 (- xc b))
	   (x3 (+ xc b))
	   (x4 (+ xc a))
	   (y1 (- yc a))
	   (y2 (- yc b))
	   (y3 (+ yc b))
	   (y4 (+ yc a)))
      `(polygon
	 (,x2 ,y1) (,x3 ,y1) (,x4 ,y2) (,x4 ,y3) (,x3 ,y4)
	 (,x2 ,y4) (,x1 ,y3) (,x1 ,y2)))))

(define filled-centered-octagon
  (lambda (xc yc width)
    (let* ((a (* 1/2 width))
	   (b (* a (/ 1 (+ 1 (sqrt 2)))))
	   (x1 (- xc a))
	   (x2 (- xc b))
	   (x3 (+ xc b))
	   (x4 (+ xc a))
	   (y1 (- yc a))
	   (y2 (- yc b))
	   (y3 (+ yc b))
	   (y4 (+ yc a)))
      `(filled-polygon
	 (,x2 ,y1) (,x3 ,y1) (,x4 ,y2) (,x4 ,y3) (,x3 ,y4)
	 (,x2 ,y4) (,x1 ,y3) (,x1 ,y2)))))

;;-------------------------- Dotted lines & boxes ----------------------------

(define %dot-interval% 1/125)

(define dotted-line
  (lambda (x1 y1 x2 y2 approx-interval-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(polypoints ,@(dotted-line-points x1 y1 x2 y2 approx-interval-length))
      `(let-sgl ((line-style dotted))
	 (line (,x1 ,y1) (,x2 ,y2))))))

(define dotted-box
  (lambda (x1 y1 x2 y2 approx-interval-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(polypoints
	 ,@(dotted-line-points x1 y1 x1 y2 approx-interval-length)
	 ,@(dotted-line-points x1 y2 x2 y2 approx-interval-length)
	 ,@(dotted-line-points x2 y1 x2 y2 approx-interval-length)
	 ,@(dotted-line-points x1 y1 x2 y1 approx-interval-length))
      `(let-sgl ((line-style dotted))
	 (rectangle (,x1 ,y1) (,x2 ,y2))))))

(define dotted-line-points
  (lambda (x1 y1 x2 y2 approx-interval-length)
    (let* ((p1 (make-rectangular x1 y1))
	   (p2 (make-rectangular x2 y2))
	   (l (- p2 p1))
	   (n (max 1 (round (/ (magnitude l) approx-interval-length))))
	   (exact-interval-length (/ (magnitude l) n))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,x1 ,y1)
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((dotted-line-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (dotted-line-points
		 (sub1 i)
		 (cons (coord-transform
			 (make-rectangular (* i exact-interval-length) 0))
		   points))))))
	(dotted-line-points n '())))))

;;----------------------------- Dashed lines & boxes ----------------------------------

;; density parameter ranges from 0 (no dashes) to 1 (no spaces).
;; dash-length parameter is the length of an individual dash in world-coordinates.
;;
;; Examples of density and dash-length parameters:
;;
;;   low density, short dash-length  |--          --          --         --          --|
;;                                   |                                                 |
;;   low density, long dash-length   |--------            --------             --------|
;;                                   |                                                 |
;;   high density, short dash-length |--  --  --  --  --  --  --  --  --  --  --  -- --|
;;                                   |                                                 |
;;   high density, long dash-length  |--------  ---------  --------  --------  --------|

(define %dash-length% 1/200)
(define %dash-density% 1/2)

(define dashed-line
  (lambda (x1 y1 x2 y2 density approx-dash-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(line ,@(dashed-line-points x1 y1 x2 y2 density approx-dash-length))
      `(let-sgl ((line-style dashed))
	 (line (,x1 ,y1) (,x2 ,y2))))))

(define dashed-box
  (lambda (x1 y1 x2 y2 density approx-dash-length)
    (if (or %nice-graphics% (not *tcl/tk-version-8_3?*))
      `(line
	,@(dashed-line-points x1 y1 x1 y2 density approx-dash-length)
	,@(dashed-line-points x1 y2 x2 y2 density approx-dash-length)
	,@(dashed-line-points x2 y1 x2 y2 density approx-dash-length)
	,@(dashed-line-points x1 y1 x2 y1 density approx-dash-length))
      `(let-sgl ((line-style dashed))
	 (rectangle (,x1 ,y1) (,x2 ,y2))))))

(define dashed-line-points
  (lambda (x1 y1 x2 y2 density approx-dash-length)
    (let* ((p1 (make-rectangular x1 y1))
	   (p2 (make-rectangular x2 y2))
	   (l (- p2 p1))
	   (total-dash-length (* (magnitude l) density))
	   (total-space-length (* (magnitude l) (- 1 density)))
	   (n (max 3 (round (/ total-dash-length approx-dash-length))))
	   (dash-length (/ total-dash-length n))
	   (space-length (/ total-space-length (sub1 n)))
	   (interval-length (+ dash-length space-length))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,x1 ,y1)
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((dashed-line-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (dashed-line-points
		 (sub1 i)
		 (cons (coord-transform
			 (make-rectangular (* i interval-length) 0))
		   (cons (coord-transform
			   (make-rectangular (+ (* i interval-length) dash-length) 0))
		     points)))))))
	(dashed-line-points (sub1 n) '())))))

;;---------------------------- Zigzag lines -------------------------------

(define zigzag-line
  (lambda (p1 p2 approx-zigzag-length sign)
    `(polyline ,@(zigzag-line-points p1 p2 approx-zigzag-length sign))))


(define centered-zigzag-line
  (lambda (p1 p2 approx-zigzag-length sign)
    `(polyline ,@(centered-zigzag-line-points p1 p2 approx-zigzag-length sign))))

;; zigzag-line-points:
;;
;;            /\  /\  /\  /\  /\  /\  /\  /\
;; sign + .../  \/  \/  \/  \/  \/  \/  \/  \
;;               ---- (approx-zigzag-length)
;; sign - ...
;;           \  /\  /\  /\  /\  /\  /\  /\  /
;;            \/  \/  \/  \/  \/  \/  \/  \/ 

(define zigzag-line-points
  (lambda (p1 p2 approx-zigzag-length sign)
    (let* ((l (- p2 p1))
	   (n (max 1 (round (/ (magnitude l) approx-zigzag-length))))
	   (period (make-rectangular (/ (magnitude l) n) 0))
	   (delta (/ (magnitude period) 2))
	   (zig (make-rectangular delta (sign delta)))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,(x-coord p1) ,(y-coord p1))
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((zigzag-line-points
	   (lambda (i sgl-points)
	     (if (< i 0)
	       sgl-points
	       (zigzag-line-points
		 (sub1 i)
		 (cons (coord-transform (* i period))
		   (cons (coord-transform (+ (* i period) zig))
		     sgl-points)))))))
	(zigzag-line-points (sub1 n) `((,(x-coord p2) ,(y-coord p2))))))))

;; centered-zigzag-line-points:
;;
;; sign + ... /\  /\  /\  /\  /\  /\  /\  /\ 
;;              \/  \/  \/  \/  \/  \/  \/  \/
;;               ---- (approx-zigzag-length)
;; sign - ...   /\  /\  /\  /\  /\  /\  /\  /\
;;            \/  \/  \/  \/  \/  \/  \/  \/ 

(define centered-zigzag-line-points
  (lambda (p1 p2 approx-zigzag-length sign)
    (let* ((l (- p2 p1))
	   (n (round (/ (magnitude l) approx-zigzag-length)))
	   (period (make-rectangular (/ (magnitude l) n) 0))
	   (delta (/ (magnitude period) 4))
	   (opp (if (eq? sign +) - +))
	   (zig (make-rectangular delta (sign delta)))
	   (zag (make-rectangular (* 3 delta) (opp delta)))
	   (coord-transform
	     (lambda (p)
	       (if (zero? p)
		 `(,(x-coord p1) ,(y-coord p1))
		 (let ((p-prime
			 (+ p1 (make-polar (magnitude p) (+ (angle l) (angle p))))))
		   `(,(x-coord p-prime) ,(y-coord p-prime)))))))
      (letrec
	((centered-zigzag-line-points
	   (lambda (i sgl-points)
	     (if (< i 0)
	       (cons `(,(x-coord p1) ,(y-coord p1)) sgl-points)
	       (centered-zigzag-line-points
		 (sub1 i)
		 (cons (coord-transform (+ (* i period) zig))
		   (cons (coord-transform (+ (* i period) zag))
		     sgl-points)))))))
	(centered-zigzag-line-points (sub1 n) `((,(x-coord p2) ,(y-coord p2))))))))

;;-------------------------------- Jagged lines --------------------------------
;; not used in Metacat

(define jagged-line
  (lambda (x1 y1 x2 y2 approx-jag-width)
    `(polyline ,@(jagged-line-points x1 y1 x2 y2 approx-jag-width))))

(define jagged-line-points
  (lambda (x1 y1 x2 y2 approx-jag-width)
    (let* ((x-len (- x2 x1))
	   (y-len (- y2 y1))
	   (short-len (min (abs x-len) (abs y-len)))
	   (n (max 5 (round (/ short-len approx-jag-width))))
	   (x-delta (/ x-len n))
	   (y-delta (/ y-len (add1 n))))
      (letrec
	((jagged-line-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (jagged-line-points
		 (sub1 i)
		 (let ((x (+ x1 (* i x-delta)))
		       (y (+ y1 (* i y-delta))))
		   (cons `(,x ,y) (cons `(,x ,(+ y y-delta)) points))))))))
	(jagged-line-points n '())))))

;;------------------------------- Circular arcs ---------------------------------
;; not used in Metacat

;; These routines draw circular arcs clockwise from point x1,y1 to point x2,y2.
;; height is the maximum height of the arc from the line connecting the endpoints.

(define circular-arc
  (lambda (x1 y1 x2 y2 height)
    (if (<= height 0)
      `(line (,x1 ,y1) (,x2 ,y2))
      (let* ((p1 (make-rectangular x1 y1))
	     (p2 (make-rectangular x2 y2))
	     (l (- p2 p1))
	     (radius (+ (/ (^2 (magnitude l)) (* 8 height)) (/ height 2)))
	     (d (* 2 radius))
	     (alpha (* 2 (acos (- 1 (/ height radius)))))
	     (beta (/ (- pi alpha) 2))
	     (origin (+ p1 (make-polar radius (- (angle l) beta))))
	     (start (+ beta (angle l)))
	     (start-deg (* 180/pi start))
	     (alpha-deg (* 180/pi alpha)))
	`(arc (,(x-coord origin) ,(y-coord origin)) (,d ,d) ,start-deg ,alpha-deg)))))

(define dotted-circular-arc
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (cond
      ((<= height 0) (dotted-line x1 y1 x2 y2 approx-interval-length))
      ((or %nice-graphics% (not *tcl/tk-version-8_3?*))
	`(polypoints
	   ,@(circular-arc-points x1 y1 x2 y2 height approx-interval-length)))
      (else `(let-sgl ((line-style dotted))
	       ,(circular-arc x1 y1 x2 y2 height))))))

(define dashed-circular-arc
  (lambda (x1 y1 x2 y2 height)
    (cond
      ((<= height 0) (dashed-line x1 y1 x2 y2 %dash-density% %dash-length%))
      ;; dashed lines don't seem to work on the Mac even with Tcl/Tk 8.4.4,
      ;; so just use dashed-polypoints for now
      ((and *tcl/tk-version-8_3?* (not (eq? *platform* 'macintosh)))
       `(let-sgl ((line-style dashed))
	  ,(circular-arc x1 y1 x2 y2 height)))
      (else `(dashed-polypoints
	       ,@(circular-arc-points x1 y1 x2 y2 height %dot-interval%))))))

(define circular-arc-points
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (let* ((p1 (make-rectangular x1 y1))
	   (p2 (make-rectangular x2 y2))
	   (l (- p2 p1))
	   (r (+ (/ (^2 (magnitude l)) (* 8 height)) (/ height 2)))
	   (alpha (* 2 (acos (- 1 (/ height r)))))
	   (beta (/ (- pi alpha) 2))
	   (origin (+ p1 (make-polar r (- (angle l) beta))))
	   (start (+ beta (angle l)))
	   (arc-length (* r alpha))
	   (n (max 3 (round (/ arc-length approx-interval-length))))
	   (angle-delta (/ alpha n))
	   (arc-point
	     (lambda (i)
	       (let ((p (+ origin (make-polar r (+ (* i angle-delta) start)))))
		 `(,(x-coord p) ,(y-coord p))))))
      (letrec
	((circular-arc-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (circular-arc-points (sub1 i) (cons (arc-point i) points))))))
	(circular-arc-points n '())))))

;;-------------------------- Elliptical arcs --------------------------

(define elliptical-arc
  (lambda (x1 y1 x2 y2 height)
    (let* ((y-orig (min y1 y2))
	   (y (abs (- y1 y2)))
	   (b (max y height)))
      (if (zero? b)
	`(line (,x1 ,y1) (,x2 ,y2))
	(let* ((root (sqrt (- 1 (/ (^2 y) (^2 b)))))
	       (a (/ (- x2 x1) (+ 1 root)))
	       (x-orig (if (= y1 y-orig) (+ x1 a) (- x2 a)))
	       (theta-deg (* 180/pi (acos root)))
	       (start-deg (if (= y1 y-orig) theta-deg 0)))
	  `(arc (,x-orig ,y-orig) (,(* 2 a) ,(* 2 b))
	     ,start-deg ,(- 180 theta-deg)))))))

(define dotted-elliptical-arc
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (cond
      ((and (= y1 y2) (<= height 0))
       (dotted-line x1 y1 x2 y2 approx-interval-length))
      ((or %nice-graphics% (not *tcl/tk-version-8_3?*))
	`(polypoints
	   ,@(elliptical-arc-points x1 y1 x2 y2 height approx-interval-length)))
      (else `(let-sgl ((line-style dotted))
	       ,(elliptical-arc x1 y1 x2 y2 height))))))

(define dashed-elliptical-arc
  (lambda (x1 y1 x2 y2 height)
    (cond
      ((and (= y1 y2) (<= height 0))
       (dashed-line x1 y1 x2 y2 %dash-density% %dash-length%))
      ;; dashed lines don't seem to work on the Mac even with Tcl/Tk 8.4.4,
      ;; so just use dashed-polypoints for now
      ((and *tcl/tk-version-8_3?* (not (eq? *platform* 'macintosh)))
       `(let-sgl ((line-style dashed))
	  ,(elliptical-arc x1 y1 x2 y2 height)))
      (else `(dashed-polypoints
	       ,@(elliptical-arc-points x1 y1 x2 y2 height %dot-interval%))))))

(define elliptical-arc-points
  (lambda (x1 y1 x2 y2 height approx-interval-length)
    (let* ((y-orig (min y1 y2))
	   (y (abs (- y1 y2)))
	   (b (max y height))
	   (root (sqrt (- 1 (/ (^2 y) (^2 b)))))
	   (a (/ (- x2 x1) (+ 1 root)))
	   (x-orig (if (= y1 y-orig) (+ x1 a) (- x2 a)))
	   (origin (make-rectangular x-orig y-orig))
	   (theta (acos root))
	   (alpha (- pi theta))
	   (start (if (= y1 y-orig) theta 0))
	   (arc-length (* (sqrt (* a b)) alpha))
	   (n (max 3 (round (/ arc-length approx-interval-length))))
	   (angle-delta (/ alpha n))
	   (arc-point
	     (lambda (i)
	       (let* ((phi (+ start (* i angle-delta)))
		      (p (+ origin (make-rectangular (* a (cos phi)) (* b (sin phi))))))
		 `(,(x-coord p) ,(y-coord p))))))
      (letrec
	((elliptical-arc-points
	   (lambda (i points)
	     (if (< i 0)
	       points
	       (elliptical-arc-points (sub1 i) (cons (arc-point i) points))))))
	(elliptical-arc-points n '())))))

;;------------------------------- Arrows ---------------------------------
;;
;; orientation-angle is measured counterclockwise from the horizontal
;; (0 degrees = pointing to the right); angles are specified in degrees.
;; (x0 y0) is the arrowhead tip coordinate.

(define arrowhead
  (lambda (x0 y0 orientation-angle arrowhead-length arrowhead-angle-size)
    (let* ((theta (* pi/180 orientation-angle))
	   (alpha (* pi/180 arrowhead-angle-size))
	   (half-width (* arrowhead-length (tan (/ alpha 2))))
	   (coord-transform
	    (lambda (x y)
	      (let ((x-prime (- (* x (cos theta)) (* y (sin theta))))
		    (y-prime (+ (* y (cos theta)) (* x (sin theta)))))
		`(,(+ x0 x-prime) ,(+ y0 y-prime))))))
      `(line (,x0 ,y0) ,(coord-transform (- arrowhead-length) (- half-width))
	     (,x0 ,y0) ,(coord-transform (- arrowhead-length) 0)
	     (,x0 ,y0) ,(coord-transform (- arrowhead-length) half-width)))))

(define centered-double-arrow
  (lambda (x y orientation-angle arrow-length arrow-width
		  arrowhead-length arrowhead-angle-size)
    (let* ((theta (* pi/180 orientation-angle))
	   (alpha (* pi/180 arrowhead-angle-size))
	   (half-arrowhead-width (* arrowhead-length (tan (/ alpha 2))))
	   (half-arrow-width (/ arrow-width 2))
	   (overhang (/ half-arrow-width (tan (/ alpha 2))))
	   (left (- (/ arrow-length 2)))
	   (right (/ arrow-length 2))
	   (right-line (- right overhang))
	   (coord-transform
	    (lambda (p)
	      (let ((x-prime (- (* (1st p) (cos theta)) (* (2nd p) (sin theta))))
		    (y-prime (+ (* (2nd p) (cos theta)) (* (1st p) (sin theta)))))
		`(,(+ x x-prime) ,(+ y y-prime)))))
	   (points
	    `((,left ,half-arrow-width) (,right-line ,half-arrow-width)
	      (,left ,(- half-arrow-width)) (,right-line ,(- half-arrow-width))
	      (,(- right arrowhead-length) ,half-arrowhead-width) (,right 0)
	      (,(- right arrowhead-length) ,(- half-arrowhead-width)) (,right 0))))
      `(line ,@(map coord-transform points)))))

(define centered-double-headed-double-arrow
  (lambda (x y orientation-angle arrow-length arrow-width
	    arrowhead-length arrowhead-angle-size)
    (let ((x-delta (* 1/4 arrow-length (cos (* pi/180 orientation-angle))))
	  (y-delta (* 1/4 arrow-length (sin (* pi/180 orientation-angle)))))
      `(let-sgl ()
	 ,(centered-double-arrow
	    (+ x x-delta) (+ y y-delta) orientation-angle (* 1/2 arrow-length)
	    arrow-width arrowhead-length arrowhead-angle-size)
	 ,(centered-double-arrow
	    (- x x-delta) (- y y-delta) (+ 180 orientation-angle) (* 1/2 arrow-length)
	    arrow-width arrowhead-length arrowhead-angle-size)))))

;;--------------------------------- Grids ------------------------------------
;; not used in Metacat

;; style = solid | dashed | dotted

(define grid
  (lambda (g style delta)
    (let* ((xmax (tell g 'get-x-max))
	   (ymax (tell g 'get-y-max))
	   (dot-interval (* 5 (tell g 'get-width-per-pixel)))
	   (line (lambda (x0 y0 x1 y1)
		   (case style
		     (solid `(line (,x0 ,y0) (,x1 ,y1)))
		     (dashed `(line (,x0 ,y0) (,x1 ,y1)))
		     (dotted (dotted-line x0 y0 x1 y1 dot-interval))))))
      (let grid ((x 0) (y 0) (lines '()))
	(cond
	  ((and (> x xmax) (> y ymax))
	   (if (eq? style 'dashed)
	     `(let-sgl ((line-style dashed)) ,@lines)
	     `(let-sgl () ,@lines)))
	  ((> x xmax) (grid x (+ y delta) (cons (line 0 y xmax y) lines)))
	  ((> y ymax) (grid (+ x delta) y (cons (line x 0 x ymax) lines)))
	  (else (grid (+ x delta) (+ y delta)
		  (cons (line 0 y xmax y) (cons (line x 0 x ymax) lines)))))))))

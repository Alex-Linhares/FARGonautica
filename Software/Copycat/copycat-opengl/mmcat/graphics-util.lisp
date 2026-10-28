(defun draw-jagged-enclosure (x1 y1 x2 y2 jag-length &aux pair old-x old-y)
  (setq pair (draw-horizontal-jagged-line x1 y1 x2 y1 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (draw-vertical-jagged-line old-x old-y x2 y2 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (draw-horizontal-jagged-line old-x old-y x1 y2 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (draw-vertical-jagged-line old-x old-y x1 y1 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (draw-line old-x old-y x1 y1))

;---------------------------------------------

(defun xor-jagged-enclosure (x1 y1 x2 y2 jag-length &aux pair old-x old-y)
  (setq pair (xor-horizontal-jagged-line x1 y1 x2 y1 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (xor-vertical-jagged-line old-x old-y x2 y2 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (xor-horizontal-jagged-line old-x old-y x1 y2 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (xor-vertical-jagged-line old-x old-y x1 y1 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (xor-line old-x old-y x1 y1))

;---------------------------------------------

(defun draw-horizontal-jagged-line (x1 y1 x2 y2 jag-length)
  (if* (< x1 x2) 
   then (draw-right-horizontal-jagged-line x1 y1 x2 y2 jag-length)
   else (draw-left-horizontal-jagged-line x1 y1 x2 y2 jag-length)))

;---------------------------------------------

(defun xor-horizontal-jagged-line (x1 y1 x2 y2 jag-length)
  (if* (< x1 x2) 
   then (xor-right-horizontal-jagged-line x1 y1 x2 y2 jag-length)
   else (xor-left-horizontal-jagged-line x1 y1 x2 y2 jag-length)))

;---------------------------------------------

(defun draw-right-horizontal-jagged-line (x1 y1 x2 y2 jag-length
				           &aux old-x old-y new-x new-y quit)  
  (setq old-x x1 old-y y1)
  (loop until quit do
        (setq new-x (+ old-x jag-length) new-y (+ old-y jag-length))
	(draw-line old-x old-y new-x new-y)
        (if* (>= new-x x2) 
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (draw-line new-x new-y (+ new-x jag-length) old-y)
              (setq old-x (+ new-x jag-length))))
  (cons old-x old-y))
	    
;---------------------------------------------

(defun xor-right-horizontal-jagged-line (x1 y1 x2 y2 jag-length
				           &aux old-x old-y new-x new-y quit)  
  (setq old-x x1 old-y y1)
  (loop until quit do
        (setq new-x (+ old-x jag-length) new-y (+ old-y jag-length))
	(xor-line old-x old-y new-x new-y)
        (if* (>= new-x x2) 
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (xor-line new-x new-y (+ new-x jag-length) old-y)
              (setq old-x (+ new-x jag-length))))
  (cons old-x old-y))
	    
;---------------------------------------------

(defun draw-left-horizontal-jagged-line (x1 y1 x2 y2 jag-length
				           &aux old-x old-y new-x new-y quit)  
  (setq old-x x1 old-y y1)
  (loop until quit do
        (setq new-x (- old-x jag-length) new-y (+ old-y jag-length))
	(draw-line old-x old-y new-x new-y)
        (if* (<= new-x x2) 
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (draw-line new-x new-y (- new-x jag-length) old-y)
              (setq old-x (- new-x jag-length))))
  (cons old-x old-y))
	    
;---------------------------------------------

(defun xor-left-horizontal-jagged-line (x1 y1 x2 y2 jag-length
				           &aux old-x old-y new-x new-y quit)  
  (setq old-x x1 old-y y1)
  (loop until quit do
        (setq new-x (- old-x jag-length) new-y (+ old-y jag-length))
	(xor-line old-x old-y new-x new-y)
        (if* (<= new-x x2) 
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (xor-line new-x new-y (- new-x jag-length) old-y)
              (setq old-x (- new-x jag-length))))
  (cons old-x old-y))
	    
;---------------------------------------------

(defun draw-vertical-jagged-line (x1 y1 x2 y2 jag-length 
				  &optional (line-length 0))
; If line-length is 0, then the line-length is the distance between the two 
; points.  Otherwise it is as given.
  (if* (< y1 y2) 
   then (draw-down-vertical-jagged-line x1 y1 x2 y2 jag-length line-length)
   else (draw-up-vertical-jagged-line x1 y1 x2 y2 jag-length line-length)))

;---------------------------------------------

(defun xor-vertical-jagged-line (x1 y1 x2 y2 jag-length 
				 &optional (line-length 0))
; If line-length is 0, then the line-length is the distance between the two 
; points.  Otherwise it is as given.
  (if* (< y1 y2) 
   then (xor-down-vertical-jagged-line x1 y1 x2 y2 jag-length line-length)
   else (xor-up-vertical-jagged-line x1 y1 x2 y2 jag-length line-length)))

;---------------------------------------------

(defun draw-down-vertical-jagged-line (x1 y1 x2 y2 jag-length 
				       &optional (line-length 0)
				       &aux old-x old-y new-x new-y line-y2 
				            quit)
; In this function, y1 < y2.    
  (if* (> line-length 0)
   then (setq line-y2 (+ y1 line-length))
   else (setq line-y2 y2))
  (setq old-x x1 old-y y1)
  (loop until quit do
	(setq new-x (- old-x jag-length) new-y (+ old-y jag-length))
        (if* (> new-y line-y2)
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (draw-line old-x old-y new-x new-y)
	      (draw-line new-x new-y old-x (+ new-y jag-length))
              (setq old-y (+ new-y jag-length))))
  (cons old-x old-y))
	    
;---------------------------------------------

(defun xor-down-vertical-jagged-line (x1 y1 x2 y2 jag-length 
			              &optional (line-length 0)
				      &aux old-x old-y new-x new-y line-y2 
				           quit)
; In this function, y1 < y2.    
  (if* (> line-length 0)
   then (setq line-y2 (+ y1 line-length))
   else (setq line-y2 y2))
  (setq old-x x1 old-y y1)
  (loop until quit do
	(setq new-x (- old-x jag-length) new-y (+ old-y jag-length))
	(if* (> new-y line-y2) 
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (xor-line old-x old-y new-x new-y)
	      (xor-line new-x new-y old-x (+ new-y jag-length))
              (setq old-y (+ new-y jag-length))))
  (cons old-x old-y))
	    

;---------------------------------------------

(defun draw-up-vertical-jagged-line (x1 y1 x2 y2 jag-length 
			             &optional (line-length 0)
				     &aux old-x old-y new-x new-y line-y2 quit)
; In this function, y2 < y1.
  (if* (> line-length 0)
   then (setq line-y2 (- y1 line-length))
   else (setq line-y2 y2))
  (setq old-x x1 old-y y1)
  (loop until quit do
	(setq new-x (- old-x jag-length) new-y (- old-y jag-length))
        (if* (< new-y line-y2)
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (draw-line old-x old-y new-x new-y)
              (draw-line new-x new-y old-x (- new-y jag-length))
              (setq old-y (- new-y jag-length))))
  (cons old-x old-y))

;---------------------------------------------

(defun xor-up-vertical-jagged-line (x1 y1 x2 y2 jag-length 
				    &optional (line-length 0)
				    &aux old-x old-y new-x new-y line-y2 quit)
; In this function, y2 < y1.
  (if* (> line-length 0)
   then (setq line-y2 (- y1 line-length))
   else (setq line-y2 y2))
  (setq old-x x1 old-y y1)
  (loop until quit do
	(setq new-x (- old-x jag-length) new-y (- old-y jag-length))
        (if* (< new-y line-y2)
	 then (setq old-x new-x old-y new-y)
	      (setq quit t)
	 else (xor-line old-x old-y new-x new-y)
	      (xor-line new-x new-y old-x (- new-y jag-length))
              (setq old-y (- new-y jag-length))))
  (cons old-x old-y))

;---------------------------------------------

(defun draw-horizontal-arrow (x1 y length direction &aux x2)
  (if* (eq direction 'right)
   then (setq x2 (+ x1 length))
        (draw-line x1 y x2 y)
        (draw-line (- x2 5) (- y 3) x2 y)
        (draw-line x2 y  (- x2 5) (+ y 3))
   else (setq x2 (- x1 length))
        (draw-line x1 y x2 y)
        (draw-line  (+ x2 5) (- y 3) x2 y)
        (draw-line x2 y  (+ x2 5) (+ y 3))))

;---------------------------------------------

(defun xor-horizontal-arrow (x1 y length direction &aux x2)
  (if* (eq direction 'right)
   then (setq x2 (+ x1 length))
        (xor-line x1 y x2 y)
        (xor-line (- x2 5) (- y 3) x2 y)
        (xor-line x2 y  (- x2 5) (+ y 3))
   else (setq x2 (- x1 length))
        (xor-line x1 y x2 y)
        (xor-line  (+ x2 5) (- y 3) x2 y)
        (xor-line x2 y  (+ x2 5) (+ y 3))))

;---------------------------------------------

(defun draw-arrow (x y direction)
  (if* (eq direction 'left) 
   then (draw-line (+ x 5) (- y 5) x y)
        (draw-line x y (+ x 5) (+ y 5))
   else (draw-line (- x 5) (- y 5) x y)
        (draw-line x y (- x 5) (+ y 5))))

;---------------------------------------------

(defun draw-big-arrow (x y direction)
  (if* (eq direction 'left) 
   then (draw-line (+ x 8) (- y 8) x y)
        (draw-line x y (+ x 8) (+ y 8))
   else (draw-line (- x 8) (- y 8) x y)
        (draw-line x y (- x 8) (+ y 8))))

;---------------------------------------------

(defun draw-bold-arrow (x y direction)
  (if* (eq direction 'left) 
   then (draw-line (+ x 5) (- y 5) x y)
        (draw-line (+ x 5) (- y 6) x (- y 1))
        (draw-line x y (+ x 5) (+ y 5))
        (draw-line x (+ y 1) (+ x 5) (+ y 6))
   else (draw-line (- x 5) (- y 5) x y)
        (draw-line (- x 5) (- y 6) x (- y 1))
        (draw-line x y (- x 5) (+ y 5))
        (draw-line x (+ y 1) (- x 5) (+ y 6))))

;---------------------------------------------

(defun draw-big-bold-arrow (x y direction)
  (if* (eq direction 'left) 
   then (draw-line (+ x 8) (- y 8) x y)
        (draw-line (+ x 8) (- y 9) x (- y 1))
        (draw-line x y (+ x 8) (+ y 8))
        (draw-line x (+ y 1) (+ x 8) (+ y 9))
   else (draw-line (- x 8) (- y 8) x y)
        (draw-line (- x 8) (- y 9) x (- y 1))
        (draw-line x y (- x 8) (+ y 8))
        (draw-line x (+ y 1) (- x 8) (+ y 9))))

;---------------------------------------------

(defun draw-jagged-semi-enclosure (x1 y1 x2 y2 jag-length 
				   &aux pair old-x old-y)
  (draw-horizontal-jagged-line x1 y1 x2 y1 jag-length)
  (setq pair (draw-vertical-jagged-line x1 y1 x1 y2 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (draw-horizontal-jagged-line old-x old-y x2 y2 jag-length)))

;---------------------------------------------

(defun xor-jagged-semi-enclosure (x1 y1 x2 y2 jag-length &aux pair old-x old-y)
  (xor-horizontal-jagged-line x1 y1 x2 y1 jag-length)
  (setq pair (xor-vertical-jagged-line x1 y1 x1 y2 jag-length))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (xor-horizontal-jagged-line old-x old-y x2 y2 jag-length)))

;---------------------------------------------

(defun draw-bracket (x1 x2 y &aux x-mid x-mid1 x-mid2)
  (setq x-mid (round (/ (+ x1 x2) 2)))
  (setq x-mid1 (round (/ (+ x1 x-mid) 2)))
  (setq x-mid2 (round (/ (+ x-mid x2) 2)))
  (draw-parabola x1 y x-mid1 (+ y 7) (- x-mid 5) (- y 2) %medium-intensity%)
  (draw-parabola (+ x-mid 5) (- y 2) x-mid2 (+ y 7) x2 y  
      %medium-intensity%)
  (draw-heavy-line (- x-mid 5) (- y 2) x-mid (+ y 4))
  (draw-heavy-line x-mid (+ y 4) (+ x-mid 5) (- y 2)))

;---------------------------------------------

(defun draw-heavy-line (x1 y1 x2 y2)
  (draw-line (- x1 1) y1 (- x2 1) y2)
  (draw-line x1 y1 x2 y2))

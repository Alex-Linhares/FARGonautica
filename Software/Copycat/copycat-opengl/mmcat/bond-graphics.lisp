(defflavor bond-graphics-obj (from-obj to-obj string (drawn? nil) 
                              left-obj right-obj x1 y1 x2 y2 x3 y3 parent))

(defmethod (bond-graphics-obj :proposed?) ()
; Returns t if the bond is proposed, rather than built.
  (send parent :proposed?))

;---------------------------------------------

(defmethod (bond-graphics-obj :intensity) ()
; Returns a value for the intensity of the parabola representing the bond.
  (if* (= (send parent :proposal-level) 1)
   then %light-intensity%
   else (if* (eq (send parent :bond-category) plato-sameness)
         then %medium-intensity%
         else %light-intensity%)))

;---------------------------------------------

(defun draw-bond-grope (from-obj to-obj 
			    &aux left-obj right-obj x1 y1 x2 y2 x-mid y-mid) 
; Draws groping feelers coming from the two objects.
  (if* (< (send from-obj :left-string-position) 
	  (send to-obj :left-string-position))
   then (setq left-obj from-obj)
        (setq right-obj to-obj)
   else (setq left-obj to-obj)
        (setq right-obj from-obj))

  (setq x1 (send (send left-obj :graphics-obj) :bond-right-x)
        y1 (send (send left-obj :graphics-obj) :bond-y)
        x2 (send (send right-obj :graphics-obj) :bond-left-x)
        y2 (send (send right-obj :graphics-obj) :bond-y))
  (setq x-mid (round (/ (+ x1 x2) 2)))
  (setq y-mid (- y1 20))
  (loop for i from 1 to 3 do
        (xor-jagged-line x1 y1 (- x-mid 1) y-mid %jag-length%)
        (xor-jagged-line x2 y2 (+ x-mid 1) y-mid %jag-length%)
        (cond ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (long-pause)))
        (xor-jagged-line x1 y1 (- x-mid 1) y-mid %jag-length%)
        (xor-jagged-line x2 y2 (+ x-mid 1) y-mid %jag-length%)
        (cond ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (long-pause)))))

;---------------------------------------------

(defmethod (workspace-string :equal-or-higher-proposed-bond-drawn?) 
           (proposed-bond)
; Returns t if there is already a proposed bond drawn whose proposal-level
; is equal to or higher than that of the given proposed bond.  Otherwise, 
; returns nil.
  (loop for r 
	in (remove proposed-bond 
	           (aref proposed-bond-array 
		         (send (send proposed-bond :from-obj) 
			       :string-number)
		         (send (send proposed-bond :to-obj) 
			       :string-number))) 
	when (and (send (send r :graphics-obj) :drawn?) 
		  (>= (send r :proposal-level) 
		      (send proposed-bond :proposal-level)))
	return t
	finally (return nil)))
							   

;---------------------------------------------

(defmethod (workspace-string :higher-proposed-bond-drawn?) (proposed-bond)
; Returns t if there is already a proposed bond drawn whose proposal-level
; is higher than that of the given proposed-bond.  Otherwise, returns nil.
  (loop for r 
	in (remove proposed-bond 
	           (aref proposed-bond-array 
			 (send (send proposed-bond :from-obj) 
			       :string-number)
		         (send (send proposed-bond :to-obj) 
			       :string-number))) 
	when (and (send (send r :graphics-obj) :drawn?) 
		  (> (send r :proposal-level) 
		     (send proposed-bond :proposal-level)))
	return t
	finally (return nil)))
							  
;---------------------------------------------

(defmethod (bond-graphics-obj :draw-spline) ()
; Draws the parabola representing this bond.
  (if* (< (send parent :proposal-level) %built%)
   then (draw-dashed-parabola x1 y1 x2 y2 x3 y3 1 
	                      (send self :get-dash-length) 
			      (send self :get-space-length))
   else (draw-parabola x1 y1 x2 y2 x3 y3 (send self :intensity)))
   (cond ((eq (send parent :direction-category) plato-left)
          (draw-bond-arrow x2 y2 'left (send parent :proposal-level)))
         ((eq (send parent :direction-category) plato-right) 
          (draw-bond-arrow 
	      (+ x2 5) y2 'right (send parent :proposal-level))))
  (send self :set-drawn? t))

;---------------------------------------------

(defmethod (bond-graphics-obj :draw) ()
; If the bond is not in a group, draw it.
  (if* (not (in-group? from-obj to-obj)) then (send self :draw-spline)))

;---------------------------------------------

(defmethod (bond-graphics-obj :draw-proposed) ()
; First see if this bond should be drawn.  Only draw proposed bonds.
; Don't draw anything if there is already a bond drawn or if a
; proposed bond of equal or higher proposal level is already drawn.
  (if* (and (send self :proposed?) 
	    (not (or (send string :bond-present? parent)
	             (send string :equal-or-higher-proposed-bond-drawn? 
		                  parent))))
   then  ;If anything else is drawn then erase it.
        (loop for p 
	      in (aref (send string :proposed-bond-array)
   	  	       (send from-obj :string-number)
		       (send to-obj :string-number))
              when (send p :drawn?) do 
	      (send p :erase-spline) 
	      (return))
        (send self :draw-spline)))

;---------------------------------------------

(defmethod (bond-graphics-obj :flash) (&aux num-of-flashes)
  (if* (eq %graphics-rate% 'fast) 
   then (setq num-of-flashes 6)
   else (setq num-of-flashes 4))
  (loop for i from 1 to num-of-flashes do 
        (send self :erase-spline)
        (cond ((eq %graphics-rate% 'fast) (quick-pause))
	      ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (medium-pause)))
	(send self :draw-spline)
        (cond ((eq %graphics-rate% 'fast) (quick-pause))
	      ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (medium-pause)))))

;---------------------------------------------

(defmethod (bond-graphics-obj :flash-proposed) ()
; Don't flash if there is already a bond drawn or if a proposed bond 
; of higher proposal level is already drawn.  
  (if* (not (or (send string :bond-present? parent)
	       (send string :higher-proposed-bond-drawn? parent)))
   then (send self :flash)))

;---------------------------------------------

(defmethod (bond :init-graphics) (&aux left-x left-y right-x right-y  
				       x1 y1 x2 y2 x3 y3 parabola-height
				       new-bond-graphics-obj)
; Sets up the graphics object for this bond.
  (if* (eq bond-facet plato-letter-category)
   then (setq left-x (send (send left-obj :graphics-obj) :bond-right-x)
              left-y (send (send left-obj :graphics-obj) :bond-y) 
              right-x (send (send right-obj :graphics-obj) :bond-left-x)
              right-y (send (send right-obj :graphics-obj) :bond-y))
   else (setq left-x (send (send left-obj :graphics-obj) 
			   :length-bond-right-x)
              left-y (send (send left-obj :graphics-obj) :length-bond-y) 
              right-x (send (send right-obj :graphics-obj) 
			    :length-bond-left-x)
              right-y (send (send right-obj :graphics-obj) 
			    :length-bond-y)))

  (if* (eq direction-category plato-left)
   then (setq parabola-height 15)
   else (setq parabola-height 30))
  (setq x1 left-x)
  (setq x2 (round (/ (+ left-x right-x) 2)))
  (setq x3 right-x)
  (setq y1 left-y)
  (setq y2 (- left-y parabola-height))
  (setq y3 right-y)

  (setq new-bond-graphics-obj
	  (make-instance 'bond-graphics-obj
                 :from-obj from-obj :to-obj to-obj
		 :string string
                 :left-obj left-obj :right-obj right-obj
		 :parent self
  		 :x1 x1 :y1 y1 :x2 x2 :y2 y2 :x3 x3
		 :y3 y3))

  (send self :set-graphics-obj new-bond-graphics-obj))


;---------------------------------------------

(defmethod (bond-graphics-obj :get-dash-length) ()
  (if* (= (send parent :proposal-level) 1) 
   then %short-bond-dash-length%
   else %long-bond-dash-length%))
       
;---------------------------------------------

(defmethod (bond-graphics-obj :get-space-length) ()
  (if* (= (send parent :proposal-level) 1) 
   then %long-bond-space-length%
   else %short-bond-space-length%))
        
;---------------------------------------------

(defun draw-bond-arrow (x y direction proposal-level)
  (if* (< proposal-level %built%)
   then (draw-arrow x y direction)
   else (draw-bold-arrow x y direction)))

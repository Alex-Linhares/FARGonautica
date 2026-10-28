;---------------------------------------------
; REPLACEMENT GRAPHICS: This file contains graphics functions for replacements.
;---------------------------------------------

(in-package 'user)

(defflavor replacement-graphics-obj
  (obj1 obj2)
  (bond-graphics-obj)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defmethod (replacement-graphics-obj :intensity) ()
; For now replacements are always drawn at light intensity. 
  %light-intensity%)

;---------------------------------------------

(defmethod (replacement-graphics-obj :draw) ()
  (draw-parabola x1 y1 x2 y2 x3 y3 (send self :intensity)))

;---------------------------------------------

(defmethod (replacement-graphics-obj :erase) ()
  (erase-parabola x1 y1 x2 y2 x3 y3 (send self :intensity)))

;---------------------------------------------

(defmethod (replacement :init-graphics) (&aux left-x left-y right-x right-y 
			                   x1 y1 x2 y2 x3 y3 parabola-height 
					   new-replacement-graphics-obj)
  (setq left-x (send (send obj1 :graphics-obj) :replacement-x)
        left-y (send (send obj1 :graphics-obj) :replacement-y)
        right-x (send (send obj2 :graphics-obj) :replacement-x)
        right-y (send (send obj2 :graphics-obj) :replacement-y))

  (setq parabola-height 40)

  (setq x1 left-x)
  (setq x2 (round (/ (+ left-x right-x) 2)))
  (setq x3 right-x)
  (setq y1 left-y)
  (setq y2 (- left-y parabola-height))
  (setq y3 right-y)

  (setq new-replacement-graphics-obj
	  (make-instance 'replacement-graphics-obj
                 :obj1 obj1
                 :obj2 obj2
		 :parent self
  		 :x1 x1
		 :y1 y1
  		 :x2 x2
		 :y2 y2
  		 :x3 x3
		 :y3 y3))

  (send self :set-graphics-obj new-replacement-graphics-obj))

;---------------------------------------------

(defmethod (replacement :draw) ()
  (send graphics-obj :draw))

;---------------------------------------------

(defmethod (replacement :erase) ()
  (send graphics-obj :erase))

;---------------------------------------------


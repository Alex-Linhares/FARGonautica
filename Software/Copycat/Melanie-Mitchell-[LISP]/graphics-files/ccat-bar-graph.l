;---------------------------------------------
; CCAT-BAR-GRAPH:  Utilities for drawing the coderack bar-graph.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defflavor bar-graph
    (x1 y1 x2 y2 max-value bar-space bar-width bar-vector name-y)
    ; (X1,Y1) is the upper-left corner; (X2, Y2) is the lower-right corner. 
    ; MAX-VALUE is the maximum value for a bar.
    ; BAR-SPACE is the horizontal space in which each bar is drawn.
    ; BAR-WIDTH is the width of the bar in BAR-SPACE.  
    ; BAR-VECTOR is the vector containing all the bars.
    ; NAME-Y is the y-coordinate of the name of the bar.
    ()
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defflavor bar
    (bar-space-x1 x1 y1 x2 y2 old-y1 name-y name value old-value parent)    
    ; (X1,Y1) is the upper-left corner; (X2, Y2) is the lower-right corner. 
    ()
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defun make-bar-graph (x1 y1 x2 y2 max-value name-list
		       &aux new-bar-graph new-x bar-space bar-width bar-count)
; Returns a bar-graph object with the given parameters.
  (setq bar-space (round (/ (- x2 x1) (length name-list))))
  (setq bar-width (round (/ bar-space 2)))
  ; The second term is to center the bar in the bar space.
  (setq new-x (round (+ (- x1 bar-space) (/ (- bar-space bar-width) 2))))
  (setq new-bar-graph
	(make-instance 'bar-graph 
	    :x1 x1 :y1 y1 :x2 x2 :y2 y2 :name-y (+ y2 15)
	    :max-value max-value :bar-space bar-space :bar-width bar-width
	    :bar-vector (make-vector (length name-list))))

  (setq bar-count -1)
  (loop for name in name-list do
        (setq new-x (+ new-x bar-space))
        (vset (send new-bar-graph :bar-vector) 
	      (setq bar-count (1+ bar-count))
	      (make-instance 'bar 
		  :bar-space-x1 
		  (round (- new-x (/ (- bar-space bar-width) 2)))
		  :x1 new-x :y1 y2 :x2 (+ new-x bar-width) :y2 y2 
		  :old-y1 y2 :name-y (+ y2 10)
 	          :name name :value 0 :old-value 0
		  :parent new-bar-graph)))
  new-bar-graph)

(defmethod (bar-graph :display) (&aux bar)
; Displays the whole bar-graph.
  (draw-unfilled-rectangle x1 y1 x2 y2)
  ; Draw leftmost line for separating names.
  (draw-line x1 %codelet-name-top-y% x1 %codelet-name-bottom-y%)
  (loop for i from 0 to (1- (vsize bar-vector)) do
	(setq bar (vref bar-vector i))
        (send bar :display-name bar-space)
	(send bar :display)))

(defmethod (bar-graph :display-bar) (bar-index)
; Displays a single bar.
  (send (vref bar-vector bar-index) :display))

(defmethod (bar :display-name) (name-width &aux new-y endline-x)
; Displays the name.  NAME is a list that contains one or more lines.
  (setq new-y name-y)
  (setq endline-x (+ bar-space-x1 name-width))  ; x-coord. of separator line.
  (loop for line in name do
       (draw-centered-text bar-space-x1 new-y line name-width)
       (setq new-y (+ new-y %codelet-name-font-height%)))
  (draw-line endline-x %codelet-name-top-y% endline-x %codelet-name-bottom-y%))

(defmethod (bar :display) ()
  (erase-centered-text x1 (- old-y1 5) (fix-to-string old-value) 
                       (send parent :bar-width))
  (if* (< old-y1 y1)  ; old bar was higher than new bar
   then (erase-solid-rectangle x1 old-y1 x2 y1))
  (draw-solid-rectangle x1 y1 x2 y2)
  (if* (> value 0)  
   then (draw-centered-text x1 (- y1 5) (fix-to-string value) 
	                    (send parent :bar-width)))
  (setq old-y1 y1))

(defmethod (bar-graph :recompute-bar) (bar-index new-value &aux bar)
; Recomputes the height of the bar and sets up the new coordinates.
  (setq bar (vref bar-vector bar-index))    
  (send bar :set-old-y1 (send bar :y1))
  (send bar :set-old-value (send bar :value))
  (send bar :set-value new-value)
  (send bar :set-y1 (round (- y2 (/ (* new-value (- y2 y1)) max-value)))))
      
(defmethod (bar-graph :get-bar) (bar-index)
; Returns the given bar.
  (vref bar-vector bar-index))
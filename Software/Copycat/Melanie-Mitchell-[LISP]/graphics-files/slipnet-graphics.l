;---------------------------------------------
; SLIPNET GRAPHICS:  This file contains graphics functions for the slipnet.
;---------------------------------------------

(in-package 'user)
			     
(defflavor graphics-node
    (x-region ; The coordinates for the rectangle representing the node.
     y-region

     x-box ; The coordinates for the solid box representing the node's 
     y-box ; activation.
                                
     x-act ; The coordinates for the displayed activation value (a number).
     y-act

     x-name ; The coordinates for the displayed name of the node.
     y-name

     old-x-box ; Used for growing and shrinking the activation box and updating
     old-y-box ; the node's activation display.
     (old-slipnode-boxsize 0) 
     (old-activation 0)

     parent ; The node represented by this graphics object.
     )                       
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defun init-slipnet-graphics ()
; Initializes the graphics for the slipnet.
  (cond ((eq %slipnet-display-level% 'low)
         (setq *nodes-to-display*
	       (list plato-leftmost plato-middle plato-rightmost 
		     plato-first plato-last plato-left plato-right 
                     plato-identity plato-opposite
		     plato-sameness plato-predecessor plato-successor
		     plato-predgrp plato-succgrp plato-samegrp
                     plato-letter plato-group
		     plato-letter-category plato-length
	             plato-string-position-category
		     plato-alphabetic-position-category
		     plato-direction-category plato-bond-category 
		     plato-group-category plato-object-category)))
	((eq %slipnet-display-level% 'medium)
	 (setq *nodes-to-display*
	       (append *slipnet-letters* *slipnet-numbers*
		       (list
			     plato-left plato-right  
			     plato-leftmost plato-rightmost plato-middle 
		             plato-predecessor plato-successor plato-sameness
	                     plato-predgrp plato-succgrp plato-samegrp 
	                     plato-first plato-last
	                     plato-letter plato-group
			     plato-identity plato-opposite 
			     plato-whole plato-single 
                             plato-letter-category plato-length
                             plato-alphabetic-position-category
                             plato-string-position-category
                             plato-direction-category 	                    
			     plato-object-category 
			     plato-bond-category
	                     plato-group-category))))
	(t (setq *nodes-to-display* *slipnet*)))

  ; Set up the regions in which the nodes will be displayed.
  (setup-slipnet-regions)
 
 ; Set up a global vector of the slipnode-boxsize for each possible activation
  (make-slipnode-boxsizes))
                             
;---------------------------------------------

(defun display-slipnet ()
  (outline-slipnet-regions)
  (update-slipnet-display))

;---------------------------------------------

(defun erase-slipnet ()
  (erase-solid-rectangle %slipnet-x% %slipnet-y% 
                         (+ %slipnet-x% %slipnet-width%) 
			 (+ %slipnet-y% %slipnet-height% 2)))

;---------------------------------------------

(defun setup-slipnet-regions (&aux num-of-nodes dummy-list 
			           num-of-regions-in-width 
				   num-of-regions-in-height 
				   current-node current-graphics-node)
; Sets up the display for the nodes to be displayed.

  (setq num-of-nodes (length *nodes-to-display*))
  (setq dummy-list *nodes-to-display*)

  (setq num-of-regions-in-width
	(case %slipnet-display-level%
	      (low 9)
	      (medium 13)
              (high 26)))

  (setq num-of-regions-in-height
	(if* (= (mod num-of-nodes num-of-regions-in-width) 0)
         then (truncate (/ num-of-nodes num-of-regions-in-width))
         else (1+ (truncate (/ num-of-nodes num-of-regions-in-width)))))

  (setq slipnode-region-height 
	(round (/ %slipnet-height% num-of-regions-in-height)))
  (setq slipnode-region-width 
	(round (/ %slipnet-width% num-of-regions-in-width)))

  (loop for i from 0 to (1- num-of-regions-in-height) do
        (loop for j from 0 to (1- num-of-regions-in-width) do
              (if* dummy-list
	       then (setq current-node (car dummy-list))
		    (setq current-graphics-node 
			  (make-instance 'graphics-node :parent current-node))
		    (send current-graphics-node :set-x-region 
			  (+ %slipnet-x% (* j slipnode-region-width)))
		    (send current-graphics-node :set-y-region 
			  (+ %slipnet-y% (* i slipnode-region-height)))
                    (send current-graphics-node :set-x-act 
			  (send current-graphics-node :x-region))
		    (send current-graphics-node :set-y-act 
			  (+ (send current-graphics-node :y-region)
			     %slipnet-font-height% 2))
  		    (send current-graphics-node :set-x-box 
		          (+ (send current-graphics-node :x-region) 10))
		    (send current-graphics-node :set-y-box 
			  (+ (send current-graphics-node :y-region) 
		             %slipnet-font-height%))
	            (send current-graphics-node :set-x-name 
			  (send current-graphics-node :x-region))
                    (send current-graphics-node :set-y-name 
			  (- (+ (send current-graphics-node :y-region) 
				slipnode-region-height) 3))
  		    (send current-node :set-graphics-obj current-graphics-node)
                    (setq dummy-list (cdr dummy-list)))
    	      until (null dummy-list))))
     

;---------------------------------------------

(defun make-slipnode-boxsizes ()
; Sets up a global vector containing the slipnode-boxsizes, indexed by 
; activation.
  (setq *slipnode-boxsizes* (make-vector (1+ %max-activation%)))
  (loop for activation from 0 to %max-activation% do
        (vset *slipnode-boxsizes* activation 
              (max 1 (round (* 0.38 slipnode-region-height 
			       (/ activation %max-activation%)))))))

;---------------------------------------------

(defun outline-slipnet-regions (&aux graphics-node new-y-name)
; Outlines the regions for displaying the slipnet    
  (loop for node in *nodes-to-display* do
        (setq graphics-node (send node :graphics-obj))
	(setq new-y-name (send graphics-node :y-name))
        (loop for line in (reverse (send node :short-name)) do
              (if* (or (memq node *slipnet-letters*) 
		       (memq node *slipnet-numbers*))
               then (set-font %slipnet-letter-font%)
               else (set-font %slipnet-font%))
	      (draw-centered-text (send graphics-node :x-name) new-y-name
				  line slipnode-region-width)
	      (setq new-y-name (- new-y-name %slipnet-font-height%))) 
        (draw-unfilled-rectangle 
	    (send graphics-node :x-region) 
            (send graphics-node :y-region)
            (+ (send graphics-node :x-region) slipnode-region-width) 
            (+ (send graphics-node :y-region) slipnode-region-height)))
  (set-font %workspace-font%))


;---------------------------------------------

(defmethod (graphics-node :new-slipnode-boxsize) ()
; Returns the box size for the node (a function of its current activation).
; The function for box size of a node is 
;  region-width * (activation / %max-activation%) 
  (vref *slipnode-boxsizes* (send parent :activation)))


;---------------------------------------------

(defmethod (graphics-node :new-box-coordinates) ()
; Returns a dotted pair of the x and y coordinates of the upper-left-hand
; corner of the box that is to be drawn.
  (if* (eq %slipnet-display-level% 'low)
   then (cons (round (+ x-region (/ (- slipnode-region-width
				       (send self :new-slipnode-boxsize)) 2)))
              (round (- (+ y-region (/ (- slipnode-region-height
					  (send self :new-slipnode-boxsize)) 2)) 2)))
   else (cons (round (+ x-region (/ (- slipnode-region-width
				       (send self :new-slipnode-boxsize)) 2)))
              (round (+ y-region (/ (- slipnode-region-height
				       (send self :new-slipnode-boxsize)) 2))))))

;---------------------------------------------

(defmethod (graphics-node :draw-box) (&aux new-x-box new-y-box
					   new-slipnode-boxsize)
; Draws the box corresponding to the current slipnode
(block nil
  (cond ((and (= (send parent :activation) 0) (= old-activation 0)) (return))
	((= (send parent :activation) 0) 
	 (erasebox old-slipnode-boxsize old-x-box old-y-box))
        (t (setq new-x-box (car (send self :new-box-coordinates))
                 new-y-box (cdr (send self :new-box-coordinates))
	         new-slipnode-boxsize (send self :new-slipnode-boxsize))
           ; If old box size > new-slipnode-boxsize then shrink old box.
           (if* (> old-slipnode-boxsize new-slipnode-boxsize)
            then (if* (< (- old-slipnode-boxsize new-slipnode-boxsize) 10)
                  then (erasebox old-slipnode-boxsize old-x-box old-y-box) 
	               (drawbox new-slipnode-boxsize new-x-box new-y-box)
 	          else (shrink-box old-slipnode-boxsize new-slipnode-boxsize 
			           old-x-box old-y-box new-x-box new-y-box))
            else ; expand old box
	         (drawbox new-slipnode-boxsize new-x-box new-y-box))))
  

  ; Erase old-activation and display new-activation numbers.
  (erase-centered-text x-act y-act (fix-to-string old-activation)
                       slipnode-region-width)
  (if* (> (send parent :activation) 0)
   then (draw-centered-text x-act y-act 
	    (fix-to-string (send parent :activation)) slipnode-region-width))

 ; Save size of box and coordinates
  (send self :set-old-slipnode-boxsize (send self :new-slipnode-boxsize))
  (send self :set-old-activation (send parent :activation))
  (send self :set-old-x-box new-x-box)
  (send self :set-old-y-box new-y-box)))


;---------------------------------------------

(defun update-slipnet-display ()
; Displays boxes corresponding to the nodes in the *nodes-to-display*
  (set-font %slipnet-activation-font%)
  (loop for node in *nodes-to-display* do 
        (send (send node :graphics-obj) :draw-box))
  (set-font %workspace-font%))

;---------------------------------------------

(defun shrink-box (oldboxsize newboxsize oldx oldy newx newy 
		   &aux var1 var2 var3 var4)
; Shrinks box from oldboxsize to newboxsize.
  (setq var1 (+ oldx oldboxsize)
	var2 (+ oldy oldboxsize)
	var3 (+ newx newboxsize)
	var4 (+ newy newboxsize))
  (erase-solid-rectangle oldx oldy var1 newy)
  (erase-solid-rectangle var3 newy var1 var2)
  (erase-solid-rectangle oldx var4 var1 var2)
  (erase-solid-rectangle oldx oldy newx var4))

;---------------------------------------------

(defun drawbox (slipnode-boxsize x y)
; Draws a filled box with upper-left-hand corner (x y) and side 
; slipnode-boxsize.
  (draw-solid-rectangle x y (+ x slipnode-boxsize) (+ y slipnode-boxsize)))


;---------------------------------------------

(defun erasebox (slipnode-boxsize x y)
; Erases a box with upper-left-hand corner (x y) and side slipnode-boxsize.
  (erase-solid-rectangle x y (+ x slipnode-boxsize) (+ y slipnode-boxsize)))

;---------------------------------------------






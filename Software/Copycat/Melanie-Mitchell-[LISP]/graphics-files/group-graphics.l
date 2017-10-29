;---------------------------------------------
; GROUP GRAPHICS:  This file contains graphics functions for groups.
;---------------------------------------------

(in-package 'user)

(defflavor group-graphics-obj 
  (string  ; The string the group is in.
   left-obj right-obj  ; The left and right objects in the group.
   direction ; The group's direction.
   x1 y1 x2 y2 ; The coordinates of the graphics object.
   parent  ; The group represented by this graphics object.
   (drawn? nil) ; T if the group is currently drawn.
  ) 
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defmethod (group :init-graphics) 
           (&aux new-group-graphics-obj leftmost-letter rightmost-letter 
		 group-graphics-size-factor)
; Sets up the graphics object for this group.
  (setq new-group-graphics-obj
	(make-instance 'group-graphics-obj
	    :string string
  	    :direction direction-category
	    :left-obj left-obj
	    :right-obj right-obj
            :parent self))

  (setq leftmost-letter (send self :leftmost-letter))
  (setq rightmost-letter (send self :rightmost-letter))

  ; (x1, y1) is the upper-left corner; (x2, y2) is the 
  ; lower-rt corner.
  (setq group-graphics-size-factor 
	(if* (= (send self :letter-span) 1) 
	 then 2 else (send self :letter-span)))
  (send new-group-graphics-obj 
	:set-x1 (- (send leftmost-letter :x) 
		   (* 5 group-graphics-size-factor)))
  (send new-group-graphics-obj 
	:set-x2 (+ (send rightmost-letter :x) 
		   (* 5 group-graphics-size-factor) 10))
  (send new-group-graphics-obj 
        :set-y1 (- (send leftmost-letter :y) 
		   (* 6 group-graphics-size-factor) 10))
  (send new-group-graphics-obj 
        :set-y2 (+ (send rightmost-letter :y) 
		   (* 6 group-graphics-size-factor)))

  (if* (eq direction-category plato-right)
   then (send new-group-graphics-obj 
	      :set-y1 (+ (send new-group-graphics-obj :y1) 7))
        (send new-group-graphics-obj 
	      :set-y2 (- (send new-group-graphics-obj :y2) 7)))
   
  (send self :set-structure-graphics-obj new-group-graphics-obj)
  (send self :init-parameter-graphics))

;---------------------------------------------

(defmethod (group-graphics-obj :proposed?) ()
  (send parent :proposed?))

;---------------------------------------------

(defmethod (group-graphics-obj :intensity) ()
; Returns the thickness of the line with which the group should be drawn.
  (if* (= (send parent :proposal-level) 1)
   then %light-intensity%
   else (if* (eq (send parent :group-category) :samegrp)
         then %medium-intensity%
         else %light-intensity%)))


;---------------------------------------------

(defmethod (group-graphics-obj :get-dash-length) ()
  (if* (= (send parent :proposal-level) 1)
   then %short-group-dash-length%       
   else %long-group-dash-length%))

;---------------------------------------------

(defmethod (group-graphics-obj :get-space-length) ()
    %group-space-length%)

;---------------------------------------------

(defmethod (workspace-string :equal-or-higher-proposed-group-drawn?)
           (proposed-group)
; Returns t if there is already a proposed group drawn whose proposal-level is 
; equal to or higher than that of the given proposed group.  Otherwise, returns
; nil.
  (loop for g 
	in (remove proposed-group 
	           (aref proposed-group-array
		         (send (send proposed-group :left-obj) 
			       :string-number)
			 (send (send proposed-group :right-obj)
			       :string-number)))
	when (and (send (send g :structure-graphics-obj) :drawn?) 
		  (>= (send g :proposal-level) 
		      (send proposed-group :proposal-level)))
	return t
	finally (return nil)))
							   

(defmethod (workspace-string :higher-proposed-group-drawn?) (proposed-group)
; Returns t if there is already a proposed group drawn whose proposal-level is 
; higher than that of the given proposed group.  Otherwise, returns nil.
  (loop for g 
	in (remove proposed-group
	           (aref proposed-group-array
		         (send (send proposed-group :left-obj) 
			       :string-number)
			 (send (send proposed-group :right-obj)
			       :string-number)))
	when (and (send (send g :structure-graphics-obj) :drawn?) 
		  (> (send g :proposal-level) 
		     (send proposed-group :proposal-level)))
	return t
	finally (return nil)))
							   
;---------------------------------------------

(defun draw-group-grope (left-obj right-obj 
			 &aux x1 y1 x2 y2 x1-mid y1-mid x2-mid y2-mid)
; Draws groping feelers coming from the left and right objects.
  (setq x1 (- (send left-obj :x) 5))
  (setq y1 (- (send left-obj :y) 25))
  (setq x2 (+ (send right-obj :x) 25))
  (setq y2 (+ (send right-obj :y) 3))
  (setq x1-mid (round (+ x1 (/ (- x2 x1) 3))))
  (setq y1-mid (- y2 10))
  (setq x2-mid (round (- x2 (/ (- x2 x1) 3))))
  (setq y2-mid (+ y1 5))
  (loop for i from 1 to 3 do
	(xor-jagged-semi-enclosure x1 y1 x1-mid y1-mid  %jag-length%)
	(xor-jagged-semi-enclosure x2 y2 x2-mid y2-mid  %jag-length%)
        (cond ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (long-pause)))
	(xor-jagged-semi-enclosure x1 y1 x1-mid y1-mid %jag-length%)
	(xor-jagged-semi-enclosure x2 y2 x2-mid y2-mid %jag-length%)
        (cond ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (long-pause)))))

;---------------------------------------------

(defmethod (group-graphics-obj :draw-rectangle) (&aux x-center)
; Draws the rectangle that represents the group.
  (if* (< (send parent :proposal-level) %built%)
   then (draw-dashed-rectangle x1 y1 x2 y2 
	                       (send self :get-dash-length)
	                       (send self :get-space-length))
   else (draw-unfilled-rectangle x1 y1 x2 y2))
  (setq x-center (round (/ (+ x1 x2) 2)))
  (cond ((eq (send parent :direction-category) plato-left)
         (draw-group-arrow (- x-center 10) y1 'left 
	                    (send parent :proposal-level)))
        ((eq (send parent :direction-category) plato-right)
	 (draw-group-arrow (+ x-center 10) y1 'right 
		           (send parent :proposal-level))))
  (send self :set-drawn? t))

;---------------------------------------------

(defmethod (group-graphics-obj :erase-rectangle) (&aux x-center)
; Draws the rectangle that represents the group.
  (if* (< (send parent :proposal-level) %built%)
   then (erase-dashed-rectangle x1 y1 x2 y2 
	                       (send self :get-dash-length)
	                       (send self :get-space-length))
   else (erase-unfilled-rectangle x1 y1 x2 y2))
  (setq x-center (round (/ (+ x1 x2) 2)))
  (cond ((eq (send parent :direction-category) plato-left)
         (erase-group-arrow (- x-center 10) y1 'left 
	                    (send parent :proposal-level)))
        ((eq (send parent :direction-category) plato-right)
	 (erase-group-arrow (+ x-center 10) y1 'right 
		           (send parent :proposal-level))))
  (send self :set-drawn? nil))
	 

;---------------------------------------------

(defmethod (group-graphics-obj :draw) ()
  (send self :draw-rectangle)
  (send parent :draw-parameter))

;---------------------------------------------

(defmethod (group-graphics-obj :erase) 
           (&aux other-proposed-groups highest-level-proposed-group)
  (send self :erase-rectangle)
  (send parent :erase-parameter)
  (setq other-proposed-groups 
	(remove parent 
		(aref (send string :proposed-group-array) 
                      (send left-obj :string-number)
                      (send right-obj :string-number))))
  ; If there is still at least one proposed group pending,
  ; draw the one with the highest proposal level.
  (if* other-proposed-groups  
   then (setq highest-level-proposed-group
	      (nth (list-max-position 
		       (send-method-to-list 
			   other-proposed-groups 
			   :proposal-level))
 		   other-proposed-groups))
        (send (send highest-level-proposed-group :structure-graphics-obj) 
	      :draw-rectangle)))
	  
;---------------------------------------------

(defmethod (group-graphics-obj :draw-proposed) ()
; First see if this group should be drawn.  Only draw proposed groups.
; Don't draw anything if there is already a group drawn or if a proposed group 
; of equal or higher proposal level is already drawn.
  (if* (and (send self :proposed?) 
	    (not (or (send string :group-present? parent)
	             (send string :equal-or-higher-proposed-group-drawn? 
			   parent))))
   then  ; If anything else is drawn then erase it.
        (loop for g 
	      in (aref (send string :proposed-group-array)
		       (send left-obj :string-number)
		       (send right-obj :string-number))
              when (send g :drawn?) do 
	      (send g :erase-rectangle) 
	      (return))
        (send self :draw-rectangle)))

;---------------------------------------------

(defmethod (group-graphics-obj :erase-proposed) 
           (&aux other-proposed-groups highest-level-proposed-group)
  ; Only erase if proposed and already drawn.
  (if* (and (send self :proposed?) drawn? 
	    (not (send string :group-present? parent)))
   then (send self :erase-rectangle)

        ; Now draw in the next-highest-level proposed group.
        (setq other-proposed-groups
	      (remove parent 
		      (aref (send string 
				  :proposed-group-array) 
                            (send left-obj 
				  :string-number)
                            (send right-obj 
				  :string-number))))
        ; If there is still at least one proposed group 
	; pending, draw the one with the highest proposal 
	; level.
        (if* other-proposed-groups
         then (setq highest-level-proposed-group
	            (nth (list-max-position 
			     (send-method-to-list 
				 other-proposed-groups
				 :proposal-level))
   		         other-proposed-groups))
              (send (send highest-level-proposed-group 
			  :structure-graphics-obj) :draw-rectangle))))
 

;---------------------------------------------

(defmethod (group-graphics-obj :flash) (&aux num-of-flashes)
  (setq num-of-flashes 4)
  (loop for i from 1 to num-of-flashes do 
        (send self :erase-rectangle)
        (cond ((eq %graphics-rate% 'fast) (quick-pause))
	      ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (long-pause)))
        (send self :draw-rectangle)
        (cond ((eq %graphics-rate% 'fast) (quick-pause))
	      ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (medium-pause)))))
	


;---------------------------------------------

(defmethod (group-graphics-obj :flash-proposed) ()
; Don't flash if there is already a group drawn or if a 
; proposed group of higher proposal level is already drawn.  
  (if* (not (or (send string :group-present? parent)
                (send string 
		      :higher-proposed-group-drawn? parent)))
   then (send self :flash)))

;---------------------------------------------

(defun draw-group-arrow (x y direction proposal-level)
  (if* (< proposal-level %built%)
   then (draw-big-arrow x y direction)
   else (draw-big-bold-arrow x y direction)))

;---------------------------------------------

(defun erase-group-arrow (x y direction proposal-level)
  (if* (< proposal-level %built%)
   then (erase-big-arrow x y direction)
   else (erase-big-bold-arrow x y direction)))

;---------------------------------------------

(defmethod (group :x1) ()
  (send structure-graphics-obj :x1))

;---------------------------------------------

(defmethod (group :y1) ()
  (send structure-graphics-obj :y1))

;---------------------------------------------

(defmethod (group :x2) ()
  (send structure-graphics-obj :x2))

;---------------------------------------------

(defmethod (group :y2) ()
  (send structure-graphics-obj :y2))

;---------------------------------------------

(defmethod (group :drawn?) ()
  (send structure-graphics-obj :drawn?))

;---------------------------------------------

(defmethod (group :flash) ()
  (send structure-graphics-obj :flash))

;---------------------------------------------

(defmethod (group :flash-proposed) ()
  (send structure-graphics-obj :flash-proposed))

;---------------------------------------------

(defmethod (group :draw) ()
  (send structure-graphics-obj :draw))

;---------------------------------------------

(defmethod (group :erase) ()
  (send structure-graphics-obj :erase))

;---------------------------------------------

(defmethod (group :draw-proposed) ()
  (send structure-graphics-obj :draw-proposed))

;---------------------------------------------

(defmethod (group :erase-proposed) ()
  (send structure-graphics-obj :erase-proposed))

;---------------------------------------------

(defmethod (group :draw-rectangle) ()
  (send structure-graphics-obj :draw-rectangle))

;---------------------------------------------

(defmethod (group :erase-rectangle) ()
  (send structure-graphics-obj :erase-rectangle))

;---------------------------------------------


;---------------------------------------------
; GROUP-PARAMETER-GRAPHICS
;---------------------------------------------

(in-package 'user)

(defflavor group-parameter-graphics-obj 
    (graphics-name graphics-length string-spanning-group-correspondence-x 
     string-spanning-group-correspondence-y length-x length-y 
     length-bond-right-x length-bond-left-x length-bond-y)
    (letter-graphics-obj)    
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defmethod (group :init-parameter-graphics)
           (&aux x1 y1 x2 y2 center-x center-y 
		 new-group-parameter-graphics-obj letter-category)

  (setq x1 (send self :x1))  
  (setq y1 (send self :y1))  
  (setq x2 (send self :x2))  
  (setq y2 (send self :y2))  

  (setq center-x 
	(- (round (/ (+ x1 x2) 2)) 8))
  (if* (send self :get-descriptor plato-letter-category)
   then (setq center-y (- y1 6)) 
   else (setq center-y y1))

  (setq new-group-parameter-graphics-obj 
	(make-instance 'group-parameter-graphics-obj
            :x center-x
	    :y center-y
	    :parent self
	    :bond-left-x (+ center-x %bond-left-x-offset%)
	    :bond-right-x (+ center-x %bond-right-x-offset%)
            :bond-y (- center-y %bond-y-offset%)))

  (send self :set-graphics-obj new-group-parameter-graphics-obj)

  (if* (setq letter-category 
	     (send self :get-descriptor plato-letter-category))
   then (send new-group-parameter-graphics-obj :set-graphics-name 
	                            (send letter-category :pname))
   else (send new-group-parameter-graphics-obj :set-graphics-name nil))

  (if* (send self :get-descriptor plato-length)
   then (send new-group-parameter-graphics-obj :init-length-graphics)
   else (send new-group-parameter-graphics-obj :set-graphics-length nil))

  ; Set up the correspondence coordinates.
  (if* (send self :graphics-name)
   then (send new-group-parameter-graphics-obj 
	      :set-correspondence-x (+ center-x 8))
   else (send new-group-parameter-graphics-obj :set-correspondence-x center-x))
  (send new-group-parameter-graphics-obj :set-correspondence-y  
       (if* (and (eq string *target-string*)
	         (send new-group-parameter-graphics-obj :graphics-name))
        then (- y1 %group-font-height%)
        else (if* (eq string *initial-string*) then y2 else y1)))

  (if* (send self :string-spanning-group?)  ; Group spans whole string.
   then (send new-group-parameter-graphics-obj 
	      :set-string-spanning-group-correspondence-x x2)
        (send new-group-parameter-graphics-obj 
	      :set-string-spanning-group-correspondence-y (send string :y)))

  (send new-group-parameter-graphics-obj 
	:set-description-x (+ (send new-group-parameter-graphics-obj :x) 10))
  (send new-group-parameter-graphics-obj 
	:set-description-y (send new-group-parameter-graphics-obj :y)))

;----------------------------------------------

(defmethod (group-parameter-graphics-obj :init-length-graphics) ()
; This is used for displaying the lengths of groups.
  (send self :set-graphics-length 
	(send (send parent :get-descriptor plato-length) :pname))
  (send self :set-length-x (+ x (text-length graphics-name) 2))
  (send self :set-length-y (if* graphics-name then y else (- y 5)))
  (send self :set-length-bond-left-x 
	(+ length-x %bond-left-x-offset%))
  (send self :set-length-bond-right-x 
	(+ length-x %bond-right-x-offset%))
  (send self :set-length-bond-y (- length-y %bond-y-offset%)))

;---------------------------------------------

(defmethod (group-parameter-graphics-obj :draw-length) ()
  (if* (send plato-length :active?)
   then (set-font %relevant-length-font%)
   else (set-font %irrelevant-length-font%))
  (draw-text length-x length-y graphics-length)
  (set-font %workspace-font%))

;----------------------------------------------

(defmethod (group-parameter-graphics-obj :erase-length) ()
  (if* (send plato-length :active?)
   then (set-font %relevant-length-font%)
   else (set-font %irrelevant-length-font%))
  (erase-text length-x length-y graphics-length)
  (set-font %workspace-font%))

;----------------------------------------------

(defmethod (group-parameter-graphics-obj :draw) ()
  (if* graphics-name
   then (set-font %group-font%)
        (draw-text x y graphics-name)
        (set-font %workspace-font%))
  (if* graphics-length then (send self :draw-length)))

;----------------------------------------------

(defmethod (group-parameter-graphics-obj :erase) ()
  (if* graphics-name 
   then (set-font %group-font%)
        (erase-text x y graphics-name)
        (set-font %workspace-font%))
  (if* graphics-length then (send self :erase-length)))

;----------------------------------------------

(defmethod (group-parameter-graphics-obj :drawn?) ()
  (send (send parent :structure-graphics-obj) :drawn?))

;----------------------------------------------

(defmethod (group :x) ()
  (send graphics-obj :x))  

;----------------------------------------------

(defmethod (group :y) ()
  (send graphics-obj :y))  

;----------------------------------------------

(defmethod (group :draw-parameter) ()
  (send graphics-obj :draw))  

;----------------------------------------------

(defmethod (group :erase-parameter) ()
  (send graphics-obj :erase))  

;----------------------------------------------

(defmethod (group :graphics-name) ()
  (send graphics-obj :graphics-name))

;----------------------------------------------

(defmethod (group :graphics-length) ()
  (send graphics-obj :graphics-length))

;----------------------------------------------

(defmethod (group :init-length-graphics) ()
  (send graphics-obj :init-length-graphics))

;----------------------------------------------

(defmethod (group :draw-length) ()
  (send graphics-obj :draw-length))  

;----------------------------------------------

(defmethod (group :erase-length) ()
  (send graphics-obj :erase-length))  

;----------------------------------------------


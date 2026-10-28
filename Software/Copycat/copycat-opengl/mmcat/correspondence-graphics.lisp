(defflavor correspondence-graphics-obj (obj1 obj2 x1 y1 x2 y2 
					concept-mapping-x concept-mapping-y 
					parent (drawn? nil))
   ; The parent is the correspondence whose graphics-obj this is.
  ())

;---------------------------------------------

(defmethod (correspondence-graphics-obj :get-dash-length) ()
; Returns the length of the dashes to be used in the dashed line for
; displaying the proposed correspondence.
  (if* (= (send parent :proposal-level) 1)
   then %short-correspondence-dash-length%       
   else %long-correspondence-dash-length%))

;---------------------------------------------

(defmethod (correspondence-graphics-obj :get-space-length) ()
; Returns the length of the spaces to be used in the dashed line for
; displaying the proposed correspondence.
    %correspondence-space-length%)

;---------------------------------------------

(defmethod (correspondence :init-graphics) 
           (&aux x1 x2 y1 y2 new-correspondence-graphics-obj)
; Sets up the graphics object for this correspondence.

  (if* (and (send obj1 :string-spanning-group?) 
	    (send obj2 :string-spanning-group?))
   then (setq x1 (send (send obj1 :graphics-obj) 
		       :string-spanning-group-correspondence-x))
        (setq y1 (send (send obj1 :graphics-obj) 
		       :string-spanning-group-correspondence-y))
        (setq x2 (send (send obj2 :graphics-obj) 
		       :string-spanning-group-correspondence-x))
        (setq y2 (send (send obj2 :graphics-obj) 
		       :string-spanning-group-correspondence-y))
   else (setq x1 (send (send obj1 :graphics-obj) :correspondence-x))
        (setq y1 (send (send obj1 :graphics-obj) :correspondence-y))
        (setq x2 (send (send obj2 :graphics-obj) :correspondence-x))
        (setq y2 (send (send obj2 :graphics-obj) :correspondence-y)))

  (setq new-correspondence-graphics-obj
        (make-instance 'correspondence-graphics-obj
	  :parent self
          :obj1 obj1 :obj2 obj2
 	  :x1 x1 :y1 y1 :x2 x2 :y2 y2

	  :concept-mapping-x 
	  (if* (and (send obj1 :string-spanning-group?) 
		    (send obj2 :string-spanning-group?))
           then	%arrow-x% ; Both objects span the entire string.
           else (cond ((send (send self :obj2) :leftmost-in-string?)
                       (- x2 %left-concept-mapping-x-offset%))
		      ((and (typep (send self :obj2) 'group)
			    (send (send self :obj2) :rightmost-in-string?))
                       (- x2 %right-group-concept-mapping-x-offset%))
		      ((send (send self :obj2) :rightmost-in-string?)
                       (- x2 %right-concept-mapping-x-offset%))
		      (t (- x2 %concept-mapping-x-offset%))))

          :concept-mapping-y 
	  (cond ((typep obj2 'letter) 
		 (+ y2 %concept-mapping-y-offset%))
                ((and (send obj1 :string-spanning-group?) 
		      (send obj2 :string-spanning-group?)) 
		 (+ y1 %string-spanning-group-concept-mapping-y-offset%))
                (t (+ (send obj2 :y2) 
		      %group-concept-mapping-y-offset%)))))

  (send self :set-graphics-obj new-correspondence-graphics-obj))

;---------------------------------------------

(defmethod (workspace :equal-or-higher-proposed-correspondence-drawn?) 
           (proposed-correspondence)
; Returns t if a correspondence of equal or higher proposal level has
; already been drawn.
  (loop for c in (remove proposed-correspondence 
	                (aref proposed-correspondence-array
		              (send (send proposed-correspondence 
				          :obj1) :string-number)
	                      (send (send proposed-correspondence 
				          :obj2) :string-number)))
	when (and (send (send c :graphics-obj) :drawn?) 
		  (>= (send c :proposal-level) 
		      (send proposed-correspondence :proposal-level)))
	return t
	finally (return nil)))

;---------------------------------------------

(defmethod (workspace :higher-proposed-correspondence-drawn?) 
           (proposed-correspondence)
; Returns t if a correspondence of higher proposal level has 
; already been drawn.
  (loop for c in (remove proposed-correspondence 
	                 (aref proposed-correspondence-array
		               (send (send proposed-correspondence :obj1) 
				     :string-number)
		                (send (send proposed-correspondence :obj2) 
			              :string-number)))
	when (and (send (send c :graphics-obj) :drawn?) 
		  (> (send c :proposal-level) (send proposed-correspondence 
						    :proposal-level)))
	return t
	finally (return nil)))
							   
;---------------------------------------------

(defun draw-correspondence-grope (from-obj to-obj &aux x1 y1 x2 y2 
				     line-length grope-length)
; Draws groping "lightning bolts" between two objects.
  (if* (and (send from-obj :string-spanning-group?) 
 	    (send to-obj :string-spanning-group?))
   then (setq x1 (send (send from-obj :graphics-obj) 
		       :string-spanning-group-correspondence-x))
        (setq y1 (send (send from-obj :graphics-obj) 
		       :string-spanning-group-correspondence-y))
        (setq x2 (send (send to-obj :graphics-obj) 
		       :string-spanning-group-correspondence-x))
        (setq y2 (send (send to-obj :graphics-obj) 
		       :string-spanning-group-correspondence-y))
   else (setq x1 (send (send from-obj :graphics-obj) :correspondence-x))
        (setq y1 (send (send from-obj :graphics-obj) :correspondence-y))
        (setq x2 (send (send to-obj :graphics-obj) :correspondence-x))
        (setq y2 (send (send to-obj :graphics-obj) :correspondence-y)))

  (if* (and (send from-obj :string-spanning-group?) 
	    (send to-obj :string-spanning-group?))
   then (draw-string-spanning-group-correspondence-grope x1 y1 x2 y2)

   else (setq line-length (round (sqrt (+ (sqr (- y2 y1)) (sqr (- x2 x1))))))
        (setq grope-length (round (/ line-length 4)))
        (if* (< (abs (- x1 x2)) 10) ; Slope is infinity
         then (loop for i from 1 to 3 do
                    (xor-vertical-jagged-line x1 y1 x2 y2 
			                     %vertical-jag-length% 
					     grope-length)
                    (xor-vertical-jagged-line x2 y2 x1 y1 
			                      %vertical-jag-length% 
 		                              grope-length)
                    (cond ((eq %graphics-rate% 'medium) (medium-pause))
 	                  ((eq %graphics-rate% 'slow) (long-pause)))
                    (xor-vertical-jagged-line x1 y1 x2 y2 
			                      %vertical-jag-length% 
			                      grope-length)
                    (xor-vertical-jagged-line x2 y2 x1 y1 
			                      %vertical-jag-length% 
			                      grope-length)
                    (cond ((eq %graphics-rate% 'medium) (medium-pause))
 	                  ((eq %graphics-rate% 'slow) (long-pause))))

         else (loop for i from 1 to 3 do
	            (xor-jagged-line x1 y1 x2 y2 %jag-length% grope-length)
                    (xor-jagged-line x2 y2 x1 y1 %jag-length% grope-length)
                    (cond ((eq %graphics-rate% 'medium) (medium-pause))
 	                  ((eq %graphics-rate% 'slow) (long-pause)))
                    (xor-jagged-line x1 y1 x2 y2 %jag-length% grope-length)
                    (xor-jagged-line x2 y2 x1 y1 %jag-length% grope-length)
                    (cond ((eq %graphics-rate% 'medium) (medium-pause))
 	                  ((eq %graphics-rate% 'slow) (long-pause)))))))

;---------------------------------------------

(defun draw-string-spanning-group-correspondence-grope (x1 y1 x2 y2)
; Draws groping "lightning bolts" between two string-spanning objects.
  (loop for i from 1 to 3 do
        (xor-string-spanning-group-correspondence x1 y1 x2 y2)
        (cond ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (long-pause)))
        (xor-string-spanning-group-correspondence x1 y1 x2 y2)))
   
;---------------------------------------------

(defmethod (correspondence-graphics-obj :draw-line) ()
; Draws the line (dashed for proposed correspondences, jagged for built
; correspondences) between the two objects.
  (if* (and (send obj1 :string-spanning-group?) 
	    (send obj2 :string-spanning-group?))
   then (send self :draw-string-spanning-group-line)
   else (if* (= (send parent :proposal-level) %built%)
         then (if* (< (abs (- x1 x2)) 10) ; Slope is infinity
               then (draw-vertical-jagged-line x1 y1 x2 y2 
			                       %vertical-jag-length%)
               else (draw-jagged-line x1 y1 x2 y2 %jag-length%))
         else (draw-dashed-line x1 y1 x2 y2 
		                (send self :get-dash-length)
		                (send self :get-space-length))))
  (send self :set-drawn? t))

;---------------------------------------------

(defmethod (correspondence-graphics-obj :draw-string-spanning-group-line) 
           (&aux dash-length space-length x-middle)
; Draws the line (dashed for proposed correspondences, jagged for built
; correspondences) between the two string-spanning objects.
  (if* (= (send parent :proposal-level) %built%)
   then (draw-string-spanning-group-correspondence x1 y1 x2 y2)
   else (setq dash-length (send self :get-dash-length))
        (setq space-length (send self :get-space-length))
        (if* %demo-graphics%
	 then (setq x-middle (+ %arrow-x% 40))
	 else (setq x-middle %arrow-x%))
        (draw-dashed-line x1 y1 x-middle y1 dash-length space-length)
        (draw-dashed-line x-middle y1 x-middle y2 dash-length space-length)
        (draw-dashed-line x2 y2 x-middle y2 dash-length space-length)))

;---------------------------------------------

(defmethod (correspondence-graphics-obj :draw) ()
  ; If there is any proposed-correspondence drawn in this position, erase it.
  (loop for proposed-correspondence 
	in (aref (send *workspace* :proposed-correspondence-array)
                 (send obj1 :string-number)
                 (send obj2 :string-number))
	when (send proposed-correspondence :drawn?) do
	     (send proposed-correspondence :erase-line)
	     (return))
  (send self :draw-line)    
  (send self :erase-concept-mappings) ; Erase old display because fonts
                                      ; might have changed.
  (send self :draw-concept-mappings))

;---------------------------------------------

(defmethod (correspondence-graphics-obj :draw-proposed) 
           (&aux new-proposal-level)
; First see if this correspondence should be drawn. Only draw proposed 
; correspondences.  Don't draw anything if there is already a correspondence 
; drawn or if a proposed correspondence of equal or higher proposal level is 
; already drawn.
  (if* (and (send self :proposed?)
	    (not (or (send *workspace* :correspondence-present? parent)
 	              (send *workspace* 
			    :equal-or-higher-proposed-correspondence-drawn? 
			    parent))))
   then ; If anything else is drawn, then erase it.
        (loop for c in (aref (send *workspace* :proposed-correspondence-array)
 	                     (send obj1 :string-number)
                             (send obj2 :string-number))
              when (and c (send c :drawn?)) 
	      do (if* (eq c parent) 
                 ; This only happens when a proposed correspondence is being 
		 ; "bumped up" a level.  It has to be erased at its old level
		 ; and drawn at its new-level.
	         then (setq new-proposal-level (send c :proposal-level))
 	              (send c :set-proposal-level (1- new-proposal-level))
                      (send c :erase-line) 
 	              (send c :set-proposal-level new-proposal-level)
		  else (send c :erase-line))
	         (return))
        (send self :draw-line)))

;---------------------------------------------

(defmethod (correspondence-graphics-obj :flash) (&aux num-of-flashes)
  (if* (eq %graphics-rate% 'fast) 
   then (setq num-of-flashes 10)
   else (setq num-of-flashes 4))
  (loop for i from 1 to num-of-flashes do 
        (send self :erase-line)
        (cond ((eq %graphics-rate% 'fast) (quick-pause))
	      ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (medium-pause)))
        (send self :draw-line)
        (cond ((eq %graphics-rate% 'fast) (quick-pause))
	      ((eq %graphics-rate% 'medium) (medium-pause))
	      ((eq %graphics-rate% 'slow) (medium-pause)))))

;---------------------------------------------

(defmethod (correspondence-graphics-obj :flash-proposed) ()
; Don't flash if there is already a correspondence drawn or if a proposed 
; correspondence of higher proposal level is already drawn.  
  (if* (not (or (send *workspace* :correspondence-present? parent)
	       (send *workspace* :higher-proposed-correspondence-drawn? 
		     parent)))
   then (send self :flash)))

;---------------------------------------------

(defmethod (correspondence :bond-concept-mapping-list) ()
; Return bond-concept-mappings related to this correspondence
; (but not the symmetric ones).
  (loop for cm in accessory-concept-mapping-list
	when (or (and (eq (send cm :description-type1) plato-bond-category)
		      (eq (send cm :descriptor1) 
			  (send obj1 :bond-category)))
		 (and (eq (send cm :description-type1) plato-bond-facet)
		      (eq (send cm :descriptor1) 
			  (send obj1 :bond-facet))))
	collect cm))

;---------------------------------------------

(defmethod (correspondence-graphics-obj :draw-concept-mappings) 
           (&aux (concept-mapping-count 0))
  (loop for concept-mapping 
	in (append (send parent :concept-mapping-list)
		   (send parent :bond-concept-mapping-list)) do
		   
        (if* (send concept-mapping :relevant?)
         then (set-font %relevant-concept-mapping-font%)
	      (send concept-mapping :set-previous-font 
		    %relevant-concept-mapping-font%)
         else (set-font %irrelevant-concept-mapping-font%)
              (send concept-mapping :set-previous-font 
	            %irrelevant-concept-mapping-font%))
        (draw-centered-text 
	    concept-mapping-x 
	    (+ concept-mapping-y (* concept-mapping-count 
				    %space-between-concept-mappings%))
            (send concept-mapping :text) %concept-mapping-text-width%)
        (incf concept-mapping-count))
  
  (set-font %workspace-font%))

;---------------------------------------------

(defun draw-string-spanning-group-correspondence (x1 y1 x2 y2 
					&aux x-middle pair old-x old-y)
  (if* %demo-graphics%
   then (setq x-middle (+ %arrow-x% 40))
   else (setq x-middle %arrow-x%))
  (setq pair (draw-horizontal-jagged-line (+ x1 2) y1 x-middle y1 
	                                  %vertical-jag-length%))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (draw-vertical-jagged-line old-x old-y x-middle y2
                                        %vertical-jag-length%))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (draw-horizontal-jagged-line (+ x2 2) y2 old-x old-y 
	                                  %vertical-jag-length%))
  (setq old-x (car pair) old-y (cdr pair))
  (draw-line old-x old-y x-middle y2))

;---------------------------------------------

(defun xor-string-spanning-group-correspondence (x1 y1 x2 y2 
				       &aux x-middle pair old-x old-y)

  (if* %demo-graphics%
   then (setq x-middle (+ %arrow-x% 40))
   else (setq x-middle %arrow-x%))
  (setq pair (xor-horizontal-jagged-line (+ x1 2) y1 x-middle y1 
	                                  %vertical-jag-length%))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (xor-vertical-jagged-line old-x old-y x-middle y2
                                        %vertical-jag-length%))
  (setq old-x (car pair) old-y (cdr pair))
  (setq pair (xor-horizontal-jagged-line (+ x2 2) y2 old-x old-y 
	                                  %vertical-jag-length%))
  (setq old-x (car pair) old-y (cdr pair))
  (xor-line old-x old-y x-middle y2))

;---------------------------------------------

(defmethod (correspondence :cm-graphics-height) ()
; Returns the height of the display for this correspondence's concept-mappings.
  (* (length (append concept-mapping-list 
		     (send self :bond-concept-mapping-list)))
     %relevant-concept-mapping-font-height%))

;---------------------------------------------

(defmethod (concept-mapping :text) ()
; Returns the text to be displayed for this concept-mapping.
  (format nil "~a->~a" (send descriptor1 :cm-name) 
	               (send descriptor2 :cm-name)))

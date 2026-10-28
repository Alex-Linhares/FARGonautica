;---------------------------------------------
; CODERACK-GRAPHICS:  Graphics routines for displaying the coderack.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defun init-coderack-graphics ()
  (setq *coderack-bar-graph* 
	(make-bar-graph %coderack-x1% %coderack-y1% 
	    %coderack-x2% %coderack-y2% 100 %codelet-short-names%)))

;---------------------------------------------

(defun update-coderack-display (&aux bar-value total-urgency-sum 
				    total-probability-for-codelet
                                    num-of-this-type-in-bin bin 
				    bin-probability-component)

; A bar displays the relative probability that a codelet of the given type 
; will be chosen next.
  (set-font %coderack-font%)   
  (setq total-urgency-sum (send *coderack* :total-urgency-sum))
  (if* (= total-urgency-sum 0)
   then (loop for codelet-number from 0 to (1- (length %codelet-types%)) do
	      (send *coderack-bar-graph* :recompute-bar codelet-number 0)
    	      (send *coderack-bar-graph* :display-bar codelet-number))
   else ; First, for each bin, compute the probability of choosing a codelet 
        ; in that bin.
        (loop for bin-number from 0 to (1- %num-of-urgency-bins%) do
	      (setq bin (get-coderack-bin bin-number))
	      (send bin :set-relative-urgency-sum (/ (send bin :urgency-sum) 
			                             total-urgency-sum))
              (if* (= (send bin :relative-urgency-sum) 0)
               then (send bin :set-codelet-in-bin-probability 0)
	       else (send bin :set-codelet-in-bin-probability
  			      (/ (send bin :relative-urgency-sum)
			         (send bin :fill-pointer)))))

        ; Now compute and display the bar value for each codelet type.
        (loop for codelet-number from 0 to (1- (length %codelet-types%)) do
             (setq total-probability-for-codelet
	           (loop for bin-number 
			 from 0 to (1- %num-of-urgency-bins%) do
			 (setq bin (get-coderack-bin bin-number))
		         (setq num-of-this-type-in-bin 
			       (aref (send *coderack* 
					   :codelet-urgency-array) 
				     codelet-number bin-number))
			 (setq bin-probability-component
			       (* num-of-this-type-in-bin 
				  (send bin :codelet-in-bin-probability)))
              sum bin-probability-component))
        
	     (setq bar-value (round (* total-probability-for-codelet 100)))
  	     (send *coderack-bar-graph* :recompute-bar
		   codelet-number bar-value)
 	     (send *coderack-bar-graph* :display-bar codelet-number)))
  
  (set-font %workspace-font%))

;---------------------------------------------

(defun display-coderack ()
  (set-font %codelet-name-font%)
  (send *coderack-bar-graph* :display) 
  (set-font %workspace-font%)
  (update-coderack-display))

;---------------------------------------------

(defun erase-coderack ()
  (erase-solid-rectangle 
      %coderack-x1% %coderack-y1%
      (+ %coderack-x2% 2)
      (+ (send *coderack-bar-graph* :name-y) (* 3 %codelet-name-font-height%))))

;---------------------------------------------

(defun get-codelet-number (codelet-name)
  (- (length %codelet-types%) (length (memq codelet-name %codelet-types%))))

;---------------------------------------------

(defun get-codelet-name (codelet-number)
  (nth codelet-number %codelet-types%))

;---------------------------------------------

(defmethod (coderack :add-codelet-to-graphics) 
           (codelet &aux codelet-number urgency-bin)

  (setq codelet-number (get-codelet-number (send codelet :codelet-type)))
  (setq urgency-bin (send codelet :urgency-bin))
  (aset codelet-urgency-array 
	codelet-number (send urgency-bin :urgency-code)
        (1+ (aref codelet-urgency-array codelet-number 
		  (send urgency-bin :urgency-code)))))
                               
;---------------------------------------------

(defmethod (coderack :delete-codelet-from-graphics) 
           (codelet &aux codelet-number urgency-bin)  
  (setq codelet-number (get-codelet-number (send codelet :codelet-type)))
  (setq urgency-bin (send codelet :urgency-bin))
  (aset codelet-urgency-array 
	codelet-number (send urgency-bin :urgency-code)
        (1- (aref codelet-urgency-array codelet-number 
		  (send urgency-bin :urgency-code)))))
                               
;---------------------------------------------

(defun init-minimal-coderack-display ()
  (set-font %minimal-coderack-font%)
  (setq %minimal-coderack-string% 
	(format nil "Number of codelets run so far: "))
  (setq %minimal-coderack-x% 
	(- %window-width% 
	   (text-length (string-append
			    %minimal-coderack-string% "10000"))))
  (setq %minimal-coderack-y% (- %slipnet-y% 5))

  (set-font %workspace-font%))

;---------------------------------------------

(defun display-minimal-coderack (&aux string)
  (set-font %minimal-coderack-font%)
  (setq string 
        (format nil "~a~a" %minimal-coderack-string% *codelet-count*))   
  (draw-text %minimal-coderack-x% %minimal-coderack-y% string)
  (setq *old-minimal-coderack-string* string)
  (set-font %workspace-font%))
	   
;---------------------------------------------

(defun update-minimal-coderack-display (&aux string)
  (set-font %minimal-coderack-font%)
  (erase-text %minimal-coderack-x% %minimal-coderack-y% *old-minimal-coderack-string*)
  (setq string (format nil "~a~a" %minimal-coderack-string% 
		       *codelet-count*))
  (draw-text %minimal-coderack-x% %minimal-coderack-y% string)
  (setq *old-minimal-coderack-string* string)
  (set-font %workspace-font%))

;---------------------------------------------

(defun draw-codelet-group-label (x1 x2 label &aux bracket-y label-y)
  (setq bracket-y (+ %codelet-name-bottom-y% 1))
  (setq label-y (+ bracket-y 20))
  (draw-bracket x1 x2 bracket-y)
  (set-font %codelet-group-font%)
  (draw-centered-text x1 label-y label (- x2 x1))
  (set-font %workspace-font%))

;---------------------------------------------

(defun erase-codelet-group-label (x1 x2 label &aux bracket-y label-y)
  (setq bracket-y (+ %codelet-name-bottom-y% 1))
  (setq label-y (+ bracket-y 20))
  (erase-bracket x1 x2 bracket-y)
  (set-font %codelet-group-font%)
  (erase-centered-text x1 label-y label (- x2 x1))
  (set-font %workspace-font%))

;---------------------------------------------

(defun draw-codelet-group-labels 
    (&aux description-bracket-x1 description-bracket-x2
	  bond-bracket-x1 bond-bracket-x2
	  group-bracket-x1 group-bracket-x2
          correspondence-bracket-x1 correspondence-bracket-x2
          rule-bracket-x1 rule-bracket-x2
          other-bracket-x1 other-bracket-x2)
      
  (setq description-bracket-x1 (+ %coderack-x1% 2))
  (setq description-bracket-x2 (- (+ description-bracket-x1 (round (* 37.5 4)))
	                          2))
  (setq bond-bracket-x1 (+ description-bracket-x2 4))
  (setq bond-bracket-x2 (- (+ bond-bracket-x1 (round (* 37.5 5))) 2))
  (setq group-bracket-x1 (+ bond-bracket-x2 4))
  (setq group-bracket-x2 (- (+ group-bracket-x1 (round (* 37.5 5))) 2))
  (setq correspondence-bracket-x1 (+ group-bracket-x2 4))
  (setq correspondence-bracket-x2 
	(- (+ correspondence-bracket-x1 (round (* 37.5 4))) 2))
  (setq rule-bracket-x1 (+ correspondence-bracket-x2 4))
  (setq rule-bracket-x2 (- (+ rule-bracket-x1 (round (* 37.5 4))) 2))
  (setq other-bracket-x1 (+ rule-bracket-x2 4))
  (setq other-bracket-x2 (- (+ other-bracket-x1 (round (* 37.5 2))) 2))

  (draw-codelet-group-label description-bracket-x1 description-bracket-x2 
      "descriptions")
  (draw-codelet-group-label bond-bracket-x1 bond-bracket-x2 "bonds")
  (draw-codelet-group-label group-bracket-x1 group-bracket-x2 "groups")
  (draw-codelet-group-label correspondence-bracket-x1 correspondence-bracket-x2 
      "correspondences")
  (draw-codelet-group-label rule-bracket-x1 rule-bracket-x2 "rules")
  (draw-codelet-group-label other-bracket-x1 other-bracket-x2 "others"))
  
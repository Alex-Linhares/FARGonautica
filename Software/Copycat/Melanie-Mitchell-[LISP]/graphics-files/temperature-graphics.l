(in-package 'user)

; This draws the rectangle.
(defun init-temperature-display ()
  (draw-unfilled-rectangle 
      %temperature-display-x1% %temperature-display-y1% 
      %temperature-display-x2% %temperature-display-y2%)
  (setq *temperature-height* 0))

; This updates the temperture.
(defun update-temperature-display 
    (&optional do-no-matter-what &aux new-temperature-height erase-width)
(block nil
  (set-font %temperature-font%)  
  (setq new-temperature-height 
	(round (/ (* *temperature* %temperature-display-height%) 100)))
  (if* (and (not do-no-matter-what)
           (= new-temperature-height 
	      *temperature-height*))
   then (return))
  ; Erase the old number.
  (if* *old-temperature-string*
   then (setq erase-width 
	      (if* (equal *old-temperature-string* "100")
	       then (+ %temperature-display-width% 5)
	       else (+ %temperature-display-width% 1)))
        (erase-centered-text %temperature-display-x1%
	                     *old-temperature-y* 
                             *old-temperature-string*
                             erase-width))
  (if* (< new-temperature-height *temperature-height*)
   then (erase-solid-rectangle 
	    %temperature-display-x1% 
	    (- %temperature-display-y2% *temperature-height*)
            %temperature-display-x2%
            (- %temperature-display-y2% 
	       new-temperature-height))
        (draw-unfilled-rectangle 
	    %temperature-display-x1% %temperature-display-y1%
            %temperature-display-x2% %temperature-display-y2%)
   else (draw-solid-rectangle 
	    %temperature-display-x1% 
            (- %temperature-display-y2% new-temperature-height)
            %temperature-display-x2% %temperature-display-y2%))

  (setq *temperature-height* new-temperature-height)

  (draw-centered-text %temperature-display-x1%
                      (setq *old-temperature-y*
			    (- (- %temperature-display-y2% 
				  new-temperature-height) 5))
                      (setq *old-temperature-string*
			    (fix-to-string *temperature*))
                      (if* (= *temperature* 100)
		       then (+ %temperature-display-width% 5)  
		       else (+ %temperature-display-width% 1)))
  (set-font %workspace-font%)))

(defun display-temperature ()
  (init-temperature-display)
  (update-temperature-display))

(defun erase-temperature ()
  (erase-solid-rectangle 
      (- %temperature-display-x1% 2) (- %temperature-display-y1% 2)
      (+ %temperature-display-x2% 2) (+ %temperature-display-y2% 2))
  ; Erase the old number.
 (erase-solid-rectangle 
     0
     (- %temperature-display-y2% *temperature-height* 25)
     (+ %temperature-number-x% 25)  
     (+ (- %temperature-display-y2% *temperature-height*) 5)))


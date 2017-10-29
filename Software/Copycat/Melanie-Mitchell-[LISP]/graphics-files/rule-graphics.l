;---------------------------------------------
; BOND GRAPHICS:  This file contains graphics functions for rules.
;---------------------------------------------

(in-package 'user)

(defmethod (rule :rule-string) (&aux part1 part2 )
; Returns a string corresponding to the rule.
(block nil
  (if* (send self :no-change?)
   then (return "Don't replace anything")
        (return))
       
  ; Set up part 1 of the rule string.
  (if* (send descriptor1 :adjective?)
   then (setq part1  (format nil "~a of ~a ~a" 
                                 (send replaced-description-type :pname)
			         (send descriptor1 :pname) 
                                 (send object-category1 :pname)))
   else (if* (null object-category1)
         then (setq part1 (send descriptor1 :pname))
         else (setq part1 (format nil "~a of ~a \`~a\'" 
                                (send replaced-description-type :pname)
			        (send object-category1 :pname) 
                                (send descriptor1 :pname)))))
  ; Set up part 2 of the rule string.
  (if* (send self :relation?)
   then (setq part2 (format nil "~a" (send relation :pname)))
   else (if* (send descriptor2 :adjective?)
         then (setq part2 (format nil "~a of ~a ~a" 
                                      (send replaced-description-type :pname)    
				      (send descriptor2 :pname) 
		                      (send object-category2 :pname)))
	 else (setq part2 (format nil "\`~a\'" (send descriptor2 :pname)))))
  (format nil "Replace ~a by ~a" part1 part2)))
		     
;---------------------------------------------

(defmethod (rule :draw) (mode &aux rule-string rule-string-width rule-x rule-y)
  (setq rule-string (send self :rule-string))
  (set-font %rule-font%)
  (setq rule-string-width (text-length rule-string))
  (if* (= mode %rule-mode%) 
   then (setq rule-x %rule-x%)
        (setq rule-y %rule-y%) 
   else (setq rule-x %translated-rule-x%)
        (setq rule-y %translated-rule-y%))
  (draw-unfilled-rectangle 
      (- rule-x 8) (- rule-y 14) (+ rule-x rule-string-width 8) (+ rule-y 8))
  (draw-text rule-x rule-y rule-string)
  (set-font %workspace-font%))

;---------------------------------------------

(defmethod (rule :erase) (mode &aux rule-string rule-string-width 
			            rule-x rule-y)
  (setq rule-string (send self :rule-string))
  (set-font %rule-font%)
  (setq rule-string-width (text-length rule-string))
  (if* (= mode %rule-mode%) 
   then (setq rule-x %rule-x%)
        (setq rule-y %rule-y%) 
   else (setq rule-x %translated-rule-x%)
        (setq rule-y %translated-rule-y%))
  (erase-unfilled-rectangle 
      (- rule-x 8) (- rule-y 14) (+ rule-x rule-string-width 8) (+ rule-y 8))
  (erase-text rule-x rule-y rule-string)
  (set-font %workspace-font%))

;---------------------------------------------

(defmethod (rule :flash) (mode)
  (loop for i from 1 to 15 do 
	(send self :erase mode)
        (quick-pause)
	(send self :draw mode)
	(quick-pause)))

;---------------------------------------------

(defmethod (rule :slow-flash) (mode)
  (loop for i from 1 to 20 do 
	(send self :erase mode)
        (long-pause)
	(send self :draw mode)
	(long-pause)))

;---------------------------------------------

(defmethod (slipnode :adjective?) ()  
; This is used for displaying the rule.
  (or (eq (send self :category) plato-string-position-category)
      (eq (send self :category) plato-alphabetic-position-category)))
		       
;---------------------------------------------


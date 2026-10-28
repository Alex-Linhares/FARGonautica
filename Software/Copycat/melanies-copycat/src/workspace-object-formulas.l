;---------------------------------------------
; WORKSPACE-OBJECT-FORMULAS:  This file contains formulas for workspace
;                              objects.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defmethod (workspace-object :calculate-raw-importance) (&aux result)
; Returns the raw (as opposed to relative) importance of the object.
; This is a function of the number and activation of the object's
; relevant descriptions.  In addition, the importance of the changed object 
; is enhanced, and the importance of objects in groups is diminished.
  ; Sum the activation of the descriptors of relevant descriptions up
  ; to 300 (this is to limit the relative importance of groups, which have
  ; lots of descriptions, versus letters, which usually have fewer. 

  (setq result 
	(min 300 (list-sum (send-method-to-list 
			       (send-method-to-list 
				   (send self :relevant-descriptions)
				   :descriptor) :activation))))
  (if* (send self :changed?) then (setq result (* 2 result)))
  (if* (send self :group) then (setq result (* 2/3 result)))
  result)
	
;---------------------------------------------

(defmethod (workspace-object :calculate-intra-string-happiness) 
           (&aux result bonds)
; Returns the intra-string happiness of the object.  This value represents
; how well the object is fitting into a structuring of its string (as
; opposed to a mapping from the initial-string to the target-string), and is 
; a function of the strength of bonds or a group involving the object.
; For now I'm giving bonds a third the weight of groups.  Objects
; on the edges of the string expect one bond (outgoing or incoming),
; and objects in the middle expect two.  Bonds are counted only
; if the object is not in a group.  Note that this function assumes
; that there is only one bond between a given pair of objects,
; and that bonds are made only between adjacent pairs of objects.

  (if* (send self :spans-whole-string?) ; Either this is the only letter in 
                                        ; the string or this is a group 
					; spanning the whole string.
   then (setq result 100)   
   else (if* group
         then (setq result (send group :total-strength))
         else (setq bonds (append incoming-bonds outgoing-bonds))
              (if* (null bonds) 
               then (setq result 0)
	       else (if* (or (send self :leftmost-in-string?)
		            (send self :rightmost-in-string?))
                     then (setq result 
				(round (/ (send (car bonds) 
				  	        :total-strength) 3)))
	             else (setq result 
				(round (/ (list-sum 
					      (send-method-to-list 
						  bonds :total-strength))  
					  6)))))))
  result)

;---------------------------------------------

(defmethod (workspace-object :calculate-intra-string-unhappiness) ()
  (fake-reciprocal (send self :intra-string-happiness)))

;---------------------------------------------

(defmethod (workspace-object :calculate-inter-string-happiness) ()
; Returns the inter-string happiness of the object.  This value represents
; how well the object is fitting into a mapping from the initial-string to
; the target-string (as opposed to a structuring of its own string), and is 
; a function of the strength of the object's correspondence, if any.
  (if* correspondence then (send correspondence :total-strength) else 0))

;---------------------------------------------

(defmethod (workspace-object :calculate-inter-string-unhappiness) ()
  (fake-reciprocal (send self :inter-string-happiness)))

;---------------------------------------------

(defmethod (workspace-object :calculate-total-happiness) ()
  (round (average (send self :intra-string-happiness) 
	          (send self :inter-string-happiness))))

;---------------------------------------------

(defmethod (workspace-object :calculate-total-unhappiness) ()
  (fake-reciprocal (send self :total-happiness)))

;---------------------------------------------

(defmethod (workspace-object :calculate-intra-string-salience) ()
; An object's intra-string salience represents how much the object is
; crying out for attention from codelets that build structures
; inside a single string (i.e., bonds and groups).  It is a function
; of the object's relative importance in its string and its intra-string
; unhappiness.  Here, intra-string unhappiness counts much more than the
; importance, since we want there to be pressure for the program to 
; formulate a coherent structuring of the entire string, and not
; leave out objects, even if they're not too importance.  This is somewhat
; domain-dependent.  Salience can be clamped to 100, as it is for the
; object causing a snag, if one is hit.
  (if* clamp-salience?
   then 100
   else (round (weighted-average 
		   `((,(send self :relative-importance) . 2)
   		     (,(send self :intra-string-unhappiness) . 8))))))

;--------------------------------------------

(defmethod (workspace-object :calculate-inter-string-salience) ()
; An object's inter-string salience represents how much the object is
; crying out for attention from codelets that build structures
; between strings (i.e., correspondences).  It is a function
; of the object's relative importance in its string and its inter-string
; unhappiness.  Here, the importance counts much more than the
; inter-string unhappiness, since we want there to be pressure for 
; the program to map important objects, and not pay too much attention
; to mapping unimportant ones.  Salience can be clamped to 100, as it is for
; the object causing a snag, if one is hit.
  (if* clamp-salience?
   then 100
   else (round (weighted-average 
		   `((,(send self :relative-importance) . 8)
   		     (,(send self :inter-string-unhappiness) . 2))))))

;---------------------------------------------

(defmethod (workspace-object :calculate-total-salience) ()
  (round (average (send self :intra-string-salience) 
	          (send self :inter-string-salience))))

;---------------------------------------------

(defmethod (workspace-object :update-object-values) ()
; This updates all the values (e.g., importance and salience) for the object.

  (send self :set-raw-importance (send self :calculate-raw-importance))

  (send self :set-intra-string-happiness 
	(send self :calculate-intra-string-happiness))

  (send self :set-intra-string-unhappiness 
	(send self :calculate-intra-string-unhappiness))

  (send self :set-inter-string-happiness
	(send self :calculate-inter-string-happiness))

  (send self :set-inter-string-unhappiness
	(send self :calculate-inter-string-unhappiness))

  (send self :set-total-happiness
	(send self :calculate-total-happiness))

  (send self :set-total-unhappiness
	(send self :calculate-total-unhappiness))

  (send self :set-intra-string-salience 
	(send self :calculate-intra-string-salience))

  (send self :set-inter-string-salience 
       (send self :calculate-inter-string-salience))

  (send self :set-total-salience 
        (send self :calculate-total-salience)))

;---------------------------------------------

(defmethod (workspace-string :update-relative-importances) 
           (&aux total-raw-importance)
; This updates the relative (normalized) importances of all the objects in 
; the string
  (setq total-raw-importance 
	(list-sum (send-method-to-list 
		      (send self :object-list) :raw-importance)))
  (loop for obj in (send self :object-list) do
	(send obj :set-relative-importance
	      (if* (= total-raw-importance 0)
               then 0 
	       else (round (* 100 (/ (send obj :raw-importance) 
				     total-raw-importance)))))))
  
;---------------------------------------------

(defmethod (workspace-string :calculate-intra-string-unhappiness) ()
; This returns the average of the intra-string unhappinesses of all
; the objects in the string.
  (round (list-average (send-method-to-list (send self :object-list) 
		                            :intra-string-unhappiness))))

;---------------------------------------------

(defmethod (workspace-string :update-intra-string-unhappiness) ()
; This updates the string's intra-string unhappiness (the average of the 
; intra-string unhappinesses of all the objects in the string.
  (send self :set-intra-string-unhappiness 
	(send self :calculate-intra-string-unhappiness)))

;---------------------------------------------

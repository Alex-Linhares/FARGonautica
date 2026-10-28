;---------------------------------------------
; CONCEPT-MAPPINGS: This file contains flavors and methods for 
;                   concept-mappings.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

; Here's an example of how a concept-mapping instance is set up:
; For "rightmost -> leftmost",
; description-type1 = plato-string-position-category,
; description-type2 = plato-string-position-category,
; descriptor1 = plato-rightmost, 
; descriptor2 = plato-leftmost, 
; label = plato-opposite,
; obj1 = the rightmost object,
; obj2 = the leftmost object.  
; The previous font is used in the graphics routines.

(defflavor concept-mapping
  (description-type1 description-type2 descriptor1 descriptor2 label obj1 obj2
   (previous-font %irrelevant-concept-mapping-font%))
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defmethod (concept-mapping :print) ()
  (format t "~a -> ~a" 
	  (send descriptor1 :pname) (send descriptor2 :pname)))

;---------------------------------------------

(defun make-concept-mapping (description-type1 description-type2 
				    descriptor1 descriptor2 obj1 obj2)    
; Returns a new concept mapping.
  (make-instance 'concept-mapping
      :description-type1 description-type1
      :description-type2 description-type2
      :descriptor1 descriptor1
      :descriptor2 descriptor2
      :label (get-label-node descriptor1 descriptor2)
      :obj1 obj1
      :obj2 obj2))

;---------------------------------------------

(defmethod (concept-mapping :slippage?) ()
; Returns t if the concept-mapping is not an identity.
  (not (eq label plato-identity)))

;---------------------------------------------

(defmethod (concept-mapping :degree-of-association) ()
; This assumes the two descriptors in the concept-mapping are connected in
; the slipnet by at most one slip link.  This should eventually be 
; generalized.
  (if* (eq descriptor1 descriptor2)
   then 100
   else (loop for link in (send descriptor1 :lateral-slip-links)
	      when (eq (send link :to-node) descriptor2)
	      return (send link :degree-of-association))))

;---------------------------------------------

(defmethod (concept-mapping :conceptual-depth) ()
  (average (send descriptor1 :conceptual-depth) 
           (send descriptor2 :conceptual-depth)))

;---------------------------------------------

(defmethod (concept-mapping :relevant?) ()
  (and (send description-type1 :active?) (send description-type2 :active?)))

;---------------------------------------------

(defmethod (concept-mapping :distinguishing?) ()
  ; For now, the concept-mapping "whole -> whole" is not considered 
  ; distinguishing, that is, a correspondence cannot be built on it alone.  
  ; This should eventually be generalized or changed.
  (if* (and (eq descriptor1 plato-whole) (eq descriptor2 plato-whole))
   then nil
   else (and (send obj1 :distinguishing-descriptor? descriptor1)
             (send obj2 :distinguishing-descriptor? descriptor2))))

;---------------------------------------------

(defmethod (concept-mapping :label-relevance) ()
  (cond ((null label) 50)
	((send label :active?) 100)
	(t 0)))

;---------------------------------------------

(defmethod (concept-mapping :symmetric-version) ()
; E.g., if the concept-mapping is "rightmost -> leftmost",
; returns "leftmost -> rightmost".
  (cond ((eq (send self :label) plato-identity) self)
	((not (eq (get-label-node (send self :descriptor2) 
		                  (send self :descriptor1))
		  (send self :label)))
	 nil)
	(t (make-concept-mapping 
	       description-type2 description-type1 
	       descriptor2 descriptor1
	       obj1 obj2))))

;---------------------------------------------

(defun contradictory-concept-mappings? (concept-mapping1 concept-mapping2)
; Returns t if the two concept-mappings contradict each other.  E.g,
; "rightmost -> leftmost" contradicts "rightmost -> rightmost".
  (or (and (eq (send concept-mapping1 :descriptor1) 
	       (send concept-mapping2 :descriptor1))
	   (not (eq (send concept-mapping1 :descriptor2) 
		    (send concept-mapping2 :descriptor2))))
      (and (eq (send concept-mapping1 :descriptor2) 
	       (send concept-mapping2 :descriptor2))
	   (not (eq (send concept-mapping1 :descriptor1) 
		    (send concept-mapping2 :descriptor1))))))

;---------------------------------------------

(defun all-opposite-concept-mappings? (concept-mapping-list)
; Returns t if all the concept-mappings in the list have label "opposite".
  (loop for concept-mapping in concept-mapping-list 
	when (not (eq (send concept-mapping :label) plato-opposite)) 
	return nil
	finally (return t)))

;---------------------------------------------

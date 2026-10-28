;---------------------------------------------
; WORKSPACE-OBJECTS: This file contains flavor definitions and functions 
;                     for workspace objects (either letters or groups).
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defflavor workspace-object 
     (string ; The string the object is in (e.g., initial-string).

      string-number ; The object's unique identifying number in its string.
                    ; This number is used for storing and referencing 
		    ; objects and structures stored in vectors and arrays.

      left-string-position ; If the object is a group, this is the string 
                           ; position of the leftmost letter in the group.  
			   ; If the object is a letter, this is its string 
			   ; position.

      right-string-position ; Similar to left-string-position.  For letters,
                            ; this is equal to left-string-position.

     ; Some values for the object.  See the file "object-formulas.l"
     ; for descriptions of these values.
     (raw-importance 0)  
     (relative-importance 0) 
     (intra-string-happiness 0) (intra-string-unhappiness 0) 
     (inter-string-happiness 0) (inter-string-unhappiness 0)
     (total-happiness 0) (total-unhappiness 0)
     (intra-string-salience 0) (inter-string-salience 0) (total-salience 0)

     (descriptions nil) ; A list of descriptions of the object.
     (extrinsic-descriptions nil) ; A list of descriptions with respect to
                                  ; other objects.  For now, only the
				  ; letter in the modified string that 
				  ; corresponds to the changed letter in
				  ; the initial string can be given an 
				  ; extrinsic description (e.g., in 
                                  ; "abc -> abd, pqrs -> ?", the 'd' would
				  ; probably get the extrinsic description
				  ; "Successor of the 'c' in 'abc'").

     ; Variables for structures that can be attached to the object.
     (outgoing-bonds nil) (incoming-bonds nil) 
     (left-bond nil) (right-bond nil)
     (group nil) (replacement nil) (correspondence nil) 

     (changed? nil) ; T if the letter is the initial-string letter that
                    ; changed.

     (new-answer-letter? nil) ; T if this is a new letter for the answer,
                              ; not corresponding to any of the target-string
			      ; letters (this is used when the 
			      ; translated-rule calls for length changes).

     (clamp-salience? nil) ; T if the salience of this object is to be 
                           ; clamped (used when a snag has been hit, to
                           ; clamp the salience of the object causing the 
                           ; snag).
     pname ; The print name of the object.

     graphics-obj) ; The object used in the graphics representation of the
                    ; object.				   
     ()
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defmethod (workspace-object :print) ()
  (format t "~a ~a:~a in ~a~&"
            (send (send self :get-descriptor plato-object-category) :pname)
 	    pname
	    left-string-position
	    (send string :pname)))

;---------------------------------------------

(defmethod (workspace-object :letter-span) ()
; Returns the number of letters spanned by the object.
  (if* (typep self 'letter)
   then 1 
   else (list-sum (send-method-to-list 
		      (send self :object-list) :letter-span))))

;---------------------------------------------

(defmethod (workspace-object :letter-list) ()
; Returns a list of the letters at the lowest level of the object.
  (if* (typep self 'letter)
   then (list self)
   else (flatten (loop for obj in (send self :object-list)
		       collect (send obj :letter-list)))))

;---------------------------------------------

(defmethod (workspace-object :leftmost-in-string?) ()
  (= left-string-position 0))

;---------------------------------------------

(defmethod (workspace-object :middle-in-string?) 
           (&aux left-neighbor right-neighbor)
  (setq left-neighbor (send self :ungrouped-left-neighbor))
  (setq right-neighbor (send self :ungrouped-right-neighbor))
  (and left-neighbor right-neighbor 
       (send left-neighbor :leftmost-in-string?)
       (send right-neighbor :rightmost-in-string?)))

;---------------------------------------------

(defmethod (workspace-object :rightmost-in-string?) ()
  (= right-string-position (1- (length (send string :letter-list)))))

;---------------------------------------------

(defmethod (workspace-object :spans-whole-string?) ()
; Returns t if the object is the single letter in its string or a group 
; that spans the string.
  (= (send self :letter-span) (send string :length)))

;----------------------------------------------

(defmethod (workspace-object :string-spanning-group?) ()
; Returns t if the object is a group that spans the string. 
  (and (typep self 'group) (send self :spans-whole-string?)))

;----------------------------------------------

(defmethod (workspace-object :ungrouped-left-neighbor) ()
; Returns the left-neighbor of the group that either isn't in a group itself,
; or is in the same group as the given object.  E.g., suppose in the
; string "aabbcc",  the pairs of letters have been grouped as 'A', 'B', and
; 'C', and the whole string has been grouped as 'ABC'.  The leftmost 'b' has 
; two left-neighbors:  the letter 'a' and the group 'A', which is in 
; the same higher-level group the 'b' is in, namely, 'ABC'. The ungrouped 
; left-neighbor of the 'b' is the group 'A'.
  (if* (send self :leftmost-in-string?)
   then nil
   else (loop for left-neighbor in (send self :all-left-neighbors)
	      when (or (null (send left-neighbor :group))
		       (recursive-group-member?
			   self (send left-neighbor :group)))
	      return left-neighbor)))

;---------------------------------------------

(defmethod (workspace-object :ungrouped-right-neighbor) ()
; Similar to the "ungrouped-left-neighbor" method.
  (if* (send self :rightmost-in-string?)
   then nil
   else (loop for right-neighbor in (send self :all-right-neighbors) 
	      when (or (null (send right-neighbor :group))
		       (recursive-group-member?
			   self (send right-neighbor :group)))
	      return right-neighbor)))

;---------------------------------------------

(defmethod (workspace-object :all-left-neighbors) ()
; Returns a list of all of the object's immediate left-neighbors, both 
; letters and groups.  E.g., E.g., suppose in the string "aabbcc",  the pairs
; of letters have been grouped as 'A', 'B', and 'C'.  The leftmost 'b' has 
; two left-neighbors:  the letter 'a' and the group 'A'.
  (if* (send self :leftmost-in-string?)
   then nil
   else (loop for obj in (vref (send string :object-position-vector) 
			       (1- left-string-position)) 
	      if (not (and (typep obj 'group) 
			   (recursive-group-member? self obj)))
	      collect obj)))  ; Only collect if obj is not the group this 
                              ; object is in.
              

;---------------------------------------------

(defmethod (workspace-object :all-right-neighbors) ()
; Similar to the "all-left-neighbors" method.
; Returns a list of all of the object's left-neighbors.
  (if* (send self :rightmost-in-string?)
   then nil
   else (loop for obj in (vref (send string :object-position-vector) 
			       (1+ right-string-position)) 
	      if (not (and (typep obj 'group) 
			   (recursive-group-member? self obj)))
	      collect obj)))  ; Only collect if obj is not the group 
                              ; this obj is in.
              
;---------------------------------------------

(defmethod (workspace-object :all-neighbors) ()
; Returns a list of all the immediate neighbors of the object.
  (append (send self :all-left-neighbors) (send self :all-right-neighbors)))

;---------------------------------------------

(defmethod (workspace-object :random-left-neighbor) ()
; Returns a randomly selected left-neighbor.
  (random-list-item (send self :all-left-neighbors)))

;---------------------------------------------

(defmethod (workspace-object :random-right-neighbor) ()
; Returns a randomly selected left-neighbor.
  (random-list-item (send self :all-right-neighbors)))

;---------------------------------------------

(defmethod (workspace-object :random-neighbor) ()
; Returns a randomly selected neighbor.
  (random-list-item (send self :all-neighbors)))

;---------------------------------------------

(defmethod (workspace-object :choose-left-neighbor) ()
; Chooses a left-neighbor probabilistically, based on intra-string-salience.
  (select-list-item-by-method (send self :all-left-neighbors) 
                              :intra-string-salience))

;---------------------------------------------

(defmethod (workspace-object :choose-right-neighbor) ()
; Chooses a right-neighbor probabilistically, based on intra-string-salience.
  (select-list-item-by-method (send self :all-right-neighbors) 
                              :intra-string-salience))
 
;---------------------------------------------

(defmethod (workspace-object :choose-neighbor) ()
; Chooses a neighbor (left or right) probabilistically, based on 
; intra-string-salience.
  (select-list-item-by-method (send self :all-neighbors) 
                              :intra-string-salience))
 
;---------------------------------------------

(defmethod (workspace-object :all-bonds) ()
; Returns all the bonds connected to this object, either incoming or 
; outgoing.
  (append incoming-bonds outgoing-bonds))

;---------------------------------------------

(defmethod (workspace-object :add-description) (d)
; Adds the given description to the object's decription list.
  (send d :set-description-number (length (send self :descriptions)))
  (push d descriptions))

;---------------------------------------------

(defmethod (workspace-object :add-extrinsic-description) (d)
   (push d extrinsic-descriptions))

;---------------------------------------------

(defmethod (workspace-object :get-descriptor) (description-type)
; Returns the descriptor of the object corresponding to the given 
; description-type.  E.g., if the description-type is "object-category", the
; descriptor might be "letter" or "group".  If the description-type
; is "letter-category", the descriptor might be 'A' if the object is an
; 'A'.  If the object has no such description, this returns nil.
; Note: this assumes that there is at most one description with a given
; object facet, which is a limitation of the current version of Copycat.
  (loop for description in descriptions 
        when (eq (send description :description-type) description-type)
        return (send description :descriptor)))

;----------------------------------------------

(defmethod (workspace-object :relevant-descriptions) ()
; Returns a list of the object's relevant descriptions (those whose
; description-type is fully active).
  (loop for description in descriptions
	when (send (send description :description-type) :active?) 
	collect description))
  
;----------------------------------------------

(defmethod (workspace-object :distinguishing-descriptions) ()
; Returns a list of the object's distinguishing descriptions (those which
; distinguish it in the string).
  (loop for d in descriptions 
	when (send self :distinguishing-descriptor? (send d :descriptor)) 
	collect d))

;----------------------------------------------

(defmethod (workspace-object :non-distinguishing-descriptions) ()
  (loop for d in descriptions
	when (null (send self :distinguishing-descriptor? 
	  		      (send d :descriptor)))
	collect d))

;----------------------------------------------

(defmethod (workspace-object :distinguishing-descriptor?) 
           (descriptor &aux other-objects other-descriptors)
; Returns t if no other object of the same type has the same descriptor.
; For now, object-category and length descriptions are not 
; distinguishing.  Maybe this should be changed.  
  (if* (or (eq descriptor plato-letter) (eq descriptor plato-group)
           (member descriptor *slipnet-numbers*))
   then nil
   else (if* (typep self 'letter)
         then (setq other-objects (remove self (send string :letter-list)))
         else (setq other-objects (remove self (send string :group-list)))
              (if* (send self :group) ; Don't count the group this object is 
		                      ; inside of, if there is one.
               then (setq other-objects 
			  (remove (send self :group) other-objects)))
              ; Don't count other groups inside of this group.
              (loop for obj in (send self :object-list) 
                    when (typep obj 'group) do 
                    (setq other-objects (remove obj other-objects))))
        (setq other-descriptors 
              (send-method-to-list 
	          (flatten (send-method-to-list other-objects 
			       :descriptions)) :descriptor))
        (null (memq descriptor other-descriptors))))

;----------------------------------------------
  
(defmethod (workspace-object :relevant-distinguishing-descriptions) ()
  (loop for description in (send self :distinguishing-descriptions)
	when (send (send description :description-type) :active?) 
	collect description))

;----------------------------------------------

(defmethod (workspace-object :relevant-non-distinguishing-descriptions) ()
  (loop for description in (send self :non-distinguishing-descriptions)
	when (send (send description :description-type) :active?) 
	collect description))

;----------------------------------------------

(defmethod (workspace-object :choose-relevant-description-by-activation) 
           (&aux relevant-descriptions)
; Chooses a relevant description probabilistically, based on the descriptor's
; activation.
  (setq relevant-descriptions (send self :relevant-descriptions))
  (if* (null relevant-descriptions)
   then nil
   else (nth (select-list-position 
		 (send-method-to-list 
		     (send-method-to-list relevant-descriptions :descriptor)
	             :activation)) 
	     relevant-descriptions)))

;----------------------------------------------

(defmethod (workspace-object 
	       :choose-relevant-distinguishing-description-by-conceptual-depth) 
           (&aux relevant-distinguishing-descriptions)
; Chooses a relevant, distinguishing description probabilistically, based on 
; the descriptor's conceptual-depth.
  (setq relevant-distinguishing-descriptions 
	(send self :relevant-distinguishing-descriptions))
  (if* (null relevant-distinguishing-descriptions)
   then nil
   else (select-list-item-by-method relevant-distinguishing-descriptions
	                            :conceptual-depth)))

;----------------------------------------------

(defmethod (workspace-object :description-present?) (new-description)
; Returns t if this object already has this description.
  (loop for d in descriptions
	when (and (eq (send d :description-type) 
		      (send new-description :description-type))
		  (eq (send d :descriptor) 
		      (send new-description :descriptor)))
	return t
	finally (return nil)))

;----------------------------------------------

(defmethod (workspace-object :rule-initial-string-descriptions) ()
; Returns all the descriptions that can be used in making the initial-string
; part of the rule, with this object as the changed object in the 
; initial-string.
  (loop for d in descriptions 
	when (and (send (send d :description-type) :active?) 
	          (send self :distinguishing-descriptor? 
			(send d :descriptor))
		  (not (eq (send d :description-type) plato-object-category)))
	collect d))

;----------------------------------------------

(defmethod (workspace-object :rule-modified-string-descriptions) ()
; Returns all the non-extrinsic descriptions that can be used in making the
; modified-string part of the rule, with this object as the object in the
; modified string corresponding to the initial-string changed object.
  (loop for d in descriptions 
	when (and (send (send d :description-type) :active?) 
	          (send self :distinguishing-descriptor? 
			(send d :descriptor))
                  (not (eq (send d :description-type) 
			    plato-string-position-category))
		  (not (eq (send d :description-type) plato-object-category)))
	collect d))

;----------------------------------------------

(defmethod (workspace-object :add-incoming-bond) (b)
; Adds a new incoming bond to the object.
  (push b incoming-bonds))

;----------------------------------------------

(defmethod (workspace-object :add-outgoing-bond) (b)
; Adds a new outgoing bond to the object.
  (push b outgoing-bonds))

;----------------------------------------------

(defmethod (workspace-object :structure-list) ()
; Returns a list of the structures attached to the object.
  (append descriptions 
	  (list left-bond right-bond group correspondence)))

;----------------------------------------------

(defmethod (workspace-object :descriptor-present?) (d)
; Returns t if this object has a description with this descriptor.
  (loop for description in descriptions
	when (eq (send description :descriptor) d) return t
	finally (return nil)))   	       

;----------------------------------------------

(defmethod (workspace-object :description-type-present?) (given-description-type)
; Returns t if this object has a description with this description-type.
  (loop for description in descriptions
	when (eq (send description :description-type) given-description-type) return t
	finally (return nil)))   	       

;----------------------------------------------

(defun letter-distance (obj1 obj2 &aux left-obj right-obj)
; Returns the distance in letters between the two objects.
  (if* (< (send obj1 :left-string-position) 
	  (send obj2 :left-string-position))    
   then (setq left-obj obj1 right-obj obj2)
   else (setq left-obj obj2 right-obj obj1))
  (- (send right-obj :left-string-position) 
     (send left-obj :right-string-position)))

;----------------------------------------------

(defun get-common-groups (obj1 obj2)
; Returns any groups that contain both objects (at some level).
  (loop for group in (send (send obj1 :string) :group-list)
	when (and (recursive-group-member? obj1 group)
		  (recursive-group-member? obj2 group))
	collect group))


;---------------------------------------------
; LETTERS
;---------------------------------------------

(defflavor letter
    ()
    (workspace-object)    
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defun make-letter (string letter-category string-position)
; Returns a new letter.
  (make-instance 'letter 
	    :string string 
    	    :left-string-position string-position
    	    :right-string-position string-position
	    :pname (string-downcase (send letter-category :pname))))
  
;----------------------------------------

; The flavor definition and methods for groups are in the file groups.l.

;----------------------------------------

;---------------------------------------------
; DESCRIPTIONS: This file contains flavors, methods, and codelets for 
;               descriptions.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defflavor description
    (object ; The object to which the description is attached.
     string ; The string this description is in.
     description-type ; The facet of the object that the description refers to,
                  ; e.g., "letter-category", or "string-position-category".
     descriptor ; The descriptor applying to the given facet, e.g., "A"
                ; or "leftmost".
     description-number ; A unique number given to each description of an 
                        ; object.
     )
    (workspace-structure)    
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defun make-description (object description-type descriptor &aux new-description)
; Returns a new description.
  (setq new-description 
	(make-instance 'description 
	    :object object
	    :string (send object :string)
	    :description-type description-type
            :descriptor descriptor))
  new-description)

;---------------------------------------------

(defmethod (description :print) ()
  (format t "~a:~a; ~a: ~a " 
	  (send object :pname) (send object :left-string-position)
          (send description-type :pname) (send descriptor :pname))
  (format t "~%"))

;---------------------------------------------

(defmethod (description :description-string) ()
  (send descriptor :short-name))

;---------------------------------------------

(defmethod (description :relevant?) ()
; Returns t if the description-type being described is active (e.g., if 
; "letter-category" is active, then letter-category descriptions, such as "A",
; are relevant).
  (send description-type :active?))

;---------------------------------------------

(defmethod (description :conceptual-depth) ()
  (send descriptor :conceptual-depth))

;----------------------------------------------

(defmethod (description :bond-description?) ()
; Returns t if the description (of a group) refers to the bonds making up 
; the group, either the bond-category (e.g., "successor") 
; or the bond-facet ("letter-category" or "length").
  (or (eq description-type plato-bond-category)
      (eq description-type plato-bond-facet)))

;---------------------------------------------

(defflavor extrinsic-description
; An extrinsic description is a description with respect to another object on 
; the workspace. For example, "successor" of "letter-category" of <other-obj>.
; In "abc -> abd", the 'd' might get the description "successor of the 'c'".
    (relation ; E.g., "successor".
     description-type-related ; E.g., "letter-category".
     other-obj ; The object this description refers to.
    )
    ()    
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defmethod (extrinsic-description :conceptual-depth) ()
  (send relation :conceptual-depth))

;----------------------------------------------

(defun make-extrinsic-description (relation description-type-related other-obj)
; Returns a new extrinsic description.
  (make-instance 'extrinsic-description
    :relation relation
    :description-type-related description-type-related
    :other-obj other-obj))

;---------------------------------------------

(defmethod (extrinsic-description :print) ()
  (format t "~a of ~a of " (send relation :pname) 
	                   (send description-type-related :pname))
  (send other-obj :print))

;---------------------------------------------

(defmethod (description :apply-slippages) (object slippage-list 
					   &aux new-description-type 
					        new-descriptor)
; Returns a new description with the given slippages applied.
  (setq new-description-type description-type)
  (setq new-descriptor descriptor)
  (loop for s in slippage-list 
	when (eq (send s :descriptor1) description-type) 
	do (setq new-description-type (send s :descriptor2))
	     
	when (eq (send s :descriptor1) descriptor) 
	do (setq new-descriptor (send s :descriptor2)))
	     
  (make-description object new-description-type new-descriptor))
	    
;----------------------------------------------

(defun descriptions-equal? (d1 d2)
; Returns t if the two descriptions are the same.
  (and (eq (send d1 :description-type) (send d2 :description-type))
       (eq (send d1 :descriptor) (send d2 :descriptor))))

;----------------------------------------------

(defun description-member? (description list-of-descriptions)
; Returns t if the given description (or its equivalent) is a member of the 
; given list of descriptions.
  (loop for d in list-of-descriptions
	when (descriptions-equal? description d)
	return t
	finally (return nil)))

;----------------------------------------------

(defun bottom-up-description-scout 
       (&aux chosen-object chosen-description chosen-descriptor
	     has-property-links choice-list chosen-property)

; Chooses an object probabilistically by total salience, and chooses a relevant
; description of the object probabilistically by activation.  Sees if the 
; descriptor has any "has property"
; links that are short enough. (E.g., "A" has a "has property" link to "
; first").  If so, chooses one
; of the properties probabilistically, based on degree of association and 
; activation, proposes a description based on the property, and posts a 
; description-strength-tester codelet with urgency a function of the 
; activation of the property. 

(block nil
  (if* %verbose% 
   then (format t "~&In bottom-up-description-scout.~&"))

  ; Choose an object.
  (setq chosen-object (send *workspace* :choose-object ':total-salience))
  (if* %verbose% 
   then (format t "Chose object ") 
        (send chosen-object :print))

  ; Choose a relevant description of the object.
  (setq chosen-description 
	(send chosen-object :choose-relevant-description-by-activation))
  (if* (null chosen-description)
   then (if* %verbose% 
	 then (format t 
		      "Couldn't choose a description.  Fizzling.~&"))
        (return))
  (setq chosen-descriptor (send chosen-description :descriptor))
  (if* %verbose% then (format t "Chose descriptor ~a~&" 
			        (send chosen-descriptor :pname)))

  ; See if this descriptor has any "has property" links that are short
  ; enough (decided probabilistically).
  (setq has-property-links 
	(send chosen-descriptor :similar-has-property-links))

  (if* %verbose% 
   then (format t "Similar has-property-links:~&")
        (send-method-to-list has-property-links :print))

  (if* (null has-property-links)
   then (if* %verbose% 
         then (format t "No short-enough has-property-links. Fizzling.~&"))
        (return))

  ; Choose a property probabilistically, based on degree of association and 
  ; activation.
  (setq choice-list 
        (list-multiply 
	    (send-method-to-list has-property-links :degree-of-association)
            (send-method-to-list
		(send-method-to-list has-property-links :to-node) 
		:activation)))
  (setq chosen-property 
	(send (nth (select-list-position choice-list) has-property-links)
	      :to-node))

  (if* %verbose% 
   then (format t 
		"Chosen-property: ~a~&" (send chosen-property :pname)))

  (propose-description chosen-object (send chosen-property :category) 
		       chosen-property)))

;---------------------------------------------

(defun top-down-description-scout (description-type 
				   &aux chosen-object possible-descriptors
				        chosen-descriptor)

; Chooses an object probabilistically by total salience, and sees if this 
; object fits any of the descriptions in this description-type's "has-instance" 
; list.  (E.g., if the description-type is "alphabetic-position-category", sees
; if the chosen object can be described as "first" or "last" in the alphabet.)
; If so, proposes a description based on the property, and posts a 
; description-strength-tester codelet with urgency a function of the 
; activation of the proposed descriptor.. 

(block nil
  (if* %verbose% 
   then (format t "~%In top-down-description-scout with description-type ~a~&"
	        (send description-type :pname)))

  ; Choose an object.
  (setq chosen-object (send *workspace* :choose-object ':total-salience))

  (if* %verbose% 
   then (format t "Chose object ") 
        (send chosen-object :print))

  ; See if a description of this type can be made.
  (setq possible-descriptors 
	(send description-type :get-possible-descriptors chosen-object))
  (if* (null possible-descriptors)
   then (if* %verbose% 
	 then (format t 
		      "Couldn't make description.  Fizzling.~&"))
        (return))
  
  (setq chosen-descriptor 
	(select-list-item-by-method possible-descriptors ':activation))

  (propose-description chosen-object description-type chosen-descriptor)))

;---------------------------------------------

(defun description-strength-tester (proposed-description 
				    &aux proposed-description-strength
				         build-probability urgency)
; Calculates the proposed-description's strength, and probabilistically decides
; whether or not to post a description-builder codelet.  If so, the urgency of
; the description-builder codelet is a function of the strength.
(block nil

  ; Activate the descriptor.
  (send (send proposed-description :descriptor) :activate-from-workspace)

  ; Update the strength values for this description.
  (send proposed-description :update-strength-values)

  (setq proposed-description-strength 
	(send proposed-description :total-strength))

  (if* %verbose% 
   then (format t "Proposed description: ")
        (send proposed-description :print)
        (format t "~% Strength: ~a~&" proposed-description-strength))

  ; Decide whether or not to post a description-builder codelet, based on the 
  ; total strength of the proposed-description.
  (setq build-probability 
	(get-temperature-adjusted-probability 
	    (/ proposed-description-strength 100)))
  (if* %verbose% 
   then (format t "Build-probability: ~a~&" build-probability))
  (if* (eq (flip-coin build-probability) 'tails)
   then (if* %verbose% 
	 then (format t "Description not strong enough.  Fizzling.~&"))
        (return))
        
  (setq urgency proposed-description-strength)

  (if* %verbose% 
   then (format t "Posting a description-builder with urgency ~a.~&"
		(get-urgency-bin urgency)))

  ; Post the description-builder codelet.
  (send *coderack* :post 
	(make-codelet 'description-builder (list proposed-description)
	              (get-urgency-bin urgency)))))

;---------------------------------------------

(defun description-builder (proposed-description)
; Tries to build the proposed description, fizzling if the object no longer
; exists or if the description already exists.

(block nil
  (if* %verbose% 
   then (format t "In description-builder with proposed-description ")
        (send proposed-description :print)
	(format t "~%"))
  
  ; If the object no longer exists, then fizzle.
  (if* (null (memq (send proposed-description :object)
		   (send *workspace* :object-list)))
   then (if* %verbose% 
	 then (format t 
		      "This object no longer exists.  Fizzling.~&"))
        (return))

  ; If this description already exists, then fizzle.
  (if* (send (send proposed-description :object) 
	     :description-present? proposed-description)
   then (if* %verbose% 
 	 then (format t "This description already exists.  Fizzling.~&"))
        (send (send proposed-description :description-type) :activate-from-workspace)
        (send (send proposed-description :descriptor) :activate-from-workspace)
        (return))

  (build-description proposed-description)))

;---------------------------------------------

(defun build-description (new-description)
; This function actually builds the new description.
  (if* (send new-description :bond-description?)
   then (send (send new-description :object) 
	      :add-bond-description new-description)
   else (send (send new-description :object) 
	      :add-description new-description))
  (send (send new-description :description-type) :activate-from-workspace)
  (send (send new-description :descriptor) :activate-from-workspace)
  (if* %workspace-graphics% 
   then (send (send new-description :object) 
	      :init-description-graphics new-description))

  ; If the description describes the length of a group, then display the
  ; length.
  (if* (and %workspace-graphics% 
	    (eq (send new-description :description-type) plato-length))
   then (send (send new-description :object) :init-length-graphics)
        (send (send new-description :object) :draw-length))
  
    ; Keep length-description statistics.
    (if* (eq (send new-description :description-type) plato-length)
     then (incf *length-description-count*)))



;---------------------------------------------

(defun propose-description (object description-type descriptor
		            &aux proposed-description urgency)
; Creates a proposed description, and posts a description-strength-tester 
; codelet with  urgency a function of the activation of the description's
; descriptor.
  (setq proposed-description (make-description object description-type descriptor))
  (send (send proposed-description :descriptor) :activate-from-workspace)
  (setq urgency (send description-type :activation))
  (if* %verbose% 
   then (format t "Posting a description-strength-tester with urgency ~a~&" 
		(get-urgency-bin urgency)))
  (send *coderack* :post 
        (make-codelet 'description-strength-tester (list proposed-description)
	              (get-urgency-bin urgency))))

;---------------------------------------------


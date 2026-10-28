;---------------------------------------------
; ANSWER:  This file contains the answer-builder function.
;; Note: The length-change part of these functions are probably not very
; robust.  They work for length additions of 1,
; (e.g., "abc -> abd, mrrjjj -> mrrjjjj", or "abc -> abd, aababc -> aababcd",
; but need to be changed in order to be able to deal with more complicated 
; length changes.
;---------------------------------------------

(in-package 'user)

(defun answer-builder (&aux description-type answer-string-letter-list 
			    objects-to-change)
; Runs the translated rule on the target-string to build the answer string.

  (setq *answer-string* 
	(make-instance 'workspace-string 
	    :highest-string-number -1 :pname "answer string"))
  
  (setq *changed-length-group* nil) ; Used when the answer involves 
                                    ; changing the length of a group.
  
  (setq *snag-object* nil) ; Just in case there is a snag in trying to build
                           ; the answer, set up a global variable for the 
			   ; object causing the snag.

  ; Get a list of the objects in the target string that need changing.
  (if* (send *translated-rule* :no-change?)
   then (setq objects-to-change nil)
   else (setq objects-to-change (get-objects-to-change-for-answer)))

  ; Get the description-type to change (e.g., letter-category or 
  ; string-position-category).
  (setq description-type (send *translated-rule* :replaced-description-type))

  ; Go through the target string and change the objects that need to be 
  ; changed, adding both unchanged and changed objects to the answer-string's
  ; letter-list.
  (loop for obj in (send *target-string* :object-list) do
	(if* (member obj objects-to-change)
         then (setq answer-string-letter-list
		    (append (get-modified-letters-for-answer 
				obj description-type)
		            answer-string-letter-list))))

  (if* *snag-object* ; There was a snag in trying to build an answer.
   then (if* %verbose% 
	 then (format t "Hit a snag!  About to try to deal with it.~&"))
        (if* %workspace-graphics% ; Flash the translated rule
	 then (loop for i from 1 to 30 do (long-pause))
	      (send *translated-rule* :slow-flash %translated-rule-mode%))
        (deal-with-snag) ; This function is defined in the file "run.l".

   else ; No snag.  Set up the answer string.
        (setq answer-string-letter-list 
	      (append (get-unmodified-letters-for-answer objects-to-change) 
		      answer-string-letter-list))

        ; If the translated rule directed a length change, fix the string 
	; positions of the letters in the answer.  Any letter with position 
        ; to the right of the modified group should have 
        ; *amount-length-changed* added to its position.  Here, this assumes
        ; that the length changed at only one position.  This should be
        ; fixed eventually.
        (if* *changed-length-group* 
         then (loop for letter in answer-string-letter-list do
	            when (and (not (memq letter *modified-letter-list*))
			      (> (send letter :left-string-position) 
			         (send *changed-length-group* 
				       :right-string-position)))
	            do (send letter :set-left-string-position
		             (+ (send letter :left-string-position) 
			        *amount-length-changed*))
	               (send letter :set-right-string-position
		             (+ (send letter :left-string-position) 
			        *amount-length-changed*))))
        
        ; Set up the answer-string.
        (send *answer-string* :set-letter-vector 
	      (make-vector (length answer-string-letter-list)))
        (send *answer-string* :set-object-position-vector 
	      (make-vector (length answer-string-letter-list)))
        (send *answer-string* :set-length
	      (make-vector (length answer-string-letter-list)))
        (loop for letter in answer-string-letter-list do
              (send *answer-string* :add-letter letter))
	(send *answer-string* 
	      :set-letter-list (vector-to-list (send *answer-string* :letter-vector)))

        (setq *a* *answer-string*)
        (setq *found-answer* t)
        ; Draw the answer string.  
        (if* %workspace-graphics% then (init-answer-graphics))))

;---------------------------------------------

(defun get-objects-to-change-for-answer 
       (&aux objects-to-change changed-object-correspondence obj2)
; Returns a list of objects in the target-string that should be changed for
; the answer.
  (loop for object in (send *target-string* :object-list) do
        ; See if this object one of the ones the translated rule says to 
	; change.
        (if* (eq (send object :get-descriptor plato-object-category) 
	         (send *translated-rule* :object-category1))
         then (if* (eq (send object :get-descriptor 
			     (send *translated-rule* :descriptor1-facet))
	               (send *translated-rule* :descriptor1))
               then (push object objects-to-change)

	       else ; If the object fits the description given in the 
	            ; translated rule even though it doesn't already have that 
		    ; description, then add that description and add the
                    ; object to the list of objects to change.
                    (if* (and (send (send *translated-rule* :descriptor1)
			            :description-tester)
		               (funcall 
				   (send (send *translated-rule* :descriptor1)
				         :description-tester) object))
	             then (send object :add-description
				(make-description 
				    object 
				    (send *translated-rule* :descriptor1-facet)
 		                    (send *translated-rule* :descriptor1)))
	                  (push object objects-to-change)))))

  ; If the descriptor is "leftmost", "middle", or "rightmost", there might be 
  ; some ambiguity (e.g., there is sometimes more than one "rightmost group", 
  ; as in "aabbcciijjkk".  In this case, only one of the possible objects 
  ; should be changed:  the one the changed-object in the initial-string 
  ; corresponds to, if any, and if not, the highest-level group.  This is not 
  ; very general, and should be fixed eventually.
  (if* (and (eq (send *translated-rule* :descriptor1-facet) 
		plato-string-position-category)
	    (> (length objects-to-change) 1)) ; More than 1 possible object 
                                              ; to change.
   then ; Find the object corresponding to the changed object in the 
        ; initial-string.
        (setq changed-object-correspondence
	      (send (loop for obj in (send *initial-string* :object-list)
		          when (send obj :changed?) return obj) 
		    :correspondence))  
        (if* (and changed-object-correspondence
 	          (memq (setq obj2 (send changed-object-correspondence :obj2))
		        objects-to-change))
	 then (list obj2)
	 else (loop for obj in objects-to-change
		    when (or (null (send obj :group))
			     (not (eq (send (send obj :group) :get-descriptor
					    plato-string-position-category)
				      (send *translated-rule* :descriptor1))))
		    return (list obj)))

   else	objects-to-change))

;---------------------------------------------

(defun get-modified-letters-for-answer 
       (obj description-type &aux new-descriptor new-letter-category
			      new-letter new-string-position
			      first-letter modified-letter-list 
			      group-direction)
; Returns a list of letters modified as directed by the translated rule.
(block nil
  (if* (typep obj 'letter)  ; Only one letter needs to be changed.
   then 
        (setq new-descriptor (get-new-descriptor-for-answer obj description-type))
        (if* (null new-descriptor) ; A snag!  The translated rule cannot
	                           ; be followed for some reason.
        then (setq *snag-object* obj)
             (return))

        ; No snag, so make a modified letter.
        (if* (eq description-type plato-letter-category)
         then (setq new-letter 
		    (make-letter *answer-string* new-descriptor 
			(send obj :left-string-position)))
	      (push new-letter modified-letter-list)
         else (setq *snag-object* obj)) ; Snag:  the description-type is 
	                                ; length, which can't be 
					; applied to a letter.

   else ; Obj is a group.  If a new letter-category is directed (e.g., the
        ; translated-rule is 
	; " Replace letter-category of rightmost group by 'D' "), 
        ; then modify all the letters in the group to be this new category, 
	; with new pname.
	; If a new length is directed (e.g., the translated-rule
        ; is "Replace length of rightmost group by successor"), then
        ; add or subtract letters from the letter-list.  Note that this may
	; not work (I think) in some cases if the group itself contains any 
	; groups.  This should be fixed.
        (if* (eq description-type plato-letter-category)
	 then (loop for letter in (send obj :letter-list) do
	            (setq new-descriptor 
			  (get-new-descriptor-for-answer letter description-type))
		    (if* (null new-descriptor) ; A snag!  
                     then (setq *snag-object* letter)
		          (return))
		    (setq new-letter 
			  (make-letter *answer-string* 
			      new-descriptor
			      (send letter :left-string-position)))
		    (push new-letter modified-letter-list))

         else ; Description-type is length.  
              ; If the group is directed  (e.g., "srqp", going
              ; to the left), increase or decrease the group in the given 
	      ; direction (e.g., srqp).
              (setq *changed-length-group* obj)
              (setq new-descriptor 
		    (get-new-descriptor-for-answer obj description-type))
              (if* (not (memq new-descriptor *slipnet-numbers*)) ; A snag!  
               then (setq *snag-object* obj)
	            (return))

              ; If the group has any groups as members, then signal a snag.
	      ; (The program can't deal with this kind of situation right now.)
              (if* (loop for group-member in (send obj :object-list)
			 when (typep group-member 'group) return t
			 finally (return nil))
               then (setq *snag-object* obj)
	            (return))

	      (setq group-direction (send obj :direction-category))
	      
	      (setq *amount-length-changed* 
		    (- (node-to-number new-descriptor)
		       (node-to-number 
			   (send obj :get-descriptor 
				 plato-length))))

  	      (if* (or (null group-direction)
		       (eq group-direction plato-right))
               then (setq first-letter 
			  (send (send obj :string) :get-letter 
				(send obj :left-obj-position)))
	            (setq new-string-position 
			  (send first-letter :left-string-position))
               else (setq first-letter 
			  (send (send obj :string) :get-letter 
				(send obj :right-obj-position)))
	            (setq new-string-position 
			  (+ (send first-letter :left-string-position)
			     *amount-length-changed*)))

              (setq new-letter 
		    (make-letter *answer-string* 
			(send first-letter :get-descriptor 
			      plato-letter-category)
			new-string-position))

	      (push new-letter modified-letter-list)
              (setq new-string-position 
		    (send new-letter :left-string-position))

              (loop for i from 1 to (1- (node-to-number new-descriptor))
		    until (null new-letter) do
                    (setq new-string-position 
			  (if* (or (null group-direction)
				   (eq group-direction plato-right))
			   then (1+ new-string-position)
			   else (1- new-string-position)))

                    (setq new-letter-category 
			  (funcall (send (send obj :group-category)  
					     :iterate-group) 
			           (get-plato-letter 
				       (string-upcase 
					   (send new-letter :pname)))))
                    (if* (null new-letter-category) ; A snag!
	             then (setq *snag-object* new-letter)
		          (return))
		    (setq new-letter 
			  (make-letter *answer-string*
			      new-letter-category
			      new-string-position))
		    (push new-letter modified-letter-list))
	      (setq *modified-letter-list* modified-letter-list)))

  modified-letter-list))

;---------------------------------------------

(defun get-new-descriptor-for-answer (obj description-type &aux old-descriptor)
; Return the new descriptor that should be applied to the given object    
; for the answer.
  (setq old-descriptor (send obj :get-descriptor description-type))
  (if* (null old-descriptor) 
   then nil
   else (if* (send *translated-rule* :relation?)
         then (send old-descriptor :get-related-node
		    (send *translated-rule* :relation))
         else (send *translated-rule* :descriptor2))))

;---------------------------------------------

(defun get-unmodified-letters-for-answer (objects-to-change)
; Return the letters from the target-string that don't need to be changed
; for the answer.
  (loop for letter in (send *target-string* :letter-list)
	when (not (loop for obj in objects-to-change
			when (member letter (send obj :letter-list))
			return t
			finally (return nil)))
	collect (make-letter 
		    *answer-string* 
		    (send letter :get-descriptor plato-letter-category)
		    (send letter :left-string-position))))
		  
;---------------------------------------------


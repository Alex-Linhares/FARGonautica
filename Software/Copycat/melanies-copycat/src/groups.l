;---------------------------------------------
; GROUPS: This file contains flavors, methods, and codelets for groups.
;---------------------------------------------

(in-package 'user)

(defflavor group
    (group-category ; E.g., "succgrp" or "predgrp".
     (direction-category nil) ; E.g., "left" or "right".
     left-obj right-obj (middle-obj nil) ; The left, right, and middle (if any)
                                         ; objects in this group.
     left-obj-position right-obj-position ; The string-positions of the left 
                                          ; and right objects in this group.
     object-list ; A list of the objects in this group.
     bond-list ; A list of the bonds in this group.
     bond-category ; The bond category associated with the 
                       ; group-category (e.g., "successor" is associated with
                       ; "succgrp").
     (bond-facet nil) ; The description-type upon which the bonds making up
                         ; this group are based (i.e., letter-category or
                         ; length).
     (bond-descriptions nil) ; Descriptions involving the bonds 
                                 ;  making up the group.  These are separated 
				 ; from other descriptions since they are not 
				 ; always used in the same way.
    structure-graphics-obj ; Graphics object for displaying group.
    )
    (workspace-object workspace-structure)    
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defun make-group (string group-category direction-category 
	           left-obj right-obj object-list bond-list 
		   &aux new-group new-group-letter-category 
			length-description-probability new-bond-facet)
; Returns a new group.
  (setq new-group
       (make-instance 'group 
         :string string
         :left-string-position (send left-obj :left-string-position)
         :right-string-position (send right-obj :right-string-position)
	 :structure-category 'group 
         :group-category group-category 
         :direction-category direction-category
         :left-obj left-obj
	 :right-obj right-obj
	 :middle-obj (loop for obj in object-list 
	                   when (eq (send obj :get-descriptor 
					  plato-string-position-category) 
				    plato-middle) 
	                   return obj)
         :left-obj-position (send left-obj :left-string-position)
         :right-obj-position (send right-obj :right-string-position)
	 :object-list object-list
         :bond-list bond-list
	 :bond-category (send group-category 
			      :get-related-node plato-bond-category)))

  ; Add some descriptions to the new group.
  
  ; Add object-category description and string-position description (if any).
  (if* (send new-group :spans-whole-string?)
   then (send new-group :add-description 
	      (make-description new-group plato-string-position-category 
		                plato-whole)))
  (send new-group :add-description 
	(make-description new-group plato-object-category plato-group))

  (cond ((and (send new-group :leftmost-in-string?) 
	      (not (send new-group :spans-whole-string?)))
         (send new-group :add-description 
	       (make-description new-group plato-string-position-category 
		                 plato-leftmost)))
        ((send new-group :middle-in-string?) 
         (send new-group :add-description 
	       (make-description new-group plato-string-position-category 
		                 plato-middle)))
        ((and (send new-group :rightmost-in-string?) 
              (not (send new-group :spans-whole-string?)))
         (send new-group :add-description 
               (make-description new-group plato-string-position-category 
		                 plato-rightmost))))

  ; If new-group is an samegrp based on letter-category, then give it a
  ; letter-category description.
  (if* (and (eq group-category plato-samegrp) 
	    (or (null bond-list)
	        (eq (send (car bond-list) :bond-facet)
		    plato-letter-category)))
   then (setq new-group-letter-category 
	      (send (send new-group :left-obj) :get-descriptor 
		    plato-letter-category))
        (send new-group :add-description 
	      (make-description new-group plato-letter-category 
		                new-group-letter-category))
	(send new-group :set-pname 
	      (send new-group-letter-category :pname)))

  ; Add group-category, direction-category, and bond descriptions.
  (send new-group :add-description 
	(make-description new-group plato-group-category group-category))
  (if* (send new-group :direction-category)
   then (send new-group :add-description 
	      (make-description new-group plato-direction-category 
 		                (send new-group :direction-category))))
  
  ; Add some descriptions of the bonds making up the group to the 
  ; group's bond-descriptions.
  ; The bond-facet is either letter-category or length.
  ; This assumes that all bonds in the group are based on the same facet.
  (if* (send new-group :bond-list)
   then (setq new-bond-facet 
	      (send (car (send new-group :bond-list)) :bond-facet))
        (send new-group :set-bond-facet new-bond-facet)
        (send new-group :add-bond-description 
	      (make-description new-group plato-bond-facet 
		                new-bond-facet)))
 
  (send new-group :add-bond-description 
	(make-description new-group plato-bond-category
	                  (send new-group :bond-category)))
 
  ; Decide whether or not to add a length description to the group.
  ; Only length descriptions from 1 to 5 can be added to groups.  This
  ; is not very general and should eventually be fixed.
  (setq length-description-probability 
        (send new-group :length-description-probability))

  (if* %verbose% 
   then (format t "Deciding whether to add length description.  Prob: ~a~&"
		  length-description-probability))

  (if* (eq (flip-coin length-description-probability) 'heads)
   then (if* %verbose% 
         then (format t "Adding length description.~&"))
        (send new-group :add-description
	      (make-description new-group plato-length
		                (get-plato-number (send new-group :length))))
   else (if* %verbose% 
         then (format t "Not adding length description.~&")))

  (if* %workspace-graphics% 
   then (send new-group :init-graphics)
        (send new-group :init-graphics))

  new-group)

;----------------------------------------------

(defmethod (group :print) (&aux sorted-object-list)
  (setq sorted-object-list 
	(sort object-list #'(lambda (x y) (< (send x :left-string-position)
					     (send y :left-string-position)))))
  (format t "~%")
  (loop for obj in sorted-object-list do 
	(format t "~a" (send obj :pname))) 
  (format t 
	  "; Group-category: ~a; Direction-category: ~a; level ~a" 
	  (if* group-category then (send group-category :pname) else "")
	  (if* direction-category 
	   then (send direction-category :pname) else "")
	  proposal-level)
     
  (format t "~%"))

;---------------------------------------------

(defmethod (group :add-bond-description) (d)
; Adds a bond-description to the group's list of 
; bond-descriptions.
  (push d bond-descriptions))

;---------------------------------------------

(defmethod (group :length) ()
; Returns the number of objects (letters or groups) in the group.
  (length object-list))

;---------------------------------------------

(defmethod (group :leftmost-letter) ()
; Returns the leftmost letter in the group or in the leftmost subgroup of 
; the group.
  (if* (typep left-obj 'letter)
   then left-obj 
   else (send left-obj :leftmost-letter)))

;--------------------------------------------

(defmethod (group :rightmost-letter) ()
; Returns the rightmost letter in the group or in the rightmost subgroup of
; the group.
  (if* (typep right-obj 'letter)
   then right-obj
   else (send right-obj :rightmost-letter)))

;---------------------------------------------

(defmethod (group :leftmost-in-string?) ()
; Returns t if the group is leftmost in its string.
  (if* (= left-obj-position 0) then t else nil))

;---------------------------------------------

(defmethod (group :rightmost-in-string?) ()
; Returns t if the group is rightmost in its string.
  (if* (= right-obj-position (1- (send string :length))) 
   then t else nil))

;---------------------------------------------

(defmethod (group :left-neighbor) (&aux possible-left-neighbor)
  (if* (send self :leftmost-in-string?) 
   then nil
   else (setq possible-left-neighbor
	      (send (send string :get-letter (1- left-obj-position)) 
		    :group))
        (if* (null possible-left-neighbor) 
	 then nil
	 else ; If this group is grouped with the object to the 
	      ; left, or if this group is a subgroup of the group to the left,
              ; don't count the larger group as the left-neighbor.
              (if* (and (not (memq self (send possible-left-neighbor 
					      :object-list)))
			(not (subgroup? self possible-left-neighbor)))
               then possible-left-neighbor else nil))))

;---------------------------------------------

(defmethod (group :right-neighbor) (&aux possible-right-neighbor)
  (if* (send self :rightmost-in-string?) 
   then nil
   else (setq possible-right-neighbor
	      (send (send string :get-letter (1+ right-obj-position)) 
		    :group))
        (if* (null possible-right-neighbor)
         then nil
	 else ; If this group is grouped with the object to the 
              ; right, or if this group is a subgroup of the group to the 
	      ; right, don't count the larger group as the right-neighbor.
              (if* (and (not (memq self (send possible-right-neighbor 
					      :object-list)))
			(not (subgroup? self possible-right-neighbor)))
               then possible-right-neighbor else nil))))

;---------------------------------------------

(defun build-group (new-group 
		    &aux (string (send new-group :string)))
; This function actually builds the new group.
  (send new-group :set-proposal-level %built%)
  (send string :add-group new-group)

  (loop for obj in (send new-group :object-list) do
 	(send obj :set-group new-group))
  (loop for r in (send new-group :bond-list) do 
	(send r :set-group new-group))
  
  (loop for description in (send new-group :descriptions) do
	(send (send description :descriptor) :activate-from-workspace))

  ; Gather statistics on single-letter-groups.
  (if* (= (send new-group :length) 1)
   then (incf *single-letter-group-count*))

  (if* %workspace-graphics% 
   then ; Erase all the proposed bonds and bonds inside this group
        (loop for bond in (send string :proposed-bond-list)
  	      when (and bond (send bond :in-group? new-group) 
			(send bond :drawn?))
	      do (send bond :erase-spline))
	(loop for bond in (send new-group :bond-list) 
	      do (send bond :erase-spline))
        (send new-group :draw))
  
  ; Keep length-description statistics.
  (loop for d in (send new-group :descriptions) 
	when (eq (send d :description-type) plato-length) do
       (incf *length-description-count*)))

;---------------------------------------------

(defun break-group (group &aux (string (send group :string))
			       proposed-bonds proposed-correspondences)

  (if* (send group :group) ; If this group is contained in another group, then
                           ; break the containing group.
   then (break-group (send group :group)))

  (send string :delete-group group)

  ; Delete the proposed bonds from or to this group.
   (setq proposed-bonds 
	(loop for i from 0 to (send string :highest-string-number)
	      collect (append (aref (send string :proposed-bond-array)
		                    (send group :string-number) i)
                              (aref (send string :proposed-bond-array)
			            i (send group :string-number)))))
                                    
  (loop for b in (remove-duplicates (flatten proposed-bonds)) do
        (send string :delete-proposed-bond b)
        (if* %workspace-graphics% then (send b :erase-proposed)))

  ; Break any bonds from or to this group.
  (loop for b in (append (send group :incoming-bonds) 
			 (send group :outgoing-bonds)) do
	(break-bond b))

  ; Delete the proposed correspondences from or to this group. 
  (if* (eq string *initial-string*) 
   then (setq proposed-correspondences 
	      (loop for i from 0 
		    to (send *target-string* :highest-string-number)
		    collect (aref (send *workspace* 
					:proposed-correspondence-array)
				   (send group :string-number) i)))
                                    
   else (setq proposed-correspondences 
	      (loop for i from 0 to (send *initial-string* 
					  :highest-string-number)
		    collect (aref (send *workspace* 
					:proposed-correspondence-array)
				   i (send group :string-number)))))

  (loop for c in (flatten proposed-correspondences) do
        (send *workspace* :delete-proposed-correspondence c)
        (if* %workspace-graphics% then (send c :erase-proposed)))

  ; Break the correspondence from or to this group, if any.
  (if* (send group :correspondence) 
   then (break-correspondence (send group :correspondence)))

  (loop for obj in (send group :object-list) do 
	(send obj :set-group nil))

  (loop for r in (send group :bond-list) do (send r :set-group nil))

  (if* %workspace-graphics% 
   then (send group :erase)
        ; Draw all the proposed bonds and bonds inside this group.
        (loop for r in (send string :proposed-bond-list)
  	      when (and r (send r :in-group? group))
	      do (if* (= (send r :proposal-level) %built%)
                  then (send r :draw)
		  else (send r :draw-proposed)))
        (loop for bond in (send group :bond-list) 
	      if (member bond (send string :bond-list)) do 
	      (send bond :draw))))

;---------------------------------------------

(defun top-down-group-scout--category 
      (group-category  
       &aux string bond-category chosen-object chosen-bond 
            direction-category bond-facet
            opposite-bond-category opposite-direction-category
            direction-to-scan num-of-bonds-to-scan quit
  	    next-bond next-object bond-to-add object-list bond-list
	    i-relevance t-relevance i-unhappiness t-unhappiness
	    possible-single-letter-group 
	    possible-single-letter-group-direction
	    single-letter-group-probability)
			      
; This codelet looks for evidence of a group of the given type.
; Chooses an object, a direction to scan in, and a number of bonds to 
; scan in that direction.  The direction-category of the group is the direction
; of the first bond scanned.  If there are no bonds and the group 
; category is not plato-samegrp, then chooses a direction-category 
; probabilistically as a function of global and local relevance.  Scans until 
; no more bonds of the necessary type and direction are found.  If 
; possible, makes a proposed group of the given type out of the objects 
; scanned, and posts a group-strength-tester codelet with urgency a function 
; of the degree-of-association of bonds of the given bond-category.

(block nil

  (if* %verbose% 
   then (format t "In top-down-group-scout--category with group type ~a~&" 
	        (send group-category :pname)))

  (setq bond-category 
	(send group-category :get-related-node plato-bond-category))

  ; Choose string probabilistically as a function of 
  ; local-bond-category-relevance.
  (setq i-relevance (send *initial-string* :local-bond-category-relevance 
			bond-category))
  (setq t-relevance (send *target-string* :local-bond-category-relevance 
			bond-category))

  (setq i-unhappiness (send *initial-string* :intra-string-unhappiness))
  (setq t-unhappiness (send *target-string* :intra-string-unhappiness))

  (if* %verbose% 
   then (format t "About to choose string.  Relevance of ~a is: " 
		(send bond-category :pname))
        (format t "i-relevance: ~a, t-relevance: ~a~&"
	        i-relevance t-relevance)
	(format t "i-unhappiness: ~a, t-unhappiness: ~a~&"
		i-unhappiness t-unhappiness))
                  
  (setq string 
	(nth (select-list-position 
		 (list (round (average i-relevance i-unhappiness))
		       (round (average t-relevance t-unhappiness))))
	     (list *initial-string* *target-string*)))
		
  (if* %verbose% 
   then (format t "Chose ~a~&" (send string :pname)))

  ; Choose an object on the workspace by intra-string-salience.
  (setq chosen-object (send string :choose-object ':intra-string-salience))

  (if* %verbose% 
   then (format t "Chose object ") (send chosen-object :print) (format t "~%"))
  
  ; If the object is a group that spans the string, fizzle.
  (if* (send chosen-object :spans-whole-string?)
   then (if* %verbose% 
	 then (format t "This object spans the whole string.  Fizzling.~&"))
        (return))

  ; Now choose a direction in which to scan. 
  (setq direction-to-scan
	(cond ((send chosen-object :leftmost-in-string?) plato-right)
              ((send chosen-object :rightmost-in-string?) plato-left)
	      (t (select-list-item-by-method (list plato-left plato-right) 
	                                     :activation))))

  ; Now choose a number of bonds to scan.
  (setq num-of-bonds-to-scan 
	(select-list-position 
	    (send string :num-of-bonds-to-scan-distribution)))

  (if* %verbose% 
   then (format t "About to scan ~a bonds to the ~a~&" 
		  num-of-bonds-to-scan (send direction-to-scan :pname)))
  
  ; Now get the first bond in that direction.
  (if* (eq direction-to-scan plato-left)
   then (setq chosen-bond (send chosen-object :left-bond))
   else (setq chosen-bond (send chosen-object :right-bond)))

  (if* (or (null chosen-bond)
	   (not (eq (send chosen-bond :bond-category) 
		     bond-category)))
   then (if* %verbose% 
         then (format t "No ~a bond in this direction.~&"
		      (send bond-category :pname)))
        (if* (typep chosen-object 'group) 
         then (if* %verbose% 
	       then (format t "Can't make group from single group. ")
	            (format t "Fizzling.~&"))
	      (return))
        (setq object-list (list chosen-object))
        (setq bond-list nil)
        ; A single-letter group should be proposed only if the local
	; support is very high.
        (setq possible-single-letter-group-direction
	      (if* (eq group-category plato-samegrp)
               then nil
	       else (nth (select-list-position ; Select left or right depending
			                       ; on local support.
			     (list (send plato-left 
					 :local-descriptor-support 
					 string plato-group)
				   (send plato-right 
					 :local-descriptor-support 
					 string plato-group)))
			 (list plato-left plato-right))))

        (setq possible-single-letter-group 
	      (make-group 
		  string group-category possible-single-letter-group-direction
                  chosen-object chosen-object object-list bond-list))

        ; If length is active and there is lots of support,
	; then a single-letter group has a good chance.  
	; length has a better chance of staying active
	; if bonds have been built between other lengths.
        (setq single-letter-group-probability 
	      (send possible-single-letter-group 
		    :single-letter-group-probability))

	(if* %verbose% 
	 then (format t "Considering single letter group: ")
	      (send possible-single-letter-group :print)
	      (format t "Propose probability: ~a~&"
		        single-letter-group-probability))
	(if* (eq (flip-coin single-letter-group-probability) 'heads)
         then (if* %verbose% 
               then (format t 
			     "About to propose single letter group!~&"))
              (propose-group object-list bond-list group-category 
		             possible-single-letter-group-direction)
	 else (if* %verbose% 
	       then (format t "Local support not strong enough. Fizzling.~&")))
	(return))
     
  (if* %verbose% 
   then (format t "The first bond is: ") 
        (send chosen-bond :print))

  (setq direction-category (send chosen-bond :direction-category))
  (setq bond-facet (send chosen-bond :bond-facet))

  (setq opposite-bond-category 
	(send bond-category :get-related-node plato-opposite))
  (if* direction-category
   then (setq opposite-direction-category 
	      (send direction-category :get-related-node plato-opposite)))

  ; Get a list of the objects and bonds.
  ; This assumes that there is at most one bond between any pair of 
  ; objects.  If there are bonds that are opposite in bond category 
  ; and direction to the chosen bond, then add their flipped versions to 
  ; the bond list.
  (setq object-list (list (send chosen-bond :left-obj)
			  (send chosen-bond :right-obj)))
  (setq bond-list (list chosen-bond))
  (setq next-bond chosen-bond)
  (loop for i from 2 to num-of-bonds-to-scan until quit do
        (setq bond-to-add nil)
        (if* (eq direction-to-scan plato-left)
         then (setq next-bond (send next-bond :choose-left-neighbor))
	      (if* (null next-bond) 
	       then (setq quit t) 
	       else (setq next-object (send next-bond :left-obj)))
         else (setq next-bond (send next-bond :choose-right-neighbor))
	      (if* (null next-bond) 
	       then (setq quit t) 
	       else (setq next-object (send next-bond :right-obj))))
	       
        ; Decide whether or not to add bond.
	(cond ((null next-bond) (setq bond-to-add nil))
	      ((and (eq (send next-bond :bond-category) 
			bond-category)
		    (eq (send next-bond :direction-category) 
			direction-category)
		    (eq (send next-bond :bond-facet) bond-facet))
               (setq bond-to-add next-bond))
	      ((and (eq (send next-bond :bond-category) 
			opposite-bond-category)
		    (eq (send next-bond :direction-category) 
			opposite-direction-category)
		    (eq (send next-bond :bond-facet) bond-facet))
	       (setq bond-to-add (send next-bond :flipped-version))))
	      
        (if* bond-to-add
         then (push next-object object-list)
              (push bond-to-add bond-list)
	 else (setq quit t)))
  
  (propose-group object-list bond-list group-category direction-category)))
  
;---------------------------------------------
 
(defun top-down-group-scout--direction 
      (direction-category  
       &aux string chosen-object chosen-bond group-category
            bond-category bond-facet
            opposite-bond-category opposite-direction-category
            direction-to-scan num-of-bonds-to-scan quit
  	    next-bond next-object bond-to-add object-list bond-list
	    i-relevance t-relevance i-unhappiness t-unhappiness)
			      
; This codelet looks for evidence of a group of the given direction.
; Chooses an object, a direction to scan in, and a number 
; of bonds to scan in that direction.  The group-category of the group
; is the associated group-category of the first bond scanned.  (Note that
; for now, this codelet cannot propose groups of only one object.) 
; Scans until no more bonds of the necessary type and direction are found.
; If possible, makes a proposed group of the given direction out of the 
; objects scanned, and posts a group-strength-tester codelet with urgency a 
; function of the degree-of-association of bonds of the given 
; bond-category.

(block nil

  (if* %verbose% 
   then (format t "In top-down-group-scout--direction with direction ~a~&" 
	        (send direction-category :pname)))

  (setq i-relevance (send *initial-string* :local-direction-category-relevance 
			direction-category))
  (setq t-relevance (send *target-string* :local-direction-category-relevance 
			direction-category))

  (setq i-unhappiness (send *initial-string* :intra-string-unhappiness))
  (setq t-unhappiness (send *target-string* :intra-string-unhappiness))

  (if* %verbose% 
   then (format t "About to choose string.  Relevance of ~a is: " 
		(send direction-category :pname))
        (format t "initial string: ~a, target string: ~a~&"
		  i-relevance t-relevance)
	(format t "i-unhappiness: ~a, t-unhappiness: ~a~&"
		  i-unhappiness t-unhappiness))

  (setq string 
	(nth (select-list-position 
		 (list (round (average i-relevance i-unhappiness))
		       (round (average t-relevance t-unhappiness))))
	     (list *initial-string* *target-string*)))
		
  (if* %verbose% then (format t "Chose ~a~&" (send string :pname)))

  ; Choose an object on the workspace by intra-string-salience.
  (setq chosen-object (send string :choose-object ':intra-string-salience))

  (if* %verbose% 
   then (format t "Chose object ") (send chosen-object :print) (format t "~%"))
  
  ; If the object is a group that spans the string, fizzle.
  (if* (send chosen-object :spans-whole-string?)
   then (if* %verbose% 
	 then (format t "This object spans the whole string.  Fizzling.~&"))
        (return))

  ; Now choose a direction in which to scan. 
  (setq direction-to-scan
	(cond ((send chosen-object :leftmost-in-string?) plato-right)
              ((send chosen-object :rightmost-in-string?) plato-left)
	      (t (select-list-item-by-method (list plato-left plato-right) 
	                                     :activation))))

  ; Now choose a number of bonds to scan.
  (setq num-of-bonds-to-scan 
	(select-list-position 
	    (send string :num-of-bonds-to-scan-distribution)))

  (if* %verbose% 
   then (format t "About to scan ~a bonds to the ~a~&" 
		  num-of-bonds-to-scan (send direction-to-scan :pname)))
  
  ; Now get the first bond in that direction.
  (if* (eq direction-to-scan plato-left)
   then (setq chosen-bond (send chosen-object :left-bond))
   else (setq chosen-bond (send chosen-object :right-bond)))

  (if* (null chosen-bond)
   then (if* %verbose% then (format t "No bond in this direction.~&"))
	(return))
     
  (if* %verbose% 
   then (format t "The first bond is: ") 
        (send chosen-bond :print))

  (if* (not (eq (send chosen-bond :direction-category) direction-category))
   then (if* %verbose% 
	 then (format t "Chosen bond has wrong direction.  Fizzling.~&"))
        (return))

  (setq bond-category (send chosen-bond :bond-category))
  (setq bond-facet (send chosen-bond :bond-facet)) 

  (setq opposite-bond-category 
	(send bond-category :get-related-node plato-opposite))
  (setq opposite-direction-category 
	(send direction-category :get-related-node plato-opposite))

  ; Get a list of the objects and bonds.
  ; This assumes that there is at most one bond between any pair of 
  ; objects.  If there are bonds that are opposite in bond category 
  ; and direction to the chosen bond, then add their flipped versions to 
  ; the bond list.
  (setq object-list (list (send chosen-bond :left-obj)
			  (send chosen-bond :right-obj)))
  (setq bond-list (list chosen-bond))
  (setq next-bond chosen-bond)
  (loop for i from 2 to num-of-bonds-to-scan until quit do
        (setq bond-to-add nil)
        (if* (eq direction-to-scan plato-left)
         then (setq next-bond (send next-bond :choose-left-neighbor))
	      (if* (null next-bond) 
	       then (setq quit t) 
	       else (setq next-object (send next-bond :left-obj)))
         else (setq next-bond (send next-bond :choose-right-neighbor))
	      (if* (null next-bond) 
	       then (setq quit t) 
	       else (setq next-object (send next-bond :right-obj))))
	       
        ; Decide whether or not to add bond.
	(cond ((null next-bond) (setq bond-to-add nil))
	      ((and (eq (send next-bond :bond-category) 
			bond-category)
		    (eq (send next-bond :direction-category) 
			direction-category)
		    (eq (send next-bond :bond-facet) bond-facet))
               (setq bond-to-add next-bond))
	      ((and (eq (send next-bond :bond-category) 
			opposite-bond-category)
		    (eq (send next-bond :direction-category) 
			opposite-direction-category)
		    (eq (send next-bond :bond-facet) bond-facet))
	       (setq bond-to-add (send next-bond :flipped-version))))
	      
        (if* bond-to-add
         then (push next-object object-list)
              (push bond-to-add bond-list)
	 else (setq quit t)))
  
  (setq group-category 
	(send bond-category :get-related-node plato-group-category))

  (propose-group object-list bond-list group-category direction-category)))

;---------------------------------------------

(defun group-scout--whole-string (&aux string left-obj next-bond next-object
			               bond-list object-list chosen-bond
				       bond-category direction-category 
				       bond-facet group-category right-obj)
; Tries to make a group out of the entire string.
; If possible, makes a proposed string-spanning group and posts a 
; group-strength-tester codelet with urgency a function of the 
; degree of association of bonds of the given bond-category.

(block nil
  (if* %verbose% then (format t "In group-string scout~&"))

  (setq string (send *workspace* :random-string))  

  (if* %verbose% then (format t "Chose ~a~&" (send string :pname)))

  ; Choose a salient leftmost-object, and get an object-list and a 
  ; bond-list.
 
  ; If no bonds, then fizzle.
  (if* (null (send string :bond-list))
   then (if* %verbose% then (format t "No bonds.  Fizzling.~&"))
         (return))
  (setq left-obj (send string :choose-leftmost-object))
  (setq next-bond (send left-obj :right-bond))
  (setq object-list (list left-obj))
  (loop until (null next-bond) do
        (push next-bond bond-list)
	(setq next-object (send next-bond :right-obj))
        (push next-object object-list)
	(setq next-bond (send next-object :right-bond)))
  (setq right-obj next-object)
  (if* (or (null bond-list) (not (send right-obj :rightmost-in-string?)))
   then (if* %verbose% 
         then (format t "Bonds do not span string.  Fizzling.~&"))
        (return))
	
  (if* %verbose% 
   then (format t "The bond-list is ") 
        (send-method-to-list bond-list :print)
        (format t "The object-list is ") 
        (send-method-to-list object-list :print))

  ; Now choose a random bond in the list and try to make a group with 
  ; that bond type and direction.
  (setq chosen-bond (random-list-item bond-list))
  (setq bond-category (send chosen-bond :bond-category))
  (setq direction-category (send chosen-bond :direction-category))
  (setq bond-facet (send chosen-bond :bond-facet))
  (setq bond-list (possible-group-bond-list bond-category 
			  direction-category bond-facet bond-list))

  (if* (null bond-list)
   then (if* %verbose% 
	 then (format t "No possible group.  Fizzling.~&"))
        (return))

  (setq group-category 
	(send bond-category :get-related-node plato-group-category))

  (propose-group object-list bond-list group-category direction-category)))

;---------------------------------------------

(defun group-strength-tester (proposed-group
		      	      &aux proposed-group-strength build-probability 
			           urgency)
; Calculates the proposed-group's strength, and probabilistically decides
; whether or not to post a group-builder codelet.  If so, the urgency of
; the group-builder codelet is a function of the strength.
(block nil
  (if* %verbose% 
   then (format t "In group-strength-tester with group ")
	(send proposed-group :print))

  (if* %workspace-graphics% then (send proposed-group :flash-proposed))

  ; Calculate the proposed group's strength.
  (send proposed-group :update-strength-values)
  (setq proposed-group-strength (send proposed-group :total-strength))

  (if* %verbose% 
   then (format t "Proposed group's strength: ~a~&" proposed-group-strength))

  ; Decide whether or not to post a group-builder codelet, based on the 
  ; strength of the proposed-group.
  (setq build-probability 
	(get-temperature-adjusted-probability (/ proposed-group-strength 100)))

  (if* %verbose% 
   then (format t "Build-probability: ~a~&" build-probability))
  (if* (eq (flip-coin build-probability) 'tails)
   then (if* %verbose% 
	 then (format t "Group not strong enough.  Fizzling.~&"))
        (send (send proposed-group :string) 
	      :delete-proposed-group proposed-group)
          (if* %workspace-graphics% 
	   then (send proposed-group :erase-proposed))
        (return))
        
  ; Activate-from-workspace some descriptions.
  (send (send proposed-group :bond-category) :activate-from-workspace)
  (if* (send proposed-group :direction-category) 
   then (send (send proposed-group :direction-category) 
	      :activate-from-workspace))

  (if* %workspace-graphics% then (send proposed-group :erase-proposed))
  (send proposed-group :set-proposal-level 2)
  (setq urgency proposed-group-strength)
  (if* %verbose% 
   then (format t "Posting a group-builder codelet with urgency ~a~&" 
		(get-urgency-bin urgency)))

  (send *coderack* :post 
	(make-codelet 'group-builder (list proposed-group) 
	              (get-urgency-bin urgency)))

  (if* %workspace-graphics% then (send proposed-group :draw-proposed))))

;---------------------------------------------

(defun group-builder (proposed-group 
		      &aux (string (send proposed-group :string))
		           existing-group incompatible-groups 
			   proposed-group-weight incompatible-group-weight
		           bonds-to-be-flipped flipped-bond 
			   existing-bond new-bond-list 
			   incompatible-correspondences fight-result)
; Tries to build the proposed group, fighting with competitors if necessary.  
(block nil  
  (if* %verbose% 
   then (format t "~%In group-builder with string ~a, and proposed group: "
                (send string :pname))
        (send proposed-group :print))

  ; If this group already exists, then add any new descriptions,
  ; activate descriptions and fizzle.
  (if* (setq existing-group (send string :group-present? proposed-group))
   then (if* %verbose% 
	 then (format t "This group already exists.  Fizzling...~&"))
        (loop for description in (send existing-group :descriptions) do
	      (send (send description :descriptor) :activate-from-workspace))
        ; Add any new descriptions.
        (loop for d in (send proposed-group :descriptions)
	      when (not (send existing-group :description-present? d)) do
	           (build-description
		       (make-description existing-group
			                 (send d :description-type)
			  	         (send d :descriptor))))
        (send string :delete-proposed-group proposed-group)
        (return))

  ; See if all the bonds (or their flipped versions) are still there.
  (let ((all-bonds-still-exist?
	 (loop for r in (send proposed-group :bond-list)
	       when (not (or (send string :bond-present? r)
			     (send string :bond-present? 
				               (send r :flipped-version))))
	       return nil
	       finally (return t))))
    (if* (not all-bonds-still-exist?)
     then (if* %verbose% 
	   then (format t "Not all the bonds in this group still exist. ")
	        (format t "Fizzling.~&"))
          (send string :delete-proposed-group proposed-group)
          (if* %workspace-graphics% 
	   then (send proposed-group :erase-proposed))
          (return)))

  (if* %workspace-graphics% then (send proposed-group :flash-proposed))

  ; Take the proposed group off the list of proposed groups.
  (send string :delete-proposed-group proposed-group)

  ; See if any bonds need to be flipped.  If so, then fight.
  (setq bonds-to-be-flipped 
	(send proposed-group :get-bonds-to-be-flipped))
  (if* bonds-to-be-flipped
   then (if* %verbose%
         then (format t "About to try to flip bonds: ") 
              (send-method-to-list bonds-to-be-flipped :print))
        (setq fight-result 
	      (fight-it-out proposed-group (send proposed-group :letter-span)
                            bonds-to-be-flipped 1))
        (if* (null fight-result)
         then (if* %verbose% 
	       then (format t  "Lost. Fizzling.~&"))
              (if* %workspace-graphics% 
	       then (send proposed-group :erase-proposed))
              (return))
        (if* %verbose%
         then (format t "Won!! Old bonds can be flipped.~&")))

  ; If there are incompatible groups, then fight.  The fight is decided 
  ; probabilistically on the basis of strength.
  (setq incompatible-groups (send proposed-group :get-incompatible-groups))
  (if* incompatible-groups
   then (if* %verbose%
         then (format t "About to fight with incompatible groups: ")
              (loop for g in incompatible-groups do 
		    (send g :print) (format t "~%")))

        (loop for g in incompatible-groups do
        ; If the two groups are of the same type and direction,
	; then the weights should depend on the length of each. 
	; This isn't a very good way to do this, but I'm doing this 
        ; to solve the problem that shorter samegrps are not that much
	; weaker than longer samegrps, and I don't have a group-extender
	; codelet.
	      (if* (and (eq (send proposed-group :group-category)
		            (send g :group-category))
			(eq (send proposed-group :direction-category)
			    (send g :direction-category)))
               then (setq proposed-group-weight (send proposed-group :length))
	            (setq incompatible-group-weight (send g :length))
	       else (setq proposed-group-weight 1)
	            (setq incompatible-group-weight 1))
              (setq fight-result 
		    (fight-it-out proposed-group proposed-group-weight
			          (list g) incompatible-group-weight))
            (if* (null fight-result)
             then (if* %verbose% 
	           then (format t  "Lost. Fizzling.~&"))
                  (if* %workspace-graphics% 
		   then (send proposed-group :erase-proposed))
    	          (return)))
        (if* (null fight-result) then (return))  ; Fizzle.
        (if* %verbose%
         then (format t 
		      "Won!! Old groups can be broken.~&")))

  ; If there are any correspondences incompatible with this group, fight 
  ; with them.
  (if* (and (send proposed-group :direction-category)
	      (setq incompatible-correspondences 
   	            (send proposed-group :get-incompatible-correspondences)))
   then (if* %verbose%
         then (format t "About to fight incompatible correspondences.~&"))
        (setq fight-result 
              (fight-it-out proposed-group 1 incompatible-correspondences 1))
        (if* (null fight-result)
         then (if* %verbose% 
               then (format t "Lost. Fizzling.~&"))
              (if* %workspace-graphics% 
	       then (send proposed-group :erase-proposed))
              (return))
        (if* %verbose%
         then (format t "Won against incompatible correspondences!!~&")))
		    
  ; Break incompatible groups
  (loop for g in incompatible-groups do (break-group g))

  ; Flip any bonds that need flipping, and replace (in group's 
  ; bond-list) any bonds that got rebuilt (and thus are equal but not 
  ; eq to the corresponding bond in the group's bond list).
  (if* bonds-to-be-flipped
   then (loop for r in (send proposed-group :bond-list) do
	      (if* (setq flipped-bond 
			 (send string :bond-present? 
			       (send r :flipped-version)))
	       then (break-bond flipped-bond)
	            (build-bond r)
	            (push r new-bond-list)
               else (if* (not (eq (setq existing-bond 
					(send string :bond-present? r)) r))
                     then (push existing-bond new-bond-list)
	             else (push r new-bond-list)))))

  ; Break incompatible correspondences
  (loop for c in incompatible-correspondences do (break-correspondence c))

  (if* %verbose% then (format t "About to build group.~&"))
  (if* %workspace-graphics% 
   then (if* (send proposed-group :drawn?) 
	 then (send proposed-group :erase-rectangle)))
  (build-group proposed-group)))

;---------------------------------------------

(defmethod (group :flipped-version) (&aux new-bond-list flipped-group)
; Returns the flipped version of this group (e.g., if the group is
; a successor group going to the right, returns a predecessor group going to
; the left, using the same objects).
  (if* (not (or (eq group-category plato-predgrp) 
		(eq group-category plato-succgrp)))
   then self
   else (setq new-bond-list 
	      (loop for r in bond-list collect (send r :flipped-version)))

        (setq flipped-group
              (make-group 
		  string 
		  (send group-category :get-related-node plato-opposite) 
		  (send direction-category :get-related-node plato-opposite) 
		  left-obj right-obj object-list new-bond-list))
        (send flipped-group :set-proposal-level (send self :proposal-level))
        flipped-group))
               
;---------------------------------------------

(defun possible-group-bond-list (bond-category direction-category 
				 bond-facet bond-list
				 &aux new-bond-list quit)
; This is used by the group-scout--whole-string codelet.  It returns a list of
; bonds that could be used in making a group out of the entire string.
  (setq new-bond-list bond-list)
  (loop for bond in new-bond-list until quit do
	(cond ((null bond) (setq new-bond-list nil) (setq quit t))
	      ((not (eq (send bond :bond-facet) bond-facet)) 
	       (setq new-bond-list nil) (setq quit t))
              ((and (eq (send (send bond :bond-category) 
			      :get-related-node plato-opposite)
			bond-category)
 		    (eq (send (send bond :direction-category) 
			      :get-related-node plato-opposite)
			direction-category))
  	        (setq new-bond-list 
		      (subst (send bond :flipped-version) 
			     bond new-bond-list)))
  	      ((or (not (eq (send bond :bond-category) 
			    bond-category))
		   (not (eq (send bond :direction-category) 
			    direction-category)))
               (setq new-bond-list nil) (setq quit t))))
  new-bond-list)

;---------------------------------------------

(defun group-equal? (group1 group2)   
; Returns t if the two groups are the same.
  (if* (and group1 group2
           (= (send group1 :left-obj-position) 
	      (send group2 :left-obj-position))
           (= (send group1 :right-obj-position) 
	      (send group2 :right-obj-position))
           (eq (send group1 :group-category) 
	       (send group2 :group-category))
           (eq (send group1 :direction-category) 
	       (send group2 :direction-category)))
   then t else nil))
   
;---------------------------------------------

(defun in-group? (obj1 obj2)
; Returns t if the two objects are in a group.
 (and (send obj1 :group) (eq (send obj1 :group) (send obj2 :group))))

;---------------------------------------------

(defun subgroup? (group1 group2)
; Returns t if group1 is a subgroup of group2.  Otherwise, returns nil.
  (and (<= (send group2 :left-obj-position) 
	   (send group1 :left-obj-position))
       (>= (send group2 :right-obj-position) 
	   (send group1 :right-obj-position))))

;---------------------------------------------

(defun groups-overlap? (group1 group2)
; Returns t if the two groups overlap.  Otherwise returns nil.
  (intersection (send group1 :object-list) (send group2 :object-list)))

;---------------------------------------------

(defmethod (group :get-incompatible-groups) ()
; Returns a list of the groups that are incompatible with the given group.
  (remove self (remove-duplicates 
		   (flatten (send-method-to-list object-list :group)))))

;---------------------------------------------

(defmethod (group :get-incompatible-correspondences) ()
; Returns a list of the correspondences that are incompatible with the given 
; group.
  (loop for obj in object-list
        when (and (send obj :correspondence) 
	  	  (send self :incompatible-correspondence? 
			     (send obj :correspondence) obj))
        collect (send obj :correspondence)))

;---------------------------------------------

(defmethod (group :incompatible-correspondence?) 
           (c obj &aux string-position-category-concept-mapping other-obj
		    other-bond group-concept-mapping)
; Returns t if the given correspondence is incompatible with the given group.
(block nil
  (setq string-position-category-concept-mapping
        (loop for cm in (send c :concept-mapping-list)
              when (eq (send cm :description-type1) plato-string-position-category)
	      return cm))

  (if* (null string-position-category-concept-mapping)
   then (return nil))
  (setq other-obj (send c :other-obj obj))
  (if* (send other-obj :leftmost-in-string?)
   then (setq other-bond (send other-obj :right-bond))
   else (if* (send other-obj :rightmost-in-string?)
	 then (setq other-bond (send other-obj :left-bond))))
  (if* (or (null other-bond) 
	   (null (send other-bond :direction-category))) 
   then (return nil))
  (setq group-concept-mapping
        (make-concept-mapping 
	    plato-direction-category plato-direction-category
 	    direction-category (send other-bond :direction-category)
	    nil nil))
  (if* (incompatible-concept-mappings? 
	   group-concept-mapping string-position-category-concept-mapping)
   then t)))

;---------------------------------------------

(defmethod (group :get-bonds-to-be-flipped) (&aux bond-to-be-flipped)
; Returns a list of the bonds that need to be flipped in order for this
; group to be built.
  (loop for r in bond-list do
	(setq bond-to-be-flipped 
	      (send string :get-bond (send r :to-obj) (send r :from-obj)))
        when (and (not (null bond-to-be-flipped))
	          (same-bond? r (send bond-to-be-flipped 
					  :flipped-version)))
        collect bond-to-be-flipped))

;---------------------------------------------

(defmethod (group :spans-whole-string?) ()
; Returns t if the group spans the string.
  (= (send self :letter-span) (send string :length)))

;---------------------------------------------

(defmethod (group :proposed?) ()
  (< proposal-level %built%))

;---------------------------------------------
  
(defun propose-group (object-list bond-list group-category 
		      direction-category
		      &aux left-obj right-obj proposed-group urgency 
		           string bond-category)
; Creates a proposed group, and posts a group-strength-tester codelet with 
; urgency a function of the degree of association of bonds of the 
; bond-category associated with this group.
  (setq string (send (car object-list) :string))

  (setq left-obj 
        (nth (list-min-position 
		 (send-method-to-list object-list :left-string-position))
	     object-list))
  (setq right-obj 
	(nth (list-max-position 
		 (send-method-to-list object-list :right-string-position))
	     object-list))

  (setq bond-category 
	(send group-category :get-related-node plato-bond-category))

  (if* %workspace-graphics% then (draw-group-grope left-obj right-obj))

  ; Make proposed-group.
  (setq proposed-group 
	(make-group string group-category direction-category 
	            left-obj right-obj object-list bond-list))

  (send proposed-group :set-proposal-level 1)
  ; Activate-from-workspace some descriptions.
  (send (send proposed-group :bond-category) :activate-from-workspace)
  (if* (send proposed-group :direction-category) 
   then (send (send proposed-group :direction-category) 
	      :activate-from-workspace))

  (send string :add-proposed-group proposed-group)
  (setq urgency (send bond-category :bond-degree-of-association))
                                                        
  (if* %verbose% 
   then (format t 
		"Posting a group-strength-tester codelet with urgency ~a~&" 
		(get-urgency-bin urgency)))

  (send *coderack* :post 
	(make-codelet 'group-strength-tester (list proposed-group) 
	              (get-urgency-bin urgency)))

  (if* %workspace-graphics% then (send proposed-group :draw-proposed)))

;-------------------------------------------

(defmethod (group :single-letter-group-probability) (&aux exponent)
; Returns the probability to be used in deciding whether or not to propose the
; single-letter-group g.  
  (setq exponent (case (send self :number-of-local-supporting-groups)
		       (1 4)
		       (2 2)
		       (otherwise 1)))
	
  (get-temperature-adjusted-probability
      (expt (* (/ (send self :local-support) 100)
	       (/ (send plato-length :activation) 100)) exponent)))

;-------------------------------------------

(defmethod (group :length-description-probability) ()
  (if* (> (send self :length) 5)
   then 0
   else (get-temperature-adjusted-probability 
	    (expt .5 (* (cube (send self :length))
			(/ (fake-reciprocal (send plato-length 
						  :activation)) 100))))))

;-------------------------------------------


;---------------------------------------------
; WORKSPACE-STRINGS:  This file contains flavor definitions and functions for 
;                      workspace strings.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------
; The strings are the initial string, the modified string,
; and the target string.  Each object (letter or group) in a 
; string has a unique "string number" that distinguishes it from other 
; objects in that string.

(defflavor workspace-string
    (highest-string-number ; The highest string-number of any object in
	                   ; the string.

     letter-vector ; A vector containing all the letters in the string.

     letter-list ; A list of all the letters in the string.

     (proposed-bond-array nil) ; An array containing all the proposed 
                                   ; bonds in the string.  Each proposed 
				   ; bond is stored at the position 
 			           ;    (string-number-of-from-obj,
			           ;	    string-number-of-to-obj)
                                   ; in the array.

     (left-right-bond-array nil) ; An array containing all the built 
                                     ; bonds in the string.  Each bond
				     ; is stored at the position 
				     ;    (string-number-of-left-obj,
				     ;	      string-number-of-right-obj)
				     ; in the array.
     
     (from-to-bond-array nil) ; An array containing all the built 
                              ; bonds in the string.  Each bond is 
			      ; storedat the position 
			      ;      (string-number-of-from-obj,
			      ;	         string-number-of-to-obj)
			      ; in the array.

     (proposed-group-array nil) ; An array containing all the proposed groups
                                ; in the string.  Each group is stored at the
				; position
				;   (string-number-of-left-obj,
				;	string-number-of-right-obj)
				; in the array.

     (group-vector nil) ; A vector containing all the built groups in 
                        ; the string.  Each group is stored at the index
			;          string-number-of-left-obj
			; in the vector.

     (object-position-vector nil) ; A vector containing all the objects 
                                  ; (letters and groups) in the string.
				  ; Each object is stored at the position
				  ;     left-string-position
				  ; in the vector.

     length ; The number of letters in the string.
     pname ; The print name of the string.
     object-spaces ; The number of spaces available for objects 
                   ; (letters or groups) in the string.  This number
		   ; sometimes needs to be increased when new objects 
		   ; (groups) are created.
     num-of-bonds-to-scan-distribution ; A probability distribution for
                                           ; group-scout codelets to use in 
				           ; deciding how many bonds to scan
				           ; in the string.  This is set up in
				           ; the file "preprocess.l". 

     (intra-string-unhappiness 0) ; A variable for the average 
                                  ; intra-string-unhappiness of objects in
				  ; the string.
     x y) ; Coordinates for graphical display of the string.
    ()                               
    :gettable-instance-variables    
    :settable-instance-variables            
    :initable-instance-variables)

;---------------------------------------------

(defmethod (workspace-string :random-object) ()
; Returns a random object in the string.
  (random-list-item (send self :object-list)))

;---------------------------------------------

(defmethod (workspace-string :random-letter) ()
; Returns a random letter in the string.
  (random-list-item (send self :letter-list)))

;---------------------------------------------

(defmethod (workspace-string :choose-object) (method &aux value-list)
; Returns an object in the string chosen probabilistically 
; (adjusted for temperature) according to the given method.    
  (setq value-list (send-method-to-list (send self :object-list) method))
  (nth (select-list-position (get-temperature-adjusted-value-list value-list))
       (send self :object-list)))

;---------------------------------------------

(defmethod (workspace-string :choose-leftmost-object) (&aux leftmost-objects)
; Returns one of the leftmost objects in the string (there is a 
; leftmost letter and zero or more leftmost groups), chosen 
; probabilistically according to the relative importance of the various 
; leftmost objects in the string.
  (setq leftmost-objects 
	(loop for obj in (send self :object-list)
	      when (eq (send obj :get-descriptor 
			         plato-string-position-category) 
		       plato-leftmost)
	      collect obj))
  (if* (null leftmost-objects) 
   then nil
   else (select-list-item-by-method leftmost-objects :relative-importance)))
	             
;---------------------------------------------

(defmethod (workspace-string :proposed-bond-list) ()
; Returns a list of all the proposed bonds in the string.
; Removes repeats, since sameness bonds are stored twice.
  (remove-duplicates (flatten (array-to-list proposed-bond-array))))

;---------------------------------------------

(defmethod (workspace-string :bond-list) ()
; Returns a list of all the bonds in the string.
  (remove-duplicates (flatten (array-to-list from-to-bond-array))))

;---------------------------------------------

(defmethod (workspace-string :proposed-group-list) ()
; Returns a list of all the proposed groups in the string.
  (flatten (array-to-list proposed-group-array)))

;---------------------------------------------

(defmethod (workspace-string :group-list) ()
; Returns a list of all the groups in the string.
  (flatten (vector-to-list group-vector)))

;---------------------------------------------

(defmethod (workspace-string :object-list) ()
; Returns a list of all the objects in the string.
  (append (send self :letter-list) (send self :group-list)))

;---------------------------------------------

(defmethod (workspace-string :non-string-spanning-object-list) ()
; Returns a list of all the non-string-spanning objects in the string.
  (loop for obj in (send self :object-list)
	when (not (send obj :spans-whole-string?)) collect obj))

;---------------------------------------------

(defmethod (workspace-string :add-letter) (l)
; Adds a letter to the string.
  (send l :set-string-number 
	  (setq highest-string-number (1+ highest-string-number)))
  (vset (send self :letter-vector) (send l :left-string-position) l)
  ; Add letter to the object-position-vector (used for determining left 
  ; and right neighbors of objects).
  (vset (send self :object-position-vector) 
	(send l :left-string-position) 
	(cons l (vref (send self :object-position-vector) 
			   (send l :left-string-position)))))

;---------------------------------------------

(defmethod (workspace-string :get-object-list) (object-category)
; Returns the list of objects in the string of the given 
; object-category.
  (cond ((eq object-category plato-letter) (send self :letter-list))
        ((eq object-category plato-group) 
	 (send self :group-list))))

;---------------------------------------------

(defmethod (workspace-string :add-proposed-bond) (b)
; Adds a proposed bond to the string.
  (aset (send self :proposed-bond-array) 
             (send (send b :from-obj) :string-number)
             (send (send b :to-obj) :string-number)
	     (cons b (aref (send self :proposed-bond-array)
                           (send (send b :from-obj) :string-number)
                           (send (send b :to-obj) :string-number)))))

;---------------------------------------------

(defmethod (workspace-string :delete-proposed-bond) (b)
; Deletes a proposed bond from the string.
  (aset (send self :proposed-bond-array) 
             (send (send b :from-obj) :string-number)
             (send (send b :to-obj) :string-number)
	     (remove b 
		   (aref (send self :proposed-bond-array) 
                              (send (send b :from-obj) :string-number)
                              (send (send b :to-obj) 
				    :string-number)))))

;---------------------------------------------

(defmethod (workspace-string :add-bond) (b)
; Adds a built bond to the string.
  (aset (send self :left-right-bond-array) 
             (send (send b :left-obj) :string-number)
             (send (send b :right-obj) :string-number)
	      b)

  (aset (send self :from-to-bond-array) 
             (send (send b :from-obj) :string-number)
             (send (send b :to-obj) :string-number)
             b)
  
  ; Store sameness bonds twice, in both directions, since they
  ; have no direction.
  (if* (eq (send b :bond-category) plato-sameness)
   then (aset (send self :left-right-bond-array) 
                   (send (send b :right-obj) :string-number)
                   (send (send b :left-obj) :string-number)
  	           b)

        (aset (send self :from-to-bond-array) 
                   (send (send b :to-obj) :string-number)
                   (send (send b :from-obj) :string-number)
                   b)))
  
;---------------------------------------------

(defmethod (workspace-string :delete-bond) (b)
; Deletes a built bond from the string.
  (aset (send self :left-right-bond-array) 
             (send (send b :left-obj) :string-number)
             (send (send b :right-obj) :string-number)
             nil)

  (aset (send self :from-to-bond-array) 
             (send (send b :from-obj) :string-number)
             (send (send b :to-obj) :string-number)
             nil)

  (if* (eq (send b :bond-category) plato-sameness)
   then (aset (send self :left-right-bond-array) 
                   (send (send b :right-obj) :string-number)
                   (send (send b :left-obj) :string-number)
  	           nil)

        (aset (send self :from-to-bond-array) 
                   (send (send b :to-obj) :string-number)
                   (send (send b :from-obj) :string-number)
                   nil)))
  
;---------------------------------------------

(defmethod (workspace-string :add-proposed-group) (g)
; Adds a proposed group to the string.
; A proposed group is stored at the position 
;   [(send left-obj :string-number), (send right-obj :string-number)]
; in the proposed-group-array.
  (aset (send self :proposed-group-array) 
             (send (send g :left-obj) :string-number)
             (send (send g :right-obj) :string-number)
 	     (cons g (aref (send self :proposed-group-array)
                           (send (send g :left-obj) :string-number)
                           (send (send g :right-obj) :string-number)))))

;---------------------------------------------

(defmethod (workspace-string :delete-proposed-group) (g)
; Deletes a proposed group from the string.
  (aset (send self :proposed-group-array) 
             (send (send g :left-obj) :string-number)
             (send (send g :right-obj) :string-number)
	     (remove g (aref (send self :proposed-group-array)
                             (send (send g :left-obj) :string-number)
                             (send (send g :right-obj) :string-number)))))

;---------------------------------------------
    
(defmethod (workspace-string :add-group) (g)
; Adds a built group to the string.  A built group is stored at the position 
;       (send left-obj :string-number)
; in the group vector.  
  (vset (send self :group-vector)
	(send (send g :left-obj) :string-number) g)

  (send g :set-string-number 
	(setq highest-string-number (1+ highest-string-number)))

  (send self :make-room-for-new-object)

  ; Add g to the object-position-vector 
  ; (used for determining left and right neighbors of objects).
  (vset (send self :object-position-vector) 
	(send g :left-string-position)
        (cons g (vref (send self :object-position-vector) 
 		        (send g :left-string-position))))
  (vset (send self :object-position-vector) 
	(send g :right-string-position)
        (cons g (vref (send self :object-position-vector) 
			      (send g :right-string-position)))))

;---------------------------------------------

(defmethod (workspace-string :delete-group) (g)
; Deletes a built group from the string.
  (vset (send self :group-vector) 
	(send (send g :left-obj) :string-number) nil)

  ; Delete g from the object-position-vector (used for determining 
  ; left and right neighbors of objects).
  (vset (send self :object-position-vector) 
	(send g :left-string-position)
        (remove g (vref (send self :object-position-vector) 
			 (send g :left-string-position))))

  (vset (send self :object-position-vector) 
	(send g :right-string-position)
        (remove g (vref (send self :object-position-vector) 
		   	 (send g :right-string-position)))))
  
;---------------------------------------------


(defmethod (workspace-string :make-room-for-new-object) () 
; When a new object (i.e., a new group, since new letters are not 
; created) is created, there is sometimes a need to create more space for it 
; in the string.  This method determines if more space is needed, and if 
; so, creates it.
(block nil
  (if* (> object-spaces highest-string-number)
   then (return))  ; There is enough space for the new object.
  
  ; If there is not enough room for the new object, then double the storage
  ; space.
  (setq object-spaces (* 2 object-spaces))
  (adjust-array proposed-bond-array 
      (list (* 2 (array-rows proposed-bond-array))
            (* 2  (array-columns proposed-bond-array)))
      :initial-element nil)
  
  (adjust-array from-to-bond-array 
      (list (* 2 (array-rows from-to-bond-array))
            (* 2 (array-columns from-to-bond-array)))
      :initial-element nil)

  (adjust-array left-right-bond-array 
      (list (* 2 (array-rows left-right-bond-array))
            (* 2 (array-columns left-right-bond-array)))
      :initial-element nil)
      
  (adjust-array proposed-group-array 
      (list (* 2 (array-rows proposed-group-array))
	    (* 2 (array-columns proposed-group-array)))
      :initial-element nil)
      
  (adjust-array group-vector (list (* 2 (vsize group-vector))) 
                :initial-element nil)

  (cond ((eq self *target-string*)
         (adjust-array (send *workspace* :proposed-correspondence-array) 
	               (list (array-dimension 
				 (send *workspace* 
				       :proposed-correspondence-array) 0)
	                     (* 2 (array-dimension 
				      (send *workspace* 
					    :proposed-correspondence-array)
				       1)))
		       :initial-element nil))

	((eq self *initial-string*)
         (adjust-array (send *workspace* :proposed-correspondence-array) 
    	               (list (* 2 (array-dimension 
				      (send *workspace* 
					    :proposed-correspondence-array) 
				      0))
 	                     (array-dimension 
				 (send *workspace* 
				       :proposed-correspondence-array) 1))
		       :initial-element nil)

         (adjust-array (send *workspace* :correspondence-vector)
                       (list (* 2 (vsize (send *workspace* 
					       :correspondence-vector))))
		       :initial-element nil)))))

;---------------------------------------------

(defmethod (workspace-string :get-letter) (position)
; Returns the object at the given position in the letter-vector of the
; string. (Positions begin at 0.)
  (vref letter-vector position))

;---------------------------------------------

(defmethod (workspace-string :get-group) (position)
; Returns the group- at the given position in the letter-vector of the
; string. (Positions begin at 0 and refer to the position of the
; leftmost object in the group).
  (send (send self :get-letter position) :group))

;---------------------------------------------

(defmethod (workspace-string :bond-present?) (bond &aux existing-bond)
; If the given bond exists in the string, returns the 
; existing-bond.  Otherwise, returns nil.
   (setq existing-bond
	(aref (send self :from-to-bond-array) 
	           (send (send bond :from-obj) :string-number)
	           (send (send bond :to-obj) :string-number)))
  (if* (and existing-bond
           (eq (send existing-bond :bond-category) 
	       (send bond :bond-category))
           (eq (send existing-bond :direction-category) 
	       (send bond :direction-category)))
   then existing-bond
   else nil))

;---------------------------------------------

(defmethod (workspace-string :get-bond) (from-obj to-obj)
; Returns the bond (if any) from the FROM-OBJ to the TO-OBJ in the
; string.
  (aref (send self :from-to-bond-array) 
             (send from-obj :string-number)
             (send to-obj :string-number)))

;---------------------------------------------

(defmethod (workspace-string :group-present?) (group &aux existing-group)
; If the given group exists in the string, returns the existing-group.
; Otherwise, returns nil.
 
  (setq existing-group 
	(vref (send self :group-vector) 
	      (send (send group :left-obj) :string-number)))
  (if* (and existing-group
           (= (send existing-group :length) (send group :length))
           (eq (send existing-group :group-category) 
	       (send group :group-category))
           (eq (send existing-group :direction-category) 
	       (send group :direction-category)))
   then existing-group
   else nil))

;---------------------------------------------

(defmethod (workspace-string :pstring) (&aux (string ""))
; Prints out the letters in the string.
   (loop for letter in (send self :letter-list) do
         (setq string (string-append string (send letter :pname))))
  (string-downcase string))


;---------------------------------------------

;---------------------------------------------
; WORKSPACE: This file contains flavor definitions and methods for the 
;            workspace.
;---------------------------------------------

(in-package 'user)

(defflavor workspace
; The workspace contains a list of replacements (mappings from the 
; initial string to the modified string, e.g., from "abc" to "abd"),
; a vector of correspondences (mappings from the initial-string to the
; target string, e.g., from "abc" to "pqrs"), and an array of proposed
; correspondences.
    ((replacement-list nil) proposed-correspondence-array 
     correspondence-vector)
    ()
    :gettable-instance-variables    
    :settable-instance-variables            
    :initable-instance-variables)

;---------------------------------------------

(defmethod (workspace :proposed-bond-list) ()
; Returns a list of the proposed bonds on the workspace.
  (append (send *initial-string* :proposed-bond-list)
	  (send *target-string* :proposed-bond-list)))

;---------------------------------------------

(defmethod (workspace :bond-list) ()
; Returns a list of the built bonds on the workspace.
  (append (send *initial-string* :bond-list)
	  (send *target-string* :bond-list)))

;---------------------------------------------

(defmethod (workspace :proposed-group-list) ()
; Returns a list of the proposed groups on the workspace.
  (append (send *initial-string* :proposed-group-list)
	  (send *target-string* :proposed-group-list)))

;---------------------------------------------

(defmethod (workspace :group-list) ()
; Returns a list of the built groups on the workspace.
  (append (send *initial-string* :group-list)
	  (send *target-string* :group-list)))

;---------------------------------------------

(defmethod (workspace :proposed-correspondence-list) ()
; Returns a list of the proposed-correspondences on the workspace.
  (flatten (array-to-list proposed-correspondence-array)))

;---------------------------------------------

(defmethod (workspace :correspondence-list) ()
; Returns a list of the built correspondences on the workspace.
  (flatten (vector-to-list correspondence-vector)))

;---------------------------------------------

(defmethod (workspace :add-replacement) (r)
; Adds a replacement to the workspace's list of replacements.
  (push r replacement-list))

;---------------------------------------------

(defmethod (workspace :add-proposed-correspondence) (c)
; Adds a proposed-correspondence to the workspace's array of
; proposed-correspondences, using the string-numbers of the two
; objects as indices.
  (aset (send self :proposed-correspondence-array) 
             (send (send c :obj1) :string-number)
             (send (send c :obj2) :string-number)
	     (cons c (aref (send self :proposed-correspondence-array) 
		                (send (send c :obj1) :string-number)
		                (send (send c :obj2) :string-number)))))

;---------------------------------------------

(defmethod (workspace :delete-proposed-correspondence) (c)
; Deletes a proposed-correspondence from the workspace's array of
; proposed-correspondences.
  (aset (send self :proposed-correspondence-array) 
             (send (send c :obj1) :string-number)
             (send (send c :obj2) :string-number)
	     (remove c (aref (send self :proposed-correspondence-array) 
		                (send (send c :obj1) :string-number)
		                (send (send c :obj2) :string-number)))))

;---------------------------------------------

(defmethod (workspace :add-correspondence) (c)
; Adds a correspondence to the workspace's vector of
; built correspondences, using the string-number of the initial-string
; object as an index.  Each object can have at most one built 
; correspondence (though more than one proposed correspondences) so it is not 
; necessary to store the built correspondences in an array.
  (vset (send self :correspondence-vector) 
	(send (send c :obj1) :string-number) c))

;---------------------------------------------

(defmethod (workspace :delete-correspondence) (c)
; Deletes a correspondence from the workspace's vector of
; built correspondences.
  (vset (send self :correspondence-vector) 
	(send (send c :obj1) :string-number) nil))

;---------------------------------------------

(defmethod (workspace :correspondence-present?) 
                      (c &aux existing-correspondence)
; Returns t if the given correspondence exists on the workspace.
  (if* (not (null (send (send c :obj1) :correspondence)))
   then (setq existing-correspondence (send (send c :obj1) :correspondence))
        (if* (eq (send existing-correspondence :obj2) (send c :obj2))
	 then existing-correspondence
	 else nil)
   else nil))

;---------------------------------------------

(defmethod (workspace :slippage-present?) (s)
; Returns t if the given slippage exists on the workspace.
  (loop for slippage in (send self :slippage-list)
	when (and (eq (send s :descriptor1) (send slippage :descriptor1))
		  (eq (send s :descriptor2) (send slippage :descriptor2)))
	return t
	finally (return nil)))

;---------------------------------------------

(defmethod (workspace :object-list) ()
; Returns a list of all the objects (letters and groups) on the 
; workspace.
  (append (send *initial-string* :object-list)
	  (send *target-string* :object-list)))

;---------------------------------------------

(defmethod (workspace :letter-list) ()
; Returns a list of all the letters on the workspace.
  (append (send *initial-string* :letter-list)
	  (send *target-string* :letter-list)))

;---------------------------------------------

(defmethod (workspace :structure-list) ()
; Returns a list of all the structures on the workspace.
  (append (send self :bond-list)
          (send self :group-list)
	  (send self :correspondence-list)
	  (if* *rule* then (list *rule*) else nil)))

;---------------------------------------------

(defmethod (workspace :structure-in-snag-structure-list?) (s)
; This method is used after a snag has been hit and the temperature has
; been clamped to determine whether or not to release the temperature
; clamp. This method is called from the function "update-everything"
; in the file "run.l".  Returns t if the given structure is in the list of 
; structures that were present when the last snag was hit.  (If this method 
; returns nil, that is, this structure was built since the snag was hit,
; then there is some chance that the temperature clamp will be released.)
  (cond ((typep s 'bond) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'bond) do
	       (if* (and (eq (send structure :from-obj) (send s :from-obj))
			 (eq (send structure :to-obj) (send s :to-obj))
			 (eq (send structure :bond-category)
			     (send s :bond-category))
			 (eq (send structure :direction-category)
			     (send s :direction-category)))
                then (return t))))


	((typep s 'group) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'group) do
	       (if* (and (eq (send structure :left-obj) (send s :left-obj))
			 (eq (send structure :right-obj) (send s :right-obj))
			 (eq (send structure :group-category)
			     (send s :group-category))
			 (eq (send structure :direction-category)
			     (send s :direction-category)))
                then (return t))))

        ((typep s 'correspondence) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'correspondence) do
	       (if* (and (eq (send structure :obj1) (send s :obj1))
			 (eq (send structure :obj2) (send s :obj2))
			 (>= (length (send structure 
					   :relevant-distinguishing-cms))
			     (length (send s :relevant-distinguishing-cms))))
                then (return t))))
	       

        ((typep s 'rule) 
	 (loop for structure in *snag-structure-list*
	       when (typep structure 'rule)
	       return (rule-equal? structure s)))))
	
;---------------------------------------------

(defmethod (workspace :random-string) ()
; Returns either the initial-string or the target string, chosen at random.
  (random-list-item (list *initial-string* *target-string*)))

;---------------------------------------------

(defmethod (workspace :random-object) ()
; Returns a random object on the workspace.
  (random-list-item (send self :object-list)))
	  
;---------------------------------------------

(defmethod (workspace :random-group) ()
; Returns a random group on the workspace.
  (random-list-item (send self :group-list)))

;---------------------------------------------

(defmethod (workspace :random-correspondence) ()
; Returns a random correspondence on the workspace.
  (random-list-item (send self :correspondence-list)))

;---------------------------------------------

(defmethod (workspace :choose-object) (method &aux value-list)
; Returns an object on the workspace chosen probabilistically 
; (adjusted for temperature) according to the given method.   
  (setq value-list (send-method-to-list (send self :object-list) method))
  (nth (select-list-position (get-temperature-adjusted-value-list value-list))
       (send self :object-list)))

;---------------------------------------------

(defmethod (workspace :null-replacement?) ()
; Returns t if there is at least one letter in the initial string
; that doesn't yet have a replacement.
  (loop for letter in (send *initial-string* :letter-list)
	when (null (send letter :replacement)) return t
	finally (return nil)))

;---------------------------------------------

(defmethod (workspace :unrelated-objects) (&aux num-of-bonds result)    
; Returns a list of all the objects on the workspace that have at least one 
; bond-slot open. Leftmost and rightmost objects have one bond-slot, 
; and other objects have two bond-slots (one on the left and one on the 
; right).
  (loop for object in (send self :object-list) do
        (if* (and (not (send object :spans-whole-string?)) 
		  (null (send object :group)))
         then (setq num-of-bonds 
	            (length (append (send object :incoming-bonds) 
		                    (send object :outgoing-bonds))))
              (if* (or (send object :leftmost-in-string?)
	               (send object :rightmost-in-string?))
               then (if* (= num-of-bonds 0) then (push object result))
    	       else (if* (< num-of-bonds 2) then (push object result)))))
  result)
;---------------------------------------------

(defmethod (workspace :ungrouped-objects) ()    
; Returns a list of all the objects on the workspace that are not in a group.
  (loop for object in (send self :object-list)
	when (and (not (send object :spans-whole-string?))
		  (null (send object :group))) collect object))

;---------------------------------------------

(defmethod (workspace :ungrouped-bonds) ()    
; Returns a list of all the bonds on the workspace that are not in groups.
; A bond which is not in a group but both of whose objects are in groups
; is considered to be grouped.
  (loop for bond in (send self :bond-list)
	when (or (null (send (send bond :from-obj) :group))
		 (null (send (send bond :to-obj) :group)))
        collect bond))

;---------------------------------------------

(defmethod (workspace :unreplaced-objects) ()    
; Returns a list of all the objects on the initial-string that 
; don't have a replacement.
  (loop for letter in (send *initial-string* :letter-list)
	when (null (send letter :replacement)) collect letter))

;---------------------------------------------

(defmethod (workspace :uncorresponding-objects) ()    
; Returns a list of all the objects on the workspace that 
; don't have a correspondence.
  (loop for object in (send self :object-list)
	when (null (send object :correspondence)) collect object))

;---------------------------------------------

(defmethod (workspace :rough-num-of-unrelated-objects) (&aux n)
; Returns either 'few, 'medium, or 'many.  This method is used for
; "self-watching" in the function "get-num-of-codelets-to-post" in the file
; "formulas.l" -- for deciding the number of bond-scout codelets that
; should be posted.
  (setq n (length (send self :unrelated-objects)))
  (cond ((< n (blur 2)) 'few)
	((< n (blur 4)) 'medium)
	(t 'many)))
	
;---------------------------------------------

(defmethod (workspace :rough-num-of-ungrouped-objects) (&aux n)
; Returns either 'few, 'medium, or 'many.  This method is used for
; "self-watching" in the function "get-num-of-codelets-to-post" in the file
; "formulas.l" -- for deciding the number of group-scout codelets that
; should be posted.
  (setq n (length (send self :ungrouped-objects)))
  (cond ((< n (blur 2)) 'few)
	((< n (blur 4)) 'medium)
	(t 'many)))
	
;---------------------------------------------

(defmethod (workspace :rough-num-of-unreplaced-objects) (&aux n)
; Returns either 'few, 'medium, or 'many.  This method is used for
; "self-watching" in the function "get-num-of-codelets-to-post" in the file
; "formulas.l" -- for deciding the number of replacement-building codelets that
; should be posted.
  (setq n (length (send self :unreplaced-objects)))
  (cond ((< n (blur 2)) 'few)
	((< n (blur 4)) 'medium)
	(t 'many)))
	
;---------------------------------------------

(defmethod (workspace :rough-num-of-uncorresponding-objects) (&aux n)
; Returns either 'few, 'medium, or 'many.  This method is used for
; "self-watching" in the function "get-num-of-codelets-to-post" in the file
; "formulas.l" -- for deciding the number of correspondence-scout codelets 
; that should be posted.
  (setq n (length (send self :uncorresponding-objects)))
  (cond ((< n (blur 2)) 'few)
	((< n (blur 4)) 'medium)
	(t 'many)))
	
;---------------------------------------------

(defmethod (workspace :rough-importance-of-uncorresponding-objects) 
           (&aux uncorresponding-objects n)
; Returns either 'low, 'medium, or 'high.
  (setq uncorresponding-objects (send self :uncorresponding-objects))
  (if* (null uncorresponding-objects)
   then 'low
   else (setq n (list-max (send-method-to-list uncorresponding-objects
	 		                       :relative-importance)))
        (cond ((< n (blur 20)) 'low)
	      ((< n (blur 40)) 'medium)
	      (t 'high))))
	
;---------------------------------------------

(defmethod (workspace :delete-proposed-structure) (s)
; Deletes the given proposed structure from the workspace.
  (cond ((typep s 'bond) 
	 (send (send s :string) :delete-proposed-bond s))    
	((typep s 'group) (send (send s :string) :delete-proposed-group s))
	((typep s 'correspondence) 
	 (send self :delete-proposed-correspondence s))))

;---------------------------------------------

(defmethod (workspace :slippage-list) ()
; Returns a list of all the slippages in all the correspondences.
  (flatten (send-method-to-list (send self :correspondence-list)
	                        :slippage-list)))
	
;---------------------------------------------

(defmethod (workspace :intra-string-unhappiness) ()
; Returns a weighted average of the intra-string unhappiness of objects
; on the workspace (weighted by each object's relative importance in the 
; string.
  (min 100 (/ (loop for obj in (send self :object-list)
                    sum (* (send obj :relative-importance)
		           (send obj :intra-string-unhappiness))) 200)))

;---------------------------------------------

(defmethod (workspace :inter-string-unhappiness) ()
; Returns a weighted average of the inter-string unhappiness of objects
; on the workspace (weighted by each object's relative importance in the 
; string.
  (min 100 (/ (loop for obj in (send self :object-list)
                    sum (* (send obj :relative-importance)
		           (send obj :inter-string-unhappiness))) 200)))

;---------------------------------------------

(defmethod (workspace :total-unhappiness) ()
; Returns a weighted average of the total unhappiness of objects
; on the workspace, weighted by each object's relative importance in the 
; string.
  (min 100 (/ (loop for obj in (send self :object-list)
                    sum (* (send obj :relative-importance)
		           (send obj :total-unhappiness))) 200)))

;---------------------------------------------

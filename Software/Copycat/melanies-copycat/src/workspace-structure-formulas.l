;---------------------------------------------
; WORKSPACE-STRUCTURE-FORMULAS:  Formulas for workspace structures.

;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defmethod (workspace-structure :update-strength-values) ()
; Updates the strength values for the structure.
  (send self :set-internal-strength (send self :calculate-internal-strength))
  (send self :set-external-strength (send self :calculate-external-strength))
  (send self :set-total-strength (send self :calculate-total-strength)))

;---------------------------------------------

(defmethod (description :calculate-internal-strength) ()
  (send descriptor :conceptual-depth))

;---------------------------------------------

(defmethod (description :calculate-external-strength) ()
  (average (send self :local-support)
           (send description-type :activation)))

;---------------------------------------------

(defmethod (bond :calculate-internal-strength) 
           (&aux member-compatibility-factor bond-facet-factor)
  ; Bonds between objects of the same type (letter-letter or 
  ; group-group) are stronger than bonds between different types 
  ; (letter-group or group-letter).					
  (if* (eq (flavor-type from-obj) (flavor-type to-obj))
   then (setq member-compatibility-factor 1)
   else (setq member-compatibility-factor .7))

  ; For now, letter-category bonds are stronger than other types of
  ; bonds (namely, length bonds).  This should be fixed; a 
  ; more general mechanism is needed.	   
  (if* (eq bond-facet plato-letter-category)
   then (setq bond-facet-factor 1)
   else (setq bond-facet-factor .7)) 

  (min 100 (round (* member-compatibility-factor bond-facet-factor
			   (send bond-category 
				 :bond-degree-of-association)))))

;---------------------------------------------

(defmethod (bond :calculate-external-strength) ()
  (send self :local-support))
 
;---------------------------------------------

(defmethod (group :calculate-internal-strength) 
           (&aux bond-facet-factor bond-component 
		 bond-component-weight 
		 length-component length-component-weight)

  ; For now, groups based on letter-category are stronger than groups based
  ; on other facets (namely, length). This should be fixed; a more 
  ; general mechanism is needed.	   
  (if* (eq bond-facet plato-letter-category)
         then (setq bond-facet-factor 1)
         else (setq bond-facet-factor .5))

  (setq bond-component 
	(* (send (send group-category :get-related-node 
		       plato-bond-category) :degree-of-association) 
	   bond-facet-factor))
  
  (setq length-component 
        (cond ((= (send self :length) 1) 5)
	      ((= (send self :length) 2) 20)
	      ((= (send self :length) 3) 60)
	      (t 90)))

  (setq bond-component-weight (expt bond-component .98))
  (setq length-component-weight (fake-reciprocal bond-component-weight))
	      
  (weighted-average `((,bond-component . ,bond-component-weight)
                      (,length-component . ,length-component-weight))))
    
;---------------------------------------------

(defmethod (group :calculate-external-strength)  ()
  (if* (send self :spans-whole-string?) 
   then 100 else (send self :local-support)))

;---------------------------------------------

(defmethod (correspondence :calculate-internal-strength) 
		(&aux relevant-distinguishing-cms 
		     average-strength internal-coherence-factor
		     num-of-concept-mappings-factor num-of-concept-mappings)
; A function of how many concept-mappings there are, how strong they are,
; and how much internal coherence there is among concept mappings.
  (setq relevant-distinguishing-cms
	(send self :relevant-distinguishing-cms))

  (if* (null relevant-distinguishing-cms)
   then 0
   else (setq average-strength
	      (list-average 
		  (send-method-to-list 
		      relevant-distinguishing-cms :strength)))

        (setq num-of-concept-mappings (length relevant-distinguishing-cms))
        (setq num-of-concept-mappings-factor
	      (case num-of-concept-mappings
		    (1 .8)  
		    (2 1.2)
		    (t 1.6)))

        (if* (send self :internally-coherent?)
         then (setq internal-coherence-factor 2.5)
         else (setq internal-coherence-factor 1))

        (min 100 (round (* average-strength internal-coherence-factor
			   num-of-concept-mappings-factor)))))

;---------------------------------------------

(defmethod (correspondence :calculate-external-strength) ()
  (send self :support))

;---------------------------------------------

(defmethod (correspondence :internally-coherent?) 
           (&aux cm-list result)
; For now this returns t if there is any pair of relevant-distinguishing 
; concept-mappings that support each other.  This isn't quite right.
  (setq cm-list
	(send self :relevant-distinguishing-cms))
  (if* (> (length cm-list) 1)
   then (loop for cm in cm-list until result do
	      (loop for other-cm in (remove cm cm-list) 
	            when (supporting-concept-mappings? cm other-cm)
	            return (setq result t))))
  result)
	      
;---------------------------------------------

(defmethod (rule :calculate-internal-strength) 
           (&aux conceptual-depth1 conceptual-depth2 conceptual-depth-difference
		 shared-descriptor-term shared-descriptor-weight
                 i-obj i-obj-corresponding-object slipped-descriptors
		 rule-strength)
(block nil
  (if* (send self :no-change?)
   then (return 100))
        
  (setq conceptual-depth1 (send descriptor1 :conceptual-depth))
  (setq conceptual-depth2 (if* (send self :relation?) 
		      then (send relation :conceptual-depth)
  	              else (send descriptor2 :conceptual-depth)))

  ; There should be pressure for descriptor1 and the relation or descriptor2 
  ; to have the same level of conceptual-depth
  (setq conceptual-depth-difference (abs (- conceptual-depth1 conceptual-depth2)))

  ; Now see if descriptor1 is shared (perhaps modulo slippage) with the 
  ; corresponding object, if any.
  (setq i-obj (loop for obj in (send *initial-string* :object-list)
	            when (send obj :changed?) return obj))

  (setq i-obj-corresponding-object 
	(if* (send i-obj :correspondence)
	 then (send (send i-obj :correspondence) :obj2) else nil))

  (if* (null i-obj-corresponding-object) 
   then (setq shared-descriptor-term 0)
   else (setq slipped-descriptors
	      (loop for d in (send i-obj-corresponding-object 
			           :relevant-descriptions)
		    collect (send (send d :apply-slippages 
					i-obj-corresponding-object
					(send *workspace* :slippage-list))
				  :descriptor)))

        (if* (memq descriptor1 slipped-descriptors)
         then (setq shared-descriptor-term 100)
         else (return 0)))  ; Can't make this rule.

  ; The less general descriptor1 is, the more we care if it's shared.
  (setq shared-descriptor-weight 
	(round (expt (/ (fake-reciprocal (send descriptor1 :conceptual-depth)) 10)
		     1.4)))

  (setq rule-strength 
	(round (weighted-average 
 	         `((,(expt (average conceptual-depth1 conceptual-depth2) 1.1) . 18)
		   (,(fake-reciprocal conceptual-depth-difference) . 12)
		   (,shared-descriptor-term . ,shared-descriptor-weight)))))
  (min rule-strength 100)))

;---------------------------------------------

(defmethod (rule :calculate-external-strength) ()
  (send self :internal-strength))

;---------------------------------------------

(defmethod (workspace-structure :calculate-total-strength) 
           (&aux internal-strength external-strength 
		 internal-strength-weight external-strength-weight)
  (setq internal-strength (send self :internal-strength))    
  (setq external-strength (send self :external-strength))
  (setq internal-strength-weight internal-strength)
  (setq external-strength-weight (fake-reciprocal internal-strength-weight))
  (weighted-average `((,internal-strength . ,internal-strength-weight)
	              (,external-strength . ,external-strength-weight))))

;---------------------------------------------

(defmethod (workspace-structure :total-weakness) ()
; This method is used by breaker codelets.  The .95 exponent is so that even 
; structures with a strength of 100 have some chance of being broken.
  (fake-reciprocal (expt (send self :total-strength) .95)))

;---------------------------------------------

(defmethod (concept-mapping :slippability) (&aux degree-of-association)
; This returns a value representing the ease with which this slippage can be
; made.  Conceptual-Depth gives a resistance to slippage.
  (setq degree-of-association (send self :degree-of-association))
  (if* (= degree-of-association 100) 
   then 100
   else (* degree-of-association
	   (- 1 (sqr (/ (send self :conceptual-depth) 100))))))

;---------------------------------------------

(defmethod (concept-mapping :strength) (&aux degree-of-association)
; This returns a value representing the strength of the concept-mapping.
; The closer and the more general the nodes, the stronger.  
  (setq degree-of-association (send self :degree-of-association))
  (if* (= degree-of-association 100) 
   then 100
   else (* degree-of-association
	   (+ 1 (sqr (/ (send self :conceptual-depth) 100))))))

;---------------------------------------------

(defun supporting-correspondences? (c1 c2 &aux (result nil))
; Returns t if c1 supports c2, nil otherwise.  For now, c1 is 
; defined to support c2 if c1 is not incompatible with c2, and 
; has a concept-mapping that supports the concept-mappings of c2.

  (cond ((or (eq (send c1 :obj1) (send c2 :obj1))	
	     (eq (send c1 :obj2) (send c2 :obj2)))
         (setq result nil))
	((incompatible-correspondences? c1 c2) (setq result nil))
        (t (loop for cm1 in (send c1 :distinguishing-concept-mappings) 
		 until result do
                  (loop for cm2 in (send c2 :distinguishing-concept-mappings)
		        when (supporting-concept-mappings? cm1 cm2) 
			do (setq result t)
			   (return)))))
  result)

;---------------------------------------------

(defun incompatible-correspondences? (c1 c2 &aux (result nil))
; Returns t if c1 is incompatible with c2, nil otherwise.  For now, c1 is 
; defined to be incompatible with c2 if c1 and c2 share objects, or c1 has a 
; concept-mapping incompatible with the concept-mappings of c2.


  (if* (or (eq (send c1 :obj1) (send c2 :obj1))	
	  (eq (send c1 :obj2) (send c2 :obj2)))
   then (setq result t)
   else (loop for cm1 in (send c1 :concept-mapping-list) until result do
              (loop for cm2 in (send c2 :concept-mapping-list) 
		    when (incompatible-concept-mappings? cm1 cm2) do
                         (setq result t)
			 (return))))
  result)

;---------------------------------------------

(defun supporting-concept-mappings? (cm1 cm2)
; Concept-mappings (a -> b) and (c -> d) support each other if a is related
; to c and if b is related to d and the a -> b relationship is the same as the
; c -> d relationship.  E.g., rightmost -> rightmost supports right -> right 
; and leftmost -> leftmost.  Notice that slipnet distances are not looked 
; at, only slipnet links.  This should be changed eventually.

  ; If the two concept-mappings are the same, then return t.  This
  ; means that letter->group supports letter->group, even though these
  ; concept-mappings have no label.
  (cond ((and (eq (send cm1 :descriptor1) (send cm2 :descriptor1))
	      (eq (send cm1 :descriptor2) (send cm2 :descriptor2))) 
	 t)
        ; If the descriptors are not related, then return nil.
        ((not (or (related? (send cm1 :descriptor1) (send cm2 :descriptor1))
 	          (related? (send cm1 :descriptor2) (send cm2 :descriptor2))))
         nil)
        ; If one of the concept-mappings has no label, then return nil.
        ((or (null (send cm1 :label)) (null (send cm2 :label)))
         nil)
        ((eq (send cm1 :label) (send cm2 :label))
         t)
        (t nil)))

;---------------------------------------------

(defun incompatible-concept-mappings? (cm1 cm2)
; Concept-mappings (a -> b) and (c -> d) are incompatible if a is 
; related to c or if b is related to d, and the a -> b relationship is 
; different from the c -> d relationship. E.g., rightmost -> leftmost
; is incompatible with right -> right, since rightmost is linked 
; to right, but the relationships (opposite and identity) are different.  
; Notice that slipnet distances are not looked at, only slipnet links. This 
; should be changed eventually.
  (if* (not (or (related? (send cm1 :descriptor1) (send cm2 :descriptor1))
 	        (related? (send cm1 :descriptor2) (send cm2 :descriptor2))))
   then nil
   else (if* (or (null (send cm1 :label)) (null (send cm2 :label)))
         then nil
         else (if* (not (eq (send cm1 :label) (send cm2 :label)))
               then t
	       else nil))))

;---------------------------------------------

(defun incompatible-concept-mapping-lists? (l1 l2 &aux incompatible?)
; Returns t if the two lists of concept-mappings contain incompatible
; concept-mappings.
  (loop for cm1 in l1
        until incompatible? do
        (loop for cm2 in l2
 	      when (incompatible-concept-mappings? cm1 cm2) do
	           (setq incompatible? t)
	           (return)))
  incompatible?)

;---------------------------------------------

(defun related? (node1 node2)
; Returns t if the two nodes are equal or are linked in the slipnet.
  (if* (eq node1 node2) then t else (linked? node1 node2)))

;---------------------------------------------

(defun linked? (node1 node2)
; Returns t if the two nodes are linked in the slipnet.
  (memq node2 (send-method-to-list (send node1 :outgoing-links) :to-node)))

;---------------------------------------------

(defun slip-linked? (node1 node2)
; Returns t if the two nodes are linked by a slip-link in the slipnet.
  (memq node2 (send-method-to-list (send node1 :lateral-slip-links) :to-node)))

;---------------------------------------------

(defmethod (slipnode :bond-degree-of-association) ()
; Returns the degree of association bonds of the given category are 
; considered to have.
  (min 100 (round (* 11 (sqrt (send self :degree-of-association))))))

;---------------------------------------------

(defmethod (bond :importance) ()
; Sameness bonds are more important that other bonds of other 
; categories (for now, the only other categories are predecessor and 
; successor).
  (if* (eq bond-category plato-sameness) 
   then 100 else 50))

;---------------------------------------------

(defmethod (bond :happiness) ()
  (if* group then (send group :total-strength) else 0))

;---------------------------------------------

(defmethod (bond :unhappiness) ()
  (fake-reciprocal (send self :happiness)))

;---------------------------------------------

(defmethod (bond :salience) ()
  (round (average (send self :importance) (send self :unhappiness))))

;---------------------------------------------

(defmethod (slipnode :local-descriptor-support) 
           (string object-category &aux object-list)
; The percentage of objects of the given category in the string that have 
; this descriptor.
  (if* (eq object-category plato-letter)
   then (setq object-list (send string :letter-list))
   else (setq object-list (send string :group-list)))
  (if* (null object-list) 
   then 0
   else (loop for obj in object-list
	      when (send obj :descriptor-present? self)
	      count t into descriptor-count
	      finally (return (round (* 100 (/ descriptor-count 
					       (length object-list))))))))
  
;---------------------------------------------

(defmethod (slipnode :local-description-type-support) (string)
; The percentage of objects in the string that have descriptions with this 
; description-type.
  (loop for obj in (send string :object-list)
        when (send obj :description-type-present? self)
        count t into description-type-count
	finally (return (round (* 100 (/ description-type-count 
				      (length (send string :object-list))))))))
  
;---------------------------------------------

(defmethod (slipnode :total-description-type-support) (string)
; A function of the local-description-type-support and the node's activation.
  (round (average (send self :local-description-type-support string) activation)))

;---------------------------------------------

(defmethod (description :local-support) (&aux num-of-supporting-objects)
; Returns the support for this description in its
; string.  This is a rough (not perfect) version.  Looks at all the other 
; objects in the string, getting support from objects with a description
; of the given object facet.  This doesn't take into account distance; all
; qualifying objects in the string give the same amount of support.
  (setq num-of-supporting-objects
	(loop for other-object 
	      in (remove object (send string :object-list)) 
   	      when (and (not (or (recursive-group-member? 
				     other-object object)
			         (recursive-group-member? 
				     object other-object)))
		        (memq description-type 
			      (send-method-to-list 
				  (send other-object :descriptions)
				  :description-type)))
  	      count t into supporting-object-count
	      finally (return supporting-object-count)))

  (case num-of-supporting-objects
        (0 0)
	(1 20)
	(2 60)
	(3 90)
	(otherwise 100)))

;---------------------------------------------

(defmethod (bond :number-of-local-supporting-bonds) 
           (&aux num-of-supporting-bonds)
; Returns the number of supporting bonds in the given bond's string.
; Looks at all the other bonds in the string, counting bonds of the 
; same bond-category and direction-category.  Doesn't take distance into 
; account; all qualifying bonds in the string are counted the same.
  (setq num-of-supporting-bonds 
	(loop for other-bond in (remove self (send string :bond-list)) 
	      when (and (not (= (letter-distance 
				    (send self :left-obj)
		                    (send other-bond :left-obj)) 0))
                        (not (= (letter-distance 
				    (send self :right-obj)
			            (send other-bond :right-obj)) 0))
		        (eq (send other-bond :bond-category) 
			    bond-category)
		        (eq (send other-bond :direction-category) 
			    direction-category))
	      count t into supporting-bond-count
	      finally (return supporting-bond-count)))

  num-of-supporting-bonds)

;---------------------------------------------

(defmethod (bond :local-density)
           (&aux next-obj last-obj next-bond 
		 (slot-sum 0) (support-sum 0))

; Returns a rough measure of the density in the string of bonds of 
; the same bond-category and direction-category as the given bond.  
; This method is used in calculating the external strength of a bond.
; I don't think this method is quite right.  The result depends on which
; right and left neighbors are chosen, which is probabilistic, so it doesn't
; always give the same value.

  ; First loop though left-neighbors.
  (setq last-obj left-obj)
  (setq next-obj (send left-obj :choose-left-neighbor))
  (loop until (null next-obj) do
	(incf slot-sum) ; Add 1 to the number of possible bond slots looked at.
        ; Look at the bond between these two objects.
	(setq next-bond (aref (send string :left-right-bond-array)
			      (send next-obj :string-number)
			      (send last-obj :string-number)))

        (if* (and next-bond 
		  (eq (send next-bond :bond-category) 
		      bond-category)
                  (eq (send next-bond :direction-category) 
		      direction-category))
                 then (incf support-sum))
  	(setq last-obj next-obj)
	(setq next-obj (send next-obj :choose-left-neighbor)))

    
  ; Now loop though right-neighbors.
  (setq last-obj right-obj)
  (setq next-obj (send right-obj :choose-right-neighbor))
  (loop until (null next-obj) do
	(incf slot-sum) ; Add 1 to the number of possible bond slots looked at.
        ; Look at the bond between these two objects.
	(setq next-bond (aref (send string :left-right-bond-array)
			      (send last-obj :string-number)
			      (send next-obj :string-number)))

        (if* (and next-bond
		  (eq (send next-bond :bond-category) 
		      bond-category)
                  (eq (send next-bond :direction-category) 
		      direction-category))
         then (incf support-sum))
	(setq last-obj next-obj)
	(setq next-obj (send next-obj :choose-right-neighbor)))
    
  (if* (= slot-sum 0) 
   then 100 else (round (* 100 (/ support-sum slot-sum)))))
  
;---------------------------------------------

(defmethod (bond :local-support) (&aux number density adjusted-density
					   number-factor)
  (setq number (send self :number-of-local-supporting-bonds))    
  (if* (= number 0)
   then 0
   else (setq density (send self :local-density))
        (setq adjusted-density (* 100 (sqrt (/ density 100))))
        (setq number-factor (min 1 (expt .6 (/ 1 (cube number)))))
        (round (* adjusted-density number-factor))))
	   

;---------------------------------------------

(defmethod (group :number-of-local-supporting-groups) 
           (&aux num-of-supporting-groups)
; Returns the number of supporting groups in the given group's string.
; Looks at all the other groups in the string, counting groups of the 
; same grouup-category and direction-category.  Doesn't take distance into 
; account; all qualifying groups in the string are counted the same.
  (setq num-of-supporting-groups
	(loop for other-group in (remove self (send string :group-list)) 
	      when (and (not (or (subgroup? self other-group)
				 (subgroup? other-group self)
			         (groups-overlap? self other-group)))
  		        (eq (send other-group :group-category) group-category)
		        (eq (send other-group :direction-category) 
			    direction-category))
	      count t into supporting-group-count
	      finally (return supporting-group-count)))

  num-of-supporting-groups)

;---------------------------------------------

(defmethod (group :local-density) 
           (&aux next-obj next-group  
		 (slot-sum 0) (support-sum 0))

; Returns a rough measure of the density in the string of groups of the same 
; group-category and direction-category as the given group.
; This method is used in calculating the external strength of a group.
; I don't think this method is quite right.  The result depends on which
; right and left neighbors are chosen, which is probabilistic, so it doesn't
; always give the same value.
(block nil
  (if* (send self :string-spanning-group?)
   then (return 100))
  
  ; First loop though left-neighbors.
  (setq next-obj (send left-obj :choose-left-neighbor))
  ; If the next object is a letter in a group, then set the next object
  ; to the the group.  I'm not sure that this is the right 
  ; way to do all this; it might need to be fixed.
  (if* (and (typep next-obj 'letter) (send next-obj :group))
   then (setq next-obj (send next-obj :group)))
  (loop until (null next-obj) do
        ; Look at next-obj's group.  Count the next-group only if it doesn't 
	; overlap the original group.  
	(setq next-group (if* (typep next-obj 'letter) then nil else next-obj))
        (incf slot-sum) ; Add 1 to the number of possible group slots looked 
	                ; at.
        (if* (and next-group
                  ; Don't count the group if it overlaps this group.
	          (not (groups-overlap? self next-group))
                  (eq (send next-group :group-category) group-category)
                  (eq (send next-group :direction-category) 
		      direction-category))
	 then (incf support-sum))
	(setq next-obj (send next-obj :choose-left-neighbor)))

  ; Now loop though right-neighbors.
  (setq next-obj (send right-obj :choose-right-neighbor))
  ; If the next object is a letter in a group, then set the next object
  ; to the group.  I'm not sure that this is the right 
  ; way to do all this; it might need to be fixed.
  (if* (and (typep next-obj 'letter) (send next-obj :group))
   then (setq next-obj (send next-obj :group)))
  (loop until (null next-obj) do
        ; Look at next-obj's group.  Count the next-group only if it doesn't 
	; overlap the original group.  
	(setq next-group (if* (typep next-obj 'letter) then nil else next-obj))
        (incf slot-sum) ; Add 1 to the number of possible group slots looked 
	                ; at.
        ; Support-sum gets full weight for same type of group, 0 
        ; weight for null group, and 0 for different type of group.
        (if* (and next-group
                  ; Don't count the group if it overlaps this group.
	          (not (groups-overlap? self next-group))
                  (eq (send next-group :group-category) group-category)
                  (eq (send next-group :direction-category) 
		      direction-category))
	 then (incf support-sum))
	(setq next-obj (send next-obj :choose-right-neighbor)))

  (if* (= slot-sum 0) 
   then 100 else (round (* 100 (/ support-sum slot-sum))))))

;---------------------------------------------

(defmethod (group :local-support) (&aux number density adjusted-density
					   number-factor)
  (setq number (send self :number-of-local-supporting-groups))    
  (if* (= number 0)
   then 0
   else (setq density (send self :local-density))
        (setq adjusted-density (* 100 (sqrt (/ density 100))))
        (setq number-factor (min 1 (expt .6 (/ 1 (cube number)))))
        (round (* adjusted-density number-factor))))
	   
;---------------------------------------------

(defmethod (correspondence :support) (&aux support-sum other-correspondences)
; For now there are three levels of compatibility: 
; supporting, not incompatible but not supporting, and incompatible.
; This returns the sum of the strengths of other correspondences that
; support this one (or 100, whichever is lower).  If one of the objects is the 
; single letter in its string, then the support is 100.
  (if* (or (and (typep obj1 'letter) (send obj1 :spans-whole-string?))
	   (and (typep obj2 'letter) (send obj2 :spans-whole-string?)))
   then 100 
   else (setq support-sum 0) ; What the correspondence gets in the absence of
                             ; compatible or incompatible correspondences.
        (setq other-correspondences 
	      (remove self (send *workspace* :correspondence-list)))
        (loop for c in other-correspondences do
              (if* (supporting-correspondences? self c)
               then (incf support-sum (send c :total-strength))))
        (min 100 support-sum)))
         
;---------------------------------------------

(defmethod (workspace-string :local-bond-category-relevance)
           (given-bond-category &aux object-list)

; A function of how many bonds in the string have the given 
; bond-category.  This function is not perfect; it gives just a rough 
; estimate of the relevance of this bond category.
; This method is used in the top-down-bond-scout--category codelet and the
; top-down-group-scout--category codelet as a way of probabilistically choosing
; a string to work in.
  (setq object-list (send self :non-string-spanning-object-list))
  (if* (= (length object-list) 1) 
   then 0
   else (loop for obj in object-list
              when (and (send obj :right-bond)
	                (eq (send (send obj :right-bond) 
				  :bond-category) 
		            given-bond-category))
              count t into bond-count
              finally 
	      (return (* 100 (/ bond-count (1- (length object-list))))))))

;---------------------------------------------

(defmethod (workspace-string :local-direction-category-relevance)
           (given-direction-category &aux object-list)

; A function of how many bonds in the string have the given 
; direction-category.  This function is not perfect; it gives just a rough 
; estimate of the relevance of this direction category.
; This method is used in the top-down-bond-scout--direction codelet and the
; top-down-group-scout--direction codelet as a way of probabilistically 
; choosing a string to work in.

  (setq object-list (send self :non-string-spanning-object-list))
  (if* (= (length object-list) 1) 
   then 0
   else (loop for obj in object-list
              when (and (send obj :right-bond)
	                (eq (send (send obj :right-bond) 
				  :direction-category) 
		            given-direction-category))
              count t into bond-count
              finally 
	      (return (* 100 (/ bond-count (1- (length object-list))))))))

;---------------------------------------------

(defun recursive-group-member? (object group)
; Returns t if the object is a member of the group, or is a member of a 
; member, etc.
  (cond ((typep group 'letter) nil)
	((memq object (send group :object-list)) t)
	(t (loop for g in (send group :object-list)
		 when (recursive-group-member? object g)
		 return t
		 finally (return nil)))))

;---------------------------------------------


;---------------------------------------------
; RULE: This file contains flavors, methods, and codelets for the rule and
;       translated rule.
;---------------------------------------------

(in-package 'user)

(defflavor rule
  (object-category1 descriptor1-facet descriptor1 
  (object-category2 nil) (descriptor2 nil)
  (replaced-description-type nil) (relation nil))
  (workspace-structure)
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

; Here are two examples of how the rule instance can be set up:

; Example 1: for the rule "Replace rightmost letter by successor": 
; OBJECT-CATEGORY1 = plato-letter 
; (The object-category of the initial-string object that changed.)
; DESCRIPTOR1 = "rightmost".
; DESCRIPTOR1-FACET = plato-string-position-category 
; (This is the facet of the letter that's being described by descriptor1 
; in the rule, not its letter-category or its length or anything 
; else.)
; REPLACED-DESCRIPTION-TYPE = letter-category 
; (This  means that the rule is saying that "successor" refers to 
; letter-category, not to any other facet of the two letters being 
; related.)
; RELATION = plato-successor.  
; (Since this is a "relation-rule", the other instance variables are 
; ignored.)

; Example 2: for the rule "Replace C by D":
; OBJECT-CATEGORY1 = plato-letter
; DESCRIPTOR1-FACET = plato-letter-category
; DESCRIPTOR1 = plato-c
; OBJECT-CATEGORY2 = plato-letter
; REPLACED-DESCRIPTION-TYPE = plato-letter-category
; DESCRIPTOR2 = plato-d

;---------------------------------------------

(defun make-relation-rule (object-category1 descriptor1-facet descriptor1 
		           object-category2 replaced-description-type relation)
; Returns a new relation rule.
  (make-instance 'rule :object-category1 object-category1
                       :descriptor1-facet descriptor1-facet 
                       :descriptor1 descriptor1 
                       :object-category2 object-category2
                       :replaced-description-type replaced-description-type
                       :relation relation
		       :structure-category 'rule))

;---------------------------------------------

(defun make-non-relation-rule (object-category1 descriptor1-facet descriptor1 
 		               object-category2 replaced-description-type 
			       descriptor2)
; Returns a new non-relation rule.
  (make-instance 'rule :object-category1 object-category1
                       :descriptor1-facet descriptor1-facet 
                       :descriptor1 descriptor1 
                       :object-category2 object-category2
                       :replaced-description-type replaced-description-type
                       :descriptor2 descriptor2
		       :structure-category 'rule))

;---------------------------------------------

(defmethod (rule :relation?) ()
; Returns t if the rule expresses a relation between the modified-string 
; object and the initial-string object.
  relation)

;---------------------------------------------

(defmethod (rule :no-change?) ()
; Returns t if the rule specifies that no changes are to be made.
  (null descriptor1))

;---------------------------------------------

(defmethod (rule :print) ()
  (if* (send self :no-change?)
   then (format t "Don't replace anything~&")
   else (if* (send self :relation?)
         then (format t "Replace ~a with ~a \"~a\" "
		        (send object-category1 :pname)
	                (send descriptor1-facet :pname)
	                (send descriptor1 :pname))
              (format t "by ~a with ~a: ~a of ~a of ~a with ~a: ~a~&"
  	              (send object-category2 :pname)
	              (send replaced-description-type :pname)
	              (send relation :pname)
	              (send replaced-description-type :pname)
	              (send object-category1 :pname)
	              (send descriptor1-facet :pname)
	              (send descriptor1 :pname))
         else (format t "Replace ~a with ~a \"~a\" by ~a with ~a \"~a\"~&"
	   	      (send object-category1 :pname)
		      (send descriptor1-facet :pname)
		      (send descriptor1 :pname)
		      (send object-category2 :pname)
	              (send replaced-description-type :pname)
		      (send descriptor2 :pname)))))

;---------------------------------------------

(defun rule-equal? (r1 r2)
; Returns t if the two rules are the same.
  (and (eq (send r1 :object-category1) (send r2 :object-category1))
       (eq (send r1 :descriptor1-facet) (send r2 :descriptor1-facet))
       (eq (send r1 :descriptor1) (send r2 :descriptor1))
       (eq (send r1 :object-category2) (send r2 :object-category2))
       (eq (send r1 :descriptor2) (send r2 :descriptor2))
       (eq (send r1 :replaced-description-type) (send r2 :replaced-description-type))
       (eq (send r1 :relation) (send r2 :relation))))
 
;---------------------------------------------

(defun rule-scout (&aux changed-objects i-obj m-obj 
			i-descriptions i-probabilities i-description 
			correspondence-slippage-list 
			m-descriptions  m-probabilities m-description 
			related-descriptor)
; This codelet fills in the rule template (for the time being, we only
; have one: "Replace _____ by _____"). To do this, it chooses descriptions of
; the changed object in the initial-string and the object in the 
; modified-string that replaces it.  If a rule can be made, 
; then one is proposed, and a rule-strength-tester codelet is posted with 
; urgency a function of the degree of conceptual-depth of the chosen descriptions.

(block nil

  (if* %verbose% then (format t "In rule-scout~&"))

  ; If not all replacements have been found, then fizzle.
  (if* (send *workspace* :null-replacement?)
   then (if* %verbose% 
	 then (format t "Not all replacements have been found.  Fizzling.~&"))
        (return))

  ; Find changed object.
  (setq changed-objects
	(loop for obj in (send *initial-string* :object-list)
	      when (send obj :changed?) collect obj))

  ; If there is more than one changed object, then signal and error, and quit.
  (if* (> (length changed-objects) 1)
   then (format t "~%More than one letter changed.~&")
        (format t "Sorry, I can't solve problems like this right now.~&")
	(setq *quit-program* t)
	(return))

  ; If no changed object, then propose rule specifying no changes.
  (if* (null changed-objects) 
   then (propose-rule nil nil nil nil)
        (return))

  ; Otherwise, go on.
  (setq i-obj (car changed-objects))
  (setq m-obj (send (send i-obj :replacement) :obj2))

  ; Get all relevant distinguishing descriptions that are shared 
  ; (modulo slippage) between the changed object and the target-string object
  ; it corresponds to.  If there is no target-string object corresponding to
  ; the changed object , then all the relevant distinguishing 
  ; descriptions are considered.
  (if* (null (send i-obj :correspondence)) 
   then (setq i-descriptions 
	      (send i-obj :rule-initial-string-descriptions))
   else (setq correspondence-slippage-list 
	      (send (send i-obj :correspondence) :slippage-list))
        (setq i-descriptions 
	      (loop for d 
		    in (send i-obj :rule-initial-string-descriptions) 
                    if (description-member?
			   (send d :apply-slippages 
				   i-obj correspondence-slippage-list)
			   (send (send (send i-obj :correspondence) :obj2) 
				 :relevant-descriptions))
      	               collect d into shared-description-list
		       finally (return shared-description-list))))

  (if* %verbose% 
   then (format t "i-descriptions: ") 
        (loop for d in i-descriptions do (send d :print)) 
        (format t "~%"))
  
  (if* (null i-descriptions)
   then (if* %verbose% 
         then (format t "No i-descriptions.  Fizzling.~&"))
        (return))

  ; Choose the descriptor for the initial-string object probabilistically.  
  (setq i-probabilities 
	(get-temperature-adjusted-value-list
	    (send-method-to-list i-descriptions :conceptual-depth)))
  (setq i-description 
	(nth (select-list-position i-probabilities) i-descriptions))

  (if* %verbose% 
   then (format t "The i-description is: ") 
        (send i-description :print))

  ; Now choose the descriptor for the modified-string object.
  ; Get the usable descriptions of the modified-string object.  For now, 
  ; this includes all descriptions except string-position-category 
  ; descriptions, and object-category descriptions.
  (setq m-descriptions 
	(append (send m-obj :extrinsic-descriptions)
	        (send m-obj :rule-modified-string-descriptions)))
	             
  (if* %verbose% 
   then (format t "m-descriptions: ") 
        (loop for d in m-descriptions do
	      (if* (or (typep d 'description) 
		      (typep d 'extrinsic-description))
	       then (send d :print) else (send d :pname))
              (format t "; "))
        (format t "~%"))
      
  (if* (null m-descriptions) 
   then (if* %verbose% 
         then (format t "No m-descriptions.  Fizzling.~&"))
        (return))

  (setq m-probabilities 
	(get-temperature-adjusted-value-list
	    (send-method-to-list m-descriptions :conceptual-depth)))

  (setq m-description 
	(nth (select-list-position m-probabilities) m-descriptions))

  ; This is a kludge to avoid rules like "Replace C by successor of C".
  ; If a description like "successor of C" is chosen, then the description 
  ; with the descriptor "D" is substituted for it.
  (if* (and (typep m-description 'extrinsic-description)
	   (setq related-descriptor 
		 (send (send i-description :descriptor) 
		       :get-related-node (send m-description :relation))))
   then (if* %verbose% 
	 then (format t "fixing description~&"))
        (setq m-description (loop for d in (send m-obj :descriptions) 
				  when (eq (send d :descriptor) 
					   related-descriptor)
				  return d)))

  (if* %verbose% 
   then (format t "The m-description is: ") 
        (send m-description :print))

  (propose-rule i-obj i-description m-obj m-description)))

;---------------------------------------------

(defun rule-strength-tester (proposed-rule &aux proposed-rule-strength
				                build-probability urgency)
; Calculates the proposed-rule's strength, and probabilistically decides
; whether or not to post a rule-builder codelet.  If so, the urgency of
; the rule-builder codelet is a function of the strength.
(block nil
  (if* %verbose% 
   then (format t "In rule-strength-tester with rule ")
        (send proposed-rule :print))

  ; Calculate the proposed rule's strength.
  (send proposed-rule :update-strength-values)
  (setq proposed-rule-strength (send proposed-rule :total-strength))
  (if* %verbose% 
   then (format t "Proposed-rule strength is ~a~&" proposed-rule-strength))

  ; Decide whether or not to post a rule-builder codelet, based on the 
  ; strength of the proposed-rule.
  (setq build-probability 
	(get-temperature-adjusted-probability 
	    (/ proposed-rule-strength 100)))
  (if* %verbose% 
   then (format t "Build-probability: ~a~&" build-probability))
  (if* (eq (flip-coin build-probability) 'tails)
   then (if* %verbose% 
	 then (format t "Rule not strong enough.  Fizzling.~&"))
        (return))
        
  (setq urgency proposed-rule-strength)
  (if* %verbose% 
   then (format t "Strong enough! Posting rule-builder with urgency ~a~&"
		(get-urgency-bin urgency)))

  (send *coderack* :post 
        (make-codelet 'rule-builder (list proposed-rule) 
                      (get-urgency-bin urgency)))))

;---------------------------------------------

(defun rule-builder (proposed-rule)
; Tries to build the proposed rule, fighting with competitors if necessary.  
 
(block nil
  (if* %verbose% 
   then (format t "In rule builder with proposed rule: ") 
        (send proposed-rule :print))
  
  ; If this rule already exists, then fizzle.  
  (if* *rule*
   then (if* (rule-equal? *rule* proposed-rule)
         then (if* %verbose% 
	       then (format t "This rule already exists.  Fizzling.~&"))
              (activate-from-workspace-rule-descriptions proposed-rule)
	      (return)))

  ; If a different rule already exists, then fight.
  (if* *rule*
   then (if* %verbose% 
	 then (format t "About to fight with old rule.~&"))
	(if* (not (fight-it-out proposed-rule 1 (list *rule*) 1))
	 then (if* %verbose% 
	       then (format t "Lost.  Fizzling.~&"))
	      (return)
	 else (if* %verbose% 
               then (format t "Won against old rule!~&"))))

  ; Build this rule.
  (if* *rule* then (break-rule *rule*))
  (build-rule proposed-rule)))
  
;---------------------------------------------

(defun build-rule (new-rule)
; This function actually builds the new rule.
  (setq *rule* new-rule)
  (activate-from-workspace-rule-descriptions new-rule)
  (if* %workspace-graphics% then (send *rule* :draw %rule-mode%)))

;---------------------------------------------

(defun build-translated-rule (new-translated-rule)
; This function builds the translated rule.
  (setq *translated-rule* new-translated-rule)
  (if* %workspace-graphics% 
   then (send *translated-rule* :draw %translated-rule-mode%)))

;---------------------------------------------

(defun break-rule (rule)
; Breaks the rule.  The only reason this function has argument "rule" is so 
; that it matchs the form of the other "break" functions, and thus the breaker 
; codelets can call it.
  (if* %workspace-graphics% then (send *rule* :erase %rule-mode%))
  (setq *rule* nil))

;---------------------------------------------

(defun rule-translator (&aux slippage-list answer-temperature-threshold 
			     changed-obj changed-obj-correspondence
			     new-translated-rule)
; This codelet translates the rule according to the translation rules given 
; in the slippages on the workspace.
(block nil
  (if* %verbose% then (format t "In rule-translator~&"))

  ; If no rule, fizzle.
  (if* (null *rule*) 
   then (if* %verbose% 
         then (format t "No rule.  Fizzling.~&"))
        (return))

  (if* (send *rule* :no-change?)
   then (setq *translated-rule* 
	      (make-non-relation-rule nil nil nil nil nil nil))
        (if* %workspace-graphics% 
         then (send *translated-rule* :draw %translated-rule-mode%))
	(return))

  ; If the temperature is too high (a threshold is probabilistically chosen), 
  ; then fizzle.
   (setq answer-temperature-threshold 
	 (send (get-answer-temperature-threshold-distribution) :choose))
   (if* %verbose%
    then (format t "The answer-temperature-threshold is ~a~&" 
		 answer-temperature-threshold))
   (if* (> *temperature* answer-temperature-threshold)
   then (if* %verbose% 
	 then (format t "Temperature too high.  Fizzling.~&"))
        (return))

  ; Otherwise build translation of rule.

  ; Find changed object.
  (setq changed-obj (loop for obj in (send *initial-string* :object-list)
			  when (send obj :changed?) return obj
			  finally (return nil)))
	
  ; If no changed object, then fizzle.
  (if* (null changed-obj) 
   then (if* %verbose% 
         then (format t "There is no changed object.~&"))
        (return))

  (setq changed-obj-correspondence (send changed-obj :correspondence))

  ; Get slippages to use.
  (setq slippage-list (send *workspace* :slippage-list))
  (if* changed-obj-correspondence
   then (loop for s in (send *workspace* :slippage-list) do
	      (loop for cm in (send changed-obj-correspondence 
				    :concept-mapping-list) 
	            when (contradictory-concept-mappings? cm s) do
	                 (setq slippage-list (remove s slippage-list)))))
	      
  (setq new-translated-rule 
	(if* (send *rule* :relation?)
         then (make-relation-rule 
		  (send (send *rule* :object-category1) 
			:apply-slippages slippage-list)
	          (send (send *rule* :descriptor1-facet) 
			:apply-slippages slippage-list)
	          (send (send *rule* :descriptor1) 
			:apply-slippages slippage-list)
	          (send (send *rule* :object-category2) 
			:apply-slippages slippage-list)
 	          (send (send *rule* :replaced-description-type) 
			:apply-slippages slippage-list)
                  (send (send *rule* :relation) 
			:apply-slippages slippage-list))
	 else (make-non-relation-rule 
		  (send (send *rule* :object-category1) 
			:apply-slippages slippage-list)
	          (send (send *rule* :descriptor1-facet) 
			:apply-slippages slippage-list)
	          (send (send *rule* :descriptor1) 
			:apply-slippages slippage-list)
	          (send (send *rule* :object-category2) 
			:apply-slippages slippage-list)
	          (send (send *rule* :replaced-description-type) 
			:apply-slippages slippage-list)
	          (send (send *rule* :descriptor2) 
			:apply-slippages slippage-list))))

  (build-translated-rule new-translated-rule)))    

;---------------------------------------------

(defun propose-rule (i-obj i-description m-obj m-description
	             &aux proposed-rule urgency)
; Creates a proposed rule, and posts a rule-strength-tester codelet with 
; urgency a function of the degree of conceptual-depth of the descriptions in the 
; rule.

  (if* (null i-obj)
   then (setq proposed-rule (make-non-relation-rule nil nil nil nil nil nil))
   else (if* (typep m-description 'extrinsic-description)
         then (setq proposed-rule 
	            (make-relation-rule 
		       (send i-obj :get-descriptor plato-object-category)
                       (send i-description :description-type)
                       (send i-description :descriptor)
		       (send m-obj :get-descriptor plato-object-category)
 	               (send m-description :description-type-related)
	               (send m-description :relation)))
         else (setq proposed-rule 
	            (make-non-relation-rule 
                        (send i-obj :get-descriptor plato-object-category)
                        (send i-description :description-type)
 	                (send i-description :descriptor)
		        (send m-obj :get-descriptor plato-object-category)
	                (send m-description :description-type)
	                (send m-description :descriptor)))))

  (if* %verbose% 
   then (format t "The proposed rule is:~&") (send proposed-rule :print) 
        (format t "~%"))

  (if* (null i-description)
   then (setq urgency 100)
   else ; The average alone is too low for low-conceptual-depth rules.
        (setq urgency 
	      (* 100 (sqrt (/ (average (send i-description :conceptual-depth)
		                       (send m-description :conceptual-depth))
  			      100)))))

  (if* %verbose% 
   then (format t "Posting a rule-strength-tester with urgency ~a~&" 
		(get-urgency-bin urgency)))
  (send *coderack* :post 
        (make-codelet 'rule-strength-tester (list proposed-rule)
	              (get-urgency-bin urgency))))

;---------------------------------------------

(defun activate-from-workspace-rule-descriptions (rule)
; Activate the nodes corresponding to the descriptions in the rule.
  (if* (send rule :descriptor1)
   then (send (send rule :descriptor1) :activate-from-workspace))
  (if* (send rule :relation?)
   then (send (send rule :relation) :activate-from-workspace)
   else (if* (send rule :descriptor2)
	 then (send (send rule :descriptor2) :activate-from-workspace))))

;---------------------------------------------


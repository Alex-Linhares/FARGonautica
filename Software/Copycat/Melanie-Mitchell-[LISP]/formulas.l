;---------------------------------------------
; FORMULAS: This file contains some general formulas used in Copycat.
;---------------------------------------------

(in-package 'user)

(defun update-temperature (&aux rule-weakness)
; Updates the temperature, which is a function of the average total-unhappiness
; of objects on the workspace (weigted by importance) and the weakness of the
; rule.
  (if* (not *clamp-temperature*)
   then (setq rule-weakness 
	      (if* (null *rule*) 
	       then 100
	       else (fake-reciprocal (send *rule* :total-strength))))
  
        (setq *temperature* 
	      (weighted-average 
	          `((,(send *workspace* :total-unhappiness) . 8) 
		    (,rule-weakness . 2))))))

;---------------------------------------------

(defun get-answer-temperature-threshold-distribution (&aux bond-density)
; Returns the probability-distribution from which to 
; choose a temperature threshold, used by rule-translator codelets in deciding
; whether or not to try to translate the rule and create an answer.  Which 
; distribution to use is a function of the amount of structure that has been 
; built (irrespective of its strength).  The more structure, the more the 
; distribution is slanted towards low temperatures.  The idea behind this
; can be given by listing the various cases:
;
;     If lots of structure has been built and it is good, then
;     the temperature will be low and it's probably a good time to try to 
;     create an answer, so return a distribution slanted towards low 
;     temperature, which, since the temperature is low, will make it likely 
;     that the rule-translator codelet will decide to create an answer.
;
;     If lots of structure has been built and it isn't good, then the
;     temperature will be high.  Since the program has found some structure,
;     it is likely that there *is* a better way of structuring things, so
;     return a distribution slanted towards low temperature, i.e., make
;     it unlikely to create an answer soon.
;
;     If not much structure has been found, then it is not likely that much
;     more will be found (since the rule-translator codelet most likely 
;     will not even be running until there has been opportunity to find
;     structure if there is any).  Thus the program shouldn't have to wait
;     much longer to go ahead and create an answer, since the lack of structure
;     at this point indicates that it probably will not be able to do much
;     better.  So return a distribution slanted more towards higher 
;     temperatures.
;
; This function only roughly measures how much structure there is.
  
  (if* (and (= (send *initial-string* :length) 1) 
	    (= (send *target-string* :length) 1))
   then (setq bond-density 1)
   else (setq bond-density 
	      (/ (length (append (send *initial-string* :bond-list) 
				 (send *target-string* :bond-list)))
	         (+ (1- (send *initial-string* :length))
		    (1- (send *target-string* :length))))))
		     
  (cond ((>= bond-density .8) 
	 %very-low-answer-temperature-threshold-distribution%)
        ((>= bond-density .6) 
	 %low-answer-temperature-threshold-distribution%)
        ((>= bond-density .4) 
	 %medium-answer-temperature-threshold-distribution%)
        ((>= bond-density .2) 
	 %high-answer-temperature-threshold-distribution%)
        (t %very-high-answer-temperature-threshold-distribution%)))

;---------------------------------------------

(defun get-temperature-adjusted-probability (prob &aux low-prob-factor
						       result)
; This function is a filter:  it inputs a value (from 0 to 100) and returns
; a probability (from 0 - 1) based on that value and the temperature.  When
; the temperature is 0, the result is (/ value 100), but at higher 
; temperatures, values below 50 get raised and values above 50 get lowered
; as a function of temperature.
; I think this whole formula could probably be simplified.

  (setq result
	(cond ((= prob 0) 0)
	      ((<= prob .5)
               (setq low-prob-factor (max 1 (truncate (abs (log prob 10)))))
               (min (+ prob 
		       (* (/ (- 10 (sqrt (fake-reciprocal *temperature*))) 
			     100) 
			  (- (expt 10 (- (1- low-prob-factor))) prob)))
		    .5))
		     
   	      ((= prob .5) .5)
	      ((> prob .5)
               (max (- 1
		        (+ (- 1 prob) 
		           (* (/ (- 10 (sqrt (fake-reciprocal *temperature*))) 
			         100)
			      (- 1 (- 1 prob)))))
	            .5))))
  result)
		     
;---------------------------------------------

(defun test-get-temperature-adjusted-probability (prob)
(with-open-file (ostream "testfile" :direction :output 
	 	                  :if-does-not-exist :create
				  :if-exists :append) 
	(format ostream "prob: ~a~&" prob)
  (loop for temp in '(0 10 20 30 40 50 60 70 80 90 100) do
        (setq *temperature* temp)
        (format ostream "Temperature: ~a; probability ~a~&"
		temp (float (get-temperature-adjusted-probability prob))))
  (format ostream "~%")))

;---------------------------------------------

(defun get-temperature-adjusted-value-list (value-list &aux exponent)  
; Returns a list with values that are exponential functions of the original
; values, with the exponent being a function of the temperature.  The higher
; the temperature, the bigger the difference between unequal values.
  (setq exponent (+ (/ (fake-reciprocal *temperature*) 30) .5))
  (loop for value in value-list collect (round (expt value exponent))))

;---------------------------------------------

(defun test-get-tempterature-adjusted-value-list (value-list)
  (loop for temp in '(0 10 20 30 40 50 60 70 80 90 100) do
        (setq *temperature* temp)
        (format t "Temperature: ~a; adjusted-value-list: ~a~&"
		temp (get-temperature-adjusted-value-list value-list))))

;---------------------------------------------

(defun get-post-codelet-probability (structure-category &aux probability)
; This function gives the program a simple form of self-watching.  For a given
; structure-category (e.g., description, or bond), it returns a probability
; to use in deciding whether or not codelets looking for this type of 
; structure should be posted.  

  (cond ((eq structure-category 'description)
	 (setq probability (/ (sqr *temperature*) 100)))
	
	((eq structure-category 'bond) 
         (setq probability (send *workspace* :intra-string-unhappiness)))

        ((eq structure-category 'group) 
         (setq probability (send *workspace* :intra-string-unhappiness)))

       ((eq structure-category 'replacement)
	(setq probability
	      (if* (send *workspace* :unreplaced-objects) then 100 else 0)))

       ((eq structure-category 'correspondence)
        (setq probability (send *workspace* :inter-string-unhappiness)))

       ((eq structure-category 'rule)
        (setq probability 
	      (if* (null *rule*) 
	       then 100 else (send *rule* :total-weakness))))

	((eq structure-category 'translated-rule)
         (setq probability (if* *rule* then 100 else 0))))

  (/ probability 100))

;---------------------------------------------

(defun get-num-of-codelets-to-post (structure-category &aux number)
; This function gives the program a simple form of self-watching.  For a given
; structure-category (e.g., description, or bond), it returns the number
; of codelets looking for this type of structure that should be posted.  

  (cond ((eq structure-category 'description)
	 (setq number 1))
	
	((eq structure-category 'bond) 
         (setq number (case (send *workspace* :rough-num-of-unrelated-objects)
			    (few 1) (medium 2) (many 3))))

        ((eq structure-category 'group) 
         (setq number
	       (if* (null (send *workspace* :bond-list))
                then 0
	        else (case (send *workspace* 
			         :rough-num-of-ungrouped-objects) 
			         (few 1) (medium 2) (many 3)))))

       ((eq structure-category 'replacement)
        (setq number 
	      (if* *rule*
	       then 0
               else (case (send *workspace* :rough-num-of-unreplaced-objects)
                          (few 1) (medium 2) (many 3)))))


       ((eq structure-category 'correspondence)
         (setq number 
	       (case (send *workspace* :rough-num-of-uncorresponding-objects) 
		     (few 1) (medium 3) (many 3))))

       ((eq structure-category 'rule)
	(setq number 2))


       ((eq structure-category 'translated-rule)
        (setq number (if* (null *rule*) then 0 else 1))))

  number)

;---------------------------------------------

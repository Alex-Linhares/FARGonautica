;---------------------------------------------
; CODERACK:  This file contains flavors, methods, and functions for the 
;                 coderack.
;---------------------------------------------

(in-package 'user)

;---------------------------------------------

(defflavor codelet
  (codelet-type ; E.g., bottom-up-bond-scout.
   arguments 
   urgency-bin ; E.g., %very-high-bin%
   (index-in-bin nil) ; This codelet's position in its urgency bin.
   (time-stamp nil) ; The time (in codelet-steps) when this codelet was 
                    ; posted
   structure-category) ; E.g., 'bond.
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)


;---------------------------------------------

(defmethod (codelet :print-to-output-file) (output-file)
  (with-open-file 
      (ostream output-file :direction :output 
	  :if-exists :append :if-does-not-exist :create)    
  (format ostream "codelet-type: ~a~&" codelet-type)))

;---------------------------------------------

(defmethod (codelet :print) ()
  (format t "codelet-type: ~a, arguments: ~a" codelet-type arguments)
  (format t " urgency-bin ~a, time-stamp ~a,~&" 
	  (send urgency-bin :pname) time-stamp)
  (format t "~%"))

;---------------------------------------------

(defmethod (codelet :run) ()
; This is the method that runs the codelet.    
  (apply codelet-type arguments))

;---------------------------------------------

(defmethod (codelet :remove-probability) ()
; Returns the probability of removing this codelet from the coderack
; (a function of the codelet's urgency and age).
; The 1+ allows some probability for codelets with the highest urgency
; to be removed.
  (* (- *codelet-count* time-stamp) 
     (1+ (- (send *extremely-high-bin* :urgency-value)
	    (send urgency-bin :urgency-value)))))

;---------------------------------------------

(defun make-codelet (codelet-type arguments urgency-bin-name 
	             &optional structure-category)
; Returns a new codelet.
  (make-instance 'codelet 
      :codelet-type codelet-type
      :arguments arguments
      :urgency-bin (eval urgency-bin-name)
      :structure-category structure-category))
      
;---------------------------------------------

(defflavor coderack 
  (bin-list ; The list of coderack bins, each corresponding to an urgency
            ; level.
   codelet-urgency-array) ; This is used in the graphics routines for
                          ; displaying the coderack.
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defmethod (coderack :total-num-of-codelets) ()
; Returns the total number of codelets currently in the coderack.
  (list-sum (send-method-to-list *coderack-bins* :num-of-codelets-in-bin)))
  
;---------------------------------------------

(defflavor coderack-bin
  (vector urgency-code relative-urgency-sum 
   codelet-in-bin-probability pname)	  
  ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defmethod (coderack :spy) (&aux codelet codelet-type codelet-type-vector
   				      codelet-number num-of-codelets 
				      urgency-sum-vector
				      urgency-sum total-sum
				      relative-sum (total-relative-sum 0))
; Prints out some info about the current state of the coderack.
  (setq codelet-type-vector 
	(make-vector (length %codelet-types%) :initial-element 0))
				
  (setq urgency-sum-vector 
	(make-vector (length %codelet-types%) :initial-element 0))

  (loop for bin in *coderack-bins* do
	(loop for i from 0 to (1- (send bin :fill-pointer)) do
              (setq codelet (vref (send bin :vector) i))
	      (setq codelet-number 
		    (get-codelet-number (send codelet :codelet-type)))
	      (vset codelet-type-vector codelet-number
		    (1+ (vref codelet-type-vector codelet-number)))
	      (vset urgency-sum-vector codelet-number
		    (+ (send bin :urgency-value)
		       (vref urgency-sum-vector codelet-number)))))
  (setq total-sum (reduce #'+ urgency-sum-vector))
  (loop for i from 0 to (1- (vsize codelet-type-vector)) do
	(setq codelet-type (get-codelet-name i))
        (setq num-of-codelets (vref codelet-type-vector i))
        (setq urgency-sum (vref urgency-sum-vector i))
        (setq relative-sum (round (* 100 (/ urgency-sum total-sum))))
	(setq total-relative-sum (+ relative-sum total-relative-sum))
	(if* (> num-of-codelets 0)
	 then (format t 
		      "~a:~% ~a codelets; eff-urg-sum: ~a; rel-sum: ~a ~&" 
		      codelet-type num-of-codelets urgency-sum
		      relative-sum)))
		      
  (format t "Total sum: ~a~&" total-sum)
  (format t "Total relative sum: ~a~&" total-relative-sum))

;---------------------------------------------

(defmethod (coderack-bin :fill-pointer) ()
  (fill-pointer vector))

;---------------------------------------------

(defmethod (coderack-bin :set-fill-pointer) (value)
  (setf (fill-pointer vector) value))

;---------------------------------------------

(defmethod (coderack-bin :num-of-codelets-in-bin) ()
  (fill-pointer vector))

;---------------------------------------------

(defmethod (coderack-bin :urgency-sum) ()
; Returns the sum of the urgencies in this bin.
  (* (send self :num-of-codelets-in-bin) (send self :urgency-value)))

;---------------------------------------------

(defmethod (coderack-bin :urgency-value) ()
; Returns the value associated with this bin (a function of which bin this is 
; and the temperature).
  (aref %urgency-value-array% urgency-code *temperature*))

;---------------------------------------------

(defmethod (coderack :codelet-list) ()
; Returns a list of all the codelets in the coderack.
  (flatten (loop for bin in *coderack-bins* 
		 collect (vector-to-list (send bin :vector)))))

;---------------------------------------------

(defmethod (coderack :empty) ()
; Empty out the coderack.
  (loop for codelet in (send *coderack* :codelet-list) do
	(send self :delete-codelet-from-graphics codelet))
  (loop for bin in *coderack-bins* do
        (send bin :set-fill-pointer 0))
  (setq *codelet-list* nil))

;---------------------------------------------

(defun make-coderack (&aux new-coderack)
; Returns a new coderack.
  (setq *extremely-low-bin* 
	(make-instance 'coderack-bin 
	    :vector (make-bin-vector %max-coderack-size%) 
            :urgency-code 0 :pname "extremely-low-bin"))

  (setq *very-low-bin* 
	(make-instance 'coderack-bin 
	    :vector (make-bin-vector %max-coderack-size%) 
            :urgency-code 1 :pname "very-low-bin"))

  (setq *low-bin* 
	(make-instance 'coderack-bin 
	    :vector (make-bin-vector %max-coderack-size%) 
            :urgency-code 2 :pname "low-bin"))

  (setq *medium-bin* 
	(make-instance 'coderack-bin 
	  :vector (make-bin-vector %max-coderack-size%) 
          :urgency-code 3 :pname "medium-bin"))

  (setq *high-bin* 
	(make-instance 'coderack-bin 
	    :vector (make-bin-vector %max-coderack-size%) 
            :urgency-code 4 :pname "high-bin"))

  (setq *very-high-bin*
	(make-instance 'coderack-bin 
	    :vector (make-bin-vector %max-coderack-size%) 
            :urgency-code 5 :pname "very-high-bin"))

  (setq *extremely-high-bin*
	(make-instance 'coderack-bin 
	    :vector (make-bin-vector %max-coderack-size%) 
            :urgency-code 6 :pname "extremely-high-bin"))

  (setq *coderack-bins* 
	(list *extremely-low-bin* *very-low-bin* *low-bin* 
	      *medium-bin* *high-bin* *very-high-bin* *extremely-high-bin*))

  (setq *urgency-list* 
	'(*extremely-low-bin* *very-low-bin* *low-bin* 
	  *medium-bin* *high-bin* *very-high-bin* *extremely-high-bin*))
	  
  (setq new-coderack
       (make-instance 'coderack
	 :bin-list *coderack-bins*
         :codelet-urgency-array 
         (make-array (list (length %codelet-types%) %num-of-urgency-bins%)
	             :initial-element 0)))
  new-coderack)
  
;---------------------------------------------

(defmethod (coderack :total-urgency-sum) ()
  (list-sum (send-method-to-list *coderack-bins* :urgency-sum)))

;---------------------------------------------

(defmethod (coderack :empty?) ()
; Returns t if the coderack is empty, nil otherwise.
  (= 0 (send self :total-urgency-sum)))

;---------------------------------------------

(defmethod (coderack :post) (codelet &aux bin)
; Posts a codelet to the coderack.  If the coderack has 
; %max-coderack-size% codelets, remove a codelet to make room for the new
; one.

  (if* (= (send self :total-num-of-codelets) %max-coderack-size%)
   then (send self :remove-codelets 1)) ; Remove a codelet from the 
                                        ; coderack.
  (setq bin (send codelet :urgency-bin))
  (vset (send bin :vector) (send bin :fill-pointer) codelet)
  (send codelet :set-index-in-bin (send bin :fill-pointer))
  (send codelet :set-time-stamp *codelet-count*)
  (send bin :set-fill-pointer (1+ (send bin :fill-pointer)))
  (push codelet *codelet-list*)
  (send self :add-codelet-to-graphics codelet))

;---------------------------------------------

(defmethod (coderack :post-without-removing) (codelet &aux bin)
; Posts a codelet to the coderack.  
  (setq bin (send codelet :urgency-bin))
  (vset (send bin :vector) (send bin :fill-pointer) codelet)
  (send codelet :set-index-in-bin (send bin :fill-pointer))
  (send codelet :set-time-stamp *codelet-count*)
  (send bin :set-fill-pointer (1+ (send bin :fill-pointer)))
  (push codelet *codelet-list*)
  (send self :add-codelet-to-graphics codelet))

;---------------------------------------------

(defmethod (coderack :post-codelet-list) (codelet-list &aux num-to-remove)
; See how many codelets have to be removed.  Remove them, and then
; post the codelets on this list.
  (setq num-to-remove 
	(- (+ (send *coderack* :total-num-of-codelets)
	      (length codelet-list))
	   %max-coderack-size%))
  (if* (> num-to-remove 0)
   then (send *coderack* :remove-codelets num-to-remove))
  (loop for codelet in codelet-list do
	(send *coderack* :post-without-removing codelet)))

;---------------------------------------------

(defmethod (coderack :choose) (&aux chosen-bin chosen-index codelet)
; Chooses a codelet from the coderack.
(block nil
  (if* (send self :empty?) 
   then (format t "Can't choose: coderack is empty.~&") 
        (return))     

  ; Choose a bin probabilistically according to the urgency sum.
  (setq chosen-bin 
	(select-list-item-by-method *coderack-bins* ':urgency-sum))

  ; Choose a random codelet in this bin.
  (setq chosen-index (random (send chosen-bin :num-of-codelets-in-bin)))
  (setq codelet (vref (send chosen-bin :vector) chosen-index))

  ; If this codelet left a hole in the vector, fill it in with the last 
  ; codelet in the bin.  Adjust the fill-pointer.
  (if* (< chosen-index (1- (send chosen-bin :fill-pointer)))
   then (vset (send chosen-bin :vector) chosen-index 
	      (vref (send chosen-bin :vector) 
		    (1- (send chosen-bin :fill-pointer))))
        ; Give the codelet that moved its new bin index.
        (send (vref (send chosen-bin :vector) chosen-index) 
	      :set-index-in-bin chosen-index))
  (send chosen-bin :set-fill-pointer (1- (send chosen-bin :fill-pointer)))
  (setq *codelet-list* (remove codelet *codelet-list*))
  (send *coderack* :delete-codelet-from-graphics codelet)
  codelet))

;---------------------------------------------

(defmethod (coderack :remove-codelets) 
           (num-to-remove &aux remove-probability-list codelet argument bin 
	                       index (num-removed 0))
; Removes the given number of codelets from the coderack 
; probabilistically, biased towards deleting low-urgency, older codelets.
(block nil
  (if* (send self :empty?) 
   then (format t "Can't remove any more codelets: coderack is empty.~&") 
        (return))     

  (setq remove-probability-list
	(send-method-to-list *codelet-list* :remove-probability))
  (loop until (or (= num-removed num-to-remove) (send self :empty?)) do
        (setq codelet (nth (select-list-position remove-probability-list)
			   *codelet-list*))
	(if* codelet
         then (setq bin (send codelet :urgency-bin))
              (setq index (send codelet :index-in-bin))
	      (vset (send bin :vector) index nil)
              (setq *codelet-list* (remove codelet *codelet-list*))
              (send *coderack* :delete-codelet-from-graphics codelet)
              (setq argument (car (send codelet :arguments)))
              (if* (and (not (eq (send codelet :codelet-type) 'breaker))
			(typep argument 'workspace-structure)
			(not (or (typep argument 'rule) 
				 (typep argument 'description))))
               then (send *workspace* :delete-proposed-structure argument)
                    (if* (and %workspace-graphics% 
			      (send argument :graphics-obj))
	             then (send argument :erase-proposed)))
		    
              (if* %verbose% 
               then (format t "Removed ") 
	            (send codelet :print))
	      (incf num-removed)

              ; Fill in hole in bin left by removed codelet, if necessary.
              (if* (< index (1- (send bin :fill-pointer)))
               then (vset (send bin :vector) index 
	    	          (vref (send bin :vector) 
				(1- (send bin :fill-pointer))))
                    ; Give the codelet that moved its new bin index.
                    (send (vref (send bin :vector) index) 
			  :set-index-in-bin index))
              (send bin :set-fill-pointer 
		    (1- (send bin :fill-pointer)))))
  (if* (send self :empty?)
   then (format t "Can't remove any codelets: coderack is empty.~&"))))
    
;---------------------------------------------

(defmethod (coderack :display) ()
; Displays the contents of the coderack (text, not graphics).
  (loop for bin in *coderack-bins* do
        (format t "~a: value: ~a; effective-sum: ~a~&" 
            (send bin :pname)
	    (send bin :urgency-value)
            (send bin :urgency-sum))
        (format t "------------~&")
        (loop for i from 0 to (1- (send bin :fill-pointer)) do
	      (send (vref (send bin :vector) i) :print))
  
        (format t "~%")))

;---------------------------------------------

(defun get-urgency-bin (value)
; Returns the urgency bin corresponding to a given number.
  (if* (>= value 100) 
   then '*extremely-high-bin*
   else (nth (truncate (/ (* value %num-of-urgency-bins%) 100)) 
	     *urgency-list*)))
  
;---------------------------------------------

(defun get-coderack-bin (bin-number)
  (nth bin-number *coderack-bins*))

;---------------------------------------------

(defun get-bottom-up-codelets ()
; Adds various bottom-up codelets to *codelets-to-post*, deciding on how
; many to add and urgency, as a function of how much each type of codelet 
; is needed.

  ; Add bottom-up description codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'description))
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'description) do
	      (push (make-codelet 'bottom-up-description-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up bond codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'bond)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'bond) do
              (push (make-codelet 'bottom-up-bond-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up group codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'group)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'group) do
              (push (make-codelet 'group-scout--whole-string nil 
		                 '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up replacement codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'replacement)) 
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'replacement) do
              (push (make-codelet 'replacement-finder nil '*low-bin*) 
	            *codelets-to-post*)))

  ; Add bottom-up correspondence codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'correspondence))
	              'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'correspondence) do
              (push (make-codelet 'bottom-up-correspondence-scout nil 
		                  '*low-bin*) *codelets-to-post*)
              (push (make-codelet 'important-object-correspondence-scout nil 
		                  '*low-bin*) *codelets-to-post*)))

  ; Add bottom-up rule codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'rule)) 'heads)
   then (loop for i from 1 to (get-num-of-codelets-to-post 'rule) do
              (push (make-codelet 'rule-scout nil '*low-bin*) 
		    *codelets-to-post*)))

  ; Add bottom-up rule-translator codelets.
  (if* (eq (flip-coin (get-post-codelet-probability 'translated-rule)) 
	   'heads)
   then (loop for i from 1 
	      to (get-num-of-codelets-to-post 'translated-rule) do
              (push (make-codelet 'rule-translator nil 
		                  (if* (> *temperature* 25) 
		                   then '*low-bin* else '*high-bin*))
	            *codelets-to-post*)))

  ; Add bottom-up breaker codelets.
  (push (make-codelet 'breaker nil '*extremely-low-bin*) 
	*codelets-to-post*))

;---------------------------------------------

(defun init-coderack ()
; Makes a coderack called *coderack*.
  (setq *coderack* (make-coderack))
  (setq *codelet-list* nil)
  (setq *coderack-initialized* t))

;---------------------------------------------

(defun post-initial-codelets ()
; Post the initial codelets the program starts out with.
  (loop for i from 1 to (* 2 (length (send *workspace* :object-list))) do
        (send *coderack* :post 
	      (make-codelet 'bottom-up-bond-scout nil '*very-low-bin*))
	(send *coderack* :post 
	      (make-codelet 'replacement-finder nil '*very-low-bin*))
	(send *coderack* :post
	      (make-codelet 'bottom-up-correspondence-scout nil 
		            '*very-low-bin*)))

  (send *coderack* :post-codelet-list *codelets-to-post*)
  (setq *codelets-to-post* nil))
  
;---------------------------------------------

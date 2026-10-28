;---------------------------------------------
; RUN: This file contains functions for running the program.
;---------------------------------------------

(in-package 'user)
  
(defun run-ccat ()
; Runs the main loop of the program:  choose a codelet, run it.  Every
; %time-step-length% time-steps, update everything in the program (all the
; values on the workspace, all the activations in the slipnet, etc.).

  ; This is the main loop of the program.
  (loop until *quit-program* do

        ; If the program has run for %time-step-length% steps, then 
	; update everything.
        (if* (= (mod *codelet-count* %time-step-length%) 0)
	 then (update-everything))

        ; If there are no codelets left in the coderack, then 
	; clamp the initially clamped nodes and post the 
	; initial set of codelets.
	(if* (send *coderack* :empty?)
         then (loop for node in *initially-clamped-slipnodes* do
	            (send node :set-clamp t))
	      (post-initial-codelets))

        ; Step the program.
	(step-ccat)

	(if* %verbose% then (break))
        
        ; If the rule has been translated, then build the answer.
        (if* *translated-rule*   
         then (answer-builder) 
              (if* *found-answer* 
	       then (update-everything)
                    (if* %verbose% 
	             then (format t "My answer is ~a.~&" 
				    (send *answer-string* :pstring)))

                    ; Collect some statistics.
		    (loop for group in (send *workspace* :group-list)
			  when (= (send group :length) 1) do
			  (incf *single-letter-group-at-end-count*))

		    (if* (= (send plato-length :activation) 100)
                     then (setq *length-relevant-at-end* t))
			  
  		    (setq *quit-program* t)))))

;---------------------------------------------

(defun update-everything (&aux new-structure-list unclamp-probability)
; Updates all the values in the program (workspace values, slipnet 
; activations, etc.).

  (setq *updating-everything* t)

  ; Update values for structures and objects.
  (send-method-to-list (send *workspace* :structure-list) 
                       :update-strength-values)
  (send-method-to-list (send *workspace* :object-list) :update-object-values)
  (send *initial-string* :update-relative-importances)
  (send *target-string* :update-relative-importances)
  (send *initial-string* :update-intra-string-unhappiness)
  (send *target-string* :update-intra-string-unhappiness)
  
  ; If %initial-slipnode-clamp-time% cycles have gone by, then unclamp
  ; the initially-clamped slipnodes.
  (if* (= *codelet-count* 
	  (* %initial-slipnode-clamp-time% %time-step-length%))
   then (loop for node in *initially-clamped-slipnodes*
	      do (send node :set-clamp nil)))
  
  ; If the program is dealing with a snag, then see if any new structures
  ; have been made.  If so, see if snag condition should be ended.
  (if* (and *snag-object* *snag-condition*)
   then (setq new-structure-list
	      (loop for structure in (send *workspace* :structure-list)
		    when (and (not (typep structure 'bond))
			      (not (send *workspace* 
					 :structure-in-snag-structure-list?
					 structure)))
		    collect structure))

        (setq unclamp-probability 
	      (if* (null new-structure-list)
               then 0
	       else (/ (list-max (send-method-to-list new-structure-list 
			 	                      :total-strength))
		       100)))

	(if* (eq (flip-coin unclamp-probability) 'heads)
         then (setq *snag-condition* nil)
              (setq *clamp-temperature* nil)
              (loop for d in (send *snag-object* :descriptions) do
   	            (send (send d :descriptor) :set-clamp nil))
              (send *snag-object* :set-clamp-salience? nil)))
 
  (if* (> *codelet-count* 0) 
   then (update-temperature)
        (get-bottom-up-codelets) 
        (get-top-down-codelets)
	(update-slipnet))

  (if* *codelets-to-post*
   then (send *coderack* :post-codelet-list *codelets-to-post*))
  (setq *codelets-to-post* nil)

  (if* (> *codelet-count* 0)
   then (if* %description-graphics% then (display-descriptions))
        (if* %temperature-graphics% then (update-temperature-display))
        (if* %coderack-graphics% then (update-coderack-display))
        (if* %minimal-coderack-graphics% 
	 then (update-minimal-coderack-display))
        (if* %slipnet-graphics% then (update-slipnet-display))
        ; Update concept-mapping and length displays.
        (if* %workspace-graphics% 
         then (loop for c in (send *workspace* :correspondence-list) do
                    (send c :erase-concept-mappings)
                    (send c :draw-concept-mappings))
              (loop for group 
	            in (send *workspace* :group-list) do
	               (if* (send (send group :graphics-obj) :graphics-length)
                        then (send group :erase-length) 
  		             (send group :draw-length)))
              (if* (= (mod *codelet-count* 100) 0) then (redraw-graphics))))

  (setq *updating-everything* nil))

;---------------------------------------------

(defun step-ccat (&aux codelet)  
; Runs one step of the program:  chooses and runs a codelet from the 
; coderack.
  (setq codelet (send *coderack* :choose))
  (if* %slightly-verbose% then (send codelet :print))
  (send codelet :run)
  (setq *codelet-count* (1+ *codelet-count*))
  (if* %verbose% then (format t "~%"))
  (if* *break-on-each-step* 
   then (if* %minimal-coderack-graphics% 
	 then (update-minimal-coderack-display))
        (break)))
      
;---------------------------------------------

(defun deal-with-snag ()
; If there is a snag in building the answer, then delete all 
; proposed structures, empty the coderack, raise and clamp the 
; temperature, and activate and clamp the activation of all the descriptions 
; of the object causing the snag.  

  (incf *snag-count*)
  (setq *last-snag-time* *codelet-count*)
  ; Save the current set of structures.  
  (setq *snag-structure-list* (send *workspace* :structure-list))

  ; Erase proposed structures.  (Their builder codelets will 
  ; disappear when the coderack is initialized.)
  (if* %workspace-graphics%
   then (loop for b in (send *workspace* :proposed-bond-list)
	      do (send (send b :string) 
		       :delete-proposed-bond b)
	         (if* (not (send (send b :string) 
				 :bond-present? b))
		  then (send b :erase-spline)))
        (loop for g in (send *workspace* :proposed-group-list)
	      do (send (send g :string) 
		       :delete-proposed-group g)
	         (if* (not (send (send g :string) 
				 :group-present? g))
                  then (send g :erase-rectangle)))
        (loop for c in (send *workspace* 
			     :proposed-correspondence-list)
	      do (send *workspace* 
		       :delete-proposed-correspondence c)
                 (if* (not (send *workspace* 
				 :correspondence-present? c))
                  then (send c :erase-line))))
  (send *coderack* :empty)
  (if* %coderack-graphics% then (update-coderack-display))

  (if* (and %workspace-graphics% *translated-rule*)
   then (send *translated-rule* :erase %translated-rule-mode%))
  (setq *translated-rule* nil)
  (setq *answer-string* nil)
  (setq *snag-condition* t)
  (setq *temperature* 100)
  (setq *clamp-temperature* t)
  (loop for d in (send *snag-object* :descriptions) do
	(send (send d :descriptor) :set-clamp t))
  (send *snag-object* :set-clamp-salience? t)
  (send *coderack* :empty)
  (post-initial-codelets)
  (update-everything))
  
;---------------------------------------------

(defun unanswer ()
; Deletes the answer so that the program can continue running.
  (setq *translated-rule* nil
	*answer-string* nil
	*found-answer* nil
	*quit-program* nil)
  (display-ccat)
  (run-ccat))

;---------------------------------------------
  





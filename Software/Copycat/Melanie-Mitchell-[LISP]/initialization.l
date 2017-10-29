;---------------------------------------------
; INITIALIZATION: This file contains functions for initializing the program.
;---------------------------------------------

(in-package 'user)

(defun init-ccat (initial-string-input modified-string-input 
		  target-string-input &key random-state no-graphics
		  &aux command quit)

  ; Initialize the random-number generator and make a copy of *random-state* 
  ; that can be stored and used again to replay this run.
  (if* (null random-state)
   then (setq *random-state* (make-random-state t))
   else (setq *random-state* (make-random-state random-state)))
  (setq *random-state-this-run* (make-random-state *random-state*))

  ; Initialize some global variables.
  (setq *workspace-initialized* nil) ; T if the workspace has been 
                                      ; initialized.
  (setq *coderack-initialized* nil)  ; T if the coderack has been 
                                          ; initialized.
  (setq *slipnet-initialized* nil)   ; T if the slipnet has been initialized.
  (setq *rule* nil) 
  (setq *translated-rule* nil)
  (setq *answer-string* nil)
  (setq *quit-program* nil)
  (setq *codelet-count* 0) 
  (setq *codelets-to-post* nil)
  (setq *found-answer* nil)
  (setq *temperature* 100)
  (setq *snag-object* nil)
  (setq *snag-condition* nil)
  (setq *snag-count* 0)
  (setq *length-description-count* 0)
  (setq *length-relevant-at-end* nil)
  (setq *single-letter-group-count* 0) ; For statistics purposes.
  (setq *single-letter-group-at-end-count* 0)
  (setq *clamp-temperature* nil)
  (setq *description-graphics-obj-list* nil)
  (setq *break-on-each-step* nil)  
  (setq *amount-length-changed* nil)  ; involving group-length changes.

  ; Initialize the constants for the program.
  (init-constants)
  (if* %demo-graphics%
   then (setq %coderack-graphics% nil)
        (setq %minimal-coderack-graphics% t)
        (setq %description-graphics% nil))

  (if* no-graphics
   then (setq %workspace-graphics% nil) 
        (setq %coderack-graphics% nil)
        (setq %minimal-coderack-graphics% nil)
	(setq %temperature-graphics% nil)
        (setq %slipnet-graphics% nil) 
        (setq %description-graphics% nil))

  (if* %workspace-graphics% 
   then (open-window %graphics-viewport-width% 
	             %graphics-viewport-height% 
	             %graphics-viewport-x% 
		     %graphics-viewport-y%) 
        (clear-window)
        (format t "~%Please initialize parameters.~&")
        (loop until quit do
              (init-init-time-menu)
              (if* (eq (get-init-time-menu-command target-string-input) 'okay)
               then (setq quit t)))
        (format t "Parameters are initialized.~&")
        (format t "Please wait...~&"))

  (if* (and (not no-graphics) %demo-graphics%)  
       ; This appears here too, in case %demo-graphics%
       ; was set to t when the parameters were set.
   then (setq %coderack-graphics% nil)
        (setq %minimal-coderack-graphics% t)
        (setq %description-graphics% nil))

  ; Initialize the coderack, slipnet, and workspace.
  (init-coderack)
  (init-slipnet) 
  (init-workspace initial-string-input modified-string-input 
                  target-string-input) 

  (if* %workspace-graphics% then (init-workspace-graphics))

  ; Clamp the activation of the slipnet nodes that are a priori relevant and 
  ; thus should stay active for at least the initial part of the run.
  (loop for node in *initially-clamped-slipnodes* do
	(send node :set-clamp t))

  ; Add initial descriptions to all letters.
  (add-descriptions)  
  (if* %workspace-graphics% then
       (loop for obj in (append (send *initial-string* :object-list)
				(send *modified-string* :object-list)
				(send *target-string* :object-list)) do 
	     (loop for d in (send obj :descriptions)
	           do (send obj :init-description-graphics d))))

  ; Post the initial codelets on the coderack.
  (post-initial-codelets)
  
  ; Update the slipnet.
  (update-slipnet)
  (if* %description-graphics%
   then (display-descriptions))
  (if* %temperature-graphics% 
   then (display-temperature))
  (if* %coderack-graphics% 
   then (init-coderack-graphics) 
        (display-coderack))
  (if* %minimal-coderack-graphics%
   then (init-minimal-coderack-display) 
        (display-minimal-coderack))
  (if* %slipnet-graphics% 
   then (init-slipnet-graphics) 
        (display-slipnet))
  (if* %workspace-graphics% 
   then (format t "Initialization is complete.~&")
        (format t "Please select next action.~&")
        (setq quit nil)
        (loop until (or (eq command 'begin-run) (eq command 'quit-program)) do 
             (init-begin-run-time-menu)
	     (setq command 
		   (get-begin-run-time-menu-command)))
        (if* (eq command 'begin-run)
         then (format t "Beginning run.~&")
    
         else (format t "Quitting program.~&")))

  ; Begin the run.
  (if* (not (eq command 'quit-program))
   then (run-ccat)))

;---------------------------------------------

(defun init-workspace (initial-string-input modified-string-input 
		       target-string-input
  		       &aux i-length m-length t-length)
; Initializes the three strings -- that is, fills in *initial-string*, 
; *modified-string*, and *target-string* structures with letters.

  ; Set up the input for the three strings. 
  (setq initial-string-input 
	(mapcar 'string (coerce (symbol-name initial-string-input) 'list)))
  (setq modified-string-input 
	(mapcar 'string (coerce (symbol-name modified-string-input) 'list)))
  (setq target-string-input 
	(mapcar 'string (coerce (symbol-name target-string-input) 'list)))
  (setq i-length (length initial-string-input))
  (setq m-length (length modified-string-input))
  (setq t-length (length target-string-input))
 
  ; Initialize the workspace.
  (setq *workspace* 
	(make-instance 'workspace 
                       :proposed-correspondence-array 
		       (make-array (list i-length t-length) 
			           :initial-element nil :adjustable t)
                        :correspondence-vector 
			(make-vector i-length)))
			
  ; Initialize the three strings.
  (setq *initial-string* 
	(make-instance 'workspace-string 
                       :highest-string-number -1
                       :letter-vector (make-vector i-length)
                       :proposed-bond-array 
		       (make-array (list i-length i-length) 
			           :initial-element nil 
				   :adjustable t)
                       :left-right-bond-array 
 		       (make-array (list i-length i-length) 
			           :initial-element nil 
				   :adjustable t)
                       :from-to-bond-array 
		       (make-array (list i-length i-length) 
			           :initial-element nil 
				   :adjustable t)
                       :proposed-group-array 
 		       (make-array (list i-length i-length) 
			           :initial-element nil 
				   :adjustable t)	
		       :group-vector (make-vector i-length)
		       :object-position-vector (make-vector i-length)
		       :length i-length
		       :object-spaces i-length
		       :num-of-bonds-to-scan-distribution
  	                (loop for i from 0 to (- i-length 1) do 
			      collect i)
	               :pname "initial string"))
  
  (setq *modified-string* 
	(make-instance 'workspace-string 
                       :highest-string-number -1
 		       :letter-vector (make-vector m-length)
 		       :object-position-vector (make-vector m-length)
 		       :length m-length
                       :pname "modified string"))

  (setq *target-string* 
	(make-instance 'workspace-string 
                       :highest-string-number -1
                       :letter-vector (make-vector t-length)
                       :proposed-bond-array 
		       (make-array (list t-length t-length) 
 		                   :initial-element nil 
				   :adjustable t)
  	               :left-right-bond-array 
 		       (make-array (list t-length t-length) 
			           :initial-element nil 
				   :adjustable t)
                       :from-to-bond-array 
		       (make-array (list t-length t-length) 
			           :initial-element nil 
				   :adjustable t)	
                       :proposed-group-array 
  		       (make-array (list t-length t-length) 
		                   :initial-element nil 
				   :adjustable t)
 		       :group-vector (make-vector t-length)
		       :object-position-vector (make-vector t-length)
		       :length t-length
		       :object-spaces t-length
		       :num-of-bonds-to-scan-distribution
  	                (loop for i from 0 to (- t-length 1) do 
			      collect i)
                       :pname "target string"))

  (setq *answer-string* nil)

  ; Nicknames.
  (setq *i* *initial-string*)
  (setq *m* *modified-string*)
  (setq *t* *target-string*)
  (setq *a* *answer-string*)

  ; Initialize the letters in the three strings.
  (make-letters initial-string-input *initial-string*)
  (make-letters modified-string-input *modified-string*)
  (make-letters target-string-input *target-string*)

  (send *initial-string* 
	:set-letter-list (vector-to-list (send *initial-string* 
					       :letter-vector)))
  (send *modified-string* 
	:set-letter-list (vector-to-list (send *modified-string* 
					       :letter-vector)))
  (send *target-string* 
	:set-letter-list (vector-to-list (send *target-string* 
					       :letter-vector)))
  (setq *rule* nil)
  (setq *translated-rule* nil)


; If one of the strings has only one letter, then activate 
; plato-object-category so that "letter" will be relevant.
  (if* (or (= (send *initial-string* :length) 1)
	   (= (send *target-string* :length) 1))
   then (send plato-object-category :activate-from-workspace))

  (setq *workspace-initialized* t))

;---------------------------------------------

(defun make-letters (input-letter-list string 
			&aux current-letter letter-category)
; Set up letter instance for each input letter.
  (loop for letter-name in input-letter-list 
        count t into letter-count do
        ; Set up an instance of the letter on the workspace.
        (setq letter-category (get-plato-letter letter-name))
        (setq current-letter 
	      (make-letter string letter-category 
		  (1- letter-count)))
        (send string :add-letter current-letter)))

;---------------------------------------------

(defun add-descriptions (&aux leftmost-letter middle-letter rightmost-letter)
; Adds initial descriptions to the letters in the three strings.
  (loop for string 
	in (list *initial-string* *modified-string* *target-string*) do
        (loop for letter in (send string :letter-list) do
              ; Add object-category description.
              (send letter :add-description 
	    		   (make-description letter plato-object-category 
             			             plato-letter))

              ; Add letter-category description
              (send letter :add-description 
 	                   (make-description 
			       letter plato-letter-category 
                               (get-plato-letter 
				   (string-upcase (send letter :pname))))))

        ; Now add string-position-category descriptions to the rightmost and 
	; leftmost letters, and to middle letter, if there is one.
        (setq leftmost-letter (send string :get-letter 0))
        (if* (> (send string :length) 1)  ; If there is only one letter in the
	                                  ;  string, it isn't described as 
					  ; either "leftmost" or "rightmost".
         then (setq rightmost-letter 
		    (send string :get-letter (1- (send string :length))))
	      (send leftmost-letter :add-description 
		    (make-description leftmost-letter
			              plato-string-position-category 
			              plato-leftmost))
              (send rightmost-letter :add-description 
		    (make-description rightmost-letter
			              plato-string-position-category 
			              plato-rightmost))
         else 
     	      (send leftmost-letter :add-description 
		    (make-description leftmost-letter 
			              plato-string-position-category 
				      plato-single)))

        (if* (= (send string :length) 3)
         then (setq middle-letter (send string :get-letter 1))
	      (send middle-letter :add-description 
  	           (make-description middle-letter
		                     plato-string-position-category 
				     plato-middle))))

; Activate the nodes in the descriptions of the letters.
  (loop for object in (send *workspace* :object-list) do
        (send-method-to-list 
	    (send-method-to-list (send object :descriptions) :descriptor)
	    :activate-from-workspace)))

;---------------------------------------------

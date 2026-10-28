;---------------------------------------------
; REPLACEMENTS: This file contains flavors, methods, and codelets for 
;               replacements.
;---------------------------------------------

(in-package 'user)
			     
(defflavor replacement
    (obj1 obj2)
    (workspace-structure)
    :gettable-instance-variables
    :settable-instance-variables
    :initable-instance-variables)

;---------------------------------------------

(defun make-replacement (obj1 obj2 &aux new-replacement)
; Returns a new replacement.
  (setq new-replacement (make-instance 'replacement :obj1 obj1 :obj2 obj2))
  (if* %workspace-graphics% then (send new-replacement :init-graphics))
  new-replacement)

;---------------------------------------------

(defun replacement-finder (&aux i-letter m-letter i-letter-category
				m-letter-category change-relation
				new-replacement)
; Chooses a letter at random in the initial-string.  Sees if it is the 
; changed letter.  If so, then marks it as changed, and adds a description 
; of the relation describing the change, if there is one.  For now, this 
; can only deal with letters changing into letters, not letters changing 
; into groups or vice versa.

(block nil  
  (if* %verbose% then (format t "In replacement-finder~&"))
  (setq i-letter (send *initial-string* :random-letter))
  (if* %verbose% 
   then (format t "Chose ") (send i-letter :print))

  (if* (send i-letter :replacement)
   then (if* %verbose% 
	 then (format t "Replacement for this letter already found. ")
              (format t "Fizzling.~&"))
        (return))
  
  (setq m-letter (send *modified-string* :get-letter 
		       (send i-letter :left-string-position)))
  (if* %verbose% 
   then (format t "Found modified-string-letter: ~a:~a~&"
	          (send m-letter :pname) 
		  (send m-letter :left-string-position)))

  ; Now see if  m-letter's letter-category is different from i-letter's.
  (setq i-letter-category 
	(send i-letter :get-descriptor plato-letter-category))
  (setq m-letter-category 
	(send m-letter :get-descriptor plato-letter-category))
  (if* (not (eq i-letter-category m-letter-category))
   then (send i-letter :set-changed? t)
 	(setq change-relation 
	      (get-label-node i-letter-category m-letter-category))
        (if* change-relation 
         then (send m-letter :add-extrinsic-description 
		    (make-extrinsic-description 
			change-relation plato-letter-category i-letter))
               (if* %verbose% 
	        then (format t "Found change-relation ~a~&" 
		               (send change-relation :pname)))))

  (setq new-replacement (make-replacement i-letter m-letter))
  (send *workspace* :add-replacement new-replacement)
  (send i-letter :set-replacement new-replacement)
  (if* %workspace-graphics% then (send (send i-letter :replacement) :draw))))
  
;---------------------------------------------
			
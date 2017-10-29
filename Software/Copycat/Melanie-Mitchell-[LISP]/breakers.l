;---------------------------------------------
; BREAKERS:  This file contains the breaker codelet.
;---------------------------------------------

(in-package 'user)

(defun breaker (&aux fizzle-probability structure structure-list
		     break-probability quit)
; First decides probabilistically whether or not to fizzle, based on 
; temperature.  Chooses a structure and random and decides probabilistically
; whether or not to break it as a function of its total weakness.
(block nil
  (if* %verbose% then (format t "In breaker~&"))

  ; Decide whether or not to fizzle as a function of temperature.
  (setq fizzle-probability (/ (fake-reciprocal *temperature*) 100))
  (if* (eq (flip-coin fizzle-probability) 'heads)
   then (if* %verbose% 
	 then (format t "Temperature is too low.  Fizzling.~&"))
        (return))

  ; Choose a structure at random.
  (setq structure (random-list-item (send *workspace* :structure-list)))
  
  (if* (null structure)
   then (if* %verbose% 
	 then (format t "Couldn't choose structure.  Fizzling.~&"))
        (return))

  ; If the structure is a bond in a group, have to break the group in 
  ; order to break the bond.
  (if* (and (typep structure 'bond) (send structure :group))
   then (setq structure-list (list structure (send structure :group)))
   else (setq structure-list (list structure)))

  ; See if the structure (or structures) can be broken.
  (loop for s in structure-list do
        (setq break-probability 
	      (get-temperature-adjusted-probability 
		  (/ (send s :total-weakness) 100)))
        (if* %verbose% 
	 then (format t "The structure is: ") (send s :print) 
	      (format t "The break-probability is ~a~&" break-probability))
        (if* (eq (flip-coin break-probability) 'tails)
         then (if* %verbose% then (format t  "Can't break it.  Fizzling.~&"))
	      (setq quit t)
	      (return)))

  ; Break the structure (or structures).
  (if* (not quit)
   then (loop for s in structure-list do
	      (if* %verbose% 
	       then (format t "About to break ") (send s :print))
              (funcall (append-symbols 'break-  (send s :structure-category))
		       s)))))

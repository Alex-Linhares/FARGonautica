;---------------------------------------------
; UTIL: This file contains utilities for Copycat.
;--------------------------------------------- 

(in-package 'user)

;---------------------------------------------

; SELECTION FUNCTIONS

;---------------------------------------------

(defun select-list-position (list &aux (sum 0) (value 0))
; LIST is a list of integers. Probabilistically chooses one of the 
; integers on the list according to value. Returns the position of that 
; integer.
  (if* list 
   then (setq sum (loop for item in list sum item))
        (if* (<= sum 0)
         then (random (length list))
         else (setq value (random sum))
              (loop for item in list
	            with newsum = 0
	            with counter = 0
	            do (incf newsum item)
	               (setq counter (1+ counter))
	            when (> newsum value)
	            return (1- counter)))
   else nil))	

;---------------------------------------------

(defun select-assoc (list &aux (sum 0) (value 0))
; LIST is an assoc-list of the form 
;       ((item . probability) (item . probability) . . .)
; Returns one of the items, chosen probabilistically.
  (if* list 
   then (setq sum (loop for pair in list sum (cdr pair)))
        (if* (<= sum 0)
         then nil
         else (setq value (random sum))
              (loop for pair in list
 	            with newsum = 0
	            do (incf newsum (cdr pair))
	            when (> newsum value)
	            return (car pair)))
   else nil))

;---------------------------------------------

(defun random-list-item (list)
; Returns a randomly chosen item in the list.
  (if* list then (nth (random (length list)) list) else nil))
 
;---------------------------------------------

(defun select-list-item-by-method (list method &optional argument)
; Applies the method to the list to get a list of numbers.
; Probabilisticallly selects a position in the list of numbers
; and returns the item at that position in the original list.
  (if* (null list) 
   then nil
   else (if* argument
         then (nth (select-list-position 
		       (send-method-to-list list method argument))
		   list)
         else (nth (select-list-position (send-method-to-list list method)) 
		   list))))

;---------------------------------------------

; PROBABILITY DISTRIBUTIONS

;---------------------------------------------

(defflavor probability-distribution
    (probability-vector length probability-sum pname)
    ()
  :gettable-instance-variables
  :settable-instance-variables
  :initable-instance-variables)

;---------------------------------------------

(defun make-probability-distribution (pname vector-size)
; Returns a new probability distribution.
  (make-instance 'probability-distribution
      :pname pname
      :probability-vector (make-vector vector-size)))

;---------------------------------------------

(defmethod (probability-distribution :update) ()
  (send self :set-length (vsize probability-vector))
  (send self :set-probability-sum (vector-sum probability-vector)))

;---------------------------------------------

(defmethod (probability-distribution :vset) (index value)
  (vset probability-vector index value))

;---------------------------------------------

(defmethod (probability-distribution :choose) ()
; Returns a number between 0 and 100, choosing probabilistically
; according to the given distribution.
  (select-list-position (vector-to-list probability-vector)))
         
;---------------------------------------------

; STRUCTURE-COMPETITION FUNCTIONS

;---------------------------------------------

(defun structure-vs-structure (structure1 weight1 structure2 weight2 
			       &aux strength-list adjusted-strength-list)
; Chooses probabilistically between the two structures, based on their
; strengths and the given weights.  Returns t if structure1 wins, nil if 
; structure2 wins.
  (if* %verbose% 
   then (format t "In structure-vs-structure with structures:  ")
        (send structure1 :print) (send structure2 :print)
	(format t "and weights ~a and ~a~&" weight1 weight2))
  (send structure1 :update-strength-values)
  (send structure2 :update-strength-values)
  (setq strength-list (list (* (send structure1 :total-strength) weight1)
			    (* (send structure2 :total-strength) weight2)))
  (setq adjusted-strength-list 
	(get-temperature-adjusted-value-list strength-list))
  (if* %verbose% 
   then (format t "Strength-list: ~a; adjusted-strength-list: ~a~&" 
	        strength-list adjusted-strength-list))

  (nth (select-list-position adjusted-strength-list) '(t nil)))
  
;---------------------------------------------

(defun fight-it-out (structure structure-weight structure-list list-weight)
; Chooses probabilistically between the structure and the list of
; structures, using the function "structure-vs-structure".
; Returns t if the structure wins over all the structures in structure-list.
; Otherwise, returns nil.
  (loop for competing-structure in structure-list
        when (null (structure-vs-structure structure structure-weight 
		                           competing-structure list-weight))
        return nil
  	finally (return t)))

;---------------------------------------------

; VECTOR AND ARRAY FUNCTIONS

;---------------------------------------------

(defun make-vector (size &key initial-element)
  (make-array (list size) :adjustable t :initial-element initial-element))

;---------------------------------------------

(defun make-bin-vector (size)
  (make-array (list size) :initial-element nil :adjustable t :fill-pointer 0))

;---------------------------------------------

(defun vref (vector index)
  (aref vector index))

;---------------------------------------------

(defun vset (vector index item)
  (setf (aref vector index) item))

;---------------------------------------------

(defun vsize (vector)
  (array-total-size vector))

;---------------------------------------------

(defun aset (array &rest args &aux indices item)
  (setq indices (butlast args))
  (setq item (car (last args)))    
  (setf (apply #'aref array indices) item))

;---------------------------------------------

(defun array-rows (array)
  (car (array-dimensions array)))

;---------------------------------------------

(defun array-columns (array)
  (cadr (array-dimensions array)))

;---------------------------------------------

(defun vector-to-list (v)
  (coerce v 'list))

;---------------------------------------------

(defun list-to-vector (l)
  (coerce l 'vector))

;---------------------------------------------

(defun array-to-list (a &aux l)
  (loop for i from (1- (array-rows a))  downto 0 do
	(loop for j from (1- (array-columns a)) downto 0 do
	      (push (aref a i j) l)))
  l)
	      
;---------------------------------------------

(defun vector-sum (v)
  (reduce #'+ v))

;---------------------------------------------

; OTHER FUNCTIONS

;---------------------------------------------

(defun endcons (obj list)
; Conses the given object to the end of the given list.
   (append list (list obj)))

;---------------------------------------------

(defun flip-coin (&optional (prob-of-heads .5))
; Returns heads or tails.  If no argument, probability of each is .5, 
; otherwise, probability of heads is equal to the argument (which is 
; assumed to be between 0 and 1).
  (if* (>= prob-of-heads 1) 
   then 'heads
   else (select-assoc `((heads . ,(round (* prob-of-heads 1000)))
	   	        (tails . ,(round (* (- 1 prob-of-heads) 1000)))))))

;---------------------------------------------

(defun get-letter (string position)
; Returns the requested letter.
   (case string
     (i (send *initial-string* :get-letter position))
     (m (send *modified-string* :get-letter position))
     (a (send *answer-string* :get-letter position))
     (t (send *target-string* :get-letter position))))

;---------------------------------------------

(defun get-group (string position)
; Returns the requested group.
   (case string
     (i (send *initial-string* :get-group position))
     (m (send *modified-string* :get-group position))
     (a (send *answer-string* :get-group position))
     (t (send *target-string* :get-group position))))

;---------------------------------------------

(defun initial (position)
; Returns the initial-string letter at the given position.
  (send *initial-string* :get-letter position))

;---------------------------------------------

(defun modified (position)
; Returns the modified-string letter at the given position.
  (send *modified-string* :get-letter position))

;---------------------------------------------

(defun target (position)
; Returns the target-string letter at the given position.
  (send *target-string* :get-letter position))

;---------------------------------------------

(defun answer (position)
; Returns the answer-string letter at the given position.
  (send *answer-string* :get-letter position))

;---------------------------------------------

(defun initial-group (position)
; Returns the initial-string group at the given position.
  (send (initial position) :group))

;---------------------------------------------

(defun modified-group (position)
; Returns the modified-string group at the given position.
  (send (modified position) :group))

;---------------------------------------------

(defun target-group (position)
; Returns the target-string group at the given position.
  (send (target position) :group))

;---------------------------------------------

(defun list-max (list)
; Returns the maximum integer in a list of integers.
  (reduce #'max list))

;---------------------------------------------

(defun list-max-position (list)
; Returns the list-position of the maximum integer in a list of integers.
  (position (list-max list) list))

;---------------------------------------------

(defun list-min (list)
; Returns the minimum integer in a list of integers.
  (reduce #'min list))

;---------------------------------------------

(defun list-min-position (list)
; Returns the list-position of the minimum integer in a list of integers.
  (position (list-min list) list))
;---------------------------------------------

(defun list-eq (l1 l2)
; Returns t if the two lists are (in order) pairwise eq. Returns nil 
; otherwise.
  (not (mismatch l1 l2)))

;---------------------------------------------

(defun average (&rest list-of-values)
; Returns the arithmetic mean of its arguments.
  (/ (list-sum list-of-values) (length list-of-values)))

;---------------------------------------------

(defun weighted-average (value-weight-list)
; Returns the weighted arithmetic mean of its arguments.  The 
; value-weight-list is of the form 
;          ((value1 . weight1) (value2 . weight2) ...)
  (loop for value-weight in value-weight-list
	sum (* (car value-weight) (cdr value-weight)) into value-sum
	sum (cdr value-weight) into weight-sum 
	finally (return (round (/ value-sum weight-sum)))))

;---------------------------------------------

(defun send-method-to-list (list method &rest arg)
; Sends the given method to each item on the list and returns a list of the 
; results.  (Note that at present, this only works for methods with at most 1 
; argument.)
  (if* arg
   then 
        (loop for item in list 
	      if (null item) collect nil into value-list
	      else collect (send item (eval method) (car arg)) 
	           into value-list
	      finally (return value-list))
   else
        (loop for item in list 
	      if (null item) collect nil into value-list
	      else collect (send item (eval method)) into value-list
	      finally (return value-list))))
    

;---------------------------------------------

(defun quick-pause ()
    (loop for i from 1 to 1000 do ()))

;---------------------------------------------

(defun medium-pause ()
    (loop for i from 1 to 100000 do ()))

;---------------------------------------------

(defun long-pause ()
    (loop for i from 1 to 200000 do ()))

;---------------------------------------------

(defun very-long-pause ()
    (loop for i from 1 to 10 do (long-pause)))

;---------------------------------------------

(defun same-letter-category? (obj1 obj2)
  (string-equal (send obj1 :pname) (send obj2 :pname)))

;---------------------------------------------

(defun flatten (l)
; Flattens a list so that there are no nested lists.
  (cond ((null l) nil)
	((atom l) l)
        ((not (listp (car l)))
	 (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

;---------------------------------------------

(defun list-sum (l)
  (reduce #'+ l))

;---------------------------------------------

(defun list-average (l)
  (/ (list-sum l) (length l)))

;---------------------------------------------

(defun std-dev (list-of-values &aux (cumulative 0) (cumulative-squares 0))
  (cond ((= (length list-of-values) 0) nil)
	((= (length list-of-values) 1) 0)
        (t (loop for item in list-of-values do
              (incf cumulative item)
              (incf cumulative-squares (sqr item)))
           (sqrt (/ (abs (- cumulative-squares
	   	            (/ (sqr cumulative) (length list-of-values))))
   	            (1- (length list-of-values)))))))

;---------------------------------------------

(defun std-err (list-of-values)
  (float (/ (std-dev list-of-values) (sqrt (length list-of-values)))))

;---------------------------------------------

(defun list-multiply (l1 l2)
  (loop for i in l1
        for j in l2
        collect (* i j) into result
        finally (return result)))

;---------------------------------------------

(defun cube (x)
  (* x x x))

;---------------------------------------------

(defun fake-reciprocal (n)
  (- 100 n))

;---------------------------------------------

(defun fix-to-string (fix)
; Returns a string representing the given fixnum.
  (format nil "~d" fix))

;---------------------------------------------

(defun flavor-type (instance)
  (flavors::flavor-type-of instance))

;---------------------------------------------

(defun xor (p q)
  (and (or p q) (not (and p q))))

;---------------------------------------------

(defun get-plato-letter (letter-name)
; Returns the slipnode corresponding to the given letter name.
  (eval (append-symbols 'plato- letter-name)))

;---------------------------------------------

(defun get-plato-number (n)
  (case n
      (1 plato-one)      
      (2 plato-two)      
      (3 plato-three)      
      (4 plato-four)      
      (5 plato-five)))

;---------------------------------------------

(defun node-to-number (node)
  (cond ((eq node plato-one) 1)
        ((eq node plato-two) 2)      
        ((eq node plato-three) 3)      
        ((eq node plato-four) 4)      
        ((eq node plato-five) 5)))

;---------------------------------------------

(defun blur (n &aux blur-amount)
; Returns a number close to n (i.e., within (sqrt n) of n).
; This function will return n twice as often as it returns
; each of the other possible numbers.  Ideally this should be
; a normal distribution around n, but that wasn't necessary for this
; purpose.
  (setq blur-amount (round (sqrt n)))
  (funcall (random-list-item '(+ -)) n (random (1+ blur-amount))))


;---------------------------------------------

(defun ibond (left-position right-position)
  (aref (send *initial-string* :left-right-bond-array)
	      left-position right-position))

;---------------------------------------------

(defun tbond (left-position right-position)
  (aref (send *target-string* :left-right-bond-array)
	      left-position right-position))

;---------------------------------------------

(defun send-iobjs (message &optional arg)
  (if* arg 
   then (send-method-to-list (send *initial-string* :object-list) message arg)
   else (send-method-to-list (send *initial-string* :object-list) message)))

;---------------------------------------------

(defun send-tobjs (message &optional arg)
  (if* arg 
   then (send-method-to-list (send *target-string* :object-list) message arg)
   else (send-method-to-list (send *target-string* :object-list) message)))

;---------------------------------------------

(defun sml (list method &optional arg)
  (if* arg 
   then (send-method-to-list list method arg)   
   else (send-method-to-list list method)))

;---------------------------------------------

(defun reduce-decimal (flonum num-of-decimal-places &aux multiplier)
; Reduces the given flonum to the given number of decimal places.
  (if* (= num-of-decimal-places 0)
   then (round flonum)
   else (setf multiplier (expt 10 num-of-decimal-places))
        (float (/ (round (* flonum multiplier)) multiplier))))

;---------------------------------------------

(defun get-random-state (n input-file
			     &optional (directory "~/")
                             &aux random-state-file random-state-list)
  (setq random-state-file
	(string-append directory input-file ".random-state"))
  (with-open-file (istream random-state-file  :direction :input)
      (setq random-state-list (read istream)))
  (nth n random-state-list))

;---------------------------------------------


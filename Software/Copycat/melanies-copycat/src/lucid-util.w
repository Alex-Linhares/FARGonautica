Utilities for Lucid Common Lisp
-------------------------------

SQR number

Squaring function.  
Examples: (sqr 3) -> 9; (sqr 37.46) -> 1403.2516

-------------------------------------------------------------------

IF* (macro)

If-Then-Else macro.  
Examples: 
(1) (if* (< x 10) 
     then (format t "X is less than 10!")
	  (setq y 4)
     else (format t "X is not less than 10!")
	  (setq y 10)
   	  (setq z 20))

(2) (if* (and (= x 10) (> y 20))
     then (setq z 4))



        

         
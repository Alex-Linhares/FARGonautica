;; Utilities for Lucid Common Lisp.  Written by Melanie Mitchell, May, 1989.

(in-package 'user)

(proclaim '(inline sqr))  ; Instruction to the compiler to compile the sqr 
                          ; function inline.
(proclaim '(inline if*))  ; Instruction to the compiler to compile the 
                          ; if* function inline.
;---------------------------------------------

(defun sqr (x) (* x x))

;---------------------------------------------

(defmacro if* (&rest args &aux args-length if-clause then-clause 
		               else-clause else-position)
; If-then-else macro.  This provides if-then-else statements of the form
; (if <stmt> then <stmt1> <stmt2> ... else <stmt1> <stmt2> ...)

  (declare (dynamic-extent args))  ; To reduce number of garbage collections.

  (cond ((not (eq (second args) 'then)) (cons 'if args))
        (t (setq args-length (length args))
           (if (< args-length 2) (error "if: not enough arguments."))
           (setq if-clause (car args))
           (setq else-position (position 'else args))
           (if (null else-position) (setq else-position args-length))
           (setq then-clause (subseq args 2 else-position))
           (if (< else-position args-length)
               (setq else-clause (subseq args (1+ else-position) 
					      args-length))
               (setq else-clause (list nil)))
           `(cond ,(cons if-clause then-clause)
	          ,(cons t else-clause)))))
  
